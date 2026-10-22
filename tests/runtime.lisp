;;;; Unit tests for wasmdown.runtime: memory/table/global objects,
;;;; segment drop state, import matching, instantiation, exports,
;;;; and the shared wasm-call trampoline (CONTRACTS.md "Runtime
;;;; objects"). Modules are assembled programmatically per
;;;; src/types.lisp (no binary encoder dependency).

(defpackage #:wasmdown.test.runtime
  (:use #:cl #:wasmdown.types #:wasmdown.runtime)
  (:nicknames #:wd.test.runtime))

(in-package #:wasmdown.test.runtime)

(defun lim (min &optional max)
  (make-wasm-limits min max))

(defun u8 (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8)
              :initial-contents bytes))

;;; ================================================================
;;; memory-instance: make / load / store / bounds

(wasmdown.test:test runtime/memory-roundtrip
  (let ((m (make-memory-instance (lim 1))))
    (wasmdown.test:is (= 1 (memory-size m)))
    ;; fresh memory is zeroed
    (wasmdown.test:is (= 0 (memory-load m 0 8)))
    ;; little-endian store/load of all widths
    (memory-store m 100 4 #x11223344)
    (wasmdown.test:is (= #x11223344 (memory-load m 100 4)))
    (wasmdown.test:is (= #x44 (memory-load m 100 1)))
    (wasmdown.test:is (= #x3344 (memory-load m 100 2)))
    (memory-store m 200 8 #x0102030405060708)
    (wasmdown.test:is (= #x0102030405060708 (memory-load m 200 8)))
    ;; partial overlap write
    (memory-store m 202 2 #xabcd)
    (wasmdown.test:is (= #xabcd (memory-load m 202 2)))
    ;; values above width mask off
    (memory-store m 300 1 #x1234)
    (wasmdown.test:is (= #x34 (memory-load m 300 1)))))

(wasmdown.test:test runtime/memory-bounds-traps
  (let ((m (make-memory-instance (lim 1))))   ; 65536 bytes
    ;; ea + width > size
    (wasmdown.test:assert-trap :out-of-bounds-memory (memory-load m 65536 1))
    (wasmdown.test:assert-trap :out-of-bounds-memory (memory-load m 65535 2))
    (wasmdown.test:assert-trap :out-of-bounds-memory (memory-store m 65533 4 1))
    (wasmdown.test:assert-trap :out-of-bounds-memory (memory-load m 65532 8))
    ;; exact fits are allowed
    (memory-load m 65535 1)
    (memory-store m 65528 8 0)
    ;; the 4 GiB boundary: max u32 address + width must not overflow
    ;; the host (bignum-safe domain). 4294967295 + 2 > 4294967296 > size.
    (wasmdown.test:assert-trap :out-of-bounds-memory
      (memory-load m 4294967295 2))
    (wasmdown.test:assert-trap :out-of-bounds-memory
      (memory-store m 4294967295 4 0))
    (wasmdown.test:assert-trap :out-of-bounds-memory
      (memory-load m 4294967296 1))))

(wasmdown.test:test runtime/memory-grow
  (let ((m (make-memory-instance (lim 1 2))))
    ;; success returns the OLD page count
    (wasmdown.test:is (= 1 (memory-grow m 1)))
    (wasmdown.test:is (= 2 (memory-size m)))
    ;; beyond declared max: -1 (4294967295), size unchanged
    (wasmdown.test:is (= 4294967295 (memory-grow m 1)))
    (wasmdown.test:is (= 2 (memory-size m)))
    ;; grow preserves contents and zero-fills the new pages
    (memory-store m 0 4 #xdeadbeef)
    (wasmdown.test:is (= 2 (memory-grow m 0)))   ; delta 0 at max still fits
    (wasmdown.test:is (= #xdeadbeef (memory-load m 0 4)))
    (let ((m2 (make-memory-instance (lim 1))))
      (memory-store m2 10 4 #xcafebabe)
      (wasmdown.test:is (= 1 (memory-grow m2 1)))
      (wasmdown.test:is (= #xcafebabe (memory-load m2 10 4)))
      (wasmdown.test:is (= 0 (memory-load m2 65536 8))))))

(wasmdown.test:test runtime/memory-fill-copy
  (let ((m (make-memory-instance (lim 1))))
    ;; fill: value is the low 8 bits
    (memory-fill m 5 #x1ff 3)
    (wasmdown.test:is (= #xff (memory-load m 5 1)))
    (wasmdown.test:is (= #xff (memory-load m 7 1)))
    (wasmdown.test:is (= 0 (memory-load m 8 1)))
    (wasmdown.test:is (= 0 (memory-load m 4 1)))
    ;; zero-length spans are no-ops even at the boundary
    (memory-fill m 65536 0 0)
    (memory-copy m 65536 65536 0)
    ;; out-of-bounds fill/copy trap
    (wasmdown.test:assert-trap :out-of-bounds-memory (memory-fill m 65535 1 2))
    (wasmdown.test:assert-trap :out-of-bounds-memory (memory-copy m 65535 0 2))
    (wasmdown.test:assert-trap :out-of-bounds-memory (memory-copy m 0 65535 2))))

(wasmdown.test:test runtime/memory-copy-overlap
  (let ((m (make-memory-instance (lim 1))))
    ;; forward overlap (dst > src): memmove must keep source bytes
    (memory-fill m 0 0 16)
    (memory-store m 0 4 #x11223344)
    (memory-store m 4 4 #x55667788)
    (memory-copy m 4 0 8)                     ; dst=4 src=0 len=8
    (wasmdown.test:is (= #x11223344 (memory-load m 4 4)))
    (wasmdown.test:is (= #x55667788 (memory-load m 8 4)))
    ;; backward overlap (dst < src)
    (memory-fill m 0 0 16)
    (memory-store m 4 4 #xaabbccdd)
    (memory-copy m 0 4 8)                     ; dst=0 src=4 len=8
    (wasmdown.test:is (= #xaabbccdd (memory-load m 0 4)))))

;;; ================================================================
;;; table-instance

(wasmdown.test:test runtime/table-basics
  (let ((tb (make-table-instance :funcref (lim 2 4))))
    (wasmdown.test:is (= 2 (table-size tb)))
    (wasmdown.test:is (eq :funcref (table-instance-type tb)))
    (wasmdown.test:is (null (table-get tb 0)))
    (table-set tb 1 :some-ref)
    (wasmdown.test:is (eq :some-ref (table-get tb 1)))
    ;; out of bounds get/set
    (wasmdown.test:assert-trap :out-of-bounds-table (table-get tb 2))
    (wasmdown.test:assert-trap :out-of-bounds-table (table-set tb 2 nil))
    (wasmdown.test:assert-trap :out-of-bounds-table (table-get tb 4294967295))))

(wasmdown.test:test runtime/table-grow
  (let ((tb (make-table-instance :funcref (lim 2 4))))
    ;; grow returns the OLD size
    (wasmdown.test:is (= 2 (table-grow tb :r1 1)))
    (wasmdown.test:is (= 3 (table-size tb)))
    (wasmdown.test:is (eq :r1 (table-get tb 2)))
    ;; exceeding max fails with -1 (4294967295)
    (wasmdown.test:is (= 4294967295 (table-grow tb :r2 2)))
    (wasmdown.test:is (= 3 (table-size tb)))
    ;; grow by 0 at/under max succeeds
    (wasmdown.test:is (= 3 (table-grow tb :r3 0)))
    (wasmdown.test:is (= 3 (table-size tb)))
    ;; unbounded table grows freely
    (let ((tb2 (make-table-instance :externref (lim 0))))
      (wasmdown.test:is (eq :externref (table-instance-type tb2)))
      (wasmdown.test:is (= 0 (table-grow tb2 :x 10)))
      (wasmdown.test:is (= 10 (table-size tb2))))))

(wasmdown.test:test runtime/table-fill-copy
  (let ((tb (make-table-instance :funcref (lim 4))))
    (table-fill tb 1 :f 2)
    (wasmdown.test:is (eq :f (table-get tb 1)))
    (wasmdown.test:is (eq :f (table-get tb 2)))
    (wasmdown.test:is (null (table-get tb 0)))
    (wasmdown.test:is (null (table-get tb 3)))
    ;; copy with forward overlap: memmove semantics
    (table-fill tb 0 :a 4)
    (table-set tb 2 :b)
    (table-copy tb 1 0 3)                     ; dst=1 src=0 len=3
    (wasmdown.test:is (eq :a (table-get tb 1)))
    (wasmdown.test:is (eq :b (table-get tb 3)))
    ;; bounds traps
    (wasmdown.test:assert-trap :out-of-bounds-table (table-fill tb 3 :x 2))
    (wasmdown.test:assert-trap :out-of-bounds-table (table-copy tb 3 0 2))
    (wasmdown.test:assert-trap :out-of-bounds-table (table-copy tb 0 3 2))))

;;; ================================================================
;;; globals

(wasmdown.test:test runtime/global-instance
  (let ((g (make-global-instance 42 :type :i32 :mutable t)))
    (wasmdown.test:is (= 42 (global-value g)))
    (setf (global-value g) 7)
    (wasmdown.test:is (= 7 (global-value g)))
    (global-set-value g 9)
    (wasmdown.test:is (= 9 (global-value g)))
    (wasmdown.test:is (eq :i32 (wasmdown.runtime::global-instance-type g)))
    (wasmdown.test:is (eq t (wasmdown.runtime::global-instance-mutable g)))))

;;; ================================================================
;;; Import matching and instantiation. Modules are built directly.

(defun rt-func (typeidx body &optional (locals '()))
  (make-wasm-func typeidx locals body 0))

(defun rt-host (params results thunk)
  (make-host-func-instance (make-wasm-func-type params results) thunk))

(wasmdown.test:test runtime/import-func-match
  (let* ((types (vector (make-wasm-func-type '(:i32) '(:i32))))
         (host (rt-host '(:i32) '(:i32) (lambda (args) (mapcar #'1+ args))))
         (mod (make-wasm-module
               :types types
               :imports (vector (make-wasm-import "env" "f" :func 0))
               :exports (vector (make-wasm-export "f" :func 0)))))
    (let ((inst (make-module-instance mod `((("env" "f") . ,host)))))
      (wasmdown.test:is (func-instance-p (export-value inst "f")))
      (wasmdown.test:is (equal '(43) (wasm-call (export-value inst "f") 42))))))

(wasmdown.test:test runtime/import-kind-mismatch
  ;; module wants a func; a memory is provided -> link error
  (let* ((types (vector (make-wasm-func-type '() '())))
         (mem (make-memory-instance (lim 1)))
         (mod (make-wasm-module
               :types types
               :imports (vector (make-wasm-import "env" "f" :func 0)))))
    (wasmdown.test:assert-raises wasmdown:link-error
      (make-module-instance mod `((("env" "f") . ,mem))))))

(wasmdown.test:test runtime/import-type-mismatch
  ;; expects (i32)->(i32); provided ()->()
  (let* ((types (vector (make-wasm-func-type '(:i32) '(:i32))))
         (host (rt-host '() '() (lambda (args) (declare (ignore args)) '())))
         (mod (make-wasm-module
               :types types
               :imports (vector (make-wasm-import "env" "f" :func 0)))))
    (wasmdown.test:assert-raises wasmdown:link-error
      (make-module-instance mod `((("env" "f") . ,host))))))

(wasmdown.test:test runtime/import-limits-mismatch
  ;; imported memory must have at least the declared min
  (let* ((small (make-memory-instance (lim 1)))
         (mod (make-wasm-module
               :imports (vector
                         (make-wasm-import "env" "m" :memory
                                           (make-wasm-memory (lim 2 3)))))))
    (wasmdown.test:assert-raises wasmdown:link-error
      (make-module-instance mod `((("env" "m") . ,small)))))
  ;; imported table: element type mismatch
  (let* ((extab (make-table-instance :externref (lim 1)))
         (mod (make-wasm-module
               :imports (vector
                         (make-wasm-import "env" "t" :table
                                           (make-wasm-table :funcref (lim 1)))))))
    (wasmdown.test:assert-raises wasmdown:link-error
      (make-module-instance mod `((("env" "t") . ,extab))))))

(wasmdown.test:test runtime/import-global-mismatch
  ;; module wants a mutable i32; provided immutable -> link error
  (let* ((g (make-global-instance 0 :type :i32 :mutable nil))
         (mod (make-wasm-module
               :imports (vector (make-wasm-import "env" "g" :global
                                                   (cons :i32 t))))))
    (wasmdown.test:assert-raises wasmdown:link-error
      (make-module-instance mod `((("env" "g") . ,g))))))

(wasmdown.test:test runtime/import-missing
  (let ((mod (make-wasm-module
              :types (vector (make-wasm-func-type '() '()))
              :imports (vector (make-wasm-import "env" "f" :func 0)))))
    (wasmdown.test:assert-raises wasmdown:link-error
      (make-module-instance mod '()))))

(wasmdown.test:test runtime/elem-oob-instantiation-error
  ;; active elem segment does not fit the table -> instantiation error
  (let* ((types (vector (make-wasm-func-type '() '())))
         (f (rt-func 0 (vector (make-wasm-instr :end nil #() 0))))
         (off (vector (make-wasm-instr :i32.const (list :value 2) #() 0)
                      (make-wasm-instr :end nil #() 0)))
         (mod (make-wasm-module
               :types types :funcs (vector f)
               :tables (vector (make-wasm-table :funcref (lim 2)))
               :elems (vector (make-wasm-elem
                               (list :active 0 off) :funcref
                               (list (vector (make-wasm-instr :ref.func
                                                              (list :func 0) #() 0)
                                              (make-wasm-instr :end nil #() 0))))))))
    (wasmdown.test:assert-raises wasmdown:instantiation-error
      (make-module-instance mod '())))
  ;; exactly fitting is fine
  (let* ((types (vector (make-wasm-func-type '() '())))
         (f (rt-func 0 (vector (make-wasm-instr :end nil #() 0))))
         (off (vector (make-wasm-instr :i32.const (list :value 1) #() 0)
                      (make-wasm-instr :end nil #() 0)))
         (mod (make-wasm-module
               :types types :funcs (vector f)
               :tables (vector (make-wasm-table :funcref (lim 2)))
               :elems (vector (make-wasm-elem
                               (list :active 0 off) :funcref
                               (list (vector (make-wasm-instr :ref.func
                                                              (list :func 0) #() 0)
                                              (make-wasm-instr :end nil #() 0))))))))
    (let ((inst (make-module-instance mod '())))
      (wasmdown.test:is (func-instance-p
                         (table-get (aref (module-instance-tables inst) 0) 1))))))

(wasmdown.test:test runtime/data-oob-instantiation-error
  ;; active data segment does not fit the memory -> instantiation error
  (let* ((mod (make-wasm-module
               :mems (vector (make-wasm-memory (lim 1)))
               :datas (vector (make-wasm-data
                               (list :active 0
                                     (vector (make-wasm-instr :i32.const
                                                              (list :value 65535)
                                                              #() 0)
                                             (make-wasm-instr :end nil #() 0)))
                               (u8 1 2))))))
    (wasmdown.test:assert-raises wasmdown:instantiation-error
      (make-module-instance mod '()))))

(wasmdown.test:test runtime/start-function
  ;; start calls a host import; the observable side effect is captured
  (let* ((types (vector (make-wasm-func-type '(:i32) '())
                         (make-wasm-func-type '() '())))
         (calls '())
         (host (rt-host '(:i32) '()
                        (lambda (args) (push args calls) '())))
         (body (vector (make-wasm-instr :i32.const (list :value 99) #() 0)
                       (make-wasm-instr :call (list :func 0) #() 0)
                       (make-wasm-instr :end nil #() 0)))
         ;; the starter is ()->(); the import is func index 0, the
         ;; starter is func index 1 in the combined func space
         (starter (rt-func 1 body))
         (mod (make-wasm-module
               :types types
               :imports (vector (make-wasm-import "env" "log" :func 0))
               :funcs (vector starter)
               :start 1)))
    (let ((inst (make-module-instance mod `((("env" "log") . ,host)))))
      (declare (ignore inst))
      (wasmdown.test:is (equal '((99)) (reverse calls))))))

(wasmdown.test:test runtime/start-trap-propagates
  (let* ((types (vector (make-wasm-func-type '() '())))
         (body (vector (make-wasm-instr :unreachable nil #() 0)
                       (make-wasm-instr :end nil #() 0)))
         (starter (rt-func 0 body))
         (mod (make-wasm-module :types types :funcs (vector starter) :start 0)))
    (wasmdown.test:assert-trap :unreachable (make-module-instance mod '()))))

(wasmdown.test:test runtime/exports-resolve
  (let* ((types (vector (make-wasm-func-type '() '(:i32))))
         (f (rt-func 0 (vector (make-wasm-instr :i32.const (list :value 5) #() 0)
                               (make-wasm-instr :end nil #() 0))))
         (mod (make-wasm-module
               :types types :funcs (vector f)
               :mems (vector (make-wasm-memory (lim 1)))
               :globals (vector (make-wasm-global :type :i32 :mutable nil
                                                  :init (vector
                                                         (make-wasm-instr :i32.const
                                                                          (list :value 7)
                                                                          #() 0)
                                                         (make-wasm-instr :end nil #() 0))))
               :tables (vector (make-wasm-table :funcref (lim 1)))
               :exports (vector (make-wasm-export "f" :func 0)
                                (make-wasm-export "m" :memory 0)
                                (make-wasm-export "g" :global 0)
                                (make-wasm-export "t" :table 0)))))
    (let ((inst (make-module-instance mod '())))
      (wasmdown.test:is (func-instance-p (export-value inst "f")))
      (wasmdown.test:is (memory-instance-p (export-value inst "m")))
      (wasmdown.test:is (global-instance-p (export-value inst "g")))
      (wasmdown.test:is (table-instance-p (export-value inst "t")))
      (wasmdown.test:is (= 7 (global-value (export-value inst "g"))))
      (wasmdown.test:is (equal '(5) (wasm-call (export-value inst "f"))))
      (wasmdown.test:assert-raises wasmdown:link-error
        (export-value inst "nope")))))

(wasmdown.test:test runtime/host-func-multi-results
  ;; host functions return lists: single and multiple results
  (let ((h1 (rt-host '() '(:i32) (lambda (args)
                                   (declare (ignore args)) '(41))))
        (h2 (rt-host '(:i32) '(:i32 :i32)
                     (lambda (args) (list (car args) (1+ (car args)))))))
    (wasmdown.test:is (equal '(41) (wasm-call h1)))
    (wasmdown.test:is (equal '(10 11) (wasm-call h2 10)))))
