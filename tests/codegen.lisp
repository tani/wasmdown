;;;; Parity suite for wasmdown.codegen (docs/design/PLAN.md 12.3): every fixture is
;;;; instantiated once, invoked through the reference interpreter (the
;;;; default *INVOKE-WASM-FUNC*), then compiled with COMPILE-MODULE-
;;;; FUNCTIONS, installed into the same instance, and invoked again;
;;;; results must agree (EQUALP) and traps must agree in kind.
;;;;
;;;; Formerly excluded and now COVERED (the interpreter was fixed to the
;;;; CONTRACTS.md shape): signed sub-word loads (i32/i64 loadN_s) and
;;;; f32/f64 load/store at the memory boundary. The pins at the end of
;;;; this file fail if either engine drifts back to the old behaviour.

(defpackage #:wasmdown.test.codegen
  (:use #:cl #:wasmdown.types #:wasmdown.runtime)
  (:nicknames #:wd.test.codegen))

(in-package #:wasmdown.test.codegen)

;;; Builders (same conventions as tests/interp.lisp).

(defun i (op &optional imms body)
  (make-wasm-instr op imms (or body #()) 0))

(defun endi () (i :end))

(defun ft (params results)
  (make-wasm-func-type params results))

(defun fn (typeidx body &optional (locals '()))
  (make-wasm-func typeidx locals body 0))

(defun lim (min &optional max)
  ;; only pass MAX when supplied: make-wasm-limits marks HAS-MAX from
  ;; whether the optional was provided, and (make-wasm-limits 1 nil)
  ;; declares a max of NIL (a validation error).
  (if max
      (make-wasm-limits min max)
      (make-wasm-limits min)))

(defun mmod (&rest keys &key types imports funcs tables mems globals
                 exports start elems datas &allow-other-keys)
  (apply #'make-wasm-module
         :types (or types #()) :imports (or imports #())
         :funcs (or funcs #()) :tables (or tables #())
         :mems (or mems #()) :globals (or globals #())
         :exports (or exports #()) :start start
         :elems (or elems #()) :datas (or datas #()) keys))

(defun call-export (inst name &rest args)
  (apply #'wasm-call (export-value inst name) args))

(defparameter *emit-dir*
  ;; scratch outside the repo: the parity suite writes generated
  ;; source here on EMIT fixtures; it is regenerated per run.
  (merge-pathnames "wasmdown-emit/" #P"/tmp/"))

;;; The central parity helper: instantiate, run via the interpreter,
;;; compile + install, run again through the compiled closures, and
;;; compare both observables. Returns the instance for extra asserts.

(defun %invoke-safe (inst name args)
  "Invoke export NAME with ARGS; capture traps as (CONS :trap KIND)."
  (block trap-block
    (handler-bind ((wasmdown:wasm-trap
                    (lambda (c)
                      (return-from trap-block
                        (cons :trap (wasmdown:wasm-trap-kind c))))))
      (cons :ok (apply #'call-export inst name args)))))

(defun %install-compiled! (mod inst compiled)
  "Install COMPILED entries on the instance's DEFINED :wasm
func-instances (absolute funcidx = imported funcs + defined index).
Only BACKEND/COMPILED-ENTRY change: exports, table elements, element
segments and other modules' imports keep the same reference objects
(the M1 contract api.lisp's :compiled mode uses)."
  (declare (ignorable mod))
  (let ((nimp (count :func (wasm-module-imports (module-instance-module inst))
                     :key #'wasm-import-kind)))
    (loop for j below (length compiled)
          for f = (svref (module-instance-funcs inst) (+ nimp j))
          do (setf (func-instance-backend f) :compiled
                   (func-instance-compiled-entry f) (svref compiled j)))
    inst))


(defun parity (mod &key imports (name "f") args emit)
  "Run the export NAME with ARGS through the interpreter, then through
the compiled backend, and require agreement: EQUALP results or the
same trap kind. The interpreter phase gets a FRESH instance; the
compiled phase compiles against a SECOND fresh instance, installs the
closures, and invokes through the same installed exports. Fresh
instances per engine keep stateful fixtures (memory.grow, globals,
host side effects) honest: each engine sees the module in its initial
state. With EMIT non-nil the generated source is pretty-printed to
*EMIT-DIR*/NAME.lisp and must contain a TAGBODY (inspectable source,
docs/design/PLAN.md 6.1/12.3). Returns the compiled-phase instance."
  (flet ((phase-inst ()
           (make-module-instance mod (or imports '()))))
    (let* ((before (%invoke-safe (phase-inst) name args))
           (inst (phase-inst))
           (emit-path (when emit
                       (ensure-directories-exist *emit-dir*)
                       (merge-pathnames (format nil "~a.lisp" name)
                                        *emit-dir*)))
           (compiled (wasmdown.codegen:compile-module-functions
                      mod inst :emit emit-path))
           (after (progn (%install-compiled! mod inst compiled)
                         (%invoke-safe inst name args))))
      (cond ((and (eq (car before) :trap) (eq (car after) :trap))
             (wasmdown.test:is (eq (cdr before) (cdr after))))
            ((or (eq (car before) :trap) (eq (car after) :trap))
             (error "parity: trap mismatch: interp ~s compiled ~s"
                    before after))
            (t (wasmdown.test:is (equalp (cdr before) (cdr after)))))
      (when emit-path
        (with-open-file (s emit-path)
          (let ((txt (make-string (file-length s))))
            (read-sequence txt s)
            (wasmdown.test:is (and (search "TAGBODY" txt :test #'char-equal)
                                   t)))))
      inst)))

(defun expect (form value)
  (wasmdown.test:is (equal value form)))


(wasmdown.test:test codegen/arithmetic-chain
  ;; (a+b)*10 - 3, plus wrap edges
  (let ((mod (mmod
              :types (vector (ft '(:i32 :i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector (i :local.get '(:local 0))
                                    (i :local.get '(:local 1))
                                    (i :i32.add)
                                    (i :i32.const '(:value 10))
                                    (i :i32.mul)
                                    (i :i32.const '(:value 3))
                                    (i :i32.sub)
                                    (i :end))))
              :exports (vector (make-wasm-export "f" :func 0)))))
    (parity mod :args '(2 5))
    (let ((inst (parity mod :args '(4294967295 1))))
      (declare (ignore inst))
      (values))))

(wasmdown.test:test codegen/i64-wrap-edges
  ;; i64 addition wraps mod 2^64; i32.wrap_i64 truncates to 32 bits
  (let ((mod (mmod
              :types (vector (ft '(:i64 :i64) '(:i64))
                             (ft '(:i64) '(:i32)))
              :funcs (vector
                      (fn 0 (vector (i :local.get '(:local 0))
                                    (i :local.get '(:local 1))
                                    (i :i64.add)
                                    (i :end)))
                      (fn 1 (vector (i :local.get '(:local 0))
                                    (i :i32.wrap_i64)
                                    (i :end))))
              :exports (vector (make-wasm-export "add64" :func 0)
                               (make-wasm-export "wrap" :func 1)))))
    ;; 2^64-1 + 2 wraps to 1
    (parity mod :name "add64" :args '(18446744073709551615 2))
    ;; (2^32 + 5) wrapped to i32 is 5
    (parity mod :name "wrap" :args '(4294967301))))

(wasmdown.test:test codegen/locals-defaults
  ;; declared locals default to zero; local.tee stores and keeps the value
  (let ((mod (mmod
              :types (vector (ft '() '(:i32)))
              :funcs (vector
                      (fn 0 (vector (i :i32.const '(:value 5))
                                    (i :local.tee '(:local 1))
                                    (i :drop)
                                    (i :local.get '(:local 1))
                                    (i :local.get '(:local 0))
                                    (i :i32.add)
                                    (i :end))
                          '((:i32 . 2))))
              :exports (vector (make-wasm-export "f" :func 0)))))
    (parity mod)))

(wasmdown.test:test codegen/if-else
  (let ((mod (mmod
              :types (vector (ft '(:i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector
                             (i :local.get '(:local 0))
                             (i :if (list :bt :i32
                                          :else-body
                                          (vector (i :i32.const '(:value 100))
                                                  (i :i32.const '(:value 200))
                                                  (i :i32.add)))
                                 (vector (i :i32.const '(:value 7))))
                             (i :end))))
              :exports (vector (make-wasm-export "f" :func 0)))))
    (parity mod :args '(1))
    (parity mod :args '(0))))

(wasmdown.test:test codegen/nested-blocks-br
  ;; f(n): the outer block (result i32) computes 3+2=5; the inner
  ;; block pushes 42 and br 1 branches out of BOTH blocks with 42 as
  ;; the outer block's result (the branch operand is pushed INSIDE
  ;; the inner block, so it is in scope); f(n) = 42 + n.
  (let ((mod (mmod
              :types (vector (ft '(:i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector
                             (i :block (list :bt :i32)
                                 (vector
                                  (i :i32.const '(:value 3))
                                  (i :i32.const '(:value 2))
                                  (i :i32.add)
                                  (i :block (list :bt nil)
                                      (vector
                                       (i :i32.const '(:value 42))
                                       (i :br (list :label 1))))))
                             (i :local.get '(:local 0))
                             (i :i32.add)
                             (i :end))))
              :exports (vector (make-wasm-export "f" :func 0)))))
    ;; br 1 from the inner block targets the outer (result i32)
    ;; block with 42 -> f(10) = 52
    (parity mod :args '(10))))

(wasmdown.test:test codegen/loop-sum
  ;; sum 1..n with block + loop + br-if (same shape as interp test)
  (let* ((loop-body
          (vector
           (i :local.get '(:local 0))
           (i :i32.eqz)
           (i :br-if (list :label 1))
           (i :local.get '(:local 1))
           (i :local.get '(:local 0))
           (i :i32.add)
           (i :local.set '(:local 1))
           (i :local.get '(:local 0))
           (i :i32.const '(:value 1))
           (i :i32.sub)
           (i :local.set '(:local 0))
           (i :br (list :label 0))))
         (mod (mmod
               :types (vector (ft '(:i32) '(:i32)))
               :funcs (vector
                       (fn 0 (vector
                              (i :i32.const '(:value 0))
                              (i :local.set '(:local 1))
                              (i :block (list :bt nil)
                                  (vector (i :loop (list :bt nil) loop-body)))
                              (i :local.get '(:local 1))
                              (i :end))
                           '((:i32 . 1))))
               :exports (vector (make-wasm-export "sum" :func 0)))))
    (parity mod :name "sum" :args '(10) :emit t)
    (parity mod :name "sum" :args '(0))))

(wasmdown.test:test codegen/br-table
  ;; same three-block shape as the interp fixture: f(0)/(1) -> 30,
  ;; f(2)/default -> 10
  (flet ((blk (body) (i :block (list :bt :i32) body)))
    (let* ((inner
            (blk (vector
                  (i :i32.const '(:value 10))
                  (i :local.get '(:local 0))
                  (i :br-table (list :targets (vector 0 1)
                                     :default 2)))))
           (middle
            (blk (vector
                  inner
                  (i :i32.const '(:value 20))
                  (i :br (list :label 0)))))
           (outer
            (blk (vector
                  middle
                  (i :i32.const '(:value 30))
                  (i :br (list :label 0)))))
           (mod (mmod
                 :types (vector (ft '(:i32) '(:i32)))
                 :funcs (vector (fn 0 (vector outer (i :end))))
                 :exports (vector (make-wasm-export "f" :func 0)))))
      (parity mod :args '(0))
      (parity mod :args '(1))
      (parity mod :args '(2))
      (parity mod :args '(99)))))

(wasmdown.test:test codegen/select-untyped
  ;; untyped (numeric) select: cond ? a : b
  (let ((mod (mmod
              :types (vector (ft '(:i32 :i32 :i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector
                             (i :local.get '(:local 0))
                             (i :local.get '(:local 1))
                             (i :local.get '(:local 2))
                             (i :select)
                             (i :end))))
              :exports (vector (make-wasm-export "f" :func 0)))))
    (parity mod :args '(11 22 1))    ; cond true -> 11
    (parity mod :args '(11 22 0))))  ; cond false -> 22

(wasmdown.test:test codegen/select-typed
  ;; typed select with an explicit (single-element) type vector
  (let ((mod (mmod
              :types (vector (ft '(:i32 :i32 :i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector
                             (i :local.get '(:local 0))
                             (i :local.get '(:local 1))
                             (i :local.get '(:local 2))
                             (i :select '(:types #(:i32)))
                             (i :end))))
              :exports (vector (make-wasm-export "f" :func 0)))))
    (parity mod :args '(11 22 1))
    (parity mod :args '(11 22 0))))

;;; ================================================================
;;; memory load/store + data segment + memory.grow, globals

(defun u8 (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(wasmdown.test:test codegen/memory-data-segment
  ;; active data segment lands in memory; i32.load reads it back from
  ;; both engines, i32.store/i32.load round-trips, and the load's
  ;; offset immediate works. (Unsigned sub-word loads stay excluded
  ;; from this suite: interp bug, see file header.)
  (let ((mod (mmod
              :types (vector (ft '() '(:i32))
                             (ft '(:i32 :i32) '()))
              :funcs (vector
                      ;; get32(): i32.load at 4 (data bytes 4..7)
                      (fn 0 (vector (i :i32.const '(:value 4))
                                    (i :i32.load '(:offset 0 :align 2
                                                            :memidx 0))
                                    (i :end)))
                      ;; set32(addr, v): i32.store
                      (fn 1 (vector (i :local.get '(:local 0))
                                    (i :local.get '(:local 1))
                                    (i :i32.store '(:offset 0 :align 2
                                                            :memidx 0))
                                    (i :end))))
              :mems (vector (make-wasm-memory (lim 1)))
              :datas (vector (make-wasm-data
                              (list :active 0
                                    (vector (i :i32.const '(:value 0))
                                            (i :end)))
                              (u8 #xde #xad #xbe #xef #x11 #x22 #x33 #x44
                                  #x55 #x66 #x77 #x88)))
              :exports (vector (make-wasm-export "get32" :func 0)
                               (make-wasm-export "set32" :func 1)
                               (make-wasm-export "mem" :memory 0)))))
    ;; data bytes 4..7 = 11 22 33 44 -> little-endian word 44332211
    (let ((inst (parity mod :name "get32" :emit t)))
      (expect (call-export inst "get32") '(#x44332211)))
    ;; store parity: both engines agree on set32's result (empty) and
    ;; the written word reads back identically afterwards. The store
    ;; writes at address 4 (where get32 reads), so the data word is
    ;; replaced by the stored value.
    (let ((inst (parity mod :name "set32" :args '(4 #x01020304))))
      (call-export inst "set32" 4 #x01020304)
      (expect (call-export inst "get32") '(#x01020304)))
    ;; and the memory object itself: page count
    (let ((inst (parity mod :name "get32")))
      (expect (memory-size (export-value inst "mem")) 1))))

(wasmdown.test:test codegen/memory-grow
  ;; memory.grow returns the old page count; -1 (u32) when refusing
  (let ((mod (mmod
              :types (vector (ft '(:i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector
                             (i :local.get '(:local 0))
                             (i :memory.grow '(:memidx 0))
                             (i :end))))
              :mems (vector (make-wasm-memory (lim 1 2)))
              :exports (vector (make-wasm-export "grow" :func 0)
                               (make-wasm-export "mem" :memory 0)))))
    (parity mod :name "grow" :args '(1))
    (parity mod :name "grow" :args '(5))))   ; beyond max -> 4294967295

(wasmdown.test:test codegen/globals-get-set
  ;; immutable global reads its init; mutable global set/get round-trips
  (let ((mod (mmod
              :types (vector (ft '() '(:i32))
                             (ft '(:i32) '()))
              :funcs (vector
                      (fn 0 (vector (i :global.get '(:global 0))
                                    (i :end)))
                      (fn 1 (vector (i :local.get '(:local 0))
                                    (i :global.set '(:global 1))
                                    (i :end)))
                      (fn 0 (vector (i :global.get '(:global 1))
                                    (i :end))))
              :globals (vector
                        (make-wasm-global :type :i32 :mutable nil
                                          :init (vector
                                                 (i :i32.const '(:value 7))
                                                 (i :end)))
                        (make-wasm-global :type :i32 :mutable t
                                          :init (vector
                                                 (i :i32.const '(:value 0))
                                                 (i :end))))
              :exports (vector (make-wasm-export "get0" :func 0)
                               (make-wasm-export "set1" :func 1)
                               (make-wasm-export "get1" :func 2)
                               (make-wasm-export "g1" :global 1)))))
    ;; reading the immutable global is pure parity
    (parity mod :name "get0")
    ;; set then get must agree across engines on the SAME instance
    (let ((inst (parity mod :name "get1")))
      (call-export inst "set1" 123)
      (expect (call-export inst "get1") '(123))
      ;; and after compile+install, setting via compiled code mutates
      ;; the global the interpreter still shares
      (call-export inst "set1" 99)
      (expect (call-export inst "get1") '(99))
      (expect (wasmdown.runtime:global-value
               (wasmdown.runtime:export-value inst "g1")) 99))))

;;; ================================================================
;;; direct calls, call_indirect, multivalue, host import, traps

(wasmdown.test:test codegen/call-direct-factorial
  ;; fac(n) = n<=1 ? 1 : n*fac(n-1) — the recursion-in-if-arm shape
  (let* ((fac-body
          (vector
           (i :local.get '(:local 0))
           (i :i32.const '(:value 1))
           (i :i32.le_s)
           (i :if (list :bt :i32
                        :else-body
                        (vector (i :local.get '(:local 0))
                                (i :local.get '(:local 0))
                                (i :i32.const '(:value 1))
                                (i :i32.sub)
                                (i :call '(:func 0))
                                (i :i32.mul)))
               (vector (i :i32.const '(:value 1))))
           (i :end)))
         (mod (mmod
               :types (vector (ft '(:i32) '(:i32)))
               :funcs (vector (fn 0 fac-body))
               :exports (vector (make-wasm-export "fac" :func 0)))))
    (parity mod :name "fac" :args '(5) :emit t)
    (parity mod :name "fac" :args '(10))
    (parity mod :name "fac" :args '(20))
    ;; the acceptance values themselves, compiled-side:
    (let ((inst (parity mod :name "fac" :args '(10))))
      (expect (call-export inst "fac" 10) '(3628800))
      (expect (call-export inst "fac" 20) '(2192834560)))))

(wasmdown.test:test codegen/call-indirect
  ;; ok call, type mismatch and null reference through the table
  (let* ((types (vector (ft '() '(:i32))        ; 0: ()->i32
                        (ft '(:i32) '(:i32))))  ; 1: i32->i32
         (one (fn 0 (vector (i :i32.const '(:value 1)) (i :end))))
         (ident (fn 1 (vector (i :local.get '(:local 0)) (i :end))))
         ;; ok: call table[0] (one, ()->i32)
         (ok (fn 0 (vector (i :i32.const '(:value 0))
                          (i :call-indirect '(:type 0 :table 0))
                          (i :end))))
         ;; bad: expect (i32)->(i32) but table[0] is ()->i32
         (bad (fn 1 (vector (i :local.get '(:local 0))
                            (i :i32.const '(:value 0))
                            (i :call-indirect '(:type 1 :table 0))
                            (i :end))))
         ;; null: table[2] is ref.null
         (nullf (fn 0 (vector (i :i32.const '(:value 2))
                              (i :call-indirect '(:type 0 :table 0))
                              (i :end))))
         (mod (mmod
               :types types
               :funcs (vector one ident ok bad nullf)
               :tables (vector (make-wasm-table :funcref (lim 4 4)))
               ;; table: [0]=one [1]=ident [2]=null [3]=null
               :elems (vector (make-wasm-elem
                               (list :active 0
                                     (vector (i :i32.const '(:value 0))
                                             (i :end)))
                               :funcref
                               (list (vector (i :ref.func '(:func 0)) (i :end))
                                     (vector (i :ref.func '(:func 1)) (i :end))
                                     (vector (i :ref.null '(:type :funcref))
                                             (i :end))
                                     (vector (i :ref.null '(:type :funcref))
                                             (i :end)))))
               :exports (vector (make-wasm-export "ok" :func 2)
                                (make-wasm-export "bad" :func 3)
                                (make-wasm-export "null" :func 4)))))
    (parity mod :name "ok" :emit t)
    (parity mod :name "bad" :args '(5))
    (parity mod :name "null")))

(wasmdown.test:test codegen/multivalue
  ;; block with a 2-result blocktype + a function returning two results
  (let ((mod (mmod
              :types (vector (ft '() '(:i32 :i32))      ; 0: two()
                             (ft '() '(:i32 :i64))      ; 1: block bt
                             (ft '(:i32) '(:i32 :i64))) ; 2: swap(n)
              :funcs (vector
                      ;; two(): block -> (i32 1, i64 99)
                      (fn 1 (vector
                             (i :block (list :bt (cons :typeidx 1))
                                 (vector (i :i32.const '(:value 1))
                                         (i :i64.const '(:value 99))))
                             (i :end)))
                      ;; swap(n): multi-result function (i32, i64)
                      (fn 2 (vector
                             (i :local.get '(:local 0))
                             (i :i64.const '(:value 99))
                             (i :end))))
              :exports (vector (make-wasm-export "two" :func 0)
                               (make-wasm-export "swap" :func 1)))))
    (parity mod :name "two")
    (parity mod :name "swap" :args '(7))))

(wasmdown.test:test codegen/host-import-call
  ;; calling an imported host function from compiled code: the host
  ;; side effect (captured list) must match the interp-side sequence
  (let* ((types (vector (ft '(:i32) '())))
         (calls '())
         (host (make-host-func-instance
                (svref types 0)
                (lambda (args)
                  (push (copy-list args) calls)
                  '())))
         (body (vector (i :local.get '(:local 0))
                       (i :call '(:func 0))
                       (i :local.get '(:local 0))
                       (i :i32.const '(:value 1))
                       (i :i32.add)
                       (i :call '(:func 0))
                       (i :end)))
         (mod (mmod
               :types types
               :imports (vector (make-wasm-import "env" "log" :func 0))
               :funcs (vector (fn 0 body))
               :exports (vector (make-wasm-export "f" :func 1)))))
    (let ((inst (parity mod :name "f" :args '(5)
                        :imports `((("env" "log") . ,host)))))
      (declare (ignore inst))
      ;; two host calls per engine invocation: (5) then (6)
      (expect (reverse calls) '((5) (6) (5) (6))))))

(wasmdown.test:test codegen/traps
  ;; unreachable, div-by-zero, OOB store — same trap kind both engines
  (let ((mod (mmod
              :types (vector (ft '() '())
                             (ft '(:i32 :i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector (i :unreachable) (i :end)))
                      (fn 1 (vector (i :local.get '(:local 0))
                                    (i :local.get '(:local 1))
                                    (i :i32.div_s)
                                    (i :end))))
              :mems (vector (make-wasm-memory (lim 1)))
              :exports (vector (make-wasm-export "boom" :func 0)
                               (make-wasm-export "div" :func 1)))))
    (parity mod :name "boom")
    (parity mod :name "div" :args '(1 0))
    ;; OOB store: dedicated function writing at offset 65535
    (let* ((oob (fn 0 (vector (i :i32.const '(:value 65535))
                              (i :local.get '(:local 0))
                              (i :i32.store '(:offset 0 :align 2 :memidx 0))
                              (i :end))))
           (mod2 (mmod
                  :types (vector (ft '(:i32) '()))
                  :funcs (vector oob)
                  :mems (vector (make-wasm-memory (lim 1)))
                  :exports (vector (make-wasm-export "oob" :func 0)))))
      (parity mod2 :name "oob" :args '(7)))))

;;; ================================================================
;;; ================================================================
;;; Regressions found by running the pinned core suite under the
;;; compiled backend: block values used MID-EXPRESSION, `br` to the
;;; function-body label, and dead code after a branch consuming the
;;; live operand tail.

(defun %parity-wat (wat &rest args)
  "Parse WAT, run F under the interpreter, install the compiled
functions, and require the same result (EQUALP)."
  (let* ((m (wasmdown.text:parse-wat wat))
         (inst (wasmdown:instantiate m))
         (compiled (wasmdown.codegen:compile-module-functions m inst))
         (interp (apply #'call-export inst "f" args)))
    (%install-compiled! m inst compiled)
    (let ((compiled-result (apply #'call-export inst "f" args)))
      (wasmdown.test:is (equalp interp compiled-result))
      compiled-result)))

(wasmdown.test:test codegen/block-value-as-operand
  ;; (select (i32.const 2) (block (result i32) (i32.const 1)) (i32.const 3))
  ;; - the block's result must join the operands already below it. The
  ;; old emitter dropped them (nthcdr nresults) and SELECT read NIL.
  (wasmdown.test:is
   (equalp '(2)
           (%parity-wat
            "(module (func (export \"f\") (result i32)
               (select (i32.const 2) (block (result i32) (i32.const 1))
                       (i32.const 3))))"))))

(wasmdown.test:test codegen/br-to-function-label
  ;; `br 0' at the FUNCTION TOP LEVEL names the function body's own
  ;; label (br.wast's as-return-values shape): without a frame for the
  ;; body the compiler looked up NIL and blew up.
  (wasmdown.test:is
   (equalp '(5)
           (%parity-wat
            "(module (func (export \"f\") (result i32)
               (i32.const 5)
               (br 0)))")))
  ;; the multi-value shape: a `return' inside a block leaves its two
  ;; values, the block yields the second
  (wasmdown.test:is
   (equalp '(2 7)
           (%parity-wat
            "(module (func (export \"f\") (result i32 i64)
               (i32.const 2)
               (block (result i64) (return (br 0 (i32.const 1)
                                                 (i64.const 7))))))"))))

(wasmdown.test:test codegen/dead-code-after-br-keeps-tail
  ;; (i32.add (local.get 0) (block (result i32) (block (br 0))
  ;;          (i32.const 2))) - the inner br's dead region must restore
  ;; the operands it found (the local), not NIL fillers.
  (wasmdown.test:is
   (equalp '(5)
           (%parity-wat
            "(module (func (export \"f\") (param i32) (result i32)
               (i32.add (local.get 0)
                        (block (result i32)
                          (block (br 0))
                          (i32.const 2)))))"
            3))))

(wasmdown.test:test codegen/mutable-local-operand-snapshot
  ;; An operand that is a MUTABLE local must be snapshotted at the
  ;; point of the read. The store's address operand is `(local.get 0)`
  ;; and the VALUE operand reassigns local 0 before the store emits:
  ;; the old emitter kept the local symbol, so the store went to
  ;; address 42 instead of the address the read produced (the defect
  ;; that made compiled SQLite write its page-count byte to address 5).
  (let* ((m (wasmdown.text:parse-wat
             "(module (memory 1)
                (func (export \"f\") (param i32) (result i32)
                  (i32.store8 (i32.const 1024) (i32.const 0))
                  (i32.store8 (i32.const 42) (i32.const 0))
                  (i32.store8 (local.get 0)
                              (local.tee 0 (i32.const 42)))
                  (i32.load8_u (i32.const 1024))))"))
         (inst (wasmdown:instantiate m))
         (compiled (wasmdown.codegen:compile-module-functions m inst))
         (mem (svref (module-instance-memories inst) 0)))
    (wasmdown.test:is (equalp (call-export inst "f" 1024) '(42)))
    (wasmdown.test:is (= 42 (aref (memory-instance-bytes mem) 1024)))
    (wasmdown.test:is (= 0 (aref (memory-instance-bytes mem) 42)))
    (%install-compiled! m inst compiled)
    (wasmdown.test:is (equalp (call-export inst "f" 1024) '(42)))
    (wasmdown.test:is (= 42 (aref (memory-instance-bytes mem) 1024)))
    (wasmdown.test:is (= 0 (aref (memory-instance-bytes mem) 42)))))

;;; -----------------------------------------------------------------
;;; M2: one guest call protocol for both backends.
;;;
;;; The trampoline lives in wasmdown.runtime (INVOKE-WASM): assembled
;;; code and interpreter bodies both run through it, so return_call* is
;;; a transfer that keeps the frame (constant host stack and constant
;;; guest depth), non-tail recursion still traps, and the uncaught-
;;; exception boundary is backend-independent.

(defun %instantiate-wat (text &key (mode :interpret) imports)
  (wasmdown:instantiate (wasmdown:validate-module
                         (wasmdown.text:parse-wat text))
                        :mode mode :imports imports))

(defparameter *tail-count-module*
  "(module (func $count (param i32) (result i32)
             (if (result i32) (i32.eqz (local.get 0))
                 (then (i32.const 0))
                 (else (return_call $count
                         (i32.sub (local.get 0) (i32.const 1))))))
           (func (export \"count\") (param i32) (result i32)
             (return_call $count (local.get 0))))")

(wasmdown.test:test codegen/return-call-constant-stack
  (dolist (mode '(:interpret :compiled))
    (let ((inst (%instantiate-wat *tail-count-module* :mode mode)))
      (wasmdown.test:is (equal '(0) (wasmdown:invoke inst "count" 200000)))
      (wasmdown.test:is (zerop wasmdown.runtime:*guest-depth*)))))

(defparameter *tail-indirect-module*
  "(module (type $t (func (param i32) (result i32)))
           (table 1 funcref)
           (elem (i32.const 0) $count)
           (func $count (type $t)
             (if (result i32) (i32.eqz (local.get 0))
                 (then (i32.const 0))
                 (else (return_call_indirect (type $t)
                         (i32.sub (local.get 0) (i32.const 1))
                         (i32.const 0)))))
           (func (export \"count\") (param i32) (result i32)
             (return_call_indirect (type $t) (local.get 0) (i32.const 0))))")

(wasmdown.test:test codegen/return-call-indirect-constant-stack
  (dolist (mode '(:interpret :compiled))
    (let ((inst (%instantiate-wat *tail-indirect-module* :mode mode)))
      (wasmdown.test:is (equal '(0) (wasmdown:invoke inst "count" 200000)))
      (wasmdown.test:is (zerop wasmdown.runtime:*guest-depth*)))))

(defparameter *tail-ref-module*
  "(module (type $t (func (param i32) (result i32)))
           (func $count (type $t)
             (if (result i32) (i32.eqz (local.get 0))
                 (then (i32.const 0))
                 (else (return_call_ref (type $t)
                         (i32.sub (local.get 0) (i32.const 1))
                         (ref.func $count)))))
           (export \"raw\" (func $count))
           (func (export \"count\") (param i32) (result i32)
             (return_call_ref (type $t) (local.get 0) (ref.func $count))))")

(wasmdown.test:test codegen/return-call-ref-constant-stack
  (dolist (mode '(:interpret :compiled))
    (let ((inst (%instantiate-wat *tail-ref-module* :mode mode)))
      (wasmdown.test:is (equal '(0) (wasmdown:invoke inst "count" 200000)))
      (wasmdown.test:is (zerop wasmdown.runtime:*guest-depth*)))))

(defparameter *non-tail-module*
  "(module (func $spin (param i32)
             (call $spin (i32.add (local.get 0) (i32.const 1))))
           (func (export \"spin\") (call $spin (i32.const 0))))")

(wasmdown.test:test codegen/non-tail-recursion-traps-in-both-backends
  (dolist (mode '(:interpret :compiled))
    (let ((inst (%instantiate-wat *non-tail-module* :mode mode)))
      (wasmdown.test:assert-trap :call-stack-exhausted
        (wasmdown:invoke inst "spin"))
      ;; the protocol unwinds its own binding: a deep trap never leaves
      ;; the counter inflated for the next invocation.
      (wasmdown.test:is (zerop wasmdown.runtime:*guest-depth*)))))

(wasmdown.test:test codegen/mixed-backend-call-graphs
  ;; compiled -> interpreted and interpreted -> compiled through the
  ;; same trampoline; each function keeps its defining backend.
  (let* ((compiled (%instantiate-wat
                    "(module (func $f (result i32) (i32.const 11))
                              (func (export \"f\") (result i32) (call $f)))"
                    :mode :compiled))
         (interped (%instantiate-wat
                    "(module (func $g (result i32) (i32.const 31))
                              (func (export \"g\") (result i32) (call $g)))"
                    :mode :interpret))
         (fv (wasmdown:instance-export compiled "f"))
         (gv (wasmdown:instance-export interped "g"))
         (mixed (%instantiate-wat
                 "(module (import \"a\" \"f\" (func $f (result i32)))
                          (import \"i\" \"g\" (func $g (result i32)))
                          (func (export \"sum\") (result i32)
                            (i32.add (call $f) (call $g))))"
                 :mode :compiled
                 :imports (list (cons '("a" "f") fv) (cons '("i" "g") gv)))))
    (wasmdown.test:is (equal '(42) (wasmdown:invoke mixed "sum")))
    (wasmdown.test:is (eq :compiled (wasmdown.runtime:func-instance-backend fv)))
    (wasmdown.test:is (eq :interpret (wasmdown.runtime:func-instance-backend gv)))))

(wasmdown.test:test codegen/uncaught-exception-boundary
  ;; an uncaught guest exception traps at the host<->guest edge in the
  ;; interpreter; the compiled backend refuses `throw` explicitly until
  ;; the EH lowering lands (M7) - never a silent interpreter fallback.
  (handler-case
      (progn (wasmdown:invoke
              (%instantiate-wat "(module (tag $e)
                                        (func (export \"boom\") (throw $e)))")
              "boom")
             (wasmdown.test:is nil))
    (wasmdown:wasm-trap (c)
      (wasmdown.test:is (eq :uncaught-exception (wasmdown:wasm-trap-kind c)))))
  ;; the compiled backend lowers throw/try_table, so an uncaught guest
  ;; exception crosses the same boundary as in the interpreter.
  (handler-case
      (progn (wasmdown:invoke
              (%instantiate-wat "(module (tag $e)
                                        (func (export \"boom\") (throw $e)))"
                                :mode :compiled)
              "boom")
             (wasmdown.test:is nil))
    (wasmdown:wasm-trap (c)
      (wasmdown.test:is (eq :uncaught-exception (wasmdown:wasm-trap-kind c))))))

;;; -----------------------------------------------------------------
;;; M3: saved operand views, arms, and cross-table copying.

(defun %parity-modes (text name &rest args)
  "Run NAME in both explicitly selected backends and require equal
results (and equal trap kinds). Uses the public instantiate path."
  (let ((i (apply #'wasmdown:invoke
                                   (%instantiate-wat text :mode :interpret)
                                   name args))
        (b-in (handler-case
                  (list :ok (apply #'wasmdown:invoke
                                   (%instantiate-wat text :mode :compiled)
                                   name args))
                (wasmdown:wasm-trap (c) (list :trap (wasmdown:wasm-trap-kind c))))))
    (wasmdown.test:is (equal (list :ok i) b-in))
    b-in))

(wasmdown.test:test codegen/mutable-local-in-saved-block-tail
  ;; an operand pushed from a local BELOW an open block keeps the value
  ;; it was read with even when the block writes that local: the saved
  ;; tail of every open frame is part of the audit.
  (%parity-modes
   "(module (func (export \"f\") (result i32)
              (local $x i32)
              (local.set $x (i32.const 10))
              (local.get $x)
              (block (result i32)
                (local.set $x (i32.const 99))
                (i32.const 1))
              (i32.add)))"
   "f")
  (wasmdown.test:is (equal '(:ok (11))
                          (%parity-modes
                           "(module (func (export \"f\") (result i32)
                                      (local $x i32)
                                      (local.set $x (i32.const 10))
                                      (local.get $x)
                                      (block (result i32)
                                        (local.set $x (i32.const 99))
                                        (i32.const 1))
                                      (i32.add)))"
                           "f"))))

(wasmdown.test:test codegen/cross-table-copy
  ;; table.copy across two tables: each span is bounds-checked against
  ;; its own table, then call_indirect resolves through the destination.
  (wasmdown.test:is
   (equal '(:ok (2))
          (%parity-modes
           "(module
              (type $t (func (result i32)))
              (table $a 4 funcref)
              (table $b 4 funcref)
              (elem (table $a) (i32.const 0) func $one $two)
              (func $one (type $t) (i32.const 1))
              (func $two (type $t) (i32.const 2))
              (func (export \"f\") (result i32)
                (table.copy $b $a (i32.const 0) (i32.const 0) (i32.const 2))
                (call_indirect $b (type $t) (i32.const 1))))"
           "f"))))

(wasmdown.test:test codegen/if-arms-share-one-entry-state
  ;; the then arm's unreachable tail must not corrupt the else arm: both
  ;; arms rebuild their operand stack from the same saved entry state.
  (let ((r (%parity-modes
            "(module (func (export \"f\") (param i32) (result i32)
                       (i32.const 5)
                       (if (result i32) (local.get 0)
                           (then (i32.const 1) (br 0) (drop))
                           (else (i32.const 3)))
                       (i32.add)))"
            "f" 1)))
    (wasmdown.test:is (equal '(:ok (6)) r))))

;;; -----------------------------------------------------------------
;;; M4: SIMD (v128 + relaxed) and GC/typed references.
;;;
;;; Both families share their value-level semantics with the reference
;;; interpreter (WASMDOWN.SIMD:EXECUTE-OP and the WASMDOWN.RUNTIME GC
;;; helpers), so these fixtures are differential: every case runs the
;;; SAME module through wasmdown:instantiate in :interpret and
;;; :compiled and requires equal results and equal trap kinds.

(defun %parity-modes-equalp (text name &rest args)
  "Like %PARITY-MODES, but a trap is an OBSERVABLE of both backends
(compared by kind) and results compare with EQUALP — v128 results are
byte vectors, which EQUAL would distinguish by identity."
  (flet ((run (mode)
           (handler-case
               (list :ok (apply #'wasmdown:invoke
                                (%instantiate-wat text :mode mode) name args))
             (wasmdown:wasm-trap (c)
               (list :trap (wasmdown:wasm-trap-kind c))))))
    (let ((i (run :interpret))
          (b (run :compiled)))
      (wasmdown.test:is (equalp i b))
      b)))

(wasmdown.test:test codegen/simd-const-and-lane-extract
  ;; v128.const + a lane extract: the compiled value is a v128, the
  ;; extract reads the same lane the interpreter does.
  (wasmdown.test:is
   (equal '(:ok (3))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result i32)
                (i32x4.extract_lane 2 (v128.const i32x4 1 2 3 4))))"
           "f")))
  ;; a v128 RESULT crosses the host boundary as the 16-byte value
  (wasmdown.test:is
   (equalp '(:ok (#(1 0 0 0 2 0 0 0 3 0 0 0 4 0 0 0)))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result v128)
                (v128.const i32x4 1 2 3 4)))"
           "f")))
  ;; i64x2 lane extract keeps the 64-bit lane value
  (wasmdown.test:is
   (equal '(:ok (4294967297))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result i64)
                (i64x2.extract_lane 0
                  (v128.const i64x2 4294967297 1))))"
           "f"))))

(wasmdown.test:test codegen/simd-i8x16-arithmetic-and-shuffle
  (wasmdown.test:is
   (equal '(:ok (17))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result i32)
                (i8x16.extract_lane_u 0
                  (i8x16.add
                    (v128.const i8x16 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
                    (v128.const i8x16 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))))"
           "f")))
  ;; i8x16.shuffle selects across BOTH operands (indices 16..31 name
  ;; the second vector)
  (wasmdown.test:is
   (equalp '(:ok (#(0 17 2 19 4 21 6 23 8 25 10 27 12 29 14 31)))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result v128)
                (i8x16.shuffle 0 17 2 19 4 21 6 23 8 25 10 27 12 29 14 31
                  (v128.const i8x16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                  (v128.const i8x16 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))))"
           "f")))
  ;; swizzle zeroes out-of-range indices
  (wasmdown.test:is
   (equalp '(:ok (#(10 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result v128)
                (i8x16.swizzle
                  (v128.const i8x16 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
                  (v128.const i8x16 0 1 16 17 18 19 20 21 22 23 24 25 26 27 28 29))))"
           "f"))))

(wasmdown.test:test codegen/simd-memory-and-lane-memory
  ;; v128.store then v128.load round-trips through the instance memory
  ;; with the full 16-byte span bounds-checked by the shared loader.
  (wasmdown.test:is
   (equal '(:ok (2055))
          (%parity-modes-equalp
           "(module
              (memory 1)
              (func (export \"f\") (result i32)
                (v128.store (i32.const 0)
                  (v128.const i8x16 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
                (i16x8.extract_lane_u 3 (v128.load (i32.const 0)))))"
           "f")))
  ;; load32_lane writes one lane of the operand vector from memory
  (wasmdown.test:is
   (equal '(:ok (305419896))
          (%parity-modes-equalp
           "(module
              (memory 1)
              (func (export \"f\") (result i32)
                (i32.store (i32.const 32) (i32.const 305419896))
                (i32x4.extract_lane 1
                  (v128.load32_lane offset=0 1 (i32.const 32)
                    (v128.const i32x4 7 8 9 10)))))"
           "f")))
  ;; store8_lane writes exactly one lane byte
  (wasmdown.test:is
   (equal '(:ok (77))
          (%parity-modes-equalp
           "(module
              (memory 1)
              (func (export \"f\") (result i32)
                (v128.store8_lane offset=0 5 (i32.const 48)
                  (v128.const i8x16 0 0 0 0 0 77 0 0 0 0 0 0 0 0 0 0))
                (i32.load8_u (i32.const 48))))"
           "f")))
  ;; an out-of-bounds v128.load traps the SAME way in both backends
  (wasmdown.test:is
   (equal '(:trap :out-of-bounds-memory)
          (%parity-modes-equalp
           "(module
              (memory 1)
              (func (export \"f\") (result i32)
                (i32x4.extract_lane 0 (v128.load (i32.const 65535)))))"
           "f")))
  ;; a splat load and a zero-padded load
  (wasmdown.test:is
   (equalp '(:ok (#(9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)))
          (%parity-modes-equalp
           "(module
              (memory 1)
              (func (export \"f\") (result v128)
                (i32.store8 (i32.const 0) (i32.const 9))
                (v128.load8_splat (i32.const 0))))"
           "f")))
  (wasmdown.test:is
   (equalp '(:ok (#(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
          (%parity-modes-equalp
           "(module
              (memory 1)
              (func (export \"f\") (result v128)
                (i32.store8 (i32.const 0) (i32.const 1))
                (v128.load32_zero (i32.const 0))))"
           "f"))))

(wasmdown.test:test codegen/simd-relaxed
  ;; relaxed swizzle (canonical: OOB -> 0) and relaxed q15mulr
  (wasmdown.test:is
   (equalp '(:ok (#(10 11 12 13 14 15 16 17 0 0 0 0 0 0 0 0)))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result v128)
                (i8x16.relaxed_swizzle
                  (v128.const i8x16 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)
                  (v128.const i8x16 0 1 2 3 4 5 6 7 200 201 202 203 204 205 206 207))))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (1))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result i32)
                (i16x8.extract_lane_u 0
                  (i16x8.relaxed_q15mulr_s
                    (v128.const i16x8 16384 2 3 4 5 6 7 8)
                    (v128.const i16x8 2 2 3 4 5 6 7 8)))))"
           "f"))))

(wasmdown.test:test codegen/gc-struct-new-get-set-packed
  ;; struct.new normalizes a 200 into the i8 field; get_u/get_s read
  ;; width-wrapped and sign-extended respectively.
  (wasmdown.test:is
   (equal '(:ok (200))
          (%parity-modes-equalp
           "(module
              (type $p (struct (field i32) (field i8)))
              (func (export \"f\") (result i32)
                (struct.get_u $p 1
                  (struct.new $p (i32.const 1) (i32.const 200)))))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (4294967240))
          (%parity-modes-equalp
           "(module
              (type $p (struct (field i32) (field i8)))
              (func (export \"f\") (result i32)
                (struct.get_s $p 1
                  (struct.new $p (i32.const 1) (i32.const 200)))))"
           "f")))
  ;; struct.new_default zeroes fields; struct.set wraps on store.
  (wasmdown.test:is
   (equal '(:ok (4294967240))
          (%parity-modes-equalp
           "(module
              (type $p (struct (field i32) (field (mut i8))))
              (func (export \"f\") (result i32)
                (local $s (ref null $p))
                (local.set $s (struct.new_default $p))
                (struct.set $p 1 (local.get $s) (i32.const 200))
                (struct.get_s $p 1 (local.get $s))))"
           "f")))
  ;; a null struct.get traps :null-reference in both backends
  (wasmdown.test:is
   (equal '(:trap :null-reference)
          (%parity-modes-equalp
           "(module
              (type $p (struct (field i32)))
              (func (export \"f\") (result i32)
                (struct.get $p 0 (ref.null $p))))"
           "f"))))

(wasmdown.test:test codegen/gc-array-new-get-len
  (wasmdown.test:is
   (equal '(:ok (513))
          (%parity-modes-equalp
           "(module
              (type $a (array (mut i16)))
              (func (export \"f\") (result i32)
                (array.get_u $a
                  (array.new $a (i32.const 513) (i32.const 3))
                  (i32.const 2))))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (4))
          (%parity-modes-equalp
           "(module
              (type $a (array (mut i16)))
              (func (export \"f\") (result i32)
                (array.len (array.new_default $a (i32.const 4)))))"
           "f")))
  ;; new_fixed + get_s, then fill + init_data in the same module
  (wasmdown.test:is
   (equal '(:ok (7))
          (%parity-modes-equalp
           "(module
              (type $a (array (mut i8)))
              (func (export \"f\") (result i32)
                (array.get_u $a
                  (array.new_fixed $a 3 (i32.const 5) (i32.const 6) (i32.const 7))
                  (i32.const 2))))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (9))
          (%parity-modes-equalp
           "(module
              (type $a (array (mut i8)))
              (func (export \"f\") (result i32)
                (local $x (ref null $a))
                (local.set $x (array.new_default $a (i32.const 4)))
                (array.fill $a (local.get $x) (i32.const 0) (i32.const 9)
                            (i32.const 2))
                (array.get_u $a (local.get $x) (i32.const 1))))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (2))
          (%parity-modes-equalp
           "(module
              (type $a (array (mut i8)))
              (data \"\\01\\02\\03\")
              (func (export \"f\") (result i32)
                (array.get_u $a
                  (array.new_data $a 0 (i32.const 1) (i32.const 2))
                  (i32.const 0))))"
           "f")))
  ;; array.copy and array.init_data mutate an existing array
  (wasmdown.test:is
   (equal '(:ok (5))
          (%parity-modes-equalp
           "(module
              (type $a (array (mut i8)))
              (data \"\\05\\06\\07\")
              (func (export \"f\") (result i32)
                (local $x (ref null $a))
                (local $y (ref null $a))
                (local.set $x (array.new_fixed $a 3 (i32.const 5) (i32.const 6) (i32.const 7)))
                (local.set $y (array.new_default $a (i32.const 3)))
                (array.copy $a $a (local.get $y) (i32.const 0)
                            (local.get $x) (i32.const 0) (i32.const 3))
                (array.get_u $a (local.get $y) (i32.const 0))))"
           "f")))
  ;; out-of-bounds array.get traps with the TABLE kind in both
  (wasmdown.test:is
   (equal '(:trap :out-of-bounds-table)
          (%parity-modes-equalp
           "(module
              (type $a (array (mut i8)))
              (func (export \"f\") (result i32)
                (array.get $a (array.new_default $a (i32.const 1))
                           (i32.const 1))))"
           "f"))))

(wasmdown.test:test codegen/gc-i31
  (wasmdown.test:is
   (equal '(:ok (4294967295))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result i32)
                (i31.get_s (ref.i31 (i32.const -1)))))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (2147483647))
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result i32)
                (i31.get_u (ref.i31 (i32.const 2147483647)))))"
           "f")))
  ;; a null i31 traps :null-reference
  (wasmdown.test:is
   (equal '(:trap :null-reference)
          (%parity-modes-equalp
           "(module
              (func (export \"f\") (result i32)
                (i31.get_s (ref.null i31))))"
           "f"))))

(wasmdown.test:test codegen/gc-ref-test-and-cast
  ;; ref.test true and false against concrete struct types
  (wasmdown.test:is
   (equal '(:ok (1))
          (%parity-modes-equalp
           "(module (type $a (struct (field i32))) (type $b (struct))
              (func (export \"f\") (result i32)
                (ref.test (ref $a) (struct.new $a (i32.const 1)))))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (0))
          (%parity-modes-equalp
           "(module (type $a (struct (field i32))) (type $b (struct))
              (func (export \"f\") (result i32)
                (ref.test (ref $b) (struct.new $a (i32.const 1)))))"
           "f")))
  ;; ref.cast succeeds and yields the value
  (wasmdown.test:is
   (equal '(:ok (9))
          (%parity-modes-equalp
           "(module (type $a (struct (field i32)))
              (func (export \"f\") (result i32)
                (struct.get $a 0
                  (ref.cast (ref $a) (struct.new $a (i32.const 9))))))"
           "f")))
  ;; ref.cast failure traps :cast-failure in both backends
  (wasmdown.test:is
   (equal '(:trap :cast-failure)
          (%parity-modes-equalp
           "(module (type $a (struct (field i32))) (type $b (struct))
              (func (export \"f\") (result i32)
                (ref.cast (ref $b) (struct.new $a (i32.const 1)))
                (drop)
                (i32.const 0)))"
           "f")))
  ;; null is only accepted against a NULLABLE target
  (wasmdown.test:is
   (equal '(:ok (1 0))
          (%parity-modes-equalp
           "(module (type $a (struct))
              (func (export \"f\") (result i32 i32)
                (ref.test (ref null $a) (ref.null none))
                (ref.test (ref $a) (ref.null none))))"
           "f"))))

(wasmdown.test:test codegen/gc-br-on-cast
  ;; br_on_cast branches WITH the value on success, falls through with
  ;; it on failure; br_on_cast_fail mirrors. Same module shape as the
  ;; interpreter's cast-family fixture.
  (wasmdown.test:is
   (equal '(:ok (1))
          (%parity-modes-equalp
           "(module (type $a (struct))
              (func (export \"f\") (result i32)
                (block $l (result (ref $a))
                  (br_on_cast $l anyref (ref $a) (struct.new_default $a))
                  (drop)
                  (i32.const 0)
                  (return))
                (drop)
                (i32.const 1)))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (0))
          (%parity-modes-equalp
           "(module (type $a (struct))
              (func (export \"f\") (result i32)
                (block $l (result (ref $a))
                  (br_on_cast $l anyref (ref $a) (ref.null none))
                  (drop)
                  (i32.const 0)
                  (return))
                (drop)
                (i32.const 1)))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (1))
          (%parity-modes-equalp
           "(module (type $a (struct))
              (func (export \"f\") (result i32)
                (block $l (result anyref)
                  (br_on_cast_fail $l anyref (ref $a) (struct.new_default $a))
                  (drop)
                  (i32.const 1)
                  (return))
                (drop)
                (i32.const 0)))"
           "f")))
  (wasmdown.test:is
   (equal '(:ok (0))
          (%parity-modes-equalp
           "(module (type $a (struct))
              (func (export \"f\") (result i32)
                (block $l (result anyref)
                  (br_on_cast_fail $l anyref (ref $a) (ref.null none))
                  (drop)
                  (i32.const 1)
                  (return))
                (drop)
                (i32.const 0)))"
           "f"))))

(wasmdown.test:test codegen/gc-ref-eq-and-conversions
  ;; ref.eq is reference IDENTITY: equal i31 payloads are the same
  ;; value, two fresh structs are not.
  (wasmdown.test:is
   (equal '(:ok (1 0 0))
          (%parity-modes-equalp
           "(module (type $a (struct))
              (func (export \"f\") (result i32 i32 i32)
                (ref.eq (ref.i31 (i32.const 5)) (ref.i31 (i32.const 5)))
                (ref.eq (ref.i31 (i32.const 5)) (ref.i31 (i32.const 6)))
                (ref.eq (struct.new_default $a) (struct.new_default $a))))"
           "f")))
  ;; any <-> extern round trip preserves the value, and null stays null
  (wasmdown.test:is
   (equal '(:ok (1 1))
          (%parity-modes-equalp
           "(module (type $a (struct))
              (func (export \"f\") (result i32 i32)
                (ref.test (ref $a)
                  (any.convert_extern (extern.convert_any (struct.new_default $a))))
                (ref.is_null
                  (any.convert_extern (extern.convert_any (ref.null none))))))"
           "f"))))

(wasmdown.test:test codegen/refusal-names-the-opcode
  ;; Every opcode the decoder and validator can produce now has a
  ;; lowering, so no valid module reaches the refusal. The path must
  ;; still exist and stay a wasmdown:codegen-error (never an
  ;; interpreter fallback): pinned at the emitter boundary with an
  ;; opcode no layer produces today.
  (let ((wasmdown.codegen::*current-function-index* 3))
    (handler-case
        (progn
          (wasmdown.codegen::%emit-instr
           (wasmdown.codegen::make-cctx)
           (make-wasm-instr :i32.not-an-opcode '(:value 1) #() 7))
          (wasmdown.test:is nil))
      (wasmdown:codegen-error (c)
        (wasmdown.test:is (eq :i32.not-an-opcode (wasmdown:codegen-error-op c)))
        (wasmdown.test:is (eql 3 (wasmdown:codegen-error-function-index c)))
        (wasmdown.test:is (eql 7 (wasmdown:codegen-error-offset c)))
        (wasmdown.test:is (search "no lowering"
                                  (wasmdown:wasmdown-error-message c)))))))

;;; -----------------------------------------------------------------
;;; Formerly-excluded parity: signed sub-word loads and float
;;; load/store at the memory boundary.

(wasmdown.test:test codegen/signed-subword-loads
  ;; sign extension goes to the RESULT width, not the load width.
  (dolist (case '(("(module (memory 1) (func (export \"f\") (result i32)
                       (i32.store8 (i32.const 0) (i32.const 255))
                       (i32.load8_s (i32.const 0))))"
                   4294967295)
                 ("(module (memory 1) (func (export \"f\") (result i32)
                       (i32.store16 (i32.const 0) (i32.const 65535))
                       (i32.load16_s (i32.const 0))))"
                   4294967295)
                 ("(module (memory 1) (func (export \"f\") (result i64)
                       (i64.store32 (i32.const 0) (i64.const 4294967295))
                       (i64.load32_s (i32.const 0))))"
                   18446744073709551615)))
    (destructuring-bind (wat expected) case
      (wasmdown.test:is (equal (list :ok (list expected))
                              (%parity-modes wat "f"))))))

(wasmdown.test:test codegen/float-memory-boundary
  ;; floats cross the memory boundary as floats, never raw bits.
  (dolist (case '(("(module (memory 1) (func (export \"f\") (result f32)
                       (f32.store (i32.const 0) (f32.const 1.5))
                       (f32.load (i32.const 0))))" 1.5)
                 ("(module (memory 1) (func (export \"f\") (result f64)
                       (f64.store (i32.const 0) (f64.const -2.25))
                       (f64.load (i32.const 0))))" -2.25d0)))
    (destructuring-bind (wat expected) case
      (let ((r (%parity-modes wat "f")))
        (wasmdown.test:is (eq :ok (first r)))
        (wasmdown.test:is (eql expected (first (second r))))))))

(wasmdown.test:test codegen/float-simd-lanes
  ;; float lanes carry FLOAT scalars: splat/extract/replace convert at
  ;; the boundary, in both backends (shared wasmdown.simd:execute-op).
  (dolist (case '(("(module (func (export \"f\") (result f32)
                        (f32x4.extract_lane 0 (f32x4.splat (f32.const 1.5)))))"
                   1.5)
                  ("(module (func (export \"f\") (result f64)
                        (f64x2.extract_lane 1 (f64x2.splat (f64.const 2.25)))))"
                   2.25d0)
                  ("(module (func (export \"f\") (result f32)
                        (f32x4.extract_lane 2
                          (f32x4.replace_lane 2 (v128.const i32x4 0 0 0 0)
                                             (f32.const 3.5)))))"
                   3.5)))
    (destructuring-bind (wat expected) case
      (let ((r (%parity-modes wat "f")))
        (wasmdown.test:is (eq :ok (first r)))
        (wasmdown.test:is (eql expected (first (second r))))))))
