;;;; Parity suite for wasmdown.codegen (docs/design/PLAN.md 12.3): every fixture is
;;;; instantiated once, invoked through the reference interpreter (the
;;;; default *INVOKE-WASM-FUNC*), then compiled with COMPILE-MODULE-
;;;; FUNCTIONS, installed into the same instance, and invoked again;
;;;; results must agree (EQUALP) and traps must agree in kind.
;;;;
;;;; Excluded on purpose: signed sub-word loads/stores (i32.load8_s and
;;;; friends) and f32/f64 load/store fixtures. The interpreter currently
;;;; sign-extends to the load width instead of the result width and
;;;; passes raw bits at float memory boundaries (known interp.lisp
;;;; bugs, reported separately); codegen implements the CONTRACTS.md
;;;; shape, so the two engines disagree and parity would fail through
;;;; no fault of the backend. Main re-adds these after the interp fix.

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
  "Replace the instance's interpreter-backed :wasm func-instances with
  host instances wrapping the COMPILED closures (absolute funcidx =
  imported funcs + defined index), then refresh the value surfaces that
  captured the old instances: function exports, table elements and
  element-segment refs (same contract as api.lisp's baseline mode)."
  (let* ((nimp (count :func (wasm-module-imports mod)
                      :key #'wasm-import-kind))
         (funcs (module-instance-funcs inst))
         (types (wasm-module-types mod)))
    (flet ((compiled-for (v)
             "The compiled host func-instance replacing V, or NIL."
             (and (func-instance-p v)
                  (eq (func-instance-kind v) :wasm)
                  (eq (func-instance-module v) inst)
                  (< (func-instance-wasm-funcidx v) (length compiled))
                  (make-host-func-instance
                   (svref types (wasm-func-typeidx
                                 (svref (wasm-module-funcs mod)
                                        (func-instance-wasm-funcidx v))))
                   (svref compiled (func-instance-wasm-funcidx v))))))
      (loop for j below (length compiled)
            do (let ((f (svref (wasm-module-funcs mod) j)))
                 (setf (svref funcs (+ nimp j))
                       (make-host-func-instance
                        (svref types (wasm-func-typeidx f))
                        (svref compiled j)))))
      ;; refresh function exports from the export section (absolute
      ;; funcidx = funcs-vector index)
      (loop for ex across (wasm-module-exports mod)
            when (eq (wasm-export-kind ex) :func)
              do (let ((cell (assoc (wasm-export-name ex)
                                    (module-instance-exports inst)
                                    :test #'equal)))
                   (setf (cdr cell)
                         (svref funcs (wasm-export-index ex)))))
      ;; call_indirect targets captured in tables + elem-seg refs
      (loop for tab across (module-instance-tables inst)
            do (let ((elems (table-instance-elems tab)))
                 (loop for j below (length elems)
                       for c = (compiled-for (svref elems j))
                       when c do (setf (svref elems j) c))))
      (loop for seg across (wasmdown.runtime::module-instance-elem-segs inst)
            do (setf (wasmdown.runtime::elem-seg-state-refs seg)
                     (mapcar (lambda (v) (or (compiled-for v) v))
                             (wasmdown.runtime::elem-seg-state-refs seg)))))))


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
  ;; that made baseline SQLite write its page-count byte to address 5).
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
