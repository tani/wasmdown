;;;; Integration tests for wasmdown.interp: reference execution of
;;;; arithmetic, locals, control flow, calls, memory, globals, host
;;;; imports, multivalue, select, and traps. Modules are built
;;;; programmatically per src/types.lisp; execution goes through the
;;;; shared wasm-call trampoline (which dispatches to interp-invoke).

(defpackage #:wasmdown.test.interp
  (:use #:cl #:wasmdown.types #:wasmdown.runtime)
  (:nicknames #:wd.test.interp))

(in-package #:wasmdown.test.interp)

(defun i (op &optional imms body)
  (make-wasm-instr op imms (or body #()) 0))

(defun endi () (i :end))

(defun ft (params results)
  (make-wasm-func-type params results))

(defun fn (typeidx body &optional (locals '()))
  (make-wasm-func typeidx locals body 0))

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

(defun expect (form value)
  (wasmdown.test:is (equal value form)))

(defun lim (min &optional max) (make-wasm-limits min max))

;;; ================================================================
;;; arithmetic, locals, constants

(wasmdown.test:test interp/arithmetic
  (let ((mod (mmod
              :types (vector (ft '(:i32 :i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector (i :local.get (list :local 0))
                                    (i :local.get (list :local 1))
                                    (i :i32.add)
                                    (i :i32.const (list :value 10))
                                    (i :i32.mul)
                                    (i :i32.const (list :value 3))
                                    (i :i32.sub)
                                    (endi))))
              :exports (vector (make-wasm-export "f" :func 0)))))
    (let ((inst (make-module-instance mod '())))
      ;; (a+b)*10 - 3 with a=2,b=5 -> 67
      (expect (call-export inst "f" 2 5) '(67))
      ;; i32 wraparound: (a+b) can exceed 2^32; result stays unsigned
      (expect (call-export inst "f" 4294967295 1) '(#xfffffffd)))))

(wasmdown.test:test interp/locals-defaults
  ;; declared locals default to zero; local.tee returns the stored value
  (let ((mod (mmod
              :types (vector (ft '() '(:i32)))
              :funcs (vector
                      (fn 0 (vector (i :i32.const (list :value 5))
                                    (i :local.tee (list :local 1))
                                    (i :drop)
                                    (i :local.get (list :local 1))
                                    (i :local.get (list :local 0))
                                    (i :i32.add)
                                    (endi))
                          '((:i32 . 2))          ; two i32 locals: idx 0,1
                          ))
              :exports (vector (make-wasm-export "f" :func 0)))))
    (let ((inst (make-module-instance mod '())))
      (expect (call-export inst "f") '(5)))))

;;; ================================================================
;;; control flow

(wasmdown.test:test interp/loop-sum
  ;; sum 1..n with block + loop + br-if
  (let* ((loop-body
           (vector
            ;; if n==0 exit the block (label 1 from inside the loop)
            (i :local.get (list :local 0))
            (i :i32.eqz)
            (i :br-if (list :label 1))
            ;; acc += n
            (i :local.get (list :local 1))
            (i :local.get (list :local 0))
            (i :i32.add)
            (i :local.set (list :local 1))
            ;; n -= 1; br loop
            (i :local.get (list :local 0))
            (i :i32.const (list :value 1))
            (i :i32.sub)
            (i :local.set (list :local 0))
            (i :br (list :label 0))))
         (mod (mmod
               :types (vector (ft '(:i32) '(:i32)))
               :funcs (vector
                       (fn 0 (vector
                              (i :i32.const (list :value 0))
                              (i :local.set (list :local 1))
                              (i :block (list :bt nil)
                                  (vector (i :loop (list :bt nil) loop-body)))
                              (i :local.get (list :local 1))
                              (endi))
                           '((:i32 . 1))))
               :exports (vector (make-wasm-export "sum" :func 0)))))
    (let ((inst (make-module-instance mod '())))
      (expect (call-export inst "sum" 10) '(55))
      (expect (call-export inst "sum" 0) '(0)))))

(wasmdown.test:test interp/if-else
  (let ((mod (mmod
              :types (vector (ft '(:i32) '(:i32)))
              :funcs (vector
                      (fn 0 (vector
                             (i :local.get (list :local 0))
                             (i :if (list :bt :i32
                                          :else-body
                                          (vector (i :i32.const (list :value 100))
                                                  (i :i32.const (list :value 200))
                                                  (i :i32.add)))
                                 (vector (i :i32.const (list :value 7))))
                             (endi))))
              :exports (vector (make-wasm-export "f" :func 0)))))
    (let ((inst (make-module-instance mod '())))
      (expect (call-export inst "f" 1) '(7))
      (expect (call-export inst "f" 0) '(300)))))

(wasmdown.test:test interp/br-table
  ;; br_table to depth d exits the d-th enclosing block with the top
  ;; stack values as its results; execution continues after that
  ;; block. Design: inner holds the br_table; each enclosing block
  ;; then pushes its own constant and branches out of itself, so the
  ;; outermost surviving value differs per target depth.
  ;;   f(0) -> exit inner -> middle pushes 20, exits -> outer pushes 30 -> 30
  ;;   f(1) -> exit middle (with br operand 10) -> outer pushes 30   -> 30
  ;;   f(2) -> exit outer (operand 10)                              -> 10
  (flet ((blk (body) (i :block (list :bt :i32) body)))
    (let* ((inner
            (blk (vector
                  (i :i32.const (list :value 10))     ; branch operand
                  (i :local.get (list :local 0))
                  (i :br-table (list :targets (vector 0 1)
                                     :default 2)))))
           (middle
            (blk (vector
                  inner
                  (i :i32.const (list :value 20))
                  (i :br (list :label 0)))))
           (outer
            (blk (vector
                  middle
                  (i :i32.const (list :value 30))
                  (i :br (list :label 0)))))
           (mod (mmod
                 :types (vector (ft '(:i32) '(:i32)))
                 :funcs (vector (fn 0 (vector outer (i :end))))
                 :exports (vector (make-wasm-export "f" :func 0)))))
      (let ((inst (make-module-instance mod '())))
        (expect (call-export inst "f" 0) '(30))
        (expect (call-export inst "f" 1) '(30))
        (expect (call-export inst "f" 2) '(10))
        (expect (call-export inst "f" 99) '(10))))))

(wasmdown.test:test interp/unreachable-trap
  (let ((mod (mmod
              :types (vector (ft '() '()))
                     :funcs (vector (fn 0 (vector (i :unreachable) (i :end))))
                     :exports (vector (make-wasm-export "boom" :func 0)))))
    (let ((inst (make-module-instance mod '())))
      (wasmdown.test:assert-trap :unreachable (call-export inst "boom")))))

;;; ================================================================
;;; calls

(wasmdown.test:test interp/call-direct-recursion
  ;; factorial via self-recursion: fac(n) = n<=1 ? 1 : n*fac(n-1)
  (let* ((types (vector (ft '(:i32) '(:i32))))
         (fac-body
           (vector
            ;; if n <= 1 -> 1
            (i :local.get (list :local 0))
            (i :i32.const (list :value 1))
            (i :i32.le_s)
            (i :if (list :bt :i32
                         ;; else: n * fac(n-1)
                         :else-body
                         (vector (i :local.get (list :local 0))
                                 (i :local.get (list :local 0))
                                 (i :i32.const (list :value 1))
                                 (i :i32.sub)
                                 (i :call (list :func 0))
                                 (i :i32.mul)))
                (vector (i :i32.const (list :value 1))))
            (i :end)))
         (mod (mmod
               :types types
               :funcs (vector (fn 0 fac-body))
               :exports (vector (make-wasm-export "fac" :func 0)))))
    (let ((inst (make-module-instance mod '())))
      (expect (call-export inst "fac" 1) '(1))
      (expect (call-export inst "fac" 5) '(120))
      (expect (call-export inst "fac" 10) '(3628800)))))

(wasmdown.test:test interp/call-stack-exhausted-trap
  ;; Unbounded guest recursion f(n) = f(n) + 1 must trap
  ;; :call-stack-exhausted (CONTRACTS.md) — not crash the host — and
  ;; the process must survive the trap: execution continues after it.
  (let* ((spin-body
           (vector
            (i :local.get (list :local 0))
            (i :call (list :func 0))
            (i :i32.const (list :value 1))
            (i :i32.add)
            (i :end)))
         (mod (mmod
               :types (vector (ft '(:i32) '(:i32))    ; 0: i32->i32
                              (ft '() '(:i32)))       ; 1: ()->i32
               :funcs (vector (fn 0 spin-body)
                              ;; shallow func 1: the survival probe
                              (fn 1 (vector (i :i32.const (list :value 42))
                                            (i :end))))
               :exports (vector (make-wasm-export "spin" :func 0)
                                (make-wasm-export "answer" :func 1)))))
    (let ((inst (make-module-instance mod '())))
      (wasmdown.test:assert-trap :call-stack-exhausted
        (call-export inst "spin" 0))
      ;; host survived: the depth counter is clean and a fresh
      ;; (shallow) guest call still executes.
      (expect (symbol-value 'wasmdown.interp::*call-depth*) 0)
      (expect (call-export inst "answer") '(42)))))

(wasmdown.test:test interp/call-mutual
  ;; is-even / is-odd by mutual recursion through a second defined func
  (let* ((types (vector (ft '(:i32) '(:i32))))
         (even-body
           (vector
            (i :local.get (list :local 0))
            (i :i32.eqz)
            (i :if (list :bt :i32
                         ;; else: is-odd(n-1)
                         :else-body
                         (vector (i :local.get (list :local 0))
                                 (i :i32.const (list :value 1))
                                 (i :i32.sub)
                                 (i :call (list :func 1))))
                (vector (i :i32.const (list :value 1))))
            (i :end)))
         (odd-body
           (vector
            (i :local.get (list :local 0))
            (i :i32.eqz)
            (i :if (list :bt :i32
                         ;; else: is-even(n-1)
                         :else-body
                         (vector (i :local.get (list :local 0))
                                 (i :i32.const (list :value 1))
                                 (i :i32.sub)
                                 (i :call (list :func 0))))
                (vector (i :i32.const (list :value 0))))
            (i :end)))
         (mod (mmod
               :types types
               :funcs (vector (fn 0 even-body) (fn 0 odd-body))
               :exports (vector (make-wasm-export "even" :func 0)))))
    (let ((inst (make-module-instance mod '())))
      (expect (call-export inst "even" 10) '(1))
      (expect (call-export inst "even" 7) '(0)))))

;;; ================================================================
;;; call_indirect: type ok / wrong type / null

(wasmdown.test:test interp/call-indirect
  (let* ((types (vector (ft '() '(:i32))          ; 0: ()->i32
                         (ft '(:i32) '(:i32))     ; 1: i32->i32
                         (ft '() '())))           ; 2: ()->()
         (one (fn 0 (vector (i :i32.const (list :value 1)) (i :end))))
         (two (fn 0 (vector (i :i32.const (list :value 2)) (i :end))))
         (ident (fn 1 (vector (i :local.get (list :local 0)) (i :end))))
         (call-ok (fn 0 (vector (i :i32.const (list :value 1))
                                 (i :call-indirect (list :type 0 :table 0))
                                 (i :end))))
         (call-bad (fn 1 (vector (i :local.get (list :local 0))
                                 (i :i32.const (list :value 1))
                                 (i :call-indirect (list :type 1 :table 0))
                                 (i :end))))
         (call-null (fn 0 (vector (i :i32.const (list :value 5))
                                   (i :call-indirect (list :type 0 :table 0))
                                   (i :end))))
         (mod (mmod
               :types types
               :funcs (vector one two ident call-ok call-bad call-null)
               :tables (vector (make-wasm-table :funcref (lim 4 4)))
               ;; table: [0]=one [1]=two [2]=ident [3]=null
               :elems (vector (make-wasm-elem
                               (list :active 0
                                     (vector (i :i32.const (list :value 0))
                                             (i :end)))
                               :funcref
                               (list (vector (i :ref.func (list :func 0))
                                              (i :end))
                                     (vector (i :ref.func (list :func 1))
                                              (i :end))
                                     (vector (i :ref.func (list :func 2))
                                              (i :end))
                                     (vector (i :ref.null (list :type :func))
                                              (i :end)))))
               :exports (vector (make-wasm-export "ok" :func 3)
                                (make-wasm-export "bad" :func 4)
                                (make-wasm-export "null" :func 5)))))
    (let ((inst (make-module-instance mod '())))
      ;; type ok: table[1] = two ()->i32 matches expected type 0
      (expect (call-export inst "ok") '(2))
      ;; wrong type: expected (i32)->(i32), table[1] is ()->i32
      (wasmdown.test:assert-trap :indirect-call-type-mismatch
        (call-export inst "bad" 5))
      ;; null: table[5] is out of the elem-initialized range... but
      ;; table has 4 slots initialized; index 5 is out of bounds.
      ;; The null slot is index 3.
      (let ((mod2 (mmod
                   :types types
                   :funcs (vector one two ident call-ok call-bad
                                  (fn 0 (vector (i :i32.const (list :value 3))
                                                (i :call-indirect
                                                   (list :type 0 :table 0))
                                                (i :end))))
                   :tables (vector (make-wasm-table :funcref (lim 4 4)))
                   :elems (vector (make-wasm-elem
                                   (list :active 0
                                         (vector (i :i32.const
                                                       (list :value 0))
                                                 (i :end)))
                                   :funcref
                                   (list (vector (i :ref.func (list :func 0))
                                                  (i :end))
                                         (vector (i :ref.func (list :func 1))
                                                  (i :end))
                                         (vector (i :ref.func (list :func 2))
                                                  (i :end))
                                         (vector (i :ref.null (list :type :func))
                                                  (i :end)))))
                   :exports (vector (make-wasm-export "n3" :func 5)))))
        (let ((inst2 (make-module-instance mod2 '())))
          (wasmdown.test:assert-trap :null-reference
            (call-export inst2 "n3")))))))

;;; ================================================================
;;; regressions: interp select + store operand order (fixed from
;;; official-suite triage; probes were lost, tests pin the fixes)

(wasmdown.test:test interp/select-regression
  ;; select pops c, b, a; returns a when c/=0 (official-suite fix)
  (expect
   (call-export
    (make-module-instance
     (mmod
      :types (vector (ft '(:i32 :i32 :i32) '(:i32)))
      :funcs (vector (fn 0 (vector (i :local.get (list :local 0))
                                   (i :local.get (list :local 1))
                                   (i :local.get (list :local 2))
                                   (i :select)
                                   (i :end))))
      :exports (vector (make-wasm-export "sel" :func 0)))
     '())
    "sel" 11 22 1)
   '(11))
  (expect
   (call-export
    (make-module-instance
     (mmod
      :types (vector (ft '(:i32 :i32 :i32) '(:i32)))
      :funcs (vector (fn 0 (vector (i :local.get (list :local 0))
                                   (i :local.get (list :local 1))
                                   (i :local.get (list :local 2))
                                   (i :select)
                                   (i :end))))
      :exports (vector (make-wasm-export "sel" :func 0)))
     '())
    "sel" 11 22 0)
   '(22)))

(wasmdown.test:test interp/store-order-regression
  ;; store pops value THEN address ([addr,val] with val on top)
  (expect
   (call-export
    (make-module-instance
     (mmod
      :types (vector (ft '() '(:i32)))
      :mems (vector (make-wasm-memory (lim 1)))
      :funcs (vector (fn 0 (vector (i :i32.const (list :value 8))
                                   (i :i32.const (list :value #xABCD))
                                   (i :i32.store (list :align 2 :offset 0))
                                   (i :i32.const (list :value 8))
                                   (i :i32.load (list :align 2 :offset 0))
                                   (i :end))))
      :exports (vector (make-wasm-export "st" :func 0)))
     '())
    "st")
   '(#xABCD)))
