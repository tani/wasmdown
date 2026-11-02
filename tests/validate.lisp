;;;; Unit tests for wasmdown.validate: module-level invariants and the
;;;; operand/control-stack algorithm with stack polymorphism.

(defpackage #:wasmdown.test.validate
  (:use #:cl #:wasmdown.types)
  (:nicknames #:wd.test.validate))

(in-package #:wasmdown.test.validate)

;;; -----------------------------------------------------------------
;;; Builders. Modules are assembled programmatically per src/types.lisp;
;;; instruction vectors hand-assembled. NOTE on IR shapes (CONTRACTS.md):
;;; block/loop bodies exclude the trailing :end; function bodies include
;;; it; :if bodies split via (:else-body . vector).

(defun i (op &optional imms body)
  (make-wasm-instr op imms (or body #()) 0))

(defun endi () (i :end))

(defun f (typeidx locals body)
  (make-wasm-func typeidx locals body 0))

(defun ft (params results)
  (make-wasm-func-type params results))

(defun mmod (&key (types #()) (imports #()) (funcs #()) (tables #())
              (mems #()) (globals #()) (exports #()) (start nil)
              (elems #()) (datas #()) (data-count nil))
  (make-wasm-module :types types :imports imports :funcs funcs
                    :tables tables :mems mems :globals globals
                    :exports exports :start start :elems elems
                    :datas datas :data-count data-count))

(defmacro ok (form)
  `(wasmdown.test:is (eq t ,form)))

;;; ================================================================
;;; Positive: module-level

(wasmdown.test:test validate/empty-module
  (ok (wasmdown.validate:validate-module (mmod))))

(wasmdown.test:test validate/all-numeric-ops
  ;; A function exercising a representative slice of every numeric
  ;; family: i32/i64/f32/f64 arithmetic, comparisons, conversions,
  ;; sign-extension and saturating truncation.
  (let* ((types (vector (ft '() '(:i32))))
         (body (vector
                (i :i32.const '(:value 1))
                (i :i32.const '(:value 2)) (i :i32.add)
                (i :i32.const '(:value 3)) (i :i32.sub)
                (i :i32.const '(:value 4)) (i :i32.mul)
                (i :i32.const '(:value 5)) (i :i32.and)
                (i :i32.const '(:value 6)) (i :i32.or)
                (i :i32.const '(:value 7)) (i :i32.xor)
                (i :i32.const '(:value 8)) (i :i32.shl)
                (i :i32.const '(:value 9)) (i :i32.shr_u)
                (i :i32.const '(:value 10)) (i :i32.rotl)
                (i :i32.const '(:value 11)) (i :i32.eq)
                (i :drop)
                (i :i64.const '(:value 1))
                (i :i64.const '(:value 2)) (i :i64.add)
                (i :i64.eqz) (i :drop)
                (i :f32.const '(:bits 0))
                (i :f32.const '(:bits 0)) (i :f32.add) (i :drop)
                (i :f64.const '(:bits 0))
                (i :f64.const '(:bits 0)) (i :f64.mul) (i :drop)
                (i :i32.const '(:value 3)) (i :i64.extend_i32_s)
                (i :i32.wrap_i64) (i :i64.extend_i32_u)
                (i :f32.convert_i64_s) (i :f64.promote_f32) (i :drop)
                (i :i32.const '(:value 1)) (i :i32.extend8_s) (i :drop)
                (i :f32.const '(:bits 0)) (i :i32.trunc_sat_f32_s) (i :drop)
                (i :i32.const '(:value 42))
                (i :end))))
    (ok (wasmdown.validate:validate-module
         (mmod :types types :funcs (vector (f 0 '() body)))))))

(wasmdown.test:test validate/blocks-loops-multivalue
  ;; block with typeidx blocktype returning two values; nested loop.
  (let* ((types (vector (ft '() '(:i32 :i64))
                         (ft '(:i32) '(:i32))))
         (block-body (vector
                      (i :i32.const '(:value 1))
                      (i :i64.const '(:value 2))))
         (loop-body (vector
                      (i :br-if (list :label 0))))
         (body (vector
                ;; multivalue block: [] -> [i32 i64]; results feed the
                ;; function's own results directly
                (i :block (list :bt (cons :typeidx 0)) block-body)
                (i :end))))
    (ok (wasmdown.validate:validate-module
         (mmod :types types :funcs (vector (f 0 '() body)))))))

(wasmdown.test:test validate/if-else-multivalue
  (let* ((types (vector (ft '() '(:i32 :i32))))
         (then (vector (i :i32.const '(:value 1))
                       (i :i32.const '(:value 2))))
         (else (vector (i :i32.const '(:value 3))
                       (i :i32.const '(:value 4))))
         (body (vector
                (i :i32.const '(:value 1))   ; condition
                (i :if (list :bt (cons :typeidx 0) :else-body else)
                    then)
                (i :end))))                 ; results = func results
    (ok (wasmdown.validate:validate-module
         (mmod :types types :funcs (vector (f 0 '() body)))))))

(wasmdown.test:test validate/br-table
  ;; br_table with three targets, all of arity 1 (i32), plus default.
  (let* ((types (vector (ft '() '(:i32))))
         (body (vector
                (i :block (list :bt :i32)
                    (vector
                     (i :block (list :bt :i32)
                         (vector
                          (i :block (list :bt :i32)
                              (vector
                               (i :i32.const '(:value 0))
                               (i :i32.const '(:value 1))
                               (i :br-table (list :targets
                                                   (vector 0 1 2)
                                                   :default 2)))))))) 
                (i :end))))
    (ok (wasmdown.validate:validate-module
         (mmod :types types :funcs (vector (f 0 '() body)))))))

(wasmdown.test:test validate/calls
  ;; call and call_indirect through a funcref table.
  (let* ((types (vector (ft '(:i32) '(:i32))
                         (ft '() '())))
         (callee (vector (i :local.get (list :local 0))
                          (i :end)))
         (caller (vector
                  (i :i32.const '(:value 1))
                  (i :call (list :func 1))
                  (i :i32.const '(:value 2))
                  (i :call-indirect (list :type 0 :table 0))
                  (i :drop)
                  (i :end)))
         (tables (vector (make-wasm-table :funcref
                                          (make-wasm-limits 1))))
         (funcs (vector (f 0 '() callee) (f 1 '() caller))))
    (ok (wasmdown.validate:validate-module
         (mmod :types types :funcs funcs :tables tables)))))

(wasmdown.test:test validate/memory-ops
  (let* ((types (vector (ft '() '(:i32))))
         (body (vector
                (i :i32.const '(:value 0))     ; [i32]
                (i :i32.load (list :align 2 :offset 0))     ; -> [i32]
                (i :i32.const '(:value 4))      ; [i32 i32]
                (i :i32.store (list :align 2 :offset 0))    ; -> []
                (i :i32.const '(:value 0))      ; [i32]
                (i :i32.load8_u (list :align 0 :offset 0))  ; [i32]
                (i :memory.size (list :memidx 0))           ; [i32 i32]
                (i :i32.add)                    ; [i32]
                (i :i32.const '(:value 1))      ; [i32 i32]
                (i :memory.grow (list :memidx 0)) ; [i32 i32]
                (i :i32.add)                    ; [i32]
                (i :i32.const '(:value 0))      ; [i32 i32]
                (i :i32.const '(:value 0))      ; [i32 i32 i32]
                (i :memory.copy (list :dst 0 :src 0)) ; -> [i32]
                (i :i32.const '(:value 0))      ; [i32 i32]
                (i :i32.const '(:value 0))      ; [i32 i32 i32]
                (i :i32.const '(:value 0))      ; [i32 i32 i32 i32]
                (i :memory.fill (list :memidx 0)) ; -> [i32]
                (i :i32.const '(:value 7))      ; [i32 i32]
                (i :end)))
         (mems (vector (make-wasm-memory (make-wasm-limits 1)))))
    (ok (wasmdown.validate:validate-module
         (mmod :types types :funcs (vector (f 0 '() body)) :mems mems)))))

(wasmdown.test:test validate/table-ops
  (let* ((types (vector (ft '() '(:i32))))
         (body (vector
                (i :i32.const '(:value 0))
                (i :ref.null (list :type :funcref))
                (i :table.set (list :table 0))
                (i :i32.const '(:value 0))
                (i :table.get (list :table 0))
                (i :drop)
                (i :table.size (list :table 0))
                (i :ref.null (list :type :funcref))
                (i :i32.const '(:value 1))
                (i :table.grow (list :table 0))
                (i :i32.add)
                (i :end)))
         (tables (vector (make-wasm-table :funcref (make-wasm-limits 1)))))
    (ok (wasmdown.validate:validate-module
         (mmod :types types :funcs (vector (f 0 '() body))
               :tables tables)))))

(wasmdown.test:test validate/elem-data-segments
  ;; active elem into a funcref table, active data into memory 0,
  ;; plus table.init/elem.drop/memory.init/data.drop in a function.
  (let* ((types (vector (ft '() '())))
         (refexpr (vector (i :ref.func (list :func 0)) (i :end)))
         (elems (vector (make-wasm-elem
                         (list :active 0
                               (vector (i :i32.const '(:value 0)) (i :end)))
                         :funcref
                         (list refexpr))))
         (datas (vector (make-wasm-data
                         (list :active 0
                               (vector (i :i32.const '(:value 0)) (i :end)))
                         #(1 2 3))))
         (body (vector
                (i :i32.const '(:value 0))
                (i :i32.const '(:value 0))
                (i :i32.const '(:value 0))
                (i :table.init (list :elem 0 :table 0))
                (i :elem.drop (list :elem 0))
                (i :i32.const '(:value 0))
                (i :i32.const '(:value 0))
                (i :i32.const '(:value 0))
                (i :memory.init (list :data 0 :memidx 0))
                (i :data.drop (list :data 0))
                (i :end)))
         (tables (vector (make-wasm-table :funcref (make-wasm-limits 1))))
         (mems (vector (make-wasm-memory (make-wasm-limits 1))))
         (funcs (vector (f 0 '() body)))
         (exports (vector (make-wasm-export "f" :func 0))))
    (ok (wasmdown.validate:validate-module
         (mmod :types types :funcs funcs :tables tables :mems mems
               :elems elems :datas datas :exports exports
               ;; data-count is required when memory.init/data.drop
               ;; appear in code (spec); one data segment exists.
               :data-count 1)))))

(wasmdown.test:test validate/globals-const-exprs
  ;; Immutable global from a const; mutable global initialized from a
  ;; previously defined global (legal: imported or previous).
  (let* ((types (vector (ft '() '())))
         (globals (vector (make-wasm-global :type :i32 :mutable nil
                                            :init (vector
                                                   (i :i32.const
                                                      '(:value 1))
                                                   (i :end)))
                          (make-wasm-global :type :i32 :mutable t
                                            :init (vector
                                                   (i :global.get
                                                      (list :global 0))
                                                   (i :end)))))
         (body (vector
                (i :global.get (list :global 1))
                (i :global.set (list :global 1))
                (i :end))))
    (ok (wasmdown.validate:validate-module
         (mmod :types types :globals globals
               :funcs (vector (f 0 '() body)))))))

;;; ================================================================
;;; Negative battery: every case must signal
;;; wasmdown:validation-error (never a decode/trap/SBCL type error).

(defmacro bad (name module-form)
  `(wasmdown.test:test ,name
     (wasmdown.test:assert-raises wasmdown:validation-error
       (wasmdown.validate:validate-module ,module-form))))

(defun t0 () (vector (ft '() '())))
(defun ti32 () (vector (ft '() '(:i32))))

;; (a) i32.add fed an i64 operand.
(bad validate/neg-i32-add-i64-operand
  (mmod :types (ti32)
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 1))
                                        (i :i64.const '(:value 2))
                                        (i :i32.add) (i :drop) (i :end))))))

;; (b) br arity mismatch: branch to the function frame (1 result) with
;; an empty stack.
(bad validate/neg-br-arity
  (mmod :types (ti32)
        :funcs (vector (f 0 '() (vector (i :br '(:label 0)) (i :end))))))

;; (b2) br to an inner block (1 result) from an empty stack.
(bad validate/neg-br-inner-arity
  (mmod :types (ti32)
        :funcs (vector (f 0 '() (vector
                                (i :block (list :bt :i32) #())
                                (i :drop) (i :i32.const '(:value 1))
                                (i :end))))))

;; (c) unknown indices.
(bad validate/neg-local-get-oob
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :local.get '(:local 0))
                                        (i :end))))))
(bad validate/neg-global-get-oob
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :global.get '(:global 0))
                                        (i :end))))))
(bad validate/neg-call-funcidx-oob
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :call '(:func 1))
                                        (i :end))))))
(bad validate/neg-call-indirect-bad-typeidx
  (mmod :types (t0)
        :tables (vector (make-wasm-table :funcref (make-wasm-limits 1)))
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 0))
                                        (i :call-indirect '(:type 5 :table 0))
                                        (i :end))))))
;; memory op with no memory declared.
(bad validate/neg-load-no-memory
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 0))
                                        (i :i32.load '(:align 2 :offset 0))
                                        (i :drop) (i :end))))))
;; table op with no table declared.
(bad validate/neg-table-get-no-table
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 0))
                                        (i :table.get '(:table 0))
                                        (i :drop) (i :end))))))

;; (d) select (typed) arity: type vector with two elements.
(bad validate/neg-select-arity
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 1))
                                        (i :i32.const '(:value 2))
                                        (i :i32.const '(:value 3))
                                        (i :select (list :types
                                                          (vector :i32 :i32)))
                                        (i :drop) (i :end))))))
;; (d2) select (typed) type mismatch: declared i32, operands i64.
(bad validate/neg-select-typed-type-mismatch
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :i64.const '(:value 1))
                                        (i :i64.const '(:value 2))
                                        (i :i32.const '(:value 3))
                                        (i :select (list :types (vector :i32)))
                                        (i :drop) (i :end))))))
;; (d3) select (untyped) with reference operands.
(bad validate/neg-select-untyped-ref
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :ref.null '(:type :funcref))
                                        (i :ref.null '(:type :funcref))
                                        (i :i32.const '(:value 1))
                                        (i :select)
                                        (i :drop) (i :end))))))
;; (d4) select (untyped) arity underflow.
(bad validate/neg-select-underflow
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 1))
                                        (i :select) (i :drop) (i :end))))))

;; (e) alignment larger than natural: i32.load with align exponent 3.
(bad validate/neg-align-too-large-i32-load
  (mmod :types (t0)
        :mems (vector (make-wasm-memory (make-wasm-limits 1)))
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 0))
                                        (i :i32.load '(:align 3 :offset 0))
                                        (i :drop) (i :end))))))
;; (e2) load8 with align > 0 (2^1 > 8/8 = 1 byte).
(bad validate/neg-align-too-large-load8
  (mmod :types (t0)
        :mems (vector (make-wasm-memory (make-wasm-limits 1)))
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 0))
                                        (i :i32.load8_u '(:align 1 :offset 0))
                                        (i :drop) (i :end))))))
;; (e3) memarg offset out of u32 range: address.wast/align.wast
;; "offset out of range" — the text grammar admits the integer but
;; validation must reject offsets above 2^32-1.
(bad validate/neg-memarg-offset-out-of-range
  (mmod :types (t0)
        :mems (vector (make-wasm-memory (make-wasm-limits 1)))
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 0))
                                        (i :i32.load
                                           (list :align 2
                                                 :offset 4294967296))
                                        (i :drop) (i :end))))))
;; (e3b) POSITIVE control: offset = 2^32-1 is the last valid u32.
(wasmdown.test:test validate/pos-memarg-offset-umax
  (ok (wasmdown.validate:validate-module
       (mmod :types (t0)
             :mems (vector (make-wasm-memory (make-wasm-limits 1)))
             :funcs (vector (f 0 '() (vector
                                      (i :i32.const '(:value 0))
                                      (i :i32.load
                                         (list :align 2 :offset 4294967295))
                                      (i :drop) (i :end))))))))

;; (f) data-count mismatch: section declares 2, data section has 1.
(bad validate/neg-datacount-mismatch
  (mmod :types (t0)
        :datas (vector (make-wasm-data '(:passive) #(1)))
        :data-count 2))
;; (f2) memory.init with no data count section (nil) though a passive
;; data segment exists — the index space was never declared.
(bad validate/neg-memory-init-no-datacount
  (mmod :types (t0)
        :mems (vector (make-wasm-memory (make-wasm-limits 1)))
        :datas (vector (make-wasm-data '(:passive) #(1)))
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 0))
                                        (i :i32.const '(:value 0))
                                        (i :i32.const '(:value 0))
                                        (i :memory.init '(:data 0 :memidx 0))
                                        (i :end))))))
;; (f3) memory.init dataidx out of range even with data-count present.
(bad validate/neg-memory-init-dataidx-oob
  (mmod :types (t0)
        :mems (vector (make-wasm-memory (make-wasm-limits 1)))
        :datas (vector (make-wasm-data '(:passive) #(1)))
        :data-count 1
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 0))
                                        (i :i32.const '(:value 0))
                                        (i :i32.const '(:value 0))
                                        (i :memory.init '(:data 3 :memidx 0))
                                        (i :end))))))

;; (g) duplicate export names.
(bad validate/neg-duplicate-exports
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :end))))
        :exports (vector (make-wasm-export "a" :func 0)
                         (make-wasm-export "a" :func 0))))

;; (h) start function with parameters.
(bad validate/neg-start-with-params
  (mmod :types (vector (ft '(:i32) '()))
        :funcs (vector (f 0 '() (vector (i :end))))
        :start 0))
;; (h2) start function with results.
(bad validate/neg-start-with-results
  (mmod :types (ti32)
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 1))
                                        (i :end))))
        :start 0))
;; (h3) start funcidx out of range.
(bad validate/neg-start-oob
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector (i :end))))
        :start 3))

;; (i) limits min > max: memory.
(bad validate/neg-memory-min-gt-max
  (mmod :types (t0)
        :mems (vector (make-wasm-memory (make-wasm-limits 2 1)))))
;; (i2) limits min > max: table.
(bad validate/neg-table-min-gt-max
  (mmod :types (t0)
        :tables (vector (make-wasm-table :funcref
                                         (make-wasm-limits 5 1)))))
;; (i3) memory min beyond 2^16 pages.
(bad validate/neg-memory-min-too-large
  (mmod :types (t0)
        :mems (vector (make-wasm-memory (make-wasm-limits 70000)))))

;; (j) call with wrong stack argument types.
(bad validate/neg-call-wrong-arg-types
  (mmod :types (vector (ft '() '()) (ft '(:i32) '()))
        :funcs (vector (f 1 '() (vector (i :i64.const '(:value 1))
                                        (i :call '(:func 0)) (i :end))))))

;; (k) if/else arm result mismatch (then: i32, else: i64).
(bad validate/neg-if-else-arm-mismatch
  (let ((then (vector (i :i32.const '(:value 1))))
        (else (vector (i :i64.const '(:value 1)))))
    (mmod :types (ti32)
          :funcs (vector (f 0 '() (vector
                                   (i :i32.const '(:value 1))
                                   (i :if (list :bt :i32 :else-body else) then)
                                   (i :end)))))))

;; (l) br_table target arity mismatch: default targets the function
;; frame (arity 0) while the target branches to a block of arity 1.
(bad validate/neg-br-table-target-arity
  (mmod :types (t0)
        :funcs (vector (f 0 '() (vector
                                 (i :block (list :bt :i32)
                                     (vector (i :i32.const '(:value 0))
                                             (i :i32.const '(:value 0))
                                             (i :br-table (list :targets #(0)
                                                                :default 1))))
                                 (i :end))))))

;; (m) global.set on an immutable global.
(bad validate/neg-global-set-immutable
  (mmod :types (t0)
        :globals (vector (make-wasm-global :type :i32 :mutable nil
                                           :init (vector (i :i32.const '(:value 1))
                                                         (i :end))))
        :funcs (vector (f 0 '() (vector (i :i32.const '(:value 2))
                                        (i :global.set '(:global 0))
                                        (i :end))))))

;; (n) global init referencing a later-defined global.
(bad validate/neg-global-init-forward-reference
  (mmod :types (t0)
        :globals (vector
                  (make-wasm-global :type :i32 :mutable nil
                                    :init (vector (i :i32.const '(:value 1))
                                                  (i :end)))
                  (make-wasm-global :type :i32 :mutable nil
                                    :init (vector (i :global.get '(:global 2))
                                                  (i :end))))))

;; (o) element segment type mismatch with the table's type.
(bad validate/neg-elem-type-mismatch
  (mmod :types (t0)
        :tables (vector (make-wasm-table :funcref (make-wasm-limits 1)))
        :elems (vector (make-wasm-elem
                        (list :active 0
                              (vector (i :i32.const '(:value 0)) (i :end)))
                        :externref
                        (list (vector (i :ref.null '(:type :externref))
                                      (i :end)))))))

;; (p) data segment memidx out of range.
(bad validate/neg-data-memidx-oob
  (mmod :types (t0)
        :datas (vector (make-wasm-data
                        (list :active 1
                              (vector (i :i32.const '(:value 0)) (i :end)))
                        #(1)))))

;; (q) POSITIVE control: unreachable's stack polymorphism must not be
;; over-strict. unreachable ; i32.add ; end in a ()->(i32) function is
;; valid per the spec algorithm.
(wasmdown.test:test validate/pos-unreachable-polymorphism
  (ok (wasmdown.validate:validate-module
       (mmod :types (ti32)
             :funcs (vector (f 0 '() (vector (i :unreachable)
                                             (i :i32.add)
                                             (i :end)))))))
  ;; and in a ()->() function the same sequence is invalid (the add's
  ;; i32 leaks past the frame boundary) — polymorphism is not a license
  ;; to leave extras.
  (wasmdown.test:assert-raises wasmdown:validation-error
    (wasmdown.validate:validate-module
     (mmod :types (t0)
           :funcs (vector (f 0 '() (vector (i :unreachable)
                                           (i :i32.add)
                                           (i :end))))))))

;; (q2) POSITIVE control: unreachable feeding select (bot operands).
(wasmdown.test:test validate/pos-unreachable-select
  (ok (wasmdown.validate:validate-module
       (mmod :types (t0)
             :funcs (vector (f 0 '() (vector (i :unreachable)
                                             (i :select)
                                             (i :drop)
                                             (i :end))))))))

;; (q3) POSITIVE control: unreachable then br (no operands needed).
(wasmdown.test:test validate/pos-unreachable-br
  (ok (wasmdown.validate:validate-module
       (mmod :types (t0)
             :funcs (vector (f 0 '() (vector (i :unreachable)
                                             (i :br '(:label 0))
                                             (i :end))))))))

;; (r) if WITHOUT else but with a non-empty result type: spec types
;; the missing else as producing nothing, so a resultful if requires
;; the else arm (if.wast type-no-else-vs-num/-nums assert_invalid).
(bad validate/neg-if-no-else-with-results
  (mmod :types (ti32)
        :funcs (vector (f 0 '() (vector
                                 (i :i32.const '(:value 1))
                                 (i :if (list :bt :i32)
                                   (vector (i :i32.const '(:value 1))))
                                 (i :end))))))

;; (r2) POSITIVE control: if without else and EMPTY results is fine.
(wasmdown.test:test validate/pos-if-no-else-void
  (ok (wasmdown.validate:validate-module
       (mmod :types (t0)
             :funcs (vector (f 0 '() (vector
                                      (i :i32.const '(:value 1))
                                      (i :if nil
                                        (vector (i :nop)))
                                      (i :end))))))))

;;; ================================================================
;;; Threads proposal (opt-in): shared memory + atomic instructions.
;;; The module builders above are reused; the opt-in is the dynamic
;;; variable WASMDOWN:*THREADS*, bound here so the default lane keeps
;;; its rejection.

(defun shared-mem (&optional (min 1) (max 1))
  (make-wasm-memory (make-wasm-limits min max nil t)))

(wasmdown.test:test validate/threads-shared-memory-positive
  (let ((wasmdown:*threads* t))
    (ok (wasmdown.validate:validate-module
         (mmod :types (vector (ft '() '(:i32)) (ft '() '()))
               :mems (vector (shared-mem))
               :funcs (vector
                       (f 0 '() (vector
                                 (i :i32.const (list :value 0))
                                 (i :i32.atomic.load (list :align 2 :offset 0))
                                 (endi)))))))
    ;; atomics are legal on UNSHARED memory too (only wait requires
    ;; shared, and that is a runtime trap)
    (ok (wasmdown.validate:validate-module
         (mmod :types (ti32)
               :mems (vector (make-wasm-memory (make-wasm-limits 1 1)))
               :funcs (vector
                       (f 0 '() (vector
                                 (i :i32.const (list :value 0))
                                 (i :i32.atomic.load (list :align 2 :offset 0))
                                 (endi)))))))))

(wasmdown.test:test validate/threads-wait-notify-fence
  (let ((wasmdown:*threads* t))
    (ok (wasmdown.validate:validate-module
         (mmod :types (vector (ft '() '(:i32)) (ft '() '()))
               :mems (vector (shared-mem))
               :funcs (vector
                       (f 0 '() (vector
                                 (i :i32.const (list :value 0))
                                 (i :i32.const (list :value 0))
                                 (i :i64.const (list :value 0))
                                 (i :memory.atomic.wait32
                                    (list :align 2 :offset 0))
                                 (endi)))
                       (f 0 '() (vector
                                 (i :i32.const (list :value 0))
                                 (i :i32.const (list :value 0))
                                 (i :memory.atomic.notify
                                    (list :align 2 :offset 0))
                                 (endi)))
                       ;; atomic.fence needs no memory at all
                       (f 1 '() (vector (i :atomic.fence) (endi)))))))))

(wasmdown.test:test validate/threads-shared-requires-maximum
  (let ((wasmdown:*threads* t))
    (wasmdown.test:assert-raises wasmdown:validation-error
      (wasmdown.validate:validate-module
       (mmod :mems (vector
                    (make-wasm-memory
                     (wasmdown.types::%make-wasm-limits* 1 nil nil nil t))))))))

(wasmdown.test:test validate/threads-atomic-alignment-natural
  (let ((wasmdown:*threads* t))
    ;; align=1 (2 bytes) on a 4-byte atomic is invalid
    (wasmdown.test:assert-raises wasmdown:validation-error
      (wasmdown.validate:validate-module
       (mmod :types (ti32)
             :mems (vector (shared-mem))
             :funcs (vector
                     (f 0 '() (vector
                               (i :i32.const (list :value 0))
                               (i :i32.atomic.load (list :align 1 :offset 0))
                               (endi)))))))
    ;; 8-byte atomic rmw with the natural align=3 validates
    (ok (wasmdown.validate:validate-module
         (mmod :types (vector (ft '() '(:i64)))
               :mems (vector (shared-mem))
               :funcs (vector
                       (f 0 '() (vector
                                 (i :i32.const (list :value 0))
                                 (i :i64.const (list :value 1))
                                 (i :i64.atomic.rmw.add
                                    (list :align 3 :offset 0))
                                 (endi)))))))))

(wasmdown.test:test validate/threads-atomic-unknown-memory
  (let ((wasmdown:*threads* t))
    (wasmdown.test:assert-raises wasmdown:validation-error
      (wasmdown.validate:validate-module
       (mmod :types (ti32)
             :funcs (vector
                     (f 0 '() (vector
                               (i :i32.const (list :value 0))
                               (i :i32.atomic.load (list :align 2 :offset 0))
                               (endi)))))))))

(wasmdown.test:test validate/threads-gate-off-rejects-atomics
  (let ((wasmdown:*threads* nil))
    (wasmdown.test:assert-raises wasmdown:validation-error
      (wasmdown.validate:validate-module
       (mmod :types (ti32)
             :mems (vector (make-wasm-memory (make-wasm-limits 1 1)))
             :funcs (vector
                     (f 0 '() (vector
                               (i :i32.const (list :value 0))
                               (i :i32.atomic.load (list :align 2 :offset 0))
                               (endi)))))))))

;; Typing is enforced: i64.atomic.store with an i32 value operand.
(wasmdown.test:test validate/threads-atomic-type-mismatch
  (let ((wasmdown:*threads* t))
    (wasmdown.test:assert-raises wasmdown:validation-error
      (wasmdown.validate:validate-module
       (mmod :types (t0)
             :mems (vector (shared-mem))
             :funcs (vector
                     (f 0 '() (vector
                               (i :i32.const (list :value 0))
                               (i :i32.const (list :value 0))
                               (i :i64.atomic.store
                                  (list :align 3 :offset 0))
                               (endi)))))))))
