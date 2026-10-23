;;;; Static validation engine (PLAN.md §5.2): module-level invariants and
;;;; the spec's operand/control-stack algorithm with stack polymorphism.
;;;; Signals wasmdown:validation-error; never decode-error.

(in-package #:wasmdown.validate)

(defparameter *max-memory-pages* 65536
  "Spec limit for 32-bit memories (2^16 pages).")
(defparameter *max-table-size* #xffffffff
  "Maximum table element count for 32-bit tables.")

(defun verr (format-control &rest args)
  "Signal wasmdown:validation-error with a formatted message."
  (error 'wasmdown:validation-error
         :message (apply #'format nil format-control args)))

;;; ---------------------------------------------------------------
;;; Limits and type well-formedness (spec: valid limits/tabletype/
;;; memtype/reftype/valtype).

(defun %limits-has-max (limits)
  "Whether the limits record carries an explicit max (wasm-limits-max
being non-NIL is not sufficient: a max of 0 is explicit and valid;
the types contract exports wasm-limits-has-max-p)."
  (wasm-limits-has-max-p limits))

(defun valid-limits-p (limits &optional (max-bound *max-memory-pages*))
  (and (integerp (wasm-limits-min limits))
       (>= (wasm-limits-min limits) 0)
       (<= (wasm-limits-min limits) max-bound)
       (or (not (%limits-has-max limits))
           (and (integerp (wasm-limits-max limits))
                (>= (wasm-limits-max limits) 0)
                (<= (wasm-limits-max limits) max-bound)
                (<= (wasm-limits-min limits)
                    (wasm-limits-max limits))))))

(defun check-limits (limits what &optional (max-bound *max-memory-pages*))
  (unless (valid-limits-p limits max-bound)
    (verr "invalid limits for ~a: min ~a, max ~a"
          what (wasm-limits-min limits)
          (if (%limits-has-max limits)
              (wasm-limits-max limits)
              "none")))
  t)

(defun numtype-p (t_)
  (member t_ '(:i32 :i64 :f32 :f64)))

(defun reftype-p (t_)
  (member t_ '(:funcref :externref)))

(defun valtype-p (t_)
  (or (numtype-p t_) (reftype-p t_)))

(defun ref-null-p (t_)
  "Reference type with explicit nullability, as in decodable blocktypes."
  (declare (ignore t_))
  nil)

(defun valid-blocktype-params-results (module bt)
  "Resolve a blocktype to (params results) lists, or signal."
  (cond
    ((null bt) (values nil nil))
    ((valtype-p bt) (values nil (list bt)))
    ((and (consp bt) (eq (car bt) :typeidx))
     (let ((ft (func-type-at module (cdr bt) "blocktype")))
       (values (wasm-func-type-params ft) (wasm-func-type-results ft))))
    (t (verr "invalid blocktype ~a" bt))))

(defun func-type-at (module typeidx what)
  "Type at TYPEIDX, or validation-error."
  (let ((types (wasm-module-types module)))
    (unless (and (integerp typeidx) (>= typeidx 0) (< typeidx (length types)))
      (verr "~a: type index ~a out of range (module has ~a type~:p)"
            what typeidx (length types)))
    (svref types typeidx)))

(defun func-type-at-soft (module typeidx)
  "Type at TYPEIDX or NIL."
  (let ((types (wasm-module-types module)))
    (and (integerp typeidx) (>= typeidx 0) (< typeidx (length types))
         (svref types typeidx))))

;;; ---------------------------------------------------------------
;;; Module context: closed index spaces (imports first, then
;;; definitions), per spec §valid/modules.

(defstruct vctx
  module
  (num-imported-funcs 0)
  (func-typeidxs #())          ; vector: absolute funcidx -> typeidx
  (tables #())                 ; vector of wasm-table (imports first)
  (mems #())                   ; vector of wasm-memory (imports first)
  (globals #())                ; vector of (type mutable-p) (imports first)
  (imported-global-count 0)
  (elems #())                  ; vector of elem reftypes
  (data-count 0)
  (declared-refs nil))         ; list of funcidx declared via elem/export

(defun imported-kind-count (module kind)
  (count kind (wasm-module-imports module) :key #'wasm-import-kind))

(defun build-vctx (module)
  "Pre-pass constructing the validation context (spec: the module rule's
pre-computable types; funcs are mutually recursive so bodies validate
afterwards)."
  (let* ((imports (coerce (wasm-module-imports module) 'list))
         (imp-funcs (count :func imports :key #'wasm-import-kind))
         (func-typeidxs
          (coerce (append
                   (mapcar #'wasm-import-type
                           (remove :func imports :key #'wasm-import-kind
                                   :test-not #'eql))
                   (map 'list #'wasm-func-typeidx (wasm-module-funcs module)))
                  'vector))
         (tables (coerce (append
                           (mapcar #'wasm-import-type
                                   (remove :table imports :key
                                           #'wasm-import-kind :test-not #'eql))
                           (map 'list #'identity (wasm-module-tables module)))
                          'vector))
         (mems (coerce (append
                         (mapcar #'wasm-import-type
                                 (remove :memory imports :key
                                         #'wasm-import-kind :test-not #'eql))
                         (map 'list #'identity (wasm-module-mems module)))
                        'vector))
         (globals (coerce (append
                           (mapcar (lambda (i)
                                     (let ((gt (wasm-import-type i)))
                                       (list (car gt) (cdr gt))))
                                   (remove :global imports :key
                                           #'wasm-import-kind :test-not #'eql))
                           (map 'list (lambda (g)
                                        (list (wasm-global-type g)
                                              (wasm-global-mutable g)))
                                (wasm-module-globals module)))
                          'vector)))
    (make-vctx :module module
               :num-imported-funcs imp-funcs
               :func-typeidxs func-typeidxs
               :tables tables
               :mems mems
               :globals globals
               :imported-global-count
               (count :global imports :key #'wasm-import-kind)
               :elems (wasm-module-elems module)
               :data-count (length (wasm-module-datas module))
               :declared-refs (%declared-func-refs module))))

(defun %declared-func-refs (module)
  "The funcidx set legal for ref.func (spec: funcidx(globals mems tables
  elems exports)): every function referenced by an element segment or by
  any export."
  (let ((refs (make-hash-table)))
    (loop for elem across (wasm-module-elems module)
          do (loop for expr in (wasm-elem-inits elem)
                   do (loop for instr across expr
                            when (eq (wasm-instr-op instr) :ref.func)
                              do (setf (gethash (getf (wasm-instr-imms instr)
                                                      :func)
                                                refs) t))))
    (loop for exp across (wasm-module-exports module)
          when (and (eq (wasm-export-kind exp) :func)
                    (integerp (wasm-export-index exp)))
            do (setf (gethash (wasm-export-index exp) refs) t))
    refs))

(defun vctx-func-type (ctx funcidx what)
  "Function type for absolute FUNCIDX (imports first), or error."
  (let ((v (vctx-func-typeidxs ctx)))
    (unless (and (integerp funcidx) (>= funcidx 0) (< funcidx (length v)))
      (verr "~a: function index ~a out of range (module has ~a func~:p)"
            what funcidx (length v)))
    (func-type-at (vctx-module ctx) (svref v funcidx)
                  (format nil "~a (function ~a)" what funcidx))))

(defun vctx-table-at (ctx tableidx what)
  (let ((v (vctx-tables ctx)))
    (unless (and (integerp tableidx) (>= tableidx 0) (< tableidx (length v)))
      (verr "~a: table index ~a out of range (module has ~a table~:p)"
            what tableidx (length v)))
    (svref v tableidx)))

(defun vctx-mem-at (ctx memidx what)
  (let ((v (vctx-mems ctx)))
    (unless (and (integerp memidx) (>= memidx 0) (< memidx (length v)))
      (verr "~a: memory index ~a out of range (module has ~a memor~:p)"
            what memidx (length v)))
    (svref v memidx)))

(defun vctx-global-at (ctx globalidx what)
  (let ((v (vctx-globals ctx)))
    (unless (and (integerp globalidx) (>= globalidx 0) (< globalidx (length v)))
      (verr "~a: global index ~a out of range (module has ~a global~:p)"
            what globalidx (length v)))
    (svref v globalidx)))

(defun vctx-elem-type-at (ctx elemidx what)
  (let ((v (vctx-elems ctx)))
    (unless (and (integerp elemidx) (>= elemidx 0) (< elemidx (length v)))
      (verr "~a: element segment index ~a out of range (module has ~a)"
            what elemidx (length v)))
    (wasm-elem-type (svref v elemidx))))

(defun vctx-data-in-range-p (ctx dataidx)
  (and (integerp dataidx) (>= dataidx 0)
       (< dataidx (vctx-data-count ctx))))

;;; ---------------------------------------------------------------
;;; Constant expressions (spec: valid/instructions.html#constant).

(defun const-expr-type (ctx expr &key (global-limit nil)
                                    (what "constant expression"))
  "Validate the constant expression EXPR (vector of instr, ending :end)
and return its single result type. GLOBAL-LIMIT, when non-nil, is the
exclusive upper bound of global indices referenceable via global.get
\(spec: imported-or-previously-defined rule for global inits)."
  (let ((stack '()))
    (flet ((push1 (t_) (push t_ stack))
           (pop1 ()
             (unless stack (verr "~a: operand stack underflow" what))
             (pop stack)))
      (loop for instr across expr
            for op = (wasm-instr-op instr)
            for imms = (wasm-instr-imms instr)
            do (cond
                 ((eq op :end))
                 ((eq op :i32.const) (push1 :i32))
                 ((eq op :i64.const) (push1 :i64))
                 ((eq op :f32.const) (push1 :f32))
                 ((eq op :f64.const) (push1 :f64))
                 ((eq op :ref.null)
                  (let ((ht (getf imms :type)))
                    (unless (reftype-p ht)
                      (verr "~a: ref.null of invalid heap type ~a"
                            what ht))
                    (push1 ht)))
                 ((eq op :ref.func)
                  (let ((x (getf imms :func))
                        (nf (length (vctx-func-typeidxs ctx))))
                    (unless (and (integerp x) (>= x 0) (< x nf))
                      (verr "~a: ref.func index ~a out of range (module has ~a func~:p)"
                            what x nf))
                    (unless (gethash x (vctx-declared-refs ctx))
                      (verr "~a: ref.func ~a is not declared by an element segment or export"
                            what x))
                    (push1 :funcref)))
                 ((eq op :global.get)
                  (let* ((x (getf imms :global))
                         (g (and (integerp x) (>= x 0)
                                 (< x (length (vctx-globals ctx)))
                                 (svref (vctx-globals ctx) x))))
                    (unless g
                      (verr "~a: global.get index ~a out of range (module has ~a global~:p)"
                            what x (length (vctx-globals ctx))))
                    (when global-limit
                      (unless (< x global-limit)
                        (verr "~a: global.get ~a must reference an imported or previously defined global"
                              what x)))
                    (push1 (first g))))
                ;; Extended-const expressions (Core 3.0): i32/i64
                ;; add/sub/mul over constant operands are constant.
                ;; Both operands must carry the op's type.
                ((member op '(:i32.add :i32.sub :i32.mul))
                 (let ((b (pop1)) (a (pop1)))
                   (unless (and (eq a :i32) (eq b :i32))
                     (verr "~a: ~a operand type mismatch (~a, ~a)"
                           what op a b))
                   (push1 :i32)))
                ((member op '(:i64.add :i64.sub :i64.mul))
                 (let ((b (pop1)) (a (pop1)))
                   (unless (and (eq a :i64) (eq b :i64))
                     (verr "~a: ~a operand type mismatch (~a, ~a)"
                           what op a b))
                   (push1 :i64)))
                (t (verr "~a: instruction ~a is not constant" what op))))
      (unless (= (length stack) 1)
        (verr "~a: expression must produce exactly one value (produces ~a)"
              what (length stack)))
      (first stack))))

;;; ---------------------------------------------------------------
;;; Operand/control-stack algorithm (spec appendix "Validation
;;; Algorithm"), restricted to this slice's types: no Bot duplication
;;; and no locals-init tracking (every local is defaultable here).

(defstruct ctrl-frame
  opcode                       ; :block :loop :if :else :func
  start-types
  end-types
  val-height
  unreachable)

(defstruct fv
  "Function-validation state."
  ctx
  locals                       ; vector of valtypes
  vals                         ; list-as-stack of valtypes (top = car)
  ctrls                        ; list-as-stack of ctrl-frame (top = car)
  (offset 0))

(declaim (inline %cur-frame))
(defun %cur-frame (fv)
  (first (fv-ctrls fv)))

(defun %push-val (fv t_)
  (push t_ (fv-vals fv)))

(defun %pop-val (fv)
  (let ((frame (%cur-frame fv)))
    (when (and (= (length (fv-vals fv)) (ctrl-frame-val-height frame))
               (not (ctrl-frame-unreachable frame)))
      (verr "operand stack underflow at offset ~a" (fv-offset fv)))
    (if (= (length (fv-vals fv)) (ctrl-frame-val-height frame))
        :bot                            ; polymorphic bottom
        (pop (fv-vals fv)))))

(defun %pop-val-expect (fv expect)
  (let ((actual (%pop-val fv)))
    (unless (or (eq actual :bot) (equal actual expect))
      (verr "type mismatch at offset ~a: expected ~a, got ~a"
            (fv-offset fv) expect actual))
    actual))

(defun %pop-ref (fv)
  "Pop any reference type (:bot passes as :funcref placeholder)."
  (let ((actual (%pop-val fv)))
    (cond ((eq actual :bot) :funcref)
          ((reftype-p actual) actual)
          (t (verr "type mismatch at offset ~a: expected a reference type, got ~a"
                    (fv-offset fv) actual)))))

(defun %push-vals (fv types)
  (dolist (t_ types) (%push-val fv t_)))

(defun %pop-vals (fv types)
  "Pop TYPES (left-to-right list); errors mention the arity position."
  (loop for t_ in (reverse types)
        do (%pop-val-expect fv t_)))

(defun %push-ctrl (fv opcode in out &key (seed-stack t))
  (push (make-ctrl-frame :opcode opcode
                         :start-types in :end-types out
                         :val-height (length (fv-vals fv))
                         :unreachable nil)
        (fv-ctrls fv))
  (when seed-stack (%push-vals fv in)))

(defun %pop-ctrl (fv)
  (unless (fv-ctrls fv)
    (verr "control stack underflow at offset ~a" (fv-offset fv)))
  (let ((frame (%cur-frame fv)))
    (%pop-vals fv (ctrl-frame-end-types frame))
    (unless (= (length (fv-vals fv)) (ctrl-frame-val-height frame))
      (verr "values remaining on stack at end of block (offset ~a): ~s extra"
            (fv-offset fv)
            (- (length (fv-vals fv)) (ctrl-frame-val-height frame))))
    (pop (fv-ctrls fv))
    frame))

(defun %label-types (frame)
  (if (eq (ctrl-frame-opcode frame) :loop)
      (ctrl-frame-start-types frame)
      (ctrl-frame-end-types frame)))

(defun %ctrl-label (fv labelidx what)
  (let ((ctrls (fv-ctrls fv)))
    (unless (and (integerp labelidx) (>= labelidx 0) (< labelidx (length ctrls)))
      (verr "~a: label index ~a out of range (depth ~a)"
            what labelidx (length ctrls)))
    (nth labelidx ctrls)))

(defun %unreachable (fv)
  ;; (fv-vals fv) is a push-down list (element 0 = stack top); the
  ;; frame's base is the OLDEST vh values, so keep the LAST vh entries.
  (let ((vh (ctrl-frame-val-height (%cur-frame fv))))
    (setf (fv-vals fv) (last (fv-vals fv) vh))
    (setf (ctrl-frame-unreachable (%cur-frame fv)) t)))


(defun %types= (a b)
  "Result-type list equality with :bot wildcards per element."
  (and (= (length a) (length b))
       (every (lambda (x y) (or (eq x :bot) (eq y :bot) (equal x y)))
              a b)))

;;; ---------------------------------------------------------------
;;; Static type signatures (params -> results) for every simple opcode
;;; the decoder can produce (spec: valid/instructions.html numeric,
;;; memory and table rules; control/variable ops are special-cased).

(defparameter *op-sig-table*
  '(
    ;; i32 ops
    (:i32.eqz (:i32) (:i32))
    (:i32.eq (:i32 :i32) (:i32)) (:i32.ne (:i32 :i32) (:i32))
    (:i32.lt_s (:i32 :i32) (:i32)) (:i32.lt_u (:i32 :i32) (:i32))
    (:i32.gt_s (:i32 :i32) (:i32)) (:i32.gt_u (:i32 :i32) (:i32))
    (:i32.le_s (:i32 :i32) (:i32)) (:i32.le_u (:i32 :i32) (:i32))
    (:i32.ge_s (:i32 :i32) (:i32)) (:i32.ge_u (:i32 :i32) (:i32))
    (:i32.clz (:i32) (:i32)) (:i32.ctz (:i32) (:i32))
    (:i32.popcnt (:i32) (:i32))
    (:i32.add (:i32 :i32) (:i32)) (:i32.sub (:i32 :i32) (:i32))
    (:i32.mul (:i32 :i32) (:i32))
    (:i32.div_s (:i32 :i32) (:i32)) (:i32.div_u (:i32 :i32) (:i32))
    (:i32.rem_s (:i32 :i32) (:i32)) (:i32.rem_u (:i32 :i32) (:i32))
    (:i32.and (:i32 :i32) (:i32)) (:i32.or (:i32 :i32) (:i32))
    (:i32.xor (:i32 :i32) (:i32))
    (:i32.shl (:i32 :i32) (:i32)) (:i32.shr_s (:i32 :i32) (:i32))
    (:i32.shr_u (:i32 :i32) (:i32))
    (:i32.rotl (:i32 :i32) (:i32)) (:i32.rotr (:i32 :i32) (:i32))
    ;; i64 ops
    (:i64.eqz (:i64) (:i32))
    (:i64.eq (:i64 :i64) (:i32)) (:i64.ne (:i64 :i64) (:i32))
    (:i64.lt_s (:i64 :i64) (:i32)) (:i64.lt_u (:i64 :i64) (:i32))
    (:i64.gt_s (:i64 :i64) (:i32)) (:i64.gt_u (:i64 :i64) (:i32))
    (:i64.le_s (:i64 :i64) (:i32)) (:i64.le_u (:i64 :i64) (:i32))
    (:i64.ge_s (:i64 :i64) (:i32)) (:i64.ge_u (:i64 :i64) (:i32))
    (:i64.clz (:i64) (:i64)) (:i64.ctz (:i64) (:i64))
    (:i64.popcnt (:i64) (:i64))
    (:i64.add (:i64 :i64) (:i64)) (:i64.sub (:i64 :i64) (:i64))
    (:i64.mul (:i64 :i64) (:i64))
    (:i64.div_s (:i64 :i64) (:i64)) (:i64.div_u (:i64 :i64) (:i64))
    (:i64.rem_s (:i64 :i64) (:i64)) (:i64.rem_u (:i64 :i64) (:i64))
    (:i64.and (:i64 :i64) (:i64)) (:i64.or (:i64 :i64) (:i64))
    (:i64.xor (:i64 :i64) (:i64))
    (:i64.shl (:i64 :i64) (:i64)) (:i64.shr_s (:i64 :i64) (:i64))
    (:i64.shr_u (:i64 :i64) (:i64))
    (:i64.rotl (:i64 :i64) (:i64)) (:i64.rotr (:i64 :i64) (:i64))
    ;; f32 ops
    (:f32.abs (:f32) (:f32)) (:f32.neg (:f32) (:f32))
    (:f32.ceil (:f32) (:f32)) (:f32.floor (:f32) (:f32))
    (:f32.trunc (:f32) (:f32)) (:f32.nearest (:f32) (:f32))
    (:f32.sqrt (:f32) (:f32))
    (:f32.add (:f32 :f32) (:f32)) (:f32.sub (:f32 :f32) (:f32))
    (:f32.mul (:f32 :f32) (:f32)) (:f32.div (:f32 :f32) (:f32))
    (:f32.min (:f32 :f32) (:f32)) (:f32.max (:f32 :f32) (:f32))
    (:f32.copysign (:f32 :f32) (:f32))
    (:f32.eq (:f32 :f32) (:i32)) (:f32.ne (:f32 :f32) (:i32))
    (:f32.lt (:f32 :f32) (:i32)) (:f32.gt (:f32 :f32) (:i32))
    (:f32.le (:f32 :f32) (:i32)) (:f32.ge (:f32 :f32) (:i32))
    ;; f64 ops
    (:f64.abs (:f64) (:f64)) (:f64.neg (:f64) (:f64))
    (:f64.ceil (:f64) (:f64)) (:f64.floor (:f64) (:f64))
    (:f64.trunc (:f64) (:f64)) (:f64.nearest (:f64) (:f64))
    (:f64.sqrt (:f64) (:f64))
    (:f64.add (:f64 :f64) (:f64)) (:f64.sub (:f64 :f64) (:f64))
    (:f64.mul (:f64 :f64) (:f64)) (:f64.div (:f64 :f64) (:f64))
    (:f64.min (:f64 :f64) (:f64)) (:f64.max (:f64 :f64) (:f64))
    (:f64.copysign (:f64 :f64) (:f64))
    (:f64.eq (:f64 :f64) (:i32)) (:f64.ne (:f64 :f64) (:i32))
    (:f64.lt (:f64 :f64) (:i32)) (:f64.gt (:f64 :f64) (:i32))
    (:f64.le (:f64 :f64) (:i32)) (:f64.ge (:f64 :f64) (:i32))
    ;; conversions
    (:i32.wrap_i64 (:i64) (:i32))
    (:i32.trunc_f32_s (:f32) (:i32)) (:i32.trunc_f32_u (:f32) (:i32))
    (:i32.trunc_f64_s (:f64) (:i32)) (:i32.trunc_f64_u (:f64) (:i32))
    (:i64.extend_i32_s (:i32) (:i64)) (:i64.extend_i32_u (:i32) (:i64))
    (:i64.trunc_f32_s (:f32) (:i64)) (:i64.trunc_f32_u (:f32) (:i64))
    (:i64.trunc_f64_s (:f64) (:i64)) (:i64.trunc_f64_u (:f64) (:i64))
    (:f32.convert_i32_s (:i32) (:f32)) (:f32.convert_i32_u (:i32) (:f32))
    (:f32.convert_i64_s (:i64) (:f32)) (:f32.convert_i64_u (:i64) (:f32))
    (:f32.demote_f64 (:f64) (:f32))
    (:f64.convert_i32_s (:i32) (:f64)) (:f64.convert_i32_u (:i32) (:f64))
    (:f64.convert_i64_s (:i64) (:f64)) (:f64.convert_i64_u (:i64) (:f64))
    (:f64.promote_f32 (:f32) (:f64))
    (:i32.reinterpret_f32 (:f32) (:i32))
    (:i64.reinterpret_f64 (:f64) (:i64))
    (:f32.reinterpret_i32 (:i32) (:f32))
    (:f64.reinterpret_i64 (:i64) (:f64))
    (:i32.extend8_s (:i32) (:i32)) (:i32.extend16_s (:i32) (:i32))
    (:i64.extend8_s (:i64) (:i64)) (:i64.extend16_s (:i64) (:i64))
    (:i64.extend32_s (:i64) (:i64))
    ;; saturating truncations
    (:i32.trunc_sat_f32_s (:f32) (:i32)) (:i32.trunc_sat_f32_u (:f32) (:i32))
    (:i32.trunc_sat_f64_s (:f64) (:i32)) (:i32.trunc_sat_f64_u (:f64) (:i32))
    (:i64.trunc_sat_f32_s (:f32) (:i64)) (:i64.trunc_sat_f32_u (:f32) (:i64))
    (:i64.trunc_sat_f64_s (:f64) (:i64))
    (:i64.trunc_sat_f64_u (:f64) (:i64))))

(defparameter *op-sigs* (make-hash-table :test 'eq))
(dolist (entry *op-sig-table*)
  (setf (gethash (first entry) *op-sigs*)
        (cons (second entry) (third entry))))

;;; Memory ops: opcode -> (value-width-bits in-bits out-type).
(defparameter *mem-op-widths*
  '((:i32.load 32 32 :i32) (:i64.load 32 64 :i64)
    (:f32.load 32 32 :f32) (:f64.load 32 64 :f64)
    (:i32.load8_s 32 8 :i32) (:i32.load8_u 32 8 :i32)
    (:i32.load16_s 32 16 :i32) (:i32.load16_u 32 16 :i32)
    (:i64.load8_s 32 8 :i64) (:i64.load8_u 32 8 :i64)
    (:i64.load16_s 32 16 :i64) (:i64.load16_u 32 16 :i64)
    (:i64.load32_s 32 32 :i64) (:i64.load32_u 32 32 :i64)
    (:i32.store 32 32 :i32) (:i64.store 32 64 :i64)
    (:f32.store 32 32 :f32) (:f64.store 32 64 :f64)
    (:i32.store8 32 8 :i32) (:i32.store16 32 16 :i32)
    (:i64.store8 32 8 :i64) (:i64.store16 32 16 :i64)
    (:i64.store32 32 32 :i64)))

;;; ---------------------------------------------------------------
;;; Immediate validation for memory and reference instructions.

(defun %check-memarg (fv imms width-bits)
  "Validate a memarg: memidx must exist, alignment must not exceed the
natural width (spec: 2^align <= N/8). Offsets are unconstrained u32."
  (let* ((memidx (or (getf imms :memidx) 0))
         (align (getf imms :align))
         (what (format nil "memory access (width ~a bits)" width-bits)))
    (vctx-mem-at (fv-ctx fv) memidx what)
    (unless (and (integerp align) (>= align 0)
                 (<= (ash 1 align) (/ width-bits 8)))
      (verr "alignment must not be larger than natural (offset ~a: align ~a > ~a)"
            (fv-offset fv) align (/ width-bits 8)))
    (unless (integerp (getf imms :offset))
      (verr "memory offset immediate must be an integer"))))

(defun %local-type (fv localidx)
  (let ((locals (fv-locals fv)))
    (unless (and (integerp localidx) (>= localidx 0)
                 (< localidx (length locals)))
      (verr "local.get/set/tee: local index ~a out of range (function has ~a local~:p)"
            localidx (length locals)))
    (svref locals localidx)))

(defun %global-type (fv globalidx what)
  (first (vctx-global-at (fv-ctx fv) globalidx what)))

(defun %global-mutable-p (fv globalidx what)
  (second (vctx-global-at (fv-ctx fv) globalidx what)))

(defun %instr-op-sig (fv op)
  "Simple-signature dispatch. Every decoded opcode must have a rule;
unknown ops fail closed (defense against IR corruption)."
  (multiple-value-bind (sig foundp) (gethash op *op-sigs*)
    (unless foundp
      (verr "unknown opcode ~a at offset ~a" op (fv-offset fv)))
    sig))

(defun validate-seq (fv body)
  "Validate a flat instruction sequence (the decoder strips block
bodies of their :end terminator; bodies of :if carry no :else/:end)."
  (loop for instr across body
        do (validate-instr fv instr)))

(defun validate-instr (fv instr)
  "Validate one instruction against FV's stacks, per the spec algorithm."
  (let ((op (wasm-instr-op instr))
        (imms (wasm-instr-imms instr))
        (module (vctx-module (fv-ctx fv))))
    (setf (fv-offset fv) (wasm-instr-offset instr))
    (macrolet ((imm (key) `(getf imms ,key)))
      (cond
        ;; --- control ---
        ((eq op :unreachable) (%unreachable fv))
        ((eq op :nop))
        ((eq op :block)
         (multiple-value-bind (in out)
             (valid-blocktype-params-results module (imm :bt))
           (%pop-vals fv in)
           (%push-ctrl fv :block in out)
           (validate-seq fv (wasm-instr-body instr))
           (let ((frame (%pop-ctrl fv)))
             (declare (ignore frame))
             (%push-vals fv out))))
        ((eq op :loop)
         (multiple-value-bind (in out)
             (valid-blocktype-params-results module (imm :bt))
           (%pop-vals fv in)
           (%push-ctrl fv :loop in out)
           (validate-seq fv (wasm-instr-body instr))
           (%pop-ctrl fv)
           (%push-vals fv out)))
        ((eq op :if)
         (multiple-value-bind (in out)
             (valid-blocktype-params-results module (imm :bt))
           (%pop-val-expect fv :i32)
           (%pop-vals fv in)
           (%push-ctrl fv :if in out)
           (validate-seq fv (wasm-instr-body instr))
           (%pop-ctrl fv)
           (when (getf imms :else-body)
             (%push-ctrl fv :else in out)
             (validate-seq fv (getf imms :else-body))
             (%pop-ctrl fv))
           (%push-vals fv out)))
        ((eq op :br)
         (let* ((frame (%ctrl-label fv (imm :label) "br"))
                (types (%label-types frame)))
           (%pop-vals fv types)
           (%unreachable fv)))
        ((eq op :br-if)
         (let* ((frame (%ctrl-label fv (imm :label) "br-if"))
                (types (%label-types frame)))
           (%pop-val-expect fv :i32)
           (%pop-vals fv types)
           (%push-vals fv types)))
        ((eq op :br-table)
         (let* ((targets (imm :targets))
                (default (imm :default))
                (dframe (%ctrl-label fv default "br-table default"))
                (arity (length (%label-types dframe))))
           (%pop-val-expect fv :i32)
           (loop for target across targets
                 for i from 0
                 for tframe = (%ctrl-label fv target "br-table")
                 do (let ((types (%label-types tframe)))
                      (unless (= (length types) arity)
                        (verr "br-table: target ~a arity ~a does not match default arity ~a"
                               i (length types) arity))
                      ;; pop-then-push preserves Bot soundness
                      (%push-vals fv (%pop-vals-bot fv types))))
           (let ((types (%label-types dframe)))
             (%pop-vals fv types))
           (%unreachable fv)))
        ((eq op :return)
         (let ((types (ctrl-frame-end-types (%ctrl-label fv 0 "return"))))
           (declare (ignore types))
           ;; function results = outermost frame's end types
           (let ((results (ctrl-frame-end-types (car (last (fv-ctrls fv))))))
             (%pop-vals fv results)))
         (%unreachable fv))
        ((eq op :call)
         (let ((ft (vctx-func-type (fv-ctx fv) (imm :func) "call")))
           (%pop-vals fv (wasm-func-type-params ft))
           (%push-vals fv (wasm-func-type-results ft))))
        ((eq op :call-indirect)
         (let* ((table (vctx-table-at (fv-ctx fv) (imm :table)
                                      "call-indirect"))
                (ft (func-type-at module (imm :type)
                                  "call-indirect type")))
           (unless (eq (wasm-table-type table) :funcref)
             (verr "call-indirect: table ~a must have type funcref, got ~a"
                   (imm :table) (wasm-table-type table)))
           (%pop-val-expect fv :i32)
           (%pop-vals fv (wasm-func-type-params ft))
           (%push-vals fv (wasm-func-type-results ft))))
        ;; --- parametric ---
        ((eq op :drop) (%pop-val fv))
        ((eq op :select)
         (let ((types (imm :types)))
           (cond (types
                  ;; typed select: the decoder emits :types as a
                  ;; one-element vector (CONTRACTS.md); accept any
                  ;; sequence and index element 0.
                  (unless (and (typep types 'sequence)
                               (= (length types) 1))
                    (verr "select: type vector must have exactly one element, got ~a"
                          (if (typep types 'sequence)
                              (length types)
                              "none")))
                  (let ((t_ (elt types 0)))
                    (unless (valtype-p t_)
                      (verr "select: invalid value type ~a" t_))
                    (%pop-val-expect fv :i32)
                    (%pop-val-expect fv t_)
                    (%pop-val-expect fv t_)
                    (%push-val fv t_)))
                 (t
                  (%pop-val-expect fv :i32)
                  (let ((t1 (%pop-val fv)) (t2 (%pop-val fv)))
                    (unless (and (or (numtype-p t1) (eq t1 :bot))
                                 (or (numtype-p t2) (eq t2 :bot)))
                      (verr "select without type: operands must be numeric (got ~a, ~a)"
                            t1 t2))
                    (cond ((eq t1 :bot) (%push-val fv t2))
                          ((eq t2 :bot) (%push-val fv t1))
                          ((equal t1 t2) (%push-val fv t1))
                          (t (verr "select: operand types differ (~a vs ~a)"
                                   t1 t2))))))))
        ;; --- variable ---
        ((eq op :local.get)
         (%push-val fv (%local-type fv (imm :local))))
        ((eq op :local.set)
         (%pop-val-expect fv (%local-type fv (imm :local))))
        ((eq op :local.tee)
         (let ((t_ (%local-type fv (imm :local))))
           (%pop-val-expect fv t_)
           (%push-val fv t_)))
        ((eq op :global.get)
         (%push-val fv (%global-type fv (imm :global) "global.get")))
        ((eq op :global.set)
         (let ((x (imm :global)))
           (unless (%global-mutable-p fv x "global.set")
             (verr "global.set: global ~a is immutable" x))
           (%pop-val-expect fv (%global-type fv x "global.set"))))
        ;; --- table ---
        ((eq op :table.get)
         (let ((table (vctx-table-at (fv-ctx fv) (imm :table) "table.get")))
           (%pop-val-expect fv :i32)
           (%push-val fv (wasm-table-type table))))
        ((eq op :table.set)
         (let ((table (vctx-table-at (fv-ctx fv) (imm :table) "table.set")))
           (%pop-val-expect fv (wasm-table-type table))
           (%pop-val-expect fv :i32)))
        ((eq op :table.size)
         (vctx-table-at (fv-ctx fv) (imm :table) "table.size")
         (%push-val fv :i32))
        ((eq op :table.grow)
         (let ((table (vctx-table-at (fv-ctx fv) (imm :table)
                                     "table.grow")))
           (%pop-val-expect fv :i32)
           (%pop-val-expect fv (wasm-table-type table))
           (%push-val fv :i32)))
        ((eq op :table.fill)
         (let ((table (vctx-table-at (fv-ctx fv) (imm :table)
                                     "table.fill")))
           (%pop-val-expect fv :i32)
           (%pop-val-expect fv (wasm-table-type table))
           (%pop-val-expect fv :i32)))
        ((eq op :table.copy)
         (let ((dst (vctx-table-at (fv-ctx fv) (imm :dst) "table.copy dst"))
               (src (vctx-table-at (fv-ctx fv) (imm :src) "table.copy src")))
           (unless (eq (wasm-table-type src) (wasm-table-type dst))
             (verr "table.copy: element types differ (~a vs ~a)"
                   (wasm-table-type src) (wasm-table-type dst)))
           (%pop-val-expect fv :i32)
           (%pop-val-expect fv :i32)
           (%pop-val-expect fv :i32)))
        ((eq op :table.init)
         (let* ((table (vctx-table-at (fv-ctx fv) (imm :table)
                                      "table.init"))
                (ert (vctx-elem-type-at (fv-ctx fv) (imm :elem)
                                        "table.init")))
           (unless (eq ert (wasm-table-type table))
             (verr "table.init: element type ~a does not match table type ~a"
                   ert (wasm-table-type table)))
           (%pop-val-expect fv :i32)
           (%pop-val-expect fv :i32)
           (%pop-val-expect fv :i32)))
        ((eq op :elem.drop)
         (vctx-elem-type-at (fv-ctx fv) (imm :elem) "elem.drop"))
        ;; --- memory ---
        ((member op '(:memory.size :memory.grow))
         (vctx-mem-at (fv-ctx fv) (imm :memidx) (string op))
         (when (eq op :memory.grow)
           (%pop-val-expect fv :i32))
         (%push-val fv :i32))
        ((eq op :memory.fill)
         (vctx-mem-at (fv-ctx fv) (imm :memidx) "memory.fill")
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32))
        ((eq op :memory.copy)
         (vctx-mem-at (fv-ctx fv) (imm :dst) "memory.copy dst")
         (vctx-mem-at (fv-ctx fv) (imm :src) "memory.copy src")
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32))
        ((eq op :memory.init)
         (vctx-mem-at (fv-ctx fv) (imm :memidx) "memory.init")
         (unless (vctx-data-in-range-p (fv-ctx fv) (imm :data))
           (verr "memory.init: data segment index ~a out of range (module has ~a)"
                 (imm :data) (vctx-data-count (fv-ctx fv))))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32))
        ((eq op :data.drop)
         (unless (vctx-data-in-range-p (fv-ctx fv) (imm :data))
           (verr "data.drop: data segment index ~a out of range (module has ~a)"
                 (imm :data) (vctx-data-count (fv-ctx fv)))))
        ;; --- reference ---
        ((eq op :ref.null)
         (let ((ht (imm :type)))
           (unless (reftype-p ht)
             (verr "ref.null: invalid heap type ~a" ht))
           (%push-val fv ht)))
        ((eq op :ref.is-null)
         (%pop-ref fv)
         (%push-val fv :i32))
        ((eq op :ref.func)
         (let ((x (imm :func)))
           (vctx-func-type (fv-ctx fv) x "ref.func")
           (unless (gethash x (vctx-declared-refs (fv-ctx fv)))
             (verr "ref.func: function ~a is not declared by an element segment or export"
                   x))
           (%push-val fv :funcref)))
        ;; --- consts ---
        ((eq op :i32.const) (%push-val fv :i32))
        ((eq op :i64.const) (%push-val fv :i64))
        ((eq op :f32.const) (%push-val fv :f32))
        ((eq op :f64.const) (%push-val fv :f64))
        ;; --- loads/stores via width table ---
        ((assoc op *mem-op-widths*)
         (let* ((info (assoc op *mem-op-widths*))
                (value-bits (third info))  ; natural (N/8) alignment bound
                (out-type (fourth info))
                (store-p (search "STORE" (symbol-name op))))
           (%check-memarg fv imms value-bits)
           (if store-p
               (progn
                 (%pop-val-expect fv out-type)
                 (%pop-val-expect fv :i32))
               (progn
                 (%pop-val-expect fv :i32)
                 (%push-val fv out-type)))))
        (t
         ;; simple fixed-signature numeric ops
         (let ((sig (%instr-op-sig fv op)))
           (%pop-vals fv (car sig))
           (%push-vals fv (cdr sig))))))))

(defun expand-locals (params locals)
  "Local index space: params then declared locals, each (valtype . count)
expanded. Also validates local valtypes."
  (let ((v (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (p params) (vector-push-extend p v))
    (dolist (entry locals)
      (let ((t_ (car entry)) (n (cdr entry)))
        (unless (valtype-p t_)
          (verr "invalid local type ~a" t_))
        (unless (and (integerp n) (>= n 0))
          (verr "invalid local count ~a" n))
        (dotimes (i n) (vector-push-extend t_ v))))
    (coerce v 'simple-vector)))

(defun validate-function-body (ctx body locals params results &key (what "function"))
  "Run the operand/control-stack algorithm over BODY (vector of
wasm-instr including the trailing :end)."
  (let ((fv (make-fv :ctx ctx :locals locals :vals nil :ctrls nil)))
    (%push-ctrl fv :func params results :seed-stack nil)
    (loop for instr across body
          for op = (wasm-instr-op instr)
          do (cond ((eq op :end)
                    ;; the outermost :end pops the :func frame; the
                    ;; popped results legitimately remain on the stack
                    ;; (they are the function's results)
                    (%pop-ctrl fv))
                   (t (validate-instr fv instr))))
    (unless (null (fv-ctrls fv))
      (verr "~a: unbalanced control stack (unclosed frames remain)" what))
    fv))

(defun %pop-vals-bot (fv types)
  "Pop TYPES returning the possibly-:bot actuals (for br_table)."
  (let ((actual '()))
    (dolist (t_ (reverse types))
      (push (%pop-val-expect fv t_) actual))
    actual))


;;; ---------------------------------------------------------------
;;; Module-level validation (spec: valid/modules.html#module).

(defun validate-module (module)
  "Validate a decoded wasm-module; return T or signal
wasmdown:validation-error."
  (check-types-section module)
  (let ((ctx (build-vctx module)))
    (check-imports module)
    (check-tables-and-mems ctx)
    (check-globals ctx)
    (check-elems ctx)
    (check-datas ctx)
    (check-exports module ctx)
    (check-start module ctx)
    (check-datacount module)
    (validate-all-funcs module ctx))
  t)

(defun check-types-section (module)
  (loop for ft across (wasm-module-types module)
        for i from 0
        do (loop for t_ in (append (wasm-func-type-params ft)
                                   (wasm-func-type-results ft))
                 unless (valtype-p t_)
                   do (verr "type ~a: invalid value type ~a" i t_))))

(defun check-imports (module)
  (loop for imp across (wasm-module-imports module)
        for i from 0
        do (let ((kind (wasm-import-kind imp))
                 (ty (wasm-import-type imp)))
             (case kind
               (:func
                (unless (func-type-at-soft module ty)
                  (verr "import ~a: func type index ~a out of range" i ty)))
               (:table
                (unless (reftype-p (wasm-table-type ty))
                  (verr "import ~a: invalid table element type ~a"
                        i (wasm-table-type ty)))
                (check-limits (wasm-table-limits ty)
                              (format nil "import ~a (table)" i)
                              *max-table-size*))
               (:memory
                (check-limits (wasm-memory-limits ty)
                              (format nil "import ~a (memory)" i)))
               (:global
                (unless (valtype-p (car ty))
                  (verr "import ~a: invalid global type ~a" i (car ty))))
               (t (verr "import ~a: unknown import kind ~a" i kind))))))

(defun check-tables-and-mems (ctx)
  (let ((module (vctx-module ctx)))
    (loop for table across (wasm-module-tables module)
          for i from 0
          do (progn
               (unless (reftype-p (wasm-table-type table))
                 (verr "table ~a: invalid element type ~a"
                       i (wasm-table-type table)))
               (check-limits (wasm-table-limits table)
                             (format nil "table ~a" i)
                             *max-table-size*)))
    (loop for mem across (wasm-module-mems module)
          for i from 0
          do (check-limits (wasm-memory-limits mem)
                            (format nil "memory ~a" i)))))

(defun check-globals (ctx)
  "Globals are validated sequentially: each init may reference only
imported or previously-defined globals (spec note on const exprs)."
  (let ((module (vctx-module ctx))
        (nimported (vctx-imported-global-count ctx)))
    (loop for g across (wasm-module-globals module)
          for i from 0
          for limit = (+ nimported i)     ; exclusive bound
          do (let ((type (const-expr-type ctx (wasm-global-init g)
                                          :global-limit limit
                                          :what (format nil
                                                        "global ~a init" i))))
               (unless (equal type (wasm-global-type g))
                 (verr "global ~a: init expression type ~a does not match declared type ~a"
                       i type (wasm-global-type g)))))))

(defun check-elems (ctx)
  (let ((module (vctx-module ctx)))
    (loop for elem across (wasm-module-elems module)
          for i from 0
          do (let ((mode (wasm-elem-mode elem))
                   (etype (wasm-elem-type elem)))
               (unless (reftype-p etype)
                 (verr "element segment ~a: invalid element type ~a"
                       i etype))
               (loop for expr in (wasm-elem-inits elem)
                     for j from 0
                     do (let ((type (const-expr-type
                                     ctx expr
                                     :global-limit
                                     (vctx-imported-global-count ctx)
                                     :what (format nil
                                                   "element segment ~a item ~a"
                                                   i j))))
                          (unless (equal type etype)
                            (verr "element segment ~a item ~a: type ~a does not match segment type ~a"
                                  i j type etype))))
               (when (and (consp mode) (eq (first mode) :active))
                 (let ((tableidx (second mode))
                       (expr (third mode)))
                   (let ((table (vctx-table-at ctx tableidx
                                               (format nil
                                                       "element segment ~a"
                                                       i))))
                     (unless (eq etype (wasm-table-type table))
                       (verr "element segment ~a: type ~a does not match table ~a type ~a"
                             i etype tableidx (wasm-table-type table))))
                   (let ((type (const-expr-type
                                ctx expr
                                :global-limit
                                (vctx-imported-global-count ctx)
                                :what (format nil
                                              "element segment ~a offset"
                                              i))))
                     (unless (eq type :i32)
                       (verr "element segment ~a: offset type must be i32, got ~a"
                             i type)))))))))

(defun check-datas (ctx)
  (let ((module (vctx-module ctx)))
    (loop for data across (wasm-module-datas module)
          for i from 0
          do (let ((mode (wasm-data-mode data)))
               (when (and (consp mode) (eq (first mode) :active))
                 (let ((memidx (second mode))
                       (expr (third mode)))
                   (vctx-mem-at ctx memidx
                                (format nil "data segment ~a" i))
                   (let ((type (const-expr-type
                                ctx expr
                                :global-limit
                                (vctx-imported-global-count ctx)
                                :what (format nil
                                              "data segment ~a offset"
                                              i))))
                     (unless (eq type :i32)
                       (verr "data segment ~a: offset type must be i32, got ~a"
                             i type)))))))))

(defun check-exports (module ctx)
  (let ((names (make-hash-table :test #'equal)))
    (loop for exp across (wasm-module-exports module)
          for i from 0
          do (let ((name (wasm-export-name exp))
                   (kind (wasm-export-kind exp))
                   (index (wasm-export-index exp)))
               (unless (member kind '(:func :table :memory :global))
                 (verr "export ~a: unknown export kind ~a" i kind))
               (unless (export-index-ok-p ctx kind index)
                 (verr "export ~a: index ~a out of range for kind ~a"
                       i index kind))
               (when (gethash name names)
                 (verr "export ~a: duplicate export name ~s" i name))
               (setf (gethash name names) t)))))

(defun export-index-ok-p (ctx kind index)
  "Export indices count imports + definitions."
  (case kind
    (:func (< -1 index (length (vctx-func-typeidxs ctx))))
    (:table (< -1 index (length (vctx-tables ctx))))
    (:memory (< -1 index (length (vctx-mems ctx))))
    (:global (< -1 index (length (vctx-globals ctx))))
    (t nil)))

(defun check-start (module ctx)
  (let ((start (wasm-module-start module)))
    (when start
      (let ((ft (vctx-func-type ctx start "start function")))
        (unless (and (null (wasm-func-type-params ft))
                     (null (wasm-func-type-results ft)))
          (verr "start function ~a must have type [] -> []" start))))))

(defun check-datacount (module)
  (let ((dc (wasm-module-data-count module))
        (n (length (wasm-module-datas module))))
    (when (and dc (/= dc n))
      (verr "data count section declares ~a but data section has ~a segment~:p"
            dc n))
    ;; Spec: the DataCount section is required when the code contains
    ;; memory.init/data.drop; its absence means the data index space
    ;; those immediates reference was never declared.
    (unless dc
      (when (%module-uses-data-ops-p module)
        (verr "module uses memory.init/data.drop but has no data count section")))))

(defun %module-uses-data-ops-p (module)
  (loop for f across (wasm-module-funcs module)
        thereis (%body-uses-data-ops-p (wasm-func-body f))))

(defun %body-uses-data-ops-p (body)
  (loop for instr across body
        thereis (or (member (wasm-instr-op instr) '(:memory.init :data.drop))
                    (and (plusp (length (wasm-instr-body instr)))
                         (%body-uses-data-ops-p (wasm-instr-body instr)))
                    (let ((eb (getf (wasm-instr-imms instr) :else-body)))
                      (and (vectorp eb)
                           (plusp (length eb))
                           (%body-uses-data-ops-p eb))))))

(defun validate-all-funcs (module ctx)
  (loop for f across (wasm-module-funcs module)
        for i from 0
        for funcidx = (+ (vctx-num-imported-funcs ctx) i)
        do (let* ((ft (vctx-func-type ctx funcidx
                                      (format nil "function ~a" funcidx)))
                  (params (wasm-func-type-params ft))
                  (results (wasm-func-type-results ft))
                  (locals (expand-locals params (wasm-func-locals f))))
             (validate-function-body ctx (wasm-func-body f) locals
                                     params results
                                     :what (format nil "function ~a"
                                                   funcidx)))))

(defun describe-module-features (module)
  "Return an alist of feature keywords to booleans for CLI inspect."
  (declare (ignore module))
  '((:sign-extension-ops . t)
    (:saturating-float-to-int . t)
    (:multi-value . t)
    (:bulk-memory . t)
    (:reference-types . t)
    (:simd . nil)
    (:threads . nil)
    (:tail-call . nil)
    (:exceptions . nil)))
