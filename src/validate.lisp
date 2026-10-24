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
  "A reference type: the legacy keywords or the typed-ref lattice
shapes (:REF NULL? HEAP) of wasmdown.types (typed-ref-p). Pure SHAPE
check — a (:TYPE N) heap's index range is checked separately at
check-valtype ('unknown type', ref.wast's assert_invalid batch)."
  (or (member t_ '(:funcref :externref :exnref))
      (and (consp t_) (wasmdown.types:typed-ref-p t_))))

(defun valid-heaptype-p (heap types)
  "Whether HEAP is well-formed against a type vector: an abstract
keyword, or (:TYPE N) with N in range ('unknown type', ref.wast)."
  (etypecase heap
    (keyword t)
    (cons (and (integerp (second heap))
               (>= (second heap) 0)
               (< (second heap) (length types))))))

(defun check-valtype (t_ types what)
  "Validate valtype T_ against the type vector TYPES, signaling
'unknown type' for an out-of-range (:TYPE N) heap (ref.wast's full
assert_invalid batch: params, results, locals, globals, tables,
elems, blocktype results)."
  (unless (and (reftype-p t_) (valid-heaptype-p
                               (wasmdown.types:typed-ref-heap t_) types))
    (verr "~a: unknown type ~a" what
          (if (consp t_)
              (second (wasmdown.types:typed-ref-heap t_))
              t_)))
  t_)

(defun check-valtype-list (types what &rest lists)
  "Every valtype in LISTS (flattened) must be well-formed: shape
\(valtype-p) AND, for typed refs, an in-range (:TYPE N) heap
\('unknown type', ref.wast's local/blocktype/select positions)."
  (dolist (lst lists)
    (dolist (t_ lst)
      (unless (valtype-p t_)
        (verr "~a: invalid value type ~a" what t_))
      (when (and (reftype-p t_) (consp t_))
        (check-valtype t_ types what)))))

(defun valtype-p (t_)
  (or (numtype-p t_) (reftype-p t_)))

(defun ref-null-p (t_)
  "Reference type with explicit nullability, as in decodable blocktypes."
  (declare (ignore t_))
  nil)

(defun valid-blocktype-params-results (module bt)
  "Resolve a blocktype to (params results) lists, or signal. A
typed-ref valtype blocktype's (:TYPE N) heap must resolve ('unknown
type', ref.wast's block/loop/if batch)."
  (cond
    ((null bt) (values nil nil))
    ((valtype-p bt)
     (when (and (reftype-p bt) (consp bt))
       (check-valtype bt (wasm-module-types module) "blocktype"))
     (values nil (list bt)))
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
  (declared-refs nil)          ; list of funcidx declared via elem/export
  ;; EH (Core 3.0): tag index space — vector of tag functypes
  ;; (imports first, then the tag section's typeidx-resolved types).
  (tags #())
  ;; CANONICALIZATION (loop 20): vector typeidx -> min equivalent
  ;; typeidx (iso-recursive equivalence classes; spec: a module's
  ;; structurally-equal types are ONE canonical type). Computed once
  ;; at build-vctx; %HEAP-SUB-P and every typeuse comparison route
  ;; through it.
  (canon #() :type vector))

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
                          'vector))
         ;; EH (Core 3.0): the tag index space — imported tag typeidxs
         ;; (their functype resolved against the module's types),
         ;; then the tag section's typeidxs. Tag typeidxs out of
         ;; range resolve to NIL here; CHECK-TAGS reports the honest
         ;; 'unknown type' at validation.
         (tags (coerce (append
                        (mapcar (lambda (ty)
                                  (func-type-at-soft module ty))
                                (mapcar #'wasm-import-type
                                        (remove :tag imports :key
                                                #'wasm-import-kind
                                                :test-not #'eql)))
                        (map 'list (lambda (ty)
                                     (func-type-at-soft module ty))
                             (wasm-module-tags module)))
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
               :declared-refs (%declared-func-refs module)
               :tags tags
               :canon (%build-canon module))))

(defun %build-canon (module)
  "The canonicalization map (loop 20): vector canon[i] = the MINIMUM
typeidx equivalent to i under iso-recursive equivalence (spec: within
a module, structurally-equal types are one canonical type; the class
representative is the least index — deterministic). Equivalence is
symmetric and transitive by construction of the spec's rule, so a
single min-scan per index is sound: canon[i] = min{ j : j equiv i }.
Non-plain entries (rec groups) memoize via the types comparator;
plain functype-only modules keep the raw EQUAL fast path (the same
result as the iso rule for plain types: no group sharing)."
  (let* ((types (wasm-module-types module))
         (n (length types))
         (canon (make-array n)))
    (when (zerop n) (return-from %build-canon canon))
    ;; fast path: no rec entries at all -> identity EXCEPT plain
    ;; duplicates (type.wast: (type $t (func)) x2 are canonicalized
    ;; too — find-or-add already dedups implicit uses, but explicit
    ;; duplicates may appear; equivalence for plain entries is plain
    ;; structural equality)
    (loop for i from 0 below n
          do (let ((min-idx i))
               (loop for j from 0 below i
                     when (wasmdown.types:type-equiv-p module j module i)
                       do (setf min-idx (min min-idx j)))
               (setf (aref canon i) min-idx)))
    canon))

(defun %declared-func-refs (module)
  "The funcidx set legal for ref.func (spec: funcidx(globals mems tables
  elems exports)): every function referenced by a global initializer's
  ref.func, by an element segment, or by any export."
  (let ((refs (make-hash-table)))
    (loop for g across (wasm-module-globals module)
          do (loop for instr across (wasm-global-init g)
                   when (eq (wasm-instr-op instr) :ref.func)
                     do (setf (gethash (getf (wasm-instr-imms instr)
                                             :func)
                                       refs) t)))
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


(defun vctx-tag-type (ctx tagidx what)
  "Tag functype for absolute TAGIDX (imports first), or error. A tag
whose typeidx resolved to NIL (out of range) at build-vctx time
reports the honest 'unknown type' here."
  (let ((v (vctx-tags ctx)))
    (unless (and (integerp tagidx) (>= tagidx 0) (< tagidx (length v)))
      (verr "~a: tag index ~a out of range (module has ~a tag~:p)"
            what tagidx (length v)))
    (let ((ft (svref v tagidx)))
      (unless ft
        (verr "~a: unknown type" what))
      ft)))

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
                    (cond
                     ;; legacy shorthand keywords: push as-is
                     ((member ht '(:funcref :externref :exnref))
                      (push1 ht))
                     ;; typed heap: (:type N) or abstract keyword —
                     ;; ref.null pushes the NULLABLE typed ref
                     ((or (keywordp ht) (consp ht))
                      (let ((types (wasm-module-types
                                    (vctx-module ctx))))
                        (let ((v (list :ref 'null ht)))
                          (unless (and (reftype-p v)
                                       (valid-heaptype-p ht types))
                            (verr "~a: ref.null of invalid heap type ~a"
                                  what ht))
                          (push1 v))))
                     (t (verr "~a: ref.null of invalid heap type ~a"
                              what ht)))))
                 ((eq op :ref.func)
                  (let ((x (getf imms :func))
                        (nf (length (vctx-func-typeidxs ctx))))
                    (unless (and (integerp x) (>= x 0) (< x nf))
                      (verr "~a: ref.func index ~a out of range (module has ~a func~:p)"
                            what x nf))
                    (unless (gethash x (vctx-declared-refs ctx))
                      (verr "~a: ref.func ~a is not declared by an element segment or export"
                            what x))
                    ;; spec: ref.func's type is [t1*] -> [(ref $t)]
                    ;; where $t is the function's OWN typeuse — the
                    ;; minimal type (call_ref.wast: local.set into a
                    ;; (ref $ii) local needs exactly this).
                    (push1 (list :ref
                                 (list :type
                                       (svref (vctx-func-typeidxs ctx)
                                              x))))))
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
                   ;; spec const-expr rule: only immutable globals
                   (when (second g)
                     (verr "~a: global.get ~a of a mutable global is not constant"
                           what x))
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
  unreachable
  ;; definite-init snapshot at frame entry (local_init.wast): the
  ;; frame's end RESTORES it — a set inside a block/if is not proven
  ;; after the join (a br may skip it); the :func frame holds NIL
  ;; (nothing to restore).
  (inits-snapshot nil))

(defstruct fv
  "Function-validation state."
  ctx
  locals                       ; vector of valtypes
  vals                         ; list-as-stack of valtypes (top = car)
  ctrls                        ; list-as-stack of ctrl-frame (top = car)
  ;; definite initialization (local_init.wast): bit vector marking
  ;; locals PROVEN initialized on every path; only non-defaultable
  ;; locals (wasmdown.types:defaultable-valtype-p NIL) matter.
  (inits nil :type (or null (vector bit)))
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
  "Pop one operand and require it <: EXPECT (spec: operand subtyping
at every pop site — reftype-sub-p for reference types, EQUAL for the
rest; :bot (stack-polymorphic bottom) always passes)."
  (let ((actual (%pop-val fv)))
    (unless (or (eq actual :bot)
                (equal actual expect)
                (and (reftype-p actual) (reftype-p expect)
                     (wasmdown.types:reftype-sub-p
                      actual expect
                      (wasm-module-types (vctx-module (fv-ctx fv)))
                      (vctx-canon (fv-ctx fv)))))
      (verr "type mismatch at offset ~a: expected ~a, got ~a"
            (fv-offset fv) expect actual))
    actual))

(defun %pop-ref (fv)
  "Pop any reference type; the polymorphic :bot (post-unreachable)
passes through unchanged — every client normalizes it (the typed
arms via %non-null-view's :bot passthrough; %pop-val-expect's
wildcard). The old :funcref placeholder predates the typed lattice
and poisoned unreachable-stack ref operands (br_on_non_null.wast's
  'unreachable' export: (br_on_non_null $l (unreachable)) computed
  (:ref :func) where the label wanted (:ref (:type 0)))."
  (let ((actual (%pop-val fv)))
    (cond ((eq actual :bot) :bot)
          ((reftype-p actual) actual)
          (t (verr "type mismatch at offset ~a: expected a reference type, got ~a"
                    (fv-offset fv) actual)))))

(defun %non-null-view (r what)
  "The NON-NULL view of a reference type R for ref.as_non_null /
br_on_null / br_on_non_null: a typed ref keeps its heap (nullable
loses the null); a legacy keyword is its nullable spelling, so the
view is (:ref :func)/(:ref :extern)/(:ref :exn). The polymorphic
BOTTOM (post-unreachable operand) passes through unchanged — bottom's
non-null view is still bottom, satisfying any label (br_on_non_null.
wast's 'unreachable' export: (br_on_non_null $l (unreachable))).
Signals a validation error for non-reference operands (WHAT names
the op)."
  (cond
   ((eq r :bot) :bot)
   ((and (consp r) (wasmdown.types:typed-ref-p r))
    (if (wasmdown.types:typed-ref-nullable-p r)
        (list :ref (wasmdown.types:typed-ref-heap r))
        r))
   ((keywordp r)
    (list :ref (ecase r (:funcref :func) (:externref :extern)
                      (:exnref :exn))))
   (t (verr "~a: operand ~a is not a reference" what r))))

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
                         :unreachable nil
                         ;; definite-init snapshot at entry
                         ;; (local_init.wast): restored at the
                         ;; frame's end — a set inside a block is
                         ;; not proven after the join.
                         :inits-snapshot (and (fv-inits fv)
                                              (copy-seq (fv-inits fv))))
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
    ;; restore the entry's definite-init state (the join is as
    ;; conservative as the entry: branches may skip any inner set).
    (when (and (ctrl-frame-inits-snapshot frame) (fv-inits fv))
      (replace (fv-inits fv) (ctrl-frame-inits-snapshot frame)))
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

(defun %check-label-types (fv labelidx want)
  "A try_table catch clause's label must exist and carry EXACTLY the
types WANT at branch time (spec: the catch handler receives the tag's
payload values, plus an exnref for the _ref clauses)."
  (let ((frame (%ctrl-label fv labelidx "try_table catch clause")))
    (let ((have (%label-types frame)))
      (unless (equal have want)
        (verr "type mismatch: catch clause label ~a carries ~a, expected ~a"
              labelidx have want)))))

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

(defun %results-sub-p (callee caller types &optional canon)
  "Per-element result subtyping for tail calls: each callee result
must be <: the caller's declared slot — numeric types EQUAL,
reference types via reftype-sub-p (return_call.wast's result
subtyping: ((ref null $t)) <: (funcref); CANON routes the
typeuse-equivalence cases, loop 20)."
  (and (= (length callee) (length caller))
       (every (lambda (x y)
                (or (equal x y)
                    (and (reftype-p x) (reftype-p y)
                         (wasmdown.types:reftype-sub-p
                          x y types canon))))
              callee caller)))

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
natural width (spec: 2^align <= N/8), and the offset must fit the u32
range the spec's memarg grammar allows (suite: address.wast/
align.wast 'offset out of range' with offset=2^32 and above)."
  (let* ((memidx (or (getf imms :memidx) 0))
         (align (getf imms :align))
         (offset (getf imms :offset))
         (what (format nil "memory access (width ~a bits)" width-bits)))
    (vctx-mem-at (fv-ctx fv) memidx what)
    (unless (and (integerp align) (>= align 0)
                 (<= (ash 1 align) (/ width-bits 8)))
      (verr "alignment must not be larger than natural (offset ~a: align ~a > ~a)"
            (fv-offset fv) align (/ width-bits 8)))
    (unless (integerp offset)
      (verr "memory offset immediate must be an integer"))
    (unless (and (>= offset 0) (<= offset #xffffffff))
      (verr "offset out of range (offset ~a): ~a is not within u32"
            (fv-offset fv) offset))))

(defun %local-type (fv localidx)
  (let ((locals (fv-locals fv)))
    (unless (and (integerp localidx) (>= localidx 0)
                 (< localidx (length locals)))
      (verr "local.get/set/tee: local index ~a out of range (function has ~a local~:p)"
            localidx (length locals)))
    (svref locals localidx)))

(defun %init-all-defaultable-p (locals)
  "Whether every local is defaultable (numeric or nullable ref): no
definite-init tracking is needed at all (the historical fast path —
the 286-test suite's modules never construct the INITS vector)."
  (loop for t_ across locals
        always (wasmdown.types:defaultable-valtype-p t_)))

;;; ---------------------------------------------------------------
;;; GC value opcodes (loop 22): the typing rules, a separate defun so
;;; the main cond stays small. The helpers resolve comptypes from the
;;; module's type vector (uniform rec model).

(defun %comptype-at (fv typeidx what)
  "The comptype at TYPEIDX ('unknown type' when out of range)."
  (let ((types (wasm-module-types (vctx-module (fv-ctx fv)))))
    (unless (and (integerp typeidx) (>= typeidx 0)
                 (< typeidx (length types)))
      (verr "~a: unknown type ~a" what typeidx))
    (let* ((entry (svref types typeidx))
           (ct (if (wasmdown.types:wasm-rec-type-p entry)
                   (wasmdown.types:wasm-rec-type-comptype entry)
                   (make-wasm-comptype
                    :func nil '()
                    (wasm-func-type-params entry)
                    (wasm-func-type-results entry)))))
      ct)))

(defun %struct-comptype-at (fv typeidx what)
  (let ((ct (%comptype-at fv typeidx what)))
    (unless (eq (wasmdown.types:wasm-comptype-kind ct) :struct)
      (verr "~a: type ~a is not a struct" what typeidx))
    ct))

(defun %array-comptype-at (fv typeidx what)
  (let ((ct (%comptype-at fv typeidx what)))
    (unless (eq (wasmdown.types:wasm-comptype-kind ct) :array)
      (verr "~a: type ~a is not an array" what typeidx))
    ct))

(defun %struct-fields (ct)
  (wasmdown.types:wasm-comptype-fields ct))

(defun %array-elem-pair (ct)
  "The array comptype's single (valtype . mutable-p) field pair."
  (first (wasmdown.types:wasm-comptype-fields ct)))

(defun %field-pair-at (ct fieldidx what)
  (let ((fs (%struct-fields ct)))
    (unless (and (integerp fieldidx) (>= fieldidx 0) (< fieldidx (length fs)))
      (verr "~a: unknown field ~a" what fieldidx))
    (nth fieldidx fs)))

(defun %packed-p (valtype)
  (member valtype '(:i8 :i16)))

(defun %storage-value-type (st)
  "The OPERAND type of a possibly-packed storage type: i8/i16
storages exchange i32 VALUES (the normalization happens at interp)."
  (if (%packed-p st) :i32 st))

(defun %ref-of (heap &optional (nullable t))
  (if nullable (list :ref 'null heap) (list :ref heap)))

(defparameter *gc-opcodes*
  (let ((h (make-hash-table :test 'eq)))
    (dolist (op '(:struct.new :struct.new-default :struct.get
                  :struct.get-s :struct.get-u :struct.set
                  :array.new :array.new-default :array.new-fixed
                  :array.new-data :array.new-elem :array.get
                  :array.get-s :array.get-u :array.set :array.len
                  :array.fill :array.copy :array.init-data
                  :array.init-elem
                  :ref.test :ref.cast :br-on-cast :br-on-cast-fail
                  :ref.eq :any.convert-extern :extern.convert-any
                  :ref.i31 :i31.get-s :i31.get-u))
      (setf (gethash op h) t))
    h))

(defun validate-gc-instr (fv op imms module)
  "The GC value family's operand rules (loop 22, PLAN.md §3.1).
  - struct.new: [ft*] -> [(ref $t)]; struct.get/set per field
    (+packed _s/_u on i8/i16 only, i32 at the operand level)
  - array.*: new/default/fixed/new_data/new_elem produce (ref $t);
    get/set/len/fill/copy/init_* consume; copy: src-elt <: dst-elt
  - ref.test/ref.cast: STATIC top-bucket check; DYNAMIC check at
    interp (:cast-failure / 0)
  - br_on_cast/br_on_cast_fail: dst <: src; label takes the branch
    operand, fallthrough keeps the complement
  - ref.eq [eqref eqref] -> [i32]; the any<->extern bridges; i31."
  (declare (ignorable module))
  (flet ((imm (key) (getf imms key)))
    (case op
      (:struct.new
       (let* ((ct (%struct-comptype-at fv (imm :type) "struct.new"))
              (fts (mapcar #'%storage-value-type
                           (mapcar #'first (%struct-fields ct)))))
         (%pop-vals fv fts)
         (%push-val fv (%ref-of (list :type (imm :type)) nil))))
      (:struct.new-default
       (let* ((ct (%struct-comptype-at fv (imm :type)
                                       "struct.new_default"))
              (fs (%struct-fields ct)))
         (dolist (f fs)
           (unless (or (%packed-p (first f))
                       (wasmdown.types:defaultable-valtype-p
                        (first f)))
             (verr "struct.new_default: field type ~a is not defaultable"
                   (first f))))
         (%push-val fv (%ref-of (list :type (imm :type)) nil))))
      ((:struct.get :struct.get-s :struct.get-u)
       (let* ((t_ (imm :type))
              (ct (%struct-comptype-at fv t_ (string op)))
              (f (%field-pair-at ct (imm :field) (string op)))
              (ft (first f)))
         (when (and (member op '(:struct.get-s :struct.get-u))
                    (not (%packed-p ft)))
           (verr "~a on non-packed field type ~a" op ft))
         (%pop-val-expect fv (%ref-of (list :type t_)))
         (%push-val fv (if (member op '(:struct.get-s :struct.get-u))
                           :i32
                           (%storage-value-type ft)))))
      (:struct.set
       (let* ((t_ (imm :type))
              (ct (%struct-comptype-at fv t_ "struct.set"))
              (f (%field-pair-at ct (imm :field) "struct.set")))
         (unless (cdr f)
           (verr "struct.set on immutable field ~a" (imm :field)))
         (%pop-val-expect fv (%storage-value-type (first f)))
         (%pop-val-expect fv (%ref-of (list :type t_)))))
      (:array.new
       (let* ((ct (%array-comptype-at fv (imm :type) "array.new"))
              (et (first (%array-elem-pair ct))))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%storage-value-type et))
         (%push-val fv (%ref-of (list :type (imm :type)) nil))))
      (:array.new-default
       (let* ((ct (%array-comptype-at fv (imm :type)
                                      "array.new_default"))
              (et (first (%array-elem-pair ct))))
         (unless (or (%packed-p et)
                     (wasmdown.types:defaultable-valtype-p et))
           (verr "array.new_default: element type ~a is not defaultable"
                 et))
         (%pop-val-expect fv :i32)
         (%push-val fv (%ref-of (list :type (imm :type)) nil))))
      (:array.new-fixed
       (let* ((ct (%array-comptype-at fv (imm :type) "array.new_fixed"))
              (et (first (%array-elem-pair ct)))
              (n (imm :count)))
         (unless (and (integerp n) (>= n 0))
           (verr "array.new_fixed: bad count ~a" n))
         (%pop-vals fv (make-list n
                                  :initial-element
                                  (%storage-value-type et)))
         (%push-val fv (%ref-of (list :type (imm :type)) nil))))
      (:array.new-data
       (let* ((ct (%array-comptype-at fv (imm :type) "array.new_data"))
              (et (first (%array-elem-pair ct))))
         (unless (member et '(:i8 :i16 :i32))
           (verr "array.new_data: element type ~a is not packed/numeric"
                 et))
         (unless (vctx-data-in-range-p (fv-ctx fv) (imm :data))
           (verr "array.new_data: data segment index ~a out of range"
                 (imm :data)))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%push-val fv (%ref-of (list :type (imm :type)) nil))))
      (:array.new-elem
       (let* ((ct (%array-comptype-at fv (imm :type) "array.new_elem"))
              (et (first (%array-elem-pair ct))))
         (unless (and (consp et) (eq (first et) :ref))
           (verr "array.new_elem: element type ~a is not a reftype" et))
         (vctx-elem-type-at (fv-ctx fv) (imm :elem) "array.new_elem")
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%push-val fv (%ref-of (list :type (imm :type)) nil))))
      ((:array.get :array.get-s :array.get-u)
       (let* ((ct (%array-comptype-at fv (imm :type) (string op)))
              (et (first (%array-elem-pair ct))))
         (when (and (member op '(:array.get-s :array.get-u))
                    (not (%packed-p et)))
           (verr "~a on non-packed element type ~a" op et))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%ref-of (list :type (imm :type))))
         (%push-val fv (if (member op '(:array.get-s :array.get-u))
                           :i32
                           (%storage-value-type et)))))
      (:array.set
       (let* ((ct (%array-comptype-at fv (imm :type) "array.set"))
              (f (%array-elem-pair ct)))
         (unless (cdr f)
           (verr "array.set on immutable array"))
         (%pop-val-expect fv (%storage-value-type (first f)))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%ref-of (list :type (imm :type))))))
      (:array.len
       ;; NO immediate (binary 0xFB 0x0F): the operand is ANY
       ;; array-typed ref — the abstract (ref null array) admits
       ;; every concrete array typeuse via the lattice.
       (%pop-val-expect fv (%ref-of :array))
       (%push-val fv :i32))
      (:array.fill
       (let* ((ct (%array-comptype-at fv (imm :type) "array.fill"))
              (f (%array-elem-pair ct)))
         (unless (cdr f)
           (verr "array.fill on immutable array"))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%storage-value-type (first f)))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%ref-of (list :type (imm :type))))))
      (:array.copy
       (let* ((types (wasm-module-types
                      (vctx-module (fv-ctx fv))))
              (dct (%array-comptype-at fv (imm :dst) "array.copy"))
              (sct (%array-comptype-at fv (imm :src) "array.copy"))
              (det (first (%array-elem-pair dct)))
              (set_ (first (%array-elem-pair sct))))
         (unless (or (equal det set_)
                     (and (reftype-p det) (reftype-p set_)
                          (wasmdown.types:reftype-sub-p
                           set_ det types (vctx-canon (fv-ctx fv)))))
           (verr "array.copy: source element type ~a does not match ~a"
                 set_ det))
         (unless (cdr (%array-elem-pair dct))
           (verr "array.copy on immutable array"))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%ref-of (list :type (imm :src))))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%ref-of (list :type (imm :dst))))))
      (:array.init-data
       (let ((ct (%array-comptype-at fv (imm :type) "array.init_data")))
         (unless (cdr (%array-elem-pair ct))
           (verr "array.init_data on immutable array"))
         (unless (vctx-data-in-range-p (fv-ctx fv) (imm :data))
           (verr "array.init_data: data segment index ~a out of range"
                 (imm :data)))
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%ref-of (list :type (imm :type))))))
      (:array.init-elem
       (let ((ct (%array-comptype-at fv (imm :type) "array.init_elem")))
         (unless (cdr (%array-elem-pair ct))
           (verr "array.init_elem on immutable array"))
         (vctx-elem-type-at (fv-ctx fv) (imm :elem) "array.init_elem")
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv :i32)
         (%pop-val-expect fv (%ref-of (list :type (imm :type))))))
      ((:ref.test :ref.cast)
       (let* ((types (wasm-module-types
                      (vctx-module (fv-ctx fv))))
              (heap (imm :type))
              (target (%ref-of heap (imm :nullable)))
              (r (%pop-ref fv)))
         (declare (ignorable types))
         (unless (or (eq r :bot)
                     (etypecase heap
                       (keyword t)
                       (cons (valid-heaptype-p heap types))))
           (verr "~a: invalid target heap ~a" op heap))
         (unless (or (eq r :bot)
                     (and (reftype-p r)
                          (eq (wasmdown.types::%heap-top
                               (wasmdown.types:typed-ref-heap r)
                               types)
                              (wasmdown.types::%heap-top heap types))))
           (verr "~a: target ~a is outside the operand's hierarchy ~a"
                 op target r))
         (if (eq op :ref.test)
             (%push-val fv :i32)
             (%push-val fv target))))
      ((:br-on-cast :br-on-cast-fail)
       (let* ((types (wasm-module-types
                      (vctx-module (fv-ctx fv))))
              (src (%ref-of (imm :src) (imm :src-nullable)))
              (dst (%ref-of (imm :dst) (imm :dst-nullable)))
              (r (%pop-ref fv)))
         (unless (or (eq r :bot)
                     (wasmdown.types:reftype-sub-p dst src types
                                                   (vctx-canon
                                                    (fv-ctx fv))))
           (verr "~a: target ~a is not a subtype of source ~a"
                 op dst src))
         ;; The branch takes the ref slot (plus the below-vals, which
         ;; the block entry already type-checked): the label's LAST
         ;; slot must admit the branch value. The cycle does NOT
         ;; push the label types back — the ref LEFT with the branch.
         (let* ((branch-val (if (eq op :br-on-cast) dst r))
                (frame (%ctrl-label fv (imm :label) (string op)))
                (lt (%label-types frame)))
           (unless (and (consp lt) (plusp (length lt)))
             (verr "~a: label ~a carries no reference slot"
                   op (imm :label)))
           ;; the label's LAST slot must be a REFTYPE admitting the
           ;; branch value (an i32/i64 label cannot take the cast ref)
           (let ((last-slot (first (last lt))))
             (unless (reftype-p last-slot)
               (verr "~a: label ~a's last slot ~a is not a reference"
                     op (imm :label) last-slot))
             (unless (or (eq branch-val :bot)
                         (wasmdown.types:reftype-sub-p
                          branch-val last-slot types
                          (vctx-canon (fv-ctx fv))))
               (verr "~a: branch value ~a does not match label ~a"
                     op branch-val lt))))
         ;; fallthrough keeps the complement: the original operand
         ;; for br_on_cast (cast failed), the narrowed target for
         ;; br_on_cast_fail (cast succeeded).
         (%push-val fv (if (eq op :br-on-cast-fail) dst r))))
      (:ref.eq
       (%pop-val-expect fv (%ref-of :eq))
       (%pop-val-expect fv (%ref-of :eq))
       (%push-val fv :i32))
      (:any.convert-extern
       (%pop-val-expect fv :externref)
       (%push-val fv (%ref-of :any)))
      (:extern.convert-any
       (%pop-val-expect fv (%ref-of :any))
       (%push-val fv :externref))
      (:ref.i31
       (%pop-val-expect fv :i32)
       (%push-val fv (list :ref :i31)))
      ((:i31.get-s :i31.get-u)
       (%pop-val-expect fv (%ref-of :i31))
       (%push-val fv :i32)))))

(defun %local-initialized-p (fv localidx)
  (let ((v (fv-inits fv)))
    (or (not v)
        (plusp (aref v localidx)))))

(defun %mark-local-initialized (fv localidx)
  "local.set/tee of LOCALIDX: the local becomes definitely
initialized from this point (union/monotone — set bits never clear)."
  (let ((v (fv-inits fv)))
    (when v (setf (aref v localidx) 1))))

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
           (cond
            ((getf imms :else-body)
             (%push-ctrl fv :else in out)
             (validate-seq fv (getf imms :else-body))
             (%pop-ctrl fv))
            ;; spec: a missing else behaves as an EMPTY else arm,
            ;; which is valid iff params = results (the params left
            ;; on the frame's stack ARE the results — the identity
            ;; rule). Otherwise the empty else fails to produce out.
            ((not (equal in out))
             (verr "if without else requires params = results (got ~a -> ~a)"
                   in out)))
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
        ;; --- tail calls (Core 3.0 §6.2) ---
        ;; Like call/call-indirect, plus the callee's RESULT types
        ;; must match the enclosing function's declared results.
        ;; Result matching is PER-ELEMENT subtyping: numeric types
        ;; EQUAL, reference types via reftype-sub-p callee <: caller
        ;; (return_call.wast ::96-98: callee results ((ref null $t))
        ;; <: caller results (funcref) — $t is a function typeuse so
        ;; its nullable ref is below funcref = (ref null func)).
        ((eq op :return-call)
         (let* ((ft (vctx-func-type (fv-ctx fv) (imm :func) "return_call"))
                (results (ctrl-frame-end-types
                          (car (last (fv-ctrls fv))))))
           (unless (%results-sub-p (wasm-func-type-results ft) results
                                    (wasm-module-types
                                     (vctx-module (fv-ctx fv)))
                                    (vctx-canon (fv-ctx fv)))
             (verr "type mismatch: return_call result types ~a do not match function results ~a"
                   (wasm-func-type-results ft) results))
           (%pop-vals fv (wasm-func-type-params ft))
           (%unreachable fv)))
        ((eq op :return-call-indirect)
         (let* ((table (vctx-table-at (fv-ctx fv) (imm :table)
                                      "return_call_indirect"))
                (ft (func-type-at module (imm :type)
                                  "return_call_indirect type"))
                (results (ctrl-frame-end-types
                          (car (last (fv-ctrls fv))))))
           ;; the table's element type must admit function references:
           ;; legacy :funcref, or a typed-funcref reftype BELOW funcref
           ;; ((ref null $t) tables — the subtype direction is
           ;; table-type <: funcref, never the reverse).
           (unless (and (reftype-p (wasm-table-type table))
                        (wasmdown.types:reftype-sub-p
                         (wasm-table-type table) :funcref
                         (wasm-module-types (vctx-module (fv-ctx fv)))))
             (verr "return_call_indirect: table ~a must have type funcref, got ~a"
                   (imm :table) (wasm-table-type table)))
           (unless (%results-sub-p (wasm-func-type-results ft) results
                                    (wasm-module-types
                                     (vctx-module (fv-ctx fv)))
                                    (vctx-canon (fv-ctx fv)))
             (verr "type mismatch: return_call_indirect result types ~a do not match function results ~a"
                   (wasm-func-type-results ft) results))
           (%pop-val-expect fv :i32)
           (%pop-vals fv (wasm-func-type-params ft))
           (%unreachable fv)))
        ;; --- EH (Core 3.0): throw / throw_ref / try_table ---
        ((eq op :throw)
         (let ((ft (vctx-tag-type (fv-ctx fv) (imm :tag) "throw")))
           (%pop-vals fv (wasm-func-type-params ft)))
         (%unreachable fv))
        ((eq op :throw-ref)
         (%pop-val-expect fv :exnref)
         (%unreachable fv))
        ((eq op :try-table)
         ;; try_table blocktype l^* (catch x l)^* (catch_ref x l)^*
         ;; (catch_all l)? (catch_all_ref l)? instr* end. The block
         ;; itself validates like block (params popped, body checked
         ;; in a new frame, results pushed). Each catch clause's
         ;; label must exist and carry the tag's params at branch
         ;; time (catch: tag params; catch_ref: tag params + exnref;
         ;; catch_all: nothing; catch_all_ref: exnref) — the clause
         ;; labels resolve in the ENCLOSING scope (they branch OUT of
         ;; the try block), so the label check runs against the ctrl
         ;; stack AFTER the try's own frame is pushed (the try label
         ;; counts as one depth level, matching the text parser's
         ;; label accounting).
         (multiple-value-bind (in out)
             (valid-blocktype-params-results module (imm :bt))
           (%pop-vals fv in)
           (%push-ctrl fv :try-table in out)
          ;; Clause labels resolve with the try frame on the stack
          ;; (the branch targets are OUTSIDE the try block; the try's
          ;; own label counts one depth level, matching the text
          ;; parser). Each label must carry exactly the payload types
          ;; the clause pushes at dispatch time.
          (loop for c across (imm :catches)
                do (let* ((label-depth (ecase (first c)
                                         ((:catch :catch-ref) (third c))
                                         ((:catch-all :catch-all-ref)
                                          (second c))))
                          (frame (%ctrl-label fv label-depth
                                             "try_table catch clause")))
                     (declare (ignore frame))
                     (let ((want
                             (ecase (first c)
                               (:catch
                                (wasm-func-type-params
                                 (vctx-tag-type (fv-ctx fv) (second c)
                                                "try_table catch")))
                               (:catch-ref
                                (append
                                 (wasm-func-type-params
                                  (vctx-tag-type (fv-ctx fv) (second c)
                                                 "try_table catch_ref"))
                                 (list :exnref)))
                               (:catch-all '())
                               (:catch-all-ref (list :exnref)))))
                       (%check-label-types fv label-depth want))))
           (validate-seq fv (wasm-instr-body instr))
           (%pop-ctrl fv)
           (%push-vals fv out)))
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
                    ;; typed-ref heaps must resolve ('unknown type',
                    ;; ref.wast's select case)
                    (when (and (reftype-p t_) (consp t_))
                      (check-valtype t_ (wasm-module-types module)
                                     "select"))
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
         (let ((x (imm :local)))
           ;; definite initialization (local_init.wast): a
           ;; non-defaultable local must be proven set on every path
           ;; before get ('uninitialized local' assert_invalid).
           (let ((t_ (%local-type fv x)))
             (unless (or (wasmdown.types:defaultable-valtype-p t_)
                         (%local-initialized-p fv x))
               (verr "uninitialized local ~a" x))
             (%push-val fv t_))))
        ((eq op :local.set)
         (let ((x (imm :local)))
           (%pop-val-expect fv (%local-type fv x))
           (%mark-local-initialized fv x)))
        ((eq op :local.tee)
         (let* ((x (imm :local))
                (t_ (%local-type fv x)))
           (%pop-val-expect fv t_)
           (%mark-local-initialized fv x)
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
           ;; the ELEMKIND form's segment type is the target table's
           ;; element type (check-elems' rule; table.init consumes it)
           (when (eq ert :elemkind-func)
             (setf ert (wasm-table-type table)))
           (unless (or (eq ert (wasm-table-type table))
                       (wasmdown.types:reftype-sub-p
                        ert (wasm-table-type table)
                        (wasm-module-types module)
                        (vctx-canon (fv-ctx fv))))
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
         (let ((ht (imm :type))
               (types (wasm-module-types module)))
           (cond
            ((member ht '(:funcref :externref :exnref))
             (%push-val fv ht))
            ;; typed heap: (:type N) or abstract keyword; the result
            ;; is the NULLABLE typed ref (:ref null ht)
            ((or (keywordp ht) (consp ht))
             (let ((v (list :ref 'null ht)))
               (unless (and (reftype-p v)
                            (valid-heaptype-p ht types))
                 (verr "ref.null: invalid heap type ~a" ht))
               (%push-val fv v)))
            (t (verr "ref.null: invalid heap type ~a" ht)))))
        ((eq op :ref.is-null)
         (%pop-ref fv)
         (%push-val fv :i32))
        ((eq op :ref.func)
         (let ((x (imm :func)))
           (vctx-func-type (fv-ctx fv) x "ref.func")
           (unless (gethash x (vctx-declared-refs (fv-ctx fv)))
             (verr "ref.func: function ~a is not declared by an element segment or export"
                   x))
           ;; spec: [t1*] -> [(ref $t)] with $t the function's own
           ;; typeuse — the minimal type (call_ref.wast's local.set
           ;; into (ref $ii) locals validates via reftype-sub-p).
           (let ((typeidx (svref (vctx-func-typeidxs (fv-ctx fv)) x)))
             (%push-val fv (list :ref (list :type typeidx))))))
        ;; --- GC value opcodes (loop 22): the ~30-opcode family
        ;; lives in VALIDATE-GC-INSTR (a separate defun — keeping
        ;; this cond small); the dispatch consults the op table.
        ((gethash op *gc-opcodes*)
         (validate-gc-instr fv op imms module))
        ;; --- typed function references (Core 3.0) ---
        ((eq op :ref.as-non-null)
         ;; [(ref null ht)] -> [(ref ht)]: legacy keywords are their
         ;; nullable spellings (funcref = (ref null func)), so the
         ;; non-null view of a funcref/externref/exnref operand is
         ;; (:ref :func)/(:ref :extern)/(:ref :exn).
         (%push-val fv (%non-null-view (%pop-ref fv) "ref.as_non_null")))
        ((eq op :br-on-null)
         ;; br_on_null l : [t* (ref null ht)] -> [t* (ref ht)]
         ;; fallthrough; the branch label l carries just [t*]
         ;; (br_on_null.wast ::4-9; a funcref operand is legal —
         ;; ::88). gc#516: the label check runs through the full
         ;; pop/push cycle AND pushes back the DECLARED label types
         ;; — the reference's label join re-types the prefix to the
         ;; label's declared [t*] (a (ref null $t) prefix slot that
         ;; the label declares funcref comes back as funcref, and
         ;; the downstream call's (ref null $t) expectation fails:
         ;; ::81-90's assert_invalid 'type mismatch').
         (let* ((frame (%ctrl-label fv (imm :label) "br_on_null"))
                (r (%pop-ref fv))
                (label-types (%label-types frame)))
           (%pop-vals fv label-types)
           (%push-vals fv label-types)
           (%push-val fv (%non-null-view r "br_on_null"))))
        ((eq op :br-on-non-null)
         ;; br_on_non_null l : [t* (ref null ht)] -> [t*] fallthrough
         ;; (the null case); the branch label l carries [t* (ref ht)]
         ;; — the label's LAST slot is satisfied by the operand's
         ;; non-null view (br_on_non_null.wast ::57-61). gc#516: the
         ;; label check runs the full pop/push cycle pushing the
         ;; DECLARED label types back (same join rule as br_on_null:
         ;; ::91-103's assert_invalid).
         (let* ((frame (%ctrl-label fv (imm :label) "br_on_non_null"))
                (r (%pop-ref fv))
                (nn (%non-null-view r "br_on_non_null"))
                (label-types (%label-types frame)))
           ;; arity sanity: the label must have at least the ref slot
           (unless (and (consp label-types) (plusp (length label-types)))
             (verr "br_on_non_null: label ~a carries no reference slot"
                   (imm :label)))
           (%push-val fv nn)
           (%pop-vals fv label-types)
           (%push-vals fv label-types)
           ;; fallthrough [t*]: the ref is consumed by the branch
           (%pop-val fv)))
        ((eq op :call-ref)
         ;; [t1* (ref null? $t)] -> [t2*] where the typeuse $t is
         ;; [t1*] -> [t2*]. The ref operand must be <: (ref null $t)
         ;; — a bare :funcref is REJECTED (call_ref.wast ::210-217:
         ;; funcref is not a subtype of the typeuse's ref).
         (let* ((ft (func-type-at module (imm :type) "call_ref type"))
                (params (wasm-func-type-params ft))
                (results (wasm-func-type-results ft))
                (want (list :ref 'null (list :type (imm :type)))))
           (%pop-val-expect fv want)
           (%pop-vals fv params)
           (%push-vals fv results)))
        ((eq op :return-call-ref)
         ;; call_ref's typing plus the tail-call rule: the callee's
         ;; results must be SUBTYPES of the caller's declared results
         ;; (return_call_ref.wast ::220-228's mixed nullable/non-null
         ;; lattice batch), then unreachable (mirrors :return-call).
         (let* ((ft (func-type-at module (imm :type)
                                   "return_call_ref type"))
                (params (wasm-func-type-params ft))
                (results (wasm-func-type-results ft))
                (want (list :ref 'null (list :type (imm :type))))
                (types (wasm-module-types module))
                (caller-results (ctrl-frame-end-types
                                 (car (last (fv-ctrls fv))))))
           (unless (and (= (length results) (length caller-results))
                        (every (lambda (r cr)
                                 (or (equal r cr)
                                     (and (reftype-p r) (reftype-p cr)
                                          (wasmdown.types:reftype-sub-p
                                           r cr types
                                           (vctx-canon (fv-ctx fv))))))
                               results caller-results))
             (verr "type mismatch: return_call_ref result types ~a do not match function results ~a"
                   results caller-results))
           (%pop-val-expect fv want)
           (%pop-vals fv params)
           (%unreachable fv)))
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
  (let ((fv (make-fv :ctx ctx :locals locals :vals nil :ctrls nil
                     ;; definite initialization: track set-bits only
                     ;; when some local is non-defaultable (nil inits
                     ;; = every local assumed initialized, the
                     ;; historical fast path).
                     :inits (unless (%init-all-defaultable-p locals)
                              (let ((v (make-array (length locals)
                                                   :element-type 'bit
                                                   :initial-element 0)))
                                ;; params arrive with arguments: always
                                ;; initialized (local_init.wast's param
                                ;; carriers must validate).
                                (loop for i below (length params)
                                      do (setf (aref v i) 1))
                                v)))))
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
    (check-tags ctx)
    (check-tables-and-mems ctx)
    (check-globals ctx)
    (check-elems ctx)
    (check-datas ctx)
    (check-exports module ctx)
    (check-start module ctx)
    (check-datacount module)
    (validate-all-funcs module ctx))
  t)

(defun %comptype-valtypes (comptype)
  "The valtype lists a comptype exposes (validate-side mirror of the
types helper): params+results for a func, field types for struct."
  (ecase (wasmdown.types:wasm-comptype-kind comptype)
    (:func (append (wasmdown.types:wasm-comptype-params comptype)
                   (wasmdown.types:wasm-comptype-results comptype)))
    ((:struct :array)
     (mapcar #'first (wasmdown.types:wasm-comptype-fields comptype)))))

(defun check-types-section (module)
  "Type-section well-formedness, RECURSION-GROUP AWARE:
  1. every exposed valtype is a valtype;
  2. every typed-ref (:TYPE N) heap is in range ('unknown type',
     ref.wast ::27-62);
  3. REC FORWARD-REFERENCE RULE (type-equivalence.wast ::76-82,
     type-rec.wast ::21-33): a typeuse inside type I may reference
     only members of I's OWN rec group or types with index <
     GROUP-START — a reference to a LATER type outside the group
     is 'unknown type' (the whole point of (rec ...): only group
     members may be mutually recursive)."
  (let ((types (wasm-module-types module)))
    (loop for ft across types
          for i from 0
          do (let* ((rec-p (wasmdown.types:wasm-rec-type-p ft))
                    (gstart (if rec-p
                                (wasmdown.types:wasm-rec-type-group-start ft)
                                i))
                    (grp (if rec-p (wasmdown.types:wasm-rec-type-group ft)
                             nil))
                    (vs (if rec-p
                            (%comptype-valtypes
                             (wasmdown.types:wasm-rec-type-comptype ft))
                            (append (wasm-func-type-params ft)
                                    (wasm-func-type-results ft)))))
               (loop for t_ in vs
                     unless (or (valtype-p t_)
                                ;; packed storagetypes (loop 22,
                                ;; binary/types.html storagetype):
                                ;; legal ONLY in struct/array FIELD
                                ;; position — %COMPTYPE-VALTYPES
                                ;; exposes exactly those.
                                (member t_ '(:i8 :i16)))
                       do (verr "type ~a: invalid value type ~a" i t_)
                     ;; (:TYPE N) heaps must resolve in range
                     when (and (reftype-p t_) (consp t_)
                               (not (valid-heaptype-p
                                     (wasmdown.types:typed-ref-heap t_)
                                     types)))
                       do (verr "type ~a: unknown type ~a" i
                                (second
                                 (wasmdown.types:typed-ref-heap t_)))
                     ;; forward-reference bound: the target must be
                     ;; in the entry's own rec group or STRICTLY
                     ;; BEFORE the group's start (an abstract heap
                     ;; like :any is not a typeuse — skip it)
                     when (and rec-p (reftype-p t_) (consp t_)
                               (consp (wasmdown.types:typed-ref-heap t_))
                               (let ((tgt (second
                                           (wasmdown.types:typed-ref-heap
                                            t_))))
                                 (and (integerp tgt) (>= tgt 0)
                                      (< tgt (length types))
                                      (>= tgt gstart)
                                      (not (eq (wasmdown.types:rec-group-of
                                                module tgt)
                                               grp)))))
                       do (verr "type ~a: unknown type ~a" i
                                (second
                                 (wasmdown.types:typed-ref-heap t_))))))))
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
               ;; EH (Core 3.0): a tag import's payload is a typeidx
               ;; whose functype must have EMPTY results.
               (:tag
                (let ((ft (func-type-at-soft module ty)))
                  (unless ft
                    (verr "import ~a: tag type index ~a out of range"
                          i ty))
                  (when (wasm-func-type-results ft)
                    (verr "import ~a: tag type ~a must have empty results"
                          i ty))))
               (t (verr "import ~a: unknown import kind ~a" i kind))))))

(defun check-tags (ctx)
  "EH (Core 3.0): every tag's type must be a functype with EMPTY
results (spec: the tag type is [t*] -> []). Covers both imported and
section tags via the vctx's tag index space; out-of-range typeidxs
surface here as 'unknown type' (the vctx slot holds NIL for them)."
  (loop for ft across (vctx-tags ctx)
        for i from 0
        do (cond
            ((null ft)
             (verr "tag ~a: unknown type" i))
            ((wasm-func-type-results ft)
             (verr "tag ~a: tag type must be empty" i)))))

(defun check-tables-and-mems (ctx)
  (let ((module (vctx-module ctx))
        (types (wasm-module-types (vctx-module ctx))))
    (loop for table across (wasm-module-tables module)
          for i from 0
          do (progn
               (unless (reftype-p (wasm-table-type table))
                 (verr "table ~a: invalid element type ~a"
                       i (wasm-table-type table)))
               ;; typed-ref heaps must resolve ('unknown type',
               ;; ref.wast's table case)
               (when (consp (wasm-table-type table))
                 (check-valtype (wasm-table-type table) types
                                (format nil "table ~a" i)))
               ;; a DEFINED table without an initializer expression
               ;; must have a DEFAULTABLE element type: the implicit
               ;; fill is null, so a non-nullable typed element type
               ;; rejects (table.wast ::70-80's (table 0 (ref func))
               ;; assert_invalid batch — min=0 included). A table
               ;; WITH an inline initializer desugars to a
               ;; from-table-init elem segment, which exempts it.
               (unless (or (wasmdown.types:defaultable-valtype-p
                            (wasm-table-type table))
                           ;; imported tables carry no initializer
                           ;; requirement (the instance provides
                           ;; elements) — only DEFINED tables here
                           ;; (the tables vector is the module's own)
                           (loop for e across (wasm-module-elems module)
                                 thereis (and (wasmdown.types:wasm-elem-from-table-init e)
                                              (consp (wasm-elem-mode e))
                                              (eq (first (wasm-elem-mode e))
                                                  :active)
                                              (= (second (wasm-elem-mode e))
                                                 (+ i (imported-kind-count
                                                       module :table))))))
                 (verr "table ~a: non-defaultable element type ~a requires an initializer"
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
imported or previously-defined globals (spec note on const exprs).
The init's type must be a SUBTYPE of the declared type
(wasmdown.types:reftype-sub-p — ref_null.wast's (global (ref null $t)
\(ref.null nofunc)) and funcref globals)."
  (let ((module (vctx-module ctx))
        (nimported (vctx-imported-global-count ctx))
        (types (wasm-module-types (vctx-module ctx))))
    (loop for g across (wasm-module-globals module)
          for i from 0
          for limit = (+ nimported i)     ; exclusive bound
          do (progn
               (when (consp (wasm-global-type g))
                 (check-valtype (wasm-global-type g) types
                                (format nil "global ~a" i)))
               (let ((type (const-expr-type ctx (wasm-global-init g)
                                               :global-limit limit
                                               :what (format nil
                                                             "global ~a init"
                                                             i))))
                 (unless (or (equal type (wasm-global-type g))
                             (and (reftype-p type)
                                  (reftype-p (wasm-global-type g))
                                  (wasmdown.types:reftype-sub-p
                                   type (wasm-global-type g) types
                                   (vctx-canon ctx))))
                   (verr "global ~a: init expression type ~a does not match declared type ~a"
                         i type (wasm-global-type g))))))))

(defun check-elems (ctx)
  "Element segments: item init types must be SUBTYPES of the segment
type (reftype-sub-p), and an ACTIVE segment's type a subtype of its
table's type (spec: elem type <: table type; elem.wast ::516-536's
\(elem (i32.const 0) funcref (ref.func 0)) into a (ref func) table is
REJECTED — funcref (nullable) is not <: (ref func))."
  (let ((module (vctx-module ctx))
        (types (wasm-module-types (vctx-module ctx))))
    (loop for elem across (wasm-module-elems module)
          for i from 0
          do (let ((mode (wasm-elem-mode elem))
                   (etype (wasm-elem-type elem)))
               ;; the ELEMKIND form ('func' / bare funcidx lists): the
               ;; segment type is the TARGET TABLE's element type when
               ;; active (elem.wast ::51's (elem (i32.const 0) func 0)
               ;; into a (ref func) table VALIDATES; the explicit
               ;; funcref spelling of the same shape is assert_invalid
               ;; ::516). Items carry their own minimal types and are
               ;; checked against the table's type directly.
               (when (eq etype :elemkind-func)
                 (let ((table-type
                         (if (and (consp mode) (eq (first mode) :active))
                             (wasm-table-type
                              (vctx-table-at ctx (second mode)
                                             (format nil
                                                     "element segment ~a"
                                                     i)))
                             :funcref)))
                   (setf etype table-type)))
               (unless (reftype-p etype)
                 (verr "element segment ~a: invalid element type ~a"
                       i etype))
               (when (consp etype)
                 (check-valtype etype types
                                (format nil "element segment ~a" i)))
               (loop for expr in (wasm-elem-inits elem)
                     for j from 0
                     do (let ((type (const-expr-type
                                     ctx expr
                                     ;; Items of a table-field
                                     ;; initializer segment may only
                                     ;; reference IMPORTED globals
                                     ;; (function-references extended
                                     ;; syntax; global.wast ::120
                                     ;; rejects defined-global
                                     ;; global.get at VALIDATE).
                                     :global-limit
                                     (when (wasmdown.types:wasm-elem-from-table-init elem)
                                       (vctx-imported-global-count ctx))
                                     :what (format nil
                                                   "element segment ~a item ~a"
                                                   i j))))
                          (unless (or (equal type etype)
                                      (and (reftype-p type)
                                           (wasmdown.types:reftype-sub-p
                                            type etype types
                                            (vctx-canon ctx))))
                            (verr "element segment ~a item ~a: type ~a does not match segment type ~a"
                                  i j type etype))))
               (when (and (consp mode) (eq (first mode) :active))
                 (let ((tableidx (second mode))
                       (expr (third mode)))
                   (let ((table (vctx-table-at ctx tableidx
                                               (format nil
                                                       "element segment ~a"
                                                       i))))
                     (unless (or (eq etype (wasm-table-type table))
                                 (wasmdown.types:reftype-sub-p
                                  etype (wasm-table-type table) types
                                  (vctx-canon ctx)))
                       (verr "element segment ~a: type ~a does not match table ~a type ~a"
                             i etype tableidx (wasm-table-type table))))
                   (let ((type (const-expr-type
                                ctx expr
                                ;; spec: elem/data offsets may use
                                ;; global.get of ANY immutable global
                                ;; (defined included — data.wast ::9/10,
                                ;; elem.wast ::15/16, global.wast
                                ;; ::113): the sequential
                                ;; imported-or-previous restriction
                                ;; applies only to global INIT exprs.
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
                                ;; as with elem offsets: any immutable
                                ;; global is legal here
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
               (unless (member kind '(:func :table :memory :global :tag))
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
    ;; EH (Core 3.0): tags share the imports-first index space.
    (:tag (< -1 index (length (vctx-tags ctx))))
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
             ;; typed-ref heaps in declared locals must resolve
             ;; ('unknown type', ref.wast's local case)
             (dolist (entry (wasm-func-locals f))
               (check-valtype-list (wasm-module-types module)
                                   (format nil "function ~a local"
                                           funcidx)
                                   (list (car entry))))
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
