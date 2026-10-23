;;;; Decoded-module IR: the cross-slice data contract between binary,
;;;; validate, runtime, interp and codegen (PLAN.md §5.3 DecodedModule).
;;;;
;;;; Representation policy:
;;;; - Value types are the keywords :i32 :i64 :f32 :f64 :v128 and
;;;;   reference types as lists (:ref null|no-null heaptype-keyword).
;;;; - Instructions are records {op, imms, body, offset}:
;;;;     op     - opcode keyword, e.g. :i32.add, :br, :block
;;;;     imms   - plist of immediate values (labelidx etc.)
;;;;     body   - (vector instr) for structured ops only
;;;;     offset - absolute byte offset in the original binary (source map)
;;;; - Expressions are (vector instr), terminated by :end.
(in-package #:wasmdown.types)

(defstruct (wasm-instr (:constructor make-wasm-instr (op imms &optional body offset)))
  "One decoded instruction."
  (op :nop :type keyword)
  (imms nil :type list)
  (body #() :type (vector wasm-instr))
  (offset 0 :type (integer 0)))

(defstruct (wasm-func-type (:constructor make-wasm-func-type (params results)))
  "A function type: params and results are lists of value types."
  params
  results)

(defstruct (wasm-limits (:constructor make-wasm-limits (min &optional (max nil max-p) &aux (has-max max-p))))
  (min 0 :type (integer 0))
  (max nil :type (or null (integer 0)))
  (has-max nil :type boolean))

(defun wasm-limits-has-max-p (limits)
  "Exported alias for the HAS-MAX slot reader."
  (wasm-limits-has-max limits))

(defstruct (wasm-table (:constructor make-wasm-table (type limits)))
  "Table type: element reference type plus limits."
  type
  limits)

(defstruct (wasm-memory (:constructor make-wasm-memory (limits))
                        (:predicate wasm-memory-p))
  "Memory type: limits (memory64 excluded from this slice: 32-bit only)."
  limits)

(defstruct wasm-global
  "A global definition from the globals section."
  (type :i32 :type keyword)
  (mutable nil :type boolean)
  (init nil :type (vector wasm-instr)))

(defstruct (wasm-import (:constructor make-wasm-import (module field kind type)))
  "One import. KIND is :func/:table/:memory/:global; TYPE is the typeidx or type."
  module
  field
  kind
  type)

(defstruct (wasm-export (:constructor make-wasm-export (name kind index)))
  name
  kind
  index)

(defstruct (wasm-func (:constructor make-wasm-func (typeidx locals body offset)))
  "A defined function body from the code section."
  typeidx
  locals                                 ; list of (valtype . count)
  body                                   ; (vector instr)
  offset)                                ; byte offset of body start in binary

(defstruct (wasm-elem (:constructor make-wasm-elem (mode type inits &optional from-table-init)))
  "Element segment. MODE: (:active tableidx expr) | (:passive) | (:declarative).
TYPE is the reference valtype; INITS is a list of expressions (vectors of instr).
FROM-TABLE-INIT marks a segment desugared from a table field's inline
initializer expr (the function-references extended syntax): per the
reference its const exprs may only use IMPORTED globals (global.wast
::120 expects 'unknown global' at VALIDATION for a defined-global
global.get)."
  mode
  type
  inits
  from-table-init)

(defstruct (wasm-data (:constructor make-wasm-data (mode bytes)))
  "Data segment. MODE: (:active memidx expr) | (:passive)."
  mode
  bytes)

(defstruct wasm-custom
  "Custom section kept as inert bytes (PLAN.md §5.1)."
  (name "" :type string)
  (bytes #() :type (vector (unsigned-byte 8)))
  (offset 0 :type (integer 0)))


(defstruct (wasm-module (:constructor %make-wasm-module))
  "A fully decoded (not yet validated) module."
  (types #() :type (vector wasm-func-type))
  (imports #() :type (vector wasm-import))
  (funcs #() :type (vector wasm-func))
  (tables #() :type (vector wasm-table))
  (mems #() :type (vector wasm-memory))
  ;; tag section (id 13): vector of typeidx; tag i's type is
  ;; (aref types (svref tags i)) and MUST be a functype with empty
  ;; results (validated in wasmdown.validate:check-tags).
  (tags #() :type (vector (integer 0)))
  (globals #() :type (vector wasm-global))
  (exports #() :type (vector wasm-export))
  (start nil :type (or null (integer 0)))
  (elems #() :type (vector wasm-elem))
  (datas #() :type (vector wasm-data))
  (customs #() :type (vector wasm-custom))
  (data-count nil :type (or null (integer 0))))

(defun make-wasm-module (&key (types #()) (imports #()) (funcs #()) (tables #())
                             (mems #()) (tags #()) (globals #()) (exports #())
                             (start nil) (elems #()) (datas #()) (customs #())
                             (data-count nil))
  (%make-wasm-module :types types :imports imports :funcs funcs :tables tables
                     :mems mems :tags tags :globals globals :exports exports
                     :start start :elems elems :datas datas :customs customs
                     :data-count data-count))


(defun func-type-arity-matches-p (ft params results)
  "Function-type equality on lists of value types."
  (and (equal (wasm-func-type-params ft) params)
       (equal (wasm-func-type-results ft) results)))

(defun valtype-width (t_)
  (declare (ignore t_))
  (ecase t_
    (:i32 32) (:i64 64) (:f32 32) (:f64 64)))

(defun typed-ref-p (t_)
  "A typed reference value type: (:REF HEAP) non-null or
(:REF NULL HEAP) nullable. HEAP is a keyword abstract heap type or
(:TYPE N) for a typeuse index. Legacy reftypes (:FUNCREF :EXTERNREF
:EXNREF) stay bare keywords."
  (and (consp t_) (eq (first t_) :ref)
       (or (= (length t_) 2) (and (= (length t_) 3)
                                  (eq (second t_) 'null)))))

(defun typed-ref-nullable-p (t_)
  "Whether the typed reftype T_ is the nullable (:REF NULL HEAP) form."
  (and (typed-ref-p t_) (= (length t_) 3)))

(defun typed-ref-heap (t_)
  "The typed reftype T_'s heap type."
  (if (typed-ref-nullable-p t_) (third t_) (second t_)))

(defun %heap-kind (heap types)
  "The lattice bucket of HEAP in the type vector TYPES:
:FUNC (function heap), :EXTERN, :EXN, :ANY, or :BOT (the null
bottoms, which are below everything in their bucket only)."
  (declare (type (or keyword (cons (eql :type) (cons t null))) heap)
           (type (simple-vector) types))
  (etypecase heap
    (keyword (ecase heap
               ((:func :nofunc) :func)
               ((:extern :noextern) :extern)
               ((:exn :noexn) :exn)
               (:any :any)
               (:none :bot)
               (:eq :any)))
    (cons
     (let ((idx (second heap)))
       (cond ((not (and (integerp idx) (<= 0 idx) (< idx (length types))))
              (error "unknown type ~a" idx))
             (t :func))))))

(defun reftype-sub-p (sub super types)
  "Whether reftype SUB is a subtype of SUPER in the type vector
TYPES (Core 3.0 function-references lattice, rec-free slice):
legacy keywords and typed refs mix; null <: nullable-everything-in-
bucket; (:TYPE N) <: :FUNC (every typeuse is a function heap type in
this slice). Equality is a subtype of itself."
  (labels ((bucket (t_)
             (etypecase t_
               (keyword (ecase t_
                          (:funcref :func) (:externref :extern) (:exnref :exn)))
               (cons (%heap-kind (typed-ref-heap t_) types)))))
    (let ((s sub) (u super))
      (cond
       ((equal s u) t)
       ;; typed <: typed-nullable of the same bucket
       ((and (consp s) (typed-ref-p s)
             (consp u) (typed-ref-nullable-p u)
             (eq (bucket s) (bucket u)))
        t)
       ;; legacy keyword <: typed-nullable of the same bucket:
       ;; funcref <: (:ref null func)
       ((and (keywordp s)
             (consp u) (typed-ref-nullable-p u)
             (eq (bucket s) (bucket u)))
        t)
       ;; typed non-null <: legacy keyword: (:ref func) <: funcref
       ((and (consp s) (typed-ref-p s) (not (typed-ref-nullable-p s))
             (keywordp u)
             (eq (bucket s) (bucket u))
             (eq (typed-ref-heap s)
                 (ecase u (:funcref :func) (:externref :extern)
                        (:exnref :exn))))
        t)
       (t nil)))))

(defun defaultable-valtype-p (t_)
  "Whether the default value exists for the type in this slice.
Numeric types and NULLABLE refs default; NON-NULLABLE typed refs do
not (spec: default values need a null; the definite-initialization
validator tracks local.set coverage for those)."
  (not (and (consp t_) (typed-ref-p t_)
            (not (typed-ref-nullable-p t_)))))

