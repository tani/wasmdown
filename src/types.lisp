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
  (type :i32 :type (or keyword cons))
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
TYPE is the reference valtype, or the special marker :ELEMKIND-FUNC
for the binary elemkind/text 'func' forms whose segment type the
reference VALIDATOR treats as the target table's element type when
active (elem.wast ::51: (elem (i32.const 0) func 0) into a
\(ref func) table VALIDATES, while the explicit funcref spelling of
the same segment is assert_invalid ::516). INITS is a list of
expressions (vectors of instr). FROM-TABLE-INIT marks a segment
desugared from a table field's inline initializer expr (the
function-references extended syntax): per the reference its const
exprs may only use IMPORTED globals (global.wast ::120 expects
'unknown global' at VALIDATION for a defined-global global.get)."
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
  "The lattice bucket of HEAP in the type vector TYPES: :FUNC
\(function heap), :EXTERN, :EXN, or :ANY. The null bottoms map to
their buckets (:nofunc→:func etc; :none→:ANY — none is the bottom of
the whole ANY hierarchy, ref_null.wast's (global anyref
\(ref.null none))) and relate downward via %HEAP-SUB-P's bottom arm."
  (declare (type (or keyword (cons (eql :type) (cons t null))) heap)
           (type (simple-vector) types))
  (etypecase heap
    (keyword (ecase heap
               ((:func :nofunc) :func)
               ((:extern :noextern) :extern)
               ((:exn :noexn) :exn)
               (:any :any)
               (:none :any)
               (:eq :any)))
    (cons
     (let ((idx (second heap)))
       (cond ((not (and (integerp idx) (<= 0 idx) (< idx (length types))))
              ;; an out-of-range typeuse heap ('unknown type',
              ;; ref.wast's assert_invalid batch): a CATCHABLE
              ;; validation-error, never a raw Lisp error leaking
              ;; through the wast harness as harness-error
              ;; (func.wast ::113 regressed here when validate
              ;; gained typed-ref lattice calls before its own
              ;; check-valtype pass).
              (error 'wasmdown:validation-error
                     :message (format nil "unknown type ~a" idx)))
             (t :func))))))

(defun %heap-sub-p (h1 h2 types)
  "Heap subtyping in the rec-free slice (no declared subtyping): h1
<: h2 iff
  - h1 = h2 (nominal equality — typeuse vs typeuse by INDEX), OR
  - h1 is a typeuse and h2 is the bucket's abstract top keyword
    (:func/:extern/:exn) — every typeuse is below its bucket top, OR
  - h1 is a bucket's null bottom (:nofunc/:noextern/:noexn — and
    :none, the ANY hierarchy's bottom: (:ref null :none) <:
    (:ref null :any), ref_null.wast module 1's (global anyref
    (ref.null none))) and h2 is anything in the same bucket (typeuse
    or top).
Cross-typeuse (:TYPE N) <: (:TYPE M), N≠M, is NOT a subtype
(return_call_ref $t with a (ref $u) operand, $u≠$t nominal: the
sibling's repro pin — assert_invalid 'type mismatch')."
  (declare (type (or keyword (cons (eql :type) (cons t null))) h1 h2)
           (type (simple-vector) types))
  (labels ((kind (h) (%heap-kind h types)))
    (cond
     ((equal h1 h2) t)
     ;; typeuse <: abstract top of its bucket
     ((and (consp h1) (keywordp h2)
           (member h2 '(:func :extern :exn))
           (eq (kind h1) (kind h2)))
      t)
     ;; null bottom <: anything in its bucket
     ((and (keywordp h1)
           (member h1 '(:nofunc :noextern :noexn :none))
           (eq (kind h1) (kind h2)))
      t)
     (t nil))))

(defun %expand-reftype (t_)
  "The canonical (REF NULL? HEAP) form of a reftype: legacy keywords
expand to their bucket-tops (:funcref = (ref null func) etc.), typed
refs pass through. Values of either spelling compare uniformly."
  (etypecase t_
    (keyword
     (list :ref 'null
           (ecase t_
             (:funcref :func) (:externref :extern) (:exnref :exn))))
    (cons t_)))

(defun reftype-sub-p (sub super types)
  "Whether reftype SUB is a subtype of SUPER in the type vector TYPES
(Core 3.0 function-references lattice, rec-free slice). Both sides
expand to canonical (REF NULL? HEAP) form (a legacy keyword IS its
bucket's nullable top), then: equality, or NULLABILITY ONLY WIDENS
\(nullable <: nullable and non-null <: nullable pass; nullable <:
non-null fails) AND the heaps relate by %HEAP-SUB-P. This admits
(ref $t) <: funcref, (ref null $t) <: funcref, funcref <:
(ref null func), (ref null nofunc) <: (ref null $t); rejects
(ref (:type 1)) <: (ref null (:type 0)) — nominal typeuses compare
by equality only."
  (let ((s (%expand-reftype sub))
        (u (%expand-reftype super)))
    (or (equal s u)
        (and (or (not (typed-ref-nullable-p s))
                 (typed-ref-nullable-p u))
             (%heap-sub-p (typed-ref-heap s) (typed-ref-heap u)
                          types)))))

(defun defaultable-valtype-p (t_)
  "Whether the default value exists for the type in this slice.
Numeric types and NULLABLE refs default; NON-NULLABLE typed refs do
not (spec: default values need a null; the definite-initialization
validator tracks local.set coverage for those)."
  (not (and (consp t_) (typed-ref-p t_)
            (not (typed-ref-nullable-p t_)))))

