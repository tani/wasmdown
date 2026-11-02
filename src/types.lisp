;;;; Decoded-module IR: the cross-slice data contract between binary,
;;;; validate, runtime, interp and codegen (docs/design/PLAN.md §5.3 DecodedModule).
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

(defvar wasmdown:*threads* nil
  "Opt-in threads-proposal gate. NIL (the default) keeps the core
engine's behavior: the binary decoder rejects the shared limits flag,
the text parser rejects `shared`, and atomic instructions do not
decode/parse/validate. Bind T (or pass the CLI's --threads) to enable
shared memories, the 0xFE atomic instruction family and the
memory.atomic.wait/notify host hooks (wasmdown.threads).")

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

(defstruct (wasm-comptype (:constructor make-wasm-comptype (kind &optional heap fields params results)))
  "A composite type (one rec-group member): KIND :FUNC/:STRUCT/:ARRAY.
Storage is uniform across kinds so the iso-recursive comparator walks
one shape: a FUNC keeps PARAMS/RESULTS (valtype lists); STRUCT/ARRAY
keep FIELDS as (valtype mutable-p) pairs (shapes parsed for
equivalence, never executed — GC value families stay :unsupported at
the wast classifier; only their STATIC matching in type-rec.wast
runs)."
  kind
  heap
  (fields '())
  (params '())
  (results '()))

(defstruct (wasm-rectype-group (:constructor make-wasm-rectype-group (types)))
  "A recursion group: the UNIT of iso-recursive equivalence. TYPES is
a vector of wasm-comptype; all members share this object (EQ)."
  (types #() :type vector))

(defstruct (wasm-rec-type
            (:include wasm-func-type)
            (:predicate wasm-rec-type-p))
  "One type section entry (a typeidx): a comptype that IS a func
type (comptype-kind :func; the included PARAMS/RESULTS slots mirror
the comptype's), or — for GC composites parsed by the text parser
only for STATIC equivalence — a struct/array comptype standing in a
type slot the VALIDATOR rejects for func-typed uses. Including
wasm-func-type means every existing consumer (func-type-at,
preflight, codegen, interp, encode) reads params/results through
the same accessors; for non-func composites those slots are ().
GROUP is the rec group shared (EQ) by every member; GROUP-IDX the
member's position inside it; GROUP-START the typeidx of the group's
first member (the forward-reference bound: a typeuse may reference
its own group's members or any typeidx < GROUP-START — a non-rec
forward ref is 'unknown type' at validation, type-equivalence.wast
::76-82 / type-rec.wast ::21-33)."
  comptype
  group
  (group-idx 0 :type (integer 0))
  (group-start 0 :type (integer 0)))

(defun make-rec-type-entry (comptype group group-idx group-start)
  "Build a rec entry mirroring the comptype's func slots."
  (make-wasm-rec-type
   :params (wasm-comptype-params comptype)
   :results (wasm-comptype-results comptype)
   :comptype comptype :group group :group-idx group-idx
   :group-start group-start))

(defun make-singleton-rec-type (comptype)
  "A rec entry for a comptype defining its OWN singleton group (a
bare (type ...) field outside (rec ...))."
  (make-rec-type-entry comptype
                      (make-wasm-rectype-group (vector comptype)) 0 0))

(defun wrap-func-type-as-rec (fty &optional (idx 0))
  "The rec entry for a plain wasm-func-type built by the binary
decoder or the legacy constructors: its own singleton group (the
uniform rec model — a bare functype definition is a singleton rec
group; a group of one that references ITSELF is a distinct type,
type-rec.wast ::46-49 vs ::51-58). IDX is the entry's typeidx (the
forward-reference bound)."
  (if (wasm-rec-type-p fty)
      fty
      (make-rec-type-entry
       (make-wasm-comptype :func nil '()
                           (wasm-func-type-params fty)
                           (wasm-func-type-results fty))
       (make-wasm-rectype-group
        (vector (make-wasm-comptype :func nil '()
                                    (wasm-func-type-params fty)
                                    (wasm-func-type-results fty))))
       0 idx)))

(defun module-type-comptype (module typeidx)
  "The comptype at TYPEIDX (NIL when out of range)."
  (let ((types (wasm-module-types module)))
    (and (integerp typeidx) (>= typeidx 0) (< typeidx (length types))
         (let ((ty (svref types typeidx)))
           (if (wasm-rec-type-p ty)
               (wasm-rec-type-comptype ty)
               (make-wasm-comptype
                :func nil '()
                (wasm-func-type-params ty)
                (wasm-func-type-results ty)))))))

(defstruct (wasm-limits
            (:constructor make-wasm-limits (min &optional (max nil max-p) (i64-p nil) (shared-p nil) &aux (has-max max-p)))
            (:constructor %make-wasm-limits* (min max has-max &optional (i64-p nil) (shared-p nil))))
  (min 0 :type (integer 0))
  (max nil :type (or null (integer 0)))
  (has-max nil :type boolean)
  ;; memory64 (Core 3.0): the address type is i64 (limits flags bit
  ;; 2 in binary; the i64 marker in text). Memory-only semantics:
  ;; a 64-bit TABLE limits flag decodes but the validator keeps
  ;; rejecting tables (table64 out of scope — honest slice).
  (i64-p nil :type boolean)
  ;; threads proposal (opt-in, wasmdown:*threads*): the shared limits
  ;; flag (binary bit 1 / text `shared`). Shared memories MUST carry a
  ;; maximum (validator); tables can never be shared.
  (shared-p nil :type boolean))

(defun wasm-limits-has-max-p (limits)
  "Exported alias for the HAS-MAX slot reader."
  (wasm-limits-has-max limits))

;;; ---------------------------------------------------------------
;;; Threads proposal: atomic instruction metadata shared by the binary
;;; decoder/encoder, the text parser, the validator and both backends.
;;; KIND is :load/:store/:rmw/:notify/:wait/:fence; OPERATION names the
;;; rmw operation (:add/:sub/:and/:or/:xor/:xchg/:cmpxchg); WIDTH is
;;; the accessed byte width; ALIGN is the mandatory (natural) alignment
;;; exponent; VALUE-TYPE is the operand/result numeric type.

(defstruct (atomic-op-info (:constructor %make-atomic-op-info
                                          (kind width align value-type
                                           &optional operation)))
  kind
  operation
  width
  align
  value-type)

(defparameter *atomic-ops* (make-hash-table :test #'eq))

(flet ((add (op kind width align value-type &optional operation)
         (setf (gethash (intern (string-upcase op) :keyword) *atomic-ops*)
               (%make-atomic-op-info kind width align value-type operation))))
  (loop for (type full-width) in '((:i32 4) (:i64 8))
        for base = (string-downcase (symbol-name type)) do
    ;; loads: full-width has no suffix; narrower loads are <n>_u.
    (loop for width in (if (= full-width 4) '(4 1 2) '(8 1 2 4))
          do (add (if (= width full-width)
                      (format nil "~a.atomic.load" base)
                      (format nil "~a.atomic.load~d_u" base (* 8 width)))
                  :load width (1- (integer-length width)) type))
    ;; stores: full-width plain; sub-width stores have no suffix.
    (loop for width in (if (= full-width 4) '(4 1 2) '(8 1 2 4))
          do (add (if (= width full-width)
                      (format nil "~a.atomic.store" base)
                      (format nil "~a.atomic.store~d" base (* 8 width)))
                  :store width (1- (integer-length width)) type))
    ;; rmw families: the sub-width spelling is rmw8/rmw16 (+ _u), the
    ;; full width is rmw for i32 and rmw32/_u for i64's 4-byte op.
    (dolist (family '((:add "add") (:sub "sub") (:and "and")
                      (:or "or") (:xor "xor") (:xchg "xchg")
                      (:cmpxchg "cmpxchg")))
      (loop for width in (if (= full-width 4) '(4 1 2) '(8 1 2 4))
            do (let* ((sub-width-p (< width full-width))
                      (mid (cond ((= width full-width) "rmw")
                                 ((= width 1) "rmw8")
                                 ((= width 2) "rmw16")
                                 ((= width 4) "rmw32")))
                      (name (format nil "~a.atomic.~a.~a~@[_u~]"
                                    base mid (second family)
                                    (and sub-width-p t))))
                 (add name :rmw
                      width (1- (integer-length width)) type
                      (first family))))))
  (add "memory.atomic.notify" :notify 4 2 :i32)
  (add "memory.atomic.wait32" :wait 4 2 :i32)
  (add "memory.atomic.wait64" :wait 8 3 :i64)
  (add "atomic.fence" :fence 0 0 nil))

(defun atomic-op-info-of (op)
  "The atomic-op-info for OP keyword, or NIL when OP is not atomic."
  (gethash op *atomic-ops*))

(defun atomic-op-p (op)
  (and (gethash op *atomic-ops*) t))

(defparameter *atomic-op-sub*
  ;; Binary 0xFE sub-opcode -> op keyword (threads proposal). The
  ;; wait/notify/fence group is 0x00..0x03; loads 0x10..; stores
  ;; 0x17..; the seven-op rmw families in order add/sub/and/or/xor/
  ;; xchg/cmpxchg (0x1E..0x4E).
  (let* ((h (make-hash-table))
         (add (lambda (sub op)
               (setf (gethash sub h) (intern (string-upcase op) :keyword)))))
    (funcall add #x00 "memory.atomic.notify")
    (funcall add #x01 "memory.atomic.wait32")
    (funcall add #x02 "memory.atomic.wait64")
    (funcall add #x03 "atomic.fence")
    (funcall add #x10 "i32.atomic.load")
    (funcall add #x11 "i64.atomic.load")
    (funcall add #x12 "i32.atomic.load8_u")
    (funcall add #x13 "i32.atomic.load16_u")
    (funcall add #x14 "i64.atomic.load8_u")
    (funcall add #x15 "i64.atomic.load16_u")
    (funcall add #x16 "i64.atomic.load32_u")
    (funcall add #x17 "i32.atomic.store")
    (funcall add #x18 "i64.atomic.store")
    (funcall add #x19 "i32.atomic.store8")
    (funcall add #x1A "i32.atomic.store16")
    (funcall add #x1B "i64.atomic.store8")
    (funcall add #x1C "i64.atomic.store16")
    (funcall add #x1D "i64.atomic.store32")
    (loop for family in '("add" "sub" "and" "or" "xor" "xchg" "cmpxchg")
          for start in '(#x1E #x25 #x2C #x33 #x3A #x41 #x48)
          do (funcall add start (format nil "i32.atomic.rmw.~a" family))
             (funcall add (1+ start) (format nil "i64.atomic.rmw.~a" family))
             (funcall add (+ start 2) (format nil "i32.atomic.rmw8.~a_u" family))
             (funcall add (+ start 3) (format nil "i32.atomic.rmw16.~a_u" family))
             (funcall add (+ start 4) (format nil "i64.atomic.rmw8.~a_u" family))
             (funcall add (+ start 5) (format nil "i64.atomic.rmw16.~a_u" family))
             (funcall add (+ start 6) (format nil "i64.atomic.rmw32.~a_u" family)))
    h)
  "Binary 0xFE sub-opcode -> atomic op keyword.")

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
  "Custom section kept as inert bytes (docs/design/PLAN.md §5.1)."
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
    (:i32 32) (:i64 64) (:f32 32) (:f64 64)
    ;; SIMD (loop 24): the v128 fixed-width type.
    (:v128 128)))

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

(defun %heap-top (heap types)
  "The TOP BUCKET of HEAP in the type vector TYPES: :FUNC, :EXTERN,
:EXN or :ANY (the GC hierarchy — eq/i31/struct/array and their
typeuses — lives under :ANY; docs/design/PLAN.md §3.1 References and GC). Null
bottoms map to their own bucket's top. An out-of-range typeuse
signals the catchable 'unknown type' validation-error (never a raw
Lisp error through the wast harness)."
  (declare (type (or keyword (cons (eql :type) (cons t null))) heap)
           (type (simple-vector) types))
  (etypecase heap
    (keyword (ecase heap
               ((:func :nofunc) :func)
               ((:extern :noextern) :extern)
               ((:exn :noexn) :exn)
               ((:any :eq :i31 :struct :array :none) :any)))
    (cons
     (let ((idx (second heap)))
       (cond ((not (and (integerp idx) (<= 0 idx) (< idx (length types))))
              ;; an out-of-range typeuse heap ('unknown type'):
              ;; a CATCHABLE validation-error, never a raw Lisp
              ;; error leaking through the wast harness as a
              ;; harness-error.
              (error 'wasmdown:validation-error
                     :message (format nil "unknown type ~a" idx)))
             ;; the uniform rec model: a typeuse's bucket is its
             ;; comptype's kind — func/struct/array (loop 22; the
             ;; pre-GC shape of every typeuse being a functype
             ;; keeps :FUNC, so the loop-19 pins are unchanged).
             (t (ecase (wasm-comptype-kind
                        (if (wasm-rec-type-p (svref types idx))
                            (wasm-rec-type-comptype (svref types idx))
                            (make-wasm-comptype
                             :func nil '()
                             (wasm-func-type-params (svref types idx))
                             (wasm-func-type-results (svref types idx)))))
                  (:func :func) (:struct :any) (:array :any))))))))

(defun %heap-kind (heap types)
  "The fine-grained bucket of HEAP: for the GC hierarchy this is the
CONCRETE kind a rank-1 comparison needs — :I31, :STRUCT or :ARRAY for
the abstract keywords, and a typeuse's COMPTYPE KIND (:STRUCT or
:ARRAY; a func typeuse is :FUNC). The null bottoms keep their
bucket-topped spellings. Used only by %HEAP-SUB-P's equal-rank arm."
  (declare (type (or keyword (cons (eql :type) (cons t null))) heap)
           (type (simple-vector) types))
  (etypecase heap
    (keyword (ecase heap
               ((:func :nofunc) :func)
               ((:extern :noextern) :extern)
               ((:exn :noexn) :exn)
               (:any :any)
               (:eq :eq)
               (:none :none)
               (:i31 :i31)
               (:struct :struct)
               (:array :array)))
    (cons
     (let* ((idx (second heap))
            (ct (and (integerp idx) (<= 0 idx) (< idx (length types))
                     (let ((ty (svref types idx)))
                       (if (wasm-rec-type-p ty)
                           (wasm-rec-type-comptype ty)
                           (make-wasm-comptype
                            :func nil '()
                            (wasm-func-type-params ty)
                            (wasm-func-type-results ty)))))))
       (ecase (wasm-comptype-kind ct)
         (:func :func)
         (:struct :struct)
         (:array :array))))))

(defun %heap-rank (heap types)
  "The lattice RANK of HEAP within its top bucket (loop 22, the GC
heap lattice; docs/design/PLAN.md §3.1):
  0 — the null bottoms (:none/:nofunc/:noextern/:noexn)
  1 — CONCRETE heaps: every typeuse (nominal concrete types) and
      the GC's kind keywords :i31/:struct/:array (the immediate
      supertypes of concrete typeuses)
  2 — the bucket TOPS (:func/:extern/:exn) and :eq
  3 — :any (the top of the whole GC hierarchy)
h1 <: h2 (same top bucket) iff rank(h1) < rank(h2), or the ranks
are equal AND the rank-1 kinds match — at rank 1 a TYPEUSE relates
to the abstract kind keyword of its comptype ((ref $s) <:
structref), and never to another typeuse (nominal; the canon-eq arm
in %HEAP-SUB-P handles equivalent typeuses first)."
  (declare (type (or keyword (cons (eql :type) (cons t null))) heap)
           (type (simple-vector) types))
  (etypecase heap
    (keyword (ecase heap
               ((:none :nofunc :noextern :noexn) 0)
               ((:i31 :struct :array) 1)
               ((:func :extern :exn :eq) 2)
               (:any 3)))
    (cons 1)))

(defun %heap-sub-p (h1 h2 types &optional canon)
  "Heap subtyping (no declared subtyping in this slice): h1 <: h2 iff
  - h1 = h2 (nominal equality — typeuse vs typeuse by INDEX), OR
  - CANONICALIZED TYPEUSE EQUALITY (loop 20): with a CANON map
    \(typeidx -> min equivalent typeidx), (:TYPE N) <: (:TYPE M)
    when canon[N] = canon[M]. Without CANON, cross-index typeuses
    are NOT subtypes (the rec-free nominal rule).
  - both are within the SAME top bucket AND rank(h1) < rank(h2):
    every concrete heap/typeuse is below its bucket top (:func,
    :extern, :exn, and — the GC lattice, loop 22 — eq < any,
    i31/struct/array < eq, typeuse-struct < struct < eq < any), and
    the null bottoms (:none/:nofunc/:noextern/:noexn) are below
    everything in their bucket; OR the ranks are equal and the
    rank-1 kinds match ((ref $s) <: structref, (ref $t) <: funcref,
    but i31 is NOT <: struct and (:TYPE a) is NOT <: (:TYPE b)
    nominal).
The heap buckets: i31 <: eq <: any; struct(N) <: eq; array(N) <: eq;
none is the bottom of the whole any hierarchy (ref_null.wast's
\(global anyref (ref.null none)))."
  (declare (type (or keyword (cons (eql :type) (cons t null))) h1 h2)
           (type (simple-vector) types))
  (labels ((canon-eq (n m)
             (and canon
                  (array-in-bounds-p canon n) (array-in-bounds-p canon m)
                  (= (aref canon n) (aref canon m))))
           (same-top-p (a b)
             (eq (%heap-top a types) (%heap-top b types)))
           (below-p (a b)
             (let ((ra (%heap-rank a types))
                   (rb (%heap-rank b types)))
               (cond ((< ra rb) t)
                     ((> ra rb) nil)
                     ;; equal ranks: only rank 1 hosts DISTINCT
                     ;; heaps. A TYPEUSE relates to the abstract
                     ;; kind keyword of its comptype kind —
                     ;; (ref $s) <: structref, (ref $t) <: funcref
                     ;; — and never to another typeuse (nominal;
                     ;; the canon-eq arm already handled
                     ;; equivalents). The abstract keywords
                     ;; :i31/:struct/:array never relate to each
                     ;; other.
                     ((and (= ra 1) (consp a) (consp b)) nil)
                     ((and (= ra 1) (consp a))
                      (eq (%heap-kind a types) b))
                     ((and (= ra 1) (consp b))
                      (eq (%heap-kind b types) a))
                     (t (equal a b))))))
    (cond
     ((equal h1 h2) t)
     ;; canonicalized typeuse <: typeuse (iso-recursive equivalence)
     ((and canon (consp h1) (consp h2)
           (canon-eq (second h1) (second h2)))
      t)
     ;; anything below an abstract/typed supertype within its bucket:
     ;; typeuse <: top keyword, abstract keyword <: higher keyword,
     ;; null bottom <: anything in its bucket
     ((and (same-top-p h1 h2) (below-p h1 h2)) t)
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

(defun reftype-sub-p (sub super types &optional canon)
  "Whether reftype SUB is a subtype of SUPER in the type vector TYPES
(Core 3.0 function-references lattice). Both sides expand to
canonical (REF NULL? HEAP) form (a legacy keyword IS its bucket's
nullable top), then: equality, or NULLABILITY ONLY WIDENS (nullable
<: nullable and non-null <: nullable pass; nullable <: non-null
fails) AND the heaps relate by %HEAP-SUB-P (which consults CANON —
the module's canonicalization map — for typeuse-vs-typeuse
equivalence, loop 20). This admits (ref $t) <: funcref, (ref null
$t) <: funcref, funcref <: (ref null func), (ref null nofunc) <:
(ref null $t), and — with CANON — (ref $t1) <: (ref $t2) whenever
the two typeidxs are equivalent (type-equivalence.wast)."
  (let ((s (%expand-reftype sub))
        (u (%expand-reftype super)))
    (or (equal s u)
        (and (or (not (typed-ref-nullable-p s))
                 (typed-ref-nullable-p u))
             (%heap-sub-p (typed-ref-heap s) (typed-ref-heap u)
                          types canon)))))

(defun defaultable-valtype-p (t_)
  "Whether the default value exists for the type in this slice.
Numeric types and NULLABLE refs default; NON-NULLABLE typed refs do
not (spec: default values need a null; the definite-initialization
validator tracks local.set coverage for those)."
  (not (and (consp t_) (typed-ref-p t_)
            (not (typed-ref-nullable-p t_)))))

;;; ---------------------------------------------------------------
;;; Iso-recursive type equivalence (Core 3.0 recursive types).
;;;
;;; Model: every typeidx is a wasm-rec-type — a comptype plus a rec
;;; group object shared (EQ) by all members of its (rec ...) block.
;;; Plain type fields are SINGLETON groups. Two typeidxs are
;;; equivalent iff their unrolled definitions match, where a
;;; typeuse ref to a position inside the compared group maps to the
;;; OTHER group's same position (the iso-recursive hypothesis).

(defun rec-group-of (module typeidx)
  "The rec group object of TYPEIDX (NIL when out of range)."
  (let* ((types (wasm-module-types module))
         (ty (and (integerp typeidx) (>= typeidx 0)
                  (< typeidx (length types)) (svref types typeidx))))
    (and ty (wasm-rec-type-p ty) (wasm-rec-type-group ty))))

(defun %comptype-valtypes (comptype)
  "The valtype lists a comptype exposes for structural comparison:
params+results for a func, the field types for struct/array."
  (ecase (wasm-comptype-kind comptype)
    (:func (append (wasm-comptype-params comptype)
                   (wasm-comptype-results comptype)))
    ((:struct :array)
     (mapcar #'first (wasm-comptype-fields comptype)))))

(defun %valtype-equiv-p (a b module1 module2 g1 g2 bindings1 bindings2)
  "Valtype equality under iso-recursive equivalence: numerics by
EQUAL; typed refs compare with heap equivalence, nullability equal."
  (cond
   ((and (typed-ref-p a) (typed-ref-p b))
    ;; typed refs ALWAYS go through the iso rule — even when
    ;; EQUAL (same index): cross-group/internal-external cases
    ;; depend on the OWNING groups, not the index (type-rec.wast
    ;; ::93-101: group2's (ref $f1) with group1's internal (ref
    ;; $f1) are equal-shaped but NOT equivalent)
    (let ((ha (typed-ref-heap a))
          (hb (typed-ref-heap b))
          (na (typed-ref-nullable-p a))
          (nb (typed-ref-nullable-p b)))
      (and (eq na nb)
           (%heap-equiv-p ha hb module1 module2 g1 g2
                         bindings1 bindings2))))
   ((equal a b) t)
   (t nil)))

(defun %heap-equiv-p (ha hb module1 module2 g1 g2 bindings1 bindings2)
  "Heap-type equivalence: abstract keywords by EQ; (:TYPE N) pairs by
the iso-recursive rule. G1/G2 are the OWNING groups of the members
whose valtypes are being compared: a ref to a type whose group is EQ
to the owning group is INTERNAL — it must map to the other group's
ref at the SAME POSITION (both sides internal, positions equal); a
ref to any other type is EXTERNAL — both sides must be external and
recursively equivalent by index pair. The internal/external MIX is
not equivalent (type-rec.wast ::93-122: a rolled-up group is NOT
equal to its unrolled copy)."
  (declare (type (or keyword (cons (eql :type) (cons t null))) ha hb))
  (cond
   ((and (keywordp ha) (keywordp hb)) (eq ha hb))
   ((and (consp ha) (consp hb))
    (let ((ia (second ha)) (ib (second hb)))
      (multiple-value-bind (ga pa) (%target-group-of ia module1)
        (multiple-value-bind (gb pb) (%target-group-of ib module2)
          (cond
           ;; both internal: same position in the two groups
           ((and (eq ga g1) (eq gb g2)) (= pa pb))
           ;; both external: equivalence by index pair
           ((and (not (eq ga g1)) (not (eq gb g2)))
            (%rec-ref-equiv-p ia ib module1 module2 bindings1 bindings2))
           ;; internal vs external: not equivalent
           (t nil))))))
   (t nil)))

(defun %target-group-of (typeidx module)
  "Values (group position) of the rec group owning TYPEIDX."
  (let ((e (and (integerp typeidx) (>= typeidx 0)
                (< typeidx (length (wasm-module-types module)))
                (svref (wasm-module-types module) typeidx))))
    (if (and e (wasm-rec-type-p e))
        (values (wasm-rec-type-group e) (wasm-rec-type-group-idx e))
        (values nil 0))))

(defun %rec-ref-equiv-p (ia ib module1 module2 bindings1 bindings2)
  "The heart of iso-recursive equivalence: are typeidx IA of
MODULE1 and IB of MODULE2 equivalent? The modules may be the same
object (intra-module canonicalization) or distinct (cross-module
link/indirect checks).
  - An (IA . IB) pair already under comparison is assumed equal (the
    recursive hypothesis; terminates on cyclic structure).
  - Otherwise unfold ONE level: the defining groups must have equal
    arity and pairwise-equivalent members; a member's refs to its
    OWN group map positionally, refs outside the group recurse by
    index pair.
Memoization/assumption set: BINDINGS1 is a hash of open pairs.
  A repeated pair short-circuits TRUE without re-unfolding, and is
  retracted when the enclosing comparison fails so sibling branches
  re-check it honestly."
  (let ((key (cons ia ib)))
    (when (gethash key bindings1) (return-from %rec-ref-equiv-p t))
    (let* ((types1 (wasm-module-types module1))
           (types2 (wasm-module-types module2)))
      (unless (and (integerp ia) (>= ia 0) (< ia (length types1))
                   (integerp ib) (>= ib 0) (< ib (length types2)))
        (return-from %rec-ref-equiv-p nil))
      ;; groups: the entry may be a plain wasm-func-type (binary or
      ;; legacy paths) with NO group object — treat it as its own
      ;; singleton group on the fly.
      (multiple-value-bind (g1 p1 t1)
          (%entry-group-of (svref types1 ia) types1 ia)
        (multiple-value-bind (g2 p2 t2)
            (%entry-group-of (svref types2 ib) types2 ib)
          (declare (ignore p1 p2))
          (unless (= (length t1) (length t2))
            (return-from %rec-ref-equiv-p nil))
          (setf (gethash key bindings1) t)
          (let ((ok t))
            (dotimes (i (length t1))
              (unless (%comptype-equiv-p (svref t1 i) (svref t2 i)
                                         module1 module2
                                         g1 g2 bindings1 bindings2)
                (setf ok nil) (return)))
            (unless ok (remhash key bindings1))
            ok))))))

(defun %entry-group-of (ty types idx)
  "Values (group group-idx member-vector) for the type entry TY at
IDX inside TYPES. Plain functypes (binary/legacy paths, no rec
object) become on-the-fly singleton groups."
  (declare (ignore types idx))
  (if (wasm-rec-type-p ty)
      (let ((grp (wasm-rec-type-group ty)))
        (values grp (wasm-rec-type-group-idx ty)
                (wasm-rectype-group-types grp)))
      (let* ((ct (make-wasm-comptype :func nil '()
                                     (wasm-func-type-params ty)
                                     (wasm-func-type-results ty)))
             (grp (make-wasm-rectype-group (vector ct))))
        (values grp 0 (wasm-rectype-group-types grp)))))

(defun %comptype-equiv-p (ct1 ct2 module1 module2 g1 g2 bindings1 bindings2)
  "Structural comparison of two comptypes. KIND must match; a FUNC's
  PARAMS and RESULTS compare as SEPARATE lists (never concatenated:
  (func (param i32)) is NOT (func (result i32)) — imports.wast ::31's
  'incompatible import type' case); a STRUCT/ARRAY's fields compare
  positionally (valtype + mutability). G1/G2 are the OWNING groups
  of the two members: a typeuse ref whose target's group is EQ to
  the owning group is INTERNAL (maps positionally to the other
  group's same position); a target outside is EXTERNAL (recurses by
  index pair through %REC-REF-EQUIV-P); an internal/external mix is
  NOT equivalent."
  (unless (eq (wasm-comptype-kind ct1) (wasm-comptype-kind ct2))
    (return-from %comptype-equiv-p nil))
  (macrolet ((vt-equiv (vs1 vs2)
               `(and (= (length ,vs1) (length ,vs2))
                     (loop for v1 in ,vs1
                           for v2 in ,vs2
                           always (%valtype-equiv-p v1 v2 module1 module2
                                                    g1 g2 bindings1 bindings2)))))
    (ecase (wasm-comptype-kind ct1)
      (:func
       (and (vt-equiv (wasm-comptype-params ct1) (wasm-comptype-params ct2))
            (vt-equiv (wasm-comptype-results ct1) (wasm-comptype-results ct2))))
      ((:struct :array)
       (let ((f1 (wasm-comptype-fields ct1))
             (f2 (wasm-comptype-fields ct2)))
         (and (= (length f1) (length f2))
              (vt-equiv (mapcar #'first f1) (mapcar #'first f2))
              (every (lambda (a b) (eq (cdr a) (cdr b))) f1 f2)))))))

(defun type-equiv-p (module1 idx1 module2 idx2)
  "PUBLIC: whether typeidx IDX1 of MODULE1 and IDX2 of MODULE2 are
equivalent under iso-recursive equivalence (the same module twice
for intra-module canonicalization; two modules for link-time and
call_indirect checks)."
  (let ((bindings1 (make-hash-table :test #'equal)))
    (%rec-ref-equiv-p idx1 idx2 module1 module2 bindings1 nil)))

(defun %functype-entry-of (fty module)
  "Values (group comptype typeidx) for the functype FTY in MODULE's
type vector: the entry whose object IS FTY (the func-instance's
type is the defining module's entry object, so EQ finds it). A
plain (non-rec) entry in the vector still yields its typeidx (its
on-the-fly singleton group is materialized by %REC-REF-EQUIV-P).
A functype NOT in the vector (host functions, ad-hoc functypes)
yields typeidx NIL: no group, no index — only signature compare
applies."
  (let ((types (wasm-module-types module)))
    (loop for i from 0
          for ty across types
          when (eq ty fty)
            do (return-from %functype-entry-of
                 (if (wasm-rec-type-p ty)
                     (values (wasm-rec-type-group ty)
                             (wasm-rec-type-comptype ty)
                             i)
                     (values nil
                             (make-wasm-comptype
                              :func nil '()
                              (wasm-func-type-params ty)
                              (wasm-func-type-results ty))
                             i)))))
  (values nil
          (make-wasm-comptype :func nil '()
                              (wasm-func-type-params fty)
                              (wasm-func-type-results fty))
          nil))

(defun %functype-group-of (fty module)
  "Values (group comptype) — the two-value view of
%FUNCTYPE-ENTRY-OF (host/ad-hoc functypes yield a fresh comptype
and no group)."
  (multiple-value-bind (grp ct idx) (%functype-entry-of fty module)
    (declare (ignore idx))
    (values grp ct)))

(defun func-type-match-p (expected actual expected-module actual-module)
  "PUBLIC: whether functype EXPECTED (of EXPECTED-MODULE) and ACTUAL
\(of ACTUAL-MODULE — possibly a DIFFERENT module, the definer of a
func-instance) match as external function types. TWO levels:
  - Both sides are MODULE TYPE ENTRIES: full iso-recursive
    equivalence of the two TYPEIDXs (%REC-REF-EQUIV-P — the group
    shape is part of the type: type-rec.wast ::148-162 pins that a
    2-member rec member with a plain (func) signature does NOT match
    a singleton (func) — arity mismatch at the group level, even
    though the signatures are trivially equal).
  - Either side is a HOST/ad-hoc functype (not in a type vector):
    signature compare only (params/results pairwise via the
    iso-recursive valtype rule, with each side's owning group for
    internal/external classification when known).
Replaces the raw EQUAL compares at the link boundary
(%check-func-import), the tag-import boundary and the
indirect-call boundary (interp)."
  (multiple-value-bind (ga cta ia) (%functype-entry-of expected
                                                     expected-module)
    (multiple-value-bind (gb ctb ib) (%functype-entry-of actual
                                                        actual-module)
      (declare (ignore ga gb))
      (cond
       ;; both module entries: the TYPEIDXs carry the group shape
       ((and ia ib)
        (type-equiv-p expected-module ia actual-module ib))
       ;; signature compare (host funcs, ad-hoc functypes)
       (t
        (let ((bindings1 (make-hash-table :test #'equal)))
          (multiple-value-bind (ga2 cta2) (%functype-group-of expected
                                                             expected-module)
            (multiple-value-bind (gb2 ctb2) (%functype-group-of actual
                                                                actual-module)
              (flet ((vs (ct dir)
                       (ecase dir
                         (:params (wasm-comptype-params ct))
                         (:results (wasm-comptype-results ct)))))
                (and (= (length (vs cta2 :params)) (length (vs ctb2 :params)))
                     (= (length (vs cta2 :results)) (length (vs ctb2 :results)))
                     (loop for pa in (vs cta2 :params)
                           for pb in (vs ctb2 :params)
                           always (%valtype-equiv-p pa pb
                                                     expected-module
                                                     actual-module
                                                     ga2 gb2 bindings1 nil))
                     (loop for ra in (vs cta2 :results)
                           for rb in (vs ctb2 :results)
                           always (%valtype-equiv-p ra rb
                                                     expected-module
                                                     actual-module
                                                     ga2 gb2 bindings1 nil))))))))))))

(defun func-type-equal-p (a b)
  "PUBLIC: plain structural equality of two functypes (the legacy
find-or-add / canonicalization fast path: no rec groups, plain
valtypes — EQUAL on params/results)."
  (and (equal (wasm-func-type-params a) (wasm-func-type-params b))
       (equal (wasm-func-type-results a) (wasm-func-type-results b))))

