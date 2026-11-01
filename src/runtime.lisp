;;;; Runtime objects and module instantiation (docs/design/PLAN.md §7, CONTRACTS.md
;;;; "Runtime objects"). Package WASMDOWN.RUNTIME.
;;;;
;;;; - module-instance / func-instance / memory-instance / table-instance /
;;;;   global-instance structs and their operations.
;;;; - make-module-instance performs the spec instantiation sequence:
;;;;   import matching (link-error), allocation, active element/data
;;;;   segment init (instantiation-error on out of bounds), start
;;;;   function invocation (wasm-trap propagates), export resolution.
;;;; - wasm-call is the shared call trampoline: :wasm funcs go through
;;;;   the funcallable *INVOKE-WASM-FUNC* hook (installed by
;;;;   wasmdown.interp; codegen may replace it), :host funcs are APPLYed
;;;;   with the individual arguments and must return a results LIST.
;;;;
;;;; const expressions are evaluated through the *CONST-EXPR-EVALUATOR*
;;;; hook (installed by wasmdown.interp); make-module-instance also
;;;; accepts an explicit :CONST-EVAL function (instance expr) -> value.

(in-package #:wasmdown.runtime)

;;; ---------------------------------------------------------------
;;; Traps.

(defun %trap (kind format-control &rest args)
  "Signal wasmdown:wasm-trap of KIND with a formatted message."
  (error 'wasmdown:wasm-trap :kind kind
         :message (apply #'format nil format-control args)))

(defparameter *page-size* 65536)
(defparameter *max-pages* 1024)          ; impl limit (docs/design/PLAN.md 3.4): 64 MiB
                                         ; default; raise for real workloads.
                                         ; 65536 (4 GiB) would exhaust the
                                         ; SBCL heap on eager allocation.
(defparameter *max-table-size* 4294967295)

;;; ---------------------------------------------------------------
;;; Structs.

(defstruct (instance-store (:constructor %make-instance-store))
  "A host-side store: tracks every module instance created in it."
  (instances '() :type list))

(defun make-instance-store ()
  (%make-instance-store))

(defstruct (module-instance (:constructor %make-module-instance))
  "An instantiated module. MEMORIES/TABLES/GLOBALS/FUNCS/TAGS are vectors
in the index space (imported entities first, then defined ones); EXPORTS
is an alist (name . instance-object). ELEM-SEGS / DATA-SEGS hold the
element/data segment drop state and retained contents."
  (memories #() :type vector)
  (tables #() :type vector)
  (globals #() :type vector)
  (funcs #() :type vector)
  (tags #() :type vector)
  (module nil)
  (exports '() :type list)
  (elem-segs #() :type vector)
  (data-segs #() :type vector)
  (store nil))

(defstruct (func-instance (:constructor %make-func-instance))
  "A function reference. KIND :WASM: WASM-FUNCIDX indexes the module's
DEFINED funcs vector (wasm-module-funcs), MODULE is the backpointer to
its module-instance. KIND :HOST: HOST-FN is a Lisp function of the
individual arguments returning a results list.

BACKEND / COMPILED-ENTRY belong to KIND :WASM only and change nothing
about the reference: an imported, exported, table-held or ref.func'd
func-instance IS the same object in both backends, so identity is
stable across instantiation modes. COMPILED-ENTRY, when non-NIL, is a
closure (lambda (args) results) used instead of the interpreter hook;
an IMPORTED function keeps the backend of its defining instance, because
the object itself is shared."
  (kind :wasm :type (member :wasm :host))
  (wasm-funcidx 0 :type (integer 0))
  module
  host-fn
  type
  (backend :interpret :type (member :interpret :compiled))
  compiled-entry)

(defstruct (memory-instance (:constructor %make-memory-instance))
  "Linear memory: BYTES is a (vector (unsigned-byte 8)) of PAGES
pages. I64-P marks the memory64 address type (Core 3.0): loads/
stores address with i64 operands and grow's failure value is the
i64 -1."
  (bytes (make-array 0 :element-type '(unsigned-byte 8))
         :type (vector (unsigned-byte 8)))
  (pages 0 :type (integer 0))
  (max-pages *max-pages* :type (integer 0))
  (has-max-p nil :type boolean)
  (i64-p nil :type boolean))

(defstruct (table-instance (:constructor %make-table-instance))
  "A table of references; ELEMS is a simple vector of func-instances /
host objects / NIL (null ref). TYPE is the element reftype: a legacy
keyword (:funcREF/:externref/:exnref) or a typed-ref cons
\(:REF [NULL] HEAP) — typed function references, Core 3.0."
  (elems (make-array 0) :type simple-vector)
  (type :funcref)
  (max *max-table-size* :type (integer 0))
  (has-max-p nil :type boolean))

(defstruct (global-instance (:constructor %make-global-instance))
  "A global. TYPE is the valtype — keyword or typed-ref cons
\(:REF [NULL] HEAP), Core 3.0 function references."
  (value 0)
  (mutable nil :type boolean)
  (type :i32))

(defstruct (elem-seg-state (:constructor make-elem-seg-state))
  "Element segment state: dropped segments retain no contents (they
behave as empty for table.init)."
  (dropped-p nil :type boolean)
  (refs '() :type list))

(defstruct (data-seg-state (:constructor make-data-seg-state))
  "Data segment state: dropped segments behave as empty for
memory.init."
  (dropped-p nil :type boolean)
  (bytes #() :type (vector (unsigned-byte 8))))

(defstruct (tag-instance (:constructor %make-tag-instance))
  "A tag (exception) instance: MODULE is the defining module-instance
backpointer, TYPE the tag's functype (results must be empty; the
validator enforces that). Tag instances compare by IDENTITY per the
spec: throwing tag t is caught only by a catch clause naming the SAME
instance (instance.wast's import/export generativity tests pin this)."
  (module nil)
  (type (make-wasm-func-type '() '()) :type wasm-func-type))

;;; ---------------------------------------------------------------
;;; GC value objects (docs/design/PLAN.md §7.5: "Represent Wasm-GC structs and
;;; arrays as Lisp-managed objects... Use Lisp GC for backing
;;; storage, but explicitly implement Wasm subtype, field mutability
;;; and cast rules"). No registry is needed — the Lisp GC owns
;;; storage; identity is EQ on the struct instances themselves.

(defstruct (wasm-struct-instance (:constructor %make-wasm-struct-instance))
  "A struct value (loop 22, L1). TYPEIDX is the defining module's
typeidx of its comptype; FIELDS is a simple vector of field values
in declaration order. PACKED fields (i8/i16) normalize on write:
struct-set stores the width-wrapped value, struct.get_s/get_u extend
per the variant (the normalization lives in wasmdown.interp, which
knows the field type from the comptype)."
  (typeidx 0 :type (integer 0))
  (fields (make-array 0) :type simple-vector))

(defstruct (wasm-array-instance (:constructor %make-wasm-array-instance))
  "An array value (loop 22, L1). TYPEIDX as for structs; CONTENTS a
simple vector of element values. The element valtype is recovered
from the comptype at use sites (validate/interp), so it is not
redundantly stored."
  (typeidx 0 :type (integer 0))
  (contents (make-array 0) :type simple-vector))

;;; i31 values: a FIXNUM-tagged representation. The payload is kept
;;; wrapped to the spec's 31-bit domain (0..2^31-1) at creation;
;;; i31.get_s sign-extends from bit 30, i31.get_u zero-extends. The
;;; distinction from ordinary i32 stack values is POSITIONAL (only
;;; ref.i31 produces them), the same convention the reference
;;; interpreter uses for its abstract (ref i31) tagging.

(defun make-i31 (value)
  "Wrap VALUE into the i31 payload domain (2^31; the spec's 31-bit
payload, interpreted as an i32 field: store wraps)."
  (mod value (ash 1 31)))

(defun i31-get-s (i31)
  "The SIGNED view: sign-extend from bit 30, wrapped into the i32
unsigned domain (CONTRACTS.md: i32 values are 0..2^32-1)."
  (let ((v (mod i31 (ash 1 31))))
    (if (>= v (ash 1 30))
        (+ (- v (ash 1 31)) (ash 1 32))
        v)))

(defun i31-get-u (i31)
  "The UNSIGNED view: the payload zero-extended into the i32 domain."
  (mod i31 (ash 1 31)))

(defun i31-p (v)
  "Whether V is an i31 payload (a non-negative fixnum below 2^31).
Any such fixnum on the operand stack in a reference position is an
i31 — the only producer is ref.i31."
  (and (integerp v) (>= v 0) (< v (ash 1 31))))

(defun make-wasm-struct-value (typeidx fields)
  "Allocate a struct value: TYPEIDX the defining module's typeidx,
FIELDS a LIST or VECTOR of already-normalized field values."
  (%make-wasm-struct-instance
   :typeidx typeidx
   :fields (coerce fields 'simple-vector)))

(defun make-wasm-array-value (typeidx contents)
  "Allocate an array value: CONTENTS a LIST or VECTOR of
already-normalized element values."
  (%make-wasm-array-instance
   :typeidx typeidx
   :contents (coerce contents 'simple-vector)))

(defun struct-field-count (struct)
  (length (wasm-struct-instance-fields struct)))

(defun struct-field-get (struct fieldidx)
  (unless (and (>= fieldidx 0) (< fieldidx (struct-field-count struct)))
    (%trap :null-reference "struct field index out of range"))
  (svref (wasm-struct-instance-fields struct) fieldidx))

(defun struct-field-set (struct fieldidx value)
  (unless (and (>= fieldidx 0) (< fieldidx (struct-field-count struct)))
    (%trap :null-reference "struct field index out of range"))
  (setf (svref (wasm-struct-instance-fields struct) fieldidx) value)
  (values))

(defun array-length (array)
  (length (wasm-array-instance-contents array)))

(declaim (inline %check-array-range))
(defun %check-array-range (array idx)
  (unless (and (>= idx 0) (< idx (length (wasm-array-instance-contents array))))
    (%trap :out-of-bounds-table
           "out of bounds array access: index ~d not within ~d"
           idx (length (wasm-array-instance-contents array)))))

(defun array-element-get (array idx)
  (%check-array-range array idx)
  (svref (wasm-array-instance-contents array) idx))

(defun array-element-set (array idx value)
  (%check-array-range array idx)
  (setf (svref (wasm-array-instance-contents array) idx) value)
  (values))

(defun %check-array-span (array start end)
  "END exclusive; traps when [START, END) leaves the array. Arrays
are tables-of-values per the loop-22 trap taxonomy: the TABLE OOB
kind covers array bounds (docs/design/PLAN.md §7.5 lists 'out-of-bounds
memory/table')."
  (unless (and (<= start end)
               (<= end (length (wasm-array-instance-contents array))))
    (%trap :out-of-bounds-table
           "out of bounds array access: [~d,~d) not within ~d"
           start end (length (wasm-array-instance-contents array)))))

;;; ---------------------------------------------------------------
;;; Value-level GC semantics (loop 22) shared by BOTH backends. The
;;; reference interpreter calls these directly; the compiled compiler
;;; emits one call per GC instruction, so the packed normalization,
;;; field defaults, bounds checks and cast rules have exactly ONE
;;; implementation.

(defun packed-normalize (value storage-type)
  "The STORE side of packed normalization: an :I8/:I16 storage wraps
VALUE to its width; every other storage keeps it (the validator proved
the type)."
  (case storage-type
    (:i8 (mod value #x100))
    (:i16 (mod value #x10000))
    (t value)))

(defun packed-extend (value storage-type signedp)
  "The LOAD side of packed normalization: SIGNEDP (struct.get_s /
array.get_s) sign-extends a packed storage to the i32 unsigned
domain; the plain and _u variants return the stored (width-wrapped)
value."
  (case storage-type
    (:i8 (if signedp
             (mod (if (>= value #x80) (- value #x100) value) #x100000000)
             value))
    (:i16 (if signedp
              (mod (if (>= value #x8000) (- value #x10000) value)
                   #x100000000)
              value))
    (t value)))

(defun default-valtype-value (valtype)
  "The default value of VALTYPE: numeric zero, packed zero, NIL for
every reference type (the null bottom; the validator proves no
non-nullable local is read before its set)."
  (case valtype
    (:i32 0) (:i64 0)
    (:i8 0) (:i16 0)
    (:f32 (wasmdown.numops:f32-from-bits 0))
    (:f64 (wasmdown.numops:f64-from-bits 0))
    (:funcref nil) (:externref nil) (:exnref nil)
    (t (if (and (consp valtype) (eq (first valtype) :ref))
           nil
           (%trap :unreachable "no default for type ~a" valtype)))))

(defun value-heap (v)
  "The HEAP of a runtime reference value for the dynamic cast check:
a func-instance -> :func; a struct/array value -> its concrete
typeuse; an i31 payload -> :i31; a host object -> :extern; null ->
the null bottom :none."
  (cond
    ((func-instance-p v) :func)
    ((wasm-struct-instance-p v)
     (list :type (wasm-struct-instance-typeidx v)))
    ((wasm-array-instance-p v)
     (list :type (wasm-array-instance-typeidx v)))
    ((i31-p v) :i31)
    ((null v) :none)
    (t :extern)))

(defun cast-succeeds-p (v heap nullable module)
  "The DYNAMIC cast rule: whether V's shape is <: the target
(REF NULL? HEAP) in MODULE's type vector. Null succeeds only against
NULLABLE targets; concrete shapes go through the L2 lattice
(WASMDOWN.TYPES:REFTYPE-SUB-P), the same relation the validator uses."
  (cond
    ((null v) (and nullable t))
    (t (reftype-sub-p (list :ref (value-heap v))
                      (if nullable
                          (list :ref 'null heap)
                          (list :ref heap))
                      (wasm-module-types module)))))

(defun %comptype-of (instance typeidx)
  (wasm-rec-type-comptype
   (svref (wasm-module-types (module-instance-module instance)) typeidx)))

(defun %field-storage-type (instance typeidx fieldidx)
  (first (nth fieldidx (wasm-comptype-fields (%comptype-of instance typeidx)))))

(defun %array-element-type (instance typeidx)
  (first (first (wasm-comptype-fields (%comptype-of instance typeidx)))))

(defun struct-new-value (instance typeidx values)
  "Allocate a struct from VALUES (push order): normalize packed
fields on store."
  (let ((fs (wasm-comptype-fields (%comptype-of instance typeidx))))
    (make-wasm-struct-value
     typeidx
     (loop for v in values
           for f in fs
           collect (packed-normalize v (first f))))))

(defun struct-new-default-value (instance typeidx)
  (make-wasm-struct-value
   typeidx
   (loop for f in (wasm-comptype-fields (%comptype-of instance typeidx))
         collect (default-valtype-value (first f)))))

(defun struct-get-value (instance typeidx fieldidx ref signedp)
  "Field FIELDidx of REF; a null REF traps, packed loads extend."
  (when (null ref)
    (%trap :null-reference "null struct reference"))
  (packed-extend (struct-field-get ref fieldidx)
                 (%field-storage-type instance typeidx fieldidx)
                 signedp))

(defun struct-set-value (instance typeidx fieldidx ref value)
  (when (null ref)
    (%trap :null-reference "null struct reference"))
  (struct-field-set ref fieldidx
                    (packed-normalize
                     value (%field-storage-type instance typeidx fieldidx)))
  (values))

(defun array-new-value (instance typeidx value len)
  (make-wasm-array-value
   typeidx
   (make-list (or len 0)
              :initial-element
              (packed-normalize value (%array-element-type instance typeidx)))))

(defun array-new-default-value (instance typeidx len)
  (make-wasm-array-value
   typeidx
   (make-list (or len 0)
              :initial-element
              (default-valtype-value (%array-element-type instance typeidx)))))

(defun array-new-fixed-value (instance typeidx values)
  (let ((st (%array-element-type instance typeidx)))
    (make-wasm-array-value
     typeidx
     (mapcar (lambda (v) (packed-normalize v st)) values))))

(defun %load-lanes-le (bytes ea width)
  "A little-endian WIDTH-byte read at EA from a data segment's BYTES
\(array.new_data/array.init_data fill sources; bounds checked by the
caller)."
  (loop for i below width
        sum (ash (aref bytes (+ ea i)) (* 8 i))))

(defun array-new-data-value (instance typeidx offset length dataidx)
  "A new array of LENGTH elements read from data segment DATAIDX at
OFFSET (both in elements of the packed storage width)."
  (let* ((st (%array-element-type instance typeidx))
         (width (ecase st (:i8 1) (:i16 2) (:i32 4)))
         (bytes (take-data-bytes instance dataidx)))
    (unless (<= (+ offset length) (/ (length bytes) width))
      (%trap :out-of-bounds-table
             "out of bounds array.new_data: [~d,~d) not within ~d"
             offset length (/ (length bytes) width)))
    (make-wasm-array-value
     typeidx
     (loop for i below length
           collect (packed-normalize
                    (%load-lanes-le bytes (* (+ offset i) width) width)
                    st)))))

(defun array-new-elem-value (instance typeidx offset length elemidx)
  (let ((refs (take-elem-ref instance elemidx)))
    (unless (<= (+ offset length) (length refs))
      (%trap :out-of-bounds-table
             "out of bounds array.new_elem: [~d,~d) not within ~d"
             offset length (length refs)))
    (make-wasm-array-value typeidx (subseq refs offset (+ offset length)))))

(defun array-get-value (instance typeidx ref idx signedp)
  (when (null ref)
    (%trap :null-reference "null array reference"))
  (packed-extend (array-element-get ref idx)
                 (%array-element-type instance typeidx)
                 signedp))

(defun array-set-value (instance typeidx ref idx value)
  (when (null ref)
    (%trap :null-reference "null array reference"))
  (array-element-set ref idx
                     (packed-normalize
                      value (%array-element-type instance typeidx)))
  (values))

(defun array-len-value (ref)
  (when (null ref)
    (%trap :null-reference "null array reference"))
  (array-length ref))

(defun array-fill-value (instance typeidx ref offset value length)
  (when (null ref)
    (%trap :null-reference "null array reference"))
  (let ((st (%array-element-type instance typeidx)))
    (unless (<= (+ offset length) (array-length ref))
      (%trap :out-of-bounds-table
             "out of bounds array.fill: [~d,~d) not within ~d"
             offset length (array-length ref)))
    (loop for i from offset below (+ offset length)
          do (array-element-set ref i (packed-normalize value st)))
    (values)))

(defun array-copy-value (dst dst-offset src src-offset length)
  (when (null dst)
    (%trap :null-reference "null array reference"))
  (when (null src)
    (%trap :null-reference "null array reference"))
  (unless (<= (+ src-offset length) (array-length src))
    (%trap :out-of-bounds-table "out of bounds array.copy source"))
  (unless (<= (+ dst-offset length) (array-length dst))
    (%trap :out-of-bounds-table "out of bounds array.copy destination"))
  (loop for i below length
        do (array-element-set dst (+ dst-offset i)
                              (array-element-get src (+ src-offset i))))
  (values))

(defun array-init-data-value (instance typeidx ref dst-index offset length
                                       dataidx)
  (when (null ref)
    (%trap :null-reference "null array reference"))
  (let* ((st (%array-element-type instance typeidx))
         (width (ecase st (:i8 1) (:i16 2) (:i32 4)))
         (bytes (take-data-bytes instance dataidx)))
    (unless (<= (+ offset length) (/ (length bytes) width))
      (%trap :out-of-bounds-table
             "out of bounds array.init_data source"))
    (unless (<= (+ dst-index length) (array-length ref))
      (%trap :out-of-bounds-table
             "out of bounds array.init_data destination"))
    (loop for i below length
          do (array-element-set
              ref (+ dst-index i)
              (packed-normalize
               (%load-lanes-le bytes (* (+ offset i) width) width)
               st)))
    (values)))

(defun array-init-elem-value (instance typeidx ref dst-index offset length
                                       elemidx)
  (declare (ignore typeidx))
  (when (null ref)
    (%trap :null-reference "null array reference"))
  (let ((refs (take-elem-ref instance elemidx)))
    (unless (<= (+ offset length) (length refs))
      (%trap :out-of-bounds-table
             "out of bounds array.init_elem source"))
    (unless (<= (+ dst-index length) (array-length ref))
      (%trap :out-of-bounds-table
             "out of bounds array.init_elem destination"))
    (loop for i below length
          do (array-element-set ref (+ dst-index i)
                                (nth (+ offset i) refs)))
    (values)))


(defun global-value (g) (global-instance-value g))
(defun (setf global-value) (v g) (setf (global-instance-value g) v))
(defun global-set-value (g v) (setf (global-instance-value g) v))

;;; ---------------------------------------------------------------
;;; Constructors.

(defun make-wasm-func-instance (module funcidx)
  "MODULE is the module-instance owning the function; FUNCIDX indexes
its DEFINED funcs vector (wasm-module-funcs of the underlying module)."
  (let* ((wmod (module-instance-module module))
         (fn (aref (wasm-module-funcs wmod) funcidx)))
    (%make-func-instance :kind :wasm :wasm-funcidx funcidx
                         :module module
                         :type (aref (wasm-module-types wmod)
                                     (wasm-func-typeidx fn)))))

(defun make-host-func-instance (type host-fn)
  "TYPE is a wasm-func-type; HOST-FN is called with the individual
arguments and returns a results LIST."
  (%make-func-instance :kind :host :host-fn host-fn :type type))

(defun make-memory-instance (limits)
  "Allocate a memory from a wasm-limits. The byte vector is zeroed."
  (let ((min (wasm-limits-min limits))
        (max (wasm-limits-max limits)))
    (when (> min *max-pages*)
      (%trap :out-of-bounds-memory "memory too large: ~d pages" min))
    (%make-memory-instance
     :bytes (make-array (* min *page-size*)
                        :element-type '(unsigned-byte 8) :initial-element 0)
     :pages min
     :max-pages (or max *max-pages*)
     :has-max-p (wasm-limits-has-max-p limits)
     ;; memory64 (Core 3.0): the address type from the limits flag
     :i64-p (wasm-limits-i64-p limits))))

(defun make-table-instance (type limits)
  "Allocate a table of TYPE (:funcref/:externref) from wasm-limits.
Elements initialize to the null reference (NIL)."
  (let ((min (wasm-limits-min limits))
        (max (wasm-limits-max limits)))
    (when (> min *max-table-size*)
      (%trap :out-of-bounds-table "table too large: ~d elements" min))
    (%make-table-instance
     :elems (make-array min :initial-element nil)
     :type type
     :max (or max *max-table-size*)
     :has-max-p (wasm-limits-has-max-p limits))))

(defun make-global-instance (value &key (type :i32) (mutable nil))
  (%make-global-instance :value value :mutable mutable :type type))

(defun make-tag-instance (module type)
  "MODULE is the owning module-instance; TYPE the tag's functype."
  (%make-tag-instance :module module :type type))

;;; ---------------------------------------------------------------
;;; Hooks. wasmdown.interp installs the reference implementations at
;;; load time; codegen may replace *INVOKE-WASM-FUNC* later.

(defparameter *invoke-wasm-func*
  (lambda (func args)
    (declare (ignore func args))
    (error 'wasmdown:host-error
           :message "no wasm function executor installed (load wasmdown.interp)"))
  "Compatibility entry point: the interpreter trampoline. WASM-CALL now
goes through INVOKE-WASM, which drives *INVOKE-WASM-BODY* itself.")

(defparameter *invoke-wasm-body*
  (lambda (func args)
    (declare (ignore func args))
    (error 'wasmdown:host-error
           :message "no wasm function body executor installed (load wasmdown.interp)"))
  "Executes ONE guest function body and returns its results list. It
does not manage depth, does not catch tail transfers and does not
convert uncaught exceptions: those belong to INVOKE-WASM, so the
interpreter and the compiled backend share one call protocol. A
return_call throws *TAIL-CALL-TAG* with a TAIL-TRANSFER instead of
calling:
    (throw *tail-call-tag* (make-tail-transfer callee args))")

;;; ---------------------------------------------------------------
;;; The guest call protocol. ONE boundary for both backends.
;;;
;;; INVOKE-WASM owns what a compiled closure and an interpreter body must
;;; not each reimplement:
;;;
;;;   - guest call depth (*GUEST-DEPTH*), which a TAIL transfer does not
;;;     grow;
;;;   - the tail-transfer trampoline, so return_call/return_call_indirect/
;;;     return_call_ref need neither SBCL TCO nor a growing host stack;
;;;   - the outermost exception boundary (an uncaught guest exception
;;;     traps here, at the host<->guest edge).
;;;
;;; A body selected by the func-instance's COMPILED-ENTRY or by
;;; *INVOKE-WASM-BODY* implements only "run this function once".

(defparameter *tail-call-tag* 'wasmdown-tail-transfer
  "Catch tag for a tail transfer; the payload is a TAIL-TRANSFER.")

(defstruct (tail-transfer (:constructor make-tail-transfer (callee args)))
  callee
  args)

(defun tail-call (callee args)
  "Tail-transfer CALLEE with ARGS from the current guest body. Never
returns: the transfer unwinds this body (and only this body) to the
INVOKE-WASM trampoline that entered it. Compiled code calls this for
return_call/return_call_indirect/return_call_ref."
  (throw *tail-call-tag* (make-tail-transfer callee args)))

(defvar *guest-depth* 0
  "Guest call frames currently executing. Bound per INVOKE-WASM; a tail
transfer keeps the frame it replaces, so only non-tail calls grow it.")

(defvar *guest-depth-limit* 1000
  "Maximum guest call nesting before :call-stack-exhausted. It bounds
the HOST stack both backends consume per frame (interpreter body or
compiled closure) plus the protocol frames; re-measure whenever the
per-frame shape changes. See docs/status.md for the declared limit.")

(declaim (inline %guest-depth-enter))
(defun %guest-depth-enter (depth)
  (when (>= depth *guest-depth-limit*)
    (%trap :call-stack-exhausted
           "call depth limit ~d exceeded" *guest-depth-limit*))
  depth)

(defun %run-guest-body (func args)
  "Run one body of FUNC with the backend its func-instance selects."
  (let ((entry (func-instance-compiled-entry func)))
    (if entry
        (progn (%count-backend-entry :compiled)
               (funcall entry args))
        (progn (%count-backend-entry :interpret)
               (funcall *invoke-wasm-body* func args)))))

(defun invoke-wasm (func args)
  "Call a :wasm FUNC with ARGS (push order) and return its results list.
The single protocol boundary shared by both backends."
  (let* ((depth (%guest-depth-enter (1+ *guest-depth*)))
         (*guest-depth* depth))
    (let ((outermost-p (= depth 1)))
      (handler-bind
          ((wasmdown:wasm-exception
             (lambda (ex)
               ;; the boundary is the host<->guest edge: a nested call's
               ;; exception must reach the CALLER's try_table handlers,
               ;; so only the outermost invocation converts it.
               (declare (ignore ex))
               (when outermost-p
                 (error 'wasmdown:wasm-trap
                        :kind :uncaught-exception
                        :message "uncaught exception")))))
        (loop with f = func
              with a = args
              do (cond
                   ;; a tail transfer landing on a HOST import has no
                   ;; guest frame to replace: its results ARE this
                   ;; invocation's results.
                   ((eq (func-instance-kind f) :host)
                    (return (funcall (func-instance-host-fn f) a)))
                   (t
                    (let ((outcome (catch *tail-call-tag*
                                     (%run-guest-body f a))))
                      (if (tail-transfer-p outcome)
                          (setf f (tail-transfer-callee outcome)
                                a (tail-transfer-args outcome))
                          (return outcome))))))))))

(defparameter *const-expr-evaluator*
  (lambda (instance expr)
    (declare (ignore instance expr))
    (error 'wasmdown:host-error
           :message "no constant-expression evaluator installed (load wasmdown.interp)")))

;;; ---------------------------------------------------------------
;;; Memory operations. Bounds are computed with exact Lisp integers
;;; (bignum domain), so a 4 GiB address never overflows the host.

(declaim (inline %check-mem-range))
(defun %check-mem-range (mem ea width-bytes)
  (let ((size (length (memory-instance-bytes mem))))
    (unless (<= (+ ea width-bytes) size)
      (%trap :out-of-bounds-memory
             "out of bounds memory access: ~d + ~d not within ~d"
             ea width-bytes size))))

(defun memory-load (mem ea width-bytes)
  "Read WIDTH-BYTES (1/2/4/8) little-endian bytes at EA as unsigned."
  (%check-mem-range mem ea width-bytes)
  (let ((bytes (memory-instance-bytes mem))
        (acc 0))
    (loop for i from 0 below width-bytes
          do (setf acc (logior acc (ash (aref bytes (+ ea i)) (* 8 i)))))
    acc))

(defun memory-store (mem ea width-bytes value)
  "Store the low WIDTH-BYTES of VALUE at EA, little-endian."
  (%check-mem-range mem ea width-bytes)
  (let ((bytes (memory-instance-bytes mem)))
    (loop for i from 0 below width-bytes
          do (setf (aref bytes (+ ea i))
                   (logand #xff (ash value (- (* 8 i))))))
    value))

(defun memory-size (mem)
  (memory-instance-pages mem))

(defun memory-grow (mem delta)
  "Grow by DELTA pages. Returns the old page count on success, -1 in
the memory's ADDRESS TYPE on failure (4294967295 for i32 memories;
18446744073709551615 for memory64 — spec exec/intabort.ml's grow
failure is the index-type -1). On success the byte vector is
replaced by a larger one that preserves contents and zero-fills.
The *max-pages* ceiling is the runtime's own allocation bound
(docs/design/PLAN.md §3.4) — the validator's spec-side bound is separate."
  (let* ((old (memory-instance-pages mem))
         (new (+ old delta))
         (fail (if (memory-instance-i64-p mem)
                   18446744073709551615
                   4294967295)))
    (if (or (> new *max-pages*) (> new (memory-instance-max-pages mem)))
        fail
        (let* ((old-bytes (memory-instance-bytes mem))
               (new-bytes (make-array (* new *page-size*)
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 0)))
          (replace new-bytes old-bytes)
          (setf (memory-instance-bytes mem) new-bytes)
          (setf (memory-instance-pages mem) new)
          old))))

(defun %check-mem-span (mem start end)
  "END is exclusive. Traps when [START, END) leaves the memory."
  (let ((size (length (memory-instance-bytes mem))))
    (unless (and (<= start end) (<= end size))
      (%trap :out-of-bounds-memory
             "out of bounds memory access: [~d,~d) not within ~d"
             start end size))))

(defun memory-fill (mem dst val len)
  "memory.fill: LEN bytes of VAL (low 8 bits) at DST."
  (%check-mem-span mem dst (+ dst len))
  (let ((bytes (memory-instance-bytes mem))
        (b (logand #xff val)))
    (loop for i from dst below (+ dst len)
          do (setf (aref bytes i) b))))

(defun memory-copy (mem dst src len)
  "memory.copy with memmove semantics (overlapping ranges allowed)."
  (%check-mem-span mem dst (+ dst len))
  (%check-mem-span mem src (+ src len))
  (let ((bytes (memory-instance-bytes mem)))
    (if (or (<= (+ src len) dst) (<= (+ dst len) src))
        (replace bytes bytes :start1 dst :end1 (+ dst len)
                 :start2 src)
        ;; overlap: copy through a fresh intermediate
        (let ((tmp (make-array len :element-type '(unsigned-byte 8))))
          (replace tmp bytes :start2 src)
          (replace bytes tmp :start1 dst)))))

(defun memory-copy2 (dst-mem dst src-mem src len)
  "Cross-memory memory.copy (Core 3.0 multi-memory): LEN bytes from
SRC-MEM [SRC, SRC+LEN) to DST-MEM [DST, DST+LEN). Bounds-check BOTH
spans in the exact-integer domain before any access; no overlap is
possible (distinct backings)."
  (%check-mem-span dst-mem dst (+ dst len))
  (%check-mem-span src-mem src (+ src len))
  (let ((dbytes (memory-instance-bytes dst-mem))
        (sbytes (memory-instance-bytes src-mem)))
    (replace dbytes sbytes :start1 dst :end1 (+ dst len) :start2 src)))

(defun memory-init (mem data-state dst src len)
  "memory.init: copy LEN bytes of data segment DATA-STATE [SRC, SRC+LEN)
 to DST. The data segment state is dropped-aware."
  (let ((seg-bytes (if (data-seg-state-dropped-p data-state)
                       (make-array 0 :element-type '(unsigned-byte 8))
                       (data-seg-state-bytes data-state))))
    (unless (and (<= src (+ src len)) (<= (+ src len) (length seg-bytes)))
      (%trap :out-of-bounds-memory
             "out of bounds memory access: data range [~d,~d) not within ~d"
             src (+ src len) (length seg-bytes)))
    (%check-mem-span mem dst (+ dst len))
    (let ((bytes (memory-instance-bytes mem)))
      (replace bytes seg-bytes :start1 dst :end1 (+ dst len) :start2 src))))

;;; ---------------------------------------------------------------
;;; Table operations.

(declaim (inline %check-table-range))
(defun %check-table-range (table idx)
  (let ((size (length (table-instance-elems table))))
    (unless (< idx size)
      (%trap :out-of-bounds-table
             "out of bounds table access: index ~d not within ~d"
             idx size))))

(defun %check-table-span (table start end)
  (unless (and (<= start end) (<= end (length (table-instance-elems table))))
    (%trap :out-of-bounds-table
           "out of bounds table access: [~d,~d) not within ~d"
           start end (length (table-instance-elems table)))))

(defun table-get (table idx)
  (%check-table-range table idx)
  (svref (table-instance-elems table) idx))

(defun table-set (table idx ref)
  (%check-table-range table idx)
  (setf (svref (table-instance-elems table) idx) ref)
  (values))

(defun table-size (table)
  (length (table-instance-elems table)))

(defun table-grow (table ref delta)
  "Grow by DELTA slots initialized to REF. Returns old size or 4294967295
 (i32 -1) on failure."
  (let* ((old (table-size table))
         (new (+ old delta)))
    (if (> new (table-instance-max table))
        4294967295
        (let* ((old-elems (table-instance-elems table))
               (new-elems (make-array new :initial-element ref)))
          (replace new-elems old-elems)
          (setf (table-instance-elems table) new-elems)
          old))))

(defun table-fill (table dst ref len)
  (%check-table-span table dst (+ dst len))
  (let ((elems (table-instance-elems table)))
    (loop for i from dst below (+ dst len)
          do (setf (svref elems i) ref))))

(defun table-copy2 (dst-table src-table dst src len)
  "table.copy across TWO tables: each table's span is bounds-checked
against its own backing, and the copy is memmove-safe (delegating to
TABLE-COPY when both indices name the same table)."
  (if (eq dst-table src-table)
      (table-copy dst-table dst src len)
      (progn
        (%check-table-span dst-table dst (+ dst len))
        (%check-table-span src-table src (+ src len))
        (replace (table-instance-elems dst-table)
                 (table-instance-elems src-table)
                 :start1 dst :end1 (+ dst len)
                 :start2 src :end2 (+ src len))
        dst-table)))

(defun table-copy (table dst src len)
  "table.copy within one table, memmove semantics."
  (%check-table-span table dst (+ dst len))
  (%check-table-span table src (+ src len))
  (let ((elems (table-instance-elems table)))
    (if (or (<= (+ src len) dst) (<= (+ dst len) src))
        (replace elems elems :start1 dst :end1 (+ dst len) :start2 src)
        (let ((tmp (make-array len)))
          (replace tmp elems :start2 src)
          (replace elems tmp :start1 dst)))))

(defun table-init (table elem-state dst src len)
  "table.init: copy LEN refs of elem segment ELEM-STATE [SRC, SRC+LEN) to
 DST. Drops nothing here; the caller drops via DROP-ELEM."
  (let ((seg-refs (if (elem-seg-state-dropped-p elem-state)
                      '()
                      (elem-seg-state-refs elem-state))))
    (unless (and (<= src (+ src len)) (<= (+ src len) (length seg-refs)))
      (%trap :out-of-bounds-table
             "out of bounds table access: elem range [~d,~d) not within ~d"
             src (+ src len) (length seg-refs)))
    (%check-table-span table dst (+ dst len))
    (let ((elems (table-instance-elems table)))
      (loop for i from 0 below len
            do (setf (svref elems (+ dst i)) (nth (+ src i) seg-refs))))))

;;; ---------------------------------------------------------------
;;; Segment drop state and accessors.

(defun elem-dropped-p (instance elemidx)
  (elem-seg-state-dropped-p (aref (module-instance-elem-segs instance) elemidx)))

(defun data-dropped-p (instance dataidx)
  (data-seg-state-dropped-p (aref (module-instance-data-segs instance) dataidx)))

(defun drop-elem (instance elemidx)
  "Mark the element segment dropped; its contents become empty."
  (let ((state (aref (module-instance-elem-segs instance) elemidx)))
    (setf (elem-seg-state-dropped-p state) t)
    (setf (elem-seg-state-refs state) '())))

(defun drop-data (instance dataidx)
  "Mark the data segment dropped; its bytes become empty."
  (let ((state (aref (module-instance-data-segs instance) dataidx)))
    (setf (data-seg-state-dropped-p state) t)
    (setf (data-seg-state-bytes state)
          (make-array 0 :element-type '(unsigned-byte 8)))))

(defun take-elem-ref (instance elemidx)
  "The refs of an element segment (empty once dropped); does NOT drop."
  (if (elem-dropped-p instance elemidx)
      '()
      (elem-seg-state-refs (aref (module-instance-elem-segs instance) elemidx))))

(defun take-data-bytes (instance dataidx)
  "The bytes of a data segment (empty once dropped); does NOT drop."
  (if (data-dropped-p instance dataidx)
      #()
      (data-seg-state-bytes (aref (module-instance-data-segs instance) dataidx))))

;;; ---------------------------------------------------------------
;;; Import matching (spec external-type matching; failures are
;;; wasmdown:link-error). The imports alist maps (module-name
;;; field-name) lists to instance objects:
;;;   ((\"env\" \"log\") . func-instance) ...

(defun %link-error (format-control &rest args)
  (error 'wasmdown:link-error
         :message (apply #'format nil format-control args)))

(defun %inst-error (format-control &rest args)
  (error 'wasmdown:instantiation-error
         :message (apply #'format nil format-control args)))

(defun %limits-match-p (expected actual-min actual-max actual-has-max-p)
  "Spec import rule: the provided limits must be at least EXPECTED's
 min and, when EXPECTED declares a max, must declare a max no larger."
  (and (<= (wasm-limits-min expected) actual-min)
       (or (not (wasm-limits-has-max-p expected))
           (and actual-has-max-p (<= actual-max (wasm-limits-max expected))))))

(defun %check-func-import (import value module)
  (unless (func-instance-p value)
    (%link-error "import ~a.~a: expected a function, got ~s"
                 (wasm-import-module import) (wasm-import-field import)
                 (type-of value)))
  (let* ((expected (aref (wasm-module-types module) (wasm-import-type import)))
         (actual (func-instance-type value))
         ;; the providing module: the DEFINING module of the func
         ;; instance (its type vector gives the typeuse context —
         ;; cross-module indices are meaningless positionally, so
         ;; the comparator is structural with the iso-recursive
         ;; group rule; loop 20, type-equivalence.wast ::191-324's
         ;; link-time cases). A host function has no module: a bare
         ;; module context works (its type is a plain functype).
         (actual-module (or (and (func-instance-module value)
                                 (module-instance-module
                                  (func-instance-module value)))
                            (make-wasm-module))))
    (unless (wasmdown.types:func-type-match-p
             expected actual module actual-module)
      (%link-error "import ~a.~a: function type mismatch"
                   (wasm-import-module import) (wasm-import-field import)))))

(defun %check-table-import (import value)
  (unless (table-instance-p value)
    (%link-error "import ~a.~a: expected a table, got ~s"
                 (wasm-import-module import) (wasm-import-field import)
                 (type-of value)))
  (let ((expected (wasm-import-type import)))
    ;; spec: import matching on the element type is EQUALITY in this
    ;; slice (table types match by equal reftype; the cons typed-ref
    ;; forms compare structurally, legacy keywords by eq).
    (unless (equal (wasm-table-type expected) (table-instance-type value))
      (%link-error "import ~a.~a: table element type mismatch"
                   (wasm-import-module import) (wasm-import-field import)))
    (unless (%limits-match-p (wasm-table-limits expected)
                             (table-size value)
                             (table-instance-max value)
                             (table-instance-has-max-p value))
      (%link-error "import ~a.~a: table limits mismatch"
                   (wasm-import-module import) (wasm-import-field import)))))

(defun %check-memory-import (import value)
  (unless (memory-instance-p value)
    (%link-error "import ~a.~a: expected a memory, got ~s"
                 (wasm-import-module import) (wasm-import-field import)
                 (type-of value)))
  (unless (%limits-match-p (wasm-memory-limits (wasm-import-type import))
                           (memory-instance-pages value)
                           (memory-instance-max-pages value)
                           (memory-instance-has-max-p value))
    (%link-error "import ~a.~a: memory limits mismatch"
                 (wasm-import-module import) (wasm-import-field import))))

(defun %check-global-import (import value module)
  "Global import matching: kind, then type. IMMUTABLE globals match
COVARIANTLY (provided <: declared — linking.wast's (ref func)
provided < (ref null func) declared); MUTABLE must be EQUAL. MODULE
is the raw wasm-module being linked (the importer); its types
vector serves the abstract-heap subtyping cases."
  (unless (global-instance-p value)
    (%link-error "import ~a.~a: expected a global, got ~s"
                 (wasm-import-module import) (wasm-import-field import)
                 (type-of value)))
  (let* ((spec (wasm-import-type import))     ; (valtype . mutable-p)
         (declared (car spec))
         (mutable (cdr spec))
         (actual (global-instance-type value))
         (import-reftype-p (lambda (t_)
                             (or (member t_ '(:funcref :externref :exnref))
                                 (and (consp t_)
                                      (wasmdown.types:typed-ref-p t_))))))
    (unless (and (or (equal actual declared)
                     (and (not mutable)
                          (funcall import-reftype-p actual)
                          (funcall import-reftype-p declared)
                          (wasmdown.types:reftype-sub-p
                           actual declared (wasm-module-types module))))
                 (eq mutable (global-instance-mutable value)))
      (%link-error "import ~a.~a: global type or mutability mismatch"
                   (wasm-import-module import)
                   (wasm-import-field import)))))

(defun %check-tag-import (import value module)
  "Tag imports match by functype equality (spec external type of a
tag: its type's params only, results being empty by construction).
  A non-tag value (imports.wast's '(import \"test\" \"tag\" (func))'
  incompatible-type case) is a link error here, never a kind mixup."
  (declare (ignorable module))
  (unless (tag-instance-p value)
    (%link-error "import ~a.~a: expected a tag, got ~s"
                 (wasm-import-module import) (wasm-import-field import)
                 (type-of value)))
  (let* ((expected-typeidx (wasm-import-type import))
         (expected (svref (wasm-module-types module) expected-typeidx))
         (actual (tag-instance-type value))
         (actual-module (or (and (tag-instance-module value)
                                 (module-instance-module
                                  (tag-instance-module value)))
                            (make-wasm-module))))
    (unless (wasmdown.types:func-type-match-p
             expected actual module actual-module)
      (%link-error "import ~a.~a: tag type mismatch"
                   (wasm-import-module import) (wasm-import-field import)))))

(defun %check-import (import value module)
  (case (wasm-import-kind import)
    (:func (%check-func-import import value module))
    (:table (%check-table-import import value))
    (:memory (%check-memory-import import value))
    (:global (%check-global-import import value module))
    (:tag (%check-tag-import import value module))
    (t (%link-error "unknown import kind ~a" (wasm-import-kind import)))))

(defun %resolve-imports (module imports-alist)
  "Return a list of (import . value) in import order, checking kinds
 and types. A missing or mismatched import is a link error."
  (loop for import across (wasm-module-imports module)
        for key = (list (wasm-import-module import) (wasm-import-field import))
        for entry = (assoc key imports-alist :test #'equal)
        do (unless entry
             (%link-error "unknown import: ~a.~a"
                          (wasm-import-module import) (wasm-import-field import)))
           (%check-import import (cdr entry) module)
        collect (cons import (cdr entry))))

;;; ---------------------------------------------------------------
;;; Backend bookkeeping. The counters record which backend actually
;;; executed each Wasm call, so a lane can prove that a start section or
;;; a nested core in a component really ran compiled (or interpreted)
;;; instead of trusting the requested mode.

(defparameter *backend-entry-counts* (make-hash-table :test #'eq)
  "Backend keyword -> number of Wasm function entries executed.")

(defun reset-backend-entry-counts ()
  "Zero the backend entry counters (test/lane hygiene)."
  (clrhash *backend-entry-counts*)
  (values))

(defun backend-entry-count (backend)
  "Entries executed by BACKEND (:INTERPRET or :COMPILED) since the last
reset; NIL for any other value."
  (gethash backend *backend-entry-counts* 0))

(defun backend-entry-counts ()
  "An alist of (backend . count) in :interpret, :compiled order."
  (loop for k in '(:interpret :compiled)
        collect (cons k (gethash k *backend-entry-counts* 0))))

(defun %count-backend-entry (backend)
  (incf (gethash backend *backend-entry-counts* 0)))

;;; ---------------------------------------------------------------
;;; The shared call trampoline.

(defun wasm-call (func &rest args)
  "Call a func-instance. :HOST functions receive the ARGS as one list
 and return a results list. :WASM functions run through COMPILED-ENTRY
 when one is installed (the :compiled backend), otherwise through the
 *INVOKE-WASM-FUNC* hook (the interpreter). The func-instance OBJECT is
 the same in both cases: compilation only fills the entry."
  (ecase (func-instance-kind func)
    (:host (funcall (func-instance-host-fn func) args))
    (:wasm (invoke-wasm func args))))

(defun export-value (instance name)
  "The instance object exported as NAME (a string); signals a
 wasmdown:link-error when the module has no such export."
  (let ((entry (assoc name (module-instance-exports instance) :test #'equal)))
    (unless entry
      (%link-error "unknown export: ~a" name))
    (cdr entry)))

;;; ---------------------------------------------------------------
;;; Instantiation.

(defun %const-eval (const-eval instance expr)
  "Evaluate a constant expression. The default hook is the interpreter
 implementation; CONST-EVAL may override it per call."
  (funcall (or const-eval *const-expr-evaluator*) instance expr))

(defun %alloc-funcs (module inst)
  "Fresh func-instances for the module's DEFINED functions."
  (let ((n (length (wasm-module-funcs module))))
    (when (plusp n)
      (loop with v = (make-array n)
            for i below n
            do (setf (svref v i) (make-wasm-func-instance inst i))
            finally (return v)))))

(defun %eval-elem-refs (module inst const-eval elem)
  "Evaluate an element segment's init expressions to references
 (func-instances or NIL)."
  (loop for expr in (wasm-elem-inits elem)
        collect (%const-eval const-eval inst expr)))

(defun make-module-instance (module imports-alist &key (store nil) (const-eval nil)
                                                  (before-start nil) (prepare nil))
  "Instantiate MODULE per the spec sequence:
  1. resolve + type-check imports (link-error),
  2. allocate funcs/tables/memories/globals (globals evaluate their
     init constant expressions; imported globals referenced there are
     already in place),
  3. build elem/data segment states,
  4. run PREPARE, when supplied, with the fully allocated instance,
  5. copy ACTIVE segments into their tables/memories with bounds
     checks (instantiation-error),
  6. publish exports,
  7. run BEFORE-START, when supplied, with the allocated instance,
  8. run the start function (traps propagate).

PREPARE is the backend-installation point: every object the module owns
(functions, tables, memories, globals, segment states and their refs)
exists, no guest code has run, and the active segments are still
unapplied. Compiling there means a compile failure precedes both the
start section and any table/memory mutation, and the start section then
runs through the selected backend. A condition signalled by PREPARE
unwinds the instantiation entirely (nothing is returned). It is called
exactly once.
BEFORE-START exists for host ABIs that must reach the instance's own
memory before any guest code runs — a WASI Preview1 import answers
from the memory export, and the start section may already call it.
It is called exactly once, after active segments and exports are in
place and before the Wasm start function; NIL preserves the previous
behavior. A condition signalled by the hook unwinds the whole
instantiation (nothing is returned).
IMPORTS-ALIST maps ((\"mod\" \"field\") . value)."
  (let* ((resolved (%resolve-imports module imports-alist))
         (inst (%make-module-instance :module module :store store))
         (ifuncs '()) (itables '()) (imems '()) (iglobals '()) (itags '()))
    ;; pass 1: partition imported entities by kind (import order)
    (dolist (pair resolved)
      (let ((value (cdr pair)))
        (ecase (wasm-import-kind (car pair))
          (:func (push value ifuncs))
          (:table (push value itables))
          (:memory (push value imems))
          (:global (push value iglobals))
          (:tag (push value itags)))))
    (setf ifuncs (nreverse ifuncs) itables (nreverse itables)
          imems (nreverse imems) iglobals (nreverse iglobals)
          itags (nreverse itags))
    ;; tags: imported then defined. Defined tags allocate BEFORE funcs/
    ;; globals: nothing in a constant expression references a tag, but
    ;; the start function may throw one, so the vector must be complete
    ;; before any guest code runs.
    (setf (module-instance-tags inst)
          (coerce
           (append itags
                   (loop for typeidx across (wasm-module-tags module)
                         collect (make-tag-instance
                                  inst
                                  (svref (wasm-module-types module) typeidx))))
           'vector))
    ;; funcs: imported then defined. Slot the func-instances into the
    ;; instance first so global init expressions and start bodies can
    ;; reference them via ref.func.
    (let* ((dfuncs (%alloc-funcs module inst))
           (all-funcs (coerce (append ifuncs (coerce dfuncs 'list)) 'vector)))
      (setf (module-instance-funcs inst) all-funcs)
      ;; tables / memories: imported then freshly allocated
      (let ((dtabs (loop for tab across (wasm-module-tables module)
                         collect (make-table-instance
                                  (wasm-table-type tab) (wasm-table-limits tab))))
            (dmems (loop for mem across (wasm-module-mems module)
                         collect (make-memory-instance (wasm-memory-limits mem)))))
        (setf (module-instance-tables inst)
              (coerce (append itables dtabs) 'vector)
              (module-instance-memories inst)
              (coerce (append imems dmems) 'vector))
        ;; globals: imported then defined; init exprs may reference
        ;; imported globals and this module's funcs (ref.func), and —
        ;; per the spec's imported-or-previously-defined rule —
        ;; each defined global is evaluated in order, with the
        ;; instance's globals vector grown incrementally so later
        ;; inits can global.get the earlier ones.
        (let ((all-globals (coerce iglobals 'vector)))
          ;; publish the imported globals FIRST: init exprs evaluate
          ;; against the instance's globals slot (interp-const-expr),
          ;; so global.get of imported globals must resolve before any
          ;; defined global is evaluated.
          (setf (module-instance-globals inst) all-globals)
          (loop for g across (wasm-module-globals module)
                for gi = (make-global-instance
                          (%const-eval const-eval inst (wasm-global-init g))
                          :type (wasm-global-type g)
                          :mutable (wasm-global-mutable g))
                ;; grow the slot after each global so later inits can
                ;; global.get the previously-defined ones (spec: const
                ;; exprs see the global index space up to the current
                ;; global).
                do (setf all-globals
                         (concatenate 'vector all-globals (vector gi))
                         (module-instance-globals inst) all-globals))
          ;; elem/data segment states (inits evaluated eagerly so the
          ;; segment contents are fixed before any table mutation).
          (let ((estates (coerce
                           (loop for elem across (wasm-module-elems module)
                                 collect (make-elem-seg-state
                                          :refs (%eval-elem-refs module inst const-eval elem)))
                           'vector))
                (dstates (coerce
                           (loop for data across (wasm-module-datas module)
                                 collect (make-data-seg-state
                                          :bytes (wasm-data-bytes data)))
                           'vector)))
            (setf (module-instance-elem-segs inst) (coerce estates 'vector)
                  (module-instance-data-segs inst) (coerce dstates 'vector))
            ;; the backend-installation point (see the docstring):
            ;; allocation is complete, active segments are not applied.
            (when prepare (funcall prepare inst))
            ;; active element segments -> tables
            (loop for elem across (wasm-module-elems module)
                  for state across estates
                  for mode = (wasm-elem-mode elem)
                  when (and (consp mode) (eq (first mode) :active))
                    do (let ((table (svref (module-instance-tables inst)
                                           (second mode)))
                             (offset (%const-eval const-eval inst (third mode)))
                             (refs (elem-seg-state-refs state)))
                         (unless (<= (+ offset (length refs))
                                     (table-size table))
                           (%inst-error
                            "element segment out of bounds: ~d + ~d into table of ~d"
                            offset (length refs) (table-size table)))
                         (loop for i from 0
                               for ref in refs
                               do (table-set table (+ offset i) ref))
                         ;; the spec implicitly drops ACTIVE segments
                         ;; after initialization: table.init from
                         ;; them traps OOB ever after (elem.wast
                         ;; ::80). DECLARATIVE segments are dropped
                         ;; without ever initializing anything
                         ;; (elem.wast ::82).
                         (setf (elem-seg-state-dropped-p state) t
                               (elem-seg-state-refs state) '())))
            (loop for elem across (wasm-module-elems module)
                  for state across estates
                  for mode = (wasm-elem-mode elem)
                  when (and (consp mode) (eq (first mode) :declarative))
                    do (setf (elem-seg-state-dropped-p state) t
                             (elem-seg-state-refs state) '()))
            ;; active data segments -> memories
            (loop for data across (wasm-module-datas module)
                  for state across dstates
                  for mode = (wasm-data-mode data)
                  when (and (consp mode) (eq (first mode) :active))
                    do (let* ((mem (svref (module-instance-memories inst)
                                          (second mode)))
                              (offset (%const-eval const-eval inst (third mode)))
                              (bytes (data-seg-state-bytes state))
                              (len (length bytes)))
                         (unless (<= (+ offset len)
                                     (length (memory-instance-bytes mem)))
                           (%inst-error
                            "data segment out of bounds: ~d + ~d into memory of ~d"
                            offset len (length (memory-instance-bytes mem))))
                         (replace (memory-instance-bytes mem) bytes
                                  :start1 offset)))
            ;; exports
            (setf (module-instance-exports inst)
                  (loop for ex across (wasm-module-exports module)
                        for kind = (wasm-export-kind ex)
                        for idx = (wasm-export-index ex)
                        for value = (ecase kind
                                      (:func (svref all-funcs idx))
                                      (:table (svref (module-instance-tables inst) idx))
                                      (:memory (svref (module-instance-memories inst) idx))
                                      (:global (svref (module-instance-globals inst) idx))
                                      (:tag (svref (module-instance-tags inst) idx)))
                        collect (cons (wasm-export-name ex) value)))
            ;; the host ABI's pre-start hook: the allocated instance is
            ;; complete (imports, memories, active segments, exports) and
            ;; no guest code has run yet.
            (when before-start
              (funcall before-start inst))
            ;; start function: run after exports exist so it may call
            ;; exported paths; traps propagate untouched.
            (when (wasm-module-start module)
              (wasm-call (svref all-funcs (wasm-module-start module))))
            inst))))))
