;;;; Runtime objects and module instantiation (PLAN.md §7, CONTRACTS.md
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
(defparameter *max-pages* 1024)          ; impl limit (PLAN.md 3.4): 64 MiB
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
individual arguments returning a results list."
  (kind :wasm :type (member :wasm :host))
  (wasm-funcidx 0 :type (integer 0))
  module
  host-fn
  type)

(defstruct (memory-instance (:constructor %make-memory-instance))
  "Linear memory: BYTES is a (vector (unsigned-byte 8)) of PAGES pages."
  (bytes (make-array 0 :element-type '(unsigned-byte 8))
         :type (vector (unsigned-byte 8)))
  (pages 0 :type (integer 0))
  (max-pages *max-pages* :type (integer 0))
  (has-max-p nil :type boolean))

(defstruct (table-instance (:constructor %make-table-instance))
  "A table of references; ELEMS is a simple vector of func-instances /
host objects / NIL (null ref)."
  (elems (make-array 0) :type simple-vector)
  (type :funcref :type keyword)
  (max *max-table-size* :type (integer 0))
  (has-max-p nil :type boolean))

(defstruct (global-instance (:constructor %make-global-instance))
  (value 0)
  (mutable nil :type boolean)
  (type :i32 :type keyword))

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
     :has-max-p (wasm-limits-has-max-p limits))))

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
           :message "no wasm function executor installed (load wasmdown.interp)")))

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
  "Grow by DELTA pages. Returns the old page count on success, 4294967295
 (i32 -1) when the limit is exceeded. On success the byte vector is
 replaced by a larger one that preserves contents and zero-fills."
  (let* ((old (memory-instance-pages mem))
         (new (+ old delta)))
    (if (or (> new *max-pages*) (> new (memory-instance-max-pages mem)))
        4294967295
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
         (actual (func-instance-type value)))
    (unless (and (equal (wasm-func-type-params expected)
                         (wasm-func-type-params actual))
                 (equal (wasm-func-type-results expected)
                         (wasm-func-type-results actual)))
      (%link-error "import ~a.~a: function type mismatch"
                   (wasm-import-module import) (wasm-import-field import)))))

(defun %check-table-import (import value)
  (unless (table-instance-p value)
    (%link-error "import ~a.~a: expected a table, got ~s"
                 (wasm-import-module import) (wasm-import-field import)
                 (type-of value)))
  (let ((expected (wasm-import-type import)))
    (unless (eq (wasm-table-type expected) (table-instance-type value))
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

(defun %check-global-import (import value)
  (unless (global-instance-p value)
    (%link-error "import ~a.~a: expected a global, got ~s"
                 (wasm-import-module import) (wasm-import-field import)
                 (type-of value)))
  (let* ((spec (wasm-import-type import))   ; (valtype . mutable-p)
         (type (car spec))
         (mutable (cdr spec)))
    (unless (and (eq type (global-instance-type value))
                 (eq mutable (global-instance-mutable value)))
      (%link-error "import ~a.~a: global type or mutability mismatch"
                   (wasm-import-module import) (wasm-import-field import)))))

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
         (actual (tag-instance-type value)))
    (unless (and (equal (wasm-func-type-params expected)
                        (wasm-func-type-params actual))
                 (equal (wasm-func-type-results expected)
                        (wasm-func-type-results actual)))
      (%link-error "import ~a.~a: tag type mismatch"
                   (wasm-import-module import) (wasm-import-field import)))))

(defun %check-import (import value module)
  (case (wasm-import-kind import)
    (:func (%check-func-import import value module))
    (:table (%check-table-import import value))
    (:memory (%check-memory-import import value))
    (:global (%check-global-import import value))
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
;;; The shared call trampoline.

(defun wasm-call (func &rest args)
  "Call a func-instance. :HOST functions receive the ARGS as one list
 and return a results list; :WASM functions go through the
 *INVOKE-WASM-FUNC* hook (the interpreter by default; codegen replaces
 it) which receives (func args-list) and returns a results list."
  (ecase (func-instance-kind func)
    (:host (funcall (func-instance-host-fn func) args))
    (:wasm (funcall *invoke-wasm-func* func args))))

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

(defun make-module-instance (module imports-alist &key (store nil) (const-eval nil))
  "Instantiate MODULE per the spec sequence:
  1. resolve + type-check imports (link-error),
  2. allocate funcs/tables/memories/globals (globals evaluate their
     init constant expressions; imported globals referenced there are
     already in place),
  3. build elem/data segment states,
  4. copy ACTIVE segments into their tables/memories with bounds
     checks (instantiation-error),
  5. run the start function (traps propagate),
  6. resolve exports.
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
            ;; start function: run after exports exist so it may call
            ;; exported paths; traps propagate untouched.
            (when (wasm-module-start module)
              (wasm-call (svref all-funcs (wasm-module-start module))))
            inst))))))
