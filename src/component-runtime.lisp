;;;; Component INSTANTIATION EXECUTION (M8): the runtime half of the
;;;; component model. src/component.lisp owns the STATIC machinery
;;;; (decode + validate); this file walks a VALIDATED comp's flat
;;;; definition sequence and EXECUTES it:
;;;;   * (core module $M ...) defs carry the parsed wasm-module IR —
;;;;     nothing to execute at def time;
;;;;   * (core instance (instantiate $M (with "a" (instance $i))))
;;;;     runs make-module-instance with the two-level import rule
;;;;     (Explainer.md 'Instance Definitions' 277-300): first name ->
;;;;     the with-arg core instance; second name -> that instance's
;;;;     export (export-value; inline-export bags resolve by name);
;;;;   * (instance (instantiate $C (with "x" <sortidx>))) recursively
;;;;     walks the child component's defs with each import bound to
;;;;     its with-arg's runtime value;
;;;;   * aliases project runtime values out of the records (an
;;;;     :export alias off a component instance, a :core-export alias
;;;;     off a core instance, an :outer alias off the enclosing
;;;;     comp-instance chain);
;;;;   * canon lift wraps a core func as a lifted (callable) func —
;;;;     FLAT arguments only in this slice (i32/i64/f32/f64); the
;;;;     string/list/record memory shapes are the values-suite slice.
;;;;     Lower/resource-*/builtin canons introduce their core-func
;;;;     index slot (opaque until the values slice);
;;;;   * exports populate the record + introduce their index;
;;;;   * start funcs run LAST (traps propagate untouched).
;;;; The executor is INDEX-SPACE COMPATIBLE with validate-component:
;;;; every sort's vector grows in exactly the order the validator's
;;;; counters do (the validator is the oracle for what exists; the
;;;; runtime keeps the parallel VALUE spaces).

(defpackage #:wasmdown.component.runtime
  (:use #:cl #:wasmdown.types #:wasmdown.component)
  (:export #:comp-instance
           #:comp-instance-p
           #:comp-instance-exports
           #:comp-instance-comp
           #:comp-instance-imports
           #:instantiate-component
           #:lifted-func
           #:lifted-func-p
           #:closed-comp-p
           #:closed-comp-comp
           #:closed-comp-instance
           #:make-lifted-func
           #:lifted-func-core
           #:lifted-func-type
           #:lifted-func-opts
           #:lifted-func-memory
           #:lifted-func-realloc
           #:lifted-func-call
           #:bag-p
           #:bag-ref
           #:runtime-export-value))
(in-package #:wasmdown.component.runtime)

(declaim (notinline bag-p bag-ref make-lifted-func lifted-func-call
                    runtime-export-value instantiate-component))

;;; ---------------------------------------------------------------
;;; Runtime records.

(defstruct comp-instance
  "The runtime counterpart of a validated comp: the per-sort index
vectors built by the def walk (parallel to validate-component's
counters) + the import/export tables."
  (comp nil)                         ; the source comp (diagnostics)
  (core-modules (make-array 8 :fill-pointer 0 :adjustable t))
  (core-instances (make-array 8 :fill-pointer 0 :adjustable t))
  (core-types (make-array 8 :fill-pointer 0 :adjustable t))
  (core-funcs (make-array 8 :fill-pointer 0 :adjustable t))
  (core-memories (make-array 8 :fill-pointer 0 :adjustable t))
  (core-tables (make-array 8 :fill-pointer 0 :adjustable t))
  (core-globals (make-array 8 :fill-pointer 0 :adjustable t))
  (core-tags (make-array 8 :fill-pointer 0 :adjustable t))
  (funcs (make-array 8 :fill-pointer 0 :adjustable t))
  (values (make-array 8 :fill-pointer 0 :adjustable t))
  (components (make-array 8 :fill-pointer 0 :adjustable t))
  (instances (make-array 8 :fill-pointer 0 :adjustable t))
  (types (make-array 8 :fill-pointer 0 :adjustable t))
  (imports (make-hash-table :test #'equal)) ; name -> runtime value
  (exports (make-hash-table :test #'equal))) ; name -> (sort . value)

(defstruct lifted-func
  "A canon-lifted component function: the wrapped core func-instance,
the component functype (ct) and the resolved canon options (memory +
realloc resolved to runtime instances at lift time). INST (M9) is
the DEFINING comp-instance — the own/borrow handle transfer
  (lift_own/lower_own) reads and writes ITS handle table."
  (core nil)                         ; func-instance | NIL (opaque)
  (type nil)                         ; the component functype ct | NIL
  (opts nil)
  (memory nil)                       ; memory-instance | NIL
  (realloc nil)                      ; func-instance | NIL
  (inst nil))                        ; the defining comp-instance (M9)

;;; ---------------------------------------------------------------
;;; Bags: inline-export instances. A bag is a hash table mapping the
;;; export name -> runtime value (a bag of ALREADY-RESOLVED values —
;;; the def walk resolves each inline sortidx at bag-build time).

(defun bag-p (v)
  (and (hash-table-p v) (gethash :%bag v)))

(defun bag-ref (bag name)
  (gethash name bag))

(declaim (notinline bag-p bag-ref))

;;; ---------------------------------------------------------------
;;; Index-space growth (each mirrors validate-component's counters).

(defmacro %vpush (vec value)
  `(vector-push-extend ,value (,vec inst)))

(declaim (notinline %push-core-module %push-core-instance %push-core-func
                    %push-core-memory %push-core-table %push-core-global
                    %push-core-tag %push-func %push-component
                    %push-instance %push-type %push-value))

(defun %push-core-module (inst v) (%vpush comp-instance-core-modules v))
(defun %push-core-instance (inst v) (%vpush comp-instance-core-instances v))
(defun %push-core-func (inst v) (%vpush comp-instance-core-funcs v))
(defun %push-core-memory (inst v) (%vpush comp-instance-core-memories v))
(defun %push-core-table (inst v) (%vpush comp-instance-core-tables v))
(defun %push-core-global (inst v) (%vpush comp-instance-core-globals v))
(defun %push-core-tag (inst v) (%vpush comp-instance-core-tags v))
(defun %push-func (inst v) (%vpush comp-instance-funcs v))
(defun %push-value (inst v) (%vpush comp-instance-values v))
(defun %push-component (inst v) (%vpush comp-instance-components v))
(defun %push-instance (inst v) (%vpush comp-instance-instances v))
(defun %push-type (inst v) (%vpush comp-instance-types v))

(declaim (notinline %push-sort))

(defun %push-sort (inst sort value)
  "Push VALUE into the index space of SORT (the slot an alias/export/
import of that sort introduces)."
  (case sort
    (:core-module (%push-core-module inst value))
    (:core-instance (%push-core-instance inst value))
    (:core-func (%push-core-func inst value))
    (:core-memory (%push-core-memory inst value))
    (:core-table (%push-core-table inst value))
    (:core-global (%push-core-global inst value))
    (:core-tag (%push-core-tag inst value))
    (:core-type (vector-push-extend :core-type
                                    (comp-instance-core-types inst)))
    (:func (%push-func inst value))
    (:value (%push-value inst value))
    (:component (%push-component inst value))
    (:instance (%push-instance inst value))
    (:type (%push-type inst value))
    (t (error 'wasmdown:link-error
              :message (format nil "bad sort ~a" sort)))))

;;; ---------------------------------------------------------------
;;; Sortidx resolution: the RUNTIME VALUE at (sort . idx). The
;;; validator already bounds-checked; out-of-bounds here is an
;;; internal-consistency link-error (never silently wrong).

(declaim (notinline %sort-value))

(defun %sort-value (inst si)
  (let ((idx (sortidx-idx si))
        (v nil)
        (ok nil))
    (macrolet ((at (vec)
                 `(let ((n (length (,vec inst))))
                    (when (< idx n)
                      (setf v (aref (,vec inst) idx) ok t)))))
      (case (sortidx-sort si)
        (:core-module (at comp-instance-core-modules))
        (:core-instance (at comp-instance-core-instances))
        (:core-func (at comp-instance-core-funcs))
        (:core-memory (at comp-instance-core-memories))
        (:core-table (at comp-instance-core-tables))
        (:core-global (at comp-instance-core-globals))
        (:core-tag (at comp-instance-core-tags))
        (:core-type (at comp-instance-core-types))
        (:func (at comp-instance-funcs))
        (:value (at comp-instance-values))
        (:component (at comp-instance-components))
        (:instance (at comp-instance-instances))
        (:type (at comp-instance-types))
        (t (error 'wasmdown:link-error
                  :message (format nil "bad sortidx sort ~a"
                                  (sortidx-sort si))))))
    (unless ok
      (error 'wasmdown:link-error
             :message (format nil "~a index out of bounds at runtime"
                              (sortidx-sort si))))
    v))

;;; ---------------------------------------------------------------
;;; Export projections (the alias arms).

(declaim (notinline %instance-export-value %core-instance-export-value))

(defun %instance-export-value (source name)
  "A component instance's export NAME as a runtime value: a
comp-instance's export table, a bag's entry, or NIL (an imported
instance hole)."
  (typecase source
    (comp-instance
     (let ((hit (gethash name (comp-instance-exports source) :missing)))
       (if (eq hit :missing) nil (cdr hit))))
    (hash-table (gethash name source))
    (t nil)))

(defun %core-instance-export-value (source name)
  "A core instance's export NAME: a runtime module-instance's
export-value (link-error when missing) or a bag's entry."
  (typecase source
    (wasmdown.runtime:module-instance
     (wasmdown.runtime:export-value source name))
    (hash-table (or (gethash name source)
                    (error 'wasmdown:link-error
                           :message
                           (format nil "unknown export: ~a" name))))
    (t (error 'wasmdown:link-error
              :message (format nil "core instance has no export: ~a"
                               name)))))

(defun runtime-export-value (inst name)
  "A comp-instance's export as a runtime value (the public lookup)."
  (%instance-export-value inst name))

;;; ---------------------------------------------------------------
;;; Two-level core instantiation (Explainer.md 277-300).

(declaim (notinline %core-imports-alist))

(defun %core-imports-alist (module args)
  "The imports-alist for make-module-instance: each two-level import
(module field) resolves as module -> the with-arg named module;
field -> that instance's export."
  (loop for import across (wasm-module-imports module)
        for mod = (wasm-import-module import)
        for field = (wasm-import-field import)
        for arg = (cdr (assoc mod args :test #'string=))
        do (unless arg
             (error 'wasmdown:link-error
                    :message
                    "missing module instantiation argument"))
        collect (cons (list mod field)
                       (if (bag-p arg)
                           (bag-ref arg field)
                           (wasmdown.runtime:export-value arg field)))))

;;; ---------------------------------------------------------------
;;; The def walk.

(defvar *outer-instances* nil
  "The enclosing comp-instances for outer alias resolution (innermost
first; a count of N resolves the Nth entry).")

(defvar *resource-id-counter* 0
  "The runtime resource identity counter (resources are generative
per instantiation; the counter never resets, mirroring the
validator's).")

(defun %fresh-resource-id ()
  (cons :fresh (incf *resource-id-counter*)))

(declaim (notinline instantiate-component %exec-core-instance
                    %exec-instance %exec-alias %exec-canon
                    %exec-import %exec-export))

(defun instantiate-component (comp &key (with-args nil) (outer nil))
  "Execute a VALIDATED comp: walk its defs in file order, allocating
core instances, nested component instances, lifted funcs and
exports; run the start functions last (traps propagate untouched).
WITH-ARGS is an alist of import name -> runtime value; OUTER is the
enclosing comp-instance chain (innermost first) for outer aliases.
Returns the comp-instance."
  (let ((inst (make-comp-instance :comp comp))
        (starts '()))
    ;; with-args pre-bind: an :import def reads the bound value by name
    (dolist (pair with-args)
      (setf (gethash (car pair) (comp-instance-imports inst)) (cdr pair)))
    (let ((*outer-instances* (cons inst (or outer *outer-instances*))))
      (loop for d across (comp-defs comp)
            for payload = (compdef-payload d)
            do (case (compdef-kind d)
                 (:core-module
                  (%push-core-module inst payload))
                 (:core-instance
                  (%exec-core-instance payload inst))
                 (:core-type
                  ;; a core type's runtime slot: the rectype payload
                  (vector-push-extend payload
                                      (comp-instance-core-types inst)))
                 (:component
                  (%push-component inst payload))
                 (:instance
                  (%exec-instance payload inst))
                 (:alias
                  (%exec-alias payload inst))
                 (:type
                  ;; the runtime type slot: the ct itself. M9: a
                  ;; RESOURCE type's dtor index is resolved HERE — in
                  ;; the DEFINING instance's core-func space — and
                  ;; recorded per (ct . instance); the drop canon
                  ;; (which may run in a CONSUMER's walk) reads the
                  ;; ORIGIN's dtor, never its own space's slot.
                  (when (and (wasmdown.component:ct-resourcetype-p
                              payload)
                             (integerp (cdr
                                         (wasmdown.component:ct-payload
                                          payload)))
                             (< (cdr (wasmdown.component:ct-payload
                                      payload))
                                (length (comp-instance-core-funcs
                                         inst))))
                    (setf (gethash (cons payload inst)
                                   *resource-dtor-table*)
                          (aref (comp-instance-core-funcs inst)
                                (cdr (wasmdown.component:ct-payload
                                      payload)))))
                  (%push-type inst payload))
                 (:canon
                  (%exec-canon payload inst))
                 (:start
                  (push payload starts))
                 (:import
                  (%exec-import payload inst))
                 (:export
                  (%exec-export payload inst))
                 (:value
                  ;; value defs: the values-suite slice; the index
                  ;; space grows so later indices stay aligned
                  (%push-value inst nil))))
      ;; start funcs run LAST, after every def: a start may touch any
      ;; index; the args are value indices (values slice: none yet)
      (dolist (s (nreverse starts))
        (let ((f (and (< (startrec-func s)
                         (length (comp-instance-funcs inst)))
                      (aref (comp-instance-funcs inst)
                            (startrec-func s)))))
          (when (and f (or (wasmdown.runtime:func-instance-p f)
                           (lifted-func-p f)))
            (if (lifted-func-p f)
                (%call-lifted0 f)
                (wasmdown.runtime:wasm-call f))))))
    inst))

(defun %call-lifted0 (lf)
  "A start-function call of a lifted func with no args."
  (lifted-func-call lf))

;;; ---- core instance ----

(defun %exec-core-instance (payload inst)
  "One coreinstrec: instantiate a core module (its two-level with-args
resolve through THIS instance's core-instance vector) or build an
inline-export BAG."
  (ecase (coreinstrec-kind payload)
    (:instantiate
     (let* ((midx (coreinstrec-index payload))
            (module (and (< midx (length (comp-instance-core-modules inst)))
                         (aref (comp-instance-core-modules inst) midx))))
       (unless (and module (wasm-module-p module))
         (error 'wasmdown:link-error
                :message (format nil "module index out of bounds")))
       (let* ((args (loop for (name . iidx)
                            across (coreinstrec-args payload)
                          collect (cons name
                                        (aref (comp-instance-core-instances
                                               inst)
                                              iidx))))
              (mi (wasmdown.runtime:make-module-instance
                   module (%core-imports-alist module args))))
         (%push-core-instance inst mi))))
    (:inline-exports
     (let ((bag (make-hash-table :test #'equal)))
       (setf (gethash :%bag bag) t)
       (loop for (name . si) across (coreinstrec-exports payload)
             do (setf (gethash name bag) (%sort-value inst si)))
       (%push-core-instance inst bag)))))

;;; ---- component instance ----

(defun %exec-instance (payload inst)
  "One instrec: instantiate a nested component recursively (the
with-args resolve to runtime values in THIS instance's spaces) or
build an inline-export bag instance."
  (ecase (instrec-kind payload)
    (:instantiate
     (let* ((cidx (instrec-index payload))
            (child (and (< cidx (length (comp-instance-components inst)))
                        (aref (comp-instance-components inst) cidx))))
       (unless child
         (error 'wasmdown:link-error
                :message (format nil "component index out of bounds")))
       ;; M9 (linking ::233): a child instantiated via an
       ;; alias-projected component closes over the EXPORTING
       ;; instance — the closure table records (exporter . comp) ->
       ;; source; the depth-1 outer chain prepends it (deeper
       ;; counts walk past it to the enclosing sites). No closure
       ;; record = the plain chain (the ::169 direct defs).
       (let* ((closure (gethash (cons inst cidx) *comp-closure*))
              (outer (if closure
                         (cons closure *outer-instances*)
                         *outer-instances*))
              (args (loop for (name . si) across (instrec-args payload)
                         collect (cons name (%sort-value inst si))))
              (ci (instantiate-component child :with-args args
                                          :outer outer)))
         (%push-instance inst ci))))
    (:inline-exports
     (let ((bag (make-hash-table :test #'equal)))
       (setf (gethash :%bag bag) t)
       ;; M9 (linking ::79): component-level bag exports carry the
       ;; shape ((name attrs) . sortidx) — the RUNTIME key is the
       ;; name string; the coreinstrec inline path uses (name . si)
       ;; with a bare string head (the two parsers differ), so read
       ;; the head defensively like the validator's attribute walk.
       (loop for (head . si) across (instrec-exports payload)
             for name = (if (consp head) (first head) head)
             do (setf (gethash name bag) (%sort-value inst si)))
       (%push-instance inst bag)))))

;;; ---- alias ----

(defun %exec-alias (payload inst)
  "One aliasrec: project the runtime value and push it at the alias's
sort (the alias introduces one index of its sort)."
  (case (aliasrec-kind payload)
    (:export
     (let ((source (and (< (aliasrec-index payload)
                           (length (comp-instance-instances inst)))
                          (aref (comp-instance-instances inst)
                                (aliasrec-index payload)))))
       (let ((v (%instance-export-value source
                                        (aliasrec-name payload))))
         ;; M9 (linking ::93): a RESOURCE ct projected out of a
         ;; specific instance — record the SOURCE so the drop
         ;; canon's dtor lookup keys the right defining instance
         (when (and (eq (aliasrec-sort payload) :type)
                    (wasmdown.component:ct-resourcetype-p v))
           ;; M9 (linking ::117): key by (THIS instance . the type
           ;; slot the alias just introduced) — the ct object is
           ;; SHARED across the defining component's instantiations,
           ;; so a bare-ct record leaks the LAST alias's source (both
           ;; drop canons resolved to one definer and the other's
           ;; dtor never ran)
           (setf (gethash (cons inst
                                (length
                                 (comp-instance-types inst)))
                          *ct-defining-inst*)
                 source)
           (setf (gethash v *ct-defining-inst*) source))
         ;; M9 (linking ::233): record the closure for an
         ;; alias-projected COMPONENT — (source inst . comp) -> src
         (when (eq (aliasrec-sort payload) :component)
           ;; key by the SLOT the alias occupies (the component index
           ;; it just introduced) within THIS instance — two aliases
           ;; projecting the SAME comp from DIFFERENT sources keep
           ;; separate records
           (setf (gethash (cons inst
                                (length
                                 (comp-instance-components inst)))
                          *comp-closure*)
                 source))
         (%push-sort inst (aliasrec-sort payload) v))))
    (:core-export
     (let ((source (and (< (aliasrec-index payload)
                           (length (comp-instance-core-instances inst)))
                        (aref (comp-instance-core-instances inst)
                              (aliasrec-index payload)))))
       (%push-sort inst (aliasrec-sort payload)
                   (%core-instance-export-value
                    source (aliasrec-name payload)))))
    (:outer
     ;; count 0 = the CURRENT instance; N = the Nth enclosing
     ;; comp-instance (the executor conses THIS instance at the head,
     ;; so the chain IS [this, outer1, outer2, ...])
     (let* ((chain *outer-instances*)
            (target (nth (aliasrec-count payload) chain)))
       (%push-sort inst (aliasrec-sort payload)
                   (%outer-alias-value target payload))))))

(defun %outer-alias-value (target payload)
  "The runtime value an outer alias projects out of TARGET: static
sorts (core-module/component/type) alias the target's vector
directly; a core-type aliases the type space; other sorts have no
runtime value this slice (the index grows, the value is opaque)."
  (unless target
    (error 'wasmdown:link-error
           :message (format nil "invalid outer alias count of ~d"
                            (aliasrec-count payload))))
  (let ((idx (aliasrec-target payload)))
    (case (aliasrec-sort payload)
      (:core-module
       (aref (comp-instance-core-modules target) idx))
      (:component
       (aref (comp-instance-components target) idx))
      (:type
       (aref (comp-instance-types target) idx))
      (:core-type
       (and (< idx (length (comp-instance-core-types target)))
            (aref (comp-instance-core-types target) idx)))
      ;; funcs/instances/values have no outer-alias runtime value in
      ;; this slice (the validator rejects the other sorts at decode)
      (t nil))))

;;; ---- canon ----

(defvar *resource-dtor-table* (make-hash-table :test #'equal)
  "M9: (resource-ct . defining-comp-instance) -> the dtor
func-instance, resolved at the TYPE def walk in the DEFINING
instance's core-func space. A consumer's resource.drop canon reads
the ORIGIN's dtor — the ct's payload dtor index is meaningful only
there.")

(defstruct closed-comp
  "M9 (linking ::233): an alias-projected COMPONENT value captured at
the export site — the comp plus the SOURCE comp-instance (the closure
for the child's depth-1 outer aliases: an exported component closes
over the instance that exported it, so instantiating it later resolves
its outer refs through THAT instance, not the instantiating site's)."
  (comp nil)
  (instance nil))

(defvar *comp-closure* (make-hash-table :test #'equal)
  "M9 (linking ::233): (exporting-inst . projected-comp) -> the
SOURCE comp-instance an alias-projected component was exported from.
The instantiate arm consults it: a child instantiated via an alias
resolves its depth-1 outer refs through the EXPORTING instance (the
closure), prepended to the outer chain. No value-shape change — the
components vector holds the RAW comp (the ::169 export chains see
exactly what they always did).")

(defvar *ct-defining-inst* (make-hash-table :test #'equal)
  "M9 (linking ::93): resource-ct -> the comp-instance whose exports
PROJECTED it (an (alias export $i \"r\" (type)) at the consumer).
The drop canon's dtor lookup keys the dtor table by the DEFINING
instance — with two instantiations of the defining component the ct
object is shared but each instance has its OWN dtor func, so the
first table hit leaks the wrong instance's counter.")

(defvar *handle-caller-inst* nil
  "M9 (linking ::126): the comp-instance whose handle table an
incoming own/borrow PARAM index refers to — bound dynamically at the
lower's %call-component-func so the target lifted func's lift_own
consumes the handle from the CALLER's table and re-enters its own.")


(defun %exec-canon (payload inst)
  "One canonrec. A LIFT wraps the target core func (allocating the
lifted func in the component FUNC space; the memory/realloc options
resolve to runtime instances). M9: a LOWER wraps the target
COMPONENT func as a real host core func (the flat sync path); the
resource built-ins wrap the per-instance handle table (new/drop/rep
with the dtor resolved when the resource type declares one). The
remaining built-ins stay opaque core-func slots (the values slice)."
  (case (canonrec-kind payload)
    (:lift
     (let* ((cidx (canonrec-func payload))
            (core (and (< cidx (length (comp-instance-core-funcs inst)))
                       (aref (comp-instance-core-funcs inst) cidx)))
            (opts (canonrec-opts payload))
            (mem-opt (wasmdown.component:canon-opt opts :memory))
            (realloc-opt (wasmdown.component:canon-opt opts :realloc))
            (tidx (canonrec-typeidx payload))
            (ft (and tidx (< tidx (length (comp-instance-types inst)))
                     (aref (comp-instance-types inst) tidx))))
       (unless (< cidx (length (comp-instance-core-funcs inst)))
         (error 'wasmdown:link-error
                :message (format nil
                                 "core function index out of bounds: ~
                                  canon lift wants core func ~d of ~d"
                                 cidx
                                 (length (comp-instance-core-funcs inst)))))
       ;; CORE may be NIL: an opaque core-func slot (a resource.* or
       ;; builtin canon target, or a lower whose behavior is the
       ;; values slice). The WRAPPER is created — calling it later
       ;; signals host-error (lifted-func-call).
       (%push-func
        inst
        (make-lifted-func
         :core core :type ft :opts opts
         :memory (when (and mem-opt
                            (< (second mem-opt)
                               (length (comp-instance-core-memories
                                        inst))))
                   (aref (comp-instance-core-memories inst)
                         (second mem-opt)))
         :realloc (when (and realloc-opt
                             (< (second realloc-opt)
                                (length (comp-instance-core-funcs inst))))
                    (aref (comp-instance-core-funcs inst)
                          (second realloc-opt)))
         ;; M9: the defining instance — the own/borrow handle
         ;; transfer (lift_own) reads ITS handle table
         :inst inst))))
    ((:lower :resource-new :resource-drop :resource-rep :builtin)
     ;; M9: the merged canon dispatch: a LOWER wraps the target
     ;; COMPONENT func as a REAL host core func; the resource
     ;; built-ins wrap THIS instance's handle table; the remaining
     ;; built-ins stay opaque (values slice).
     (let* ((kind (canonrec-kind payload))
            (rkind (if (member kind
                               '(:resource-new :resource-drop
                                 :resource-rep))
                       kind
                       (first (canonrec-imms payload))))
            (tidx (if (member kind
                              '(:resource-new :resource-drop
                                :resource-rep))
                      (canonrec-typeidx payload)
                      (second (canonrec-imms payload))))
            (ct (and (integerp tidx)
                     (< tidx (length (comp-instance-types inst)))
                     (aref (comp-instance-types inst) tidx)))
            (rt ct)
            (dtor (or (and ct
                          (eq rkind :resource-drop)
                          ;; M9 (linking ::93): key by the ct's
                          ;; PROJECTING instance first — the ct object
                          ;; is shared across instantiations of the
                          ;; defining component, so a bare-ct scan
                          ;; leaks another instance's dtor; fall back
                          ;; to the first-hit scan (single-instance
                          ;; fixtures) then the local slot
                          (or (gethash (cons ct inst)
                                       *resource-dtor-table*)
                              ;; M9 (linking ::117): the canon's OWN
                              ;; (instance . type-slot) record — the
                              ;; alias that introduced THIS slot named
                              ;; its source instance
                              (let ((src (gethash (cons inst tidx)
                                                  *ct-defining-inst*)))
                                (and src
                                     (gethash (cons ct src)
                                              *resource-dtor-table*)))
                              (let ((src (gethash ct
                                                 *ct-defining-inst*)))
                                (and src
                                     (gethash (cons ct src)
                                              *resource-dtor-table*)))
                              (block origin
                                (maphash
                                 (lambda (key val)
                                   (when (and (eq (car key) ct)
                                              (wasmdown.runtime:func-instance-p
                                               val))
                                     (return-from origin val)))
                                 *resource-dtor-table*)
                                nil)))
                     (and ct
                          (wasmdown.component:ct-resourcetype-p ct)
                          (integerp (cdr (wasmdown.component:ct-payload
                                          ct)))
                          (< (cdr (wasmdown.component:ct-payload ct))
                             (length (comp-instance-core-funcs inst)))
                          (aref (comp-instance-core-funcs inst)
                                (cdr (wasmdown.component:ct-payload
                                      ct)))))))
       (cond
        ((and (eq kind :lower)
              (< (canonrec-func payload)
                 (length (comp-instance-funcs inst)))
              (aref (comp-instance-funcs inst)
                    (canonrec-func payload)))
         (%push-core-func
          inst
          (%lower-host-func inst (canonrec-func payload))))
        ((and (member rkind
                      '(:resource-new :resource-drop :resource-rep))
              rt)
         (%push-core-func
          inst
          (%resource-builtin-host-func inst rkind rt dtor)))
        (t (%push-core-func inst nil)))))))

;;; ---- import ----

(defun %exec-import (payload inst)
  "One importrec: the sort's index grows; the VALUE is the with-arg
bound under the import's name (a runtime value supplied by the
parent, or a hole — :missing keeps the index aligned; the type arg
of an instantiation binds the ct)."
  (let* ((et (importrec-type payload))
         (sort (car et))
         (name (importrec-name payload))
         (value (gethash name (comp-instance-imports inst) nil)))
    (ecase sort
      ;; the externtype grammar: exactly these five sorts
      (:func (%push-func inst value))
      (:value (%push-value inst value))
      (:instance (%push-instance inst value))
      (:type (%push-type inst value))
      (:component (%push-component inst value))
      (:core-module (%push-core-module inst value)))))

;;; ---- export ----

(defun %exec-export (payload inst)
  "One exportrec: the export table gains (name -> (sort . value)) AND
the export introduces one index of its sort (the validator's :export
arm; the introduced type re-exports the source ct)."
  (let* ((si (exportrec-sortidx payload))
         (value (%sort-value inst si)))
    (setf (gethash (exportrec-name payload) (comp-instance-exports inst))
          (cons (sortidx-sort si) value))
    (case (sortidx-sort si)
      (:func (%push-func inst value))
      (:value (%push-value inst value))
      (:instance (%push-instance inst value))
      (:component (%push-component inst value))
      (:type (%push-type inst value))
      (:core-module (%push-core-module inst value))
      ;; the core instance/func/... spaces: a component-level export
      ;; of a core sort introduces a CORE index (the validator's
      ;; :core-module arm is the visible one; the others grow too)
      (:core-instance (%push-core-instance inst value))
      (:core-func (%push-core-func inst value))
      (:core-memory (%push-core-memory inst value))
      (:core-table (%push-core-table inst value))
      (:core-global (%push-core-global inst value))
      (:core-tag (%push-core-tag inst value))
      (:core-type (vector-push-extend :exported
                                      (comp-instance-core-types inst))))))

;;; ---------------------------------------------------------------
;;; The flat canonical call path (CanonicalABI.md lift/lower): FLAT
;;; numeric params/results pass through with the prim->core mapping;
;;; anything needing memory (string/list/record/variant) is a
;;; values-suite shape and signals host-error when CALLED (creating
;;; the wrapper stays legal).

(declaim (notinline lifted-func-call %valtype-flat-core))

;;; The flat CORE type of a component valtype: the numeric prims map
;;; to i32/i64/f32/f64; OWN/BORROW handles ride i32 (CanonicalABI.md
;;; lower_own/lower_borrow: the handle-table index); a STRING is a
;;; [ptr,len] pair (NOT flat-callable as a single core value — the
;;; string path goes through the lift memory). INST is the runtime
;;; instance whose TYPE space resolves (:idx . n) references; the
;;; prim keyword passes through directly.
(defun %valtype-prim-of (vt inst)
  "Resolve a valtype to its PRIMITIVE keyword when flat: a prim
keyword as-is; an (:idx . n) reference resolving to an :own/:borrow
ct's prim (i32); anything else is a values-suite shape."
  (declare (ignore inst))
  (typecase vt
    (keyword vt)
    (cons
     (when (eq (car vt) :idx)
       ;; an own/borrow of a resource type: the FLAT core type is i32
       ;; (the handle-table index) — the type-shape resolution needs
       ;; the component's type space, which %exec-canon pre-resolves
       ;; into the lifted-func's TYPE slot; the prim for a resource
       ;; handle is :own/:borrow only for identity checks, so treat
       ;; any resolved reference as i32-shaped here.
       (let ((n (cdr vt)))
         (declare (ignore n))
         :i32-handle)))
    (t nil)))

(defun %valtype-flat-core (vt &optional (context :params))
  "The flat CORE type of one component valtype in CONTEXT. The
numeric prims map to i32/i64/f32/f64; OWN/BORROW handles (an
(:idx . n) reference) ride i32 — the handle-table index. A STRING
RESULT flattens to ONE i32 (the pointer to the [ptr,len] tuple —
MAX_FLAT_RESULTS=1 fallthrough); a string PARAM is [ptr,len] (TWO
core values) and stays a values-suite shape in this slice."
  (typecase vt
    (keyword
     (case vt
       ((:bool :u8 :s8 :u16 :s16 :u32 :s32 :char) :i32)
       ((:u64 :s64) :i64)
       (:f32 :f32)
       (:f64 :f64)
       (:string (when (eq context :results) :i32))
       (t nil)))
    ;; an (:idx . n) reference: own/borrow handles flatten to i32
    (cons (when (eq (car vt) :idx) :i32))
    (t nil)))

(declaim (notinline %valtype-flat-core))

;;; The PRIMITIVE keyword of a valtype when it is one; NIL otherwise
;;; (an (:idx . n) cons or a ct — those resolve through the type
;;; space, which the flat slice defers).
(defun %valtype-prim (vt)
  (and (keywordp vt) vt))

(declaim (notinline %valtype-prim))

(defun lifted-func-call (lf &rest args)
  "Call a lifted func with FLAT component values: lower the args
(identity for the numeric prims — bool/ints/char ride i32/i64,
floats ride f32/f64), call the wrapped core func, lift the single
result back. M9: OWN/BORROW params/results ride i32 handle indexes
(the values the core side produced); a STRING result lifts through
the canon (memory ...) option's memory — [ptr,len] at the returned
pointer. Non-flat shapes signal host-error (values slice).
M9 (linking ::126): an OWN/BORROW PARAM index arrives in the CALLER
instance's table space (the lower added it there) — lift_own
consumes it from the CALLER's table (the dynamic *handle-caller-inst*,
falling back to the defining instance) and re-enters the DEFINING
instance's table so the core side's drop/removal finds it."
  (let* ((ft (lifted-func-type lf))
         (core (lifted-func-core lf)))
    (unless (and core (wasmdown.runtime:func-instance-p core))
      (error 'wasmdown:host-error
             :message "lifted func wraps no core func (values slice)"))
    (let ((flat-params
            (and ft (eq (wasmdown.component:ct-kind ft) :func)
                 (coerce (first (wasmdown.component:ct-payload ft))
                         'list)))
          (flat-result
            (and ft (eq (wasmdown.component:ct-kind ft) :func)
                 (second (wasmdown.component:ct-payload ft)))))
      (let ((core-params
              (mapcar (lambda (p) (%valtype-flat-core (cdr p) :params))
                      (or flat-params '())))
            (core-result
              (and flat-result
                   (%valtype-flat-core flat-result :results))))
        (when (or (member nil core-params)
                  (and flat-result (null core-result)))
          (error 'wasmdown:host-error
                 :message "canonical ABI for non-flat values is a future slice"))
        (unless (= (length args) (length core-params))
          (error 'wasmdown:host-error
                 :message (format nil
                                  "lifted func expects ~d argument~:p, got ~d"
                                  (length core-params) (length args))))
        ;; M9 (linking ::126): an own/borrow PARAM's i32 is a handle
        ;; index in the CALLER instance's table — consume it there and
        ;; re-enter THIS (defining) instance's table
        (let ((self (lifted-func-inst lf))
              (caller (or *handle-caller-inst* (lifted-func-inst lf))))
          (setf args
                (mapcar
                 (lambda (p a)
                   (let ((ty (and (consp (cdr p))
                                  (eq (car (cdr p)) :idx)
                                  self
                                  (integerp (cdr (cdr p)))
                                  (< (cdr (cdr p))
                                     (length (comp-instance-types self)))
                                  (aref (comp-instance-types self)
                                        (cdr (cdr p))))))
                    (if (and ty
                             (eq (wasmdown.component:ct-kind ty) :own)
                             (integerp a)
                              (integerp
                               (wasmdown.component:ct-payload ty))
                              (< (wasmdown.component:ct-payload ty)
                                 (length (comp-instance-types self))))
                         (let* ((rt (aref (comp-instance-types self)
                                          (wasmdown.component:ct-payload
                                           ty)))
                                (h (and (wasmdown.component:ct-resourcetype-p
                                         rt)
                                        (%handle-remove caller rt a))))
                           (if h
                               (%handle-add self rt (cdar h) t)
                               a))
                         ;; M9 (linking ::110): a BORROW param's i32
                         ;; is a handle index in the CALLER's table —
                         ;; deref to the REP for the core side (no
                         ;; table transfer: a borrow holds no
                         ;; ownership); unknown handles pass through
                         (if (and ty
                                  (eq (wasmdown.component:ct-kind ty)
                                      :borrow)
                                  (integerp a)
                                  (integerp
                                   (wasmdown.component:ct-payload ty))
                                  (< (wasmdown.component:ct-payload ty)
                                     (length (comp-instance-types self))))
                             (let* ((rt (aref (comp-instance-types self)
                                              (wasmdown.component:ct-payload
                                               ty))))
                               (if (wasmdown.component:ct-resourcetype-p rt)
                                   (let ((rep (%handle-rep caller rt a)))
                                     (or rep a))
                                   a))
                             a))))
                 (or flat-params '())
                 args)))
        (let ((res (apply #'wasmdown.runtime:wasm-call core args)))
          (if core-result
              (cond
               ((and (eq core-result :i32)
                     (consp flat-result)
                     (eq (car flat-result) :idx))
                (let* ((tidx (cdr flat-result))
                       (inst (lifted-func-inst lf))
                       (res-ct (and inst
                                    (< tidx
                                       (length (comp-instance-types inst)))
                                    (aref (comp-instance-types inst)
                                          tidx))))
                  (let* ((rt (cond
                              ((and res-ct
                                    (eq (wasmdown.component:ct-kind res-ct)
                                        :resource))
                               res-ct)
                              ((and res-ct
                                    (member (wasmdown.component:ct-kind
                                             res-ct)
                                            '(:own :borrow))
                                    (integerp
                                     (wasmdown.component:ct-payload res-ct))
                                    inst
                                    (< (wasmdown.component:ct-payload res-ct)
                                       (length (comp-instance-types inst))))
                               (aref (comp-instance-types inst)
                                     (wasmdown.component:ct-payload
                                      res-ct)))
                              (t nil)))
                         (h (and rt (%handle-remove inst rt (first res)))))
                    (unless h
                      (error 'wasmdown:host-error
                             :message
                             "own result references an unknown type"))
                    (list :own-handle rt (cdar h)))))
               ((and (eq core-result :i32)
                     (eq flat-result :string))
                (lift-flat-string (lifted-func-memory lf) (first res)))
               (t (first res)))
              (values)))))))
(defun lift-flat-string (memory ptr)
  "lift_flat_string (CanonicalABI.md 3088): read the [ptr,len] tuple
at PTR in the lift MEMORY and return the Lisp string. Traps on a
bad range (the spec's trap taxonomy)."
  (unless memory
    (error 'wasmdown:host-error
           :message "string lift requires the canon (memory ...) option"))
  (let ((bytes (wasmdown.runtime:memory-instance-bytes memory)))
    (unless (and (>= ptr 0) (<= (+ ptr 8) (length bytes)))
      (error 'wasmdown:wasm-trap
             :kind :out-of-bounds-memory
             :message (format nil "string pointer out of bounds: ~d" ptr)))
    (let ((sp (logior (aref bytes ptr)
                      (ash (aref bytes (+ ptr 1)) 8)
                      (ash (aref bytes (+ ptr 2)) 16)
                      (ash (aref bytes (+ ptr 3)) 24)))
          (sl (logior (aref bytes (+ ptr 4))
                      (ash (aref bytes (+ ptr 5)) 8)
                      (ash (aref bytes (+ ptr 6)) 16)
                      (ash (aref bytes (+ ptr 7)) 24))))
      (unless (and (>= sp 0) (<= (+ sp sl) (length bytes)))
        (error 'wasmdown:wasm-trap
               :kind :out-of-bounds-memory
               :message (format nil "string range out of bounds: ~d" sp)))
      (let ((v (make-array sl :element-type 'character)))
        (loop for i below sl
              do (setf (aref v i) (code-char (aref bytes (+ sp i)))))
        v))))


;;; ---------------------------------------------------------------
;;; M9 (linking lane): canon LOWER + the resource built-ins.
;;; canon lower (CanonicalABI.md 3618): the lowered core func wraps
;;; the target COMPONENT func — the core side calls it with the flat
;;; core args, the wrapper lifts them and calls through.
;;; canon resource.new/drop/rep (CanonicalABI.md 3743-3810): the
;;; per-instance HANDLE TABLE — new adds an owning handle and returns
;;; its index; drop removes it (running the dtor when it owns); rep
;;; extracts the representation.

(defun %inst-handles (inst)
  "The comp-instance's handle table (lazily built): a vector of
ResourceHandle records — (rt . rep) pairs plus the own flag —
indexed from 1 (the spec's Table.add semantics: 0 stays invalid so a
fresh per-instance allocation starts at 1, mirroring the fixtures'
per-instance handle-index allocation)."
  (or (gethash :handles (comp-instance-imports inst))
      (let ((v (make-array 8 :fill-pointer 0 :adjustable t)))
        (setf (gethash :handles (comp-instance-imports inst)) v)
        v)))

(defun %handle-add (inst rt rep own-p)
  "Table.add (CanonicalABI.md 3762): append an owning handle; the
index handed to wasm is the 1-based slot."
  (let ((h (cons (cons rt rep) own-p))
        (tab (gethash :handles (comp-instance-imports inst))))
    (declare (ignore tab))
    (let ((v (%inst-handles inst)))
      (vector-push-extend h v)
      (length v))))

(defun %handle-remove (inst rt i)
  "Table.remove (lift_own / canon_resource_drop): the handle at slot
I of THIS instance, type-checked against RT; an empty slot traps."
  (let ((v (%inst-handles inst)))
    (unless (and (integerp i) (> i 0) (<= i (length v))
                 (aref v (1- i)))
      (error 'wasmdown:wasm-trap
             :kind :generic
             :message (format nil "invalid handle index ~a" i)))
    (let ((h (aref v (1- i))))
      (unless (eql (caar h) rt)
        (error 'wasmdown:wasm-trap
               :kind :generic
               :message (format nil "handle ~a has the wrong resource type" i)))
      (setf (aref v (1- i)) nil)
      h)))

(defun %handle-rep (inst rt i)
  "M9 (linking ::110): Table.get for a BORROW lift — the handle's REP
at slot I of the CALLER's table, type-checked against RT, WITHOUT
removing it (a borrow holds no ownership; the handle stays). NIL when
the slot is empty or out of range (the caller passes the raw value
through)."
  (let ((v (%inst-handles inst)))
    (when (and (integerp i) (> i 0) (<= i (length v)))
      (let ((h (aref v (1- i))))
        (when (and h (eql (caar h) rt))
          (cdar h))))))

(defun %resource-rt (inst typeidx)
  "The runtime resource identity for the TYPEIDX'th component type —
the validator's generative per-instantiation identity (the fresh id
the type space carries). The comp-instance's TYPES vector holds the
ct; the identity rides the validator's fresh counter, so the
RUNTIME identity is the (comp-instance . typeidx) pair — distinct per
instantiation, shared through aliases of the same source."
  (declare (ignore inst))
  (cons :rt typeidx))

(defun %call-component-func (inst fidx args)
  "Call the FIDX'th component func of INST with FLAT core args: a
lifted func through lifted-func-call, a raw core func directly (the
flat domain). Returns the single result or NIL."
  (let ((f (and (>= fidx 0) (< fidx (length (comp-instance-funcs inst)))
                (aref (comp-instance-funcs inst) fidx))))
    (typecase f
      (lifted-func
       (apply #'lifted-func-call f args))
      (wasmdown.runtime:func-instance
       (let ((res (apply #'wasmdown.runtime:wasm-call f args)))
         (first res)))
      (t (error 'wasmdown:link-error
                :message "lowered func target has no runtime value")))))

(defun %lower-host-func (inst fidx)
  "canon lower's CORE wrapper (CanonicalABI.md 3618, sync case): a
host func-instance whose type is the FLATTENED core functype of the
target component func — [flat params] -> [flat results] — calling
through %call-component-func. M9 lower_own (CanonicalABI.md 2829):
an ABSTRACT own-handle result (the lift's :own-handle record) is
ADDED to THIS instance's handle table and the new index handed to
the core side. M9 (linking ::126): the CALLER instance is bound
dynamically so the target lifted func's own-PARAM lift can consume
the handle from the CALLER's table and re-enter its own."
  (let* ((f (aref (comp-instance-funcs inst) fidx))
         (ft (and (lifted-func-p f) (lifted-func-type f)))
         (params (and ft (eq (wasmdown.component:ct-kind ft) :func)
                      (coerce (first (wasmdown.component:ct-payload ft))
                              'list)))
         (result (and ft (eq (wasmdown.component:ct-kind ft) :func)
                      (second (wasmdown.component:ct-payload ft))))
         (core-params (mapcar (lambda (p) (%valtype-flat-core (cdr p)))
                              (or params '())))
         (core-result (and result (%valtype-flat-core result :results))))
    (wasmdown.runtime:make-host-func-instance
     (wasmdown.types:make-wasm-func-type core-params
                                         (if core-result
                                             (list core-result) '()))
     (lambda (args)
       (let* ((*handle-caller-inst* inst)
              (res (%call-component-func inst fidx args)))
         (if core-result
             (list
              (if (and (consp res) (eq (first res) :own-handle))
                  ;; lower_own: the abstract handle enters THIS
                  ;; instance's table; the core side gets the INDEX
                  (%handle-add inst (second res) (third res) t)
                  res))
             '()))))))

(defun %resource-builtin-host-func (inst kind rt &optional dtor)
  "The canon resource built-in's core wrapper: [i32] -> [i32] for
resource.new (param rep, result handle), [i32] -> [] for
resource.drop (param handle), [i32] -> [i32] for resource.rep."
  (ecase kind
    (:resource-new
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '(:i32))
      (lambda (args)
        (list (%handle-add inst rt (first args) t)))))
    (:resource-drop
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '())
      (lambda (args)
        (let* ((h (%handle-remove inst rt (first args)))
               (rep (cdar h)))
          ;; an OWNING handle's drop runs the resource's dtor with
          ;; the rep (canon_resource_drop, CanonicalABI.md 3782)
          (when (and (cdr h) dtor)
            (apply #'wasmdown.runtime:wasm-call dtor (list rep)))
          '()))))
    (:resource-rep
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '(:i32))
      (lambda (args)
        (let ((v (%inst-handles inst)))
          (list (cdar (aref v (1- (first args)))))))))))

;;; Install the entry point into the runner's hook (src/wast.lisp
;;; loads first and calls through *COMPONENT-INSTANTIATOR*).
(setf wasmdown.wast:*component-instantiator* #'instantiate-component)

;;; M9: the runner's component-instance ACTION hook (the contract
;;; documented at wasmdown.wast:*component-invoker*).
(defun %component-action (sel &rest rest)
  "The runner's component-instance ACTION hook. SEL selects the
operation: :INSTANCE-P (a type test), :GET (a global read), or the
INVOKE shape — SEL is the comp-instance and REST the (name args)."
  (cond
   ((eq sel :instance-p)
    (comp-instance-p (first rest)))
   ((eq sel :get)
    (let* ((inst (first rest))
           (v (runtime-export-value inst (second rest))))
      (unless (wasmdown.runtime:global-instance-p v)
        (error 'wasmdown:host-error :message "get: export is not a global"))
      (wasmdown.runtime:global-value v)))
   (t
    ;; INVOKE: SEL is the comp-instance; resolve the export; a lifted
    ;; func calls through the FLAT canon path, a raw core func calls
    ;; directly
    (let* ((inst sel)
           (name (first rest))
           (args (second rest))
           (hit (gethash name (comp-instance-exports inst) :missing)))
      (when (eq hit :missing)
        (error 'wasmdown:link-error
               :message (format nil "unknown export: ~a" name)))
      (let ((value (cdr hit)))
        (typecase value
          (lifted-func
           (let* ((ft (lifted-func-type value))
                  (res-vt (and ft
                               (eq (wasmdown.component:ct-kind ft)
                                   :func)
                               (second (wasmdown.component:ct-payload
                                        ft)))))
             (if res-vt
                 (list (apply #'lifted-func-call value args))
                 (progn (apply #'lifted-func-call value args) '()))))
          (wasmdown.runtime:func-instance
           (apply #'wasmdown.runtime:wasm-call value args))
          (t (error 'wasmdown:host-error
                    :message
                    (format nil "export ~a is not a function" name)))))))))

(setf wasmdown.wast:*component-invoker* #'%component-action)
