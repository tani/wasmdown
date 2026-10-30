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
           #:lifted-func-host
           #:lifted-func-call
           #:make-comp-instance
           #:bag-p
           #:bag-ref
           #:runtime-export-value
           ;; M16 slice 2 — the HOST side of stream<u8>/future<T>
           #:async-end-p
           #:async-end-other
           #:make-host-stream-pair
           #:make-host-future-pair
           #:host-end-set-sink
           #:host-end-set-source
           #:host-end-set-on-drop
           #:host-end-set-error
           #:host-end-drop
           #:host-end-drain
           #:host-end-read
           #:host-future-resolve
           #:make-host-buffer
           #:host-buffer-p
           #:host-buffer-values
           #:host-buffer-progress
           ;; M16 slice 3 — the HOST RESOURCE path (wasmdown.wasi's
           ;; descriptor: a provider-owned resource whose rep is a CL
           ;; record and whose drop runs a host finalizer)
           #:host-resource-rep
           #:host-resource-take
           #:*host-resource-dtor-table*
           ;; M16 slice 6 — the RUNNER's store-boundary teardown: a host
           ;; embedder that drives one command to completion (the WASI
           ;; `run' entry) tears the whole store down afterwards, so a
           ;; `wasi:cli/exit' cannot leak guest threads.
           #:store-teardown))
(in-package #:wasmdown.component.runtime)

;;; THE ASYNC DYNAMIC CONTEXT: proclaimed SPECIAL at the file's
;;; head so EVERY binding site compiles as a dynamic binding
;;; (a binding site that precedes the defparameters — an
;;; unproclaimed LET binding compiles LEXICAL and never reaches
;;; the guest arms; this was the root cause of the sessions
;;; 21-23 "binding-visibility mystery").
(declaim (special *current-task* *current-thread*
                 *handle-caller-inst* *sync-lift-owner*))

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
  (inst nil)                         ; the defining comp-instance (M9)
  (host nil))                        ; M16: a host closure
                                     ; (domain-args-list -> domain value)
                                     ; called INSTEAD of the flat core
                                     ; path (the WASI providers)

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

(defvar *resource-dtor-table* (make-hash-table :test #'equal)
  "M9: (resource-ct . defining-comp-instance) -> the dtor
func-instance, resolved at the TYPE def walk in the DEFINING
instance's core-func space. A consumer's resource.drop canon reads
the ORIGIN's dtor — the ct's payload dtor index is meaningful only
there.")

(defvar *host-resource-dtor-table* (make-hash-table :test #'eq)
  "M16 slice 3: resource-ct -> (lambda (rep) ...) for a HOST-provided
resource (wasi:filesystem's descriptor). Such a resource has no guest
dtor func-instance; canon resource.drop calls this finalizer with the
rep so the host capability (the fd) is released deterministically.")

(defun %fresh-resource-id ()
  (cons :fresh (incf *resource-id-counter*)))

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

(defvar *host-provider-inst* nil
  "M16 slice 3b: the provider's OWN comp-instance, bound while its
host closure runs (lifted-func-call's host branch). The canon-lower
wrappers LEND a borrow param into the callee-defining table — for a
host provider that is its own instance — so a provider resolving
the raw index (host-resource-rep) falls back to this table when the
dynamic caller's table does not hold it.")


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

(defstruct closed-comp
  "M9 (linking ::233): an alias-projected COMPONENT value captured at
the export site — the comp plus the SOURCE comp-instance (the closure
for the child's depth-1 outer aliases: an exported component closes
over the instance that exported it, so instantiating it later resolves
its outer refs through THAT instance, not the instantiating site's)."
  (comp nil)
  (instance nil))

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
              (canonrec-func payload)
              (< (canonrec-func payload)
                 (length (comp-instance-funcs inst)))
              (aref (comp-instance-funcs inst)
                    (canonrec-func payload)))
         (%push-core-func
          inst
          ;; M11a: an ASYNC lower (the `async` canonopt) routes
          ;; through the Subtask wrapper (the packed state return,
          ;; the eager RETURNED, the [ptr] out-param); the sync
          ;; lowers keep the M9 flat path. The (values lane)
          ;; CANONREC passes the LOWER's OWN canon options (the
          ;; caller's memory/realloc) to both.
          (if (wasmdown.component:canon-opt (canonrec-opts payload)
                                            :async)
              (%canon-lower-async-host-func
               inst (canonrec-func payload) payload)
              (%lower-host-func inst (canonrec-func payload)
                                payload))))
        ((and (member rkind
                      '(:resource-new :resource-drop :resource-rep))
              rt)
         (%push-core-func
          inst
          (%resource-builtin-host-func inst rkind rt dtor)))
        ((eq kind :builtin)
         ;; M11a: the ASYNC builtins become REAL host funcs
         ;; (task.return/cancel, context.get/set, backpressure,
         ;; waitable-set.*, waitable.join, subtask.*); the M11b
         ;; kinds (thread.*/future.*/stream.*/error-context.*)
         ;; return NIL — an opaque slot that traps when called,
         ;; never at instantiate time.
         ;; M16 slice 6: the BINARY reader stores a builtin's
         ;; canonopts on the CANONREC (`canonrec-opts'), while the
         ;; TEXT parser embeds them as the immediates' THIRD element
         ;; (the shape `%task-return-opts' and the stream/future copy
         ;; builtins read). Normalize the binary shape here — without
         ;; this, a binary `stream.write (memory ...)` resolved NO
         ;; memory and the copy trapped on a NIL memory-instance.
         (let* ((imms (canonrec-imms payload))
                (opts (canonrec-opts payload))
                (imms (if (and opts (>= (length imms) 2)
                               (null (third imms)))
                          (append (list (first imms) (second imms) opts))
                          imms)))
           (%push-core-func
            inst
            (%builtin-async-host-func inst rkind imms))))
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
;;; ---------------------------------------------------------------
;;; The canonical ABI call path (CanonicalABI.md, the values lane):
;;; lower_flat_values / lift_flat_values with the MAX_FLAT fallthrough
;;; and the full store/load machinery per type — strings (realloc +
;;; the utf8/utf16/latin1+utf16 transcodes), lists (realloc the
;;; elements then the pair), records (the aligned field sequence),
;;; variants (the discriminant + payload + the join coercions), and
;;; the flags/enum/option/result specializations.

(declaim (notinline lifted-func-call %abi-lower-values %abi-lift-values
                    %abi-lower %abi-lift %abi-store %abi-load
                    %abi-store-string %abi-load-string
                    %abi-allocate %abi-reallocate
                    %abi-align %abi-elem-size %abi-alignment
                    %abi-utf8-encode %abi-utf8-decode
                    %abi-utf16-encode %abi-utf16-decode
                    %abi-latin1-encode
                    %abi-variant-cases %abi-variant-payload-size))

;;; ---- alignment / elem_size (mirror the validator's twins; the two
;;; ---- MUST stay consistent — CanonicalABI.md #alignment,
;;; ---- #element-size; ptr_size = 4 for the i32 memories this lane)

(defun %abi-align-to (ptr alignment)
  "align_to (CanonicalABI.md 2112): round PTR up to ALIGNMENT."
  (* (ceiling ptr alignment) alignment))

(defun %abi-alignment (vt inst)
  "alignment(t, i32) (CanonicalABI.md 1991-2063)."
  (labels
      ((al (vt)
         (let ((c (%abi-ct vt inst)))
           (case (wasmdown.component:ct-kind c)
             (:prim
              (case (wasmdown.component:ct-payload c)
                ((:bool :s8 :u8) 1)
                ((:s16 :u16) 2)
                ((:s32 :u32 :f32 :char :error-context) 4)
                ((:s64 :u64 :f64) 8)
                (:string 4)
                (t 4)))
             (:list
              (if (cdr (wasmdown.component:ct-payload c))
                  (al (car (wasmdown.component:ct-payload c)))
                  4))
             (:record
              (or (loop for (nil . fvt) across
                           (wasmdown.component:ct-payload c)
                         maximize (%abi-alignment fvt inst))
                  1))
             (:variant
              (max (%abi-align-discriminant-size c)
                   (or (loop for (nil . cvt) across
                                (wasmdown.component:ct-payload c)
                              when cvt maximize
                              (%abi-alignment cvt inst))
                       1)))
             ((:flags :enum) 1)
             (:tuple
              (or (loop for t_ across (wasmdown.component:ct-payload c)
                        maximize (%abi-alignment t_ inst))
                  1))
             (:option (%abi-alignment (wasmdown.component:ct-payload c)
                                      inst))
             (:result
              (let ((ok (car (wasmdown.component:ct-payload c)))
                    (err (cdr (wasmdown.component:ct-payload c))))
                (max 4 (if ok (%abi-alignment ok inst) 1)
                     (if err (%abi-alignment err inst) 1))))
             (t 4)))))
    (al vt)))

(defun %abi-ct (vt inst)
  "Resolve a valtype to its ct: an (:idx . n) ref through the
instance's type space (NIL when unresolvable — the caller's default
shapes treat it as opaque), a prim keyword as a prim ct, a ct as-is.
A MAP ct DESPECIALIZES here (CanonicalABI.md 1938:
despecialize(MapType(k, v)) = ListType(TupleType([k, v]))) so every
alignment/elem-size/store/lower/lift walker sees the plain
list-of-tuples shape."
  (labels
      ((despecialize (c)
         (if (and (wasmdown.component:ct-p c)
                  (eq (wasmdown.component:ct-kind c) :map))
             (despecialize
              (make-ct* :list
                        (cons (make-ct* :tuple
                                        (vector
                                         (car (wasmdown.component:ct-payload
                                               c))
                                         (cdr (wasmdown.component:ct-payload
                                               c))))
                               nil)))
             c)))
    (let ((raw
            (cond
             ((and (consp vt) (eq (car vt) :idx))
              (or (wasmdown.component:ct-at (comp-instance-types inst)
                                            (cdr vt))
                  (make-ct* :prim vt)))
             ((keywordp vt) (make-ct* :prim vt))
             ((wasmdown.component:ct-p vt) vt)
             (t (make-ct* :prim :bool)))))
      (despecialize raw))))

(defun make-ct* (kind payload &optional (offset 0))
  "A fresh ct (the component package's keyword constructor, one arm)."
  (wasmdown.component:make-ct :kind kind :payload payload :offset offset))

(defun %abi-discriminant-size (cases)
  "discriminant_type (CanonicalABI.md 2038): the smallest unsigned
width covering the case count — 1 byte up to 256 cases, 2 to 65536,
else 4."
  (let ((n (length cases)))
    (cond ((<= n 256) 1) ((<= n 65536) 2) (t 4))))

(defun %abi-align-discriminant-size (c)
  "The ALIGNMENT of the discriminant (alignment(discriminant_type))."
  (let ((n (length (wasmdown.component:ct-payload c))))
    (cond ((<= n 256) 1) ((<= n 65536) 2) (t 4))))

(defun %abi-elem-size (vt inst)
  "elem_size(t, i32) (CanonicalABI.md 2080-2130)."
  (let ((c (%abi-ct vt inst)))
    (case (wasmdown.component:ct-kind c)
      (:prim
       (case (wasmdown.component:ct-payload c)
         ((:bool :s8 :u8) 1)
         ((:s16 :u16) 2)
         ((:s32 :u32 :f32 :char :error-context) 4)
         ((:s64 :u64 :f64) 8)
         (:string 8)
         (t 4)))
      (:list
       (let ((elem (car (wasmdown.component:ct-payload c)))
             (fixed (cdr (wasmdown.component:ct-payload c))))
         (if fixed (* fixed (%abi-elem-size elem inst)) 8)))
      (:record
       (let ((sz 0))
         (loop for (nil . fvt) across (wasmdown.component:ct-payload c)
               do (let ((a (%abi-alignment fvt inst)))
                    (setf sz (* (ceiling sz a) a))
                    (incf sz (%abi-elem-size fvt inst))))
         (let ((a (or (loop for (nil . fvt) across
                               (wasmdown.component:ct-payload c)
                             maximize (%abi-alignment fvt inst))
                     1)))
           (* (ceiling sz a) a))))
      (:variant
       (let* ((cases (wasmdown.component:ct-payload c))
              (ds (%abi-discriminant-size cases))
              (ma (max 1 (or (loop for (nil . cvt) across cases
                                   when cvt maximize
                                   (%abi-alignment cvt inst))
                             1)))
              (payload (or (loop for (nil . cvt) across cases
                                 when cvt maximize (%abi-elem-size cvt inst))
                           0)))
         (let ((sz (+ (* (ceiling ds ma) ma) payload)))
           (* (ceiling sz ma) ma))))
      (:flags
       (let ((n (length (wasmdown.component:ct-payload c))))
         (cond ((<= n 8) 1) ((<= n 16) 2) (t 4))))
      (:enum 1)
      (:tuple
       (let ((sz 0))
         (loop for t_ across (wasmdown.component:ct-payload c)
               do (let ((a (%abi-alignment t_ inst)))
                    (setf sz (* (ceiling sz a) a))
                    (incf sz (%abi-elem-size t_ inst))))
         (let ((a (or (loop for t_ across (wasmdown.component:ct-payload c)
                            maximize (%abi-alignment t_ inst))
                     1)))
           (* (ceiling sz a) a))))
      (:option
       ;; despecialized: variant(none, some) — the FULL variant rule:
       ;; align(disc_size, max_case_alignment) + max payload size,
       ;; then align to the variant's alignment (CanonicalABI.md
       ;; elem_size_variant). The old payload-only formula dropped
       ;; the discriminant (16 for concat.wast ::23's option(tuple
       ;; string (list u32)) where the core strides 20).
       (let* ((payload (wasmdown.component:ct-payload c))
              (ds 1)
              (ma (max 1 (or (and payload
                                  (%abi-alignment payload inst))
                             1)))
              (ps (or (and payload (%abi-elem-size payload inst)) 0)))
         (let ((sz (+ (* (ceiling ds ma) ma) ps)))
           (* (ceiling sz ma) ma))))
      (:result
       ;; despecialized: variant(ok, error) — disc 1 + payload
       (let* ((ok (car (wasmdown.component:ct-payload c)))
              (err (cdr (wasmdown.component:ct-payload c)))
              (p (max (if ok (%abi-elem-size ok inst) 0)
                      (if err (%abi-elem-size err inst) 0)))
              (a (max 1 (if ok (%abi-alignment ok inst) 1)
                     (if err (%abi-alignment err inst) 1))))
         (max 1 (* (ceiling p a) a))))
      (t 4))))

;;; ---- codecs (utf8 / utf16 / latin1) ----

(defun %abi-utf8-encode (s)
  "Encode a Lisp string as UTF-8 bytes (SBCL's encoder)."
  (sb-ext:string-to-octets s :external-format :utf-8))

(defun %abi-utf8-decode (bytes &optional (start 0) (end nil))
  "Decode UTF-8 BYTES [START, END) as a Lisp string; signals a
wasm-trap (:invalid-utf8 style) on malformed input — load_string's
trap taxonomy (invalid utf-8 / incomplete utf-8 byte sequence)."
  (let ((end (or end (length bytes))))
    (handler-case
        (sb-ext:octets-to-string bytes :start start :end end
                                 :external-format :utf-8)
      (error (c)
        (declare (ignore c))
        (error 'wasmdown:wasm-trap
               :kind :generic
               :message "invalid utf-8")))))

(defun %abi-utf16-encode (s)
  "Encode a Lisp string as little-endian UTF-16 code units."
  (let* ((units
           (loop for ch across s
                 for cp = (char-code ch)
                 if (< cp #x10000)
                   collect cp
                 else
                   ;; surrogate pair
                   collect (+ #xD800 (ash (- cp #x10000) -10))
                   and collect (+ #xDC00 (logand #x3FF
                                                 (- cp #x10000)))))
         (bytes (make-array (* 2 (length units))
                            :element-type '(unsigned-byte 8))))
    (loop for u in units
          for i from 0 by 2
          do (setf (aref bytes i) (logand #xff u)
                   (aref bytes (+ i 1)) (ash u -8)))
    bytes))

(defun %abi-utf16-decode (bytes &optional (start 0) (end nil))
  "Decode little-endian UTF-16 code units [START, END) as a string."
  (let* ((end (or end (length bytes)))
         (n (ash (- end start) -1))
         (s (make-array n :element-type 'character :fill-pointer 0
                        :adjustable t)))
    (loop for i below n
          for u = (logior (aref bytes (+ start (* 2 i)))
                          (ash (aref bytes (+ start (+ 1 (* 2 i)))) 8))
          do (cond
              ((and (>= u #xD800) (<= u #xDBFF))
               ;; high surrogate — pair with the next unit
               (let ((lo (if (< (1+ i) n)
                              (logior
                               (aref bytes (+ start (* 2 (+ i 1))))
                               (ash (aref bytes
                                          (+ start (+ 1 (* 2 (+ i 1)))))
                                    8))
                              0)))
                 (vector-push-extend
                  (code-char (+ #x10000
                                (ash (- u #xD800) 10)
                                (- lo #xDC00)))
                  s)
                 (incf i)))
              ((and (>= u #xDC00) (<= u #xDFFF))
               ;; lone low surrogate — invalid; treat as replacement
               (vector-push-extend (code-char u) s))
              (t (vector-push-extend (code-char u) s))))
    s))

(defun %abi-latin1-encode (s)
  "Encode a Lisp string as Latin-1 bytes; traps when a code point
exceeds U+00FF (store_string_copy's latin-1 target)."
  (let ((bytes (make-array (length s)
                           :element-type '(unsigned-byte 8))))
    (loop for ch across s
          for i from 0
          do (let ((cp (char-code ch)))
               (when (> cp #xFF)
                 (error 'wasmdown:wasm-trap
                        :kind :generic
                        :message "invalid latin-1 code point"))
               (setf (aref bytes i) cp)))
    bytes))

;;; ---- the lower context (cx: memory + realloc + encoding + inst) ----

(defun %abi-encoding-of (enc-opt)
  "The canonopt's string-encoding as the ABI's keyword — the text
parser interns the symbols in ITS package and the binary reader in
the keyword package, so compare by NAME (component.lisp's %enc-str
lesson)."
  (let ((v (and enc-opt (second enc-opt))))
    (cond
     ((null v) :utf8)
     ((string-equal (string v) "UTF8") :utf8)
     ((string-equal (string v) "UTF16") :utf16)
     ((string-equal (string v) "LATIN1+UTF16") :latin1+utf16)
     (t (error 'wasmdown:host-error
               :message (format nil "unknown string encoding ~a" v))))))

(defstruct abi-cx
  "LiftLowerContext (CanonicalABI.md 1038): the lift/lower ambient
state — the memory, the realloc core func, the string encoding and
the defining comp-instance (the handle transfers).

TYPE-INST is the TYPE-space owner (the CALLEE's defining instance,
whose declarator indices the walkers' (:idx . n) valtypes resolve
against); INST stays the VALUE-space owner (the LOWERING CALLER,
whose handle table the stream/future/own/borrow transfers enter).
The two spaces coincide in every same-instance context, so TYPE-INST
defaults to NIL and %ABI-TYPE-INST falls back to INST."
  (memory nil)
  (realloc nil)
  (encoding :utf8)
  (inst nil)
  (type-inst nil))

(defun %abi-type-inst (cx)
  "The TYPE-space instance: the explicit TYPE-INST when a lower/lift
crosses spaces (canon-lower: callee types, caller handles), else the
value-space INST."
  (or (abi-cx-type-inst cx) (abi-cx-inst cx)))

(defun %abi-reallocate (cx old old-size alignment new-size)
  "LiftLowerContext.reallocate (CanonicalABI.md 1057): call the
callee's realloc core func, then trap-check the returned pointer's
ALIGNMENT and BOUNDS (realloc.wast's exact taxonomy: 'unaligned
pointer' for the misaligned case, 'list content out-of-bounds' /
'realloc return: ...' for the OOB family)."
  (let ((realloc (abi-cx-realloc cx))
        (memory (abi-cx-memory cx)))
    (unless (and realloc (wasmdown.runtime:func-instance-p realloc))
      (error 'wasmdown:host-error
             :message "realloc option required for non-flat values"))
    (let ((ptr (first (wasmdown.runtime:wasm-call
                       realloc old old-size alignment new-size))))
      (unless (zerop (mod ptr alignment))
        (error 'wasmdown:wasm-trap
               :kind :generic
               :message "unaligned pointer"))
      (when (> (+ ptr new-size) (length (wasmdown.runtime:memory-instance-bytes
                                         memory)))
        (error 'wasmdown:wasm-trap
               :kind :generic
               :message "realloc return: beyond end of memory"))
      ptr)))

(defun %abi-allocate (cx alignment byte-length)
  "LiftLowerContext.allocate (CanonicalABI.md 1077)."
  (%abi-reallocate cx 0 0 alignment byte-length))

;;; ---- store (lower into memory) / load (lift out of memory) ----

(defun %abi-memory-bytes (cx)
  (wasmdown.runtime:memory-instance-bytes (abi-cx-memory cx)))

(defun %abi-load-int (cx ptr nbytes &key (signed nil))
  "load_int (CanonicalABI.md 2171)."
  (let ((bytes (%abi-memory-bytes cx))
        (acc 0))
    (loop for i from 0 below nbytes
          do (setf acc (logior acc (ash (aref bytes (+ ptr i))
                                         (* 8 i)))))
    (if (and signed (>= acc (ash 1 (1- (* 8 nbytes)))))
        (- acc (ash 1 (* 8 nbytes)))
        acc)))

(defun %abi-store-int (cx v ptr nbytes)
  "store_int (CanonicalABI.md 2491): the low NBYTES of V, little-
endian."
  (let ((bytes (%abi-memory-bytes cx)))
    (loop for i from 0 below nbytes
          do (setf (aref bytes (+ ptr i)) (logand #xff (ash v (- (* 8 i))))))))

(declaim (notinline %abi-load-int %abi-store-int))

(defun %abi-check-span (cx ptr size kind-name)
  "The load/store bounds assert (CanonicalABI.md load/store top):
trap when [PTR, PTR+SIZE) leaves the memory."
  (let ((len (length (%abi-memory-bytes cx))))
    (unless (and (>= ptr 0) (<= (+ ptr size) len))
      (error 'wasmdown:wasm-trap
             :kind :out-of-bounds-memory
             :message (format nil "~a content out-of-bounds" kind-name)))))

(defun %abi-check-align (cx ptr alignment what)
  "The load/store alignment assert: trap when PTR is not aligned
(alignment.wast's 'unaligned pointer')."
  (unless (zerop (mod ptr alignment))
    (error 'wasmdown:wasm-trap
           :kind :generic
           :message (format nil "unaligned pointer (~a)" what))))

;;; store_string (CanonicalABI.md 2575-2735): lower the string into
;;; the callee's memory via realloc with the declared encoding, and
;;; return (ptr . tagged-code-units). The source is always a Lisp
;;; string here (the host boundary), which the spec models as the
;;; (str, encoding, units) tuple with a fresh per-call hint.
(defun %abi-store-string (cx v)
  "store_string_into_range: the [ptr, tagged-length] pair for the
value V under CX's encoding. utf8 stores the encoded bytes; utf16
the code units; latin1+utf16 picks per content — latin1 when every
code point fits, utf16 (tagged) otherwise."
  (case (abi-cx-encoding cx)
    (:utf8
     (let* ((encoded (%abi-utf8-encode v))
            (n (length encoded))
            (ptr (%abi-allocate cx 1 n)))
       (replace (%abi-memory-bytes cx) encoded :start1 ptr)
       (cons ptr n)))
    (:utf16
     (let* ((encoded (%abi-utf16-encode v))
            (n (ash (length encoded) -1))
            (ptr (%abi-allocate cx 2 (length encoded))))
       (replace (%abi-memory-bytes cx) encoded :start1 ptr)
       (cons ptr n)))
    (:latin1+utf16
     (let ((latin1-p (every (lambda (ch) (<= (char-code ch) #xFF)) v)))
       (if latin1-p
           (let* ((encoded (%abi-latin1-encode v))
                  (ptr (%abi-allocate cx 2 (length encoded))))
             (replace (%abi-memory-bytes cx) encoded :start1 ptr)
             (cons ptr (length encoded)))
           (let* ((encoded (%abi-utf16-encode v))
                  (n (ash (length encoded) -1))
                  (ptr (%abi-allocate cx 2 (length encoded))))
             (replace (%abi-memory-bytes cx) encoded :start1 ptr)
             (cons ptr (logior n #x80000000))))))
    (t (error 'wasmdown:host-error
              :message "unknown string encoding"))))

(defun %abi-load-string (cx ptr tagged)
  "load_string_from_range (CanonicalABI.md 2265): the string at PTR
with the TAGGED code-unit count under CX's encoding. Traps on OOB
(including a zero length at an OOB ptr), on misalignment for the
2-aligned encodings, and on invalid utf-8."
  (let* ((encoding (abi-cx-encoding cx))
         (tag #x80000000)
         (bytes (%abi-memory-bytes cx))
         (len (length bytes)))
    (multiple-value-bind (byte-length decode-start decode-end)
        (case encoding
          (:utf8
           (unless (and (>= ptr 0) (<= (+ ptr tagged) len))
             (error 'wasmdown:wasm-trap
                    :kind :out-of-bounds-memory
                    :message "string pointer/length out of bounds of memory"))
           (unless (zerop (mod ptr 1))
             (error 'wasmdown:wasm-trap
                    :kind :generic :message "unaligned pointer"))
           (values tagged ptr (+ ptr tagged)))
          (:utf16
           (let ((bl (* 2 tagged)))
             (unless (and (>= ptr 0) (<= (+ ptr bl) len))
               (error 'wasmdown:wasm-trap
                      :kind :out-of-bounds-memory
                      :message "string pointer/length out of bounds of memory"))
             (unless (zerop (mod ptr 2))
               (error 'wasmdown:wasm-trap
                      :kind :generic :message "unaligned pointer"))
             (values bl ptr (+ ptr bl))))
          (:latin1+utf16
           (let* ((utf16p (logtag-p tagged tag))
                  (units (logxor (logand tagged (lognot tag))
                                 (if utf16p tag 0)))
                  (units (if utf16p (- tagged tag) tagged))
                  (bl (if utf16p (* 2 units) units)))
             (declare (ignore units))
             (unless (and (>= ptr 0) (<= (+ ptr bl) len))
               (error 'wasmdown:wasm-trap
                      :kind :out-of-bounds-memory
                      :message "string pointer/length out of bounds of memory"))
             (unless (zerop (mod ptr 2))
               (error 'wasmdown:wasm-trap
                      :kind :generic :message "unaligned pointer"))
             (values bl ptr (+ ptr bl))))
          (t (error 'wasmdown:host-error
                    :message "unknown string encoding")))
      (declare (ignore decode-start decode-end))
      ;; decode per encoding from [ptr, ptr+byte-length)
      (case encoding
        (:utf8 (%abi-utf8-decode bytes ptr (+ ptr byte-length)))
        (:utf16 (%abi-utf16-decode bytes ptr (+ ptr byte-length)))
        (:latin1+utf16
         (if (logtag-p tagged tag)
             (%abi-utf16-decode bytes ptr (+ ptr byte-length))
             ;; latin1: each byte is a code point
             (let ((n (logand tagged (lognot tag))))
               (declare (ignore n))
               (let ((s (make-array byte-length :element-type 'character)))
                 (loop for i below byte-length
                       do (setf (char s i)
                                (code-char (aref bytes (+ ptr i)))))
                 s))))
        (t (error 'wasmdown:host-error
                  :message "unknown string encoding"))))))

(defun logtag-p (i tag)
  "utf16_tag (CanonicalABI.md 2258): the high-bit tag test."
  (not (zerop (logand i tag))))

;;; store/load (CanonicalABI.md 2458 / 2140) — the full per-type table.
;;; The VALUE domain: numbers (unsigned host ints), single/double
;;; floats, booleans, strings, lists, alists (records/tuples),
;;; (:variant label . payload), (:enum label), (:flags labels),
;;; (:some . payload), :none, (:ok . payload), (:err . payload).

(defun %abi-store (cx v vt ptr)
  "store (CanonicalABI.md 2458): write V of valtype VT at PTR."
  (%abi-check-align cx ptr (%abi-alignment vt (%abi-type-inst cx)) "store")
  (%abi-check-span cx ptr (%abi-elem-size vt (%abi-type-inst cx)) "value")
  (let ((c (%abi-ct vt (%abi-type-inst cx))))
    (case (wasmdown.component:ct-kind c)
      (:prim
       (case (wasmdown.component:ct-payload c)
         (:bool (%abi-store-int cx (if v 1 0) ptr 1))
         (:u8 (%abi-store-int cx v ptr 1))
         (:u16 (%abi-store-int cx v ptr 2))
         (:u32 (%abi-store-int cx v ptr 4))
         (:u64 (%abi-store-int cx v ptr 8))
         (:s8 (%abi-store-int cx v ptr 1))
         (:s16 (%abi-store-int cx v ptr 2))
         (:s32 (%abi-store-int cx v ptr 4))
         (:s64 (%abi-store-int cx v ptr 8))
         (:f32 (%abi-store-int
                cx (wasmdown.numops:f32-bits v) ptr 4))
         (:f64 (%abi-store-int
                cx (wasmdown.numops:f64-bits v) ptr 8))
         (:char (%abi-store-int cx (char-code (character v)) ptr 4))
         (:string
          (let ((p (%abi-store-string cx v)))
            (%abi-store-int cx (car p) ptr 4)
            (%abi-store-int cx (cdr p) (+ ptr 4) 4)))
         (t (%abi-store-int cx v ptr 4))))
      (:list
       (let ((elem (car (wasmdown.component:ct-payload c)))
             (fixed (cdr (wasmdown.component:ct-payload c))))
         (if fixed
             (let ((es (%abi-elem-size elem (%abi-type-inst cx))))
               (loop for e in v
                     for i from 0
                     do (%abi-store cx e elem (+ ptr (* i es)))))
             (let ((p (%abi-store-list cx v elem)))
               (%abi-store-int cx (car p) ptr 4)
               (%abi-store-int cx (cdr p) (+ ptr 4) 4)))))
      (:record
       (let ((off ptr))
         (loop for (nil . fvt) across (wasmdown.component:ct-payload c)
               for (nil . fv) in v
               do (setf off (%abi-align-to off
                                           (%abi-alignment
                                            fvt (%abi-type-inst cx))))
                  (%abi-store cx fv fvt off)
                  (incf off (%abi-elem-size fvt (%abi-type-inst cx))))))
      (:tuple
       (let ((off ptr))
         (loop for t_ across (wasmdown.component:ct-payload c)
               for tv in v
               do (setf off (%abi-align-to off
                                           (%abi-alignment
                                            t_ (%abi-type-inst cx))))
                  (%abi-store cx tv t_ off)
                  (incf off (%abi-elem-size t_ (%abi-type-inst cx))))))
      (:variant
       (%abi-store-variant cx v c ptr))
      (:flags
       (let ((labels (coerce (wasmdown.component:ct-payload c) 'list))
             ;; the TAGGED shape (cons :flags labels): a bare list
             ;; (a nested record field's pre-converted value) also
             ;; accepts
             (set (if (and (consp v) (eq (car v) :flags))
                      (cdr v)
                      v)))
         (let ((bits 0))
           (loop for l in labels
                 for i from 0
                 when (member l set :test #'string=)
                   do (setf bits (logior bits (ash 1 i))))
           (%abi-store-int cx bits ptr
                           (%abi-elem-size vt (%abi-type-inst cx))))))
      (:enum
       (let ((labels (coerce (wasmdown.component:ct-payload c) 'list)))
         (let ((idx (or (position v labels :test #'string=) 0)))
           (%abi-store-int cx idx ptr 1))))
      (:option
       ;; the DOMAIN shape (:some . payload) (or the bare :none) —
       ;; unwrap the tag into the variant's ("some" . payload) form;
       ;; the old (cons "some" v) DOUBLE-WRAPPED and the store's
       ;; case lookup read the :some SYMBOL where the payload belonged
       (%abi-store-variant
        cx
        (if (eq v :none)
            (cons "none" nil)
            (cons "some" (if (and (consp v) (eq (car v) :some))
                             (cdr v)
                             v)))
        (make-ct* :variant
                  (vector (cons "none" nil)
                          (cons "some" (wasmdown.component:ct-payload c))))
        ptr))
      (:result
       (let ((ok (car (wasmdown.component:ct-payload c)))
             (err (cdr (wasmdown.component:ct-payload c))))
         (%abi-store-variant
          cx
          (cond ((and (consp v) (eq (car v) :ok))
                 (cons "ok" (cdr v)))
                ((and (consp v) (eq (car v) :err))
                 (cons "error" (cdr v)))
                (t (cons "ok" v)))
          (make-ct* :variant
                    (vector (cons "ok" ok) (cons "error" err)))
          ptr)))
      ((:stream :future)
       ;; lower_async_value (§2857): the END (the domain value)
       ;; enters THIS instance's table; store the new index
       (%abi-store-int cx (%lower-async-end v cx) ptr 4))
      ((:own :borrow :resource)
       ;; M16 slice 3: a RESOURCE value inside a compound result
       ;; (the host-provider shape): the abstract (:own-handle rt rep)
       ;; record enters THIS (the caller's) table, the index is
       ;; stored; an integer is already an index.
       (%abi-store-int
        cx
        (if (and (consp v) (eq (car v) :own-handle))
            (%handle-add (abi-cx-inst cx) (second v) (third v) t)
            v)
        ptr 4))
      (t (%abi-store-int cx v ptr 4)))))

(defun %abi-store-list (cx v elem-type)
  "store_list_into_range (CanonicalABI.md 2761): realloc the elements,
store each, return (ptr . length)."
  (let* ((n (length v))
         (es (%abi-elem-size elem-type (%abi-type-inst cx)))
         (ptr (%abi-allocate cx
                              (%abi-alignment elem-type (%abi-type-inst cx))
                              (* n es))))
    (loop for e in v
          for i from 0
          do (%abi-store cx e elem-type (+ ptr (* i es))))
    (cons ptr n)))

(defun %abi-store-variant (cx v c ptr)
  "store_variant (CanonicalABI.md 2787): the discriminant + the
payload at the join-aligned offset."
  (let* ((cases (coerce (wasmdown.component:ct-payload c) 'list))
         (idx (or (position (car v) cases
                            :key #'car :test #'string=)
                  0))
         (case (nth idx cases))
         (payload (cdr case))
         (ds (%abi-discriminant-size cases))
         (ma (max 1 (or (loop for (nil . cvt) in cases
                              when cvt maximize
                              (%abi-alignment cvt (%abi-type-inst cx)))
                        1))))
    (%abi-store-int cx idx ptr ds)
    (when payload
      (let ((p (+ ptr (%abi-align-to ds ma))))
        (%abi-store cx (cdr v) payload p)))))

(defun %abi-load (cx vt ptr)
  "load (CanonicalABI.md 2140): read a value of valtype VT at PTR."
  (%abi-check-align cx ptr (%abi-alignment vt (%abi-type-inst cx)) "load")
  (%abi-check-span cx ptr (%abi-elem-size vt (%abi-type-inst cx)) "value")
  (let ((c (%abi-ct vt (%abi-type-inst cx))))
    (case (wasmdown.component:ct-kind c)
      (:prim
       (case (wasmdown.component:ct-payload c)
         (:bool (not (zerop (%abi-load-int cx ptr 1))))
         (:u8 (%abi-load-int cx ptr 1))
         (:u16 (%abi-load-int cx ptr 2))
         (:u32 (%abi-load-int cx ptr 4))
         (:u64 (%abi-load-int cx ptr 8))
         (:s8 (%abi-load-int cx ptr 1 :signed t))
         (:s16 (%abi-load-int cx ptr 2 :signed t))
         (:s32 (%abi-load-int cx ptr 4 :signed t))
         (:s64 (%abi-load-int cx ptr 8 :signed t))
         (:f32 (wasmdown.numops:f32-from-bits
                (%abi-load-int cx ptr 4)))
         (:f64 (wasmdown.numops:f64-from-bits
                (%abi-load-int cx ptr 8)))
         (:char
          (let ((i (%abi-load-int cx ptr 4)))
            (cond
             ((or (>= i #x110000)
                  (and (>= i #xD800) (<= i #xDFFF)))
              (error 'wasmdown:wasm-trap
                     :kind :generic
                     :message "invalid `char` bit pattern"))
              (t (code-char i)))))
         (:string
          (let ((sp (%abi-load-int cx ptr 4))
                (sl (%abi-load-int cx (+ ptr 4) 4)))
            (%abi-load-string cx sp sl)))
         (t (%abi-load-int cx ptr 4))))
      (:list
       (let ((elem (car (wasmdown.component:ct-payload c)))
             (fixed (cdr (wasmdown.component:ct-payload c))))
         (if fixed
             (loop repeat fixed
                   for i from 0
                   collect (%abi-load cx elem
                                      (+ ptr (* i
                                                (%abi-elem-size
                                                 elem (%abi-type-inst cx))))))
             (let ((lp (%abi-load-int cx ptr 4))
                   (ln (%abi-load-int cx (+ ptr 4) 4)))
               (%abi-load-list cx lp ln elem)))))
      (:record
       (let ((off ptr)
             (fields (coerce (wasmdown.component:ct-payload c) 'list))
             (out '()))
         (loop for (label . fvt) in fields
               do (setf off (%abi-align-to
                             off (%abi-alignment fvt (%abi-type-inst cx))))
                  (push (cons label (%abi-load cx fvt off)) out)
                  (incf off (%abi-elem-size fvt (%abi-type-inst cx))))
         (nreverse out)))
      (:tuple
       (let ((off ptr)
             (out '()))
         (loop for t_ across (wasmdown.component:ct-payload c)
               do (setf off (%abi-align-to
                             off (%abi-alignment t_ (%abi-type-inst cx))))
                  (push (%abi-load cx t_ off) out)
                  (incf off (%abi-elem-size t_ (%abi-type-inst cx))))
         (nreverse out)))
      (:variant
       (let* ((cases (coerce (wasmdown.component:ct-payload c) 'list))
              (ds (%abi-discriminant-size cases))
              (idx (%abi-load-int cx ptr ds)))
         (when (>= idx (length cases))
           (error 'wasmdown:wasm-trap
                  :kind :generic
                  :message "invalid variant discriminant"))
         (let* ((case (nth idx cases))
                (payload (cdr case))
                (ma (max 1 (or (loop for (nil . cvt) in cases
                                     when cvt maximize
                                     (%abi-alignment cvt (%abi-type-inst cx)))
                           1))))
           (if payload
               (cons (car case)
                     (%abi-load cx payload
                                (+ ptr (%abi-align-to ds ma))))
               (cons (car case) nil)))))
      (:flags
       (let ((labels (coerce (wasmdown.component:ct-payload c) 'list)))
         (let ((bits (%abi-load-int cx ptr
                                     (%abi-elem-size vt
                                                     (%abi-type-inst cx)))))
           (loop for l in labels
                 for i from 0
                 when (logbitp i bits)
                   collect l))))
      (:enum
       (let ((labels (coerce (wasmdown.component:ct-payload c) 'list)))
         (let ((idx (%abi-load-int cx ptr 1)))
           (if (< idx (length labels))
               (cons :enum (nth idx labels))
               (error 'wasmdown:wasm-trap
                      :kind :generic
                      :message "invalid variant discriminant")))))
      (:option
       (let* ((payload (wasmdown.component:ct-payload c))
              (r (%abi-load
                  cx
                  (make-ct* :variant
                            (vector (cons "none" nil)
                                    (cons "some" payload)))
                  ptr)))
         (if (string= (car r) "none") :none (cons :some (cdr r)))))
      (:result
       (let* ((ok (car (wasmdown.component:ct-payload c)))
              (err (cdr (wasmdown.component:ct-payload c)))
              (r (%abi-load
                  cx
                  (make-ct* :variant
                            (vector (cons "ok" ok)
                                    (cons "error" err)))
                  ptr)))
         (if (string= (car r) "ok")
             (cons :ok (cdr r))
             (cons :err (cdr r)))))
      (t (%abi-load-int cx ptr 4)))))

(defun %abi-load-list (cx ptr length elem-type)
  "load_list_from_range (CanonicalABI.md 2323): bounds-check the
span then load each element."
  (let ((es (%abi-elem-size elem-type (%abi-type-inst cx))))
    (%abi-check-align cx ptr (%abi-alignment elem-type (%abi-type-inst cx))
                      "list")
    (%abi-check-span cx ptr (* length es) "list")
    (loop for i below length
          collect (%abi-load cx elem-type (+ ptr (* i es))))))

;;; ---- lower_flat / lift_flat (the register domain) ----

(defun %abi-lower (cx v vt &optional (joined-types nil))
  "lower_flat (CanonicalABI.md 3164): one component value as the flat
core value list. JOINED-TYPES is the variant join's flat types (the
CoerceValueIter pairs) — NIL outside variants."
  (declare (ignore joined-types))
  (let ((c (%abi-ct vt (%abi-type-inst cx))))
    (case (wasmdown.component:ct-kind c)
      (:prim
       (case (wasmdown.component:ct-payload c)
         (:bool (list (if v 1 0)))
         ((:u8 :u16 :u32) (list (mod v #x100000000)))
         ((:s8 :s16 :s32)
          (list (mod (mod v #x100000000) #x100000000)))
         ((:u64 :s64) (list (mod v #x10000000000000000)))
         (:f32 (list v))
         (:f64 (list v))
         (:char (list (char-code (character v))))
         (:string
          (let ((p (%abi-store-string cx v)))
            (list (car p) (cdr p))))
         (t (list (mod v #x100000000)))))
      (:list
       (let ((elem (car (wasmdown.component:ct-payload c)))
             (fixed (cdr (wasmdown.component:ct-payload c))))
         (if fixed
             (loop for e in v append (%abi-lower cx e elem))
             (let ((p (%abi-store-list cx v elem)))
               (list (car p) (cdr p))))))
      (:record
       (let ((fields (coerce (wasmdown.component:ct-payload c) 'list)))
         (loop for (nil . fvt) in fields
               for (nil . fv) in v
               append (%abi-lower cx fv fvt))))
      (:tuple
       (loop for t_ across (wasmdown.component:ct-payload c)
             for tv in v
             append (%abi-lower cx tv t_)))
      (:option
       ;; (values lane, concat.wast ::23 'deep') lower_flat of an
       ;; option: the variant(none, some) special case — the flat
       ;; path had no :option arm and the (t ...) default lowered the
       ;; domain cons as a single i32 (disc 0 = 'none' for every
       ;; some). The DOMAIN (:some . payload) unwraps to ("some" .
       ;; payload).
       (%abi-lower
        cx
        (if (eq v :none)
            (cons "none" nil)
            (cons "some"
                  (if (and (consp v) (eq (car v) :some)) (cdr v) v)))
        (make-ct* :variant
                  (vector (cons "none" nil)
                          (cons "some"
                                (wasmdown.component:ct-payload c))))))
      (:variant
       ;; lower_flat_variant (CanonicalABI.md 3235): the discriminant
       ;; + the case payload COERCED to the joined slot types + 0 pad
       (let* ((cases (coerce (wasmdown.component:ct-payload c) 'list))
              (idx (or (position (car v) cases :key #'car :test #'string=)
                       0))
              (case (nth idx cases))
              (payload (cdr case))
              ;; the JOINED flat slot types (flatten_variant minus the
              ;; discriminant) and the payload's own flat types
              (want (cdr (%flat-type-of c (%abi-type-inst cx))))
              (have (and payload
                         (%flat-type-of payload (%abi-type-inst cx))))
              (raw (if payload (%abi-lower cx (cdr v) payload) '()))
              (coerced
                (loop for rv in raw
                      for i from 0
                      collect (%abi-coerce-lower
                               rv (and have (nth i have))
                               (nth i want)))))
         (append (list idx)
                 (append coerced
                         (make-list (max 0 (- (length want)
                                               (length coerced)))
                                    :initial-element 0)))))
      (:flags
       ;; the TAGGED domain (cons :flags labels); a bare label list
       ;; (a nested field's pre-converted value) also accepts
       (let ((labels (coerce (wasmdown.component:ct-payload c) 'list))
             (set (if (and (consp v) (eq (car v) :flags)) (cdr v) v))
             (bits 0))
         (loop for l in labels
               for i from 0
               when (member l set :test #'string=)
                 do (setf bits (logior bits (ash 1 i))))
         (list bits)))
      (:enum
       ;; the TAGGED domain (cons :enum label); a bare label accepts
       (let ((labels (coerce (wasmdown.component:ct-payload c) 'list))
             (e (if (and (consp v) (eq (car v) :enum)) (cdr v) v)))
         (list (or (position e labels :test #'string=) 0))))
      (:option
       (%abi-lower cx
                   (if (eq v :none)
                       (cons "none" nil)
                       (cons "some" (cdr v)))
                   (make-ct* :variant
                             (vector (cons "none" nil)
                                     (cons "some"
                                           (wasmdown.component:ct-payload
                                            c))))))
      (:result
       (let ((ok (car (wasmdown.component:ct-payload c)))
             (err (cdr (wasmdown.component:ct-payload c))))
         (%abi-lower cx
                     (cond ((and (consp v) (eq (car v) :ok))
                            (cons "ok" (cdr v)))
                           ((and (consp v) (eq (car v) :err))
                            (cons "error" (cdr v)))
                           (t (cons "ok" v)))
                     (make-ct* :variant
                               (vector (cons "ok" ok)
                                       (cons "error" err))))))
      ((:stream :future)
       ;; lower_async_value (§2851): the END enters THIS instance's
       ;; table; the flat i32 is the new index
       (list (%lower-async-end v cx)))
      ((:own :borrow :resource)
       ;; M16 slice 3: a RESOURCE value inside a compound result (the
       ;; host-provider shape — `open-at`'s result<descriptor, _>, a
       ;; list<tuple<descriptor, string>>) rides the abstract
       ;; (:own-handle rt rep) record; lowering it ADDS the handle to
       ;; THIS (the caller's) table and yields its index. A plain
       ;; integer is already an index (the borrow lift's domain) and
       ;; passes through.
       (if (and (consp v) (eq (car v) :own-handle))
           (list (%handle-add (abi-cx-inst cx) (second v) (third v) t))
           (list (mod v #x100000000))))
      (t (list (mod v #x100000000))))))


;;; %abi-coerce-lower (lower_flat_variant's match table,
;;; CanonicalABI.md 3247): the payload's RAW flat value coerced to the
;;; JOINED slot's core type. (f32,i32) re-encodes the bits; (i32,i64)
;;; zero-extends; (f32,i64) re-encodes then zero-extends; (f64,i64)
;;; re-encodes; equal pairs pass through.
(defun %abi-coerce-lower (rv have want)
  (cond
   ((or (null want) (null have) (eq have want)) rv)
   ((and (eq have :f32) (eq want :i32))
    (wasmdown.numops:f32-bits rv))
   ((and (eq have :f32) (eq want :i64))
    (wasmdown.numops:f32-bits rv))
   ((and (eq have :f64) (eq want :i64))
    (wasmdown.numops:f64-bits rv))
   ((and (eq have :i32) (eq want :i64)) rv)
   (t rv)))

;;; %abi-coerce-lift (lift_flat_variant's CoerceValueIter,
;;; CanonicalABI.md 3125): the JOINED slot's raw core value coerced
;;; to the case payload's own flat type. (i32,f32) re-decodes;
;;; (i64,i32) wraps; (i64,f32) wraps then re-decodes; (i64,f64)
;;; re-decodes; equal pairs pass through.
(defun %abi-coerce-lift (rv have want)
  (cond
   ((or (null want) (null have) (eq have want)) rv)
   ((and (eq have :i32) (eq want :f32))
    (wasmdown.numops:f32-from-bits rv))
   ((and (eq have :i64) (eq want :i32)) (mod rv #x100000000))
   ((and (eq have :i64) (eq want :f32))
    (wasmdown.numops:f32-from-bits (mod rv #x100000000)))
   ((and (eq have :i64) (eq want :f64))
    (wasmdown.numops:f64-from-bits rv))
   (t rv)))

;;; ---- lift_flat (CanonicalABI.md 3035) ----

(defun %abi-lift (cx vi vt)
  "lift_flat (CanonicalABI.md 3035): one component value from the
flat core value iterator VI (a fill-pointer vector of (value . type)
pairs consumed in order)."
  (labels
      ((next (&optional (want nil))
         (let ((pair (vector-pop vi)))
           (if (and want (not (eq (cdr pair) want)))
               (error 'wasmdown:host-error
                      :message (format nil
                                      "flat value type mismatch: ~
                                       have ~a want ~a"
                                      (cdr pair) want))
               (car pair))))
       (lift (vt)
         (let ((c (%abi-ct vt (%abi-type-inst cx))))
           (case (wasmdown.component:ct-kind c)
             (:prim
              (case (wasmdown.component:ct-payload c)
                (:bool (not (zerop (next :i32))))
                (:u8 (mod (next :i32) #x100))
                (:u16 (mod (next :i32) #x10000))
                (:u32 (next :i32))
                (:u64 (next :i64))
                (:s8 (let ((i (mod (next :i32) #x100)))
                       (if (>= i #x80) (- i #x100) i)))
                (:s16 (let ((i (mod (next :i32) #x10000)))
                       (if (>= i #x8000) (- i #x10000) i)))
                (:s32 (let ((i (next :i32)))
                       (if (>= i #x80000000) (- i #x100000000) i)))
                (:s64 (let ((i (next :i64)))
                       (if (>= i #x8000000000000000)
                           (- i #x10000000000000000)
                           i)))
                (:f32 (next :f32))
                (:f64 (next :f64))
                (:char
                 (let ((i (next :i32)))
                   (cond
                    ((or (>= i #x110000)
                         (and (>= i #xD800) (<= i #xDFFF)))
                     (error 'wasmdown:wasm-trap
                            :kind :generic
                            :message "invalid `char` bit pattern"))
                    (t (code-char i)))))
                (:string
                 (let ((ptr (next :i32))
                       (len (next :i32)))
                   (%abi-load-string cx ptr len)))
                (t (next :i32))))
             (:list
              (let ((elem (car (wasmdown.component:ct-payload c)))
                    (fixed (cdr (wasmdown.component:ct-payload c))))
                (if fixed
                    (loop repeat fixed collect (lift elem))
                    (let ((ptr (next :i32))
                          (len (next :i32)))
                      (%abi-load-list cx ptr len elem)))))
             (:record
              (let ((fields (coerce (wasmdown.component:ct-payload c)
                                    'list))
                    (out '()))
                (loop for (label . fvt) in fields
                      do (push (cons label (lift fvt)) out))
                (nreverse out)))
             (:tuple
              (let ((out '()))
                (loop for t_ across (wasmdown.component:ct-payload c)
                      do (push (lift t_) out))
                (nreverse out)))
             (:variant
              (let* ((cases (coerce (wasmdown.component:ct-payload c)
                                    'list))
                     (idx (next :i32)))
                (when (>= idx (length cases))
                  (error 'wasmdown:wasm-trap
                         :kind :generic
                         :message "invalid variant discriminant"))
                (let* ((case (nth idx cases))
                       (payload (cdr case))
                       ;; the JOINED flat types minus the discriminant
                       (want (cdr (%flat-type-of c (%abi-type-inst cx))))
                       ;; consume EXACTLY the joined slots
                       (slots
                         (loop repeat (length want)
                               collect (vector-pop vi)))
                       (have (and payload
                                  (%flat-type-of payload
                                                 (%abi-type-inst cx)))))
                  (when payload
                    ;; coerce each slot to the payload's own type and
                    ;; re-push for the recursive lift. VI is a LIFO
                    ;; stack: the slots were popped in flat order, so
                    ;; they must be pushed back REVERSED for the
                    ;; payload's lift to read them in flat order (the
                    ;; earlier pop-and-push-onto-the-same-vector was a
                    ;; no-op and left a multi-slot payload — a variant
                    ;; case holding a string — with its ptr/len
                    ;; swapped).
                    (let ((items
                            (loop for (rv . nil) in slots
                                  for i from 0
                                  collect
                                  (cons (%abi-coerce-lift
                                         rv (nth i want) (nth i have))
                                        (nth i have)))))
                      (dolist (item (reverse items))
                        (vector-push-extend item vi))))
                  (let ((v (if payload (lift payload) nil)))
                    ;; drop the unconsumed pad slots (a NO-PAYLOAD case
                    ;; already dropped every joined slot above)
                    (when payload
                      (loop repeat (- (length want) (length have))
                            do (vector-pop vi)))
                    (cons (car case) v)))))
             (:flags
              (let ((labels (coerce (wasmdown.component:ct-payload c)
                                    'list)))
                (let ((bits (next :i32)))
                  ;; the TAGGED shape (cons :flags labels) — the
                  ;; domain convention shared with :enum
                  ;; ((cons :enum label)); the flags.const spec's
                  ;; (list :flags labels) WRAPS one level deeper and
                  ;; %abi-arg-domain unwraps to this shape
                  (cons :flags
                        (loop for l in labels
                              for i from 0
                              when (logbitp i bits)
                                collect l)))))
             (:enum
              (let ((labels (coerce (wasmdown.component:ct-payload c)
                                    'list)))
                (let ((idx (next :i32)))
                  (if (< idx (length labels))
                      (cons :enum (nth idx labels))
                      (error 'wasmdown:wasm-trap
                             :kind :generic
                             :message "invalid variant discriminant")))))
             (:option
              (let* ((payload (wasmdown.component:ct-payload c))
                     (r (lift (make-ct*
                               :variant
                               (vector (cons "none" nil)
                                       (cons "some" payload))))))
                (if (string= (car r) "none") :none
                    (cons :some (cdr r)))))
             (:result
              (let* ((ok (car (wasmdown.component:ct-payload c)))
                     (err (cdr (wasmdown.component:ct-payload c)))
                     (r (lift (make-ct*
                               :variant
                               (vector (cons "ok" ok)
                                       (cons "error" err))))))
                (if (string= (car r) "ok")
                    (cons :ok (cdr r))
                    (cons :err (cdr r)))))
             ((:stream :future)
              ;; lift_async_value (§2438): the i32 indexes THIS
              ;; instance's handles table; the READABLE end moves
              ;; out to the caller (the cx's instance is the owner)
              (%lift-async-end cx (next :i32)))
             (t (next :i32))))))
    (lift vt)))

;;; ---- lift_flat_values / lower_flat_values (CanonicalABI.md 3270 /
;;; ---- 3289) ----

(defun %abi-lower-values (cx max-flat vs ts)
  "lower_flat_values: lower the component values VS of types TS to
the flat core arg list, with the MAX_FLAT fallthrough: >MAX-FLAT
spills the TUPLE through realloc into a single [ptr]."
  (let* ((flat-types (loop for vt in ts
                           append (%flat-type-of vt (%abi-type-inst cx)))))
    (if (> (length flat-types) max-flat)
        ;; the tuple in memory via realloc: (type i, ...) — the
        ;; runtime's tuple is a TUPLE ct of TS
        (let* ((tuple (make-ct* :tuple (coerce ts 'vector)))
               (size (%abi-elem-size tuple (%abi-type-inst cx)))
               (align (%abi-alignment tuple (%abi-type-inst cx)))
               (ptr (%abi-allocate cx align size))
               ;; the record-style store: field i at its aligned slot
               (off ptr))
          (loop for vt in ts
                for v in vs
                do (setf off (%abi-align-to
                              off (%abi-alignment vt (%abi-type-inst cx))))
                   (%abi-store cx v vt off)
                   (incf off (%abi-elem-size vt (%abi-type-inst cx))))
          (list ptr))
        (loop for v in vs
              for vt in ts
              append (%abi-lower cx v vt)))))

(defun %abi-lift-values (cx max-flat vi ts)
  "lift_flat_values: lift the core results VI into the component
values of types TS, with the MAX_FLAT fallthrough: a single [ptr]
result reads the TUPLE back out of memory. VI's (value . type) pairs
are consumed IN ORDER — %abi-lift pops LIFO, so the vector is
reversed up front (an un-reversed pop would swap every adjacent pair
of flat arguments)."
  ;; reverse into a fresh scratch so the caller's vector is intact
  (let* ((n (length vi))
         (scratch (make-array (max 1 n) :fill-pointer 0 :adjustable t)))
    (loop for i from (1- n) downto 0
          do (vector-push-extend (aref vi i) scratch))
    (setf vi scratch))
  (let* ((flat-types (loop for vt in ts
                           append (%flat-type-of vt (%abi-type-inst cx)))))
    (if (> (length flat-types) max-flat)
        ;; the spilled results read the TUPLE back out of memory —
        ;; for the SINGLE-result case (lift_flat_values with one type
        ;; whose flattening exceeds MAX_FLAT_RESULTS) the tuple IS the
        ;; bare value: load it as the result's own type
        (let* ((pair (vector-pop vi))
               (ptr (car pair)))
          (if (= (length ts) 1)
              (list (%abi-load cx (first ts) ptr))
              (let ((tuple (make-ct* :tuple (coerce ts 'vector))))
                (coerce (%abi-load cx tuple ptr) 'list))))
        (loop for vt in ts
              collect (%abi-lift cx vi vt)))))

;;; ---- the lifted call (canon_lift, sync; CanonicalABI.md 3389) ----

(defparameter *sync-lift-owner* nil
  "The sync-lift context's OWNING task (the top-level lift that
created it): a NESTED sync call borrows the outer context and must
NOT exit it on return — only the owner's unwind-protect unregisters
the thread. A binding site that precedes this DEFPARAMETER compiles
LEXICAL (the compile-order class) — proclaimed at the file's head.")

(declaim (special *sync-lift-owner*))

(defun %sync-lift-context (lf)
  "The SYNC-LIFT TASK/THREAD (sessions 21-23 + M12): EVERY sync lift
creates a FRESH task+thread pair and registers it through the gate
(the per-call Task of §572 — a nested sync call must NOT borrow the
caller's context: the callee's thread.* built-ins would address the
CALLER's index — reentrance test 10's promote-the-running-thread
phase check; the sync func holds the exclusive lock only when
async-typed, per §652). The OWNER global records the OUTERMOST
sync-lift task (its unwind-protect is the one that exits; nested
callers PARK instead — %sync-lift-run). Returns the (values task
thread) to bind on the executor."
  (let* ((inst (lifted-func-inst lf))
         (task (make-async-task
                :ft (lifted-func-type lf)
                :opts (lifted-func-opts lf)
                :inst inst
                :on-start (lambda () nil)
                :on-resolve (lambda (r) (declare (ignore r)))))
         (task (progn (%tok-stamp-task task) task))
         (th (make-async-thread :task task)))
    (setf (async-task-implicit-thread task) th)
    (%thread-birth th)
    (setf (async-task-gate-parked task) t) ; inhibit the enter gate's backpressure park
    (unless (%task-enter-implicit-thread task)
      (error 'wasmdown:wasm-trap :kind :generic
             :message "sync lift cancelled at gate"))
    (setf (async-task-gate-parked task) nil)
    (setf (async-task-state task) :started)
    (unless *sync-lift-owner*
      (setf *sync-lift-owner* task))
    (values task th)))

(defun %sync-lift-exit (task th)
  "The sync-lift context's EXIT: unregister the thread — called by
%SYNC-LIFT-RUN for the task IT created (idempotent: a double exit's
remove is a no-op; a trap mid-call still unwinds through here)."
  (when (and task th (async-thread-index th))
    (ignore-errors
      (setf (async-task-gate-parked task) t)
      (%task-exit-implicit-thread task))
    (when (eq *sync-lift-owner* task)
      (setf *sync-lift-owner* nil))))

(declaim (special *right-self* *right-driver* *store-right*
                  *right-ticket* *right-owner*
                  *driver-records* *current-task* *current-thread*))

(defun %sync-thunk-run (entry)
  "M13-tok9: the nested-sync route's executor entry — run ENTRY
(a component-level closure: lifted-func-call + its lift/transfer)
on a fresh executor SBCL thread; the caller parks on the cell's
completion exactly like %sync-lift-run. Returns the closure's
value."
  (let* ((cell (cons nil nil))
         (dmu (sb-thread:make-mutex :name "sync-lift"))
         (done (sb-thread:make-waitqueue :name "sync-lift"))
         (done-p nil)
         (result nil)
         (failure nil)
         ;; STEP 4: this executor is a HOST thread running a nested
         ;; component call — it takes a synthetic ticket under the
         ;; CREATOR's path (a driver record sorts before guest roots,
         ;; a child path sorts after its creator), so the call tree
         ;; order is preserved on both sides of the boundary.
         (self (make-async-thread
                :task (make-async-task
                       :seq (let ((p (%ticket-path *right-self*)))
                              (if p (append p (list 0)) (list 0))))))
         (executor
           (sb-thread:make-thread
            (lambda ()
              (let ((*right-self* self))
                (%right-acquire self)
                (unwind-protect
                     (multiple-value-bind (r e)
                         (ignore-errors (funcall entry))
                       (setf (car cell) r)
                       (when e (setf (cdr cell) e)))
                  (sb-thread:with-recursive-lock (dmu)
                    (setf done-p t)
                    (sb-thread:condition-broadcast done))
                  (%right-release-on-exit self))))
            :name "sync-lift-executor")))
    (declare (ignore done))
    (unwind-protect
         (cond
          ((sb-thread:main-thread-p)
           (loop
             with idle-cycles = 0
             for before = *async-progress*
             do (async-thread-resume-all)
                (sb-thread:with-recursive-lock (dmu)
                  (when done-p (return)))
                ;; STEP 4 PARK SITE 10 (main-thread drain): the poll
                ;; slice releases the right so the executor can run.
                (unless done-p
                  (%right-stall (%driver-self) :seconds 0.001))
                (if (> *async-progress* before)
                    (setf idle-cycles 0)
                    (incf idle-cycles))
             when (>= idle-cycles 200)
               do (error 'wasmdown:wasm-trap :kind :generic
                         :message
                         "deadlock detected: event loop cannot make further progress")))
          (t
           (loop
             (let ((finished (sb-thread:with-recursive-lock (dmu) done-p)))
               (when finished (return))
               ;; STEP 4 PARK SITE 10 (nested caller): release the
               ;; right for the poll slice.
               (%right-stall (or *right-self* (%driver-self))
                            :seconds 0.001)))))
      ;; capture the outcome OUTSIDE the cleanup: the unwind-
      ;; protect's cleanup values are DISCARDED (the earlier
      ;; shape's (car cell) in the cleanup returned NIL always)
      (setf result (car cell) failure (cdr cell))
      ;; STEP 4: the drain loop may have acquired the right on its
      ;; last poll — a driver that returns must not keep it.
      (%right-drop (%driver-self)))
    (when failure (error failure))
    result))

(defun %sync-lift-run (lf core flat-args)
  "The SYNC-LIFT MINI-SCHEDULER (§3538-3543): a sync lift runs its
core call on a FRESH executor SBCL thread — the implicit thread of
the per-call Task (§572: EVERY component-export call creates one) —
so the guest body's thread.* built-ins address THEIR OWN index (a
nested sync call must NOT run inline on the caller's thread: the
caller's index collides with the callee's promote target, the
reentrance phase checks). The CALLER then waits:
  - TOP-LEVEL (the main OS thread): the main thread runs the drain
    loop itself (§3538-3543 — Store.tick every live thread until the
    executor completes), with the DEADLOCK ORACLE (§3539: enough
    quiescent cycles with the executor unfinished = no ready
    candidate can ever make progress).
  - NESTED (the caller is a guest OS thread): the caller parks on
    DONE-P (a condition-wait with a short timeout — the enclosing
    drain loop's resume-all nudges the CALLER too, so it re-tests);
    the executor is registered in the scheduler and gets its slices
    from the same resume-all."
  (multiple-value-bind (task th) (%sync-lift-context lf)
    (let* ((cell (cons nil nil))
           (dmu (sb-thread:make-mutex :name "sync-lift"))
           (done (sb-thread:make-waitqueue :name "sync-lift"))
           (done-p nil)
           (oracled nil)
           (executor
             (sb-thread:make-thread
              (lambda ()
                (let ((*current-task* task)
                      (*current-thread* th)
                      (*right-self* th))
                  ;; STEP 4: the guest body runs only under the right.
                  (%right-acquire th)
                  (unwind-protect
                       (multiple-value-bind (r e)
                           (ignore-errors
                            (apply #'wasmdown.runtime:wasm-call
                                   core flat-args))
                         (setf (car cell) r)
                         (when e (setf (cdr cell) e)))
                    (sb-thread:with-recursive-lock (dmu)
                      (setf done-p t)
                      (sb-thread:condition-broadcast done))
                    (%right-release-on-exit th))))
              :name "sync-lift-executor")))
      (unwind-protect
           (cond
            ((sb-thread:main-thread-p)
             ;; THE DRAIN LOOP (§3538-3543): Store.tick every live
             ;; thread, then poll DONE-P. A condition-wait WITH A
             ;; TIMEOUT on the recursive DMU misfires under load
             ;; ("not holding mutex" — the M11b park-lock class):
             ;; poll + sleep instead.
             (loop
               with idle-cycles = 0
               for before = *async-progress*
               do (async-thread-resume-all)
                  (sb-thread:with-recursive-lock (dmu)
                    (when done-p (return)))
                  ;; STEP 4 PARK SITE 11: the poll slice releases the
                  ;; right so the executor can make progress (the
                  ;; pre-change 1 ms cadence: the during-sync rows ride
                  ;; on the driver's tick now landing between the
                  ;; guest's yields, not on when it lands).
                  (unless done-p
                    (%right-stall (%driver-self) :seconds 0.001))
                  (if (> *async-progress* before)
                      (setf idle-cycles 0)
                      (incf idle-cycles))
               when (>= idle-cycles 200)
                 do (setf oracled t)
                    (error 'wasmdown:wasm-trap :kind :generic
                           :message
                           "deadlock detected: event loop cannot make further progress")))
            (t
             ;; a NON-main caller: a guest thread (its own record) or
             ;; a host executor with no *current-thread* (a synthetic
             ;; driver ticket) — never NIL, or the stall would not
             ;; release anything.
             (let ((ct (or *current-thread* (%driver-self))))
               (%block-enter ct)
               (unwind-protect
                    (loop
                      (let ((finished
                              (sb-thread:with-recursive-lock (dmu)
                                done-p)))
                        (when finished (return))
                        ;; STEP 4 PARK SITE 11 (nested caller): the
                        ;; poll slice releases the right.
                        (%right-stall ct :seconds 0.001)))
                 (%block-exit ct)))))
        ;; THE ORACLE-TRAP TEARDOWN (§3552: "trap and tear down the
        ;; whole store"): a deadlocked executor stays parked in the
        ;; scheduler — the NEXT test's resume-all would nudge a
        ;; ZOMBIE (the observed inter-test interference: test 8's
        ;; deadlock left the executor registered; test 9's flow
        ;; then interleaved with it and trapped "unreachable").
        ;; Unregister it and unblock its park so the SBCL thread
        ;; can finish (the cell stays empty; the caller traps).
        (when oracled
          ;; THE ZOMBIE JOIN (§3552's teardown): mark the executor
          ;; ABANDONED — its park sites unwind (the guest body
          ;; aborts) instead of re-parking — unblock every park,
          ;; and JOIN the SBCL thread so the next test's resume-all
          ;; can never nudge it (the T8->T9 interference: the
          ;; re-parked zombie consumed ticks and starved T9's
          ;; STARTED-wait, ~10/10 deterministic).
          (setf (async-thread-suspended-p th) nil)
          (setf (async-thread-yield-parked th) nil)
          (setf (async-thread-abandoned th) t)
          (%sync-lift-exit task th)
          (sb-thread:with-recursive-lock ((async-thread-mutex th))
            (sb-thread:condition-broadcast
             (async-thread-suspend-waitqueue th))
            (sb-thread:condition-broadcast (async-thread-waitqueue th)))
          (ignore-errors
            (sb-thread:join-thread executor :timeout 0.25
                                          :default :timeout)))
        (unless oracled
          (%sync-lift-exit task th))
        ;; STEP 4: a driver that returns must not keep the right; the
        ;; oracle-teardown JOIN above is only reached when the right
        ;; is already gone (the trap unwound the loop), so it adds no
        ;; new blocking. A nested guest caller keeps its right.
        (%right-drop (%driver-self)))
      (when (cdr cell) (error (cdr cell)))
      (car cell))))

(defun lifted-func-call (lf &rest args)
  "canon_lift (CanonicalABI.md 3389): the ASYNC-TYPED functype lifts
  route through the task state machine (%canon-lift-async — the
  guest body runs on its own SBCL thread and resolves via
  task.return); everything else keeps the SYNC flat path (lower the
  component ARGS per the canonical ABI, call the wrapped core func,
  lift the results, run the optional post-return with the SAME flat
  core results). M9 (linking ::126): an OWN/BORROW param index
  arrives in the CALLER instance's table — lift_own consumes it there
  and re-enters the defining instance's table."
  (let* ((ft (lifted-func-type lf))
         (core (lifted-func-core lf)))
    ;; M16: a HOST-PROVIDER lifted func carries a CL closure that takes
    ;; the component-domain ARGS list and returns the domain value. It
    ;; runs INSTEAD of the flat core path (the provider already speaks
    ;; the canonical VALUE domain; the canon-lower wrapper around it
    ;; lifts/lowers the ABI in the caller's memory).
    (when (lifted-func-host lf)
      ;; M16 slice 3b: the canon-lower wrappers LEND a borrow param
      ;; into the CALLEE-DEFINING instance's table (the provider's
      ;; own comp-instance) and hand the provider that index; the
      ;; provider's host-resource-rep falls back to it when the
      ;; dynamic caller's table does not hold the index.
      (let ((*host-provider-inst* (lifted-func-inst lf)))
        (return-from lifted-func-call
          (funcall (lifted-func-host lf) args))))
    (unless (and core (wasmdown.runtime:func-instance-p core))
      (error 'wasmdown:host-error
             :message "lifted func wraps no core func"))
    ;; M11a: an ASYNC-TYPED functype (the payload's third slot) goes
    ;; through the task state machine — the SYNC flat path below
    ;; would run the guest body on the CALLER's thread where the
    ;; built-ins could not find their task and the YIELD/WAIT
    ;; protocol could not suspend.
    (when (and ft (third (wasmdown.component:ct-payload ft)))
      (return-from lifted-func-call
        (%canon-lift-async lf args)))
    (let* ((params (and ft (eq (wasmdown.component:ct-kind ft) :func)
                        (coerce (first (wasmdown.component:ct-payload ft))
                                'list)))
           (result (and ft (eq (wasmdown.component:ct-kind ft) :func)
                         (second (wasmdown.component:ct-payload ft))))
           (opts (lifted-func-opts lf))
           (enc-opt (wasmdown.component:canon-opt opts
                                                  :string-encoding))
           (cx (make-abi-cx
                :memory (lifted-func-memory lf)
                :realloc (lifted-func-realloc lf)
                :encoding (%abi-encoding-of enc-opt)
                :inst (lifted-func-inst lf))))
      ;; M9 (linking ::126): an OWN/BORROW param's i32 is a handle
      ;; index in the CALLER instance's table — lift_own consumes it
      ;; there and re-enters THIS (defining) instance's table; a
      ;; BORROW derefs the REP for the core side (no transfer)
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
                                        (wasmdown.component:ct-payload ty)))
                              (h (and
                                  (wasmdown.component:ct-resourcetype-p rt)
                                  (%handle-remove caller rt a))))
                         (if h
                             (%handle-add self rt (resource-handle-rep h) t)
                             a))
                       (if (and ty
                                (eq (wasmdown.component:ct-kind ty)
                                     :borrow)
                                (integerp a)
                                (integerp
                                 (wasmdown.component:ct-payload ty))
                                (< (wasmdown.component:ct-payload ty)
                                   (length (comp-instance-types self))))
                           (let* ((rt (aref
                                       (comp-instance-types self)
                                       (wasmdown.component:ct-payload
                                        ty))))
                             (if (wasmdown.component:ct-resourcetype-p
                                 rt)
                                 (let* ((rep (%handle-rep caller rt a))
                                        ;; M13-lad §2834/§3690: the SYNC
                                        ;; lower of a borrow LENDS like
                                        ;; the async branch — a borrow
                                        ;; handle is added to the
                                        ;; CALLEE-DEFINING table (SELF)
                                        ;; and the callee's core gets
                                        ;; ITS index — ONLY for an
                                        ;; ASYNC-TYPED callee (the
                                        ;; cross-task shape: the callee
                                        ;; holds the handle across a
                                        ;; task boundary and drops it
                                        ;; from another task). A SYNC-
                                        ;; typed callee ($C's rep/dtor
                                        ;; funcs — the §2834 same-
                                        ;; implementor optimization)
                                        ;; takes the REP positionally;
                                        ;; lending there broke ::2 (the
                                        ;; identity rep func returned
                                        ;; the index)
                                        (bi (and rep
                                                 (third
                                                  (wasmdown.component:ct-payload
                                                   ft))
                                                 (%handle-add self rt
                                                              rep nil))))
                                   (or bi rep a))
                                 a))
                           a))))
               (or params '())
               args)))
      ;; the runtime LOWER domain: the parsed specs (the runner
      ;; passes the component-value AST) become the domain values
      (let ((vals
              (loop for p in (or params '())
                    for a in args
                    collect (%abi-arg-domain a (cdr p) cx))))
        (let ((flat-args
                (if (or (abi-cx-memory cx) params)
                    (%abi-lower-values
                     cx 16
                     vals (loop for p in (or params '())
                                collect (cdr p)))
                    ;; no params at all
                    '()))
              ;; the core signature per the LIFT context
              (core-sig (and ft
                             (wasmdown.component:flatten-functype
                              ft (comp-instance-types (lifted-func-inst lf))
                              :lift))))
          (declare (ignore core-sig))
          ;; THE SYNC-LIFT TASK/THREAD (sessions 21-23 + M12): every
          ;; sync lift runs its core call through %SYNC-LIFT-RUN —
          ;; the fresh implicit thread (a real per-call Task, §572)
          ;; with the caller draining (top level) or parking
          ;; (nested). The earlier INLINE nested shape had TWO
          ;; measured defects: the index collision (the callee's
          ;; promote targeting the caller's own thread — reentrance
          ;; test 10's phase checks) and the borrowed-context exit
          ;; unregistering the caller mid-turn (the empty-scheduler
          ;; starvation).
          (let ((res (%sync-lift-run lf core flat-args)))
            (let ((vi (make-array 16 :fill-pointer 0 :adjustable t)))
              (loop for r in (coerce res 'list)
                    for rt in (result-core-types lf result)
                    do (vector-push-extend (cons r rt) vi))
              (let ((lifted
                      (if result
                          (%abi-lift-values cx 1 vi (list result))
                          '())))
                ;; M9: an OWN-typed result lifts via lift_own — the
                ;; handle at the raw index is REMOVED from the defining
                ;; instance's table and the REP rides the abstract
                ;; (:own-handle rt rep) record; lower_own (the caller
                ;; side) adds it to ITS table.
                (when (and result (consp result)
                           (eq (car result) :idx))
                  (let* ((tidx (cdr result))
                         (inst (lifted-func-inst lf))
                         (res-ct (and inst
                                      (< tidx (length
                                               (comp-instance-types
                                                inst)))
                                      (aref (comp-instance-types inst)
                                            tidx))))
                    (when (and res-ct
                               (eq (wasmdown.component:ct-kind res-ct)
                                   :own)
                               (integerp
                                (wasmdown.component:ct-payload res-ct))
                               (< (wasmdown.component:ct-payload res-ct)
                                  (length (comp-instance-types inst))))
                      (let* ((rt (aref (comp-instance-types inst)
                                       (wasmdown.component:ct-payload
                                        res-ct)))
                             (h (and
                                 (wasmdown.component:ct-resourcetype-p rt)
                                 (%handle-remove
                                  inst rt (first lifted)))))
                        (unless h
                          (error 'wasmdown:host-error
                                 :message
                                 "own result references an unknown type"))
                        (setf lifted
                              (list (list :own-handle rt
                                          (resource-handle-rep h))))))))
                ;; post-return runs with the SAME flat results
                (let ((pr (wasmdown.component:canon-opt
                           opts :post-return)))
                  (when pr
                    (let ((prf (and (second pr)
                                    (< (second pr)
                                       (length (comp-instance-core-funcs
                                                (lifted-func-inst lf))))
                                    (aref (comp-instance-core-funcs
                                           (lifted-func-inst lf))
                                          (second pr)))))
                      (when prf
                        (apply #'wasmdown.runtime:wasm-call prf res)))))
                (first lifted)))))))))

(defun result-core-types (lf result)
  "The core result types the LIFT produces for RESULT (flatten of
the single result; NIL when none). The LIFTED FUNC's defining
instance resolves (:idx . n) refs."
  (if result
      (coerce (%flat-type-of result (lifted-func-inst lf)) 'list)
      '()))

(defun %abi-arg-domain (a vt cx)
  "Convert a runner ARG (the component-value AST from the parser, or
an already-domain value from another call) to the runtime domain
value for valtype VT."
  (labels
      ((prim-of (c) (wasmdown.component:ct-payload c))
       (cnv (a vt)
         (let ((c (%abi-ct vt (%abi-type-inst cx))))
           (case (first a)
             ((:num :float :bool :char :str)
              (case (prim-of c)
                (:bool (second a))
                (:char (if (stringp (second a))
                           (char (second a) 0)
                           (second a)))
                ((:f32 :f64)
                 (if (eq (first a) :float)
                     (if (eq (second a) :f32)
                         (wasmdown.numops:f32-from-bits (third a))
                         (wasmdown.numops:f64-from-bits (third a)))
                     ;; a non-float spec against a float type: the
                     ;; numeric path below (the FLAT domain delivered
                     ;; ints for f32/f64 in M9) — keep the raw value
                     (cond
                      ((eq (first a) :num) (third a))
                      (t (error 'wasmdown:host-error
                                :message "float type mismatch")))))
                (t (cond
                    ((eq (first a) :num) (third a))
                    ((eq (first a) :str) (second a))
                    (t (error 'wasmdown:host-error
                              :message "bad prim value"))))))
             (:list
              (let ((elem (car (wasmdown.component:ct-payload c))))
                (mapcar (lambda (e) (cnv e elem)) (second a))))
             (:tuple
              (mapcar (lambda (e t_) (cnv e t_))
                      (second a)
                      (coerce (wasmdown.component:ct-payload c) 'list)))
             (:record
              (mapcar (lambda (spec-pair field)
                        (cons (car field) (cnv (cdr spec-pair) (cdr field))))
                      (second a)
                      (coerce (wasmdown.component:ct-payload c) 'list)))
             (:variant
              (let ((hit (find (second a)
                               (coerce (wasmdown.component:ct-payload c)
                                       'list)
                               :key #'car :test #'string=)))
                (cons (second a)
                      (and hit (cdr hit) (third a)
                           (cnv (third a) (cdr hit))))))
             (:enum (second a))
             ;; the TAGGED domain shape (cons :flags labels) — the
             ;; same form %abi-lift's :flags arm produces and
             ;; %abi-lower's flags arm consumes. BOTH input shapes:
             ;; a flags.const spec WRAPS the labels ((:flags labels)
             ;; — (second a) reads them), a LIFTED value SPREADS
             ;; them ((:flags . labels) — the (cdr a) tail); the
             ;; lowered-host-func chain passes lifted values.
             (:flags (cons :flags
                           (if (and (consp (second a))
                                    (not (stringp (second a))))
                               (second a)
                               (cdr a))))
             (:option-some
              (cons :some (cnv (second a)
                               (wasmdown.component:ct-payload c))))
             (:option-none :none)
             (:result-ok
              (cons :ok (and (second a)
                             (cnv (second a)
                                  (car (wasmdown.component:ct-payload c))))))
             (:result-err
              (cons :err (and (second a)
                              (cnv (second a)
                                   (cdr (wasmdown.component:ct-payload
                                         c))))))
             (t a)))))
    (if (and (consp a) (keywordp (first a))
             (member (first a)
                     '(:num :float :bool :char :str :list :tuple
                       :record :variant :enum :flags :option-some
                       :option-none :result-ok :result-err)))
        (cnv a vt)
        ;; already a domain value (a nested call or a start func)
        a)))

;;; ---------------------------------------------------------------
;;; M9 (linking lane): canon LOWER + the resource built-ins.
;;; canon lower (CanonicalABI.md 3618): the lowered core func wraps
;;; the target COMPONENT func — the core side calls it with the flat
;;; core args, the wrapper lifts them and calls through.
;;; canon resource.new/drop/rep (CanonicalABI.md 3743-3810): the
;;; per-instance HANDLE TABLE — new adds an owning handle and returns
;;; its index; drop removes it (running the dtor when it owns); rep
;;; extracts the representation.

(defstruct (resource-handle
             (:constructor make-resource-handle
                 (rt rep own-p &optional (num-lends 0) borrow-scope)))
  "The explicit RESOURCE handle record (CanonicalABI.md 1146's
Table entries for resources): the resource identity RT, the
representation REP, the ownership flag OWN-P, the live borrow count
NUM-LENDS (§1446: a borrow of this handle lends it to a subtask
until its deliver_resolve) and the BORROW-SCOPE the lend belongs to.
Replaces the old (cons (cons rt rep) own-p) shape — every caller
reads through the accessors, never the cons cells."
  (rt nil)
  (rep nil)
  (own-p nil)
  (num-lends 0)
  (borrow-scope nil))

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
index handed to wasm is the 1-based slot. APPEND-ONLY: the fixtures
pin absolute handle indices, so a freed slot is never reused for a
resource (the free list belongs to the shared waitable/subtask
namespace — %HANDLE-ADD-ANY)."
  (let ((v (%inst-handles inst)))
    (vector-push-extend (make-resource-handle rt rep own-p) v)
    (length v)))

(defun %handle-resource (inst rt i)
  "The RESOURCE handle record at slot I when it names RT, else NIL
(the non-trapping LOOKUP: a borrow deref of an index that names no
resource passes the raw value through)."
  (let ((v (%inst-handles inst)))
    (when (and (integerp i) (> i 0) (<= i (length v)))
      (let ((h (aref v (1- i))))
        (when (and (resource-handle-p h)
                   (eql (resource-handle-rt h) rt))
          h)))))

(defun %handle-resource-at (inst i &optional rt)
  "Table.get for a RESOURCE handle, TYPE-CHECKED: the 1-based slot I
must hold a resource-handle (and, when RT is non-NIL, one naming
RT). An empty slot traps 'invalid handle index'; another class (the
single-namespace table's waitable/subtask records) traps 'wrong
resource type' — never a raw accessor crash on a struct."
  (let* ((v (%inst-handles inst))
         (present (and (integerp i) (> i 0) (<= i (length v))
                       (aref v (1- i)))))
    (unless present
      (error 'wasmdown:wasm-trap
             :kind :generic
             :message (format nil "invalid handle index ~a" i)))
    (let ((h present))
      (unless (and (resource-handle-p h)
                   (or (null rt) (eql (resource-handle-rt h) rt)))
        (error 'wasmdown:wasm-trap
               :kind :generic
               :message (format nil "handle ~a has the wrong resource type" i)))
      h)))

(defun %handle-remove (inst rt i)
  "Table.remove (lift_own / canon_resource_drop): the handle at slot
I of THIS instance, type-checked against RT; an empty slot traps.
CLASS GUARD (M13-lad): the handle table is SINGLE-NAMESPACE
(§121-130) — waitable sets, subtasks and stream/future ends share
the slots, and a FREED index is legitimately REUSABLE by another
class. A resource op on a stale index (freed by an earlier drop,
reallocated as a waitable set — drop-cross-task-borrow's shape)
must TRAP, never reach the record accessors: the type check in
%HANDLE-RESOURCE-AT rejects a non-resource record with the clean
'wrong resource type' trap."
  (let* ((v (%inst-handles inst))
         (h (%handle-resource-at inst i rt)))
    (setf (aref v (1- i)) nil)
    (%handle-free-slot inst i)
    h))

(defun %handle-rep (inst rt i)
  "M9 (linking ::110): Table.get for a BORROW lift — the handle's REP
at slot I of the CALLER's table, type-checked against RT, WITHOUT
removing it (a borrow holds no ownership; the handle stays). NIL when
the slot is empty or out of range (the caller passes the raw value
through). CLASS GUARD (M13-lad): only a resource-handle naming RT
yields a rep; a stale index reused by another class reads as
no-resource (NIL — the raw value passes through), never as an
accessor crash."
  (let ((h (%handle-resource inst rt i)))
    (and h (resource-handle-rep h))))

(defun %handle-hit-by-rep (inst rt rep)
  "The 1-based slot of the resource-handle naming RT with REP, else
NIL (the borrow miss-walk: a callee's core received the REP for a
borrowed param and passes it back as an index)."
  (let ((v (%inst-handles inst)))
    (loop for k from 0 below (length v)
          for h = (aref v k)
          when (and (resource-handle-p h)
                    (eql (resource-handle-rt h) rt)
                    (eql (resource-handle-rep h) rep))
            do (return (1+ k)))))

(defun %handle-lend (inst rt i scope)
  "lift_borrow (§2406/§2412): lend the resource handle at slot I of
the SOURCE table INST to SCOPE (the Subtask) — the source handle's
NUM-LENDS rises (the source may not be dropped out from under the
borrower), the lend is recorded on the subtask's ledger (its
deliver_resolve returns the count), and the REP is returned for the
callee's core. NIL when I names no RT resource (the caller's (or bi
rep a) fallback)."
  (let ((h (%handle-resource inst rt i)))
    (when h
      (incf (resource-handle-num-lends h))
      (setf (resource-handle-borrow-scope h) scope)
      (when scope (%subtask-record-lend scope h))
      (resource-handle-rep h))))

(defun host-resource-rep (rt index)
  "M16 slice 3: the REP of the RESOURCE handle at INDEX on the
DYNAMIC CALLER's handle table (*HANDLE-CALLER-INST*, bound by the
canon-lower wrapper), type-checked against RT (the resource ct).
A HOST provider's borrow param arrives as this raw index; the
provider resolves it here. M16 slice 3b: the canon-lower wrappers
LEND the borrow into the CALLEE-defining table, so when the caller's
table does not name RT at INDEX the provider's own table
(*HOST-PROVIDER-INST*, bound by lifted-func-call's host branch) is
consulted. NIL when neither names an RT handle."
  (and (integerp index)
       (or (and *handle-caller-inst*
                (%handle-rep *handle-caller-inst* rt index))
           (and *host-provider-inst*
                (%handle-rep *host-provider-inst* rt index)))))

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

(defun %flat-type-of (vt inst)
  "The FLAT core types of one component valtype (CanonicalABI.md
flatten_type) on the RUNTIME side: prims map directly; an (:idx . n)
ref resolves through THIS instance's type space when it holds a ct,
and otherwise flattens as ONE i32 (the flat shape of every opaque
handle-ish slot: own/borrow/stream/future/func/instance/resource
all ride the single i32 of the table's catch-all rows — the ref may
name a DECLARATOR-space slot whose ct is not resident here, which is
exactly the case the uniform i32 covers). Compound cts recurse; a
tuple is a record; a variable-length list is a [ptr,len] pair; a
variant joins its cases (the i32 discriminant + the joined payload
slots)."
  (labels
      ((fl (vt)
         (cond
          ((and (consp vt) (eq (car vt) :idx))
           (let ((c (wasmdown.component:ct-at
                     (comp-instance-types inst) (cdr vt))))
             (if c (fl-ct c) (list :i32))))
          ((keywordp vt) (fl-prim vt))
          ((wasmdown.component:ct-p vt) (fl-ct vt))
          (t (list :i32))))
       (fl-prim (p)
         ;; FRESH lists every time: MAPCAN NCONCs its results, and a
         ;; shared literal would be destructively appended — the
         ;; second use of the same literal builds a CYCLE (the lane's
         ;; link-time-virtualization hang)
         (case p
           ((:bool :u8 :u16 :u32 :s8 :s16 :s32 :char) (list :i32))
           ((:u64 :s64) (list :i64))
           (:f32 (list :f32))
           (:f64 (list :f64))
           (:string (list :i32 :i32))
           (t (list :i32))))
       (fl-ct (c)
         (case (wasmdown.component:ct-kind c)
           (:prim (fl-prim (wasmdown.component:ct-payload c)))
           (:record
            (mapcan (lambda (f) (fl (cdr f)))
                    (coerce (wasmdown.component:ct-payload c) 'list)))
           (:tuple
            (mapcan #'fl (coerce (wasmdown.component:ct-payload c)
                                 'list)))
           (:list
            (let ((elem (car (wasmdown.component:ct-payload c)))
                  (len (cdr (wasmdown.component:ct-payload c))))
              (if len
                  (loop repeat len append (fl elem))
                  (list :i32 :i32))))
           (:option
            (fl-variant (list (cons "none" nil)
                              (cons "some"
                                    (wasmdown.component:ct-payload
                                     c)))))
           (:result
            (fl-variant
             (list (cons "ok" (car (wasmdown.component:ct-payload c)))
                   (cons "err" (cdr (wasmdown.component:ct-payload
                                     c))))))
           (:variant
            (fl-variant (coerce (wasmdown.component:ct-payload c)
                                'list)))
           (:map (list :i32 :i32))
           ;; flags/enum/own/borrow/stream/future/resource/func/
           ;; instance/component: all one i32
           (t (list :i32))))
       (fl-variant (cases)
         ;; ACC stays in ORDER (a join REPLACES a slot by INDEX; a
         ;; prepend-ordered acc would write the joined slot at the
         ;; wrong position — same lesson as the validator's fix)
         (let ((acc '()))
           (dolist (c cases)
             (when (cdr c)
               (loop for ft in (fl (cdr c))
                     for i from 0
                     do (if (< i (length acc))
                            (setf (nth i acc)
                                  (%join-flat (nth i acc) ft))
                            (setf acc (append acc (list ft)))))))
           (cons :i32 acc))))
    (fl vt)))

(defun %join-flat (a b)
  "CanonicalABI.md join: the tightest approximation of two flat core
types (bitcast-compatible pairs)."
  (cond ((eq a b) a)
        ((or (and (eq a :i32) (eq b :f32))
             (and (eq a :f32) (eq b :i32)))
         :i32)
        (t :i64)))

(defun %flat-sig-of (ft inst)
  "flatten_functype of the target component functype on the RUNTIME
side, with the LOWER context: >16 flat params collapse to one [ptr];
>1 flat result appends the [ptr] out-param to the params and returns
NO results (CanonicalABI.md 2906-2912). Returns (params . results)."
  (let* ((params (and ft (coerce (first (wasmdown.component:ct-payload
                                         ft)) 'list)))
         (result (and ft (second (wasmdown.component:ct-payload ft))))
         (flat-params (mapcan (lambda (p) (%flat-type-of (cdr p) inst))
                             (or params '())))
         (flat-results (and result (%flat-type-of result inst))))
    (when (> (length flat-params) 16) (setf flat-params '(:i32)))
    (when (and flat-results (> (length flat-results) 1))
      (setf flat-params (append flat-params '(:i32)))
      (setf flat-results '()))
    (cons flat-params flat-results)))

(defun %callee-module-imports-blocking-p (lf)
  "M13-tok8: the static blocking discriminator."
  (let ((core (and (lifted-func-p lf) (lifted-func-core lf))))
    (and (wasmdown.runtime:func-instance-p core)
         (let ((module (wasmdown.runtime:module-instance-module
                        (wasmdown.runtime:func-instance-module core))))
           (loop for imp across (wasmdown.types:wasm-module-imports
                                 module)
                 thereis (let ((f (wasmdown.types:wasm-import-field
                                   imp)))
                           (and (stringp f)
                                (or (search "thread." f)
                                    (search "waitable" f)))))))))

(defun %lower-host-func (inst fidx &optional canon)
  "canon lower's CORE wrapper (CanonicalABI.md 3618, the sync case):
a host func-instance whose type is the FLATTENED core functype (the
LOWER context). The call lifts the core args per the component
functype (lift_flat_values, MAX_FLAT_PARAMS — a spilled tuple reads
the fields back out of the CALLER's memory), calls the target
component func, then lowers the result back (lower_flat_values,
MAX_FLAT_RESULTS — a spilled tuple STORES the fields at the caller's
[ptr] out-param). M9 lower_own: an abstract own-handle result enters
THIS instance's table and the core side gets the INDEX. M9 (linking
::126): the CALLER instance is bound dynamically so the target
lifted func's own-PARAM lift consumes the handle from the CALLER's
table and re-enters its own.

(values lane) the functype's (:idx . n) refs belong to the TARGET
LIFTED FUNC's own instance (an alias projecting $C's func into $D
carries $C's functype; $D's type space cannot resolve the refs) —
the flat sig and the arg/result type walks resolve against the
TARGET's space."
  (let* ((f (aref (comp-instance-funcs inst) fidx))
         (ft (and (lifted-func-p f) (lifted-func-type f)))
         (opts (and (lifted-func-p f) (lifted-func-opts f)))
         (params (and ft (eq (wasmdown.component:ct-kind ft) :func)
                      (coerce (first (wasmdown.component:ct-payload ft))
                              'list)))
         (result (and ft (eq (wasmdown.component:ct-kind ft) :func)
                      (second (wasmdown.component:ct-payload ft))))
         (ft-inst (if (lifted-func-p f) (lifted-func-inst f) inst))
         (flat (and ft (eq (wasmdown.component:ct-kind ft) :func)
                    (%flat-sig-of ft ft-inst)))
         (core-params (and flat (coerce (car flat) 'list)))
         (core-results (and flat (coerce (cdr flat) 'list)))
         (core-result (first core-results))
         ;; (values lane, concat.wast's map fixtures): the ABI's
         ;; memory/realloc/encoding are the LOWER's OWN canonopts
         ;; (CANON — resolved against THIS component's spaces: the
         ;; caller's data lives in the LOWER's memory). The TARGET's
         ;; lift opts are a DIFFERENT memory — reading those made the
         ;; arg lift pull the caller's (ptr,len) out of the wrong
         ;; memory (empty tuples) and the result store write where
         ;; the caller's retptr never pointed.
         (lopts (or (and canon (wasmdown.component:canonrec-opts
                                 canon))
                    ;; the pre-values shape (no canonrec): keep the
                    ;; legacy TARGET-opts read
                    opts))
         (enc-opt (and lopts (wasmdown.component:canon-opt
                              lopts :string-encoding)))
         (mem-opt (and lopts (wasmdown.component:canon-opt
                              lopts :memory)))
         (realloc-opt (and lopts (wasmdown.component:canon-opt
                                  lopts :realloc)))
         (memory (and mem-opt
                      (< (second mem-opt)
                         (length (comp-instance-core-memories inst)))
                      (aref (comp-instance-core-memories inst)
                            (second mem-opt))))
         (realloc (and realloc-opt
                       (< (second realloc-opt)
                          (length (comp-instance-core-funcs inst)))
                       (aref (comp-instance-core-funcs inst)
                             (second realloc-opt)))))
    (wasmdown.runtime:make-host-func-instance
     (wasmdown.types:make-wasm-func-type core-params core-results)
     (lambda (args)
       (let* ((*handle-caller-inst* inst)
              ;; (values lane) the ARG LIFT resolves the functype's
              ;; (:idx . n) refs against the TARGET LIFTED FUNC's own
              ;; instance (the declarator-space indices are meaningless
              ;; in THIS component's space; resolving them here
              ;; produced an own-wrapper's integer payload where a
              ;; variant ct was expected — variants.wast's
              ;; take-variant param). THE SINGLE CONTEXT separates the
              ;; two spaces (§step-1): TYPE-INST = FT-INST resolves the
              ;; valtypes, INST (the lowering caller) owns the handle
              ;; table the stream/future/own transfers enter.
              (cx (make-abi-cx
                   :memory memory
                   :realloc realloc
                   :encoding (%abi-encoding-of enc-opt)
                   :inst inst
                   :type-inst ft-inst))
              (ts (loop for p in (or params '()) collect (cdr p)))
              (flat-types (loop for vt in ts
                                append (%flat-type-of vt ft-inst)))
              ;; M11b: stream/future PARAMS consume their handle
              ;; from the CALLER's table (lift_async_value §2438: the
              ;; end LEAVES the caller) while the TYPE resolves
              ;; against the TARGET (the values-lane convention:
              ;; both sides needed — now carried by one cx's two
              ;; instance slots). The POSITIONAL walk mirrors the
              ;; async lower's: each param consumes its flat slots;
              ;; async-value params pre-resolve their ct (target-side)
              ;; and lift the single i32 with the CALLER-side handle
              ;; transfer.
              (component-args
                (if (> (length flat-types) 16)
                    (let* ((ptr (first args))
                           (tuple (make-ct* :tuple
                                           (coerce
                                            (loop for vt in ts
                                                  collect
                                                  (%abi-ct vt
                                                           ft-inst))
                                            'vector)))
                           (vals (%abi-load cx tuple ptr)))
                      (coerce vals 'list))
                    (let ((vals '())
                          (ai 0))
                      (loop for vt in ts
                            do (let ((c (%abi-ct vt ft-inst)))
                                 (cond
                                  ((and (wasmdown.component:ct-p c)
                                        (member (wasmdown.component:ct-kind c)
                                                '(:stream :future)))
                                   (push (%lift-async-end cx (nth ai args))
                                         vals)
                                   (incf ai))
                                    ;; M13-lad §2834/§3690: the (borrow $R)
                                    ;; param LENDS — resolve the index on
                                    ;; the CALLER's table (INST: the
                                    ;; lowering side, where the source
                                    ;; handle lives), add a borrow handle
                                    ;; to the CALLEE-DEFINING table
                                    ;; (FT-INST: the local shape the
                                    ;; callee's resource arms resolve
                                    ;; against), pass ITS index to the
                                    ;; callee's core; the num-borrows
                                    ;; counts the lend on the current task
                                    ;; until the call returns (§3690 — the
                                    ;; SYNC case has no subtask; the
                                    ;; call's own unwind below decrements).
                                    ;; The raw-index pass-through was the
                                    ;; M11b gap (::4: the callee's R-drop
                                    ;; hit whatever sat at the raw index).
                                    ((and (wasmdown.component:ct-p c)
                                          (eq (wasmdown.component:ct-kind c) :borrow)
                                          (integerp (wasmdown.component:ct-payload c))
                                          (< (wasmdown.component:ct-payload c)
                                             (length (comp-instance-types ft-inst)))
                                          ;; ONLY an ASYNC-TYPED callee: the
                                          ;; cross-task shape (the callee
                                          ;; holds the handle across a
                                          ;; task boundary). A SYNC-typed
                                          ;; callee ($C's rep/dtor — the
                                          ;; §2834 same-implementor case)
                                          ;; takes the REP positionally;
                                          ;; lending there broke ::2 (the
                                          ;; identity rep func returned
                                          ;; the index)
                                          (third (wasmdown.component:ct-payload ft)))
                                     (let* ((rt-idx (wasmdown.component:ct-payload c))
                                            (rt (aref (comp-instance-types ft-inst) rt-idx))
                                            (rep (%handle-rep inst rt (nth ai args)))
                                            ;; M16 slice 3b: a HOST callee's
                                            ;; provider resolves the index
                                            ;; through the dynamic caller's
                                            ;; table, so the lend goes THERE
                                            ;; (the guest-callee local shape
                                            ;; keeps the callee-defining table)
                                            (bi (and rep (%handle-add
                                                          (if (lifted-func-host f)
                                                              inst ft-inst)
                                                          rt rep nil))))
                                       (when bi
                                         (incf (async-task-num-borrows *current-task*)))
                                       (push bi vals)
                                       (incf ai)))
                                    (t
                                     (let* ((pflat (%flat-type-of vt ft-inst))
                                            (n (length pflat))
                                            (vi (make-array 16 :fill-pointer 0 :adjustable t)))
                                       (loop for k below n
                                             do (vector-push-extend
                                                 (cons (nth ai args) (nth k pflat))
                                                 vi)
                                                (incf ai))
                                       (push (first (%abi-lift-values cx 16 vi (list vt)))
                                             vals))))))
                        (nreverse vals))))
              ;; NOTE: no APPLY — %call-component-func-list takes the
              ;; args LIST as its third parameter (an EMPTY list
              ;; would be swallowed by apply's spread)
              (res (unwind-protect
                        (let ((lf2 (aref (comp-instance-funcs inst)
                                         fidx)))
                          (if (ignore-errors
                               (and *current-thread*
                                    (lifted-func-p lf2)
                                    (not (third
                                          (wasmdown.component:ct-payload
                                           (lifted-func-type lf2))))
                                    (%callee-module-imports-blocking-p
                                     lf2)))
                              ;; M13-tok9 THE COMPONENT-LEVEL
                              ;; NESTED ENTRY: the executor runs
                              ;; lifted-func-call — the CANONICAL
                              ;; component-level call — so the
                              ;; callee's params are LOWERED from
                              ;; the component args and its result
                              ;; LIFTED + handle-TRANSFERRED (the
                              ;; callee's table -> the abstract
                              ;; own-handle -> THIS instance's
                              ;; table) exactly as the inline path
                              ;; does; the raw core entry (the
                              ;; earlier shape) fed the callee's
                              ;; table index to the caller-side
                              ;; %abi-lower — idle-drop's transfer
                              ;; rows' '2 is not of type
                              ;; ASYNC-END' / 'invalid stream end
                              ;; index 1'.
                              (%sync-thunk-run
                               (lambda ()
                                 (apply #'lifted-func-call lf2
                                        component-args)))
                              (%call-component-func-list
                               inst fidx component-args)))
                     ;; §3690: the sync case's lent borrows are logically
                     ;; returned when the call returns
                     (when (and *current-task*
                                (> (async-task-num-borrows *current-task*) 0))
                       (decf (async-task-num-borrows *current-task*))))))
         (if (or core-result result)
             (list
              (if (and (consp res) (eq (first res) :own-handle))
                  ;; lower_own: the abstract handle enters THIS
                  ;; instance's table; the core side gets the INDEX
                  (%handle-add inst (second res) (third res) t)
                  (if result
                      ;; lower_flat_values: FLAT when <= 1 flat slot,
                      ;; else the result STORED at the [ptr] out-param
                      ;; (the last core arg). The SAME cx serves both
                      ;; spaces: its INST (the caller) owns the handle
                      ;; table a stream/future/own result enters, its
                      ;; TYPE-INST (FT-INST) resolves the result's
                      ;; (:idx . n) refs — the two-space split made the
                      ;; old second rcx redundant.
                      (let ((rflat (%flat-type-of result ft-inst)))
                        (let ((result-ct
                                (%abi-ct result (%abi-type-inst cx))))
                          (if (> (length rflat) 1)
                              ;; the SPILLED result: store at the
                              ;; [ptr] out-param (the LAST core
                              ;; param; the sig carries NO core
                              ;; results — the old (if core-result
                              ;; ...) gate skipped the store
                              ;; entirely and the caller read an
                              ;; empty retptr)
                              (let ((ptr (first (last args))))
                                (%abi-store cx res result-ct ptr)
                                nil)
                              (first (%abi-lower cx res
                                                 result-ct)))))
                      res)))
             '()))))))


(defun %call-component-func-list (inst fidx args)
  "Like %call-component-func but takes the args as a LIST (the lower
path's apply shape) and returns the single result or NIL."
  (let ((f (and (>= fidx 0) (< fidx (length (comp-instance-funcs inst)))
                (aref (comp-instance-funcs inst) fidx))))
    (typecase f
      (lifted-func (apply #'lifted-func-call f args))
      (wasmdown.runtime:func-instance
       (let ((res (apply #'wasmdown.runtime:wasm-call f args)))
         (first res)))
      (t (error 'wasmdown:link-error
                :message "lowered func target has no runtime value")))))

(defun %resource-current-inst (inst)
  "M13-lad §3783 current_instance() for the resource OPS: a handle
index refers to the table of the instance whose DYNAMIC CONTEXT is
on the thread — *HANDLE-CALLER-INST* (the M9 dynamic binding: the
comp-instance whose table an incoming own/borrow PARAM index
refers to, bound at the lower's call sites). A cross-task BORROW's
lent handle lives on the LOWERING CALLER's table ($E — the async
guest's binding); the callee's own task ($D — the sync-lift task
for drop-handle) is NOT where the index resolves. Same-instance
callers (the M9/M10 lanes: the defining instance's own tasks —
*HANDLE-CALLER-INST* eq the captured inst, or unbound at the
top-level runner) are a NO-OP."
  (or *handle-caller-inst* inst))

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
        ;; §3783: the handle resolves on the dynamic-context table.
        ;; M13-tok29: the remove's GUARD TRAPS on a miss (the
        ;; class-guard's 'invalid handle index' error) — the
        ;; miss-walk below must see the miss, so ignore-errors
        ;; around the first remove.
        (let* ((h (ignore-errors
                   (%handle-remove (%resource-current-inst inst)
                                   rt (first args))))
               (rep (and h (resource-handle-rep h))))
          ;; M13-tok29 THE BORROW MISS-WALK (the own-transfer
          ;; chain's companion to the guest-body conversion):
          ;; when the index misses the local table (the callee's
          ;; core received the REP for a borrowed param — the
          ;; spec's §2412 value — and passed it here as the
          ;; index: drop-cross-task-borrow's dont-drop stashes
          ;; its borrow param and calls this builtin with it),
          ;; resolve the param BY REP through the BORROW records
          ;; on the callee's own table (the BI handles the
          ;; lends added) and drop THE FOUND handle — the
          ;; fixture's cross-task drop works through the
          ;; borrowed record.
          (unless h
            (let* ((cur (%resource-current-inst inst))
                   (hit (%handle-hit-by-rep cur rt (first args))))
              (when hit
                (setf h (%handle-remove cur rt hit)
                      rep (first args)))))
          ;; an OWNING handle's drop runs the resource's dtor with
          ;; the rep (canon_resource_drop, CanonicalABI.md 3782).
          ;; M16 slice 3: a HOST-provided resource has no guest dtor
          ;; func-instance — its host finalizer (keyed by the ct,
          ;; the handle's rt) releases the capability instead.
          (when (and h (resource-handle-own-p h))
            (cond
              (dtor (apply #'wasmdown.runtime:wasm-call dtor (list rep)))
              (t (let ((hdtor (gethash rt *host-resource-dtor-table*)))
                   (when hdtor (funcall hdtor rep))))))
          '()))))
    (:resource-rep
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '(:i32))
      (lambda (args)
        ;; §3783: the handle resolves on the dynamic-context table
        (let ((h (%handle-resource-at (%resource-current-inst inst)
                                      (first args))))
          (list (resource-handle-rep h))))))))

;;; ---------------------------------------------------------------
;;; ---------------------------------------------------------------
;;; M11a: THE RUNTIME ASYNC CORE (the component-model task state
;;; machine — CanonicalABI.md Tasks §569-825, Threads §360-568,
;;; Concurrency §162-358, Store.tick §922-941, Waitable State
;;; §1218-1396, Subtask State §1397-1490, canon_lift §3389-3544,
;;; canon_lower §3618-3741 and the async built-ins §3835-4239).
;;;
;;; THREADING MODEL: the spec's cooperative Thread objects (suspend /
;;; resume / wait_until, §447-566) map onto SBCL OS threads: each
;;; guest Task runs on its own sb-thread:thread and coordinates
;;; through a (mutex, waitqueue) pair. `wait_until(ready)` = loop
;;; testing READY under the lock, condition-waiting when false;
;;; `resume` = a broadcast nudge (the woken thread re-tests its own
;;; condition — Store.tick's random.choice is the host's freedom, a
;;; nudge-all is a valid scheduling). The fixtures observe only the
;;; PROTOCOL VALUES (packed states, event codes, trap messages), not
;;; the interleaving.

;;; ---- Task (CanonicalABI.md 609-637) ----

(defstruct async-task
  "The Task record (CanonicalABI.md 609): the per-CALL state tracking
the callee's progression through the FuncInst protocol."
  (ft nil)                           ; the component functype ct
  (opts nil)                         ; the canon's opts
  (inst nil)                         ; the DEFINING comp-instance
  (on-start nil)                     ; thunk -> component-level args
  (on-resolve nil)                   ; (result | :cancelled) -> done
  (state :initial)   ; :initial :started :pending-cancel
                     ; :cancel-delivered :resolved
  (num-borrows 0)
  (implicit-thread nil)              ; the async-thread record
  (mutex (sb-thread:make-mutex :name "task"))
  (waitqueue (sb-thread:make-waitqueue :name "task"))
  (resolved-p nil)
  (gate-parked nil)                 ; parked at the backpressure gate
  (result nil)
  (seq nil)
  (tok-child-count 0)
  (turns 0))                        ; M14 the callback event loop's TURN
                                    ; COUNTER (§756-759: Thread.resume
                                    ; returns when the resumed callback
                                    ; "returns or blocks") — the boundary
                                    ; %task-request-cancellation's
                                    ; synchronous-resume wait observes

;;; M13-tok: THE DETERMINISTIC-INTERLEAVE TOKEN — the creation-order
;;; key (a PATH list) + the start turnstile + the FIFO gate.
;;;
;;; THE DECODE (async-calls-sync run1, the ~7/8 failing shape): the
;;; fixture's four $blocking-call callees share ONE instance
;;; ($AsyncInner); each call's core does context.set($counter) then
;;; $counter++ — the RETURNED value each subtask asserts is the
;;; counter AT ITS CORE'S ENTRY. The 3rd/4th subtasks' values crossed
;;; (4 stored at subtask 5's out-param and vice versa): the CORES
;;; entered in the order 2,3,5,4. Three layers were measured:
;;;   1. The plain creation seq (stamped at the gate) — the bodies'
;;;      thread-start shuffle re-assigns it (the 3rd/4th pair's
;;;      $SyncMiddle bodies run on DIFFERENT instances; their start
;;;      order is the OS thread lottery).
;;;   2. The per-instance FIFO alone — the earlier-parked check
;;;      cannot see a task that has not yet REACHED the gate.
;;;   3. A GLOBAL gate+turnstile — DEADLOCKS (the M11b attempt-2
;;;      negative result reproduced: a gate-parked task holds the
;;;      turnstile of a later task whose progress the parked task
;;;      itself needs).
;;; THE SHAPE THAT HOLDS: the PATH key (the call TREE's order,
;;; inherited: a nested callee's path extends the caller's), a
;;; GLOBAL start turnstile ordered by path (safe: a turnstile wait
;;; holds no gate; bodies then start in call order, so their nested
;;; callees are CREATED in call order), and the per-instance FIFO
;;; gate ordered by path (the exclusive instances' cores then run in
;;; call order). Plus the DURABLE resolve wake (the wset's parked
;;; waiters get a real wake-count: the resume-all nudge's broadcast
;;; can fall in the waiter's condition-wait registration window —
;;; run1's 4th delivery stalled there).

(defparameter *tok-path-counter* 0)

(defun %tok-path-earlier-p (a b)
  "The PATH comparison — the creation-order key's lexicographic
order ((1 1) < (1 2) < (2); the call tree's shape)."
  (loop for x in a
        for y in b
        do (cond
            ((< x y) (return t))
            ((> x y) (return nil)))
        finally (return (< (length a) (length b)))))

(defun %tok-stamp-task (task)
  "Stamp TASK's path: a ROOT task gets (n) (the global counter); a
task created INSIDE another task's call tree gets the parent's
path extended by this call's index (the per-parent :child-count on
the opts plist, guarded by the scheduler mutex)."
  (sb-thread:with-recursive-lock (*scheduler-mutex*)
    (let* ((parent (and *current-task*
                        (async-task-seq *current-task*)))
           (n (if parent
                  (incf (async-task-tok-child-count *current-task*))
                  (incf *tok-path-counter*))))
      (setf (async-task-seq task)
            (if parent
                (append parent (list n))
                (list n))))))

;;; ---- Waitable / Subtask / WaitableSet (CanonicalABI.md 1272-1489) ----

(defstruct async-waitable
  "The Waitable base (CanonicalABI.md 1272): a pending-event closure,
  its waitable set, and the sync-waiter guard."
  (pending-event nil)                ; closure -> (code p1 p2)
  (wset nil)
  (has-sync-waiter nil)
  ;; M15 MECHANISM 7: the thread parked in a SYNC read/write's
  ;; wait on THIS end — %end-notify posts the DURABLE wake to it
  ;; (the rendezvous's only other wake was the transient tick
  ;; broadcast, which a non-grantable waiter does not receive).
  (sync-waiter-thread nil))

(defstruct (async-subtask (:include async-waitable))
  "The Subtask (CanonicalABI.md 1406): the caller-side record."
  (state 0)  ; 0=STARTING 1=STARTED 2=RETURNED
             ; 3=CANCELLED-BEFORE-STARTED 4=CANCELLED-BEFORE-RETURNED
  (on-cancel nil)                    ; the callee's OnCancel
  (lenders nil)                      ; the LENT resource-handle records
  (lending-tasks nil)                ; the lending async-tasks
  (cancellation-requested nil)
  (flat-results nil)
  (resolve-delivered-p nil)
  (result-memory nil)                ; the async lower's out-param
  (result-ptr nil)                   ; memory + [ptr]
  (task nil))                        ; the callee's async-task

(defstruct async-waitable-set
  "The WaitableSet (CanonicalABI.md 1327): elems + the num_waiting
  guard for WaitableSet.drop."
  (elems nil)
  (num-waiting 0)
  (mutex (sb-thread:make-mutex :name "wset"))
  (waitqueue (sb-thread:make-waitqueue :name "wset"))
  ;; M13-tok2 GATE 1: the parked WAITER THREADS — %subtask-resolve
  ;; posts a REAL wake (the durable count) to each: the resume-all
  ;; nudge is TRANSIENT (a bare broadcast), and a parked waiter's
  ;; condition-wait registration can MISS it — the lost-broadcast
  ;; window (the set's OWN waitqueue broadcast reaches nobody: the
  ;; waiters park on their THREAD queues, not the set's)
  (waiters nil))

;;; ---- The {Stream,Future} End records (CanonicalABI.md 1598-1922) ----
;;; The End base derives from Waitable (both ends can join waitable
;;; sets). The 4 states (§1618): IDLE/COPYING/CANCELLING-COPY/DONE.
;;; The buffer: a guest linear-memory region — here captured as
;;; (list memory ptr length progress t) per the Readable/Writable
;;; GuestBuffer shape (§1545-1586), with the load/store helpers
;;; reusing the ABI's %abi-load/%abi-store per element type.

(defstruct (async-end (:include async-waitable))
  "The End base (CanonicalABI.md 1617): the readable/writable end
of a stream or future pair."
  (t-ct nil)                         ; the element type ct (stream<T>)
  (state :idle)   ; :idle :copying :cancelling-copy :done
  (other nil)                        ; the paired end (NIL = dropped)
  (buffer nil)    ; (list memory ptr length progress) — the guest buffer
  (owner nil)                        ; the owning comp-instance
  (index nil)                        ; the handles-table index (events)
  (event-code nil)  ; 2=STREAM_READ 3=STREAM_WRITE 4=FUTURE_READ 5=FUTURE_WRITE
  (future-p nil)   ; T = FutureEnd (the notify/DONE transitions differ)
  ;; M16 SLICE 2 — THE HOST BACKING. A provider-owned end carries the
  ;; CL closure that feeds/drains its PAIRED end (PLAN.md §8.3/§9.1:
  ;; the host side of stream<u8>/future<T>, no ambient globals):
  ;;   HOST-SINK (on a host READABLE end): (byte-list) -> error-code
  ;;     string or NIL. Consulted when the guest's writable peer
  ;;     stages a write: the bytes are drained immediately.
  ;;   HOST-SOURCE (on a host WRITABLE end): (max) -> (values
  ;;     byte-list eof-p). Consulted when the guest's readable peer
  ;;     asks: the bytes are pulled from the CL source now.
  ;;   HOST-VALUE(-P) (on a host WRITABLE future end): the resolved
  ;;     component-domain value, delivered at the guest's read.
  ;;   HOST-EOF-P: the source reported EOF (the writable peer is then
  ;;     DROPPED, the readable side sees DROPPED).
  ;;   HOST-ERROR: the sink/source's error-code (delivered through the
  ;;     resolved future).
  ;;   HOST-ON-DROP: (dropped-end error) -> side effect, fired when
  ;;     the PAIRED guest end is dropped (resolve the future).
  (host-sink nil)
  (host-source nil)
  (host-value nil)
  (host-value-p nil)
  (host-eof-p nil)
  (host-error nil)
  (host-on-drop nil))

(defun %end-copying-or-cancelling-p (end)
  (member (async-end-state end) '(:copying :cancelling-copy)))

;;; EventCode constants (CanonicalABI.md 1232).
(defconstant +event-stream-read+ 2)
(defconstant +event-stream-write+ 3)
(defconstant +event-future-read+ 4)
(defconstant +event-future-write+ 5)

;;; CopyResult (CanonicalABI.md 1799): COMPLETED/DROPPED/CANCELLED,
;;; packed with the progress for STREAM ends.
(defconstant +copy-completed+ 0)
(defconstant +copy-dropped+ 1)
(defconstant +copy-cancelled+ 2)

;;; new_{stream,future} (CanonicalABI.md 1907-1920): the linked pair.
(defun %new-async-pair (t-ct owner event-code future-p)
  "new_stream / new_future: the linked readable+writable ends."
  (let* ((r-event (if future-p +event-future-read+ +event-stream-read+))
         (w-event (if future-p +event-future-write+ +event-stream-write+))
         (reader (make-async-end
                  :t-ct t-ct :owner owner :event-code r-event
                  :future-p future-p))
         (writer (make-async-end
                  :t-ct t-ct :owner owner :event-code w-event
                  :future-p future-p)))
    (setf (async-end-other reader) writer)
    (setf (async-end-other writer) reader)
    (values reader writer)))

;;; End.notify (CanonicalABI.md 1826-1877): arm the pending event
;;; closure; the STREAM/FUTURE result precedence and DONE
;;; transitions differ (§1848-1882: streams DONE only on DROPPED;
;;; futures DONE on COMPLETED and DROPPED; COMPLETED wins).
(defun %end-notify (end progress)
  (setf (async-waitable-pending-event end)
        (lambda ()
          (let ((result
                  (cond
                   ;; FUTURE: COMPLETED conveys the essential info
                   ((and (async-end-future-p end) (= progress 1))
                    (setf (async-end-buffer end) nil)
                    (setf (async-end-state end) :done)
                    +copy-completed+)
                   ((null (async-end-other end))
                    ;; DROPPED (the peer is gone)
                    (setf (async-end-buffer end) nil)
                    (setf (async-end-state end) :done)
                    +copy-dropped+)
                   ;; CANCELLED (a cancel-copy was requested)
                   ((eq (async-end-state end) :cancelling-copy)
                    (setf (async-end-buffer end) nil)
                    (setf (async-end-state end) :idle)
                    +copy-cancelled+)
                   ;; COMPLETED (the copy finished)
                   ((eq (async-end-state end) :copying)
                    (setf (async-end-buffer end) nil)
                    (setf (async-end-state end)
                          (if (async-end-future-p end) :done :idle))
                    +copy-completed+)
                   (t
                    (setf (async-end-buffer end) nil)
                    (setf (async-end-state end) :idle)
                    +copy-completed+))))
            (list (async-end-event-code end)
                  (or (async-end-index end) 0)
                  (if (async-end-future-p end)
                      result          ; futures do NOT pack progress
                      (logior result (ash progress 4)))))))
  ;; M15 MECHANISM 7 — THE SYNC RENDEZVOUS WAKE: a thread parked in
  ;; this end's SYNC read/write re-tests on a REAL wake.
  (let ((wt (async-waitable-sync-waiter-thread end)))
    (when wt (%async-thread-wake wt))))

;;; ---- The GuestBuffer (CanonicalABI.md 1545-1586) ----
;;; A buffer is (list memory ptr length progress realloc encoding) — the
;;; load/store helpers below advance ptr and progress per element. The
;;; last two carry the READ side's canon opts (M16 slice 3b): storing a
;;; STRUCTURED element (a directory-entry's name string) needs the
;;; canon lower's realloc, exactly as `store` does at any other
;;; boundary.

(defconstant +buffer-max-length+ (1- (expt 2 28)))

(defun %buffer-make (end memory ptr length &key (realloc nil) (encoding :utf8))
  "GuestBuffer's eager guards (§1552-1556): the alignment + bounds
trap at construction; zero-length buffers skip them."
  (let* ((t-ct (async-end-t-ct end))
         (inst (async-end-owner end))
         (elem-size (and t-ct
                         (handler-case
                             (%abi-elem-size t-ct inst)
                           (error () 1)))))
    (when (> length +buffer-max-length+)
      (error 'wasmdown:wasm-trap :kind :generic
             :message "buffer length exceeds the maximum"))
    (when (and t-ct (plusp length))
      (let ((align (handler-case
                       (%abi-alignment t-ct inst)
                     (error () 1))))
        (unless (zerop (mod ptr align))
          (error 'wasmdown:wasm-trap :kind :generic
                 :message (format nil "unaligned pointer (~a)"
                                  "stream buffer"))))
      ;; memory-size returns PAGES — compare against the BYTES
      ;; vector's length (the byte length, §1556)
      (when (> (+ ptr (* length (or elem-size 1)))
               (length (wasmdown.runtime:memory-instance-bytes memory)))
        (error 'wasmdown:wasm-trap :kind :generic
               :message "out of bounds memory access")))
    (list memory ptr length 0 realloc encoding)))

;;; A HOST buffer (M16 slice 2): the CL-side stand-in for a guest
;;; linear-memory region. It carries the same (capacity progress) shape
;;; so the copy/take/put paths need no second protocol — a provider (or
;;; a CL-only test) can act as the other side of a stream without a
;;; guest memory.
(defstruct (host-buffer (:constructor make-host-buffer
                                      (capacity &key values)))
  (capacity 0)
  (progress 0)
  ;; VALUES is the element queue (domain values, in order): the staging
  ;; side of a CL-only write, or the destination's accumulated reads.
  ;; PROGRESS counts taken/put elements, so CAPACITY - PROGRESS is the
  ;; remainder the copy protocol sees.
  (values nil))

(defun %buffer-remain (buf)
  (if (host-buffer-p buf)
      (- (host-buffer-capacity buf) (host-buffer-progress buf))
      (- (third buf) (fourth buf))))

(defun %buffer-progress (buf)
  (if (host-buffer-p buf) (host-buffer-progress buf) (fourth buf)))

(defun %own-elem-take (cx ptr t-ct inst)
  "M13-tok25: the OWN-typed stream element's SOURCE-side transfer
at the copy boundary — the raw handle index at PTR is REMOVED from
the SOURCE instance's table (type-checked against the own's
resource type) and the abstract (:own-handle rt rep) record is
returned; the %buffer-put re-adds at the dest. A failed remove
(the guard traps, a stale index) falls back to the raw index."
  (let* ((idx (%abi-load-int cx ptr 4))
         (rt (and (integerp (wasmdown.component:ct-payload t-ct))
                  (< (wasmdown.component:ct-payload t-ct)
                     (length (comp-instance-types inst)))
                  (aref (comp-instance-types inst)
                        (wasmdown.component:ct-payload t-ct))))
         (h (and rt (ignore-errors (%handle-remove inst rt idx)))))
    (if h (list :own-handle rt (resource-handle-rep h)) idx)))

(defun %buffer-take (buf n t-ct inst)
  "ReadableGuestBuffer.read(n) (§1567-1575): load N elements from
the buffer's memory at its advancing ptr; returns the values list
and advances the buffer's progress. A HOST buffer yields its first N
domain values (no ABI encoding — there is no guest memory)."
  (when (host-buffer-p buf)
    (let* ((values (host-buffer-values buf))
           (k (min n (length values)))
           (vals (subseq values 0 k)))
      (setf (host-buffer-values buf) (nthcdr k values))
      (incf (host-buffer-progress buf) k)
      (return-from %buffer-take vals)))
  (let* ((memory (first buf))
         (elem-size (and t-ct
                         (handler-case
                             (%abi-elem-size t-ct inst)
                           (error () 1))))
         (ptr (+ (second buf) (* (fourth buf) (or elem-size 1))))
         (vals nil))
    (if t-ct
        (when (plusp n)
          ;; M13-tok25 THE OWN-TRANSFER AT THE COPY BOUNDARY: an
          ;; OWN-typed element (a (stream (own $R))'s resource)
          ;; would load as the RAW handle index by %abi-load's
          ;; default arm — the M9 own-transfer (the handle LEAVES
          ;; the source table, the rep rides) and the M10 buffer
          ;; met HERE and the buffer carried the bare index
          ;; (passing-resources' 'NIL is not of type REAL'). The
          ;; take performs the SOURCE-side transfer; the put
          ;; re-adds.
          (let ((cx (make-abi-cx :memory memory
                                 :encoding (or (nth 5 buf) :utf8)
                                 :inst inst))
                (own-p (and (wasmdown.component:ct-p t-ct)
                            (eq (wasmdown.component:ct-kind t-ct)
                                :own))))
            (dotimes (i n)
              (push (if own-p
                        (%own-elem-take cx ptr t-ct inst)
                        (%abi-load cx t-ct ptr))
                    vals)
              (incf ptr (or elem-size 1)))))
        ;; an EMPTY element type (§1567): n logical values, each
        ;; zero bytes — the COUNT drives the progress (forward's
        ;; empty-element-type expects COMPLETED|n<<4)
        (dotimes (i n)
          (push nil vals)))
    (incf (fourth buf) n)
    (nreverse vals)))

(defun %buffer-put (buf vals t-ct inst)
  "WritableGuestBuffer.write (§1577-1585): store the VALUES list
into the buffer's memory at its advancing ptr, advancing the
progress by the count. A HOST buffer accumulates the domain values."
  (when (host-buffer-p buf)
    (setf (host-buffer-values buf)
          (append (host-buffer-values buf) (copy-list vals)))
    (incf (host-buffer-progress buf) (length vals))
    (return-from %buffer-put (length vals)))
  (let* ((memory (first buf))
         (elem-size (and t-ct
                         (handler-case
                             (%abi-elem-size t-ct inst)
                           (error () 1))))
         (n (length vals))
         (ptr (+ (second buf) (* (fourth buf) (or elem-size 1)))))
    (when (and t-ct vals)
      (let ((cx (make-abi-cx :memory memory
                             :realloc (nth 4 buf)
                             :encoding (or (nth 5 buf) :utf8)
                             :inst inst)))
        ;; M13-tok25 THE OWN-TRANSFER'S DEST HALF: an
        ;; (:own-handle rt rep) element (the take's source-side
        ;; transfer) is RE-ADDED into THIS instance's table and
        ;; the new index stored — the consumer's core then holds
        ;; a live handle whose rep survives.
        (dolist (v vals)
          (if (and (consp v) (eq (car v) :own-handle))
              (%abi-store-int cx (%handle-add inst (second v)
                                              (third v) t)
                              ptr 4)
              (%abi-store cx v t-ct ptr))
          (incf ptr (or elem-size 1)))))
    (incf (fourth buf) n)))

;;; ---- The HOST-BACKED ends (M16 slice 2, PLAN.md §8.3/§9.1) ----
;;; A provider turns one end of a stream/future pair into the HOST
;;; side: a readable end with a SINK (the guest's writes drain into a
;;; CL stream) or a writable end with a SOURCE (the guest's reads pull
;;; from a CL stream) / a resolved value (a future). The copy's case 2
;;; (the peer has no buffer) consults these slots instead of parking —
;;; the canonical stream protocol, with the host as the peer.

(defun make-host-stream-pair (element-type &key (owner nil))
  "The linked (readable . writable) STREAM end pair whose element type
is ELEMENT-TYPE (a ct); the provider backs one end and hands the other
to the guest. Same pair/linkage as canon stream.new."
  (%new-async-pair element-type owner +event-stream-read+ nil))

(defun make-host-future-pair (element-type &key (owner nil))
  "The linked (readable . writable) FUTURE end pair for ELEMENT-TYPE."
  (%new-async-pair element-type owner +event-future-read+ t))

(defun host-end-set-sink (end sink)
  "Make the READABLE end END drain the bytes its guest writer stages
into SINK: (byte-list) -> error-code string or NIL."
  (setf (async-end-host-sink end) sink)
  end)

(defun host-end-set-source (end source)
  "Make the WRITABLE end END feed its guest reader from SOURCE:
(max) -> (values byte-list eof-p)."
  (setf (async-end-host-source end) source)
  end)

(defun host-end-set-on-drop (end hook)
  "HOOK is (dropped-end error-code) -> side effect, run when the end
paired with the host end END is dropped (resolve the future)."
  (setf (async-end-host-on-drop end) hook)
  end)

(defun host-end-set-error (end code)
  "Record CODE (a WIT error-code label) as the host end END's error:
the sink/source path that detects it drops END, whose one-shot hook
then resolves the guest's result future with (:err . CODE)."
  (setf (async-end-host-error end) code)
  end)

(defun host-end-drop (end)
  "Drop the HOST end END: its guest peer learns DROPPED (the WIT's
`closed' read status) and END's own one-shot hook fires. The provider
side of a refused stream uses this to close a stream it cannot feed."
  (%host-drop end))

(defun %host-peer-dropped (host-end dropped)
  "The DROP NOTIFICATION: the guest dropped DROPPED, which was paired
with the host end HOST-END. Fire its one-shot hook."
  (let ((hook (async-end-host-on-drop host-end)))
    (when hook
      (setf (async-end-host-on-drop host-end) nil)
      (funcall hook dropped (async-end-host-error host-end)))))

(defun %host-drop (host-end)
  "Drop the host end HOST-END: its guest peer learns DROPPED and the
host end's one-shot hook fires (the future resolves)."
  (%end-drop host-end)
  (when (async-end-host-on-drop host-end)
    (%host-peer-dropped host-end nil))
  (setf (async-end-host-source host-end) nil))

(defun host-end-drain (reader)
  "The provider-side drain: READER is the host READABLE end a provider
just took over (wasi:cli's write-via-stream). If the guest staged a
write before the call, its parked buffer drains into the sink now;
returns the number of elements drained (0 when nothing was staged)."
  (let ((writer (async-end-other reader)))
    (if (and writer (async-end-host-sink reader)
             (let ((buf (async-end-buffer writer)))
               (and buf (plusp (%buffer-remain buf)))))
        (%host-sink-drain writer)
        0)))

(defun %host-sink-drain (writer)
  "Drain WRITER's staged buffer (case 2 parked it there) into the
HOST-SINK of its readable peer, then notify WRITER COMPLETED. A sink
error drops the readable peer (the guest's next write sees DROPPED)
and rides the resolved future."
  (let* ((buf (async-end-buffer writer))
         (reader (async-end-other writer))
         (sink (and reader (async-end-host-sink reader)))
         (n (if buf (%buffer-remain buf) 0)))
    (cond
      ((null buf) 0)
      ((or (zerop n) (null sink))
       (setf (async-end-buffer writer) nil)
       0)
      (t
       (let* ((vals (%buffer-take buf n (async-end-t-ct writer)
                                  (async-end-owner writer)))
              (progress (%buffer-progress buf))
              (err (funcall sink vals)))
         (setf (async-end-buffer writer) nil)
         (if err
             (progn
               (setf (async-end-host-error reader) err)
               (%host-drop reader))
             (%end-notify writer progress))
         n)))))

(defun %host-source-fill (reader buffer peer)
  "Fill READER's BUFFER from the SOURCE on its writable peer PEER:
bytes now -> COMPLETED with the count; none + EOF -> PEER dropped and
READER notified DROPPED; none + not EOF -> park BUFFER and wait."
  (if (async-end-host-eof-p peer)
      (progn (setf (async-end-buffer reader) nil) (%host-drop peer))
      (multiple-value-bind (bytes eof)
          (funcall (async-end-host-source peer)
                   (%buffer-remain buffer))
        (when eof (setf (async-end-host-eof-p peer) t))
        (cond
          ((plusp (length bytes))
           (%buffer-put buffer bytes (async-end-t-ct reader)
                        (async-end-owner reader))
           (%end-notify reader (%buffer-progress buffer)))
          (eof
           ;; §: the host writer's end is gone — DROPPED to the reader
           (setf (async-end-buffer reader) nil)
           (%host-drop peer))
          (t (setf (async-end-buffer reader) buffer))))))

(defun %host-value-deliver (reader buffer peer)
  "Deliver the resolved HOST-VALUE on PEER into READER's BUFFER
(COMPLETED). The value is consumed once (a future reads once)."
  (let ((v (async-end-host-value peer)))
    (setf (async-end-host-value-p peer) nil
          (async-end-buffer reader) nil)
    (%buffer-put buffer (list v) (async-end-t-ct reader)
                 (async-end-owner reader))
    (%end-notify reader 1)))

(defun host-future-resolve (writer value)
  "Resolve the host WRITABLE future end WRITER with the component
domain VALUE (`(:ok)` / `(:err . \"io\")`): delivered at the guest's
read — or now, if the guest is already parked with a buffer."
  (setf (async-end-host-value writer) value
        (async-end-host-value-p writer) t)
  (let ((reader (async-end-other writer)))
    (when (and reader (async-end-buffer reader))
      (let ((buf (async-end-buffer reader)))
        (setf (async-end-buffer reader) nil)
        (%buffer-put buf (list value) (async-end-t-ct reader)
                     (async-end-owner reader))
        (%end-notify reader 1))))
  value)

(defun host-end-read (reader)
  "The provider-side read of a GUEST-RESOLVED value on READER (the
READABLE future end a guest handed over and later wrote through its
writable peer). Only an already-resolved value is read: a value the
peer delivered into the HOST-VALUE slot, or a copy that is already
parked on the peer's buffer (the guest's `future.write' staged it).
Returns (values value present-p); (values NIL NIL) when nothing is
ready — the provider never blocks the executor on a guest future."
  (let* ((peer (async-end-other reader))
         (buffer (and peer (async-end-buffer peer))))
    (cond
      ((null peer) (values nil nil))
      ((async-end-host-value-p peer)
       (let ((v (async-end-host-value peer)))
         (setf (async-end-host-value-p peer) nil)
         (values v t)))
      ((or (null buffer) (zerop (%buffer-remain buffer)))
       (values nil nil))
      (t
       ;; the canonical copy: READER's host buffer <- PEER's staged
       ;; buffer (one future element). The protocol's notifications
       ;; fire exactly as for a guest-to-guest copy.
       (let ((buf (make-host-buffer 1)))
         (ignore-errors (%end-copy reader buf t))
         (let ((vals (host-buffer-values buf)))
           (if vals
               (values (first vals) t)
               (values nil nil))))))))

(defun host-resource-take (rt index)
  "Consume the OWN resource handle at INDEX: the rep is REMOVED from
the dynamic caller's handle table (the canon-lower own-transfer) and
returned. The provider now owns the rep and must release it through
its own finalizer. (values NIL NIL) when no such handle exists (a
direct CL call passes the rep itself)."
  (let* ((inst (or *handle-caller-inst* *host-provider-inst*))
         (h (and inst (integerp index)
                 (ignore-errors (%handle-remove inst rt index)))))
    (if h (values (resource-handle-rep h) t) (values nil nil))))

;;; ---- End.copy (CanonicalABI.md 1687-1711): the 5 cases ----

(defun %none-or-number-type-p (t-ct)
  "The same-instance copy trap's exemption (§1695): number-typed
elements may copy within one instance. T-CT may be a bare keyword
(the (stream u8) payload) — resolve through %abi-ct."
  (let ((c (and t-ct (%abi-ct t-ct nil))))
    (or (null t-ct)
        (and c
             (eq (wasmdown.component:ct-kind c) :prim)
             (member (wasmdown.component:ct-payload c)
                     '(:u8 :s8 :u16 :s16 :u32 :s32 :u64 :s64
                       :f32 :f64 :float16 :float32 :float64
                       :char :bool))))))

(defun %end-copy (end buffer is-read)
  "End.copy (§1687): the single parameterized copy. The buffer is
the (memory ptr length progress) list. The 5 cases in order:
1. racily-dropped peer (pending event already armed);
2. no peer buffer — park mine, wait for the peer;
3. both non-zero — copy the min, notify both, drop the drained one;
4. mine non-zero or (read + the peer's zero-length) — notify the
   peer with progress 0, take my buffer as pending;
5. my zero-length — notify myself with progress 0."
  (setf (async-end-state end) :copying)
  (let ((other (async-end-other end)))
    (cond
     ;; case 1: the peer was racily dropped (its drop armed my event)
     ((null other)
      ;; the drop already notified us (the pending event holds)
      nil)
     ;; case 2: the peer has no buffer — park mine, wait for it. A
     ;; HOST-BACKED peer (M16 slice 2) acts NOW instead: a sink drains
     ;; my staged write, a source fills my read buffer, a resolved
     ;; future delivers its value.
     ((null (async-end-buffer other))
      (cond
        ((and (async-end-host-sink other)
              (plusp (%buffer-remain buffer)))
         (setf (async-end-buffer end) buffer)
         (%host-sink-drain end))
        ((async-end-host-source other)
         (%host-source-fill end buffer other))
        ((async-end-host-value-p other)
         (%host-value-deliver end buffer other))
        (t (setf (async-end-buffer end) buffer))))
     ;; case 3: both have remain > 0 — copy the min
     ((and (plusp (%buffer-remain buffer))
           (plusp (%buffer-remain (async-end-buffer other))))
      ;; the same-instance non-numeric trap (§1695)
      (when (and (async-end-owner end)
                 (eq (async-end-owner end) (async-end-owner other))
                 (not (%none-or-number-type-p (async-end-t-ct end))))
        (error 'wasmdown:wasm-trap :kind :generic
               :message
               "cannot read from and write to intra-component future/stream"))
      (let ((n (min (%buffer-remain buffer)
                    (%buffer-remain (async-end-buffer other))))
            (inst (async-end-owner end))
            (t-ct (async-end-t-ct end)))
        (if is-read
            ;; the READER's buffer is the DESTINATION: load n from
            ;; the peer's (writable-side staged) buffer, store into
            ;; mine — the values move peer -> me
            (let* ((peer (async-end-buffer other))
                   ;; M13-tok25 THE OWN REMOVE'S SIDE: the
                   ;; element's handle lives in the WRITER's
                   ;; table — the transfer removes it THERE.
                   (vals (%buffer-take peer n t-ct
                                       (async-end-owner other))))
              (%buffer-put buffer vals t-ct inst))
            ;; the WRITER's buffer is the SOURCE: load n from mine,
            ;; store into the peer's writable buffer — me -> peer
            (let* ((vals (%buffer-take buffer n t-ct inst)))
              (%buffer-put (async-end-buffer other) vals t-ct inst)))
        ;; notify BOTH with their progress; the fully-drained
        ;; peer buffer is dropped
        (%end-notify end (%buffer-progress buffer))
        (%end-notify other (%buffer-progress (async-end-buffer other)))
        (when (zerop (%buffer-remain (async-end-buffer other)))
          (setf (async-end-buffer other) nil))))
     ;; case 4: mine non-zero, or a read meeting the peer's
     ;; zero-length — the peer learns progress 0 and I park
     ((or (plusp (%buffer-remain buffer))
          (and is-read (zerop (%buffer-remain
                               (async-end-buffer other)))))
      (%end-notify other 0)
      (setf (async-end-buffer other) nil)
      (setf (async-end-buffer end) buffer))
     ;; case 5: my zero-length write meeting a non-zero peer —
     ;; notify myself with progress 0
     (t
      (%end-notify end 0)))))

;;; ---- lift_async_value / lower_async_value (CanonicalABI.md 2432-2460
;;; ---- / 2851-2865): the READABLE end crossing a component boundary.

(defun %end-drop (end)
  "End.drop (CanonicalABI.md 1778-1790): notify the peer (progress 0,
DROPPED on its next event delivery) unless it is already DONE or
already has a pending event; detach both ends. M16 slice 2: the
detached HOST peer's one-shot hook fires (a provider resolves its
future when the guest drops the stream's writable end)."
  (let ((other (async-end-other end)))
    (when other
      (unless (or (eq (async-end-state other) :done)
                  (async-waitable-pending-event other))
        (%end-notify other 0))
      (setf (async-end-other other) nil))
    (setf (async-end-other end) nil)
    (when (and other (async-end-host-on-drop other))
      (%host-peer-dropped other end)))
  ;; Waitable.drop: leave any waitable set
  (%waitable-join end nil))

(defun %end-cancel (end)
  "End.cancel (CanonicalABI.md 1732-1740): COPYING -> CANCELLING-
COPY, arming the pending event with progress 0 (the deterministic
profile's always-notify — the host-owned nondeterministic branch
is the host's freedom, taken as notify here) unless one is
already armed (never overwrite a previous progress)."
  (setf (async-end-state end) :cancelling-copy)
  (unless (async-waitable-pending-event end)
    (%end-notify end 0)))

(defun %lift-async-end (cx i)
  "lift_stream / lift_future (§2432-2460): the i32 index I in CX's
instance resolves the READABLE end; the guards; the end transfers
OUT of this table (the caller re-adds it at its boundary)."
  (let* ((inst (abi-cx-inst cx))
         (end (%handle-at inst i #'async-end-p "stream end")))
    (unless (member (async-end-event-code end)
                    (list +event-stream-read+ +event-future-read+))
      (error 'wasmdown:wasm-trap :kind :generic
             :message "handle index used with the wrong type"))
    (unless (eq (async-end-state end) :idle)
      (error 'wasmdown:wasm-trap :kind :generic
             :message "cannot lift a stream or future in a non-idle state"))
    (when (%waitable-in-wset end)
      (error 'wasmdown:wasm-trap :kind :generic
             :message "cannot lift stream while it's in a waitable set"))
    (setf (aref (%inst-handles inst) (1- i)) nil)
    (%handle-free-slot inst i)
    ;; §2446: lift_async_value clears the owner (lower re-sets it)
    (setf (async-end-owner end) nil)
    end))

(defun %lower-async-end (end cx)
  "lower_stream / lower_future (§2851-2865): the END enters the
callee's table; returns the new index. IDEMPOTENT per (end,
instance): a second lower of the SAME end into the SAME instance
(the multi-site result path — the sync lower's lower_flat_values
AND the %abi-store out-param site both fire for one boundary
crossing) returns the EXISTING index instead of re-adding (the
wrong-index probe: one transfer produced index 3)."
  (let* ((inst (abi-cx-inst cx))
         (h (%inst-handles inst))
         (i (and (> (length h) 0)
                 (position end h :test #'eq :from-end t))))
    (if i
        (1+ i)
        (let ((j (%handle-add-any inst end)))
          (setf (async-end-index end) j)
          (setf (async-end-owner end) inst)
          j))))

;;; ---- The Thread record (CanonicalABI.md 391-407) ----

(defstruct async-thread
  "The Thread record (CanonicalABI.md 391): the guest thread's
  blocking state + the per-thread STORAGE (context.get/set TLS)."
  (task nil)
  (index nil)
  (spawn-thunk nil)                   ; new-indirect's suspended body
  (storage (make-array 2 :initial-element 0))
  (cancellable nil)
  (abandoned nil)                    ; store-teardown mark: parks unwind, the thread exits
  (yield-parked nil)                 ; parked by async-thread-yield (READY)
  (suspended-p nil)                  ; SUSPENDED (§494): only an explicit resume wakes
  (ready-p nil)                      ; §458-465 start_waiting(ready=True): resume-later's
                                     ; mark — the next tick (§932) fires the pending
                                     ; spawn / grants a suspend-park, or an explicit
                                     ; switch starts/resumes it
  (wake-count 0 :type (unsigned-byte 64))
                                     ; M13-4 the DURABLE wake count: every REAL wake of
                                     ; a park (an explicit resume, the §242-273 handoff
                                     ; post — the target's park/exit handing control
                                     ; back to its switcher) increments it ATOMICALLY
                                     ; and broadcasts BOTH waitqueues. A park consumes
                                     ; the whole count; one posted before the park's
                                     ; wait registers is still consumed at park time —
                                     ; the lost-broadcast window does not exist. Ticks
                                     ;; are transient (no count): a yield-park retries
                                     ;; on the next tick, a wait-until re-tests its
                                     ;; own readiness.
  (post-to nil)                      ; M13-4 the HANDOFF slot: the switcher this thread
                                     ; must hand control back to at its NEXT park/exit
                                     ; (§242-273's handler lock — installed through
                                     ; async-thread-resume's locked section, posted
                                     ;; once by %async-thread-post-handoff)
  (mutex (sb-thread:make-mutex :name "thread"))
  (waitqueue (sb-thread:make-waitqueue :name "thread"))
  (suspend-waitqueue (sb-thread:make-waitqueue :name "thread-suspend"))
  ;; M14 THE PARK READINESS (the spec's ready_func, §433-440/§447-456,
  ;; made explicit): the closure installed at each WAITING park (the
  ;; callback event loop's YIELD/WAIT turns) and cleared at the wake.
  ;; A thread with no ready-fn is SUSPENDED (§494 — only an explicit
  ;; resume), running, or exited; Task.request_cancellation's §769
  ;; gate (cancellable AND ready) tests this slot — the canceller
  ;; waits for the callee's turn boundary only when the callee is
  ;; actually ready to take its turn.
  (ready-fn nil)
  ;; M13-tok19: the SBCL thread handle (for the teardown JOIN —
  ;; the abandoned threads' deaths must complete before the
  ;; next script starts; the unwind storm stole its ticks).
  ;; M14 THE TURN GRANT (§932's model made explicit): set by
  ;; async-thread-resume-all — the REAL tick (the drain loops, the
  ;; caller's blocking-op-driven resume-all) — for every thread the
  ;; tick nudges; the callback loop's post-first-turn GATE parks until
  ;; granted (the unsupervised-continuation decode: the worker's
  ;; OS-thread continuation — end-copy -> WAIT -> loop-top -> the
  ;; callback's task.return — completed inside the caller's cancel
  ;; window with no grant; the reference's continuation is granted by
  ;; Store.tick and the caller does not tick until it blocks). The
  ;; deferred/incidental propagations (the cancel-wait window's four
  ;; sites) never set it — the incidental broadcasts do not grant.
  (granted nil)
  ;; M15 THE BLOCKED STATE (§932's tick-execution model): set while
  ;; this thread sits in a BLOCKING primitive (wait-until, yield,
  ;; suspend, the callback WAIT park, the switch park). The
  ;; STARTED-wait's FIRST-TURN BOUNDARY (mechanism 5) reads it; the
  ;; invoke drain's tick gate (mechanism 2) also consults it. Pure
  ;; bookkeeping here — no behaviour changes.
  (block-depth 0 :type (unsigned-byte 64))
  (io-parked nil)
  (native nil)
  ;; STEP 4 — THE STORE RIGHT's per-ticket state (see the
  ;; "THE STORE RIGHT" section below): the per-slice HOP BUDGET (a
  ;; yield wakes its successor only while the budget lasts — a
  ;; rotation of false-positive contenders then costs one bounded
  ;; slice instead of a tight wake loop), and the CONTENDER mark.
  (right-hops 0 :type fixnum)
  ;; TRUE while this thread is CONTENDING for the store right (it is
  ;; inside %right-acquire). Only active contenders are eligible
  ;; handoff targets, which is what keeps a park from rotating
  ;; through parked sleepers.
  (right-waiting nil)
  ;; STEP 4-QUANTUM: TRUE while this thread's QUANTUM is in progress
  ;; — it has taken the right and has not yet parked, stalled,
  ;; handed off or exited. Its one mid-quantum release is
  ;; %without-right (the peer-mutex fan-out), which RESERVES the
  ;; ticket for this thread instead of naming a successor: the
  ;; release does not end the turn, so no peer may take the window
  ;; as its own. Set by %right-take, cleared at every real turn
  ;; boundary.
  (right-turn-p nil))

(defun %block-enter (th) (when th (sb-ext:atomic-incf (async-thread-block-depth th))))
(defun %block-exit (th) (when th (sb-ext:atomic-decf (async-thread-block-depth th))))
(defun %thread-blocked-p (th) (and th (> (async-thread-block-depth th) 0)))
(defun %drain-tick-needed-p (root)
  "M15 — `a tick can help => spend it`: the ROOT parked, or any LIVE
thread a GRANTEE only a tick wakes (yield-park, ready-mark, armed
non-IO park ready). IO parks belong to the root's block tick. THE
LIVENESS INVARIANT: a hang means this classification missed a site."
  (or (%thread-blocked-p root)
      (let ((need nil))
        (sb-thread:with-recursive-lock (*scheduler-mutex*)
          (dolist (t2 *scheduler-threads*)
            (unless (or (eq t2 root) (async-thread-abandoned t2))
              (when (or (async-thread-yield-parked t2)
                        (async-thread-ready-p t2)
                        (and (async-thread-ready-fn t2)
                             (not (async-thread-io-parked t2))
                             (ignore-errors
                               (funcall (async-thread-ready-fn t2)))))
                (setf need t)))))
        need)))

;;; ---- Thread-local context: bound per SBCL thread by the task
;;; body, so the built-ins find their task/instance. SBCL
;;; defparameter bindings made inside sb-thread:make-thread bodies
;;; are per-thread, so a guest thread sees ITS task.

(defparameter *current-task* nil
  "The current Thread's Task (CanonicalABI.md current_task).")

(defparameter *current-thread* nil
  "The current Thread record (CanonicalABI.md current_thread).")

(defparameter *cancel-window-inst* nil
  "M14 THE PER-INSTANCE CANCEL-WINDOW GATE: the cancelled task's
instance — during the synchronous-resume wait, the drain's ticks
(the outer invoke drain's ~2ms grants) must not grant THIS
instance's OTHER parked callbacks (cancel-targeted-resume ::7/::9's
worker: its event ready, granted inside the cancel window where
the fixtures pin the caller's own observation first). The rest of
the store keeps its grants (the during-sync family's interleave
depends on the pre-window ticks — the whole-store suppression
measured as the acs::2 + ctr::5 regressions).")

(defparameter *deferred-resume-all-tasks* nil
  "M14: the cancel-wait window's deferred propagation queue (drained
when the window closes).")

(defparameter *defer-resume-all* nil
  "M14 THE CANCEL-WAIT WINDOW'S TICK DEFERRAL: set around
%task-request-cancellation's synchronous-resume wait — the resumed
callee's own task.{return,cancel} M11a propagation (the resume-all)
would grant OTHER ready threads their turn inside the wait
(cancel-targeted-resume ::7's worker: ready via its future's event,
woken by the propagation's broadcast, running its callback and
delivering the SUBTASK event at the caller's NEXT poll — the row
pins the worker STAYS PUT until the caller blocks). The deferred
resume-all runs when the window closes.")

(defparameter *suppress-spawn-fire* nil
  "M14 THE PACK WINDOW'S SPAWN-FIRE SUPPRESSION: dynamically bound
around the async lower's STARTED-wait + pack — the callee instance's
resume-later'd EXPLICIT threads (the keeper/worker class) must not
take their turn inside the caller's observation window (the packed
return + the cancel's BLOCKED read) — the reference's caller
regains control synchronously (§3530) and the ready spawns run at
the caller's NEXT blocking op (the fixtures' pinned interleave).")

(defparameter *scheduler-threads* nil
  "The live async-thread records — Store.tick's nudge-all walks it.
MUTATED FROM MULTIPLE GUEST THREADS (enter/exit implicit thread):
every access goes through *SCHEDULER-MUTEX* — a bare pushnew/remove
races (a lost registration leaves a thread permanently un-nudgable,
the observed acs deadlock).")

(defparameter *scheduler-mutex* (sb-thread:make-mutex
                                 :name "scheduler"))

(defparameter *async-progress* 0
  "M11a liveness counter: bumped on every observable state change
(a thread waking, a task state transition). The canon_lift caller
loop uses it as the deadlock oracle (§3539-3542): if a full
nudge+wait cycle passes with NO progress and the task unresolved,
no candidate thread can ever make progress — the deadlock trap.")

;;; ---------------------------------------------------------------
;;; STEP 4 — THE STORE RIGHT: ONE GUEST THREAD EXECUTES AT A TIME.
;;;
;;; THE INVARIANT. A guest thread runs guest code only while it holds
;;; *STORE-RIGHT* (an ordinary SBCL mutex). Every blocking primitive
;;; in this runtime is one of two shapes, and both release the right
;;; across their wait:
;;;
;;;   %park   (self mutex queue)      — release, bounded wait on
;;;                                     QUEUE/MUTEX, re-acquire.
;;;   %right-stall (self)             — release, bounded sleep of
;;;                                     one poll slice, re-acquire.
;;;
;;; THE ORDER. Re-acquisition is not a mutex race: *RIGHT-TICKET*
;;; names the ONE ticket allowed to run next, chosen in the call
;;; tree's order (the async-task-seq PATH — the key that already
;;; pins the fixtures' interleaves, minted by %tok-stamp-task at
;;; CREATION by the creator, which holds the right at that moment).
;;; %right-acquire blocks on *store-right*, checks the ticket, and
;;; hands the ticket on when it is not its turn. A dead/exited
;;; thread's ticket is dropped by %right-release-on-exit, which is
;;; called from %task-exit-implicit-thread — the single exit path
;;; every native factory, trap handler, abandon sweep and the
;;; teardown funnel through.
;;;
;;; THE QUANTUM IS ENFORCED AT THE ACQUISITION, NOT AT THE PARK.
;;; The M17 store-right measurement named the cost: during the
;;; peer-mutex fan-out (task.return -> async-thread-resume-all) the
;;; right was RELEASED, so the thread that resolution had just woken
;;; could take it and run — the callee's own continuation was left
;;; CONTENDING at %right-acquire, neither suspended nor parked, and
;;; the next command's thread.resume-later trapped ("cannot resume
;;; thread which is not suspended"). Two rules, both in
;;; %right-acquire, both keyed on RIGHT-TURN-P:
;;;   1. RESERVATION. %without-right releases the right only for the
;;;      fan-out; the quantum is NOT over, so the ticket stays named
;;;      for that thread and a peer that grabs the mutex waits
;;;      without renaming it (%right-wait-reserved).
;;;   2. MID-TURN TAKE-BACK. A thread whose turn is in progress takes
;;;      the right back after its own fan-out, whatever the ticket
;;;      order says.
;;; Together they restore the spec's "a guest continuation runs until
;;; it blocks": the callee's continuation after task.return reaches
;;; its own next park before the thread that resolution woke may run.
;;; Admission stays CONTENDING-ONLY: widening it to the parked/ready
;;; classes rotates the right through sleepers and HANGS (2 of 2
;;; runs, rc=124). A boundary-side "prefer a newly-ready peer" rule
;;; was implemented, measured (adjacent n=30) and REMOVED: it moved
;;; no row better and made during-sync-call-may-block ::8 and
;;; during-sync-scheduling-candidates ::19 worse — see the ledger.
;;;
;;; THE THREE MEASURED FACTS OF THE M17 LEDGER, and why they cannot
;;; recur here:
;;;  a. SBCL 2.6.4's (condition-wait q m :timeout t) returns with M
;;;     RELEASED on timeout. THE WHOLE RUNTIME NOW HAS EXACTLY ONE
;;;     condition-wait: inside %bounded-wait, which unconditionally
;;;     re-grabs unless the caller already owns the mutex. No site
;;;     can observe a wait's return without the mutex.
;;;  b. A release "for the park" is not enough when the park is a
;;;     LOOP. Here the release is not a statement at park entry: it
;;;     is %park's first act, and %park is the only way to reach the
;;;     wait — so every iteration of every loop releases (the
;;;     release and the wait are welded together in one function).
;;;  c. %task-return / %task-exit-implicit-thread call
;;;     async-thread-resume-all, whose fan-out takes OTHER threads'
;;;     mutexes. Every such call is wrapped in %without-right: the
;;;     fan-out runs with the right RELEASED, and the right is
;;;     re-acquired only after the fan-out has returned and dropped
;;;     every peer mutex. That release is a RESERVATION, never a park
;;;     (rule 1 above). The reverse order (a peer mutex held across
;;;     an acquisition of the right) cannot occur either: no mutex
;;;     section in this runtime spans a wait — every wait goes
;;;     through %park/%right-stall, both of which are called with no
;;;     mutex but the parked one, and %right-acquire is never called
;;;     from inside a lock. %without-right's THUNK must therefore not
;;;     wait: it is a fan-out of mutex sections and broadcasts.
;;;
;;; THE LOCK ORDER (depth 1 + short leaves):
;;;   *store-right*  ->  *scheduler-mutex*  ->  a thread/task/wset
;;;   mutex, never the other way, and never across a wait. The
;;;   per-thread/task/wset mutexes now protect ONLY the host-side
;;;   registration fields a PEER writes (wake-count, granted,
;;;   ready-p, spawn-thunk, the wset waiter list); every field a
;;;   parked thread's PREDICATE reads is written under the right
;;;   (task state / resolved-p / result, end state / pending-event,
;;;   subtask state / flat-results, the instance's async fields), so
;;;   a predicate needs no foreign mutex — it reads under the right.
;;;
;;; THE EXITS. %right-release-on-exit drops the ticket and, when the
;;; leaving thread owns the right, hands it to the earliest runnable
;;; candidate and releases. It NEVER waits and NEVER joins: the
;;; store teardown must not block at a script boundary (the
;;; piece-(i) measurement: a script-boundary block changed the
;;; timing the next fixture inherits and cost 5 extra acs::1 per 30
;;; runs).
;;;
;;; THE SLICES. +PARK-SLICE+ is the backstop for a park whose wake
;;; was lost or whose waiter is not broadcast-woken; parks are
;;; bridge-woken by their own queues, so the slice only bounds the
;;; damage. +POLL-SLICE+ is the cadence of the five poll loops
;;; (the oracle drains and the STARTED spin) whose predicate is
;;; tested by polling rather than by a broadcast — the same 2 ms
;;; the M15/M17 oracles already use, so their idle-cycle budgets
;;; keep their meaning.

(defparameter *store-right* (sb-thread:make-mutex :name "store-right"))

(defparameter *right-owner* nil
  "The thread record holding the right (informational: ownership is
the mutex itself; this names it for the watchdog dump).")

(defparameter *right-ticket* nil
  "The ONE ticket allowed to run next — an async-thread record whose
async-task-seq is the creation path. Written only under
*STORE-RIGHT*.")

(defparameter *right-owner-os* nil
  "The OS thread of *RIGHT-OWNER* (diagnostics only).")

(defparameter *right-self* nil
  "The thread record of THIS native thread (bound in every guest
body and every driver loop). NIL outside the store.")

(defparameter *right-driver* nil
  "The driver ticket currently polling (the runner/drain thread). It
is ALWAYS a runnable candidate so that a park hands the right back
to the tick engine, exactly as the pre-permit runtime expected.")

(defparameter *driver-records* (make-hash-table :test #'eq)
  "Per-OS-thread synthetic ticket for a driver (the runner or a
nested sync executor): an async-thread whose task's seq is (0), so
it sorts before every guest path (roots are (1), (2), ...).")

(defparameter +park-slice+ 0.05
  "The bounded condition-wait slice for the broadcast-woken park
sites (1-7): they are woken by name, so the slice is a backstop.")

(defparameter +poll-slice+ 0.002
  "The bounded slice of the five ORACLE/POLL sites (8-12): their
predicate is polled, and 2 ms is the cadence the oracles' 50/200
idle-cycle budgets were calibrated on.")

(defun %bounded-wait (queue mutex timeout)
  "THE ONLY condition-wait in this runtime. Called holding MUTEX;
returns holding MUTEX. Fact (a) of the ledger: SBCL 2.6.4 returns
from a TIMED-OUT wait with the mutex RELEASED — re-grab unless this
thread already owns it. Every park site goes through here, so no
site can observe a wait's return without the mutex."
  (sb-thread:condition-wait queue mutex :timeout timeout)
  (unless (sb-thread:holding-mutex-p mutex)
    (sb-thread:grab-mutex mutex))
  t)

(defun %ticket-path (tk)
  (let ((task (and tk (async-thread-task tk))))
    (and task (async-task-seq task))))

(defun %ticket< (a b)
  "The ticket order: the async-task-seq PATH (the call tree's order),
with a pathless record last."
  (let ((pa (%ticket-path a))
        (pb (%ticket-path b)))
    (cond ((and pa pb) (%tok-path-earlier-p pa pb))
          (pa t)
          (pb nil)
          (t nil))))

(defun %right-candidates ()
  "Every record that may hold the right, in ticket order.
READ WITHOUT *SCHEDULER-MUTEX*: the registry is a persistent list
(push / remove cons new lists), so a stale read costs at most one
wasted handoff, while TAKING the scheduler lock here would build the
own-park-mutex -> scheduler-mutex edge that completes a cycle with
resume-all's scheduler-mutex -> peer-park-mutex order (measured:
SBCL's deadlock detector fired on exactly that pair)."
  (let ((all *scheduler-threads*))
    (when (and *right-driver* (not (member *right-driver* all)))
      (setf all (cons *right-driver* all)))
    (sort (copy-list all) #'%ticket<)))

(defun %right-runnable-p (th)
  "Is TH a legitimate handoff target? Only a thread that is ACTIVE-
LY CONTENDING for the right (inside %right-acquire) is: a thread
that parks with a false predicate waits on its OWN queue and needs
the right only once its own wake arrives, so handing it the ticket
early would rotate the right through sleepers."
  (and th
       (not (async-thread-abandoned th))
       (async-thread-right-waiting th)))

(defun %right-successor (self)
  "The earliest RUNNABLE candidate other than SELF, or NIL."
  (find-if (lambda (th)
             (and (not (eq th self))
                  (%right-runnable-p th)))
           (%right-candidates)))

(defun %right-reserved-p (tk self)
  "Does the TICKET name a holder that is MID-TURN? Such a holder
released the right only so the peer-mutex fan-out could run
(%without-right); its quantum is NOT over, so the window is not a
turn: a peer that grabbed the mutex must not take it (measured: the
during-sync family's ::16/::19 — the callee lost the acquisition
race inside its own task.return's fan-out, was left contending at
%right-acquire instead of reaching thread.suspend, and the next
command's thread.resume-later trapped 'cannot resume thread which is
not suspended')."
  (and tk
       (not (eq tk self))
       (not (async-thread-abandoned tk))
       (async-thread-right-turn-p tk)))

(defun %right-take (self)
  "Name SELF the ticket holder — the ONLY take path — and mark its
quantum IN PROGRESS (RIGHT-TURN-P, the mid-turn reservation's key).
Returns T."
  (setf *right-ticket* self
        *right-owner* self
        *right-owner-os* sb-thread:*current-thread*
        (async-thread-right-hops self) 0
        (async-thread-right-turn-p self) t)
  t)

(defun %right-acquire (self)
  "Block until SELF is the ticket holder; return holding the right.
IDEMPOTENT on the owning thread: a thread that already holds the
right (a nested component call on one thread) gets it back without
a second grab and without any depth bookkeeping — the right is
either held or not, and the only release is %right-yield / the exit
handoff. SELF is a CONTENDER while it waits, which is what lets a
yielding thread name it as the next ticket.

THE THREE ACQUISITION RULES (the quantum, enforced HERE):
  1. A ticket whose holder is MID-TURN is a reservation, not a
     turn: SELF waits without renaming it.
  2. SELF's own quantum in progress (it released only for a
     %without-right fan-out) is taken back — the fan-out did not
     end the turn.
  3. At a real boundary (SELF has parked), a contending peer that
     became ready AFTER SELF took its turn is preferred over SELF's
     stale ticket; otherwise the ticket order decides."
  (when (null self)
    (return-from %right-acquire nil))
  (when (sb-thread:holding-mutex-p *store-right*)
    (return-from %right-acquire t))
  (setf (async-thread-right-waiting self) t)
  (unwind-protect
       (loop
         (sb-thread:grab-mutex *store-right*)
         (cond
          ;; (1) THE TICKET IS RESERVED: its holder is mid-turn, so
          ;; SELF waits WITHOUT renaming it (a rename would hand SELF
          ;; a turn the running quantum has not finished).
          ((%right-reserved-p *right-ticket* self)
           (%right-wait-reserved self))
          ;; (2) SELF's quantum is still in progress (it released the
          ;; right only for its own %without-right fan-out): take it
          ;; back, whatever the ticket order says. While mid-turn the
          ;; ticket IS self (the reservation), so this cannot preempt
          ;; a nominated peer.
          ((async-thread-right-turn-p self)
           (%right-take self)
           (return t))
          ;; (3) THE TICKET ORDER — SELF was named, or nobody else
          ;; can run (the creator may already have yielded before
          ;; this thread registered as a contender).
          ((or (eq *right-ticket* self)
               (null (%right-successor self)))
           (%right-take self)
           (return t))
          (t
           (%right-wait-turn self))))
    (setf (async-thread-right-waiting self) nil)))

(defun %right-wait-reserved (self)
  "Called holding the right, by SELF, while the ticket names a
MID-TURN holder. The ticket is NOT renamed (renaming it would steal
the reservation and hand SELF a turn the running quantum has not
finished) and no wake is owed: the holder re-grabs the mutex within
microseconds. SELF releases the right and takes one short bounded
sleep before retrying."
  (setf *right-owner* nil)
  (sb-thread:release-mutex *store-right*)
  (sleep 0.0002)
  nil)

(defun %right-wait-turn (self)
  "Called holding the right, by SELF which is not the ticket holder.
Hands the ticket to the earliest runnable candidate (itself when it
is the only one, so a single-runnable store behaves exactly as
before), releases the right, and waits (bounded) on SELF's OWN
queue for the named wake. Returns with neither the right nor any
mutex held; the caller loops back to %right-acquire."
  (let ((m (async-thread-mutex self))
        (q (async-thread-waitqueue self))
        (next (or (%right-successor self) self)))
    (setf *right-ticket* next
          (async-thread-right-turn-p self) nil)
    (cond
     ((not (eq next self))
      (sb-thread:release-mutex *store-right*)
      (when (< (async-thread-right-hops self) 8)
        (incf (async-thread-right-hops self))
        (%async-thread-wake next)))
     (t
      ;; nobody else can run: hand nothing on, but STILL release the
      ;; right across the wait (fact b) — a peer may become runnable
      ;; from a host thread while we sleep, and it must not need us
      ;; to release a mutex we are not using.
      (sb-thread:release-mutex *store-right*)))
    (setf *right-owner* nil)
    ;; The TURN wait is a bounded poll: its predicate (the ticket)
    ;; is re-tested under the mutex by %right-acquire's loop, so no
    ;; wake bookkeeping is needed — and the durable count is LEFT
    ;; ALONE (it belongs to the park site).
    (sb-thread:with-recursive-lock (m)
      (when (zerop (async-thread-wake-count self))
        (%bounded-wait q m +poll-slice+)))
    nil))

(defun %right-yield (self)
  "SELF holds the right and is about to wait: END its quantum (the
park boundary), name the next ticket (the earliest runnable
candidate, else SELF) and RELEASE the right. Returns NIL (the caller
re-acquires after its wait; the release is paired with the wait,
never with the park)."
  (when (or (null self) (not (sb-thread:holding-mutex-p *store-right*)))
    (return-from %right-yield nil))
  (let ((next (%right-successor self)))
    (setf *right-ticket* (or next self)
          *right-owner* nil
          (async-thread-right-turn-p self) nil)
    (sb-thread:release-mutex *store-right*)
    (when (and next (< (async-thread-right-hops self) 8))
      (incf (async-thread-right-hops self))
      (%async-thread-wake next))
    t))

(defun %park (self mutex queue &key (timeout +park-slice+))
  "SELF holds the right and MUTEX. Release the right, wait bounded on
QUEUE/MUTEX (the wait leaves MUTEX released, and fact (a)'s re-grab
is inside %bounded-wait), release MUTEX, re-acquire the right.
Returns with the right held and MUTEX not held."
  (%right-yield self)
  (%bounded-wait queue mutex timeout)
  (sb-thread:release-mutex mutex)
  (%right-acquire self))

(defun %right-stall (self &key (seconds +poll-slice+))
  "The five POLL sites' wait: release the right, sleep one bounded
slice, re-acquire. Same cadence as the M15/M17 sleep-polls (their
oracles' idle budgets are calibrated on it)."
  (%right-yield self)
  (sleep seconds)
  (%right-acquire self))

(defun %right-handoff-to (self target &key (seconds 0.0002))
  "Release the right NAMING TARGET as the next ticket — a targeted
handoff for one very short bounded slice (the caller's pack window:
the callee's FIRST TURN must happen, but its later turns must not —
the fixtures pin STARTED at the pack, before the callback's
resolution). Then sleep the slice and re-acquire."
  (when (and self target (sb-thread:holding-mutex-p *store-right*))
    (setf *right-ticket* target
          *right-owner* nil
          (async-thread-right-turn-p self) nil)
    (sb-thread:release-mutex *store-right*))
  (sleep seconds)
  (%right-acquire self))

(defun %right-reserve-self (self)
  "Release the right WITHOUT ending SELF's quantum: name SELF (the
reservation) and KEEP RIGHT-TURN-P, so a peer that grabs the mutex
in this window defers (%right-reserved-p) instead of taking a turn
the quantum in progress has not yielded. Used only by
%without-right."
  (setf *right-ticket* self
        *right-owner* nil)
  (sb-thread:release-mutex *store-right*))

(defun %without-right (self thunk)
  "Run THUNK with the right released (fact (c): a peer-mutex fan-out —
async-thread-resume-all, %subtask-resolve's waiters, the nudge spawn
fires — must never run with the right held), then re-acquire it.
STEP 4-QUANTUM: the release is MID-TURN, never a park — the guest
frame the caller is in the middle of does not end here, so the
ticket is RESERVED for SELF and no peer may take the window as a
turn of its own. THUNK must not wait (a fan-out of mutex sections
and broadcasts only): the reservation lasts exactly as long as the
fan-out. The rule this enforces is the during-sync family's: the
callee's continuation after task.return must reach its own next
block before the thread that resolution woke may run."
  (if (and self (sb-thread:holding-mutex-p *store-right*))
      (unwind-protect
           (progn (%right-reserve-self self) (funcall thunk))
        (%right-acquire self))
      (funcall thunk)))

(defun %right-drop (self)
  "Release the right if THIS thread holds it (a driver loop's exit).
Hands the ticket to the earliest contender first, so a waiting guest
is named while the driver stops polling. NO wait."
  (when (and self (sb-thread:holding-mutex-p *store-right*))
    (setf *right-ticket* (%right-successor self)
          *right-owner* nil
          (async-thread-right-turn-p self) nil)
    (sb-thread:release-mutex *store-right*))
  nil)

(defun %right-release-on-exit (self)
  "SELF is leaving the store: drop its ticket and, when it owns the
right, hand it to the earliest runnable candidate and release.
NEVER waits, NEVER joins (the piece-(i) lesson: the teardown must not
block at a script boundary)."
  (when self
    (setf (async-thread-right-turn-p self) nil))
  (when (and self
             ;; ONLY the exiting thread itself may hand the right on:
             ;; a sweep from another thread (teardown, an oracle
             ;; abandon) must not release a right it does not own —
             ;; and A3: the teardown never touches the right at all.
             (eq self *right-self*)
             (sb-thread:holding-mutex-p *store-right*))
    (setf *right-ticket* (%right-successor self)
          *right-owner* nil)
    (sb-thread:release-mutex *store-right*))
  (when (and self (eq self *right-self*))
    (sb-thread:with-recursive-lock (*scheduler-mutex*)
      (remhash self *driver-records*)))
  nil)

(defun %right-thread-death (self)
  "THE exit gate of a native thread (every factory's unwind-protect
calls this AFTER %task-exit-implicit-thread): drop the ticket from
the registry and, if THIS thread still holds the right, hand it to
the earliest contender and release. This is what makes a dead
thread's ticket unable to become the head, on EVERY exit path —
normal return, trap, abandoned park, cancellation — even when the
thread died somewhere that skipped the task-exit helper. Idempotent
and NEVER waits."
  (when self
    (sb-thread:with-recursive-lock (*scheduler-mutex*)
      (setf *scheduler-threads*
            (remove self *scheduler-threads* :test #'eq))
      (remhash self *driver-records*))
    (setf (async-thread-right-waiting self) nil)
    (setf (async-thread-right-turn-p self) nil)
    (when (sb-thread:holding-mutex-p *store-right*)
      (setf *right-ticket* (%right-successor self)
            *right-owner* nil
            *right-owner-os* nil)
      (sb-thread:release-mutex *store-right*)))
  nil)

(defun %thread-birth (th)
  "Reserve TH's ticket in the registry AT CREATION. The CREATOR calls
this while it holds the right (every native factory runs on a thread
that holds it), so the ticket — the key the handoff order is taken
from (async-task-seq, stamped by %tok-stamp-task in the same
creator) — exists before the new thread can contend. %task-enter-
implicit-thread's own pushnew is the idempotent second registration."
  (sb-thread:with-recursive-lock (*scheduler-mutex*)
    (pushnew th *scheduler-threads* :test #'eq))
  th)

(defun %driver-self ()
  "The right ticket of THIS OS thread when it is not a guest thread
(the runner and the nested-sync executors): a per-thread synthetic
record whose path is (0), i.e. before every guest root (1)."
  (or *right-self*
      (sb-thread:with-recursive-lock (*scheduler-mutex*)
        (or (gethash sb-thread:*current-thread* *driver-records*)
            (setf (gethash sb-thread:*current-thread* *driver-records*)
                  (make-async-thread
                   :task (make-async-task :seq (list 0))))))))

;;; ---------------------------------------------------------------
;;; The thread primitives (CanonicalABI.md 447-566).

(declaim (notinline async-thread-wait-until async-thread-resume
                    async-thread-resume-all async-thread-yield
                    async-thread-suspend async-thread-nudge
                    %async-thread-wake %async-thread-post-handoff
                    %async-thread-start-spawn))

(define-condition async-thread-abandoned (wasmdown:wasm-trap)
  ()
  (:documentation "The store's teardown abandoned this thread: the
park sites unwind (the thread exits its guest body) instead of
re-parking as a nudge-consuming zombie."))

(defun async-thread-check-abandoned (thread)
  "The park sites' teardown gate: signal ABANDONED when the store
has torn this thread down — the unwind aborts the guest body (the
task never resolves; the caller already trapped its oracle)."
  (when (async-thread-abandoned thread)
    (error 'async-thread-abandoned
           :kind :generic
           :message "abandoned by store teardown")))

(defun %async-thread-wake (thread)
  "M13-4 the DURABLE wake: every REAL wake of a park (an explicit
resume, a §242-273 handoff post) increments the thread's WAKE-COUNT
atomically and broadcasts BOTH waitqueues — NO far-side mutex (a
lock-order inversion otherwise: this thread's park mutex held
while taking the woken thread's, vs the tick path's scheduler
mutex -> thread mutexs — the observed SBCL DEADLOCK cycle). The
count's any-wake semantics make a lost broadcast harmless: the
count is consumed at the next park."
  (sb-ext:atomic-incf (async-thread-wake-count thread))
  (sb-thread:condition-broadcast (async-thread-waitqueue thread))
  (sb-thread:condition-broadcast (async-thread-suspend-waitqueue
                                  thread)))

(defun %async-thread-post-handoff (thread)
  "M13-4 the HANDOFF post (§242-273's handler-lock release): a
thread about to park — or exiting — hands control back to its
switcher (the POST-TO slot, claimed under THIS thread's own mutex
exactly once; the wake itself is the mutex-free %async-thread-wake)."
  (let ((r nil))
    (sb-thread:with-recursive-lock ((async-thread-mutex thread))
      (setf r (async-thread-post-to thread))
      (setf (async-thread-post-to thread) nil))
    (when r
      (%async-thread-wake r))))

(defun async-thread-wait-until (thread ready-fn)
  "Thread.wait_until (CanonicalABI.md 505): block until READY-FN
holds, re-testing on each wakeup. M13-4: the count is cleared with
the wake — a resume posted during THIS park is THIS park's, never
the next one's (a count leaking into a later park is a spurious
early return).
STEP 4 PARK SITE 1 (also the backpressure gate's and %wset-wait's
wait): the store right is released for EACH wait and re-acquired
after it — the release lives in %park, in the loop body, so no
iteration can re-wait while holding the right (ledger fact b)."
  (%block-enter thread)
  (unwind-protect
       (loop
         (async-thread-check-abandoned thread)
         (%right-acquire thread)
         (let ((m (async-thread-mutex thread))
               (ready nil))
           (sb-thread:grab-mutex m)
           (setf ready (funcall ready-fn))
           (cond
            (ready
             (sb-thread:release-mutex m)
             (setf (async-thread-wake-count thread) 0)
             (incf *async-progress*)
             (return))
            (t
             ;; m is STILL held here: %park performs the bounded
             ;; wait on it (fact (a)'s re-grab is inside
             ;; %bounded-wait), leaves it released, and returns with
             ;; the right re-acquired.
             (%park thread m (async-thread-waitqueue thread))))))
    (%block-exit thread)))

(defun async-thread-suspend (thread)
  "Thread.suspend (CanonicalABI.md 494): block with NO readiness
condition — a SUSPENDED thread is only woken by an EXPLICIT
async-thread-resume (the switch builtins' target). Store.tick's
resume-all does NOT touch the suspend waitqueue: a scheduler nudge
must not un-suspend a thread that only an explicit switch may.
M13-4: the park posts the HANDOFF (§242-273 — this park IS the
switcher's wake; the post is durable), re-checks the count after
the post (the switcher's resume can land between the post and the
wait's registration — its broadcast would be lost, its COUNT is
durable), and clears the count on the returning wake.
STEP 4 PARK SITE 2: one bounded wait per park iteration, with the
store right released across it by %park (the post-handoff runs with
the right held but takes only the SWITCHER's mutex for a two-line
critical section — right -> short peer section is the declared
order; the reverse cannot occur because no peer holds a mutex across
a right acquisition)."
  (setf (async-thread-suspended-p thread) t)
  (%block-enter thread)
  (unwind-protect
       (loop
         (async-thread-check-abandoned thread)
         (%right-acquire thread)
         (let ((m (async-thread-mutex thread))
               (woke nil))
           ;; 1. a durable count already present is THIS park's wake
           (sb-thread:grab-mutex m)
           (when (> (async-thread-wake-count thread) 0)
             (setf (async-thread-wake-count thread) 0)
             (setf woke t))
           (sb-thread:release-mutex m)
           (when woke (return))
           ;; 2. the HANDOFF post (§242-273) — a two-line peer
           ;; section, then the durable re-check (a resume landing
           ;; between the post and the wait registration)
           (%async-thread-post-handoff thread)
           (sb-thread:grab-mutex m)
           (when (> (async-thread-wake-count thread) 0)
             (setf (async-thread-wake-count thread) 0)
             (setf woke t))
           (cond
            (woke
             (sb-thread:release-mutex m)
             (return))
            (t
             (%park thread m
                    (async-thread-suspend-waitqueue thread))
             (setf (async-thread-wake-count thread) 0)
             (return)))))
    (setf (async-thread-suspended-p thread) nil)
    (%block-exit thread)))

(defun %async-thread-start-spawn (thread)
  "M13-4 the SPAWN-START half of Thread.resume (§474/§4590): the
pending spawn's thunk fires ONCE, atomically — the claim and the
fire BOTH under the thread's mutex with a :CLAIMED sentinel: a
racing nudge sees the sentinel and SKIPS (one fire, ever), and a
thread re-entering resume re-fires the :CLAIMED thunk (the resume
is the explicit grant — the sentinel is only ever set by the nudge
whose fire is deferred to the unlock, so a resume arriving in that
window re-fires rather than losing the spawn). Returns T when the
spawn was started. NO count: a fresh §4590 thread has no park to
wake, and a count left on it is consumed by its FIRST park as a
spurious early return (X's round-1 double-suspend)."
  (let ((started nil))
    (sb-thread:with-recursive-lock ((async-thread-mutex thread))
      (let ((th (async-thread-spawn-thunk thread)))
        (cond
         ((functionp th)
          (setf (async-thread-spawn-thunk thread) nil)
          (setf started t)
          (funcall th))
         ;; RECOVER: a nudge claimed the thunk but its deferred
         ;; fire has not run yet (or was lost) — the explicit
         ;; resume re-fires it now; the sentinel's :done-clear
         ;; happens in the thunk's own body
         ((eq th :claimed)
          (setf (async-thread-spawn-thunk thread) nil)
          (setf (async-thread-ready-p thread) nil)
          (setf started t))))
      started)))

(defun async-thread-nudge (thread)
  "Store.tick's per-thread nudge (§932: the candidates are the
READY threads; a nudge = one quantum): wake the WAITCONDITION
parkers (wait-until loops re-test; yield-parks return), GRANT a
suspend-parked thread marked READY by resume-later (§458-465:
start_waiting(ready=True) makes the thread a tick candidate —
the grant posts a REAL count so ANY of its parks honors it), and
CLAIM a ready-marked PENDING SPAWN's thunk — the :CLAIMED sentinel
with the fire deferred past the unlock (holding the guest's mutex
across the thunk's SBCL-thread start re-enters on the new thread).
A spawn that is NOT ready-marked is NOT started by a bare tick
(only the explicit switch/resume may — the observed
no-sibling-resume stall: a nudge fired X's spawn during setup,
BEFORE inner-block set $implicit-thread-index; X's first
resume-later read 0xdead and no-oped). An ABANDONED thread's spawn
is never fired (the cross-file zombie leak). The nudge is
TRANSIENT except the ready-grant (a real wake)."
  (let ((thunk nil) (ready-grant nil))
    (sb-thread:with-recursive-lock ((async-thread-mutex thread))
      (when (and (not (async-thread-abandoned thread))
                 (async-thread-ready-p thread)
                 (functionp (async-thread-spawn-thunk thread)))
        (setf thunk (async-thread-spawn-thunk thread))
        (setf (async-thread-spawn-thunk thread) :claimed)
        (setf (async-thread-ready-p thread) nil))
      (when (and (async-thread-ready-p thread)
                 (async-thread-suspended-p thread))
        (setf (async-thread-ready-p thread) nil)
        (setf ready-grant t))
      (sb-thread:condition-broadcast (async-thread-waitqueue thread)))
    ;; the deferred fires — OUTSIDE the thread's mutex
    (when (functionp thunk)
      (funcall thunk))
    (when ready-grant
      (%async-thread-wake thread))))

(defun async-thread-resume (thread &optional (post-to nil post-to-p))
  "Thread.resume (CanonicalABI.md 474) — the EXPLICIT switch (the
  -then-{resume,promote} builtins' target, the cancellation resume):
  starts a PENDING SPAWN (§4590's first resume — the atomic
  claim-then-fire), wakes every park (§494's only waker) via the
  DURABLE wake-count, and — when POST-TO is given (the switch
  arms' §242-273 handoff install) — sets the target's HANDOFF slot
  under the SAME locked section (atomic against the target's own
  post-handoff claim; the caller's park mutex held across the call
  creates no cycle: the far-side wake is mutex-free). The COUNT is
  posted ONLY on the park-wake half (a spawn-START's fresh thread
  has no park to wake)."
  (let ((started-p (%async-thread-start-spawn thread)))
    (sb-thread:with-recursive-lock ((async-thread-mutex thread))
      (when post-to-p
        (setf (async-thread-post-to thread) post-to))
      ;; M14 THE TURN GRANT: the explicit resume (the switch arms, the
      ;; cancellation's synchronous resume) IS a grant — the resumed
      ;; callee's continuation takes its turn (the TASK_CANCELLED
      ;; delivery); the incidental broadcasts do not set this
      (setf (async-thread-granted thread) t)
      (unless started-p
        (incf (async-thread-wake-count thread))
        (sb-thread:condition-broadcast
         (async-thread-suspend-waitqueue thread)))
      (sb-thread:condition-broadcast (async-thread-waitqueue thread)))))

(defparameter *tok-skip-age* (make-hash-table :test #'eq)
  "M13-tok9 THE AGING COUNTER: per-thread count of consecutive
grants it was eligible for but did NOT receive. The path order
(the pinned interleave) holds while every grantable thread's age
is under the threshold; the moment some thread's age crosses it,
THAT thread gets the grant (its age resets). The escape fires only
under a genuine starvation — the candidates' setup thread (parked
BY DESIGN, resolved, doing live callback work for later rows)
accumulates age while the active callee runs and gets its turn at
the threshold; the strict-pin fixtures (whose threads all cycle)
never accumulate age and keep the deterministic creation order.")

(defparameter *tok-age-threshold* 40
  "M13-tok9: the age at which a skipped thread earns the grant.
MUST stay under the deadlock oracle's idle budget (50 cycles in
%canon-lift-async's drain; 200 in %sync-lift-run's): the escape
fires BEFORE the oracle, else the starved thread dies at the
oracle first (the candidates' row 11 measured exactly this race:
the callee's age crossed the oracle's 50 before the 64 escape).")

(defun async-thread-resume-all ()
  "STEP 4 (ledger fact c): async-thread-resume-all's fan-out takes
OTHER threads' mutexes (the nudge's claim section, the armed-park
wake). It therefore runs with the STORE RIGHT RELEASED — %without-
right hands the right on and re-acquires it only after the fan-out
has returned and dropped every peer mutex, so the AB-BA of
%task-return / %task-exit-implicit-thread -> resume-all cannot
exist. The quantum itself is %resume-all-body, unchanged."
  (%without-right *right-self*
                  (lambda () (%resume-all-body))))

(defun %resume-all-body ()
  "M13-tok9 THE QUANTUM (CanonicalABI.md 922-941): ONE grant per
tick — the reference tick allows a SINGLE THREAD one quantum,
granted in the CREATION-ORDER path sort (the pinned interleave
the sequential-schedule fixtures require). THE AGING ESCAPE: a
thread skipped for *tok-age-threshold* consecutive grants takes
the next one regardless of order — the starvation case (a
permanently-parked-by-design thread ahead of an active callee
starved it into the deadlock oracle) without the nondeterministic
reordering of the parity policy (which flaked the strict-pin
fixtures into an 11-13 fail band). THE PRIORITY PASS first
(ready-marked suspend-parked/spawn-pending). THE GRANT SET: never
the caller's own thread; never a plain suspend-parked thread
(§494). THE NO-QUANTUM FALLBACK: broadcast-only."
  (sb-thread:with-recursive-lock (*scheduler-mutex*)
    (let* ((sorted
             (sort (copy-list *scheduler-threads*)
                   (lambda (a b)
                     (let ((pa (and (async-thread-task a)
                                    (async-task-seq
                                     (async-thread-task a))))
                           (pb (and (async-thread-task b)
                                    (async-task-seq
                                     (async-thread-task b)))))
                       (cond
                        ((and pa pb) (%tok-path-earlier-p pa pb))
                        (pa t)
                        (t nil))))))
           (grantable-p
             (lambda (th)
               (and (not (async-thread-abandoned th))
                    (not (eq th *current-thread*))
                    (or (async-thread-yield-parked th)
                        (and (async-thread-task th)
                             (async-task-gate-parked
                              (async-thread-task th)))))))
           (eligible (remove-if-not grantable-p sorted)))
      (let ((granted-thread nil))
        ;; THE PRIORITY PASS: ready-marked grantable threads
        ;; (M14: *suppress-spawn-fire* — the STARTED-wait's window
        ;; must not FIRE ready-marked pending spawns of the callee's
        ;; own instance: cancel-targeted-resume's keeper/worker is
        ;; resume-later'd INSIDE the callee's core, and the exit's
        ;; M11a resume-all (running in the wait's dynamic extent)
        ;; fired it via this pass — the keeper's task.return
        ;; resolved the subtask before the caller's pack could
        ;; observe STARTED / the cancel return BLOCKED. The
        ;; suppression is DYNAMICALLY BOUND (the pack's window
        ;; only); the suspend-parked ready-grant is unaffected.)
        (loop for th in sorted
              do (when (and (not (async-thread-abandoned th))
                            (not (eq th *current-thread*))
                            (async-thread-ready-p th)
                            ;; M15 MECHANISM 4 — THE CANCEL-WINDOW
                            ;; FIRE GATE (the grant gate's per-instance
                            ;; shape, extended to the FIRE): while the
                            ;; synchronous-resume wait is open the
                            ;; drain's ticks must not FIRE the
                            ;; cancelled instance's ready-marked
                            ;; pending spawns (cancel-targeted-resume
                            ;; ::9: the callback's resume-later'd
                            ;; spawn ran on the HOST drain's tick
                            ;; before subtask.cancel returned).
                            (not (and *cancel-window-inst*
                                      (async-thread-task th)
                                      (eq (async-task-inst
                                           (async-thread-task th))
                                          *cancel-window-inst*)))
                            (or (async-thread-suspended-p th)
                                (and (not *suppress-spawn-fire*)
                                     (functionp
                                      (async-thread-spawn-thunk th)))))
                   (async-thread-nudge th)
                   (setf granted-thread th)
                   (return)))
        (unless granted-thread
          ;; THE AGING CHECK: the oldest skipped thread past the
          ;; threshold takes the grant (its age resets)
          (let ((starved
                  (find-if
                   (lambda (th)
                     (>= (or (gethash th *tok-skip-age*) 0)
                         *tok-age-threshold*))
                   eligible)))
            (cond
             (starved
              (async-thread-nudge starved)
              (setf granted-thread starved))
             (t
              ;; THE MAIN PASS: the path-ordered grant
              (loop for th in eligible
                    do (progn (async-thread-nudge th)
                              (setf granted-thread th)
                              (return))))))
        ;; THE AGE BOOKKEEPING: every eligible thread that did not
        ;; get this grant ages; the granted one resets
        (dolist (th eligible)
          (if (eq th granted-thread)
              (setf (gethash th *tok-skip-age*) 0)
              (incf (gethash th *tok-skip-age* 0))))
        ;; GC the ages of threads no longer registered
        (when (> (hash-table-count *tok-skip-age*) (* 4 (length sorted)))
          (let ((live (copy-list sorted)))
            (maphash (lambda (th _)
                       (unless (member th live)
                         (remhash th *tok-skip-age*)))
                     *tok-skip-age*)))
        ;; M14 THE TURN GRANT: this resume-all IS a real Store.tick
        ;; (§932 — the drain loops', the blocking-op-driven, the
        ;; switch arms' one-tick) for every thread it touches: mark
        ;; ALL of them granted (the callback loop's post-first-turn
        ;; gate re-tests on any broadcast; the flag answers WHICH
        ;; broadcast was a tick — the deferred/incidental
        ;; propagations never reach here in the cancel-wait window)
        (dolist (th sorted)
          (unless (or (async-thread-abandoned th)
                      ;; M14 THE PER-INSTANCE CANCEL-WINDOW GATE:
                      ;; during the synchronous-resume wait, the
                      ;; cancelled task's INSTANCE's threads keep
                      ;; holding (the ::7/::9 worker — the drain's
                      ;; tick granted it inside the window; the
                      ;; fixtures pin the caller's own observation
                      ;; first); the REST of the store keeps its
                      ;; grants (the during-sync interleave's
                      ;; pre-window ticks — the whole-store
                      ;; suppression measured as the acs::2 +
                      ;; ctr::5 regressions)
                      (and *cancel-window-inst*
                           (async-thread-task th)
                           (eq (async-task-inst
                                (async-thread-task th))
                               *cancel-window-inst*)))
            ;; M15: a grant must WAKE the PARKER it touches — a
            ;; grant whose broadcast never arrives leaves the parker
            ;; in its condition-wait until some later tick's
            ;; fallback. ONLY an ARMED park is woken (blocked-p + a
            ;; ready-fn): a mid-turn thread gets its grant consumed
            ;; at its next park, exactly as before.
            (setf (async-thread-granted th) t)
            (when (and (%thread-blocked-p th)
                       (async-thread-ready-fn th))
              (%async-thread-wake th))))
        (when granted-thread
          (%async-thread-wake granted-thread))
        (unless granted-thread
          ;; THE NO-QUANTUM FALLBACK: broadcast-only — M14: skip
          ;; the ready-marked PENDING SPAWNS under the pack window's
          ;; suppression (the fallback nudges every registered
          ;; thread and async-thread-nudge FIRES a claimed spawn —
          ;; cancel-targeted-resume's keeper ran through here in
          ;; the trace; the suppressed candidates stay parked and
          ;; the next tick after the window takes them)
          (dolist (th sorted)
            (unless (or (and *suppress-spawn-fire*
                             (async-thread-ready-p th)
                             (functionp (async-thread-spawn-thunk th)))
                        ;; M15 MECHANISM 4 (the fallback's share of
                        ;; the cancel-window fire gate): a nudge
                        ;; FIRES a ready-marked pending spawn
                        (and *cancel-window-inst*
                             (async-thread-task th)
                             (async-thread-ready-p th)
                             (functionp (async-thread-spawn-thunk th))
                             (eq (async-task-inst
                                  (async-thread-task th))
                                 *cancel-window-inst*)))
              (async-thread-nudge th)))))))))

(defun async-thread-yield (thread)
  "Thread.yield_ (CanonicalABI.md 523): cooperatively give up the
CPU. wait-until with an always-true predicate returns IMMEDIATELY
(§507's host freedom) which hot-spins a yielding guest and starves
siblings; instead PARK on the thread's waitqueue for a single
scheduler nudge (Store.tick / resume-all) so other threads run. The
YIELD-PARKED flag marks the thread READY while parked (a promote
target's resumability condition, §549). M13-4: NO HANDOFF POST —
a yield does NOT hand control away (§523: the yielder expects to
CONTINUE; its post woke a switcher mid-round, suspend-vs-yield's
(0)). A pending durable wake is honored (a resume posted before
the park registered), and the count is cleared with the wake — a
count arriving with the wake is THIS park's, never the next's."
  (async-thread-check-abandoned thread)
  (%right-acquire thread)
  (let ((m (async-thread-mutex thread)))
    (sb-thread:grab-mutex m)
    (setf (async-thread-yield-parked thread) t)
    (%block-enter thread)
    ;; M14 the PARK READINESS (§523: a yield is ALWAYS ready — the
    ;; park's ready-fn, per the spec's start_waiting(ready_func))
    (setf (async-thread-ready-fn thread) (lambda () t))
    (cond
     ((> (async-thread-wake-count thread) 0)
      (setf (async-thread-wake-count thread) 0)
      (sb-thread:release-mutex m))
     (t
      ;; STEP 4 PARK SITE 3: one bounded wait, the right released
      ;; across it by %park (fact b: welded to THIS wait).
      (%park thread m (async-thread-waitqueue thread))))
    (sb-thread:grab-mutex m)
    (setf (async-thread-wake-count thread) 0
          (async-thread-ready-fn thread) nil
          (async-thread-yield-parked thread) nil)
    (sb-thread:release-mutex m)
    (%block-exit thread)))

;;; ---- the ComponentInstance concurrency fields (§133-150) ----

(defun %inst-async-fields (inst)
  "backpressure / num_waiting_to_enter / exclusive_thread / threads,
  lazily initialized under the imports table's reserved :async key."
  (or (gethash :async (comp-instance-imports inst))
      (let ((fields (list :backpressure 0
                          :num-waiting-to-enter 0
                          :exclusive-thread nil
                          :threads nil)))
        (setf (gethash :async (comp-instance-imports inst))
              fields)
        fields)))

(defun %inst-backpressure (inst)
  (getf (%inst-async-fields inst) :backpressure))

(defun (setf %inst-backpressure) (v inst)
  (let ((fields (%inst-async-fields inst)))
    (setf (getf fields :backpressure) v)))

(defun %inst-exclusive-thread (inst)
  (getf (%inst-async-fields inst) :exclusive-thread))

(defun (setf %inst-exclusive-thread) (v inst)
  (let ((fields (%inst-async-fields inst)))
    (setf (getf fields :exclusive-thread) v)))

;;; ---------------------------------------------------------------
;;; Task lifecycle (CanonicalABI.md 657-823).

(declaim (notinline %task-needs-exclusive %task-enter-implicit-thread
                    %task-exit-implicit-thread %task-return
                    %task-cancel %task-request-cancellation
                    %task-deliver-pending-cancel))

(defun %task-ft-async-p (task)
  "The functype's async flag (the payload's third slot)."
  (and (async-task-ft task)
       (third (wasmdown.component:ct-payload (async-task-ft task)))))

(defun %task-opts-async-p (task)
  (and (wasmdown.component:canon-opt (async-task-opts task) :async) t))

(defun %task-opts-callback-p (task)
  (and (wasmdown.component:canon-opt (async-task-opts task) :callback)
       t))

(defun %task-needs-exclusive (task)
  "Task.needs_exclusive (CanonicalABI.md 652): true unless the lift
  opted into stackful async (the `async` opt WITHOUT `callback`)."
  (or (not (%task-opts-async-p task))
      (%task-opts-callback-p task)))

(defun %task-enter-implicit-thread (task)
  "Task.enter_implicit_thread (CanonicalABI.md 681): the BACKPRESSURE
gate — wait until backpressure subsides, then register the thread.
Returns NIL when a pending cancellation aborted the start. The
instance's async FIELDS plist is pulled ONCE into a local: the
getf-setf then mutates the shared plist object through the local
binding (an incf on (getf (%inst-async-fields inst) ...) would
expand into a (setf (%inst-async-fields inst)) — a function that
does not exist)."
  (let* ((th (async-task-implicit-thread task))
         (inst (async-task-inst task))
         (fields (%inst-async-fields inst)))
    (when (%task-ft-async-p task)
      (let ((has-backpressure
              (lambda ()
                (or (> (getf fields :backpressure) 0)
                    (and (%task-needs-exclusive task)
                         (getf fields :exclusive-thread))))))
        (when (or (funcall has-backpressure)
                  (> (getf fields :num-waiting-to-enter) 0))
          ;; the num_waiting_to_enter counter is the RESIDUAL
          ;; backpressure rule (§669-672): a later waiter sees an
          ;; earlier one waiting and waits too (no thundering herd)
          (incf (getf fields :num-waiting-to-enter))
          ;; the thread must be NUDGABLE while parked: register it
          ;; in the scheduler BEFORE parking (the gate's readiness
          ;; flips on another thread's release — resume-all must
          ;; see this thread to deliver the nudge)
          (setf (async-task-gate-parked task) t)
          (sb-thread:with-recursive-lock (*scheduler-mutex*)
            (pushnew th *scheduler-threads* :test #'eq))
          ;; M15 GATE-2: a task CANCELLED while waiting at the gate
          ;; must WAKE and deliver CANCELLED_BEFORE_STARTED
          ;; (CanonicalABI.md §681 + §761's deliver_pending_cancel) —
          ;; the backpressure itself may never subside (big-
          ;; interleaving-test's mock-bp-inc row: bp-inc, then
          ;; call-block-empty at the gate, then subtask.cancel,
          ;; pinned CANCELLED_BEFORE_STARTED(3) WITHOUT a bp-dec).
          ;; The readiness therefore ALSO holds on a pending cancel.
          (async-thread-wait-until
           th (lambda ()
                (or (not (funcall has-backpressure))
                    (%task-has-pending-cancel task))))
          (setf (async-task-gate-parked task) nil)
          (decf (getf fields :num-waiting-to-enter))
          (when (%task-deliver-pending-cancel task)
            ;; cancelled while waiting: resolve without running
            (%task-cancel task)
            (return-from %task-enter-implicit-thread nil)))
        (when (%task-needs-exclusive task)
          (setf (getf fields :exclusive-thread) th))))
    ;; §705: register_thread assigns the instance's next index
    (unless (async-thread-index th)
      (setf (async-thread-index th)
            ;; M13-tok6 GATE 2: the LIVE-ONLY idx assignment — a
            ;; stale :done record (a raced unregister window) must
            ;; not inflate the max; the assignment skips dead marks
            (1+ (loop for t2 in (getf fields :threads)
                      when (not (eq (async-thread-spawn-thunk t2)
                                    :done))
                        maximize (or (async-thread-index t2)
                                     0)))))
    (setf (getf fields :threads)
          (cons th (getf fields :threads)))
    (sb-thread:with-recursive-lock (*scheduler-mutex*)
      (push th *scheduler-threads*))
    t))


(defun %task-exit-implicit-thread (task)
  "Task.exit_implicit_thread (CanonicalABI.md 730): release the
exclusive lock and unregister the thread (the FIELDS local, like
the enter gate: getf-setf through the shared plist).
STEP 4: this is THE exit path — the ticket is dropped here (the
registry removal) and the right is handed to the earliest runnable
candidate if the leaving thread owned it. Every native factory, the
trap handlers, the oracle abandon sweeps and the teardown funnel
through this function, so no dead ticket can ever be the head."
  (let* ((th (async-task-implicit-thread task))
         (inst (async-task-inst task))
         (fields (%inst-async-fields inst)))
    ;; the leaving record is marked :done — the lookups' dead-record
    ;; filter: a removed-but-still-visible record (a stale list
    ;; reference from a concurrent registration) must never resolve
    ;; as a target (the ::34 self-resume was a prior row's caller
    ;; record at a colliding index passing the filter)
    (setf (async-thread-spawn-thunk th) :done)
    (setf (getf fields :threads)
          (remove th (getf fields :threads)))
    (sb-thread:with-recursive-lock (*scheduler-mutex*)
      (setf *scheduler-threads*
            (remove th *scheduler-threads* :test #'eq)))
    (when (and (%task-ft-async-p task)
               (%task-needs-exclusive task))
      (setf (getf fields :exclusive-thread) nil)
      ;; releasing the exclusive lock may admit a gate-parked peer
      ;; of this instance (its gate readiness just became true).
      ;; M14: DEFER inside the cancel-wait window (the FOURTH M11a
      ;; site running on the callee's thread — the resumed callee's
      ;; EXIT follows its task.cancel; the propagation's nudge
      ;; would grant the callee's OTHER ready threads (the ::7
      ;; worker) in the caller's observation window)
      (if *defer-resume-all*
          (push task *deferred-resume-all-tasks*)
          (async-thread-resume-all)))
    ;; STEP 4: drop the ticket / hand the right on. NEVER waits.
    (%right-release-on-exit th)))

(defun %task-has-pending-cancel (task)
  (eq (async-task-state task) :pending-cancel))

(defun %task-deliver-pending-cancel (task)
  "Task.deliver_pending_cancel (CanonicalABI.md 775)."
  (when (%task-has-pending-cancel task)
    (setf (async-task-state task) :cancel-delivered)
    t))

(defun %task-grant-callee (task)
  "M14 THE CALLEE-ONLY GRANT: nudge ONE thread — the callee task's
own implicit thread — leaving every other registered thread parked
(the STARTED-wait's driver; the full resume-all would fire the
callee instance's OTHER ready threads — the cancel-targeted-
resume keeper/worker class — resolving the subtask inside the
wait's window where the fixtures pin the caller's own observation
first). The callee's thread is the only grant the STARTING->
STARTED flip needs (§3530's synchronous resume shape)."
  (let ((th (and task (async-task-implicit-thread task))))
    (when (and th
               (not (async-thread-abandoned th))
               (not (eq (async-thread-spawn-thunk th) :done)))
      (async-thread-nudge th))))

(defun %task-bump-turn (task)
  "M14 the callback event loop's TURN COUNTER bump: one per loop-top
entry, broadcast under the task's mutex so the synchronous-resume
waiter in %task-request-cancellation re-tests (the spec §756-759:
`Thread.resume` returns as soon as the callback function returns or
blocks — the turn boundary is the observable)."
  (sb-thread:with-recursive-lock ((async-task-mutex task))
    (incf (async-task-turns task))
    (sb-thread:condition-broadcast (async-task-waitqueue task))))

(defun %task-request-cancellation (task)
  "Task.request_cancellation (CanonicalABI.md 761): the OnCancel the
caller's subtask.cancel invokes. INITIAL (still at the backpressure
gate): mark PENDING and nudge the gate — the gate's re-test
performs Task.cancel, so wait for the resolution (the spec asserts
RESOLVED on return). STARTED: mark PENDING; the SPEC's §769 gate
(`implicit_thread.cancellable and implicit_thread.ready()`) then
resumes the callee SYNCHRONOUSLY (§474-484: Thread.resume returns
when the resumed continuation returns or blocks) — the canceller
waits for the callee's TURN BOUNDARY: the task resolved (the
eager-cancel shape: the callback receives TASK_CANCELLED and calls
task.{cancel,return} — cancel-subtask ::1, reentrance ::18,
cancel-targeted-resume ::5's pins) OR the turn counter moved (the
callback returned/blocked again WITHOUT resolving — the BLOCKED
shape). The readiness gate is the M12 sync-delivery negative
result's resolution: a NOT-ready callee (the exclusive held —
cancel-targeted-resume ::7's hold-lock; a stackful blocked task —
::2's stuck, cancellable=NIL; a gone implicit thread — ::1's
exiter, ready-fn=NIL) is NOT resumed-and-waited (the wait would
self-deadlock on a callee that cannot take its turn); it keeps the
legacy nudge and the caller observes the unresolved state."
  (cond
   ((eq (async-task-state task) :initial)
    (setf (async-task-state task) :pending-cancel)
    (when (async-task-implicit-thread task)
      (async-thread-resume (async-task-implicit-thread task)))
    ;; STEP 4 PARK SITE 7 — the ledger's UNBOUNDED condition-wait:
    ;; now the shared bounded park. The right is released across the
    ;; wait and re-acquired after it; the wait's own queue is the
    ;; TASK's (the resolution broadcasts it), and its slice is the
    ;; poll slice because a resolution cannot always shoot the
    ;; count.
    (let ((self *current-thread*))
      (loop
        (%right-acquire self)
        (let ((m (async-task-mutex task))
              (done nil))
          (sb-thread:grab-mutex m)
          (setf done (async-task-resolved-p task))
          (cond
           (done
            (sb-thread:release-mutex m)
            (return))
           (t
            (if self
                (%park self m (async-task-waitqueue task)
                       :timeout +poll-slice+)
                (progn
                  (%bounded-wait (async-task-waitqueue task) m
                                 +poll-slice+)
                  (sb-thread:release-mutex m)))))))))
   (t
    (setf (async-task-state task) :pending-cancel)
    (let* ((th (async-task-implicit-thread task))
           (ready-th
             (and th
                  (not (eq (async-thread-spawn-thunk th) :done))
                  (async-thread-cancellable th)
                  (async-thread-ready-fn th)
                  (funcall (async-thread-ready-fn th)))))
      (cond
       (ready-th
        ;; the SPEC's synchronous resume (§756-759: `Thread.resume`
        ;; returns as soon as the callback function RETURNS OR
        ;; BLOCKS): the callee IS parked at a cancellable turn whose
        ;; readiness holds (the pending-cancel mark just made the wset
        ;; park's readiness true / the yield park is always ready).
        ;; Sample the turn count BEFORE the resume so any subsequent
        ;; loop-top entry is > sampled (the RETURNS case); the BLOCKS
        ;; case is the mid-turn re-park — the switch arms' park sets
        ;; suspended-p/yield-parked (cancel-resumed-callback-switch:
        ;; the resumed callback suspends-then-resumes an explicit
        ;; thread that blocks; the canceller must observe the BLOCK,
        ;; not wait for a turn the parked callback can never take —
        ;; the measured deadlock regression of the turn-only wait).
        ;; The observation is the STARTED-wait's own spin shape
        ;; (§3707's loop): a condition-wait here would need the
        ;; callee's re-park to broadcast the TASK's queue — the park
        ;; sites broadcast the THREAD's queues, so SPIN + sleep and
        ;; re-test all three observables (resolved / turn moved /
        ;; re-parked); the resolved case is the common one (the
        ;; resumed turn delivers TASK_CANCELLED and the callback
        ;; resolves in-bounds), the two-block cases take one spin
        ;; slice each.
        (let ((sampled (async-task-turns task)))
          (async-thread-resume th)
          ;; M14 THE PER-INSTANCE CANCEL-WINDOW: record the cancelled
          ;; task's instance — the drain's ticks must not grant ITS
          ;; parked callbacks while the window is open
          (setf *cancel-window-inst* (async-task-inst task))
          ;; M14 THE CANCEL-WAIT WINDOW: defer the M11a propagation
          ;; (the callee's task.{return,cancel} runs on ITS thread
          ;; inside this wait; the propagation's resume-all would
          ;; grant OTHER ready threads — ::7's worker — their turn
          ;; in the caller's observation window). Drain on exit.
          (setf *defer-resume-all* t)
          (unwind-protect
               (let ((self *current-thread*))
                 (loop
                   ;; STEP 4 PARK SITE 8: the spin's slice is a
                   ;; %right-stall — the right is released across
                   ;; every poll so the resumed callee's turn can
                   ;; actually run (this loop OBSERVES that turn).
                   (%right-acquire self)
                   (when (or (async-task-resolved-p task)
                             (> (async-task-turns task) sampled)
                             (async-thread-suspended-p th)
                             (async-thread-yield-parked th))
                     (return))
                   ;; the RE-PARK only: the callee's own park flags
                   ;; are set at the arm's entry — the resume consumed
                   ;; the wait; the flags at THIS point are the turn's
                   ;; NEW park (the initial park's flags were cleared
                   ;; at its wake)
                   (%right-stall self :seconds 0.001)))
            (setf *defer-resume-all* nil
                  *cancel-window-inst* nil)
            ;; M14: NO DRAIN — the deferred propagations' RESOLVES
            ;; are already broadcast on the task/waitable waitqueues
            ;; (the wset waiter's durable wake posted by
            ;; %subtask-resolve); the resume-all is only the M11a
            ;; nudge, and the fixtures pin the ready threads' turn at
            ;; the CALLER's next blocking op (the caller's own park/
            ;; drain loop's resume-all delivers it — ::7's worker
            ;; runs at the row's final wset.wait, ::9's at the yield
            ;; loop). Draining HERE measured the worker running at
            ;; the caller's NEXT POLL (the row's NONE check).
            (setf *deferred-resume-all-tasks* nil))))
       (th
        ;; the legacy nudge for the not-ready shapes (a mid-turn
        ;; callee, a stackful block, a suspend park): harmless when
        ;; the park re-tests and stays; the caller observes the
        ;; unresolved state (BLOCKED) per the spec's own else-arm
        (async-thread-resume th)))))))

(defun %task-return (task result)
  "Task.return_ (CanonicalABI.md 802): resolve with a value."
  (when (eq (async-task-state task) :resolved)
    (error 'wasmdown:wasm-trap :kind :generic
           :message "task already returned"))
  (unless (zerop (async-task-num-borrows task))
    (error 'wasmdown:wasm-trap :kind :generic
           :message "cannot return with outstanding borrows"))
  (funcall (async-task-on-resolve task) result)
  (setf (async-task-state task) :resolved)
  (sb-thread:with-recursive-lock ((async-task-mutex task))
    (setf (async-task-resolved-p task) t)
    (setf (async-task-result task) result)
    (sb-thread:condition-broadcast (async-task-waitqueue task)))
  ;; M11a wake propagation: a resolution may unblock SIBLING tasks
  ;; (a parked waiter's readiness condition reads this task's
  ;; state, a subtask's pending-event closure runs at delivery) —
  ;; nudge every live thread so they re-test (Store.tick, §932).
  ;; M14: DEFER inside the cancel-wait window (the synchronous-
  ;; resume wait's callee runs task.return on ITS thread; the
  ;; propagation's broadcast would grant OTHER ready threads in
  ;; the wait's window — the fixture pins the caller's own
  ;; observation first)
  (if *defer-resume-all*
      (push task *deferred-resume-all-tasks*)
      (async-thread-resume-all)))

(defun %task-cancel (task)
  "Task.cancel (CanonicalABI.md 818): resolve with cancellation (only
  after DELIVERY)."
  (unless (eq (async-task-state task) :cancel-delivered)
    (error 'wasmdown:wasm-trap :kind :generic
           :message "cannot cancel before delivery"))
  (unless (zerop (async-task-num-borrows task))
    (error 'wasmdown:wasm-trap :kind :generic
           :message "cannot cancel with outstanding borrows"))
  (funcall (async-task-on-resolve task) :cancelled)
  (setf (async-task-state task) :resolved)
  (sb-thread:with-recursive-lock ((async-task-mutex task))
    (setf (async-task-resolved-p task) t)
    (sb-thread:condition-broadcast (async-task-waitqueue task)))
  ;; M11a wake propagation (same as %task-return) — M14: DEFER
  ;; inside the cancel-wait window (see %task-return)
  (if *defer-resume-all*
      (push task *deferred-resume-all-tasks*)
      (async-thread-resume-all)))

;;; ---------------------------------------------------------------
;;; Waitable / WaitableSet operations (CanonicalABI.md 1282-1394).

(declaim (notinline %waitable-join %wset-has-pending-event
                    %wset-get-pending-event %wset-wait
                    %wset-poll %wset-drop %wset-wait-from-callback))

(defun %waitable-in-wset (w) (and (async-waitable-wset w) t))

(defun %waitable-join (w wset)
  "Waitable.join (CanonicalABI.md 1302): move W into WSET (or out,
  when WSET is NIL)."
  (when (async-waitable-has-sync-waiter w)
    (error 'wasmdown:wasm-trap :kind :generic
           :message "waitable cannot be used synchronously while added to a waitable set"))
  (when (async-waitable-wset w)
    (setf (async-waitable-set-elems (async-waitable-wset w))
          (remove w (async-waitable-set-elems
                     (async-waitable-wset w))
                  :test #'eq)))
  (setf (async-waitable-wset w) wset)
  (when wset
    (sb-thread:with-recursive-lock ((async-waitable-set-mutex wset))
      (push w (async-waitable-set-elems wset))
      ;; an arriving waitable may already hold a pending event: wake
      ;; the set's waiters
      (sb-thread:condition-broadcast (async-waitable-set-waitqueue wset)
                                 ))))

(defun %waitable-get-pending-event (w)
  "Waitable.get_pending_event (CanonicalABI.md 1297): run and clear."
  (let ((ev (async-waitable-pending-event w)))
    (setf (async-waitable-pending-event w) nil)
    (funcall ev)))

(defun %waitable-event-ready-p (w)
  "A waitable's event is READY when its closure exists AND — for a
SUBTASK — the resolve has actually happened (§1282-1294: the event
exists only in the resolved state; the pack-time closure merely
computes the tuple at delivery time)."
  (and (async-waitable-pending-event w)
       (or (not (async-subtask-p w))
           (%subtask-resolved-p w))))

(defun %wset-has-pending-event (wset)
  "WaitableSet.has_pending_event (CanonicalABI.md 1335)."
  (some #'%waitable-event-ready-p
        (async-waitable-set-elems wset)))

(defun %wset-get-pending-event (wset)
  "WaitableSet.get_pending_event (CanonicalABI.md 1338): deliver one
  element's pending event (arrival order — the spec's shuffle is
  the host's freedom)."
  (loop for w in (reverse (async-waitable-set-elems wset))
        when (%waitable-event-ready-p w)
          do (return-from %wset-get-pending-event
               (%waitable-get-pending-event w))))

(defun %wset-wait (wset)
  "WaitableSet.wait (CanonicalABI.md 1352): block until an element
  has a pending event, then deliver it. The num_waiting count guards
  WaitableSet.drop. §3522-3543: a SYNCHRONOUS task (no implicit
  thread — e.g. a sync-lifted start function) cannot block: no
  other thread could ever wake it — trap instead of hanging."
  (unless *current-thread*
    (error 'wasmdown:wasm-trap :kind :generic
           :message "cannot block a synchronous task before returning"))
  (incf (async-waitable-set-num-waiting wset))
  ;; M13-tok2 GATE 1: the parked-waiter registration (the durable
  ;; wake target)
  (sb-thread:with-recursive-lock ((async-waitable-set-mutex wset))
    (pushnew *current-thread* (async-waitable-set-waiters wset)
             :test #'eq))
  (unwind-protect
       (progn
         (async-thread-wait-until
          *current-thread*
          (lambda () (%wset-has-pending-event wset)))
         (%wset-get-pending-event wset))
    (sb-thread:with-recursive-lock ((async-waitable-set-mutex wset))
      (setf (async-waitable-set-waiters wset)
            (remove *current-thread*
                    (async-waitable-set-waiters wset)
                    :test #'eq)))
    (decf (async-waitable-set-num-waiting wset))))

(defun %wset-poll (wset)
  "WaitableSet.poll (CanonicalABI.md 1358)."
  (if (%wset-has-pending-event wset)
      (%wset-get-pending-event wset)
      (list 0 0 0)))

(defun %wset-drop (wset)
  "WaitableSet.drop (CanonicalABI.md 1391): trap when non-empty or
  waited upon (the fixture message covers both cases)."
  (when (or (async-waitable-set-elems wset)
            (> (async-waitable-set-num-waiting wset) 0))
    (error 'wasmdown:wasm-trap :kind :generic
           :message "cannot drop waitable set with waiters")))

(defun %wset-wait-from-callback (wset)
  "WaitableSet.wait_from_callback (CanonicalABI.md 1373): the
  readiness condition ALSO requires the exclusive_thread lock free;
  pending cancellation is delivered as TASK_CANCELLED (code 6)."
  (let* ((thread *current-thread*)
         (task *current-task*)
         (inst (async-task-inst task))
         ;; M14 THE PARK READINESS (§1373's readiness condition,
         ;; hoisted to the thread's ready-fn per the spec's
         ;; start_waiting(ready_func) — Task.request_cancellation's
         ;; §769 `cancellable and ready()` gate tests THIS closure)
         (readiness
           (lambda ()
             (and (null (%inst-exclusive-thread inst))
                  (or (%wset-has-pending-event wset)
                      (%task-has-pending-cancel task))))))
    (incf (async-waitable-set-num-waiting wset))
    ;; M13-tok2 GATE 1: the parked-waiter registration (the durable
    ;; wake target)
    (sb-thread:with-recursive-lock ((async-waitable-set-mutex wset))
      (pushnew thread (async-waitable-set-waiters wset) :test #'eq))
    (unwind-protect
         (progn
           (setf (async-thread-ready-fn thread) readiness)
           (%block-enter thread)
           ;; M14 THE GRANTED PARK (the §932 turn model at the
           ;; callback WAIT arm): the park returns only on a GRANT
           ;; — the real tick (async-thread-resume-all sets granted
           ;; on every thread it touches: the caller's blocking-op
           ;; resume-all, the drain loops' ticks) or the explicit
           ;; resume (async-thread-resume — the switch arms, the
           ;; cancellation's synchronous resume). The incidental
           ;; broadcasts (the end-notify of an arriving event, the
           ;; spurious condition-wait return, the leaked inter-park
           ;; count) do NOT grant: the worker's continuation past
           ;; its first block waits for the caller's next blocking
           ;; op — the fixtures' pinned interleave ("the worker
           ;; stays put"; ::7's NONE polls, ::9's $ran check). The
           ;; readiness is re-tested AFTER the grant (the event may
           ;; have arrived during the park — the grant + the
           ;; readiness together complete the turn); a grant without
           ;; readiness loops (the count is consumed, the wait
           ;; re-arms).
           ;; M15 MECHANISM 1 (§932 ONE THREAD ONE QUANTUM + §1373's
           ;; readiness): the park exits only on GRANT **AND**
           ;; READINESS. The grant is consumed either way — a grant
           ;; spent while NOT ready is a wasted quantum (the thread
           ;; re-arms for the NEXT tick). The OLD tail
           ;; (`async-thread-wait-until thread readiness`) woke on
           ;; the ARRIVING EVENT with NO grant: the unsupervised
           ;; continuation — cancel-targeted-resume ::7's worker ran
           ;; inside the cancel window on its future's readiness
           ;; alone.
           (loop
             (async-thread-check-abandoned thread)
             (%right-acquire thread)
             (let ((m (async-thread-mutex thread))
                   (done nil))
               (sb-thread:grab-mutex m)
               (let ((g (async-thread-granted thread)))
                 (cond
                  (g
                   (setf (async-thread-granted thread) nil)
                   (setf (async-thread-wake-count thread) 0)
                   (setf done (funcall readiness)))
                  (t
                   (when (> (async-thread-wake-count thread) 0)
                     (setf (async-thread-wake-count thread) 0)))))
               (cond
                (done
                 (sb-thread:release-mutex m)
                 (incf *async-progress*)
                 (return))
                (t
                 ;; STEP 4 PARK SITE 4: the grant/readiness test runs
                 ;; under the right; the wait releases it (fact b).
                 (%park thread m (async-thread-waitqueue thread))))))
           (if (%task-deliver-pending-cancel task)
               (list 6 0 0)             ; EventCode.TASK_CANCELLED
               (%wset-get-pending-event wset)))
      (setf (async-thread-ready-fn thread) nil)
      (%block-exit thread)
      (sb-thread:with-recursive-lock ((async-waitable-set-mutex wset))
        (setf (async-waitable-set-waiters wset)
              (remove thread (async-waitable-set-waiters wset)
                      :test #'eq)))
      (decf (async-waitable-set-num-waiting wset)))))

;;; ---------------------------------------------------------------
;;; Subtask operations (CanonicalABI.md 1435-1488).

(declaim (notinline %subtask-resolved-p %subtask-resolve
                    %subtask-deliver-resolve %subtask-drop))

(defun %subtask-resolved-p (s)
  "Subtask.resolved (CanonicalABI.md 1435)."
  (>= (async-subtask-state s) 2))

(defun %subtask-resolve (s state flat-results)
  "Subtask.resolve (CanonicalABI.md 1460)."
  (setf (async-subtask-state s) state)
  (setf (async-subtask-flat-results s) flat-results)
  ;; the resolve arms the subtask's WAITABLE event: a waiter parked
  ;; on the owning set must re-test (§1282-1294 — the pending-event
  ;; exists only once resolved)
  (when (async-waitable-wset s)
    ;; M13-tok2 GATE 1: post the DURABLE wake to the parked waiters
    ;; — a REAL wake-count (the M13-4 lost-broadcast closure
    ;; applied to the resolve path) rather than only the transient
    ;; resume-all nudge; a count posted inside the park's
    ;; registration window is consumed at the park itself. The
    ;; set's own broadcast stays (harmless when no count rides it).
    (let ((waiters
            (sb-thread:with-recursive-lock
                ((async-waitable-set-mutex (async-waitable-wset s)))
              (sb-thread:condition-broadcast
               (async-waitable-set-waitqueue (async-waitable-wset s)))
              (copy-list (async-waitable-set-waiters
                          (async-waitable-wset s))))))
      (dolist (w waiters)
        (sb-thread:with-recursive-lock ((async-thread-mutex w))
          (incf (async-thread-wake-count w))
          (sb-thread:condition-broadcast (async-thread-waitqueue w))
          (sb-thread:condition-broadcast
           (async-thread-suspend-waitqueue w)))))
    ;; M14: DEFER inside the cancel-wait window (the third M11a
    ;; site running on the callee's thread inside the synchronous-
    ;; resume wait — the resolve's nudge would grant the callee's
    ;; OTHER ready threads (the ::7 worker) in the caller's
    ;; observation window; the durable wake-count above already
    ;; reached the resolved subtask's OWN wset waiters)
    (if *defer-resume-all*
        (push s *deferred-resume-all-tasks*)
        (async-thread-resume-all))))

(defun %subtask-record-lend (s h)
  "Record the LEND of the resource-handle H on the subtask S (the
§1446 accounting home): H joins the subtask's LENDERS and, on a
CURRENT-TASK, that task joins LENDING-TASKS (pushnew) — the FIRST
lend to this subtask raises the task's NUM-BORROWS by one, so the
count and deliver_resolve's per-task decrement stay balanced even
when one task lends several handles to the same subtask."
  (push h (async-subtask-lenders s))
  (when *current-task*
    (unless (member *current-task* (async-subtask-lending-tasks s)
                    :test #'eq)
      (incf (async-task-num-borrows *current-task*))
      (push *current-task* (async-subtask-lending-tasks s))))
  h)

(defun %subtask-deliver-resolve (s)
  "Subtask.deliver_resolve (CanonicalABI.md 1467): the delivered
  mark + §1446-1456's num_lends accounting: all the borrowed
  handles are logically returned to the caller at this point —
  every lent handle's NUM-LENDS decrements and every LENDING task's
  NUM-BORROWS decrements (the async lower's (borrow $R) branch
  recorded each lend through %HANDLE-LEND, §2838/§1446; without the
  return the lending task's own resolution traps 'outstanding
  borrows' — drop-cross-task-borrow ::2's decoded layer). The
  ledger is CLEARED FIRST so a second delivery is a NO-OP."
  (setf (async-subtask-resolve-delivered-p s) t)
  (let ((lent (async-subtask-lenders s))
        (tasks (async-subtask-lending-tasks s)))
    (setf (async-subtask-lenders s) nil
          (async-subtask-lending-tasks s) nil)
    (dolist (h lent)
      (when (and (resource-handle-p h)
                 (> (resource-handle-num-lends h) 0))
        (decf (resource-handle-num-lends h))
        (when (zerop (resource-handle-num-lends h))
          (setf (resource-handle-borrow-scope h) nil))))
    (dolist (task tasks)
      (when (> (async-task-num-borrows task) 0)
        (decf (async-task-num-borrows task))))))

(defun %subtask-drop (s)
  "Subtask.drop (CanonicalABI.md 1485): trap when the resolve has
  not been delivered."
  (unless (async-subtask-resolve-delivered-p s)
    (error 'wasmdown:wasm-trap :kind :generic
           :message "cannot drop a subtask which has not yet resolved"))
  (%waitable-join s nil))

;;; ---- Table.add for any handle record (waitables, sets, subtasks
;;; share the instance's 1-based handle vector) ----

(defun %handle-free-slot (inst i)
  "Record the freed slot I on the instance's Table free list
(CanonicalABI.md 1146: remove appends to free; add pops free FIRST —
the fixtures assert exact re-used indices: a second stream.new after a
drop-readable must return index 1 again)."
  (push i (gethash :handle-free (comp-instance-imports inst))))

(defun %handle-add-any (inst v)
  "Table.add (§1131-1140): pop the free list first, else extend."
  (let* ((vec (%inst-handles inst))
         (free (gethash :handle-free (comp-instance-imports inst))))
    (if free
        (let ((i (pop (gethash :handle-free
                                 (comp-instance-imports inst)))))
          (setf (aref vec (1- i)) v)
          i)
        (progn
          (vector-push-extend v vec)
          (length vec)))))

(defun %handle-at (inst i &optional (type-p #'identity) (what "handle"))
  "Table.get with a type guard: the 1-based slot at I must hold a
  record satisfying TYPE-P."
  (let ((v (%inst-handles inst)))
    (unless (and (integerp i) (> i 0) (<= i (length v))
                 (funcall type-p (aref v (1- i))))
      (error 'wasmdown:wasm-trap :kind :generic
             :message (format nil "invalid ~a index ~a" what i)))
    (aref v (1- i))))

;;; ---------------------------------------------------------------
;;; The canon_lift ASYNC entry (CanonicalABI.md 3389-3544): the
;;; guest body runs on its own SBCL thread; the CALLER waits on the
;;; task's resolution. The callback event loop runs INSIDE the
;;; guest thread; the YIELD/WAIT turn boundaries are where the
;;; guest thread blocks on its readiness condition.

(defun %unpack-callback-result (packed)
  "unpack_callback_result (CanonicalABI.md 3564)."
  (let ((code (logand packed 15)))
    (when (> code 2)
      (error 'wasmdown:wasm-trap :kind :generic
             :message "invalid callback result"))
    (cons code (ash packed -4))))



(defun %async-lift-guest-body (task lf cx)
  "The thread_func of canon_lift (CanonicalABI.md 3390-3493): runs on
  the guest's own SBCL thread. The task resolves via task.return /
  task.cancel (the host built-ins call %task-return on THIS task —
  *CURRENT-TASK* is thread-bound); the SYNC-lifted shape returns the
  value directly from the core call and resolves the task itself."
  (let* ((inst (async-task-inst task))
         (opts (async-task-opts task))
         (ft (async-task-ft task))
         (core (lifted-func-core lf))
         (callback-opt (wasmdown.component:canon-opt opts :callback))
         (callback (and callback-opt
                         (< (second callback-opt)
                            (length (comp-instance-core-funcs inst)))
                         (aref (comp-instance-core-funcs inst)
                               (second callback-opt))))
         (th (async-task-implicit-thread task)))
    (unless (%task-enter-implicit-thread task)
      ;; cancelled at the backpressure gate: already resolved
      (return-from %async-lift-guest-body :cancelled-at-gate))
    (setf (async-task-state task) :started)
    (let* ((args (funcall (async-task-on-start task)))
           (params (and ft (coerce (first (wasmdown.component:ct-payload
                                           ft)) 'list)))
           (vals (loop for p in (or params '())
                       for a in (or args '())
                       collect
                       ;; M13-tok29 THE GUEST-BODY BORROW
                       ;; CONVERSION (the last boundary of the
                       ;; own-transfer chain, sessions 22-28):
                       ;; the lower's borrow arm put the BI (the
                       ;; index at the CALLEE-DEFINING table —
                       ;; the cx's inst, the record the lend
                       ;; added) in the component args; the OLD
                       ;; domain conversion passed the raw INT
                       ;; through to the core (passing-
                       ;; resources' R.foo core received the
                       ;; index 5 and returned 55 where the
                       ;; fixture pins 100). For a (borrow $R)
                       ;; param whose domain value is an INT,
                       ;; deref the BI at the callee's own table
                       ;; and pass the REP (the spec's §2412
                       ;; h.rep — the core consumes it
                       ;; positionally). The BI's record stays
                       ;; in the table for the callee's resource
                       ;; BUILTINS (drop-cross-task-borrow's
                       ;; R-drop resolves its index there).
                       (let ((c (and (cdr p)
                                     (%abi-ct (cdr p)
                                              (abi-cx-inst cx)))))
                         (if (and c
                                  (wasmdown.component:ct-p c)
                                  (eq (wasmdown.component:ct-kind c)
                                      :borrow)
                                  (integerp a))
                             (let* ((rt-idx
                                      (wasmdown.component:ct-payload c))
                                    (rt (and (integerp rt-idx)
                                             (< rt-idx
                                                (length
                                                 (comp-instance-types
                                                  (abi-cx-inst cx))))
                                             (aref
                                              (comp-instance-types
                                               (abi-cx-inst cx))
                                              rt-idx))))
                               (or (and rt
                                        (%handle-rep
                                         (abi-cx-inst cx) rt a))
                                   (%abi-arg-domain a (cdr p) cx)))
                             (%abi-arg-domain a (cdr p) cx)))))
           (flat-args
             (if (or (abi-cx-memory cx) params)
                 ;; M13-lad (cross-abi-calls ::8-::11): the LIFT
                 ;; direction's guest body lowers the caller's
                 ;; component params into the CALLEE's core args with
                 ;; MAX_FLAT_PARAMS (16) ALWAYS — the async 4-limit
                 ;; (MAX_FLAT_ASYNC_PARAMS) is the LOWER direction's
                 ;; core-param rule (§2914-2929: the async LOWER's
                 ;; collapse); the async-callback LIFT's core funcs
                 ;; take their params flat up to 16 (async-5-param:
                 ;; five flat slots; async-17-param: the [ptr]
                 ;; collapse via the canon's memory+realloc).
                 (%abi-lower-values
                  cx 16
                  vals (loop for p in (or params '())
                             collect (cdr p)))
                 '()))
           (flat-ft
             (and ft (wasmdown.component:flatten-functype
                      ft (comp-instance-types inst)
                      (if callback :lower :lift)))))
      (cond
       ((and (%task-opts-async-p task) callback)
        ;; ---- THE CALLBACK EVENT LOOP (§3464-3493) ----
        (let ((packed (first (apply #'wasmdown.runtime:wasm-call
                                     core flat-args))))
          (loop
            ;; M14 THE TURN BOUNDARY bump (§756-759: Thread.resume
            ;; returns when the resumed thread's callback "returns
            ;; or blocks"): each loop-top entry is one turn. The
            ;; broadcast under the TASK's mutex wakes the
            ;; synchronous-resume waiter in %task-request-
            ;; cancellation (the task resolved — the waiter checks
            ;; resolved-p — or the turn moved: the callback
            ;; returned/blocked WITHOUT resolving, the BLOCKED
            ;; case).
            (%task-bump-turn task)
            (let ((code/si (%unpack-callback-result packed)))
              (when (eq (car code/si) 0) (return)) ; EXIT
              ;; the turn boundary: deliver pending cancel FIRST
              (let ((event
                      (if (%task-deliver-pending-cancel task)
                          (list 6 0 0)          ; TASK_CANCELLED
                          (case (car code/si)
                            (1                     ; YIELD
                             ;; release the exclusive lock while
                             ;; waiting (§3470-3471) so siblings run;
                             ;; the readiness (exclusive lock FREE)
                             ;; is IMMEDIATELY true here, so a bare
                             ;; wait-until would hot-spin the guest
                             ;; (cb -> YIELD -> cb) and starve the
                             ;; siblings — the §507 host freedom is
                             ;; used to BLOCK: park this thread on
                             ;; its waitqueue for a scheduler nudge
                             ;; (Store.tick), giving other threads
                             ;; real CPU
                             (setf (async-thread-cancellable th) t)
                             (let ((saved (%inst-exclusive-thread inst)))
                               (setf (%inst-exclusive-thread inst) nil)
                               (unwind-protect
                                    (async-thread-yield th)
                                 (setf (%inst-exclusive-thread inst)
                                       saved)
                                 (setf (async-thread-cancellable th)
                                       nil)))
                             (if (%task-deliver-pending-cancel task)
                                 (list 6 0 0)
                                 (list 0 0 0)))
                            (2                     ; WAIT
                             (let ((wset
                                     (%handle-at
                                      inst (cdr code/si)
                                      #'async-waitable-set-p
                                      "waitable set")))
                               (setf (async-thread-cancellable th) t)
                               ;; §3480-3483: the WAIT turn ALSO
                               ;; releases the exclusive lock for the
                               ;; duration of the wait (the readiness
                               ;; condition of wait_from_callback
                               ;; REQUIRES it free — holding it here
                               ;; self-deadlocks the callback loop)
                               (let ((saved (%inst-exclusive-thread inst)))
                                 (setf (%inst-exclusive-thread inst) nil)
                                 (unwind-protect
                                      (%wset-wait-from-callback wset)
                                   (setf (%inst-exclusive-thread inst)
                                         saved)
                                   (setf (async-thread-cancellable th)
                                         nil)))))
                            (t
                             (error 'wasmdown:wasm-trap
                                    :kind :generic
                                    :message "invalid callback result"))))))
                (setf packed
                      (first (apply #'wasmdown.runtime:wasm-call
                                    callback
                                    (list (first event)
                                          (second event)
                                          (third event))))))))))
       (t
        ;; ---- the STACKFUL async case (§3452-3456) and the SYNC
        ;; lift of an ASYNC-TYPED functype: a single core call. The
        ;; callee resolves via task.return (async-typed) — or, when
        ;; the functype is SYNC-typed (a plain sync lift), the core
        ;; results ARE the value (lifted here and resolved).
        (let ((res (apply #'wasmdown.runtime:wasm-call core flat-args)))
          (unless (%task-opts-async-p task)
            ;; SYNC-TYPED: the results lift per the functype; the
            ;; sync-caller drain loop (below) needs the task resolved
            (let* ((result (second (wasmdown.component:ct-payload
                                    ft)))
                   (vi (make-array 16 :fill-pointer 0
                                   :adjustable t)))
              (loop for r in (coerce res 'list)
                    for rt in (result-core-types lf result)
                    do (vector-push-extend (cons r rt) vi))
              (let ((lifted
                      (if result
                          (%abi-lift-values cx 1 vi (list result))
                          '())))
                (%task-return task (first lifted))))))))
      (%task-exit-implicit-thread task))))

(defun %canon-lift-async (lf args)
  "The ASYNC canon_lift entry (CanonicalABI.md 3389, 3535-3543):
  create the Task + Thread pair, run the guest body on a fresh SBCL
  thread, wait for the resolution, and return the component-level
  result (the runner's INVOKE path). A guest TRAP propagates to the
  caller; a task that never resolves (the guest returned without
  task.return and no other thread can make progress) is the DEADLOCK
  trap (§3539-3542: no ready candidates)."
  (let* ((ft (lifted-func-type lf))
         (opts (lifted-func-opts lf))
         (inst (lifted-func-inst lf))
         (task (make-async-task
                :ft ft :opts opts :inst inst
                :on-start (lambda () args)))
         (task (progn (%tok-stamp-task task) task))
         (th (make-async-thread :task task))
         (enc-opt (wasmdown.component:canon-opt opts :string-encoding))
         (cx (make-abi-cx
              :memory (lifted-func-memory lf)
              :realloc (lifted-func-realloc lf)
              :encoding (%abi-encoding-of enc-opt)
              :inst inst))
         (cell (cons nil nil)))
    (setf (async-task-implicit-thread task) th)
    (%thread-birth th)
    (setf (async-task-on-resolve task)
          (lambda (result)
            (setf (car cell) t (cdr cell) result)))
    (let ((guest
            (sb-thread:make-thread
             (lambda ()
               (let ((*current-task* task)
                     (*current-thread* th)
                     (*right-self* th)
                     (*handle-caller-inst* inst))
                 ;; STEP 4: guest code runs only under the right.
                 (%right-acquire th)
                 (unwind-protect
                     (handler-case
                         (%async-lift-guest-body task lf cx)
                 (error (c)
                   ;; PUBLISH the failure FIRST (the write happens
                   ;; under the right, so the runner's poll reads a
                   ;; field no one else can be writing), THEN exit —
                   ;; %task-exit-implicit-thread drops the ticket and
                   ;; hands the right on.
                   (setf (car cell) :trap
                         (cdr cell) c)
                   (sb-thread:with-recursive-lock
                       ((async-task-mutex task))
                     (setf (async-task-resolved-p task) t)
                     (sb-thread:condition-broadcast
                      (async-task-waitqueue task)))
                   ;; The trap path MUST run Task.exit_implicit_thread
                   ;; FIRST: the guest body died mid-turn, so the
                   ;; exclusive lock + scheduler registration may
                   ;; still be held — resolving without releasing
                   ;; LEAKS the lock and starves every
                   ;; needs-exclusive peer of this instance.
                   ;; Idempotent guards: an already-exited (or
                   ;; never-entered) task's remove-setf is a no-op.
                   (handler-case
                       (%task-exit-implicit-thread task)
                     (error () nil))
                     ;; M11a wake propagation: a trapped task may
                     ;; be the waker sibling parked tasks awaited
                     ;; (Store.tick, §932)
                     (async-thread-resume-all))))
                   ;; STEP 4: every exit path drops the ticket and
                   ;; hands the right on.
                   (%right-thread-death th))))))
      (declare (ignore guest))
      (let* ((self (%driver-self))
             (*right-self* self)
             (*right-driver* self))
        (%right-acquire self)
        (let ((nested (and (boundp '*current-thread*) *current-thread*)))
          (%block-enter nested))
        (unwind-protect
             (loop
               with idle-cycles = 0
               ;; STEP 4: the runner is a right PARTICIPANT with the
               ;; synthetic ticket (0) — it sorts before every guest
               ;; root, so a parked guest hands the right back to the
               ;; tick engine exactly as the pre-permit runtime's
               ;; drain loops expected.
               do (%right-acquire self)
               when (async-task-resolved-p task)
                 do (return)
               ;; spend one tick (the M11a propagation) then release
               ;; the right for one bounded poll slice — the guest
               ;; threads are the only parties that can resolve the
               ;; task, and they can only run while the right is
               ;; released
               do (let* ((before *async-progress*)
                         (spent (%drain-tick-needed-p th)))
                    (when spent
                      (async-thread-resume-all))
                    (unless (async-task-resolved-p task)
                      (%right-stall self))
                    ;; THE ORACLE'S QUISCENCE RULE: a tick SPENT with
                    ;; no progress is a quiescent cycle (the deadlock
                    ;; fixtures' oracle); a cycle with NO tick spent
                    ;; cannot judge progress (the guest is mid-turn).
                    (if (> *async-progress* before)
                        (setf idle-cycles 0)
                        (if spent
                            (incf idle-cycles)
                            (setf idle-cycles 0))))
             ;; THE DEADLOCK TRAP (§3539-3542): enough consecutive
             ;; quiescent cycles with the task unresolved = no ready
             ;; candidate can ever make progress
             when (>= idle-cycles 50)
               do
               ;; THE ZOMBIE JOIN (§3552's teardown — the same class
               ;; as %sync-lift-run's): the unresolved task's GUEST
               ;; thread stays parked in the scheduler; the NEXT
               ;; test's resume-all would nudge it (a re-parking
               ;; CPU thief). Mark it ABANDONED (its parks unwind),
               ;; unblock every park, unregister it — the SBCL
               ;; thread exits on its own; the caller traps.
               (setf (async-thread-suspended-p th) nil)
               (setf (async-thread-yield-parked th) nil)
               (setf (async-thread-block-depth th) 0)
               (setf (async-thread-abandoned th) t)
               (ignore-errors (%task-exit-implicit-thread task))
               (sb-thread:with-recursive-lock ((async-thread-mutex th))
                 (sb-thread:condition-broadcast
                  (async-thread-suspend-waitqueue th))
                 (sb-thread:condition-broadcast (async-thread-waitqueue th)))
               ;; M13-tok9 THE WHOLE-STORE TEARDOWN (§3552 'trap
               ;; and tear down the whole store'): the DEADLOCKED
               ;; instance's OTHER parked threads (the fixture's
               ;; parked-by-design yielder/waiter) stay registered
               ;; and yield-parked — path-earlier than every LATER
               ;; round's threads, they consume each tick's single
               ;; grant (switch-to-ready-callback row 16: four
               ;; leftovers from rows 13/15 starved the live round
               ;; into its own oracle). The store is the SCRIPT's
               ;; whole state — every earlier round's leftover is
               ;; in it (each assert row instantiates a NEW
               ;; instance, so an inst-scoped teardown matches
               ;; nothing). Abandon + unblock + release every
               ;; OTHER registered thread; the SBCL threads unwind
               ;; on their own.
               (let ((inst (async-task-inst task)))
                 (declare (ignore inst))
                 (dolist (th2 (copy-list *scheduler-threads*))
                   (when (and (async-thread-task th2)
                              (not (eq th2 th)))
                     (setf (async-thread-suspended-p th2) nil)
                     (setf (async-thread-yield-parked th2) nil)
                     (setf (async-thread-block-depth th2) 0)
                     (setf (async-thread-abandoned th2) t)
                     (ignore-errors
                       (%task-exit-implicit-thread
                        (async-thread-task th2)))
                     (sb-thread:with-recursive-lock
                         ((async-thread-mutex th2))
                       (sb-thread:condition-broadcast
                        (async-thread-suspend-waitqueue th2))
                       (sb-thread:condition-broadcast
                        (async-thread-waitqueue th2))))))
               (error 'wasmdown:wasm-trap :kind :generic
                      :message
                      "deadlock detected: event loop cannot make further progress"))
          (%block-exit (and (boundp '*current-thread*) *current-thread*))
          (%right-release-on-exit self)))
      (etypecase (car cell)
        ((eql t)
         (let ((r (cdr cell)))
           (cond
            ((eq r :cancelled) nil)
            ;; a trapped CALLEE re-signs at the boundary (§3584)
            ((and (consp r) (eq (car r) :trap))
             (error (cdr r)))
            (t r))))
        (null
         ;; the guest body finished without resolving the task: no
         ;; thread can make further progress — the deadlock trap
         (error 'wasmdown:wasm-trap :kind :generic
                :message
                "deadlock detected: event loop cannot make further progress"))
        (keyword (error (cdr cell)))))))

;;; ---------------------------------------------------------------
;;; The canon <builtin> ASYNC host funcs (CanonicalABI.md 3835-4239).
;;; Each canon's %exec-canon :builtin arm allocates one; the bodies
;;; read *CURRENT-TASK* (bound per-thread during guest execution).

(declaim (notinline %builtin-async-host-func))

(defun %current-task-or-trap ()
  (or *current-task*
      (error 'wasmdown:wasm-trap :kind :generic
             :message "called an async builtin outside a task")))
(declaim (notinline %task-return-result-type))

(defun %task-return-result-type (imms)
  "The (result $t)? immediate's component valtype, or NIL. The
BINARY's second immediate is the raw valtype itself (a keyword or
an (:idx . n) cons) — binary.wast ::94's (:task-return :u32); the
TEXT parser's is the collected-valtypes list whose first element
is the valtype."
  (let ((rs (second imms)))
    (cond
     ((null rs) nil)
     ;; the BINARY reader's raw valtype: a keyword or a DOTTED
     ;; (:idx . n) cons (a type reference — never a list of valtypes;
     ;; reading it as one made the flat walk see the index's CDR).
     ((keywordp rs) rs)
     ((and (consp rs) (eq (car rs) :idx)) rs)
     ;; the TEXT parser's collected-valtypes list: the first element
     ;; is the valtype keyword or the (:idx . n) cons
     ((and (consp rs) (consp (first rs))
           (listp (first (first rs))))
      (first (first rs)))
     ;; the TEXT parser's collected-valtypes list (one element for a
     ;; single-result func): the valtype itself
     ((consp rs) (first rs))
     (t rs))))

(defun %task-return-opts (imms)
  "The canon's trailing canonopts (the third immediate)."
  (third imms))

(defun %task-return-flat-types (inst imms)
  "The flat core PARAMS of task.return — the result type's flattened
  slots (CanonicalABI.md 3929: flatten_functype of (func (param $t)?)
  in the LOWER context: the params ARE the flat result slots, no
  out-param is appended). M13-lad: the MEMORY collapse — when the
  canon carries the `memory` opt and the flat slots exceed
  MAX_FLAT_PARAMS (16), the values pass through memory and the core
  func takes the single [ptr] param (cross-abi-calls' task.return17:
  the (memory ...) canon of a 17-tuple result)."
  (let* ((rt (%task-return-result-type imms))
         (opts (%task-return-opts imms))
         (mem-opt (and opts
                       (wasmdown.component:canon-opt opts :memory))))
    (let ((flat (if rt (%flat-type-of rt inst) '())))
      (if (and mem-opt (> (length flat) 16))
          '(:i32)
          flat))))

(defun %wset-event-cx (inst kind imms)
  "waitable-set.wait/poll's memory option (the event-payload store
  memory) as an abi-cx."
  (declare (ignore kind))
  (let* ((mem-idx (second imms))
         (memory (and (integerp mem-idx)
                      (< mem-idx (length (comp-instance-core-memories
                                         inst)))
                      (aref (comp-instance-core-memories inst)
                            mem-idx))))
    (make-abi-cx :memory memory :inst inst)))

(defun %unpack-event-store (cx event ptr)
  "unpack_event (CanonicalABI.md 4043): the two i32 payloads stored
  at PTR and PTR+4; the event code returns as the i32 result."
  (%abi-check-span cx ptr 8 "event")
  (%abi-check-align cx ptr 4 "event")
  (%abi-store-int cx (second event) ptr 4)
  (%abi-store-int cx (third event) (+ ptr 4) 4)
  (first event))

(defun %builtin-async-host-func (inst kind imms)
  "The canon <builtin> core wrapper: a real host func-instance whose
  signature is the flat sig the canon marker recorded. IMMS carries
  the builtin's immediates (task.return's result + opts, the
  subtask.cancel async flag, wait/poll's memory option). The
  thread.*/future.*/stream.*/error-context.* kinds stay OPAQUE (M11b):
  the canon still allocates the core-func slot (instantiate stays
  green); the CALL traps."
  (ecase kind
    ((:task-return)
     (let ((flat (%task-return-flat-types inst imms))
           (rt (%task-return-result-type imms))
           (opts (%task-return-opts imms)))
       (let* ((mem-opt (and opts
                            (wasmdown.component:canon-opt
                             opts :memory)))
              (enc-opt (and opts
                            (wasmdown.component:canon-opt
                             opts :string-encoding)))
              (memory (and mem-opt
                           (< (second mem-opt)
                              (length (comp-instance-core-memories
                                       inst)))
                           (aref (comp-instance-core-memories inst)
                                 (second mem-opt)))))
         (wasmdown.runtime:make-host-func-instance
          (wasmdown.types:make-wasm-func-type flat '())
          (lambda (args)
            (let* ((task (%current-task-or-trap))
                   (cx (make-abi-cx
                        :memory memory
                        :encoding (%abi-encoding-of enc-opt)
                        :inst inst)))
              ;; lift_flat_values of the flat args per the RESULT
              ;; type (MAX_FLAT_PARAMS — 16 — the task-return
              ;; direction is a LIFT: core -> component)
              (let ((vi (make-array 16 :fill-pointer 0
                                   :adjustable t)))
                (loop for a in args
                      for ft_ in flat
                      do (vector-push-extend (cons a ft_) vi))
                (let ((lifted
                       (if rt (%abi-lift-values cx 16 vi (list rt))
                           '())))
                  (%task-return task (first lifted))))
              '()))))))
    ((:task-cancel)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '() '())
      (lambda (args)
        (declare (ignore args))
        (%task-cancel (%current-task-or-trap))
        '())))
    ((:context-get)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '() '(:i32))
      (lambda (args)
        (declare (ignore args))
        (let* ((thread *current-thread*)
               (i (third imms)))
          (unless thread
            (error 'wasmdown:wasm-trap :kind :generic
                   :message "called an async builtin outside a task"))
          (list (aref (async-thread-storage thread) i))))))
    ((:context-set)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '())
      (lambda (args)
        (let* ((thread *current-thread*)
               (i (third imms)))
          (unless thread
            (error 'wasmdown:wasm-trap :kind :generic
                   :message "called an async builtin outside a task"))
          (setf (aref (async-thread-storage thread) i)
                (first args)))
        '())))
    ((:backpressure-inc)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '() '())
      (lambda (args)
        (declare (ignore args))
        (incf (%inst-backpressure inst))
        '())))
    ((:backpressure-dec)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '() '())
      (lambda (args)
        (declare (ignore args))
        (decf (%inst-backpressure inst))
        ;; a dec may unblock gate-waiting threads
        (async-thread-resume-all)
        '())))
    ((:waitable-set-new)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '() '(:i32))
      (lambda (args)
        (declare (ignore args))
        (list (%handle-add-any inst
                               (make-async-waitable-set))))))
    ((:waitable-set-drop)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '())
      (lambda (args)
        (let* ((wset (%handle-at inst (first args)
                                #'async-waitable-set-p
                                "waitable set"))
               (h (%inst-handles inst)))
          (%wset-drop wset)
          (setf (aref h (1- (first args))) nil)
          (%handle-free-slot inst (first args)))
        '())))
    ((:waitable-set-wait :waitable-set-poll)
     (let ((cx (%wset-event-cx inst kind imms)))
       (wasmdown.runtime:make-host-func-instance
        (wasmdown.types:make-wasm-func-type '(:i32 :i32) '(:i32))
        (lambda (args)
          (let* ((wset (%handle-at inst (first args)
                                   #'async-waitable-set-p
                                   "waitable set"))
                 (ptr (second args))
                 (event (if (eq kind :waitable-set-wait)
                            (%wset-wait wset)
                            (%wset-poll wset))))
            (list (%unpack-event-store cx event ptr)))))))
    ((:waitable-join)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32 :i32) '())
      (lambda (args)
        (let* ((w (%handle-at inst (first args)
                              #'async-waitable-p "waitable"))
               (si (second args))
               (wset (and (/= si 0)
                          (%handle-at inst si
                                      #'async-waitable-set-p
                                      "waitable set"))))
          (when (async-waitable-has-sync-waiter w)
            (error 'wasmdown:wasm-trap :kind :generic
                   :message "waitable has a sync waiter"))
          ;; §4124's trap_if(w.has_sync_waiter) — the SPEC's own
          ;; message: a waitable being synchronously waited by
          ;; ANOTHER built-in (the subtask.cancel reentrance case)
          ;; cannot join a set (the sync user would deadlock)
          (%waitable-join w wset))
        '())))
    ((:subtask-cancel)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '(:i32))
      (lambda (args)
        (let ((subtask (%handle-at inst (first args)
                                    #'async-subtask-p "subtask"))
              (result nil))
          (when (async-subtask-resolve-delivered-p subtask)
            (error 'wasmdown:wasm-trap :kind :generic
                   :message "subtask resolve already delivered"))
          (when (async-subtask-cancellation-requested subtask)
            (error 'wasmdown:wasm-trap :kind :generic
                   :message "subtask cancellation already requested"))
          (when (%waitable-in-wset subtask)
            (error 'wasmdown:wasm-trap :kind :generic
                   :message "subtask is in a waitable set"))
          (unless (%subtask-resolved-p subtask)
            (setf (async-subtask-cancellation-requested subtask) t)
            (setf (async-waitable-has-sync-waiter subtask) t)
            (unwind-protect
                 (funcall (async-subtask-on-cancel subtask))
              (setf (async-waitable-has-sync-waiter subtask) nil))
            (unless (or (second imms)          ; the `async` opt
                        (%subtask-resolved-p subtask))
              ;; SYNC cancel: block until the subtask resolves
              (async-thread-wait-until
               *current-thread*
               (lambda () (%subtask-resolved-p subtask))))
            (unless (%subtask-resolved-p subtask)
              (setf result (list #xffffffff))))  ; BLOCKED
          (unless result
            (%subtask-deliver-resolve subtask)
            (setf result (list (async-subtask-state subtask))))
          result))))
    ((:subtask-drop)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '())
      (lambda (args)
        (let ((s (%handle-at inst (first args)
                             #'async-subtask-p "subtask"))
               (h (%inst-handles inst)))
          (%subtask-drop s)
          (setf (aref h (1- (first args))) nil)
          (%handle-free-slot inst (first args)))
        '())))
    ;; M11b: {stream,future}.new — the linked End pair added to the
    ;; instance's handles table, packed readable|writable<<32
    ;; (CanonicalABI.md 4242-4277). The may_leave gate is not yet
    ;; modeled (no post-return path creates ends in the fixtures).
    ((:stream-new :future-new)
     (let* ((t-ct (and (second imms)
                       (< (second imms)
                          (length (comp-instance-types inst)))
                       (aref (comp-instance-types inst)
                             (second imms))))
            ;; M13-tok25 THE ELEMENT'S SINGLE RESOLUTION: the
            ;; stream ct's payload is the element's RAW REF (an
            ;; (:idx . k) to the INTERNED element ct — passing-
            ;; resources' (stream (own $R)): the OWN ct sits at
            ;; that idx, its own-ness INTACT in the type space);
            ;; the OLD shape passed the raw ref to the pair and
            ;; every consumer re-resolved — the take one level
            ;; too far (the resource). Resolve ONCE here: the
            ;; end's t-ct IS the element ct.
            (elem (and t-ct
                       (wasmdown.component:ct-p t-ct)
                       (let ((raw (wasmdown.component:ct-payload t-ct)))
                         (or (and (consp raw)
                                  (eq (car raw) :idx)
                                  (integerp (cdr raw))
                                  (< (cdr raw)
                                     (length (comp-instance-types inst)))
                                  (aref (comp-instance-types inst)
                                        (cdr raw)))
                             raw))))
            (future-p (eq kind :future-new)))
       ;; the element ct: (stream $t?)'s payload — NIL for the
       ;; empty (stream) form (§4251)
       (wasmdown.runtime:make-host-func-instance
        (wasmdown.types:make-wasm-func-type '() '(:i64))
        (lambda (args)
          (declare (ignore args))
          ;; canon_{stream,future}_new (§4262-4276): add the
          ;; LINKED pair at CALL time, return readable|writable<<32
          (multiple-value-bind (reader writer)
              (%new-async-pair elem inst nil future-p)
            (let ((ri (%handle-add-any inst reader))
                  (wi (%handle-add-any inst writer)))
              (setf (async-end-index reader) ri)
              (setf (async-end-index writer) wi)
              (list (logior ri (ash wi 32)))))))))
    ;; the still-opaque kinds: the slot exists, the call traps
    ;; M11b: {stream,future}.{read,write} — the COPY family
    ;; (CanonicalABI.md 4280-4375): the 5-case End.copy over the
    ;; guest buffer, the BLOCKED sentinel for the async path, the
    ;; blocking wait for the sync path.
    ((:stream-read :stream-write :future-read :future-write)
     (let* ((future-p (member kind '(:future-read :future-write)))
            (read-p (member kind '(:stream-read :future-read)))
            ;; the canonopts: the parser arm embeds them as the
            ;; imms' THIRD element; the BINARY reader puts them
            ;; on the canonrec's OPTS (both shapes read here)
            ;; the canonopts: the parser arm embeds them as the imms'
            ;; THIRD element (the binary path's fixtures reach here
            ;; via the text parser in this lane; an empty opts list
            ;; is the safe default otherwise)
            (opts (or (third imms) '()))
            (mem-opt (wasmdown.component:canon-opt opts :memory))
            (memory (and mem-opt
                        (< (second mem-opt)
                           (length (comp-instance-core-memories inst)))
                        (aref (comp-instance-core-memories inst)
                              (second mem-opt))))
            ;; M16 slice 3b: the READ side's canon opts supply the
            ;; realloc/encoding a STRUCTURED element (a
            ;; directory-entry's name) needs when the host source
            ;; stores it into the guest buffer.
            (realloc-opt (wasmdown.component:canon-opt opts :realloc))
            (realloc (and realloc-opt
                          (< (second realloc-opt)
                             (length (comp-instance-core-funcs inst)))
                          (aref (comp-instance-core-funcs inst)
                                (second realloc-opt))))
            (enc-opt (wasmdown.component:canon-opt opts
                                                   :string-encoding))
            (t-idx (second imms))
            (t-ct (and (integerp t-idx)
                       (< t-idx (length (comp-instance-types inst)))
                       (aref (comp-instance-types inst) t-idx)))
            (elem-ct (and t-ct (wasmdown.component:ct-payload t-ct)))
            (sig (if future-p '(:i32 :i32) '(:i32 :i32 :i32))))
       (declare (ignorable future-p))
       (wasmdown.runtime:make-host-func-instance
        (wasmdown.types:make-wasm-func-type sig '(:i32))
        (lambda (args)
          ;; copy (§4317-4335): the guards, the buffer, the copy,
          ;; then the event/BLOCKED
          (let* ((i (first args))
                 (ptr (second args))
                 (length (if future-p 1 (third args)))
                 (end (%handle-at inst i #'async-end-p "stream end"))
                 (elem-decl (and t-ct
                                 (wasmdown.component:ct-payload t-ct)))
                 ;; the type guard: end.t == the canon's $t (§4322)
                 (declared (and elem-decl
                               (or (eq (async-end-t-ct end) elem-decl)
                                   (equal (async-end-t-ct end) elem-decl)))))
            (declare (ignorable declared elem-decl))
            (unless (eq (async-end-state end) :idle)
              (error 'wasmdown:wasm-trap :kind :generic
                     :message "cannot read or write a stream or future in a non-idle state"))
            (when (and (%waitable-in-wset end)
                       (not (wasmdown.component:canon-opt opts :async)))
              (error 'wasmdown:wasm-trap :kind :generic
                     :message "cannot read or write while in a waitable set"))
            ;; the future ends only read/write ONCE (§1666: futures
            ;; have no length param — always implicitly 1)
            (let ((buffer (%buffer-make end memory ptr length
                                        :realloc realloc
                                        :encoding (%abi-encoding-of enc-opt))))
              (%end-copy end buffer read-p)
              (if (async-waitable-pending-event end)
                  ;; the event is ready: deliver it synchronously
                  (let* ((event (%waitable-get-pending-event end))
                         (payload (third event)))
                    (list payload))
                  ;; no event yet: BLOCKED (async) or wait (sync)
                  (if (wasmdown.component:canon-opt opts :async)
                      (list #xffffffff)  ; BLOCKED (§4158)
                      (progn
                        (setf (async-waitable-has-sync-waiter end) t
                              (async-waitable-sync-waiter-thread end)
                              *current-thread*
                              (async-thread-io-parked *current-thread*) t)
                        (unwind-protect
                             ;; M15 MECHANISM 7 (§932 + the fixture's
                             ;; "switch to the caller"): a blocking
                             ;; SYNC read/write continues at the
                             ;; CALLER's next tick, not the moment the
                             ;; peer's copy notifies (sync-streams
                             ;; pins $C.get still holding $C's
                             ;; exclusive lock when the caller's next
                             ;; $set enters its gate -> the pack
                             ;; reports STARTING).
                             (let ((th *current-thread*))
                               (setf (async-thread-ready-fn th)
                                     (lambda ()
                                       (not (null
                                             (async-waitable-pending-event
                                              end)))))
                               (%block-enter th)
                               (sb-thread:with-recursive-lock
                                   ((async-thread-mutex th))
                                 (setf (async-thread-granted th) nil)
                                 (setf (async-thread-wake-count th) 0))
                               (loop
                                 (async-thread-check-abandoned th)
                                 (%right-acquire th)
                                 (let ((m (async-thread-mutex th))
                                       (done nil))
                                   (sb-thread:grab-mutex m)
                                   (let ((g (async-thread-granted th)))
                                     (cond
                                      (g
                                       (setf (async-thread-granted th) nil)
                                       (setf (async-thread-wake-count th) 0)
                                       (setf done
                                             (not (null
                                                   (async-waitable-pending-event
                                                    end)))))
                                      (t
                                       (when (> (async-thread-wake-count th)
                                                0)
                                         (setf (async-thread-wake-count
                                                th) 0)))))
                                   (cond
                                    (done
                                     (sb-thread:release-mutex m)
                                     (incf *async-progress*)
                                     (return))
                                    (t
                                     ;; STEP 4 PARK SITE 5 (the sync
                                     ;; read/write rendezvous): the
                                     ;; grant/event test runs under the
                                     ;; right; the wait releases it.
                                     (%park th m
                                            (async-thread-waitqueue
                                             th))))))
                               (setf (async-thread-ready-fn th) nil)
                               (%block-exit th))
                          (setf (async-thread-io-parked *current-thread*) nil)
                          (setf (async-waitable-has-sync-waiter end)
                                nil)
                          (setf (async-waitable-sync-waiter-thread end)
                                nil))
                        (let* ((event (%waitable-get-pending-event end))
                               (payload (third event)))
                          (list payload)))))))))))
    ;; M11b: {stream,future}.drop-{readable,writable} (§4448-4492):
    ;; the guards (the readable/writable class, the copying-or-
    ;; cancelling check, the WritableFutureEnd DONE check) + End.drop
    ((:stream-drop-readable :stream-drop-writable
      :future-drop-readable :future-drop-writable)
     (let* ((readable-p (member kind '(:stream-drop-readable
                                       :future-drop-readable)))
            (t-ct (and (second imms)
                       (< (second imms)
                          (length (comp-instance-types inst)))
                       (aref (comp-instance-types inst)
                             (second imms)))))
       (wasmdown.runtime:make-host-func-instance
        (wasmdown.types:make-wasm-func-type '(:i32) '())
        (lambda (args)
          (let* ((i (first args))
                 (end (%handle-at inst i #'async-end-p "stream end")))
            ;; the class guard: readable drops take READ ends
            (unless (member (async-end-event-code end)
                            (list (if readable-p +event-stream-read+
                                      +event-stream-write+)
                                  (if readable-p +event-future-read+
                                      +event-future-write+)))
              (error 'wasmdown:wasm-trap :kind :generic
                     :message "handle index used with the wrong type"))
            ;; §4488: trap on a copy in progress
            (when (%end-copying-or-cancelling-p end)
              (error 'wasmdown:wasm-trap :kind :generic
                     :message
                     "cannot remove busy stream"))
            ;; §4489: dropping a WRITABLE FUTURE end traps unless
            ;; the value was written (DONE) or the reader already
            ;; dropped (the peer notify armed DROPPED)
            (when (and (not readable-p)
                       (async-end-future-p end)
                       (not (eq (async-end-state end) :done)))
              (error 'wasmdown:wasm-trap :kind :generic
                     :message "cannot drop busy stream"))
            (%end-drop end)
            (setf (aref (%inst-handles inst) (1- i)) nil)
            (%handle-free-slot inst i))
          '()))))
    ;; M11b: {stream,future}.cancel-{read,write} (§4376-4445):
    ;; cancel_copy — the guards (the class, the COPYING state, the
    ;; sync-waiter, the in-wset-without-async), End.cancel, then
    ;; the event delivery / BLOCKED / the sync wait
    ((:stream-cancel-read :stream-cancel-write
      :future-cancel-read :future-cancel-write)
     (let* ((read-p (member kind '(:stream-cancel-read
                                    :future-cancel-read)))
            (async-p (third imms)))
       (wasmdown.runtime:make-host-func-instance
        (wasmdown.types:make-wasm-func-type '(:i32) '(:i32))
        (lambda (args)
          (let* ((i (first args))
                 (end (%handle-at inst i #'async-end-p "stream end")))
            ;; the class guard (§4409): cancel-read takes READ ends
            (unless (member (async-end-event-code end)
                            (list (if read-p +event-stream-read+
                                      +event-stream-write+)
                                  (if read-p +event-future-read+
                                      +event-future-write+)))
              (error 'wasmdown:wasm-trap :kind :generic
                     :message "handle index used with the wrong type"))
            ;; §4411: only an in-progress copy can cancel
            (unless (eq (async-end-state end) :copying)
              (error 'wasmdown:wasm-trap :kind :generic
                     :message
                     "cannot cancel a stream or future in a non-copying state"))
            ;; §4412: the sync waiter guard
            (when (async-waitable-has-sync-waiter end)
              (error 'wasmdown:wasm-trap :kind :generic
                     :message "waitable has a sync waiter"))
            ;; §4413: a sync cancel of an end in a waitable set
            (when (and (%waitable-in-wset end) (not async-p))
              (error 'wasmdown:wasm-trap :kind :generic
                     :message
                     "cannot cancel a stream or future in a waitable set"))
            (%end-cancel end)
            (if (async-waitable-pending-event end)
                (let* ((event (%waitable-get-pending-event end))
                       (payload (third event)))
                  (list payload))
                (if async-p
                    (list #xffffffff)  ; BLOCKED (§4419)
                    (progn
                      (setf (async-waitable-has-sync-waiter end) t)
                      (unwind-protect
                           (async-thread-wait-until
                            *current-thread*
                            (lambda ()
                              (not (null (async-waitable-pending-event
                                          end)))))
                        (setf (async-waitable-has-sync-waiter end)
                              nil))
                      (let* ((event (%waitable-get-pending-event end))
                             (payload (third event)))
                        (list payload))))))))))
    ;; M11b: thread.{index,yield,suspend,resume-later} (§4535-4665)
    ((:thread-index)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '() '(:i32))
      (lambda (args)
        (declare (ignore args))
        (list (or (and *current-thread*
                       (async-thread-index *current-thread*))
                  0)))))
    ;; thread.yield (§4641: Thread.yield_ — READY while parked, a
    ;; scheduler nudge re-runs it) vs thread.suspend (§4625:
    ;; Thread.suspend — NO readiness: only an EXPLICIT resume (the
    ;; switch builtins / resume-later) may wake it; Store.tick's
    ;; resume-all does NOT). The M11b shape parked BOTH on the
    ;; yield queue — exclusive-resume's sync thread then woke on a
    ;; bare nudge BEFORE the callback set $pinged.
    ((:thread-yield)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '() '(:i32))
      (lambda (args)
        (declare (ignore args))
        (unless *current-thread*
          (error 'wasmdown:wasm-trap :kind :generic
                 :message "called an async builtin outside a task"))
        (async-thread-yield *current-thread*)
        (list 0))))
    ((:thread-suspend)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '() '(:i32))
      (lambda (args)
        (declare (ignore args))
        (unless *current-thread*
          (error 'wasmdown:wasm-trap :kind :generic
                 :message "called an async builtin outside a task"))
        (async-thread-suspend *current-thread*)
        (list 0))))
    ((:thread-resume-later)
     (wasmdown.runtime:make-host-func-instance
      (wasmdown.types:make-wasm-func-type '(:i32) '())
      (lambda (args)
        (let* ((fields (%inst-async-fields inst))
               (target nil))
          (dolist (th (getf fields :threads))
            ;; §4605-4610: threads.get(i) returns the LIVE thread at
            ;; index i — a :done record (fired and finished, or swept
            ;; by the task-exit unfired-spawn teardown) is not a
            ;; target; the spec's Table.get would not return it
            (when (and (async-thread-index th)
                       (not (eq (async-thread-spawn-thunk th) :done))
                       (not (async-thread-abandoned th))
                       (= (async-thread-index th) (first args)))
              (setf target th)))
          ;; §4605-4610: Table.get traps OOB/empty; §4609's
          ;; trap_if(not other_thread.suspended()) — the target must
          ;; be SUSPENDED (a pending spawn, a suspend park, a yield
          ;; park); a resume path must never silently no-op (the
          ;; no-sibling-resume stall's 0xdead read).
          ;; §458-465 resume_later = start_waiting(ready = True):
          ;; MARK READY, never FIRE, never wake directly. The
          ;; pending spawn starts at the next TICK (the nudge's
          ;; claim-then-fire); a SUSPEND-parked target gets the
          ;; §242-273 HANDOFF install — THIS thread (the marker)
          ;; hands control back to the target at ITS next park/exit,
          ;; so the target's wake is causally AFTER the marker's
          ;; round completes (suspend-vs-yield's determinism: $r
          ;; set before the caller observes it); the tick's
          ;; ready-grant covers the case the marker never parks
          ;; again. A yield-parked target keeps its any-nudge wake.
          ;; (The old shape fired the spawn EAGERLY: the spawned
          ;; body raced the caller's switch — the ::34 flake.)
          (unless target
            (error 'wasmdown:wasm-trap :kind :generic
                   :message
                   (format nil "unknown thread index ~a"
                           (first args))))
          (unless (or (functionp (async-thread-spawn-thunk target))
                      (async-thread-suspended-p target)
                      (async-thread-yield-parked target))
            (error 'wasmdown:wasm-trap :kind :generic
                   :message "cannot resume thread which is not suspended"))
          (sb-thread:with-recursive-lock
              ((async-thread-mutex target))
            (cond
             ;; a PENDING SPAWN: §462-465's resume_later =
             ;; start_waiting(ready=True) — MARK ONLY, never fire:
             ;; the spawn starts at the next TICK (§932's grant) or
             ;; an EXPLICIT switch. The M13-4 eager fire measured
             ;; ::2's regression at the PRE-one-lock shape; the
             ;; M13-9 durable wake-count closes that race by
             ;; construction (the armed caller's park cannot lose
             ;; the worker's exit-post — the count is durable), and
             ;; the eager fire BREAKS cancel-targeted-resume's
             ;; pinned interleave: the spawned body (the keeper /
             ;; the ::9 worker) runs INSIDE the callee's core, its
             ;; task.return resolves the subtask in the STARTED-
             ;; wait's window, and the pack then reports RETURNED
             ;; where the rows pin STARTED (::1) / the cancel
             ;; returns CANCELLED where the row pins BLOCKED (::9's
             ;; own $ran check) — the caller must observe the
             ;; callee's state BEFORE any other thread of the
             ;; callee's instance takes a turn.
             ((functionp (async-thread-spawn-thunk target))
              (setf (async-thread-ready-p target) t))
             ;; a SUSPEND-parked target: the handoff install — the
             ;; marker's own next park/exit posts the target's wake
             ;; (causally after the marker's round completes)
             ((async-thread-suspended-p target)
              (setf (async-thread-ready-p target) t)
              (setf (async-thread-post-to target) *current-thread*)
              (setf (async-thread-wake-count target) 0))
             ;; otherwise (a yield park): the any-nudge wake
             (t
              (setf (async-thread-ready-p target) t)
              (sb-thread:condition-broadcast
               (async-thread-waitqueue target))))))
        '())))
    ;; thread.new-indirect (§4555-4596): read the funcref from the
    ;; canon's core TABLE (trap OOB/null), create a SUSPENDED
    ;; thread registered on the current instance, return the index.
    ;; The thread runs the funcref with the closure param on its
    ;; first resume (§4590's suspended() assert).
    ((:thread-new-indirect)
     (let* ((tbl-idx (third imms))
            (tbl (and (integerp tbl-idx)
                      (< tbl-idx
                         (length (comp-instance-core-tables inst)))
                      (aref (comp-instance-core-tables inst)
                            tbl-idx))))
       (wasmdown.runtime:make-host-func-instance
        (wasmdown.types:make-wasm-func-type '(:i32 :i32) '(:i32))
        (lambda (args)
          (let* ((task *current-task*)
                 (elem (and tbl
                            (< (first args)
                               (length (wasmdown.runtime:table-instance-elems
                                        tbl)))
                            (aref (wasmdown.runtime:table-instance-elems
                                   tbl)
                                  (first args)))))
            (unless task
              (error 'wasmdown:wasm-trap :kind :generic
                     :message
                     "called an async builtin outside a task"))
            (unless elem
              (error 'wasmdown:wasm-trap :kind :generic
                     :message "undefined element"))
            ;; §4590: the suspended thread calls the funcref with
            ;; the closure param $c, unregisters on return
            (let* ((fields (%inst-async-fields inst))
                   (th (make-async-thread :task task))
                   (c (second args)))
              (%thread-birth th)
              (setf (async-thread-spawn-thunk th)
                    (lambda ()
                      (sb-thread:make-thread
                       (lambda ()
                         (let* ((*current-task* task)
                                (*current-thread* th)
                                (*right-self* th))
                           (setf (async-thread-native th)
                                 sb-thread:*current-thread*)
                           ;; STEP 4: guest code runs only under the
                           ;; right. The spawn fires from another
                           ;; thread's tick, which has already
                           ;; released the right (resume-all runs
                           ;; under %without-right), so the grab here
                           ;; never blocks it.
                                    (%right-acquire th)
                           ;; §4587's call_and_trap_on_throw: the
                           ;; thread body's trap ENDS the thread
                           ;; (an unhandled trap on a guest thread
                           ;; would kill the whole process — the
                           ;; spawned thread has no outer handler)
                           (handler-case
                               (unwind-protect
                                   (wasmdown.runtime:wasm-call
                                    elem c)
                                 ;; unregister on return (§4588) —
                                 ;; and POST THE HANDOFF: the body's
                                 ;; EXIT hands control back to a
                                 ;; resume-later marker still parked
                                 ;; (the §242-273 exit-side release;
                                 ;; suspend-vs-yield's X never parks
                                 ;; again after its resume-later —
                                 ;; its EXIT is the handoff)
                                 (sb-thread:with-recursive-lock
                                     ((async-thread-mutex th))
                                   (setf (async-thread-spawn-thunk
                                          th)
                                         :done))
                                 ;; M13-tok6 GATE 1: UNREGISTER —
                                 ;; §4588's unregister_on_return: the
                                 ;; thread leaves the instance's
                                 ;; :threads list AND the scheduler
                                 ;; (the :done mark alone kept the
                                 ;; record resolvable-by-walk: the
                                 ;; next test's resume-all nudged a
                                 ;; finished thread, and §705's
                                 ;; maximize-index counted it — the
                                 ;; colliding idx 2 in the pin trace)
                                 (let ((fields (%inst-async-fields
                                                inst)))
                                   (setf (getf fields :threads)
                                         (remove th (getf fields
                                                          :threads)
                                                 :test #'eq)))
                                 (sb-thread:with-recursive-lock
                                     (*scheduler-mutex*)
                                   (setf *scheduler-threads*
                                         (remove th
                                                 *scheduler-threads*
                                                 :test #'eq)))
                                 ;; STEP 4: drop the ticket and hand
                                 ;; the right on (never waits).
                                 (%right-release-on-exit th)
                                 (%async-thread-post-handoff th))
                             (wasmdown:wasm-trap () nil))))
                       :name "new-indirect-thread")))
              ;; §4591: register_thread (the instance's next
              ;; index assignment, the :threads list + scheduler)
              (setf (async-thread-index th)
                    ;; M13-tok6 GATE 2: the live-only idx
                    ;; assignment (see the enter gate's note)
                    (1+ (loop for t2 in (getf fields :threads)
                              when (not (eq (async-thread-spawn-thunk
                                             t2)
                                            :done))
                                maximize (or (async-thread-index t2)
                                             0))))
              (setf (getf fields :threads)
                    (cons th (getf fields :threads)))
              (sb-thread:with-recursive-lock (*scheduler-mutex*)
                (push th *scheduler-threads*))
              (list (async-thread-index th))))))))
    ;; M11b: {stream,future}.forward (§4495-4534): the guards
    ;; (the class, the type match, IDLE, not in a wset), both ends
    ;; REMOVED, then End.forward (§1754): the trivial-loop case
    ;; drops both; otherwise the OUTER ends link directly and the
    ;; both-pending corner copies the SMALLER buffer through.
    ((:stream-forward :future-forward)
     (let* ((future-p (eq kind :future-forward))
            (t-idx (second imms))
            (t-ct (and (integerp t-idx)
                       (< t-idx
                          (length (comp-instance-types inst)))
                       (aref (comp-instance-types inst) t-idx))))
       (wasmdown.runtime:make-host-func-instance
        (wasmdown.types:make-wasm-func-type '(:i32 :i32) '())
        (lambda (args)
          (let* ((h (%inst-handles inst))
                 (ri (first args))
                 (wi (second args))
                 (r-end (and (<= 1 ri (length h))
                             (aref h (1- ri))))
                 (w-end (and (<= 1 wi (length h))
                             (aref h (1- wi)))))
            (flet ((bad (msg)
                     (error 'wasmdown:wasm-trap :kind :generic
                            :message msg)))
              (unless (and r-end (async-end-p r-end)
                           (eq (async-end-future-p r-end)
                               future-p))
                (bad "invalid readable end index"))
              (unless (and w-end (async-end-p w-end)
                           (eq (async-end-future-p w-end)
                               future-p))
                (bad "invalid writable end index"))
              ;; the type guard: end.t == the canon's $t (§4502):
              ;; a (stream $t?) ct's payload IS the element type
              (let ((elem (and t-ct
                               (wasmdown.component:ct-payload
                                t-ct))))
                (when (and elem
                           (not (equal (async-end-t-ct r-end)
                                       elem)))
                  (bad "stream type mismatch"))
                (when (and elem
                           (not (equal (async-end-t-ct w-end)
                                       elem)))
                  (bad "stream type mismatch")))
              (unless (eq (async-end-state r-end) :idle)
                (bad "cannot forward a non-idle readable end"))
              (unless (eq (async-end-state w-end) :idle)
                (bad "cannot forward a non-idle writable end"))
              (when (%waitable-in-wset r-end)
                (bad "cannot forward an end in a waitable set"))
              (when (%waitable-in-wset w-end)
                (bad "cannot forward an end in a waitable set"))
              ;; both REMOVED (§4519-4524)
              (setf (aref h (1- ri)) nil)
              (%handle-free-slot inst ri)
              (setf (aref h (1- wi)) nil)
              (%handle-free-slot inst wi)
              ;; End.forward (§1754-1763)
              (let ((src r-end) (dst w-end))
                (if (or (eq (async-end-other src) dst)
                        (null (async-end-other src))
                        (null (async-end-other dst)))
                    (progn (%end-drop src) (%end-drop dst))
                    (let ((writable-end (async-end-other src))
                          (readable-end (async-end-other dst)))
                      (setf (async-end-other writable-end)
                            readable-end)
                      (setf (async-end-other readable-end)
                            writable-end)
                      ;; the both-pending corner: copy the SMALLER
                      ;; buffer through (§1760-1763)
                      (when (and (async-end-buffer writable-end)
                                 (async-end-buffer readable-end))
                        (if (> (%buffer-remain
                                (async-end-buffer readable-end))
                               (%buffer-remain
                                (async-end-buffer writable-end)))
                            (let ((bigger readable-end)
                                  (smaller writable-end))
                              (declare (ignore bigger))
                              (let ((buffer (async-end-buffer
                                             smaller)))
                                (setf (async-end-buffer smaller)
                                      nil)
                                (%end-copy smaller buffer nil)))
                            (let ((bigger writable-end)
                                  (smaller readable-end))
                              (declare (ignore bigger))
                              (let ((buffer (async-end-buffer
                                             smaller)))
                                (setf (async-end-buffer smaller)
                                      nil)
                                (%end-copy smaller buffer
                                           t)))))))))
            '())))))
    ;; M11b: thread.{suspend,yield}-then-{resume,promote}
    ;; (§4668-4770; Thread.suspend_then_{resume,promote} §549-565).
    ;; All four take the target index + return 0. The SPEC SHAPE:
    ;;  - then-resume: SWITCH — the current thread parks (suspend:
    ;;    blocked, no readiness) or stays READY (yield: parked but
    ;;    re-runnable at the next tick), and the TARGET is resumed
    ;;    unconditionally (§531/§536: assert other.suspended()).
    ;;  - then-promote: resume the target ONLY when it is READY
    ;;    (a RUNNING or SUSPENDED-not-waiting target is left alone
    ;;    and the call falls back to a plain suspend/yield, §549).
    ;; A thread parked on its waitqueue by async-thread-yield is
    ;; WAITING (resumable); a thread blocked inside wait-until is
    ;; waiting on its own condition. The CURRENT thread is running
    ;; — never its own promote target (the trap).
    ((:thread-suspend-then-resume :thread-yield-then-resume
      :thread-suspend-then-promote :thread-yield-then-promote)
     (let ((promote-p (member kind '(:thread-suspend-then-promote
                                     :thread-yield-then-promote)))
           (yield-p (member kind '(:thread-yield-then-resume
                                   :thread-yield-then-promote))))
       (wasmdown.runtime:make-host-func-instance
        (wasmdown.types:make-wasm-func-type '(:i32) '(:i32))
        (lambda (args)
          (let* ((task *current-task*)
                 (fields (%inst-async-fields inst))
                 (target nil))
            (unless (and task *current-thread*)
              (error 'wasmdown:wasm-trap :kind :generic
                     :message
                     "called an async builtin outside a task"))
            ;; §4728/§4749: the promote variants trap on the
            ;; CURRENT thread's index
            (when (and promote-p
                       (= (or (async-thread-index *current-thread*)
                              -1)
                          (first args)))
              (error 'wasmdown:wasm-trap :kind :generic
                     :message "cannot switch to the current thread"))
            (dolist (th (getf fields :threads))
              ;; threads.get(i) returns the LIVE thread at index i —
              ;; a :done record (fired and finished, or swept by the
              ;; task-exit unfired-spawn teardown) is not a target
              (when (and (async-thread-index th)
                         (not (eq (async-thread-spawn-thunk th) :done))
                         (not (async-thread-abandoned th))
                         (= (async-thread-index th) (first args)))
                (setf target th)))
            ;; §4688/§4714 (canon_thread_{suspend,yield}_then_resume
            ;; ONLY): trap_if(not other_thread.suspended()) — the
            ;; target must be SUSPENDED (spawn thunk pending or
            ;; parked). The CURRENT thread is running: a self-index
            ;; (or a running sibling) traps here, in the arm — the
            ;; fixture's "cannot resume thread which is not
            ;; suspended". A not-found index is out-of-bounds — also
            ;; a trap. The PROMOTE pair (§4731/§549-565) has NO
            ;; not-suspended trap: only the i == thread.index trap
            ;; above; a running/unready target falls back to a plain
            ;; suspend/yield below. [NYI-NOTE: yield-parked is READY
            ;; not SUSPENDED in the spec's Thread model — this
            ;; predicate approximates parked≈suspended pending a
            ;; real blocked/ready thread state; self-switch-traps
            ;; was vacuously green at M11b (the component-payload
            ;; assert_trap matches phase-only, so the old
            ;; outside-task trap satisfied rows expecting the
            ;; switch trap) — the fixture now passes for the real
            ;; reason.]
            (when (and (not promote-p)
                       (not (and target
                                 (or (functionp (async-thread-spawn-thunk
                                                 target))
                                     (async-thread-ready-p target)
                                     (async-thread-suspended-p target)
                                     (async-thread-yield-parked
                                      target)
                                     ;; STEP 4: a thread CONTENDING for
                                     ;; the store right is not running
                                     ;; — it is parked at %right-acquire
                                     ;; and will continue when named.
                                     ;; Without this a legal switch
                                     ;; target reads as "not suspended"
                                     ;; (measured: during-sync rows).
                                     (async-thread-right-waiting
                                      target)))))
              (error 'wasmdown:wasm-trap :kind :generic
                     :message
                     "cannot resume thread which is not suspended"))
            ;; §549-565: the promote pair only resumes a READY
            ;; target (a pending spawn, a resume-later ready-mark,
            ;; a yield-park); a RUNNING target (on the stack below
            ;; the reentrant call) is NOT ready — the plain
            ;; suspend/yield fallback leaves it alone. The
            ;; -then-resume pair resumes any found target.
            (let ((resumable-target
                    (if (and promote-p target
                             (or (functionp (async-thread-spawn-thunk
                                             target))
                                 (async-thread-ready-p target)
                                 (async-thread-suspended-p target)
                                 (async-thread-yield-parked target)
                                 (async-thread-right-waiting target)))
                        target
                        (if promote-p nil target))))
              ;; M13-4 THE ONE-LOCK HANDOFF (§242-273's discipline,
              ;; per the session-3 design): ONE TICK FIRST, OUTSIDE
              ;; the park mutex (§932 — resume-all takes the scheduler
              ;; mutex then each thread's; holding the caller's park
              ;; mutex across it was the DEADLOCK cycle), then the
              ;; park is armed and the fire happens INSIDE the
              ;; caller's own mutex — the condition-wait's release is
              ;; the only window, and the target's park-back post is
              ;; DURABLE, so no interleaving of the three parties
              ;; loses a wake.
              (async-thread-resume-all)
              (let ((mu (async-thread-mutex *current-thread*))
                    (ct *current-thread*))
                (sb-thread:grab-mutex mu)
                (async-thread-check-abandoned ct)
                ;; M13-tok34 THE INTER-ROUND DRAIN (see the note
                ;; below): drain a surviving count under the mutex
                ;; before arming.
                (setf (async-thread-wake-count ct) 0)
                (if yield-p
                    (setf (async-thread-yield-parked ct) t)
                    (setf (async-thread-suspended-p ct) t))
                (%block-enter ct)
                (unwind-protect
                     (progn
                       ;; the resume takes the TARGET's mutex; drop our
                       ;; own first (no lock cycle: neither side takes
                       ;; the other's while holding its own)
                       (sb-thread:release-mutex mu)
                       (when resumable-target
                         ;; §242-273: BOTH pairs switch — the
                         ;; then-resume pair unconditionally
                         ;; (§531/§536's block(switch_to=other)),
                         ;; the promote pair on its ready branch
                         ;; (§549: stop_waiting +
                         ;; yield_then_resume(other)) — and the
                         ;; caller proceeds only at the target's
                         ;; NEXT park/exit. The POST-TO rides the
                         ;; resume's locked section: atomic against
                         ;; the target's own handoff claim, no lock
                         ;; cycle (the far-side wake is mutex-free).
                         (async-thread-resume resumable-target ct))
                       ;; M13-9 WAIT-FIRST + CHECK-AFTER (no arm-time
                       ;; return): the switch is a ROUND BARRIER — the
                       ;; caller hands the CPU over ONCE, so a
                       ;; yield-then-promote proceeds only at the
                       ;; target's next park/exit/exit-post, never at
                       ;; its own arm. STEP 4: each of those waits is
                       ;; %park, which releases the store right (fact
                       ;; b: welded to THIS wait, every iteration).
                       (loop
                         (sb-thread:grab-mutex mu)
                         (%park ct mu
                                (if yield-p
                                    (async-thread-waitqueue ct)
                                    (async-thread-suspend-waitqueue ct)))
                         (sb-thread:grab-mutex mu)
                         (let ((done (if yield-p
                                         t
                                         (> (async-thread-wake-count ct)
                                            0))))
                           (setf (async-thread-wake-count ct) 0)
                           (sb-thread:release-mutex mu)
                           (when done (return)))))
                  (sb-thread:grab-mutex mu)
                  (if yield-p
                      (setf (async-thread-yield-parked ct) nil)
                      (setf (async-thread-suspended-p ct) nil))
                  (setf (async-thread-wake-count ct) 0)
                  (sb-thread:release-mutex mu)
                  (%block-exit ct))))
            (list 0))))))
    ;; the still-opaque kinds: the slot exists, the call traps
    ((:thread-new-indirect
      :error-context-new :error-context-debug-message
      :error-context-drop)
     nil)))

;;; ---------------------------------------------------------------
;;; The canon_lower ASYNC core wrapper (CanonicalABI.md 3618-3741):
;;; a host func with the async-lower flat sig. The call starts the
;;; callee's FuncInst, hands it the OnStart/OnResolve pair, and
;;; returns the packed state|(subtaski<<4) — or the eager [RETURNED]
;;; when the callee resolved before returning.

(defun %canon-lower-async-host-func (inst fidx canon)
  "The async LOWER's core wrapper (CanonicalABI.md 3713-3729): spawn
  the callee task via the target lifted func's async lift entry,
  register the Subtask, and return the packed state — RETURNED(2)
  eager when the callee resolved synchronously."
  (let* ((f (aref (comp-instance-funcs inst) fidx))
         (ft (and (lifted-func-p f) (lifted-func-type f)))
         (opts (and (lifted-func-p f) (lifted-func-opts f)))
         (params (and ft (eq (wasmdown.component:ct-kind ft) :func)
                      (coerce (first (wasmdown.component:ct-payload
                                     ft)) 'list)))
         (result (and ft (eq (wasmdown.component:ct-kind ft) :func)
                      (second (wasmdown.component:ct-payload ft))))
         (ft-inst (if (lifted-func-p f) (lifted-func-inst f) inst))
         (flat (wasmdown.component:flatten-functype
                ft (comp-instance-types ft-inst) :lower
                :async-p t))
         (core-params (coerce (car flat) 'list))
         ;; the LOWER's OWN canonopts (the caller's memory/realloc)
         (lopts (wasmdown.component:canonrec-opts canon))
         (mem-opt (wasmdown.component:canon-opt lopts :memory))
         (realloc-opt (wasmdown.component:canon-opt lopts :realloc))
         (enc-opt (wasmdown.component:canon-opt lopts
                                          :string-encoding))
         (memory (and mem-opt
                      (< (second mem-opt)
                         (length (comp-instance-core-memories inst)))
                      (aref (comp-instance-core-memories inst)
                            (second mem-opt))))
         (realloc (and realloc-opt
                       (< (second realloc-opt)
                          (length (comp-instance-core-funcs inst)))
                       (aref (comp-instance-core-funcs inst)
                             (second realloc-opt)))))
    (wasmdown.runtime:make-host-func-instance
     (wasmdown.types:make-wasm-func-type core-params '(:i32))
     (lambda (args)
       (let* ((*handle-caller-inst* inst)
              (subtask (make-async-subtask))
              (cx (make-abi-cx
                   :memory memory
                   :realloc realloc
                   :encoding (%abi-encoding-of enc-opt)
                   ;; the two spaces: INST (the lowering caller) owns
                   ;; the handle table the async-param transfers and
                   ;; lent borrows use; TYPE-INST (FT-INST, the callee)
                   ;; resolves the valtypes' (:idx . n) refs.
                   :inst inst
                   :type-inst ft-inst))
              (ts (loop for p in (or params '())
                        collect (cdr p)))
              (flat-types (loop for vt in ts
                                append (%flat-type-of vt ft-inst)))
              (component-args
                ;; M11b: stream/future PARAMS resolve their i32 against
                ;; the CALLER's table (INST) — the arg-lift cx resolves
                ;; valtype refs against the TARGET (the values-lane
                ;; convention) but the END lives in the caller's
                ;; handles. The flat walk is POSITIONAL: each param
                ;; consumes its flat slots from ARGS; async-value
                ;; params take their single i32 directly.
                (if (> (length flat-types) 4)
                    (let* ((ptr (first args))
                           (tuple (make-ct* :tuple (coerce ts 'vector))))
                      (coerce (%abi-load cx tuple ptr) 'list))
                    (let ((vals '())
                          (ai 0))
                      (loop for vt in ts
                            do (let ((c (%abi-ct vt ft-inst)))
                                 (cond
                                  ((and (wasmdown.component:ct-p c)
                                        (member
                                         (wasmdown.component:ct-kind c)
                                         '(:stream :future)))
                                   (push (%lift-async-end cx
                                                          (nth ai args))
                                         vals)
                                   (incf ai))
                                  ;; M13-LADDER §2406 lift_borrow: the
                                  ;; (borrow $R) param's i32 is an index
                                  ;; in the LOWERING CALLER's table
                                  ;; (INST — the same table the sync
                                  ;; path's own-transfer consumes from,
                                  ;; lifted-func-call 2130): resolve it,
                                  ;; LEND (record the lender on the
                                  ;; subtask, §1446: the handle's
                                  ;; num-lends guards the source against
                                  ;; a premature drop; the count
                                  ;; decrements at the subtask's
                                  ;; deliver_resolve), and pass the REP
                                  ;; to the callee (§2412: lift_borrow
                                  ;; returns h.rep — the raw INDEX was
                                  ;; the M11b gap: dont-drop's core
                                  ;; stashed it and its later R-rep/
                                  ;; R-drop at that index hit the
                                  ;; waitable-set the callee's own
                                  ;; waitable-set.new allocated at the
                                  ;; same slot — drop-cross-task-borrow
                                  ;; ::2/::4's wrong-resource-type)
                                  ((and (wasmdown.component:ct-p c)
                                        (eq (wasmdown.component:ct-kind c)
                                            :borrow)
                                        (integerp
                                         (wasmdown.component:ct-payload c))
                                        (< (wasmdown.component:ct-payload c)
                                           (length
                                            (comp-instance-types ft-inst))))
                                   (let* ((rt-idx
                                            (wasmdown.component:ct-payload
                                             c))
                                          (rt (aref
                                               (comp-instance-types ft-inst)
                                               rt-idx))
                                          ;; M13-lad §2834 lower_borrow,
                                          ;; the LOCAL-SHAPE move (the
                                          ;; parent's (b)): the BORROW
                                          ;; HANDLE is added to the
                                          ;; CALLEE-DEFINING instance's
                                          ;; table (FT-INST — the
                                          ;; lifted func's inst, $D)
                                          ;; rather than the spec's
                                          ;; lowering-caller table ($E):
                                          ;; our runtime's async-callee
                                          ;; rebind (5337's
                                          ;; *HANDLE-CALLER-INST* =
                                          ;; (lifted-func-inst f)) and
                                          ;; the resource arms'
                                          ;; captured-inst resolution
                                          ;; BOTH point at the callee's
                                          ;; defining table, so the
                                          ;; callee's core stashes the
                                          ;; index and its R-rep/R-drop
                                          ;; resolve it there — the
                                          ;; M11b/M12 lanes' local
                                          ;; shape (the spec's 3648
                                          ;; cx.inst = the caller is
                                          ;; the upstream shape our
                                          ;; 5337 diverged from).
                                          ;;
                                          ;; %HANDLE-LEND is the ONE
                                          ;; lend record (§2838/§1446):
                                          ;; it derefs the rep from the
                                          ;; LOWERING CALLER's table
                                          ;; (INST — the source handle
                                          ;; lives there), raises that
                                          ;; source handle's NUM-LENDS,
                                          ;; and records the lend on
                                          ;; the SUBTASK — its
                                          ;; deliver_resolve returns the
                                          ;; count and decrements the
                                          ;; lending task's
                                          ;; NUM-BORROWS.
                                          (rep (%handle-lend inst rt
                                                             (nth ai args)
                                                             subtask))
                                          ;; M16 slice 3b: the same
                                          ;; host-callee rule as the
                                          ;; sync lower — the provider
                                          ;; reads the index off the
                                          ;; dynamic caller's table
                                          (bi (and rep
                                                   (%handle-add
                                                    (if (lifted-func-host f)
                                                        inst ft-inst)
                                                    rt rep nil))))
                                     (push bi vals)
                                     (incf ai)))
                                  (t
                                   (let* ((pflat (%flat-type-of vt
                                                                ft-inst))
                                          (n (length pflat))
                                          (vi (make-array 16
                                                          :fill-pointer 0
                                                          :adjustable t)))
                                     (loop for k below n
                                           do (vector-push-extend
                                               (cons (nth ai args)
                                                     (nth k pflat))
                                               vi)
                                              (incf ai))
                                     (push (first
                                            (%abi-lift-values cx 4 vi
                                                              (list vt)))
                                           vals))))))
                      (nreverse vals))))
              (resolved-state nil)
              (resolved-val nil)
              ;; M14 THE FLIP-TIME CAPTURE (the pack's §3715 reads
              ;; subtask.state AT THE PACK; the reference's timing
              ;; reads STARTED — an EXPLICIT thread of the callee's
              ;; instance (cancel-targeted-resume ::1's keeper:
              ;; resume-later'd inside the callee's core) can resolve
              ;; the task in the STARTED-wait's window via the exit's
              ;; resume-all firing the ready-marked pending spawn;
              ;; the pack then read RETURNED where the fixture pins
              ;; STARTED — the row's FIRST unreachable). Capture the
              ;; STARTED fact AT THE FLIP: once the callee's on_start
              ;; ran, the packed return reports STARTED even if a
              ;; later resolution lands before the pack.
              (started-at-flip nil))
         ;; OnStart (§3651): the callee reads its args (the state
         ;; moves STARTING -> STARTED)
         ;; OnResolve (§3657): the callee's result lowers back into
         ;; the CALLER's [ptr] out-param (the LAST core arg)
         (multiple-value-bind (on-cancel callee-task)
             (%call-async-funcinst f component-args
               :on-start (lambda ()
                           (setf started-at-flip t
                                 (async-subtask-state subtask) 1))
               :on-resolve (lambda (val)
                             (cond
                              ((and (consp val) (eq (car val) :trap))
                               ;; the callee TRAPPED: resolve the
                               ;; subtask; the trap rides flat-
                               ;; results and re-signs at the
                               ;; caller's DELIVERY boundary.
                               ;; M13-tok10 THE RESOLVE-STASH
                               ;; WINDOW (cancel-not-delivered's
                               ;; ~15% flake, 6/40 in the capture
                               ;; loop): the OLD order (resolve(2
                               ;; nil) THEN stash the :trap) left a
                               ;; window where the caller's wset
                               ;; delivery observed the resolved
                               ;; subtask with the flat-results
                               ;; still NIL — the closure's :trap
                               ;; check missed, the SUBTASK event
                               ;; delivered, the caller continued
                               ;; to 42 and the assert_trap failed
                               ;; with 'the action returned'. The
                               ;; stash-first shape INVERTED it
                               ;; (%subtask-resolve's flat-results
                               ;; overwrite clobbered the stash —
                               ;; the trap always lost). THE FIX:
                               ;; resolve WITH the trap — one
                               ;; atomic call sets the state AND
                               ;; the flat-results — no window.
                               (setf resolved-val val)
                               (%subtask-resolve subtask 2 val))
                              ((eq val :cancelled)
                               (setf resolved-state
                                     (if (= (async-subtask-state
                                             subtask)
                                             0)
                                         3 4))
                               (%subtask-resolve subtask
                                                 resolved-state nil))
                              (t
                               ;; lower the results into the
                               ;; caller's out-param
                               (when (and result memory)
                                 (let ((rflat
                                         (%flat-type-of result
                                                        ft-inst)))
                                   ;; M15 MECHANISM 8 — THE
                                   ;; RESULT'S TWO SPACES: the
                                   ;; result's VALTYPE lives in the
                                   ;; CALLEE's type space
                                   ;; (FT-INST), but the out-param
                                   ;; [ptr] and the new stream handle
                                   ;; live in the CALLER (INST). An
                                   ;; async value result lowers the
                                   ;; end into the CALLER's table
                                   ;; (partial-stream-copies: the
                                   ;; freed slot 1 is re-used;
                                   ;; storing through the callee's cx
                                   ;; re-added it to $C at index 3).
                                   (when rflat
                                     (let ((rc (%abi-ct result ft-inst))
                                           (ptr (first (last args)))
                                           (scx (make-abi-cx
                                                 :memory memory
                                                 :realloc realloc
                                                 :encoding
                                                 (%abi-encoding-of enc-opt)
                                                 :inst inst)))
                                       (if (and rc
                                                (member
                                                 (wasmdown.component:ct-kind
                                                  rc)
                                                 '(:stream :future)))
                                           (let ((idx (%lower-async-end
                                                       val scx)))
                                             (%abi-check-align scx ptr 4
                                                               "store")
                                             (%abi-check-span scx ptr 4
                                                              "value")
                                             (%abi-store-int scx idx ptr 4))
                                           (if rc
                                               (%abi-store scx val rc ptr)
                                               (%abi-store cx val result
                                                           ptr)))))))
                               (%subtask-resolve subtask 2 nil)))
                             (setf resolved-val val)
                             ;; M15 MECHANISM 4b — the FIFTH M11a
                             ;; DEFERRAL SITE: the LOWER's own
                             ;; propagation runs on the callee's
                             ;; thread inside the synchronous-resume
                             ;; wait; its resume-all fired the
                             ;; callee instance's ready spawns in the
                             ;; window (::9's $ran check).
                             (if *defer-resume-all*
                                 (push subtask *deferred-resume-all-tasks*)
                                 (async-thread-resume-all))))
           (setf (async-subtask-task subtask) callee-task)
           (setf (async-subtask-on-cancel subtask) on-cancel))
         ;; §3707: the packed return reports STARTING(0) or
         ;; STARTED(1); the fixtures REQUIRE STARTED when the callee
         ;; actually got started (async-calls-sync's 0x21/0x31
         ;; checks) — the callee's OnStart fires on its own thread,
         ;; so WAIT for the STARTING->STARTED transition (or the
         ;; eager resolve) before packing. A sync-typed inline
         ;; callee resolves before reaching here (no wait).
         ;; M14 THE PACK WINDOW: set the spawn-fire suppression —
         ;; the callee's exit resume-all (the M11a propagation,
         ;; running on the CALLEE'S OWN SBCL thread) must not fire
         ;; the resume-later'd explicit threads during the caller's
         ;; observation. A GLOBAL (not a dynamic binding: the
         ;; callee's thread does not inherit our bindings), set
         ;; under the scheduler mutex and cleared after the pack.
         (setf *suppress-spawn-fire* t)
         (let ((eager-p (%subtask-resolved-p subtask))
               (packed-state 0))
          (unless (%subtask-resolved-p subtask)
            (loop
              with cycles = 0
              until (or (%subtask-resolved-p subtask)
                        (/= (async-subtask-state subtask) 0)
                        (async-task-gate-parked
                         (async-subtask-task subtask)))
              ;; M14 THE CALLEE-ONLY SPIN: the STARTED-wait's driver
              ;; grants ONLY the callee task's implicit thread — the
              ;; full resume-all fires the callee instance's OTHER
              ;; ready threads (cancel-targeted-resume ::1's keeper:
              ;; resume-later'd inside the callee's core; the
              ;; priority pass's claim-then-fire ran it inside THIS
              ;; spin, its task.return resolving the subtask before
              ;; the pack read STARTED and before the cancel could
              ;; return BLOCKED — the row's first unreachables). The
              ;; reference observes the callee's on_start via the
              ;; SYNCHRONOUS resume (§3530's thread.resume — no
              ;; tick at all); the callee's own thread is the only
              ;; grant the wait needs.
              ;; STEP 4 PARK SITE 12: the spin's wait is a TARGETED
              ;; handoff — the right goes to the callee alone for one
              ;; very short slice, so the callee's FIRST TURN (its
              ;; OnStart, which flips STARTING -> STARTED) happens
              ;; while its later turns do not: the pack must report
              ;; STARTED (and the fixtures pin it), never the
              ;; callback's resolution.
              do (%right-acquire *current-thread*)
                 (%task-grant-callee (async-subtask-task subtask))
                 (unless (or (%subtask-resolved-p subtask)
                             (/= (async-subtask-state subtask) 0))
                   (%right-handoff-to *current-thread*
                                      (async-task-implicit-thread
                                       (async-subtask-task subtask))
                                      :seconds 0.0002)))
             ;; THE ATOMIC EXIT (session-11's refined candidate): the
             ;; loop's exit and the state read were TWO steps — the
             ;; callee could RESOLVE between them, and the pack then
             ;; observed RETURNED where the fixture pins STARTED
             ;; (reentrance cmd18, ~10/10). Capture the pair TOGETHER
             ;; under the CALLEE TASK's mutex (the same lock
             ;; %task-return holds when it flips resolved-p) and pack
             ;; from the captured values — no second read.
            (let ((ctask (async-subtask-task subtask)))
              (sb-thread:with-recursive-lock ((async-task-mutex ctask))
                (setf packed-state
                      ;; M14 THE FLIP-TIME PACK: report STARTED when
                      ;; the callee's on_start RAN — the reference's
                      ;; pack (§3715) reads the state before the
                      ;; ready-marked EXPLICIT thread's grant; a
                      ;; resolution landing in the STARTED-wait's
                      ;; window (the keeper's task.return) does not
                      ;; change what the CALLER observes at the pack
                      ;; (the resolution delivers at the caller's
                      ;; wait — the SUBTASK event, state RETURNED).
                      (if started-at-flip 1 (async-subtask-state subtask))
                      eager-p (%subtask-resolved-p subtask)))))
         ;; M14: clear the pack window's suppression (the caller's
         ;; observation is complete; the ready spawns run at the
         ;; caller's next blocking op — the fixtures' interleave)
         (setf *suppress-spawn-fire* nil)
         (if eager-p
             ;; the EAGER case (§3714-3717): resolved before return
             (progn
               (%subtask-deliver-resolve subtask)
               (list 2))                   ; Subtask.State.RETURNED
             (let ((subtaski (%handle-add-any inst subtask)))
               ;; the PENDING-EVENT closure (§3720-3725): delivered
               ;; on the next wait/poll — computes the event at
               ;; DELIVERY time
               (setf (async-waitable-pending-event subtask)
                     (lambda ()
                       (when (%subtask-resolved-p subtask)
                         (%subtask-deliver-resolve subtask))
                       ;; a TRAPPED callee re-signs at the delivery
                       ;; boundary (§3584): the caller's wait/poll
                       ;; observes the trap, not a state
                       (let ((fr (async-subtask-flat-results
                                  subtask)))
                         (when (and (consp fr)
                                    (eq (car fr) :trap))
                           (error (cdr fr))))
                       (list 1 subtaski          ; EventCode.SUBTASK
                             (async-subtask-state subtask))))
               (list (logior packed-state
                             (ash subtaski 4)))))))))))

(defun %call-async-funcinst (f component-args &key on-start on-resolve)
  "Start the callee's FuncInst: F is the target lifted func; the
  async-typed ones go through the task machinery (the task runs on
  its own thread and resolves asynchronously — %canon-lift-async
  BLOCKS until resolution, so run it on a FRESH thread); a
  SYNC-typed callee runs inline and resolves before returning.
  Returns the OnCancel closure (the subtask's cancel hook)."
  (let ((ft (and (lifted-func-p f)
                 (lifted-func-type f))))
    (when (and (lifted-func-p f) (lifted-func-host f))
      ;; M16 slice 3b: a HOST provider is a CL closure — it cannot
      ;; block, so the async lift contract reduces to on_start, the
      ;; call, on_resolve (the caller's pack then sees the resolved
      ;; subtask and returns RETURNED eagerly).
      (when on-start (funcall on-start))
      (funcall on-resolve (apply #'lifted-func-call f component-args))
      (return-from %call-async-funcinst (values (lambda () nil) nil)))
    (if (and ft (third (wasmdown.component:ct-payload ft)))
        ;; the callee is ASYNC-TYPED: run the lift on its own thread
        ;; (the task resolves via task.return whenever it's ready)
        (let* ((task (make-async-task
                      :ft ft
                      :opts (lifted-func-opts f)
                      :inst (lifted-func-inst f)
                      :on-start (lambda ()
                                  (when on-start (funcall on-start))
                                  component-args)))
               (task (progn (%tok-stamp-task task) task))
               (th (make-async-thread :task task)))
          (setf (async-task-implicit-thread task) th)
          (%thread-birth th)
                (setf (async-task-on-resolve task)
                (lambda (result)
                  (funcall on-resolve
                           (if (eq result :cancelled)
                               :cancelled result))))
          (%inst-async-fields (lifted-func-inst f))
          (sb-thread:make-thread
           (lambda ()
             (let ((*current-task* task)
                   (*current-thread* th)
                   (*right-self* th)
                   (*handle-caller-inst* (lifted-func-inst f)))
               ;; STEP 4: guest code runs only under the right.
               (%right-acquire th)
               (unwind-protect
                   (handler-case
                       (%async-lift-guest-body
                        task f
                    (make-abi-cx
                     :memory (lifted-func-memory f)
                     :realloc (lifted-func-realloc f)
                     :inst (lifted-func-inst f)))
                 (error (c)
                   ;; Like the invoke-side trap handler: publish the
                   ;; resolution under the right FIRST, then exit —
                   ;; Task.exit_implicit_thread releases the
                   ;; exclusive lock and hands the store right on.
                   (unless (async-task-resolved-p task)
                     (funcall on-resolve (cons :trap c)))
                   (sb-thread:with-recursive-lock
                       ((async-task-mutex task))
                     (setf (async-task-resolved-p task) t)
                     (sb-thread:condition-broadcast
                      (async-task-waitqueue task)))
                   (handler-case
                       (%task-exit-implicit-thread task)
                     (error () nil))
                   ;; M11a wake propagation: the trapped-callee case
                   (async-thread-resume-all)))
                 ;; STEP 4: every exit path drops the ticket and
                 ;; hands the right on.
                 (%right-thread-death th))))
           :name "async-lower-callee")
          ;; the OnCancel: request the task's cancellation (the
          ;; TASK is the second value — the lower stores it in the
          ;; subtask for the STARTED-wait's gate-parked check)
          (values (lambda () (%task-request-cancellation task))
                  task))
        ;; the callee is SYNC-TYPED: an inline call resolves at once
        (let ((res (progn (when on-start (funcall on-start))
                          (apply #'lifted-func-call f component-args))))
          (funcall on-resolve res)
          (values (lambda () nil) nil)))))

;;; Install the entry point into the runner's hook (src/wast.lisp
;;; loads first and calls through *COMPONENT-INSTANTIATOR*).

(defun %store-teardown ()
  "M13-tok9 THE BETWEEN-SCRIPT STORE TEARDOWN (§3552's 'trap and
tear down the whole store' applied at the script boundary): a
script's parked-but-unfinished guest threads (the fixtures'
parked-by-design callbacks, or an oracle-abandoned zombie) stay
registered in the global scheduler after the last command; the
NEXT script's rounds then compete with them for each tick's
single grant (empty-wait::1 deadlocked only in the LANE — its
predecessor's leftovers consumed every grant; alone it passes).
Abandon + unblock + unregister every still-registered thread.

STEP 4 / A3 — THE TEARDOWN RULE: this function sets flags and
broadcasts under *SCHEDULER-MUTEX* and NEVER acquires the store
right and NEVER waits unboundedly (the join below is BOUNDED).
The abandoned parks unwind on their own threads, each dropping its
ticket through %task-exit-implicit-thread -> %right-release-on-exit
(which refuses to act for a thread that is not the caller), so the
next script starts on an empty registry. A script-boundary block
would change the timing the next fixture inherits — the piece-(i)
measurement: an unbounded join there cost 5 extra acs::1 per 30
runs, and the ledger's rule is that this boundary may not block."
  (let ((threads-to-join nil))
    (sb-thread:with-recursive-lock (*scheduler-mutex*)
      (dolist (th (copy-list *scheduler-threads*))
        (setf (async-thread-suspended-p th) nil)
        (setf (async-thread-yield-parked th) nil)
        (setf (async-thread-block-depth th) 0)
        (setf (async-thread-abandoned th) t)
        (when (async-thread-task th)
          (ignore-errors
            (%task-exit-implicit-thread (async-thread-task th))))
        (sb-thread:with-recursive-lock ((async-thread-mutex th))
          (sb-thread:condition-broadcast
           (async-thread-suspend-waitqueue th))
          (sb-thread:condition-broadcast (async-thread-waitqueue th)))
        (when (and (async-thread-native th)
                   (sb-thread:thread-alive-p
                    (async-thread-native th))
                   (not (eq sb-thread:*current-thread*
                            (async-thread-native th))))
          (push (async-thread-native th) threads-to-join)))
      (setf *scheduler-threads* nil)
      ;; STEP 4: no ticket survives a teardown (a recycled thread
      ;; object must not alias a dead entry)
      (clrhash *driver-records*))
    ;; M13-tok19 THE TEARDOWN JOIN — BOUNDED (A3): the abandoned
    ;; threads' unwinds run concurrently with the next fixture's flow
    ;; otherwise (the unwind storm), so give each a bounded grace and
    ;; move on; NEVER block this boundary indefinitely.
    (dolist (n threads-to-join)
      (ignore-errors
        (sb-thread:join-thread n :timeout 0.25 :default :timeout)))))

(setf wasmdown.wast:*store-teardown-hook* #'%store-teardown)

(defun store-teardown ()
  "Tear down the whole store (CanonicalABI.md §3552: 'trap and tear
down the whole store'): abandon + unblock + unregister every still
registered guest thread and join the native threads. The WASI command
runner calls this at its boundary so a `wasi:cli/exit' — a host
condition that unwinds the guest's run task — leaves no parked
thread behind (PLAN.md §9, slice 6 exit semantics). Idempotent: an
empty scheduler is a no-op."
  (%store-teardown))

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
