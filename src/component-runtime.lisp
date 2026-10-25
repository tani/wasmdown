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
realloc resolved to runtime instances at lift time)."
  (core nil)                         ; func-instance | NIL (opaque)
  (type nil)                         ; the component functype ct | NIL
  (opts nil)
  (memory nil)                       ; memory-instance | NIL
  (realloc nil))                     ; func-instance | NIL

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
                  ;; the runtime type slot: the ct itself
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
       (let* ((args (loop for (name . si) across (instrec-args payload)
                          collect (cons name (%sort-value inst si))))
              (ci (instantiate-component child :with-args args
                                           :outer *outer-instances*)))
         (%push-instance inst ci))))
    (:inline-exports
     (let ((bag (make-hash-table :test #'equal)))
       (setf (gethash :%bag bag) t)
       (loop for (name . si) across (instrec-exports payload)
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
       (%push-sort inst (aliasrec-sort payload)
                   (%instance-export-value source
                                           (aliasrec-name payload)))))
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

(defun %exec-canon (payload inst)
  "One canonrec. A LIFT wraps the target core func (allocating the
lifted func in the component FUNC space; the memory/realloc options
resolve to runtime instances). Everything else (lower, resource-*,
builtins) introduces a CORE FUNC index slot — the executable behavior
is the values-suite slice; here the index exists so later defs
(start/exports/other canon opts) resolve."
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
                          (second realloc-opt)))))))
    ((:lower :resource-new :resource-drop :resource-rep :builtin)
     (%push-core-func inst nil))))

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
;;; to i32/i64/f32/f64; anything else (an (:idx . n) reference, a
;;; compound ct, string/list/...) needs the memory ABI and is NOT
;;; flat-callable in this slice.
(defun %valtype-flat-core (vt)
  (and (keywordp vt)
       (case vt
         ((:bool :u8 :s8 :u16 :s16 :u32 :s32 :char) :i32)
         ((:u64 :s64) :i64)
         (:f32 :f32)
         (:f64 :f64)
         (t nil))))

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
result back. Non-flat shapes signal host-error (values slice)."
  (let* ((ft (lifted-func-type lf))
         (core (lifted-func-core lf)))
    (unless (and core (wasmdown.runtime:func-instance-p core))
      (error 'wasmdown:host-error
             :message "lifted func wraps no core func (values slice)"))
    ;; the flat slice: params/results are the numeric prims. A ct
    ;; whose params are flat passes them in order; NO ct (an empty
    ;; ascription) means [] -> [].
    (let ((flat-params
            (and ft (eq (wasmdown.component:ct-kind ft) :func)
                 (coerce (first (wasmdown.component:ct-payload ft))
                         'list)))
          (flat-result
            (and ft (eq (wasmdown.component:ct-kind ft) :func)
                 (second (wasmdown.component:ct-payload ft)))))
      (let ((core-params
              (mapcar (lambda (p)
                        (%valtype-flat-core (%valtype-prim (cdr p))))
                      (or flat-params '())))
            (core-result
              (and flat-result
                   (%valtype-flat-core (%valtype-prim flat-result)))))
        (when (or (member nil core-params)
                  (and flat-result (null core-result)))
          (error 'wasmdown:host-error
                 :message "canonical ABI for non-flat values is a future slice"))
        (unless (= (length args) (length core-params))
          (error 'wasmdown:host-error
                 :message (format nil
                                  "lifted func expects ~d argument~:p, got ~d"
                                  (length core-params) (length args))))
        (let ((res (apply #'wasmdown.runtime:wasm-call core args)))
          (if core-result
              (first res)
              (values)))))))

;;; Install the entry point into the runner's hook (src/wast.lisp
;;; loads first and calls through *COMPONENT-INSTANTIATOR*).
(setf wasmdown.wast:*component-instantiator* #'instantiate-component)
