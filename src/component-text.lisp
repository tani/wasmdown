;;;; WASMDOWN — the Component Model TEXT parser (PLAN.md §12.7, M7
;;;; slice 2). Produces the SAME component IR the binary reader builds
;;;; (src/component.lisp's comp/compdef records): the text parser walks
;;;; the .wast runner's s-expression token trees (wasmdown.wast's
;;;; %parse-tree nodes and stokens) and the slice-1 validator runs
;;;; UNCHANGED against the result (parse+validate, no instantiation).
;;;;
;;;; Grammar surface (Explainer.md 'Grammar' + the text sugar the
;;;; vendored validation suite pins):
;;;;   * (component <id>? <definition>*) — any order, forward-acyclic;
;;;;   * index sugar: u32 | $id | (sort idx name+) projection aliases;
;;;;     a bare $id that misses the current scope becomes an implicit
;;;;     (alias outer ...) for the four outer-aliasable sorts (core
;;;;     module, core type, component, type);
;;;;   * defvaltypes: record/variant/list[n]/tuple/flags/enum/option/
;;;;     result/map/own/borrow/stream/future + the primitives;
;;;;   * functypes (param "l" t)* (result t)? incl. async; resource
;;;;     types (rep i32|i64) (dtor)?;
;;;;   * component/instance TYPES (declarator lists);
;;;;   * import/export with externnamelit + attributes (implements /
;;;;     version-suffix / external-id) + optional export ascription;
;;;;   * canon lift/lower + resource/concurrency built-ins (both the
;;;;     upright and the inverted (func ...) forms);
;;;;   * core module (via wasmdown.text:parse-wat on the raw span),
;;;;     core type (module types), core instance (instantiate /
;;;;     inline exports / with-argument bags).
;;;;
;;;; Phases: MALFORMED text (a parse/shape error, a duplicate
;;;; attribute, an unresolvable $id, bad UTF-8) signals
;;;; wasmdown:decode-error so assert_malformed rows classify :decode;
;;;; every static rule the fixtures pin as assert_invalid is left to
;;;; the validator.

(defpackage #:wasmdown.component.text
  (:use #:cl #:wasmdown.types #:wasmdown.binary #:wasmdown.component
        #:wasmdown.wast)
  (:export #:parse-component-text))
(in-package #:wasmdown.component.text)

;;; The runner's token tree: a node is (:list start end . children);
;;; stoken accessors are the script-layer export contract (M7 slice 2).

(declaim (inline %text-err))
(defun %text-err (pos control &rest args)
  (error 'wasmdown:decode-error :offset (or pos 0)
         :message (apply #'format nil control args)))

(declaim (inline %nodep %children %tokp))
(defun %nodep (x) (and (consp x) (eq (car x) :list)))
(defun %children (node) (cdddr node))
(defun %tokp (x) (stoken-p x))
(defun %tok-kind (tk) (stoken-kind tk))
(defun %tok-text (tk) (stoken-text tk))
(defun %tok-start (tk) (stoken-start tk))
(defun %node-start (node) (second node))

(defun %nat-token (tk)
  "An unsigned integer index token's value."
  (unless (and (%tokp tk) (member (%tok-kind tk) '(:nat :int)))
    (%text-err (if (%tokp tk) (%tok-start tk) 0)
               "expected a u32 index"))
  (let ((v (parse-integer (remove #\_ (%tok-text tk)))))
    (unless (and (integerp v) (>= v 0))
      (%text-err (%tok-start tk) "expected a u32 index"))
    v))

(defun %string-token (tk)
  "A quoted string token's decoded (byte-accurate) text."
  (unless (and (%tokp tk) (eq (%tok-kind tk) :string))
    (%text-err (if (%tokp tk) (%tok-start tk) 0) "expected a string"))
  (%tok-text tk))

(defun %label-of (tk)
  "A labellit: a string token; strictly UTF-8 validated (labels and
externnames must be real strings)."
  (let ((raw (%string-token tk)))
    (handler-case
        (wasmdown.component::%utf8-decode
         (map '(vector (unsigned-byte 8)) #'char-code raw)
         (%tok-start tk))
      (wasmdown:decode-error (c)
        (declare (ignore c))
        (%text-err (%tok-start tk) "malformed UTF-8 encoding")))
    raw))

;;; ---- the parse scope: one (component ...) level's index spaces ----

(defstruct ct-scope
  (ids (make-hash-table :test #'equal) :type hash-table)
  (outer nil :type list)                ; enclosing ct-scopes, innermost first
  (name "" :type string)
  (n-type 0) (n-func 0) (n-value 0) (n-component 0) (n-instance 0)
  (n-core-type 0) (n-core-module 0) (n-core-instance 0)
  (n-core-func 0) (n-core-memory 0) (n-core-table 0)
  (n-core-global 0)
  (interned nil :type list))            ; inline types, in intern order

(defun %bind (scope id sort idx)
  (when id
    ;; each SORT has its own identifier namespace: reusing $id across
    ;; sorts is legal (instantiation.wast ::51's $libc is both a core
    ;; module and a core instance; resources.wast ::44's $c2 is both
    ;; an instance and a component); only a SAME-sort collision is a
    ;; duplicate. Slice 5 (family D): the table stores EVERY binding
    ;; as a list of (sort . idx) — the old single-entry hash let the
    ;; second sort's bind SHADOW the first, so a later reference
    ;; needing the first sort failed 'unknown'.
    (let* ((table (ct-scope-ids scope))
           (old (gethash id table))
           (bindings (cond
                      ((null old) nil)
                      ((consp (car old)) old)
                      (t (list old)))))
      (when (member sort bindings :key #'car)
        (%text-err 0 "duplicate identifier ~a" id))
      (setf (gethash id table)
            (cons (cons sort idx) bindings))))
  idx)

(defun %lookup (scope id &optional sort)
  "ID's binding in SCOPE, optionally the SORT-matching one. The table
stores a list of (sort . idx) bindings per id (cross-sort reuse is
legal); SORT NIL returns the FIRST binding (innermost = most recent)."
  (let ((hit (gethash id (ct-scope-ids scope))))
    (cond
     ((null hit) nil)
     ((consp (car hit))
      (if sort (find sort hit :key #'car) (car hit)))
     ;; a legacy single (sort . idx) entry
     ((or (not sort) (eq (car hit) sort)) hit)
     (t nil))))


(defun %scope-count (scope sort)
  "The current size of SORT's index space in SCOPE."
  (case sort
    (:type (ct-scope-n-type scope))
    (:func (ct-scope-n-func scope))
    (:value (ct-scope-n-value scope))
    (:component (ct-scope-n-component scope))
    (:instance (ct-scope-n-instance scope))
    (:core-type (ct-scope-n-core-type scope))
    (:core-module (ct-scope-n-core-module scope))
    (:core-instance (ct-scope-n-core-instance scope))
    (:core-func (ct-scope-n-core-func scope))
    (:core-memory (ct-scope-n-core-memory scope))
    (:core-table (ct-scope-n-core-table scope))
    (:core-global (ct-scope-n-core-global scope))
    (t (%text-err 0 "no index space for sort ~a" sort))))

(defun %scope-incf (scope sort)
  (case sort
    (:type (incf (ct-scope-n-type scope)))
    (:func (incf (ct-scope-n-func scope)))
    (:value (incf (ct-scope-n-value scope)))
    (:component (incf (ct-scope-n-component scope)))
    (:instance (incf (ct-scope-n-instance scope)))
    (:core-type (incf (ct-scope-n-core-type scope)))
    (:core-module (incf (ct-scope-n-core-module scope)))
    (:core-instance (incf (ct-scope-n-core-instance scope)))
    (:core-func (incf (ct-scope-n-core-func scope)))
    (:core-memory (incf (ct-scope-n-core-memory scope)))
    (:core-table (incf (ct-scope-n-core-table scope)))
    (:core-global (incf (ct-scope-n-core-global scope)))
    (t (%text-err 0 "no index space for sort ~a" sort))))

(defun %alloc (scope sort &optional id)
  "A new index in SORT's space, binding ID. Returns the index."
  (let ((idx (%scope-count scope sort)))
    (%scope-incf scope sort)
    (%bind scope id sort idx)))

;;; ---- the defs under construction + inline-type interning ----

(defvar *defs* nil
  "The current (component ...)'s defs, most recent first.")

(defvar *component-source* ""
  "The component's raw source (for slicing (core module ...) spans).")

(defvar *type-decl-sink* nil
  "When non-NIL, an implicit outer alias fired while parsing a
component/instance TYPE's declarators lands HERE (as a typedecl
:alias entry of that type) instead of a top-level :alias def — the
de Bruijn count of an alias inside a TYPE declarator skips the TYPE
boundary itself (Explainer: 'the number of enclosing components or
types'), so it must resolve within the type's own declarator
sequence (defined-types ::42/::43's (instance (type $t)) uses inside
(type (component ...))).")

(defvar *intern-scope* nil
  "The FLUSHING scope: inline externtype expressions inside
component/instance TYPE declarators desugar into out-of-line types
of the CONCRETE component (Explainer 'Declarators'), so they intern
HERE (the scope parse-component-text flushes at the end), never in
an intermediate type scope (which nobody flushes — the index would
dangle; annotated-names ::30's (func) inside a doubly-nested type).")

(defun %push-def (kind payload)
  (push (make-compdef :kind kind :payload payload :offset 0) *defs*))

(defun %intern-valtype (scope ct)
  "Intern an inline compound type in SCOPE; the caller flushes the
interned defs when its own definition is complete — UNLESS SCOPE is a
component/instance TYPE's own space (never flushed): then the ct
emits as a :type typedecl of that type via *TYPE-DECL-SINK*, keeping
the returned index valid inside the type's declarator sequence
(the Explainer's desugaring: inline externtype expressions inside
declarators become out-of-line types OF THE TYPE)."
  (let ((idx (ct-scope-n-type scope)))
    (incf (ct-scope-n-type scope))
    (if (and (boundp '*type-decl-sink*)
             (not (eq scope *intern-scope*)))
        (push (make-typedecl :kind :type :payload ct) *type-decl-sink*)
        (push (cons idx ct) (ct-scope-interned scope)))
    idx))

(defun %intern-core-type (scope payload)
  "Intern an inline core type (a moduletype) in SCOPE's CORE type
space. The caller flushes via %flush-interned (the :core-type marker
pairs emit as :core-type defs, in text order)."
  (let ((idx (ct-scope-n-core-type scope)))
    (incf (ct-scope-n-core-type scope))
    (push (cons :core-type (cons idx payload))
          (ct-scope-interned scope))
    idx))

(defun %flush-interned (scope)
  "Emit the interned inline types as defs in intern (text) order:
plain (idx ct) pairs are :type defs; (:core-type idx . payload)
markers are :core-type defs."
  (dolist (pair (reverse (shiftf (ct-scope-interned scope) nil)))
    (if (and (consp pair) (eq (car pair) :core-type))
        (%push-def :core-type (cddr pair))
        (%push-def :type (cdr pair)))))

;;; ---- sorts + index resolution ----

(defparameter *sort-names*
  '(("func" . :func) ("value" . :value) ("type" . :type)
    ("component" . :component) ("instance" . :instance)
    ("module" . :core-module) ("memory" . :core-memory)
    ("table" . :core-table) ("global" . :core-global)
    ("tag" . :core-tag)))

;; The (core X ...) prefix's sort names — X after `core` always means
;; the CORE sort, distinct from the component-level *sort-names* (the
;; bare `func` vs `(core func ...)` distinction the fixtures pin).
(defparameter *core-sort-names*
  '(("module" . :core-module) ("instance" . :core-instance)
    ("func" . :core-func) ("memory" . :core-memory)
    ("table" . :core-table) ("global" . :core-global)
    ("type" . :core-type) ("tag" . :core-tag)))

(defparameter *outer-aliasable*
  '(:core-module :core-type :component :type))

(defun %find-outer (scope id &optional sort)
  "Search this scope then the enclosing ones, innermost-first, for ID.
Returns (values sort idx ct); CT is the de Bruijn count (1 = the
nearest ENCLOSING scope). SORT (family D) selects the sort-matching
binding of a multi-sort $id; NIL takes the first."
  (loop for rest on (cons scope (ct-scope-outer scope))
        for depth from 0
        for s = (first rest)
        for hit = (and s (%lookup s id sort))
        when hit
          do (return (values (car hit) (cdr hit) depth))
        finally (return (values nil nil nil))))

(defun %emit-implicit-outer-alias (scope id sort ct idx)
  "The text format's implicit (alias outer ct idx (sort id)). Inside
a component/instance TYPE's declarators (*TYPE-DECL-SINK* BOUND, even
when its collected value is still empty) the alias lands as a typedecl
of THAT TYPE (its de Bruijn count skips the type boundary), never as
a top-level def."
  (if (boundp '*type-decl-sink*)
      (push (make-typedecl :kind :alias
                           :payload (make-aliasrec
                                     :kind :outer :sort sort :count ct
                                     :target idx :name "" :offset 0))
            *type-decl-sink*)
      (progn (%flush-interned scope)
             (%push-def :alias (make-aliasrec :kind :outer :sort sort
                                             :count ct :target idx
                                             :name "" :offset 0))))
  (%alloc scope sort id))

(defun %parse-sortidx-atom (item sort scope)
  "A bare index in SORT's space: u32 | $id (the latter possibly an
implicit outer alias). Returns (sort . idx)."
  (cond
   ((and (%tokp item) (member (%tok-kind item) '(:nat :int)))
    (cons sort (%nat-token item)))
   ((and (%tokp item) (eq (%tok-kind item) :id))
    (let* ((id (%tok-text item))
           (hit (%lookup scope id sort)))
      (cond
       ((and hit (eq (car hit) sort)) hit)
       (hit
        (%text-err (%tok-start item) "unknown ~a ~a"
                   (string-downcase (string sort)) id))
       (t
        (multiple-value-bind (fsort fidx ct) (%find-outer scope id sort)
          (cond
           ((and fsort (eq fsort sort))
            (cons sort (%emit-implicit-outer-alias
                        scope id sort ct fidx)))
           ((and fsort (member fsort *outer-aliasable*))
            (%text-err (%tok-start item)
                       "outer item `~a` is not a ~a" id
                       (string-downcase (string sort))))
           (fsort
            (%text-err (%tok-start item)
                       "outer item `~a` is not a module, type, or component"
                       id))
           (t
            (%text-err (%tok-start item) "unknown ~a ~a"
                       (string-downcase (string sort)) id))))))))
   (t
    (%text-err (if (%tokp item) (%tok-start item) (%node-start item))
               "expected an index"))))

(defun %parse-projection (sort items scope)
  "The (sort idx name+) inline export-alias chain. Returns (values
sort idx)."
  (unless items (%text-err 0 "expected an index"))
  (unless (cdr items)
    (let ((v (%parse-sortidx-atom (first items) sort scope)))
      (return-from %parse-projection
        (values (car v) (cdr v)))))
  (let* ((inst-sort (if (member sort '(:core-func :core-memory :core-table
                                          :core-global :core-tag :core-module))
                        :core-instance :instance))
         (inst (%parse-sortidx-atom (first items) inst-sort scope))
         (names (cdr items)))
    (unless (and names (every (lambda (n)
                                (and (%tokp n)
                                     (eq (%tok-kind n) :string)))
                              names))
      (%text-err 0 "expected export names"))
    (let ((cur-idx (cdr inst)))
      (dolist (name-tok names)
        (%flush-interned scope)
        ;; each INTERMEDIATE name projects from the (instance) result
        ;; of the preceding alias (Explainer 386-388): the alias's
        ;; sort is INSTANCE and its index the preceding alias's
        ;; instance slot; only the LAST name aliases the final SORT
        ;; (external-visibility ::57's (type $i "i1" "i2") chain)
        (let ((last-p (null (rest (member name-tok names)))))
          (%push-def :alias (make-aliasrec
                             :kind (if (eq inst-sort :core-instance)
                                       :core-export :export)
                             :sort (if last-p sort inst-sort)
                             :index cur-idx
                             :name (%string-token name-tok) :offset 0))
          (setf cur-idx (%alloc scope (if last-p sort inst-sort)
                                nil))))
      (values sort cur-idx))))

(defun %sort-of-name (sname)
  (let ((s (assoc sname *sort-names* :test #'string=)))
    (unless s (%text-err 0 "unknown sort ~a" sname))
    (cdr s)))

(defun %parse-externidx (item sort scope)
  "externidx for SORT: u32 | $id | (sort idx name+) | (core sort
idx name+). Returns (values sort idx)."
  (cond
   ((%tokp item)
    (let ((v (%parse-sortidx-atom item sort scope)))
      (values (car v) (cdr v))))
   ((%nodep item)
    (let* ((items (%children item))
           (head (first items)))
      (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
        (%text-err (%node-start item) "expected an index"))
      (let* ((core-p (and (second items) (%tokp (second items))
                          (string= (%tok-text (second items)) "core")))
             (sname (if core-p
                        (and (third items) (%tokp (third items))
                             (%tok-text (third items)))
                        (%tok-text head)))
             (after (if core-p (cdddr items) (cdr items))))
        (values (car (%parse-projection (%sort-of-name sname)
                                        after scope))
                nil))))
   (t (%text-err (%node-start item) "expected an index"))))

(defun %parse-coreidx (item sort scope)
  "A core sort's index (already unwrapped from any 'core' head by the
caller): u32 | $id | (sort idx name+). Returns (values sort idx)."
  (cond
   ((%tokp item)
    (let ((v (%parse-sortidx-atom item sort scope)))
      (values (car v) (cdr v))))
   ((%nodep item)
    (let* ((items (%children item))
           (head (first items)))
      (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
        (%text-err (%node-start item) "expected a core index"))
      ;; head is the sort name; the items are idx name*
      (%parse-projection (%sort-of-name (%tok-text head))
                         (rest items) scope)))
   (t (%text-err (%node-start item) "expected an index"))))

;;; ---- primitives and value types ----

(defparameter *primval-names*
  '(("bool" . :bool) ("s8" . :s8) ("u8" . :u8) ("s16" . :s16)
    ("u16" . :u16) ("s32" . :s32) ("u32" . :u32) ("s64" . :s64)
    ("u64" . :u64) ("f32" . :f32) ("f64" . :f64) ("char" . :char)
    ("string" . :string) ("error-context" . :error-context)))

(defun %prim (text)
  (cdr (assoc text *primval-names* :test #'string=)))

(defun %parse-valtype (item scope)
  "A valtype: primitive | index | inline compound (interned). Returns
(:idx . n) | a prim keyword."
  (cond
   ((and (%tokp item) (eq (%tok-kind item) :keyword) (%prim (%tok-text item)))
    (%prim (%tok-text item)))
   ((and (%tokp item)
         (or (eq (%tok-kind item) :id)
             (member (%tok-kind item) '(:nat :int))))
    (cons :idx (cdr (%parse-sortidx-atom item :type scope))))
   ((%nodep item)
    (cons :idx (%intern-valtype scope (%parse-defvaltype item scope))))
   (t (%text-err (if (%tokp item) (%tok-start item) (%node-start item))
                 "expected a value type"))))

(defun %parse-typeidx (item scope)
  (cdr (%parse-sortidx-atom item :type scope)))

(defun %head-text (node)
  "The lowercased keyword text of NODE's first child (its head)."
  (let ((h (first (%children node))))
    (and (%tokp h) (%tok-text h))))

(defun %parse-defvaltype (node scope)
  "One defvaltype constructor form. Returns a ct."
  (unless (and (%nodep node) (%children node))
    (%text-err 0 "expected a type constructor"))
  (let* ((items (%children node))
         (head (first items))
         (body (rest items)))
    (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
      (%text-err (%node-start node) "expected a type constructor"))
    (let ((h (%tok-text head)))
      (cond
       ((string= h "record")
        (make-ct :kind :record
                 :payload (coerce
                           (mapcar
                            (lambda (f)
                              (unless (and (%nodep f)
                                           (string= (%head-text f) "field"))
                                (%text-err (%node-start f)
                                           "expected a (field ...)"))
                              (let ((fi (rest (%children f))))
                                (unless (= (length fi) 2)
                                  (%text-err (%node-start f)
                                             "field needs a label and type"))
                                (cons (%label-of (first fi))
                                      (%parse-valtype (second fi) scope))))
                            body)
                           'vector)
                 :offset 0))
       ((string= h "variant")
        (make-ct :kind :variant
                 :payload (coerce
                           (mapcar
                            (lambda (c)
                              (unless (and (%nodep c)
                                           (string= (%head-text c) "case"))
                                (%text-err (%node-start c)
                                           "expected a (case ...)"))
                              (let ((ci (rest (%children c))))
                                (unless (>= (length ci) 1)
                                  (%text-err (%node-start c)
                                             "case needs a label"))
                                (cons (%label-of (first ci))
                                      (when (second ci)
                                        (%parse-valtype (second ci)
                                                        scope)))))
                            body)
                           'vector)
                 :offset 0))
       ((string= h "list")
        (unless (and (>= (length body) 1) (<= (length body) 2))
          (%text-err (%tok-start head) "expected 1 or 2 arguments"))
        (let* ((elem (%parse-valtype (first body) scope))
               (n (when (and (= (length body) 2)
                             (%tokp (second body))
                             (member (%tok-kind (second body))
                                     '(:nat :int)))
                    (%nat-token (second body)))))
          (make-ct :kind :list :payload (cons elem n) :offset 0)))
       ((string= h "tuple")
        (make-ct :kind :tuple
                 :payload (coerce
                           (mapcar (lambda (vt)
                                     (%parse-valtype vt scope))
                                   body)
                           'vector)
                 :offset 0))
       ((string= h "flags")
        (make-ct :kind :flags
                 :payload (coerce (mapcar #'%label-of body) 'vector)
                 :offset 0))
       ((string= h "enum")
        (make-ct :kind :enum
                 :payload (coerce (mapcar #'%label-of body) 'vector)
                 :offset 0))
       ((string= h "option")
        (unless (= (length body) 1)
          (%text-err (%tok-start head) "expected 1 argument"))
        (make-ct :kind :option
                 :payload (%parse-valtype (first body) scope) :offset 0))
       ((string= h "result")
        (let (ok err)
          (dolist (b body)
            (cond
             ((and (%nodep b) (string= (%head-text b) "error"))
              (let ((ei (rest (%children b))))
                (unless (= (length ei) 1)
                  (%text-err (%node-start b)
                             "(error ...) needs exactly one type"))
                (setf err (%parse-valtype (first ei) scope))))
             (t (setf ok (%parse-valtype b scope)))))
          (make-ct :kind :result :payload (cons ok err) :offset 0)))
       ((string= h "map")
        (unless (= (length body) 2)
          (%text-err (%tok-start head) "expected 2 arguments"))
        (make-ct :kind :map
                 :payload (cons (%parse-valtype (first body) scope)
                                (%parse-valtype (second body) scope))
                 :offset 0))
       ((string= h "own")
        (unless (= (length body) 1)
          (%text-err (%tok-start head) "expected 1 argument"))
        (make-ct :kind :own :payload (%parse-typeidx (first body) scope)
                 :offset 0))
       ((string= h "borrow")
        (unless (= (length body) 1)
          (%text-err (%tok-start head) "expected 1 argument"))
        (make-ct :kind :borrow
                 :payload (%parse-typeidx (first body) scope) :offset 0))
       ((string= h "stream")
        (make-ct :kind :stream
                 :payload (when body
                            (%parse-valtype (first body) scope))
                 :offset 0))
       ((string= h "future")
        (make-ct :kind :future
                 :payload (when body
                            (%parse-valtype (first body) scope))
                 :offset 0))
       (t (%text-err (%tok-start head)
                     "unknown type constructor ~a" h))))))

;;; ---- functype / resourcetype ----

(defun %parse-functype (node scope)
  "(func async? (param \"l\" t)* (result t)?) — NODE is the (func ...)
form. Returns a ct."
  (let ((params '()) (results nil) (async nil))
    (dolist (b (rest (%children node)))
      (cond
       ;; an optional $id binder precedes the body — skip it
       ((and (%tokp b) (eq (%tok-kind b) :id)))
       ((and (%nodep b) (string= (%head-text b) "param"))
        (let ((prm (rest (%children b))))
          (unless (= (length prm) 2)
            (%text-err (%node-start b)
                       "param needs a label and a type"))
          (push (cons (%label-of (first prm))
                      (%parse-valtype (second prm) scope))
                params)))
       ((and (%nodep b) (string= (%head-text b) "result"))
        (let ((ri (rest (%children b))))
          (unless (= (length ri) 1)
            (%text-err (%node-start b)
                       "result needs exactly one type"))
          (setf results (%parse-valtype (first ri) scope))))
       ((and (%tokp b) (string= (%tok-text b) "async"))
        (setf async t))
       (t (%text-err (if (%nodep b) (%node-start b) (%tok-start b))
                     "unexpected token in func type"))))
    (make-ct :kind :func
             :payload (list (coerce (nreverse params) 'vector)
                            results async)
             :offset 0)))

(defun %parse-resourcetype (node scope)
  "(resource (rep i32|i64) (dtor ...)?) — NODE is the (resource ...)
form. Returns a ct."
  (let ((rep nil) (dtor nil))
    (dolist (b (rest (%children node)))
      (cond
       ((and (%nodep b) (string= (%head-text b) "rep"))
        (let ((ri (rest (%children b))))
          (unless (and (= (length ri) 1) (%tokp (first ri))
                       (member (%tok-text (first ri)) '("i32" "i64")
                               :test #'string=))
            (%text-err (%node-start b)
                       "expected (rep i32) or (rep i64)"))
          (setf rep (if (string= (%tok-text (first ri)) "i32")
                        :i32 :i64))))
       ((and (%nodep b) (string= (%head-text b) "dtor"))
        (let ((di (rest (%children b))))
          (unless (= (length di) 1)
            (%text-err (%node-start b)
                       "dtor needs a core func index"))
          (setf dtor (%parse-corefunc-idx (first di) scope))))
       (t (%text-err (%node-start b)
                     "unexpected token in resource type"))))
    (unless rep
      (%text-err (%node-start node) "resource needs (rep i32)"))
    (make-ct :kind :resource :payload (cons rep dtor) :offset 0)))

(defun %parse-corefunc-idx (item scope)
  "core:funcidx: u32 | $id | (core func idx name*) | (func idx name*)."
  (multiple-value-bind (s i) (%parse-any-idx item :core-func scope)
    (declare (ignore s))
    i))

(defun %parse-coremem-idx (item scope)
  (multiple-value-bind (s i) (%parse-any-idx item :core-memory scope)
    (declare (ignore s))
    i))

(defun %parse-any-idx (item sort scope)
  "An index that may be bare, (sort ...), or (core sort ...). Returns
(values sort idx)."
  (cond
   ((%tokp item)
    (let ((v (%parse-sortidx-atom item sort scope)))
      (values (car v) (cdr v))))
   ((%nodep item)
    (let* ((items (%children item))
           (head (first items)))
      (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
        (%text-err (%node-start item) "expected an index"))
      (let ((h (%tok-text head)))
        (cond
         ((string= h "core")
          ;; (core X ...): X names the CORE sort — (core func ...)
          ;; is :core-func, NOT the component-level :func.
          (let* ((sname (and (second items) (%tokp (second items))
                             (%tok-text (second items))))
                 (after (cddr items))
                 (core-sort (cdr (assoc sname *core-sort-names*
                                        :test #'string=))))
            (unless core-sort
              (%text-err (%node-start item) "unknown core sort ~a"
                         sname))
            (%parse-projection core-sort after scope)))
         (t
          (%parse-projection (%sort-of-name h) (rest items) scope))))))
   (t (%text-err (%node-start item) "expected an index"))))

;;; ---- externtype (import/export payloads) ----

(defun %parse-externtype (node scope &optional (intern-scope scope))
  "The (sort ...) externtype of an import/export declarator. Returns
(values externtype id): externtype is (sort . payload); ID the $id
bound inside the (sort $id ...) form. SCOPE resolves explicit
(type $t)/index references; INTERN-SCOPE (default SCOPE) is where
INLINE type expressions intern. Inside component/instance TYPE
declarators the two differ: a `(type $t)' reference resolves against
the declarator's own (initially-empty) index space, while an inline
expression desugars into an out-of-line type of the ENCLOSING
component (Explainer 'Declarators': the text format desugars inline
type expressions into out-of-line type definitions — and the binary
format has no per-declarator type space, only the component's)."
  (let* ((items (%children node))
         (head (first items))
         (id (when (and (second items) (%tokp (second items))
                        (eq (%tok-kind (second items)) :id))
               (%tok-text (second items))))
         (body (if id (cddr items) (cdr items))))
    (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
      (%text-err (%node-start node) "expected an externtype"))
    (let ((h (%tok-text head)))
      (cond
       ((string= h "func")
        ;; (func $id? (type $t)? | inline functype)
        (let ((type-use (find-if
                         (lambda (b)
                           (and (%nodep b) (string= (%head-text b)
                                                    "type")))
                         body)))
          (if type-use
              (let ((ti (rest (%children type-use))))
                (unless (= (length ti) 1)
                  (%text-err (%node-start type-use)
                             "type needs exactly one index"))
                (values (cons :func (%parse-typeidx (first ti) scope))
                        id))
              (values (cons :func (%intern-valtype
                                   intern-scope
                                   (%parse-functype node scope)))
                      id))))
       ((string= h "instance")
        (let ((type-use (find-if
                         (lambda (b)
                           (and (%nodep b) (string= (%head-text b)
                                                    "type")))
                         body)))
          (if (and type-use (= (length body) 1))
              ;; (instance (type $t)): an out-of-line type-USE
              ;; (Explainer line 650: externtype ::= (<sort> (type
              ;; <idx>)) for EVERY sort) — resolve $t in SCOPE, never
              ;; a declarator list (defined-types ::41-43/
              ;; ::44-46, external-visibility ::41, annotated-
              ;; names ::30's 'type needs a type body' family)
              (let ((ti (rest (%children type-use))))
                (unless (= (length ti) 1)
                  (%text-err (%node-start type-use)
                             "type needs exactly one index"))
                (values (cons :instance
                             (%parse-typeidx (first ti) scope))
                        id))
              (let ((inner (make-ct-scope
                            :outer (cons scope (ct-scope-outer scope)))))
                (multiple-value-bind (caller-sink-bound caller-sink)
                    (if (boundp '*type-decl-sink*)
                        (values t *type-decl-sink*)
                        (values nil nil))
                  ;; a scratch sink during the DECLS' parse keeps each
                  ;; decl's interning/aliasing local to its own drain;
                  ;; in a CONCRETE (non-type) context there is no
                  ;; caller sink to shadow — the decls intern/alias
                  ;; normally (a nested component's import with an
                  ;; outer reference must emit its alias as a def)
                  (let ((decls
                          (if caller-sink-bound
                              (let* ((*type-decl-sink* nil)
                                     (ds (mapcan
                                          (lambda (d)
                                            ;; SCOPE interns the decls'
                                            ;; inline expressions; INNER
                                            ;; resolves their (type $t)
                                            ;; references (the declarator's
                                            ;; own index space)
                                            (%parse-instancedecl d scope
                                                                 inner))
                                          body)))
                                (nconc ds (nreverse
                                           (shiftf *type-decl-sink* nil))))
                              (mapcan (lambda (d)
                                        (%parse-instancedecl d scope inner))
                                      body))))
                    ;; the arm's OWN interning runs with the CALLER's
                    ;; binding re-established (the enclosing declarator
                    ;; arm drains it in sequence order); the scratch
                    ;; binding above has already popped to the caller
                    ;; state, so no rebind is needed at all
                    (values (cons :instance
                                  (%intern-valtype
                                   intern-scope
                                   (make-ct :kind :instance
                                           :payload (coerce decls
                                                           'vector)
                                           :offset 0)))
                            id)))))))
       ((string= h "component")
        (let ((type-use (find-if
                         (lambda (b)
                           (and (%nodep b) (string= (%head-text b)
                                                    "type")))
                         body)))
          (if (and type-use (= (length body) 1))
              ;; (component (type $t)): an out-of-line type-USE —
              ;; same rule as the instance arm (Explainer line 650)
              (let ((ti (rest (%children type-use))))
                (unless (= (length ti) 1)
                  (%text-err (%node-start type-use)
                             "type needs exactly one index"))
                (values (cons :component
                             (%parse-typeidx (first ti) scope))
                        id))
              (let ((inner (make-ct-scope
                            :outer (cons scope (ct-scope-outer scope)))))
                (let* ((*type-decl-sink* nil)
                       (decls (mapcan (lambda (d)
                                        ;; SCOPE interns (flushing chain);
                                        ;; INNER resolves (type $t)
                                        (%parse-componentdecl d scope
                                                              inner))
                                      body)))
                  ;; trailing sink entries (see the instance arm)
                  (setf decls (nconc decls (nreverse
                                            (shiftf *type-decl-sink*
                                                    nil))))
                  (values (cons :component
                                (%intern-valtype
                                 intern-scope (make-ct :kind :component
                                                     :payload (coerce decls
                                                                     'vector)
                                                     :offset 0)))
                          id))))))
       ((string= h "type")
        (let ((bound (first body)))
          (unless (and (%nodep bound) (%children bound))
            (%text-err (%node-start node)
                       "expected (eq ...) or (sub resource)"))
          (let* ((bi (%children bound))
                 (bh (first bi)))
            (cond
             ((string= (%tok-text bh) "eq")
              (values (cons :type
                           (list :eq (%parse-typeidx (second bi)
                                                     scope)))
                      id))
             ((and (string= (%tok-text bh) "sub")
                   (string= (%tok-text (second bi)) "resource"))
              (values (cons :type :sub-resource) id))
             (t (%text-err (%tok-start bh)
                           "expected (eq ...) or (sub resource)"))))))
       ((string= h "value")
        (%text-err (%node-start node)
                   "value imports/exports are outside this slice"))
       ((string= h "core")
        ;; (core module <id>? (type-use)? <moduletype>?) — the only
        ;; core externtype. The IR payload is a CORE TYPE index (the
        ;; binary reader's %read-externtype #x00 shape): an explicit
        ;; (type $t) use resolves directly, an inline moduletype is
        ;; INTERNED as a fresh core type (Binary.md: the externtype
        ;; refers to a core module type, never carries decls inline).
        (let* ((mhead (first body))
               (mid (when (and (second body) (%tokp (second body))
                               (eq (%tok-kind (second body)) :id))
                      (%tok-text (second body))))
               (mt (if mid (cddr body) (cdr body))))
          (unless (and (%tokp mhead)
                       (string= (%tok-text mhead) "module"))
            (%text-err (%tok-start mhead)
                       "expected (core module ...)"))
          ;; an explicit type-use: (core module $id (type $t))
          (let ((type-use (find-if
                           (lambda (b)
                             (and (%nodep b)
                                  (string= (%head-text b) "type")))
                           mt)))
            (values
             (cons :core-module
                   (cond
                    (type-use
                     (let ((ti (rest (%children type-use))))
                       (unless (= (length ti) 1)
                         (%text-err (%node-start type-use)
                                    "type needs exactly one index"))
                       ;; a core module's type-use resolves in the
                       ;; CORE type space, not the component one
                       ;; (defined-types ::44-46: (core type $t ...)
                       ;; then (core module (type $t)))
                       (cdr (%parse-sortidx-atom (first ti)
                                                 :core-type scope))))
                    (t
                     ;; intern the inline moduletype as a core type
                     (%intern-core-type
                      intern-scope (%parse-moduletype mt scope)))))
             (or mid id)))))
       (t (%text-err (%tok-start head)
                     "unknown external sort ~a" h))))))

;;; ---- declarators (component/instance types) ----

(defun %parse-instancedecl (node scope type-scope)
  "One instancedecl: (core type ...) | (type ...) | (alias ...) |
(export ...). Returns a LIST of typedecls (usually one; an export
with INLINE type expressions or outer-name references prepends its
desugared :type / :alias entries in parse order — the indices the
parser allocated line up with the returned sequence positions).
References inside the decl resolve against TYPE-SCOPE (the
declarator's own index space) — see %parse-deftype-node."
  (let* ((items (%children node))
         (head (first items)))
    (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
      (%text-err (%node-start node) "expected a declaration"))
    (let ((h (%tok-text head)))
      (cond
       ((string= h "core")
        (let ((sh (second items)))
          (unless (and (%tokp sh) (string= (%tok-text sh) "type"))
            (%text-err (%tok-start sh) "expected (core type ...)"))
          (list (make-typedecl :kind :core-type
                               :payload (%parse-core-type-node
                                         (cddr items) scope)))))
       ((string= h "type")
        (multiple-value-bind (id rest) (%split-id (rest items))
          (unless (and rest (= (length rest) 1))
            (%text-err (%node-start node) "type needs a type body"))
          (let ((ct (%parse-deftype-node (first rest) scope
                                          type-scope)))
            (incf (ct-scope-n-type type-scope))
            (%bind type-scope id :type
                   (1- (ct-scope-n-type type-scope)))
            (list (make-typedecl :kind :type :payload ct)))))
       ((string= h "alias")
        (multiple-value-bind (alias id)
            (%parse-alias-node node type-scope)
          (incf (ct-scope-n-type type-scope))
          (%bind type-scope id :type
                 (1- (ct-scope-n-type type-scope)))
          (list (make-typedecl :kind :alias :payload alias))))
       ((string= h "export")
        (multiple-value-bind (name attrs et id)
            ;; TYPE-SCOPE resolves `(type $t)' references against the
            ;; declarator's own index space (binary.wast ::81) and
            ;; INTERNs INLINE externtype expressions into that SAME
            ;; space: the Explainer's desugaring puts the out-of-line
            ;; type in the TYPE's own declarator sequence (interning
            ;; elsewhere would leave the externtype index dangling)
            (%parse-importdecl-parts node type-scope type-scope)
          ;; the desugared :type/:alias entries collected by the sink
          ;; during the parse above (parse order)
          (let ((pre (nreverse (shiftf *type-decl-sink* nil))))
            ;; an exportdecl introduces a new type index (Binary.md
            ;; line 274) AND binds its bind-id at the SORT-MATCHING
            ;; space: an (export "r" (type $r (sub resource))) inside
            ;; an instance type binds $r for the following declarators
            ;; (resources.wast ::28/::29/::30, annotated-names ::30)
            (incf (ct-scope-n-type type-scope))
            (%bind type-scope id (car et)
                   (1- (ct-scope-n-type type-scope)))
            (nconc pre
                   (list (make-typedecl :kind :export
                                        :payload (list name attrs
                                                       et)))))))
       (t (%text-err (%tok-start head)
                     "unexpected declaration ~a" h))))))

(defun %parse-componentdecl (node scope type-scope)
  "One componentdecl: an instancedecl or an (import ...). Returns a
LIST of typedecls (see %parse-instancedecl)."
  (let* ((items (%children node))
         (head (first items)))
    (if (and (%tokp head) (string= (%tok-text head) "import"))
        (multiple-value-bind (name attrs et id)
            ;; TYPE-SCOPE resolves `(type $t)' references (the
            ;; declarator's own space) and INTERNs inline expressions
            ;; into that same space (see the export arm)
            (%parse-importdecl-parts node type-scope type-scope)
          (let ((pre (nreverse (shiftf *type-decl-sink* nil))))
            ;; an import's bind-id introduces a type index inside the
            ;; enclosing component/instance TYPE (Binary.md line 274)
            ;; AND binds at the SORT-MATCHING space: an (instance $i)
            ;; import's $i is an INSTANCE reference for the following
            ;; (alias export $i ...) declarators (outer-alias.wast
            ;; ::13/::14's $i; the Explainer's $fs example)
            (incf (ct-scope-n-type type-scope))
            (%bind type-scope id (car et)
                   (1- (ct-scope-n-type type-scope)))
            (nconc pre
                   (list (make-typedecl :kind :import
                                        :payload (list name attrs
                                                       et))))))
        (%parse-instancedecl node scope type-scope))))

(defun %parse-deftype-node (ty scope &optional type-scope)
  "The BODY of a (type ...): a defvaltype form, (func ...),
(resource ...), (component ...), (instance ...), or a bare primitive
token. TYPE-SCOPE is the declarator's own scope when nested inside a
component/instance TYPE: it is the scope sibling references resolve
against (external-visibility.wast ::35/::40's $Rec) and it carries
the OUTER chain so (alias outer $C ...) resolves through the type
boundary (outer-alias.wast ::0/::10)."
  (let ((parse-scope (or type-scope scope)))
    (cond
     ((%nodep ty)
      (let ((h (%head-text ty)))
        (cond
         ((string= h "func") (%parse-functype ty parse-scope))
         ((string= h "resource")
          (%parse-resourcetype ty parse-scope))
         ((string= h "component")
          (let ((inner (make-ct-scope
                        :outer (cons parse-scope
                                     (ct-scope-outer parse-scope)))))
            (let* ((*type-decl-sink* nil)
                   (decls (mapcan (lambda (d)
                                    (%parse-componentdecl
                                     d parse-scope inner))
                                  (rest (%children ty)))))
              ;; trailing sink entries join in parse order
              (setf decls (nconc decls (nreverse
                                        (shiftf *type-decl-sink* nil))))
              (make-ct :kind :component
                       :payload (coerce decls 'vector)
                       :offset 0))))
         ((string= h "instance")
          (let ((inner (make-ct-scope
                        :outer (cons parse-scope
                                     (ct-scope-outer parse-scope)))))
            (let* ((*type-decl-sink* nil)
                   (decls (mapcan (lambda (d)
                                    (%parse-instancedecl
                                     d parse-scope inner))
                                  (rest (%children ty)))))
              ;; trailing sink entries join in parse order
              (setf decls (nconc decls (nreverse
                                        (shiftf *type-decl-sink* nil))))
              (make-ct :kind :instance
                       :payload (coerce decls 'vector)
                       :offset 0))))
         ((and (%tokp (first (%children ty)))
               (%prim (%tok-text (first (%children ty)))))
          (make-ct :kind :prim
                   :payload (%prim (%tok-text (first (%children ty))))
                   :offset 0))
         (t (%parse-defvaltype ty parse-scope)))))
     ((and (%tokp ty) (%prim (%tok-text ty)))
      (make-ct :kind :prim :payload (%prim (%tok-text ty)) :offset 0))
     (t (%text-err (if (%tokp ty) (%tok-start ty) (%node-start ty))
                  "expected a type body")))))

;;; ---- import/export parts ----

(defun %attr-of (node)
  "One attribute form (implements/version-suffix/external-id): returns
(kind . value) or NIL."
  (let ((h (%head-text node)))
    (cond
     ((string= h "implements")
      (cons :implements (%string-token (second (%children node)))))
     ((string= h "version-suffix")
      (cons :version-suffix (%string-token (second (%children node)))))
     ((string= h "external-id")
      (cons :external-id (%string-token (second (%children node)))))
     (t nil))))

(defun %take-attrs (items)
  "Split leading attribute forms off ITEMS (each at most once).
Returns (values attrs rest)."
  (let ((attrs '()) (rest items))
    (loop while (and rest (%nodep (first rest)) (%attr-of (first rest)))
          do (let ((kind (car (%attr-of (first rest)))))
               (when (assoc kind attrs)
                 (%text-err (%node-start (first rest))
                            "unexpected token: duplicate attribute"))
               (push (%attr-of (first rest)) attrs)
               (setf rest (rest rest))))
    (values (nreverse attrs) rest)))

(defun %parse-importdecl-parts (node scope &optional (intern-scope scope))
  "(import <externnamelit> <attributes> bind-id(<externtype>)).
Returns (values name attrs externtype id). INTERN-SCOPE forwards to
%parse-externtype (inline-expression interning)."
  (let* ((items (%children node))
         (name-tok (second items)))
    (unless (and name-tok (%tokp name-tok)
                 (eq (%tok-kind name-tok) :string))
      (%text-err (%node-start node) "import needs a string name"))
    (let ((name (%string-token name-tok)))
      (multiple-value-bind (attrs rest)
          (%take-attrs (cddr items))
        (unless (and (first rest) (%nodep (first rest)))
          (%text-err (%node-start node) "import needs an externtype"))
        (multiple-value-bind (et id)
            (%parse-externtype (first rest) scope intern-scope)
          (values name attrs et id))))))

(defun %parse-exportdecl-parts (node scope)
  "(export <id>? <externnamelit> <attributes> <externidx>
<externtype>?). Returns (values name attrs sortidx id ascription)."
  (let* ((items (%children node))
         (idx 1)
         (id (when (and (nth idx items) (%tokp (nth idx items))
                        (eq (%tok-kind (nth idx items)) :id))
               (%tok-text (nth idx items)))))
    (when id (incf idx))
    (let ((name-tok (nth idx items)))
      (unless (and name-tok (%tokp name-tok)
                   (eq (%tok-kind name-tok) :string))
        (%text-err (%node-start node) "export needs a string name"))
      (let ((name (%string-token name-tok)))
        (multiple-value-bind (attrs rest)
            (%take-attrs (nthcdr (1+ idx) items))
          (unless (and rest (first rest))
            (%text-err (%node-start node) "export needs an index"))
          (multiple-value-bind (sort sort-idx)
              (%parse-export-externidx (first rest) scope)
            (let ((rest2 (rest rest)))
              (values name attrs (cons sort sort-idx) id
                      (when (and rest2 (%nodep (first rest2)))
                        (multiple-value-bind (et no-id)
                            (%parse-externtype (first rest2) scope)
                          (declare (ignore no-id))
                          et))))))))))

(defun %parse-export-externidx (item scope)
  "The externidx of a concrete export definition. Returns (values
sort idx)."
  (cond
   ((%nodep item)
    (let* ((items (%children item))
           (head (first items)))
      (cond
       ((and (%tokp head) (string= (%tok-text head) "core"))
        ;; (core module idx) etc.
        (let* ((sname (and (second items) (%tokp (second items))
                           (%tok-text (second items))))
               (after (cddr items)))
          (%parse-projection (%sort-of-name sname) after scope)))
       ((and (%tokp head) (assoc (%tok-text head) *sort-names*
                                 :test #'string=))
        (%parse-projection (%sort-of-name (%tok-text head))
                           (rest items) scope))
       (t (%text-err (%tok-start head) "unknown sort")))))
   ((%tokp item)
    (let ((v (%parse-sortidx-atom item :func scope)))
      (values (car v) (cdr v))))
   (t (%text-err (%node-start item) "expected an index"))))

;;; ---- aliases ----

(defun %parse-alias-node (node scope)
  "(alias export idx name (sort id?)) | (alias core export ...) |
(alias outer ct idx (sort id?)). Returns (values aliasrec id)."
  (let* ((items (%children node))
         (head (second items)))
    (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
      (%text-err (%node-start node) "expected an alias kind"))
    (let ((h (%tok-text head)))
      (cond
       ((string= h "export")
        (let* ((idx-item (third items))
               (name-tok (fourth items))
               (sort-node (fifth items)))
          (unless (and name-tok (%tokp name-tok)
                       (eq (%tok-kind name-tok) :string))
            (%text-err (%node-start node)
                       "alias export needs a name"))
          (let* ((inst (cdr (%parse-sortidx-atom idx-item :instance
                                                scope)))
                 (sort+id (%parse-alias-sortdecl sort-node))
                 (sort (car sort+id))
                 (id (cdr sort+id)))
            (values (make-aliasrec :kind :export :sort sort
                                  :index inst
                                  :name (%string-token name-tok)
                                  :offset 0)
                    id))))
       ((string= h "outer")
        (let* ((ct-item (third items))
               (idx-item (fourth items))
               (sort-node (fifth items))
               ;; slice 5 (family D): the sort parses FIRST so the
               ;; target resolution can pick the SORT-matching
               ;; binding when $id names both an instance and a
               ;; component (resources.wast ::44's $c2)
               (sort+id (%parse-alias-sortdecl sort-node))
               (sort (car sort+id))
               (id (cdr sort+id)))
          (multiple-value-bind (ct target)
              (%parse-outer-alias-targets ct-item idx-item scope sort)
            (values
             (make-aliasrec :kind :outer :sort sort :count ct
                            :target target :name "" :offset 0)
             id))))
       ((and (string= h "core") (third items) (%tokp (third items))
             (string= (%tok-text (third items)) "export"))
        (let* ((idx-item (fourth items))
               (name-tok (fifth items))
               (sort-node (sixth items)))
          (unless (and name-tok (%tokp name-tok)
                       (eq (%tok-kind name-tok) :string))
            (%text-err (%node-start node)
                       "alias core export needs a name"))
          (let* ((inst (cdr (%parse-sortidx-atom idx-item
                                                 :core-instance scope)))
                 (sort+id (%parse-alias-sortdecl sort-node))
                 (sort (car sort+id))
                 (id (cdr sort+id)))
            (values (make-aliasrec :kind :core-export :sort sort
                                  :index inst
                                  :name (%string-token name-tok)
                                  :offset 0)
                    id))))
       (t (%text-err (%tok-start head) "unknown alias kind ~a" h))))))

(defun %parse-alias-sortdecl (node)
  "The trailing (<sort> <id>?) of an alias — core sorts appear as
(core func ...). Returns (sort . id)."
  (let* ((items (%children node))
         (head (first items)))
    (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
      (%text-err (%node-start node) "expected a sort declaration"))
    (let* ((core-p (string= (%tok-text head) "core"))
           ;; a CORE sort resolves through *core-sort-names* —
           ;; (alias core export $m "f" (core func $f)) must bind
           ;; $f in the :core-func space, NOT the component-level
           ;; :func space (indicies.wast ::4/::5's 'unknown
           ;; core-func' family)
           (sname (if core-p
                     (%tok-text (second items))
                     (%tok-text head)))
           (id-item (if core-p
                        (third items)
                        (second items)))
           (id (when (and id-item (%tokp id-item)
                          (eq (%tok-kind id-item) :id))
                 (%tok-text id-item))))
      (cons (if core-p
                (or (cdr (assoc sname *core-sort-names*
                                :test #'string=))
                    (%text-err (%node-start node)
                               "unknown core sort ~a" sname))
                (%sort-of-name sname))
            id))))

(defun %parse-outer-alias-targets (ct-item idx-item scope &optional sort)
  "(alias outer ct idx): CT is a u32 or an outer $id (sugar naming the
enclosing scope); IDX a u32 or a $id resolving in THAT scope. Returns
(values ct target). SORT (family D) selects the SORT-matching binding
when IDX's $id names multiple sorts."
  (cond
   ((and (%tokp ct-item) (member (%tok-kind ct-item) '(:nat :int)))
    (let ((ct (%nat-token ct-item)))
      (values ct
              (cond
               ((and (%tokp idx-item)
                     (member (%tok-kind idx-item) '(:nat :int)))
                (%nat-token idx-item))
               ((%tokp idx-item)
                (let ((s (nth ct (ct-scope-outer scope))))
                  (unless s
                    (%text-err (%tok-start ct-item)
                               "unknown outer scope"))
                  (let ((hit (%lookup s (%tok-text idx-item) sort)))
                    (unless hit
                      (%text-err (%tok-start idx-item)
                                 "unknown outer identifier"))
                  (cdr hit))))
               (t (%text-err 0 "expected an index"))))))
   ((and (%tokp ct-item) (eq (%tok-kind ct-item) :id))
    (let* ((cid (%tok-text ct-item))
           (chain (cons scope (ct-scope-outer scope)))
           (depth (position cid chain
                            :key #'ct-scope-name :test #'string=)))
      (unless depth
        (%text-err (%tok-start ct-item)
                   "unknown outer scope ~a" cid))
      ;; de Bruijn convention (Explainer 408-412): count 0 = the
      ;; CURRENT component/type; count N = N boundaries out. The
      ;; named-scope sugar $S therefore resolves to the scope's
      ;; POSITION in the (current . outers) chain — $S naming the
      ;; current scope yields ct 0 (outer-alias.wast ::0's root-level
      ;; named aliases), the first enclosing scope yields 1 (::2's
      ;; cross-boundary alias).
      (let ((s (nth depth chain)))
        (values depth
                (cond
                 ((and (%tokp idx-item)
                       (member (%tok-kind idx-item) '(:nat :int)))
                  (%nat-token idx-item))
                 ((%tokp idx-item)
                  (let ((hit (%lookup s (%tok-text idx-item) sort)))
                    (unless hit
                      (%text-err (%tok-start idx-item)
                                 "unknown outer identifier"))
                    (cdr hit)))
                 (t (%text-err 0 "expected an index")))))))
   (t (%text-err 0 "expected an outer alias target"))))

;;; ---- core types ----

(defun %parse-core-type-node (items scope)
  "The body of a (core type ...): a (module ...) moduletype or a core
functype. Returns the core-type payload."
  (declare (ignore scope))
  (let ((body (first items)))
    (unless (and body (%nodep body))
      (%text-err 0 "expected a core type body"))
    (let ((h (%head-text body)))
      (cond
       ((string= h "module")
        (%parse-moduletype (rest (%children body)) scope))
       ((string= h "func")
        (list :func (%parse-core-params (rest (%children body))
                                        "param")
              (%parse-core-params (rest (%children body))
                                  "result")))
       (t (%text-err (%node-start body)
                     "expected (module ...) or (func ...)"))))))

(defun %parse-core-params (items which)
  "The core (param i32 i32)/(result i32) lists of a functype."
  (let ((out '()))
    (dolist (b items)
      (when (and (%nodep b) (string= (%head-text b) which))
        (setf out (append (mapcar #'%core-valtype-of
                                  (rest (%children b)))
                          out))))
    (reverse out)))

(defun %core-valtype-of (tk)
  (unless (and (%tokp tk) (eq (%tok-kind tk) :keyword))
    (%text-err (%tok-start tk) "expected a core value type"))
  (let ((m (assoc (%tok-text tk)
                  '(("i32" . :i32) ("i64" . :i64) ("f32" . :f32)
                    ("f64" . :f64) ("funcref" . :funcref)
                    ("externref" . :externref) ("v128" . :v128)
                    ("i8" . :i8) ("i16" . :i16))
                  :test #'string=)))
    (unless m
      (%text-err (%tok-start tk) "expected a core value type"))
    (cdr m)))

(defun %parse-moduletype (items scope)
  "A (module <moduledecl>*) moduletype. Returns (:moduletype .
vector-of-typedecls). SCOPE resolves a named (alias outer $C $FT
(type $a)) declarator's outer reference (slice 5, family E)."
  (let ((decls '())
        (ntypes 0))
    (dolist (it items)
      (unless (and (%nodep it) (%children it))
        (%text-err 0 "expected a module declarator"))
      (let* ((mi (%children it))
             (mh (first mi)))
        (unless (and (%tokp mh) (eq (%tok-kind mh) :keyword))
          (%text-err (%node-start it)
                     "expected a module declarator"))
        (let ((h (%tok-text mh)))
          (cond
           ((string= h "import")
            (let* ((mod-tok (second mi)) (field-tok (third mi))
                   (desc (fourth mi)))
              (unless (and (every
                            (lambda (tk)
                              (and tk (%tokp tk)
                                   (eq (%tok-kind tk) :string)))
                            (list mod-tok field-tok))
                           (%nodep desc))
                (%text-err (%node-start it)
                           "bad import declarator"))
              (push (make-typedecl :kind :core-import
                     :payload (list (%string-token mod-tok)
                                    (%string-token field-tok)
                                    (%parse-core-importdesc desc
                                                            scope)))
                    decls)))
           ((string= h "export")
            (let* ((name-tok (second mi)) (desc (third mi)))
              (unless (and name-tok (%tokp name-tok)
                           (eq (%tok-kind name-tok) :string)
                           (%nodep desc))
                (%text-err (%node-start it)
                           "bad export declarator"))
              (push (make-typedecl :kind :core-export
                                   :payload (list
                                             (%string-token name-tok)
                                             (%parse-core-importdesc
                                              desc scope)))
                    decls)))
           ((string= h "type")
            (let* ((id (when (and (second mi) (%tokp (second mi))
                                  (eq (%tok-kind (second mi)) :id))
                         (%tok-text (second mi))))
                   (ty (if id (third mi) (second mi))))
              ;; slice 5 (core-modules ::0): the moduletype's OWN
              ;; type space — a named (type $ft (func)) binds $ft
              ;; for the following (func (type $ft)) type-uses
              (when id (%bind scope id :type ntypes))
              (incf ntypes)
              (unless (and ty (%nodep ty))
                (%text-err (%node-start it) "bad type declarator"))
              (push (make-typedecl :kind :core-type
                     :payload (list :func
                                    (%parse-core-params
                                     (%children ty) "param")
                                    (%parse-core-params
                                     (%children ty) "result")))
                    decls)))
           ((string= h "alias")
            ;; (alias outer ct idx (type id?)) — slice 5 (family E,
            ;; outer-alias.wast ::29): CT/IDX accept the NAMED form
            ;; too ((alias outer $C $FT (type $a))) via the same
            ;; de Bruijn resolution as top-level outer aliases; the
            ;; moduletype's OWN type space binds the trailing id so
            ;; later (func (type $a)) type-uses resolve.
            (let* ((ct-item (third mi))
                   (idx-item (fourth mi))
                   (sort-node (fifth mi))
                   (sort+id (%parse-alias-sortdecl sort-node))
                   (sort (car sort+id))
                   (id (cdr sort+id)))
              (unless (eq sort :type)
                (%text-err (%node-start it) "bad alias declarator"))
              (multiple-value-bind (ct target)
                  ;; NIL sort: the TARGET may be a core type bound
                  ;; under :core-type (the alias's OWN sort is :type —
                  ;; the moduletype alias aliases the type space)
                  (%parse-outer-alias-targets ct-item idx-item scope
                                              nil)
                (when id
                  (%bind scope id :type ntypes))
                (incf ntypes)
                (push (make-typedecl :kind :core-alias
                       :payload (list ct target))
                      decls))))
           (t (%text-err (%tok-start mh)
                         "unexpected module declarator ~a" h))))))
    (cons :moduletype (coerce (nreverse decls) 'vector))))

(defun %parse-core-importdesc (node &optional scope)
  "A core importdesc: (func (type n)?) | (memory n ...) |
(table n reftype) | (global i32 | (mut t)) | (tag (type n)?).
SCOPE resolves a NAMED (type $a) type-use against the moduletype's
own type space (slice 5, family E)."
  (let* ((items (%children node))
         (head (first items)))
    (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
      (%text-err (%node-start node)
                 "expected a core import descriptor"))
    (let ((h (%tok-text head)))
      (cond
       ((string= h "func")
        (let ((tu (second items)))
          (if (and (%nodep tu) (string= (%head-text tu) "type"))
              (let ((tk (second (%children tu))))
                (list :func
                      (if (and (%tokp tk) (member (%tok-kind tk)
                                                   '(:nat :int)))
                          (%nat-token tk)
                          ;; a NAMED (type $a): resolve in the
                          ;; moduletype's own space (core-modules ::0)
                          (let ((hit (and scope
                                         (%lookup scope
                                                  (%tok-text tk)
                                                  :type))))
                            (unless hit
                              (%text-err (%tok-start tk)
                                         "unknown type ~a"
                                         (%tok-text tk)))
                            (cdr hit)))))
              (list :func 0))))
       ((string= h "memory")
        (list :memory (%parse-core-limits (rest items))))
       ((string= h "table")
        ;; (table min max? reftype): the reftype is the LAST item, the
        ;; limits the leading nat items — mirrors %read-core-importdesc
        ;; (a max limits form previously fed the reftype slot →
        ;; 'expected a core value type'; instantiation ::44-::46)
        (list :table (%core-valtype-of (first (last items)))
              (%parse-core-limits (butlast (rest items)))))
       ((string= h "global")
        (let* ((g (second items))
               (mut (and (%nodep g) (string= (%head-text g) "mut"))))
          (list :global
                (if mut (%core-valtype-of
                         (second (%children g)))
                    (%core-valtype-of g))
                mut)))
       ((string= h "tag")
        (list :tag 0))
       (t (%text-err (%tok-start head)
                     "unknown core import descriptor ~a" h))))))

(defun %parse-core-limits (items)
  "Core limits: min | min max | min shared | min max shared — the
shared flag RIDES ALONG (the third list slot) so the validator can
compare it (instantiation ::47's 'mismatch in the shared flag')."
  (let ((min (first items))
        (max (second items))
        (shared (third items))
        (shared2 (third (rest items))))
    (unless (and (%tokp min) (member (%tok-kind min) '(:nat :int)))
      (%text-err 0 "expected a limit"))
    (let ((lo (%nat-token min))
          (hi (when (and (%tokp max) (member (%tok-kind max)
                                             '(:nat :int)))
                (%nat-token max)))
          (shared-p (or (and (%tokp shared)
                             (string= (%tok-text shared) "shared"))
                        (and (%tokp shared2)
                             (string= (%tok-text shared2) "shared")))))
      (list lo hi shared-p))))

;;; ---- canon ----

(defun %parse-canonopts (items scope)
  "canonopt*: string-encoding=X | (memory i) | (realloc i) |
(post-return i) | async | (callback i). Returns a plist."
  (let ((opts '()))
    (dolist (it items)
      (cond
       ((and (%tokp it) (string= (%tok-text it) "async"))
        (push (list :async t) opts))
       ((and (%tokp it) (eq (%tok-kind it) :keyword)
             (>= (length (%tok-text it)) 16)
             (string= (%tok-text it) "string-encoding=" :end1 16))
        (let ((enc (subseq (%tok-text it) 16)))
          (cond
           ((string= enc "utf8")
            (push (list :string-encoding 'utf8) opts))
           ((string= enc "utf16")
            (push (list :string-encoding 'utf16) opts))
           ((string= enc "latin1+utf16")
            (push (list :string-encoding 'latin1+utf16) opts))
           (t (%text-err (%tok-start it)
                         "unknown string encoding ~a" enc)))))
       ((%nodep it)
        (let* ((ci (%children it))
               (ch (first ci)))
          (unless (and (%tokp ch) (eq (%tok-kind ch) :keyword))
            (%text-err (%node-start it) "expected a canon option"))
          (let ((h (%tok-text ch)))
            (cond
             ((string= h "memory")
              (push (list :memory
                          (%parse-coremem-idx (second ci) scope))
                    opts))
             ((string= h "realloc")
              (push (list :realloc
                          (%parse-corefunc-idx (second ci) scope))
                    opts))
             ((string= h "post-return")
              (push (list :post-return
                          (%parse-corefunc-idx (second ci) scope))
                    opts))
             ((string= h "callback")
              (push (list :callback
                          (%parse-corefunc-idx (second ci) scope))
                    opts))
             (t (%text-err (%tok-start ch)
                           "unknown canon option ~a" h))))))
       (t (%text-err 0 "unexpected canon option"))))
    (nreverse opts)))

(defun %canon-kind-kw (h)
  "The built-in kind keyword for a dotted text name (waitable-set.wait
-> :waitable-set-wait)."
  (let ((name (substitute #\- #\. h)))
    (intern (string-upcase name) :keyword)))

(defun %parse-canon-def (items scope &optional inverted-p)
  "(canon lift ...) | (canon lower ...) | (canon <built-in> ...).
Returns a canonrec. INVERTED-P: called from the (core func $id (canon
...)) / (func $id (canon ...)) inverted forms, where the trailing
(core func ...) target node is IMPLICIT — the def itself names the
introduced func (resources.wast ::62, abi.wast ::3)."
  (let ((head (first items)))
    (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
      (%text-err 0 "expected a canon kind"))
    (let ((h (%tok-text head))
          (rest (rest items)))
      (cond
       ((find #\. h :test #'char=)
        ;; built-ins: everything before the final (core func ...) is an
        ;; immediate — except in the inverted form, where ALL items
        ;; are immediates and the target is implicit.
        (if inverted-p
            (make-canonrec
             :kind :builtin
             :imms (%parse-builtin-imms h rest scope)
             :offset 0)
            (let* ((last (car (last items)))
                   ;; strip the kind keyword: it is not an immediate
                   (imm-items (rest (butlast items))))
              (unless (and (%nodep last) (%children last)
                           (string= (%head-text last) "core"))
                (%text-err 0 "expected a (core func ...) target"))
              (make-canonrec
               :kind :builtin
               :imms (%parse-builtin-imms h imm-items scope)
               :offset 0))))
       ((string= h "lift")
        (let* ((f (%parse-corefunc-idx (first rest) scope))
               (after (rest rest))
               (opts '())
               (tail after))
          ;; opts until the (func ...) ascription
          (loop while (and tail
                           (not (and (%nodep (first tail))
                                     (string= (%head-text (first tail))
                                              "func"))))
                do (push (first tail) opts) (setf tail (rest tail)))
          (let* ((opts* (%parse-canonopts (nreverse opts) scope))
                 (type-idx (when tail
                             ;; the (func ...) ascription: inline type
                             (%intern-valtype
                              scope (%parse-functype (first tail)
                                                     scope)))))
            (make-canonrec :kind :lift :func f :opts opts*
                           :typeidx type-idx :offset 0))))
       ((string= h "lower")
        (let* ((f (%parse-corefunc-idx (first rest) scope))
               (after (rest rest))
               (opts '())
               (tail after))
          (loop while (and tail
                           (not (and (%nodep (first tail))
                                     (string= (%head-text (first tail))
                                              "core"))))
                do (push (first tail) opts) (setf tail (rest tail)))
          ;; the inverted (core func $id (canon lower ...)) form has
          ;; no trailing target node — the def itself is the target
          ;; and every remaining item is a canonopt (abi.wast ::3/::6).
          (if inverted-p
              (make-canonrec
               :kind :lower :func f
               :opts (%parse-canonopts after scope)
               :offset 0)
              (progn
                (unless (and tail (%nodep (first tail)))
                  (%text-err 0 "expected (core func ...)"))
                (make-canonrec
                 :kind :lower :func f
                 :opts (%parse-canonopts (nreverse opts) scope)
                 :offset 0)))))
       (t (%text-err (%tok-start head) "unknown canon kind ~a" h))))))

(defparameter *builtin-typeidx-kinds*
  '("resource.new" "resource.drop" "resource.rep"
    "stream.new" "stream.read" "stream.write"
    "stream.cancel-read" "stream.cancel-write"
    "stream.drop-readable" "stream.drop-writable"
    "stream.forward" "future.new" "future.read"
    "future.write" "future.cancel-read"
    "future.cancel-write" "future.drop-readable"
    "future.drop-writable" "future.forward"))

(defun %parse-builtin-imms (h items scope)
  "The immediates of a dotted canon built-in. Returns the imms list."
  (let ((kind (%canon-kind-kw h)))
    (cond
     ((member h *builtin-typeidx-kinds* :test #'string=)
      (list kind
            (%parse-builtin-typeidx (first items) scope)))
     ((member h '("waitable-set.wait" "waitable-set.poll")
              :test #'string=)
      (let ((mem-node (first items)))
        (unless (and (%nodep mem-node)
                     (string= (%head-text mem-node) "memory"))
          (%text-err 0 "expected a memory option"))
        (list kind
              (%parse-coremem-idx (second (%children mem-node))
                                  scope))))
     ((string= h "thread.new-indirect")
      (list kind
            (%parse-builtin-idx (first items) :core-type scope)
            (%parse-builtin-idx (second items) :core-table scope)))
     (t
      (list kind)))))

(defun %parse-builtin-typeidx (item scope)
  "A built-in's type immediate: u32 | $id | (type $t) | (type u32)."
  (cond
   ((and (%tokp item) (member (%tok-kind item) '(:nat :int)))
    (%nat-token item))
   ((%tokp item)
    (cdr (%parse-sortidx-atom item :type scope)))
   ((and (%nodep item) (string= (%head-text item) "type"))
    (%parse-typeidx (second (%children item)) scope))
   (t (%text-err 0 "expected a type index"))))

(defun %parse-builtin-idx (item sort scope)
  (cond
   ((and (%tokp item) (member (%tok-kind item) '(:nat :int)))
    (%nat-token item))
   ((%tokp item)
    (cdr (%parse-sortidx-atom item sort scope)))
   ((%nodep item)
    ;; (core type $t) / (core table $t) / (type $t) shapes
    (let* ((items (%children item))
           (head (first items)))
      (cond
       ((and (%tokp head) (string= (%tok-text head) "core"))
        (let ((idx-item (third items)))
          (if (and (%tokp idx-item)
                   (member (%tok-kind idx-item) '(:nat :int)))
              (%nat-token idx-item)
              (cdr (%parse-sortidx-atom idx-item sort scope)))))
       ((and (%tokp head) (string= (%tok-text head) "type"))
        (%parse-typeidx (second items) scope))
       (t
        (multiple-value-bind (s i)
            (%parse-projection (sort-of-name (%tok-text head))
                               (rest items) scope)
          (declare (ignore s))
          i)))))
   (t (%text-err 0 "expected an index"))))

;;; ---- definitions ----

(defun %split-id (items)
  "Split an optional leading $id off ITEMS."
  (if (and items (%tokp (first items))
           (eq (%tok-kind (first items)) :id))
      (values (%tok-text (first items)) (rest items))
      (values nil items)))

(defun %parse-definition (node scope)
  "Parse one (component ...) definition into *defs* + scope."
  (let* ((items (%children node))
         (head (first items)))
    (unless (and (%tokp head) (eq (%tok-kind head) :keyword))
      (%text-err (%node-start node) "expected a definition"))
    (let ((h (%tok-text head)))
      (cond
       ;; (core module $id? (module ...)) — core wasm text
       ((and (string= h "core") (second items) (%tokp (second items))
             (string= (%tok-text (second items)) "module"))
        (%parse-core-module-def node (cddr items) scope))
       ((and (string= h "core") (second items) (%tokp (second items))
             (string= (%tok-text (second items)) "instance"))
        (%parse-core-instance-def (cddr items) scope))
       ((and (string= h "core") (second items) (%tokp (second items))
             (string= (%tok-text (second items)) "type"))
        (%parse-core-type-def (cddr items) scope))
       ((and (string= h "core") (second items) (%tokp (second items))
             (string= (%tok-text (second items)) "func"))
        ;; (core func $id (canon ...)) — inverted canon form
        (%parse-core-func-def (cddr items) scope))
       ((string= h "component")
        (multiple-value-bind (id rest) (%split-id (rest items))
          (let ((inner (make-ct-scope
                        :outer (cons scope (ct-scope-outer scope))
                        :name (or id ""))))
            (let* ((saved *defs*)
                   (saved-intern *intern-scope*)
                   (*intern-scope* inner))
              (setf *defs* nil)
              (%parse-defs rest inner)
              (%flush-interned inner)
              (let ((comp (make-comp
                           :defs (coerce (nreverse *defs*)
                                         'vector))))
                (setf *defs* saved)
                (setf *intern-scope* saved-intern)
                (%flush-interned scope)
                (%push-def :component comp)
                (%alloc scope :component id))))))
       ((string= h "instance")
        (%parse-instance-def (rest items) scope))
       ((string= h "alias")
        (multiple-value-bind (alias id)
            (%parse-alias-node node scope)
          (%flush-interned scope)
          (%push-def :alias alias)
          (%alloc scope (aliasrec-sort alias) id)))
       ((string= h "type")
        (%parse-type-def (rest items) scope))
       ((string= h "func")
        ;; (func $id (type $t)? (canon lift ...)) — inverted lift
        (%parse-func-def (rest items) scope))
       ((string= h "canon")
        (let ((cr (%parse-canon-def (rest items) scope)))
          (%flush-interned scope)
          (%push-def :canon cr)
          ;; a (canon ...) def introduces one func (lift) or one
          ;; core func (lower); the binary form's index spaces are
          ;; updated by the validator's :canon arm already, so only
          ;; the def itself is pushed here.
          (case (wasmdown.component:canonrec-kind cr)
            ((:lift :lower) (%alloc scope :func nil))
            (t nil))))
       ((string= h "import")
        (multiple-value-bind (name attrs et id)
            (%parse-importdecl-parts node scope)
          (%flush-interned scope)
          (%push-def :import (make-importrec :name name :attrs attrs
                                             :type et))
          (%alloc-def-import scope et id)))
       ((string= h "export")
        (multiple-value-bind (name attrs sortidx id ascription)
            (%parse-exportdecl-parts node scope)
          (%flush-interned scope)
          (%push-def :export (make-exportrec :name name :attrs attrs
                                             :sortidx sortidx
                                             :type ascription
                                             :offset 0))
          (%alloc scope (car sortidx) id)))
       ((string= h "start")
        (%text-err (%node-start node)
                   "start definitions are outside this slice"))
       ((string= h "value")
        (%text-err (%node-start node)
                   "value definitions are outside this slice"))
       (t (%text-err (%tok-start head)
                     "unknown definition ~a" h))))))

(defun %parse-defs (items scope)
  (dolist (n items)
    (unless (%nodep n)
      (%text-err 0 "expected a definition form"))
    (%parse-definition n scope)))

(defun %alloc-def-import (scope et id)
  "An import extends the imported sort's space."
  (case (car et)
    (:func (%alloc scope :func id))
    (:type (%alloc scope :type id))
    (:value (%alloc scope :value id))
    (:component (%alloc scope :component id))
    (:instance (%alloc scope :instance id))
    (:core-module (%alloc scope :core-module id))
    (t (%text-err 0 "unknown import sort ~a" (car et)))))

;;; ---- core module / core instance ----

(defun %parse-core-module-def (node items scope)
  "(core module $id? fields...) — the node IS the module: its fields
parse via wasmdown.text:parse-wat on the raw span with 'module'
substituted for 'core module $id'."
  (let* ((id (when (and items (%tokp (first items))
                        (eq (%tok-kind (first items)) :id))
              (first items)))
         (rest-items (if id (rest items) items))
         (body-start (if rest-items
                         (if (%nodep (first rest-items))
                             (second (first rest-items))
                             (stoken-start (first rest-items)))
                         ;; EMPTY body: the span from the closing
                         ;; paren to itself is EMPTY — the old (1-
                         ;; (third node)) grabbed the char before
                         ;; ')' so an empty (core module $m) built
                         ;; "(module m)" -> 'unknown module field'
                         ;; (instantiation ::35/::56/::78, outer-
                         ;; alias ::16, indicies ::15)
                         (third node)))
         (text (format nil "(module ~a)"
                       (subseq *component-source*
                               body-start (third node))))
         (module (wasmdown.text:parse-wat text)))
    (%flush-interned scope)
    (%push-def :core-module module)
    (%alloc scope :core-module (if id (stoken-text id) nil))))

(defun %parse-core-instance-def (items scope)
  "(core instance $id? (instantiate $m (with ...)*) | exports+)."
  (multiple-value-bind (id rest) (%split-id items)
    (let ((first-item (first rest)))
      (cond
       ((and (%nodep first-item) (string= (%head-text first-item)
                                          "instantiate"))
        (let* ((ii (rest (%children first-item)))
               (m (cdr (%parse-sortidx-atom (first ii) :core-module
                                            scope)))
               (args '()))
          (dolist (w (rest ii))
            (multiple-value-bind (name idx)
                (%parse-with-arg w scope :core-instance)
              (push (cons name idx) args)))
          (%flush-interned scope)
          (%push-def :core-instance
                     (make-coreinstrec :kind :instantiate :index m
                                       :args (coerce (nreverse args)
                                                     'vector)))
          (%alloc scope :core-instance id)))
       (t
        (let ((exports '()))
          (dolist (e rest)
            (let ((pair (%parse-inline-export e scope)))
              (push pair exports)))
          (%flush-interned scope)
          (%push-def :core-instance
                     (make-coreinstrec :kind :inline-exports
                                       :exports (coerce (nreverse
                                                         exports)
                                                        'vector)))
          (%alloc scope :core-instance id)))))))

(defun %parse-inline-export (e scope)
  "One (export \"name\" <core-externidx>) of an inline-export list.
Returns (name sort . idx) — the coreinstrec export shape."
  (unless (and (%nodep e) (string= (%head-text e) "export"))
    (%text-err (%node-start e) "expected (export ...)"))
  (let* ((ei (rest (%children e)))
         (name (%string-token (first ei)))
         (idx-node (second ei)))
    (unless (and idx-node (%nodep idx-node))
      (%text-err (%node-start e) "expected a core index"))
    (multiple-value-bind (s i)
        (%parse-coreidx idx-node :core-func scope)
      (cons name (cons s i)))))

(defun %parse-with-arg (w scope sort)
  "One (with \"name\" (instance ...)) argument — core instances supply
either a $id/index or an inline export bag. Returns (values name
instance-idx)."
  (unless (and (%nodep w) (string= (%head-text w) "with"))
    (%text-err (%node-start w) "expected (with ...)"))
  (let* ((wi (rest (%children w)))
         (name (%string-token (first wi)))
         (inst-node (second wi)))
    (unless (and (%nodep inst-node)
                 (string= (%head-text inst-node) "instance"))
      (%text-err (%node-start inst-node)
                 "expected (instance ...)"))
    (let* ((ii (rest (%children inst-node))))
      (cond
       ((and (= (length ii) 1) (%tokp (first ii)))
        (values name
                (cdr (%parse-sortidx-atom (first ii) sort scope))))
       (t
        ;; inline export bag: synthesize an anonymous instance
        (let ((exports '()))
          (dolist (e ii)
            (push (%parse-inline-export e scope) exports))
          (%flush-interned scope)
          (%push-def (if (eq sort :core-instance) :core-instance
                         :instance)
                     (if (eq sort :core-instance)
                         (make-coreinstrec :kind :inline-exports
                                           :exports (coerce
                                                     (nreverse exports)
                                                     'vector))
                         (make-instrec :kind :inline-exports
                                       :exports (coerce
                                                 (nreverse exports)
                                                 'vector))))
          (values name (%alloc scope sort nil))))))))

(defun %parse-core-type-def (items scope)
  "(core type $id? <rectype | moduletype>)."
  (multiple-value-bind (id rest) (%split-id items)
    (let ((payload (%parse-core-type-node rest scope)))
      (%flush-interned scope)
      (%push-def :core-type payload)
      (%alloc scope :core-type id))))

(defun %parse-core-func-def (items scope)
  "(core func $id (canon ...)) — the inverted canon form: the target
core func is the def itself, so the canon node has NO trailing
(core func ...) node (resources.wast ::69, abi.wast ::3)."
  (multiple-value-bind (id rest) (%split-id items)
    (let ((canon-node (first rest)))
      (unless (and (%nodep canon-node)
                   (string= (%head-text canon-node) "canon"))
        (%text-err (%node-start canon-node)
                   "expected (canon ...)"))
      (let ((canon (%parse-canon-def (rest (%children canon-node))
                                     scope t)))
        (%flush-interned scope)
        (%push-def :canon canon)
        (if (eq (canonrec-kind canon) :lift)
            (%alloc scope :func id)
            (%alloc scope :core-func id))))))

;;; ---- (type ...) definitions ----

(defun %parse-type-def (items scope)
  "(type $id? <deftype>) + the (type $id (export \"n\") <deftype>)
sugar."
  (multiple-value-bind (id rest) (%split-id items)
    (let ((first-body (first rest)))
      (when (and first-body (%nodep first-body)
                 (string= (%head-text first-body) "export"))
        ;; (type $r (export "r1") <deftype>) — export sugar
        (let* ((ex-items (rest (%children first-body)))
               (name (%string-token (first ex-items)))
               (ty (second rest)))
          (unless ty
            (%text-err (%node-start first-body)
                       "type needs a type body"))
          (let ((ct (%parse-deftype-node ty scope)))
            (%flush-interned scope)
            (%push-def :type ct)
            (let ((idx (%alloc scope :type id)))
              (%push-def :export
                         (make-exportrec :name name :attrs nil
                                         :sortidx (cons :type idx)
                                         :type nil :offset 0))
              (%alloc scope :type nil)))
          (return-from %parse-type-def)))
      (unless (and rest (= (length rest) 1))
        (%text-err 0 "type needs a type body"))
      (let ((ct (%parse-deftype-node first-body scope)))
        (%flush-interned scope)
        (%push-def :type ct)
        (%alloc scope :type id)))))

;;; ---- (func ...) inverted canon-lift definitions ----

(defun %parse-func-def (items scope)
  "(func $id (type $t)? <inline sig>? (canon lift ...))."
  (multiple-value-bind (id rest) (%split-id items)
    (let ((type-use (find-if
                     (lambda (b)
                       (and (%nodep b) (string= (%head-text b) "type")))
                     rest))
          (canon-node (find-if
                       (lambda (b)
                         (and (%nodep b)
                              (string= (%head-text b) "canon")))
                       rest)))
      (unless canon-node
        (%text-err 0 "expected (canon lift ...)"))
      (let* ((canon (%parse-canon-def
                     (rest (%children canon-node)) scope t))
             (ft (when type-use
                   (%parse-typeidx
                    (second (%children type-use)) scope))))
        ;; an inline (param ...)/(result ...) ascription on the FUNC
        ;; node IS the lifted functype: intern it so the canonrec's
        ;; typeidx points at it (the validator's lift rules read it).
        (when (eq (canonrec-kind canon) :lift)
          (setf (canonrec-typeidx canon)
                (or ft (canonrec-typeidx canon)
                    (let ((sig-nodes
                            (remove-if
                             (lambda (b)
                               (or (and (%nodep b)
                                        (member (%head-text b)
                                                '("type" "canon")
                                                :test #'string=))
                                   (%tokp b)))
                             rest)))
                      (when sig-nodes
                        (let ((ft-ct (%parse-functype
                                      (list* :list 0 0
                                             (cons (first
                                                    (%children canon-node))
                                                   sig-nodes))
                                      scope)))
                          (%intern-valtype scope ft-ct)))))))
        (%flush-interned scope)
        (%push-def :canon canon)
        (%alloc scope :func id)))))

;;; ---- (instance ...) definitions ----

(defun %parse-instance-def (items scope)
  "(instance $id? (instantiate $c (with ...)*) | exports+ |
(inimport sugar (import \"n\") (type $t)?))."
  (multiple-value-bind (id rest) (%split-id items)
    (let ((import-node (find-if
                        (lambda (b)
                          (and (%nodep b)
                               (string= (%head-text b) "import")))
                        rest)))
      (when import-node
        ;; (instance (import "n") (type $t)? exports*): the import
        ;; sugar; the type-use is (type $t) or an inline instance type
        (let* ((ii (rest (%children import-node)))
               (name (%string-token (first ii)))
               (type-node (second ii))
               (body (remove import-node rest :test #'eq)))
          (let ((et (cond
                     ((and (%nodep type-node)
                           (string= (%head-text type-node) "type"))
                      (cons :instance
                            (%parse-typeidx
                             (second (%children type-node)) scope)))
                     (type-node
                      (cons :instance
                            (%intern-valtype
                             scope
                             (%parse-deftype-node type-node scope))))
                     (t (cons :instance
                              ;; empty instance type
                              (%intern-valtype
                               scope
                               (make-ct :kind :instance
                                        :payload #() :offset 0)))))))
            (declare (ignore body))
            (%flush-interned scope)
            (%push-def :import
                       (make-importrec :name name :attrs nil
                                       :type et))
            (%alloc scope :instance id)
            (return-from %parse-instance-def))))
      ;; (instantiate ...) or inline exports
      (let ((first-item (first rest)))
        (cond
         ((and (%nodep first-item)
               (string= (%head-text first-item) "instantiate"))
          (let* ((ii (rest (%children first-item)))
                 (c (cdr (%parse-sortidx-atom (first ii) :component
                                              scope)))
                 (args '()))
            (dolist (w (rest ii))
              (let* ((wi (rest (%children w)))
                     (name (%string-token (first wi)))
                     (arg (second wi)))
                (push (cons name (%parse-component-with-arg arg scope))
                      args)))
            (%flush-interned scope)
            (%push-def :instance
                       (make-instrec :kind :instantiate :index c
                                     :args (coerce (nreverse args)
                                                   'vector)))
            (%alloc scope :instance id)))
         (t
          (let ((exports '()))
            (dolist (e rest)
              (unless (and (%nodep e)
                           (string= (%head-text e) "export"))
                (%text-err (%node-start e)
                           "expected (export ...)"))
              (multiple-value-bind (name attrs sortidx)
                  (%parse-exportdecl-parts e scope)
                (push (cons (list name attrs) sortidx) exports)))
            (%flush-interned scope)
            (%push-def :instance
                       (make-instrec :kind :inline-exports
                                     :exports (coerce (nreverse
                                                      exports)
                                                     'vector)))
            (%alloc scope :instance id))))))))

(defun %parse-component-with-arg (arg scope)
  "A component (with ...) argument's externidx — (func ...) (instance
...) (type ...) (component ...) (core module ...) or an inline
(instance (export ...)*) bag. Returns the sortidx cons."
  (cond
   ((%nodep arg)
    (let* ((items (%children arg))
           (head (first items)))
      (cond
       ((and (%tokp head) (string= (%tok-text head) "core"))
        (let* ((sname (and (second items) (%tokp (second items))
                           (%tok-text (second items))))
               (after (cddr items)))
          (multiple-value-bind (s i)
              (%parse-projection (%sort-of-name sname) after scope)
            (cons s i))))
       ((and (%tokp head) (string= (%tok-text head) "instance"))
        ;; (instance $s / idx "name"*): a projection chain — the
        ;; one-atom shape is a direct reference, longer chains alias
        ;; out of the instance's exports (indicies.wast ::0:
        ;; (instance $j "k")). Only an ALL-node body is the inline
        ;; export bag.
        (if (every #'%nodep (rest items))
            (let* ((body (rest items))
                   (exports '()))
              (dolist (e body)
                (unless (and (%nodep e)
                             (string= (%head-text e) "export"))
                  (%text-err (%node-start e)
                             "expected (export ...)"))
                (multiple-value-bind (name attrs sortidx)
                    (%parse-exportdecl-parts e scope)
                  ;; the attrs ride along: the validator's bag arm
                  ;; checks implements/version-suffix (version-suffix
                  ;; ::35's with-arg bag export carries a
                  ;; version-suffix; dropping it made the name's bare
                  ;; canonversion fail the semver check)
                  (push (cons (list name attrs) sortidx) exports)))
              (%flush-interned scope)
              (%push-def :instance
                         (make-instrec :kind :inline-exports
                                       :exports (coerce (nreverse
                                                         exports)
                                                        'vector)))
              (cons :instance (%alloc scope :instance nil)))
            (multiple-value-bind (s i)
                (%parse-projection :instance (rest items) scope)
              (cons s i))))
       ((and (%tokp head) (assoc (%tok-text head) *sort-names*
                                 :test #'string=))
        (multiple-value-bind (s i)
            (%parse-projection (%sort-of-name (%tok-text head))
                               (rest items) scope)
          (cons s i)))
       (t (%text-err (%tok-start head) "unknown sort")))))
   (t (%text-err 0 "expected an externidx"))))

;;; ---- the entry point ----

(defun parse-component-text (source)
  "Parse component TEXT (the raw source of a (component ...) form,
parens included) into the wasmdown.component comp IR. Signals
wasmdown:decode-error on malformed text."
  (let* ((tokens (wasmdown.wast::%tokenize source))
         (nodes (wasmdown.wast::%parse-tree tokens source)))
    (unless (= (length nodes) 1)
      (%text-err 0 "expected exactly one (component ...) form"))
    (let ((node (first nodes)))
      (unless (and (%nodep node) (%children node)
                   (%tokp (first (%children node)))
                   (string= (%tok-text (first (%children node)))
                            "component"))
        (%text-err 0 "expected a (component ...) form"))
      (let ((*component-source* source)
            (*defs* nil)
            ;; the root scope is the FLUSHING scope for every inline
            ;; externtype expression, however deeply nested inside
            ;; component/instance TYPE declarators it parses
            (*intern-scope* nil))
        (let ((rest (rest (%children node))))
          ;; a named (component definition ...) form: the definition
          ;; keyword is the runner's named-form marker, not a def
          (when (and rest (%tokp (first rest))
                     (eq (%tok-kind (first rest)) :keyword)
                     (string= (%tok-text (first rest)) "definition"))
            (setf rest (rest rest)))
          (multiple-value-bind (id rest)
              (%split-id rest)
            (let ((scope (make-ct-scope :name (or id ""))))
              (setf *intern-scope* scope)
              (%parse-defs rest scope)
              (%flush-interned scope)
              (make-comp
               :defs (coerce (nreverse *defs*) 'vector)))))))))

;;; Install the entry point into the runner's hook (src/wast.lisp
;;; loads first and calls through *COMPONENT-TEXT-PARSER*).
(setf wasmdown.wast:*component-text-parser* #'parse-component-text)
