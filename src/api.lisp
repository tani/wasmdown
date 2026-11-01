;;;; Public API (docs/design/PLAN.md §11.1). Package WASMDOWN — the user-facing
;;;; surface over binary/validate/runtime/interp.
;;;;
;;;; Phase taxonomy (docs/CONTRACTS.md): read-module signals
;;;; decode-error; validate-module signals validation-error and never
;;;; compiles, allocates or runs anything; instantiate signals
;;;; link-error / instantiation-error and runs the start function
;;;; (traps propagate as wasm-trap); invoke passes traps through
;;;; untouched. Host-side misuse (wrong arity, bad hex input, missing
;;;; backend) is host-error.
;;;;
;;;; Capability notes, honestly:
;;;; - module-bytes returns the CANONICAL re-encoding of the module
;;;;   (wasmdown.binary:encode-module), not the original file bytes:
;;;;   the decoded IR does not retain the source binary. Padded LEB
;;;;   encodings and section padding are lost; decode(module-bytes m)
;;;;   is semantically identical to m.
;;;; - run-string takes a HEX-ENCODED module binary; the text format is a
;;;;   separate front end (wasmdown.text:parse-wat), not a run-string
;;;;   input form.
;;;; - :mode :baseline requires wasmdown.codegen:compile-module-functions
;;;;   to be loaded (src/codegen.lisp); the check is a runtime
;;;;   capability probe, never a stub. The start function and active
;;;;   segment initialization always run through the reference
;;;;   interpreter (make-module-instance); baseline compilation is
;;;;   installed afterwards for every later call.

(in-package #:wasmdown)

;;; ---------------------------------------------------------------
;;; Reading modules.

(defun %read-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8)
                     :direction :input :if-does-not-exist :error)
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun %coerce-octets (source)
  "A strict (vector (unsigned-byte 8)) for SOURCE: passed through when
already octet-typed, otherwise copied element-wise. Literal #(...)
vectors are simple-vectors (element type T), not octet vectors; a bad
element is host-error (wrong input type, not a malformed binary)."
  (if (typep source '(vector (unsigned-byte 8)))
      source
      (let ((v (make-array (length source) :element-type '(unsigned-byte 8))))
        (loop for i below (length source)
              for x = (aref source i)
              unless (typep x '(unsigned-byte 8))
                do (error 'host-error
                          :message (format nil "~s is not an octet" x))
              do (setf (aref v i) x))
        v)))
(defun read-module (source)
  "Decode a module from SOURCE — a pathname, a pathname namestring, or
a (vector (unsigned-byte 8)) — into a wasm-module. Signals decode-error
(with :offset) on malformed input. No validation happens here."
  (etypecase source
    ((or pathname string)
     (wasmdown.binary:decode-module (%read-file-bytes source)))
    (vector
     (wasmdown.binary:decode-module (%coerce-octets source)))))

(defun module-bytes (module)
  "The module as a canonical binary byte vector: a re-encoding via
wasmdown.binary:encode-module, NOT the original input bytes (the IR
does not retain them). decode-module of the result is semantically
identical to MODULE."
  (wasmdown.binary:encode-module module))

(defun validate-module (module)
  "Statically validate MODULE (docs/design/PLAN.md §11.1: no compilation, no host
allocation, no execution). Returns the module on success; signals
validation-error otherwise."
  (wasmdown.validate:validate-module module)
  module)

(defun run-string (hex)
  "Build a module from HEX, a hex-encoded WebAssembly binary
(whitespace is ignored). The string is binary hex, not WebAssembly text;
use wasmdown.text:parse-wat for the text format. Signals host-error on
malformed hex, decode-error on a malformed binary."
  (let ((clean
          (remove-if (lambda (c) (member c '(#\space #\tab #\newline #\return)))
                     hex)))
    (unless (evenp (length clean))
      (error 'host-error
             :message "hex string has an odd number of digits"))
    (let ((v (make-array (ash (length clean) -1)
                         :element-type '(unsigned-byte 8))))
      (loop for i below (length v)
            do (setf (aref v i) (%hex-byte clean (* 2 i))))
      (read-module v))))

(defun %hex-digit (c)
  (let ((d (digit-char-p c 16)))
    (unless d
      (error 'host-error
             :message (format nil "invalid hex digit ~c in module string" c)))
    d))

(defun %hex-byte (s pos)
  (+ (* 16 (%hex-digit (char s pos))) (%hex-digit (char s (1+ pos)))))

;;; ---------------------------------------------------------------
;;; Imports. The import table maps (\"module\" \"field\") to runtime
;;; instance objects; make-imports/imports-set-function build it. Only
;;; function imports are supported in this slice; other import kinds
;;; can be attached by passing a raw alist to INstantiate :IMPORTS.

(defstruct (imports-table (:constructor %make-imports-table)
                          (:predicate imports-table-p))
  "Mutable import table: an alist ((module-name field-name) . value)
of runtime instance objects."
  (entries '() :type list))

(defun make-imports ()
  "A fresh, empty import table (see imports-set-function)."
  (%make-imports-table))

(defun imports-set-function (table module-name field-name lisp-fn
                             param-types result-types)
  "Register LISP-FN as the function import MODULE-NAME.FIELD-NAME in
TABLE. PARAM-TYPES / RESULT-TYPES are lists of value-type keywords
(:i32 :i64 :f32 :f64) used for spec link-time type checking. LISP-FN
is called with the individual wasm arguments and its (multiple) return
values become the results list; when RESULT-TYPES is empty the results
list is forced to (). Returns the host func-instance. Other import
kinds (tables, memories, globals) are not supported by this helper in
this slice: signal host-error / attach raw instances via the alist
form instead."
  (declare (type imports-table table))
  (let* ((ftype (wasmdown.types:make-wasm-func-type param-types result-types))
         (wrapper
           (lambda (args)
             ;; runtime host-fn convention: one list of args in,
             ;; results list out (CONTRACTS.md "Runtime objects").
             (if result-types
                 (multiple-value-list (apply lisp-fn args))
                 (progn (apply lisp-fn args) '()))))
         (instance (wasmdown.runtime:make-host-func-instance ftype wrapper))
         (key (list module-name field-name)))
    (let ((entry (assoc key (imports-table-entries table) :test #'equal)))
      (if entry
          (setf (cdr entry) instance)
          (push (cons key instance) (imports-table-entries table))))
    instance))

;;; ---------------------------------------------------------------
;;; Instantiation.

(defun %imports-alist (imports)
  "Accept an imports-table from MAKE-IMPORTS or a raw runtime alist
(((\"mod\" \"field\") . value) ...)."
  (cond ((null imports) '())
        ((imports-table-p imports) (imports-table-entries imports))
        ((listp imports) imports)
        (t (error 'host-error
                  :message (format nil
                                  "instantiate: :imports must be an import table or an alist, got ~s"
                                  (type-of imports))))))

(defun %baseline-compile (module instance)
  "Compile MODULE's defined functions against INSTANCE using the
baseline backend, or signal host-error when it is not loaded. Runtime
capability probe — no stub."
  (let ((sym (find-symbol "COMPILE-MODULE-FUNCTIONS" :wasmdown.codegen)))
    (if (and sym (fboundp sym))
        (funcall (symbol-function sym) module instance)
        (error 'host-error
               :message "baseline codegen backend not loaded (src/codegen.lisp)"))))

(defun %install-baseline! (module inst)
  "Install compiled entries into MODULE's DEFINED func-instances.

Absolute funcidx = imported-func count + defined index, matching the
module-instance-funcs layout (imports first, then defined). The
func-instance OBJECTS are not replaced: compilation only fills
BACKEND and COMPILED-ENTRY, so exports, table elements, element-segment
refs, ref.func constants and other modules' imports keep pointing at the
same reference. IMPORTED func-instances belong to their defining
instance and are left untouched, so an imported Wasm function keeps its
own backend.

Called from the instantiation PREPARE hook, before active segments and
before the start section; a codegen-error unwinds instantiation, so no
guest code and no segment mutation happened."
  (let* ((compiled (%baseline-compile module inst))
         (nimp (loop for im across (wasmdown.types:wasm-module-imports module)
                     count (eq (wasmdown.types:wasm-import-kind im) :func)))
         (funcs (wasmdown.runtime:module-instance-funcs inst)))
    (unless (= (length compiled)
               (- (length funcs) nimp))
      (error 'host-error
             :message "baseline: compiled entry count does not match defined functions"))
    (loop for i below (length compiled)
          for fi = (wasmdown.runtime:func-instance-kind (svref funcs (+ nimp i)))
          ;; a defined slot always holds a :wasm instance of THIS module
          do (unless (eq fi :wasm)
               (error 'host-error
                      :message "baseline: defined function slot is not a :wasm func-instance"))
             (let ((f (svref funcs (+ nimp i))))
               (setf (wasmdown.runtime:func-instance-backend f) :baseline
                     (wasmdown.runtime:func-instance-compiled-entry f)
                     (svref compiled i))))
    inst))

(defun instantiate (module &key (imports nil) (mode :interpret) (before-start nil))
  "Instantiate MODULE and return the module-instance.

Order: static validation (validation-error) -> import resolution
(link-error) -> allocation -> elem/data segment preparation -> MODE
backend installation -> active segments (instantiation-error) ->
exports -> BEFORE-START -> the Wasm start section (wasm-trap
propagates).

MODE is explicit and applies to the functions THIS module defines:

  :INTERPRET (default) - the reference interpreter executes them.
  :BASELINE           - every defined function is compiled before the
                        start section runs; a function the compiler
                        cannot lower signals codegen-error, so guest
                        code never runs and there is no interpreter
                        fallback. host-error when the backend is not
                        loaded.

Imported Wasm functions keep the backend of their defining instance
(the func-instance object is shared), and the reference objects are
never replaced by compilation.

BEFORE-START, when non-NIL, is called once with the allocated instance
after active segments/exports exist and before the Wasm start section
(the WASI Preview1 memory gate). It works with imported and defined
memory and with both modes."
  (wasmdown.validate:validate-module module)
  (flet ((prepare (inst)
           (when (eq mode :baseline) (%install-baseline! module inst))))
    (let ((inst (wasmdown.runtime:make-module-instance
                 module (%imports-alist imports)
                 :before-start before-start
                 :prepare (and (eq mode :baseline) #'prepare))))
      (ecase mode
        (:interpret inst)
        (:baseline inst)))))

;;; ---------------------------------------------------------------
;;; Invocation and inspection.

(defun invoke (instance export-name &rest args)
  "Call the function exported as EXPORT-NAME (a string) with ARGS
(Lisp-domain values per docs/CONTRACTS.md). Returns the results LIST.
Traps propagate untouched (wasm-trap). Host-side misuse — unknown
export (link-error), non-function export or wrong argument count
(host-error) — is distinguished from guest outcomes."
  (let ((fv (wasmdown.runtime:export-value instance export-name)))
    (unless (wasmdown.runtime:func-instance-p fv)
      (error 'host-error
             :message (format nil "invoke: export ~a is not a function"
                              export-name)))
    (let* ((ty (wasmdown.runtime:func-instance-type fv))
           (nparams (length (wasmdown.types:wasm-func-type-params ty))))
      (unless (= (length args) nparams)
        (error 'host-error
               :message (format nil
                               "invoke: export ~a expects ~d argument~:p, got ~d"
                               export-name nparams (length args))))
      (apply #'wasmdown.runtime:wasm-call fv args))))

(defun instance-export (instance name)
  "The raw instance object exported as NAME: a func-instance,
memory-instance, table-instance or global-instance. Signals link-error
when the module has no such export."
  (wasmdown.runtime:export-value instance name))

;;; ---------------------------------------------------------------
;;; Feature derivation for `inspect` (docs/design/PLAN.md §11.2): which keywords of
;;; the implemented extension surface the module actually uses.

(defun %scan-ops (module)
  "Walk every instruction of MODULE (function bodies, global/elem/data
init and offset expressions). Returns the ops hash set plus the
typed-select and externref flags."
  (let ((ops (make-hash-table :test 'eq))
        (typed-select-p nil)
        (externref-p nil))
    (labels ((visit-expr (expr)
               (loop for in across expr do (visit-instr in)))
             (visit-instr (in)
               (let ((op (wasmdown.types:wasm-instr-op in)))
                 (setf (gethash op ops) t)
                 (when (and (eq op :select)
                            (getf (wasmdown.types:wasm-instr-imms in) :types))
                   (setf typed-select-p t))
                 (visit-expr (wasmdown.types:wasm-instr-body in))
                 (let ((eb (getf (wasmdown.types:wasm-instr-imms in) :else-body)))
                   (when eb (visit-expr eb))))))
      (loop for f across (wasmdown.types:wasm-module-funcs module)
            do (visit-expr (wasmdown.types:wasm-func-body f)))
      (loop for g across (wasmdown.types:wasm-module-globals module)
            do (visit-expr (wasmdown.types:wasm-global-init g)))
      (loop for e across (wasmdown.types:wasm-module-elems module)
            do (let ((mode (wasmdown.types:wasm-elem-mode e)))
                 (when (and (consp mode) (eq (first mode) :active))
                   (visit-expr (third mode))))
               (when (eq (wasmdown.types:wasm-elem-type e) :externref)
                 (setf externref-p t))
               (loop for ex in (wasmdown.types:wasm-elem-inits e)
                     do (visit-expr ex)))
      (loop for d across (wasmdown.types:wasm-module-datas module)
            do (let ((mode (wasmdown.types:wasm-data-mode d)))
                 (when (and (consp mode) (eq (first mode) :active))
                   (visit-expr (third mode)))))
      (loop for im across (wasmdown.types:wasm-module-imports module)
            do (case (wasmdown.types:wasm-import-kind im)
                 (:table
                  (when (eq (wasmdown.types:wasm-table-type
                             (wasmdown.types:wasm-import-type im))
                            :externref)
                    (setf externref-p t)))
                 (:global
                  (when (eq (car (wasmdown.types:wasm-import-type im))
                            :externref)
                    (setf externref-p t)))))
      (loop for tb across (wasmdown.types:wasm-module-tables module)
            do (when (eq (wasmdown.types:wasm-table-type tb) :externref)
                 (setf externref-p t)))
      (values ops typed-select-p externref-p))))

(defun module-features (module)
  "The list of extension keywords the module actually uses, derived
from its shape: :IMPORTS :MEMORY :MULTI-MEMORY :TABLE :GLOBALS :START
:MULTI-VALUE :SIGN-EXTENSION-OPS :SATURATING-FLOAT-TO-INT :BULK-MEMORY
:REFERENCE-TYPES (in that fixed order)."
  (multiple-value-bind (ops typed-select-p externref-p) (%scan-ops module)
    (flet ((op-p (&rest syms) (loop for s in syms thereis (gethash s ops))))
      (let* ((imports (wasmdown.types:wasm-module-imports module))
             (nkind (lambda (kind)
                      (loop for im across imports
                            count (eq (wasmdown.types:wasm-import-kind im) kind))))
             (mems (+ (funcall nkind :memory)
                      (length (wasmdown.types:wasm-module-mems module))))
             (tables (+ (funcall nkind :table)
                        (length (wasmdown.types:wasm-module-tables module))))
             (globals (+ (funcall nkind :global)
                         (length (wasmdown.types:wasm-module-globals module))))
             (passive-p
               (or (loop for e across (wasmdown.types:wasm-module-elems module)
                         thereis (eq (first (wasmdown.types:wasm-elem-mode e))
                                     :passive))
                   (loop for d across (wasmdown.types:wasm-module-datas module)
                         thereis (eq (first (wasmdown.types:wasm-data-mode d))
                                     :passive)))))
        (loop for (feature present-p) in
              `((:imports ,(plusp (length imports)))
                (:memory ,(plusp mems))
                (:multi-memory ,(< 1 mems))
                (:table ,(plusp tables))
                (:globals ,(plusp globals))
                (:start ,(and (wasmdown.types:wasm-module-start module) t))
                (:multi-value
                 ,(loop for ty across (wasmdown.types:wasm-module-types module)
                        thereis (< 1 (length (wasmdown.types:wasm-func-type-results ty)))))
                (:sign-extension-ops
                 ,(op-p :i32.extend8_s :i32.extend16_s :i64.extend8_s
                        :i64.extend16_s :i64.extend32_s))
                (:saturating-float-to-int
                 ,(op-p :i32.trunc_sat_f32_s :i32.trunc_sat_f32_u
                        :i32.trunc_sat_f64_s :i32.trunc_sat_f64_u
                        :i64.trunc_sat_f32_s :i64.trunc_sat_f32_u
                        :i64.trunc_sat_f64_s :i64.trunc_sat_f64_u))
                (:bulk-memory
                 ,(or passive-p
                       (op-p :memory.init :data.drop :memory.copy
                             :memory.fill :table.init :elem.drop :table.copy)))
                (:reference-types
                 ,(or typed-select-p externref-p
                       (op-p :ref.null :ref.is-null :ref.func :table.get
                             :table.set :table.grow :table.size :table.fill))))
              when present-p collect feature)))))
