;;;; Enumerate the runtime's REAL WASI registration surfaces.
;;;;
;;;; Emits tab-separated records on stdout; tools/backend-inventory.py
;;;; turns them into tests/backend-inventory.json and
;;;; docs/conformance/inventory.md. Nothing here reads prose docs:
;;;; every name comes from a defparameter table, a builder table, or a
;;;; package parsed out of the vendored WIT tree.
;;;;
;;;;   P1-SIG   <name>              *p1-signatures* (the import table)
;;;;   P1-IMPL  <name>              *p1-implementations*
;;;;   P2-VEND  <version> <name>    interface parsed from the vendored release
;;;;   P2-BOUND <version> <name>    provider bound by p2-builder-table
;;;;   P3-VEND  <pkg> <name>        interface parsed from wasi-wit-0.3.2
;;;;   P3-BOUND <pkg> <name>        provider bound by %wasi-provider-builders
;;;;   SBCL     <version>
;;;;   COMMIT   <sha or "unknown">

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)
(asdf:load-system :wasmdown)

(defun emit (&rest fields)
  (format t "~{~a~^	~}~%" fields))

(defun commit ()
  "HEAD of the working tree, or NIL when git is unavailable."
  (let ((out (make-string-output-stream)))
    (when (ignore-errors
            (sb-ext:run-program
             "git" (list "-C" (namestring *root*) "rev-parse" "HEAD")
             :search t :output out :error nil :wait t))
      (let ((line (string-trim '(#\Space #\Newline #\Return)
                               (get-output-stream-string out))))
        (when (plusp (length line)) line)))))

(defun stable-p (interface)
  "T unless INTERFACE carries an `@unstable(feature = ...)' annotation."
  (not (find "unstable"
             (mapcar #'wasmdown.wit:wit-annotation-name
                     (wasmdown.wit:wit-interface-annotations interface))
             :test #'string=)))

(defun types-only-p (interface)
  "T when INTERFACE declares neither a function nor a resource."
  (and (null (wasmdown.wit::wit-interface-func-list interface))
       (null (wasmdown.wit::wit-interface-resource-list interface))))

(defun interface-records (package)
  "Sorted (name stable flag types-only flag) for PACKAGE's interfaces."
  (sort (loop for iface in (wasmdown.wit::wit-package-interface-list package)
              collect (list (wasmdown.wit:interface-import-name iface)
                            (if (stable-p iface) "stable" "unstable")
                            (if (types-only-p iface) "types-only" "has-surface")))
        #'string< :key #'car))

;; --- P1 -----------------------------------------------------------------

(emit "SBCL" (lisp-implementation-version))
(let ((c (commit))) (emit "COMMIT" (or c "unknown")))

(let (sigs impls)
  (dolist (row wasmdown.wasi::*p1-signatures*) (push (car row) sigs))
  (dolist (row wasmdown.wasi::*p1-implementations*) (push (car row) impls))
  (dolist (n (sort (remove-duplicates sigs :test #'equal) #'string<))
    (emit "P1-SIG" n))
  (dolist (n (sort (remove-duplicates impls :test #'equal) #'string<))
    (emit "P1-IMPL" n)))

;; --- P2 -----------------------------------------------------------------

(defparameter *p2-versions*
  '("0.2.0" "0.2.1" "0.2.2" "0.2.3" "0.2.4" "0.2.5" "0.2.6"
    "0.2.7" "0.2.8" "0.2.9" "0.2.10" "0.2.11" "0.2.12"))

(defparameter *p2-packages* '("io" "clocks" "random" "cli" "filesystem" "sockets" "http"))

(let ((context (wasmdown.wasi:make-wasi-context)))
  (dolist (version *p2-versions*)
    ;; the vendored surface: every interface parsed out of the release
    (let ((rows '()))
      (dolist (short *p2-packages*)
        (let ((pkg (handler-case (wasmdown.wasi:wasi-package
                                 short :version version)
                     (error () nil))))
          (when pkg (setf rows (append (interface-records pkg) rows)))))
      (dolist (row (sort (remove-duplicates rows :test #'equal) #'string< :key #'car))
        (emit "P2-VEND" version (first row) (second row))))
    ;; what the command world EXPORTS at this version (a world export is
    ;; not an import and therefore never a builder entry)
    (let ((names '()))
      (handler-case
          (dolist (item (wasmdown.wit:wit-world-exports
                         (wasmdown.wasi::preview2-command-world :version version)))
            (let ((target (wasmdown.wit:wit-world-item-target item)))
              (when (wasmdown.wit:wit-interface-p target)
                (push (wasmdown.wit:interface-import-name target) names))))
        (error (e) (format *error-output* "P2 world ~a: ~a~%" version e)))
      (dolist (n (sort (remove-duplicates names :test #'string=) #'string<))
        (emit "P2-EXPORT" version n)))
    ;; what the builder registry actually binds at this exact version
    (let ((names '()))
      (dolist (cell (wasmdown.wasi::p2-builder-table context version))
        (push (car cell) names))
      (dolist (n (sort (remove-duplicates names :test #'string=) #'string<))
        (emit "P2-BOUND" version n)))))

;; --- P3 -----------------------------------------------------------------

(let* ((context (wasmdown.wasi:make-wasi-context))
       (bound (mapcar #'car (wasmdown.wasi::%wasi-provider-builders context))))
  (dolist (short '("clocks" "random" "cli" "filesystem" "sockets" "http"))
    (let ((pkg (handler-case (wasmdown.wasi:wasi-package short)
                 (error () nil))))
      (when pkg
        (dolist (row (interface-records pkg))
          (emit "P3-VEND" short (first row) (second row) (third row))))))
  (dolist (n (sort (remove-duplicates bound :test #'string=) #'string<))
    (emit "P3-BOUND" "all" n))
  ;; the 0.3.2 command world's EXPORTS (run is an export, not an import)
  (handler-case
      (let ((world (wasmdown.wit:resolve-wit-world
                    (wasmdown.wasi:wasi-package "cli") "command"
                    :packages (wasmdown.wasi::%wasi-wit-packages))))
        (dolist (item (wasmdown.wit:wit-world-exports world))
          (let ((target (wasmdown.wit:wit-world-item-target item)))
            (when (wasmdown.wit:wit-interface-p target)
              (emit "P3-EXPORT" "cli"
                    (wasmdown.wit:interface-import-name target))))))
    (error (e) (format *error-output* "P3 world: ~a~%" e))))
