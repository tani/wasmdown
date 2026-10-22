#!/usr/bin/env -S sbcl --script
;;;; WASMDOWN CLI launcher (PLAN.md §11.2). Runs the wasmdown.cli
;;;; entry in-process and exits with its stable status:
;;;;   0 ok, 1 guest/compile error, 2 usage error.
;;;;
;;;; Loads the ASDF system from the repository containing this script.
;;;; Optional environment variable WASMDOWN_API_MODE selects the
;;;; execution backend for `run`/`compile`: "baseline" enables the
;;;; codegen backend when loaded (default: interpreter).

(require :asdf)

(defparameter *script-root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *script-root* asdf:*central-registry* :test #'equal)

(defvar *extra-files* '("src/api" "src/cli"))

(asdf:load-system :wasmdown)

;; src/api.lisp and src/cli.lisp are not yet in wasmdown.asd's
;; component list (tracked by the system owner); load them directly
;; when present so this launcher works from a plain checkout.
(dolist (f *extra-files*)
  (let ((p (merge-pathnames (make-pathname :name f :type "lisp")
                            *script-root*)))
    (when (probe-file p)
      (load p))))

(when (find-package :wasmdown.cli)
  (let ((code (wasmdown.cli:main)))
    #+sbcl (when (numberp code) (sb-ext:exit :code code))
    code))
