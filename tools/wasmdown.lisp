#!/usr/bin/env -S sbcl --script
;;;; WASMDOWN CLI launcher (PLAN.md §11.2). Runs the wasmdown.cli
;;;; entry in-process and exits with its stable status:
;;;;   0 ok, 1 guest/compile error, 2 usage error.
;;;;
;;;; Loads the ASDF system from the repository containing this script.
;;;; WASMDOWN_API_MODE selects the execution backend for core code
;;;; (`interpret', the default, or `baseline').
;;;;
;;;; ONE loading path: the ASDF system is the only definition of the
;;;; API and CLI (the old direct `load' of src/api + src/cli re-ran
;;;; obsolete source over the compiled system).

(require :asdf)

(defparameter *script-root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *script-root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

(let ((code (wasmdown.cli:main)))
  #+sbcl (when (numberp code) (sb-ext:exit :code code))
  code)
