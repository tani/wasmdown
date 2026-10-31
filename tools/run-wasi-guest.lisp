;;;; Development-only guest driver for the WASI conformance lanes.
;;;;
;;;; Two jobs:
;;;;   * `--version` prints the loaded `wasmdown` ASDF system version —
;;;;     the runtime version the runner records, read from ASDF rather
;;;;     than from a CLI flag the runtime does not have;
;;;;   * command mode delegates to the real CLI (`wasmdown.cli:main`),
;;;;     so a lane exercises the same entry point a user does.
;;;;
;;;; The HTTP mode of the published plan is NOT here yet: wasi:http is
;;;; not implemented, and a stub must not masquerade as a lane.

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

(let ((first (first sb-ext:*posix-argv*)))
  (declare (ignore first))
  (if (equal (second sb-ext:*posix-argv*) "--version")
      (let* ((sys (asdf:find-system :wasmdown))
             (version (or (asdf:component-version sys) "unversioned")))
        (format t "wasmdown ~a~%" version)
        (finish-output)
        (sb-ext:exit :code 0))
      ;; command mode: the CLI's argv verbatim. Under --script SBCL
      ;; strips the script path, so *POSIX-ARGV* is (program . args).
      (sb-ext:exit :code (or (wasmdown.cli:main (cdr sb-ext:*posix-argv*)) 0))))
