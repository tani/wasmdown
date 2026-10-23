;;;; Test runner entry: loads the WASMDOWN system plus the test harness,
;;;; then runs every test file. Exits nonzero on any failure (PLAN.md §12.7).

(require :asdf)

;;; Repo root = parent directory of this script (tools/).
(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

(load (merge-pathnames "tests/harness" *root*))
(dolist (f '("tests/numops" "tests/binary" "tests/validate"
             "tests/runtime" "tests/interp" "tests/codegen" "tests/api"
             "tests/text" "tests/wast"))
  (load (merge-pathnames f *root*)))

(multiple-value-bind (passed failed) (wasmdown.test:run-all-tests)
  (declare (ignore passed))
  (when (plusp failed)
    (sb-ext:exit :code 1)))
