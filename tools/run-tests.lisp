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
             "tests/text" "tests/simd" "tests/wast" "tests/component-pins" "tests/component-store"
             "tests/wit" "tests/wasi" "tests/wasi-preview1" "tests/wasi-p2-fixtures" "tests/wasi-preview2"))
  (load (merge-pathnames f *root*)))

(multiple-value-bind (passed failed) (wasmdown.test:run-all-tests)
  (declare (ignore passed))
  (when (plusp failed)
    (sb-ext:exit :code 1)))
