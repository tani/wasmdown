;;;; Test runner entry: loads the WASMDOWN system plus the test harness,
;;;; then runs every test file. Exits nonzero on any failure (docs/design/PLAN.md §12.7).

(require :asdf)

;;; Repo root = parent directory of this script (tools/).
(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

(defun load-tests (file)
  "Load FILE, but fail the run if SBCL reports a compile-time ERROR.
A macroexpansion error in one top-level form only drops that form; the
remaining tests still run and the suite could otherwise report green
after silently shrinking."
  (let ((diagnostics (make-string-output-stream)))
    (let ((*error-output* (make-broadcast-stream *error-output* diagnostics)))
      (load file))
    (let ((text (get-output-stream-string diagnostics)))
      (when (search "caught ERROR" text)
        (format *error-output* "~&test file ~a had compile-time errors; refusing to run a truncated suite~%"
                file)
        (sb-ext:exit :code 1)))))

(load-tests (merge-pathnames "tests/harness" *root*))
(load-tests (merge-pathnames "tools/component-encode" *root*))
(dolist (f '("tests/numops" "tests/binary" "tests/validate"
             "tests/runtime" "tests/interp" "tests/codegen" "tests/api"
             "tests/text" "tests/simd" "tests/wast" "tests/component-pins" "tests/component-store"
             "tests/wit" "tests/wasi" "tests/component-encode" "tests/wasi-preview1" "tests/wasi-p2-fixtures" "tests/wasi-preview2"))
  (load-tests (merge-pathnames f *root*)))

(multiple-value-bind (passed failed) (wasmdown.test:run-all-tests)
  (declare (ignore passed))
  (when (plusp failed)
    (sb-ext:exit :code 1)))
