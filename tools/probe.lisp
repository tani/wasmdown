;;;; Probe: run component fixture(s) given as argv paths, print outcomes.
(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)
(asdf:load-system :wasmdown)

(dolist (rel (rest sb-ext:*posix-argv*))
  (let* ((path (merge-pathnames rel *root*))
         (results (wasmdown.wast:run-script
                   path :file (file-namestring path))))
    (format t "~&== ~a~%" rel)
    (dolist (r results)
      (format t "  ~a :: ~a [~a] ~a — ~a~%"
              (wasmdown.wast:wast-result-command-index r)
              (wasmdown.wast:wast-result-command-name r)
              (wasmdown.wast:wast-result-outcome r)
              (or (wasmdown.wast:wast-result-detail r) "")
              (wasmdown.wast:wast-result-message r)))))
