;;;; Threads-proposal spec-script lane: the vendored
;;;; tests/spec-core/threads/*.wast (WebAssembly/threads @
;;;; cc535ada1aa21cfaa3cabf3ac73b89acef78a0a0, test/core/threads)
;;;; through the stateful WAST runner with the threads feature ON and
;;;; REAL SBCL threads: (thread $T [(shared (module $M))] cmd*) spawns
;;;; an sb-thread, (wait $T) joins it and propagates its failures.
;;;;
;;;; The same scripts run through both guest backends (:interpret and
;;;; :compiled) in one process; each run gets a fresh wast-state.
;;;;
;;;; This IS a gate: exit 1 when any command is :fail, :unsupported,
;;;; :timeout or :harness-error, because a threads test that does not
;;;; execute is not a pass. Pass --jsonl to also emit per-command
;;;; wasmdown.test-result.v1 lines.
;;;;
;;;; Usage:
;;;;   sbcl --script tools/run-spec-threads.lisp
;;;;   sbcl --script tools/run-spec-threads.lisp --mode interpret
;;;;   sbcl --script tools/run-spec-threads.lisp --files 'tests/spec-core/threads/thread.wast'
;;;;   sbcl --script tools/run-spec-threads.lisp --jsonl tests/threads-results.jsonl

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

;;; ---------------------------------------------------------------
;;; CLI.

(defparameter *default-files* "tests/spec-core/threads/*.wast")

(defun %parse-args (argv)
  (let ((options (list :files *default-files*
                       :modes '(:interpret :compiled)
                       :jsonl nil)))
    (loop for rest on argv
          for arg = (first rest)
          do (cond
               ((string= arg "--files")
                (unless (second rest) (error "--files needs a value"))
                (setf (getf options :files) (second rest))
                (pop rest))
               ((string= arg "--mode")
                (unless (second rest) (error "--mode needs a value"))
                (let ((mode (intern (string-upcase (second rest)) :keyword)))
                  (setf (getf options :modes)
                        (case mode
                          (:both '(:interpret :compiled))
                          ((:interpret :compiled) (list mode))
                          (t (error "unknown --mode ~a (interpret|compiled|both)"
                                    (second rest)))))
                  (pop rest)))
               ((string= arg "--jsonl")
                (unless (second rest) (error "--jsonl needs a value"))
                (setf (getf options :jsonl) (second rest))
                (pop rest))
               ((string= arg "--")
                (return))
               ((and (> (length arg) 1) (char= (char arg 0) #\-))
                (error "unknown option ~a" arg))))
    options))

(defun %resolve-files (spec)
  "Expand SPEC (a glob or comma-separated globs, relative to the repo
root) into sorted pathnames."
  (let ((files '()))
    (dolist (part (uiop:split-string spec :separator ","))
      (let ((part (string-trim " " part)))
        (when (plusp (length part))
          (dolist (f (directory (merge-pathnames part *root*)))
            (pushnew f files :test #'equal)))))
    (setf files (sort files #'string< :key #'namestring))
    (when (null files)
      (error "no .wast files match ~s" spec))
    files))

;;; ---------------------------------------------------------------
;;; One run.

(defun %mode-label (mode) (string-downcase (string mode)))

(defun %run-file (file mode stream)
  "Run FILE with the threads feature enabled under MODE. Returns the
result list (also streamed when STREAM is non-nil)."
  (let ((wasmdown.wast::*instantiate-mode* mode)
        (wasmdown.wast:*jsonl-suite*
          (format nil "threads-spec-~a" (%mode-label mode))))
    (wasmdown.wast::run-script file
                               :file (file-namestring file)
                               :stream stream
                               :threads t)))

(defun %counts (results)
  (let ((table (make-hash-table :test #'eq)))
    (dolist (r results)
      (incf (gethash (wasmdown.wast:wast-result-outcome r) table 0)))
    table))

(defun %fmt (table outcome) (or (gethash outcome table) 0))

(defun %threads-spawned (results)
  (count :thread results :key #'wasmdown.wast:wast-result-command-name))

(defun %fail-detail (results)
  "The non-pass results that make this a gate failure."
  (remove-if (lambda (r)
               (eq (wasmdown.wast:wast-result-outcome r) :pass))
             results))

(defun %run-mode (files mode jsonl)
  (let ((results '()))
    (flet ((run (stream)
             (dolist (file files)
               (setf results
                     (append results (%run-file file mode stream))))))
      (if jsonl
          (with-open-file (stream jsonl
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create
                                  :external-format :utf-8)
            (run stream))
          (run nil)))
    (format t "~&mode ~a:~%" (%mode-label mode))
    (format t "~2@a| File | pass | fail | unsupported | other |~%"
            "#")
    (dolist (file files)
      (let* ((name (file-namestring file))
             (rs (remove-if-not
                  (lambda (r)
                    (string= (wasmdown.wast:wast-result-file r) name))
                  results))
             (tbl (%counts rs)))
        (format t "| ~a | ~d | ~d | ~d | ~d |~%"
                name
                (%fmt tbl :pass) (%fmt tbl :fail) (%fmt tbl :unsupported)
                (+ (%fmt tbl :skip) (%fmt tbl :timeout)
                   (%fmt tbl :harness-error)))))
    (let ((tbl (%counts results)))
      (format t "totals ~a: commands ~d, pass ~d, fail ~d, unsupported ~d, ~
timeout ~d, harness-error ~d, skip ~d; threads spawned ~d~%"
              (%mode-label mode) (length results)
              (%fmt tbl :pass) (%fmt tbl :fail) (%fmt tbl :unsupported)
              (%fmt tbl :timeout) (%fmt tbl :harness-error)
              (%fmt tbl :skip) (%threads-spawned results)))
    (dolist (r (%fail-detail results))
      (format t "  [~a] ~a ~a/~a: ~a~%"
              (wasmdown.wast:wast-result-file r)
              (wasmdown.wast:wast-result-command-name r)
              (wasmdown.wast:wast-result-outcome r)
              (wasmdown.wast:wast-result-command-index r)
              (wasmdown.wast:wast-result-detail r)))
    results))

;;; ---------------------------------------------------------------
;;; Main.

(let* ((options (%parse-args (rest sb-ext:*posix-argv*)))
       (files (%resolve-files (getf options :files)))
       (jsonl (getf options :jsonl))
       (failed nil))
  (format t "~&threads suite: ~d file~:p~%" (length files))
  (dolist (mode (getf options :modes))
    (let* ((path (and jsonl
                      (if (rest (getf options :modes))
                          (format nil "~a.~a" jsonl (%mode-label mode))
                          jsonl)))
           (results (%run-mode files mode path)))
      (when (%fail-detail results) (setf failed t))))
  (if failed
      (progn
        (format t "~&threads: FAIL (see non-pass commands above)~%")
        (sb-ext:exit :code 1))
      (format t "~&threads: PASS~%")))
