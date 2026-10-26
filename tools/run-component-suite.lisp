;;;; Component spec-script conformance lane (PLAN.md §12.7/§12.8, M7).
;;;; Runs the vendored component-model suite subset through the stateful
;;;; WAST runner and emits:
;;;;   - tests/component-results.jsonl  (one result per command)
;;;;   - CONFORMANCE-COMPONENT.md      (per-file + aggregate report)
;;;; Scope: tests/component/{binary,validation,linking,values}/*.wast
;;;; minus nyi.txt — the M7/M8/M9/M10 acceptance surface. The async/
;;;; resources suites need the task state machine (future slices) and
;;;; are NOT run here.
;;;; Exit code is ALWAYS 0: a baseline MEASUREMENT, not a gate; the
;;;; classification is honest per command (:pass/:fail/:unsupported).

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

;;; ---- the file set: binary + validation, minus nyi.txt ----

(defparameter *nyi*
  (with-open-file (s (merge-pathnames "tests/component/nyi.txt" *root*))
    (loop for line = (read-line s nil nil)
          while line
          when (and (plusp (length line))
                    (char= (char line 0) #\.))
            collect (subseq line 2))))         ; strip leading "./"

(defun nyi-p (file)
  "True when FILE's path (relative to tests/component/) is excluded.
The REL is the FILESYSTEM shape (values/post-return.wast — the
nyi.txt entries); the REPORTED case-id path is a DIFFERENT shape
(component/values/... via the :file format below), so derive the
rel from the last TWO directory components' SECOND entry (the
parent dir), not the first (the grandparent 'component' — the old
(car (last dir 2)) matched nothing once the values glob landed)."
  (let ((rel (format nil "~a/~a"
                     (second (last (pathname-directory file) 2))
                     (file-namestring file))))
    (member rel *nyi* :test #'string=)))

(defparameter *files*
  (sort
   (remove-if #'nyi-p
              (append
               (directory (merge-pathnames
                           "tests/component/binary/*.wast" *root*))
               (directory (merge-pathnames
                           "tests/component/validation/*.wast" *root*))
               ;; M9 (linking lane): the linking suite — unit.wast's
               ;; instance multiplicity/sharing, shared-everything's
               ;; core-level dynamic linking, tags' EH-tag linking,
               ;; link-time-virtualization's component composition.
               (directory (merge-pathnames
                           "tests/component/linking/*.wast" *root*))
               ;; M11a (async lane, slices 1+2): the async-core-green
               ;; fixtures. Slice 1: the VALIDATION-only files
               ;; (task.return's no-borrow rule, stream<char>
               ;; rejection, the async-canonopt
               ;; requires-async-type rule). Slice 2: the
               ;; EXECUTION files whose machinery is the landed
               ;; async core — the task/thread state machine, the
               ;; callback event loop, waitable-sets/join/subtask,
               ;; the backpressure gate + the deadlock oracle and
               ;; the sync-task cannot-block trap. The remaining
               ;; async files stay in nyi.txt with per-file probe
               ;; evidence (future/stream/thread/error-context
               ;; canons = M11b; async-calls-sync = the acs
               ;; interleave, one open runtime bug).
               (remove-if
                (lambda (f)
                  (not (member (file-namestring f)
                               '("validate-task-return.wast"
                                 "validate-no-stream-char.wast"
                                 "validate-no-async-abi-for-sync-type.wast"
                                 "deadlock.wast"
                                 "dont-block-start.wast"
                                 "drop-subtask.wast"
                                 "drop-waitable-set.wast"
                                 "cross-task-future.wast"
                                 "closed-stream.wast"
                                 "drop-stream.wast"
                                 "cancel-stream.wast"
                                 "cancel-and-exclusive-lock.wast"
                                 "cancel-not-delivered.wast"
                                 "zero-length.wast"
                                 "wait-during-callback.wast"
                                 "builtin-trap-poisons-instance.wast"
"forward.wast"
"self-switch-traps.wast"
                                 "empty-wait.wast"
"same-component-stream-future.wast"
                                 "trap-if-done.wast"
                                 "trap-if-transfer-in-waitable-set.wast"
                                 "trap-if-sync-and-waitable-set.wast")
                               :test #'string=)))
                (or (directory (merge-pathnames
                                "tests/component/async/*.wast" *root*))
                    '()))
               ;; M10 (values lane, SHIPPED): the canonical ABI at
               ;; runtime — strings/lists/records/variants lowered
               ;; through realloc + memory (CanonicalABI.md's
               ;; lift/lower_value, load/store per type, the
               ;; MAX_FLAT pointer indirection). post-return.wast
               ;; and variants.wast sit in nyi.txt: their bulk /
               ;; their SECOND fixture need the async built-ins
               ;; (task.return + may_leave — the next milestone);
               ;; the rest of the values suite is fully green.
               (directory (merge-pathnames
                           "tests/component/values/*.wast" *root*))
               ))
   #'string< :key #'namestring))

(defparameter *jsonl*
  (merge-pathnames "tests/component-results.jsonl" *root*))

(defparameter *results*
  (wasmdown.wast:run-directory *root*
                               ;; run-directory joins DIR + PATTERN, so
                               ;; run the two dirs separately here:
                               :pattern "tests/component/binary/*.wast"
                               :jsonl-path nil))

;;; Because run-directory takes ONE pattern, drive the suites directly:

(setf *results* nil)
(dolist (f *files*)
  (setf *results*
        (append *results*
                (wasmdown.wast:run-script
                 f :file (format nil "~a/~a"
                                 (car (last (pathname-directory f) 2))
                                 (file-namestring f))))))
(when *jsonl* (wasmdown.wast:write-jsonl *results* *jsonl*))

;;; ---- report ----

(defun outcome-counts (results)
  (let ((table (make-hash-table :test #'eq)))
    (dolist (r results)
      (incf (gethash (wasmdown.wast:wast-result-outcome r) table 0)))
    table))

(defun %fmt (table outcome)
  (or (gethash outcome table) 0))

(let ((table (outcome-counts *results*))
      (out (merge-pathnames "CONFORMANCE-COMPONENT.md" *root*)))
  (with-open-file (s out :direction :output :if-exists :supersede)
    (format s "# CONFORMANCE-COMPONENT.md — component suite baseline (M7 slice 1)~%~%")
    (format s "Generated by `make test-component` (tools/run-component-suite.lisp).~%")
    (format s "Baseline measurement, not a release gate (PLAN.md §12.8).~%~%")
    (format s "- Pinned suite: WebAssembly/component-model `d095e2f60ddae1f9a1e9b1e2fb547ec1114da677` (2026-10-24, specs/lock.toml)~%")
    (format s "- Slice scope: layer discrimination + component binary reader + WIT value~%")
    (format s "  types + structural sections + the (component ...) command family.~%")
    (format s "- Files: tests/component/{binary,validation}/*.wast minus nyi.txt~%")
    (format s "- `unsupported` = text-form components (no component text parser yet),~%")
    (format s "  valid binary components awaiting instantiation (canonical ABI is a~%")
    (format s "  future slice), or suite features outside this slice. Honest per command.~%~%")
    (format s "## Aggregate outcome counts~%~%")
    (format s "| Outcome | Count |~%| --- | --- |~%")
    (dolist (o '(:pass :fail :skip :unsupported :timeout :harness-error))
      (format s "| ~a | ~d |~%" (string-downcase (string o)) (%fmt table o)))
    (let* ((total (reduce #'+ (loop for v being the hash-values of table
                                    collect v)))
           (pass (%fmt table :pass)))
      (format s "| **total** | **~d** |~%~%" total)
      (format s "Executed: ~d commands; pass rate ~,1f%.~%~%"
              total (if (zerop total) 0.0 (* 100.0 (/ pass total)))))
    (format s "## Per-file counts~%~%")
    (format s "| File | pass | fail | unsupported | other |~%| --- | --- | --- | --- | --- |~%")
    (dolist (f *files*)
      (let* ((fname (format nil "~a/~a"
                             (car (last (pathname-directory f) 2))
                             (file-namestring f)))
             (rs (remove-if-not
                  (lambda (r) (string= (wasmdown.wast:wast-result-file r)
                                       fname))
                  *results*))
             (ft (outcome-counts rs)))
        (format s "| ~a | ~d | ~d | ~d | ~d |~%" fname
                (%fmt ft :pass) (%fmt ft :fail) (%fmt ft :unsupported)
                (+ (%fmt ft :skip) (%fmt ft :timeout)
                   (%fmt ft :harness-error)))))))

(format t "~&CONFORMANCE-COMPONENT.md + tests/component-results.jsonl written.~%")
(let ((table (outcome-counts *results*)))
  (dolist (o '(:pass :fail :unsupported :harness-error))
    (format t "  ~a: ~d~%" o (or (gethash o table) 0))))
