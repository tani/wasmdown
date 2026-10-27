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
                                 ;; M13-tok10 (the flake-class
                                 ;; fixes): cancel-not-delivered —
                                 ;; THE RESOLVE-STASH WINDOW (the
                                 ;; trap rode flat-results stashed
                                 ;; AFTER %subtask-resolve; the
                                 ;; caller's wset delivery could
                                 ;; observe the resolved subtask
                                 ;; with the rider missing — the
                                 ;; SUBTASK event delivered, the
                                 ;; caller continued, the
                                 ;; assert_trap failed with 'the
                                 ;; action returned'; ~15% then,
                                 ;; 0/40 in the capture loop + the
                                 ;; 10-run FAILROW probe now).
                                 "cancel-not-delivered.wast"
                                 "zero-length.wast"
                                 "wait-during-callback.wast"
                                 "builtin-trap-poisons-instance.wast"
"forward.wast"
"during-sync-call-exclusive-resume.wast"
"during-sync-scheduling-candidates.wast"
"during-sync-call-may-block-if-other-ready-threads.wast"
                                 ;; M13 session 2: REVERTED to nyi — the
                                 ;; ::34 lost-wake (the fire->park window)
                                 ;; is a three-party rendezvous needing the
                                 ;; switch-lock design (nyi.txt session-2
                                 ;; note); the fixture's 47 green rows
                                 ;; return WITH the fix, same green commit.
"self-switch-traps.wast"
                                 "empty-wait.wast"
"same-component-stream-future.wast"
                                 "trap-if-done.wast"
                                 "trap-if-transfer-in-waitable-set.wast"
"trap-if-sync-and-waitable-set.wast"
                                 "future-completion-order.wast"
                                 "futures-must-write.wast"
                                 "cancel-delivery.wast"
                                 "idle-drop.wast"
                                 ;; M13-tok10 (the flake-class
                                 ;; fixes): switch-to-ready-
                                 ;; callback — the resolve-stash
                                 "sync-barges-in.wast"
                                 ;; M13-tok38 (the close-out's
                                 ;; un-park): passing-resources —
                                 ;; the own-transfer chain's
                                 ;; fixture (the resource crosses
                                 ;; the stream with its rep; the
                                 ;; borrow crosses the call; the
                                 ;; builtin drops through the
                                 ;; borrowed record): 10/10
                                 ;; standalone + the lane green.
                                 "passing-resources.wast"
                                 "drop-cross-task-borrow.wast"
                                 ;; M13-tok11 (the family ladder):
                                 ;; async-calls-sync — the acs
                                 ;; interleave (the four concurrent
                                 ;; sync-func calls' backpressure
                                 ;; pin) is DETERMINISTIC under the
                                 ;; aging quantum + the path key.
                                 ;; The 10-run FAILROW probe 10/10;
                                 ;; the lane green with it in.
                                 "async-calls-sync.wast"
                                 ;; M13-lad: the cross-ABI
                                 ;; signature/memory-collapse family
                                 ;; (49 rows: the task.return
                                 ;; [ptr] collapse + the async-lift
                                 ;; guest-body 16-flat rule)
                                 "cross-abi-calls.wast"
                                 ;; M14 (the resolved-yet family's
                                 ;; un-park gate 1): reentrance —
                                 ;; row 18's state-read race CLOSED
                                 ;; by the spec's synchronous
                                 ;; resume (§761-770's TURN
                                 ;; boundary: the eager-cancel
                                 ;; shape resolves BEFORE
                                 ;; subtask.cancel returns). 0/10 ->
                                 ;; 10/10 standalone at the gate;
                                 ;; the lane + the FAILROW probe
                                 ;; at the new denominator.
                                 "reentrance.wast"
                                 ;; M14 (the resolved-yet family's
                                 ;; un-park gate 2): cancel-subtask
                                 ;; — ::1's eager-cancel row CLOSED
                                 ;; by the same synchronous resume
                                 ;; (the fixture's first row pins
                                 ;; CANCELLED_BEFORE_RETURNED(4)
                                 ;; eagerly). 0/10 -> 10/10
                                 ;; standalone at gate 1's build.
                                 "cancel-subtask.wast"
                                 ;; M14 (the resolved-yet family's
                                 ;; un-park gate 3): cancel-resumed-
                                 ;; callback-switch — BOTH rows (the
                                 ;; own-task and the cross-task
                                 ;; switch shapes) GREEN at the
                                 ;; synchronous-resume build: the
                                 ;; resumed callback's SWITCH turn
                                 ;; blocks and the canceller observes
                                 ;; the mid-turn RE-PARK (the
                                 ;; "blocks" half of §756-759's
                                 ;; boundary), reporting BLOCKED
                                 ;; exactly as the fixture pins.
                                 ;; 10/10 standalone (the turn-only
                                 ;; wait's deadlock regression
                                 ;; measured and closed at gate 1).
                                 "cancel-resumed-callback-switch.wast")
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

;; M13-tok8: SBCL's unhandled-condition dump pretty-prints frame
;; ARGUMENTS; a cyclic async-end struct (the OTHER slot's two-way
;; link) printed with *print-circle* NIL expands without bound —
;; the 1.2GB heap-exhaustion crash on the async-lower-callee
;; thread. Print cycles finitely instead.
(setf *print-circle* t)
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
