;;;; Component spec-script conformance lane (PLAN.md §12.7/§12.8, M7).
;;;; Runs the vendored component-model suite subset through the stateful
;;;; WAST runner and emits:
;;;;   - tests/component-results.jsonl  (one result per command)
;;;;   - CONFORMANCE-COMPONENT.md      (per-file + aggregate report)
;;;; Scope: tests/component/{binary,validation,linking,values}/*.wast
;;;; minus nyi.txt — the M7/M8/M9/M10 acceptance surface. The async/
;;;; resources suites need the task state machine (future slices) and
;;;; are NOT run here.
;;;; The run is a GATE: a fail, harness error, timeout, skip, or a
;;;; census mismatch (a planned file with no commands, a duplicate
;;;; case-id, an unknown outcome) exits NON-ZERO.

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
                                 "cancel-resumed-callback-switch.wast"
                                 ;; M14 (the idx/resume class closed
                                 ;; EN PASSANT): during-sync-call-
                                 ;; no-sibling-resume — the three
                                 ;; unknown-thread-index variants
                                 ;; (index 2, index 5, the
                                 ;; not-suspended trap) ABSENT from
                                 ;; 20/20 standalone + all 10 lane-
                                 ;; probe runs at the granted-park
                                 ;; build: resume_later's MARK-ONLY
                                 ;; removed the fire path that raced
                                 ;; the exit mid-handoff (the
                                 ;; lookup/timerace), and the granted
                                 ;; park's arm-time semantics removed
                                 ;; the stale returns. The inter-round
                                 ;; drain (M13-tok34) closed the
                                 ;; resume window proper.
                                 "during-sync-call-no-sibling-resume.wast"
                                 ;; M14 (the ::34 double-fire lead
                                 ;; closed EN PASSANT): trap-if-block-
                                 ;; and-sync — 48 rows, 40/40
                                 ;; standalone + ABSENT from all 10
                                 ;; lane-probe runs at the granted-
                                 ;; park build: the mark-only
                                 ;; resume_later removed the tick's
                                 ;; claim-then-fire vs the arm's fire
                                 ;; on the SAME record (the double-
                                 ;; fire — the M13-tok9 hypothesis,
                                 ;; consistent with all four
                                 ;; sessions' traces), and the
                                 ;; granted park's arm-time semantics
                                 ;; removed the stale-count returns.
                                 "trap-if-block-and-sync.wast"
                                 ;; M15 un-park gates: the four
                                 ;; fixtures the M14/M15 grant/park
                                 ;; work closed (cancel-targeted-
                                 ;; resume: the granted park's
                                 ;; quantum + the cancel-window fire
                                 ;; gate; switch-to-ready: the park
                                 ;; sweep; sync-streams: the sync
                                 ;; rendezvous; partial-stream-
                                 ;; copies: the result store's two
                                 ;; spaces). big-interleaving-test
                                 ;; stays nyi'd (its backpressure
                                 ;; row hangs — see nyi.txt).
                                 "cancel-targeted-resume.wast"
                                 "switch-to-ready-callback.wast"
                                 "partial-stream-copies.wast"
                                 "sync-streams.wast"
                                 ;; M15 gate 6: the backpressure
                                 ;; gate's pending-cancel readiness
                                 ;; closed the mock-bp-inc hang.
                                 "big-interleaving-test.wast")
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
(when *jsonl* (wasmdown.wast:write-jsonl *results* *jsonl* "component"))

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
    (format s "# CONFORMANCE-COMPONENT.md — the component lane (a GATE, not a baseline)~%~%")
    (format s "Generated by `make test-component` (tools/run-component-suite.lisp).~%")
    (format s "THIS RUN IS A GATE: the runner exits NON-ZERO on any fail,~%")
    (format s "harness-error, timeout, skip, or census mismatch (a planned file that~%")
    (format s "contributed no commands, a duplicate command, an unknown outcome).~%")
    (format s "The denominator printed below is the number of commands EXECUTED.~%~%")
    (format s "- Pinned suite: WebAssembly/component-model `d095e2f60ddae1f9a1e9b1e2fb547ec1114da677` (2026-10-24, specs/lock.toml)~%")
    (format s "- Scope: the whole vendored component suite in {binary, validation,~%")
    (format s "  linking, values, async} minus the entries in tests/component/nyi.txt~%")
    (format s "  — binary decoding, validation, instantiation and linking, the canonical~%")
    (format s "  ABI (lift/lower, memory, records/lists/variants/results/options, resource~%")
    (format s "  handles), and the async task machinery (threads, threads^tables,~%")
    (format s "  waitables, streams/futures, subtasks, cancellation, callbacks).~%")
    (format s "- `unsupported` counts suite features this engine classifies as~%")
    (format s "  out of scope rather than running; it must not grow silently, and the~%")
    (format s "  census above fails the run if a planned file stops contributing.~%~%")
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

;;; ---- the census + the gate ----
;;; The run is a GATE (PLAN.md §12.7): a fail, a harness error, a
;;; timeout, a silent skip or a census mismatch exits NON-ZERO. The
;;; denominator is the number of commands actually executed, and the
;;; census proves every planned file contributed and no command was
;;; silently dropped or doubly counted.

(defun %file-key (f)
  (format nil "~a/~a" (car (last (pathname-directory f) 2))
          (file-namestring f)))

(let* ((table (outcome-counts *results*))
       (fail (%fmt table :fail))
       (herr (%fmt table :harness-error))
       (tout (%fmt table :timeout))
       (skip (%fmt table :skip))
       (census-problems '()))
  ;; every planned file must contribute at least one command
  (let ((by-file (make-hash-table :test #'equal)))
    (dolist (r *results*)
      (incf (gethash (wasmdown.wast:wast-result-file r) by-file 0)))
    (dolist (f *files*)
      (let ((k (%file-key f)))
        (unless (plusp (gethash k by-file 0))
          (push (format nil "no commands executed from ~a" k)
                census-problems)))))
  ;; every case-id appears exactly once (a duplicate = a double run)
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (r *results*)
      (let ((id (list (wasmdown.wast:wast-result-file r)
                      (wasmdown.wast:wast-result-command-index r))))
        (incf (gethash id ids 0))))
    (maphash (lambda (id n)
               (when (> n 1)
                 (push (format nil "duplicate command ~a::~a (x~a)"
                               (first id) (second id) n)
                       census-problems)))
             ids))
  ;; any planned file with a skip/fail-free but EMPTY body is caught by
  ;; the per-file rule above; an outcome outside the known set is not
  (dolist (r *results*)
    (unless (member (wasmdown.wast:wast-result-outcome r)
                    '(:pass :fail :skip :unsupported :timeout
                      :harness-error))
      (push (format nil "unknown outcome ~a at ~a::~a"
                    (wasmdown.wast:wast-result-outcome r)
                    (wasmdown.wast:wast-result-file r)
                    (wasmdown.wast:wast-result-command-index r))
            census-problems)))
  (format t "~&CONFORMANCE-COMPONENT.md + tests/component-results.jsonl written.~%")
  (format t "  files: ~d planned, ~d contributed~%"
          (length *files*)
          (let ((seen (make-hash-table :test #'equal)))
            (dolist (f *files*) (setf (gethash (%file-key f) seen) t))
            (let ((n 0))
              (maphash (lambda (k v) (declare (ignore v))
                         (when (gethash k seen) (incf n)))
                       (let ((m (make-hash-table :test #'equal)))
                         (dolist (r *results*)
                           (setf (gethash (wasmdown.wast:wast-result-file r) m) t))
                         m))
              n)))
  (dolist (o '(:pass :fail :unsupported :skip :timeout :harness-error))
    (format t "  ~a: ~d~%" o (or (gethash o table) 0)))
  (format t "  executed (denominator): ~d~%" (length *results*))
  (when census-problems
    (format t "~&CENSUS PROBLEMS:~%")
    (dolist (c census-problems) (format t "  - ~a~%" c)))
  (if (or (plusp fail) (plusp herr) (plusp tout) (plusp skip)
          census-problems)
      (progn
        (format t "~&GATE: RED (~d fail, ~d harness-error, ~d timeout, ~
                   ~d skip, ~d census problem~:p)~%"
                fail herr tout skip (length census-problems))
        (sb-ext:exit :code 1))
      (progn
        (format t "~&GATE: GREEN~%")
        (sb-ext:exit :code 0))))
