;;;; tests/async-property.lisp — THE DETERMINISTIC ASYNC PROPERTY LANE
;;;; (docs/design/PLAN.md §9.3 "Deterministic async property suites",
;;;; §14.3 F4: deterministic fake-reactor/fake-executor property lane,
;;;; cancel/close races, exactly-once and backpressure invariants;
;;;; executor thread-affinity audit).
;;;;
;;;; WHAT IS DRIVEN. The runtime's REAL state-machine primitives — not a
;;;; model of them:
;;;;   * host-pending (async host calls): terminalise / cancel / ack /
;;;;     continuation attachment / pump / store-tick;
;;;;   * async-end (stream/future ends): %end-copy, %end-notify, %end-cancel,
;;;;     %end-drop, waitable-set poll, host sink/source generations;
;;;;   * host-future-watch: one-shot subscription resolve/drop/cancel;
;;;;   * async-task / async-subtask: %task-return, %task-cancel,
;;;;     %subtask-resolve, %subtask-deliver-resolve, %subtask-drop;
;;;;   * the backpressure admission gate (%task-enter-implicit-thread);
;;;;   * the bounded host executor and the wasi-context close/detach paths
;;;;     (fixed worker budgets, thread affinity, never-join close).
;;;;
;;;; DETERMINISM. Every random choice comes from the seed's xorshift64*
;;;; stream; no wall-clock sleeps are used. Worker synchronisation uses
;;;; semaphores and condition variables (the existing test convention),
;;;; and every wait is bounded by a yield count — a timeout is a lane
;;;; failure, not a flake. The fake reactor is a scripted reactor record
;;;; whose %REACTOR-STEP runs one queued thunk per store tick, so
;;;; ordering is a function of the seed only.
;;;;
;;;; INVARIANTS are checked after every primitive (see AP-*-INVARIANTS):
;;;; exactly-once terminal/event/delivery, monotone state, no double
;;;; delivery or double resolve, bounded work accounting, generation
;;;; monotonicity, cancel/close lifetime rules.
;;;;
;;;; Run it through tools/run-async-property.lisp (`make
;;;; test-async-property`). A violated invariant records a failure and the
;;;; runner exits non-zero. `--negative-control` deliberately corrupts a
;;;; delivered record and requires the same checkers to detect it.

(in-package :wasmdown.component.runtime)

;;; ---------------------------------------------------------------
;;; Result collection.

(defvar *ap-checks* 0)
(defvar *ap-failures* '())
(defvar *ap-scope* "async-property")
(defvar *ap-verbose* nil)

(defun ap-reset (&key (scope "async-property"))
  (setf *ap-checks* 0
        *ap-failures* '()
        *ap-scope* scope))

(defun ap-fail (fmt &rest args)
  (push (format nil "~@[~a: ~]~?" *ap-scope* fmt args) *ap-failures*)
  nil)

(defmacro ap-is (form &optional note)
  "Check FORM; NOTE is a format-control string evaluated only on failure."
  `(progn
     (incf *ap-checks*)
     (unless ,form
       (ap-fail "~a" (or ,note (format nil "invariant failed: ~s" ',form))))
     ,form))

(defun ap-is-equal (expected actual &optional note)
  (incf *ap-checks*)
  (unless (equal expected actual)
    (ap-fail "~a" (or note
                      (format nil "expected ~s, got ~s" expected actual))))
  (equal expected actual))

(defun ap-expect-error (thunk &optional (type 'error) note)
  "THUNK must signal TYPE. Returns the condition or NIL."
  (incf *ap-checks*)
  (handler-case
      (progn
        (funcall thunk)
        (ap-fail "~a" (or note "expected an error, none signalled"))
        nil)
    (error (c)
      (if (typep c type)
          c
          (progn (ap-fail "~a"
                          (or note (format nil "wrong condition: ~a" c)))
                 nil)))))

(defun ap-note (fmt &rest args)
  (when *ap-verbose*
    (apply #'format t fmt args)
    (terpri)))

;;; ---------------------------------------------------------------
;;; Deterministic PRNG: xorshift64* (explicit 64-bit mask, no wall clock,
;;; no implementation RNG state).

(defstruct (ap-rng (:constructor make-ap-rng (state)))
  (state 1 :type (unsigned-byte 64)))

(defun ap-rng-next (rng)
  (let ((x (ap-rng-state rng)))
    (setf x (ldb (byte 64 0) (logxor x (ash x 13))))
    (setf x (logxor x (ash x -7)))
    (setf x (ldb (byte 64 0) (logxor x (ash x 17))))
    (setf (ap-rng-state rng) x)
    x))

(defun ap-below (rng n)
  (if (or (not (integerp n)) (<= n 1))
      0
      (mod (ap-rng-next rng) n)))

(defun ap-pick (rng seq)
  (elt seq (ap-below rng (length seq))))

(defun ap-one-in (rng n)
  (zerop (ap-below rng n)))

;;; Bounded, sleep-free wait (the existing tests' convention): a timeout
;;; is a property failure, never an implicit pass.
(defun ap-wait-until (predicate &key (limit 4000000))
  (loop repeat limit
        when (funcall predicate) return t
        do (sb-thread:thread-yield)))

;;; ---------------------------------------------------------------
;;; The scripted reactor: a deterministic test double for %REACTOR-STEP.
;;; Each tick consumes exactly one queued thunk; MAX-WAIT-NS is recorded
;;; so the lane can assert the driver's bounded budget and idle gate.

(defstruct (ap-fake-reactor (:constructor make-ap-fake-reactor))
  (queue '())
  (steps 0)
  (max-waits '())
  (armed-deadline-p nil))

(defmethod %reactor-step ((reactor ap-fake-reactor) max-wait-ns)
  (push max-wait-ns (ap-fake-reactor-max-waits reactor))
  (incf (ap-fake-reactor-steps reactor))
  (let ((thunk (pop (ap-fake-reactor-queue reactor))))
    (when thunk (funcall thunk))
    (values (and thunk t)
            (and (ap-fake-reactor-queue reactor) t))))

(defmethod %reactor-pending-deadline-p ((reactor ap-fake-reactor))
  (ap-fake-reactor-armed-deadline-p reactor))

;;; ---------------------------------------------------------------
;;; Host-pending property suite.

(defun ap-pending-invariants (p store deliveries cancel-calls
                              &key previous-terminal previous-outcome
                                   previous-delivered)
  (let ((terminal (host-pending-terminal-p p))
        (state (host-pending-state p))
        (outcome (host-pending-outcome p)))
    (ap-is (member state '(:starting :waiting :resolved :rejected :cancelled))
           (format nil "illegal host-pending state ~s" state))
    (ap-is (eq terminal (not (member state '(:starting :waiting))))
           (format nil "terminal flag/state disagree: ~s ~s" terminal state))
    (ap-is (or (not terminal) outcome)
           "a terminal pending must carry an outcome")
    (ap-is (or terminal (null outcome))
           "a live pending must not carry an outcome")
    (when terminal
      (ap-is (eq state (ecase (car outcome)
                         (:resolve :resolved)
                         (:reject :rejected)
                         (:cancel :cancelled)))
             (format nil "outcome ~s disagrees with state ~s" outcome state))
      (when previous-terminal
        (ap-is (eq previous-outcome outcome)
               "a terminal pending's outcome changed (double resolve?)")))
    (ap-is (<= cancel-calls 1)
           (format nil "provider cancel-fn ran ~a times" cancel-calls))
    (ap-is (<= (length deliveries) 1)
           (format nil "the continuation ran ~a times" (length deliveries)))
    (when deliveries
      (ap-is (host-pending-delivered-p p)
             "a continuation ran before delivery was marked"))
    (when (and previous-delivered (not (host-pending-delivered-p p)))
      (ap-fail "delivered-p went backwards"))
    (ap-is (eq (host-pending-delivered-p p)
               (host-pending-work-released-p p))
           "the store-work unit must be released exactly with delivery")
    (ap-is (not (and (host-pending-delivered-p p)
                     (host-pending-delivery-queued-p p)))
           "a delivered pending left a delivery queued")
    (ap-is (>= (host-store-work store) 0)
           "the store's host-work count went negative")))

(defun ap-pending-trial (rng seed trial)
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (deliveries '())
         (cancel-calls 0)
         (mode (ap-pick rng '(:stop :noop :ack-then-nil)))
         (resolve-fn nil)
         (reject-fn nil)
         (ack-fn nil)
         (continuation-attached nil)
         (previous-terminal nil)
         (previous-outcome nil)
         (previous-delivered nil)
         (p nil))
    (setf p (make-host-pending
             :start (lambda (resolve reject ack)
                      (setf resolve-fn resolve
                            reject-fn reject
                            ack-fn ack)
                      (lambda ()
                        (incf cancel-calls)
                        (ecase mode
                          (:stop t)
                          (:noop nil)
                          (:ack-then-nil (funcall ack) nil))))))
    (ap-is (eq (host-pending-store p) store)
           "the pending must belong to the bound store")
    (ap-is (= 1 (host-store-work store))
           "a live pending holds exactly one store-work unit")
    (dotimes (step 10)
      (let ((op (ap-pick rng
                         '(:cancel :cancel :resolve :reject :ack
                           :attach :pump :pump :tick :stale-op))))
        (ecase op
          (:cancel
           (let ((fresh (%host-pending-request-cancel p)))
             (when (host-pending-terminal-p p)
               (ap-is (null fresh)
                      "cancel after terminal must be refused"))))
          (:resolve
           (if (host-pending-terminal-p p)
               (progn
                 ;; a late result must be DROPPED without changing the
                 ;; terminal outcome
                 (let ((before (host-pending-outcome p)))
                   (%host-pending-resolve p (list :late trial step))
                   (ap-is (eq before (host-pending-outcome p))
                          "a late resolve changed a terminal outcome")))
               (%host-pending-resolve p (list :v trial step))))
          (:reject
           (if (host-pending-terminal-p p)
               (%host-pending-reject p (make-condition 'simple-error
                                                       :format-control "late"))
               (%host-pending-reject p
                                     (make-condition 'simple-error
                                                     :format-control "reject"))))
          (:ack
           (if (host-pending-terminal-p p)
               (progn
                 (%host-pending-acknowledge-cancel p)
                 (ap-is (host-pending-terminal-p p)
                        "ack after terminal must not clear terminal"))
               (%host-pending-acknowledge-cancel p)))
          (:attach
           (unless continuation-attached
             (setf continuation-attached t)
             (%host-pending-set-continuation
              p (lambda (outcome) (push outcome deliveries)))))
          (:pump
           (%host-pump store))
          (:tick
           (%store-tick store))
          (:stale-op
           ;; a stale cancel request / second continuation attachment is
           ;; legal but may never re-deliver
           (when continuation-attached
             (%host-pending-set-continuation
              p (lambda (outcome) (push outcome deliveries))))))
        (ap-pending-invariants p store deliveries cancel-calls
                               :previous-terminal previous-terminal
                               :previous-outcome previous-outcome
                               :previous-delivered previous-delivered)
        (setf previous-terminal (host-pending-terminal-p p)
              previous-outcome (host-pending-outcome p)
              previous-delivered (host-pending-delivered-p p)))
      (ap-pending-invariants p store deliveries cancel-calls
                             :previous-terminal previous-terminal
                             :previous-outcome previous-outcome
                             :previous-delivered previous-delivered))
    ;; drain whatever the seed queued, then assert the final accounting
    (%host-pump store)
    (%host-pump store)
    (ap-pending-invariants p store deliveries cancel-calls
                           :previous-terminal previous-terminal
                           :previous-outcome previous-outcome
                           :previous-delivered previous-delivered)
    (when (host-pending-terminal-p p)
      (ap-is (zerop (host-store-work store))
             (format nil "terminal pending left store work ~a"
                     (host-store-work store)))
      (ap-is (host-pending-delivered-p p)
             "a terminal pending must be delivered by the final pump"))))

(defun ap-pending-targeted-tests ()
  "Deterministic arms the random walk cannot be relied on to hit."
  ;; resolve synchronously during start: the provider wins before any
  ;; cancel can be requested; the outcome is stable and delivered once.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (deliveries '())
         (p (make-host-pending
             :start (lambda (resolve reject ack)
                      (declare (ignore reject ack))
                      (funcall resolve 1)
                      (lambda () t)))))
    (ap-is (host-pending-terminal-p p) "synchronous resolve is terminal")
    (ap-is (null (%host-pending-request-cancel p))
           "cancel after synchronous resolve is refused")
    (%host-pending-set-continuation p (lambda (o) (push o deliveries)))
    (%host-pump store)
    (%host-pump store)
    (ap-is-equal '((:resolve . 1)) deliveries
                 "synchronous resolve delivers exactly once")
    (ap-is (zerop (host-store-work store)) "no leaked work unit"))
  ;; cancel-fn returns NIL: the call stays live; a later resolve wins and
  ;; the cancel-fn ran AT MOST once.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (calls 0)
         (deliveries '())
         (resolve-fn nil)
         (p (make-host-pending
             :start (lambda (resolve reject ack)
                      (declare (ignore reject ack))
                      (setf resolve-fn resolve)
                      (lambda () (incf calls) nil)))))
    (%host-pending-set-continuation p (lambda (o) (push o deliveries)))
    (%host-pending-request-cancel p)
    (%host-pump store)
    (%host-pump store)
    (ap-is (= 1 calls) "unstoppable cancel-fn runs once")
    (ap-is (eq :waiting (host-pending-state p))
           "NIL from cancel-fn leaves the operation live")
    (ap-is (= 1 (host-store-work store)) "live pending keeps its work unit")
    (funcall resolve-fn 9)
    (%host-pump store)
    (ap-is-equal '((:resolve . 9)) deliveries
                 "the later normal result delivers after a failed cancel"))
  ;; cancel-fn ACKNOWLEDGES directly and returns NIL: still cancelled,
  ;; delivered exactly once, and a second ack is a no-op.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (deliveries '())
         (p (make-host-pending
             :start (lambda (resolve reject ack)
                      (declare (ignore resolve reject))
                      (lambda () (funcall ack) nil)))))
    (%host-pending-set-continuation p (lambda (o) (push o deliveries)))
    (%host-pending-request-cancel p)
    (%host-pump store)
    (ap-is-equal '((:cancel)) deliveries "direct ack delivers exactly once")
    (let ((before (host-pending-outcome p)))
      (%host-pending-acknowledge-cancel p)
      (ap-is (eq before (host-pending-outcome p))
             "a second ack cannot change the outcome"))
    (ap-is (zerop (host-store-work store)) "cancelled pending releases work"))
  ;; resolve/reject first-wins race, in both orders.
  (dolist (order '((:resolve :reject) (:reject :resolve)))
    (let* ((store (make-component-store))
           (*current-component-store* store)
           (deliveries '())
           (p (make-host-pending
               :start (lambda (resolve reject ack)
                        (declare (ignore ack))
                        (ecase (first order)
                          (:resolve (funcall resolve 3))
                          (:reject (funcall reject
                                            (make-condition 'simple-error
                                                            :format-control "x"))))
                        (ecase (second order)
                          (:resolve (funcall resolve 4))
                          (:reject (funcall reject
                                            (make-condition 'simple-error
                                                            :format-control "y"))))
                        (lambda () t)))))
      (%host-pending-set-continuation p (lambda (o) (push o deliveries)))
      (%host-pump store)
      (ap-is (= 1 (length deliveries)) "first terminal event wins")
      (ap-is (eq (car (host-pending-outcome p)) (first order))
             "outcome is the first terminal event")))
  ;; a fake reactor + the store tick: idle stores never pump, a pending
  ;; does, the budget is clamped, and the armed-deadline oracle is the
  ;; fake reactor's scripted answer.
  (let* ((reactor (make-ap-fake-reactor))
         (store (make-component-store :reactor reactor))
         (*current-component-store* store))
    (push (lambda () (%host-store-work-add store 0))
          (ap-fake-reactor-queue reactor))
    (%store-tick store)
    (ap-is (= 0 (ap-fake-reactor-steps reactor))
           "an idle store never steps the reactor")
    (setf (ap-fake-reactor-armed-deadline-p reactor) t)
    (ap-is (null (%host-store-future-progress-p store))
           "an idle store cannot be future-progress even with a deadline")
    (let* ((p (make-host-pending
               :start (lambda (resolve reject ack)
                        (declare (ignore resolve reject ack))
                        (lambda () nil)))))
      (ap-is (%host-store-future-progress-p store)
             "pending host work + armed deadline is future progress")
      (%store-tick store :max-wait-ns 99999999)
      (ap-is (= 1 (ap-fake-reactor-steps reactor))
             "a busy tick steps the reactor exactly once")
      (ap-is (every (lambda (w) (<= w +store-tick-max-wait-ns+))
                    (ap-fake-reactor-max-waits reactor))
             "the reactor budget is clamped to the store tick's bound")
      (setf (ap-fake-reactor-armed-deadline-p reactor) nil)
      (ap-is (null (%host-store-future-progress-p store))
             "without an armed deadline a pending alone is not progress")
      (%host-pending-resolve p 0)
      (%host-pump store))))

;;; ---------------------------------------------------------------
;;; One-shot future watch suite.

(defun ap-watch-state-rank (state)
  (ecase state
    (:waiting 0) (:queued 1) (:delivered 2) (:cancelled 2)))

(defun ap-watch-trial (rng seed trial)
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (events '())
         (resolved-p nil))
    (multiple-value-bind (reader writer) (make-host-future-pair :u32)
      (let* ((cancel-fn (host-future-watch
                         reader
                         (lambda (value dropped)
                           (push (cons value dropped) events))))
             (watch (async-end-host-watch reader))
             (previous-state (and watch (host-future-watch-state watch))))
        (dotimes (step 8)
          (let ((op (ap-pick rng
                             '(:resolve :resolve :drop-writer :drop-reader
                               :cancel :cancel :pump :pump))))
            (ecase op
              (:resolve
               (unless resolved-p
                 (setf resolved-p t)
                 (host-future-resolve writer (list :ok (list trial step)))))
              (:drop-writer
               (%end-drop writer))
              (:drop-reader
               (%end-drop reader))
              (:cancel
               (let ((before (and watch (host-future-watch-state watch)))
                     (maybe (funcall cancel-fn)))
                 (if (eq before :waiting)
                     (ap-is maybe "a waiting watch is cancellable")
                     (ap-is (null maybe)
                            "a queued/delivered watch cannot be cancelled"))
                 (when watch
                   (ap-is (>= (ap-watch-state-rank
                               (host-future-watch-state watch))
                              (ap-watch-state-rank before))
                          "watch state went backwards"))))
              (:pump
               (%host-pump store)))
            (when watch
              (let ((state (host-future-watch-state watch)))
                (ap-is (>= (ap-watch-state-rank state)
                           (ap-watch-state-rank previous-state))
                       (format nil "watch state regressed ~s -> ~s"
                               previous-state state))
                (ap-is (<= (length events) 1)
                       "one-shot watch delivered more than once")
                (when (eq state :delivered)
                  (ap-is (= 1 (length events))
                         "delivered watch must have exactly one callback")
                  (ap-is (null (async-end-host-watch reader))
                         "delivery drops the end's watch slot"))
                (when (eq state :cancelled)
                  (ap-is (null events)
                         "a cancelled watch must never deliver"))
                (setf previous-state state))))
          (%host-pump store))
        ;; final drain
        (%host-pump store)
        (%host-pump store)
        (when watch
          (let ((state (host-future-watch-state watch)))
            (if (member state '(:queued))
                (ap-fail "a queued watch was never delivered")
                (ap-is (member state '(:waiting :delivered :cancelled))
                       "a watch settles as waiting, delivered or cancelled"))
            (when (eq state :delivered)
              (ap-is (<= (length events) 1)
                     "exactly-once future delivery"))
            (ap-is (not (and (eq state :delivered)
                             (null events)))
                   "delivered watch callback ran")))
        (when (eq (host-future-watch-state watch) :delivered)
          (destructuring-bind (value . dropped) (first events)
            (ap-is (or value dropped)
                   "a delivered watch carries a value or the drop flag")))))))

(defun ap-watch-targeted-tests ()
  ;; cancel before the value arrives: no callback ever runs.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (events '()))
    (multiple-value-bind (reader writer) (make-host-future-pair :u32)
      (let* ((cancel (host-future-watch reader
                                        (lambda (v d) (push (cons v d) events))))
             (watch (async-end-host-watch reader)))
        (ap-is (funcall cancel) "waiting watch cancels")
        (ap-is (eq :cancelled (host-future-watch-state watch))
               "the watch is marked cancelled")
        (ap-is (null (async-end-host-watch reader))
               "cancel clears the end's watch slot")
        (host-future-resolve writer (list :ok 1))
        (%host-pump store)
        (%host-pump store)
        (ap-is (null events) "cancelled watch never observes the value"))))
  ;; already-resolved peer: delivery is still QUEUED, never inline.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (events '()))
    (multiple-value-bind (reader writer) (make-host-future-pair :u32)
      (host-future-resolve writer (list :ok 7))
      (host-future-watch reader
                         (lambda (v d) (push (cons v d) events)))
      (ap-is (null events) "a watch never delivers inline")
      (ap-is (= 1 (host-store-count store))
             "the one-shot delivery is queued")
      (%host-pump store)
      (ap-is (= 1 (length events)) "the queued watch delivers once")
      (ap-is-equal (list :ok 7) (caar events) "the value crossed once")
      (%host-pump store)
      (ap-is (= 1 (length events)) "never twice"))))

;;; ---------------------------------------------------------------
;;; Stream/future end-copy state machine suite.

(defun ap-end-state-rank (state)
  (ecase state
    (:idle 0) (:copying 1) (:cancelling-copy 2) (:done 3)))

(defun ap-end-structurally-ok-p (end)
  (let ((other (async-end-other end)))
    (and (or (null other) (eq (async-end-other other) end))
         (or (null (async-end-buffer end))
             (host-buffer-p (async-end-buffer end)))))
  )

(defun ap-end-transition-ok-p (from to)
  "The end state machine's legal transitions: DONE is terminal; a
completion returns COPYING/CANCELLING to IDLE; a parked copy starts
from IDLE."
  (case from
    (:idle (member to '(:idle :copying :done)))
    (:copying (member to '(:copying :idle :done :cancelling-copy)))
    (:cancelling-copy (member to '(:cancelling-copy :idle :done)))
    (:done (eq to :done))))

(defun ap-end-invariants (ends gens states)
  (dolist (end ends)
    (ap-is (member (async-end-state end)
                   '(:idle :copying :cancelling-copy :done))
           "illegal end state")
    (ap-is (ap-end-structurally-ok-p end)
           "pair linkage / buffer shape")
    (let ((buf (async-end-buffer end)))
      (when buf
        (ap-is (plusp (host-buffer-capacity buf))
               "a parked host buffer has positive capacity")
        (ap-is (<= (host-buffer-progress buf) (host-buffer-capacity buf))
               "host buffer progress exceeded its capacity")))
    (let ((prev (gethash end gens)))
      (when prev
        (ap-is (>= (async-end-generation end) prev)
               "end copy generation went backwards"))
      (setf (gethash end gens) (async-end-generation end)))
    (let ((prev-state (gethash end states)))
      (when prev-state
        (ap-is (ap-end-transition-ok-p prev-state (async-end-state end))
               (format nil "illegal end transition ~s -> ~s"
                       prev-state (async-end-state end))))
      (setf (gethash end states) (async-end-state end)))))

(defun ap-poll-end (end wset &key (expect-event t))
  "Deliver at most one armed event for END; assert exactly-once and the
event code. Returns the event or NIL."
  (let ((armed (async-waitable-pending-event end))
        (ev (%wset-poll wset)))
    (cond
     ((equal ev '(0 0 0))
      (when (and expect-event armed)
        (ap-fail "an armed event was not delivered by poll"))
      nil)
     (t
      (ap-is (= (first ev) (async-end-event-code end))
             "event code names the polled end")
      (ap-is (null (async-waitable-pending-event end))
             "delivery clears the pending-event closure")
      (ap-is (null (async-waitable-pending-event end))
             "an armed event cannot be delivered twice")
      (%waitable-join end wset)
      ;; a second poll without a new event must be empty
      (ap-is-equal '(0 0 0) (%wset-poll wset)
                   "a delivered event is not re-delivered")
      ev))))

(defun ap-end-trial (rng seed trial)
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (future-p (ap-one-in rng 2)))
    (multiple-value-bind (r w)
        (%new-async-pair (if future-p :u32 :u8) nil
                         (if future-p +event-future-read+
                             +event-stream-read+)
                         future-p)
      (let* ((rs (make-async-waitable-set))
             (ws (make-async-waitable-set))
             (ends (list r w))
             (gens (make-hash-table :test #'eq))
             (states (make-hash-table :test #'eq)))
        (%waitable-join r rs)
        (%waitable-join w ws)
        (dotimes (step 12)
          (let ((op (ap-pick rng
                             '(:stage-read :stage-write :read-copy
                               :write-copy :poll :poll :cancel :drop
                               :cancel))))
            (ecase op
              (:stage-read
               (when (and (eq (async-end-state r) :idle)
                          (null (async-end-buffer r)))
                 (setf (async-end-buffer r)
                       (make-host-buffer (1+ (ap-below rng 5))))))
              (:stage-write
               (when (and (eq (async-end-state w) :idle)
                          (null (async-end-buffer w)))
                 (let ((n (1+ (ap-below rng 5))))
                   (setf (async-end-buffer w)
                         (make-host-buffer
                          n :values (loop for i below n
                                          collect (+ 65 (ap-below rng 26))))))))
              (:read-copy
               (when (and (async-end-buffer r)
                          (not (eq (async-end-state r) :done)))
                 (%end-copy r (async-end-buffer r) t)))
              (:write-copy
               (when (and (async-end-buffer w)
                          (not (eq (async-end-state w) :done)))
                 (%end-copy w (async-end-buffer w) nil)))
              (:poll
               (ap-poll-end r rs :expect-event nil)
               (ap-poll-end w ws :expect-event nil))
              (:cancel
               (let ((end (ap-pick rng ends)))
                 (when (member (async-end-state end)
                               '(:copying :cancelling-copy))
                   (let ((armed (async-waitable-pending-event end)))
                     (%end-cancel end)
                     (when armed
                       (ap-is (eq armed (async-waitable-pending-event end))
                              "End.cancel never overwrites an armed event"))))))
              (:drop
               (let ((end (ap-pick rng ends)))
                 (%end-drop end)
                 (ap-is (null (async-end-other end))
                        "drop detaches the dropped end")
                 (when (async-end-other end)
                   (ap-fail "drop left a peer link"))))))
          (ap-end-invariants ends gens states)
          ;; deliver any events armed by this step; this also proves the
          ;; poll path is not required for state convergence
          (ap-poll-end r rs :expect-event nil)
          (ap-poll-end w ws :expect-event nil)
          (ap-end-invariants ends gens states))
        ;; final drain
        (ap-poll-end r rs :expect-event nil)
        (ap-poll-end w ws :expect-event nil)
        (ap-end-invariants ends gens states)
        (when (member (async-end-state r) '(:idle :copying))
          (let ((buf (async-end-buffer r)))
            (when buf
              (ap-is (<= (host-buffer-progress buf)
                         (host-buffer-capacity buf))
                     "final read buffer accounting"))))))))

(defun ap-end-targeted-tests ()
  ;; COPYING -> cancel ARMS exactly one event; a pre-armed progress is
  ;; never overwritten; a stale poll is empty.
  (let* ((store (make-component-store))
         (*current-component-store* store))
    (multiple-value-bind (r w) (make-host-stream-pair :u8)
      (let* ((ws (make-async-waitable-set))
             (rb (make-host-buffer 2))
             (wb (make-host-buffer 2 :values '(1 2))))
        (%waitable-join w ws)
        (setf (async-end-buffer r) rb
              (async-end-buffer w) wb)
        (%end-copy w wb nil)
        (let ((first-event (async-waitable-pending-event w)))
          (ap-is first-event "the copy armed W's completion")
          (%end-cancel w)
          (ap-is (eq first-event (async-waitable-pending-event w))
                 "cancel preserves the already-armed completion")
          (let ((ev (ap-poll-end w ws)))
            (ap-is ev "the preserved event delivers")))))
    ;; drop: the peer receives DROPPED once, never twice.
    (multiple-value-bind (r w) (make-host-future-pair :u32)
      (let ((rs (make-async-waitable-set)))
        (%waitable-join r rs)
        (%end-drop w)
        (ap-is (null (async-end-other r)) "drop detaches both directions")
        (let ((ev (ap-poll-end r rs)))
          (ap-is ev "the peer observes the drop")
          (ap-is (= +copy-dropped+ (third ev))
                 "the peer event is DROPPED")
          (ap-is-equal '(0 0 0) (%wset-poll rs)
                       "the drop event is delivered once")))))
  ;; host-backed parked copy: a stale generation resumes nothing, the
  ;; current generation drains the buffer once.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (calls 0)
         (accept nil))
    (multiple-value-bind (reader writer) (make-host-stream-pair :u8)
      (host-end-set-sink
       reader
       (lambda (octets start end)
         (incf calls)
         (if accept
             (values (- end start) nil)
             (values 0 nil))))
      (let* ((buf (make-host-buffer 4 :values '(97 98 99 100)))
             (ws (make-async-waitable-set)))
        (%waitable-join writer ws)
        (setf (async-end-buffer writer) buf)
        (%end-copy writer buf nil)
        (ap-is (async-end-host-copy reader)
               "a would-block sink parks the copy")
        (let ((gen (async-end-generation reader)))
          (host-end-ready reader (1- gen))
          (%store-tick store)
          (ap-is (host-end-ready reader (1- gen))
                 "a stale readiness notification is accepted (and queued)")
          (%store-tick store)
          (ap-is (= 1 calls) "a stale generation resumes nothing")
          (ap-is (= 1 calls) "stale readiness never resumes the copy")
          (setf accept t)
          (host-end-ready reader gen)
          (%store-tick store)
          (ap-is (> calls 1) "current readiness retries the sink")
          (ap-is (null (async-end-host-copy reader))
                 "a completed retry clears the parked copy")
          (ap-is (null (async-end-buffer writer))
                 "the accepted buffer is unparked"))))))

;;; ---------------------------------------------------------------
;;; Task / Subtask state machine suite.

(defun ap-task-state-rank (state)
  (ecase state
    (:initial 0) (:started 1) (:pending-cancel 2)
    (:cancel-delivered 3) (:resolved 4)))

(defun ap-task-trial (rng seed trial)
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (resolves '())
         (task (make-async-task
                :on-resolve (lambda (result) (push result resolves))))
         (subtask (make-async-subtask))
         (lend (make-resource-handle :r 7 t 1 nil))
         (prev-task-state (async-task-state task))
         (prev-resolved (async-task-resolved-p task))
         (prev-sub-state (async-subtask-state subtask))
         (prev-delivered (async-subtask-resolve-delivered-p subtask)))
    (%subtask-record-lend subtask lend)
    (dotimes (step 8)
      (let ((op (ap-pick rng
                         '(:task-return :task-return
                           :deliver-cancel :deliver-cancel
                           :task-cancel :subtask-resolve
                           :subtask-resolve :deliver-resolve
                           :subtask-drop))))
        (ecase op
          (:task-return
           (if (async-task-resolved-p task)
               (ap-expect-error (lambda () (%task-return task (list :late step)))
                                'wasmdown:wasm-trap
                                "a second task.return must trap")
               (%task-return task (list :value step))))
          (:deliver-cancel
           (if (eq (async-task-state task) :pending-cancel)
               (ap-is (%task-deliver-pending-cancel task)
                      "pending cancellation is delivered once")
               (ap-is (null (%task-deliver-pending-cancel task))
                      "no cancellation is delivered without a request")))
          (:task-cancel
           (if (eq (async-task-state task) :cancel-delivered)
               (%task-cancel task)
               (ap-expect-error (lambda () (%task-cancel task))
                                'wasmdown:wasm-trap
                                "task.cancel before delivery must trap")))
          (:subtask-resolve
           (unless (%subtask-resolved-p subtask)
             (%subtask-resolve subtask
                               (ap-pick rng '(2 3 4))
                               (list (list :value step)))))
          (:deliver-resolve
           (%subtask-deliver-resolve subtask))
          (:subtask-drop
           (if (async-subtask-resolve-delivered-p subtask)
               (%subtask-drop subtask)
               (ap-expect-error (lambda () (%subtask-drop subtask))
                                'wasmdown:wasm-trap
                                "subtask.drop before delivery must trap"))))
        ;; invariants after every transition
        (ap-is (<= (length resolves) 1)
               "the task resolved more than once")
        (when (async-task-resolved-p task)
          (ap-is (eq :resolved (async-task-state task))
                 "resolved-p and the task state agree")
          (ap-is (= 1 (length resolves))
                 "a resolved task ran on-resolve exactly once"))
        (when (and (not prev-resolved) (async-task-resolved-p task))
          (ap-is (eq (length resolves) 1)
                 "resolution delivered exactly one result"))
        (ap-is (>= (ap-task-state-rank (async-task-state task))
                   (ap-task-state-rank prev-task-state))
               "task state went backwards")
        (setf prev-resolved (async-task-resolved-p task)
              prev-task-state (async-task-state task))
        (ap-is (>= (async-subtask-state subtask) prev-sub-state)
               "subtask state went backwards")
        (ap-is (or (not (%subtask-resolved-p subtask))
                   (>= (async-subtask-state subtask) 2))
               "resolved-p and the subtask state agree")
        (when (async-subtask-resolve-delivered-p subtask)
          (ap-is (null (async-subtask-lenders subtask))
                 "delivery clears the lender ledger")
          (ap-is (zerop (resource-handle-num-lends lend))
                 "a lender is returned exactly once"))
        (when (and (not prev-delivered)
                   (async-subtask-resolve-delivered-p subtask))
          (ap-is (api-subtask-delivery-ok-p subtask)
                 "delivery happened only after resolve"))
        (setf prev-delivered (async-subtask-resolve-delivered-p subtask)
              prev-sub-state (async-subtask-state subtask))))
    ;; a delivered subtask may be dropped; a resolved-but-undelivered one
    ;; may not. The random walk has already exercised both traps.
    (when (async-subtask-resolve-delivered-p subtask)
      (%subtask-drop subtask)
      (ap-is (null (async-waitable-wset subtask))
             "subtask.drop leaves its waitable set"))))

(defun api-subtask-delivery-ok-p (subtask)
  (or (async-subtask-resolve-delivered-p subtask)
      (%subtask-resolved-p subtask)))

(defun ap-task-targeted-tests ()
  ;; return then double-return/cancel traps; one on-resolve.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (resolves '())
         (task (make-async-task
                :on-resolve (lambda (r) (push r resolves)))))
    (%task-return task 42)
    (ap-is-equal '(42) resolves "task.return resolves once")
    (ap-expect-error (lambda () (%task-return task 43))
                     'wasmdown:wasm-trap
                     "a second task.return traps")
    (ap-expect-error (lambda () (%task-cancel task))
                     'wasmdown:wasm-trap
                     "task.cancel after return traps")
    (ap-is-equal '(42) resolves "no second resolution"))
  ;; pending-cancel -> deliver -> cancel: exactly once.
  (let* ((resolves '())
         (task (make-async-task :on-resolve (lambda (r) (push r resolves)))))
    (setf (async-task-state task) :pending-cancel)
    (ap-is (%task-deliver-pending-cancel task) "delivery succeeds")
    (ap-is (null (%task-deliver-pending-cancel task))
           "a second delivery is a no-op")
    (%task-cancel task)
    (ap-is-equal '(:cancelled) resolves
                 "task.cancel resolves with :cancelled")
    (ap-expect-error (lambda () (%task-cancel task))
                     'wasmdown:wasm-trap
                     "a second task.cancel traps"))
  ;; outstanding borrows trap return/cancel.
  (let ((task (make-async-task :on-resolve (lambda (r) (declare (ignore r))))))
    (setf (async-task-num-borrows task) 1)
    (ap-expect-error (lambda () (%task-return task 1))
                     'wasmdown:wasm-trap
                     "task.return with outstanding borrows traps")
    (setf (async-task-num-borrows task) 0)
    (%task-return task 1)
    (ap-is (eq :resolved (async-task-state task)) "return succeeds after drop"))
  ;; subtask: event exactly once, delivery idempotent, drop requires delivery.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (subtask (make-async-subtask))
         (wset (make-async-waitable-set))
         (lend (make-resource-handle :r 1 t 1 nil)))
    (%waitable-join subtask wset)
    (setf (async-waitable-pending-event subtask)
          (lambda ()
            (when (%subtask-resolved-p subtask)
              (%subtask-deliver-resolve subtask))
            (list 1 4 (async-subtask-state subtask))))
    (ap-expect-error (lambda () (%subtask-drop subtask))
                     'wasmdown:wasm-trap
                     "drop before resolve traps")
    (%subtask-record-lend subtask lend)
    (%subtask-resolve subtask 2 '((:value 1)))
    (ap-is (%wset-has-pending-event wset) "resolve arms the event")
    (let ((ev (%wset-poll wset)))
      (ap-is (= 1 (first ev)) "the subtask event is code 1")
      (ap-is (= 4 (second ev)) "the event names the subtask index")
      (ap-is (= 2 (third ev)) "the event carries RETURNED")
      (ap-is (async-subtask-resolve-delivered-p subtask)
             "delivery happened inside the event")
      (ap-is (zerop (resource-handle-num-lends lend))
             "resolve delivery returns the lend")
      (ap-is-equal '(0 0 0) (%wset-poll wset)
                   "the subtask event is delivered exactly once"))
    (%subtask-deliver-resolve subtask)
    (ap-is (zerop (resource-handle-num-lends lend))
           "second delivery does not underflow the lend count")
    (%subtask-drop subtask)
    (ap-is (null (async-waitable-wset subtask))
           "drop removes the subtask from its set")))

;;; ---------------------------------------------------------------
;;; Backpressure admission gate suite (the real %task-enter-implicit-thread
;;; on real SB-THREADs, synchronised by yield-counted waits only).

(defun ap-make-gate-task (inst)
  (let ((ft (wasmdown.component:make-ct :kind :func
                                        :payload (list nil nil t))))
    (let* ((task (make-async-task
                  :ft ft :opts '((:async . t) (:callback . t))
                  :inst inst))
           (th (make-async-thread :task task)))
      (setf (async-task-implicit-thread task) th)
      (values task th))))

(defun ap-gate-trial (rng seed trial)
  (declare (ignore rng))
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (inst (make-comp-instance))
         (fields (%inst-async-fields inst)))
    (multiple-value-bind (task th) (ap-make-gate-task inst)
      (let ((done nil) (err nil)
            (backpressure 1))
        (setf (getf fields :backpressure) backpressure)
        (let ((worker
                (sb-thread:make-thread
                 (lambda ()
                   (let ((*current-component-store* store))
                     (handler-case
                         (progn (%task-enter-implicit-thread task)
                                (setf done t))
                       (error (c) (setf err c)))))
                 :name (format nil "ap-gate-~a-~a" seed trial))))
          (ap-is (ap-wait-until (lambda () (async-task-gate-parked task)))
                 "the gate parks the task while backpressure holds")
          (ap-is (= 1 (getf fields :num-waiting-to-enter))
                 "the gate counts exactly one waiter")
          (ap-is (null (async-thread-index th))
                 "a gate-parked task is not registered")
          (ap-is (null (getf fields :exclusive-thread))
                 "a gate-parked task holds no exclusive lock")
          (ap-is (null done) "the gate actually blocks the start")
          (setf (getf fields :backpressure) 0)
          (async-thread-resume-all)
          (ap-is (ap-wait-until (lambda () (or done err)))
                 "releasing backpressure admits the task")
          (ap-is (null err) "admission signalled no error")
          (ap-is (integerp (async-thread-index th))
                 "an admitted task has a thread index")
          (ap-is (eq (getf fields :exclusive-thread) th)
                 "an exclusive task owns the exclusive slot")
          (ap-is (zerop (getf fields :num-waiting-to-enter))
                 "the waiter count is restored")
          (ap-is (not (async-task-gate-parked task))
                 "the gate-parked mark is cleared")
          (%task-exit-implicit-thread task)
          (ap-is (null (getf fields :exclusive-thread))
                 "exit releases the exclusive slot")
          (ap-is (null (member th (%store-threads) :test #'eq))
                 "exit unregisters the thread")
          (ignore-errors (sb-thread:join-thread worker)))))
    ;; TWO waiters, one exclusive lock: admission is serialised.
    (multiple-value-bind (t1 th1) (ap-make-gate-task inst)
      (multiple-value-bind (t2 th2) (ap-make-gate-task inst)
        (declare (ignore th2))
        (let ((done1 nil) (done2 nil))
          (setf (getf fields :backpressure) 1
                (getf fields :num-waiting-to-enter) 0)
          (let ((w1 (sb-thread:make-thread
                     (lambda ()
                       (let ((*current-component-store* store))
                         (%task-enter-implicit-thread t1)
                         (setf done1 t)))))
                (w2 (sb-thread:make-thread
                     (lambda ()
                       (let ((*current-component-store* store))
                         (%task-enter-implicit-thread t2)
                         (setf done2 t))))))
            (ap-is (ap-wait-until
                    (lambda () (and (async-task-gate-parked t1)
                                    (async-task-gate-parked t2))))
                   "both waiters park at the gate")
            (ap-is (= 2 (getf fields :num-waiting-to-enter))
                   "both waiters are counted")
            (setf (getf fields :backpressure) 0)
            (async-thread-resume-all)
            (ap-is (ap-wait-until (lambda () (or done1 done2)))
                   "one waiter is admitted")
            (ap-is (not (and done1 done2))
                   "the exclusive slot serialises admission")
            (let ((first (if done1 t1 t2))
                  (second (if done1 t2 t1))
                  (first-done (if done1 done1 done2)))
              (declare (ignore first-done))
              (ap-is (eq (getf fields :exclusive-thread)
                         (async-task-implicit-thread first))
                     "the admitted task owns the slot")
              (%task-exit-implicit-thread first)
              (async-thread-resume-all)
              (ap-is (ap-wait-until (lambda () (or (eq (getf fields :exclusive-thread)
                                                      (async-task-implicit-thread second))
                                                   (and (or done1 done2)
                                                        (async-task-gate-parked second)))))
                     "the second waiter is admitted after release")
              (ap-is (ap-wait-until (lambda () (and done1 done2)))
                     "both waiters eventually admit, one at a time")
              (%task-exit-implicit-thread second))
            (ignore-errors (sb-thread:join-thread w1))
            (ignore-errors (sb-thread:join-thread w2))))))))

(defun ap-gate-targeted-tests ()
  ;; no backpressure, non-exclusive (stackful async): admission is immediate
  ;; and does NOT take the exclusive slot.
  (let* ((store (make-component-store))
         (*current-component-store* store)
         (inst (make-comp-instance))
         (ft (wasmdown.component:make-ct :kind :func
                                         :payload (list nil nil t)))
         (task (make-async-task :ft ft :opts '((:async . t))
                                :inst inst))
         (th (make-async-thread :task task)))
    (setf (async-task-implicit-thread task) th)
    (%task-enter-implicit-thread task)
    (ap-is (integerp (async-thread-index th)) "immediate admission registers")
    (ap-is (null (getf (%inst-async-fields inst) :exclusive-thread))
           "stackful async takes no exclusive slot")
    (%task-exit-implicit-thread task)))

;;; ---------------------------------------------------------------
;;; Bounded host executor / wasi-context close audit.

(defun ap-executor-trial (rng seed trial)
  (declare (ignore rng))
  (let* ((workers (1+ (mod (+ seed trial) 3)))
         (exec (wasmdown.wasi::make-host-executor
                :name (format nil "ap-exec-~a-~a" seed trial)
                :workers workers))
         (mutex (sb-thread:make-mutex :name "ap-exec"))
         (release (sb-thread:make-semaphore :name "ap-release"))
         (started 0)
         (finished 0)
         (post-close-runs 0)
         (max-active 0)
         (affine t))
    (unwind-protect
         (progn
           (ap-is (= workers
                     (wasmdown.wasi::host-executor-worker-count exec))
                  "the worker budget is the construction argument")
           (ap-is (= workers
                     (length (wasmdown.wasi::host-executor-threads exec)))
                  "the executor owns exactly the fixed number of threads")
           (dolist (th (wasmdown.wasi::host-executor-threads exec))
             (ap-is (sb-thread:thread-alive-p th)
                    "a worker is alive after construction")
             (ap-is (search (format nil "ap-exec-~a-~a-"
                                    seed trial)
                            (sb-thread:thread-name th))
                    "workers are named by their service"))
           ;; Saturate the fixed budget with in-flight blockers.
           (dotimes (i workers)
             (ap-is (wasmdown.wasi::%executor-submit
                     exec
                     (lambda ()
                       (let ((self sb-thread:*current-thread*))
                         (sb-thread:with-recursive-lock (mutex)
                           (incf started)
                           (setf max-active
                                 (max max-active
                                      (wasmdown.wasi::host-executor-active
                                       exec)))
                           (unless (member self
                                           (wasmdown.wasi::host-executor-threads
                                            exec)
                                           :test #'eq)
                             (setf affine nil))))
                       (sb-thread:wait-on-semaphore release)
                       (sb-thread:with-recursive-lock (mutex)
                         (incf finished))))
                    "blocking jobs are accepted before close"))
           (ap-is (ap-wait-until
                   (lambda ()
                     (= workers
                        (wasmdown.wasi::host-executor-active exec))))
                  "all workers are busy")
           (ap-is (<= (wasmdown.wasi::host-executor-active exec) workers)
                  "active jobs never exceed the worker budget")
           ;; Queue more jobs, then CLOSE: the queued ones are dropped.
           (dotimes (i 5)
             (ap-is (wasmdown.wasi::%executor-submit
                     exec
                     (lambda ()
                       (sb-thread:with-recursive-lock (mutex)
                         (incf finished)
                         (incf post-close-runs))))
                    "queued jobs are accepted before close"))
           (ap-is (wasmdown.wasi::%executor-close exec)
                  "close succeeds")
           (ap-is (null (wasmdown.wasi::%executor-submit
                         exec (lambda () nil)))
                  "a closed executor refuses new work")
           ;; Close returned while the blockers are still in flight.
           (ap-is (= workers
                     (wasmdown.wasi::host-executor-active exec))
                  "close never joins in-flight jobs")
           (dotimes (i workers) (sb-thread:signal-semaphore release))
           (ap-is (ap-wait-until
                   (lambda ()
                     (= finished workers)))
                  "in-flight jobs finish into discarded results")
           (ap-is (zerop post-close-runs)
                  "queued jobs are dropped, never run after close")
           (ap-is (<= max-active workers)
                  "max observed concurrency respects the budget")
           (ap-is affine
                  "every job ran on a worker of this executor")
           (ap-is (ap-wait-until
                   (lambda ()
                     (notany #'sb-thread:thread-alive-p
                             (wasmdown.wasi::host-executor-threads exec))))
                  "workers exit after close + drain"))
      (wasmdown.wasi::%executor-close exec))))

(defun ap-executor-targeted-tests ()
  ;; Context close: reject new work, detach both services, idempotent,
  ;; fixed distinct budgets.
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (let* ((dns (wasmdown.wasi::wasi-context-dns-executor ctx))
           (stdio (wasmdown.wasi::wasi-context-stdio-executor ctx)))
      (ap-is (not (eq dns stdio)) "DNS and stdio never share a pool")
      (ap-is (= wasmdown.wasi::*dns-worker-count*
                (wasmdown.wasi::host-executor-worker-count dns))
             "DNS has its fixed budget")
      (ap-is (= wasmdown.wasi::*stdio-worker-count*
                (wasmdown.wasi::host-executor-worker-count stdio))
             "stdio has its fixed budget")
      (ap-is (eq dns (wasmdown.wasi::wasi-context-dns-executor ctx))
             "the context caches the DNS service")
      (ap-is (eq stdio (wasmdown.wasi::wasi-context-stdio-executor ctx))
             "the context caches the stdio service")
      (dolist (th (wasmdown.wasi::host-executor-threads dns))
        (ap-is (search "wasi-dns" (sb-thread:thread-name th))
               "a DNS worker is named by its service"))
      (dolist (th (wasmdown.wasi::host-executor-threads stdio))
        (ap-is (search "wasi-stdio" (sb-thread:thread-name th))
               "a stdio worker is named by its service"))
      ;; DNS jobs run on the DNS service's own worker, never the caller.
      (let ((ran-on nil))
        (ap-is (wasmdown.wasi::%executor-submit
                dns
                (lambda ()
                  (setf ran-on
                        (and (member sb-thread:*current-thread*
                                     (wasmdown.wasi::host-executor-threads dns)
                                     :test #'eq)
                             t))))
               "the DNS service accepts a job")
        (ap-is (ap-wait-until (lambda () ran-on))
               "the DNS job runs on a DNS worker (affinity)")
        (ap-is (eq ran-on t) "the DNS caller thread did not run the job"))
      (ap-is (eq ctx (wasmdown.wasi:close-wasi-context ctx))
             "close returns the context")
      (ap-is (wasmdown.wasi::wasi-context-closed-p ctx)
             "the context is marked closed")
      (ap-is (null (wasmdown.wasi::wasi-context-dns-exec ctx))
             "close detaches the DNS service")
      (ap-is (null (wasmdown.wasi::wasi-context-stdio-exec ctx))
             "close detaches the stdio service")
      (ap-is (wasmdown.wasi::host-executor-closed-p dns)
             "the DNS service is closed")
      (ap-is (wasmdown.wasi::host-executor-closed-p stdio)
             "the stdio service is closed")
      (ap-expect-error (lambda ()
                         (wasmdown.wasi::wasi-context-dns-executor ctx))
                       'wasmdown:host-error
                       "a closed context refuses DNS work")
      (ap-expect-error (lambda ()
                         (wasmdown.wasi::wasi-context-stdio-executor ctx))
                       'wasmdown:host-error
                       "a closed context refuses stdio work")
      (ap-is (eq ctx (wasmdown.wasi:close-wasi-context ctx))
             "close is idempotent")
      (ap-is (wasmdown.wasi:close-wasi-context-executors ctx)
             "executor detach is idempotent")
      (ap-is (ap-wait-until
              (lambda ()
                (and (notany #'sb-thread:thread-alive-p
                             (wasmdown.wasi::host-executor-threads dns))
                     (notany #'sb-thread:thread-alive-p
                             (wasmdown.wasi::host-executor-threads stdio)))))
             "closed services' workers exit without a join")))
  ;; Blocked-stdio: the caller's span is copied to a worker, the caller
  ;; returns immediately, and a close rejects the next submission.
  (multiple-value-bind (read-fd write-fd) (sb-unix:unix-pipe)
    (let* ((out (sb-sys:make-fd-stream write-fd :output t
                                              :element-type '(unsigned-byte 8)
                                              :buffering :none))
           (ctx (wasmdown.wasi:make-wasi-context))
           (sink nil)
           (octets (make-array 4 :element-type '(unsigned-byte 8)
                                 :initial-contents '(97 98 99 100))))
      (unwind-protect
           (progn
             (ap-is (not (wasmdown.wasi::%fd-nonblocking-p write-fd))
                    "a borrowed pipe is a blocking fd (the worker path)")
             (multiple-value-bind (pair-reader end)
                 (make-host-stream-pair :u8)
               (declare (ignore pair-reader))
               (setf sink (wasmdown.wasi:cl-stream-sink
                           out :context ctx :end end)))
             (multiple-value-bind (consumed error) (funcall sink octets 0 4)
               (ap-is (eql 0 consumed)
                      "the caller never performs the blocking write")
               (ap-is (null error) "a would-block span reports no error"))
             (ap-is (wasmdown.wasi:close-wasi-context ctx)
                    "close returns while a stdio job may be in flight")
             (multiple-value-bind (pair-reader end2)
                 (make-host-stream-pair :u8)
               (declare (ignore pair-reader))
               (let ((fresh (wasmdown.wasi:cl-stream-sink
                             out :context ctx :end end2)))
                 (ap-expect-error
                  (lambda () (funcall fresh octets 0 4))
                  'wasmdown:host-error
                  "a closed context rejects a NEW stdio submission"))))
        (ignore-errors (close out))
        (wasmdown.wasi:close-wasi-context-executors ctx)
        (ignore-errors (sb-unix:unix-close read-fd))))))

;;; ---------------------------------------------------------------
;;; The lane driver.

(defparameter *ap-default-seeds* '(1 2 3 5 8 13 21 34))

(defvar *ap-inject-pending-double-delivery* nil)

(defun ap-run-one-random-bundle (rng seed trial)
  (ap-pending-trial rng seed trial)
  (ap-watch-trial rng seed trial)
  (ap-end-trial rng seed trial)
  (ap-task-trial rng seed trial))

(defun ap-run-targeted-tests ()
  (ap-pending-targeted-tests)
  (ap-watch-targeted-tests)
  (ap-end-targeted-tests)
  (ap-task-targeted-tests)
  (ap-gate-targeted-tests)
  (ap-executor-targeted-tests))

(defun run-async-property-lane (&key (seeds *ap-default-seeds*)
                                      (repeats 3)
                                      (verbose nil)
                                      negative-control)
  "Run the deterministic async property lane. Returns (values CHECKS
FAILURES). FAILURES is a list of strings; the caller exits non-zero
when it is non-empty. NEGATIVE-CONTROL runs the corrupted arm and
returns a single synthetic result: (values 1 NIL) when every expected
violation was detected, (values 1 (\"negative control did not...\"))
otherwise."
  (let ((*ap-verbose* verbose))
    (if negative-control
        (progn
          (ap-reset :scope "negative-control")
          ;; Deliberately break exactly-once: run the pending's
          ;; continuation, clear its delivered mark, and run it again
          ;; through the real delivery path. The invariant checker MUST
          ;; see the second delivery.
          (let* ((store (make-component-store))
                 (*current-component-store* store)
                 (deliveries '())
                 (p (make-host-pending
                     :start (lambda (resolve reject ack)
                              (declare (ignore reject ack))
                              (funcall resolve 1)
                              (lambda () t)))))
            (%host-pending-set-continuation
             p (lambda (outcome) (push outcome deliveries)))
            (%host-pump store)
            (ap-is (= 1 (length deliveries))
                   "the honest path delivers exactly once")
            (setf (host-pending-delivered-p p) nil)
            (%host-pending-deliver p)
            (ap-pending-invariants p store deliveries 0))
          (let ((detected (length *ap-failures*)))
            (ap-reset :scope "negative-control")
            (if (plusp detected)
                (progn
                  (format t "~&async-property negative control: detected ~d injected violation(s) as required~%"
                          detected)
                  (values 1 nil))
                (values 1 (list "negative control: injected double delivery was NOT detected")))))
        (let ((total-checks 0)
              (all-failures '()))
          (dolist (seed seeds)
            (ap-reset :scope (format nil "seed ~a" seed))
            (dotimes (i repeats)
              (let* ((stream-seed (ldb (byte 64 0)
                                       (+ (* seed 1000003) (1+ i) 17)))
                     (rng (make-ap-rng (if (zerop stream-seed) 1 stream-seed))))
                (ap-run-one-random-bundle rng seed (1+ i))))
            (ap-run-targeted-tests)
            (incf total-checks *ap-checks*)
            (let ((fails (reverse *ap-failures*)))
              (setf all-failures (append all-failures fails))
              (format t "~&async-property seed ~2,'0d: ~4d checks, ~d failure(s)~%"
                      seed *ap-checks* (length fails))
              (dolist (f fails)
                (format t "  FAIL ~a~%" f))))
          (format t "~&async-property TOTAL: ~d checks over ~d seed(s) x ~d repeat(s)~%"
                  total-checks (length seeds) repeats)
          (format t "async-property seeds: ~{~a~^ ~}~%" seeds)
          (values total-checks all-failures)))))
