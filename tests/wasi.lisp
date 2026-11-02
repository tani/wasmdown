;;;; M16 slice 1: the WASI provider pins -- the CL-level provider
;;;; implementations, the capability model, the host-provider
;;;; framework, and the end-to-end component fixture that imports
;;;; wasi:clocks/monotonic-clock@0.3.2 and calls it through a real
;;;; canon-lowered core import.

(defpackage #:wasmdown.test.wasi
  (:use #:cl #:wasmdown.test))

(in-package #:wasmdown.test.wasi)

(defun provider-fn (instance name)
  (wasmdown.wasi:provider-instance-function instance name))

(defun call-provider (instance name &rest args)
  (apply #'wasmdown.component.runtime:lifted-func-call
         (provider-fn instance name) args))

(defun rt (name)
  "The runtime's internal function/accessor NAME (the host-end
machinery is deliberately not part of the public surface)."
  (or (find-symbol name (find-package '#:wasmdown.component.runtime))
      (error "no runtime symbol ~a" name)))

(defmacro rtcall (name &rest args)
  `(funcall (rt ,(string name)) ,@args))

;;; ---------------------------------------------------------------
;;; wasi:random -- OS entropy only.

(test wasi/random-os-entropy
  ;; length + range
  (let ((bytes (wasmdown.wasi:os-random-bytes 64)))
    (is (= 64 (length bytes)))
    (is (every (lambda (b) (and (integerp b) (<= 0 b 255))) bytes)))
  ;; zero is an empty list, never a trap (the WIT's MUST)
  (is (null (wasmdown.wasi:random-bytes 0)))
  (is (null (wasmdown.wasi:insecure-random-bytes 0)))
  ;; the provider's short-read cap is a bound, and at least one byte
  ;; comes back for a positive request
  (let ((bytes (wasmdown.wasi:random-bytes 100000000)))
    (is (plusp (length bytes)))
    (is (<= (length bytes) wasmdown.wasi::*max-random-bytes*)))
  ;; u64 range
  (let ((u (wasmdown.wasi:random-u64)))
    (is (and (integerp u) (<= 0 u) (< u (ash 1 64))))
    (is (not (= u (wasmdown.wasi:random-u64)))))
  ;; the insecure-seed tuple
  (let ((seed (wasmdown.wasi:insecure-seed)))
    (is (= 2 (length seed)))
    (is (every (lambda (u) (and (integerp u) (<= 0 u) (< u (ash 1 64))))
               seed))))

;;; ---------------------------------------------------------------
;;; wasi:clocks.

(test wasi/clocks-monotonic
  (let ((a (wasmdown.wasi:monotonic-now))
        (b (wasmdown.wasi:monotonic-now)))
    (is (integerp a))
    (is (<= a b)))
  (is (plusp (wasmdown.wasi:monotonic-resolution))))

(test wasi/clocks-system-instant
  (let ((instant (wasmdown.wasi:system-now)))
    (is (= 2 (length instant)))
    (let ((seconds (cdr (assoc "seconds" instant :test #'string=)))
          (nanos (cdr (assoc "nanoseconds" instant :test #'string=))))
      (is (integerp seconds))
      (is (> seconds 1700000000))       ; after 2023-11-14
      (is (and (integerp nanos) (<= 0 nanos) (< nanos 1000000000)))))
  (is (plusp (wasmdown.wasi:system-resolution))))

;;; ---------------------------------------------------------------
;;; The capability model: everything comes from the context object.

(test wasi/capability-environment
  (let ((ctx-a (wasmdown.wasi:make-wasi-context
                :arguments '("prog" "-v")
                :environment '(("HOME" . "/root") ("LANG" . "C"))
                :cwd "/work"))
        (ctx-b (wasmdown.wasi:make-wasi-context
                :arguments '("other")
                :environment '(("HOME" . "/tmp"))
                :cwd nil)))
    (is (equal (wasmdown.wasi:environment-get-arguments ctx-a)
               '("prog" "-v")))
    (is (equal (wasmdown.wasi:environment-get-environment ctx-a)
               '(("HOME" "/root") ("LANG" "C"))))
    (is (equal (wasmdown.wasi:environment-get-initial-cwd ctx-a)
               '(:some . "/work")))
    (is (eq (wasmdown.wasi:environment-get-initial-cwd ctx-b) :none))
    ;; two differently-authorized contexts never see each other's state
    (is (equal (wasmdown.wasi:environment-get-environment ctx-b)
               '(("HOME" "/tmp"))))
    ;; the provider instances are built from the context object
    (let ((ia (wasmdown.wasi:cli-environment-instance ctx-a))
          (ib (wasmdown.wasi:cli-environment-instance ctx-b)))
      (is (equal (call-provider ia "get-arguments") '("prog" "-v")))
      (is (equal (call-provider ib "get-arguments") '("other")))
      (is (equal (call-provider ia "get-initial-cwd") '(:some . "/work")))
      (is (eq (call-provider ib "get-initial-cwd") :none)))))

(defun exit-status-of (thunk)
  "Run THUNK; the wasi-exit status it signals, or NIL if it returned."
  (block nil
    (handler-case (progn (funcall thunk) nil)
      (wasmdown.wasi:wasi-exit (c)
        (wasmdown.wasi:wasi-exit-status c)))))

(test wasi/exit
  (let ((exit (wasmdown.wasi:cli-exit-instance)))
    (is (eql 7 (exit-status-of
                (lambda () (call-provider exit "exit-with-code" 7)))))
    ;; exit(result): ok -> 0, err -> 1
    (is (eql 0 (exit-status-of
                (lambda () (call-provider exit "exit" '(:ok))))))
    (is (eql 1 (exit-status-of
                (lambda () (call-provider exit "exit" '(:err))))))
    ;; the condition is an error, so it unwinds the invocation
    (is (subtypep 'wasmdown.wasi:wasi-exit 'error))))

;;; ---------------------------------------------------------------
;;; The framework: a provider is a comp-instance of lifted host funcs
;;; with the WIT-derived functypes, keyed by the versioned import names.

(test wasi/provider-instance-shape
  (let* ((provider (wasmdown.wasi:clocks-provider))
         (names (mapcar #'car provider))
         (mono (cdr (assoc "wasi:clocks/monotonic-clock@0.3.2" provider
                           :test #'string=)))
         (sys (cdr (assoc "wasi:clocks/system-clock@0.3.2" provider
                          :test #'string=))))
    (is (member "wasi:clocks/types@0.3.2" names :test #'string=))
    (is (member "wasi:clocks/monotonic-clock@0.3.2" names :test #'string=))
    (is (member "wasi:clocks/system-clock@0.3.2" names :test #'string=))
    (is (wasmdown.component.runtime:comp-instance-p mono))
    ;; exported functions are the runtime's own lifted-func values
    (let ((now (provider-fn mono "now")))
      (is (wasmdown.component.runtime:lifted-func-p now))
      (let ((ct (wasmdown.component.runtime:lifted-func-type now)))
        (is (eq (wasmdown.component:ct-kind ct) :func))
        (is (eq (wasmdown.component:ct-kind
                 (second (wasmdown.component:ct-payload ct)))
                :prim))
        (is (eq (wasmdown.component:ct-payload
                 (second (wasmdown.component:ct-payload ct)))
                :u64))))
    ;; the type exports ride the same table (a consumer can alias them)
    (is (eq (car (gethash "duration"
                          (wasmdown.component.runtime:comp-instance-exports
                           (cdr (assoc "wasi:clocks/types@0.3.2" provider
                                       :test #'string=)))))
            :type))
    ;; the async waits are supported, but need the context's reactor:
    ;; without one they report the missing capability (never a busy
    ;; wait, never a silent success)
    (is (block nil
          (handler-case
              (progn (call-provider mono "wait-until" 0) :returned)
            (wasmdown:host-error (c)
              (search "no reactor capability"
                      (wasmdown:wasmdown-error-message c))))))
    (is (block nil
          (handler-case
              (progn (call-provider mono "wait-for" 0) :returned)
            (wasmdown:host-error (c)
              (search "no reactor capability"
                      (wasmdown:wasmdown-error-message c))))))
    ;; direct host calls return the WIT domain values
    (is (integerp (call-provider mono "now")))
    (is (= 1 (call-provider mono "get-resolution")))
    (let ((instant (call-provider sys "now")))
      (is (string= (car (first instant)) "seconds"))
      (is (string= (car (second instant)) "nanoseconds")))))

;;; ---------------------------------------------------------------
;;; Provider completeness (the M17 coverage audit, as a test).
;;;
;;; `provider-instance' used to export a declared-but-unimplemented
;;; function as a trapping stub. It now refuses at construction time,
;;; and the audit below pins the fact that nothing was hiding behind
;;; the old default: every function/method the vendored 0.3.2 WIT
;;; declares in an interface this runtime provides is implemented, and
;;; every gate on that surface is the stable `@since(0.3.0)'.

(defun %wasi-interface-for-import-name (name)
  "The WIT interface whose versioned import name is NAME, over the
packages this runtime provides."
  (dolist (short '("clocks" "random" "cli" "filesystem" "sockets" "http"))
    (let ((found nil))
      (maphash (lambda (key interface)
                 (declare (ignore key))
                 (when (and (null found)
                            (string= (wasmdown.wit:interface-import-name
                                      interface)
                                     name))
                   (setf found interface)))
               (wasmdown.wit:wit-package-interfaces
                (wasmdown.wasi:wasi-package short)))
      (when found (return found)))))

(defun %wasi-declared-exports (interface)
  "Every function/method export name INTERFACE declares, as an alist
(NAME . ANNOTATIONS) in declaration order."
  (let (out)
    (dolist (fn (wasmdown.wit:wit-interface-func-list interface))
      (push (cons (wasmdown.wit:wit-func-name fn)
                  (wasmdown.wit:wit-func-annotations fn))
            out))
    (dolist (res (wasmdown.wit:wit-interface-resource-list interface))
      (dolist (m (wasmdown.wit:wit-resource-methods res))
        (push (cons (wasmdown.wasi::%resource-method-export-name res m)
                    (wasmdown.wit:wit-func-annotations m))
              out)))
    (nreverse out)))

(defun %wasi-exported-func-names (instance)
  "The WIT-DECLARED function names an instance exports. The implicit
`[resource-drop]R' destructors are ABI-level exports (every imported
resource needs one), not WIT functions, so they are excluded from the
declared-surface comparison."
  (let (out)
    (maphash (lambda (key value)
               (when (and (eq (car value) :func)
                          (not (eql 0 (search "[resource-drop]" key))))
                 (push key out)))
             (wasmdown.component.runtime:comp-instance-exports instance))
    out))

(test wasi/provider-surface-covers-every-stable-wit-name
  ;; Every provider the runtime registers builds — which under the new
  ;; PROVIDER-INSTANCE contract already proves each declared function
  ;; has an implementation — and the exported function set is EXACTLY
  ;; the WIT-declared set, so no name falls to a stub.
  (flet ((sorted (l) (sort (copy-list l) #'string<)))
    (let* ((ctx (wasmdown.wasi:make-wasi-context
                 :arguments '("prog")
                 :environment '(("K" . "V"))
                 :cwd "/"
                 :stdin (make-string-input-stream "")
                 :stdout (make-string-output-stream)
                 :stderr (make-string-output-stream)))
           (builders (wasmdown.wasi::%wasi-provider-builders ctx))
           (declared-total 0)
           (exported-total 0))
      ;; the audited surface: 22 interface imports (21 of them declare
      ;; functions; clocks/types is type-only)
      (is (= 22 (length builders)))
      (dolist (entry builders)
        (let* ((import-name (car entry))
               (interface (%wasi-interface-for-import-name import-name))
               (declared (%wasi-declared-exports interface))
               (instance (funcall (cdr entry)))
               (exports (wasmdown.component.runtime:comp-instance-exports
                         instance))
               (exported (%wasi-exported-func-names instance)))
          (is interface)
          (is (wasmdown.component.runtime:comp-instance-p instance))
          ;; exported function set == WIT-declared set, exactly
          (is (equal (sorted (mapcar #'car declared)) (sorted exported)))
          (is (= (length declared) (length exported)))
          (dolist (declared-entry declared)
            (let ((name (car declared-entry))
                  (annotations (cdr declared-entry)))
              (incf declared-total)
              (incf exported-total)
              ;; every declared name is an exported lifted host function
              (let ((hit (gethash name exports)))
                (is (eq (car hit) :func))
                (is (wasmdown.component.runtime:lifted-func-p (cdr hit))))
              ;; and nothing on the provided surface is `unstable': the
              ;; coverage claim is about the STABLE API
              (is (null (member "unstable" annotations
                                :key #'wasmdown.wit:wit-annotation-name
                                :test #'string=)))))))
      ;; the audit's totals: 125 declared = 125 exported
      (is (= 125 declared-total))
      (is (= 125 exported-total))
      ;; Deliberately-absent names are NOT stubs inside a provider: the
      ;; whole interfaces are absent from the registry, so no trapping
      ;; closure can be bound for them. The refusal itself lives in
      ;; COMPOSE-WORLD-PROVIDERS (wasi/world-provider-unsupported-
      ;; import-is-construction-error).
      (let ((names (mapcar #'car
                           (wasmdown.wasi::%wasi-provider-builders
                            (wasmdown.wasi:make-wasi-context)))))
        (is (not (member "wasi:clocks/timezone@0.3.2" names
                         :test #'string=)))
        ;; the outgoing HTTP surface is served; the incoming handler
        ;; direction is not
        (is (member "wasi:http/types@0.3.2" names :test #'string=))
        (is (member "wasi:http/client@0.3.2" names :test #'string=))
        (is (not (member "wasi:http/handler@0.3.2" names
                         :test #'string=)))))))

(test wasi/provider-instance-refuses-unimplemented-function
  ;; A declared function the alist does not cover is a CONSTRUCTION-TIME
  ;; host-error naming the interface and the function — never an
  ;; exported trapping stub.
  (let* ((package (wasmdown.wasi:wasi-package "clocks"))
         (iface (wasmdown.wit:package-interface package "monotonic-clock")))
    (flet ((refusal (implementations)
             (let ((message nil))
               (handler-case
                   (progn (wasmdown.wasi:provider-instance
                           iface implementations :packages (list package))
                          nil)
                 (wasmdown:host-error (c)
                   (setf message (wasmdown:wasmdown-error-message c))))
               message)))
      ;; the complete alist still builds
      (is (wasmdown.component.runtime:comp-instance-p
           (wasmdown.wasi:provider-instance
            iface
            (list (cons "now" #'wasmdown.wasi:monotonic-now)
                  (cons "get-resolution" #'wasmdown.wasi:monotonic-resolution)
                  (cons "wait-until" (lambda (when) when))
                  (cons "wait-for" (lambda (how-long) how-long)))
            :packages (list package))))
      ;; one missing function: refused, naming BOTH
      (let ((message (refusal (list (cons "now" #'wasmdown.wasi:monotonic-now)))))
        (is message)
        (is (search "monotonic-clock" message))
        (is (search "get-resolution" message))
        (is (search "refusing" message)))
      ;; the retired :UNSUPPORTED marker is not an escape hatch either
      (let ((message (refusal
                      (list (cons "now" :unsupported)
                            (cons "get-resolution"
                                  #'wasmdown.wasi:monotonic-resolution)
                            (cons "wait-until" (lambda (when) when))
                            (cons "wait-for" (lambda (how-long) how-long))))))
        (is message)
        (is (search "now" message)))))
  ;; a resource METHOD is held to the same rule; the message reports the
  ;; method's component core name
  (let* ((package (wasmdown.wasi:wasi-package "filesystem"))
         (iface (wasmdown.wit:package-interface package "types"))
         (message
           (handler-case
               (progn (wasmdown.wasi:provider-instance
                       iface nil
                       :packages (wasmdown.wasi::%filesystem-packages
                                  :package package))
                      nil)
             (wasmdown:host-error (c)
               (wasmdown:wasmdown-error-message c)))))
    (is message)
    (is (search "types" message))
    (is (search "[method]descriptor." message))))

;;; ---------------------------------------------------------------
;;; The end-to-end fixture: a real component text that imports the
;;; versioned interface instance, aliases its functions, canon-lowers
;;; them for a core module, and lifts a call back out. Supplied through
;;; instantiate-component's :with-args.

(defparameter *monotonic-fixture*
  "(component
     (import \"wasi:clocks/monotonic-clock@0.3.2\"
       (instance $clk (export \"now\" (func (result u64)))
                      (export \"get-resolution\" (func (result u64)))))
     (alias export $clk \"now\" (func $now))
     (alias export $clk \"get-resolution\" (func $res))
     (core func $now-lowered (canon lower (func $now)))
     (core func $res-lowered (canon lower (func $res)))
     (core module $M
       (import \"clk\" \"now\" (func $n (result i64)))
       (import \"clk\" \"res\" (func $r (result i64)))
       (func (export \"now\") (result i64) (call $n))
       (func (export \"res\") (result i64) (call $r)))
     (core instance $m
       (instantiate $M
         (with \"clk\"
           (instance (export \"now\" (func $now-lowered))
                     (export \"res\" (func $res-lowered))))))
     (func (export \"now\") (result u64)
       (canon lift (core func $m \"now\")))
     (func (export \"res\") (result u64)
       (canon lift (core func $m \"res\"))))")

(test wasi/e2e-monotonic-clock-component
  (let* ((comp (wasmdown.component.text::parse-component-text
                *monotonic-fixture*))
         (provider (wasmdown.wasi:clocks-provider)))
    (wasmdown.component:validate-component comp)
    (let ((inst (wasmdown.component.runtime:instantiate-component
                 comp :with-args provider)))
      (let* ((now-fn (wasmdown.component.runtime:runtime-export-value
                      inst "now"))
             (res-fn (wasmdown.component.runtime:runtime-export-value
                      inst "res")))
        (is (wasmdown.component.runtime:lifted-func-p now-fn))
        (let ((a (wasmdown.component.runtime:lifted-func-call now-fn))
              (b (wasmdown.component.runtime:lifted-func-call now-fn)))
          (is (integerp a))
          (is (plusp a))
          ;; the host clock is monotonic through the whole call path
          (is (<= a b)))
        (is (plusp (wasmdown.component.runtime:lifted-func-call res-fn)))))))

;;; ---------------------------------------------------------------
;;; The ASYNC clock waits: wait-until/wait-for answer with a
;;; HOST-PENDING armed ONCE on the context's reactor. Every test
;;; injects a fake clock, so nothing here sleeps or polls in a loop.

(defun clock-pending-call (instance name &rest args)
  "Call an async clock provider's DOMAIN closure (not the joined
lifted call), so the test can drive the pending and the reactor
itself."
  (funcall (wasmdown.component.runtime:lifted-func-host
            (provider-fn instance name))
           args))

(defun clock-deadline-count (reactor)
  "The reactor's live deadline registrations: the pending count the
wait tests assert against."
  (length (wasmdown.wasi::wasi-reactor-deadlines reactor)))

(defun clock-continuation (pending)
  "Attach the pending's completion continuation, recording the
delivered OUTCOME in the returned cons's CDR (:UNSET until then)."
  (let ((seen (cons :unset :unset)))
    (rtcall "%HOST-PENDING-SET-CONTINUATION"
            pending (lambda (outcome) (setf (cdr seen) outcome)))
    seen))

(test wasi/clock-pending-future
  ;; a future deadline resolves through the reactor STEP and the store's
  ;; pump: the provider thread never sits in a bounded-step loop
  (let* ((now 1000)
         (reactor (reactor-fake (lambda () now) nil))
         (mono (wasmdown.wasi:monotonic-clock-instance :reactor reactor))
         (pending (clock-pending-call mono "wait-until" 5000))
         (seen (clock-continuation pending)))
    (unwind-protect
         (progn
           ;; the call PARKED: the timer is the one-shot registration
           (is (eq :waiting (rtcall "HOST-PENDING-STATE" pending)))
           (is (not (rtcall "HOST-PENDING-TERMINAL-P" pending)))
           (is (= 1 (clock-deadline-count reactor)))
           ;; a step before the deadline neither fires nor resolves
           (multiple-value-bind (progress reactor-pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is (not progress))
             (is reactor-pending))
           (is (not (rtcall "HOST-PENDING-TERMINAL-P" pending)))
           (is (eq :unset (cdr seen)))
           ;; the deadline arrives: the reactor fires the ONE-SHOT timer
           (setf now 5000)
           (multiple-value-bind (progress reactor-pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is progress)
             (is (not reactor-pending)))
           (is (eq :resolved (rtcall "HOST-PENDING-STATE" pending)))
           ;; a resolved wait leaves NO timer registered
           (is (= 0 (clock-deadline-count reactor)))
           ;; ... and the continuation has not run: delivery belongs to
           ;; the pump, never to the reactor callback
           (is (eq :unset (cdr seen)))
           (rtcall "%HOST-PUMP")
           (is (equal '(:resolve) (cdr seen))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/clock-pending-future-wait-for
  ;; wait-for computes NOW + HOW-LONG on the reactor's clock and arms
  ;; the same one-shot deadline
  (let* ((now 7000)
         (reactor (reactor-fake (lambda () now) nil))
         (mono (wasmdown.wasi:monotonic-clock-instance :reactor reactor))
         (pending (clock-pending-call mono "wait-for" 3000))
         (seen (clock-continuation pending)))
    (unwind-protect
         (progn
           (is (eq :waiting (rtcall "HOST-PENDING-STATE" pending)))
           (is (= 10000
                  (wasmdown.wasi::reactor-registration-deadline
                   (first (wasmdown.wasi::wasi-reactor-deadlines reactor)))))
           (setf now 9999)
           (multiple-value-bind (progress)
               (wasmdown.wasi:reactor-step reactor 0)
             (is (not progress)))
           (is (eq :waiting (rtcall "HOST-PENDING-STATE" pending)))
           (setf now 10000)
           (multiple-value-bind (progress)
               (wasmdown.wasi:reactor-step reactor 0)
             (is progress))
           (is (eq :resolved (rtcall "HOST-PENDING-STATE" pending)))
           (is (= 0 (clock-deadline-count reactor)))
           (rtcall "%HOST-PUMP")
           (is (equal '(:resolve) (cdr seen))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/clock-pending-immediate
  ;; a past or zero deadline resolves IMMEDIATELY with NO timer
  ;; registered — the call still has the pending's shape
  (let* ((now 1000)
         (reactor (reactor-fake (lambda () now) nil))
         (mono (wasmdown.wasi:monotonic-clock-instance :reactor reactor)))
    (unwind-protect
         (dolist (case (list (cons "wait-until" 1000)
                             (cons "wait-until" 999)
                             (cons "wait-for" 0)))
           (let* ((pending (clock-pending-call mono (car case) (cdr case)))
                  (seen (clock-continuation pending)))
             (is (eq :resolved (rtcall "HOST-PENDING-STATE" pending)))
             (is (equal '(:resolve)
                        (rtcall "HOST-PENDING-OUTCOME" pending)))
             ;; the immediate path never touches the reactor
             (is (= 0 (clock-deadline-count reactor)))
             (rtcall "%HOST-PUMP")
             (is (equal '(:resolve) (cdr seen)))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/clock-pending-overflow
  ;; NOW + HOW-LONG is checked against the u64 mark space: overflow
  ;; TRAPS, it never wraps into an immediate (or bogus) wait
  (let* ((now (- #xffffffffffffffff 5))
         (reactor (reactor-fake (lambda () now) nil))
         (mono (wasmdown.wasi:monotonic-clock-instance :reactor reactor)))
    (unwind-protect
         (progn
           ;; exactly representable: the deadline IS u64-max, no trap
           (let ((pending (clock-pending-call mono "wait-for" 5)))
             (is (eq :waiting (rtcall "HOST-PENDING-STATE" pending)))
             (is (= #xffffffffffffffff
                    (wasmdown.wasi::reactor-registration-deadline
                     (first (wasmdown.wasi::wasi-reactor-deadlines reactor)))))
             (rtcall "%HOST-PENDING-REQUEST-CANCEL" pending)
             (rtcall "%HOST-PUMP")
             (is (= 0 (clock-deadline-count reactor))))
           ;; one nanosecond past the mark space traps (no wrap)
           (assert-trap :generic (clock-pending-call mono "wait-for" 6))
           (is (= 0 (clock-deadline-count reactor))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/clock-pending-cancel
  ;; cancel removes the one-shot timer EXACTLY once; the cancelled wait
  ;; can never resolve afterwards
  (let* ((now 1000)
         (reactor (reactor-fake (lambda () now) nil))
         (mono (wasmdown.wasi:monotonic-clock-instance :reactor reactor))
         (pending (clock-pending-call mono "wait-for" 5000))
         (seen (clock-continuation pending)))
    (unwind-protect
         (progn
           (is (= 1 (clock-deadline-count reactor)))
           ;; the runtime's cancel path is QUEUED, then run on the pump
           (rtcall "%HOST-PENDING-REQUEST-CANCEL" pending)
           (is (eq :waiting (rtcall "HOST-PENDING-STATE" pending)))
           (rtcall "%HOST-PUMP")
           (is (eq :cancelled (rtcall "HOST-PENDING-STATE" pending)))
           (is (equal '(:cancel)
                      (rtcall "HOST-PENDING-OUTCOME" pending)))
           ;; the timer is GONE: the cancel reached the reactor once
           (is (= 0 (clock-deadline-count reactor)))
           ;; a deadline that passes later cannot resurrect it
           (setf now 9000)
           (multiple-value-bind (progress reactor-pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is (not progress))
             (is (not reactor-pending)))
           (rtcall "%HOST-PUMP")
           (is (eq :cancelled (rtcall "HOST-PENDING-STATE" pending)))
           (is (equal '(:cancel) (cdr seen))))
      (wasmdown.wasi:reactor-close reactor))))

;;; ===============================================================
;;; M16 SLICE 2 — THE HOST STREAM/FUTURE ABI + wasi:cli stdio.
;;; ===============================================================

(defparameter *u8-ct*
  (wasmdown.component:make-ct :kind :prim :payload :u8))

(defun %completed (n) (logior 0 (ash n 4)))

;;; ---------------------------------------------------------------
;;; The host-backed ENDS themselves (a CL-only round trip: no guest
;;; memory, no guest instance — the copy protocol's case 2 consults
;;; the host backing directly).

(test wasi/host-sink-end-roundtrip
  ;; The guest's writable end stages 2 elements; its peer is a HOST
  ;; readable end whose SINK collects them. The write completes with
  ;; COMPLETED|2<<4 — the canonical stream protocol, host as the peer.
  (let ((sink-bytes '()))
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
      (wasmdown.component.runtime:host-end-set-sink
       reader (lambda (octets start end)
                (setf sink-bytes (coerce (subseq octets start end) 'list))
                (values (- end start) nil)))
      (let ((staged (wasmdown.component.runtime:make-host-buffer
                     2 :values '(104 105))))
        ;; the guest's write (case 2: the peer has no buffer)
        (rtcall "%END-COPY" writer staged nil)
        (is (equal '(104 105) sink-bytes))
        (is (eql (%completed 2)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer))))
        ;; the stream write end returns to idle after delivery
        (is (eq :idle (rtcall "ASYNC-END-STATE" writer)))))))

(test wasi/host-sink-zero-length-write
  ;; Regression (http-service-echo's empty-body POST): a ZERO-LENGTH
  ;; write into a host-sink-backed stream must complete at once. It
  ;; used to park the empty buffer on the peer (End.copy case 2) with
  ;; no sink call ever pending, so the guest's `write_all' of an empty
  ;; response body waited forever and the provider's transmission
  ;; future never resolved (the serve loop then timed the response
  ;; out). The correct acknowledgement is COMPLETED|0<<4 on the
  ;; writer end, with no sink call.
  (multiple-value-bind (reader writer)
      (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
    (let ((calls 0))
      (wasmdown.component.runtime:host-end-set-sink
       reader (lambda (octets start end)
                (declare (ignore octets start end))
                (incf calls)
                (values 0 nil)))
      (let ((staged (wasmdown.component.runtime:make-host-buffer 0)))
        (rtcall "%END-COPY" writer staged nil)
        (is (zerop calls))
        (is (eql (%completed 0)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer))))
        (is (eq :idle (rtcall "ASYNC-END-STATE" writer)))))))

(test wasi/host-sink-would-block-retry-keeps-generation
  ;; Regression: a WOULD-BLOCK sink parks the copy; the readiness retry
  ;; must REUSE that parked attempt. Clearing it made the retry advance
  ;; the generation and resubmit the same span to the worker, so every
  ;; readiness wrote the bytes again (the P3 stdout duplication).
  (multiple-value-bind (reader writer)
      (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
    (let ((calls 0) (sink-bytes '()))
      (wasmdown.component.runtime:host-end-set-sink
       reader (lambda (octets start end)
                (incf calls)
                (if (= calls 1)
                    (values 0 nil)                       ; WOULD-BLOCK, parked
                    (progn
                      (setf sink-bytes
                            (append sink-bytes
                                    (coerce (subseq octets start end) 'list)))
                      (values (- end start) nil)))))
      (let ((staged (wasmdown.component.runtime:make-host-buffer
                     2 :values '(104 105))))
        (is (eql 0 (rtcall "%END-COPY" writer staged nil)))
        (let ((generation
                (wasmdown.component.runtime:host-end-generation reader)))
          ;; the worker completes at the parked attempt's generation
          (wasmdown.component.runtime:host-end-ready reader generation)
          (is (= 2 calls))
          (is (equal '(104 105) sink-bytes))
          (is (eql generation
                     (wasmdown.component.runtime:host-end-generation reader)))
          (is (eql (%completed 2)
                   (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer)))))))))

(test wasi/host-source-end-eof-dropped
  ;; The guest's readable end pulls from a HOST writable end's SOURCE:
  ;; the bytes arrive COMPLETED|n<<4; the source's EOF drops the host
  ;; writer, so the next read is DROPPED (the canonical 1).
  (multiple-value-bind (reader writer)
      (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
    (let ((pulls 0))
      (wasmdown.component.runtime:host-end-set-source
       writer (lambda (max)
                (incf pulls)
                (if (= pulls 1)
                    (values '(1 2 3) t)     ; bytes AND eof
                    (values nil t))))
      (let ((buf (wasmdown.component.runtime:make-host-buffer 8)))
        (rtcall "%END-COPY" reader buf t)
        (is (equal '(1 2 3)
                   (wasmdown.component.runtime:host-buffer-values buf)))
        (is (eql (%completed 3)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader)))))
      (let ((buf (wasmdown.component.runtime:make-host-buffer 8)))
        (rtcall "%END-COPY" reader buf t)
        ;; DROPPED == 1 (copy result 1, no progress for a stream)
        (is (eql 1 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader))))))))

(test wasi/host-future-resolve
  ;; A host future: the provider resolves the WRITABLE end; the guest's
  ;; READABLE read delivers COMPLETED (0) with the value at its buffer.
  (let ((fct (wasmdown.component:make-ct
              :kind :result
              :payload (cons nil (wasmdown.component:make-ct
                                  :kind :enum
                                  :payload (vector "io" "pipe"))))))
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-future-pair fct)
      (wasmdown.component.runtime:host-future-resolve writer '(:ok))
      (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
        (rtcall "%END-COPY" reader buf t)
        (is (equal '((:ok))
                   (wasmdown.component.runtime:host-buffer-values buf)))
        (is (eql 0 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader))))))
    ;; the error case rides the same slot
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-future-pair fct)
      (wasmdown.component.runtime:host-future-resolve writer '(:err . "io"))
      (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
        (rtcall "%END-COPY" reader buf t)
        (is (equal '((:err . "io"))
                   (wasmdown.component.runtime:host-buffer-values buf)))))))

;;; ---------------------------------------------------------------
;;; wasi:cli stdio over the CONTEXT's CL streams (no ambient globals).

(test wasi/cli-stdout-write-via-stream
  (let* ((sink (make-string-output-stream))
         (ctx (wasmdown.wasi:make-wasi-context :stdout sink))
         (inst (wasmdown.wasi:cli-stdout-instance ctx))
         (fn (provider-fn inst "write-via-stream")))
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
      ;; the guest writes BEFORE handing the end over: the provider's
      ;; registration drains the parked buffer
      (rtcall "%END-COPY" writer
              (wasmdown.component.runtime:make-host-buffer
               3 :values '(104 105 33))
              nil)
      (let ((future (wasmdown.component.runtime:lifted-func-call fn reader)))
        (is (wasmdown.component.runtime:async-end-p future))
        (is (string= "hi!" (get-output-stream-string sink)))
        (is (eql (%completed 3)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer))))
        ;; a second write after registration drains straight through
        (rtcall "%END-COPY" writer
                (wasmdown.component.runtime:make-host-buffer
                 2 :values '(10 10))
                nil)
        (is (string= (format nil "~c~c" #\Newline #\Newline)
                     (get-output-stream-string sink)))
        ;; dropping the guest's writable end resolves the future
        (rtcall "%END-DROP" writer)
        (let ((fbuf (wasmdown.component.runtime:make-host-buffer 1)))
          (rtcall "%END-COPY" future fbuf t)
          (is (equal '((:ok))
                     (wasmdown.component.runtime:host-buffer-values fbuf)))
          (is (eql 0 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" future)))))))))

(test wasi/cli-stderr-write-via-stream
  (let* ((sink (make-string-output-stream))
         (ctx (wasmdown.wasi:make-wasi-context :stderr sink))
         (inst (wasmdown.wasi:cli-stderr-instance ctx))
         (fn (provider-fn inst "write-via-stream")))
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
      (wasmdown.component.runtime:lifted-func-call fn reader)
      (rtcall "%END-COPY" writer
              (wasmdown.component.runtime:make-host-buffer 2 :values '(111 107))
              nil)
      (is (string= "ok" (get-output-stream-string sink))))))

(test wasi/cli-stdin-read-via-stream
  (let* ((source (make-string-input-stream "hello"))
         (ctx (wasmdown.wasi:make-wasi-context :stdin source))
         (inst (wasmdown.wasi:cli-stdin-instance ctx))
         (fn (provider-fn inst "read-via-stream")))
    (let* ((pair (wasmdown.component.runtime:lifted-func-call fn))
           (stream (first pair))
           (future (second pair)))
      (is (= 2 (length pair)))
      (is (wasmdown.component.runtime:async-end-p stream))
      (is (wasmdown.component.runtime:async-end-p future))
      ;; the guest reads the whole source
      (let ((buf (wasmdown.component.runtime:make-host-buffer 5)))
        (rtcall "%END-COPY" stream buf t)
        (is (string= "hello"
                     (coerce (mapcar #'code-char
                                     (wasmdown.component.runtime:host-buffer-values
                                      buf))
                             'string)))
        (is (eql (%completed 5)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" stream)))))
      ;; the source is exhausted: the next read is DROPPED
      (let ((buf (wasmdown.component.runtime:make-host-buffer 5)))
        (rtcall "%END-COPY" stream buf t)
        (is (eql 1 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" stream)))))
      ;; the read result future is already resolved (success)
      (let ((fbuf (wasmdown.component.runtime:make-host-buffer 1)))
        (rtcall "%END-COPY" future fbuf t)
        (is (equal '((:ok))
                   (wasmdown.component.runtime:host-buffer-values fbuf)))))))

(test wasi/cli-stdio-providers-span-contract
  ;; M17 step 5: the CLI providers speak the byte contract directly —
  ;; the sink consumes a SPAN and reports (values count error), the
  ;; source yields a REUSABLE u8 VECTOR whose length is the count
  ;; (never a one-argument byte list).
  (let ((out (make-string-output-stream)))
    (let ((sink (wasmdown.wasi:cl-stream-sink out)))
      (multiple-value-bind (consumed err)
          (funcall sink (make-array 4 :element-type '(unsigned-byte 8)
                                      :initial-contents '(97 98 99 100))
                   1 3)
        (is (eql 2 consumed))
        (is (null err)))
      (is (string= "bc" (get-output-stream-string out)))
      ;; an empty span consumes nothing and is not an error
      (multiple-value-bind (consumed err)
          (funcall sink (make-array 1 :element-type '(unsigned-byte 8)) 0 0)
        (is (eql 0 consumed))
        (is (null err)))))
  (let ((in (make-string-input-stream "hello")))
    (let ((source (wasmdown.wasi:cl-stream-source in)))
      (multiple-value-bind (octets eof) (funcall source 3)
        (is (vectorp octets))
        (is (not (listp octets)))
        (is (string= "hel" (map 'string #'code-char octets)))
        (is (null eof)))
      (multiple-value-bind (octets eof) (funcall source 8)
        (is (string= "lo" (map 'string #'code-char octets)))
        (is eof))
      ;; exhausted: a reusable empty vector and EOF (not would-block)
      (multiple-value-bind (octets eof) (funcall source 8)
        (is (= 0 (length octets)))
        (is eof)))))

(test wasi/cli-stdio-capability-withdrawn
  ;; An absent stream is a withdrawn capability: the interface is still
  ;; exported, but calling it traps with host-error (never an ambient
  ;; *STANDARD-OUTPUT*).
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (out (wasmdown.wasi:cli-stdout-instance ctx))
         (in (wasmdown.wasi:cli-stdin-instance ctx)))
    (is (block nil
          (handler-case
              (progn (call-provider out "write-via-stream" :not-an-end) :returned)
            (wasmdown:host-error (c)
              (search "capability" (wasmdown:wasmdown-error-message c))))))
    (is (block nil
          (handler-case
              (progn (call-provider in "read-via-stream") :returned)
            (wasmdown:host-error (c)
              (search "capability" (wasmdown:wasmdown-error-message c)))))))
  ;; the provider surfaces are keyed by the versioned WIT import names
  (let ((names (mapcar #'car
                       (wasmdown.wasi:cli-provider
                        (wasmdown.wasi:make-wasi-context)))))
    (is (member "wasi:cli/stdin@0.3.2" names :test #'string=))
    (is (member "wasi:cli/stdout@0.3.2" names :test #'string=))
    (is (member "wasi:cli/stderr@0.3.2" names :test #'string=))
    (is (member "wasi:cli/environment@0.3.2" names :test #'string=))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END FIXTURE: a component imports wasi:cli/stdout@0.3.2,
;;; canon-lowers write-via-stream, hands it its stream's readable end,
;;; writes through its own stream, drops the writable end and reads the
;;; result future. The bytes land in the CONTEXT's sink.

(defparameter *stdout-fixture*
  (concatenate 'string
   "(component"
   " (type $ERR (enum \"io\" \"illegal-byte-sequence\" \"pipe\"))"
   " (type $ST (stream u8))"
   " (import \"wasi:cli/stdout@0.3.2\""
   "   (instance $out"
   "     (export \"error-code\" (type (eq $ERR)))"
   "     (type $r (result (error $ERR)))"
   "     (type $f (future $r))"
   "     (type $s (stream u8))"
   "     (export \"write-via-stream\""
   "       (func (param \"data\" $s) (result $f)))))"
   " (alias export $out \"write-via-stream\" (func $wvs))"
   " (core func $wvs-lowered (canon lower (func $wvs)))"
   " (core module $Memory (memory (export \"mem\") 1))"
   " (core instance $memory (instantiate $Memory))"
   " (core module $M"
   "  (import \"env\" \"mem\" (memory 1))"
   "  (import \"env\" \"stream.new\" (func $snew (result i64)))"
   "  (import \"env\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))"
   "  (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))"
   "  (import \"env\" \"future.read\" (func $fread (param i32 i32) (result i32)))"
   "  (import \"env\" \"write-via-stream\" (func $wvs (param i32) (result i32)))"
   "  (global $fut (mut i32) (i32.const 0))"
   "  (func (export \"f\") (result i32)"
   "    (local $p i64) (local $rx i32) (local $tx i32) (local $ret i32)"
   "    (local.set $p (call $snew))"
   "    (local.set $rx (i32.wrap_i64 (local.get $p)))"
   "    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $p) (i64.const 32))))"
   "    (i32.store16 (i32.const 16) (i32.const 0x6968))"
   "    (global.set $fut (call $wvs (local.get $rx)))"
   "    (local.set $ret (call $swrite (local.get $tx) (i32.const 16) (i32.const 2)))"
   "    (if (i32.ne (i32.const 32) (local.get $ret)) (then unreachable))"
   "    (call $sdropw (local.get $tx))"
   "    (local.set $ret (call $fread (global.get $fut) (i32.const 32)))"
   "    (if (i32.ne (i32.const 0) (local.get $ret)) (then unreachable))"
   "    (i32.load8_u (i32.const 32)))"
   " )"
   " (type $FT (future (result (error $ERR))))"
   " (canon stream.new $ST (core func $snew))"
   " (canon stream.write $ST (memory (core memory $memory \"mem\")) (core func $swrite))"
   " (canon stream.drop-writable $ST (core func $sdropw))"
   " (canon future.read $FT (memory (core memory $memory \"mem\")) (core func $fread))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $memory \"mem\"))"
   "   (export \"stream.new\" (func $snew))"
   "   (export \"stream.write\" (func $swrite))"
   "   (export \"stream.drop-writable\" (func $sdropw))"
   "   (export \"future.read\" (func $fread))"
   "   (export \"write-via-stream\" (func $wvs-lowered))))))"
   " (func (export \"f\") (result u32) (canon lift (core func $m \"f\"))))"))

(test wasi/e2e-stdout-component
  (let* ((sink (make-string-output-stream))
         (context (wasmdown.wasi:make-wasi-context :stdout sink))
         (comp (wasmdown.component.text::parse-component-text
                *stdout-fixture*)))
    (wasmdown.component:validate-component comp)
    (let* ((inst (wasmdown.component.runtime:instantiate-component
                  comp :with-args (wasmdown.wasi:cli-provider context)))
           (f (wasmdown.component.runtime:runtime-export-value inst "f")))
      (is (wasmdown.component.runtime:lifted-func-p f))
      ;; the guest's 2 stream bytes ("hi") reached the context's sink
      ;; and the result future's ok discriminant is 0
      (is (eql 0 (wasmdown.component.runtime:lifted-func-call f)))
      (is (string= "hi" (get-output-stream-string sink)))
      ;; a second invocation sees a fresh, independently-authorized run
      (is (eql 0 (wasmdown.component.runtime:lifted-func-call f)))
      (is (string= "hi" (get-output-stream-string sink))))))

;;; ===============================================================
;;; M16 SLICE 3a — wasi:filesystem: the capability model, the
;;; descriptor resource, and the non-stream surface.
;;; ===============================================================

;;; ---------------------------------------------------------------
;;; Fixtures: a private tree under /tmp, and the cleanup that removes
;;; exactly the entries the tests create (files, symlinks, then
;;; directories deepest-first) — no shell, no third-party library.

(defun fs-p (dir name) (format nil "~a/~a" dir name))

(defun fs-mkdir! (path &optional (mode #o755))
  (sb-unix:unix-mkdir path mode))

(defun fs-symlink! (target path)
  "A HOST-side symlink (the test plays the part of an outside
process planting a link inside the root)."
  (wasmdown.wasi::%libc-symlinkat target -100 path))

(defun fs-write! (path content)
  (with-open-file (s path :direction :output :if-exists :overwrite
                          :if-does-not-exist :create)
    (write-string content s))
  path)

(defun fs-poke-bytes! (path n)
  "Create PATH with N zero bytes (open-at/set-size fixtures)."
  (with-open-file (s path :direction :output :if-exists :overwrite
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (dotimes (i n) (write-byte 0 s)))
  path)

(defun fs-test-unlink-tree (root &optional names)
  "Remove NAMES under ROOT (files/symlinks/dirs) and then ROOT."
  (dolist (n names)
    (let ((p (fs-p root n)))
      (ignore-errors (wasmdown.wasi::%libc-unlinkat -100 p 0))
      (ignore-errors
        (wasmdown.wasi::%libc-unlinkat -100 p wasmdown.wasi::+at-removedir+))))
  (ignore-errors
    (wasmdown.wasi::%libc-unlinkat -100 root wasmdown.wasi::+at-removedir+)))

(defun fs-test-root (tag)
  (let ((dir (format nil "/tmp/wasmdown-~a-~d-~d"
                     tag (sb-unix:unix-getpid)
                     (random 1000000))))
    (unless (fs-mkdir! dir)
      (error "cannot create fixture root ~a" dir))
    dir))

(defmacro with-fs-root ((root-var tag &optional (names nil)) &body body)
  "Run BODY with ROOT-VAR bound to a fresh private tree; the cleanup
removes NAMES (an evaluated list, so callers quote it) and the root."
  `(let ((,root-var (fs-test-root ,tag)))
     (unwind-protect (progn ,@body)
       (fs-test-unlink-tree ,root-var ,names))))

(defun fs-error-code-of (form)
  "FORM must be a (:err . code) result; its code label."
  (unless (and (consp form) (eq (car form) :err))
    (error "expected an error result, got ~s" form))
  (cdr form))

(defun fs-ok-value (form)
  (unless (and (consp form) (eq (car form) :ok))
    (error "expected an ok result, got ~s" form))
  (cdr form))

(defun fs-type-name (stat)
  "The descriptor-type case name of a descriptor-stat domain value."
  (second (assoc "%type" stat :test #'string=)))

(defmacro => (expected actual)
  "Assert (equal EXPECTED ACTUAL) with a useful failure datum."
  `(let ((e ,expected) (a ,actual))
     (unless (equal e a)
       (error "expected ~s, got ~s" e a))))

;;; ---------------------------------------------------------------
;;; The capability object: an OPEN HOST DIRECTORY HANDLE (never a
;;; path prefix), and the operations that go through it.

(test wasi/fs-preopen-capability
  (with-fs-root (root "fs-cap" '("sub" "sub/inner" "a.txt" "lnk"))
    (fs-mkdir! (fs-p root "sub"))
    (fs-mkdir! (fs-p root "sub/inner"))
    (fs-write! (fs-p root "a.txt") "hello")
    (fs-symlink! "a.txt" (fs-p root "lnk"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; the authority IS a real directory handle + a guest name
             (is (plusp (wasmdown.wasi:wasi-preopen-fd pre)))
             (is (string= "/sandbox" (wasmdown.wasi:wasi-preopen-name pre)))
             (is (eq d (wasmdown.wasi:wasi-preopen-descriptor pre)))
             (is (wasmdown.wasi:wasi-descriptor-p d))
             ;; stat of the root: a directory, with the mutate right
             (let ((stat (fs-ok-value (wasmdown.wasi:fs-stat ctx d))))
               (=> "directory" (fs-type-name stat))
               (is (integerp (cdr (assoc "size" stat :test #'string=))))
               (is (member "mutate-directory"
                           (cdr (fs-ok-value (wasmdown.wasi:fs-get-flags ctx d)))
                           :test #'string=)))
             ;; get-type / get-flags
             (=> "directory" (car (fs-ok-value (wasmdown.wasi:fs-get-type ctx d))))
             ;; stat-at: a file, a directory, a symlink (no-follow default)
             (let ((file (fs-ok-value (wasmdown.wasi:fs-stat-at ctx d nil "a.txt"))))
               (=> "regular-file" (fs-type-name file))
               (=> 5 (cdr (assoc "size" file :test #'string=))))
             (=> "directory"
                 (fs-type-name (fs-ok-value (wasmdown.wasi:fs-stat-at ctx d nil "sub"))))
             (=> "symbolic-link"
                 (fs-type-name (fs-ok-value (wasmdown.wasi:fs-stat-at ctx d nil "lnk"))))
             ;; path-flags.symlink-follow expands the FINAL component
             (=> "regular-file"
                 (fs-type-name
                  (fs-ok-value
                   (wasmdown.wasi:fs-stat-at
                    ctx d (cons :flags '("symlink-follow")) "lnk"))))
             ;; nested paths stay handle-relative
             (=> "directory"
                 (fs-type-name
                  (fs-ok-value
                   (wasmdown.wasi:fs-stat-at ctx d nil "sub/inner"))))
             ;; readlink-at: the link TEXT (preopen-relative)
             (=> "a.txt" (fs-ok-value (wasmdown.wasi:fs-readlink-at ctx d "lnk"))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE SECURITY INVARIANTS: no absolute escape, no `..' climb, no
;;; symlink-escape (a host-planted absolute or climbing link), and no
;;; cross-authority descriptor.

(test wasi/fs-no-absolute-or-dotdot-escape
  (with-fs-root (root "fs-escape" '("a.txt"))
    (fs-write! (fs-p root "a.txt") "x")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; absolute paths never reach the host
             (=> "not-permitted"
                 (fs-error-code-of (wasmdown.wasi:fs-stat-at ctx d nil "/etc/passwd")))
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-open-at ctx d nil "/tmp/x"
                                            (cons :flags '("create")) nil)))
             ;; `..' is refused outright (the conservative 0.3 policy):
             ;; no step may climb, so no link or .. can leave the root
             (=> "not-permitted"
                 (fs-error-code-of (wasmdown.wasi:fs-stat-at ctx d nil "..")))
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil "../etc/passwd")))
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil "sub/../../etc")))
             ;; empty is ENOENT (POSIX resolves an empty path to
             ;; no-entry for every *at call; the upstream wasip3
             ;; filesystem tests assert it), NUL is invalid
             (=> "no-entry"
                 (fs-error-code-of (wasmdown.wasi:fs-stat-at ctx d nil "")))
             (=> "invalid"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil (format nil "a~cx" (code-char 0)))))
             ;; create-directory / unlink / rename all share the check
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-create-directory-at ctx d "../evil")))
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-rename-at ctx d "a.txt" d "../evil")))
             ;; the descriptor of another authority is refused
             (let* ((root-b (fs-test-root "fs-escape-b"))
                    (pre-b (wasmdown.wasi:open-wasi-preopen root-b :name "/b"))
                    (ctx-b (wasmdown.wasi:make-wasi-context
                            :preopens (list pre-b))))
               (unwind-protect
                    (progn
                      (=> "not-permitted"
                          (fs-error-code-of
                           (wasmdown.wasi:fs-stat ctx
                                                  (wasmdown.wasi:wasi-preopen-descriptor
                                                   pre-b))))
                      ;; ... and the same descriptor is fine in its own ctx
                      (=> "directory" (car (fs-ok-value
                                            (wasmdown.wasi:fs-get-type
                                             ctx-b
                                             (wasmdown.wasi:wasi-preopen-descriptor
                                              pre-b))))))
                 (wasmdown.wasi:close-wasi-preopen pre-b)
                 (fs-test-unlink-tree root-b))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-symlink-escape-refused
  (with-fs-root (root "fs-link" '("inside.txt" "out-abs" "out-rel" "esc-dir" "good-link" "new-link"))
    (fs-write! (fs-p root "inside.txt") "in")
    ;; a symlink to an absolute host path, planted by the host (the
    ;; guest can also try symlink-at, which refuses up front)
    (fs-symlink! "/etc/passwd" (fs-p root "out-abs"))
    ;; a relative link that climbs above the root
    (fs-symlink! "../../etc/passwd" (fs-p root "out-rel"))
    ;; a link to an absolute DIRECTORY (an intermediate component)
    (fs-symlink! "/etc" (fs-p root "esc-dir"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre))
           (follow (cons :flags '("symlink-follow"))))
      (unwind-protect
           (progn
             ;; no-follow reads the LINK itself
             (=> "symbolic-link"
                 (fs-type-name (fs-ok-value
                                (wasmdown.wasi:fs-stat-at ctx d nil "out-abs"))))
             ;; following an absolute target is refused
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d follow "out-abs")))
             ;; following a climbing target is refused
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d follow "out-rel")))
             ;; an INTERMEDIATE escaping link is refused too
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil "esc-dir/passwd")))
             ;; opening through an escaping link fails closed
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-open-at ctx d follow "out-abs" nil
                                            (cons :flags '("read")))))
             ;; readlink-at refuses an absolute target (the WIT rule)
             (=> "not-permitted"
                 (fs-error-code-of (wasmdown.wasi:fs-readlink-at ctx d "out-abs")))
             (=> "not-permitted"
                 (fs-error-code-of (wasmdown.wasi:fs-readlink-at ctx d "out-rel")))
             ;; symlink-at refuses an absolute old-path, and never
             ;; creates the link
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-symlink-at ctx d "/etc/passwd" "new-link")))
             (is (null (probe-file (fs-p root "new-link"))))
             ;; a legitimate relative link does get created
             (=> '(:ok)
                 (wasmdown.wasi:fs-symlink-at ctx d "inside.txt" "good-link"))
             (=> "inside.txt"
                 (fs-ok-value (wasmdown.wasi:fs-readlink-at ctx d "good-link"))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; The directory/file mutations, and the errno -> error-code map.

(test wasi/fs-directory-operations
  (with-fs-root (root "fs-ops" '("d" "d/e" "f.txt" "g.txt" "plain.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             (=> '(:ok) (wasmdown.wasi:fs-create-directory-at ctx d "d"))
             (is (sb-unix:unix-stat (fs-p root "d")))
             (=> "exist"
                 (fs-error-code-of (wasmdown.wasi:fs-create-directory-at ctx d "d")))
             (=> '(:ok) (wasmdown.wasi:fs-create-directory-at ctx d "d/e"))
             ;; a non-empty directory cannot be removed
             (=> "not-empty"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-remove-directory-at ctx d "d")))
             (=> '(:ok) (wasmdown.wasi:fs-remove-directory-at ctx d "d/e"))
             ;; rename within one descriptor (and across descriptors)
             (fs-write! (fs-p root "f.txt") "abc")
             (=> '(:ok) (wasmdown.wasi:fs-rename-at ctx d "f.txt" d "g.txt"))
             (is (null (probe-file (fs-p root "f.txt"))))
             (is (probe-file (fs-p root "g.txt")))
             (=> 3 (cdr (assoc "size" (fs-ok-value
                                       (wasmdown.wasi:fs-stat-at ctx d nil "g.txt"))
                               :test #'string=)))
             ;; unlink a file; the missing one maps to no-entry
             (=> '(:ok) (wasmdown.wasi:fs-unlink-file-at ctx d "g.txt"))
             (=> "no-entry"
                 (fs-error-code-of (wasmdown.wasi:fs-unlink-file-at ctx d "g.txt")))
             ;; a directory is not a file (EISDIR)
             (let ((code (fs-error-code-of
                          (wasmdown.wasi:fs-unlink-file-at ctx d "d"))))
               (is (member code '("is-directory" "access") :test #'string=)))
             ;; a path THROUGH a regular file is not-directory
             (fs-write! (fs-p root "plain.txt") "x")
             (=> "not-directory"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil "plain.txt/x")))
             ;; remove the (now empty) directory
             (=> '(:ok) (wasmdown.wasi:fs-remove-directory-at ctx d "d")))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-error-code-mapping
  ;; the errno -> WIT error-code table (the cases the API documents)
  (=> "no-entry" (wasmdown.wasi:errno->error-code 2))
  (=> "exist" (wasmdown.wasi:errno->error-code 17))
  (=> "not-directory" (wasmdown.wasi:errno->error-code 20))
  (=> "is-directory" (wasmdown.wasi:errno->error-code 21))
  (=> "access" (wasmdown.wasi:errno->error-code 13))
  (=> "not-permitted" (wasmdown.wasi:errno->error-code 1))
  (=> "loop" (wasmdown.wasi:errno->error-code 40))
  (=> "not-empty" (wasmdown.wasi:errno->error-code 39))
  (=> "bad-descriptor" (wasmdown.wasi:errno->error-code 9))
  (=> "read-only" (wasmdown.wasi:errno->error-code 30))
  (=> "insufficient-space" (wasmdown.wasi:errno->error-code 28))
  ;; an unmapped errno is the variant's catch-all
  (=> "other" (wasmdown.wasi:errno->error-code 9999)))

(test wasi/fs-open-at-flags
  (with-fs-root (root "fs-open" '("made.txt" "made2.txt" "dir" "six.txt" "sixlink" "child.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre))
           (read (cons :flags '("read")))
           (write (cons :flags '("read" "write")))
           (create (cons :flags '("create")))
           (excl (cons :flags '("create" "exclusive")))
           (trunc (cons :flags '("create" "truncate")))
           (direc (cons :flags '("directory"))))
      (unwind-protect
           (progn
             ;; create + write: the descriptor exists as a host file
             (let* ((ok (fs-ok-value (wasmdown.wasi:fs-open-at ctx d nil "made.txt"
                                                               create write)))
                    (desc (third ok)))
               (is (wasmdown.wasi:wasi-descriptor-p desc))
               (is (probe-file (fs-p root "made.txt")))
               (is (member "write" (wasmdown.wasi:wasi-descriptor-flags desc)
                           :test #'string=))
               ;; set-size extends the file (zeros)
               (=> '(:ok) (wasmdown.wasi:fs-set-size ctx desc 6))
               (=> 6 (cdr (assoc "size" (fs-ok-value (wasmdown.wasi:fs-stat ctx desc))
                                 :test #'string=)))
               ;; sync / sync-data / advise / set-times are honest no-ops
               ;; over the OS calls
               (=> '(:ok) (wasmdown.wasi:fs-sync ctx desc))
               (=> '(:ok) (wasmdown.wasi:fs-sync-data ctx desc))
               (=> '(:ok) (wasmdown.wasi:fs-advise ctx desc 0 0
                                                   (cons :enum "sequential")))
               (=> '(:ok) (wasmdown.wasi:fs-set-times ctx desc
                                                     (cons "now" nil)
                                                     (cons "now" nil)))
               ;; set-times-at through the directory descriptor
               (=> '(:ok) (wasmdown.wasi:fs-set-times-at
                           ctx d nil "made.txt" (cons "no-change" nil)
                           (cons "now" nil)))
               ;; the descriptor is closed ONCE the host drops it
               (wasmdown.wasi:close-wasi-descriptor desc)
               (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc))))
             ;; exclusive creation of an existing file -> exist
             (=> "exist"
                 (fs-error-code-of (wasmdown.wasi:fs-open-at ctx d nil "made.txt"
                                                             excl write)))
             ;; opening with the directory flag on a file -> not-directory
             (let ((code (fs-error-code-of
                          (wasmdown.wasi:fs-open-at ctx d nil "made.txt"
                                                    direc read))))
               (is (member code '("not-directory") :test #'string=)))
             ;; a directory opens with the directory flag
             (fs-mkdir! (fs-p root "dir"))
             (let ((ok (fs-ok-value (wasmdown.wasi:fs-open-at ctx d nil "dir"
                                                              direc read))))
               (is (wasmdown.wasi:wasi-descriptor-dir-p (third ok)))
               (wasmdown.wasi:close-wasi-descriptor (third ok)))
             ;; truncate empties the file
             (fs-write! (fs-p root "six.txt") "abcdef")
             (let* ((ok (fs-ok-value (wasmdown.wasi:fs-open-at ctx d nil "six.txt"
                                                               trunc write)))
                    (desc (third ok)))
               (=> 0 (cdr (assoc "size" (fs-ok-value
                                         (wasmdown.wasi:fs-stat-at ctx d nil "six.txt"))
                                 :test #'string=)))
               (wasmdown.wasi:close-wasi-descriptor desc))
             ;; open-at without the follow flag on a symlink fails closed
             (fs-symlink! "six.txt" (fs-p root "sixlink"))
             (=> "loop"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-open-at ctx d nil "sixlink" nil read)))
             ;; a non-directory base refuses creation (read-only)
             (let ((file-desc (third (fs-ok-value
                                      (wasmdown.wasi:fs-open-at ctx d nil "made.txt" nil read)))))
               (=> "read-only"
                   (fs-error-code-of
                    (wasmdown.wasi:fs-open-at ctx file-desc nil "child.txt"
                                              create write)))
               (wasmdown.wasi:close-wasi-descriptor file-desc)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-metadata
  (with-fs-root (root "fs-meta" '("a.txt" "b.txt" "hard.txt"))
    (fs-write! (fs-p root "a.txt") "hello")
    (fs-write! (fs-p root "b.txt") "hello")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; is-same-object: the same descriptor yes, a different
             ;; inode no
             (is (wasmdown.wasi:fs-is-same-object ctx d d))
             (let ((a (third (fs-ok-value (wasmdown.wasi:fs-open-at
                                           ctx d nil "a.txt" nil
                                           (cons :flags '("read"))))))
                   (b (third (fs-ok-value (wasmdown.wasi:fs-open-at
                                           ctx d nil "b.txt" nil
                                           (cons :flags '("read")))))))
               (unwind-protect
                    (progn
                      (is (not (wasmdown.wasi:fs-is-same-object ctx a b)))
                      ;; metadata-hash is stable for an unmodified object
                      ;; and differs across different objects
                      (let ((ha (fs-ok-value (wasmdown.wasi:fs-metadata-hash ctx a))))
                        (is (equal ha (fs-ok-value (wasmdown.wasi:fs-metadata-hash ctx a))))
                        (is (not (equal ha (fs-ok-value
                                            (wasmdown.wasi:fs-metadata-hash ctx b))))))
                      ;; metadata-hash-at agrees with metadata-hash
                      (is (equal (fs-ok-value (wasmdown.wasi:fs-metadata-hash ctx a))
                                 (fs-ok-value
                                  (wasmdown.wasi:fs-metadata-hash-at ctx d nil "a.txt")))))
                 (wasmdown.wasi:close-wasi-descriptor a)
                 (wasmdown.wasi:close-wasi-descriptor b)))
             ;; link-at: a hard link shares the inode
             (let* ((a (third (fs-ok-value (wasmdown.wasi:fs-open-at
                                           ctx d nil "a.txt" nil
                                           (cons :flags '("read")))))))
               (unwind-protect
                    (progn
                      (=> '(:ok) (wasmdown.wasi:fs-link-at ctx d nil "a.txt" d "hard.txt"))
                      (=> 2 (cdr (assoc "link-count"
                                       (fs-ok-value
                                        (wasmdown.wasi:fs-stat-at ctx d nil "hard.txt"))
                                       :test #'string=)))
                      (=> "exist"
                          (fs-error-code-of
                           (wasmdown.wasi:fs-link-at ctx d nil "a.txt" d "hard.txt"))))
                 (wasmdown.wasi:close-wasi-descriptor a))))
        (wasmdown.wasi:close-wasi-preopen pre)))))


;;; ---------------------------------------------------------------
;;; The descriptor RESOURCE through the runtime's handle table:
;;; own/borrow, the dtor, and the provider export surface.

(defun provider-exports (instance)
  (wasmdown.component.runtime:comp-instance-exports instance))

(defun has-method-p (types name)
  (let ((hit (gethash (format nil "[method]descriptor.~a" name)
                      (provider-exports types))))
    (and hit (wasmdown.component.runtime:lifted-func-p (cdr hit)))))

(test wasi/fs-provider-surface
  (with-fs-root (root "fs-prov" '("a.txt"))
    (fs-write! (fs-p root "a.txt") "hello")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (types (wasmdown.wasi:filesystem-types-instance ctx))
           (preopens (wasmdown.wasi:filesystem-preopens-instance ctx))
           (dtor-ct (wasmdown.wasi:descriptor-resource-ct))
           (type-export (gethash "descriptor" (provider-exports types)))
           (pre-desc (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; the interface instance exports the resource TYPE and
             ;; every method under its component core name
             (is (eq (car type-export) :type))
             (is (eq (wasmdown.component:ct-kind (cdr type-export)) :resource))
             ;; the type ct is THE handle identity: the exported type,
             ;; the method self borrows and descriptor-resource-ct agree
             (is (eq dtor-ct (cdr type-export)))
             (dolist (m '("stat" "stat-at" "get-type" "get-flags" "set-size"
                          "is-same-object" "metadata-hash" "metadata-hash-at"
                          "create-directory-at" "remove-directory-at"
                          "unlink-file-at" "rename-at" "link-at"
                          "readlink-at" "symlink-at" "open-at" "advise"
                          "sync" "sync-data" "set-times" "set-times-at"
                          "read-via-stream" "write-via-stream"
                          "append-via-stream" "read-directory"))
               (unless (has-method-p types m)
                 (error "missing exported method ~a" m)))
             ;; the method functype: the WIT signature + the implicit
             ;; self borrow, marked async
             (let* ((fn (provider-fn types "[method]descriptor.stat"))
                    (ft (wasmdown.component.runtime:lifted-func-type fn))
                    (params (coerce (first (wasmdown.component:ct-payload ft))
                                    'list)))
               (is (= 1 (length params)))
               (is (string= "self" (caar params)))
               (is (eq :borrow (wasmdown.component:ct-kind (cdar params))))
               (is (eq dtor-ct (wasmdown.component:ct-payload (cdar params))))
               (is (third (wasmdown.component:ct-payload ft))))
             ;; get-directories: own-descriptor handles + the guest name
             (let ((dirs (call-provider preopens "get-directories")))
               (is (= 1 (length dirs)))
               (let* ((entry (first dirs))
                      (own (first entry))
                      (name (second entry)))
                 (=> "/sandbox" name)
                 (=> :own-handle (car own))
                 (is (eq dtor-ct (second own)))
                 (is (eq pre-desc (third own)))
                 ;; a direct call with the REP works (no handle table)
                 (=> "directory"
                     (car (fs-ok-value
                           (call-provider types "[method]descriptor.get-type"
                                          pre-desc))))))
             ;; BORROW through the handle table: a raw handle index is
             ;; resolved on the DYNAMIC CALLER's table through the
             ;; shared resource ct
             (let* ((guest (wasmdown.component.runtime:make-comp-instance))
                    (idx (wasmdown.component.runtime::%handle-add
                          guest dtor-ct pre-desc nil)))
               (is (eql 1 idx))
               (is (eq pre-desc
                       (let ((wasmdown.component.runtime::*handle-caller-inst*
                               guest))
                         (wasmdown.component.runtime:host-resource-rep
                          dtor-ct idx))))
               (is (equal "directory"
                          (car (fs-ok-value
                                (let ((wasmdown.component.runtime::*handle-caller-inst*
                                        guest))
                                  (call-provider types
                                                 "[method]descriptor.get-type"
                                                 idx))))))
               ;; an index that names no descriptor is bad-descriptor
               (is (equal "bad-descriptor"
                          (fs-error-code-of
                           (let ((wasmdown.component.runtime::*handle-caller-inst*
                                   guest))
                             (call-provider types
                                            "[method]descriptor.get-type"
                                            99))))))
             ;; an OPEN descriptor is owned: the host finalizer closes
             ;; its fd; a preopen descriptor's fd is NOT closed
             (let* ((opened (third (fs-ok-value
                                    (wasmdown.wasi:fs-open-at
                                     ctx pre-desc nil "a.txt" nil
                                     (cons :flags '("read"))))))
                    ;; M17: the finalizer is registered on THIS
                    ;; provider instance (the per-instance registry),
                    ;; not in the retired ct-keyed global
                    (hdtor (wasmdown.component.runtime:host-resource-finalizer
                            types dtor-ct)))
               (is (eq (symbol-function 'wasmdown.wasi:close-wasi-descriptor)
                       hdtor))
               (is (plusp (wasmdown.wasi:wasi-descriptor-fd opened)))
               (funcall hdtor opened)
               (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd opened)))
               (funcall hdtor pre-desc)
               (is (plusp (wasmdown.wasi:wasi-preopen-fd pre)))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END FIXTURE: a component imports
;;; wasi:filesystem/types@0.3.2 + preopens@0.3.2, calls
;;; get-directories, opens a file with open-at(create) through the
;;; preopen descriptor, stats it (the new file is size 0) and stats
;;; the preopen directory itself (the guest's descriptor-stat must
;;; equal the host's own fstat of that directory). The host sees the
;;; file the guest created.

(defparameter *fs-fixture*
  (concatenate
   'string
   "(component"
   " (import \"wasi:filesystem/types@0.3.2\""
   "   (instance $fs"
   "     (export \"descriptor\" (type $D (sub resource)))"
   "     (export \"[method]descriptor.open-at\""
   "       (func (param \"self\" (borrow $D))"
   "             (param \"path-flags\" u32) (param \"path\" string)"
   "             (param \"open-flags\" u32) (param \"flags\" u32)"
   "             (result (result (own $D) u32))))"
   "     (export \"[method]descriptor.stat\""
   "       (func (param \"self\" (borrow $D)) (result (tuple u64 u64))))))"
   " (import \"wasi:filesystem/preopens@0.3.2\""
   "   (instance $pre"
   "     (export \"descriptor\" (type $D2 (sub resource)))"
   "     (export \"get-directories\""
   "       (func (result (list (tuple (own $D2) string)))))))"
   " (alias export $pre \"get-directories\" (func $gd))"
   " (alias export $fs \"[method]descriptor.open-at\" (func $open-at))"
   " (alias export $fs \"[method]descriptor.stat\" (func $stat))"
   " (core module $Mem"
   "   (memory (export \"mem\") 1)"
   "   (global $bump (mut i32) (i32.const 1024))"
   "   (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "     (local $p i32)"
   "     (local.set $p"
   "       (i32.and (i32.add (global.get $bump)"
   "                         (i32.sub (local.get 2) (i32.const 1)))"
   "                (i32.sub (i32.const 0) (local.get 2))))"
   "     (global.set $bump (i32.add (local.get $p) (local.get 3)))"
   "     (local.get $p)))"
   " (core instance $mem (instantiate $Mem))"
   " (core func $gd-lowered"
   "   (canon lower (func $gd) (memory (core memory $mem \"mem\"))"
   "                (realloc (core func $mem \"realloc\"))))"
   " (core func $open-lowered"
   "   (canon lower (func $open-at) (memory (core memory $mem \"mem\"))"
   "                (realloc (core func $mem \"realloc\"))))"
   " (core func $stat-lowered"
   "   (canon lower (func $stat) (memory (core memory $mem \"mem\"))"
   "                (realloc (core func $mem \"realloc\"))))"
   " (core module $M"
   "   (import \"env\" \"mem\" (memory 1))"
   "   (import \"env\" \"gd\" (func $gd (param i32)))"
   "   (import \"env\" \"open\" (func $open (param i32 i32 i32 i32 i32 i32 i32)))"
   "   (import \"env\" \"stat\" (func $stat (param i32 i32)))"
   "   (func (export \"f\") (result i64)"
   "     (local $d i32)"
   "     (call $gd (i32.const 0))"
   "     (local.set $d (i32.load (i32.load (i32.const 0))))"
   "     (call $open (local.get $d) (i32.const 0) (i32.const 256)"
   "                 (i32.const 8) (i32.const 1) (i32.const 3)"
   "                 (i32.const 128))"
   "     (local.set $d (i32.load (i32.const 132)))"
   "     (call $stat (local.get $d) (i32.const 512))"
   "     (i64.load (i32.const 544)))"
   "   (func (export \"g\") (result i64)"
   "     (call $gd (i32.const 0))"
   "     (call $stat (i32.load (i32.load (i32.const 0))) (i32.const 512))"
   "     (i64.load (i32.const 544)))"
   "   (data (i32.const 256) \"made.txt\"))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $mem \"mem\"))"
   "   (export \"gd\" (func $gd-lowered))"
   "   (export \"open\" (func $open-lowered))"
   "   (export \"stat\" (func $stat-lowered))))))"
   " (func (export \"f\") (result u64) (canon lift (core func $m \"f\")))"
   " (func (export \"g\") (result u64) (canon lift (core func $m \"g\"))))"))

(test wasi/e2e-filesystem-component
  (with-fs-root (root "fs-e2e" '("made.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (comp (wasmdown.component.text::parse-component-text *fs-fixture*)))
      (unwind-protect
           (progn
             (wasmdown.component:validate-component comp)
             (let* ((inst (wasmdown.component.runtime:instantiate-component
                           comp
                           :with-args (wasmdown.wasi:filesystem-provider ctx)))
                    (f (wasmdown.component.runtime:runtime-export-value inst "f"))
                    (g (wasmdown.component.runtime:runtime-export-value inst "g")))
               ;; open-at(create) made the file through the preopen
               ;; handle; the guest's stat of the new descriptor says 0
               (is (eql 0 (wasmdown.component.runtime:lifted-func-call f)))
               (is (probe-file (fs-p root "made.txt")))
               ;; the host sees the very same file after the call
               (is (sb-unix:unix-stat (fs-p root "made.txt")))
               ;; the guest's stat of the PREOPEN descriptor equals
               ;; the host's fstat of that same directory handle (the
               ;; st_size field, read out of the canonical layout)
               (is (= (nth-value 8 (sb-unix:unix-fstat
                                    (wasmdown.wasi:wasi-preopen-fd pre)))
                      (wasmdown.component.runtime:lifted-func-call g)))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ===============================================================
;;; M16 SLICE 3b — THE FILESYSTEM STREAM SURFACE: read/write/
;;; append-via-stream + read-directory over the host stream/future
;;; ABI, and the async canon path.
;;; ===============================================================

;;; ---------------------------------------------------------------
;;; CL-side drivers for the host ends (the provider's peer).

(defun fs-bytes (path)
  "PATH's bytes as a u8 list."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s)
                         :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      (coerce v 'list))))

(defun fs-drain-source (reader &optional (limit 64))
  "Read the guest end READER into CL buffers until DROPPED; the
accumulated u8 values."
  (let ((out '()))
    (loop
      (let ((buf (wasmdown.component.runtime:make-host-buffer limit)))
        (rtcall "%END-COPY" reader buf t)
        (setf out (append out (wasmdown.component.runtime:host-buffer-values
                               buf)))
        (let ((ev (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader))))
          (when (< (logand ev #xf) 2)   ; DROPPED(1)/CANCELLED(2)
            (return out)))))))

(defun fs-stage-write (writer bytes)
  "The guest's write through WRITER; returns the packed event code."
  (rtcall "%END-COPY" writer
          (wasmdown.component.runtime:make-host-buffer
           (length bytes) :values bytes)
          nil)
  (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer)))

(defun fs-drop-end (end)
  (rtcall "%END-DROP" end))

(defun fs-future-value (future)
  "The (single) value a host future delivers."
  (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
    (rtcall "%END-COPY" future buf t)
    (first (wasmdown.component.runtime:host-buffer-values buf))))

(defun fs-fresh-stream ()
  (wasmdown.component.runtime:make-host-stream-pair *u8-ct*))

(defun fs-open-file (ctx base path open-flags flags)
  "open-at through the preopen descriptor BASE."
  (third (fs-ok-value (wasmdown.wasi:fs-open-at ctx base nil path
                                               open-flags flags))))

(test wasi/fs-write-via-stream
  (with-fs-root (root "fs-wvs" '("out.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let ((desc (fs-open-file ctx d "out.txt"
                                     (cons :flags '("create"))
                                     (cons :flags '("read" "write")))))
             (unwind-protect
                  (progn
                    ;; write "hello" at offset 0
                    (multiple-value-bind (reader writer) (fs-fresh-stream)
                      (let ((future (wasmdown.wasi:fs-write-via-stream
                                     ctx desc reader 0)))
                        (is (wasmdown.component.runtime:async-end-p future))
                        (is (eql (%completed 5)
                                 (fs-stage-write writer '(104 101 108 108 111))))
                        ;; the bytes are on disk once drained (synchronously
                        ;; at the guest's write), not only at the drop
                        (is (equal '(104 101 108 108 111)
                                   (fs-bytes (fs-p root "out.txt"))))
                        ;; dropping the writable end resolves the future
                        (fs-drop-end writer)
                        (is (equal '(:ok) (fs-future-value future)))))
                    ;; a second stream writes PAST the end (a zero-filled
                    ;; gap — the WIT: "the file is extended to the extent
                    ;; of the write")
                    (multiple-value-bind (reader writer) (fs-fresh-stream)
                      (let ((future (wasmdown.wasi:fs-write-via-stream
                                     ctx desc reader 8)))
                        (fs-stage-write writer '(88))
                        (fs-drop-end writer)
                        (is (equal '(:ok) (fs-future-value future))))
                      ;; the kernel file offset never moved: the writes
                      ;; were pwrite, so the descriptor is still at 0
                      (is (= 0 (sb-unix:unix-lseek
                                (wasmdown.wasi:wasi-descriptor-fd desc)
                                0 sb-unix:l_incr))))
                    (is (equal '(104 101 108 108 111 0 0 0 88)
                               (fs-bytes (fs-p root "out.txt")))))
               (wasmdown.wasi:close-wasi-descriptor desc)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-read-via-stream-offset-independence
  (with-fs-root (root "fs-rvs" '("data.txt"))
    (fs-write! (fs-p root "data.txt") "abcdefghij")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((desc (fs-open-file ctx d "data.txt" nil
                                      (cons :flags '("read"))))
                  (a (wasmdown.wasi:fs-read-via-stream ctx desc 2))
                  (b (wasmdown.wasi:fs-read-via-stream ctx desc 5))
                  (sa (first a)) (fa (second a))
                  (sb (first b)) (fb (second b)))
             ;; two streams, different offsets: interleaved reads pull
             ;; THEIR OWN windows (no shared offset drift)
             (is (equal '(99 100 101) (fs-drain-3 sa)))   ; offset 2
             (is (equal '(102 103 104) (fs-drain-3 sb)))   ; offset 5
             (is (equal '(102 103 104) (fs-drain-3 sa)))   ; offset 5
             (is (equal '(105 106) (fs-drain-3 sb)))       ; offset 8
             (is (equal '(105 106) (fs-drain-3 sa)))
             (is (equal nil (fs-drain-3 sa)))
             (is (equal nil (fs-drain-3 sb)))
             ;; the fd's own kernel offset was never touched
             (is (= 0 (sb-unix:unix-lseek
                       (wasmdown.wasi:wasi-descriptor-fd desc)
                       0 sb-unix:l_incr)))
             ;; both futures ended (:ok) at EOF
             (is (equal '(:ok) (fs-future-value fa)))
             (is (equal '(:ok) (fs-future-value fb)))
             (wasmdown.wasi:close-wasi-descriptor desc))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(defun fs-drain-3 (reader)
  "One 3-element CL read from READER (NIL at DROPPED)."
  (let ((buf (wasmdown.component.runtime:make-host-buffer 3)))
    (rtcall "%END-COPY" reader buf t)
    (let ((vals (wasmdown.component.runtime:host-buffer-values buf)))
      (and vals (not (eql 1 (logand (third (rtcall "%WAITABLE-GET-PENDING-EVENT"
                                                   reader))
                                    #xf)))
           vals))))

(test wasi/fs-append-via-stream
  (with-fs-root (root "fs-avs" '("log.txt"))
    (fs-write! (fs-p root "log.txt") "ab")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let ((desc (fs-open-file ctx d "log.txt" nil
                                     (cons :flags '("read" "write")))))
             (unwind-protect
                  (multiple-value-bind (reader writer) (fs-fresh-stream)
                    (let ((future (wasmdown.wasi:fs-append-via-stream
                                   ctx desc reader)))
                      ;; two drains of the SAME append stream: each goes to
                      ;; the file's CURRENT end (O_APPEND semantics), so the
                      ;; second chunk lands after the first
                      (is (eql (%completed 2) (fs-stage-write writer '(99 100))))
                      (is (eql (%completed 2) (fs-stage-write writer '(101 102))))
                      (fs-drop-end writer)
                      (is (equal '(:ok) (fs-future-value future)))
                      (is (equal '(97 98 99 100 101 102)
                                 (fs-bytes (fs-p root "log.txt"))))))
               (wasmdown.wasi:close-wasi-descriptor desc)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-stream-flag-gating
  (with-fs-root (root "fs-gate" '("w.txt" "r.txt" "dir"))
    (fs-write! (fs-p root "w.txt") "x")
    (fs-write! (fs-p root "r.txt") "y")
    (fs-mkdir! (fs-p root "dir"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let ((w (fs-open-file ctx d "w.txt" nil (cons :flags '("write"))))
                 (rd (fs-open-file ctx d "r.txt" nil (cons :flags '("read")))))
             (unwind-protect
                  (progn
                    ;; read needs the read flag: a DROPPED stream and a
                    ;; future carrying the WIT error-code
                    (let ((pair (wasmdown.wasi:fs-read-via-stream ctx w 0)))
                      (is (equal nil (fs-drain-3 (first pair))))
                      (is (equal '(:err . "not-permitted")
                                 (fs-future-value (second pair)))))
                    (let ((pair (wasmdown.wasi:fs-read-directory ctx w)))
                      (is (equal '(:err . "not-permitted")
                                 (fs-future-value (second pair)))))
                    ;; the write forms need the write flag
                    (multiple-value-bind (reader writer) (fs-fresh-stream)
                      (is (equal '(:err . "read-only")
                                 (fs-future-value
                                  (wasmdown.wasi:fs-write-via-stream
                                   ctx rd reader 0))))
                      ;; the guest's staged write now meets DROPPED
                      (is (eql 1 (logand (fs-stage-write writer '(1)) #xf))))
                    (multiple-value-bind (reader writer) (fs-fresh-stream)
                      (declare (ignore writer))
                      (is (equal '(:err . "read-only")
                                 (fs-future-value
                                  (wasmdown.wasi:fs-append-via-stream
                                   ctx rd reader)))))
                    ;; a directory opened read-only IS readable
                    (let ((dir (fs-open-file ctx d "dir"
                                             (cons :flags '("directory"))
                                             (cons :flags '("read")))))
                      (unwind-protect
                           (let ((pair (wasmdown.wasi:fs-read-directory ctx dir)))
                             (is (wasmdown.component.runtime:async-end-p
                                  (first pair)))
                             (is (wasmdown.component.runtime:async-end-p
                                  (second pair))))
                        (wasmdown.wasi:close-wasi-descriptor dir))))
               (wasmdown.wasi:close-wasi-descriptor w)
               (wasmdown.wasi:close-wasi-descriptor rd)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-stream-read-error-future
  ;; A read on a DIRECTORY descriptor: pread fails with EISDIR — the
  ;; future carries the WIT error case, the stream ends DROPPED.
  (with-fs-root (root "fs-rerr" '("dir"))
    (fs-mkdir! (fs-p root "dir"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((sub (fs-open-file ctx d "dir"
                                     (cons :flags '("directory"))
                                     (cons :flags '("read"))))
                  (entry (wasmdown.wasi:fs-read-via-stream ctx sub 0))
                  (stream (first entry))
                  (future (second entry)))
             (unwind-protect
                  (progn
                    (is (equal nil (fs-drain-3 stream)))
                    (is (eql 1 (logand (third (rtcall "%WAITABLE-GET-PENDING-EVENT"
                                                      stream))
                                       #xf)))
                    (is (equal '(:err . "is-directory")
                               (fs-future-value future))))
               (wasmdown.wasi:close-wasi-descriptor sub)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-read-directory
  (with-fs-root (root "fs-rd" '("a.txt" "sub" "lnk"))
    (fs-write! (fs-p root "a.txt") "hi")
    (fs-mkdir! (fs-p root "sub"))
    (fs-symlink! "a.txt" (fs-p root "lnk"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((pair (wasmdown.wasi:fs-read-directory ctx d))
                  (stream (first pair))
                  (future (second pair))
                  (buf (wasmdown.component.runtime:make-host-buffer 16)))
             (rtcall "%END-COPY" stream buf t)
             (let* ((entries (wasmdown.component.runtime:host-buffer-values buf))
                    (by-name (mapcar (lambda (e)
                                       (cons (cdr (assoc "name" e :test #'string=))
                                             (second (assoc "%type" e
                                                            :test #'string=))))
                                     entries)))
               (is (eql (%completed 3)
                        (third (rtcall "%WAITABLE-GET-PENDING-EVENT" stream))))
               ;; `.` and `..` are omitted
               (is (null (assoc "." by-name :test #'string=)))
               (is (null (assoc ".." by-name :test #'string=)))
               ;; every entry with its type (d_type on this filesystem)
               (is (equal (sort (copy-list by-name) #'string< :key #'car)
                          '(("a.txt" . "regular-file")
                            ("lnk" . "symbolic-link")
                            ("sub" . "directory")))))
             ;; a second read is DROPPED (the directory is exhausted); the
             ;; future resolved (:ok) at the end of the iteration
             (is (equal nil (fs-drain-3 stream)))
             (is (equal '(:ok) (fs-future-value future)))
             ;; a SECOND stream starts at the beginning again (the WIT:
             ;; "always returns a new stream which starts at the
             ;; beginning"; concurrent streams do not interfere)
             (let* ((pair2 (wasmdown.wasi:fs-read-directory ctx d))
                    (buf2 (wasmdown.component.runtime:make-host-buffer 1)))
               (rtcall "%END-COPY" (first pair2) buf2 t)
               (is (eql 1 (length (wasmdown.component.runtime:host-buffer-values
                                   buf2)))))
             ;; the DT_UNKNOWN fallback: the entry's type from
             ;; O_PATH|O_NOFOLLOW + fstat (a link reports ITSELF)
             (let ((dirfd (wasmdown.wasi::%libc-openat
                           (wasmdown.wasi:wasi-preopen-fd pre) "."
                           (logior wasmdown.wasi::+o-rdonly+
                                   wasmdown.wasi::+o-directory+
                                   wasmdown.wasi::+o-cloexec+)
                           0)))
               (unwind-protect
                    (progn
                      (is (equal "regular-file"
                                 (car (wasmdown.wasi::%dirent-stat-type
                                       dirfd "a.txt"))))
                      (is (equal "symbolic-link"
                                 (car (wasmdown.wasi::%dirent-stat-type
                                       dirfd "lnk")))))
                 (wasmdown.wasi::%close-fd dirfd))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE BYTE SOURCE/SINK CONTRACT at the provider boundary: a source
;;; hands back a REUSABLE u8 VECTOR (not a byte list) and reports EOF
;;; separately from would-block; a sink consumes a SPAN in ONE syscall.

(test wasi/fd-pread-source-vector-and-eof
  (with-fs-root (root "fs-pread-src" '("data.txt"))
    (fs-write! (fs-p root "data.txt") "abcdef")
    (let* ((fd (wasmdown.wasi::%libc-open
                (fs-p root "data.txt") wasmdown.wasi::+o-rdonly+ 0))
           (settled '())
           (source (wasmdown.wasi::%fd-pread-source
                    fd 0 (lambda (v) (push v settled)))))
      (unwind-protect
           (progn
             (multiple-value-bind (octets eof) (funcall source 4)
               (is (vectorp octets))
               (is (not (listp octets)))
               (is (equal "abcd" (map 'string #'code-char octets)))
               (is (null eof)))
             ;; the same vector is reused: the length is the new count
             (multiple-value-bind (octets eof) (funcall source 4)
               (is (equal "ef" (map 'string #'code-char octets)))
               (is eof))
             ;; a short read settles the stream's future (:ok)
             (is (equal '((:ok)) settled))
             (multiple-value-bind (octets eof) (funcall source 4)
               (is (null octets))
               (is eof)))
        (wasmdown.wasi::%close-fd fd)))))

(test wasi/fs-descriptor-refcount-stream-outlives-handle
  ;; dropping the descriptor handle while a stream is live must not
  ;; close the fd under the stream; the fd closes once, at the
  ;; stream's EOF (the last holder's release)
  (with-fs-root (root "fs-ref1" '("data.txt"))
    (fs-write! (fs-p root "data.txt") "abcdef")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((desc (fs-open-file ctx d "data.txt" nil
                                      (cons :flags '("read"))))
                  (fd (wasmdown.wasi:wasi-descriptor-fd desc))
                  (pair (wasmdown.wasi:fs-read-via-stream ctx desc 0))
                  (stream (first pair))
                  (future (second pair)))
             ;; the handle's own reference + the stream's
             (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
             (wasmdown.wasi:close-wasi-descriptor desc)
             (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (wasmdown.wasi:wasi-descriptor-closed-p desc))
             (is (plusp fd))
             ;; the fd is STILL open: the stream keeps it alive
             (is (not (null (sb-unix:unix-fstat fd))))
             (is (equal '(97 98 99) (fs-drain-3 stream)))
             (is (equal '(100 101 102) (fs-drain-3 stream)))
             (is (null (fs-drain-3 stream)))
             (is (equal '(:ok) (fs-future-value future)))
             ;; the last holder released: closed exactly once
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc)))
             (is (wasmdown.wasi:wasi-descriptor-closed-p desc))
             (is (null (sb-unix:unix-fstat fd)))
             ;; a redundant handle release cannot decrement again
             (wasmdown.wasi:close-wasi-descriptor desc)
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-descriptor-drop-fd-plateau-pin
  ;; §9.3's capability check, counted on NATIVE FDs: a warm-up then
  ;; repeated open->stream->drop->stream-end cycles return the open-fd
  ;; count to baseline every time, with the descriptor's refs passing
  ;; through 2 -> 1 -> 0 and closing the fd exactly once (a stream
  ;; outliving its handle keeps the fd, and the last release closes
  ;; it). The OWNED handle's drop runs the provider finalizer once:
  ;; a second drop of the same index traps (the slot is gone) and
  ;; cannot decrement/close a second time.
  (flet ((open-fd-count ()
           (loop for fd from 0 below 256
                 count (sb-unix:unix-fstat fd))))
    (with-fs-root (root "fs-plateau" '("data.txt"))
      (fs-write! (fs-p root "data.txt") "abcdef")
      (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
             (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
             (d (wasmdown.wasi:wasi-preopen-descriptor pre))
             (baseline (open-fd-count)))
        (unwind-protect
             (progn
               ;; WARM-UP + repeated cycles: every count returns to
               ;; baseline and every capacity (refs/fd slot) is the
               ;; same as the warm-up's
               (dotimes (i 6)
                 (let* ((desc (fs-open-file ctx d "data.txt" nil
                                            (cons :flags '("read"))))
                        (fd (wasmdown.wasi:wasi-descriptor-fd desc))
                        (pair (wasmdown.wasi:fs-read-via-stream ctx desc 0))
                        (stream (first pair))
                        (future (second pair)))
                   (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (is (= (1+ baseline) (open-fd-count)))
                   ;; the guest handle goes first: the stream holds the
                   ;; fd open
                   (wasmdown.wasi:close-wasi-descriptor desc)
                   (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (is (wasmdown.wasi:wasi-descriptor-closed-p desc))
                   (is (plusp fd))
                   (is (not (null (sb-unix:unix-fstat fd))))
                   ;; drain the stream: the last holder releases once
                   (is (equal '(97 98 99) (fs-drain-3 stream)))
                   (is (equal '(100 101 102) (fs-drain-3 stream)))
                   (is (null (fs-drain-3 stream)))
                   (is (equal '(:ok) (fs-future-value future)))
                   (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc)))
                   (is (null (sb-unix:unix-fstat fd)))
                   (is (= baseline (open-fd-count)))))
               ;; OWNED DESCRIPTOR via the ABI's handle: the drop
               ;; closes once, and the second drop of the same index
               ;; traps without a second release
               (let* ((ct (wasmdown.wasi:descriptor-resource-ct))
                      (provider (wasmdown.wasi:filesystem-types-instance ctx))
                      (guest (wasmdown.component.runtime:make-comp-instance))
                      (desc (wasmdown.wasi::make-wasi-descriptor
                             :fd (sb-unix:unix-open "/dev/null"
                                                    sb-unix:o_rdonly 0)
                             :owned-p t))
                      (fd (wasmdown.wasi:wasi-descriptor-fd desc)))
                 (unwind-protect
                      (progn
                        (wasmdown.component.runtime::%push-instance
                         guest provider)
                        (let* ((idx (wasmdown.component.runtime::%handle-add
                                     guest ct desc t))
                               (drop (wasmdown.component.runtime::%resource-builtin-host-func
                                      guest :resource-drop ct nil)))
                          (is (= (1+ baseline) (open-fd-count)))
                          (wasmdown.runtime:wasm-call drop idx)
                          (is (wasmdown.wasi:wasi-descriptor-closed-p desc))
                          (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
                          (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc)))
                          (is (null (sb-unix:unix-fstat fd)))
                          (is (= baseline (open-fd-count)))
                          ;; the index was freed: a stale drop cannot
                          ;; release anything a second time
                          (wasmdown.runtime:wasm-call drop idx)
                          (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
                          (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc)))
                          (is (= baseline (open-fd-count)))))
                   (let ((left (wasmdown.wasi:wasi-descriptor-fd desc)))
                     (when (and (integerp left) (not (minusp left)))
                       (sb-unix:unix-close left))))))
          (wasmdown.wasi:close-wasi-preopen pre))))))

(test wasi/fs-stream-reference-released-once
  ;; the stream's reference is released on its terminal event: the
  ;; read-error path and the guest's own drop
  (with-fs-root (root "fs-ref2" '("dir" "f.txt"))
    (fs-mkdir! (fs-p root "dir"))
    (fs-write! (fs-p root "f.txt") "xy")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; an OS error (pread on a directory) ends the read and
             ;; releases the stream's reference
             (let* ((sub (fs-open-file ctx d "dir"
                                       (cons :flags '("directory"))
                                       (cons :flags '("read"))))
                    (fd (wasmdown.wasi:wasi-descriptor-fd sub))
                    (entry (wasmdown.wasi:fs-read-via-stream ctx sub 0)))
               (is (= 2 (wasmdown.wasi:wasi-descriptor-refs sub)))
               (is (null (fs-drain-3 (first entry))))
               (is (equal '(:err . "is-directory")
                          (fs-future-value (second entry))))
               (is (= 1 (wasmdown.wasi:wasi-descriptor-refs sub)))
               (is (not (null (sb-unix:unix-fstat fd))))
               (wasmdown.wasi:close-wasi-descriptor sub)
               (is (zerop (wasmdown.wasi:wasi-descriptor-refs sub)))
               (is (null (sb-unix:unix-fstat fd))))
             ;; the guest dropping its own end (the cancel path)
             ;; releases the same single reference
             (let* ((desc (fs-open-file ctx d "f.txt" nil
                                        (cons :flags '("read"))))
                    (fd (wasmdown.wasi:wasi-descriptor-fd desc))
                    (pair (wasmdown.wasi:fs-read-via-stream ctx desc 0)))
               (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
               (fs-drop-end (first pair))
               (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
               (is (equal '(:ok) (fs-future-value (second pair))))
               (wasmdown.wasi:close-wasi-descriptor desc)
               (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
               (is (null (sb-unix:unix-fstat fd))))
             ;; a WRITE stream outlives its handle too: the bytes still
             ;; land, and the fd closes at the guest's drop
             (let* ((desc (fs-open-file ctx d "f.txt" nil
                                        (cons :flags '("read" "write"))))
                    (fd (wasmdown.wasi:wasi-descriptor-fd desc)))
               (multiple-value-bind (reader writer) (fs-fresh-stream)
                 (let ((future (wasmdown.wasi:fs-write-via-stream
                                ctx desc reader 1)))
                   (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (wasmdown.wasi:close-wasi-descriptor desc)
                   (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (is (not (null (sb-unix:unix-fstat fd))))
                   (is (eql (%completed 1) (fs-stage-write writer '(90))))
                   (fs-drop-end writer)
                   (is (equal '(:ok) (fs-future-value future)))
                   (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (is (null (sb-unix:unix-fstat fd)))
                   ;; "xy" with 'Z' pwritten at offset 1
                   (is (equal '(120 90) (fs-bytes (fs-p root "f.txt"))))))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-descriptor-release-marker-idempotent
  ;; the hold's released marker makes a second release a no-op, and a
  ;; release below zero can never happen
  (with-fs-root (root "fs-ref3" '("a.txt"))
    (fs-write! (fs-p root "a.txt") "z")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((desc (fs-open-file ctx d "a.txt" nil
                                      (cons :flags '("read"))))
                  (fd (wasmdown.wasi:wasi-descriptor-fd desc))
                  (hold (wasmdown.wasi::%descriptor-hold desc)))
             (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (= 1 (wasmdown.wasi::%fd-hold-release hold)))
             (is (not (null (sb-unix:unix-fstat fd))))
             ;; second release: the marker swallows it
             (is (= 1 (wasmdown.wasi::%fd-hold-release hold)))
             (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (not (null (sb-unix:unix-fstat fd))))
             ;; the handle's release is the last one
             (wasmdown.wasi:close-wasi-descriptor desc)
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (null (sb-unix:unix-fstat fd)))
             ;; releasing an already-zero descriptor is a no-op
             (is (zerop (wasmdown.wasi::%descriptor-release desc)))
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-preopen-drop-by-guest-keeps-root
  ;; a guest resource.drop of the preopen's own descriptor handle must
  ;; not close the embedder's root fd, and the root keeps working
  (with-fs-root (root "fs-ref4" '("a.txt"))
    (fs-write! (fs-p root "a.txt") "q")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre))
           (fd (wasmdown.wasi:wasi-preopen-fd pre)))
      (unwind-protect
           (progn
             (wasmdown.wasi:close-wasi-descriptor d)
             (is (not (wasmdown.wasi:wasi-descriptor-closed-p d)))
             (is (eql fd (wasmdown.wasi:wasi-descriptor-fd d)))
             (is (plusp (wasmdown.wasi:wasi-preopen-fd pre)))
             (is (not (null (sb-unix:unix-fstat fd))))
             ;; the root still resolves through the guest's handle
             (=> "regular-file"
                 (fs-type-name (fs-ok-value
                                (wasmdown.wasi:fs-stat-at ctx d nil "a.txt"))))
             ;; only the embedder's close releases it, exactly once
             (wasmdown.wasi:close-wasi-preopen pre)
             (is (eql -1 (wasmdown.wasi:wasi-preopen-fd pre)))
             (is (null (sb-unix:unix-fstat fd))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-preopen-close-invalidates-descriptor
  ;; closing the root invalidates the cached descriptor: open-at/stat
  ;; through it is the WIT error, and a reused fd NUMBER (the freed
  ;; number, handed to an unrelated file) grants no authority
  (with-fs-root (root "fs-ref5" '("a.txt" "b.txt"))
    (fs-write! (fs-p root "a.txt") "hello")
    (fs-write! (fs-p root "b.txt") "x")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre))
           (fd (wasmdown.wasi:wasi-preopen-fd pre)))
      (unwind-protect
           (progn
             (is (plusp fd))
             ;; the root works before the close
             (let ((opened (fs-open-file ctx d "a.txt" nil
                                         (cons :flags '("read")))))
               (is (plusp (wasmdown.wasi:wasi-descriptor-fd opened)))
               (wasmdown.wasi:close-wasi-descriptor opened))
             (wasmdown.wasi:close-wasi-preopen pre)
             (is (eql -1 (wasmdown.wasi:wasi-preopen-fd pre)))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd d)))
             (is (wasmdown.wasi:wasi-descriptor-closed-p d))
             (is (null (sb-unix:unix-fstat fd)))
             ;; the guest's cached descriptor handle names nothing
             (=> "bad-descriptor"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-open-at ctx d nil "b.txt" nil
                                            (cons :flags '("read")))))
             (=> "bad-descriptor"
                 (fs-error-code-of (wasmdown.wasi:fs-stat ctx d)))
             ;; Linux hands out the lowest free fd, so this open takes
             ;; the freed preopen NUMBER and keeps it live — yet the
             ;; closed descriptor still reaches nothing through it
             (let ((reused (sb-unix:unix-open
                            (fs-p root "b.txt")
                            (logior wasmdown.wasi::+o-rdonly+
                                    wasmdown.wasi::+o-cloexec+)
                            0)))
               (unwind-protect
                    (progn
                      (is (eql fd reused))
                      (is (not (null (sb-unix:unix-fstat reused))))
                      (=> "bad-descriptor"
                          (fs-error-code-of
                           (wasmdown.wasi:fs-open-at ctx d nil "b.txt" nil
                                                     (cons :flags '("read"))))))
                 (wasmdown.wasi::%close-fd reused)))
             ;; a live root opened after the close is unaffected
             (let* ((pre2 (wasmdown.wasi:open-wasi-preopen root :name "/r2"))
                    (ctx2 (wasmdown.wasi:make-wasi-context
                           :preopens (list pre2)))
                    (d2 (wasmdown.wasi:wasi-preopen-descriptor pre2)))
               (unwind-protect
                    (=> "regular-file"
                        (fs-type-name (fs-ok-value
                                       (wasmdown.wasi:fs-stat-at
                                        ctx2 d2 nil "a.txt"))))
                 (wasmdown.wasi:close-wasi-preopen pre2)))
             ;; closing twice is a no-op
             (wasmdown.wasi:close-wasi-preopen pre)
             (is (eql -1 (wasmdown.wasi:wasi-preopen-fd pre))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END STREAM FIXTURE: a component writes "hello" through
;;; write-via-stream at offset 0, appends "!" through
;;; append-via-stream, reads the whole file back through
;;; read-via-stream(0) into its own memory and returns n<<8|byte0.

(defparameter *fs-stream-fixture*
  (concatenate
   'string
   "(component"
   " (type $ST (stream u8))"
   " (type $FT (future (result u32 u32)))"
   " (import \"wasi:filesystem/types@0.3.2\""
   "   (instance $fs"
   "     (export \"descriptor\" (type $D (sub resource)))"
   "     (export \"[method]descriptor.write-via-stream\""
   "       (func (param \"self\" (borrow $D)) (param \"data\" (stream u8))"
   "             (param \"offset\" u64) (result (future (result u32 u32)))))"
   "     (export \"[method]descriptor.read-via-stream\""
   "       (func (param \"self\" (borrow $D)) (param \"offset\" u64)"
   "             (result (tuple (stream u8) (future (result u32 u32))))))"
   "     (export \"[method]descriptor.append-via-stream\""
   "       (func (param \"self\" (borrow $D)) (param \"data\" (stream u8))"
   "             (result (future (result u32 u32)))))"
   "     (export \"[method]descriptor.open-at\""
   "       (func (param \"self\" (borrow $D))"
   "             (param \"path-flags\" u32) (param \"path\" string)"
   "             (param \"open-flags\" u32) (param \"flags\" u32)"
   "             (result (result (own $D) u32))))))"
   " (import \"wasi:filesystem/preopens@0.3.2\""
   "   (instance $pre"
   "     (export \"descriptor\" (type $D2 (sub resource)))"
   "     (export \"get-directories\""
   "       (func (result (list (tuple (own $D2) string)))))))"
   " (alias export $pre \"get-directories\" (func $gd))"
   " (alias export $fs \"[method]descriptor.write-via-stream\" (func $wvs))"
   " (alias export $fs \"[method]descriptor.read-via-stream\" (func $rvs))"
   " (alias export $fs \"[method]descriptor.append-via-stream\" (func $avs))"
   " (alias export $fs \"[method]descriptor.open-at\" (func $open-at))"
   " (core module $Mem"
   "   (memory (export \"mem\") 1)"
   "   (global $bump (mut i32) (i32.const 1024))"
   "   (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "     (local $p i32)"
   "     (local.set $p"
   "       (i32.and (i32.add (global.get $bump)"
   "                         (i32.sub (local.get 2) (i32.const 1)))"
   "                (i32.sub (i32.const 0) (local.get 2))))"
   "     (global.set $bump (i32.add (local.get $p) (local.get 3)))"
   "     (local.get $p)))"
   " (core instance $memory (instantiate $Mem))"
   " (core func $gd-lowered"
   "   (canon lower (func $gd) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $wvs-lowered"
   "   (canon lower (func $wvs) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $rvs-lowered"
   "   (canon lower (func $rvs) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $avs-lowered"
   "   (canon lower (func $avs) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $open-lowered"
   "   (canon lower (func $open-at) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (canon stream.new $ST (core func $snew))"
   " (canon stream.write $ST (memory (core memory $memory \"mem\"))"
   "       (core func $swrite))"
   " (canon stream.read $ST (memory (core memory $memory \"mem\"))"
   "       (core func $sread))"
   " (canon stream.drop-writable $ST (core func $sdropw))"
   " (canon future.read $FT (memory (core memory $memory \"mem\"))"
   "       (core func $fread))"
   " (core module $M"
   "   (import \"env\" \"mem\" (memory 1))"
   "   (import \"env\" \"gd\" (func $gd (param i32)))"
   "   (import \"env\" \"wvs\" (func $wvs (param i32 i32 i64) (result i32)))"
   "   (import \"env\" \"rvs\" (func $rvs (param i32 i64 i32)))"
   "   (import \"env\" \"avs\" (func $avs (param i32 i32) (result i32)))"
   "   (import \"env\" \"open\" (func $open (param i32 i32 i32 i32 i32 i32 i32)))"
   "   (import \"env\" \"snew\" (func $snew (result i64)))"
   "   (import \"env\" \"swrite\" (func $swrite (param i32 i32 i32) (result i32)))"
   "   (import \"env\" \"sread\" (func $sread (param i32 i32 i32) (result i32)))"
   "   (import \"env\" \"sdropw\" (func $sdropw (param i32)))"
   "   (import \"env\" \"fread\" (func $fread (param i32 i32) (result i32)))"
   "   (func (export \"f\") (result i32)"
   "     (local $d i32) (local $pair i64) (local $rx i32) (local $tx i32)"
   "     (local $fut i32) (local $r i32) (local $n i32)"
   "     (call $gd (i32.const 0))"
   "     (local.set $d (i32.load (i32.load (i32.const 0))))"
   "     (i32.store (i32.const 256) (i32.const 778530668))"
   "     (i32.store (i32.const 260) (i32.const 7633012))"
   "     (call $open (local.get $d) (i32.const 0) (i32.const 256)"
   "                 (i32.const 7) (i32.const 1) (i32.const 3)"
   "                 (i32.const 128))"
   "     (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 128)))"
   "         (then unreachable))"
   "     (local.set $d (i32.load (i32.const 132)))"
   "     (i32.store8 (i32.const 512) (i32.const 104))"
   "     (i32.store8 (i32.const 513) (i32.const 101))"
   "     (i32.store8 (i32.const 514) (i32.const 108))"
   "     (i32.store8 (i32.const 515) (i32.const 108))"
   "     (i32.store8 (i32.const 516) (i32.const 111))"
   "     (local.set $pair (call $snew))"
   "     (local.set $rx (i32.wrap_i64 (local.get $pair)))"
   "     (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $pair) (i64.const 32))))"
   "     (local.set $fut (call $wvs (local.get $d) (local.get $rx) (i64.const 0)))"
   "     (local.set $r (call $swrite (local.get $tx) (i32.const 512) (i32.const 5)))"
   "     (if (i32.ne (i32.const 80) (local.get $r)) (then unreachable))"
   "     (call $sdropw (local.get $tx))"
   "     (local.set $r (call $fread (local.get $fut) (i32.const 544)))"
   "     (if (i32.ne (i32.const 0) (local.get $r)) (then unreachable))"
   "     (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 544))) (then unreachable))"
   "     (i32.store8 (i32.const 520) (i32.const 33))"
   "     (local.set $pair (call $snew))"
   "     (local.set $rx (i32.wrap_i64 (local.get $pair)))"
   "     (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $pair) (i64.const 32))))"
   "     (local.set $fut (call $avs (local.get $d) (local.get $rx)))"
   "     (local.set $r (call $swrite (local.get $tx) (i32.const 520) (i32.const 1)))"
   "     (if (i32.ne (i32.const 16) (local.get $r)) (then unreachable))"
   "     (call $sdropw (local.get $tx))"
   "     (if (i32.ne (i32.const 0) (call $fread (local.get $fut) (i32.const 548)))"
   "         (then unreachable))"
   "     (call $rvs (local.get $d) (i64.const 0) (i32.const 560))"
   "     (local.set $rx (i32.load (i32.const 560)))"
   "     (local.set $r (call $sread (local.get $rx) (i32.const 600) (i32.const 20)))"
   "     (local.set $n (i32.shr_u (local.get $r) (i32.const 4)))"
   "     (i32.or (i32.shl (local.get $n) (i32.const 8))"
   "             (i32.load8_u (i32.const 600)))))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $memory \"mem\"))"
   "   (export \"gd\" (func $gd-lowered))"
   "   (export \"wvs\" (func $wvs-lowered))"
   "   (export \"rvs\" (func $rvs-lowered))"
   "   (export \"avs\" (func $avs-lowered))"
   "   (export \"open\" (func $open-lowered))"
   "   (export \"snew\" (func $snew))"
   "   (export \"swrite\" (func $swrite))"
   "   (export \"sread\" (func $sread))"
   "   (export \"sdropw\" (func $sdropw))"
   "   (export \"fread\" (func $fread))))))"
   " (func (export \"f\") (result u32) (canon lift (core func $m \"f\"))))"))

(test wasi/e2e-filesystem-stream-component
  (with-fs-root (root "fs-stream-e2e" '("log.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (comp (wasmdown.component.text::parse-component-text
                  *fs-stream-fixture*)))
      (unwind-protect
           (progn
             (wasmdown.component:validate-component comp)
             (let* ((inst (wasmdown.component.runtime:instantiate-component
                           comp
                           :with-args (wasmdown.wasi:filesystem-provider ctx)))
                    (f (wasmdown.component.runtime:runtime-export-value inst "f")))
               ;; 6 bytes read back, first byte 'h' (n<<8|byte0)
               (is (eql 1640 (wasmdown.component.runtime:lifted-func-call f)))
               ;; the host sees exactly the bytes the guest wrote and
               ;; appended
               (is (equal '(104 101 108 108 111 33)
                          (fs-bytes (fs-p root "log.txt"))))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE ASYNC CANON FIXTURE: a guest that canon-lowers ASYNC-typed
;;; filesystem methods with `async` (the packed state|subtask<<4
;;; return, the [ptr] out-param) and drives the waitable-set
;;; protocol. `is-same-object(d, d)` proves the borrow-param lend +
;;; scalar result; `write-via-stream` proves a STREAM/FUTURE result
;;; lowered into the caller's table through the async path, with the
;;; future's resolution delivered by waitable-set.wait after the
;;; guest drops the writable end.

(defparameter *fs-async-canon-fixture*
  (concatenate
   'string
   "(component"
   " (type $ST (stream u8))"
   " (type $FT (future (result u32 u32)))"
   " (import \"wasi:filesystem/types@0.3.2\""
   "   (instance $fs"
   "     (export \"descriptor\" (type $D (sub resource)))"
   "     (export \"[method]descriptor.is-same-object\""
   "       (func async (param \"self\" (borrow $D))"
   "             (param \"other\" (borrow $D)) (result bool)))"
   "     (export \"[method]descriptor.write-via-stream\""
   "       (func async (param \"self\" (borrow $D)) (param \"data\" (stream u8))"
   "             (param \"offset\" u64) (result (future (result u32 u32)))))"
   "     (export \"[method]descriptor.open-at\""
   "       (func (param \"self\" (borrow $D))"
   "             (param \"path-flags\" u32) (param \"path\" string)"
   "             (param \"open-flags\" u32) (param \"flags\" u32)"
   "             (result (result (own $D) u32))))))"
   " (import \"wasi:filesystem/preopens@0.3.2\""
   "   (instance $pre"
   "     (export \"descriptor\" (type $D2 (sub resource)))"
   "     (export \"get-directories\""
   "       (func (result (list (tuple (own $D2) string)))))))"
   " (alias export $pre \"get-directories\" (func $gd))"
   " (alias export $fs \"[method]descriptor.is-same-object\" (func $same))"
   " (alias export $fs \"[method]descriptor.write-via-stream\" (func $wvs))"
   " (alias export $fs \"[method]descriptor.open-at\" (func $open-at))"
   " (core module $Mem"
   "   (memory (export \"mem\") 1)"
   "   (global $bump (mut i32) (i32.const 1024))"
   "   (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "     (local $p i32)"
   "     (local.set $p"
   "       (i32.and (i32.add (global.get $bump)"
   "                         (i32.sub (local.get 2) (i32.const 1)))"
   "                (i32.sub (i32.const 0) (local.get 2))))"
   "     (global.set $bump (i32.add (local.get $p) (local.get 3)))"
   "     (local.get $p)))"
   " (core instance $memory (instantiate $Mem))"
   " (core func $gd-lowered"
   "   (canon lower (func $gd) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $same-lowered"
   "   (canon lower (func $same) async (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $wvs-lowered"
   "   (canon lower (func $wvs) async (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $open-lowered"
   "   (canon lower (func $open-at) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (canon stream.new $ST (core func $snew))"
   " (canon stream.write $ST async (memory (core memory $memory \"mem\"))"
   "       (core func $swrite))"
   " (canon stream.drop-writable $ST (core func $sdropw))"
   " (canon future.read $FT async (memory (core memory $memory \"mem\"))"
   "       (core func $fread))"
   " (canon subtask.drop (core func $subdrop))"
   " (canon waitable.join (core func $join))"
   " (canon waitable-set.new (core func $wsnew))"
   " (canon waitable-set.wait (memory (core memory $memory \"mem\"))"
   "       (core func $wait))"
   " (core module $M"
   "   (import \"env\" \"mem\" (memory 1))"
   "   (import \"env\" \"gd\" (func $gd (param i32)))"
   "   (import \"env\" \"same\" (func $same (param i32 i32 i32) (result i32)))"
   "   (import \"env\" \"wvs\" (func $wvs (param i32 i32 i64 i32) (result i32)))"
   "   (import \"env\" \"open\" (func $open (param i32 i32 i32 i32 i32 i32 i32)))"
   "   (import \"env\" \"snew\" (func $snew (result i64)))"
   "   (import \"env\" \"swrite\" (func $swrite (param i32 i32 i32) (result i32)))"
   "   (import \"env\" \"sdropw\" (func $sdropw (param i32)))"
   "   (import \"env\" \"fread\" (func $fread (param i32 i32) (result i32)))"
   "   (import \"env\" \"subdrop\" (func $subdrop (param i32)))"
   "   (import \"env\" \"join\" (func $join (param i32 i32)))"
   "   (import \"env\" \"wsnew\" (func $wsnew (result i32)))"
   "   (import \"env\" \"wait\" (func $wait (param i32 i32) (result i32)))"
   "   (func $poll-subtask (param $packed i32) (param $ws i32)"
   "     (local $sub i32)"
   "     (if (i32.ne (i32.and (local.get $packed) (i32.const 15)) (i32.const 2))"
   "       (then"
   "         (local.set $sub (i32.shr_u (local.get $packed) (i32.const 4)))"
   "         (call $join (local.get $sub) (local.get $ws))"
   "         (block $done"
   "           (loop $poll"
   "             (drop (call $wait (local.get $ws) (i32.const 224)))"
   "             (br_if $done (i32.eq (i32.load (i32.const 224)) (i32.const 1)))"
   "             (br_if $done (i32.ge_u (i32.load (i32.const 228)) (i32.const 2)))"
   "             (br $poll)))"
   "         (if (i32.ne (i32.const 1) (i32.load (i32.const 224)))"
   "             (then unreachable))"
   "         (call $subdrop (local.get $sub)))))"
   "   (func (export \"f\") (result i32)"
   "     (local $d0 i32) (local $d i32) (local $ws i32) (local $pair i64)"
   "     (local $rx i32) (local $tx i32) (local $fut i32) (local $packed i32)"
   "     (local $r i32) (local $sameok i32) (local $ok i32)"
   "     (call $gd (i32.const 0))"
   "     (local.set $d0 (i32.load (i32.load (i32.const 0))))"
   "     (i32.store8 (i32.const 320) (i32.const 97))"
   "     (i32.store8 (i32.const 321) (i32.const 115))"
   "     (i32.store8 (i32.const 322) (i32.const 121))"
   "     (i32.store8 (i32.const 323) (i32.const 110))"
   "     (i32.store8 (i32.const 324) (i32.const 99))"
   "     (i32.store8 (i32.const 325) (i32.const 46))"
   "     (i32.store8 (i32.const 326) (i32.const 116))"
   "     (i32.store8 (i32.const 327) (i32.const 120))"
   "     (i32.store8 (i32.const 328) (i32.const 116))"
   "     (call $open (local.get $d0) (i32.const 0) (i32.const 320)"
   "                 (i32.const 9) (i32.const 1) (i32.const 3)"
   "                 (i32.const 128))"
   "     (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 128)))"
   "         (then unreachable))"
   "     (local.set $d (i32.load (i32.const 132)))"
   "     (local.set $ws (call $wsnew))"
   "     (local.set $packed (call $same (local.get $d) (local.get $d)"
   "                                  (i32.const 128)))"
   "     (if (i32.ne (i32.const 2) (i32.and (local.get $packed) (i32.const 15)))"
   "         (then unreachable))"
   "     (call $poll-subtask (local.get $packed) (local.get $ws))"
   "     (local.set $sameok (i32.load8_u (i32.const 128)))"
   "     (local.set $pair (call $snew))"
   "     (local.set $rx (i32.wrap_i64 (local.get $pair)))"
   "     (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $pair) (i64.const 32))))"
   "     (i32.store16 (i32.const 512) (i32.const 0x6968))"
   "     (local.set $packed (call $wvs (local.get $d) (local.get $rx) (i64.const 0)"
   "                                  (i32.const 160)))"
   "     (if (i32.ne (i32.const 2) (i32.and (local.get $packed) (i32.const 15)))"
   "         (then unreachable))"
   "     (call $poll-subtask (local.get $packed) (local.get $ws))"
   "     (local.set $fut (i32.load (i32.const 160)))"
   "     (call $join (local.get $fut) (local.get $ws))"
   "     (local.set $r (call $fread (local.get $fut) (i32.const 192)))"
   "     (if (i32.ne (i32.const -1) (local.get $r)) (then unreachable))"
   "     (local.set $r (call $swrite (local.get $tx) (i32.const 512) (i32.const 2)))"
   "     (if (i32.ne (i32.const 32) (local.get $r)) (then unreachable))"
   "     (call $sdropw (local.get $tx))"
   "     (local.set $r (call $wait (local.get $ws) (i32.const 224)))"
   "     (if (i32.ne (i32.const 4) (local.get $r)) (then unreachable))"
   "     (if (i32.ne (i32.const 0) (i32.load (i32.const 228)))"
   "         (then unreachable))"
   "     (local.set $ok (i32.load8_u (i32.const 192)))"
   "     (i32.or (i32.shl (local.get $sameok) (i32.const 8))"
   "             (i32.or (i32.shl (local.get $ok) (i32.const 4))"
   "                     (i32.const 1)))))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $memory \"mem\"))"
   "   (export \"gd\" (func $gd-lowered))"
   "   (export \"same\" (func $same-lowered))"
   "   (export \"wvs\" (func $wvs-lowered))"
   "   (export \"open\" (func $open-lowered))"
   "   (export \"snew\" (func $snew))"
   "   (export \"swrite\" (func $swrite))"
   "   (export \"sdropw\" (func $sdropw))"
   "   (export \"fread\" (func $fread))"
   "   (export \"subdrop\" (func $subdrop))"
   "   (export \"join\" (func $join))"
   "   (export \"wsnew\" (func $wsnew))"
   "   (export \"wait\" (func $wait))))))"
   " (func (export \"f\") (result u32) (canon lift (core func $m \"f\"))))"))

(test wasi/e2e-async-canon-filesystem-component
  (with-fs-root (root "fs-async-e2e" '("async.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (comp (wasmdown.component.text::parse-component-text
                  *fs-async-canon-fixture*)))
      (unwind-protect
           (progn
             (wasmdown.component:validate-component comp)
             (let* ((inst (wasmdown.component.runtime:instantiate-component
                           comp
                           :with-args (wasmdown.wasi:filesystem-provider ctx)))
                    (f (wasmdown.component.runtime:runtime-export-value inst "f")))
               ;; sameok=1 (<<8), the future's ok discriminant 0 (<<4),
               ;; the FUTURE_READ event delivered (1)
               (is (eql 257 (wasmdown.component.runtime:lifted-func-call f)))
               ;; the async-lowered write-via-stream put the bytes on disk
               (is (string= "hi" (fs-bytes->string (fs-p root "async.txt"))))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(defun fs-bytes->string (path)
  (map 'string #'code-char (fs-bytes path)))

;;; ===============================================================
;;; M16 SLICE 4 — wasi:sockets: the network authority, the TCP/UDP
;;; resources, ip-name-lookup, the errno mapping, and a TCP-echo
;;; e2e component.
;;; ===============================================================

;;; Domains + drivers.
(defun sock-addr (port &optional (host '(127 0 0 1)))
  (cons "ipv4" (list (cons "port" port) (cons "address" host))))

(defun sock-addr6 (port &optional (host '(0 0 0 0 0 0 0 1)))
  (cons "ipv6" (list (cons "port" port)
                     (cons "flow-info" 0)
                     (cons "address" host)
                     (cons "scope-id" 0))))

(defun sock-endpoint (&optional (port :any))
  (wasmdown.wasi:make-wasi-endpoint :address '(127 0 0 1) :port port))

(defun sock-network (&key (outbound :loopback) (listen :loopback) (dns :none))
  (wasmdown.wasi:make-wasi-network
   :outbound (if (eq outbound :loopback) (list (sock-endpoint)) outbound)
   :listen (if (eq listen :loopback) (list (sock-endpoint)) listen)
   :dns dns))

(defun sock-ok (result) (cdr result))
(defun sock-code (result)
  (wasmdown.wasi:sockets-error-code (cdr result)))
(defun sock-rep (result) (third (sock-ok result)))

(defun sock-value-port (value)
  "The port of an ip-socket-address domain value."
  (cdr (assoc "port" (cdr value) :test #'string=)))

(defun sock-future-code (value)
  "The error-code case name of a (:err . code) future value (NIL for
an ok result)."
  (when (and (consp value) (eq (car value) :err))
    (wasmdown.wasi:sockets-error-code (cdr value))))

(defun sock-stream ()
  (wasmdown.component.runtime:make-host-stream-pair *u8-ct*))

(defun sock-stage (writer bytes)
  "The guest's write through WRITER; the packed event code."
  (rtcall "%END-COPY" writer
          (wasmdown.component.runtime:make-host-buffer
           (length bytes) :values bytes)
          nil)
  (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer)))

(defun sock-drop (end) (rtcall "%END-DROP" end))

(defun sock-drain (reader &optional (limit 64) (want nil))
  "Accumulate CL reads from READER until WANT values arrived, no
progress is possible, or the end is DROPPED."
  (let ((out '()))
    (loop
      (let ((buf (wasmdown.component.runtime:make-host-buffer limit)))
        (rtcall "%END-COPY" reader buf t)
        (setf out (append out
                          (wasmdown.component.runtime:host-buffer-values buf)))
        (let ((ev (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader))))
          (when (or (and want (>= (length out) want))
                    (null ev)
                    (< (logand ev #xf) 2))
            (return out)))))))

(defun sock-future (future)
  "The single value a HOST future delivers."
  (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
    (rtcall "%END-COPY" future buf t)
    (first (wasmdown.component.runtime:host-buffer-values buf))))

(defun sock-accept (listener-stream)
  "One accepted tcp-socket rep out of LISTENER-STREAM, or NIL."
  (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
    (rtcall "%END-COPY" listener-stream buf t)
    (let ((values (wasmdown.component.runtime:host-buffer-values buf)))
      (and values (third (first values))))))

;;; ---------------------------------------------------------------
;;; The sockaddr layout is empirical: sizes + the bytes the domain
;;; conversion writes (the slice-3b `struct dirent' discipline).

(test wasi/sockets-sockaddr-layout
  (is (= 16 (sb-alien:alien-size
             (sb-alien:struct wasmdown.wasi::%sockaddr-in) :bytes)))
  (is (= 28 (sb-alien:alien-size
             (sb-alien:struct wasmdown.wasi::%sockaddr-in6) :bytes)))
  (multiple-value-bind (alien len family)
      (wasmdown.wasi:ip-socket-address->sockaddr (sock-addr 4321))
    (unwind-protect
         (let ((sap (sb-alien:alien-sap alien))
               (d (sb-alien:deref alien)))
           (is (eql :ipv4 family))
           (is (= 16 len))
           ;; family at 0 (host-endian u16), port at 2 (network order),
           ;; address at 4..7
           (is (= wasmdown.wasi::+af-inet+ (sb-sys:sap-ref-8 sap 0)))
           (is (= 0 (sb-sys:sap-ref-8 sap 1)))
           (is (= #x10 (sb-sys:sap-ref-8 sap 2)))     ; 4321 = #x10E1
           (is (= #xE1 (sb-sys:sap-ref-8 sap 3)))
           (is (equal '(127 0 0 1)
                      (loop for i below 4
                            collect (sb-sys:sap-ref-8 sap (+ 4 i)))))
           ;; the struct's slots agree with those byte offsets
           (is (= wasmdown.wasi::+af-inet+
                  (sb-alien:slot d 'wasmdown.wasi::sin-family)))
           (is (= #xE110 (sb-alien:slot d 'wasmdown.wasi::sin-port)))
           ;; the round trip returns the same domain value
           (is (equal (sock-addr 4321)
                      (wasmdown.wasi:sockaddr->ip-socket-address sap))))
      (sb-alien:free-alien alien)))
  (multiple-value-bind (alien len family)
      (wasmdown.wasi:ip-socket-address->sockaddr
       (sock-addr6 1234 '(0 0 0 0 0 0 0 1)))
    (unwind-protect
         (let ((sap (sb-alien:alien-sap alien))
               (d (sb-alien:deref alien)))
           (is (eql :ipv6 family))
           (is (= 28 len))
           (is (= wasmdown.wasi::+af-inet6+ (sb-sys:sap-ref-8 sap 0)))
           (is (= #x04 (sb-sys:sap-ref-8 sap 2)))     ; 1234 = #x04D2
           (is (= #xD2 (sb-sys:sap-ref-8 sap 3)))
           ;; sin6_flowinfo at 4, sin6_addr at 8, sin6_scope_id at 24
           (is (= 0 (sb-alien:slot d 'wasmdown.wasi::sin6-flowinfo)))
           (is (= 0 (sb-alien:slot d 'wasmdown.wasi::sin6-scope-id)))
           (is (equal (sock-addr6 1234 '(0 0 0 0 0 0 0 1))
                      (wasmdown.wasi:sockaddr->ip-socket-address sap))))
      (sb-alien:free-alien alien)))
  ;; a malformed address is the WIT's invalid-argument, never a crash
  (let ((caught
          (handler-case
              (progn (wasmdown.wasi:ip-socket-address->sockaddr
                      (cons "ipv4" (list (cons "port" 1)
                                         (cons "address" '(1 2 3)))))
                     nil)
            (wasmdown.wasi:wasi-socket-error (c)
              (wasmdown.wasi:sockets-error-code
               (wasmdown.wasi:wasi-socket-error-code c))))))
    (is (string= "invalid-argument" caught))))

;;; ---------------------------------------------------------------
;;; The capability policy.

(test wasi/sockets-network-policy
  (let ((net (sock-network :dns '("localhost" "**.example.com"))))
    (is (wasmdown.wasi:wasi-network-allows-outbound-p
         net :ipv4 '(127 0 0 1) 80))
    (is (wasmdown.wasi:wasi-network-allows-outbound-p
         net :ipv4 '(127 0 0 1) 0))
    (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
              net :ipv4 '(10 1 2 3) 80)))
    (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
              net :ipv6 '(0 0 0 0 0 0 0 1) 80)))
    (is (wasmdown.wasi:wasi-network-allows-listen-p
         net :ipv4 '(127 0 0 1) 0))
    (is (not (wasmdown.wasi:wasi-network-allows-listen-p
              net :ipv4 '(0 0 0 0) 0)))
    ;; an endpoint's declared port narrows the match
    (let ((net2 (wasmdown.wasi:make-wasi-network
                 :outbound (list (wasmdown.wasi:make-wasi-endpoint
                                  :address '(127 0 0 1) :port 8080)))))
      (is (wasmdown.wasi:wasi-network-allows-outbound-p
           net2 :ipv4 '(127 0 0 1) 8080))
      (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
                net2 :ipv4 '(127 0 0 1) 8081))))
    ;; DNS: exact + `**.suffix' patterns
    (is (wasmdown.wasi:wasi-network-allows-dns-p net "localhost"))
    (is (wasmdown.wasi:wasi-network-allows-dns-p net "a.example.com"))
    (is (wasmdown.wasi:wasi-network-allows-dns-p net "deep.a.example.com"))
    (is (not (wasmdown.wasi:wasi-network-allows-dns-p net "elsewhere.test"))))
  ;; the deny-all default and the explicit :any escape hatch
  (let ((deny (wasmdown.wasi:make-wasi-network))
        (any (wasmdown.wasi:make-wasi-network
              :outbound :any :listen :any :dns :all)))
    (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
              deny :ipv4 '(127 0 0 1) 80)))
    (is (not (wasmdown.wasi:wasi-network-allows-listen-p
              deny :ipv4 '(0 0 0 0) 0)))
    (is (not (wasmdown.wasi:wasi-network-allows-dns-p deny "localhost")))
    (is (wasmdown.wasi:wasi-network-allows-outbound-p
         any :ipv6 '(0 0 0 0 0 0 0 1) 443))
    (is (wasmdown.wasi:wasi-network-allows-dns-p any "anything.test")))
  ;; a NIL network authority is deny-all, never an error
  (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
            nil :ipv4 '(127 0 0 1) 80))))

;;; ---------------------------------------------------------------
;;; TCP over real loopback sockets: create/bind/listen/connect/
;;; accept/send/receive, both directions.

(test wasi/sockets-tcp-loopback
  (let* ((net (sock-network :dns '("localhost")))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (accepted nil))
    (unwind-protect
         (progn
           (is (eq :unbound (wasmdown.wasi:wasi-tcp-socket-state server)))
           (is (eq :unbound (wasmdown.wasi:wasi-tcp-socket-state client)))
           ;; bind(127.0.0.1:0) -> an ephemeral loopback port
           (is (equal '(:ok)
                      (wasmdown.wasi:tcp-bind nil net server (sock-addr 0))))
           (let* ((local (sock-ok (wasmdown.wasi:tcp-get-local-address
                                   nil net server)))
                  (port (sock-value-port local)))
             (is (plusp port))
             (is (equal "ipv4" (second (wasmdown.wasi:tcp-get-address-family
                                        nil net server))))
             (is (null (wasmdown.wasi:tcp-get-is-listening nil net server)))
             ;; listen -> a stream of accepted sockets
             (let ((listening (wasmdown.wasi:tcp-listen nil net server)))
               (is (eq :ok (car listening)))
               (is (wasmdown.wasi:tcp-get-is-listening nil net server))
               ;; connect (the async resource method; the host resolves
               ;; eagerly)
               (is (equal '(:ok)
                          (wasmdown.wasi:tcp-connect nil net client
                                                     (sock-addr port))))
               (is (equal port
                          (sock-value-port
                           (sock-ok (wasmdown.wasi:tcp-get-remote-address
                                     nil net client)))))
               (is (equal port
                          (sock-value-port
                           (sock-ok (wasmdown.wasi:tcp-get-local-address
                                     nil net server)))))
               (setf accepted (sock-accept (cdr listening)))
               (is (not (null accepted)))
               (is (eq :connected
                       (wasmdown.wasi:wasi-tcp-socket-state accepted)))
               ;; client -> server
               (multiple-value-bind (reader writer) (sock-stream)
                 (let ((future (wasmdown.wasi:tcp-send nil net client reader)))
                   (is (eql (%completed 4)
                            (sock-stage writer '(112 105 110 103))))
                   (sock-drop writer)
                   (is (equal '(:ok) (sock-future future)))))
               (let* ((pair (wasmdown.wasi:tcp-receive nil net accepted))
                      (stream (first pair))
                      (future (second pair)))
                 (is (equal '(112 105 110 103) (sock-drain stream 64 4)))
                 (sock-drop stream)
                 (is (equal '(:ok) (sock-future future))))
               ;; server -> client
               (multiple-value-bind (reader writer) (sock-stream)
                 (let ((future (wasmdown.wasi:tcp-send nil net accepted reader)))
                   (is (eql (%completed 4)
                            (sock-stage writer '(112 111 110 103))))
                   (sock-drop writer)
                   (is (equal '(:ok) (sock-future future)))))
               (let* ((pair (wasmdown.wasi:tcp-receive nil net client))
                      (stream (first pair))
                      (future (second pair)))
                 (is (equal '(112 111 110 103) (sock-drain stream 64 4)))
                 ;; receive is once-per-socket: the second call is the
                 ;; REFUSED shape (a dropped stream + invalid-state)
                 (let ((again (wasmdown.wasi:tcp-receive nil net client)))
                   (is (equal nil (sock-drain (first again) 8 1)))
                   (is (string= "invalid-state"
                                (sock-future-code (sock-future
                                                   (second again))))))
                 (sock-drop stream)
                 (is (equal '(:ok) (sock-future future)))))))
      (when accepted (wasmdown.wasi:close-wasi-tcp-socket accepted))
      (wasmdown.wasi:close-wasi-tcp-socket client)
      (wasmdown.wasi:close-wasi-tcp-socket server))))

;;; ---------------------------------------------------------------
;;; The keep-alive / hop-limit / buffer knobs and their validation.

(test wasi/tcp-accept-source-would-block
  ;; M17 step 5: accept4 is NONBLOCKING. An empty accept queue is
  ;; WOULD-BLOCK ((nil nil) — never an empty-EOF, never a spin), and a
  ;; queued connection is an element VECTOR of own handles.
  (let* ((net (sock-network))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (accepted nil))
    (unwind-protect
         (progn
           (is (equal '(:ok)
                      (wasmdown.wasi:tcp-bind nil net server (sock-addr 0))))
           (let* ((port (sock-value-port
                         (sock-ok (wasmdown.wasi:tcp-get-local-address
                                   nil net server))))
                  (listening (wasmdown.wasi:tcp-listen nil net server))
                  (reader (cdr listening))
                  (source (rtcall "ASYNC-END-HOST-SOURCE"
                                  (rtcall "ASYNC-END-OTHER" reader))))
             (is (functionp source))
             ;; nothing queued: WOULD-BLOCK, not EOF
             (multiple-value-bind (seq eof) (funcall source 4)
               (is (null seq))
               (is (null eof)))
             (is (equal '(:ok)
                        (wasmdown.wasi:tcp-connect nil net client
                                                   (sock-addr port))))
             (multiple-value-bind (seq eof) (funcall source 4)
               (is (vectorp seq))
               (is (= 1 (length seq)))
               (is (null eof))
               (setf accepted (third (aref seq 0))))
             (is (eq :connected
                     (wasmdown.wasi:wasi-tcp-socket-state accepted)))))
      (when accepted (wasmdown.wasi:close-wasi-tcp-socket accepted))
      (wasmdown.wasi:close-wasi-tcp-socket client)
      (wasmdown.wasi:close-wasi-tcp-socket server))))

(test wasi/sockets-tcp-knobs
  (let* ((net (sock-network))
         (sock (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4")))))
    (unwind-protect
         (progn
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-keep-alive-enabled
                              nil net sock t)))
           (is (eq t (sock-ok (wasmdown.wasi::tcp-get-keep-alive-enabled
                               nil net sock))))
           ;; 5s idle / 2s interval / 3 probes
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-keep-alive-idle-time
                              nil net sock 5000000000)))
           (is (= 5000000000
                  (sock-ok (wasmdown.wasi::tcp-get-keep-alive-idle-time
                            nil net sock))))
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-keep-alive-interval
                              nil net sock 2000000000)))
           (is (= 2000000000
                  (sock-ok (wasmdown.wasi::tcp-get-keep-alive-interval
                            nil net sock))))
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-keep-alive-count
                              nil net sock 3)))
           (is (= 3 (sock-ok (wasmdown.wasi::tcp-get-keep-alive-count
                              nil net sock))))
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-hop-limit
                              nil net sock 7)))
           (is (= 7 (sock-ok (wasmdown.wasi::tcp-get-hop-limit nil net sock))))
           ;; the buffer size is a hint: set 128 KiB, read back > 0
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-receive-buffer-size
                              nil net sock 131072)))
           (is (plusp (sock-ok (wasmdown.wasi::tcp-get-receive-buffer-size
                                nil net sock))))
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-send-buffer-size
                              nil net sock 131072)))
           (is (plusp (sock-ok (wasmdown.wasi::tcp-get-send-buffer-size
                                nil net sock))))
           ;; zero is the WIT's invalid-argument on every knob
           (is (string= "invalid-argument"
                        (sock-code (wasmdown.wasi::tcp-set-keep-alive-idle-time
                                    nil net sock 0))))
           (is (string= "invalid-argument"
                        (sock-code (wasmdown.wasi::tcp-set-listen-backlog-size
                                    nil net sock 0))))
           (is (string= "invalid-argument"
                        (sock-code (wasmdown.wasi::tcp-set-hop-limit
                                    nil net sock 0))))
           (is (string= "invalid-argument"
                        (sock-code (wasmdown.wasi::tcp-set-send-buffer-size
                                    nil net sock 0))))
           ;; a valid backlog is accepted
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-listen-backlog-size
                              nil net sock 32))))
      (wasmdown.wasi:close-wasi-tcp-socket sock))))

;;; ---------------------------------------------------------------
;;; UDP over loopback.

(test wasi/sockets-udp-loopback
  (let* ((net (sock-network))
         (a (sock-rep (wasmdown.wasi:udp-create nil net (cons :enum "ipv4"))))
         (b (sock-rep (wasmdown.wasi:udp-create nil net (cons :enum "ipv4")))))
    (unwind-protect
         (progn
           (is (equal '(:ok) (wasmdown.wasi:udp-bind nil net a (sock-addr 0))))
           (is (equal '(:ok) (wasmdown.wasi:udp-bind nil net b (sock-addr 0))))
           (let ((pa (sock-value-port
                      (sock-ok (wasmdown.wasi:udp-get-local-address
                                nil net a))))
                 (pb (sock-value-port
                      (sock-ok (wasmdown.wasi:udp-get-local-address
                                nil net b)))))
             (is (and (plusp pa) (plusp pb) (/= pa pb)))
             ;; a datagram with an explicit peer address (sendto)
             (is (equal '(:ok) (wasmdown.wasi:udp-send
                                nil net a '(1 2 3) (cons :some (sock-addr pb)))))
             (let ((got (wasmdown.wasi:udp-receive nil net b)))
               (is (eq :ok (car got)))
               (is (equal '(1 2 3) (first (cdr got))))
               (is (equal pa (sock-value-port (second (cdr got))))))
             ;; connect + send (the peer is implicit) + receive
             (is (equal '(:ok) (wasmdown.wasi:udp-connect
                                nil net b (sock-addr pa))))
             (is (equal pa (sock-value-port
                            (sock-ok (wasmdown.wasi:udp-get-remote-address
                                      nil net b)))))
             (is (equal '(:ok) (wasmdown.wasi:udp-send
                                nil net b '(9 8) :none)))
             (let ((got (wasmdown.wasi:udp-receive nil net a)))
               (is (equal '(9 8) (first (cdr got))))
               (is (equal pb (sock-value-port (second (cdr got))))))
             ;; a connected socket rejects a different explicit peer
             (is (string= "invalid-argument"
                          (sock-code (wasmdown.wasi:udp-send
                                      nil net b '(1) (cons :some (sock-addr pb))))))
             ;; an unconnected socket needs a peer address
             (is (string= "invalid-argument"
                          (sock-code (wasmdown.wasi:udp-send
                                      nil net a '(1) :none))))
             (is (equal '(:ok) (wasmdown.wasi:udp-disconnect nil net b)))
             (is (string= "invalid-state"
                          (sock-code (wasmdown.wasi:udp-disconnect nil net b))))
             (is (equal "ipv4"
                        (second (wasmdown.wasi:udp-get-address-family
                                 nil net a))))
             ;; unicast hop limit
             (is (equal '(:ok) (wasmdown.wasi::udp-set-unicast-hop-limit
                                nil net a 5)))
             (is (= 5 (sock-ok (wasmdown.wasi::udp-get-unicast-hop-limit
                                nil net a))))))
      (wasmdown.wasi:close-wasi-udp-socket b)
      (wasmdown.wasi:close-wasi-udp-socket a))))

;;; ---------------------------------------------------------------
;;; ip-name-lookup: literals are parsed as-is; names consult the DNS
;;; policy and then getaddrinfo (localhost is /etc/hosts — hermetic).

(test wasi/sockets-name-lookup
  (let ((net (sock-network :dns '("localhost"))))
    (let ((ok (wasmdown.wasi:ip-name-lookup-resolve nil net "localhost")))
      (is (eq :ok (car ok)))
      (is (find (cons "ipv4" '(127 0 0 1)) (cdr ok) :test #'equal)))
    ;; an IPv4 literal needs no DNS policy at all
    (let ((lit (wasmdown.wasi:ip-name-lookup-resolve
                nil (wasmdown.wasi:make-wasi-network) "127.0.0.1")))
      (is (equal '(:ok ("ipv4" 127 0 0 1)) lit)))
    ;; an IPv6 literal goes through the numeric resolver
    (let ((lit (wasmdown.wasi:ip-name-lookup-resolve
                nil (wasmdown.wasi:make-wasi-network) "::1")))
      (is (eq :ok (car lit)))
      (is (equal "ipv6" (car (first (cdr lit))))))
    ;; an undeclared name is denied by the policy
    (is (string= "access-denied"
                 (sock-code (wasmdown.wasi:ip-name-lookup-resolve
                             nil (wasmdown.wasi:make-wasi-network)
                             "localhost"))))
    ;; an empty name is invalid-argument
    (is (string= "invalid-argument"
                 (sock-code (wasmdown.wasi:ip-name-lookup-resolve
                             nil net ""))))))

;;; ---------------------------------------------------------------
;;; The policy refusals at the operation level: an undeclared
;;; endpoint/listen address/DNS name is the WIT's access-denied.

(test wasi/sockets-policy-refusals
  (let* ((net (sock-network))
         (tcp (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4"))))
         (udp (sock-rep (wasmdown.wasi:udp-create nil net (cons :enum "ipv4")))))
    (unwind-protect
         (progn
           (is (string= "access-denied"
                        (sock-code (wasmdown.wasi:tcp-bind
                                    nil net tcp (sock-addr 0 '(10 1 2 3))))))
           (is (string= "access-denied"
                        (sock-code (wasmdown.wasi:tcp-connect
                                    nil net tcp (sock-addr 80 '(10 1 2 3))))))
           (is (string= "access-denied"
                        (sock-code (wasmdown.wasi:udp-send
                                    nil net udp '(1)
                                    (cons :some (sock-addr 53 '(10 1 2 3)))))))
           ;; the socket is still usable after a refused operation
           (is (equal '(:ok) (wasmdown.wasi:udp-bind
                              nil net udp (sock-addr 0))))
           ;; a refused connect leaves the socket CLOSED (the WIT's
           ;; post-failure state: the only valid action is drop)
           (is (eq :closed (wasmdown.wasi:wasi-tcp-socket-state tcp)))
           ;; no network authority => everything is refused
           (let* ((ctx (wasmdown.wasi:make-wasi-context))
                  (inst (wasmdown.wasi:sockets-types-instance ctx))
                  (create (provider-fn inst "[static]tcp-socket.create")))
             (declare (ignore create))
             (let* ((bare (wasmdown.wasi::tcp-create
                           nil nil (cons :enum "ipv4")))
                    (sock (sock-rep bare)))
               (unwind-protect
                    (is (string= "access-denied"
                                 (sock-code (wasmdown.wasi:tcp-connect
                                             nil nil sock (sock-addr 80)))))
                 (wasmdown.wasi:close-wasi-tcp-socket sock)))))
      (wasmdown.wasi:close-wasi-udp-socket udp)
      (wasmdown.wasi:close-wasi-tcp-socket tcp))))

;;; ---------------------------------------------------------------
;;; The errno -> sockets error-code mapping, observed through real
;;; loopback failures.

(test wasi/sockets-error-mappings
  (let* ((net (sock-network))
         (a (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4"))))
         (b (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4"))))
         (c (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4")))))
    (unwind-protect
         (progn
           (is (equal '(:ok) (wasmdown.wasi:tcp-bind nil net a (sock-addr 0))))
           (let ((port (sock-value-port
                        (sock-ok (wasmdown.wasi:tcp-get-local-address
                                  nil net a)))))
             ;; a bound-but-not-listening port refuses the connection
             (is (string= "connection-refused"
                          (sock-code (wasmdown.wasi:tcp-connect
                                      nil net b (sock-addr port)))))
             ;; the failed connect closed B (the WIT's rule)
             (is (eq :closed (wasmdown.wasi:wasi-tcp-socket-state b)))
             ;; a listening bind blocks a second bind (address-in-use)
             (is (eq :ok (car (wasmdown.wasi:tcp-listen nil net a))))
             (is (string= "address-in-use"
                          (sock-code (wasmdown.wasi:tcp-bind
                                      nil net c (sock-addr port)))))
             ;; double bind / connect-after-listen = invalid-state
             (is (string= "invalid-state"
                          (sock-code (wasmdown.wasi:tcp-bind
                                      nil net a (sock-addr 0)))))
             (is (string= "invalid-state"
                          (sock-code (wasmdown.wasi:tcp-connect
                                      nil net a (sock-addr port)))))
             (is (string= "invalid-state"
                          (sock-code (wasmdown.wasi:tcp-listen nil net a)))))
           ;; send on a non-connected socket is invalid-state, and the
           ;; guest's stream end learns DROPPED
           (multiple-value-bind (reader writer) (sock-stream)
             (let ((future (wasmdown.wasi:tcp-send nil net c reader)))
               (is (string= "invalid-state"
                            (sock-future-code (sock-future future))))
               (is (eql 1 (logand (sock-stage writer '(1)) #xf))))))
      (wasmdown.wasi:close-wasi-tcp-socket c)
      (wasmdown.wasi:close-wasi-tcp-socket b)
      (wasmdown.wasi:close-wasi-tcp-socket a))))

;;; ---------------------------------------------------------------
;;; The provider surface: the resource cts + every method exported,
;;; and the two import names.

(test wasi/sockets-provider-surface
  (let* ((net (sock-network))
         (ctx (wasmdown.wasi:make-wasi-context :networks (list net)))
         (inst (wasmdown.wasi:sockets-types-instance ctx))
         (prov (wasmdown.wasi:sockets-provider ctx)))
    (is (eq :type (car (gethash "tcp-socket"
                                (wasmdown.component.runtime:comp-instance-exports
                                 inst)))))
    (is (eq :type (car (gethash "udp-socket"
                                (wasmdown.component.runtime:comp-instance-exports
                                 inst)))))
    (dolist (name '("[static]tcp-socket.create" "[method]tcp-socket.bind"
                    "[method]tcp-socket.connect" "[method]tcp-socket.listen"
                    "[method]tcp-socket.send" "[method]tcp-socket.receive"
                    "[method]tcp-socket.get-local-address"
                    "[method]tcp-socket.get-remote-address"
                    "[method]tcp-socket.get-is-listening"
                    "[method]tcp-socket.get-address-family"
                    "[method]tcp-socket.set-listen-backlog-size"
                    "[method]tcp-socket.get-keep-alive-enabled"
                    "[method]tcp-socket.get-receive-buffer-size"
                    "[static]udp-socket.create" "[method]udp-socket.bind"
                    "[method]udp-socket.connect" "[method]udp-socket.disconnect"
                    "[method]udp-socket.send" "[method]udp-socket.receive"
                    "[method]udp-socket.get-local-address"))
      (is (wasmdown.wasi:provider-instance-function inst name)))
    (is (= 2 (length prov)))
    (is (assoc "wasi:sockets/types@0.3.2" prov :test #'string=))
    (is (assoc "wasi:sockets/ip-name-lookup@0.3.2" prov :test #'string=))
    ;; the provider-level create is callable through the lifted-func
    ;; boundary (the static method's domain arg + resource result)
    (let ((result (call-provider inst "[static]tcp-socket.create"
                                 (cons :enum "ipv4"))))
      (is (eq :ok (car result)))
      (let ((sock (sock-rep result)))
        (unwind-protect
             (is (eq :unbound (wasmdown.wasi:wasi-tcp-socket-state sock)))
          (wasmdown.wasi:close-wasi-tcp-socket sock))))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END TCP ECHO: a component imports wasi:sockets/types,
;;; creates a tcp-socket, connects to a host echo server on loopback,
;;; sends "ping" through send(stream), reads "pong" through
;;; receive(), and returns the first echoed byte.
;;;
;;; Two text-lane workarounds are visible here (both pre-existing
;;; validator/parser limitations, not sockets behaviour):
;;;   * the compound-param (connect) export is declared BEFORE the
;;;     own-result export (create): the imported-instance
;;;     declarator-space remap resolves the compound param only in
;;;     that order;
;;;   * a handful of scalar `(type $DN u32)' declarations keep the
;;;     canon-opt memory check's raw declarator index in range.
;;; The declared remote-address shape and the error payload are
;;; narrowed (u32) for the same reason; the RUNTIME always lifts the
;;; arguments/results with the provider's real WIT functype, so the
;;; memory layout exercised is the real ip-socket-address one.

(defun tcp-echo-fixture (port)
  (format nil "(component ~
 (type $SADDR (variant ~
   (case \"ipv4\" (tuple u32 u32 u32)) ~
   (case \"ipv6\" (tuple u32 u32 u32)))) ~
 (import \"wasi:sockets/types@0.3.2\" ~
  (instance $sock ~
   (export \"tcp-socket\" (type $T (sub resource))) ~
   (export \"[method]tcp-socket.connect\" ~
     (func async (param \"self\" (borrow $T)) (param \"remote-address\" $SADDR) ~
                 (result (result (error u32))))) ~
   (export \"[static]tcp-socket.create\" ~
     (func (param \"address-family\" u32) (result (result (own $T) u32)))) ~
   (export \"[method]tcp-socket.send\" ~
     (func (param \"self\" (borrow $T)) (param \"data\" (stream u8)) ~
                 (result (future (result (error u32)))))) ~
   (export \"[method]tcp-socket.receive\" ~
     (func (param \"self\" (borrow $T)) ~
                 (result (tuple (stream u8) (future (result (error u32))))))))) ~
 (alias export $sock \"[static]tcp-socket.create\" (func $create)) ~
 (alias export $sock \"[method]tcp-socket.connect\" (func $connect)) ~
 (alias export $sock \"[method]tcp-socket.send\" (func $send)) ~
 (alias export $sock \"[method]tcp-socket.receive\" (func $receive)) ~
 (type $D1 u32) (type $D2 u32) (type $D3 u32) (type $D4 u32) ~
 (type $D5 u32) (type $D6 u32) (type $D7 u32) (type $D8 u32) ~
 (type $D9 u32) (type $D10 u32) (type $D11 u32) (type $D12 u32) ~
 (core module $Mem (memory (export \"mem\") 1)) ~
 (core instance $memory (instantiate $Mem)) ~
 (core func $create-l (canon lower (func $create) (memory (core memory $memory \"mem\")))) ~
 (core func $connect-l (canon lower (func $connect) async (memory (core memory $memory \"mem\")))) ~
 (core func $send-l (canon lower (func $send) (memory (core memory $memory \"mem\")))) ~
 (core func $receive-l (canon lower (func $receive) (memory (core memory $memory \"mem\")))) ~
 (type $ST (stream u8)) ~
 (type $FT (future (result (error u32)))) ~
 (canon stream.new $ST (core func $snew)) ~
 (canon stream.write $ST async (memory (core memory $memory \"mem\")) (core func $swrite)) ~
 (canon stream.read $ST async (memory (core memory $memory \"mem\")) (core func $sread)) ~
 (canon stream.drop-writable $ST (core func $sdropw)) ~
 (canon subtask.drop (core func $subdrop)) ~
 (canon waitable.join (core func $join)) ~
 (canon waitable-set.new (core func $wsnew)) ~
 (canon waitable-set.wait (memory (core memory $memory \"mem\")) (core func $wait)) ~
 (core module $M ~
  (import \"env\" \"mem\" (memory 1)) ~
  (import \"env\" \"create\" (func $create (param i32 i32))) ~
  (import \"env\" \"connect\" (func $connect (param i32 i32) (result i32))) ~
  (import \"env\" \"send\" (func $send (param i32 i32) (result i32))) ~
  (import \"env\" \"receive\" (func $receive (param i32 i32))) ~
  (import \"env\" \"snew\" (func $snew (result i64))) ~
  (import \"env\" \"swrite\" (func $swrite (param i32 i32 i32) (result i32))) ~
  (import \"env\" \"sread\" (func $sread (param i32 i32 i32) (result i32))) ~
  (import \"env\" \"sdropw\" (func $sdropw (param i32))) ~
  (import \"env\" \"subdrop\" (func $subdrop (param i32))) ~
  (import \"env\" \"join\" (func $join (param i32 i32))) ~
  (import \"env\" \"wsnew\" (func $wsnew (result i32))) ~
  (import \"env\" \"wait\" (func $wait (param i32 i32) (result i32))) ~
  (func $poll-subtask (param $packed i32) (param $ws i32) ~
    (local $sub i32) ~
    (if (i32.ne (i32.and (local.get $packed) (i32.const 15)) (i32.const 2)) ~
      (then ~
        (local.set $sub (i32.shr_u (local.get $packed) (i32.const 4))) ~
        (call $join (local.get $sub) (local.get $ws)) ~
        (block $done ~
          (loop $poll ~
            (drop (call $wait (local.get $ws) (i32.const 224))) ~
            (br_if $done (i32.eq (i32.load (i32.const 224)) (i32.const 1))) ~
            (br_if $done (i32.ge_u (i32.load (i32.const 228)) (i32.const 2))) ~
            (br $poll))) ~
        (if (i32.ne (i32.const 1) (i32.load (i32.const 224))) (then unreachable)) ~
        (call $subdrop (local.get $sub))))) ~
  (global $sock (mut i32) (i32.const 0)) ~
  (func (export \"f1\") (result i32) ~
    (local $ws i32) (local $packed i32) ~
    (local $rx i32) (local $tx i32) (local $pair i64) ~
    (local $r i32) ~
    (call $create (i32.const 0) (i32.const 64)) ~
    (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 64))) (then unreachable)) ~
    (global.set $sock (i32.load (i32.const 68))) ~
    (i32.store (i32.const 0) (global.get $sock)) ~
    (i32.store8 (i32.const 4) (i32.const 0)) ~
    (i32.store16 (i32.const 8) (i32.const ~d)) ~
    (i32.store8 (i32.const 10) (i32.const 127)) ~
    (i32.store8 (i32.const 11) (i32.const 0)) ~
    (i32.store8 (i32.const 12) (i32.const 0)) ~
    (i32.store8 (i32.const 13) (i32.const 1)) ~
    (local.set $ws (call $wsnew)) ~
    (local.set $packed (call $connect (i32.const 0) (i32.const 16))) ~
    (if (i32.ne (i32.const 2) (i32.and (local.get $packed) (i32.const 15))) (then unreachable)) ~
    (call $poll-subtask (local.get $packed) (local.get $ws)) ~
    (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 16))) (then unreachable)) ~
    (local.set $pair (call $snew)) ~
    (local.set $rx (i32.wrap_i64 (local.get $pair))) ~
    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $pair) (i64.const 32)))) ~
    (i32.store8 (i32.const 160) (i32.const 112)) ~
    (i32.store8 (i32.const 161) (i32.const 105)) ~
    (i32.store8 (i32.const 162) (i32.const 110)) ~
    (i32.store8 (i32.const 163) (i32.const 103)) ~
    (drop (call $send (global.get $sock) (local.get $rx))) ~
    (local.set $r (call $swrite (local.get $tx) (i32.const 160) (i32.const 4))) ~
    (if (i32.ne (i32.const 64) (local.get $r)) (then unreachable)) ~
    (call $sdropw (local.get $tx)) ~
    (i32.const 0)) ~
  (func (export \"f2\") (result i32) ~
    (local $rs i32) (local $r i32) ~
    (call $receive (global.get $sock) (i32.const 128)) ~
    (local.set $rs (i32.load (i32.const 128))) ~
    (local.set $r (call $sread (local.get $rs) (i32.const 176) (i32.const 4))) ~
    (if (i32.ne (i32.const 64) (local.get $r)) (then unreachable)) ~
    (i32.load8_u (i32.const 176)))) ~
 (core instance $m (instantiate $M (with \"env\" (instance ~
   (export \"mem\" (memory $memory \"mem\")) ~
   (export \"create\" (func $create-l)) ~
   (export \"connect\" (func $connect-l)) ~
   (export \"send\" (func $send-l)) ~
   (export \"receive\" (func $receive-l)) ~
   (export \"snew\" (func $snew)) ~
   (export \"swrite\" (func $swrite)) ~
   (export \"sread\" (func $sread)) ~
   (export \"sdropw\" (func $sdropw)) ~
   (export \"subdrop\" (func $subdrop)) ~
   (export \"join\" (func $join)) ~
   (export \"wsnew\" (func $wsnew)) ~
   (export \"wait\" (func $wait)))))) ~
 (func (export \"f1\") (result u32) (canon lift (core func $m \"f1\"))) ~
 (func (export \"f2\") (result u32) (canon lift (core func $m \"f2\"))))"
          port))

(defun sock-host-listener ()
  "A BLOCKING loopback TCP listener (the host side of the echo); the
caller closes FD."
  (let ((fd (wasmdown.wasi::%libc-socket
             wasmdown.wasi::+af-inet+ wasmdown.wasi::+sock-stream+
             wasmdown.wasi::+ipproto-tcp+)))
    (multiple-value-bind (alien len)
        (wasmdown.wasi::%make-sockaddr
         "ipv4" (list (cons "port" 0) (cons "address" '(127 0 0 1))))
      (unwind-protect
           (progn
             (when (minusp (wasmdown.wasi::%libc-bind
                            fd (sb-alien:alien-sap alien) len))
               (error "host listener bind failed"))
             (wasmdown.wasi::%libc-listen fd 4))
        (sb-alien:free-alien alien)))
    (values (sock-value-port
             (wasmdown.wasi::%getsockname-domain fd :local))
            fd)))

(defun sock-echo-thread (listen-fd got)
  "Accept one connection, remember its bytes in GOT, echo `pong'."
  (sb-thread:make-thread
   (lambda ()
     (let ((cfd (wasmdown.wasi::%libc-accept4
                 listen-fd (sb-sys:int-sap 0) (sb-sys:int-sap 0) 0)))
       (unwind-protect
            (let ((buf (sb-alien:make-alien sb-alien:unsigned-char 8)))
              (unwind-protect
                   (let ((n (wasmdown.wasi::%libc-recv
                             cfd (sb-alien:alien-sap buf) 8 0)))
                     (setf (car got)
                           (loop for i below n
                                 collect (sb-sys:sap-ref-8
                                          (sb-alien:alien-sap buf) i)))
                     (let ((msg (make-array
                                 4 :element-type '(unsigned-byte 8)
                                   :initial-contents '(112 111 110 103))))
                       (wasmdown.wasi::%libc-send
                        cfd (sb-sys:vector-sap msg) 4 0)))
                (sb-alien:free-alien buf)))
         (wasmdown.wasi::%close-fd cfd))))
   :name "sock-echo"))

(test wasi/e2e-tcp-echo-component
  (multiple-value-bind (port listen-fd) (sock-host-listener)
    (let* ((got (list nil))
           (thread (sock-echo-thread listen-fd got)))
      (unwind-protect
           (let* ((net (sock-network :dns '("localhost")))
                  (context (wasmdown.wasi:make-wasi-context :networks (list net)))
                  (comp (wasmdown.component.text::parse-component-text
                         (tcp-echo-fixture port))))
             (wasmdown.component:validate-component comp)
             (let* ((inst (wasmdown.component.runtime:instantiate-component
                           comp
                           :with-args (wasmdown.wasi:sockets-provider context)))
                    (f1 (wasmdown.component.runtime:runtime-export-value
                         inst "f1"))
                    (f2 (wasmdown.component.runtime:runtime-export-value
                         inst "f2")))
               ;; PHASE 1: the guest connects, sends "ping" and drops its
               ;; write end. The provider's sink must not block, so the
               ;; guest's READ is a second call: the test lets the host
               ;; echo land first, then the nonblocking recv finds it.
               (is (eql 0 (wasmdown.component.runtime:lifted-func-call f1)))
               (sb-thread:join-thread thread)
               ;; the host received exactly the guest's "ping"
               (is (equal '(112 105 110 103) (car got)))
               ;; PHASE 2: the guest reads "pong" and returns its first byte
               (is (eql 112 (wasmdown.component.runtime:lifted-func-call f2)))))
        (ignore-errors (sb-thread:join-thread thread :timeout 2))
        (wasmdown.wasi::%close-fd listen-fd)))))

;;; ===============================================================
;;; M16 SLICE 5 — wasi:http: the types resources (fields semantics,
;;; request/response/request-options), client.send over HTTP/1.1 on a
;;; loopback server, the pluggable transport, and an e2e component.
;;; ===============================================================

(defun http-net (&key (outbound :any) (dns :none))
  (wasmdown.wasi:make-wasi-network :outbound outbound :dns dns))

(defun http-ctx (&key (net (http-net)))
  (wasmdown.wasi:make-wasi-context :networks (list net)))

(defun http-fields (&rest entries)
  (let ((f (wasmdown.wasi:make-wasi-fields)))
    (loop for (name value) on entries by #'cddr
          do (wasmdown.wasi:fields-append f name value))
    f))

(defun http-request (&key (port 80) (path "/x") (method (cons "get" nil))
                          headers (options nil) (scheme :none))
  "A request rep built directly (the CL side of request.new)."
  (let* ((f (or headers (wasmdown.wasi:make-wasi-fields)))
         (req (third (first (wasmdown.wasi:http-request-new
                             f :none :none :none)))))
    (setf (wasmdown.wasi::wasi-http-request-authority req)
          (cons :some (format nil "127.0.0.1:~d" port))
          (wasmdown.wasi::wasi-http-request-path-with-query req)
          (cons :some path)
          (wasmdown.wasi::wasi-http-request-method req) method
          (wasmdown.wasi::wasi-http-request-scheme req) scheme
          (wasmdown.wasi::wasi-http-request-options req) options)
    req))

(defun http-serve-once (response)
  "A block loopback server emitting RESPONSE after reading the
request head: (values port listen-fd thread got-cell)."
  (multiple-value-bind (port fd) (sock-host-listener)
    (let ((got (list nil)))
      (values port fd
              (sb-thread:make-thread
               (lambda ()
                 (let ((cfd (wasmdown.wasi::%libc-accept4
                             fd (sb-sys:int-sap 0) (sb-sys:int-sap 0) 0)))
                   (unwind-protect
                        (let ((buf (make-array 0 :element-type '(unsigned-byte 8)
                                                 :adjustable t :fill-pointer 0))
                              (done nil))
                          (loop while (not done)
                                do (let ((tmp (sb-alien:make-alien
                                               sb-alien:unsigned-char 4096)))
                                     (unwind-protect
                                          (let ((n (wasmdown.wasi::%libc-recv
                                                    cfd (sb-alien:alien-sap tmp)
                                                    4096 0)))
                                            (loop for i below n
                                                  do (vector-push-extend
                                                      (sb-sys:sap-ref-8
                                                       (sb-alien:alien-sap tmp) i)
                                                      buf)))
                                       (sb-alien:free-alien tmp)))
                                   (let ((s (map 'string #'code-char buf)))
                                     (when (search (format nil "~c~c~c~c"
                                                           #\Return #\Newline
                                                           #\Return #\Newline)
                                                   s)
                                       (setf done t))))
                          (setf (car got) (map 'string #'code-char buf))
                          (let* ((bytes (map 'list #'char-code response))
                                 (abuf (wasmdown.wasi::%byte-list->alien bytes)))
                            (unwind-protect
                                 (wasmdown.wasi::%libc-send
                                  cfd (sb-alien:alien-sap abuf) (length bytes) 0)
                              (sb-alien:free-alien abuf))))
                     (wasmdown.wasi::%close-fd cfd))))
               :name "http-serve")
              got))))

(defmacro with-http-server ((port response got) &body body)
  `(multiple-value-bind (,port fd thread ,got)
       (http-serve-once ,response)
     (declare (ignorable thread))
     (unwind-protect (progn ,@body)
       (ignore-errors (sb-thread:join-thread thread :timeout 2))
       (wasmdown.wasi::%close-fd fd))))

(defun http-drain (reader)
  "Copy READER's values until no progress is left (the body source
yields every chunk the framing decodes, then EOF)."
  (let ((out '()))
    (loop
      (let* ((buf (wasmdown.component.runtime:make-host-buffer 4096))
             (vals (progn (rtcall "%END-COPY" reader buf t)
                          (wasmdown.component.runtime:host-buffer-values buf))))
        (when (null vals) (return out))
        (setf out (append out vals))))))

(defun http-body-text (resp)
  "consume-body + drain the stream through the copy protocol."
  (let* ((pair (wasmdown.wasi:http-response-consume-body resp :none))
         (reader (first pair))
         (bytes (http-drain reader))
         (future (sock-future (second pair))))
    (values (map 'string #'code-char bytes) future)))

;;; ---------------------------------------------------------------
;;; fields: case-insensitive names, ordered values, append vs set,
;;; the validation errors, copy-all/clone.

(test wasi/http-fields-semantics
  (let* ((f (wasmdown.wasi:make-wasi-fields)))
    (is (eq :ok (car (wasmdown.wasi:fields-set f "X-Test" (list (list 104 105))))))
    (is (eq :ok (car (wasmdown.wasi:fields-append f "x-test" (list 33)))))
    ;; case-insensitive lookup keeps the insertion order; the stored
    ;; spelling is CANONICAL (lowercase), as http::HeaderName does
    (is (equal '((104 105) (33)) (wasmdown.wasi:fields-get f "X-TEST")))
    (is (equal '(("x-test" (104 105)) ("x-test" (33)))
               (wasmdown.wasi:fields-copy-all f)))
    (is (wasmdown.wasi:fields-has f "x-Test"))
    (is (not (wasmdown.wasi:fields-has f "bad name")))
    (is (equal '() (wasmdown.wasi:fields-get f "absent")))
    ;; set replaces every value; the original position is kept
    (is (eq :ok (car (wasmdown.wasi:fields-set f "x-test" (list (list 49))))))
    (is (equal '((49)) (wasmdown.wasi:fields-get f "X-Test")))
    (is (equal '(("x-test" (49))) (wasmdown.wasi:fields-copy-all f)))
    ;; get-and-delete
    (let ((del (wasmdown.wasi:fields-get-and-delete f "X-Test")))
      (is (eq :ok (car del)))
      (is (equal '((49)) (cdr del)))
      (is (equal '() (cdr (wasmdown.wasi:fields-get-and-delete f "never")))))
    (is (not (wasmdown.wasi:fields-has f "x-test")))
    ;; clone is a DEEP, MUTABLE copy
    (let* ((g (http-fields "A" (list 49)))
           (c (third (wasmdown.wasi:fields-clone g))))
      (is (eq :ok (car (wasmdown.wasi:fields-set c "A" (list (list 50))))))
      (is (equal '((49)) (wasmdown.wasi:fields-get g "A")))
      (is (equal '((50)) (wasmdown.wasi:fields-get c "A"))))
    ;; validation: forbidden names, invalid syntax, immutability
    (is (equal "forbidden"
               (second (wasmdown.wasi:fields-set f "Host" (list (list 49))))))
    (is (equal "invalid-syntax"
               (second (wasmdown.wasi:fields-set f "X Y" (list (list 49))))))
    (is (equal "invalid-syntax"
               (second (wasmdown.wasi:fields-set f "X" (list (list 13 10 65))))))
    (is (equal "forbidden"
               (second (wasmdown.wasi:fields-append f "proxy-authorization"
                                                    (list 48)))))
    (let ((im (third (wasmdown.wasi::%fields-immutable-view f))))
      (is (equal "immutable"
                 (second (wasmdown.wasi:fields-set im "A" (list (list 49))))))
      (is (equal "immutable"
                 (second (wasmdown.wasi:fields-delete im "A")))))
    ;; from-list validates the whole list
    (is (equal "invalid-syntax"
               (second (wasmdown.wasi:fields-from-list
                        (list (list "A" (list 49))
                              (list "bad name" (list 49)))))))
    (is (equal "forbidden"
               (second (wasmdown.wasi:fields-from-list
                        (list (list "transfer-encoding" (list 49)))))))
    ;; the size limit is header-error.size-exceeded
    (is (equal "size-exceeded"
               (second (wasmdown.wasi:fields-set
                        f "X" (list (make-list 70000
                                               :initial-element 97))))))))

;;; ---------------------------------------------------------------
;;; request: method/scheme/authority/path round-trips + validation,
;;; the immutable header/options views.

(test wasi/http-request-surface
  (let ((req (http-request)))
    (is (equal (cons "get" nil) (wasmdown.wasi:http-request-get-method req)))
    (is (eq :ok (car (wasmdown.wasi:http-request-set-method
                      req (cons "other" "PURGE")))))
    (is (equal (cons "other" "PURGE")
               (wasmdown.wasi:http-request-get-method req)))
    (is (eq :err (car (wasmdown.wasi:http-request-set-method
                       req (cons "other" "BAD METHOD")))))
    (is (eq :ok (car (wasmdown.wasi:http-request-set-scheme
                      req (cons :some (cons "HTTPS" nil))))))
    (is (equal (cons :some (cons "HTTPS" nil))
               (wasmdown.wasi:http-request-get-scheme req)))
    (is (eq :err (car (wasmdown.wasi:http-request-set-scheme
                       req (cons :some (cons "other" "1bad"))))))
    (is (eq :ok (car (wasmdown.wasi:http-request-set-authority
                      req (cons :some "example.test:8080")))))
    (is (equal (cons :some "example.test:8080")
               (wasmdown.wasi:http-request-get-authority req)))
    (is (eq :err (car (wasmdown.wasi:http-request-set-authority
                       req (cons :some "exa mple")))))
    (is (eq :ok (car (wasmdown.wasi:http-request-set-path-with-query
                      req (cons :some "/a/b?c=d")))))
    (is (equal (cons :some "/a/b?c=d")
               (wasmdown.wasi:http-request-get-path-with-query req)))
    (is (eq :err (car (wasmdown.wasi:http-request-set-path-with-query
                       req (cons :some "no-slash")))))
    ;; the headers/options accessors hand out IMMUTABLE handles
    (let ((h (third (wasmdown.wasi:http-request-get-headers req))))
      (is (equal "immutable"
                 (second (wasmdown.wasi:fields-set h "A" (list (list 49)))))))
    (is (eq :none (wasmdown.wasi:http-request-get-options req)))
    (let ((opt (wasmdown.wasi:make-wasi-http-options)))
      (wasmdown.wasi:request-options-set-connect-timeout opt (cons :some 5))
      (setf (wasmdown.wasi::wasi-http-request-options req) opt)
      (let ((view (third (wasmdown.wasi:http-request-get-options req))))
        (is (equal "immutable"
                   (second (wasmdown.wasi:request-options-set-first-byte-timeout
                            view (cons :some 5)))))))))

(test wasi/http-request-options
  (let ((o (wasmdown.wasi:make-wasi-http-options)))
    (is (eq :none (wasmdown.wasi:request-options-get-connect-timeout o)))
    (is (eq :none (wasmdown.wasi:request-options-get-first-byte-timeout o)))
    (is (eq :none (wasmdown.wasi:request-options-get-between-bytes-timeout o)))
    (is (eq :ok (car (wasmdown.wasi:request-options-set-connect-timeout
                      o (cons :some 1000000)))))
    (is (equal (cons :some 1000000)
               (wasmdown.wasi:request-options-get-connect-timeout o)))
    ;; a ZERO duration is not-supported (the WIT's request-options-error)
    (is (equal '(:err "not-supported")
               (wasmdown.wasi:request-options-set-first-byte-timeout
                o (cons :some 0))))
    (is (equal '(:err "not-supported")
               (wasmdown.wasi:request-options-set-between-bytes-timeout o 0)))
    ;; a clone is mutable even when the source was made immutable
    (let ((im (third (wasmdown.wasi:request-options-clone o))))
      (setf (wasmdown.wasi::wasi-http-options-mutable-p im) nil)
      (is (equal "immutable"
                 (second (wasmdown.wasi:request-options-set-connect-timeout
                          im (cons :some 5)))))
      (let ((c (third (wasmdown.wasi:request-options-clone im))))
        (is (eq :ok (car (wasmdown.wasi:request-options-set-connect-timeout
                          c (cons :some 7)))))
        (is (equal (cons :some 7)
                   (wasmdown.wasi:request-options-get-connect-timeout c)))))))

(test wasi/http-response-surface
  (let ((r (wasmdown.wasi::make-wasi-http-response
            :headers (http-fields "A" (list 49)))))
    (is (= 200 (wasmdown.wasi:http-response-get-status-code r)))
    (is (eq :ok (car (wasmdown.wasi:http-response-set-status-code r 404))))
    (is (= 404 (wasmdown.wasi:http-response-get-status-code r)))
    (is (eq :err (car (wasmdown.wasi:http-response-set-status-code r 99))))
    (is (equal '(("a" (49)))
               (wasmdown.wasi:fields-copy-all
                (third (wasmdown.wasi:http-response-get-headers r)))))))

;;; ---------------------------------------------------------------
;;; The loopback round trips: content-length and chunked+trailers.

(test wasi/http-loopback-content-length
  (with-http-server
      (port
       (format nil "HTTP/1.1 200 OK~c~cContent-Length: 5~c~cX-Reply: yes~c~c~c~chello"
               #\Return #\Newline #\Return #\Newline #\Return #\Newline
               #\Return #\Newline)
       got)
    (let* ((net (http-net))
           (ctx (http-ctx :net net))
           (req (http-request :port port :path "/hi?x=1"
                              :headers (http-fields "x-test" (list 104))))
           (res (wasmdown.wasi:http-client-send ctx req :network net)))
      (is (eq :ok (car res)))
      (let* ((resp (fourth res))
             (head (car got)))
        (is (= 200 (wasmdown.wasi:http-response-get-status-code resp)))
        (is (equal '((121 101 115))
                   (wasmdown.wasi:fields-get
                    (wasmdown.wasi::wasi-http-response-headers resp) "x-reply")))
        (multiple-value-bind (body future) (http-body-text resp)
          (is (string= "hello" body))
          (is (equal (cons :ok :none) future)))
        (is (search "GET /hi?x=1 HTTP/1.1" head))
        (is (search "x-test: h" head))
        (is (search (format nil "Host: 127.0.0.1:~d" port) head))))))

(test wasi/http-loopback-chunked-trailers
  (with-http-server
      (port
       (format nil "HTTP/1.1 200 OK~c~cTransfer-Encoding: chunked~c~c~c~c2~c~cab~c~c2~c~ccd~c~c0~c~cX-Trail: t~c~c~c~c"
               #\Return #\Newline #\Return #\Newline #\Return #\Newline
               #\Return #\Newline #\Return #\Newline #\Return #\Newline
               #\Return #\Newline #\Return #\Newline #\Return #\Newline
               #\Return #\Newline #\Return #\Newline)
       got)
    (let* ((net (http-net))
           (ctx (http-ctx :net net))
           (req (http-request :port port))
           (res (wasmdown.wasi:http-client-send ctx req :network net)))
      (is (eq :ok (car res)))
      (let ((resp (fourth res)))
        (is (= 200 (wasmdown.wasi:http-response-get-status-code resp)))
        (multiple-value-bind (body future) (http-body-text resp)
          (is (string= "abcd" body))
          ;; the trailers arrive only with the body's completion
          (is (eq :ok (car future)))
          (let ((opt (cdr future)))
            (is (consp opt))
            (is (eq :some (car opt)))
            (let ((trailers (third (cdr opt))))
              (is (equal '((116))
                         (wasmdown.wasi:fields-get trailers "x-trail"))))))))))

(test wasi/http-chunked-empty-trailer-section-is-none
  ;; HTTP's empty trailer section (0\r\n\r\n) is the WIT's `none', not
  ;; an empty Some(fields) — the upstream http-client-chunked contract.
  (with-http-server
      (port
       (format nil "HTTP/1.1 200 OK~c~cTransfer-Encoding: chunked~c~c~c~c1~c~cx~c~c0~c~c~c~c"
               #\Return #\Newline #\Return #\Newline #\Return #\Newline
               #\Return #\Newline #\Return #\Newline #\Return #\Newline
               #\Return #\Newline #\Return #\Newline)
       got)
    (let* ((net (http-net))
           (ctx (http-ctx :net net))
           (req (http-request :port port))
           (res (wasmdown.wasi:http-client-send ctx req :network net)))
      (is (eq :ok (car res)))
      (multiple-value-bind (body future) (http-body-text (fourth res))
        (is (string= "x" body))
        (is (equal (cons :ok :none) future))))))

(test wasi/http-send-future-reports-failed-transmission
  ;; A send that never reaches the wire must settle request.new's
  ;; transmission future with an error (upstream http-client-sent).
  (let* ((net (http-net :outbound
                        (list (wasmdown.wasi:make-wasi-endpoint
                               :address '(10 9 9 9)))))
         (ctx (http-ctx :net net))
         (pair (wasmdown.wasi:http-request-new
                (wasmdown.wasi:make-wasi-fields) :none :none :none))
         (req (third (first pair)))
         (future (second pair)))
    (setf (wasmdown.wasi::wasi-http-request-authority req)
          (cons :some "127.0.0.1:80")
          (wasmdown.wasi::wasi-http-request-scheme req)
          (cons :some (cons "HTTP" nil)))
    (is (eq :err (car (wasmdown.wasi:http-client-send
                       ctx req :network net))))
    (let ((value (sock-future future)))
      (is (consp value))
      (is (eq :err (car value))))))

;;; ---------------------------------------------------------------
;;; The refusals: https-without-transport, the outbound/DNS policy,
;;; and header injection.

(test wasi/http-transport-refusals
  ;; https without a registered TLS transport is configuration-error
  (let* ((net (http-net))
         (ctx (http-ctx :net net))
         (req (http-request :port 443
                            :scheme (cons :some (cons "HTTPS" nil)))))
    (is (equal '("configuration-error")
               (cdr (wasmdown.wasi:http-client-send ctx req :network net)))))
  ;; an undeclared destination is destination-IP-prohibited
  (let* ((net (http-net :outbound (list (wasmdown.wasi:make-wasi-endpoint
                                          :address '(10 9 9 9)))))
         (ctx (http-ctx :net net))
         (req (http-request :port 80)))
    (is (equal '("destination-IP-prohibited")
               (cdr (wasmdown.wasi:http-client-send ctx req :network net)))))
  ;; a DNS-denied host is HTTP-request-denied (no outbound traffic)
  (let* ((net (http-net))
         (ctx (http-ctx :net net))
         (req (third (first (wasmdown.wasi:http-request-new
                             (wasmdown.wasi:make-wasi-fields)
                             :none :none :none)))))
    (setf (wasmdown.wasi::wasi-http-request-authority req)
          (cons :some "example.test:80"))
    (is (equal '("HTTP-request-denied")
               (cdr (wasmdown.wasi:http-client-send ctx req :network net)))))
  ;; a fields value carrying CRLF can never be serialized (defence in
  ;; depth behind the fields validation)
  (let ((f (wasmdown.wasi:make-wasi-fields)))
    (setf (wasmdown.wasi::wasi-fields-entries f)
          (list (cons "X" (list 49 13 10 88))))
    (is (handler-case
            (progn (wasmdown.wasi::%http-serialize-request-head
                    "GET" "/" "h" f '())
                   nil)
          (wasmdown.wasi:wasi-http-error (c)
            (wasmdown.wasi:http-error-code
             (wasmdown.wasi:wasi-http-error-code c)))))))

;;; ---------------------------------------------------------------
;;; THE PLUGGABLE TRANSPORT: an embedder-supplied in-memory transport
;;; serves the request without any socket.

(test wasi/http-pluggable-transport
  (let* ((response
           (map 'list #'char-code
                 (format nil "HTTP/1.1 204 No Content~c~cX-Fake: 1~c~c~c~c"
                         #\Return #\Newline #\Return #\Newline
                         #\Return #\Newline)))
         (written (list nil))
         (read-state (list response 0))
         (transport
           (wasmdown.wasi:make-wasi-http-transport
            :scheme "https" :tls-p t
            :connect
            (lambda (host port destination timeout)
              (declare (ignore host port destination timeout))
              (wasmdown.wasi:make-wasi-http-connection
               :read-fn (lambda (max timeout)
                          (declare (ignore timeout))
                          (let* ((bytes (first read-state))
                                 (pos (second read-state))
                                 (n (min max (- (length bytes) pos))))
                            (if (zerop n)
                                (values nil :eof)
                                (let ((chunk (subseq bytes pos (+ pos n))))
                                  (incf (second read-state) n)
                                  (values chunk :ok)))))
               :write-fn (lambda (bytes)
                           (setf (car written)
                                 (append (car written) bytes))
                           nil)
               :close-fn (lambda () t)))))
         (net (http-net))
         (ctx (wasmdown.wasi:make-wasi-context
                 :networks (list net)
                 :http-transports (list transport)))
         (req (http-request :port 443
                            :scheme (cons :some (cons "HTTPS" nil)))))
    (let ((res (wasmdown.wasi:http-client-send ctx req :network net)))
      (is (eq :ok (car res)))
      (let ((resp (fourth res)))
        (is (= 204 (wasmdown.wasi:http-response-get-status-code resp)))
        (is (equal '((49))
                   (wasmdown.wasi:fields-get
                    (wasmdown.wasi::wasi-http-response-headers resp) "x-fake")))
        (multiple-value-bind (body future) (http-body-text resp)
          (is (string= "" body))
          (is (equal (cons :ok :none) future))))
      ;; the bytes went through the EMBEDDER's write hook
      (is (search "GET /x HTTP/1.1"
                  (map 'string #'code-char (car written)))))))

;;; ---------------------------------------------------------------
;;; The provider surface.

(test wasi/http-provider-surface
  (let* ((ctx (http-ctx))
         (inst (wasmdown.wasi:http-types-instance ctx))
         (prov (wasmdown.wasi:http-provider ctx)))
    (dolist (name '("fields" "request" "request-options" "response"))
      (is (eq :type (car (gethash name
                                  (wasmdown.component.runtime:comp-instance-exports
                                   inst))))))
    (dolist (name '("[constructor]fields" "[static]fields.from-list"
                    "[method]fields.get" "[method]fields.set"
                    "[method]fields.delete" "[method]fields.get-and-delete"
                    "[method]fields.append" "[method]fields.copy-all"
                    "[method]fields.clone"
                    "[static]request.new" "[method]request.get-method"
                    "[method]request.set-method"
                    "[method]request.set-authority"
                    "[method]request.get-headers"
                    "[method]request.get-options"
                    "[static]request.consume-body"
                    "[constructor]request-options"
                    "[method]request-options.set-connect-timeout"
                    "[method]request-options.clone"
                    "[method]response.get-status-code"
                    "[method]response.set-status-code"
                    "[static]response.consume-body"))
      (is (wasmdown.wasi:provider-instance-function inst name)))
    (is (= 2 (length prov)))
    (is (assoc "wasi:http/types@0.3.2" prov :test #'string=))
    (is (assoc "wasi:http/client@0.3.2" prov :test #'string=))
    ;; the constructor is callable through the lifted-func boundary
    (let ((v (wasmdown.component.runtime:lifted-func-call
              (wasmdown.wasi:provider-instance-function
               inst "[constructor]fields"))))
      (is (eq :own-handle (car v)))
      (is (wasmdown.wasi:wasi-fields-p (third v))))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END COMPONENT is DEFERRED.
;;;
;;; tests/http-client-component.wat is restored (the M16 slice-5
;;; fixture: it imports wasi:http/types + client, builds a GET, sends
;;; it, reads the status and the first body byte), but the component
;;; leg does not run on the current tree. M17 step 5 added the
;;; canonical `end.t == t' guard to %LIFT-ASYNC-END: the element type
;;; recorded on a GUEST-authored end (`future.new' of a future whose
;;; element names guest-defined types) is keyed in the guest's type
;;; space, while the provider's lifted functype is keyed in the
;;; provider's space; a composite element (the request trailer future
;;; is `result<option<trailers>, error-code>') therefore cannot be
;;; compared across the two spaces yet, and the fixture traps with
;;; "stream or future element type mismatch" before request.new runs.
;;; The host-side surface above (the fields/request/response
;;; resources, client.send over the loopback, the pluggable transport,
;;; the provider instances) is covered without a guest. Re-enabling
;;; this leg belongs with the incoming service/handler wiring, where
;;; the cross-space key question is decided.

;;; ===============================================================
;;; M16 SLICE 6 — THE COMMAND WORLD, THE TERMINALS, THE RUNNER.
;;; ===============================================================

;;; ---- the terminals: some/none by isatty(3) ----

(defun %pty-stream ()
  "A CL stream over a fresh pty master (/dev/ptmx) — isatty(3) true.
Returns (values stream fd); the caller closes FD."
  (multiple-value-bind (fd errno)
      (sb-unix:unix-open "/dev/ptmx" (logior 2 0) 0)
    (unless (and (integerp fd) (not (minusp fd)))
      (error "cannot open /dev/ptmx (errno ~a)" errno))
    (values (sb-sys:make-fd-stream fd :input t :output t
                                        :element-type '(unsigned-byte 8))
            fd)))

(test wasi/terminals-none-for-non-tty
  ;; a string stream has no host fd: no terminal capability exists,
  ;; never a fabricated handle (docs/design/PLAN.md §9.1)
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdin (make-string-input-stream "")
               :stdout (make-string-output-stream)
               :stderr (make-string-output-stream)))
         (in (wasmdown.wasi:terminal-stdin-instance ctx))
         (out (wasmdown.wasi:terminal-stdout-instance ctx))
         (err (wasmdown.wasi:terminal-stderr-instance ctx)))
    (is (eq :none (call-provider in "get-terminal-stdin")))
    (is (eq :none (call-provider out "get-terminal-stdout")))
    (is (eq :none (call-provider err "get-terminal-stderr")))
    ;; the full cli provider binds all five terminal imports
    (let ((names (mapcar #'car (wasmdown.wasi:cli-provider ctx))))
      (dolist (n '("wasi:cli/terminal-input@0.3.2"
                   "wasi:cli/terminal-output@0.3.2"
                   "wasi:cli/terminal-stdin@0.3.2"
                   "wasi:cli/terminal-stdout@0.3.2"
                   "wasi:cli/terminal-stderr@0.3.2"))
        (is (member n names :test #'string=))))))

(test wasi/terminals-some-for-tty
  (multiple-value-bind (stream fd) (%pty-stream)
    (unwind-protect
         (let* ((ctx (wasmdown.wasi:make-wasi-context :stdout stream))
                (out (wasmdown.wasi:terminal-stdout-instance ctx))
                (r (call-provider out "get-terminal-stdout")))
           (is (consp r))
           (is (eq (car r) :some))
           ;; the some payload is an own handle whose rep is the
           ;; wasi-terminal (fd + isatty) — a BORROWED stream fd
           (let* ((handle (cdr r))
                  (rep (third handle)))
             (is (eq (first handle) :own-handle))
             (is (wasmdown.wasi:wasi-terminal-p rep))
             (is (= fd (wasmdown.wasi:wasi-terminal-fd rep)))))
      (sb-unix:unix-close fd))))

;;; ---- the runner: ok / err / exit-with-code over an async run ----

(defparameter *command-ok-fixture*
  (concatenate 'string
   "(component"
   " (type $ERR (enum \"io\" \"illegal-byte-sequence\" \"pipe\"))"
   " (type $ST (stream u8))"
   " (import \"wasi:cli/environment@0.3.2\""
   "   (instance $env"
   "     (export \"get-arguments\" (func (result (list string))))))"
   " (import \"wasi:cli/stdout@0.3.2\""
   "   (instance $out"
   "     (export \"error-code\" (type (eq $ERR)))"
   "     (type $r (result (error $ERR)))"
   "     (type $f (future $r))"
   "     (type $s (stream u8))"
   "     (export \"write-via-stream\""
   "       (func (param \"data\" $s) (result $f)))))"
   " (alias export $env \"get-arguments\" (func $ga))"
   " (alias export $out \"write-via-stream\" (func $wvs))"
   " (core module $Memory (memory (export \"mem\") 1))"
   " (core instance $memory (instantiate $Memory))"
   " (core module $Realloc"
   "   (global $bump (mut i32) (i32.const 1024))"
   "   (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "     (local $p i32)"
   "     (local.set $p (i32.and (i32.add (global.get $bump) (i32.const 7)) (i32.const -8)))"
   "     (global.set $bump (i32.add (local.get $p) (local.get 3)))"
   "     (local.get $p)))"
   " (core instance $r (instantiate $Realloc))"
   " (alias core export $r \"realloc\" (core func $realloc))"
   " (canon lower (func $ga) (memory (core memory $memory \"mem\")) (realloc (core func $realloc)) (core func $ga-lowered))"
   " (core func $wvs-lowered (canon lower (func $wvs)))"
   " (core module $M"
   "  (import \"env\" \"mem\" (memory 1))"
   "  (import \"env\" \"task.return\" (func $tr (param i32)))"
   "  (import \"env\" \"get-arguments\" (func $args (param i32)))"
   "  (import \"env\" \"stream.new\" (func $snew (result i64)))"
   "  (import \"env\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))"
   "  (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))"
   "  (import \"env\" \"future.read\" (func $fread (param i32 i32) (result i32)))"
   "  (import \"env\" \"write-via-stream\" (func $wvs (param i32) (result i32)))"
   "  (global $fut (mut i32) (i32.const 0))"
   "  (func (export \"run\") (result i32)"
   "    (local $p i64) (local $rx i32) (local $tx i32) (local $ret i32)"
   "    (local $list i32) (local $sp i32) (local $sl i32)"
   "    (call $args (i32.const 16))"
   "    (local.set $list (i32.load (i32.const 16)))"
   "    (local.set $sp (i32.load (local.get $list)))"
   "    (local.set $sl (i32.load (i32.add (local.get $list) (i32.const 4))))"
   "    (local.set $p (call $snew))"
   "    (local.set $rx (i32.wrap_i64 (local.get $p)))"
   "    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $p) (i64.const 32))))"
   "    (global.set $fut (call $wvs (local.get $rx)))"
   "    (local.set $ret (call $swrite (local.get $tx) (local.get $sp) (local.get $sl)))"
   "    (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))"
   "    (call $sdropw (local.get $tx))"
   "    (local.set $ret (call $fread (global.get $fut) (i32.const 64)))"
   "    (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))"
   "    (call $tr (i32.const 0))"
   "    (i32.const 0))"
   "  (func (export \"run-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"
   " (type $FT (future (result (error $ERR))))"
   " (canon task.return (result (result)) (core func $tr))"
   " (canon stream.new $ST (core func $snew))"
   " (canon stream.write $ST (memory (core memory $memory \"mem\")) (core func $swrite))"
   " (canon stream.drop-writable $ST (core func $sdropw))"
   " (canon future.read $FT (memory (core memory $memory \"mem\")) (core func $fread))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $memory \"mem\"))"
   "   (export \"task.return\" (func $tr))"
   "   (export \"get-arguments\" (func $ga-lowered))"
   "   (export \"stream.new\" (func $snew))"
   "   (export \"stream.write\" (func $swrite))"
   "   (export \"stream.drop-writable\" (func $sdropw))"
   "   (export \"future.read\" (func $fread))"
   "   (export \"write-via-stream\" (func $wvs-lowered))))))"
   " (func $run (export \"run\") async (result (result)) (canon lift (core func $m \"run\") async (callback (core func $m \"run-cb\"))))"
   " (instance $runiface (export \"run\" (func $run)))"
   " (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
   "   (instance (export \"run\" (func async (result (result)))))))"))

(defparameter *command-err-fixture*
  (concatenate 'string
   "(component"
   " (core module $M"
   "  (import \"env\" \"task.return\" (func $tr (param i32)))"
   "  (func (export \"run\") (result i32) (call $tr (i32.const 1)) (i32.const 0))"
   "  (func (export \"run-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"
   " (canon task.return (result (result)) (core func $tr))"
   " (core instance $m (instantiate $M (with \"env\" (instance (export \"task.return\" (func $tr))))))"
   " (func $run (export \"run\") async (result (result)) (canon lift (core func $m \"run\") async (callback (core func $m \"run-cb\"))))"
   " (instance $runiface (export \"run\" (func $run)))"
   " (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
   "   (instance (export \"run\" (func async (result (result)))))))"))

(defparameter *command-exit-fixture*
  (concatenate 'string
   "(component"
   " (import \"wasi:cli/exit@0.3.2\""
   "   (instance $ex (export \"exit-with-code\" (func (param \"code\" u8)))))"
   " (alias export $ex \"exit-with-code\" (func $ewc))"
   " (core func $ewc-lowered (canon lower (func $ewc)))"
   " (core module $M"
   "  (import \"env\" \"task.return\" (func $tr (param i32)))"
   "  (import \"env\" \"exit-with-code\" (func $ewc (param i32)))"
   "  (func (export \"run\") (result i32)"
   "    (call $ewc (i32.const 7))"
   "    (call $tr (i32.const 0))"
   "    (i32.const 0))"
   "  (func (export \"run-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"
   " (canon task.return (result (result)) (core func $tr))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"task.return\" (func $tr))"
   "   (export \"exit-with-code\" (func $ewc-lowered))))))"
   " (func $run (export \"run\") async (result (result)) (canon lift (core func $m \"run\") async (callback (core func $m \"run-cb\"))))"
   " (instance $runiface (export \"run\" (func $run)))"
   " (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
   "   (instance (export \"run\" (func async (result (result)))))))"))

(defun %command-component (text)
  (let ((comp (wasmdown.component.text::parse-component-text text)))
    (wasmdown.component:validate-component comp)
    comp))

(test wasi/command-runner-ok-arguments-stdout
  ;; the exported async `run' is driven to completion: get-arguments
  ;; reaches the guest (argv[0] printed through stdout.write-via-stream)
  ;; and the `result' ok maps to status 0
  (let* ((sink (make-string-output-stream))
         (context (wasmdown.wasi:make-wasi-context
                   :arguments '("prog" "-v")
                   :stdout sink))
         (status nil) (store nil))
    (multiple-value-setq (status store)
      (wasmdown.wasi:run-component
       (%command-component *command-ok-fixture*) context))
    (is (eql 0 status))
    (is (string= "prog" (get-output-stream-string sink)))
    ;; the exit boundary tore the RUN'S OWN store down: no registered
    ;; guest threads in it
    (is (zerop (length
                (wasmdown.component.runtime:component-store-threads store))))))

(test wasi/command-runner-err-and-exit
  ;; run returning err -> 1; exit-with-code(7) -> 7 with a clean store
  (let (store)
    (is (eql 1 (multiple-value-bind (st s)
                   (wasmdown.wasi:run-component
                    (%command-component *command-err-fixture*)
                    (wasmdown.wasi:make-wasi-context))
                 (setf store s)
                 st)))
    (is (zerop (length
                (wasmdown.component.runtime:component-store-threads store)))))
  (let (store)
    (is (eql 7 (multiple-value-bind (st s)
                   (wasmdown.wasi:run-component
                    (%command-component *command-exit-fixture*)
                    (wasmdown.wasi:make-wasi-context))
                 (setf store s)
                 st)))
    (is (zerop (length
                (wasmdown.component.runtime:component-store-threads store))))))

;;; ---- the CLI: a component binary file + the WASI flags ----

(defun %u32b (n)
  (let ((out '()))
    (loop while (>= n 128)
          do (push (logior 128 (logand n 127)) out) (setf n (ash n -7))
          finally (push n out) (return (coerce (nreverse out)
                                               '(vector (unsigned-byte 8)))))))

(defun %bs (&rest xs) (coerce xs '(vector (unsigned-byte 8))))
(defun %bcat (&rest vs) (apply #'concatenate '(vector (unsigned-byte 8)) vs))
(defun %bname (s) (%bcat (%u32b (length s)) (map 'vector #'char-code s)))
(defun %bna (s) (%bcat (%bs 0) (%bname s)))
(defun %bvec (&rest xs) (%bcat (%u32b (length xs)) (apply #'%bcat xs)))
(defun %bsec (id content) (%bcat (%bs id) (%u32b (length content)) content))
(defun %bwat (text)
  (wasmdown.binary:encode-module (wasmdown.text:parse-wat text)))

(defun command-component-binary ()
  "A hand-assembled command-world component: it imports
wasi:cli/stdout@0.3.2, writes \"hi\" through a stream, task.returns
ok, and exports `run' (the runner's direct-export fallback)."
  (let* ((memmod (%bwat "(module (memory (export \"mem\") 1))"))
         (mmod (%bwat
                "(module
                  (import \"env\" \"mem\" (memory 1))
                  (import \"env\" \"task.return\" (func $tr (param i32)))
                  (import \"env\" \"stream.new\" (func $snew (result i64)))
                  (import \"env\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))
                  (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))
                  (import \"env\" \"future.read\" (func $fread (param i32 i32) (result i32)))
                  (import \"env\" \"write-via-stream\" (func $wvs (param i32) (result i32)))
                  (global $fut (mut i32) (i32.const 0))
                  (func (export \"run\") (result i32)
                    (local $p i64) (local $rx i32) (local $tx i32) (local $ret i32)
                    (local.set $p (call $snew))
                    (local.set $rx (i32.wrap_i64 (local.get $p)))
                    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $p) (i64.const 32))))
                    (i32.store16 (i32.const 16) (i32.const 0x6968))
                    (global.set $fut (call $wvs (local.get $rx)))
                    (local.set $ret (call $swrite (local.get $tx) (i32.const 16) (i32.const 2)))
                    (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))
                    (call $sdropw (local.get $tx))
                    (local.set $ret (call $fread (global.get $fut) (i32.const 32)))
                    (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))
                    (call $tr (i32.const 0))
                    (i32.const 0))
                  (func (export \"run-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"))
         (types (%bvec
                 (%bcat (%bs #x6d 3) (%bname "io")
                        (%bname "illegal-byte-sequence") (%bname "pipe"))
                 (%bs #x66 1 #x7d)
                 (%bs #x6a 0 1 0)
                 (%bs #x65 1 2)
                 (%bcat (%bs #x40 1) (%bname "data") (%bs 1) (%bs 0 3))
                 (%bcat (%bs #x42 7)
                        (%bs #x02 #x03 #x02 #x01 #x00)
                        (%bcat (%bs #x04) (%bna "error-code") (%bs #x03 #x00 #x00))
                        (%bs #x01 #x6a #x00 #x01 #x00)
                        (%bs #x01 #x65 #x01 #x02)
                        (%bs #x01 #x66 #x01 #x7d)
                        (%bcat (%bs #x01 #x40 #x01) (%bname "data") (%bs #x04 #x00 #x03))
                        (%bcat (%bs #x04) (%bna "write-via-stream") (%bs #x01 #x05)))
                 (%bs #x6a 0 0)
                 (%bcat (%bs #x43 0) (%bs 0 6))))
         (import (%bvec (%bcat (%bna "wasi:cli/stdout@0.3.2") (%bs #x05 5))))
         (env-bag (%bcat (%bs #x01 7)
                         (%bcat (%bname "mem") (%bs #x02 0))
                         (%bcat (%bname "task.return") (%bs #x00 0))
                         (%bcat (%bname "stream.new") (%bs #x00 1))
                         (%bcat (%bname "stream.write") (%bs #x00 2))
                         (%bcat (%bname "stream.drop-writable") (%bs #x00 3))
                         (%bcat (%bname "future.read") (%bs #x00 4))
                         (%bcat (%bname "write-via-stream") (%bs #x00 5)))))
    (%bcat (%bs 0 #x61 #x73 #x6d #x0d 0 1 0)
           (%bsec 7 types)
           (%bsec 10 import)
           (%bsec 1 memmod)
           (%bsec 1 mmod)
           (%bsec 2 (%bvec (%bs #x00 0 0)))
           (%bsec 6 (%bvec (%bcat (%bs #x00 #x02 #x01 0) (%bname "mem"))))
           (%bsec 6 (%bvec (%bcat (%bs #x01 #x00 0) (%bname "write-via-stream"))))
           (%bsec 8 (%bvec (%bs #x09 #x00 6 #x00)   ; task.return
                           (%bs #x0e 1)              ; stream.new
                           (%bs #x10 1 #x01 #x03 0)  ; stream.write (memory 0)
                           (%bs #x14 1)              ; stream.drop-writable
                           (%bs #x16 3 #x01 #x03 0)  ; future.read (memory 0)
                           (%bs #x01 #x00 0 #x00)))  ; lower write-via-stream
           (%bsec 2 (%bvec env-bag (%bcat (%bs #x00 1 1) (%bname "env") (%bs #x12 1))))
           (%bsec 6 (%bvec (%bcat (%bs #x00 #x00 #x01 2) (%bname "run"))
                           (%bcat (%bs #x00 #x00 #x01 2) (%bname "run-cb"))))
           (%bsec 8 (%bvec (%bs #x00 #x00 6 #x02 #x06 #x07 7 7)))
           (%bsec 11 (%bvec (%bcat (%bna "run") (%bs #x01 1) (%bs 0)))))))

(defun %write-temp-bytes (name bytes)
  (let ((path (merge-pathnames name *default-pathname-defaults*)))
    (with-open-file (s path :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :supersede)
      (write-sequence bytes s))
    path))

(test wasi/e2e-command-component-binary
  ;; the same component through the runner and through the decoded IR
  (let* ((bytes (command-component-binary))
         (comp (wasmdown.component:decode-component bytes))
         (sink (make-string-output-stream)))
    (is (wasmdown.component:component-p bytes))
    (wasmdown.component:validate-component comp)
    (is (eql 0 (wasmdown.wasi:run-component
                comp (wasmdown.wasi:make-wasi-context :stdout sink))))
    (is (string= "hi" (get-output-stream-string sink)))))

(test wasi/cli-run-component-wasi-flags
  (let ((path (%write-temp-bytes "tmp-cli-command.wasm"
                                 (command-component-binary))))
    (unwind-protect
         (let ((out (with-output-to-string (s)
                      (let ((*standard-output* s))
                        (is (eql 0 (wasmdown.cli:main
                                    (list "run" (namestring path)
                                          "--wasi" "--env" "HOME=/tmp"
                                          "guest-arg"))))))))
           ;; the guest's stdout.write-via-stream reached the CLI's
           ;; standard output; the guest arguments were plumbed
           (is (string= "hi" out)))
      (when (probe-file path) (delete-file path))))
  ;; network options are accepted on the component path; a malformed
  ;; endpoint is a host error (2), and a guest argument without --wasi
  ;; is likewise a usage error
  (let ((path (%write-temp-bytes "tmp-cli-command2.wasm"
                                 (command-component-binary))))
    (unwind-protect
         (progn
           (let ((out (with-output-to-string (s)
                        (let ((*standard-output* s))
                          (is (eql 0 (wasmdown.cli:main
                                      (list "run" (namestring path) "--wasi"
                                            "--net" "127.0.0.1:1"
                                            "--listen" "*:*" "--dns" "*"))))))))
             (is (string= "hi" out)))
           (is (eql 2 (wasmdown.cli:main
                       (list "run" (namestring path) "--wasi"
                             "--net" "nonsense"))))
           (is (eql 2 (wasmdown.cli:main
                       (list "run" (namestring path) "orphan-arg")))))
      (when (probe-file path) (delete-file path)))))

;;; ---- the CLI-side authority helpers ----

(test wasi/endpoint-parsing
  (let ((ep (wasmdown.wasi:parse-wasi-endpoint "127.0.0.1:8080")))
    (is (equal '(127 0 0 1) (wasmdown.wasi:wasi-endpoint-address ep)))
    (is (eql 8080 (wasmdown.wasi:wasi-endpoint-port ep))))
  (let ((ep (wasmdown.wasi:parse-wasi-endpoint "*:*")))
    (is (eq :any (wasmdown.wasi:wasi-endpoint-address ep)))
    (is (eq :any (wasmdown.wasi:wasi-endpoint-port ep))))
  (is (block nil
        (handler-case (progn (wasmdown.wasi:parse-wasi-endpoint "nonsense") nil)
          (wasmdown:host-error () t)))))

;;; ---------------------------------------------------------------
;;; The reactor backend (plan step 6): poll(2) + the monotonic clock +
;;; the self-pipe wake channel. Every test here injects its clock and
;;; readiness (or uses real loopback readiness that is already there),
;;; so nothing sleeps and nothing polls in a loop.

(defun reactor-fake (clock-fn poll-fn)
  "A reactor with the injected CLOCK-FN/POLL-FN and no self-pipe:
STEP never blocks, so a fake clock fully determines the outcome."
  (wasmdown.wasi:make-wasi-reactor :clock-fn clock-fn :poll-fn poll-fn
                                   :wake-p nil))

(test wasi/reactor-deadlines
  (let* ((now 1000000000)
         (reactor (reactor-fake (lambda () now) nil))
         (fired '()))
    (unwind-protect
         (progn
           ;; a zero deadline fires on the next step (no pollable fd, no
           ;; pipe: the step returns immediately and the clock decides)
           (wasmdown.wasi:reactor-watch-deadline
            reactor 0 (lambda () (push :zero fired)))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is progress)
             (is (not pending))
             (is (equal '(:zero) fired)))
           ;; a past deadline fires, a future one does not; a future
           ;; registration leaves PENDING-P true
           (setf now 5000)
           (wasmdown.wasi:reactor-watch-deadline
            reactor 4000 (lambda () (push :past fired)))
           (let ((future (wasmdown.wasi:reactor-watch-deadline
                          reactor 9000 (lambda () (push :future fired)))))
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is progress)
               (is pending)
               (is (equal '(:past :zero) fired)))
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is (not progress))
               (is pending)
               (is (equal '(:past :zero) fired)))
             ;; cancel is idempotent and removes the pending deadline
             (funcall future)
             (funcall future)
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is (not progress))
               (is (not pending))
               (is (equal '(:past :zero) fired))))
           ;; due deadlines fire in (deadline, registration) order
           (setf now 20000)
           (let ((order '()))
             (wasmdown.wasi:reactor-watch-deadline
              reactor 10000 (lambda () (push :a order)))
             (wasmdown.wasi:reactor-watch-deadline
              reactor 10000 (lambda () (push :b order)))
             (wasmdown.wasi:reactor-watch-deadline
              reactor 9000 (lambda () (push :c order)))
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is progress)
               (is (not pending))
               (is (equal (list :c :a :b) (reverse order))))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-pending-deadline-p
  ;; THE DRAIN ORACLE'S LIVENESS QUERY: an armed one-shot deadline is
  ;; future progress; a spent/cancelled registration and a bare fd
  ;; readiness watch are not.
  (let* ((now 1000000000)
         (reactor (reactor-fake (lambda () now) nil)))
    (unwind-protect
         (progn
           (is (not (wasmdown.wasi:reactor-pending-deadline-p reactor)))
           ;; a readiness watch alone is NOT guaranteed progress: a
           ;; socket may never become ready
           (let ((cancel (wasmdown.wasi:reactor-watch-fd
                          reactor 0 :read 'oracle-test
                          (lambda (status) (declare (ignore status))))))
             (is (not (wasmdown.wasi:reactor-pending-deadline-p reactor)))
             (funcall cancel))
           ;; an armed deadline is
           (let ((cancel (wasmdown.wasi:reactor-watch-deadline
                          reactor (+ now 5000000000) (lambda () nil))))
             (is (wasmdown.wasi:reactor-pending-deadline-p reactor))
             ;; the STORE-level predicate combines it with the tick gate
             (let ((store (wasmdown.component.runtime:make-component-store
                           :reactor reactor)))
               (is (not (rtcall "%HOST-STORE-FUTURE-PROGRESS-P" store)))
               (rtcall "%HOST-STORE-WORK-ADD" store 1)
               (is (rtcall "%HOST-STORE-FUTURE-PROGRESS-P" store))
               (rtcall "%HOST-STORE-WORK-ADD" store -1))
             ;; cancelling removes it
             (funcall cancel)
             (is (not (wasmdown.wasi:reactor-pending-deadline-p reactor))))
           ;; a fired one-shot leaves nothing behind
           (wasmdown.wasi:reactor-watch-deadline reactor 0 (lambda () nil))
           (is (wasmdown.wasi:reactor-pending-deadline-p reactor))
           (wasmdown.wasi:reactor-step reactor 0)
           (is (not (wasmdown.wasi:reactor-pending-deadline-p reactor))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-fd-order
  ;; one pump = one callback per ready registration; readiness is
  ;; delivered in registration order, and each step repeats it
  ;; (level-triggered) until the cancel-fn runs
  (let* ((calls 0)
         (requests-seen '())
         (poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (incf calls)
                    (push (mapcar (lambda (request) (getf request :fd))
                                  requests)
                          requests-seen)
                    (mapcar (lambda (request)
                              (if (member (getf request :fd) '(7 9)) 1 0))
                            requests)))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (events '()))
    (unwind-protect
         (let ((seven (wasmdown.wasi:reactor-watch-fd
                       reactor 7 :read 'conn-7
                       (lambda (status) (push (cons 7 status) events))))
               (nine (wasmdown.wasi:reactor-watch-fd
                      reactor 9 :read 'conn-9
                      (lambda (status) (push (cons 9 status) events)))))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 5000000)
             (is progress)
             (is pending)
             (is (= 1 calls))
             ;; both ready fds were polled in registration order and
             ;; both callbacks ran within the one step
             (is (equal '((7 9)) requests-seen))
             (is (equal '((7 . :read) (9 . :read)) (reverse events))))
           ;; the second step pumps once more: one callback each, in order
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is progress)
             (is pending)
             (is (= 2 calls))
             (is (= 4 (length events)))
             (is (equal '((7 . :read) (9 . :read))
                        (subseq (reverse events) 2 4))))
           ;; cancelled watch is skipped, the other one still fires
           (funcall seven)
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is progress)
             (is pending)
             (is (= 5 (length events)))
             (is (equal '(9 . :read) (first events))))
           (funcall nine)
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is (not progress))
             (is (not pending))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-fd-status-and-eintr
  ;; POLLERR / POLLHUP / POLLNVAL reach the callback as :error / :hup /
  ;; :nval, taking precedence over a plain readiness
  (let* ((revents-of (lambda (fd)
                       (cond ((= fd 11) 16)     ; POLLHUP
                             ((= fd 13) 8)      ; POLLERR
                             ((= fd 15) 32)     ; POLLNVAL
                             ((= fd 17) 4)      ; POLLOUT
                             (t 0))))
         (poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (mapcar (lambda (request)
                              (funcall revents-of (getf request :fd)))
                            requests)))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (seen '()))
    (unwind-protect
         (progn
           (wasmdown.wasi:reactor-watch-fd
            reactor 11 :read 'hup-fd (lambda (status) (push (cons :hup status) seen)))
           (wasmdown.wasi:reactor-watch-fd
            reactor 13 :read 'err-fd (lambda (status) (push (cons :err status) seen)))
           (wasmdown.wasi:reactor-watch-fd
            reactor 15 :read 'nval-fd (lambda (status) (push (cons :nval status) seen)))
           (wasmdown.wasi:reactor-watch-fd
            reactor 17 :write 'write-fd
            (lambda (status) (push (cons :write status) seen)))
           (wasmdown.wasi:reactor-step reactor 1000000)
           (is (equal '((:hup . :hup) (:err . :error)
                        (:nval . :nval) (:write . :write))
                      (reverse seen))))
      (wasmdown.wasi:reactor-close reactor)))
  ;; the same fd, the same direction, DIFFERENT registration ids: two
  ;; separate poll entries -> two callbacks (never coalesced)
  (let* ((poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (mapcar (constantly 1) requests)))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (seen '()))
    (unwind-protect
         (progn
           (wasmdown.wasi:reactor-watch-fd
            reactor 5 :read 'conn-a
            (lambda (status) (push (list 'a status) seen)))
           (wasmdown.wasi:reactor-watch-fd
            reactor 5 :read 'conn-b
            (lambda (status) (push (list 'b status) seen)))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 1000000)
             (is progress)
             (is pending)
             (is (= 2 (length seen)))
             (is (equal '((a :read) (b :read)) (reverse seen)))))
      (wasmdown.wasi:reactor-close reactor)))
  ;; EINTR re-polls within the same bounded step and then dispatches
  (let* ((calls 0)
         (poll-fn (lambda (requests timeout)
                    (declare (ignore requests timeout))
                    (if (= (incf calls) 1) :eintr (list 1))))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (seen '()))
    (unwind-protect
         (progn
           (wasmdown.wasi:reactor-watch-fd
            reactor 21 :read 'eintr-fd
            (lambda (status) (push status seen)))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 2000000)
             (is (= 2 calls))
             (is progress)
             (is pending)
             (is (equal '(:read) seen))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-cancel-close
  (let* ((poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (mapcar (constantly 1) requests)))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (fired '()))
    (unwind-protect
         (progn
           ;; cancel is idempotent and no callback runs afterwards
           (let ((cancel (wasmdown.wasi:reactor-watch-fd
                          reactor 3 :read 'cancelled
                          (lambda (status) (push status fired)))))
             (funcall cancel)
             (funcall cancel)
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 1000000)
               (is (not progress))
               (is (not pending))
               (is (null fired))))
           ;; a callback cancelling itself does not deadlock and stops
           ;; the watch after exactly one delivery
           (let ((cancel nil)
                 (count 0))
             (setf cancel
                   (wasmdown.wasi:reactor-watch-fd
                    reactor 4 :read 'self-cancel
                    (lambda (status)
                      (declare (ignore status))
                      (incf count)
                      (funcall cancel))))
             (wasmdown.wasi:reactor-step reactor 1000000)
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 1000000)
               (is (not progress))
               (is (not pending)))
             (is (= 1 count)))
           ;; close cancels every registration, is idempotent, and the
           ;; reactor refuses new watches
           (let ((nothing '()))
             (wasmdown.wasi:reactor-watch-fd
              reactor 5 :read 'closed-fd
              (lambda (status) (push status nothing)))
             (wasmdown.wasi:reactor-watch-deadline
              reactor 0 (lambda () (push :deadline nothing)))
             (wasmdown.wasi:reactor-close reactor)
             (wasmdown.wasi:reactor-close reactor)
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is (not progress))
               (is (not pending)))
             (is (null nothing))
             (is (handler-case
                     (progn (wasmdown.wasi:reactor-watch-fd
                             reactor 6 :read 'after-close
                             (lambda (status) (declare (ignore status))))
                            nil)
                   (error () t)))
             (is (handler-case
                     (progn (wasmdown.wasi:reactor-watch-deadline
                             reactor 0 (lambda () nil))
                            nil)
                   (error () t)))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-tcp-loopback
  ;; the real backend: a listening TCP socket becomes readable after a
  ;; loopback connect; one bounded step reports it (no polling loop and
  ;; no sleep in the test)
  (let* ((net (sock-network))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (reactor (wasmdown.wasi:make-wasi-reactor))
         (seen '()))
    (unwind-protect
         (progn
           (is (equal '(:ok)
                      (wasmdown.wasi:tcp-bind nil net server (sock-addr 0))))
           (let ((port (sock-value-port
                        (sock-ok (wasmdown.wasi:tcp-get-local-address
                                  nil net server)))))
             (is (eq :ok (car (wasmdown.wasi:tcp-listen nil net server))))
             (is (equal '(:ok)
                        (wasmdown.wasi:tcp-connect nil net client
                                                   (sock-addr port))))
             (let* ((fd (wasmdown.wasi:wasi-tcp-socket-fd server))
                    (cancel (wasmdown.wasi:reactor-watch-fd
                             reactor fd :read 'listener
                             (lambda (status) (push status seen)))))
               (declare (ignore cancel))
               (multiple-value-bind (progress pending)
                   (wasmdown.wasi:reactor-step
                    reactor (* 2 1000 1000 1000))
                 (is progress)
                 (is pending)
                 (is (equal '(:read) seen))))))
      (wasmdown.wasi:reactor-close reactor)
      (wasmdown.wasi:close-wasi-tcp-socket client)
      (wasmdown.wasi:close-wasi-tcp-socket server))))

(test wasi/reactor-wake-self-pipe
  ;; an external producer's wake byte interrupts a blocked step
  (let ((reactor (wasmdown.wasi:make-wasi-reactor)))
    (unwind-protect
         (let* ((result nil)
                (thread
                  (sb-thread:make-thread
                   (lambda ()
                     (multiple-value-bind (progress pending)
                         (wasmdown.wasi:reactor-step
                          reactor (* 5 1000 1000 1000))
                       (setf result (list progress pending)))))))
           (is (wasmdown.wasi:reactor-wake reactor))
           (sb-thread:join-thread thread)
           (is (equal '(t nil) result)))
      (wasmdown.wasi:reactor-close reactor))))

;;; ---------------------------------------------------------------
;;; M17: the per-instance host-resource destructor registry, proven on
;;; a REAL provider graph (command-provider).

(defun %provider-of (providers package interface)
  "The provider comp-instance PROVIDERS binds for PACKAGE's INTERFACE
(the same lookup command-provider's alist keys use)."
  (cdr (assoc (wasmdown.wit:interface-import-name
               (wasmdown.wit:package-interface package interface))
              providers :test #'string=)))

(test wasi/provider-dtors-are-per-provider-instance
  ;; Every host-provided resource finalizer is registered on the
  ;; PROVIDER comp-instance whose types produced the resource ct (the
  ;; instance that OWNS the rep) — never on a guest/current instance
  ;; and never in the retired ct-keyed global. The registry is the
  ;; instance's :HOST-RESOURCE-DTORS slot, so two providers of the
  ;; same ct keep distinct releases.
  (with-fs-root (root "dtor-inst" '("a.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context
                 :preopens (list pre)
                 :networks (list (sock-network))))
           (providers (wasmdown.wasi:command-provider ctx))
           (fs-pkg (wasmdown.wasi:wasi-package "filesystem"))
           (sock-pkg (wasmdown.wasi:wasi-package "sockets"))
           (fs-types (%provider-of providers fs-pkg "types"))
           (fs-preopens (%provider-of providers fs-pkg "preopens"))
           (sock-types (%provider-of providers sock-pkg "types"))
           (desc-ct (wasmdown.wasi:descriptor-resource-ct))
           (tcp-ct (wasmdown.wasi:tcp-socket-resource-ct))
           (udp-ct (wasmdown.wasi:udp-socket-resource-ct))
           (guest (wasmdown.component.runtime:make-comp-instance))
           (resolve-at (lambda (inst ct)
                         (wasmdown.component.runtime::%host-resource-finalizer-at
                          inst ct)))
           (fin (lambda (inst ct)
                  (wasmdown.component.runtime:host-resource-finalizer
                   inst ct))))
      (unwind-protect
           (progn
             ;; command-provider really handed us the provider instances
             (is (wasmdown.component.runtime:comp-instance-p fs-types))
             (is (wasmdown.component.runtime:comp-instance-p fs-preopens))
             (is (wasmdown.component.runtime:comp-instance-p sock-types))
             ;; (a) the descriptor finalizer lives on EACH filesystem
             ;; provider instance (each owns descriptor reps: opened
             ;; files through `types', the preopen's own descriptor
             ;; through `preopens')
             (is (eq (symbol-function 'wasmdown.wasi:close-wasi-descriptor)
                     (funcall fin fs-types desc-ct)))
             (is (eq (symbol-function 'wasmdown.wasi:close-wasi-descriptor)
                     (funcall fin fs-preopens desc-ct)))
             ;; (b) the sockets provider instance owns both socket cts
             (is (eq (symbol-function 'wasmdown.wasi:close-wasi-tcp-socket)
                     (funcall fin sock-types tcp-ct)))
             (is (eq (symbol-function 'wasmdown.wasi:close-wasi-udp-socket)
                     (funcall fin sock-types udp-ct)))
             ;; (c) ABSENT from the guest/current instance — which holds
             ;; no destructor table at all, so a lookup allocates none
             (is (null (funcall fin guest desc-ct)))
             (is (null (funcall fin guest tcp-ct)))
             (is (null (funcall fin guest udp-ct)))
             (is (null (gethash :host-resource-dtors
                                (wasmdown.component.runtime:comp-instance-imports
                                 guest))))
             ;; (d) the DROP path's provenance: the provider instances
             ;; are the ONLY registration (no ct-keyed global exists any
             ;; more), so a guest resolves nothing until it LINKED the
             ;; provider — and then resolves the provider's own hook,
             ;; not some global one
             (let ((wasmdown.component.runtime::*handle-caller-inst* nil))
               (is (null (funcall resolve-at guest desc-ct)))
               (is (null (funcall resolve-at guest tcp-ct)))
               (wasmdown.component.runtime::%push-instance guest fs-types)
               (wasmdown.component.runtime::%push-instance guest sock-types)
               (is (eq (funcall fin fs-types desc-ct)
                       (funcall resolve-at guest desc-ct)))
               (is (eq (funcall fin sock-types tcp-ct)
                       (funcall resolve-at guest tcp-ct))))
             ;; (e) the registered hooks are LIVE for resources created
             ;; THROUGH the provider: the preopen's own own<descriptor>
             ;; releases as a no-op (the embedder keeps the root fd),
             ;; and created sockets close their fd exactly once
             (let* ((dirs (call-provider fs-preopens "get-directories"))
                    (own (first (first dirs)))
                    (rep (third own)))
               (is (eq desc-ct (second own)))
               (is (eq rep (wasmdown.wasi:wasi-preopen-descriptor pre)))
               (funcall (funcall fin fs-preopens desc-ct) rep)
               (is (not (wasmdown.wasi:wasi-descriptor-closed-p rep)))
               (is (plusp (wasmdown.wasi:wasi-preopen-fd pre))))
             (let* ((tcp-own (call-provider sock-types
                                            "[static]tcp-socket.create"
                                            (cons :enum "ipv4")))
                    ;; result<own<tcp-socket>, error-code> lowers to
                    ;; (:ok :own-handle CT REP)
                    (tcp (fourth tcp-own))
                    (fd (wasmdown.wasi:wasi-tcp-socket-fd tcp)))
               (is (eq tcp-ct (third tcp-own)))
               (is (and (integerp fd) (not (minusp fd))))
               (funcall (funcall fin sock-types tcp-ct) tcp)
               (is (eql -1 (wasmdown.wasi:wasi-tcp-socket-fd tcp)))
               (is (eq :closed (wasmdown.wasi:wasi-tcp-socket-state tcp)))
               (is (null (sb-unix:unix-fstat fd))))
             (let* ((udp-own (call-provider sock-types
                                            "[static]udp-socket.create"
                                            (cons :enum "ipv4")))
                    (udp (fourth udp-own))
                    (fd (wasmdown.wasi:wasi-udp-socket-fd udp)))
               (is (eq udp-ct (third udp-own)))
               (is (and (integerp fd) (not (minusp fd))))
               (funcall (funcall fin sock-types udp-ct) udp)
               (is (eql -1 (wasmdown.wasi:wasi-udp-socket-fd udp)))
               (is (null (sb-unix:unix-fstat fd))))
             ;; (f) a SECOND provider instance is independently
             ;; registered (a ct-keyed global could hold only one), a
             ;; RE-registration on the same instance is a no-op that
             ;; keeps the first hook, and a registration elsewhere
             ;; never reaches the provider's slot
             (let* ((ctx2 (wasmdown.wasi:make-wasi-context
                           :preopens (list pre)
                           :networks (list (sock-network))))
                    (fs2 (wasmdown.wasi:filesystem-types-instance ctx2))
                    (is-close (symbol-function
                               'wasmdown.wasi:close-wasi-descriptor))
                    (other (wasmdown.component.runtime:make-comp-instance))
                    (hook (lambda (rep) (declare (ignore rep)) :other)))
               (is (not (eq fs2 fs-types)))
               (is (eq is-close (funcall fin fs2 desc-ct)))
               (is (eq is-close
                       (wasmdown.component.runtime:host-register-resource-dtor
                        fs2 desc-ct hook)))
               (is (eq is-close (funcall fin fs2 desc-ct)))
               (is (eq is-close (funcall fin fs-types desc-ct)))
               (wasmdown.component.runtime:host-register-resource-dtor
                other desc-ct hook)
               (is (eq hook (funcall fin other desc-ct)))
               (is (eq is-close (funcall fin fs2 desc-ct)))
               (is (eq is-close (funcall fin fs-types desc-ct)))
               (is (eq is-close (funcall fin fs-preopens desc-ct)))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE STORE DRIVER (steps 5/6): the store-owned completion queue,
;;; the lock-free host-work gate, the bounded reactor tick, and the
;;; queued host-end-ready retry. Every test here injects a SCRIPTED
;;; reactor, so no clock is read and nothing sleeps.

(defun rsym (name)
  "The runtime's internal symbol NAME (upcased at lookup)."
  (or (find-symbol (string-upcase name) '#:wasmdown.component.runtime)
      (error "no runtime symbol ~a" name)))

(defstruct scripted-reactor
  (calls 0)                          ; %reactor-step invocations
  (budgets '())                      ; the max-wait-ns of each step
  (on-step nil))                     ; a closure a test may fire

(defmethod wasmdown.component.runtime::%reactor-step
    ((reactor scripted-reactor) max-wait-ns)
  (incf (scripted-reactor-calls reactor))
  (push max-wait-ns (scripted-reactor-budgets reactor))
  (let ((fn (scripted-reactor-on-step reactor)))
    (when fn (funcall fn)))
  (values nil nil))

(defun store-instance (&key reactor)
  "A bare comp-instance whose store has REACTOR installed (installing
it creates the store). Returns (values INSTANCE STORE)."
  (let ((inst (wasmdown.component.runtime:make-comp-instance)))
    (funcall (fdefinition (list 'setf (rsym "%inst-reactor"))) reactor inst)
    (values inst (funcall (rsym "%inst-host-store") inst))))

(defun store-work (store)
  (funcall (rsym "host-store-work") store))

(test wasi/store-tick-idle-does-not-step-reactor
  ;; THE GATE: an idle store pays one word read and performs NO
  ;; reactor step and NO pump.
  (let ((reactor (make-scripted-reactor)))
    (multiple-value-bind (inst store) (store-instance :reactor reactor)
      (declare (ignore inst))
      (is (zerop (store-work store)))
      (is (zerop (funcall (rsym "%store-tick") store)))
      (is (zerop (scripted-reactor-calls reactor)))
      (is (null (scripted-reactor-budgets reactor))))))

(test wasi/store-tick-pending-work-pumps-bounded
  ;; THE ONLY TIME THE REACTOR IS STEPPED: pending host work. The
  ;; budget is BOUNDED, and a caller's larger request is clamped.
  (let ((reactor (make-scripted-reactor))
        (ran nil))
    (multiple-value-bind (inst store) (store-instance :reactor reactor)
      (declare (ignore inst))
      (funcall (rsym "%host-enqueue") (lambda () (setf ran t)) store)
      (is (funcall (rsym "%host-store-work-pending-p") store))
      (is (= 1 (funcall (rsym "%store-tick") store)))
      (is ran)
      (is (= 1 (scripted-reactor-calls reactor)))
      (is (= (symbol-value (rsym "+store-tick-max-wait-ns+"))
             (first (scripted-reactor-budgets reactor))))
      (is (plusp (first (scripted-reactor-budgets reactor))))
      ;; the gate closes: nothing pending means no second step
      (is (zerop (funcall (rsym "%store-tick") store)))
      (is (= 1 (scripted-reactor-calls reactor)))
      ;; an unbounded caller request is clamped to the constant
      (funcall (rsym "%host-enqueue") (lambda () nil) store)
      (is (integerp (funcall (rsym "%store-tick") store
                             :max-wait-ns 999999999)))
      (is (= (symbol-value (rsym "+store-tick-max-wait-ns+"))
             (first (scripted-reactor-budgets reactor))))
      (is (= 2 (scripted-reactor-calls reactor))))))

(test wasi/store-tick-completion-runs-on-store-context
  ;; A completion QUEUED from a poster thread runs on the store's own
  ;; context: *CURRENT-COMPONENT-STORE* inside the continuation is the
  ;; store, and it did not run before the tick.
  (multiple-value-bind (inst store) (store-instance :reactor (make-scripted-reactor))
    (declare (ignore inst))
    (let ((sym (rsym "*current-component-store*"))
          (p nil)
          (seen :unset)
          (value :unset))
      (progv (list sym) (list store)
        (setf p (wasmdown.component.runtime:make-host-pending
                 :start (lambda (resolve reject acknowledge)
                          (declare (ignore reject acknowledge))
                          (funcall resolve 42)
                          (lambda () nil)))))
      (funcall (rsym "%host-pending-set-continuation")
               p (lambda (outcome)
                   (setf seen (symbol-value sym)
                         value (cdr outcome))))
      (is (eq :unset seen))            ; never inline
      (is (integerp (funcall (rsym "%store-tick") store)))
      (is (eq store seen))
      (is (eql 42 value))
      ;; the pending's work unit and the delivery event are both gone
      (is (zerop (store-work store)))
      (is (zerop (funcall (rsym "%store-tick") store))))))

(test wasi/host-end-ready-queues-and-guards-generation
  ;; A readiness notification is ENQUEUED on the owning store, not run
  ;; on the caller's thread; a stale generation cannot resume a newer
  ;; parked copy.
  (multiple-value-bind (inst store) (store-instance :reactor (make-scripted-reactor))
    (let ((calls 0) (accept nil))
      (multiple-value-bind (reader writer)
          (wasmdown.component.runtime:make-host-stream-pair :u8 :owner inst)
        (wasmdown.component.runtime:host-end-set-sink
         reader (lambda (octets start end)
                  (declare (ignore octets start end))
                  (incf calls)
                  (values (if accept 3 0) nil)))
        (funcall (fdefinition (list 'setf (rsym "async-end-buffer")))
                 (wasmdown.component.runtime:make-host-buffer
                  3 :values '(1 2 3))
                 writer)
        ;; park the first attempt (sink (0 nil) = would-block)
        (funcall (rsym "%host-sink-drain") writer)
        (let ((gen1 (wasmdown.component.runtime:host-end-generation reader)))
          (is (= 1 calls))
          ;; a NEWER attempt, with a new generation, replaces the park
          (funcall (rsym "%end-advance-generation") reader)
          (funcall (rsym "%host-sink-drain") writer)
          (let ((gen2 (wasmdown.component.runtime:host-end-generation reader)))
            (is (= 2 calls))
            (is (not (eql gen1 gen2)))
            ;; STALE: enqueued, ticked, and resumes NOTHING
            (wasmdown.component.runtime:host-end-ready reader gen1)
            (is (= 2 calls))           ; not run inline
            (is (integerp (funcall (rsym "%store-tick") store)))
            (is (= 2 calls))
            (is (funcall (rsym "async-end-host-copy") reader))
            ;; CURRENT: still queued, then resumed on the tick
            (setf accept t)
            (wasmdown.component.runtime:host-end-ready reader gen2)
            (is (= 2 calls))
            (is (integerp (funcall (rsym "%store-tick") store)))
            (is (= 3 calls))
            (is (null (funcall (rsym "async-end-host-copy") reader)))
            (is (null (funcall (rsym "async-end-buffer") writer)))))))))

;;; ---------------------------------------------------------------
;;; STEP 6: the bounded HOST EXECUTOR (DNS + blocking stdio).
;;; Deterministic: worker completion is observed through condition
;;; variables / bounded YIELDs — no test sleeps.

(defun wait-until (predicate &key (limit 200000))
  "Bounded, sleep-free wait for PREDICATE (a worker's observable
effect): yield the CPU between checks, never sleep."
  (loop repeat limit
        when (funcall predicate) return t
        do (sb-thread:thread-yield)))

(test wasi/host-executor-budget-and-close
  (let* ((exec (wasmdown.wasi:make-host-executor :name "test-exec"
                                                 :workers 2))
         (mutex (sb-thread:make-mutex :name "test-exec-done"))
         (cv (sb-thread:make-waitqueue :name "test-exec-done"))
         (done 0))
    ;; the budget is FIXED at construction
    (is (= 2 (wasmdown.wasi::host-executor-worker-count exec)))
    (is (= 2 (length (wasmdown.wasi::host-executor-threads exec))))
    (dotimes (i 2)
      (is (wasmdown.wasi::%executor-submit
           exec
           (lambda ()
             (sb-thread:with-recursive-lock (mutex)
               (incf done)
               (sb-thread:condition-broadcast cv))))))
    (sb-thread:with-recursive-lock (mutex)
      (loop while (< done 2)
            do (sb-thread:condition-wait cv mutex)))
    (is (= 2 done))
    (is (wasmdown.wasi::%executor-close exec))
    ;; a closed service refuses work: the caller DETACHES
    (is (null (wasmdown.wasi::%executor-submit exec (lambda () nil))))))

(test wasi/host-executor-close-never-joins
  ;; CLOSE must not wait for an un-cancellable job: it returns while
  ;; the job is still running, and the job finishes into a discarded
  ;; result.
  (let* ((exec (wasmdown.wasi:make-host-executor :name "test-block"
                                                 :workers 1))
         (started (sb-thread:make-semaphore :name "started"))
         (release (sb-thread:make-semaphore :name "release"))
         (finished (sb-thread:make-semaphore :name "finished"))
         (ran nil))
    (is (wasmdown.wasi::%executor-submit
         exec (lambda ()
                (sb-thread:signal-semaphore started)
                (sb-thread:wait-on-semaphore release)
                (setf ran t)
                (sb-thread:signal-semaphore finished))))
    (sb-thread:wait-on-semaphore started)
    (is (wasmdown.wasi::%executor-close exec))
    (is (null ran))                    ; close returned first
    (is (null (wasmdown.wasi::%executor-submit exec (lambda () nil))))
    (sb-thread:signal-semaphore release)
    (sb-thread:wait-on-semaphore finished)
    (is ran)))

(test wasi/host-executor-separate-budgets
  ;; DNS and blocking stdio never share a pool: one slow resolver can
  ;; not starve a stdio span.
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let ((dns (wasmdown.wasi:wasi-context-dns-executor ctx))
               (stdio (wasmdown.wasi:wasi-context-stdio-executor ctx)))
           (is (not (eq dns stdio)))
           (is (= wasmdown.wasi::*dns-worker-count*
                  (wasmdown.wasi::host-executor-worker-count dns)))
           (is (= wasmdown.wasi::*stdio-worker-count*
                  (wasmdown.wasi::host-executor-worker-count stdio)))
           ;; the context caches them: same service on the next call
           (is (eq dns (wasmdown.wasi:wasi-context-dns-executor ctx)))
           (is (eq stdio (wasmdown.wasi:wasi-context-stdio-executor ctx))))
      (is (wasmdown.wasi:close-wasi-context-executors ctx))
      ;; idempotent, and a fresh service can be built afterwards
      (is (wasmdown.wasi:close-wasi-context-executors ctx)))))

(test wasi/stdio-blocking-sink-copies-the-span
  ;; A borrowed BLOCKING fd must never run write(2) on the guest's
  ;; thread. The span is COPIED into the worker's vector: overwriting
  ;; the caller's vector immediately after the call cannot change what
  ;; reaches the pipe.
  (multiple-value-bind (read-fd write-fd) (sb-unix:unix-pipe)
    (multiple-value-bind (pair-reader end)
        (wasmdown.component.runtime:make-host-stream-pair :u8)
      (declare (ignore pair-reader))
      (let* ((out (sb-sys:make-fd-stream write-fd :output t
                                                 :element-type '(unsigned-byte 8)
                                                 :buffering :none))
             (ctx (wasmdown.wasi:make-wasi-context))
             (sink nil)
             (octets (make-array 4 :element-type '(unsigned-byte 8)
                                   :initial-contents '(97 98 99 100))))
      (unwind-protect
           (progn
             ;; the fd is BLOCKING (borrowed): the worker path applies
             (is (not (wasmdown.wasi::%fd-nonblocking-p write-fd)))
             (setf sink (wasmdown.wasi:cl-stream-sink
                         out :context ctx :end end))
             (multiple-value-bind (consumed error) (funcall sink octets 0 4)
               ;; WOULD-BLOCK: nothing was CLAIMED before the write ran
               (is (eql 0 consumed))
               (is (null error)))
             ;; the worker owns a COPY: clobber the caller's vector now
             (fill octets 120)
             (let ((count nil))
               (is (wait-until (lambda ()
                                 (multiple-value-bind (n error)
                                     (funcall sink octets 0 4)
                                   (declare (ignore error))
                                   (when (and (integerp n) (plusp n))
                                     (setf count n)
                                     t)))))
               (is (= 4 count)))
             (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
               (is (= 4 (sb-unix:unix-read read-fd (sb-sys:vector-sap buf) 4)))
               (is (equal '(97 98 99 100) (coerce buf 'list)))))
        (ignore-errors (close out))
        (wasmdown.wasi:close-wasi-context-executors ctx)
        (ignore-errors (sb-unix:unix-close read-fd)))))))

(test wasi/stdio-blocking-source-reads-on-the-worker
  ;; Even when bytes are ALREADY available, a blocking fd's read is the
  ;; worker's: the first attempt is WOULD-BLOCK, the retry delivers.
  (multiple-value-bind (read-fd write-fd) (sb-unix:unix-pipe)
    (multiple-value-bind (pair-reader end)
        (wasmdown.component.runtime:make-host-stream-pair :u8)
      (declare (ignore pair-reader))
      (let* ((in (sb-sys:make-fd-stream read-fd :input t
                                               :element-type '(unsigned-byte 8)
                                               :buffering :none))
             (ctx (wasmdown.wasi:make-wasi-context))
             (source nil))
      (unwind-protect
           (progn
             (let ((bytes (make-array 3 :element-type '(unsigned-byte 8)
                                        :initial-contents '(104 105 33))))
               (sb-unix:unix-write write-fd bytes 0 3))
             (setf source (wasmdown.wasi:cl-stream-source
                           in :context ctx :end end))
             (multiple-value-bind (octets eof) (funcall source 3)
               (is (null octets))
               (is (null eof)))
             (let ((got nil))
               (is (wait-until (lambda ()
                                 (multiple-value-bind (octets eof)
                                     (funcall source 3)
                                   (declare (ignore eof))
                                   (when octets
                                     (setf got (coerce octets 'list))
                                     t)))))
               (is (equal '(104 105 33) got))))
        (ignore-errors (close in))
        (wasmdown.wasi:close-wasi-context-executors ctx)
        (ignore-errors (sb-unix:unix-close write-fd)))))))

;;; ---------------------------------------------------------------
;;; STEP 6: resolve-addresses on the bounded DNS executor.

(test wasi/dns-async-pending-and-detach
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (net (sock-network :dns '("localhost")))
         (exec (wasmdown.wasi:wasi-context-dns-executor ctx)))
    (unwind-protect
         (progn
           (is (= wasmdown.wasi::*dns-worker-count*
                  (wasmdown.wasi::host-executor-worker-count exec)))
           ;; the provider's answer is a HOST-PENDING (async WIT), and
           ;; the join drives it to the resolved value
           (let ((pending (wasmdown.wasi:ip-name-lookup-resolve-async
                           ctx net "localhost")))
             (is (wasmdown.component.runtime:host-pending-p pending))
             (let ((value (pending-drive pending)))
               (is (eq :ok (car value)))
               (is (plusp (length (cdr value))))))
           ;; an IP literal never reaches the resolver (no worker)
           (let ((literal (wasmdown.wasi:ip-name-lookup-resolve-async
                           ctx net "127.0.0.1")))
             (is (equal '(:ok ("ipv4" 127 0 0 1)) literal)))
           ;; a DNS-policy refusal stays a synchronous error value
           (let ((denied (wasmdown.wasi:ip-name-lookup-resolve-async
                          ctx (sock-network :dns :none) "example.invalid")))
             (is (eq :err (car denied)))
             (is (equal "access-denied"
                        (wasmdown.wasi:sockets-error-code (cdr denied)))))
           ;; the provider closure itself answers with a HOST-PENDING
           ;; (the async functype); a direct CL caller has no guest
           ;; thread to park, so the test drives the pending itself
           (let* ((inst (wasmdown.wasi:ip-name-lookup-instance ctx :network net))
                  (fn (provider-fn inst "resolve-addresses"))
                  (pending (funcall (wasmdown.component.runtime:lifted-func-host fn)
                                    (list "localhost"))))
             (is (wasmdown.component.runtime:host-pending-p pending))
             (is (eq :ok (car (pending-drive pending)))))
           ;; CANCEL DETACHES: the pending is terminal (:cancelled) after
           ;; the pump ran its cancel-fn, and a LATE worker result cannot
           ;; deliver into it (the job's target is gone, the chain is
           ;; still freeaddrinfo'd by %GETADDRINFO-CHAIN).
           (let ((pending (wasmdown.wasi:ip-name-lookup-resolve-async
                           ctx net "localhost")))
             (is (funcall (rsym "%host-pending-request-cancel") pending))
             (funcall (rsym "%host-pump")
                      (funcall (rsym "host-pending-store") pending))
             (is (eq :cancelled
                     (funcall (rsym "host-pending-state") pending)))
             (is (eq :cancel
                     (car (funcall (rsym "host-pending-outcome") pending))))
             ;; a second resolve request is refused (already terminal)
             (is (null (funcall (rsym "%host-pending-request-cancel")
                                pending)))))
      (is (wasmdown.wasi:close-wasi-context-executors ctx)))))

;;; ---------------------------------------------------------------
;;; STEP 6: accept/recv/send register a reactor watch keyed by the
;;; CONNECTION/STREAM identity, and readiness resumes the parked copy
;;; through the store queue. The reactor is REAL (libc poll); the fd is
;;; made ready BEFORE the tick, so no step waits and nothing sleeps.

(test wasi/sockets-accept-reactor-readiness
  (let* ((reactor (wasmdown.wasi:make-wasi-reactor :wake-p nil))
         (ctx (wasmdown.wasi:make-wasi-context :reactor reactor))
         (net (sock-network))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (accepted nil)
         (listening nil))
    (multiple-value-bind (inst store) (store-instance :reactor reactor)
      (declare (ignore inst))
      (let ((wasmdown.component.runtime::*current-component-store* store))
        (unwind-protect
             (progn
               (is (equal '(:ok) (wasmdown.wasi:tcp-bind
                                  ctx net server (sock-addr 0))))
               (let* ((port (sock-value-port
                             (sock-ok (wasmdown.wasi:tcp-get-local-address
                                       ctx net server))))
                      (buf (wasmdown.component.runtime:make-host-buffer 1)))
                 (setf listening (wasmdown.wasi:tcp-listen ctx net server))
                 (is (eq :ok (car listening)))
                 (let ((stream (cdr listening)))
                   ;; nothing queued: WOULD-BLOCK, and a READ watch is
                   ;; armed under the STREAM's identity (never the fd)
                   (rtcall "%END-COPY" stream buf t)
                   (is (null (funcall (rsym "async-waitable-pending-event") stream)))
                   (is (= 1 (hash-table-count
                             (wasmdown.wasi::wasi-reactor-fd-table reactor))))
                   (let ((key (first (loop for k being the hash-keys
                                           of (wasmdown.wasi::wasi-reactor-fd-table
                                               reactor)
                                           collect k))))
                     (is (eq (rtcall "ASYNC-END-OTHER" stream) (second key)))
                     (is (eq :read (third key))))
                   ;; the connection arrives; ONE tick: reactor fires ->
                   ;; host-end-ready is QUEUED -> the parked copy resumes
                   (is (equal '(:ok) (wasmdown.wasi:tcp-connect
                                      ctx net client (sock-addr port))))
                   (rtcall "%STORE-TICK" store)
                   (is (eql (%completed 1)
                            (third (rtcall "%WAITABLE-GET-PENDING-EVENT"
                                           stream))))
                   (let ((vals (wasmdown.component.runtime:host-buffer-values
                                buf)))
                     (is (= 1 (length vals)))
                     (setf accepted (third (first vals))))
                   ;; one-shot: the watch is gone and the gate released
                   (is (zerop (hash-table-count
                               (wasmdown.wasi::wasi-reactor-fd-table reactor))))
                   (is (zerop (funcall (rsym "host-store-work") store))))))
          (when accepted (wasmdown.wasi:close-wasi-tcp-socket accepted))
          (wasmdown.wasi:close-wasi-tcp-socket client)
          (wasmdown.wasi:close-wasi-tcp-socket server)
          (wasmdown.wasi:reactor-close reactor))))))

(test wasi/sockets-recv-reactor-readiness
  (let* ((reactor (wasmdown.wasi:make-wasi-reactor :wake-p nil))
         (ctx (wasmdown.wasi:make-wasi-context :reactor reactor))
         (net (sock-network))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (accepted nil))
    (multiple-value-bind (inst store) (store-instance :reactor reactor)
      (declare (ignore inst))
      (let ((wasmdown.component.runtime::*current-component-store* store))
        (unwind-protect
             (progn
               (is (equal '(:ok) (wasmdown.wasi:tcp-bind
                                  ctx net server (sock-addr 0))))
               (let* ((port (sock-value-port
                             (sock-ok (wasmdown.wasi:tcp-get-local-address
                                       ctx net server))))
                      (listening (wasmdown.wasi:tcp-listen ctx net server)))
                 (is (equal '(:ok) (wasmdown.wasi:tcp-connect
                                    ctx net client (sock-addr port))))
                 (setf accepted (sock-accept (cdr listening)))
                 (is (not (null accepted)))
                 (let* ((pair (wasmdown.wasi:tcp-receive ctx net accepted))
                        (stream (first pair))
                        (buf (wasmdown.component.runtime:make-host-buffer 8)))
                   ;; nothing to read: WOULD-BLOCK + a READ watch
                   (rtcall "%END-COPY" stream buf t)
                   (is (null (funcall (rsym "async-waitable-pending-event") stream)))
                   (is (= 1 (hash-table-count
                             (wasmdown.wasi::wasi-reactor-fd-table reactor))))
                   ;; the peer writes only AFTER the park
                   (multiple-value-bind (wreader wwriter) (sock-stream)
                     (let ((future (wasmdown.wasi:tcp-send ctx net client
                                                           wreader)))
                       (is (eql (%completed 4)
                                (sock-stage wwriter '(112 105 110 103))))
                       (sock-drop wwriter)
                       (is (equal '(:ok) (sock-future future)))))
                   (rtcall "%STORE-TICK" store)
                   (is (eql (%completed 4)
                            (third (rtcall "%WAITABLE-GET-PENDING-EVENT"
                                           stream))))
                   (is (equal '(112 105 110 103)
                              (wasmdown.component.runtime:host-buffer-values
                               buf)))
                   (is (zerop (hash-table-count
                               (wasmdown.wasi::wasi-reactor-fd-table reactor))))
                   (is (zerop (funcall (rsym "host-store-work") store)))
                   (sock-drop stream))))
          (when accepted (wasmdown.wasi:close-wasi-tcp-socket accepted))
          (wasmdown.wasi:close-wasi-tcp-socket client)
          (wasmdown.wasi:close-wasi-tcp-socket server)
          (wasmdown.wasi:reactor-close reactor))))))

(defun drain-available (reader limit)
  "One non-blocking read of whatever READER has RIGHT NOW (a parked
copy stays parked and its absent event is simply cleared)."
  (let ((buf (wasmdown.component.runtime:make-host-buffer limit)))
    (rtcall "%END-COPY" reader buf t)
    (let ((values (wasmdown.component.runtime:host-buffer-values buf))
          (event (funcall (rsym "async-waitable-pending-event") reader)))
      (when event (funcall event))
      values)))

(test wasi/sockets-send-reactor-readiness
  ;; A full kernel buffer makes send(2) would-block; the sink arms a
  ;; WRITE watch; draining the peer and ticking the store resumes the
  ;; parked write until it is fully accepted.
  (let* ((reactor (wasmdown.wasi:make-wasi-reactor :wake-p nil))
         (ctx (wasmdown.wasi:make-wasi-context :reactor reactor))
         (net (sock-network))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (accepted nil))
    (multiple-value-bind (inst store) (store-instance :reactor reactor)
      (declare (ignore inst))
      (let ((wasmdown.component.runtime::*current-component-store* store))
        (unwind-protect
             (progn
               (is (equal '(:ok) (wasmdown.wasi:tcp-bind
                                  ctx net server (sock-addr 0))))
               (let* ((port (sock-value-port
                             (sock-ok (wasmdown.wasi:tcp-get-local-address
                                       ctx net server))))
                      (listening (wasmdown.wasi:tcp-listen ctx net server)))
                 (is (equal '(:ok) (wasmdown.wasi:tcp-connect
                                    ctx net client (sock-addr port))))
                 (setf accepted (sock-accept (cdr listening)))
                 (is (not (null accepted)))
                 (is (equal '(:ok) (wasmdown.wasi::tcp-set-send-buffer-size
                                    ctx net client 4096)))
                 (is (equal '(:ok)
                            (wasmdown.wasi::tcp-set-receive-buffer-size
                             ctx net accepted 4096)))
                 (multiple-value-bind (reader writer) (sock-stream)
                   (let ((future (wasmdown.wasi:tcp-send ctx net client
                                                         reader))
                         (payload (make-list 65536 :initial-element 122)))
                     ;; stage the whole payload: it cannot all fit
                     (rtcall "%END-COPY" writer
                             (wasmdown.component.runtime:make-host-buffer
                              65536 :values payload)
                             nil)
                     ;; the copy is PARKED with a WRITE watch
                     (is (not (null (funcall (rsym "async-end-host-copy")
                                             reader))))
                     (is (= 1 (hash-table-count
                               (wasmdown.wasi::wasi-reactor-fd-table reactor))))
                     (let ((key (first (loop for k being the hash-keys
                                             of (wasmdown.wasi::wasi-reactor-fd-table
                                                 reactor)
                                             collect k))))
                       (is (eq reader (second key)))
                       (is (eq :write (third key))))
                     ;; the peer ITSELF can read what was accepted (the
                     ;; bytes the sink took are on the wire)
                     (let* ((pair (wasmdown.wasi:tcp-receive ctx net accepted))
                            (stream (first pair))
                            (got 0))
                       (loop repeat 500
                             for before = got
                             do (incf got (length (drain-available stream 65536)))
                             while (and (> got before) (< got 32768)))
                       (is (plusp got)))
                     ;; dropping the writer ends the copy and settles the
                     ;; send future (the remaining bytes are abandoned)
                     (sock-drop writer)
                     (is (equal '(:ok) (sock-future future)))))))
          (when accepted (wasmdown.wasi:close-wasi-tcp-socket accepted))
          (wasmdown.wasi:close-wasi-tcp-socket client)
          (wasmdown.wasi:close-wasi-tcp-socket server)
          (wasmdown.wasi:reactor-close reactor))))))

(test wasi/sockets-send-resume-scripted-readiness
  ;; THE SEND RESUME LOOP, driven by SCRIPTED readiness: the sink
  ;; really arms a WRITE watch on the socket after send(2) reports
  ;; EAGAIN, but POLLOUT comes from the injected POLL-FN (the reactor's
  ;; SCRIPTED-READY flag), never from the kernel. So the parked copy's
  ;; exact resume path — reactor callback -> host-end-ready into the
  ;; store queue -> %STORE-TICK -> retry — is asserted without a real
  ;; POLLOUT, without a sleep and without a polling loop.
  ;;
  ;; The watch accounting is exact: SEQ counts every registration ever
  ;; made, and one armed watch is delivered exactly once, so SEQ equals
  ;; the number of deliveries and the registration table returns to its
  ;; prior value (empty) when the copy completes; a later look must
  ;; never see two, and a readiness naming an OLD generation must
  ;; resume nothing while the CURRENT watch stays armed.
  (let* ((ready-p nil)                ; the poll-fn's SCRIPTED readiness
         (fd-requests '())            ; fd watches each poll saw, newest first
         (poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (let ((n 0))
                      (dolist (request requests)
                        (when (eq :fd (getf request :kind)) (incf n)))
                      (push n fd-requests))
                    (mapcar (lambda (request)
                              (if (and ready-p
                                       (eq :fd (getf request :kind)))
                                  wasmdown.wasi::+pollout+
                                  0))
                            requests)))
         (reactor (wasmdown.wasi:make-wasi-reactor
                   :wake-p nil :clock-fn (constantly 0) :poll-fn poll-fn))
         (ctx (wasmdown.wasi:make-wasi-context :reactor reactor))
         (net (sock-network))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (accepted nil)
         (deliveries 0)
         (received 0))
    (multiple-value-bind (inst store) (store-instance :reactor reactor)
      (declare (ignore inst))
      (let ((wasmdown.component.runtime::*current-component-store* store))
        (unwind-protect
             (progn
               (is (equal '(:ok) (wasmdown.wasi:tcp-bind
                                  ctx net server (sock-addr 0))))
               (let* ((port (sock-value-port
                             (sock-ok (wasmdown.wasi:tcp-get-local-address
                                       ctx net server))))
                      (listening (wasmdown.wasi:tcp-listen ctx net server)))
                 (is (equal '(:ok) (wasmdown.wasi:tcp-connect
                                    ctx net client (sock-addr port))))
                 (setf accepted (sock-accept (cdr listening)))
                 (is (not (null accepted)))
                 ;; 32 KiB each: small enough that a 256 KiB payload
                 ;; cannot all be accepted (so send(2) really reports
                 ;; EAGAIN) yet above the loopback stall the 4 KiB
                 ;; settings hit, so draining the peer reopens the
                 ;; window and the resumed send makes progress
                 (is (equal '(:ok) (wasmdown.wasi::tcp-set-send-buffer-size
                                    ctx net client 32768)))
                 (is (equal '(:ok)
                            (wasmdown.wasi::tcp-set-receive-buffer-size
                             ctx net accepted 32768)))
                 (multiple-value-bind (reader writer) (sock-stream)
                   (let* ((future (wasmdown.wasi:tcp-send ctx net client reader))
                          (fd (wasmdown.wasi:wasi-tcp-socket-fd client))
                          ;; the PEER receive stream is deliberately
                          ;; created WITHOUT a reactor: its own would-block
                          ;; must not add a READ watch, so the reactor's
                          ;; registration table only ever holds the SEND
                          ;; watch under test (the test drains the peer
                          ;; explicitly with DRAIN-AVAILABLE)
                          (peer (first (wasmdown.wasi:tcp-receive
                                        nil net accepted)))
                          (payload (make-list 262144 :initial-element 122))
                          (fd-table (lambda ()
                                      (wasmdown.wasi::wasi-reactor-fd-table
                                       reactor)))
                          (armed (lambda ()
                                   (hash-table-count (funcall fd-table))))
                          (parked-p
                            (lambda ()
                              (not (null (funcall
                                          (rsym "async-end-host-copy")
                                          reader)))))
                          (resume-until-done
                            (lambda ()
                              "Drain the peer, then deliver ONE scripted
readiness per parked attempt until the copy completes."
                              (loop
                                (when (not (funcall parked-p)) (return))
                                (incf received
                                      (length (drain-available peer 65536)))
                                (when (not (funcall parked-p)) (return))
                                (setf ready-p t)
                                (funcall (rsym "%store-tick") store)
                                (setf ready-p nil)
                                (incf deliveries)
                                (is (<= (funcall armed) 1))
                                (is (= 1 (first fd-requests)))
                                (when (> deliveries 500)
                                  (error "the send copy never completed"))))))
                     ;; STAGE the whole payload: send(2) fills the small
                     ;; kernel buffer, reports EAGAIN, and the sink arms
                     ;; a WRITE watch keyed by the STREAM identity
                     (rtcall "%END-COPY" writer
                             (wasmdown.component.runtime:make-host-buffer
                              262144 :values payload)
                             nil)
                     (is (funcall parked-p))
                     (is (= 1 (funcall armed)))
                     (let ((key (first (loop for k being the hash-keys
                                             of (funcall fd-table)
                                             collect k))))
                       (is (eq reader (second key)))
                       (is (eq :write (third key)))
                       (is (= fd (first key))))
                     (let ((gen1 (wasmdown.component.runtime:host-end-generation
                                  reader)))
                       ;; SCRIPTED readiness: the parked send resumes and
                       ;; runs to completion through the store queue
                       (funcall resume-until-done)
                       (loop for n = (length (drain-available peer 65536))
                             while (plusp n) do (incf received n))
                       ;; the copy completed with the FULL byte count
                       (is (plusp deliveries)) ; the park really RESUMED
                       (is (eql (%completed 262144)
                                (third (rtcall "%WAITABLE-GET-PENDING-EVENT"
                                               writer))))
                       (is (= 262144 received))
                       ;; the watch is released: the registration table
                       ;; is back to its prior (empty) value, and every
                       ;; registration made was delivered exactly once
                       (is (zerop (funcall armed)))
                       (is (null (wasmdown.wasi::wasi-reactor-fd-order
                                  reactor)))
                       (is (= deliveries (wasmdown.wasi::wasi-reactor-seq
                                          reactor)))
                       (is (zerop (funcall (rsym "host-store-work") store)))
                       ;; ---- THE STALE GENERATION ----
                       ;; a NEWER send parks at a NEW copy generation
                       (setf ready-p nil)
                       (let ((gen2 nil))
                         (rtcall "%END-COPY" writer
                                 (wasmdown.component.runtime:make-host-buffer
                                  262144 :values (make-list
                                                  262144 :initial-element 121))
                                 nil)
                         (setf gen2 (wasmdown.component.runtime:host-end-generation
                                     reader))
                         (is (not (eql gen1 gen2)))
                         (is (funcall parked-p))
                         (is (= 1 (funcall armed)))
                         ;; the peer's bytes from attempt 1 are gone; a
                         ;; readiness naming the OLD generation is
                         ;; delivered and ticked while readiness stays
                         ;; SCRIPTED OFF. The old watch was one-shot and
                         ;; already consumed, so the LATE notification is
                         ;; modelled by the one call such a callback
                         ;; makes: host-end-ready with the generation it
                         ;; captured at arm time.
                         (wasmdown.component.runtime:host-end-ready reader gen1)
                         (is (integerp (funcall (rsym "%store-tick") store)))
                         ;; NOTHING resumed: the newer attempt is still
                         ;; parked and the CURRENT watch is untouched
                         (is (funcall parked-p))
                         (is (null (funcall (rsym "async-waitable-pending-event")
                                            writer)))
                         (is (= 1 (funcall armed)))
                         ;; the CURRENT generation is what resumes it
                         (let ((before deliveries))
                           (funcall resume-until-done)
                           (is (> deliveries before)))
                         (loop for n = (length (drain-available peer 65536))
                               while (plusp n) do (incf received n))
                         (is (eql (%completed 262144)
                                  (third (rtcall "%WAITABLE-GET-PENDING-EVENT"
                                                 writer))))
                         (is (= (* 2 262144) received))
                         (is (zerop (funcall armed)))
                         (is (null (wasmdown.wasi::wasi-reactor-fd-order
                                    reactor)))
                         (is (= deliveries (wasmdown.wasi::wasi-reactor-seq
                                            reactor)))
                         (is (zerop (funcall (rsym "host-store-work") store))))
                       ;; every poll saw at most one fd registration
                       (is (every (lambda (n) (<= n 1)) fd-requests))
                       (sock-drop writer)
                       (is (equal '(:ok) (sock-future future)))))))
          (when accepted (wasmdown.wasi:close-wasi-tcp-socket accepted))
          (wasmdown.wasi:close-wasi-tcp-socket client)
          (wasmdown.wasi:close-wasi-tcp-socket server)
          (wasmdown.wasi:reactor-close reactor)))))))

(defun pending-drive (pending)
  "Drive PENDING to a terminal outcome on its own store (the test's
store driver: bounded %STORE-TICK yields — no sleep) and return its
resolved/rejected value (:cancelled for a cancel)."
  (let ((store (funcall (rsym "host-pending-store") pending)))
    (is (wait-until (lambda ()
                      (funcall (rsym "%store-tick") store)
                      (funcall (rsym "host-pending-terminal-p") pending))))
    (funcall (rsym "%host-pump") store)
    (let ((outcome (funcall (rsym "host-pending-outcome") pending)))
      (ecase (car outcome)
        (:resolve (cdr outcome))
        (:reject (cdr outcome))
        (:cancel :cancelled)))))

;;; ---------------------------------------------------------------
;;; M17 plan step 3: the provider set is the RESOLVED WORLD's import
;;; set. `command-provider' no longer appends a hand-written list;
;;; `compose-world-providers' walks the imports of a world resolved by
;;; `wasmdown.wit:resolve-wit-world' (gates applied, references
;;; resolved, includes expanded) and builds exactly those providers.

(defun %world-import-names (world)
  "The import names COMPOSE-WORLD-PROVIDERS binds for WORLD."
  (mapcar #'car (wasmdown.wasi:compose-world-providers
                 world (wasmdown.wasi:make-wasi-context))))

(defun %sorted (list)
  (sort (copy-list list) #'string<))

(test wasi/command-provider-composed-from-command-world
  ;; The resolved `wasi:cli/command' world composes EXACTLY the set the
  ;; pre-cutover list-driven composition produced (names compared as a
  ;; set: declaration order is the world's, never a runtime order).
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (world (wasmdown.wasi:command-world))
         (today (append (mapcar #'car (wasmdown.wasi:cli-provider ctx))
                        (mapcar #'car (wasmdown.wasi:clocks-provider))
                        (mapcar #'car (wasmdown.wasi:random-provider))
                        (mapcar #'car (wasmdown.wasi:filesystem-provider ctx))
                        (mapcar #'car (wasmdown.wasi:sockets-provider ctx)))))
    (multiple-value-bind (providers unbound)
        (wasmdown.wasi:compose-world-providers world ctx)
      (is (= 20 (length providers)))
      (is (= 20 (length today)))
      (is (equal (%sorted (mapcar #'car providers)) (%sorted today)))
      (is (every #'wasmdown.component.runtime:comp-instance-p
                 (mapcar #'cdr providers)))
      ;; the world declares 21 imports: `wasi:cli/types@0.3.2' is
      ;; types-only (one enum), so it contributes no host instance and
      ;; is reported as unbound instead of an invented provider
      (is (equal '("wasi:cli/types@0.3.2") unbound))
      (is (member "wasi:cli/types@0.3.2" unbound :test #'string=))
      (is (not (member "wasi:cli/types@0.3.2" (mapcar #'car providers)
                       :test #'string=))))
    ;; COMMAND-PROVIDER is that same composition
    (is (equal (%sorted (mapcar #'car (wasmdown.wasi:command-provider ctx)))
               (%sorted today)))
    ;; and it accepts a caller-resolved world (the selection/packages
    ;; entry point) rather than resolving behind the caller's back
    (is (equal (%sorted (mapcar #'car
                                (wasmdown.wasi:command-provider
                                 ctx :world world)))
               (%sorted today)))))

(test wasi/world-provider-composition-is-world-driven
  ;; Two declared imports give two providers; removing ONE declared
  ;; import (from the resolved world) removes exactly that provider and
  ;; nothing else; a one-import world composes exactly one provider.
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (pkgs (wasmdown.wasi::%wasi-wit-packages))
         (pkg (wasmdown.wit:parse-wit-string
               "package test:mini@0.1.0;

world mini {
  import wasi:clocks/monotonic-clock@0.3.2;
  import wasi:clocks/system-clock@0.3.2;
  import wasi:random/insecure@0.3.2;
}
"))
         (world (wasmdown.wit:resolve-wit-world
                 pkg "mini" :packages pkgs)))
    (is (equal '("wasi:clocks/monotonic-clock@0.3.2"
                 "wasi:clocks/system-clock@0.3.2"
                 "wasi:random/insecure@0.3.2")
               (%world-import-names world)))
    ;; exactly one provider disappears, the other two keep their
    ;; instances (a re-composition, so this proves filtering by world
    ;; declaration, not a prefix/list slice)
    (setf (wasmdown.wit:wit-world-imports world)
          (remove "system-clock" (wasmdown.wit:wit-world-imports world)
                  :key #'wasmdown.wit:wit-world-item-interface
                  :test #'string=))
    (let ((providers (wasmdown.wasi:compose-world-providers world ctx)))
      (is (equal '("wasi:clocks/monotonic-clock@0.3.2"
                   "wasi:random/insecure@0.3.2")
                 (mapcar #'car providers)))
      (is (every #'wasmdown.component.runtime:comp-instance-p
                 (mapcar #'cdr providers))))
    ;; a world with a single declared import composes a single provider
    (let* ((one (wasmdown.wit:parse-wit-string
                 "package test:one@0.1.0;

world one {
  import wasi:sockets/ip-name-lookup@0.3.2;
}
"))
           (w (wasmdown.wit:resolve-wit-world one "one" :packages pkgs)))
      (is (equal '("wasi:sockets/ip-name-lookup@0.3.2")
                 (%world-import-names w))))
    ;; an empty world imports nothing: no provider at all
    (let* ((none (wasmdown.wit:parse-wit-string
                  "package test:none@0.1.0;

world none {
}
"))
           (w (wasmdown.wit:resolve-wit-world none "none")))
      (multiple-value-bind (providers unbound)
          (wasmdown.wasi:compose-world-providers w ctx)
        (is (null providers))
        (is (null unbound))))))

(test wasi/world-provider-types-only-import-needs-no-binding
  ;; A types-only import the runtime has no entry for needs no host
  ;; instance: it composes no entry and is reported as unbound. The
  ;; command world's `wasi:cli/types@0.3.2' is the real case; a
  ;; synthetic world pins it in isolation. `wasi:clocks/types@0.3.2'
  ;; IS in the runtime's surface (it has always been in the command
  ;; provider set), so it still composes its type-only instance.
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (pkgs (wasmdown.wasi::%wasi-wit-packages))
         (pkg (wasmdown.wit:parse-wit-string
               "package test:typesonly@0.1.0;

world t {
  import wasi:cli/types@0.3.2;
  import wasi:clocks/types@0.3.2;
}
"))
         (world (wasmdown.wit:resolve-wit-world pkg "t" :packages pkgs)))
    (multiple-value-bind (providers unbound)
        (wasmdown.wasi:compose-world-providers world ctx)
      (is (equal '("wasi:clocks/types@0.3.2") (mapcar #'car providers)))
      (is (wasmdown.component.runtime:comp-instance-p (cdr (first providers))))
      (is (equal '("wasi:cli/types@0.3.2") unbound)))
    ;; cli/types alongside a function-bearing import: the types-only
    ;; one still composes nothing, the other still composes
    (let* ((mixed (wasmdown.wit:parse-wit-string
                   "package test:mixed@0.1.0;

world m {
  import wasi:cli/types@0.3.2;
  import wasi:clocks/system-clock@0.3.2;
}
"))
           (w (wasmdown.wit:resolve-wit-world mixed "m" :packages pkgs)))
      (multiple-value-bind (providers unbound)
          (wasmdown.wasi:compose-world-providers w ctx)
        (is (equal '("wasi:clocks/system-clock@0.3.2")
                   (mapcar #'car providers)))
        (is (equal '("wasi:cli/types@0.3.2") unbound))))))

(test wasi/world-provider-unsupported-import-is-construction-error
  ;; A declared import this runtime has no provider for is a
  ;; CONSTRUCTION-TIME wit-error naming the interface — never a
  ;; trapping stub installed as a successful binding.
  (let ((ctx (wasmdown.wasi:make-wasi-context))
        (feature (wasmdown.wit:make-wit-selection
                  :enabled-features '("clocks-timezone"))))
    ;; the real declaration: `wasi:cli/command' imports timezone only
    ;; under the clocks-timezone feature, and this runtime has no
    ;; timezone provider
    (let* ((world (wasmdown.wasi:command-world :selection feature))
           (item (find "timezone"
                       (wasmdown.wit:wit-world-imports world)
                       :key #'wasmdown.wit:wit-world-item-interface
                       :test #'string=))
           (message nil))
      (is item)
      (handler-case (wasmdown.wasi:compose-world-providers world ctx)
        (wasmdown.wit:wit-error (c)
          (setf message (wasmdown:wasmdown-error-message c))))
      (is message)
      ;; the message names the world and the exact import
      (is (search "command" message))
      (is (search "wasi:clocks/timezone@0.3.2" message))
      (is (search "no provider" message))
      ;; and it is raised while COMPOSING: no partial graph escapes,
      ;; and the providers that WOULD have composed are never bound
      (is (handler-case
              (progn (wasmdown.wasi:command-provider ctx :world world) nil)
            (wasmdown.wit:wit-error () t))))
    ;; an inline interface import with a function: no provider exists
    ;; for it either, and the error names the binding
    (let* ((pkg (wasmdown.wit:parse-wit-string
                 "package test:inline@0.1.0;

world w {
  import bogus: interface {
    ping: func();
  }
}
"))
           (world (wasmdown.wit:resolve-wit-world pkg "w"))
           (message nil))
      (handler-case (wasmdown.wasi:compose-world-providers world ctx)
        (wasmdown.wit:wit-error (c)
          (setf message (wasmdown:wasmdown-error-message c))))
      (is message)
      (is (search "bogus" message)))))

(test wasi/world-provider-http-import-composed
  ;; `wasi:http/types' + `wasi:http/client' are served (M16 slice 5,
  ;; restored): a world that declares them gets their providers from
  ;; COMPOSE-WORLD-PROVIDERS, under the world's own import names.
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (pkgs (wasmdown.wasi::%wasi-wit-packages))
         (pkg (wasmdown.wit:parse-wit-string
               "package test:httpworld@0.1.0;

world h {
  import wasi:http/types@0.3.2;
  import wasi:http/client@0.3.2;
}
"))
         (world (wasmdown.wit:resolve-wit-world pkg "h" :packages pkgs)))
    (is (equal '("wasi:http/types@0.3.2" "wasi:http/client@0.3.2")
               (mapcar #'wasmdown.wasi::%world-import-name
                       (wasmdown.wit:wit-world-imports world))))
    (multiple-value-bind (providers unbound)
        (wasmdown.wasi:compose-world-providers world ctx)
      (is (null unbound))
      (is (equal '("wasi:http/types@0.3.2" "wasi:http/client@0.3.2")
                 (mapcar #'car providers)))
      (dolist (pair providers)
        (is (wasmdown.component.runtime:comp-instance-p (cdr pair)))))
    ;; the pinned command world imports no http itself, so the plain
    ;; command provider still binds none (a DECLARING component gets
    ;; them from COMPONENT-COMMAND-PROVIDERS instead)
    (is (notany (lambda (name) (eql 0 (search "wasi:http" name)))
                (mapcar #'car (wasmdown.wasi:command-provider ctx))))))

;;; -----------------------------------------------------------------
;;; wasi:sockets is a declared guest capability on the 0.3 line: a
;;; component that declares `wasi:sockets/types' or
;;; `wasi:sockets/ip-name-lookup' is bound to the same provider the
;;; pinned command world uses, and every operation is still gated by
;;; the explicit network authority in its context (no ambient access).

(defun %synthetic-import-comp (&rest names)
  "A decoded-component stand-in declaring exactly NAMES as imports.
COMPONENT-COMMAND-PROVIDERS reads only the declared import names, so
the payloads need no resolvable extern types."
  (wasmdown.component:make-comp
   :defs (coerce
          (loop for name in names
                collect (wasmdown.component:make-compdef
                         :kind :import
                         :payload (wasmdown.component:make-importrec
                                   :name name
                                   :attrs nil
                                   :type (list :instance 0))
                         :offset 0))
          'vector)))

(test wasi/component-sockets-declared-imports-bind-providers
  ;; a 0.3 component declaring a sockets interface composes without a
  ;; refusal and gets that name bound to a real provider instance. The
  ;; whole 0.3 command world is still the graph the path composes, so
  ;; the check is that the declared names the guest can see are present
  ;; and usable, not merely that an error is absent.
  (let ((ctx (wasmdown.wasi:make-wasi-context
              :networks (list (sock-network)))))
    (dolist (name '("wasi:sockets/types@0.3.2"
                    "wasi:sockets/ip-name-lookup@0.3.2"))
      (let* ((providers (wasmdown.wasi:component-command-providers
                         ctx (%synthetic-import-comp name)))
             (provider (cdr (assoc name providers :test #'string=))))
        (is provider)
        (is (wasmdown.component.runtime:comp-instance-p provider))))))

(test wasi/component-declared-sockets-check-does-not-overreach
  ;; positive control: a component declaring only `wasi:cli/exit@0.3.2'
  ;; still composes the command world; lookalike package names are not
  ;; mistaken for `wasi:sockets' declarations.
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (expected (length (wasmdown.wasi:command-provider ctx))))
    (dolist (names '(("wasi:cli/exit@0.3.2")
                     ("wasi:socketsx/types@0.3.2")
                     ("my:wasi:sockets/types@0.3.2")
                     ("wasi:cli/exit@0.3.2" "wasi:socketsx/types@0.3.2")))
      (let ((providers (wasmdown.wasi:component-command-providers
                        ctx (apply #'%synthetic-import-comp names))))
        (is (= expected (length providers)))
        (is (every #'wasmdown.component.runtime:comp-instance-p
                   (mapcar #'cdr providers)))))))

(test wasi/component-mixed-lines-compose-one-graph
  ;; A component declaring BOTH release lines gets ONE composed graph:
  ;; the 0.3-line declared imports from the 0.3 command builders and the
  ;; exact-version 0.2 imports from the P2 tables, in declaration order,
  ;; under one with-args alist. Nothing the component did not declare is
  ;; bound (the 0.3 command world is NOT composed wholesale here).
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (names '("wasi:cli/environment@0.3.2"
                  "wasi:cli/types@0.3.2"
                  "wasi:clocks/monotonic-clock@0.3.2"
                  "wasi:io/poll@0.2.9"
                  "wasi:io/streams@0.2.9"
                  "wasi:cli/stdout@0.2.9"
                  "wasi:cli/environment@0.2.9"))
         (providers (wasmdown.wasi:component-command-providers
                     ctx (apply #'%synthetic-import-comp names))))
    ;; every provider is bound under an EXACT declared name, in order;
    ;; only the types-only 0.3 import composes no entry
    (is (equal '("wasi:cli/environment@0.3.2"
                 "wasi:clocks/monotonic-clock@0.3.2"
                 "wasi:io/poll@0.2.9"
                 "wasi:io/streams@0.2.9"
                 "wasi:cli/stdout@0.2.9"
                 "wasi:cli/environment@0.2.9")
               (mapcar #'car providers)))
    (is (every #'wasmdown.component.runtime:comp-instance-p
               (mapcar #'cdr providers)))
    ;; both lines are real providers with DISTINCT instances: the same
    ;; short interface at two release lines is never one graph reused
    (let ((env3 (cdr (assoc "wasi:cli/environment@0.3.2" providers
                            :test #'string=)))
          (env2 (cdr (assoc "wasi:cli/environment@0.2.9" providers
                            :test #'string=))))
      (is env3)
      (is env2)
      (is (not (eq env3 env2))))
    ;; nothing beyond the declared contract is bound
    (dolist (undeclared '("wasi:sockets/types@0.3.2"
                          "wasi:sockets/ip-name-lookup@0.3.2"
                          "wasi:filesystem/types@0.3.2"
                          "wasi:filesystem/preopens@0.3.2"
                          "wasi:random/random@0.3.2"))
      (is (not (assoc undeclared providers :test #'string=))))))

(test wasi/component-mixed-lines-keep-declaration-order
  ;; The composed alist follows the component's own declaration order,
  ;; interleaving the two lines: the order is not per-line grouping.
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (providers (wasmdown.wasi:component-command-providers
                     ctx (%synthetic-import-comp
                          "wasi:io/poll@0.2.9"
                          "wasi:clocks/system-clock@0.3.2"
                          "wasi:cli/stdout@0.2.9"
                          "wasi:random/random@0.3.2"))))
    (is (equal '("wasi:io/poll@0.2.9"
                 "wasi:clocks/system-clock@0.3.2"
                 "wasi:cli/stdout@0.2.9"
                 "wasi:random/random@0.3.2")
               (mapcar #'car providers)))))

(test wasi/component-mixed-sockets-declared-imports-bind-providers
  ;; Admission of the mixed graph also admits a 0.3 sockets
  ;; declaration: the 0.3 subset binds it through the sockets builder
  ;; while the 0.2 import still comes from the P2 table, in declaration
  ;; order, under one graph.
  (let ((ctx (wasmdown.wasi:make-wasi-context
              :networks (list (sock-network)))))
    (dolist (name '("wasi:sockets/types@0.3.2"
                    "wasi:sockets/ip-name-lookup@0.3.2"))
      (let* ((providers (wasmdown.wasi:component-command-providers
                         ctx (%synthetic-import-comp
                              name "wasi:io/streams@0.2.9")))
             (provider (cdr (assoc name providers :test #'string=))))
        (is provider)
        (is (wasmdown.component.runtime:comp-instance-p provider))
        (is (equal (list name "wasi:io/streams@0.2.9")
                   (mapcar #'car providers)))))))

(test wasi/component-mixed-http-declared-import
  ;; In a mixed component the 0.3 `wasi:http' declaration composes its
  ;; providers (restored slice 5), while the 0.2 declaration still has
  ;; no P2 provider and keeps the canonical 0.2-line refusal.
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    ;; the 0.3 declaration composes: the http types instance is bound
    ;; next to the P2 streams provider
    (let ((providers
            (wasmdown.wasi:component-command-providers
             ctx (%synthetic-import-comp "wasi:http/types@0.3.2"
                                         "wasi:io/streams@0.2.9"))))
      (is (equal '("wasi:http/types@0.3.2" "wasi:io/streams@0.2.9")
                 (mapcar #'car providers)))
      (is (wasmdown.component.runtime:comp-instance-p
           (cdr (assoc "wasi:http/types@0.3.2" providers :test #'string=)))))
    ;; the 0.2 declaration is refused by the P2 table's own exclusion
    (let ((message nil))
      (handler-case
          (progn (wasmdown.wasi:component-command-providers
                  ctx (%synthetic-import-comp "wasi:http/types@0.2.12"
                                              "wasi:clocks/system-clock@0.3.2"))
                 nil)
        (wasmdown.wit:wit-error (c)
          (setf message (wasmdown:wasmdown-error-message c))))
      (is message)
      (is (string= wasmdown.wasi::+wasi-http-unsupported-message+ message))
      (is (not (search "no provider" message))))))

(test wasi/component-mixed-unstable-declared-import-still-refused
  ;; the 0.3 @unstable gate keeps applying to the 0.3 subset of a mixed
  ;; component: timezone names import and feature, and never composes.
  (let ((condition nil))
    (handler-case
        (progn (wasmdown.wasi:component-command-providers
                (wasmdown.wasi:make-wasi-context)
                (%synthetic-import-comp
                 "wasi:clocks/timezone@0.3.2" "wasi:io/poll@0.2.9"))
               nil)
      (wasmdown:link-error (c) (setf condition c)))
    (is condition)
    (let ((message (wasmdown:wasmdown-error-message condition)))
      (is (search "wasi:clocks/timezone@0.3.2" message))
      (is (search "@unstable(feature = clocks-timezone)" message))
      (is (not (search "mixes WASI release lines" message))))))

(test wasi/component-mixed-unknown-line-import-is-construction-error
  ;; no provider is INVENTED for the mixed graph: a declared import the
  ;; 0.3 builders do not serve (an unknown interface or a version the
  ;; pinned packages do not contain) stays a construction-time
  ;; WIT-ERROR naming it, alongside a served 0.2 import.
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (dolist (name '("wasi:filesystem/types@0.3.0"
                    "wasi:bogus/x@0.3.2"
                    "wasi:clocks/monotonic-clock"))
      (let ((message nil))
        (handler-case
            (progn (wasmdown.wasi:component-command-providers
                    ctx (%synthetic-import-comp name "wasi:io/poll@0.2.9"))
                   nil)
          (wasmdown.wit:wit-error (c)
            (setf message (wasmdown:wasmdown-error-message c))))
        (is message)
        (is (search name message))
        (is (search "no provider" message))))))

(test wasi/component-pure-lines-are-unchanged-by-mixed-support
  ;; Regression: a 0.2-only component still gets exactly the P2 graph for
  ;; its declared imports (no 0.3 entry), and a 0.3-only component still
  ;; gets the whole command world entry for entry.
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (let* ((names '("wasi:io/poll@0.2.9" "wasi:cli/stdout@0.2.9"))
           (providers (wasmdown.wasi:component-command-providers
                       ctx (apply #'%synthetic-import-comp names)))
           (expected (wasmdown.wasi:preview2-command-provider
                      ctx :version "0.2.9" :imports names)))
      (is (equal (sort (mapcar #'car providers) #'string<)
                 (sort (mapcar #'car expected) #'string<)))
      (is (notany (lambda (pair) (search "@0.3" (car pair))) providers)))
    (let ((providers (wasmdown.wasi:component-command-providers
                      ctx (%synthetic-import-comp
                           "wasi:clocks/system-clock@0.3.2"))))
      (is (equal (mapcar #'car (wasmdown.wasi:command-provider ctx))
                 (mapcar #'car providers))))))

(test wasi/component-unstable-declared-import-is-link-error
  ;; a 0.3 component DECLARING `wasi:clocks/timezone@0.3.2' is refused
  ;; before composition: the interface is @unstable(feature =
  ;; clocks-timezone) and the default selection enables no feature. The
  ;; message names the import and the feature; it is never the generic
  ;; no-provider text.
  (let ((ctx (wasmdown.wasi:make-wasi-context))
        (condition nil))
    (handler-case
        (progn (wasmdown.wasi:component-command-providers
                ctx (%synthetic-import-comp "wasi:clocks/timezone@0.3.2"))
               nil)
      (wasmdown:link-error (c) (setf condition c)))
    (is condition)
    (let ((message (wasmdown:wasmdown-error-message condition)))
      (is (search "wasi:clocks/timezone@0.3.2" message))
      (is (search "@unstable(feature = clocks-timezone)" message))
      (is (search "not enabled" message))
      (is (search "outside the selected stable WASI 0.3.2 surface"
                  message))
      (is (not (search "no provider" message))))))

(test wasi/component-stable-declared-import-is-not-gated
  ;; positive control: the stable clocks interfaces a command genuinely
  ;; declares are NOT refused; composition equals the plain command
  ;; provider set, entry for entry.
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (dolist (name '("wasi:clocks/monotonic-clock@0.3.2"
                    "wasi:clocks/system-clock@0.3.2"
                    "wasi:clocks/types@0.3.2"))
      (let ((providers (wasmdown.wasi:component-command-providers
                        ctx (%synthetic-import-comp name))))
        (is (equal (mapcar #'car (wasmdown.wasi:command-provider ctx))
                   (mapcar #'car providers)))
        (is (every #'wasmdown.component.runtime:comp-instance-p
                   (mapcar #'cdr providers)))))))

(test wasi/component-unknown-declared-import-keeps-existing-behavior
  ;; unknown names are not classified as unstable and are never
  ;; refused by the surface gate: the 0.3 path composes the command
  ;; world exactly as before.
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (expected (mapcar #'car (wasmdown.wasi:command-provider ctx))))
    (dolist (name '("wasi:clocks/timezone-x@0.3.2"
                    "nonsense"))
      (multiple-value-bind (status interface feature)
          (wasmdown.wit:wit-import-surface-status
           name (wasmdown.wasi::%wasi-wit-packages))
        (is (eq :unknown status))
        (is (null interface))
        (is (null feature)))
      (let ((providers (wasmdown.wasi:component-command-providers
                        ctx (%synthetic-import-comp name))))
        (is (equal expected (mapcar #'car providers)))))))

(test wasi/cli-network-options-parse-into-authority
  ;; `--net'/`--listen'/`--dns' parse in command-line order and become
  ;; the context's ONE explicit network authority. A destination the
  ;; command line did not declare is denied — the policy is not widened
  ;; by parsing a sibling endpoint.
  (multiple-value-bind (file invokes options guest-args)
      (wasmdown.cli::%parse-run-args
       (list "app.wasm" "--wasi" "--net" "127.0.0.1:8080"
             "--listen" "*:*" "--dns" "*" "guest-arg"))
    (declare (ignore invokes))
    (is (string= "app.wasm" file))
    (is (equal '("guest-arg") guest-args))
    (is (equal '(("--wasi" . t)
                 ("--net" . "127.0.0.1:8080")
                 ("--listen" . "*:*")
                 ("--dns" . "*"))
               options))
    (multiple-value-bind (context reactor)
        (wasmdown.cli::%component-context options guest-args file)
      (unwind-protect
           (let ((networks (wasmdown.wasi:wasi-context-networks context))
                 (net (first (wasmdown.wasi:wasi-context-networks context))))
             (is (= 1 (length networks)))
             (is net)
             ;; the declared outbound endpoint is allowed ...
             (is (wasmdown.wasi:wasi-network-allows-outbound-p
                  net :ipv4 '(127 0 0 1) 8080))
             ;; ... while an undeclared port or host is denied
             (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
                       net :ipv4 '(127 0 0 1) 8081)))
             (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
                       net :ipv4 '(127 0 0 2) 8080)))
             ;; `*:*' listen and `*' DNS are the explicit grants given
             (is (wasmdown.wasi:wasi-network-allows-listen-p
                  net :ipv4 '(0 0 0 0) 0))
             (is (wasmdown.wasi:wasi-network-allows-dns-p
                  net "anything.test")))
        (when reactor (ignore-errors (wasmdown.wasi:reactor-close reactor)))))))

(test wasi/cli-network-options-default-and-errors
  ;; with no network option the context carries NO authority (deny-all),
  ;; and --dir/--env keep their behavior
  (multiple-value-bind (file invokes options guest-args)
      (wasmdown.cli::%parse-run-args
       (list "app.wasm" "--wasi" "--env" "HOME=/tmp" "--dir" "/tmp"
             "guest-arg"))
    (declare (ignore invokes))
    (multiple-value-bind (context reactor)
        (wasmdown.cli::%component-context options guest-args file)
      (unwind-protect
           (progn
             (is (null (wasmdown.wasi:wasi-context-networks context)))
             (is (equal '("HOME=/tmp")
                        (mapcar (lambda (pair)
                                  (format nil "~a=~a"
                                          (car pair) (cdr pair)))
                                (wasmdown.wasi:wasi-context-environment
                                 context))))
             (is (plusp (length (wasmdown.wasi:wasi-context-preopens
                                 context)))))
        (dolist (preopen (wasmdown.wasi:wasi-context-preopens context))
          (ignore-errors (wasmdown.wasi:close-wasi-preopen preopen)))
        (when reactor (ignore-errors (wasmdown.wasi:reactor-close reactor))))))
  ;; a missing option value is a usage error at parse time
  (handler-case
      (progn (wasmdown.cli::%parse-run-args
              (list "run" "app.wasm" "--wasi" "--net"))
             (is nil "missing --net value was accepted"))
    (wasmdown.cli::usage-error () t))
  ;; network options without --wasi are a usage error
  (multiple-value-bind (file invokes options guest-args)
      (wasmdown.cli::%parse-run-args
       (list "app.wasm" "--net" "127.0.0.1:1"))
    (declare (ignore invokes))
    (handler-case
        (progn (wasmdown.cli::%component-context options guest-args file)
               (is nil "network options without --wasi were accepted"))
      (wasmdown.cli::usage-error () t)))
  ;; a malformed endpoint is rejected while the authority is built
  (multiple-value-bind (file invokes options guest-args)
      (wasmdown.cli::%parse-run-args
       (list "app.wasm" "--wasi" "--net" "nonsense"))
    (declare (ignore invokes))
    (handler-case
        (progn (wasmdown.cli::%component-context options guest-args file)
               (is nil "malformed endpoint was accepted"))
      (wasmdown:host-error () t))))

(test wasi/cli-help-documents-network-options
  (let ((help (with-output-to-string (s)
                (wasmdown.cli::%usage s))))
    (is (search "--net HOST:PORT" help))
    (is (search "--listen ADDR:PORT" help))
    (is (search "--dns NAME" help))
    (is (not (search "refused" help)))))

;;; -----------------------------------------------------------------
;;; Backend evidence for a command component (M9): the nested core the
;;; component instantiates runs through the SELECTED backend, and the
;;; counter proves it — a mode that silently interpreted would show a
;;; zero entry count here.

(test wasi/command-component-runs-the-selected-backend
  (dolist (mode '(:interpret :compiled))
    (let* ((sink (make-string-output-stream))
           (ctx (wasmdown.wasi:make-wasi-context :stdout sink)))
      (wasmdown.runtime:reset-backend-entry-counts)
      (is (eql 0 (wasmdown.wasi:run-component
                  (wasmdown.component:decode-component
                   (command-component-binary))
                  ctx :mode mode)))
      (is (string= "hi" (get-output-stream-string sink)))
      (if (eq mode :compiled)
          (progn
            (is (plusp (wasmdown.runtime:backend-entry-count :compiled)))
            (is (zerop (wasmdown.runtime:backend-entry-count :interpret))))
          (progn
            (is (plusp (wasmdown.runtime:backend-entry-count :interpret)))
            (is (zerop (wasmdown.runtime:backend-entry-count :compiled))))))))

(test wasi/pinned-p3-guest-runs-both-backends
  ;; the tracked builder writes this file; when it is present (a fixture
  ;; build ran) the pinned bytes must be a working command in BOTH modes.
  (let ((path (merge-pathnames "tests/fixtures/wasi/p3-guest.wasm"
                               (make-pathname
                                :directory
                                (butlast (pathname-directory
                                          *load-truename*))))))
    (when (probe-file path)
      (let ((bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                     (let ((v (make-array (file-length s)
                                          :element-type '(unsigned-byte 8))))
                       (read-sequence v s) v))))
        (dolist (mode '(:interpret :compiled))
          (is (eql 0 (wasmdown.wasi:run-component
                      (wasmdown.component:decode-component bytes)
                      (wasmdown.wasi:make-wasi-context) :mode mode))))))))

;;; ---------------------------------------------------------------
;;; The mixed-release graph and the host behaviours the upstream
;;; wasip3 lane (tests/wasi-upstream-results.json) pins.

(test wasi/insecure-seed-is-stable-per-provider-instance
  ;; wasi:random/insecure-seed is a value in every practical sense: the
  ;; WIT says it is called once and will become a value import, the
  ;; reference runtime answers a second call with the first value, and
  ;; the upstream wasip3 random test asserts both halves. ONE provider
  ;; instance therefore answers one seed on every call.
  (let ((instance (wasmdown.wasi:insecure-seed-instance)))
    (let ((a (call-provider instance "get-insecure-seed"))
          (b (call-provider instance "get-insecure-seed")))
      (is (equal a b))
      (is (= 2 (length a)))
      (is (every (lambda (u) (and (integerp u) (<= 0 u) (< u (ash 1 64))))
                 a)))))

(test wasi/fs-stat-reports-distinct-timestamps
  ;; descriptor-stat's three instant fields are the stat struct's own
  ;; atime/mtime/ctime slots. A record that echoed one timestamp for
  ;; all three cannot answer the upstream filesystem-stat test (which
  ;; sets access 42 and modification 69 and reads them back).
  (let ((record (wasmdown.wasi::%stat-record 1 2 0 1 0 69 42 7)))
    (flet ((seconds (key)
             ;; the field is (:some (seconds . N) (nanoseconds . 0))
             (cdr (assoc "seconds"
                         (cddr (assoc key record :test #'string=))
                         :test #'string=))))
      (=> 42 (seconds "data-access-timestamp"))
      (=> 69 (seconds "data-modification-timestamp"))
      (=> 7 (seconds "status-change-timestamp")))))

(test wasi/fs-host-contract
  ;; The host-side rules the newly admitted upstream filesystem tests
  ;; pin, exercised through the provider entry points:
  ;;  * a DIRECTORY's get-flags never reports the file-only `write'
  ;;    mode, and a directory opened with no flags reports `read';
  ;;  * advise on a directory is bad-descriptor;
  ;;  * a filesize beyond off_t is file-too-large, not a host error;
  ;;  * CREATE implies WRITE in the effective access mode.
  (with-fs-root (root "fs-host-contract" '("plain.txt"))
    (fs-write! (fs-p root "plain.txt") "x")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; a read/write preopen directory: READ | MUTATE_DIRECTORY
             (let ((flags (cdr (fs-ok-value
                                (wasmdown.wasi:fs-get-flags ctx d)))))
               (is (member "read" flags :test #'string=))
               (is (member "mutate-directory" flags :test #'string=))
               (is (not (member "write" flags :test #'string=)))
               (is (= 1 (count "read" flags :test #'string=))))
             ;; opened with no descriptor flags: read, never `write'
             (let* ((opened (fs-ok-value
                             (wasmdown.wasi:fs-open-at ctx d nil "." nil nil)))
                    (dir-desc (third opened)))
               (=> '("read")
                   (cdr (fs-ok-value
                         (wasmdown.wasi:fs-get-flags ctx dir-desc)))))
             ;; advise is file-only: a directory is bad-descriptor
             (=> "bad-descriptor"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-advise ctx d 0 0 (cons :enum "normal"))))
             ;; CREATE with no descriptor flags: READ | WRITE, and a
             ;; size off_t cannot hold is file-too-large
             (let* ((opened (fs-ok-value
                             (wasmdown.wasi:fs-open-at
                              ctx d nil "made.txt"
                              (cons :flags '("create")) nil)))
                    (made (third opened)))
               (=> '("read" "write")
                   (cdr (fs-ok-value
                         (wasmdown.wasi:fs-get-flags ctx made))))
               (=> "file-too-large"
                   (fs-error-code-of
                    (wasmdown.wasi:fs-set-size ctx made (1- (ash 1 64)))))))
        (wasmdown.wasi:close-wasi-preopen pre)
        (ignore-errors (delete-file (fs-p root "made.txt")))))))

(test wasi/cli-argv0-is-the-file-name
  ;; argv[0] is the guest program's FILE NAME (POSIX's program name),
  ;; not the path handed to the CLI: the reference runtime does the same,
  ;; and the pinned upstream wasip3 cli-env test asserts `cli-env.wasm'.
  (dolist (file '("/tmp/some/dir/prog.wasm" "prog.wasm"))
    (multiple-value-bind (parsed-file invokes options guest-args)
        (wasmdown.cli::%parse-run-args
         (list file "--wasi" "--" "a" "b"))
      (declare (ignore invokes))
      (is (string= file parsed-file))
      (multiple-value-bind (ctx reactor)
          (wasmdown.cli::%component-context options guest-args file)
        (unwind-protect
             (=> '("prog.wasm" "a" "b")
                 (wasmdown.wasi::wasi-context-arguments ctx))
          (when reactor (wasmdown.wasi:reactor-close reactor)))))))
