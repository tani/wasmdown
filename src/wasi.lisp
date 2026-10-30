;;;; WASMDOWN — the WASI host-provider framework (PLAN.md §9, M16):
;;;; the WIT interface + an implementation alist -> a component
;;;; instance value the runtime can bind through with-args, plus the
;;;; wasi:clocks, wasi:random and wasi:cli providers (environment,
;;;; exit, and — slice 2 — the stdin/stdout/stderr streams over the
;;;; runtime's host-backed stream/future ends).
;;;;
;;;; Design contract:
;;;;   * a provider instance is a real comp-instance whose exports
;;;;     hash maps each WIT function name to a lifted-func carrying
;;;;     the WIT-derived component FUNCTYPE and a host closure. The
;;;;     runtime's canon-lower wrapper lifts/lowers the canonical ABI
;;;;     in the CALLER's memory, calls the closure with/returns the
;;;;     component VALUE domain (CL integers, strings, lists, records);
;;;;   * capabilities are an EXPLICIT wasi-context (PLAN.md §9.1);
;;;;     no provider reads ambient Lisp globals;
;;;;   * secure randomness is OS entropy (/dev/urandom), never
;;;;     CL:RANDOM;
;;;;   * every function/method a provided interface declares is
;;;;     implemented: a provider alist that misses one is a
;;;;     construction-time host-error naming the interface and the
;;;;     function — never a trapping stub (see PROVIDER COMPLETENESS
;;;;     below; the deliberately-unserved interfaces are listed there).

(in-package #:wasmdown.wasi)

;;; M16 slice 2 exports (kept here so the slice's surface travels with
;;; its implementation).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (find-package '#:wasmdown.wasi)))
    (dolist (name '("WASI-CONTEXT-STDIN" "WASI-CONTEXT-STDOUT"
                    "WASI-CONTEXT-STDERR" "CL-STREAM-SOURCE" "CL-STREAM-SINK"
                    "CLI-STDIN-INSTANCE" "CLI-STDOUT-INSTANCE"
                    "CLI-STDERR-INSTANCE" "CLI-OUTPUT-INSTANCE"
                    "CLI-STDIO-PROVIDER"
                    ;; the bounded host-executor service (step 6)
                    "MAKE-HOST-EXECUTOR" "HOST-EXECUTOR"
                    "WASI-CONTEXT-DNS-EXECUTOR" "WASI-CONTEXT-STDIO-EXECUTOR"
                    "CLOSE-WASI-CONTEXT-EXECUTORS"
                    "IP-NAME-LOOKUP-RESOLVE-ASYNC"))
      (export (intern name package) package))))

;;; ---------------------------------------------------------------
;;; Capability context.

(defstruct wasi-context
  "The explicit authority object a CLI provider is built from
(PLAN.md §9.1). ARGUMENTS is a list of strings, ENVIRONMENT an alist
of (name . value) strings, CWD a string or NIL, and STDIN/STDOUT/
STDERR the CL streams the stdio providers may use (or NIL: no such
capability — never an ambient global). PREOPENS is the filesystem
authority list: wasi-preopen objects (open host directory handles,
slice 3) — never a path-prefix string."
  (arguments nil)
  (environment nil)
  (cwd nil)
  (stdin nil)
  (stdout nil)
  (stderr nil)
  (preopens nil)
  ;; M16 slice 4: the network authorities (wasi-network objects). A
  ;; socket provider is built from ONE of them; every bind/connect/
  ;; resolve consults it. NIL = no network authority at all.
  (networks nil)
  ;; M17 step 6: the context's REACTOR (a wasi-reactor, or NIL = no
  ;; clock/time capability). monotonic-clock's wait-until/wait-for arm
  ;; their one-shot deadline here; the store driver owns when STEP
  ;; runs, exactly as it owns every other readiness pump.
  (reactor nil)
  ;; M17 step 6: the TWO bounded host-executor services (created on
  ;; first use, FIXED worker budgets, SEPARATE so a slow resolver
  ;; cannot starve blocking stdio). They are host services, not a
  ;; result FIFO: a worker's only way back is the store queue.
  (dns-exec nil)
  (stdio-exec nil))

;;; ---------------------------------------------------------------
;;; THE BOUNDED HOST EXECUTOR (plan step 6).
;;;
;;; getaddrinfo(3) and a borrowed BLOCKING stdio stream cannot be made
;;; nonblocking without changing an fd the embedder owns. They run on
;;; a small explicit worker service instead: a FIXED number of native
;;; worker threads, each taking one host thunk at a time.
;;;
;;; THE WORKER DISCIPLINE:
;;;   * a worker NEVER touches guest memory and NEVER calls guest code;
;;;     the only way back is the STORE's completion queue (a pending's
;;;     resolve, or host-end-ready for a parked copy);
;;;   * a job that is cancelled/closed is DETACHED from its completion
;;;     target and its late result is discarded (and, for DNS, the
;;;     getaddrinfo chain is still freed by %GETADDRINFO-CHAIN's own
;;;     unwind-protect);
;;;   * CLOSE NEVER JOINS: an un-cancellable job finishes into a
;;;     dropped result and its worker exits afterwards;
;;;   * DNS and stdio have SEPARATE services/budgets (never one pool).

(defparameter *dns-worker-count* 2
  "The DNS executor's FIXED worker budget.")
(defparameter *stdio-worker-count* 2
  "The blocked-stdio executor's FIXED worker budget (separate from DNS's).")

(defstruct (host-executor (:constructor %make-host-executor))
  (name "host-executor")
  (mutex (sb-thread:make-mutex :name "host-executor"))
  (condition (sb-thread:make-waitqueue :name "host-executor"))
  (jobs '())                           ; host thunks (LIFO)
  (worker-count 0)
  (active 0)
  (closed-p nil)
  (threads '()))

(defun %host-executor-loop (executor)
  "One worker: take a job, run it (its failure is the job's own
business), repeat until the executor is closed AND its queue drained."
  (loop
    (let ((job (sb-thread:with-recursive-lock ((host-executor-mutex executor))
                 (loop
                   (cond
                     ((and (null (host-executor-jobs executor))
                           (host-executor-closed-p executor))
                      (return-from %host-executor-loop nil))
                     ((host-executor-jobs executor)
                      (let ((j (pop (host-executor-jobs executor))))
                        (incf (host-executor-active executor))
                        (return j)))
                     (t
                      (sb-thread:condition-wait
                       (host-executor-condition executor)
                       (host-executor-mutex executor))))))))
      (unwind-protect
           (handler-case (funcall job) (error () nil))
        (sb-thread:with-recursive-lock ((host-executor-mutex executor))
          (decf (host-executor-active executor))
          (sb-thread:condition-broadcast
           (host-executor-condition executor)))))))

(defun make-host-executor (&key (name "host-executor") (workers 1))
  "An explicit worker service with WORKERS FIXED native threads. The
count is a budget decided at construction and never grows; two
services never share a queue. Jobs are host thunks."
  (unless (and (integerp workers) (plusp workers))
    (error 'wasmdown:host-error
           :message (format nil "host executor ~a needs a positive fixed worker count, not ~s"
                            name workers)))
  (let ((executor (%make-host-executor :name name :worker-count workers)))
    (setf (host-executor-threads executor)
          (loop for i below workers
                collect (sb-thread:make-thread
                         (lambda () (%host-executor-loop executor))
                         :name (format nil "~a-~d" name i))))
    executor))

(defun %executor-submit (executor job)
  "Queue JOB on EXECUTOR. Never runs it here and never blocks. NIL when
the executor is closed — the caller must then DETACH (a submit into a
closed service can never be answered)."
  (when executor
    (sb-thread:with-recursive-lock ((host-executor-mutex executor))
      (unless (host-executor-closed-p executor)
        (push job (host-executor-jobs executor))
        (sb-thread:condition-broadcast (host-executor-condition executor))
        t))))

(defun %executor-close (executor)
  "Close EXECUTOR: drop every QUEUED job (none started, so none holds
a resource) and let an in-flight job run to completion into a
DISCARDED result. Never joins — the store's close may not wait on an
un-cancellable worker."
  (when executor
    (sb-thread:with-recursive-lock ((host-executor-mutex executor))
      (setf (host-executor-closed-p executor) t
            (host-executor-jobs executor) nil)
      (sb-thread:condition-broadcast (host-executor-condition executor)))
    t))

(defvar *executor-service-lock* (sb-thread:make-mutex :name "wasi-executors"))
(defvar *default-dns-executor* nil
  "The DNS service for a provider built with a NIL context (a bare
host call / a unit test): same fixed budget, created on first use.")
(defvar *default-stdio-executor* nil
  "The blocked-stdio service for a NIL context (see *DEFAULT-DNS-EXECUTOR*).")

(defun wasi-context-dns-executor (context)
  "CONTEXT's DNS worker service (fixed *DNS-WORKER-COUNT* budget),
created on first use. A NIL context uses the process default."
  (sb-thread:with-recursive-lock (*executor-service-lock*)
    (if context
        (or (wasi-context-dns-exec context)
            (setf (wasi-context-dns-exec context)
                  (make-host-executor :name "wasi-dns"
                                      :workers *dns-worker-count*)))
        (or *default-dns-executor*
            (setf *default-dns-executor*
                  (make-host-executor :name "wasi-dns"
                                      :workers *dns-worker-count*))))))

(defun wasi-context-stdio-executor (context)
  "CONTEXT's blocked-stdio worker service (fixed
*STDIO-WORKER-COUNT* budget), created on first use."
  (sb-thread:with-recursive-lock (*executor-service-lock*)
    (if context
        (or (wasi-context-stdio-exec context)
            (setf (wasi-context-stdio-exec context)
                  (make-host-executor :name "wasi-stdio"
                                      :workers *stdio-worker-count*)))
        (or *default-stdio-executor*
            (setf *default-stdio-executor*
                  (make-host-executor :name "wasi-stdio"
                                      :workers *stdio-worker-count*))))))

(defun close-wasi-context-executors (context)
  "Detach CONTEXT's executor services (idempotent). Queued jobs are
dropped, an in-flight job finishes into a discarded result; nothing
is joined."
  (when context
    (sb-thread:with-recursive-lock (*executor-service-lock*)
      (let ((dns (wasi-context-dns-exec context))
            (stdio (wasi-context-stdio-exec context)))
        (setf (wasi-context-dns-exec context) nil
              (wasi-context-stdio-exec context) nil)
        (%executor-close dns)
        (%executor-close stdio))))
  t)

;;; ---------------------------------------------------------------
;;; Host exit.

(define-condition wasi-exit (wasmdown:wasmdown-error)
  ((status :initarg :status :initform 0 :reader wasi-exit-status))
  (:default-initargs :message "WASI exit")
  (:report (lambda (c s)
             (format s "WASI exit with status ~a" (wasi-exit-status c))))
  (:documentation "The wasi:cli/exit* functions do not return: they
signal this condition, which unwinds the invocation (the runtime's
sync-lift path re-raises it at the caller)."))

(defun wasi-exit-p (c) (typep c 'wasi-exit))

(defun %result-status (status)
  "The exit status of a `result` domain value: ok -> 0, err -> 1."
  (if (and (consp status) (eq (car status) :ok)) 0 1))

(defun %wasi-exit (&optional (status 0))
  (error 'wasi-exit :status status))

;;; ---------------------------------------------------------------
;;; The vendored WIT snapshot.

(defmacro %source-file ()
  "This source file's pathname, captured at COMPILE time (an ASDF
fasl loads from the cache, where *LOAD-TRUENAME* would point at the
fasl rather than the checkout)."
  (or *compile-file-truename* *load-truename* *default-pathname-defaults*))

(defparameter *wasi-wit-root*
  (merge-pathnames #p"../specs/wasi/wasi-wit-0.3.2/"
                   (%source-file))
  "The vendored WASI 0.3.2 WIT tree (specs/wasi/MANIFEST.md).")

(defun wasi-package-file (short-name)
  "The vendored file for the package whose short name is SHORT-NAME
(\"clocks\" -> clocks/clocks.wit)."
  (merge-pathnames (make-pathname :directory (list :relative short-name)
                                  :name short-name :type "wit")
                   *wasi-wit-root*))

(defvar *wasi-package-cache* (make-hash-table :test #'equal))

(defun wasi-package (short-name)
  "Parse (and cache) the vendored WIT package SHORT-NAME."
  (let* ((path (wasi-package-file short-name))
         (key (namestring path)))
    (or (gethash key *wasi-package-cache*)
        (setf (gethash key *wasi-package-cache*)
              (wasmdown.wit:parse-wit-file path)))))

;;; ---------------------------------------------------------------
;;; The host-provider framework.

(defun make-host-lifted-func (functype host-fn &key inst)
  "A lifted-func whose invocation is HOST-FN (a list of component
domain args -> one component domain value). INST must be a
comp-instance: the runtime's sync-lift context reads its handle and
back-pressure tables even for a host callee."
  (wasmdown.component.runtime:make-lifted-func
   :core nil :type functype :opts nil :memory nil :realloc nil
   :inst inst :host host-fn))

;;; ---------------------------------------------------------------
;;; PROVIDER COMPLETENESS (M17 audit).
;;;
;;; The audit enumerated, from the vendored 0.3.2 WIT, every function
;;; and resource METHOD of every interface this runtime provides, and
;;; compared it with the implementation alist each provider supplies:
;;; 89 declared, 89 implemented, 0 stubs.
;;;
;;;   clocks/monotonic-clock   4    random/random           2
;;;   clocks/system-clock      2    random/insecure         2
;;;   cli/environment          3    random/insecure-seed    1
;;;   cli/exit                 2    filesystem/types       25
;;;   cli/stdin|stdout|stderr  1+1+1    filesystem/preopens 1
;;;   cli/terminal-*          1+1+1    sockets/types         40
;;;                                   sockets/ip-name-lookup 1
;;;
;;; (clocks/types, cli/terminal-input and cli/terminal-output declare
;;; no functions.) Every gate on that surface is
;;; `@since(version = 0.3.0)', i.e. STABLE in this package — so no
;;; STABLE function of a provided interface is left unimplemented.
;;;
;;; What this runtime DELIBERATELY does not serve is therefore not a
;;; stub inside a provider instance: it is a whole interface with no
;;; provider at all, refused when a world asks for it
;;; (COMPOSE-WORLD-PROVIDERS names the import):
;;;   * wasi:clocks/timezone — iana-id, utc-offset, to-debug-string;
;;;     all three are `@unstable(feature = clocks-timezone)';
;;;   * wasi:http/types (35), wasi:http/client (1), wasi:http/handler
;;;     (1) — WASI HTTP was removed at user direction;
;;;   * wasi:cli/run — the command world's EXPORT (served by
;;;     COMPONENT-RUN-EXPORT), never an import to bind.
;;;
;;; PROVIDER-INSTANCE enforces the 89/89: an alist that does not cover
;;; a declared function is a construction-time HOST-ERROR naming the
;;; interface and the function. There is no stub factory, so the
;;; implemented set cannot shrink silently.

(defun %provider-missing (interface function-name)
  (error 'wasmdown:host-error
         :message (format nil
                          "WASI interface ~a declares ~a, but this provider ~
                           supplies no implementation for it (refusing to ~
                           export a trapping stub)"
                          (wasmdown.wit:wit-interface-name interface)
                          function-name)))

(defun %provider-closure (interface implementations name)
  "The host closure for declared function NAME over IMPLEMENTATIONS.
A missing entry is a construction-time error: an unimplemented
function is never exported as a trapping stub."
  (let ((impl (cdr (assoc name implementations :test #'string=))))
    (unless (or (functionp impl)
                (and (symbolp impl) (fboundp impl)))
      (%provider-missing interface name))
    (lambda (args) (apply impl args))))

(defun %resource-method-export-name (resource method)
  "The component-model core export name of a resource METHOD:
[constructor]R, [static]R.m, or [method]R.m."
  (let ((r (wasmdown.wit:wit-resource-name resource))
        (m (wasmdown.wit:wit-func-name method)))
    (cond
      ((wasmdown.wit:wit-func-constructor-p method)
       (format nil "[constructor]~a" r))
      ((wasmdown.wit:wit-func-static-p method)
       (format nil "[static]~a.~a" r m))
      (t (format nil "[method]~a.~a" r m)))))

(defun %resource-method-ct (interface resource method packages)
  "The component functype of RESOURCE's METHOD: the WIT signature
with the implicit `self: borrow<R>' prepended (CanonicalABI: a method
is a plain func whose first param is the self borrow). A CONSTRUCTOR
has no self, and its result is the implicit `own<R>' (the WIT's
`constructor();` carries no explicit result)."
  (let* ((ct (wasmdown.wit:wit-func->ct interface method :packages packages))
         (params (coerce (first (wasmdown.component:ct-payload ct)) 'list))
         (resource-ct
           (wasmdown.wit:wit-type->ct
            interface (list :resource (wasmdown.wit:wit-resource-name resource))
            :packages packages))
         (self (cons "self"
                     (wasmdown.component:make-ct :kind :borrow
                                                 :payload resource-ct))))
    (wasmdown.component:make-ct
     :kind :func
     :payload (list (coerce (if (or (wasmdown.wit:wit-func-static-p method)
                                    (wasmdown.wit:wit-func-constructor-p method))
                                params
                                (cons self params))
                            'vector)
                    (if (wasmdown.wit:wit-func-constructor-p method)
                        (wasmdown.component:make-ct :kind :own
                                                    :payload resource-ct)
                        (second (wasmdown.component:ct-payload ct)))
                    (wasmdown.wit:wit-func-async-p method)))))

(defun provider-instance (interface implementations &key packages)
  "Build a comp-instance for the parsed WIT INTERFACE. IMPLEMENTATIONS
is an alist ((\"now\" . fn) ...) of CL functions taking the component
domain arguments. Every function and resource method the INTERFACE
declares MUST be supplied — an absent entry is a construction-time
HOST-ERROR naming the interface and the function; it is never
exported as a trapping stub. The instance's export table also carries
the interface's TYPE definitions, so a consumer can alias them with
the runtime's type-projection path.

M16 slice 3: RESOURCE methods are exported under their component
core names ([method]R.m / [static]R.m / [constructor]R), can be
implemented either by that name or by the bare method name, and the
resource type is exported as (:type . ct) — the ct every occurrence
of the resource shares (the handle table's type identity)."
  (let ((inst (wasmdown.component.runtime:make-comp-instance)))
    (dolist (fn (wasmdown.wit:wit-interface-func-list interface))
      (let* ((fname (wasmdown.wit:wit-func-name fn))
             (ct (wasmdown.wit:wit-func->ct interface fn
                                            :packages packages))
             (closure (%provider-closure interface implementations fname)))
        (setf (gethash fname
                       (wasmdown.component.runtime:comp-instance-exports inst))
              (cons :func (make-host-lifted-func ct closure :inst inst)))))
    ;; M16 slice 3: the interface's RESOURCES — the type export first
    ;; (the shared ct), then each method as a func export.
    (dolist (res (wasmdown.wit:wit-interface-resource-list interface))
      (let* ((rname (wasmdown.wit:wit-resource-name res))
             (rct (wasmdown.wit:wit-type->ct
                   interface (list :resource rname) :packages packages)))
        (setf (gethash rname
                       (wasmdown.component.runtime:comp-instance-exports inst))
              (cons :type rct))
        (dolist (method (wasmdown.wit:wit-resource-methods res))
          (let* ((mname (wasmdown.wit:wit-func-name method))
                 (fname (%resource-method-export-name res method))
                 (ct (%resource-method-ct interface res method packages))
                 (key (cond ((assoc fname implementations :test #'string=)
                             fname)
                            ((assoc mname implementations :test #'string=)
                             mname)
                            (t fname)))
                 (closure (%provider-closure interface implementations key)))
            (setf (gethash fname
                           (wasmdown.component.runtime:comp-instance-exports
                            inst))
                  (cons :func (make-host-lifted-func ct closure
                                                     :inst inst)))))))
    ;; The interface's defined types (not resources: those are above)
    ;; are exported as (:type . ct).
    (dolist (item (wasmdown.wit:wit-interface-items interface))
      (when (wasmdown.wit:wit-type-p item)
        (let ((expr (wasmdown.wit:wit-type-expr item)))
          (unless (and (consp expr) (eq (car expr) :resource))
            (let ((ct (wasmdown.wit:wit-type->ct interface expr
                                                 :packages packages)))
              (when ct
                (setf (gethash (wasmdown.wit:wit-type-name item)
                               (wasmdown.component.runtime:comp-instance-exports
                                inst))
                      (cons :type ct))))))))
    inst))

(defun provider-instance-function (instance name)
  "The lifted-func exported by provider INSTANCE as NAME."
  (let ((hit (gethash name
                      (wasmdown.component.runtime:comp-instance-exports
                       instance))))
    (and hit (cdr hit))))

;;; ---------------------------------------------------------------
;;; wasi:clocks.

(defun monotonic-now ()
  "Nanoseconds on the host's monotonic clock."
  (multiple-value-bind (sec nsec)
      (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(defun monotonic-resolution ()
  "The monotonic clock's tick: 1 ns (the nanosecond clock_gettime
representation)."
  1)

(defun system-now ()
  "The system clock as the WIT `instant` record (unix seconds +
nanoseconds)."
  (multiple-value-bind (sec nsec)
      (sb-unix:clock-gettime sb-unix:clock-realtime)
    (list (cons "seconds" sec)
          (cons "nanoseconds" nsec))))

(defun system-resolution ()
  "The system clock's practical resolution: 1 microsecond."
  1000)

;;; The ASYNC clock waits (WIT: `wait-until: async func` /
;;; `wait-for: async func`). A bounded-step busy wait would hold the
;;; calling OS thread — and, for a guest, the store's single execution
;;; right — for the whole wait. Instead the provider registers ONE-SHOT
;;; deadline with the context's reactor and answers with a HOST-PENDING:
;;; the guest's task parks, the reactor's callback only ENQUEUES the
;;; pending's resolution (a reactor callback never calls guest code),
;;; and the store's own pump delivers it. No provider thread sleeps.

(defconstant +u64-max+ #xffffffffffffffff
  "The largest `mark`/`duration` (both are u64 in wasi:clocks).")

(defun %clock-mark (value what)
  "VALUE as a u64 mark, or a trap: a mark is a u64, so a value outside
its range cannot be represented (the WIT's representability trap)."
  (unless (and (integerp value) (<= 0 value +u64-max+))
    (error 'wasmdown:wasm-trap
           :message (format nil "~a ~s is not a u64 mark" what value)))
  value)

(defun %clock-deadline-for (reactor how-long)
  "NOW + HOW-LONG in monotonic nanoseconds. The sum must stay a u64:
overflow TRAPS rather than wrapping (a wrapped deadline would be a
silent no-wait)."
  (let ((now (%reactor-clock reactor)))
    (%clock-mark how-long "duration")
    (when (> how-long (- +u64-max+ now))
      (error 'wasmdown:wasm-trap
             :message (format nil
                              "wait-for ~d ns from ~d overflows the u64 mark space"
                              how-long now)))
    (+ now how-long)))

(defstruct (clock-wait (:constructor %make-clock-wait))
  "One armed deadline. STATE serializes the two possible wins: the
timer FIRED (the wait resolves, once) or the wait was CANCELLED (its
timer is removed, once)."
  (mutex (sb-thread:make-mutex :name "clock-wait"))
  (state :armed)                       ; :armed | :fired | :cancelled
  (timer nil))

(defun %clock-arm (reactor deadline resolve)
  "Arm DEADLINE as a one-shot reactor deadline that calls RESOLVE.
Returns the pending's cancel-fn: it removes the timer at most once and
T (stopped) only when it actually won the race against the timer. A
fired wait never cancels, a cancelled wait never resolves."
  (let* ((wait (%make-clock-wait))
         (timer
           (reactor-watch-deadline
            reactor deadline
            (lambda ()
              (let ((fire nil))
                (sb-thread:with-recursive-lock ((clock-wait-mutex wait))
                  (when (eq (clock-wait-state wait) :armed)
                    (setf (clock-wait-state wait) :fired
                          (clock-wait-timer wait) nil)
                    (setf fire t)))
                (when fire (funcall resolve nil)))))))
    (sb-thread:with-recursive-lock ((clock-wait-mutex wait))
      (setf (clock-wait-timer wait) timer))
    (lambda ()
      (let ((timer nil) (stopped nil))
        (sb-thread:with-recursive-lock ((clock-wait-mutex wait))
          (when (eq (clock-wait-state wait) :armed)
            (setf (clock-wait-state wait) :cancelled
                  timer (clock-wait-timer wait)
                  (clock-wait-timer wait) nil
                  stopped t)))
        (when timer (funcall timer))
        stopped))))

(defun %clock-pending (reactor deadline)
  "The async answer of wait-until/wait-for. A deadline the clock has
ALREADY reached resolves immediately with NO timer registered — still
through the pending, so every path of the call has one shape."
  (if (<= deadline (%reactor-clock reactor))
      (wasmdown.component.runtime:make-host-pending
       :start (lambda (resolve reject acknowledge-cancel)
                (declare (ignore reject acknowledge-cancel))
                (funcall resolve nil)
                ;; already due: nothing is registered, nothing to stop
                (lambda () nil)))
      (wasmdown.component.runtime:make-host-pending
       :start (lambda (resolve reject acknowledge-cancel)
                (declare (ignore reject acknowledge-cancel))
                (%clock-arm reactor deadline resolve)))))

(defun monotonic-wait-until (reactor when)
  "wait-until: park until the monotonic clock reaches WHEN."
  (unless reactor
    (error 'wasmdown:host-error
           :message "no reactor capability for monotonic-clock wait-until"))
  (%clock-pending reactor (%clock-mark when "deadline")))

(defun monotonic-wait-for (reactor how-long)
  "wait-for: park for HOW-LONG nanoseconds."
  (unless reactor
    (error 'wasmdown:host-error
           :message "no reactor capability for monotonic-clock wait-for"))
  (%clock-pending reactor (%clock-deadline-for reactor how-long)))

(defun monotonic-clock-instance (&key (package (wasi-package "clocks"))
                                      reactor)
  "The monotonic-clock provider. REACTOR is the context's reactor; the
async waits need it (NIL: the waits report the missing capability)."
  (provider-instance
   (wasmdown.wit:package-interface package "monotonic-clock")
   (list (cons "now" #'monotonic-now)
         (cons "get-resolution" #'monotonic-resolution)
         (cons "wait-until"
               (lambda (when) (monotonic-wait-until reactor when)))
         (cons "wait-for"
               (lambda (how-long) (monotonic-wait-for reactor how-long))))
   :packages (list package)))

(defun system-clock-instance (&key (package (wasi-package "clocks")))
  (provider-instance
   (wasmdown.wit:package-interface package "system-clock")
   (list (cons "now" #'system-now)
         (cons "get-resolution" #'system-resolution))
   :packages (list package)))

(defun clocks-provider (&key (package (wasi-package "clocks")) reactor)
  "The wasi:clocks imports as a with-args alist. REACTOR (the
context's reactor) backs the async wait-until/wait-for; NIL leaves
them reporting the missing capability."
  (list
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "types"))
         (provider-instance
          (wasmdown.wit:package-interface package "types") nil
          :packages (list package)))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "monotonic-clock"))
         (monotonic-clock-instance :package package :reactor reactor))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "system-clock"))
         (system-clock-instance :package package))))

;;; ---------------------------------------------------------------
;;; wasi:random — OS entropy only.

(defparameter *max-random-bytes* (* 1024 1024)
  "The provider's short-read cap (the WIT explicitly allows returning
fewer bytes than requested; a bounded read also bounds the host).")

(defun os-random-bytes (n)
  "N bytes of OS entropy as a list of (unsigned-byte 8) integers."
  (unless (and (integerp n) (not (minusp n)))
    (error 'wasmdown:host-error
           :message (format nil "invalid random byte count ~s" n)))
  (if (zerop n)
      '()
      (let ((out (make-array n :element-type '(unsigned-byte 8))))
        (with-open-file (in #p"/dev/urandom" :direction :input
                                           :element-type '(unsigned-byte 8))
          (unless (= n (read-sequence out in))
            (error 'wasmdown:host-error
                   :message "short read from /dev/urandom")))
        (coerce out 'list))))

(defun %u64-from-bytes (bytes)
  (loop for b in bytes
        for i from 0
        sum (ash b (* 8 i))))

(defun %clamped-length (max-len)
  (unless (and (integerp max-len) (not (minusp max-len)))
    (error 'wasmdown:host-error
           :message (format nil "invalid random byte count ~s" max-len)))
  (min max-len *max-random-bytes*))

(defun random-bytes (max-len)
  "get-random-bytes: up to MAX-LEN cryptographically secure bytes."
  (os-random-bytes (%clamped-length max-len)))

(defun insecure-random-bytes (max-len)
  "get-insecure-random-bytes: up to MAX-LEN bytes."
  (os-random-bytes (%clamped-length max-len)))

(defun random-u64 ()
  (logand (%u64-from-bytes (os-random-bytes 8))
          #xffffffffffffffff))

(defun insecure-random-u64 ()
  (logand (%u64-from-bytes (os-random-bytes 8))
          #xffffffffffffffff))

(defun insecure-seed ()
  "get-insecure-seed: the tuple<u64, u64> domain value."
  (list (random-u64) (random-u64)))

(defun random-instance (&key (package (wasi-package "random")))
  (provider-instance
   (wasmdown.wit:package-interface package "random")
   (list (cons "get-random-bytes" #'random-bytes)
         (cons "get-random-u64" #'random-u64))
   :packages (list package)))

(defun insecure-random-instance (&key (package (wasi-package "random")))
  (provider-instance
   (wasmdown.wit:package-interface package "insecure")
   (list (cons "get-insecure-random-bytes" #'insecure-random-bytes)
         (cons "get-insecure-random-u64" #'insecure-random-u64))
   :packages (list package)))

(defun insecure-seed-instance (&key (package (wasi-package "random")))
  (provider-instance
   (wasmdown.wit:package-interface package "insecure-seed")
   (list (cons "get-insecure-seed" #'insecure-seed))
   :packages (list package)))

(defun random-provider (&key (package (wasi-package "random")))
  (list
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "random"))
         (random-instance :package package))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "insecure"))
         (insecure-random-instance :package package))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "insecure-seed"))
         (insecure-seed-instance :package package))))

;;; ---------------------------------------------------------------
;;; wasi:cli — environment + exit (the stdio interface follows below,
;;; over the runtime's host stream/future machinery).

(defun environment-get-environment (context)
  "list<tuple<string,string>> from the context alist."
  (loop for (name . value) in (wasi-context-environment context)
        collect (list name value)))

(defun environment-get-arguments (context)
  (copy-list (wasi-context-arguments context)))

(defun environment-get-initial-cwd (context)
  (let ((cwd (wasi-context-cwd context)))
    (if cwd (cons :some cwd) :none)))

(defun cli-environment-instance (context
                                 &key (package (wasi-package "cli")))
  (provider-instance
   (wasmdown.wit:package-interface package "environment")
   (list (cons "get-environment"
               (lambda () (environment-get-environment context)))
         (cons "get-arguments"
               (lambda () (environment-get-arguments context)))
         (cons "get-initial-cwd"
               (lambda () (environment-get-initial-cwd context))))
   :packages (list package)))

(defun cli-exit-instance (&key (package (wasi-package "cli")))
  (provider-instance
   (wasmdown.wit:package-interface package "exit")
   (list (cons "exit"
               (lambda (status) (%wasi-exit (%result-status status))))
         (cons "exit-with-code"
               (lambda (code) (%wasi-exit code))))
   :packages (list package)))

;;; ---------------------------------------------------------------
;;; wasi:cli — the STDIO stream/future providers (M16 slice 2): the
;;; host side of the WIT's
;;;   read-via-stream:  func() -> tuple<stream<u8>, future<result<_, error-code>>>
;;;   write-via-stream: func(stream<u8>) -> future<result<_, error-code>>
;;; over the CONTEXT's CL streams (PLAN.md §9.1: an explicit authority
;;; object, never *STANDARD-OUTPUT*). The host ends are constructed by
;;; the runtime's host-backed stream/future machinery: the guest's
;;; writes drain into the sink, the guest's reads pull from the source,
;;; and the future resolves (`(:ok)` / `(:err . "io")`) when the guest
;;; drops the end or the source hits EOF.

(defparameter *cli-stream-chunk* 4096
  "The host source's per-call read bound (a stream read may return
fewer bytes than the guest asked for; the canonical ABI allows it).")

;;; The poll(2)/errno values the HOST-BACKED stream providers need
;;; before the libc section below defines its socket surface (the
;;; sockets section reuses these).
(defparameter +eintr+ 4)
(defparameter +eagain+ 11)
(defparameter +pollin+ 1)
(defparameter +pollout+ 4)
(defparameter +pollhup+ #x10)
(defparameter +pollerr+ #x8)
(defparameter +pollnval+ #x20)

(defun %cli-u8-ct ()
  "The stream<u8>'s element ct."
  (wasmdown.component:make-ct :kind :prim :payload :u8))

(defun %cli-future-ct (package interface-name func-name)
  "The element ct of the `future<result<_, error-code>>` a CLI stdio
function returns: the future ct from the WIT functype (it is the
result itself for write-via-stream, the second tuple field for
read-via-stream)."
  (let* ((interface (wasmdown.wit:package-interface package interface-name))
         (func (find func-name
                     (wasmdown.wit:wit-interface-func-list interface)
                     :key #'wasmdown.wit:wit-func-name :test #'string=))
         (ft (and func (wasmdown.wit:wit-func->ct interface func
                                                 :packages (list package))))
         (result (and ft (second (wasmdown.component:ct-payload ft))))
         (future (cond
                   ((and result
                         (eq (wasmdown.component:ct-kind result) :future))
                    result)
                   ((and result
                         (eq (wasmdown.component:ct-kind result) :tuple))
                    (find-if (lambda (c)
                               (eq (wasmdown.component:ct-kind c) :future))
                             (coerce (wasmdown.component:ct-payload result)
                                     'list))))))
    (unless future
      (error 'wasmdown:host-error
             :message (format nil "~a/~a has no future result in its WIT"
                              interface-name func-name)))
    (wasmdown.component:ct-payload future)))

(defun %byte-stream-p (stream)
  (subtypep (stream-element-type stream) '(unsigned-byte 8)))

(defun %stream-input-ready-p (stream fd)
  "T when STREAM can deliver input without blocking: buffered input
(LISTEN) or a readiness/hangup event on FD. A hangup counts as ready
so the following read reports EOF instead of parking forever."
  (or (listen stream)
      (and (%poll-fd fd (logior +pollin+ +pollhup+ +pollerr+ +pollnval+) 0)
           t)))

;;; ---- BORROWED BLOCKING STDIO: the bounded worker path ----
;;; A stream whose fd is NOT O_NONBLOCK can block inside read(2)/
;;; write(2) (a full pipe, a terminal). The guest's thread must never
;;; be the one that blocks: the SPAN IS COPIED into a worker-owned
;;; vector and the context's bounded STDIO EXECUTOR performs the
;;; call. No guest pointer ever reaches the worker, and the worker's
;;; only way back is the store queue (HOST-END-READY for the parked
;;; copy's generation). A string stream or an already-nonblocking fd
;;; keeps today's direct path verbatim.

(defstruct (stdio-wait (:constructor %make-stdio-wait))
  "One blocking stream's in-flight worker job and its lifecycle."
  (mutex (sb-thread:make-mutex :name "stdio-wait"))
  (job nil)
  (closed-p nil))

(defstruct (stdio-sink-job (:constructor %make-stdio-sink-job))
  (generation 0)
  (vector nil)                        ; the COPIED span (host-owned)
  (count 0)
  (error nil)
  (done-p nil))

(defstruct (stdio-source-job (:constructor %make-stdio-source-job))
  (generation 0)
  (want 0)
  (vector nil)
  (eof nil)
  (done-p nil))

(defun %stdio-generation (end)
  "END's current copy generation (the attempt a worker completion must
name, so a late completion cannot resume a LATER copy)."
  (and end (wasmdown.component.runtime:host-end-generation end)))

(defun %stdio-wait-close (wait)
  "Close WAIT (the stream dropped / the provider gave it up), DROPPING
the in-flight job's binding: its late completion still posts a
readiness notification, which finds no parked copy."
  (when wait
    (sb-thread:with-recursive-lock ((stdio-wait-mutex wait))
      (setf (stdio-wait-closed-p wait) t
            (stdio-wait-job wait) nil))
    t))

(defun %stdio-sink-run (stream byte-p job end generation)
  "The WORKER body for one copied span: write it (the job's OWN vector
only) and let the STORE queue wake the parked copy."
  (handler-case
      (let ((vector (stdio-sink-job-vector job)))
        (if byte-p
            (write-sequence vector stream)
            (let ((chars (make-array (length vector)
                                     :element-type 'character)))
              (dotimes (i (length vector))
                (setf (aref chars i) (code-char (aref vector i))))
              (write-sequence chars stream)))
        (finish-output stream)
        (setf (stdio-sink-job-count job) (length vector)))
    (error ()
      (setf (stdio-sink-job-count job) 0
            (stdio-sink-job-error job) "io")))
  (setf (stdio-sink-job-done-p job) t)
  (wasmdown.component.runtime:host-end-ready end generation))

(defun %stdio-source-run (stream byte-p job end generation)
  "The WORKER body for one bounded read into the job's OWN vector."
  (handler-case
      (let ((want (stdio-source-job-want job))
            (vector (make-array *cli-stream-chunk*
                                :element-type '(unsigned-byte 8)
                                :fill-pointer 0))
            (chars (unless byte-p
                     (make-array *cli-stream-chunk*
                                 :element-type 'character))))
        (setf (fill-pointer vector) want)
        (let ((got (if byte-p
                       (read-sequence vector stream :end want)
                       (read-sequence chars stream :end want))))
          (unless byte-p
            (dotimes (i got)
              (setf (aref vector i)
                    (logand #xff (char-code (aref chars i))))))
          (setf (fill-pointer vector) got
                (stdio-source-job-vector job) vector
                (stdio-source-job-eof job) (< got want))))
    (error ()
      (setf (stdio-source-job-vector job) nil
            (stdio-source-job-eof job) t)))
  (setf (stdio-source-job-done-p job) t)
  (wasmdown.component.runtime:host-end-ready end generation))

(defun cl-stream-source (stream &key context end)
  "A host SOURCE over CL input STREAM: (max) -> (values octets eof-p),
OCTETS a REUSABLE u8 vector whose length is the count. Character
streams are encoded byte-per-character (the WASI byte stream is
opaque; chars are taken modulo the code). (NIL NIL) is WOULD-BLOCK: a
host-backed stream with nothing to read right now parks the copy
instead of blocking the host thread.

M17 step 6: a BORROWED BLOCKING fd (with a CONTEXT and the host END)
hands the read to the context's bounded stdio executor — the worker
reads into its OWN vector and its completion posts HOST-END-READY."
  (let* ((byte-p (%byte-stream-p stream))
         (fd (%stream-fd stream))
         (worker-p (and context end (integerp fd) (>= fd 0)
                        (not (%fd-nonblocking-p fd))))
         (wait (and worker-p (%make-stdio-wait)))
         (buf (make-array *cli-stream-chunk*
                          :element-type '(unsigned-byte 8)
                          :fill-pointer 0))
         (chars (unless byte-p
                  (make-array *cli-stream-chunk* :element-type 'character))))
    (if worker-p
        (lambda (max)
          (let ((want (min max *cli-stream-chunk*)))
            (if (<= want 0)
                (values nil nil)
                (let ((job nil) (submit nil) (generation nil))
                  (sb-thread:with-recursive-lock ((stdio-wait-mutex wait))
                    (setf job (stdio-wait-job wait))
                    (when (and job
                               (not (eql (stdio-source-job-generation job)
                                         (%stdio-generation end))))
                      ;; a job from a copy attempt that is GONE: drop it
                      (setf job nil
                            (stdio-wait-job wait) nil))
                    (when (and (null job) (not (stdio-wait-closed-p wait)))
                      (setf generation (%stdio-generation end)
                            job (%make-stdio-source-job
                                 :generation generation :want want)
                            (stdio-wait-job wait) job
                            submit t)))
                  (cond
                    (submit
                     (if (%executor-submit
                          (wasi-context-stdio-executor context)
                          (lambda ()
                            (%stdio-source-run stream byte-p job
                                               end generation)))
                         (values nil nil)
                         (progn
                           (sb-thread:with-recursive-lock
                               ((stdio-wait-mutex wait))
                             (setf (stdio-wait-job wait) nil))
                           (values nil t))))
                    ((null job) (values nil nil))
                    ((not (stdio-source-job-done-p job)) (values nil nil))
                    (t
                     (let ((vector (stdio-source-job-vector job))
                           (eof (stdio-source-job-eof job)))
                       (sb-thread:with-recursive-lock
                           ((stdio-wait-mutex wait))
                         (when (eq (stdio-wait-job wait) job)
                           (setf (stdio-wait-job wait) nil)))
                       (values vector eof))))))))
        (lambda (max)
          (let ((want (min max *cli-stream-chunk*)))
            (cond
              ((<= want 0) (values nil nil))
              ((and (integerp fd) (minusp fd)) (values nil nil))
              ((and (integerp fd) (not (%stream-input-ready-p stream fd)))
               (values nil nil))
              (t
               (setf (fill-pointer buf) want)
               (let ((got (if byte-p
                              (read-sequence buf stream :end want)
                              (read-sequence chars stream :end want))))
                 (unless byte-p
                   (dotimes (i got)
                     (setf (aref buf i)
                           (logand #xff (char-code (aref chars i))))))
                 (setf (fill-pointer buf) got)
                 (values buf (< got want))))))))))

(defun %output-stream-fd (stream)
  "The host file descriptor CL STREAM writes to, or NIL (string /
broadcast streams have none). Synonym streams are unwrapped and a
two-way stream uses its OUTPUT side."
  (typecase stream
    (sb-sys:fd-stream
     (sb-sys:fd-stream-fd stream))
    (synonym-stream
     (%output-stream-fd (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (%output-stream-fd (two-way-stream-output-stream stream)))
    (t nil)))

(defun cl-stream-sink (stream &key context end)
  "A host SINK over CL output STREAM: (octets start end) ->
(values consumed error-or-nil). At most END-START bytes are written in
one call and CONSUMED is what the stream took. A host-backed stream
that is not writable RIGHT NOW returns (0 NIL) — WOULD-BLOCK, so the
copy parks instead of blocking the host thread — and an I/O failure
returns (0 \"io\"). Flushed on every drain.

M17 step 6: a BORROWED BLOCKING fd (with a CONTEXT and the host END)
hands the SPAN — COPIED into a worker-owned vector, never a guest
pointer — to the context's bounded stdio executor. The sink returns
WOULD-BLOCK until the worker's completion posts HOST-END-READY; the
retry then reports the worker's actual count (or its error)."
  (let* ((byte-p (%byte-stream-p stream))
         (fd (%output-stream-fd stream))
         (worker-p (and context end (integerp fd) (>= fd 0)
                        (not (%fd-nonblocking-p fd))))
         (wait (and worker-p (%make-stdio-wait)))
         (chars (unless byte-p
                  (make-array 0 :element-type 'character
                                :adjustable t :fill-pointer 0))))
    (if worker-p
        (lambda (octets offset limit)
          (let ((n (- limit offset)))
            (if (<= n 0)
                (values 0 nil)
                (let ((job nil) (submit nil) (generation nil))
                  (sb-thread:with-recursive-lock ((stdio-wait-mutex wait))
                    (setf job (stdio-wait-job wait))
                    (when (and job
                               (not (eql (stdio-sink-job-generation job)
                                         (%stdio-generation end))))
                      ;; a job from a copy attempt that is GONE: drop it
                      (setf job nil
                            (stdio-wait-job wait) nil))
                    (when (and (null job) (not (stdio-wait-closed-p wait)))
                      (let ((vector (make-array n
                                                :element-type
                                                '(unsigned-byte 8))))
                        (replace vector octets :start2 offset :end2 limit)
                        (setf generation (%stdio-generation end)
                              job (%make-stdio-sink-job
                                   :generation generation
                                   :vector vector :count n)
                              (stdio-wait-job wait) job
                              submit t))))
                  (cond
                    (submit
                     (if (%executor-submit
                          (wasi-context-stdio-executor context)
                          (lambda ()
                            (%stdio-sink-run stream byte-p job
                                             end generation)))
                         (values 0 nil)
                         (progn
                           (sb-thread:with-recursive-lock
                               ((stdio-wait-mutex wait))
                             (setf (stdio-wait-job wait) nil))
                           (values 0 "io"))))
                    ((null job) (values 0 "io"))
                    ((not (stdio-sink-job-done-p job)) (values 0 nil))
                    ((stdio-sink-job-error job)
                     (let ((error (stdio-sink-job-error job)))
                       (sb-thread:with-recursive-lock
                           ((stdio-wait-mutex wait))
                         (when (eq (stdio-wait-job wait) job)
                           (setf (stdio-wait-job wait) nil)))
                       (values 0 error)))
                    (t
                     (let ((count (stdio-sink-job-count job)))
                       (sb-thread:with-recursive-lock
                           ((stdio-wait-mutex wait))
                         (when (eq (stdio-wait-job wait) job)
                           (setf (stdio-wait-job wait) nil)))
                       (when (> count n)
                         (error 'wasmdown:host-error
                                :message
                                "a stdio sink accepted more bytes than its span"))
                       (values count nil))))))))
        (lambda (octets start end)
          (let ((n (- end start)))
            (cond
              ((<= n 0) (values 0 nil))
              ((and (integerp fd) (minusp fd)) (values 0 nil))
              ((and (integerp fd) (not (%poll-fd fd +pollout+ 0)))
               (values 0 nil))
              (t
               (handler-case
                   (progn
                     (if byte-p
                         (write-sequence octets stream :start start :end end)
                         (progn
                           (setf (fill-pointer chars) 0)
                           (loop for i from start below end
                                 do (vector-push-extend
                                     (code-char (aref octets i)) chars))
                           (write-sequence chars stream)))
                     (finish-output stream)
                     (values n nil))
                 (error () (values 0 "io"))))))))))

(defun %stdio-require (stream what)
  (or stream
      (error 'wasmdown:host-error
             :message (format nil "no ~a capability in this wasi-context"
                              what))))

(defun %write-via-stream (data stream future-ct &key context)
  "Drain the guest's stream DATA into STREAM; the returned future
resolves once the guest drops the stream's writable end (the WIT's
success condition) or a sink error drops it. A borrowed blocking
STREAM's span is copied onto the context's stdio worker (M17 step 6)."
  (let ((sink-stream (%stdio-require stream "stdout/stderr")))
    (unless (wasmdown.component.runtime:async-end-p data)
      (error 'wasmdown:host-error
             :message "write-via-stream: data is not a stream end"))
    (multiple-value-bind (future-reader future-writer)
        (wasmdown.component.runtime:make-host-future-pair future-ct)
      (wasmdown.component.runtime:host-end-set-sink
       data (cl-stream-sink sink-stream :context context :end data))
      (wasmdown.component.runtime:host-end-set-on-drop
       data (lambda (dropped error)
              (declare (ignore dropped))
              (wasmdown.component.runtime:host-future-resolve
               future-writer
               (if error (cons :err error) (cons :ok nil)))))
      ;; the guest may have staged a write BEFORE handing us the end
      (wasmdown.component.runtime:host-end-drain data)
      future-reader)))

(defun %read-via-stream (stream future-ct &key context)
  "A stream fed from STREAM and a future signalling the read result.
A borrowed blocking STREAM's read runs on the context's stdio worker
(M17 step 6), so the guest's thread parks instead of blocking."
  (let ((source-stream (%stdio-require stream "stdin")))
    (multiple-value-bind (stream-reader stream-writer)
        (wasmdown.component.runtime:make-host-stream-pair (%cli-u8-ct))
      (multiple-value-bind (future-reader future-writer)
          (wasmdown.component.runtime:make-host-future-pair future-ct)
        (wasmdown.component.runtime:host-end-set-source
         stream-writer (cl-stream-source source-stream
                                         :context context
                                         :end stream-writer))
        (wasmdown.component.runtime:host-end-set-on-drop
         stream-writer (lambda (dropped error)
                         (declare (ignore dropped))
                         (wasmdown.component.runtime:host-future-resolve
                          future-writer
                          (if error (cons :err error) (cons :ok nil)))))
        ;; tuple<stream<u8>, future<result<_, error-code>>>
        (list stream-reader future-reader)))))

(defun cli-stdin-instance (context
                           &key (package (wasi-package "cli")))
  (let ((future-ct (%cli-future-ct package "stdin" "read-via-stream")))
    (provider-instance
     (wasmdown.wit:package-interface package "stdin")
     (list (cons "read-via-stream"
                 (lambda ()
                   (%read-via-stream (wasi-context-stdin context)
                                     future-ct
                                     :context context))))
     :packages (list package))))

(defun cli-output-instance (context stream interface-name
                            &key (package (wasi-package "cli")))
  (let ((future-ct (%cli-future-ct package interface-name
                                   "write-via-stream")))
    (provider-instance
     (wasmdown.wit:package-interface package interface-name)
     (list (cons "write-via-stream"
                 (lambda (data)
                   (%write-via-stream data stream future-ct
                                      :context context))))
     :packages (list package))))

(defun cli-stdout-instance (context &key (package (wasi-package "cli")))
  (cli-output-instance context (wasi-context-stdout context) "stdout"
                       :package package))

(defun cli-stderr-instance (context &key (package (wasi-package "cli")))
  (cli-output-instance context (wasi-context-stderr context) "stderr"
                       :package package))

(defun cli-stdio-provider (context &key (package (wasi-package "cli")))
  "The wasi:cli stdio imports (stdin/stdout/stderr) as a with-args
alist over CONTEXT's CL streams. An absent stream leaves the interface
exported but trapping (host-error) — the capability is withdrawable."
  (list
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "stdin"))
         (cli-stdin-instance context :package package))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "stdout"))
         (cli-stdout-instance context :package package))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "stderr"))
         (cli-stderr-instance context :package package))))

(defun cli-provider (context &key (package (wasi-package "cli")))
  "The full wasi:cli provider: environment, exit, the stdio streams
(whose authority is CONTEXT's stdin/stdout/stderr slots) and — M16
slice 6 — the terminal authorities."
  (append
   (list
    (cons (wasmdown.wit:interface-import-name
           (wasmdown.wit:package-interface package "environment"))
          (cli-environment-instance context :package package))
    (cons (wasmdown.wit:interface-import-name
           (wasmdown.wit:package-interface package "exit"))
          (cli-exit-instance :package package)))
   (cli-stdio-provider context :package package)
   (cli-terminal-provider context :package package)))

;;; ---------------------------------------------------------------
;;; wasi:cli — THE TERMINALS (M16 slice 6, `cli.wit': terminal-input,
;;; terminal-output and the three link-time authorities).
;;;
;;; The WIT declares only the resource types (no methods yet); the
;;; authorities answer `option<terminal-*>' — `some' exactly when the
;;; context stream behind stdin/stdout/stderr is a HOST TERMINAL.
;;; `isatty(3)' is the only honest test (a redirected file/string
;;; stream is not a terminal), so the rep is the host fd + the probe
;;; result, and a non-TTY stream yields `none' — never a fabricated
;;; handle (PLAN.md §9.1: the capability is explicit).

(defstruct wasi-terminal
  "The REP of a terminal-input / terminal-output handle: the host file
descriptor behind the context stream and whether isatty(3) accepted
it. The fd is BORROWED from the CL stream (dropping the handle never
closes it)."
  (fd nil)
  (isatty nil))

(defun %stream-fd (stream)
  "The host file descriptor a CL STREAM is backed by, or NIL (string /
broadcast streams have none). Synonym and two-way streams are
unwrapped — a CLI launcher's *standard-output* is a synonym stream."
  (typecase stream
    (sb-sys:fd-stream
     (sb-sys:fd-stream-fd stream))
    (synonym-stream
     (%stream-fd (symbol-value (synonym-stream-symbol stream))))
    (two-way-stream
     (or (%stream-fd (two-way-stream-input-stream stream))
         (%stream-fd (two-way-stream-output-stream stream))))
    (t nil)))

(defun %terminal-probe (stream)
  "The wasi-terminal a CL STREAM denotes, or NIL when it has no host
fd or isatty(3) reports a non-terminal."
  (let ((fd (%stream-fd stream)))
    (when (and (integerp fd) (not (minusp fd)))
      (multiple-value-bind (tty errno)
          (sb-unix:unix-isatty fd)
        (declare (ignore errno))
        (when (eql tty 1)
          (make-wasi-terminal :fd fd :isatty t))))))

(defun %cli-resource-ct (package interface-name resource-name)
  "The resource ct of RESOURCE-NAME in INTERFACE-NAME — the SHARED
identity every occurrence of the resource (here through `use' in the
authority interfaces) resolves to."
  (wasmdown.wit:wit-type->ct
   (wasmdown.wit:package-interface package interface-name)
   (list :resource resource-name)
   :packages (list package)))

(defun terminal-input-instance (&key (package (wasi-package "cli")))
  "The wasi:cli/terminal-input interface: the resource type alone (the
WIT declares no functions yet)."
  (provider-instance
   (wasmdown.wit:package-interface package "terminal-input") nil
   :packages (list package)))

(defun terminal-output-instance (&key (package (wasi-package "cli")))
  "The wasi:cli/terminal-output interface."
  (provider-instance
   (wasmdown.wit:package-interface package "terminal-output") nil
   :packages (list package)))

(defun %terminal-authority (stream interface-name resource-name
                            &key (package (wasi-package "cli")))
  "The `get-terminal-*' authority instance over STREAM: `some' with an
own<resource> handle whose rep is the wasi-terminal when the stream is
a terminal, `none' otherwise."
  (let ((ct (%cli-resource-ct package interface-name resource-name)))
    (provider-instance
     (wasmdown.wit:package-interface package interface-name)
     (list
      (cons (format nil "get-terminal-~a"
                    (subseq interface-name (length "terminal-")))
            (lambda ()
              (let ((terminal (%terminal-probe stream)))
                (if terminal
                    (cons :some (list :own-handle ct terminal))
                    :none)))))
     :packages (list package))))

(defun terminal-stdin-instance (context
                                &key (package (wasi-package "cli")))
  (%terminal-authority (wasi-context-stdin context)
                       "terminal-stdin" "terminal-input"
                       :package package))

(defun terminal-stdout-instance (context
                                 &key (package (wasi-package "cli")))
  (%terminal-authority (wasi-context-stdout context)
                       "terminal-stdout" "terminal-output"
                       :package package))

(defun terminal-stderr-instance (context
                                 &key (package (wasi-package "cli")))
  (%terminal-authority (wasi-context-stderr context)
                       "terminal-stderr" "terminal-output"
                       :package package))

(defun cli-terminal-provider (context &key (package (wasi-package "cli")))
  "The wasi:cli terminal imports (the two resource interfaces + the
three link-time authorities) as a with-args alist."
  (flet ((import-name (name)
           (wasmdown.wit:interface-import-name
            (wasmdown.wit:package-interface package name))))
    (list
     (cons (import-name "terminal-input")
           (terminal-input-instance :package package))
     (cons (import-name "terminal-output")
           (terminal-output-instance :package package))
     (cons (import-name "terminal-stdin")
           (terminal-stdin-instance context :package package))
     (cons (import-name "terminal-stdout")
           (terminal-stdout-instance context :package package))
     (cons (import-name "terminal-stderr")
           (terminal-stderr-instance context :package package)))))

;;; ===============================================================
;;; wasi:filesystem (M16 slice 3a) — THE CAPABILITY MODEL.
;;;
;;; PLAN.md §9.1: a file root is an OPEN HOST DIRECTORY HANDLE, never
;;; a string prefix. Every operation is handle-relative (`*at' calls
;;; against the descriptor's fd); there is no path-prefix filtering
;;; anywhere. The authority object is WASI-PREOPEN — the fd, the
;;; guest-visible name, and the descriptor the guest sees.
;;;
;;; Path policy (the WIT's 0.3 model, plus the conservative reading
;;; §9.1 asks for):
;;;   * paths are guest-relative: an empty path, an absolute path, a
;;;     `..' component, or an embedded NUL is rejected (not-permitted
;;;     / invalid) — a guest can never name a location outside the
;;;     preopen;
;;;   * the resolver walks components itself, opening each with
;;;     O_NOFOLLOW, so the KERNEL never follows a symlink on our
;;;     behalf (no TOCTOU window where a link swap redirects us);
;;;     symlinks are expanded explicitly by readlinkat, and a target
;;;     that is absolute or contains `..' is refused;
;;;   * `path-flags.symlink-follow' decides whether the FINAL
;;;     component is expanded; without it the operation addresses the
;;;     link itself (stat-at reports symbolic-link, open-at fails
;;;     closed with loop).
;;;
;;; The descriptor RESOURCE: one host resource per open descriptor,
;;; with the runtime's handle table doing own/borrow (the provider's
;;; result lowering adds the handle to the caller's table; a borrow
;;; param arrives as that table's index and is resolved back through
;;; the shared resource ct). The rep is the WASI-DESCRIPTOR record;
;;; canon resource.drop runs the registered host finalizer, so the fd
;;; is released deterministically.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (find-package '#:wasmdown.wasi)))
    (dolist (name '("WASI-CONTEXT-PREOPENS"
                    "WASI-PREOPEN" "WASI-PREOPEN-P" "MAKE-WASI-PREOPEN"
                    "WASI-PREOPEN-FD" "WASI-PREOPEN-NAME"
                    "WASI-PREOPEN-DESCRIPTOR"
                    "OPEN-WASI-PREOPEN" "CLOSE-WASI-PREOPEN"
                    "WASI-DESCRIPTOR" "WASI-DESCRIPTOR-P"
                    "MAKE-WASI-DESCRIPTOR" "WASI-DESCRIPTOR-FD"
                    "WASI-DESCRIPTOR-FLAGS" "WASI-DESCRIPTOR-PREOPEN"
                    "WASI-DESCRIPTOR-DIR-P" "WASI-DESCRIPTOR-OWNED-P"
                    "WASI-DESCRIPTOR-REFS" "WASI-DESCRIPTOR-CLOSED-P"
                    "CLOSE-WASI-DESCRIPTOR"
                    "WASI-FS-ERROR" "WASI-FS-ERROR-CODE"
                    "ERRNO->ERROR-CODE"
                    "FILESYSTEM-TYPES-INSTANCE" "FILESYSTEM-PREOPENS-INSTANCE"
                    "FILESYSTEM-PROVIDER" "DESCRIPTOR-RESOURCE-CT"
                    "FS-STAT" "FS-STAT-AT" "FS-OPEN-AT" "FS-GET-TYPE"
                    "FS-GET-FLAGS" "FS-SET-SIZE" "FS-IS-SAME-OBJECT"
                    "FS-METADATA-HASH" "FS-METADATA-HASH-AT"
                    "FS-CREATE-DIRECTORY-AT" "FS-REMOVE-DIRECTORY-AT"
                    "FS-UNLINK-FILE-AT" "FS-RENAME-AT" "FS-READLINK-AT"
                    "FS-SYMLINK-AT" "FS-ADVISE" "FS-SYNC" "FS-SYNC-DATA"
                    "FS-SET-TIMES" "FS-SET-TIMES-AT" "FS-LINK-AT"
                    "FS-READ-VIA-STREAM" "FS-WRITE-VIA-STREAM"
                    "FS-APPEND-VIA-STREAM" "FS-READ-DIRECTORY"
                    "FS-GET-DIRECTORIES"))
      (export (intern name package) package))))

;;; ---------------------------------------------------------------
;;; The libc `*at' bindings (PLAN.md §9.2: thin, audited SB-ALIEN).

(sb-alien:define-alien-routine ("open" %libc-open) sb-alien:int
  (path sb-alien:c-string) (flags sb-alien:int) (mode sb-alien:int))
(sb-alien:define-alien-routine ("openat" %libc-openat) sb-alien:int
  (dirfd sb-alien:int) (path sb-alien:c-string) (flags sb-alien:int)
  (mode sb-alien:int))
(sb-alien:define-alien-routine ("mkdirat" %libc-mkdirat) sb-alien:int
  (dirfd sb-alien:int) (path sb-alien:c-string) (mode sb-alien:int))
(sb-alien:define-alien-routine ("unlinkat" %libc-unlinkat) sb-alien:int
  (dirfd sb-alien:int) (path sb-alien:c-string) (flags sb-alien:int))
(sb-alien:define-alien-routine ("renameat" %libc-renameat) sb-alien:int
  (olddir sb-alien:int) (oldpath sb-alien:c-string)
  (newdir sb-alien:int) (newpath sb-alien:c-string))
(sb-alien:define-alien-routine ("linkat" %libc-linkat) sb-alien:int
  (olddir sb-alien:int) (oldpath sb-alien:c-string)
  (newdir sb-alien:int) (newpath sb-alien:c-string) (flags sb-alien:int))
(sb-alien:define-alien-routine ("symlinkat" %libc-symlinkat) sb-alien:int
  (target sb-alien:c-string) (dirfd sb-alien:int) (path sb-alien:c-string))
(sb-alien:define-alien-routine ("readlinkat" %libc-readlinkat) sb-alien:long
  (dirfd sb-alien:int) (path sb-alien:c-string)
  (buf (* sb-alien:unsigned-char)) (size sb-alien:size-t))
(sb-alien:define-alien-routine ("ftruncate" %libc-ftruncate) sb-alien:int
  (fd sb-alien:int) (length sb-alien:off-t))
(sb-alien:define-alien-routine ("fsync" %libc-fsync) sb-alien:int
  (fd sb-alien:int))
(sb-alien:define-alien-routine ("fdatasync" %libc-fdatasync) sb-alien:int
  (fd sb-alien:int))
(sb-alien:define-alien-routine ("posix_fadvise" %libc-fadvise) sb-alien:int
  (fd sb-alien:int) (offset sb-alien:off-t) (length sb-alien:off-t)
  (advice sb-alien:int))

;;; M16 slice 3b — THE STREAM SURFACE's OS primitives. pread/pwrite
;;; keep the kernel file offset out of play entirely (the WIT: several
;;; read/write/append streams on one open file "do not interfere").
;;; The directory iterator goes through fdopendir/readdir: SBCL wraps
;;; neither, so `struct dirent' is spelled out here. The layout is the
;;; Linux LP64 one (both x86_64 and arm64): ino_t (8) + off_t (8) +
;;; unsigned short d_reclen (2) + unsigned char d_type (1) + d_name
;;; (256) — d_type at offset 18, d_name at 19. SBCL's struct
;;; alignment agrees with the C compiler's (verified by layout, not
;;; guessed: the size is 280 bytes, the 8-aligned rounding).
(sb-alien:define-alien-routine ("pread" %libc-pread) sb-alien:long
  (fd sb-alien:int) (buf (* sb-alien:unsigned-char))
  (count sb-alien:size-t) (offset sb-alien:off-t))
(sb-alien:define-alien-routine ("pwrite" %libc-pwrite) sb-alien:long
  (fd sb-alien:int) (buf (* sb-alien:unsigned-char))
  (count sb-alien:size-t) (offset sb-alien:off-t))

(sb-alien:define-alien-type nil
  (sb-alien:struct %dirent
    (d-ino sb-alien:unsigned-long)
    (d-off sb-alien:long)
    (d-reclen sb-alien:unsigned-short)
    (d-type sb-alien:unsigned-char)
    (d-name (sb-alien:array sb-alien:char 256))))

(sb-alien:define-alien-routine ("fdopendir" %libc-fdopendir)
    (sb-alien:* (sb-alien:struct %dirent))
  (fd sb-alien:int))
(sb-alien:define-alien-routine ("readdir" %libc-readdir)
    (sb-alien:* (sb-alien:struct %dirent))
  (dirp (sb-alien:* (sb-alien:struct %dirent))))
(sb-alien:define-alien-routine ("closedir" %libc-closedir) sb-alien:int
  (dirp (sb-alien:* (sb-alien:struct %dirent))))

;;; d_type values (linux dirent.h).
(defparameter +dt-unknown+ 0)
(defparameter +dt-fifo+ 1)
(defparameter +dt-chr+ 2)
(defparameter +dt-dir+ 4)
(defparameter +dt-blk+ 6)
(defparameter +dt-reg+ 8)
(defparameter +dt-lnk+ 10)
(defparameter +dt-sock+ 12)

(sb-alien:define-alien-type nil
  (sb-alien:struct %wasi-timespec
    (tv-sec sb-alien:long)
    (tv-nsec sb-alien:long)))
(sb-alien:define-alien-type nil
  (sb-alien:struct %wasi-timespec-pair
    (a (sb-alien:struct %wasi-timespec))
    (b (sb-alien:struct %wasi-timespec))))

(sb-alien:define-alien-routine ("futimens" %libc-futimens) sb-alien:int
  (fd sb-alien:int) (times (* (sb-alien:struct %wasi-timespec))))
(sb-alien:define-alien-routine ("utimensat" %libc-utimensat) sb-alien:int
  (dirfd sb-alien:int) (path sb-alien:c-string)
  (times (* (sb-alien:struct %wasi-timespec))) (flags sb-alien:int))

;;; Fcntl flag values. Most are asm-generic (identical on x86_64 and
;;; arm64), but O_DIRECTORY / O_NOFOLLOW are ARCH-SPECIFIC: arm and
;;; arm64 override them in asm/fcntl.h (O_DIRECTORY 040000,
;;; O_NOFOLLOW 0100000) while the rest of the world uses the
;;; asm-generic 0200000/0400000. Getting O_NOFOLLOW wrong would
;;; silently let the kernel follow symlinks, so the arm split is
;;; explicit.
(defparameter +o-rdonly+ 0)
(defparameter +o-wronly+ 1)
(defparameter +o-rdwr+ 2)
(defparameter +o-creat+ #x40)
(defparameter +o-excl+ #x80)
(defparameter +o-trunc+ #x200)
(defparameter +o-cloexec+ #x80000)
(defparameter +o-path+ #x200000)
(defparameter +o-directory+
  #+(or arm arm64) #o40000
  #-(or arm arm64) #o200000)
(defparameter +o-nofollow+
  #+(or arm arm64) #o100000
  #-(or arm arm64) #o40000)
(defparameter +at-symlink-nofollow+ #x100)
(defparameter +at-removedir+ #x200)
(defparameter +at-symlink-follow+ #x400)
(defparameter +utime-now+ 1073741823)
(defparameter +utime-omit+ 1073741822)
(defparameter +s-ifmt+ #o170000)
(defparameter +s-ifreg+ #o100000)
(defparameter +s-ifdir+ #o40000)
(defparameter +s-iflnk+ #o120000)
(defparameter +s-ifchr+ #o20000)
(defparameter +s-ifblk+ #o60000)
(defparameter +s-ififo+ #o10000)
(defparameter +s-ifsock+ #o140000)

;;; ---------------------------------------------------------------
;;; errno -> the WIT error-code variant's cases.

(defparameter *errno->error-code*
  '((1 . "not-permitted")               ; EPERM
    (2 . "no-entry")                    ; ENOENT
    (4 . "interrupted")                 ; EINTR
    (5 . "io")                          ; EIO
    (6 . "no-such-device")              ; ENXIO
    (9 . "bad-descriptor")              ; EBADF
    (12 . "insufficient-memory")        ; ENOMEM
    (13 . "access")                     ; EACCES
    (16 . "busy")                       ; EBUSY
    (17 . "exist")                      ; EEXIST
    (18 . "cross-device")               ; EXDEV
    (19 . "no-device")                  ; ENODEV
    (20 . "not-directory")              ; ENOTDIR
    (21 . "is-directory")               ; EISDIR
    (22 . "invalid")                    ; EINVAL
    (23 . "other")                      ; ENFILE
    (24 . "other")                      ; EMFILE
    (25 . "no-tty")                     ; ENOTTY
    (26 . "text-file-busy")             ; ETXTBSY
    (27 . "file-too-large")             ; EFBIG
    (28 . "insufficient-space")         ; ENOSPC
    (29 . "invalid-seek")               ; ESPIPE
    (30 . "read-only")                  ; EROFS
    (31 . "too-many-links")             ; EMLINK
    (32 . "pipe")                       ; EPIPE
    (35 . "deadlock")                   ; EDEADLK
    (36 . "name-too-long")              ; ENAMETOOLONG
    (37 . "no-lock")                    ; ENOLCK
    (38 . "unsupported")                ; ENOSYS
    (39 . "not-empty")                  ; ENOTEMPTY
    (40 . "loop")                       ; ELOOP
    (75 . "overflow")                   ; EOVERFLOW
    (84 . "illegal-byte-sequence")      ; EILSEQ
    (90 . "message-size")               ; EMSGSIZE
    (95 . "unsupported")                ; EOPNOTSUPP
    (114 . "already")                   ; EALREADY
    (115 . "in-progress")               ; EINPROGRESS
    (122 . "quota")                     ; EDQUOT
    (131 . "not-recoverable"))          ; ENOTRECOVERABLE
  "The Linux errno -> error-code case labels. Anything absent is the
variant's `other' case (the WIT's catch-all).")

(defparameter +enotdir+ 20)
(defparameter +eloop+ 40)

(defun errno->error-code (errno)
  "The WIT error-code case label for ERRNO."
  (or (cdr (assoc errno *errno->error-code*)) "other"))

(define-condition wasi-fs-error (wasmdown:wasmdown-error)
  ((code :initarg :code :reader wasi-fs-error-code))
  (:report (lambda (c s)
             (format s "wasi:filesystem error: ~a"
                     (wasi-fs-error-code c))))
  (:documentation "The internal short-circuit of a filesystem
operation: the provider's method wrapper turns it into the WIT's
`(:err . code)' result. A capability violation (an escaping path,
a descriptor of another authority) signals it too."))

(defun %fs-error (code)
  (error 'wasi-fs-error :code code))

(defmacro %fs-result (&body body)
  "Run BODY; its value is the ok payload: (:ok . value). A
wasi-fs-error becomes (:err . code)."
  `(handler-case (cons :ok (progn ,@body))
     (wasi-fs-error (c) (cons :err (wasi-fs-error-code c)))))

(defun %fs-check (ret what)
  "Turn a libc -1 return into the mapped wasi-fs-error (the errno is
read only on failure: a successful call leaves it stale)."
  (when (minusp ret)
    (%fs-error (errno->error-code (sb-alien:get-errno))))
  ret)

;;; ---------------------------------------------------------------
;;; The authority objects.

(defstruct wasi-preopen
  "A WASI file root: an OPEN HOST DIRECTORY HANDLE (PLAN.md §9.1) and
the guest-visible path string the guest knows it by. Every operation
on it is handle-relative; there is no path-prefix check to bypass."
  (fd -1)
  (name "")
  (descriptor nil))

(defstruct wasi-descriptor
  "One open descriptor (the `descriptor' resource's rep): the host fd,
the preopen it is confined to, the guest-visible descriptor-flags
labels, whether the fd belongs to the descriptor (a preopen's does
not — dropping the guest's handle must not close the root), whether
it names a directory, and the live REFERENCE COUNT.

REFS counts every holder of the fd: the descriptor handle's own
reference (its initial one) plus one per live stream end. The fd is
closed exactly once, when the last holder releases it. CLOSED-P says
no new holder may be taken: the handle was released, or the embedder
closed the preopen root."
  (fd -1)
  (preopen nil)
  (flags nil)
  (owned-p t)
  (dir-p nil)
  (refs 1)
  (closed-p nil))

(defstruct (wasi-fd-hold (:constructor %make-wasi-fd-hold (descriptor)))
  "One holder's reference to a descriptor's fd (a stream end). The
RELEASED-P marker makes the release idempotent: a stream's EOF,
error, cancel and drop paths may each run, and the count moves once."
  (descriptor nil)
  (released-p nil))

(defun %close-fd (fd)
  (when (and (integerp fd) (not (minusp fd)))
    (sb-unix:unix-close fd)))

(defun %descriptor-release (descriptor)
  "Drop one reference to DESCRIPTOR and return the remaining count.
At zero the fd is closed once (only when the descriptor owns it) and
the fd slot is invalidated, so a closed fd number can never be
reached again. A release on an already-zero descriptor is a no-op."
  (let ((refs (wasi-descriptor-refs descriptor)))
    (when (plusp refs)
      (decf refs)
      (setf (wasi-descriptor-refs descriptor) refs)
      (when (zerop refs)
        (setf (wasi-descriptor-closed-p descriptor) t)
        (let ((fd (wasi-descriptor-fd descriptor)))
          (setf (wasi-descriptor-fd descriptor) -1)
          (when (and (wasi-descriptor-owned-p descriptor)
                     (integerp fd) (not (minusp fd)))
            (%close-fd fd)))))
    refs))

(defun %descriptor-hold (descriptor)
  "Take a reference on DESCRIPTOR's fd for one stream end."
  (incf (wasi-descriptor-refs descriptor))
  (%make-wasi-fd-hold descriptor))

(defun %fd-hold-release (hold)
  "Release HOLD once (a second release is a no-op) and return the
descriptor's remaining reference count."
  (let ((descriptor (wasi-fd-hold-descriptor hold)))
    (when (and descriptor (not (wasi-fd-hold-released-p hold)))
      (setf (wasi-fd-hold-released-p hold) t)
      (%descriptor-release descriptor))
    (and descriptor (wasi-descriptor-refs descriptor))))

(defun close-wasi-descriptor (descriptor)
  "Release DESCRIPTOR's own handle reference: the fd closes when the
last live stream end has released its reference too. Idempotent.
Only an owned descriptor is released; a preopen root's descriptor is
embedder-owned (close-wasi-preopen releases it), so dropping the
guest's handle must not close the root."
  (when (wasi-descriptor-p descriptor)
    (when (and (wasi-descriptor-owned-p descriptor)
               (not (wasi-descriptor-closed-p descriptor)))
      ;; the handle names nothing any more, even while a live stream
      ;; keeps the fd itself open
      (setf (wasi-descriptor-closed-p descriptor) t)
      (%descriptor-release descriptor))
    descriptor))

(defun %open-host-directory (path)
  "Open PATH as a directory fd (the capability's root handle)."
  (let ((fd (%libc-open (namestring path)
                        (logior +o-rdonly+ +o-directory+ +o-cloexec+)
                        0)))
    (when (minusp fd)
      (error 'wasmdown:host-error
             :message (format nil "cannot open preopen directory ~a (errno ~a)"
                              path (sb-alien:get-errno))))
    fd))

(defun open-wasi-preopen (host-path &key name)
  "Establish a file root: open HOST-PATH once as a directory handle.
NAME is the guest-visible preopen name (defaults to the host path).
The caller owns the preopen; close-wasi-preopen releases the fd."
  (let* ((fd (%open-host-directory host-path))
         (pre (make-wasi-preopen :fd fd :name (or name (namestring host-path))))
         (desc (make-wasi-descriptor
                :fd fd
                :preopen pre
                :flags '("read" "write" "mutate-directory")
                :owned-p nil
                :dir-p t)))
    (setf (wasi-preopen-descriptor pre) desc)
    pre))

(defun close-wasi-preopen (preopen)
  "Release the preopen's host directory handle and INVALIDATE the
cached descriptor, so a later open-at/stat through that preopen is a
bad-descriptor instead of reaching a file through a closed (and
possibly reused) fd number. Idempotent."
  (when (wasi-preopen-p preopen)
    (let ((fd (wasi-preopen-fd preopen)))
      (when (and (integerp fd) (not (minusp fd)))
        (setf (wasi-preopen-fd preopen) -1)
        (%close-fd fd)))
    (let ((descriptor (wasi-preopen-descriptor preopen)))
      (when (wasi-descriptor-p descriptor)
        (setf (wasi-descriptor-closed-p descriptor) t
              (wasi-descriptor-fd descriptor) -1))))
  preopen)

(defun descriptor-resource-ct (&key (package (wasi-package "filesystem")))
  "The `descriptor' resource's ct for PACKAGE — the handle table's
type identity for every descriptor handle (the provider functypes,
the exported type and the drop finalizer all share it)."
  (wasmdown.wit:wit-type->ct
   (wasmdown.wit:package-interface package "types")
   (list :resource "descriptor")
   :packages (list package)))

;;; M17: there is NO provider-side "already installed" ledger any more.
;;; The descriptor's host finalizer (CLOSE-WASI-DESCRIPTOR, run by a
;;; guest's canon resource.drop with the rep) is registered with
;;; HOST-REGISTER-RESOURCE-DTOR on each provider instance that owns
;;; descriptor reps — FILESYSTEM-TYPES-INSTANCE and
;;; FILESYSTEM-PREOPENS-INSTANCE — so the registry is per (instance,
;;; ct): two providers of wasi:filesystem keep distinct releases.

;;; ---------------------------------------------------------------
;;; The descriptor domain boundary: own/borrow handles.

(defun %descriptor-of (context value ct)
  "The wasi-descriptor a method's self/borrow argument denotes:
VALUE is either the rep itself (a direct CL call / a nested call) or
the handle INDEX the canon-lower lift produced, resolved on the
dynamic CALLER's handle table through the resource ct."
  (let ((desc
          (cond
            ((wasi-descriptor-p value) value)
            ((integerp value)
             (or (wasmdown.component.runtime:host-resource-rep ct value)
                 (%fs-error "bad-descriptor")))
            (t (%fs-error "invalid")))))
    (unless (wasi-descriptor-p desc)
      (%fs-error "bad-descriptor"))
    ;; a released handle (or a preopen root the embedder closed) names
    ;; no live fd any more — never hand out an authority through it
    (when (wasi-descriptor-closed-p desc)
      (%fs-error "bad-descriptor"))
    ;; the authority check: the descriptor must belong to a preopen
    ;; of THIS provider's context — descriptors never travel between
    ;; differently-authorized providers.
    (unless (member (wasi-descriptor-preopen desc)
                    (wasi-context-preopens context))
      (%fs-error "not-permitted"))
    desc))

(defun %descriptor-fd (desc)
  (when (wasi-descriptor-closed-p desc)
    (%fs-error "bad-descriptor"))
  (let ((fd (wasi-descriptor-fd desc)))
    (unless (and (integerp fd) (not (minusp fd)))
      (%fs-error "bad-descriptor"))
    fd))

(defun %flag-set (v)
  "The label list of a flags domain value (:flags (\"read\" ...)), a
bare list, or NIL."
  (cond ((null v) '())
        ((and (consp v) (eq (car v) :flags)) (cdr v))
        ((listp v) v)
        (t (list v))))

(defun %descriptor-writable-p (desc)
  (let ((flags (wasi-descriptor-flags desc)))
    (or (member "write" flags :test #'string=)
        (member "mutate-directory" flags :test #'string=))))

(defun %require-mutable (desc)
  (unless (%descriptor-writable-p desc)
    (%fs-error "read-only")))

;;; ---------------------------------------------------------------
;;; Path validation + the handle-relative resolver.

(defun %path-components (path)
  "PATH's components after dropping empty and `.' pieces. Signals the
WIT's capability errors: empty -> invalid, absolute -> not-permitted,
NUL -> invalid, `..' -> not-permitted (the conservative rule: the
guest may never climb, so no symbolic-link or `..' step can leave the
preopen)."
  (unless (stringp path)
    (%fs-error "invalid"))
  (when (zerop (length path))
    (%fs-error "invalid"))
  (when (char= (char path 0) #\/)
    (%fs-error "not-permitted"))
  (when (find (code-char 0) path)
    (%fs-error "invalid"))
  (let ((parts '())
        (start 0))
    (loop for i from 0 to (length path)
          do (when (or (= i (length path)) (char= (char path i) #\/))
               (let ((part (subseq path start i)))
                 (setf start (1+ i))
                 (cond
                   ((or (zerop (length part)) (string= part "."))
                    nil)
                   ((string= part "..")
                    (%fs-error "not-permitted"))
                   (t (push part parts))))))
    (nreverse parts)))

(defun %readlink-raw (dirfd name)
  "The raw target text of the symlink (DIRFD, NAME)."
  (let* ((size 4096)
         (buf (sb-alien:make-alien sb-alien:unsigned-char size)))
    (unwind-protect
         (let ((n (%libc-readlinkat dirfd name buf size)))
           (when (minusp n)
             (%fs-error (errno->error-code (sb-alien:get-errno))))
           (when (or (zerop n) (= n size))
             (%fs-error "invalid"))
           (let ((target (make-string n)))
             (dotimes (i n)
               (setf (char target i)
                     (code-char (sb-sys:sap-ref-8
                                 (sb-alien:alien-sap buf) i))))
             target))
      (sb-alien:free-alien buf))))

(defun %readlink-components (dirfd name)
  "The components of the symlink at (DIRFD, NAME); an absolute or
`..'-bearing target is refused (not-permitted)."
  (let ((target (%readlink-raw dirfd name)))
    (when (char= (char target 0) #\/)
      (%fs-error "not-permitted"))
    (%path-components target)))

(defparameter *max-symlink-expansions* 32
  "The symlink budget of one path resolution (a bounded walk instead
of the kernel's ELOOP-only feedback).")

(defun %resolve-path (desc path path-flags)
  "Resolve PATH relative to DESC's fd, one component at a time.
Returns (values DIR-FD LEAF OWNS-DIR-P): DIR-FD is the directory the
operation applies to, LEAF its final component (\".\" when PATH names
the descriptor itself), and OWNS-DIR-P says whether the caller must
close DIR-FD. Every component is opened O_NOFOLLOW — the kernel never
follows a symlink for us — and any symlink is expanded here, so a
link swapped in mid-walk can only make the operation FAIL, never
escape. PATH-FLAGS is the WIT flags domain value; `symlink-follow'
expands the final component too."
  (let* ((parts (%path-components path))
         (follow (member "symlink-follow" (%flag-set path-flags)
                         :test #'string=))
         (base (%descriptor-fd desc))
         (cur base)
         (owned nil)
         (queue parts)
         (links 0))
    (labels
        ((close-cur ()
           (when owned
             (when (not (minusp cur)) (%close-fd cur))
             (setf owned nil)))
         (drop-everything (code)
           (close-cur)
           (%fs-error code))
         (expand (name)
           (incf links)
           (when (> links *max-symlink-expansions*)
             (drop-everything "loop"))
           (handler-case (%readlink-components cur name)
             (wasi-fs-error (c)
               (drop-everything (wasi-fs-error-code c)))))
         (open-component (name flags)
           (let ((fd (%libc-openat cur name flags 0)))
             (if (minusp fd) -1 fd)))
         (fail-errno ()
           (let ((errno (sb-alien:get-errno)))
             (cond
               ((= errno +eloop+) :link)
               ((= errno +enotdir+) :not-dir)
               (t (drop-everything (errno->error-code errno)))))))
      (loop
        (cond
          ((null queue)
           (return (values cur "." owned)))
          ((null (rest queue))
           ;; the final component: expand it when requested
           (when follow
             (loop
               (when (or (null queue) (rest queue)) (return))
               (let ((probe (open-component (first queue)
                                  (logior +o-path+ +o-nofollow+ +o-cloexec+))))
                 (cond
                   ((not (minusp probe))
                    (let ((mode (nth-value 3 (sb-unix:unix-fstat probe))))
                      (%close-fd probe)
                      (if (and mode (= (logand mode +s-ifmt+) +s-iflnk+))
                          (setf queue (expand (first queue)))
                          (return))))
                   ((= (sb-alien:get-errno) +eloop+)
                    (setf queue (expand (first queue))))
                   (t (return))))))
           (when (or (null queue) (null (rest queue)))
             (return (values cur (if queue (first queue) ".") owned)))
           ;; an expansion grew the queue: keep walking
           nil)
          (t
           ;; an intermediate component: it must be a directory
           (let ((fd (open-component (first queue)
                           (logior +o-rdonly+ +o-directory+
                                   +o-nofollow+ +o-cloexec+))))
             (cond
               ((not (minusp fd))
                (close-cur)
                (setf cur fd owned t)
                (setf queue (rest queue)))
               (t
                (case (fail-errno)
                  (:link
                   (setf queue (append (expand (first queue))
                                       (rest queue))))
                  (:not-dir
                   ;; a symlink opened with O_NOFOLLOW|O_DIRECTORY
                   ;; reports ENOTDIR; expand it when it IS a link so
                   ;; the error (or the resolution) is the honest one.
                   ;; A POLICY refusal (an absolute/climbing target)
                   ;; must keep its code — collapsing it to
                   ;; not-directory would hide the escape attempt.
                   (handler-case
                       (setf queue
                             (append (%readlink-components cur (first queue))
                                     (rest queue)))
                     (wasi-fs-error (c)
                       (let ((code (wasi-fs-error-code c)))
                         (drop-everything
                          (if (string= code "invalid")
                              "not-directory"
                              code))))))
                  (t nil)))))))))))

(defmacro %with-resolved ((dir leaf owned) (desc path flags) &body body)
  "Resolve DESC's PATH with FLAGS into (DIR LEAF OWNED) and close the
borrowed directory fd on exit."
  `(multiple-value-bind (,dir ,leaf ,owned)
       (%resolve-path ,desc ,path ,flags)
     (unwind-protect (progn ,@body)
       (when ,owned (%close-fd ,dir)))))

;;; ---------------------------------------------------------------
;;; Handle-relative primitives.

(defun %fstat (fd)
  "FD's (values dev ino mode nlink size mtime). SBCL's unix-fstat
returns (t dev ino mode nlink uid gid rdev size atime mtime ctime
blksize blocks) on success and (nil errno) on failure — the second
slot is DEV, not an errno (a shift here made every type read as
`other')."
  (multiple-value-bind (ok dev ino mode nlink uid gid rdev
                        size atime mtime ctime blksize blocks)
      (sb-unix:unix-fstat fd)
    (declare (ignore uid gid rdev atime ctime blksize blocks))
    (unless ok
      ;; on failure the second value is the errno
      (%fs-error (errno->error-code dev)))
    (values dev ino mode nlink size mtime)))

(defun %mode->type (mode)
  "The descriptor-type VARIANT domain value: (case-name . payload)."
  (let ((kind (logand mode +s-ifmt+)))
    (cond ((= kind +s-ifreg+) (cons "regular-file" nil))
          ((= kind +s-ifdir+) (cons "directory" nil))
          ((= kind +s-iflnk+) (cons "symbolic-link" nil))
          ((= kind +s-ifchr+) (cons "character-device" nil))
          ((= kind +s-ifblk+) (cons "block-device" nil))
          ((= kind +s-ififo+) (cons "fifo" nil))
          ((= kind +s-ifsock+) (cons "socket" nil))
          (t (cons "other" :none)))))

(defun %instant (seconds nanoseconds)
  (list (cons "seconds" seconds) (cons "nanoseconds" nanoseconds)))

(defun %stat-record (dev ino mode nlink size mtime)
  "The WIT descriptor-stat domain value for the fstat tuple."
  (declare (ignore dev ino))
  (list (cons "%type" (%mode->type mode))
        (cons "link-count" nlink)
        (cons "size" size)
        (cons "data-access-timestamp" (cons :some (%instant mtime 0)))
        (cons "data-modification-timestamp" (cons :some (%instant mtime 0)))
        (cons "status-change-timestamp" (cons :some (%instant mtime 0)))))

(defun %stat-at (desc path flags)
  "fstatat emulated with O_PATH + fstat (SBCL's stat wrapper is
portable; the struct layout is not). The resolver has already decided
the symlink policy, so the probe never follows a link."
  (%with-resolved (dir leaf owned) (desc path flags)
    (let ((fd (%libc-openat dir leaf (logior +o-path+ +o-nofollow+
                                             +o-cloexec+) 0)))
      (when (minusp fd)
        (%fs-error (errno->error-code (sb-alien:get-errno))))
      (unwind-protect
           (%fstat fd)
        (%close-fd fd)))))

(defun %metadata-hash (dev ino mtime size)
  "A 128-bit split hash of the metadata the WIT suggests (dev/ino/
mtime/size mixed through a 64-bit avalanche)."
  (let* ((x (logand (+ (* dev 1000003) (* ino 31)
                       (* mtime 2654435761) size)
                    #xffffffffffffffff))
         (x (logand (* (logxor x (ash x -33)) #xff51afd7ed558ccd)
                    #xffffffffffffffff))
         (x (logand (* (logxor x (ash x -33)) #xc4ceb9fe1a85ec53)
                    #xffffffffffffffff))
         (h (logxor x (ash x -33))))
    (list (cons "lower" (logand h #xffffffffffffffff))
          (cons "upper" 0))))

(defun %timestamp->timespec (value)
  "The (sec . nsec) a new-timestamp domain value denotes; UTIME_NOW /
UTIME_OMIT are the sentinels."
  (let ((case (car value)))
    (cond
      ((string= case "no-change") (cons 0 +utime-omit+))
      ((string= case "now") (cons 0 +utime-now+))
      ((string= case "timestamp")
       (let ((instant (cdr value)))
         (cons (or (cdr (assoc "seconds" instant :test #'string=)) 0)
               (or (cdr (assoc "nanoseconds" instant :test #'string=)) 0))))
      (t (cons 0 +utime-now+)))))

(defun %timespec-pair (atime mtime)
  "Allocate the two-timespec array FUTIMENS/UTIMENSAT take."
  (let ((p (sb-alien:make-alien (sb-alien:struct %wasi-timespec-pair))))
    (flet ((set1 (slot spec)
             (let ((s (sb-alien:slot (sb-alien::deref p) slot)))
               (setf (sb-alien:slot s 'tv-sec) (car spec))
               (setf (sb-alien:slot s 'tv-nsec) (cdr spec)))))
      (set1 'a (%timestamp->timespec atime))
      (set1 'b (%timestamp->timespec mtime)))
    p))

;;; ---------------------------------------------------------------
;;; The WIT packages the filesystem functypes resolve against.

(defun %filesystem-packages (&key (package (wasi-package "filesystem")))
  "The WIT packages the filesystem functypes resolve against:
filesystem itself and wasi:clocks (descriptor-stat's `instant')."
  (list package (wasi-package "clocks")))

;;; ---------------------------------------------------------------
;;; The descriptor methods (the WIT's async surface; the provider
;;; wraps each in the runtime's async-typed functype, so a guest's
;;; canon-lowered call goes through the canonical task protocol).

(defun fs-stat (context self)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (multiple-value-bind (dev ino mode nlink size mtime)
         (%fstat (%descriptor-fd desc))
       (%stat-record dev ino mode nlink size mtime)))))

(defun fs-stat-at (context self path-flags path)
  (%fs-result
   (%descriptor-of context self (descriptor-resource-ct))
   (multiple-value-bind (dev ino mode nlink size mtime)
       (%stat-at (%descriptor-of context self (descriptor-resource-ct))
                 path path-flags)
     (%stat-record dev ino mode nlink size mtime))))

(defun fs-get-type (context self)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (%mode->type (nth-value 2 (%fstat (%descriptor-fd desc)))))))

(defun fs-get-flags (context self)
  (%fs-result
   (cons :flags (copy-list
                 (wasi-descriptor-flags
                  (%descriptor-of context self (descriptor-resource-ct)))))))

(defun fs-set-size (context self size)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (%require-mutable desc)
     (%fs-check (%libc-ftruncate (%descriptor-fd desc) size) "ftruncate")
     nil)))

(defun fs-is-same-object (context self other)
  "The WIT returns a plain bool (no result wrapper). A bad handle is a
host error, not an error-code."
  (let* ((ct (descriptor-resource-ct))
         (a (%descriptor-of context self ct))
         (b (%descriptor-of context other ct)))
    (multiple-value-bind (adev aino) (%fstat (%descriptor-fd a))
      (multiple-value-bind (bdev bino) (%fstat (%descriptor-fd b))
        (and (= adev bdev) (= aino bino))))))

(defun fs-metadata-hash (context self)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (multiple-value-bind (dev ino mode nlink size mtime)
         (%fstat (%descriptor-fd desc))
       (declare (ignore mode nlink))
       (%metadata-hash dev ino mtime size)))))

(defun fs-metadata-hash-at (context self path-flags path)
  (%fs-result
   (multiple-value-bind (dev ino mode nlink size mtime)
       (%stat-at (%descriptor-of context self (descriptor-resource-ct))
                 path path-flags)
     (declare (ignore mode nlink))
     (%metadata-hash dev ino mtime size))))

(defun fs-create-directory-at (context self path)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (%require-mutable desc)
     (%with-resolved (dir leaf owned) (desc path nil)
       (%fs-check (%libc-mkdirat dir leaf #o755) "mkdirat")
       nil))))

(defun fs-remove-directory-at (context self path)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (%require-mutable desc)
     (%with-resolved (dir leaf owned) (desc path nil)
       (%fs-check (%libc-unlinkat dir leaf +at-removedir+) "unlinkat")
       nil))))

(defun fs-unlink-file-at (context self path)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (%require-mutable desc)
     (%with-resolved (dir leaf owned) (desc path nil)
       (%fs-check (%libc-unlinkat dir leaf 0) "unlinkat")
       nil))))

(defun fs-rename-at (context self old-path new-descriptor new-path)
  (%fs-result
   (let* ((ct (descriptor-resource-ct))
          (old (%descriptor-of context self ct))
          (new (%descriptor-of context new-descriptor ct)))
     (%require-mutable old)
     (%require-mutable new)
     (%with-resolved (odir oleaf oowned) (old old-path nil)
       (%with-resolved (ndir nleaf nowned) (new new-path nil)
         (%fs-check (%libc-renameat odir oleaf ndir nleaf) "renameat")
         nil)))))

(defun fs-readlink-at (context self path)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (%with-resolved (dir leaf owned) (desc path nil)
       (let ((target (%readlink-raw dir leaf)))
         ;; the WIT: an absolute or rooted target is not-permitted
         ;; (the guest must not learn a path it could use to reason
         ;; about escaping, and the value must stay preopen-relative)
         (when (char= (char target 0) #\/)
           (%fs-error "not-permitted"))
         (%path-components target)
         target)))))

(defun fs-symlink-at (context self old-path new-path)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (%require-mutable desc)
     (when (and (plusp (length old-path)) (char= (char old-path 0) #\/))
       (%fs-error "not-permitted"))
     (%path-components old-path)  ; the target must not climb either
     (%with-resolved (dir leaf owned) (desc new-path nil)
       (%fs-check (%libc-symlinkat old-path dir leaf) "symlinkat")
       nil))))

(defun fs-open-at (context self path-flags path open-flags flags)
  (%fs-result
   (let* ((desc (%descriptor-of context self (descriptor-resource-ct)))
          (oflags (%flag-set open-flags))
          (dflags (%flag-set flags))
          (create (member "create" oflags :test #'string=))
          (directory (member "directory" oflags :test #'string=))
          (exclusive (member "exclusive" oflags :test #'string=))
          (truncate (member "truncate" oflags :test #'string=))
          (write-p (member "write" dflags :test #'string=)))
     ;; the WIT's read-only rule: creating, truncating, writing or
     ;; mutating through a descriptor that lacks mutate-directory is
     ;; refused with read-only
     (when (and (or create truncate write-p
                    (member "mutate-directory" dflags :test #'string=))
                (not (member "mutate-directory"
                             (wasi-descriptor-flags desc) :test #'string=)))
       (%fs-error "read-only"))
     (let* ((access (cond ((and (member "read" dflags :test #'string=)
                                write-p)
                           +o-rdwr+)
                          (write-p +o-wronly+)
                          (t +o-rdonly+)))
            ;; a directory is opened read-only (the OS refuses write
            ;; modes on directories); the guest flags still record the
            ;; intent
            (access (if directory +o-rdonly+ access))
            (oflags-bits (logior access +o-cloexec+ +o-nofollow+
                                 (if create +o-creat+ 0)
                                 (if directory +o-directory+ 0)
                                 (if exclusive +o-excl+ 0)
                                 (if truncate +o-trunc+ 0))))
       (%with-resolved (dir leaf owned) (desc path path-flags)
         (let ((fd (%libc-openat dir leaf oflags-bits #o666)))
           (when (minusp fd)
             (%fs-error (errno->error-code (sb-alien:get-errno))))
           (let* ((mode (nth-value 3 (sb-unix:unix-fstat fd)))
                  (is-dir (and mode (= (logand mode +s-ifmt+) +s-ifdir+)))
                  (opened (make-wasi-descriptor
                           :fd fd
                           :preopen (wasi-descriptor-preopen desc)
                           :flags (copy-list dflags)
                           :owned-p t
                           :dir-p is-dir)))
             (list :own-handle (descriptor-resource-ct) opened))))))))

(defun fs-advise (context self offset length advice)
  (%fs-result
   (let* ((desc (%descriptor-of context self (descriptor-resource-ct)))
          (a (if (and (consp advice) (eq (car advice) :enum))
                 (cdr advice)
                 advice))
          (code (cond ((string= a "normal") 0)
                      ((string= a "sequential") 2)
                      ((string= a "random") 1)
                      ((string= a "will-need") 3)
                      ((string= a "dont-need") 4)
                      ((string= a "no-reuse") 5)
                      (t 0)))
          (ret (%libc-fadvise (%descriptor-fd desc) offset length code)))
     ;; posix_fadvise RETURNS the error number (it does not set errno)
     (unless (zerop ret)
       (%fs-error (errno->error-code ret)))
     nil)))

(defun fs-sync (context self)
  (%fs-result
   (%fs-check (%libc-fsync (%descriptor-fd
                            (%descriptor-of context self
                                            (descriptor-resource-ct))))
              "fsync")
   nil))

(defun fs-sync-data (context self)
  (%fs-result
   (%fs-check (%libc-fdatasync (%descriptor-fd
                                (%descriptor-of context self
                                                (descriptor-resource-ct))))
              "fdatasync")
   nil))

(defun fs-set-times (context self atime mtime)
  (%fs-result
   (let* ((desc (%descriptor-of context self (descriptor-resource-ct)))
          (pair (%timespec-pair atime mtime)))
     (unwind-protect
          (progn
            (%fs-check (%libc-futimens
                        (%descriptor-fd desc)
                        (sb-alien:addr
                         (sb-alien:slot (sb-alien::deref pair) 'a)))
                       "futimens")
            nil)
       (sb-alien:free-alien pair)))))

(defun fs-set-times-at (context self path-flags path atime mtime)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (when (null (%descriptor-writable-p desc))
       (%fs-error "read-only"))
     (%with-resolved (dir leaf owned) (desc path path-flags)
       (let ((pair (%timespec-pair atime mtime)))
         (unwind-protect
              (progn
                ;; the resolver already applied the symlink policy;
                ;; NOFOLLOW makes a raced-in link fail closed
                (%fs-check (%libc-utimensat
                            dir leaf
                            (sb-alien:addr
                             (sb-alien:slot (sb-alien::deref pair) 'a))
                            +at-symlink-nofollow+)
                           "utimensat")
                nil)
           (sb-alien:free-alien pair)))))))

(defun fs-link-at (context self old-path-flags old-path
                   new-descriptor new-path)
  (%fs-result
   (let* ((ct (descriptor-resource-ct))
          (old (%descriptor-of context self ct))
          (new (%descriptor-of context new-descriptor ct))
          (follow (member "symlink-follow" (%flag-set old-path-flags)
                          :test #'string=)))
     (%require-mutable new)
     (%with-resolved (odir oleaf oowned) (old old-path nil)
       (%with-resolved (ndir nleaf nowned) (new new-path nil)
         (%fs-check (%libc-linkat odir oleaf ndir nleaf
                                  (if follow +at-symlink-follow+ 0))
                    "linkat")
         nil)))))

;;; ---------------------------------------------------------------
;;; M16 slice 3b — THE STREAM SURFACE: read/write/append-via-stream
;;; and read-directory over the M16 slice-2 host stream/future ABI.
;;;
;;; Each method hands the guest one end of a host-backed pair and
;;; keeps the other: a READ-VIA-STREAM's guest end pulls pread() bytes
;;; at the guest's offset (the kernel file offset is never touched, so
;;; several streams on one descriptor do not interfere); a
;;; WRITE/APPEND-VIA-STREAM's guest end pushes bytes into a pwrite()
;;; sink. The paired future resolves (:ok) at EOF / when the guest
;;; drops the data end, (:err . code) on an OS error. The descriptor's
;;; descriptor-flags gate all of it — the WIT's own error case.

(defparameter *fs-stream-chunk* 4096
  "The filesystem source's per-call read bound (the canonical ABI
allows a short read; the guest simply asks again).")

(defun %fs-u8-ct ()
  "The stream<u8>'s ELEMENT ct (the pair's t-ct)."
  (wasmdown.component:make-ct :kind :prim :payload :u8))

(defun %filesystem-method-ct (method-name
                              &key (package (wasi-package "filesystem")))
  "The component functype of descriptor's METHOD-NAME as the WIT
declares it (the implicit self borrow included)."
  (let* ((interface (wasmdown.wit:package-interface package "types"))
         (resource (find "descriptor"
                         (wasmdown.wit:wit-interface-resource-list interface)
                         :key #'wasmdown.wit:wit-resource-name
                         :test #'string=))
         (method (and resource
                      (find method-name
                            (wasmdown.wit:wit-resource-methods resource)
                            :key #'wasmdown.wit:wit-func-name
                            :test #'string=))))
    (unless method
      (error 'wasmdown:host-error
             :message (format nil
                              "wasi:filesystem/types has no descriptor.~a"
                              method-name)))
    (%resource-method-ct interface resource method
                         (%filesystem-packages :package package))))

(defun %fs-future-ct (method-name
                      &key (package (wasi-package "filesystem")))
  "The ELEMENT ct of the method's `future<result<_, error-code>>'
(the result itself, or the future inside a tuple result)."
  (let* ((ft (%filesystem-method-ct method-name :package package))
         (result (second (wasmdown.component:ct-payload ft))))
    (or (and result
             (eq (wasmdown.component:ct-kind result) :future)
             (wasmdown.component:ct-payload result))
        (and result
             (eq (wasmdown.component:ct-kind result) :tuple)
             (let ((future
                     (find-if (lambda (c)
                                (eq (wasmdown.component:ct-kind c) :future))
                              (coerce (wasmdown.component:ct-payload result)
                                      'list))))
               (and future (wasmdown.component:ct-payload future))))
        (error 'wasmdown:host-error
               :message (format nil "descriptor.~a has no future result"
                                method-name)))))

(defun %fs-stream-pair-cts (method-name
                            &key (package (wasi-package "filesystem")))
  "The (values ELEMENT-CT FUTURE-ELEMENT-CT) of a method whose result
is tuple<stream<T>, future<result<_, error-code>>>."
  (let* ((ft (%filesystem-method-ct method-name :package package))
         (result (second (wasmdown.component:ct-payload ft)))
         (parts (and result
                     (coerce (wasmdown.component:ct-payload result) 'list)))
         (stream (find-if (lambda (c)
                            (eq (wasmdown.component:ct-kind c) :stream))
                          parts)))
    (unless stream
      (error 'wasmdown:host-error
             :message (format nil "descriptor.~a has no stream result"
                              method-name)))
    (values (wasmdown.component:ct-payload stream)
            (%fs-future-ct method-name :package package))))

(defun %require-read-capability (desc)
  "The WIT's read gate: a descriptor without the read flag cannot
produce bytes (its error case, never a trap)."
  (unless (member "read" (wasi-descriptor-flags desc) :test #'string=)
    (%fs-error "not-permitted")))

(defun %require-write-capability (desc)
  "The WIT's write gate: a descriptor without the write flag cannot
produce a writable stream."
  (unless (member "write" (wasi-descriptor-flags desc) :test #'string=)
    (%fs-error "read-only")))

(defmacro %fs-settle-once (done-var writer value)
  `(unless ,done-var
     (setf ,done-var t)
     (wasmdown.component.runtime:host-future-resolve ,writer ,value)))

(defun %fd-pread-source (fd offset settle)
  "A host SOURCE over pread(FD, ..., POS): (max) -> (values octets
eof-p). OCTETS is a REUSABLE u8 vector whose length is the count; one
pread(2) per attempt, so at most MAX (and at most *FS-STREAM-CHUNK*)
bytes and no per-call allocation. POS advances with every read and the
kernel file offset is never consulted. A short/empty read is EOF and
SETTLEs the future (:ok); an OS error SETTLEs it (:err . code) and
also reports EOF (the source drops its end, so the guest's read sees
DROPPED)."
  (let ((pos offset)
        (slice (make-array *fs-stream-chunk*
                           :element-type '(unsigned-byte 8)))
        (out (make-array *fs-stream-chunk*
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0)))
    (lambda (max)
      (let ((want (min max *fs-stream-chunk*)))
        (if (<= want 0)
            (values nil nil)
            (let ((n (sb-sys:with-pinned-objects (slice)
                       (%libc-pread fd (sb-sys:vector-sap slice) want pos))))
              (cond
                ((minusp n)
                 (funcall settle (cons :err (errno->error-code
                                             (sb-alien:get-errno))))
                 (values nil t))
                ((zerop n)
                 (funcall settle (cons :ok nil))
                 (values nil t))
                (t
                 (setf (fill-pointer out) n)
                 (replace out slice :start2 0 :end2 n)
                 (incf pos n)
                 (when (< n want)
                   (funcall settle (cons :ok nil)))
                 (values out (< n want))))))))))

(defun %byte-list->alien (bytes)
  "A fresh unsigned-char alien holding BYTES (a u8 element list)."
  (let ((buf (sb-alien:make-alien sb-alien:unsigned-char (length bytes))))
    (loop for i from 0
          for byte in bytes
          do (setf (sb-sys:sap-ref-8 (sb-alien:alien-sap buf) i)
                   (logand #xff byte)))
    buf))

(defun %fd-pwrite-sink (fd position-fn advance-fn)
  "A host SINK writing a span at POSITION-FN's offset: (octets start
end) -> (values consumed error-or-nil). ONE pwrite(2) per call: a
short write is the count ACCEPTED (the remainder continues on the
guest's next attempt), EAGAIN/EWOULDBLOCK is WOULD-BLOCK (0 NIL,
nothing consumed), and any other failure is (values 0 <error-code
label>). ADVANCE-FN is told the accepted count, so a fixed-offset
stream's cursor moves across attempts; an append stream re-reads the
file's size from POSITION-FN at every attempt (EOF-append semantics)."
  (lambda (octets start end)
    (let ((n (- end start)))
      (cond
        ((<= n 0) (values 0 nil))
        (t
         (let ((pos (funcall position-fn fd)))
           (sb-alien:set-errno 0)
           (let ((written
                   (sb-sys:with-pinned-objects (octets)
                     (%libc-pwrite fd
                                   (sb-sys:sap+ (sb-sys:vector-sap octets)
                                                start)
                                   n pos))))
             (cond
               ((plusp written)
                (funcall advance-fn pos written)
                (values written nil))
               ((zerop written) (values 0 "io"))
               (t
                (let ((errno (sb-alien:get-errno)))
                  (if (= errno +eagain+)
                      (values 0 nil)
                      (values 0 (errno->error-code errno)))))))))))))

(defun %fs-dropped-stream-and-future (elem-ct future-ct code)
  "The REFUSED shape of a stream-returning method: the guest's stream
end is already DROPPED and the paired future carries CODE (the WIT
signature has no result, so error-code travels through the future)."
  (multiple-value-bind (stream-reader stream-writer)
      (wasmdown.component.runtime:make-host-stream-pair elem-ct)
    (multiple-value-bind (future-reader future-writer)
        (wasmdown.component.runtime:make-host-future-pair future-ct)
      (wasmdown.component.runtime:host-end-drop stream-writer)
      (wasmdown.component.runtime:host-future-resolve future-writer
                                                     (cons :err code))
      (list stream-reader future-reader))))

(defun %fs-error-future (future-ct code)
  "The REFUSED shape of a future-returning method: a future already
resolved to (:err . CODE)."
  (multiple-value-bind (future-reader future-writer)
      (wasmdown.component.runtime:make-host-future-pair future-ct)
    (wasmdown.component.runtime:host-future-resolve future-writer
                                                   (cons :err code))
    future-reader))

(defun fs-read-via-stream (context self offset
                           &key (package (wasi-package "filesystem")))
  "pread-backed read stream: tuple<stream<u8>, future<result>>. The
kernel file offset is never consulted, so concurrent streams on one
descriptor cannot drift. A missing read flag (or a bad descriptor) is
the REFUSED shape: a dropped stream + a future with the error-code."
  (multiple-value-bind (elem-ct future-ct)
      (%fs-stream-pair-cts "read-via-stream" :package package)
    (handler-case
        (let* ((desc (%descriptor-of context self
                                     (descriptor-resource-ct :package package)))
               (fd (%descriptor-fd desc)))
          (%require-read-capability desc)
          (multiple-value-bind (stream-reader stream-writer)
              (wasmdown.component.runtime:make-host-stream-pair elem-ct)
            (multiple-value-bind (future-reader future-writer)
                (wasmdown.component.runtime:make-host-future-pair future-ct)
              ;; the stream end holds its OWN reference to the fd: the
              ;; guest may drop the descriptor handle while the stream
              ;; is still live, and the fd closes only when the last
              ;; holder (EOF/error/drop, once) has released it
              (let ((hold (%descriptor-hold desc))
                    (settled nil))
                (flet ((settle (value)
                         (unless settled
                           (setf settled t)
                           (%fd-hold-release hold)
                           (wasmdown.component.runtime:host-future-resolve
                            future-writer value))))
                  (handler-case
                      (progn
                        (wasmdown.component.runtime:host-end-set-source
                         stream-writer (%fd-pread-source fd offset #'settle))
                        (wasmdown.component.runtime:host-end-set-on-drop
                         stream-writer
                         (lambda (dropped error)
                           (declare (ignore dropped))
                           (settle (if error (cons :err error) (cons :ok nil)))))
                        (list stream-reader future-reader))
                    (error (c)
                      (%fd-hold-release hold)
                      (error c))))))))
      (wasi-fs-error (c)
        (%fs-dropped-stream-and-future elem-ct future-ct
                                       (wasi-fs-error-code c))))))

(defun %fs-write-stream (context self data future-method position-fn
                         advance-fn)
  "The write/append body: register DATA (the guest's readable end) as
a pwrite sink, and resolve the result future when the guest drops its
writable end (or a sink error drops the pair). A missing write flag
(or a bad descriptor) drops DATA and returns a future already resolved
to the error-code."
  (let ((future-ct (%fs-future-ct future-method)))
    (handler-case
        (let* ((desc (%descriptor-of context self
                                     (descriptor-resource-ct)))
               (fd (%descriptor-fd desc)))
          (%require-write-capability desc)
          (unless (wasmdown.component.runtime:async-end-p data)
            (%fs-error "invalid"))
          (multiple-value-bind (future-reader future-writer)
              (wasmdown.component.runtime:make-host-future-pair future-ct)
            ;; same reference discipline as the read stream: the sink
            ;; end keeps the fd alive independently of the handle
            (let ((hold (%descriptor-hold desc))
                  (settled nil))
              (flet ((settle (value)
                       (unless settled
                         (setf settled t)
                         (%fd-hold-release hold)
                         (wasmdown.component.runtime:host-future-resolve
                          future-writer value))))
                (handler-case
                    (progn
                      (wasmdown.component.runtime:host-end-set-sink
                       data (%fd-pwrite-sink fd position-fn advance-fn))
                      (wasmdown.component.runtime:host-end-set-on-drop
                       data
                       (lambda (dropped error)
                         (declare (ignore dropped))
                         (settle (if error (cons :err error) (cons :ok nil)))))
                      ;; the guest may have staged a write before the call
                      (wasmdown.component.runtime:host-end-drain data)
                      future-reader)
                  (error (c)
                    (%fd-hold-release hold)
                    (error c)))))))
      (wasi-fs-error (c)
        ;; the refused write: the guest's end learns DROPPED on its
        ;; next write, the future carries the code
        (when (wasmdown.component.runtime:async-end-p data)
          (wasmdown.component.runtime:host-end-drop data))
        (%fs-error-future future-ct (wasi-fs-error-code c))))))

(defun fs-write-via-stream (context self data offset)
  "pwrite-backed write stream starting at OFFSET. The cursor is the
stream's own: every byte the sink ACCEPTS (a short write is a partial
accept) moves it, so later attempts continue where the file left off."
  (let ((pos offset))
    (%fs-write-stream
     context self data "write-via-stream"
     (lambda (fd) (declare (ignore fd)) pos)
     (lambda (p n) (setf pos (+ p n))))))

(defun fs-append-via-stream (context self data)
  "append stream: every drain pwrites at the file's CURRENT size."
  (%fs-write-stream
   context self data "append-via-stream"
   (lambda (fd) (nth-value 4 (%fstat fd)))
   (lambda (p n) (declare (ignore n)) p)))

;;; read-directory: a host source of directory-entry values.

(defun %dirent-name (p)
  "The NUL-terminated name of the dirent at P."
  (let ((e (sb-alien:deref p)))
    (map 'string #'code-char
         (loop for i below 256
               for n = (sb-alien:deref (sb-alien:slot e 'd-name) i)
               until (zerop n)
               collect n))))

(defun %dirent-type (p)
  (sb-alien:slot (sb-alien:deref p) 'd-type))

(defun %dt->type (dt)
  "The descriptor-type VARIANT value for a dirent d_type, or NIL when
the filesystem declined to say (DT_UNKNOWN / an unknown value)."
  (cond
    ((= dt +dt-fifo+) (cons "fifo" nil))
    ((= dt +dt-chr+) (cons "character-device" nil))
    ((= dt +dt-dir+) (cons "directory" nil))
    ((= dt +dt-blk+) (cons "block-device" nil))
    ((= dt +dt-reg+) (cons "regular-file" nil))
    ((= dt +dt-lnk+) (cons "symbolic-link" nil))
    ((= dt +dt-sock+) (cons "socket" nil))
    (t nil)))

(defun %dirent-stat-type (dirfd name)
  "The DT_UNKNOWN fallback: the entry's type via O_PATH|O_NOFOLLOW +
fstat (never following the link — a symlink reports itself)."
  (let ((fd (%libc-openat dirfd name (logior +o-path+ +o-nofollow+
                                            +o-cloexec+) 0)))
    (if (minusp fd)
        (cons "other" nil)
        (unwind-protect
             (%mode->type (nth-value 2 (%fstat fd)))
          (%close-fd fd)))))

(defun %dirent-entry (p dirfd)
  "The directory-entry domain value for the dirent at P."
  (let ((name (%dirent-name p)))
    (list (cons "%type" (or (%dt->type (%dirent-type p))
                            (%dirent-stat-type dirfd name)))
          (cons "name" name))))

(defun %directory-source (dir dirfd settle)
  "A host SOURCE over the fdopendir stream DIR (whose fd is DIRFD):
(min) -> (values entry-vector eof-p), ENTRY-VECTOR a REUSABLE vector
whose length is the count (the typed directory-entry values, never a
byte list). `.` and `..` are omitted (the WIT's rule). At the end of
the directory SETTLE the future (:ok); a readdir error SETTLEs
(:err . code) and reports EOF."
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (lambda (max)
      (if (<= max 0)
          (values nil nil)
          (progn
            (setf (fill-pointer out) 0)
            (let ((eof nil))
              ;; the requested bound counts DELIVERED entries: `.`/`..` are
              ;; skipped inside the walk (a skipped entry must not turn the
              ;; guest's read into a zero-progress completion).
              (loop while (and (< (fill-pointer out) max) (not eof))
                    do (sb-alien:set-errno 0)
                       (let ((p (%libc-readdir dir)))
                         (cond
                           ((sb-sys:sap= (sb-alien:alien-sap p)
                                         (sb-sys:int-sap 0))
                            (setf eof t)
                            (let ((errno (sb-alien:get-errno)))
                              (if (zerop errno)
                                  (funcall settle (cons :ok nil))
                                  (funcall settle (cons :err
                                                        (errno->error-code
                                                         errno))))))
                           (t
                            (let ((name (%dirent-name p)))
                              (unless (member name '("." "..") :test #'string=)
                                (vector-push-extend (%dirent-entry p dirfd)
                                                    out)))))))
              (values out eof)))))))

(defun fs-read-directory (context self
                          &key (package (wasi-package "filesystem")))
  "A fresh directory iteration over SELF's descriptor. A missing read
flag / a non-directory descriptor is the REFUSED shape (a dropped
stream + a future with the error-code)."
  (multiple-value-bind (elem-ct future-ct)
      (%fs-stream-pair-cts "read-directory" :package package)
    (handler-case
        (let* ((desc (%descriptor-of context self
                                     (descriptor-resource-ct :package package)))
               (fd (%descriptor-fd desc)))
          (%require-read-capability desc)
          (unless (wasi-descriptor-dir-p desc)
            (%fs-error "not-directory"))
          ;; a FRESH fd for this iteration (fdopendir takes ownership:
          ;; closedir closes it), so concurrent streams never share a
          ;; directory cursor
          (let ((dfd (%libc-openat fd "." (logior +o-rdonly+ +o-directory+
                                                +o-cloexec+) 0)))
            (when (minusp dfd)
              (%fs-error (errno->error-code (sb-alien:get-errno))))
            (let ((dir (%libc-fdopendir dfd)))
              (when (sb-sys:sap= (sb-alien:alien-sap dir) (sb-sys:int-sap 0))
                (%close-fd dfd)
                (%fs-error (errno->error-code (sb-alien:get-errno))))
              (multiple-value-bind (stream-reader stream-writer)
                  (wasmdown.component.runtime:make-host-stream-pair elem-ct)
                (multiple-value-bind (future-reader future-writer)
                    (wasmdown.component.runtime:make-host-future-pair future-ct)
                  (let ((settled nil)
                        (closed nil))
                    (labels ((close-dir ()
                               (unless closed
                                 (setf closed t)
                                 (%libc-closedir dir)))
                             (settle (value)
                               (%fs-settle-once settled future-writer value)
                               (when (consp value)
                                 (close-dir))))
                      (wasmdown.component.runtime:host-end-set-source
                       stream-writer (%directory-source dir dfd #'settle))
                      (wasmdown.component.runtime:host-end-set-on-drop
                       stream-writer
                       (lambda (dropped error)
                         (declare (ignore dropped))
                         (settle (if error (cons :err error) (cons :ok nil)))
                         (close-dir)))
                      (list stream-reader future-reader))))))))
      (wasi-fs-error (c)
        (%fs-dropped-stream-and-future elem-ct future-ct
                                       (wasi-fs-error-code c))))))

;;; ---------------------------------------------------------------
;;; preopens.get-directories.

(defun fs-get-directories (context ct)
  "list<tuple<own<descriptor>, string>>: the context's preopens."
  (loop for pre in (wasi-context-preopens context)
        collect (list (list :own-handle ct (wasi-preopen-descriptor pre))
                      (wasi-preopen-name pre))))

;;; ---------------------------------------------------------------
;;; The provider instances.

(defun %bind-context (context function)
  (lambda (&rest args) (apply function context args)))

(defun filesystem-types-instance (context
                                  &key (package (wasi-package "filesystem")))
  "The wasi:filesystem/types import: the descriptor resource (its ct
exported, its methods exported as [method]descriptor.*) bound to
CONTEXT's preopen authorities."
  (let* ((ct (descriptor-resource-ct :package package))
         (inst
          (provider-instance
           (wasmdown.wit:package-interface package "types")
           (list (cons "advise" (%bind-context context #'fs-advise))
                 (cons "sync" (%bind-context context #'fs-sync))
                 (cons "sync-data" (%bind-context context #'fs-sync-data))
                 (cons "get-flags" (%bind-context context #'fs-get-flags))
                 (cons "get-type" (%bind-context context #'fs-get-type))
                 (cons "set-size" (%bind-context context #'fs-set-size))
                 (cons "set-times" (%bind-context context #'fs-set-times))
                 (cons "set-times-at" (%bind-context context #'fs-set-times-at))
                 (cons "stat" (%bind-context context #'fs-stat))
                 (cons "stat-at" (%bind-context context #'fs-stat-at))
                 (cons "create-directory-at"
                       (%bind-context context #'fs-create-directory-at))
                 (cons "remove-directory-at"
                       (%bind-context context #'fs-remove-directory-at))
                 (cons "unlink-file-at" (%bind-context context #'fs-unlink-file-at))
                 (cons "rename-at" (%bind-context context #'fs-rename-at))
                 (cons "link-at" (%bind-context context #'fs-link-at))
                 (cons "readlink-at" (%bind-context context #'fs-readlink-at))
                 (cons "symlink-at" (%bind-context context #'fs-symlink-at))
                 (cons "open-at" (%bind-context context #'fs-open-at))
                 (cons "read-via-stream"
                       (%bind-context context #'fs-read-via-stream))
                 (cons "write-via-stream"
                       (%bind-context context #'fs-write-via-stream))
                 (cons "append-via-stream"
                       (%bind-context context #'fs-append-via-stream))
                 (cons "read-directory"
                       (%bind-context context #'fs-read-directory))
                 (cons "is-same-object" (%bind-context context #'fs-is-same-object))
                 (cons "metadata-hash" (%bind-context context #'fs-metadata-hash))
                 (cons "metadata-hash-at"
                       (%bind-context context #'fs-metadata-hash-at)))
           :packages (%filesystem-packages :package package))))
    ;; M17: the finalizer is registered on THIS provider instance (the
    ;; owner of every descriptor rep it hands out), not in a ct-keyed
    ;; global.
    (wasmdown.component.runtime:host-register-resource-dtor
     inst ct #'close-wasi-descriptor)
    inst))

(defun filesystem-preopens-instance (context
                                     &key (package (wasi-package "filesystem")))
  ;; M17: the descriptor's host finalizer belongs to THIS provider
  ;; instance (the one whose types produced the ct, and whose
  ;; get-directories hands out the preopen's own descriptors), not to a
  ;; ct-keyed global — so a guest resource.drop releases through the
  ;; instance that owns the rep.
  (let* ((ct (descriptor-resource-ct :package package))
         (inst
          (provider-instance
           (wasmdown.wit:package-interface package "preopens")
           (list (cons "get-directories"
                       (lambda () (fs-get-directories context ct))))
           :packages (%filesystem-packages :package package))))
    (wasmdown.component.runtime:host-register-resource-dtor
     inst ct #'close-wasi-descriptor)
    inst))

(defun filesystem-provider (context
                            &key (package (wasi-package "filesystem")))
  "The wasi:filesystem imports as a with-args alist (types +
preopens)."
  (list
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "types"))
         (filesystem-types-instance context :package package))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "preopens"))
         (filesystem-preopens-instance context :package package))))

;;; ===============================================================
;;; wasi:sockets (M16 slice 4) — THE NETWORK CAPABILITY + the
;;; tcp-socket/udp-socket RESOURCES + ip-name-lookup.
;;;
;;; PLAN.md §9.1: "A host import must receive an explicitly allocated
;;; authority object, not ambient Lisp globals." WASI-NETWORK is that
;;; object: the declared OUTBOUND endpoints, the allowed LISTEN
;;; addresses and the DNS policy. Every socket is created through one
;;; network; bind/connect/send/resolve consult it and an undeclared
;;; destination is refused with the WIT's `access-denied' error case.
;;;
;;; The pinned WIT (specs/wasi/wasi-wit-0.3.2/sockets/sockets.wit) has
;;; no `network' resource/parameter: 0.3.2's resolve-addresses takes
;;; only `name'. The authority therefore travels through provider
;;; CONSTRUCTION (sockets-types-instance/sockets-provider take the
;;; network), which is the only capability-parameter channel the
;;; vendored interface offers.
;;;
;;; The OS surface is SB-ALIEN only (PLAN.md §9.2): the sockaddr
;;; structs are spelled out for the Linux LP64 layout (x86_64 and
;;; arm64 identical) and the offsets are verified empirically by the
;;; tests, the same discipline slice 3b used for `struct dirent'.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (find-package '#:wasmdown.wasi)))
    (dolist (name '("WASI-NETWORK" "WASI-NETWORK-P" "MAKE-WASI-NETWORK"
                    "WASI-NETWORK-OUTBOUND" "WASI-NETWORK-LISTEN"
                    "WASI-NETWORK-DNS"
                    "WASI-ENDPOINT" "WASI-ENDPOINT-P" "MAKE-WASI-ENDPOINT"
                    "WASI-ENDPOINT-ADDRESS" "WASI-ENDPOINT-PORT"
                    "WASI-NETWORK-ALLOWS-OUTBOUND-P"
                    "WASI-NETWORK-ALLOWS-LISTEN-P"
                    "WASI-NETWORK-ALLOWS-DNS-P"
                    "WASI-TCP-SOCKET" "WASI-TCP-SOCKET-P"
                    "WASI-TCP-SOCKET-FD" "WASI-TCP-SOCKET-STATE"
                    "WASI-TCP-SOCKET-FAMILY" "WASI-TCP-SOCKET-NETWORK"
                    "WASI-UDP-SOCKET" "WASI-UDP-SOCKET-P"
                    "WASI-UDP-SOCKET-FD" "WASI-SOCKET-ERROR"
                    "WASI-SOCKET-ERROR-CODE" "SOCKETS-ERROR-CODE"
                    "TCP-SOCKET-RESOURCE-CT" "UDP-SOCKET-RESOURCE-CT"
                    "CLOSE-WASI-TCP-SOCKET" "CLOSE-WASI-UDP-SOCKET"
                    "TCP-CREATE" "TCP-BIND" "TCP-CONNECT" "TCP-LISTEN"
                    "TCP-SEND" "TCP-RECEIVE" "TCP-GET-LOCAL-ADDRESS"
                    "TCP-GET-REMOTE-ADDRESS" "TCP-GET-IS-LISTENING"
                    "TCP-GET-ADDRESS-FAMILY" "TCP-SET-LISTEN-BACKLOG-SIZE"
                    "UDP-CREATE" "UDP-BIND" "UDP-CONNECT" "UDP-DISCONNECT"
                    "UDP-SEND" "UDP-RECEIVE" "UDP-GET-LOCAL-ADDRESS"
                    "UDP-GET-REMOTE-ADDRESS" "UDP-GET-ADDRESS-FAMILY"
                    "IP-NAME-LOOKUP-RESOLVE"
                    "SOCKETS-TYPES-INSTANCE" "IP-NAME-LOOKUP-INSTANCE"
                    "SOCKETS-PROVIDER"
                    "IP-SOCKET-ADDRESS->SOCKADDR"
                    "SOCKADDR->IP-SOCKET-ADDRESS"))
      (export (intern name package) package))))

;;; ---------------------------------------------------------------
;;; Linux socket constants (asm-generic: x86_64 and arm64 agree).

(defparameter +af-unspec+ 0)
(defparameter +af-inet+ 2)
(defparameter +af-inet6+ 10)
(defparameter +sock-stream+ 1)
(defparameter +sock-dgram+ 2)
(defparameter +sock-nonblock+ #o4000)     ; SOCK_NONBLOCK
(defparameter +sock-cloexec+ #o2000000)   ; SOCK_CLOEXEC
(defparameter +ipproto-tcp+ 6)
(defparameter +ipproto-udp+ 17)
(defparameter +ipproto-ipv6+ 41)
(defparameter +ipproto-ip+ 0)           ; IPPROTO_IP: IP_TTL's level
(defparameter +sol-socket+ 1)
(defparameter +sol-tcp+ 6)
(defparameter +so-reuseaddr+ 2)
(defparameter +so-error+ 4)
(defparameter +so-sndbuf+ 7)
(defparameter +so-rcvbuf+ 8)
(defparameter +so-keepalive+ 9)
(defparameter +so-acceptconn+ 30)
(defparameter +so-domain+ 39)
(defparameter +tcp-keepidle+ 4)
(defparameter +tcp-keepintvl+ 5)
(defparameter +tcp-keepcnt+ 6)
(defparameter +ip-ttl+ 2)
(defparameter +ipv6-unicast-hops+ 16)
(defparameter +ipv6-v6only+ 26)
(defparameter +shut-rd+ 0)
(defparameter +shut-wr+ 1)
(defparameter +msg-nosignal+ #x4000)
(defparameter +einprogress+ 115)

;;; The sockaddr layouts (Linux LP64). Offsets are the C ABI's:
;;;   sockaddr_in  : family u16 @0, port u16 @2, addr[4] @4, zero @8 (size 16)
;;;   sockaddr_in6 : family u16 @0, port u16 @2, flowinfo u32 @4,
;;;                  addr[16] @8, scope-id u32 @24 (size 28)
;;; The tests verify alien-size and the written bytes at these offsets.

(sb-alien:define-alien-type nil
  (sb-alien:struct %sockaddr-in
    (sin-family sb-alien:unsigned-short)
    (sin-port sb-alien:unsigned-short)
    (sin-addr (sb-alien:array sb-alien:unsigned-char 4))
    (sin-zero (sb-alien:array sb-alien:unsigned-char 8))))

(sb-alien:define-alien-type nil
  (sb-alien:struct %sockaddr-in6
    (sin6-family sb-alien:unsigned-short)
    (sin6-port sb-alien:unsigned-short)
    (sin6-flowinfo sb-alien:unsigned-int)
    (sin6-addr (sb-alien:array sb-alien:unsigned-char 16))
    (sin6-scope-id sb-alien:unsigned-int)))

(defparameter +sockaddr-in-size+ 16)
(defparameter +sockaddr-in6-size+ 28)
(defparameter +sockaddr-storage-size+ 128)

;;; The libc socket surface.

(sb-alien:define-alien-routine ("socket" %libc-socket) sb-alien:int
  (domain sb-alien:int) (type sb-alien:int) (protocol sb-alien:int))
(sb-alien:define-alien-routine ("bind" %libc-bind) sb-alien:int
  (fd sb-alien:int) (addr sb-sys:system-area-pointer)
  (len sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("listen" %libc-listen) sb-alien:int
  (fd sb-alien:int) (backlog sb-alien:int))
(sb-alien:define-alien-routine ("accept4" %libc-accept4) sb-alien:int
  (fd sb-alien:int) (addr sb-sys:system-area-pointer)
  (addrlen sb-sys:system-area-pointer) (flags sb-alien:int))
(sb-alien:define-alien-routine ("connect" %libc-connect) sb-alien:int
  (fd sb-alien:int) (addr sb-sys:system-area-pointer)
  (len sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("send" %libc-send) sb-alien:long
  (fd sb-alien:int) (buf sb-sys:system-area-pointer)
  (len sb-alien:size-t) (flags sb-alien:int))
(sb-alien:define-alien-routine ("sendto" %libc-sendto) sb-alien:long
  (fd sb-alien:int) (buf sb-sys:system-area-pointer)
  (len sb-alien:size-t) (flags sb-alien:int)
  (addr sb-sys:system-area-pointer) (addrlen sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("recv" %libc-recv) sb-alien:long
  (fd sb-alien:int) (buf sb-sys:system-area-pointer)
  (len sb-alien:size-t) (flags sb-alien:int))
(sb-alien:define-alien-routine ("recvfrom" %libc-recvfrom) sb-alien:long
  (fd sb-alien:int) (buf sb-sys:system-area-pointer)
  (len sb-alien:size-t) (flags sb-alien:int)
  (addr sb-sys:system-area-pointer) (addrlen sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("shutdown" %libc-shutdown) sb-alien:int
  (fd sb-alien:int) (how sb-alien:int))
(sb-alien:define-alien-routine ("getsockname" %libc-getsockname) sb-alien:int
  (fd sb-alien:int) (addr sb-sys:system-area-pointer)
  (addrlen sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("getpeername" %libc-getpeername) sb-alien:int
  (fd sb-alien:int) (addr sb-sys:system-area-pointer)
  (addrlen sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("setsockopt" %libc-setsockopt) sb-alien:int
  (fd sb-alien:int) (level sb-alien:int) (opt sb-alien:int)
  (val sb-sys:system-area-pointer) (len sb-alien:unsigned-int))
(sb-alien:define-alien-routine ("getsockopt" %libc-getsockopt) sb-alien:int
  (fd sb-alien:int) (level sb-alien:int) (opt sb-alien:int)
  (val sb-sys:system-area-pointer) (len sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("poll" %libc-poll) sb-alien:int
  (fds sb-sys:system-area-pointer) (nfds sb-alien:unsigned-long)
  (timeout sb-alien:int))
(sb-alien:define-alien-routine ("getaddrinfo" %libc-getaddrinfo) sb-alien:int
  (node sb-alien:c-string) (service sb-alien:c-string)
  (hints sb-sys:system-area-pointer) (res sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("freeaddrinfo" %libc-freeaddrinfo)
    sb-alien:void
  (res sb-sys:system-area-pointer))

;;; struct addrinfo (LP64): ai_flags@0, ai_family@4, ai_socktype@8,
;;; ai_protocol@12, ai_addrlen@16 (u32), pad@20, ai_addr@24,
;;; ai_canonname@32, ai_next@40; size 48. Verified by the tests
;;; through a real getaddrinfo chain.
(defparameter +addrinfo-size+ 48)
(defparameter +ai-flags-offset+ 0)
(defparameter +ai-family-offset+ 4)
(defparameter +ai-addrlen-offset+ 16)
(defparameter +ai-addr-offset+ 24)
(defparameter +ai-next-offset+ 40)
(defparameter +ai-numerichost+ 4)

;;; ---------------------------------------------------------------
;;; The error-code condition: code is the WIT VARIANT DOMAIN value
;;; (cons "label" payload), the shape the canonical store accepts for
;;; a real `error-code' (and that the CL tests read via
;;; SOCKETS-ERROR-CODE).

(define-condition wasi-socket-error (wasmdown:wasmdown-error)
  ((code :initarg :code :reader wasi-socket-error-code))
  (:report (lambda (c s)
             (format s "wasi:sockets error: ~a"
                     (wasi-socket-error-code c))))
  (:documentation "The internal short-circuit of a socket operation;
the provider wrapper turns it into the WIT's `(:err . code)'."))

(defun sockets-error-code (value)
  "The error-code CASE NAME denoted by VALUE (a cons variant domain
value or a bare label string)."
  (if (consp value) (car value) value))

(defun %socket-error (code)
  (error 'wasi-socket-error :code code))

(defmacro %socket-result (&body body)
  "Run BODY; its value is the ok payload: (:ok . value). A
wasi-socket-error becomes (:err . code)."
  `(handler-case (cons :ok (progn ,@body))
     (wasi-socket-error (c) (cons :err (wasi-socket-error-code c)))))

;;; The WIT variant cases: a domain value is (label . payload).
(defun %socket-code (label) (cons label nil))

;;; errno -> the sockets error-code variant (the WIT's own POSIX
;;; equivalences, docs on each method).
(defparameter *socket-errno->code*
  '((1 . "access-denied")               ; EPERM
    (13 . "access-denied")              ; EACCES
    (95 . "not-supported")              ; EOPNOTSUPP
    (92 . "not-supported")              ; ENOPROTOOPT
    (96 . "not-supported")              ; EPFNOSUPPORT
    (93 . "not-supported")              ; EPROTONOSUPPORT
    (94 . "not-supported")              ; ESOCKTNOSUPPORT
    (22 . "invalid-argument")           ; EINVAL
    (89 . "invalid-argument")           ; EDESTADDRREQ
    (97 . "invalid-argument")           ; EAFNOSUPPORT
    (12 . "out-of-memory")              ; ENOMEM
    (105 . "out-of-memory")             ; ENOBUFS
    (110 . "timeout")                   ; ETIMEDOUT
    (99 . "address-not-bindable")       ; EADDRNOTAVAIL
    (98 . "address-in-use")             ; EADDRINUSE
    (113 . "remote-unreachable")        ; EHOSTUNREACH
    (112 . "remote-unreachable")        ; EHOSTDOWN
    (100 . "remote-unreachable")        ; ENETDOWN
    (101 . "remote-unreachable")        ; ENETUNREACH
    (64 . "remote-unreachable")         ; ENONET
    (111 . "connection-refused")        ; ECONNREFUSED
    (32 . "connection-broken")          ; EPIPE
    (104 . "connection-reset")          ; ECONNRESET
    (103 . "connection-aborted")        ; ECONNABORTED
    (90 . "datagram-too-large")         ; EMSGSIZE
    (106 . "invalid-state")             ; EISCONN
    (107 . "invalid-state")             ; ENOTCONN
    (114 . "invalid-state")             ; EALREADY
    (9 . "invalid-state"))              ; EBADF
  "The Linux errno -> sockets error-code case labels (anything else
is the variant's `other' case).")

(defun %socket-errno-code (errno)
  (cons (or (cdr (assoc errno *socket-errno->code*)) "other") nil))

(defun %socket-check (ret what)
  "Turn a libc -1 return into the mapped wasi-socket-error."
  (declare (ignore what))
  (when (minusp ret)
    (%socket-error (%socket-errno-code (sb-alien:get-errno))))
  ret)

(defun %socket-eagain-p (errno)
  (or (= errno +eagain+) (= errno 11)))

;;; ---------------------------------------------------------------
;;; The authority objects.

(defstruct wasi-endpoint
  "One declared endpoint: ADDRESS is :any, an IPv4 4-tuple or an
IPv6 8-tuple; PORT is :any or an integer."
  (address :any)
  (port :any))

(defstruct wasi-network
  "A wasi:sockets network authority (PLAN.md §9.1): the declared
OUTBOUND destinations, the allowed LISTEN addresses and the DNS
policy. NIL in OUTBOUND/LISTEN means `nothing is declared' (deny);
:any means `every address/port'. DNS is :none (deny), :all, or a
list of names (case-insensitive, `*.suffix' wildcards allowed)."
  (outbound nil)
  (listen nil)
  (dns :none))

(defun %endpoint-slot (network slot)
  (ecase slot
    (:outbound (wasi-network-outbound network))
    (:listen (wasi-network-listen network))))

(defun %endpoint-address-matches-p (spec family host)
  (cond
    ((eq spec :any) t)
    ((and (listp spec) (eql (length spec) (if (eq family :ipv6) 8 4)))
     (equal spec host))
    (t nil)))

(defun %network-allows-p (network slot family host port)
  "Does NETWORK permit FAMILY/HOST/PORT in SLOT (:outbound/:listen)?
No network authority permits nothing."
  (if (null network)
      nil
      (let ((eps (%endpoint-slot network slot)))
        (cond
          ((eq eps :any) t)
          ((null eps) nil)
          (t (some (lambda (ep)
                     (and (%endpoint-address-matches-p
                           (wasi-endpoint-address ep) family host)
                          (or (eq (wasi-endpoint-port ep) :any)
                              (eql (wasi-endpoint-port ep) port))))
                   eps))))))

(defun wasi-network-allows-outbound-p (network family host port)
  (%network-allows-p network :outbound family host port))

(defun wasi-network-allows-listen-p (network family host port)
  (%network-allows-p network :listen family host port))

(defun %dns-pattern-matches-p (pattern name)
  (let ((pattern (string-downcase pattern))
        (name (string-downcase name)))
    (cond
      ((string= pattern name) t)
      ((and (> (length pattern) 2)
            (string= "**" (subseq pattern 0 2)))
       ;; "**.example.com" or "*.example.com": any suffix match
       (let ((suffix (if (char= (char pattern 1) #\*)
                         (subseq pattern 2)
                         (subseq pattern 1))))
         (and (>= (length name) (length suffix))
              (string= suffix name :start2 (- (length name) (length suffix))))))
      (t nil))))

(defun wasi-network-allows-dns-p (network name)
  (let ((dns (and network (wasi-network-dns network))))
    (cond
      ((eq dns :all) t)
      ((null dns) nil)
      ((eq dns :none) nil)
      ((listp dns) (some (lambda (pat) (%dns-pattern-matches-p pat name))
                         dns))
      (t nil))))

;;; ---------------------------------------------------------------
;;; sockaddr <-> the WIT's ip-socket-address domain values.

(defun %u16->be-bytes (port)
  (values (ldb (byte 8 8) port) (ldb (byte 8 0) port)))

(defun %sap-u16-be (sap off)
  (logior (ash (sb-sys:sap-ref-8 sap off) 8)
          (sb-sys:sap-ref-8 sap (1+ off))))

(defun %family-key (family)
  (case family (:ipv4 +af-inet+) (:ipv6 +af-inet6+)))

(defun %make-sockaddr (case record)
  "The (values ALIEN LENGTH FAMILY) a WIT ip-socket-address domain
value denotes. CASE is \"ipv4\"/\"ipv6\", RECORD the alist."
  (let ((port (or (cdr (assoc "port" record :test #'string=)) 0))
        (addr (cdr (assoc "address" record :test #'string=))))
    (cond
      ((string= case "ipv4")
       (unless (and (listp addr) (= 4 (length addr)))
         (%socket-error (%socket-code "invalid-argument")))
       (let ((alien (sb-alien:make-alien (sb-alien:struct %sockaddr-in)))
             (sap nil))
         (setf sap (sb-alien:alien-sap alien))
         (setf (sb-sys:sap-ref-8 sap 0) +af-inet+
               (sb-sys:sap-ref-8 sap 1) 0)
         (multiple-value-bind (hi lo) (%u16->be-bytes port)
           (setf (sb-sys:sap-ref-8 sap 2) hi
                 (sb-sys:sap-ref-8 sap 3) lo))
         (loop for i from 0 for b in addr
               do (setf (sb-sys:sap-ref-8 sap (+ 4 i)) (logand b #xff)))
         (values alien +sockaddr-in-size+ :ipv4)))
      ((string= case "ipv6")
       (unless (and (listp addr) (= 8 (length addr)))
         (%socket-error (%socket-code "invalid-argument")))
       (let ((alien (sb-alien:make-alien (sb-alien:struct %sockaddr-in6)))
             (sap nil)
             (flow (or (cdr (assoc "flow-info" record :test #'string=)) 0))
             (scope (or (cdr (assoc "scope-id" record :test #'string=)) 0)))
         (setf sap (sb-alien:alien-sap alien))
         (setf (sb-sys:sap-ref-8 sap 0) +af-inet6+
               (sb-sys:sap-ref-8 sap 1) 0)
         (multiple-value-bind (hi lo) (%u16->be-bytes port)
           (setf (sb-sys:sap-ref-8 sap 2) hi
                 (sb-sys:sap-ref-8 sap 3) lo))
         (setf (sb-sys:sap-ref-32 sap 4) flow)
         (loop for g from 0 below 8
               for v in addr
               do (setf (sb-sys:sap-ref-8 sap (+ 8 (* 2 g)))
                        (ldb (byte 8 8) v)
                        (sb-sys:sap-ref-8 sap (+ 9 (* 2 g)))
                        (ldb (byte 8 0) v)))
         (setf (sb-sys:sap-ref-32 sap 24) scope)
         (values alien +sockaddr-in6-size+ :ipv6)))
      (t (%socket-error (%socket-code "invalid-argument"))))))

(defun ip-socket-address->sockaddr (value)
  "The (values ALIEN LENGTH FAMILY) of a WIT ip-socket-address domain
value (`(\"ipv4\" . record)'). The caller frees ALIEN."
  (unless (and (consp value) (stringp (car value)))
    (%socket-error (%socket-code "invalid-argument")))
  (%make-sockaddr (car value) (cdr value)))

(defun %sockaddr-sap->key (sap)
  "The (values FAMILY HOST PORT) a raw sockaddr SAP denotes."
  (let ((fam (logior (sb-sys:sap-ref-8 sap 0)
                     (ash (sb-sys:sap-ref-8 sap 1) 8))))
    (cond
      ((= fam +af-inet+)
       (values :ipv4
               (loop for i below 4 collect (sb-sys:sap-ref-8 sap (+ 4 i)))
               (%sap-u16-be sap 2)))
      ((= fam +af-inet6+)
       (values :ipv6
               (loop for g below 8
                     collect (logior (ash (sb-sys:sap-ref-8 sap (+ 8 (* 2 g))) 8)
                                     (sb-sys:sap-ref-8 sap (+ 9 (* 2 g)))))
               (%sap-u16-be sap 2)))
      (t (values nil nil nil)))))

(defun sockaddr->ip-socket-address (sap)
  "The WIT ip-socket-address DOMAIN value a raw sockaddr SAP denotes."
  (multiple-value-bind (family host port) (%sockaddr-sap->key sap)
    (case family
      (:ipv4
       (cons "ipv4" (list (cons "port" port)
                          (cons "address" host))))
      (:ipv6
       (cons "ipv6" (list (cons "port" port)
                          (cons "flow-info" (sb-sys:sap-ref-32 sap 4))
                          (cons "address" host)
                          (cons "scope-id" (sb-sys:sap-ref-32 sap 24)))))
      (t (%socket-error (%socket-code "invalid-argument"))))))

(defun %sockaddr-key-equal-p (alien len other-alien other-len)
  (and (= len other-len)
       (loop for i below len
             always (= (sb-sys:sap-ref-8 (sb-alien:alien-sap alien) i)
                       (sb-sys:sap-ref-8 (sb-alien:alien-sap other-alien) i)))))

;;; ---------------------------------------------------------------
;;; Small OS helpers.

(defun %sock-null () (sb-sys:int-sap 0))

(defun %getsockopt-int (fd level opt)
  (let ((alien (sb-alien:make-alien sb-alien:int))
        (len (sb-alien:make-alien sb-alien:unsigned-int)))
    (unwind-protect
         (progn
           (setf (sb-alien:deref len) 4)
           (%socket-check
            (%libc-getsockopt fd level opt (sb-alien:alien-sap alien)
                              (sb-alien:alien-sap len))
            "getsockopt")
           (sb-alien:deref alien))
      (sb-alien:free-alien alien)
      (sb-alien:free-alien len))))

(defun %setsockopt-int (fd level opt value)
  (let ((alien (sb-alien:make-alien sb-alien:int)))
    (unwind-protect
         (progn
           (setf (sb-alien:deref alien) value)
           (%socket-check
            (%libc-setsockopt fd level opt (sb-alien:alien-sap alien) 4)
            "setsockopt"))
      (sb-alien:free-alien alien))))

(defun %poll-fd (fd events timeout-ms)
  "Wait for EVENTS (POLLIN/POLLOUT) on FD. The pollfd layout is
int fd @0, short events @4, short revents @6 (8 bytes). Returns the
revents, or NIL on timeout/error."
  (let ((alien (sb-alien:make-alien
                (sb-alien:array sb-alien:unsigned-char 8))))
    (unwind-protect
         (let ((sap (sb-alien:alien-sap alien)))
           (setf (sb-sys:sap-ref-32 sap 0) fd
                 (sb-sys:sap-ref-16 sap 4) events
                 (sb-sys:sap-ref-16 sap 6) 0)
           (let ((rc (%libc-poll sap 1 timeout-ms)))
             (when (plusp rc)
               (sb-sys:sap-ref-16 sap 6))))
      (sb-alien:free-alien alien))))

(defun %connect-fd (fd alien len)
  "connect(2) with the nonblocking EINPROGRESS completion wait."
  (sb-alien:set-errno 0)
  (let ((ret (%libc-connect fd (sb-alien:alien-sap alien) len)))
    (cond
      ((zerop ret) t)
      ((= (sb-alien:get-errno) +einprogress+)
       (unless (%poll-fd fd +pollout+ 5000)
         (%socket-error (%socket-code "timeout")))
       (let ((err (%getsockopt-int fd +sol-socket+ +so-error+)))
         (unless (zerop err)
           (%socket-error (%socket-errno-code err)))))
      (t (%socket-check ret "connect")))))

;;; ---------------------------------------------------------------
;;; The reactor backend (plan step 6): one poll(2) + the monotonic
;;; clock + a self-pipe wake channel. This is a STANDALONE host
;;; facility: the store driver owns when STEP is called; the reactor
;;; only reports what the OS says and hands each readiness to the
;;; registration's callback.
;;;
;;; Contract
;;;   * (REACTOR-STEP reactor max-wait-ns) pumps at most once and
;;;     never blocks longer than MAX-WAIT-NS. It returns
;;;     (values PROGRESS-P PENDING-P): PROGRESS-P = a callback ran or
;;;     an external producer's wake byte was consumed; PENDING-P =
;;;     registrations remain (so a caller with neither progress nor
;;;     pending, no ready candidate and no external producer is in
;;;     the spec's deadlock, not merely idle).
;;;   * (REACTOR-WATCH-FD reactor fd direction registration-id cb)
;;;     keys a watch by (fd, registration-id, direction): a numeric
;;;     fd number reused by a DIFFERENT connection cannot inherit the
;;;     old watch, and two watches on one fd with the same direction
;;;     but different registration-ids are separate poll entries, so
;;;     they never coalesce. FD watches are level-triggered (poll
;;;     semantics): the callback runs on every step that observes the
;;;     fd ready until the cancel-fn runs. CB is called with one
;;;     argument, the STATUS: :read / :write / :both for the requested
;;;     direction, or :error / :hup / :nval for POLLERR / POLLHUP /
;;;     POLLNVAL (in that precedence).
;;;   * (REACTOR-WATCH-DEADLINE reactor deadline-ns cb) is one-shot;
;;;     a past or zero DEADLINE fires on the next step, a fired
;;;     deadline is removed before CB runs. CB takes no arguments.
;;;   * (REACTOR-CLOSE reactor) is idempotent; it cancels every
;;;     registration (no callback runs after it returns, unless the
;;;    caller is itself inside a callback of this reactor) and frees
;;;     the self-pipe.
;;;   * Callbacks run with no reactor lock held; they are expected to
;;;     only enqueue an event. A callback must never be called after
;;;     its cancel-fn ran: CANCEL-FN is idempotent and waits out a
;;;     callback that is already in flight (except when called from
;;;     inside that very callback).
;;;   * CLOCK-FN (monotonic ns) and POLL-FN are injectable so tests
;;;     script time and readiness without sleeping. POLL-FN receives
;;;     (REQUESTS TIMEOUT-MS) where each request is a plist
;;;     (:kind :fd|:wake :fd n :events bits) and returns
;;;     (values REVENTS STATUS); REVENTS is one revents word per
;;;     request (a bare list is STATUS :ok), STATUS is :ok, :eintr
;;;     (re-poll without dispatching) or :error.
;;;   * REACTOR-WAKE writes one byte to the self-pipe so an external
;;;     producer blocked elsewhere can interrupt a blocked STEP.
;;;   * Without a self-pipe (WAKE-P NIL, the testing configuration) a
;;;     step never sleeps: with no fd registration there is nothing
;;;     pollable, so it returns immediately instead of blocking.

(defparameter +pollerr+ 8)
(defparameter +pollhup+ 16)
(defparameter +pollnval+ 32
  "POLLNVAL — the fd was not open when poll ran.")
(defparameter +o-nonblock+ #x800)

;;; F_GETFL: does a borrowed stdio fd carry O_NONBLOCK? A BLOCKING fd
;;; is the bounded stdio executor's business (M17 step 6) — an fd
;;; already in nonblocking mode keeps the direct would-block path.
(defconstant +f-getfl+ 3)

(sb-alien:define-alien-routine ("fcntl" %libc-fcntl) sb-alien:int
  (fd sb-alien:int) (cmd sb-alien:int) (arg sb-alien:int))

(defun %fd-nonblocking-p (fd)
  "T when FD carries O_NONBLOCK (or the probe itself failed, so the
caller keeps the safe direct path)."
  (and (integerp fd)
       (>= fd 0)
       (let ((flags (%libc-fcntl fd +f-getfl+ 0)))
         (or (minusp flags) (logtest +o-nonblock+ flags)))))

(sb-alien:define-alien-routine ("pipe2" %libc-pipe2) sb-alien:int
  (fds sb-sys:system-area-pointer) (flags sb-alien:int))

(defstruct (reactor-registration (:constructor %make-reactor-registration))
  "One watch. STATE is :live or :cancelled; BUSY-P is true while its
callback is executing (CANCEL-FN waits on it, see the section
comment). KEY is the (fd registration-id direction) table key of an
:fd watch."
  (kind nil :read-only t)              ; :fd | :deadline
  (key nil)
  (fd nil :read-only t)
  (direction nil :read-only t)
  (registration-id nil :read-only t)
  (deadline nil :read-only t)
  (callback nil :read-only t)
  (seq 0)
  (state :live)
  (busy-p nil))

(defstruct (wasi-reactor (:constructor %make-wasi-reactor))
  "The poll(2) reactor: its registrations (ordered), the injectable
clock/poll, and the self-pipe. MUTEX guards every field below the
constructor's input; callbacks never run under it."
  (mutex (sb-thread:make-mutex :name "wasi-reactor"))
  (condition (sb-thread:make-waitqueue :name "wasi-reactor"))
  (clock-fn nil)
  (poll-fn nil)
  (fd-table (make-hash-table :test #'equal))
  (fd-order '())                       ; registrations, insertion order
  (deadlines '())                      ; registrations, (deadline, seq) order
  (seq 0)
  (wake-read-fd nil)
  (wake-write-fd nil)
  (closed-p nil)
  (fds-closed-p nil)
  (pollers 0)
  (in-flight 0))

(defvar *reactor-callback-registration* nil
  "The registration whose callback is running on this thread (or
NIL): lets CANCEL-FN/REACTOR-CLOSE avoid waiting on themselves.")

(defun %reactor-clock (reactor)
  (funcall (wasi-reactor-clock-fn reactor)))

(defun %reactor-events-for-direction (direction)
  (ecase direction
    (:read +pollin+)
    (:write +pollout+)
    (:both (logior +pollin+ +pollout+))))

(defun %reactor-status (direction revents)
  "The STATUS a watch on DIRECTION reports for REVENTS, or NIL when
the fd is not ready for it. The error statuses take precedence: a
connection that is hung up or errored is reported as such, not as a
plain readiness."
  (cond
    ((logtest +pollnval+ revents) :nval)
    ((logtest +pollerr+ revents) :error)
    ((logtest +pollhup+ revents) :hup)
    (t
     (let ((wanted (logand revents (%reactor-events-for-direction direction))))
       (cond
         ((= wanted (%reactor-events-for-direction direction))
          (if (eq direction :both) :both direction))
         ((logtest +pollin+ wanted) :read)
         ((logtest +pollout+ wanted) :write)
         (t nil))))))

;;; --- the real backend -------------------------------------------------

(defun %reactor-poll-libc (requests timeout-ms)
  "The default POLL-FN: one poll(2) over REQUESTS, one pollfd per
registration (never coalesced). The pollfd layout is the C ABI's:
int fd @0, short events @4, short revents @6; the stride is 8 bytes."
  (let* ((n (length requests))
         (alien (sb-alien:make-alien sb-alien:unsigned-char
                                     (max 8 (* 8 n)))))
    (unwind-protect
         (let ((sap (sb-alien:alien-sap alien)))
           (loop for request in requests
                 for i from 0
                 for base = (* 8 i)
                 do (setf (sb-sys:sap-ref-32 sap base) (getf request :fd)
                          (sb-sys:sap-ref-16 sap (+ base 4))
                          (getf request :events)
                          (sb-sys:sap-ref-16 sap (+ base 6)) 0))
           (sb-alien:set-errno 0)
           (let ((rc (%libc-poll sap n (min timeout-ms #x7fffffff))))
             (cond
               ((plusp rc)
                (values (loop for i below n
                              collect (sb-sys:sap-ref-16 sap (+ (* 8 i) 6)))
                        :ok))
               ((zerop rc)
                (values (make-list n :initial-element 0) :ok))
               ((= (sb-alien:get-errno) +eintr+)
                (values nil :eintr))
               (t (values nil :error)))))
      (sb-alien:free-alien alien))))

(defun %reactor-open-pipe ()
  "A nonblocking close-on-exec self-pipe as (values READ-FD WRITE-FD),
or NIL when the OS refuses one."
  (let ((fds (sb-alien:make-alien (sb-alien:array sb-alien:unsigned-char 8))))
    (unwind-protect
         (let ((sap (sb-alien:alien-sap fds)))
           (when (zerop (%libc-pipe2 sap (logior +o-nonblock+ +o-cloexec+)))
             (values (sb-sys:sap-ref-32 sap 0)
                     (sb-sys:sap-ref-32 sap 4))))
      (sb-alien:free-alien fds))))

;;; --- the registry ------------------------------------------------------

(defun %reactor-unlink (reactor registration)
  "Remove REGISTRATION from its ordered list/table. The reactor mutex
must be held."
  (ecase (reactor-registration-kind registration)
    (:fd
     (remhash (reactor-registration-key registration)
              (wasi-reactor-fd-table reactor))
     (setf (wasi-reactor-fd-order reactor)
           (remove registration (wasi-reactor-fd-order reactor)
                   :test #'eq)))
    (:deadline
     (setf (wasi-reactor-deadlines reactor)
           (remove registration (wasi-reactor-deadlines reactor)
                   :test #'eq)))))

(defun %reactor-finalize (reactor)
  "Close the self-pipe's read end once it is safe: no callback is in
flight and no poller still holds the fd. Every caller holds the
reactor mutex."
  (when (and (wasi-reactor-closed-p reactor)
             (not (wasi-reactor-fds-closed-p reactor))
             (zerop (wasi-reactor-pollers reactor)))
    (setf (wasi-reactor-fds-closed-p reactor) t)
    (let ((fd (wasi-reactor-wake-read-fd reactor)))
      (setf (wasi-reactor-wake-read-fd reactor) nil)
      (when (and fd (>= fd 0)) (%close-fd fd)))))

(defun %reactor-claim (reactor registration &key one-shot)
  "Mark REGISTRATION busy for one callback. Returns true when this
caller won the claim (the registration was :live); false when it was
cancelled meanwhile. A ONE-SHOT claim unlinks and cancels the
registration before the callback runs, so a cancel-fn invoked from
inside the callback is a harmless no-op."
  (let ((mutex (wasi-reactor-mutex reactor))
        (claimed nil))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (when (eq :live (reactor-registration-state registration))
           (setf (reactor-registration-busy-p registration) t)
           (when one-shot
             (setf (reactor-registration-state registration) :cancelled)
             (%reactor-unlink reactor registration))
           (incf (wasi-reactor-in-flight reactor))
           (setf claimed t))
      (sb-thread:release-mutex mutex))
    claimed))

(defun %reactor-finish (reactor registration)
  "End a callback: clear BUSY-P, wake a cancel-fn waiting on it, and
finalize a pending close."
  (let ((mutex (wasi-reactor-mutex reactor)))
    (sb-thread:grab-mutex mutex)
    (setf (reactor-registration-busy-p registration) nil)
    (decf (wasi-reactor-in-flight reactor))
    (sb-thread:condition-broadcast (wasi-reactor-condition reactor))
    (%reactor-finalize reactor)
    (sb-thread:release-mutex mutex)))

(defun %reactor-cancel (reactor registration)
  "Cancel REGISTRATION. Idempotent. Waits out a callback already in
flight unless this is that callback (a callback cancelling itself).
The wait also covers a registration another caller has already marked
cancelled but whose callback is still running (the replace path)."
  (let ((mutex (wasi-reactor-mutex reactor))
        (self *reactor-callback-registration*))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (progn
           (setf (reactor-registration-state registration) :cancelled)
           (%reactor-unlink reactor registration)
           (unless (eq self registration)
             (loop while (reactor-registration-busy-p registration)
                   do (sb-thread:condition-wait
                       (wasi-reactor-condition reactor) mutex)))
           t)
      (sb-thread:release-mutex mutex))))

(defun %reactor-write-wake-byte (reactor)
  "Write one byte to the self-pipe (nonblocking: a full pipe is
already a pending wake, so EAGAIN is success)."
  (let ((fd (wasi-reactor-wake-write-fd reactor))
        (byte (make-array 1 :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (when (and fd (>= fd 0))
      ;; UNIX-WRITE is (fd buf offset len) with BUF a simple unboxed
      ;; array (no pinning needed)
      (sb-unix:unix-write fd byte 0 1)
      t)))

(defun reactor-wake (reactor)
  "Wake a step blocked in poll(2) (an external producer's channel).
Serialized against REACTOR-CLOSE so a wake never writes a closed fd."
  (let ((mutex (wasi-reactor-mutex reactor)))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (not (null (%reactor-write-wake-byte reactor)))
      (sb-thread:release-mutex mutex))))

(defun %reactor-drain-wake (reactor wake-fd)
  "Consume every byte the self-pipe holds on WAKE-FD (the fd
%REACTOR-SNAPSHOT captured under the mutex). Returns true when at
least one byte was read: an external producer signalled."
  (declare (ignore reactor))
  (let ((buffer (make-array 256 :element-type '(unsigned-byte 8)))
        (drained nil))
    (when (and wake-fd (>= wake-fd 0))
      (loop
        (multiple-value-bind (count errno)
            (sb-sys:with-pinned-objects (buffer)
              (sb-unix:unix-read wake-fd (sb-sys:vector-sap buffer) 256))
          (declare (ignore errno))
          (unless (and count (plusp count))
            (return))
          (setf drained t))))
    drained))

;;; --- the public surface ------------------------------------------------

(defun make-wasi-reactor (&key clock-fn poll-fn (wake-p t))
  "A reactor whose time comes from CLOCK-FN (default MONOTONIC-NOW)
and whose readiness comes from POLL-FN (default the libc poll
backend). WAKE-P NIL omits the self-pipe: useful for tests whose
injected POLL-FN scripts readiness, and it makes STEP nonblocking
(when nothing is registered there is nothing to wait for)."
  (let ((reactor (%make-wasi-reactor
                  :clock-fn (or clock-fn #'monotonic-now)
                  :poll-fn (or poll-fn #'%reactor-poll-libc))))
    (when wake-p
      (multiple-value-bind (read-fd write-fd) (%reactor-open-pipe)
        (unless (and read-fd write-fd (not (minusp read-fd))
                     (not (minusp write-fd)))
          (error "unable to open the reactor's self-pipe"))
        (setf (wasi-reactor-wake-read-fd reactor) read-fd
              (wasi-reactor-wake-write-fd reactor) write-fd)))
    reactor))

(defun reactor-watch-fd (reactor fd direction registration-id callback)
  "Watch FD for DIRECTION (:read / :write / :both). REGISTRATION-ID is
the caller's opaque identity for the resource behind FD; the watch is
keyed by (fd, registration-id, direction), so a reused numeric fd
cannot inherit this watch. Returns an idempotent cancel-fn. A repeat
registration with the same key replaces the previous watch. Signals an
error when the reactor is closed."
  (unless (member direction '(:read :write :both))
    (error "reactor direction ~s is not :read, :write or :both" direction))
  (let* ((mutex (wasi-reactor-mutex reactor))
         (key (list fd registration-id direction))
         (registration
           (%make-reactor-registration
            :kind :fd :key key :fd fd :direction direction
            :registration-id registration-id :callback callback))
         (previous nil))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (progn
           (when (wasi-reactor-closed-p reactor)
             (error "reactor is closed"))
           (setf previous (gethash key (wasi-reactor-fd-table reactor)))
           (when previous
             (setf (reactor-registration-state previous) :cancelled)
             (%reactor-unlink reactor previous))
           (setf (reactor-registration-seq registration)
                 (incf (wasi-reactor-seq reactor)))
           (setf (gethash key (wasi-reactor-fd-table reactor)) registration)
           (setf (wasi-reactor-fd-order reactor)
                 (append (wasi-reactor-fd-order reactor)
                         (list registration))))
      (sb-thread:release-mutex mutex))
    (when previous
      ;; a callback of the replaced watch may still be in flight; make
      ;; sure it has returned before its replacement can run
      (%reactor-cancel reactor previous))
    (lambda () (%reactor-cancel reactor registration))))

(defun %reactor-deadline-before-p (a b)
  (let ((da (reactor-registration-deadline a))
        (db (reactor-registration-deadline b)))
    (or (< da db) (and (= da db)
                       (< (reactor-registration-seq a)
                          (reactor-registration-seq b))))))

(defun reactor-watch-deadline (reactor deadline-ns callback)
  "Call CALLBACK (no arguments) on the first step whose clock reads
DEADLINE-NS or later; the registration is one-shot. A past or zero
DEADLINE fires on the next step. Returns an idempotent cancel-fn."
  (unless (integerp deadline-ns)
    (error "reactor deadline ~s is not an integer" deadline-ns))
  (let* ((mutex (wasi-reactor-mutex reactor))
         (registration
           (%make-reactor-registration
            :kind :deadline :deadline deadline-ns :callback callback)))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (progn
           (when (wasi-reactor-closed-p reactor)
             (error "reactor is closed"))
           (setf (reactor-registration-seq registration)
                 (incf (wasi-reactor-seq reactor)))
           (setf (wasi-reactor-deadlines reactor)
                 (stable-sort (cons registration
                                    (wasi-reactor-deadlines reactor))
                              #'%reactor-deadline-before-p)))
      (sb-thread:release-mutex mutex))
    (lambda () (%reactor-cancel reactor registration))))

;;; --- the pump ----------------------------------------------------------

(defun %reactor-snapshot (reactor)
  "The poll requests and the registrations they belong to. Returns
(values REQUESTS FD-REGISTRATIONS DEADLINES WAKE-FD). The fd requests
are in registration order, one per watch; the wake request (when the
self-pipe exists) comes last."
  (let ((mutex (wasi-reactor-mutex reactor))
        (fds '()) (deadlines '()) (wake-fd nil))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (setf fds (copy-list (wasi-reactor-fd-order reactor))
               deadlines (copy-list (wasi-reactor-deadlines reactor))
               wake-fd (wasi-reactor-wake-read-fd reactor))
      (sb-thread:release-mutex mutex))
    (let ((requests
            (loop for registration in fds
                  collect (list :kind :fd
                                :fd (reactor-registration-fd registration)
                                :events (%reactor-events-for-direction
                                         (reactor-registration-direction
                                          registration))))))
      (when (and wake-fd (>= wake-fd 0))
        (setf requests (append requests
                               (list (list :kind :wake :fd wake-fd
                                           :events +pollin+)))))
      (values requests fds deadlines wake-fd))))

(defun %reactor-call-poll (reactor requests timeout-ms)
  "Run the injected POLL-FN with the reactor's poller count raised, so
REACTOR-CLOSE cannot free an fd poll(2) is still using."
  (let ((mutex (wasi-reactor-mutex reactor)))
    (sb-thread:grab-mutex mutex)
    (incf (wasi-reactor-pollers reactor))
    (sb-thread:release-mutex mutex)
    (unwind-protect
         (let ((revents nil) (status nil))
           (multiple-value-setq (revents status)
             (funcall (wasi-reactor-poll-fn reactor) requests timeout-ms))
           (case revents
             (:eintr (setf revents nil status :eintr))
             (:error (setf revents nil status :error)))
           (values revents status))
      (sb-thread:grab-mutex mutex)
      (decf (wasi-reactor-pollers reactor))
      (%reactor-finalize reactor)
      (sb-thread:release-mutex mutex))))

(defun %reactor-pump (reactor limit)
  "One poll phase, bounded by LIMIT (monotonic ns): wait no longer than
LIMIT - now and no longer than the earliest deadline. EINTR re-polls
(the budget shrinks, so a signal storm cannot extend the step past
LIMIT). Returns (values REQUESTS FD-REGISTRATIONS DEADLINES WAKE-FD
REVENTS). A closed reactor, or nothing pollable, yields REVENTS NIL
without calling POLL-FN."
  (loop
    (multiple-value-bind (requests fds deadlines wake-fd)
        (%reactor-snapshot reactor)
      (when (null requests)
        (return-from %reactor-pump
          (values requests fds deadlines wake-fd nil)))
      (let* ((now (%reactor-clock reactor))
             (budget (- limit now)))
        (when deadlines
          (setf budget (min budget
                            (- (reactor-registration-deadline
                                (first deadlines))
                               now))))
        (when (minusp budget) (setf budget 0))
        ;; FLOOR, not ceiling: poll's timeout is whole milliseconds, and
        ;; rounding up would let a step block for up to 1ms past
        ;; MAX-WAIT-NS. A sub-millisecond budget polls nonblockingly and
        ;; leaves the (still future) deadline to the next step.
        (let ((timeout-ms (floor budget 1000000)))
          (multiple-value-bind (revents status)
              (%reactor-call-poll reactor requests timeout-ms)
            (unless (and (eq status :eintr) (plusp timeout-ms))
              (return-from %reactor-pump
                (values requests fds deadlines wake-fd
                        (and (listp revents) revents))))))))))

(defun %reactor-dispatch-fds (reactor registrations revents)
  "Deliver FD readiness in registration order; true when any callback
ran."
  (let ((progress nil))
    (loop for registration in registrations
          for revents-word in revents
          for status = (%reactor-status
                        (reactor-registration-direction registration)
                        (or revents-word 0))
          when status
            do (when (%reactor-claim reactor registration)
                 (setf progress t)
                 (unwind-protect
                      (let ((*reactor-callback-registration* registration))
                        (funcall (reactor-registration-callback registration)
                                 status))
                   (%reactor-finish reactor registration))))
    progress))

(defun %reactor-dispatch-deadlines (reactor registrations now)
  "Fire every due deadline in (deadline, registration) order; true
when any callback ran."
  (let ((progress nil))
    (dolist (registration registrations)
      (when (<= (reactor-registration-deadline registration) now)
        (when (%reactor-claim reactor registration :one-shot t)
          (setf progress t)
          (unwind-protect
               (let ((*reactor-callback-registration* registration))
                 (funcall (reactor-registration-callback registration)))
            (%reactor-finish reactor registration)))))
    progress))

(defun reactor-step (reactor max-wait-ns)
  "Pump the reactor at most once. Waits no longer than MAX-WAIT-NS, or
until the earliest deadline, whichever comes first; a past deadline
or a ready fd makes the wait 0. Callbacks run in this order: fd
readiness in registration order, then due deadlines in deadline order.
Returns (values PROGRESS-P PENDING-P)."
  (let ((mutex (wasi-reactor-mutex reactor)))
    (sb-thread:grab-mutex mutex)
    (let ((closed (wasi-reactor-closed-p reactor)))
      (unless closed (incf (wasi-reactor-pollers reactor)))
      (sb-thread:release-mutex mutex)
      (when closed (return-from reactor-step (values nil nil))))
    (unwind-protect
         (progn
           (let* ((wait (if (and (integerp max-wait-ns) (plusp max-wait-ns))
                            max-wait-ns
                            0))
                  (limit (+ (%reactor-clock reactor) wait)))
             (multiple-value-bind (requests fds deadlines wake-fd revents)
                 (%reactor-pump reactor limit)
               (declare (ignore requests))
               ;; REACTOR-CLOSE during the poll cancels everything; no
               ;; callback runs after close.
               (sb-thread:grab-mutex mutex)
               (let ((closed (wasi-reactor-closed-p reactor)))
                 (sb-thread:release-mutex mutex)
                 (when closed (return-from reactor-step (values nil nil))))
               (let ((progress nil))
                 (when (and wake-fd (>= wake-fd 0) revents
                            (plusp (or (nth (length fds) revents) 0)))
                   (when (%reactor-drain-wake reactor wake-fd)
                     (setf progress t)))
                 (when revents
                   (when (%reactor-dispatch-fds reactor fds revents)
                     (setf progress t)))
                 (when deadlines
                   (when (%reactor-dispatch-deadlines reactor deadlines
                                                      (%reactor-clock reactor))
                     (setf progress t)))
                 (sb-thread:grab-mutex mutex)
                 (let ((pending (and (not (wasi-reactor-closed-p reactor))
                                     (or (plusp (hash-table-count
                                                 (wasi-reactor-fd-table
                                                  reactor)))
                                         (not (null (wasi-reactor-deadlines
                                                     reactor)))))))
                   (sb-thread:release-mutex mutex)
                   (values (and progress t) (and pending t)))))))
      ;; the permit covers the pump AND the drain/dispatch, so a
      ;; concurrent close cannot free the self-pipe's read fd while
      ;; this step is still reading it
      (sb-thread:grab-mutex mutex)
      (decf (wasi-reactor-pollers reactor))
      (%reactor-finalize reactor)
      (sb-thread:release-mutex mutex))))

(defun reactor-close (reactor)
  "Idempotent: cancel every registration, close the self-pipe and make
the reactor refuse new watches. No callback starts after this returns
(unless the caller is itself inside one of this reactor's callbacks)."
  (let ((mutex (wasi-reactor-mutex reactor))
        (self *reactor-callback-registration*))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (progn
           (unless (wasi-reactor-closed-p reactor)
             (setf (wasi-reactor-closed-p reactor) t)
             (dolist (registration (wasi-reactor-fd-order reactor))
               (setf (reactor-registration-state registration) :cancelled))
             (dolist (registration (wasi-reactor-deadlines reactor))
               (setf (reactor-registration-state registration) :cancelled))
             (clrhash (wasi-reactor-fd-table reactor))
             (setf (wasi-reactor-fd-order reactor) nil
                   (wasi-reactor-deadlines reactor) nil)
             ;; closing the write end wakes a poller blocked on the
             ;; read end (it observes POLLHUP), so a close never waits
             ;; for a step in another thread
             (let ((write-fd (wasi-reactor-wake-write-fd reactor)))
               (setf (wasi-reactor-wake-write-fd reactor) nil)
               (when (and write-fd (>= write-fd 0)) (%close-fd write-fd)))
             (unless self
               (loop while (plusp (wasi-reactor-in-flight reactor))
                     do (sb-thread:condition-wait
                         (wasi-reactor-condition reactor) mutex))))
           (%reactor-finalize reactor)
           t)
      (sb-thread:release-mutex mutex))))

;;; ---------------------------------------------------------------
;;; STREAM READINESS (plan step 6): with a reactor attached, an
;;; EAGAIN/EWOULDBLOCK on accept/recv/send REGISTERS a watch keyed by
;;; the CONNECTION/STREAM IDENTITY (never the bare fd: a reused fd
;;; number must not inherit a stale watch) and the parked copy
;;; resumes through the STORE QUEUE via HOST-END-READY. Without a
;;; reactor the provider keeps today's would-block/poll path exactly.
;;;
;;; A live watch holds ONE store-work unit — a HOST-PENDING "gate" —
;;; so the store driver keeps pumping the reactor while the guest is
;;; parked (the gate is released exactly once when the watch fires or
;;; is disarmed; a store with no live host work never polls).
;;;
;;; THE GENERATION is captured when the watch is ARMED (the attempt
;;; the copy record holds); a callback that fires after the copy was
;;; cancelled, dropped or replaced names a different generation and
;;; HOST-END-READY resumes nothing.

(defstruct (readiness-wait (:constructor %make-readiness-wait (end)))
  "One parked stream copy's readiness subscription."
  (end nil)                            ; the host end whose copy wakes
  (mutex (sb-thread:make-mutex :name "readiness-wait"))
  (armed-p nil)
  (generation 0)
  (cancel nil)                         ; the reactor watch's cancel-fn
  (resolve nil))                       ; the gate pending's resolve

(defun %context-reactor (context)
  "CONTEXT's reactor, or NIL (a NIL context has no capabilities)."
  (and context (wasi-context-reactor context)))

(defun %readiness-armed-p (wait)
  (and wait
       (sb-thread:with-recursive-lock ((readiness-wait-mutex wait))
         (readiness-wait-armed-p wait))))

(defun %readiness-arm (wait reactor fd direction identity)
  "Register a ONE-SHOT reactor watch for FD/DIRECTION under
REGISTRATION-IDENTITY (the connection/stream object, not the fd), and
wake WAIT's parked copy through the store queue. Idempotent while
armed. T when newly armed."
  (when (and wait reactor (integerp fd) (>= fd 0))
    (let ((arm nil) (generation nil))
      (sb-thread:with-recursive-lock ((readiness-wait-mutex wait))
        (unless (readiness-wait-armed-p wait)
          (setf (readiness-wait-armed-p wait) t
                generation (wasmdown.component.runtime:host-end-generation
                            (readiness-wait-end wait))
                (readiness-wait-generation wait) generation
                arm t)))
      (when arm
        (let ((resolve nil))
          (handler-case
              (progn
                (wasmdown.component.runtime:make-host-pending
                 :start
                 (lambda (res reject acknowledge)
                   (declare (ignore reject acknowledge))
                   (setf resolve res)
                   (let ((cancel
                           (reactor-watch-fd
                            reactor fd direction identity
                            (lambda (status)
                              (declare (ignore status))
                              ;; the watch is SPENT: unlink it and clear
                              ;; the record, so the resumed copy's next
                              ;; would-block arms a FRESH watch
                              (let ((spent nil))
                                (sb-thread:with-recursive-lock
                                    ((readiness-wait-mutex wait))
                                  (when (readiness-wait-armed-p wait)
                                    (setf spent (readiness-wait-cancel wait)
                                          (readiness-wait-armed-p wait) nil
                                          (readiness-wait-cancel wait) nil
                                          (readiness-wait-resolve wait) nil)))
                                (when spent
                                  (ignore-errors (funcall spent))))
                              ;; the ONLY way back: the store queue.
                              (wasmdown.component.runtime:host-end-ready
                               (readiness-wait-end wait) generation)
                              (funcall res nil)))))
                     (sb-thread:with-recursive-lock
                         ((readiness-wait-mutex wait))
                       (setf (readiness-wait-cancel wait) cancel))
                     ;; the gate pending's own cancel-fn (unused: the
                     ;; disarm settles it with RESOLVE)
                     (lambda () (funcall cancel) t))))
                (sb-thread:with-recursive-lock ((readiness-wait-mutex wait))
                  (setf (readiness-wait-resolve wait) resolve))
                t)
            (error ()
              ;; the watch could not be registered (a closed reactor):
              ;; settle the gate and let the caller retry later
              (sb-thread:with-recursive-lock ((readiness-wait-mutex wait))
                (setf (readiness-wait-armed-p wait) nil
                      (readiness-wait-resolve wait) nil))
              (when resolve (funcall resolve nil))
              nil)))))))

(defun %readiness-disarm (wait)
  "Release WAIT exactly once: cancel the watch and settle its gate
pending (the store's work unit is freed when the delivery runs). A
later callback for the SAME registration finds it cancelled, and its
generation is stale anyway."
  (when wait
    (let ((cancel nil) (resolve nil))
      (sb-thread:with-recursive-lock ((readiness-wait-mutex wait))
        (when (readiness-wait-armed-p wait)
          (setf (readiness-wait-armed-p wait) nil
                cancel (readiness-wait-cancel wait)
                resolve (readiness-wait-resolve wait)
                (readiness-wait-cancel wait) nil
                (readiness-wait-resolve wait) nil)))
      (when cancel (ignore-errors (funcall cancel)))
      (when resolve (funcall resolve nil)))
    t))

(defvar *socket-wait-lock* (sb-thread:make-mutex :name "socket-waits")
  "Guards the per-socket list of live readiness waits (close must be
able to release them all; it is a lock, never a result queue).")

(defun %socket-wait-register (sock wait)
  (when (and sock wait)
    (sb-thread:with-recursive-lock (*socket-wait-lock*)
      (pushnew wait (wasi-tcp-socket-waits sock) :test #'eq))))

(defun %socket-wait-unregister (sock wait)
  (when (and sock wait)
    (sb-thread:with-recursive-lock (*socket-wait-lock*)
      (setf (wasi-tcp-socket-waits sock)
            (delete wait (wasi-tcp-socket-waits sock) :test #'eq)))))

(defun %socket-waits-release (sock)
  "Release every watch a SOCK's streams still hold (CLOSE): each is
disarmed exactly once."
  (when sock
    (let ((waits nil))
      (sb-thread:with-recursive-lock (*socket-wait-lock*)
        (setf waits (wasi-tcp-socket-waits sock)
              (wasi-tcp-socket-waits sock) nil))
      (dolist (wait waits) (%readiness-disarm wait)))))

(defun %getsockname-domain (fd what)
  "getsockname/getpeername as a WIT ip-socket-address domain value."
  (let ((alien (sb-alien:make-alien sb-alien:unsigned-char +sockaddr-storage-size+))
        (len (sb-alien:make-alien sb-alien:unsigned-int)))
    (unwind-protect
         (progn
           (setf (sb-alien:deref len) +sockaddr-storage-size+)
           (%socket-check
            (if (eq what :peer)
                (%libc-getpeername fd (sb-alien:alien-sap alien)
                                   (sb-alien:alien-sap len))
                (%libc-getsockname fd (sb-alien:alien-sap alien)
                                   (sb-alien:alien-sap len)))
            "getsockname")
           (sockaddr->ip-socket-address (sb-alien:alien-sap alien)))
      (sb-alien:free-alien alien)
      (sb-alien:free-alien len))))

(defun %family-of-value (value)
  "The :ipv4/:ipv6 a WIT ip-address-family domain value denotes."
  (let ((name (cond ((and (consp value) (eq (car value) :enum)) (cdr value))
                    ((keywordp value) (string-downcase (symbol-name value)))
                    (t value))))
    (cond ((string-equal name "ipv4") :ipv4)
          ((string-equal name "ipv6") :ipv6)
          (t (%socket-error (%socket-code "not-supported"))))))

(defun %family-value (family)
  (cons :enum (ecase family (:ipv4 "ipv4") (:ipv6 "ipv6"))))

(defun %wildcard-host (family)
  (ecase family (:ipv4 '(0 0 0 0)) (:ipv6 '(0 0 0 0 0 0 0 0))))

;;; ---------------------------------------------------------------
;;; The socket resources.

(defstruct wasi-tcp-socket
  "The `tcp-socket' resource rep: the OS fd + the capability state.
REFS is the shared-ownership count the WIT mandates (the handle plus
every derived send/receive/listen stream); the fd closes at zero."
  (fd -1)
  (network nil)
  (family :ipv4)
  (state :unbound)                    ; :unbound/:bound/:listening/:connected/:closed
  (listen-p nil)
  (sent-p nil)
  (received-p nil)
  (backlog 128)
  ;; M17 step 6: the LIVE readiness watches this socket's streams hold
  ;; (released by CLOSE and by each stream's drop).
  (waits nil)
  (refs 1))

(defstruct wasi-udp-socket
  "The `udp-socket' resource rep."
  (fd -1)
  (network nil)
  (family :ipv4)
  (bound-p nil)
  (connected-p nil)
  (remote nil)                        ; the connected ip-socket-address domain
  (refs 1))

(defun %socket-ct (name &key (package (wasi-package "sockets")))
  (wasmdown.wit:wit-type->ct
   (wasmdown.wit:package-interface package "types")
   (list :resource name)
   :packages (%sockets-packages :package package)))

(defun tcp-socket-resource-ct (&key (package (wasi-package "sockets")))
  (%socket-ct "tcp-socket" :package package))

(defun udp-socket-resource-ct (&key (package (wasi-package "sockets")))
  (%socket-ct "udp-socket" :package package))

;;; M17: there is NO provider-side "already installed" ledger any more.
;;; The tcp-socket/udp-socket host finalizers (CLOSE-WASI-TCP-SOCKET /
;;; CLOSE-WASI-UDP-SOCKET, run by a guest's canon resource.drop with
;;; the rep) are registered with HOST-REGISTER-RESOURCE-DTOR on each
;;; SOCKETS-TYPES-INSTANCE that adopts them, so the registry is per
;;; (instance, ct): two providers of wasi:sockets keep distinct
;;; releases.

(defun %release-tcp (sock &optional shutdown-how)
  "Drop one reference; SHUTDOWN-HOW (when non-NIL) shuts the OS socket
down first (a send stream's drop = SHUT_WR, a receive stream's =
SHUT_RD)."
  (when (wasi-tcp-socket-p sock)
    (let ((fd (wasi-tcp-socket-fd sock)))
      (when (and shutdown-how (>= fd 0))
        (ignore-errors (%libc-shutdown fd shutdown-how)))
      (close-wasi-tcp-socket sock))))

(defun close-wasi-tcp-socket (sock)
  "Decode one reference to SOCK's fd; close it when the last goes."
  (when (wasi-tcp-socket-p sock)
    (when (plusp (wasi-tcp-socket-refs sock))
      (decf (wasi-tcp-socket-refs sock))
      (when (zerop (wasi-tcp-socket-refs sock))
        (let ((fd (wasi-tcp-socket-fd sock)))
          (setf (wasi-tcp-socket-fd sock) -1
                (wasi-tcp-socket-state sock) :closed)
          ;; a watch on this fd must not survive the fd
          (%socket-waits-release sock)
          (when (and (integerp fd) (not (minusp fd)))
            (%close-fd fd)))))))

(defun close-wasi-udp-socket (sock)
  (when (wasi-udp-socket-p sock)
    (when (plusp (wasi-udp-socket-refs sock))
      (decf (wasi-udp-socket-refs sock))
      (when (zerop (wasi-udp-socket-refs sock))
        (let ((fd (wasi-udp-socket-fd sock)))
          (setf (wasi-udp-socket-fd sock) -1)
          (when (and (integerp fd) (not (minusp fd)))
            (%close-fd fd)))))))

(defun %tcp-of (network value ct)
  "The wasi-tcp-socket VALUE denotes, resolved through the caller's
handle table (a borrow) and checked against THIS provider's network
authority."
  (let ((sock (cond ((wasi-tcp-socket-p value) value)
                    ((integerp value)
                     (or (wasmdown.component.runtime:host-resource-rep ct value)
                         (%socket-error (%socket-code "invalid-state"))))
                    (t (%socket-error (%socket-code "invalid-argument"))))))
    (unless (wasi-tcp-socket-p sock)
      (%socket-error (%socket-code "invalid-state")))
    (unless (eq (wasi-tcp-socket-network sock) network)
      (%socket-error (%socket-code "access-denied")))
    (when (eq (wasi-tcp-socket-state sock) :closed)
      (%socket-error (%socket-code "invalid-state")))
    sock))

(defun %udp-of (network value ct)
  (let ((sock (cond ((wasi-udp-socket-p value) value)
                    ((integerp value)
                     (or (wasmdown.component.runtime:host-resource-rep ct value)
                         (%socket-error (%socket-code "invalid-state"))))
                    (t (%socket-error (%socket-code "invalid-argument"))))))
    (unless (wasi-udp-socket-p sock)
      (%socket-error (%socket-code "invalid-state")))
    (unless (eq (wasi-udp-socket-network sock) network)
      (%socket-error (%socket-code "access-denied")))
    sock))

(defun %tcp-fd (sock)
  (let ((fd (wasi-tcp-socket-fd sock)))
    (unless (and (integerp fd) (not (minusp fd)))
      (%socket-error (%socket-code "invalid-state")))
    fd))

(defun %udp-fd (sock)
  (let ((fd (wasi-udp-socket-fd sock)))
    (unless (and (integerp fd) (not (minusp fd)))
      (%socket-error (%socket-code "invalid-state")))
    fd))

;;; ---------------------------------------------------------------
;;; The WIT resource/functype helpers for the sockets interfaces.

(defun %sockets-packages (&key (package (wasi-package "sockets")))
  "The WIT packages sockets functypes resolve against (duration comes
from wasi:clocks/types)."
  (list package (wasi-package "clocks")))

(defun %sockets-resource-method (resource method
                                 &key (package (wasi-package "sockets")))
  (let* ((interface (wasmdown.wit:package-interface package "types"))
         (res (find resource
                    (wasmdown.wit:wit-interface-resource-list interface)
                    :key #'wasmdown.wit:wit-resource-name :test #'string=))
         (fn (and res
                  (find method
                        (wasmdown.wit:wit-resource-methods res)
                        :key #'wasmdown.wit:wit-func-name :test #'string=))))
    (unless fn
      (error 'wasmdown:host-error
             :message (format nil "wasi:sockets/types has no ~a.~a"
                              resource method)))
    (values interface res fn)))

(defun %sockets-method-ct (resource method
                           &key (package (wasi-package "sockets")))
  (multiple-value-bind (interface res fn)
      (%sockets-resource-method resource method :package package)
    (%resource-method-ct interface res fn (%sockets-packages :package package))))

(defun %sockets-result-ct (resource method
                           &key (package (wasi-package "sockets")))
  (second (wasmdown.component:ct-payload
           (%sockets-method-ct resource method :package package))))

(defun %ct-find-kind (ct kind)
  (cond
    ((null ct) nil)
    ((eq (wasmdown.component:ct-kind ct) kind) ct)
    ((member (wasmdown.component:ct-kind ct) '(:tuple :record))
     (some (lambda (part)
             (%ct-find-kind (if (consp part) (cdr part) part) kind))
           (coerce (wasmdown.component:ct-payload ct) 'list)))
    ((eq (wasmdown.component:ct-kind ct) :result)
     (let ((payload (wasmdown.component:ct-payload ct)))
       (or (%ct-find-kind (car payload) kind)
           (%ct-find-kind (cdr payload) kind))))
    ((eq (wasmdown.component:ct-kind ct) :option)
     (%ct-find-kind (wasmdown.component:ct-payload ct) kind))
    ((eq (wasmdown.component:ct-kind ct) :variant)
     (some (lambda (case)
             (%ct-find-kind (cdr case) kind))
           (coerce (wasmdown.component:ct-payload ct) 'list)))
    ((eq (wasmdown.component:ct-kind ct) :list)
     (%ct-find-kind (car (wasmdown.component:ct-payload ct)) kind))
    (t nil)))

(defun %sockets-future-ct (resource method
                           &key (package (wasi-package "sockets")))
  "The ELEMENT ct of the method's future<result<_, error-code>>."
  (let ((fc (%ct-find-kind (%sockets-result-ct resource method
                                                :package package)
                           :future)))
    (unless fc
      (error 'wasmdown:host-error
             :message (format nil "~a.~a has no future result"
                              resource method)))
    (wasmdown.component:ct-payload fc)))

(defun %sockets-stream-ct (resource method
                           &key (package (wasi-package "sockets")))
  "The ELEMENT ct of the method's stream<T> result."
  (let ((sc (%ct-find-kind (%sockets-result-ct resource method
                                                :package package)
                           :stream)))
    (unless sc
      (error 'wasmdown:host-error
             :message (format nil "~a.~a has no stream result"
                              resource method)))
    (wasmdown.component:ct-payload sc)))

(defun %socket-error-future (future-ct code)
  "The REFUSED shape of a future-returning method: a future already
resolved to (:err . CODE)."
  (multiple-value-bind (reader writer)
      (wasmdown.component.runtime:make-host-future-pair future-ct)
    (wasmdown.component.runtime:host-future-resolve writer (cons :err code))
    reader))

(defun %dropped-stream-and-future (elem-ct future-ct code)
  "The REFUSED shape of a receive(): a dropped stream end + a future
resolved to (:err . CODE)."
  (multiple-value-bind (reader writer)
      (wasmdown.component.runtime:make-host-stream-pair elem-ct)
    (wasmdown.component.runtime:host-end-drop writer)
    (list reader (%socket-error-future future-ct code))))

;;; ---------------------------------------------------------------
;;; TCP.

(defun tcp-create (context network family-value
                   &key (package (wasi-package "sockets")))
  "socket(AF_INET|AF_INET6, SOCK_STREAM|NONBLOCK|CLOEXEC, IPPROTO_TCP).
IPV6_V6ONLY is on (the WIT's default) and SO_REUSEADDR is set so a
TIME_WAIT binding does not block the guest."
  (declare (ignore context))
  (%socket-result
   (let* ((family (%family-of-value family-value))
          (fd (%libc-socket
               (%family-key family)
               (logior +sock-stream+ +sock-nonblock+ +sock-cloexec+)
               +ipproto-tcp+)))
     (%socket-check fd "socket")
     (%setsockopt-int fd +sol-socket+ +so-reuseaddr+ 1)
     (when (eq family :ipv6)
       (%setsockopt-int fd +ipproto-ipv6+ +ipv6-v6only+ 1))
     (list :own-handle
           (tcp-socket-resource-ct :package package)
           (make-wasi-tcp-socket :fd fd :network network :family family)))))

(defun %implicit-bind-tcp (sock network)
  "Bind SOCK to the family's wildcard address on an ephemeral port
(the WIT's implicit bind), consulting the listen policy first."
  (multiple-value-bind (alien len family)
      (%make-sockaddr (ecase (wasi-tcp-socket-family sock)
                        (:ipv4 "ipv4") (:ipv6 "ipv6"))
                      (list (cons "port" 0)
                            (cons "address" (%wildcard-host
                                             (wasi-tcp-socket-family sock)))))
    (unwind-protect
         (multiple-value-bind (fam host port) (%sockaddr-sap->key
                                               (sb-alien:alien-sap alien))
           (declare (ignore fam))
           (unless (wasi-network-allows-listen-p network family host port)
             (%socket-error (%socket-code "access-denied")))
           (%socket-check (%libc-bind (%tcp-fd sock)
                                      (sb-alien:alien-sap alien) len)
                          "bind")
           (setf (wasi-tcp-socket-state sock) :bound))
      (sb-alien:free-alien alien))))

(defun tcp-bind (context network self value
                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                      :package package))))
     (unless (eq (wasi-tcp-socket-state sock) :unbound)
       (%socket-error (%socket-code "invalid-state")))
     (multiple-value-bind (alien len family)
         (ip-socket-address->sockaddr value)
       (unwind-protect
            (progn
              (unless (eq family (wasi-tcp-socket-family sock))
                (%socket-error (%socket-code "invalid-argument")))
              (multiple-value-bind (fam host port)
                  (%sockaddr-sap->key (sb-alien:alien-sap alien))
                (declare (ignore fam))
                (unless (wasi-network-allows-listen-p network family host port)
                  (%socket-error (%socket-code "access-denied")))
                (%socket-check (%libc-bind (%tcp-fd sock)
                                           (sb-alien:alien-sap alien) len)
                               "bind")
                (setf (wasi-tcp-socket-state sock) :bound)
                nil))
         (sb-alien:free-alien alien))))))

(defun tcp-connect (context network self value
                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let ((sock nil))
    (handler-case
        (progn
          (setf sock (%tcp-of network self (tcp-socket-resource-ct
                                            :package package)))
          (when (member (wasi-tcp-socket-state sock)
                        '(:connecting :connected :listening))
            (%socket-error (%socket-code "invalid-state")))
          (multiple-value-bind (alien len family)
              (ip-socket-address->sockaddr value)
            (unwind-protect
                 (progn
                   (unless (eq family (wasi-tcp-socket-family sock))
                     (%socket-error (%socket-code "invalid-argument")))
                   (multiple-value-bind (fam host port)
                       (%sockaddr-sap->key (sb-alien:alien-sap alien))
                     (declare (ignore fam))
                     (unless (wasi-network-allows-outbound-p
                              network family host port)
                       (%socket-error (%socket-code "access-denied")))
                     (%connect-fd (%tcp-fd sock) alien len)
                     (setf (wasi-tcp-socket-state sock) :connected)))
              (sb-alien:free-alien alien)))
          (cons :ok nil))
      (wasi-socket-error (c)
        ;; the WIT: after a failed connect the socket is CLOSED and
        ;; the only valid action is drop (a capability refusal also
        ;; leaves no usable socket).
        (when sock
          (setf (wasi-tcp-socket-state sock) :closed)
          (let ((fd (wasi-tcp-socket-fd sock)))
            (setf (wasi-tcp-socket-fd sock) -1)
            (when (and (integerp fd) (not (minusp fd))) (%close-fd fd))))
        (cons :err (wasi-socket-error-code c))))))

(defun tcp-listen (context network self
                   &key (package (wasi-package "sockets")))
  (let ((elem-ct (%sockets-stream-ct "tcp-socket" "listen" :package package)))
    (handler-case
        (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                           :package package))))
          (when (member (wasi-tcp-socket-state sock)
                        '(:listening :connecting :connected))
            (%socket-error (%socket-code "invalid-state")))
          (when (eq (wasi-tcp-socket-state sock) :unbound)
            (%implicit-bind-tcp sock network))
          (%socket-check (%libc-listen (%tcp-fd sock)
                                       (wasi-tcp-socket-backlog sock))
                         "listen")
          (setf (wasi-tcp-socket-state sock) :listening
                (wasi-tcp-socket-listen-p sock) t)
          (incf (wasi-tcp-socket-refs sock))
          (multiple-value-bind (reader writer)
              (wasmdown.component.runtime:make-host-stream-pair elem-ct)
            ;; M17 step 6: a would-blocked accept waits on the LISTENER's
            ;; fd through the reactor, keyed by this stream's identity
            (let ((wait (%make-readiness-wait writer))
                  (reactor (%context-reactor context)))
              (%socket-wait-register sock wait)
              (wasmdown.component.runtime:host-end-set-source
               writer (%tcp-accept-source sock
                                          :reactor reactor
                                          :identity writer
                                          :wait wait))
              (wasmdown.component.runtime:host-end-set-on-drop
               writer (lambda (dropped error)
                        (declare (ignore dropped error))
                        (%readiness-disarm wait)
                        (%socket-wait-unregister sock wait)
                        (%release-tcp sock)))
              (cons :ok reader))))
      (wasi-socket-error (c)
        (cons :err (wasi-socket-error-code c))))))

(defun %tcp-accept-source (sock &key reactor identity wait)
  "A host SOURCE yielding new tcp-socket OWN HANDLES: (max) ->
(values element-vector eof-p), ELEMENT-VECTOR a REUSABLE vector whose
length is the count. accept4 is already nonblocking: EAGAIN/EINTR is
WOULD-BLOCK — (NIL NIL) when nothing was accepted, else the entries
accepted so far with eof NIL — never an accept spin; a fatal error
ends the stream.

With a REACTOR, an empty queue instead REGISTERS a read watch on the
listener (keyed by this stream's IDENTITY) and the parked copy resumes
through the store queue."
  (let ((ct (tcp-socket-resource-ct))
        (family (wasi-tcp-socket-family sock))
        (fd (wasi-tcp-socket-fd sock))
        (out (make-array 0 :adjustable t :fill-pointer 0)))
    (lambda (max)
      (if (<= max 0)
          (values nil nil)
          (progn
            (setf (fill-pointer out) 0)
            (let ((eof nil)
                  (blocked nil))
              (loop while (and (< (fill-pointer out) max) (not eof)
                               (not blocked))
                    do (sb-alien:set-errno 0)
                       (let ((new (%libc-accept4 (wasi-tcp-socket-fd sock)
                                                 (%sock-null) (%sock-null)
                                                 (logior +sock-nonblock+
                                                         +sock-cloexec+))))
                         (cond
                           ((>= new 0)
                            (vector-push-extend
                             (list :own-handle ct
                                   (make-wasi-tcp-socket
                                    :fd new
                                    :network (wasi-tcp-socket-network sock)
                                    :family family
                                    :state :connected))
                             out))
                           (t
                            (let ((errno (sb-alien:get-errno)))
                              (if (or (= errno +eagain+) (= errno +eintr+))
                                  ;; would-block: nothing more RIGHT NOW;
                                  ;; the copy parks until a readiness wake
                                  (setf blocked t)
                                  (setf eof t)))))))
              (cond
                ((plusp (fill-pointer out))
                 (%readiness-disarm wait)
                 (values out eof))
                (eof
                 (%readiness-disarm wait)
                 (values nil t))
                (t
                 (%readiness-arm wait reactor fd :read identity)
                 (values nil nil)))))))))

(defun %fd-send-sink (fd &key reactor identity wait)
  "A host SINK over send(2) on the connected stream socket FD:
(octets start end) -> (values consumed error-or-nil). ONE send per
call: a short send is the count ACCEPTED (the remainder continues on
the guest's next attempt), EAGAIN/EWOULDBLOCK is WOULD-BLOCK (0 NIL,
nothing consumed), a zero-length send is connection-broken, and
MSG_NOSIGNAL keeps a peer reset from killing the host. The error is
the sockets error-code DOMAIN value.

With a REACTOR, a would-block REGISTERS a write watch (keyed by the
stream IDENTITY, never the fd) and the parked copy resumes through the
store queue; the watch is released once on the next progress, error or
close."
  (lambda (octets offset limit)
    (let ((n (- limit offset)))
      (if (<= n 0)
          (values 0 nil)
          (loop
            (sb-alien:set-errno 0)
            (let ((ret (sb-sys:with-pinned-objects (octets)
                         (%libc-send fd
                                     (sb-sys:sap+ (sb-sys:vector-sap octets)
                                                  offset)
                                     n +msg-nosignal+))))
              (cond
                ((plusp ret)
                 (%readiness-disarm wait)
                 (return (values ret nil)))
                ((zerop ret)
                 (%readiness-disarm wait)
                 (return (values 0 (%socket-code "connection-broken"))))
                (t
                 (let ((errno (sb-alien:get-errno)))
                   (cond
                     ((= errno +eintr+) nil)   ; retry the same span
                     ((= errno +eagain+)
                      (%readiness-arm wait reactor fd :write identity)
                      (return (values 0 nil)))
                     (t
                      (%readiness-disarm wait)
                      (return (values 0 (%socket-errno-code errno))))))))))))))

(defun tcp-send (context network self data
                 &key (package (wasi-package "sockets")))
  (let ((future-ct (%sockets-future-ct "tcp-socket" "send"
                                        :package package))
        (reactor (%context-reactor context)))
    (handler-case
        (let* ((sock (%tcp-of network self (tcp-socket-resource-ct
                                            :package package)))
               (fd (%tcp-fd sock)))
          (unless (eq (wasi-tcp-socket-state sock) :connected)
            (%socket-error (%socket-code "invalid-state")))
          (when (wasi-tcp-socket-sent-p sock)
            (%socket-error (%socket-code "invalid-state")))
          (unless (wasmdown.component.runtime:async-end-p data)
            (%socket-error (%socket-code "invalid-argument")))
          (setf (wasi-tcp-socket-sent-p sock) t)
          (multiple-value-bind (reader writer)
              (wasmdown.component.runtime:make-host-future-pair future-ct)
            (let ((settled nil)
                  (released nil)
                  (wait (%make-readiness-wait data)))
              (flet ((settle (v)
                       (unless settled
                         (setf settled t)
                         (wasmdown.component.runtime:host-future-resolve
                          writer v)))
                     (release (&optional shutdown-how)
                       (unless released
                         (setf released t)
                         (%readiness-disarm wait)
                         (%socket-wait-unregister sock wait)
                         (%release-tcp sock shutdown-how))))
                (incf (wasi-tcp-socket-refs sock))
                (%socket-wait-register sock wait)
                (wasmdown.component.runtime:host-end-set-sink
                 data (%fd-send-sink fd :reactor reactor
                                        :identity data :wait wait))
                (wasmdown.component.runtime:host-end-set-on-drop
                 data
                 (lambda (dropped error)
                   (declare (ignore dropped))
                   (if error
                       (progn (settle (cons :err error))
                              (release nil))
                       (progn (settle (cons :ok nil))
                              (release +shut-wr+)))))
                (wasmdown.component.runtime:host-end-drain data)
                reader))))
      (wasi-socket-error (c)
        (when (wasmdown.component.runtime:async-end-p data)
          (wasmdown.component.runtime:host-end-drop data))
        (%socket-error-future future-ct (wasi-socket-error-code c))))))

(defun %tcp-recv-source (fd settle &key reactor identity wait)
  "A host SOURCE over recv(2): (max) -> (values octets eof-p),
OCTETS a REUSABLE u8 vector whose length is the count. ONE
nonblocking recv per attempt: EAGAIN/EINTR is WOULD-BLOCK ((NIL NIL)),
so the guest's read parks on readiness instead of polling the host
thread. A zero-length recv is a graceful FIN and SETTLES the future
(:ok) with an EMPTY vector and EOF; a recv error SETTLES it
(:err . code) and reports EOF.

With a REACTOR the would-block REGISTERS a read watch keyed by the
stream IDENTITY and the parked copy resumes through the store queue;
data/EOF/error release the watch once."
  (let ((slice (make-array 65536 :element-type '(unsigned-byte 8)))
        (out (make-array 65536 :element-type '(unsigned-byte 8)
                                :fill-pointer 0)))
    (lambda (max)
      (if (<= max 0)
          (values nil nil)
          (let ((want (min max 65536)))
            (sb-alien:set-errno 0)
            (let ((n (sb-sys:with-pinned-objects (slice)
                       (%libc-recv fd (sb-sys:vector-sap slice) want 0))))
              (cond
                ((plusp n)
                 (%readiness-disarm wait)
                 (setf (fill-pointer out) n)
                 (replace out slice :start2 0 :end2 n)
                 (values out nil))
                ((zerop n)
                 (%readiness-disarm wait)
                 (funcall settle (cons :ok nil))
                 (values #() t))
                (t
                 (let ((errno (sb-alien:get-errno)))
                   (cond
                     ((or (= errno +eagain+) (= errno +eintr+))
                      (%readiness-arm wait reactor fd :read identity)
                      (values nil nil))
                     (t
                      (%readiness-disarm wait)
                      (funcall settle
                               (cons :err (%socket-errno-code errno)))
                      (values nil t))))))))))))

(defun tcp-receive (context network self
                    &key (package (wasi-package "sockets")))
  (let* ((ft (%sockets-method-ct "tcp-socket" "receive" :package package))
         (result (second (wasmdown.component:ct-payload ft)))
         (elem-ct (wasmdown.component:ct-payload
                   (%ct-find-kind result :stream)))
         (future-ct (wasmdown.component:ct-payload
                     (%ct-find-kind result :future)))
         (reactor (%context-reactor context)))
    (handler-case
        (let* ((sock (%tcp-of network self (tcp-socket-resource-ct
                                            :package package)))
               (fd (%tcp-fd sock)))
          (unless (eq (wasi-tcp-socket-state sock) :connected)
            (%socket-error (%socket-code "invalid-state")))
          (when (wasi-tcp-socket-received-p sock)
            (%socket-error (%socket-code "invalid-state")))
          (setf (wasi-tcp-socket-received-p sock) t)
          (multiple-value-bind (stream-reader stream-writer)
              (wasmdown.component.runtime:make-host-stream-pair elem-ct)
            (multiple-value-bind (future-reader future-writer)
                (wasmdown.component.runtime:make-host-future-pair future-ct)
              (let ((settled nil)
                    (released nil)
                    (wait (%make-readiness-wait stream-writer)))
                (flet ((settle (v)
                         (unless settled
                           (setf settled t)
                           (wasmdown.component.runtime:host-future-resolve
                            future-writer v)))
                       (release (&optional shutdown-how)
                         (unless released
                           (setf released t)
                           (%readiness-disarm wait)
                           (%socket-wait-unregister sock wait)
                           (%release-tcp sock shutdown-how))))
                  (incf (wasi-tcp-socket-refs sock))
                  (%socket-wait-register sock wait)
                  (wasmdown.component.runtime:host-end-set-source
                   stream-writer (%tcp-recv-source fd #'settle
                                                  :reactor reactor
                                                  :identity stream-writer
                                                  :wait wait))
                  (wasmdown.component.runtime:host-end-set-on-drop
                   stream-writer
                   (lambda (dropped error)
                     (declare (ignore dropped))
                     (settle (if error (cons :err error) (cons :ok nil)))
                     (release +shut-rd+)))
                  (list stream-reader future-reader))))))
      (wasi-socket-error (c)
        (%dropped-stream-and-future elem-ct future-ct
                                    (wasi-socket-error-code c))))))

(defun tcp-get-local-address (context network self
                             &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                      :package package))))
     (when (eq (wasi-tcp-socket-state sock) :unbound)
       (%socket-error (%socket-code "invalid-state")))
     (%getsockname-domain (%tcp-fd sock) :local))))

(defun tcp-get-remote-address (context network self
                              &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                      :package package))))
     (unless (eq (wasi-tcp-socket-state sock) :connected)
       (%socket-error (%socket-code "invalid-state")))
     (%getsockname-domain (%tcp-fd sock) :peer))))

(defun tcp-get-is-listening (context network self
                             &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (eq (wasi-tcp-socket-state
       (%tcp-of network self (tcp-socket-resource-ct :package package)))
      :listening))

(defun tcp-get-address-family (context network self
                               &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%family-value
   (wasi-tcp-socket-family
    (%tcp-of network self (tcp-socket-resource-ct :package package)))))

(defun tcp-set-listen-backlog-size (context network self value
                                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                      :package package))))
     (when (zerop value)
       (%socket-error (%socket-code "invalid-argument")))
     (when (member (wasi-tcp-socket-state sock) '(:connecting :connected))
       (%socket-error (%socket-code "invalid-state")))
     (setf (wasi-tcp-socket-backlog sock) (min value #x7fffffff))
     (when (eq (wasi-tcp-socket-state sock) :listening)
       (%socket-check (%libc-listen (%tcp-fd sock)
                                    (wasi-tcp-socket-backlog sock))
                      "listen"))
     nil)))

;;; The keep-alive / hop-limit / buffer knobs. Duration values are
;;; nanoseconds (wasi:clocks/types.duration); the kernel takes ms.

(defun %ns->ms (ns)
  "A positive duration in ms (the WIT rejects 0; a sub-ms value
rounds up rather than becoming 0)."
  (max 1 (floor ns 1000000)))

(defun tcp-get-keep-alive-enabled (context network self
                                   &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (not (zerop (%getsockopt-int (%tcp-fd
                                 (%tcp-of network self
                                          (tcp-socket-resource-ct
                                           :package package)))
                                +sol-socket+ +so-keepalive+)))))

(defun tcp-set-keep-alive-enabled (context network self value
                                   &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (%setsockopt-int (%tcp-fd (%tcp-of network self
                                      (tcp-socket-resource-ct
                                       :package package)))
                    +sol-socket+ +so-keepalive+ (if value 1 0))
   nil))

(defun %tcp-knob-get (network self ct getter)
  (%socket-result (%getsockopt-int (%tcp-fd (%tcp-of network self ct))
                                   +sol-tcp+ getter)))

(defun %tcp-knob-set (network self ct level opt value check)
  (%socket-result
   (when (and check (zerop value))
     (%socket-error (%socket-code "invalid-argument")))
   (%setsockopt-int (%tcp-fd (%tcp-of network self ct)) level opt value)
   nil))

(defun tcp-get-keep-alive-idle-time (context network self
                                     &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (* 1000000 (%getsockopt-int (%tcp-fd (%tcp-of
                                         network self
                                         (tcp-socket-resource-ct
                                          :package package)))
                                +sol-tcp+ +tcp-keepidle+))))

(defun tcp-set-keep-alive-idle-time (context network self value
                                     &key (package (wasi-package "sockets")))
  (declare (ignore context))
  ;; the WIT rejects a ZERO duration before it reaches the kernel
  (%socket-result
   (when (zerop value)
     (%socket-error (%socket-code "invalid-argument")))
   (%tcp-knob-set network self (tcp-socket-resource-ct :package package)
                  +sol-tcp+ +tcp-keepidle+ (%ns->ms value) nil)
   nil))

(defun tcp-get-keep-alive-interval (context network self
                                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (* 1000000 (%getsockopt-int (%tcp-fd (%tcp-of
                                         network self
                                         (tcp-socket-resource-ct
                                          :package package)))
                                +sol-tcp+ +tcp-keepintvl+))))

(defun tcp-set-keep-alive-interval (context network self value
                                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (when (zerop value)
     (%socket-error (%socket-code "invalid-argument")))
   (%tcp-knob-set network self (tcp-socket-resource-ct :package package)
                  +sol-tcp+ +tcp-keepintvl+ (%ns->ms value) nil)
   nil))

(defun tcp-get-keep-alive-count (context network self
                                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (%getsockopt-int (%tcp-fd (%tcp-of network self
                                      (tcp-socket-resource-ct
                                       :package package)))
                    +sol-tcp+ +tcp-keepcnt+)))

(defun tcp-set-keep-alive-count (context network self value
                                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%tcp-knob-set network self (tcp-socket-resource-ct :package package)
                 +sol-tcp+ +tcp-keepcnt+ value t))

(defun tcp-get-hop-limit (context network self
                          &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                      :package package))))
    (%socket-result
     (let ((level (if (eq (wasi-tcp-socket-family sock) :ipv6)
                      +ipproto-ipv6+ +ipproto-ip+))
           (opt (if (eq (wasi-tcp-socket-family sock) :ipv6)
                    +ipv6-unicast-hops+ +ip-ttl+)))
       (%getsockopt-int (%tcp-fd sock) level opt)))))

(defun tcp-set-hop-limit (context network self value
                          &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                      :package package))))
    (%socket-result
     (when (or (zerop value) (> value 255))
       (%socket-error (%socket-code "invalid-argument")))
     (let ((level (if (eq (wasi-tcp-socket-family sock) :ipv6)
                      +ipproto-ipv6+ +ipproto-ip+))
           (opt (if (eq (wasi-tcp-socket-family sock) :ipv6)
                    +ipv6-unicast-hops+ +ip-ttl+)))
       (%setsockopt-int (%tcp-fd sock) level opt value))
     nil)))

(defun %fd-get-buffer (fd opt)
  "SO_RCVBUF/SO_SNDBUF. Linux reports DOUBLE the requested size."
  (%socket-result
   (floor (%getsockopt-int fd +sol-socket+ opt) 2)))

(defun %fd-set-buffer (fd opt value)
  (%socket-result
   (when (zerop value)
     (%socket-error (%socket-code "invalid-argument")))
   (%setsockopt-int fd +sol-socket+ opt (min value #x7fffffff))
   nil))

(defun tcp-get-receive-buffer-size (context network self
                                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%fd-get-buffer (%tcp-fd (%tcp-of network self
                                    (tcp-socket-resource-ct :package package)))
                  +so-rcvbuf+))

(defun tcp-set-receive-buffer-size (context network self value
                                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%fd-set-buffer (%tcp-fd (%tcp-of network self
                                    (tcp-socket-resource-ct :package package)))
                  +so-rcvbuf+ value))

(defun tcp-get-send-buffer-size (context network self
                                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%fd-get-buffer (%tcp-fd (%tcp-of network self
                                    (tcp-socket-resource-ct :package package)))
                  +so-sndbuf+))

(defun tcp-set-send-buffer-size (context network self value
                                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%fd-set-buffer (%tcp-fd (%tcp-of network self
                                    (tcp-socket-resource-ct :package package)))
                  +so-sndbuf+ value))

;;; ---------------------------------------------------------------
;;; UDP.

(defun udp-create (context network family-value
                   &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let* ((family (%family-of-value family-value))
          (fd (%libc-socket
               (%family-key family)
               (logior +sock-dgram+ +sock-nonblock+ +sock-cloexec+)
               +ipproto-udp+)))
     (%socket-check fd "socket")
     (%setsockopt-int fd +sol-socket+ +so-reuseaddr+ 1)
     (when (eq family :ipv6)
       (%setsockopt-int fd +ipproto-ipv6+ +ipv6-v6only+ 1))
     (list :own-handle
           (udp-socket-resource-ct :package package)
           (make-wasi-udp-socket :fd fd :network network :family family)))))

(defun udp-bind (context network self value
                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%udp-of network self (udp-socket-resource-ct
                                      :package package))))
     (when (wasi-udp-socket-bound-p sock)
       (%socket-error (%socket-code "invalid-state")))
     (multiple-value-bind (alien len family)
         (ip-socket-address->sockaddr value)
       (unwind-protect
            (progn
              (unless (eq family (wasi-udp-socket-family sock))
                (%socket-error (%socket-code "invalid-argument")))
              (multiple-value-bind (fam host port)
                  (%sockaddr-sap->key (sb-alien:alien-sap alien))
                (declare (ignore fam))
                (unless (wasi-network-allows-listen-p network family host port)
                  (%socket-error (%socket-code "access-denied")))
                (%socket-check (%libc-bind (%udp-fd sock)
                                           (sb-alien:alien-sap alien) len)
                               "bind")
                (setf (wasi-udp-socket-bound-p sock) t)
                nil))
         (sb-alien:free-alien alien))))))

(defun udp-connect (context network self value
                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%udp-of network self (udp-socket-resource-ct
                                      :package package))))
     (multiple-value-bind (alien len family)
         (ip-socket-address->sockaddr value)
       (unwind-protect
            (progn
              (unless (eq family (wasi-udp-socket-family sock))
                (%socket-error (%socket-code "invalid-argument")))
              (multiple-value-bind (fam host port)
                  (%sockaddr-sap->key (sb-alien:alien-sap alien))
                (declare (ignore fam))
                (unless (wasi-network-allows-outbound-p network family host port)
                  (%socket-error (%socket-code "access-denied")))
                (%socket-check (%libc-connect (%udp-fd sock)
                                              (sb-alien:alien-sap alien) len)
                               "connect")
                (setf (wasi-udp-socket-connected-p sock) t
                      (wasi-udp-socket-bound-p sock) t
                      (wasi-udp-socket-remote sock) value)
                nil))
         (sb-alien:free-alien alien))))))

(defun udp-disconnect (context network self
                       &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%udp-of network self (udp-socket-resource-ct
                                      :package package))))
     (unless (wasi-udp-socket-connected-p sock)
       (%socket-error (%socket-code "invalid-state")))
     (let ((alien (sb-alien:make-alien sb-alien:unsigned-char +sockaddr-storage-size+)))
       (unwind-protect
            (progn
              ;; AF_UNSPEC: the POSIX disconnect for a datagram socket
              (let ((sap (sb-alien:alien-sap alien)))
                (loop for i below +sockaddr-storage-size+
                      do (setf (sb-sys:sap-ref-8 sap i) 0))
                (%socket-check (%libc-connect (%udp-fd sock) sap
                                              +sockaddr-in-size+)
                               "connect"))
              (setf (wasi-udp-socket-connected-p sock) nil
                    (wasi-udp-socket-remote sock) nil)
              nil)
         (sb-alien:free-alien alien))))))

(defun udp-send (context network self data remote
                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let* ((sock (%udp-of network self (udp-socket-resource-ct
                                       :package package)))
          (fd (%udp-fd sock))
          (dest (cond ((eq remote :none) nil)
                      ((and (consp remote) (eq (car remote) :some))
                       (cdr remote))
                      ((and (consp remote) (stringp (car remote))) remote)
                      (t (%socket-error (%socket-code "invalid-argument"))))))
     (when (and dest (wasi-udp-socket-connected-p sock)
                (not (equal dest (wasi-udp-socket-remote sock))))
       (%socket-error (%socket-code "invalid-argument")))
     (when (and (null dest) (not (wasi-udp-socket-connected-p sock)))
       (%socket-error (%socket-code "invalid-argument")))
     (if dest
         (multiple-value-bind (alien len family)
             (ip-socket-address->sockaddr dest)
           (unwind-protect
                (multiple-value-bind (fam host port)
                    (%sockaddr-sap->key (sb-alien:alien-sap alien))
                  (declare (ignore fam))
                  (unless (wasi-network-allows-outbound-p
                           network family host port)
                    (%socket-error (%socket-code "access-denied")))
                  (%udp-send-bytes fd data alien len))
             (sb-alien:free-alien alien)))
         (%udp-send-bytes fd data nil nil))
     nil)))

(defun %udp-send-bytes (fd bytes alien len)
  "sendto/send BYTES; error-code DOMAIN or NIL."
  (let ((n (length bytes)))
    (if (zerop n)
        nil
        (let ((buf (%byte-list->alien bytes)))
          (unwind-protect
               (block write
                 (let ((done 0))
                   (loop while (< done n)
                         do (let ((ret
                                    (if alien
                                        (%libc-sendto fd
                                                      (sb-sys:sap+ (sb-alien:alien-sap buf) done)
                                                      (- n done)
                                                      +msg-nosignal+
                                                      (sb-alien:alien-sap alien) len)
                                        (%libc-send fd
                                                    (sb-sys:sap+ (sb-alien:alien-sap buf) done)
                                                    (- n done)
                                                    +msg-nosignal+))))
                              (cond
                                ((plusp ret) (incf done ret))
                                ((zerop ret) (return-from write nil))
                                (t
                                 (let ((errno (sb-alien:get-errno)))
                                   (cond
                                     ((= errno +eintr+) nil)
                                     ((= errno +eagain+)
                                      (unless (%poll-fd fd +pollout+ 5000)
                                        (return-from write
                                          (%socket-code "timeout"))))
                                     (t
                                      (return-from write
                                        (%socket-errno-code errno))))))))))
                 nil)
            (sb-alien:free-alien buf))))))

(defun udp-receive (context network self
                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let* ((sock (%udp-of network self (udp-socket-resource-ct
                                       :package package)))
          (fd (%udp-fd sock)))
     (unless (wasi-udp-socket-bound-p sock)
       (%socket-error (%socket-code "invalid-state")))
     (multiple-value-bind (bytes addr) (%udp-recv fd)
       (unless bytes
         (%socket-error (%socket-code "timeout")))
       (list bytes addr)))))

(defun %udp-recv (fd)
  "One datagram: (values byte-list address-domain) or (values nil NIL)
on a bounded wait expiry."
  (let ((cap 65536)
        (buf (sb-alien:make-alien sb-alien:unsigned-char 65536))
        (addr (sb-alien:make-alien sb-alien:unsigned-char +sockaddr-storage-size+))
        (len (sb-alien:make-alien sb-alien:unsigned-int)))
    (unwind-protect
         (block read
           (loop
             (setf (sb-alien:deref len) +sockaddr-storage-size+)
             (let ((ret (%libc-recvfrom fd (sb-alien:alien-sap buf) cap 0
                                        (sb-alien:alien-sap addr)
                                        (sb-alien:alien-sap len))))
               (cond
                 ((>= ret 0)
                  (return-from read
                    (values (loop for i below ret
                                  collect (sb-sys:sap-ref-8
                                           (sb-alien:alien-sap buf) i))
                            (sockaddr->ip-socket-address
                             (sb-alien:alien-sap addr)))))
                 (t
                  (let ((errno (sb-alien:get-errno)))
                    (cond
                      ((= errno +eintr+) nil)
                      ((= errno +eagain+)
                       (unless (%poll-fd fd +pollin+ 1000)
                         (return-from read (values nil nil))))
                      (t (return-from read
                           (%socket-error
                            (%socket-errno-code errno)))))))))))
      (sb-alien:free-alien buf)
      (sb-alien:free-alien addr)
      (sb-alien:free-alien len))))

(defun udp-get-local-address (context network self
                              &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%udp-of network self (udp-socket-resource-ct
                                      :package package))))
     (unless (wasi-udp-socket-bound-p sock)
       (%socket-error (%socket-code "invalid-state")))
     (%getsockname-domain (%udp-fd sock) :local))))

(defun udp-get-remote-address (context network self
                               &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (let ((sock (%udp-of network self (udp-socket-resource-ct
                                      :package package))))
     (unless (wasi-udp-socket-connected-p sock)
       (%socket-error (%socket-code "invalid-state")))
     (%getsockname-domain (%udp-fd sock) :peer))))

(defun udp-get-address-family (context network self
                               &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%family-value
   (wasi-udp-socket-family
    (%udp-of network self (udp-socket-resource-ct :package package)))))

(defun udp-get-unicast-hop-limit (context network self
                                  &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let ((sock (%udp-of network self (udp-socket-resource-ct
                                     :package package))))
    (%socket-result
     (let ((level (if (eq (wasi-udp-socket-family sock) :ipv6)
                      +ipproto-ipv6+ +ipproto-ip+))
           (opt (if (eq (wasi-udp-socket-family sock) :ipv6)
                    +ipv6-unicast-hops+ +ip-ttl+)))
       (%getsockopt-int (%udp-fd sock) level opt)))))

(defun udp-set-unicast-hop-limit (context network self value
                                  &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let ((sock (%udp-of network self (udp-socket-resource-ct
                                     :package package))))
    (%socket-result
     (when (or (zerop value) (> value 255))
       (%socket-error (%socket-code "invalid-argument")))
     (let ((level (if (eq (wasi-udp-socket-family sock) :ipv6)
                      +ipproto-ipv6+ +ipproto-ip+))
           (opt (if (eq (wasi-udp-socket-family sock) :ipv6)
                    +ipv6-unicast-hops+ +ip-ttl+)))
       (%setsockopt-int (%udp-fd sock) level opt value))
     nil)))

(defun udp-get-receive-buffer-size (context network self
                                   &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%fd-get-buffer (%udp-fd (%udp-of network self
                                    (udp-socket-resource-ct :package package)))
                  +so-rcvbuf+))

(defun udp-set-receive-buffer-size (context network self value
                                   &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%fd-set-buffer (%udp-fd (%udp-of network self
                                    (udp-socket-resource-ct :package package)))
                  +so-rcvbuf+ value))

(defun udp-get-send-buffer-size (context network self
                                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%fd-get-buffer (%udp-fd (%udp-of network self
                                    (udp-socket-resource-ct :package package)))
                  +so-sndbuf+))

(defun udp-set-send-buffer-size (context network self value
                                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%fd-set-buffer (%udp-fd (%udp-of network self
                                    (udp-socket-resource-ct :package package)))
                  +so-sndbuf+ value))

;;; ---------------------------------------------------------------
;;; ip-name-lookup.

(defun %split-string (string char)
  (let ((parts '()) (start 0))
    (loop
      (let ((pos (position char string :start start)))
        (if pos
            (progn (push (subseq string start pos) parts)
                   (setf start (1+ pos)))
            (progn (push (subseq string start) parts)
                   (return (nreverse parts))))))))

(defun %parse-ipv4-literal (name)
  "The 4-byte tuple NAME spells as a dotted quad, or NIL."
  (let ((parts (%split-string name #\.)))
    (when (= 4 (length parts))
      (let ((bytes (mapcar (lambda (p)
                             (when (and (plusp (length p))
                                        (every #'digit-char-p p))
                               (ignore-errors (parse-integer p))))
                           parts)))
        (when (every (lambda (v) (and (integerp v) (<= 0 v 255))) bytes)
          bytes)))))

(defun %gai->lookup-code (rc)
  (cons
   (cond
     ((member rc '(-2 -5 -9)) "name-unresolvable") ; EAI_NONAME/NODATA/ADDRFAMILY
     ((= rc -3) "temporary-resolver-failure")      ; EAI_AGAIN
     ((= rc -4) "permanent-resolver-failure")      ; EAI_FAIL
     ((= rc -1) "invalid-argument")                ; EAI_BADFLAGS
     ((= rc -11) "permanent-resolver-failure")     ; EAI_SYSTEM
     (t "other"))
   nil))

(defun %getaddrinfo-chain (name flags)
  "Resolve NAME through getaddrinfo(3) into a de-duplicated
`list<ip-address>' domain value. Signals wasi-socket-error with the
ip-name-lookup error variant on failure."
  (let ((hints (sb-alien:make-alien sb-alien:unsigned-char +addrinfo-size+))
        (out (sb-alien:make-alien
              (sb-alien:array sb-alien:unsigned-char 8))))
    (unwind-protect
         (let ((hsap (sb-alien:alien-sap hints))
               (osap (sb-alien:alien-sap out)))
           (loop for i below +addrinfo-size+
                 do (setf (sb-sys:sap-ref-8 hsap i) 0))
           (setf (sb-sys:sap-ref-32 hsap +ai-flags-offset+) flags)
           (let ((rc (%libc-getaddrinfo name nil hsap osap)))
             (unless (zerop rc)
               (%socket-error (%gai->lookup-code rc)))
             (let ((head (sb-sys:sap-ref-64 osap 0))
                   (addresses '()))
               (unwind-protect
                    (loop for p = head then (sb-sys:sap-ref-64
                                             (sb-sys:int-sap p)
                                             +ai-next-offset+)
                          while (and (integerp p) (plusp p))
                          do (let* ((psap (sb-sys:int-sap p))
                                    (family (sb-sys:sap-ref-32
                                             psap +ai-family-offset+))
                                    (addr (sb-sys:sap-ref-64
                                           psap +ai-addr-offset+)))
                               (when (and addr (plusp addr))
                                 (let* ((asap (sb-sys:int-sap addr))
                                        (value
                                          (case family
                                            (2 (cons "ipv4"
                                                     (loop for i below 4
                                                           collect
                                                           (sb-sys:sap-ref-8
                                                            asap (+ 4 i)))))
                                            (10 (cons "ipv6"
                                                      (loop for g below 8
                                                            collect
                                                            (logior
                                                             (ash (sb-sys:sap-ref-8
                                                                   asap (+ 8 (* 2 g)))
                                                                  8)
                                                             (sb-sys:sap-ref-8
                                                              asap (+ 9 (* 2 g)))))))
                                            (t nil))))
                                   (when (and value (not (member value addresses
                                                                 :test #'equal)))
                                     (push value addresses))))))
                 (when (and (integerp head) (plusp head))
                   (%libc-freeaddrinfo (sb-sys:int-sap head))))
               (nreverse addresses))))
      (sb-alien:free-alien hints)
      (sb-alien:free-alien out))))

;;; ---- resolve-addresses on the DNS EXECUTOR ----
;;; The WIT's `resolve-addresses` is an ASYNC func, and getaddrinfo(3)
;;; BLOCKS: it must not run on the guest's thread. The job is submitted
;;; to the context's DNS service (a FIXED, SEPARATE worker budget) and
;;; its result travels back ONLY through the store queue (the pending's
;;; RESOLVE). A cancel/close DETACHES the job from that target; the
;;; worker still frees the getaddrinfo chain (%GETADDINFO-CHAIN's own
;;; unwind-protect), and the detached result is dropped.

(defstruct (dns-job (:constructor %make-dns-job))
  (mutex (sb-thread:make-mutex :name "dns-job"))
  (target nil))                        ; (RESOLVE . REJECT), NIL once detached

(defun %lookup-result (name flags)
  "The domain result of one getaddrinfo chain: (:ok . addresses) or
(:err . error-code)."
  (handler-case
      (cons :ok (%getaddrinfo-chain name flags))
    (wasi-socket-error (c)
      (cons :err (wasi-socket-error-code c)))))

(defun %dns-job-run (job name flags)
  "The WORKER body: resolve NAME, then hand the result to the pending's
resolve/reject UNLESS the job was detached first. The chain is freed
before the target is consulted, so a cancelled job's late result is
always freeaddrinfo'd."
  (let ((result (%lookup-result name flags)))
    (let ((target nil))
      (sb-thread:with-recursive-lock ((dns-job-mutex job))
        (setf target (dns-job-target job)
              (dns-job-target job) nil))
      (when target
        (if (eq (car result) :ok)
            (funcall (car target) result)
            (funcall (cdr target) result))))))

(defun %dns-resolve-pending (context name flags)
  "NAME goes through getaddrinfo(3) on the DNS service. The guest's
thread only SUBMITS; the worker posts the result to the store queue."
  (let ((executor (wasi-context-dns-executor context)))
    (wasmdown.component.runtime:make-host-pending
     :start
     (lambda (resolve reject acknowledge-cancel)
       (declare (ignore acknowledge-cancel))
       (let ((job (%make-dns-job)))
         (setf (dns-job-target job) (cons resolve reject))
         (unless (%executor-submit executor
                                   (lambda () (%dns-job-run job name flags)))
           ;; no worker can ever answer: detach and report it
           (sb-thread:with-recursive-lock ((dns-job-mutex job))
             (setf (dns-job-target job) nil))
           (funcall reject
                    (make-condition 'wasmdown:host-error
                                    :message
                                    "the DNS executor is closed")))
         ;; the pending's cancel-fn: DETACH. T when the job was
         ;; detached (the store forgets it); NIL once it completed
         ;; (its result delivers normally).
         (lambda ()
           (let ((detached nil))
             (sb-thread:with-recursive-lock ((dns-job-mutex job))
               (when (dns-job-target job)
                 (setf (dns-job-target job) nil
                       detached t)))
             detached)))))))

(defun ip-name-lookup-resolve (context network name
                               &key (package (wasi-package "sockets")))
  "resolve-addresses: an IP literal is parsed as-is (no external
request); a name needs the network's DNS policy and then goes through
getaddrinfo."
  (declare (ignore context package))
  (%socket-result
   (cond
     ((zerop (length name))
      (%socket-error (%socket-code "invalid-argument")))
     ((%parse-ipv4-literal name)
      (list (cons "ipv4" (%parse-ipv4-literal name))))
     ((find #\: name)
      ;; an IPv6 literal: numeric resolution only (AI_NUMERICHOST)
      (%getaddrinfo-chain name +ai-numerichost+))
     (t
      (unless (wasi-network-allows-dns-p network name)
        (%socket-error (%socket-code "access-denied")))
      (%getaddrinfo-chain name 0)))))

(defun ip-name-lookup-resolve-async (context network name
                                     &key (package (wasi-package "sockets")))
  "The PROVIDER's `resolve-addresses`: the same policy checks as
IP-NAME-LOOKUP-RESOLVE, but a real NAME is resolved on the bounded DNS
executor and answered with a HOST-PENDING (the async WIT signature).
A literal, a policy refusal or a numeric IPv6 lookup answer INLINE."
  (declare (ignore package))
  (cond
    ((zerop (length name))
     (cons :err (%socket-code "invalid-argument")))
    ((%parse-ipv4-literal name)
     (cons :ok (list (cons "ipv4" (%parse-ipv4-literal name)))))
    ((find #\: name)
     (%lookup-result name +ai-numerichost+))
    ((not (wasi-network-allows-dns-p network name))
     (cons :err (%socket-code "access-denied")))
    (t
     (%dns-resolve-pending context name 0))))

;;; ---------------------------------------------------------------
;;; The provider instances.

(defun sockets-types-instance (context
                               &key network (package (wasi-package "sockets")))
  "The wasi:sockets/types import: the tcp-socket/udp-socket resources
bound to CONTEXT's network authority (NETWORK overrides the context's
first one). No network authority => every bind/connect is refused."
  (let* ((net (or network (first (wasi-context-networks context))))
         (tcp-ct (tcp-socket-resource-ct :package package))
         (udp-ct (udp-socket-resource-ct :package package))
         (pkg (%sockets-packages :package package))
         (inst
          (provider-instance
           (wasmdown.wit:package-interface package "types")
           (list
            (cons "[static]tcp-socket.create"
                  (lambda (fam) (tcp-create context net fam :package package)))
            (cons "[method]tcp-socket.bind"
                  (lambda (self addr) (tcp-bind context net self addr
                                                :package package)))
            (cons "[method]tcp-socket.connect"
                  (lambda (self addr) (tcp-connect context net self addr
                                                   :package package)))
            (cons "[method]tcp-socket.listen"
                  (lambda (self) (tcp-listen context net self :package package)))
            (cons "[method]tcp-socket.send"
                  (lambda (self data) (tcp-send context net self data
                                                :package package)))
            (cons "[method]tcp-socket.receive"
                  (lambda (self) (tcp-receive context net self :package package)))
            (cons "[method]tcp-socket.get-local-address"
                  (lambda (self) (tcp-get-local-address context net self
                                                        :package package)))
            (cons "[method]tcp-socket.get-remote-address"
                  (lambda (self) (tcp-get-remote-address context net self
                                                         :package package)))
            (cons "[method]tcp-socket.get-is-listening"
                  (lambda (self) (tcp-get-is-listening context net self
                                                       :package package)))
            (cons "[method]tcp-socket.get-address-family"
                  (lambda (self) (tcp-get-address-family context net self
                                                         :package package)))
            (cons "[method]tcp-socket.set-listen-backlog-size"
                  (lambda (self v) (tcp-set-listen-backlog-size context net self v
                                                                :package package)))
            (cons "[method]tcp-socket.get-keep-alive-enabled"
                  (lambda (self) (tcp-get-keep-alive-enabled context net self
                                                             :package package)))
            (cons "[method]tcp-socket.set-keep-alive-enabled"
                  (lambda (self v) (tcp-set-keep-alive-enabled context net self v
                                                               :package package)))
            (cons "[method]tcp-socket.get-keep-alive-idle-time"
                  (lambda (self) (tcp-get-keep-alive-idle-time context net self
                                                               :package package)))
            (cons "[method]tcp-socket.set-keep-alive-idle-time"
                  (lambda (self v) (tcp-set-keep-alive-idle-time context net self v
                                                                 :package package)))
            (cons "[method]tcp-socket.get-keep-alive-interval"
                  (lambda (self) (tcp-get-keep-alive-interval context net self
                                                              :package package)))
            (cons "[method]tcp-socket.set-keep-alive-interval"
                  (lambda (self v) (tcp-set-keep-alive-interval context net self v
                                                                :package package)))
            (cons "[method]tcp-socket.get-keep-alive-count"
                  (lambda (self) (tcp-get-keep-alive-count context net self
                                                           :package package)))
            (cons "[method]tcp-socket.set-keep-alive-count"
                  (lambda (self v) (tcp-set-keep-alive-count context net self v
                                                             :package package)))
            (cons "[method]tcp-socket.get-hop-limit"
                  (lambda (self) (tcp-get-hop-limit context net self
                                                    :package package)))
            (cons "[method]tcp-socket.set-hop-limit"
                  (lambda (self v) (tcp-set-hop-limit context net self v
                                                     :package package)))
            (cons "[method]tcp-socket.get-receive-buffer-size"
                  (lambda (self) (tcp-get-receive-buffer-size context net self
                                                              :package package)))
            (cons "[method]tcp-socket.set-receive-buffer-size"
                  (lambda (self v) (tcp-set-receive-buffer-size context net self v
                                                                :package package)))
            (cons "[method]tcp-socket.get-send-buffer-size"
                  (lambda (self) (tcp-get-send-buffer-size context net self
                                                           :package package)))
            (cons "[method]tcp-socket.set-send-buffer-size"
                  (lambda (self v) (tcp-set-send-buffer-size context net self v
                                                             :package package)))
            ;; udp-socket
            (cons "[static]udp-socket.create"
                  (lambda (fam) (udp-create context net fam :package package)))
            (cons "[method]udp-socket.bind"
                  (lambda (self addr) (udp-bind context net self addr
                                                :package package)))
            (cons "[method]udp-socket.connect"
                  (lambda (self addr) (udp-connect context net self addr
                                                   :package package)))
            (cons "[method]udp-socket.disconnect"
                  (lambda (self) (udp-disconnect context net self
                                                 :package package)))
            (cons "[method]udp-socket.send"
                  (lambda (self data remote) (udp-send context net self data remote
                                                       :package package)))
            (cons "[method]udp-socket.receive"
                  (lambda (self) (udp-receive context net self :package package)))
            (cons "[method]udp-socket.get-local-address"
                  (lambda (self) (udp-get-local-address context net self
                                                        :package package)))
            (cons "[method]udp-socket.get-remote-address"
                  (lambda (self) (udp-get-remote-address context net self
                                                         :package package)))
            (cons "[method]udp-socket.get-address-family"
                  (lambda (self) (udp-get-address-family context net self
                                                         :package package)))
            (cons "[method]udp-socket.get-unicast-hop-limit"
                  (lambda (self) (udp-get-unicast-hop-limit context net self
                                                            :package package)))
            (cons "[method]udp-socket.set-unicast-hop-limit"
                  (lambda (self v) (udp-set-unicast-hop-limit context net self v
                                                              :package package)))
            (cons "[method]udp-socket.get-receive-buffer-size"
                  (lambda (self) (udp-get-receive-buffer-size context net self
                                                              :package package)))
            (cons "[method]udp-socket.set-receive-buffer-size"
                  (lambda (self v) (udp-set-receive-buffer-size context net self v
                                                                :package package)))
            (cons "[method]udp-socket.get-send-buffer-size"
                  (lambda (self) (udp-get-send-buffer-size context net self
                                                           :package package)))
            (cons "[method]udp-socket.set-send-buffer-size"
                  (lambda (self v) (udp-set-send-buffer-size context net self v
                                                             :package package))))
           :packages pkg)))
    ;; M17: the socket resources' release hooks belong to THIS
    ;; provider instance (the one whose types produced the cts), not
    ;; to a ct-keyed global: two sockets providers never share a
    ;; finalizer.
    (wasmdown.component.runtime:host-register-resource-dtor
     inst tcp-ct #'close-wasi-tcp-socket)
    (wasmdown.component.runtime:host-register-resource-dtor
     inst udp-ct #'close-wasi-udp-socket)
    inst))

(defun ip-name-lookup-instance (context
                                &key network
                                     (package (wasi-package "sockets")))
  "The wasi:sockets/ip-name-lookup import, consulting CONTEXT's (or
NETWORK's) DNS policy."
  (let ((net (or network (first (wasi-context-networks context)))))
    (provider-instance
     (wasmdown.wit:package-interface package "ip-name-lookup")
     (list (cons "resolve-addresses"
                 (lambda (name)
                   (ip-name-lookup-resolve-async context net name
                                                 :package package))))
     :packages (%sockets-packages :package package))))

(defun sockets-provider (context
                         &key network (package (wasi-package "sockets")))
  "The wasi:sockets imports (types + ip-name-lookup) as a with-args
alist, all bound to one network authority."
  (let ((net (or network (first (wasi-context-networks context)))))
    (list
     (cons (wasmdown.wit:interface-import-name
            (wasmdown.wit:package-interface package "types"))
           (sockets-types-instance context :network net :package package))
     (cons (wasmdown.wit:interface-import-name
            (wasmdown.wit:package-interface package "ip-name-lookup"))
           (ip-name-lookup-instance context :network net :package package)))))

;;; ---------------------------------------------------------------
;;; `wasi:http` was REMOVED at user direction in M17: this runtime
;;; does not support WASI HTTP. The M16 slice 5 implementation (the
;;; `types` resources, `client.send`, the pluggable transports and the
;;; plain HTTP/1.1 client) is recoverable from git history; nothing
;;; here is stubbed, disabled or shimmed. The vendored WIT under
;;; `specs/wasi/wasi-wit-0.3.2/http/*.wit` remains reference data
;;; only — it still parses and validates, but no provider is built
;;; from it and no `wasi:http` name is exported by this package.

;;; ===============================================================
;;; THE COMMAND WORLD (`wasi:cli/run`, M16 slice 6) — THE RUNNER.
;;;
;;; A component built from the `command' world imports the whole WASI
;;; surface as instance imports and exports the `run' function of
;;; `wasi:cli/run@0.3.2` (`run: async func() -> result`). The runner
;;; is the host side of that contract:
;;;
;;;   1. instantiate the validated component with EVERY provider bound
;;;      under its versioned WIT import name;
;;;   2. call the exported `run' through `lifted-func-call' — the
;;;      ASYNC-TYPED functype routes through the task state machine
;;;      (its own guest thread + the caller's drain loop), a sync-typed
;;;      `run' takes the sync path; both RETURN the component-level
;;;      `result' domain value;
;;;   3. map it to the process status: `ok' -> 0, `err' -> 1;
;;;   4. `wasi:cli/exit's functions signal WASI-EXIT from the guest
;;;      thread; the condition unwinds through the lift (the async
;;;      trap path re-signals it at the boundary), is caught HERE, and
;;;      its status becomes the process status;
;;;   5. the store is torn down on every exit path (STORE-TEARDOWN):
;;;      an exit — or a trap — leaves no parked guest thread behind.
;;;
;;; The provider set is capability-driven: every provider consults the
;;; explicit WASI-CONTEXT (PLAN.md §9.1), never an ambient global — and
;;; (M17, plan step 3) the SET is the resolved world's import set, not
;;; a hand-written whole-surface list.

;;; ---------------------------------------------------------------
;;; PROVIDER COMPOSITION FROM THE RESOLVED WORLD.
;;;
;;; `COMMAND-PROVIDER' walks the imports of the world that
;;; `WASMDOWN.WIT:RESOLVE-WIT-WORLD' resolved (gates applied, includes
;;; expanded, references resolved) and builds exactly those providers,
;;; under exactly the import names the world declares:
;;;
;;;   * a declared import this runtime serves gets its provider
;;;     instance (the builders close over CONTEXT's authorities —
;;;     streams, preopens, network, reactor — and are invoked LAZILY,
;;;     so an import the world does not declare builds nothing);
;;;   * a types-only import (no functions, no resources) for which the
;;;     runtime has NO entry — `wasi:cli/types@0.3.2' — needs no host
;;;     instance: a guest resolves its types structurally through
;;;     `use', so it composes no entry and is reported as unbound
;;;     rather than invented (`clocks'/`filesystem'/`sockets' `types'
;;;     ARE in the surface, and compose their (empty-of-functions)
;;;     instance exactly as before);
;;;   * any OTHER declared import is a CONSTRUCTION-TIME `WIT-ERROR'
;;;     naming the interface. A trapping stub (PROVIDER-INSTANCE's
;;;     unimplemented-function closure) is never installed in place of
;;;     a provider the runtime does not have.
;;;
;;; `wasi:http' is not supported by this runtime: the vendored `http'
;;; package takes part in resolution (reference data only) so such an
;;; import resolves to a named interface and is refused HERE, with its
;;; import name, instead of failing to resolve.

(defun %wasi-wit-packages ()
  "The packages provider composition resolves against: the five WASI
0.3.2 packages this runtime serves, plus the vendored `http' package
as REFERENCE DATA (it parses/validates; no provider is built from
it), so a world importing `wasi:http' names a real interface and is
then refused by the provider check."
  (mapcar #'wasi-package
          '("clocks" "random" "cli" "filesystem" "sockets" "http")))

(defun %world-import-name (item)
  "The component-model import name of the world import ITEM: its
plain binding name when it has one (`import foo: path'), else the
interface's versioned import name (`ns:pkg/iface@version')."
  (or (wasmdown.wit:wit-world-item-name item)
      (let ((target (wasmdown.wit:wit-world-item-target item)))
        (if (wasmdown.wit:wit-interface-p target)
            (wasmdown.wit:interface-import-name target)
            (if (wasmdown.wit:wit-world-item-package item)
                (format nil "~a:~a/~a~@[@~a~]"
                        (or (wasmdown.wit:wit-world-item-namespace item) "")
                        (wasmdown.wit:wit-world-item-package item)
                        (wasmdown.wit:wit-world-item-interface item)
                        (wasmdown.wit:wit-world-item-version item))
                (wasmdown.wit:wit-world-item-interface item))))))

(defun %types-only-world-import-p (item)
  "T when ITEM's resolved target is an interface with no functions
and no resources: nothing for a provider instance to bind. Only a
RESOLVED target qualifies — an unresolved reference is never assumed
types-only, so it fails the provider check instead of vanishing."
  (let ((target (wasmdown.wit:wit-world-item-target item)))
    (and (wasmdown.wit:wit-interface-p target)
         (null (wasmdown.wit:wit-interface-func-list target))
         (null (wasmdown.wit:wit-interface-resource-list target)))))

(defun %wasi-provider-builders (context)
  "An alist import-name -> thunk for every WASI interface this
runtime serves, each thunk building that interface's provider
instance for CONTEXT. This is the RUNTIME's capability surface; WHICH
of these entries a component gets is decided by the world."
  (let* ((cli (wasi-package "cli"))
         (clocks (wasi-package "clocks"))
         (filesystem (wasi-package "filesystem"))
         (random (wasi-package "random"))
         (sockets (wasi-package "sockets"))
         (reactor (wasi-context-reactor context))
         (network (first (wasi-context-networks context)))
         (builders '()))
    (flet ((bind (package interface thunk)
             (push (cons (wasmdown.wit:interface-import-name
                          (wasmdown.wit:package-interface package interface))
                         thunk)
                   builders))
           (empty (package interface)
             ;; a types-only interface's provider: the type exports
             ;; alone, exactly as clocks/filesystem/sockets `types'
             ;; have always been built
             (let ((node (wasmdown.wit:package-interface package interface)))
               (lambda ()
                 (provider-instance node nil :packages (list package))))))
      ;; wasi:clocks
      (bind clocks "types" (empty clocks "types"))
      (bind clocks "monotonic-clock"
            (lambda ()
              (monotonic-clock-instance :package clocks :reactor reactor)))
      (bind clocks "system-clock"
            (lambda () (system-clock-instance :package clocks)))
      ;; wasi:random
      (bind random "random"
            (lambda () (random-instance :package random)))
      (bind random "insecure"
            (lambda () (insecure-random-instance :package random)))
      (bind random "insecure-seed"
            (lambda () (insecure-seed-instance :package random)))
      ;; wasi:cli
      (bind cli "environment"
            (lambda () (cli-environment-instance context :package cli)))
      (bind cli "exit"
            (lambda () (cli-exit-instance :package cli)))
      (bind cli "stdin"
            (lambda () (cli-stdin-instance context :package cli)))
      (bind cli "stdout"
            (lambda () (cli-stdout-instance context :package cli)))
      (bind cli "stderr"
            (lambda () (cli-stderr-instance context :package cli)))
      (bind cli "terminal-input"
            (lambda () (terminal-input-instance :package cli)))
      (bind cli "terminal-output"
            (lambda () (terminal-output-instance :package cli)))
      (bind cli "terminal-stdin"
            (lambda () (terminal-stdin-instance context :package cli)))
      (bind cli "terminal-stdout"
            (lambda () (terminal-stdout-instance context :package cli)))
      (bind cli "terminal-stderr"
            (lambda () (terminal-stderr-instance context :package cli)))
      ;; wasi:filesystem
      (bind filesystem "types"
            (lambda () (filesystem-types-instance context :package filesystem)))
      (bind filesystem "preopens"
            (lambda ()
              (filesystem-preopens-instance context :package filesystem)))
      ;; wasi:sockets
      (bind sockets "types"
            (lambda ()
              (sockets-types-instance context :network network
                                      :package sockets)))
      (bind sockets "ip-name-lookup"
            (lambda ()
              (ip-name-lookup-instance context :network network
                                       :package sockets)))
      (nreverse builders))))

(defun compose-world-providers (world context)
  "Compose the provider graph for the RESOLVED WORLD: exactly the
imports WORLD declares, in declaration order, each bound under the
world's own import name. Returns (values ALIST UNBOUND), where ALIST
is the `:with-args' alist and UNBOUND lists the declared types-only
imports the runtime has no entry for (they need no host binding).
Raises `WIT-ERROR' naming the interface for any other declared
import this runtime has no provider for."
  (let ((builders (%wasi-provider-builders context))
        (providers '())
        (unbound '())
        (seen (make-hash-table :test #'equal)))
    (dolist (item (wasmdown.wit:wit-world-imports world))
      (let ((name (%world-import-name item)))
        (unless (gethash name seen)
          (setf (gethash name seen) t)
          (let ((builder (cdr (assoc name builders :test #'string=))))
            (cond
              (builder
               (push (cons name (funcall builder)) providers))
              ((%types-only-world-import-p item)
               (push name unbound))
              (t
               (error 'wasmdown.wit:wit-error
                      :message
                      (format nil "world `~a` declares import `~a`, for which this runtime has no provider"
                              (wasmdown.wit:wit-world-name world) name))))))))
    (values (nreverse providers) (nreverse unbound))))

(defun command-world (&key (packages (%wasi-wit-packages)) selection)
  "The RESOLVED `command' world of the vendored `wasi:cli' package
under SELECTION (NIL: the default selection), with the package set
PACKAGES supplying its cross-package imports."
  (wasmdown.wit:resolve-wit-world (wasi-package "cli") "command"
                                  :packages packages
                                  :selection selection))

(defun command-provider (context &key (world (command-world)))
  "The provider set of the `command' world as a with-args alist,
bound to CONTEXT and composed from the resolved WORLD (default: the
vendored `wasi:cli/command'). Every import the world declares gets
its provider; a types-only import the runtime has no entry for
composes no entry; an unsupported one is a construction-time
`WIT-ERROR'. `wasi:http' is not supported by this runtime and is
never bound."
  (compose-world-providers world context))

(defun component-run-export (instance)
  "The `run' function a command-world component exports: the exported
`wasi:cli/run@0.3.2' instance's `run' (a comp-instance or an inline
bag), else a direct top-level `run' export. NIL when absent."
  (flet ((%export (name)
           (let ((hit (gethash name
                               (wasmdown.component.runtime:comp-instance-exports
                                instance)
                               :missing)))
             (unless (eq hit :missing) (cdr hit))))
         (run-of (value)
           (typecase value
             (wasmdown.component.runtime:comp-instance
              (wasmdown.component.runtime:runtime-export-value value "run"))
             (hash-table (gethash "run" value))
             (t nil))))
    (or (run-of (%export "wasi:cli/run@0.3.2"))
        (let ((direct (%export "run")))
          (and (wasmdown.component.runtime:lifted-func-p direct) direct)))))

(defun %run-result-status (result)
  "The `result' domain value of the command's run -> the process
status: ok -> 0, err (or an absent/false result) -> 1."
  (if (and (consp result) (eq (car result) :ok)) 0 1))

(defun run-component (comp context &key providers)
  "Drive the VALIDATED command component COMP to completion under the
capabilities of CONTEXT. Instantiates with PROVIDERS (default: the
whole COMMAND-PROVIDER set for CONTEXT), calls the exported
`wasi:cli/run@0.3.2' `run' (async or sync), and returns the process
status. A `wasi:cli/exit' terminates the run with its status; the
store is torn down in every case (no leaked guest threads). Traps
propagate to the caller."
  (let ((providers (or providers (command-provider context))))
    (unwind-protect
         (handler-case
             (let* ((instance
                      (wasmdown.component.runtime:instantiate-component
                       comp :with-args providers))
                    (run (component-run-export instance)))
               ;; THE STORE'S REACTOR (step 5/6 driver): registering the context's
               ;; reactor on the instance's store is what lets a would-blocked copy
               ;; or a clock deadline be woken from the store's own tick. Without
               ;; it the store falls back to the single global FIFO and only
               ;; direct/fake-reactor paths can be served.
               (when (wasi-context-reactor context)
                 (setf (wasmdown.component.runtime::%inst-reactor instance)
                       (wasi-context-reactor context)))
               (unless run
                 (error 'wasmdown:link-error
                        :message
                        "component exports no wasi:cli/run@0.3.2 `run' function"))
               (%run-result-status
                (wasmdown.component.runtime:lifted-func-call run)))
           (wasi-exit (c) (wasi-exit-status c)))
      ;; the exit semantics: whatever the guest started, the command
      ;; is over — abandon + unblock + join every registered thread
      (wasmdown.component.runtime:store-teardown))))

;;; ---------------------------------------------------------------
;;; The CLI's network-authority parsing (`--net HOST:PORT').

(defun %parse-ipv4-octets (text)
  "TEXT as a dotted-quad IPv4 address (4 octets), or NIL."
  (let ((parts '()) (start 0))
    (loop
      (let ((dot (position #\. text :start start)))
        (let ((piece (subseq text start dot)))
          (unless (and (plusp (length piece)) (every #'digit-char-p piece))
            (return-from %parse-ipv4-octets nil))
          (let ((n (parse-integer piece)))
            (unless (<= 0 n 255) (return-from %parse-ipv4-octets nil))
            (push n parts)))
        (if dot (setf start (1+ dot)) (return))))
    (when (= (length parts) 4) (nreverse parts))))

(defun parse-wasi-endpoint (spec)
  "Parse \"HOST:PORT\" into a WASI-ENDPOINT. HOST is `*' (any address)
or a dotted-quad IPv4 address; PORT is `*' (any port) or 0..65535.
The WIT's address slot is an ip-socket-address, so host names are not
endpoints (they belong to the DNS policy, `--dns'). Signals
HOST-ERROR on a malformed spec."
  (let ((colon (position #\: spec :from-end t)))
    (unless colon
      (error 'wasmdown:host-error
             :message (format nil "endpoint ~s is not HOST:PORT" spec)))
    (let* ((host (subseq spec 0 colon))
           (port-text (subseq spec (1+ colon)))
           (address (cond ((string= host "*") :any)
                          ((%parse-ipv4-octets host))
                          (t (error 'wasmdown:host-error
                                    :message
                                    (format nil "endpoint host ~s is not `*' or an IPv4 address"
                                            host)))))
           (port (cond ((string= port-text "*") :any)
                       ((every #'digit-char-p port-text)
                        (let ((n (parse-integer port-text)))
                          (unless (<= 0 n 65535)
                            (error 'wasmdown:host-error
                                   :message
                                   (format nil "endpoint port ~d out of range" n)))
                          n))
                       (t (error 'wasmdown:host-error
                                 :message
                                 (format nil "endpoint port ~s is not `*' or an integer"
                                         port-text))))))
      (make-wasi-endpoint :address address :port port))))
