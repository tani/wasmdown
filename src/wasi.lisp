;;;; WASMDOWN — the WASI host-provider framework (docs/design/PLAN.md §9, M16):
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
;;;;   * capabilities are an EXPLICIT wasi-context (docs/design/PLAN.md §9.1);
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
(docs/design/PLAN.md §9.1). ARGUMENTS is a list of strings, ENVIRONMENT an alist
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
  ;; M16 slice 5 (restored): the embedder-supplied HTTP transports
  ;; (docs/design/PLAN.md §9.2: "pluggable system TLS providers or
  ;; embedder-supplied transport services"). A wasi-http-transport
  ;; whose SCHEME matches a request's scheme is used instead of the
  ;; built-in plain TCP transport; no match for an https request is
  ;; the WIT's `configuration-error', never a silent plaintext
  ;; fallback.
  (http-transports nil)
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
  (stdio-exec nil)
  ;; the lifecycle flag: a closed context rejects NEW work and owns
  ;; nothing any more (close-wasi-context is idempotent)
  (closed-p nil))

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

(defun %context-work-gate (context what)
  "Reject new host work for a CLOSED context: a context that was
released must never start a resolver/stdio job whose completion no
store will drain."
  (when (and context (wasi-context-closed-p context))
    (error 'wasmdown:host-error
           :message (format nil "wasi context is closed: refusing ~a" what))))

(defun wasi-context-dns-executor (context)
  "CONTEXT's DNS worker service (fixed *DNS-WORKER-COUNT* budget),
created on first use. A NIL context uses the process default.
A CLOSED context refuses new work."
  (%context-work-gate context "a DNS lookup")
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
*STDIO-WORKER-COUNT* budget), created on first use. A CLOSED context
refuses new work."
  (%context-work-gate context "a blocking stdio job")
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

(defun close-wasi-context (context)
  "Release CONTEXT: reject new host work, detach its owned executor
services (in-flight jobs finish into discarded results, never joined)
and mark the context closed. Borrowed authorities (preopens, stdio
streams, networks, reactor, a caller-owned store) are NOT closed —
the embedder that lent them owns them. Idempotent."
  (when context
    (sb-thread:with-recursive-lock (*executor-service-lock*)
      (setf (wasi-context-closed-p context) t))
    (close-wasi-context-executors context))
  context)

(defmacro call-with-wasi-context ((context &key (close-p t)) &body body)
  "Run BODY with CONTEXT established. When CLOSE-P (the default) the
context is released on exit — a CLI-style invocation owns its
context, while an embedder lending one closes it itself."
  `(unwind-protect (progn ,@body)
     ,(when close-p `(close-wasi-context ,context))))

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

(defparameter *wasi-spec-root*
  (merge-pathnames #p"../specs/wasi/" (%source-file))
  "The vendored specification root: every supported release plus the
generated index (specs/wasi/index.txt).")

(defparameter *wasi-supported-versions*
  '("0.3.2" "0.2.12" "0.2.11" "0.2.10" "0.2.9" "0.2.8" "0.2.7"
    "0.2.6" "0.2.5" "0.2.4" "0.2.3" "0.2.2" "0.2.1" "0.2.0")
  "Every WIT release whose sources are vendored, newest first.")

(defvar *wasi-package-index* nil
  "version -> package short name -> list of pathnames, built once from
specs/wasi/index.txt (never from directory-name guessing).")

(defun %wasi-split-fields (line)
  "Split an index line on tabs."
  (let ((fields '()) (start 0))
    (loop for i from 0 to (length line)
          when (or (= i (length line)) (char= (char line i) #\Tab))
            do (push (subseq line start i) fields)
               (setf start (1+ i)))
    (nreverse fields)))

(defun wasi-package-index ()
  "version -> package -> sorted list of vendored source pathnames."
  (or *wasi-package-index*
      (let ((table (make-hash-table :test #'equal))
            (file (merge-pathnames "index.txt" *wasi-spec-root*)))
        (with-open-file (in file :direction :input
                                 :if-does-not-exist :error)
          (loop for line = (read-line in nil nil)
                while line
                unless (or (zerop (length line)) (char= (char line 0) #\#))
                  do (let* ((fields (%wasi-split-fields line))
                            (version (first fields))
                            (package (second fields))
                            (rel (third fields)))
                       (when (and version package rel)
                         (let ((pkgs (or (gethash version table)
                                         (setf (gethash version table)
                                               (make-hash-table :test #'equal)))))
                           (push (merge-pathnames rel *wasi-spec-root*)
                                 (gethash package pkgs)))))))
        (maphash (lambda (version pkgs)
                   (declare (ignore version))
                   (maphash (lambda (package paths)
                              (setf (gethash package pkgs)
                                    (sort paths #'string< :key #'namestring)))
                            pkgs))
                 table)
        (setf *wasi-package-index* table))))

(defun %wasi-version-ok-p (version)
  (member version *wasi-supported-versions* :test #'string=))

(defun %wasi-version-check (short-name version)
  (unless (%wasi-version-ok-p version)
    (error 'wasmdown:link-error
           :message (format nil "wasi:~a@~a is not a supported release (supported: ~{~a~^, ~})"
                            short-name version *wasi-supported-versions*))))

(defun wasi-package-files (short-name &key (version "0.3.2"))
  "Every vendored WIT source file of SHORT-NAME at VERSION, in lexical
order. Unknown versions are a link error naming the exact request."
  (%wasi-version-check short-name version)
  (let* ((pkgs (gethash version (wasi-package-index)))
         (paths (and pkgs (gethash short-name pkgs))))
    (unless paths
      (error 'wasmdown:link-error
             :message (format nil "no vendored sources for wasi:~a@~a"
                              short-name version)))
    ;; a release ships its dependencies as copies under <pkg>/deps/;
    ;; those belong to OTHER packages (their headers say so) and are
    ;; loaded through their own index entry, never as this package's
    ;; declarations.
    (remove-if (lambda (path)
                 (search "/deps/" (namestring path) :test #'char=))
               paths)))

(defun wasi-package-file (short-name &key (version "0.3.2"))
  "The PRIMARY source file of the package: the header-carrying unit the
single-file API has always returned (0.3.2: <short>/<short>.wit, the
manifest's entry). Multi-file loading uses WASI-PACKAGE-FILES."
  (%wasi-version-check short-name version)
  (if (string= version "0.3.2")
      (let ((path (merge-pathnames (make-pathname :directory (list :relative short-name)
                                                  :name short-name :type "wit")
                                   *wasi-wit-root*)))
        (unless (probe-file path)
          (error 'wasmdown:link-error
                 :message (format nil "no vendored sources for wasi:~a@~a"
                                  short-name version)))
        path)
      (or (first (wasi-package-files short-name :version version))
          (error 'wasmdown:link-error
                 :message (format nil "no vendored sources for wasi:~a@~a"
                                  short-name version)))))

(defvar *wasi-package-cache* (make-hash-table :test #'equal))

(defun wasi-package (short-name &key (version "0.3.2"))
  "Parse (and cache) the vendored WIT package SHORT-NAME at VERSION.
Multi-file packages are loaded through PARSE-WIT-DIRECTORY so the
released layout (headerless continuation files) is honoured."
  (let ((key (list version short-name)))
    (or (gethash key *wasi-package-cache*)
        (setf (gethash key *wasi-package-cache*)
              (if (string= version "0.3.2")
                  (wasmdown.wit:parse-wit-file
                   (wasi-package-file short-name :version version))
                  (wasmdown.wit:parse-wit-files
                   (wasi-package-files short-name :version version)
                   :origin (format nil "wasi:~a@~a" short-name version)))))))

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
;;; 125 declared, 125 implemented, 0 stubs.
;;;
;;;   clocks/monotonic-clock   4    random/random           2
;;;   clocks/system-clock      2    random/insecure         2
;;;   cli/environment          3    random/insecure-seed    1
;;;   cli/exit                 2    filesystem/types       25
;;;   cli/stdin|stdout|stderr  1+1+1    filesystem/preopens 1
;;;   cli/terminal-*          1+1+1    sockets/types         40
;;;                                   sockets/ip-name-lookup 1
;;;   http/types              35    http/client             1
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
;;;   * wasi:http/handler (1) — the INCOMING service/middleware
;;;     direction is a follow-up; the outgoing `types' + `client'
;;;     surface IS served (M16 slice 5, restored);
;;;   * wasi:cli/run — the command world's EXPORT (served by
;;;     COMPONENT-RUN-EXPORT), never an import to bind.
;;;
;;; PROVIDER-INSTANCE enforces the 125/125: an alist that does not cover
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

(defun provider-instance (interface implementations &key packages (include nil))
  "Build a comp-instance for the parsed WIT INTERFACE. IMPLEMENTATIONS
is an alist ((\"now\" . fn) ...) of CL functions taking the component
domain arguments. Every function and resource method the INTERFACE
declares MUST be supplied — an absent entry is a construction-time
HOST-ERROR naming the interface and the function; it is never
exported as a trapping stub. INCLUDE, when non-NIL, is a predicate on
the WIT node (func/resource/type): a node it rejects is NOT exported
(the WASI 0.2 facade uses it to keep `@unstable` members out of a
stable release's surface). The instance's export table also carries
the interface's TYPE definitions, so a consumer can alias them with
the runtime's type-projection path.

M16 slice 3: RESOURCE methods are exported under their component
core names ([method]R.m / [static]R.m / [constructor]R), can be
implemented either by that name or by the bare method name, and the
resource type is exported as (:type . ct) — the ct every occurrence
of the resource shares (the handle table's type identity)."
  (let ((inst (wasmdown.component.runtime:make-comp-instance))
        (wanted (lambda (node) (or (null include) (funcall include node)))))
    (dolist (fn (wasmdown.wit:wit-interface-func-list interface))
      (when (funcall wanted fn)
        (let* ((fname (wasmdown.wit:wit-func-name fn))
               (ct (wasmdown.wit:wit-func->ct interface fn
                                              :packages packages))
               (closure (%provider-closure interface implementations fname)))
          (setf (gethash fname
                         (wasmdown.component.runtime:comp-instance-exports inst))
                (cons :func (make-host-lifted-func ct closure :inst inst))))))
    ;; M16 slice 3: the interface's RESOURCES — the type export first
    ;; (the shared ct), then each method as a func export.
    (dolist (res (wasmdown.wit:wit-interface-resource-list interface))
      (let* ((rname (wasmdown.wit:wit-resource-name res))
             (rct (wasmdown.wit:wit-type->ct
                   interface (list :resource rname) :packages packages)))
        (when (funcall wanted res)
          (setf (gethash rname
                         (wasmdown.component.runtime:comp-instance-exports inst))
                (cons :type rct)))
        ;; The IMPLICIT resource destructor is part of an imported
        ;; interface's surface: a component that imports an interface
        ;; instance imports `[resource-drop]R` as a FUNCTION (the
        ;; adapter does for every resource it forwards). Export it for
        ;; every resource, whatever @unstable filtering says about the
        ;; explicit methods: dropping a handle is not a feature gate.
        (let* ((i32 (wasmdown.component:make-ct :kind :prim :payload :i32))
               (drop-ft (wasmdown.component:make-ct
                         :kind :func
                         :payload (list (vector (cons "rep" i32)) nil nil)))
               (drop-name (format nil "[resource-drop]~a" rname))
               (rct-for-drop rct))
          (setf (gethash drop-name
                         (wasmdown.component.runtime:comp-instance-exports inst))
                (cons :func
                      (make-host-lifted-func
                       drop-ft
                       (lambda (args)
                         (let ((rep (and (consp args)
                                         (wasmdown.component.runtime:host-resource-rep
                                          rct-for-drop (car args)))))
                           (let ((finalizer
                                   (wasmdown.component.runtime:host-resource-finalizer
                                    inst rct-for-drop)))
                             (when (and finalizer rep)
                               (ignore-errors (funcall finalizer rep)))))
                         '())
                       :inst inst))))
        (dolist (method (wasmdown.wit:wit-resource-methods res))
          (when (funcall wanted method)
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
                                                       :inst inst))))))))
    ;; The interface's defined types (not resources: those are above)
    ;; are exported as (:type . ct).
    (dolist (item (wasmdown.wit:wit-interface-items interface))
      (when (wasmdown.wit:wit-type-p item)
        (let ((expr (wasmdown.wit:wit-type-expr item)))
          (unless (and (consp expr) (eq (car expr) :resource))
            (when (funcall wanted item)
              (let ((ct (wasmdown.wit:wit-type->ct interface expr
                                                   :packages packages)))
                (when ct
                  (setf (gethash (wasmdown.wit:wit-type-name item)
                                 (wasmdown.component.runtime:comp-instance-exports
                                  inst))
                        (cons :type ct)))))))))
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
  "The insecure-seed provider: ONE 128-bit value per provider instance,
returned unchanged on every call. The WIT says the function is
intended to be called once and will become a value import so that
callers cannot get different seeds; the reference runtime answers a
second call with the first value, and the upstream wasip3 random test
asserts it. The value is still host entropy, drawn once."
  (let ((seed (insecure-seed)))
    (provider-instance
     (wasmdown.wit:package-interface package "insecure-seed")
     (list (cons "get-insecure-seed" (lambda () (copy-list seed))))
     :packages (list package))))

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
;;; over the CONTEXT's CL streams (docs/design/PLAN.md §9.1: an explicit authority
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
;;; handle (docs/design/PLAN.md §9.1: the capability is explicit).

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
;;; docs/design/PLAN.md §9.1: a file root is an OPEN HOST DIRECTORY HANDLE, never
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
;;; The libc `*at' bindings (docs/design/PLAN.md §9.2: thin, audited SB-ALIEN).

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
  "A WASI file root: an OPEN HOST DIRECTORY HANDLE (docs/design/PLAN.md §9.1) and
the guest-visible path string the guest knows it by. Every operation
on it is handle-relative; there is no path-prefix check to bypass.
PERMISSIONS is the EXPLICIT authority the embedder granted (\"read\",
\"write\", \"mutate-directory\") — the Preview1/P2 rights are derived
from it, never minted from the host handle's own mode bits."
  (fd -1)
  (name "")
  (permissions '("read" "write" "mutate-directory"))
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

(defun open-wasi-preopen (host-path &key name
                                      (permissions '("read" "write" "mutate-directory")))
  "Establish a file root: open HOST-PATH once as a directory handle.
NAME is the guest-visible preopen name (defaults to the host path).
PERMISSIONS is the explicit authority the guest may exercise through
this root; the default preserves the previous behavior (a read/write
root). A read-only root denies creation, writing and mutation, and
its derived ABI rights can never be widened by a guest.
The caller owns the preopen; close-wasi-preopen releases the fd."
  (dolist (p permissions)
    (unless (member p '("read" "write" "mutate-directory") :test #'string=)
      (error 'wasmdown:host-error
             :message (format nil "unknown preopen permission ~s" p))))
  (let* ((fd (%open-host-directory host-path))
         (pre (make-wasi-preopen :fd fd
                                 :name (or name (namestring host-path))
                                 :permissions permissions))
         (desc (make-wasi-descriptor
                :fd fd
                :preopen pre
                :flags (if (member "write" permissions :test #'string=)
                           '("read" "write" "mutate-directory")
                           '("read"))
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
WIT's capability errors: empty -> no-entry, absolute ->
not-permitted, NUL -> invalid, `..' -> not-permitted (the conservative
rule: the guest may never climb, so no symbolic-link or `..' step can
leave the preopen). An empty path is what POSIX resolves to ENOENT
(`openat', `mkdirat', `unlinkat', `linkat' all return ENOENT), so the
WIT's `no-entry' case is the honest answer; the upstream wasip3
filesystem tests assert exactly that."
  (unless (stringp path)
    (%fs-error "invalid"))
  (when (zerop (length path))
    (%fs-error "no-entry"))
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
  "FD's (values dev ino mode nlink size mtime atime ctime). SBCL's
unix-fstat returns (t dev ino mode nlink uid gid rdev size atime mtime
ctime blksize blocks) on success and (nil errno) on failure — the
second slot is DEV, not an errno (a shift here made every type read as
`other'). The THREE timestamps are distinct slots of the stat struct
and are reported as such: a stat that echoed the mtime for all three
cannot answer the WIT's access/modification/change timestamps (the
upstream filesystem-stat test sets them to different values and reads
them back)."
  (multiple-value-bind (ok dev ino mode nlink uid gid rdev
                        size atime mtime ctime blksize blocks)
      (sb-unix:unix-fstat fd)
    (declare (ignore uid gid rdev blksize blocks))
    (unless ok
      ;; on failure the second value is the errno
      (%fs-error (errno->error-code dev)))
    (values dev ino mode nlink size mtime atime ctime)))

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

(defun %stat-record (dev ino mode nlink size mtime atime ctime)
  "The WIT descriptor-stat domain value for the fstat tuple. The three
`instant' fields are the struct's own atime, mtime and ctime slots
(SBCL reports whole seconds; the nanoseconds slot is 0)."
  (declare (ignore dev ino))
  (flet ((instant (seconds) (cons :some (%instant seconds 0))))
    (list (cons "%type" (%mode->type mode))
          (cons "link-count" nlink)
          (cons "size" size)
          (cons "data-access-timestamp" (instant atime))
          (cons "data-modification-timestamp" (instant mtime))
          (cons "status-change-timestamp" (instant ctime)))))

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
     (multiple-value-bind (dev ino mode nlink size mtime atime ctime)
         (%fstat (%descriptor-fd desc))
       (%stat-record dev ino mode nlink size mtime atime ctime)))))

(defun fs-stat-at (context self path-flags path)
  (%fs-result
   (%descriptor-of context self (descriptor-resource-ct))
   (multiple-value-bind (dev ino mode nlink size mtime atime ctime)
       (%stat-at (%descriptor-of context self (descriptor-resource-ct))
                 path path-flags)
     (%stat-record dev ino mode nlink size mtime atime ctime))))

(defun fs-get-type (context self)
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     (%mode->type (nth-value 2 (%fstat (%descriptor-fd desc)))))))

(defun %effective-access-flags (dflags implied-write-p)
  "The descriptor flags an OPEN reports: the guest's requested flags
normalized the way the host actually opened the descriptor. A missing
explicit `write' request means read mode (an empty request opens
read-only and reports `read'); an explicit `write' without `read' is
write-only. IMPLIED-WRITE-P says `create'/`truncate' opened the file
for writing: it adds `write' but, unlike an explicit `write' request,
does not remove the read default (the upstream
filesystem-flags-and-type test: CREATE with no flags reports
READ|WRITE; CREATE with WRITE reports WRITE)."
  (let ((requested-read (member "read" dflags :test #'string=))
        (requested-write (member "write" dflags :test #'string=)))
    (append (if (or requested-read (not requested-write)) '("read") '())
            (if (or requested-write implied-write-p) '("write") '())
            (remove-if (lambda (f) (member f '("read" "write") :test #'string=))
                       dflags))))

(defun fs-get-flags (context self)
  "The descriptor's flags as the host reports them. A DIRECTORY reports
`read' plus the directory rights it actually has (`mutate-directory'),
never the file-only `write' mode: the upstream wasip3
filesystem-flags-and-type test requires READ|MUTATE_DIRECTORY for a
read/write preopen. A file reports its effective access mode."
  (%fs-result
   (let* ((desc (%descriptor-of context self (descriptor-resource-ct)))
          (flags (wasi-descriptor-flags desc))
          (dir-p (or (wasi-descriptor-dir-p desc)
                     (let ((mode (nth-value 2 (%fstat (%descriptor-fd desc)))))
                       (and mode (= (logand mode +s-ifmt+) +s-ifdir+))))))
     (cons :flags
           (if dir-p
               (cons "read"
                     (remove-if (lambda (f)
                                  (member f '("read" "write") :test #'string=))
                                (copy-list flags)))
               (copy-list flags))))))

(defun fs-set-size (context self size)
  "Adjust the file size. The KERNEL is the authority on whether this
descriptor may be resized: a read-only descriptor gets `invalid' from
`ftruncate' (EINVAL), exactly as the reference runtime reports it — the
upstream filesystem-set-size test accepts invalid/bad-descriptor/access
and rejects a provider-invented `read-only'."
  (%fs-result
   (let ((desc (%descriptor-of context self (descriptor-resource-ct))))
     ;; the WIT's `filesize' is u64 but the host's off_t is signed: a
     ;; size that cannot be represented is `file-too-large' (EFBIG),
     ;; never a host type error escaping the provider
     (when (> size (1- (ash 1 63)))
       (%fs-error "file-too-large"))
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
                  ;; `create'/`truncate' can only have opened the file
                  ;; for writing, so they are part of the effective
                  ;; access mode the descriptor reports (upstream
                  ;; filesystem-flags-and-type: CREATE implies WRITE)
                  (effective (%effective-access-flags
                              dflags (or create truncate)))
                  (opened (make-wasi-descriptor
                           :fd fd
                           :preopen (wasi-descriptor-preopen desc)
                           :flags effective
                           :owned-p t
                           :dir-p is-dir)))
             (list :own-handle (descriptor-resource-ct) opened))))))))

(defun fs-advise (context self offset length advice)
  "File advisory information. `posix_fadvise' applies to files only; a
DIRECTORY is `bad-descriptor', never a silent success (the upstream
wasip3 filesystem-advise test asserts it, and an advisory on a
directory has no meaning)."
  (%fs-result
   (let* ((desc (%descriptor-of context self (descriptor-resource-ct)))
          (mode (nth-value 2 (%fstat (%descriptor-fd desc))))
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
     (when (and mode (= (logand mode +s-ifmt+) +s-ifdir+))
       (%fs-error "bad-descriptor"))
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
        (cond
          ;; off_t is signed: an offset at or above 2^63 cannot be
          ;; represented, and POSIX makes such a pread EINVAL. The WIT
          ;; case is `invalid' (the upstream filesystem-io test asks
          ;; for a read at u64::MAX and expects exactly that).
          ((>= pos (ash 1 63))
           (funcall settle (cons :err "invalid"))
           (values nil t))
          ((<= want 0) (values nil nil))
          (t
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
                 (values out (< n want)))))))))))

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
           (if (>= pos (ash 1 63))
               ;; off_t cannot represent POS: EINVAL, the WIT's invalid
               (values 0 "invalid")
             (progn
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
                      (values 0 (errno->error-code errno)))))))))))))))

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
;;; docs/design/PLAN.md §9.1: "A host import must receive an explicitly allocated
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
;;; The OS surface is SB-ALIEN only (docs/design/PLAN.md §9.2): the sockaddr
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
  "A wasi:sockets network authority (docs/design/PLAN.md §9.1): the declared
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

(defun reactor-pending-deadline-p (reactor)
  "T when REACTOR holds at least one ARMED one-shot deadline: a
registration still in the :LIVE state (dispatch and cancel both
unlink it, so the deadline list never keeps a spent registration).
This is the liveness question the store's drain oracle asks through
%REACTOR-PENDING-DEADLINE-P: the clock alone makes such a registration
due on the next step. FD watches are not consulted — readiness is not
guaranteed."
  (let ((mutex (wasi-reactor-mutex reactor))
        (armed nil))
    (sb-thread:grab-mutex mutex)
    (unwind-protect
         (unless (wasi-reactor-closed-p reactor)
           (dolist (registration (wasi-reactor-deadlines reactor))
             (when (eq (reactor-registration-state registration) :live)
               (setf armed t)
               (return))))
      (sb-thread:release-mutex mutex))
    (and armed t)))

(defmethod wasmdown.component.runtime:%reactor-pending-deadline-p
    ((reactor wasi-reactor))
  "The store driver's view of the reactor's armed timers (the async
lift's drain oracle consults this; see the generic's documentation)."
  (reactor-pending-deadline-p reactor))

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
  (list :enum (ecase family (:ipv4 "ipv4") (:ipv6 "ipv6"))))

(defun %wildcard-host (family)
  (ecase family (:ipv4 '(0 0 0 0)) (:ipv6 '(0 0 0 0 0 0 0 0))))

(defun %unspecified-host-p (host)
  "T when HOST is INADDR_ANY / in6addr_any (all zero)."
  (and (listp host) (every #'zerop host)))

(defun %ipv4-mapped-host-p (family host)
  "T for an IPv6 address in the ::ffff:0:0/96 mapped range, which the
WIT rejects (dual-stack sockets do not exist in this authority)."
  (and (eq family :ipv6)
       (listp host)
       (= 8 (length host))
       (every #'zerop (subseq host 0 5))
       (= #xffff (nth 5 host))))

(defun %multicast-or-broadcast-host-p (family host)
  "T for an address that is not unicast: IPv4 multicast (224/4) and
the limited broadcast, IPv6 multicast (ff00::/8)."
  (case family
    (:ipv4 (and (listp host) (= 4 (length host))
                (or (<= 224 (first host) 239)
                    (equal host '(255 255 255 255)))))
    (:ipv6 (and (listp host) (= 8 (length host))
                (<= #xff00 (first host) #xffff)))
    (t nil)))

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
  ;; The keep-alive durations/count are WIT values, not raw kernel
  ;; integers: the kernel takes seconds (and rejects values beyond its
  ;; own limits), while the WIT is nanoseconds and accepts any u64.
  ;; The setters store the requested value and push a best-effort
  ;; clamped value to the kernel; the getters answer the stored one.
  (keep-alive-idle-time 0)
  (keep-alive-interval 0)
  (keep-alive-count 0)
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
                (when (or (%ipv4-mapped-host-p family host)
                          (%multicast-or-broadcast-host-p family host))
                  (%socket-error (%socket-code "invalid-argument")))
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
                     (when (or (zerop port)
                               (%unspecified-host-p host)
                               (%ipv4-mapped-host-p family host)
                               (%multicast-or-broadcast-host-p family host))
                       (%socket-error (%socket-code "invalid-argument")))
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
                                    :state :connected
                                    :keep-alive-idle-time
                                    (wasi-tcp-socket-keep-alive-idle-time sock)
                                    :keep-alive-interval
                                    (wasi-tcp-socket-keep-alive-interval sock)
                                    :keep-alive-count
                                    (wasi-tcp-socket-keep-alive-count sock)))
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
;;; nanoseconds (wasi:clocks/types.duration); the kernel takes seconds,
;;; so the setters store the WIT value and only push a clamped hint.

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

(defun tcp-get-keep-alive-idle-time (context network self
                                     &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (wasi-tcp-socket-keep-alive-idle-time
    (%tcp-of network self (tcp-socket-resource-ct :package package)))))

(defun tcp-set-keep-alive-idle-time (context network self value
                                     &key (package (wasi-package "sockets")))
  (declare (ignore context))
  ;; the WIT rejects a ZERO duration before it reaches the kernel
  (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                     :package package))))
    (%socket-result
     (when (zerop value)
       (%socket-error (%socket-code "invalid-argument")))
     (setf (wasi-tcp-socket-keep-alive-idle-time sock) value)
     ;; best effort: TCP_KEEPIDLE is in SECONDS, clamped to the
     ;; kernel's accepted range; the WIT answer is the stored value
     (ignore-errors
       (%setsockopt-int (%tcp-fd sock) +sol-tcp+ +tcp-keepidle+
                        (max 1 (min 32767 (floor value 1000000000)))))
     nil)))

(defun tcp-get-keep-alive-interval (context network self
                                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (wasi-tcp-socket-keep-alive-interval
    (%tcp-of network self (tcp-socket-resource-ct :package package)))))

(defun tcp-set-keep-alive-interval (context network self value
                                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                     :package package))))
    (%socket-result
     (when (zerop value)
       (%socket-error (%socket-code "invalid-argument")))
     (setf (wasi-tcp-socket-keep-alive-interval sock) value)
     (ignore-errors
       (%setsockopt-int (%tcp-fd sock) +sol-tcp+ +tcp-keepintvl+
                        (max 1 (min 32767 (floor value 1000000000)))))
     nil)))

(defun tcp-get-keep-alive-count (context network self
                                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (%socket-result
   (wasi-tcp-socket-keep-alive-count
    (%tcp-of network self (tcp-socket-resource-ct :package package)))))

(defun tcp-set-keep-alive-count (context network self value
                                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let ((sock (%tcp-of network self (tcp-socket-resource-ct
                                     :package package))))
    (%socket-result
     (when (zerop value)
       (%socket-error (%socket-code "invalid-argument")))
     (setf (wasi-tcp-socket-keep-alive-count sock) value)
     ;; the kernel accepts 1..127 probes; the WIT answer is the
     ;; requested value
     (ignore-errors
       (%setsockopt-int (%tcp-fd sock) +sol-tcp+ +tcp-keepcnt+
                        (min value 127)))
     nil)))

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
                (when (or (%ipv4-mapped-host-p family host)
                          (%multicast-or-broadcast-host-p family host))
                  (%socket-error (%socket-code "invalid-argument")))
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
                (when (or (zerop port)
                          (%unspecified-host-p host)
                          (%ipv4-mapped-host-p family host)
                          (%multicast-or-broadcast-host-p family host))
                  (%socket-error (%socket-code "invalid-argument")))
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
                  (unless (eq family (wasi-udp-socket-family sock))
                    (%socket-error (%socket-code "invalid-argument")))
                  (when (or (zerop port)
                            (%unspecified-host-p host)
                            (%ipv4-mapped-host-p family host)
                            (%multicast-or-broadcast-host-p family host))
                    (%socket-error (%socket-code "invalid-argument")))
                  (unless (wasi-network-allows-outbound-p
                           network family host port)
                    (%socket-error (%socket-code "access-denied")))
                  (%udp-send-bytes fd data alien len))
             (sb-alien:free-alien alien)))
         (%udp-send-bytes fd data nil nil))
     ;; a successful datagram send implicitly binds the socket (POSIX)
     (setf (wasi-udp-socket-bound-p sock) t)
     nil)))

(defun %udp-send-bytes (fd bytes alien len)
  "sendto/send BYTES; a socket error is signalled as WASI-SOCKET-ERROR
(the caller's %SOCKET-RESULT turns it into the WIT variant)."
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
                                        (%socket-error
                                         (%socket-code "timeout"))))
                                     (t
                                      (%socket-error
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

(defun %resolved-host-pending (value)
  "A host-pending already resolved with VALUE (used when the async
entry's synchronous preconditions fail)."
  (wasmdown.component.runtime:make-host-pending
   :start (lambda (resolve reject acknowledge-cancel)
            (declare (ignore reject acknowledge-cancel))
            (funcall resolve value)
            (lambda () nil))))

(defun %udp-receive-pending (context network self
                             &key (package (wasi-package "sockets")))
  "The ASYNC `udp-socket.receive` answer. The accept/read itself runs
on a worker thread, so the guest's other tasks keep making progress
(the WIT declares this method `async`); the result comes back through
the store's pending pump. Every failure is the WIT domain error."
  (handler-case
      (let* ((sock (%udp-of network self (udp-socket-resource-ct
                                          :package package)))
             (fd (%udp-fd sock)))
        (if (not (wasi-udp-socket-bound-p sock))
            (%resolved-host-pending
             (cons :err (%socket-code "invalid-state")))
            (wasmdown.component.runtime:make-host-pending
             :start
             (lambda (resolve reject acknowledge-cancel)
               (declare (ignore acknowledge-cancel))
               (sb-thread:make-thread
                (lambda ()
                  (handler-case
                      (multiple-value-bind (bytes addr) (%udp-recv fd)
                        (if bytes
                            (funcall resolve (cons :ok (list bytes addr)))
                            (funcall resolve
                                     (cons :err (%socket-code "timeout")))))
                    (wasi-socket-error (c)
                      (funcall resolve
                               (cons :err (wasi-socket-error-code c))))
                    (error (c) (funcall reject c))))
                :name "wasi-udp-receive")
               ;; the read is not interruptible mid-poll: a cancel
               ;; request leaves the worker's normal result to deliver
               (lambda () nil)))))
    (wasi-socket-error (c)
      (%resolved-host-pending (cons :err (wasi-socket-error-code c))))))

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
                  (lambda (self) (%udp-receive-pending context net self
                                                       :package package)))
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

;;; ===============================================================
;;; wasi:http (M16 slice 5) — THE TYPES RESOURCES and client.send
;;; over HTTP/1.1 with a PLUGGABLE TRANSPORT.
;;;
;;; PLAN.md §9: "HTTP outgoing client and incoming service/middleware
;;; worlds, header/body/trailer semantics, backpressure, status/
;;; errors; TLS/HTTPS via an explicit host transport abstraction."
;;; §9.2: "plugin system TLS providers or embedder-supplied transport
;;; services. Do not write custom cryptographic TLS."
;;;
;;; The transport abstraction is the answer to https. A request whose
;;; scheme is "http" rides the built-in plain TCP transport; any other
;;; scheme is looked up in the CONTEXT's wasi-http-transport list
;;; (never an ambient global) and, when absent, the send fails with
;;; the WIT's `configuration-error' — never a silent plaintext
;;; fallback, never home-grown TLS.
;;;
;;; The client is a PULL-DRIVEN HTTP/1.1 implementation: send()
;;; writes the head + already-staged request body and parses the
;;; response head synchronously; the response body is decoded lazily
;;; by the host stream SOURCE the guest reads from (content-length,
;;; chunked with trailers, or read-to-close), so no reactor thread is
;;; needed. Request bodies must be staged (written and closed) before
;;; send — the residual for streaming uploads is recorded in the
;;; slice report.
;;;
;;; The `fields` resource is the WIT's header map: case-insensitive
;;; names, ordered values, original casing preserved, mutable vs
;;; immutable handles, the header-error validation (invalid-syntax,
;;; forbidden, immutable, size-exceeded).

;;; ---------------------------------------------------------------
;;; The WIT packages + functype helpers.

(defun %http-packages (&key (package (wasi-package "http")))
  "The WIT packages wasi:http functypes resolve against (duration
comes from wasi:clocks/types). The clocks package is taken at the
SAME release as PACKAGE, so the 0.2 facade resolves `duration' against
wasi:clocks@0.2.x and the 0.3 providers against 0.3.2."
  (list package
        (wasi-package "clocks"
                      :version (wasmdown.wit::wit-package-version package))))

(defun http-resource-ct (name &key (package (wasi-package "http")))
  "The ONE ct object naming the resource NAME in wasi:http/types —
the handle table's type identity for every handle of that resource."
  (wasmdown.wit:wit-type->ct
   (wasmdown.wit:package-interface package "types")
   (list :resource name)
   :packages (%http-packages :package package)))

(defun %http-resource-method (resource method
                              &key (package (wasi-package "http")))
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
             :message (format nil "wasi:http/types has no ~a.~a"
                              resource method)))
    (values interface res fn)))

(defun %http-method-ct (resource method
                        &key (package (wasi-package "http")))
  (multiple-value-bind (interface res fn)
      (%http-resource-method resource method :package package)
    (%resource-method-ct interface res fn (%http-packages :package package))))

(defun %http-result-ct (resource method
                        &key (package (wasi-package "http")))
  (second (wasmdown.component:ct-payload
           (%http-method-ct resource method :package package))))

(defun %http-future-ct (resource method
                        &key (package (wasi-package "http")))
  "The ELEMENT ct of the FIRST future<...> in the method's result."
  (let ((fc (%ct-find-kind (%http-result-ct resource method
                                            :package package)
                           :future)))
    (unless fc
      (error 'wasmdown:host-error
             :message (format nil "~a.~a has no future result"
                              resource method)))
    (wasmdown.component:ct-payload fc)))

(defun %http-stream-ct (resource method
                        &key (package (wasi-package "http")))
  "The ELEMENT ct of the FIRST stream<...> in the method's result."
  (let ((sc (%ct-find-kind (%http-result-ct resource method
                                            :package package)
                           :stream)))
    (unless sc
      (error 'wasmdown:host-error
             :message (format nil "~a.~a has no stream result"
                              resource method)))
    (wasmdown.component:ct-payload sc)))

;;; ---------------------------------------------------------------
;;; The error-code condition. The CODE is the WIT VARIANT DOMAIN
;;; value (label . payload), the shape the canonical store accepts
;;; for a real `error-code'.

(define-condition wasi-http-error (wasmdown:wasmdown-error)
  ((code :initarg :code
         :initform (cons "internal-error" (cons :some "wasi:http error"))
         :reader wasi-http-error-code))
  (:report (lambda (c s)
             (format s "wasi:http error: ~a" (wasi-http-error-code c)))))

(defun http-error-code (value)
  "The case name of an error-code domain value (the CL test view)."
  (and (consp value) (car value)))

(defun %http-error (code)
  (when (sb-ext:posix-getenv "WASMDOWN_TRACE_HTTP")
    (format *error-output* "HTTP ERROR ~s~%" code)
    (when (null code) (sb-debug:print-backtrace :count 12)))
  (error 'wasi-http-error :code code))

(defmacro %http-result (&body body)
  "Run BODY; a wasi-http-error becomes the WIT's (:err . code)."
  `(handler-case (progn ,@body)
     (wasi-http-error (c) (cons :err (wasi-http-error-code c)))))

;;; The variant helpers: a domain value is (label . payload).
(defun %http-code (label) (cons label nil))
(defun %http-code-payload (label payload) (cons label payload))
(defun %http-internal (message)
  (cons "internal-error" (cons :some message)))

;;; ---------------------------------------------------------------
;;; fields: the header map.

(defparameter +http-max-field-name-size+ 4096)
(defparameter +http-max-field-value-size+ 65536)
(defparameter +http-max-fields-total-size+ (* 1024 1024))

;;; RFC 7230 tchar (the token production).
(defparameter +http-tchar-chars+
  "!#$%&'*+-.^_`|~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "The HTTP token character set (RFC 7230 §3.2.6).")

(defparameter +http-forbidden-field-names+
  '("connection" "keep-alive" "proxy-authenticate" "proxy-authorization"
    "proxy-connection" "transfer-encoding" "upgrade" "host"
    "http2-settings")
  "The field names the HTTP implementation owns: a guest cannot set
them. The exact wasmtime `DEFAULT_FORBIDDEN_HEADERS' set (WASI #940
has no spec list yet); `te'/`trailer'/`content-length' are NOT on it
— the upstream client fixtures append `trailer' to a fresh fields.
Case-insensitive.")

(defun %http-token-p (string)
  (and (stringp string)
       (plusp (length string))
       (every (lambda (c) (find c +http-tchar-chars+ :test #'char=)) string)))

(defun %http-forbidden-field-name-p (name)
  (and (stringp name)
       (member (string-downcase name)
               +http-forbidden-field-names+ :test #'string=)))

(defun %field-value-bytes (value)
  "A field-value's bytes as a list of u8 (a CL string, vector and
list are all accepted at the host boundary; the ABI produces a list)."
  (cond ((stringp value) (map 'list #'char-code value))
        ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t (list value))))

(defun %http-field-value-ok-p (bytes)
  "field-value = *( HTAB / SP / VCHAR / obs-text ) — CR, LF and other
CTLs are rejected (§4.2's invalid-syntax)."
  (and (listp bytes)
       (every (lambda (b)
                (and (integerp b) (<= 0 b 255)
                     (or (= b 9) (<= 32 b 126) (<= 128 b 255))))
              bytes)))

(defmacro %http-header-error (label)
  `(cons :err (cons ,label nil)))

(defun %fields-canonical-name (name)
  "The STORED spelling of a field name: lowercase. HTTP field names
are case-insensitive and the reference implementation parses them
through http::HeaderName, which lowercases; `copy-all'/`clone' then
report the lowercase spelling whatever case the caller used
(wasi-testsuite http-fields' case-insensitivity check)."
  (string-downcase name))

(defstruct (wasi-fields (:constructor %make-wasi-fields (entries mutable-p)))
  "The `fields' resource rep: ENTRIES is the ordered list of
(canonical-name . value-bytes) pairs (one pair per value; the name is
stored lowercase — http::HeaderName semantics), and MUTABLE-P is the
WIT's mutability bit (a `fields' returned by request.headers /
response.headers is immutable)."
  (entries nil)
  (mutable-p t))

(defun make-wasi-fields (&key entries (mutable-p t))
  "A `fields' from (name . value) entries; values may be strings,
vectors or u8 lists."
  (%make-wasi-fields
   (loop for (name . value) in entries
         collect (cons (%fields-canonical-name name)
                       (%field-value-bytes value)))
   mutable-p))

(defun %fields-of (value ct)
  "The wasi-fields VALUE denotes (a rep, or a handle index resolved
through the dynamic caller's/provider's handle table)."
  (let ((f (cond ((wasi-fields-p value) value)
                 ((integerp value)
                  (wasmdown.component.runtime:host-resource-rep ct value)))))
    (unless (wasi-fields-p f)
      (error 'wasmdown:host-error :message "fields: invalid handle"))
    f))

(defun %http-take-resource (value ct)
  "An OWN resource argument: the rep, consuming the handle from the
dynamic caller's table when VALUE is a handle index. A bare rep (a
direct CL call) passes through."
  (cond
    ((integerp value)
     (multiple-value-bind (rep found)
         (wasmdown.component.runtime:host-resource-take ct value)
       (declare (ignore found))
       (or rep value)))
    (t value)))

(defun %fields-values (fields name)
  (loop for (n . v) in (wasi-fields-entries fields)
        when (string-equal n name)
          collect v))

(defun %fields-total-size (fields)
  (loop for (n . v) in (wasi-fields-entries fields)
        sum (+ (length n) (length v))))

(defun %fields-replace (fields name values)
  "Replace every value of NAME (case-insensitively), keeping the
position of the first old entry; append when absent."
  (let* ((entries (wasi-fields-entries fields))
         (name (%fields-canonical-name name))
         (pos (position name entries :key #'car :test #'string-equal))
         (new (loop for v in values collect (cons name v))))
    (setf (wasi-fields-entries fields)
          (if pos
              (append (subseq entries 0 pos) new
                      (remove name (nthcdr (1+ pos) entries)
                              :key #'car :test #'string-equal))
              (append entries new)))))

(defun %fields-append (fields name value)
  (setf (wasi-fields-entries fields)
        (append (wasi-fields-entries fields)
                (list (cons (%fields-canonical-name name) value)))))

(defun %fields-delete (fields name)
  (setf (wasi-fields-entries fields)
        (remove name (wasi-fields-entries fields)
                :key #'car :test #'string-equal)))

(defun %fields-size-check (fields name values &key (exceeded "size-exceeded"))
  "The implementation limit check: NAME + VALUES fit the per-field and
aggregate limits (header-error.EXCEEDED otherwise). EXCEEDED defaults
to the 0.3 case `size-exceeded'; the 0.2 releases do not declare that
case, so their callers pass `invalid-syntax'."
  (flet ((too-big () (%http-header-error exceeded)))
    (cond
      ((> (length name) +http-max-field-name-size+) (too-big))
      ((some (lambda (v) (> (length v) +http-max-field-value-size+)) values)
       (too-big))
      (t
       (let ((projected
               (+ (%fields-total-size
                   (%make-wasi-fields
                    (remove name (wasi-fields-entries fields)
                            :key #'car :test #'string-equal)
                    t))
                  (* (max 1 (length values)) (length name))
                  (reduce #'+ values :key #'length :initial-value 0))))
         (if (> projected +http-max-fields-total-size+)
             (too-big)
             (cons :ok nil)))))))

;;; The WIT methods.

(defun %fields-ct (package)
  (http-resource-ct "fields" :package package))

(defun fields-from-list (entries &key (package (wasi-package "http"))
                                   (exceeded "size-exceeded"))
  "fields.from-list: validate every (name, value) entry."
  (let ((entries (coerce entries 'list)))
    (loop for entry in entries
          for name = (first entry)
          for value = (%field-value-bytes (second entry))
          do (cond
               ((not (%http-token-p name))
                (return-from fields-from-list
                  (%http-header-error "invalid-syntax")))
               ((%http-forbidden-field-name-p name)
                (return-from fields-from-list
                  (%http-header-error "forbidden")))
               ((not (%http-field-value-ok-p value))
                (return-from fields-from-list
                  (%http-header-error "invalid-syntax")))
               ((> (length name) +http-max-field-name-size+)
                (return-from fields-from-list
                  (%http-header-error exceeded)))
               ((> (length value) +http-max-field-value-size+)
                (return-from fields-from-list
                  (%http-header-error exceeded)))))
    (let ((fields (make-wasi-fields
                   :entries (loop for entry in entries
                                  collect (cons (%fields-canonical-name
                                                 (first entry))
                                                (%field-value-bytes
                                                 (second entry)))))))
      (if (> (%fields-total-size fields) +http-max-fields-total-size+)
          (%http-header-error exceeded)
          (list :own-handle (%fields-ct package) fields)))))

(defun fields-get (fields name &key (package (wasi-package "http")))
  (let ((f (%fields-of fields (%fields-ct package))))
    (if (%http-token-p name) (%fields-values f name) '())))

(defun fields-has (fields name &key (package (wasi-package "http")))
  (let ((f (%fields-of fields (%fields-ct package))))
    (and (%http-token-p name) (not (null (%fields-values f name))))))

(defun fields-set (fields name values &key (package (wasi-package "http"))
                                         (exceeded "size-exceeded"))
  (let* ((f (%fields-of fields (%fields-ct package)))
         (values (mapcar #'%field-value-bytes (coerce values 'list))))
    (cond
      ((not (wasi-fields-mutable-p f)) (%http-header-error "immutable"))
      ((not (%http-token-p name)) (%http-header-error "invalid-syntax"))
      ((%http-forbidden-field-name-p name) (%http-header-error "forbidden"))
      ((not (every #'%http-field-value-ok-p values))
       (%http-header-error "invalid-syntax"))
      (t (let ((check (%fields-size-check f name values :exceeded exceeded)))
           (if (eq (car check) :ok)
               (progn (%fields-replace f name values) (cons :ok nil))
               check))))))

(defun fields-delete (fields name &key (package (wasi-package "http")))
  (let ((f (%fields-of fields (%fields-ct package))))
    (cond
      ((not (wasi-fields-mutable-p f)) (%http-header-error "immutable"))
      ;; HeaderName::try_from: an invalid field-name is
      ;; header-error.invalid-syntax, never a silent no-op (the
      ;; upstream http-fields suite asserts Err for "", "voilà",
      ;; "hey ho", ...).
      ((not (%http-token-p name)) (%http-header-error "invalid-syntax"))
      (t (progn (%fields-delete f name) (cons :ok nil))))))

(defun fields-get-and-delete (fields name
                              &key (package (wasi-package "http")))
  (let ((f (%fields-of fields (%fields-ct package))))
    (cond
      ((not (wasi-fields-mutable-p f)) (%http-header-error "immutable"))
      ((not (%http-token-p name)) (%http-header-error "invalid-syntax"))
      (t (let ((values (%fields-values f name)))
           (%fields-delete f name)
           (cons :ok values))))))

(defun fields-append (fields name value
                      &key (package (wasi-package "http"))
                        (exceeded "size-exceeded"))
  (let* ((f (%fields-of fields (%fields-ct package)))
         (value (%field-value-bytes value)))
    (cond
      ((not (wasi-fields-mutable-p f)) (%http-header-error "immutable"))
      ((not (%http-token-p name)) (%http-header-error "invalid-syntax"))
      ((%http-forbidden-field-name-p name) (%http-header-error "forbidden"))
      ((not (%http-field-value-ok-p value))
       (%http-header-error "invalid-syntax"))
      (t (let ((check (%fields-size-check f name (list value)
                                          :exceeded exceeded)))
           (if (eq (car check) :ok)
               (progn (%fields-append f name value) (cons :ok nil))
               check))))))

(defun fields-copy-all (fields &key (package (wasi-package "http")))
  (let ((f (%fields-of fields (%fields-ct package))))
    (loop for (name . value) in (wasi-fields-entries f)
          collect (list name (copy-list value)))))

(defun fields-clone (fields &key (package (wasi-package "http")))
  (let* ((f (%fields-of fields (%fields-ct package)))
         (copy (make-wasi-fields
                :entries (loop for (name . value)
                                 in (wasi-fields-entries f)
                               collect (cons name (copy-list value))))))
    (list :own-handle (%fields-ct package) copy)))

(defun fields-constructor (&key (package (wasi-package "http")))
  (list :own-handle (%fields-ct package) (make-wasi-fields)))

(defun %fields-immutable-view (fields &key (package (wasi-package "http")))
  "The `headers' a request/response accessor returns: a copy flagged
immutable so set/append/delete fail with header-error.immutable."
  (let ((copy (make-wasi-fields
               :entries (loop for (name . value)
                                in (wasi-fields-entries fields)
                              collect (cons name (copy-list value)))
               :mutable-p nil)))
    (list :own-handle (http-resource-ct "fields" :package package) copy)))

;;; ---------------------------------------------------------------
;;; request-options.

(defstruct wasi-http-options
  "The `request-options' resource rep. Durations are nanoseconds
(wasi:clocks/types.duration), :none for unset."
  (connect-timeout :none)
  (first-byte-timeout :none)
  (between-bytes-timeout :none)
  (mutable-p t))

(defun %options-of (value ct)
  (let ((o (cond ((wasi-http-options-p value) value)
                 ((integerp value)
                  (wasmdown.component.runtime:host-resource-rep ct value)))))
    (unless (wasi-http-options-p o)
      (error 'wasmdown:host-error :message "request-options: invalid handle"))
    o))

(defun %http-options-error (label) (cons :err (cons label nil)))

(defun %options-set-timeout (options slot-key duration)
  "A duration is option<duration> (u64 nanos). A ZERO duration is
rejected: the WIT's `not-supported' — this implementation cannot
honour a zero-length transport timeout."
  (cond
    ((not (wasi-http-options-mutable-p options))
     (%http-options-error "immutable"))
    ((and (consp duration) (integerp (cdr duration)) (zerop (cdr duration)))
     (%http-options-error "not-supported"))
    ((and (integerp duration) (zerop duration))
     (%http-options-error "not-supported"))
    (t
     (ecase slot-key
       (:connect (setf (wasi-http-options-connect-timeout options) duration))
       (:first-byte
        (setf (wasi-http-options-first-byte-timeout options) duration))
       (:between-bytes
        (setf (wasi-http-options-between-bytes-timeout options) duration)))
     (cons :ok nil))))

(defun %options-ct (package)
  (http-resource-ct "request-options" :package package))

(defun request-options-constructor (&key (package (wasi-package "http")))
  (list :own-handle (%options-ct package)
        (make-wasi-http-options)))

(defun request-options-get-connect-timeout (options
                                            &key (package (wasi-package "http")))
  (wasi-http-options-connect-timeout
   (%options-of options (%options-ct package))))

(defun request-options-set-connect-timeout (options duration
                                            &key (package (wasi-package "http")))
  (%options-set-timeout
   (%options-of options (%options-ct package))
   :connect duration))

(defun request-options-get-first-byte-timeout (options
                                               &key (package (wasi-package "http")))
  (wasi-http-options-first-byte-timeout
   (%options-of options (%options-ct package))))

(defun request-options-set-first-byte-timeout (options duration
                                               &key (package (wasi-package "http")))
  (%options-set-timeout
   (%options-of options (%options-ct package))
   :first-byte duration))

(defun request-options-get-between-bytes-timeout (options
                                                  &key (package (wasi-package "http")))
  (wasi-http-options-between-bytes-timeout
   (%options-of options (%options-ct package))))

(defun request-options-set-between-bytes-timeout (options duration
                                                  &key (package (wasi-package "http")))
  (%options-set-timeout
   (%options-of options (%options-ct package))
   :between-bytes duration))

(defun request-options-clone (options &key (package (wasi-package "http")))
  (let ((o (%options-of options (%options-ct package))))
    (list :own-handle
          (%options-ct package)
          (make-wasi-http-options
           :connect-timeout (wasi-http-options-connect-timeout o)
           :first-byte-timeout (wasi-http-options-first-byte-timeout o)
           :between-bytes-timeout
           (wasi-http-options-between-bytes-timeout o)))))

;;; ---------------------------------------------------------------
;;; method / scheme / authority / path validation.

(defun %http-method-ok-p (method)
  "A method domain value: a known case, or other(string) whose string
is a valid token."
  (and (consp method)
       (let ((label (car method)))
         (cond
           ((string-equal label "other")
            (%http-token-p (cdr method)))
           (t
            (member label
                    '("get" "head" "post" "put" "delete" "connect"
                      "options" "trace" "patch")
                    :test #'string=))))))

(defun %http-scheme-ascii-alpha-p (c)
  (or (char<= #\a c #\z) (char<= #\A c #\Z)))

(defun %http-scheme-ok-p (scheme)
  ;; RFC 3986 scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ); the
  ;; grammar is ASCII-only, so a Unicode letter is NOT a valid scheme
  ;; even though SBCL's alpha-char-p would accept it.
  (and (consp scheme)
       (cond
         ((string-equal (car scheme) "other")
          (let ((s (cdr scheme)))
            (and (stringp s) (plusp (length s))
                 (%http-scheme-ascii-alpha-p (char s 0))
                 (every (lambda (c)
                          (or (%http-scheme-ascii-alpha-p c)
                              (digit-char-p c)
                              (find c "+-." :test #'char=)))
                        s))))
         (t (member (car scheme) '("HTTP" "HTTPS") :test #'string=)))))

(defun %http-path-ok-p (path)
  "origin-form or a query-only target: begins with `/`, `*`, or `?`;
  visible ASCII (no space/CR/LF/NUL; `#' starts a fragment and is not
  part of a request target); raw UTF-8 codepoints >= 0x80 are allowed.
  Empty is normalized to `/` by the setter."
  (and (stringp path)
       (plusp (length path))
       (or (char= (char path 0) #\/) (char= (char path 0) #\?)
           (string= path "*"))
       (every (lambda (c)
                (let ((code (char-code c)))
                  (or (>= code #x80)
                      (char<= #\0 c #\9)
                      (char<= #\a c #\z)
                      (char<= #\A c #\Z)
                      (find c "-._~%!$&'()*+,;=:@/?" :test #'char=))))
              path)))

(defun %http-authority-char-p (c)
  (or (char<= #\0 c #\9) (char<= #\a c #\z)
      (char<= #\A c #\Z)
      (find c "-._~%!$&'()*+,;=:" :test #'char=)))

(defun %http-authority-ok-p (authority)
  "RFC 3986 authority = [ userinfo \"@\" ] host [ \":\" port ].
  Userinfo is accepted (the upstream suite asserts user:pass@host);
  percent/sub-delims/unreserved are legal, a non-numeric or empty port
  is not, and a second @ or : is rejected. IPv6 literals are not yet
  accepted (the upstream suite leaves them commented out)."
  (and (stringp authority)
       (plusp (length authority))
       (let ((at (position #\@ authority)))
         (and (or (null at)
                  (and (plusp at)
                       (not (position #\@ authority :start (1+ at)))
                       (every #'%http-authority-char-p
                              (subseq authority 0 at))))
              (let* ((hostport (if at (subseq authority (1+ at)) authority))
                     (colon (position #\: hostport)))
                (and (plusp (length hostport))
                     (or (null colon)
                         (and (plusp colon)
                              (not (position #\: hostport :start (1+ colon)))
                              (plusp (- (length hostport) (1+ colon)))
                              (every #'digit-char-p
                                     (subseq hostport (1+ colon)))))
                     (every (lambda (c)
                              (or (char= c #\:)
                                  (and (not (char= c #\@))
                                       (%http-authority-char-p c))))
                            hostport)))))))

(defun %http-split-authority (authority)
  "authority -> (values host port): userinfo is stripped and the
default port is 80. The caller has already validated AUTHORITY with
%HTTP-AUTHORITY-OK-P (so a port, when present, is digits)."
  (let* ((at (position #\@ authority))
         (hostport (if at (subseq authority (1+ at)) authority))
         (colon (position #\: hostport)))
    (if colon
        (values (subseq hostport 0 colon)
                (parse-integer (subseq hostport (1+ colon))))
        (values hostport 80))))

(defun %http-method-string (method)
  "The wire spelling of a request method: other(X) -> X, a known case
-> its uppercase name."
  (if (string-equal (car method) "other")
      (string (cdr method))
      (string-upcase (car method))))

(defun %http-scheme-string (scheme)
  "The lowercase scheme string; :none means the implementation picks
the default (http for this transport stack). The field holds an
option<scheme>, so a (:some . scheme) is unwrapped here."
  (let ((s (if (and (consp scheme) (eq (car scheme) :some))
               (cdr scheme)
               scheme)))
    (if (eq s :none)
        "http"
        (if (string-equal (car s) "other")
            (string-downcase (string (cdr s)))
            (string-downcase (car s))))))

;;; ---------------------------------------------------------------
;;; The request resource.

(defstruct wasi-http-request
  (method (cons "get" nil))
  (path-with-query :none)
  (scheme :none)
  (authority :none)
  (headers nil)                       ; wasi-fields
  (options nil)                       ; wasi-http-options or NIL
  (body-end nil)                      ; the guest's readable contents end
  ;; T when a body was EXTRACTED from the request but is written through
  ;; a host-side sink instead of BODY-END (the 0.2 outgoing-body shape):
  ;; the sender then streams BODY-BUFFER until BODY-CLOSED-P exactly as
  ;; it does for a request.new contents stream.
  (body-present-p nil)
  (body-buffer nil)                   ; reversed list of byte-lists
  (body-length 0)
  (body-closed-p nil)
  (body-error nil)
  (body-mutex (sb-thread:make-mutex :name "http-request-body"))
  (body-cond (sb-thread:make-waitqueue :name "http-request-body"))
  (trailers-end nil)                  ; the readable trailers future
  (trailers-state :absent)            ; :absent :waiting :settled
  (trailers-value nil)                ; the watch's domain value
  (trailers-dropped-p nil)
  (trailers-inst nil)                 ; the guest table own handles resolve in
  (transmission-writer nil)           ; request.new's result future
  (transmission-mutex (sb-thread:make-mutex :name "http-request-send"))
  (consumed-p nil))

(defun %request-of (value ct)
  (let ((r (cond ((wasi-http-request-p value) value)
                 ((integerp value)
                  (wasmdown.component.runtime:host-resource-rep ct value)))))
    (unless (wasi-http-request-p r)
      (error 'wasmdown:host-error :message "request: invalid handle"))
    r))

(defun %http-end-of (value)
  "An option<stream> / option<resource> argument."
  (cond ((null value) nil)
        ((eq value :none) nil)
        ((and (consp value) (eq (car value) :some)) (cdr value))
        ((and (consp value) (member (car value) '("none" "some") :test #'string=))
         (if (string= (car value) "none") nil (cdr value)))
        (t value)))

(defun %http-future-value (end)
  "A non-blocking read of a guest-resolved future end: (values value
present-p)."
  (if (and end (wasmdown.component.runtime:async-end-p end))
      (wasmdown.component.runtime:host-end-read end)
      (values nil nil)))

(defun %http-request-settle (request value)
  "Resolve request.new's transmission future once VALUE is known. The
worker and the drop finalizer race; the first settlement wins."
  (let ((w nil))
    (sb-thread:with-recursive-lock
        ((wasi-http-request-transmission-mutex request))
      (setf w (wasi-http-request-transmission-writer request))
      (when w
        (setf (wasi-http-request-transmission-writer request) nil)))
    (when w
      (wasmdown.component.runtime:host-future-resolve w value))))

(defun http-request-new (headers contents trailers options
                         &key (package (wasi-package "http")))
  "request.new: construct the request and take the contents stream's
readable end as the body sink plus the trailers future's readable
end as a one-shot watch. The guest may keep writing after this
returns: client.send drains the body/trailers concurrently."
  (let* ((fields-ct (http-resource-ct "fields" :package package))
         (request-ct (http-resource-ct "request" :package package))
         (future-ct (%http-future-ct "request" "new" :package package))
         (h (%http-take-resource headers fields-ct))
         (req (make-wasi-http-request
               :headers (if (wasi-fields-p h) h (make-wasi-fields)))))
    (unless (wasi-fields-p h)
      (error 'wasmdown:host-error
             :message "request.new: headers is not a fields resource"))
    (let ((body (%http-end-of contents))
          (trailers-end (%http-end-of trailers))
          (opts (%http-end-of options)))
      (when body
        (unless (wasmdown.component.runtime:async-end-p body)
          (error 'wasmdown:host-error
                 :message "request.new: contents is not a stream end"))
        (setf (wasi-http-request-body-end req) body))
      (when trailers-end
        (unless (wasmdown.component.runtime:async-end-p trailers-end)
          (error 'wasmdown:host-error
                 :message "request.new: trailers is not a future end"))
        (setf (wasi-http-request-trailers-end req) trailers-end
              (wasi-http-request-trailers-state req) :waiting
              ;; own<fields> handles inside a guest-written async value
              ;; are raw indices in the LOWERING CALLER's table.
              (wasi-http-request-trailers-inst req)
              (or wasmdown.component.runtime::*handle-caller-inst*
                  wasmdown.component.runtime::*host-provider-inst*)))
      (when (and opts (not (wasmdown.component.runtime:async-end-p opts)))
        (let ((o (%http-take-resource opts
                                      (http-resource-ct "request-options"
                                                        :package package))))
          (unless (wasi-http-options-p o)
            (error 'wasmdown:host-error
                   :message "request.new: options is not a request-options"))
          (setf (wasi-http-request-options req) o)))
      (multiple-value-bind (future-reader future-writer)
          (wasmdown.component.runtime:make-host-future-pair future-ct)
        (setf (wasi-http-request-transmission-writer req) future-writer)
        (when body
          (wasmdown.component.runtime:host-end-set-sink
           body
           (lambda (octets start end)
             (let ((n (- end start)))
               (when (plusp n)
                 (sb-thread:with-recursive-lock
                     ((wasi-http-request-body-mutex req))
                   (push (coerce (subseq octets start end) 'list)
                         (wasi-http-request-body-buffer req))
                   (incf (wasi-http-request-body-length req) n)
                   (sb-thread:condition-broadcast
                    (wasi-http-request-body-cond req))))
               (values n nil))))
          (wasmdown.component.runtime:host-end-set-on-drop
           body
           (lambda (dropped error)
             (declare (ignore dropped))
             (sb-thread:with-recursive-lock
                 ((wasi-http-request-body-mutex req))
               (setf (wasi-http-request-body-closed-p req) t)
               (when error
                 (setf (wasi-http-request-body-error req) error))
               (sb-thread:condition-broadcast
                (wasi-http-request-body-cond req)))))
          (wasmdown.component.runtime:host-end-drain body))
        (when trailers-end
          (wasmdown.component.runtime:host-future-watch
           trailers-end
           (lambda (value dropped-p)
             (sb-thread:with-recursive-lock
                 ((wasi-http-request-body-mutex req))
               (setf (wasi-http-request-trailers-state req) :settled
                     (wasi-http-request-trailers-value req) value
                     (wasi-http-request-trailers-dropped-p req)
                     dropped-p)
               (sb-thread:condition-broadcast
                (wasi-http-request-body-cond req))))))
        (list (list :own-handle request-ct req) future-reader)))))

(defun http-request-get-method (this &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-request-method (%request-of this (http-resource-ct "request"))))

(defun %normalize-http-method (method)
  "A known other(GET) normalizes to the case `get' (the WIT issue
  the upstream suite asserts); arbitrary valid tokens stay other."
  (if (and (consp method) (string-equal (car method) "other")
           (stringp (cdr method)))
      (let ((known (member (string-downcase (cdr method))
                           '("get" "head" "post" "put" "delete" "connect"
                             "options" "trace" "patch")
                           :test #'string=)))
        (if known (list (car known)) method))
      method))

(defun http-request-set-method (this method
                                &key (package (wasi-package "http")))
  (declare (ignore package))
  (let* ((req (%request-of this (http-resource-ct "request")))
         (method (%normalize-http-method method)))
    (if (%http-method-ok-p method)
        (progn (setf (wasi-http-request-method req) method) (cons :ok nil))
        (cons :err nil))))

(defun http-request-get-path-with-query (this
                                         &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-request-path-with-query
   (%request-of this (http-resource-ct "request"))))

(defun http-request-set-path-with-query (this path
                                         &key (package (wasi-package "http")))
  (declare (ignore package))
  (let* ((req (%request-of this (http-resource-ct "request")))
         (tagged (and (consp path) (eq (car path) :some)))
         (raw (if tagged (cdr path) path)))
    (when (and (stringp raw) (zerop (length raw)))
      (setf raw "/" tagged t))
    (cond
      ((eq path :none) (setf (wasi-http-request-path-with-query req) :none)
       (cons :ok nil))
      ((%http-path-ok-p raw)
       (setf (wasi-http-request-path-with-query req)
             (cons :some raw))
       (cons :ok nil))
      (t (cons :err nil)))))

(defun %normalize-http-scheme (scheme)
  "other(http)/other(https) normalize to the named Scheme case; the
WIT issue behind the upstream assertion. Case-insensitive input,
canonical HTTP/HTTPS output. Any other value is unchanged."
  (if (and (consp scheme) (string-equal (car scheme) "other")
           (stringp (cdr scheme)))
      (cond ((string-equal (cdr scheme) "http") (list "HTTP"))
            ((string-equal (cdr scheme) "https") (list "HTTPS"))
            (t scheme))
      scheme))

(defun http-request-get-scheme (this &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-request-scheme
   (%request-of this (http-resource-ct "request"))))

(defun http-request-set-scheme (this scheme
                                &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((req (%request-of this (http-resource-ct "request"))))
    (cond
      ((eq scheme :none) (setf (wasi-http-request-scheme req) :none)
       (cons :ok nil))
      ((and (consp scheme) (eq (car scheme) :some))
       (let ((normalized (%normalize-http-scheme (cdr scheme))))
         (if (%http-scheme-ok-p normalized)
             (progn (setf (wasi-http-request-scheme req)
                          (cons :some normalized))
                    (cons :ok nil))
             (cons :err nil))))
      (t (let ((normalized (%normalize-http-scheme scheme)))
           (if (%http-scheme-ok-p normalized)
               (progn (setf (wasi-http-request-scheme req)
                            (cons :some normalized))
                      (cons :ok nil))
               (cons :err nil)))))))

(defun http-request-get-authority (this &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-request-authority
   (%request-of this (http-resource-ct "request"))))

(defun http-request-set-authority (this authority
                                   &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((req (%request-of this (http-resource-ct "request"))))
    (cond
      ((eq authority :none)
       (setf (wasi-http-request-authority req) :none)
       (cons :ok nil))
      ((and (consp authority) (eq (car authority) :some))
       (if (%http-authority-ok-p (cdr authority))
           (progn (setf (wasi-http-request-authority req) authority)
                  (cons :ok nil))
           (cons :err nil)))
      (t (if (%http-authority-ok-p authority)
             (progn (setf (wasi-http-request-authority req)
                          (cons :some authority))
                    (cons :ok nil))
             (cons :err nil))))))

(defun http-request-get-options (this &key (package (wasi-package "http")))
  (declare (ignore package))
  (let* ((req (%request-of this (http-resource-ct "request")))
         (opts (wasi-http-request-options req)))
    (if opts
        (list :own-handle
              (http-resource-ct "request-options")
              (make-wasi-http-options
               :connect-timeout (wasi-http-options-connect-timeout opts)
               :first-byte-timeout
               (wasi-http-options-first-byte-timeout opts)
               :between-bytes-timeout
               (wasi-http-options-between-bytes-timeout opts)
               :mutable-p nil))
        :none)))

(defun http-request-get-headers (this &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((req (%request-of this (http-resource-ct "request"))))
    (%fields-immutable-view (wasi-http-request-headers req))))

(defun http-request-consume-body (this res &key (package (wasi-package "http")))
  "request.consume-body: serve the request's captured body bytes as a
stream and resolve the trailers future at EOF. (The provider-built
INCOMING request side arrives with the handler world.)"
  (declare (ignore package))
  (let* ((request-ct (http-resource-ct "request"))
         (req (%http-take-resource this request-ct))
         (elem-ct (%http-stream-ct "request" "consume-body"))
         (future-ct (%http-future-ct "request" "consume-body"))
         (bytes (apply #'append
                       (reverse (wasi-http-request-body-buffer
                                 (%request-of req request-ct))))))
    (setf (wasi-http-request-consumed-p (%request-of req request-ct)) t)
    (multiple-value-bind (stream-reader stream-writer)
        (wasmdown.component.runtime:make-host-stream-pair elem-ct)
      (multiple-value-bind (future-reader future-writer)
          (wasmdown.component.runtime:make-host-future-pair future-ct)
        (let ((pos 0)
              (settled nil))
          (flet ((settle (value)
                   (unless settled
                     (setf settled t)
                     (wasmdown.component.runtime:host-future-resolve
                      future-writer value))))
            (wasmdown.component.runtime:host-end-set-source
             stream-writer
             (lambda (max)
               (if (>= pos (length bytes))
                   (values nil t)
                   (let ((k (min max (- (length bytes) pos))))
                     (prog1 (values (subseq bytes pos (+ pos k)) nil)
                       (incf pos k))))))
            (wasmdown.component.runtime:host-end-set-on-drop
             stream-writer
             (lambda (dropped error)
               (declare (ignore dropped error))
               (multiple-value-bind (value present)
                   (%http-future-value res)
                 (if (and present (consp value) (eq (car value) :err))
                     (settle value)
                     (settle (cons :ok :none))))))
            (list stream-reader future-reader)))))))

;;; ---------------------------------------------------------------
;;; The response resource.

(defstruct wasi-http-response
  (status 200)
  (headers nil)
  (body nil)                          ; wasi-http-body
  (consumed-p nil)
  (transmission-writer nil))

(defun %response-of (value ct)
  (let ((r (cond ((wasi-http-response-p value) value)
                 ((integerp value)
                  (wasmdown.component.runtime:host-resource-rep ct value)))))
    (unless (wasi-http-response-p r)
      (error 'wasmdown:host-error :message "response: invalid handle"))
    r))

(defun http-response-new (headers contents trailers
                          &key (package (wasi-package "http")))
  "response.new: the provider-side constructor (the handler world's
direction); the body bytes supplied through CONTENTS are buffered and
a transmission future reports completion."
  (let* ((fields-ct (http-resource-ct "fields" :package package))
         (response-ct (http-resource-ct "response" :package package))
         (future-ct (%http-future-ct "response" "new" :package package))
         (h (%http-take-resource headers fields-ct)))
    (unless (wasi-fields-p h)
      (error 'wasmdown:host-error
             :message "response.new: headers is not a fields resource"))
    (let ((resp (make-wasi-http-response :headers h))
          (body (%http-end-of contents)))
      (declare (ignore trailers))
      (multiple-value-bind (future-reader future-writer)
          (wasmdown.component.runtime:make-host-future-pair future-ct)
        (setf (wasi-http-response-transmission-writer resp) future-writer)
        (if body
            (progn
              (wasmdown.component.runtime:host-end-set-sink
               body
               (lambda (octets start end)
                 (let ((state (or (wasi-http-response-body resp)
                                  (make-wasi-http-body))))
                   (setf (wasi-http-response-body resp) state)
                   (setf (wasi-http-body-buffer state)
                         (append (wasi-http-body-buffer state)
                                 (coerce (subseq octets start end) 'list)))
                   (values (- end start) nil))))
              (wasmdown.component.runtime:host-end-set-on-drop
               body
               (lambda (dropped error)
                 (declare (ignore dropped error))
                 (let ((w (wasi-http-response-transmission-writer resp)))
                   (when w
                     (setf (wasi-http-response-transmission-writer resp) nil)
                     (wasmdown.component.runtime:host-future-resolve
                      w (cons :ok nil))))))
              (wasmdown.component.runtime:host-end-drain body))
            (wasmdown.component.runtime:host-future-resolve
             future-writer (cons :ok nil)))
        (list (list :own-handle response-ct resp) future-reader)))))

(defun http-response-get-status-code (this
                                      &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-response-status
   (%response-of this (http-resource-ct "response"))))

(defun http-response-set-status-code (this status
                                      &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((resp (%response-of this (http-resource-ct "response"))))
    (if (and (integerp status) (<= 100 status 599))
        (progn (setf (wasi-http-response-status resp) status) (cons :ok nil))
        (cons :err nil))))

(defun http-response-get-headers (this &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((resp (%response-of this (http-resource-ct "response"))))
    (%fields-immutable-view (wasi-http-response-headers resp))))

(defun http-response-consume-body (this res
                                   &key (package (wasi-package "http")))
  (declare (ignore package))
  (let* ((response-ct (http-resource-ct "response"))
         (resp (%response-of (%http-take-resource this response-ct)
                             response-ct))
         (elem-ct (%http-stream-ct "response" "consume-body"))
         (future-ct (%http-future-ct "response" "consume-body"))
         (body (wasi-http-response-body resp)))
    (setf (wasi-http-response-consumed-p resp) t)
    (multiple-value-bind (stream-reader stream-writer)
        (wasmdown.component.runtime:make-host-stream-pair elem-ct)
      (multiple-value-bind (future-reader future-writer)
          (wasmdown.component.runtime:make-host-future-pair future-ct)
        (let ((settled nil))
          (flet ((settle (value)
                   (unless settled
                     (setf settled t)
                     (wasmdown.component.runtime:host-future-resolve
                      future-writer value))))
            ;; the guest's `res' future: a delivered error wins
            (multiple-value-bind (value present)
                (%http-future-value res)
              (when (and present body (consp value) (eq (car value) :err))
                (setf (wasi-http-body-error body) (cdr value))))
            (when body
              (wasmdown.component.runtime:host-end-set-source
               stream-writer (%http-body-source body)))
            (wasmdown.component.runtime:host-end-set-on-drop
             stream-writer
             (lambda (dropped error)
               (declare (ignore dropped))
               (let ((code (or error (and body (wasi-http-body-error body)))))
                 (cond
                   (code (settle (cons :err code)))
                   ((and body (wasi-http-body-trailers body))
                    (settle
                     (cons :ok
                           (cons :some
                                 (list :own-handle
                                       (http-resource-ct "fields")
                                       (wasi-http-body-trailers body))))))
                   (t (settle (cons :ok :none)))))
               (%http-body-close body)))
            (when (and body (eq (wasi-http-body-mode body) :none))
              (wasmdown.component.runtime:host-end-drop stream-writer))
            (list stream-reader future-reader)))))))

;;; ---------------------------------------------------------------
;;; The pluggable transport.

(defstruct wasi-http-connection
  "A duplex byte channel to one HTTP server. READ-FN: (max timeout-ms)
-> (values byte-list status) with status :ok/:eof/:timeout or a
(:error . error-code) cons. WRITE-FN: (byte-list) -> error-code or
NIL. CLOSE-FN: () -> ignored."
  (fd -1)
  (read-fn nil)
  (write-fn nil)
  (close-fn nil)
  (closed-p nil))

(defstruct wasi-http-transport
  "An embedder-supplied HTTP transport (PLAN §9.2). SCHEME is matched
case-insensitively against the request's scheme; CONNECT is
(host port destination timeout-ms) -> wasi-http-connection, where
DESTINATION is (family address-bytes port) already cleared by the
network capability. The built-in plain-TCP transport serves http;
register a TLS transport for https through the CONTEXT's
http-transports — this file never implements TLS itself."
  (scheme "https")
  (tls-p t)
  (connect nil))

(defun %http-transport-for (context scheme)
  "The transport for SCHEME: the built-in plain TCP for http, else one
of the CONTEXT's registered transports (case-insensitive). NIL when
none is registered — the caller turns that into configuration-error."
  (cond
    ((string-equal scheme "http") (%http-plain-transport))
    (t
     (find-if (lambda (tr)
                (and (string-equal (wasi-http-transport-scheme tr) scheme)
                     (wasi-http-transport-connect tr)))
              (wasi-context-http-transports context)))))

(defparameter +http-default-connect-timeout-ms+ 30000)
(defparameter +http-default-first-byte-timeout-ms+ 30000)
(defparameter +http-default-between-bytes-timeout-ms+ 30000)

;;; errno -> the WIT's error-code cases.
(defparameter *http-errno->code*
  '((111 . "connection-refused")        ; ECONNREFUSED
    (110 . "connection-timeout")        ; ETIMEDOUT
    (104 . "connection-terminated")     ; ECONNRESET
    (32 . "connection-terminated")      ; EPIPE
    (113 . "destination-unavailable")   ; EHOSTUNREACH
    (101 . "destination-IP-unroutable") ; ENETUNREACH
    (13 . "HTTP-request-denied")        ; EACCES
    (22 . "configuration-error")))      ; EINVAL

(defun %http-errno-code (errno)
  (cons (or (cdr (assoc errno *http-errno->code*)) "internal-error")
        nil))

(defun %http-connect-fd (fd alien len timeout-ms)
  "connect(2) with the EINPROGRESS completion wait bounded by
TIMEOUT-MS (0 = the plain %connect-fd's 5 s)."
  (sb-alien:set-errno 0)
  (let ((ret (%libc-connect fd (sb-alien:alien-sap alien) len)))
    (cond
      ((zerop ret) t)
      ((= (sb-alien:get-errno) +einprogress+)
       (unless (%poll-fd fd +pollout+
                         (if (and timeout-ms (plusp timeout-ms))
                             timeout-ms
                             5000))
         (%http-error (%http-code "connection-timeout")))
       (let ((err (%getsockopt-int fd +sol-socket+ +so-error+)))
         (unless (zerop err)
           (%http-error (%http-errno-code err)))))
      (t (%http-error (%http-errno-code (sb-alien:get-errno)))))))

(defun %http-plain-read (fd max timeout-ms)
  "One recv(2) bounded by POLLIN/TIMEOUT-MS."
  (let ((buf (sb-alien:make-alien sb-alien:unsigned-char max)))
    (unwind-protect
         (loop
           (sb-alien:set-errno 0)
           (let ((n (%libc-recv fd (sb-alien:alien-sap buf) max 0)))
             (cond
               ((plusp n)
                (return (values (loop for i below n
                                      collect (sb-sys:sap-ref-8
                                               (sb-alien:alien-sap buf) i))
                                :ok)))
               ((zerop n) (return (values nil :eof)))
               (t
                (let ((errno (sb-alien:get-errno)))
                  (cond
                    ((= errno +eintr+) nil)
                    ((= errno +eagain+)
                     (unless (%poll-fd fd +pollin+
                                       (if timeout-ms timeout-ms 30000))
                       (return (values nil :timeout))))
                    (t (return (values nil (%http-errno-code errno))))))))))
      (sb-alien:free-alien buf))))

(defun %http-plain-write (fd bytes)
  (let ((n (length bytes)))
    (if (zerop n)
        nil
        (let ((buf (%byte-list->alien bytes)))
          (unwind-protect
               (block write
                 (let ((done 0))
                   (loop while (< done n)
                         do (let* ((ptr (sb-sys:sap+ (sb-alien:alien-sap buf)
                                                     done))
                                   (ret (%libc-send fd ptr (- n done)
                                                    +msg-nosignal+)))
                              (cond
                                ((plusp ret) (incf done ret))
                                ((zerop ret)
                                 (return-from write (%http-code
                                                     "connection-terminated")))
                                (t
                                 (let ((errno (sb-alien:get-errno)))
                                   (cond
                                     ((= errno +eintr+) nil)
                                     ((= errno +eagain+)
                                      (unless (%poll-fd fd +pollout+ 30000)
                                        (return-from write
                                          (%http-code
                                           "connection-write-timeout"))))
                                     (t (return-from write
                                          (%http-errno-code errno)))))))))
                   nil))
            (sb-alien:free-alien buf))))))

(defun %http-plain-connect (host port destination timeout-ms)
  "The built-in plain-TCP transport. DESTINATION is
(family address-bytes port) already checked against the network
policy."
  (declare (ignore host port))
  (destructuring-bind (family address port) destination
    (multiple-value-bind (alien len)
        (%make-sockaddr
         (ecase family (:ipv4 "ipv4") (:ipv6 "ipv6"))
         (list (cons "port" port) (cons "address" address)))
      (unwind-protect
           (let ((fd (%libc-socket
                      (%family-key family)
                      (logior +sock-stream+ +sock-nonblock+ +sock-cloexec+)
                      +ipproto-tcp+)))
             (when (minusp fd)
               (%http-error (%http-errno-code (sb-alien:get-errno))))
             (handler-case
                 (progn
                   (%http-connect-fd fd alien len timeout-ms)
                   (make-wasi-http-connection
                    :fd fd
                    :read-fn (lambda (max to) (%http-plain-read fd max to))
                    :write-fn (lambda (bytes) (%http-plain-write fd bytes))
                    :close-fn (lambda () (%close-fd fd))))
               (error (c)
                 (%close-fd fd)
                 (error c))))
        (sb-alien:free-alien alien)))))

(defvar *http-plain-transport* nil)

(defun %http-plain-transport ()
  "The ONE plain-TCP transport (https never falls back to it)."
  (or *http-plain-transport*
      (setf *http-plain-transport*
            (make-wasi-http-transport
             :scheme "http" :tls-p nil
             :connect #'%http-plain-connect))))

(defun %http-connection-close (conn)
  (when (and conn (not (wasi-http-connection-closed-p conn)))
    (setf (wasi-http-connection-closed-p conn) t)
    (when (wasi-http-connection-close-fn conn)
      (ignore-errors (funcall (wasi-http-connection-close-fn conn))))))

;;; ---------------------------------------------------------------
;;; The network capability's view of the destination.

(defun %http-resolve-destination (context network host port)
  "Resolve HOST to (family address-bytes port), consulting the
network's DNS policy for a name and its OUTBOUND policy for the
resolved address. Signals wasi-http-error with the WIT's cases."
  (let* ((literal
           (cond
             ((%parse-ipv4-literal host)
              (list (cons "ipv4" (%parse-ipv4-literal host))))
             ((find #\: host)
              (handler-case
                  (%getaddrinfo-chain host +ai-numerichost+)
                (wasi-socket-error ()
                  (%http-error (%http-code "HTTP-request-URI-invalid")))))
             (t
              (unless (wasi-network-allows-dns-p network host)
                (%http-error (%http-code "HTTP-request-denied")))
              (handler-case (%getaddrinfo-chain host 0)
                (wasi-socket-error ()
                  (%http-error (%http-code "destination-not-found")))))))
         (candidates
           (loop for value in literal
                 for family = (if (string= (car value) "ipv4") :ipv4 :ipv6)
                 collect (list family (cdr value) port))))
    (or (find-if (lambda (c)
                   (wasi-network-allows-outbound-p
                    network (first c) (second c) port))
                 candidates)
        (%http-error (%http-code "destination-IP-prohibited")))))

;;; ---------------------------------------------------------------
;;; The HTTP/1.1 reader and parser.

(defstruct wasi-http-reader
  "A buffered reader over a connection."
  (conn nil)
  (buf (make-array 0 :element-type '(unsigned-byte 8)
                     :adjustable t :fill-pointer 0))
  (pos 0)
  (eof nil)
  (timeout-ms +http-default-first-byte-timeout-ms+))

(defun %hr-available (r)
  (- (fill-pointer (wasi-http-reader-buf r)) (wasi-http-reader-pos r)))

(defun %hr-compact (r)
  (let* ((buf (wasi-http-reader-buf r))
         (pos (wasi-http-reader-pos r))
         (n (- (fill-pointer buf) pos)))
    (when (plusp pos)
      (replace buf buf :start2 pos :end2 (fill-pointer buf))
      (setf (fill-pointer buf) n
            (wasi-http-reader-pos r) 0))))

(defun %hr-fill (r want timeout-ms)
  "Ensure at least WANT buffered bytes: :ok, :eof, :timeout, or a
(:error . code) cons."
  (loop
    (when (>= (%hr-available r) want) (return :ok))
    (when (wasi-http-reader-eof r) (return :eof))
    (%hr-compact r)
    (multiple-value-bind (bytes status)
        (funcall (wasi-http-connection-read-fn (wasi-http-reader-conn r))
                 65536
                 (or timeout-ms (wasi-http-reader-timeout-ms r)))
      (cond
        ((eq status :ok)
         (if (null bytes)
             (return :eof)
             (dolist (b bytes)
               (vector-push-extend b (wasi-http-reader-buf r)))))
        ((eq status :eof)
         (setf (wasi-http-reader-eof r) t)
         (return :eof))
        ((eq status :timeout) (return :timeout))
        (t (return status))))))

(defun %hr-take (r n timeout-ms)
  "Exactly N bytes as a list, or a wasi-http-error."
  (let ((st (%hr-fill r n timeout-ms)))
    (cond
      ((eq st :ok)
       (let ((buf (wasi-http-reader-buf r))
             (pos (wasi-http-reader-pos r)))
         (prog1 (loop for i below n collect (aref buf (+ pos i)))
           (setf (wasi-http-reader-pos r) (+ pos n)))))
      ((eq st :eof)
       (%http-error (%http-code "HTTP-response-incomplete")))
      ((eq st :timeout)
       (%http-error (%http-code "HTTP-response-timeout")))
      (t (%http-error st)))))

(defun %hr-take-byte (r timeout-ms)
  (let ((st (%hr-fill r 1 timeout-ms)))
    (cond
      ((eq st :ok)
       (let ((buf (wasi-http-reader-buf r))
             (pos (wasi-http-reader-pos r)))
         (prog1 (aref buf pos)
           (setf (wasi-http-reader-pos r) (1+ pos)))))
      ((eq st :eof) :eof)
      ((eq st :timeout) :timeout)
      (t st))))

(defun %hr-take-upto (r n timeout-ms)
  "At most N bytes; (values bytes eof-p)."
  (if (zerop n)
      (values nil nil)
      (let ((st (%hr-fill r 1 timeout-ms)))
        (cond
          ((eq st :ok)
           (let ((k (min n (%hr-available r))))
             (values (%hr-take r k timeout-ms) nil)))
          ((eq st :eof) (values nil t))
          ((eq st :timeout)
           (%http-error (%http-code "HTTP-response-timeout")))
          (t (%http-error st))))))

(defun %http-bytes->string (bytes)
  (map 'string #'code-char bytes))

(defun %hr-line (r &key (max 8192) (timeout-ms nil))
  "One CRLF-terminated line (without the CRLF) as a latin-1 string;
:eof only when the stream ends with no bytes; anything else is
HTTP-response-incomplete / HTTP-protocol-error."
  (let ((bytes '())
        (cr nil))
    (loop
      (let ((b (%hr-take-byte r timeout-ms)))
        (cond
          ((eq b :eof)
           (if (null bytes)
               (return :eof)
               (%http-error (%http-code "HTTP-response-incomplete"))))
          ((eq b :timeout)
           (%http-error (%http-code "HTTP-response-timeout")))
          ((consp b) (%http-error b))
          (cr
           (unless (= b 10)
             (%http-error (%http-code "HTTP-protocol-error")))
           (return (%http-bytes->string (nreverse bytes))))
          ((= b 13) (setf cr t))
          (t
           (push b bytes)
           (when (> (length bytes) max)
             (%http-error (%http-code "HTTP-response-header-section-size")))))))))

(defun %http-parse-status-line (line)
  "HTTP/1.x <code> [reason] -> the status code."
  (unless (and (stringp line)
               (>= (length line) 12)
               (string= (subseq line 0 7) "HTTP/1."))
    (%http-error (%http-code "HTTP-protocol-error")))
  (let ((space (position #\Space line)))
    (unless (and space (>= (- (length line) space) 4))
      (%http-error (%http-code "HTTP-protocol-error")))
    (let ((digits (subseq line (1+ space) (+ space 4))))
      (unless (every #'digit-char-p digits)
        (%http-error (%http-code "HTTP-protocol-error")))
      (let ((code (parse-integer digits)))
        (unless (<= 100 code 599)
          (%http-error (%http-code "HTTP-protocol-error")))
        code))))

(defparameter +http-max-header-section-size+ (* 64 1024))
(defparameter +http-max-header-count+ 128)

(defun %http-read-fields (r &key (timeout-ms nil))
  "Parse header/trailer lines into an IMMUTABLE fields resource."
  (let ((entries '())
        (total 0)
        (count 0))
    (loop
      (let ((line (%hr-line r :timeout-ms timeout-ms)))
        (when (eq line :eof)
          (%http-error (%http-code "HTTP-response-incomplete")))
        (when (zerop (length line))
          (return))
        (incf total (+ (length line) 2))
        (incf count)
        (when (> total +http-max-header-section-size+)
          (%http-error (%http-code "HTTP-response-header-section-size")))
        (when (> count +http-max-header-count+)
          (%http-error (%http-code "HTTP-response-header-section-size")))
        (when (or (char= (char line 0) #\Space)
                  (char= (char line 0) #\Tab))
          (%http-error (%http-code "HTTP-protocol-error")))
        (let ((colon (position #\: line)))
          (unless colon
            (%http-error (%http-code "HTTP-protocol-error")))
          (let* ((name (subseq line 0 colon))
                 (value (string-trim '(#\Space #\Tab)
                                     (subseq line (1+ colon)))))
            (unless (%http-token-p name)
              (%http-error (%http-code "HTTP-protocol-error")))
            (push (cons name (map 'list #'char-code value)) entries)))))
    (make-wasi-fields :entries (nreverse entries) :mutable-p nil)))

(defun %http-read-response-head (r method)
  "Parse the status line + header section. Returns (values status
fields)."
  (declare (ignore method))
  (let ((line (%hr-line r)))
    (when (eq line :eof)
      (%http-error (%http-code "HTTP-response-incomplete")))
    (let ((status (%http-parse-status-line line)))
      (values status (%http-read-fields r)))))

(defun %http-te-chunked-p (fields)
  "The last Transfer-Encoding token is chunked; a TE that is not
chunked is the WIT's HTTP-response-transfer-coding."
  (let* ((raw (%fields-values fields "transfer-encoding"))
         (values (loop for v in raw
                       append (mapcar (lambda (s)
                                        (string-downcase
                                         (string-trim '(#\Space #\Tab) s)))
                                      (%split-string (%http-bytes->string v)
                                                     #\,))))
         (last (car (last values))))
    (cond
      ((null values) nil)
      ((string= last "chunked") t)
      (t (%http-error
          (%http-code-payload
           "HTTP-response-transfer-coding"
           (cons :some (%http-bytes->string (car (last raw))))))))))

(defun %http-content-length (fields)
  (let ((values (%fields-values fields "content-length")))
    (when (null values) (return-from %http-content-length nil))
    (let ((parsed
            (mapcar (lambda (v)
                      (let ((s (string-trim '(#\Space #\Tab)
                                            (%http-bytes->string v))))
                        (unless (and (plusp (length s))
                                     (every #'digit-char-p s))
                          (%http-error (%http-code "HTTP-protocol-error")))
                        (parse-integer s)))
                    values)))
      (unless (every (lambda (n) (= n (first parsed))) parsed)
        (%http-error (%http-code "HTTP-protocol-error")))
      (first parsed))))

(defun %http-response-body-mode (status fields method)
  "(values mode (length or NIL))."
  (cond
    ((or (eq method :head)
         (and (>= status 100) (< status 200))
         (member status '(204 304)))
     (values :none 0))
    ((%http-te-chunked-p fields) (values :chunked nil))
    ((%http-content-length fields)
     (values :length (%http-content-length fields)))
    (t (values :close nil))))

;;; ---------------------------------------------------------------
;;; The response body state + the lazy host source.

(defstruct wasi-http-body
  (reader nil)
  (mode :none)
  (remaining 0)
  (chunk-state :size)
  (chunk-remaining 0)
  (trailers nil)
  (done-p nil)
  (error nil)
  (first-byte-p t)
  (first-timeout +http-default-first-byte-timeout-ms+)
  (between-timeout +http-default-between-bytes-timeout-ms+)
  (buffer nil))                        ; response.new's buffered bytes

(defun %http-body-source (body)
  (lambda (max)
    (if (or (<= max 0) (wasi-http-body-done-p body))
        (values nil t)
        (let ((timeout (if (wasi-http-body-first-byte-p body)
                           (wasi-http-body-first-timeout body)
                           (wasi-http-body-between-timeout body))))
          (setf (wasi-http-body-first-byte-p body) nil)
          (handler-case
              (%http-body-read body max timeout)
            (wasi-http-error (c)
              (setf (wasi-http-body-error body) (wasi-http-error-code c)
                    (wasi-http-body-done-p body) t)
              (values nil t)))))))

(defun %http-body-read (body max timeout)
  (let ((reader (wasi-http-body-reader body)))
    (case (wasi-http-body-mode body)
      (:none
       (setf (wasi-http-body-done-p body) t)
       (values nil t))
      (:length
       (let ((remaining (wasi-http-body-remaining body)))
         (when (zerop remaining)
           (setf (wasi-http-body-done-p body) t)
           (return-from %http-body-read (values nil t)))
         (multiple-value-bind (bytes eof)
             (%hr-take-upto reader (min max remaining) timeout)
           (cond
             ((plusp (length bytes))
              (decf (wasi-http-body-remaining body) (length bytes))
              (when (zerop (wasi-http-body-remaining body))
                (setf (wasi-http-body-done-p body) t))
              (values bytes nil))
             (eof
              (%http-error (%http-code "HTTP-response-incomplete")))
             (t (values nil nil))))))
      (:chunked (%http-chunked-read body max timeout))
      (:close
       (multiple-value-bind (bytes eof)
           (%hr-take-upto reader max timeout)
         (when eof (setf (wasi-http-body-done-p body) t))
         (values bytes eof))))))

(defun %http-chunked-read (body max timeout)
  (let ((reader (wasi-http-body-reader body)))
    (loop
      (case (wasi-http-body-chunk-state body)
        (:size
         (let ((line (%hr-line reader :timeout-ms timeout)))
           (when (eq line :eof)
             (%http-error (%http-code "HTTP-response-incomplete")))
           (let* ((semi (position #\; line))
                  (hex (string-trim '(#\Space #\Tab)
                                    (if semi (subseq line 0 semi) line))))
             (unless (and (plusp (length hex))
                          (every (lambda (c) (digit-char-p c 16)) hex))
               (%http-error (%http-code "HTTP-protocol-error")))
             (let ((n (parse-integer hex :radix 16)))
               (if (zerop n)
                   (let ((trailers (%http-read-fields reader
                                                      :timeout-ms timeout)))
                     ;; HTTP's empty trailer section is the WIT's
                     ;; `none': only a real trailer field is exposed.
                     (setf (wasi-http-body-trailers body)
                           (and (wasi-fields-entries trailers) trailers)
                           (wasi-http-body-chunk-state body) :done
                           (wasi-http-body-done-p body) t)
                     (return (values nil t)))
                   (setf (wasi-http-body-chunk-remaining body) n
                         (wasi-http-body-chunk-state body) :data))))))
        (:data
         (let ((n (min max (wasi-http-body-chunk-remaining body))))
           (multiple-value-bind (bytes eof)
               (%hr-take-upto reader n timeout)
             (cond
               ((plusp (length bytes))
                (decf (wasi-http-body-chunk-remaining body) (length bytes))
                (when (zerop (wasi-http-body-chunk-remaining body))
                  (setf (wasi-http-body-chunk-state body) :data-crlf))
                (return (values bytes nil)))
               (eof (%http-error (%http-code "HTTP-response-incomplete")))
               (t (return (values nil nil)))))))
        (:data-crlf
         (let ((crlf (%hr-take reader 2 timeout)))
           (unless (and (= (first crlf) 13) (= (second crlf) 10))
             (%http-error (%http-code "HTTP-protocol-error")))
           (setf (wasi-http-body-chunk-state body) :size)))
        (:done (return (values nil t)))))))

(defun %http-body-close (body)
  (when body
    (let ((reader (wasi-http-body-reader body)))
      (when (and reader (wasi-http-reader-conn reader))
        (%http-connection-close (wasi-http-reader-conn reader))))))

;;; ---------------------------------------------------------------
;;; client.send.

(defun %http-header-bytes (fields name value)
  (let ((name-bytes (map 'list #'char-code name)))
    (when (or (not (%http-token-p name))
              (find 10 (append name-bytes value))
              (find 13 (append name-bytes value)))
      (%http-error (%http-code "HTTP-protocol-error")))))

(defun %http-serialize-request-head (method path authority fields extra)
  "The request line + every header line, as a byte list. EXTRA is a
list of pre-built \"Name: value\" strings (the implementation's own
Host/Content-Length/Transfer-Encoding/Connection)."
  (let ((out (make-string-output-stream)))
    (format out "~a ~a HTTP/1.1~c~c" method path #\Return #\Newline)
    (dolist (line extra)
      (format out "~a~c~c" line #\Return #\Newline))
    (dolist (entry (wasi-fields-entries fields))
      (%http-header-bytes fields (car entry) (cdr entry))
      (format out "~a: ~a~c~c"
              (car entry) (%http-bytes->string (cdr entry))
              #\Return #\Newline))
    (format out "~c~c" #\Return #\Newline)
    (map 'list #'char-code (get-output-stream-string out))))

(defun %http-timeout-ms (options slot default)
  (if (null options)
      default
      (let ((v (funcall slot options)))
        (cond
          ((eq v :none) default)
          ((and (consp v) (eq (car v) :some) (integerp (cdr v)))
           (max 1 (ceiling (cdr v) 1000000)))
          ((integerp v) (max 1 (ceiling v 1000000)))
          (t default)))))

(defun %http-request-body-bytes (req)
  (apply #'append (reverse (wasi-http-request-body-buffer req))))

(defun %http-request-body-take (req)
  "Wait for request body progress and drain every staged byte.
Returns (values bytes closed-p error-code). CALLER: the send worker.
The guest's sink appends under the same lock, so the wait neither
loses a wake nor keeps the guest from writing."
  (sb-thread:with-recursive-lock ((wasi-http-request-body-mutex req))
    (loop while (and (null (wasi-http-request-body-buffer req))
                     (not (wasi-http-request-body-closed-p req))
                     (not (wasi-http-request-body-error req)))
          do (sb-thread:condition-wait
              (wasi-http-request-body-cond req)
              (wasi-http-request-body-mutex req)))
    (let ((bytes (%http-request-body-bytes req)))
      (setf (wasi-http-request-body-buffer req) nil)
      (values bytes
              (wasi-http-request-body-closed-p req)
              (wasi-http-request-body-error req)))))

(defun %http-request-body-wait-closed (req)
  "Wait until the guest closes the body, then its final length (the
Content-Length the request head needs when the guest declared none)."
  (sb-thread:with-recursive-lock ((wasi-http-request-body-mutex req))
    (loop while (and (not (wasi-http-request-body-closed-p req))
                     (not (wasi-http-request-body-error req)))
          do (sb-thread:condition-wait
              (wasi-http-request-body-cond req)
              (wasi-http-request-body-mutex req)))
    (let ((error (wasi-http-request-body-error req)))
      (when error (%http-error error))
      (wasi-http-request-body-length req))))

(defun %http-trailer-fields (handle req)
  "The fields rep behind a trailer future's guest-written
option<own<fields>> payload. The canonical copy of a NESTED own
leaves the raw handle index in the value, so transfer it out of the
guest's table here (once)."
  (cond
    ((wasi-fields-p handle) handle)
    ((and (consp handle) (eq (car handle) :own-handle)) (third handle))
    ((integerp handle)
     (let* ((ct (http-resource-ct "fields"))
            (inst (wasi-http-request-trailers-inst req))
            (wasmdown.component.runtime::*handle-caller-inst* inst)
            (wasmdown.component.runtime::*host-provider-inst* nil))
       (multiple-value-bind (rep found)
           (wasmdown.component.runtime:host-resource-take ct handle)
         (declare (ignore found))
         (and (wasi-fields-p rep) rep))))
    (t nil)))

(defun %http-trailers-value (req value)
  "Parse the trailers future's domain VALUE: (values trailers-or-nil
error-code-or-nil). A dropped future is `none' (the WIT permits a
request without trailers)."
  (cond
    ((null value) (values nil nil))
    ((and (consp value) (eq (car value) :err))
     (values nil (cdr value)))
    ((and (consp value) (eq (car value) :ok))
     (let ((opt (cdr value)))
       (cond
         ((eq opt :none) (values nil nil))
         ((and (consp opt) (eq (car opt) :some))
          (values (%http-trailer-fields (cdr opt) req) nil))
         (t (values nil nil)))))
    ((wasi-fields-p value) (values value nil))
    (t (values nil nil))))

(defun %http-request-trailers (req)
  "Wait for the request's trailers future to resolve:
(values trailers-or-nil present-p), signalling WASI-HTTP-ERROR for a
trailers error. PRESENT-P means the future resolved (Some or None);
TRAILERS is the non-empty fields, or NIL for `none'."
  (if (or (null (wasi-http-request-trailers-end req))
          (eq (wasi-http-request-trailers-state req) :absent))
      (values nil nil)
      (let ((value nil))
        (sb-thread:with-recursive-lock
            ((wasi-http-request-body-mutex req))
          (loop while (eq (wasi-http-request-trailers-state req) :waiting)
                do (sb-thread:condition-wait
                    (wasi-http-request-body-cond req)
                    (wasi-http-request-body-mutex req)))
          (setf value (wasi-http-request-trailers-value req)))
        (multiple-value-bind (trailers error)
            (%http-trailers-value req value)
          (when error (%http-error error))
          (values trailers t)))))

(defun %http-chunk-prefix (length)
  (map 'list #'char-code (format nil "~x~c~c" length #\Return #\Newline)))

(defun %http-serialize-trailers (trailers)
  (append
   (loop for (name . value) in (wasi-fields-entries trailers)
         append (map 'list #'char-code
                     (format nil "~a: ~a~c~c" name
                             (%http-bytes->string value)
                             #\Return #\Newline)))
   (list 13 10)))

(defun %http-write-request (conn req trailers trailers-present)
  "Write the whole request: head, then the body bytes as the guest
produces them (Content-Length or chunked framing), then the chunked
terminator and trailer section when TRAILERS is non-NIL. Signals
wasi-http-error on an I/O, body or serialization failure."
  (let* ((method (%http-method-string (wasi-http-request-method req)))
         (authority (let ((a (wasi-http-request-authority req)))
                      (if (consp a) (cdr a) a)))
         (path (let ((p (wasi-http-request-path-with-query req)))
                 (if (and (consp p) (eq (car p) :some)) (cdr p) "/")))
         ;; chunked framing is required only when a real trailer
         ;; section will follow; `none' rides Content-Length.
         (chunked-p (and trailers-present trailers))
         (body-present (or (wasi-http-request-body-end req)
                           (wasi-http-request-body-present-p req)
                           (plusp (wasi-http-request-body-length req))))
         (declared-length (and (not chunked-p)
                               (%fields-values
                                (wasi-http-request-headers req)
                                "content-length")))
         (extra (append
                 (list (format nil "Host: ~a" authority)
                       "Connection: close")
                 (cond
                   (chunked-p (list "Transfer-Encoding: chunked"))
                   ((and body-present (not declared-length))
                    (list (format nil "Content-Length: ~d"
                                  (if (or (wasi-http-request-body-end req)
                                          (wasi-http-request-body-present-p req))
                                      (%http-request-body-wait-closed req)
                                      (wasi-http-request-body-length
                                       req)))))
                   (t '()))))
         (write (wasi-http-connection-write-fn conn)))
    (funcall write
             (%http-serialize-request-head
              method path authority (wasi-http-request-headers req) extra))
    ;; The body is drained in the framing the head announced. The wait
    ;; is on the guest writer, which the scheduler runs independently
    ;; while client.send's host call is pending.
    (if (or (wasi-http-request-body-end req)
            (wasi-http-request-body-present-p req))
        (loop
          (multiple-value-bind (bytes closed error)
              (%http-request-body-take req)
            (when (plusp (length bytes))
              (let ((payload (if chunked-p
                                 (append (%http-chunk-prefix (length bytes))
                                         bytes (list 13 10))
                                 bytes)))
                (let ((st (funcall write payload)))
                  (when st (%http-error st)))))
            (when error (%http-error error))
            (when closed (return))))
        ;; a rep with no contents stream (the direct/provider shape)
        (let ((bytes (%http-request-body-bytes req)))
          (when bytes
            (let ((st (funcall write
                               (if chunked-p
                                   (append (%http-chunk-prefix
                                            (length bytes))
                                           bytes (list 13 10))
                                   bytes))))
              (when st (%http-error st))))))
    (when chunked-p
      (let ((st (funcall write (map 'list #'char-code
                                    (format nil "0~c~c" #\Return #\Newline)))))
        (when st (%http-error st)))
      (let ((st (funcall write
                         (if trailers
                             (%http-serialize-trailers trailers)
                             ;; the terminator + an empty trailer section
                             (list 13 10)))))
        (when st (%http-error st))))
    nil))

(defun %http-guest-context-p ()
  "T when this call runs on a guest task thread: the async canon-lower
path expects a host-pending and drives its resolution. NIL for a
direct CL/host caller (the unit-test surface), which joins instead."
  (and (boundp 'wasmdown.component.runtime::*current-task*)
       (not (null (symbol-value
                   'wasmdown.component.runtime::*current-task*)))))

(defun %http-client-send-worker (req transport host port destination
                                 connect-timeout options response-ct
                                 store resolve reject)
  "The transmission worker: connect, drain the guest's body and
trailers while it writes them, transmit the request, settle the
transmission future, and read the response head. Runs outside the
guest scheduler's right; every expected failure is the WIT's
(:err . error-code)."
  (wasmdown.component.runtime:call-with-component-store
      (store :close-p nil)
    (handler-case
        (let ((conn (funcall (wasi-http-transport-connect transport)
                             host port destination connect-timeout)))
          (handler-case
              (let ((keep nil))
                (unwind-protect
                     (progn
                       ;; The guest announces request trailers with the
                       ;; HTTP `Trailer' header. Only then may the
                       ;; transmission wait for the trailers future:
                       ;; an unannounced/never-written future stays
                       ;; unresolved and must not stall the send (the
                       ;; WIT request may simply not carry trailers).
                       (multiple-value-bind (trailers trailers-present)
                           (if (%fields-values
                                (wasi-http-request-headers req) "trailer")
                               (%http-request-trailers req)
                               (values nil nil))
                         (%http-write-request conn req trailers
                                              trailers-present))
                       ;; the request is on the wire: transmission is
                       ;; complete even if the response never arrives.
                       (%http-request-settle req (cons :ok nil))
                       (let* ((method (wasi-http-request-method req))
                              (head-method
                                (if (string-equal
                                     (%http-method-string method) "HEAD")
                                    :head :other))
                              (reader
                                (make-wasi-http-reader
                                 :conn conn
                                 :timeout-ms
                                 (%http-timeout-ms
                                  options
                                  #'wasi-http-options-first-byte-timeout
                                  +http-default-first-byte-timeout-ms+))))
                         (multiple-value-bind (status fields)
                             (%http-read-response-head reader head-method)
                           (multiple-value-bind (mode length)
                               (%http-response-body-mode
                                status fields head-method)
                             (let* ((body-state
                                      (make-wasi-http-body
                                       :reader reader
                                       :mode mode
                                       :remaining (or length 0)
                                       :first-timeout
                                       (%http-timeout-ms
                                        options
                                        #'wasi-http-options-first-byte-timeout
                                        +http-default-first-byte-timeout-ms+)
                                       :between-timeout
                                       (%http-timeout-ms
                                        options
                                        #'wasi-http-options-between-bytes-timeout
                                        +http-default-between-bytes-timeout-ms+)))
                                    (response
                                      (make-wasi-http-response
                                       :status status
                                       :headers fields
                                       :body body-state)))
                               (when (eq mode :none)
                                 (%http-connection-close conn)
                                 (setf (wasi-http-body-done-p body-state) t))
                               (setf keep t)
                               (funcall resolve
                                        (cons :ok
                                              (list :own-handle
                                                    response-ct
                                                    response))))))))
                  (unless keep (%http-connection-close conn))))
            (error (c)
              (%http-connection-close conn)
              (error c))))
      (wasi-http-error (c)
        (let ((code (wasi-http-error-code c)))
          (%http-request-settle req (cons :err code))
          (funcall resolve (cons :err code))))
      (error (c)
        (%http-request-settle
         req (cons :err (%http-internal (princ-to-string c))))
        (funcall reject c)))))

(defun %http-client-send-blocking (req transport host port destination
                                   connect-timeout options package)
  "The DIRECT (non-guest) client.send: run the transmission on the
caller's thread. Only a host embedder/unit test takes this path; a
guest call returns a host-pending so its writer tasks keep running."
  (let ((response-ct (http-resource-ct "response" :package package))
        (store wasmdown.component.runtime::*current-component-store*)
        (result nil))
    (%http-client-send-worker
     req transport host port destination connect-timeout options
     response-ct store
     (lambda (value) (setf result value))
     (lambda (failure) (error failure)))
    result))

(defun %http-client-send-pending (req transport host port destination
                                  connect-timeout options package)
  "The ASYNC client.send answer: one worker thread owns the socket, so
the guest's body writer tasks keep running while the host call is
pending."
  (let ((response-ct (http-resource-ct "response" :package package))
        (store wasmdown.component.runtime::*current-component-store*))
    (wasmdown.component.runtime:make-host-pending
     :start
     (lambda (resolve reject acknowledge-cancel)
       (declare (ignore acknowledge-cancel))
       (sb-thread:make-thread
        (lambda ()
          (%http-client-send-worker
           req transport host port destination connect-timeout options
           response-ct store resolve reject))
        :name "http-client-send")
       ;; the transport timeouts bound the work; a cancel cannot
       ;; interrupt a blocking connect/send mid-call
       (lambda () nil)))))

(defun http-client-send (context request
                         &key (network nil) (package (wasi-package "http")))
  "client.send: transmit the HTTP/1.1 request over the transport and
return (:ok own<response>) or (:err . error-code). On a guest thread
the work runs in a host-pending, so the guest's body writer tasks
keep running while the request is drained and written."
  (let* ((net (or network (first (wasi-context-networks context))))
         (request-ct (http-resource-ct "request" :package package))
         (req nil))
    (handler-case
        (progn
          (setf req (%request-of (%http-take-resource request request-ct)
                                 request-ct))
          (let* ((scheme (%http-scheme-string
                          (wasi-http-request-scheme req)))
                 (authority (wasi-http-request-authority req)))
            (when (wasi-http-request-consumed-p req)
              (%http-error (%http-internal "request already consumed")))
            (when (eq authority :none)
              (%http-error (%http-code "HTTP-request-URI-invalid")))
            (let ((authority-string (if (consp authority)
                                        (cdr authority) authority)))
              (unless (%http-authority-ok-p authority-string)
                (%http-error (%http-code "HTTP-request-URI-invalid")))
              (multiple-value-bind (host port)
                  (%http-split-authority authority-string)
                (when (and (string-equal scheme "https") (= port 80))
                  (setf port 443))
                (let* ((transport (%http-transport-for context scheme))
                       (options (wasi-http-request-options req))
                       (connect-timeout
                         (%http-timeout-ms
                          options #'wasi-http-options-connect-timeout
                          +http-default-connect-timeout-ms+)))
                  (unless transport
                    (%http-error (%http-code "configuration-error")))
                  (let ((destination
                          (%http-resolve-destination context net
                                                     host port)))
                    (if (%http-guest-context-p)
                        (%http-client-send-pending
                         req transport host port destination
                         connect-timeout options package)
                        (%http-client-send-blocking
                         req transport host port destination
                         connect-timeout options package))))))))
      (wasi-http-error (c)
        (let ((code (wasi-http-error-code c)))
          (when req (%http-request-settle req (cons :err code)))
          (cons :err code)))
      (error (c)
        (when req
          (%http-request-settle
           req (cons :err (%http-internal (princ-to-string c)))))
        (error c)))))

;;; ---------------------------------------------------------------
;;; The provider instances.

(defun %http-register-dtors (inst &key (package (wasi-package "http")))
  "The request/response resources hold stream ends and OS sockets;
canon resource.drop runs these host finalizers. Each finalizer belongs
to THIS provider instance (never a ct-keyed global): two http types
providers never share a release hook."
  (let ((rt (http-resource-ct "request" :package package))
        (st (http-resource-ct "response" :package package)))
    (wasmdown.component.runtime:host-register-resource-dtor
     inst rt
     (lambda (rep)
       (when (wasi-http-request-p rep)
         (when (wasi-http-request-body-end rep)
           (ignore-errors
             (wasmdown.component.runtime:host-end-drop
              (wasi-http-request-body-end rep)))
           (setf (wasi-http-request-body-end rep) nil))
         ;; a request dropped without a successful transmission must
         ;; never report success (http-client-sent's contract).
         (%http-request-settle
          rep
          (cons :err (%http-internal
                      "request dropped before transmission"))))))
    (wasmdown.component.runtime:host-register-resource-dtor
     inst st
     (lambda (rep)
       (when (wasi-http-response-p rep)
         (%http-body-close (wasi-http-response-body rep)))))
    inst))

(defun http-types-instance (context
                            &key (package (wasi-package "http")))
  "The wasi:http/types import: the fields/request/request-options/
response resources."
  (declare (ignore context))
  (let ((inst
          (provider-instance
           (wasmdown.wit:package-interface package "types")
           (list
    (cons "[constructor]fields" (lambda () (fields-constructor
                                            :package package)))
    (cons "[static]fields.from-list"
          (lambda (entries) (fields-from-list entries)))
    (cons "[method]fields.get"
          (lambda (self name) (fields-get self name :package package)))
    (cons "[method]fields.has"
          (lambda (self name) (fields-has self name :package package)))
    (cons "[method]fields.set"
          (lambda (self name values)
            (fields-set self name values :package package)))
    (cons "[method]fields.delete"
          (lambda (self name) (fields-delete self name :package package)))
    (cons "[method]fields.get-and-delete"
          (lambda (self name) (fields-get-and-delete self name
                                                     :package package)))
    (cons "[method]fields.append"
          (lambda (self name value)
            (fields-append self name value :package package)))
    (cons "[method]fields.copy-all"
          (lambda (self) (fields-copy-all self :package package)))
    (cons "[method]fields.clone"
          (lambda (self) (fields-clone self :package package)))
    ;; request
    (cons "[static]request.new"
          (lambda (headers contents trailers options)
            (http-request-new headers contents trailers options
                              :package package)))
    (cons "[method]request.get-method"
          (lambda (self) (http-request-get-method self :package package)))
    (cons "[method]request.set-method"
          (lambda (self method)
            (http-request-set-method self method :package package)))
    (cons "[method]request.get-path-with-query"
          (lambda (self) (http-request-get-path-with-query
                          self :package package)))
    (cons "[method]request.set-path-with-query"
          (lambda (self path)
            (http-request-set-path-with-query self path :package package)))
    (cons "[method]request.get-scheme"
          (lambda (self) (http-request-get-scheme self :package package)))
    (cons "[method]request.set-scheme"
          (lambda (self scheme)
            (http-request-set-scheme self scheme :package package)))
    (cons "[method]request.get-authority"
          (lambda (self) (http-request-get-authority self :package package)))
    (cons "[method]request.set-authority"
          (lambda (self authority)
            (http-request-set-authority self authority :package package)))
    (cons "[method]request.get-options"
          (lambda (self) (http-request-get-options self :package package)))
    (cons "[method]request.get-headers"
          (lambda (self) (http-request-get-headers self :package package)))
    (cons "[static]request.consume-body"
          (lambda (this res)
            (http-request-consume-body this res :package package)))
    ;; request-options
    (cons "[constructor]request-options"
          (lambda () (request-options-constructor :package package)))
    (cons "[method]request-options.get-connect-timeout"
          (lambda (self) (request-options-get-connect-timeout
                          self :package package)))
    (cons "[method]request-options.set-connect-timeout"
          (lambda (self duration)
            (request-options-set-connect-timeout self duration
                                                 :package package)))
    (cons "[method]request-options.get-first-byte-timeout"
          (lambda (self) (request-options-get-first-byte-timeout
                          self :package package)))
    (cons "[method]request-options.set-first-byte-timeout"
          (lambda (self duration)
            (request-options-set-first-byte-timeout self duration
                                                    :package package)))
    (cons "[method]request-options.get-between-bytes-timeout"
          (lambda (self) (request-options-get-between-bytes-timeout
                          self :package package)))
    (cons "[method]request-options.set-between-bytes-timeout"
          (lambda (self duration)
            (request-options-set-between-bytes-timeout self duration
                                                       :package package)))
    (cons "[method]request-options.clone"
          (lambda (self) (request-options-clone self :package package)))
    ;; response
    (cons "[static]response.new"
          (lambda (headers contents trailers)
            (http-response-new headers contents trailers
                               :package package)))
    (cons "[method]response.get-status-code"
          (lambda (self) (http-response-get-status-code self
                                                        :package package)))
    (cons "[method]response.set-status-code"
          (lambda (self status)
            (http-response-set-status-code self status :package package)))
    (cons "[method]response.get-headers"
          (lambda (self) (http-response-get-headers self :package package)))
    (cons "[static]response.consume-body"
          (lambda (this res)
            (http-response-consume-body this res :package package))))
           :packages (%http-packages :package package))))
    (%http-register-dtors inst :package package)
    inst))

(defun http-client-instance (context
                             &key network (package (wasi-package "http")))
  "The wasi:http/client import: send, bound to CONTEXT's transports and
network authority."
  (provider-instance
   (wasmdown.wit:package-interface package "client")
   (list (cons "send"
               (lambda (request)
                 (http-client-send context request
                                   :network (or network
                                                (first (wasi-context-networks
                                                        context)))
                                   :package package))))
   :packages (%http-packages :package package)))

(defun http-provider (context
                      &key network (package (wasi-package "http")))
  "The wasi:http imports (types + client) as a with-args alist."
  (list
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "types"))
         (http-types-instance context :package package))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "client"))
         (http-client-instance context :network network
                               :package package))))

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
;;; explicit WASI-CONTEXT (docs/design/PLAN.md §9.1), never an ambient global — and
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
;;; `wasi:http' is served OUTGOING only: `types' and `client' have
;;; builders (M16 slice 5, restored), so a world or component that
;;; declares them gets their providers. The INCOMING
;;; `wasi:http/handler' direction has no provider yet and falls to the
;;; construction-time error above, with its import name.

(defun %wasi-wit-packages ()
  "The packages provider composition resolves against: the six WASI
0.3.2 packages this runtime serves (`cli', `clocks', `random',
`filesystem', `sockets' and `http'). A world importing any interface
of them names a real interface and is served or refused by the
provider check."
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

(defun %wasi-http-import-name-p (name)
  "T when the import NAME belongs to the `wasi:http' package, at any
version and for any interface (`wasi:http/types@0.3.2', ...). The
namespace and package segments are matched structurally, so a version
suffix, an unversioned spelling, or an interface this file does not
know still counts — and a lookalike such as `wasi:http-client/x' or
`my:wasi:http/types' does not."
  (and (stringp name)
       (let ((colon (position #\: name))
             (slash (position #\/ name)))
         (and colon slash (< colon slash)
              (string= "wasi" name :end2 colon)
              (string= "http" (subseq name (1+ colon) slash))))))

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
         (http (wasi-package "http"))
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
      ;; wasi:http (M16 slice 5, restored): the OUTGOING surface —
      ;; the types resources and client.send. The incoming
      ;; service/middleware world is a follow-up: `handler' has no
      ;; builder, so a world declaring it still gets the generic
      ;; no-provider error.
      (bind http "types"
            (lambda () (http-types-instance context :package http)))
      (bind http "client"
            (lambda ()
              (http-client-instance context :network network
                                    :package http)))
      ;; the middleware world's imported `handler'; an embedder that
      ;; wants to forward supplies http-middleware-providers' explicit
      ;; provider, otherwise the default answers internal-error.
      (bind http "handler"
            (lambda ()
              (http-handler-provider-instance nil :package http)))
      (nreverse builders))))

(defparameter +wasi-unstable-import-message-format+
  "~a is @unstable(feature = ~a) and that feature is not enabled; unstable interfaces are outside the selected stable WASI 0.3.2 surface (see docs/usage/wasi.md and specs/wasi/MANIFEST.md)."
  "The refusal template for a component that DECLARES an @unstable WASI
0.3.2 import whose feature the selected surface does not enable. The
first argument is the import name, the second the feature name.")

(defun %wasi-unstable-import-p (name)
  "The @unstable feature NAME's target interface is gated out by, or
NIL when NAME is not an @unstable-gated interface of the pinned package
set. Delegates the whole annotation question to the WIT selection gate
(WIT-IMPORT-SURFACE-STATUS); unknown names are never assumed unstable."
  (multiple-value-bind (status interface feature)
      (wasmdown.wit:wit-import-surface-status name (%wasi-wit-packages))
    (declare (ignore interface))
    (and (eq status :unstable) feature)))

(defun %refuse-unstable-declared-imports (wasi-names)
  "Refuse the first @unstable import in WASI-NAMES whose feature is not
enabled, in declaration order, as a LINK-ERROR naming the import and
the feature. Stable and unknown names pass through untouched."
  (dolist (name wasi-names)
    (let ((feature (%wasi-unstable-import-p name)))
      (when feature
        (error 'wasmdown:link-error
               :message (format nil +wasi-unstable-import-message-format+
                                name feature)))))
  nil)

(defparameter +wasi-http-unsupported-message+
  "wasi:http is not supported by this runtime by design on the WASI 0.2 (Preview2) line; the 0.3.2 `types` + `client` providers are the served HTTP surface. See docs/usage/wasi.md and specs/wasi/MANIFEST.md."
  "The canonical refusal text for a declared `wasi:http' import on the
WASI 0.2 (Preview2) line: the P2 provider table has no http provider,
so a 0.2 declaration is refused with this message (also when it is the
0.2 subset of a mixed-release component). The 0.3.2 line serves
`wasi:http/types' + `wasi:http/client' through the provider builders
and is never refused here.")

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

(defun declared-component-imports (comp)
  "The exact import names a decoded component DECLARES, in order."
  (loop for d across (wasmdown.component:comp-defs comp)
        when (eq (wasmdown.component:compdef-kind d) :import)
          collect (wasmdown.component::importrec-name
                   (wasmdown.component:compdef-payload d))))

(defun %types-only-declared-import-p (name)
  "T when the 0.3-line declared import NAME resolves to an interface
with no functions and no resources, so nothing needs a host binding.
Only a RESOLVED 0.3 package interface qualifies: an unknown name is
never assumed types-only (it fails the provider check instead)."
  (multiple-value-bind (status interface)
      (wasmdown.wit:wit-import-surface-status name (%wasi-wit-packages))
    (declare (ignore status))
    (and interface
         (null (wasmdown.wit:wit-interface-func-list interface))
         (null (wasmdown.wit:wit-interface-resource-list interface)))))

(defun %declared-03-provider (name builders)
  "The provider instance for the 0.3-line declared import NAME, or NIL
when the interface is types-only (it needs no host binding). A name
this runtime has no provider for is a construction-time `WIT-ERROR'
naming the import — never a trapping stub. NAME has already passed the
0.3 line's exclusions."
  (let ((builder (cdr (assoc name builders :test #'string=))))
    (cond
      (builder (funcall builder))
      ((%types-only-declared-import-p name) nil)
      (t
       (error 'wasmdown.wit:wit-error
              :message
              (format nil "component declares import `~a`, for which this runtime has no provider"
                      name))))))

(defun %compose-declared-03-providers (context names)
  "The provider alist for the 0.3-line DECLARED imports NAMES, in
declaration order. The 0.3 gates run FIRST, over exactly these names,
so no provider is built for a refused import: an @unstable interface
whose feature is not enabled is refused naming import and feature.
`wasi:sockets' names bind through the network authority's providers,
and `wasi:http/types'/`wasi:http/client' bind through their builders;
a types-only interface composes no entry and `wasi:http/handler' (the
incoming direction) has no builder yet."
  (%refuse-unstable-declared-imports names)
  (let ((builders (%wasi-provider-builders context))
        (providers '()))
    (dolist (name names)
      (let ((provider (%declared-03-provider name builders)))
        (when provider
          (push (cons name provider) providers))))
    (nreverse providers)))

(defun %p2-declared-provider-table (context v2)
  "A hash table import-name -> provider for the exact-version 0.2
declared imports V2: one P2 provider table per DISTINCT patch version,
each built for exactly the names that version declares. The selection
stays version-exact; no provider is invented for a name the table does
not have (PREVIEW2-COMMAND-PROVIDER raises for it)."
  (let ((by-version (make-hash-table :test #'equal))
        (table (make-hash-table :test #'equal)))
    (dolist (pair v2)
      (push (car pair) (gethash (cdr pair) by-version)))
    (maphash (lambda (ver imports)
               (dolist (provider
                        (preview2-command-provider
                         context :version ver
                         :imports (nreverse imports)))
                 (setf (gethash (car provider) table) (cdr provider))))
             by-version)
    table))

(defun %compose-mixed-release-providers (context wasi-names v2)
  "Compose ONE provider graph for a component that declares both
release lines. V2 is the component's exact-version 0.2 declared
imports as (name . version); WASI-NAMES is every declared `wasi:'
import in declaration order. The imports NOT in V2 are the 0.3 line
and are composed from the 0.3 command builders (gated exactly like a
0.3-only component); the V2 imports come from the P2 tables at their
own exact versions. The result is a single with-args alist in
declaration order. A declared import outside both line's providers is
still an error: no provider is invented to make a mix compose."
  (let* ((p2-names (mapcar #'car v2))
         (03-names (remove-if (lambda (name)
                                (member name p2-names :test #'string=))
                              wasi-names))
         (03-providers (%compose-declared-03-providers context 03-names))
         (p2-table (%p2-declared-provider-table context v2))
         (providers '()))
    (dolist (name wasi-names)
      (let ((provider (or (gethash name p2-table)
                          (cdr (assoc name 03-providers :test #'string=)))))
        (when provider
          (push (cons name provider) providers))))
    (nreverse providers)))

(defun component-command-providers (context comp &key (version "0.2.12"))
  "The provider set for COMP's DECLARED command imports, chosen by the
release line the component asks for: an exact `@0.2.x' import gets the
versioned P2 provider table at THAT version (mixed patch versions are
composed per version), anything else gets the vendored 0.3 command
world. Choosing from the declared contract — never from the file name
— is what keeps a 0.2 component from being handed 0.3 providers.

A 0.3-line component may also IMPORT `wasi:sockets/types',
`wasi:sockets/ip-name-lookup', `wasi:http/types' and `wasi:http/client':
sockets names bind through the network authority's builders, and the
declared http names are appended on top of the command set (the pinned
command world imports no http). `wasi:http/handler' (the incoming
direction) has no provider yet and is a construction-time error. The
0.3 path still refuses an @unstable declared import whose feature the
selected surface does not enable.

A component that declares BOTH lines gets one COMPOSED graph
(%COMPOSE-MIXED-RELEASE-PROVIDERS): the 0.3 command builders bind
exactly its 0.3-line declared imports under the same @unstable gate,
and the P2 tables bind its 0.2-line imports at their exact versions,
in declaration order. An import that neither line can serve is still
an error (a 0.2 `wasi:http' declaration included: that line has no
http provider). No feature can be enabled from the CLI: the gate is
refusal-only."
  (let* ((names (declared-component-imports comp))
         (wasi-names (remove-if-not (lambda (n)
                                      (and (stringp n)
                                           (eql 0 (search "wasi:" n))))
                                    names))
         (v2 (loop for n in wasi-names
                   for (short iface ver) = (multiple-value-list
                                            (p2-import-name-parts n))
                   when (and short ver (p2-version-p ver)
                             (eql 0 (search "0.2." ver)))
                     collect (cons n ver))))
    (cond
     ((null v2)
      ;; the 0.3 path: the declared imports must lie inside the selected
      ;; STABLE surface. An @unstable interface whose feature the default
      ;; selection does not enable is refused here, naming import and
      ;; feature, before anything composes.
      (%refuse-unstable-declared-imports wasi-names)
      (let ((http-names (remove-if-not #'%wasi-http-import-name-p
                                       wasi-names)))
        (if (null http-names)
            (command-provider context)
            ;; the command world declares no http import, so the
            ;; component's declared http interfaces are bound on top
            ;; of it (types + client; handler stays a no-provider error)
            (append (command-provider context)
                    (%compose-declared-03-providers context http-names)))))
     ((= (length v2) (length wasi-names))
      ;; every WASI import is a 0.2 release: compose per exact version
      (let ((by-version (make-hash-table :test #'equal))
            (providers '()))
        (dolist (pair v2)
          (push (car pair) (gethash (cdr pair) by-version)))
        (maphash (lambda (ver imports)
                   (dolist (p (preview2-command-provider
                               context :version ver
                               :imports (nreverse imports)))
                     (push p providers)))
                 by-version)
        (nreverse providers)))
     (t
      ;; both release lines in one component: compose each line from
      ;; its own provider graph under one alist
      (%compose-mixed-release-providers context wasi-names v2)))))

(defun command-provider (context &key (world (command-world)))
  "The provider set of the `command' world as a with-args alist,
bound to CONTEXT and composed from the resolved WORLD (default: the
vendored `wasi:cli/command'). Every import the world declares gets
its provider; a types-only import the runtime has no entry for
composes no entry; an unsupported one is a construction-time
`WIT-ERROR'. The pinned command world imports no `wasi:http'; a
component that DECLARES http gets its providers from
COMPONENT-COMMAND-PROVIDERS."
  (compose-world-providers world context))

(defun component-run-export (instance)
  "The `run' function a command-world component exports: the exported
`wasi:cli/run@<version>' instance's `run' (a comp-instance or an
inline bag) at ANY supported version — a 0.2.12 command and a 0.3.2
command export the same shape under their own release name. A
component that exports SEVERAL run versions (the wasip3 test guests
also export a `wasi:cli/run@0.2.0' compatibility shim) is driven
through the NEWEST one: the command entry is the highest release line
the component itself offers, never whichever entry the export table
happens to visit first. Fallbacks, in order: the unversioned
`wasi:cli/run' instance, then a direct top-level `run' export. NIL
when absent."
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
    (let ((versioned '())
          (unversioned nil))
      ;; pick the run instances by PREFIX: the versions are the
      ;; component's own, never assumed
      (maphash (lambda (name value)
                 (when (and (stringp name)
                            (let ((p (search "wasi:cli/run" name
                                             :test #'char=)))
                              (and p (zerop p))))
                   ;; the export table entry is (sort . value)
                   (cond
                     ((or (= (length name) 12)
                          (not (char= (char name 12) #\@)))
                      (setf unversioned (cdr value)))
                     (t
                      (let ((version (subseq name 13)))
                        ;; a malformed suffix names no comparable
                        ;; release: it is not a candidate
                        (when (handler-case
                                  (progn
                                    (wasmdown.wit:wit-version<
                                     version version)
                                    t)
                                (error () nil))
                          (push (cons version (cdr value))
                                versioned)))))))
               (wasmdown.component.runtime:comp-instance-exports instance))
      (or (loop for (version . value) in
                (stable-sort versioned
                             (lambda (a b)
                               (wasmdown.wit:wit-version< (car b) (car a))))
                when (run-of value)
                  return it)
          (and unversioned (run-of unversioned))
          (let ((direct (%export "run")))
            (and (wasmdown.component.runtime:lifted-func-p direct) direct))))))

(defun %run-result-status (result)
  "The `result' domain value of the command's run -> the process
status: ok -> 0, err (or an absent/false result) -> 1."
  (if (and (consp result) (eq (car result) :ok)) 0 1))

(defun run-component (comp context &key providers (mode :interpret))
  "Drive the VALIDATED command component COMP to completion under the
capabilities of CONTEXT. Instantiates with PROVIDERS (default: the
whole COMMAND-PROVIDER set for CONTEXT), calls the exported
`wasi:cli/run@<version>' `run' (async or sync) under execution MODE
(:interpret or :compiled — the embedded cores compile through the
wasmdown codegen backend when :compiled), and returns the process
status. A `wasi:cli/exit' terminates the run with its status; the
store is torn down in every case (no leaked guest threads). Traps
propagate to the caller. Returns (VALUES STATUS STORE): the store is
the run's own boundary object, so a caller can prove the teardown
left nothing registered (COMPONENT-STORE-THREADS)."
  (let ((providers (or providers (command-provider context)))
        (store (wasmdown.component.runtime:make-component-store
                :reactor (wasi-context-reactor context))))
    (values
     (unwind-protect
         (handler-case
             (let* ((instance
                      (wasmdown.component.runtime:instantiate-component
                       comp :with-args providers :store store
                       :mode mode))
                    (run (component-run-export instance)))
               ;; THE STORE'S REACTOR was installed on the store BEFORE
               ;; instantiation/start (step 2's ownership rule), so a
               ;; would-blocked copy or clock deadline is already
               ;; servable by the store's own tick.
               (unless run
                 (error 'wasmdown:link-error
                        :message
                        "component exports no wasi:cli/run@0.3.2 `run' function"))
               (%run-result-status
                (wasmdown.component.runtime:lifted-func-call run)))
           (wasi-exit (c) (wasi-exit-status c)))
      ;; the exit semantics: whatever the guest started, the command
      ;; is over — abandon + unblock + join every registered thread
      ;; (the store this run created is the boundary)
      (wasmdown.component.runtime:store-teardown store))
     store)))

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

;;; ===============================================================
;;; wasi:http/service — THE INCOMING DIRECTION (the `handler' export).
;;;
;;; A service-world component imports the HTTP types/client surface
;;; plus the CLI/clocks/random capabilities and EXPORTS
;;; `wasi:http/handler@<version>' (`handle: async func(request) ->
;;; result<response, error-code>'). This section is the host side:
;;;
;;;   * HTTP-SERVICE-WORLD / HTTP-SERVICE-PROVIDERS compose the
;;;     provider graph from the RESOLVED world (compose-world-
;;;     providers), exactly as the command world does;
;;;   * COMPONENT-HANDLER-EXPORT finds the exported `handle' at any
;;;     supported version, with the same fallbacks the command
;;;     runner uses for `run';
;;;   * SERVE-COMPONENT binds a loopback listener, prints the service
;;;     URL as the FIRST stderr line, reads HTTP/1.1 requests,
;;;     constructs the INCOMING `request' resource (method, path,
;;;     scheme, authority, headers and the buffered body), calls the
;;;     guest handler through `lifted-func-call', then serializes the
;;;     returned `response' resource (status, headers, body).
;;;
;;; The incoming request is fully buffered before the call: the guest
;;; reads it through `request.consume-body', whose provider already
;;; serves a buffered body. The response body arrives through
;;; `response.new''s contents sink; the host waits for that stream's
;;; guest writer to drop it (the transmission future) before writing
;;; the reply.

(defun http-service-world (&key (packages (%wasi-wit-packages)) selection)
  "The RESOLVED `service' world of the vendored `wasi:http' package
under SELECTION (NIL: the default selection), with PACKAGES supplying
its cross-package imports."
  (wasmdown.wit:resolve-wit-world (wasi-package "http") "service"
                                  :packages packages
                                  :selection selection))

(defun http-service-providers (context
                               &key (world (http-service-world)))
  "The provider graph of the `wasi:http/service' world as a with-args
alist, bound to CONTEXT: its types/client imports through the
restored HTTP providers, and its cli/clocks/random imports through
the command-world builders. A declared import the world does not
have (a test world adding `wasi:cli/environment') is added by
SERVE-COMPONENT-PROVIDERS, never silently ignored."
  (compose-world-providers world context))

(defun http-handler-provider-instance (next &key (package (wasi-package "http")))
  "The `wasi:http/handler' import as a provider instance. NEXT is a CL
function of the component-domain request returning the domain
result<response,error-code>; NIL means the embedder supplied no
downstream handler and every call resolves to an internal-error result.
This is the middleware world's host-supplied `next' handler."
  (provider-instance
   (wasmdown.wit:package-interface package "handler")
   (list
    (cons "handle"
          (lambda (request)
            (if next
                (funcall next request)
                (%http-internal
                 "middleware handler has no downstream handler bound")))))
   :packages (%http-packages :package package)))

(defun http-middleware-providers (context next
                                 &key (package (wasi-package "http")))
  "The with-args alist for a `wasi:http/middleware' component: the
service world's imports (types/client/cli/clocks/random) plus the
host-supplied `handler' NEXT the middleware forwards to. The component
exports its own handler; use COMPONENT-HANDLER-EXPORT/SERVE-COMPONENT to
run it. NEXT is a domain-level CL function, so an embedder can chain a
downstream guest handler or an in-process host handler."
  (let ((name (wasmdown.wit:interface-import-name
               (wasmdown.wit:package-interface package "handler"))))
    (append (http-service-providers context)
            (list (cons name
                        (http-handler-provider-instance
                         next :package package))))))

;;; ---------------------------------------------------------------
;;; The exported handler.

(defun %handler-export-value (source)
  "The `handle' value an exported handler SOURCE denotes: a
comp-instance's export, a bag's entry, or an already-lifted func."
  (typecase source
    (wasmdown.component.runtime:comp-instance
     (wasmdown.component.runtime:runtime-export-value source "handle"))
    (hash-table (gethash "handle" source))
    (wasmdown.component.runtime:lifted-func source)
    (t nil)))

(defun %handler-version-ok-p (version)
  (handler-case
      (progn (wasmdown.wit:wit-version< version version) t)
    (error () nil)))

(defun component-handler-export (instance)
  "The `wasi:http/handler' `handle' a service-world component exports:
the newest exported `wasi:http/handler@<version>' instance's
`handle' (a comp-instance or an inline bag), then the unversioned
`wasi:http/handler' instance, then a direct top-level `handle'
export. NIL when the component exports none."
  (let ((versioned '())
        (unversioned nil)
        (direct nil))
    (maphash
     (lambda (name entry)
       (when (stringp name)
         (cond
           ((string= name "handle") (setf direct (cdr entry)))
           ((let ((p (search "wasi:http/handler" name :test #'char=)))
              (and p (zerop p)))
            (if (or (= (length name) 17)
                    (not (char= (char name 17) #\@)))
                (setf unversioned (cdr entry))
                (let ((version (subseq name 18)))
                  (when (%handler-version-ok-p version)
                    (push (cons version (cdr entry)) versioned))))))))
     (wasmdown.component.runtime:comp-instance-exports instance))
    (or (loop for (version . value) in
              (stable-sort versioned
                           (lambda (a b)
                             (wasmdown.wit:wit-version< (car b) (car a))))
              when (%handler-export-value value)
                return (%handler-export-value value))
        (and unversioned (%handler-export-value unversioned))
        (and (wasmdown.component.runtime:lifted-func-p direct) direct))))

;;; ---------------------------------------------------------------
;;; The provider graph for a COMPONENT: the service world plus every
;;; declared WASI import the world does not itself name.

(defun serve-component-providers (context comp &key (world (http-service-world)))
  "The with-args alist for the service-world component COMP: the
service world's own provider graph (HTTP-SERVICE-PROVIDERS) plus a
provider for each DECLARED WASI import the world does not name. A
wasip3 guest built by wit-bindgen mixes the 0.3 service surface with
0.2 compatibility imports (`wasi:io/poll@0.2.9',
`wasi:cli/environment@0.2.9', ...): those bind through the versioned
P2 tables exactly as COMPONENT-COMMAND-PROVIDERS binds a mixed
command component, while the 0.3 names keep their builders. An import
neither line serves is a construction-time WIT-ERROR naming it."
  (let* ((names (declared-component-imports comp))
         (wasi-names (remove-if-not (lambda (n)
                                      (and (stringp n)
                                           (eql 0 (search "wasi:" n))))
                                    names))
         (v2 (loop for n in wasi-names
                   for (short iface ver) = (multiple-value-list
                                            (p2-import-name-parts n))
                   when (and short ver (p2-version-p ver)
                             (eql 0 (search "0.2." ver)))
                     collect (cons n ver)))
         (providers
           (if v2
               (%compose-mixed-release-providers context wasi-names v2)
               (%compose-declared-03-providers context wasi-names))))
    ;; the RESOLVED service world supplies the capabilities a guest
    ;; that uses them declares; bind any the component omitted too
    ;; (instantiation ignores unused with-args, and this keeps the
    ;; composed graph the world's contract)
    (dolist (pair (http-service-providers context :world world))
      (unless (assoc (car pair) providers :test #'string=)
        (push pair providers)))
    providers))

;;; ---------------------------------------------------------------
;;; The listener and HTTP/1.1 request parsing.

(defparameter +serve-max-body-bytes+ (* 16 1024 1024)
  "The incoming request body bound (refused with 413 beyond it).")

(defun parse-serve-address (spec)
  "Parse \"HOST:PORT\" for `serve'. HOST is an IPv4 literal or
`localhost'; PORT is 0..65535 (0: the OS picks). Returns (values
HOST-OCTETS PORT). Signals HOST-ERROR on a malformed spec."
  (let ((colon (position #\: spec :from-end t)))
    (unless colon
      (error 'wasmdown:host-error
             :message (format nil "address ~s is not HOST:PORT" spec)))
    (let* ((host (subseq spec 0 colon))
           (port-text (subseq spec (1+ colon)))
           (octets (cond ((string= host "localhost") '(127 0 0 1))
                         ((%parse-ipv4-literal host))
                         (t (error 'wasmdown:host-error
                                   :message
                                   (format nil "address host ~s is not an IPv4 literal or `localhost'"
                                           host)))))
           (port (unless (and (plusp (length port-text))
                              (every #'digit-char-p port-text))
                   (error 'wasmdown:host-error
                          :message
                          (format nil "address port ~s is not an integer"
                                  port-text)))))
      (let ((n (parse-integer port-text)))
        (unless (<= 0 n 65535)
          (error 'wasmdown:host-error
                 :message (format nil "address port ~d out of range" n)))
        (values octets n)))))

(defun %serve-open-listener (host-octets port)
  "Bind AF_INET SOCK_STREAM to HOST-OCTETS:PORT and listen. Returns
(values FD PORT)."
  (multiple-value-bind (alien len)
      (%make-sockaddr "ipv4" (list (cons "port" port)
                                   (cons "address" host-octets)))
    (unwind-protect
         (let ((fd (%libc-socket +af-inet+
                                 (logior +sock-stream+ +sock-nonblock+
                                         +sock-cloexec+)
                                 +ipproto-tcp+)))
           (when (minusp fd)
             (error 'wasmdown:host-error
                    :message (format nil "serve: socket(2) failed (errno ~d)"
                                     (sb-alien:get-errno))))
           (handler-case
               (progn
                 (%setsockopt-int fd +sol-socket+ +so-reuseaddr+ 1)
                 (%socket-check (%libc-bind fd (sb-alien:alien-sap alien) len)
                                "bind")
                 (%socket-check (%libc-listen fd 128) "listen")
                 (multiple-value-bind (bound-port bound-len)
                     (%serve-sockname fd)
                   (declare (ignore bound-len))
                   (values fd bound-port)))
             (error (c)
               (%close-fd fd)
               (error c))))
      (sb-alien:free-alien alien))))

(defun %serve-sockname (fd)
  "(values PORT LEN) from getsockname."
  (let ((alien (sb-alien:make-alien
                (sb-alien:array sb-alien:unsigned-char 128)))
        (len (sb-alien:make-alien sb-alien:unsigned-int)))
    (unwind-protect
         (progn
           (setf (sb-alien:deref len) +sockaddr-storage-size+)
           (%socket-check
            (%libc-getsockname fd (sb-alien:alien-sap alien)
                               (sb-alien:alien-sap len))
            "getsockname")
           (multiple-value-bind (family host port)
               (%sockaddr-sap->key (sb-alien:alien-sap alien))
             (declare (ignore family host))
             (values port (sb-alien:deref len))))
      (sb-alien:free-alien alien)
      (sb-alien:free-alien len))))

(defun %serve-method-value (name)
  "The WIT `method' domain value for the request-line NAME."
  (let ((lower (string-downcase name)))
    (if (member lower '("get" "head" "post" "put" "delete" "connect"
                        "options" "trace" "patch")
                :test #'string=)
        (cons lower nil)
        (cons "other" name))))

(defun %serve-read-chunked-body (r)
  "Decode a chunked request body; returns the byte list. Chunk
extensions and trailers are skipped."
  (let ((out '()))
    (loop
      (let* ((line (%hr-line r :timeout-ms 30000))
             (semi (position #\; line))
             (hex (string-trim '(#\Space #\Tab)
                               (if semi (subseq line 0 semi) line))))
        (unless (and (plusp (length hex))
                     (every (lambda (c) (digit-char-p c 16)) hex))
          (%http-error (%http-code "HTTP-protocol-error")))
        (let ((size (parse-integer hex :radix 16)))
          (when (zerop size)
            ;; trailer section
            (loop for trailer = (%hr-line r :timeout-ms 30000)
                  until (and (stringp trailer) (zerop (length trailer))))
            (return out))
          (when (> (+ (length out) size) +serve-max-body-bytes+)
            (%http-error (%http-code "HTTP-request-body-size")))
          (setf out (nconc out (%hr-take r size 30000)))
          (%hr-take r 2 30000))))))

(defun %serve-read-request (conn)
  "Read one HTTP/1.1 request from CONN. Returns the
WASI-HTTP-INCOMING record, or NIL on a clean EOF before any byte.
Signals WASI-HTTP-ERROR on a malformed request."
  (let* ((r (make-wasi-http-reader :conn conn :timeout-ms 30000))
         (line (%hr-line r :timeout-ms 30000)))
    (when (eq line :eof)
      (return-from %serve-read-request nil))
    (let ((parts (%split-string line #\Space)))
      (unless (and (= 3 (length parts))
                   (plusp (length (first parts)))
                   (%http-token-p (first parts))
                   (plusp (length (second parts)))
                   (string= (third parts) "HTTP/1.1"))
        (%http-error (%http-code "HTTP-protocol-error")))
      (let ((fields (%http-read-fields r :timeout-ms 30000))
            (method (first parts))
            (target (second parts)))
        (let* ((chunked-p
                 (let ((te (%fields-values fields "transfer-encoding")))
                   (and te
                        (string-equal
                         (string-trim '(#\Space #\Tab)
                                      (%http-bytes->string (car (last te))))
                         "chunked"))))
               (length (%http-content-length fields))
               (body
                 (cond
                   (chunked-p (%serve-read-chunked-body r))
                   ((and length (plusp length))
                    (when (> length +serve-max-body-bytes+)
                      (%http-error (%http-code "HTTP-request-body-size")))
                    (%hr-take r length 30000))
                   (t '())))
               (host (%fields-values fields "host"))
               (authority (and host
                               (%http-bytes->string (car (last host))))))
          (make-wasi-http-incoming
           :method method
           :path target
           :version (third parts)
           :headers (wasi-fields-entries fields)
           :authority authority
           :body body))))))

;;; ---------------------------------------------------------------
;;; The incoming request resource + the response serializer.

(defstruct (wasi-http-incoming (:constructor make-wasi-http-incoming))
  "One parsed incoming HTTP/1.1 request (the host side of the WIT's
incoming `request')."
  (method "GET")
  (path "/")
  (version "HTTP/1.1")
  (headers nil)                       ; (name . value-bytes) entries
  (authority nil)
  (body '()))                         ; u8 list

(defun %serve-request-resource (incoming &key (package (wasi-package "http")))
  "The wasi-http-request REP holding INCOMING's method/path/scheme/
authority/headers and its fully buffered body."
  (declare (ignore package))
  (let ((req (make-wasi-http-request
              :method (%serve-method-value
                       (wasi-http-incoming-method incoming))
              :path-with-query (cons :some (wasi-http-incoming-path incoming))
              :scheme (cons :some (cons "HTTP" nil))
              :authority (let ((a (wasi-http-incoming-authority incoming)))
                           (if a (cons :some a) :none))
              :headers (%make-wasi-fields
                        (loop for (name . value)
                                in (wasi-http-incoming-headers incoming)
                              collect (cons name (copy-list value)))
                        nil)
              :body-buffer (list (copy-list (wasi-http-incoming-body incoming)))
              :body-length (length (wasi-http-incoming-body incoming))
              :body-closed-p t)))
    req))

(defun %serve-invoke (handler instance request-ct incoming store
                      &key (package (wasi-package "http")))
  "Call HANDLER with an OWN incoming request handle. Returns
(values RESPONSE-REP ERROR-CODE). Waits (bounded) for the guest
response contents writer to resolve before returning."
  (let ((req (%serve-request-resource incoming :package package)))
    (let ((value (wasmdown.component.runtime:lifted-func-call
                  handler (list :own-handle request-ct req))))
      (cond
        ((and (consp value) (eq (car value) :err))
         (values nil (cdr value)))
        ((and (consp value) (eq (car value) :ok))
         (let ((payload (cdr value)))
           (let ((resp (cond ((wasi-http-response-p payload) payload)
                             ((and (consp payload)
                                   (eq (car payload) :own-handle))
                              (third payload))
                             ;; the async result lift leaves an OWN
                             ;; result's handle index in the defining
                             ;; instance's table when the resource ct
                             ;; is the provider schema's; read its rep
                             ((integerp payload)
                              (let ((h (ignore-errors
                                         (wasmdown.component.runtime::%handle-resource-at
                                          instance payload))))
                                (and h (wasmdown.component.runtime::resource-handle-rep
                                        h))))
                             (t nil))))
             (when resp
               (let ((writer (wasi-http-response-transmission-writer resp)))
                 ;; The guest's response-body writer is a SPAWNED guest
                 ;; async thread (the `spawn_local' an echo handler uses
                 ;; to feed its contents stream). The handler call's own
                 ;; drain has ended, so only a SCHEDULER quantum can run
                 ;; it: a host-work tick alone never does, and the
                 ;; transmission future would then stay unresolved until
                 ;; the wait gave up (the empty-body POST: the writer
                 ;; writes nothing and drops, so no stream readiness
                 ;; event ever arrives to wake the old loop). Drive both
                 ;; lines of work here, bounded so a handler that keeps
                 ;; its body open cannot wedge the serve loop.
                 (when writer
                   (loop repeat 5000
                         until (wasmdown.component.runtime:host-future-resolved-p
                                writer)
                         do (ignore-errors
                              (wasmdown.component.runtime:store-service-tick
                               store))
                            (sleep 0.001)))))
             (values resp nil))))
        (t (values nil (cons "internal-error" nil)))))))

(defparameter +serve-reasons+
  '((200 . "OK") (201 . "Created") (202 . "Accepted")
    (204 . "No Content") (301 . "Moved Permanently") (302 . "Found")
    (304 . "Not Modified") (400 . "Bad Request") (401 . "Unauthorized")
    (403 . "Forbidden") (404 . "Not Found") (405 . "Method Not Allowed")
    (408 . "Request Timeout") (413 . "Content Too Large")
    (418 . "I'm a teapot") (500 . "Internal Server Error")
    (501 . "Not Implemented") (502 . "Bad Gateway")
    (503 . "Service Unavailable")))

(defun %serve-response-bytes (resp)
  "The response REP's buffered body as a u8 list."
  (let ((body (and resp (wasi-http-response-body resp))))
    (if body (copy-list (wasi-http-body-buffer body)) '())))

(defun %serve-write-response (conn status headers body head-p)
  "Serialize one HTTP/1.1 response (status + guest headers + the
buffered body). The connection is closed by the caller."
  (let* ((out (make-string-output-stream))
         (entries (or headers '()))
         (has-length (some (lambda (e)
                             (string-equal (car e) "content-length"))
                           entries))
         (resp-body (if head-p '() body)))
    (format out "HTTP/1.1 ~d ~a~c~c"
            status
            (or (cdr (assoc status +serve-reasons+)) "OK")
            #\Return #\Newline)
    (dolist (entry entries)
      (let ((name (car entry)))
        (unless (member (string-downcase name)
                        '("connection" "transfer-encoding" "keep-alive")
                        :test #'string=)
          (format out "~a: ~a~c~c" name
                  (%http-bytes->string (cdr entry))
                  #\Return #\Newline))))
    (unless has-length
      (format out "Content-Length: ~d~c~c" (length resp-body)
              #\Return #\Newline))
    (format out "Connection: close~c~c~c~c" #\Return #\Newline
            #\Return #\Newline)
    (%http-plain-write (wasi-http-connection-fd conn)
                       (map 'list #'char-code (get-output-stream-string out)))
    (when (plusp (length resp-body))
      (%http-plain-write (wasi-http-connection-fd conn) resp-body))
    nil))

(defun %serve-error-status (code)
  "The HTTP status an error-code case maps to."
  (let ((label (if (consp code) (car code) code)))
    (cond
      ((string-equal label "HTTP-request-method-invalid") 400)
      ((string-equal label "HTTP-request-URI-invalid") 400)
      ((string-equal label "HTTP-request-URI-too-long") 414)
      ((string-equal label "HTTP-request-denied") 403)
      ((string-equal label "HTTP-request-length-required") 411)
      ((string-equal label "HTTP-request-body-size") 413)
      ((string-equal label "HTTP-request-header-section-size") 431)
      ((string-equal label "HTTP-request-header-size") 431)
      (t 500))))

(defun %serve-connection-handle (conn handler instance request-ct store
                                 &key (package (wasi-package "http")))
  "One request/response exchange on an accepted CONN FD."
  (handler-case
      (let ((incoming (%serve-read-request conn)))
        (when incoming
          (multiple-value-bind (resp code)
              (%serve-invoke handler instance request-ct incoming store
                             :package package)
            (cond
              (resp
               (let ((status (wasi-http-response-status resp))
                     (headers (wasi-fields-entries
                               (wasi-http-response-headers resp)))
                     (body (%serve-response-bytes resp)))
                 (%serve-write-response
                  conn status headers body
                  (string-equal (wasi-http-incoming-method incoming) "HEAD"))))
              (t
               (%serve-write-response conn (%serve-error-status code) nil nil
                                      (string-equal
                                       (wasi-http-incoming-method incoming)
                                       "HEAD")))))))
    (wasi-http-error (c)
      (format *error-output* "serve: bad request: ~a~%" c)
      (ignore-errors
        (%serve-write-response conn 400 nil nil nil)))
    (wasmdown:wasmdown-error (c)
      (format *error-output* "serve: handler failed: ~a~%" c)
      (ignore-errors (%serve-write-response conn 500 nil nil nil))))
  nil)

;;; ---------------------------------------------------------------
;;; The serve loop.

(defvar *serve-stop* nil
  "Set by the SIGINT handler; the accept loop exits after it.")

(defun %serve-interrupt (&rest args)
  (declare (ignore args))
  (setf *serve-stop* t))

(defun serve-component (comp context
                        &key providers (address "127.0.0.1:0")
                          (mode :interpret) (url-stream nil))
  "Instantiate the validated service-world COMP under CONTEXT, print
`http://HOST:PORT' as the FIRST line on the URL stream (default: the
context's stderr, else *ERROR-OUTPUT*), and serve HTTP/1.1 requests
until SIGINT. Each request is answered by the exported
`wasi:http/handler' `handle' function. Returns the process status
(0)."
  (multiple-value-bind (host-octets port) (parse-serve-address address)
    (let* ((store (wasmdown.component.runtime:make-component-store
                   :reactor (wasi-context-reactor context)))
           (providers (or providers
                          (serve-component-providers context comp)))
           (fd nil)
           (old-handler nil)
           (*serve-stop* nil))
      (unwind-protect
           (progn
             (setf old-handler
                   (sb-sys:enable-interrupt sb-unix:sigint
                                            #'%serve-interrupt))
             (multiple-value-setq (fd port)
               (%serve-open-listener host-octets port))
             (let* ((instance
                      (wasmdown.component.runtime:instantiate-component
                       comp :with-args providers :store store :mode mode))
                    (handler (component-handler-export instance)))
               (unless handler
                 (error 'wasmdown:link-error
                        :message
                        "component exports no wasi:http/handler `handle' function"))
               (let* ((request-ct (http-resource-ct "request"))
                      (stream (or url-stream
                                  (wasi-context-stderr context)
                                  *error-output*))
                      (display-host
                        (if (equal host-octets '(0 0 0 0))
                            "127.0.0.1"
                            (format nil "~{~d~^.~}" host-octets))))
                 (format stream "http://~a:~d~%" display-host port)
                 (finish-output stream)
                 (%serve-accept-loop fd handler instance request-ct store))))
        0)
      (when fd
        (ignore-errors (%close-fd fd)))
      (when old-handler
        (ignore-errors
          (sb-sys:enable-interrupt sb-unix:sigint old-handler)))
      (ignore-errors
        (wasmdown.component.runtime:store-teardown store)))))

(defun %serve-accept-loop (fd handler instance request-ct store)
  "Accept connections until *SERVE-STOP*, handling each one on the
accepting thread (the guest handler serializes through its store
anyway)."
  (loop until *serve-stop*
        do (let ((revents (%poll-fd fd +pollin+ 100)))
             (when (and revents (logtest revents +pollin+))
               (let ((cfd (%libc-accept4 fd (%sock-null) (%sock-null)
                                         (logior +sock-nonblock+
                                                 +sock-cloexec+))))
                 (when (>= cfd 0)
                   (let ((conn
                           (make-wasi-http-connection
                            :fd cfd
                            :read-fn (lambda (max to)
                                       (%http-plain-read cfd max to))
                            :write-fn (lambda (bytes)
                                        (%http-plain-write cfd bytes))
                            :close-fn (lambda () (%close-fd cfd)))))
                     (unwind-protect
                          (%serve-connection-handle
                           conn handler instance request-ct store)
                       (%http-connection-close conn))))))))
  nil)
