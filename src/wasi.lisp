;;;; WASMDOWN — the WASI host-provider framework (PLAN.md §9, M16
;;;; slice 1): the WIT interface + an implementation alist -> a
;;;; component instance value the runtime can bind through with-args,
;;;; plus the wasi:clocks, wasi:random and wasi:cli (environment/exit)
;;;; providers.
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
;;;;   * unimplemented-but-declared functions are still exported, as
;;;;     closures that signal host-error (the multi-gate discipline:
;;;;     the interface surface is complete, the support is honest).

(in-package #:wasmdown.wasi)

;;; ---------------------------------------------------------------
;;; Capability context.

(defstruct wasi-context
  "The explicit authority object a CLI provider is built from
(PLAN.md §9.1). ARGUMENTS is a list of strings, ENVIRONMENT an alist
of (name . value) strings, CWD a string or NIL."
  (arguments nil)
  (environment nil)
  (cwd nil))

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

(defun %unsupported-closure (interface function-name)
  (lambda (args)
    (declare (ignore args))
    (error 'wasmdown:host-error
           :message (format nil
                            "WASI function ~a of interface ~a is declared ~
                             but not implemented in this slice"
                            function-name
                            (wasmdown.wit:wit-interface-name interface)))))

(defun provider-instance (interface implementations &key packages)
  "Build a comp-instance for the parsed WIT INTERFACE. IMPLEMENTATIONS
is an alist ((\"now\" . fn) ...) of CL functions taking the component
domain arguments; a function absent from the alist (or bound to
:UNSUPPORTED) is still exported, as a host-error trap. The instance's
export table also carries the interface's TYPE definitions, so a
consumer can alias them with the runtime's type-projection path."
  (let ((inst (wasmdown.component.runtime:make-comp-instance)))
    (dolist (fn (wasmdown.wit:wit-interface-func-list interface))
      (let* ((fname (wasmdown.wit:wit-func-name fn))
             (ct (wasmdown.wit:wit-func->ct interface fn
                                            :packages packages))
             (impl (cdr (assoc fname implementations :test #'string=)))
             (closure
               (cond
                 ((and impl (not (eq impl :unsupported)))
                  (lambda (args) (apply impl args)))
                 (t (%unsupported-closure interface fname)))))
        (setf (gethash fname
                       (wasmdown.component.runtime:comp-instance-exports inst))
              (cons :func (make-host-lifted-func ct closure :inst inst)))))
    ;; The interface's defined types (not resources: a resource has no
    ;; nominal identity in slice 1) are exported as (:type . ct).
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

(defun monotonic-clock-instance (&key (package (wasi-package "clocks")))
  (provider-instance
   (wasmdown.wit:package-interface package "monotonic-clock")
   (list (cons "now" #'monotonic-now)
         (cons "get-resolution" #'monotonic-resolution))
   :packages (list package)))

(defun system-clock-instance (&key (package (wasi-package "clocks")))
  (provider-instance
   (wasmdown.wit:package-interface package "system-clock")
   (list (cons "now" #'system-now)
         (cons "get-resolution" #'system-resolution))
   :packages (list package)))

(defun clocks-provider (&key (package (wasi-package "clocks")))
  "The wasi:clocks imports as a with-args alist. wait-until/wait-for
are exported but unsupported in this slice."
  (list
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "types"))
         (provider-instance
          (wasmdown.wit:package-interface package "types") nil
          :packages (list package)))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "monotonic-clock"))
         (monotonic-clock-instance :package package))
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
;;; wasi:cli — environment + exit. The stdio/stream interfaces are
;;; slice 2 (they need the stream/future host machinery).

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

(defun cli-provider (context &key (package (wasi-package "cli")))
  (list
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "environment"))
         (cli-environment-instance context :package package))
   (cons (wasmdown.wit:interface-import-name
          (wasmdown.wit:package-interface package "exit"))
         (cli-exit-instance :package package))))
