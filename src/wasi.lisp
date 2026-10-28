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
;;;;   * unimplemented-but-declared functions are still exported, as
;;;;     closures that signal host-error (the multi-gate discipline:
;;;;     the interface surface is complete, the support is honest).

(in-package #:wasmdown.wasi)

;;; M16 slice 2 exports (kept here so the slice's surface travels with
;;; its implementation).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((package (find-package '#:wasmdown.wasi)))
    (dolist (name '("WASI-CONTEXT-STDIN" "WASI-CONTEXT-STDOUT"
                    "WASI-CONTEXT-STDERR" "CL-STREAM-SOURCE" "CL-STREAM-SINK"
                    "CLI-STDIN-INSTANCE" "CLI-STDOUT-INSTANCE"
                    "CLI-STDERR-INSTANCE" "CLI-OUTPUT-INSTANCE"
                    "CLI-STDIO-PROVIDER"))
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
  ;; M16 slice 5: the embedder-supplied HTTP transports (PLAN §9.2:
  ;; "pluggable system TLS providers or embedder-supplied transport
  ;; services"). A wasi-http-transport whose SCHEME matches a
  ;; request's scheme is used instead of the built-in plain TCP
  ;; transport; no match for an https request is the WIT's
  ;; `configuration-error', never a silent plaintext fallback.
  (http-transports nil))

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
domain arguments; a function absent from the alist (or bound to
:UNSUPPORTED) is still exported, as a host-error trap. The instance's
export table also carries the interface's TYPE definitions, so a
consumer can alias them with the runtime's type-projection path.

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
             (impl (cdr (assoc fname implementations :test #'string=)))
             (closure
               (cond
                 ((and impl (not (eq impl :unsupported)))
                  (lambda (args) (apply impl args)))
                 (t (%unsupported-closure interface fname)))))
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
                 (impl (or (cdr (assoc fname implementations :test #'string=))
                           (cdr (assoc mname implementations :test #'string=))))
                 (closure
                   (cond
                     ((and impl (not (eq impl :unsupported)))
                      (lambda (args) (apply impl args)))
                     (t (%unsupported-closure interface fname)))))
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

(defun cl-stream-source (stream)
  "A host SOURCE over CL input STREAM: (max) -> (values byte-list
eof-p). Character streams are encoded byte-per-character (the WASI
byte stream is opaque; chars are taken modulo the code)."
  (let* ((byte-p (%byte-stream-p stream))
         (buf (if byte-p
                  (make-array *cli-stream-chunk*
                              :element-type '(unsigned-byte 8))
                  (make-array *cli-stream-chunk* :element-type 'character))))
    (lambda (max)
      (let ((want (min max *cli-stream-chunk*)))
        (if (<= want 0)
            (values nil nil)
            (let ((got (read-sequence buf stream :end want)))
              (values (if byte-p
                          (loop for i below got collect (aref buf i))
                          (loop for i below got
                                collect (char-code (aref buf i))))
                      (< got want))))))))

(defun cl-stream-sink (stream)
  "A host SINK over CL output STREAM: (byte-list) -> an error-code
label or NIL. Flushed on every drain."
  (let ((byte-p (%byte-stream-p stream)))
    (lambda (bytes)
      (handler-case
          (progn
            (if byte-p
                (write-sequence
                 (make-array (length bytes) :element-type '(unsigned-byte 8)
                                            :initial-contents bytes)
                 stream)
                (write-sequence (map 'string #'code-char bytes) stream))
            (finish-output stream)
            nil)
        (error () "io")))))

(defun %stdio-require (stream what)
  (or stream
      (error 'wasmdown:host-error
             :message (format nil "no ~a capability in this wasi-context"
                              what))))

(defun %write-via-stream (data stream future-ct)
  "Drain the guest's stream DATA into STREAM; the returned future
resolves once the guest drops the stream's writable end (the WIT's
success condition) or a sink error drops it."
  (let ((sink-stream (%stdio-require stream "stdout/stderr")))
    (unless (wasmdown.component.runtime:async-end-p data)
      (error 'wasmdown:host-error
             :message "write-via-stream: data is not a stream end"))
    (multiple-value-bind (future-reader future-writer)
        (wasmdown.component.runtime:make-host-future-pair future-ct)
      (wasmdown.component.runtime:host-end-set-sink
       data (cl-stream-sink sink-stream))
      (wasmdown.component.runtime:host-end-set-on-drop
       data (lambda (dropped error)
              (declare (ignore dropped))
              (wasmdown.component.runtime:host-future-resolve
               future-writer
               (if error (cons :err error) (cons :ok nil)))))
      ;; the guest may have staged a write BEFORE handing us the end
      (wasmdown.component.runtime:host-end-drain data)
      future-reader)))

(defun %read-via-stream (stream future-ct)
  "A stream fed from STREAM and a future signalling the read result."
  (let ((source-stream (%stdio-require stream "stdin")))
    (multiple-value-bind (stream-reader stream-writer)
        (wasmdown.component.runtime:make-host-stream-pair (%cli-u8-ct))
      (multiple-value-bind (future-reader future-writer)
          (wasmdown.component.runtime:make-host-future-pair future-ct)
        (wasmdown.component.runtime:host-end-set-source
         stream-writer (cl-stream-source source-stream))
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
                                     future-ct))))
     :packages (list package))))

(defun cli-output-instance (context stream interface-name
                            &key (package (wasi-package "cli")))
  (let ((future-ct (%cli-future-ct package interface-name
                                   "write-via-stream")))
    (provider-instance
     (wasmdown.wit:package-interface package interface-name)
     (list (cons "write-via-stream"
                 (lambda (data)
                   (%write-via-stream data stream future-ct))))
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
not — dropping the guest's handle must not close the root), and
whether it names a directory."
  (fd -1)
  (preopen nil)
  (flags nil)
  (owned-p t)
  (dir-p nil))

(defun %close-fd (fd)
  (when (and (integerp fd) (not (minusp fd)))
    (sb-unix:unix-close fd)))

(defun close-wasi-descriptor (descriptor)
  "Release DESCRIPTOR's fd when the descriptor owns it. Idempotent."
  (when (wasi-descriptor-p descriptor)
    (let ((fd (wasi-descriptor-fd descriptor)))
      (when (and (wasi-descriptor-owned-p descriptor)
                 (integerp fd) (not (minusp fd)))
        (setf (wasi-descriptor-fd descriptor) -1)
        (%close-fd fd)))))

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
  "Release the preopen's host directory handle."
  (when (wasi-preopen-p preopen)
    (%close-fd (wasi-preopen-fd preopen))
    (setf (wasi-preopen-fd preopen) -1)))

(defun descriptor-resource-ct (&key (package (wasi-package "filesystem")))
  "The `descriptor' resource's ct for PACKAGE — the handle table's
type identity for every descriptor handle (the provider functypes,
the exported type and the drop finalizer all share it)."
  (wasmdown.wit:wit-type->ct
   (wasmdown.wit:package-interface package "types")
   (list :resource "descriptor")
   :packages (list package)))

(defparameter *descriptor-dtor-installed* (make-hash-table :test #'eq))

(defun %install-descriptor-dtor (ct)
  "Register the host finalizer for the descriptor resource: canon
resource.drop has no guest dtor func-instance for a host-provided
resource, so it calls this with the rep (the wasi-descriptor)."
  (unless (gethash ct *descriptor-dtor-installed*)
    (setf (gethash ct *descriptor-dtor-installed*) t)
    (setf (gethash ct
                   wasmdown.component.runtime:*host-resource-dtor-table*)
          #'close-wasi-descriptor))
  ct)

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
    ;; the authority check: the descriptor must belong to a preopen
    ;; of THIS provider's context — descriptors never travel between
    ;; differently-authorized providers.
    (unless (member (wasi-descriptor-preopen desc)
                    (wasi-context-preopens context))
      (%fs-error "not-permitted"))
    desc))

(defun %descriptor-fd (desc)
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
  "A host SOURCE over pread(FD, ..., POS): (max) -> (values byte-list
eof-p). POS advances with every read and the kernel file offset is
never consulted. A short/empty read is EOF and SETTLEs the future
(:ok); an OS error SETTLEs it (:err . code) and also reports EOF (the
source drops its end, so the guest's read sees DROPPED)."
  (let ((pos offset))
    (lambda (max)
      (let ((want (min max *fs-stream-chunk*)))
        (if (<= want 0)
            (values nil nil)
            (let ((buf (sb-alien:make-alien sb-alien:unsigned-char want)))
              (unwind-protect
                   (let ((n (%libc-pread fd buf want pos)))
                     (cond
                       ((minusp n)
                        (funcall settle (cons :err (errno->error-code
                                                    (sb-alien:get-errno))))
                        (values nil t))
                       ((zerop n)
                        (funcall settle (cons :ok nil))
                        (values nil t))
                       (t
                        (let ((bytes
                                (loop for i below n
                                      collect (sb-sys:sap-ref-8
                                               (sb-alien:alien-sap buf) i))))
                          (incf pos n)
                          (when (< n want)
                            (funcall settle (cons :ok nil)))
                          (values bytes (< n want))))))
                (sb-alien:free-alien buf))))))))

(defun %byte-list->alien (bytes)
  "A fresh unsigned-char alien holding BYTES (a u8 element list)."
  (let ((buf (sb-alien:make-alien sb-alien:unsigned-char (length bytes))))
    (loop for i from 0
          for byte in bytes
          do (setf (sb-sys:sap-ref-8 (sb-alien:alien-sap buf) i)
                   (logand #xff byte)))
    buf))

(defun %fd-pwrite-sink (fd position-fn advance-fn)
  "A host SINK writing BYTES at POSITION-FN's offset each drain:
error-code label or NIL. ADVANCE-FN records the bytes written (the
write-via-stream cursor advances; append re-reads the file's size
every drain — O_APPEND/EOF-append semantics)."
  (lambda (bytes)
    (if (null bytes)
        nil
        (let ((n (length bytes)))
          (if (zerop n)
              nil
              (let ((buf (%byte-list->alien bytes)))
                (unwind-protect
                     (let ((done 0)
                           (pos (funcall position-fn fd)))
                       (loop while (< done n)
                             do (let* ((ptr (sb-alien:sap-alien
                                             (sb-sys:sap+ (sb-alien:alien-sap buf)
                                                          done)
                                             (* sb-alien:unsigned-char)))
                                       (written (%libc-pwrite fd ptr (- n done)
                                                              pos)))
                                  (cond
                                    ((minusp written)
                                     (return (errno->error-code
                                              (sb-alien:get-errno))))
                                    ((zerop written)
                                     (return "io"))
                                    (t
                                     (incf done written)
                                     (setf pos (funcall advance-fn pos
                                                        written))))))
                       nil)
                  (sb-alien:free-alien buf))))))))

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
              (let ((settled nil))
                (flet ((settle (value)
                         (%fs-settle-once settled future-writer value)))
                  (wasmdown.component.runtime:host-end-set-source
                   stream-writer (%fd-pread-source fd offset #'settle))
                  (wasmdown.component.runtime:host-end-set-on-drop
                   stream-writer
                   (lambda (dropped error)
                     (declare (ignore dropped))
                     (settle (if error (cons :err error) (cons :ok nil)))))
                  (list stream-reader future-reader))))))
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
            (let ((settled nil))
              (flet ((settle (value)
                       (%fs-settle-once settled future-writer value)))
                (wasmdown.component.runtime:host-end-set-sink
                 data (%fd-pwrite-sink fd position-fn advance-fn))
                (wasmdown.component.runtime:host-end-set-on-drop
                 data
                 (lambda (dropped error)
                   (declare (ignore dropped))
                   (settle (if error (cons :err error) (cons :ok nil)))))
                ;; the guest may have staged a write before the call
                (wasmdown.component.runtime:host-end-drain data)
                future-reader))))
      (wasi-fs-error (c)
        ;; the refused write: the guest's end learns DROPPED on its
        ;; next write, the future carries the code
        (when (wasmdown.component.runtime:async-end-p data)
          (wasmdown.component.runtime:host-end-drop data))
        (%fs-error-future future-ct (wasi-fs-error-code c))))))

(defun fs-write-via-stream (context self data offset)
  "pwrite-backed write stream starting at OFFSET."
  (let ((pos offset))
    (%fs-write-stream
     context self data "write-via-stream"
     (lambda (fd) (declare (ignore fd)) pos)
     (lambda (p n) (+ p n)))))

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
  "A host SOURCE over the fdopendir stream DIR (whose fds is DIRFD):
(min) -> (values entry-list eof-p). `.` and `..` are omitted (the
WIT's rule). At the end of the directory SETTLE the future (:ok); a
readdir error SETTLEs (:err . code) and reports EOF."
  (lambda (max)
    (if (<= max 0)
        (values nil nil)
        (let ((entries '())
              (eof nil))
          ;; the requested bound counts DELIVERED entries: `.`/`..` are
          ;; skipped inside the walk (a skipped entry must not turn the
          ;; guest's read into a zero-progress completion).
          (loop while (and (< (length entries) max) (not eof))
                do (sb-alien:set-errno 0)
                   (let ((p (%libc-readdir dir)))
                     (cond
                       ((sb-sys:sap= (sb-alien:alien-sap p) (sb-sys:int-sap 0))
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
                            (push (%dirent-entry p dirfd) entries)))))))
          (values (nreverse entries) eof)))))

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
  (let ((ct (descriptor-resource-ct :package package)))
    (%install-descriptor-dtor ct)
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

(defun filesystem-preopens-instance (context
                                     &key (package (wasi-package "filesystem")))
  (let ((ct (descriptor-resource-ct :package package)))
    (%install-descriptor-dtor ct)
    (provider-instance
     (wasmdown.wit:package-interface package "preopens")
     (list (cons "get-directories"
                 (lambda () (fs-get-directories context ct))))
     :packages (%filesystem-packages :package package))))

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
(defparameter +pollin+ 1)
(defparameter +pollout+ 4)
(defparameter +eintr+ 4)
(defparameter +eagain+ 11)
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

(defparameter *socket-dtor-installed* (make-hash-table :test #'eq))

(defun %install-socket-dtors (tcp-ct udp-ct)
  "Register the host finalizers for the two socket resources: canon
resource.drop calls them with the rep, releasing the OS fd."
  (unless (gethash tcp-ct *socket-dtor-installed*)
    (setf (gethash tcp-ct *socket-dtor-installed*) t)
    (setf (gethash tcp-ct wasmdown.component.runtime:*host-resource-dtor-table*)
          #'close-wasi-tcp-socket))
  (unless (gethash udp-ct *socket-dtor-installed*)
    (setf (gethash udp-ct *socket-dtor-installed*) t)
    (setf (gethash udp-ct wasmdown.component.runtime:*host-resource-dtor-table*)
          #'close-wasi-udp-socket))
  t)

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
  (declare (ignore context))
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
            (wasmdown.component.runtime:host-end-set-source
             writer (%tcp-accept-source sock))
            (wasmdown.component.runtime:host-end-set-on-drop
             writer (lambda (dropped error)
                      (declare (ignore dropped error))
                      (%release-tcp sock)))
            (cons :ok reader)))
      (wasi-socket-error (c)
        (cons :err (wasi-socket-error-code c))))))

(defun %tcp-accept-source (sock)
  "A host SOURCE yielding new tcp-socket OWN HANDLES: (max) ->
(values element-list eof-p). EAGAIN returns what was accepted and
parks (eof NIL); a fatal error ends the stream."
  (let ((ct (tcp-socket-resource-ct))
        (family (wasi-tcp-socket-family sock)))
    (lambda (max)
      (if (<= max 0)
          (values nil nil)
          (let ((out '())
                (eof nil))
            (loop while (and (< (length out) max) (not eof))
                  do (sb-alien:set-errno 0)
                     (let ((fd (%libc-accept4 (wasi-tcp-socket-fd sock)
                                              (%sock-null) (%sock-null)
                                              (logior +sock-nonblock+
                                                      +sock-cloexec+))))
                       (cond
                         ((>= fd 0)
                          (push (list :own-handle ct
                                      (make-wasi-tcp-socket
                                       :fd fd
                                       :network (wasi-tcp-socket-network sock)
                                       :family family
                                       :state :connected))
                                out))
                         (t
                          (let ((errno (sb-alien:get-errno)))
                            (cond ((or (= errno +eagain+) (= errno +eintr+))
                                   ;; park: the guest may retry the read
                                   nil)
                                  (t (setf eof t))))))))
            (values (nreverse out) eof))))))

(defun %fd-send-all (fd bytes)
  "Send BYTES to the stream socket FD; returns an error-code DOMAIN
value or NIL. EAGAIN waits (bounded) for POLLOUT; MSG_NOSIGNAL keeps
a peer reset from killing the host."
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
                                 (return-from write (%socket-code "connection-broken")))
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
                                        (%socket-errno-code errno)))))))))
                   nil))
            (sb-alien:free-alien buf))))))

(defun tcp-send (context network self data
                 &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let ((future-ct (%sockets-future-ct "tcp-socket" "send"
                                        :package package)))
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
                  (released nil))
              (flet ((settle (v)
                       (unless settled
                         (setf settled t)
                         (wasmdown.component.runtime:host-future-resolve
                          writer v)))
                     (release (&optional shutdown-how)
                       (unless released
                         (setf released t)
                         (%release-tcp sock shutdown-how))))
                (incf (wasi-tcp-socket-refs sock))
                (wasmdown.component.runtime:host-end-set-sink
                 data (lambda (bytes) (%fd-send-all fd bytes)))
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

(defun %tcp-recv-source (fd settle)
  "A host SOURCE over recv(2): (max) -> (values byte-list eof-p).
A would-block result gets one bounded poll(2) readiness wait (there
is no socket reactor to wake the guest later); when nothing arrives
the source parses as no-progress and the guest's read parks. A
zero-length recv is a graceful FIN and SETTLES the future (:ok); a
recv error SETTLES it (:err . code) and reports EOF."
  (lambda (max)
    (if (<= max 0)
        (values nil nil)
        (let ((want (min max 65536)))
          (loop
            (let ((buf (sb-alien:make-alien sb-alien:unsigned-char want)))
              (unwind-protect
                   (let ((n (%libc-recv fd (sb-alien:alien-sap buf) want 0)))
                     (cond
                       ((plusp n)
                        (return
                          (values (loop for i below n
                                        collect (sb-sys:sap-ref-8
                                                 (sb-alien:alien-sap buf) i))
                                  nil)))
                       ((zerop n)
                        (funcall settle (cons :ok nil))
                        (return (values nil t)))
                       (t
                        (let ((errno (sb-alien:get-errno)))
                          (cond
                            ((or (= errno +eagain+) (= errno +eintr+))
                             (unless (%poll-fd fd +pollin+ 2000)
                               (return (values nil nil))))
                            (t
                             (funcall settle
                                      (cons :err (%socket-errno-code errno)))
                             (return (values nil t))))))))
                (sb-alien:free-alien buf))))))))

(defun tcp-receive (context network self
                    &key (package (wasi-package "sockets")))
  (declare (ignore context))
  (let* ((ft (%sockets-method-ct "tcp-socket" "receive" :package package))
         (result (second (wasmdown.component:ct-payload ft)))
         (elem-ct (wasmdown.component:ct-payload
                   (%ct-find-kind result :stream)))
         (future-ct (wasmdown.component:ct-payload
                     (%ct-find-kind result :future))))
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
                    (released nil))
                (flet ((settle (v)
                         (unless settled
                           (setf settled t)
                           (wasmdown.component.runtime:host-future-resolve
                            future-writer v)))
                       (release (&optional shutdown-how)
                         (unless released
                           (setf released t)
                           (%release-tcp sock shutdown-how))))
                  (incf (wasi-tcp-socket-refs sock))
                  (wasmdown.component.runtime:host-end-set-source
                   stream-writer (%tcp-recv-source fd #'settle))
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
         (pkg (%sockets-packages :package package)))
    (%install-socket-dtors tcp-ct udp-ct)
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
                   (ip-name-lookup-resolve context net name
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
comes from wasi:clocks/types)."
  (list package (wasi-package "clocks")))

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

(defparameter +http-max-field-name-size+ 256)
(defparameter +http-max-field-value-size+ 65536)
(defparameter +http-max-fields-total-size+ (* 1024 1024))

;;; RFC 7230 tchar (the token production).
(defparameter +http-tchar-chars+
  "!#$%&'*+-.^_`|~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "The HTTP token character set (RFC 7230 §3.2.6).")

(defparameter +http-forbidden-field-names+
  '("connection" "content-length" "host" "keep-alive" "proxy-connection"
    "te" "trailer" "transfer-encoding" "upgrade")
  "The field names the HTTP implementation owns: a guest cannot set
them (RFC 7230 §6.1 + the wasi:http forbidden set). Case-insensitive.")

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

(defstruct (wasi-fields (:constructor %make-wasi-fields (entries mutable-p)))
  "The `fields' resource rep: ENTRIES is the ordered list of
(original-name . value-bytes) pairs (one pair per value), and
MUTABLE-P is the WIT's mutability bit (a `fields' returned by
request.headers / response.headers is immutable)."
  (entries nil)
  (mutable-p t))

(defun make-wasi-fields (&key entries (mutable-p t))
  "A `fields' from (name . value) entries; values may be strings,
vectors or u8 lists."
  (%make-wasi-fields
   (loop for (name . value) in entries
         collect (cons name (%field-value-bytes value)))
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
        (append (wasi-fields-entries fields) (list (cons name value)))))

(defun %fields-delete (fields name)
  (setf (wasi-fields-entries fields)
        (remove name (wasi-fields-entries fields)
                :key #'car :test #'string-equal)))

(defun %fields-size-check (fields name values)
  "The implementation limit check: NAME + VALUES fit the per-field and
aggregate limits (header-error.size-exceeded otherwise)."
  (cond
    ((> (length name) +http-max-field-name-size+)
     (%http-header-error "size-exceeded"))
    ((some (lambda (v) (> (length v) +http-max-field-value-size+)) values)
     (%http-header-error "size-exceeded"))
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
           (%http-header-error "size-exceeded")
           (cons :ok nil))))))

;;; The WIT methods.

(defun fields-from-list (entries)
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
                  (%http-header-error "size-exceeded")))
               ((> (length value) +http-max-field-value-size+)
                (return-from fields-from-list
                  (%http-header-error "size-exceeded")))))
    (let ((fields (make-wasi-fields
                   :entries (loop for entry in entries
                                  collect (cons (first entry)
                                                (%field-value-bytes
                                                 (second entry)))))))
      (if (> (%fields-total-size fields) +http-max-fields-total-size+)
          (%http-header-error "size-exceeded")
          (list :own-handle (http-resource-ct "fields") fields)))))

(defun fields-get (fields name &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((f (%fields-of fields (http-resource-ct "fields"))))
    (if (%http-token-p name) (%fields-values f name) '())))

(defun fields-has (fields name &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((f (%fields-of fields (http-resource-ct "fields"))))
    (and (%http-token-p name) (not (null (%fields-values f name))))))

(defun fields-set (fields name values &key (package (wasi-package "http")))
  (declare (ignore package))
  (let* ((f (%fields-of fields (http-resource-ct "fields")))
         (values (mapcar #'%field-value-bytes (coerce values 'list))))
    (cond
      ((not (wasi-fields-mutable-p f)) (%http-header-error "immutable"))
      ((not (%http-token-p name)) (%http-header-error "invalid-syntax"))
      ((%http-forbidden-field-name-p name) (%http-header-error "forbidden"))
      ((not (every #'%http-field-value-ok-p values))
       (%http-header-error "invalid-syntax"))
      (t (let ((check (%fields-size-check f name values)))
           (if (eq (car check) :ok)
               (progn (%fields-replace f name values) (cons :ok nil))
               check))))))

(defun fields-delete (fields name &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((f (%fields-of fields (http-resource-ct "fields"))))
    (if (not (wasi-fields-mutable-p f))
        (%http-header-error "immutable")
        (progn (%fields-delete f name) (cons :ok nil)))))

(defun fields-get-and-delete (fields name
                              &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((f (%fields-of fields (http-resource-ct "fields"))))
    (if (not (wasi-fields-mutable-p f))
        (%http-header-error "immutable")
        (let ((values (if (%http-token-p name) (%fields-values f name) '())))
          (%fields-delete f name)
          (cons :ok values)))))

(defun fields-append (fields name value
                      &key (package (wasi-package "http")))
  (declare (ignore package))
  (let* ((f (%fields-of fields (http-resource-ct "fields")))
         (value (%field-value-bytes value)))
    (cond
      ((not (wasi-fields-mutable-p f)) (%http-header-error "immutable"))
      ((not (%http-token-p name)) (%http-header-error "invalid-syntax"))
      ((%http-forbidden-field-name-p name) (%http-header-error "forbidden"))
      ((not (%http-field-value-ok-p value))
       (%http-header-error "invalid-syntax"))
      (t (let ((check (%fields-size-check f name (list value))))
           (if (eq (car check) :ok)
               (progn (%fields-append f name value) (cons :ok nil))
               check))))))

(defun fields-copy-all (fields &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((f (%fields-of fields (http-resource-ct "fields"))))
    (loop for (name . value) in (wasi-fields-entries f)
          collect (list name (copy-list value)))))

(defun fields-clone (fields &key (package (wasi-package "http")))
  (declare (ignore package))
  (let* ((f (%fields-of fields (http-resource-ct "fields")))
         (copy (make-wasi-fields
                :entries (loop for (name . value)
                                 in (wasi-fields-entries f)
                               collect (cons name (copy-list value))))))
    (list :own-handle (http-resource-ct "fields") copy)))

(defun fields-constructor (&key (package (wasi-package "http")))
  (declare (ignore package))
  (list :own-handle (http-resource-ct "fields") (make-wasi-fields)))

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

(defun request-options-constructor (&key (package (wasi-package "http")))
  (declare (ignore package))
  (list :own-handle (http-resource-ct "request-options")
        (make-wasi-http-options)))

(defun request-options-get-connect-timeout (options
                                            &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-options-connect-timeout
   (%options-of options (http-resource-ct "request-options"))))

(defun request-options-set-connect-timeout (options duration
                                            &key (package (wasi-package "http")))
  (declare (ignore package))
  (%options-set-timeout
   (%options-of options (http-resource-ct "request-options"))
   :connect duration))

(defun request-options-get-first-byte-timeout (options
                                               &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-options-first-byte-timeout
   (%options-of options (http-resource-ct "request-options"))))

(defun request-options-set-first-byte-timeout (options duration
                                               &key (package (wasi-package "http")))
  (declare (ignore package))
  (%options-set-timeout
   (%options-of options (http-resource-ct "request-options"))
   :first-byte duration))

(defun request-options-get-between-bytes-timeout (options
                                                  &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-options-between-bytes-timeout
   (%options-of options (http-resource-ct "request-options"))))

(defun request-options-set-between-bytes-timeout (options duration
                                                  &key (package (wasi-package "http")))
  (declare (ignore package))
  (%options-set-timeout
   (%options-of options (http-resource-ct "request-options"))
   :between-bytes duration))

(defun request-options-clone (options &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((o (%options-of options (http-resource-ct "request-options"))))
    (list :own-handle
          (http-resource-ct "request-options")
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

(defun %http-scheme-ok-p (scheme)
  (and (consp scheme)
       (cond
         ((string-equal (car scheme) "other")
          (let ((s (cdr scheme)))
            (and (stringp s) (plusp (length s))
                 (alpha-char-p (char s 0))
                 (every (lambda (c)
                          (or (alpha-char-p c) (digit-char-p c)
                              (find c "+-." :test #'char=)))
                        s))))
         (t (member (car scheme) '("HTTP" "HTTPS") :test #'string=)))))

(defun %http-path-ok-p (path)
  "origin-form: \"/\" or \"*\", then visible ASCII (no space, CR, LF,
NUL; a fragment `#' is not part of a request target)."
  (and (stringp path)
       (plusp (length path))
       (or (char= (char path 0) #\/) (string= path "*"))
       (every (lambda (c)
                (let ((code (char-code c)))
                  (and (<= 33 code 126) (not (char= c #\#)))))
              path)))

(defun %http-authority-ok-p (authority)
  "authority = host [ \":\" port ]; userinfo is not accepted."
  (and (stringp authority)
       (plusp (length authority))
       (every (lambda (c)
                (let ((code (char-code c)))
                  (and (<= 33 code 126)
                       (not (find c "/?#@" :test #'char=)))))
              authority)
       ;; the port, when present, must be digits
       (let* ((host-port (if (and (char= (char authority 0) #\[)
                                  (find #\] authority))
                             (let ((close (position #\] authority)))
                               (if (< (1+ close) (length authority))
                                   (subseq authority (1+ close))
                                   ""))
                             (let ((colon (position #\: authority)))
                               (if colon (subseq authority colon) "")))))
         (or (string= host-port "")
             (progn (unless (char= (char host-port 0) #\:) (return-from %http-authority-ok-p nil))
                    (let ((digits (subseq host-port 1)))
                      (and (plusp (length digits))
                           (every #'digit-char-p digits)
                           (<= (ignore-errors (parse-integer digits)) 65535))))))))

(defun %http-split-authority (authority)
  "authority -> (values host-without-brackets port)"
  (if (and (char= (char authority 0) #\[)
           (find #\] authority))
      (let* ((close (position #\] authority))
             (host (subseq authority 1 close))
             (rest (subseq authority (1+ close))))
        (values host
                (if (and (plusp (length rest)) (char= (char rest 0) #\:))
                    (parse-integer (subseq rest 1))
                    80)))
      (let ((colon (position #\: authority)))
        (if colon
            (values (subseq authority 0 colon)
                    (parse-integer (subseq authority (1+ colon))))
            (values authority 80)))))

(defun %http-method-string (method)
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
  (body-buffer nil)                   ; reversed list of byte-lists
  (body-length 0)
  (body-closed-p nil)
  (body-error nil)
  (trailers-end nil)                  ; the readable trailers future
  (transmission-writer nil)           ; request.new's result future
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
  (let ((w (wasi-http-request-transmission-writer request)))
    (when w
      (setf (wasi-http-request-transmission-writer request) nil)
      (wasmdown.component.runtime:host-future-resolve w value))))

(defun http-request-new (headers contents trailers options
                         &key (package (wasi-package "http")))
  "request.new: construct the request, take the contents stream's
readable end as the body sink, and return (request, transmission
future). The body is fully staged by the guest before send."
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
        (setf (wasi-http-request-trailers-end req) trailers-end))
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
        (if body
            (progn
              (wasmdown.component.runtime:host-end-set-sink
               body
               (lambda (bytes)
                 (push bytes (wasi-http-request-body-buffer req))
                 (incf (wasi-http-request-body-length req) (length bytes))
                 nil))
              (wasmdown.component.runtime:host-end-set-on-drop
               body
               (lambda (dropped error)
                 (declare (ignore dropped))
                 (setf (wasi-http-request-body-closed-p req) t)
                 (if error
                     (progn
                       (setf (wasi-http-request-body-error req) error)
                       (%http-request-settle req (cons :err error)))
                     (%http-request-settle req (cons :ok nil)))))
              (wasmdown.component.runtime:host-end-drain body))
            (%http-request-settle req (cons :ok nil)))
        (list (list :own-handle request-ct req) future-reader)))))

(defun http-request-get-method (this &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-request-method (%request-of this (http-resource-ct "request"))))

(defun http-request-set-method (this method
                                &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((req (%request-of this (http-resource-ct "request"))))
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
  (let ((req (%request-of this (http-resource-ct "request"))))
    (cond
      ((eq path :none) (setf (wasi-http-request-path-with-query req) :none)
       (cons :ok nil))
      ((and (consp path) (eq (car path) :some))
       (if (%http-path-ok-p (cdr path))
           (progn (setf (wasi-http-request-path-with-query req) path)
                  (cons :ok nil))
           (cons :err nil)))
      (t (if (%http-path-ok-p path)
             (progn (setf (wasi-http-request-path-with-query req)
                          (cons :some path))
                    (cons :ok nil))
             (cons :err nil))))))

(defun http-request-get-scheme (this &key (package (wasi-package "http")))
  (declare (ignore package))
  (wasi-http-request-scheme (%request-of this (http-resource-ct "request"))))

(defun http-request-set-scheme (this scheme
                                &key (package (wasi-package "http")))
  (declare (ignore package))
  (let ((req (%request-of this (http-resource-ct "request"))))
    (cond
      ((eq scheme :none) (setf (wasi-http-request-scheme req) :none)
       (cons :ok nil))
      ((and (consp scheme) (eq (car scheme) :some))
       (if (%http-scheme-ok-p (cdr scheme))
           (progn (setf (wasi-http-request-scheme req) scheme) (cons :ok nil))
           (cons :err nil)))
      (t (if (%http-scheme-ok-p scheme)
             (progn (setf (wasi-http-request-scheme req) (cons :some scheme))
                    (cons :ok nil))
             (cons :err nil))))))

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
               (lambda (bytes)
                 (let ((state (or (wasi-http-response-body resp)
                                  (make-wasi-http-body))))
                   (setf (wasi-http-response-body resp) state)
                   (setf (wasi-http-body-buffer state)
                         (append (wasi-http-body-buffer state) bytes))
                   nil)))
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
    (%make-wasi-fields (nreverse entries) nil)))

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
                   (progn
                     (setf (wasi-http-body-trailers body)
                           (%http-read-fields reader :timeout-ms timeout)
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

(defun %http-request-trailers (req)
  "The request's trailers, read non-blockingly: (values trailers
present-p) where TRAILERS is a wasi-fields or NIL."
  (multiple-value-bind (value present)
      (%http-future-value (wasi-http-request-trailers-end req))
    (if (not present)
        (values nil nil)
        (cond
          ((and (consp value) (eq (car value) :err))
           (%http-error (cdr value)))
          ((and (consp value) (eq (car value) :ok))
           (let ((opt (cdr value)))
             (cond
               ((eq opt :none) (values nil t))
               ((and (consp opt) (eq (car opt) :some))
                (let ((handle (cdr opt)))
                  (if (and (consp handle) (eq (car handle) :own-handle))
                      (values (third handle) t)
                      (values (and (wasi-fields-p handle) handle) t))))
               (t (values nil t)))))
          (t (values nil t))))))

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

(defun %http-write-request (conn req body trailers trailers-present)
  "Write the whole request (head + body framing). Signals
wasi-http-error on an I/O or serialization failure."
  (let* ((method (%http-method-string (wasi-http-request-method req)))
         (authority (let ((a (wasi-http-request-authority req)))
                      (if (consp a) (cdr a) a)))
         (path (let ((p (wasi-http-request-path-with-query req)))
                 (if (and (consp p) (eq (car p) :some)) (cdr p) "/")))
         (chunked-p trailers-present)
         (body-present (or (wasi-http-request-body-end req)
                           (plusp (wasi-http-request-body-length req))))
         (extra (append
                 (list (format nil "Host: ~a" authority)
                       "Connection: close")
                 (cond
                   (chunked-p (list "Transfer-Encoding: chunked"))
                   ((or body-present (plusp (length body)))
                    (list (format nil "Content-Length: ~d" (length body))))
                   (t '()))))
         (write (wasi-http-connection-write-fn conn)))
    (funcall write
             (%http-serialize-request-head
              method path authority (wasi-http-request-headers req) extra))
    (cond
      (chunked-p
       (unless (null body)
         (%http-error
          (or (funcall write (append (%http-chunk-prefix (length body))
                                     body (list 13 10)))
              nil)))
       (%http-error
        (or (funcall write (map 'list #'char-code
                                (format nil "0~c~c" #\Return #\Newline)))
            (and trailers (funcall write (%http-serialize-trailers trailers)))))
       (when (null trailers)
         ;; the terminating chunk + an empty trailer section
         (%http-error (funcall write (list 13 10)))))
      (t
       (when body (funcall write body))))
    nil))

(defun http-client-send (context request
                         &key (network nil) (package (wasi-package "http")))
  "client.send: write the HTTP/1.1 request over the transport and
return (:ok own<response>) or (:err . error-code)."
  (let* ((net (or network (first (wasi-context-networks context))))
         (request-ct (http-resource-ct "request" :package package)))
    (handler-case
        (let* ((req (%request-of (%http-take-resource request request-ct)
                                 request-ct))
               (scheme (%http-scheme-string (wasi-http-request-scheme req)))
               (authority (wasi-http-request-authority req))
               (method (wasi-http-request-method req)))
          (when (wasi-http-request-consumed-p req)
            (%http-error (%http-internal "request already consumed")))
          (when (eq authority :none)
            (%http-error (%http-code "HTTP-request-URI-invalid")))
          (let ((authority-string (if (consp authority) (cdr authority)
                                      authority)))
            (unless (%http-authority-ok-p authority-string)
              (%http-error (%http-code "HTTP-request-URI-invalid")))
            (multiple-value-bind (host port)
                (%http-split-authority authority-string)
              (when (and (string-equal scheme "https") (= port 80))
                (setf port 443))
              (let* ((transport (%http-transport-for context scheme))
                     (destination (%http-resolve-destination
                                   context net host port))
                     (options (wasi-http-request-options req))
                     (connect-timeout
                       (%http-timeout-ms
                        options #'wasi-http-options-connect-timeout
                        +http-default-connect-timeout-ms+)))
                (unless transport
                  (%http-error (%http-code "configuration-error")))
                (let ((conn (funcall (wasi-http-transport-connect transport)
                                     host port destination connect-timeout)))
                  (handler-case
                      (let* ((body (%http-request-body-bytes req))
                             (body-error (wasi-http-request-body-error req)))
                        (when body-error
                          (%http-error body-error))
                        (when (and (wasi-http-request-body-end req)
                                   (not (wasi-http-request-body-closed-p req)))
                          (%http-error
                           (%http-internal
                            "request body stream must be closed before client.send (streaming uploads need a reactor)")))
                        (multiple-value-bind (trailers trailers-present)
                            (%http-request-trailers req)
                          (%http-write-request conn req body trailers
                                               trailers-present))
                        (let* ((reader
                                 (make-wasi-http-reader
                                  :conn conn
                                  :timeout-ms
                                  (%http-timeout-ms
                                   options
                                   #'wasi-http-options-first-byte-timeout
                                   +http-default-first-byte-timeout-ms+)))
                               (head-method
                                 (if (string-equal
                                      (%http-method-string method)
                                      "HEAD")
                                     :head :other)))
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
                                (cons :ok
                                      (list :own-handle
                                            (http-resource-ct
                                             "response" :package package)
                                            response)))))))
                    (error (c)
                      (%http-connection-close conn)
                      (error c))))))))
      (wasi-http-error (c) (cons :err (wasi-http-error-code c))))))

;;; ---------------------------------------------------------------
;;; The provider instances.

(defun %http-install-dtors (&key (package (wasi-package "http")))
  "The request/response resources hold stream ends and OS sockets;
canon resource.drop runs these host finalizers."
  (let ((rt (http-resource-ct "request" :package package))
        (st (http-resource-ct "response" :package package)))
    (unless (gethash rt wasmdown.component.runtime:*host-resource-dtor-table*)
      (setf (gethash rt wasmdown.component.runtime:*host-resource-dtor-table*)
            (lambda (rep)
              (when (wasi-http-request-p rep)
                (when (wasi-http-request-body-end rep)
                  (ignore-errors
                    (wasmdown.component.runtime:host-end-drop
                     (wasi-http-request-body-end rep)))
                  (setf (wasi-http-request-body-end rep) nil))
                (%http-request-settle rep (cons :ok nil))))))
    (unless (gethash st wasmdown.component.runtime:*host-resource-dtor-table*)
      (setf (gethash st wasmdown.component.runtime:*host-resource-dtor-table*)
            (lambda (rep)
              (when (wasi-http-response-p rep)
                (%http-body-close (wasi-http-response-body rep))))))
    t))

(defun http-types-instance (context
                            &key (package (wasi-package "http")))
  "The wasi:http/types import: the fields/request/request-options/
response resources."
  (declare (ignore context))
  (%http-install-dtors :package package)
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
   :packages (%http-packages :package package)))

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
;;; explicit WASI-CONTEXT (PLAN.md §9.1), never an ambient global.

(defun command-provider (context)
  "The full `command' world import set as a with-args alist: cli
(environment/exit/stdio/terminals), clocks, random, filesystem
(types + preopens), sockets (types + ip-name-lookup) and http (types
+ client), all bound to CONTEXT."
  (append (cli-provider context)
          (clocks-provider)
          (random-provider)
          (filesystem-provider context)
          (sockets-provider context)
          (http-provider context)))

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
