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
  (preopens nil))

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
is a plain func whose first param is the self borrow)."
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
     :payload (list (coerce (if (wasmdown.wit:wit-func-static-p method)
                                params
                                (cons self params))
                            'vector)
                    (second (wasmdown.component:ct-payload ct))
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
  "The full wasi:cli provider: environment, exit and the stdio streams
(whose authority is CONTEXT's stdin/stdout/stderr slots)."
  (append
   (list
    (cons (wasmdown.wit:interface-import-name
           (wasmdown.wit:package-interface package "environment"))
          (cli-environment-instance context :package package))
    (cons (wasmdown.wit:interface-import-name
           (wasmdown.wit:package-interface package "exit"))
          (cli-exit-instance :package package)))
   (cli-stdio-provider context :package package)))

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
