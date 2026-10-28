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
capability — never an ambient global)."
  (arguments nil)
  (environment nil)
  (cwd nil)
  (stdin nil)
  (stdout nil)
  (stderr nil))

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
