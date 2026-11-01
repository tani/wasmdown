;;;; WASMDOWN — WASI 0.2 (Preview2) provider facade.
;;;;
;;;; WASI 0.2 is the *release* line whose stable interfaces are
;;;; `wasi:io`, `wasi:clocks`, `wasi:random`, `wasi:cli`,
;;;; `wasi:filesystem`, `wasi:sockets` and `wasi:http`, each imported
;;;; at an EXACT patch version (`@0.2.0` .. `@0.2.12`). It is a
;;;; component-model resource ABI, unlike Preview1's linear memory.
;;;;
;;;; This file builds the provider registry for those interfaces:
;;;;   * the WIT is loaded from the pinned release trees through the
;;;;     versioned WASI package cache (specs/wasi/index.txt);
;;;;   * every interface is bound under the EXACT import name the
;;;;     component declares, never a renamed label;
;;;;   * resource reps are host objects carrying the native capability
;;;;     (the same native authorities the 0.3 providers use), so a P2
;;;;     resource and a P3 resource never share an ABI shape;
;;;;   * an unknown interface, a missing method or a wrong version is
;;;;     a construction/link error BEFORE any guest start.

(in-package #:wasmdown.wasi)

;;; ---------------------------------------------------------------
;;; Release identity: import name -> (namespace package interface
;;; version), and the parsed package set for one exact version.

(defun p2-import-name-parts (name)
  "Split `wasi:<pkg>/<iface>@<version>' into (values pkg iface
version). NIL for a name that is not a versioned wasi interface."
  (when (and (stringp name)
             (search "wasi:" name :test #'char=)
             (plusp (length name)))
    (let* ((slash (position #\/ name))
           (at (position #\@ name))
           (colon (position #\: name)))
      (when (and slash at colon (< colon slash) (< slash at))
        (values (subseq name (1+ colon) slash)
                (subseq name (1+ slash) at)
                (subseq name (1+ at)))))))

(defun p2-version-p (version)
  (and (stringp version)
       (plusp (length version))
       (every (lambda (c) (or (digit-char-p c) (char= c #\.))) version)))

(defun p2-packages (version)
  "Every vendored WASI package parsed at VERSION, as a list. A package
that the release does not ship (e.g. wasi:io at 0.3.2) is skipped."
  (declare (ignore))
  (loop for short in '("io" "clocks" "random" "cli" "filesystem" "sockets" "http")
        for pkg = (handler-case (wasi-package short :version version)
                    (wasmdown:link-error () nil))
        when pkg
          collect pkg))

(defun p2-package (short version)
  (wasi-package short :version version))

(defun p2-interface (short iface version)
  (wasmdown.wit:package-interface (p2-package short version) iface))

(defun p2-interface-name (short iface version)
  "The canonical import name of an interface at VERSION."
  (wasmdown.wit:interface-import-name (p2-interface short iface version)))

;;; ---------------------------------------------------------------
;;; The command world.

(defun preview2-command-world (&key (version "0.2.12"))
  "The resolved `wasi:cli/command' world of VERSION. The world's
imports/exports are version-exact; resolving against the whole
package set keeps `use' paths resolvable."
  (unless (p2-version-p version)
    (error 'wasmdown:link-error
           :message (format nil "wasi 0.2 version must be numeric, got ~s" version)))
  (let ((packages (p2-packages version)))
    (unless (find version (mapcar (lambda (p) (wasmdown.wit::wit-package-version p))
                                  packages)
                  :test #'string=)
      (error 'wasmdown:link-error
             :message (format nil "no vendored wasi packages at ~a" version)))
    (wasmdown.wit:resolve-wit-world
     (p2-package "cli" version) "command" :packages packages)))

;;; ---------------------------------------------------------------
;;; Host resources for the P2 ABI.
;;;
;;; A resource REP is a plain host object; methods receive the handle
;;; INDEX and resolve it through HOST-RESOURCE-REP with the resource's
;;; ct (exactly as the 0.3 providers do).

(defstruct (p2-error (:constructor %make-p2-error (message &optional (kind "other"))))
  "A wasi:io/error error object: a debug string plus the P2 error
kind it should downcast to."
  (message "" :type string)
  (kind "other" :type string))

(defun p2-error (message &optional (kind "other"))
  (%make-p2-error message kind))

(defun p2-error-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "io" "error" version)
   (list :resource "error")
   :packages (p2-packages version)))

(defun p2-resource-rep (ct value what)
  "The REP behind a P2 resource handle: VALUE is either the handle
INDEX the canonical ABI passed in, or the rep itself for a DIRECT CL
call (the same rep-or-index convention the descriptor helpers use).
Real lowering always passes an index."
  (cond
    ;; a direct CL call may pass the own-handle marker itself
    ((and (consp value) (eq (car value) :own-handle)) (third value))
    ((not (integerp value)) value)
    (t (or (wasmdown.component.runtime:host-resource-rep ct value)
           (error 'wasmdown:host-error
                  :message (format nil "~a: no live resource rep for handle ~a"
                                   what value))))))

;;; ---- pollable ---------------------------------------------------

(defconstant +p2-poll-slice-ns+ 50000000
  "How long `poll`/`pollable.block` waits on one pollable before
re-scanning the whole set. `poll' must report EVERY ready input, and a
host without a real epoll cannot watch several fds at once, so it waits
in short slices: at most ~50 ms of extra latency, no busy spin.")

(defstruct (p2-pollable (:constructor %make-p2-pollable (&key ready-p block)))
  "A wasi:io/poll pollable: READY-P answers readiness now, BLOCK waits
(up to a caller-supplied timeout in ns, NIL = until ready) and returns
T when the event fired."
  (ready-p (lambda () t) :type function)
  (block nil))

(defun p2-pollable-wait (rep timeout-ns)
  "Wait on one pollable until it is ready or TIMEOUT-NS expires.
Returns T when ready. A pollable with no BLOCK sleeps one slice so a
never-ready source can never spin the host."
  (if (funcall (p2-pollable-ready-p rep))
      t
      (let ((block (p2-pollable-block rep)))
        (if block
            (not (null (funcall block timeout-ns)))
            (progn (sleep (/ (or timeout-ns +p2-poll-slice-ns+) 1000000000.0))
                   nil)))))

(defun p2-pollable-from-deadline (deadline-ns)
  "A pollable that becomes ready at DEADLINE-NS on the monotonic
clock."
  (%make-p2-pollable
   :ready-p (lambda () (>= (monotonic-now) deadline-ns))
   :block (lambda (timeout-ns)
            (let* ((now (monotonic-now))
                   (wait (max 0 (- deadline-ns now)))
                   (wait (if timeout-ns (min wait timeout-ns) wait)))
              (when (plusp wait)
                (sleep (/ wait 1000000000.0)))
              (>= (monotonic-now) deadline-ns)))))

(defun p2-pollable-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "io" "poll" version)
   (list :resource "pollable")
   :packages (p2-packages version)))

;;; ---------------------------------------------------------------
;;; Stability gating.
;;;
;;; A release may DECLARE a function that is not stable in it (0.2.11
;;; declares `exit-with-code` under `@unstable(feature =
;;; cli-exit-with-code)`; 0.2.12 makes it `@since(version = 0.2.12)`).
;;; An exact-version binding must export only the STABLE surface.

(defun p2-node-annotations (node)
  (typecase node
    (wasmdown.wit:wit-func (wasmdown.wit:wit-func-annotations node))
    (wasmdown.wit:wit-resource (wasmdown.wit:wit-resource-annotations node))
    (wasmdown.wit:wit-type (wasmdown.wit:wit-type-annotations node))
    (wasmdown.wit:wit-interface (wasmdown.wit:wit-interface-annotations node))
    (t nil)))

(defun p2-stable-node-p (node)
  "NODE carries no `@unstable(feature = ...)` annotation."
  (not (find "unstable"
             (mapcar #'wasmdown.wit:wit-annotation-name
                     (p2-node-annotations node))
             :test #'string=)))

;;; ---------------------------------------------------------------
;;; Interface implementations (function/method level).

(defun p2-monotonic-clock-instance (version)
  (provider-instance
   (p2-interface "clocks" "monotonic-clock" version)
   (list (cons "now" (lambda () (monotonic-now)))
         (cons "resolution" (lambda () (monotonic-resolution)))
         (cons "subscribe-instant"
               (lambda (when) (p2-pollable-from-deadline when)))
         (cons "subscribe-duration"
               (lambda (how-long)
                 (p2-pollable-from-deadline (+ (monotonic-now) how-long)))))
   :packages (p2-packages version)
   :include #'p2-stable-node-p))

(defun p2-wall-clock-instance (version)
  (provider-instance
   (p2-interface "clocks" "wall-clock" version)
   (list (cons "now" (lambda () (system-now)))
         (cons "resolution" (lambda () (system-resolution))))
   :packages (p2-packages version)
   :include #'p2-stable-node-p))

(defun p2-random-instance (version)
  (provider-instance
   (p2-interface "random" "random" version)
   (list (cons "get-random-bytes" #'random-bytes)
         (cons "get-random-u64" #'random-u64))
   :packages (p2-packages version)
   :include #'p2-stable-node-p))

(defun p2-insecure-instance (version)
  (provider-instance
   (p2-interface "random" "insecure" version)
   (list (cons "get-insecure-random-bytes" #'insecure-random-bytes)
         (cons "get-insecure-random-u64" #'insecure-random-u64))
   :packages (p2-packages version)
   :include #'p2-stable-node-p))

(defun p2-insecure-seed-instance (version)
  (provider-instance
   (p2-interface "random" "insecure-seed" version)
   ;; 0.2 spells it `insecure-seed'; there is a 0.3 rename that must
   ;; NOT be used here
   (list (cons "insecure-seed" #'insecure-seed))
   :packages (p2-packages version)
   :include #'p2-stable-node-p))

(defun p2-environment-instance (context version)
  (provider-instance
   (p2-interface "cli" "environment" version)
   (list (cons "get-environment"
               (lambda ()
                 ;; list<tuple<string,string>>: each TUPLE is a proper
                 ;; two-element list; the ABI iterates it element-wise,
                 ;; so a dotted pair is a malformed tuple.
                 (loop for (k . v) in (wasi-context-environment context)
                       collect (list k v))))
         (cons "get-arguments"
               (lambda () (wasi-context-arguments context)))
         (cons "initial-cwd"
               (lambda ()
                 (let ((cwd (wasi-context-cwd context)))
                   ;; the option domain shape: (:some . payload)
                   (if cwd (cons :some cwd) :none)))))
   :packages (p2-packages version)
   :include #'p2-stable-node-p))

(defun p2-exit-instance (version)
  (provider-instance
   (p2-interface "cli" "exit" version)
   (list (cons "exit"
               (lambda (status)
                 ;; `status: result' arrives as the variant value
                 ;; (:ok . payload) / (:err . payload)
                 (%wasi-exit (if (and (consp status) (eq (car status) :ok))
                                 0 1))))
         ;; exit-with-code is `@since(version = 0.2.12)': only that
         ;; release declares it, so the provider table is checked
         ;; against the parsed interface rather than assumed.
         (cons "exit-with-code"
               (lambda (code) (%wasi-exit code))))
   :packages (p2-packages version)
   :include #'p2-stable-node-p))


;;; ---------------------------------------------------------------
;;; The provider registry.
;;;
;;; Keyed by the EXACT canonical import name; a component's declared
;;; external label is used only as the binding key, never to look up
;;; the implementation.

(defun p2-builder-table (context version)
  "An alist canonical-import-name -> thunk producing that interface's
provider instance for CONTEXT at VERSION."
  (let ((packages (p2-packages version))
        (builders '()))
    (flet ((bind (short iface thunk)
             (push (cons (p2-interface-name short iface version) thunk)
                   builders)))
      (bind "clocks" "monotonic-clock"
            (lambda () (p2-monotonic-clock-instance version)))
      (bind "clocks" "wall-clock"
            (lambda () (p2-wall-clock-instance version)))
      (bind "random" "random"
            (lambda () (p2-random-instance version)))
      (bind "random" "insecure"
            (lambda () (p2-insecure-instance version)))
      (bind "random" "insecure-seed"
            (lambda () (p2-insecure-seed-instance version)))
      (bind "cli" "environment"
            (lambda () (p2-environment-instance context version)))
      (bind "cli" "exit"
            (lambda () (p2-exit-instance version)))
      (setf builders (p2-register-io-cli context version builders))
      (setf builders (p2-register-filesystem context version builders))
      (setf builders (p2-register-sockets context version builders)))
    ;; an interface that declares only types needs a provider with no
    ;; implementations: it exists for its exported type identities
    (dolist (short '("clocks" "random" "cli" "io" "filesystem" "sockets" "http"))
      (let ((pkg (handler-case (p2-package short version)
                   (wasmdown:link-error () nil))))
        (when pkg
          (dolist (iface (wasmdown.wit::wit-package-interface-list pkg))
            (let ((name (wasmdown.wit:interface-import-name iface)))
              (unless (assoc name builders :test #'string=)
                (when (and (null (wasmdown.wit::wit-interface-func-list iface))
                           (null (wasmdown.wit::wit-interface-resource-list iface)))
                  (push (cons name
                              (lambda ()
                                (provider-instance iface nil
                                                   :packages packages)))
                        builders))))))))
    (nreverse builders)))

(defun preview2-command-provider (context &key (version "0.2.12") imports)
  "Bind every import the component DECLARES to a provider of VERSION.
IMPORTS is the declared contract list of import names; NIL means the
whole `wasi:cli/command' world. An interface with no provider is a
link error naming it and the version; a types-only import needs no
host binding."
  (let* ((builders (p2-builder-table context version))
         (declared (or imports
                       (loop for item in
                             (wasmdown.wit:wit-world-imports
                              (preview2-command-world :version version))
                             collect (wasmdown.wit:interface-import-name
                                      (wasmdown.wit:wit-world-item-target item)))))
         (providers '()))
    (dolist (name declared)
      (let ((hit (assoc name builders :test #'string=)))
        (cond
          ;; HTTP is excluded globally, not just on the 0.3 path: an
          ;; exact-version 0.2 import must get the same canonical
          ;; message instead of a generic "no provider".
          ((%wasi-http-import-name-p name)
           (error 'wasmdown.wit:wit-error
                  :message +wasi-http-unsupported-message+))
          (hit (push (cons name (funcall (cdr hit))) providers))
          ;; a types-only import carries no host binding
          ((let ((iface (p2-interface-by-name name)))
             (and iface
                  (null (wasmdown.wit::wit-interface-func-list iface))
                  (null (wasmdown.wit::wit-interface-resource-list iface))))
           nil)
          (t
           (error 'wasmdown:link-error
                  :message (format nil "wasi ~a interface `~a` has no provider"
                                   version name))))))
    (nreverse providers)))

(defun p2-interface-by-name (name)
  "The parsed interface whose canonical import name is NAME, across
every vendored package, or NIL."
  (multiple-value-bind (short iface version) (p2-import-name-parts name)
    (when (and short iface version)
      (handler-case (p2-interface short iface version)
        (wasmdown:link-error () nil)
        (wasmdown.wit:wit-error () nil)))))

(defun p2-in-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "io" "streams" version)
   (list :resource "input-stream")
   :packages (p2-packages version)))

(defun p2-out-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "io" "streams" version)
   (list :resource "output-stream")
   :packages (p2-packages version)))

(defstruct (p2-input-stream (:constructor %make-p2-input-stream
                                  (&key stream fd position-offset)))
  "An input stream: a CL stream, or a host fd with an optional
positional offset (a file stream created by read-via-stream)."
  (stream nil)
  (fd -1 :type fixnum)
  (position-offset nil)
  (closed nil))

(defstruct (p2-output-stream (:constructor %make-p2-output-stream
                                   (&key stream fd position-offset appending)))
  "An output stream: a CL stream, or a host fd with an optional
positional offset (write-via-stream) or append semantics."
  (stream nil)
  (fd -1 :type fixnum)
  (position-offset nil)
  (appending nil)
  (closed nil)
  ;; the permit the last `check-write' granted, i.e. "the number of
  ;; bytes permitted for the next call to `write'" (io/streams
  ;; output-stream.check-write); NIL = no check-write yet
  (permit nil)
  ;; the unwritten suffix of an offered write whose sink accepted fewer
  ;; bytes than offered (a short host write, or EWOULDBLOCK). RETAINED,
  ;; never dropped: `flush'/the blocking operations hand it to the
  ;; sink, and `check-write' reports 0 while it is non-empty.
  (pending nil))

(defconstant +p2-io-chunk+ (* 1024 1024)
  "The largest single host buffer a P2 stream operation allocates (1
MiB). The WIT lets `read' return fewer bytes than `len' and
`check-write' cap a `write', so a guest asking for more is served in
bounded chunks; allocating the caller's u64 directly would abort the
host on the SBCL heap instead of answering the guest.")

(defconstant +p2-write-permit+ +p2-io-chunk+
  "The byte count `output-stream.check-write` reports: the largest
single `write` this host hands to a sink in one call. WIT io/streams
`output-stream.check-write`: \"Returns the number of bytes permitted for
the next call to `write` ... Calling `write` with more bytes than this
function has permitted will trap\", and `output-stream.write`:
\"Precondition: check-write gave permit of Ok(n) and contents has a
length of less than or equal to n. Otherwise, this function will trap.\"
P2-OUT-CHECK-PERMIT enforces that, so an over-permit write is a trap,
never a silently truncated sink.")

(defun p2-closed-error ()
  "The WIT `stream-error::closed` value."
  (cons :closed nil))

(defun p2-failed-error (message version)
  "The WIT `stream-error::last-operation-failed` value. Its payload is
`own<error>', so the domain value is an OWN-HANDLE MARKER (the runtime
allocates the handle when it lowers the result), never a bare rep."
  (cons :last-operation-failed
        (list :own-handle (p2-error-ct version) (p2-error message))))


(defun %p2-char->octets (char)
  "The octets a character contributes to an input-stream: one octet in
the Latin-1 range (so a byte pipe such as stdin round-trips
unchanged), the UTF-8 encoding above it (the WIT's rule for a text
source)."
  (if (< (char-code char) 256)
      (vector (char-code char))
      (sb-ext:string-to-octets (string char) :external-format :utf-8)))

(defun %p2-read-text (stream len)
  "Read up to LEN octets from a TEXT stream. Returns an octet vector,
:closed at end of input. A character that does not fit the request is
pushed back; only a first character wider than the whole request is
truncated (the guest asked for fewer octets than the code point)."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (loop while (< (fill-pointer out) len)
          do (let ((c (read-char stream nil nil)))
               (unless c (return))
               (let* ((bytes (%p2-char->octets c))
                      (room (- len (fill-pointer out))))
                 (cond
                   ((<= (length bytes) room)
                    (loop for b across bytes
                          do (vector-push-extend b out)))
                   ((zerop (fill-pointer out))
                    (loop for i from 0 below room
                          do (vector-push-extend (aref bytes i) out)))
                   (t (unread-char c stream)
                      (return))))))
    (if (zerop (fill-pointer out))
        :closed
        (coerce out '(vector (unsigned-byte 8))))))

(defun p2-would-block-p (errno)
  "ERRNO is EAGAIN (11) / EWOULDBLOCK: the fd is nonblocking and the
operation cannot make prompt progress. WIT io/streams: a `read` whose
source has no bytes \"available for reading at this time\" answers an
EMPTY list, not `last-operation-failed`; `write` \"never blocks\" either."
  (and (integerp errno) (or (= errno 11) (= errno 35))))

(defun p2-in-read (rep len)
  "Read at most LEN octets from an input-stream rep, in one bounded
chunk, never blocking. WIT io/streams `input-stream.read`: \"The list is
empty when no bytes are available for reading at this time\" and \"When
the caller gives a `len` of 0, it represents a request to read 0 bytes.
If the stream is still open, this call should succeed and return an
empty list, or otherwise fail with `closed`.\" So an EMPTY octet vector
is `ok(list<u8>)` for an OPEN stream with nothing promptly available
\(a nonblocking fd's EWOULDBLOCK) and for every zero-length request;
`:closed` is the stream's own ended/closed state. Returns an octet
vector, :closed, or (:failed . errno) when the host read itself failed."
  (let ((len (min len +p2-io-chunk+)))
    (cond
      ((p2-input-stream-closed rep) :closed)
      ((p2-input-stream-stream rep)
       (let ((stream (p2-input-stream-stream rep)))
         (cond
           ;; a zero-length read is a request for 0 bytes, never an EOF
           ;; probe: `listen' on a pipe-backed CL stream may block or
           ;; consume, and answering `closed' made an OPEN stream look
           ;; closed to the guest
           ((zerop len) #())
           ((subtypep (stream-element-type stream) '(unsigned-byte 8))
            (let ((buf (make-array len :element-type '(unsigned-byte 8))))
              (let ((n (read-sequence buf stream)))
                (if (zerop n) :closed (subseq buf 0 n)))))
           ;; a TEXT stream (the CLI hands P2 stdin as a character
           ;; stream): reading into a ub8 vector raises TYPE-ERROR, so
           ;; the characters are mapped octet by octet instead
           (t (%p2-read-text stream len)))))
      ((not (minusp (p2-input-stream-fd rep)))
       (cond
         ;; a zero-length read is a request for 0 bytes, not an eof probe
         ((zerop len) #())
         (t
          (let* ((buf (make-array len :element-type '(unsigned-byte 8)))
                 (off (p2-input-stream-position-offset rep))
                 (n (if off
                        (%libc-pread (p2-input-stream-fd rep)
                                     (sb-alien:sap-alien (sb-sys:vector-sap buf)
                                                         (* sb-alien:unsigned-char))
                                     len off)
                        (%p1-libc-read (p2-input-stream-fd rep)
                                       (sb-sys:vector-sap buf) len))))
            (when (and off (plusp n))
              (incf (p2-input-stream-position-offset rep) n))
            (cond ((minusp n)
                   (let ((errno (sb-alien:get-errno)))
                     ;; EWOULDBLOCK: the stream is OPEN and simply has no
                     ;; bytes promptly available — the WIT's empty list,
                     ;; NOT last-operation-failed
                     (if (p2-would-block-p errno)
                         #()
                         (cons :failed errno))))
                  ((zerop n) :closed)
                  (t (subseq buf 0 n)))))))
      (t :closed))))

(defun p2-in-skip (rep len)
  "Consume at most LEN octets. Returns the count consumed, :closed, or
\(:failed . errno) when the host read itself failed."
  (let ((data (p2-in-read rep len)))
    (cond ((eq data :closed) :closed)
          ((and (consp data) (eq (car data) :failed)) data)
          (t (length data)))))

(defun p2-out-write-n (rep bytes)
  "One host write of BYTES to REP. Returns (values OCTETS-WRITTEN
STATE) with STATE :ok, :closed, or (:failed . errno). An APPENDING rep
positions at the end first (append-via-stream is the WIT's \"write with
O_APPEND\", whatever the descriptor's own flags are)."
  (cond
    ((p2-output-stream-closed rep) (values 0 :closed))
    ((p2-output-stream-stream rep)
     (let ((stream (p2-output-stream-stream rep))
           (vec (coerce bytes '(vector (unsigned-byte 8)))))
       (if (subtypep (stream-element-type stream) '(unsigned-byte 8))
           (write-sequence vec stream)
           (write-string (map 'string #'code-char vec) stream))
       (values (length vec) :ok)))
    ((not (minusp (p2-output-stream-fd rep)))
     (let* ((vec (coerce bytes '(vector (unsigned-byte 8))))
            (off (p2-output-stream-position-offset rep))
            (fd (p2-output-stream-fd rep)))
       (when (and (p2-output-stream-appending rep) (minusp (%p1-libc-lseek fd 0 2)))
         (return-from p2-out-write-n (values 0 (cons :failed (sb-alien:get-errno)))))
       (let ((n (if off
                    (%libc-pwrite fd
                                  (sb-alien:sap-alien (sb-sys:vector-sap vec)
                                                      (* sb-alien:unsigned-char))
                                  (length vec) off)
                    (%p1-libc-write fd (sb-sys:vector-sap vec) (length vec)))))
         (if (minusp n)
             (values 0 (cons :failed (sb-alien:get-errno)))
             (progn (when off (incf (p2-output-stream-position-offset rep) n))
                    (values n :ok))))))
    (t (values 0 :closed))))

(defun p2-out-permit (rep)
  "The permit REP's `check-write` grants now, recorded for the next
`write`. WIT io/streams `output-stream.check-write`: \"Returns the number
of bytes permitted for the next call to `write`, or an error ... When
this function returns 0 bytes, the `subscribe` pollable will become
ready when this function will report at least 1 byte, or an error.\"
While a retained unwritten suffix is still buffered the sink is not
accepting data (the guest's `flush` has not completed), so the permit
is 0."
  (let ((permit (if (p2-output-stream-pending rep) 0 +p2-write-permit+)))
    (setf (p2-output-stream-permit rep) permit)
    permit))

(defun p2-out-check-permit (rep len what)
  "Enforce the `check-write` precondition for an offer of LEN bytes.
WIT io/streams `output-stream.write`: \"Precondition: check-write gave
permit of Ok(n) and contents has a length of less than or equal to n.
Otherwise, this function will trap\", and `write-zeroes`: \"should be
used precisely like `write` with the exact same preconditions (must use
check-write first)\". A guest that never called `check-write` is given
the permit `check-write` would report, so the enforced rule is exactly
the WIT's: LEN must not exceed the permit."
  (let ((permit (p2-out-permit rep)))
    (when (> len permit)
      (error 'wasmdown:wasm-trap
             :message
             (format nil "wasi:io/streams ~a: ~d byte(s) offered but check-write permitted ~d"
                     what len permit)))
    t))

(defun p2-out-drain-pending (rep &key block)
  "Hand REP's retained unwritten suffix to the sink. Non-blocking by
default (one attempt), so `flush' can honour \"This function never
blocks\" (WIT `output-stream.flush'); with BLOCK, wait for the sink and
finish, which is what `blocking-flush' / `blocking-write-and-flush'
promise. Whatever the sink does not take stays buffered. Returns T,
:closed, or (:failed . errno)."
  (let ((buf (p2-output-stream-pending rep)))
    (when (null buf) (return-from p2-out-drain-pending t))
    (loop
      (multiple-value-bind (n state) (p2-out-write-n rep buf)
        (cond
          ((eq state :ok)
           (cond
             ;; the whole suffix went out
             ((>= n (length buf))
              (setf (p2-output-stream-pending rep) nil)
              (return t))
             (t
              ;; a SHORT write: retain exactly the unwritten suffix, so
              ;; the next attempt (or the next write) continues where
              ;; the sink stopped instead of dropping guest bytes
              (setf buf (subseq buf n)
                    (p2-output-stream-pending rep) buf)
              (unless block (return t))
              ;; a sink that accepted nothing while we must block:
              ;; wait for it rather than spin
              (when (zerop n)
                (let ((p (p2-output-subscribe-pollable rep)))
                  (loop until (funcall (p2-pollable-ready-p p))
                        do (p2-pollable-wait p +p2-poll-slice-ns+)))))))
          ((eq state :closed) (return :closed))
          ((and (consp state) (eq (car state) :failed)
                (p2-would-block-p (cdr state)))
           (if block
               (let ((p (p2-output-subscribe-pollable rep)))
                 (loop until (funcall (p2-pollable-ready-p p))
                       do (p2-pollable-wait p +p2-poll-slice-ns+)))
               (return t)))
          (t (return state)))))))

(defun p2-out-flush-pending (rep)
  "Hand REP's buffered suffix to the sink as far as it accepts it right
now (never blocking). Looping matters for the WIT's readiness contract:
`check-write` \"returns 0 bytes\" only while the sink is genuinely full,
so the `subscribe` pollable — a POLLOUT watch on the fd — agreeing with
it makes a polling guest sleep instead of spin. Returns T, :closed, or
\(:failed . errno)."
  (loop
    (let* ((before (if (p2-output-stream-pending rep)
                       (length (p2-output-stream-pending rep))
                       0))
           (r (p2-out-drain-pending rep)))
      (unless (eq r t) (return r))
      (let ((after (if (p2-output-stream-pending rep)
                       (length (p2-output-stream-pending rep))
                       0)))
        (when (or (zerop after) (>= after before))
          (return t))))))

(defun p2-out-write (rep bytes)
  "Offer BYTES to REP without blocking. WIT `output-stream.write`:
\"This function never blocks.\" Everything the sink accepts is handed
over; the unwritten suffix is RETAINED in REP's pending buffer instead
of being dropped, so a short host write never loses guest data and the
ok result still covers the whole list. The caller has already enforced
the `check-write` permit. Returns T on success, :closed when the stream
is already closed, or (:failed . errno) when the HOST write itself
failed — the two are distinct WIT values (`closed' vs
`last-operation-failed'), so a broken sink is never reported as a
closed stream."
  (cond
    ((p2-output-stream-closed rep) :closed)
    (t
     (let* ((vec (coerce bytes '(vector (unsigned-byte 8))))
            (pend (p2-output-stream-pending rep))
            (buf (if pend
                     (concatenate '(vector (unsigned-byte 8)) pend vec)
                     vec)))
       ;; bytes already accepted by the guest stay FIRST, in order
       (when (plusp (length buf))
         (setf (p2-output-stream-pending rep) buf))
       (p2-out-flush-pending rep)))))

(defun p2-out-flush (rep)
  "Flush REP. WIT `output-stream.flush': \"Request to flush buffered
output. This function never blocks. ... the output which is expected to
be flushed is all that has been passed to `write` prior to this call.
Upon calling this function, the `output-stream' will not accept any
writes (`check-write` will return `ok(0)`) until the flush has
completed.\" The retained suffix is offered to the sink now; whatever
it does not take stays buffered, so check-write keeps answering ok(0)
and `subscribe` reports readiness when the sink can take more. Returns
T, :closed (stream already closed), or (:failed . errno) when the host
flush failed."
  (cond ((p2-output-stream-closed rep) :closed)
        (t
         (let ((drained (p2-out-flush-pending rep)))
           (if (not (eq drained t))
               drained
               (cond
                 ((and (p2-output-stream-stream rep)
                       (null (p2-output-stream-pending rep)))
                  (let ((stream (p2-output-stream-stream rep)))
                    (handler-case (progn (force-output stream) t)
                      (stream-error () (cons :failed nil)))))
                 (t t)))))))

(defun p2-out-sink-result (value version what)
  "The `result<_, stream-error>` for an output-stream operation whose
sink helper returned VALUE: T = ok, :closed = stream-error::closed,
\(:failed . errno) = stream-error::last-operation-failed carrying a
downcastable error."
  (cond
    ((eq value t) (cons :ok nil))
    ((eq value :closed) (cons :err (p2-closed-error)))
    (t (cons :err
             (p2-failed-error
              (if (cdr value)
                  (format nil "~a failed (errno ~a)" what (cdr value))
                  (format nil "~a failed" what))
              version)))))

(defun p2-out-write-zeroes (rep len)
  "Write LEN zero octets to REP in bounded chunks (never allocating
LEN). WIT `output-stream.write-zeroes`: \"This should be used precisely
like `write` with the exact same preconditions (must use check-write
first), but instead of passing a list of bytes, you simply pass the
number of zero-bytes that should be written.\" The permit is enforced
by the caller; the bytes go through the same retain-the-suffix path as
`write', so a short host write loses nothing. Returns T, :closed, or
\(:failed . errno)."
  (let ((chunk (make-array 4096 :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (loop with remaining = len
          while (plusp remaining)
          do (let* ((n (min remaining (length chunk)))
                    (r (p2-out-write rep (if (= n (length chunk))
                                             chunk
                                             (subseq chunk 0 n)))))
               (unless (eq r t) (return r))
               (decf remaining n))
          finally (return t))))

(defun p2-blocking-in (rep len)
  "The shared body of `blocking-read'/`blocking-skip'/`blocking-splice':
read LEN octets, waiting for the source instead of reporting the
nonblocking `read` path's outcomes. WIT `blocking-read`: \"Read bytes
from a stream, after blocking until at least one byte can be read.
Except for blocking, behavior is identical to `read`.\" The empty list
`read` answers for a source with no bytes promptly available is exactly
that \"no byte yet\" signal, so it waits; a source that has ended still
answers :closed, and a zero-length request still answers ok with an
empty list at once."
  (if (zerop len)
      #()
      (loop
        (let ((r (p2-in-read rep len)))
          (cond
            ((or (and (vectorp r) (zerop (length r)))
                 (and (consp r) (eq (car r) :failed)
                      (p2-would-block-p (cdr r))))
             (let ((p (p2-input-subscribe-pollable rep)))
               (loop until (funcall (p2-pollable-ready-p p))
                     do (p2-pollable-wait p +p2-poll-slice-ns+))))
            (t (return r)))))))

(defun p2-blocking-in-skip (rep len)
  "`blocking-skip' / `skip' over the blocking read path."
  (let ((data (p2-blocking-in rep len)))
    (cond ((eq data :closed) :closed)
          ((and (consp data) (eq (car data) :failed)) data)
          (t (length data)))))

(defun p2-blocking-out-write (rep bytes)
  "Write ALL of BYTES to REP: the retained suffix plus BYTES is offered
to the sink until nothing is left, waiting for a nonblocking sink
\(EWOULDBLOCK). WIT `blocking-write-and-flush`: \"Block until all of
these operations are complete, or an error occurs\" and \"Returns
success when all of the contents written are successfully flushed to
output\", so no byte may be dropped here."
  (cond
    ((p2-output-stream-closed rep) :closed)
    (t
     (let* ((vec (coerce bytes '(vector (unsigned-byte 8))))
            (pend (p2-output-stream-pending rep))
            (buf (if pend
                     (concatenate '(vector (unsigned-byte 8)) pend vec)
                     vec)))
       (setf (p2-output-stream-pending rep)
             (if (plusp (length buf)) buf nil))
       (p2-out-drain-pending rep :block t)))))

(defun p2-out-write-zeroes-blocking (rep len)
  "P2-OUT-WRITE-ZEROES with the blocking write path."
  (let ((chunk (make-array 4096 :element-type '(unsigned-byte 8)
                                :initial-element 0)))
    (loop with remaining = len
          while (plusp remaining)
          do (let* ((n (min remaining (length chunk)))
                    (r (p2-blocking-out-write
                        rep (if (= n (length chunk)) chunk (subseq chunk 0 n)))))
               (unless (eq r t) (return r))
               (decf remaining n))
          finally (return t))))

(defun p2-error-instance (version)
  "wası:io/error — the `error` resource with to-debug-string."
  (let ((ct (p2-error-ct version)))
    (provider-instance
     (p2-interface "io" "error" version)
     (list (cons "to-debug-string"
                 (lambda (self)
                   (let ((rep (p2-resource-rep ct self "error.to-debug-string")))
                     (if (p2-error-p rep) (p2-error-message rep) (format nil "~a" rep))))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-poll-instance (context version)
  "wası:io/poll — the pollable resource and the interface `poll'."
  (declare (ignore context))
  (let ((ct (p2-pollable-ct version)))
    (labels ((rep (self what) (p2-resource-rep ct self what)))
      (provider-instance
       (p2-interface "io" "poll" version)
       (list (cons "ready"
                   (lambda (self)
                     (not (null (funcall (p2-pollable-ready-p
                                          (rep self "pollable.ready")))))))
             (cons "block"
                   ;; `block' returns only once the pollable is ready
                   (lambda (self)
                     (let ((r (rep self "pollable.block")))
                       (loop until (funcall (p2-pollable-ready-p r))
                             do (p2-pollable-wait r +p2-poll-slice-ns+))
                       t)))
             (cons "poll"
                   (lambda (handles)
                     ;; WIT: "This function traps if ... the list is
                     ;; empty", and it waits until at least one input is
                     ;; ready (a timeout is a CLOCK pollable in the same
                     ;; list, never an argument).
                     (when (null handles)
                       (error 'wasmdown:wasm-trap
                              :message "wasi:io/poll.poll: the pollable list is empty"))
                     (let ((reps (mapcar (lambda (h) (rep h "poll")) handles)))
                       (loop
                         ;; report every ready input index, in INPUT
                         ;; ORDER, duplicates included
                         (let ((ready (loop for r in reps
                                            for i from 0
                                            when (funcall (p2-pollable-ready-p r))
                                              collect i)))
                           (when ready (return ready)))
                         (let ((blocker (find-if #'p2-pollable-block reps)))
                           (unless blocker
                             (error 'wasmdown:wasm-trap
                                    :message "wasi:io/poll.poll: no pollable can be waited on"))
                           (p2-pollable-wait blocker +p2-poll-slice-ns+)))))))
       :packages (p2-packages version)
       :include #'p2-stable-node-p))))

(defun p2-input-subscribe-pollable (rep)
  "A pollable for an input-stream rep: resolves when the host fd has
bytes available or reached hangup/error, or when the stream is closed.
A CL-backed stream has no readiness probe, so it stays always-ready
(claiming otherwise would deadlock a waiting guest)."
  (if (or (p2-input-stream-closed rep)
          (minusp (p2-input-stream-fd rep)))
      (%make-p2-pollable
       :ready-p (lambda () t)
       :block (lambda (timeout) (declare (ignore timeout)) t))
      (%p2-pollable-from-fd
       (p2-input-stream-fd rep)
       (logior +pollin+ +pollhup+ +pollerr+ +pollnval+))))

(defun p2-output-subscribe-pollable (rep)
  "A pollable for an output-stream rep: resolves when the sink can
accept bytes again, and is always ready once the stream is closed."
  (if (or (p2-output-stream-closed rep)
          (minusp (p2-output-stream-fd rep)))
      (%make-p2-pollable
       :ready-p (lambda () t)
       :block (lambda (timeout) (declare (ignore timeout)) t))
      (%p2-pollable-from-fd
       (p2-output-stream-fd rep)
       (logior +pollout+ +pollhup+ +pollerr+ +pollnval+))))

(defun p2-streams-instance (context version)
  "wası:io/streams — input-stream/output-stream resources."
  (let ((in-ct (p2-in-ct version))
        (out-ct (p2-out-ct version))
        (poll-ct (p2-pollable-ct version)))
    (labels ((in (self what)
               (p2-resource-rep in-ct self what))
             (out (self what)
               (p2-resource-rep out-ct self what))
             (wrap-read (result)
               (cond
                 ((eq result :closed) (cons :err (p2-closed-error)))
                 ((and (consp result) (eq (car result) :failed))
                  (cons :err (p2-failed-error
                              (format nil "read failed (errno ~a)" (cdr result))
                              version)))
                 (t (cons :ok (coerce result 'list)))))
             (wrap-skip (result)
               (cond ((eq result :closed) (cons :err (p2-closed-error)))
                     ((and (consp result) (eq (car result) :failed))
                      (cons :err (p2-failed-error
                                  (format nil "skip failed (errno ~a)" (cdr result))
                                  version)))
                     (t (cons :ok result)))))
      (declare (ignore context))
      (provider-instance
       (p2-interface "io" "streams" version)
       (list
        ;; ---- input-stream
        (cons "read"
              (lambda (self len) (wrap-read (p2-in-read (in self "read") len))))
        (cons "blocking-read"
              (lambda (self len)
                (wrap-read (p2-blocking-in (in self "blocking-read") len))))
        (cons "skip"
              (lambda (self len) (wrap-skip (p2-in-skip (in self "skip") len))))
        (cons "blocking-skip"
              (lambda (self len)
                (wrap-skip (p2-blocking-in-skip (in self "blocking-skip") len))))
        (cons "[method]input-stream.subscribe"
              (lambda (self)
                (list :own-handle poll-ct
                      (p2-input-subscribe-pollable (in self "subscribe")))))
        ;; ---- output-stream
        (cons "check-write"
              (lambda (self)
                (let ((rep (out self "check-write")))
                  (if (p2-output-stream-closed rep)
                      (cons :err (p2-closed-error))
                      ;; the permit is 0 until the flush the guest asked
                      ;; for has completed (WIT flush), so try the
                      ;; retained suffix first; a sink failure is a
                      ;; stream error, never a permit
                      (let ((d (if (p2-output-stream-pending rep)
                                   (p2-out-flush-pending rep)
                                   t)))
                        (if (eq d t)
                            (cons :ok (p2-out-permit rep))
                            (p2-out-sink-result d version "check-write")))))))
        (cons "write"
              (lambda (self bytes)
                (let ((rep (out self "write")))
                  (if (p2-output-stream-closed rep)
                      ;; WIT: "returns Err(closed) without writing if the
                      ;; stream has closed since the last call to
                      ;; check-write provided a permit"
                      (cons :err (p2-closed-error))
                      (progn
                        (p2-out-check-permit rep (length bytes) "write")
                        (p2-out-sink-result (p2-out-write rep bytes)
                                            version "write"))))))
        (cons "blocking-write-and-flush"
              (lambda (self bytes)
                (let* ((rep (out self "blocking-write-and-flush"))
                       (w (p2-blocking-out-write rep bytes)))
                  (if (eq w t)
                      (p2-out-sink-result (p2-out-flush rep) version "flush")
                      (p2-out-sink-result w version "write")))))
        (cons "flush"
              (lambda (self)
                (p2-out-sink-result (p2-out-flush (out self "flush"))
                                    version "flush")))
        (cons "blocking-flush"
              (lambda (self)
                ;; WIT: "Request to flush buffered output, and block
                ;; until flush completes" — the retained suffix must be
                ;; OUT before this returns
                (let* ((rep (out self "blocking-flush"))
                       (d (p2-out-drain-pending rep :block t)))
                  (if (eq d t)
                      (p2-out-sink-result (p2-out-flush rep) version "flush")
                      (p2-out-sink-result d version "flush")))))
        (cons "[method]output-stream.subscribe"
              (lambda (self)
                (list :own-handle poll-ct
                      (p2-output-subscribe-pollable (out self "subscribe")))))
        (cons "write-zeroes"
              (lambda (self len)
                (let ((rep (out self "write-zeroes")))
                  (if (p2-output-stream-closed rep)
                      (cons :err (p2-closed-error))
                      (progn
                        (p2-out-check-permit rep len "write-zeroes")
                        (p2-out-sink-result (p2-out-write-zeroes rep len)
                                            version "write-zeroes"))))))
        (cons "blocking-write-zeroes-and-flush"
              (lambda (self len)
                (let* ((rep (out self "blocking-write-zeroes-and-flush"))
                       (w (p2-out-write-zeroes-blocking rep len)))
                  (if (eq w t)
                      (p2-out-sink-result (p2-out-flush rep) version "flush")
                      (p2-out-sink-result w version "write-zeroes")))))
        (cons "splice"
              (lambda (self src len)
                ;; WIT `output-stream.splice`: "The behavior of splice is
                ;; equivalent to: 1. calling `check-write` on the
                ;; `output-stream` 2. calling `read` on the
                ;; `input-stream` with the smaller of the `check-write`
                ;; permitted length and the `len` provided to `splice`
                ;; 3. calling `write` on the `output-stream` with that
                ;; read data. Any error reported by the call to
                ;; `check-write`, `read`, or `write` ends the splice and
                ;; reports that error."
                (let* ((src-rep (p2-resource-rep in-ct src "splice"))
                       (dst (out self "splice")))
                  (if (p2-output-stream-closed dst)
                      (cons :err (p2-closed-error))
                      (let ((data (p2-in-read src-rep
                                              (min len (p2-out-permit dst)))))
                        (cond
                          ;; a source that has just ended transfers 0
                          ;; bytes; splice reports a COUNT, not the
                          ;; read's own error
                          ((eq data :closed) (cons :ok 0))
                          ((and (consp data) (eq (car data) :failed))
                           (cons :err (p2-failed-error
                                       (format nil "splice: read failed (errno ~a)"
                                               (cdr data))
                                       version)))
                          (t (let ((w (p2-out-write dst data)))
                               (if (eq w t)
                                   (cons :ok (length data))
                                   (p2-out-sink-result w version "splice"))))))))))
        (cons "blocking-splice"
              (lambda (self src len)
                (let* ((src-rep (p2-resource-rep in-ct src "blocking-splice"))
                       (dst (out self "blocking-splice"))
                       (data (p2-blocking-in src-rep len)))
                  (cond
                    ((eq data :closed) (cons :ok 0))
                    ((and (consp data) (eq (car data) :failed))
                     (cons :err (p2-failed-error
                                 (format nil "blocking-splice: read failed (errno ~a)"
                                         (cdr data))
                                 version)))
                    (t (let ((w (p2-blocking-out-write dst data)))
                         (if (eq w t)
                             (let ((f (p2-out-flush dst)))
                               (if (eq f t)
                                   (cons :ok (length data))
                                   (p2-out-sink-result f version "flush")))
                             (p2-out-sink-result w version "splice")))))))))
       :packages (p2-packages version)
       :include #'p2-stable-node-p))))

;;; ---------------------------------------------------------------
;;; wasi:cli — stdio streams and terminals.

(defun p2-stdin-instance (context version)
  (let ((ct (p2-in-ct version)))
    (provider-instance
     (p2-interface "cli" "stdin" version)
     (list (cons "get-stdin"
                 (lambda ()
                   (list :own-handle ct
                         (%make-p2-input-stream
                          :stream (wasi-context-stdin context))))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-stdout-instance (context version which)
  (let ((ct (p2-out-ct version)))
    (provider-instance
     (p2-interface "cli" which version)
     (list (cons (format nil "get-~a" which)
                 (lambda ()
                   (list :own-handle ct
                         (%make-p2-output-stream
                          :stream (if (string= which "stdout")
                                      (wasi-context-stdout context)
                                      (wasi-context-stderr context)))))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-terminal-resource-instance (version which)
  "wası:cli/terminal-input|terminal-output — a resource-only interface:
the provider exports the resource TYPE (its identity for the option
returned by terminal-stdin/stdout/stderr)."
  (provider-instance
   (p2-interface "cli" which version)
   nil
   :packages (p2-packages version)
   :include #'p2-stable-node-p))

(defun p2-terminal-stream-instance (context version which)
  "wası:cli/terminal-stdin|stdout|stderr: an option<terminal-*> for a
stream the context actually lent."
  (let* ((resource (if (string= which "terminal-stdin")
                       "terminal-input" "terminal-output"))
         (ct (wasmdown.wit:wit-type->ct
              (p2-interface "cli" resource version)
              (list :resource resource)
              :packages (p2-packages version)))
         (stream (cond ((string= which "terminal-stdin") (wasi-context-stdin context))
                       ((string= which "terminal-stdout") (wasi-context-stdout context))
                       (t (wasi-context-stderr context)))))
    (provider-instance
     (p2-interface "cli" which version)
     (list (cons (format nil "get-~a" which)
                 (lambda ()
                   (if (and stream (ignore-errors (interactive-stream-p stream)))
                       (cons :some (list :own-handle ct stream))
                       :none))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

;;; ---------------------------------------------------------------
;;; The registry, extended with the resource-bearing interfaces.

(defun p2-register-io-cli (context version builders)
  "Push the io/cli provider thunks onto BUILDERS (a local alist)."
  (flet ((bind (short iface thunk)
           (push (cons (p2-interface-name short iface version) thunk)
                 builders)))
    (bind "io" "error" (lambda () (p2-error-instance version)))
    (bind "io" "poll" (lambda () (p2-poll-instance context version)))
    (bind "io" "streams" (lambda () (p2-streams-instance context version)))
    (bind "cli" "stdin" (lambda () (p2-stdin-instance context version)))
    (bind "cli" "stdout" (lambda () (p2-stdout-instance context version "stdout")))
    (bind "cli" "stderr" (lambda () (p2-stdout-instance context version "stderr")))
    (bind "cli" "terminal-input"
          (lambda () (p2-terminal-resource-instance version "terminal-input")))
    (bind "cli" "terminal-output"
          (lambda () (p2-terminal-resource-instance version "terminal-output")))
    (bind "cli" "terminal-stdin"
          (lambda () (p2-terminal-stream-instance context version "terminal-stdin")))
    (bind "cli" "terminal-stdout"
          (lambda () (p2-terminal-stream-instance context version "terminal-stdout")))
    (bind "cli" "terminal-stderr"
          (lambda () (p2-terminal-stream-instance context version "terminal-stderr")))
    builders))

;;; ---------------------------------------------------------------
;;; Running a P2 command.

(defun p2-run-export (instance version)
  "The exported `wasi:cli/run@VERSION' `run' function of INSTANCE, or
a DIRECT top-level `run' (a reactor-style fixture)."
  (let ((name (format nil "wasi:cli/run@~a" version)))
    (flet ((from (value)
             (typecase value
               (wasmdown.component.runtime:comp-instance
                (wasmdown.component.runtime:runtime-export-value value "run"))
               (hash-table (gethash "run" value))
               (t nil))))
      (or (from (let ((hit (gethash name
                                    (wasmdown.component.runtime:comp-instance-exports
                                     instance)
                                    :missing)))
                  (unless (eq hit :missing) (cdr hit))))
          (let ((hit (gethash "run"
                              (wasmdown.component.runtime:comp-instance-exports
                               instance)
                              :missing)))
            (unless (eq hit :missing)
              (let ((v (cdr hit)))
                (and (wasmdown.component.runtime:lifted-func-p v) v))))))))

(defun run-preview2 (comp context &key providers store (mode :interpret)
                                    (version "0.2.12"))
  "Drive a validated WASI 0.2 command component to completion under
CONTEXT. PROVIDERS defaults to the whole command world's provider set
at VERSION. STORE, when supplied, is caller-owned (never closed here);
otherwise a fresh store is created and torn down at the end. MODE is
passed to the nested core instantiation. Returns the exit status:
`exit-with-code' preserves its u8, `exit(result)' and a `run'
returning ok/err map to 0/1."
  (let* ((own-store (null store))
         (store (or store (wasmdown.component.runtime:make-component-store
                           :reactor (wasi-context-reactor context))))
         (providers (or providers
                        (preview2-command-provider context :version version))))
    (unwind-protect
         (handler-case
             (let* ((instance (wasmdown.component.runtime:instantiate-component
                               comp :with-args providers :store store
                               :mode mode))
                    (run (p2-run-export instance version)))
               (unless run
                 (error 'wasmdown:link-error
                        :message (format nil "component exports no ~a `run' function"
                                         (format nil "wasi:cli/run@~a" version))))
               (let ((result (wasmdown.component.runtime:lifted-func-call run)))
                 ;; run: func() -> result
                 (if (and (consp result) (eq (car result) :ok)) 0 1)))
           (wasi-exit (c) (wasi-exit-status c)))
      (when own-store
        (wasmdown.component.runtime:store-teardown store)))))

;;; ---------------------------------------------------------------
;;; wasi:filesystem — descriptors, preopens, directory streams.
;;;
;;; Descriptor reps are the SAME native `wasi-descriptor` objects the
;;; 0.3 providers use (an open host fd + its preopen authority), so
;;; the native operations, error table and confinement resolver are
;;; shared; only the WIT SHAPES are 0.2's (error-code as an enum case,
;;; `new-timestamp` variants, positional read/write, directory-entry
;;; streams).

(defun p2-descriptor-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "filesystem" "types" version)
   (list :resource "descriptor")
   :packages (p2-packages version)))

(defun p2-entry-stream-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "filesystem" "types" version)
   (list :resource "directory-entry-stream")
   :packages (p2-packages version)))

(defun %p2-enum (code)
  "A P2 enum value: (:enum \"label\")."
  (list :enum (string-downcase (string code))))

(defun %p2-type-label (value)
  "0.2 declares descriptor-type as an ENUM (domain = the label
string); the shared native helpers carry the 0.3 VARIANT shape
\(label . nil), so the label is unwrapped here."
  (if (consp value) (car value) value))

(defun %p2-norm-stat (stat)
  "Normalize a descriptor-stat alist for the 0.2 enum descriptor-type."
  (if (and (consp stat) (eq (car stat) :ok))
      (let ((alist (cdr stat)))
        (cons :ok
              (mapcar (lambda (pair)
                        (if (and (consp pair) (equal (car pair) "%type"))
                            (cons (car pair) (%p2-type-label (cdr pair)))
                            pair))
                      alist)))
      stat))

(defun p2-wrap-fs (result)
  "Map a native fs result (:ok . v) / (:err . code-string) into the
P2 domain: the error payload is the error-code ENUM value."
  (if (and (consp result) (eq (car result) :ok))
      (cons :ok (cdr result))
      (cons :err (%p2-enum (cdr result)))))

(defun %p2-desc (context value version)
  "The native descriptor a P2 method's SELF/new-descriptor argument
denotes: a rep object directly, or a handle index in the CALLER's
table resolved through the P2 descriptor ct."
  (let ((ct (p2-descriptor-ct version)))
    (cond ((wasi-descriptor-p value) value)
          ((integerp value)
           (or (wasmdown.component.runtime:host-resource-rep ct value)
               (%fs-error "bad-descriptor")))
          (t (%fs-error "invalid")))))

(defstruct (p2-dir-stream (:constructor %make-p2-dir-stream (entries)))
  "A directory iteration: the (name . type) entries already read from
the host, plus a cursor (0-based)."
  (entries '() :type list)
  (cursor 0 :type integer))

(defun %p2-dir-stream-of (ct index)
  (let ((rep (wasmdown.component.runtime:host-resource-rep ct index)))
    (unless (p2-dir-stream-p rep)
      (error 'wasmdown:host-error
             :message "directory-entry-stream: no live rep"))
    rep))

(defun %p2-dir-entries (desc)
  "Read the descriptor's directory entries as (name . descriptor-type)
in host order (the P2 listing does NOT synthesize `.`/`..')."
  (let* ((fd (%descriptor-fd desc))
         (dfd (%libc-openat fd "." (logior +o-rdonly+ +o-directory+ +o-cloexec+) 0)))
    (when (minusp dfd) (%fs-error (errno->error-code (sb-alien:get-errno))))
    (let ((dir (%libc-fdopendir dfd)))
      (when (sb-alien:null-alien dir)
        (%close-fd dfd)
        (%fs-error (errno->error-code (sb-alien:get-errno))))
      (let ((out '()))
        (unwind-protect
             (loop for d = (%libc-readdir dir)
                   while (not (sb-alien:null-alien d))
                   do (let ((name (make-array 0 :element-type '(unsigned-byte 8)
                                                :adjustable t :fill-pointer 0)))
                        (loop for i from 0
                              for code = (sb-alien:deref
                                          (sb-alien:slot (sb-alien:deref d) 'd-name) i)
                              until (zerop code)
                              do (vector-push-extend (logand code #xff) name))
                        (let ((nm (coerce name '(vector (unsigned-byte 8)))))
                          (unless (member (map 'string #'code-char nm) '("." "..")
                                          :test #'string=)
                            (let* ((path (map 'string #'code-char nm))
                                   (probe (%libc-openat fd path
                                                        (logior +o-path+ +o-nofollow+
                                                                +o-cloexec+)
                                                        0)))
                              (when (minusp probe)
                                (%fs-error (errno->error-code (sb-alien:get-errno))))
                              (unwind-protect
                                   (multiple-value-bind (dev ino mode) (%p1-fstat probe)
                                     (declare (ignore dev ino))
                                     (push (cons path (%p2-type-label
                                                       (%mode->type mode)))
                                           out))
                                (%close-fd probe)))))))
          (%libc-closedir dir))
        (nreverse out)))))

(defun %p2-new-timestamp-timespec (value)
  "A `new-timestamp` variant -> (values sec nsec special) for
futimens/utimensat: no-change = UTIME_OMIT, now = UTIME_NOW, and
timestamp(datetime) = the given instant."
  (let ((label (and (consp value) (car value)))
        (payload (cdr value)))
    (cond
      ((eq label :no-change) (values 0 +utime-omit+ nil))
      ((eq label :now) (values 0 +utime-now+ nil))
      ((eq label :timestamp)
       (values (or (cdr (assoc "seconds" payload :test #'string=)) 0)
               (or (cdr (assoc "nanoseconds" payload :test #'string=)) 0)
               t))
      (t (values 0 +utime-omit+ nil)))))

(defun %p2-timespec-pair-from (atime mtime)
  (let ((pair (sb-alien:make-alien sb-alien:unsigned-char 32)))
    (labels ((put (base value)
               (multiple-value-bind (sec nsec) (%p2-new-timestamp-timespec value)
                 (setf (sb-sys:sap-ref-64 (sb-alien:alien-sap pair) base) sec
                       (sb-sys:sap-ref-64 (sb-alien:alien-sap pair) (+ base 8)) nsec))))
      (put 0 atime)
      (put 16 mtime))
    pair))

(defun p2-filesystem-types-instance (context version)
  "wası:filesystem/types."
  (let ((desc-ct (p2-descriptor-ct version))
        (entry-ct (p2-entry-stream-ct version))
        (in-ct (p2-in-ct version))
        (out-ct (p2-out-ct version)))
    (labels ((desc (self) (%p2-desc context self version))
             (with-times (atime mtime thunk)
               (let ((pair (%p2-timespec-pair-from atime mtime)))
                 (unwind-protect (funcall thunk pair)
                   (sb-alien:free-alien pair))))
             (stream-in (offset)
               (list :own-handle in-ct
                     (%make-p2-input-stream :fd (%descriptor-fd (desc offset))
                                            :position-offset 0)))
             (open-via (self offset)
               (let ((d (desc self)))
                 (%require-read-capability d)
                 (cons :ok
                       (list :own-handle in-ct
                             (%make-p2-input-stream :fd (%descriptor-fd d)
                                                    :position-offset offset)))))
             (out-via (self offset)
               (let ((d (desc self)))
                 (%require-mutable d)
                 (cons :ok
                       (list :own-handle out-ct
                             (%make-p2-output-stream :fd (%descriptor-fd d)
                                                     :position-offset offset)))))
             (append-via (self)
               (let ((d (desc self)))
                 (%require-mutable d)
                 (cons :ok
                       (list :own-handle out-ct
                             (%make-p2-output-stream :fd (%descriptor-fd d)
                                                     :appending t))))))
      (provider-instance
       (p2-interface "filesystem" "types" version)
       (list
        ;; ---- streams
        (cons "read-via-stream"
              (lambda (self offset)
                (handler-case (open-via self offset)
                  (wasi-fs-error (c) (cons :err (%p2-enum (wasi-fs-error-code c)))))))
        (cons "write-via-stream"
              (lambda (self offset)
                (handler-case (out-via self offset)
                  (wasi-fs-error (c) (cons :err (%p2-enum (wasi-fs-error-code c)))))))
        (cons "append-via-stream"
              (lambda (self)
                (handler-case (append-via self)
                  (wasi-fs-error (c) (cons :err (%p2-enum (wasi-fs-error-code c)))))))
        ;; ---- metadata / flags
        (cons "advise"
              (lambda (self offset length advice)
                (p2-wrap-fs (fs-advise context (desc self) offset length advice))))
        (cons "sync-data"
              (lambda (self) (p2-wrap-fs (fs-sync-data context (desc self)))))
        (cons "sync"
              (lambda (self) (p2-wrap-fs (fs-sync context (desc self)))))
        (cons "get-flags"
              (lambda (self) (p2-wrap-fs (fs-get-flags context (desc self)))))
        (cons "get-type"
              (lambda (self)
                (let ((r (p2-wrap-fs (fs-get-type context (desc self)))))
                  (if (eq (car r) :ok)
                      (cons :ok (%p2-type-label (cdr r)))
                      r))))
        (cons "set-size"
              (lambda (self size) (p2-wrap-fs (fs-set-size context (desc self) size))))
        (cons "set-times"
              (lambda (self atime mtime)
                (let ((d (desc self)))
                  (handler-case
                      (with-times atime mtime
                        (lambda (pair)
                          (%require-mutable d)
                          (%p1-check (%libc-futimens
                                      (%descriptor-fd d)
                                      (%p1-timespec-pair-pointer
                                       (sb-alien:sap-alien
                                        (sb-alien:alien-sap pair)
                                        (* (sb-alien:struct %wasi-timespec))))))
                          (cons :ok nil)))
                    (wasi-fs-error (c) (cons :err (%p2-enum (wasi-fs-error-code c))))))))
        ;; ---- positional read/write
        (cons "read"
              (lambda (self length offset)
                (let ((d (desc self)))
                  (handler-case
                      (let* (;; the caller's `length' is a filesize (u64);
                             ;; allocating it would abort the host, and
                             ;; the WIT allows a short result, so the
                             ;; buffer is one bounded chunk
                             (want (min length +p2-io-chunk+))
                             (buf (make-array want :element-type '(unsigned-byte 8)))
                             (n (%libc-pread (%descriptor-fd d)
                                             (sb-sys:vector-sap buf) want offset)))
                        (when (minusp n)
                          (%fs-error (errno->error-code (sb-alien:get-errno))))
                        ;; the eof flag is "this attempt got fewer bytes
                        ;; than it asked for", never "fewer than the
                        ;; caller's (possibly clamped) length"
                        (cons :ok (list (coerce (subseq buf 0 n) 'list)
                                        (< n want))))
                    (wasi-fs-error (c) (cons :err (%p2-enum (wasi-fs-error-code c))))))))
        (cons "write"
              (lambda (self buffer offset)
                (let* ((d (desc self))
                       (bytes (coerce buffer '(vector (unsigned-byte 8)))))
                  (handler-case
                      (let ((n (%libc-pwrite (%descriptor-fd d)
                                             (sb-sys:vector-sap bytes)
                                             (length bytes) offset)))
                        (when (minusp n)
                          (%fs-error (errno->error-code (sb-alien:get-errno))))
                        (cons :ok n))
                    (wasi-fs-error (c) (cons :err (%p2-enum (wasi-fs-error-code c))))))))
        ;; ---- directory stream
        (cons "read-directory"
              (lambda (self)
                (let ((d (desc self)))
                  (handler-case
                      (cons :ok (list :own-handle entry-ct
                                      (%make-p2-dir-stream (%p2-dir-entries d))))
                    (wasi-fs-error (c) (cons :err (%p2-enum (wasi-fs-error-code c))))))))
        ;; ---- path operations
        (cons "create-directory-at"
              (lambda (self path)
                (p2-wrap-fs (fs-create-directory-at context (desc self) path))))
        (cons "stat"
              (lambda (self)
                (%p2-norm-stat (p2-wrap-fs (fs-stat context (desc self))))))
        (cons "stat-at"
              (lambda (self path-flags path)
                (%p2-norm-stat
                 (p2-wrap-fs (fs-stat-at context (desc self) path-flags path)))))
        (cons "set-times-at"
              (lambda (self path-flags path atime mtime)
                (let ((d (desc self)))
                  (handler-case
                      (with-times atime mtime
                        (lambda (pair)
                          (%require-mutable d)
                          (let ((follow (member "symlink-follow" (coerce path-flags 'list)
                                                :test #'string=)))
                            (%with-resolved (dir leaf owned)
                                (d path (if follow t nil))
                              (%p1-check
                               (%libc-utimensat dir leaf
                                                (%p1-timespec-pair-pointer
                                                 (sb-alien:sap-alien
                                                  (sb-alien:alien-sap pair)
                                                  (* (sb-alien:struct %wasi-timespec))))
                                                (if follow
                                                    +at-symlink-follow+
                                                    +at-symlink-nofollow+)))))
                          (cons :ok nil)))
                    (wasi-fs-error (c) (cons :err (%p2-enum (wasi-fs-error-code c))))))))
        (cons "link-at"
              (lambda (self old-path-flags old-path new-descriptor new-path)
                (p2-wrap-fs
                 (fs-link-at context (desc self) old-path-flags old-path
                             (desc new-descriptor) new-path))))
        (cons "open-at"
              (lambda (self path-flags path open-flags flags)
                (let ((r (fs-open-at context (desc self) path-flags path
                                     open-flags flags)))
                  (if (and (consp r) (eq (car r) :ok))
                      ;; re-tag the created descriptor with the P2 ct
                      (cons :ok (list :own-handle desc-ct
                                      (third (cdr r))))
                      (cons :err (%p2-enum (cdr r)))))))
        (cons "readlink-at"
              (lambda (self path)
                (p2-wrap-fs (fs-readlink-at context (desc self) path))))
        (cons "remove-directory-at"
              (lambda (self path)
                (p2-wrap-fs (fs-remove-directory-at context (desc self) path))))
        (cons "rename-at"
              (lambda (self old-path new-descriptor new-path)
                (p2-wrap-fs (fs-rename-at context (desc self) old-path
                                          (desc new-descriptor) new-path))))
        (cons "symlink-at"
              (lambda (self old-path new-path)
                (p2-wrap-fs (fs-symlink-at context (desc self) old-path new-path))))
        (cons "unlink-file-at"
              (lambda (self path)
                (p2-wrap-fs (fs-unlink-file-at context (desc self) path))))
        (cons "is-same-object"
              (lambda (self other)
                (fs-is-same-object context (desc self) (desc other))))
        (cons "metadata-hash"
              (lambda (self) (p2-wrap-fs (fs-metadata-hash context (desc self)))))
        (cons "metadata-hash-at"
              (lambda (self path-flags path)
                (p2-wrap-fs (fs-metadata-hash-at context (desc self)
                                                 path-flags path))))
        ;; ---- directory-entry-stream
        (cons "read-directory-entry"
              (lambda (self)
                (let ((s (%p2-dir-stream-of entry-ct self)))
                  (if (>= (p2-dir-stream-cursor s) (length (p2-dir-stream-entries s)))
                      (cons :ok :none)
                      (let ((e (nth (p2-dir-stream-cursor s)
                                    (p2-dir-stream-entries s))))
                        (incf (p2-dir-stream-cursor s))
                        (cons :ok
                              (cons :some
                                    (list (cons "%type" (cdr e))
                                          (cons "name" (car e))))))))))
        ;; ---- error downcast
        (cons "filesystem-error-code"
              (lambda (err)
                (let ((rep (wasmdown.component.runtime:host-resource-rep
                            (p2-error-ct version) err)))
                  (if (and (p2-error-p rep) (plusp (length (p2-error-kind rep))))
                      (cons :some (%p2-enum (p2-error-kind rep)))
                      :none)))))
       :packages (p2-packages version)
       :include #'p2-stable-node-p))))

(defun p2-preopens-instance (context version)
  "wası:filesystem/preopens: the context's preopen directories as
owned descriptor handles plus their guest-visible names."
  (let ((ct (p2-descriptor-ct version)))
    (provider-instance
     (p2-interface "filesystem" "preopens" version)
     (list (cons "get-directories"
                 (lambda ()
                   (loop for pre in (wasi-context-preopens context)
                         for desc = (wasi-preopen-descriptor pre)
                         when (wasi-descriptor-p desc)
                           collect (list (list :own-handle ct desc)
                                         (wasi-preopen-name pre))))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-register-filesystem (context version builders)
  (flet ((bind (short iface thunk)
           (push (cons (p2-interface-name short iface version) thunk)
                 builders)))
    (bind "filesystem" "types"
          (lambda () (p2-filesystem-types-instance context version)))
    (bind "filesystem" "preopens"
          (lambda () (p2-preopens-instance context version)))
    builders))

;;; ---------------------------------------------------------------
;;; wasi:sockets — network, instance-network, ip-name-lookup,
;;; tcp-create-socket, tcp, udp-create-socket, udp.
;;;
;;; The native socket objects and operations are the SAME ones the 0.3
;;; providers use (wasi-tcp-socket / wasi-udp-socket over real,
;;; nonblocking host sockets, with the context's network authority
;;; consulted before every bind/connect/resolve). What differs is
;;; 0.2's phase model: `start-*' records an operation, `finish-*'
;;; performs it and reports the outcome, and no operation silently
;;; blocks.

(defun p2-sockets-package (version)
  (p2-package "sockets" version))

(defun p2-network-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "sockets" "network" version)
   (list :resource "network")
   :packages (p2-packages version)))

(defun p2-tcp-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "sockets" "tcp" version)
   (list :resource "tcp-socket")
   :packages (p2-packages version)))

(defun p2-udp-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "sockets" "udp" version)
   (list :resource "udp-socket")
   :packages (p2-packages version)))

(defun p2-incoming-dgram-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "sockets" "udp" version)
   (list :resource "incoming-datagram-stream")
   :packages (p2-packages version)))

(defun p2-outgoing-dgram-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "sockets" "udp" version)
   (list :resource "outgoing-datagram-stream")
   :packages (p2-packages version)))

(defun p2-resolve-stream-ct (version)
  (wasmdown.wit:wit-type->ct
   (p2-interface "sockets" "ip-name-lookup" version)
   (list :resource "resolve-address-stream")
   :packages (p2-packages version)))

(defun %p2-sock-enum (code)
  "A sockets error-code value: the WIT VARIANT case with no payload."
  (cons (string-downcase (string code)) nil))

(defun %p2-sock-code-domain (code)
  "A native socket error code as the P2 error-code VARIANT value: the
native helpers already carry the variant domain, plain labels are
wrapped."
  (if (consp code) code (%p2-sock-enum code)))

(defun %p2-sock-wrap (result)
  "Native (:ok . v) / (:err . case) -> the P2 sockets domain."
  (if (and (consp result) (eq (car result) :ok))
      (cons :ok (cdr result))
      (cons :err (%p2-sock-enum (cdr result)))))

(defparameter +p2-net-error-kinds+
  '("access-denied" "not-supported" "invalid-argument" "out-of-memory"
    "timeout" "concurrency-conflict" "not-in-progress" "would-block"
    "invalid-state" "new-socket-limit" "address-not-bindable"
    "address-in-use" "remote-unreachable" "connection-refused"
    "connection-reset" "connection-aborted" "datagram-too-large"
    "name-unresolvable" "temporary-resolver-failure"
    "permanent-resolver-failure")
  "The `wasi:sockets/network.error-code' case names, in WIT order.
`network-error-code' answers `some(case)' only for one of these; any
other error has no network information and answers `none'.")

(defun p2-network-error-code (version err)
  "wası:sockets/network.network-error-code: `option<error-code>' (NOT
a result) extracted from an io/error handle. The payload the 0.2
streams put in `last-operation-failed' is one of these errors; an error
whose kind is not a sockets case carries no network information."
  (let ((rep (cond ((p2-error-p err) err)
                   (t (wasmdown.component.runtime:host-resource-rep
                       (p2-error-ct version) err)))))
    (if (and (p2-error-p rep)
             (member (p2-error-kind rep) +p2-net-error-kinds+ :test #'string=))
        (cons :some (%p2-sock-enum (p2-error-kind rep)))
        :none)))

(defstruct (p2-resolve-stream (:constructor %make-p2-resolve-stream (addresses)))
  (addresses '() :type list)
  (cursor 0 :type integer))

(defstruct (p2-incoming-dgram (:constructor %make-p2-incoming-dgram
                                   (&key fd remote)))
  (fd -1 :type fixnum)
  (remote nil))

(defstruct (p2-outgoing-dgram (:constructor %make-p2-outgoing-dgram
                                   (&key fd remote)))
  (fd -1 :type fixnum)
  (remote nil)
  ;; the datagram count the last `check-send` permitted for the next
  ;; `send`; NIL = check-send was never called (WIT sockets/udp
  ;; outgoing-datagram-stream.send: "Implementations must trap if either
  ;; `check-send` was not called or `datagrams` contains more items than
  ;; `check-send` permitted")
  (permit nil))

(defun %p2-pollable-from-fd (fd events)
  "A pollable ready when FD reports EVENTS (POLLIN/POLLOUT). BLOCK
waits: it honours a timeout in ns, and NIL (until ready) waits in
bounded 1 s slices instead of spinning non-blocking poll(2) calls."
  (%make-p2-pollable
   :ready-p (lambda () (not (null (%poll-fd fd events 0))))
   :block (lambda (timeout)
            (if timeout
                (not (null (%poll-fd fd events
                                     (max 0 (ceiling timeout 1000000)))))
                (loop (let ((revents (%poll-fd fd events 1000)))
                        (when revents (return t))))))))

(defun %p2-tcp-streams (fd version)
  "The (input-stream, output-stream) own handles over a connected
socket fd, each on its own dup so one side closing never poisons the
other."
  (let ((in (list :own-handle (p2-in-ct version)
                  (%make-p2-input-stream :fd (%p1-libc-dup fd))))
        (out (list :own-handle (p2-out-ct version)
                   (%make-p2-output-stream :fd (%p1-libc-dup fd)))))
    (list in out)))

(defun %p2-tcp-socket-of (value version)
  (%p2-desc-for value (p2-tcp-ct version) "tcp-socket"))

(defun %p2-udp-socket-of (value version)
  (%p2-desc-for value (p2-udp-ct version) "udp-socket"))

(defun %p2-desc-for (value ct what)
  "Resolve a P2 resource argument (handle index or direct rep)."
  (cond
    ;; a direct CL call may pass the own-handle marker itself
    ((and (consp value) (eq (car value) :own-handle)) (third value))
    ((not (integerp value)) value)
    (t (or (wasmdown.component.runtime:host-resource-rep ct value)
           (error 'wasmdown:host-error
                  :message (format nil "~a: no live rep for handle ~a" what value))))))

(defun %p2-network-of (context value version)
  "The network a `borrow<network>' argument denotes. A real call passes
a handle INDEX, which must resolve to a live network resource of THIS
context; a direct CL call may pass the rep. The argument is never
ignored in favour of the context's default network: a forged handle, or
one naming another context's network, is a host error, so naming a
handle the guest was not given can never widen its authority."
  (let* ((ct (p2-network-ct version))
         (rep (cond ((and (consp value) (eq (car value) :own-handle)) (third value))
                    ((not (integerp value)) value)
                    (t (or (wasmdown.component.runtime:host-resource-rep ct value)
                           (error 'wasmdown:host-error
                                  :message
                                  (format nil
                                          "network: no live resource rep for handle ~a"
                                          value)))))))
    ;; a NIL rep is what instance-network vends in a context with no
    ;; network authority: callers deny the operation (access-denied)
    (when rep
      (unless (member rep (wasi-context-networks context) :test #'eq)
        (error 'wasmdown:host-error
               :message "network: the handle names a network this context did not grant")))
    rep))

(defun p2-tcp-instance (context version)
  "wası:sockets/tcp — the tcp-socket resource."
  (let* ((net (first (wasi-context-networks context)))
         (ct (p2-tcp-ct version))
         (pending (make-hash-table :test #'eq)))
    (labels ((sock (self) (%p2-tcp-socket-of self version))
             (op (self) (gethash (sock self) pending))
             (set-op (self op) (setf (gethash (sock self) pending) op))
             (clear-op (self) (remhash (sock self) pending))
             (with-brand (result)
               ;; tcp-create/bind return native error codes
               (%p2-sock-wrap result)))
      (provider-instance
       (p2-interface "sockets" "tcp" version)
       (list
        (cons "start-bind"
              (lambda (self network-value addr)
                ;; the network argument is the authority the bind runs
                ;; under; it is resolved (never ignored) and recorded
                ;; for finish-bind
                (set-op self (list :bind (%p2-network-of context network-value version)
                                   addr))
                (cons :ok nil)))
        (cons "finish-bind"
              (lambda (self)
                (let ((pending-op (op self)))
                  (if (and pending-op (eq (first pending-op) :bind))
                      (progn
                        (clear-op self)
                        (with-brand (tcp-bind context (second pending-op) (sock self)
                                              (third pending-op))))
                      (cons :err (%p2-sock-enum "not-in-progress"))))))
        (cons "start-connect"
              (lambda (self network-value addr)
                (set-op self (list :connect (%p2-network-of context network-value version)
                                   addr))
                (cons :ok nil)))
        (cons "finish-connect"
              (lambda (self)
                (let ((pending-op (op self)))
                  (if (and pending-op (eq (first pending-op) :connect))
                      (let ((r (tcp-connect context (second pending-op) (sock self)
                                            (third pending-op))))
                        (clear-op self)
                        (if (eq (car (%p2-sock-wrap r)) :ok)
                            (cons :ok (%p2-tcp-streams
                                       (wasi-tcp-socket-fd (sock self)) version))
                            (%p2-sock-wrap r)))
                      (cons :err (%p2-sock-enum "not-in-progress"))))))
        (cons "start-listen"
              (lambda (self)
                (set-op self (list :listen))
                (cons :ok nil)))
        (cons "finish-listen"
              (lambda (self)
                (let* ((s (sock self))
                       (pending-op (op self)))
                  (if (and pending-op (eq (first pending-op) :listen))
                      (progn
                        (clear-op self)
                        (handler-case
                            (progn
                        (when (eq (wasi-tcp-socket-state s) :unbound)
                          (%implicit-bind-tcp s net))
                        (when (member (wasi-tcp-socket-state s)
                                      '(:listening :connecting :connected))
                          (%socket-error (%socket-code "invalid-state")))
                        (%socket-check (%libc-listen
                                        (wasi-tcp-socket-fd s)
                                        (wasi-tcp-socket-backlog s))
                                       "listen")
                              (setf (wasi-tcp-socket-state s) :listening
                                    (wasi-tcp-socket-listen-p s) t)
                              (cons :ok nil))
                          (wasi-socket-error (c)
                            (cons :err (%p2-sock-enum
                                        (wasi-socket-error-code c))))))
                      (cons :err (%p2-sock-enum "not-in-progress"))))))
        (cons "accept"
              (lambda (self)
                (let* ((s (sock self))
                       (fd (%libc-accept4 (wasi-tcp-socket-fd s)
                                          (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                                          (logior +o-nonblock+ +o-cloexec+))))
                  (if (minusp fd)
                      (let ((errno (sb-alien:get-errno)))
                        (cons :err
                              (if (member errno '(11 35)) ; EAGAIN/EWOULDBLOCK
                                  (%p2-sock-enum "would-block")
                                  (%p2-sock-enum (%socket-errno-code errno)))))
                      (let* ((new (make-wasi-tcp-socket
                                   :fd fd :network net
                                   :family (wasi-tcp-socket-family s)
                                   :state :connected))
                             ;; ONE dup pair for the two stream
                             ;; handles: calling %P2-TCP-STREAMS twice
                             ;; leaked two fds per accepted connection
                             (streams (%p2-tcp-streams fd version)))
                        (cons :ok
                              (list (list :own-handle ct new)
                                    (first streams)
                                    (second streams))))))))
        (cons "local-address"
              (lambda (self)
                (%p2-sock-wrap (tcp-get-local-address context net (sock self)
                                                     ))))
        (cons "remote-address"
              (lambda (self)
                (%p2-sock-wrap (tcp-get-remote-address context net (sock self)
                                                      ))))
        (cons "is-listening"
              (lambda (self) (tcp-get-is-listening context net (sock self)
                                                  )))
        (cons "address-family"
              (lambda (self)
                (ecase (wasi-tcp-socket-family (sock self))
                  (:ipv4 "ipv4") (:ipv6 "ipv6"))))
        (cons "set-listen-backlog-size"
              (lambda (self value)
                (%p2-sock-wrap (tcp-set-listen-backlog-size
                                context net (sock self) value))))
        (cons "keep-alive-enabled"
              (lambda (self) (%p2-sock-wrap (tcp-get-keep-alive-enabled
                                             context net (sock self)))))
        (cons "set-keep-alive-enabled"
              (lambda (self v) (%p2-sock-wrap (tcp-set-keep-alive-enabled
                                               context net (sock self) v))))
        (cons "keep-alive-idle-time"
              (lambda (self) (%p2-sock-wrap (tcp-get-keep-alive-idle-time
                                             context net (sock self)))))
        (cons "set-keep-alive-idle-time"
              (lambda (self v) (%p2-sock-wrap (tcp-set-keep-alive-idle-time
                                               context net (sock self) v))))
        (cons "keep-alive-interval"
              (lambda (self) (%p2-sock-wrap (tcp-get-keep-alive-interval
                                             context net (sock self)))))
        (cons "set-keep-alive-interval"
              (lambda (self v) (%p2-sock-wrap (tcp-set-keep-alive-interval
                                               context net (sock self) v))))
        (cons "keep-alive-count"
              (lambda (self) (%p2-sock-wrap (tcp-get-keep-alive-count
                                             context net (sock self)))))
        (cons "set-keep-alive-count"
              (lambda (self v) (%p2-sock-wrap (tcp-set-keep-alive-count
                                               context net (sock self) v))))
        (cons "hop-limit"
              (lambda (self) (%p2-sock-wrap (tcp-get-hop-limit
                                             context net (sock self)))))
        (cons "set-hop-limit"
              (lambda (self v) (%p2-sock-wrap (tcp-set-hop-limit
                                               context net (sock self) v))))
        (cons "receive-buffer-size"
              (lambda (self) (%p2-sock-wrap (tcp-get-receive-buffer-size
                                             context net (sock self)))))
        (cons "set-receive-buffer-size"
              (lambda (self v) (%p2-sock-wrap (tcp-set-receive-buffer-size
                                               context net (sock self) v))))
        (cons "send-buffer-size"
              (lambda (self) (%p2-sock-wrap (tcp-get-send-buffer-size
                                             context net (sock self)))))
        (cons "set-send-buffer-size"
              (lambda (self v) (%p2-sock-wrap (tcp-set-send-buffer-size
                                               context net (sock self) v))))
        (cons "subscribe"
              (lambda (self)
                (list :own-handle (p2-pollable-ct version)
                      (%p2-pollable-from-fd (wasi-tcp-socket-fd (sock self))
                                            +pollin+))))
        (cons "shutdown"
              (lambda (self how)
                (let ((s (sock self))
                      (code (cond ((or (eq how "receive") (eq how :receive)) +shut-rd+)
                                  ((or (eq how "send") (eq how :send)) +shut-wr+)
                                  (t 2))))
                  (handler-case
                      (progn (%socket-check (%libc-shutdown
                                             (wasi-tcp-socket-fd s) code)
                                            "shutdown")
                             (cons :ok nil))
                    (wasi-socket-error (c)
                      (cons :err (%p2-sock-enum (wasi-socket-error-code c)))))))))
       :packages (p2-packages version)
       :include #'p2-stable-node-p))))

(defun p2-udp-instance (context version)
  "wası:sockets/udp — the udp-socket and its datagram streams."
  (let* ((net (first (wasi-context-networks context)))
         (ct (p2-udp-ct version))
         (in-ct (p2-incoming-dgram-ct version))
         (out-ct (p2-outgoing-dgram-ct version))
         (pending (make-hash-table :test #'eq)))
    (labels ((sock (self) (%p2-udp-socket-of self version))
             (op (self) (gethash (sock self) pending))
             (set-op (self v) (setf (gethash (sock self) pending) v))
             (clear-op (self) (remhash (sock self) pending))
             (in-rep (self) (%p2-desc-for self in-ct "incoming-datagram-stream"))
             (out-rep (self) (%p2-desc-for self out-ct "outgoing-datagram-stream"))
             (family-label (sock)
               (ecase (wasi-udp-socket-family sock) (:ipv4 "ipv4") (:ipv6 "ipv6"))))
      (provider-instance
       (p2-interface "sockets" "udp" version)
       (list
        (cons "start-bind"
              (lambda (self network-value addr)
                (set-op self (list :bind (%p2-network-of context network-value version)
                                   addr))
                (cons :ok nil)))
        (cons "finish-bind"
              (lambda (self)
                (let ((pending-op (op self)))
                  (if (and pending-op (eq (first pending-op) :bind))
                      (progn (clear-op self)
                             (%p2-sock-wrap (udp-bind context (second pending-op)
                                                      (sock self)
                                                      (third pending-op))))
                      (cons :err (%p2-sock-enum "not-in-progress"))))))
        (cons "[method]udp-socket.stream"
              (lambda (self remote)
                (let ((s (sock self)))
                  (if (wasi-udp-socket-bound-p s)
                      (let ((remote-addr (if (eq remote :none) nil (cdr remote))))
                        (cons :ok
                              (list (list :own-handle in-ct
                                          (%make-p2-incoming-dgram
                                           :fd (%p1-libc-dup
                                                (wasi-udp-socket-fd s))
                                           :remote remote-addr))
                                    (list :own-handle out-ct
                                          (%make-p2-outgoing-dgram
                                           :fd (wasi-udp-socket-fd s)
                                           :remote remote-addr)))))
                      (cons :err (%p2-sock-enum "invalid-state"))))))
        (cons "local-address"
              (lambda (self) (%p2-sock-wrap (udp-get-local-address
                                             context net (sock self)))))
        (cons "remote-address"
              (lambda (self) (%p2-sock-wrap (udp-get-remote-address
                                             context net (sock self)))))
        (cons "address-family"
              (lambda (self) (family-label (sock self))))
        (cons "unicast-hop-limit"
              (lambda (self) (%p2-sock-wrap (udp-get-unicast-hop-limit
                                             context net (sock self)))))
        (cons "set-unicast-hop-limit"
              (lambda (self v) (%p2-sock-wrap (udp-set-unicast-hop-limit
                                               context net (sock self) v))))
        (cons "receive-buffer-size"
              (lambda (self) (%p2-sock-wrap (udp-get-receive-buffer-size
                                             context net (sock self)))))
        (cons "set-receive-buffer-size"
              (lambda (self v) (%p2-sock-wrap (udp-set-receive-buffer-size
                                               context net (sock self) v))))
        (cons "send-buffer-size"
              (lambda (self) (%p2-sock-wrap (udp-get-send-buffer-size
                                             context net (sock self)))))
        (cons "set-send-buffer-size"
              (lambda (self v) (%p2-sock-wrap (udp-set-send-buffer-size
                                               context net (sock self) v))))
        (cons "[method]udp-socket.subscribe"
              (lambda (self)
                (list :own-handle (p2-pollable-ct version)
                      (%p2-pollable-from-fd (wasi-udp-socket-fd (sock self))
                                            +pollin+))))
        ;; ---- incoming datagrams
        (cons "[method]incoming-datagram-stream.receive"
              (lambda (self max-results)
                (let* ((rep (in-rep self))
                       (fd (p2-incoming-dgram-fd rep))
                       (out '())
                       (cap (min max-results 1024)))
                  (dotimes (i cap)
                    (multiple-value-bind (data addr) (%udp-recv fd)
                      (cond
                        ((null data)
                         ;; bounded wait expired: no more datagrams NOW
                         (return))
                        ((and (p2-incoming-dgram-remote rep)
                              addr
                              (not (equal addr (p2-incoming-dgram-remote rep))))
                         ;; a datagram from an unexpected peer is dropped
                         nil)
                        ((equal (first data) :closed)
                         (return))
                        (t (push (list (cons "data" data)
                                       (cons "remote-address" addr))
                                 out)))))
                  (cons :ok (nreverse out)))))
        (cons "[method]outgoing-datagram-stream.check-send"
              (lambda (self)
                ;; WIT sockets/udp outgoing-datagram-stream.check-send:
                ;; "Returns the number of datagrams permitted for the next
                ;; call to `send`, or an error. Calling `send` with more
                ;; datagrams than this function has permitted will trap.
                ;; ... Never returns `would-block`."
                (let ((rep (out-rep self)))
                  (setf (p2-outgoing-dgram-permit rep) 1)
                  (cons :ok 1))))
        (cons "[method]outgoing-datagram-stream.send"
              (lambda (self datagrams)
                ;; WIT sockets/udp outgoing-datagram-stream.send:
                ;; "This function attempts to send all provided `datagrams`
                ;; on the socket without blocking and returns how many
                ;; messages were actually sent (or queued for sending).
                ;; ... If none of the datagrams were able to be sent,
                ;; `ok(0)` is returned. ... If at least one datagram has
                ;; been sent successfully, this function never returns an
                ;; error. If the input list is empty, the function returns
                ;; `ok(0)`. Each call to `send` must be permitted by a
                ;; preceding `check-send`. Implementations must trap if
                ;; either `check-send` was not called or `datagrams`
                ;; contains more items than `check-send` permitted."
                (let* ((rep (out-rep self))
                       (fd (p2-outgoing-dgram-fd rep))
                       (permit (p2-outgoing-dgram-permit rep))
                       (count (length datagrams)))
                  (unless permit
                    (error 'wasmdown:wasm-trap
                           :message
                           "wasi:sockets/udp outgoing-datagram-stream.send: check-send was not called"))
                  (when (> count permit)
                    (error 'wasmdown:wasm-trap
                           :message
                           (format nil "wasi:sockets/udp outgoing-datagram-stream.send: ~d datagram(s) offered but check-send permitted ~d"
                                   count permit)))
                  ;; the permit is not consumed: the WIT mandates exactly
                  ;; these two traps, and `check-send` re-reports the
                  ;; count permitted "for the next call to `send`"
                  (block send
                    (labels ((stop (result) (return-from send result)))
                      (loop with sent = 0
                            for d in datagrams
                            do (let* ((data (cdr (assoc "data" d :test #'string=)))
                                      (remote (cdr (assoc "remote-address" d
                                                          :test #'string=)))
                                      (addr (cond ((and (consp remote)
                                                        (eq (car remote) :none))
                                                   (p2-outgoing-dgram-remote rep))
                                                  (remote (cdr remote))
                                                  (t (p2-outgoing-dgram-remote rep)))))
                                 (unless addr
                                   (stop (cons :err (%p2-sock-enum
                                                     "invalid-argument"))))
                                 (handler-case
                                     (multiple-value-bind (alien len family)
                                         (ip-socket-address->sockaddr addr)
                                       (declare (ignore family))
                                       (unwind-protect
                                            (let ((err (%udp-send-bytes fd data alien len)))
                                              (when err
                                                (%socket-error err)))
                                         (sb-alien:free-alien alien)))
                                   (wasi-socket-error (c)
                                     ;; nothing sent yet: the error is
                                     ;; the call's result; otherwise the
                                     ;; count is (WIT: "If at least one
                                     ;; datagram has been sent
                                     ;; successfully, this function never
                                     ;; returns an error")
                                     (stop (if (plusp sent)
                                               (cons :ok sent)
                                               (cons :err (%p2-sock-code-domain
                                                           (wasi-socket-error-code c))))))))
                            (incf sent)
                            finally (stop (cons :ok sent))))))))
        (cons "[method]incoming-datagram-stream.subscribe"
              (lambda (self)
                (let ((rep (in-rep self)))
                  (list :own-handle (p2-pollable-ct version)
                        (%p2-pollable-from-fd (p2-incoming-dgram-fd rep)
                                              +pollin+)))))
        (cons "[method]outgoing-datagram-stream.subscribe"
              (lambda (self)
                (declare (ignore self))
                (list :own-handle (p2-pollable-ct version)
                      (%make-p2-pollable :ready-p (lambda () t)
                                         :block (lambda (timeout)
                                                  (declare (ignore timeout))
                                                  t))))))
       :packages (p2-packages version)
       :include #'p2-stable-node-p))))

(defun p2-ip-name-lookup-instance (context version)
  "wası:sockets/ip-name-lookup."
  (let ((ct (p2-resolve-stream-ct version)))
    (provider-instance
     (p2-interface "sockets" "ip-name-lookup" version)
     (list
      (cons "resolve-addresses"
            (lambda (network-value name)
              ;; the DNS authority is the one on the NETWORK the guest
              ;; names (resolved, not the context's default)
              (let ((net (%p2-network-of context network-value version)))
                (cond
                  ((null net)
                   (cons :err (%p2-sock-enum "access-denied")))
                  ((not (wasi-network-allows-dns-p net name))
                   (cons :err (%p2-sock-enum "access-denied")))
                  (t
                   (let ((r (%lookup-result name 0)))
                     (if (eq (car r) :ok)
                         (cons :ok (list :own-handle ct
                                         (%make-p2-resolve-stream (cdr r))))
                         (cons :err (%p2-sock-code-domain (cdr r))))))))))
      (cons "[method]resolve-address-stream.resolve-next-address"
            (lambda (self)
              (let ((rep (%p2-desc-for self ct "resolve-address-stream")))
                (if (>= (p2-resolve-stream-cursor rep)
                        (length (p2-resolve-stream-addresses rep)))
                    (cons :ok :none)
                    (prog1 (cons :ok
                                 (cons :some
                                       (nth (p2-resolve-stream-cursor rep)
                                            (p2-resolve-stream-addresses rep))))
                      (incf (p2-resolve-stream-cursor rep)))))))
      (cons "[method]resolve-address-stream.subscribe"
            (lambda (self)
              (declare (ignore self))
              (list :own-handle (p2-pollable-ct version)
                    (%make-p2-pollable :ready-p (lambda () t)
                                       :block (lambda (timeout)
                                                (declare (ignore timeout))
                                                t))))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-instance-network-instance (context version)
  (let* ((net (first (wasi-context-networks context)))
         (ct (p2-network-ct version)))
    (provider-instance
     (p2-interface "sockets" "instance-network" version)
     (list (cons "instance-network"
                 (lambda () (list :own-handle ct net))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-network-instance (context version)
  "wası:sockets/network: the network resource and its error downcast."
  (declare (ignore context))
  (provider-instance
   (p2-interface "sockets" "network" version)
   (list
    ;; `network-error-code' is `@unstable(feature = network-error-code)'
    ;; at 0.2.12, so the stability gate keeps it out of this exact
    ;; release's surface; when a release makes it stable it must export
    ;; under its PLAIN name with the option<error-code> shape.
    (cons "network-error-code"
          (lambda (err) (p2-network-error-code version err))))
   :packages (p2-packages version)
   :include #'p2-stable-node-p))

(defun p2-tcp-create-instance (context version)
  (let* ((net (first (wasi-context-networks context))))
    (provider-instance
     (p2-interface "sockets" "tcp-create-socket" version)
     (list (cons "create-tcp-socket"
                 (lambda (family)
                   (%p2-sock-wrap
                    (tcp-create context net family)))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-udp-create-instance (context version)
  (let* ((net (first (wasi-context-networks context))))
    (provider-instance
     (p2-interface "sockets" "udp-create-socket" version)
     (list (cons "create-udp-socket"
                 (lambda (family)
                   (%p2-sock-wrap
                    (udp-create context net family)))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-register-sockets (context version builders)
  (flet ((bind (iface thunk)
           (push (cons (p2-interface-name "sockets" iface version) thunk)
                 builders)))
    (bind "network" (lambda () (p2-network-instance context version)))
    (bind "instance-network"
          (lambda () (p2-instance-network-instance context version)))
    (bind "ip-name-lookup"
          (lambda () (p2-ip-name-lookup-instance context version)))
    (bind "tcp-create-socket"
          (lambda () (p2-tcp-create-instance context version)))
    (bind "tcp" (lambda () (p2-tcp-instance context version)))
    (bind "udp-create-socket"
          (lambda () (p2-udp-create-instance context version)))
    (bind "udp" (lambda () (p2-udp-instance context version)))
    builders))
