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

(defstruct (p2-pollable (:constructor %make-p2-pollable (&key ready-p block)))
  "A wasi:io/poll pollable: READY-P answers readiness now, BLOCK waits
(bounded by the context's reactor when one exists) and returns T when
the event fired."
  (ready-p (lambda () t) :type function)
  (block nil))

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
  (closed nil))

(defun p2-closed-error ()
  "The WIT `stream-error::closed` value."
  (cons :closed nil))

(defun p2-failed-error (message version)
  "The WIT `stream-error::last-operation-failed` value. Its payload is
`own<error>', so the domain value is an OWN-HANDLE MARKER (the runtime
allocates the handle when it lowers the result), never a bare rep."
  (cons :last-operation-failed
        (list :own-handle (p2-error-ct version) (p2-error message))))


(defun p2-in-read (rep len)
  "Read at most LEN octets from an input-stream rep. Returns an octet
vector, or :closed at end of input."
  (cond
    ((p2-input-stream-closed rep) :closed)
    ((p2-input-stream-stream rep)
     (let ((stream (p2-input-stream-stream rep)))
       (if (plusp len)
           (let ((buf (make-array len :element-type '(unsigned-byte 8))))
             (let ((n (read-sequence buf stream)))
               (if (zerop n) :closed (subseq buf 0 n))))
           (if (listen stream) #() :closed))))
    ((not (minusp (p2-input-stream-fd rep)))
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
       (cond ((minusp n) (cons :failed (sb-alien:get-errno)))
             ((zerop n) :closed)
             (t (subseq buf 0 n)))))
    (t :closed)))

(defun p2-in-skip (rep len)
  "Consume at most LEN octets. Returns the count consumed, :closed, or
\(:failed . errno) when the host read itself failed."
  (let ((data (p2-in-read rep len)))
    (cond ((eq data :closed) :closed)
          ((and (consp data) (eq (car data) :failed)) data)
          (t (length data)))))

(defun p2-out-write (rep bytes)
  "Write BYTES to an output-stream rep. Returns T, or NIL when the
stream is closed."
  (cond
    ((p2-output-stream-closed rep) nil)
    ((p2-output-stream-stream rep)
     (let ((stream (p2-output-stream-stream rep)))
       (if (subtypep (stream-element-type stream) '(unsigned-byte 8))
           (write-sequence (coerce bytes '(vector (unsigned-byte 8))) stream)
           (write-string (map 'string #'code-char bytes) stream))
       t))
    ((not (minusp (p2-output-stream-fd rep)))
     (let* ((vec (coerce bytes '(vector (unsigned-byte 8))))
            (off (p2-output-stream-position-offset rep))
            (n (cond (off
                      (%libc-pwrite (p2-output-stream-fd rep)
                                    (sb-alien:sap-alien (sb-sys:vector-sap vec)
                                                        (* sb-alien:unsigned-char))
                                    (length vec) off))
                     ((p2-output-stream-appending rep)
                      (%p1-libc-write (p2-output-stream-fd rep)
                                      (sb-sys:vector-sap vec)
                                      (length vec)))
                     (t (%p1-libc-write (p2-output-stream-fd rep)
                                        (sb-sys:vector-sap vec)
                                        (length vec))))))
       (when (and off (not (minusp n)))
         (incf (p2-output-stream-position-offset rep) n))
       (not (minusp n))))
    (t nil)))

(defun p2-out-flush (rep)
  (cond ((p2-output-stream-closed rep) nil)
        ((p2-output-stream-stream rep)
         (force-output (p2-output-stream-stream rep)) t)
        (t t)))

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
  (let ((ct (p2-pollable-ct version)))
    (provider-instance
     (p2-interface "io" "poll" version)
     (list (cons "ready"
                 (lambda (self)
                   (let ((rep (p2-resource-rep ct self "pollable.ready")))
                     (not (null (funcall (p2-pollable-ready-p rep)))))))
           (cons "block"
                 (lambda (self)
                   (let ((rep (p2-resource-rep ct self "pollable.block")))
                     (let ((block (p2-pollable-block rep)))
                       (if block (not (null (funcall block nil))) t)))))
           (cons "poll"
                 (lambda (handles)
                   ;; return every ready input index, in INPUT ORDER,
                   ;; duplicates included
                   (let ((i 0) (out '()))
                     (dolist (h handles (nreverse out))
                       (let ((rep (p2-resource-rep ct h "poll")))
                         (when (funcall (p2-pollable-ready-p rep))
                           (push i out)))
                       (incf i))))))
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

(defun p2-streams-instance (context version)
  "wası:io/streams — input-stream/output-stream resources."
  (let ((in-ct (p2-in-ct version))
        (out-ct (p2-out-ct version))
        (poll-ct (p2-pollable-ct version)))
    (labels ((in (self what)
               (p2-resource-rep in-ct self what))
             (out (self what)
               (p2-resource-rep out-ct self what))
             (pollable ()
               (%make-p2-pollable :ready-p (lambda () t)
                                  :block (lambda (timeout) (declare (ignore timeout)) t)))
             (wrap-read (result)
               (cond
                 ((eq result :closed) (cons :err (p2-closed-error)))
                 ((and (consp result) (eq (car result) :failed))
                  (cons :err (p2-failed-error
                              (format nil "read failed (errno ~a)" (cdr result))
                              version)))
                 (t (cons :ok (coerce result 'list))))))
      (declare (ignore context))
      (provider-instance
       (p2-interface "io" "streams" version)
       (list
        ;; ---- input-stream
        (cons "read"
              (lambda (self len) (wrap-read (p2-in-read (in self "read") len))))
        (cons "blocking-read"
              (lambda (self len) (wrap-read (p2-in-read (in self "blocking-read") len))))
        (cons "skip"
              (lambda (self len)
                (let ((n (p2-in-skip (in self "skip") len)))
                  (cond ((eq n :closed) (cons :err (p2-closed-error)))
                        ((and (consp n) (eq (car n) :failed))
                         (cons :err (p2-failed-error
                                     (format nil "skip failed (errno ~a)" (cdr n))
                                     version)))
                        (t (cons :ok n))))))
        (cons "blocking-skip"
              (lambda (self len)
                (let ((n (p2-in-skip (in self "blocking-skip") len)))
                  (cond ((eq n :closed) (cons :err (p2-closed-error)))
                        ((and (consp n) (eq (car n) :failed))
                         (cons :err (p2-failed-error
                                     (format nil "skip failed (errno ~a)" (cdr n))
                                     version)))
                        (t (cons :ok n))))))
        (cons "[method]input-stream.subscribe"
              (lambda (self)
                (declare (ignore self))
                (list :own-handle poll-ct (pollable))))
        ;; ---- output-stream
        (cons "check-write"
              (lambda (self)
                (let ((rep (out self "check-write")))
                  (if (p2-output-stream-closed rep)
                      (cons :err (p2-closed-error))
                      (cons :ok +p2-write-permit+)))))
        (cons "write"
              (lambda (self bytes)
                (if (p2-out-write (out self "write") bytes)
                    (cons :ok nil)
                    (cons :err (p2-closed-error)))))
        (cons "blocking-write-and-flush"
              (lambda (self bytes)
                (let ((rep (out self "blocking-write-and-flush")))
                  (if (and (p2-out-write rep bytes) (p2-out-flush rep))
                      (cons :ok nil)
                      (cons :err (p2-closed-error))))))
        (cons "flush"
              (lambda (self)
                (if (p2-out-flush (out self "flush"))
                    (cons :ok nil)
                    (cons :err (p2-closed-error)))))
        (cons "blocking-flush"
              (lambda (self)
                (if (p2-out-flush (out self "blocking-flush"))
                    (cons :ok nil)
                    (cons :err (p2-closed-error)))))
        (cons "[method]output-stream.subscribe"
              (lambda (self)
                (declare (ignore self))
                (list :own-handle poll-ct (pollable))))
        (cons "write-zeroes"
              (lambda (self len)
                (let ((rep (out self "write-zeroes")))
                  (if (p2-out-write rep (make-array len :element-type '(unsigned-byte 8)))
                      (cons :ok nil)
                      (cons :err (p2-closed-error))))))
        (cons "blocking-write-zeroes-and-flush"
              (lambda (self len)
                (let ((rep (out self "blocking-write-zeroes-and-flush")))
                  (if (and (p2-out-write rep (make-array len :element-type '(unsigned-byte 8)))
                           (p2-out-flush rep))
                      (cons :ok nil)
                      (cons :err (p2-closed-error))))))
        (cons "splice"
              (lambda (self src len)
                (let* ((src-rep (p2-resource-rep in-ct src "splice"))
                       (data (p2-in-read src-rep len)))
                  (cond ((eq data :closed) (cons :ok 0))
                        ((p2-out-write (out self "splice") data)
                         (cons :ok (length data)))
                        (t (cons :err (p2-closed-error)))))))
        (cons "blocking-splice"
              (lambda (self src len)
                (let* ((src-rep (p2-resource-rep in-ct src "blocking-splice"))
                       (data (p2-in-read src-rep len)))
                  (cond ((eq data :closed) (cons :ok 0))
                        ((and (p2-out-write (out self "blocking-splice") data)
                              (p2-out-flush (out self "blocking-splice")))
                         (cons :ok (length data)))
                        (t (cons :err (p2-closed-error))))))))
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
                      (let* ((buf (make-array length :element-type '(unsigned-byte 8)))
                             (n (%libc-pread (%descriptor-fd d)
                                             (sb-sys:vector-sap buf) length offset)))
                        (when (minusp n)
                          (%fs-error (errno->error-code (sb-alien:get-errno))))
                        (cons :ok (list (coerce (subseq buf 0 n) 'list)
                                        (< n length))))
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
  (remote nil))

(defun %p2-pollable-from-fd (fd events)
  "A pollable ready when FD reports EVENTS (POLLIN/POLLOUT)."
  (%make-p2-pollable
   :ready-p (lambda () (not (null (%poll-fd fd events 0))))
   :block (lambda (timeout)
            (let ((ms (if timeout (ceiling timeout 1000000) 0)))
              (not (null (%poll-fd fd events (min ms 1000))))))))

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
                (declare (ignore network-value))
                (set-op self (list :bind addr))
                (cons :ok nil)))
        (cons "finish-bind"
              (lambda (self)
                (let ((pending-op (op self)))
                  (if (and pending-op (eq (first pending-op) :bind))
                      (progn
                        (clear-op self)
                        (with-brand (tcp-bind context net (sock self)
                                              (second pending-op))))
                      (cons :err (%p2-sock-enum "not-in-progress"))))))
        (cons "start-connect"
              (lambda (self network-value addr)
                (declare (ignore network-value))
                (set-op self (list :connect addr))
                (cons :ok nil)))
        (cons "finish-connect"
              (lambda (self)
                (let ((pending-op (op self)))
                  (if (and pending-op (eq (first pending-op) :connect))
                      (let ((r (tcp-connect context net (sock self)
                                            (second pending-op))))
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
                      (let ((new (make-wasi-tcp-socket
                                  :fd fd :network net
                                  :family (wasi-tcp-socket-family s)
                                  :state :connected)))
                        (cons :ok
                              (list (list :own-handle ct new)
                                    (first (%p2-tcp-streams fd version))
                                    (second (%p2-tcp-streams fd version)))))))))
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
                (declare (ignore network-value))
                (set-op self (list :bind addr))
                (cons :ok nil)))
        (cons "finish-bind"
              (lambda (self)
                (let ((pending-op (op self)))
                  (if (and pending-op (eq (first pending-op) :bind))
                      (progn (clear-op self)
                             (%p2-sock-wrap (udp-bind context net (sock self)
                                                      (second pending-op)
                                                     )))
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
                (declare (ignore self))
                (cons :ok 1)))
        (cons "[method]outgoing-datagram-stream.send"
              (lambda (self datagrams)
                (let* ((rep (out-rep self))
                       (fd (p2-outgoing-dgram-fd rep))
                       (sent 0))
                  (handler-case
                      (progn
                        (dolist (d datagrams)
                          (let* ((data (cdr (assoc "data" d :test #'string=)))
                                 (remote (cdr (assoc "remote-address" d
                                                     :test #'string=)))
                                 (addr (cond ((and (consp remote) (eq (car remote) :none))
                                              (p2-outgoing-dgram-remote rep))
                                             (remote (cdr remote))
                                             (t (p2-outgoing-dgram-remote rep)))))
                            (unless addr
                              (%socket-error (%socket-code "invalid-argument")))
                            (multiple-value-bind (alien len family)
                                (ip-socket-address->sockaddr addr)
                              (declare (ignore family))
                              (unwind-protect
                                   (let ((err (%udp-send-bytes fd data alien len)))
                                     (when err
                                       (%socket-error err)))
                                (sb-alien:free-alien alien)))
                            (incf sent)))
                        (cons :ok sent))
                    (wasi-socket-error (c)
                      (cons :err (%p2-sock-code-domain
                                  (wasi-socket-error-code c))))))))
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
  (let* ((net (first (wasi-context-networks context)))
         (ct (p2-resolve-stream-ct version)))
    (provider-instance
     (p2-interface "sockets" "ip-name-lookup" version)
     (list
      (cons "resolve-addresses"
            (lambda (network-value name)
              (declare (ignore network-value))
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
                       (cons :err (%p2-sock-code-domain (cdr r)))))))))
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
  (let ((ct (p2-network-ct version)))
    (provider-instance
     (p2-interface "sockets" "network" version)
     (list (cons "[method]network-error-code"
                 (lambda (self)
                   (declare (ignore self))
                   (cons :err (%p2-sock-enum "unknown"))))
           ;; the NETWORK resource is created by instance-network; this
           ;; interface only adds the downcast method
           )
     :packages (p2-packages version)
     :include #'p2-stable-node-p)))

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
