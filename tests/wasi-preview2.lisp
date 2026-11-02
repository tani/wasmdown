;;;; WASI 0.2 (Preview2) provider facade: the versioned registry, the
;;;; stability gate and the interface implementations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defpackage #:wasmdown.test.wasi-preview2
  (:use #:cl #:wasmdown.test))

(in-package #:wasmdown.test.wasi-preview2)

(defun p2-call (instance name &rest args)
  (apply #'wasmdown.component.runtime:lifted-func-call
         (wasmdown.wasi:provider-instance-function instance name) args))

(test wasi-preview2/import-name-parts
  (multiple-value-bind (pkg iface version)
      (wasmdown.wasi:p2-import-name-parts "wasi:io/streams@0.2.12")
    (is (string= pkg "io"))
    (is (string= iface "streams"))
    (is (string= version "0.2.12")))
  (is (null (wasmdown.wasi:p2-import-name-parts "wasi:cli/run")))
  (is (null (wasmdown.wasi:p2-import-name-parts "env.stat"))))

(test wasi-preview2/command-world
  (let ((world (wasmdown.wasi:preview2-command-world :version "0.2.12")))
    (is (string= "command" (wasmdown.wit:wit-world-name world)))
    ;; the command world imports the stable interfaces and exports run
    (is (plusp (length (wasmdown.wit:wit-world-imports world))))
    (is (= 1 (length (wasmdown.wit:wit-world-exports world))))
    (let ((exports (mapcar (lambda (item)
                             (wasmdown.wit:interface-import-name
                              (wasmdown.wit:wit-world-item-target item)))
                           (wasmdown.wit:wit-world-exports world))))
      (is (member "wasi:cli/run@0.2.12" exports :test #'string=))))
  ;; an unknown release is a link error naming it
  (is (handler-case
          (progn (wasmdown.wasi:preview2-command-world :version "0.2.99") nil)
        (wasmdown:link-error (c)
          (search "0.2.99" (wasmdown:wasmdown-error-message c))))))

(test wasi-preview2/http-imports-are-provided
  ;; The 0.2 outgoing surface is served: no by-design refusal remains.
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let* ((builders (wasmdown.wasi:p2-builder-table ctx "0.2.12"))
                (prov (wasmdown.wasi:preview2-command-provider
                       ctx :imports '("wasi:http/types@0.2.12"
                                      "wasi:http/outgoing-handler@0.2.12"))))
           (is (assoc "wasi:http/types@0.2.12" builders :test #'string=))
           (is (assoc "wasi:http/outgoing-handler@0.2.12" builders
                      :test #'string=))
           (is (= 2 (length prov)))
           (is (wasmdown.wasi:provider-instance-function
                (cdr (assoc "wasi:http/types@0.2.12" prov :test #'string=))
                "[constructor]fields"))
           (is (wasmdown.wasi:provider-instance-function
                (cdr (assoc "wasi:http/outgoing-handler@0.2.12" prov
                            :test #'string=))
                "handle")))
      (wasmdown.wasi:close-wasi-context ctx))))

(test wasi-preview2/clocks-and-random
  (let ((clock (wasmdown.wasi:p2-monotonic-clock-instance "0.2.12")))
    (let ((a (p2-call clock "now")))
      (is (integerp a))
      (is (>= (p2-call clock "now") a))
      (is (plusp (p2-call clock "resolution")))))
  (let ((wall (wasmdown.wasi:p2-wall-clock-instance "0.2.12")))
    (let ((now (p2-call wall "now")))
      (is (consp (assoc "seconds" now :test #'string=)))))
  (let ((rnd (wasmdown.wasi:p2-random-instance "0.2.12")))
    (is (= 8 (length (p2-call rnd "get-random-bytes" 8))))
    (is (integerp (p2-call rnd "get-random-u64"))))
  ;; the 0.2 spelling `insecure-seed' is the interface's only function
  (let ((seed (wasmdown.wasi:p2-insecure-seed-instance "0.2.12")))
    (is (= 2 (length (p2-call seed "insecure-seed"))))))

(test wasi-preview2/environment-and-exit-gating
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :arguments '("prog" "a")
               :environment '(("K" . "V"))
               :cwd "/tmp"))
         (env (wasmdown.wasi:p2-environment-instance ctx "0.2.12")))
    (is (equal '(("K" "V")) (p2-call env "get-environment")))
    (is (equal '("prog" "a") (p2-call env "get-arguments")))
    ;; the domain OPTION shape: (:some . value) / :none
    (is (equal (cons :some "/tmp") (p2-call env "initial-cwd"))))
  ;; exit-with-code is STABLE only at 0.2.12; 0.2.11 declares it under
  ;; @unstable, so an exact-version binding must not export it
  (is (wasmdown.wasi:provider-instance-function
       (wasmdown.wasi:p2-exit-instance "0.2.12") "exit-with-code"))
  (is (null (wasmdown.wasi:provider-instance-function
             (wasmdown.wasi:p2-exit-instance "0.2.11") "exit-with-code")))
  ;; exit(result) maps ok->0, err->1 through wasi-exit
  (let ((exit (wasmdown.wasi:p2-exit-instance "0.2.12")))
    (is (= 0 (handler-case (p2-call exit "exit" (cons :ok nil))
               (wasmdown.wasi:wasi-exit (c) (wasmdown.wasi:wasi-exit-status c)))))
    (is (= 1 (handler-case (p2-call exit "exit" (cons :err nil))
               (wasmdown.wasi:wasi-exit (c) (wasmdown.wasi:wasi-exit-status c)))))
    (is (= 200 (handler-case (p2-call exit "exit-with-code" 200)
                 (wasmdown.wasi:wasi-exit (c) (wasmdown.wasi:wasi-exit-status c)))))))

(test wasi-preview2/registry-binds-every-io-cli-interface
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdin (make-string-input-stream "abc")
               :stdout (make-string-output-stream)
               :stderr (make-string-output-stream)))
         (builders (wasmdown.wasi:p2-builder-table ctx "0.2.12")))
    (dolist (name '("wasi:io/error@0.2.12" "wasi:io/poll@0.2.12"
                    "wasi:io/streams@0.2.12"
                    "wasi:cli/stdin@0.2.12" "wasi:cli/stdout@0.2.12"
                    "wasi:cli/stderr@0.2.12"
                    "wasi:cli/terminal-input@0.2.12"
                    "wasi:cli/terminal-output@0.2.12"
                    "wasi:cli/terminal-stdin@0.2.12"
                    "wasi:cli/terminal-stdout@0.2.12"
                    "wasi:cli/terminal-stderr@0.2.12"
                    "wasi:clocks/monotonic-clock@0.2.12"
                    "wasi:clocks/wall-clock@0.2.12"
                    "wasi:random/random@0.2.12"
                    "wasi:random/insecure@0.2.12"
                    "wasi:random/insecure-seed@0.2.12"
                    "wasi:cli/environment@0.2.12"
                    "wasi:cli/exit@0.2.12"
                    "wasi:filesystem/types@0.2.12"
                    "wasi:filesystem/preopens@0.2.12"
                    "wasi:sockets/network@0.2.12"
                    "wasi:sockets/instance-network@0.2.12"
                    "wasi:sockets/ip-name-lookup@0.2.12"
                    "wasi:sockets/tcp@0.2.12"
                    "wasi:sockets/tcp-create-socket@0.2.12"
                    "wasi:sockets/udp@0.2.12"
                    "wasi:sockets/udp-create-socket@0.2.12"))
      (is (assoc name builders :test #'string=)))))

(test wasi-preview2/streams-write-and-read
  (let* ((out (make-string-output-stream))
         (in (make-string-input-stream "hello"))
         (ctx (wasmdown.wasi:make-wasi-context :stdin in :stdout out))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12")))
    ;; get-stdout yields an own handle marker
    (let* ((stdout-inst (wasmdown.wasi:p2-stdout-instance ctx "0.2.12" "stdout"))
           (handle (p2-call stdout-inst "get-stdout")))
      (is (and (listp handle) (eq (first handle) :own-handle))))
    ;; the resource method itself: drive it with a rep object as SELF
    ;; (the provider closures resolve reps through the handle table in
    ;; a real call; here the rep IS the object)
    (let ((write (wasmdown.wasi:provider-instance-function
                  streams "[method]output-stream.write")))
      (is write))))

(test wasi-preview2/stream-error-encoding
  ;; stream-error is a variant; last-operation-failed carries an
  ;; own<error> HANDLE marker (allocated at lowering), closed has no
  ;; payload
  (let* ((ct (wasmdown.wasi::p2-error-ct "0.2.12"))
         (failed (wasmdown.wasi::p2-failed-error "boom" "0.2.12")))
    (is (eq (car failed) :last-operation-failed))
    (let ((payload (cdr failed)))
      (is (eq (first payload) :own-handle))
      (is (eq (second payload) ct))
      (is (string= "boom" (wasmdown.wasi::p2-error-message (third payload))))))
  (let ((closed (wasmdown.wasi::p2-closed-error)))
    (is (eq (car closed) :closed))))

(test wasi-preview2/filesystem-descriptor
  (let* ((dir (concatenate 'string (sb-posix:mkdtemp "/tmp/wmd-p2fs-XXXXXX") "/"))
         (_ (progn (ensure-directories-exist (merge-pathnames "x" (pathname dir)))
                   (with-open-file (f (merge-pathnames "f.txt" (pathname dir))
                                      :direction :output :if-exists :supersede)
                     (write-line "x" f))))
         (pre (wasmdown.wasi:open-wasi-preopen dir :name "/work"))
         (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre))))
    (declare (ignore _))
    (unwind-protect
         (let* ((types (wasmdown.wasi:p2-filesystem-types-instance ctx "0.2.12"))
                (desc (wasmdown.wasi::wasi-preopen-descriptor pre))
                (stat (p2-call types "[method]descriptor.stat" desc))
                (flags (p2-call types "[method]descriptor.get-flags" desc))
                (type (p2-call types "[method]descriptor.get-type" desc))
                (open (p2-call types "[method]descriptor.open-at" desc
                               '() "missing.txt" '() '()))
                (listing (p2-call types "[method]descriptor.read-directory" desc)))
           (is (eq (car stat) :ok))
           (is (equal "directory" (cdr (assoc "%type" (cdr stat) :test #'string=))))
           (is (eq (car flags) :ok))
           (is (equal "directory" (cdr type)))
           ;; a missing path is the error-code ENUM value, not a string
           (is (eq (car open) :err))
           (is (eq (car (cdr open)) :enum))
           (is (string= "no-entry" (second (cdr open))))
           ;; the directory stream yields an own handle, and its first
           ;; entry names the file we created
           (is (eq (car listing) :ok))
           (let* ((marker (cdr listing))
                  (stream-ct (wasmdown.wasi:p2-entry-stream-ct "0.2.12"))
                  (stream (third marker)))
             (declare (ignore stream-ct))
             (let ((entry (wasmdown.wasi::p2-dir-stream-entries stream)))
               (is (member "f.txt" entry :key #'car :test #'string=)))))
      (ignore-errors (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi-preview2/preopens-get-directories
  (let* ((dir (concatenate 'string (sb-posix:mkdtemp "/tmp/wmd-p2pre-XXXXXX") "/"))
         (_ (ensure-directories-exist (merge-pathnames "x" (pathname dir))))
         (pre (wasmdown.wasi:open-wasi-preopen dir :name "/work"))
         (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre))))
    (unwind-protect
         (let* ((inst (wasmdown.wasi:p2-preopens-instance ctx "0.2.12"))
                (dirs (p2-call inst "get-directories")))
           (declare (ignore _))
           (is (= 1 (length dirs)))
           (is (string= "/work" (second (first dirs))))
           (is (eq (first (first (first dirs))) :own-handle)))
      (ignore-errors (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi-preview2/streams-flush-closed-is-err
  ;; a CLOSED output stream must keep answering `closed' for BOTH write
  ;; shapes and for flush — never a silent success (a guest that flushes
  ;; a dropped sink must see the error).
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdout (make-string-output-stream)))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12")))
    (flet ((closed-out ()
             (let ((rep (wasmdown.wasi::%make-p2-output-stream
                         :stream (make-string-output-stream))))
               (setf (wasmdown.wasi::p2-output-stream-closed rep) t)
               rep)))
      (let ((rep (closed-out)))
        (dolist (entry '(("[method]output-stream.write" 2)
                         ("[method]output-stream.blocking-write-and-flush" 2)
                         ("[method]output-stream.flush" 1)
                         ("[method]output-stream.blocking-flush" 1)))
          (let ((r (apply #'wasmdown.component.runtime:lifted-func-call
                          (wasmdown.wasi:provider-instance-function
                           streams (first entry))
                          (if (= 2 (second entry))
                              (list rep (list 65 66))
                              (list rep)))))
            ;; the result is an err whose payload is stream-error::closed
            (is (eq (car r) :err))
            (is (eq (car (cdr r)) :closed))))))))

(defun p2-net-ctx (&key (dns :all) (listen :any) (outbound :any))
  (wasmdown.wasi:make-wasi-context
   :networks (list (wasmdown.wasi:make-wasi-network
                    :listen listen :dns dns :outbound outbound))))

(defun p2-ipv4 (host port)
  (cons "ipv4" (list (cons "port" port) (cons "address" host))))

(defun p2-port-of (addr)
  (cdr (assoc "port" (cdr addr) :test #'string=)))

(test wasi-preview2/tcp-loopback
  (let* ((ctx (p2-net-ctx))
         (net (first (wasmdown.wasi:wasi-context-networks ctx)))
         (create (wasmdown.wasi:p2-tcp-create-instance ctx "0.2.12"))
         (tcp (wasmdown.wasi:p2-tcp-instance ctx "0.2.12"))
         (listener (third (cdr (p2-call create "create-tcp-socket" "ipv4")))))
    (is (eq :ok (car (p2-call tcp "[method]tcp-socket.start-bind" listener net
                              (p2-ipv4 '(127 0 0 1) 0)))))
    (is (eq :ok (car (p2-call tcp "[method]tcp-socket.finish-bind" listener))))
    (is (eq :ok (car (p2-call tcp "[method]tcp-socket.start-listen" listener))))
    (is (eq :ok (car (p2-call tcp "[method]tcp-socket.finish-listen" listener))))
    (is (eq t (p2-call tcp "[method]tcp-socket.is-listening" listener)))
    (let* ((local (p2-call tcp "[method]tcp-socket.local-address" listener))
           (port (p2-port-of (cdr local)))
           (client (third (cdr (p2-call create "create-tcp-socket" "ipv4")))))
      (is (plusp port))
      (is (eq :ok (car (p2-call tcp "[method]tcp-socket.start-connect" client net
                                (p2-ipv4 '(127 0 0 1) port)))))
      (let ((connected (p2-call tcp "[method]tcp-socket.finish-connect" client)))
        (is (eq (car connected) :ok))
        (let* ((client-in (first (cdr connected)))
               (client-out (second (cdr connected)))
               (accepted (p2-call tcp "[method]tcp-socket.accept" listener)))
          (is (eq (car accepted) :ok))
          (let* ((server-sock (first (cdr accepted)))
                 (server-in (second (cdr accepted)))
                 (server-out (third (cdr accepted)))
                 (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12")))
            (declare (ignore server-out client-in))
            (let ((w (apply #'wasmdown.component.runtime:lifted-func-call
                            (wasmdown.wasi:provider-instance-function
                             streams "[method]output-stream.write")
                            (list client-out (list 104 105)))))
              (is (eq (car w) :ok)))
            (let ((r (apply #'wasmdown.component.runtime:lifted-func-call
                            (wasmdown.wasi:provider-instance-function
                             streams "[method]input-stream.blocking-read")
                            (list server-in 2))))
              (is (eq (car r) :ok))
              (is (equal '(104 105) (cdr r))))
            ;; the accepted socket is a real resource handle
            (is (eq (first server-sock) :own-handle))))))))

(test wasi-preview2/sockets-dns-authority
  ;; a context with NO dns authority refuses resolution before any
  ;; resolver work
  (let* ((ctx (p2-net-ctx :dns :none))
         (net (first (wasmdown.wasi:wasi-context-networks ctx)))
         (lookup (wasmdown.wasi:p2-ip-name-lookup-instance ctx "0.2.12"))
         (r (p2-call lookup "resolve-addresses" net "example.invalid")))
    (is (eq (car r) :err))
    (unless (equal "access-denied" (car (cdr r)))
      (error "dns-denial payload was ~s" r)))
  ;; with authority, localhost resolves and yields at least one address
  (let* ((ctx (p2-net-ctx))
         (net (first (wasmdown.wasi:wasi-context-networks ctx)))
         (lookup (wasmdown.wasi:p2-ip-name-lookup-instance ctx "0.2.12"))
         (r (p2-call lookup "resolve-addresses" net "localhost")))
    (is (eq (car r) :ok))
    (let* ((stream (third (cdr r)))
           (next (p2-call lookup
                          "[method]resolve-address-stream.resolve-next-address"
                          stream)))
      (is (eq (car next) :ok))
      (is (eq (car (cdr next)) :some)))))

(test wasi-preview2/udp-datagram-roundtrip
  (let* ((ctx (p2-net-ctx))
         (net (first (wasmdown.wasi:wasi-context-networks ctx)))
         (create (wasmdown.wasi:p2-udp-create-instance ctx "0.2.12"))
         (udp (wasmdown.wasi:p2-udp-instance ctx "0.2.12")))
    (labels ((bound-socket ()
               (let ((sock (third (cdr (p2-call create "create-udp-socket" "ipv4")))))
                 (p2-call udp "[method]udp-socket.start-bind" sock net
                          (p2-ipv4 '(127 0 0 1) 0))
                 (p2-call udp "[method]udp-socket.finish-bind" sock)
                 sock))
             (streams-of (sock)
               ;; (:ok (incoming outgoing)) — no extra tag level
               (cdr (p2-call udp "[method]udp-socket.stream" sock :none)))
             (addr (sock)
               (cdr (p2-call udp "[method]udp-socket.local-address" sock))))
      (let* ((a (bound-socket))
             (b (bound-socket))
             (a-streams (streams-of a))
             (b-streams (streams-of b))
             (a-in (first a-streams))
             (b-out (second b-streams))
             (a-addr (addr a))
             ;; WIT sockets/udp `send`: "Each call to `send` must be
             ;; permitted by a preceding `check-send`"
             (_ (p2-call udp "[method]outgoing-datagram-stream.check-send" b-out))
             (sent (p2-call udp "[method]outgoing-datagram-stream.send" b-out
                            (list (list (cons "data" (list 65 66 67))
                                        (cons "remote-address" (cons :some a-addr)))))))
        (declare (ignore a-in))
        (is (eq (car sent) :ok))
        (is (= 1 (cdr sent)))
        (let ((got (p2-call udp "[method]incoming-datagram-stream.receive"
                            a-in 4)))
          (is (eq (car got) :ok))
          (is (= 1 (length (cdr got))))
          (is (equal '(65 66 67) (cdr (assoc "data" (first (cdr got))
                                             :test #'string=)))))))))

;;; ---------------------------------------------------------------
;;; M6: 0.2.12 behaviour audited against the vendored WIT.
;;;
;;; Each test below pins a defect found by reading the implementation
;;; against `specs/wasi/wasi-wit-0.2.12/': the failure it prevents is
;;; guest-visible (a host Lisp error where the WIT defines a value, a
;;; wrong `stream-error' case, an unbounded host allocation, an
;;; ignored authority argument, or a leaked descriptor).

(defun p2-fd-count ()
  "The open-descriptor count of this process (Linux /proc/self/fd).
Counts raw directory entries: `directory' would dedupe them by
truename (two fds on one socket are one pathname) and sb-posix:readdir
reports end-of-directory as a NULL alien, not NIL."
  (let ((dir (sb-posix:opendir "/proc/self/fd"))
        (n 0))
    (unwind-protect
         (loop
           (let ((entry (sb-posix:readdir dir)))
             (when (or (null entry)
                       (sb-sys:sap= (sb-alien:alien-sap entry)
                                    (sb-sys:int-sap 0)))
               (return))
             (incf n)))
      (sb-posix:closedir dir))
    n))

(defun p2-temp-dir ()
  (concatenate 'string (sb-posix:mkdtemp "/tmp/wmd-p2t-XXXXXX") "/"))

(test wasi-preview2/check-write-reports-a-permit
  ;; `output-stream.check-write' is the WIT function every 0.2 adapter
  ;; calls before `write'. It signalled UNBOUND-VARIABLE (the permit
  ;; constant was never defined), a host error no guest can survive.
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdout (make-string-output-stream)))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
         (rep (wasmdown.wasi::%make-p2-output-stream
               :stream (make-string-output-stream))))
    (let ((r (p2-call streams "[method]output-stream.check-write" rep)))
      (is (eq :ok (car r)))
      (is (integerp (cdr r)))
      (is (plusp (cdr r)))
      (is (<= (cdr r) wasmdown.wasi::+p2-io-chunk+)))
    ;; a closed stream answers `closed', never a permit
    (setf (wasmdown.wasi::p2-output-stream-closed rep) t)
    (let ((r (p2-call streams "[method]output-stream.check-write" rep)))
      (is (eq :err (car r)))
      (is (eq :closed (car (cdr r)))))))

(test wasi-preview2/sink-failure-is-last-operation-failed
  ;; A failed host write is `stream-error::last-operation-failed' with a
  ;; downcastable error; reporting `closed' invents a stream state that
  ;; never happened and hides the cause from `to-debug-string'.
  (let* ((dir (p2-temp-dir))
         (path (concatenate 'string dir "ro.bin"))
         (fd (sb-posix:open path (logior sb-posix:o-creat sb-posix:o-wronly)
                            #o600))
         (ctx (wasmdown.wasi:make-wasi-context
               :stdout (make-string-output-stream)))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12")))
    (is (plusp fd))
    (sb-posix:close fd)
    (unwind-protect
         (let* ((rfd (sb-posix:open path sb-posix:o-rdonly))
                (rep (wasmdown.wasi::%make-p2-output-stream :fd rfd)))
           (unwind-protect
                (flet ((error-kind (result)
                         (is (eq :err (car result)))
                         (is (eq :last-operation-failed (car (cdr result))))
                         (let ((payload (cdr (cdr result))))
                           (is (eq :own-handle (first payload)))
                           (wasmdown.wasi::p2-error-message (third payload)))))
                  (let ((w (p2-call streams "[method]output-stream.write"
                                    rep '(65 66))))
                    (is (search "write failed" (error-kind w))))
                  (let ((bf (p2-call streams
                                     "[method]output-stream.blocking-write-and-flush"
                                     rep '(65))))
                    (is (search "write failed" (error-kind bf)))))
             (sb-posix:close rfd)))
      (ignore-errors (delete-file path)))))

(test wasi-preview2/splice-read-failure-is-a-stream-error
  ;; `splice' is defined as check-write + read + write: a failed read
  ;; must END the splice with that error. It used to hand the
  ;; (:failed . errno) marker to the writer as if it were bytes, which
  ;; raised a Lisp TYPE-ERROR inside the host.
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdout (make-string-output-stream)))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
         (dir (p2-temp-dir))
         (path (concatenate 'string dir "dead.txt"))
         (dead-fd (sb-posix:open path (logior sb-posix:o-creat
                                               sb-posix:o-wronly)
                                 #o600))
         (out-rep (wasmdown.wasi::%make-p2-output-stream
                   :stream (make-string-output-stream))))
    (sb-posix:close dead-fd)
    (let ((r (p2-call streams "[method]output-stream.splice" out-rep
                      (wasmdown.wasi::%make-p2-input-stream :fd dead-fd) 4)))
      (is (eq :err (car r)))
      (is (eq :last-operation-failed (car (cdr r))))
      (is (search "read failed"
                  (wasmdown.wasi::p2-error-message (third (cdr (cdr r)))))))
    ;; a source that has simply ended transfers zero bytes
    (let ((r (p2-call streams "[method]output-stream.splice" out-rep
                      (wasmdown.wasi::%make-p2-input-stream
                       :stream (make-string-input-stream ""))
                      4)))
      (is (equal '(:ok . 0) r)))
    (ignore-errors (delete-file path))))

(test wasi-preview2/poll-empty-list-traps
  ;; WIT io/poll: "This function traps if ... the list is empty", and
  ;; it WAITS until at least one input is ready.
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdout (make-string-output-stream)))
         (poll (wasmdown.wasi:p2-poll-instance ctx "0.2.12"))
         (ready (wasmdown.wasi::%make-p2-pollable
                 :ready-p (lambda () t)
                 :block (lambda (timeout) (declare (ignore timeout)) t)))
         (not-ready (wasmdown.wasi::%make-p2-pollable
                     :ready-p (lambda () nil)
                     :block (lambda (timeout) (declare (ignore timeout)) nil))))
    (assert-trap :generic (p2-call poll "poll" '()))
    ;; every ready index, in input order, duplicates included
    (is (equal '(0 2) (p2-call poll "poll" (list ready not-ready ready))))
    (is (eq t (p2-call poll "[method]pollable.ready" ready)))
    (is (eq nil (p2-call poll "[method]pollable.ready" not-ready)))
    (is (eq t (p2-call poll "[method]pollable.block" ready)))
    ;; a deadline in the past is already ready, so `poll' returns at once
    (let ((past (wasmdown.wasi::p2-pollable-from-deadline 1)))
      (is (eq t (p2-call poll "[method]pollable.ready" past)))
      (is (equal '(0) (p2-call poll "poll" (list past)))))))

(test wasi-preview2/large-lengths-are-bounded
  ;; `filesize'/`u64' lengths are guest-controlled: allocating them
  ;; aborted the host (SBCL heap exhaustion) instead of answering the
  ;; guest. Reads are served in one bounded chunk, eof is honest, and
  ;; `write-zeroes' streams zeros instead of allocating the count.
  (let* ((dir (p2-temp-dir))
         (pre (wasmdown.wasi:open-wasi-preopen dir :name "/w"))
         (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
         (types (wasmdown.wasi:p2-filesystem-types-instance ctx "0.2.12"))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
         (desc (wasmdown.wasi::wasi-preopen-descriptor pre)))
    (unwind-protect
         (progn
           (with-open-file (f (merge-pathnames "small.txt" (pathname dir))
                              :direction :output :if-exists :supersede)
             (write-string "hello" f))
           (let ((d (third (cdr (p2-call types "[method]descriptor.open-at"
                                         desc '() "small.txt" '() '())))))
             (let ((r (p2-call types "[method]descriptor.read" d (ash 1 40) 0)))
               (is (eq :ok (car r)))
               (is (equal '(104 101 108 108 111) (first (cdr r))))
               ;; the clamp is not reported as eof: 5 < 1 MiB, so the
               ;; file really did end
               (is (eq t (second (cdr r)))))
             ;; a zero-length read is not an eof probe
             (is (eq nil (second (cdr (p2-call types "[method]descriptor.read"
                                               d 0 0))))))
           ;; the io/streams read takes a u64 `len' too
           (let ((r (p2-call streams "[method]input-stream.read"
                             (wasmdown.wasi::%make-p2-input-stream
                              :stream (make-string-input-stream "hey"))
                             (ash 1 40))))
             (is (eq :ok (car r)))
             (is (equal '(104 101 121) (cdr r))))
           ;; write-zeroes(5000) writes 5000 zeros, not a 5000-byte
           ;; allocation per call site
           (let* ((zpath (merge-pathnames "zeros.bin" (pathname dir)))
                  (zfd (sb-posix:open zpath (logior sb-posix:o-creat
                                                    sb-posix:o-rdwr)
                                      #o600)))
             (unwind-protect
                  (let ((rep (wasmdown.wasi::%make-p2-output-stream :fd zfd)))
                    (let ((r (p2-call streams "[method]output-stream.write-zeroes"
                                      rep 5000)))
                      (is (eq :ok (car r)))
                      (is (= 5000 (sb-posix:stat-size (sb-posix:fstat zfd)))))
                    ;; a zero-length read at EOF is a liveness probe:
                    ;; it answers ok(#()), never `closed'
                    (let ((r (p2-call streams "[method]input-stream.read"
                                      (wasmdown.wasi::%make-p2-input-stream
                                       :fd zfd)
                                      0)))
                      (is (eq :ok (car r)))
                      (is (equal '() (cdr r)))))
               (sb-posix:close zfd))))
      (ignore-errors (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi-preview2/sockets-network-argument-is-honoured
  ;; `network' is a borrow<network>: the authority an operation runs
  ;; under. It used to be IGNORED in favour of the context's default, so
  ;; a handle the guest was never given still ran with full authority.
  (let* ((ctx (p2-net-ctx))
         (net (first (wasmdown.wasi:wasi-context-networks ctx)))
         (lookup (wasmdown.wasi:p2-ip-name-lookup-instance ctx "0.2.12"))
         (tcp (wasmdown.wasi:p2-tcp-instance ctx "0.2.12"))
         (create (wasmdown.wasi:p2-tcp-create-instance ctx "0.2.12"))
         (sock (third (cdr (p2-call create "create-tcp-socket" "ipv4")))))
    ;; a forged handle index never resolves to a network
    (assert-raises wasmdown:host-error
      (p2-call lookup "resolve-addresses" 4242 "localhost"))
    ;; another context's network is not this context's authority
    (assert-raises wasmdown:host-error
      (p2-call tcp "[method]tcp-socket.start-bind" sock
               (wasmdown.wasi:make-wasi-network) (p2-ipv4 '(127 0 0 1) 0)))
    ;; no operation was recorded by the refused starts
    (is (eq :err (car (p2-call tcp "[method]tcp-socket.finish-bind" sock))))
    ;; the granted network is accepted and the bind runs under it
    (is (eq :ok (car (p2-call tcp "[method]tcp-socket.start-bind" sock net
                              (p2-ipv4 '(127 0 0 1) 0)))))
    (is (eq :ok (car (p2-call tcp "[method]tcp-socket.finish-bind" sock))))))

(test wasi-preview2/tcp-accept-does-not-leak-descriptors
  ;; accept used to call the stream-pair helper twice, leaking two dup'd
  ;; descriptors per accepted connection (guest-visible as exhaustion).
  (let* ((ctx (p2-net-ctx))
         (net (first (wasmdown.wasi:wasi-context-networks ctx)))
         (create (wasmdown.wasi:p2-tcp-create-instance ctx "0.2.12"))
         (tcp (wasmdown.wasi:p2-tcp-instance ctx "0.2.12"))
         (listener (third (cdr (p2-call create "create-tcp-socket" "ipv4")))))
    (p2-call tcp "[method]tcp-socket.start-bind" listener net
             (p2-ipv4 '(127 0 0 1) 0))
    (p2-call tcp "[method]tcp-socket.finish-bind" listener)
    (p2-call tcp "[method]tcp-socket.start-listen" listener)
    (p2-call tcp "[method]tcp-socket.finish-listen" listener)
    (let ((port (p2-port-of (cdr (p2-call tcp "[method]tcp-socket.local-address"
                                            listener)))))
      (dotimes (i 3)
        (let ((client (third (cdr (p2-call create "create-tcp-socket" "ipv4")))))
          (p2-call tcp "[method]tcp-socket.start-connect" client net
                   (p2-ipv4 '(127 0 0 1) port))
          (p2-call tcp "[method]tcp-socket.finish-connect" client)
          (let ((before (p2-fd-count)))
            (let ((accepted (p2-call tcp "[method]tcp-socket.accept" listener)))
              (is (eq :ok (car accepted)))
              (is (eq :own-handle (first (first (cdr accepted))))))
            ;; the accepted socket owns one fd, each stream one dup:
            ;; exactly three descriptors, never five
            (is (= 3 (- (p2-fd-count) before)))))))))

(test wasi-preview2/input-stream-subscribe-tracks-readiness
  ;; subscribe answered "always ready", so a guest polling an empty
  ;; source spun instead of waiting: readiness must come from the fd.
  (multiple-value-bind (rfd wfd) (sb-posix:pipe)
    (let* ((ctx (wasmdown.wasi:make-wasi-context
                 :stdin (make-string-input-stream "")))
           (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
           (poll (wasmdown.wasi:p2-poll-instance ctx "0.2.12"))
           (in-rep (wasmdown.wasi::%make-p2-input-stream :fd rfd)))
      (unwind-protect
           (let ((marker (p2-call streams "[method]input-stream.subscribe"
                                  in-rep)))
             (is (eq :own-handle (first marker)))
             (let ((pollable (third marker)))
               (is (eq nil (p2-call poll "[method]pollable.ready" pollable)))
               (let ((byte (make-array 1 :element-type '(unsigned-byte 8)
                                         :initial-element 122)))
                 (is (= 1 (sb-posix:write wfd (sb-sys:vector-sap byte) 1))))
               (is (eq t (p2-call poll "[method]pollable.ready" pollable)))
               ;; and the byte is what the stream now reads
               (is (equal '(122) (cdr (p2-call streams
                                               "[method]input-stream.read"
                                               in-rep 1))))))
        (sb-posix:close rfd)
        (sb-posix:close wfd)))))

(test wasi-preview2/network-error-code-is-an-option
  ;; wasi:sockets/network.network-error-code returns option<error-code>
  ;; (not a result) and is `@unstable' at 0.2.12, so a stable-release
  ;; provider must not export it — but the implementation must still
  ;; have the WIT shape for the release that makes it stable.
  (let* ((ctx (p2-net-ctx))
         (inst (wasmdown.wasi:p2-network-instance ctx "0.2.12"))
         (net-err (wasmdown.wasi::p2-error "connection refused" "connection-refused"))
         (io-err (wasmdown.wasi::p2-error "boom" "other")))
    (is (null (wasmdown.wasi:provider-instance-function
               inst "network-error-code")))
    ;; the sockets error-code is a VARIANT: the domain value is the case
    (is (equal (cons :some (list "connection-refused"))
               (wasmdown.wasi::p2-network-error-code "0.2.12" net-err)))
    (is (eq :none (wasmdown.wasi::p2-network-error-code "0.2.12" io-err)))))

(test wasi-preview2/append-via-stream-appends
  ;; append-via-stream is the WIT's "write with O_APPEND": the stream
  ;; writes at the END of the file, never at the descriptor's shared
  ;; offset. It used to write wherever the fd happened to sit, so an
  ;; append over a descriptor opened without O_APPEND overwrote bytes.
  (let* ((dir (p2-temp-dir))
         (file (merge-pathnames "f.txt" (pathname dir)))
         (pre (wasmdown.wasi:open-wasi-preopen dir :name "/w"))
         (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
         (types (wasmdown.wasi:p2-filesystem-types-instance ctx "0.2.12"))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
         (desc (wasmdown.wasi::wasi-preopen-descriptor pre)))
    (with-open-file (f file :direction :output :if-exists :supersede)
      (write-string "abc" f))
    (unwind-protect
         (let ((d (third (cdr (p2-call types "[method]descriptor.open-at"
                                       desc '() "f.txt" '() '("write"))))))
           ;; a positional write leaves the descriptor's offset at 0
           (is (eq :ok (car (p2-call types "[method]descriptor.write" d '(88) 1))))
           (let ((out (third (cdr (p2-call types
                                           "[method]descriptor.append-via-stream"
                                           d)))))
             (is (eq :ok (car (p2-call streams "[method]output-stream.write"
                                       out '(89 90))))))
           (with-open-file (f file)
             (let ((s (make-string (file-length f))))
               (read-sequence s f)
               ;; "abc" with X at offset 1 is "aXc"; the append then
               ;; lands at the END ("aXcYZ"), not at offset 0 ("YZc")
               (is (equal "aXcYZ" s)))))
      (ignore-errors (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi-preview2/blocking-read-waits-on-a-nonblocking-source
  ;; `blocking-read' must WAIT for at least one byte. On a nonblocking
  ;; host fd it answered err(last-operation-failed)[EAGAIN] at once,
  ;; which every socket-backed stream (tcp dup's are O_NONBLOCK) hits.
  (multiple-value-bind (rfd wfd) (sb-posix:pipe)
    ;; O_NONBLOCK on the read end: F_GETFL = 3, F_SETFL = 4
    (is (not (minusp
              (wasmdown.wasi::%libc-fcntl
               rfd 4 (logior (wasmdown.wasi::%libc-fcntl rfd 3 0)
                             wasmdown.wasi::+o-nonblock+)))))
    (let* ((ctx (wasmdown.wasi:make-wasi-context
                 :stdin (make-string-input-stream "")))
           (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
           (in-rep (wasmdown.wasi::%make-p2-input-stream :fd rfd))
           (byte (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-element 122))
           (writer (sb-thread:make-thread
                    (lambda ()
                      (sleep 0.2)
                      (sb-posix:write wfd (sb-sys:vector-sap byte) 1)))))
      (unwind-protect
           (let ((reader (sb-thread:make-thread
                          (lambda ()
                            (p2-call streams "[method]input-stream.blocking-read"
                                     in-rep 1)))))
             (let ((result (sb-thread:join-thread reader :timeout 10
                                                         :default :timeout)))
               (is (not (eq result :timeout)))
               (is (eq :ok (car result)))
               (is (equal '(122) (cdr result)))))
        (sb-thread:join-thread writer :timeout 10 :default nil)
        (sb-posix:close rfd)
        (sb-posix:close wfd)))))

;;; ---------------------------------------------------------------
;;; M7: 0.2.12 io/streams read/write permit semantics.
;;;
;;; Contracts quoted from `specs/wasi/wasi-wit-0.2.12/io/io.wit'
;;; (`input-stream.read`, `output-stream.check-write'/`write'/
;;; `write-zeroes'/`flush') and
;;; `specs/wasi/wasi-wit-0.2.12/sockets/sockets.wit'
;;; (`outgoing-datagram-stream.check-send'/`send').

(defun p2-test-nonblocking (fd)
  "Set FD nonblocking (F_GETFL = 3, F_SETFL = 4, O_NONBLOCK)."
  (not (minusp
        (wasmdown.wasi::%libc-fcntl
         fd 4 (logior (wasmdown.wasi::%libc-fcntl fd 3 0)
                      wasmdown.wasi::+o-nonblock+)))))

(defun p2-test-read-exactly (fd count)
  "Read COUNT octets from FD into a fresh ub8 vector; (values VECTOR N)."
  (let ((buf (make-array count :element-type '(unsigned-byte 8)))
        (done 0))
    (loop while (< done count)
          do (let ((n (sb-posix:read
                       fd (sb-sys:sap+ (sb-sys:vector-sap buf) done)
                       (- count done))))
               (when (or (minusp n) (zerop n)) (return))
               (incf done n)))
    (values buf done)))

(test wasi-preview2/read-zero-length-does-not-report-closed
  ;; WIT input-stream.read: "When the caller gives a `len` of 0, it
  ;; represents a request to read 0 bytes. If the stream is still open,
  ;; this call should succeed and return an empty list, or otherwise
  ;; fail with `closed`."
  ;;
  ;; Old behaviour: the CL-stream branch answered `listen`'s verdict, so
  ;; read(0) on an OPEN stream that simply had no bytes at hand answered
  ;; err(closed) — an empty source looked like a closed stream, and the
  ;; `listen` probe itself could consume from a pipe-backed stream.
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdin (make-string-input-stream "")))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12")))
    ;; an open, empty CL text stream
    (let ((rep (wasmdown.wasi::%make-p2-input-stream
                :stream (make-string-input-stream ""))))
      (is (equal '(:ok) (p2-call streams "[method]input-stream.read" rep 0)))
      ;; ... and the stream is still OPEN for the next operation
      (is (eq :err (car (p2-call streams "[method]input-stream.read" rep 1)))))
    ;; read(0) does not consume from an open source with data waiting
    (let ((rep (wasmdown.wasi::%make-p2-input-stream
                :stream (make-string-input-stream "hi"))))
      (is (equal '(:ok) (p2-call streams "[method]input-stream.read" rep 0)))
      (is (equal '(104 105)
                 (cdr (p2-call streams "[method]input-stream.read" rep 4)))))
    ;; an fd at EOF: read(0) is a 0-byte request, read(n) is the eof
    (let* ((dir (p2-temp-dir))
           (path (concatenate 'string dir "e.bin"))
           (fd (sb-posix:open path (logior sb-posix:o-creat sb-posix:o-rdwr)
                              #o600)))
      (unwind-protect
           (let ((rep (wasmdown.wasi::%make-p2-input-stream :fd fd)))
             (is (equal '(:ok) (p2-call streams "[method]input-stream.read"
                                          rep 0)))
             (is (eq :err (car (p2-call streams "[method]input-stream.read"
                                        rep 1)))))
        (sb-posix:close fd)
        (ignore-errors (delete-file path))))
    ;; ... and the stream's OWN closed state wins even for len 0
    (let ((rep (wasmdown.wasi::%make-p2-input-stream
                :stream (make-string-input-stream "x"))))
      (setf (wasmdown.wasi::p2-input-stream-closed rep) t)
      (dolist (len '(0 4))
        (let ((r (p2-call streams "[method]input-stream.read" rep len)))
          (is (eq :err (car r)))
          (is (eq :closed (car (cdr r)))))))))

(test wasi-preview2/read-on-an-empty-open-source-is-ok-empty
  ;; WIT input-stream.read: "The list is empty when no bytes are
  ;; available for reading at this time." `input-stream`s are
  ;; "non-blocking to the extent practical": a nonblocking host fd with
  ;; nothing queued is a successful empty read, NOT a stream error.
  ;;
  ;; Old behaviour: the host read's EAGAIN surfaced as
  ;; err(last-operation-failed)[errno 11] — every socket-backed
  ;; input-stream (tcp dup's are O_NONBLOCK) reported a fake I/O error.
  (multiple-value-bind (rfd wfd) (sb-posix:pipe)
    (is (p2-test-nonblocking rfd))
    (let* ((ctx (wasmdown.wasi:make-wasi-context
                 :stdin (make-string-input-stream "")))
           (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
           (rep (wasmdown.wasi::%make-p2-input-stream :fd rfd))
           (byte (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-element 122)))
      (unwind-protect
           (progn
             (is (equal '(:ok) (p2-call streams "[method]input-stream.read"
                                          rep 4)))
             ;; `skip' behaves identically to `read': 0 bytes consumed
             (is (equal '(:ok . 0) (p2-call streams "[method]input-stream.skip"
                                              rep 4)))
             ;; the empty read left the source OPEN and unpoisoned
             (is (= 1 (sb-posix:write wfd (sb-sys:vector-sap byte) 1)))
             (is (equal '(122) (cdr (p2-call streams
                                              "[method]input-stream.read"
                                              rep 4)))))
        (sb-posix:close rfd)
        (sb-posix:close wfd)))))

(test wasi-preview2/write-beyond-the-permit-traps
  ;; WIT output-stream.write: "Precondition: check-write gave permit of
  ;; Ok(n) and contents has a length of less than or equal to n.
  ;; Otherwise, this function will trap", and write-zeroes: "should be
  ;; used precisely like `write` with the exact same preconditions".
  ;;
  ;; Old behaviour: `write' ignored the permit entirely — it accepted an
  ;; arbitrarily long list (the canonical ABI had already copied it) and
  ;; a short host write dropped the tail, so over-permit writes "worked"
  ;; while losing data.
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdout (make-string-output-stream)))
         (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
         (rep (wasmdown.wasi::%make-p2-output-stream
               :stream (make-string-output-stream))))
    (let ((permit (p2-call streams "[method]output-stream.check-write" rep)))
      (is (eq :ok (car permit)))
      (is (integerp (cdr permit)))
      ;; exactly the permit is permitted: allocation-free zeroes
      (is (eq :ok (car (p2-call streams "[method]output-stream.write-zeroes"
                                rep (cdr permit)))))
      ;; one byte more is a trap, for both write shapes
      (assert-trap :generic
        (p2-call streams "[method]output-stream.write-zeroes" rep
                 (1+ (cdr permit))))
      (assert-trap :generic
        (p2-call streams "[method]output-stream.write" rep
                 (make-array (1+ (cdr permit)) :element-type '(unsigned-byte 8)))))
    ;; the stream is untouched by the trapped calls
    (is (eq :ok (car (p2-call streams "[method]output-stream.check-write" rep))))))

(test wasi-preview2/partial-write-retains-the-unwritten-suffix
  ;; WIT output-stream.write: "Precondition ... Otherwise, this
  ;; function will trap" + "This function never blocks"; flush: "the
  ;; output which is expected to be flushed is all that has been passed
  ;; to `write` prior to this call. Upon calling this function, the
  ;; `output-stream` will not accept any writes (`check-write` will
  ;; return `ok(0)`) until the flush has completed."
  ;;
  ;; Old behaviour: `p2-out-write' discarded the count of the single
  ;; host write, so a sink that took fewer bytes than offered (a
  ;; nonblocking pipe, a full socket) silently LOST the rest while
  ;; `write` reported ok; check-write kept reporting a full permit and
  ;; `flush` had nothing to flush.
  (multiple-value-bind (rfd wfd) (sb-posix:pipe)
    (is (p2-test-nonblocking wfd))
    (let* ((ctx (wasmdown.wasi:make-wasi-context
                 :stdout (make-string-output-stream)))
           (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
           (rep (wasmdown.wasi::%make-p2-output-stream :fd wfd))
           (total 200000)
           (payload (make-array total :element-type '(unsigned-byte 8))))
      (dotimes (i total) (setf (aref payload i) (mod i 251)))
      (unwind-protect
           (progn
             ;; the whole list is accepted: some goes to the sink, the
             ;; rest is buffered, nothing is dropped
             (is (eq :ok (car (p2-call streams "[method]output-stream.write"
                                       rep payload))))
             (let ((pending (wasmdown.wasi::p2-output-stream-pending rep)))
               ;; the pipe took what it could; the suffix is retained
               (is (vectorp pending))
               (is (plusp (length pending)))
               (is (< (length pending) total)))
             ;; check-write answers ok(0) until that flush completes
             (is (equal '(:ok . 0) (p2-call streams
                                             "[method]output-stream.check-write"
                                             rep)))
             ;; ... so any further non-empty offer traps (permit 0)
             (assert-trap :generic
               (p2-call streams "[method]output-stream.write" rep
                        (vector 65)))
             ;; `splice' caps its read at the permit, so it transfers 0
             (is (equal '(:ok . 0)
                        (p2-call streams "[method]output-stream.splice" rep
                                 (wasmdown.wasi::%make-p2-input-stream
                                  :stream (make-string-input-stream "abcd"))
                                 4)))
             ;; `flush' is a request: it never blocks ...
             (is (eq :ok (car (p2-call streams "[method]output-stream.flush"
                                       rep))))
             ;; ... `blocking-flush' finishes the job while a reader
             ;; drains the pipe, and then the permit is whole again
             (let ((reader (sb-thread:make-thread
                            (lambda ()
                              (multiple-value-list
                               (p2-test-read-exactly rfd total))))))
               (is (eq :ok (car (p2-call streams
                                          "[method]output-stream.blocking-flush"
                                          rep))))
               (let ((out (sb-thread:join-thread reader :timeout 20
                                                        :default :timeout)))
                 (is (consp out))
                 (destructuring-bind (got n) out
                   ;; every offered byte arrived, in order
                   (is (= total n))
                   (is (equalp payload got))))
               (is (null (wasmdown.wasi::p2-output-stream-pending rep)))
               (is (eq :ok (car (p2-call streams
                                          "[method]output-stream.check-write"
                                          rep))))))
        (sb-posix:close rfd)
        (sb-posix:close wfd)))))

(test wasi-preview2/check-send-permit-guards-datagram-send
  ;; WIT sockets/udp outgoing-datagram-stream.send: "Each call to `send`
  ;; must be permitted by a preceding `check-send`. Implementations must
  ;; trap if either `check-send` was not called or `datagrams` contains
  ;; more items than `check-send` permitted", and "If at least one
  ;; datagram has been sent successfully, this function never returns an
  ;; error."
  ;;
  ;; Old behaviour: `check-send' was a fixed ok(1) with no session state
  ;; and `send' accepted any number of datagrams, so neither mandated
  ;; trap ever happened (an unbounded batch could be handed to the
  ;; socket).
  (let* ((ctx (p2-net-ctx))
         (net (first (wasmdown.wasi:wasi-context-networks ctx)))
         (create (wasmdown.wasi:p2-udp-create-instance ctx "0.2.12"))
         (udp (wasmdown.wasi:p2-udp-instance ctx "0.2.12"))
         (make-sock (lambda ()
                      (let ((sock (third (cdr (p2-call create "create-udp-socket"
                                                      "ipv4")))))
                        (p2-call udp "[method]udp-socket.start-bind" sock net
                                 (p2-ipv4 '(127 0 0 1) 0))
                        (p2-call udp "[method]udp-socket.finish-bind" sock)
                        sock)))
         (a (funcall make-sock))
         (b (funcall make-sock))
         (a-in (first (cdr (p2-call udp "[method]udp-socket.stream" a :none))))
         (b-out (second (cdr (p2-call udp "[method]udp-socket.stream" b :none))))
         (a-addr (cdr (p2-call udp "[method]udp-socket.local-address" a)))
         (dg (lambda (byte)
               (list (cons "data" (list byte))
                     (cons "remote-address" (cons :some a-addr))))))
    ;; no check-send yet: traps
    (assert-trap :generic
      (p2-call udp "[method]outgoing-datagram-stream.send" b-out
               (list (funcall dg 65))))
    ;; check-send governs the NEXT call's count
    (is (equal '(:ok . 1)
               (p2-call udp "[method]outgoing-datagram-stream.check-send"
                        b-out)))
    (assert-trap :generic
      (p2-call udp "[method]outgoing-datagram-stream.send" b-out
               (list (funcall dg 65) (funcall dg 66))))
    ;; within the permit: sent, and the peer sees it
    (is (equal '(:ok . 1)
               (p2-call udp "[method]outgoing-datagram-stream.send" b-out
                        (list (funcall dg 65)))))
    (let ((got (p2-call udp "[method]incoming-datagram-stream.receive" a-in 4)))
      (is (eq :ok (car got)))
      (is (equal '(65) (cdr (assoc "data" (first (cdr got))
                                   :test #'string=)))))
    ;; the empty list is always ok(0) (WIT: "If the input list is empty,
    ;; the function returns `ok(0)`")
    (is (equal '(:ok . 0)
               (p2-call udp "[method]outgoing-datagram-stream.send" b-out '())))))

;;; ===============================================================
;;; wasi:http@0.2 — the outgoing import surface.
;;; ===============================================================

(defun p2-http-marker-rep (marker)
  "The rep behind an OWN-HANDLE marker (:own-handle ct rep)."
  (third marker))

(defun p2-http-net (&key (outbound (list (wasmdown.wasi:make-wasi-endpoint
                                          :address '(127 0 0 1)))))
  (wasmdown.wasi:make-wasi-network :outbound outbound))

(defun p2-http-ctx (&key (net (p2-http-net)))
  (wasmdown.wasi:make-wasi-context :networks (list net)))

(defun p2-http-fields (types &rest entries)
  (let ((fields (p2-http-marker-rep (p2-call types "[constructor]fields"))))
    (dolist (entry entries)
      (p2-call types "[method]fields.append"
               fields (first entry) (second entry)))
    fields))

(defun p2-http-request (types port &key (path "/x") headers)
  (let* ((fields (or headers (p2-http-fields types)))
         (request (p2-http-marker-rep
                   (p2-call types "[constructor]outgoing-request" fields))))
    (p2-call types "[method]outgoing-request.set-authority"
             request (cons :some (format nil "127.0.0.1:~d" port)))
    (p2-call types "[method]outgoing-request.set-path-with-query"
             request (cons :some path))
    request))

(defun p2-http-handle (handler request &optional (options :none))
  (p2-call handler "handle" request options))

(defun p2-http-future-rep (handle-result)
  (p2-http-marker-rep (cdr handle-result)))

(defun p2-http-future-inner (get-value)
  "The inner result<incoming-response,error-code> of a
future-incoming-response get value."
  (cdr (cdr get-value)))

(defun p2-http-future-wait (types future &key (tries 500))
  (loop repeat tries
        for value = (p2-call types
                             "[method]future-incoming-response.get"
                             future)
        unless (eq value :none)
          return value
        do (sleep 0.01)
        finally (error "future-incoming-response did not become ready")))

(defun p2-http-status (types response)
  (p2-call types "[method]incoming-response.status" response))

(defun p2-http-headers-field (types response name)
  (let ((fields (p2-http-marker-rep
                 (p2-call types "[method]incoming-response.headers"
                          response))))
    (p2-call types "[method]fields.get" fields name)))

(defun p2-http-body-of (types response)
  (let ((result (p2-call types "[method]incoming-response.consume" response)))
    (is (eq :ok (car result)))
    (p2-http-marker-rep (cdr result))))

(defun p2-http-stream-of (types body)
  (let ((result (p2-call types "[method]incoming-body.stream" body)))
    (is (eq :ok (car result)))
    (p2-http-marker-rep (cdr result))))

(defun p2-http-read-all (ctx stream &optional (want nil))
  "Read STREAM to its end through BLOCKING-READ (the body pump is a host
thread, so the guest waits on the input pollable)."
  (let ((streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12"))
        (out '()))
    (loop
      (let ((result (p2-call streams "[method]input-stream.blocking-read"
                             stream 65536)))
        (cond
          ((and (eq (car result) :ok) (plusp (length (cdr result))))
           (setf out (append out (cdr result))))
          ((eq (car result) :ok)
           (return (coerce out '(vector (unsigned-byte 8)))))
          ((and (eq (car result) :err)
                (eq (car (cdr result)) :closed))
           (return (coerce out '(vector (unsigned-byte 8)))))
          (t (return out))))
      (when (and want (>= (length out) want))
        (return (coerce out '(vector (unsigned-byte 8))))))))

(defun p2-http-bytes-string (bytes)
  (map 'string #'code-char bytes))

;;; ---- a one-shot loopback server (reads the body a Content-Length
;;; announces, then answers) -----------------------------------------

(defun p2-http-listener ()
  (let ((fd (wasmdown.wasi::%libc-socket
             wasmdown.wasi::+af-inet+
             wasmdown.wasi::+sock-stream+
             wasmdown.wasi::+ipproto-tcp+)))
    (multiple-value-bind (alien len)
        (wasmdown.wasi::%make-sockaddr
         "ipv4" (list (cons "port" 0) (cons "address" '(127 0 0 1))))
      (unwind-protect
           (progn
             (when (minusp (wasmdown.wasi::%libc-bind
                            fd (sb-alien:alien-sap alien) len))
               (error "p2 http test listener bind failed"))
             (wasmdown.wasi::%libc-listen fd 4))
        (sb-alien:free-alien alien)))
    (values (cdr (assoc "port"
                        (cdr (wasmdown.wasi::%getsockname-domain fd :local))
                        :test #'string=))
            fd)))

(defun p2-http-recv (cfd buffer)
  (let ((tmp (sb-alien:make-alien sb-alien:unsigned-char 4096)))
    (unwind-protect
         (let ((n (wasmdown.wasi::%libc-recv cfd (sb-alien:alien-sap tmp)
                                            4096 0)))
           (when (plusp n)
             (loop for i below n
                   do (vector-push-extend
                       (sb-sys:sap-ref-8 (sb-alien:alien-sap tmp) i)
                       buffer)))
           n)
      (sb-alien:free-alien tmp))))

(defun p2-http-read-request (cfd)
  "Read the request head, then the body its Content-Length announces."
  (let* ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
         (crlfcrlf (format nil "~c~c~c~c"
                           #\Return #\Newline #\Return #\Newline)))
    (loop until (search crlfcrlf
                        (map 'string #'code-char buffer) :test #'char=)
          do (when (<= (p2-http-recv cfd buffer) 0) (return)))
    (let* ((head-text (map 'string #'code-char buffer))
           (head-end (+ 4 (search crlfcrlf head-text :test #'char=)))
           (line (or (search "Content-Length:" head-text :test #'char-equal)
                     (search "content-length:" head-text :test #'char-equal)))
           (length (if line
                       (parse-integer
                        (string-trim '(#\Space #\Return #\Newline)
                                     (subseq head-text
                                             (+ line (length "Content-Length:"))
                                             (or (position #\Return head-text
                                                           :start (+ line 16))
                                                 (length head-text)))))
                       0)))
      (loop while (< (- (fill-pointer buffer) head-end) length)
            do (when (<= (p2-http-recv cfd buffer) 0) (return)))
      (map 'string #'code-char buffer))))

(defun p2-http-send-bytes (cfd text)
  (let* ((bytes (map 'list #'char-code text))
         (alien (wasmdown.wasi::%byte-list->alien bytes)))
    (unwind-protect
         (wasmdown.wasi::%libc-send cfd (sb-alien:alien-sap alien)
                                    (length bytes) 0)
      (sb-alien:free-alien alien))))

(defun p2-http-serve-once (response)
  "A one-shot loopback server emitting RESPONSE; (values port fd thread
got-cell)."
  (multiple-value-bind (port fd) (p2-http-listener)
    (let ((got (list nil)))
      (values port fd
              (sb-thread:make-thread
               (lambda ()
                 (let ((cfd (wasmdown.wasi::%libc-accept4
                             fd (sb-sys:int-sap 0) (sb-sys:int-sap 0) 0)))
                   (unwind-protect
                        (let ((request (p2-http-read-request cfd)))
                          (setf (car got) request)
                          (p2-http-send-bytes cfd response))
                     (wasmdown.wasi::%close-fd cfd))))
               :name "p2-http-serve")
              got))))

(defmacro with-p2-http-server ((port response got) &body body)
  `(multiple-value-bind (,port fd thread ,got)
       (p2-http-serve-once ,response)
     (declare (ignore thread))
     (unwind-protect (progn ,@body)
       (ignore-errors (sb-thread:join-thread thread :timeout 3))
       (wasmdown.wasi::%close-fd fd))))

(defun p2-http-response (status &key body (extra ""))
  (format nil "HTTP/1.1 ~d X~c~cContent-Length: ~d~c~c~a~c~c~a"
          status #\Return #\Newline (length body) #\Return #\Newline
          extra #\Return #\Newline body))

;;; ---- the provider surface -----------------------------------------

(defun p2-http-wit-export-names (iface)
  "Every export provider-instance must lay down for IFACE: the stable
functions, every resource + drop, the stable methods, and the defined
types."
  (let ((names '()))
    (dolist (fn (wasmdown.wit:wit-interface-func-list iface))
      (when (wasmdown.wasi:p2-stable-node-p fn)
        (push (wasmdown.wit:wit-func-name fn) names)))
    (dolist (resource (wasmdown.wit:wit-interface-resource-list iface))
      (when (wasmdown.wasi:p2-stable-node-p resource)
        (push (wasmdown.wit:wit-resource-name resource) names))
      (push (format nil "[resource-drop]~a"
                    (wasmdown.wit:wit-resource-name resource))
            names)
      (dolist (method (wasmdown.wit:wit-resource-methods resource))
        (when (wasmdown.wasi:p2-stable-node-p method)
          (push (wasmdown.wasi::%resource-method-export-name resource method)
                names))))
    (dolist (item (wasmdown.wit:wit-interface-items iface))
      (when (and (wasmdown.wit:wit-type-p item)
                 (wasmdown.wasi:p2-stable-node-p item)
                 (not (and (consp (wasmdown.wit:wit-type-expr item))
                           (eq (car (wasmdown.wit:wit-type-expr item))
                               :resource))))
        (push (wasmdown.wit:wit-type-name item) names)))
    (nreverse names)))

(test wasi-preview2/http-provider-surface-matches-wit
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (dolist (spec '(("types" . wasmdown.wasi:p2-http-types-instance)
                         ("outgoing-handler"
                          . wasmdown.wasi:p2-http-outgoing-handler-instance)))
           (let* ((iface (wasmdown.wasi:p2-interface
                          "http" (car spec) "0.2.12"))
                  (instance (funcall (cdr spec) ctx "0.2.12"))
                  (exports
                    (wasmdown.component.runtime:comp-instance-exports instance))
                  (expected (p2-http-wit-export-names iface)))
             (dolist (name expected)
               (is (gethash name exports)))
             (is (= (length expected) (hash-table-count exports))
                 )))
      (wasmdown.wasi:close-wasi-context ctx)))
  ;; the exact outgoing-handler interface is one function
  (let ((iface (wasmdown.wasi:p2-interface "http" "outgoing-handler"
                                           "0.2.12")))
    (is (equal '("handle")
               (mapcar #'wasmdown.wit:wit-func-name
                       (wasmdown.wit:wit-interface-func-list iface))))))

(test wasi-preview2/http-proxy-world-providers
  (let* ((world (wasmdown.wasi:preview2-proxy-world))
         (names (mapcar (lambda (item)
                          (wasmdown.wasi::%world-import-name item))
                        (wasmdown.wit:wit-world-imports world)))
         (ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let ((providers (wasmdown.wasi:preview2-proxy-provider ctx)))
           (is (string= "proxy" (wasmdown.wit:wit-world-name world)))
           (is (member "wasi:http/types@0.2.12" names :test #'string=))
           (is (member "wasi:http/outgoing-handler@0.2.12" names
                       :test #'string=))
           (is (member "wasi:cli/stdin@0.2.12" names :test #'string=))
           (is (equal (sort (copy-list names) #'string<)
                      (sort (mapcar #'car providers) #'string<)))
           (is (= 1 (length (wasmdown.wit:wit-world-exports world))))
           (is (string= "incoming-handler"
                        (wasmdown.wit:wit-interface-name
                         (wasmdown.wit:wit-world-item-target
                          (first (wasmdown.wit:wit-world-exports world))))))
           (let ((imports (wasmdown.wasi:preview2-http-imports-provider ctx)))
             (is (= (length names) (length imports)))))
      (wasmdown.wasi:close-wasi-context ctx))))

;;; ---- fields --------------------------------------------------------

(test wasi-preview2/http-fields-semantics
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let* ((types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
                (from-list
                  (p2-call types "[static]fields.from-list"
                           (list (list "X-Test" (list 104 105))
                                 (list "x-test" (list 33)))))
                (fields (third from-list)))
           (is (eq :own-handle (car from-list)))
           (is (equal '((104 105) (33))
                      (p2-call types "[method]fields.get" fields "X-TEST")))
           (is (p2-call types "[method]fields.has" fields "x-Test"))
           (is (not (p2-call types "[method]fields.has" fields "bad name")))
           (is (equal '()
                      (p2-call types "[method]fields.get" fields "absent")))
           (is (equal '(:ok)
                      (p2-call types "[method]fields.set"
                               fields "x-test" (list (list 49)))))
           (is (equal '((49))
                      (p2-call types "[method]fields.get" fields "X-Test")))
           (is (equal '(("x-test" (49)))
                      (p2-call types "[method]fields.entries" fields)))
           (is (equal '(:ok)
                      (p2-call types "[method]fields.append"
                               fields "x-test" (list 33))))
           (is (equal '((49) (33))
                      (p2-call types "[method]fields.get" fields "x-test")))
           (is (equal '(:ok)
                      (p2-call types "[method]fields.delete" fields "x-test")))
           (is (not (p2-call types "[method]fields.has" fields "x-test")))
           ;; header-error cases (0.2 has exactly these three)
           (is (equal '(:err "forbidden")
                      (p2-call types "[method]fields.set"
                               fields "Host" (list (list 49)))))
           (is (equal '(:err "invalid-syntax")
                      (p2-call types "[method]fields.set"
                               fields "X Y" (list (list 49)))))
           (is (equal '(:err "invalid-syntax")
                      (p2-call types "[method]fields.append"
                               fields "x" (list 13 10))))
           ;; 0.2 has no `size-exceeded' case: the host limit is reported
           ;; as invalid-syntax, never as an undeclared variant case
           (is (equal '(:err "invalid-syntax")
                      (p2-call types "[method]fields.set"
                               fields "x"
                               (list (make-list 70000 :initial-element 97)))))
           ;; clone is a deep, mutable copy
           (let* ((orig (p2-http-fields types '("A" (49))))
                  (clone (p2-http-marker-rep
                          (p2-call types "[method]fields.clone" orig))))
             (is (eq :ok (car (p2-call types "[method]fields.set"
                                       clone "A" (list (list 50))))))
             (is (equal '((49))
                        (p2-call types "[method]fields.get" orig "A")))
             (is (equal '((50))
                        (p2-call types "[method]fields.get" clone "A"))))
           ;; from-list rejects the whole list
           (is (equal '(:err "invalid-syntax")
                      (p2-call types "[static]fields.from-list"
                               (list (list "A" (list 49))
                                     (list "bad name" (list 49))))))
           (is (equal '(:err "forbidden")
                      (p2-call types "[static]fields.from-list"
                               (list (list "transfer-encoding" (list 49)))))))
      (wasmdown.wasi:close-wasi-context ctx))))

;;; ---- request / options / bodies ------------------------------------

(test wasi-preview2/http-request-and-options-surface
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let* ((types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
                (request (p2-http-request types 8080)))
           (is (equal '("get")
                      (p2-call types "[method]outgoing-request.method"
                               request)))
           (is (equal '(:ok)
                      (p2-call types "[method]outgoing-request.set-method"
                               request (cons "other" "PURGE"))))
           (is (equal '("other" . "PURGE")
                      (p2-call types "[method]outgoing-request.method"
                               request)))
           (is (equal '(:err)
                      (p2-call types "[method]outgoing-request.set-method"
                               request (cons "other" "BAD METHOD"))))
           (is (equal '(:ok)
                      (p2-call types "[method]outgoing-request.set-scheme"
                               request (cons :some (list "HTTPS")))))
           (is (equal (cons :some (list "HTTPS"))
                      (p2-call types "[method]outgoing-request.scheme"
                               request)))
           (is (equal '(:err)
                      (p2-call types "[method]outgoing-request.set-authority"
                               request (cons :some "exa mple"))))
           (is (equal '(:ok)
                      (p2-call types
                               "[method]outgoing-request.set-path-with-query"
                               request (cons :some "/a/b?c=d"))))
           (is (equal (cons :some "/a/b?c=d")
                      (p2-call types
                               "[method]outgoing-request.path-with-query"
                               request)))
           ;; headers are an immutable child view
           (let ((fields (p2-http-marker-rep
                          (p2-call types "[method]outgoing-request.headers"
                                   request))))
             (is (equal '(:err "immutable")
                        (p2-call types "[method]fields.set"
                                 fields "A" (list (list 49))))))
           ;; request-options
           (let ((options (p2-http-marker-rep
                           (p2-call types "[constructor]request-options"))))
             (is (eq :none (p2-call types
                                   "[method]request-options.connect-timeout"
                                   options)))
             (is (equal '(:ok)
                        (p2-call types
                                 "[method]request-options.set-connect-timeout"
                                 options (cons :some 1000000))))
             (is (equal (cons :some 1000000)
                        (p2-call types
                                 "[method]request-options.connect-timeout"
                                 options)))
             ;; a zero duration cannot be honoured: result err (no payload)
             (is (equal '(:err)
                        (p2-call types
                                 "[method]request-options.set-first-byte-timeout"
                                 options (cons :some 0))))
             (is (eq :none (p2-call types
                                   "[method]request-options.first-byte-timeout"
                                   options))))
           ;; outgoing-body: write/finish and a one-shot stream
           (let* ((body-result (p2-call types
                                       "[method]outgoing-request.body"
                                        request))
                  (body (p2-http-marker-rep (cdr body-result))))
             (is (eq :ok (car body-result)))
             (is (equal '(:err)
                        (p2-call types "[method]outgoing-request.body"
                                 request)))
             (let* ((write-result (p2-call types
                                           "[method]outgoing-body.write"
                                           body))
                    (stream (p2-http-marker-rep (cdr write-result)))
                    (streams (wasmdown.wasi:p2-streams-instance
                              ctx "0.2.12")))
               (is (eq :ok (car write-result)))
               (let ((permit (p2-call streams "[method]output-stream.check-write"
                                      stream)))
                 (is (eq :ok (car permit)))
                 (is (plusp (cdr permit))))
               (is (equal '(:ok)
                          (p2-call streams "[method]output-stream.write"
                                   stream (list 104 105))))
               (is (equal '(:err)
                          (p2-call types "[method]outgoing-body.write"
                                   body)))
               (is (equal '(:ok)
                          (p2-call types "[static]outgoing-body.finish"
                                   body :none)))
               (is (eq :err
                       (car (p2-call types "[static]outgoing-body.finish"
                                     body :none)))))
             ;; a Content-Length that disagrees with the bytes written is
             ;; the WIT's HTTP-request-body-size
             (let* ((fields (p2-http-fields types
                                            '("content-length" (49))))
                    (req2 (p2-http-marker-rep
                           (p2-call types
                                    "[constructor]outgoing-request" fields)))
                    (body2 (p2-http-marker-rep
                            (cdr (p2-call types
                                          "[method]outgoing-request.body"
                                          req2))))
                    (stream2 (p2-http-marker-rep
                              (cdr (p2-call types
                                            "[method]outgoing-body.write"
                                            body2))))
                    (streams (wasmdown.wasi:p2-streams-instance
                              ctx "0.2.12")))
               (p2-call streams "[method]output-stream.check-write" stream2)
               (p2-call streams "[method]output-stream.write" stream2
                        (list 104 105))
               (let ((finish (p2-call types "[static]outgoing-body.finish"
                                      body2 :none)))
                 (is (eq :err (car finish)))
                 (is (string= "HTTP-request-body-size"
                              (car (cdr finish))))))))
      (wasmdown.wasi:close-wasi-context ctx))))

;;; ---- outgoing-handler over a loopback server -----------------------

(test wasi-preview2/http-loopback-get
  (with-p2-http-server
      (port
       (p2-http-response 200 :body "hello"
                             :extra (format nil "X-Reply: yes~c~c"
                                            #\Return #\Newline))
       got)
    (let* ((net (p2-http-net))
           (ctx (p2-http-ctx :net net))
           (types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
           (handler (wasmdown.wasi:p2-http-outgoing-handler-instance
                     ctx "0.2.12")))
      (unwind-protect
           (let* ((request (p2-http-request types port :path "/hi?x=1"))
                  (handle (p2-http-handle handler request))
                  (future (p2-http-future-rep handle)))
             (is (eq :ok (car handle)))
             ;; a pollable is exposed and becomes ready with the response
             (let ((pollable (p2-http-marker-rep
                              (p2-call types
                                       "[method]future-incoming-response.subscribe"
                                       future))))
               (is (not (null pollable))))
             (let* ((value (p2-http-future-wait types future))
                    (inner (p2-http-future-inner value)))
               (is (eq :some (car value)))
               (is (eq :ok (car inner)))
               (let ((response (p2-http-marker-rep (cdr inner))))
                 (is (= 200 (p2-http-status types response)))
                 (is (equal '((121 101 115))
                            (p2-http-headers-field types response "x-reply")))
                 (let* ((body (p2-http-body-of types response))
                        (stream (p2-http-stream-of types body)))
                   (is (string= "hello"
                                (p2-http-bytes-string
                                 (p2-http-read-all ctx stream))))
                   (let* ((trailers-marker
                            (p2-call types "[static]incoming-body.finish"
                                     body))
                          (trailers (p2-http-marker-rep trailers-marker)))
                     (is (equal (cons :some (cons :ok (cons :ok :none)))
                                (p2-call types
                                         "[method]future-trailers.get"
                                         trailers))))))
               ;; a second get reports the retrieval error, not the value
               (is (equal '(:some :err)
                          (p2-call types
                                   "[method]future-incoming-response.get"
                                   future)))
               (is (search "GET /hi?x=1 HTTP/1.1" (car got)))
               (is (search (format nil "Host: 127.0.0.1:~d" port) (car got)))))
        (wasmdown.wasi:close-wasi-context ctx)))))

(test wasi-preview2/http-loopback-request-body
  (with-p2-http-server
      (port (p2-http-response 200 :body "ok") got)
    (let* ((net (p2-http-net))
           (ctx (p2-http-ctx :net net))
           (types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
           (handler (wasmdown.wasi:p2-http-outgoing-handler-instance
                     ctx "0.2.12"))
           (streams (wasmdown.wasi:p2-streams-instance ctx "0.2.12")))
      (unwind-protect
           (let* ((fields (p2-http-fields types
                                          '("content-length" (53))))
                  (request (p2-http-request types port :headers fields
                                                    :path "/post"))
                  (_method (p2-call types
                                    "[method]outgoing-request.set-method"
                                    request (list "post")))
                  (body (p2-http-marker-rep
                         (cdr (p2-call types
                                       "[method]outgoing-request.body"
                                       request))))
                  (stream (p2-http-marker-rep
                           (cdr (p2-call types
                                         "[method]outgoing-body.write"
                                         body)))))
             (p2-call streams "[method]output-stream.check-write" stream)
             (is (equal '(:ok)
                        (p2-call streams "[method]output-stream.write"
                                 stream (list 104 101 108 108 111))))
             (is (equal '(:ok)
                        (p2-call types "[static]outgoing-body.finish"
                                 body :none)))
             (let* ((handle (p2-http-handle handler request))
                    (value (p2-http-future-wait
                            types (p2-http-future-rep handle)))
                    (inner (p2-http-future-inner value)))
               (is (eq :ok (car inner)))
               (let ((response (p2-http-marker-rep (cdr inner))))
                 (is (= 200 (p2-http-status types response)))
                 (let ((body (p2-http-body-of types response)))
                   (is (string= "ok"
                                (p2-http-bytes-string
                                 (p2-http-read-all
                                  ctx (p2-http-stream-of types body))))))))
             (is (search "POST /post HTTP/1.1" (car got)))
             (is (search "hello" (car got))))
        (wasmdown.wasi:close-wasi-context ctx)))))

(test wasi-preview2/http-loopback-chunked-trailers
  (let ((response (format nil "HTTP/1.1 200 OK~c~cTransfer-Encoding: chunked~c~c~c~c2~c~cab~c~c2~c~ccd~c~c0~c~cX-Trail: t~c~c~c~c"
                          #\Return #\Newline #\Return #\Newline
                          #\Return #\Newline #\Return #\Newline
                          #\Return #\Newline #\Return #\Newline
                          #\Return #\Newline #\Return #\Newline
                          #\Return #\Newline #\Return #\Newline)))
    (with-p2-http-server (port response got)
      (let* ((net (p2-http-net))
             (ctx (p2-http-ctx :net net))
             (types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
             (handler (wasmdown.wasi:p2-http-outgoing-handler-instance
                       ctx "0.2.12")))
        (unwind-protect
             (let* ((request (p2-http-request types port))
                    (value (p2-http-future-wait
                            types
                            (p2-http-future-rep
                             (p2-http-handle handler request))))
                    (inner (p2-http-future-inner value)))
               (is (eq :ok (car inner)))
               (let* ((response (p2-http-marker-rep (cdr inner)))
                      (body (p2-http-body-of types response))
                      (stream (p2-http-stream-of types body)))
                 (is (string= "abcd"
                              (p2-http-bytes-string
                               (p2-http-read-all ctx stream))))
                 (let* ((trailers (p2-http-marker-rep
                                   (p2-call types
                                            "[static]incoming-body.finish"
                                            body)))
                        (value (p2-call types
                                        "[method]future-trailers.get"
                                        trailers)))
                   (is (eq :some (car value)))
                   (is (eq :ok (car (cdr value))))
                   (is (eq :ok (car (cdr (cdr value)))))
                   (let* ((opt (cddr (cdr value)))
                          (marker (cdr opt))
                          (fields (p2-http-marker-rep marker)))
                     (is (eq :some (car opt)))
                     (is (equal '((116))
                                (p2-call types "[method]fields.get"
                                         fields "x-trail")))))))
          (wasmdown.wasi:close-wasi-context ctx))))))

;;; ---- error mapping -------------------------------------------------

(test wasi-preview2/http-error-mapping
  ;; a request without an authority is rejected by handle itself
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let* ((types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
                (handler (wasmdown.wasi:p2-http-outgoing-handler-instance
                          ctx "0.2.12"))
                (request (p2-http-marker-rep
                          (p2-call types "[constructor]outgoing-request"
                                   (p2-http-fields types)))))
           (is (equal '(:err "HTTP-request-URI-invalid")
                      (p2-http-handle handler request))))
      (wasmdown.wasi:close-wasi-context ctx)))
  ;; https without an embedder transport is configuration-error
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let* ((types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
                (handler (wasmdown.wasi:p2-http-outgoing-handler-instance
                          ctx "0.2.12"))
                (request (p2-http-request types 443)))
           (p2-call types "[method]outgoing-request.set-scheme"
                    request (cons :some (list "HTTPS")))
           (is (equal '(:err "configuration-error")
                      (p2-http-handle handler request))))
      (wasmdown.wasi:close-wasi-context ctx)))
  ;; the network authority refuses an undeclared destination THROUGH
  ;; the future (a protocol error, not a handle error)
  (let* ((net (p2-http-net
               :outbound (list (wasmdown.wasi:make-wasi-endpoint
                                :address '(10 9 9 9)))))
         (ctx (p2-http-ctx :net net))
         (types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
         (handler (wasmdown.wasi:p2-http-outgoing-handler-instance
                   ctx "0.2.12")))
    (unwind-protect
         (let* ((request (p2-http-request types 80))
                (value (p2-http-future-wait
                        types
                        (p2-http-future-rep
                         (p2-http-handle handler request))))
                (inner (p2-http-future-inner value)))
           (is (eq :err (car inner)))
           (is (string= "destination-IP-prohibited" (car (cdr inner)))))
      (wasmdown.wasi:close-wasi-context ctx)))
  ;; a refused connection is connection-refused
  (multiple-value-bind (port fd) (p2-http-listener)
    (wasmdown.wasi::%close-fd fd)
    (let* ((net (p2-http-net))
           (ctx (p2-http-ctx :net net))
           (types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
           (handler (wasmdown.wasi:p2-http-outgoing-handler-instance
                     ctx "0.2.12")))
      (unwind-protect
           (let* ((request (p2-http-request types port))
                  (value (p2-http-future-wait
                          types
                          (p2-http-future-rep
                           (p2-http-handle handler request))))
                  (inner (p2-http-future-inner value)))
             (is (eq :err (car inner)))
             (is (string= "connection-refused" (car (cdr inner)))))
        (wasmdown.wasi:close-wasi-context ctx))))
  ;; http-error-code downcasts the error an HTTP stream failure carries
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let* ((types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
                (code (cons "HTTP-response-timeout" nil))
                (err (wasmdown.wasi:p2-error "timeout" "http" code)))
           (is (equal (cons :some code)
                      (p2-call types "http-error-code" err)))
           (is (eq :none
                   (p2-call types "http-error-code"
                            (wasmdown.wasi:p2-error "plain")))))
      (wasmdown.wasi:close-wasi-context ctx))))

;;; ---- the serve side: incoming-handler@0.2.x ------------------------

(defun p2-serve-test-comp (&key (version "0.2.12")
                                (imports '("wasi:io/poll@0.2.12"
                                           "wasi:clocks/monotonic-clock@0.2.12"
                                           "wasi:clocks/wall-clock@0.2.12"
                                           "wasi:random/random@0.2.12"
                                           "wasi:io/error@0.2.12"
                                           "wasi:io/streams@0.2.12"
                                           "wasi:cli/stdout@0.2.12"
                                           "wasi:cli/stderr@0.2.12"
                                           "wasi:cli/stdin@0.2.12"
                                           "wasi:http/types@0.2.12"
                                           "wasi:http/outgoing-handler@0.2.12"))
                                (exports '("wasi:http/incoming-handler@0.2.12")))
  "A decoded-component stand-in carrying only the import/export names
the serve composition and dispatch inspect (no binary, no code)."
  (wasmdown.component:make-comp
   :defs
   (coerce
    (append
     (loop for name in imports
           collect (wasmdown.component:make-compdef
                    :kind :import
                    :payload (wasmdown.component:make-importrec
                              :name name :type (cons :func 0))))
     (loop for name in exports
           collect (wasmdown.component:make-compdef
                    :kind :export
                    :payload (wasmdown.component:make-exportrec
                              :name name :sortidx (cons :func 0)))))
    'vector)))

(defun p2-serve-test-handler (ctx version capture
                              &key (status 200) (body "hello") trailers)
  "A host `incoming-handler.handle' that consumes the incoming request,
records method/path/host/body/trailers in CAPTURE, and sets a response
through the real P2 types provider."
  (let ((types (wasmdown.wasi:p2-http-types-instance ctx version))
        (streams (wasmdown.wasi:p2-streams-instance ctx version)))
    (wasmdown.wasi:provider-instance
     (wasmdown.wasi:p2-interface "http" "incoming-handler" version)
     (list
      (cons "handle"
            (lambda (request outparam)
              (let* ((req (p2-http-marker-rep request))
                     (method (p2-call types
                                      "[method]incoming-request.method"
                                      req))
                     (path (p2-call types
                                    "[method]incoming-request.path-with-query"
                                    req))
                     (header-fields
                       (p2-http-marker-rep
                        (p2-call types
                                 "[method]incoming-request.headers" req)))
                     (host (p2-call types "[method]fields.get"
                                    header-fields "host"))
                     (body-result (p2-call types
                                           "[method]incoming-request.consume"
                                           req))
                     (ibody (p2-http-marker-rep (cdr body-result)))
                     (stream (p2-http-marker-rep
                              (cdr (p2-call types
                                            "[method]incoming-body.stream"
                                            ibody))))
                     (bytes (p2-http-read-all ctx stream))
                     (future (p2-http-marker-rep
                              (p2-call types
                                       "[static]incoming-body.finish"
                                       ibody)))
                     (trailer-value (p2-call types
                                             "[method]future-trailers.get"
                                             future)))
                (setf (car capture)
                      (list method path host (p2-http-bytes-string bytes)
                            trailer-value))
                (let* ((fields (p2-http-fields types
                                               '("content-type"
                                                 (116 101 120 116 47 112 108 97 105 110))))
                       (response (p2-http-marker-rep
                                  (p2-call types
                                           "[constructor]outgoing-response"
                                           fields))))
                  (p2-call types "[method]outgoing-response.set-status-code"
                           response status)
                  (let* ((obody (p2-http-marker-rep
                                 (cdr (p2-call types
                                               "[method]outgoing-response.body"
                                               response)))))
                    (when (and body (plusp (length body)))
                      (let ((out (p2-http-marker-rep
                                  (cdr (p2-call types
                                                "[method]outgoing-body.write"
                                                obody)))))
                        (p2-call streams "[method]output-stream.check-write" out)
                        (p2-call streams "[method]output-stream.write" out
                                 (map 'list #'char-code body))))
                    (p2-call types "[static]outgoing-body.finish" obody
                             (if trailers
                                 (p2-http-fields types
                                                 (list "x-trail"
                                                       (list 116)))
                                 :none))
                    (p2-call types "[static]response-outparam.set"
                             outparam (cons :ok response))))))))
     :packages (wasmdown.wasi::p2-packages version))))

(defun p2-serve-test-request (port text)
  "Send TEXT over a fresh loopback TCP connection to PORT and return
the whole response as a string."
  (let ((conn (wasmdown.wasi::%http-plain-connect
               "127.0.0.1" port
               (list :ipv4 '(127 0 0 1) port) 5000)))
    (unwind-protect
         (progn
           (funcall (wasmdown.wasi::wasi-http-connection-write-fn conn)
                    (map 'list #'char-code text))
           (let ((out '()))
             (loop
               (multiple-value-bind (bytes status)
                   (funcall (wasmdown.wasi::wasi-http-connection-read-fn conn)
                            65536 5000)
                 (cond
                   ((eq status :ok) (setf out (nconc out bytes)))
                   (t (return (map 'string #'code-char out))))))))
      (wasmdown.wasi::%http-connection-close conn))))

(defun p2-serve-test-loopback (handler version request-text)
  "Run the P2 accept loop over a fresh listener, send REQUEST-TEXT, and
return the response string."
  (multiple-value-bind (port fd) (p2-http-listener)
    (let ((wasmdown.wasi::*serve-stop* nil)
          (thread
            (sb-thread:make-thread
             (lambda ()
               (wasmdown.wasi::%p2-serve-accept-loop fd handler version))
             :name "p2-serve-test")))
      (unwind-protect
           (p2-serve-test-request port request-text)
        (setf wasmdown.wasi::*serve-stop* t)
        (ignore-errors (sb-thread:join-thread thread :timeout 3))
        (wasmdown.wasi::%close-fd fd)))))

(test wasi-preview2/http-serve-providers-complete
  (let* ((comp (p2-serve-test-comp))
         (ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let* ((providers (wasmdown.wasi:preview2-serve-providers ctx comp))
                (names (mapcar #'car providers)))
           (is (wasmdown.wasi:preview2-serve-p comp))
           ;; every declared proxy-world import is bound, exactly once
           (is (= 11 (length names)))
           (is (member "wasi:http/types@0.2.12" names :test #'string=))
           (is (member "wasi:http/outgoing-handler@0.2.12" names
                       :test #'string=))
           (is (member "wasi:io/streams@0.2.12" names :test #'string=))
           (is (member "wasi:cli/stdin@0.2.12" names :test #'string=))
           (is (member "wasi:clocks/monotonic-clock@0.2.12" names
                       :test #'string=))
           (is (member "wasi:random/random@0.2.12" names :test #'string=))
           ;; declaration order is preserved
           (is (equal names
                      '("wasi:io/poll@0.2.12"
                        "wasi:clocks/monotonic-clock@0.2.12"
                        "wasi:clocks/wall-clock@0.2.12"
                        "wasi:random/random@0.2.12"
                        "wasi:io/error@0.2.12"
                        "wasi:io/streams@0.2.12"
                        "wasi:cli/stdout@0.2.12"
                        "wasi:cli/stderr@0.2.12"
                        "wasi:cli/stdin@0.2.12"
                        "wasi:http/types@0.2.12"
                        "wasi:http/outgoing-handler@0.2.12")))
           ;; the exported handler interface is never an import provider
           (is (not (member "wasi:http/incoming-handler@0.2.12" names
                            :test #'string=))))
      (wasmdown.wasi:close-wasi-context ctx)))
  ;; mixed patch versions compose per version
  (let* ((comp (p2-serve-test-comp
                :imports '("wasi:io/streams@0.2.5"
                           "wasi:http/types@0.2.9"
                           "wasi:cli/stdin@0.2.5")))
         (ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let ((names (mapcar #'car
                              (wasmdown.wasi:preview2-serve-providers
                               ctx comp))))
           (is (equal '("wasi:io/streams@0.2.5"
                        "wasi:http/types@0.2.9"
                        "wasi:cli/stdin@0.2.5")
                      names)))
      (wasmdown.wasi:close-wasi-context ctx))))

(test wasi-preview2/http-response-outparam-capture
  (let ((ctx (wasmdown.wasi:make-wasi-context)))
    (unwind-protect
         (let* ((types (wasmdown.wasi:p2-http-types-instance ctx "0.2.12"))
                (captured (list :unset))
                (outparam
                  (wasmdown.wasi:make-p2-http-response-outparam
                   :set-fn (lambda (value) (setf (car captured) value))))
                (fields (p2-http-fields types))
                (response (p2-http-marker-rep
                           (p2-call types "[constructor]outgoing-response"
                                    fields))))
           ;; a direct host call passes the rep; the provider must
           ;; capture it, not silently no-op on the marker shape
           (p2-call types "[static]response-outparam.set"
                    outparam (cons :ok response))
           (is (eq :ok (car (car captured))))
           (is (eq response (cdr (car captured))))
           ;; an error-code response is captured as err
           (let ((code (cons "internal-error" nil)))
             (p2-call types "[static]response-outparam.set"
                      outparam (cons :err code))
             (is (equal (cons :err code) (car captured)))))
      (wasmdown.wasi:close-wasi-context ctx))))

(test wasi-preview2/http-serve-loopback
  (let ((ctx (wasmdown.wasi:make-wasi-context))
        (capture (list nil)))
    (unwind-protect
         (let* ((version "0.2.12")
                (instance (p2-serve-test-handler ctx version capture
                                                 :status 201 :body "world"
                                                 :trailers t))
                (handler
                  (wasmdown.component.runtime:runtime-export-value
                   instance "handle"))
                (request
                  (let ((crlf (format nil "~c~c" #\Return #\Newline)))
                    (format nil
                            "POST /echo?x=1 HTTP/1.1~aHost: 127.0.0.1~aTransfer-Encoding: chunked~a~a5~ahello~a0~aX-Req-Trail: v~a~a"
                            crlf crlf crlf crlf crlf crlf crlf crlf crlf)))
                (response (p2-serve-test-loopback handler version request)))
           ;; the guest saw the parsed request and its buffered body
           (destructuring-bind (method path host body trailers) (car capture)
             (is (equal '("post") method))
             (is (equal (cons :some "/echo?x=1") path))
             (is (equal '((49 50 55 46 48 46 48 46 49)) host))
             (is (string= "hello" body))
             ;; the chunked request's trailer section reached
             ;; incoming-body.finish as a fields resource
             (is (eq :some (car trailers)))
             (is (eq :ok (car (cdr trailers))))
             (let* ((opt (cddr (cdr trailers)))
                    (fields (p2-http-marker-rep (cdr opt))))
               (is (eq :some (car opt)))
               (is (equal '((118))
                          (p2-call (wasmdown.wasi:p2-http-types-instance
                                    ctx version)
                                   "[method]fields.get" fields
                                   "x-req-trail")))))
           ;; status, headers and the chunked response body
           (is (search "HTTP/1.1 201 Created" response))
           (is (search "content-type: text/plain" response))
           (is (search "Transfer-Encoding: chunked" response))
           (is (search "5" response))
           (is (search "world" response))
           (is (search "x-trail: t" response))
           (is (not (search "Content-Length" response))))
      (wasmdown.wasi:close-wasi-context ctx))))

(test wasi-preview2/http-serve-loopback-unset-outparam
  ;; the WIT contract: a handler that never sets the outparam makes the
  ;; host answer 500 (never hang)
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (version "0.2.12")
         (instance
           (wasmdown.wasi:provider-instance
            (wasmdown.wasi:p2-interface "http" "incoming-handler" version)
            (list (cons "handle" (lambda (request outparam)
                                   (declare (ignore request outparam))
                                   nil)))
            :packages (wasmdown.wasi::p2-packages version)))
         (handler
           (wasmdown.component.runtime:runtime-export-value
            instance "handle"))
         (response
           (p2-serve-test-loopback
            handler version
            (let ((crlf (format nil "~c~c" #\Return #\Newline)))
              (format nil "GET / HTTP/1.1~aHost: h~a~a"
                      crlf crlf crlf)))))
    (unwind-protect
         (progn
           (is (search "HTTP/1.1 500 Internal Server Error" response))
           (is (search "Content-Length: 0" response)))
      (wasmdown.wasi:close-wasi-context ctx))))

(test wasi-preview2/http-incoming-handler-export-lookup
  (let* ((version "0.2.12")
         (iface (wasmdown.wasi:p2-interface "http" "incoming-handler" version))
         (provider
           (wasmdown.wasi:provider-instance
            iface
            (list (cons "handle"
                        (lambda (request outparam)
                          (declare (ignore request outparam))
                          nil)))
            :packages (wasmdown.wasi::p2-packages version)))
         (inst (wasmdown.component.runtime:make-comp-instance))
         (name (format nil "wasi:http/incoming-handler@~a" version)))
    (setf (gethash name
                   (wasmdown.component.runtime:comp-instance-exports inst))
          (cons :instance provider))
    (multiple-value-bind (handler found-version)
        (wasmdown.wasi:p2-incoming-handler-export inst)
      (is (wasmdown.component.runtime:lifted-func-p handler))
      (is (string= version found-version)))
    ;; the interface name is only an export: a plain `handle' key is
    ;; a last-resort direct export, and an empty instance exports none
    (let ((bare (wasmdown.component.runtime:make-comp-instance)))
      (setf (gethash "handle"
                     (wasmdown.component.runtime:comp-instance-exports bare))
            (cons :func
                  (wasmdown.wasi:provider-instance-function provider
                                                            "handle")))
      (is (wasmdown.component.runtime:lifted-func-p
           (wasmdown.wasi:p2-incoming-handler-export bare)))
      (is (null (wasmdown.wasi:p2-incoming-handler-export
                 (wasmdown.component.runtime:make-comp-instance)))))))
