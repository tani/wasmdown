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
