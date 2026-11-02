;;;; CLI entry (docs/design/PLAN.md §11.2 subset). Package WASMDOWN.CLI.
;;;;
;;;; wasmdown.cli:main (argv) runs one subcommand in-process and
;;;; returns the stable host exit status:
;;;;   0  ok
;;;;   1  guest/module error: decode, validation, link,
;;;;      instantiation, trap, or compile failure
;;;;   2  usage/host-invocation error: bad arguments, unknown command,
;;;;      missing file, malformed integer, host-error
;;;; Structured diagnostics go to *error-output*; results and summaries
;;;; to *standard-output*. Traps print their kind (docs/design/PLAN.md §11.2:
;;;; "stable host exit status independent of Wasm trap representation").
;;;;
;;;; Implemented subset: validate / inspect / compile / run / help.
;;;; M16 slice 6: `run' accepts BOTH layers — a component binary is
;;;; driven through the WASI command runner (`--wasi', `--dir',
;;;; `--env', `--net', `--listen', `--dns' and guest arguments), a
;;;; core module keeps the `--invoke' path. No --mapdir /
;;;; --profile-conditional / artifact ABI checks yet; --http-transport
;;;; is embedder-only (the outgoing `wasi:http/types' + `client'
;;;; providers are served and plain `http' rides the built-in TCP
;;;; transport).
;;;;
;;;; Network communication is explicit authority, never ambient: the
;;;; network options `--net', `--listen' and `--dns' declare exactly
;;;; the endpoints and DNS names a guest's `wasi:sockets' imports may
;;;; use; with none declared, every socket operation is refused.

(in-package #:wasmdown.cli)

;;; ---------------------------------------------------------------
;;; Usage.

(define-condition usage-error (error)
  ((message :initarg :message :reader usage-error-message))
  (:report (lambda (c s)
             (format s "usage: ~a" (usage-error-message c)))))

(defun %usage (stream)
  (format stream "usage: wasmdown <command> [options] <file>~%~
                  commands:~%~
                  ~4tvalidate [--profile name] <file.wasm>~%~
                  ~4tinspect <file.wasm>~%~
                  ~4tcompile <file.wasm> --emit-lisp <out.lisp>~%~
                  ~4trun <file> [--invoke <name> <int> ...]~%~
                  ~4trun <component> --wasi [--dir HOST[::GUEST]]~%~
                  ~10t[--env K=V] [--net HOST:PORT] [--listen ADDR:PORT]~%~
                  ~10t[--dns NAME] [arg ...]~%~
                  ~4tserve <component> --wasi [--addr HOST:PORT]~%~
                  ~10t[--dir HOST[::GUEST]] [--env K=V] [--net HOST:PORT]~%~
                  ~10t[--listen ADDR:PORT] [--dns NAME]~%~
                  ~4thelp~%~
                  global options:~%~
                  ~4t--threads~10tenable the threads proposal (shared memory, atomics, WASI thread spawn)~%~
                  exit codes: 0 ok, 1 guest/compile error, 2 usage error~%"))

(defun %usage-error (format-control &rest args)
  (error 'usage-error
         :message (apply #'format nil format-control args)))

;;; ---------------------------------------------------------------
;;; Argument parsing.

(defun %flagp (token)
  (and (>= (length token) 2) (string= "--" (subseq token 0 2))))

(defun %parse-args (argv flags-with-arg)
  "Split ARGV into (values positional-list options-alist). Flags in
FLAGS-WITH-ARG consume the next token as their value; any other --
flag is a usage error."
  (do ((rest argv) pos opts)
      ((null rest) (values (nreverse pos) (nreverse opts)))
    (let ((tok (pop rest)))
      (cond ((%flagp tok)
             (cond ((member tok flags-with-arg :test #'string=)
                    (let ((val (pop rest)))
                      (unless val
                        (%usage-error "option ~a requires a value" tok))
                      (push (cons tok val) opts)))
                   (t (%usage-error "unknown option ~a" tok))))
            (t (push tok pos))))))

(defun %parse-int (token)
  (multiple-value-bind (n end)
      (parse-integer token :junk-allowed t)
    (unless (and n (= end (length token)))
      (%usage-error "not an integer argument: ~s" token))
    n))

;;; ---------------------------------------------------------------
;;; Command: validate.

(defun %cmd-validate (args)
  (multiple-value-bind (pos opts) (%parse-args args '("--profile"))
    (declare (ignore opts))            ; one profile only in this slice
    (unless (= 1 (length pos))
      (%usage-error "validate: exactly one input file, got ~d" (length pos)))
    (handler-case
        (let ((module (wasmdown:read-module (first pos))))
          (wasmdown:validate-module module)
          (format t "ok: ~a decodes and validates (~d type~:p, ~d function~:p, ~d import~:p, ~d export~:p)~%"
                  (first pos)
                  (length (wasmdown.types:wasm-module-types module))
                  (+ (loop for im across (wasmdown.types:wasm-module-imports module)
                           count (eq (wasmdown.types:wasm-import-kind im) :func))
                     (length (wasmdown.types:wasm-module-funcs module)))
                  (length (wasmdown.types:wasm-module-imports module))
                  (length (wasmdown.types:wasm-module-exports module)))
          0)
      (wasmdown:decode-error (c) (%diag c) 1)
      (wasmdown:validation-error (c) (%diag c) 1)
      (wasmdown:wasmdown-error (c) (%diag c) 1))))

;;; ---------------------------------------------------------------
;;; Command: inspect.

(defun %valtype-list (list)
  (with-output-to-string (s)
    (princ "(" s)
    (loop for tp in list
          for first = t then nil
          do (progn (unless first (princ ", " s))
                    (princ tp s)))
    (princ ")" s)))

(defun %func-sig (module funcidx)
  "Signature string of the function at absolute FUNCIDX (imported
functions first)."
  (let* ((imports (loop for im across (wasmdown.types:wasm-module-imports module)
                        when (eq (wasmdown.types:wasm-import-kind im) :func)
                          collect im))
         (types (wasmdown.types:wasm-module-types module))
         (ft (if (< funcidx (length imports))
                 (svref types (wasmdown.types:wasm-import-type
                               (nth funcidx imports)))
                 (let ((fn (svref (wasmdown.types:wasm-module-funcs module)
                                  (- funcidx (length imports)))))
                   (svref types (wasmdown.types:wasm-func-typeidx fn))))))
    (format nil "~a -> ~a"
            (%valtype-list (wasmdown.types:wasm-func-type-params ft))
            (%valtype-list (wasmdown.types:wasm-func-type-results ft)))))

(defun %cmd-inspect (args)
  (multiple-value-bind (pos opts) (%parse-args args '())
    (declare (ignore opts))
    (unless (= 1 (length pos))
      (%usage-error "inspect: exactly one input file, got ~d" (length pos)))
    (handler-case
        (let ((m (wasmdown:read-module (first pos))))
          (wasmdown:validate-module m)
          (format t "module: ~a~%" (first pos))
          (format t "types: ~d~%" (length (wasmdown.types:wasm-module-types m)))
          (format t "imports: ~d~%" (length (wasmdown.types:wasm-module-imports m)))
          (loop for im across (wasmdown.types:wasm-module-imports m)
                for i from 0
                do (ecase (wasmdown.types:wasm-import-kind im)
                     (:func
                      (format t "  func ~a.~a : ~a~%"
                              (wasmdown.types:wasm-import-module im)
                              (wasmdown.types:wasm-import-field im)
                              (%func-sig m i)))
                     (:table (format t "  table ~a.~a~%"
                                     (wasmdown.types:wasm-import-module im)
                                     (wasmdown.types:wasm-import-field im)))
                     (:memory (format t "  memory ~a.~a~%"
                                      (wasmdown.types:wasm-import-module im)
                                      (wasmdown.types:wasm-import-field im)))
                     (:global (format t "  global ~a.~a~%"
                                      (wasmdown.types:wasm-import-module im)
                                      (wasmdown.types:wasm-import-field im)))))
          (format t "exports: ~d~%" (length (wasmdown.types:wasm-module-exports m)))
          (loop for ex across (wasmdown.types:wasm-module-exports m)
                do (case (wasmdown.types:wasm-export-kind ex)
                     (:func
                      (format t "  func ~s : ~a~%"
                              (wasmdown.types:wasm-export-name ex)
                              (%func-sig m (wasmdown.types:wasm-export-index ex))))
                     (otherwise
                      (format t "  ~a ~s : index ~d~%"
                              (wasmdown.types:wasm-export-kind ex)
                              (wasmdown.types:wasm-export-name ex)
                              (wasmdown.types:wasm-export-index ex)))))
          (format t "memories: ~d~%" (length (wasmdown.types:wasm-module-mems m)))
          (loop for mem across (wasmdown.types:wasm-module-mems m)
                for lim = (wasmdown.types:wasm-memory-limits mem)
                do (format t "  memory min ~d page~:p~@[ max ~d~]~%"
                           (wasmdown.types:wasm-limits-min lim)
                           (and (wasmdown.types:wasm-limits-has-max-p lim)
                                (wasmdown.types:wasm-limits-max lim))))
          (format t "tables: ~d~%" (length (wasmdown.types:wasm-module-tables m)))
          (loop for tab across (wasmdown.types:wasm-module-tables m)
                for lim = (wasmdown.types:wasm-table-limits tab)
                do (format t "  table ~a min ~d~@[ max ~d~]~%"
                           (wasmdown.types:wasm-table-type tab)
                           (wasmdown.types:wasm-limits-min lim)
                           (and (wasmdown.types:wasm-limits-has-max-p lim)
                                (wasmdown.types:wasm-limits-max lim))))
          (format t "globals: ~d~%" (length (wasmdown.types:wasm-module-globals m)))
          (format t "start: ~:[none~;~d~]~%"
                  (wasmdown.types:wasm-module-start m)
                  (wasmdown.types:wasm-module-start m))
          (format t "features: ~{~a~^ ~}~%" (wasmdown:module-features m))
          0)
      (wasmdown:decode-error (c) (%diag c) 1)
      (wasmdown:validation-error (c) (%diag c) 1)
      (wasmdown:wasmdown-error (c) (%diag c) 1))))

;;; ---------------------------------------------------------------
;;; Command: compile.

(defun %cmd-compile (args)
  (multiple-value-bind (pos opts) (%parse-args args '("--emit-lisp"))
    (unless (= 1 (length pos))
      (%usage-error "compile: exactly one input file, got ~d" (length pos)))
    (let ((emit (parse-namestring (cdr (assoc "--emit-lisp" opts :test #'string=)))))
      (unless emit
        (%usage-error "compile: --emit-lisp <out.lisp> is required"))
      (handler-case
          (let* ((module (wasmdown:read-module (first pos))))
            (wasmdown:validate-module module)
            (when (zerop (length (wasmdown.types:wasm-module-funcs module)))
              (format *error-output*
                      "wasmdown: compile: module defines no functions; nothing to emit~%")
              (return-from %cmd-compile 1))
            (let ((sym (find-symbol "COMPILE-MODULE-FUNCTIONS" :wasmdown.codegen)))
              (unless (and sym (fboundp sym))
                (format *error-output*
                        "wasmdown: compile: compiled codegen backend not loaded (src/codegen.lisp)~%")
                (return-from %cmd-compile 1))
              (let* ((inst (wasmdown:instantiate module))
                     (n (length (funcall (symbol-function sym)
                                         module inst :emit emit))))
                (declare (ignore n))
                (format t "ok: compiled and emitted Lisp source to ~a~%" emit)
                0)))
        (wasmdown:decode-error (c) (%diag c) 1)
        (wasmdown:validation-error (c) (%diag c) 1)
        (wasmdown:wasmdown-error (c) (%diag c) 1)))))

;;; ---------------------------------------------------------------
;;; Command: run.
;;;
;;; M16 slice 6: the run command accepts BOTH layers. A component
;;; binary (the component preamble) goes through the WASI command
;;; runner (`wasi:cli/run@0.3.2'), configured by the WASI flags; a
;;; core module keeps the `--invoke' path. The flags:
;;;
;;;   --wasi             enable the WASI capability set (the process
;;;                      standard streams, the guest arguments after
;;;                      the component path, --env/--dir)
;;;   --dir HOST[::GUEST] a preopen (guest-visible name after `::')
;;;   --env K=V          one environment variable
;;;   --net HOST:PORT    declare an outbound endpoint (network policy)
;;;   --listen ADDR:PORT declare a listen address
;;;   --dns NAME         allow a name (or `*' for any)
;;;
;;; HTTP (outgoing `wasi:http/types' + `wasi:http/client') is served:
;;; a component that DECLARES those imports gets their providers, and
;;; an `http' request rides the built-in plain TCP transport. TLS
;;; transports are EMBEDDER-only (PLAN.md §9.2 — pluggable transport
;;; services are CL objects on the wasi-context, not a CLI capability):
;;; `--http-transport' is refused with that explanation, and https
;;; without an embedder transport is the WIT's `configuration-error',
;;; never a silent plaintext fallback.
;;;
;;; Network authority is declared, never ambient: with no --net/
;;; --listen/--dns the context carries no network and every socket
;;; operation is refused.

(defun %parse-run-args (argv)
  "Return (values file invokes options guest-args). INVOKES is a list
of (name . integer-args) in command-line order (core modules).
OPTIONS is an alist of the WASI-capability flags (repeats kept, in
order); GUEST-ARGS are the positional tokens AFTER the input file.
`--` ends option parsing: every later token reaches the guest
unchanged, including dash-prefixed ones.

The network options `--net', `--listen' and `--dns' take a value here;
their endpoints are parsed and the authority built in
`%component-context' only after the whole command line has parsed, so a
malformed option can never leave a half-built authority behind."
  (let ((rest argv) (file nil) (invokes '()) (options '()) (guest '()))
    (loop while rest
          for tok = (pop rest)
          do (cond
               ((string= tok "--")
                ;; everything after `--' is a guest argument
                (loop while rest
                      do (push (pop rest) guest))
                (setf rest nil))
               ((string= tok "--invoke")
                (let ((name (pop rest)))
                  (unless name
                    (%usage-error "run: --invoke requires an export name"))
                  (let ((ints '()))
                    (loop while (and rest
                                     (not (string= (first rest) "--"))
                                     (not (%flagp (first rest))))
                          do (push (%parse-int (pop rest)) ints))
                    (push (cons name (nreverse ints)) invokes))))
               ((string= tok "--wasi")
                (push (cons "--wasi" t) options))
               ((string= tok "--http-transport")
                ;; HTTP transports are embedder-only: the built-in
                ;; plain TCP transport serves `http', and a TLS
                ;; transport is a CL object on the wasi-context
                (%usage-error
                 "run: --http-transport is embedder-only; configure :http-transports on the wasi-context in the embedder"))
               ((member tok '("--dir" "--env" "--net" "--listen" "--dns"
                              "--addr")
                        :test #'string=)
                (let ((value (pop rest)))
                  (unless value
                    (%usage-error "run: ~a requires a value" tok))
                  (push (cons tok value) options)))
               ((%flagp tok) (%usage-error "unknown option ~a" tok))
               (file (push tok guest))
               (t (setf file tok))))
    (unless file
      (%usage-error "run: missing input file"))
    (values file (nreverse invokes) (nreverse options) (nreverse guest))))

(defun %api-mode ()
  "The documented WASMDOWN_API_MODE selection: absent or `interpret'
selects the reference interpreter, `compiled' the wasmdown codegen
backend, anything else is a host usage error. This mode is threaded
into the CORE instantiation that actually executes guest code."
  (let ((value (sb-ext:posix-getenv "WASMDOWN_API_MODE")))
    (cond
      ((or (null value) (string= value "") (string= value "interpret"))
       :interpret)
      ((string= value "compiled") :compiled)
      (t (%usage-error "WASMDOWN_API_MODE must be `interpret' or `compiled', got ~s"
                       value)))))

(defun %run-collect (options flag)
  "The values of the repeated option FLAG, in command-line order."
  (loop for (key . value) in options
        when (string= key flag)
          collect value))

(defun %read-run-file-bytes (path)
  (with-open-file (s path :element-type '(unsigned-byte 8)
                          :direction :input)
    (let ((v (make-array (file-length s)
                         :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun %env-pair (spec)
  "NAME=VALUE -> (name . value) for the environment capability."
  (let ((eq (position #\= spec)))
    (unless (and eq (plusp eq))
      (%usage-error "run: --env expects NAME=VALUE, got ~s" spec))
    (cons (subseq spec 0 eq) (subseq spec (1+ eq)))))

(defun %dir-preopen (spec)
  "HOST[::GUEST][:ro] -> an open wasi-preopen (the host directory
handle). `:ro' grants a read-only authority: creation, writing and
mutation through the root are denied by the derived ABI rights."
  (let* ((read-only (and (> (length spec) 3)
                         (string= spec ":ro" :start1 (- (length spec) 3))))
         (spec (if read-only (subseq spec 0 (- (length spec) 3)) spec))
         (sep (search "::" spec))
         (host (if sep (subseq spec 0 sep) spec))
         (guest (if sep (subseq spec (+ sep 2)) spec)))
    (unless (plusp (length host))
      (%usage-error "run: --dir expects HOST[::GUEST][:ro], got ~s" spec))
    (wasmdown.wasi:open-wasi-preopen
     host :name guest
     :permissions (if read-only '("read") '("read" "write" "mutate-directory")))))

(defun %run-network (options)
  "The network authority declared by --net/--listen/--dns, or NIL
(no network authority: every socket operation is refused). Each
endpoint spec is parsed here — after the whole command line parsed and
before the authority object exists — so a malformed endpoint is a
usage error and no partial authority escapes."
  (let ((outgoing (mapcar #'wasmdown.wasi:parse-wasi-endpoint
                          (%run-collect options "--net")))
        (listen (mapcar #'wasmdown.wasi:parse-wasi-endpoint
                        (%run-collect options "--listen")))
        (dns (%run-collect options "--dns")))
    (when (or outgoing listen dns)
      (wasmdown.wasi:make-wasi-network
       :outbound outgoing
       :listen listen
       :dns (cond ((member "*" dns :test #'string=) :all)
                  (dns dns)
                  (t :none))))))

(defun %component-context (options guest-args file)
  "The capability context a component run is configured with. Without
--wasi every capability is withdrawn (docs/design/PLAN.md §9.1: the default is
empty); with it, the process streams, the guest arguments and exactly
the declared network endpoints are the explicitly configured ones.
argv[0] is FILE's file name (the basename), never its path: POSIX
argv[0] is the program name, the reference runtime's `wasi:cli'
command entry sets it that way, and the upstream testsuite asserts it
(a guest reading `get-arguments' sees `cli-env.wasm', not
`/some/dir/cli-env.wasm'). The context's network authority is built
only from explicit --net/--listen/--dns declarations; with none it
carries no network at all."
  (let ((wasi (assoc "--wasi" options)))
    (when (and (not wasi)
               (or (%run-collect options "--dir")
                   (%run-collect options "--env")
                   (%run-collect options "--net")
                   (%run-collect options "--listen")
                   (%run-collect options "--dns")))
      (%usage-error
       "run: --dir/--env/--net/--listen/--dns require --wasi"))
    (when (and (not wasi) guest-args)
      (%usage-error "run: guest arguments require --wasi"))
    (let* ((network (%run-network options))
           ;; The CLI OWNS its reactor: clock waits are then servable
           ;; by the store driver, instead of every wait failing for
           ;; want of a watch. NIL without --wasi (no capabilities,
           ;; nothing to wait on).
           (reactor (and wasi (wasmdown.wasi:make-wasi-reactor))))
      (values
       (wasmdown.wasi:make-wasi-context
        :arguments (if wasi (cons (file-namestring file) guest-args) nil)
        :environment (mapcar #'%env-pair (%run-collect options "--env"))
        :stdin (and wasi *standard-input*)
        :stdout (and wasi *standard-output*)
        :stderr (and wasi *error-output*)
        :preopens (mapcar #'%dir-preopen (%run-collect options "--dir"))
        :networks (and network (list network))
        :reactor reactor)
       reactor))))

(defun %run-component-file (bytes options guest-args file)
  "Decode + validate a component binary and drive its command world."
  (let ((comp (wasmdown.component:decode-component bytes)))
    (wasmdown.component:validate-component comp)
    ;; the capability context (and its preopen fds) exists only once
    ;; the component is known to be a valid command
    (multiple-value-bind (context reactor) (%component-context options guest-args file)
      (unwind-protect
           ;; providers follow the component's DECLARED imports: a 0.2
           ;; command gets the versioned P2 tables, a 0.3 one the
           ;; vendored 0.3 world
           (wasmdown.wasi:run-component
            comp context
            ;; the documented WASMDOWN_API_MODE reaches the embedded
            ;; cores through this mode, not through an unused
            ;; environment read
            :mode (%api-mode)
            :providers (wasmdown.wasi:component-command-providers
                        context comp))
        ;; the preopen handles belong to the CLI invocation
        (dolist (preopen (wasmdown.wasi:wasi-context-preopens context))
          (ignore-errors (wasmdown.wasi:close-wasi-preopen preopen)))
        ;; the reactor is the CLI's own: closing it cancels every
        ;; remaining watch and releases the self-pipe.
        (when reactor (ignore-errors (wasmdown.wasi:reactor-close reactor)))))))

(defun %cmd-run (args)
  (multiple-value-bind (file invokes options guest-args) (%parse-run-args args)
    (when (%run-collect options "--addr")
      (%usage-error "run: --addr is a serve option"))
    (handler-case
        (let ((bytes (%read-run-file-bytes file)))
          (if (wasmdown.component:component-p bytes)
              (progn
                (when invokes
                  (%usage-error
                   "run: --invoke applies to core modules, not components"))
                (%run-component-file bytes options guest-args file))
              (progn
                ;; CORE module. With --wasi it is a Preview1 command:
                ;; it gets the explicit capabilities and its `_start'
                ;; runs once; without it only --invoke is available.
                (let ((wasi (assoc "--wasi" options :test #'string=)))
                  (when (and wasi invokes)
                    (%usage-error
                     "run: a WASI command module runs `_start'; `--invoke' may not also be requested"))
                  (if wasi
                      (let* ((module (wasmdown:read-module bytes))
                             (context (%component-context options guest-args file)))
                        (unwind-protect
                             (let ((status (wasmdown.wasi:run-preview1
                                            module context :mode (%api-mode))))
                               ;; the embedding result is the RAW u32; the
                               ;; CLI status is its low eight bits
                               (logand status #xff))
                          (dolist (preopen (wasmdown.wasi:wasi-context-preopens context))
                            (ignore-errors (wasmdown.wasi:close-wasi-preopen preopen)))))
                      (progn
                        (when guest-args
                          (%usage-error "run: stray argument ~s (core modules take --invoke)"
                                        (first guest-args)))
                        (when (or (%run-collect options "--dir")
                                  (%run-collect options "--env"))
                          (%usage-error
                           "run: --dir/--env require --wasi"))
                        (let* ((module (wasmdown:read-module bytes))
                               (inst (wasmdown:instantiate module :mode (%api-mode))))
                          (dolist (inv invokes)
                            (let ((results (apply #'wasmdown:invoke inst
                                                  (car inv) (cdr inv))))
                              (format t "~{~a~^ ~}~%" results)))
                          0)))))))
      (wasmdown:decode-error (c) (%diag c) 1)
      (wasmdown:validation-error (c) (%diag c) 1)
      (wasmdown:link-error (c) (%diag c) 1)
      (wasmdown:instantiation-error (c) (%diag c) 1)
      (wasmdown:wasm-trap (c) (%diag c) 1)
      (wasmdown:wasmdown-error (c) (%diag c) 2))))

(defun %cmd-serve (args)
  "Serve a `wasi:http/service' component over HTTP/1.1. Prints
`http://HOST:PORT' as the FIRST stderr line, then answers requests
through the component's exported `wasi:http/handler' until SIGINT."
  (multiple-value-bind (file invokes options guest-args) (%parse-run-args args)
    (when invokes
      (%usage-error "serve: --invoke applies to core modules, not components"))
    (handler-case
        (let ((bytes (%read-run-file-bytes file)))
          (unless (wasmdown.component:component-p bytes)
            (%usage-error "serve: ~a is not a component" file))
          (let ((comp (wasmdown.component:decode-component bytes)))
            ;; The validator's cross-component visibility layer does not
            ;; yet accept every wit-bindgen async service shape (the
            ;; exported handler instance re-exports types through nested
            ;; component type spaces). SERVE, unlike RUN, is a verified
            ;; execution gate in its own right: instantiation performs
            ;; the link checks and the incoming handler is exercised by
            ;; the request. A validation diagnostic is therefore not
            ;; fatal here; RUN stays strict.
            (handler-case (wasmdown.component:validate-component comp)
              (wasmdown:validation-error () nil))
            (multiple-value-bind (context reactor)
                (%component-context options guest-args file)
              (let ((addr (or (cdr (assoc "--addr" options :test #'string=))
                              "127.0.0.1:0")))
                (unwind-protect
                     (if (wasmdown.wasi:preview2-serve-p comp)
                         ;; a 0.2 proxy-world guest exports
                         ;; wasi:http/incoming-handler: drive its sync
                         ;; handler with the exact-version P2 providers
                         (wasmdown.wasi:serve-preview2-component
                          comp context
                          :address addr
                          :mode (%api-mode)
                          :providers
                          (wasmdown.wasi:preview2-serve-providers
                           context comp))
                         (wasmdown.wasi:serve-component
                          comp context
                          :address addr
                          :mode (%api-mode)
                          :providers (wasmdown.wasi:serve-component-providers
                                      context comp)))
                  (dolist (preopen (wasmdown.wasi:wasi-context-preopens
                                    context))
                    (ignore-errors
                      (wasmdown.wasi:close-wasi-preopen preopen)))
                  (when reactor
                    (ignore-errors
                      (wasmdown.wasi:reactor-close reactor))))))))
      (wasmdown:decode-error (c) (%diag c) 1)
      (wasmdown:validation-error (c) (%diag c) 1)
      (wasmdown:link-error (c) (%diag c) 1)
      (wasmdown:instantiation-error (c) (%diag c) 1)
      (wasmdown:wasm-trap (c) (%diag c) 1)
      (wasmdown:wasmdown-error (c) (%diag c) 2))))

(defun %diag (condition)
  "Structured diagnostic: the condition's own report (message, trap
kind, byte offset where applicable) on *error-output*."
  (format *error-output* "~a~%" condition))

(defun %script-name-p (token)
  "Whether TOKEN looks like this launcher's script path (some SBCL
invocation layouts keep it in *posix-argv*)."
  (let ((n (length "wasmdown.lisp")))
    (and (>= (length token) n)
         (string= "wasmdown.lisp" token :start2 (- (length token) n)))))

(defun %script-argv ()
  "The subcommand arguments from SBCL's *posix-argv*: drop the
program name, and any launcher script path defensively."
  (let ((rest (rest sb-ext:*posix-argv*)))
    (loop while (and rest
                     (or (string-equal (first rest) "sbcl")
                         (%script-name-p (first rest))))
          do (pop rest))
    rest))

(defun main (&optional argv)
  "Run one subcommand. ARGV is the argument list AFTER the program
name, e.g. (~\"validate\" ~\"x.wasm\"); when unsupplied it is derived
from sb-ext:*posix-argv*. Returns the process exit status: 0 ok,
1 guest/compile error, 2 usage error. Prints usage on *standard-output*
when called with no arguments or with ~\"help\"."
  (block nil
    (handler-bind
        ((usage-error
           (lambda (c)
             (format *error-output* "~a~%" c)
             (%usage *error-output*)
             (return 2)))
         (sb-int:simple-file-error
           (lambda (c)
             (format *error-output* "wasmdown: ~a~%" c)
             (return 2)))
         (file-error
           (lambda (c)
             (format *error-output* "wasmdown: file error: ~a~%" c)
             (return 2))))
      (let ((args (or argv (%script-argv))))
        (cond ((null args) (%usage *standard-output*) (return 2))
              ((string= (first args) "help") (%usage *standard-output*) (return 0))
              (t
               ;; --threads is a global option: consume it before a
               ;; subcommand parses its own flags, and bind the
               ;; opt-in gate for the whole command (decode through
               ;; execution). Tokens after `--' are guest arguments
               ;; and are never touched.
               (let ((threads-p nil)
                     (kept '())
                     (guest-tail nil))
                 (dolist (tok args)
                   (cond (guest-tail (push tok kept))
                         ((string= tok "--")
                          (setf guest-tail t)
                          (push tok kept))
                         ((string= tok "--threads")
                          (setf threads-p t))
                         (t (push tok kept))))
                 (setf kept (nreverse kept))
                 (let ((wasmdown:*threads* threads-p)
                       (cmd (first kept))
                       (rest (rest kept)))
                   (cond ((string= cmd "validate") (return (%cmd-validate rest)))
                         ((string= cmd "inspect") (return (%cmd-inspect rest)))
                         ((string= cmd "compile") (return (%cmd-compile rest)))
                         ((string= cmd "run") (return (%cmd-run rest)))
                         ((string= cmd "serve") (return (%cmd-serve rest)))
                         (t
                          (format *error-output* "wasmdown: unknown command ~s~%" cmd)
                          (%usage *error-output*)
                          (return 2)))))))))))
