;;;; CLI entry (PLAN.md §11.2 subset). Package WASMDOWN.CLI.
;;;;
;;;; wasmdown.cli:main (argv) runs one subcommand in-process and
;;;; returns the stable host exit status:
;;;;   0  ok
;;;;   1  guest/module error: decode, validation, link,
;;;;      instantiation, trap, or compile failure
;;;;   2  usage/host-invocation error: bad arguments, unknown command,
;;;;      missing file, malformed integer, host-error
;;;; Structured diagnostics go to *error-output*; results and summaries
;;;; to *standard-output*. Traps print their kind (PLAN.md §11.2:
;;;; "stable host exit status independent of Wasm trap representation").
;;;;
;;;; Implemented subset: validate / inspect / compile / run / help.
;;;; M16 slice 6: `run' accepts BOTH layers — a component binary is
;;;; driven through the WASI command runner (`--wasi', `--dir',
;;;; `--env', `--net', `--listen', `--dns' and guest arguments), a
;;;; core module keeps the `--invoke' path. No --mapdir /
;;;; --profile-conditional / artifact ABI checks yet.

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
                  ~4thelp~%~
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
                        "wasmdown: compile: baseline codegen backend not loaded (src/codegen.lisp)~%")
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
;;;                      the component path, --env/--dir/--net/...)
;;;   --dir HOST[::GUEST] a preopen (guest-visible name after `::')
;;;   --env K=V          one environment variable
;;;   --net HOST:PORT    declare an outbound endpoint (network policy)
;;;   --listen ADDR:PORT declare a listen address
;;;   --dns NAME         allow a name (or `*' for any)
;;;
;;; HTTP is not supported in this runtime: there is no `wasi:http'
;;; provider and no transport option (the `wasi:http' surface was
;;; removed at user direction in M17).

(defun %parse-run-args (argv)
  "Return (values file invokes options guest-args). INVOKES is a list
of (name . integer-args) in command-line order (core modules).
OPTIONS is an alist of the WASI-capability flags (repeats kept, in
order); GUEST-ARGS are the positional tokens AFTER the input file.
`--` ends option parsing: every later token reaches the guest
unchanged, including dash-prefixed ones."
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
               ((member tok '("--dir" "--env" "--net" "--listen" "--dns")
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
selects the reference interpreter, `baseline' the wasmdown codegen
backend, anything else is a host usage error. This mode is threaded
into the CORE instantiation that actually executes guest code."
  (let ((value (sb-ext:posix-getenv "WASMDOWN_API_MODE")))
    (cond
      ((or (null value) (string= value "") (string= value "interpret"))
       :interpret)
      ((string= value "baseline") :baseline)
      (t (%usage-error "WASMDOWN_API_MODE must be `interpret' or `baseline', got ~s"
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
(no network authority: every socket operation is refused)."
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
--wasi every capability is withdrawn (PLAN.md §9.1: the default is
empty); with it, the process streams and the guest arguments (the
component path as argv[0]) are the explicitly configured ones."
  (let ((wasi (assoc "--wasi" options)))
    (when (and (not wasi)
               (or (%run-collect options "--dir")
                   (%run-collect options "--env")
                   (%run-collect options "--net")
                   (%run-collect options "--listen")
                   (%run-collect options "--dns")))
      (%usage-error "run: --dir/--env/--net/--listen/--dns require --wasi"))
    (when (and (not wasi) guest-args)
      (%usage-error "run: guest arguments require --wasi"))
    (let ((network (%run-network options)))
      (wasmdown.wasi:make-wasi-context
       :arguments (if wasi (cons file guest-args) nil)
       :environment (mapcar #'%env-pair (%run-collect options "--env"))
       :stdin (and wasi *standard-input*)
       :stdout (and wasi *standard-output*)
       :stderr (and wasi *error-output*)
       :preopens (mapcar #'%dir-preopen (%run-collect options "--dir"))
       :networks (and network (list network))))))

(defun %run-component-file (bytes options guest-args file)
  "Decode + validate a component binary and drive its command world."
  (let ((comp (wasmdown.component:decode-component bytes)))
    (wasmdown.component:validate-component comp)
    ;; the capability context (and its preopen fds) exists only once
    ;; the component is known to be a valid command
    (let ((context (%component-context options guest-args file)))
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
          (ignore-errors (wasmdown.wasi:close-wasi-preopen preopen)))))))

(defun %cmd-run (args)
  (multiple-value-bind (file invokes options guest-args) (%parse-run-args args)
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
                                  (%run-collect options "--env")
                                  (%run-collect options "--net")
                                  (%run-collect options "--listen")
                                  (%run-collect options "--dns"))
                          (%usage-error
                           "run: --dir/--env/--net/--listen/--dns require --wasi"))
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

;;; ---------------------------------------------------------------
;;; Entry point.

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
               (let ((cmd (first args)) (rest (rest args)))
                 (cond ((string= cmd "validate") (return (%cmd-validate rest)))
                       ((string= cmd "inspect") (return (%cmd-inspect rest)))
                       ((string= cmd "compile") (return (%cmd-compile rest)))
                       ((string= cmd "run") (return (%cmd-run rest)))
                       (t
                        (format *error-output* "wasmdown: unknown command ~s~%" cmd)
                        (%usage *error-output*)
                        (return 2))))))))))
