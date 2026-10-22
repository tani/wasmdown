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
;;;; No --wasi, --mapdir, --profile-conditional or artifact ABI checks
;;;; (Component Model / WASI are out of this slice).

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
                  ~4trun <file.wasm> [--invoke <name> <int> ...]~%~
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

(defun %parse-run-args (argv)
  "Return (values file invokes) where INVOKES is a list of
(name . integer-args) in command-line order. --invoke may repeat;
integer arguments may repeat."
  (let ((rest argv) (file nil) (invokes '()))
    (loop while rest
          for tok = (pop rest)
          do (cond ((string= tok "--invoke")
                    (let ((name (pop rest)))
                      (unless name
                        (%usage-error "run: --invoke requires an export name"))
                      (let ((ints '()))
                        (loop while (and rest (not (%flagp (first rest))))
                              do (push (%parse-int (pop rest)) ints))
                        (push (cons name (nreverse ints)) invokes))))
                   ((%flagp tok) (%usage-error "unknown option ~a" tok))
                   (file (%usage-error "run: exactly one input file, got ~a and ~a"
                                       file tok))
                   (t (setf file tok))))
    (unless file
      (%usage-error "run: missing input file"))
    (values file (nreverse invokes))))

(defun %cmd-run (args)
  (multiple-value-bind (file invokes) (%parse-run-args args)
    (handler-case
        (let* ((module (wasmdown:read-module file))
               (inst (wasmdown:instantiate module)))
          (dolist (inv invokes)
            (let ((results (apply #'wasmdown:invoke inst
                                   (car inv) (cdr inv))))
              (format t "~{~a~^ ~}~%" results)))
          0)
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
