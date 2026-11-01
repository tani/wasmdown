;;;; Backend parity lane: run the same .wast script through both guest
;;;; backends (:interpret, :compiled) in one process and pair the
;;;; per-command results.
;;;;
;;;; Emits:
;;;;   - tests/backend-parity-results.jsonl   (one line per case, schema
;;;;     wasmdown.backend-parity.v1)
;;;;   - tests/backend-parity-summary.json    (machine summary)
;;;;   - docs/conformance/backends.md         (--commit; otherwise
;;;;     tests/backend-parity/backends.md)
;;;;
;;;; --mode report always exits 0; --mode gate exits non-zero on a
;;;; divergence, a compiled compile refusal, a timeout/harness error in
;;;; either backend, or a command-count mismatch between the backends.
;;;;
;;;; Each file is run twice: a fresh wast-state per run-script call, and
;;;; the backend entry counters are zeroed immediately before each run,
;;;; so neither run inherits the other's state.

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

;;; ---------------------------------------------------------------
;;; CLI.

(defparameter *default-file-spec* "tests/spec-core/*.wast")
(defparameter *jsonl-path* "tests/backend-parity-results.jsonl")
(defparameter *summary-path* "tests/backend-parity-summary.json")
(defparameter *commit-report-path* "docs/conformance/backends.md")
(defparameter *local-report-path* "tests/backend-parity/backends.md")

(defun %split-comma (string)
  (let ((parts '()) (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\,)
            do (let ((part (string-trim " " (subseq string start i))))
                 (when (plusp (length part)) (push part parts)))
               (setf start (1+ i)))
    (let ((part (string-trim " " (subseq string start))))
      (when (plusp (length part)) (push part parts)))
    (nreverse parts)))

(defun %resolve-files (spec)
  "Expand SPEC (comma-separated paths/globs, relative to the repo root;
  a bare file name resolves under tests/spec-core)."
  (let ((parts (%split-comma (or spec *default-file-spec*)))
        (files '()))
    (dolist (part parts)
      (let* ((relative (if (find #\/ part) part
                           (format nil "tests/spec-core/~a" part)))
             (pathname (merge-pathnames relative *root*)))
        (dolist (f (directory pathname))
          (pushnew f files :test #'equal))))
    (setf files (sort files #'string< :key #'namestring))
    (when (null files)
      (error "no .wast files match ~s" spec))
    files))

(defun %parse-args (argv)
  (let ((options (list :files *default-file-spec*
                       :mode :report
                       :commit nil
                       :output *jsonl-path*)))
    (loop for rest on argv
          for arg = (first rest)
          do (cond
               ((string= arg "--files")
                (unless (second rest) (error "--files needs a value"))
                (setf (getf options :files) (second rest))
                (pop rest))
               ((string= arg "--mode")
                (unless (second rest) (error "--mode needs a value"))
                (let ((mode (intern (string-upcase (second rest)) :keyword)))
                  (unless (member mode '(:report :gate))
                    (error "unknown --mode ~a (report|gate)" (second rest)))
                  (setf (getf options :mode) mode))
                (pop rest))
               ((string= arg "--output")
                (unless (second rest) (error "--output needs a value"))
                (setf (getf options :output) (second rest))
                (pop rest))
               ((string= arg "--commit")
                (setf (getf options :commit) t))
               ((string= arg "--")
                (return))
               ((and (> (length arg) 1) (char= (char arg 0) #\-))
                (error "unknown option ~a" arg))))
    options))

;;; ---------------------------------------------------------------
;;; JSON helpers.

(defun %json-escape (s)
  (with-output-to-string (out)
    (loop for c across (or s "")
          do (case c
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< (char-code c) 32)
                      (format out "\\u~4,'0x" (char-code c))
                      (write-char c out)))))))

(defun %json-string (s)
  (format nil "\"~a\"" (%json-escape s)))

(defun %keyword-name (k)
  (and k (string-downcase (string k))))

(defun %json-phase (phase)
  (if phase (%json-string (%keyword-name phase)) "null"))

;;; ---------------------------------------------------------------
;;; A run.

(defun %run-mode (path mode)
  "Run PATH under MODE with fresh counters. Returns (values results
 counts) where COUNTS is an alist (backend . count)."
  (wasmdown.runtime:reset-backend-entry-counts)
  (let ((wasmdown.wast::*instantiate-mode* mode))
    (values (wasmdown.wast:run-script path :file (file-namestring path))
            (wasmdown.runtime:backend-entry-counts))))

(defun %result->case (result)
  (list :file (wasmdown.wast:wast-result-file result)
        :case_id (format nil "~a::~a"
                         (wasmdown.wast:wast-result-file result)
                         (wasmdown.wast:wast-result-command-index result))
        :assertion (%keyword-name
                    (wasmdown.wast:wast-result-command-name result))
        :outcome (wasmdown.wast:wast-result-outcome result)
        :phase (wasmdown.wast:wast-result-phase result)
        :detail (wasmdown.wast:wast-result-detail result)))

(defun %synthetic-case (file index outcome detail)
  (list :file file
        :case_id (format nil "~a::~a" file index)
        :assertion "missing"
        :outcome outcome
        :phase nil
        :detail detail))

(defun %refused-opcode (detail)
  (let ((prefix "no lowering for opcode "))
    (when detail
      (let ((pos (search prefix detail :test #'char-equal)))
        (when pos
          (let* ((start (+ pos (length prefix)))
                 (end (or (position #\Space detail :start start)
                          (length detail))))
            (when (< start end)
              (subseq detail start end))))))))

(defun %classify (i b)
  "CLASS for a divergence between interpret case I and compiled case B."
  (let ((io (getf i :outcome)) (bo (getf b :outcome))
        (ip (getf i :phase)) (bp (getf b :phase)))
    (cond
      ((and (eq bo :fail) (eq bp :compile)) :compiled-compile-refusal)
      ((and (eq io :pass) (not (eq bo :pass))) :compiled-only-failure)
      ((and (eq bo :pass) (not (eq io :pass))) :interpret-only-failure)
      ((and (/= 0 (length (or (getf b :detail) "")))
            (search "trap" (or (getf b :detail) "") :test #'char-equal)
            (search "trap" (or (getf i :detail) "") :test #'char-equal)
            (not (equal (getf b :detail) (getf i :detail))))
       :trap-mismatch)
      ((not (eq ip bp)) :phase-mismatch)
      ((not (eq io bo)) :outcome-mismatch)
      ((not (equal (getf i :detail) (getf b :detail))) :detail-mismatch)
      (t :outcome-mismatch))))

(defun %pair-file (file interpret compiled)
  "Pair the two result lists by command index (case_id). Returns
 (values cases counts) where CASES is a list of plists."
  (let ((by-index (make-hash-table :test #'eql))
        (iby-index (make-hash-table :test #'eql))
        (indices '()))
    (dolist (r interpret)
      (let ((idx (wasmdown.wast:wast-result-command-index r)))
        (setf (gethash idx iby-index) (%result->case r))
        (pushnew idx indices)))
    (dolist (r compiled)
      (let ((idx (wasmdown.wast:wast-result-command-index r)))
        (setf (gethash idx by-index) (%result->case r))
        (pushnew idx indices)))
    (let ((cases '()))
      (dolist (idx (sort indices #'<))
        (let* ((i (or (gethash idx iby-index)
                      (%synthetic-case file idx :harness-error
                                       "no interpret result")))
               (b (or (gethash idx by-index)
                      (%synthetic-case file idx :harness-error
                                       "no compiled result")))
               (match (and (eq (getf i :outcome) (getf b :outcome))
                           (eq (getf i :phase) (getf b :phase)))))
          (push (list :file file
                      :case_id (getf i :case_id)
                      :assertion (getf i :assertion)
                      :interpret i
                      :compiled b
                      :verdict (if match :match :diverge)
                      :class (if match nil (%classify i b)))
                cases)))
      (values (nreverse cases) (length interpret)))))

;;; ---------------------------------------------------------------
;;; The run.

(defun %sbcl-version ()
  (lisp-implementation-version))

(defun %git-line (args)
  (handler-case
      (let* ((out (uiop:run-program (cons "git" args)
                                    :output :string
                                    :error-output nil
                                    :directory (namestring *root*)))
             (trimmed (string-trim '(#\Space #\Newline #\Return #\Tab) out)))
        (if (plusp (length trimmed)) trimmed "unknown"))
    (error () "unknown")))

(defun %source-commit () (%git-line '("rev-parse" "HEAD")))
(defun %source-tree () (%git-line '("rev-parse" "HEAD^{tree}")))
(defun %clean-tree-p ()
  "True when the tested working tree was clean BEFORE this run wrote its
artifacts; computed at summary time, so the report's own writes do not
count against it."
  (handler-case
      (zerop (length (string-trim '(#\Space #\Newline #\Return #\Tab)
                                  (uiop:run-program '("git" "status" "--porcelain")
                                                    :output :string
                                                    :error-output nil
                                                    :directory (namestring *root*)))))
    (error () nil)))

(defun %now-utc ()
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day h m s)))

(defun %run-parity (files)
  (let ((all-cases '()) (per-file '()) (class-counts (make-hash-table :test #'eq))
        (opcode-counts (make-hash-table :test #'equal))
        (opcode-examples (make-hash-table :test #'equal))
        (total-interpret-entries 0) (total-compiled-entries 0)
        (total-interpret-commands 0) (total-compiled-commands 0)
        (start (get-internal-real-time)))
    ;; A measurement lane never writes to docs/conformance/core.md; the
    ;; only reports it can touch are its own.
    (dolist (path files)
      (let* ((name (file-namestring path))
             (interpret-results nil) (compiled-results nil)
             (interpret-counts nil) (compiled-counts nil))
        (format t "~&parity: ~a~%" name)
        (finish-output)
        (multiple-value-setq (interpret-results interpret-counts)
          (%run-mode path :interpret))
        (multiple-value-setq (compiled-results compiled-counts)
          (%run-mode path :compiled))
        (multiple-value-bind (cases i-count)
            (%pair-file name interpret-results compiled-results)
          (let ((match 0) (diverge 0) (refusals 0))
            (dolist (case cases)
              (let ((verdict (getf case :verdict))
                    (class (getf case :class))
                    (base (getf case :compiled)))
                (if (eq verdict :match)
                    (incf match)
                    (incf diverge))
                (incf (gethash (or class :match) class-counts 0))
                (when (eq class :compiled-compile-refusal)
                  (incf refusals)
                  (let ((opcode (%refused-opcode (getf base :detail))))
                    (when opcode
                      (incf (gethash opcode opcode-counts 0))
                      (unless (gethash opcode opcode-examples)
                        (setf (gethash opcode opcode-examples)
                              (list :file name
                                    :case_id (getf case :case_id))))))))
              (push case all-cases))
            (let ((i-entries (or (cdr (assoc :interpret interpret-counts)) 0))
                  (b-entries (or (cdr (assoc :compiled compiled-counts)) 0)))
              (incf total-interpret-entries i-entries)
              (incf total-compiled-entries b-entries)
              (incf total-interpret-commands i-count)
              (incf total-compiled-commands (length compiled-results))
              (push (list :file name
                          :interpret_commands i-count
                          :compiled_commands (length compiled-results)
                          :match match
                          :diverge diverge
                          :refusals refusals
                          :interpret_entries i-entries
                          :compiled_entries b-entries)
                    per-file))))))
    (setf all-cases (nreverse all-cases)
          per-file (nreverse per-file))
    (list :cases all-cases
          :per-file per-file
          :class-counts class-counts
          :opcode-counts opcode-counts
          :opcode-examples opcode-examples
          :interpret-entries total-interpret-entries
          :compiled-entries total-compiled-entries
          :interpret-commands total-interpret-commands
          :compiled-commands total-compiled-commands
          :elapsed (/ (- (get-internal-real-time) start)
                      (float internal-time-units-per-second)))))

;;; ---------------------------------------------------------------
;;; JSONL output.

(defun %write-jsonl (cases path)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (case cases)
      (let ((i (getf case :interpret)) (b (getf case :compiled)))
        (format s
                "{\"schema\":\"wasmdown.backend-parity.v1\",~
                 \"file\":~a,~
                 \"case_id\":~a,~
                 \"assertion\":~a,~
                 \"interpret\":{\"outcome\":~a,\"phase\":~a,\"detail\":~a},~
                 \"compiled\":{\"outcome\":~a,\"phase\":~a,\"detail\":~a},~
                 \"verdict\":~a,~
                 \"class\":~a}~%"
                (%json-string (getf case :file))
                (%json-string (getf case :case_id))
                (%json-string (getf case :assertion))
                (%json-string (%keyword-name (getf i :outcome)))
                (%json-phase (getf i :phase))
                (%json-string (or (getf i :detail) ""))
                (%json-string (%keyword-name (getf b :outcome)))
                (%json-phase (getf b :phase))
                (%json-string (or (getf b :detail) ""))
                (%json-string (%keyword-name (getf case :verdict)))
                (if (getf case :class)
                    (%json-string (%keyword-name (getf case :class)))
                    "null"))))))

;;; ---------------------------------------------------------------
;;; Summary JSON.

(defun %opcodes-alist (opcode-counts opcode-examples)
  (let ((out '()))
    (maphash (lambda (opcode count)
               (push (list :opcode opcode :count count
                           :example (gethash opcode opcode-examples))
                     out))
             opcode-counts)
    (sort out #'string< :key (lambda (x) (getf x :opcode)))))

(defun %write-summary (run options files path)
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (format s "{\"schema\":\"wasmdown.backend-parity-summary.v1\",~%")
    (format s " \"generated_at\":~a,~%" (%json-string (%now-utc)))
    (format s " \"mode\":~a,~%" (%json-string (%keyword-name (getf options :mode))))
    (format s " \"files_arg\":~a,~%" (%json-string (getf options :files)))
    (format s " \"sbcl_version\":~a,~%" (%json-string (%sbcl-version)))
    (format s " \"source_commit\":~a,~%" (%json-string (%source-commit)))
    (format s " \"source_tree\":~a,~%" (%json-string (%source-tree)))
    (format s " \"clean_tree\":~a,~%" (if (%clean-tree-p) "true" "false"))
    (format s " \"files\":[~%")
    (loop for cell in (getf run :per-file)
          for first = t then nil
          do (unless first (format s ",~%"))
             (format s
                     "  {\"file\":~a,\"interpret_commands\":~d,~
                      \"compiled_commands\":~d,\"match\":~d,\"diverge\":~d,~
                      \"refusals\":~d,\"interpret_entries\":~d,~
                      \"compiled_entries\":~d}"
                     (%json-string (getf cell :file))
                     (getf cell :interpret_commands)
                     (getf cell :compiled_commands)
                     (getf cell :match)
                     (getf cell :diverge)
                     (getf cell :refusals)
                     (getf cell :interpret_entries)
                     (getf cell :compiled_entries)))
    (format s "~% ],~%")
    (let ((cases (getf run :cases)))
      (format s " \"totals\":{\"cases\":~d,\"match\":~d,\"diverge\":~d,~
               \"interpret_commands\":~d,\"compiled_commands\":~d,~
               \"refusals\":~d},~%"
              (length cases)
              (count :match cases :key (lambda (c) (getf c :verdict)))
              (count :diverge cases :key (lambda (c) (getf c :verdict)))
              (getf run :interpret-commands)
              (getf run :compiled-commands)
              (count :compiled-compile-refusal cases
                     :key (lambda (c) (getf c :class)))))
    (format s " \"classes\":{")
    (let ((first t))
      (dolist (class '(:match :compiled-compile-refusal
                       :compiled-only-failure :interpret-only-failure
                       :phase-mismatch :outcome-mismatch :trap-mismatch
                       :detail-mismatch))
        (let ((n (gethash class (getf run :class-counts) 0)))
          (format s "~:[,~;~]~a:~d" first
                  (%json-string (%keyword-name class)) n)
          (setf first nil))))
    (format s "},~%")
    (format s " \"refused_opcodes\":[")
    (loop for cell in (%opcodes-alist (getf run :opcode-counts)
                                     (getf run :opcode-examples))
          for first = t then nil
          do (unless first (format s ","))
             (let ((example (getf cell :example)))
               (format s
                       "{\"opcode\":~a,\"count\":~d,\"example_file\":~a,~
                        \"example_case_id\":~a}"
                       (%json-string (getf cell :opcode))
                       (getf cell :count)
                       (%json-string (or (getf example :file) ""))
                       (%json-string (or (getf example :case_id) "")))))
    (format s "],~%")
    (format s " \"backend_entry_counts\":{\"interpret\":~d,\"compiled\":~d},~%"
            (getf run :interpret-entries)
            (getf run :compiled-entries))
    (format s " \"elapsed_seconds\":~,2f,~%" (getf run :elapsed))
    (format s " \"files_run\":~d~%}" (length files))))

;;; ---------------------------------------------------------------
;;; gate.

(defun %gate-failures (run files)
  (let ((failures '())
        (cases (getf run :cases)))
    (when (/= (getf run :interpret-commands) (getf run :compiled-commands))
      (push (format nil "executed command count differs: interpret ~d vs compiled ~d"
                    (getf run :interpret-commands)
                    (getf run :compiled-commands))
            failures))
    (when (plusp (count :diverge cases :key (lambda (c) (getf c :verdict))))
      (push (format nil "~d diverging case~:p"
                    (count :diverge cases :key (lambda (c) (getf c :verdict))))
            failures))
    (let ((refusals (count :compiled-compile-refusal cases
                           :key (lambda (c) (getf c :class)))))
      (when (plusp refusals)
        (push (format nil "~d compiled compile refusal~:p" refusals)
              failures)))
    (dolist (case cases)
      (dolist (side '(:interpret :compiled))
        (let ((outcome (getf (getf case side) :outcome)))
          (when (member outcome '(:timeout :harness-error))
            (push (format nil "~a has ~a outcome" (getf case :case_id) outcome)
                  failures)))))
    (when (null files) (push "no files were run" failures))
    failures))

;;; ---------------------------------------------------------------
;;; Markdown report.

(defun %md-cell (s)
  (let ((s (or s "")))
    (with-output-to-string (out)
      (loop for c across s
            do (cond ((char= c #\|) (write-string "\\|" out))
                     ((char= c #\Newline) (write-char #\Space out))
                     (t (write-char c out)))))))

(defun %file-stem (name)
  (let ((dot (position #\. name :from-end t)))
    (if dot (subseq name 0 dot) name)))

(defun %report-command (options files)
  "The exact shell command that reproduces this report."
  (let ((files (getf options :files)))
    (if (and (string= files *default-file-spec*)
             (eq (getf options :mode) :report)
             (getf options :commit))
        "make report-backend-parity"
        (format nil
                "sbcl --script tools/run-backend-parity.lisp --files \"~a\" --mode ~a --output ~a~@[ --commit~]"
                files
                (%keyword-name (getf options :mode))
                (getf options :output)
                (getf options :commit)))))

(defun %write-report (run options files path)
  (ensure-directories-exist path)
  (let* ((cases (getf run :cases))
         (divergences (count-if (lambda (c) (eq (getf c :verdict) :diverge))
                                cases))
         (refusals (count :compiled-compile-refusal cases
                          :key (lambda (c) (getf c :class))))
         (interp-timeouts (count-if
                           (lambda (c) (eq (getf (getf c :interpret) :outcome)
                                           :timeout))
                           cases))
         (base-timeouts (count-if
                         (lambda (c) (eq (getf (getf c :compiled) :outcome)
                                         :timeout))
                         cases))
         (interp-harness (count-if
                          (lambda (c) (eq (getf (getf c :interpret) :outcome)
                                          :harness-error))
                          cases))
         (base-harness (count-if
                        (lambda (c) (eq (getf (getf c :compiled) :outcome)
                                        :harness-error))
                        cases)))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "# Backend parity~%~%")
      (format s "Generated by `~a` (tools/run-backend-parity.lisp). Lane index: [`docs/conformance/README.md`](README.md).~%~%"
              (%report-command options files))
      (format s "The same pinned scripts run twice in one process, once with~%")
      (format s "`wasmdown.wast:*instantiate-mode*` bound to `:interpret` and once to~%")
      (format s "`:compiled` (the compiled backend lowers every defined function of~%")
      (format s "the module). A **pair** is one command executed by both backends,~%")
      (format s "matched by case id; two backends never share a wast-state, and the~%")
      (format s "backend entry counters are zeroed before each run.~%~%")
      (format s "A **match** is the same outcome *and* failure phase in both modes.~%")
      (format s "Anything else is a **diverge** and is classified below; a compiled~%")
      (format s "`compile` phase is a refusal to lower an instruction, never a~%")
      (format s "silent fall back to the interpreter.~%~%")
      (format s "## Lane modes~%~%")
      (format s "| Mode | Command | Exit semantics |~%")
      (format s "| --- | --- | --- |~%")
      (format s "| report | `make report-backend-parity` | Always zero; writes this report. |~%")
      (format s "| gate | `make test-backend-parity` | Non-zero on any divergence, compiled compile refusal, timeout or harness error in either backend, or a command-count mismatch. |~%~%")
      (format s "- Scripts: ~a~%"
              (if (string= (getf options :files) *default-file-spec*)
                  "pinned core scripts, `tests/spec-core/*.wast`"
                  (format nil "selected by `--files \"~a\"` (pinned core scripts unless named)"
                          (getf options :files))))
      (format s "- SBCL: `~a`~%" (%sbcl-version))
      (format s "- Source commit: `~a`~%" (%source-commit))
      (format s "- Files run: ~d~%" (length files))
      (format s "- Wall time: ~,1f s~%~%" (getf run :elapsed))
      (format s "## Aggregate~%~%")
      (format s "| Verdict | Count |~%| --- | --- |~%")
      (format s "| match | ~d |~%"
              (count :match cases :key (lambda (c) (getf c :verdict))))
      (format s "| diverge | ~d |~%| **total** | **~d** |~%~%"
              divergences (length cases))
      (format s "Interpret commands: ~d; compiled commands: ~d;~%"
              (getf run :interpret-commands)
              (getf run :compiled-commands))
      (format s "command-count mismatch: ~:[no~;yes~].~%~%"
              (/= (getf run :interpret-commands)
                  (getf run :compiled-commands)))
      (format s "## Divergence classes~%~%")
      (format s "| Class | Count |~%| --- | --- |~%")
      (format s "| match | ~d |~%"
              (gethash :match (getf run :class-counts) 0))
      (dolist (class '(:compiled-compile-refusal :compiled-only-failure
                       :interpret-only-failure :phase-mismatch
                       :outcome-mismatch :trap-mismatch :detail-mismatch))
        (format s "| ~a | ~d |~%" (%keyword-name class)
                (gethash class (getf run :class-counts) 0)))
      (format s "~%Timeout outcomes: interpret ~d, compiled ~d.~%"
              interp-timeouts base-timeouts)
      (format s "Harness-error outcomes: interpret ~d, compiled ~d.~%~%"
              interp-harness base-harness)
      (format s "## Per-file counts~%~%")
      (format s "| File | interpret commands | compiled commands | match | diverge | refusals |~%")
      (format s "| --- | --- | --- | --- | --- | --- |~%")
      (dolist (cell (getf run :per-file))
        (format s "| ~a | ~d | ~d | ~d | ~d | ~d |~%"
                (%md-cell (%file-stem (getf cell :file)))
                (getf cell :interpret_commands)
                (getf cell :compiled_commands)
                (getf cell :match)
                (getf cell :diverge)
                (getf cell :refusals)))
      (format s "~%## Refused opcodes~%~%")
      (let ((opcodes (%opcodes-alist (getf run :opcode-counts)
                                     (getf run :opcode-examples))))
        (format s "| Opcode | Count | Example |~%")
        (format s "| --- | --- | --- |~%")
        (if (null opcodes)
            (format s "| (none) | 0 | — |~%")
            (dolist (cell opcodes)
              (let ((example (getf cell :example)))
                (format s "| `~a` | ~d | ~a ~a |~%"
                        (getf cell :opcode)
                        (getf cell :count)
                        (%md-cell (or (getf example :file) ""))
                        (%md-cell (or (getf example :case_id) "")))))))
      (format s "~%## Backend entry counts~%~%")
      (format s "| Backend | Wasm function entries executed |~%")
      (format s "| --- | --- |~%")
      (format s "| interpret | ~d |~%" (getf run :interpret-entries))
      (format s "| compiled | ~d |~%" (getf run :compiled-entries))
      (format s "~%Counters are reset before each per-file run and summed here; they~%")
      (format s "count Wasm function entries, not host calls.~%~%")
      (format s "## Regenerating~%~%")
      (format s "```~%~a~%```~%" (%report-command options files)))))

;;; ---------------------------------------------------------------
;;; Main.

(let* ((options (%parse-args (rest sb-ext:*posix-argv*)))
       (files (%resolve-files (getf options :files)))
       (run (%run-parity files))
       (jsonl (merge-pathnames
               (if (and (eq (getf options :mode) :gate)
                        (string= (getf options :output)
                                 *jsonl-path*))
                   ;; a gate run keeps its own results out of the
                   ;; committed report pair
                   "tests/backend-parity/results.jsonl"
                   (getf options :output))
               *root*))
       ;; A GATE run must never overwrite the committed report/summary
       ;; pair that a report run produces: its machine summary goes to
       ;; the local (gitignored) directory instead.
       (gate-p (eq (getf options :mode) :gate))
       (summary (merge-pathnames
                 (if gate-p "tests/backend-parity/summary.json"
                     *summary-path*)
                 *root*))
       (report (merge-pathnames
                (if (and (getf options :commit)
                         (not gate-p))
                    *commit-report-path*
                    *local-report-path*)
                *root*)))
  (%write-jsonl (getf run :cases) jsonl)
  (%write-summary run options files summary)
  (%write-report run options files report)
  (format t "~&backend parity: ~d case~:p, ~d diverge, ~d compiled compile refusal~:p~%"
          (length (getf run :cases))
          (count :diverge (getf run :cases)
                 :key (lambda (c) (getf c :verdict)))
          (count :compiled-compile-refusal (getf run :cases)
                 :key (lambda (c) (getf c :class))))
  (format t "  interpret commands ~d, compiled commands ~d~%"
          (getf run :interpret-commands) (getf run :compiled-commands))
  (format t "  wrote ~a~%  wrote ~a~%  wrote ~a~%"
          (namestring jsonl) (namestring summary) (namestring report))
  (when (and (getf options :commit) (eq (getf options :mode) :gate))
    (format t "~&note: --commit is ignored in gate mode (committed reports come from report runs)~%"))
  (when (eq (getf options :mode) :gate)
    (let ((failures (%gate-failures run files)))
      (if failures
          (progn
            (format t "~&GATE: RED~%")
            (dolist (f (nreverse failures))
              (format t "  - ~a~%" f))
            (sb-ext:exit :code 1))
          (format t "~&GATE: GREEN~%"))))
  (sb-ext:exit :code 0))
