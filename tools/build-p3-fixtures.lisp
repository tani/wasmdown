;;;; Build the hand-authored WASI 0.3.2 (P3) command fixtures.
;;;;
;;;; No toolchain in this environment emits the @0.3.2 component ABI, so
;;;; these guests are ASSEMBLED HERE from the pinned Component binary
;;;; grammar (section ids and encodings in specs/Binary.md and
;;;; src/component.lisp). They are real command components:
;;;;
;;;;   p3-guest.wasm          no imports; run returns result ok (status 0)
;;;;   p3-import-guest.wasm   imports wasi:cli/exit@0.3.2, lowers
;;;;                          exit-with-code into its core, calls it with 7,
;;;;                          so the command's process status is 7
;;;;
;;;; The importing guest is the provider-importing evidence the no-import
;;;; smoke cannot give: the runner composes the provider from the
;;;; component's DECLARED import and the guest calls through a canon lower.
;;;;
;;;; A richer stdout-importing async command lives in tests/wasi.lisp
;;;; (wasi/e2e-command-component-binary); its body is not resume-safe, so
;;;; it is exercised at the run-component level and not pinned as a CLI
;;;; guest (see docs/status.md, "Known gaps").
;;;;
;;;; This is ABI coverage, not evidence of real-toolchain integration:
;;;; no @0.3.2 guest from an upstream toolchain exists here.
;;;;
;;;; Usage:
;;;;   sbcl --script tools/build-p3-fixtures.lisp           ; write both
;;;;   sbcl --script tools/build-p3-fixtures.lisp --check   ; verify hashes

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

(defparameter *dir* (merge-pathnames "tests/fixtures/wasi/" *root*))
(defparameter *sums* (merge-pathnames "P3-CHECKSUMS" *dir*))

;;; ---------------------------------------------------------------
;;; Byte assembly helpers (component binary encodings).

(defun b-u32 (n)
  (let ((out '()))
    (loop while (>= n 128)
          do (push (logior 128 (logand n 127)) out) (setf n (ash n -7))
          finally (push n out)
                  (return (coerce (nreverse out) '(vector (unsigned-byte 8)))))))

(defun b-bytes (&rest xs) (coerce xs '(vector (unsigned-byte 8))))
(defun b-cat (&rest vs) (apply #'concatenate '(vector (unsigned-byte 8)) vs))
(defun b-name (s) (b-cat (b-u32 (length s)) (map 'vector #'char-code s)))
(defun b-nameattrs (s) (b-cat (b-bytes 0) (b-name s)))
(defun b-vec (&rest xs) (b-cat (b-u32 (length xs)) (apply #'b-cat xs)))
(defun b-sec (id content) (b-cat (b-bytes id) (b-u32 (length content)) content))
(defun b-wat (text)
  (wasmdown.binary:encode-module (wasmdown.text:parse-wat text)))

;;; ---------------------------------------------------------------
;;; Fixture 1: no imports, run returns ok.

(defun p3-sync-guest-bytes ()
  (let* ((core (b-wat "(module (func (export \"run\") (result i32)
                                  (i32.const 0)))"))
         (result-type (b-bytes #x6a #x00 #x00))      ; result<_,_>
         (func-type (b-cat (b-bytes #x40) (b-u32 0)  ; functype () -> type 0
                           (b-bytes #x00) (b-u32 0)))
         (types (b-vec result-type func-type)))
    (b-cat (b-bytes 0 #x61 #x73 #x6d #x0d 0 1 0)
           (b-sec 7 types)
           (b-sec 1 core)
           (b-sec 2 (b-vec (b-bytes #x00 0 0)))       ; instantiate module 0
           (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x00 #x01 0)
                                  (b-name "run"))))
           (b-sec 8 (b-vec (b-bytes #x00 #x00 0 0 1))) ; lift core 0: type 1
           (b-sec 5 (b-vec (b-cat (b-bytes #x01 1)
                                  (b-nameattrs "run")
                                  (b-bytes #x01 0))))
           (b-sec 11 (b-vec (b-cat (b-nameattrs "wasi:cli/run@0.3.2")
                                   (b-bytes #x05 0) (b-bytes 0)))))))

;;; ---------------------------------------------------------------
;;; Fixture 2: imports wasi:cli/exit@0.3.2 and exits with code 7.

(defun p3-import-guest-bytes ()
  (let* ((core
           (b-wat "(module
                      (import \"env\" \"exit-with-code\"
                              (func $exit (param i32)))
                      (func (export \"run\") (result i32)
                        (call $exit (i32.const 7))
                        (i32.const 0)))"))
         (result-type (b-bytes #x6a #x00 #x00))            ; result<_,_>
         (exit-ft (b-cat (b-bytes #x40)                     ; functype
                         (b-vec (b-cat (b-name "status-code") (b-bytes #x7d))) ; param u8
                         (b-bytes #x01 #x00)))              ; no results
         (run-ft (b-cat (b-bytes #x40) (b-vec)              ; functype () ->
                        (b-bytes #x00) (b-bytes 0)))        ; result type 0
         ;; inside an INSTANCE TYPE the type-index space is the type's
         ;; own declarations: the func type is declared inline (0x01)
         ;; and the export declarator references it locally as 0.
         (exit-itype
           (b-cat (b-bytes #x42)                            ; instance type
                  (b-vec (b-cat (b-bytes #x01) exit-ft)     ; local type 0
                         (b-cat (b-bytes #x04)              ; export decl
                                (b-nameattrs "exit-with-code")
                                (b-bytes #x01 0)))))        ; func, local 0
         (types (b-vec result-type run-ft exit-itype))
         (import (b-vec (b-cat (b-nameattrs "wasi:cli/exit@0.3.2")
                               (b-bytes #x05 2))))          ; instance type 2
         ;; core instance 0: the bag handing the lowered exit to the module
         (bag (b-cat (b-bytes #x01)                         ; inline exports
                     (b-vec (b-cat (b-name "exit-with-code")
                                   (b-bytes #x00 0)))))     ; core func 0
         ;; core instance 1: instantiate module 0 with env = bag 0
         (m-inst (b-cat (b-bytes #x00 0)                    ; instantiate module
                        (b-vec (b-cat (b-name "env")
                                      (b-bytes #x12 0)))))) ; arg = instance 0
    (b-cat (b-bytes 0 #x61 #x73 #x6d #x0d 0 1 0)
           (b-sec 7 types)
           (b-sec 10 import)
           (b-sec 1 core)
           (b-sec 6 (b-vec (b-cat (b-bytes #x01 #x00 0)     ; alias import export
                                  (b-name "exit-with-code"))))
           (b-sec 8 (b-vec (b-bytes #x01 #x00 0 0)))        ; lower exit
           (b-sec 2 (b-vec bag))                            ; hands it to the core
           (b-sec 2 (b-vec m-inst))
           (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x00 #x01 1)
                                  (b-name "run"))))
           (b-sec 8 (b-vec (b-bytes #x00 #x00 1 0 1)))      ; lift run: type 1
           (b-sec 5 (b-vec (b-cat (b-bytes #x01)            ; inline exports
                                  (b-vec (b-cat (b-nameattrs "run")
                                                (b-bytes #x01 1)))))) ; comp func 1
           (b-sec 11 (b-vec (b-cat (b-nameattrs "wasi:cli/run@0.3.2")
                                   ;; imported exit is instance 0, the
                                   ;; defined run instance is instance 1
                                   (b-bytes #x05 1) (b-bytes 0)))))))

(defparameter *fixtures*
  (list (list "p3-guest.wasm" #'p3-sync-guest-bytes 0 "")
        (list "p3-import-guest.wasm" #'p3-import-guest-bytes 7 "")))

;;; ---------------------------------------------------------------
;;; Verify through this project's own decoder/validator/runner, then
;;; write (or check) the artifacts.

(defun sha256 (path)
  "The FIRST field of sha256sum's output: the digest, without the path."
  (let ((text (with-output-to-string (out)
                (sb-ext:run-program "sha256sum" (list (namestring path))
                                    :output out :search t))))
    (subseq text 0 64)))

(defun recorded-sha (name)
  (when (probe-file *sums*)
    (with-open-file (s *sums*)
      (loop for line = (read-line s nil nil)
            while line
            when (search name line)
              do (return (subseq line 0 (min 64 (length line))))))))

(defun verify (label bytes expected-status expected-stdout)
  (let* ((comp (wasmdown.component:decode-component bytes))
         (_ (wasmdown.component:validate-component comp))
         (sink (make-string-output-stream))
         (context (wasmdown.wasi:make-wasi-context :stdout sink))
         (status (wasmdown.wasi:run-component comp context)))
    (declare (ignore _))
    (unless (eql expected-status status)
      (error "~a: command returned status ~a, expected ~a"
             label status expected-status))
    (let ((out (get-output-stream-string sink)))
      (unless (string= expected-stdout out)
        (error "~a: stdout ~s, expected ~s" label out expected-stdout)))
    (format t "~&~a: decoded, validated, ran (status ~a, stdout ~s)~%"
            label status expected-stdout)))

;;; ---------------------------------------------------------------
;;; Main.

(let ((check (and (member "--check" sb-ext:*posix-argv* :test #'string=) t)))
  (if check
      (let ((bad 0))
        (dolist (entry *fixtures*)
          (destructuring-bind (name _ status stdout) entry
            (declare (ignore _ status stdout))
            (let ((path (merge-pathnames name *dir*))
                  (recorded (recorded-sha name)))
              (if (not (probe-file path))
                  (progn (format t "~&missing fixture ~a (run without --check)~%" path)
                         (incf bad))
                  (let ((actual (sha256 path)))
                    (if (equal recorded actual)
                        (format t "~&~a: OK ~a~%" name actual)
                        (progn (format t "~&~a: STALE (recorded ~a, actual ~a)~%"
                                       name recorded actual)
                               (incf bad))))))))
        (sb-ext:exit :code (if (zerop bad) 0 1)))
      (progn
        (ensure-directories-exist *dir*)
        (let ((lines '()))
          (dolist (entry *fixtures*)
            (destructuring-bind (name builder status stdout) entry
              (let* ((bytes (funcall builder))
                     (path (merge-pathnames name *dir*)))
                (with-open-file (s path :direction :output :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
                  (write-sequence bytes s))
                (verify name bytes status stdout)
                (push (format nil "~a  ~a" (sha256 path) name) lines)
                (format t "~&wrote ~a~%" path))))
          (with-open-file (s *sums* :direction :output :if-exists :supersede)
            (dolist (line (nreverse lines))
              (format s "~a~%" line)))
          (format t "~&wrote ~a~%" *sums*)))))
(sb-ext:exit :code 0)
