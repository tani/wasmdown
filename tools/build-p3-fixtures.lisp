;;;; Build the hand-authored WASI 0.3.2 (P3) command fixture.
;;;;
;;;; No toolchain in this environment emits the @0.3.2 component ABI, so
;;;; the P3 guest is ASSEMBLED HERE from the pinned Component binary
;;;; grammar (section ids and encodings in specs/Binary.md and
;;;; src/component.lisp). It is a real command component, not a stub:
;;;;
;;;;   no imports; core `run` () -> i32 returning 0
;;;;   component functype () -> (result<_,_>), canon lift, and the
;;;;   `wasi:cli/run@0.3.2' export (the command world's success status)
;;;;
;;;; A richer stdout-importing async command lives in tests/wasi.lisp
;;;; (wasi/e2e-command-component-binary); its body is not resume-safe, so
;;;; it is exercised at the run-component level and not pinned as a CLI
;;;; guest (see docs/status.md, "Known gaps").
;;;;
;;;; This is ABI coverage, not evidence of real-toolchain integration:
;;;; no pinned @0.3.2 guest from an upstream toolchain exists here.
;;;;
;;;; Usage:
;;;;   sbcl --script tools/build-p3-fixtures.lisp           ; write
;;;;   sbcl --script tools/build-p3-fixtures.lisp --check   ; verify hash

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

(defparameter *out*
  (merge-pathnames "tests/fixtures/wasi/p3-guest.wasm" *root*))
(defparameter *sums*
  (merge-pathnames "tests/fixtures/wasi/P3-CHECKSUMS" *root*))

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
;;; The guest.

;;; ---------------------------------------------------------------
;;; The SYNC command guest the lane drives through the CLI: no imports,
;;; canonical `result` layout, nested core compiled by the selected
;;; backend. (The stdout-importing async command above is pinned by
;;; tests/wasi.lisp at the run-component level; driving an async-lifted
;;; command through the CLI with a reactor re-enters the core body, which
;;; is recorded as an open async defect, so the guest LANE uses this one.)

(defun p3-sync-guest-bytes ()
  "A command component with no imports: run returns ok."
  (let* ((core (b-wat "(module (func (export \"run\") (result i32)
                                  (i32.const 0)))"))
         (result-type (b-bytes #x6a #x00 #x00))      ; result<_,_>
         (func-type (b-cat (b-bytes #x40) (b-u32 0)  ; functype () ->
                           (b-bytes #x00) (b-u32 0))) ;  -> typeidx 0
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
;;; Verify through this project's own decoder/validator/runner, then
;;; write (or check) the artifact.

(defun sha256 (path)
  "The FIRST field of sha256sum's output: the digest, without the path."
  (let ((text (with-output-to-string (out)
                (sb-ext:run-program "sha256sum" (list (namestring path))
                                    :output out :search t))))
    (subseq text 0 64)))

(defun recorded-sha ()
  (when (probe-file *sums*)
    (with-open-file (s *sums*)
      (let ((line (read-line s nil nil)))
        (when line (subseq line 0 (min 64 (length line))))))))

(defun verify (bytes expected-stdout)
  (let* ((comp (wasmdown.component:decode-component bytes))
         (_ (wasmdown.component:validate-component comp))
         (sink (make-string-output-stream))
         (status (wasmdown.wasi:run-component
                  comp (wasmdown.wasi:make-wasi-context :stdout sink))))
    (declare (ignore _))
    (unless (eql 0 status)
      (error "p3 fixture: command returned status ~a, expected 0" status))
    (let ((out (get-output-stream-string sink)))
      (unless (string= expected-stdout out)
        (error "p3 fixture: stdout ~s, expected ~s" out expected-stdout)))
    (format t "~&p3 fixture: decoded, validated, ran (status 0, stdout ~s)~%"
            expected-stdout)))

(let ((check (and (member "--check" sb-ext:*posix-argv* :test #'string=) t)))
  (if check
      (progn
        (unless (probe-file *out*)
          (format t "~&missing fixture ~a (run without --check)~%" *out*)
          (sb-ext:exit :code 1))
        (let* ((recorded (recorded-sha))
               (actual (sha256 *out*)))
          (cond ((equal recorded actual)
                 (format t "~&p3 fixture: OK ~a~%" actual))
                (t
                 (format t "~&p3 fixture: STALE (recorded ~a, actual ~a)~%"
                         recorded actual)
                 (sb-ext:exit :code 1)))))
      (let ((bytes (p3-sync-guest-bytes)))
        (verify bytes "")
        (ensure-directories-exist *out*)
        (with-open-file (s *out* :direction :output :if-exists :supersede
                                :element-type '(unsigned-byte 8))
          (write-sequence bytes s))
        (let ((sum (sha256 *out*)))
          (with-open-file (s *sums* :direction :output :if-exists :supersede)
            (format s "~a  p3-guest.wasm~%" sum))
          (format t "~&wrote ~a~%wrote ~a~%" *out* *sums*)))))
(sb-ext:exit :code 0)
