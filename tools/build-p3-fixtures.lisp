;;;; Build the hand-authored WASI 0.3.2 (P3) command fixture.
;;;;
;;;; No toolchain in this environment emits the @0.3.2 component ABI, so
;;;; the P3 guest is ASSEMBLED HERE from the pinned Component binary
;;;; grammar (section ids and encodings in specs/Binary.md and
;;;; src/component.lisp). It is a real command component, not a stub:
;;;;
;;;;   import wasi:cli/stdout@0.3.2
;;;;   core module run: stream.new, write "hi" through a stream,
;;;;                    future.read the end, task.return(ok)
;;;;   export `run' (the command runner's direct-export fallback)
;;;;
;;;; The same construction lives in tests/wasi.lisp
;;;; (wasi/e2e-command-component-binary); the lane runs THIS pinned file
;;;; through the real CLI so the fixture is exercised as a guest.
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

(defun p3-guest-bytes ()
  "A command-world component importing wasi:cli/stdout@0.3.2. It writes
\"hi\" through a stream and task.returns ok."
  (let* ((memmod (b-wat "(module (memory (export \"mem\") 1))"))
         (mmod
           (b-wat
            "(module
               (import \"env\" \"mem\" (memory 1))
               (import \"env\" \"task.return\" (func $tr (param i32)))
               (import \"env\" \"stream.new\" (func $snew (result i64)))
               (import \"env\" \"stream.write\"
                       (func $swrite (param i32 i32 i32) (result i32)))
               (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))
               (import \"env\" \"future.read\"
                       (func $fread (param i32 i32) (result i32)))
               (import \"env\" \"write-via-stream\" (func $wvs (param i32) (result i32)))
               (global $fut (mut i32) (i32.const 0))
               (func (export \"run\") (result i32)
                 (local $p i64) (local $rx i32) (local $tx i32)
                 (local $ret i32)
                 (local.set $p (call $snew))
                 (local.set $rx (i32.wrap_i64 (local.get $p)))
                 (local.set $tx
                   (i32.wrap_i64 (i64.shr_u (local.get $p) (i64.const 32))))
                 (i32.store16 (i32.const 16) (i32.const 0x6968))
                 (global.set $fut (call $wvs (local.get $rx)))
                 (local.set $ret
                   (call $swrite (local.get $tx) (i32.const 16) (i32.const 2)))
                 (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))
                 (call $sdropw (local.get $tx))
                 (local.set $ret
                   (call $fread (global.get $fut) (i32.const 32)))
                 (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))
                 (call $tr (i32.const 0))
                 (i32.const 0))
               (func (export \"run-cb\") (param i32 i32 i32) (result i32)
                 (i32.const 0)))"))
         (types
           (b-vec
            (b-cat (b-bytes #x6d 3) (b-name "io")
                   (b-name "illegal-byte-sequence") (b-name "pipe"))
            (b-bytes #x66 1 #x7d)                 ; stream<u8>
            (b-bytes #x6a 0 1 0)                  ; result<_,_>
            (b-bytes #x65 1 2)                    ; enum error-code
            (b-cat (b-bytes #x40 1) (b-name "data") (b-bytes 1) (b-bytes 0 3))
            (b-cat (b-bytes #x42 7)               ; instance: stdout iface
                   (b-bytes #x02 #x03 #x02 #x01 #x00)
                   (b-cat (b-bytes #x04) (b-nameattrs "error-code")
                          (b-bytes #x03 #x00 #x00))
                   (b-bytes #x01 #x6a #x00 #x01 #x00)
                   (b-bytes #x01 #x65 #x01 #x02)
                   (b-bytes #x01 #x66 #x01 #x7d)
                   (b-cat (b-bytes #x01 #x40 #x01) (b-name "data")
                          (b-bytes #x04 #x00 #x03))
                   (b-cat (b-bytes #x04) (b-nameattrs "write-via-stream")
                          (b-bytes #x01 #x05)))
            (b-bytes #x6a 0 0)
            (b-cat (b-bytes #x43 0) (b-bytes 0 6))))
         (import (b-vec (b-cat (b-nameattrs "wasi:cli/stdout@0.3.2")
                               (b-bytes #x05 5))))
         (env-bag
           (b-cat (b-bytes #x01 7)
                  (b-cat (b-name "mem") (b-bytes #x02 0))
                  (b-cat (b-name "task.return") (b-bytes #x00 0))
                  (b-cat (b-name "stream.new") (b-bytes #x00 1))
                  (b-cat (b-name "stream.write") (b-bytes #x00 2))
                  (b-cat (b-name "stream.drop-writable") (b-bytes #x00 3))
                  (b-cat (b-name "future.read") (b-bytes #x00 4))
                  (b-cat (b-name "write-via-stream") (b-bytes #x00 5)))))
    (b-cat (b-bytes 0 #x61 #x73 #x6d #x0d 0 1 0)   ; magic/version/layer
           (b-sec 7 types)
           (b-sec 10 import)
           (b-sec 1 memmod)
           (b-sec 1 mmod)
           (b-sec 2 (b-vec (b-bytes #x00 0 0)))
           (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x02 #x01 0) (b-name "mem"))))
           (b-sec 6 (b-vec (b-cat (b-bytes #x01 #x00 0)
                                  (b-name "write-via-stream"))))
           (b-sec 8 (b-vec (b-bytes #x09 #x00 6 #x00)        ; task.return
                           (b-bytes #x0e 1)                  ; stream.new
                           (b-bytes #x10 1 #x01 #x03 0)      ; stream.write
                           (b-bytes #x14 1)                  ; drop-writable
                           (b-bytes #x16 3 #x01 #x03 0)      ; future.read
                           (b-bytes #x01 #x00 0 #x00)))      ; lower stdout
           (b-sec 2 (b-vec env-bag
                           (b-cat (b-bytes #x00 1 1) (b-name "env")
                                  (b-bytes #x12 1))))
           (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x00 #x01 2) (b-name "run"))
                           (b-cat (b-bytes #x00 #x00 #x01 2)
                                  (b-name "run-cb"))))
           (b-sec 8 (b-vec (b-bytes #x00 #x00 6 #x02 #x06 #x07 7 7)))
           (b-sec 11 (b-vec (b-cat (b-nameattrs "run")
                                   (b-bytes #x01 1) (b-bytes 0)))))))

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
