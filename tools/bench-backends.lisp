;;;; Backend cost bench: decode / validate / instantiate (+compile) /
;;;; execute for the SAME modules under both explicitly selected guest
;;;; backends.
;;;;
;;;; The point is not a scoreboard: it separates the costs the plan
;;;; requires to be measured independently — decoding, validation,
;;;; instantiation, compilation, execution — and reports them per
;;;; backend so a later change cannot hide a compile-cost regression
;;;; inside an execution improvement. Eager whole-module compilation is
;;;; the design: `:compiled` instantiate time INCLUDES compiling every
;;;; defined function.
;;;;
;;;; Usage:
;;;;   sbcl --script tools/bench-backends.lisp                 ; report on stdout
;;;;   sbcl --script tools/bench-backends.lisp --json out.json
;;;;   sbcl --script tools/bench-backends.lisp --commit        ; writes
;;;;                                        docs/conformance/performance.md
;;;;   sbcl --script tools/bench-backends.lisp --wasm big.wasm ; add a fixture
;;;;
;;;; Every number is a median of --repeat runs (default 3) unless noted.

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

;;; ---------------------------------------------------------------
;;; CLI.

(defun %option (argv flag default)
  (let ((pos (position flag argv :test #'string=)))
    (if (and pos (< (1+ pos) (length argv)))
        (nth (1+ pos) argv)
        default)))

(defparameter *argv* (rest sb-ext:*posix-argv*))
(defparameter *repeat* (parse-integer (%option *argv* "--repeat" "3")))
(defparameter *json-path* (%option *argv* "--json" nil))
(defparameter *commit* (and (position "--commit" *argv* :test #'string=) t))
(defparameter *wasm* (%option *argv* "--wasm" nil))

;;; ---------------------------------------------------------------
;;; Fixtures. Small kernels whose work is dominated by the guest loop;
;;;; each is a function of one i32 argument and returns i32/f64 so a
;;;; single invocation is measurable.

(defparameter *kernels*
  '(("loop-sum"
     "(module (func (export \"run\") (param $n i32) (result i32)
        (local $i i32) (local $acc i32)
        (block $done
          (loop $l
            (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
            (local.set $acc (i32.add (local.get $acc) (local.get $i)))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $l)))
        (local.get $acc)))"
     2000000)
    ("tail-count"
     "(module (func $count (param $n i32) (result i32)
        (if (result i32) (i32.eqz (local.get $n))
            (then (i32.const 0))
            (else (return_call $count
                    (i32.sub (local.get $n) (i32.const 1))))))
        (func (export \"run\") (param $n i32) (result i32)
          (return_call $count (local.get $n))))"
     2000000)
    ("fib-recursive"
     "(module (func $fib (param $n i32) (result i32)
        (if (result i32) (i32.lt_s (local.get $n) (i32.const 2))
            (then (local.get $n))
            (else (i32.add (call $fib (i32.sub (local.get $n) (i32.const 1)))
                           (call $fib (i32.sub (local.get $n) (i32.const 2)))))))
        (func (export \"run\") (param $n i32) (result i32) (call $fib (local.get $n))))"
     25)
    ("memory-fill-sum"
     "(module (memory 16)
        (func (export \"run\") (param $n i32) (result i32)
          (local $i i32) (local $acc i32)
          (block $done
            (loop $l
              (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
              (i32.store (i32.and (local.get $i) (i32.const 65535))
                         (local.get $i))
              (local.set $acc
                (i32.add (local.get $acc)
                         (i32.load (i32.and (local.get $i) (i32.const 65535)))))
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $l)))
          (local.get $acc)))"
     1000000)
    ("f64-accumulate"
     "(module (func (export \"run\") (param $n i32) (result i32)
        (local $i i32) (local $acc f64)
        (block $done
          (loop $l
            (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
            (local.set $acc (f64.add (local.get $acc) (f64.convert_i32_s (local.get $i))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $l)))
        ;; the result is an i32 by contract; fold the float back so the
        ;; loop cannot be removed by a future optimizer
        (i32.trunc_sat_f64_s (local.get $acc))))"
     500000)
    ("indirect-call"
     "(module (type $t (func (param i32) (result i32)))
        (table 1 funcref)
        (elem (i32.const 0) $inc)
        (func $inc (type $t) (i32.add (local.get 0) (i32.const 1)))
        (func (export \"run\") (param $n i32) (result i32)
          (local $i i32) (local $acc i32)
          (block $done
            (loop $l
              (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
              (local.set $acc (call_indirect (type $t) (local.get $acc) (i32.const 0)))
              (local.set $i (i32.add (local.get $i) (i32.const 1)))
              (br $l)))
          (local.get $acc)))"
     500000)))

;;; ---------------------------------------------------------------
;;; Timing helpers.

(defun %median (xs)
  (let* ((s (sort (copy-list xs) #'<))
         (n (length s)))
    (if (oddp n)
        (nth (floor n 2) s)
        (/ (+ (nth (1- (floor n 2)) s) (nth (floor n 2) s)) 2.0))))

(defun %seconds (fn)
  "Run FN once; return (values elapsed-seconds bytes-consed result)."
  (multiple-value-bind (sec0 usec0) (sb-ext:get-time-of-day)
    (let* ((consed-before (sb-ext:get-bytes-consed))
           (result (funcall fn)))
      (multiple-value-bind (sec1 usec1) (sb-ext:get-time-of-day)
        (values (+ (- sec1 sec0) (/ (- usec1 usec0) 1000000.0d0))
                (- (sb-ext:get-bytes-consed) consed-before)
                result)))))

(defun %median-of (function)
  "Median elapsed seconds and median bytes consed of *REPEAT* fresh
calls, plus the last result."
  (let ((times '()) (consed '()) (last nil))
    (dotimes (i *repeat*)
      (multiple-value-bind (elapsed bytes result)
          (%seconds function)
        (push elapsed times)
        (push bytes consed)
        (setf last result)))
    (values (%median times) (%median consed) last)))

;;; ---------------------------------------------------------------
;;; One measurement per (fixture, backend).

(defun %bytes-of (text)
  "Encode a WAT fixture the way read-module would receive bytes: decode
the text to the IR and canonically re-encode it once, so BOTH backends
decode identical bytes (the WAT parse itself is not being measured)."
  (wasmdown:module-bytes (wasmdown.text:parse-wat text)))

(defun %measure (label bytes argument)
  "Measure MODULE (bytes) under both backends and return an alist."
  (let* ((t-decode (%median-of (lambda () (wasmdown:read-module bytes))))
         (module (wasmdown:read-module bytes))
         (t-validate (%median-of (lambda () (wasmdown:validate-module module))))
         (rows '()))
    (dolist (mode '(:interpret :compiled))
      (multiple-value-bind (t-instantiate consed-instantiate)
          (%median-of (lambda () (wasmdown:instantiate module :mode mode)))
        (let* ((inst (wasmdown:instantiate module :mode mode))
               (t-run (%median-of (lambda ()
                                    (wasmdown:invoke inst "run" argument))))
               (result (wasmdown:invoke inst "run" argument)))
          (push (list :mode mode
                      :instantiate_seconds (float t-instantiate 1d0)
                      :instantiate_consed consed-instantiate
                      :run_seconds (float t-run 1d0)
                      :result (format nil "~s" result))
                rows))))
    (list :fixture label
          :decode_seconds (float t-decode 1d0)
          :validate_seconds (float t-validate 1d0)
          :backends (nreverse rows))))

;;; ---------------------------------------------------------------
;;; Output.

(defun %ms (seconds)
  "Milliseconds, or microseconds with a us suffix for sub-millisecond
values (decode/validate of a tiny module is not 0 ms)."
  (let ((ms (* 1000.0d0 (float seconds 1d0))))
    (if (< ms 1.0d0)
        (format nil "~,1f us" (* 1000.0d0 ms))
        (format nil "~,2f ms" ms))))

(defun %print-row (row)
  (destructuring-bind (&key fixture decode_seconds validate_seconds backends) row
    (format t "~&~a  decode ~a  validate ~a~%"
            fixture (%ms decode_seconds) (%ms validate_seconds))
    (dolist (b backends)
      (format t "  ~a  instantiate ~a (~a MiB)  run ~a  => ~a~%"
              (getf b :mode)
              (%ms (getf b :instantiate_seconds))
              (format nil "~,1f"
                      (/ (float (or (getf b :instantiate_consed) 0) 1d0)
                         (* 1024d0 1024d0)))
              (%ms (getf b :run_seconds))
              (getf b :result)))))

(defun %report-md (rows)
  (with-output-to-string (s)
    (format s "# Backend cost bench~%~%")
    (format s "Generated by `make report-backends-bench` (tools/bench-backends.lisp).~%")
    (format s "Medians of ~d runs per measurement, same module bytes for both backends.~%~%"
            *repeat*)
    (format s "`:compiled` instantiate time INCLUDES compiling every defined function:~%")
    (format s "compilation is eager and happens before the start section, so it is a~%")
    (format s "one-shot cost per instantiation, not part of the run.~%~%")
    (format s "| Fixture | Backend | decode | validate | instantiate+compile | consed | run | result |~%")
    (format s "| --- | --- | --- | --- | --- | --- | --- | --- |~%")
    (dolist (row rows)
      (destructuring-bind (&key fixture decode_seconds validate_seconds backends) row
        (dolist (b backends)
          (format s "| ~a | ~a | ~a | ~a | ~a | ~a MiB | ~a | `~a` |~%"
                  fixture (getf b :mode)
                  (%ms decode_seconds) (%ms validate_seconds)
                  (%ms (getf b :instantiate_seconds))
                  (format nil "~,1f"
                          (/ (float (or (getf b :instantiate_consed) 0) 1d0)
                             (* 1024d0 1024d0)))
                  (%ms (getf b :run_seconds))
                  (getf b :result)))))
    (format s "~%Regenerate: `sbcl --script tools/bench-backends.lisp --commit`~%")))

(defun %json (rows)
  (with-output-to-string (s)
    (format s "{\"schema\":\"wasmdown.backend-bench.v1\",\"sbcl\":~s,\"repeat\":~d,\"fixtures\":["
            (lisp-implementation-version) *repeat*)
    (loop for row in rows
          for first = t then nil
          do (unless first (format s ","))
             (format s "{\"fixture\":~s,\"decode_s\":~,6f,\"validate_s\":~,6f,\"backends\":["
                     (getf row :fixture) (getf row :decode_seconds)
                     (getf row :validate_seconds))
             (loop for b in (getf row :backends)
                   for bfirst = t then nil
                   do (unless bfirst (format s ","))
                      (format s "{\"mode\":~s,\"instantiate_s\":~,6f,\"instantiate_consed\":~d,\"run_s\":~,6f,\"result\":~s}"
                              (getf b :mode)
                              (getf b :instantiate_seconds)
                              (or (getf b :instantiate_consed) 0)
                              (getf b :run_seconds)
                              (getf b :result)))
             (format s "]}"))
    (format s "]}")))

;;; ---------------------------------------------------------------
;;; Main.

(let ((rows '()))
  (dolist (k *kernels*)
    (destructuring-bind (label text argument) k
      (push (%measure label (%bytes-of text) argument) rows)))
  (when *wasm*
    (let ((bytes (with-open-file (s *wasm* :element-type '(unsigned-byte 8))
                   (let ((v (make-array (file-length s)
                                        :element-type '(unsigned-byte 8))))
                     (read-sequence v s) v))))
      ;; a real module: no exported "run", so measure instantiate only
      (let* ((module (wasmdown:read-module bytes))
             (_ (wasmdown:validate-module module))
             (t-interp (%median-of (lambda () (wasmdown:instantiate module))))
             (t-compiled (%median-of (lambda ()
                                       (wasmdown:instantiate module :mode :compiled)))))
        (declare (ignore _))
        (push (list :fixture (file-namestring *wasm*)
                    :decode_seconds 0d0
                    :validate_seconds 0d0
                    :backends (list (list :mode :interpret
                                          :instantiate_seconds (float t-interp 1d0)
                                          :run_seconds 0d0
                                          :result "instantiate only")
                                    (list :mode :compiled
                                          :instantiate_seconds (float t-compiled 1d0)
                                          :run_seconds 0d0
                                          :result "instantiate+compile only")))
              rows))))
  (setf rows (nreverse rows))
  (dolist (row rows) (%print-row row))
  (when *json-path*
    (with-open-file (s *json-path* :direction :output :if-exists :supersede)
      (write-string (%json rows) s))
    (format t "~&wrote ~a~%" *json-path*))
  (when *commit*
    (let ((out (merge-pathnames "docs/conformance/performance.md" *root*)))
      (ensure-directories-exist out)
      (with-open-file (s out :direction :output :if-exists :supersede)
        (write-string (%report-md rows) s))
      (format t "~&wrote ~a~%" out)))
  (sb-ext:exit :code 0))
