;;;; Compile-cost microbench: the cost of compiling N generated funcs.
;;;;
;;;; This is the harness a compile-time/heap reduction must move. Each
;;;; function is a small loop with locals, branches and memory ops, so
;;;; the number is dominated by SBCL's compilation of one emitted lambda,
;;;; not by decoding (which is measured separately by bench-backends).
;;;;
;;;; Usage:
;;;;   sbcl --script tools/compile-cost.lisp
;;;;   sbcl --script tools/compile-cost.lisp --functions 300 --repeat 5
;;;;
;;;; Recorded experiment (2026-11-01, SBCL on arm64): adding
;;;; `(debug 0) (space 3)` to the emitted lambda's optimize declaration
;;;; changed 300 functions from 0.20 s / 212.2 MiB consed to
;;;; 0.19 s / 209.6 MiB (-1.2 % consing). The policy lever is therefore
;;;; measured and rejected; the reduction work is in the emitted form
;;;; itself (temp-slot count / tagbody size), not in SBCL policies.

(require :asdf)
(defparameter *root* #P"/home/tani/ghq/github.com/tani/wasmdown/")
(pushnew *root* asdf:*central-registry* :test #'equal)
(asdf:load-system :wasmdown)

(defun gen-wat (n)
  (let ((body "(func (export \"f~d\") (param $n i32) (result i32)
    (local $i i32) (local $acc i32)
    (block $done
      (loop $l
        (br_if $done (i32.ge_s (local.get $i) (local.get $n)))
        (local.set $acc
          (i32.add (local.get $acc)
                   (i32.mul (local.get $i) (i32.const ~d))))
        (if (i32.eq (i32.and (local.get $i) (i32.const 7)) (i32.const 0))
            (then (i32.store (i32.const 0) (local.get $acc))))
        (i32.store8 (i32.const 4)
                    (i32.load (i32.const 0)))
        (local.set $i (i32.add (local.get $i) (i32.const 1)))
        (br $l)))
    (local.get $acc))"))
    (with-output-to-string (s)
      (format s "(module~%  (memory 1)~%")
      (dotimes (i n)
        (format s "  ~a~%"
                (format nil body i (1+ (mod i 5)))))
      (format s ")~%"))))

(defun measure (n repeats)
  (let* ((bytes (wasmdown.binary:encode-module
                 (wasmdown.text:parse-wat (gen-wat n))))
         (module (wasmdown.binary:decode-module bytes)))
    (format t "~&~d functions, ~d bytes~%" n (length bytes))
    (loop repeat repeats
          do (sb-ext:gc :full t)
             (let ((c0 (sb-ext:get-bytes-consed))
                   (t0 (get-internal-real-time)))
               (wasmdown:instantiate module :mode :compiled)
               (let ((dt (/ (- (get-internal-real-time) t0)
                            (float internal-time-units-per-second)))
                     (dc (- (sb-ext:get-bytes-consed) c0)))
                 (format t "  compile ~,2fs  consed ~,1f MiB  (~,1f ms/func)~%"
                         dt (/ dc (expt 2. 20)) (/ (* dt 1000) n)))))))

(defparameter *argv* (rest sb-ext:*posix-argv*))
(defun option (flag default)
  (let ((pos (position flag *argv* :test #'string=)))
    (if (and pos (< (1+ pos) (length *argv*))) (nth (1+ pos) *argv*) default)))

(measure (parse-integer (option "--functions" "300"))
         (parse-integer (option "--repeat" "3")))
