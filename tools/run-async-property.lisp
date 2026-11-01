;;;; tools/run-async-property.lisp — runner for the deterministic async
;;;; property lane (tests/async-property.lisp; docs/design/PLAN.md §9.3/F4).
;;;;
;;;; A GATE: exits non-zero when any invariant is violated. Every run is
;;;; driven by explicit integer seeds; the same seeds reproduce the same
;;;; operation sequence (xorshift64* in tests/async-property.lisp), and no
;;;; wall-clock sleep is used by the lane.
;;;;
;;;;   sbcl --script tools/run-async-property.lisp
;;;;   sbcl --script tools/run-async-property.lisp --seeds 7,11 --repeats 5
;;;;   sbcl --script tools/run-async-property.lisp --seed 7 --verbose
;;;;   sbcl --script tools/run-async-property.lisp --negative-control
;;;;
;;;; Make targets: `make test-async-property` (default seeds x repeats)
;;;; and `make test-async-property-negative` (the injected-violation arm,
;;;; which must FAIL the checkers; the runner then exits zero because the
;;;; detector worked).

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)
(asdf:load-system :wasmdown)
(load (merge-pathnames "tests/async-property" *root*))

(defun parse-seeds (text)
  (handler-case
      (let ((seeds (mapcar (lambda (piece)
                             (parse-integer piece :junk-allowed nil))
                           (uiop:split-string text :separator ","))))
        (unless (and seeds (every (lambda (s) (plusp s)) seeds))
          (error "seeds must be positive integers"))
        seeds)
    (error (c)
      (format *error-output* "bad --seeds value ~s: ~a~%" text c)
      (sb-ext:exit :code 2))))

(defun main (&key seeds (repeats 3) verbose negative-control)
  (let ((wasmdown.component.runtime::*ap-verbose* verbose))
    ;; The lane's functions live in the runtime package so they can call
    ;; the internals under test directly.
    (multiple-value-bind (checks failures)
        (wasmdown.component.runtime::run-async-property-lane
         :seeds seeds :repeats repeats :verbose verbose
         :negative-control negative-control)
      (declare (ignore checks))
      (cond
        (negative-control
         (if failures
             (progn
               (format *error-output* "~&async-property negative control FAILED: ~{~a~^; ~}~%"
                       failures)
               (sb-ext:exit :code 1))
             (progn
               (format t "~&async-property negative control: PASS (~d checker invocation)~%"
                       1)
               (sb-ext:exit :code 0))))
        ((null failures)
         (format t "~&async-property: GATE GREEN~%")
         (sb-ext:exit :code 0))
        (t
         (format *error-output* "~&async-property: GATE RED — ~d invariant failure(s)~%"
                 (length failures))
         (dolist (failure failures)
           (format *error-output* "  ~a~%" failure))
         (sb-ext:exit :code 1))))))

(let ((args (rest sb-ext:*posix-argv*))
      (seeds '(1 2 3 5 8 13 21 34))
      (repeats 3)
      (verbose nil)
      (negative-control nil))
  (loop while args
        for arg = (pop args)
        do (cond
             ((string= arg "--seeds")
              (unless args
                (format *error-output* "--seeds needs a comma-separated list~%")
                (sb-ext:exit :code 2))
              (setf seeds (parse-seeds (pop args))))
             ((string= arg "--seed")
              (unless args
                (format *error-output* "--seed needs an integer~%")
                (sb-ext:exit :code 2))
              (setf seeds (list (parse-integer (pop args)))))
             ((string= arg "--repeats")
              (unless args
                (format *error-output* "--repeats needs an integer~%")
                (sb-ext:exit :code 2))
              (setf repeats (parse-integer (pop args))))
             ((string= arg "--negative-control")
              (setf negative-control t))
             ((string= arg "--verbose")
              (setf verbose t))
             ((member arg '("-h" "--help") :test #'string=)
              (format t "usage: run-async-property.lisp [--seeds a,b,c] [--seed n] [--repeats n] [--negative-control] [--verbose]~%")
              (format t "default seeds: ~{~a~^ ~}; default repeats: 3~%" seeds)
              (sb-ext:exit :code 0))
             (t
              (format *error-output* "unknown argument: ~a~%" arg)
              (sb-ext:exit :code 2))))
  (main :seeds seeds :repeats repeats :verbose verbose
        :negative-control negative-control))
