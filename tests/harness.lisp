;;;; Minimal test harness: deterministic, zero dependencies (PLAN.md §12).

(defpackage #:wasmdown.test
  (:use #:cl)
  (:export #:test #:is #:run-all-tests #:*tests* #:check-trap
           #:assert-trap #:assert-raises #:run-one))

(in-package #:wasmdown.test)

(defvar *tests* '())
(defvar *verbose* t)

(defstruct test-entry name fn)

(defmacro test (name &body body)
  "Register a named test thunk. Runs during RUN-ALL-TESTS."
  `(eval-when (:load-toplevel :execute)
     (push (make-test-entry :name ',name :fn (lambda () ,@body)) *tests*)
     nil))

(defun run-all-tests (&key ((:verbose *verbose*) *verbose*))
  (let ((passed 0) (failed 0) (failures '()))
    (dolist (entry (reverse *tests*))
      (let ((ok nil) (err nil))
        (block nil
          (handler-bind ((error (lambda (c)
                                  (setf err c)
                                  (return))))
            (funcall (test-entry-fn entry))
            (setf ok t)))
        (if ok
            (incf passed)
            (progn
              (incf failed)
              (push (list (test-entry-name entry) err) failures)))
        (when *verbose*
          (format t "~&~a ... ~a~%" (test-entry-name entry)
                  (if ok "PASS" (if err (format nil "FAIL: ~a" err) "FAIL"))))))
    (format t "~&--- ~d passed, ~d failed ---~%" passed failed)
    (values passed failed (reverse failures))))

(defun check-trap (thunk expected-kind)
  "THUNK must signal wasmdown:wasm-trap with kind EXPECTED-KIND; returns the condition or NIL."
  (block nil
    (handler-bind ((wasmdown:wasm-trap
                     (lambda (c)
                       (return (and (eq (wasmdown:wasm-trap-kind c) expected-kind) c))))
                   (error (lambda (c) (declare (ignore c)) (return nil))))
      (funcall thunk)
      nil)))

(defmacro assert-trap (kind &body body)
  `(let ((c (check-trap (lambda () ,@body) ,kind)))
     (unless c (error "expected trap of kind ~a" ,kind))
     c))

(defmacro assert-raises (type &body body)
  `(block nil
     (handler-bind ((,type (lambda (c) (declare (ignore c)) (return t)))
                   (error (lambda (c)
                            (declare (ignore c))
                            (return (error "wrong condition type from ~s" ',body)))))
       ,@body
       (error "expected condition ~a from ~s" ',type ',body))))
(defmacro is (form)
  `(unless ,form
     (error "assertion failed: ~s" ',form)))
