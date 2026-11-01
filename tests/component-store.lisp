;;;; Step 2 — the component STORE as an explicit ownership object.
;;;;
;;;; The store is the boundary of one instantiation graph: a top-level
;;;; graph gets exactly one, nested instances inherit it, a nested
;;;; graph may not be split across two, and teardown names its store.

(defpackage #:wasmdown.test.component-store
  (:use #:cl #:wasmdown.test))

(in-package #:wasmdown.test.component-store)

(defun rt (name)
  (or (find-symbol name '#:wasmdown.component.runtime)
      (error "no runtime symbol ~a" name)))

(defmacro rtsym (name)
  `(rt ,(string name)))

(test component-store/make-and-attach
  (let ((store (wasmdown.component.runtime:make-component-store)))
    (is (wasmdown.component.runtime:component-store-p store))
    (is (wasmdown.component.runtime::host-store-p store))
    ;; a reactor is installed at creation time, so it is already
    ;; available to a guest start section
    (let ((reactor (wasmdown.wasi:make-wasi-reactor :wake-p nil)))
      (let ((s2 (wasmdown.component.runtime:make-component-store :reactor reactor)))
        (is (eq reactor (wasmdown.component.runtime::host-store-reactor s2)))))))

(test component-store/instantiate-joins-explicit-store
  (let* ((store (wasmdown.component.runtime:make-component-store))
         ;; the smallest valid component: an empty header-only component
         ;; is not decodable here, so use a core-free component text
         (comp (funcall (find-symbol "PARSE-COMPONENT-TEXT" '#:wasmdown.component.text)
                        "(component)")))
    (let ((inst (wasmdown.component.runtime:instantiate-component
                 comp :store store)))
      (is (eq store (wasmdown.component.runtime:component-instance-store inst)))
      ;; a second, unbound instantiation gets a DIFFERENT store
      (let ((inst2 (wasmdown.component.runtime:instantiate-component comp)))
        (is (not (eq (wasmdown.component.runtime:component-instance-store inst2)
                     store)))))))

(test component-store/cross-store-is-link-error
  (let* ((a (wasmdown.component.runtime:make-component-store))
         (b (wasmdown.component.runtime:make-component-store))
         (comp (funcall (find-symbol "PARSE-COMPONENT-TEXT" '#:wasmdown.component.text)
                        "(component)"))
         (outer-inst (wasmdown.component.runtime:instantiate-component
                      comp :store a)))
    ;; OUTER names an innermost-first chain: its ROOT already owns A,
    ;; so asking for B must be a link error, never a silent second store
    (is (handler-case
            (progn (wasmdown.component.runtime:instantiate-component
                    comp :outer (list outer-inst) :store b)
                   nil)
          (wasmdown:link-error (c)
            (declare (ignore c))
            t)))))

(test component-store/teardown-names-its-store
  ;; an argumentless teardown requires a BOUND store
  (is (handler-case (progn (wasmdown.component.runtime:store-teardown) nil)
        (wasmdown:host-error () t)))
  ;; with a store argument it runs and returns that store
  (let ((store (wasmdown.component.runtime:make-component-store)))
    (is (eq store (wasmdown.component.runtime:store-teardown store))))
  ;; a non-store argument is rejected
  (is (handler-case (progn (wasmdown.component.runtime:store-teardown 42) nil)
        (wasmdown:host-error () t))))

(test component-store/call-with-binds-and-closes
  (let ((store (wasmdown.component.runtime:make-component-store))
        (seen nil)
        (closed nil))
    (wasmdown.component.runtime:call-with-component-store (store)
      (setf seen (eq store wasmdown.component.runtime::*current-component-store*)))
    (is seen)
    ;; on exit the binding is gone again
    (is (null wasmdown.component.runtime::*current-component-store*))
    ;; CLOSE-P NIL leaves the store to the caller (a nested helper must
    ;; never close a caller-owned store) — observable as no error and
    ;; the store still usable afterwards
    (wasmdown.component.runtime:call-with-component-store (store :close-p nil)
      (setf closed t))
    (is closed)
    (is (wasmdown.component.runtime:component-store-p store))))

(test component-store/script-run-hook-binds-one-store
  ;; the .wast driver runs a whole script inside ONE fresh store
  (let ((hook wasmdown.wast::*store-run-hook*)
        (inner nil))
    (is (functionp hook))
    (funcall hook (lambda ()
                    (setf inner wasmdown.component.runtime::*current-component-store*)))
    (is (wasmdown.component.runtime:component-store-p inner))
    ;; and the binding does not outlive the script
    (is (null wasmdown.component.runtime::*current-component-store*))))

;;; ---------------------------------------------------------------
;;; Scheduler state is the store's, not the process's (step 2).

(test component-store/scheduler-state-is-per-store
  ;; the registry and the execution right live in the store: two
  ;; stores never see each other's threads or ticket, and a teardown
  ;; of one leaves the other intact.
  (let* ((a (wasmdown.component.runtime:make-component-store))
         (b (wasmdown.component.runtime:make-component-store))
         (th (wasmdown.component.runtime::make-async-thread
              :task (wasmdown.component.runtime::make-async-task
                     :seq '(1)))))
    (let ((wasmdown.component.runtime::*current-component-store* a))
      (push th (wasmdown.component.runtime:component-store-threads a))
      (setf (wasmdown.component.runtime:component-store-right-ticket a) th)
      (is (eq th (first (wasmdown.component.runtime:component-store-threads
                         a)))))
    (is (null (wasmdown.component.runtime:component-store-threads b)))
    (is (null (wasmdown.component.runtime:component-store-right-ticket b)))
    ;; A's default-store view (a thread with no binding) is the
    ;; fallback store, NOT A: nothing leaks across the boundary
    (is (null (wasmdown.component.runtime:component-store-threads
               (wasmdown.component.runtime::%cs))))
    ;; teardown of B does not disturb A
    (let ((wasmdown.component.runtime::*current-component-store* b))
      (wasmdown.component.runtime:store-teardown b))
    (is (eq th (first (wasmdown.component.runtime:component-store-threads
                       a))))))

