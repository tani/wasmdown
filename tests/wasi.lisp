;;;; M16 slice 1: the WASI provider pins -- the CL-level provider
;;;; implementations, the capability model, the host-provider
;;;; framework, and the end-to-end component fixture that imports
;;;; wasi:clocks/monotonic-clock@0.3.2 and calls it through a real
;;;; canon-lowered core import.

(defpackage #:wasmdown.test.wasi
  (:use #:cl #:wasmdown.test))

(in-package #:wasmdown.test.wasi)

(defun provider-fn (instance name)
  (wasmdown.wasi:provider-instance-function instance name))

(defun call-provider (instance name &rest args)
  (apply #'wasmdown.component.runtime:lifted-func-call
         (provider-fn instance name) args))

;;; ---------------------------------------------------------------
;;; wasi:random -- OS entropy only.

(test wasi/random-os-entropy
  ;; length + range
  (let ((bytes (wasmdown.wasi:os-random-bytes 64)))
    (is (= 64 (length bytes)))
    (is (every (lambda (b) (and (integerp b) (<= 0 b 255))) bytes)))
  ;; zero is an empty list, never a trap (the WIT's MUST)
  (is (null (wasmdown.wasi:random-bytes 0)))
  (is (null (wasmdown.wasi:insecure-random-bytes 0)))
  ;; the provider's short-read cap is a bound, and at least one byte
  ;; comes back for a positive request
  (let ((bytes (wasmdown.wasi:random-bytes 100000000)))
    (is (plusp (length bytes)))
    (is (<= (length bytes) wasmdown.wasi::*max-random-bytes*)))
  ;; u64 range
  (let ((u (wasmdown.wasi:random-u64)))
    (is (and (integerp u) (<= 0 u) (< u (ash 1 64))))
    (is (not (= u (wasmdown.wasi:random-u64)))))
  ;; the insecure-seed tuple
  (let ((seed (wasmdown.wasi:insecure-seed)))
    (is (= 2 (length seed)))
    (is (every (lambda (u) (and (integerp u) (<= 0 u) (< u (ash 1 64))))
               seed))))

;;; ---------------------------------------------------------------
;;; wasi:clocks.

(test wasi/clocks-monotonic
  (let ((a (wasmdown.wasi:monotonic-now))
        (b (wasmdown.wasi:monotonic-now)))
    (is (integerp a))
    (is (<= a b)))
  (is (plusp (wasmdown.wasi:monotonic-resolution))))

(test wasi/clocks-system-instant
  (let ((instant (wasmdown.wasi:system-now)))
    (is (= 2 (length instant)))
    (let ((seconds (cdr (assoc "seconds" instant :test #'string=)))
          (nanos (cdr (assoc "nanoseconds" instant :test #'string=))))
      (is (integerp seconds))
      (is (> seconds 1700000000))       ; after 2023-11-14
      (is (and (integerp nanos) (<= 0 nanos) (< nanos 1000000000)))))
  (is (plusp (wasmdown.wasi:system-resolution))))

;;; ---------------------------------------------------------------
;;; The capability model: everything comes from the context object.

(test wasi/capability-environment
  (let ((ctx-a (wasmdown.wasi:make-wasi-context
                :arguments '("prog" "-v")
                :environment '(("HOME" . "/root") ("LANG" . "C"))
                :cwd "/work"))
        (ctx-b (wasmdown.wasi:make-wasi-context
                :arguments '("other")
                :environment '(("HOME" . "/tmp"))
                :cwd nil)))
    (is (equal (wasmdown.wasi:environment-get-arguments ctx-a)
               '("prog" "-v")))
    (is (equal (wasmdown.wasi:environment-get-environment ctx-a)
               '(("HOME" "/root") ("LANG" "C"))))
    (is (equal (wasmdown.wasi:environment-get-initial-cwd ctx-a)
               '(:some . "/work")))
    (is (eq (wasmdown.wasi:environment-get-initial-cwd ctx-b) :none))
    ;; two differently-authorized contexts never see each other's state
    (is (equal (wasmdown.wasi:environment-get-environment ctx-b)
               '(("HOME" "/tmp"))))
    ;; the provider instances are built from the context object
    (let ((ia (wasmdown.wasi:cli-environment-instance ctx-a))
          (ib (wasmdown.wasi:cli-environment-instance ctx-b)))
      (is (equal (call-provider ia "get-arguments") '("prog" "-v")))
      (is (equal (call-provider ib "get-arguments") '("other")))
      (is (equal (call-provider ia "get-initial-cwd") '(:some . "/work")))
      (is (eq (call-provider ib "get-initial-cwd") :none)))))

(defun exit-status-of (thunk)
  "Run THUNK; the wasi-exit status it signals, or NIL if it returned."
  (block nil
    (handler-case (progn (funcall thunk) nil)
      (wasmdown.wasi:wasi-exit (c)
        (wasmdown.wasi:wasi-exit-status c)))))

(test wasi/exit
  (let ((exit (wasmdown.wasi:cli-exit-instance)))
    (is (eql 7 (exit-status-of
                (lambda () (call-provider exit "exit-with-code" 7)))))
    ;; exit(result): ok -> 0, err -> 1
    (is (eql 0 (exit-status-of
                (lambda () (call-provider exit "exit" '(:ok))))))
    (is (eql 1 (exit-status-of
                (lambda () (call-provider exit "exit" '(:err))))))
    ;; the condition is an error, so it unwinds the invocation
    (is (subtypep 'wasmdown.wasi:wasi-exit 'error))))

;;; ---------------------------------------------------------------
;;; The framework: a provider is a comp-instance of lifted host funcs
;;; with the WIT-derived functypes, keyed by the versioned import names.

(test wasi/provider-instance-shape
  (let* ((provider (wasmdown.wasi:clocks-provider))
         (names (mapcar #'car provider))
         (mono (cdr (assoc "wasi:clocks/monotonic-clock@0.3.2" provider
                           :test #'string=)))
         (sys (cdr (assoc "wasi:clocks/system-clock@0.3.2" provider
                          :test #'string=))))
    (is (member "wasi:clocks/types@0.3.2" names :test #'string=))
    (is (member "wasi:clocks/monotonic-clock@0.3.2" names :test #'string=))
    (is (member "wasi:clocks/system-clock@0.3.2" names :test #'string=))
    (is (wasmdown.component.runtime:comp-instance-p mono))
    ;; exported functions are the runtime's own lifted-func values
    (let ((now (provider-fn mono "now")))
      (is (wasmdown.component.runtime:lifted-func-p now))
      (let ((ct (wasmdown.component.runtime:lifted-func-type now)))
        (is (eq (wasmdown.component:ct-kind ct) :func))
        (is (eq (wasmdown.component:ct-kind
                 (second (wasmdown.component:ct-payload ct)))
                :prim))
        (is (eq (wasmdown.component:ct-payload
                 (second (wasmdown.component:ct-payload ct)))
                :u64))))
    ;; the type exports ride the same table (a consumer can alias them)
    (is (eq (car (gethash "duration"
                          (wasmdown.component.runtime:comp-instance-exports
                           (cdr (assoc "wasi:clocks/types@0.3.2" provider
                                       :test #'string=)))))
            :type))
    ;; a declared-but-unsupported function is exported and traps with
    ;; host-error when called
    (is (block nil
          (handler-case
              (progn (call-provider mono "wait-until" 0) :returned)
            (wasmdown:host-error (c)
              (search "not implemented" (wasmdown:wasmdown-error-message c))))))
    ;; direct host calls return the WIT domain values
    (is (integerp (call-provider mono "now")))
    (is (= 1 (call-provider mono "get-resolution")))
    (let ((instant (call-provider sys "now")))
      (is (string= (car (first instant)) "seconds"))
      (is (string= (car (second instant)) "nanoseconds")))))

;;; ---------------------------------------------------------------
;;; The end-to-end fixture: a real component text that imports the
;;; versioned interface instance, aliases its functions, canon-lowers
;;; them for a core module, and lifts a call back out. Supplied through
;;; instantiate-component's :with-args.

(defparameter *monotonic-fixture*
  "(component
     (import \"wasi:clocks/monotonic-clock@0.3.2\"
       (instance $clk (export \"now\" (func (result u64)))
                      (export \"get-resolution\" (func (result u64)))))
     (alias export $clk \"now\" (func $now))
     (alias export $clk \"get-resolution\" (func $res))
     (core func $now-lowered (canon lower (func $now)))
     (core func $res-lowered (canon lower (func $res)))
     (core module $M
       (import \"clk\" \"now\" (func $n (result i64)))
       (import \"clk\" \"res\" (func $r (result i64)))
       (func (export \"now\") (result i64) (call $n))
       (func (export \"res\") (result i64) (call $r)))
     (core instance $m
       (instantiate $M
         (with \"clk\"
           (instance (export \"now\" (func $now-lowered))
                     (export \"res\" (func $res-lowered))))))
     (func (export \"now\") (result u64)
       (canon lift (core func $m \"now\")))
     (func (export \"res\") (result u64)
       (canon lift (core func $m \"res\"))))")

(test wasi/e2e-monotonic-clock-component
  (let* ((comp (wasmdown.component.text::parse-component-text
                *monotonic-fixture*))
         (provider (wasmdown.wasi:clocks-provider)))
    (wasmdown.component:validate-component comp)
    (let ((inst (wasmdown.component.runtime:instantiate-component
                 comp :with-args provider)))
      (let* ((now-fn (wasmdown.component.runtime:runtime-export-value
                      inst "now"))
             (res-fn (wasmdown.component.runtime:runtime-export-value
                      inst "res")))
        (is (wasmdown.component.runtime:lifted-func-p now-fn))
        (let ((a (wasmdown.component.runtime:lifted-func-call now-fn))
              (b (wasmdown.component.runtime:lifted-func-call now-fn)))
          (is (integerp a))
          (is (plusp a))
          ;; the host clock is monotonic through the whole call path
          (is (<= a b)))
        (is (plusp (wasmdown.component.runtime:lifted-func-call res-fn)))))))
