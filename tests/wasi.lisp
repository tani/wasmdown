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

;;; ===============================================================
;;; M16 SLICE 2 — THE HOST STREAM/FUTURE ABI + wasi:cli stdio.
;;; ===============================================================

(defun rt (name)
  "The runtime's internal function/accessor NAME (the host-end
machinery is deliberately not part of the public surface)."
  (or (find-symbol name (find-package '#:wasmdown.component.runtime))
      (error "no runtime symbol ~a" name)))

(defmacro rtcall (name &rest args)
  `(funcall (rt ,(string name)) ,@args))

(defparameter *u8-ct*
  (wasmdown.component:make-ct :kind :prim :payload :u8))

(defun %completed (n) (logior 0 (ash n 4)))

;;; ---------------------------------------------------------------
;;; The host-backed ENDS themselves (a CL-only round trip: no guest
;;; memory, no guest instance — the copy protocol's case 2 consults
;;; the host backing directly).

(test wasi/host-sink-end-roundtrip
  ;; The guest's writable end stages 2 elements; its peer is a HOST
  ;; readable end whose SINK collects them. The write completes with
  ;; COMPLETED|2<<4 — the canonical stream protocol, host as the peer.
  (let ((sink-bytes '()))
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
      (wasmdown.component.runtime:host-end-set-sink
       reader (lambda (bytes) (setf sink-bytes bytes) nil))
      (let ((staged (wasmdown.component.runtime:make-host-buffer
                     2 :values '(104 105))))
        ;; the guest's write (case 2: the peer has no buffer)
        (rtcall "%END-COPY" writer staged nil)
        (is (equal '(104 105) sink-bytes))
        (is (eql (%completed 2)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer))))
        ;; the stream write end returns to idle after delivery
        (is (eq :idle (rtcall "ASYNC-END-STATE" writer)))))))

(test wasi/host-source-end-eof-dropped
  ;; The guest's readable end pulls from a HOST writable end's SOURCE:
  ;; the bytes arrive COMPLETED|n<<4; the source's EOF drops the host
  ;; writer, so the next read is DROPPED (the canonical 1).
  (multiple-value-bind (reader writer)
      (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
    (let ((pulls 0))
      (wasmdown.component.runtime:host-end-set-source
       writer (lambda (max)
                (incf pulls)
                (if (= pulls 1)
                    (values '(1 2 3) t)     ; bytes AND eof
                    (values nil t))))
      (let ((buf (wasmdown.component.runtime:make-host-buffer 8)))
        (rtcall "%END-COPY" reader buf t)
        (is (equal '(1 2 3)
                   (wasmdown.component.runtime:host-buffer-values buf)))
        (is (eql (%completed 3)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader)))))
      (let ((buf (wasmdown.component.runtime:make-host-buffer 8)))
        (rtcall "%END-COPY" reader buf t)
        ;; DROPPED == 1 (copy result 1, no progress for a stream)
        (is (eql 1 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader))))))))

(test wasi/host-future-resolve
  ;; A host future: the provider resolves the WRITABLE end; the guest's
  ;; READABLE read delivers COMPLETED (0) with the value at its buffer.
  (let ((fct (wasmdown.component:make-ct
              :kind :result
              :payload (cons nil (wasmdown.component:make-ct
                                  :kind :enum
                                  :payload (vector "io" "pipe"))))))
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-future-pair fct)
      (wasmdown.component.runtime:host-future-resolve writer '(:ok))
      (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
        (rtcall "%END-COPY" reader buf t)
        (is (equal '((:ok))
                   (wasmdown.component.runtime:host-buffer-values buf)))
        (is (eql 0 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader))))))
    ;; the error case rides the same slot
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-future-pair fct)
      (wasmdown.component.runtime:host-future-resolve writer '(:err . "io"))
      (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
        (rtcall "%END-COPY" reader buf t)
        (is (equal '((:err . "io"))
                   (wasmdown.component.runtime:host-buffer-values buf)))))))

;;; ---------------------------------------------------------------
;;; wasi:cli stdio over the CONTEXT's CL streams (no ambient globals).

(test wasi/cli-stdout-write-via-stream
  (let* ((sink (make-string-output-stream))
         (ctx (wasmdown.wasi:make-wasi-context :stdout sink))
         (inst (wasmdown.wasi:cli-stdout-instance ctx))
         (fn (provider-fn inst "write-via-stream")))
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
      ;; the guest writes BEFORE handing the end over: the provider's
      ;; registration drains the parked buffer
      (rtcall "%END-COPY" writer
              (wasmdown.component.runtime:make-host-buffer
               3 :values '(104 105 33))
              nil)
      (let ((future (wasmdown.component.runtime:lifted-func-call fn reader)))
        (is (wasmdown.component.runtime:async-end-p future))
        (is (string= "hi!" (get-output-stream-string sink)))
        (is (eql (%completed 3)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer))))
        ;; a second write after registration drains straight through
        (rtcall "%END-COPY" writer
                (wasmdown.component.runtime:make-host-buffer
                 2 :values '(10 10))
                nil)
        (is (string= (format nil "~c~c" #\Newline #\Newline)
                     (get-output-stream-string sink)))
        ;; dropping the guest's writable end resolves the future
        (rtcall "%END-DROP" writer)
        (let ((fbuf (wasmdown.component.runtime:make-host-buffer 1)))
          (rtcall "%END-COPY" future fbuf t)
          (is (equal '((:ok))
                     (wasmdown.component.runtime:host-buffer-values fbuf)))
          (is (eql 0 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" future)))))))))

(test wasi/cli-stderr-write-via-stream
  (let* ((sink (make-string-output-stream))
         (ctx (wasmdown.wasi:make-wasi-context :stderr sink))
         (inst (wasmdown.wasi:cli-stderr-instance ctx))
         (fn (provider-fn inst "write-via-stream")))
    (multiple-value-bind (reader writer)
        (wasmdown.component.runtime:make-host-stream-pair *u8-ct*)
      (wasmdown.component.runtime:lifted-func-call fn reader)
      (rtcall "%END-COPY" writer
              (wasmdown.component.runtime:make-host-buffer 2 :values '(111 107))
              nil)
      (is (string= "ok" (get-output-stream-string sink))))))

(test wasi/cli-stdin-read-via-stream
  (let* ((source (make-string-input-stream "hello"))
         (ctx (wasmdown.wasi:make-wasi-context :stdin source))
         (inst (wasmdown.wasi:cli-stdin-instance ctx))
         (fn (provider-fn inst "read-via-stream")))
    (let* ((pair (wasmdown.component.runtime:lifted-func-call fn))
           (stream (first pair))
           (future (second pair)))
      (is (= 2 (length pair)))
      (is (wasmdown.component.runtime:async-end-p stream))
      (is (wasmdown.component.runtime:async-end-p future))
      ;; the guest reads the whole source
      (let ((buf (wasmdown.component.runtime:make-host-buffer 5)))
        (rtcall "%END-COPY" stream buf t)
        (is (string= "hello"
                     (coerce (mapcar #'code-char
                                     (wasmdown.component.runtime:host-buffer-values
                                      buf))
                             'string)))
        (is (eql (%completed 5)
                 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" stream)))))
      ;; the source is exhausted: the next read is DROPPED
      (let ((buf (wasmdown.component.runtime:make-host-buffer 5)))
        (rtcall "%END-COPY" stream buf t)
        (is (eql 1 (third (rtcall "%WAITABLE-GET-PENDING-EVENT" stream)))))
      ;; the read result future is already resolved (success)
      (let ((fbuf (wasmdown.component.runtime:make-host-buffer 1)))
        (rtcall "%END-COPY" future fbuf t)
        (is (equal '((:ok))
                   (wasmdown.component.runtime:host-buffer-values fbuf)))))))

(test wasi/cli-stdio-capability-withdrawn
  ;; An absent stream is a withdrawn capability: the interface is still
  ;; exported, but calling it traps with host-error (never an ambient
  ;; *STANDARD-OUTPUT*).
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (out (wasmdown.wasi:cli-stdout-instance ctx))
         (in (wasmdown.wasi:cli-stdin-instance ctx)))
    (is (block nil
          (handler-case
              (progn (call-provider out "write-via-stream" :not-an-end) :returned)
            (wasmdown:host-error (c)
              (search "capability" (wasmdown:wasmdown-error-message c))))))
    (is (block nil
          (handler-case
              (progn (call-provider in "read-via-stream") :returned)
            (wasmdown:host-error (c)
              (search "capability" (wasmdown:wasmdown-error-message c)))))))
  ;; the provider surfaces are keyed by the versioned WIT import names
  (let ((names (mapcar #'car
                       (wasmdown.wasi:cli-provider
                        (wasmdown.wasi:make-wasi-context)))))
    (is (member "wasi:cli/stdin@0.3.2" names :test #'string=))
    (is (member "wasi:cli/stdout@0.3.2" names :test #'string=))
    (is (member "wasi:cli/stderr@0.3.2" names :test #'string=))
    (is (member "wasi:cli/environment@0.3.2" names :test #'string=))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END FIXTURE: a component imports wasi:cli/stdout@0.3.2,
;;; canon-lowers write-via-stream, hands it its stream's readable end,
;;; writes through its own stream, drops the writable end and reads the
;;; result future. The bytes land in the CONTEXT's sink.

(defparameter *stdout-fixture*
  (concatenate 'string
   "(component"
   " (type $ERR (enum \"io\" \"illegal-byte-sequence\" \"pipe\"))"
   " (type $ST (stream u8))"
   " (import \"wasi:cli/stdout@0.3.2\""
   "   (instance $out"
   "     (export \"error-code\" (type (eq $ERR)))"
   "     (type $r (result (error $ERR)))"
   "     (type $f (future $r))"
   "     (type $s (stream u8))"
   "     (export \"write-via-stream\""
   "       (func (param \"data\" $s) (result $f)))))"
   " (alias export $out \"write-via-stream\" (func $wvs))"
   " (core func $wvs-lowered (canon lower (func $wvs)))"
   " (core module $Memory (memory (export \"mem\") 1))"
   " (core instance $memory (instantiate $Memory))"
   " (core module $M"
   "  (import \"env\" \"mem\" (memory 1))"
   "  (import \"env\" \"stream.new\" (func $snew (result i64)))"
   "  (import \"env\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))"
   "  (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))"
   "  (import \"env\" \"future.read\" (func $fread (param i32 i32) (result i32)))"
   "  (import \"env\" \"write-via-stream\" (func $wvs (param i32) (result i32)))"
   "  (global $fut (mut i32) (i32.const 0))"
   "  (func (export \"f\") (result i32)"
   "    (local $p i64) (local $rx i32) (local $tx i32) (local $ret i32)"
   "    (local.set $p (call $snew))"
   "    (local.set $rx (i32.wrap_i64 (local.get $p)))"
   "    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $p) (i64.const 32))))"
   "    (i32.store16 (i32.const 16) (i32.const 0x6968))"
   "    (global.set $fut (call $wvs (local.get $rx)))"
   "    (local.set $ret (call $swrite (local.get $tx) (i32.const 16) (i32.const 2)))"
   "    (if (i32.ne (i32.const 32) (local.get $ret)) (then unreachable))"
   "    (call $sdropw (local.get $tx))"
   "    (local.set $ret (call $fread (global.get $fut) (i32.const 32)))"
   "    (if (i32.ne (i32.const 0) (local.get $ret)) (then unreachable))"
   "    (i32.load8_u (i32.const 32)))"
   " )"
   " (type $FT (future (result (error $ERR))))"
   " (canon stream.new $ST (core func $snew))"
   " (canon stream.write $ST (memory (core memory $memory \"mem\")) (core func $swrite))"
   " (canon stream.drop-writable $ST (core func $sdropw))"
   " (canon future.read $FT (memory (core memory $memory \"mem\")) (core func $fread))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $memory \"mem\"))"
   "   (export \"stream.new\" (func $snew))"
   "   (export \"stream.write\" (func $swrite))"
   "   (export \"stream.drop-writable\" (func $sdropw))"
   "   (export \"future.read\" (func $fread))"
   "   (export \"write-via-stream\" (func $wvs-lowered))))))"
   " (func (export \"f\") (result u32) (canon lift (core func $m \"f\"))))"))

(test wasi/e2e-stdout-component
  (let* ((sink (make-string-output-stream))
         (context (wasmdown.wasi:make-wasi-context :stdout sink))
         (comp (wasmdown.component.text::parse-component-text
                *stdout-fixture*)))
    (wasmdown.component:validate-component comp)
    (let* ((inst (wasmdown.component.runtime:instantiate-component
                  comp :with-args (wasmdown.wasi:cli-provider context)))
           (f (wasmdown.component.runtime:runtime-export-value inst "f")))
      (is (wasmdown.component.runtime:lifted-func-p f))
      ;; the guest's 2 stream bytes ("hi") reached the context's sink
      ;; and the result future's ok discriminant is 0
      (is (eql 0 (wasmdown.component.runtime:lifted-func-call f)))
      (is (string= "hi" (get-output-stream-string sink)))
      ;; a second invocation sees a fresh, independently-authorized run
      (is (eql 0 (wasmdown.component.runtime:lifted-func-call f)))
      (is (string= "hi" (get-output-stream-string sink))))))
