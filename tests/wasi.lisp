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

;;; ===============================================================
;;; M16 SLICE 3a — wasi:filesystem: the capability model, the
;;; descriptor resource, and the non-stream surface.
;;; ===============================================================

;;; ---------------------------------------------------------------
;;; Fixtures: a private tree under /tmp, and the cleanup that removes
;;; exactly the entries the tests create (files, symlinks, then
;;; directories deepest-first) — no shell, no third-party library.

(defun fs-p (dir name) (format nil "~a/~a" dir name))

(defun fs-mkdir! (path &optional (mode #o755))
  (sb-unix:unix-mkdir path mode))

(defun fs-symlink! (target path)
  "A HOST-side symlink (the test plays the part of an outside
process planting a link inside the root)."
  (wasmdown.wasi::%libc-symlinkat target -100 path))

(defun fs-write! (path content)
  (with-open-file (s path :direction :output :if-exists :overwrite
                          :if-does-not-exist :create)
    (write-string content s))
  path)

(defun fs-poke-bytes! (path n)
  "Create PATH with N zero bytes (open-at/set-size fixtures)."
  (with-open-file (s path :direction :output :if-exists :overwrite
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (dotimes (i n) (write-byte 0 s)))
  path)

(defun fs-test-unlink-tree (root &optional names)
  "Remove NAMES under ROOT (files/symlinks/dirs) and then ROOT."
  (dolist (n names)
    (let ((p (fs-p root n)))
      (ignore-errors (wasmdown.wasi::%libc-unlinkat -100 p 0))
      (ignore-errors
        (wasmdown.wasi::%libc-unlinkat -100 p wasmdown.wasi::+at-removedir+))))
  (ignore-errors
    (wasmdown.wasi::%libc-unlinkat -100 root wasmdown.wasi::+at-removedir+)))

(defun fs-test-root (tag)
  (let ((dir (format nil "/tmp/wasmdown-~a-~d-~d"
                     tag (sb-unix:unix-getpid)
                     (random 1000000))))
    (unless (fs-mkdir! dir)
      (error "cannot create fixture root ~a" dir))
    dir))

(defmacro with-fs-root ((root-var tag &optional (names nil)) &body body)
  "Run BODY with ROOT-VAR bound to a fresh private tree; the cleanup
removes NAMES (an evaluated list, so callers quote it) and the root."
  `(let ((,root-var (fs-test-root ,tag)))
     (unwind-protect (progn ,@body)
       (fs-test-unlink-tree ,root-var ,names))))

(defun fs-error-code-of (form)
  "FORM must be a (:err . code) result; its code label."
  (unless (and (consp form) (eq (car form) :err))
    (error "expected an error result, got ~s" form))
  (cdr form))

(defun fs-ok-value (form)
  (unless (and (consp form) (eq (car form) :ok))
    (error "expected an ok result, got ~s" form))
  (cdr form))

(defun fs-type-name (stat)
  "The descriptor-type case name of a descriptor-stat domain value."
  (second (assoc "%type" stat :test #'string=)))

(defmacro => (expected actual)
  "Assert (equal EXPECTED ACTUAL) with a useful failure datum."
  `(let ((e ,expected) (a ,actual))
     (unless (equal e a)
       (error "expected ~s, got ~s" e a))))

;;; ---------------------------------------------------------------
;;; The capability object: an OPEN HOST DIRECTORY HANDLE (never a
;;; path prefix), and the operations that go through it.

(test wasi/fs-preopen-capability
  (with-fs-root (root "fs-cap" '("sub" "sub/inner" "a.txt" "lnk"))
    (fs-mkdir! (fs-p root "sub"))
    (fs-mkdir! (fs-p root "sub/inner"))
    (fs-write! (fs-p root "a.txt") "hello")
    (fs-symlink! "a.txt" (fs-p root "lnk"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; the authority IS a real directory handle + a guest name
             (is (plusp (wasmdown.wasi:wasi-preopen-fd pre)))
             (is (string= "/sandbox" (wasmdown.wasi:wasi-preopen-name pre)))
             (is (eq d (wasmdown.wasi:wasi-preopen-descriptor pre)))
             (is (wasmdown.wasi:wasi-descriptor-p d))
             ;; stat of the root: a directory, with the mutate right
             (let ((stat (fs-ok-value (wasmdown.wasi:fs-stat ctx d))))
               (=> "directory" (fs-type-name stat))
               (is (integerp (cdr (assoc "size" stat :test #'string=))))
               (is (member "mutate-directory"
                           (cdr (fs-ok-value (wasmdown.wasi:fs-get-flags ctx d)))
                           :test #'string=)))
             ;; get-type / get-flags
             (=> "directory" (car (fs-ok-value (wasmdown.wasi:fs-get-type ctx d))))
             ;; stat-at: a file, a directory, a symlink (no-follow default)
             (let ((file (fs-ok-value (wasmdown.wasi:fs-stat-at ctx d nil "a.txt"))))
               (=> "regular-file" (fs-type-name file))
               (=> 5 (cdr (assoc "size" file :test #'string=))))
             (=> "directory"
                 (fs-type-name (fs-ok-value (wasmdown.wasi:fs-stat-at ctx d nil "sub"))))
             (=> "symbolic-link"
                 (fs-type-name (fs-ok-value (wasmdown.wasi:fs-stat-at ctx d nil "lnk"))))
             ;; path-flags.symlink-follow expands the FINAL component
             (=> "regular-file"
                 (fs-type-name
                  (fs-ok-value
                   (wasmdown.wasi:fs-stat-at
                    ctx d (cons :flags '("symlink-follow")) "lnk"))))
             ;; nested paths stay handle-relative
             (=> "directory"
                 (fs-type-name
                  (fs-ok-value
                   (wasmdown.wasi:fs-stat-at ctx d nil "sub/inner"))))
             ;; readlink-at: the link TEXT (preopen-relative)
             (=> "a.txt" (fs-ok-value (wasmdown.wasi:fs-readlink-at ctx d "lnk"))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE SECURITY INVARIANTS: no absolute escape, no `..' climb, no
;;; symlink-escape (a host-planted absolute or climbing link), and no
;;; cross-authority descriptor.

(test wasi/fs-no-absolute-or-dotdot-escape
  (with-fs-root (root "fs-escape" '("a.txt"))
    (fs-write! (fs-p root "a.txt") "x")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; absolute paths never reach the host
             (=> "not-permitted"
                 (fs-error-code-of (wasmdown.wasi:fs-stat-at ctx d nil "/etc/passwd")))
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-open-at ctx d nil "/tmp/x"
                                            (cons :flags '("create")) nil)))
             ;; `..' is refused outright (the conservative 0.3 policy):
             ;; no step may climb, so no link or .. can leave the root
             (=> "not-permitted"
                 (fs-error-code-of (wasmdown.wasi:fs-stat-at ctx d nil "..")))
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil "../etc/passwd")))
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil "sub/../../etc")))
             ;; empty + NUL are invalid, never a host-side surprise
             (=> "invalid" (fs-error-code-of (wasmdown.wasi:fs-stat-at ctx d nil "")))
             (=> "invalid"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil (format nil "a~cx" (code-char 0)))))
             ;; create-directory / unlink / rename all share the check
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-create-directory-at ctx d "../evil")))
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-rename-at ctx d "a.txt" d "../evil")))
             ;; the descriptor of another authority is refused
             (let* ((root-b (fs-test-root "fs-escape-b"))
                    (pre-b (wasmdown.wasi:open-wasi-preopen root-b :name "/b"))
                    (ctx-b (wasmdown.wasi:make-wasi-context
                            :preopens (list pre-b))))
               (unwind-protect
                    (progn
                      (=> "not-permitted"
                          (fs-error-code-of
                           (wasmdown.wasi:fs-stat ctx
                                                  (wasmdown.wasi:wasi-preopen-descriptor
                                                   pre-b))))
                      ;; ... and the same descriptor is fine in its own ctx
                      (=> "directory" (car (fs-ok-value
                                            (wasmdown.wasi:fs-get-type
                                             ctx-b
                                             (wasmdown.wasi:wasi-preopen-descriptor
                                              pre-b))))))
                 (wasmdown.wasi:close-wasi-preopen pre-b)
                 (fs-test-unlink-tree root-b))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-symlink-escape-refused
  (with-fs-root (root "fs-link" '("inside.txt" "out-abs" "out-rel" "esc-dir" "good-link" "new-link"))
    (fs-write! (fs-p root "inside.txt") "in")
    ;; a symlink to an absolute host path, planted by the host (the
    ;; guest can also try symlink-at, which refuses up front)
    (fs-symlink! "/etc/passwd" (fs-p root "out-abs"))
    ;; a relative link that climbs above the root
    (fs-symlink! "../../etc/passwd" (fs-p root "out-rel"))
    ;; a link to an absolute DIRECTORY (an intermediate component)
    (fs-symlink! "/etc" (fs-p root "esc-dir"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre))
           (follow (cons :flags '("symlink-follow"))))
      (unwind-protect
           (progn
             ;; no-follow reads the LINK itself
             (=> "symbolic-link"
                 (fs-type-name (fs-ok-value
                                (wasmdown.wasi:fs-stat-at ctx d nil "out-abs"))))
             ;; following an absolute target is refused
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d follow "out-abs")))
             ;; following a climbing target is refused
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d follow "out-rel")))
             ;; an INTERMEDIATE escaping link is refused too
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil "esc-dir/passwd")))
             ;; opening through an escaping link fails closed
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-open-at ctx d follow "out-abs" nil
                                            (cons :flags '("read")))))
             ;; readlink-at refuses an absolute target (the WIT rule)
             (=> "not-permitted"
                 (fs-error-code-of (wasmdown.wasi:fs-readlink-at ctx d "out-abs")))
             (=> "not-permitted"
                 (fs-error-code-of (wasmdown.wasi:fs-readlink-at ctx d "out-rel")))
             ;; symlink-at refuses an absolute old-path, and never
             ;; creates the link
             (=> "not-permitted"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-symlink-at ctx d "/etc/passwd" "new-link")))
             (is (null (probe-file (fs-p root "new-link"))))
             ;; a legitimate relative link does get created
             (=> '(:ok)
                 (wasmdown.wasi:fs-symlink-at ctx d "inside.txt" "good-link"))
             (=> "inside.txt"
                 (fs-ok-value (wasmdown.wasi:fs-readlink-at ctx d "good-link"))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; The directory/file mutations, and the errno -> error-code map.

(test wasi/fs-directory-operations
  (with-fs-root (root "fs-ops" '("d" "d/e" "f.txt" "g.txt" "plain.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             (=> '(:ok) (wasmdown.wasi:fs-create-directory-at ctx d "d"))
             (is (sb-unix:unix-stat (fs-p root "d")))
             (=> "exist"
                 (fs-error-code-of (wasmdown.wasi:fs-create-directory-at ctx d "d")))
             (=> '(:ok) (wasmdown.wasi:fs-create-directory-at ctx d "d/e"))
             ;; a non-empty directory cannot be removed
             (=> "not-empty"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-remove-directory-at ctx d "d")))
             (=> '(:ok) (wasmdown.wasi:fs-remove-directory-at ctx d "d/e"))
             ;; rename within one descriptor (and across descriptors)
             (fs-write! (fs-p root "f.txt") "abc")
             (=> '(:ok) (wasmdown.wasi:fs-rename-at ctx d "f.txt" d "g.txt"))
             (is (null (probe-file (fs-p root "f.txt"))))
             (is (probe-file (fs-p root "g.txt")))
             (=> 3 (cdr (assoc "size" (fs-ok-value
                                       (wasmdown.wasi:fs-stat-at ctx d nil "g.txt"))
                               :test #'string=)))
             ;; unlink a file; the missing one maps to no-entry
             (=> '(:ok) (wasmdown.wasi:fs-unlink-file-at ctx d "g.txt"))
             (=> "no-entry"
                 (fs-error-code-of (wasmdown.wasi:fs-unlink-file-at ctx d "g.txt")))
             ;; a directory is not a file (EISDIR)
             (let ((code (fs-error-code-of
                          (wasmdown.wasi:fs-unlink-file-at ctx d "d"))))
               (is (member code '("is-directory" "access") :test #'string=)))
             ;; a path THROUGH a regular file is not-directory
             (fs-write! (fs-p root "plain.txt") "x")
             (=> "not-directory"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-stat-at ctx d nil "plain.txt/x")))
             ;; remove the (now empty) directory
             (=> '(:ok) (wasmdown.wasi:fs-remove-directory-at ctx d "d")))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-error-code-mapping
  ;; the errno -> WIT error-code table (the cases the API documents)
  (=> "no-entry" (wasmdown.wasi:errno->error-code 2))
  (=> "exist" (wasmdown.wasi:errno->error-code 17))
  (=> "not-directory" (wasmdown.wasi:errno->error-code 20))
  (=> "is-directory" (wasmdown.wasi:errno->error-code 21))
  (=> "access" (wasmdown.wasi:errno->error-code 13))
  (=> "not-permitted" (wasmdown.wasi:errno->error-code 1))
  (=> "loop" (wasmdown.wasi:errno->error-code 40))
  (=> "not-empty" (wasmdown.wasi:errno->error-code 39))
  (=> "bad-descriptor" (wasmdown.wasi:errno->error-code 9))
  (=> "read-only" (wasmdown.wasi:errno->error-code 30))
  (=> "insufficient-space" (wasmdown.wasi:errno->error-code 28))
  ;; an unmapped errno is the variant's catch-all
  (=> "other" (wasmdown.wasi:errno->error-code 9999)))

(test wasi/fs-open-at-flags
  (with-fs-root (root "fs-open" '("made.txt" "made2.txt" "dir" "six.txt" "sixlink" "child.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre))
           (read (cons :flags '("read")))
           (write (cons :flags '("read" "write")))
           (create (cons :flags '("create")))
           (excl (cons :flags '("create" "exclusive")))
           (trunc (cons :flags '("create" "truncate")))
           (direc (cons :flags '("directory"))))
      (unwind-protect
           (progn
             ;; create + write: the descriptor exists as a host file
             (let* ((ok (fs-ok-value (wasmdown.wasi:fs-open-at ctx d nil "made.txt"
                                                               create write)))
                    (desc (third ok)))
               (is (wasmdown.wasi:wasi-descriptor-p desc))
               (is (probe-file (fs-p root "made.txt")))
               (is (member "write" (wasmdown.wasi:wasi-descriptor-flags desc)
                           :test #'string=))
               ;; set-size extends the file (zeros)
               (=> '(:ok) (wasmdown.wasi:fs-set-size ctx desc 6))
               (=> 6 (cdr (assoc "size" (fs-ok-value (wasmdown.wasi:fs-stat ctx desc))
                                 :test #'string=)))
               ;; sync / sync-data / advise / set-times are honest no-ops
               ;; over the OS calls
               (=> '(:ok) (wasmdown.wasi:fs-sync ctx desc))
               (=> '(:ok) (wasmdown.wasi:fs-sync-data ctx desc))
               (=> '(:ok) (wasmdown.wasi:fs-advise ctx desc 0 0
                                                   (cons :enum "sequential")))
               (=> '(:ok) (wasmdown.wasi:fs-set-times ctx desc
                                                     (cons "now" nil)
                                                     (cons "now" nil)))
               ;; set-times-at through the directory descriptor
               (=> '(:ok) (wasmdown.wasi:fs-set-times-at
                           ctx d nil "made.txt" (cons "no-change" nil)
                           (cons "now" nil)))
               ;; the descriptor is closed ONCE the host drops it
               (wasmdown.wasi:close-wasi-descriptor desc)
               (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc))))
             ;; exclusive creation of an existing file -> exist
             (=> "exist"
                 (fs-error-code-of (wasmdown.wasi:fs-open-at ctx d nil "made.txt"
                                                             excl write)))
             ;; opening with the directory flag on a file -> not-directory
             (let ((code (fs-error-code-of
                          (wasmdown.wasi:fs-open-at ctx d nil "made.txt"
                                                    direc read))))
               (is (member code '("not-directory") :test #'string=)))
             ;; a directory opens with the directory flag
             (fs-mkdir! (fs-p root "dir"))
             (let ((ok (fs-ok-value (wasmdown.wasi:fs-open-at ctx d nil "dir"
                                                              direc read))))
               (is (wasmdown.wasi:wasi-descriptor-dir-p (third ok)))
               (wasmdown.wasi:close-wasi-descriptor (third ok)))
             ;; truncate empties the file
             (fs-write! (fs-p root "six.txt") "abcdef")
             (let* ((ok (fs-ok-value (wasmdown.wasi:fs-open-at ctx d nil "six.txt"
                                                               trunc write)))
                    (desc (third ok)))
               (=> 0 (cdr (assoc "size" (fs-ok-value
                                         (wasmdown.wasi:fs-stat-at ctx d nil "six.txt"))
                                 :test #'string=)))
               (wasmdown.wasi:close-wasi-descriptor desc))
             ;; open-at without the follow flag on a symlink fails closed
             (fs-symlink! "six.txt" (fs-p root "sixlink"))
             (=> "loop"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-open-at ctx d nil "sixlink" nil read)))
             ;; a non-directory base refuses creation (read-only)
             (let ((file-desc (third (fs-ok-value
                                      (wasmdown.wasi:fs-open-at ctx d nil "made.txt" nil read)))))
               (=> "read-only"
                   (fs-error-code-of
                    (wasmdown.wasi:fs-open-at ctx file-desc nil "child.txt"
                                              create write)))
               (wasmdown.wasi:close-wasi-descriptor file-desc)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-metadata
  (with-fs-root (root "fs-meta" '("a.txt" "b.txt" "hard.txt"))
    (fs-write! (fs-p root "a.txt") "hello")
    (fs-write! (fs-p root "b.txt") "hello")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; is-same-object: the same descriptor yes, a different
             ;; inode no
             (is (wasmdown.wasi:fs-is-same-object ctx d d))
             (let ((a (third (fs-ok-value (wasmdown.wasi:fs-open-at
                                           ctx d nil "a.txt" nil
                                           (cons :flags '("read"))))))
                   (b (third (fs-ok-value (wasmdown.wasi:fs-open-at
                                           ctx d nil "b.txt" nil
                                           (cons :flags '("read")))))))
               (unwind-protect
                    (progn
                      (is (not (wasmdown.wasi:fs-is-same-object ctx a b)))
                      ;; metadata-hash is stable for an unmodified object
                      ;; and differs across different objects
                      (let ((ha (fs-ok-value (wasmdown.wasi:fs-metadata-hash ctx a))))
                        (is (equal ha (fs-ok-value (wasmdown.wasi:fs-metadata-hash ctx a))))
                        (is (not (equal ha (fs-ok-value
                                            (wasmdown.wasi:fs-metadata-hash ctx b))))))
                      ;; metadata-hash-at agrees with metadata-hash
                      (is (equal (fs-ok-value (wasmdown.wasi:fs-metadata-hash ctx a))
                                 (fs-ok-value
                                  (wasmdown.wasi:fs-metadata-hash-at ctx d nil "a.txt")))))
                 (wasmdown.wasi:close-wasi-descriptor a)
                 (wasmdown.wasi:close-wasi-descriptor b)))
             ;; link-at: a hard link shares the inode
             (let* ((a (third (fs-ok-value (wasmdown.wasi:fs-open-at
                                           ctx d nil "a.txt" nil
                                           (cons :flags '("read")))))))
               (unwind-protect
                    (progn
                      (=> '(:ok) (wasmdown.wasi:fs-link-at ctx d nil "a.txt" d "hard.txt"))
                      (=> 2 (cdr (assoc "link-count"
                                       (fs-ok-value
                                        (wasmdown.wasi:fs-stat-at ctx d nil "hard.txt"))
                                       :test #'string=)))
                      (=> "exist"
                          (fs-error-code-of
                           (wasmdown.wasi:fs-link-at ctx d nil "a.txt" d "hard.txt"))))
                 (wasmdown.wasi:close-wasi-descriptor a))))
        (wasmdown.wasi:close-wasi-preopen pre)))))


;;; ---------------------------------------------------------------
;;; The descriptor RESOURCE through the runtime's handle table:
;;; own/borrow, the dtor, and the provider export surface.

(defun provider-exports (instance)
  (wasmdown.component.runtime:comp-instance-exports instance))

(defun has-method-p (types name)
  (let ((hit (gethash (format nil "[method]descriptor.~a" name)
                      (provider-exports types))))
    (and hit (wasmdown.component.runtime:lifted-func-p (cdr hit)))))

(test wasi/fs-provider-surface
  (with-fs-root (root "fs-prov" '("a.txt"))
    (fs-write! (fs-p root "a.txt") "hello")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (types (wasmdown.wasi:filesystem-types-instance ctx))
           (preopens (wasmdown.wasi:filesystem-preopens-instance ctx))
           (dtor-ct (wasmdown.wasi:descriptor-resource-ct))
           (type-export (gethash "descriptor" (provider-exports types)))
           (pre-desc (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; the interface instance exports the resource TYPE and
             ;; every method under its component core name
             (is (eq (car type-export) :type))
             (is (eq (wasmdown.component:ct-kind (cdr type-export)) :resource))
             ;; the type ct is THE handle identity: the exported type,
             ;; the method self borrows and descriptor-resource-ct agree
             (is (eq dtor-ct (cdr type-export)))
             (dolist (m '("stat" "stat-at" "get-type" "get-flags" "set-size"
                          "is-same-object" "metadata-hash" "metadata-hash-at"
                          "create-directory-at" "remove-directory-at"
                          "unlink-file-at" "rename-at" "link-at"
                          "readlink-at" "symlink-at" "open-at" "advise"
                          "sync" "sync-data" "set-times" "set-times-at"
                          "read-via-stream" "write-via-stream"
                          "append-via-stream" "read-directory"))
               (unless (has-method-p types m)
                 (error "missing exported method ~a" m)))
             ;; the method functype: the WIT signature + the implicit
             ;; self borrow, marked async
             (let* ((fn (provider-fn types "[method]descriptor.stat"))
                    (ft (wasmdown.component.runtime:lifted-func-type fn))
                    (params (coerce (first (wasmdown.component:ct-payload ft))
                                    'list)))
               (is (= 1 (length params)))
               (is (string= "self" (caar params)))
               (is (eq :borrow (wasmdown.component:ct-kind (cdar params))))
               (is (eq dtor-ct (wasmdown.component:ct-payload (cdar params))))
               (is (third (wasmdown.component:ct-payload ft))))
             ;; get-directories: own-descriptor handles + the guest name
             (let ((dirs (call-provider preopens "get-directories")))
               (is (= 1 (length dirs)))
               (let* ((entry (first dirs))
                      (own (first entry))
                      (name (second entry)))
                 (=> "/sandbox" name)
                 (=> :own-handle (car own))
                 (is (eq dtor-ct (second own)))
                 (is (eq pre-desc (third own)))
                 ;; a direct call with the REP works (no handle table)
                 (=> "directory"
                     (car (fs-ok-value
                           (call-provider types "[method]descriptor.get-type"
                                          pre-desc))))))
             ;; BORROW through the handle table: a raw handle index is
             ;; resolved on the DYNAMIC CALLER's table through the
             ;; shared resource ct
             (let* ((guest (wasmdown.component.runtime:make-comp-instance))
                    (idx (wasmdown.component.runtime::%handle-add
                          guest dtor-ct pre-desc nil)))
               (is (eql 1 idx))
               (is (eq pre-desc
                       (let ((wasmdown.component.runtime::*handle-caller-inst*
                               guest))
                         (wasmdown.component.runtime:host-resource-rep
                          dtor-ct idx))))
               (is (equal "directory"
                          (car (fs-ok-value
                                (let ((wasmdown.component.runtime::*handle-caller-inst*
                                        guest))
                                  (call-provider types
                                                 "[method]descriptor.get-type"
                                                 idx))))))
               ;; an index that names no descriptor is bad-descriptor
               (is (equal "bad-descriptor"
                          (fs-error-code-of
                           (let ((wasmdown.component.runtime::*handle-caller-inst*
                                   guest))
                             (call-provider types
                                            "[method]descriptor.get-type"
                                            99))))))
             ;; an OPEN descriptor is owned: the host finalizer closes
             ;; its fd; a preopen descriptor's fd is NOT closed
             (let* ((opened (third (fs-ok-value
                                    (wasmdown.wasi:fs-open-at
                                     ctx pre-desc nil "a.txt" nil
                                     (cons :flags '("read"))))))
                    (hdtor (gethash dtor-ct
                                    wasmdown.component.runtime:*host-resource-dtor-table*)))
               (is (functionp hdtor))
               (is (plusp (wasmdown.wasi:wasi-descriptor-fd opened)))
               (funcall hdtor opened)
               (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd opened)))
               (funcall hdtor pre-desc)
               (is (plusp (wasmdown.wasi:wasi-preopen-fd pre)))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END FIXTURE: a component imports
;;; wasi:filesystem/types@0.3.2 + preopens@0.3.2, calls
;;; get-directories, opens a file with open-at(create) through the
;;; preopen descriptor, stats it (the new file is size 0) and stats
;;; the preopen directory itself (the guest's descriptor-stat must
;;; equal the host's own fstat of that directory). The host sees the
;;; file the guest created.

(defparameter *fs-fixture*
  (concatenate
   'string
   "(component"
   " (import \"wasi:filesystem/types@0.3.2\""
   "   (instance $fs"
   "     (export \"descriptor\" (type $D (sub resource)))"
   "     (export \"[method]descriptor.open-at\""
   "       (func (param \"self\" (borrow $D))"
   "             (param \"path-flags\" u32) (param \"path\" string)"
   "             (param \"open-flags\" u32) (param \"flags\" u32)"
   "             (result (result (own $D) u32))))"
   "     (export \"[method]descriptor.stat\""
   "       (func (param \"self\" (borrow $D)) (result (tuple u64 u64))))))"
   " (import \"wasi:filesystem/preopens@0.3.2\""
   "   (instance $pre"
   "     (export \"descriptor\" (type $D2 (sub resource)))"
   "     (export \"get-directories\""
   "       (func (result (list (tuple (own $D2) string)))))))"
   " (alias export $pre \"get-directories\" (func $gd))"
   " (alias export $fs \"[method]descriptor.open-at\" (func $open-at))"
   " (alias export $fs \"[method]descriptor.stat\" (func $stat))"
   " (core module $Mem"
   "   (memory (export \"mem\") 1)"
   "   (global $bump (mut i32) (i32.const 1024))"
   "   (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "     (local $p i32)"
   "     (local.set $p"
   "       (i32.and (i32.add (global.get $bump)"
   "                         (i32.sub (local.get 2) (i32.const 1)))"
   "                (i32.sub (i32.const 0) (local.get 2))))"
   "     (global.set $bump (i32.add (local.get $p) (local.get 3)))"
   "     (local.get $p)))"
   " (core instance $mem (instantiate $Mem))"
   " (core func $gd-lowered"
   "   (canon lower (func $gd) (memory (core memory $mem \"mem\"))"
   "                (realloc (core func $mem \"realloc\"))))"
   " (core func $open-lowered"
   "   (canon lower (func $open-at) (memory (core memory $mem \"mem\"))"
   "                (realloc (core func $mem \"realloc\"))))"
   " (core func $stat-lowered"
   "   (canon lower (func $stat) (memory (core memory $mem \"mem\"))"
   "                (realloc (core func $mem \"realloc\"))))"
   " (core module $M"
   "   (import \"env\" \"mem\" (memory 1))"
   "   (import \"env\" \"gd\" (func $gd (param i32)))"
   "   (import \"env\" \"open\" (func $open (param i32 i32 i32 i32 i32 i32 i32)))"
   "   (import \"env\" \"stat\" (func $stat (param i32 i32)))"
   "   (func (export \"f\") (result i64)"
   "     (local $d i32)"
   "     (call $gd (i32.const 0))"
   "     (local.set $d (i32.load (i32.load (i32.const 0))))"
   "     (call $open (local.get $d) (i32.const 0) (i32.const 256)"
   "                 (i32.const 8) (i32.const 1) (i32.const 3)"
   "                 (i32.const 128))"
   "     (local.set $d (i32.load (i32.const 132)))"
   "     (call $stat (local.get $d) (i32.const 512))"
   "     (i64.load (i32.const 544)))"
   "   (func (export \"g\") (result i64)"
   "     (call $gd (i32.const 0))"
   "     (call $stat (i32.load (i32.load (i32.const 0))) (i32.const 512))"
   "     (i64.load (i32.const 544)))"
   "   (data (i32.const 256) \"made.txt\"))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $mem \"mem\"))"
   "   (export \"gd\" (func $gd-lowered))"
   "   (export \"open\" (func $open-lowered))"
   "   (export \"stat\" (func $stat-lowered))))))"
   " (func (export \"f\") (result u64) (canon lift (core func $m \"f\")))"
   " (func (export \"g\") (result u64) (canon lift (core func $m \"g\"))))"))

(test wasi/e2e-filesystem-component
  (with-fs-root (root "fs-e2e" '("made.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (comp (wasmdown.component.text::parse-component-text *fs-fixture*)))
      (unwind-protect
           (progn
             (wasmdown.component:validate-component comp)
             (let* ((inst (wasmdown.component.runtime:instantiate-component
                           comp
                           :with-args (wasmdown.wasi:filesystem-provider ctx)))
                    (f (wasmdown.component.runtime:runtime-export-value inst "f"))
                    (g (wasmdown.component.runtime:runtime-export-value inst "g")))
               ;; open-at(create) made the file through the preopen
               ;; handle; the guest's stat of the new descriptor says 0
               (is (eql 0 (wasmdown.component.runtime:lifted-func-call f)))
               (is (probe-file (fs-p root "made.txt")))
               ;; the host sees the very same file after the call
               (is (sb-unix:unix-stat (fs-p root "made.txt")))
               ;; the guest's stat of the PREOPEN descriptor equals
               ;; the host's fstat of that same directory handle (the
               ;; st_size field, read out of the canonical layout)
               (is (= (nth-value 8 (sb-unix:unix-fstat
                                    (wasmdown.wasi:wasi-preopen-fd pre)))
                      (wasmdown.component.runtime:lifted-func-call g)))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ===============================================================
;;; M16 SLICE 3b — THE FILESYSTEM STREAM SURFACE: read/write/
;;; append-via-stream + read-directory over the host stream/future
;;; ABI, and the async canon path.
;;; ===============================================================

;;; ---------------------------------------------------------------
;;; CL-side drivers for the host ends (the provider's peer).

(defun fs-bytes (path)
  "PATH's bytes as a u8 list."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s)
                         :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      (coerce v 'list))))

(defun fs-drain-source (reader &optional (limit 64))
  "Read the guest end READER into CL buffers until DROPPED; the
accumulated u8 values."
  (let ((out '()))
    (loop
      (let ((buf (wasmdown.component.runtime:make-host-buffer limit)))
        (rtcall "%END-COPY" reader buf t)
        (setf out (append out (wasmdown.component.runtime:host-buffer-values
                               buf)))
        (let ((ev (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader))))
          (when (< (logand ev #xf) 2)   ; DROPPED(1)/CANCELLED(2)
            (return out)))))))

(defun fs-stage-write (writer bytes)
  "The guest's write through WRITER; returns the packed event code."
  (rtcall "%END-COPY" writer
          (wasmdown.component.runtime:make-host-buffer
           (length bytes) :values bytes)
          nil)
  (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer)))

(defun fs-drop-end (end)
  (rtcall "%END-DROP" end))

(defun fs-future-value (future)
  "The (single) value a host future delivers."
  (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
    (rtcall "%END-COPY" future buf t)
    (first (wasmdown.component.runtime:host-buffer-values buf))))

(defun fs-fresh-stream ()
  (wasmdown.component.runtime:make-host-stream-pair *u8-ct*))

(defun fs-open-file (ctx base path open-flags flags)
  "open-at through the preopen descriptor BASE."
  (third (fs-ok-value (wasmdown.wasi:fs-open-at ctx base nil path
                                               open-flags flags))))

(test wasi/fs-write-via-stream
  (with-fs-root (root "fs-wvs" '("out.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let ((desc (fs-open-file ctx d "out.txt"
                                     (cons :flags '("create"))
                                     (cons :flags '("read" "write")))))
             (unwind-protect
                  (progn
                    ;; write "hello" at offset 0
                    (multiple-value-bind (reader writer) (fs-fresh-stream)
                      (let ((future (wasmdown.wasi:fs-write-via-stream
                                     ctx desc reader 0)))
                        (is (wasmdown.component.runtime:async-end-p future))
                        (is (eql (%completed 5)
                                 (fs-stage-write writer '(104 101 108 108 111))))
                        ;; the bytes are on disk once drained (synchronously
                        ;; at the guest's write), not only at the drop
                        (is (equal '(104 101 108 108 111)
                                   (fs-bytes (fs-p root "out.txt"))))
                        ;; dropping the writable end resolves the future
                        (fs-drop-end writer)
                        (is (equal '(:ok) (fs-future-value future)))))
                    ;; a second stream writes PAST the end (a zero-filled
                    ;; gap — the WIT: "the file is extended to the extent
                    ;; of the write")
                    (multiple-value-bind (reader writer) (fs-fresh-stream)
                      (let ((future (wasmdown.wasi:fs-write-via-stream
                                     ctx desc reader 8)))
                        (fs-stage-write writer '(88))
                        (fs-drop-end writer)
                        (is (equal '(:ok) (fs-future-value future))))
                      ;; the kernel file offset never moved: the writes
                      ;; were pwrite, so the descriptor is still at 0
                      (is (= 0 (sb-unix:unix-lseek
                                (wasmdown.wasi:wasi-descriptor-fd desc)
                                0 sb-unix:l_incr))))
                    (is (equal '(104 101 108 108 111 0 0 0 88)
                               (fs-bytes (fs-p root "out.txt")))))
               (wasmdown.wasi:close-wasi-descriptor desc)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-read-via-stream-offset-independence
  (with-fs-root (root "fs-rvs" '("data.txt"))
    (fs-write! (fs-p root "data.txt") "abcdefghij")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((desc (fs-open-file ctx d "data.txt" nil
                                      (cons :flags '("read"))))
                  (a (wasmdown.wasi:fs-read-via-stream ctx desc 2))
                  (b (wasmdown.wasi:fs-read-via-stream ctx desc 5))
                  (sa (first a)) (fa (second a))
                  (sb (first b)) (fb (second b)))
             ;; two streams, different offsets: interleaved reads pull
             ;; THEIR OWN windows (no shared offset drift)
             (is (equal '(99 100 101) (fs-drain-3 sa)))   ; offset 2
             (is (equal '(102 103 104) (fs-drain-3 sb)))   ; offset 5
             (is (equal '(102 103 104) (fs-drain-3 sa)))   ; offset 5
             (is (equal '(105 106) (fs-drain-3 sb)))       ; offset 8
             (is (equal '(105 106) (fs-drain-3 sa)))
             (is (equal nil (fs-drain-3 sa)))
             (is (equal nil (fs-drain-3 sb)))
             ;; the fd's own kernel offset was never touched
             (is (= 0 (sb-unix:unix-lseek
                       (wasmdown.wasi:wasi-descriptor-fd desc)
                       0 sb-unix:l_incr)))
             ;; both futures ended (:ok) at EOF
             (is (equal '(:ok) (fs-future-value fa)))
             (is (equal '(:ok) (fs-future-value fb)))
             (wasmdown.wasi:close-wasi-descriptor desc))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(defun fs-drain-3 (reader)
  "One 3-element CL read from READER (NIL at DROPPED)."
  (let ((buf (wasmdown.component.runtime:make-host-buffer 3)))
    (rtcall "%END-COPY" reader buf t)
    (let ((vals (wasmdown.component.runtime:host-buffer-values buf)))
      (and vals (not (eql 1 (logand (third (rtcall "%WAITABLE-GET-PENDING-EVENT"
                                                   reader))
                                    #xf)))
           vals))))

(test wasi/fs-append-via-stream
  (with-fs-root (root "fs-avs" '("log.txt"))
    (fs-write! (fs-p root "log.txt") "ab")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let ((desc (fs-open-file ctx d "log.txt" nil
                                     (cons :flags '("read" "write")))))
             (unwind-protect
                  (multiple-value-bind (reader writer) (fs-fresh-stream)
                    (let ((future (wasmdown.wasi:fs-append-via-stream
                                   ctx desc reader)))
                      ;; two drains of the SAME append stream: each goes to
                      ;; the file's CURRENT end (O_APPEND semantics), so the
                      ;; second chunk lands after the first
                      (is (eql (%completed 2) (fs-stage-write writer '(99 100))))
                      (is (eql (%completed 2) (fs-stage-write writer '(101 102))))
                      (fs-drop-end writer)
                      (is (equal '(:ok) (fs-future-value future)))
                      (is (equal '(97 98 99 100 101 102)
                                 (fs-bytes (fs-p root "log.txt"))))))
               (wasmdown.wasi:close-wasi-descriptor desc)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-stream-flag-gating
  (with-fs-root (root "fs-gate" '("w.txt" "r.txt" "dir"))
    (fs-write! (fs-p root "w.txt") "x")
    (fs-write! (fs-p root "r.txt") "y")
    (fs-mkdir! (fs-p root "dir"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let ((w (fs-open-file ctx d "w.txt" nil (cons :flags '("write"))))
                 (rd (fs-open-file ctx d "r.txt" nil (cons :flags '("read")))))
             (unwind-protect
                  (progn
                    ;; read needs the read flag: a DROPPED stream and a
                    ;; future carrying the WIT error-code
                    (let ((pair (wasmdown.wasi:fs-read-via-stream ctx w 0)))
                      (is (equal nil (fs-drain-3 (first pair))))
                      (is (equal '(:err . "not-permitted")
                                 (fs-future-value (second pair)))))
                    (let ((pair (wasmdown.wasi:fs-read-directory ctx w)))
                      (is (equal '(:err . "not-permitted")
                                 (fs-future-value (second pair)))))
                    ;; the write forms need the write flag
                    (multiple-value-bind (reader writer) (fs-fresh-stream)
                      (is (equal '(:err . "read-only")
                                 (fs-future-value
                                  (wasmdown.wasi:fs-write-via-stream
                                   ctx rd reader 0))))
                      ;; the guest's staged write now meets DROPPED
                      (is (eql 1 (logand (fs-stage-write writer '(1)) #xf))))
                    (multiple-value-bind (reader writer) (fs-fresh-stream)
                      (declare (ignore writer))
                      (is (equal '(:err . "read-only")
                                 (fs-future-value
                                  (wasmdown.wasi:fs-append-via-stream
                                   ctx rd reader)))))
                    ;; a directory opened read-only IS readable
                    (let ((dir (fs-open-file ctx d "dir"
                                             (cons :flags '("directory"))
                                             (cons :flags '("read")))))
                      (unwind-protect
                           (let ((pair (wasmdown.wasi:fs-read-directory ctx dir)))
                             (is (wasmdown.component.runtime:async-end-p
                                  (first pair)))
                             (is (wasmdown.component.runtime:async-end-p
                                  (second pair))))
                        (wasmdown.wasi:close-wasi-descriptor dir))))
               (wasmdown.wasi:close-wasi-descriptor w)
               (wasmdown.wasi:close-wasi-descriptor rd)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-stream-read-error-future
  ;; A read on a DIRECTORY descriptor: pread fails with EISDIR — the
  ;; future carries the WIT error case, the stream ends DROPPED.
  (with-fs-root (root "fs-rerr" '("dir"))
    (fs-mkdir! (fs-p root "dir"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((sub (fs-open-file ctx d "dir"
                                     (cons :flags '("directory"))
                                     (cons :flags '("read"))))
                  (entry (wasmdown.wasi:fs-read-via-stream ctx sub 0))
                  (stream (first entry))
                  (future (second entry)))
             (unwind-protect
                  (progn
                    (is (equal nil (fs-drain-3 stream)))
                    (is (eql 1 (logand (third (rtcall "%WAITABLE-GET-PENDING-EVENT"
                                                      stream))
                                       #xf)))
                    (is (equal '(:err . "is-directory")
                               (fs-future-value future))))
               (wasmdown.wasi:close-wasi-descriptor sub)))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-read-directory
  (with-fs-root (root "fs-rd" '("a.txt" "sub" "lnk"))
    (fs-write! (fs-p root "a.txt") "hi")
    (fs-mkdir! (fs-p root "sub"))
    (fs-symlink! "a.txt" (fs-p root "lnk"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((pair (wasmdown.wasi:fs-read-directory ctx d))
                  (stream (first pair))
                  (future (second pair))
                  (buf (wasmdown.component.runtime:make-host-buffer 16)))
             (rtcall "%END-COPY" stream buf t)
             (let* ((entries (wasmdown.component.runtime:host-buffer-values buf))
                    (by-name (mapcar (lambda (e)
                                       (cons (cdr (assoc "name" e :test #'string=))
                                             (second (assoc "%type" e
                                                            :test #'string=))))
                                     entries)))
               (is (eql (%completed 3)
                        (third (rtcall "%WAITABLE-GET-PENDING-EVENT" stream))))
               ;; `.` and `..` are omitted
               (is (null (assoc "." by-name :test #'string=)))
               (is (null (assoc ".." by-name :test #'string=)))
               ;; every entry with its type (d_type on this filesystem)
               (is (equal (sort (copy-list by-name) #'string< :key #'car)
                          '(("a.txt" . "regular-file")
                            ("lnk" . "symbolic-link")
                            ("sub" . "directory")))))
             ;; a second read is DROPPED (the directory is exhausted); the
             ;; future resolved (:ok) at the end of the iteration
             (is (equal nil (fs-drain-3 stream)))
             (is (equal '(:ok) (fs-future-value future)))
             ;; a SECOND stream starts at the beginning again (the WIT:
             ;; "always returns a new stream which starts at the
             ;; beginning"; concurrent streams do not interfere)
             (let* ((pair2 (wasmdown.wasi:fs-read-directory ctx d))
                    (buf2 (wasmdown.component.runtime:make-host-buffer 1)))
               (rtcall "%END-COPY" (first pair2) buf2 t)
               (is (eql 1 (length (wasmdown.component.runtime:host-buffer-values
                                   buf2)))))
             ;; the DT_UNKNOWN fallback: the entry's type from
             ;; O_PATH|O_NOFOLLOW + fstat (a link reports ITSELF)
             (let ((dirfd (wasmdown.wasi::%libc-openat
                           (wasmdown.wasi:wasi-preopen-fd pre) "."
                           (logior wasmdown.wasi::+o-rdonly+
                                   wasmdown.wasi::+o-directory+
                                   wasmdown.wasi::+o-cloexec+)
                           0)))
               (unwind-protect
                    (progn
                      (is (equal "regular-file"
                                 (car (wasmdown.wasi::%dirent-stat-type
                                       dirfd "a.txt"))))
                      (is (equal "symbolic-link"
                                 (car (wasmdown.wasi::%dirent-stat-type
                                       dirfd "lnk")))))
                 (wasmdown.wasi::%close-fd dirfd))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; DESCRIPTOR / HOST-RESOURCE LIFETIME: the descriptor handle and
;;; each stream end hold independent references to the fd; the fd is
;;; closed exactly once, when the last holder releases it.

(test wasi/fs-descriptor-refcount-stream-outlives-handle
  ;; dropping the descriptor handle while a stream is live must not
  ;; close the fd under the stream; the fd closes once, at the
  ;; stream's EOF (the last holder's release)
  (with-fs-root (root "fs-ref1" '("data.txt"))
    (fs-write! (fs-p root "data.txt") "abcdef")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((desc (fs-open-file ctx d "data.txt" nil
                                      (cons :flags '("read"))))
                  (fd (wasmdown.wasi:wasi-descriptor-fd desc))
                  (pair (wasmdown.wasi:fs-read-via-stream ctx desc 0))
                  (stream (first pair))
                  (future (second pair)))
             ;; the handle's own reference + the stream's
             (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
             (wasmdown.wasi:close-wasi-descriptor desc)
             (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (wasmdown.wasi:wasi-descriptor-closed-p desc))
             (is (plusp fd))
             ;; the fd is STILL open: the stream keeps it alive
             (is (not (null (sb-unix:unix-fstat fd))))
             (is (equal '(97 98 99) (fs-drain-3 stream)))
             (is (equal '(100 101 102) (fs-drain-3 stream)))
             (is (null (fs-drain-3 stream)))
             (is (equal '(:ok) (fs-future-value future)))
             ;; the last holder released: closed exactly once
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc)))
             (is (wasmdown.wasi:wasi-descriptor-closed-p desc))
             (is (null (sb-unix:unix-fstat fd)))
             ;; a redundant handle release cannot decrement again
             (wasmdown.wasi:close-wasi-descriptor desc)
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-stream-reference-released-once
  ;; the stream's reference is released on its terminal event: the
  ;; read-error path and the guest's own drop
  (with-fs-root (root "fs-ref2" '("dir" "f.txt"))
    (fs-mkdir! (fs-p root "dir"))
    (fs-write! (fs-p root "f.txt") "xy")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (progn
             ;; an OS error (pread on a directory) ends the read and
             ;; releases the stream's reference
             (let* ((sub (fs-open-file ctx d "dir"
                                       (cons :flags '("directory"))
                                       (cons :flags '("read"))))
                    (fd (wasmdown.wasi:wasi-descriptor-fd sub))
                    (entry (wasmdown.wasi:fs-read-via-stream ctx sub 0)))
               (is (= 2 (wasmdown.wasi:wasi-descriptor-refs sub)))
               (is (null (fs-drain-3 (first entry))))
               (is (equal '(:err . "is-directory")
                          (fs-future-value (second entry))))
               (is (= 1 (wasmdown.wasi:wasi-descriptor-refs sub)))
               (is (not (null (sb-unix:unix-fstat fd))))
               (wasmdown.wasi:close-wasi-descriptor sub)
               (is (zerop (wasmdown.wasi:wasi-descriptor-refs sub)))
               (is (null (sb-unix:unix-fstat fd))))
             ;; the guest dropping its own end (the cancel path)
             ;; releases the same single reference
             (let* ((desc (fs-open-file ctx d "f.txt" nil
                                        (cons :flags '("read"))))
                    (fd (wasmdown.wasi:wasi-descriptor-fd desc))
                    (pair (wasmdown.wasi:fs-read-via-stream ctx desc 0)))
               (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
               (fs-drop-end (first pair))
               (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
               (is (equal '(:ok) (fs-future-value (second pair))))
               (wasmdown.wasi:close-wasi-descriptor desc)
               (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
               (is (null (sb-unix:unix-fstat fd))))
             ;; a WRITE stream outlives its handle too: the bytes still
             ;; land, and the fd closes at the guest's drop
             (let* ((desc (fs-open-file ctx d "f.txt" nil
                                        (cons :flags '("read" "write"))))
                    (fd (wasmdown.wasi:wasi-descriptor-fd desc)))
               (multiple-value-bind (reader writer) (fs-fresh-stream)
                 (let ((future (wasmdown.wasi:fs-write-via-stream
                                ctx desc reader 1)))
                   (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (wasmdown.wasi:close-wasi-descriptor desc)
                   (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (is (not (null (sb-unix:unix-fstat fd))))
                   (is (eql (%completed 1) (fs-stage-write writer '(90))))
                   (fs-drop-end writer)
                   (is (equal '(:ok) (fs-future-value future)))
                   (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
                   (is (null (sb-unix:unix-fstat fd)))
                   ;; "xy" with 'Z' pwritten at offset 1
                   (is (equal '(120 90) (fs-bytes (fs-p root "f.txt"))))))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-descriptor-release-marker-idempotent
  ;; the hold's released marker makes a second release a no-op, and a
  ;; release below zero can never happen
  (with-fs-root (root "fs-ref3" '("a.txt"))
    (fs-write! (fs-p root "a.txt") "z")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre)))
      (unwind-protect
           (let* ((desc (fs-open-file ctx d "a.txt" nil
                                      (cons :flags '("read"))))
                  (fd (wasmdown.wasi:wasi-descriptor-fd desc))
                  (hold (wasmdown.wasi::%descriptor-hold desc)))
             (is (= 2 (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (= 1 (wasmdown.wasi::%fd-hold-release hold)))
             (is (not (null (sb-unix:unix-fstat fd))))
             ;; second release: the marker swallows it
             (is (= 1 (wasmdown.wasi::%fd-hold-release hold)))
             (is (= 1 (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (not (null (sb-unix:unix-fstat fd))))
             ;; the handle's release is the last one
             (wasmdown.wasi:close-wasi-descriptor desc)
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (null (sb-unix:unix-fstat fd)))
             ;; releasing an already-zero descriptor is a no-op
             (is (zerop (wasmdown.wasi::%descriptor-release desc)))
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs desc)))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-preopen-drop-by-guest-keeps-root
  ;; a guest resource.drop of the preopen's own descriptor handle must
  ;; not close the embedder's root fd, and the root keeps working
  (with-fs-root (root "fs-ref4" '("a.txt"))
    (fs-write! (fs-p root "a.txt") "q")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre))
           (fd (wasmdown.wasi:wasi-preopen-fd pre)))
      (unwind-protect
           (progn
             (wasmdown.wasi:close-wasi-descriptor d)
             (is (not (wasmdown.wasi:wasi-descriptor-closed-p d)))
             (is (eql fd (wasmdown.wasi:wasi-descriptor-fd d)))
             (is (plusp (wasmdown.wasi:wasi-preopen-fd pre)))
             (is (not (null (sb-unix:unix-fstat fd))))
             ;; the root still resolves through the guest's handle
             (=> "regular-file"
                 (fs-type-name (fs-ok-value
                                (wasmdown.wasi:fs-stat-at ctx d nil "a.txt"))))
             ;; only the embedder's close releases it, exactly once
             (wasmdown.wasi:close-wasi-preopen pre)
             (is (eql -1 (wasmdown.wasi:wasi-preopen-fd pre)))
             (is (null (sb-unix:unix-fstat fd))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(test wasi/fs-preopen-close-invalidates-descriptor
  ;; closing the root invalidates the cached descriptor: open-at/stat
  ;; through it is the WIT error, and a reused fd NUMBER (the freed
  ;; number, handed to an unrelated file) grants no authority
  (with-fs-root (root "fs-ref5" '("a.txt" "b.txt"))
    (fs-write! (fs-p root "a.txt") "hello")
    (fs-write! (fs-p root "b.txt") "x")
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/r"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (d (wasmdown.wasi:wasi-preopen-descriptor pre))
           (fd (wasmdown.wasi:wasi-preopen-fd pre)))
      (unwind-protect
           (progn
             (is (plusp fd))
             ;; the root works before the close
             (let ((opened (fs-open-file ctx d "a.txt" nil
                                         (cons :flags '("read")))))
               (is (plusp (wasmdown.wasi:wasi-descriptor-fd opened)))
               (wasmdown.wasi:close-wasi-descriptor opened))
             (wasmdown.wasi:close-wasi-preopen pre)
             (is (eql -1 (wasmdown.wasi:wasi-preopen-fd pre)))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd d)))
             (is (wasmdown.wasi:wasi-descriptor-closed-p d))
             (is (null (sb-unix:unix-fstat fd)))
             ;; the guest's cached descriptor handle names nothing
             (=> "bad-descriptor"
                 (fs-error-code-of
                  (wasmdown.wasi:fs-open-at ctx d nil "b.txt" nil
                                            (cons :flags '("read")))))
             (=> "bad-descriptor"
                 (fs-error-code-of (wasmdown.wasi:fs-stat ctx d)))
             ;; Linux hands out the lowest free fd, so this open takes
             ;; the freed preopen NUMBER and keeps it live — yet the
             ;; closed descriptor still reaches nothing through it
             (let ((reused (sb-unix:unix-open
                            (fs-p root "b.txt")
                            (logior wasmdown.wasi::+o-rdonly+
                                    wasmdown.wasi::+o-cloexec+)
                            0)))
               (unwind-protect
                    (progn
                      (is (eql fd reused))
                      (is (not (null (sb-unix:unix-fstat reused))))
                      (=> "bad-descriptor"
                          (fs-error-code-of
                           (wasmdown.wasi:fs-open-at ctx d nil "b.txt" nil
                                                     (cons :flags '("read"))))))
                 (wasmdown.wasi::%close-fd reused)))
             ;; a live root opened after the close is unaffected
             (let* ((pre2 (wasmdown.wasi:open-wasi-preopen root :name "/r2"))
                    (ctx2 (wasmdown.wasi:make-wasi-context
                           :preopens (list pre2)))
                    (d2 (wasmdown.wasi:wasi-preopen-descriptor pre2)))
               (unwind-protect
                    (=> "regular-file"
                        (fs-type-name (fs-ok-value
                                       (wasmdown.wasi:fs-stat-at
                                        ctx2 d2 nil "a.txt"))))
                 (wasmdown.wasi:close-wasi-preopen pre2)))
             ;; closing twice is a no-op
             (wasmdown.wasi:close-wasi-preopen pre)
             (is (eql -1 (wasmdown.wasi:wasi-preopen-fd pre))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END STREAM FIXTURE: a component writes "hello" through
;;; write-via-stream at offset 0, appends "!" through
;;; append-via-stream, reads the whole file back through
;;; read-via-stream(0) into its own memory and returns n<<8|byte0.

(defparameter *fs-stream-fixture*
  (concatenate
   'string
   "(component"
   " (type $ST (stream u8))"
   " (type $FT (future (result u32 u32)))"
   " (import \"wasi:filesystem/types@0.3.2\""
   "   (instance $fs"
   "     (export \"descriptor\" (type $D (sub resource)))"
   "     (export \"[method]descriptor.write-via-stream\""
   "       (func (param \"self\" (borrow $D)) (param \"data\" (stream u8))"
   "             (param \"offset\" u64) (result (future (result u32 u32)))))"
   "     (export \"[method]descriptor.read-via-stream\""
   "       (func (param \"self\" (borrow $D)) (param \"offset\" u64)"
   "             (result (tuple (stream u8) (future (result u32 u32))))))"
   "     (export \"[method]descriptor.append-via-stream\""
   "       (func (param \"self\" (borrow $D)) (param \"data\" (stream u8))"
   "             (result (future (result u32 u32)))))"
   "     (export \"[method]descriptor.open-at\""
   "       (func (param \"self\" (borrow $D))"
   "             (param \"path-flags\" u32) (param \"path\" string)"
   "             (param \"open-flags\" u32) (param \"flags\" u32)"
   "             (result (result (own $D) u32))))))"
   " (import \"wasi:filesystem/preopens@0.3.2\""
   "   (instance $pre"
   "     (export \"descriptor\" (type $D2 (sub resource)))"
   "     (export \"get-directories\""
   "       (func (result (list (tuple (own $D2) string)))))))"
   " (alias export $pre \"get-directories\" (func $gd))"
   " (alias export $fs \"[method]descriptor.write-via-stream\" (func $wvs))"
   " (alias export $fs \"[method]descriptor.read-via-stream\" (func $rvs))"
   " (alias export $fs \"[method]descriptor.append-via-stream\" (func $avs))"
   " (alias export $fs \"[method]descriptor.open-at\" (func $open-at))"
   " (core module $Mem"
   "   (memory (export \"mem\") 1)"
   "   (global $bump (mut i32) (i32.const 1024))"
   "   (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "     (local $p i32)"
   "     (local.set $p"
   "       (i32.and (i32.add (global.get $bump)"
   "                         (i32.sub (local.get 2) (i32.const 1)))"
   "                (i32.sub (i32.const 0) (local.get 2))))"
   "     (global.set $bump (i32.add (local.get $p) (local.get 3)))"
   "     (local.get $p)))"
   " (core instance $memory (instantiate $Mem))"
   " (core func $gd-lowered"
   "   (canon lower (func $gd) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $wvs-lowered"
   "   (canon lower (func $wvs) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $rvs-lowered"
   "   (canon lower (func $rvs) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $avs-lowered"
   "   (canon lower (func $avs) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $open-lowered"
   "   (canon lower (func $open-at) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (canon stream.new $ST (core func $snew))"
   " (canon stream.write $ST (memory (core memory $memory \"mem\"))"
   "       (core func $swrite))"
   " (canon stream.read $ST (memory (core memory $memory \"mem\"))"
   "       (core func $sread))"
   " (canon stream.drop-writable $ST (core func $sdropw))"
   " (canon future.read $FT (memory (core memory $memory \"mem\"))"
   "       (core func $fread))"
   " (core module $M"
   "   (import \"env\" \"mem\" (memory 1))"
   "   (import \"env\" \"gd\" (func $gd (param i32)))"
   "   (import \"env\" \"wvs\" (func $wvs (param i32 i32 i64) (result i32)))"
   "   (import \"env\" \"rvs\" (func $rvs (param i32 i64 i32)))"
   "   (import \"env\" \"avs\" (func $avs (param i32 i32) (result i32)))"
   "   (import \"env\" \"open\" (func $open (param i32 i32 i32 i32 i32 i32 i32)))"
   "   (import \"env\" \"snew\" (func $snew (result i64)))"
   "   (import \"env\" \"swrite\" (func $swrite (param i32 i32 i32) (result i32)))"
   "   (import \"env\" \"sread\" (func $sread (param i32 i32 i32) (result i32)))"
   "   (import \"env\" \"sdropw\" (func $sdropw (param i32)))"
   "   (import \"env\" \"fread\" (func $fread (param i32 i32) (result i32)))"
   "   (func (export \"f\") (result i32)"
   "     (local $d i32) (local $pair i64) (local $rx i32) (local $tx i32)"
   "     (local $fut i32) (local $r i32) (local $n i32)"
   "     (call $gd (i32.const 0))"
   "     (local.set $d (i32.load (i32.load (i32.const 0))))"
   "     (i32.store (i32.const 256) (i32.const 778530668))"
   "     (i32.store (i32.const 260) (i32.const 7633012))"
   "     (call $open (local.get $d) (i32.const 0) (i32.const 256)"
   "                 (i32.const 7) (i32.const 1) (i32.const 3)"
   "                 (i32.const 128))"
   "     (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 128)))"
   "         (then unreachable))"
   "     (local.set $d (i32.load (i32.const 132)))"
   "     (i32.store8 (i32.const 512) (i32.const 104))"
   "     (i32.store8 (i32.const 513) (i32.const 101))"
   "     (i32.store8 (i32.const 514) (i32.const 108))"
   "     (i32.store8 (i32.const 515) (i32.const 108))"
   "     (i32.store8 (i32.const 516) (i32.const 111))"
   "     (local.set $pair (call $snew))"
   "     (local.set $rx (i32.wrap_i64 (local.get $pair)))"
   "     (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $pair) (i64.const 32))))"
   "     (local.set $fut (call $wvs (local.get $d) (local.get $rx) (i64.const 0)))"
   "     (local.set $r (call $swrite (local.get $tx) (i32.const 512) (i32.const 5)))"
   "     (if (i32.ne (i32.const 80) (local.get $r)) (then unreachable))"
   "     (call $sdropw (local.get $tx))"
   "     (local.set $r (call $fread (local.get $fut) (i32.const 544)))"
   "     (if (i32.ne (i32.const 0) (local.get $r)) (then unreachable))"
   "     (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 544))) (then unreachable))"
   "     (i32.store8 (i32.const 520) (i32.const 33))"
   "     (local.set $pair (call $snew))"
   "     (local.set $rx (i32.wrap_i64 (local.get $pair)))"
   "     (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $pair) (i64.const 32))))"
   "     (local.set $fut (call $avs (local.get $d) (local.get $rx)))"
   "     (local.set $r (call $swrite (local.get $tx) (i32.const 520) (i32.const 1)))"
   "     (if (i32.ne (i32.const 16) (local.get $r)) (then unreachable))"
   "     (call $sdropw (local.get $tx))"
   "     (if (i32.ne (i32.const 0) (call $fread (local.get $fut) (i32.const 548)))"
   "         (then unreachable))"
   "     (call $rvs (local.get $d) (i64.const 0) (i32.const 560))"
   "     (local.set $rx (i32.load (i32.const 560)))"
   "     (local.set $r (call $sread (local.get $rx) (i32.const 600) (i32.const 20)))"
   "     (local.set $n (i32.shr_u (local.get $r) (i32.const 4)))"
   "     (i32.or (i32.shl (local.get $n) (i32.const 8))"
   "             (i32.load8_u (i32.const 600)))))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $memory \"mem\"))"
   "   (export \"gd\" (func $gd-lowered))"
   "   (export \"wvs\" (func $wvs-lowered))"
   "   (export \"rvs\" (func $rvs-lowered))"
   "   (export \"avs\" (func $avs-lowered))"
   "   (export \"open\" (func $open-lowered))"
   "   (export \"snew\" (func $snew))"
   "   (export \"swrite\" (func $swrite))"
   "   (export \"sread\" (func $sread))"
   "   (export \"sdropw\" (func $sdropw))"
   "   (export \"fread\" (func $fread))))))"
   " (func (export \"f\") (result u32) (canon lift (core func $m \"f\"))))"))

(test wasi/e2e-filesystem-stream-component
  (with-fs-root (root "fs-stream-e2e" '("log.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (comp (wasmdown.component.text::parse-component-text
                  *fs-stream-fixture*)))
      (unwind-protect
           (progn
             (wasmdown.component:validate-component comp)
             (let* ((inst (wasmdown.component.runtime:instantiate-component
                           comp
                           :with-args (wasmdown.wasi:filesystem-provider ctx)))
                    (f (wasmdown.component.runtime:runtime-export-value inst "f")))
               ;; 6 bytes read back, first byte 'h' (n<<8|byte0)
               (is (eql 1640 (wasmdown.component.runtime:lifted-func-call f)))
               ;; the host sees exactly the bytes the guest wrote and
               ;; appended
               (is (equal '(104 101 108 108 111 33)
                          (fs-bytes (fs-p root "log.txt"))))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

;;; ---------------------------------------------------------------
;;; THE ASYNC CANON FIXTURE: a guest that canon-lowers ASYNC-typed
;;; filesystem methods with `async` (the packed state|subtask<<4
;;; return, the [ptr] out-param) and drives the waitable-set
;;; protocol. `is-same-object(d, d)` proves the borrow-param lend +
;;; scalar result; `write-via-stream` proves a STREAM/FUTURE result
;;; lowered into the caller's table through the async path, with the
;;; future's resolution delivered by waitable-set.wait after the
;;; guest drops the writable end.

(defparameter *fs-async-canon-fixture*
  (concatenate
   'string
   "(component"
   " (type $ST (stream u8))"
   " (type $FT (future (result u32 u32)))"
   " (import \"wasi:filesystem/types@0.3.2\""
   "   (instance $fs"
   "     (export \"descriptor\" (type $D (sub resource)))"
   "     (export \"[method]descriptor.is-same-object\""
   "       (func async (param \"self\" (borrow $D))"
   "             (param \"other\" (borrow $D)) (result bool)))"
   "     (export \"[method]descriptor.write-via-stream\""
   "       (func async (param \"self\" (borrow $D)) (param \"data\" (stream u8))"
   "             (param \"offset\" u64) (result (future (result u32 u32)))))"
   "     (export \"[method]descriptor.open-at\""
   "       (func (param \"self\" (borrow $D))"
   "             (param \"path-flags\" u32) (param \"path\" string)"
   "             (param \"open-flags\" u32) (param \"flags\" u32)"
   "             (result (result (own $D) u32))))))"
   " (import \"wasi:filesystem/preopens@0.3.2\""
   "   (instance $pre"
   "     (export \"descriptor\" (type $D2 (sub resource)))"
   "     (export \"get-directories\""
   "       (func (result (list (tuple (own $D2) string)))))))"
   " (alias export $pre \"get-directories\" (func $gd))"
   " (alias export $fs \"[method]descriptor.is-same-object\" (func $same))"
   " (alias export $fs \"[method]descriptor.write-via-stream\" (func $wvs))"
   " (alias export $fs \"[method]descriptor.open-at\" (func $open-at))"
   " (core module $Mem"
   "   (memory (export \"mem\") 1)"
   "   (global $bump (mut i32) (i32.const 1024))"
   "   (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "     (local $p i32)"
   "     (local.set $p"
   "       (i32.and (i32.add (global.get $bump)"
   "                         (i32.sub (local.get 2) (i32.const 1)))"
   "                (i32.sub (i32.const 0) (local.get 2))))"
   "     (global.set $bump (i32.add (local.get $p) (local.get 3)))"
   "     (local.get $p)))"
   " (core instance $memory (instantiate $Mem))"
   " (core func $gd-lowered"
   "   (canon lower (func $gd) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $same-lowered"
   "   (canon lower (func $same) async (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $wvs-lowered"
   "   (canon lower (func $wvs) async (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (core func $open-lowered"
   "   (canon lower (func $open-at) (memory (core memory $memory \"mem\"))"
   "                (realloc (core func $memory \"realloc\"))))"
   " (canon stream.new $ST (core func $snew))"
   " (canon stream.write $ST async (memory (core memory $memory \"mem\"))"
   "       (core func $swrite))"
   " (canon stream.drop-writable $ST (core func $sdropw))"
   " (canon future.read $FT async (memory (core memory $memory \"mem\"))"
   "       (core func $fread))"
   " (canon subtask.drop (core func $subdrop))"
   " (canon waitable.join (core func $join))"
   " (canon waitable-set.new (core func $wsnew))"
   " (canon waitable-set.wait (memory (core memory $memory \"mem\"))"
   "       (core func $wait))"
   " (core module $M"
   "   (import \"env\" \"mem\" (memory 1))"
   "   (import \"env\" \"gd\" (func $gd (param i32)))"
   "   (import \"env\" \"same\" (func $same (param i32 i32 i32) (result i32)))"
   "   (import \"env\" \"wvs\" (func $wvs (param i32 i32 i64 i32) (result i32)))"
   "   (import \"env\" \"open\" (func $open (param i32 i32 i32 i32 i32 i32 i32)))"
   "   (import \"env\" \"snew\" (func $snew (result i64)))"
   "   (import \"env\" \"swrite\" (func $swrite (param i32 i32 i32) (result i32)))"
   "   (import \"env\" \"sdropw\" (func $sdropw (param i32)))"
   "   (import \"env\" \"fread\" (func $fread (param i32 i32) (result i32)))"
   "   (import \"env\" \"subdrop\" (func $subdrop (param i32)))"
   "   (import \"env\" \"join\" (func $join (param i32 i32)))"
   "   (import \"env\" \"wsnew\" (func $wsnew (result i32)))"
   "   (import \"env\" \"wait\" (func $wait (param i32 i32) (result i32)))"
   "   (func $poll-subtask (param $packed i32) (param $ws i32)"
   "     (local $sub i32)"
   "     (if (i32.ne (i32.and (local.get $packed) (i32.const 15)) (i32.const 2))"
   "       (then"
   "         (local.set $sub (i32.shr_u (local.get $packed) (i32.const 4)))"
   "         (call $join (local.get $sub) (local.get $ws))"
   "         (block $done"
   "           (loop $poll"
   "             (drop (call $wait (local.get $ws) (i32.const 224)))"
   "             (br_if $done (i32.eq (i32.load (i32.const 224)) (i32.const 1)))"
   "             (br_if $done (i32.ge_u (i32.load (i32.const 228)) (i32.const 2)))"
   "             (br $poll)))"
   "         (if (i32.ne (i32.const 1) (i32.load (i32.const 224)))"
   "             (then unreachable))"
   "         (call $subdrop (local.get $sub)))))"
   "   (func (export \"f\") (result i32)"
   "     (local $d0 i32) (local $d i32) (local $ws i32) (local $pair i64)"
   "     (local $rx i32) (local $tx i32) (local $fut i32) (local $packed i32)"
   "     (local $r i32) (local $sameok i32) (local $ok i32)"
   "     (call $gd (i32.const 0))"
   "     (local.set $d0 (i32.load (i32.load (i32.const 0))))"
   "     (i32.store8 (i32.const 320) (i32.const 97))"
   "     (i32.store8 (i32.const 321) (i32.const 115))"
   "     (i32.store8 (i32.const 322) (i32.const 121))"
   "     (i32.store8 (i32.const 323) (i32.const 110))"
   "     (i32.store8 (i32.const 324) (i32.const 99))"
   "     (i32.store8 (i32.const 325) (i32.const 46))"
   "     (i32.store8 (i32.const 326) (i32.const 116))"
   "     (i32.store8 (i32.const 327) (i32.const 120))"
   "     (i32.store8 (i32.const 328) (i32.const 116))"
   "     (call $open (local.get $d0) (i32.const 0) (i32.const 320)"
   "                 (i32.const 9) (i32.const 1) (i32.const 3)"
   "                 (i32.const 128))"
   "     (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 128)))"
   "         (then unreachable))"
   "     (local.set $d (i32.load (i32.const 132)))"
   "     (local.set $ws (call $wsnew))"
   "     (local.set $packed (call $same (local.get $d) (local.get $d)"
   "                                  (i32.const 128)))"
   "     (if (i32.ne (i32.const 2) (i32.and (local.get $packed) (i32.const 15)))"
   "         (then unreachable))"
   "     (call $poll-subtask (local.get $packed) (local.get $ws))"
   "     (local.set $sameok (i32.load8_u (i32.const 128)))"
   "     (local.set $pair (call $snew))"
   "     (local.set $rx (i32.wrap_i64 (local.get $pair)))"
   "     (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $pair) (i64.const 32))))"
   "     (i32.store16 (i32.const 512) (i32.const 0x6968))"
   "     (local.set $packed (call $wvs (local.get $d) (local.get $rx) (i64.const 0)"
   "                                  (i32.const 160)))"
   "     (if (i32.ne (i32.const 2) (i32.and (local.get $packed) (i32.const 15)))"
   "         (then unreachable))"
   "     (call $poll-subtask (local.get $packed) (local.get $ws))"
   "     (local.set $fut (i32.load (i32.const 160)))"
   "     (call $join (local.get $fut) (local.get $ws))"
   "     (local.set $r (call $fread (local.get $fut) (i32.const 192)))"
   "     (if (i32.ne (i32.const -1) (local.get $r)) (then unreachable))"
   "     (local.set $r (call $swrite (local.get $tx) (i32.const 512) (i32.const 2)))"
   "     (if (i32.ne (i32.const 32) (local.get $r)) (then unreachable))"
   "     (call $sdropw (local.get $tx))"
   "     (local.set $r (call $wait (local.get $ws) (i32.const 224)))"
   "     (if (i32.ne (i32.const 4) (local.get $r)) (then unreachable))"
   "     (if (i32.ne (i32.const 0) (i32.load (i32.const 228)))"
   "         (then unreachable))"
   "     (local.set $ok (i32.load8_u (i32.const 192)))"
   "     (i32.or (i32.shl (local.get $sameok) (i32.const 8))"
   "             (i32.or (i32.shl (local.get $ok) (i32.const 4))"
   "                     (i32.const 1)))))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $memory \"mem\"))"
   "   (export \"gd\" (func $gd-lowered))"
   "   (export \"same\" (func $same-lowered))"
   "   (export \"wvs\" (func $wvs-lowered))"
   "   (export \"open\" (func $open-lowered))"
   "   (export \"snew\" (func $snew))"
   "   (export \"swrite\" (func $swrite))"
   "   (export \"sdropw\" (func $sdropw))"
   "   (export \"fread\" (func $fread))"
   "   (export \"subdrop\" (func $subdrop))"
   "   (export \"join\" (func $join))"
   "   (export \"wsnew\" (func $wsnew))"
   "   (export \"wait\" (func $wait))))))"
   " (func (export \"f\") (result u32) (canon lift (core func $m \"f\"))))"))

(test wasi/e2e-async-canon-filesystem-component
  (with-fs-root (root "fs-async-e2e" '("async.txt"))
    (let* ((pre (wasmdown.wasi:open-wasi-preopen root :name "/sandbox"))
           (ctx (wasmdown.wasi:make-wasi-context :preopens (list pre)))
           (comp (wasmdown.component.text::parse-component-text
                  *fs-async-canon-fixture*)))
      (unwind-protect
           (progn
             (wasmdown.component:validate-component comp)
             (let* ((inst (wasmdown.component.runtime:instantiate-component
                           comp
                           :with-args (wasmdown.wasi:filesystem-provider ctx)))
                    (f (wasmdown.component.runtime:runtime-export-value inst "f")))
               ;; sameok=1 (<<8), the future's ok discriminant 0 (<<4),
               ;; the FUTURE_READ event delivered (1)
               (is (eql 257 (wasmdown.component.runtime:lifted-func-call f)))
               ;; the async-lowered write-via-stream put the bytes on disk
               (is (string= "hi" (fs-bytes->string (fs-p root "async.txt"))))))
        (wasmdown.wasi:close-wasi-preopen pre)))))

(defun fs-bytes->string (path)
  (map 'string #'code-char (fs-bytes path)))

;;; ===============================================================
;;; M16 SLICE 4 — wasi:sockets: the network authority, the TCP/UDP
;;; resources, ip-name-lookup, the errno mapping, and a TCP-echo
;;; e2e component.
;;; ===============================================================

;;; Domains + drivers.
(defun sock-addr (port &optional (host '(127 0 0 1)))
  (cons "ipv4" (list (cons "port" port) (cons "address" host))))

(defun sock-addr6 (port &optional (host '(0 0 0 0 0 0 0 1)))
  (cons "ipv6" (list (cons "port" port)
                     (cons "flow-info" 0)
                     (cons "address" host)
                     (cons "scope-id" 0))))

(defun sock-endpoint (&optional (port :any))
  (wasmdown.wasi:make-wasi-endpoint :address '(127 0 0 1) :port port))

(defun sock-network (&key (outbound :loopback) (listen :loopback) (dns :none))
  (wasmdown.wasi:make-wasi-network
   :outbound (if (eq outbound :loopback) (list (sock-endpoint)) outbound)
   :listen (if (eq listen :loopback) (list (sock-endpoint)) listen)
   :dns dns))

(defun sock-ok (result) (cdr result))
(defun sock-code (result)
  (wasmdown.wasi:sockets-error-code (cdr result)))
(defun sock-rep (result) (third (sock-ok result)))

(defun sock-value-port (value)
  "The port of an ip-socket-address domain value."
  (cdr (assoc "port" (cdr value) :test #'string=)))

(defun sock-future-code (value)
  "The error-code case name of a (:err . code) future value (NIL for
an ok result)."
  (when (and (consp value) (eq (car value) :err))
    (wasmdown.wasi:sockets-error-code (cdr value))))

(defun sock-stream ()
  (wasmdown.component.runtime:make-host-stream-pair *u8-ct*))

(defun sock-stage (writer bytes)
  "The guest's write through WRITER; the packed event code."
  (rtcall "%END-COPY" writer
          (wasmdown.component.runtime:make-host-buffer
           (length bytes) :values bytes)
          nil)
  (third (rtcall "%WAITABLE-GET-PENDING-EVENT" writer)))

(defun sock-drop (end) (rtcall "%END-DROP" end))

(defun sock-drain (reader &optional (limit 64) (want nil))
  "Accumulate CL reads from READER until WANT values arrived, no
progress is possible, or the end is DROPPED."
  (let ((out '()))
    (loop
      (let ((buf (wasmdown.component.runtime:make-host-buffer limit)))
        (rtcall "%END-COPY" reader buf t)
        (setf out (append out
                          (wasmdown.component.runtime:host-buffer-values buf)))
        (let ((ev (third (rtcall "%WAITABLE-GET-PENDING-EVENT" reader))))
          (when (or (and want (>= (length out) want))
                    (null ev)
                    (< (logand ev #xf) 2))
            (return out)))))))

(defun sock-future (future)
  "The single value a HOST future delivers."
  (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
    (rtcall "%END-COPY" future buf t)
    (first (wasmdown.component.runtime:host-buffer-values buf))))

(defun sock-accept (listener-stream)
  "One accepted tcp-socket rep out of LISTENER-STREAM, or NIL."
  (let ((buf (wasmdown.component.runtime:make-host-buffer 1)))
    (rtcall "%END-COPY" listener-stream buf t)
    (let ((values (wasmdown.component.runtime:host-buffer-values buf)))
      (and values (third (first values))))))

;;; ---------------------------------------------------------------
;;; The sockaddr layout is empirical: sizes + the bytes the domain
;;; conversion writes (the slice-3b `struct dirent' discipline).

(test wasi/sockets-sockaddr-layout
  (is (= 16 (sb-alien:alien-size
             (sb-alien:struct wasmdown.wasi::%sockaddr-in) :bytes)))
  (is (= 28 (sb-alien:alien-size
             (sb-alien:struct wasmdown.wasi::%sockaddr-in6) :bytes)))
  (multiple-value-bind (alien len family)
      (wasmdown.wasi:ip-socket-address->sockaddr (sock-addr 4321))
    (unwind-protect
         (let ((sap (sb-alien:alien-sap alien))
               (d (sb-alien:deref alien)))
           (is (eql :ipv4 family))
           (is (= 16 len))
           ;; family at 0 (host-endian u16), port at 2 (network order),
           ;; address at 4..7
           (is (= wasmdown.wasi::+af-inet+ (sb-sys:sap-ref-8 sap 0)))
           (is (= 0 (sb-sys:sap-ref-8 sap 1)))
           (is (= #x10 (sb-sys:sap-ref-8 sap 2)))     ; 4321 = #x10E1
           (is (= #xE1 (sb-sys:sap-ref-8 sap 3)))
           (is (equal '(127 0 0 1)
                      (loop for i below 4
                            collect (sb-sys:sap-ref-8 sap (+ 4 i)))))
           ;; the struct's slots agree with those byte offsets
           (is (= wasmdown.wasi::+af-inet+
                  (sb-alien:slot d 'wasmdown.wasi::sin-family)))
           (is (= #xE110 (sb-alien:slot d 'wasmdown.wasi::sin-port)))
           ;; the round trip returns the same domain value
           (is (equal (sock-addr 4321)
                      (wasmdown.wasi:sockaddr->ip-socket-address sap))))
      (sb-alien:free-alien alien)))
  (multiple-value-bind (alien len family)
      (wasmdown.wasi:ip-socket-address->sockaddr
       (sock-addr6 1234 '(0 0 0 0 0 0 0 1)))
    (unwind-protect
         (let ((sap (sb-alien:alien-sap alien))
               (d (sb-alien:deref alien)))
           (is (eql :ipv6 family))
           (is (= 28 len))
           (is (= wasmdown.wasi::+af-inet6+ (sb-sys:sap-ref-8 sap 0)))
           (is (= #x04 (sb-sys:sap-ref-8 sap 2)))     ; 1234 = #x04D2
           (is (= #xD2 (sb-sys:sap-ref-8 sap 3)))
           ;; sin6_flowinfo at 4, sin6_addr at 8, sin6_scope_id at 24
           (is (= 0 (sb-alien:slot d 'wasmdown.wasi::sin6-flowinfo)))
           (is (= 0 (sb-alien:slot d 'wasmdown.wasi::sin6-scope-id)))
           (is (equal (sock-addr6 1234 '(0 0 0 0 0 0 0 1))
                      (wasmdown.wasi:sockaddr->ip-socket-address sap))))
      (sb-alien:free-alien alien)))
  ;; a malformed address is the WIT's invalid-argument, never a crash
  (let ((caught
          (handler-case
              (progn (wasmdown.wasi:ip-socket-address->sockaddr
                      (cons "ipv4" (list (cons "port" 1)
                                         (cons "address" '(1 2 3)))))
                     nil)
            (wasmdown.wasi:wasi-socket-error (c)
              (wasmdown.wasi:sockets-error-code
               (wasmdown.wasi:wasi-socket-error-code c))))))
    (is (string= "invalid-argument" caught))))

;;; ---------------------------------------------------------------
;;; The capability policy.

(test wasi/sockets-network-policy
  (let ((net (sock-network :dns '("localhost" "**.example.com"))))
    (is (wasmdown.wasi:wasi-network-allows-outbound-p
         net :ipv4 '(127 0 0 1) 80))
    (is (wasmdown.wasi:wasi-network-allows-outbound-p
         net :ipv4 '(127 0 0 1) 0))
    (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
              net :ipv4 '(10 1 2 3) 80)))
    (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
              net :ipv6 '(0 0 0 0 0 0 0 1) 80)))
    (is (wasmdown.wasi:wasi-network-allows-listen-p
         net :ipv4 '(127 0 0 1) 0))
    (is (not (wasmdown.wasi:wasi-network-allows-listen-p
              net :ipv4 '(0 0 0 0) 0)))
    ;; an endpoint's declared port narrows the match
    (let ((net2 (wasmdown.wasi:make-wasi-network
                 :outbound (list (wasmdown.wasi:make-wasi-endpoint
                                  :address '(127 0 0 1) :port 8080)))))
      (is (wasmdown.wasi:wasi-network-allows-outbound-p
           net2 :ipv4 '(127 0 0 1) 8080))
      (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
                net2 :ipv4 '(127 0 0 1) 8081))))
    ;; DNS: exact + `**.suffix' patterns
    (is (wasmdown.wasi:wasi-network-allows-dns-p net "localhost"))
    (is (wasmdown.wasi:wasi-network-allows-dns-p net "a.example.com"))
    (is (wasmdown.wasi:wasi-network-allows-dns-p net "deep.a.example.com"))
    (is (not (wasmdown.wasi:wasi-network-allows-dns-p net "elsewhere.test"))))
  ;; the deny-all default and the explicit :any escape hatch
  (let ((deny (wasmdown.wasi:make-wasi-network))
        (any (wasmdown.wasi:make-wasi-network
              :outbound :any :listen :any :dns :all)))
    (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
              deny :ipv4 '(127 0 0 1) 80)))
    (is (not (wasmdown.wasi:wasi-network-allows-listen-p
              deny :ipv4 '(0 0 0 0) 0)))
    (is (not (wasmdown.wasi:wasi-network-allows-dns-p deny "localhost")))
    (is (wasmdown.wasi:wasi-network-allows-outbound-p
         any :ipv6 '(0 0 0 0 0 0 0 1) 443))
    (is (wasmdown.wasi:wasi-network-allows-dns-p any "anything.test")))
  ;; a NIL network authority is deny-all, never an error
  (is (not (wasmdown.wasi:wasi-network-allows-outbound-p
            nil :ipv4 '(127 0 0 1) 80))))

;;; ---------------------------------------------------------------
;;; TCP over real loopback sockets: create/bind/listen/connect/
;;; accept/send/receive, both directions.

(test wasi/sockets-tcp-loopback
  (let* ((net (sock-network :dns '("localhost")))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (accepted nil))
    (unwind-protect
         (progn
           (is (eq :unbound (wasmdown.wasi:wasi-tcp-socket-state server)))
           (is (eq :unbound (wasmdown.wasi:wasi-tcp-socket-state client)))
           ;; bind(127.0.0.1:0) -> an ephemeral loopback port
           (is (equal '(:ok)
                      (wasmdown.wasi:tcp-bind nil net server (sock-addr 0))))
           (let* ((local (sock-ok (wasmdown.wasi:tcp-get-local-address
                                   nil net server)))
                  (port (sock-value-port local)))
             (is (plusp port))
             (is (equal "ipv4" (cdr (wasmdown.wasi:tcp-get-address-family
                                     nil net server))))
             (is (null (wasmdown.wasi:tcp-get-is-listening nil net server)))
             ;; listen -> a stream of accepted sockets
             (let ((listening (wasmdown.wasi:tcp-listen nil net server)))
               (is (eq :ok (car listening)))
               (is (wasmdown.wasi:tcp-get-is-listening nil net server))
               ;; connect (the async resource method; the host resolves
               ;; eagerly)
               (is (equal '(:ok)
                          (wasmdown.wasi:tcp-connect nil net client
                                                     (sock-addr port))))
               (is (equal port
                          (sock-value-port
                           (sock-ok (wasmdown.wasi:tcp-get-remote-address
                                     nil net client)))))
               (is (equal port
                          (sock-value-port
                           (sock-ok (wasmdown.wasi:tcp-get-local-address
                                     nil net server)))))
               (setf accepted (sock-accept (cdr listening)))
               (is (not (null accepted)))
               (is (eq :connected
                       (wasmdown.wasi:wasi-tcp-socket-state accepted)))
               ;; client -> server
               (multiple-value-bind (reader writer) (sock-stream)
                 (let ((future (wasmdown.wasi:tcp-send nil net client reader)))
                   (is (eql (%completed 4)
                            (sock-stage writer '(112 105 110 103))))
                   (sock-drop writer)
                   (is (equal '(:ok) (sock-future future)))))
               (let* ((pair (wasmdown.wasi:tcp-receive nil net accepted))
                      (stream (first pair))
                      (future (second pair)))
                 (is (equal '(112 105 110 103) (sock-drain stream 64 4)))
                 (sock-drop stream)
                 (is (equal '(:ok) (sock-future future))))
               ;; server -> client
               (multiple-value-bind (reader writer) (sock-stream)
                 (let ((future (wasmdown.wasi:tcp-send nil net accepted reader)))
                   (is (eql (%completed 4)
                            (sock-stage writer '(112 111 110 103))))
                   (sock-drop writer)
                   (is (equal '(:ok) (sock-future future)))))
               (let* ((pair (wasmdown.wasi:tcp-receive nil net client))
                      (stream (first pair))
                      (future (second pair)))
                 (is (equal '(112 111 110 103) (sock-drain stream 64 4)))
                 ;; receive is once-per-socket: the second call is the
                 ;; REFUSED shape (a dropped stream + invalid-state)
                 (let ((again (wasmdown.wasi:tcp-receive nil net client)))
                   (is (equal nil (sock-drain (first again) 8 1)))
                   (is (string= "invalid-state"
                                (sock-future-code (sock-future
                                                   (second again))))))
                 (sock-drop stream)
                 (is (equal '(:ok) (sock-future future)))))))
      (when accepted (wasmdown.wasi:close-wasi-tcp-socket accepted))
      (wasmdown.wasi:close-wasi-tcp-socket client)
      (wasmdown.wasi:close-wasi-tcp-socket server))))

;;; ---------------------------------------------------------------
;;; The keep-alive / hop-limit / buffer knobs and their validation.

(test wasi/sockets-tcp-knobs
  (let* ((net (sock-network))
         (sock (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4")))))
    (unwind-protect
         (progn
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-keep-alive-enabled
                              nil net sock t)))
           (is (eq t (sock-ok (wasmdown.wasi::tcp-get-keep-alive-enabled
                               nil net sock))))
           ;; 5s idle / 2s interval / 3 probes
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-keep-alive-idle-time
                              nil net sock 5000000000)))
           (is (= 5000000000
                  (sock-ok (wasmdown.wasi::tcp-get-keep-alive-idle-time
                            nil net sock))))
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-keep-alive-interval
                              nil net sock 2000000000)))
           (is (= 2000000000
                  (sock-ok (wasmdown.wasi::tcp-get-keep-alive-interval
                            nil net sock))))
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-keep-alive-count
                              nil net sock 3)))
           (is (= 3 (sock-ok (wasmdown.wasi::tcp-get-keep-alive-count
                              nil net sock))))
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-hop-limit
                              nil net sock 7)))
           (is (= 7 (sock-ok (wasmdown.wasi::tcp-get-hop-limit nil net sock))))
           ;; the buffer size is a hint: set 128 KiB, read back > 0
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-receive-buffer-size
                              nil net sock 131072)))
           (is (plusp (sock-ok (wasmdown.wasi::tcp-get-receive-buffer-size
                                nil net sock))))
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-send-buffer-size
                              nil net sock 131072)))
           (is (plusp (sock-ok (wasmdown.wasi::tcp-get-send-buffer-size
                                nil net sock))))
           ;; zero is the WIT's invalid-argument on every knob
           (is (string= "invalid-argument"
                        (sock-code (wasmdown.wasi::tcp-set-keep-alive-idle-time
                                    nil net sock 0))))
           (is (string= "invalid-argument"
                        (sock-code (wasmdown.wasi::tcp-set-listen-backlog-size
                                    nil net sock 0))))
           (is (string= "invalid-argument"
                        (sock-code (wasmdown.wasi::tcp-set-hop-limit
                                    nil net sock 0))))
           (is (string= "invalid-argument"
                        (sock-code (wasmdown.wasi::tcp-set-send-buffer-size
                                    nil net sock 0))))
           ;; a valid backlog is accepted
           (is (equal '(:ok) (wasmdown.wasi::tcp-set-listen-backlog-size
                              nil net sock 32))))
      (wasmdown.wasi:close-wasi-tcp-socket sock))))

;;; ---------------------------------------------------------------
;;; UDP over loopback.

(test wasi/sockets-udp-loopback
  (let* ((net (sock-network))
         (a (sock-rep (wasmdown.wasi:udp-create nil net (cons :enum "ipv4"))))
         (b (sock-rep (wasmdown.wasi:udp-create nil net (cons :enum "ipv4")))))
    (unwind-protect
         (progn
           (is (equal '(:ok) (wasmdown.wasi:udp-bind nil net a (sock-addr 0))))
           (is (equal '(:ok) (wasmdown.wasi:udp-bind nil net b (sock-addr 0))))
           (let ((pa (sock-value-port
                      (sock-ok (wasmdown.wasi:udp-get-local-address
                                nil net a))))
                 (pb (sock-value-port
                      (sock-ok (wasmdown.wasi:udp-get-local-address
                                nil net b)))))
             (is (and (plusp pa) (plusp pb) (/= pa pb)))
             ;; a datagram with an explicit peer address (sendto)
             (is (equal '(:ok) (wasmdown.wasi:udp-send
                                nil net a '(1 2 3) (cons :some (sock-addr pb)))))
             (let ((got (wasmdown.wasi:udp-receive nil net b)))
               (is (eq :ok (car got)))
               (is (equal '(1 2 3) (first (cdr got))))
               (is (equal pa (sock-value-port (second (cdr got))))))
             ;; connect + send (the peer is implicit) + receive
             (is (equal '(:ok) (wasmdown.wasi:udp-connect
                                nil net b (sock-addr pa))))
             (is (equal pa (sock-value-port
                            (sock-ok (wasmdown.wasi:udp-get-remote-address
                                      nil net b)))))
             (is (equal '(:ok) (wasmdown.wasi:udp-send
                                nil net b '(9 8) :none)))
             (let ((got (wasmdown.wasi:udp-receive nil net a)))
               (is (equal '(9 8) (first (cdr got))))
               (is (equal pb (sock-value-port (second (cdr got))))))
             ;; a connected socket rejects a different explicit peer
             (is (string= "invalid-argument"
                          (sock-code (wasmdown.wasi:udp-send
                                      nil net b '(1) (cons :some (sock-addr pb))))))
             ;; an unconnected socket needs a peer address
             (is (string= "invalid-argument"
                          (sock-code (wasmdown.wasi:udp-send
                                      nil net a '(1) :none))))
             (is (equal '(:ok) (wasmdown.wasi:udp-disconnect nil net b)))
             (is (string= "invalid-state"
                          (sock-code (wasmdown.wasi:udp-disconnect nil net b))))
             (is (equal "ipv4"
                        (cdr (wasmdown.wasi:udp-get-address-family nil net a))))
             ;; unicast hop limit
             (is (equal '(:ok) (wasmdown.wasi::udp-set-unicast-hop-limit
                                nil net a 5)))
             (is (= 5 (sock-ok (wasmdown.wasi::udp-get-unicast-hop-limit
                                nil net a))))))
      (wasmdown.wasi:close-wasi-udp-socket b)
      (wasmdown.wasi:close-wasi-udp-socket a))))

;;; ---------------------------------------------------------------
;;; ip-name-lookup: literals are parsed as-is; names consult the DNS
;;; policy and then getaddrinfo (localhost is /etc/hosts — hermetic).

(test wasi/sockets-name-lookup
  (let ((net (sock-network :dns '("localhost"))))
    (let ((ok (wasmdown.wasi:ip-name-lookup-resolve nil net "localhost")))
      (is (eq :ok (car ok)))
      (is (find (cons "ipv4" '(127 0 0 1)) (cdr ok) :test #'equal)))
    ;; an IPv4 literal needs no DNS policy at all
    (let ((lit (wasmdown.wasi:ip-name-lookup-resolve
                nil (wasmdown.wasi:make-wasi-network) "127.0.0.1")))
      (is (equal '(:ok ("ipv4" 127 0 0 1)) lit)))
    ;; an IPv6 literal goes through the numeric resolver
    (let ((lit (wasmdown.wasi:ip-name-lookup-resolve
                nil (wasmdown.wasi:make-wasi-network) "::1")))
      (is (eq :ok (car lit)))
      (is (equal "ipv6" (car (first (cdr lit))))))
    ;; an undeclared name is denied by the policy
    (is (string= "access-denied"
                 (sock-code (wasmdown.wasi:ip-name-lookup-resolve
                             nil (wasmdown.wasi:make-wasi-network)
                             "localhost"))))
    ;; an empty name is invalid-argument
    (is (string= "invalid-argument"
                 (sock-code (wasmdown.wasi:ip-name-lookup-resolve
                             nil net ""))))))

;;; ---------------------------------------------------------------
;;; The policy refusals at the operation level: an undeclared
;;; endpoint/listen address/DNS name is the WIT's access-denied.

(test wasi/sockets-policy-refusals
  (let* ((net (sock-network))
         (tcp (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4"))))
         (udp (sock-rep (wasmdown.wasi:udp-create nil net (cons :enum "ipv4")))))
    (unwind-protect
         (progn
           (is (string= "access-denied"
                        (sock-code (wasmdown.wasi:tcp-bind
                                    nil net tcp (sock-addr 0 '(10 1 2 3))))))
           (is (string= "access-denied"
                        (sock-code (wasmdown.wasi:tcp-connect
                                    nil net tcp (sock-addr 80 '(10 1 2 3))))))
           (is (string= "access-denied"
                        (sock-code (wasmdown.wasi:udp-send
                                    nil net udp '(1)
                                    (cons :some (sock-addr 53 '(10 1 2 3)))))))
           ;; the socket is still usable after a refused operation
           (is (equal '(:ok) (wasmdown.wasi:udp-bind
                              nil net udp (sock-addr 0))))
           ;; a refused connect leaves the socket CLOSED (the WIT's
           ;; post-failure state: the only valid action is drop)
           (is (eq :closed (wasmdown.wasi:wasi-tcp-socket-state tcp)))
           ;; no network authority => everything is refused
           (let* ((ctx (wasmdown.wasi:make-wasi-context))
                  (inst (wasmdown.wasi:sockets-types-instance ctx))
                  (create (provider-fn inst "[static]tcp-socket.create")))
             (declare (ignore create))
             (let* ((bare (wasmdown.wasi::tcp-create
                           nil nil (cons :enum "ipv4")))
                    (sock (sock-rep bare)))
               (unwind-protect
                    (is (string= "access-denied"
                                 (sock-code (wasmdown.wasi:tcp-connect
                                             nil nil sock (sock-addr 80)))))
                 (wasmdown.wasi:close-wasi-tcp-socket sock)))))
      (wasmdown.wasi:close-wasi-udp-socket udp)
      (wasmdown.wasi:close-wasi-tcp-socket tcp))))

;;; ---------------------------------------------------------------
;;; The errno -> sockets error-code mapping, observed through real
;;; loopback failures.

(test wasi/sockets-error-mappings
  (let* ((net (sock-network))
         (a (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4"))))
         (b (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4"))))
         (c (sock-rep (wasmdown.wasi:tcp-create nil net (cons :enum "ipv4")))))
    (unwind-protect
         (progn
           (is (equal '(:ok) (wasmdown.wasi:tcp-bind nil net a (sock-addr 0))))
           (let ((port (sock-value-port
                        (sock-ok (wasmdown.wasi:tcp-get-local-address
                                  nil net a)))))
             ;; a bound-but-not-listening port refuses the connection
             (is (string= "connection-refused"
                          (sock-code (wasmdown.wasi:tcp-connect
                                      nil net b (sock-addr port)))))
             ;; the failed connect closed B (the WIT's rule)
             (is (eq :closed (wasmdown.wasi:wasi-tcp-socket-state b)))
             ;; a listening bind blocks a second bind (address-in-use)
             (is (eq :ok (car (wasmdown.wasi:tcp-listen nil net a))))
             (is (string= "address-in-use"
                          (sock-code (wasmdown.wasi:tcp-bind
                                      nil net c (sock-addr port)))))
             ;; double bind / connect-after-listen = invalid-state
             (is (string= "invalid-state"
                          (sock-code (wasmdown.wasi:tcp-bind
                                      nil net a (sock-addr 0)))))
             (is (string= "invalid-state"
                          (sock-code (wasmdown.wasi:tcp-connect
                                      nil net a (sock-addr port)))))
             (is (string= "invalid-state"
                          (sock-code (wasmdown.wasi:tcp-listen nil net a)))))
           ;; send on a non-connected socket is invalid-state, and the
           ;; guest's stream end learns DROPPED
           (multiple-value-bind (reader writer) (sock-stream)
             (let ((future (wasmdown.wasi:tcp-send nil net c reader)))
               (is (string= "invalid-state"
                            (sock-future-code (sock-future future))))
               (is (eql 1 (logand (sock-stage writer '(1)) #xf))))))
      (wasmdown.wasi:close-wasi-tcp-socket c)
      (wasmdown.wasi:close-wasi-tcp-socket b)
      (wasmdown.wasi:close-wasi-tcp-socket a))))

;;; ---------------------------------------------------------------
;;; The provider surface: the resource cts + every method exported,
;;; and the two import names.

(test wasi/sockets-provider-surface
  (let* ((net (sock-network))
         (ctx (wasmdown.wasi:make-wasi-context :networks (list net)))
         (inst (wasmdown.wasi:sockets-types-instance ctx))
         (prov (wasmdown.wasi:sockets-provider ctx)))
    (is (eq :type (car (gethash "tcp-socket"
                                (wasmdown.component.runtime:comp-instance-exports
                                 inst)))))
    (is (eq :type (car (gethash "udp-socket"
                                (wasmdown.component.runtime:comp-instance-exports
                                 inst)))))
    (dolist (name '("[static]tcp-socket.create" "[method]tcp-socket.bind"
                    "[method]tcp-socket.connect" "[method]tcp-socket.listen"
                    "[method]tcp-socket.send" "[method]tcp-socket.receive"
                    "[method]tcp-socket.get-local-address"
                    "[method]tcp-socket.get-remote-address"
                    "[method]tcp-socket.get-is-listening"
                    "[method]tcp-socket.get-address-family"
                    "[method]tcp-socket.set-listen-backlog-size"
                    "[method]tcp-socket.get-keep-alive-enabled"
                    "[method]tcp-socket.get-receive-buffer-size"
                    "[static]udp-socket.create" "[method]udp-socket.bind"
                    "[method]udp-socket.connect" "[method]udp-socket.disconnect"
                    "[method]udp-socket.send" "[method]udp-socket.receive"
                    "[method]udp-socket.get-local-address"))
      (is (wasmdown.wasi:provider-instance-function inst name)))
    (is (= 2 (length prov)))
    (is (assoc "wasi:sockets/types@0.3.2" prov :test #'string=))
    (is (assoc "wasi:sockets/ip-name-lookup@0.3.2" prov :test #'string=))
    ;; the provider-level create is callable through the lifted-func
    ;; boundary (the static method's domain arg + resource result)
    (let ((result (call-provider inst "[static]tcp-socket.create"
                                 (cons :enum "ipv4"))))
      (is (eq :ok (car result)))
      (let ((sock (sock-rep result)))
        (unwind-protect
             (is (eq :unbound (wasmdown.wasi:wasi-tcp-socket-state sock)))
          (wasmdown.wasi:close-wasi-tcp-socket sock))))))

;;; ---------------------------------------------------------------
;;; THE END-TO-END TCP ECHO: a component imports wasi:sockets/types,
;;; creates a tcp-socket, connects to a host echo server on loopback,
;;; sends "ping" through send(stream), reads "pong" through
;;; receive(), and returns the first echoed byte.
;;;
;;; Two text-lane workarounds are visible here (both pre-existing
;;; validator/parser limitations, not sockets behaviour):
;;;   * the compound-param (connect) export is declared BEFORE the
;;;     own-result export (create): the imported-instance
;;;     declarator-space remap resolves the compound param only in
;;;     that order;
;;;   * a handful of scalar `(type $DN u32)' declarations keep the
;;;     canon-opt memory check's raw declarator index in range.
;;; The declared remote-address shape and the error payload are
;;; narrowed (u32) for the same reason; the RUNTIME always lifts the
;;; arguments/results with the provider's real WIT functype, so the
;;; memory layout exercised is the real ip-socket-address one.

(defun tcp-echo-fixture (port)
  (format nil "(component ~
 (type $SADDR (variant ~
   (case \"ipv4\" (tuple u32 u32 u32)) ~
   (case \"ipv6\" (tuple u32 u32 u32)))) ~
 (import \"wasi:sockets/types@0.3.2\" ~
  (instance $sock ~
   (export \"tcp-socket\" (type $T (sub resource))) ~
   (export \"[method]tcp-socket.connect\" ~
     (func async (param \"self\" (borrow $T)) (param \"remote-address\" $SADDR) ~
                 (result (result (error u32))))) ~
   (export \"[static]tcp-socket.create\" ~
     (func (param \"address-family\" u32) (result (result (own $T) u32)))) ~
   (export \"[method]tcp-socket.send\" ~
     (func (param \"self\" (borrow $T)) (param \"data\" (stream u8)) ~
                 (result (future (result (error u32)))))) ~
   (export \"[method]tcp-socket.receive\" ~
     (func (param \"self\" (borrow $T)) ~
                 (result (tuple (stream u8) (future (result (error u32))))))))) ~
 (alias export $sock \"[static]tcp-socket.create\" (func $create)) ~
 (alias export $sock \"[method]tcp-socket.connect\" (func $connect)) ~
 (alias export $sock \"[method]tcp-socket.send\" (func $send)) ~
 (alias export $sock \"[method]tcp-socket.receive\" (func $receive)) ~
 (type $D1 u32) (type $D2 u32) (type $D3 u32) (type $D4 u32) ~
 (type $D5 u32) (type $D6 u32) (type $D7 u32) (type $D8 u32) ~
 (type $D9 u32) (type $D10 u32) (type $D11 u32) (type $D12 u32) ~
 (core module $Mem (memory (export \"mem\") 1)) ~
 (core instance $memory (instantiate $Mem)) ~
 (core func $create-l (canon lower (func $create) (memory (core memory $memory \"mem\")))) ~
 (core func $connect-l (canon lower (func $connect) async (memory (core memory $memory \"mem\")))) ~
 (core func $send-l (canon lower (func $send) (memory (core memory $memory \"mem\")))) ~
 (core func $receive-l (canon lower (func $receive) (memory (core memory $memory \"mem\")))) ~
 (type $ST (stream u8)) ~
 (type $FT (future (result (error u32)))) ~
 (canon stream.new $ST (core func $snew)) ~
 (canon stream.write $ST async (memory (core memory $memory \"mem\")) (core func $swrite)) ~
 (canon stream.read $ST async (memory (core memory $memory \"mem\")) (core func $sread)) ~
 (canon stream.drop-writable $ST (core func $sdropw)) ~
 (canon subtask.drop (core func $subdrop)) ~
 (canon waitable.join (core func $join)) ~
 (canon waitable-set.new (core func $wsnew)) ~
 (canon waitable-set.wait (memory (core memory $memory \"mem\")) (core func $wait)) ~
 (core module $M ~
  (import \"env\" \"mem\" (memory 1)) ~
  (import \"env\" \"create\" (func $create (param i32 i32))) ~
  (import \"env\" \"connect\" (func $connect (param i32 i32) (result i32))) ~
  (import \"env\" \"send\" (func $send (param i32 i32) (result i32))) ~
  (import \"env\" \"receive\" (func $receive (param i32 i32))) ~
  (import \"env\" \"snew\" (func $snew (result i64))) ~
  (import \"env\" \"swrite\" (func $swrite (param i32 i32 i32) (result i32))) ~
  (import \"env\" \"sread\" (func $sread (param i32 i32 i32) (result i32))) ~
  (import \"env\" \"sdropw\" (func $sdropw (param i32))) ~
  (import \"env\" \"subdrop\" (func $subdrop (param i32))) ~
  (import \"env\" \"join\" (func $join (param i32 i32))) ~
  (import \"env\" \"wsnew\" (func $wsnew (result i32))) ~
  (import \"env\" \"wait\" (func $wait (param i32 i32) (result i32))) ~
  (func $poll-subtask (param $packed i32) (param $ws i32) ~
    (local $sub i32) ~
    (if (i32.ne (i32.and (local.get $packed) (i32.const 15)) (i32.const 2)) ~
      (then ~
        (local.set $sub (i32.shr_u (local.get $packed) (i32.const 4))) ~
        (call $join (local.get $sub) (local.get $ws)) ~
        (block $done ~
          (loop $poll ~
            (drop (call $wait (local.get $ws) (i32.const 224))) ~
            (br_if $done (i32.eq (i32.load (i32.const 224)) (i32.const 1))) ~
            (br_if $done (i32.ge_u (i32.load (i32.const 228)) (i32.const 2))) ~
            (br $poll))) ~
        (if (i32.ne (i32.const 1) (i32.load (i32.const 224))) (then unreachable)) ~
        (call $subdrop (local.get $sub))))) ~
  (func (export \"f\") (result i32) ~
    (local $s i32) (local $ws i32) (local $packed i32) ~
    (local $rx i32) (local $tx i32) (local $pair i64) (local $fut i32) ~
    (local $rs i32) (local $rf i32) (local $r i32) ~
    (call $create (i32.const 0) (i32.const 64)) ~
    (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 64))) (then unreachable)) ~
    (local.set $s (i32.load (i32.const 68))) ~
    (i32.store (i32.const 0) (local.get $s)) ~
    (i32.store8 (i32.const 4) (i32.const 0)) ~
    (i32.store16 (i32.const 8) (i32.const ~d)) ~
    (i32.store8 (i32.const 10) (i32.const 127)) ~
    (i32.store8 (i32.const 11) (i32.const 0)) ~
    (i32.store8 (i32.const 12) (i32.const 0)) ~
    (i32.store8 (i32.const 13) (i32.const 1)) ~
    (local.set $ws (call $wsnew)) ~
    (local.set $packed (call $connect (i32.const 0) (i32.const 16))) ~
    (if (i32.ne (i32.const 2) (i32.and (local.get $packed) (i32.const 15))) (then unreachable)) ~
    (call $poll-subtask (local.get $packed) (local.get $ws)) ~
    (if (i32.ne (i32.const 0) (i32.load8_u (i32.const 16))) (then unreachable)) ~
    (local.set $pair (call $snew)) ~
    (local.set $rx (i32.wrap_i64 (local.get $pair))) ~
    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $pair) (i64.const 32)))) ~
    (i32.store8 (i32.const 160) (i32.const 112)) ~
    (i32.store8 (i32.const 161) (i32.const 105)) ~
    (i32.store8 (i32.const 162) (i32.const 110)) ~
    (i32.store8 (i32.const 163) (i32.const 103)) ~
    (local.set $fut (call $send (local.get $s) (local.get $rx))) ~
    (local.set $r (call $swrite (local.get $tx) (i32.const 160) (i32.const 4))) ~
    (if (i32.ne (i32.const 64) (local.get $r)) (then unreachable)) ~
    (call $sdropw (local.get $tx)) ~
    (call $receive (local.get $s) (i32.const 128)) ~
    (local.set $rs (i32.load (i32.const 128))) ~
    (local.set $rf (i32.load (i32.const 132))) ~
    (local.set $r (call $sread (local.get $rs) (i32.const 176) (i32.const 4))) ~
    (if (i32.ne (i32.const 64) (local.get $r)) (then unreachable)) ~
    (i32.load8_u (i32.const 176)))) ~
 (core instance $m (instantiate $M (with \"env\" (instance ~
   (export \"mem\" (memory $memory \"mem\")) ~
   (export \"create\" (func $create-l)) ~
   (export \"connect\" (func $connect-l)) ~
   (export \"send\" (func $send-l)) ~
   (export \"receive\" (func $receive-l)) ~
   (export \"snew\" (func $snew)) ~
   (export \"swrite\" (func $swrite)) ~
   (export \"sread\" (func $sread)) ~
   (export \"sdropw\" (func $sdropw)) ~
   (export \"subdrop\" (func $subdrop)) ~
   (export \"join\" (func $join)) ~
   (export \"wsnew\" (func $wsnew)) ~
   (export \"wait\" (func $wait)))))) ~
 (func (export \"f\") (result u32) (canon lift (core func $m \"f\"))))"
          port))

(defun sock-host-listener ()
  "A BLOCKING loopback TCP listener (the host side of the echo); the
caller closes FD."
  (let ((fd (wasmdown.wasi::%libc-socket
             wasmdown.wasi::+af-inet+ wasmdown.wasi::+sock-stream+
             wasmdown.wasi::+ipproto-tcp+)))
    (multiple-value-bind (alien len)
        (wasmdown.wasi::%make-sockaddr
         "ipv4" (list (cons "port" 0) (cons "address" '(127 0 0 1))))
      (unwind-protect
           (progn
             (when (minusp (wasmdown.wasi::%libc-bind
                            fd (sb-alien:alien-sap alien) len))
               (error "host listener bind failed"))
             (wasmdown.wasi::%libc-listen fd 4))
        (sb-alien:free-alien alien)))
    (values (sock-value-port
             (wasmdown.wasi::%getsockname-domain fd :local))
            fd)))

(defun sock-echo-thread (listen-fd got)
  "Accept one connection, remember its bytes in GOT, echo `pong'."
  (sb-thread:make-thread
   (lambda ()
     (let ((cfd (wasmdown.wasi::%libc-accept4
                 listen-fd (sb-sys:int-sap 0) (sb-sys:int-sap 0) 0)))
       (unwind-protect
            (let ((buf (sb-alien:make-alien sb-alien:unsigned-char 8)))
              (unwind-protect
                   (let ((n (wasmdown.wasi::%libc-recv
                             cfd (sb-alien:alien-sap buf) 8 0)))
                     (setf (car got)
                           (loop for i below n
                                 collect (sb-sys:sap-ref-8
                                          (sb-alien:alien-sap buf) i)))
                     (let ((msg (make-array
                                 4 :element-type '(unsigned-byte 8)
                                   :initial-contents '(112 111 110 103))))
                       (wasmdown.wasi::%libc-send
                        cfd (sb-sys:vector-sap msg) 4 0)))
                (sb-alien:free-alien buf)))
         (wasmdown.wasi::%close-fd cfd))))
   :name "sock-echo"))

(test wasi/e2e-tcp-echo-component
  (multiple-value-bind (port listen-fd) (sock-host-listener)
    (let* ((got (list nil))
           (thread (sock-echo-thread listen-fd got)))
      (unwind-protect
           (let* ((net (sock-network :dns '("localhost")))
                  (context (wasmdown.wasi:make-wasi-context :networks (list net)))
                  (comp (wasmdown.component.text::parse-component-text
                         (tcp-echo-fixture port))))
             (wasmdown.component:validate-component comp)
             (let* ((inst (wasmdown.component.runtime:instantiate-component
                           comp
                           :with-args (wasmdown.wasi:sockets-provider context)))
                    (f (wasmdown.component.runtime:runtime-export-value
                        inst "f")))
               ;; the guest read "pong" and returned its first byte
               (is (eql 112 (wasmdown.component.runtime:lifted-func-call f))))
             (sb-thread:join-thread thread)
             ;; the host received exactly the guest's "ping"
             (is (equal '(112 105 110 103) (car got))))
        (ignore-errors (sb-thread:join-thread thread :timeout 2))
        (wasmdown.wasi::%close-fd listen-fd)))))

;;; ===============================================================
;;; M16 SLICE 6 — THE COMMAND WORLD, THE TERMINALS, THE RUNNER.
;;; ===============================================================

;;; ---- the terminals: some/none by isatty(3) ----

(defun %pty-stream ()
  "A CL stream over a fresh pty master (/dev/ptmx) — isatty(3) true.
Returns (values stream fd); the caller closes FD."
  (multiple-value-bind (fd errno)
      (sb-unix:unix-open "/dev/ptmx" (logior 2 0) 0)
    (unless (and (integerp fd) (not (minusp fd)))
      (error "cannot open /dev/ptmx (errno ~a)" errno))
    (values (sb-sys:make-fd-stream fd :input t :output t
                                        :element-type '(unsigned-byte 8))
            fd)))

(test wasi/terminals-none-for-non-tty
  ;; a string stream has no host fd: no terminal capability exists,
  ;; never a fabricated handle (PLAN.md §9.1)
  (let* ((ctx (wasmdown.wasi:make-wasi-context
               :stdin (make-string-input-stream "")
               :stdout (make-string-output-stream)
               :stderr (make-string-output-stream)))
         (in (wasmdown.wasi:terminal-stdin-instance ctx))
         (out (wasmdown.wasi:terminal-stdout-instance ctx))
         (err (wasmdown.wasi:terminal-stderr-instance ctx)))
    (is (eq :none (call-provider in "get-terminal-stdin")))
    (is (eq :none (call-provider out "get-terminal-stdout")))
    (is (eq :none (call-provider err "get-terminal-stderr")))
    ;; the full cli provider binds all five terminal imports
    (let ((names (mapcar #'car (wasmdown.wasi:cli-provider ctx))))
      (dolist (n '("wasi:cli/terminal-input@0.3.2"
                   "wasi:cli/terminal-output@0.3.2"
                   "wasi:cli/terminal-stdin@0.3.2"
                   "wasi:cli/terminal-stdout@0.3.2"
                   "wasi:cli/terminal-stderr@0.3.2"))
        (is (member n names :test #'string=))))))

(test wasi/terminals-some-for-tty
  (multiple-value-bind (stream fd) (%pty-stream)
    (unwind-protect
         (let* ((ctx (wasmdown.wasi:make-wasi-context :stdout stream))
                (out (wasmdown.wasi:terminal-stdout-instance ctx))
                (r (call-provider out "get-terminal-stdout")))
           (is (consp r))
           (is (eq (car r) :some))
           ;; the some payload is an own handle whose rep is the
           ;; wasi-terminal (fd + isatty) — a BORROWED stream fd
           (let* ((handle (cdr r))
                  (rep (third handle)))
             (is (eq (first handle) :own-handle))
             (is (wasmdown.wasi:wasi-terminal-p rep))
             (is (= fd (wasmdown.wasi:wasi-terminal-fd rep)))))
      (sb-unix:unix-close fd))))

;;; ---- the runner: ok / err / exit-with-code over an async run ----

(defparameter *command-ok-fixture*
  (concatenate 'string
   "(component"
   " (type $ERR (enum \"io\" \"illegal-byte-sequence\" \"pipe\"))"
   " (type $ST (stream u8))"
   " (import \"wasi:cli/environment@0.3.2\""
   "   (instance $env"
   "     (export \"get-arguments\" (func (result (list string))))))"
   " (import \"wasi:cli/stdout@0.3.2\""
   "   (instance $out"
   "     (export \"error-code\" (type (eq $ERR)))"
   "     (type $r (result (error $ERR)))"
   "     (type $f (future $r))"
   "     (type $s (stream u8))"
   "     (export \"write-via-stream\""
   "       (func (param \"data\" $s) (result $f)))))"
   " (alias export $env \"get-arguments\" (func $ga))"
   " (alias export $out \"write-via-stream\" (func $wvs))"
   " (core module $Memory (memory (export \"mem\") 1))"
   " (core instance $memory (instantiate $Memory))"
   " (core module $Realloc"
   "   (global $bump (mut i32) (i32.const 1024))"
   "   (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "     (local $p i32)"
   "     (local.set $p (i32.and (i32.add (global.get $bump) (i32.const 7)) (i32.const -8)))"
   "     (global.set $bump (i32.add (local.get $p) (local.get 3)))"
   "     (local.get $p)))"
   " (core instance $r (instantiate $Realloc))"
   " (alias core export $r \"realloc\" (core func $realloc))"
   " (canon lower (func $ga) (memory (core memory $memory \"mem\")) (realloc (core func $realloc)) (core func $ga-lowered))"
   " (core func $wvs-lowered (canon lower (func $wvs)))"
   " (core module $M"
   "  (import \"env\" \"mem\" (memory 1))"
   "  (import \"env\" \"task.return\" (func $tr (param i32)))"
   "  (import \"env\" \"get-arguments\" (func $args (param i32)))"
   "  (import \"env\" \"stream.new\" (func $snew (result i64)))"
   "  (import \"env\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))"
   "  (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))"
   "  (import \"env\" \"future.read\" (func $fread (param i32 i32) (result i32)))"
   "  (import \"env\" \"write-via-stream\" (func $wvs (param i32) (result i32)))"
   "  (global $fut (mut i32) (i32.const 0))"
   "  (func (export \"run\") (result i32)"
   "    (local $p i64) (local $rx i32) (local $tx i32) (local $ret i32)"
   "    (local $list i32) (local $sp i32) (local $sl i32)"
   "    (call $args (i32.const 16))"
   "    (local.set $list (i32.load (i32.const 16)))"
   "    (local.set $sp (i32.load (local.get $list)))"
   "    (local.set $sl (i32.load (i32.add (local.get $list) (i32.const 4))))"
   "    (local.set $p (call $snew))"
   "    (local.set $rx (i32.wrap_i64 (local.get $p)))"
   "    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $p) (i64.const 32))))"
   "    (global.set $fut (call $wvs (local.get $rx)))"
   "    (local.set $ret (call $swrite (local.get $tx) (local.get $sp) (local.get $sl)))"
   "    (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))"
   "    (call $sdropw (local.get $tx))"
   "    (local.set $ret (call $fread (global.get $fut) (i32.const 64)))"
   "    (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))"
   "    (call $tr (i32.const 0))"
   "    (i32.const 0))"
   "  (func (export \"run-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"
   " (type $FT (future (result (error $ERR))))"
   " (canon task.return (result (result)) (core func $tr))"
   " (canon stream.new $ST (core func $snew))"
   " (canon stream.write $ST (memory (core memory $memory \"mem\")) (core func $swrite))"
   " (canon stream.drop-writable $ST (core func $sdropw))"
   " (canon future.read $FT (memory (core memory $memory \"mem\")) (core func $fread))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"mem\" (memory $memory \"mem\"))"
   "   (export \"task.return\" (func $tr))"
   "   (export \"get-arguments\" (func $ga-lowered))"
   "   (export \"stream.new\" (func $snew))"
   "   (export \"stream.write\" (func $swrite))"
   "   (export \"stream.drop-writable\" (func $sdropw))"
   "   (export \"future.read\" (func $fread))"
   "   (export \"write-via-stream\" (func $wvs-lowered))))))"
   " (func $run (export \"run\") async (result (result)) (canon lift (core func $m \"run\") async (callback (core func $m \"run-cb\"))))"
   " (instance $runiface (export \"run\" (func $run)))"
   " (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
   "   (instance (export \"run\" (func async (result (result)))))))"))

(defparameter *command-err-fixture*
  (concatenate 'string
   "(component"
   " (core module $M"
   "  (import \"env\" \"task.return\" (func $tr (param i32)))"
   "  (func (export \"run\") (result i32) (call $tr (i32.const 1)) (i32.const 0))"
   "  (func (export \"run-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"
   " (canon task.return (result (result)) (core func $tr))"
   " (core instance $m (instantiate $M (with \"env\" (instance (export \"task.return\" (func $tr))))))"
   " (func $run (export \"run\") async (result (result)) (canon lift (core func $m \"run\") async (callback (core func $m \"run-cb\"))))"
   " (instance $runiface (export \"run\" (func $run)))"
   " (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
   "   (instance (export \"run\" (func async (result (result)))))))"))

(defparameter *command-exit-fixture*
  (concatenate 'string
   "(component"
   " (import \"wasi:cli/exit@0.3.2\""
   "   (instance $ex (export \"exit-with-code\" (func (param \"code\" u8)))))"
   " (alias export $ex \"exit-with-code\" (func $ewc))"
   " (core func $ewc-lowered (canon lower (func $ewc)))"
   " (core module $M"
   "  (import \"env\" \"task.return\" (func $tr (param i32)))"
   "  (import \"env\" \"exit-with-code\" (func $ewc (param i32)))"
   "  (func (export \"run\") (result i32)"
   "    (call $ewc (i32.const 7))"
   "    (call $tr (i32.const 0))"
   "    (i32.const 0))"
   "  (func (export \"run-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"
   " (canon task.return (result (result)) (core func $tr))"
   " (core instance $m (instantiate $M (with \"env\" (instance"
   "   (export \"task.return\" (func $tr))"
   "   (export \"exit-with-code\" (func $ewc-lowered))))))"
   " (func $run (export \"run\") async (result (result)) (canon lift (core func $m \"run\") async (callback (core func $m \"run-cb\"))))"
   " (instance $runiface (export \"run\" (func $run)))"
   " (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
   "   (instance (export \"run\" (func async (result (result)))))))"))

(defun %command-component (text)
  (let ((comp (wasmdown.component.text::parse-component-text text)))
    (wasmdown.component:validate-component comp)
    comp))

(test wasi/command-runner-ok-arguments-stdout
  ;; the exported async `run' is driven to completion: get-arguments
  ;; reaches the guest (argv[0] printed through stdout.write-via-stream)
  ;; and the `result' ok maps to status 0
  (let* ((sink (make-string-output-stream))
         (context (wasmdown.wasi:make-wasi-context
                   :arguments '("prog" "-v")
                   :stdout sink))
         (status (wasmdown.wasi:run-component
                  (%command-component *command-ok-fixture*) context)))
    (is (eql 0 status))
    (is (string= "prog" (get-output-stream-string sink)))
    ;; the exit boundary tore the store down: no registered guest threads
    (is (zerop (length wasmdown.component.runtime::*scheduler-threads*)))))

(test wasi/command-runner-err-and-exit
  ;; run returning err -> 1; exit-with-code(7) -> 7 with a clean store
  (is (eql 1 (wasmdown.wasi:run-component
              (%command-component *command-err-fixture*)
              (wasmdown.wasi:make-wasi-context))))
  (is (zerop (length wasmdown.component.runtime::*scheduler-threads*)))
  (is (eql 7 (wasmdown.wasi:run-component
              (%command-component *command-exit-fixture*)
              (wasmdown.wasi:make-wasi-context))))
  (is (zerop (length wasmdown.component.runtime::*scheduler-threads*))))

;;; ---- the CLI: a component binary file + the WASI flags ----

(defun %u32b (n)
  (let ((out '()))
    (loop while (>= n 128)
          do (push (logior 128 (logand n 127)) out) (setf n (ash n -7))
          finally (push n out) (return (coerce (nreverse out)
                                               '(vector (unsigned-byte 8)))))))

(defun %bs (&rest xs) (coerce xs '(vector (unsigned-byte 8))))
(defun %bcat (&rest vs) (apply #'concatenate '(vector (unsigned-byte 8)) vs))
(defun %bname (s) (%bcat (%u32b (length s)) (map 'vector #'char-code s)))
(defun %bna (s) (%bcat (%bs 0) (%bname s)))
(defun %bvec (&rest xs) (%bcat (%u32b (length xs)) (apply #'%bcat xs)))
(defun %bsec (id content) (%bcat (%bs id) (%u32b (length content)) content))
(defun %bwat (text)
  (wasmdown.binary:encode-module (wasmdown.text:parse-wat text)))

(defun command-component-binary ()
  "A hand-assembled command-world component: it imports
wasi:cli/stdout@0.3.2, writes \"hi\" through a stream, task.returns
ok, and exports `run' (the runner's direct-export fallback)."
  (let* ((memmod (%bwat "(module (memory (export \"mem\") 1))"))
         (mmod (%bwat
                "(module
                  (import \"env\" \"mem\" (memory 1))
                  (import \"env\" \"task.return\" (func $tr (param i32)))
                  (import \"env\" \"stream.new\" (func $snew (result i64)))
                  (import \"env\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))
                  (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))
                  (import \"env\" \"future.read\" (func $fread (param i32 i32) (result i32)))
                  (import \"env\" \"write-via-stream\" (func $wvs (param i32) (result i32)))
                  (global $fut (mut i32) (i32.const 0))
                  (func (export \"run\") (result i32)
                    (local $p i64) (local $rx i32) (local $tx i32) (local $ret i32)
                    (local.set $p (call $snew))
                    (local.set $rx (i32.wrap_i64 (local.get $p)))
                    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $p) (i64.const 32))))
                    (i32.store16 (i32.const 16) (i32.const 0x6968))
                    (global.set $fut (call $wvs (local.get $rx)))
                    (local.set $ret (call $swrite (local.get $tx) (i32.const 16) (i32.const 2)))
                    (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))
                    (call $sdropw (local.get $tx))
                    (local.set $ret (call $fread (global.get $fut) (i32.const 32)))
                    (if (i32.and (local.get $ret) (i32.const 15)) (then unreachable))
                    (call $tr (i32.const 0))
                    (i32.const 0))
                  (func (export \"run-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"))
         (types (%bvec
                 (%bcat (%bs #x6d 3) (%bname "io")
                        (%bname "illegal-byte-sequence") (%bname "pipe"))
                 (%bs #x66 1 #x7d)
                 (%bs #x6a 0 1 0)
                 (%bs #x65 1 2)
                 (%bcat (%bs #x40 1) (%bname "data") (%bs 1) (%bs 0 3))
                 (%bcat (%bs #x42 7)
                        (%bs #x02 #x03 #x02 #x01 #x00)
                        (%bcat (%bs #x04) (%bna "error-code") (%bs #x03 #x00 #x00))
                        (%bs #x01 #x6a #x00 #x01 #x00)
                        (%bs #x01 #x65 #x01 #x02)
                        (%bs #x01 #x66 #x01 #x7d)
                        (%bcat (%bs #x01 #x40 #x01) (%bname "data") (%bs #x04 #x00 #x03))
                        (%bcat (%bs #x04) (%bna "write-via-stream") (%bs #x01 #x05)))
                 (%bs #x6a 0 0)
                 (%bcat (%bs #x43 0) (%bs 0 6))))
         (import (%bvec (%bcat (%bna "wasi:cli/stdout@0.3.2") (%bs #x05 5))))
         (env-bag (%bcat (%bs #x01 7)
                         (%bcat (%bname "mem") (%bs #x02 0))
                         (%bcat (%bname "task.return") (%bs #x00 0))
                         (%bcat (%bname "stream.new") (%bs #x00 1))
                         (%bcat (%bname "stream.write") (%bs #x00 2))
                         (%bcat (%bname "stream.drop-writable") (%bs #x00 3))
                         (%bcat (%bname "future.read") (%bs #x00 4))
                         (%bcat (%bname "write-via-stream") (%bs #x00 5)))))
    (%bcat (%bs 0 #x61 #x73 #x6d #x0d 0 1 0)
           (%bsec 7 types)
           (%bsec 10 import)
           (%bsec 1 memmod)
           (%bsec 1 mmod)
           (%bsec 2 (%bvec (%bs #x00 0 0)))
           (%bsec 6 (%bvec (%bcat (%bs #x00 #x02 #x01 0) (%bname "mem"))))
           (%bsec 6 (%bvec (%bcat (%bs #x01 #x00 0) (%bname "write-via-stream"))))
           (%bsec 8 (%bvec (%bs #x09 #x00 6 #x00)   ; task.return
                           (%bs #x0e 1)              ; stream.new
                           (%bs #x10 1 #x01 #x03 0)  ; stream.write (memory 0)
                           (%bs #x14 1)              ; stream.drop-writable
                           (%bs #x16 3 #x01 #x03 0)  ; future.read (memory 0)
                           (%bs #x01 #x00 0 #x00)))  ; lower write-via-stream
           (%bsec 2 (%bvec env-bag (%bcat (%bs #x00 1 1) (%bname "env") (%bs #x12 1))))
           (%bsec 6 (%bvec (%bcat (%bs #x00 #x00 #x01 2) (%bname "run"))
                           (%bcat (%bs #x00 #x00 #x01 2) (%bname "run-cb"))))
           (%bsec 8 (%bvec (%bs #x00 #x00 6 #x02 #x06 #x07 7 7)))
           (%bsec 11 (%bvec (%bcat (%bna "run") (%bs #x01 1) (%bs 0)))))))

(defun %write-temp-bytes (name bytes)
  (let ((path (merge-pathnames name *default-pathname-defaults*)))
    (with-open-file (s path :element-type '(unsigned-byte 8)
                            :direction :output :if-exists :supersede)
      (write-sequence bytes s))
    path))

(test wasi/e2e-command-component-binary
  ;; the same component through the runner and through the decoded IR
  (let* ((bytes (command-component-binary))
         (comp (wasmdown.component:decode-component bytes))
         (sink (make-string-output-stream)))
    (is (wasmdown.component:component-p bytes))
    (wasmdown.component:validate-component comp)
    (is (eql 0 (wasmdown.wasi:run-component
                comp (wasmdown.wasi:make-wasi-context :stdout sink))))
    (is (string= "hi" (get-output-stream-string sink)))))

(test wasi/cli-run-component-wasi-flags
  (let ((path (%write-temp-bytes "tmp-cli-command.wasm"
                                 (command-component-binary))))
    (unwind-protect
         (let ((out (with-output-to-string (s)
                      (let ((*standard-output* s))
                        (is (eql 0 (wasmdown.cli:main
                                    (list "run" (namestring path)
                                          "--wasi" "--env" "HOME=/tmp"
                                          "guest-arg"))))))))
           ;; the guest's stdout.write-via-stream reached the CLI's
           ;; standard output; the guest arguments were plumbed
           (is (string= "hi" out)))
      (when (probe-file path) (delete-file path))))
  ;; a malformed --net endpoint is a host-invocation error (2); a
  ;; component run without --wasi but with guest args is a usage error
  (let ((path (%write-temp-bytes "tmp-cli-command2.wasm"
                                 (command-component-binary))))
    (unwind-protect
         (progn
           (is (eql 2 (wasmdown.cli:main
                       (list "run" (namestring path) "--wasi"
                             "--net" "nonsense"))))
           (is (eql 2 (wasmdown.cli:main
                       (list "run" (namestring path) "orphan-arg")))))
      (when (probe-file path) (delete-file path)))))

;;; ---- the CLI-side authority helpers ----

(test wasi/endpoint-parsing
  (let ((ep (wasmdown.wasi:parse-wasi-endpoint "127.0.0.1:8080")))
    (is (equal '(127 0 0 1) (wasmdown.wasi:wasi-endpoint-address ep)))
    (is (eql 8080 (wasmdown.wasi:wasi-endpoint-port ep))))
  (let ((ep (wasmdown.wasi:parse-wasi-endpoint "*:*")))
    (is (eq :any (wasmdown.wasi:wasi-endpoint-address ep)))
    (is (eq :any (wasmdown.wasi:wasi-endpoint-port ep))))
  (is (block nil
        (handler-case (progn (wasmdown.wasi:parse-wasi-endpoint "nonsense") nil)
          (wasmdown:host-error () t)))))

;;; ---------------------------------------------------------------
;;; The reactor backend (plan step 6): poll(2) + the monotonic clock +
;;; the self-pipe wake channel. Every test here injects its clock and
;;; readiness (or uses real loopback readiness that is already there),
;;; so nothing sleeps and nothing polls in a loop.

(defun reactor-fake (clock-fn poll-fn)
  "A reactor with the injected CLOCK-FN/POLL-FN and no self-pipe:
STEP never blocks, so a fake clock fully determines the outcome."
  (wasmdown.wasi:make-wasi-reactor :clock-fn clock-fn :poll-fn poll-fn
                                   :wake-p nil))

(test wasi/reactor-deadlines
  (let* ((now 1000000000)
         (reactor (reactor-fake (lambda () now) nil))
         (fired '()))
    (unwind-protect
         (progn
           ;; a zero deadline fires on the next step (no pollable fd, no
           ;; pipe: the step returns immediately and the clock decides)
           (wasmdown.wasi:reactor-watch-deadline
            reactor 0 (lambda () (push :zero fired)))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is progress)
             (is (not pending))
             (is (equal '(:zero) fired)))
           ;; a past deadline fires, a future one does not; a future
           ;; registration leaves PENDING-P true
           (setf now 5000)
           (wasmdown.wasi:reactor-watch-deadline
            reactor 4000 (lambda () (push :past fired)))
           (let ((future (wasmdown.wasi:reactor-watch-deadline
                          reactor 9000 (lambda () (push :future fired)))))
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is progress)
               (is pending)
               (is (equal '(:past :zero) fired)))
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is (not progress))
               (is pending)
               (is (equal '(:past :zero) fired)))
             ;; cancel is idempotent and removes the pending deadline
             (funcall future)
             (funcall future)
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is (not progress))
               (is (not pending))
               (is (equal '(:past :zero) fired))))
           ;; due deadlines fire in (deadline, registration) order
           (setf now 20000)
           (let ((order '()))
             (wasmdown.wasi:reactor-watch-deadline
              reactor 10000 (lambda () (push :a order)))
             (wasmdown.wasi:reactor-watch-deadline
              reactor 10000 (lambda () (push :b order)))
             (wasmdown.wasi:reactor-watch-deadline
              reactor 9000 (lambda () (push :c order)))
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is progress)
               (is (not pending))
               (is (equal (list :c :a :b) (reverse order))))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-fd-order
  ;; one pump = one callback per ready registration; readiness is
  ;; delivered in registration order, and each step repeats it
  ;; (level-triggered) until the cancel-fn runs
  (let* ((calls 0)
         (requests-seen '())
         (poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (incf calls)
                    (push (mapcar (lambda (request) (getf request :fd))
                                  requests)
                          requests-seen)
                    (mapcar (lambda (request)
                              (if (member (getf request :fd) '(7 9)) 1 0))
                            requests)))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (events '()))
    (unwind-protect
         (let ((seven (wasmdown.wasi:reactor-watch-fd
                       reactor 7 :read 'conn-7
                       (lambda (status) (push (cons 7 status) events))))
               (nine (wasmdown.wasi:reactor-watch-fd
                      reactor 9 :read 'conn-9
                      (lambda (status) (push (cons 9 status) events)))))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 5000000)
             (is progress)
             (is pending)
             (is (= 1 calls))
             ;; both ready fds were polled in registration order and
             ;; both callbacks ran within the one step
             (is (equal '((7 9)) requests-seen))
             (is (equal '((7 . :read) (9 . :read)) (reverse events))))
           ;; the second step pumps once more: one callback each, in order
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is progress)
             (is pending)
             (is (= 2 calls))
             (is (= 4 (length events)))
             (is (equal '((7 . :read) (9 . :read))
                        (subseq (reverse events) 2 4))))
           ;; cancelled watch is skipped, the other one still fires
           (funcall seven)
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is progress)
             (is pending)
             (is (= 5 (length events)))
             (is (equal '(9 . :read) (first events))))
           (funcall nine)
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (is (not progress))
             (is (not pending))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-fd-status-and-eintr
  ;; POLLERR / POLLHUP / POLLNVAL reach the callback as :error / :hup /
  ;; :nval, taking precedence over a plain readiness
  (let* ((revents-of (lambda (fd)
                       (cond ((= fd 11) 16)     ; POLLHUP
                             ((= fd 13) 8)      ; POLLERR
                             ((= fd 15) 32)     ; POLLNVAL
                             ((= fd 17) 4)      ; POLLOUT
                             (t 0))))
         (poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (mapcar (lambda (request)
                              (funcall revents-of (getf request :fd)))
                            requests)))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (seen '()))
    (unwind-protect
         (progn
           (wasmdown.wasi:reactor-watch-fd
            reactor 11 :read 'hup-fd (lambda (status) (push (cons :hup status) seen)))
           (wasmdown.wasi:reactor-watch-fd
            reactor 13 :read 'err-fd (lambda (status) (push (cons :err status) seen)))
           (wasmdown.wasi:reactor-watch-fd
            reactor 15 :read 'nval-fd (lambda (status) (push (cons :nval status) seen)))
           (wasmdown.wasi:reactor-watch-fd
            reactor 17 :write 'write-fd
            (lambda (status) (push (cons :write status) seen)))
           (wasmdown.wasi:reactor-step reactor 1000000)
           (is (equal '((:hup . :hup) (:err . :error)
                        (:nval . :nval) (:write . :write))
                      (reverse seen))))
      (wasmdown.wasi:reactor-close reactor)))
  ;; the same fd, the same direction, DIFFERENT registration ids: two
  ;; separate poll entries -> two callbacks (never coalesced)
  (let* ((poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (mapcar (constantly 1) requests)))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (seen '()))
    (unwind-protect
         (progn
           (wasmdown.wasi:reactor-watch-fd
            reactor 5 :read 'conn-a
            (lambda (status) (push (list 'a status) seen)))
           (wasmdown.wasi:reactor-watch-fd
            reactor 5 :read 'conn-b
            (lambda (status) (push (list 'b status) seen)))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 1000000)
             (is progress)
             (is pending)
             (is (= 2 (length seen)))
             (is (equal '((a :read) (b :read)) (reverse seen)))))
      (wasmdown.wasi:reactor-close reactor)))
  ;; EINTR re-polls within the same bounded step and then dispatches
  (let* ((calls 0)
         (poll-fn (lambda (requests timeout)
                    (declare (ignore requests timeout))
                    (if (= (incf calls) 1) :eintr (list 1))))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (seen '()))
    (unwind-protect
         (progn
           (wasmdown.wasi:reactor-watch-fd
            reactor 21 :read 'eintr-fd
            (lambda (status) (push status seen)))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 2000000)
             (is (= 2 calls))
             (is progress)
             (is pending)
             (is (equal '(:read) seen))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-cancel-close
  (let* ((poll-fn (lambda (requests timeout)
                    (declare (ignore timeout))
                    (mapcar (constantly 1) requests)))
         (reactor (reactor-fake (constantly 0) poll-fn))
         (fired '()))
    (unwind-protect
         (progn
           ;; cancel is idempotent and no callback runs afterwards
           (let ((cancel (wasmdown.wasi:reactor-watch-fd
                          reactor 3 :read 'cancelled
                          (lambda (status) (push status fired)))))
             (funcall cancel)
             (funcall cancel)
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 1000000)
               (is (not progress))
               (is (not pending))
               (is (null fired))))
           ;; a callback cancelling itself does not deadlock and stops
           ;; the watch after exactly one delivery
           (let ((cancel nil)
                 (count 0))
             (setf cancel
                   (wasmdown.wasi:reactor-watch-fd
                    reactor 4 :read 'self-cancel
                    (lambda (status)
                      (declare (ignore status))
                      (incf count)
                      (funcall cancel))))
             (wasmdown.wasi:reactor-step reactor 1000000)
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 1000000)
               (is (not progress))
               (is (not pending)))
             (is (= 1 count)))
           ;; close cancels every registration, is idempotent, and the
           ;; reactor refuses new watches
           (let ((nothing '()))
             (wasmdown.wasi:reactor-watch-fd
              reactor 5 :read 'closed-fd
              (lambda (status) (push status nothing)))
             (wasmdown.wasi:reactor-watch-deadline
              reactor 0 (lambda () (push :deadline nothing)))
             (wasmdown.wasi:reactor-close reactor)
             (wasmdown.wasi:reactor-close reactor)
             (multiple-value-bind (progress pending)
                 (wasmdown.wasi:reactor-step reactor 0)
               (is (not progress))
               (is (not pending)))
             (is (null nothing))
             (is (handler-case
                     (progn (wasmdown.wasi:reactor-watch-fd
                             reactor 6 :read 'after-close
                             (lambda (status) (declare (ignore status))))
                            nil)
                   (error () t)))
             (is (handler-case
                     (progn (wasmdown.wasi:reactor-watch-deadline
                             reactor 0 (lambda () nil))
                            nil)
                   (error () t)))))
      (wasmdown.wasi:reactor-close reactor))))

(test wasi/reactor-tcp-loopback
  ;; the real backend: a listening TCP socket becomes readable after a
  ;; loopback connect; one bounded step reports it (no polling loop and
  ;; no sleep in the test)
  (let* ((net (sock-network))
         (server (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (client (sock-rep (wasmdown.wasi:tcp-create
                            nil net (cons :enum "ipv4"))))
         (reactor (wasmdown.wasi:make-wasi-reactor))
         (seen '()))
    (unwind-protect
         (progn
           (is (equal '(:ok)
                      (wasmdown.wasi:tcp-bind nil net server (sock-addr 0))))
           (let ((port (sock-value-port
                        (sock-ok (wasmdown.wasi:tcp-get-local-address
                                  nil net server)))))
             (is (eq :ok (car (wasmdown.wasi:tcp-listen nil net server))))
             (is (equal '(:ok)
                        (wasmdown.wasi:tcp-connect nil net client
                                                   (sock-addr port))))
             (let* ((fd (wasmdown.wasi:wasi-tcp-socket-fd server))
                    (cancel (wasmdown.wasi:reactor-watch-fd
                             reactor fd :read 'listener
                             (lambda (status) (push status seen)))))
               (declare (ignore cancel))
               (multiple-value-bind (progress pending)
                   (wasmdown.wasi:reactor-step
                    reactor (* 2 1000 1000 1000))
                 (is progress)
                 (is pending)
                 (is (equal '(:read) seen))))))
      (wasmdown.wasi:reactor-close reactor)
      (wasmdown.wasi:close-wasi-tcp-socket client)
      (wasmdown.wasi:close-wasi-tcp-socket server))))

(test wasi/reactor-wake-self-pipe
  ;; an external producer's wake byte interrupts a blocked step
  (let ((reactor (wasmdown.wasi:make-wasi-reactor)))
    (unwind-protect
         (let* ((result nil)
                (thread
                  (sb-thread:make-thread
                   (lambda ()
                     (multiple-value-bind (progress pending)
                         (wasmdown.wasi:reactor-step
                          reactor (* 5 1000 1000 1000))
                       (setf result (list progress pending)))))))
           (is (wasmdown.wasi:reactor-wake reactor))
           (sb-thread:join-thread thread)
           (is (equal '(t nil) result)))
      (wasmdown.wasi:reactor-close reactor))))
