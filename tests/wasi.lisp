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
                          "sync" "sync-data" "set-times" "set-times-at"))
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
