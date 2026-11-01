;;;; Build the hand-authored WASI 0.3.2 (P3) command fixtures.
;;;;
;;;; No toolchain in this environment emits the @0.3.2 component ABI, so
;;;; these guests are ASSEMBLED HERE from the pinned Component binary
;;;; grammar (section ids and encodings in specs/Binary.md and
;;;; src/component.lisp). They are real command components:
;;;;
;;;;   p3-guest.wasm          no imports; run returns result ok (status 0)
;;;;   p3-import-guest.wasm   imports wasi:cli/exit@0.3.2, lowers
;;;;                          exit-with-code into its core, calls it with 7,
;;;;                          so the command's process status is 7
;;;;   p3-clock-guest.wasm    imports wasi:clocks/monotonic-clock@0.3.2 and
;;;;                          traps unless now() is monotonic
;;;;   p3-async-guest.wasm    resume-safe async run (callback, state kept in
;;;;                          memory, task.return in the callback)
;;;;   p3-stream-guest.wasm   stdout write-via-stream in canonical future
;;;;                          order; prints "hi\n"
;;;;   p3-cancel-guest.wasm   lowers the clock's async wait-for, cancels the
;;;;                          pending subtask and drops it (asserts cancelled)
;;;;   p3-random-guest.wasm   imports wasi:random/random@0.3.2 (and the
;;;;                          insecure/insecure-seed interfaces), lowers the
;;;;                          list-returning funcs with (memory ...) and
;;;;                          (realloc ...), and traps unless two 16-byte
;;;;                          reads have exact lengths and differ
;;;;   p3-deadline-guest.wasm lowers the clock's async wait-for (200 ms),
;;;;                          joins the subtask into a waitable set and
;;;;                          returns WAIT; the reactor's callback loop
;;;;                          parks until the deadline fires, and the
;;;;                          resumed callback traps unless the subtask
;;;;                          returned after at least 200 ms of clock time
;;;;                          (the bound also proves the drain oracle no
;;;;                          longer calls an armed timer a deadlock)
;;;;   p3-cli-guest.wasm      imports wasi:cli/{stdin,environment,stderr,
;;;;                          terminal-stdout}@0.3.2; asserts argv[0]/
;;;;                          marker, a --env pair, option-typed
;;;;                          get-initial-cwd and terminal-stdout none,
;;;;                          reads "stdin-probe\n" from real stdin
;;;;                          (read-via-stream + stream.read to DROPPED +
;;;;                          the paired future's ok), then writes a fixed
;;;;                          message to stderr through write-via-stream
;;;;
;;;; The importing guest is the provider-importing evidence the no-import
;;;; smoke cannot give: the runner composes the provider from the
;;;; component's DECLARED import and the guest calls through a canon lower.
;;;;
;;;; The parser-visible async command in tests/wasi.lisp
;;;; (wasi/e2e-command-component-binary) is not resume-safe; the CLI lane
;;;; pins the resume-safe replacements built here instead: a callback
;;;; command that keeps its state in memory, and the stdout stream command
;;;; (canonical future order: park the read, stage the write, drop the
;;;; writable end, waitable-set.wait on the future event).
;;;;
;;;; This is ABI coverage, not evidence of real-toolchain integration:
;;;; no @0.3.2 guest from an upstream toolchain exists here.
;;;;
;;;; Usage:
;;;;   sbcl --script tools/build-p3-fixtures.lisp           ; write both
;;;;   sbcl --script tools/build-p3-fixtures.lisp --check   ; verify hashes

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)
(load (merge-pathnames "tools/component-encode" *root*))

(defparameter *dir* (merge-pathnames "tests/fixtures/wasi/" *root*))
(defparameter *sums* (merge-pathnames "P3-CHECKSUMS" *dir*))

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
;;; Fixture 1: no imports, run returns ok.

(defun p3-sync-guest-bytes ()
  (let* ((core (b-wat "(module (func (export \"run\") (result i32)
                                  (i32.const 0)))"))
         (result-type (b-bytes #x6a #x00 #x00))      ; result<_,_>
         (func-type (b-cat (b-bytes #x40) (b-u32 0)  ; functype () -> type 0
                           (b-bytes #x00) (b-u32 0)))
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
;;; Fixture 2: imports wasi:cli/exit@0.3.2 and exits with code 7.

(defun p3-import-guest-bytes ()
  (let* ((core
           (b-wat "(module
                      (import \"env\" \"exit-with-code\"
                              (func $exit (param i32)))
                      (func (export \"run\") (result i32)
                        (call $exit (i32.const 7))
                        (i32.const 0)))"))
         (result-type (b-bytes #x6a #x00 #x00))            ; result<_,_>
         (exit-ft (b-cat (b-bytes #x40)                     ; functype
                         (b-vec (b-cat (b-name "status-code") (b-bytes #x7d))) ; param u8
                         (b-bytes #x01 #x00)))              ; no results
         (run-ft (b-cat (b-bytes #x40) (b-vec)              ; functype () ->
                        (b-bytes #x00) (b-bytes 0)))        ; result type 0
         ;; inside an INSTANCE TYPE the type-index space is the type's
         ;; own declarations: the func type is declared inline (0x01)
         ;; and the export declarator references it locally as 0.
         (exit-itype
           (b-cat (b-bytes #x42)                            ; instance type
                  (b-vec (b-cat (b-bytes #x01) exit-ft)     ; local type 0
                         (b-cat (b-bytes #x04)              ; export decl
                                (b-nameattrs "exit-with-code")
                                (b-bytes #x01 0)))))        ; func, local 0
         (types (b-vec result-type run-ft exit-itype))
         (import (b-vec (b-cat (b-nameattrs "wasi:cli/exit@0.3.2")
                               (b-bytes #x05 2))))          ; instance type 2
         ;; core instance 0: the bag handing the lowered exit to the module
         (bag (b-cat (b-bytes #x01)                         ; inline exports
                     (b-vec (b-cat (b-name "exit-with-code")
                                   (b-bytes #x00 0)))))     ; core func 0
         ;; core instance 1: instantiate module 0 with env = bag 0
         (m-inst (b-cat (b-bytes #x00 0)                    ; instantiate module
                        (b-vec (b-cat (b-name "env")
                                      (b-bytes #x12 0)))))) ; arg = instance 0
    (b-cat (b-bytes 0 #x61 #x73 #x6d #x0d 0 1 0)
           (b-sec 7 types)
           (b-sec 10 import)
           (b-sec 1 core)
           (b-sec 6 (b-vec (b-cat (b-bytes #x01 #x00 0)     ; alias import export
                                  (b-name "exit-with-code"))))
           (b-sec 8 (b-vec (b-bytes #x01 #x00 0 0)))        ; lower exit
           (b-sec 2 (b-vec bag))                            ; hands it to the core
           (b-sec 2 (b-vec m-inst))
           (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x00 #x01 1)
                                  (b-name "run"))))
           (b-sec 8 (b-vec (b-bytes #x00 #x00 1 0 1)))      ; lift run: type 1
           (b-sec 5 (b-vec (b-cat (b-bytes #x01)            ; inline exports
                                  (b-vec (b-cat (b-nameattrs "run")
                                                (b-bytes #x01 1)))))) ; comp func 1
           (b-sec 11 (b-vec (b-cat (b-nameattrs "wasi:cli/run@0.3.2")
                                   ;; imported exit is instance 0, the
                                   ;; defined run instance is instance 1
                                   (b-bytes #x05 1) (b-bytes 0)))))))

;;; ---------------------------------------------------------------
;;; Fixture 3: imports wasi:clocks/monotonic-clock@0.3.2 and checks the
;;; clock is monotonic across two calls (a broken provider traps).

(defun p3-clock-guest-bytes ()
  (let* ((core
           (b-wat "(module
                      (import \"env\" \"now\" (func $now (result i64)))
                      (func (export \"run\") (result i32)
                        (local $a i64) (local $b i64)
                        (local.set $a (call $now))
                        (local.set $b (call $now))
                        (if (i64.lt_u (local.get $b) (local.get $a))
                            (then unreachable))
                        (i32.const 0)))"))
         (result-type (b-bytes #x6a #x00 #x00))            ; result<_,_>
         (run-ft (b-cat (b-bytes #x40) (b-vec)              ; functype () ->
                        (b-bytes #x00) (b-bytes 0)))        ; result type 0
         ;; mark is a u64 alias in the pinned WIT; the provider's func is
         ;; flat `() -> i64`, so the instance type declares that functype.
         (now-ft (b-cat (b-bytes #x40) (b-vec)
                        (b-bytes #x00) (b-bytes #x77)))     ; -> u64
         (clock-itype
           (b-cat (b-bytes #x42)
                  (b-vec (b-cat (b-bytes #x01) now-ft)      ; local type 0
                         (b-cat (b-bytes #x04)
                                (b-nameattrs "now")
                                (b-bytes #x01 0)))))
         (types (b-vec result-type run-ft clock-itype))
         (import (b-vec (b-cat (b-nameattrs
                                "wasi:clocks/monotonic-clock@0.3.2")
                               (b-bytes #x05 2))))
         (bag (b-cat (b-bytes #x01)
                     (b-vec (b-cat (b-name "now")
                                   (b-bytes #x00 0)))))      ; core func 0
         (m-inst (b-cat (b-bytes #x00 0)
                        (b-vec (b-cat (b-name "env")
                                      (b-bytes #x12 0))))))
    (b-cat (b-bytes 0 #x61 #x73 #x6d #x0d 0 1 0)
           (b-sec 7 types)
           (b-sec 10 import)
           (b-sec 1 core)
           (b-sec 6 (b-vec (b-cat (b-bytes #x01 #x00 0)
                                  (b-name "now"))))
           (b-sec 8 (b-vec (b-bytes #x01 #x00 0 0)))        ; lower now
           (b-sec 2 (b-vec bag))
           (b-sec 2 (b-vec m-inst))
           (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x00 #x01 1)
                                  (b-name "run"))))
           (b-sec 8 (b-vec (b-bytes #x00 #x00 1 0 1)))      ; lift run: type 1
           (b-sec 5 (b-vec (b-cat (b-bytes #x01 1)
                                  (b-nameattrs "run")
                                  (b-bytes #x01 1))))
           (b-sec 11 (b-vec (b-cat (b-nameattrs "wasi:cli/run@0.3.2")
                                   (b-bytes #x05 1) (b-bytes 0)))))))

;;; ---------------------------------------------------------------
;;; Fixture 4: an ASYNC command (resume-safe). run is lifted async with
;;; a callback: the core run returns YIELD, the callback is invoked on
;;; the resume and calls task.return(ok) before returning EXIT.

(defun p3-async-guest-bytes ()
  (let* ((core
           (b-wat "(module
                      (import \"env\" \"task.return\"
                              (func $task.return (param i32)))
                      (type $cb (func (param i32 i32 i32) (result i32)))
                      (func (export \"run\") (result i32)
                        (i32.const 1))
                      (func (export \"cb\") (type $cb)
                        (call $task.return (i32.const 0))
                        (i32.const 0)))"))
         (result-type (b-bytes #x6a #x00 #x00))
         (run-ft (b-cat (b-bytes #x43) (b-vec)            ; async () ->
                        (b-bytes #x00) (b-bytes 0)))
         (types (b-vec result-type run-ft))
         ;; task.return: 0x09 resultlist opts (the flat result is the i32
         ;; variant discriminant of result<_,_>)
         (taskret-canon (b-sec 8 (b-vec (b-cat (b-bytes #x09 #x00 #x00)
                                                (b-vec)))))
         (bag (b-sec 2 (b-vec (b-cat (b-bytes #x01)
                                     (b-vec (b-cat (b-name "task.return")
                                                   (b-bytes #x00 0)))))))
         (m-inst (b-sec 2 (b-vec (b-cat (b-bytes #x00 0)
                                        (b-vec (b-cat (b-name "env")
                                                      (b-bytes #x12 0)))))))
         (aliases (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x00 #x01 1)
                                         (b-name "run"))
                                  (b-cat (b-bytes #x00 #x00 #x01 1)
                                         (b-name "cb")))))
         (lift-canon (b-sec 8 (b-vec (b-cat (b-bytes #x00 #x00 1)
                                            (b-vec (b-bytes #x06)   ; async
                                                   (b-cat (b-bytes #x07)
                                                          (b-u32 2))) ; callback
                                            (b-bytes 1)))))
         (compinst (b-sec 5 (b-vec (b-cat (b-bytes #x01)
                                          (b-vec (b-cat (b-nameattrs "run")
                                                        (b-bytes #x01 0)))))))
         (export (b-sec 11 (b-vec (b-cat (b-nameattrs "wasi:cli/run@0.3.2")
                                         (b-bytes #x05 0) (b-bytes 0))))))
    (b-cat (b-bytes 0 #x61 #x73 #x6d #x0d 0 1 0)
           (b-sec 7 types)
           (b-sec 1 core)
           taskret-canon
           bag
           m-inst
           aliases
           lift-canon
           compinst
           export)))

;;; ---------------------------------------------------------------
;;; Fixture 5: the stdout stream ABI. run is lifted async with a
;;; callback; the callback uses the CANONICAL future order: park the
;;; future read first, stage the write, drop the writable end (the
;;; provider's future resolves on that drop), then waitable-set.wait
;;; for the future event (4). A WOULD-BLOCK write is waited on through
;;; its own event (3) before the drop — the worker-backed stdout path.
(defun p3-stream-guest-bytes ()
  (let* ((mem-core (b-wat "(module (memory (export \"mem\") 1))"))
         (core (b-wat "(module
      (import \"env\" \"task.return\" (func $task.return (param i32)))
      (import \"env\" \"stream.new\" (func $stream.new (result i64)))
      (import \"env\" \"stream.write\" (func $stream.write (param i32 i32 i32) (result i32)))
      (import \"env\" \"stream.drop-writable\" (func $dropw (param i32)))
      (import \"env\" \"write-via-stream\" (func $writevia (param i32) (result i32)))
      (import \"env\" \"future.read\" (func $future.read (param i32 i32) (result i32)))
      (import \"env\" \"future.drop-readable\" (func $dropf (param i32)))
      (import \"env\" \"stream.drop-readable\" (func $dropr (param i32)))
      (import \"env\" \"waitable-set.new\" (func $wsetnew (result i32)))
      (import \"env\" \"waitable.join\" (func $wjoin (param i32 i32)))
      (import \"env\" \"waitable-set.wait\" (func $wsetwait (param i32 i32) (result i32)))
      (import \"env\" \"mem\" (memory 1))
      (global $fut (mut i32) (i32.const 0))
      (global $readable (mut i32) (i32.const 0))
      (global $writable (mut i32) (i32.const 0))
      (global $set (mut i32) (i32.const 0))
      (global $state (mut i32) (i32.const 0))
      (type $cbty (func (param i32 i32 i32) (result i32)))
      (func (export \"run\") (result i32) (i32.const 1))
      (func (export \"cb\") (type $cbty)
        (local $s i64) (local $ret i32)
        (block $done
          (block $st10
            (block $st20
              (br_table $done $st10 $st20 (i32.div_u (global.get $state) (i32.const 10))))
            ;; state 20: the future's event (4) arrived
            (if (i32.ne (i32.const 0) (i32.load (i32.const 192)))
                (then unreachable))
            (call $task.return (i32.const 0))
            (return (i32.const 0)))
          ;; state 10: the write's event (3) arrived; drop and await the future
          (call $dropw (global.get $writable))
          (global.set $state (i32.const 20))
          (local.set $ret (call $wsetwait (global.get $set) (i32.const 224)))
          (if (i32.ne (local.get $ret) (i32.const 4))
              (then unreachable))
          (if (i32.ne (i32.const 0) (i32.load (i32.const 192)))
              (then unreachable))
          (call $task.return (i32.const 0))
          (return (i32.const 0)))
        ;; state 0
        (local.set $s (call $stream.new))
        (global.set $readable (i32.wrap_i64 (local.get $s)))
        (global.set $writable (i32.wrap_i64 (i64.shr_u (local.get $s) (i64.const 32))))
        (global.set $fut (call $writevia (global.get $readable)))
        (global.set $set (call $wsetnew))
        (call $wjoin (global.get $fut) (global.get $set))
        (local.set $ret (call $future.read (global.get $fut) (i32.const 192)))
        (if (i32.ne (local.get $ret) (i32.const -1))
            (then unreachable))
        (i32.store8 (i32.const 64) (i32.const 104))
        (i32.store8 (i32.const 65) (i32.const 105))
        (i32.store8 (i32.const 66) (i32.const 10))
        (local.set $ret (call $stream.write (global.get $writable) (i32.const 64) (i32.const 3)))
        (if (i32.and (local.get $ret) (i32.const 15))
            (then
              ;; write blocked: join it and wait for event 3
              (call $wjoin (global.get $writable) (global.get $set))
              (global.set $state (i32.const 10))
              (local.set $ret (call $wsetwait (global.get $set) (i32.const 224)))
              (if (i32.ne (local.get $ret) (i32.const 3))
                  (then unreachable))
              (call $dropw (global.get $writable))
              (global.set $state (i32.const 20))
              (local.set $ret (call $wsetwait (global.get $set) (i32.const 224)))
              (if (i32.ne (local.get $ret) (i32.const 4))
                  (then unreachable))
              (if (i32.ne (i32.const 0) (i32.load (i32.const 192)))
                  (then unreachable))
              (call $task.return (i32.const 0))
              (return (i32.const 0))))
        ;; write completed eagerly: drop and await the future
        (call $dropw (global.get $writable))
        (global.set $state (i32.const 20))
        (local.set $ret (call $wsetwait (global.get $set) (i32.const 224)))
        (if (i32.ne (local.get $ret) (i32.const 4))
            (then unreachable))
        (if (i32.ne (i32.const 0) (i32.load (i32.const 192)))
            (then unreachable))
        (call $task.return (i32.const 0))
        (i32.const 0)))"))
         (enum-ty (b-cat (b-bytes #x6d)
                         (b-vec (b-name "io") (b-name "illegal-byte-sequence") (b-name "pipe"))))
         (stream-ty (b-bytes #x66 #x01 #x7d))
         (result-ty (b-cat (b-bytes #x6a #x00 #x01) (b-bytes 0)))
         (future-ty (b-cat (b-bytes #x65 #x01) (b-bytes 2)))
         (wvs-itype
           (b-cat (b-bytes #x42)
                  (b-vec (b-cat (b-bytes #x02 #x03 #x02) (b-bytes 1 0))
                         (b-cat (b-bytes #x04) (b-nameattrs "error-code") (b-bytes #x03 #x00 0))
                         (b-cat (b-bytes #x01) stream-ty)
                         (b-cat (b-bytes #x01) (b-cat (b-bytes #x6a #x00 #x01) (b-bytes 0)))
                         (b-cat (b-bytes #x01) (b-cat (b-bytes #x65 #x01) (b-bytes 3)))
                         (b-cat (b-bytes #x01)
                                (b-cat (b-bytes #x40)
                                       (b-vec (b-cat (b-name "data") (b-bytes 2)))
                                       (b-bytes #x00) (b-bytes 4)))
                         (b-cat (b-bytes #x04) (b-nameattrs "write-via-stream") (b-bytes #x01 5)))))
         (run-result (b-bytes #x6a #x00 #x00))
         (run-ft (b-cat (b-bytes #x43) (b-vec) (b-bytes #x00) (b-bytes 5)))
         (types (b-vec enum-ty stream-ty result-ty future-ty wvs-itype run-result run-ft))
         (import (b-vec (b-cat (b-nameattrs "wasi:cli/stdout@0.3.2") (b-bytes #x05 4))))
         (builtins
           (b-sec 8
             (b-vec (b-cat (b-bytes #x09 #x00 #x05) (b-vec))
                    (b-cat (b-bytes #x0e) (b-u32 1))
                    (b-cat (b-bytes #x10) (b-u32 1) (b-vec (b-bytes #x06) (b-cat (b-bytes #x03) (b-u32 0))))
                    (b-cat (b-bytes #x14) (b-u32 1))
                    (b-cat (b-bytes #x16) (b-u32 3) (b-vec (b-bytes #x06) (b-cat (b-bytes #x03) (b-u32 0))))
                    (b-cat (b-bytes #x1a) (b-u32 3))
                    (b-cat (b-bytes #x13) (b-u32 1))
                    (b-bytes #x1f)
                    (b-bytes #x23)
                    (b-cat (b-bytes #x20 #x00 #x00)))))
         (alias-export (b-sec 6
                         (b-vec (b-cat (b-bytes #x01 #x00 0) (b-name "write-via-stream")))))
         (lower (b-sec 8 (b-vec (b-bytes #x01 #x00 0 0))))
         (bag (b-sec 2
                (b-vec (b-cat (b-bytes #x01)
                              (b-vec (b-cat (b-name "task.return") (b-bytes #x00 0))
                                     (b-cat (b-name "stream.new") (b-bytes #x00 1))
                                     (b-cat (b-name "stream.write") (b-bytes #x00 2))
                                     (b-cat (b-name "stream.drop-writable") (b-bytes #x00 3))
                                     (b-cat (b-name "write-via-stream") (b-bytes #x00 10))
                                     (b-cat (b-name "future.read") (b-bytes #x00 4))
                                     (b-cat (b-name "future.drop-readable") (b-bytes #x00 5))
                                     (b-cat (b-name "stream.drop-readable") (b-bytes #x00 6))
                                     (b-cat (b-name "waitable-set.new") (b-bytes #x00 7))
                                     (b-cat (b-name "waitable.join") (b-bytes #x00 8))
                                     (b-cat (b-name "waitable-set.wait") (b-bytes #x00 9))
                                     (b-cat (b-name "mem") (b-bytes #x02 0)))))))
         (m-inst (b-sec 2 (b-vec (b-cat (b-bytes #x00 1)
                                        (b-vec (b-cat (b-name "env") (b-bytes #x12 1)))))))
         (aliases (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x00 #x01 2) (b-name "run"))
                                  (b-cat (b-bytes #x00 #x00 #x01 2) (b-name "cb")))))
         (lift (b-sec 8 (b-vec (b-cat (b-bytes #x00 #x00 11)
                                      (b-vec (b-bytes #x06) (b-cat (b-bytes #x07) (b-u32 12)))
                                      (b-bytes 6)))))
         (compinst (b-sec 5 (b-vec (b-cat (b-bytes #x01)
                                          (b-vec (b-cat (b-nameattrs "run") (b-bytes #x01 1)))))))
         (export (b-sec 11 (b-vec (b-cat (b-nameattrs "wasi:cli/run@0.3.2")
                                         (b-bytes #x05 1) (b-bytes 0))))))
    (b-cat (b-bytes 0 #x61 #x73 #x6d #x0d 0 1 0)
           (b-sec 7 types)
           (b-sec 10 import)
           (b-sec 1 mem-core)
           (b-sec 1 core)
           (b-sec 2 (b-vec (b-cat (b-bytes #x00 0) (b-vec))))
           (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x02 #x01 0) (b-name "mem"))))
           builtins alias-export lower bag m-inst aliases lift compinst export)))

;;; ---------------------------------------------------------------
;;; Fixture 6: async CANCELLATION. The callback lowers
;;; wasi:clocks/monotonic-clock@0.3.2's async `wait-for` with the
;;; `async` canon option, gets a pending subtask, cancels it
;;; synchronously (the state must be one of the cancelled states)
;;; and drops it before task.return. A 1 s wait that finishes in
;;; ~0.2 s proves the cancelling path ran.
(defun p3-cancel-guest-bytes ()
  (let* ((core (b-wat "(module
      (import \"env\" \"task.return\" (func $tr (param i32)))
      (import \"env\" \"wait-for\" (func $waitfor (param i64) (result i32)))
      (import \"env\" \"subtask.cancel\" (func $cancel (param i32) (result i32)))
      (import \"env\" \"subtask.drop\" (func $drop (param i32)))
      (type $cbty (func (param i32 i32 i32) (result i32)))
      (func (export \"run\") (result i32) (i32.const 1))
      (func (export \"cb\") (type $cbty)
        (local $p i32) (local $s i32) (local $r i32)
        (local.set $p (call $waitfor (i64.const 1000000000)))
        (if (i32.ne (i32.and (local.get $p) (i32.const 15)) (i32.const 0))
            (then (local.set $s (i32.shr_u (local.get $p) (i32.const 4)))
                  (local.set $r (call $cancel (local.get $s)))
                  (if (i32.or (i32.eq (local.get $r) (i32.const 3))
                              (i32.eq (local.get $r) (i32.const 4)))
                      (then (call $drop (local.get $s)))
                      (else unreachable))))
        (call $tr (i32.const 0))
        (i32.const 0)))"))
         (result-type (b-bytes #x6a #x00 #x00))
         (run-ft (b-cat (b-bytes #x43) (b-vec) (b-bytes #x00) (b-bytes 0)))
         ;; wait-for(how-long: duration) async
         (wait-ft (b-cat (b-bytes #x43)
                         (b-vec (b-cat (b-name "how-long") (b-bytes #x77)))
                         (b-bytes #x01 #x00)))
         (clock-itype (b-cat (b-bytes #x42)
                             (b-vec (b-cat (b-bytes #x01) wait-ft)
                                    (b-cat (b-bytes #x04)
                                           (b-nameattrs "wait-for")
                                           (b-bytes #x01 0)))))
         (types (b-vec result-type run-ft clock-itype))
         (import (b-vec (b-cat (b-nameattrs "wasi:clocks/monotonic-clock@0.3.2")
                               (b-bytes #x05 2))))
         (builtins (b-sec 8 (b-vec (b-cat (b-bytes #x09 #x00 #x00) (b-vec))
                                   (b-bytes #x06 #x00)
                                   (b-bytes #x0d))))
         (alias-export (b-sec 6 (b-vec (b-cat (b-bytes #x01 #x00 0) (b-name "wait-for")))))
         (lower (b-sec 8 (b-vec (b-cat (b-bytes #x01 #x00 0) (b-vec (b-bytes #x06))))))
         (bag (b-sec 2 (b-vec (b-cat (b-bytes #x01)
                                     (b-vec (b-cat (b-name "task.return") (b-bytes #x00 0))
                                            (b-cat (b-name "subtask.cancel") (b-bytes #x00 1))
                                            (b-cat (b-name "subtask.drop") (b-bytes #x00 2))
                                            (b-cat (b-name "wait-for") (b-bytes #x00 3)))))))
         (m-inst (b-sec 2 (b-vec (b-cat (b-bytes #x00 0)
                                        (b-vec (b-cat (b-name "env") (b-bytes #x12 0)))))))
         (aliases (b-sec 6 (b-vec (b-cat (b-bytes #x00 #x00 #x01 1) (b-name "run"))
                                  (b-cat (b-bytes #x00 #x00 #x01 1) (b-name "cb")))))
         (lift (b-sec 8 (b-vec (b-cat (b-bytes #x00 #x00 4)
                                      (b-vec (b-bytes #x06) (b-cat (b-bytes #x07) (b-u32 5)))
                                      (b-bytes 1)))))
         (compinst (b-sec 5 (b-vec (b-cat (b-bytes #x01)
                                          (b-vec (b-cat (b-nameattrs "run") (b-bytes #x01 1)))))))
         (export (b-sec 11 (b-vec (b-cat (b-nameattrs "wasi:cli/run@0.3.2")
                                         (b-bytes #x05 1) (b-bytes 0))))))
    (b-cat (b-bytes 0 #x61 #x73 #x6d #x0d 0 1 0)
           (b-sec 7 types)
           (b-sec 10 import)
           (b-sec 1 core)
           builtins alias-export lower bag m-inst aliases lift compinst export)))


;;; ---------------------------------------------------------------
;;; Fixture 7: wasi:random. TEXT authored (tools/component-encode) rather
;;; than byte-assembled: the list results need a memory and a realloc,
;;; and the text encoder round-trips those options exactly.
;;;
;;; The canonical LOWER core signature is derived from CanonicalABI's
;;; flatten rules (MAX_FLAT_RESULTS = 1) and confirmed by
;;; validate/link errors:
;;;
;;;   get-random-bytes(max-len: u64) -> list<u8>
;;;     flat params  [i64]; flat results [i32 i32] (ptr,len) > 1
;;;     => core (param i64 i32)  — the trailing i32 is the caller's
;;;        retptr; the lowered core func returns nothing and the
;;;        callee writes ptr@retptr, len@retptr+4.
;;;   get-random-u64() -> u64
;;;     one flat result => core (result i64), no memory needed.
;;;   get-insecure-seed() -> tuple<u64,u64>
;;;     flat results [i64 i64] > 1 => core (param i32) retptr, the two
;;;     u64 fields stored at retptr and retptr+8.
;;;
;;; The list results are lowered with BOTH (memory (core memory
;;; $mem "mem")) and (realloc (core func $mem "realloc")); the realloc
;;; is a bump allocator (i32,i32,i32,i32)->i32 that never returns a
;;; region below its 1024-byte base, so the caller's retptr slots at
;;; 0..31 and the two returned buffers cannot alias.
(defparameter *p3-random-text*
  (format nil "~{~a~%~}"
          (list
           "(component"
   "  (type $res (result))"
   "  (import \"wasi:random/random@0.3.2\""
   "    (instance $random"
   "      (export \"get-random-bytes\""
   "        (func (param \"max-len\" u64) (result (list u8))))"
   "      (export \"get-random-u64\" (func (result u64)))))"
   "  (import \"wasi:random/insecure@0.3.2\""
   "    (instance $insecure"
   "      (export \"get-insecure-random-bytes\""
   "        (func (param \"max-len\" u64) (result (list u8))))"
   "      (export \"get-insecure-random-u64\" (func (result u64)))))"
   "  (import \"wasi:random/insecure-seed@0.3.2\""
   "    (instance $seed"
   "      (export \"get-insecure-seed\" (func (result (tuple u64 u64))))))"
   "  (alias export $random \"get-random-bytes\" (func $bytes))"
   "  (alias export $random \"get-random-u64\" (func $u64))"
   "  (alias export $insecure \"get-insecure-random-bytes\" (func $ibytes))"
   "  (alias export $insecure \"get-insecure-random-u64\" (func $iu64))"
   "  (alias export $seed \"get-insecure-seed\" (func $seedfn))"
   "  (core module $Mem"
   "    (memory (export \"mem\") 1)"
   "    (global $bump (mut i32) (i32.const 1024))"
   "    (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
   "      (local $p i32)"
   "      (local.set $p (global.get $bump))"
   "      (global.set $bump"
   "        (i32.and (i32.add (global.get $bump)"
   "                          (i32.add (local.get 3) (i32.const 7)))"
   "                 (i32.const -8)))"
   "      (local.get $p)))"
   "  (core instance $mem (instantiate $Mem))"
   "  (core func $bytes-lowered"
   "    (canon lower (func $bytes)"
   "      (memory (core memory $mem \"mem\"))"
   "      (realloc (core func $mem \"realloc\"))))"
   "  (core func $u64-lowered (canon lower (func $u64)))"
   "  (core func $ibytes-lowered"
   "    (canon lower (func $ibytes)"
   "      (memory (core memory $mem \"mem\"))"
   "      (realloc (core func $mem \"realloc\"))))"
   "  (core func $iu64-lowered (canon lower (func $iu64)))"
   "  (core func $seed-lowered"
   "    (canon lower (func $seedfn)"
   "      (memory (core memory $mem \"mem\"))))"
   "  (core module $M"
   "    (import \"env\" \"mem\" (memory 1))"
   "    (import \"env\" \"get-random-bytes\" (func $bytes (param i64 i32)))"
   "    (import \"env\" \"get-random-u64\" (func $u64 (result i64)))"
   "    (import \"env\" \"get-insecure-random-bytes\" (func $ibytes (param i64 i32)))"
   "    (import \"env\" \"get-insecure-random-u64\" (func $iu64 (result i64)))"
   "    (import \"env\" \"get-insecure-seed\" (func $seed (param i32)))"
   "    (func $memcmp16 (param $a i32) (param $b i32) (result i32)"
   "      (local $i i32) (local $diff i32)"
   "      (block $done"
   "        (loop $l"
   "          (br_if $done (i32.ge_u (local.get $i) (i32.const 16)))"
   "          (if (i32.ne (i32.load8_u (i32.add (local.get $a) (local.get $i)))"
   "                      (i32.load8_u (i32.add (local.get $b) (local.get $i))))"
   "              (then (local.set $diff (i32.const 1)) (br $done)))"
   "          (local.set $i (i32.add (local.get $i) (i32.const 1)))"
   "          (br $l)))"
   "      (local.get $diff))"
   "    (func (export \"run\") (result i32)"
   "      (local $p1 i32) (local $p2 i32)"
   "      ;; secure bytes: two 16-byte reads, lengths exact, contents differ"
   "      (call $bytes (i64.const 16) (i32.const 0))"
   "      (local.set $p1 (i32.load (i32.const 0)))"
   "      (if (i32.ne (i32.load (i32.const 4)) (i32.const 16)) (then unreachable))"
   "      (call $bytes (i64.const 16) (i32.const 0))"
   "      (local.set $p2 (i32.load (i32.const 0)))"
   "      (if (i32.ne (i32.load (i32.const 4)) (i32.const 16)) (then unreachable))"
   "      (if (i32.eqz (call $memcmp16 (local.get $p1) (local.get $p2)))"
   "          (then unreachable))"
   "      ;; secure u64 completes"
   "      (drop (call $u64))"
   "      ;; insecure bytes: same shape"
   "      (call $ibytes (i64.const 16) (i32.const 0))"
   "      (local.set $p1 (i32.load (i32.const 0)))"
   "      (if (i32.ne (i32.load (i32.const 4)) (i32.const 16)) (then unreachable))"
   "      (call $ibytes (i64.const 16) (i32.const 0))"
   "      (local.set $p2 (i32.load (i32.const 0)))"
   "      (if (i32.ne (i32.load (i32.const 4)) (i32.const 16)) (then unreachable))"
   "      (if (i32.eqz (call $memcmp16 (local.get $p1) (local.get $p2)))"
   "          (then unreachable))"
   "      (drop (call $iu64))"
   "      ;; insecure-seed: tuple<u64,u64> through the retptr"
   "      (call $seed (i32.const 16))"
   "      (i32.const 0)))"
   "  (core instance $m"
   "    (instantiate $M"
   "      (with \"env\""
   "        (instance"
   "          (export \"mem\" (memory $mem \"mem\"))"
   "          (export \"get-random-bytes\" (func $bytes-lowered))"
   "          (export \"get-random-u64\" (func $u64-lowered))"
   "          (export \"get-insecure-random-bytes\" (func $ibytes-lowered))"
   "          (export \"get-insecure-random-u64\" (func $iu64-lowered))"
   "          (export \"get-insecure-seed\" (func $seed-lowered))))))"
   "  (type $run (func (result $res)))"
   "  (func $run (type $run) (canon lift (core func $m \"run\")))"
   "  (instance $cli-run (export \"run\" (func $run)))"
   "  (export \"wasi:cli/run@0.3.2\" (instance $cli-run)))")))

(defun p3-random-guest-bytes ()
  (component-encode:encode-component
   (wasmdown.component.text::parse-component-text *p3-random-text*)))

;;; ---------------------------------------------------------------
;;; Fixture 8: async DEADLINE COMPLETION — the guest OBSERVES the
;;; clock wait finishing through the CLI reactor, rather than
;;; cancelling it (fixture 6) or only reading `now` (fixture 3).
;;;
;;; The callback lowers wasi:clocks/monotonic-clock@0.3.2's
;;; `wait-for: async func(how-long: duration)` with the `async`
;;; canonopt, records `now` as t0, joins the pending subtask into a
;;; fresh waitable set, and returns WAIT (callback code 2, the set
;;; handle in the high bits). The reactor's callback loop then parks
;;; the task (wait_from_callback) until the deadline fires and the
;;; subtask resolves RETURNED; the resumed callback requires the
;;; event to be SUBTASK(1)/RETURNED(2) of ITS subtask, reads `now`
;;; again and traps unless t1 - t0 >= the 200 ms it asked for. A
;;; wait that completes early is a trap, never a pass.
;;;
;;; No memory is needed: the WAIT protocol hands the event to the
;;; callback as core arguments (the event STORE belongs only to a
;;; guest-called waitable-set.wait, which this guest never makes).
;;;
;;; THE 200 ms BOUND IS THE LIVENESS PROOF: the async lift's CLI
;;; caller drain once declared a deadlock after 50 quiescent ~2 ms
;;; poll slices (~117 ms without progress), so a park longer than
;;; that tripped the oracle instead of observing the deadline
;;; (measured before the fix: 100 ms passed, 150 ms deadlock-trapped).
;;; The oracle now treats an ARMED reactor deadline as future
;;; progress, so the fixture asks for 200 ms deliberately: comfortably
;;; past the old 50-cycle budget, yet short enough to keep the lane
;;; fast. The elapsed check only requires >= 200 ms, so an overshoot
;;; (a loaded machine) still passes.
(defparameter *p3-deadline-text*
  (format nil "~{~a~%~}"
          (list
           "(component"
   "  (import \"wasi:clocks/monotonic-clock@0.3.2\""
   "    (instance $clock"
   "      (export \"now\" (func (result u64)))"
   "      (export \"wait-for\" (func async (param \"how-long\" u64)))))"
   "  (alias export $clock \"now\" (func $now))"
   "  (alias export $clock \"wait-for\" (func $waitfor))"
   ;; wait-for is async: MAX_FLAT_ASYNC_PARAMS keeps the single i64
   ;; argument flat, so the lowered core func is (param i64) -> i32
   ;; (the packed subtask state) with no memory/realloc option.
   "  (core func $now-lowered (canon lower (func $now)))"
   "  (core func $waitfor-lowered (canon lower (func $waitfor) async))"
   "  (canon task.return (result (result)) (core func $tr))"
   "  (canon waitable-set.new (core func $wsnew))"
   "  (canon waitable.join (core func $join))"
   "  (canon waitable-set.drop (core func $wsdrop))"
   "  (canon subtask.drop (core func $subdrop))"
   "  (core module $M"
   "    (import \"env\" \"task.return\" (func $tr (param i32)))"
   "    (import \"env\" \"now\" (func $now (result i64)))"
   "    (import \"env\" \"wait-for\" (func $waitfor (param i64) (result i32)))"
   "    (import \"env\" \"waitable-set.new\" (func $wsnew (result i32)))"
   "    (import \"env\" \"waitable.join\" (func $join (param i32 i32)))"
   "    (import \"env\" \"waitable-set.drop\" (func $wsdrop (param i32)))"
   "    (import \"env\" \"subtask.drop\" (func $subdrop (param i32)))"
   "    (global $t0 (mut i64) (i64.const 0))"
   "    (global $sub (mut i32) (i32.const 0))"
   "    (global $ws (mut i32) (i32.const 0))"
   ;; YIELD once: the callback loop then enters the callback with the
   ;; no-event tuple (0 0 0)
   "    (func (export \"run\") (result i32) (i32.const 1))"
   "    (func (export \"run-cb\")"
   "          (param $code i32) (param $si i32) (param $payload i32)"
   "          (result i32)"
   "      (local $p i32) (local $t1 i64)"
   "      (if (i32.eqz (local.get $code))"
   "          (then"
   "            (global.set $t0 (call $now))"
   "            (local.set $p (call $waitfor (i64.const 200000000)))"
   ;; a 200 ms wait must still be pending: STARTING(0) or STARTED(1);
   ;; RETURNED(2) or a cancelled state is a premature completion
   "            (if (i32.gt_u (i32.and (local.get $p) (i32.const 15))"
   "                          (i32.const 1))"
   "                (then unreachable))"
   "            (global.set $sub (i32.shr_u (local.get $p) (i32.const 4)))"
   "            (global.set $ws (call $wsnew))"
   "            (call $join (global.get $sub) (global.get $ws))"
   ;; WAIT (code 2) on the set: the reactor parks until the deadline
   ;; fires and the subtask resolves
   "            (return (i32.or (i32.shl (global.get $ws) (i32.const 4))"
   "                            (i32.const 2)))))"
   ;; resumed WITH an event: only SUBTASK(1)/RETURNED(2) for OUR
   ;; subtask may complete the wait
   "      (if (i32.ne (local.get $code) (i32.const 1)) (then unreachable))"
   "      (if (i32.ne (local.get $payload) (i32.const 2)) (then unreachable))"
   "      (if (i32.ne (local.get $si) (global.get $sub)) (then unreachable))"
   "      (local.set $t1 (call $now))"
   "      (if (i64.lt_u (i64.sub (local.get $t1) (global.get $t0))"
   "                    (i64.const 200000000))"
   "          (then unreachable))"
   "      (call $join (global.get $sub) (i32.const 0))"
   "      (call $subdrop (global.get $sub))"
   "      (call $wsdrop (global.get $ws))"
   "      (call $tr (i32.const 0))"
   "      (i32.const 0)))"
   "  (core instance $m"
   "    (instantiate $M"
   "      (with \"env\""
   "        (instance"
   "          (export \"task.return\" (func $tr))"
   "          (export \"now\" (func $now-lowered))"
   "          (export \"wait-for\" (func $waitfor-lowered))"
   "          (export \"waitable-set.new\" (func $wsnew))"
   "          (export \"waitable.join\" (func $join))"
   "          (export \"waitable-set.drop\" (func $wsdrop))"
   "          (export \"subtask.drop\" (func $subdrop))))))"
   "  (func $run (export \"run\") async (result (result))"
   "    (canon lift (core func $m \"run\") async"
   "                (callback (core func $m \"run-cb\"))))"
   "  (instance $runiface (export \"run\" (func $run)))"
   "  (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
   "    (instance (export \"run\" (func async (result (result)))))))")))

(defun p3-deadline-guest-bytes ()
  (component-encode:encode-component
   (wasmdown.component.text::parse-component-text *p3-deadline-text*)))

;;; ---------------------------------------------------------------
;;; Fixture 9: the CLI consumer surface — environment, stderr and the
;;; terminal authority (docs/design/PLAN.md §9.3's CLI guest coverage
;;; row, which only had exit/stdout).
;;;
;;; Imports (exact `cli.wit' @0.3.2 signatures, specs/wasi/
;;; wasi-wit-0.3.2/cli/cli.wit):
;;;
;;;   wasi:cli/environment@0.3.2
;;;     get-environment:  func() -> list<tuple<string, string>>
;;;     get-arguments:    func() -> list<string>
;;;     get-initial-cwd:  func() -> option<string>
;;;   wasi:cli/stdin@0.3.2
;;;     read-via-stream:  func() -> tuple<stream<u8>,
;;;                                       future<result<_, error-code>>>
;;;   wasi:cli/stderr@0.3.2
;;;     write-via-stream: func(data: stream<u8>)
;;;                             -> future<result<_, error-code>>
;;;   wasi:cli/terminal-stdout@0.3.2
;;;     get-terminal-stdout: func() -> option<terminal-output>
;;;
;;; Canonical LOWER signatures (CanonicalABI flatten, MAX_FLAT_RESULTS
;;; = 1 — the trailing i32 is the caller's retptr; the host stores the
;;; result there and allocates through the (realloc ...) bump):
;;;
;;;   get-environment      core (param i32)     ; retptr -> (ptr,len)
;;;   get-arguments        core (param i32)
;;;   get-initial-cwd      core (param i32)     ; retptr -> option<string>
;;;                                              ; (disc u8 @0, ptr @4, len @8)
;;;   get-terminal-stdout  core (param i32)     ; retptr -> option<own<..>>
;;;                                              ; (disc u8 @0, handle @4)
;;;   read-via-stream      core (param i32)     ; retptr -> tuple
;;;                                              ; (stream handle @0,
;;;                                              ;  future handle @4)
;;;   write-via-stream     core (param i32) (result i32)   ; one flat result
;;;
;;; The guest (run lifted async with a callback) asserts, and TRAPS on
;;; any failure, that: argv has >= 2 entries, argv[0] equals the value
;;; of WMD_EXPECT_ARGV0 (the fixture path the CLI supplied), argv[1]
;;; is the literal "wmd-cli-marker", the environment carries
;;; WMD_ENV_PROBE=env-probe-value, get-initial-cwd is an option (none
;;; or a non-empty string), get-terminal-stdout is an option and — in
;;; the captured (non-tty) lane, signalled by WMD_EXPECT_TERMINAL=none
;;; — is `none'. It then reads the suite-supplied stdin through
;;; stdin's read-via-stream (stream.read in the canonical async copy
;;; order: an eager event is the packed CopyResult|progress<<4 return,
;;; a BLOCKED copy is joined into a waitable set and waited), requires
;;; exactly "stdin-probe\n" and the explicit DROPPED (EOF) result, and
;;; requires the paired future to read back result::ok. It then writes
;;; the fixed message "p3-cli-guest: stderr probe" through stderr's
;;; write-via-stream in the canonical future order (async future.read
;;; parks the read, the write is staged, the writable end is dropped,
;;; waitable-set.wait takes the future event). Success exits 0; every
;;; mismatch traps.
;;;
;;; The two string lists are read out of guest memory (list header
;;; ptr@0,len@4 for <string>; ptr@0,len@4 then elements of 8 bytes;
;;; list<tuple<string,string>> has 16-byte elements: name ptr/len then
;;; value ptr/len) — the exact layouts the canonical ABI's store rules
;;; give.

(defparameter *p3-cli-text*
  (format nil "~{~a~%~}"
          (list
           "(component"
           "  (type $ec (enum \"io\" \"illegal-byte-sequence\" \"pipe\"))"
           "  (type $ST (stream u8))"
           "  (type $FT (future (result (error $ec))))"
           "  (import \"wasi:cli/environment@0.3.2\""
           "    (instance $env"
           "      (export \"get-environment\" (func (result (list (tuple string string)))))"
           "      (export \"get-arguments\" (func (result (list string))))"
           "      (export \"get-initial-cwd\" (func (result (option string))))))"
           "  (import \"wasi:cli/stdin@0.3.2\""
           "    (instance $in"
           "      (export \"read-via-stream\""
           "        (func (result (tuple (stream u8)"
           "                       (future (result (error $ec)))))))))"
           "  (import \"wasi:cli/stderr@0.3.2\""
           "    (instance $stderr"
           "      (export \"write-via-stream\""
           "        (func (param \"data\" (stream u8))"
           "              (result (future (result (error $ec))))))))"
           "  (import \"wasi:cli/terminal-stdout@0.3.2\""
           "    (instance $ttyout"
           "      (export \"terminal-output\" (type $termout (sub resource)))"
           "      (export \"get-terminal-stdout\""
           "        (func (result (option (own $termout)))))))"
           "  (alias export $env \"get-environment\" (func $getenv))"
           "  (alias export $env \"get-arguments\" (func $getargs))"
           "  (alias export $env \"get-initial-cwd\" (func $getcwd))"
           "  (alias export $in \"read-via-stream\" (func $rvs))"
           "  (alias export $stderr \"write-via-stream\" (func $wvs))"
           "  (alias export $ttyout \"get-terminal-stdout\" (func $gettty))"
           "  (core module $Mem"
           "    (memory (export \"mem\") 1)"
           "    (global $bump (mut i32) (i32.const 4096))"
           "    (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
           "      (local $p i32)"
           "      (local.set $p (global.get $bump))"
           "      (global.set $bump"
           "        (i32.and (i32.add (global.get $bump)"
           "                          (i32.add (local.get 3) (i32.const 7)))"
           "                 (i32.const -8)))"
           "      (local.get $p)))"
           "  (core instance $mem (instantiate $Mem))"
           "  (core func $getenv-lowered"
           "    (canon lower (func $getenv)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $getargs-lowered"
           "    (canon lower (func $getargs)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $getcwd-lowered"
           "    (canon lower (func $getcwd)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $wvs-lowered"
           "    (canon lower (func $wvs)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $rvs-lowered"
           "    (canon lower (func $rvs)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $gettty-lowered"
           "    (canon lower (func $gettty)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (canon task.return (result (result)) (core func $tr))"
           "  (canon stream.new $ST (core func $snew))"
           "  (canon stream.write $ST async (memory (core memory $mem \"mem\")) (core func $swrite))"
           "  (canon stream.drop-writable $ST (core func $sdropw))"
           "  (canon stream.read $ST async (memory (core memory $mem \"mem\")) (core func $sread))"
           "  (canon stream.drop-readable $ST (core func $sdropr))"
           "  (canon future.drop-readable $FT (core func $fdropr))"
           "  (canon future.read $FT async (memory (core memory $mem \"mem\")) (core func $fread))"
           "  (canon waitable-set.new (core func $wsnew))"
           "  (canon waitable.join (core func $wjoin))"
           "  (canon waitable-set.wait (memory (core memory $mem \"mem\")) (core func $wswait))"
           "  (core module $M"
           "    (import \"env\" \"mem\" (memory 1))"
           "    (import \"env\" \"get-environment\" (func $getenv (param i32)))"
           "    (import \"env\" \"get-arguments\" (func $getargs (param i32)))"
           "    (import \"env\" \"get-initial-cwd\" (func $getcwd (param i32)))"
           "    (import \"env\" \"get-terminal-stdout\" (func $gettty (param i32)))"
           "    (import \"env\" \"write-via-stream\" (func $wvs (param i32) (result i32)))"
           "    (import \"env\" \"read-via-stream\" (func $rvs (param i32)))"
           "    (import \"env\" \"task.return\" (func $tr (param i32)))"
           "    (import \"env\" \"stream.new\" (func $snew (result i64)))"
           "    (import \"env\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))"
           "    (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))"
           "    (import \"env\" \"stream.read\" (func $sread (param i32 i32 i32) (result i32)))"
           "    (import \"env\" \"stream.drop-readable\" (func $sdropr (param i32)))"
           "    (import \"env\" \"future.drop-readable\" (func $fdropr (param i32)))"
           "    (import \"env\" \"future.read\" (func $fread (param i32 i32) (result i32)))"
           "    (import \"env\" \"waitable-set.new\" (func $wsnew (result i32)))"
           "    (import \"env\" \"waitable.join\" (func $wjoin (param i32 i32)))"
           "    (import \"env\" \"waitable-set.wait\" (func $wswait (param i32 i32) (result i32)))"
           "    (data (i32.const 64) \"wmd-cli-marker\")"
           "    (data (i32.const 80) \"WMD_ENV_PROBE\")"
           "    (data (i32.const 96) \"env-probe-value\")"
           "    (data (i32.const 112) \"WMD_EXPECT_ARGV0\")"
           "    (data (i32.const 128) \"WMD_EXPECT_TERMINAL\")"
           "    (data (i32.const 148) \"none\")"
           "    (data (i32.const 160) \"p3-cli-guest: stderr probe\\n\")"
           "    (data (i32.const 320) \"stdin-probe\\n\")"
           "    (global $fut (mut i32) (i32.const 0))"
           "    (global $readable (mut i32) (i32.const 0))"
           "    (global $writable (mut i32) (i32.const 0))"
           "    (global $set (mut i32) (i32.const 0))"
           "    (global $state (mut i32) (i32.const 0))"
           "    (global $total (mut i32) (i32.const 0))"
           "    (global $srcread (mut i32) (i32.const 0))"
           "    (global $futread (mut i32) (i32.const 0))"
           "    (type $cbty (func (param i32 i32 i32) (result i32)))"
           "    (func $streq (param $a i32) (param $an i32) (param $b i32) (param $bn i32) (result i32)"
           "      (local $i i32)"
           "      (if (i32.ne (local.get $an) (local.get $bn)) (then (return (i32.const 0))))"
           "      (block $done"
           "        (loop $l"
           "          (br_if $done (i32.ge_u (local.get $i) (local.get $an)))"
           "          (if (i32.ne (i32.load8_u (i32.add (local.get $a) (local.get $i)))"
           "                      (i32.load8_u (i32.add (local.get $b) (local.get $i))))"
           "              (then (return (i32.const 0))))"
           "          (local.set $i (i32.add (local.get $i) (i32.const 1)))"
           "          (br $l)))"
           "      (i32.const 1))"
           "    (func $env-find (param $name i32) (param $nlen i32) (result i32)"
           "      (local $base i32) (local $count i32) (local $i i32) (local $a i32)"
           "      (call $getenv (i32.const 8))"
           "      (local.set $base (i32.load (i32.const 8)))"
           "      (local.set $count (i32.load (i32.const 12)))"
           "      (block $done"
           "        (loop $l"
           "          (br_if $done (i32.ge_u (local.get $i) (local.get $count)))"
           "          (local.set $a (i32.add (local.get $base)"
           "                                 (i32.mul (local.get $i) (i32.const 16))))"
           "          (if (call $streq (i32.load (local.get $a))"
           "                           (i32.load (i32.add (local.get $a) (i32.const 4)))"
           "                           (local.get $name) (local.get $nlen))"
           "              (then"
           "                (i32.store (i32.const 48) (i32.load (i32.add (local.get $a) (i32.const 8))))"
           "                (i32.store (i32.const 52) (i32.load (i32.add (local.get $a) (i32.const 12))))"
           "                (return (i32.const 1))))"
           "          (local.set $i (i32.add (local.get $i) (i32.const 1)))"
           "          (br $l)))"
           "      (i32.const 0))"
           "    (func (export \"run\") (result i32)"
           "      (local $base i32) (local $count i32) (local $d i32)"
           "      (call $getargs (i32.const 0))"
           "      (local.set $base (i32.load (i32.const 0)))"
           "      (local.set $count (i32.load (i32.const 4)))"
           "      (if (i32.lt_u (local.get $count) (i32.const 2)) (then unreachable))"
           "      (if (i32.eqz (call $env-find (i32.const 112) (i32.const 16)))"
           "          (then unreachable))"
           "      (if (i32.eqz (call $streq (i32.load (local.get $base))"
           "                                (i32.load (i32.add (local.get $base) (i32.const 4)))"
           "                                (i32.load (i32.const 48)) (i32.load (i32.const 52))))"
           "          (then unreachable))"
           "      (if (i32.eqz (call $streq (i32.load (i32.add (local.get $base) (i32.const 8)))"
           "                                (i32.load (i32.add (local.get $base) (i32.const 12)))"
           "                                (i32.const 64) (i32.const 14)))"
           "          (then unreachable))"
           "      (if (i32.eqz (call $env-find (i32.const 80) (i32.const 13)))"
           "          (then unreachable))"
           "      (if (i32.eqz (call $streq (i32.load (i32.const 48)) (i32.load (i32.const 52))"
           "                                (i32.const 96) (i32.const 15)))"
           "          (then unreachable))"
           "      (call $getcwd (i32.const 16))"
           "      (local.set $d (i32.load8_u (i32.const 16)))"
           "      (if (i32.gt_u (local.get $d) (i32.const 1)) (then unreachable))"
           "      (if (i32.eq (local.get $d) (i32.const 1))"
           "          (then (if (i32.eqz (i32.load (i32.const 24))) (then unreachable))))"
           "      (call $gettty (i32.const 32))"
           "      (local.set $d (i32.load8_u (i32.const 32)))"
           "      (if (i32.gt_u (local.get $d) (i32.const 1)) (then unreachable))"
           "      (if (call $env-find (i32.const 128) (i32.const 19))"
           "          (then"
           "            (if (call $streq (i32.load (i32.const 48))"
           "                             (i32.load (i32.const 52))"
           "                             (i32.const 148) (i32.const 4))"
           "                (then"
           "                  (if (i32.ne (local.get $d) (i32.const 0))"
           "                      (then unreachable))))))"
           "      (i32.const 1))"
           "    (func (export \"run-cb\") (type $cbty)"
           "      (local $s i64) (local $ret i32) (local $n i32) (local $i i32)"
           "      (block $done"
           "        (block $st10"
           "          (block $st20"
           "            (br_table $done $st10 $st20"
           "                      (i32.div_u (global.get $state) (i32.const 10))))"
           "          (if (i32.ne (i32.const 0) (i32.load (i32.const 192)))"
           "              (then unreachable))"
           "          (call $tr (i32.const 0))"
           "          (return (i32.const 0)))"
           "        (call $sdropw (global.get $writable))"
           "        (global.set $state (i32.const 20))"
           "        (local.set $ret (call $wswait (global.get $set) (i32.const 224)))"
           "        (if (i32.ne (local.get $ret) (i32.const 4)) (then unreachable))"
           "        (if (i32.ne (i32.const 0) (i32.load (i32.const 192)))"
           "            (then unreachable))"
           "        (call $tr (i32.const 0))"
           "        (return (i32.const 0)))"
           "      (global.set $set (call $wsnew))"
           ;; ---- the STDIN probe: read-via-stream, then stream.read
           ;; until the explicit DROPPED (EOF) result, and the paired
           ;; future's ok result. A BLOCKED copy is joined into the
           ;; set and waited (the abi's async copy order).
           "      (call $rvs (i32.const 336))"
           "      (global.set $srcread (i32.load (i32.const 336)))"
           "      (global.set $futread (i32.load (i32.const 340)))"
           "      (global.set $total (i32.const 0))"
           "      (block $eof"
           "        (loop $more"
           "          (local.set $ret (call $sread (global.get $srcread)"
           "                                      (i32.const 256) (i32.const 64)))"
           "          (if (i32.eq (local.get $ret) (i32.const -1))"
           "              (then"
           "                (call $wjoin (global.get $srcread) (global.get $set))"
           "                (local.set $ret (call $wswait (global.get $set)"
           "                                              (i32.const 224)))"
           "                (if (i32.ne (local.get $ret) (i32.const 2))"
           "                    (then unreachable))"
           "                (call $wjoin (global.get $srcread) (i32.const 0))"
           "                (local.set $ret (i32.load (i32.const 228)))))"
           ;; CopyResult: COMPLETED=0 (bytes), DROPPED=1 (EOF, progress 0)
           "          (if (i32.eq (i32.and (local.get $ret) (i32.const 15))"
           "                      (i32.const 1))"
           "              (then (br $eof)))"
           "          (if (i32.ne (i32.and (local.get $ret) (i32.const 15))"
           "                      (i32.const 0))"
           "              (then unreachable))"
           "          (local.set $n (i32.shr_u (local.get $ret) (i32.const 4)))"
           "          (if (i32.or (i32.eqz (local.get $n))"
           "                      (i32.gt_u (i32.add (global.get $total)"
           "                                        (local.get $n))"
           "                                (i32.const 12)))"
           "              (then unreachable))"
           "          (local.set $i (i32.const 0))"
           "          (block $bytes"
           "            (loop $byte"
           "              (br_if $bytes (i32.ge_u (local.get $i) (local.get $n)))"
           "              (if (i32.ne"
           "                    (i32.load8_u (i32.add (i32.const 256)"
           "                                          (local.get $i)))"
           "                    (i32.load8_u (i32.add (i32.const 320)"
           "                                         (i32.add (global.get $total)"
           "                                                  (local.get $i)))))"
           "                  (then unreachable))"
           "              (local.set $i (i32.add (local.get $i) (i32.const 1)))"
           "              (br $byte)))"
           "          (global.set $total (i32.add (global.get $total)"
           "                                      (local.get $n)))"
           "          (br $more)))"
           "      (if (i32.ne (global.get $total) (i32.const 12))"
           "          (then unreachable))"
           "      (local.set $ret (call $fread (global.get $futread)"
           "                                    (i32.const 240)))"
           "      (if (i32.eq (local.get $ret) (i32.const -1))"
           "          (then"
           "            (call $wjoin (global.get $futread) (global.get $set))"
           "            (local.set $ret (call $wswait (global.get $set)"
           "                                          (i32.const 224)))"
           "            (if (i32.ne (local.get $ret) (i32.const 4))"
           "                (then unreachable))"
           "            (call $wjoin (global.get $futread) (i32.const 0))))"
           "      (if (i32.ne (i32.load8_u (i32.const 240)) (i32.const 0))"
           "          (then unreachable))"
           "      (call $sdropr (global.get $srcread))"
           "      (call $fdropr (global.get $futread))"
           "      (local.set $s (call $snew))"
           "      (global.set $readable (i32.wrap_i64 (local.get $s)))"
           "      (global.set $writable (i32.wrap_i64 (i64.shr_u (local.get $s) (i64.const 32))))"
           "      (global.set $fut (call $wvs (global.get $readable)))"
           "      (call $wjoin (global.get $fut) (global.get $set))"
           "      (local.set $ret (call $fread (global.get $fut) (i32.const 192)))"
           "      (if (i32.ne (local.get $ret) (i32.const -1)) (then unreachable))"
           "      (local.set $ret (call $swrite (global.get $writable) (i32.const 160) (i32.const 27)))"
           "      (if (i32.and (local.get $ret) (i32.const 15))"
           "          (then"
           "            (call $wjoin (global.get $writable) (global.get $set))"
           "            (global.set $state (i32.const 10))"
           "            (local.set $ret (call $wswait (global.get $set) (i32.const 224)))"
           "            (if (i32.ne (local.get $ret) (i32.const 3)) (then unreachable))"
           "            (call $sdropw (global.get $writable))"
           "            (global.set $state (i32.const 20))"
           "            (local.set $ret (call $wswait (global.get $set) (i32.const 224)))"
           "            (if (i32.ne (local.get $ret) (i32.const 4)) (then unreachable))"
           "            (if (i32.ne (i32.const 0) (i32.load (i32.const 192)))"
           "                (then unreachable))"
           "            (call $tr (i32.const 0))"
           "            (return (i32.const 0))))"
           "      (call $sdropw (global.get $writable))"
           "      (global.set $state (i32.const 20))"
           "      (local.set $ret (call $wswait (global.get $set) (i32.const 224)))"
           "      (if (i32.ne (local.get $ret) (i32.const 4)) (then unreachable))"
           "      (if (i32.ne (i32.const 0) (i32.load (i32.const 192)))"
           "          (then unreachable))"
           "      (call $tr (i32.const 0))"
           "      (i32.const 0)))"
           "  (core instance $m"
           "    (instantiate $M"
           "      (with \"env\""
           "        (instance"
           "          (export \"mem\" (memory $mem \"mem\"))"
           "          (export \"get-environment\" (func $getenv-lowered))"
           "          (export \"get-arguments\" (func $getargs-lowered))"
           "          (export \"get-initial-cwd\" (func $getcwd-lowered))"
           "          (export \"get-terminal-stdout\" (func $gettty-lowered))"
           "          (export \"write-via-stream\" (func $wvs-lowered))"
           "          (export \"read-via-stream\" (func $rvs-lowered))"
           "          (export \"task.return\" (func $tr))"
           "          (export \"stream.new\" (func $snew))"
           "          (export \"stream.write\" (func $swrite))"
           "          (export \"stream.drop-writable\" (func $sdropw))"
           "          (export \"stream.read\" (func $sread))"
           "          (export \"stream.drop-readable\" (func $sdropr))"
           "          (export \"future.drop-readable\" (func $fdropr))"
           "          (export \"future.read\" (func $fread))"
           "          (export \"waitable-set.new\" (func $wsnew))"
           "          (export \"waitable.join\" (func $wjoin))"
           "          (export \"waitable-set.wait\" (func $wswait))))))"
           "  (func $run (export \"run\") async (result (result))"
           "    (canon lift (core func $m \"run\") async (callback (core func $m \"run-cb\"))))"
           "  (instance $runiface (export \"run\" (func $run)))"
           "  (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
           "    (instance (export \"run\" (func async (result (result)))))))"
               )))

(defun p3-cli-guest-bytes ()
  (component-encode:encode-component
   (wasmdown.component.text::parse-component-text *p3-cli-text*)))

;;; ---------------------------------------------------------------
;;; Fixture 10: the FILESYSTEM consumer surface — wasi:filesystem/
;;; {types,preopens}@0.3.2. This is the guest the CLI lane's PLAN
;;; §9.3 row lacked: the P2 lane only ever exercised the filesystem
;;; through an in-process provider/unit test, never through a real
;;; command component.
;;;
;;; Import (exact signatures, specs/wasi/wasi-wit-0.3.2/filesystem/
;;; filesystem.wit):
;;;
;;;   wasi:filesystem/preopens@0.3.2
;;;     get-directories: func() -> list<tuple<descriptor, string>>
;;;                      ; descriptor is own<descriptor>, FIRST
;;;   wasi:filesystem/types@0.3.2
;;;     [method]descriptor.open-at:
;;;       async func(self: borrow<descriptor>, path-flags: path-flags,
;;;                  path: string, open-flags: open-flags,
;;;                  flags: descriptor-flags)
;;;         -> result<descriptor, error-code>
;;;     [method]descriptor.read-via-stream:
;;;       func(self: borrow<descriptor>, offset: filesize)
;;;         -> tuple<stream<u8>, future<result<_, error-code>>>
;;;     [method]descriptor.write-via-stream:
;;;       func(self: borrow<descriptor>, data: stream<u8>,
;;;            offset: filesize)
;;;         -> future<result<_, error-code>>
;;;     [method]descriptor.append-via-stream:
;;;       func(self: borrow<descriptor>, data: stream<u8>)
;;;         -> future<result<_, error-code>>
;;;     [method]descriptor.stat:
;;;       async func(self: borrow<descriptor>)
;;;         -> result<descriptor-stat, error-code>
;;;     [method]descriptor.create-directory-at:
;;;       async func(self: borrow<descriptor>, path: string)
;;;         -> result<_, error-code>
;;;     [method]descriptor.stat-at:
;;;       async func(self: borrow<descriptor>, path-flags: path-flags,
;;;                  path: string)
;;;         -> result<descriptor-stat, error-code>
;;;     [method]descriptor.read-directory:
;;;       func(self: borrow<descriptor>)
;;;         -> tuple<stream<directory-entry>,
;;;                  future<result<_, error-code>>>
;;;     [method]descriptor.rename-at:
;;;       async func(self: borrow<descriptor>, old-path: string,
;;;                  new-descriptor: borrow<descriptor>, new-path: string)
;;;         -> result<_, error-code>
;;;     [method]descriptor.unlink-file-at:
;;;       async func(self: borrow<descriptor>, path: string)
;;;         -> result<_, error-code>
;;;     [method]descriptor.remove-directory-at:
;;;       async func(self: borrow<descriptor>, path: string)
;;;         -> result<_, error-code>
;;;
;;; Canonical LOWER shapes (CanonicalABI flatten §2894-2930;
;;; MAX_FLAT_ASYNC_PARAMS = 4, ANY async result rides the appended
;;; [ptr] out-param):
;;;
;;;   get-directories  (sync)  core (param i32)     ; retptr -> (ptr,len)
;;;   open-at          (async) core (param i32 i32) (result i32)
;;;       ; the 5 flat params (path-flags i32, string ptr+len,
;;;       ; open-flags i32, descriptor-flags i32) exceed 4, so the
;;;       ; FIRST param is the ADDRESS of the params tuple; the
;;;       ; SECOND is the caller's result out-param; the i32 result
;;;       ; is the packed subtask state (state | index<<4).
;;;       ; memory + realloc are required: the result's error-code
;;;       ; variant carries a `string' case (other(option<string>)).
;;;   read-via-stream  (sync)  core (param i32 i64 i32)
;;;       ; (self handle, offset, retptr) -> (stream handle @0,
;;;       ; future handle @4)
;;;   write-via-stream (sync)  core (param i32 i32 i64) (result i32)
;;;       ; (self, readable data end, offset) -> the RESULT FUTURE's
;;;       ; handle; 1 flat result, so no retptr and no memory needed
;;;   append-via-stream (sync) core (param i32 i32) (result i32)
;;;       ; (self, readable data end) -> the result future's handle
;;;   stat             (async) core (param i32 i32) (result i32)
;;;       ; (self, result out-param); the i32 is the packed subtask
;;;       ; state. The async rule always passes ANY result through the
;;;       ; appended [ptr], hence memory (the store writes the whole
;;;       ; result<descriptor-stat, error-code> there).
;;;   create-directory-at / unlink-file-at / remove-directory-at
;;;                    (async) core (param i32 i32 i32 i32) (result i32)
;;;       ; (self, path ptr, path len, result out-param): 3 flat params,
;;;       ; so they stay scalar and only the result goes through [ptr].
;;;   stat-at          (async) core (param i32 i32 i32 i32 i32) (result i32)
;;;       ; (self, path-flags, path ptr, path len, result out-param).
;;;   rename-at        (async) core (param i32 i32) (result i32)
;;;       ; 6 flat params exceed 4, so the FIRST param is the params
;;;       ; tuple address (self@0, old ptr@4/len@8, new-descriptor@12,
;;;       ; new ptr@16/len@20, size 24) and the SECOND the out-param.
;;;   read-directory   (sync)  core (param i32 i32)
;;;       ; (self handle, retptr) -> (stream handle @0, future @4); the
;;;       ; tuple result's 2 flat handles exceed MAX_FLAT_RESULTS, so it
;;;       ; rides the appended [ptr]. Each element is a 24-byte
;;;       ; directory-entry (descriptor-type variant 16 bytes, then the
;;;       ; name's ptr@16/len@20); storing its string needs realloc.
;;;
;;; The params-tuple layout (flags have an ELEMENT SIZE OF 1/2/4 per
;;; alignment_flags: 1 label and 4 labels are 1-byte fields, so the
;;; tuple is pf@0 u8, string ptr@4/len@8, open-flags@12 u8,
;;; descriptor-flags@13 u8, size 16):
;;;
;;;   0 path-flags u8 | 4 path ptr | 8 path len | 12 open-flags u8
;;;   | 13 descriptor-flags u8
;;;
;;; The result out-param (result<own<descriptor>, error-code>) is
;;; disc@0 u8 and the payload at align 4: handle@4, or the
;;; error-code variant's disc@4 (no-entry = case 19).
;;;
;;; The memory layouts the guest reads are the canonical store rules:
;;; a list<tuple<own<descriptor>, string>> header is (ptr@0, len@4)
;;; with 12-byte elements (handle@0, name ptr@4, name len@8); a
;;; tuple<stream<u8>, future<...>> result is two i32 handles.
;;;
;;; stat's out-param (result<descriptor-stat, error-code>): disc@0
;;; (ok = 0), payload at align 8 — the variant's join alignment is the
;;; max over its cases, and descriptor-stat's u64 fields out-align the
;;; error-code's 4. descriptor-stat there is 104 bytes:
;;; descriptor-type@0 (16: disc u8, other(option<string>) at +4),
;;; link-count u64@16, size u64@24, then three option<instant>@32/56/80
;;; (24 each: disc u8, instant {seconds u64, nanoseconds u32} at +8).
;;; The guest checks the type case (regular-file = 5), link-count and
;;; size; the timestamps are not asserted.
;;;
;;; The guest (run lifted async with a callback) asserts, and TRAPS
;;; on any failure:
;;;   1. get-directories has an entry named "/work"; take its handle;
;;;   2. open-at("hello.txt", read) returns ok and a live descriptor;
;;;   3. read-via-stream(0) + stream.read yields exactly 17 bytes
;;;      equal to the seeded "filesystem-probe\n" and the paired
;;;      future resolves ok; a WRITE through that read-only descriptor
;;;      (write-via-stream) must REFUSE with error-code::read-only
;;;      (case 32) through the paired future — an err result, never a
;;;      trap;
;;;   4. THE WRITE PATH on the same preopen: open-at("out.txt",
;;;      create|truncate, read|write) returns ok; write-via-stream at
;;;      offset 0 moves 15 bytes through stream.new + stream.write (a
;;;      BLOCKED copy is waited in the set and its event records the
;;;      progress); a second write-via-stream puts "!" at offset 17,
;;;      so bytes 15..16 must be the WIT's ZERO FILL; append-via-stream
;;;      then adds "-app" at EOF. stat must report regular-file,
;;;      link-count 1, size 22; read-via-stream(0) on the SAME
;;;      descriptor then compares all 22 bytes against the expected
;;;      span, and the paired future must resolve ok. After every
;;;      write the guest has parked a future.read, so the provider's
;;;      resolve has already written result::ok where the future event
;;;      wakes it — an err result or a partial copy traps;
;;;   5. open-at("missing.txt") returns ERR with error-code::no-entry
;;;      (case 19) — an err result, never a trap;
;;;   6. THE DIRECTORY SURFACE: create-directory-at("subdir") ok;
;;;      open-at("subdir", directory, read) ok; stat($dir) reports
;;;      descriptor-type::directory (case 2); read-directory on the
;;;      preopen enumerates every entry and its names include
;;;      hello.txt, out.txt, subdir and temp.txt, with the paired
;;;      future ok; rename-at("temp.txt" -> "moved.txt") ok and
;;;      stat-at("moved.txt") reports regular-file, size 0;
;;;      unlink-file-at("moved.txt") and
;;;      remove-directory-at("subdir") both ok, then
;;;      stat-at("subdir") is ERR/no-entry (case 19). The temporary
;;;      objects are removed so repeated runs share the workdir;
;;;   7. every handle is dropped (all descriptors, all stream and
;;;      future ends, the preopen's own handle).
;;;
;;; The async open-at is waited the CanonicalABI way: a packed
;;; STARTING/STARTED state is joined into a waitable set and
;;; waitable-set.wait takes the SUBTASK(1)/RETURNED(2) event of THAT
;;; subtask; the eager RETURNED(2) pack needs no wait. The stream and
;;; future reads use the async copy built-ins: an eager event is the
;;; return value, a BLOCKED (-1) copy is waited on in the set and the
;;; event payload (CopyResult + progress<<4) is the result.

(defparameter *p3-fs-error-code-cases*
  '("access" "already" "bad-descriptor" "busy" "deadlock" "quota"
    "exist" "file-too-large" "illegal-byte-sequence" "in-progress"
    "interrupted" "invalid" "io" "is-directory" "loop" "too-many-links"
    "message-size" "name-too-long" "no-device" "no-entry" "no-lock"
    "insufficient-memory" "insufficient-space" "not-directory"
    "not-empty" "not-recoverable" "unsupported" "no-tty"
    "no-such-device" "overflow" "not-permitted" "pipe" "read-only"
    "invalid-seek" "text-file-busy" "cross-device"))

(defparameter *p3-fs-text*
  (format nil "~{~a~%~}"
          (list
           "(component"
           ;; the ALIAS DECLARATORS INSIDE AN INSTANCE TYPE come FIRST
           ;; and target indices at or beyond the instance's own
           ;; partial type vector: the validator's declarator walk
           ;; pushes a :outer-aliased marker for a non-resource
           ;; target but a RESOURCE marker when the target index
           ;; happens to name a resource slot of the partial space.
           ;; Ordered this way every numeric target (0,1,2,4) is past
           ;; the partial vector, so the walk only ever emits
           ;; markers and the func result validates. (Top-level type
           ;; order: $pf=0, $of=1, $df=2, option<string>=3, $ec=4.)
           "  (type $pf (flags \"symlink-follow\"))"
           "  (type $of (flags \"create\" \"directory\" \"exclusive\" \"truncate\"))"
           "  (type $df (flags \"read\" \"write\" \"file-integrity-sync\""
           "                   \"data-integrity-sync\" \"requested-write-sync\""
           "                   \"mutate-directory\"))"
           (format nil
                   "  (type $ec (variant ~{(case ~s)~^ ~} (case \"other\" (option string))))"
                   *p3-fs-error-code-cases*)
           "  (type $ST (stream u8))"
           "  (type $FT (future (result (error $ec))))"
           "  (type $res (result))"
           ;; the clocks record descriptor-stat uses via the WIT's
           ;; `use wasi:clocks/system-clock@0.3.2.{instant}': a TOP-level
           ;; type so the instance type can reference it (the parser
           ;; emits the implicit outer alias a `use` produces).
           "  (type $instant (record (field \"seconds\" u64)"
           "                        (field \"nanoseconds\" u32)))"
           ;; the directory iteration's element type at the TOP level,
           ;; so the stream.read canon can name it (the instance space
           ;; has the WIT-shaped copy).
           "  (type $dtv (variant (case \"block-device\")"
           "                      (case \"character-device\")"
           "                      (case \"directory\") (case \"fifo\")"
           "                      (case \"symbolic-link\")"
           "                      (case \"regular-file\") (case \"socket\")"
           "                      (case \"other\" (option string))))"
           "  (type $ent (record (field \"type\" $dtv)"
           "                     (field \"name\" string)))"
           "  (type $DENT (stream $ent))"
           ;; The imported instance type follows the WIT shape: every
           ;; defined type is declared LOCALLY and then EXPORTED under
           ;; its WIT name (`(export "name" (type $slot (eq $local)))`);
           ;; later types and every method signature reference the
           ;; EXPORT SLOT, not the local definition (external-visibility
           ;; ::0/::2: only the export's NEW index is externally
           ;; visible, and a func export may not reference a local
           ;; record/variant). The provider exports the same WIT names.
           ;; descriptor-stat's timestamps reference the top-level
           ;; clocks `instant`; the text parser emits the implicit outer
           ;; alias a WIT `use` produces.
           "  (import \"wasi:filesystem/types@0.3.2\""
           "    (instance $types"
           "      (type $pf2 (flags \"symlink-follow\"))"
           "      (export \"path-flags\" (type $pf3 (eq $pf2)))"
           "      (type $of2 (flags \"create\" \"directory\" \"exclusive\""
           "                       \"truncate\"))"
           "      (export \"open-flags\" (type $of3 (eq $of2)))"
           "      (type $df2 (flags \"read\" \"write\" \"file-integrity-sync\""
           "                        \"data-integrity-sync\""
           "                        \"requested-write-sync\""
           "                        \"mutate-directory\"))"
           "      (export \"descriptor-flags\" (type $df3 (eq $df2)))"
           (format nil
                   "      (type $ec2 (variant ~{(case ~s)~^ ~} (case \"other\" (option string))))"
                   *p3-fs-error-code-cases*)
           "      (export \"error-code\" (type $ec3 (eq $ec2)))"
           ;; the STAT surface (the guest asserts the write's placement by
           ;; the descriptor's own metadata). The WIT shape is honoured:
           ;; each defined type is EXPORTED (a type-bound export's NEW
           ;; index is the externally visible name — external-visibility
           ;; ::0/::2), and later types reference that NAME, never the
           ;; local definition. descriptor-stat's timestamps use the
           ;; outer-aliased clocks `instant` (the WIT's `use`).
           "      (type $dtp (variant (case \"block-device\")"
           "                          (case \"character-device\")"
           "                          (case \"directory\") (case \"fifo\")"
           "                          (case \"symbolic-link\")"
           "                          (case \"regular-file\")"
           "                          (case \"socket\")"
           "                          (case \"other\" (option string))))"
           "      (export \"descriptor-type\" (type $dtp2 (eq $dtp)))"
           "      (type $dst (record (field \"type\" $dtp2)"
           "                         (field \"link-count\" u64)"
           "                         (field \"size\" u64)"
           "                         (field \"data-access-timestamp\""
           "                                (option $instant))"
           "                         (field \"data-modification-timestamp\""
           "                                (option $instant))"
           "                         (field \"status-change-timestamp\""
           "                                (option $instant))))"
           "      (export \"descriptor-stat\" (type $dst2 (eq $dst)))"
           "      (type $dent (record (field \"type\" $dtp2)"
           "                          (field \"name\" string)))"
           "      (export \"directory-entry\" (type $dentx (eq $dent)))"
           "      (export \"descriptor\" (type $dt (sub resource)))"
           "      (export \"[method]descriptor.open-at\""
           "        (func async (param \"self\" (borrow $dt))"
           "                    (param \"path-flags\" $pf3)"
           "                    (param \"path\" string)"
           "                    (param \"open-flags\" $of3)"
           "                    (param \"flags\" $df3)"
           "                    (result (result (own $dt) (error $ec3)))))"
           "      (export \"[method]descriptor.read-via-stream\""
           "        (func (param \"self\" (borrow $dt))"
           "              (param \"offset\" u64)"
           "              (result (tuple (stream u8)"
           "                             (future (result (error $ec3)))))))"
           "      (export \"[method]descriptor.write-via-stream\""
           "        (func (param \"self\" (borrow $dt))"
           "              (param \"data\" (stream u8))"
           "              (param \"offset\" u64)"
           "              (result (future (result (error $ec3))))))"
           "      (export \"[method]descriptor.append-via-stream\""
           "        (func (param \"self\" (borrow $dt))"
           "              (param \"data\" (stream u8))"
           "              (result (future (result (error $ec3))))))"
           "      (export \"[method]descriptor.stat\""
           "        (func async (param \"self\" (borrow $dt))"
           "                    (result (result $dst2 (error $ec3)))))"
           "      (export \"[method]descriptor.create-directory-at\""
           "        (func async (param \"self\" (borrow $dt))"
           "                    (param \"path\" string)"
           "                    (result (result (error $ec3)))))"
           "      (export \"[method]descriptor.stat-at\""
           "        (func async (param \"self\" (borrow $dt))"
           "                    (param \"path-flags\" $pf3)"
           "                    (param \"path\" string)"
           "                    (result (result $dst2 (error $ec3)))))"
           "      (export \"[method]descriptor.read-directory\""
           "        (func (param \"self\" (borrow $dt))"
           "                    (result (tuple (stream $dentx)"
           "                                   (future (result (error $ec3)))))))"
           "      (export \"[method]descriptor.rename-at\""
           "        (func async (param \"self\" (borrow $dt))"
           "                    (param \"old-path\" string)"
           "                    (param \"new-descriptor\" (borrow $dt))"
           "                    (param \"new-path\" string)"
           "                    (result (result (error $ec3)))))"
           "      (export \"[method]descriptor.unlink-file-at\""
           "        (func async (param \"self\" (borrow $dt))"
           "                    (param \"path\" string)"
           "                    (result (result (error $ec3)))))"
           "      (export \"[method]descriptor.remove-directory-at\""
           "        (func async (param \"self\" (borrow $dt))"
           "                    (param \"path\" string)"
           "                    (result (result (error $ec3)))))))"
           "  (alias export $types \"descriptor\" (type $desc))"
           "  (import \"wasi:filesystem/preopens@0.3.2\""
           "    (instance $pre"
           "      (export \"get-directories\""
           "        (func (result (list (tuple (own $desc) string)))))))"
           "  (alias export $types \"[method]descriptor.open-at\" (func $openat-f))"
           "  (alias export $types \"[method]descriptor.read-via-stream\" (func $rvs-f))"
           "  (alias export $types \"[method]descriptor.write-via-stream\" (func $wvs-f))"
           "  (alias export $types \"[method]descriptor.append-via-stream\" (func $avs-f))"
           "  (alias export $types \"[method]descriptor.stat\" (func $stat-f))"
           "  (alias export $types \"[method]descriptor.create-directory-at\" (func $mkdir-f))"
           "  (alias export $types \"[method]descriptor.stat-at\" (func $statat-f))"
           "  (alias export $types \"[method]descriptor.read-directory\" (func $readdir-f))"
           "  (alias export $types \"[method]descriptor.rename-at\" (func $rename-f))"
           "  (alias export $types \"[method]descriptor.unlink-file-at\" (func $unlink-f))"
           "  (alias export $types \"[method]descriptor.remove-directory-at\" (func $rmdir-f))"
           "  (alias export $pre \"get-directories\" (func $getdirs-f))"
           "  (core module $Mem"
           "    (memory (export \"mem\") 1)"
           "    (global $bump (mut i32) (i32.const 4096))"
           "    (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
           "      (local $p i32)"
           "      (local.set $p (global.get $bump))"
           "      (global.set $bump"
           "        (i32.and (i32.add (global.get $bump)"
           "                          (i32.add (local.get 3) (i32.const 7)))"
           "                 (i32.const -8)))"
           "      (local.get $p)))"
           "  (core instance $mem (instantiate $Mem))"
           "  (core func $getdirs-lowered"
           "    (canon lower (func $getdirs-f)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $openat-lowered"
           "    (canon lower (func $openat-f) async"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $rvs-lowered"
           "    (canon lower (func $rvs-f)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $wvs-lowered"
           "    (canon lower (func $wvs-f)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $avs-lowered"
           "    (canon lower (func $avs-f)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $stat-lowered"
           "    (canon lower (func $stat-f) async"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $mkdir-lowered"
           "    (canon lower (func $mkdir-f) async"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $statat-lowered"
           "    (canon lower (func $statat-f) async"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $readdir-lowered"
           "    (canon lower (func $readdir-f)"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $rename-lowered"
           "    (canon lower (func $rename-f) async"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $unlink-lowered"
           "    (canon lower (func $unlink-f) async"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (core func $rmdir-lowered"
           "    (canon lower (func $rmdir-f) async"
           "      (memory (core memory $mem \"mem\"))"
           "      (realloc (core func $mem \"realloc\"))))"
           "  (canon task.return (result $res) (core func $tr))"
           "  (canon waitable-set.new (core func $wsnew))"
           "  (canon waitable.join (core func $wjoin))"
           "  (canon waitable-set.wait (memory (core memory $mem \"mem\")) (core func $wswait))"
           "  (canon subtask.drop (core func $subdrop))"
           "  (canon stream.new $ST (core func $snew))"
           "  (canon stream.read $ST async (memory (core memory $mem \"mem\")) (core func $sread))"
           "  (canon stream.read $DENT async"
           "         (memory (core memory $mem \"mem\"))"
           "         (realloc (core func $mem \"realloc\"))"
           "         (core func $sreadd))"
           "  (canon stream.write $ST async (memory (core memory $mem \"mem\")) (core func $swrite))"
           "  (canon future.read $FT async"
           "         (memory (core memory $mem \"mem\"))"
           "         (realloc (core func $mem \"realloc\"))"
           "         (core func $fread))"
           "  (canon stream.drop-readable $ST (core func $sdropr))"
           "  (canon stream.drop-writable $ST (core func $sdropw))"
           "  (canon future.drop-readable $FT (core func $fdropr))"
           "  (canon resource.drop $desc (core func $fdrop))"
           "  (core module $M"
           "    (import \"env\" \"mem\" (memory 1))"
           "    (import \"env\" \"get-directories\" (func $getdirs (param i32)))"
           "    (import \"env\" \"open-at\" (func $openat (param i32 i32) (result i32)))"
           "    (import \"env\" \"read-via-stream\" (func $rvs (param i32 i64 i32)))"
           "    (import \"env\" \"write-via-stream\" (func $wvs (param i32 i32 i64) (result i32)))"
           "    (import \"env\" \"append-via-stream\" (func $avs (param i32 i32) (result i32)))"
           "    (import \"env\" \"stat\" (func $stat (param i32 i32) (result i32)))"
           "    (import \"env\" \"create-directory-at\" (func $mkdir (param i32 i32 i32 i32) (result i32)))"
           "    (import \"env\" \"stat-at\" (func $statat (param i32 i32 i32 i32 i32) (result i32)))"
           "    (import \"env\" \"read-directory\" (func $readdir (param i32 i32)))"
           "    (import \"env\" \"rename-at\" (func $rename (param i32 i32) (result i32)))"
           "    (import \"env\" \"unlink-file-at\" (func $unlink (param i32 i32 i32 i32) (result i32)))"
           "    (import \"env\" \"remove-directory-at\" (func $rmdir (param i32 i32 i32 i32) (result i32)))"
           "    (import \"env\" \"stream.new\" (func $snew (result i64)))"
           "    (import \"env\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))"
           "    (import \"env\" \"stream.drop-writable\" (func $sdropw (param i32)))"
           "    (import \"env\" \"task.return\" (func $tr (param i32)))"
           "    (import \"env\" \"waitable-set.new\" (func $wsnew (result i32)))"
           "    (import \"env\" \"waitable.join\" (func $wjoin (param i32 i32)))"
           "    (import \"env\" \"waitable-set.wait\" (func $wswait (param i32 i32) (result i32)))"
           "    (import \"env\" \"subtask.drop\" (func $subdrop (param i32)))"
           "    (import \"env\" \"stream.read\" (func $sread (param i32 i32 i32) (result i32)))"
           "    (import \"env\" \"stream.read-dir\" (func $sreadd (param i32 i32 i32) (result i32)))"
           "    (import \"env\" \"future.read\" (func $fread (param i32 i32) (result i32)))"
           "    (import \"env\" \"stream.drop-readable\" (func $sdropr (param i32)))"
           "    (import \"env\" \"future.drop-readable\" (func $fdropr (param i32)))"
           "    (import \"env\" \"resource.drop\" (func $fdrop (param i32)))"
           "    (data (i32.const 64) \"hello.txt\")"
           "    (data (i32.const 80) \"missing.txt\")"
           "    (data (i32.const 96) \"/work\")"
           "    (data (i32.const 112) \"filesystem-probe\\n\")"
           ;; the write path's probe: the path, the three spans handed to
           ;; the write streams, and the exact 22 bytes a correct
           ;; out.txt holds afterwards (write at 0, "!" at offset 17 with
           ;; the 15..16 hole the WIT's extension rule requires, append
           ;; at EOF). Kept above 512: the hello.txt assertion reads the
           ;; newline at 128, and 192/224/256/336/400 are result, event,
           ;; buffer and stat areas.
           "    (data (i32.const 512) \"out.txt\")"
           "    (data (i32.const 520) \"wmd-write-probe\")"
           "    (data (i32.const 536) \"!\")"
           "    (data (i32.const 544) \"-app\")"
           "    (data (i32.const 552) \"wmd-write-probe\\00\\00!-app\")"
           ;; the directory probe's paths; the read-directory element
           ;; buffer at 1024 (24 bytes per directory-entry: type variant
           ;; 16 + string ptr/len 8); the realloc'd names live above 4096
           "    (data (i32.const 600) \"subdir\")"
           "    (data (i32.const 608) \"temp.txt\")"
           "    (data (i32.const 624) \"moved.txt\")"
           "    (global $ws (mut i32) (i32.const 0))"
           "    (func $streq (param $a i32) (param $b i32) (param $n i32) (result i32)"
           "      (local $i i32)"
           "      (block $done"
           "        (loop $l"
           "          (br_if $done (i32.ge_u (local.get $i) (local.get $n)))"
           "          (if (i32.ne (i32.load8_u (i32.add (local.get $a) (local.get $i)))"
           "                      (i32.load8_u (i32.add (local.get $b) (local.get $i))))"
           "              (then (return (i32.const 0))))"
           "          (local.set $i (i32.add (local.get $i) (i32.const 1)))"
           "          (br $l)))"
           "      (i32.const 1))"
           ;; a directory-entry's NAME equals the given span: the record
           ;; stores the type variant at +0 and the string at +16 (ptr)
           ;; and +20 (len)
           "    (func $nameeq (param $e i32) (param $p i32) (param $n i32) (result i32)"
           "      (if (i32.ne (i32.load (i32.add (local.get $e) (i32.const 20)))"
           "                  (local.get $n))"
           "          (then (return (i32.const 0))))"
           "      (call $streq (i32.load (i32.add (local.get $e) (i32.const 16)))"
           "                   (local.get $p) (local.get $n)))"
           ;; the async open-at: read the out-param only after the
           ;; subtask has RETURNED; the event tuple lands at 336
           "    (func $settle (param $packed i32)"
           "      (local $st i32) (local $sub i32) (local $ret i32)"
           "      (local.set $st (i32.and (local.get $packed) (i32.const 15)))"
           "      (local.set $sub (i32.shr_u (local.get $packed) (i32.const 4)))"
           ;; the EAGER pack (§3714-3717) is the bare state 2: the
           ;; runtime already delivered and dropped the subtask, so
           ;; there is no index to drop here
           "      (if (i32.eq (local.get $st) (i32.const 2)) (then (return)))"
           "      (if (i32.gt_u (local.get $st) (i32.const 1)) (then unreachable))"
           "      (call $wjoin (local.get $sub) (global.get $ws))"
           "      (local.set $ret (call $wswait (global.get $ws) (i32.const 336)))"
           "      (if (i32.ne (local.get $ret) (i32.const 1)) (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 340)) (local.get $sub)) (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 344)) (i32.const 2)) (then unreachable))"
           "      (call $wjoin (local.get $sub) (i32.const 0))"
           "      (call $subdrop (local.get $sub)))"
           ;; THE WRITE SPAN: move LEN bytes from SRC into FD through a
           ;; fresh stream pair. stream.new packs readable|writable<<32;
           ;; the readable end is handed to write-via-stream (OFFSET) or
           ;; append-via-stream, and the guest keeps the writable end.
           ;; A BLOCKED stream.write is joined into the set and waited
           ;; (the event's payload is the CopyResult: state 0, progress
           ;; <<4); the writable end is then dropped, which is what
           ;; lets the provider resolve the future, and the guest had
           ;; parked a future.read at 192 so the resolved result
           ;; (result<_, error-code>) is already in memory when the
           ;; future event arrives.
           "    (func $write-span"
           "          (param $fd i32) (param $src i32) (param $len i32)"
           "          (param $off i64) (param $append i32)"
           "      (local $pk i64) (local $rd i32) (local $wr i32)"
           "      (local $fut i32) (local $ret i32) (local $done i32)"
           "      (local.set $pk (call $snew))"
           "      (local.set $rd (i32.wrap_i64 (local.get $pk)))"
           "      (local.set $wr"
           "        (i32.wrap_i64 (i64.shr_u (local.get $pk) (i64.const 32))))"
           "      (if (local.get $append)"
           "          (then (local.set $fut"
           "                            (call $avs (local.get $fd) (local.get $rd))))"
           "          (else (local.set $fut"
           "                            (call $wvs (local.get $fd) (local.get $rd)"
           "                                       (local.get $off)))))"
           "      (call $wjoin (local.get $fut) (global.get $ws))"
           "      (local.set $ret (call $fread (local.get $fut) (i32.const 192)))"
           "      (if (i32.ne (local.get $ret) (i32.const -1)) (then unreachable))"
           "      (block $wdone"
           "        (loop $wloop"
           "          (br_if $wdone (i32.ge_u (local.get $done) (local.get $len)))"
           "          (local.set $ret"
           "            (call $swrite (local.get $wr)"
           "                          (i32.add (local.get $src) (local.get $done))"
           "                          (i32.sub (local.get $len) (local.get $done))))"
           "          (if (i32.eq (i32.and (local.get $ret) (i32.const 15))"
           "                      (i32.const 0))"
           "              (then"
           "                (local.set $done"
           "                  (i32.add (local.get $done)"
           "                           (i32.shr_u (local.get $ret) (i32.const 4))))"
           "                (br $wloop)))"
           ;; BLOCKED: the copy is staged; wait for its stream-write event
           ;; (code 3) and take the progress the event carries
           "          (call $wjoin (local.get $wr) (global.get $ws))"
           "          (local.set $ret (call $wswait (global.get $ws) (i32.const 224)))"
           "          (if (i32.ne (local.get $ret) (i32.const 3)) (then unreachable))"
           "          (local.set $ret (i32.load (i32.const 228)))"
           "          (if (i32.ne (i32.and (local.get $ret) (i32.const 15))"
           "                      (i32.const 0))"
           "              (then unreachable))"
           "          (if (i32.eqz (i32.shr_u (local.get $ret) (i32.const 4)))"
           "              (then unreachable))"
           "          (local.set $done"
           "            (i32.add (local.get $done)"
           "                     (i32.shr_u (local.get $ret) (i32.const 4))))"
           "          (br $wloop)))"
           "      (call $sdropw (local.get $wr))"
           "      (local.set $ret (call $wswait (global.get $ws) (i32.const 224)))"
           "      (if (i32.ne (local.get $ret) (i32.const 4)) (then unreachable))"
           ;; result<_, error-code> at 192: case 0 is ok
           "      (if (i32.ne (i32.load8_u (i32.const 192)) (i32.const 0))"
           "          (then unreachable))"
           "      (call $wjoin (local.get $fut) (i32.const 0))"
           "      (call $fdropr (local.get $fut)))"
           ;; THE READ SPAN: read FD at OFFSET into DST until the stream
           ;; reports DROPPED (EOF), compare every byte against EXPECT
           ;; (contiguous across chunks), require exactly WANT bytes, and
           ;; require the paired future to resolve ok.
           "    (func $read-span"
           "          (param $fd i32) (param $dst i32) (param $max i32)"
           "          (param $off i64) (param $want i32) (param $expect i32)"
           "      (local $rd i32) (local $fut i32) (local $ret i32)"
           "      (local $n i32) (local $done i32) (local $i i32)"
           "      (call $rvs (local.get $fd) (local.get $off) (i32.const 16))"
           "      (local.set $rd (i32.load (i32.const 16)))"
           "      (local.set $fut (i32.load (i32.const 20)))"
           "      (block $eof"
           "        (loop $more"
           "          (local.set $ret"
           "            (call $sread (local.get $rd) (local.get $dst)"
           "                         (local.get $max)))"
           "          (if (i32.eq (local.get $ret) (i32.const -1))"
           "              (then"
           "                (call $wjoin (local.get $rd) (global.get $ws))"
           "                (local.set $ret"
           "                  (call $wswait (global.get $ws) (i32.const 336)))"
           "                (if (i32.ne (local.get $ret) (i32.const 2))"
           "                    (then unreachable))"
           "                (call $wjoin (local.get $rd) (i32.const 0))"
           "                (local.set $ret (i32.load (i32.const 344)))))"
           ;; CopyResult: DROPPED=1 (EOF); COMPLETED=0 with the count<<4
           "          (if (i32.eq (i32.and (local.get $ret) (i32.const 15))"
           "                      (i32.const 1))"
           "              (then (br $eof)))"
           "          (if (i32.ne (i32.and (local.get $ret) (i32.const 15))"
           "                      (i32.const 0))"
           "              (then unreachable))"
           "          (local.set $n (i32.shr_u (local.get $ret) (i32.const 4)))"
           "          (if (i32.or (i32.eqz (local.get $n))"
           "                      (i32.gt_u (i32.add (local.get $done) (local.get $n))"
           "                                (local.get $want)))"
           "              (then unreachable))"
           "          (local.set $i (i32.const 0))"
           "          (block $bytes"
           "            (loop $byte"
           "              (br_if $bytes (i32.ge_u (local.get $i) (local.get $n)))"
           "              (if (i32.ne"
           "                    (i32.load8_u (i32.add (local.get $dst)"
           "                                          (local.get $i)))"
           "                    (i32.load8_u (i32.add (local.get $expect)"
           "                                         (i32.add (local.get $done)"
           "                                                  (local.get $i)))))"
           "                  (then unreachable))"
           "              (local.set $i (i32.add (local.get $i) (i32.const 1)))"
           "              (br $byte)))"
           "          (local.set $done (i32.add (local.get $done) (local.get $n)))"
           "          (br $more)))"
           "      (if (i32.ne (local.get $done) (local.get $want)) (then unreachable))"
           "      (local.set $ret (call $fread (local.get $fut) (i32.const 24)))"
           "      (if (i32.eq (local.get $ret) (i32.const -1))"
           "          (then"
           "            (call $wjoin (local.get $fut) (global.get $ws))"
           "            (local.set $ret (call $wswait (global.get $ws) (i32.const 336)))"
           "            (if (i32.ne (local.get $ret) (i32.const 4)) (then unreachable))"
           "            (call $wjoin (local.get $fut) (i32.const 0))))"
           "      (if (i32.ne (i32.load8_u (i32.const 24)) (i32.const 0))"
           "          (then unreachable))"
           "      (call $sdropr (local.get $rd))"
           "      (call $fdropr (local.get $fut)))"
           "    (func (export \"run\") (result i32) (i32.const 1))"
           "    (func (export \"run-cb\")"
           "          (param $code i32) (param $si i32) (param $payload i32)"
           "          (result i32)"
           "      (local $base i32) (local $n i32) (local $i i32) (local $e i32)"
           "      (local $pre i32) (local $ret i32) (local $cnt i32)"
           "      (local $stream i32) (local $fut i32) (local $fd i32)"
           "      (local $wr i32) (local $dir i32) (local $found i32)"
           "      (local $pk i64)"
           "      (if (i32.ne (local.get $code) (i32.const 0)) (then unreachable))"
           ;; 1. the preopen named "/work"
           "      (call $getdirs (i32.const 0))"
           "      (local.set $base (i32.load (i32.const 0)))"
           "      (local.set $n (i32.load (i32.const 4)))"
           "      (block $found"
           "        (loop $l"
           "          (br_if $found (i32.ge_u (local.get $i) (local.get $n)))"
           "          (local.set $e (i32.add (local.get $base)"
           "                                 (i32.mul (local.get $i) (i32.const 12))))"
           "          (if (i32.and (i32.eq (i32.load (i32.add (local.get $e) (i32.const 8)))"
           "                               (i32.const 5))"
           "                       (call $streq (i32.load (i32.add (local.get $e) (i32.const 4)))"
           "                                    (i32.const 96) (i32.const 5)))"
           "              (then (local.set $pre (i32.load (local.get $e)))))"
           "          (local.set $i (i32.add (local.get $i) (i32.const 1)))"
           "          (br $l)))"
           "      (if (i32.eqz (local.get $pre)) (then unreachable))"
           "      (global.set $ws (call $wsnew))"

           ;; 2. open "hello.txt" read-only through the preopen.
           ;; The async lower has 5 flat params (self borrow, one
           ;; path-flags field, the path's ptr+len, open-flags,
           ;; descriptor-flags), so MAX_FLAT_ASYNC_PARAMS = 4 collapses
           ;; the WHOLE parameter tuple into the first core arg. The
           ;; tuple layout is (self i32 @0, path-flags u8 @4, string
           ;; ptr @8 / len @12, open-flags u8 @16, descriptor-flags
           ;; u8 @17).
           "      (i32.store (i32.const 32) (local.get $pre))"
           "      (i32.store8 (i32.const 36) (i32.const 0))"
           "      (i32.store (i32.const 40) (i32.const 64))"
           "      (i32.store (i32.const 44) (i32.const 9))"
           "      (i32.store8 (i32.const 48) (i32.const 0))"
           "      (i32.store8 (i32.const 49) (i32.const 1))"
           "      (local.set $ret (call $openat (i32.const 32) (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 0)) (then unreachable))"
           "      (local.set $fd (i32.load (i32.const 12)))"
           ;; 3. read it through read-via-stream + stream.read
           "      (call $rvs (local.get $fd) (i64.const 0) (i32.const 16))"
           "      (local.set $stream (i32.load (i32.const 16)))"
           "      (local.set $fut (i32.load (i32.const 20)))"
           "      (local.set $ret (call $sread (local.get $stream)"
           "                                   (i32.const 256) (i32.const 64)))"
           "      (if (i32.eq (local.get $ret) (i32.const -1))"
           "          (then"
           "            (call $wjoin (local.get $stream) (global.get $ws))"
           "            (local.set $ret (call $wswait (global.get $ws) (i32.const 336)))"
           "            (if (i32.ne (local.get $ret) (i32.const 2)) (then unreachable))"
           "            (call $wjoin (local.get $stream) (i32.const 0))"
           "            (local.set $ret (i32.load (i32.const 344)))))"
           "      (if (i32.ne (i32.and (local.get $ret) (i32.const 15)) (i32.const 0))"
           "          (then unreachable))"
           "      (local.set $cnt (i32.shr_u (local.get $ret) (i32.const 4)))"
           "      (if (i32.ne (local.get $cnt) (i32.const 17)) (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 256)) (i32.load (i32.const 112)))"
           "          (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 260)) (i32.load (i32.const 116)))"
           "          (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 264)) (i32.load (i32.const 120)))"
           "          (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 268)) (i32.load (i32.const 124)))"
           "          (then unreachable))"
           "      (if (i32.ne (i32.load8_u (i32.const 272)) (i32.load8_u (i32.const 128)))"
           "          (then unreachable))"
           ;; the stream's paired future must resolve ok
           "      (local.set $ret (call $fread (local.get $fut) (i32.const 24)))"
           "      (if (i32.eq (local.get $ret) (i32.const -1))"
           "          (then"
           "            (call $wjoin (local.get $fut) (global.get $ws))"
           "            (local.set $ret (call $wswait (global.get $ws) (i32.const 336)))"
           "            (if (i32.ne (local.get $ret) (i32.const 4)) (then unreachable))"
           "            (call $wjoin (local.get $fut) (i32.const 0))))"
           "      (if (i32.ne (i32.load8_u (i32.const 24)) (i32.const 0)) (then unreachable))"
           "      (call $sdropr (local.get $stream))"
           "      (call $fdropr (local.get $fut))"
           ;; 3a. THE WRITE REFUSAL: this descriptor was opened with the
           ;; read flag only, so write-via-stream must REFUSE with
           ;; error-code::read-only (case 32) through the paired future —
           ;; never a trap, and never a byte written. The provider drops
           ;; the guest's data end; the guest drops its writable peer and
           ;; reads the already-resolved future (eager CopyResult 0 with
           ;; result<_, error-code> = err/read-only at 24).
           "      (local.set $pk (call $snew))"
           "      (local.set $ret (i32.wrap_i64 (local.get $pk)))"
           "      (local.set $wr"
           "        (i32.wrap_i64 (i64.shr_u (local.get $pk) (i64.const 32))))"
           "      (local.set $fut"
           "        (call $wvs (local.get $fd) (local.get $ret) (i64.const 0)))"
           "      (call $sdropw (local.get $wr))"
           "      (local.set $ret (call $fread (local.get $fut) (i32.const 24)))"
           "      (if (i32.eq (local.get $ret) (i32.const -1))"
           "          (then"
           "            (call $wjoin (local.get $fut) (global.get $ws))"
           "            (local.set $ret (call $wswait (global.get $ws) (i32.const 336)))"
           "            (if (i32.ne (local.get $ret) (i32.const 4)) (then unreachable))"
           "            (call $wjoin (local.get $fut) (i32.const 0))))"
           "      (if (i32.ne (i32.load8_u (i32.const 24)) (i32.const 1)) (then unreachable))"
           "      (if (i32.ne (i32.load8_u (i32.const 28)) (i32.const 32)) (then unreachable))"
           "      (call $fdropr (local.get $fut))"
           "      (call $fdrop (local.get $fd))"
           ;; 4. THE WRITE PATH: create+truncate "out.txt" through the
           ;; preopen with read+write descriptor flags (open-flags
           ;; create|truncate = 9, flags read|write = 3). The created
           ;; file's own descriptor carries both, so the same handle
           ;; writes and reads back.
           "      (i32.store (i32.const 32) (local.get $pre))"
           "      (i32.store8 (i32.const 36) (i32.const 0))"
           "      (i32.store (i32.const 40) (i32.const 512))"
           "      (i32.store (i32.const 44) (i32.const 7))"
           "      (i32.store8 (i32.const 48) (i32.const 9))"
           "      (i32.store8 (i32.const 49) (i32.const 3))"
           "      (local.set $ret (call $openat (i32.const 32) (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 0)) (then unreachable))"
           "      (local.set $fd (i32.load (i32.const 12)))"
           ;; 4a. write-via-stream at offset 0
           "      (call $write-span (local.get $fd) (i32.const 520)"
           "                        (i32.const 15) (i64.const 0) (i32.const 0))"
           ;; 4b. a NONZERO offset: "!" at 17 leaves 15..16 zero, the
           ;; WIT's "bytes between the previous end and the start of the
           ;; write set to zero"
           "      (call $write-span (local.get $fd) (i32.const 536)"
           "                        (i32.const 1) (i64.const 17) (i32.const 0))"
           ;; 4c. append-via-stream: "-app" at the current EOF
           "      (call $write-span (local.get $fd) (i32.const 544)"
           "                        (i32.const 4) (i64.const 0) (i32.const 1))"
           ;; 4d. stat: ok, descriptor-type regular-file (case 5),
           ;; link-count 1, size 22 (u64). The result variant's payload
           ;; begins at retptr+8: the variant's join alignment is the
           ;; MAXIMUM over its cases, and the ok case (descriptor-stat,
           ;; u64 fields) is 8-aligned, not the error-code's 4. The
           ;; descriptor-stat record there is 104 bytes: descriptor-type
           ;; (16, disc at +0), link-count at +16, size at +24.
           "      (local.set $ret (call $stat (local.get $fd) (i32.const 400)))"
           "      (call $settle (local.get $ret))"
           ;; result<descriptor-stat, error-code>: case 0 is ok
           "      (if (i32.ne (i32.load8_u (i32.const 400)) (i32.const 0)) (then unreachable))"
           "      (if (i32.ne (i32.load8_u (i32.const 408)) (i32.const 5)) (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 424)) (i32.const 1)) (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 432)) (i32.const 22)) (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 436)) (i32.const 0)) (then unreachable))"
           ;; 4e. read the same file back at offset 0 through
           ;; read-via-stream: all 22 bytes must equal the expected span
           "      (call $read-span (local.get $fd) (i32.const 256)"
           "                       (i32.const 64) (i64.const 0) (i32.const 22)"
           "                       (i32.const 552))"
           "      (call $fdrop (local.get $fd))"
           ;; 5. the negative probe: a missing path is an ERR result
           ;; (error-code::no-entry = case 19), never a trap. The whole
           ;; tuple is rewritten: the write open left create|truncate and
           ;; read|write in it, and a create would make the probe pass.
           "      (i32.store (i32.const 32) (local.get $pre))"
           "      (i32.store8 (i32.const 36) (i32.const 0))"
           "      (i32.store (i32.const 40) (i32.const 80))"
           "      (i32.store (i32.const 44) (i32.const 11))"
           "      (i32.store8 (i32.const 48) (i32.const 0))"
           "      (i32.store8 (i32.const 49) (i32.const 1))"
           "      (local.set $ret (call $openat (i32.const 32) (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 1)) (then unreachable))"
           "      (if (i32.ne (i32.load8_u (i32.const 12)) (i32.const 19)) (then unreachable))"
           ;; 6. THE DIRECTORY SURFACE. create-directory-at("subdir")
           ;; must succeed; open-at("subdir", directory, read) yields a
           ;; directory descriptor; stat reports directory (case 2);
           ;; read-directory on the PREOPEN enumerates the entries and
           ;; must list hello.txt, out.txt, subdir and temp.txt; rename-at
           ;; (temp.txt -> moved.txt) + stat-at prove the rename,
           ;; unlink-file-at removes it and remove-directory-at removes
           ;; subdir — every temporary object is gone before the task
           ;; returns, so the case's repeats share the workdir safely.
           "      (local.set $ret (call $mkdir (local.get $pre)"
           "                                   (i32.const 600) (i32.const 6)"
           "                                   (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 0)) (then unreachable))"
           ;; 6a. open the new directory: open-flags directory(2),
           ;; descriptor-flags read(1) — a read-only open of a directory
           ;; needs no mutate-directory authority
           "      (i32.store (i32.const 32) (local.get $pre))"
           "      (i32.store8 (i32.const 36) (i32.const 0))"
           "      (i32.store (i32.const 40) (i32.const 600))"
           "      (i32.store (i32.const 44) (i32.const 6))"
           "      (i32.store8 (i32.const 48) (i32.const 2))"
           "      (i32.store8 (i32.const 49) (i32.const 1))"
           "      (local.set $ret (call $openat (i32.const 32) (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 0)) (then unreachable))"
           "      (local.set $dir (i32.load (i32.const 12)))"
           ;; 6b. stat($dir): ok, descriptor-type directory (case 2) at
           ;; the payload's +0, link-count at +16 — at least one link
           "      (local.set $ret (call $stat (local.get $dir) (i32.const 400)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 400)) (i32.const 0)) (then unreachable))"
           "      (if (i32.ne (i32.load8_u (i32.const 408)) (i32.const 2)) (then unreachable))"
           "      (if (i32.lt_u (i32.load (i32.const 424)) (i32.const 1)) (then unreachable))"
           ;; 6c. create a second file for the rename probe
           "      (i32.store (i32.const 32) (local.get $pre))"
           "      (i32.store8 (i32.const 36) (i32.const 0))"
           "      (i32.store (i32.const 40) (i32.const 608))"
           "      (i32.store (i32.const 44) (i32.const 8))"
           "      (i32.store8 (i32.const 48) (i32.const 9))"
           "      (i32.store8 (i32.const 49) (i32.const 2))"
           "      (local.set $ret (call $openat (i32.const 32) (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 0)) (then unreachable))"
           "      (call $fdrop (i32.load (i32.const 12)))"
           ;; 6d. read-directory(preopen): tuple<stream, future>. Every
           ;; delivered entry is 24 bytes and its NAME is compared
           ;; against the four expected names; the loop ends on the
           ;; stream's DROPPED (EOF) and the paired future must be ok.
           "      (call $readdir (local.get $pre) (i32.const 16))"
           "      (local.set $stream (i32.load (i32.const 16)))"
           "      (local.set $fut (i32.load (i32.const 20)))"
           "      (local.set $found (i32.const 0))"
           "      (block $deof"
           "        (loop $dmore"
           "          (local.set $ret (call $sreadd (local.get $stream)"
           "                                       (i32.const 1024) (i32.const 8)))"
           "          (if (i32.eq (local.get $ret) (i32.const -1))"
           "              (then"
           "                (call $wjoin (local.get $stream) (global.get $ws))"
           "                (local.set $ret (call $wswait (global.get $ws) (i32.const 336)))"
           "                (if (i32.ne (local.get $ret) (i32.const 2)) (then unreachable))"
           "                (call $wjoin (local.get $stream) (i32.const 0))"
           "                (local.set $ret (i32.load (i32.const 344)))))"
           "          (if (i32.eq (i32.and (local.get $ret) (i32.const 15)) (i32.const 1))"
           "              (then (br $deof)))"
           "          (if (i32.ne (i32.and (local.get $ret) (i32.const 15)) (i32.const 0))"
           "              (then unreachable))"
           "          (local.set $n (i32.shr_u (local.get $ret) (i32.const 4)))"
           "          (if (i32.eqz (local.get $n)) (then unreachable))"
           "          (local.set $i (i32.const 0))"
           "          (block $ents"
           "            (loop $ent"
           "              (br_if $ents (i32.ge_u (local.get $i) (local.get $n)))"
           "              (local.set $e (i32.add (i32.const 1024)"
           "                                     (i32.mul (local.get $i) (i32.const 24))))"
           "              (if (call $nameeq (local.get $e) (i32.const 64) (i32.const 9))"
           "                  (then (local.set $found"
           "                         (i32.or (local.get $found) (i32.const 1)))))"
           "              (if (call $nameeq (local.get $e) (i32.const 512) (i32.const 7))"
           "                  (then (local.set $found"
           "                         (i32.or (local.get $found) (i32.const 2)))))"
           "              (if (call $nameeq (local.get $e) (i32.const 600) (i32.const 6))"
           "                  (then (local.set $found"
           "                         (i32.or (local.get $found) (i32.const 4)))))"
           "              (if (call $nameeq (local.get $e) (i32.const 608) (i32.const 8))"
           "                  (then (local.set $found"
           "                         (i32.or (local.get $found) (i32.const 8)))))"
           "              (local.set $i (i32.add (local.get $i) (i32.const 1)))"
           "              (br $ent)))"
           "          (br $dmore)))"
           "      (if (i32.ne (local.get $found) (i32.const 15)) (then unreachable))"
           "      (local.set $ret (call $fread (local.get $fut) (i32.const 24)))"
           "      (if (i32.eq (local.get $ret) (i32.const -1))"
           "          (then"
           "            (call $wjoin (local.get $fut) (global.get $ws))"
           "            (local.set $ret (call $wswait (global.get $ws) (i32.const 336)))"
           "            (if (i32.ne (local.get $ret) (i32.const 4)) (then unreachable))"
           "            (call $wjoin (local.get $fut) (i32.const 0))))"
           "      (if (i32.ne (i32.load8_u (i32.const 24)) (i32.const 0)) (then unreachable))"
           "      (call $sdropr (local.get $stream))"
           "      (call $fdropr (local.get $fut))"
           ;; 6e. rename-at: the 6 flat params exceed
           ;; MAX_FLAT_ASYNC_PARAMS, so the params tuple is at 32
           ;; (self@0, old ptr@4/len@8, new-descriptor@12, new ptr@16/
           ;; len@20) and the core call takes (params-ptr, retptr).
           "      (i32.store (i32.const 32) (local.get $pre))"
           "      (i32.store (i32.const 36) (i32.const 608))"
           "      (i32.store (i32.const 40) (i32.const 8))"
           "      (i32.store (i32.const 44) (local.get $pre))"
           "      (i32.store (i32.const 48) (i32.const 624))"
           "      (i32.store (i32.const 52) (i32.const 9))"
           "      (local.set $ret (call $rename (i32.const 32) (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 0)) (then unreachable))"
           ;; 6f. stat-at("moved.txt"): 4 flat params stay scalar, so
           ;; the core call is (self, flags, ptr, len, retptr); ok,
           ;; regular-file (5), size 0 (created empty)
           "      (local.set $ret (call $statat (local.get $pre) (i32.const 0)"
           "                                    (i32.const 624) (i32.const 9)"
           "                                    (i32.const 400)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 400)) (i32.const 0)) (then unreachable))"
           "      (if (i32.ne (i32.load8_u (i32.const 408)) (i32.const 5)) (then unreachable))"
           "      (if (i32.ne (i32.load (i32.const 432)) (i32.const 0)) (then unreachable))"
           ;; 6g. unlink-file-at(moved.txt) then remove-directory-at
           ;; (subdir): both must report ok
           "      (local.set $ret (call $unlink (local.get $pre) (i32.const 624)"
           "                                    (i32.const 9) (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 0)) (then unreachable))"
           "      (local.set $ret (call $rmdir (local.get $pre) (i32.const 600)"
           "                                   (i32.const 6) (i32.const 8)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 8)) (i32.const 0)) (then unreachable))"
           ;; 6h. stat-at("subdir") after the removal: err/no-entry. The
           ;; result's join alignment is descriptor-stat's 8, so the
           ;; error-code discriminant sits at 408.
           "      (local.set $ret (call $statat (local.get $pre) (i32.const 0)"
           "                                    (i32.const 600) (i32.const 6)"
           "                                    (i32.const 400)))"
           "      (call $settle (local.get $ret))"
           "      (if (i32.ne (i32.load8_u (i32.const 400)) (i32.const 1)) (then unreachable))"
           "      (if (i32.ne (i32.load8_u (i32.const 408)) (i32.const 19)) (then unreachable))"
           "      (call $fdrop (local.get $dir))"
           ;; 7. the preopen's own handle goes too
           "      (call $fdrop (local.get $pre))"
           "      (call $tr (i32.const 0))"
           "      (i32.const 0)))"
           "  (core instance $m"
           "    (instantiate $M"
           "      (with \"env\""
           "        (instance"
           "          (export \"mem\" (memory $mem \"mem\"))"
           "          (export \"get-directories\" (func $getdirs-lowered))"
           "          (export \"open-at\" (func $openat-lowered))"
           "          (export \"read-via-stream\" (func $rvs-lowered))"
           "          (export \"write-via-stream\" (func $wvs-lowered))"
           "          (export \"append-via-stream\" (func $avs-lowered))"
           "          (export \"stat\" (func $stat-lowered))"
           "          (export \"create-directory-at\" (func $mkdir-lowered))"
           "          (export \"stat-at\" (func $statat-lowered))"
           "          (export \"read-directory\" (func $readdir-lowered))"
           "          (export \"rename-at\" (func $rename-lowered))"
           "          (export \"unlink-file-at\" (func $unlink-lowered))"
           "          (export \"remove-directory-at\" (func $rmdir-lowered))"
           "          (export \"task.return\" (func $tr))"
           "          (export \"waitable-set.new\" (func $wsnew))"
           "          (export \"waitable.join\" (func $wjoin))"
           "          (export \"waitable-set.wait\" (func $wswait))"
           "          (export \"subtask.drop\" (func $subdrop))"
           "          (export \"stream.new\" (func $snew))"
           "          (export \"stream.read\" (func $sread))"
           "          (export \"stream.read-dir\" (func $sreadd))"
           "          (export \"stream.write\" (func $swrite))"
           "          (export \"stream.drop-readable\" (func $sdropr))"
           "          (export \"stream.drop-writable\" (func $sdropw))"
           "          (export \"future.read\" (func $fread))"
           "          (export \"future.drop-readable\" (func $fdropr))"
           "          (export \"resource.drop\" (func $fdrop))))))"
           "  (func $run (export \"run\") async (result $res)"
           "    (canon lift (core func $m \"run\") async"
           "                (callback (core func $m \"run-cb\"))))"
           "  (instance $runiface (export \"run\" (func $run)))"
           "  (export \"wasi:cli/run@0.3.2\" (instance $runiface)"
           "    (instance (export \"run\" (func async (result $res))))))")))

(defun %canon-definition (comp kind)
  "Rewrite the text parser's :builtin classification of the dotted
canon DEFINITION KIND (:resource-drop / :resource-new / :resource-rep)
into the canon definition the encoder understands. The parser routes
EVERY dotted name to :builtin (src/component-text.lisp), while these
three are canon-style definitions carrying a typeidx immediate; the
binary encoding is opcode 0x03/0x02/0x04 + typeidx."
  (loop for d across (wasmdown.component::comp-defs comp)
        do (let ((p (and (eq (wasmdown.component::compdef-kind d) :canon)
                         (wasmdown.component::compdef-payload d))))
             (when (and p
                        (eq (wasmdown.component::canonrec-kind p) :builtin)
                        (eq (first (wasmdown.component::canonrec-imms p))
                            kind))
               (let ((idx (second (wasmdown.component::canonrec-imms p))))
                 (setf (wasmdown.component::canonrec-kind p) kind
                       (wasmdown.component::canonrec-typeidx p) idx
                       (wasmdown.component::canonrec-imms p) nil)))))
  comp)

(defun %normalize-eq-bounds (comp)
  "The text parser emits a `(type (eq N))` externtype payload as the
LIST (:type :eq N); tools/component-encode's enc-typebound reads the
CONS shape (:eq . N) (no earlier fixture used an eq-bound export).
Normalize the IR the encoder consumes."
  (loop for d across (wasmdown.component::comp-defs comp)
        do (let ((p (wasmdown.component::compdef-payload d)))
             (when (and (eq (wasmdown.component::compdef-kind d) :type)
                        (eq (wasmdown.component::ct-kind p) :instance))
               (loop for td across (wasmdown.component::ct-payload p)
                     when (member (wasmdown.component::typedecl-kind td)
                                  '(:import :export))
                       do (let* ((pl (wasmdown.component::typedecl-payload td))
                                 (et (third pl)))
                            (when (and (consp et)
                                       (eq (car et) :type)
                                       (consp (cdr et))
                                       (eq (second et) :eq)
                                       (= (length et) 3))
                              (setf (third pl)
                                    (cons :type
                                          (cons :eq
                                                (third et))))))))))
  comp)

(defun p3-fs-guest-bytes ()
  (component-encode:encode-component
   (%normalize-eq-bounds
    (%canon-definition
     (wasmdown.component.text::parse-component-text *p3-fs-text*)
     :resource-drop))))

(defvar *fs-context-counter* 0)

(defun p3-fs-context (path)
  "The filesystem capability context the fixture asserts against: one
preopen named /work over a fresh temporary directory seeded with
hello.txt. :AFTER closes the preopen and removes the directory (the
builder runs in-process, so it owns the host handle)."
  (declare (ignore path))
  (let* ((dir (merge-pathnames
               ;; the pid + a counter make the name unique ACROSS
               ;; processes: SBCL's default random-state is seeded
               ;; deterministically, so a bare (random ...) reused the
               ;; same directory after a failed run left residue (a
               ;; leftover "subdir" then made the guest's
               ;; create-directory-at return exist and trap)
               (format nil "wasmdown-fs-~d-~36r/"
                       (sb-unix:unix-getpid) (incf *fs-context-counter*))
               (uiop:temporary-directory)))
         (seed (merge-pathnames "hello.txt" dir)))
    (ensure-directories-exist dir)
    (with-open-file (s seed :direction :output :if-exists :supersede)
      (format s "filesystem-probe~%"))
    (let ((pre (wasmdown.wasi:open-wasi-preopen dir :name "/work")))
      (list :preopens (list pre)
            :after (lambda ()
                     (ignore-errors (wasmdown.wasi:close-wasi-preopen pre))
                     (ignore-errors
                       (uiop:delete-directory-tree
                        dir :validate t :if-does-not-exist :ignore)))))))

(defun p3-cli-context (path)
  "The capability context the CLI-lane fixture asserts against: argv
(the path the runner gave it as argv[0], plus the marker the suite
passes), the environment pairs the suite passes with --env, and the
suite's stdin bytes (\"stdin-probe\\n\")."
  (let ((argv0 (namestring path)))
    (list :arguments (list argv0 "wmd-cli-marker")
          :stdin (make-string-input-stream (format nil "stdin-probe~%"))
          :environment (list (cons "WMD_ENV_PROBE" "env-probe-value")
                             (cons "WMD_EXPECT_ARGV0" argv0)
                             (cons "WMD_EXPECT_TERMINAL" "none")))))

;;; Each entry: NAME BUILDER EXPECTED-STATUS EXPECTED-STDOUT
;;; EXPECTED-STDERR CONTEXT-FN (name -> a plist of WASI context
;;; keyword arguments, or NIL for the plain fixtures).

(defparameter *fixtures*
  (list (list "p3-guest.wasm" #'p3-sync-guest-bytes 0 "" "" nil)
        (list "p3-import-guest.wasm" #'p3-import-guest-bytes 7 "" "" nil)
        (list "p3-clock-guest.wasm" #'p3-clock-guest-bytes 0 "" "" nil)
        (list "p3-async-guest.wasm" #'p3-async-guest-bytes 0 "" "" nil)
        (list "p3-stream-guest.wasm" #'p3-stream-guest-bytes 0 (format nil "hi~%")
              "" nil)
        (list "p3-cancel-guest.wasm" #'p3-cancel-guest-bytes 0 "" "" nil)
        (list "p3-random-guest.wasm" #'p3-random-guest-bytes 0 "" "" nil)
        (list "p3-deadline-guest.wasm" #'p3-deadline-guest-bytes 0 "" "" nil)
        (list "p3-cli-guest.wasm" #'p3-cli-guest-bytes 0 ""
              (format nil "p3-cli-guest: stderr probe~%")
              #'p3-cli-context)
        (list "p3-fs-guest.wasm" #'p3-fs-guest-bytes 0 "" ""
              #'p3-fs-context)))

;;; ---------------------------------------------------------------
;;; Verify through this project's own decoder/validator/runner, then
;;; write (or check) the artifacts.

(defun sha256 (path)
  "The FIRST field of sha256sum's output: the digest, without the path."
  (let ((text (with-output-to-string (out)
                (sb-ext:run-program "sha256sum" (list (namestring path))
                                    :output out :search t))))
    (subseq text 0 64)))

(defun recorded-sha (name)
  (when (probe-file *sums*)
    (with-open-file (s *sums*)
      (loop for line = (read-line s nil nil)
            while line
            when (search name line)
              do (return (subseq line 0 (min 64 (length line))))))))

(defun verify (label bytes expected-status expected-stdout
               &key expected-stderr arguments environment preopens stdin)
  (let* ((comp (wasmdown.component:decode-component bytes))
         (_ (wasmdown.component:validate-component comp))
         (sink (make-string-output-stream))
         (errsink (make-string-output-stream))
         ;; the reactor services clock deadlines; the cancellation
         ;; fixture lowers the provider's async wait-for
         (reactor (wasmdown.wasi:make-wasi-reactor))
         (context (apply #'wasmdown.wasi:make-wasi-context
                         :stdout sink :stderr errsink :reactor reactor
                         (append (and arguments (list :arguments arguments))
                                 (and environment
                                      (list :environment environment))
                                 (and stdin (list :stdin stdin))
                                 (and preopens
                                      (list :preopens preopens)))))
         (status (progn (wasmdown.wasi:run-component comp context))))
    (declare (ignore _))
    (ignore-errors (wasmdown.wasi:reactor-close reactor))
    (unless (eql expected-status status)
      (error "~a: command returned status ~a, expected ~a"
             label status expected-status))
    (let ((out (get-output-stream-string sink))
          (err (get-output-stream-string errsink)))
      (unless (string= expected-stdout out)
        (error "~a: stdout ~s, expected ~s" label out expected-stdout))
      (unless (string= expected-stderr err)
        (error "~a: stderr ~s, expected ~s" label err expected-stderr))
      (format t "~&~a: decoded, validated, ran (status ~a, stdout ~s, stderr ~s)~%"
              label status expected-stdout expected-stderr))))

;;; ---------------------------------------------------------------
;;; Main.

(let ((check (and (member "--check" sb-ext:*posix-argv* :test #'string=) t)))
  (if check
      (let ((bad 0))
        (dolist (entry *fixtures*)
          (destructuring-bind (name _ status stdout stderr context) entry
            (declare (ignore _ status stdout stderr context))
            (let ((path (merge-pathnames name *dir*))
                  (recorded (recorded-sha name)))
              (if (not (probe-file path))
                  (progn (format t "~&missing fixture ~a (run without --check)~%" path)
                         (incf bad))
                  (let ((actual (sha256 path)))
                    (if (equal recorded actual)
                        (format t "~&~a: OK ~a~%" name actual)
                        (progn (format t "~&~a: STALE (recorded ~a, actual ~a)~%"
                                       name recorded actual)
                               (incf bad))))))))
        (sb-ext:exit :code (if (zerop bad) 0 1)))
      (progn
        (ensure-directories-exist *dir*)
        (let ((lines '()))
          (dolist (entry *fixtures*)
            (destructuring-bind (name builder status stdout stderr context) entry
              (let* ((bytes (funcall builder))
                     (path (merge-pathnames name *dir*))
                     (extra (and context (funcall context path))))
                (with-open-file (s path :direction :output :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
                  (write-sequence bytes s))
                (verify name bytes status stdout
                        :expected-stderr stderr
                        :arguments (getf extra :arguments)
                        :environment (getf extra :environment)
                        :stdin (getf extra :stdin)
                        :preopens (getf extra :preopens))
                (let ((after (getf extra :after)))
                  (when after (funcall after)))
                (push (format nil "~a  ~a" (sha256 path) name) lines)
                (format t "~&wrote ~a~%" path))))
          (with-open-file (s *sums* :direction :output :if-exists :supersede)
            (dolist (line (nreverse lines))
              (format s "~a~%" line)))
          (format t "~&wrote ~a~%" *sums*)))))
(sb-ext:exit :code 0)
