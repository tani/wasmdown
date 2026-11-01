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

(defparameter *fixtures*
  (list (list "p3-guest.wasm" #'p3-sync-guest-bytes 0 "")
        (list "p3-import-guest.wasm" #'p3-import-guest-bytes 7 "")
        (list "p3-clock-guest.wasm" #'p3-clock-guest-bytes 0 "")
        (list "p3-async-guest.wasm" #'p3-async-guest-bytes 0 "")
        (list "p3-stream-guest.wasm" #'p3-stream-guest-bytes 0 "hi
")
        (list "p3-cancel-guest.wasm" #'p3-cancel-guest-bytes 0 "")))

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

(defun verify (label bytes expected-status expected-stdout)
  (let* ((comp (wasmdown.component:decode-component bytes))
         (_ (wasmdown.component:validate-component comp))
         (sink (make-string-output-stream))
         ;; the reactor services clock deadlines; the cancellation
         ;; fixture lowers the provider's async wait-for
         (reactor (wasmdown.wasi:make-wasi-reactor))
         (context (wasmdown.wasi:make-wasi-context :stdout sink
                                                   :reactor reactor))
         (status (progn (wasmdown.wasi:run-component comp context))))
    (declare (ignore _))
    (ignore-errors (wasmdown.wasi:reactor-close reactor))
    (unless (eql expected-status status)
      (error "~a: command returned status ~a, expected ~a"
             label status expected-status))
    (let ((out (get-output-stream-string sink)))
      (unless (string= expected-stdout out)
        (error "~a: stdout ~s, expected ~s" label out expected-stdout)))
    (format t "~&~a: decoded, validated, ran (status ~a, stdout ~s)~%"
            label status expected-stdout)))

;;; ---------------------------------------------------------------
;;; Main.

(let ((check (and (member "--check" sb-ext:*posix-argv* :test #'string=) t)))
  (if check
      (let ((bad 0))
        (dolist (entry *fixtures*)
          (destructuring-bind (name _ status stdout) entry
            (declare (ignore _ status stdout))
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
            (destructuring-bind (name builder status stdout) entry
              (let* ((bytes (funcall builder))
                     (path (merge-pathnames name *dir*)))
                (with-open-file (s path :direction :output :if-exists :supersede
                                        :element-type '(unsigned-byte 8))
                  (write-sequence bytes s))
                (verify name bytes status stdout)
                (push (format nil "~a  ~a" (sha256 path) name) lines)
                (format t "~&wrote ~a~%" path))))
          (with-open-file (s *sums* :direction :output :if-exists :supersede)
            (dolist (line (nreverse lines))
              (format s "~a~%" line)))
          (format t "~&wrote ~a~%" *sums*)))))
(sb-ext:exit :code 0)
