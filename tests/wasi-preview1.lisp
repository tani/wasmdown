;;;; WASI Preview1 (wasi_snapshot_preview1) behavioral tests.
;;;;
;;;; Every assertion is made against a REAL guest: the text parser builds
;;;; the module, `run-preview1'/`make-preview1-instance' executes it, and
;;;; the observable is guest memory or the configured host stream. The
;;;; pinned ABI census lives in specs/wasi/preview1/INVENTORY.md; these
;;;; tests exercise the semantics the inventory names.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defpackage #:wasmdown.test.wasi-preview1
  (:use #:cl #:wasmdown.test))

(in-package #:wasmdown.test.wasi-preview1)

;;; ---------------------------------------------------------------
;;; helpers

(defun p1-instance (wat context &key sockets (mode :interpret))
  "Parse WAT and instantiate it with the Preview1 imports in MODE.
Returns (values INSTANCE STATE)."
  (wasmdown.wasi:make-preview1-instance
   (funcall (find-symbol "PARSE-WAT" '#:wasmdown.text) wat)
   context
   :mode mode
   :sockets sockets))

(defun p1-state-memory (state)
  (wasmdown.wasi::preview1-state-memory state))

(defun p1-bytes (state)
  (wasmdown.runtime:memory-instance-bytes (p1-state-memory state)))

(defun p1-u32 (state addr)
  (let ((b (p1-bytes state)))
    (logior (aref b addr)
            (ash (aref b (+ addr 1)) 8)
            (ash (aref b (+ addr 2)) 16)
            (ash (aref b (+ addr 3)) 24))))

(defun p1-u64 (state addr)
  (logior (p1-u32 state addr) (ash (p1-u32 state (+ addr 4)) 32)))

(defun p1-set-u32 (state addr value)
  (let ((b (p1-bytes state)))
    (loop for i from 0 below 4
          do (setf (aref b (+ addr i)) (logand (ash value (* -8 i)) #xff))))
  value)

(defun p1-string (state addr len)
  (map 'string #'code-char
       (subseq (p1-bytes state) addr (+ addr len))))

(defun is= (expected actual)
  (unless (= expected actual)
    (error "expected ~a but got ~a" expected actual)))

(defun p1-call (instance name &rest args)
  "Invoke a guest export and return its FIRST result value (the ABI
functions here are single-valued or void)."
  (first (apply (find-symbol "%P1-INVOKE" '#:wasmdown.wasi)
                instance name args)))

(defun p1-context (&key arguments environment stdin stdout preopens networks)
  (wasmdown.wasi:make-wasi-context
   :arguments arguments :environment environment
   :stdin stdin :stdout stdout :preopens preopens :networks networks))

(defun p1-temp-dir ()
  "A fresh, UNIQUE temp directory (trailing slash). MKDTEMP allocates
atomically: SBCL's default *RANDOM-STATE* is deterministic in this
build, so the old (random ...) name repeated across processes and
reused stale directories left in /tmp by earlier runs — the leaked
contents then showed up in the readdir listing."
  (concatenate 'string (sb-posix:mkdtemp "/tmp/wmd-p1-XXXXXX") "/"))

(defmacro with-temp-dir ((var) &body body)
  `(let ((,var (p1-temp-dir)))
     (unwind-protect (progn ,@body)
       (ignore-errors
         (let ((dir (pathname (concatenate 'string ,var))))
           (dolist (f (directory (merge-pathnames "*.*" dir)))
             (ignore-errors (delete-file f)))
           (ignore-errors (sb-posix:rmdir ,var)))))))

(defun write-text-file (path text)
  (with-open-file (s path :direction :output :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence (map '(vector (unsigned-byte 8)) #'char-code text) s))
  path)

;;; ---------------------------------------------------------------
;;; the start section runs AFTER the memory export exists

(test wasi-preview1/start-section-uses-memory
  ;; The module's START section calls fd_write; the ABI memory must be
  ;; bound before it runs, and the active data segment must already be
  ;; in place. _start then writes its own bytes.
  (let* ((out (make-string-output-stream))
         (wat "(module
                 (import \"wasi_snapshot_preview1\" \"fd_write\"
                         (func $w (param i32 i32 i32 i32) (result i32)))
                 (memory (export \"memory\") 1)
                 (data (i32.const 100) \"start\\n\")
                 (data (i32.const 110) \"after\\n\")
                 (func $wstr (param i32 i32)
                   (i32.store (i32.const 0) (local.get 0))
                   (i32.store (i32.const 4) (local.get 1))
                   (drop (call $w (i32.const 1) (i32.const 0) (i32.const 1)
                                 (i32.const 8))))
                 (func $start (call $wstr (i32.const 100) (i32.const 6)))
                 (start $start)
                 (func (export \"_start\")
                   (call $wstr (i32.const 110) (i32.const 6))))")
         (module (funcall (find-symbol "PARSE-WAT" '#:wasmdown.text) wat))
         (ctx (p1-context :arguments '("x") :stdout out)))
    (is (= 0 (wasmdown.wasi:run-preview1 module ctx)))
    (is (string= (format nil "start~%after~%")
                 (get-output-stream-string out)))))

;;; ---------------------------------------------------------------
;;; ABI edge cases: iovec bounds, huge counts, stale fds

(test wasi-preview1/iovec-out-of-range-is-fault
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"fd_write\"
                              (func $w (param i32 i32 i32 i32) (result i32)))
                      (memory (export \"memory\") 1)
                      (func (export \"go\") (result i32)
                        ;; iovec table at 0xFFFFFF00: outside the 1-page memory
                        (call $w (i32.const 1) (i32.const 4294967040)
                                 (i32.const 1) (i32.const 8))))"
                    (p1-context :stdout (make-string-output-stream)))
    (declare (ignore state))
    ;; valid module: the ABI returns errno::fault (21), never a Lisp error
    (is (= 21 (p1-call instance "go")))))

(test wasi-preview1/huge-iovec-count-is-fault
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"fd_write\"
                              (func $w (param i32 i32 i32 i32) (result i32)))
                      (memory (export \"memory\") 1)
                      (func (export \"go\") (result i32)
                        (call $w (i32.const 1) (i32.const 0)
                                 (i32.const 4294967295) (i32.const 8))))"
                    (p1-context :stdout (make-string-output-stream)))
    (declare (ignore state))
    ;; count * 8 overflows the address space: preflighted in Lisp integers
    (is (= 21 (p1-call instance "go")))))

(test wasi-preview1/stale-fd-is-badf
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"fd_close\"
                              (func $c (param i32) (result i32)))
                      (import \"wasi_snapshot_preview1\" \"fd_write\"
                              (func $w (param i32 i32 i32 i32) (result i32)))
                      (memory (export \"memory\") 1)
                      (func (export \"go\") (result i32)
                        (drop (call $c (i32.const 1)))
                        ;; the slot is gone: BADF (8), not a host error
                        (call $w (i32.const 1) (i32.const 0)
                                 (i32.const 0) (i32.const 8))))"
                    (p1-context :stdout (make-string-output-stream)))
    (declare (ignore state))
    (is (= 8 (p1-call instance "go")))))

;;; ---------------------------------------------------------------
;;; args / environ: exact UTF-8 byte counts and NUL terminators

(test wasi-preview1/args-and-environ
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"args_sizes_get\"
                              (func $as (param i32 i32) (result i32)))
                      (import \"wasi_snapshot_preview1\" \"args_get\"
                              (func $ag (param i32 i32) (result i32)))
                      (import \"wasi_snapshot_preview1\" \"environ_sizes_get\"
                              (func $es (param i32 i32) (result i32)))
                      (import \"wasi_snapshot_preview1\" \"environ_get\"
                              (func $eg (param i32 i32) (result i32)))
                      (memory (export \"memory\") 1)
                      (func (export \"go\") (result i32)
                        (drop (call $as (i32.const 0) (i32.const 4)))
                        (drop (call $ag (i32.const 16) (i32.const 128)))
                        (drop (call $es (i32.const 8) (i32.const 12)))
                        (drop (call $eg (i32.const 32) (i32.const 256)))
                        (i32.const 0)))"
                    (p1-context :arguments (list "prog" "a"
                                                 (concatenate 'string "caf"
                                                              (string (code-char 233))))
                                :environment '(("K" . "V") ("EMPTY" . ""))))
    (is (= 0 (p1-call instance "go")))
    (is (= 3 (p1-u32 state 0)))
    ;; "prog\0a\0café\0" = 5 + 2 + 6 (é is 2 UTF-8 bytes)
    (is (= 13 (p1-u32 state 4)))
    (is (string= "prog" (p1-string state (p1-u32 state 16) 4)))
    (is (= 0 (aref (p1-bytes state) (+ (p1-u32 state 16) 4))))
    ;; environ: "K=V\0EMPTY=\0"
    (is (= 2 (p1-u32 state 8)))
    (is (= 11 (p1-u32 state 12)))
    (is (string= "K=V" (p1-string state (p1-u32 state 32) 3)))))

;;; ---------------------------------------------------------------
;;; process control: exit statuses and the signal limitation

(test wasi-preview1/proc-exit-status
  (dolist (status '(0 37 255 256 4294967295))
    (let* ((module (funcall (find-symbol "PARSE-WAT" '#:wasmdown.text)
                            (format nil "(module
                                           (import \"wasi_snapshot_preview1\" \"proc_exit\"
                                                   (func $e (param i32)))
                                           (memory (export \"memory\") 1)
                                           (func (export \"_start\")
                                             (call $e (i32.const ~d))))"
                                    status)))
           (ctx (p1-context :arguments '("x"))))
      (is (= status (wasmdown.wasi:run-preview1 module ctx))))))

(test wasi-preview1/proc-raise-notsup-never-signals
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"proc_raise\"
                              (func $r (param i32) (result i32)))
                      (memory (export \"memory\") 1)
                      (func (export \"valid\") (result i32)
                        (call $r (i32.const 2)))
                      (func (export \"invalid\") (result i32)
                        (call $r (i32.const 99))))"
                    (p1-context))
    (declare (ignore state))
    ;; a VALID signal is the named platform limitation: NOTSUP (58),
    ;; and the hosting SBCL process is untouched
    (is (= 58 (p1-call instance "valid")))
    (is (= 28 (p1-call instance "invalid")))
    ;; the image is still alive: a second guest runs normally
    (multiple-value-bind (instance2 state2)
        (p1-instance "(module
                        (memory (export \"memory\") 1)
                        (func (export \"go\") (result i32) (i32.const 7)))"
                      (p1-context))
      (declare (ignore state2))
      (is (= 7 (p1-call instance2 "go"))))))

;;; ---------------------------------------------------------------
;;; clocks, entropy, poll_oneoff

(test wasi-preview1/clock-and-random
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"clock_time_get\"
                              (func $t (param i32 i64 i32) (result i32)))
                      (import \"wasi_snapshot_preview1\" \"clock_res_get\"
                              (func $r (param i32 i32) (result i32)))
                      (import \"wasi_snapshot_preview1\" \"random_get\"
                              (func $rand (param i32 i32) (result i32)))
                      (memory (export \"memory\") 2)
                      (func (export \"go\") (result i32)
                        (i32.store (i32.const 0)
                          (call $t (i32.const 1) (i64.const 0) (i32.const 64)))
                        (i32.store (i32.const 4)
                          (call $t (i32.const 9) (i64.const 0) (i32.const 64)))
                        (i32.store (i32.const 8)
                          (call $r (i32.const 1) (i32.const 72)))
                        ;; fill 1500 bytes from the entropy source
                        (i32.store (i32.const 12)
                          (call $rand (i32.const 1024) (i32.const 1500)))
                        (i32.const 0)))"
                    (p1-context))
    (is (= 0 (p1-call instance "go")))
    (is (= 0 (p1-u32 state 0)))         ; monotonic now succeeds
    (is (= 28 (p1-u32 state 4)))        ; clockid 9 is invalid
    (is (plusp (p1-u64 state 72)))      ; a real resolution
    (is (= 0 (p1-u32 state 12)))
    ;; the full requested range was written: not all bytes stayed zero
    (let ((b (p1-bytes state)))
      (is (some (lambda (x) (plusp x)) (subseq b 1024 2524))))))

(test wasi-preview1/poll-oneoff-clock-event
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"poll_oneoff\"
                              (func $p (param i32 i32 i32 i32) (result i32)))
                      (memory (export \"memory\") 1)
                      (func (export \"go\") (result i32)
                        ;; subscription: userdata 0x1234, clock type,
                        ;; monotonic id 1, relative 1 ms, no precision
                        (i64.store (i32.const 0) (i64.const 4660))
                        (i32.store8 (i32.const 8) (i32.const 0))
                        (i32.store (i32.const 16) (i32.const 1))
                        (i64.store (i32.const 24) (i64.const 1000000))
                        (i64.store (i32.const 32) (i64.const 0))
                        (i32.store16 (i32.const 40) (i32.const 0))
                        (drop (call $p (i32.const 0) (i32.const 128)
                                       (i32.const 1) (i32.const 200)))
                        (drop (call $p (i32.const 0) (i32.const 128)
                                       (i32.const 0) (i32.const 204)))
                        (i32.const 0)))"
                    (p1-context))
    (is (= 0 (p1-call instance "go")))
    (is (= 1 (p1-u32 state 200)))       ; one event
    (is (= 4660 (p1-u64 state 128)))    ; userdata round trip
    (is (= 0 (aref (p1-bytes state) (+ 128 10)))) ; eventtype clock
    (is (= 0 (p1-u32 state 204))))
  ;; a zero-subscription poll is INVAL, and an unknown type too
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"poll_oneoff\"
                              (func $p (param i32 i32 i32 i32) (result i32)))
                      (memory (export \"memory\") 1)
                      (func (export \"zero\") (result i32)
                        (call $p (i32.const 0) (i32.const 128)
                                 (i32.const 0) (i32.const 200)))
                      (func (export \"badtype\") (result i32)
                        (i32.store8 (i32.const 8) (i32.const 9))
                        (call $p (i32.const 0) (i32.const 128)
                                 (i32.const 1) (i32.const 200))))"
                    (p1-context))
    (declare (ignore state))
    (is (= 28 (p1-call instance "zero")))
    (is (= 28 (p1-call instance "badtype")))))

;;; ---------------------------------------------------------------
;;; files: preopens, rights, confinement, readdir, prestat

(defparameter *open-and-read-wat*
  "(module
     (import \"wasi_snapshot_preview1\" \"path_open\"
             (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
     (import \"wasi_snapshot_preview1\" \"fd_read\"
             (func $read (param i32 i32 i32 i32) (result i32)))
     (import \"wasi_snapshot_preview1\" \"fd_close\"
             (func $close (param i32) (result i32)))
     (memory (export \"memory\") 1)
     (func (export \"open-file\") (param $path i32) (param $len i32) (result i32)
       ;; lookupflags 1 = symlink-follow: an escaping symlink must be
       ;; rejected by the CONFINED resolver, not merely by O_NOFOLLOW
       (call $open (i32.const 3) (i32.const 1) (local.get $path) (local.get $len)
                   (i32.const 0) (i64.const 2) (i64.const 0) (i32.const 0)
                   (i32.const 400)))
     (func (export \"open-write\") (param $path i32) (param $len i32) (result i32)
       (call $open (i32.const 3) (i32.const 1) (local.get $path) (local.get $len)
                   (i32.const 0) (i64.const 64) (i64.const 0) (i32.const 0)
                   (i32.const 400)))
     (func (export \"read-all\") (result i32)
       (i32.store (i32.const 16) (i32.const 500))
       (i32.store (i32.const 20) (i32.const 64))
       (call $read (i32.load (i32.const 400)) (i32.const 16) (i32.const 1)
                   (i32.const 24))))")

(test wasi-preview1/preopen-rights-and-confinement
  (with-temp-dir (dir)
    (write-text-file (merge-pathnames "a.txt" (pathname dir)) "hello-preopen")
    (ensure-directories-exist (merge-pathnames "sub/" (pathname dir)))
    (write-text-file (merge-pathnames "sub/b.txt" (pathname dir)) "nested")
    ;; a symlink whose target escapes the root
    (ignore-errors
      (sb-posix:symlink "/etc/passwd" (merge-pathnames "escape" (pathname dir))))
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir :name "/work"))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance *open-and-read-wat* ctx)
             ;; "a.txt" at 600, "escape" at 620, "sub/../a.txt" at 640
             (dotimes (i (length "a.txt"))
               (setf (aref (p1-bytes state) (+ 600 i))
                     (char-code (char "a.txt" i))))
             (dotimes (i (length "escape"))
               (setf (aref (p1-bytes state) (+ 620 i))
                     (char-code (char "escape" i))))
             (dotimes (i (length "sub/../a.txt"))
               (setf (aref (p1-bytes state) (+ 640 i))
                     (char-code (char "sub/../a.txt" i))))
             ;; a normal read-only open succeeds and reads the content
             (is (= 0 (p1-call instance "open-file" 600 5)))
             (is (= 0 (p1-call instance "read-all")))
             (is (= 13 (p1-u32 state 24)))
             (is (string= "hello-preopen" (p1-string state 500 13)))
             ;; internal `..' inside the root is allowed (version-neutral
             ;; confinement): sub/../a.txt resolves to a.txt
             (is (= 0 (p1-call instance "open-file" 640 12)))
             ;; an absolute path is NOTCAPABLE (76), never host-absolute
             (dotimes (i (length "/etc/passwd"))
               (setf (aref (p1-bytes state) (+ 700 i))
                     (char-code (char "/etc/passwd" i))))
             (is (= 76 (p1-call instance "open-file" 700 11)))
             ;; a symlink leaving the root is NOTCAPABLE, never /etc/passwd
             (is (= 76 (p1-call instance "open-file" 620 6))))
        (wasmdown.wasi:close-wasi-preopen preopen)))))

(test wasi-preview1/read-only-preopen-denies-write
  (with-temp-dir (dir)
    (write-text-file (merge-pathnames "ro.txt" (pathname dir)) "ro")
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir :name "/ro"
                                                     :permissions '("read")))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance *open-and-read-wat* ctx)
             (dotimes (i (length "ro.txt"))
               (setf (aref (p1-bytes state) (+ 600 i))
                     (char-code (char "ro.txt" i))))
             ;; reading works
             (is (= 0 (p1-call instance "open-file" 600 6)))
             ;; asking for WRITE rights exceeds the granted authority: the
             ;; parent's inheriting rights do not include fd_write
             (is (= 76 (p1-call instance "open-write" 600 6))))
        (wasmdown.wasi:close-wasi-preopen preopen)))))

(test wasi-preview1/readdir-and-prestat
  (with-temp-dir (dir)
    (write-text-file (merge-pathnames "one.txt" (pathname dir)) "1")
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir :name "/d"))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance "(module
                               (import \"wasi_snapshot_preview1\" \"fd_readdir\"
                                       (func $rd (param i32 i32 i32 i64 i32) (result i32)))
                               (import \"wasi_snapshot_preview1\" \"fd_prestat_get\"
                                       (func $pg (param i32 i32) (result i32)))
                               (import \"wasi_snapshot_preview1\" \"fd_prestat_dir_name\"
                                       (func $pn (param i32 i32 i32) (result i32)))
                               (memory (export \"memory\") 1)
                               (func (export \"prestat\") (result i32)
                                 (call $pg (i32.const 3) (i32.const 0)))
                               (func (export \"dirname\") (result i32)
                                 (call $pn (i32.const 3) (i32.const 32) (i32.const 32)))
                               (func (export \"list\") (result i32)
                                 (call $rd (i32.const 3) (i32.const 512)
                                           (i32.const 512) (i64.const 0)
                                           (i32.const 0))))"
                             ctx)
             (is (= 0 (p1-call instance "prestat")))
             (is (= 2 (p1-u32 state 4)))   ; "/d" is two bytes
             (is (= 0 (p1-call instance "dirname")))
             (is (string= "/d" (p1-string state 32 2)))
             ;; readdir: "." and ".." come first, then the file
             (is (= 0 (p1-call instance "list")))
             (let* ((used (p1-u32 state 0))
                    (names '()))
               (loop with off = 512
                     while (< off (+ 512 used))
                     do (let* ((namelen (p1-u32 state (+ off 16)))
                               (name (p1-string state (+ off 24) namelen)))
                          (push name names)
                          (incf off (+ 24 namelen))))
               (setf names (nreverse names))
               (is (equal '("." ".." "one.txt") names))))
        (wasmdown.wasi:close-wasi-preopen preopen)))))

;;; ---------------------------------------------------------------
;;; imported memory: the ABI gate must accept a module that imports
;;; its memory and does not re-export it

(test wasi-preview1/imported-memory
  (let* ((out (make-string-output-stream))
         (mem (wasmdown.runtime:make-memory-instance
               (funcall (find-symbol "MAKE-WASM-LIMITS" '#:wasmdown.types) 1)))
         (wat "(module
                 (import \"wasi_snapshot_preview1\" \"fd_write\"
                         (func $w (param i32 i32 i32 i32) (result i32)))
                 (import \"env\" \"memory\" (memory 1))
                 (data (i32.const 32) \"imported\\n\")
                 (func (export \"_start\")
                   (i32.store (i32.const 0) (i32.const 32))
                   (i32.store (i32.const 4) (i32.const 9))
                   (drop (call $w (i32.const 1) (i32.const 0)
                                 (i32.const 1) (i32.const 8)))))")
         (module (funcall (find-symbol "PARSE-WAT" '#:wasmdown.text) wat))
         (ctx (p1-context :arguments '("x") :stdout out)))
    (is (= 0 (wasmdown.wasi:run-preview1
              module ctx
              :imports (list (cons (list "env" "memory") mem)))))
    (is (string= (format nil "imported~%") (get-output-stream-string out)))))

;;; a module with NO memory at all is a link/instantiation failure
;;; BEFORE any guest side effect
(test wasi-preview1/missing-memory-rejected
  (let* ((wat "(module
                 (import \"wasi_snapshot_preview1\" \"fd_write\"
                         (func $w (param i32 i32 i32 i32) (result i32)))
                 (func (export \"_start\")
                   (drop (call $w (i32.const 1) (i32.const 0)
                                 (i32.const 0) (i32.const 8)))))")
         (module (funcall (find-symbol "PARSE-WAT" '#:wasmdown.text) wat))
         (ctx (p1-context :arguments '("x") :stdout (make-string-output-stream))))
    (is (handler-case (progn (wasmdown.wasi:run-preview1 module ctx) nil)
          (wasmdown:instantiation-error () t)))))

;;; ---------------------------------------------------------------
;;; descriptor accounting and off_t range validation

(defun fd-count ()
  (length (directory #p"/proc/self/fd/*")))

(test wasi-preview1/no-fd-leak-on-multi-component-paths
  (with-temp-dir (dir)
    (ensure-directories-exist (merge-pathnames "a/b/" (pathname dir)))
    (write-text-file (merge-pathnames "a/b/c.txt" (pathname dir)) "x")
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance "(module (memory (export \"memory\") 1))" ctx)
             (declare (ignore instance))
             ;; write "a/b/c.txt" into guest memory
             (let* ((path "a/b/c.txt")
                    (bytes (map '(vector (unsigned-byte 8)) #'char-code path)))
               (replace (p1-bytes state) bytes :start1 0)
               (let ((entry (gethash 3 (wasmdown.wasi::preview1-state-fds state))))
                 (flet ((resolve-once ()
                          (multiple-value-bind (dirfd leaf)
                              (wasmdown.wasi::%p1-resolve state entry 0 (length path) nil)
                            (is (string= "c.txt" leaf))
                            (funcall (find-symbol "%P1-LIBC-CLOSE" '#:wasmdown.wasi) dirfd))))
                   (resolve-once)       ; warm-up (allocates any caches)
                   (let ((before (fd-count)))
                     (dotimes (i 200) (resolve-once))
                     ;; two intermediate directories per lookup must not
                     ;; accumulate: allow only scheduler jitter
                     (is (<= (fd-count) (+ before 2))))))))
        (wasmdown.wasi:close-wasi-preopen preopen)))))

(test wasi-preview1/off-t-range-is-an-abi-error
  (with-temp-dir (dir)
    (write-text-file (merge-pathnames "f.txt" (pathname dir)) "0123456789")
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance
                "(module
                   (import \"wasi_snapshot_preview1\" \"path_open\"
                           (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
                   (import \"wasi_snapshot_preview1\" \"fd_pread\"
                           (func $pread (param i32 i32 i32 i64 i32) (result i32)))
                   (import \"wasi_snapshot_preview1\" \"fd_filestat_set_size\"
                           (func $trunc (param i32 i64) (result i32)))
                   (import \"wasi_snapshot_preview1\" \"fd_allocate\"
                           (func $alloc (param i32 i64 i64) (result i32)))
                   (memory (export \"memory\") 1)
                   (func (export \"open\") (result i32)
                     (call $open (i32.const 3) (i32.const 1) (i32.const 32)
                                 (i32.const 5) (i32.const 0) (i64.const 4194566)
                                 (i64.const 0) (i32.const 0) (i32.const 400)))
                   (func (export \"huge-pread\") (result i32)
                     (i32.store (i32.const 16) (i32.const 500))
                     (i32.store (i32.const 20) (i32.const 4))
                     (call $pread (i32.load (i32.const 400)) (i32.const 16)
                                  (i32.const 1) (i64.const 9223372036854775808)
                                  (i32.const 24)))
                   (func (export \"huge-trunc\") (result i32)
                     (call $trunc (i32.load (i32.const 400))
                                  (i64.const 18446744073709551615)))
                   (func (export \"huge-alloc\") (result i32)
                     (call $alloc (i32.load (i32.const 400))
                                  (i64.const 9223372036854775807)
                                  (i64.const 2))))"
                ctx)
             (dotimes (i (length "f.txt"))
               (setf (aref (p1-bytes state) (+ 32 i))
                     (char-code (char "f.txt" i))))
             (is= 0 (p1-call instance "open"))
             ;; offsets/sizes beyond the host off_t are ABI errors
             (is= 28 (p1-call instance "huge-pread"))     ; inval
             (is= 22 (p1-call instance "huge-trunc"))     ; fbig
             (is= 22 (p1-call instance "huge-alloc")))    ; fbig
        (wasmdown.wasi:close-wasi-preopen preopen)))))


;;; ---------------------------------------------------------------
;;; context lifecycle (step 2): close is idempotent, rejects new work,
;;; never closes borrowed authorities

(test wasi-preview1/context-close-rejects-new-work
  (let* ((dir (p1-temp-dir))
         (preopen (wasmdown.wasi:open-wasi-preopen dir))
         (ctx (wasmdown.wasi:make-wasi-context
               :preopens (list preopen)
               :stdout (make-string-output-stream))))
    (unwind-protect
         (progn
           ;; a live context can build its worker service
           (is (wasmdown.wasi:wasi-context-dns-executor ctx))
           (wasmdown.wasi:close-wasi-context ctx)
           (is (wasmdown.wasi:wasi-context-closed-p ctx))
           ;; idempotent
           (is (eq ctx (wasmdown.wasi:close-wasi-context ctx)))
           ;; new host work is refused, not silently queued
           (is (handler-case
                   (progn (wasmdown.wasi:wasi-context-dns-executor ctx) nil)
                 (wasmdown:host-error () t)))
           (is (handler-case
                   (progn (wasmdown.wasi:wasi-context-stdio-executor ctx) nil)
                 (wasmdown:host-error () t)))
           ;; BORROWED authorities survive: the preopen is the
           ;; embedder's handle, not the context's
           (is (wasmdown.wasi:wasi-preopen-p preopen))
           (is (not (minusp (wasmdown.wasi::wasi-preopen-fd preopen)))))
      (ignore-errors (wasmdown.wasi:close-wasi-preopen preopen))
      (ignore-errors (delete-directory dir)))))

(test wasi-preview1/call-with-context-closes-by-default
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (borrowed (wasmdown.wasi:make-wasi-context)))
    (wasmdown.wasi:call-with-wasi-context (ctx)
      (is (not (wasmdown.wasi:wasi-context-closed-p ctx))))
    (is (wasmdown.wasi:wasi-context-closed-p ctx))
    ;; CLOSE-P NIL leaves a caller-owned context open
    (wasmdown.wasi:call-with-wasi-context (borrowed :close-p nil)
      nil)
    (is (not (wasmdown.wasi:wasi-context-closed-p borrowed)))
    (wasmdown.wasi:close-wasi-context borrowed)))

;;; ---------------------------------------------------------------
;;; sockets: Preview1's socket calls are inherited-surface only. No
;;; socket is ever created or imported implicitly, and an fd that the
;;; embedder did not hand over is `BADF` — the ABI's own answer, not a
;;; link failure (a link error would be stricter than WITX).

(test wasi-preview1/socket-surface-is-explicit-only
  (multiple-value-bind (instance state)
      (p1-instance "(module
                      (import \"wasi_snapshot_preview1\" \"sock_recv\"
                              (func $recv (param i32 i32 i32 i32 i32 i32) (result i32)))
                      (import \"wasi_snapshot_preview1\" \"sock_send\"
                              (func $send (param i32 i32 i32 i32 i32) (result i32)))
                      (import \"wasi_snapshot_preview1\" \"sock_shutdown\"
                              (func $shut (param i32 i32) (result i32)))
                      (memory (export \"memory\") 1)
                      ;; no sockets table was supplied to the instance
                      (func (export \"recv-on-unknown\") (result i32)
                        (call $recv (i32.const 7) (i32.const 0) (i32.const 0)
                                    (i32.const 0) (i32.const 0) (i32.const 0)))
                      (func (export \"send-on-unknown\") (result i32)
                        (call $send (i32.const 7) (i32.const 0) (i32.const 0)
                                    (i32.const 0) (i32.const 0)))
                      (func (export \"shutdown-on-unknown\") (result i32)
                        (call $shut (i32.const 7) (i32.const 1))))"
                    (p1-context))
    (declare (ignore state))
    ;; BADF (8) for every socket call on an fd the embedder never gave
    (is= 8 (p1-call instance "recv-on-unknown"))
    (is= 8 (p1-call instance "send-on-unknown"))
    (is= 8 (p1-call instance "shutdown-on-unknown"))))

;;; ---------------------------------------------------------------
;;; Backend evidence: the host providers are the SAME for both backends,
;;; and the ABI memory is re-resolved per operation, so a guest that
;;; grows memory between two host calls is still correct.

(test wasi-preview1/memory-grow-between-host-calls
  ;; fd_write, memory.grow, then fd_write again from the NEW page: the
  ;; second call must see the grown memory (a cached byte vector would
  ;; answer FAULT for a span the guest legitimately owns).
  (dolist (mode '(:interpret :compiled))
    (let* ((out (make-string-output-stream))
           (ctx (p1-context :arguments '("x") :stdout out)))
      (multiple-value-bind (instance state)
          (p1-instance "(module
                          (import \"wasi_snapshot_preview1\" \"fd_write\"
                                  (func $w (param i32 i32 i32 i32) (result i32)))
                          (memory (export \"memory\") 1 4)
                          (data (i32.const 64) \"a\\n\")
                          (func (export \"go\") (result i32)
                            (i32.store (i32.const 0) (i32.const 64))
                            (i32.store (i32.const 4) (i32.const 2))
                            (drop (call $w (i32.const 1) (i32.const 0)
                                          (i32.const 1) (i32.const 8)))
                            (drop (memory.grow (i32.const 1)))
                            (i32.store8 (i32.const 65536) (i32.const 98))
                            (i32.store8 (i32.const 65537) (i32.const 10))
                            (i32.store (i32.const 0) (i32.const 65536))
                            (call $w (i32.const 1) (i32.const 0)
                                     (i32.const 1) (i32.const 8))))"
                        ctx :mode mode)
        (declare (ignore state))
        (is= 0 (p1-call instance "go"))
        (is (string= (format nil "a~%b~%") (get-output-stream-string out)))))))

(defun p1-parity-probe (mode)
  "Run the start-section/`_start' host-call probe in MODE against a
fresh stdout stream. Returns (values STDOUT ERRNO-RESULTS
BACKEND-ENTRY-COUNTS), with the counters reset before the run."
  (wasmdown.runtime:reset-backend-entry-counts)
  (let* ((out (make-string-output-stream))
         (ctx (p1-context :arguments '("x") :stdout out)))
    (multiple-value-bind (instance state)
        (p1-instance "(module
                        (import \"wasi_snapshot_preview1\" \"fd_write\"
                                (func $w (param i32 i32 i32 i32) (result i32)))
                        (memory (export \"memory\") 1)
                        (data (i32.const 64) \"start\\n\")
                        (data (i32.const 72) \"main\\n\")
                        (func $emit (param i32 i32) (result i32)
                          (i32.store (i32.const 0) (local.get 0))
                          (i32.store (i32.const 4) (local.get 1))
                          (call $w (i32.const 1) (i32.const 0)
                                   (i32.const 1) (i32.const 8)))
                        (func $start (drop (call $emit (i32.const 64) (i32.const 6))))
                        (start $start)
                        (func (export \"_start\")
                          (drop (call $emit (i32.const 72) (i32.const 5))))
                        (func (export \"bad-fd\") (result i32)
                          (i32.store (i32.const 0) (i32.const 64))
                          (i32.store (i32.const 4) (i32.const 2))
                          (call $w (i32.const 77) (i32.const 0)
                                   (i32.const 1) (i32.const 8)))
                        (func (export \"bad-iovec\") (result i32)
                          (call $w (i32.const 1) (i32.const 4294967040)
                                   (i32.const 1) (i32.const 8))))"
                      ctx :mode mode)
      (declare (ignore state))
      ;; the START section already ran during instantiation, through host
      ;; calls that read the iovec/data written by guest code
      (let ((start-bytes (get-output-stream-string out)))
        (p1-call instance "_start")
        (values (concatenate 'string start-bytes (get-output-stream-string out))
                (list (p1-call instance "bad-fd")
                      (p1-call instance "bad-iovec"))
                (wasmdown.runtime:backend-entry-counts))))))

(test wasi-preview1/backend-parity-start-section-and-host-memory
  (multiple-value-bind (i-out i-errnos i-counts)
      (p1-parity-probe :interpret)
    (multiple-value-bind (b-out b-errnos b-counts)
        (p1-parity-probe :compiled)
      ;; identical guest-visible results from the same host providers
      (is (string= (format nil "start~%main~%") i-out))
      (is (string= i-out b-out))
      (is (equal '(8 21) i-errnos))          ; badf, fault
      (is (equal i-errnos b-errnos))
      ;; the counters prove WHICH backend executed: the interpreted lane
      ;; never enters a compiled entry, and the compiled lane runs the
      ;; START SECTION and `_start' as compiled code with zero
      ;; interpreted entries (no silent fallback).
      (is= 0 (cdr (assoc :compiled i-counts)))
      (is (plusp (cdr (assoc :interpret i-counts))))
      (is= 0 (cdr (assoc :interpret b-counts)))
      (is (>= (cdr (assoc :compiled b-counts)) 2)))))

;;; ---------------------------------------------------------------
;;; host-resource accounting: handles are released on success, trap,
;;; exit and on every rejected call

(test wasi-preview1/dir-handle-released-on-rejected-path
  ;; %P1-RESOLVE duplicates the preopen before it walks the path. A
  ;; rejected path (absolute, or `..' escaping the root) must not leak
  ;; that duplicate.
  (with-temp-dir (dir)
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance "(module
                               (import \"wasi_snapshot_preview1\" \"path_open\"
                                       (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32)
                                             (result i32)))
                               (memory (export \"memory\") 1)
                               (data (i32.const 32) \"/etc/passwd\")
                               (data (i32.const 48) \"../../etc/passwd\")
                               ;; a NUL never appears in a path
                               (data (i32.const 80) \"a.txt\\00\")
                               (func $try (param i32 i32) (result i32)
                                 (call $open (i32.const 3) (i32.const 1)
                                             (local.get 0) (local.get 1)
                                             (i32.const 0) (i64.const 0) (i64.const 0)
                                             (i32.const 0) (i32.const 200)))
                               (func (export \"absolute\") (result i32)
                                 (call $try (i32.const 32) (i32.const 11)))
                               (func (export \"escape\") (result i32)
                                 (call $try (i32.const 48) (i32.const 16)))
                               (func (export \"nul\") (result i32)
                                 (call $try (i32.const 80) (i32.const 6))))"
                             ctx)
             (let ((slots (hash-table-count
                           (wasmdown.wasi::preview1-state-fds state))))
               (is= 76 (p1-call instance "absolute")) ; notcapable
               (is= 76 (p1-call instance "escape"))
               (is= 25 (p1-call instance "nul"))
               (let ((before (fd-count)))
                 (dotimes (i 100)
                   (is= 76 (p1-call instance "absolute"))
                   (is= 76 (p1-call instance "escape")))
                 (is (<= (fd-count) (+ before 2))))
               (is= slots (hash-table-count
                           (wasmdown.wasi::preview1-state-fds state)))))
        (wasmdown.wasi:close-wasi-preopen preopen)))))

(test wasi-preview1/path-open-rejected-out-pointer-releases-the-fd
  (with-temp-dir (dir)
    (write-text-file (merge-pathnames "f.txt" (pathname dir)) "0123")
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance "(module
                               (import \"wasi_snapshot_preview1\" \"path_open\"
                                       (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32)
                                             (result i32)))
                               (memory (export \"memory\") 1)
                               (data (i32.const 32) \"f.txt\")
                               (func $try (param i32) (result i32)
                                 (call $open (i32.const 3) (i32.const 1)
                                             (i32.const 32) (i32.const 5)
                                             (i32.const 0) (i64.const 0) (i64.const 0)
                                             (i32.const 0) (local.get 0)))
                               (func (export \"bad-out\") (result i32)
                                 (call $try (i32.const 4294967295)))
                               (func (export \"good\") (result i32)
                                 (call $try (i32.const 200))))"
                             ctx)
             (let* ((fds (wasmdown.wasi::preview1-state-fds state))
                    (slots (hash-table-count fds))
                    (before (fd-count)))
               ;; the out pointer is preflighted: FAULT, no host open, no
               ;; ABI slot installed
               (is= 21 (p1-call instance "bad-out"))
               (is= slots (hash-table-count fds))
               (dotimes (i 50) (is= 21 (p1-call instance "bad-out")))
               (is (<= (fd-count) (+ before 2)))
               ;; the same path still opens normally
               (is= 0 (p1-call instance "good"))
               (is (gethash (p1-u32 state 200) fds))
               (is= (1+ slots) (hash-table-count fds)))
        (wasmdown.wasi:close-wasi-preopen preopen))))))

(test wasi-preview1/path-readlink-preflights-its-buffer
  (with-temp-dir (dir)
    (write-text-file (merge-pathnames "f.txt" (pathname dir)) "x")
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance "(module
                               (import \"wasi_snapshot_preview1\" \"path_readlink\"
                                       (func $rl (param i32 i32 i32 i32 i32 i32) (result i32)))
                               (memory (export \"memory\") 1)
                               (data (i32.const 32) \"f.txt\")
                               (func (export \"huge\") (result i32)
                                 ;; buffer and length both outside the memory
                                 (call $rl (i32.const 3) (i32.const 32) (i32.const 5)
                                           (i32.const 4294901760) (i32.const 4294967295)
                                           (i32.const 0))))"
                             ctx)
             (declare (ignore state))
             (is= 21 (p1-call instance "huge"))) ; fault, before any read
        (wasmdown.wasi:close-wasi-preopen preopen)))))

(defun p1-open-then (tail &key exit-import-p)
  "A command module that path_opens \"f.txt\" through the preopen at fd
3 and then runs TAIL (a WAT instruction string)."
  (format nil "(module
                 (import \"wasi_snapshot_preview1\" \"path_open\"
                         (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
                 ~a
                 (memory (export \"memory\") 1)
                 (data (i32.const 32) \"f.txt\")
                 (func (export \"_start\")
                   (drop (call $open (i32.const 3) (i32.const 1) (i32.const 32)
                                     (i32.const 5) (i32.const 0) (i64.const 0)
                                     (i64.const 0) (i32.const 0) (i32.const 200)))
                   ~a))"
          (if exit-import-p
              "(import \"wasi_snapshot_preview1\" \"proc_exit\" (func $exit (param i32)))"
              "")
          tail))

(test wasi-preview1/descriptors-released-on-trap-and-exit
  (with-temp-dir (dir)
    (write-text-file (merge-pathnames "f.txt" (pathname dir)) "0123")
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir))
           (ctx (p1-context :preopens (list preopen)
                            :arguments '("x")
                            :stdout (make-string-output-stream))))
      (unwind-protect
           (dolist (mode '(:interpret :compiled))
             (flet ((run (wat)
                      (wasmdown.wasi:run-preview1
                       (funcall (find-symbol "PARSE-WAT" '#:wasmdown.text) wat)
                       ctx :mode mode)))
               ;; TRAP after the open: the descriptor is still released
               (let ((before (fd-count)))
                 (is (handler-case (progn (run (p1-open-then "(unreachable)")) nil)
                       (wasmdown:wasm-trap () t)))
                 (is (<= (fd-count) (+ before 2))))
               ;; proc_exit after the open: a distinct path, never a trap,
               ;; and the descriptor is released by the owning runner
               (let ((before (fd-count)))
                 (is= 7 (run (p1-open-then "(call $exit (i32.const 7))"
                                         :exit-import-p t)))
                 (is (<= (fd-count) (+ before 2))))
               ;; the same path opens again: no handle or ABI slot survived
               (is= 0 (run (p1-open-then "" :exit-import-p t)))))
        (wasmdown.wasi:close-wasi-preopen preopen)))))

;;; ---------------------------------------------------------------
;;; Directory authority and handle-relative resolution (upstream
;;; wasi-testsuite regressions: path_open_dirfd_not_dir family,
;;; directory_seek, fdopendir-with-access, fd_readdir, poll_oneoff).

(test wasi-preview1/preopen-directory-rights-are-split
  ;; A preopen's BASE rights describe the directory handle (no byte
  ;; stream rights); INHERITING is what a path_open below it may ask
  ;; for. wasi-libc derives the host access mode from the requested
  ;; rights, so advertising FD_WRITE in base made every `open(dir)'
  ;; an O_RDWR directory open: EISDIR.
  (with-temp-dir (dir)
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir :name "/"))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance "(module
                               (import \"wasi_snapshot_preview1\" \"fd_fdstat_get\"
                                       (func $fs (param i32 i32) (result i32)))
                               (import \"wasi_snapshot_preview1\" \"path_open\"
                                       (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32)
                                             (result i32)))
                               (memory (export \"memory\") 1)
                               (data (i32.const 64) \".\")
                               (func (export \"fdstat\") (result i32)
                                 (call $fs (i32.const 3) (i32.const 0)))
                               ;; open \".\" with the preopen's own rights
                               (func (export \"open-base\") (result i32)
                                 (i32.store (i32.const 240)
                                            (call $fs (i32.const 3) (i32.const 0)))
                                 (call $open (i32.const 3) (i32.const 0)
                                             (i32.const 64) (i32.const 1)
                                             (i32.const 2)
                                             (i64.load (i32.const 8))
                                             (i64.load (i32.const 16))
                                             (i32.const 0) (i32.const 200)))
                               ;; read|write on a directory is EISDIR (31)
                               (func (export \"open-rw\") (result i32)
                                 (call $open (i32.const 3) (i32.const 0)
                                             (i32.const 64) (i32.const 1)
                                             (i32.const 2) (i64.const 66) (i64.const 0)
                                             (i32.const 0) (i32.const 200))))"
                             ctx)
             (is= 0 (p1-call instance "fdstat"))
             (let ((base (p1-u64 state 8))
                   (inh (p1-u64 state 16)))
               ;; no byte-stream rights on the directory handle itself
               (is= 0 (logand 64 base))  ; fd_write
               (is= 0 (logand 2 base))   ; fd_read
               (is= 0 (logand 4 base))   ; fd_seek
               ;; but the descendants may request them
               (is= 64 (logand 64 inh))
               (is= 2 (logand 2 inh)))
             (is= 0 (p1-call instance "open-base"))
             (is= 3 (p1-u32 state 0)) ; fdstat filetype directory
             ;; the opened directory carries no fd_seek right
             (is= 0 (logand 4 (p1-u64 state 8)))
             (is= 31 (p1-call instance "open-rw")))
        (wasmdown.wasi:close-wasi-preopen preopen)))))

(test wasi-preview1/path-lookup-is-handle-relative
  ;; path_filestat_get(subdir_fd, "a.txt") must name subdir/a.txt, not
  ;; <preopen>/a.txt (the fdopendir-with-access regression).
  (with-temp-dir (dir)
    (write-text-file (merge-pathnames "a.txt" (pathname dir)) "root")
    (ensure-directories-exist (merge-pathnames "sub/" (pathname dir)))
    (write-text-file (merge-pathnames "sub/a.txt" (pathname dir)) "nested!")
    (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir :name "/"))
           (ctx (p1-context :preopens (list preopen))))
      (unwind-protect
           (multiple-value-bind (instance state)
               (p1-instance "(module
                               (import \"wasi_snapshot_preview1\" \"path_open\"
                                       (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32)
                                             (result i32)))
                               (import \"wasi_snapshot_preview1\" \"path_filestat_get\"
                                       (func $pfs (param i32 i32 i32 i32 i32) (result i32)))
                               (memory (export \"memory\") 1)
                               (data (i32.const 32) \"sub\")
                               (data (i32.const 40) \"a.txt\")
                               (func (export \"stat-through-sub\") (result i32)
                                 ;; rights: path_open | path_filestat_get
                                 (i32.store (i32.const 240)
                                            (call $open (i32.const 3) (i32.const 0)
                                                        (i32.const 32) (i32.const 3)
                                                        (i32.const 2) (i64.const 270336)
                                                        (i64.const 0) (i32.const 0)
                                                        (i32.const 240)))
                                 (call $pfs (i32.load (i32.const 240)) (i32.const 0)
                                            (i32.const 40) (i32.const 5)
                                            (i32.const 300))))"
                             ctx)
             (is= 0 (p1-call instance "stat-through-sub"))
             ;; size 7 from sub/a.txt, and the open directory fd is
             ;; itself statable
             (is= 7 (p1-u64 state (+ 300 32))))
        (wasmdown.wasi:close-wasi-preopen preopen)))))

  (let* ((ctx (p1-context :stdin (make-string-input-stream "x"))))
    (multiple-value-bind (instance state)
        (p1-instance
         "(module
            (import \"wasi_snapshot_preview1\" \"poll_oneoff\"
                    (func $p (param i32 i32 i32 i32) (result i32)))
            (memory (export \"memory\") 1)
            (func (export \"go\") (result i32)
              (i64.store (i32.const 0) (i64.const 0x1111))
              (i32.store8 (i32.const 8) (i32.const 0))
              (i32.store (i32.const 16) (i32.const 1))
              (i64.store (i32.const 24) (i64.const 10000000000))
              (i64.store (i32.const 32) (i64.const 0))
              (i32.store16 (i32.const 40) (i32.const 0))
              (i64.store (i32.const 48) (i64.const 0x2222))
              (i32.store8 (i32.const 56) (i32.const 1))
              (i32.store (i32.const 64) (i32.const 0))
              (call $p (i32.const 0) (i32.const 256)
                       (i32.const 2) (i32.const 300))))"
         ctx)
      (is= 0 (p1-call instance "go"))
      (is= 1 (p1-u32 state 300))            ; only stdin is ready
      (is= #x2222 (p1-u64 state 256))       ; its userdata
      (is= 1 (aref (p1-bytes state) (+ 256 10)))))   ; fd_read event type

(test wasi-preview1/renumber-requires-a-destination
  ;; fd_renumber may only target an EXISTING descriptor.
  (with-temp-dir (dir)
   (let* ((preopen (wasmdown.wasi:open-wasi-preopen dir :name "/"))
          (ctx (p1-context :preopens (list preopen)))
         (instance (p1-instance
                    "(module
                       (import \"wasi_snapshot_preview1\" \"fd_renumber\"
                               (func $r (param i32 i32) (result i32)))
                       (memory (export \"memory\") 1)
                       (func (export \"bad-dest\") (result i32)
                         (call $r (i32.const 3) (i32.const 25)))
                       (func (export \"self\") (result i32)
                         (call $r (i32.const 3) (i32.const 3))))"
                    ctx)))
     (is= 8 (p1-call instance "bad-dest"))   ; badf
     (is= 0 (p1-call instance "self"))
     (wasmdown.wasi:close-wasi-preopen preopen))))
