;;;; WASMDOWN — WASI Preview1 (`wasi_snapshot_preview1`) host ABI.
;;;;
;;;; Preview1 is a LINEAR-MEMORY ABI: every pointer is a 32-bit guest
;;;; address, every call returns a single `errno`, and the descriptor
;;;; table is a flat `fd -> capability` map. It has nothing in common
;;;; with the component-model resource ABI used by WASI 0.2/0.3, so it
;;;; lives in its OWN facade over the same native authorities:
;;;;
;;;;   * capabilities come from an explicit wasi-context (docs/design/PLAN.md §9.1);
;;;;     a P1 embedder never reads an ambient global;
;;;;   * preopens are host DIRECTORY HANDLES resolved per component
;;;;     (handle-relative, `..`-confined) — never path prefixes;
;;;;   * entropy, clocks, descriptors, sockets and the reactor are the
;;;;     same native services the component providers use;
;;;;   * `proc_raise` NEVER signals the hosting SBCL process: a valid
;;;;     discriminant returns `errno::notsup` (the platform limitation
;;;;     documented in specs/wasi/preview1/INVENTORY.md and matched by
;;;;     the pinned reference), an invalid one `errno::inval`;
;;;;   * `proc_exit` raises `wasi-exit`, caught only by the owning
;;;;     runner — it never calls `exit(3)`.
;;;;
;;;; The ABI inventory (46 exports) is generated from the pinned WITX by
;;;; tools/wasi-inventory.py; this file implements every entry.

(in-package #:wasmdown.wasi)

;;; ---------------------------------------------------------------
;;; errno (typenames.witx order — the ABI values are indices).

(macrolet ((def-errnos (&rest names)
             (let ((forms
                     (loop for name in names
                           for i from 0
                           collect `(defconstant ,(intern (format nil "+P1-~a+"
                                                                  (string-upcase (symbol-name name))))
                                       ,i))))
               `(progn ,@forms))))
  (def-errnos success 2big acces addrinuse addrnotavail afnosupport again
    already badf badmsg busy canceled child connaborted connrefused
    connreset deadlk destaddrreq dom dquot exist fault fbig hostunreach idrm
    ilseq inprogress intr inval io isconn isdir loop mfile mlink msgsize
    multihop nametoolong netdown netreset netunreach nfile nobufs nodev noent
    noexec nolck nolink nomem nomsg noprotoopt nospc nosys notconn notdir
    notempty notrecoverable notsock notsup notty nxio overflow ownerdead perm
    pipe proto protonosupport prototype range rofs spipe srch stale timedout
    txtbsy xdev notcapable))

;;; rights (u64 bit set, declaration order).
(defconstant +p1-right-fd-datasync+       (ash 1 0))
(defconstant +p1-right-fd-read+           (ash 1 1))
(defconstant +p1-right-fd-seek+           (ash 1 2))
(defconstant +p1-right-fd-fdstat-set-flags+ (ash 1 3))
(defconstant +p1-right-fd-sync+           (ash 1 4))
(defconstant +p1-right-fd-tell+           (ash 1 5))
(defconstant +p1-right-fd-write+          (ash 1 6))
(defconstant +p1-right-fd-advise+         (ash 1 7))
(defconstant +p1-right-fd-allocate+       (ash 1 8))
(defconstant +p1-right-path-create-directory+ (ash 1 9))
(defconstant +p1-right-path-create-file+  (ash 1 10))
(defconstant +p1-right-path-link-source+  (ash 1 11))
(defconstant +p1-right-path-link-target+  (ash 1 12))
(defconstant +p1-right-path-open+         (ash 1 13))
(defconstant +p1-right-fd-readdir+        (ash 1 14))
(defconstant +p1-right-path-readlink+     (ash 1 15))
(defconstant +p1-right-path-rename-source+ (ash 1 16))
(defconstant +p1-right-path-rename-target+ (ash 1 17))
(defconstant +p1-right-path-filestat-get+ (ash 1 18))
(defconstant +p1-right-path-filestat-set-size+ (ash 1 19))
(defconstant +p1-right-path-filestat-set-times+ (ash 1 20))
(defconstant +p1-right-fd-filestat-get+   (ash 1 21))
(defconstant +p1-right-fd-filestat-set-size+ (ash 1 22))
(defconstant +p1-right-fd-filestat-set-times+ (ash 1 23))
(defconstant +p1-right-path-symlink+      (ash 1 24))
(defconstant +p1-right-path-remove-directory+ (ash 1 25))
(defconstant +p1-right-path-unlink-file+  (ash 1 26))
(defconstant +p1-right-poll-fd-readwrite+ (ash 1 27))
(defconstant +p1-right-sock-shutdown+     (ash 1 28))
(defconstant +p1-right-sock-accept+       (ash 1 29))

(defconstant +p1-rights-all+
  (1- (ash 1 30))
  "Every right the ABI defines (the authority a fully writable preopen
grants to descendants; the preopen's OWN base rights are only the
directory rights below).")

;;; A directory handle is not a byte stream: fd_read/fd_write/fd_seek/
;;; fd_tell and the file-size rights do not apply to it. The WASI
;;; convention (shared with wasmtime) separates the two sets: a
;;; directory's BASE rights are the lookup/listing/metadata rights it
;;; may use on itself, while its INHERITING rights are the authority a
;;; path_open below it may request. A preopen with only `read' must not
;;; advertise write authority, or userlands that derive the host access
;;; mode from the requested rights (wasi-libc requests
;;; `max & inheriting') would open directories read-write, which POSIX
;;; rejects with EISDIR.

(defconstant +p1-rights-dir-read-only+
  (logior +p1-right-fd-readdir+
          +p1-right-path-open+
          +p1-right-path-readlink+
          +p1-right-path-filestat-get+
          +p1-right-fd-filestat-get+
          +p1-right-fd-filestat-set-times+)
  "Directory base rights for a read-only preopen.")

(defconstant +p1-rights-dir-write+
  (logior +p1-rights-dir-read-only+
          +p1-right-path-create-directory+
          +p1-right-path-create-file+
          +p1-right-path-link-source+
          +p1-right-path-link-target+
          +p1-right-path-rename-source+
          +p1-right-path-rename-target+
          +p1-right-path-symlink+
          +p1-right-path-remove-directory+
          +p1-right-path-unlink-file+
          +p1-right-path-filestat-set-size+
          +p1-right-path-filestat-set-times+)
  "Directory base rights for a writable, directory-mutating preopen.")

(defconstant +p1-rights-file-read-only+
  (logior +p1-right-fd-read+ +p1-right-fd-seek+ +p1-right-fd-tell+
          +p1-right-fd-fdstat-set-flags+ +p1-right-fd-advise+
          +p1-right-fd-filestat-get+ +p1-right-poll-fd-readwrite+)
  "File rights a read-only preopen's descendants may request.")

(defconstant +p1-rights-file-write+
  (logior +p1-rights-file-read-only+
          +p1-right-fd-datasync+ +p1-right-fd-sync+ +p1-right-fd-write+
          +p1-right-fd-allocate+ +p1-right-fd-filestat-set-size+
          +p1-right-fd-filestat-set-times+)
  "File rights a writable preopen's descendants may request.")

(defconstant +p1-rights-not-on-directory+
  (logior +p1-right-fd-seek+ +p1-right-fd-tell+
          +p1-right-fd-allocate+ +p1-right-fd-filestat-set-size+)
  "Rights that a directory descriptor never carries: the host rejects
the corresponding operations on a directory (EISDIR/EINVAL), so
granting them would misdescribe the handle.")

;;; fdflags (u16).
(defconstant +p1-fdflag-append+   1)
(defconstant +p1-fdflag-dsync+    2)
(defconstant +p1-fdflag-nonblock+ 4)
(defconstant +p1-fdflag-rsync+    8)
(defconstant +p1-fdflag-sync+     16)
(defconstant +p1-fdflag-all+ 31)

;;; oflags (u16).
(defconstant +p1-oflag-creat+     1)
(defconstant +p1-oflag-directory+ 2)
(defconstant +p1-oflag-excl+      4)
(defconstant +p1-oflag-trunc+     8)

;;; lookupflags (u32).
(defconstant +p1-lookup-symlink-follow+ 1)

;;; fstflags (u16).
(defconstant +p1-fst-atim+     1)
(defconstant +p1-fst-atim-now+ 2)
(defconstant +p1-fst-mtim+     4)
(defconstant +p1-fst-mtim-now+ 8)

;;; eventtype (u8) and related.
(defconstant +p1-event-clock+    0)
(defconstant +p1-event-fd-read+  1)
(defconstant +p1-event-fd-write+ 2)
(defconstant +p1-eventrw-hangup+ 1)
(defconstant +p1-roflag-data-truncated+ 1)
(defconstant +p1-riflag-peek+     1)
(defconstant +p1-riflag-waitall+  2)
(defconstant +p1-sdflag-rd+ 1)
(defconstant +p1-sdflag-wr+ 2)

;;; filetype (u8).
(defconstant +p1-file-unknown+ 0)
(defconstant +p1-file-block-device+ 1)
(defconstant +p1-file-character-device+ 2)
(defconstant +p1-file-directory+ 3)
(defconstant +p1-file-regular-file+ 4)
(defconstant +p1-file-socket-dgram+ 5)
(defconstant +p1-file-socket-stream+ 6)
(defconstant +p1-file-symbolic-link+ 7)

;;; clockid (u32).
(defconstant +p1-clock-realtime+ 0)
(defconstant +p1-clock-monotonic+ 1)
(defconstant +p1-clock-process-cputime+ 2)
(defconstant +p1-clock-thread-cputime+ 3)

;;; signal: 31 discriminants (none .. sys).
(defconstant +p1-signal-max+ 30)

;;; WITX record sizes (byte, ABI layout).
(defconstant +p1-size-iovec+ 8)
(defconstant +p1-size-dirent+ 24)
(defconstant +p1-size-fdstat+ 24)
(defconstant +p1-size-filestat+ 64)
(defconstant +p1-size-prestat+ 8)
(defconstant +p1-size-event+ 32)
;; A subscription is userdata (8) + the variant at offset 8: its tag
;; slot is 8 bytes (the payload is 8-aligned) and the largest payload
;; (clock: id u32, timeout u64, precision u64, flags u16) is 32 bytes
;; after alignment, so 8 + 8 + 32 = 48. Using 56 walked the second
;; subscription into the middle of its own tag/payload.
(defconstant +p1-size-subscription+ 48)

;;; ---------------------------------------------------------------
;;; ABI abort: a host-side rejection that becomes an `errno` result.

(define-condition p1-abort (condition)
  ((errno :initarg :errno :reader p1-abort-errno)))

(defun %p1-abort (errno)
  (error 'p1-abort :errno errno))

;;; ---------------------------------------------------------------
;;; state

(defstruct (p1-fd (:constructor %make-p1-fd))
  "One Preview1 descriptor slot: the host handle plus the PREVIEW1
rights/flags/position state. P1 fds are an ABI table of their own —
they never alias the component runtime's resource handles."
  (fd -1 :type fixnum)                 ; host fd, or -1 (stream slots)
  (kind :file :type keyword)           ; :file | :stream | :socket
  (stream nil)                         ; CL stream for the stdio slots
  (preopen nil)                        ; host preopen fd (directory) or NIL
  (name "")                            ; guest-visible preopen name
  (rights-base 0 :type (unsigned-byte 64))
  (rights-inheriting 0 :type (unsigned-byte 64))
  (flags 0 :type (unsigned-byte 16))
  (dir-p nil)
  (owned-p t)
  (filetype +p1-file-unknown+ :type (unsigned-byte 8))
  (position 0 :type (unsigned-byte 64)) ; stream slots have no host offset
  (entries nil)                        ; cached readdir entries
  (entries-cookie 0 :type (unsigned-byte 64)))

(defstruct (preview1-state (:constructor %make-preview1-state))
  (context nil)
  (module nil)
  (instance nil)
  (memory nil)                         ; the memory-instance
  (fds (make-hash-table :test #'eql))
  (sockets nil)
  (closed-p nil)
  (initialized-p nil))

(defvar *p1-current-state* nil
  "The state whose import closures are executing on this thread. Set
by the import trampolines; used only for diagnostics and by
`proc_exit`'s owner check.")

;;; ---------------------------------------------------------------
;;; Guest memory access. Every access is bounds-checked in Lisp
;;; integers BEFORE any host effect, and the backing vector is
;;; re-read on each access so a guest `memory.grow` between calls can
;;; never leave a stale span.

(defun %p1-bytes (state)
  (let ((mem (preview1-state-memory state)))
    (or (and mem (wasmdown.runtime::memory-instance-bytes mem))
        (%p1-abort +p1-fault+))))

(defun %p1-check-range (state addr len)
  "Validate [ADDR, ADDR+LEN) in guest memory and return ADDR as an
integer. Overflow of the 32-bit address space is `fault`."
  (let ((n (length (%p1-bytes state))))
    (unless (and (integerp addr) (integerp len)
                 (<= 0 addr) (<= 0 len)
                 (<= (+ addr len) n))
      (%p1-abort +p1-fault+))
    addr))

(defun %p1-u8 (state addr)
  (aref (%p1-bytes state) (%p1-check-range state addr 1)))

(defun %p1-u16 (state addr)
  (%p1-check-range state addr 2)
  (let ((b (%p1-bytes state)))
    (logior (aref b addr) (ash (aref b (+ addr 1)) 8))))

(defun %p1-u32 (state addr)
  (%p1-check-range state addr 4)
  (let ((b (%p1-bytes state)))
    (logior (aref b addr)
            (ash (aref b (+ addr 1)) 8)
            (ash (aref b (+ addr 2)) 16)
            (ash (aref b (+ addr 3)) 24))))

(defun %p1-u64 (state addr)
  (%p1-check-range state addr 8)
  (let ((b (%p1-bytes state)))
    (logior (%p1-u32 state addr)
            (ash (%p1-u32 state (+ addr 4)) 32))))

(defun %p1-s64 (state addr)
  "A signed 64-bit field (WITX `s64'): two's complement from the
unsigned representation."
  (let ((v (%p1-u64 state addr)))
    (if (logbitp 63 v) (- v (ash 1 64)) v)))

(defun (setf %p1-u8) (value state addr)
  (aref (%p1-bytes state) (%p1-check-range state addr 1))
  (setf (aref (%p1-bytes state) addr) (logand value #xff))
  value)

(defun (setf %p1-u16) (value state addr)
  (%p1-check-range state addr 2)
  (let ((b (%p1-bytes state))
        (v (logand value #xffff)))
    (setf (aref b addr) (logand v #xff)
          (aref b (+ addr 1)) (logand (ash v -8) #xff))
    v))

(defun (setf %p1-u32) (value state addr)
  (%p1-check-range state addr 4)
  (let ((b (%p1-bytes state))
        (v (logand value #xffffffff)))
    (loop for i from 0 below 4
          do (setf (aref b (+ addr i)) (logand (ash v (* -8 i)) #xff)))
    v))

(defun (setf %p1-u64) (value state addr)
  (%p1-check-range state addr 8)
  (let ((v (logand value (1- (ash 1 64)))))
    (setf (%p1-u32 state addr) (logand v #xffffffff)
          (%p1-u32 state (+ addr 4)) (ash v -32))
    v))

(defun %p1-read-bytes (state addr len)
  "A fresh octet vector copy of the guest range (snapshot semantics:
input descriptors are copied BEFORE any aliased output is written)."
  (%p1-check-range state addr len)
  (let ((b (%p1-bytes state))
        (copy (make-array len :element-type '(unsigned-byte 8))))
    (replace copy b :start2 addr :end2 (+ addr len))
    copy))

(defun %p1-write-bytes (state addr bytes)
  (%p1-check-range state addr (length bytes))
  (replace (%p1-bytes state) bytes :start1 addr)
  bytes)

(defun %p1-string (state addr len)
  "Read a WITX `string' parameter: the ABI passes (POINTER, LENGTH),
so the length is explicit and no NUL scan is involved."
  (%p1-read-bytes state addr len))

(defun %p1-bytes->string (bytes)
  "Decode UTF-8 BYTES into a host string. WASI strings/paths are UTF-8;
an ill-formed sequence yields replacement characters rather than a
host error (the ABI's own validation is length/range, not encoding)."
  (let ((out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (n (length bytes))
        (i 0))
    (flet ((cont (k)
             (when (< (+ i k) n)
               (aref bytes (+ i k)))))
      (loop while (< i n)
            do (let ((b (aref bytes i)))
                 (cond
                   ((< b #x80)
                    (vector-push-extend (code-char b) out)
                    (incf i))
                   ((and (<= #xC2 b #xDF) (cont 1))
                    (let ((c1 (cont 1)))
                      (if (= #x80 (logand c1 #xC0))
                          (progn
                            (vector-push-extend
                             (code-char (logior (ash (logand b #x1f) 6)
                                                (logand c1 #x3f)))
                             out)
                            (incf i 2))
                          (progn (vector-push-extend #\? out) (incf i)))))
                   ((and (<= #xE0 b #xEF) (cont 2))
                    (let ((c1 (cont 1)) (c2 (cont 2)))
                      (if (and (= #x80 (logand c1 #xC0)) (= #x80 (logand c2 #xC0)))
                          (progn
                            (vector-push-extend
                             (code-char (logior (ash (logand b #x0f) 12)
                                                (ash (logand c1 #x3f) 6)
                                                (logand c2 #x3f)))
                             out)
                            (incf i 3))
                          (progn (vector-push-extend #\? out) (incf i)))))
                   ((and (<= #xF0 b #xF4) (cont 3))
                    (let ((c1 (cont 1)) (c2 (cont 2)) (c3 (cont 3)))
                      (if (and (= #x80 (logand c1 #xC0)) (= #x80 (logand c2 #xC0))
                               (= #x80 (logand c3 #xC0)))
                          (progn
                            (vector-push-extend
                             (code-char (logior (ash (logand b #x07) 18)
                                                (ash (logand c1 #x3f) 12)
                                                (ash (logand c2 #x3f) 6)
                                                (logand c3 #x3f)))
                             out)
                            (incf i 4))
                          (progn (vector-push-extend #\? out) (incf i)))))
                   (t (vector-push-extend #\? out) (incf i)))))
      (concatenate 'simple-string out))))

(defun %p1-string->bytes (string)
  "Encode STRING as UTF-8 — the byte form every WASI string
parameter carries."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (loop for ch across string
          for code = (char-code ch)
          do (cond
               ((< code #x80)
                (vector-push-extend code out))
               ((< code #x800)
                (vector-push-extend (logior #xC0 (ash code -6)) out)
                (vector-push-extend (logior #x80 (logand code #x3f)) out))
               ((< code #x10000)
                (vector-push-extend (logior #xE0 (ash code -12)) out)
                (vector-push-extend (logior #x80 (logand (ash code -6) #x3f)) out)
                (vector-push-extend (logior #x80 (logand code #x3f)) out))
               (t
                (vector-push-extend (logior #xF0 (ash code -18)) out)
                (vector-push-extend (logior #x80 (logand (ash code -12) #x3f)) out)
                (vector-push-extend (logior #x80 (logand (ash code -6) #x3f)) out)
                (vector-push-extend (logior #x80 (logand code #x3f)) out))))
    (coerce out '(vector (unsigned-byte 8)))))

;;; ---------------------------------------------------------------
;;; Host errno -> Preview1 errno.

(defvar *p1-errno-map* nil)

(defun %p1-errno-map* ()
  "Linux errno -> Preview1 errno, named (no bare ABI indices): the
values are the constants the WITX enumeration defines."
  (or *p1-errno-map*
      (setf *p1-errno-map*
            (let ((h (make-hash-table :test #'eql)))
              (dolist (pair
                        (list (cons 1 +p1-perm+) (cons 2 +p1-noent+)
                              (cons 3 +p1-srch+) (cons 4 +p1-intr+)
                              (cons 5 +p1-io+) (cons 6 +p1-nxio+)
                              (cons 7 +p1-2big+) (cons 8 +p1-noexec+)
                              (cons 9 +p1-badf+) (cons 10 +p1-child+)
                              (cons 11 +p1-again+) (cons 12 +p1-nomem+)
                              (cons 13 +p1-acces+) (cons 14 +p1-fault+)
                              (cons 16 +p1-busy+) (cons 17 +p1-exist+)
                              (cons 18 +p1-xdev+) (cons 19 +p1-nodev+)
                              (cons 20 +p1-notdir+) (cons 21 +p1-isdir+)
                              (cons 22 +p1-inval+) (cons 23 +p1-nfile+)
                              (cons 24 +p1-mfile+) (cons 25 +p1-notty+)
                              (cons 26 +p1-txtbsy+) (cons 27 +p1-fbig+)
                              (cons 28 +p1-nospc+) (cons 29 +p1-spipe+)
                              (cons 30 +p1-rofs+) (cons 31 +p1-mlink+)
                              (cons 32 +p1-pipe+) (cons 33 +p1-dom+)
                              (cons 34 +p1-range+) (cons 35 +p1-deadlk+)
                              (cons 36 +p1-nametoolong+) (cons 37 +p1-nolck+)
                              (cons 38 +p1-nosys+) (cons 39 +p1-notempty+)
                              (cons 40 +p1-loop+) (cons 42 +p1-nomsg+)
                              (cons 43 +p1-idrm+) (cons 71 +p1-proto+)
                              (cons 75 +p1-overflow+) (cons 84 +p1-ilseq+)
                              (cons 88 +p1-notsock+) (cons 89 +p1-destaddrreq+)
                              (cons 90 +p1-msgsize+) (cons 91 +p1-prototype+)
                              (cons 92 +p1-noprotoopt+) (cons 93 +p1-protonosupport+)
                              (cons 97 +p1-afnosupport+) (cons 98 +p1-addrinuse+)
                              (cons 99 +p1-addrnotavail+) (cons 100 +p1-netdown+)
                              (cons 101 +p1-netunreach+) (cons 102 +p1-netreset+)
                              (cons 103 +p1-connaborted+) (cons 104 +p1-connreset+)
                              (cons 105 +p1-nobufs+) (cons 106 +p1-isconn+)
                              (cons 107 +p1-notconn+) (cons 110 +p1-timedout+)
                              (cons 111 +p1-connrefused+) (cons 113 +p1-hostunreach+)
                              (cons 114 +p1-already+) (cons 115 +p1-inprogress+)
                              (cons 125 +p1-canceled+) (cons 130 +p1-ownerdead+)
                              (cons 131 +p1-notrecoverable+)))
                (setf (gethash (car pair) h) (cdr pair)))
              h))))

(defun %p1-host-errno (linux-errno)
  (or (gethash linux-errno (%p1-errno-map*))
      +p1-io+))

(defun %p1-check (ret)
  "Turn a `-1' libc result into the matching Preview1 errno."
  (when (minusp ret)
    (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
  ret)

(defconstant +p1-off-max+ (1- (ash 1 63))
  "The largest offset the host off_t can hold.")

(defun %p1-offset (value)
  "A WASI u64 file OFFSET as a host offset. A value the host cannot
represent is the ABI's INVAL, never a Lisp type error."
  (unless (<= 0 value +p1-off-max+) (%p1-abort +p1-inval+))
  value)

(defun %p1-filesize (value)
  "A WASI filesize as a host off_t: beyond the host range the ABI
answer is FBIG."
  (unless (<= 0 value +p1-off-max+) (%p1-abort +p1-fbig+))
  value)

;;; ---------------------------------------------------------------
;;; Additional libc bindings the linear-memory ABI needs.

(sb-alien:define-alien-routine ("read" %p1-libc-read) sb-alien:long
  (fd sb-alien:int) (buf (* sb-alien:unsigned-char)) (count sb-alien:size-t))
(sb-alien:define-alien-routine ("write" %p1-libc-write) sb-alien:long
  (fd sb-alien:int) (buf (* sb-alien:unsigned-char)) (count sb-alien:size-t))
(sb-alien:define-alien-routine ("lseek" %p1-libc-lseek) sb-alien:off-t
  (fd sb-alien:int) (offset sb-alien:off-t) (whence sb-alien:int))
(sb-alien:define-alien-routine ("dup" %p1-libc-dup) sb-alien:int
  (fd sb-alien:int))
(sb-alien:define-alien-routine ("close" %p1-libc-close) sb-alien:int
  (fd sb-alien:int))
(sb-alien:define-alien-routine ("fcntl" %p1-libc-fcntl) sb-alien:int
  (fd sb-alien:int) (cmd sb-alien:int) (arg sb-alien:int))
(sb-alien:define-alien-routine ("posix_fallocate" %p1-libc-fallocate) sb-alien:int
  (fd sb-alien:int) (offset sb-alien:off-t) (length sb-alien:off-t))
(sb-alien:define-alien-routine ("sched_yield" %p1-libc-sched-yield) sb-alien:int)
;; statx(2) is the only portable-in-ABI way to read NANOSECOND
;; timestamps from a descriptor (SBCL's unix-fstat truncates them to
;; whole seconds), and the WASI ABI reports timestamps in ns.
(sb-alien:define-alien-routine ("statx" %p1-libc-statx) sb-alien:int
  (dirfd sb-alien:int) (path sb-alien:c-string) (flags sb-alien:int)
  (mask sb-alien:unsigned-int) (buf sb-sys:system-area-pointer))

(defconstant +at-empty-path+ #x1000)
(defconstant +statx-basic-stats+ #x7ff)

(defun %p1-fstat-times-ns (fd)
  "The descriptor's atime/mtime/ctime as INTEGER NANOSECONDS, or NIL
when statx cannot answer (the caller falls back to whole seconds)."
  (let ((buf (sb-alien:make-alien sb-alien:unsigned-char 256)))
    (unwind-protect
         (let ((ret (%p1-libc-statx fd "" +at-empty-path+
                                    +statx-basic-stats+
                                    (sb-alien:alien-sap buf))))
           (when (zerop ret)
             (let ((sap (sb-alien:alien-sap buf)))
               ;; struct statx: atime @64, ctime @96, mtime @112;
               ;; each {s64 tv_sec, u32 tv_nsec, s32 reserved}.
               (flet ((stamp (base)
                        (+ (* (sb-sys:sap-ref-64 sap base) 1000000000)
                           (sb-sys:sap-ref-32 sap (+ base 8)))))
                 (values (stamp 64) (stamp 112) (stamp 96))))))
      (sb-alien:free-alien buf))))

(sb-alien:define-alien-type nil
  (sb-alien:struct p1-timespec
    (tv-sec sb-alien:long)
    (tv-nsec sb-alien:long)))

(sb-alien:define-alien-routine ("clock_gettime" %p1-libc-clock-gettime) sb-alien:int
  (clock-id sb-alien:int) (tp sb-sys:system-area-pointer))
(sb-alien:define-alien-routine ("clock_getres" %p1-libc-clock-getres) sb-alien:int
  (clock-id sb-alien:int) (tp sb-sys:system-area-pointer))

(sb-alien:define-alien-routine ("nanosleep" %p1-libc-nanosleep) sb-alien:int
  (req sb-sys:system-area-pointer)
  (rem sb-sys:system-area-pointer))

(defconstant +o-append+ #o2000)

(defconstant +p1-pollin+ 1)
(defconstant +p1-pollout+ 4)
(defconstant +p1-pollerr+ 8)
(defconstant +p1-pollhup+ 16)
(defconstant +p1-pollnval+ 32)

(sb-alien:define-alien-routine ("ioctl" %p1-libc-ioctl) sb-alien:int
  (fd sb-alien:int) (request sb-alien:unsigned-long) (arg (* sb-alien:int)))

(defconstant +fionread+ #x541b)

;;; ---------------------------------------------------------------
;;; The fd table.

(defun %p1-add-fd (state entry &key (prefer nil))
  "Install ENTRY in the ABI fd table at PREFER (a preopen slot) or at
the lowest free non-negative slot. Returns the assigned fd."
  (let ((fds (preview1-state-fds state))
        (fd (or prefer 0)))
    (when (null prefer)
      (loop while (gethash fd fds) do (incf fd)))
    (setf (gethash fd fds) entry)
    fd))

(defun %p1-lookup-fd (state fd)
  (or (gethash fd (preview1-state-fds state))
      (%p1-abort +p1-badf+)))

(defun %p1-drop-fd (state fd)
  (remhash fd (preview1-state-fds state)))

(defun %p1-close-entry (entry)
  "Release ENTRY's host handle exactly once."
  (let ((fd (p1-fd-fd entry)))
    (when (and (p1-fd-owned-p entry) (integerp fd) (not (minusp fd)))
      (setf (p1-fd-fd entry) -1)
      (%p1-libc-close fd))))

(defun %p1-require (entry right)
  (unless (logtest right (p1-fd-rights-base entry))
    (%p1-abort +p1-notcapable+))
  entry)

(defun %p1-readable (entry)
  (%p1-require entry +p1-right-fd-read+)
  entry)

(defun %p1-writable (entry)
  (%p1-require entry +p1-right-fd-write+)
  entry)

(defun %p1-filetype-of-mode (mode)
  (let ((fmt (logand mode +s-ifmt+)))
    (cond ((= fmt +s-ifreg+) +p1-file-regular-file+)
          ((= fmt +s-ifdir+) +p1-file-directory+)
          ((= fmt +s-iflnk+) +p1-file-symbolic-link+)
          ((= fmt +s-ifchr+) +p1-file-character-device+)
          ((= fmt +s-ifblk+) +p1-file-block-device+)
          ((= fmt #o140000) +p1-file-socket-stream+)
          (t +p1-file-unknown+))))

(defun %p1-fstat (fd)
  "Host fstat as (values dev ino mode nlink size atime mtime ctime)."
  (multiple-value-bind (ok dev ino mode nlink uid gid rdev size atime mtime ctime)
      (sb-unix:unix-fstat fd)
    (declare (ignore uid gid rdev))
    (unless ok
      (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
    (values dev ino mode nlink size atime mtime ctime)))

;;; ---------------------------------------------------------------
;;; The stdio adapters: a CL stream is a BYTE source/sink here.

(defun %p1-stream-read (stream len)
  "Read at most LEN octets from STREAM, returning an octet vector (a
shorter vector at end of input — never an error)."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (et (stream-element-type stream)))
    (if (subtypep et '(unsigned-byte 8))
        (let ((buf (make-array len :element-type '(unsigned-byte 8))))
          (let ((n (read-sequence buf stream)))
            (replace out buf :end2 n)
            (setf (fill-pointer out) n)
            out))
        (loop repeat len
              for ch = (read-char stream nil nil)
              while ch
              do (vector-push-extend (logand (char-code ch) #xff) out)))
    (coerce out '(vector (unsigned-byte 8)))))

(defun %p1-stream-write (stream bytes)
  "Write BYTES to STREAM (a character stream gets the byte-for-byte
latin-1 mapping) and flush it: a Preview1 `fd_write' is observable
immediately."
  (let ((et (stream-element-type stream)))
    (if (subtypep et '(unsigned-byte 8))
        (write-sequence bytes stream)
        (write-string (map 'string #'code-char bytes) stream)))
  (force-output stream)
  (length bytes))

;;; ---------------------------------------------------------------
;;; Handle-relative path resolution.
;;;
;;; The Preview1/P2 confinement mode is VERSION-NEUTRAL: an internal
;;; `..` is allowed as long as it cannot climb above the preopen root
;;; (the P3 resolver conservatively rejects every `..`, which is
;;; correct but stricter than the P1 spec). Absolute paths are
;;; `notcapable`; symlinks are expanded in-process with a bounded
;;; budget and their targets re-confined.

(defparameter *p1-max-symlink-expansions* 32)

(defun %p1-path-components (state addr len)
  "Split a WITX path parameter (POINTER, LENGTH) into components.
Empty components are dropped; an absolute path is `notcapable'.
The second value is T when the path ends in `/': POSIX gives a
trailing slash meaning (the target must be a directory), and the
final leaf keeps it so the host call sees the same rule."
  (let* ((bytes (%p1-string state addr len))
         (path (%p1-bytes->string bytes)))
    (when (and (plusp (length path)) (char= (char path 0) #\/))
      (%p1-abort +p1-notcapable+))
    ;; A NUL cannot appear in a path: passing it to a C string routine
    ;; would silently truncate the name instead of failing.
    (when (find (code-char 0) path)
      (%p1-abort +p1-ilseq+))
    (let ((parts '())
          (start 0))
      (loop for i from 0 to (length path)
            when (or (= i (length path)) (char= (char path i) #\/))
              do (let ((part (subseq path start i)))
                   (unless (string= part "")
                     (push part parts))
                   (setf start (1+ i))))
      (values (nreverse parts)
              (and (plusp (length path))
                   (char= (char path (1- (length path))) #\/))))))

(defun %p1-open-dir (dirfd name)
  "openat(NAME) relative to DIRFD as a directory handle, not following
a symlink in NAME."
  (%p1-check (%libc-openat dirfd name
                           (logior +o-path+ +o-nofollow+ +o-cloexec+)
                           0)))

(defun %p1-symlink-p (fd)
  (multiple-value-bind (dev ino mode) (%p1-fstat fd)
    (declare (ignore dev ino))
    (= (logand mode +s-ifmt+) +s-iflnk+)))

(defun %p1-readlink-of (dirfd name)
  (let ((buf (sb-alien:make-alien sb-alien:unsigned-char 4096)))
    (unwind-protect
         (let ((n (%libc-readlinkat dirfd name buf 4096)))
           (when (minusp n)
             (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
           (let ((out (make-array n :element-type '(unsigned-byte 8))))
             (loop for i below n
                   do (setf (aref out i) (sb-alien:deref buf i)))
             out))
      (sb-alien:free-alien buf))))

(defun %p1-resolve (state entry path-addr path-len follow-final
                    &key create-p)
  "Resolve PATH-ADDR relative to ENTRY's own directory handle. Returns
(values DIR-FD LEAF) where DIR-FD is the ONLY owned fd the caller must
close and LEAF is the final component (or \".\"). Every intermediate
directory fd opened on the way is closed before returning, so a
multi-component lookup never leaks descriptors.

The lookup is HANDLE-relative: `path_filestat_get(dfd, \"x\")' must
name \"dfd/x\", not \"<preopen>/x\" (a guest that opened a subdirectory
and stats entries in it is the ordinary case, not an escape). A
`..' climbs to the parent by opening it; the preopen root is the
hard floor (compared by host dev/inode), so walking up from any
handle below it stays confined. Symlink targets are re-confined the
same way and the expansion budget is bounded. CREATE-P allows a
MISSING final component (path_open with oflags::creat): resolution
must not require the file to exist before it is created."
  ;; The path components are read BEFORE the handle is duplicated: a
  ;; bad pointer or an absolute path (FAULT/NOTCAPABLE) must not leak
  ;; the dup'd directory handle.
  (multiple-value-bind (todo trailing-slash-p)
      (%p1-path-components state path-addr path-len)
    (let* ((preopen (p1-fd-preopen entry))
           (start (p1-fd-fd entry))
           (budget *p1-max-symlink-expansions*)
           (root-id (multiple-value-bind (dev ino) (%p1-fstat preopen)
                      (cons dev ino)))
           (floor-id (multiple-value-bind (dev ino) (%p1-fstat start)
                       (cons dev ino)))
           (stack (list (if start
                            (%p1-check (%p1-libc-dup start))
                            (%p1-abort +p1-badf+)))))
      (labels ((fail (errno)
                 (dolist (fd stack) (ignore-errors (%p1-libc-close fd)))
                 (%p1-abort errno))
               (done (leaf)
                 "Hand DIR=top back to the caller and close every
                  intermediate directory fd below it."
                 (dolist (fd (cdr stack)) (ignore-errors (%p1-libc-close fd)))
                 (values (car stack)
                         (if trailing-slash-p
                             (concatenate 'string leaf "/")
                             leaf)))
               (at-id-p (fd id)
                 (multiple-value-bind (dev ino) (%p1-fstat fd)
                   (and (= dev (car id)) (= ino (cdr id)))))
               (pop-root ()
                 ;; `..' is confined to the descriptor the path is
                 ;; resolved against (and, transitively, to the
                 ;; preopen root): a path may use `..' inside the
                 ;; handle but may not climb out of it. wasi-testsuite
                 ;; requires PERM/NOTCAPABLE for such a path rather
                 ;; than resolving it against the preopen.
                 (if (or (at-id-p (car stack) floor-id)
                         (at-id-p (car stack) root-id))
                     (fail +p1-notcapable+)
                     (let ((parent (%libc-openat (car stack) ".."
                                                 (logior +o-path+ +o-directory+
                                                         +o-cloexec+)
                                                 0)))
                       (when (minusp parent)
                         (fail (%p1-host-errno (sb-alien:get-errno))))
                       (%p1-libc-close (pop stack))
                       (push parent stack))))
             (open-probe (name)
               (let ((fd (%libc-openat (car stack) name
                                       (logior +o-path+ +o-nofollow+ +o-cloexec+)
                                       0)))
                 (when (minusp fd)
                   (fail (%p1-host-errno (sb-alien:get-errno))))
                 fd))
             (expand (name)
               "The symlink NAME's target components, re-confined."
               (when (zerop budget) (fail +p1-loop+))
               (decf budget)
               (let* ((target (%p1-readlink-of (car stack) name))
                      (tpath (%p1-bytes->string target)))
                 (when (and (plusp (length tpath))
                            (char= (char tpath 0) #\/))
                   (fail +p1-notcapable+))
                 (%p1-path-components-from-string tpath))))
      (block resolve
        (loop
          (cond
            ((null todo)
             ;; the path itself names the directory: operate on "."
             (return-from resolve (done ".")))
            ((null (cdr todo))
             ;; final component
             (let ((name (car todo)))
               (cond
                 ((string= name ".")
                  (return-from resolve (done ".")))
                 ((string= name "..")
                  (pop-root)
                  (return-from resolve (done ".")))
                 (t
                  (let ((probe (%libc-openat (car stack) name
                                             (logior +o-path+ +o-nofollow+ +o-cloexec+)
                                             0))
                        (errno (sb-alien:get-errno)))
                    (when (minusp probe)
                      (if (and create-p (= errno 2))
                          (return-from resolve (done name))
                          (fail (%p1-host-errno errno))))
                    (cond
                      ((%p1-symlink-p probe)
                       (%p1-libc-close probe)
                       (if follow-final
                           (setf todo (expand name))
                           (return-from resolve (done name))))
                      (t
                       (%p1-libc-close probe)
                       (return-from resolve (done name)))))))))
            (t
             ;; intermediate component
             (let ((name (car todo)))
               (cond
                 ((string= name ".")
                  (setf todo (cdr todo)))
                 ((string= name "..")
                  (pop-root)
                  (setf todo (cdr todo)))
                 (t
                  (let ((probe (open-probe name)))
                    (if (%p1-symlink-p probe)
                        (progn
                          (%p1-libc-close probe)
                          (setf todo (append (expand name) (cdr todo))))
                        (progn
                          (push probe stack)
                          (setf todo (cdr todo))))))))))))))))

(defun %p1-path-components-from-string (path)
  (let ((parts '())
        (start 0))
    (loop for i from 0 to (length path)
          when (or (= i (length path)) (char= (char path i) #\/))
            do (let ((part (subseq path start i)))
                 (unless (string= part "")
                   (push part parts))
                 (setf start (1+ i))))
    (nreverse parts)))

(defmacro %p1-with-resolved ((dir leaf) (state entry path-addr path-len follow
                                             &key create-p) &body body)
  "Bind DIR/LEAF from %P1-RESOLVE and close the owned dir fd."
  `(multiple-value-bind (,dir ,leaf) (%p1-resolve ,state ,entry ,path-addr ,path-len ,follow
                                                  :create-p ,create-p)
     (unwind-protect (progn ,@body)
       (%p1-libc-close ,dir))))

;;; ---------------------------------------------------------------
;;; iovec / ciovec helpers.

(defun %p1-iov-length (state iovs count)
  "Validate COUNT iovec records and return their total byte length as
a Lisp integer (overflow beyond the 32-bit address space is FAULT)."
  (%p1-check-range state iovs (* count +p1-size-iovec+))
  (let ((total 0))
    (dotimes (i count)
      (let* ((base (+ iovs (* i +p1-size-iovec+)))
             (ptr (%p1-u32 state base))
             (len (%p1-u32 state (+ base 4))))
        (%p1-check-range state ptr len)
        (incf total len)))
    total))

(defun %p1-scatter (state iovs count bytes)
  "Write BYTES across COUNT iovecs in order; return the number of
bytes actually placed (bytes beyond the iovecs are dropped). Every
data span is re-validated at USE time: the descriptor table may
overlap the destination, so an earlier scatter write can change a
later iovec."
  (let ((offset 0))
    (dotimes (i count)
      (let* ((base (+ iovs (* i +p1-size-iovec+)))
             (ptr (%p1-u32 state base))
             (len (%p1-u32 state (+ base 4))))
        (%p1-check-range state ptr len)
        (when (< offset (length bytes))
          (let ((n (min len (- (length bytes) offset))))
            (replace (%p1-bytes state) bytes
                     :start1 ptr :start2 offset :end2 (+ offset n))
            (incf offset n)))))
    offset))

(defun %p1-gather (state iovs count)
  "A contiguous octet vector of the COUNT iovecs' contents. Each span
is validated again at copy time (the descriptors may alias the data)."
  (let* ((total (%p1-iov-length state iovs count))
         (out (make-array total :element-type '(unsigned-byte 8))))
    (let ((offset 0))
      (dotimes (i count)
        (let* ((base (+ iovs (* i +p1-size-iovec+)))
               (ptr (%p1-u32 state base))
               (len (%p1-u32 state (+ base 4))))
          (%p1-check-range state ptr len)
          (replace out (%p1-bytes state) :start1 offset :start2 ptr :end2 (+ ptr len))
          (incf offset len))))
    out))

;;; ---------------------------------------------------------------
;;; The import bodies. Each takes the state plus the ABI arguments and
;;; returns the errno integer; %P1-ABORT becomes its errno.

(defmacro %p1-fn (name (state &rest params) &body body)
  `(defun ,name (,state ,@params)
     (handler-case (locally ,@body)
       (p1-abort (c) (p1-abort-errno c)))))

;;; ---- args / environ

(defun %p1-string-list-sizes (strings)
  "Count and TOTAL BYTE SIZE of a WASI string table: each entry is
UTF-8 plus one NUL terminator."
  (values (length strings)
          (reduce #'+ strings
                  :key (lambda (s) (1+ (length (%p1-string->bytes s))))
                  :initial-value 0)))

(%p1-fn %p1-args-sizes-get (state argc-ptr size-ptr)
  (multiple-value-bind (count size) (%p1-string-list-sizes
                                      (wasi-context-arguments (preview1-state-context state)))
    (setf (%p1-u32 state argc-ptr) count
          (%p1-u32 state size-ptr) size)
    +p1-success+))

(defun %p1-write-string-table (state table-ptr buf-ptr strings)
  "Write a NUL-terminated string table and its pointer vector."
  (let ((cursor buf-ptr))
    (loop for s in strings
          for i from 0
          do (setf (%p1-u32 state (+ table-ptr (* i 4))) cursor)
             (let ((bytes (%p1-string->bytes s)))
               (%p1-write-bytes state cursor bytes)
               (setf (%p1-u8 state (+ cursor (length bytes))) 0)
               (incf cursor (1+ (length bytes)))))
    +p1-success+))

(%p1-fn %p1-args-get (state argv buf)
  (%p1-write-string-table state argv buf
                          (wasi-context-arguments (preview1-state-context state))))

(defun %p1-environ-strings (state)
  (loop for (name . value) in (wasi-context-environment (preview1-state-context state))
        collect (format nil "~a=~a" name value)))

(%p1-fn %p1-environ-sizes-get (state count-ptr size-ptr)
  (multiple-value-bind (count size) (%p1-string-list-sizes (%p1-environ-strings state))
    (setf (%p1-u32 state count-ptr) count
          (%p1-u32 state size-ptr) size)
    +p1-success+))

(%p1-fn %p1-environ-get (state environ buf)
  (%p1-write-string-table state environ buf (%p1-environ-strings state)))

;;; ---- clocks

(defun %p1-clock-id (id)
  "The Linux clock id for a Preview1 clockid, or NIL (invalid)."
  (case id
    (0 0)                               ; CLOCK_REALTIME
    (1 1)                               ; CLOCK_MONOTONIC
    (2 2)                               ; CLOCK_PROCESS_CPUTIME_ID
    (3 3)                               ; CLOCK_THREAD_CPUTIME_ID
    (t nil)))

(defun %p1-timespec ()
  "A 16-byte timespec buffer (tv_sec, tv_nsec as 64-bit longs)."
  (sb-alien:make-alien sb-alien:unsigned-char 16))

(defun %p1-timespec-sec (alien)
  (sb-sys:sap-ref-64 (sb-alien:alien-sap alien) 0))

(defun %p1-timespec-nsec (alien)
  (sb-sys:sap-ref-64 (sb-alien:alien-sap alien) 8))

(defun %p1-clock-now-ns (id)
  "Nanoseconds from clock ID, or NIL when the host cannot serve it."
  (let ((host (%p1-clock-id id)))
    (unless host (return-from %p1-clock-now-ns nil))
    (let ((ts (%p1-timespec)))
      (unwind-protect
           (progn
             (when (minusp (%p1-libc-clock-gettime host (sb-alien:alien-sap ts)))
               (return-from %p1-clock-now-ns nil))
             (+ (* (%p1-timespec-sec ts) 1000000000)
                (%p1-timespec-nsec ts)))
        (sb-alien:free-alien ts)))))

(%p1-fn %p1-clock-res-get (state id res-ptr)
  (let ((host (%p1-clock-id id)))
    (unless host (%p1-abort +p1-inval+))
    (let ((ts (%p1-timespec)))
      (unwind-protect
           (progn
             (when (minusp (%p1-libc-clock-getres host (sb-alien:alien-sap ts)))
               (%p1-abort +p1-inval+))
             (setf (%p1-u64 state res-ptr)
                   (max 1 (+ (* (%p1-timespec-sec ts) 1000000000)
                             (%p1-timespec-nsec ts))))
             +p1-success+)
        (sb-alien:free-alien ts)))))

(%p1-fn %p1-clock-time-get (state id precision time-ptr)
  (declare (ignore precision))
  (let ((ns (%p1-clock-now-ns id)))
    (unless ns (%p1-abort +p1-inval+))
    (setf (%p1-u64 state time-ptr) ns)
    +p1-success+))

;;; ---- fd lifecycle / fdstat / filestat

(%p1-fn %p1-fdclose (state fd)
  (let ((entry (%p1-lookup-fd state fd)))
    (%p1-close-entry entry)
    (%p1-drop-fd state fd)
    +p1-success+))

(%p1-fn %p1-fd-advise (state fd offset len advice)
  (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-advise+)
  (unless (<= 0 advice 5) (%p1-abort +p1-inval+))
  (let ((ret (%libc-fadvise (p1-fd-fd (%p1-lookup-fd state fd))
                            (%p1-offset offset) (%p1-offset len) advice)))
    (if (zerop ret) +p1-success+ (%p1-host-errno ret))))

(%p1-fn %p1-fd-allocate (state fd offset len)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-allocate+)))
    (%p1-offset offset)
    (%p1-offset len)
    (when (> (+ offset len) +p1-off-max+) (%p1-abort +p1-fbig+))
    (let ((ret (%p1-libc-fallocate (p1-fd-fd entry) offset len)))
      (if (zerop ret) +p1-success+ (%p1-host-errno ret)))))

(%p1-fn %p1-fd-datasync (state fd)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-datasync+)))
    (%p1-check (%libc-fdatasync (p1-fd-fd entry)))
    +p1-success+))

(%p1-fn %p1-fd-sync (state fd)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-sync+)))
    (%p1-check (%libc-fsync (p1-fd-fd entry)))
    +p1-success+))

(defun %p1-entry-filetype (entry)
  (if (eq (p1-fd-kind entry) :stream)
      +p1-file-character-device+
      (multiple-value-bind (dev ino mode) (%p1-fstat (p1-fd-fd entry))
        (declare (ignore dev ino))
        (%p1-filetype-of-mode mode))))

(%p1-fn %p1-fd-fdstat-get (state fd ptr)
  (let* ((entry (%p1-lookup-fd state fd))
         (type (%p1-entry-filetype entry)))
    (%p1-check-range state ptr +p1-size-fdstat+)
    ;; fs_filetype u8 @0, fs_flags u16 @2, fs_rights_base u64 @8,
    ;; fs_rights_inheriting u64 @16, size 24
    (setf (%p1-u8 state ptr) type
          (%p1-u16 state (+ ptr 2)) (p1-fd-flags entry)
          (%p1-u64 state (+ ptr 8)) (p1-fd-rights-base entry)
          (%p1-u64 state (+ ptr 16)) (p1-fd-rights-inheriting entry))
    +p1-success+))

(%p1-fn %p1-fd-fdstat-set-flags (state fd flags)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-fdstat-set-flags+)))
    (when (logtest flags (lognot +p1-fdflag-all+)) (%p1-abort +p1-inval+))
    (when (and (not (eq (p1-fd-kind entry) :stream))
               (not (minusp (p1-fd-fd entry))))
      ;; O_APPEND and O_NONBLOCK are the status flags that can be
      ;; changed after open. Record-only flags left the host handle in
      ;; append mode forever, so clearing APPEND still appended.
      (let ((cur (%p1-libc-fcntl (p1-fd-fd entry) 3 0))) ; F_GETFL
        (when (minusp cur) (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
        (let ((want (logandc2 cur (logior +o-append+ +o-nonblock+))))
          (when (logtest flags +p1-fdflag-append+)
            (setf want (logior want +o-append+)))
          (when (logtest flags +p1-fdflag-nonblock+)
            (setf want (logior want +o-nonblock+)))
          (%p1-check (%p1-libc-fcntl (p1-fd-fd entry) 4 want))))) ; F_SETFL
    (setf (p1-fd-flags entry) flags)
    +p1-success+))

(%p1-fn %p1-fd-fdstat-set-rights (state fd base inheriting)
  (let ((entry (%p1-lookup-fd state fd)))
    ;; rights may only ever shrink
    (when (logtest base (lognot (p1-fd-rights-base entry)))
      (%p1-abort +p1-notcapable+))
    (when (logtest inheriting (lognot (p1-fd-rights-inheriting entry)))
      (%p1-abort +p1-notcapable+))
    (setf (p1-fd-rights-base entry) base
          (p1-fd-rights-inheriting entry) inheriting)
    +p1-success+))

(defun %p1-fill-filestat (state ptr dev ino mode nlink size atim-ns mtim-ns ctim-ns)
  (%p1-check-range state ptr +p1-size-filestat+)
  ;; dev u64 @0, ino u64 @8, filetype u8 @16, nlink u64 @24,
  ;; size u64 @32, atim @40, mtim @48, ctim @56. Timestamps are
  ;; already NANOSECONDS: truncating them to seconds here made a
  ;; guest's set-then-get round trip lose the sub-second part.
  (setf (%p1-u64 state ptr) dev
        (%p1-u64 state (+ ptr 8)) ino
        (%p1-u8 state (+ ptr 16)) (%p1-filetype-of-mode mode)
        (%p1-u64 state (+ ptr 24)) nlink
        (%p1-u64 state (+ ptr 32)) size
        (%p1-u64 state (+ ptr 40)) atim-ns
        (%p1-u64 state (+ ptr 48)) mtim-ns
        (%p1-u64 state (+ ptr 56)) ctim-ns)
  +p1-success+)

(%p1-fn %p1-fd-filestat-get (state fd ptr)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-filestat-get+)))
    (if (eq (p1-fd-kind entry) :stream)
        (%p1-fill-filestat state ptr 0 0 0 1 0 0 0 0)
        (multiple-value-bind (dev ino mode nlink size atim mtim ctim)
            (%p1-fstat (p1-fd-fd entry))
          (multiple-value-bind (atim-ns mtim-ns ctim-ns)
              (%p1-fstat-times-ns (p1-fd-fd entry))
            (%p1-fill-filestat
             state ptr dev ino mode nlink size
             (or atim-ns (* atim 1000000000))
             (or mtim-ns (* mtim 1000000000))
             (or ctim-ns (* ctim 1000000000))))))))

(%p1-fn %p1-fd-filestat-set-size (state fd size)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-filestat-set-size+)))
    (%p1-check (%libc-ftruncate (p1-fd-fd entry) (%p1-filesize size)))
    +p1-success+))

(defun %p1-check-fstflags (fstflags)
  "FSTFLAGS is a 4-bit set, and the atim/atim-now (and mtim/mtim-now)
pair is mutually exclusive: both set is INVAL, not \"now wins\"."
  (when (logtest fstflags (lognot 15)) (%p1-abort +p1-inval+))
  (when (= (logand fstflags (logior +p1-fst-atim+ +p1-fst-atim-now+))
           (logior +p1-fst-atim+ +p1-fst-atim-now+))
    (%p1-abort +p1-inval+))
  (when (= (logand fstflags (logior +p1-fst-mtim+ +p1-fst-mtim-now+))
           (logior +p1-fst-mtim+ +p1-fst-mtim-now+))
    (%p1-abort +p1-inval+)))

(defun %p1-timespec-pair (atim mtim fstflags)
  "Build the futimens/utimensat pair: UTIME_NOW / UTIME_OMIT encode
the fstflags' now/omit cases. The pair is a 32-byte buffer laid out as
two timespecs (tv_sec, tv_nsec each 64-bit)."
  (let ((pair (sb-alien:make-alien sb-alien:unsigned-char 32))
        (sap nil))
    (setf sap (sb-alien:alien-sap pair))
    (flet ((fill-slot (base value flag-value flag-now)
             (cond ((logtest fstflags flag-now)
                    (setf (sb-sys:sap-ref-64 sap base) 0
                          (sb-sys:sap-ref-64 sap (+ base 8)) +utime-now+))
                   ((logtest fstflags flag-value)
                    (%p1-offset value)
                    (setf (sb-sys:sap-ref-64 sap base) (truncate value 1000000000)
                          (sb-sys:sap-ref-64 sap (+ base 8)) (mod value 1000000000)))
                   (t
                    (setf (sb-sys:sap-ref-64 sap base) 0
                          (sb-sys:sap-ref-64 sap (+ base 8)) +utime-omit+)))))
      (fill-slot 0 atim +p1-fst-atim+ +p1-fst-atim-now+)
      (fill-slot 16 mtim +p1-fst-mtim+ +p1-fst-mtim-now+))
    pair))

(defun %p1-timespec-pair-pointer (pair)
  "PAIR (a 32-byte buffer) as the (* timespec) the libc routines take."
  (sb-alien:sap-alien (sb-alien:alien-sap pair)
                      (* (sb-alien:struct %wasi-timespec))))

(%p1-fn %p1-fd-filestat-set-times (state fd atim mtim fstflags)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-filestat-set-times+)))
    (%p1-check-fstflags fstflags)
    (let ((pair (%p1-timespec-pair atim mtim fstflags)))
      (unwind-protect
           (progn (%p1-check (%libc-futimens (p1-fd-fd entry)
                                             (%p1-timespec-pair-pointer pair)))
                  +p1-success+)
        (sb-alien:free-alien pair)))))

;;; ---- seek / tell

(%p1-fn %p1-fd-seek (state fd offset whence newoffset-ptr)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-seek+))
         ;; the ABI offset is s64; the runtime carries the unsigned bits
         (signed (if (logbitp 63 offset) (- offset (ash 1 64)) offset)))
    (unless (<= 0 whence 2) (%p1-abort +p1-inval+))
    (cond
      ((eq (p1-fd-kind entry) :stream)
       ;; a CL stream has no host offset: keep the guest-visible
       ;; position (programs use lseek(SEEK_CUR, 0) as a tell)
       (when (= whence 2) (%p1-abort +p1-spipe+))
       (let ((new (+ (if (= whence 0) 0 (p1-fd-position entry)) signed)))
         (when (minusp new) (%p1-abort +p1-inval+))
         (setf (p1-fd-position entry) new
               (%p1-u64 state newoffset-ptr) new)
         +p1-success+))
      (t
       (let ((ret (%p1-libc-lseek (p1-fd-fd entry) signed whence)))
         (when (minusp ret)
           (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
         (setf (%p1-u64 state newoffset-ptr) ret)
         +p1-success+)))))

(%p1-fn %p1-fd-tell (state fd ptr)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-tell+)))
    (if (eq (p1-fd-kind entry) :file)
        (let ((ret (%p1-libc-lseek (p1-fd-fd entry) 0 1)))
          (when (minusp ret) (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
          (setf (%p1-u64 state ptr) ret)
          +p1-success+)
        (progn (setf (%p1-u64 state ptr) (p1-fd-position entry))
               +p1-success+))))

;;; ---- read / write

(defun %p1-read-into (entry total &key offset)
  "One read of at most TOTAL octets from ENTRY. OFFSET selects a
positional (pread) transfer. Returns an octet vector."
  (let* ((fd (p1-fd-fd entry))
         (buf (make-array total :element-type '(unsigned-byte 8))))
    (case (p1-fd-kind entry)
      (:stream
       (%p1-stream-read (p1-fd-stream entry) total))
      (:socket
       (let ((alien (sb-alien:make-alien sb-alien:unsigned-char (max 1 total))))
         (unwind-protect
              (let ((n (%libc-recv fd (sb-alien:alien-sap alien) total 0)))
                (when (minusp n)
                  (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
                (loop for i below n
                      do (setf (aref buf i) (sb-sys:sap-ref-8 (sb-alien:alien-sap alien) i)))
                (subseq buf 0 n))
           (sb-alien:free-alien alien))))
      (t
       (let ((alien (sb-alien:make-alien sb-alien:unsigned-char (max 1 total))))
         (unwind-protect
              (let ((n (if offset
                           (%libc-pread fd (sb-alien:alien-sap alien) total offset)
                           (%p1-libc-read fd (sb-alien:alien-sap alien) total))))
                (when (minusp n)
                  (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
                (loop for i below n
                      do (setf (aref buf i) (sb-sys:sap-ref-8 (sb-alien:alien-sap alien) i)))
                (subseq buf 0 n))
           (sb-alien:free-alien alien)))))))

(%p1-fn %p1-fd-read (state fd iovs count nread-ptr)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-read+))
         (total (%p1-iov-length state iovs count))
         (data (%p1-read-into entry total)))
    (let ((placed (%p1-scatter state iovs count data)))
      (when (eq (p1-fd-kind entry) :stream)
        (incf (p1-fd-position entry) placed))
      (setf (%p1-u32 state nread-ptr) placed)
      +p1-success+)))

(%p1-fn %p1-fd-pread (state fd iovs count offset nread-ptr)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-read+))
         (_ (%p1-require entry +p1-right-fd-seek+))
         (total (%p1-iov-length state iovs count))
         (data (%p1-read-into entry total :offset (%p1-offset offset))))
    (declare (ignore _))
    (setf (%p1-u32 state nread-ptr) (%p1-scatter state iovs count data))
    +p1-success+))

(defun %p1-write-from (entry data &key offset)
  "Write DATA to ENTRY (positional when OFFSET). Returns the count."
  (let ((fd (p1-fd-fd entry))
        (total (length data)))
    (case (p1-fd-kind entry)
      (:stream
       (when offset (%p1-abort +p1-inval+))
       (%p1-stream-write (p1-fd-stream entry) data))
      (:socket
       (when offset (%p1-abort +p1-inval+))
       (let ((alien (sb-alien:make-alien sb-alien:unsigned-char (max 1 total))))
         (unwind-protect
              (progn
                (loop for i below total
                      do (setf (sb-sys:sap-ref-8 (sb-alien:alien-sap alien) i)
                               (aref data i)))
                (let ((n (%libc-send fd (sb-alien:alien-sap alien) total +msg-nosignal+)))
                  (when (minusp n)
                    (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
                  n))
           (sb-alien:free-alien alien))))
      (t
       (let ((alien (sb-alien:make-alien sb-alien:unsigned-char (max 1 total))))
         (unwind-protect
              (progn
                (loop for i below total
                      do (setf (sb-sys:sap-ref-8 (sb-alien:alien-sap alien) i)
                               (aref data i)))
                (let ((n (if offset
                             (%libc-pwrite fd (sb-alien:alien-sap alien) total offset)
                             (%p1-libc-write fd (sb-alien:alien-sap alien) total))))
                  (when (minusp n)
                    (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
                  n))
           (sb-alien:free-alien alien)))))))

(%p1-fn %p1-fd-write (state fd iovs count nwritten-ptr)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-write+))
         (data (%p1-gather state iovs count))
         (n (%p1-write-from entry data)))
    (when (eq (p1-fd-kind entry) :stream)
      (incf (p1-fd-position entry) n))
    (setf (%p1-u32 state nwritten-ptr) n)
    +p1-success+))

(%p1-fn %p1-fd-pwrite (state fd iovs count offset nwritten-ptr)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-write+))
         (_ (%p1-require entry +p1-right-fd-seek+))
         (data (%p1-gather state iovs count))
         (n (%p1-write-from entry data :offset (%p1-offset offset))))
    (declare (ignore _))
    (setf (%p1-u32 state nwritten-ptr) n)
    +p1-success+))

;;; ---- preopens

(%p1-fn %p1-fd-prestat-get (state fd ptr)
  (let ((entry (%p1-lookup-fd state fd)))
    (unless (p1-fd-preopen entry) (%p1-abort +p1-badf+))
    (%p1-check-range state ptr +p1-size-prestat+)
    ;; tag u8 @0 (dir = 0), pr_name_len u32 @4
    (setf (%p1-u8 state ptr) 0
          (%p1-u32 state (+ ptr 4)) (length (%p1-string->bytes (p1-fd-name entry))))
    +p1-success+))

(%p1-fn %p1-fd-prestat-dir-name (state fd path len)
  (let* ((entry (%p1-lookup-fd state fd))
         (bytes (%p1-string->bytes (p1-fd-name entry))))
    (unless (p1-fd-preopen entry) (%p1-abort +p1-badf+))
    (when (< len (length bytes)) (%p1-abort +p1-nametoolong+))
    (%p1-write-bytes state path bytes)
    +p1-success+))

;;; ---- renumber

(%p1-fn %p1-fd-renumber (state from to)
  (let ((entry (%p1-lookup-fd state from))
        (fds (preview1-state-fds state)))
    (when (= from to)
      (return-from %p1-fd-renumber +p1-success+))
    ;; `to' must be an existing descriptor: dup2-style renumbering to
    ;; an arbitrary slot is not allowed (EBADF).
    (let ((target (gethash to fds)))
      (unless target (%p1-abort +p1-badf+))
      (%p1-close-entry target))
    (remhash from fds)
    (setf (gethash to fds) entry)
    +p1-success+))

;;; ---- directory reading

(defun %p1-dir-cache-invalidate (entry)
  "Drop ENTRY's cached listing after an operation changed the
directory it names; a stale cache would hide the new/removed child
from a later fd_readdir."
  (when entry
    (setf (p1-fd-entries entry) nil
          (p1-fd-entries-cookie entry) 0)))

(defun %p1-dir-entries (state entry)
  "The cached entry list: (\".\" \"..\" then directory order), each
entry a list (NAME-BYTES INO FILETYPE)."
  (or (p1-fd-entries entry)
      (let* ((fd (%p1-check (%p1-libc-dup (p1-fd-fd entry))))
             (dir (%libc-fdopendir fd))
             (names '()))
        ;; dup(2) shares the open file DESCRIPTION, so the host
        ;; directory offset is shared with the descriptor. fdopendir
        ;; does not rewind, so without this seek a second fd_readdir
        ;; on the same directory would start at EOF and see nothing.
        (%p1-check (%p1-libc-lseek fd 0 0))
        (when (sb-alien:null-alien dir)
          (%p1-libc-close fd)
          (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
        (unwind-protect
             (progn
               (loop for d = (%libc-readdir dir)
                     while (not (sb-alien:null-alien d))
                     do (let ((name (make-array 0 :element-type '(unsigned-byte 8)
                                                  :adjustable t :fill-pointer 0)))
                          (loop for i from 0
                                for code = (sb-alien:deref
                                            (sb-alien:slot (sb-alien:deref d) 'd-name)
                                            i)
                                until (zerop code)
                                do (vector-push-extend (logand code #xff) name))
                          ;; the Preview1 listing synthesizes "." and
                          ;; ".." itself: the host's copies are dropped
                          (let ((nm (coerce name '(vector (unsigned-byte 8)))))
                            (unless (member (map 'string #'code-char nm)
                                            '("." "..") :test #'string=)
                              (push nm names)))))
               (setf names (nreverse names))
               ;; "." must report the directory's OWN inode (guests
               ;; compare it with fd_filestat_get) and ".." its
               ;; parent's, not a synthesized 0.
               (let* ((self (multiple-value-bind (dev ino mode) (%p1-fstat fd)
                              (declare (ignore mode))
                              (cons dev ino)))
                      (parent (let ((probe (%libc-openat fd ".."
                                                         (logior +o-path+ +o-directory+
                                                                 +o-cloexec+)
                                                         0)))
                                (when (minusp probe)
                                  (%p1-abort (%p1-host-errno
                                              (sb-alien:get-errno))))
                                (unwind-protect
                                     (multiple-value-bind (dev ino mode)
                                         (%p1-fstat probe)
                                       (declare (ignore dev mode))
                                       ino)
                                  (%p1-libc-close probe)))))
                 (let ((full (list (list (%p1-string->bytes ".") (cdr self)
                                         +p1-file-directory+)
                                   (list (%p1-string->bytes "..") parent
                                         +p1-file-directory+))))
                   (dolist (name names)
                     (let ((probe (%libc-openat
                                   fd (%p1-bytes->string name)
                                   (logior +o-path+ +o-nofollow+ +o-cloexec+)
                                   0)))
                       (when (minusp probe)
                         (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
                       (multiple-value-bind (dev ino mode) (%p1-fstat probe)
                         (declare (ignore dev))
                         (%p1-libc-close probe)
                         (setf full
                               (append full
                                       (list (list name ino
                                                   (%p1-filetype-of-mode mode))))))))
                   (setf (p1-fd-entries entry) full)
                   (p1-fd-entries entry))))
          (%libc-closedir dir)))))

(%p1-fn %p1-fd-readdir (state fd buf buf-len cookie bufused-ptr)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-readdir+)))
    (unless (p1-fd-dir-p entry) (%p1-abort +p1-notdir+))
    (%p1-check-range state buf buf-len)
    (let* ((all (%p1-dir-entries state entry))
           (used 0)
           (remaining buf-len)
           (header (make-array +p1-size-dirent+
                               :element-type '(unsigned-byte 8))))
      ;; A page is EOF only when it is SHORTER than the buffer: a
      ;; truncated final dirent is written instead of leaving the
      ;; buffer partly empty, so `bufused = buf_len' means "more
      ;; entries remain" (the same rule wasi-libc and wasmtime use).
      (block fill
        (loop for rest on (nthcdr (min cookie (length all)) all)
              for i from cookie
              while rest
              do (let* ((e (car rest))
                        (name (first e))
                        (namelen (length name)))
                   ;; d_next (u64 @0) and d_ino (u64 @8), little endian
                   (loop for k below 8
                         do (setf (aref header k)
                                  (logand (ash (1+ i) (* -8 k)) #xff)
                                  (aref header (+ 8 k))
                                  (logand (ash (second e) (* -8 k)) #xff)))
                   ;; d_namlen (u32 @16) and d_type (u8 @20)
                   (setf (aref header 16) (logand namelen #xff)
                         (aref header 17) (logand (ash namelen -8) #xff)
                         (aref header 18) (logand (ash namelen -16) #xff)
                         (aref header 19) (logand (ash namelen -24) #xff)
                         (aref header 20) (third e))
                   (let* ((hcopied (min remaining +p1-size-dirent+))
                          (ptr (+ buf used)))
                     (replace (%p1-bytes state) header
                              :start1 ptr :end2 hcopied)
                     (incf used hcopied)
                     (decf remaining hcopied)
                     (when (zerop remaining) (return-from fill))
                     (let ((copied (min namelen remaining)))
                       (replace (%p1-bytes state) name
                                :start1 (+ ptr hcopied) :end2 copied)
                       (incf used copied)
                       (decf remaining copied)
                       (when (zerop remaining) (return-from fill))
                       (when (< copied namelen) (return-from fill)))))))
      (setf (%p1-u32 state bufused-ptr) used)
      +p1-success+)))

;;; ---- path operations

(%p1-fn %p1-path-create-directory (state fd path-addr path-len)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-create-directory+)))
    (%p1-with-resolved (dir leaf) (state entry path-addr path-len nil :create-p t)
      (%p1-check (%libc-mkdirat dir leaf #o755))
      (%p1-dir-cache-invalidate entry)
      +p1-success+)))

(%p1-fn %p1-path-filestat-get (state fd flags path-addr path-len ptr)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-filestat-get+))
         (follow (logtest flags +p1-lookup-symlink-follow+)))
    (%p1-with-resolved (dir leaf) (state entry path-addr path-len follow)
      (let ((probe (%libc-openat dir leaf
                                 (logior +o-path+ +o-cloexec+
                                         (if follow 0 +o-nofollow+))
                                 0)))
        (when (minusp probe)
          (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
        (unwind-protect
             (multiple-value-bind (dev ino mode nlink size atim mtim ctim)
                 (%p1-fstat probe)
               (multiple-value-bind (atim-ns mtim-ns ctim-ns)
                   (%p1-fstat-times-ns probe)
                 (%p1-fill-filestat
                  state ptr dev ino mode nlink size
                  (or atim-ns (* atim 1000000000))
                  (or mtim-ns (* mtim 1000000000))
                  (or ctim-ns (* ctim 1000000000)))))
          (%p1-libc-close probe))))))

(%p1-fn %p1-path-filestat-set-times (state fd flags path-addr path-len atim mtim fstflags)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-filestat-set-times+))
         (follow (logtest flags +p1-lookup-symlink-follow+)))
    (%p1-check-fstflags fstflags)
    (let ((pair (%p1-timespec-pair atim mtim fstflags)))
      (unwind-protect
           (progn
             (%p1-with-resolved (dir leaf) (state entry path-addr path-len follow)
               ;; utimensat accepts only AT_SYMLINK_NOFOLLOW or 0
               ;; (AT_SYMLINK_FOLLOW is EINVAL there).
               (%p1-check (%libc-utimensat dir leaf
                                           (%p1-timespec-pair-pointer pair)
                                           (if follow 0 +at-symlink-nofollow+))))
             +p1-success+)
        (sb-alien:free-alien pair)))))

(%p1-fn %p1-path-link (state old-fd old-flags old-path old-path-len new-fd new-path new-path-len)
  (let* ((old-entry (%p1-require (%p1-lookup-fd state old-fd) +p1-right-path-link-source+))
         (new-entry (%p1-require (%p1-lookup-fd state new-fd) +p1-right-path-link-target+))
         (follow (logtest old-flags +p1-lookup-symlink-follow+)))
    (%p1-with-resolved (old-dir old-leaf) (state old-entry old-path old-path-len follow)
      (%p1-with-resolved (new-dir new-leaf) (state new-entry new-path new-path-len nil
                                           :create-p t)
        ;; linkat accepts only AT_SYMLINK_FOLLOW or 0:
        ;; AT_SYMLINK_NOFOLLOW (0x100) is EINVAL there.
        (%p1-check (%libc-linkat old-dir old-leaf new-dir new-leaf
                                 (if follow +at-symlink-follow+ 0)))
        (%p1-dir-cache-invalidate new-entry)
        +p1-success+))))

(defun %p1-rights-fdflags-open (entry oflags fdflags)
  "The host open flags for OFLAGS/FDFLAGS, with the access mode chosen
from the rights the guest asked for on the new descriptor."
  (declare (ignore fdflags))
  (let ((flags 0))
    (when (logtest oflags +p1-oflag-creat+) (setf flags (logior flags +o-creat+)))
    (when (logtest oflags +p1-oflag-excl+) (setf flags (logior flags +o-excl+)))
    (when (logtest oflags +p1-oflag-trunc+) (setf flags (logior flags +o-trunc+)))
    (when (logtest oflags +p1-oflag-directory+) (setf flags (logior flags +o-directory+)))
    flags))

(%p1-fn %p1-path-open (state fd dirflags path-addr path-len oflags rights-base rights-inheriting fdflags out-ptr)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-open+))
         (follow (logtest dirflags +p1-lookup-symlink-follow+))
         (parent-base (p1-fd-rights-base entry))
         (parent-inh (p1-fd-rights-inheriting entry)))
    (declare (ignore parent-base))
    (unless (zerop (logandc2 oflags (logior +p1-oflag-creat+ +p1-oflag-directory+
                                             +p1-oflag-excl+ +p1-oflag-trunc+)))
      (%p1-abort +p1-inval+))
    ;; rights attenuation: the child may never exceed the parent's
    ;; inheriting rights
    (when (logtest rights-base (lognot parent-inh)) (%p1-abort +p1-notcapable+))
    (when (logtest rights-inheriting (lognot parent-inh)) (%p1-abort +p1-notcapable+))
    (when (logtest oflags +p1-oflag-creat+)
      (%p1-require entry +p1-right-path-create-file+))
    (when (logtest oflags +p1-oflag-trunc+)
      (%p1-require entry +p1-right-path-filestat-set-size+))
    ;; the result slot is validated BEFORE any host side effect and
    ;; before a guest fd is installed.
    (%p1-check-range state out-ptr 4)
    ;; The host access mode comes from the rights the guest asked for:
    ;; a write right means the host open itself must be writable, so
    ;; `path_open "." O_DIRECTORY (FD_READ|FD_WRITE)' fails with EISDIR
    ;; exactly as POSIX `open' does. A directory therefore only opens
    ;; read-only; callers that do not ask for write (the preopen base
    ;; rights contain none) never make the host demand it.
    (let ((access (cond ((and (logtest rights-base +p1-right-fd-write+)
                               (logtest rights-base +p1-right-fd-read+))
                         +o-rdwr+)
                        ((logtest rights-base +p1-right-fd-write+) +o-wronly+)
                        (t +o-rdonly+))))
      (%p1-with-resolved (dir leaf) (state entry path-addr path-len follow
                                           :create-p (logtest oflags +p1-oflag-creat+))
        (let ((host-flags (logior access +o-cloexec+
                                  (%p1-rights-fdflags-open entry oflags fdflags)
                                  (if follow 0 +o-nofollow+))))
          (when (logtest fdflags +p1-fdflag-append+) (setf host-flags (logior host-flags +o-append+)))
          (when (logtest fdflags +p1-fdflag-nonblock+) (setf host-flags (logior host-flags +o-nonblock+)))
          (let ((new-fd (%libc-openat dir leaf host-flags #o644)))
            (when (minusp new-fd)
              (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
            (let ((guest-fd nil))
              (handler-case
                  (multiple-value-bind (dev ino mode) (%p1-fstat new-fd)
                    (declare (ignore dev ino))
                    (let* ((filetype (%p1-filetype-of-mode mode))
                           (dir-p (= filetype +p1-file-directory+))
                           ;; A directory handle cannot seek, tell, allocate
                           ;; or be truncated, so those requested rights are
                           ;; not granted on it (`directory_seek' asserts the
                           ;; opened directory has no FD_SEEK). The
                           ;; INHERITING rights are the authority for opens
                           ;; BELOW the new descriptor and stay intact.
                           (granted (if dir-p
                                        (logand rights-base
                                                (lognot +p1-rights-not-on-directory+))
                                        rights-base))
                           (new (%make-p1-fd
                                 :fd new-fd
                                 :kind :file
                                 :preopen (p1-fd-preopen entry)
                                 :name ""
                                 :rights-base granted
                                 :rights-inheriting rights-inheriting
                                 :flags fdflags
                                 :dir-p dir-p
                                 :filetype filetype)))
                      (setf guest-fd (%p1-add-fd state new))
                      (setf (%p1-u32 state out-ptr) guest-fd)
                      (when (logtest oflags +p1-oflag-creat+)
                        (%p1-dir-cache-invalidate entry))
                      +p1-success+))
                (p1-abort (c)
                  ;; release BOTH the host handle and the ABI slot: a
                  ;; failed call must not leave a usable-looking fd
                  (when guest-fd (%p1-drop-fd state guest-fd))
                  (%p1-libc-close new-fd)
                  (error c))))))))))

(%p1-fn %p1-path-readlink (state fd path-addr path-len buf buf-len bufused-ptr)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-readlink+)))
    ;; the guest buffer is validated BEFORE the host read and before the
    ;; scratch allocation (a huge BUF-LEN can otherwise size an alien
    ;; buffer from a bogus span).
    (%p1-check-range state buf buf-len)
    (%p1-with-resolved (dir leaf) (state entry path-addr path-len nil)
      (let* ((alien (sb-alien:make-alien sb-alien:unsigned-char (max 1 buf-len))))
        (unwind-protect
             (let ((n (%libc-readlinkat dir leaf alien buf-len)))
               (when (minusp n)
                 (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
               (when (> n buf-len) (%p1-abort +p1-range+))
               (%p1-check-range state buf n)
               (loop for i below n
                     do (setf (aref (%p1-bytes state) (+ buf i))
                              (sb-sys:sap-ref-8 (sb-alien:alien-sap alien) i)))
               (setf (%p1-u32 state bufused-ptr) n)
               +p1-success+)
          (sb-alien:free-alien alien))))))

(%p1-fn %p1-path-remove-directory (state fd path-addr path-len)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-remove-directory+)))
    (%p1-with-resolved (dir leaf) (state entry path-addr path-len nil)
      ;; unlinkat accepts AT_REMOVEDIR; AT_SYMLINK_NOFOLLOW is NOT a
      ;; valid unlinkat flag (the kernel answers EINVAL), and removing
      ;; a directory never follows a symlink in the first place.
      (%p1-check (%libc-unlinkat dir leaf +at-removedir+))
      (%p1-dir-cache-invalidate entry)
      +p1-success+)))

(%p1-fn %p1-path-rename (state fd old-addr old-len new-fd new-addr new-len)
  (let ((old-entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-rename-source+))
        (new-entry (%p1-require (%p1-lookup-fd state new-fd) +p1-right-path-rename-target+)))
    (%p1-with-resolved (old-dir old-leaf) (state old-entry old-addr old-len nil)
      (%p1-with-resolved (new-dir new-leaf) (state new-entry new-addr new-len nil
                                           :create-p t)
        (%p1-check (%libc-renameat old-dir old-leaf new-dir new-leaf))
        (%p1-dir-cache-invalidate old-entry)
        (%p1-dir-cache-invalidate new-entry)
        +p1-success+))))

(%p1-fn %p1-path-symlink (state old-addr old-len fd new-addr new-len)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-symlink+))
        (target (%p1-bytes->string (%p1-string state old-addr old-len))))
    ;; An absolute target can never be resolved inside the sandbox, so
    ;; creating it is NOTCAPABLE (the same rule the resolver applies
    ;; when expanding a link).
    (when (and (plusp (length target)) (char= (char target 0) #\/))
      (%p1-abort +p1-notcapable+))
    (%p1-with-resolved (dir leaf) (state entry new-addr new-len nil :create-p t)
      (%p1-check (%libc-symlinkat target dir leaf))
      (%p1-dir-cache-invalidate entry)
      +p1-success+)))

(%p1-fn %p1-path-unlink-file (state fd path-addr path-len)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-path-unlink-file+)))
    (%p1-with-resolved (dir leaf) (state entry path-addr path-len nil)
      (%p1-check (%libc-unlinkat dir leaf 0))
      (%p1-dir-cache-invalidate entry)
      +p1-success+)))

;;; ---- process / scheduling / entropy

(%p1-fn %p1-proc-exit (state code)
  (declare (ignore state))
  (error 'wasi-exit :status code))

(%p1-fn %p1-proc-raise (state signal)
  (%p1-check-range state 0 0)
  (unless (<= 0 signal +p1-signal-max+)
    (%p1-abort +p1-inval+))
  ;; The in-process embedding limitation: a VALID signal request is
  ;; answered NOTSUP and NEVER reaches raise(3) — signalling would
  ;; terminate the hosting SBCL process.
  +p1-notsup+)

(%p1-fn %p1-sched-yield (state)
  (declare (ignore state))
  (%p1-libc-sched-yield)
  +p1-success+)

(%p1-fn %p1-random-get (state buf len)
  (%p1-check-range state buf len)
  (let ((bytes (os-random-bytes len)))
    (%p1-write-bytes state buf bytes)
    +p1-success+))

;;; ---- sockets

(%p1-fn %p1-sock-accept (state fd flags out-ptr)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-sock-accept+)))
    (unless (zerop (logandc2 flags +p1-fdflag-nonblock+)) (%p1-abort +p1-inval+))
    ;; the result slot is validated before accept(2) and before a guest
    ;; fd is installed.
    (%p1-check-range state out-ptr 4)
    (let ((new-fd (%libc-accept4 (p1-fd-fd entry) (sb-sys:int-sap 0) (sb-sys:int-sap 0)
                                 (if (logtest flags +p1-fdflag-nonblock+) +o-nonblock+ 0))))
      (when (minusp new-fd)
        (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
      (let* ((new (%make-p1-fd :fd new-fd :kind :socket
                              :rights-base (p1-fd-rights-base entry)
                              :rights-inheriting (p1-fd-rights-inheriting entry)
                              :flags flags
                              :filetype +p1-file-socket-stream+))
             (guest-fd nil))
        (handler-case
            (progn
              (setf guest-fd (%p1-add-fd state new))
              (setf (%p1-u32 state out-ptr) guest-fd)
              +p1-success+)
          (p1-abort (c)
            (when guest-fd (%p1-drop-fd state guest-fd))
            (%p1-libc-close new-fd)
            (error c)))))))

(%p1-fn %p1-sock-recv (state fd iovs count ri-flags datalen-ptr roflags-ptr)
  (let* ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-read+))
         (total (%p1-iov-length state iovs count))
         (flags (logior (if (logtest ri-flags +p1-riflag-peek+) 2 0) ; MSG_PEEK
                        (if (logtest ri-flags +p1-riflag-waitall+) #x100 :0)))) ; MSG_WAITALL
    ;; flags are validated BEFORE the scratch buffer exists: a rejection
    ;; here must not leak the alien allocation.
    (when (logtest ri-flags (lognot 3)) (%p1-abort +p1-inval+))
    (let ((alien (sb-alien:make-alien sb-alien:unsigned-char (max 1 total))))
      (unwind-protect
           (let ((n (%libc-recv (p1-fd-fd entry) (sb-alien:alien-sap alien) total flags)))
             (when (minusp n)
               (%p1-abort (%p1-host-errno (sb-alien:get-errno))))
             (let ((data (make-array n :element-type '(unsigned-byte 8))))
               (loop for i below n
                     do (setf (aref data i)
                              (sb-sys:sap-ref-8 (sb-alien:alien-sap alien) i)))
               (setf (%p1-u32 state datalen-ptr) (%p1-scatter state iovs count data)
                     (%p1-u16 state roflags-ptr) 0)
               +p1-success+))
        (sb-alien:free-alien alien)))))

(%p1-fn %p1-sock-send (state fd iovs count si-flags datalen-ptr)
  (let ((entry (%p1-require (%p1-lookup-fd state fd) +p1-right-fd-write+)))
    ;; flags first: nothing is copied out of guest memory for a request
    ;; the ABI rejects.
    (when (plusp si-flags) (%p1-abort +p1-inval+))
    (let ((data (%p1-gather state iovs count)))
      (setf (%p1-u32 state datalen-ptr)
            (%p1-write-from entry data))
      +p1-success+)))

(%p1-fn %p1-sock-shutdown (state fd how)
  ;; POSIX shutdown(2) on a non-socket must report ENOTSOCK even though
  ;; the fd has no socket rights (the upstream C suite asserts this for
  ;; STDOUT_FILENO). Only a real socket goes through the rights gate.
  (let* ((entry (%p1-lookup-fd state fd))
         (host (p1-fd-fd entry))
         ;; the guest-visible filetype is authoritative: stdio slots are
         ;; character devices behind a CL stream and may have no host fd
         (socket-p (= (p1-fd-filetype entry) +p1-file-socket-stream+)))
    (unless (logtest how (logior +p1-sdflag-rd+ +p1-sdflag-wr+))
      (%p1-abort +p1-inval+))
    (when (logtest how (lognot 3)) (%p1-abort +p1-inval+))
    (let ((direction (cond ((= how 1) +shut-rd+)
                           ((= how 2) +shut-wr+)
                           (t 2))))
      (unless socket-p
        (%p1-abort +p1-notsock+))
      (%p1-require entry +p1-right-sock-shutdown+)
      (%p1-check (%libc-shutdown host direction))
      +p1-success+)))

;;; ---- poll_oneoff

(defun %p1-fd-poll-mask (entry)
  (let ((mask 0))
    (when (logtest (p1-fd-rights-base entry) +p1-right-fd-read+)
      (setf mask (logior mask +p1-pollin+)))
    (when (logtest (p1-fd-rights-base entry) +p1-right-fd-write+)
      (setf mask (logior mask +p1-pollout+)))
    mask))

(defun %p1-now-ns ()
  (or (%p1-clock-now-ns +p1-clock-monotonic+) 0))

(%p1-fn %p1-poll-oneoff (state in out n nevents-ptr)
  (when (zerop n)
    (%p1-abort +p1-inval+))
  (%p1-check-range state in (* n +p1-size-subscription+))
  (%p1-check-range state out (* n +p1-size-event+))
  (let ((subs '())
        (deadline nil))
    (dotimes (i n)
      (let* ((base (+ in (* i +p1-size-subscription+)))
             (userdata (%p1-u64 state base))
             (type (%p1-u8 state (+ base 8))))
        (case type
          (0
           (let* ((id (%p1-u32 state (+ base 16)))
                  (timeout (%p1-u64 state (+ base 24)))
                  (flags (%p1-u16 state (+ base 40)))
                  (now (%p1-clock-now-ns id)))
             (unless now (%p1-abort +p1-inval+))
             ;; A deadline belongs to ITS clock: an absolute realtime
             ;; timestamp is meaningless against the monotonic clock.
             ;; Normalize to a monotonic DEADLINE here.
             (let* ((delay (if (logtest flags 1)
                               (max 0 (- timeout now))
                               timeout))
                    (deadline-at (+ (%p1-now-ns) delay)))
               (push (list :index i :userdata userdata :type 0
                           :deadline deadline-at)
                     subs)
               (when (or (null deadline) (< deadline-at deadline))
                 (setf deadline deadline-at)))))
          ((1 2)
           (let* ((fd (%p1-u32 state (+ base 16)))
                  (entry (%p1-require (%p1-lookup-fd state fd)
                                      +p1-right-poll-fd-readwrite+)))
             (push (list :index i :userdata userdata :type type
                         :fd fd :entry entry
                         ;; REVENTS is present from the start: later
                         ;; `(setf (getf s :revents) ...)' mutates the
                         ;; shared plist cell. Adding a missing key would
                         ;; only rebind the loop-local variable.
                         :revents 0
                         ;; a CL stream has no host fd to poll: the
                         ;; embedder handed us a stream, so readiness is
                         ;; "data is available now" — report it ready
                         ;; rather than block on a negative fd.
                         :stream-p (eq (p1-fd-kind entry) :stream))
                   subs)))
          (t (%p1-abort +p1-inval+)))))
    (setf subs (nreverse subs))
    (let* ((fd-subs (remove-if-not (lambda (s)
                                     (and (getf s :fd) (not (getf s :stream-p))))
                                   subs))
           (nfds (length fd-subs))
           (alien (sb-alien:make-alien sb-alien:unsigned-char (max 8 (* 8 nfds)))))
      (unwind-protect
           (let ((sap (sb-alien:alien-sap alien)))
             ;; fill the pollfd array (int fd @0, short events @4)
             (loop for s in fd-subs
                   for pindex from 0
                   for pbase = (* 8 pindex)
                   do (setf (sb-sys:sap-ref-32 sap pbase) (getf s :fd)
                            (sb-sys:sap-ref-16 sap (+ pbase 4))
                            (%p1-fd-poll-mask (getf s :entry))
                            (sb-sys:sap-ref-16 sap (+ pbase 6)) 0))
             (let ((timeout (cond
                              ((some (lambda (s) (getf s :stream-p)) subs) 0)
                              (deadline
                               (min #.(1- (ash 1 31))
                                    (max 0 (ceiling (- deadline (%p1-now-ns))
                                                    1000000))))
                              (t -1))))
               (loop
                 (let ((rc (%libc-poll sap nfds timeout)))
                   (cond ((minusp rc)
                          (if (= (sb-alien:get-errno) 4) ; EINTR
                              (return)
                              (%p1-abort (%p1-host-errno (sb-alien:get-errno)))))
                         (t (return))))))
             ;; revents per fd subscription
             (loop for s in fd-subs
                   for pindex from 0
                   do (setf (getf s :revents)
                            (sb-sys:sap-ref-16 sap (+ (* 8 pindex) 6))))
             (dolist (s subs)
               (when (getf s :stream-p)
                 (setf (getf s :revents) (%p1-fd-poll-mask (getf s :entry)))))
             (let ((nevents (%p1-emit-events state out subs (%p1-now-ns))))
               (setf (%p1-u32 state nevents-ptr) nevents)
               +p1-success+))
        (sb-alien:free-alien alien)))))

(defun %p1-emit-events (state out subs now)
  "Write one event per READY subscription, in subscription order, and
return the count. A clock subscription is ready at its (monotonic
normalized) deadline; an fd subscription when poll reported a revents
(or when the fd is an always-ready CL stream)."
  (let ((nevents 0))
    (dolist (s subs nevents)
      (let ((ready
              (case (getf s :type)
                (0 (and (getf s :deadline) (>= now (getf s :deadline))))
                (t (plusp (or (getf s :revents) 0))))))
        (when ready
          (let ((ptr (+ out (* nevents +p1-size-event+)))
                (type (getf s :type)))
            (setf (%p1-u64 state ptr) (getf s :userdata)
                  (%p1-u16 state (+ ptr 8)) 0
                  (%p1-u8 state (+ ptr 10)) type)
            (if (zerop type)
                (setf (%p1-u64 state (+ ptr 16)) 0
                      (%p1-u16 state (+ ptr 24)) 0)
                (let* ((entry (getf s :entry))
                       (revents (getf s :revents))
                       (nbytes
                         (cond
                           ((logtest revents +p1-pollerr+) 0)
                           ((/= type 1) 0)
                           ((eq (p1-fd-kind entry) :file)
                            (let ((size (nth-value 4 (%p1-fstat (p1-fd-fd entry))))
                                  (pos (%p1-libc-lseek (p1-fd-fd entry) 0 1)))
                              (max 0 (- size (if (minusp pos) 0 pos)))))
                           (t (%p1-fionread (p1-fd-fd entry))))))
                  (setf (%p1-u64 state (+ ptr 16)) nbytes
                        (%p1-u16 state (+ ptr 24))
                        (if (logtest revents +p1-pollhup+)
                            +p1-eventrw-hangup+ 0)))))
          (incf nevents))))))

(defun %p1-fionread (fd)
  "Bytes readable on a socket, 0 when the ioctl cannot answer."
  (let ((alien (sb-alien:make-alien sb-alien:int)))
    (unwind-protect
         (if (zerop (%p1-libc-ioctl fd +fionread+ alien))
             (max 0 (sb-alien:deref alien))
             0)
      (sb-alien:free-alien alien))))

;;; ---------------------------------------------------------------
;;; The import table.
;;;
;;; Every signature here is the pinned WITX one (the validator checks
;;; it against the module's declared import type at link time), so a
;;; wrong table cannot silently bind.

(defparameter *p1-signatures*
  '(("args_get" (:i32 :i32))
    ("args_sizes_get" (:i32 :i32))
    ("environ_get" (:i32 :i32))
    ("environ_sizes_get" (:i32 :i32))
    ("clock_res_get" (:i32 :i32))
    ("clock_time_get" (:i32 :i64 :i32))
    ("fd_advise" (:i32 :i64 :i64 :i32))
    ("fd_allocate" (:i32 :i64 :i64))
    ("fd_close" (:i32))
    ("fd_datasync" (:i32))
    ("fd_fdstat_get" (:i32 :i32))
    ("fd_fdstat_set_flags" (:i32 :i32))
    ("fd_fdstat_set_rights" (:i32 :i64 :i64))
    ("fd_filestat_get" (:i32 :i32))
    ("fd_filestat_set_size" (:i32 :i64))
    ("fd_filestat_set_times" (:i32 :i64 :i64 :i32))
    ("fd_pread" (:i32 :i32 :i32 :i64 :i32))
    ("fd_prestat_get" (:i32 :i32))
    ("fd_prestat_dir_name" (:i32 :i32 :i32))
    ("fd_pwrite" (:i32 :i32 :i32 :i64 :i32))
    ("fd_read" (:i32 :i32 :i32 :i32))
    ("fd_readdir" (:i32 :i32 :i32 :i64 :i32))
    ("fd_renumber" (:i32 :i32))
    ("fd_seek" (:i32 :i64 :i32 :i32))
    ("fd_sync" (:i32))
    ("fd_tell" (:i32 :i32))
    ("fd_write" (:i32 :i32 :i32 :i32))
    ("path_create_directory" (:i32 :i32 :i32))
    ("path_filestat_get" (:i32 :i32 :i32 :i32 :i32))
    ("path_filestat_set_times" (:i32 :i32 :i32 :i32 :i64 :i64 :i32))
    ("path_link" (:i32 :i32 :i32 :i32 :i32 :i32 :i32))
    ("path_open" (:i32 :i32 :i32 :i32 :i32 :i64 :i64 :i32 :i32))
    ("path_readlink" (:i32 :i32 :i32 :i32 :i32 :i32))
    ("path_remove_directory" (:i32 :i32 :i32))
    ("path_rename" (:i32 :i32 :i32 :i32 :i32 :i32))
    ("path_symlink" (:i32 :i32 :i32 :i32 :i32))
    ("path_unlink_file" (:i32 :i32 :i32))
    ("poll_oneoff" (:i32 :i32 :i32 :i32))
    ("proc_exit" (:i32))
    ("proc_raise" (:i32))
    ("sched_yield" ())
    ("random_get" (:i32 :i32))
    ("sock_accept" (:i32 :i32 :i32))
    ("sock_recv" (:i32 :i32 :i32 :i32 :i32 :i32))
    ("sock_send" (:i32 :i32 :i32 :i32 :i32))
    ("sock_shutdown" (:i32 :i32))))

(defparameter *p1-implementations*
  '(("args_get" . %p1-args-get)
    ("args_sizes_get" . %p1-args-sizes-get)
    ("environ_get" . %p1-environ-get)
    ("environ_sizes_get" . %p1-environ-sizes-get)
    ("clock_res_get" . %p1-clock-res-get)
    ("clock_time_get" . %p1-clock-time-get)
    ("fd_advise" . %p1-fd-advise)
    ("fd_allocate" . %p1-fd-allocate)
    ("fd_close" . %p1-fdclose)
    ("fd_datasync" . %p1-fd-datasync)
    ("fd_fdstat_get" . %p1-fd-fdstat-get)
    ("fd_fdstat_set_flags" . %p1-fd-fdstat-set-flags)
    ("fd_fdstat_set_rights" . %p1-fd-fdstat-set-rights)
    ("fd_filestat_get" . %p1-fd-filestat-get)
    ("fd_filestat_set_size" . %p1-fd-filestat-set-size)
    ("fd_filestat_set_times" . %p1-fd-filestat-set-times)
    ("fd_pread" . %p1-fd-pread)
    ("fd_prestat_get" . %p1-fd-prestat-get)
    ("fd_prestat_dir_name" . %p1-fd-prestat-dir-name)
    ("fd_pwrite" . %p1-fd-pwrite)
    ("fd_read" . %p1-fd-read)
    ("fd_readdir" . %p1-fd-readdir)
    ("fd_renumber" . %p1-fd-renumber)
    ("fd_seek" . %p1-fd-seek)
    ("fd_sync" . %p1-fd-sync)
    ("fd_tell" . %p1-fd-tell)
    ("fd_write" . %p1-fd-write)
    ("path_create_directory" . %p1-path-create-directory)
    ("path_filestat_get" . %p1-path-filestat-get)
    ("path_filestat_set_times" . %p1-path-filestat-set-times)
    ("path_link" . %p1-path-link)
    ("path_open" . %p1-path-open)
    ("path_readlink" . %p1-path-readlink)
    ("path_remove_directory" . %p1-path-remove-directory)
    ("path_rename" . %p1-path-rename)
    ("path_symlink" . %p1-path-symlink)
    ("path_unlink_file" . %p1-path-unlink-file)
    ("poll_oneoff" . %p1-poll-oneoff)
    ("proc_exit" . %p1-proc-exit)
    ("proc_raise" . %p1-proc-raise)
    ("sched_yield" . %p1-sched-yield)
    ("random_get" . %p1-random-get)
    ("sock_accept" . %p1-sock-accept)
    ("sock_recv" . %p1-sock-recv)
    ("sock_send" . %p1-sock-send)
    ("sock_shutdown" . %p1-sock-shutdown)))

(defun %p1-preopen-rights (preopen)
  "The (values BASE INHERITING) rights a preopen's EXPLICIT
permissions grant. BASE describes the directory handle itself;
INHERITING is the authority a path_open below it may request."
  (let ((perms (wasi-preopen-permissions preopen)))
    (cond
      ((and (member "write" perms :test #'string=)
            (member "mutate-directory" perms :test #'string=))
       (values +p1-rights-dir-write+
               (logior +p1-rights-dir-write+ +p1-rights-file-write+)))
      ((member "read" perms :test #'string=)
       (values +p1-rights-dir-read-only+
               (logior +p1-rights-dir-read-only+ +p1-rights-file-read-only+)))
      (t (values 0 0)))))

(defun %p1-install-stdio (state context)
  "Fill the stdio slots from the context's EXPLICIT streams. A stream
the context does not grant leaves its fd invalid."
  (flet ((install (fd stream rights)
           (when stream
             (setf (gethash fd (preview1-state-fds state))
                   (%make-p1-fd :kind :stream :stream stream
                               :rights-base rights
                               :rights-inheriting rights
                               :owned-p nil
                               :filetype +p1-file-character-device+)))))
    (install 0 (wasi-context-stdin context)
             (logior +p1-right-fd-read+ +p1-right-fd-fdstat-set-flags+
                     +p1-right-fd-filestat-get+ +p1-right-poll-fd-readwrite+))
    (install 1 (wasi-context-stdout context)
             (logior +p1-right-fd-write+ +p1-right-fd-fdstat-set-flags+
                     +p1-right-fd-filestat-get+ +p1-right-poll-fd-readwrite+))
    (install 2 (wasi-context-stderr context)
             (logior +p1-right-fd-write+ +p1-right-fd-fdstat-set-flags+
                     +p1-right-fd-filestat-get+ +p1-right-poll-fd-readwrite+))))

(defun %p1-install-preopens (state context)
  (loop for pre in (wasi-context-preopens context)
        for fd from 3
        do (multiple-value-bind (base inheriting) (%p1-preopen-rights pre)
             (setf (gethash fd (preview1-state-fds state))
                   (%make-p1-fd :fd (wasi-preopen-fd pre)
                               :kind :file
                               :preopen (wasi-preopen-fd pre)
                               :name (wasi-preopen-name pre)
                               :rights-base base
                               :rights-inheriting inheriting
                               :owned-p nil
                               :dir-p t
                               :filetype +p1-file-directory+)))))

(defun %p1-install-sockets (state sockets)
  "SOCKETS is an explicit fd -> native-capability alist (an integer
host socket fd). No socket is ever created or imported implicitly."
  (dolist (pair sockets)
    (let ((fd (car pair))
          (native (cdr pair)))
      (unless (integerp fd)
        (error 'wasmdown:host-error
               :message (format nil "Preview1 socket table: ~s is not a guest fd" fd)))
      (unless (integerp native)
        (error 'wasmdown:host-error
               :message (format nil "Preview1 socket table: fd ~d has no native socket" fd)))
      (setf (gethash fd (preview1-state-fds state))
            (%make-p1-fd :fd native :kind :socket
                        :rights-base (logior +p1-right-fd-read+
                                             +p1-right-fd-write+
                                             +p1-right-fd-fdstat-set-flags+
                                             +p1-right-fd-filestat-get+
                                             +p1-right-poll-fd-readwrite+
                                             +p1-right-sock-shutdown+
                                             +p1-right-sock-accept+
                                             +p1-right-fd-seek+
                                             +p1-right-fd-tell+)
                        :rights-inheriting 0
                        :filetype +p1-file-socket-stream+)))))

(defun make-preview1-imports (state)
  "The `wasi_snapshot_preview1' import table for STATE."
  (let ((table (wasmdown:make-imports)))
    (dolist (sig *p1-signatures*)
      (let* ((name (car sig))
             (params (second sig))
             (impl (cdr (assoc name *p1-implementations* :test #'string=))))
        (unless (or (functionp impl)
                    (and (symbolp impl) (fboundp impl)))
          (error 'wasmdown:host-error
                 :message (format nil "Preview1 import ~a has no implementation" name)))
        (wasmdown:imports-set-function
         table "wasi_snapshot_preview1" name
         (lambda (&rest args) (apply impl state args))
         params
         (if (string= name "proc_exit") '() '(:i32)))))
    table))

;;; ---------------------------------------------------------------
;;; Instance construction and the command runner.

(defun %p1-attach-memory (state instance)
  "Bind the ABI memory. The module must export `memory' as a memory32;
an imported memory the guest does not re-export is accepted when it is
the instance's only memory. Runs in BEFORE-START, so the start section
can already call into the host."
  (let* ((exports (wasmdown.runtime:module-instance-exports instance))
         (exported (cdr (assoc "memory" exports :test #'string=)))
         (memories (wasmdown.runtime:module-instance-memories instance))
         (memory (cond ((wasmdown.runtime:memory-instance-p exported) exported)
                       ((and (= (length memories) 1)
                             (wasmdown.runtime:memory-instance-p (aref memories 0)))
                        (aref memories 0))
                       (t nil))))
    (unless memory
      (error 'wasmdown:instantiation-error
             :message "WASI Preview1 module must export `memory' (a memory32)"))
    (when (wasmdown.runtime:memory-instance-i64-p memory)
      (error 'wasmdown:instantiation-error
             :message "WASI Preview1 requires a memory32, not memory64"))
    (setf (preview1-state-memory state) memory)
    memory))

(defun make-preview1-instance (module context &key (mode :interpret) imports sockets)
  "Instantiate a core MODULE with the complete `wasi_snapshot_preview1'
import surface bound to CONTEXT's capabilities. Returns
(values INSTANCE STATE). IMPORTS (an imports table or raw alist) may
add non-WASI imports; a conflicting WASI name is a link-error rather
than a silent override. SOCKETS is the explicit fd -> native socket
alist. MODE is :INTERPRET (default) or :COMPILED."
  (let* ((state (%make-preview1-state :context context :module module
                                      :sockets sockets))
         (table (make-preview1-imports state)))
    ;; caller imports: only non-Preview1 names may be added
    (let ((entries (cond ((null imports) '())
                         ((wasmdown::imports-table-p imports)
                          (wasmdown::imports-table-entries imports))
                         (t imports))))
      (dolist (pair entries)
        (when (and (consp (car pair))
                   (string= (first (car pair)) "wasi_snapshot_preview1"))
          (error 'wasmdown:link-error
                 :message (format nil "caller import ~a conflicts with the Preview1 provider"
                                  (car pair))))
        (push pair (wasmdown::imports-table-entries table))))
    (setf (wasmdown::imports-table-entries table)
          (nreverse (wasmdown::imports-table-entries table)))
    (%p1-install-stdio state context)
    (%p1-install-preopens state context)
    (%p1-install-sockets state sockets)
    ;; MODE is installed in the PREPARE hook, not after instantiation:
    ;; PREPARE runs with every object allocated and no guest code yet, so
    ;; in :COMPILED the START SECTION itself runs compiled. Installing
    ;; afterwards would silently interpret the start section.
    (ecase mode
      (:interpret)
      (:compiled))
    (let ((inst (wasmdown.runtime:make-module-instance
                 module
                 (wasmdown::imports-table-entries table)
                 :before-start (lambda (instance)
                                 (%p1-attach-memory state instance))
                 :prepare (when (eq mode :compiled)
                            (lambda (instance)
                              (wasmdown::%install-compiled! module instance))))))
      (setf (preview1-state-instance state) inst)
      (values inst state))))

(defun close-preview1-state (state)
  "Release the host handles the STATE owns (opened descriptors, never
the preopens or stdio streams the context lends). Idempotent."
  (when (and (preview1-state-p state)
             (not (preview1-state-closed-p state)))
    (setf (preview1-state-closed-p state) t)
    (maphash (lambda (fd entry)
               (declare (ignore fd))
               (%p1-close-entry entry))
             (preview1-state-fds state))
    (clrhash (preview1-state-fds state)))
  state)

(defun %p1-export (instance name)
  (cdr (assoc name (wasmdown.runtime:module-instance-exports instance)
              :test #'string=)))

(defun %p1-invoke (instance func-export &rest args)
  (let ((f (%p1-export instance func-export)))
    (unless (wasmdown.runtime:func-instance-p f)
      (error 'wasmdown:link-error
             :message (format nil "WASI Preview1 module exports no `~a' function"
                              func-export)))
    (apply #'wasmdown.runtime:wasm-call f args)))

(defun run-preview1 (module context &key (mode :interpret) imports sockets)
  "Run a Preview1 COMMAND module: validate `_start' as () -> (), then
instantiate and call it once. Returns the RAW u32 exit status (the CLI
truncates it to eight bits). A `proc_exit' anywhere in the guest
produces the status; traps and link failures propagate."
  (wasmdown.validate:validate-module module)
  (multiple-value-bind (params results)
      (%p1-module-export-functype module "_start")
    (unless (and (null params) (null results))
      (error 'wasmdown:link-error
             :message "WASI Preview1 `_start' must have type () -> ()")))
    (multiple-value-bind (instance state)
        (make-preview1-instance module context :mode mode
                                            :imports imports :sockets sockets)
      (unwind-protect
           (handler-case
               (progn
                 (%p1-invoke instance "_start")
                 0)
             (wasi-exit (c) (wasi-exit-status c)))
        (close-preview1-state state))))

(defun %p1-module-export-functype (module name)
  "The (values param-types result-types) of the EXPORT NAME, or a
link-error when the module does not export it as a function."
  (let ((export (find name (wasmdown.types:wasm-module-exports module)
                      :test #'string= :key #'wasmdown.types:wasm-export-name)))
    (unless (and export (eq (wasmdown.types:wasm-export-kind export) :func))
      (error 'wasmdown:link-error
             :message (format nil "WASI Preview1 command module exports no `~a' function"
                              name)))
    (let* ((fidx (wasmdown.types:wasm-export-index export))
           (imported (remove-if-not (lambda (i)
                                      (eq (wasmdown.types:wasm-import-kind i) :func))
                                    (coerce (wasmdown.types:wasm-module-imports module)
                                            'list)))
           (nimp (length imported))
           (ftype
             (if (< fidx nimp)
                 (svref (wasmdown.types:wasm-module-types module)
                        (wasmdown.types:wasm-import-type (nth fidx imported)))
                 (let ((fn (svref (wasmdown.types:wasm-module-funcs module)
                                  (- fidx nimp))))
                   (svref (wasmdown.types:wasm-module-types module)
                          (wasmdown.types:wasm-func-typeidx fn))))))
      (values (wasmdown.types:wasm-func-type-params ftype)
              (wasmdown.types:wasm-func-type-results ftype)))))


