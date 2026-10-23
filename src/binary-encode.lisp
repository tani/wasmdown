;;;; Binary encoder: wasm-module -> (vector (unsigned-byte 8)).
;;;; Inverse of src/binary.lisp's decoder; lives in the same package so
;;;; the already-exported ENCODE-MODULE symbol resolves (CONTRACTS.md).
;;;;
;;;; Emission policy:
;;;; - Canonical minimal LEB128 (u32/s33/s64) everywhere.
;;;; - Known sections in ascending id order (1..12); custom sections
;;;;   last.  DataC (12) must follow Code (10) and Data (11) because
;;;;   the decoder enforces ascending section ids.
;;;; - memarg: bit-6 + memidx emitted only when the decoder stored
;;;;   (:memidx . n) in the memarg plist; else plain align/offset.
;;;; - Data-count section emitted iff the module's data-count slot is
;;;;   non-nil or any data segment exists (bulk memory needs it).
;;;;
;;;; Table-driven simple opcodes are emitted by reverse lookup of the
;;;; decoder's own *simple-ops* table, keeping encoder and decoder in
;;;; lockstep (the table is the single source of truth for those
;;;; bytes).  Ops with dedicated decoder clauses emit canonical bytes.

(in-package #:wasmdown.binary)

;;; ---------------------------------------------------------------
;;; Byte accumulation.

(deftype octet () '(unsigned-byte 8))

(defstruct (encoder (:constructor make-encoder ())
                   (:copier nil)
                   (:predicate nil))
  (buf (make-array 64 :element-type 'octet :fill-pointer 0
                   :adjustable t)))

(declaim (inline enc-byte))
(defun enc-byte (e b)
  (vector-push-extend (the octet b) (encoder-buf e)))

(defun enc-bytes (e bytes)
  (loop for b across bytes do (enc-byte e b)))

(defun encoder->vector (e)
  "Snapshot the encoder buffer as a simple octet vector."
  (let* ((buf (encoder-buf e))
         (out (make-array (length buf) :element-type 'octet)))
    (replace out buf)
    out))

(defun with-encoder (fn)
  "Run FN (of the encoder) on a fresh encoder; return its bytes."
  (let ((tmp (make-encoder)))
    (funcall fn tmp)
    (encoder->vector tmp)))

;;; ---------------------------------------------------------------
;;; Canonical minimal LEB128.

(defun enc-uleb (e n)
  "Canonical minimal unsigned LEB128 of the non-negative integer N."
  (loop
    (let ((b (logand n #x7F)))
      (setf n (ash n -7))
      (if (zerop n)
          (progn (enc-byte e b) (return))
          (enc-byte e (logior b #x80))))))

(defun enc-sleb (e n)
  "Canonical minimal signed LEB128 of the integer N."
  (loop
    (let ((b (logand n #x7F)))
      (setf n (ash n -7))
      (if (or (and (zerop n) (not (logtest b #x40)))
              (and (= n -1) (logtest b #x40)))
          (progn (enc-byte e b) (return))
          (enc-byte e (logior b #x80))))))

(declaim (inline enc-u32 enc-s32 enc-s33 enc-s64))
(defun enc-u32 (e n) (enc-uleb e n))
(defun enc-s32 (e n) (enc-sleb e n))
(defun enc-s33 (e n) (enc-sleb e n))
(defun enc-s64 (e n) (enc-sleb e n))

;;; ---------------------------------------------------------------
;;; Names (UTF-8).

(defun utf8-encode-name (s)
  "UTF-8 encode string S into an octet vector."
  (let ((buf (make-array (max 4 (* 4 (length s)))
                         :element-type 'octet :fill-pointer 0
                         :adjustable t)))
    (flet ((push3 (b0 b1 b2)
             (vector-push-extend b0 buf)
             (vector-push-extend b1 buf)
             (vector-push-extend b2 buf))
           (push4 (b0 b1 b2 b3)
             (vector-push-extend b0 buf)
             (vector-push-extend b1 buf)
             (vector-push-extend b2 buf)
             (vector-push-extend b3 buf)))
      (loop for ch across s
            for cp = (char-code ch)
            do (cond ((< cp #x80) (vector-push-extend cp buf))
                     ((< cp #x800)
                      (vector-push-extend (logior #xC0 (ash cp -6)) buf)
                      (vector-push-extend (logior #x80 (logand cp #x3F)) buf))
                     ((< cp #x10000)
                      (push3 (logior #xE0 (ash cp -12))
                             (logior #x80 (logand (ash cp -6) #x3F))
                             (logior #x80 (logand cp #x3F))))
                     (t
                      (push4 (logior #xF0 (ash cp -18))
                             (logior #x80 (logand (ash cp -12) #x3F))
                             (logior #x80 (logand (ash cp -6) #x3F))
                             (logior #x80 (logand cp #x3F)))))))
    (coerce buf '(vector octet))))

(defun enc-name (e s)
  "Emit a UTF-8 name: u32 byte length + bytes."
  (let ((v (utf8-encode-name s)))
    (enc-u32 e (length v))
    (enc-bytes e v)))

;;; ---------------------------------------------------------------
;;; Value types, limits, blocktypes.

(defun enc-heaptype (e heap)
  "A heap type: abstract keyword -> its single byte (the absheaptype
encodings; a minimal negative s33), (:TYPE N) typeuse -> positive
s33 N."
  (etypecase heap
    (keyword
     (enc-byte e (ecase heap
                   (:func #x70) (:extern #x6f) (:exn #x69)
                   (:any #x6e) (:none #x71)
                   (:nofunc #x73) (:noextern #x72) (:noexn #x74))))
    (cons (enc-s33 e (second heap)))))

(defun enc-reftype (e t_)
  "A reference type: legacy keywords as their shorthand bytes
(= (ref null ht)), typed refs as 0x64 (ref ht) / 0x63 (ref null ht)
plus the heap type."
  (etypecase t_
    (keyword (enc-byte e (%valtype-byte t_)))
    (cons
     (if (eq (second t_) 'null)
         (progn (enc-byte e #x63)
                (enc-heaptype e (third t_)))
         (progn (enc-byte e #x64)
                (enc-heaptype e (second t_)))))))

(defun %valtype-byte (t_)
  (etypecase t_
    (keyword
     (ecase t_
       (:i32 #x7F) (:i64 #x7E) (:f32 #x7D) (:f64 #x7C)
       (:funcref #x70) (:externref #x6F)
       ;; EH (Core 3.0): exnref 0x69.
       (:exnref #x69)))
    ;; typed refs are NOT single bytes: ENC-VALTYPE dispatches them
    ;; to ENC-REFTYPE; the etypecase's one-line fallthrough keeps an
    ;; accidental bare call a compile-time error instead of a silent
    ;; wrong byte.
    (cons (error "use enc-reftype for typed refs"))))

(defun enc-valtype (e t_)
  "A valtype: numerics/legacy refs as one byte, typed refs via
ENC-REFTYPE (0x63/0x64 + heap type)."
  (if (and (consp t_) (eq (first t_) :ref))
      (enc-reftype e t_)
      (enc-byte e (%valtype-byte t_))))

(defun enc-blocktype (e bt)
  "NIL -> 0x40; valtype -> its bytes (typed refs included);
\(:typeidx . n) -> s33 n."
  (cond ((null bt) (enc-byte e #x40))
        ((and (consp bt) (eq (first bt) :typeidx))
         (enc-s33 e (cdr bt)))
        (t (enc-valtype e bt))))

(defun enc-limits (e limits)
  (if (wasm-limits-has-max-p limits)
      (progn (enc-byte e 1)
             (enc-u32 e (wasm-limits-min limits))
             (enc-u32 e (wasm-limits-max limits)))
      (progn (enc-byte e 0)
             (enc-u32 e (wasm-limits-min limits)))))

;;; ---------------------------------------------------------------
;;; Reverse lookup of the decoder's simple-op table.

(defparameter *op->byte* (make-hash-table))

(defun build-op->byte ()
  (loop for byte below (length *simple-ops*)
        for op = (svref *simple-ops* byte)
        when (and op (not (gethash op *op->byte*)))
          do (setf (gethash op *op->byte*) byte))
  *op->byte*)

(build-op->byte)

(defun simple-op-byte (op)
  "Byte for a table-driven simple op (from the decoder's own table), or nil."
  (gethash op *op->byte*))

;;; Canonical bytes for ops with dedicated decoder clauses.  Used when
;;; the op byte is not table-driven in the decoder (loads/stores are
;;; explicit clauses reading memarg; control/vars/consts/reftypes/bulk
;;; have their own clauses).

(defparameter *explicit-op->byte*
  (list
   (cons :block #x02) (cons :loop #x03) (cons :if #x04)
   (cons :br #x0C) (cons :br-if #x0D) (cons :br-table #x0E)
   (cons :return #x0F) (cons :call #x10) (cons :call-indirect #x11)
   (cons :return-call #x12) (cons :return-call-indirect #x13)
   ;; EH (Core 3.0): throw 0x08, try_table 0x1F.
   (cons :throw #x08) (cons :try-table #x1F)
   (cons :select #x1B)
   (cons :local.get #x20) (cons :local.set #x21) (cons :local.tee #x22)
   (cons :global.get #x23) (cons :global.set #x24)
   (cons :table.get #x25) (cons :table.set #x26)
   (cons :memory.size #x3F) (cons :memory.grow #x40)
   (cons :i32.const #x41) (cons :i64.const #x42)
   (cons :f32.const #x43) (cons :f64.const #x44)
   (cons :ref.null #xD0) (cons :ref.is-null #xD1) (cons :ref.func #xD2)
   ;; loads/stores (memarg ops) — canonical bytes
   (cons :i32.load #x28) (cons :i64.load #x29)
   (cons :f32.load #x2A) (cons :f64.load #x2B)
   (cons :i32.load8_s #x2C) (cons :i32.load8_u #x2D)
   (cons :i32.load16_s #x2E) (cons :i32.load16_u #x2F)
   (cons :i64.load8_s #x30) (cons :i64.load8_u #x31)
   (cons :i64.load16_s #x32) (cons :i64.load16_u #x33)
   (cons :i64.load32_s #x34) (cons :i64.load32_u #x35)
   (cons :i32.store #x36) (cons :i64.store #x37)
   (cons :f32.store #x38) (cons :f64.store #x39)
   (cons :i32.store8 #x3A) (cons :i32.store16 #x3B)
   (cons :i64.store8 #x3C) (cons :i64.store16 #x3D)
   (cons :i64.store32 #x3E)))

(defun explicit-op-byte (op)
  (cdr (assoc op *explicit-op->byte*)))

;;; 0xFC sub-opcode table (sat trunc + bulk memory/table ops).
(defparameter *fc-op->sub*
  (list
   (cons :i32.trunc_sat_f32_s 0) (cons :i32.trunc_sat_f32_u 1)
   (cons :i32.trunc_sat_f64_s 2) (cons :i32.trunc_sat_f64_u 3)
   (cons :i64.trunc_sat_f32_s 4) (cons :i64.trunc_sat_f32_u 5)
   (cons :i64.trunc_sat_f64_s 6) (cons :i64.trunc_sat_f64_u 7)
   (cons :memory.init 8) (cons :data.drop 9)
   (cons :memory.copy 10) (cons :memory.fill 11)
   (cons :table.init 12) (cons :elem.drop 13)
   (cons :table.copy 14) (cons :table.grow 15)
   (cons :table.size 16) (cons :table.fill 17)))

;;; The set of memarg ops (dedicated decoder clause reading a memarg).
(defparameter *memarg-ops*
  '(:i32.load :i64.load :f32.load :f64.load
    :i32.load8_s :i32.load8_u :i32.load16_s :i32.load16_u
    :i64.load8_s :i64.load8_u :i64.load16_s :i64.load16_u
    :i64.load32_s :i64.load32_u
    :i32.store :i64.store :f32.store :f64.store
    :i32.store8 :i32.store16 :i64.store8 :i64.store16 :i64.store32))

;;; Ops with dedicated clauses that take a table/func/label index arg.
(defparameter *u32-arg-op->byte*
  (list
   (cons :br #x0C) (cons :br-if #x0D)
   (cons :call #x10)
   (cons :local.get #x20) (cons :local.set #x21) (cons :local.tee #x22)
   (cons :global.get #x23) (cons :global.set #x24)
   (cons :table.get #x25) (cons :table.set #x26)
   (cons :ref.func #xD2)))

;;; ---------------------------------------------------------------
;;; Instruction encoding.

(defun enc-memarg (e imms)
  "Emit a memarg from the decoded plist form.  (:memidx . n) present
-> align with bit 6 set + memidx + offset; else align + offset."
  (let* ((align (getf imms :align))
         (offset (getf imms :offset))
         (memidx (getf imms :memidx)))
    (if memidx
        (progn (enc-u32 e (logior align #x40))
               (enc-u32 e memidx)
               (enc-u32 e offset))
        (progn (enc-u32 e align)
               (enc-u32 e offset)))))

(defun enc-f32-bits (e bits)
  "Emit 4 little-endian bytes of the f32 bit pattern (u32 domain)."
  (enc-byte e (logand (ash bits 0) #xFF))
  (enc-byte e (logand (ash bits -8) #xFF))
  (enc-byte e (logand (ash bits -16) #xFF))
  (enc-byte e (logand (ash bits -24) #xFF)))

(defun enc-f64-bits (e bits)
  "Emit 8 little-endian bytes of the f64 bit pattern (u64 domain):
low word first, then high word."
  (loop for shift from 0 below 64 by 8
        do (enc-byte e (logand (ash bits (- shift)) #xFF))))

(defun enc-instr (e instr)
  "Emit one wasm-instr (including nested bodies and terminators)."
  (let* ((op (wasm-instr-op instr))
         (imms (wasm-instr-imms instr))
         (body (wasm-instr-body instr)))
    (case op
      (:end (enc-byte e #x0B))
      (:else (enc-byte e #x05))
      ((:block :loop :if)
       (enc-byte e (ecase op (:block #x02) (:loop #x03) (:if #x04)))
       (enc-blocktype e (getf imms :bt))
       (enc-instrs e body)
       (when (and (eq op :if) (getf imms :else-body))
         (enc-byte e #x05)
         (enc-instrs e (getf imms :else-body)))
       (enc-byte e #x0B))
      (:br (enc-byte e #x0C) (enc-u32 e (getf imms :label)))
      (:br-if (enc-byte e #x0D) (enc-u32 e (getf imms :label)))
      (:br-table
       (enc-byte e #x0E)
       (enc-u32 e (length (getf imms :targets)))
       (loop for x across (getf imms :targets) do (enc-u32 e x))
       (enc-u32 e (getf imms :default)))
      (:call (enc-byte e #x10) (enc-u32 e (getf imms :func)))
      (:call-indirect
       (enc-byte e #x11)
       (enc-u32 e (getf imms :type))
       (enc-u32 e (getf imms :table)))
      (:return-call (enc-byte e #x12) (enc-u32 e (getf imms :func)))
      (:return-call-indirect
       (enc-byte e #x13)
       (enc-u32 e (getf imms :type))
       (enc-u32 e (getf imms :table)))
      ;; EH (Core 3.0): throw 0x08 (tagidx), try_table 0x1F
      ;; (blocktype, catch clause vector, body, end). throw_ref 0x0A
      ;; is a table-driven simple op (the decoder's *SIMPLE-OPS*).
      (:throw (enc-byte e #x08) (enc-u32 e (getf imms :tag)))
      (:try-table
       (enc-byte e #x1F)
       (enc-blocktype e (getf imms :bt))
       (let ((catches (getf imms :catches)))
         (enc-u32 e (length catches))
         (loop for c across catches
               do (case (first c)
                    (:catch (enc-byte e #x00)
                            (enc-u32 e (second c))
                            (enc-u32 e (third c)))
                    (:catch-ref (enc-byte e #x01)
                                (enc-u32 e (second c))
                                (enc-u32 e (third c)))
                    (:catch-all (enc-byte e #x02) (enc-u32 e (second c)))
                    (:catch-all-ref (enc-byte e #x03)
                                    (enc-u32 e (second c))))))
       (enc-instrs e body)
       (enc-byte e #x0B))
      (:select
       (if (getf imms :types)
           (progn (enc-byte e #x1C)
                  (enc-u32 e (length (getf imms :types)))
                  (loop for t_ across (getf imms :types) do (enc-valtype e t_)))
           (enc-byte e #x1B)))
      ((:local.get :local.set :local.tee)
       (enc-byte e (ecase op (:local.get #x20) (:local.set #x21) (:local.tee #x22)))
       (enc-u32 e (getf imms :local)))
      ((:global.get :global.set)
       (enc-byte e (if (eq op :global.get) #x23 #x24))
       (enc-u32 e (getf imms :global)))
      ((:table.get :table.set)
       (enc-byte e (if (eq op :table.get) #x25 #x26))
       (enc-u32 e (getf imms :table)))
      ((:i32.load :i64.load :f32.load :f64.load
        :i32.load8_s :i32.load8_u :i32.load16_s :i32.load16_u
        :i64.load8_s :i64.load8_u :i64.load16_s :i64.load16_u
        :i64.load32_s :i64.load32_u
        :i32.store :i64.store :f32.store :f64.store
        :i32.store8 :i32.store16 :i64.store8 :i64.store16 :i64.store32)
       (enc-byte e (explicit-op-byte op))
       (enc-memarg e imms))
      ((:memory.size :memory.grow)
       (enc-byte e (if (eq op :memory.size) #x3F #x40))
       (enc-u32 e (getf imms :memidx)))
      (:i32.const (enc-byte e #x41) (enc-s32 e (getf imms :value)))
      (:i64.const (enc-byte e #x42) (enc-s64 e (getf imms :value)))
      (:f32.const (enc-byte e #x43) (enc-f32-bits e (getf imms :bits)))
      (:f64.const (enc-byte e #x44) (enc-f64-bits e (getf imms :bits)))
      (:ref.null
       (enc-byte e #xD0)
       ;; the imm is a HEAP type: legacy shorthand keywords
       ;; (:funcref/:externref/:exnref), the lattice's abstract
       ;; keywords, or (:TYPE N) — all via ENC-HEAPTYPE's etypecase
       ;; (the legacy keywords map to their = (ref null ht) bytes).
       (let ((ht (getf imms :type)))
         (etypecase ht
           (keyword
            (enc-byte e (ecase ht
                          (:funcref #x70) (:externref #x6f)
                          (:exnref #x69)
                          (:func #x70) (:extern #x6f) (:exn #x69)
                          (:any #x6e) (:none #x71)
                          (:nofunc #x73) (:noextern #x72)
                          (:noexn #x74))))
           (cons (enc-heaptype e ht)))))
      (:ref.is-null (enc-byte e #xD1))
      (:ref.func (enc-byte e #xD2) (enc-u32 e (getf imms :func)))
      ;; typed function references (Core 3.0): ref.as_non_null 0xD4,
      ;; br_on_null 0xD5 / br_on_non_null 0xD6 (labelidx),
      ;; call_ref 0x14 / return_call_ref 0x15 (typeidx).
      (:ref.as-non-null (enc-byte e #xD4))
      (:br-on-null (enc-byte e #xD5) (enc-u32 e (getf imms :label)))
      (:br-on-non-null (enc-byte e #xD6) (enc-u32 e (getf imms :label)))
      (:call-ref (enc-byte e #x14) (enc-u32 e (getf imms :type)))
      (:return-call-ref (enc-byte e #x15) (enc-u32 e (getf imms :type)))
      ((:i32.trunc_sat_f32_s :i32.trunc_sat_f32_u
        :i32.trunc_sat_f64_s :i32.trunc_sat_f64_u
        :i64.trunc_sat_f32_s :i64.trunc_sat_f32_u
        :i64.trunc_sat_f64_s :i64.trunc_sat_f64_u
        :memory.init :data.drop :memory.copy :memory.fill
        :table.init :elem.drop :table.copy :table.grow
        :table.size :table.fill)
       (enc-byte e #xFC)
       (enc-u32 e (cdr (assoc op *fc-op->sub*)))
       (case op
         (:memory.init (enc-u32 e (getf imms :data)) (enc-u32 e (getf imms :memidx)))
         (:data.drop (enc-u32 e (getf imms :data)))
         (:memory.copy (enc-u32 e (getf imms :dst)) (enc-u32 e (getf imms :src)))
         (:memory.fill (enc-u32 e (getf imms :memidx)))
         (:table.init (enc-u32 e (getf imms :elem)) (enc-u32 e (getf imms :table)))
         (:elem.drop (enc-u32 e (getf imms :elem)))
         (:table.copy (enc-u32 e (getf imms :dst)) (enc-u32 e (getf imms :src)))
         (:table.grow (enc-u32 e (getf imms :table)))
         (:table.size (enc-u32 e (getf imms :table)))
         (:table.fill (enc-u32 e (getf imms :table)))))
      (t
       (let ((byte (simple-op-byte op)))
         (if byte
             (enc-byte e byte)
             (error "encode: unknown opcode ~a" op)))))))

(defun enc-instrs (e instrs)
  "Emit a vector of instructions."
  (loop for i across instrs do (enc-instr e i)))

;;; ---------------------------------------------------------------
;;; Sections.

(defun enc-section (e id content-fn)
  "Emit a known section: id byte + u32 size + content."
  (let ((content (with-encoder content-fn)))
    (enc-byte e id)
    (enc-u32 e (length content))
    (enc-bytes e content)))

(defun enc-vec (e seq fn)
  "Emit a length-prefixed sequence via FN (encoder, element)."
  (enc-u32 e (length seq))
  (loop for x across seq do (funcall fn e x)))

;;; ---------------------------------------------------------------
;;; Section encoders.

(defun enc-type-section (e types)
  (enc-section e 1
    (lambda (e)
      (enc-u32 e (length types))
      (loop for ft across types
            do (enc-byte e #x60)
               (enc-u32 e (length (wasm-func-type-params ft)))
               (loop for t_ in (wasm-func-type-params ft) do (enc-valtype e t_))
               (enc-u32 e (length (wasm-func-type-results ft)))
               (loop for t_ in (wasm-func-type-results ft) do (enc-valtype e t_))))))

(defun enc-import-section (e imports)
  (enc-section e 2
    (lambda (e)
      (enc-u32 e (length imports))
      (loop for imp across imports
            do (enc-name e (wasm-import-module imp))
               (enc-name e (wasm-import-field imp))
               (enc-byte e (ecase (wasm-import-kind imp)
                             (:func 0) (:table 1) (:memory 2) (:global 3)
                             ;; EH (Core 3.0): tag import kind.
                             (:tag 4)))
               (let ((ty (wasm-import-type imp)))
                 (ecase (wasm-import-kind imp)
                   (:func (enc-u32 e ty))
                   (:table (enc-valtype e (wasm-table-type ty))
                           (enc-limits e (wasm-table-limits ty)))
                   (:memory (enc-limits e (wasm-memory-limits ty)))
                   (:global (enc-valtype e (car ty))
                            (enc-byte e (if (cdr ty) 1 0)))
                   ;; tag payload: typeidx (the functype; empty results
                   ;; enforced at validation).
                   (:tag (enc-u32 e ty))))))))

(defun enc-func-section (e funcs)
  (enc-section e 3
    (lambda (e)
      (enc-u32 e (length funcs))
      (loop for f across funcs do (enc-u32 e (wasm-func-typeidx f))))))

(defun enc-table-section (e tables)
  (enc-section e 4
    (lambda (e)
      (enc-u32 e (length tables))
      (loop for tb across tables
            do (enc-valtype e (wasm-table-type tb))
               (enc-limits e (wasm-table-limits tb))))))

(defun enc-mem-section (e mems)
  (enc-section e 5
    (lambda (e)
      (enc-u32 e (length mems))
      (loop for m across mems do (enc-limits e (wasm-memory-limits m))))))

(defun enc-global-section (e globals)
  (enc-section e 6
    (lambda (e)
      (enc-u32 e (length globals))
(loop for g across globals
      do (enc-valtype e (wasm-global-type g))
         (enc-byte e (if (wasm-global-mutable g) 1 0))
         (enc-instrs e (wasm-global-init g))))))

(defun enc-export-section (e exports)
  (enc-section e 7
    (lambda (e)
      (enc-u32 e (length exports))
      (loop for ex across exports
            do (enc-name e (wasm-export-name ex))
               (enc-byte e (ecase (wasm-export-kind ex)
                             (:func 0) (:table 1) (:memory 2) (:global 3)
                             ;; EH (Core 3.0): tag export kind.
                             (:tag 4)))
               (enc-u32 e (wasm-export-index ex))))))

(defun enc-start-section (e start)
  (enc-section e 8 (lambda (e) (enc-u32 e start))))

(defun elem-init-legacy-funcidx-p (init)
  "Whether INIT is a canonical (ref.func x) end expression, i.e. the
legacy funcidx item form; returns the funcidx or nil."
  (and (= (length init) 2)
       (eq (wasm-instr-op (svref init 0)) :ref.func)
       (eq (wasm-instr-op (svref init 1)) :end)))

(defun elem-init-ref-func-index (init)
  (getf (wasm-instr-imms (svref init 0)) :func))

(defun enc-elem-section (e elems)
  (enc-section e 9
    (lambda (e)
      (enc-u32 e (length elems))
      (loop for el across elems
            do (let ((mode (wasm-elem-mode el))
                     (type (wasm-elem-type el))
                     (inits (wasm-elem-inits el)))
                 (cond
                   ((and (consp mode) (eq (first mode) :active))
                    (let ((tableidx (second mode))
                          (expr (third mode)))
                      (cond
                        ((and (eql tableidx 0)
                              (member type '(:funcref :elemkind-func))
                              (every #'elem-init-legacy-funcidx-p inits))
                         (enc-u32 e 0)      ; flag 0: legacy active
                         (enc-instrs e expr)
                         ;; flags 0: elemkind is implicit, not encoded
                         (enc-u32 e (length inits))
                         (loop for init in inits
                               do (enc-u32 e (elem-init-ref-func-index init))))
                        ((and (eql tableidx 0) (eq type :funcref))
                         (enc-u32 e 4)      ; flag 4: active, elemexpr, funcref
                         (enc-instrs e expr)
                         (enc-u32 e (length inits))
                         (loop for init in inits do (enc-instrs e init)))
                        ((eq type :elemkind-func)
                         ;; elemkind form with a non-zero table index:
                         ;; flag 2 (active, table, elemkind byte 0x00,
                         ;; funcidx vector) — the elemkind form IS the
                         ;; funcidx form binary-wise; no reftype byte.
                         (enc-u32 e 2)
                         (enc-u32 e tableidx)
                         (enc-instrs e expr)
                         (enc-byte e 0)    ; elemkind funcref
                         (enc-u32 e (length inits))
                         (loop for init in inits
                               do (enc-u32 e (elem-init-ref-func-index init))))
                        (t
                         (enc-u32 e 6)      ; flag 6: active, elemexpr, type
                         (enc-u32 e tableidx)
                         (enc-instrs e expr)
                         (enc-valtype e type)
                         (enc-u32 e (length inits))
                         (loop for init in inits do (enc-instrs e init))))))
                   ((equal mode '(:passive))
                    (cond
                      ((and (member type '(:funcref :elemkind-func))
                            (every #'elem-init-legacy-funcidx-p inits))
                       (enc-u32 e 1)
                       (enc-byte e 0)    ; elemkind funcref
                       (enc-u32 e (length inits))
                       (loop for init in inits
                             do (enc-u32 e (elem-init-ref-func-index init))))
                      (t
                       (enc-u32 e 5)
                       (enc-valtype e type)
                       (enc-u32 e (length inits))
                       (loop for init in inits do (enc-instrs e init)))))
                   ((equal mode '(:declarative))
                    (cond
                      ((and (member type '(:funcref :elemkind-func))
                            (every #'elem-init-legacy-funcidx-p inits))
                       (enc-u32 e 3)
                       (enc-byte e 0)    ; elemkind funcref
                       (enc-u32 e (length inits))
                       (loop for init in inits
                             do (enc-u32 e (elem-init-ref-func-index init))))
                      (t
                       (enc-u32 e 7)
                       (enc-valtype e type)
                       (enc-u32 e (length inits))
                       (loop for init in inits do (enc-instrs e init)))))
                   (t (error "encode: bad elem mode ~s" mode))))))))

(defun enc-data-section (e datas)
  (enc-section e 11
    (lambda (e)
      (enc-u32 e (length datas))
      (loop for d across datas
            do (let ((mode (wasm-data-mode d))
                     (bytes (wasm-data-bytes d)))
                 (cond
                   ((and (consp mode) (eq (first mode) :active))
                    (let ((memidx (second mode))
                          (expr (third mode)))
                      (if (eql memidx 0)
                          (progn (enc-u32 e 0)
                                 (enc-instrs e expr)
                                 (enc-u32 e (length bytes))
                                 (enc-bytes e bytes))
                          (progn (enc-u32 e 2)
                                 (enc-u32 e memidx)
                                 (enc-instrs e expr)
                                 (enc-u32 e (length bytes))
                                 (enc-bytes e bytes)))))
                   ((equal mode '(:passive))
                    (enc-u32 e 1)
                    (enc-u32 e (length bytes))
                    (enc-bytes e bytes))
                   (t (error "encode: bad data mode ~s" mode))))))))

(defun enc-datacount-section (e n)
  (enc-section e 12 (lambda (e) (enc-u32 e n))))

(defun enc-tag-section (e tags)
  "The tag section (id 13, EH Core 3.0): a vector of typeidx."
  (enc-section e 13
    (lambda (e)
      (enc-u32 e (length tags))
      (loop for ty across tags do (enc-u32 e ty)))))

(defun enc-code-section (e funcs)
  (enc-section e 10
    (lambda (e)
      (enc-u32 e (length funcs))
      (loop for f across funcs
            do (let ((entry (with-encoder
                              (lambda (e)
                                (enc-u32 e (length (wasm-func-locals f)))
                                (loop for (ty . cnt) in (wasm-func-locals f)
                                      do (enc-u32 e cnt)
                                         (enc-valtype e ty))
                                (enc-instrs e (wasm-func-body f))))))
                 (enc-u32 e (length entry))
                 (enc-bytes e entry))))))

(defun enc-custom-section (e custom)
  "Emit a custom section: id 0 + size + (name: u32 length + bytes) +
payload bytes."
  (let ((content (with-encoder
                   (lambda (e)
                     (enc-name e (wasm-custom-name custom))
                     (enc-bytes e (wasm-custom-bytes custom))))))
    (enc-byte e 0)
    (enc-u32 e (length content))
    (enc-bytes e content)))

;;; ---------------------------------------------------------------
;;; Entry point.

(defun encode-module (m)
  "Encode a wasm-module into a canonical octet vector."
  (let ((e (make-encoder)))
    (enc-bytes e #(0 #x61 #x73 #x6D 1 0 0 0))
    (let ((types (wasm-module-types m))
          (imports (wasm-module-imports m))
          (funcs (wasm-module-funcs m))
          (tables (wasm-module-tables m))
          (mems (wasm-module-mems m))
          (globals (wasm-module-globals m))
          (exports (wasm-module-exports m))
          (start (wasm-module-start m))
          (elems (wasm-module-elems m))
          (datas (wasm-module-datas m))
          (tags (wasm-module-tags m))
          (customs (wasm-module-customs m))
          (data-count (wasm-module-data-count m)))
      (unless (zerop (length types)) (enc-type-section e types))
      (unless (zerop (length imports)) (enc-import-section e imports))
      (unless (zerop (length funcs)) (enc-func-section e funcs))
      (unless (zerop (length tables)) (enc-table-section e tables))
      (unless (zerop (length mems)) (enc-mem-section e mems))
      (unless (zerop (length globals)) (enc-global-section e globals))
      (unless (zerop (length exports)) (enc-export-section e exports))
      (when start (enc-start-section e start))
      (unless (zerop (length elems)) (enc-elem-section e elems))
      ;; Data-count precedes code per spec section order (id 12 < 10 is
      ;; false; the actual rule: DataC id 12 comes after Elem id 9 and
      ;; before Code id 10 — it violates ascending ids, so the decoder's
      ;; ascending check must exempt id 12; here emit in spec position.
      (when (or data-count (plusp (length datas)))
        (enc-datacount-section e (or data-count (length datas))))
      (unless (zerop (length funcs)) (enc-code-section e funcs))
      (unless (zerop (length datas)) (enc-data-section e datas))
      ;; Tag (id 13) comes after Data (id 11) in the canonical section
      ;; order (the decoder's rank table enforces the same).
      (unless (zerop (length tags)) (enc-tag-section e tags))
      (loop for c across customs do (enc-custom-section e c)))
    (encoder->vector e)))
