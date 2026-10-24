(in-package #:wasmdown.binary)

;;; ---------------------------------------------------------------
;;; Errors. decode-error lives in the WASMDOWN package (the public
;;; condition taxonomy); every signal carries an absolute :offset.

(defun decode-error-at (offset format-control &rest args)
  "Signal wasmdown:decode-error at absolute byte OFFSET."
  (error 'wasmdown:decode-error
         :offset offset
         :message (apply #'format nil format-control args)))

(defun %decode-error (r format-control &rest args)
  (apply #'decode-error-at (reader-offset r) format-control args))

;;; ---------------------------------------------------------------
;;; Bounds-checked reader over (vector (unsigned-byte 8)). LIMIT is the
;;; exclusive end of the readable window (absolute); sub-readers over
;;; section/body spans share the same bytes vector, so offsets stay
;;; absolute throughout.

(defstruct (reader (:constructor make-reader (bytes &optional (offset 0) (limit (length bytes))))
                   (:copier nil))
  (bytes #() :type (vector (unsigned-byte 8)))
  (offset 0 :type (integer 0))
  (limit 0 :type (integer 0)))


(declaim (inline %need-bytes))
(defun %need-bytes (r n what)
  (when (> (+ (reader-offset r) n) (reader-limit r))
    (%decode-error r "truncated ~a: ~d byte~:p missing"
                   what (- (+ (reader-offset r) n) (reader-limit r)))))

(defun read-byte-at (r)
  "Read one raw byte; decode-error at R's offset when out of bounds."
  (%need-bytes r 1 "byte")
  (let ((b (aref (reader-bytes r) (reader-offset r))))
    (incf (reader-offset r))
    b))

(defun peek-byte-at (r)
  (%need-bytes r 1 "byte")
  (aref (reader-bytes r) (reader-offset r)))

(defun read-bytes (r n)
  "Read N raw bytes into a fresh simple vector."
  (%need-bytes r n "byte span")
  (let ((v (make-array n :element-type '(unsigned-byte 8)))
        (start (reader-offset r)))
    (replace v (reader-bytes r) :start2 start)
    (incf (reader-offset r) n)
    v))

(defun read-vec (r elem-fn what)
  "Read a u32-counted vector. The count is checked against the number
of bytes remaining before any allocation, so a hostile huge count is
rejected as malformed immediately (every element needs at least one
byte of encoding)."
  (let ((start (reader-offset r))
        (count (read-u32 r)))
    (when (> count (- (reader-limit r) (reader-offset r)))
      (decode-error-at start "length out of bounds: ~a vector declares ~d element~:p"
                       what count))
    (coerce (loop repeat count collect (funcall elem-fn r)) 'vector)))

(defun read-byte-vec (r what)
  "A u32-counted vector of raw bytes, specialized to
(vector (unsigned-byte 8)) — including the EMPTY case, which must not
degrade to a simple-vector (data-seg-state's slot type)."
  (let ((start (reader-offset r))
        (count (read-u32 r)))
    (when (> count (- (reader-limit r) (reader-offset r)))
      (decode-error-at start "length out of bounds: ~a vector declares ~d element~:p"
                       what count))
    (let ((v (make-array count :element-type '(unsigned-byte 8))))
      (loop for i below count
            do (setf (aref v i) (read-byte-at r)))
      v)))

;;; ---------------------------------------------------------------
;;; LEB128. Spec-permitted padded (non-minimal) encodings are accepted
;;; within the width's maximum byte length (5 bytes for u32/s32/s33,
;;; 10 for u64/s64); longer encodings and values that overflow the
;;; width are decode-error. Signed forms sign-extend from the
;;; terminating byte's bit 6. Errors point at the encoding's first byte.

(defparameter *leb-u32-max* 5)
(defparameter *leb-s32-max* 5)
(defparameter *leb-s33-max* 5)
(defparameter *leb-u64-max* 10)
(defparameter *leb-s64-max* 10)

(defun read-uleb (r bits max-bytes what)
  (let ((start (reader-offset r))
        (value 0) (shift 0) (count 0))
    (loop
      (when (>= count max-bytes)
        (decode-error-at start "~a: integer representation too long" what))
      (let ((b (read-byte-at r)))
        (setf value (logior value (ash (logand b #x7f) shift)))
        (incf shift 7)
        (incf count)
        (unless (logtest b #x80)
          (return))
        (when (>= count max-bytes)
          (decode-error-at start "~a: integer representation too long" what))))
    (when (> value (1- (ash 1 bits)))
      (decode-error-at start "~a: integer too large" what))
    value))

(defun read-sleb (r bits max-bytes what)
  (let ((start (reader-offset r))
        (value 0) (shift 0) (count 0) (b 0))
    (loop
      (when (>= count max-bytes)
        (decode-error-at start "~a: integer representation too long" what))
      (setf b (read-byte-at r))
      (setf value (logior value (ash (logand b #x7f) shift)))
      (incf shift 7)
      (incf count)
      (unless (logtest b #x80)
        (return)))
    (when (logtest b #x40)
      (setf value (- value (ash 1 shift))))
    (unless (and (>= value (- (ash 1 (1- bits))))
                 (<= value (1- (ash 1 (1- bits)))))
      (decode-error-at start "~a: integer too large" what))
    value))

(defun read-u32 (r) (read-uleb r 32 *leb-u32-max* "u32"))
(defun read-u64 (r) (read-uleb r 64 *leb-u64-max* "u64"))
(defun read-s32 (r) (read-sleb r 32 *leb-s32-max* "s32"))
(defun read-s33 (r) (read-sleb r 33 *leb-s33-max* "s33"))
(defun read-s64 (r) (read-sleb r 64 *leb-s64-max* "s64"))

;;; ---------------------------------------------------------------
;;; Names: u32 byte length + strict UTF-8. Rejects bad lead bytes,
;;; bad continuation bytes (must be 0x80..0xBF), overlong forms,
;;; surrogate code points and code points above #x10FFFF.

(defun %cont-byte (r pos)
  (when (>= pos (reader-limit r))
    (decode-error-at pos "truncated name: invalid UTF-8 encoding"))
  (let ((b (aref (reader-bytes r) pos)))
    (unless (and (>= b #x80) (<= b #xBF))
      (decode-error-at pos "invalid UTF-8 encoding: bad continuation byte"))
    b))

(defun read-name (r)
  (let* ((start (reader-offset r))
         (len (read-u32 r)))
    (when (> len (- (reader-limit r) (reader-offset r)))
      (decode-error-at start "length out of bounds: name declares ~d byte~:p" len))
    (let ((code-points '())
          (pos (reader-offset r)))
      (declare (type (integer 0) pos))
      (flet ((next ()
               (let ((b (aref (reader-bytes r) pos)))
                 (incf (reader-offset r))
                 (incf pos)
                 b))
             (cont ()
               (let ((b (%cont-byte r pos)))
                 (incf (reader-offset r))
                 (incf pos)
                 b)))
        (let ((end (+ (reader-offset r) len)))
          (loop while (< pos end)
                for b = (next)
                for cp = (cond
                           ((< b #x80) b)
                           ((and (>= b #xC2) (<= b #xDF))
                            (logior (ash (- b #xC0) 6) (- (cont) #x80)))
                           ((and (>= b #xE0) (<= b #xEF))
                            (let ((cp (logior (ash (- b #xE0) 12)
                                              (ash (- (cont) #x80) 6)
                                              (- (cont) #x80))))
                              (cond ((< cp #x800)
                                     (decode-error-at pos "invalid UTF-8 encoding: overlong sequence"))
                                    ((and (>= cp #xD800) (<= cp #xDFFF))
                                     (decode-error-at pos "invalid UTF-8 encoding: surrogate code point"))
                                    (t cp))))
                           ((and (>= b #xF0) (<= b #xF4))
                            (let ((cp (logior (ash (- b #xF0) 18)
                                              (ash (- (cont) #x80) 12)
                                              (ash (- (cont) #x80) 6)
                                              (- (cont) #x80))))
                              (cond ((< cp #x10000)
                                     (decode-error-at pos "invalid UTF-8 encoding: overlong sequence"))
                                    ((> cp #x10FFFF)
                                     (decode-error-at pos "invalid UTF-8 encoding: code point too large"))
                                    (t cp))))
                           (t (decode-error-at pos "invalid UTF-8 encoding")))
                do (push cp code-points)))
        (coerce (mapcar #'code-char (nreverse code-points)) 'string)))))

;;; ---------------------------------------------------------------
;;; Value types, block types, limits, heaptype.
;;;
;;; Typed function references (Core 3.0, the gc-less function-
;;; references subset): valtype bytes 0x64 (ref ht) and 0x63
;;; (ref null ht), heap types as the abstract single bytes (0x70
;;; func .. 0x74 noexn, identical to the absheaptype encodings of
;;; binary/types.html) or a positive s33 typeuse -> (:TYPE N). The
;;; struct/array/i31/eq abstract heaps (0x6A/0x6B/0x6C/0x6D) stay
;;; out of scope: they decode-error here (the wast classifier marks
;;; those modules :unsupported before decode).

(defun %read-valtype (r)
  (let ((b (read-byte-at r)))
    (case b
      (#x7f :i32) (#x7e :i64) (#x7d :f32) (#x7c :f64)
      (#x70 :funcref) (#x6f :externref)
      ;; SIMD (loop 24, binary/types.html valtype): v128 0x7B.
      (#x7b :v128)
      ;; EH (Core 3.0): exnref 0x69 in valtype position
      ;; (ref_null.wast's binary mirror of (ref.null exn)).
      ;; packed storage types (loop 22, binary/types.html
      ;; storagetype): i8 0x78, i16 0x77 — field positions only;
      ;; the VALIDATOR rejects them in value positions.
      (#x78 :i8) (#x77 :i16)
      ;; GC shorthand reftypes (loop 22, binary/types.html): the
      ;; abstract any/eq/i31/struct/array heap bytes 0x6E/0x6D/
      ;; 0x6C/0x6B/0x6A in VALTYPE position are the nullable
      ;; shorthand spellings anyref/eqref/i31ref/structref/
      ;; arrayref = (ref null ht).
      (#x6e (list :ref 'null :any))
      (#x6d (list :ref 'null :eq))
      (#x6c (list :ref 'null :i31))
      (#x6b (list :ref 'null :struct))
      (#x6a (list :ref 'null :array))
      ;; typed refs: 0x64 (ref ht), 0x63 (ref null ht) — the byte
      ;; order pinned by binary/types.html (0x63 = ref null ht,
      ;; 0x64 = ref ht) and elem.wast ::458's "\64\70" = (ref func).
      (#x64 (list :ref (%read-heaptype r)))
      (#x63 (list :ref 'null (%read-heaptype r)))
      (t (%decode-error r "unknown type form 0x~x" b)))))

(defun %read-reftype (r)
  (let ((b (read-byte-at r)))
    (case b
      (#x70 :funcref) (#x6f :externref)
      ;; EH (Core 3.0): table/global element type exnref (0x69).
      ;; GC shorthand reftypes (loop 22): anyref 0x6E / eqref 0x6D /
      ;; i31ref 0x6C / structref 0x6B / arrayref 0x6A in table/elem
      ;; element position (binary/types.html reftype).
      (#x6e (list :ref 'null :any))
      (#x6d (list :ref 'null :eq))
      (#x6c (list :ref 'null :i31))
      (#x6b (list :ref 'null :struct))
      (#x6a (list :ref 'null :array))
      ;; typed refs in table/elem element position: 0x64/0x63
      ;; (elem.wast ::458's table byte stream "\40\00\64\70").
      (#x64 (list :ref (%read-heaptype r)))
      (#x63 (list :ref 'null (%read-heaptype r)))
      (t (%decode-error r "unknown type form 0x~x" b)))))

(defun %read-heaptype (r)
  "A heap type (binary/types.html heaptype): the abstract single
bytes — func 0x70, extern 0x6F, exn 0x69, any 0x6E, none 0x71,
nofunc 0x73, noextern 0x72, noexn 0x74, and (loop 22) the GC
heaps eq 0x6D / i31 0x6C / struct 0x6B / array 0x6A — or a
positive s33 typeuse index -> (:TYPE N). A negative multi-byte s33
\(malformed abstract heap) is a decode error; the single abstract
bytes ARE minimal negative s33 encodings (0x70 = -16, one's
complement), so the byte case catches them before the s33 read."
  (let ((start (reader-offset r))
        (b (peek-byte-at r)))
    (case b
      ;; abstract heap types: single bytes that ARE minimal negative
      ;; s33 encodings (0x70 = -16 = one's complement). Peek then
      ;; consume — the byte case fires only on the exact byte, so the
      ;; s33 read below never sees a partially-consumed LEB.
      (#x70 (read-byte-at r) :func)
      (#x6f (read-byte-at r) :extern)
      (#x69 (read-byte-at r) :exn)
      (#x6e (read-byte-at r) :any)
      ;; the GC heaps (loop 22, binary/types.html): eq 0x6D,
      ;; i31 0x6C, struct 0x6B, array 0x6A.
      (#x6d (read-byte-at r) :eq)
      (#x6c (read-byte-at r) :i31)
      (#x6b (read-byte-at r) :struct)
      (#x6a (read-byte-at r) :array)
      (#x71 (read-byte-at r) :none)
      (#x73 (read-byte-at r) :nofunc)
      (#x72 (read-byte-at r) :noextern)
      (#x74 (read-byte-at r) :noexn)
      (t
       (let ((n (read-s33 r)))
         (if (>= n 0)
             (list :type n)
             ;; binary.wast ::1241's (ref null 0x8000_0000) decodes
             ;; fine here; 'unknown type' surfaces at VALIDATE.
             ;; A negative multi-byte s33 (binary.wast ::1250) is a
             ;; malformed abstract heap: decode error.
             (decode-error-at start "unknown type form 0x~x" b)))))))

(defun %read-blocktype (r)
  "NIL (empty), a valtype, or (:typeidx . n)."
  (let ((b (peek-byte-at r)))
    (cond ((= b #x40) (read-byte-at r) nil)
          ;; the GC shorthand bytes 0x6E..0x6A (loop 22) are valtype
          ;; spellings too — dispatch through %read-valtype.
          ((member b '(#x7f #x7e #x7d #x7c #x70 #x6f #x69 #x63 #x64
                       #x6e #x6d #x6c #x6b #x6a))
           (%read-valtype r))
          (t
           (let ((start (reader-offset r))
                 (n (read-s33 r)))
             (if (>= n 0)
                 (cons :typeidx n)
                 (decode-error-at start "unknown type form 0x~x" b)))))))


(defun %read-limits (r)
  "Limits flags (Core 3.0, spec binary/decode.ml `limits`): bit 0 =
has-max, bit 2 = 64-bit address type (memory64; table64 decodes
but the validator keeps rejecting tables — out of scope). Legal
flag sets exactly {0x00,0x01,0x04,0x05}; min/max read as u64
UNCONDITIONALLY (the spec reads the width-agnostic domain; the
VALIDATOR imposes the per-addrtype page bounds).
(logtest flags #xfa) covers all illegal bits (shared + reserved)."
  (let ((flags (read-byte-at r)))
    (unless (zerop (logand flags #xfa))
      (%decode-error r "malformed limits flags 0x~x" flags))
    (let ((min (read-u64 r))
          (i64-p (logtest flags #x04)))
      (if (logtest flags #x01)
          (make-wasm-limits min (read-u64 r) i64-p)
          ;; no max: the positional constructor's &aux has-max keys
          ;; off SUPPLIED-P — use the explicit constructor
          (wasmdown.types::%make-wasm-limits* min nil nil i64-p)))))

;;; ---------------------------------------------------------------
;;; Simple (immediate-free) opcodes: opcode byte -> op keyword.

(defun %op (r keyword)
  (make-wasm-instr keyword nil #() (reader-offset r)))

(defparameter *simple-ops*
  ;; Index = opcode byte (WebAssembly core binary format). Ops with
  ;; dedicated decoder clauses (block/loop/if/else/end/br/.../consts/
  ;; local.../select-typed/ref.*) also sit at their real bytes here so
  ;; the encoder's reverse lookup stays in lockstep; the decoder never
  ;; consults those entries (its case clauses match first).
  ;; EH (Core 3.0): throw_ref (0x0A) is immediate-free and lives in
  ;; this table; throw (0x08) and try_table (0x1F) carry immediates
  ;; and keep dedicated %READ-INSTR clauses below.
  #(
    :unreachable :nop nil nil nil :else nil nil   ; 0x00-0x07
    nil nil :throw-ref nil :br :br-if :br-table :return   ; 0x08-0x0F (0x0A throw_ref, EH)
    :call :call-indirect :call-ref :return-call-ref nil nil nil nil   ; 0x10-0x17 (0x14/0x15 typed refs)
    nil nil :drop :select nil nil nil nil   ; 0x18-0x1F
    :local.get :local.set :local.tee :global.get :global.set :table.get :table.set nil   ; 0x20-0x27
    :i32.load :i64.load :f32.load :f64.load :i32.load8_s :i32.load8_u :i32.load16_s :i32.load16_u   ; 0x28-0x2F
    :i64.load8_s :i64.load8_u :i64.load16_s :i64.load16_u :i64.load32_s :i64.load32_u :i32.store :i64.store   ; 0x30-0x37
    :f32.store :f64.store :i32.store8 :i32.store16 :i64.store8 :i64.store16 :i64.store32 :memory.size   ; 0x38-0x3F
    :memory.grow :i32.const :i64.const :f32.const :f64.const :i32.eqz :i32.eq :i32.ne   ; 0x40-0x47
    :i32.lt_s :i32.lt_u :i32.gt_s :i32.gt_u :i32.le_s :i32.le_u :i32.ge_s :i32.ge_u   ; 0x48-0x4F
    :i64.eqz :i64.eq :i64.ne :i64.lt_s :i64.lt_u :i64.gt_s :i64.gt_u :i64.le_s   ; 0x50-0x57
    :i64.le_u :i64.ge_s :i64.ge_u :f32.eq :f32.ne :f32.lt :f32.gt :f32.le   ; 0x58-0x5F
    :f32.ge :f64.eq :f64.ne :f64.lt :f64.gt :f64.le :f64.ge :i32.clz   ; 0x60-0x67
    :i32.ctz :i32.popcnt :i32.add :i32.sub :i32.mul :i32.div_s :i32.div_u :i32.rem_s   ; 0x68-0x6F
    :i32.rem_u :i32.and :i32.or :i32.xor :i32.shl :i32.shr_s :i32.shr_u :i32.rotl   ; 0x70-0x77
    :i32.rotr :i64.clz :i64.ctz :i64.popcnt :i64.add :i64.sub :i64.mul :i64.div_s   ; 0x78-0x7F
    :i64.div_u :i64.rem_s :i64.rem_u :i64.and :i64.or :i64.xor :i64.shl :i64.shr_s   ; 0x80-0x87
    :i64.shr_u :i64.rotl :i64.rotr :f32.abs :f32.neg :f32.ceil :f32.floor :f32.trunc   ; 0x88-0x8F
    :f32.nearest :f32.sqrt :f32.add :f32.sub :f32.mul :f32.div :f32.min :f32.max   ; 0x90-0x97
    :f32.copysign :f64.abs :f64.neg :f64.ceil :f64.floor :f64.trunc :f64.nearest :f64.sqrt   ; 0x98-0x9F
    :f64.add :f64.sub :f64.mul :f64.div :f64.min :f64.max :f64.copysign :i32.wrap_i64   ; 0xA0-0xA7
    :i32.trunc_f32_s :i32.trunc_f32_u :i32.trunc_f64_s :i32.trunc_f64_u :i64.extend_i32_s :i64.extend_i32_u :i64.trunc_f32_s :i64.trunc_f32_u   ; 0xA8-0xAF
    :i64.trunc_f64_s :i64.trunc_f64_u :f32.convert_i32_s :f32.convert_i32_u :f32.convert_i64_s :f32.convert_i64_u :f32.demote_f64 :f64.convert_i32_s   ; 0xB0-0xB7
    :f64.convert_i32_u :f64.convert_i64_s :f64.convert_i64_u :f64.promote_f32 :i32.reinterpret_f32 :i64.reinterpret_f64 :f32.reinterpret_i32 :f64.reinterpret_i64   ; 0xB8-0xBF
    :i32.extend8_s :i32.extend16_s :i64.extend8_s :i64.extend16_s :i64.extend32_s nil nil nil   ; 0xC0-0xC7
    nil nil nil nil nil nil nil nil   ; 0xC8-0xCF
    ;; 0xD0-0xD6: ref.null carries a heaptype immediate, ref.is_null
    ;; and ref.func (funcidx) have dedicated clauses; ref.eq 0xD3
    ;; (loop 22, GC reference identity) is immediate-free and lives
    ;; here; 0xD4-0xD6 sit in the table so the ENCODER's reverse
    ;; lookup finds them (their decoder clauses match first).
    :ref.null :ref.is-null :ref.func :ref.eq :ref.as-non-null
    :br-on-null :br-on-non-null))

(defun %simple-op (byte)
  (and (< byte (length *simple-ops*)) (svref *simple-ops* byte)))

(defun %le-word (b off)
  "Little-endian unsigned 32-bit word at OFF in byte vector B."
  (logior (aref b off)
          (ash (aref b (+ off 1)) 8)
          (ash (aref b (+ off 2)) 16)
          (ash (aref b (+ off 3)) 24)))

(defun %word->s32 (w)
  "Reinterpret W as signed 32-bit: SBCL's float makers take a signed
word, but little-endian accumulation is unsigned."
  (if (>= w #x80000000) (- w #x100000000) w))

(defun %read-f32 (r)
  "Little-endian 4 bytes -> single-float via its bit pattern."
  (sb-kernel:make-single-float
   (%word->s32 (%le-word (read-bytes r 4) 0))))

(defun %read-f64 (r)
  "Little-endian 8 bytes -> double-float via its bit pattern."
  (let ((b (read-bytes r 8)))
    (sb-kernel:make-double-float
     (%word->s32 (%le-word b 4))
     (%le-word b 0))))

(defun %f32-bits (x)
  (logand (sb-kernel:single-float-bits x) #xFFFFFFFF))

(defun %f64-bits (x)
  (logand (sb-kernel:double-float-bits x) #xFFFFFFFFFFFFFFFF))

;;; Float literals: we keep the exact bit pattern so NaN payloads
;;; survive. The IMMEDIATE is the bit pattern (unsigned, per the value
;;; representation contract); interp/codegen reconstruct via
;;; f32-from-bits / f64-from-bits equivalents. SBCL-only helpers; the
;;; rest of the decoder is portable CL.

(defun read-memarg (r)
  "Exported alias of %READ-MEMARG (loop 24: the SIMD 0xFD decoder
decodes memarg immediates through the exact same grammar)."
  (%read-memarg r))

(defun %read-memarg (r)
  "memarg per the reference decoder (decode.ml memop): flags is a u32
that must be < 0x80 (else 'malformed memop flags' at DECODE — align
exponents beyond 2^63 and multi-byte flag encodings are malformed);
bit 6 -> memidx follows (stored as :memidx); align = flags & 0x3F;
offset is u64 ALWAYS (memory64's address domain; every pinned
'integer too large' fixture's value is >= 2^64 and stays
malformed — the u64 domain only admits what the spec admits).
The VALIDATOR imposes the per-addrtype offset bound (< 2^32 for
i32-addressed memories) at %check-memarg."
  (let ((start (reader-offset r))
        (flags (read-u32 r)))
    (unless (< flags #x80)
      (decode-error-at start "malformed memop flags: ~a" flags))
    (if (logtest flags #x40)
        (let ((memidx (read-u32 r))
              (offset (read-u64 r)))
          (list :align (logand flags #x3F) :offset offset :memidx memidx))
        (list :align flags :offset (read-u64 r)))))

(defun %read-instr (r depth)
  "Decode one instruction at R. Returns the wasm-instr, or :end / :else
markers (keywords) for the block terminators."
  (let ((start (reader-offset r))
        (b (read-byte-at r)))
    (flet ((mk (op imms &optional (body #()))
             (make-wasm-instr op imms body start)))
      (case b
        ((#x02 #x03)                      ; block loop
         (let* ((bt (%read-blocktype r))
                (op (if (= b #x02) :block :loop))
                (body (coerce (%read-block-body r (1+ depth)) 'vector)))
           (mk op (list :bt bt) body)))
        (#x04                              ; if
         (let ((bt (%read-blocktype r)))
           (multiple-value-bind (then else present)
               (%read-if-body r (1+ depth))
             (mk :if (if present
                         (list :bt bt :else-body (coerce else 'vector))
                         (list :bt bt))
                 (coerce then 'vector)))))
        (#x05
         (when (zerop depth)
           (%decode-error r "unexpected else"))
         :else)
        (#x0B :end)
        (#x0C (mk :br (list :label (read-u32 r))))
        (#x0D (mk :br-if (list :label (read-u32 r))))
        (#x0E
         (let ((targets (read-vec r #'read-u32 "br_table target"))
               (default (read-u32 r)))
           (mk :br-table (list :targets targets :default default))))
        (#x10 (mk :call (list :func (read-u32 r))))
        (#x11
         (let ((type (read-u32 r))
               (table (read-u32 r)))
           (mk :call-indirect (list :type type :table table))))
        ;; EH (Core 3.0): throw 0x08 — tagidx immediate.
        (#x08 (mk :throw (list :tag (read-u32 r))))
        ;; tail calls (Core 3.0 §6.2): same immediates as their
        ;; non-tail counterparts; NOT in *simple-ops* (they carry
        ;; immediates), so dedicated clauses like :call/:call-indirect.
        (#x12 (mk :return-call (list :func (read-u32 r))))
        (#x13
         (let ((type (read-u32 r))
               (table (read-u32 r)))
           (mk :return-call-indirect
               (list :type type :table table))))
        ;; EH (Core 3.0): try_table 0x1F — blocktype, catch clause
        ;; vector, then the guarded body up to the matching end
        ;; (catch clause tags: 0x00 catch, 0x01 catch_ref, 0x02
        ;; catch_all, 0x03 catch_all_ref; Core 3.0 binary format).
        (#x1F
         (let* ((bt (%read-blocktype r))
                (catches
                  (read-vec r
                            (lambda (r)
                              (let ((cc (read-byte-at r)))
                                (case cc
                                  (#x00 (list :catch (read-u32 r)
                                              (read-u32 r)))
                                  (#x01 (list :catch-ref (read-u32 r)
                                              (read-u32 r)))
                                  (#x02 (list :catch-all (read-u32 r)))
                                  (#x03 (list :catch-all-ref (read-u32 r)))
                                  (t (%decode-error r
                                                    "malformed catch clause 0x~x"
                                                    cc)))))
                            "catch clause"))
                (body (coerce (%read-block-body r (1+ depth)) 'vector)))
           (mk :try-table (list :bt bt :catches catches) body)))
        (#x1B (mk :select (list :types nil)))   ; untyped select: no imms
        (#x1C
          (let ((types (read-vec r #'%read-valtype "select type")))
            (mk :select (list :types types))))
        ((#x20 #x21 #x22)
         (let* ((x (read-u32 r))
                (op (case b (#x20 :local.get) (#x21 :local.set) (#x22 :local.tee))))
           (mk op (list :local x))))
        ((#x23 #x24)
         (let* ((x (read-u32 r))
                (op (if (= b #x23) :global.get :global.set)))
           (mk op (list :global x))))
        ((#x25 #x26)
         (let* ((x (read-u32 r))
                (op (if (= b #x25) :table.get :table.set)))
           (mk op (list :table x))))
        ((#x28 #x29 #x2A #x2B #x2C #x2D #x2E #x2F #x30 #x31
          #x32 #x33 #x34 #x35 #x36 #x37 #x38 #x39 #x3A #x3B
          #x3C #x3D #x3E)
         (mk (%simple-op b) (%read-memarg r)))
        ((#x3F #x40)
         ;; memory.size/grow carry a memidx (multi-memory form)
         (mk (if (= b #x3F) :memory.size :memory.grow)
             (list :memidx (read-u32 r))))
        (#x41 (mk :i32.const (list :value (read-s32 r))))
        (#x42 (mk :i64.const (list :value (read-s64 r))))
        (#x43 (mk :f32.const (list :bits (%f32-bits (%read-f32 r)))))
        (#x44 (mk :f64.const (list :bits (%f64-bits (%read-f64 r)))))
        ((#x45 #x46 #x47 #x48 #x49 #x4A #x4B #x4C #x4D #x4E #x4F
          #x50 #x51 #x52 #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A
          #x5B #x5C #x5D #x5E #x5F #x60 #x61 #x62 #x63 #x64 #x65
          #x66 #x67 #x68 #x69 #x6A #x6B #x6C #x6D #x6E #x6F #x70
          #x71 #x72 #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x7B
          #x7C #x7D #x7E #x7F #x80 #x81 #x82 #x83 #x84 #x85 #x86
          #x87 #x88 #x89 #x8A #x8B #x8C #x8D #x8E #x8F #x90 #x91
          #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C
          #x9D #x9E #x9F #xA0 #xA1 #xA2 #xA3 #xA4 #xA5 #xA6 #xA7
          #xA8 #xA9 #xAA #xAB #xAC #xAD #xAE #xAF #xB0 #xB1 #xB2
          #xB3 #xB4 #xB5 #xB6 #xB7 #xB8 #xB9 #xBA #xBB #xBC #xBD
          #xBE #xBF #xC0 #xC1 #xC2 #xC3 #xC4)
         (mk (%simple-op b) nil))
        ((#xD0 #xD1 #xD2 #xD4 #xD5 #xD6)
         ;; typed function references (Core 3.0 binary format,
         ;; instructions.html): ref.as_non_null 0xD4 (no imm),
         ;; br_on_null 0xD5 / br_on_non_null 0xD6 (labelidx u32);
         ;; call_ref 0x14 / return_call_ref 0x15 are DEDICATED top
         ;; bytes (not 0xFC sub-opcodes) — see the 0x14/0x15 clauses.
         ;; ref.eq 0xD3 (loop 22, GC reference identity) is
         ;; immediate-free: it decodes via *SIMPLE-OPS*' 0xD3 entry
         ;; through the generic fallback clause below.
         (case b
           (#xD0 (mk :ref.null (list :type (%read-heaptype r))))
           (#xD1 (mk :ref.is-null nil))
           (#xD2 (mk :ref.func (list :func (read-u32 r))))
           (#xD4 (mk :ref.as-non-null nil))
           (#xD5 (mk :br-on-null (list :label (read-u32 r))))
           (#xD6 (mk :br-on-non-null (list :label (read-u32 r))))))
        ((#x14 #x15)
         ;; call_ref 0x14 / return_call_ref 0x15: typeidx u32
         ;; immediate (Core 3.0 moved these OFF the 0xFC prefix).
         (mk (if (= b #x14) :call-ref :return-call-ref)
             (list :type (read-u32 r))))
        (#xFC
         (let ((sub (read-u32 r)))
           (case sub
             (0 (mk :i32.trunc_sat_f32_s nil))
             (1 (mk :i32.trunc_sat_f32_u nil))
             (2 (mk :i32.trunc_sat_f64_s nil))
             (3 (mk :i32.trunc_sat_f64_u nil))
             (4 (mk :i64.trunc_sat_f32_s nil))
             (5 (mk :i64.trunc_sat_f32_u nil))
             (6 (mk :i64.trunc_sat_f64_s nil))
             (7 (mk :i64.trunc_sat_f64_u nil))
             ;; memory.init x y: dataidx then memidx
             (8 (mk :memory.init (list :data (read-u32 r) :memidx (read-u32 r))))
             (9 (mk :data.drop (list :data (read-u32 r))))
             ;; memory.copy x1 x2: dst memidx then src memidx
             (10 (mk :memory.copy (list :dst (read-u32 r) :src (read-u32 r))))
             (11 (mk :memory.fill (list :memidx (read-u32 r))))
            ;; table.init x y: elemidx then tableidx — the BINARY
            ;; order (the text form is reversed: tableidx first)
             (12 (mk :table.init (list :elem (read-u32 r) :table (read-u32 r))))
             (13 (mk :elem.drop (list :elem (read-u32 r))))
             (14 (mk :table.copy (list :dst (read-u32 r) :src (read-u32 r))))
             (15 (mk :table.grow (list :table (read-u32 r))))
             (16 (mk :table.size (list :table (read-u32 r))))
             (17 (mk :table.fill (list :table (read-u32 r))))
             (t (%decode-error r "unknown 0xFC opcode ~d" sub)))))
        ;; GC value opcodes (loop 22, the 0xFB prefix family —
        ;; binary/instructions.html). Struct ops:
        ;;   0x00 struct.new t, 0x01 struct.new_default t,
        ;;   0x02 struct.get t f, 0x03 struct.get_s, 0x04 struct.get_u,
        ;;   0x05 struct.set t f
        ;; Array ops: 0x06 array.new t, 0x07 array.new_default t,
        ;;   0x08 array.new_fixed t n, 0x09 array.new_data t d,
        ;;   0x0A array.new_elem t e, 0x0B array.get t,
        ;;   0x0C array.get_s, 0x0D array.get_u, 0x0E array.set t,
        ;;   0x0F array.len, 0x10 array.fill t, 0x11 array.copy t1 t2,
        ;;   0x12 array.init_data t d, 0x13 array.init_elem t e
        ;; Casts: 0x14/0x15 ref.test (nullable/non-null),
        ;;   0x16/0x17 ref.cast, 0x18 br_on_cast l ht1 ht2,
        ;;   0x19 br_on_cast_fail
        ;; Conversions: 0x1A any.convert_extern, 0x1B extern.convert_any
        ;; i31: 0x1C ref.i31, 0x1D i31.get_s, 0x1E i31.get_u
        (#xFB
         (let ((sub (read-u32 r)))
           (case sub
             (0 (mk :struct.new (list :type (read-u32 r))))
             (1 (mk :struct.new-default (list :type (read-u32 r))))
             (2 (mk :struct.get (list :type (read-u32 r)
                                      :field (read-u32 r))))
             (3 (mk :struct.get-s (list :type (read-u32 r)
                                        :field (read-u32 r))))
             (4 (mk :struct.get-u (list :type (read-u32 r)
                                        :field (read-u32 r))))
             (5 (mk :struct.set (list :type (read-u32 r)
                                      :field (read-u32 r))))
             (6 (mk :array.new (list :type (read-u32 r))))
             (7 (mk :array.new-default (list :type (read-u32 r))))
             (8 (mk :array.new-fixed (list :type (read-u32 r)
                                           :count (read-u32 r))))
             (9 (mk :array.new-data (list :type (read-u32 r)
                                         :data (read-u32 r))))
             (10 (mk :array.new-elem (list :type (read-u32 r)
                                          :elem (read-u32 r))))
             (11 (mk :array.get (list :type (read-u32 r))))
             (12 (mk :array.get-s (list :type (read-u32 r))))
             (13 (mk :array.get-u (list :type (read-u32 r))))
             (14 (mk :array.set (list :type (read-u32 r))))
             (15 (mk :array.len nil))
             (16 (mk :array.fill (list :type (read-u32 r))))
             (17 (mk :array.copy (list :dst (read-u32 r)
                                        :src (read-u32 r))))
             (18 (mk :array.init-data (list :type (read-u32 r)
                                            :data (read-u32 r))))
             (19 (mk :array.init-elem (list :type (read-u32 r)
                                            :elem (read-u32 r))))
             ;; ref.test/ref.cast: heaptype immediate follows; the
             ;; immediate flags NULLABLE (0x14/0x16) vs NON-NULL
             ;; (0x15/0x17) are separate op keywords.
             (20 (mk :ref.test (list :type (%read-heaptype r)
                                     :nullable t)))
             (21 (mk :ref.test (list :type (%read-heaptype r))))
             (22 (mk :ref.cast (list :type (%read-heaptype r)
                                     :nullable t)))
             (23 (mk :ref.cast (list :type (%read-heaptype r))))
             ;; br_on_cast l ht1 ht2: labelidx, the source's heap
             ;; (must be a supertype of ht2) and the target heap.
             (24 (mk :br-on-cast
                     (list :label (read-u32 r)
                           :src (%read-heaptype r)
                           :dst (%read-heaptype r))))
             (25 (mk :br-on-cast-fail
                     (list :label (read-u32 r)
                           :src (%read-heaptype r)
                           :dst (%read-heaptype r))))
             (26 (mk :any.convert-extern nil))
             (27 (mk :extern.convert-any nil))
             (28 (mk :ref.i31 nil))
             (29 (mk :i31.get-s nil))
             (30 (mk :i31.get-u nil))
             (t (%decode-error r "unknown 0xFB opcode ~d" sub)))))
        ;; SIMD (loop 24, the 0xFD prefix family — the Core 3.0
        ;; SIMD binary encoding, proposals/simd/BinarySIMD.md, plus
        ;; the relaxed-SIMD opcodes 0x100..0x113,
        ;; relaxed-simd/proposals/relaxed-simd/Overview.md). All
        ;; immediates decode through wasmdown.simd's entry table;
        ;; sub-opcodes are read as u32 LEB (per the simdop:varuint32
        ;; grammar; the 0x100+ relaxed values NEED the multi-byte
        ;; form).
        (#xFD
         (let ((sub (read-u32 r)))
           (let ((e (wasmdown.simd:entry-by-sub sub)))
             (if (null e)
                 (%decode-error r "unknown 0xFD opcode ~d" sub)
                 (mk (wasmdown.simd:simd-entry-op e)
                     (wasmdown.simd:decode-simd-imms r e))))))
        (t
         (let ((simple (%simple-op b)))
           (if simple
               (mk simple nil)
               (%decode-error r "illegal opcode 0x~x" b))))))))

(defun %read-block-body (r depth)
  "Read instructions until the matching :end, returning them WITHOUT
the :end. :if bodies split at :else into imms (:else-body . vector)."
  (loop for instr = (%read-instr r depth)
        until (eq instr :end)
        collect instr into body
        finally (return body)))

(defun %read-if-body (r depth)
  "Read the then-branch until :else or :end; return (values then-body
else-body present-p)."
  (loop for instr = (%read-instr r depth)
        if (eq instr :end)
          return (values body nil nil)
        if (eq instr :else)
          return (values body (coerce (%read-block-body r depth) 'vector) t)
        collect instr into body
        finally (return (values body nil nil))))

(defun %decode-instr (r depth)
  "Decode one structured instruction (block/loop/if) at R, consuming
its terminator(s). Simple instructions decode via %read-instr."
  (%read-instr r depth))

(defun decode-instr-seq (r)
  "Decode a full expression: instructions up to and including the
terminating :end, which IS included in the result. R is a reader."
  (let ((body (%read-block-body r 0)))
    (coerce (append body
                    (list (make-wasm-instr :end nil #() (reader-offset r))))
            'vector)))

;;; ---------------------------------------------------------------
;;; Sections. decode-module walks the section list in order; known
;;; sections must appear at most once and in id order (custom sections
;;; may appear anywhere after the magic/version). Section sizes are
;;; enforced: content must exactly fill the declared size.

(defun %read-fieldtype (r)
  "A fieldtype (binary/types.html): storage valtype + mutability
byte 0/1. Stored as a (valtype . mutable-p) pair, matching the
text parser's comptype FIELDS contract."
  (let ((vt (%read-valtype r))
        (mut (read-byte-at r)))
    (unless (or (= mut 0) (= mut 1))
      (%decode-error r "malformed field mutability 0x~x" mut))
    (cons vt (= mut 1))))

(defun %read-comptype-body (r)
  "A comptype after any sub-count (binary/types.html comptype):
  functype 0x60, structtype 0x5F (fieldtype vec), arraytype 0x5E
  \(one fieldtype). Returns a wasm-comptype — the uniform rec
  model: all kinds keep FIELDS as (valtype . mutable-p) pairs;
  func keeps params/results."
  (let ((form (read-byte-at r)))
    (case form
      (#x60 (make-wasm-comptype
             :func nil '()
             (coerce (read-vec r #'%read-valtype "functype param") 'list)
             (coerce (read-vec r #'%read-valtype "functype result") 'list)))
      (#x5F (make-wasm-comptype
             :struct nil
             (coerce (read-vec r #'%read-fieldtype "struct field") 'list)
             '() '()))
      (#x5E (let ((ft (%read-fieldtype r)))
              (make-wasm-comptype :array nil (list ft) '() '())))
      (t (%decode-error r "unknown type form 0x~x" form)))))

(defun %read-sub-count (r)
  "A subtype's sub-count byte (binary/types.html subtype): 0x00 =
  no explicit supertypes, 0x01 = a supertypevec follows. The
  non-null flag forms (0x03/0x04) are not in this slice (no
  declared subtyping): decode error, the same honesty as
  malformed limits flags. Returns the number of supertypes read."
  (let ((b (read-byte-at r)))
    (case b
      (#x00 0)
      (#x01 (let ((n (read-u32 r)))
              (dotimes (_ n) (read-u32 r))
              n))
      (t (%decode-error r "malformed subtype form 0x~x" b)))))

(defun %read-rectype (r group-start)
  "One type-section entry (binary/types.html rectype): a bare
  comptype (functype 0x60 / structtype 0x5F / arraytype 0x5E — its
  own singleton group, NO sub-count byte), 0x50 SUB (sub-count then
  one comptype) or 0x4E REC (comptype vec). Returns one
  wasm-rec-type per MEMBER, all sharing one group object,
  consecutive typeidxs from GROUP-START (the loop-20 uniform rec
  model), in DECLARATION order."
  (let ((b (peek-byte-at r)))
    (cond
     ((= b #x4E)
      (read-byte-at r)
      (let* ((cts (read-vec r
                            ;; each rec member is a BARE comptype
                            ;; (binary/types.html rectype: the
                            ;; sub-count byte belongs to the SUB
                            ;; form, not rec members)
                            #'%read-comptype-body
                            "rec comptype"))
             (grp (make-wasm-rectype-group cts)))
        (loop for ct across cts
              for i from 0
              collect (make-rec-type-entry ct grp i group-start))))
     ((= b #x50)
      ;; SUB: sub-count then ONE comptype (no declared subtyping in
      ;; this slice; the supertypes are read and discarded — the
      ;; nominal heap lattice governs subtyping)
      (read-byte-at r)
      (let* ((sub-n (%read-sub-count r))
             (ct (%read-comptype-body r))
             (grp (make-wasm-rectype-group (vector ct))))
        (declare (ignore sub-n))
        (list (make-rec-type-entry ct grp 0 group-start))))
     ;; a bare comptype: no sub-count byte at all (binary/types.html
     ;; rectype's first alternative — the pre-GC module shape every
     ;; legacy fixture uses)
     (t
      (let* ((ct (%read-comptype-body r))
             (grp (make-wasm-rectype-group (vector ct))))
        (list (make-rec-type-entry ct grp 0 group-start)))))))

(defun %read-type-section (r)
  "The type section (binary/types.html typesec): a vec of RECTYPE
  entries — bare comptypes (singleton rec groups) or REC blocks.
  Every entry expands to a LIST of wasm-rec-type members; the
  section's flat member sequence is the module's type vector (spec:
  a rec block's members occupy CONSECUTIVE typeidxs, one per
  member, not one per block)."
  (let ((start (reader-offset r))
        (count (read-u32 r)))
    (when (> count (- (reader-limit r) (reader-offset r)))
      (decode-error-at start "length out of bounds: type vector declares ~d entries" count))
    (let ((members '()))
      (dotimes (_ count)
        ;; group-start of the NEXT entry: the members flattened so
        ;; far (the rec model's forward-reference bound). %READ-
        ;; RECTYPE returns DECLARATION order; append keeps it.
        (let ((gs (length members)))
          (setf members (nconc members (%read-rectype r gs)))))
      (coerce members 'vector))))

(defun %read-import-section (r)
  (read-vec r
            (lambda (r)
              (let* ((module (read-name r))
                     (field (read-name r))
                     (kind (read-byte-at r)))
                (case kind
                  (#x00 (make-wasm-import module field :func (read-u32 r)))
                  (#x01 (let ((rt (%read-reftype r))
                              (limits (%read-limits r)))
                          (make-wasm-import module field :table
                                            (make-wasm-table rt limits))))
                  (#x02 (make-wasm-import module field :memory
                                          (make-wasm-memory (%read-limits r))))
                  (#x03 (let ((type (%read-valtype r))
                              (mut (read-byte-at r)))
                          (unless (or (= mut 0) (= mut 1))
                            (%decode-error r "malformed global mutability 0x~x" mut))
                          (make-wasm-import module field :global
                                            (cons type (= mut 1)))))
                  ;; EH (Core 3.0): tag import — kind 0x04, payload a
                  ;; typeidx (the tag's functype; results must be
                  ;; empty, checked at validation).
                  (#x04 (make-wasm-import module field :tag (read-u32 r)))
                  (t (%decode-error r "unknown import kind 0x~x" kind)))))
            "import"))

(defun %read-func-section (r)
  (read-vec r #'read-u32 "function"))

(defun %read-table-section (r)
  "Table entries: plain tabletype (reftype limits), or the Core 3.0
extended form 0x40 0x00 + tabletype + init EXPR — elem.wast ::458's
'\40\00\64\70\00\01\d2\00\0b' = (table 1 (ref func) (ref.func 0)).
The extended form desugars exactly like the WAT (table n reft expr):
an ACTIVE elem segment [expr x min] at offset 0, marked
from-table-init (imported-globals-only const rule at validate).
Returns (values tables-list desugared-elems-list)."
  (let ((tables '()) (elems '()))
    (read-vec r
              (lambda (r)
                (if (and (= (peek-byte-at r) #x40)
                         ;; disambiguate: 0x40 could start a reftype?
                         ;; No — 0x40 is not a reftype byte; the plain
                         ;; form starts with the reftype itself.
                         t)
                    (let ((start (reader-offset r)))
                      (read-byte-at r)                   ; 0x40
                      (let ((flags (read-byte-at r)))
                        (unless (zerop flags)
                          (decode-error-at start
                                           "malformed table entry flags 0x~x"
                                           flags)))
                      (let* ((rt (%read-reftype r))
                             (limits (%read-limits r))
                             (expr (decode-instr-seq r)))
                        ;; tables accumulates in push order; the
                        ;; entry just pushed sits at (1- length) in
                        ;; DECLARATION order — the elem targets it
                        ;; 0-based (check-tables' exemption compares
                        ;; i + imported-count, both 0-based; the
                        ;; import partition happens at module
                        ;; assembly, not here).
                        (push (make-wasm-table rt limits) tables)
                        ;; active elem at offset 0, [expr x min]; the
                        ;; just-pushed table sits at (1- length) in
                        ;; DECLARATION order — 0-based, matching
                        ;; check-tables' i + imported-count.
                        (push (make-wasm-elem
                               (list :active (1- (length tables))
                                     (vector
                                      (make-wasm-instr :i32.const
                                                       (list :value 0) #()
                                                       (reader-offset r))
                                      (make-wasm-instr :end nil #()
                                                       (reader-offset r))))
                               rt
                               (loop repeat (wasm-limits-min limits)
                                     collect expr)
                               t)
                              elems)))
                    (let ((tab (make-wasm-table (%read-reftype r)
                                                 (%read-limits r))))
                      (push tab tables))))
              "table")
    (values (coerce (nreverse tables) 'vector)
            (nreverse elems))))

(defun %read-mem-section (r)
  (read-vec r
            (lambda (r)
              (make-wasm-memory (%read-limits r)))
            "memory"))

(defun %read-global-section (r)
  (read-vec r
            (lambda (r)
              (let ((type (%read-valtype r))
                    (mut (read-byte-at r)))
                (unless (or (= mut 0) (= mut 1))
                  (%decode-error r "malformed global mutability 0x~x" mut))
                (make-wasm-global :type type :mutable (= mut 1)
                                  :init (decode-instr-seq r))))
            "global"))

(defun %read-export-section (r)
  (read-vec r
            (lambda (r)
              (let* ((name (read-name r))
                     (kind (read-byte-at r))
                     (index (read-u32 r)))
                (make-wasm-export name
                                   (case kind
                                     (#x00 :func) (#x01 :table)
                                     (#x02 :memory) (#x03 :global)
                                     ;; EH (Core 3.0): tag export kind.
                                     (#x04 :tag)
                                     (t (%decode-error r "unknown export kind 0x~x" kind)))
                                   index)))
            "export"))

(defun %read-start-section (r)
  (read-u32 r))

(defun %read-elemexpr (r)
  "An element expression is a full expr (vector incl. :end)."
  (decode-instr-seq r))

(defun %read-elem-funcidx (r)
  "Legacy funcidx item becomes (ref.func x) init expression."
  (let ((start (reader-offset r))
        (x (read-u32 r)))
    (vector (make-wasm-instr :ref.func (list :func x) #() start)
            (make-wasm-instr :end nil #() (reader-offset r)))))

(defun %read-elem-section (r)
  "All flags 0..7: legacy funcidx forms and elemexpr forms, active /
passive / declarative modes. Inits are normalized to LISTS of expr
vectors (the wasm-elem contract; runtime/validate iterate with IN)."
  (read-vec r
            (lambda (r)
              (let ((flags (read-u32 r)))
                (ecase flags
                  (0 (let ((expr (decode-instr-seq r)))
                       ;; flags 0: implicit funcref elemkind is NOT
                       ;; in the byte stream (reference decoder:
                       ;; active_zero then vec(elem_index) directly).
                       ;; :ELEMKIND-FUNC (not :funcref): the funcidx
                       ;; form's marker — the TEXT parser emits the
                       ;; same one for the mirrored form (elem.wast
                       ;; ::448-497's text/binary pairs), so
                       ;; encode/decode round-trips stay EQUALP and
                       ;; validate re-types it by the target table.
                       (make-wasm-elem (list :active 0 expr)
                                       :elemkind-func
                                       (%elem-funcidx-inits r))))
                  (1 (%read-elemkind r)
                     (make-wasm-elem (list :passive) :elemkind-func
                                     (%elem-funcidx-inits r)))
                  (2 (let ((table (read-u32 r))
                           (expr (decode-instr-seq r)))
                       (%read-elemkind r)
                       (make-wasm-elem (list :active table expr)
                                       :elemkind-func
                                       (%elem-funcidx-inits r))))
                  (3 (%read-elemkind r)
                     (make-wasm-elem (list :declarative) :elemkind-func
                                     (%elem-funcidx-inits r)))
                  (4 (let ((expr (decode-instr-seq r)))
                       (make-wasm-elem (list :active 0 expr) :funcref
                                       (%elem-expr-inits r))))
                  (5 (let ((rt (%read-reftype r)))
                       (make-wasm-elem (list :passive) rt
                                       (%elem-expr-inits r))))
                  (6 (let ((table (read-u32 r))
                           (expr (decode-instr-seq r))
                           (rt (%read-reftype r)))
                       (make-wasm-elem (list :active table expr) rt
                                       (%elem-expr-inits r))))
                  (7 (let ((rt (%read-reftype r)))
                       (make-wasm-elem (list :declarative) rt
                                       (%elem-expr-inits r)))))))
            "element"))

(defun %read-elemkind (r)
  "The elemkind byte of the legacy funcidx elem forms (flags 0..3):
  only 0x00 (funcref) is defined in this engine slice; any other
  byte is a decode error. Previously the byte was NOT consumed,
  so any module whose legacy elem segment carried elemkind + a
  funcidx vector misparsed (elemkind read as the vec count) and
  failed the section-size check (binary-leb128.wast ::5-89)."
  (let ((start (reader-offset r))
        (b (read-byte-at r)))
    (unless (zerop b)
      (decode-error-at start "bad element kind 0x~x" b))))

(defun %elem-funcidx-inits (r)
  (coerce (read-vec r #'%read-elem-funcidx "elem funcidx") 'list))

(defun %elem-expr-inits (r)
  (coerce (read-vec r #'%read-elemexpr "elem expr") 'list))

(defun %read-code-section (r typeidxs)
  "Each entry: u32 size, then locals vec + body expr. The entry count
must match the function section's count; the typeidx vector from the
function section supplies each body's type index. Per-entry sizes are
enforced exactly."
  (let ((count (read-u32 r)))
    (unless (= count (length typeidxs))
      (%decode-error r "function and code section have inconsistent lengths: ~d vs ~d"
                     count (length typeidxs)))
    (coerce
     (loop for typeidx across typeidxs
           collect (let* ((entry-start (reader-offset r))
                          (size (read-u32 r))
                          (body-end (+ (reader-offset r) size))
                          (locals (coerce (read-vec r
                                                     (lambda (r)
                                                       ;; spec: u32 count then valtype
                                                       (let ((n (read-u32 r)))
                                                         (cons (%read-valtype r) n)))
                                                     "local")
                                          'list))
                          (body (decode-instr-seq r)))
                     (unless (= (reader-offset r) body-end)
                       (decode-error-at entry-start
                                        "section size mismatch: function body declares ~d bytes"
                                        size))
                     (make-wasm-func typeidx locals body entry-start)))
     'vector)))

(defun %read-data-section (r)
  (read-vec r
            (lambda (r)
              (let ((flags (read-u32 r)))
                (case flags
                  (#x00 (make-wasm-data (list :active 0 (decode-instr-seq r))
                                        (read-byte-vec r "data byte")))
                  (#x01 (make-wasm-data (list :passive)
                                        (read-byte-vec r "data byte")))
                  (#x02 (let ((mem (read-u32 r))
                              (expr (decode-instr-seq r)))
                          (make-wasm-data (list :active mem expr)
                                          (read-byte-vec r "data byte"))))
                  (t (%decode-error r "malformed data segment flags ~d" flags)))))
            "data"))

(defun %read-datacount-section (r)
  (read-u32 r))

(defun %read-tag-section (r)
  "The tag section (id 13, EH Core 3.0): a vector of typeidx, one per
defined tag. The referenced type must be a functype with EMPTY
results; that check is validation's (check-tags), keeping decode
purely structural."
  (read-vec r #'read-u32 "tag"))

(defun decode-module (bytes)
  "Decode a module byte vector into a wasm-module. Signals
wasmdown:decode-error (with :offset) on any malformation."
  (let ((r (make-reader bytes)))
    ;; magic + version
    (unless (>= (reader-limit r) 8)
      (decode-error-at 0 "unexpected end of module: missing magic/version"))
    (let ((magic (read-bytes r 4)))
      (unless (and (= (aref magic 0) 0) (= (aref magic 1) #x61)
                   (= (aref magic 2) #x73) (= (aref magic 3) #x6d))
        (decode-error-at 0 "magic header not detected")))
    (let ((version (read-bytes r 4)))
      (unless (= (aref version 0) 1)
        (decode-error-at 4 "unknown binary version")))
    (let ((seen (make-hash-table))
          (last-rank 0)
          (types #()) (imports #()) (funcs #()) (tables #()) (mems #())
          (globals #()) (exports #()) (start nil) (elems #()) (datas #())
          (customs '()) (data-count nil) (func-typeidxs nil) (tags #()))
      (loop
        (when (>= (reader-offset r) (reader-limit r))
          (return))
        (let* ((sec-start (reader-offset r))
               (id (read-byte-at r))
               (size (read-u32 r))
               (content-end (+ (reader-offset r) size)))
          (when (> content-end (reader-limit r))
            (decode-error-at sec-start
                             "section size mismatch: section ~d declares ~d bytes"
                             id size))
          (cond
            ((= id 0)
             ;; The name and payload must fit the section's DECLARED
             ;; content: decode through a sub-reader bounded at
             ;; CONTENT-END. A zero-size section then fails "truncated"
             ;; reading the name length (custom.wast ::5, "unexpected
             ;; end") and an over-long name fails the bounds check —
             ;; previously the whole-module reader let the name length
             ;; eat bytes of the NEXT section, and the payload span
             ;; (- CONTENT-END OFFSET) went NEGATIVE, crashing
             ;; make-array as a harness error.
             (let* ((sub (make-reader (reader-bytes r)
                                      (reader-offset r) content-end))
                    (name (read-name sub))
                    (data (read-bytes sub (- (reader-limit sub)
                                             (reader-offset sub)))))
               (setf (reader-offset r) (reader-offset sub))
               (push (make-wasm-custom :name name :bytes data
                                       :offset sec-start)
                     customs)))
            (t
             (when (> id 13)
               (decode-error-at sec-start "unknown section id ~d" id))
             (when (gethash id seen)
               (decode-error-at sec-start "duplicate section id ~d" id))
             ;; Spec order: 1..9, then DataCount(12), then Code(10),
             ;; then Data(11), then Tag(13) (EH Core 3.0; the section
             ;; list's canonical rank — Tag comes after Data).
             (let ((rank (ecase id
                           ((1 2 3 4 5 6 7 8 9) id)
                           (12 10) (10 11) (11 12) (13 13))))
               (when (<= rank last-rank)
                 (decode-error-at sec-start
                                  "out of order section id ~d"
                                  id))
               (setf last-rank rank))
             (setf (gethash id seen) t)
             (case id
               (1 (setf types (%read-type-section r)))
               (2 (setf imports (%read-import-section r)))
               (3 (setf func-typeidxs (%read-func-section r)))
               (4 (multiple-value-bind (tabs desugared)
                      (%read-table-section r)
                    (setf tables tabs)
                    (when desugared
                      (setf elems (concatenate 'vector elems
                                               (coerce desugared 'vector))))))
               (5 (setf mems (%read-mem-section r)))
               (6 (setf globals (%read-global-section r)))
               (7 (setf exports (%read-export-section r)))
               (8 (setf start (%read-start-section r)))
               (9 (setf elems (concatenate 'vector elems
                                           (%read-elem-section r))))
               (10 (setf funcs (%read-code-section r (or func-typeidxs #()))))
               (11 (setf datas (%read-data-section r)))
               (12 (setf data-count (%read-datacount-section r)))
               (13 (setf tags (%read-tag-section r))))))
          (unless (= (reader-offset r) content-end)
            (decode-error-at sec-start
                             "section size mismatch: section ~d content does not fill declared size ~d"
                             id size))))
      (make-wasm-module
       :types types :imports imports :funcs funcs :tables tables
       :mems mems :tags tags :globals globals :exports exports :start start
       :elems elems :datas datas :customs (coerce (nreverse customs) 'vector)
       :data-count data-count))))
