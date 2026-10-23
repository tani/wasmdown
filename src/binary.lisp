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
;;; Value types, block types, limits, heaptype (funcref/externref only).

(defun %read-valtype (r)
  (let ((b (read-byte-at r)))
    (case b
      (#x7f :i32) (#x7e :i64) (#x7d :f32) (#x7c :f64)
      (#x70 :funcref) (#x6f :externref)
      (t (%decode-error r "unknown type form 0x~x" b)))))

(defun %read-reftype (r)
  (let ((b (read-byte-at r)))
    (case b
      (#x70 :funcref) (#x6f :externref)
      (t (%decode-error r "unknown type form 0x~x" b)))))

(defun %read-heaptype (r)
  "For ref.null: only the two shorthand heap types of this slice."
  (let ((b (read-byte-at r)))
    (case b
      (#x70 :funcref) (#x6f :externref)
      (t (%decode-error r "unknown type form 0x~x" b)))))

(defun %read-blocktype (r)
  "NIL (empty), a valtype, or (:typeidx . n)."
  (let ((b (peek-byte-at r)))
    (cond ((= b #x40) (read-byte-at r) nil)
          ((member b '(#x7f #x7e #x7d #x7c #x70 #x6f))
           (%read-valtype r))
          (t
           (let ((start (reader-offset r))
                 (n (read-s33 r)))
             (if (>= n 0)
                 (cons :typeidx n)
                 (decode-error-at start "unknown type form 0x~x" b)))))))

(defun %read-limits (r)
  "Flags 0x00 (min) and 0x01 (min+max) only: shared and 64-bit flag
forms are rejected as decode-error."
  (let ((flags (read-byte-at r)))
    (case flags
      (#x00 (make-wasm-limits (read-u32 r)))
      (#x01 (let ((min (read-u32 r))
                  (max (read-u32 r)))
              (make-wasm-limits min max)))
      (t (%decode-error r "malformed limits flags 0x~x" flags)))))

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
  #(
    :unreachable :nop nil nil nil :else nil nil   ; 0x00-0x07
    nil nil nil :end :br :br-if :br-table :return   ; 0x08-0x0F
    :call :call-indirect nil nil nil nil nil nil   ; 0x10-0x17
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
    :ref.null :ref.is-null :ref.func))

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

(defun %read-memarg (r)
  "align u32; bit 6 set -> memidx follows, stored as (:memidx . n) in
the returned plist; offset u32 (this slice is 32-bit-only, so a u32
even though the grammar writes u64)."
  (let ((align (read-u32 r)))
    (if (logtest align #x40)
        (let ((memidx (read-u32 r))
              (offset (read-u32 r)))
          (list :align (- align #x40) :offset offset :memidx memidx))
        (list :align align :offset (read-u32 r)))))

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
        ((#xD0 #xD1 #xD2)
         (case b
           (#xD0 (mk :ref.null (list :type (%read-heaptype r))))
           (#xD1 (mk :ref.is-null nil))
           (#xD2 (mk :ref.func (list :func (read-u32 r))))))
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
             ;; table.init x y: elemidx then tableidx (imms match the
             ;; textual form table.init x y)
             (12 (mk :table.init (list :elem (read-u32 r) :table (read-u32 r))))
             (13 (mk :elem.drop (list :elem (read-u32 r))))
             (14 (mk :table.copy (list :dst (read-u32 r) :src (read-u32 r))))
             (15 (mk :table.grow (list :table (read-u32 r))))
             (16 (mk :table.size (list :table (read-u32 r))))
             (17 (mk :table.fill (list :table (read-u32 r))))
             (t (%decode-error r "unknown 0xFC opcode ~d" sub)))))
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

(defun %read-type-section (r)
  (read-vec r
            (lambda (r)
              (let ((form (read-byte-at r)))
                (unless (= form #x60)
                  (%decode-error r "unknown type form 0x~x" form))
                (make-wasm-func-type
                 (coerce (read-vec r #'%read-valtype "functype param") 'list)
                 (coerce (read-vec r #'%read-valtype "functype result") 'list))))
            "type"))

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
                  (t (%decode-error r "unknown import kind 0x~x" kind)))))
            "import"))

(defun %read-func-section (r)
  (read-vec r #'read-u32 "function"))

(defun %read-table-section (r)
  (read-vec r
            (lambda (r)
              (make-wasm-table (%read-reftype r) (%read-limits r)))
            "table"))

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
                       (make-wasm-elem (list :active 0 expr) :funcref
                                       (%elem-funcidx-inits r))))
                  (1 (make-wasm-elem (list :passive) :funcref
                                     (%elem-funcidx-inits r)))
                  (2 (let ((table (read-u32 r))
                           (expr (decode-instr-seq r)))
                       (make-wasm-elem (list :active table expr) :funcref
                                       (%elem-funcidx-inits r))))
                  (3 (make-wasm-elem (list :declarative) :funcref
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
          (customs '()) (data-count nil) (func-typeidxs nil))
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
             (let* ((name (read-name r))
                    (data (read-bytes r (- content-end (reader-offset r)))))
               (push (make-wasm-custom :name name :bytes data
                                       :offset sec-start)
                     customs)))
            (t
             (when (> id 12)
               (decode-error-at sec-start "unknown section id ~d" id))
             (when (gethash id seen)
               (decode-error-at sec-start "duplicate section id ~d" id))
             ;; Spec order: 1..9, then DataCount(12), then Code(10),
             ;; then Data(11). Enforce that exact order.
             (let ((rank (ecase id
                           ((1 2 3 4 5 6 7 8 9) id)
                           (12 10) (10 11) (11 12))))
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
               (4 (setf tables (%read-table-section r)))
               (5 (setf mems (%read-mem-section r)))
               (6 (setf globals (%read-global-section r)))
               (7 (setf exports (%read-export-section r)))
               (8 (setf start (%read-start-section r)))
               (9 (setf elems (%read-elem-section r)))
               (10 (setf funcs (%read-code-section r (or func-typeidxs #()))))
               (11 (setf datas (%read-data-section r)))
               (12 (setf data-count (%read-datacount-section r))))))
          (unless (= (reader-offset r) content-end)
            (decode-error-at sec-start
                             "section size mismatch: section ~d content does not fill declared size ~d"
                             id size))))
      (make-wasm-module
       :types types :imports imports :funcs funcs :tables tables
       :mems mems :globals globals :exports exports :start start
       :elems elems :datas datas :customs (coerce (nreverse customs) 'vector)
       :data-count data-count))))
