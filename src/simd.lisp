;;;; SIMD (v128 + relaxed SIMD), loop 24 — shared metadata + semantics.
;;;; Package WASMDOWN.SIMD.
;;;;
;;;; REPRESENTATION (docs/design/PLAN.md 7.3, binding): a v128 value is ONE
;;;; object — a (simple-array (unsigned-byte 8) (16)) byte vector —
;;;; on the value stack, never per-lane boxed objects. Every lane op
;;;; scalarizes over the 16 backing bytes (loop per lane count).
;;;;
;;;; OPCODES: the full 0xFD family per the Core 3.0 spec's SIMD
;;;; binary encoding (BinarySIMD.md, exact sub-opcode bytes cited per
;;;; entry below) plus the relaxed-SIMD family (relaxed-simd
;;;; Overview.md, sub-opcodes 0x100..0x113).
;;;;
;;;; RELAXED ALLOWANCES (documented per op in the exec section): each
;;;; relaxed op admits a spec-sanctioned SET of outcomes; this engine
;;;; implements ONE canonical deterministic member of each set (the
;;;; portable non-relaxed / plain-arithmetic branch) and documents
;;;; the alternatives the spec admits.

(defpackage #:wasmdown.simd
  (:use #:cl)
  (:export #:v128-p #:make-v128 #:v128-bytes #:make-v128-from-bytes
           #:v128-zero #:lane-u #:set-lane-u
           #:simd-entry-op #:simd-entry-name #:simd-entry-sub
           #:simd-entry-params #:simd-entry-results #:simd-entry-imm
           #:simd-entry-class #:simd-entry-lane-width #:simd-entry-lanes
           #:simd-entry-float-p
           #:simd-op-p #:entry-of #:entry-by-sub #:entry-by-name
           #:entry-name #:entry-sub #:entry-op #:entry-params
           #:entry-results #:entry-imm #:entry-class
           #:entry-lane-width #:entry-lanes #:entry-float-p
           #:text-op-p #:plain-op-names #:memarg-op-entries
           #:exec #:validate-lane-imms #:lane-imm-p
           #:decode-simd-imms #:encode-simd-imms))

(in-package #:wasmdown.simd)

;;; ---------------------------------------------------------------
;;; Representation: one 16-byte vector per v128 value.

(declaim (inline make-v128 v128-p v128-bytes v128-zero))

(defun make-v128 ()
  "A fresh all-zero v128 value."
  (make-array 16 :element-type '(unsigned-byte 8)))

(defun v128-p (x)
  "Whether X is a v128 runtime value (the 16-byte vector shape; the
only other runtime values are fixnums, bignums and floats, so the
type test is unambiguous)."
  (typep x '(simple-array (unsigned-byte 8) (16))))

(defun v128-bytes (x)
  (declare (type (simple-array (unsigned-byte 8) (16)) x))
  x)

(defun make-v128-from-bytes (bytes &optional (offset 0))
  "A v128 from the first 16 bytes of BYTES at OFFSET (copied)."
  (declare (type vector bytes))
  (let ((v (make-array 16 :element-type '(unsigned-byte 8))))
    (loop for i below 16
          do (setf (aref v i) (aref bytes (+ offset i))))
    v))

(defun v128-zero ()
  "The zero v128 (shared immutable-ish constant; callers never mutate
const-loaded vectors — exec always builds fresh results)."
  (make-array 16 :element-type '(unsigned-byte 8)))

(declaim (inline lane-u set-lane-u))

(defun lane-u (v i width)
  "Lane I of V (unsigned, WIDTH bits) as a non-negative integer."
  (declare (type (simple-array (unsigned-byte 8) (16)) v)
           (type (integer 0 15) i)
           (type (integer 1 64) width))
  (let* ((bytes (ash width -3)) (off (* i bytes)) (acc 0))
    (declare (type (integer 0 8) bytes))
    (loop for j below bytes
          do (setf acc (logior acc (ash (aref v (+ off j)) (* 8 j)))))
    acc))

(defun set-lane-u (v i width x)
  "Store the low WIDTH bits of X into lane I of V (destructive on V)."
  (declare (type (simple-array (unsigned-byte 8) (16)) v)
           (type (integer 0 15) i)
           (type (integer 1 64) width))
  (let* ((bytes (ash width -3)) (off (* i bytes)))
    (declare (type (integer 0 8) bytes))
    (loop for j below bytes
          do (setf (aref v (+ off j))
                   (logand #xff (ash x (- (* 8 j)))))))
  v)

;;; ---------------------------------------------------------------
;;; The opcode table. One entry per 0xFD sub-opcode:
;;;;
;;;   (name sub params results imm class)
;;;;
;;; name    — the exact WAT spelling (string, lowercase)
;;; sub     — the 0xFD sub-opcode (u32; 0x100+ for relaxed)
;;; params  — a compact string: v=v128 i=i32 l=i64 f=f32 d=f64 a=addr
;;;           (the memory-op address param, typed by the memory's
;;;           addrtype at validation)
;;; results — same encoding
;;; imm     — () no immediates
;;;           (:const)                v128.const, 16 raw bytes
;;;           (:memarg N)             memarg, natural width N bytes
;;;           (:memarg-lane N M)      memarg (N bytes) + laneidx <= M
;;;           (:lane M)               one laneidx immediate <= M
;;;           (:lanes)                i8x16.shuffle: 16 idx bytes <=31
;;; class   — the exec semantic family (keyword)
;;;;
;;; Sub-opcode bytes are the WebAssembly SIMD binary encoding
;;; (proposals/simd/BinarySIMD.md) and the relaxed-SIMD encoding
;;; (relaxed-simd/proposals/relaxed-simd/Overview.md).

(defstruct simd-entry
  name
  (sub 0 :type (integer 0))
  op                                  ; interned IR keyword
  params                              ; list of valtype keywords / :addr
  results
  imm
  class
  lane-width                          ; bits (nil for v128.* shapes)
  lanes
  float-p)

(defun %param-decode (s)
  (loop for c across s
        collect (case c
                  (#\v :v128) (#\i :i32) (#\l :i64)
                  (#\f :f32) (#\d :f64) (#\a :addr))))

(defun %lane-shape (name)
  "The (width lanes float-p) of the op's shape prefix (i8x16 etc.),
or (nil 1 nil) for the v128 bit-shape ops."
  (let* ((dot (position #\. name))
         (prefix (subseq name 0 dot))
         (x (and (>= (length prefix) 4)
                 (member (char prefix 0) '(#\i #\f))
                 (position #\x prefix))))
    ;; shape: {i|f}{width-digits}x{lane-digits}  (i8x16, i16x8, i32x4,
    ;; i64x2, f32x4, f64x2); anything else (v128) is the bit shape.
    (if (and x (> x 1) (< x (- (length prefix) 1)))
        (values (parse-integer (subseq prefix 1 x) :radix 10)
                (parse-integer (subseq prefix (1+ x)) :radix 10)
                (char= (char prefix 0) #\f))
        (values nil 1 nil))))

(declaim (inline cdddr-6))
(defun cdddr-6 (x) (cddddr (cddr x)))

(defparameter *simd-table*
  '("v128.load"                #x00 "a" "v" (:memarg 16) ()
    "v128.load8x8_s"           #x01 "a" "v" (:memarg 8) ()
    "v128.load8x8_u"           #x02 "a" "v" (:memarg 8) ()
    "v128.load16x4_s"          #x03 "a" "v" (:memarg 8) ()
    "v128.load16x4_u"          #x04 "a" "v" (:memarg 8) ()
    "v128.load32x2_s"          #x05 "a" "v" (:memarg 8) ()
    "v128.load32x2_u"          #x06 "a" "v" (:memarg 8) ()
    "v128.load8_splat"         #x07 "a" "v" (:memarg 1) ()
    "v128.load16_splat"        #x08 "a" "v" (:memarg 2) ()
    "v128.load32_splat"        #x09 "a" "v" (:memarg 4) ()
    "v128.load64_splat"        #x0a "a" "v" (:memarg 8) ()
    "v128.store"               #x0b "av" ""  (:memarg 16) ()
    "v128.const"               #x0c ""  "v" (:const) :const
    "i8x16.shuffle"            #x0d "vv" "v" (:lanes) :shuffle
    "i8x16.swizzle"            #x0e "vv" "v" () :swizzle
    ;; ---- splats ----
    "i8x16.splat"              #x0f "i"  "v" () :splat
    "i16x8.splat"              #x10 "i"  "v" () :splat
    "i32x4.splat"              #x11 "i"  "v" () :splat
    "i64x2.splat"              #x12 "l"  "v" () :splat
    "f32x4.splat"              #x13 "f"  "v" () :splat
    "f64x2.splat"              #x14 "d"  "v" () :splat
    ;; ---- extract/replace lanes (lane immediate; ImmLaneIdxN) ----
    "i8x16.extract_lane_s"     #x15 "v"  "i" (:lane 15) :extract-s
    "i8x16.extract_lane_u"     #x16 "v"  "i" (:lane 15) :extract-u
    "i8x16.replace_lane"       #x17 "vi" "v" (:lane 15) :replace
    "i16x8.extract_lane_s"     #x18 "v"  "i" (:lane 7)  :extract-s
    "i16x8.extract_lane_u"     #x19 "v"  "i" (:lane 7)  :extract-u
    "i16x8.replace_lane"       #x1a "vi" "v" (:lane 7)  :replace
    "i32x4.extract_lane"       #x1b "v"  "i" (:lane 3)  :extract
    "i32x4.replace_lane"       #x1c "vi" "v" (:lane 3)  :replace
    "i64x2.extract_lane"       #x1d "v"  "l" (:lane 1)  :extract
    "i64x2.replace_lane"       #x1e "vl" "v" (:lane 1)  :replace
    "f32x4.extract_lane"       #x1f "v"  "f" (:lane 3)  :extract
    "f32x4.replace_lane"       #x20 "vf" "v" (:lane 3)  :replace
    "f64x2.extract_lane"       #x21 "v"  "d" (:lane 1)  :extract
    "f64x2.replace_lane"       #x22 "vd" "v" (:lane 1)  :replace
    ;; ---- i8x16 comparisons 0x23-0x2c ----
    "i8x16.eq"                 #x23 "vv" "v" () :i-eq
    "i8x16.ne"                 #x24 "vv" "v" () :i-ne
    "i8x16.lt_s"               #x25 "vv" "v" () :i-lt-s
    "i8x16.lt_u"               #x26 "vv" "v" () :i-lt-u
    "i8x16.gt_s"               #x27 "vv" "v" () :i-gt-s
    "i8x16.gt_u"               #x28 "vv" "v" () :i-gt-u
    "i8x16.le_s"               #x29 "vv" "v" () :i-le-s
    "i8x16.le_u"               #x2a "vv" "v" () :i-le-u
    "i8x16.ge_s"               #x2b "vv" "v" () :i-ge-s
    "i8x16.ge_u"               #x2c "vv" "v" () :i-ge-u
    ;; ---- i16x8 comparisons 0x2d-0x36 ----
    "i16x8.eq"                 #x2d "vv" "v" () :i-eq
    "i16x8.ne"                 #x2e "vv" "v" () :i-ne
    "i16x8.lt_s"               #x2f "vv" "v" () :i-lt-s
    "i16x8.lt_u"               #x30 "vv" "v" () :i-lt-u
    "i16x8.gt_s"               #x31 "vv" "v" () :i-gt-s
    "i16x8.gt_u"               #x32 "vv" "v" () :i-gt-u
    "i16x8.le_s"               #x33 "vv" "v" () :i-le-s
    "i16x8.le_u"               #x34 "vv" "v" () :i-le-u
    "i16x8.ge_s"               #x35 "vv" "v" () :i-ge-s
    "i16x8.ge_u"               #x36 "vv" "v" () :i-ge-u
    ;; ---- i32x4 comparisons 0x37-0x40 ----
    "i32x4.eq"                 #x37 "vv" "v" () :i-eq
    "i32x4.ne"                 #x38 "vv" "v" () :i-ne
    "i32x4.lt_s"               #x39 "vv" "v" () :i-lt-s
    "i32x4.lt_u"               #x3a "vv" "v" () :i-lt-u
    "i32x4.gt_s"               #x3b "vv" "v" () :i-gt-s
    "i32x4.gt_u"               #x3c "vv" "v" () :i-gt-u
    "i32x4.le_s"               #x3d "vv" "v" () :i-le-s
    "i32x4.le_u"               #x3e "vv" "v" () :i-le-u
    "i32x4.ge_s"               #x3f "vv" "v" () :i-ge-s
    "i32x4.ge_u"               #x40 "vv" "v" () :i-ge-u
    ;; ---- f32x4 comparisons 0x41-0x46 ----
    "f32x4.eq"                 #x41 "vv" "v" () :f-eq
    "f32x4.ne"                 #x42 "vv" "v" () :f-ne
    "f32x4.lt"                 #x43 "vv" "v" () :f-lt
    "f32x4.gt"                 #x44 "vv" "v" () :f-gt
    "f32x4.le"                 #x45 "vv" "v" () :f-le
    "f32x4.ge"                 #x46 "vv" "v" () :f-ge
    ;; ---- f64x2 comparisons 0x47-0x4c ----
    "f64x2.eq"                 #x47 "vv" "v" () :f-eq
    "f64x2.ne"                 #x48 "vv" "v" () :f-ne
    "f64x2.lt"                 #x49 "vv" "v" () :f-lt
    "f64x2.gt"                 #x4a "vv" "v" () :f-gt
    "f64x2.le"                 #x4b "vv" "v" () :f-le
    "f64x2.ge"                 #x4c "vv" "v" () :f-ge
    ;; ---- v128 bitwise 0x4d-0x53 ----
    "v128.not"                 #x4d "v"  "v" () :v-not
    "v128.and"                 #x4e "vv" "v" () :v-and
    "v128.andnot"              #x4f "vv" "v" () :v-andnot
    "v128.or"                  #x50 "vv" "v" () :v-or
    "v128.xor"                 #x51 "vv" "v" () :v-xor
    "v128.bitselect"           #x52 "vvv" "v" () :bitselect
    "v128.any_true"            #x53 "v"  "i" () :any-true
    ;; ---- load/store lane pairs 0x54-0x5b + zero loads 0x5c/0x5d ----
    "v128.load8_lane"          #x54 "av" "v" (:memarg-lane 1 15) ()
    "v128.load16_lane"         #x55 "av" "v" (:memarg-lane 2 7) ()
    "v128.load32_lane"         #x56 "av" "v" (:memarg-lane 4 3) ()
    "v128.load64_lane"         #x57 "av" "v" (:memarg-lane 8 1) ()
    "v128.store8_lane"         #x58 "av" ""  (:memarg-lane 1 15) ()
    "v128.store16_lane"        #x59 "av" ""  (:memarg-lane 2 7) ()
    "v128.store32_lane"        #x5a "av" ""  (:memarg-lane 4 3) ()
    "v128.store64_lane"        #x5b "av" ""  (:memarg-lane 8 1) ()
    "v128.load32_zero"         #x5c "a"  "v" (:memarg 4) ()
    "v128.load64_zero"         #x5d "a"  "v" (:memarg 8) ()
    ;; ---- f32x4/f64x2 lane rounding 0x60-0x6a, 0x74-0x75, 0x7a, 0x94 ----
    "f32x4.ceil"               #x60 "v"  "v" () :f-ceil
    "i8x16.neg"                #x61 "v"  "v" () :i-neg
    "i8x16.popcnt"             #x62 "v"  "v" () :i-popcnt
    "i8x16.all_true"           #x63 "v"  "i" () :all-true
    "i8x16.bitmask"            #x64 "v"  "i" () :bitmask
    "i8x16.narrow_i16x8_s"     #x65 "vv" "v" () :i-narrow-s
    "i8x16.narrow_i16x8_u"     #x66 "vv" "v" () :i-narrow-u
    "f32x4.floor"              #x67 "v"  "v" () :f-floor
    "f32x4.trunc"              #x68 "v"  "v" () :f-trunc
    "f32x4.nearest"            #x6a "v"  "v" () :f-nearest
    ;; ---- i8x16 shifts/arith 0x6b-0x7b ----
    "i8x16.shl"                #x6b "vi" "v" () :i-shl
    "i8x16.shr_s"              #x6c "vi" "v" () :i-shr-s
    "i8x16.shr_u"              #x6d "vi" "v" () :i-shr-u
    "i8x16.add"                #x6e "vv" "v" () :i-add
    "i8x16.add_sat_s"          #x6f "vv" "v" () :i-add-sat-s
    "i8x16.add_sat_u"          #x70 "vv" "v" () :i-add-sat-u
    "i8x16.sub"                #x71 "vv" "v" () :i-sub
    "i8x16.sub_sat_s"          #x72 "vv" "v" () :i-sub-sat-s
    "i8x16.sub_sat_u"          #x73 "vv" "v" () :i-sub-sat-u
    "f64x2.ceil"               #x74 "v"  "v" () :f-ceil
    "f64x2.floor"              #x75 "v"  "v" () :f-floor
    "i8x16.min_s"              #x76 "vv" "v" () :i-min-s
    "i8x16.min_u"              #x77 "vv" "v" () :i-min-u
    "i8x16.max_s"              #x78 "vv" "v" () :i-max-s
    "i8x16.max_u"              #x79 "vv" "v" () :i-max-u
    "f64x2.trunc"              #x7a "v"  "v" () :f-trunc
    "i8x16.avgr_u"             #x7b "vv" "v" () :i-avgr-u
    ;; ---- extadd pairwise 0x7c-0x7f ----
    "i16x8.extadd_pairwise_i8x16_s"  #x7c "v" "v" () :extadd-s
    "i16x8.extadd_pairwise_i8x16_u"  #x7d "v" "v" () :extadd-u
    "i32x4.extadd_pairwise_i16x8_s"  #x7e "v" "v" () :extadd-s
    "i32x4.extadd_pairwise_i16x8_u"  #x7f "v" "v" () :extadd-u
    ;; ---- i16x8 family 0x80-0x9f ----
    "i16x8.abs"                #x80 "v"  "v" () :i-abs
    "i16x8.neg"                #x81 "v"  "v" () :i-neg
    "i16x8.q15mulr_sat_s"      #x82 "vv" "v" () :q15mulr
    "i16x8.all_true"           #x83 "v"  "i" () :all-true
    "i16x8.bitmask"            #x84 "v"  "i" () :bitmask
    "i16x8.narrow_i32x4_s"     #x85 "vv" "v" () :i-narrow-s
    "i16x8.narrow_i32x4_u"     #x86 "vv" "v" () :i-narrow-u
    "i16x8.extend_low_i8x16_s" #x87 "v"  "v" () :ext-low-s
    "i16x8.extend_high_i8x16_s" #x88 "v" "v" () :ext-high-s
    "i16x8.extend_low_i8x16_u" #x89 "v" "v" () :ext-low-u
    "i16x8.extend_high_i8x16_u" #x8a "v" "v" () :ext-high-u
    "i16x8.shl"                #x8b "vi" "v" () :i-shl
    "i16x8.shr_s"              #x8c "vi" "v" () :i-shr-s
    "i16x8.shr_u"              #x8d "vi" "v" () :i-shr-u
    "i16x8.add"                #x8e "vv" "v" () :i-add
    "i16x8.add_sat_s"          #x8f "vv" "v" () :i-add-sat-s
    "i16x8.add_sat_u"          #x90 "vv" "v" () :i-add-sat-u
    "i16x8.sub"                #x91 "vv" "v" () :i-sub
    "i16x8.sub_sat_s"          #x92 "vv" "v" () :i-sub-sat-s
    "i16x8.sub_sat_u"          #x93 "vv" "v" () :i-sub-sat-u
    "f64x2.nearest"            #x94 "v"  "v" () :f-nearest
    "i16x8.mul"                #x95 "vv" "v" () :i-mul
    "i16x8.min_s"              #x96 "vv" "v" () :i-min-s
    "i16x8.min_u"              #x97 "vv" "v" () :i-min-u
    "i16x8.max_s"              #x98 "vv" "v" () :i-max-s
    "i16x8.max_u"              #x99 "vv" "v" () :i-max-u
    "i16x8.avgr_u"             #x9b "vv" "v" () :i-avgr-u
    "i16x8.extmul_low_i8x16_s" #x9c "vv" "v" () :extmul-low-s
    "i16x8.extmul_high_i8x16_s" #x9d "vv" "v" () :extmul-high-s
    "i16x8.extmul_low_i8x16_u" #x9e "vv" "v" () :extmul-low-u
    "i16x8.extmul_high_i8x16_u" #x9f "vv" "v" () :extmul-high-u
    ;; ---- i32x4 family 0xa0-0xbf ----
    "i32x4.abs"                #xa0 "v"  "v" () :i-abs
    "i32x4.neg"                #xa1 "v"  "v" () :i-neg
    "i32x4.all_true"           #xa3 "v"  "i" () :all-true
    "i32x4.bitmask"            #xa4 "v"  "i" () :bitmask
    "i32x4.extend_low_i16x8_s" #xa7 "v"  "v" () :ext-low-s
    "i32x4.extend_high_i16x8_s" #xa8 "v" "v" () :ext-high-s
    "i32x4.extend_low_i16x8_u" #xa9 "v" "v" () :ext-low-u
    "i32x4.extend_high_i16x8_u" #xaa "v" "v" () :ext-high-u
    "i32x4.shl"                #xab "vi" "v" () :i-shl
    "i32x4.shr_s"              #xac "vi" "v" () :i-shr-s
    "i32x4.shr_u"              #xad "vi" "v" () :i-shr-u
    "i32x4.add"                #xae "vv" "v" () :i-add
    "i32x4.sub"                #xb1 "vv" "v" () :i-sub
    "i32x4.mul"                #xb5 "vv" "v" () :i-mul
    "i32x4.min_s"              #xb6 "vv" "v" () :i-min-s
    "i32x4.min_u"              #xb7 "vv" "v" () :i-min-u
    "i32x4.max_s"              #xb8 "vv" "v" () :i-max-s
    "i32x4.max_u"              #xb9 "vv" "v" () :i-max-u
    "i32x4.dot_i16x8_s"        #xba "vv" "v" () :dot
    "i32x4.extmul_low_i16x8_s" #xbc "vv" "v" () :extmul-low-s
    "i32x4.extmul_high_i16x8_s" #xbd "vv" "v" () :extmul-high-s
    "i32x4.extmul_low_i16x8_u" #xbe "vv" "v" () :extmul-low-u
    "i32x4.extmul_high_i16x8_u" #xbf "vv" "v" () :extmul-high-u
    ;; ---- i64x2 family 0xc0-0xdf ----
    "i64x2.abs"                #xc0 "v"  "v" () :i-abs
    "i64x2.neg"                #xc1 "v"  "v" () :i-neg
    "i64x2.all_true"           #xc3 "v"  "i" () :all-true
    "i64x2.bitmask"            #xc4 "v"  "i" () :bitmask
    "i64x2.extend_low_i32x4_s" #xc7 "v"  "v" () :ext-low-s
    "i64x2.extend_high_i32x4_s" #xc8 "v" "v" () :ext-high-s
    "i64x2.extend_low_i32x4_u" #xc9 "v" "v" () :ext-low-u
    "i64x2.extend_high_i32x4_u" #xca "v" "v" () :ext-high-u
    "i64x2.shl"                #xcb "vi" "v" () :i-shl
    "i64x2.shr_s"              #xcc "vi" "v" () :i-shr-s
    "i64x2.shr_u"              #xcd "vi" "v" () :i-shr-u
    "i64x2.add"                #xce "vv" "v" () :i-add
    "i64x2.sub"                #xd1 "vv" "v" () :i-sub
    "i64x2.mul"                #xd5 "vv" "v" () :i-mul
    "i64x2.eq"                 #xd6 "vv" "v" () :i-eq
    "i64x2.ne"                 #xd7 "vv" "v" () :i-ne
    "i64x2.lt_s"               #xd8 "vv" "v" () :i-lt-s
    "i64x2.gt_s"               #xd9 "vv" "v" () :i-gt-s
    "i64x2.le_s"               #xda "vv" "v" () :i-le-s
    "i64x2.ge_s"               #xdb "vv" "v" () :i-ge-s
    "i64x2.extmul_low_i32x4_s" #xdc "vv" "v" () :extmul-low-s
    "i64x2.extmul_high_i32x4_s" #xdd "vv" "v" () :extmul-high-s
    "i64x2.extmul_low_i32x4_u" #xde "vv" "v" () :extmul-low-u
    "i64x2.extmul_high_i32x4_u" #xdf "vv" "v" () :extmul-high-u
    ;; ---- f32x4/f64x2 arithmetic 0xe0-0xf7 ----
    "f32x4.abs"                #xe0 "v"  "v" () :f-abs
    "f32x4.neg"                #xe1 "v"  "v" () :f-neg
    "f32x4.sqrt"               #xe3 "v"  "v" () :f-sqrt
    "f32x4.add"                #xe4 "vv" "v" () :f-add
    "f32x4.sub"                #xe5 "vv" "v" () :f-sub
    "f32x4.mul"                #xe6 "vv" "v" () :f-mul
    "f32x4.div"                #xe7 "vv" "v" () :f-div
    "f32x4.min"                #xe8 "vv" "v" () :f-min
    "f32x4.max"                #xe9 "vv" "v" () :f-max
    "f32x4.pmin"               #xea "vv" "v" () :f-pmin
    "f32x4.pmax"               #xeb "vv" "v" () :f-pmax
    "f64x2.abs"                #xec "v"  "v" () :f-abs
    "f64x2.neg"                #xed "v"  "v" () :f-neg
    "f64x2.sqrt"               #xef "v"  "v" () :f-sqrt
    "f64x2.add"                #xf0 "vv" "v" () :f-add
    "f64x2.sub"                #xf1 "vv" "v" () :f-sub
    "f64x2.mul"                #xf2 "vv" "v" () :f-mul
    "f64x2.div"                #xf3 "vv" "v" () :f-div
    "f64x2.min"                #xf4 "vv" "v" () :f-min
    "f64x2.max"                #xf5 "vv" "v" () :f-max
    "f64x2.pmin"               #xf6 "vv" "v" () :f-pmin
    "f64x2.pmax"               #xf7 "vv" "v" () :f-pmax
    ;; ---- conversions 0xf8-0xff, 0x5e/0x5f ----
    "i32x4.trunc_sat_f32x4_s"  #xf8 "v" "v" () :trunc-sat-s
    "i32x4.trunc_sat_f32x4_u"  #xf9 "v" "v" () :trunc-sat-u
    "f32x4.convert_i32x4_s"    #xfa "v" "v" () :conv-i32-s
    "f32x4.convert_i32x4_u"    #xfb "v" "v" () :conv-i32-u
    "i32x4.trunc_sat_f64x2_s_zero" #xfc "v" "v" () :trunc-sat-s-64
    "i32x4.trunc_sat_f64x2_u_zero" #xfd "v" "v" () :trunc-sat-u-64
    "f64x2.convert_low_i32x4_s" #xfe "v" "v" () :conv-low-i32-s
    "f64x2.convert_low_i32x4_u" #xff "v" "v" () :conv-low-i32-u
    "f32x4.demote_f64x2_zero"  #x5e "v" "v" () :demote
    "f64x2.promote_low_f32x4"  #x5f "v" "v" () :promote
    ;; ---- relaxed SIMD 0x100-0x113 (relaxed-simd Overview.md) ----
    ;; Canonical choices per op are documented at the exec site.
    "i8x16.relaxed_swizzle"    #x100 "vv" "v" () :r-swizzle
    "i32x4.relaxed_trunc_f32x4_s" #x101 "v" "v" () :trunc-sat-s
    "i32x4.relaxed_trunc_f32x4_u" #x102 "v" "v" () :trunc-sat-u
    "i32x4.relaxed_trunc_f64x2_s_zero" #x103 "v" "v" () :trunc-sat-s-64
    "i32x4.relaxed_trunc_f64x2_u_zero" #x104 "v" "v" () :trunc-sat-u-64
    "f32x4.relaxed_madd"       #x105 "vvv" "v" () :f-madd
    "f32x4.relaxed_nmadd"      #x106 "vvv" "v" () :f-nmadd
    "f64x2.relaxed_madd"       #x107 "vvv" "v" () :f-madd
    "f64x2.relaxed_nmadd"      #x108 "vvv" "v" () :f-nmadd
    "i8x16.relaxed_laneselect" #x109 "vvv" "v" () :laneselect
    "i16x8.relaxed_laneselect" #x10a "vvv" "v" () :laneselect
    "i32x4.relaxed_laneselect" #x10b "vvv" "v" () :laneselect
    "i64x2.relaxed_laneselect" #x10c "vvv" "v" () :laneselect
    "f32x4.relaxed_min"        #x10d "vv" "v" () :r-min
    "f32x4.relaxed_max"        #x10e "vv" "v" () :r-max
    "f64x2.relaxed_min"        #x10f "vv" "v" () :r-min
    "f64x2.relaxed_max"        #x110 "vv" "v" () :r-max
    "i16x8.relaxed_q15mulr_s"  #x111 "vv" "v" () :r-q15mulr
    "i16x8.relaxed_dot_i8x16_i7x16_s" #x112 "vv" "v" () :r-dot
    "i32x4.relaxed_dot_i8x16_i7x16_add_s" #x113 "vvv" "v" () :r-dot-add))

(defparameter *simd-entries*
  (let ((v '()) (h-sub (make-hash-table)) (h-name (make-hash-table :test #'equal))
        (h-op (make-hash-table)))
    (loop for (name sub params results imm class) on *simd-table* by #'cdddr-6
          do (multiple-value-bind (width lanes float-p) (%lane-shape name)
               (let* ((op (intern (string-upcase name) :keyword))
                      (e (make-simd-entry
                          :name name :sub sub :op op
                          :params (%param-decode params)
                          :results (%param-decode results)
                          :imm (if (eq imm '()) nil imm)
                          :class (or class
                                     (case (first imm)
                                       ((:memarg :memarg-lane) :mem)
                                       (:const :const)
                                       (:lanes :shuffle)
                                       (t nil)))
                          :lane-width width :lanes lanes :float-p float-p)))
                 (push e v)
                 (setf (gethash sub h-sub) e
                       (gethash name h-name) e
                       (gethash op h-op) e))))
    (coerce (nreverse v) 'vector)))

(defparameter *simd-by-sub* (make-hash-table))
(defparameter *simd-by-name* (make-hash-table :test #'equal))
(defparameter *simd-by-op* (make-hash-table))
(loop for e across *simd-entries*
      do (setf (gethash (simd-entry-sub e) *simd-by-sub*) e
               (gethash (simd-entry-name e) *simd-by-name*) e
               (gethash (simd-entry-op e) *simd-by-op*) e))

(defun simd-op-p (op)
  "Whether OP (an IR keyword) is a SIMD opcode."
  (nth-value 1 (gethash op *simd-by-op*)))

(defun entry-of (op)
  "The simd-entry for IR keyword OP."
  (or (gethash op *simd-by-op*)
      (error "simd: not a SIMD opcode ~a" op)))

(defun entry-by-sub (sub)
  "The simd-entry for 0xFD sub-opcode SUB, or NIL."
  (gethash sub *simd-by-sub*))

(defun entry-by-name (name)
  "The simd-entry for WAT spelling NAME (string), or NIL."
  (gethash name *simd-by-name*))

(defun text-op-p (name)
  "Whether WAT atom NAME is a SIMD opcode spelling."
  (nth-value 1 (gethash name *simd-by-name*)))

(defun plain-op-names ()
  "The immediate-free SIMD spellings (for the text parser's plain-op
table)."
  (loop for e across *simd-entries*
        when (null (simd-entry-imm e))
          collect (simd-entry-name e)))

(defun memarg-op-entries ()
  "The pure-memarg SIMD spellings with their natural widths, for the
text parser's memarg-op table: (name width-bytes)*. The
memarg+lane variants are handled by the SIMD immediate parser."
  (loop for e across *simd-entries*
        when (and (consp (simd-entry-imm e))
                  (eq (first (simd-entry-imm e)) :memarg))
          collect (list (simd-entry-name e)
                        (second (simd-entry-imm e)))))

(defun lane-imm-p (imm)
  "Whether IMM shape carries a laneidx (for the binary decoder)."
  (and (consp imm)
       (member (first imm) '(:lane :memarg-lane))))

(defun validate-lane-imms (imm imms verr)
  "Check lane immediate ranges, calling VERR (message . args) on
violation. IMM is the entry's imm shape."
  (when (and (consp imm) (member (first imm) '(:lane :memarg-lane)))
    (let ((max (if (eq (first imm) :lane) (second imm) (third imm)))
          (lane (getf imms :lane)))
      (unless (and (integerp lane) (>= lane 0) (<= lane max))
        (funcall verr "invalid lane index ~a (max ~a)" lane max))))
  (when (and (consp imm) (eq (first imm) :lanes))
    (loop for l across (getf imms :lanes)
          do (unless (and (integerp l) (>= l 0) (<= l 31))
               (funcall verr "invalid lane index ~a (max 31)" l)))))

;;; ---------------------------------------------------------------
;;; Scalarized execution (docs/design/PLAN.md 7.3: lane-by-lane over the byte
;;; backing; the v128 stays ONE object through every op).
;;;
;;; Argument order: values arrive in push order (last pushed = LAST
;;; param); EXEC takes (args...) in WAT operand order a b c.
;;; Address-based ops (loads/stores) are handled by the interp layer
;;; (they need the memory instance); EXEC handles only pure compute.

(declaim (inline i8-s i16-s i32-s i64-s))
(defun i8-s (x) (if (>= x #x80) (- x #x100) x))
(defun i16-s (x) (if (>= x #x8000) (- x #x10000) x))
(defun i32-s (x) (if (>= x #x80000000) (- x #x100000000) x))
(defun i64-s (x) (if (>= x #x8000000000000000) (- x #x10000000000000000) x))

(declaim (inline u8-of u16-of u32-of u64-of))
(defun u8-of (x) (logand x #xff))
(defun u16-of (x) (logand x #xffff))
(defun u32-of (x) (logand x #xffffffff))
(defun u64-of (x) (logand x #xffffffffffffffff))

(defun %sat-s (x width)
  "Saturate X to the signed WIDTH-bit domain."
  (let ((min (- (ash 1 (1- width)))) (max (1- (ash 1 (1- width)))))
    (min max (max min x))))

(defun %sat-u (x width)
  "Saturate X to the unsigned WIDTH-bit domain."
  (let ((max (1- (ash 1 width))))
    (min max (max 0 x))))

;;; Lane-wise map helpers: build a fresh v128 by per-lane compute.
(defun %map1 (v width lanes fn)
  (declare (type (simple-array (unsigned-byte 8) (16)) v))
  (let ((r (make-v128)))
    (loop for i below lanes
          do (set-lane-u r i width (mod (funcall fn (lane-u v i width))
                                        (ash 1 width))))
    r))

(defun %map2 (a b width lanes fn)
  (declare (type (simple-array (unsigned-byte 8) (16)) a b))
  (let ((r (make-v128)))
    (loop for i below lanes
          do (set-lane-u r i width (mod (funcall fn (lane-u a i width)
                                                   (lane-u b i width))
                                        (ash 1 width))))
    r))

(defun %map3 (a b c width lanes fn)
  (declare (type (simple-array (unsigned-byte 8) (16)) a b c))
  (let ((r (make-v128))
        (mask (1- (ash 1 width))))
    (declare (type (integer 0) mask))
    (loop for i below lanes
          do (set-lane-u r i width
                         (logand mask (funcall fn (lane-u a i width)
                                                (lane-u b i width)
                                                (lane-u c i width)))))
    r))

;;; Float lane views: f32x4 lanes are 32-bit bit patterns through
;;; wasmdown.numops; f64x2 likewise.
(defun %f32 (bits) (wasmdown.numops:f32-from-bits bits))
(defun %f32b (x) (wasmdown.numops:f32-bits x))
(defun %f64 (bits) (wasmdown.numops:f64-from-bits bits))
(defun %f64b (x) (wasmdown.numops:f64-bits x))

(defun %fm (v width lanes fbits finv ffn)
  "Map unary float FN over lanes: lane -> bits -> float -> result."
  (declare (type (simple-array (unsigned-byte 8) (16)) v)
           (type (integer 1 64) width))
  (let ((r (make-v128)))
    (loop for i below lanes
          do (set-lane-u r i width
                         (funcall fbits (funcall ffn (funcall finv
                                                            (lane-u v i width))))))
    r))

(defun %fcm2 (a b width lanes f32-p ffn)
  "Float LANEWISE COMPARISON: the result lanes are MASKS (all-ones on
true, zero on false), never floats — the old dispatch ran the compare
through the float map, which fed the 0xFFFFFFFF mask back into
F32BITS and trapped on the first assertion."
  (let ((inv (if f32-p #'%f32 #'%f64))
        (mask (1- (ash 1 width))))
    (%map2 a b width lanes
           (lambda (x y)
             (if (funcall ffn (funcall inv x) (funcall inv y)) mask 0)))))

(defun %fm2 (a b width lanes fbits finv ffn)
  "Map binary float FN over lanes."
  (declare (type (simple-array (unsigned-byte 8) (16)) a b)
           (type (integer 1 64) width))
  (let ((r (make-v128)))
    (loop for i below lanes
          do (set-lane-u r i width
                         (funcall fbits
                                  (funcall ffn (funcall finv (lane-u a i width))
                                           (funcall finv (lane-u b i width))))))
    r))

;;; 128-bit whole-vector integer ops (v128.and etc.): fold two
;;; 64-bit halves.
(declaim (inline %half %set-half))
(defun %half (v k)
  (logior (aref v (* k 8))
          (ash (aref v (+ (* k 8) 1)) 8)
          (ash (aref v (+ (* k 8) 2)) 16)
          (ash (aref v (+ (* k 8) 3)) 24)
          (ash (aref v (+ (* k 8) 4)) 32)
          (ash (aref v (+ (* k 8) 5)) 40)
          (ash (aref v (+ (* k 8) 6)) 48)
          (ash (aref v (+ (* k 8) 7)) 56)))

(defun %set-half (v k x)
  (loop for j below 8
        do (setf (aref v (+ (* k 8) j))
                 (logand #xff (ash x (- (* 8 j))))))
  v)

(defun %vmap128 (a b fn-64)
  "Whole-vector binary op via 64-bit halves."
  (declare (type (simple-array (unsigned-byte 8) (16)) a b))
  (let ((r (make-v128)))
    (%set-half r 0 (funcall fn-64 (%half a 0) (%half b 0)))
    (%set-half r 1 (funcall fn-64 (%half a 1) (%half b 1)))
    r))

(defun %vnot128 (a)
  "Whole-vector bitwise NOT."
  (declare (type (simple-array (unsigned-byte 8) (16)) a))
  (let ((r (make-v128)))
    (%set-half r 0 (lognot (%half a 0)))
    (%set-half r 1 (lognot (%half a 1)))
    r))

(defun %vbitselect (a b c)
  "v128.bitselect: (a & c) | (b & ~c), whole-vector via 64-bit halves."
  (declare (type (simple-array (unsigned-byte 8) (16)) a b c))
  (let ((r (make-v128)))
    (flet ((bs (x y z)
             (logior (logand x z) (logand y (lognot z)))))
      (%set-half r 0 (bs (%half a 0) (%half b 0) (%half c 0)))
      (%set-half r 1 (bs (%half a 1) (%half b 1) (%half c 1))))
    r))

(declaim (inline sv-at))
(defun sv-at (v i width)
  "Lane I of V as a SIGNED integer (WIDTH bits)."
  (let ((x (lane-u v i width)))
    (case width
      (8 (i8-s x)) (16 (i16-s x)) (32 (i32-s x)) (t (i64-s x)))))

;;; ---------------------------------------------------------------
;;; EXEC: the pure-compute dispatch. CLASS comes from the entry; A/B/C
;;; are the operand v128s (or scalars for splat/replace/shuffle imms).
;;; Loads/stores are NOT here (the interp handles them).

(defun exec (entry a &optional b c)
  "Execute the pure-compute SIMD op ENTRY (a simd-entry) on operands
A B C. Operand convention: WAT order — A is the first operand. Pure
lane ops receive v128 byte vectors; the scalar-shaped ops (splat,
extract/replace, shifts, shuffle) receive their scalars through the
ENTRY's class dispatch (the interp passes them; see EXEC-OP for the
imms plumbing). Loads/stores and the reductions (all_true/any_true/
bitmask -> i32) are handled by the interp layer, NOT here."
  (let* ((class (simd-entry-class entry))
         (width (or (simd-entry-lane-width entry) 128))
         (lanes (or (simd-entry-lanes entry) 1))
         (f32-p (and (simd-entry-float-p entry) (= width 32)))
         (mask (1- (ash 1 width))))
    (declare (type (integer 1 128) width)
             (type (integer 1 16) lanes)
             (type (integer 0) mask))
    (labels ((cmp-mask (x) (if x mask 0))
             (sv (x) (case width
                       (8 (i8-s x)) (16 (i16-s x))
                       (32 (i32-s x)) (t (i64-s x))))
             (i-b2 (fn) (%map2 a b width lanes fn))
             (i-u1 (fn) (%map1 a width lanes fn))
             (f1 (fn) (%fm a width lanes (if f32-p #'%f32b #'%f64b)
                          (if f32-p #'%f32 #'%f64) fn))
             (f2 (fn) (%fm2 a b width lanes (if f32-p #'%f32b #'%f64b)
                            (if f32-p #'%f32 #'%f64) fn)))
      (ecase class
        ((:mem :const :shuffle :swizzle :splat :extract :extract-s
               :extract-u :replace :any-true :all-true :bitmask
               :r-swizzle)
         ;; handled by the interp layer (memory / immediates /
         ;; reductions / splat-replace plumbing)
         (error "simd exec: class ~a not a pure lane op" class))
        ;; ---- integer lanewise ----
        (:i-add (i-b2 #'+))
        (:i-sub (i-b2 #'-))
        (:i-mul (i-b2 #'*))
        (:i-abs (i-u1 (lambda (x) (abs (if (= width 8) (i8-s x)
                                           (if (= width 16) (i16-s x)
                                               (if (= width 32) (i32-s x)
                                                   (i64-s x))))))))
        (:i-neg (i-u1 #'-))
        (:i-popcnt (i-u1 #'logcount))
        (:i-eq (i-b2 (lambda (x y) (cmp-mask (= x y)))))
        (:i-ne (i-b2 (lambda (x y) (cmp-mask (/= x y)))))
        (:i-lt-s (i-b2 (lambda (x y) (cmp-mask (< (sv x) (sv y))))))
        (:i-lt-u (i-b2 (lambda (x y) (cmp-mask (< x y)))))
        (:i-gt-s (i-b2 (lambda (x y) (cmp-mask (> (sv x) (sv y))))))
        (:i-gt-u (i-b2 (lambda (x y) (cmp-mask (> x y)))))
        (:i-le-s (i-b2 (lambda (x y) (cmp-mask (<= (sv x) (sv y))))))
        (:i-le-u (i-b2 (lambda (x y) (cmp-mask (<= x y)))))
        (:i-ge-s (i-b2 (lambda (x y) (cmp-mask (>= (sv x) (sv y))))))
        (:i-ge-u (i-b2 (lambda (x y) (cmp-mask (>= x y)))))
        (:i-min-s (i-b2 (lambda (x y) (min (sv x) (sv y)))))
        (:i-min-u (i-b2 #'min))
        (:i-max-s (i-b2 (lambda (x y) (max (sv x) (sv y)))))
        (:i-max-u (i-b2 #'max))
        (:i-avgr-u (i-b2 (lambda (x y) (floor (+ x y 1) 2)))
         ;; (ash 1 width) domain: mod applied by %map2
         )
        (:i-add-sat-s (i-b2 (lambda (x y) (%sat-s (+ (sv x) (sv y)) width))))
        (:i-add-sat-u (i-b2 (lambda (x y) (%sat-u (+ x y) width))))
        (:i-sub-sat-s (i-b2 (lambda (x y) (%sat-s (- (sv x) (sv y)) width))))
        (:i-sub-sat-u (i-b2 (lambda (x y) (%sat-u (- x y) width))))
        (:i-narrow-s
         ;; i8x16.narrow_i16x8_s: the ENTRY shape is the RESULT
         ;; shape (width 8, lanes 16); SOURCE lanes are width*2 (i16),
         ;; read as SIGNED regardless of the _s/_u suffix; the
         ;; suffix picks the saturation domain.
         ;; RESULT: LANES lanes of WIDTH bits (16 x i8); SOURCE:
         ;; LANES/2 lanes of WIDTH*2 (8 x i16) per operand.
         (let* ((sw (* width 2)) (r (make-v128)) (n2 (ash lanes -1)))
           (loop for i below n2
                 do (set-lane-u r i width
                                (mod (%sat-s (sv-at a i sw) width)
                                     (ash 1 width))))
           (loop for i below n2
                 do (set-lane-u r (+ i n2) width
                                (mod (%sat-s (sv-at b i sw) width)
                                     (ash 1 width))))
           r))
        (:i-narrow-u
         (let* ((sw (* width 2)) (r (make-v128)) (n2 (ash lanes -1)))
           (loop for i below n2
                 do (set-lane-u r i width
                                (mod (%sat-u (sv-at a i sw) width)
                                     (ash 1 width))))
           (loop for i below n2
                 do (set-lane-u r (+ i n2) width
                                (mod (%sat-u (sv-at b i sw) width)
                                     (ash 1 width))))
           r))
        (:q15mulr
         ;; i16x8.q15mulr_sat_s: signed saturate ((x*y + 0x4000) >> 15)
         (%map2 a b 16 8 (lambda (x y)
                           (%sat-s (ash (+ (* (i16-s x) (i16-s y)) #x4000) -15)
                                   16))))
        (:extadd-s
         ;; pairwise widen: result lanes = lanes, width = 2*input
         (let* ((sw (ash width -1)) (r (make-v128)))
           (declare (type (integer 1 64) sw))
           (loop for i below lanes
                 do (set-lane-u r i width
                                (mod (+ (sv-at a (* 2 i) sw)
                                        (sv-at a (1+ (* 2 i)) sw))
                                     (ash 1 width))))
           r))
        (:extadd-u
         (let* ((sw (ash width -1)) (r (make-v128)))
           (declare (type (integer 1 64) sw))
           (loop for i below lanes
                 do (set-lane-u r i width
                                (mod (+ (lane-u a (* 2 i) sw)
                                        (lane-u a (1+ (* 2 i)) sw))
                                     (ash 1 width))))
           r))
        (:ext-low-s (let ((r (make-v128)))
                      (loop for i below lanes
                            do (set-lane-u r i width (sv-at a i (ash width -1))))
                      r))
        (:ext-high-s (let ((r (make-v128)))
                       (loop for i below lanes
                             do (set-lane-u r i width
                                            (sv-at a (+ i lanes) (ash width -1))))
                       r))
        (:ext-low-u (let ((r (make-v128)))
                      (loop for i below lanes
                            do (set-lane-u r i width
                                           (lane-u a i (ash width -1))))
                      r))
        (:ext-high-u (let ((r (make-v128)))
                       (loop for i below lanes
                             do (set-lane-u r i width
                                            (lane-u a (+ i lanes) (ash width -1))))
                       r))
        (:extmul-low-s
         (let* ((sw (ash width -1)) (r (make-v128)))
           (loop for i below lanes
                 do (set-lane-u r i width
                                (mod (* (sv-at a i sw) (sv-at b i sw))
                                     (ash 1 width))))
           r))
        (:extmul-high-s
         (let* ((sw (ash width -1)) (r (make-v128)))
           (loop for i below lanes
                 do (set-lane-u r i width
                                (mod (* (sv-at a (+ i lanes) sw)
                                        (sv-at b (+ i lanes) sw))
                                     (ash 1 width))))
           r))
        (:extmul-low-u
         (let* ((sw (ash width -1)) (r (make-v128)))
           (loop for i below lanes
                 do (set-lane-u r i width
                                (mod (* (lane-u a i sw) (lane-u b i sw))
                                     (ash 1 width))))
           r))
        (:extmul-high-u
         (let* ((sw (ash width -1)) (r (make-v128)))
           (loop for i below lanes
                 do (set-lane-u r i width
                                (mod (* (lane-u a (+ i lanes) sw)
                                        (lane-u b (+ i lanes) sw))
                                     (ash 1 width))))
           r))
        (:dot
         ;; i32x4.dot_i16x8_s: pairs of i16 products -> i32
         (let ((r (make-v128)))
           (loop for i below 4
                 do (set-lane-u r i 32
                                (mod (+ (* (i16-s (lane-u a (* 2 i) 16))
                                           (i16-s (lane-u b (* 2 i) 16)))
                                        (* (i16-s (lane-u a (1+ (* 2 i)) 16))
                                           (i16-s (lane-u b (1+ (* 2 i)) 16))))
                                     #x100000000)))
           r))
        ;; ---- shifts: B is the i32 scalar (interp passes it raw) ----
        (:i-shl (let ((r (make-v128)) (k (mod b width)))
                  (loop for i below lanes
                        do (set-lane-u r i width (mod (ash (lane-u a i width) k)
                                                     (ash 1 width))))
                  r))
        (:i-shr-s (let ((r (make-v128)) (k (mod b width)))
                    (loop for i below lanes
                          do (set-lane-u r i width
                                         (mod (ash (sv-at a i width) (- k))
                                              (ash 1 width))))
                    r))
        (:i-shr-u (let ((r (make-v128)) (k (mod b width)))
                    (loop for i below lanes
                          do (set-lane-u r i width
                                         (mod (ash (lane-u a i width) (- k))
                                              (ash 1 width))))
                    r))
        ;; ---- v128 bitwise ----
        (:v-not (%vnot128 a))
        (:v-and (%vmap128 a b #'logand))
        (:v-andnot (%vmap128 a b (lambda (x y) (logand x (lognot y)))))
        (:v-or (%vmap128 a b #'logior))
        (:v-xor (%vmap128 a b #'logxor))
        (:bitselect (%vbitselect a b c))
        (:laneselect
         ;; RELAXED laneselect: CANONICAL CHOICE = v128.bitselect
         ;; semantics on every mask (the spec admits returning the
         ;; all-ones/zero-mask operand's lane instead when the mask
         ;; is mixed).
         (%vbitselect a b c))
        ;; ---- float lanewise ----
        (:f-abs (f1 #'abs))
        (:f-neg (f1 #'-))
        (:f-sqrt (f1 (lambda (x)
                       (if (and (minusp x) (not (zerop x)))
                           (if f32-p
                               (wasmdown.numops:f32-from-bits #x7fc00000)
                               (wasmdown.numops:f64-from-bits
                                #x7ff8000000000000))
                           (sqrt x)))))
        (:f-ceil (f1 (lambda (x)
                      (if (wasmdown.numops:nan-p x)
                          (wasmdown.numops:quiet-nan x)
                          (fceiling x)))))
        (:f-floor (f1 (lambda (x)
                       (if (wasmdown.numops:nan-p x)
                           (wasmdown.numops:quiet-nan x)
                           (ffloor x)))))
        (:f-trunc (f1 #'wasmdown.numops:ftrunc))
        (:f-nearest (f1 #'wasmdown.numops:fnearest))
        (:f-add (f2 #'+))
        (:f-sub (f2 #'-))
        (:f-mul (f2 #'*))
        (:f-div (f2 #'/))
        (:f-min (f2 #'wasmdown.numops:fmin))
        (:f-max (f2 #'wasmdown.numops:fmax))
        (:f-pmin (f2 (lambda (x y) (if (< y x) y x))))
        (:f-pmax (f2 (lambda (x y) (if (< x y) y x))))
        (:f-eq (%fcm2 a b width lanes f32-p #'=))
        (:f-ne (%fcm2 a b width lanes f32-p #'/=))
        (:f-lt (%fcm2 a b width lanes f32-p #'<))
        (:f-gt (%fcm2 a b width lanes f32-p #'>))
        (:f-le (%fcm2 a b width lanes f32-p #'<=))
        (:f-ge (%fcm2 a b width lanes f32-p #'>=))
        (:f-madd
         ;; relaxed madd: CANONICAL CHOICE = the plain two-rounding
         ;; a*b+c (the spec admits fused single-rounding too).
         (let ((r (make-v128)))
           (loop for i below lanes
                 do (set-lane-u r i width
                                (funcall (if f32-p #'%f32b #'%f64b)
                                         (+ (* (funcall (if f32-p #'%f32 #'%f64)
                                                        (lane-u a i width))
                                               (funcall (if f32-p #'%f32 #'%f64)
                                                        (lane-u b i width)))
                                            (funcall (if f32-p #'%f32 #'%f64)
                                                     (lane-u c i width))))))
           r))
        (:f-nmadd
         ;; relaxed nmadd: canonical = plain (- (a*b)) + c.
         (let ((r (make-v128)))
           (loop for i below lanes
                 do (set-lane-u r i width
                                (funcall (if f32-p #'%f32b #'%f64b)
                                         (+ (- (* (funcall (if f32-p #'%f32 #'%f64)
                                                          (lane-u a i width))
                                                  (funcall (if f32-p #'%f32 #'%f64)
                                                           (lane-u b i width))))
                                            (funcall (if f32-p #'%f32 #'%f64)
                                                     (lane-u c i width))))))
           r))
        ((:r-min :r-max)
         ;; RELAXED min/max: CANONICAL CHOICE = the non-relaxed
         ;; scalar semantics (NaN -> canonical NaN, -0/+0 ordered per
         ;; fmin/fmax). The spec admits returning either operand on
         ;; NaN or -0/+0 input lanes.
         (f2 (if (eq class :r-min) #'wasmdown.numops:fmin
                 #'wasmdown.numops:fmax)))
        (:r-q15mulr
         ;; relaxed q15mulr: CANONICAL = the saturating path
         ;; (i16x8.q15mulr_sat_s); the spec admits wrapping the
         ;; INT16_MIN*INT16_MIN overflow lane instead.
         (%map2 a b 16 8 (lambda (x y)
                           (%sat-s (ash (+ (* (i16-s x) (i16-s y)) #x4000) -15)
                                   16))))
        ((:trunc-sat-s :trunc-sat-u :trunc-sat-s-64 :trunc-sat-u-64)
         ;; Both the NON-relaxed trunc_sat ops and their relaxed
         ;; twins land here; the relaxed ones document their
         ;; alternative outcomes (NaN->0, overflow->MIN/MAX) while
         ;; the canonical choice is full saturation.
         ;; Entry width = the FLOAT input's width (32: f32x4->i32x4,
         ;; 64: f64x2->i32x2 with zero high lanes).
         (let* ((f64-in-p (>= width 64))
                (in-w (if f64-in-p 64 32))
                (in-lanes (if f64-in-p 2 4))
                (signed-p (or (eq class :trunc-sat-s)
                              (eq class :trunc-sat-s-64)))
                (r (make-v128)))
           (flet ((sat-lane (x)
                    (cond
                     ((wasmdown.numops:nan-p x) 0)
                     ((sb-ext:float-infinity-p x)
                      (if (plusp x)
                          (if signed-p #x7fffffff #xffffffff)
                          (if signed-p #x80000000 0)))
                     (t
                      (let* ((n (round (ftruncate x)))
                             (n (if signed-p
                                    (min #x7fffffff (max #x-80000000 n))
                                    (min #xffffffff (max 0 n)))))
                        (mod n #x100000000))))))
             (loop for i below in-lanes
                   do (let ((f (funcall (if f64-in-p #'%f64 #'%f32)
                                        (lane-u a i in-w))))
                        (set-lane-u r i 32 (sat-lane f))))
             ;; f64x2 sources: lanes 2-3 of R stay zero (fresh
             ;; vector); f32x4 sources fill all four lanes.
             r)))
        (:conv-i32-s
         ;; f32x4.convert_i32x4_s
         (let ((r (make-v128)))
           (loop for i below 4
                 do (set-lane-u r i 32 (%f32b (wasmdown.numops:f32-convert-i32-s
                                                (lane-u a i 32)))))
           r))
        (:conv-i32-u
         (let ((r (make-v128)))
           (loop for i below 4
                 do (set-lane-u r i 32 (%f32b (wasmdown.numops:f32-convert-i32-u
                                                (lane-u a i 32)))))
           r))
        (:conv-low-i32-s
         (let ((r (make-v128)))
           (loop for i below 2
                 do (set-lane-u r i 64 (%f64b (wasmdown.numops:f64-convert-i32-s
                                                (lane-u a i 32)))))
           r))
        (:conv-low-i32-u
         (let ((r (make-v128)))
           (loop for i below 2
                 do (set-lane-u r i 64 (%f64b (wasmdown.numops:f64-convert-i32-u
                                                (lane-u a i 32)))))
           r))
        (:demote
         ;; f32x4.demote_f64x2_zero: two low lanes; upper lanes 0
         (let ((r (make-v128)))
           (loop for i below 2
                 do (set-lane-u r i 32
                                (%f32b (wasmdown.numops:f32-demote-f64
                                        (%f64 (lane-u a i 64))))))
           r))
        (:promote
         (let ((r (make-v128)))
           (loop for i below 2
                 do (set-lane-u r i 64
                                (%f64b (wasmdown.numops:f64-promote-f32
                                        (%f32 (lane-u a i 32))))))
           r))
        (:r-dot
         ;; i16x8.relaxed_dot_i8x16_i7x16_s: CANONICAL = treat the
         ;; b operand's lanes as SIGNED 7-bit (b & 0x7f, the
         ;; guaranteed-valid reading) with saturation per pair.
         (let ((r (make-v128)))
           (loop for i below 8
                 do (let* ((x (i8-s (lane-u a (* 2 i) 8)))
                           (y (i8-s (logand (lane-u b (* 2 i) 8) #x7f)))
                           (x2 (i8-s (lane-u a (1+ (* 2 i)) 8)))
                           (y2 (i8-s (logand (lane-u b (1+ (* 2 i)) 8) #x7f)))
                           (p1 (* x y)) (p2 (* x2 y2))
                           (s (+ p1 p2)))
                      (set-lane-u r i 16 (mod (%sat-s s 16) #x10000))))
           r))
        (:r-dot-add
         ;; i32x4.relaxed_dot_i8x16_i7x16_add_s: canonical = the
         ;; 7-bit reading, saturating 16-bit pairs, + c lanes.
         (let ((r (make-v128)))
           (loop for i below 4
                 do (let ((acc 0))
                      (loop for j below 4
                            do (let* ((k (+ (* 4 i) (* 2 j)))
                                      (x (i8-s (lane-u a k 8)))
                                      (y (i8-s (logand (lane-u b k 8) #x7f)))
                                      (x2 (i8-s (lane-u a (1+ k) 8)))
                                      (y2 (i8-s (logand (lane-u b (1+ k) 8) #x7f))))
                                 (incf acc (mod (%sat-s (+ (* x y) (* x2 y2))
                                                            16)
                                                 #x10000))))
                      (set-lane-u r i 32 (mod (+ acc (lane-u c i 32))
                                              #x100000000))))
           r))
        ;; ---- narrowed-lane masks for the float comparisons are
        ;; handled by width above (32/64 = the f32x4/f64x2 shapes).
        (:extract (error "simd exec: extract via interp"))
        (:replace (error "simd exec: replace via interp"))))))

;;; ---------------------------------------------------------------
;;; Binary immediate decoding (the 0xFD family's operand grammar,
;;; BinarySIMD.md: memarg via the host %read-memarg; v128.const =
;;; 16 raw bytes; laneidx bytes with entry-specific bounds; the
;;; shuffle's 16 ImmLaneIdx32 bytes).

(defun decode-simd-imms (r entry)
  "Decode the immediates of ENTRY from reader R (wasmdown.binary's
reader). Returns the imms plist. Lane bounds are DECODE-time
enforced (a laneidx byte above the shape's max is malformed per the
binary grammar's ImmLaneIdxN definition)."
  (let ((imm (simd-entry-imm entry)))
    (cond
     ((null imm) nil)
     ((and (consp imm) (eq (first imm) :const))
      (list :bytes (wasmdown.binary:read-bytes r 16)))
     ((and (consp imm) (eq (first imm) :memarg))
      (wasmdown.binary:read-memarg r))
     ((and (consp imm) (eq (first imm) :memarg-lane))
      (append (wasmdown.binary:read-memarg r)
              (list :lane (%read-laneidx r (third imm)))))
     ((and (consp imm) (eq (first imm) :lane))
      (list :lane (%read-laneidx r (second imm))))
     ((and (consp imm) (eq (first imm) :lanes))
      (list :lanes (loop repeat 16
                         collect (%read-laneidx r 31))))
     (t (error "simd: bad imm shape ~a" imm)))))

(defun %read-laneidx (r max)
  "One laneidx byte, bounds-checked at DECODE (ImmLaneIdxN)."
  (let ((b (wasmdown.binary:read-byte-at r)))
    (unless (<= b max)
      (wasmdown.binary:decode-error-at
       (wasmdown.binary:reader-offset r)
       "invalid lane index ~d (max ~d)" b max))
    b))


;;; ---------------------------------------------------------------
;;; Binary immediate ENCODING — the exact mirror of
;;; decode-simd-imms (round-trip contract: encode(decode(bytes))
;;; reproduces the canonical byte sequence).

(defun encode-simd-imms (e entry imms)
  "Emit ENTRY's immediates from the decoded plist IMMS to encoder E
(wasmdown.binary's encoder). Mirrors decode-simd-imms."
  (let ((imm (simd-entry-imm entry)))
    (cond
     ((null imm) nil)
     ((and (consp imm) (eq (first imm) :const))
      (let ((bytes (getf imms :bytes)))
        (loop for i below 16
              do (wasmdown.binary:enc-byte e (aref bytes i)))))
     ((and (consp imm) (member (first imm) '(:memarg :memarg-lane)))
      (wasmdown.binary:enc-memarg e imms)
      (when (eq (first imm) :memarg-lane)
        (wasmdown.binary:enc-byte e (getf imms :lane))))
     ((and (consp imm) (eq (first imm) :lane))
      (wasmdown.binary:enc-byte e (getf imms :lane)))
     ((and (consp imm) (eq (first imm) :lanes))
      (loop for l across (getf imms :lanes)
            do (wasmdown.binary:enc-byte e l)))
     (t (error "simd encode: bad imm shape ~a" imm)))))
