;;;; SIMD pins (loop 24): v128 + relaxed SIMD end to end — decode,
;;;; validate, encode round-trip, and interp execution, per family.
;;;; Every pin pair: a POSITIVE computation and a NEGATIVE (type
;;;; mismatch / trap / validation error). Self-authored: the vendored
;;;; suite has no SIMD fixtures (the one obsolete-keywords row is a
;;;; malformed-text probe that now passes at DECODE).

(in-package #:wasmdown.test)

;;; Helpers: build a module from WAT and invoke an export.

(defun %simd-mod (wat)
  (let ((m (wasmdown.text:parse-wat wat)))
    (wasmdown:validate-module m)
    m))

(defun %simd-run (wat export &rest args)
  (let ((inst (wasmdown:instantiate (%simd-mod wat))))
    (apply #'wasmdown:invoke inst export args)))

(defun %v128 (&rest bytes)
  (coerce bytes '(vector (unsigned-byte 8))))

(defun %simd-invalid-p (wat)
  "The module must be INVALID at validation."
  (let ((m (wasmdown.text:parse-wat wat)))
    (handler-case
        (progn (wasmdown:validate-module m) nil)
      (wasmdown:validation-error () t))))

(defun %simd-malformed-p (wat)
  "The text must be MALFORMED at decode."
  (handler-case
      (progn (wasmdown.text:parse-wat wat) nil)
    (wasmdown:decode-error () t)))

(defun %simd-trap-p (wat export &rest args)
  (handler-case
      (progn (apply #'%simd-run wat export args) nil)
    (wasmdown:wasm-trap () t)))

;;; ================================================================
;;; Construct / extract family.

(wasmdown.test:test simd/const-roundtrip
  ;; v128.const parses, validates, encodes/decodes byte-stable.
  (let* ((m (%simd-mod
             "(module (func (export \"f\") (result v128)
                (v128.const i32x4 1 2 3 4)))"))
         (b1 (wasmdown.binary:encode-module m))
         (d (wasmdown.binary:decode-module b1))
         (b2 (wasmdown.binary:encode-module d)))
    (wasmdown.test:is (equalp b1 b2))
    (let ((inst (wasmdown:instantiate d)))
      (wasmdown.test:is (equalp (list (%v128 1 0 0 0 2 0 0 0 3 0 0 0 4 0 0 0))
                                (wasmdown:invoke inst "f"))))))

(wasmdown.test:test simd/const-f64x2-and-float-shapes
  ;; f64x2/f32x4 const literals take the float bit encodings.
  (let ((r (%simd-run
            "(module (func (export \"f\") (result v128)
               (v128.const f64x2 1.0 2.0)))"
            "f")))
    (wasmdown.test:is (equalp (list (%v128 0 0 0 0 0 0 #xf0 #x3f
                                       0 0 0 0 0 0 #x00 #x40))
                              r))))

(wasmdown.test:test simd/splat-extract-replace
  ;; i32x4.splat + extract_lane + replace_lane.
  (wasmdown.test:is (equalp '(42)
                            (%simd-run
                             "(module (func (export \"f\") (param i32) (result i32)
                                (i32x4.extract_lane 2 (i32x4.splat (local.get 0)))))"
                             "f" 42)))
  (wasmdown.test:is
   (equalp (list (%v128 0 0 0 0 99 0 0 0 0 0 0 0 0 0 0 0))
           (%simd-run
            "(module (func (export \"f\") (param i32) (result v128)
               (i32x4.replace_lane 1 (v128.const i32x4 0 0 0 0)
                                   (local.get 0))))"
            "f" 99))))

(wasmdown.test:test simd/extract-lane-oob-rejected
  ;; NEGATIVE: laneidx 4 on i32x4 (ImmLaneIdx4 max 3) is malformed
  ;; at decode (the binary grammar's ImmLaneIdxN bound) / rejected
  ;; in text at the lane parser.
  (wasmdown.test:is (%simd-malformed-p
                     "(module (func (export \"f\") (result i32)
                        (i32x4.extract_lane 4 (v128.const i32x4 0 0 0 0))))")))

(wasmdown.test:test simd/extract-lane-wrong-operand-type
  ;; NEGATIVE: extracting from an i32 operand is a validation error.
  (wasmdown.test:is (%simd-invalid-p
                     "(module (func (export \"f\") (result i32)
                        (i32x4.extract_lane 0 (i32.const 0))))")))

;;; ================================================================
;;; Integer lane arithmetic.

(wasmdown.test:test simd/i32x4-add-sub
  (wasmdown.test:is
   (equalp (list (%v128 12 0 0 0 14 0 0 0 16 0 0 0 18 0 0 0))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (i32x4.add (v128.const i32x4 5 6 7 8)
                          (v128.const i32x4 7 8 9 10))))"
            "f")))
  (wasmdown.test:is
   (equalp (list (%v128 #xfe #xff #xff #xff 0 0 0 0 0 0 0 0 0 0 0 0))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (i32x4.sub (v128.const i32x4 1 0 0 0)
                          (v128.const i32x4 3 0 0 0))))"
            "f"))))

(wasmdown.test:test simd/i8x16-narrow-saturates
  ;; narrow_i16x8_s: 300 -> 0x7f (127), -300 -> 0x80 (-128).
  (wasmdown.test:is
   (equalp (list (%v128 #x7f #x80 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (i8x16.narrow_i16x8_s
                (v128.const i32x4 0xfed4012c 0 0 0)
                (v128.const i32x4 0 0 0 0))))"
            "f"))))

(wasmdown.test:test simd/i8x16-comparisons-mask
  ;; eq produces all-ones masks per lane.
  (wasmdown.test:is
   (equalp (list (%v128 #xff #xff #xff #xff 0 #xff #xff #xff #xff #xff #xff #xff 0 #xff #xff #xff))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (i8x16.eq (v128.const i32x4 1 1 1 1)
                         (v128.const i32x4 1 2 1 2))))"
            "f"))))

(wasmdown.test:test simd/i16x8-min-s-avgr
  (wasmdown.test:is
   (equalp '(4294967291)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i16x8.extract_lane_s 0
                 (i16x8.min_s (v128.const i32x4 0x0001fffb 0 0 0)
                              (v128.const i32x4 0x00010001 0 0 0)))))"
            "f")))
  ;; avgr_u: (1 + 2 + 1) / 2 = 2.
  (wasmdown.test:is
   (equalp '(2)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i16x8.extract_lane_u 0
                 (i16x8.avgr_u (v128.const i32x4 0x00010001 0 0 0)
                               (v128.const i32x4 0x00020002 0 0 0)))))"
            "f"))))

(wasmdown.test:test simd/i-lane-type-mismatch
  ;; NEGATIVE: i32x4.add with a scalar operand is invalid.
  (wasmdown.test:is (%simd-invalid-p
                     "(module (func (export \"f\") (result v128)
                        (i32x4.add (v128.const i32x4 1 2 3 4)
                                   (i32.const 1))))")))

(wasmdown.test:test simd/i64x2-mul-bitmask
  (wasmdown.test:is
   (equalp '(3)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i64x2.bitmask (v128.const i64x2 -1 -1))))"
            "f")))
  (wasmdown.test:is
   (equalp '(1)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i64x2.bitmask (v128.const i64x2 -1 1))))"
            "f"))))

;;; ================================================================
;;; Float lane ops.

(wasmdown.test:test simd/f32x4-add-min-convert
  ;; add: 1.5 + 2.25 = 3.75 (0x40700000)
  (wasmdown.test:is
   (equalp '(#x40700000)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i32x4.extract_lane 0
                 (f32x4.add (v128.const f32x4 1.5 0 0 0)
                            (v128.const f32x4 2.25 0 0 0)))))"
            "f" )))
  ;; min propagates the smaller.
  (wasmdown.test:is
   (equalp '(#x3f800000)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i32x4.extract_lane 1
                 (f32x4.min (v128.const f32x4 9 1.0 9 9)
                            (v128.const f32x4 9 2.0 9 9)))))"
            "f"))))

(wasmdown.test:test simd/f32x4-convert-pair
  ;; convert_i32x4_s: 3 -> 3.0f (0x40400000).
  (wasmdown.test:is
   (equalp '(#x40400000)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i32x4.extract_lane 0
                 (f32x4.convert_i32x4_s (v128.const i32x4 3 0 0 0)))))"
            "f")))
  ;; trunc_sat_f32x4_s: 1.9 -> 1.
  (wasmdown.test:is
   (equalp '(1)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i32x4.extract_lane 0
                 (i32x4.trunc_sat_f32x4_s (v128.const f32x4 1.9 0 0 0)))))"
            "f"))))

(wasmdown.test:test simd/f-lane-type-mismatch
  ;; NEGATIVE: f32x4.add with an i32x4-typed... (types are by shape)
  ;; f32x4.abs of a scalar is invalid.
  (wasmdown.test:is (%simd-invalid-p
                     "(module (func (export \"f\") (result v128)
                        (f32x4.abs (f32.const 1.0))))")))

;;; ================================================================
;;; Loads / stores (incl. lane stores + OOB traps).

(wasmdown.test:test simd/load-store-roundtrip
  ;; store then load back a v128.
  (wasmdown.test:is
   (equalp (list (%v128 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))
           (%simd-run
            "(module (memory 1)
               (func (export \"f\") (result v128)
                 (v128.store (i32.const 0)
                             (v128.const i32x4 0x04030201 0x08070605
                                              0x0c0b0a09 0x100f0e0d))
                 (v128.load (i32.const 0))))"
            "f"))))

(wasmdown.test:test simd/load-extend-s
  ;; load8x8_s: byte 0xff at addr 0 sign-extends to lane0 = -1.
  (wasmdown.test:is
   (equalp '(4294967295)
           (%simd-run
            "(module (memory 1)
               (data (i32.const 0) \"\\ff\\00\\00\\00\\00\\00\\00\\00\")
               (func (export \"f\") (result i32)
                 (i16x8.extract_lane_s 0
                   (v128.load8x8_s (i32.const 0)))))"
            "f"))))

(wasmdown.test:test simd/load32-zero-upper
  (wasmdown.test:is
   (equalp (list (%v128 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
           (%simd-run
            "(module (memory 1)
               (data (i32.const 0) \"\\07\")
               (func (export \"f\") (result v128)
                 (v128.load32_zero (i32.const 0))))"
            "f"))))

(wasmdown.test:test simd/store8-lane
  ;; store8_lane stores ONE byte of the vector.
  (wasmdown.test:is
   (equalp '(#xab)
           (%simd-run
            "(module (memory 1)
               (func (export \"f\") (result i32)
                 (v128.store8_lane offset=0 3 (i32.const 0)
                                   (v128.const i32x4 0xab000000 0 0 0))
                 (i32.load8_u (i32.const 0))))"
            "f"))))

(wasmdown.test:test simd/load-oob-traps
  ;; NEGATIVE: a load past the memory end traps OOB.
  (wasmdown.test:is
   (%simd-trap-p
    "(module (memory 1)
       (func (export \"f\") (result v128)
         (v128.load (i32.const 65535))))"
    "f"))
  ;; NEGATIVE: a lane load OOB traps too.
  (wasmdown.test:is
   (%simd-trap-p
    "(module (memory 1)
       (func (export \"f\") (result v128)
         (v128.load8_lane offset=65536 0 (i32.const 65536)
                          (v128.const i32x4 0 0 0 0))))"
    "f")))

(wasmdown.test:test simd/store-align-too-large
  ;; NEGATIVE: alignment 32 on a v128.load (natural 16) is invalid.
  (wasmdown.test:is
   (%simd-invalid-p
    "(module (memory 1)
       (func (export \"f\") (result v128)
         (v128.load align=32 (i32.const 0))))")))

;;; ================================================================
;;; bitselect / any_true / bitmask / swizzle / shuffle.

(wasmdown.test:test simd/bitselect-any-true
  (wasmdown.test:is
   (equalp (list (%v128 #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55
                        #xaa #x55 #xaa #x55 #xaa #x55 #xaa #x55))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (v128.bitselect (v128.const i32x4 -1 -1 -1 -1)
                               (v128.const i32x4 0 0 0 0)
                               (v128.const i32x4 0x55aa55aa 0x55aa55aa
                                                0x55aa55aa 0x55aa55aa))))"
            "f")))
  (wasmdown.test:is (equalp '(0) (%simd-run
    "(module (func (export \"f\") (result i32)
       (v128.any_true (v128.const i32x4 0 0 0 0))))" "f")))
  (wasmdown.test:is (equalp '(1) (%simd-run
    "(module (func (export \"f\") (result i32)
       (v128.any_true (v128.const i32x4 0 0 0 1))))" "f"))))

(wasmdown.test:test simd/all-true-bitmask
  (wasmdown.test:is (equalp '(0) (%simd-run
    "(module (func (export \"f\") (result i32)
       (i32x4.all_true (v128.const i32x4 1 2 0 4))))" "f")))
  (wasmdown.test:is (equalp '(1) (%simd-run
    "(module (func (export \"f\") (result i32)
       (i32x4.all_true (v128.const i32x4 1 2 3 4))))" "f")))
  ;; bitmask: high bit per lane.
  (wasmdown.test:is (equalp '(5) (%simd-run
    "(module (func (export \"f\") (result i32)
       (i32x4.bitmask (v128.const i32x4 -1 0 -1 0))))" "f"))))

(wasmdown.test:test simd/swizzle-shuffle
  ;; swizzle: OOB indices give 0.
  (wasmdown.test:is
   (equalp (list (%v128 0 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (i8x16.swizzle (v128.const i32x4 1 2 3 4)
                              (v128.const i32x4 2 0 16 17))))"
            "f")))
  ;; shuffle: indices >= 16 select from the second operand.
  (wasmdown.test:is
   (equalp '(#x14131211)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i32x4.extract_lane 0
                 (i8x16.shuffle 16 17 18 19 0 0 0 0 0 0 0 0 0 0 0 0
                   (v128.const i32x4 1 2 3 4)
                   (v128.const i32x4 0x14131211 0 0 0)))))"
            "f"))))

(wasmdown.test:test simd/shuffle-lane-oob-malformed
  ;; NEGATIVE: shuffle index 32 (ImmLaneIdx32 max 31) is malformed.
  (wasmdown.test:is (%simd-malformed-p
    "(module (func (export \"f\") (result v128)
       (i8x16.shuffle 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32
         (v128.const i32x4 0 0 0 0)
         (v128.const i32x4 0 0 0 0))))")))

;;; ================================================================
;;; v128 bitwise + not.

(wasmdown.test:test simd/v128-bitwise
  (wasmdown.test:is
   (equalp (list (%v128 #xf0 #xf0 #xf0 #xf0 #xf0 #xf0 #xf0 #xf0
                        #xf0 #xf0 #xf0 #xf0 #xf0 #xf0 #xf0 #xf0))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (v128.and (v128.const i32x4 -1 -1 -1 -1)
                         (v128.const i32x4 0xf0f0f0f0 0xf0f0f0f0
                                          0xf0f0f0f0 0xf0f0f0f0))))"
            "f")))
  (wasmdown.test:is
   (equalp (list (%v128 #xff #xff #xff #xff #xff #xff #xff #xff
                        #xff #xff #xff #xff #xff #xff #xff #xff))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (v128.not (v128.const i32x4 0 0 0 0))))"
            "f"))))

;;; ================================================================
;;; Relaxed subset (canonical choices, documented alternatives).

(wasmdown.test:test simd/relaxed-swizzle-canonical
  ;; CANONICAL: OOB indices (16..255) give 0. The spec admits
  ;; a[s%16] for 16..127; this engine picks the 0 outcome.
  (wasmdown.test:is
   (equalp (list (%v128 7 7 7 7 0 7 7 7 0 7 7 7 0 7 7 7))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (i8x16.relaxed_swizzle (v128.const i32x4 7 3 5 9)
                                      (v128.const i32x4 0 1 20 99))))"
            "f"))))

(wasmdown.test:test simd/relaxed-madd-two-rounding
  ;; CANONICAL: plain two-rounding a*b+c (the spec also admits a
  ;; fused single-rounding FMA on hosts that support it).
  (wasmdown.test:is
   (equalp '(#x40400000)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i32x4.extract_lane 0
                 (f32x4.relaxed_madd (v128.const f32x4 1.0 0 0 0)
                                     (v128.const f32x4 2.0 0 0 0)
                                     (v128.const f32x4 1.0 0 0 0)))))"
            "f"))))

(wasmdown.test:test simd/relaxed-q15mulr-saturating
  ;; CANONICAL: the saturating path (i16x8.q15mulr_sat_s); the spec
  ;; also admits wrapping the INT16_MIN*INT16_MIN overflow lane.
  ;; 0x4000*0x4000 + 0x4000 = 0x10000000; >> 15 = 0x2000.
  (wasmdown.test:is
   (equalp '(#x2000)
           (%simd-run
            "(module (func (export \"f\") (result i32)
               (i16x8.extract_lane_u 0
                 (i16x8.relaxed_q15mulr_s
                   (v128.const i32x4 0x00004000 0 0 0)
                   (v128.const i32x4 0x00004000 0 0 0)))))"
            "f"))))

(wasmdown.test:test simd/relaxed-laneselect-bitselect
  ;; CANONICAL: bitselect semantics on every mask (the spec admits
  ;; returning the mask-favored operand on mixed masks).
  (wasmdown.test:is
   (equalp (list (%v128 #xff #xff #xff #xff #xff #xff #xff #xff
                        #xff #xff #xff #xff #xff #xff #xff #xff))
           (%simd-run
            "(module (func (export \"f\") (result v128)
               (i8x16.relaxed_laneselect
                 (v128.const i32x4 -1 -1 -1 -1)
                 (v128.const i32x4 0 0 0 0)
                 (v128.const i32x4 -1 -1 -1 -1))))"
            "f"))))

;;; ================================================================
;;; Binary round-trip over the family (encoder mirror).

(wasmdown.test:test simd/binary-roundtrip-family
  ;; A module using one op per major family encodes/decodes stable.
  (let* ((m (%simd-mod
             "(module (memory 1)
                (func (export \"a\") (result v128) (v128.const i32x4 1 2 3 4))
                (func (export \"b\") (param v128) (result v128)
                  (i32x4.add (local.get 0) (local.get 0)))
                (func (export \"c\") (result v128)
                  (f32x4.sqrt (v128.const f32x4 4 4 4 4)))
                (func (export \"d\") (result i32)
                  (v128.any_true (v128.const i32x4 0 0 0 1)))
                (func (export \"e\") (result v128)
                  (i8x16.relaxed_swizzle (v128.const i32x4 1 2 3 4)
                                         (v128.const i32x4 0 1 2 3))))"))
         (b1 (wasmdown.binary:encode-module m))
         (d (wasmdown.binary:decode-module b1))
         (b2 (wasmdown.binary:encode-module d)))
    (wasmdown.test:is (equalp b1 b2))))

(wasmdown.test:test simd/relaxed-sub-encoding
  ;; The relaxed sub-opcodes (>= 0x100) encode as MULTI-BYTE u32 LEB
  ;; and round-trip: i8x16.relaxed_swizzle = 0x100.
  (let* ((m (%simd-mod
             "(module (func (export \"f\") (result v128)
                (i8x16.relaxed_swizzle (v128.const i32x4 1 2 3 4)
                                       (v128.const i32x4 0 1 2 3))))"))
         (b1 (wasmdown.binary:encode-module m))
         (d (wasmdown.binary:decode-module b1)))
    (wasmdown.test:is (plusp (length b1)))
    ;; the decoded module re-executes the same op
    (let ((inst (wasmdown:instantiate d)))
      (wasmdown.test:is
       ;; byte semantics: s = [0,0,0,0,1,0,0,0,2,0,0,0,3,0,0,0],
       ;; a = [1,0,0,0,2,0,0,0,3,0,0,0,4,0,0,0]:
       ;; r[0]=a[0]=1, r[4]=a[1]=0, r[8]=a[2]=0, r[12]=a[3]=0.
       (equalp (list (%v128 1 1 1 1 0 1 1 1 0 1 1 1 0 1 1 1))
               (wasmdown:invoke inst "f"))))))

;;; ================================================================
;;; obsolete-keywords ::10 — the vendored SIMD row now passes at
;;; DECODE (regression pin for the classifier release).

(wasmdown.test:test simd/obsolete-keywords-shape-honest
  ;; The shape f32x4.convert_s/i32x4 is NOT an opcode: text decode
  ;; rejects it with 'bad or missing immediates' — the honest
  ;; assert_malformed outcome, not an unsupported classification.
  (wasmdown.test:is
   (%simd-malformed-p
    "(module (func (drop (f32x4.convert_s/i32x4 (v128.const i64x2 0 0)))))")))

;;; ================================================================
;;; Regressions found by the upstream-testsuite lane (docs/design/PLAN.md §12.7).

(wasmdown.test:test simd/float-lane-comparisons-yield-masks
  ;; f32x4.eq used to run its compare through the FLOAT map: the
  ;; all-ones lane mask was fed back into f32-bits, so the very first
  ;; assertion trapped. A comparison's result lanes are masks.
  (let ((r (%simd-run
            "(module (func (export \"f\") (result v128)
               (f32x4.eq (v128.const f32x4 1 2 3 4)
                         (v128.const f32x4 1 0 3 9))))"
            "f")))
    ;; the invoke path returns a RESULT LIST of one v128
    (wasmdown.test:is
     (equalp (list (%v128 #xff #xff #xff #xff
                          #x00 #x00 #x00 #x00
                          #xff #xff #xff #xff
                          #x00 #x00 #x00 #x00))
             r)))
  ;; f64x2.ne: unequal -> all ones, equal -> zero
  (let ((r (%simd-run
            "(module (func (export \"f\") (result v128)
               (f64x2.ne (v128.const f64x2 1 2)
                         (v128.const f64x2 2 2))))"
            "f")))
    (wasmdown.test:is
     (equalp (list (%v128 #xff #xff #xff #xff #xff #xff #xff #xff
                          0 0 0 0 0 0 0 0))
             r))))

(wasmdown.test:test simd/load-extend-lane-counts
  ;; v128.load16x4_s / load32x2_s load EIGHT source bytes into 4 / 2
  ;; result lanes; writing 8 lanes overran the 16-byte value.
  (let ((r (%simd-run
            "(module (memory 1)
               (func (export \"f\") (result v128)
                 (i32.store (i32.const 0) (i32.const 0x01020304))
                 (i32.store (i32.const 4) (i32.const 0x05060708))
                 (v128.load16x4_s (i32.const 0))))"
            "f")))
    ;; memory bytes: 04 03 02 01 08 07 06 05 (i32.store is
    ;; little-endian); the 4 i16 lanes 0x0304 0x0102 0x0708 0x0506
    ;; widen to i32 lanes
    (wasmdown.test:is
     (equalp (list (%v128 #x04 #x03 0 0  #x02 #x01 0 0
                          #x08 #x07 0 0  #x06 #x05 0 0))
             r)))
  (let ((r (%simd-run
            "(module (memory 1)
               (func (export \"f\") (result v128)
                 (i32.store (i32.const 0) (i32.const 0x01020304))
                 (i32.store (i32.const 4) (i32.const 0x05060708))
                 (v128.load32x2_s (i32.const 0))))"
            "f")))
    (wasmdown.test:is
     (equalp (list (%v128 #x04 #x03 #x02 #x01 0 0 0 0
                          #x08 #x07 #x06 #x05 0 0 0 0))
             r))))

(wasmdown.test:test simd/load-store-oob-is-a-trap
  ;; The SIMD load/store family must trap :out-of-bounds-memory like the
  ;; scalar path — a raw array-index error would be a host defect, and
  ;; an in-bounds-but-exactly-fit access must NOT trap.
  (let* ((m (%simd-mod "(module (memory 1)
                          (func (export \"full\") (result v128)
                            (v128.load (i32.const 65520)))
                          (func (export \"over\") (result v128)
                            (v128.load (i32.const 65521)))
                          (func (export \"extend\") (result v128)
                            (v128.load16x4_s (i32.const 65529)))
                          (func (export \"store\") (result i32)
                            (v128.store (i32.const 65521)
                                        (v128.const i32x4 1 2 3 4))
                            (i32.const 0)))"))
         (inst (wasmdown:instantiate (wasmdown:validate-module m))))
    ;; 65520 + 16 == 65536: exactly the page end, in bounds
    (wasmdown.test:is (equalp (list (%v128 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
                              (wasmdown:invoke inst "full")))
    (dolist (name '("over" "extend" "store"))
      (wasmdown.test:is
       (eq :out-of-bounds-memory
           (wasmdown:wasm-trap-kind
            (wasmdown.test:assert-trap :out-of-bounds-memory
              (wasmdown:invoke inst name))))))))
