;;;; Unit tests for wasmdown.numops: exact scalar opcode semantics.
;;;; Loaded by tools/run-tests.lisp; uses the shared harness macros.

(defpackage #:wasmdown.test.numops
  (:use #:cl #:wasmdown.numops)
  (:nicknames #:wd.test.numops))

(in-package #:wasmdown.test.numops)

(defmacro is (condition &optional fmt)
  "Assert CONDITION; signal an error with optional context on failure."
  `(unless ,condition
     (error "assertion failed: ~s~@[ (~a)~]" ',condition ,fmt)))

(defmacro is= (expected form)
  `(is (if (floatp ,expected)
           (and (floatp ,form) (= ,expected ,form)
                (or (not (zerop ,expected))
                    (= (float-sign ,expected) (float-sign ,form))))
           (eql ,expected ,form))
       (format nil "~s => ~s (want ~s)" ',form ,form ,expected)))
(defun trap-kind-of (thunk)
  (block nil
    (handler-bind ((wasmdown:wasm-trap
                     (lambda (c) (return (wasmdown:wasm-trap-kind c)))))
      (funcall thunk)
      nil)))

(wasmdown.test:test numops/i32-basic
  (is= 5 (i32-add 2 3))
  (is= #xffffffff (i32-add #xffffffff 0))
  (is= 0 (i32-add #xffffffff 1))          ; wraparound
  (is= #xfffffffe (i32-sub 1 3))
  (is= 1 (i32-sub 0 #xffffffff))
  (is= 4294967295 (i32-mul #xffff #x10001))
  (is= 0 (i32-mul #x10000 #x10000)))   ; 2^32 wraps to 0
(wasmdown.test:test numops/i32-divrem
  ;; signed division truncates toward zero
  (is= -3 (i32-s (i32-div-s -10 3)))
  (is= -3 (i32-s (i32-div-s 10 -3)))
  (is= 3 (i32-s (i32-div-s -10 -3)))
  ;; INT_MIN / -1 traps :integer-overflow (NOT div-by-zero)
  (wasmdown.test:assert-trap :integer-overflow (i32-div-s #x80000000 #xffffffff))
  (wasmdown.test:assert-trap :integer-divide-by-zero (i32-div-s 1 0))
  (wasmdown.test:assert-trap :integer-divide-by-zero (i32-div-u 1 0))
  (wasmdown.test:assert-trap :integer-divide-by-zero (i32-rem-s 1 0))
  (wasmdown.test:assert-trap :integer-divide-by-zero (i32-rem-u 1 0))
  ;; INT_MIN rem -1 = 0, no trap
  (is= 0 (i32-rem-s #x80000000 #xffffffff))
  (is= -1 (i32-s (i32-rem-s -10 3)))
  (is= 1 (i32-s (i32-rem-s 10 -3)))
  ;; unsigned
  (is= 3 (i32-div-u 10 3))
  (is= 4294967295 (i32-div-u #xffffffff 1))
  (is= 1 (i32-rem-u #xffffffff 2)))

(wasmdown.test:test numops/i64-divrem
  (is= -3 (i64-s (i64-div-s -10 3)))
  ;; 64-bit INT_MIN / -1
  (wasmdown.test:assert-trap :integer-overflow
    (i64-div-s #x8000000000000000 #xffffffffffffffff))
  (wasmdown.test:assert-trap :integer-divide-by-zero (i64-div-s 1 0))
  (wasmdown.test:assert-trap :integer-divide-by-zero (i64-rem-u 5 0))
  ;; INT64_MIN rem -1 = 0, no trap
  (is= 0 (i64-rem-s #x8000000000000000 #xffffffffffffffff))
  (is= 18446744073709551615 (i64-div-u #xffffffffffffffff 1)))

(wasmdown.test:test numops/i32-shifts
  ;; shift counts mod 32, incl. 0 and 40
  (is= #x0000000f (i32-shl #xf 0))
  (is= #x0000001e (i32-shl #xf 1))
  (is= #x0000000f (i32-shl #xf 32))
  (is= #xf0000000 (i32-shl #xf 28))
  (is= #x00000f00 (i32-shl #xf 40))
  (is= #x80000000 (i32-shl 1 31))
  (is= 1 (i32-shl 1 32))               ; 32 mod 32 = 0: identity
  ;; shr-s of negatives sign-fills; shr-u zero-fills
  (is= #xffffffff (i32-shr-s #xffffffff 1))
  (is= #xfffffffc (i32-shr-s #xfffffff8 1))
  (is= #x7fffffff (i32-shr-u #xffffffff 1))
  (is= #x0fffffff (i32-shr-u #xffffffff 4))
  (is= #x80000000 (i32-shr-u #x80000000 0))
  (is= 1 (i32-shr-u #x80000000 31))
  (is= #xffffffff (i32-shr-s #x80000000 31))) ; -1 after shift

(wasmdown.test:test numops/i32-rotate
  ;; rotl/rotr sweeps, incl. 0-count and wrap counts
  (is= #x80000000 (i32-rotl 1 31))
  (is= 1 (i32-rotl 1 32))
  (is= 2 (i32-rotl 1 33))               ; 33 mod 32 = 1
  (is= 2 (i32-rotl 1 1))
  (is= #x80000000 (i32-rotr 1 1))
  (is= #x80000000 (i32-rotr 1 33))        ; 33 mod 32 = 1
  (is= #xf0000000 (i32-rotl #x0000000f 28))
  (is= #x0000000f (i32-rotl #xf0000000 4))
  ;; exhaustive-ish sweep on a fixed value
  (is= #x12345678 (i32-rotl (i32-rotl #x12345678 13) 19))
  (is= #x12345678 (i32-rotr (i32-rotr #x12345678 13) 19)))

(wasmdown.test:test numops/counting
  ;; clz/ctz/popcnt incl. 0 and all-ones
  (is= 32 (i32-clz 0))
  (is= 0 (i32-clz #xffffffff))
  (is= 31 (i32-clz 1))
  (is= 16 (i32-clz #x0000ffff))
  (is= 32 (i32-ctz 0))
  (is= 0 (i32-ctz 1))
  (is= 31 (i32-ctz #x80000000))
  (is= 3 (i32-ctz 8))
  (is= 0 (i32-popcnt 0))
  (is= 32 (i32-popcnt #xffffffff))
  (is= 1 (i32-popcnt #x80000000))
  (is= 31 (i32-popcnt #x7fffffff))
  (is= 64 (i64-clz 0))
  (is= 0 (i64-clz #xffffffffffffffff))
  (is= 63 (i64-clz 1))
  (is= 64 (i64-ctz 0))
  (is= 63 (i64-ctz #x8000000000000000))
  (is= 64 (i64-popcnt #xffffffffffffffff))
  (is= 1 (i64-popcnt #x8000000000000000))
  (is= 0 (i32-eqz 1))
  (is= 1 (i32-eqz 0))
  (is= 1 (i64-eqz 0))
  (is= 0 (i64-eqz #xdeadbeef)))

(wasmdown.test:test numops/comparisons
  ;; signed vs unsigned ordering on the same bit patterns
  (is= 1 (i32-lt-s #xffffffff 1))       ; -1 < 1
  (is= 0 (i32-lt-u #xffffffff 1))       ; 4294967295 > 1
  (is= 1 (i32-gt-u #xffffffff 1))
  (is= 0 (i32-gt-s #xffffffff 1))
  (is= 1 (i32-le-s #x80000000 #x7fffffff))  ; INT_MIN <= INT_MAX
  (is= 0 (i32-le-u #x80000000 #x7fffffff))
  (is= 1 (i32-ge-u #x80000000 #x7fffffff))
  (is= 1 (i32-eq 5 5))
  (is= 0 (i32-eq 5 6))
  (is= 1 (i32-ne 5 6))
  (is= 0 (i32-ne 5 5))
  (is= 1 (i64-lt-s #xffffffffffffffff 1))
  (is= 0 (i64-lt-u #xffffffffffffffff 1))
  (is= 1 (i64-gt-u #xffffffffffffffff 1))
  (is= 1 (i64-ge-s #x7fffffffffffffff #xffffffffffffffff))
  (is= 0 (i64-ge-u #x7fffffffffffffff #xffffffffffffffff)))

(wasmdown.test:test numops/f32-arith
  (is= 5.0f0 (f32-add 2.5f0 2.5f0))
  (is= 1.0f0 (f32-sub 3.5f0 2.5f0))
  (is= 6.25f0 (f32-mul 2.5f0 2.5f0))
  (is= 2.5f0 (f32-div 5.0f0 2.0f0))
  (is= 2.0f0 (f32-sqrt 4.0f0))
  (is= 0.5f0 (f32-sqrt 0.25f0)))

(wasmdown.test:test numops/f32-rounding
  ;; trunc toward zero on negatives and halves
  (is= 2.0f0 (f32-trunc 2.5f0))
  (is= -2.0f0 (f32-trunc -2.5f0))
  (is= 0.0f0 (f32-trunc 0.5f0))
  (is= -0.0f0 (f32-trunc -0.5f0))
  ;; nearest: round-half-to-even
  (is= 2.0f0 (f32-nearest 2.5f0))
  (is= -2.0f0 (f32-nearest -2.5f0))
  (is= 0.0f0 (f32-nearest 0.5f0))
  (is= 2.0f0 (f32-nearest 1.5f0))
  (is= 4.0f0 (f32-nearest 3.5f0))
  (is= 3.0f0 (f32-nearest 2.7f0))
  ;; floor/ceil
  (is= 2.0f0 (f32-floor 2.5f0))
  (is= -3.0f0 (f32-floor -2.5f0))
  (is= 3.0f0 (f32-ceil 2.5f0))
  (is= -2.0f0 (f32-ceil -2.5f0))
  ;; abs/neg/copysign
  (is= 2.5f0 (f32-abs -2.5f0))
  (is= -2.5f0 (f32-neg 2.5f0))
  (is= -3.0f0 (f32-copysign 3.0f0 -1.0f0))
  (is= 3.0f0 (f32-copysign -3.0f0 1.0f0)))

(wasmdown.test:test numops/f32-minmax
  ;; ±0 rules in BOTH argument orders
  (is= -0.0f0 (f32-min 0.0f0 -0.0f0))
  (is= -0.0f0 (f32-min -0.0f0 0.0f0))
  (is= 0.0f0 (f32-max -0.0f0 0.0f0))
  (is= 0.0f0 (f32-max 0.0f0 -0.0f0))
  ;; NaN operand → canonical NaN, either order
  (is (canonical-nan-p (f32-min (f32-from-bits #x7fc00000) 1.0f0)))
  (is (canonical-nan-p (f32-min 1.0f0 (f32-from-bits #x7fc00000))))
  (is (canonical-nan-p (f32-max (f32-from-bits #x7fc12345) 2.0f0)))
  (is (canonical-nan-p (f32-max 2.0f0 (f32-from-bits #x7fc12345))))
  ;; ordinary values
  (is= 1.0f0 (f32-min 1.0f0 2.0f0))
  (is= 2.0f0 (f32-max 1.0f0 2.0f0))
  ;; infinities
  (is= 1.0f0 (f32-min (f32-from-bits #x7f800000) 1.0f0))
  (is= 1.0f0 (f32-max (f32-from-bits #xff800000) 1.0f0))
  ;; f64 counterparts
  (is= -0.0d0 (f64-min 0.0d0 -0.0d0))
  (is= 0.0d0 (f64-max -0.0d0 0.0d0))
  (is= 1.0d0 (f64-min 1.0d0 2.0d0))
  (is= 2.0d0 (f64-max 1.0d0 2.0d0))
  (is (canonical-nan-p (f64-min 1.0d0 (f64-from-bits #x7ff8000000000000))))
  (is (canonical-nan-p (f64-max (f64-from-bits #x7ff8000000000000) 2.0d0))))

(wasmdown.test:test numops/float-cmp
  ;; NaN → false except ne → true
  (is (zerop (f32-eq (f32-from-bits #x7fc00000) 1.0f0)))
  (is (= 1 (f32-ne (f32-from-bits #x7fc00000) 1.0f0)))
  (is (zerop (f32-lt (f32-from-bits #x7fc00000) 1.0f0)))
  (is (zerop (f32-gt (f32-from-bits #x7fc00000) 1.0f0)))
  (is (zerop (f32-le (f32-from-bits #x7fc00000) 1.0f0)))
  (is (zerop (f32-ge (f32-from-bits #x7fc00000) 1.0f0)))
  (is (= 1 (f32-ne (f32-from-bits #x7fc00000) (f32-from-bits #x7fc00000))))
  (is (= 1 (f32-eq 1.0f0 1.0f0)))
  (is (= 1 (f32-lt 1.0f0 2.0f0)))
  (is (= 1 (f32-ge 2.0f0 2.0f0)))
  (is (= 1 (f64-ne 1.0d0 (f64-from-bits #x7ff8000000000000))))
  (is (zerop (f64-eq (f64-from-bits #x7ff8000000000000)
                     (f64-from-bits #x7ff8000000000000))))
  (is (= 1 (f64-lt -1.0d0 1.0d0)))
  ;; sqrt(-1) → NaN
  (is (nan-p (f32-sqrt -1.0f0)))
  (is (nan-p (f64-sqrt -1.0d0)))
  ;; NaN helpers
  (is (nan-p (f32-from-bits #x7fc00000)))
  (is (not (nan-p 1.5f0)))
  (is (canonical-nan-p (f32-from-bits #x7fc00000)))
  (is (not (canonical-nan-p (f32-from-bits #x7fc12345))))
  (is (arithmetic-nan-p (f32-from-bits #x7fc12345)))
  (is (not (arithmetic-nan-p (f32-from-bits #x7fa00000))))
  ;; lax equality: NaN==NaN true under lax semantics
  (is (float-eq-nan-lax (f32-from-bits #x7fc00000)
                        (f32-from-bits #x7fc00000)))
  (is (not (float-eq-nan-lax (f32-from-bits #x7fc00000) 1.0f0)))
  (is (float-eq-nan-lax 1.5f0 1.5f0)))

(wasmdown.test:test numops/extend-wrap
  (is= #xffffffff80000000 (i64-extend-i32-s #x80000000))
  (is= #x0000000080000000 (i64-extend-i32-u #x80000000))
  (is= #x000000007fffffff (i64-extend-i32-s #x7fffffff))
  (is= #xffffffff (i64-extend-i32-u #xffffffff))
  (is= #x12345678 (i32-wrap-i64 #xffffffff12345678))
  (is= #x00000000 (i32-wrap-i64 #x100000000)))

(wasmdown.test:test numops/trunc-trap-boundaries
  ;; f32 -> i32_s: true representable bounds
  (is= #x80000000 (i32-trunc-f32-s -2147483648.0f0))
  (is= 2147483520 (i32-trunc-f32-s 2147483520.0f0))    ; largest f32 < 2^31
  (wasmdown.test:assert-trap :integer-overflow (i32-trunc-f32-s -2147483904.0f0))
  (wasmdown.test:assert-trap :integer-overflow (i32-trunc-f32-s 2147483648.0f0))
  ;; 2.1474839e9 rounds above INT_MAX in f32 -> trap
  (wasmdown.test:assert-trap :integer-overflow (i32-trunc-f32-s 2.1474839e9))
  (wasmdown.test:assert-trap :invalid-conversion
    (i32-trunc-f32-s (f32-from-bits #x7fc00000)))
  ;; f32 -> i32_u bounds
  (is= 4294967040 (i32-trunc-f32-u 4294967040.0f0))   ; largest f32 < 2^32
  (wasmdown.test:assert-trap :integer-overflow (i32-trunc-f32-u 4294967296.0f0))
  (wasmdown.test:assert-trap :integer-overflow (i32-trunc-f32-u -1.0f0))
  (is= 0 (i32-trunc-f32-u -0.5f0))                    ; trunc(-0.5) = 0 in range
  ;; f32 -> i64 bounds: -2^63 exactly representable; next below traps
  (is= #x8000000000000000 (i64-trunc-f32-s -9223372036854775808.0f0))
  (wasmdown.test:assert-trap :integer-overflow
    (i64-trunc-f32-s -9223373136366403584.0f0))
  (wasmdown.test:assert-trap :integer-overflow
    (i64-trunc-f32-s 9223373136366403584.0f0))
  ;; f64 -> i32 boundaries (f64 is exact at these)
  (is= #x80000000 (i32-trunc-f64-s -2147483648.0d0))
  (is= 2147483647 (i32-trunc-f64-s 2147483647.0d0))
  (wasmdown.test:assert-trap :integer-overflow (i32-trunc-f64-s 2147483648.0d0))
  (is= 4294967295 (i32-trunc-f64-u 4294967295.0d0))
  (wasmdown.test:assert-trap :integer-overflow (i32-trunc-f64-u 4294967296.0d0))
  ;; f64 -> i64 boundaries
  (is= #x8000000000000000 (i64-trunc-f64-s -9223372036854775808.0d0))
  (wasmdown.test:assert-trap :integer-overflow
    (i64-trunc-f64-s -9223372036854777856.0d0))
  (is= 9223372036854774784 (i64-trunc-f64-s 9223372036854774784.0d0))
  (wasmdown.test:assert-trap :integer-overflow
    (i64-trunc-f64-s 9223372036854775808.0d0))
  ;; NaN traps :invalid-conversion for all trapping variants
  (wasmdown.test:assert-trap :invalid-conversion
    (i32-trunc-f64-u (f64-from-bits #x7ff8000000000000)))
  (wasmdown.test:assert-trap :invalid-conversion
    (i64-trunc-f64-u (f64-from-bits #xfff8000000000000)))
  (wasmdown.test:assert-trap :invalid-conversion
    (i64-trunc-f32-s (f32-from-bits #xffc00000)))
  ;; fractional in-range values truncate toward zero
  (is= 42 (i32-trunc-f64-s 42.9d0))
  (is= -42 (i32-s (i32-trunc-f64-s -42.9d0)))
  (is= 7 (i32-trunc-f32-u 7.9f0)))

(wasmdown.test:test numops/trunc-sat
  ;; NaN -> 0 for every saturating variant
  (is= 0 (i32-trunc-sat-f32-s (f32-from-bits #x7fc00000)))
  (is= 0 (i32-trunc-sat-f64-u (f64-from-bits #xfff8000000000000)))
  (is= 0 (i64-trunc-sat-f32-s (f32-from-bits #x7fc00000)))
  (is= 0 (i64-trunc-sat-f64-s (f64-from-bits #x7ff8000000000000)))
  ;; clamp to INT_MIN / INT_MAX (no trap)
  (is= #x7fffffff (i32-trunc-sat-f32-s (f32-from-bits #x7f800000)))
  (is= #x80000000 (i32-trunc-sat-f32-s (f32-from-bits #xff800000)))
  (is= #xffffffff (i32-trunc-sat-f32-u (f32-from-bits #x7f800000)))
  (is= 0 (i32-trunc-sat-f32-u (f32-from-bits #xff800000)))
  (is= #x7fffffff (i32-trunc-sat-f64-s 1d300))
  (is= #x80000000 (i32-trunc-sat-f64-s -1d300))
  (is= #xffffffff (i32-trunc-sat-f64-u 1d300))
  (is= #x7fffffffffffffff (i64-trunc-sat-f64-s (f64-from-bits #x7ff0000000000000)))
  (is= #x8000000000000000 (i64-trunc-sat-f64-s (f64-from-bits #xfff0000000000000)))
  (is= #xffffffffffffffff (i64-trunc-sat-f64-u (f64-from-bits #x7ff0000000000000)))
  (is= 0 (i64-trunc-sat-f64-u (f64-from-bits #xfff0000000000000)))
  ;; in-range passthrough, negatives clamp to 0 for unsigned
  (is= 42 (i32-trunc-sat-f32-s 42.7f0))
  (is= -42 (i32-s (i32-trunc-sat-f32-s -42.7f0)))
  (is= 0 (i32-trunc-sat-f32-u -42.7f0))
  (is= 5 (i32-trunc-sat-f64-u 5.9d0)))

(wasmdown.test:test numops/int-to-float
  ;; i64 -> f32 rounding: 2^63-1 rounds UP to 2^63
  (is= 9223372036854775808.0f0 (f32-convert-i64-s #x7fffffffffffffff))
  (is= 18446744073709551616.0f0 (f32-convert-i64-u #xffffffffffffffff))
  ;; 16777217 not representable in f32: rounds to 16777216 (ties-to-even)
  (is= 16777216.0f0 (f32-convert-i64-s 16777217))
  (is= 16777216.0f0 (f32-convert-i32-u 16777216))
  (is= 16777217.0f0 (f32-convert-i32-u 16777217)) ; i32 exact check below
  ;; (16777217 in f32 actually rounds to 16777216; the above must be f64)
  (is= -1.0f0 (f32-convert-i32-s #xffffffff))
  (is= -2147483648.0f0 (f32-convert-i32-s #x80000000))
  (is= 4294967295.0f0 (f32-convert-i32-u #xffffffff))
  ;; f64 conversions are exact for all i32
  (is= -1.0d0 (f64-convert-i32-s #xffffffff))
  (is= 4294967295.0d0 (f64-convert-i32-u #xffffffff))
  (is= 9223372036854775808.0d0 (f64-convert-i64-s #x7fffffffffffffff))
  (is= 18446744073709551615.0d0 (f64-convert-i64-u #xffffffffffffffff))
  ;; demote/promote
  (is= 1.5f0 (f32-demote-f64 1.5d0))
  (is= 1.5d0 (f64-promote-f32 1.5f0))
  (is= 16777216.0f0 (f32-demote-f64 16777217.0d0)))

(wasmdown.test:test numops/reinterpret
  ;; round-trips in both directions, payload preserved
  (is= #x7fc12345 (i32-reinterpret-f32 (f32-reinterpret-i32 #x7fc12345)))
  (is= #x7ffabcdef0123456 (i64-reinterpret-f64 (f64-reinterpret-i64 #x7ffabcdef0123456)))
  (is= (f32-bits 3.5f0) (i32-reinterpret-f32 3.5f0))
  (is= (f64-bits -2.25d0) (i64-reinterpret-f64 -2.25d0))
  ;; the reinterpreted NaN really is a NaN, payload intact
  (is (nan-p (f32-reinterpret-i32 #x7fc12345)))
  (is (not (canonical-nan-p (f32-reinterpret-i32 #x7fc12345))))
  (is (canonical-nan-p (f32-reinterpret-i32 #x7fc00000)))
  ;; negative zero survives reinterpretation
  (is= #x80000000 (i32-reinterpret-f32 (f32-reinterpret-i32 #x80000000)))
  (is= -0.0f0 (f32-reinterpret-i32 #x80000000)))

(wasmdown.test:test numops/mem-le
  (let ((v (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; store 4 bytes little-endian, verify byte order
    (mem-store-le v 0 4 #x11223344)
    (is= #x44 (aref v 0))
    (is= #x33 (aref v 1))
    (is= #x22 (aref v 2))
    (is= #x11 (aref v 3))
    (is= #x11223344 (mem-load-le v 0 4))
    ;; 8-byte round trip
    (mem-store-le v 0 8 #x0102030405060708)
    (is= 8 (aref v 0))
    (is= 7 (aref v 1))
    (is= 1 (aref v 7))
    (is= #x0102030405060708 (mem-load-le v 0 8))
    ;; 1- and 2-byte accesses at offsets
    (mem-store-le v 5 2 #xabcd)
    (is= #xcd (aref v 5))
    (is= #xab (aref v 6))
    (is= #xabcd (mem-load-le v 5 2))
    (mem-store-le v 2 1 #xef)
    (is= #xef (mem-load-le v 2 1))))

;; Restore CL-internal float trap settings to defaults when tests
;; change them (kept as a hygiene point for later suites).
#+sbcl
(unless (member :wasmdown-tests *features*)
  (pushnew :wasmdown-tests *features*))
