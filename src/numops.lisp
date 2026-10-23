;;;; Exact numeric primitive semantics for every scalar Wasm opcode
;;;; (PLAN.md §4 numops). i32/i64 are unsigned Lisp integers, f32/f64 are
;;;; single/double floats per docs/CONTRACTS.md. All traps use
;;;; wasmdown:wasm-trap with the kinds fixed there.

(in-package #:wasmdown.numops)


;;; ---------------------------------------------------------------
;;; Normalizers and signed/unsigned views.

(defmacro i32-> (x)
  "Normalize an arbitrary integer into the unsigned 32-bit domain."
  `(the (unsigned-byte 32) (logand ,x #xffffffff)))

(defmacro i32-wrap (x)
  "Alias of I32->: wrap/truncate into 32 unsigned bits."
  `(i32-> ,x))

(defmacro i64-> (x)
  "Normalize an arbitrary integer into the unsigned 64-bit domain."
  `(the (unsigned-byte 64) (logand ,x #xffffffffffffffff)))

(defmacro i64-wrap (x)
  "Alias of I64->: wrap/truncate into 64 unsigned bits."
  `(i64-> ,x))
;; identity helpers (unsigned domain is the stored representation).
(declaim (inline i32-u) (inline i64-u))
(defun i32-u (x) (i32-> x))
(defun i64-u (x) (i64-> x))
(defun i32-s (x)                    ; unsigned 32-bit -> signed view
  (if (>= x #x80000000) (- x #x100000000) x))
(defun i64-s (x)                    ; unsigned 64-bit -> signed view
  (if (>= x #x8000000000000000) (- x #x10000000000000000) x))


;;; ---------------------------------------------------------------
;;; i32 arithmetic (results wrap into the unsigned 32-bit domain).

(macrolet
    ((defwrap (name lambda-list &body body)
       `(defun ,name ,lambda-list (i32-> (progn ,@body)))))
  (defwrap i32-add (a b) (+ a b))
  (defwrap i32-sub (a b) (- a b))
  (defwrap i32-mul (a b) (* a b)))

(defun i32-div-s (a b)
  (when (zerop b) (error 'wasmdown:wasm-trap :kind :integer-divide-by-zero
                         :message "i32.div_s divide by zero"))
  (let ((sa (i32-s a)) (sb (i32-s b)))
    (when (and (= sa (- (expt 2 31))) (= sb -1))
      (error 'wasmdown:wasm-trap :kind :integer-overflow
             :message "i32.div_s overflow"))
    (i32-> (truncate sa sb))))

(defun i32-div-u (a b)
  (when (zerop b) (error 'wasmdown:wasm-trap :kind :integer-divide-by-zero
                         :message "i32.div_u divide by zero"))
  (i32-> (truncate a b)))

(defun i32-rem-s (a b)
  (when (zerop b) (error 'wasmdown:wasm-trap :kind :integer-divide-by-zero
                         :message "i32.rem_s divide by zero"))
  (let ((sa (i32-s a)) (sb (i32-s b)))
    (if (and (= sa (- (expt 2 31))) (= sb -1))
        0
        (i32-> (rem sa sb)))))

(defun i32-rem-u (a b)
  (when (zerop b) (error 'wasmdown:wasm-trap :kind :integer-divide-by-zero
                         :message "i32.rem_u divide by zero"))
  (i32-> (rem a b)))

;;; ---------------------------------------------------------------
;;; i32 bitwise and shifts (counts taken mod 32).

(defun i32-and (a b) (logand a b))
(defun i32-or (a b) (logior a b))
(defun i32-xor (a b) (logxor a b))
(defun i32-shl (a b) (i32-> (ash a (mod b 32))))
(defun i32-shr-s (a b)
  (i32-> (ash (i32-s a) (- (mod b 32)))))
(defun i32-shr-u (a b) (i32-> (ash a (- (mod b 32)))))

;;; Rotate: build the rotation with two shifts inside the widened domain.
(defun i32-rotl (a b)
  (let ((k (mod b 32)))
    (i32-> (logior (ash (logand a #xffffffff) k)
                   (ash (logand a #xffffffff) (- k 32))))))
(defun i32-rotr (a b)
  (let ((k (mod b 32)))
    (i32-> (logior (ash (logand a #xffffffff) (- k))
                   (ash (logand a #xffffffff) (- 32 k))))))

;;; Counting ops.  SBCL LOGCOUNT counts set bits of nonnegative ints.
(defun i32-clz (a)
  (if (zerop a) 32 (- 32 (integer-length (logand a #xffffffff)))))
(defun i32-ctz (a)
  (if (zerop a) 32 (1- (integer-length (logand a (- a))))))
(defun i32-popcnt (a) (logcount (logand a #xffffffff)))

(defun i32-eqz (a) (if (zerop a) 1 0))

(defun i32-eq (a b) (if (= a b) 1 0))
(defun i32-ne (a b) (if (/= a b) 1 0))
(defun i32-lt-s (a b) (if (< (i32-s a) (i32-s b)) 1 0))
(defun i32-lt-u (a b) (if (< a b) 1 0))
(defun i32-gt-s (a b) (if (> (i32-s a) (i32-s b)) 1 0))
(defun i32-gt-u (a b) (if (> a b) 1 0))
(defun i32-le-s (a b) (if (<= (i32-s a) (i32-s b)) 1 0))
(defun i32-le-u (a b) (if (<= a b) 1 0))
(defun i32-ge-s (a b) (if (>= (i32-s a) (i32-s b)) 1 0))
(defun i32-ge-u (a b) (if (>= a b) 1 0))

;;; ---------------------------------------------------------------
;;; i64 arithmetic: same shapes as i32, widened to 64 bits.

(macrolet
    ((defwrap64 (name lambda-list &body body)
       `(defun ,name ,lambda-list (i64-> (progn ,@body)))))
  (defwrap64 i64-add (a b) (+ a b))
  (defwrap64 i64-sub (a b) (- a b))
  (defwrap64 i64-mul (a b) (* a b)))

(defun i64-div-s (a b)
  (when (zerop b) (error 'wasmdown:wasm-trap :kind :integer-divide-by-zero
                         :message "i64.div_s divide by zero"))
  (let ((sa (i64-s a)) (sb (i64-s b)))
    (when (and (= sa (- (expt 2 63))) (= sb -1))
      (error 'wasmdown:wasm-trap :kind :integer-overflow
             :message "i64.div_s overflow"))
    (i64-> (truncate sa sb))))

(defun i64-div-u (a b)
  (when (zerop b) (error 'wasmdown:wasm-trap :kind :integer-divide-by-zero
                         :message "i64.div_u divide by zero"))
  (i64-> (truncate a b)))

(defun i64-rem-s (a b)
  (when (zerop b) (error 'wasmdown:wasm-trap :kind :integer-divide-by-zero
                         :message "i64.rem_s divide by zero"))
  (let ((sa (i64-s a)) (sb (i64-s b)))
    (if (and (= sa (- (expt 2 63))) (= sb -1))
        0
        (i64-> (rem sa sb)))))

(defun i64-rem-u (a b)
  (when (zerop b) (error 'wasmdown:wasm-trap :kind :integer-divide-by-zero
                         :message "i64.rem_u divide by zero"))
  (i64-> (rem a b)))

(defun i64-and (a b) (logand a b))
(defun i64-or (a b) (logior a b))
(defun i64-xor (a b) (logxor a b))
(defun i64-shl (a b) (i64-> (ash a (mod b 64))))
(defun i64-shr-s (a b)
  (i64-> (ash (i64-s a) (- (mod b 64)))))
(defun i64-shr-u (a b) (i64-> (ash a (- (mod b 64)))))

(defun i64-rotl (a b)
  (let ((k (mod b 64)))
    (i64-> (logior (ash (logand a #xffffffffffffffff) k)
                   (ash (logand a #xffffffffffffffff) (- k 64))))))
(defun i64-rotr (a b)
  (let ((k (mod b 64)))
    (i64-> (logior (ash (logand a #xffffffffffffffff) (- k))
                   (ash (logand a #xffffffffffffffff) (- 64 k))))))

(defun i64-clz (a)
  (if (zerop a) 64 (- 64 (integer-length (logand a #xffffffffffffffff)))))
(defun i64-ctz (a)
  (if (zerop a) 64 (1- (integer-length (logand a (- a))))))
(defun i64-popcnt (a) (logcount (logand a #xffffffffffffffff)))

(defun i64-eqz (a) (if (zerop a) 1 0))
(defun i64-eq (a b) (if (= a b) 1 0))
(defun i64-ne (a b) (if (/= a b) 1 0))
(defun i64-lt-s (a b) (if (< (i64-s a) (i64-s b)) 1 0))
(defun i64-lt-u (a b) (if (< a b) 1 0))
(defun i64-gt-s (a b) (if (> (i64-s a) (i64-s b)) 1 0))
(defun i64-gt-u (a b) (if (> a b) 1 0))
(defun i64-le-s (a b) (if (<= (i64-s a) (i64-s b)) 1 0))
(defun i64-le-u (a b) (if (<= a b) 1 0))
(defun i64-ge-s (a b) (if (>= (i64-s a) (i64-s b)) 1 0))
(defun i64-ge-u (a b) (if (>= a b) 1 0))

;;; ---------------------------------------------------------------
;;; Float helpers shared by f32 and f64.

(declaim (inline nan-p))
(defun nan-p (x)
  (and (floatp x) (sb-ext:float-nan-p x)))

;;; Bit-level reinterpretation via SBCL's exact IEEE-754 representations.
(declaim (inline f32-bits) (inline f64-bits)
         (inline f32-from-bits) (inline f64-from-bits))
(defun f32-bits (f)
  (declare (type single-float f))
  (logand #xffffffff (sb-kernel:single-float-bits f)))
(defun f32-from-bits (bits)
  (declare (type (unsigned-byte 32) bits))
  (sb-kernel:make-single-float (if (>= bits #x80000000) (- bits #x100000000) bits)))
(defun f64-bits (f)
  (declare (type double-float f))
  (let ((lo (logand #xffffffff (sb-kernel:double-float-low-bits f)))
        (hi (logand #xffffffff (sb-kernel:double-float-high-bits f))))
    (declare (type (unsigned-byte 32) lo hi))
    (logior (ash hi 32) lo)))
(defun f64-from-bits (bits)
  (declare (type (unsigned-byte 64) bits))
  (let* ((lo (logand bits #xffffffff))
          (hi (logand (ash bits -32) #xffffffff))
          (hi (if (>= hi #x80000000) (- hi #x100000000) hi)))
    (sb-kernel:make-double-float hi lo)))

(defun canonical-nan-p (x)
  "Canonical NaN: exponent all-ones, payload exactly the quiet bit.
The sign bit is unconstrained (spec nan:canonical class): a negative
canonical NaN (e.g. hardware propagation from -nan operands) matches."
  (and (nan-p x)
       (etypecase x
         (single-float (= (logand (f32-bits x) #x7fffffff) #x7fc00000))
         (double-float (= (logand (f64-bits x) #x7fffffffffffffff)
                          #x7ff8000000000000)))))

(defun arithmetic-nan-p (x)
  "NaN with MSB of payload set (quiet), any payload: the arithmetic NaN form."
  (and (nan-p x)
       (etypecase x
         (single-float (logtest (f32-bits x) #x00400000))
         (double-float (logtest (f64-bits x) #x0008000000000000)))))

;;; Quiet canonical NaN of the same float format as A (used for NaN propagation).
(declaim (inline canonical-nan))
(defun canonical-nan (a)
  (etypecase a
    (single-float (f32-from-bits #x7fc00000))
    (double-float (f64-from-bits #x7ff8000000000000))))

;;; Wasm trunc: toward zero.  FTRUNCATE is exactly that for floats.
(declaim (inline ftrunc))
(defun ftrunc (x) (ftruncate x))

;;; Wasm nearest: round-half-to-even.  FROUND is ties-to-even.
(declaim (inline fnearest))
(defun fnearest (x)
  (if (or (zerop x) (sb-ext:float-infinity-p x) (nan-p x))
      x
      (fround x)))

(declaim (inline fmin fmax %negzero-p))
(defun %negzero-p (x)
  "True when X is negative zero."
  (and (zerop x) (= -1.0 (float-sign x))))

(defun fmin (a b)
  (cond ((or (nan-p a) (nan-p b)) (canonical-nan a))
        ;; equal magnitudes: min prefers -0 (the neg-zero operand wins)
        ((= a b) (cond ((%negzero-p a) a)
                       ((%negzero-p b) b)
                       ((minusp a) a)
                       (t b)))
        ((< a b) a)
        (t b)))

(defun fmax (a b)
  (cond ((or (nan-p a) (nan-p b)) (canonical-nan a))
        ;; equal magnitudes: max prefers +0 (only -0 if both are -0)
        ((= a b) (if (and (%negzero-p a) (%negzero-p b))
                     a
                     (if (%negzero-p a) b a)))
        ((> a b) a)
        (t b)))

;;; ---------------------------------------------------------------
;;; f32 ops. SBCL float ops are IEEE-correct for add/sub/mul/div/sqrt.

(declaim (inline %f32))
(defun %f32 (x) (coerce x 'single-float))

(defun f32-abs (x) (declare (type single-float x)) (abs x))
(defun f32-neg (x) (declare (type single-float x)) (- x))
(defun f32-ceil (x) (declare (type single-float x)) (fceiling x))
(defun f32-floor (x) (declare (type single-float x)) (ffloor x))
(defun f32-trunc (x) (declare (type single-float x)) (ftrunc x))
(defun f32-nearest (x) (declare (type single-float x)) (fnearest x))
(defun f32-sqrt (x)
  (declare (type single-float x))
  (if (and (minusp x) (not (zerop x)))
      (f32-from-bits #x7fc00000)      ; sqrt(negative) = NaN in Wasm
      (sqrt x)))
(defun f32-add (a b) (declare (type single-float a b)) (%f32 (+ a b)))
(defun f32-sub (a b) (declare (type single-float a b)) (%f32 (- a b)))
(defun f32-mul (a b) (declare (type single-float a b)) (%f32 (* a b)))
(defun f32-div (a b) (declare (type single-float a b)) (%f32 (/ a b)))
(defun f32-min (a b) (declare (type single-float a b)) (fmin a b))
(defun f32-max (a b) (declare (type single-float a b)) (fmax a b))
(defun f32-copysign (a b)
  (declare (type single-float a b))
  (f32-from-bits (logior (logand (f32-bits a) #x7fffffff)
                         (logand (f32-bits b) #x80000000))))

(defun f32-eq (a b) (if (= a b) 1 0))
(defun f32-ne (a b) (if (not (= a b)) 1 0))
(defun f32-lt (a b) (if (< a b) 1 0))
(defun f32-gt (a b) (if (> a b) 1 0))
(defun f32-le (a b) (if (<= a b) 1 0))
(defun f32-ge (a b) (if (>= a b) 1 0))

;;; ---------------------------------------------------------------
;;; f64 ops.

(defun f64-abs (x) (declare (type double-float x)) (abs x))
(defun f64-neg (x) (declare (type double-float x)) (- x))
(defun f64-ceil (x) (declare (type double-float x)) (fceiling x))
(defun f64-floor (x) (declare (type double-float x)) (ffloor x))
(defun f64-trunc (x) (declare (type double-float x)) (ftrunc x))
(defun f64-nearest (x) (declare (type double-float x)) (fnearest x))
(defun f64-sqrt (x)
  (declare (type double-float x))
  (if (and (minusp x) (not (zerop x)))
      (f64-from-bits #x7ff8000000000000)  ; sqrt(negative) = NaN in Wasm
      (sqrt x)))
(defun f64-add (a b) (declare (type double-float a b)) (+ a b))
(defun f64-sub (a b) (declare (type double-float a b)) (- a b))
(defun f64-mul (a b) (declare (type double-float a b)) (* a b))
(defun f64-div (a b) (declare (type double-float a b)) (/ a b))
(defun f64-min (a b) (declare (type double-float a b)) (fmin a b))
(defun f64-max (a b) (declare (type double-float a b)) (fmax a b))
(defun f64-copysign (a b)
  (declare (type double-float a b))
  (f64-from-bits (logior (logand (f64-bits a) #x7fffffffffffffff)
                         (logand (f64-bits b) #x8000000000000000))))

(defun f64-eq (a b) (if (= a b) 1 0))
(defun f64-ne (a b) (if (not (= a b)) 1 0))
(defun f64-lt (a b) (if (< a b) 1 0))
(defun f64-gt (a b) (if (> a b) 1 0))
(defun f64-le (a b) (if (<= a b) 1 0))
(defun f64-ge (a b) (if (>= a b) 1 0))

;;; ---------------------------------------------------------------
;;; Conversions.

;;; i32.wrap_i64: truncate to the low 32 bits.
(declaim (inline i32-wrap-i64))
(defun i32-wrap-i64 (x) (i32-> x))

;;; i64.extend_i32_s / _u: sign-extend / zero-extend 32 -> 64 bits.
(declaim (inline i64-extend-i32-s) (inline i64-extend-i32-u))
(defun i64-extend-i32-s (x)
  (let ((x (i32-> x)))
    (i64-> (if (>= x #x80000000) (- x #x100000000) x))))
(defun i64-extend-i32-u (x) (i64-> (i32-> x)))

;;; Trapping float->int truncation.  Bounds: trunc(z) must land in
;;; [-2^(N-1), 2^(N-1)) (signed) or [0, 2^N) (unsigned).  NaN traps
;;; :invalid-conversion; out-of-range traps :integer-overflow.
(defmacro define-trunc (name float-type width signedp)
  "Define a trapping float->int truncation NAME for the given WIDTH."
  (let ((minv (if signedp (- (expt 2 (1- width))) 0))
        (maxv (if signedp (- (expt 2 (1- width)) 1) (- (expt 2 width) 1))))
    `(progn
       (declaim (ftype (function (,float-type) (integer 0 *)) ,name))
       (defun ,name (x)
         (cond ((nan-p x)
                (error 'wasmdown:wasm-trap :kind :invalid-conversion
                       :message ,(format nil "~(~a~): NaN" name)))
               ((sb-ext:float-infinity-p x)
                (error 'wasmdown:wasm-trap :kind :integer-overflow
                       :message ,(format nil "~(~a~): out of range" name)))
               (t
                (let ((n (round (ftruncate x))))  ; exact integral value
                  (declare (type integer n))
                  (if (or (< n ,minv) (> n ,maxv))
                      (error 'wasmdown:wasm-trap :kind :integer-overflow
                             :message ,(format nil "~(~a~): out of range" name))
                      (,(ecase width (32 'i32->) (64 'i64->)) n)))))))))


(define-trunc i32-trunc-f32-s single-float 32 t)
(define-trunc i32-trunc-f32-u single-float 32 nil)
(define-trunc i32-trunc-f64-s double-float 32 t)
(define-trunc i32-trunc-f64-u double-float 32 nil)
(define-trunc i64-trunc-f32-s single-float 64 t)
(define-trunc i64-trunc-f32-u single-float 64 nil)
(define-trunc i64-trunc-f64-s double-float 64 t)
(define-trunc i64-trunc-f64-u double-float 64 nil)

;;; Saturating float->int truncation: NaN -> 0, clamp out-of-range,
;;; never traps.
(defmacro define-trunc-sat (name float-type width signedp)
  (let ((minv (if signedp (- (expt 2 (1- width))) 0))
        (maxv (if signedp (- (expt 2 (1- width)) 1) (- (expt 2 width) 1))))
    `(progn
       (declaim (ftype (function (,float-type) (integer 0 *)) ,name))
       (defun ,name (x)
         (declare (type ,float-type x))
         (let ((n (cond ((nan-p x) 0)
                        ((sb-ext:float-infinity-p x)
                         (if (plusp x) ,maxv ,minv))
                        (t (round (ftruncate x))))))
           (declare (type integer n))
           (let ((n (min ,maxv (max ,minv n))))
             (,(ecase width (32 'i32->) (64 'i64->)) n)))))))

(define-trunc-sat i32-trunc-sat-f32-s single-float 32 t)
(define-trunc-sat i32-trunc-sat-f32-u single-float 32 nil)
(define-trunc-sat i32-trunc-sat-f64-s double-float 32 t)
(define-trunc-sat i32-trunc-sat-f64-u double-float 32 nil)
(define-trunc-sat i64-trunc-sat-f32-s single-float 64 t)
(define-trunc-sat i64-trunc-sat-f32-u single-float 64 nil)
(define-trunc-sat i64-trunc-sat-f64-s double-float 64 t)
(define-trunc-sat i64-trunc-sat-f64-u double-float 64 nil)

;;; ---------------------------------------------------------------
;;; int -> float conversions.  Lisp integers are exact, so COERCE
;;; round-to-nearest is exact IEEE conversion.

(declaim (inline %to-f32) (inline %to-f64))
(defun %to-f32 (n) (coerce n 'single-float))
(defun %to-f64 (n) (coerce n 'double-float))

(defun f32-convert-i32-s (x) (%to-f32 (i32-s x)))
(defun f32-convert-i32-u (x) (%to-f32 (i32-u x)))
(defun f32-convert-i64-s (x) (%to-f32 (i64-s x)))
(defun f32-convert-i64-u (x) (%to-f32 (i64-u x)))
(defun f64-convert-i32-s (x) (%to-f64 (i32-s x)))
(defun f64-convert-i32-u (x) (%to-f64 (i32-u x)))
(defun f64-convert-i64-s (x) (%to-f64 (i64-s x)))
(defun f64-convert-i64-u (x) (%to-f64 (i64-u x)))

;;; f32.demote_f64 / f64.promote_f32: COERCE rounds to nearest even.
(defun f32-demote-f64 (x) (%to-f32 x))
(defun f64-promote-f32 (x) (%to-f64 x))

;;; reinterpret: pure bit moves.
(defun i32-reinterpret-f32 (f) (f32-bits f))
(defun i64-reinterpret-f64 (f) (f64-bits f))
(defun f32-reinterpret-i32 (bits) (f32-from-bits bits))
(defun f64-reinterpret-i64 (bits) (f64-from-bits bits))

;;; Lax float equality: NaN == NaN counts as equal here (test helper
;;; semantics); distinguishes only numeric difference.
(defun float-eq-nan-lax (a b)
  (or (= a b)
      (and (nan-p a) (nan-p b))))

;;; ---------------------------------------------------------------
;;; Little-endian memory access (caller guarantees bounds).

(declaim (inline mem-load-le) (inline mem-store-le))
(defun mem-load-le (bytes offset width)
  "Read WIDTH (1/2/4/8) little-endian bytes from BYTES at OFFSET."
  (declare (type (vector (unsigned-byte 8)) bytes)
           (type (integer 0 *) offset)
           (type (member 1 2 4 8) width))
  (let ((acc 0))
    (declare (type (unsigned-byte 64) acc))
    (loop for i from 0 below width
          do (setf acc (logior acc (ash (aref bytes (+ offset i)) (* 8 i)))))
    acc))

(defun mem-store-le (bytes offset width value)
  "Store the low WIDTH bytes of VALUE at OFFSET in BYTES, little-endian."
  (declare (type (vector (unsigned-byte 8)) bytes)
           (type (integer 0 *) offset)
           (type (member 1 2 4 8) width)
           (type (unsigned-byte 64) value))
  (loop for i from 0 below width
        do (setf (aref bytes (+ offset i))
                 (logand #xff (ash value (- (* 8 i))))))
  value)
