;;;; Unit tests for wasmdown.binary: golden decode tests over
;;;; hand-built byte vectors, plus encode/decode round-trip tests via
;;;; canonical byte comparison (encode m == encode (decode (encode m)))
;;;; and idempotency of the decoded-then-reencoded byte stream.
;;;;
;;;; NOTE on comparators: decoded modules carry source-map offsets that
;;;; legitimately differ between decodes of differently-laid-out byte
;;;; vectors, so structural equality on wasm-module values is NOT a
;;;; valid oracle. All round-trip checks compare canonical encoder
;;;; output (bytes) or specific IR fields.

(defpackage #:wasmdown.test.binary
  (:use #:cl)
  (:nicknames #:wd.test.binary))

(in-package #:wasmdown.test.binary)

;;; -----------------------------------------------------------------
;;; Byte-vector helpers: hand-written literals, LEB128 spelled out.

(defun bv (&rest bytes)
  "Build a byte vector from literal bytes."
  (make-array (length bytes) :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(defun u32 (n)
  "Canonical minimal unsigned LEB128 bytes of N (for building goldens)."
  (let ((acc '()))
    (loop
      (let ((b (logand n #x7F)))
        (setf n (ash n -7))
        (if (zerop n)
            (progn (push b acc)
                   (return (coerce (nreverse acc) '(vector (unsigned-byte 8)))))
            (push (logior b #x80) acc))))))

(defun s32 (n)
  "Canonical minimal signed LEB128 bytes of N."
  (let ((acc '()))
    (loop
      (let ((b (logand n #x7F)))
        (setf n (ash n -7))
        (cond ((or (and (zerop n) (not (logtest b #x40)))
                   (and (= n -1) (logtest b #x40)))
               (push b acc)
               (return (coerce (nreverse acc) '(vector (unsigned-byte 8)))))
              (t (push (logior b #x80) acc)))))))

(defun cat (&rest parts)
  "Concatenate byte vectors (integers are single bytes)."
  (let ((parts (mapcar (lambda (p) (if (integerp p) (bv p) p)) parts)))
    (make-array (reduce #'+ parts :key #'length :initial-value 0)
                :element-type '(unsigned-byte 8)
                :initial-contents
                (apply #'append (mapcar (lambda (v) (coerce v 'list)) parts)))))

(defparameter *magic* #(0 #x61 #x73 #x6D))
(defparameter *version* #(1 0 0 0))
(defparameter *header* (cat *magic* *version*))

(defun section (id &rest content)
  "id byte + u32 size + content bytes."
  (let ((parts (mapcar (lambda (p) (if (integerp p) (bv p) p)) content)))
    (cat (bv id)
         (u32 (reduce #'+ parts :key #'length :initial-value 0))
         (apply #'cat parts))))

(defun str-bytes (s)
  "UTF-8 bytes of a pure-ASCII-or-Latin-1-free helper string: map
chars below #x80 directly; caller uses explicit bytes otherwise."
  (map '(vector (unsigned-byte 8)) #'char-code s))

;;; -----------------------------------------------------------------
;;; Golden decode helpers.

(defun dec (bytes) (wasmdown.binary:decode-module bytes))

(defun decode-error-at (bytes expected-offset)
  "Decode BYTES; return the decode-error iff its offset is
EXPECTED-OFFSET; return NIL on success or offset mismatch."
  (block nil
    (handler-bind ((wasmdown:decode-error
                    (lambda (c)
                      (return (and (= (wasmdown:decode-error-offset c)
                                      expected-offset)
                                   c)))))
      (dec bytes)
      nil)))

(defun assert-decode-error (bytes expected-offset)
  (wasmdown.test:is (decode-error-at bytes expected-offset)))


(defun decode-error-p (bytes)
  "Whether decoding BYTES signals any wasmdown:decode-error."
  (block nil
    (handler-bind ((wasmdown:decode-error (lambda (c) (return t))))
      (dec bytes)
      nil)))
;;; -----------------------------------------------------------------
;;; IR builders for round-trip modules (thin wrappers: this file lives
;;; in the WASMDOWN.TEST package, so type constructors are qualified).

(defun i (op &optional imms body)
  (wasmdown.types:make-wasm-instr op imms (or body #()) 0))

(defun endi () (i :end))

(defun ft (params results)
  (wasmdown.types:make-wasm-func-type params results))

(defun fn (typeidx locals body)
  (wasmdown.types:make-wasm-func typeidx locals body 0))

(defun lim (min &optional max)
  (if max
      (wasmdown.types:make-wasm-limits min max)
      (wasmdown.types:make-wasm-limits min)))

(defun tab (reftype &optional (min 1) max)
  (wasmdown.types:make-wasm-table reftype (lim min max)))

(defun mem (&optional (min 1) max)
  (wasmdown.types:make-wasm-memory (lim min max)))

(defun xexp (name kind index)
  (wasmdown.types:make-wasm-export name kind index))

(defun glob (type mutable init)
  (wasmdown.types:make-wasm-global :type type :mutable mutable :init init))

(defun mmod (&rest keys &key types imports funcs tables mems globals
                          exports start elems datas customs data-count
                          &allow-other-keys)
  (apply #'wasmdown.types:make-wasm-module
         :types (or types #()) :imports (or imports #())
         :funcs (or funcs #()) :tables (or tables #())
         :mems (or mems #()) :globals (or globals #())
         :exports (or exports #()) :start start
         :elems (or elems #()) :datas (or datas #())
         :customs (or customs #()) :data-count data-count keys))

(defun ref-func-expr (n)
  (vector (i :ref.func (list :func n)) (endi)))

(defun ref-null-expr (ty)
  (vector (i :ref.null (list :type ty)) (endi)))

(defun i32-const-expr (n)
  (vector (i :i32.const (list :value n)) (endi)))

(defun roundtrip-bytes (m)
  "Encode M, decode, re-encode: canonical bytes must be identical.
Returns the canonical encoding."
  (let* ((e1 (wasmdown.binary:encode-module m))
         (d  (dec e1))
         (e2 (wasmdown.binary:encode-module d)))
    (wasmdown.test:is (equalp e1 e2))
    e2))

(defun assert-roundtrip (m)
  (roundtrip-bytes m)
  (values))

;;; ================================================================
;;; Golden: magic / version / LEB128 / section ordering.

(wasmdown.test:test binary/golden-magic-version
  ;; (a) magic+version decode to an empty module.
  (let ((m (dec (cat *magic* *version*))))
    (wasmdown.test:is (zerop (length (wasmdown.types:wasm-module-types m))))
    (wasmdown.test:is (zerop (length (wasmdown.types:wasm-module-funcs m))))
    (wasmdown.test:is (null (wasmdown.types:wasm-module-start m))))
  ;; (b) bad magic -> decode-error at offset 0.
  (assert-decode-error (cat (bv 0 #x62 #x73 #x6D) *version*) 0)
  ;; (c) bad version -> decode-error at offset 4.
  (assert-decode-error (cat *magic* (bv 2 0 0 0)) 4)
  (assert-decode-error (cat *magic* (bv 0 0 0 0)) 4)
  ;; truncated header (missing version bytes)
  (assert-decode-error (cat *magic* #(1 0 0)) 0))

(wasmdown.test:test binary/golden-padded-uleb
  ;; (d) padded 5-byte u32 LEB for 0 accepted (0x80 0x80 0x80 0x80
  ;; 0x00), here as a custom-section name length.
  (let ((m (dec (cat *header*
                     (section 0 (bv #x80 #x80 #x80 #x80 #x00)
                              (bv #xDE #xAD))))))
    (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-module-customs m))))
    (let ((c (svref (wasmdown.types:wasm-module-customs m) 0)))
      (wasmdown.test:is (equal "" (wasmdown.types:wasm-custom-name c)))
      (wasmdown.test:is (equalp #(222 173)
                                (wasmdown.types:wasm-custom-bytes c)))))
  ;; padded form of 1 as a name length, name "a", one payload byte.
  (let ((m (dec (cat *header*
                     (section 0 (bv #x81 #x80 #x80 #x80 #x00)
                              (bv #x61) (bv #xAB))))))
    (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-module-customs m))))
    (let ((c (svref (wasmdown.types:wasm-module-customs m) 0)))
      (wasmdown.test:is (equal "a" (wasmdown.types:wasm-custom-name c)))
      (wasmdown.test:is (equalp #(171) (wasmdown.types:wasm-custom-bytes c))))))

(wasmdown.test:test binary/golden-uleb-too-long
  ;; (e) 6-byte u32 (encoding 0) rejected at the field's first byte.
  (assert-decode-error
   (cat *header*
        (bv 0)                             ; custom section id at offset 8
        (bv #x80 #x80 #x80 #x80 #x80 #x00) ; 6-byte size at offset 9
        (bv 0))
   9))

(wasmdown.test:test binary/golden-uleb-overflow
  ;; (f) 5-byte u32 with high bits set in the last byte: value 2^35-1
  ;; overflows u32 -> "integer too large" at the field's first byte.
  (assert-decode-error
   (cat *header*
        (bv 0)
        (bv #xFF #xFF #xFF #xFF #x7F)
        (bv 0))
   9))

(wasmdown.test:test binary/golden-sleb-padded-minus-one
  ;; (g) padded signed -1 (0xFF 0xFF 0xFF 0xFF 0x7F) accepted as an
  ;; i32.const immediate; decoded immediate is the signed value -1.
  (let ((m (dec (cat *header*
                     (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                     (section 3 (u32 1) (u32 0))
                     (section 10
                              (u32 1)
                              (u32 8)
                              (u32 0)
                              (bv #x41)
                              (bv #xFF #xFF #xFF #xFF #x7F)
                              (bv #x0B))))))
    (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-module-funcs m))))
    (let ((f (svref (wasmdown.types:wasm-module-funcs m) 0)))
      (wasmdown.test:is (null (wasmdown.types:wasm-func-locals f)))
      (let ((i0 (svref (wasmdown.types:wasm-func-body f) 0)))
        (wasmdown.test:is (eq :i32.const (wasmdown.types:wasm-instr-op i0)))
        (wasmdown.test:is (= -1 (getf (wasmdown.types:wasm-instr-imms i0)
                                      :value)))))))

(wasmdown.test:test binary/golden-section-order
  ;; (h) func section (id 3) before type section (id 1) rejected at
  ;; the offending section's start (offset 11 = header 8 + the
  ;; 3-byte func section).
  (assert-decode-error
   (cat *header*
        (section 3 (u32 0))
        (section 1 (u32 0)))
   11))

(wasmdown.test:test binary/golden-duplicate-section
  ;; (i) duplicate section id rejected at the second occurrence.
  (assert-decode-error
   (cat *header*
        (section 1 (u32 0))
        (section 1 (u32 0)))
   11))

(wasmdown.test:test binary/golden-unknown-section-id
  ;; (j) unknown section id 14 rejected — id 13 is the EH tag section
  ;; (Core 3.0); binary.wast ::32 pins 14 as the first unknown id.
  (assert-decode-error
   (cat *header* (section 14 (u32 0)))
   8))

(wasmdown.test:test binary/golden-custom-section
  ;; (k) custom section decoded inertly: name + bytes preserved.
  (let ((m (dec (cat *header*
                     (section 0 (u32 4) (str-bytes "name")
                              (bv #xDE #xAD #xBE #xEF))))))
    (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-module-customs m))))
    (let ((c (svref (wasmdown.types:wasm-module-customs m) 0)))
      (wasmdown.test:is (equal "name" (wasmdown.types:wasm-custom-name c)))
      (wasmdown.test:is (equalp #(222 173 190 239)
                                (wasmdown.types:wasm-custom-bytes c))))))

(wasmdown.test:test binary/golden-utf8-names
  ;; (l) valid multibyte UTF-8 export name accepted: "foé" (2-byte
  ;; é) + 耀 (3-byte) = 7 bytes.
  (let ((m (dec (cat *header*
                     (section 7 (u32 1) (u32 7)
                              (bv #x66 #x6F #xC3 #xA9 #xE8 #x80 #x80)
                              (bv 0) (u32 0))))))
    (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-module-exports m))))
    (let ((ex (svref (wasmdown.types:wasm-module-exports m) 0)))
      (wasmdown.test:is (equal "foé耀"
                               (wasmdown.types:wasm-export-name ex))))))

(wasmdown.test:test binary/golden-invalid-utf8
  ;; (m) invalid UTF-8 (lone continuation byte 0x80) rejected; the
  ;; error offset points just past the bad lead byte.
  (let* ((bad (cat *header*
                   (section 7 (u32 1) (u32 1)
                            (bv #x80) (bv 0) (u32 0))))
         (ok (decode-error-at bad 13)))
    (wasmdown.test:is ok)))

(wasmdown.test:test binary/golden-code-func-mismatch
  ;; (n) function section declares 1 func but code section has 0
  ;; entries -> decode-error.
  (let ((bad (cat *header*
                  (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                  (section 3 (u32 1) (u32 0))
                  (section 10 (u32 0)))))
    (wasmdown.test:is (decode-error-p bad)))
  ;; and the converse: code section declares 1 body, func section 0.
  (let ((bad (cat *header*
                  (section 3 (u32 0))
                  (section 10 (u32 1) (u32 2) (u32 0) (bv #x0B)))))
    (wasmdown.test:is (decode-error-p bad))))

(wasmdown.test:test binary/golden-truncated-body
  ;; (o) function body declares more bytes than the section holds ->
  ;; decode-error (truncated).
  (let ((bad (cat *header*
                  (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                  (section 3 (u32 1) (u32 0))
                  (section 10 (u32 1)
                           (u32 4)        ; body size 4
                           (u32 0)        ; locals count
                           (bv #x41) (u32 1) ; i32.const then END MISSING
                           ))))
    (wasmdown.test:is (decode-error-p bad))))

(wasmdown.test:test binary/golden-memarg-memidx
  ;; (p) memarg with bit 6 set (align|0x40) followed by memidx decodes
  ;; to a memarg containing (:memidx n).
  ;;
  ;; Part 1: the memarg decoder itself, probed directly via the
  ;; reader.
  (let* ((r (wasmdown.binary:make-reader
             (make-array 5 :element-type '(unsigned-byte 8)
                         :initial-contents #(#x42 1 3 0 0))))
         (m (wasmdown.binary::%read-memarg r)))
    (wasmdown.test:is (= 2 (getf m :align)))
    (wasmdown.test:is (= 3 (getf m :offset)))
    (wasmdown.test:is (= 1 (getf m :memidx))))
  ;; non-bit-6 form: plain align + offset, no memidx.
  (let* ((r (wasmdown.binary:make-reader
             (make-array 2 :element-type '(unsigned-byte 8)
                         :initial-contents #(#x02 7))))
         (m (wasmdown.binary::%read-memarg r)))
    (wasmdown.test:is (= 2 (getf m :align)))
    (wasmdown.test:is (= 7 (getf m :offset)))
    (wasmdown.test:is (null (getf m :memidx))))
  ;; Part 2 (D1 fixed): a function body containing the real i32.load
  ;; opcode byte 0x28 decodes to :i32.load with its memarg intact,
  ;; including the multi-memory (:memidx n) form.
  (let ((m (dec (cat *header*
                    (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                    (section 3 (u32 1) (u32 0))
                    (section 5 (u32 2) (u32 0) (u32 1)
                             (u32 0) (u32 1))
                    (section 10
                             (u32 1)
                             (u32 9)
                             (u32 0)
                             (bv #x41) (u32 0)
                             (bv #x28)
                             (bv #x42) (u32 1) (u32 3)
                             (bv #x1A)
                             (bv #x0B))))))
    (let* ((f (svref (wasmdown.types:wasm-module-funcs m) 0))
           (ins (svref (wasmdown.types:wasm-func-body f) 1))
           (imm (wasmdown.types:wasm-instr-imms ins)))
      (wasmdown.test:is (eq :i32.load (wasmdown.types:wasm-instr-op ins)))
      (wasmdown.test:is (= 2 (getf imm :align)))
      (wasmdown.test:is (= 3 (getf imm :offset)))
      (wasmdown.test:is (= 1 (getf imm :memidx))))))

(wasmdown.test:test binary/spec-memop-flags-malformed
  ;; align.wast: memarg flags is a u32 that must be < 0x80 —
  ;; 0x80 0x01 (align exponent 2^128) and 0x80 0x02 (2^256) are
  ;; "malformed memop flags" DECODE errors (reference decode.ml memop
  ;; requires I32.lt_u flags 0x80l).
  (dolist (flags-bytes '(#(#x80 #x01) #(#x80 #x02)))
    (let ((bad (cat *header*
                    (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                    (section 3 (u32 1) (u32 0))
                    (section 5 (u32 1) (bv #x00) (u32 1))
                    (section 10 (u32 1) (u32 8) (u32 0)
                             (bv #x41) (u32 0)
                             (bv #x28)
                             (apply #'bv (coerce flags-bytes 'list))
                             (u32 0)
                             (bv #x1A) (bv #x0B)))))
      (wasmdown.test:is (decode-error-p bad))))
  ;; control: flags 0x02 (align 2) with offset 0 decodes fine.
  (let ((good (dec (cat *header*
                        (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                        (section 3 (u32 1) (u32 0))
                        (section 5 (u32 1) (bv #x00) (u32 1))
                        (section 10 (u32 1) (u32 8) (u32 0)
                                 (bv #x41) (u32 0)
                                 (bv #x28) (bv #x02) (u32 0)
                                 (bv #x1A) (bv #x0B))))))
    (wasmdown.test:is (wasmdown.types:wasm-module-p good))))

(wasmdown.test:test binary/golden-datacount-position
  ;; (q) DataCount (12) between Elem (9) and Code (10): accepted.
  ;; Also Data (11) after Code (10): accepted.
  (let ((m (dec (cat *header*
                     (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                     (section 3 (u32 1) (u32 0))
                     (section 5 (u32 1) (bv #x00) (u32 1))
                     (section 12 (u32 1))          ; datacount 1
                     (section 10 (u32 1) (u32 2) (u32 0) (bv #x0B))
                     (section 11 (u32 1)
                              (u32 1) (u32 2) (bv #xAA) (bv #xBB))))))
    (wasmdown.test:is (= 1 (wasmdown.types:wasm-module-data-count m)))
    (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-module-datas m))))))

(wasmdown.test:test binary/golden-data-flag2
  ;; (r) data segment flag 2: active with explicit memidx 1.
  (let ((m (dec (cat *header*
                     (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                     (section 5 (u32 2) (u32 0) (u32 1) ; mem0 min1
                              (u32 0) (u32 1))         ; mem1 min1
                     (section 12 (u32 1))
                     (section 11 (u32 1)
                              (u32 2)                 ; flag 2
                              (u32 1)                 ; memidx 1
                              (bv #x41) (u32 5) (bv #x0B) ; offset expr
                              (u32 3) (bv 1 2 3))))))
    (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-module-datas m))))
    (let* ((d (svref (wasmdown.types:wasm-module-datas m) 0))
           (mode (wasmdown.types:wasm-data-mode d)))
      (wasmdown.test:is (eq :active (first mode)))
      (wasmdown.test:is (= 1 (second mode)))
      (wasmdown.test:is (equalp #(1 2 3) (wasmdown.types:wasm-data-bytes d))))))

(wasmdown.test:test binary/golden-elem-flag4
  ;; (s) elem segment flag 4: active table 0 with elemexpr funcref
  ;; initializers (ref.func n) end.
  (let ((m (dec (cat *header*
                     (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                     (section 3 (u32 1) (u32 0))
                     (section 4 (u32 1) (bv #x70) (u32 0) (u32 2))
                     (section 9 (u32 1)
                              (u32 4)                 ; flag 4
                              (bv #x41) (u32 0) (bv #x0B) ; offset expr
                              (u32 1)                 ; 1 init expr
                              (bv #xD2) (u32 0) (bv #x0B)))))) ; ref.func 0 end
    (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-module-elems m))))
    (let* ((el (svref (wasmdown.types:wasm-module-elems m) 0))
           (mode (wasmdown.types:wasm-elem-mode el)))
      (wasmdown.test:is (eq :active (first mode)))
      (wasmdown.test:is (zerop (second mode)))
      (wasmdown.test:is (eq :funcref (wasmdown.types:wasm-elem-type el)))
      (wasmdown.test:is (= 1 (length (wasmdown.types:wasm-elem-inits el))))
      (let ((init (first (wasmdown.types:wasm-elem-inits el))))
        (wasmdown.test:is (= 2 (length init)))
        (wasmdown.test:is (eq :ref.func
                              (wasmdown.types:wasm-instr-op (svref init 0))))
        (wasmdown.test:is (eq :end
                              (wasmdown.types:wasm-instr-op (svref init 1))))))))

;;; ================================================================
;;; Round-trip: canonical byte comparison (encode m == encode (decode
;;; (encode m))). The decoder/encoder discrepancies D1..D5 found
;;; while writing this suite (opcode table misalignment, f64 byte
;;; order, locals wire order, custom-section name length, untyped
;;; select) are FIXED in src/binary.lisp / src/binary-encode.lisp;
;;; their spec-alignment regressions live at the end of this file.
;;; Round-trip tests below cover all families end-to-end.

(wasmdown.test:test binary/roundtrip-empty
  (assert-roundtrip (mmod)))

(wasmdown.test:test binary/roundtrip-type-func-export
  (assert-roundtrip (mmod :types (vector (ft '(:i32) '(:i32))) :funcs (vector (fn 0 nil (vector (i :local.get (list :local 0)) (i :i32.const (list :value 10)) (i :i32.add) (endi)))) :exports (vector (xexp "f" :func 0)))))

(wasmdown.test:test binary/roundtrip-if-else
  (assert-roundtrip (mmod :types (vector (ft nil '(:i32))) :funcs (vector (fn 0 nil (vector (i :i32.const (list :value 1)) (i :if (list :bt :i32 :else-body (vector (i :i32.const (list :value 2)))) (vector (i :i32.const (list :value 3)))) (endi)))))))

(wasmdown.test:test binary/roundtrip-if-no-else
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :i32.const (list :value 1)) (i :if (list :bt nil) (vector (i :nop))) (endi)))))))

(wasmdown.test:test binary/roundtrip-nested-blocks-br
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :block nil (vector (i :block nil (vector (i :i32.const (list :value 1)) (i :br-if (list :label 1)))) (i :br (list :label 0)))) (i :end)))))))

(wasmdown.test:test binary/roundtrip-loop
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :loop nil (vector (i :br (list :label 0)))) (i :end)))))))

(wasmdown.test:test binary/roundtrip-blocktype-typeidx
  (assert-roundtrip (mmod :types (vector (ft nil '(:i32 :i32)) (ft nil nil)) :funcs (vector (fn 1 nil (vector (i :block (list :bt (cons :typeidx 0)) (vector (i :i32.const (list :value 1)) (i :i32.const (list :value 2)))) (i :drop) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-br-table
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :block nil (vector (i :br-table (list :targets (vector 0 1) :default 0)))) (i :end)))))))

(wasmdown.test:test binary/roundtrip-call-indirect
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :tables (vector (tab :funcref)) :funcs (vector (fn 0 nil (vector (i :end))) (fn 0 nil (vector (i :call (list :func 0)) (i :i32.const (list :value 0)) (i :call-indirect (list :type 0 :table 0)) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-select-typed
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :i32.const (list :value 1)) (i :i32.const (list :value 2)) (i :i32.const (list :value 0)) (i :select (list :types (vector :i32))) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-globals
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :globals (vector (glob :i32 nil (i32-const-expr 42)) (glob :i64 t (vector (i :i64.const (list :value 7)) (endi)))) :funcs (vector (fn 0 nil (vector (i :global.get (list :global 0)) (i :global.set (list :global 1)) (i :end)))))))

(wasmdown.test:test binary/roundtrip-imports-all-kinds
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :imports (vector (wasmdown.types:make-wasm-import "m" "f" :func 0) (wasmdown.types:make-wasm-import "m" "t" :table (tab :funcref 1 2)) (wasmdown.types:make-wasm-import "m" "mem" :memory (mem 1)) (wasmdown.types:make-wasm-import "m" "g" :global (cons :i32 t))))))

(wasmdown.test:test binary/roundtrip-exports-all-kinds
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :end)))) :tables (vector (tab :funcref)) :mems (vector (mem 1)) :globals (vector (glob :i32 nil (i32-const-expr 1))) :exports (vector (xexp "f" :func 0) (xexp "t" :table 0) (xexp "m" :memory 0) (xexp "g" :global 0)))))

(wasmdown.test:test binary/roundtrip-start
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :end)))) :start 0)))

(wasmdown.test:test binary/roundtrip-float-consts-bits
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :f32.const (list :bits #x7fc12345)) (i :drop) (i :f32.const (list :bits #x3f800000)) (i :drop) (i :f64.const (list :bits 0)) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-i64-arith
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :i64.const (list :value -5)) (i :drop) (i :i64.const (list :value 1234567890123)) (i :i64.const (list :value 2)) (i :i64.mul) (i :drop) (i :i64.const (list :value 10)) (i :i64.const (list :value 3)) (i :i64.div_s) (i :i64.const (list :value 3)) (i :i64.rem_u) (i :i64.const (list :value 7)) (i :i64.and) (i :i64.const (list :value 2)) (i :i64.shl) (i :i64.const (list :value 1)) (i :i64.rotl) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-sat-trunc-signext
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :f32.const (list :bits 0)) (i :i32.trunc_sat_f32_s) (i :drop) (i :f64.const (list :bits 0)) (i :i64.trunc_sat_f64_u) (i :drop) (i :i32.const (list :value 1)) (i :i32.extend8_s) (i :drop) (i :i64.const (list :value 1)) (i :i64.extend16_s) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-memory-size-grow
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :mems (vector (mem 1)) :funcs (vector (fn 0 nil (vector (i :memory.size (list :memidx 0)) (i :drop) (i :i32.const (list :value 1)) (i :memory.grow (list :memidx 0)) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-local-ops
  (assert-roundtrip (mmod :types (vector (ft '(:i32) nil)) :funcs (vector (fn 0 nil (vector (i :local.get (list :local 0)) (i :local.set (list :local 1)) (i :local.get (list :local 1)) (i :local.tee (list :local 2)) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-table-ops
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :tables (vector (tab :funcref) (tab :funcref)) :elems (vector (wasmdown.types:make-wasm-elem '(:passive) :funcref (list (ref-func-expr 0)))) :funcs (vector (fn 0 nil (vector (i :i32.const (list :value 0)) (i :ref.null (list :type :funcref)) (i :table.set (list :table 0)) (i :i32.const (list :value 0)) (i :table.get (list :table 0)) (i :drop) (i :table.size (list :table 0)) (i :drop) (i :ref.null (list :type :funcref)) (i :i32.const (list :value 1)) (i :table.grow (list :table 0)) (i :drop) (i :i32.const (list :value 0)) (i :ref.null (list :type :funcref)) (i :i32.const (list :value 1)) (i :table.fill (list :table 0)) (i :drop) (i :i32.const (list :value 0)) (i :i32.const (list :value 0)) (i :i32.const (list :value 1)) (i :table.copy (list :dst 0 :src 1)) (i :drop) (i :i32.const (list :value 0)) (i :i32.const (list :value 0)) (i :i32.const (list :value 1)) (i :table.init (list :elem 0 :table 0)) (i :drop) (i :elem.drop (list :elem 0)) (i :ref.func (list :func 0)) (i :ref.is-null) (i :drop) (i :end)))))))

(wasmdown.test:test binary/roundtrip-bulk-memory
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :mems (vector (mem 1)) :datas (vector (wasmdown.types:make-wasm-data '(:passive) (make-array 2 :element-type '(unsigned-byte 8) :initial-contents #(1 2)))) :funcs (vector (fn 0 nil (vector (i :i32.const (list :value 0)) (i :i32.const (list :value 0)) (i :i32.const (list :value 2)) (i :memory.init (list :data 0 :memidx 0)) (i :data.drop (list :data 0)) (i :i32.const (list :value 0)) (i :i32.const (list :value 0)) (i :i32.const (list :value 2)) (i :memory.copy (list :dst 0 :src 0)) (i :i32.const (list :value 0)) (i :i32.const (list :value 0)) (i :i32.const (list :value 2)) (i :memory.fill (list :memidx 0)) (i :end)))))))

(wasmdown.test:test binary/roundtrip-elem-segments
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :tables (vector (tab :funcref)) :funcs (vector (fn 0 nil (vector (i :end)))) :elems (vector (wasmdown.types:make-wasm-elem '(:passive) :funcref (list (ref-func-expr 0) (ref-null-expr :funcref))) (wasmdown.types:make-wasm-elem '(:declarative) :funcref (list (ref-func-expr 0))) (wasmdown.types:make-wasm-elem (list :active 0 (i32-const-expr 0)) :funcref (list (ref-func-expr 0)))))))

(wasmdown.test:test binary/roundtrip-elem-externref
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :funcs (vector (fn 0 nil (vector (i :end)))) :elems (vector (wasmdown.types:make-wasm-elem '(:passive) :externref (list (ref-null-expr :externref)))))))

(wasmdown.test:test binary/roundtrip-data-segments
  (assert-roundtrip (mmod :types (vector (ft nil nil)) :mems (vector (mem 1)) :funcs (vector (fn 0 nil (vector (i :end)))) :datas (vector (wasmdown.types:make-wasm-data '(:passive) (make-array 3 :element-type '(unsigned-byte 8) :initial-contents #(1 2 3))) (wasmdown.types:make-wasm-data (list :active 0 (i32-const-expr 0)) (make-array 2 :element-type '(unsigned-byte 8) :initial-contents #(9 8)))))))

;;; ================================================================
;;; Spec-alignment regressions: these families were pinned as known
;;; discrepancies (D1..D5) while the decoder/encoder disagreed with the
;;; real WebAssembly binary format; the sources are fixed now, so each
;;; test asserts the SPEC-CORRECT behavior end-to-end.

(wasmdown.test:test binary/spec-alignment-table-misalignment
  ;; D1 fixed: the decoder's *simple-ops* table is aligned with real
  ;; opcode bytes. Real i32.load 0x28 decodes to :i32.load with its
  ;; memarg; real i32.add 0x6A decodes to :i32.add.
  (wasmdown.test:is (eq :i32.load (svref wasmdown.binary::*simple-ops* #x28)))
  (wasmdown.test:is (eq :i32.add (svref wasmdown.binary::*simple-ops* #x6A)))
  ;; Hand-written Wasm with a real i32.load decodes cleanly:
  (let ((m (dec (cat *header*
                    (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                    (section 3 (u32 1) (u32 0))
                    (section 5 (u32 1) (bv #x00) (u32 1))
                    (section 10
                             (u32 1)
                             (u32 8)
                             (u32 0)
                             (bv #x41) (u32 0)
                             (bv #x28) (bv #x02) (u32 0)
                             (bv #x1A)
                             (bv #x0B))))))
    (let* ((ins (svref (wasmdown.types:wasm-func-body
                        (svref (wasmdown.types:wasm-module-funcs m) 0))
                       1))
           (imm (wasmdown.types:wasm-instr-imms ins)))
      (wasmdown.test:is (eq :i32.load (wasmdown.types:wasm-instr-op ins)))
      (wasmdown.test:is (= 2 (getf imm :align)))
      (wasmdown.test:is (= 0 (getf imm :offset)))))
  ;; Hand-written i32.add (real byte 0x6A) decodes as :i32.add:
  (let ((m (dec (cat *header*
                    (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                    (section 3 (u32 1) (u32 0))
                    (section 10
                             (u32 1)
                             (u32 8)
                             (u32 0)
                             (bv #x41) (u32 1)
                             (bv #x41) (u32 2)
                             (bv #x6A)
                             (bv #x1A)
                             (bv #x0B))))))
    (let ((add (svref (wasmdown.types:wasm-func-body
                       (svref (wasmdown.types:wasm-module-funcs m) 0))
                      2)))
      (wasmdown.test:is (eq :i32.add (wasmdown.types:wasm-instr-op add))))))

(wasmdown.test:test binary/spec-alignment-f64-byte-order
  ;; D2 fixed: the decoder reads the f64 high word little-endian. A
  ;; real f64.const 1.0 (#x3FF0000000000000, LE bytes 00..00 F0 3F)
  ;; decodes to exactly those bits.
  (let ((m (dec (cat *header*
                    (section 1 (u32 1) (bv #x60) (u32 0) (u32 0))
                    (section 3 (u32 1) (u32 0))
                    (section 10
                             (u32 1)
                             (u32 12)
                             (u32 0)
                             (bv #x44)
                             (bv #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #x3F)
                             (bv #x1A)
                             (bv #x0B))))))
    (let ((bits (getf (wasmdown.types:wasm-instr-imms
                       (svref (wasmdown.types:wasm-func-body
                               (svref (wasmdown.types:wasm-module-funcs m) 0))
                              0))
                      :bits)))
      (wasmdown.test:is (= #x3FF0000000000000 bits))))
  ;; f64.const 1.0 round-trips through encode -> decode:
  (assert-roundtrip (mmod :types (vector (ft nil nil))
                          :funcs (vector (fn 0 nil
                                          (vector (i :f64.const (list :bits #x3FF0000000000000))
                                                  (i :drop)
                                                  (i :end)))))))

(wasmdown.test:test binary/spec-alignment-locals-order
  ;; D3 fixed: the encoder emits locals in spec order (u32 count then
  ;; valtype) and the decoder reads the same order, so modules with
  ;; locals round-trip exactly.
  (let* ((m (mmod :types (vector (ft nil nil))
                  :funcs (vector (fn 0 (list (cons :i32 2) (cons :f64 1))
                                  (vector (i :end))))))
         (e1 (wasmdown.binary:encode-module m))
         (d (dec e1))
         (e2 (wasmdown.binary:encode-module d)))
    (wasmdown.test:is (equalp e1 e2))
    (let ((f (svref (wasmdown.types:wasm-module-funcs d) 0)))
      (wasmdown.test:is (equal (list (cons :i32 2) (cons :f64 1))
                               (wasmdown.types:wasm-func-locals f))))))

(wasmdown.test:test binary/spec-alignment-custom-section
  ;; D4 fixed: the encoder emits the u32 name-length prefix in custom
  ;; sections, so encode -> decode of a custom section round-trips.
  (let* ((m (mmod :customs
                  (vector (wasmdown.types:make-wasm-custom
                           :name "hi"
                           :bytes (make-array 1 :element-type
                                              '(unsigned-byte 8)
                                              :initial-contents #(1))
                           :offset 0))))
         (e (wasmdown.binary:encode-module m))
         (d (dec e))
         (c (svref (wasmdown.types:wasm-module-customs d) 0)))
    (wasmdown.test:is (equal "hi" (wasmdown.types:wasm-custom-name c)))
    (wasmdown.test:is (equalp #(1) (wasmdown.types:wasm-custom-bytes c))))
  ;; and the canonical-bytes round trip holds:
  (assert-roundtrip (mmod :customs
                          (vector (wasmdown.types:make-wasm-custom
                                   :name "hi"
                                   :bytes (make-array 1 :element-type
                                                      '(unsigned-byte 8)
                                                      :initial-contents #(1))
                                   :offset 0)))))

(wasmdown.test:test binary/spec-alignment-select-untyped
  ;; D5 fixed: untyped :select encodes to the real byte 0x1B, which
  ;; the decoder accepts (empty :types), so it round-trips.
  (assert-roundtrip (mmod :types (vector (ft nil nil))
                          :funcs (vector (fn 0 nil
                                          (vector (i :i32.const (list :value 1))
                                                  (i :i32.const (list :value 2))
                                                  (i :i32.const (list :value 0))
                                                  (i :select)
                                                  (i :drop)
                                                  (i :end)))))))

(wasmdown.test:test binary/flags1-elemkind-byte-emitted
  ;; Regression (loop 15): the elem decoder skipped the ELEMKIND byte
  ;; for flags 1/2/3 (only flag 0 omits it per decode.ml) and the
  ;; encoder had the matching bug — consistent round-trips masked
  ;; both; binary-leb128's section-size asserts caught it. A flags-1
  ;; passive funcref segment round-trips WITH the 0x00 elemkind
  ;; consumed/emitted exactly once.
  (assert-roundtrip
   (mmod :types (vector (ft nil nil))
         :tables (vector (tab :funcref))
         :funcs (vector (fn 0 nil (vector (i :end))))
         :elems (vector (wasmdown.types:make-wasm-elem
                         '(:passive) :funcref
                         (list (ref-func-expr 0)))))))

;;; ================================================================
;;; Threads proposal (opt-in): shared limits flag (0x02) and the 0xFE
;;; atomic instruction family. Default behavior stays the core
;;; rejection; WASMDOWN:*THREADS* opts in.

(defun shared-memory-bytes ()
  ;; memory section (id 5, size 4): count 1, flags 0x03 (has-max +
  ;; shared), min 1, max 1.
  (cat *header* (bv 5 4 1 3 1 1)))

(defun shared-table-bytes ()
  ;; table section (id 4, size 4): count 1, funcref, flags 0x02
  ;; (shared), min 0.
  (cat *header* (bv 4 4 1 #x70 2 0)))

(wasmdown.test:test binary/shared-flag-default-rejected
  (let ((wasmdown:*threads* nil))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (dec (shared-memory-bytes)))))

(wasmdown.test:test binary/shared-flag-decodes-with-threads
  (let* ((wasmdown:*threads* t)
         (m (dec (shared-memory-bytes)))
         (mem (svref (wasmdown.types:wasm-module-mems m) 0))
         (lim (wasmdown.types:wasm-memory-limits mem)))
    (wasmdown.test:is (wasmdown.types:wasm-limits-shared-p lim))
    (wasmdown.test:is (wasmdown.types:wasm-limits-has-max-p lim))
    (wasmdown.test:is (wasmdown.types:wasm-limits-max lim))
    ;; canonical re-encoding keeps the shared bit
    (let ((bytes (wasmdown.binary:encode-module m)))
      (wasmdown.test:is (equalp bytes (shared-memory-bytes))))))

(wasmdown.test:test binary/shared-table-rejected
  (let ((wasmdown:*threads* t))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (dec (shared-table-bytes)))))

(wasmdown.test:test binary/atomic-op-encode-decode-roundtrip
  (let ((wasmdown:*threads* t))
    (let* ((text "(module (memory 1 1 shared)
        (func (export \"f\") (param i32) (result i32)
          (i32.atomic.rmw.add align=2 (local.get 0) (i32.const 1)))
        (func (drop (memory.atomic.notify (i32.const 0) (i32.const 1)))
              (drop (memory.atomic.wait32 (i32.const 0) (i32.const 0) (i64.const -1)))
              (atomic.fence)))")
           (m (wasmdown.text:parse-wat text))
           (bytes (wasmdown.binary:encode-module m))
           (m2 (wasmdown.binary:decode-module bytes))
           (ops (loop for f across (wasmdown.types:wasm-module-funcs m2)
                      append (loop for ins across (wasmdown.types:wasm-func-body f)
                                   collect (wasmdown.types:wasm-instr-op ins)))))
      (dolist (op '(:i32.atomic.rmw.add :memory.atomic.notify
                    :memory.atomic.wait32 :atomic.fence))
        (wasmdown.test:is (member op ops)))
      ;; round-trip is byte-stable
      (wasmdown.test:is (equalp bytes
                                (wasmdown.binary:encode-module m2))))))
