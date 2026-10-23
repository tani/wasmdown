;;;; WASMDOWN — .wat text format parser (PLAN.md §3.3: the core spec
;;;; defines a text format; this is the M0 development/conformance
;;;; frontend). Package WASMDOWN.TEXT.
;;;;
;;;; parse-wat (string) -> wasmdown.types:wasm-module, producing exactly
;;;; the IR of src/binary.lisp's decoder (CONTRACTS.md is binding):
;;;; - ops are the hyphenated/dotted keywords of the binary slice,
;;;;   case-insensitive in the source text,
;;;; - block/loop/if bodies exclude the trailing :end, const expressions
;;;;   (global init, elem/data offsets and items) include it,
;;;; - immediates are the plists of the contract (:label :targets
;;;;   :default :func :local :global :table :type :value :bits :types
;;;;   :data :elem :memidx :dst :src :align :offset), blocktype in :bt.
;;;;
;;;; Phase taxonomy: malformed text signals wasmdown:decode-error (the
;;;; text grammar is decode-phase; :offset carries the character index).
;;;; Semantic-index errors (label out of scope, unknown $id) are
;;;; reported the same way — the text format is the same phase.
;;;;
;;;; APPROACH (overall): tokenize the whole input into parenthesized
;;;; s-expressions of tokens (never CL READ — PLAN.md §5.1), then fold
;;;; each module field with a name/index environment. Instruction
;;;; immediates resolve identifiers to numeric indices at parse time so
;;;; encode-module can consume the module unchanged.
;;;;
;;;; FLOAT LITERALS (documented approach, per assignment): the mapping
;;;; literal -> IEEE-754 bit pattern is computed EXACTLY, in integer
;;;; domain only — never through host SINGLE/DOUBLE-FLOAT parsing:
;;;; - decimal float: mantissa/exponent are exact bignums; the value is
;;;;   scaled into the target precision with round-to-nearest-even on
;;;;   the exact value (sticky bits recorded), then normalized like a
;;;;   hardware float unit. This matches the spec's round-to-nearest of
;;;;   the mathematically exact literal.
;;;; - hexadecimal float: value = hexdigits * 2^exp is already binary;
;;;;   shift to the target range, rounding to nearest even.
;;;; - nan / nan:0xpayload: exponent all ones, payload in the low bits.
;;;;   -nan sets the sign bit. nan:payload with an MSB-clear payload is
;;;;   an arithmetic NaN (the parser preserves it bit-exactly).

(defpackage #:wasmdown.text
  (:use #:cl #:wasmdown.types)
  (:export #:parse-wat #:parse-wat-file))

(in-package #:wasmdown.text)

;;; ---------------------------------------------------------------
;;; Tokenizer. A token is (kind . string) with kind in
;;; (:lparen :rparen :string :nat :int :float :id :keyword :atom).
;;; :string tokens carry the DECODED bytes as a Lisp string; numeric
;;; and id tokens carry the raw source text (undecoded).

(defun %text-error (pos format-control &rest args)
  (error 'wasmdown:decode-error
         :offset pos
         :message (apply #'format nil format-control args)))

(declaim (inline %digit-p %hexdigit-p))
(defun %digit-p (c) (and (char>= c #\0) (char<= c #\9)))
(defun %hexdigit-p (c)
  (or (%digit-p c)
      (and (char>= c #\a) (char<= c #\f))
      (and (char>= c #\A) (char<= c #\F))))
(declaim (inline %reserved-char-p))
(defun %reserved-char-p (c)
  "Whether C can continue a reserved token after a string's closing
quote: any idchar (atom material) or another quote. Per spec lexer.mll
reserved = (idchar | string)+, so `\"a\"\"b\"` and `\"a\"x` are ONE
reserved token, not two."
  (not (or (member c '(#\space #\tab #\newline #\return #\page #\null))
           (char= c (code-char 40)) (char= c (code-char 41))
           (char= c #\;) (char= c #\,))))

(defun %reject-string-adjacency (s start next)
  "Signal 'unknown operator' when the string (or $\"...\" id) closing
at S[NEXT-1] abuts more reserved material. START is the token's first
index (the opening quote or the $). token.wast: a string lexed alone
is fine (only ( ) whitespace , ; and comments delimit); adjacent
strings / atoms are malformed at DECODE."
  (declare (type (integer 0) start next))
  (let ((n (length s)))
    (when (and (< next n)
               (let ((c (char s next)))
                 (or (char= c #\") (%reserved-char-p c))))
      (%text-error start "unknown operator ~s..." (subseq s start (min n (+ start 10)))))))

(defun %tokenize (s)
  "Tokenize WAT text S. Returns a flat vector of (kind . text) tokens
with :lparen/:rparen balanced; strings are decoded (with escapes)."
  (let ((n (length s))
        (tokens '())
        (i 0))
    (flet ((advance (k) (incf i k))
           (emit (kind text) (push (cons kind text) tokens)))
      (loop
        (when (>= i n) (return))
        (let ((c (char s i)))
          (cond
           ;; whitespace
           ((member c '(#\space #\tab #\newline #\return #\page))
            (advance 1))
           ;; line comment ;; ... (to end of line)
           ((and (< (1+ i) n) (char= c #\;) (char= (char s (1+ i)) #\;))
            (loop while (and (< i n) (char/= (char s i) #\newline))
                  do (advance 1)))
           ;; block comment (; ... ;) with nesting
           ((and (< (1+ i) n) (char= c (code-char 40))
                 (char= (char s (1+ i)) #\;))
            (let ((start i))
              (advance 2)
              (let ((depth 1))
                (loop
                  (when (>= i n)
                    (%text-error start "unclosed block comment"))
                  (cond
                   ((and (< (1+ i) n) (char= (char s i) (code-char 40))
                         (char= (char s (1+ i)) #\;))
                    (advance 2) (incf depth))
                   ((and (< (1+ i) n) (char= (char s i) #\;)
                         (char= (char s (1+ i)) (code-char 41)))
                    (advance 2) (decf depth)
                    (when (zerop depth) (return)))
                   (t (advance 1)))))))
           ;; parens
           ((char= c (code-char 40)) (emit :lparen "(") (advance 1))
           ((char= c (code-char 41)) (emit :rparen ")") (advance 1))
           ;; string with escapes. spec lexer.mll:
           ;; reserved = (idchar | string)+ — a string ABUTTING
           ;; another string or an atom char with no delimiter is
           ;; ONE reserved token, 'unknown operator' malformed text
           ;; (token.wast 'Space required between strings and
           ;; non-parenthesis tokens': `(data "a""b")`, `(func
           ;; "a"x)`). Only ( ) whitespace and comments close a
           ;; string token.
           ((char= c #\")
            (multiple-value-bind (decoded next)
                (%read-string s i)
              (emit :string decoded)
              (%reject-string-adjacency s i next)
              (setf i next)))
           ;; reserved chars are errors in atom position
           ;; '#' is reserved (never valid); '=' only starts atoms in
           ;; the memarg immediates offset=/align=
           ((char= c #\#)
            (%text-error i "unexpected character ~c" c))
           ;; id $"..." (string-form identifier, spec lexer.mll
           ;; '$'(string as s)): the decoded, UTF-8-validated string
           ;; content IS the id name; empty -> 'empty identifier'
           ;; (id.wast assert_malformed batch). Plain $id atoms are
           ;; read by %read-atom below.
           ((and (char= c #\$)
                 (< (1+ i) n)
                 (char= (char s (1+ i)) #\"))
            (multiple-value-bind (decoded next)
                (%read-string s (1+ i))
              (let ((name (%name-string (%utf8-validate decoded (1+ i)))))
                (when (zerop (length name))
                  (%text-error i "empty identifier"))
                (emit :id (concatenate 'string "$" name))
                ;; $"l"0 / $"l"$l: the id-string's closing quote
                ;; abuts more reserved material — one reserved token,
                ;; 'unknown operator' (token.wast ::23/:26).
                (%reject-string-adjacency s i next)
                (setf i next))))
           ;; id $... / keyword op / number
           (t
            (multiple-value-bind (kind text next)
                (%read-atom s i)
              (emit kind text)
              (setf i next))))))
      (coerce (nreverse tokens) 'vector))))

(defun %read-string (s i)
  "Read a \"...\" string literal starting at S[I] = #\". Returns the
decoded BYTE VECTOR and the index past the closing quote. Per the
WAT grammar a string literal is a sequence of BYTES: \\xx escapes are
raw bytes, \\t \\n \\r \\\" \\' \\\\ map to their byte values, and \\u{hex}
appends the UTF-8 encoding of the code point (surrogates and code
points above U+10FFFF are malformed). Raw source characters above
U+007F arrive pre-encoded as their UTF-8 byte sequence by the script
reader (byte-valued characters); non-byte characters in a direct
parse-wat input are UTF-8 encoded here so both entry paths agree."
  (let ((n (length s))
        (out (make-array 16 :element-type '(unsigned-byte 8)
                         :fill-pointer 0 :adjustable t)))
    (labels ((put (b) (vector-push-extend b out))
             (put-utf8 (cp)
               (cond
                ((< cp #x80) (put cp))
                ((< cp #x800)
                 (put (logior #xC0 (ash cp -6)))
                 (put (logior #x80 (logand cp #x3F))))
                ((< cp #x10000)
                 (put (logior #xE0 (ash cp -12)))
                 (put (logior #x80 (logand (ash cp -6) #x3F)))
                 (put (logior #x80 (logand cp #x3F))))
                (t
                 (put (logior #xF0 (ash cp -18)))
                 (put (logior #x80 (logand (ash cp -12) #x3F)))
                 (put (logior #x80 (logand (ash cp -6) #x3F)))
                 (put (logior #x80 (logand cp #x3F)))))))
      (incf i)
      (loop
        (when (>= i n)
          (%text-error i "unclosed string literal"))
        (let ((c (char s i)))
          (cond
           ((char= c #\")
            (return (values (coerce out 'vector) (1+ i))))
           ((char/= c #\\)
            ;; Raw source chars >= U+0080 must be UTF-8 ENCODED —
            ;; byte-dumping them (the old < #x100 test) produces a
            ;; lone lead byte for U+0080..U+00FF (e.g. U+00C5 Å
            ;; became 0xC5, an invalid UTF-8 start; names.wast
            ;; module-3's 480-fail cascade). Only chars < U+0080
            ;; are their own byte.
            ;;
            ;; Raw CONTROL characters (< U+0020, or U+007F) are not
            ;; string characters per the WAT grammar (only \\t \\n
            ;; \\r escapes produce them): 'illegal control character
            ;; in string literal' malformed text (id.wast
            ;; assert_malformed "(func $\"a\\nb\")" — the quote payload
            ;; carries a raw newline).
            (cond
             ((or (< (char-code c) #x20) (= (char-code c) #x7F))
              (%text-error i "illegal control character in string literal"))
             ((< (char-code c) #x80) (put (char-code c)))
             (t (put-utf8 (char-code c))))
            (incf i))
           (t
            (incf i)
            (when (>= i n)
              (%text-error i "unclosed string escape"))
            (let ((e (char s i)))
              (cond
               ((char= e #\t) (put 9) (incf i))
               ((char= e #\n) (put 10) (incf i))
               ((char= e #\r) (put 13) (incf i))
               ((char= e #\") (put 34) (incf i))
               ((char= e #\') (put 39) (incf i))
               ((char= e #\\) (put 92) (incf i))
               ((char= e #\u)
                ;; \u{hex...}: 1..6 hex digits, then the UTF-8 bytes
                (let ((j (+ i 2)))
                  (unless (and (< j n) (char= (char s (1+ i)) #\{))
                    (%text-error i "bad \\u escape"))
                  (loop
                    (unless (< j n) (%text-error j "unclosed \\u escape"))
                    (let ((u (char s j)))
                      (cond ((char= u #\}) (incf j) (return))
                            ((%hexdigit-p u) (incf j))
                            (t (%text-error j "bad \\u escape")))))
                  (let ((hex (subseq s (+ i 2) (1- j))))
                    (when (zerop (length hex))
                      (%text-error i "empty \\u escape"))
                    (let ((cp (parse-integer hex :radix 16)))
                      (cond
                       ((and (>= cp #xD800) (<= cp #xDFFF))
                        (%text-error i "malformed UTF-8 codepoint"))
                       ((> cp #x10FFFF)
                        (%text-error i "malformed UTF-8 codepoint"))
                       (t (put-utf8 cp)))))
                  (setf i j)))
               ((%hexdigit-p e)
                (unless (and (< (1+ i) n) (%hexdigit-p (char s (1+ i))))
                  (%text-error i "bad hex escape"))
                (put (parse-integer s :start i :end (+ i 2) :radix 16))
                (incf i 2))
               (t (%text-error i "unknown string escape \\~c" e)))))))))))

;;; ---------------------------------------------------------------
;;; Names: a name is a UTF-8-encoded byte vector. Strict validation
;;; (the same rules as the binary decoder's read-name): overlong
;;; forms, surrogates, code points above U+10FFFF, bad lead and
;;; continuation bytes are decode errors. assert_malformed
;;; "malformed UTF-8 encoding" cases exercise all of them through
;;; \\xx escapes in quoted modules.

(defun %utf8-validate (bytes pos)
  "Validate BYTE VECTOR as UTF-8; signals decode-error at POS on any
malformed sequence (the same rules as the binary decoder's
read-name: overlong forms, surrogate code points, code points above
U+10FFFF, bad lead/continuation bytes, truncated sequences).
Returns the vector."
  (let ((n (length bytes))
        (i 0))
    (labels ((bad () (%text-error pos "malformed UTF-8 encoding"))
           (cont (k)
             (unless (and (< (+ i k) n)
                          (>= (aref bytes (+ i k)) #x80)
                          (<= (aref bytes (+ i k)) #xBF))
               (bad))
             (- (aref bytes (+ i k)) #x80)))
      (declare (ignorable #'bad #'cont))
      (loop while (< i n)
            for b = (aref bytes i)
            do (cond
                ((< b #x80) (incf i))
                ((and (>= b #xC2) (<= b #xDF))
                 (unless (< (+ i 1) n) (bad))
                 (cont 1) (incf i 2))
                ((and (>= b #xE0) (<= b #xEF))
                 (unless (< (+ i 2) n) (bad))
                 (let ((cp (logior (ash (- b #xE0) 12)
                                   (ash (cont 1) 6)
                                   (cont 2))))
                   (when (< cp #x800) (bad))
                   (when (and (>= cp #xD800) (<= cp #xDFFF)) (bad))
                   (incf i 3)))
                ((and (>= b #xF0) (<= b #xF4))
                 (unless (< (+ i 3) n) (bad))
                 (let ((cp (logior (ash (- b #xF0) 18)
                                   (ash (cont 1) 12)
                                   (ash (cont 2) 6)
                                   (cont 3))))
                   (when (< cp #x10000) (bad))
                   (when (> cp #x10FFFF) (bad))
                   (incf i 4)))
                (t (bad)))))
    bytes))

(defun %read-atom (s i)
  "Read an atom (id / keyword / number) starting at S[I]. Returns the
token kind, raw text and next index. Numbers are recognized so module
fields can require numeric forms; keyword classification happens later."
  (let ((n (length s))
        (start i))
    (loop while (< i n)
          for c = (char s i)
          when (or (char= c (code-char 40)) (char= c (code-char 41))
                   (char= c #\")
                   (char= c #\space) (char= c #\tab) (char= c #\newline)
                   (char= c #\return) (char= c #\;))
            do (return)
          do (incf i))
    (when (= i start)
      (%text-error start "unexpected character ~c" (char s start)))
    ;; spec lexer.mll: reserved = (idchar | string)+ — an atom
    ;; abutting a string literal with no space (e.g. `(data"a")`,
    ;; `$l"a"`) is ONE reserved token, never keyword+string
    ;; (token.wast 'Space required between strings and
    ;; non-parenthesis tokens' assert_malformed batch: 'unknown
    ;; operator'). Reject here so the downstream never sees a bare
    ;; string in an immediate position.
    (when (and (< i n) (char= (char s i) #\"))
      (%text-error i
                   "unknown operator ~a\"..." (subseq s start i)))
    (let* ((text (subseq s start i))
           (c0 (char text 0)))
      ;; spec lexer.mll: a bare '$' with no id chars / string is
      ;; 'empty identifier' malformed text (id.wast assert_malformed
      ;; "(func $)").
      (when (and (char= c0 #\$) (= (length text) 1))
        (%text-error start "empty identifier"))
      (values
       (cond ((char= c0 #\$) :id)
             (t (%atom-kind text)))
       text i))))

(defun %atom-kind (text)
  "Classify a bare atom: nat, int, float, keyword (op/field name) or
plain symbol (type names like i32 are keywords here). All numeric
shapes go through the STRICT %num-shape grammar (underscores between
digits only, digits around '.'/exponents), so malformed spellings
(1__000, 0x_1p1, 99_) classify as :keyword — const parsers then
report them as bad immediates ('unknown operator' per the suite)."
  (let ((low (string-downcase text)))
    (case (%num-shape low)
      (:decint (if (and (plusp (length low))
                        (member (char low 0) '(#\+ #\-)))
                  :int :nat))
      (:hexint (if (and (plusp (length low))
                        (member (char low 0) '(#\+ #\-)))
                  :int :nat))
      (:hexfloat :float)
      (:decfloat :float)
      (t
       ;; inf / nan / nan:0xpayload (strict payload grammar)
       (cond
        ;; signed inf/nan (+inf, -inf, +nan, -nan and signed
        ;; nan:0x payloads via %float-literal-p)
        ((or (string= low "inf") (string= low "nan")
             (and (>= (length low) 4)
                  (member (char low 0) '(#\+ #\-))
                  (or (string= (subseq low 1) "inf")
                      (string= (subseq low 1) "nan")))
             (%float-literal-p text))
         :float)
        (t :keyword))))))

(defun %nat-hex-ok (s)
  "Whether S is a nonempty hex-digit run with legal single
underscores between digits (the nan:0x payload grammar)."
  (and (plusp (length s))
       (loop with prev-digit = nil
             for c across s
             always (cond
                     ((%hexdigit-p c) (setf prev-digit t) t)
                     ((char= c #\_)
                      (prog1 prev-digit (setf prev-digit nil)))
                     (t nil)))
       (char/= (char s (1- (length s))) #\_)))

(defun %float-literal-p (text)
  "Whether TEXT is a float literal per the WAT grammar: inf, nan,
nan:0x<digits>, or a strict decimal/hex float shape. nan:canonical /
nan:arithmetic are SCRIPT-ONLY directives (assert_return results);
inside modules they are malformed (i32.wast/i64.wast/f32.wast/
f64.wast assert_malformed 'unexpected token' cases)."
  (let* ((low0 (string-downcase text))
         (low (if (and (plusp (length low0))
                       (member (char low0 0) '(#\+ #\-)))
                  (subseq low0 1)
                  low0)))
    (declare (ignorable low0))
    (cond ((or (string= low "inf") (string= low "nan")) t)
          ;; nan:0x payload: hex digits only, no underscores per the
          ;; grammar? The suite pins nan:0x7f_ffff as VALID, so
          ;; underscores are allowed like digits here.
          ((and (> (length low) 6) (string= low "nan:0x" :end1 6)
                (%nat-hex-ok (subseq low 6)))
           t)
          (t (multiple-value-bind (kind) (%num-shape low)
               (member kind '(:hexfloat :decfloat)))))))

(defun %num-shape (low)
  "Return the shape of a number token LOWERCASED: :hexint, :decint,
:hexfloat, :decfloat or :other. STRICT per the WAT grammar:

  - underscores separate DIGITS only: never leading, trailing,
    doubled, or adjacent to '.', 'e'/'p', or a sign;
  - at least one digit before '.' ('.0' is malformed), at least one
    digit after '.' when a fraction is present;
  - hex floats (0x...) may omit the fraction entirely and the p
    exponent entirely, but any '.' or 'p' present must carry digits
    around it ('0x.', '0x0p', '0x0p+' malformed);
  - decimal floats require digits around '.', and e-exponents
    require digits after the optional sign;
  - '0x' with no digits is malformed (not a number).

This strictness is pinned by float_literals.wast, int_literals.wast
and const.wast assert_malformed batches ('unknown operator')."
  (labels ((digp (c) (%digit-p c))
           (hexp (c) (%hexdigit-p c))
           ;; run of digits with single underscores between them;
           ;; returns (values okp nextpos digit-count)
           (digits (from pred)
             ;; a run of digits separated by single underscores.
             ;; OK is NIL when an underscore is leading, trailing,
             ;; doubled, or adjacent to a non-digit on either side.
             (let ((p from) (count 0) (ok t) (last-digit-p nil))
               (loop while (< p (length low))
                     for c = (char low p)
                     while (or (funcall pred c) (char= c #\_))
                     do (cond
                         ((char= c #\_)
                          (unless (and last-digit-p
                                       (< (1+ p) (length low))
                                       (funcall pred (char low (1+ p))))
                            (setf ok nil)
                            (return))
                          (setf last-digit-p nil))
                         (t (incf count) (setf last-digit-p t)))
                        do (incf p))
               (values ok p count))))
    (let* ((len (length low))
           (p (if (and (plusp len) (member (char low 0) '(#\+ #\-))) 1 0))
           (start p)
           (hex (and (<= (+ p 2) len) (char= (char low p) #\0)
                     (char= (char low (1+ p)) #\x)))
           (pred (if hex #'hexp #'digp)))
      (when hex (incf p 2))
      (multiple-value-bind (int-ok p1 int-count)
          (digits p pred)
        (unless int-ok (return-from %num-shape :other))
        (setf p p1)
        (cond
         ;; integer only
         ((and (= p len) (> int-count 0))
          (if hex :hexint :decint))
         ;; fraction and/or exponent
         ((< p len)
          (let ((frac-count 0)
                (dot-p nil))
            (when (char= (char low p) #\.)
              ;; '.' must follow at least one digit and lead >=1 digit
              (when (zerop int-count) (return-from %num-shape :other))
              (setf dot-p t)
              (incf p)
              (multiple-value-bind (f-ok p2 fc)
                  (digits p pred)
                (unless f-ok (return-from %num-shape :other))
                (setf p p2 frac-count fc)))
            (cond
             ((and (< p len)
                   (char= (char low p) (if hex #\p #\e)))
              ;; exponent: hex floats REQUIRE either digits or a dot
              ;; before p; decimals likewise; digits must follow the
              ;; optional sign.
              (when (and hex (zerop int-count) (zerop frac-count))
                (return-from %num-shape :other))
              (when (and (not hex) (zerop int-count) (zerop frac-count))
                (return-from %num-shape :other))
              (incf p)
              (when (and (< p len) (member (char low p) '(#\+ #\-)))
                (incf p))
              (multiple-value-bind (e-ok p3 ec)
                  (digits p #'digp)
                (declare (ignore e-ok))
                (when (zerop ec) (return-from %num-shape :other))
                (setf p p3)
                (if (= p len)
                    (if hex :hexfloat :decfloat)
                    :other)))
             ;; no exponent: decimal fraction only; hex needs >=1
             ;; digit on one side of the dot (both-empty rejected
             ;; above via int-count check)
             ((= p len)
              (if (and (or dot-p (> int-count 0)) (not hex))
                  :decfloat
                  (if (and hex dot-p (> (+ int-count frac-count) 0))
                      :hexfloat
                      :other)))
             (t :other))))
         (t :other))))))

;;; ---------------------------------------------------------------
;;; Token trees. A node is (:list . children) or a leaf token
;;; ((kind . text) . position). Positions are character offsets used
;;; for error reporting.

(defstruct tok kind text pos)

(defun %parse-tokens (tokens)
  "Build a tree of lists from the flat token vector. Each list element
is a TOK struct; lists are plain Lisp lists (the :lparen tok is
replaced by its children list)."
  (let ((stack (list nil)))
    (loop for (kind . text) across tokens
          for idx from 0
          do (case kind
               (:lparen (push nil stack))
               (:rparen
                (let ((children (pop stack)))
                  (when (null stack)
                    (%text-error idx "unbalanced right parenthesis"))
                  (push (cons :list (nreverse children))
                        (car stack))))
               (t (push (make-tok :kind kind :text text :pos idx)
                        (car stack)))))
    (unless (null (cdr stack))
      (%text-error 0 "unclosed parenthesis"))
    (nreverse (car stack))))

(defun tok-elem-p (x) (and (consp x) (eq (car x) :list)))

(defun %first-tok (node)
  (when (and (listp node) (consp (cdr node)) (tok-p (cadr node)))
    (cadr node)))

;;; ---------------------------------------------------------------
;;; Exact float literal -> IEEE-754 bits (never host float parsing).
;;
;;; The value of a decimal/hex literal is an exact rational. Round it
;;; to the target precision with round-to-nearest-even (the IEEE
;;; default the spec requires), then encode sign/exponent/mantissa:
;;;   normal:   1.f * 2^e with bias offset
;;;   subnormal: 0.f * 2^(1-bias) when e < 1-bias
;;;   overflow:  rounds to infinity or the max finite (RNE)
;;;   zero:      sign bit only
;;; NaN: exponent all ones; payload into the low (p-1) bits, MSB of
;;; payload forced to 1 (spec: nan:0x... is the raw payload and any
;;; payload is arithmetic when it fits; the text syntax already
;;; requires 0 < payload < 2^(p-1) for nan:0x and forces nothing, but
;;; our encoder stores what was given bit-exactly).

(defun %float-bits (text p bias)
  "TEXT is a float literal; P the total precision INCLUDING the hidden
bit (24 or 53), BIAS the exponent bias (127 or 1023). Returns the full
unsigned bit pattern (sign + exponent field + mantissa field)."
  (let* ((low (string-downcase text))
         (neg (and (plusp (length low))
                   (char= (char low 0) #\-)))
         (body (if (and (plusp (length low))
                        (member (char low 0) '(#\+ #\-)))
                   (subseq low 1) low))
         ;; field widths derived from P and BIAS
         (mant-w (- p 1))                  ; mantissa field bits
         (exp-w (integer-length (* 2 bias))) ; 8 for 127, 11 for 1023
         (sign-pos (+ mant-w exp-w))
         (exp-all-ones (- (ash 1 exp-w) 1)))
    (flet ((sign-bit () (if neg 1 0)))
      (cond
       ((string= body "inf")
        (logior (ash (sign-bit) sign-pos)
                (ash exp-all-ones mant-w)))
       ((string= body "nan")
        (logior (ash (sign-bit) sign-pos)
                (ash exp-all-ones mant-w)
                (ash 1 (- mant-w 1))))       ; canonical NaN payload
       ((and (> (length body) 5) (string= body "nan:0x" :end1 6)
             ;; strict payload grammar: hex digits with legal single
             ;; underscores (const.wast: nan:0x7f_ffff VALID, nan:1
             ;; and nan:0x0/overwide REJECTED)
             (%nat-hex-ok (subseq body 6)))
        (let ((payload (parse-integer (remove #\_ body)
                                      :start 6 :radix 16)))
          ;; WAT grammar: 0 < payload < 2^significand (the FULL
          ;; significand, not the canonical-quiet sub-range).
          (when (or (zerop payload) (>= payload (ash 1 (- p 1))))
            (%text-error 0 "NaN payload 0x~x out of range" payload))
          (logior (ash (sign-bit) sign-pos)
                  (ash exp-all-ones mant-w)
                  payload)))
       (t
        (let* ((v (%literal-value body))
               (bits (%round-and-encode v p bias)))
          ;; overflow to infinity is malformed text per the spec
          ;; text grammar: a float literal must denote a value in the
          ;; target format's range (const.wast 'constant out of range'
          ;; batch: 0x1p128 / 1e39 / 0x1.ffffffp127 for f32, and the
          ;; f64 counterparts).
          (when (%float-overflow-p v p bias)
            (%text-error 0 "constant out of range"))
          (logior (ash (sign-bit) sign-pos) bits)))))))

(defun %float-overflow-p (v p bias)
  "Whether the exact positive rational V, rounded to precision P with
exponent bias BIAS, overflows the finite range (rounds to infinity).
Overflow is re > emax after RNE, where emax is the largest normal
exponent; a value that rounds UP to 2^(emax+1) overflows too (the
rounding carry bumps re past emax — same test)."
  (and (plusp v)
       (multiple-value-bind (m re)
           (%scaled-mantissa v p bias)
         (declare (ignore m))
         (let ((emax (- (- (ash 1 (integer-length (* 2 bias))) 1) bias)))
           (> re emax)))))

(defun %round-and-encode (v p bias)
  "Encode the exact positive rational V as the low (p+bias) bits of an
IEEE-754 number (the sign bit is the caller's business). Exact
integer/rational arithmetic, round-to-nearest-even.

Model: write v = m * 2^(e-p) where m is a p-bit (or fewer) integer and
e the exponent position (2^(e-1) <= v < 2^e). If e >= emin the
mantissa m is v * 2^(p-e) rounded; if e < emin the number is
subnormal with mantissa v * 2^(p-emin) rounded (and rounding up to 2^p
is the smallest normal)."
  (if (zerop v)
      0
      (multiple-value-bind (m re)
          (%scaled-mantissa v p bias)
        (let* ((emin (- 1 bias))
               (mant-w (- p 1))             ; mantissa field width
               (exp-w (integer-length (* 2 bias)))
               (mant-max (1- (ash 1 mant-w)))
               (exp-all-ones (- (ash 1 exp-w) 1))
               ;; finite exponent range: re in [emin, emax] where the
               ;; biased field (re + bias - 1) must stay <= exp-all-ones-1
               (emax (- exp-all-ones bias)))
          (cond
           ;; overflow: rounds to infinity
           ((> re emax)
            (logior (ash exp-all-ones mant-w) mant-max))
           ;; subnormal result: exponent field 0, mantissa m
           ((< re emin) m)
           ;; normal: exponent field (re-1)+bias over the low mant-w bits
           (t (logior (ash (+ re bias -1) mant-w)
                      (logand m mant-max))))))))

(defun %scaled-mantissa (v p bias)
  "Two values: the RNE-rounded mantissa M of V and its effective
exponent RE (units of 2^(RE-P)). For normals M has p bits with the
hidden bit at 2^(P-1) and RE = e; for subnormals M has at most P-1
bits (no hidden bit) and RE = emin. A rounding carry into the next
binade is folded here (M loses its top bit, RE is bumped)."
  (let* ((emin (- 1 bias))
         (mant-w (- p 1))
         ;; subnormal iff v is strictly below the smallest normal 2^emin
         (subnormal-p (< v (expt 2 emin)))
         (e (%log2-ceil v))               ; 2^(e-1) <= v < 2^e
         ;; scale so that m = v * 2^scale is the integer mantissa
         (scale (if subnormal-p (- mant-w emin) (- p e)))
         (num (numerator v))
         (den (denominator v))
         ;; v * 2^scale = snum/sden, exactly
         (sden (if (minusp scale)
                   (* den (expt 2 (- scale)))
                   den))
         (snum (if (minusp scale)
                   num
                   (* num (expt 2 scale)))))
    (multiple-value-bind (m0 rem)
        (floor snum sden)
      (let ((m (cond
                ((> (* 2 rem) sden) (1+ m0)) ; fraction > 1/2
                ((< (* 2 rem) sden) m0)      ; fraction < 1/2
                ((oddp m0) (1+ m0))          ; tie -> even
                (t m0)))
            (limit (if subnormal-p (ash 1 mant-w) (ash 1 p))))
        (if (>= m limit)
            ;; carry into the next binade: hidden bit drops out
            (values (- m limit) (1+ (if subnormal-p emin e)))
            (values m (if subnormal-p emin e)))))))

(defun %log2-ceil (v)
  "Smallest E with V < 2^E, for positive rational V. Exact (works for
negative exponents too, via expt rather than ash)."
  (if (integerp v)
      (integer-length v)
      (let ((num (numerator v)) (den (denominator v)))
        (let ((e (integer-length num)))
          (loop while (<= (* den (expt 2 e)) num) do (incf e))
          (loop while (<= num (* den (expt 2 (- e 1)))) do (decf e))
          e))))

;;; ---------------------------------------------------------------
;;; Literal text -> exact rational value.

(defun %literal-value (body)
  "BODY is a float literal without sign/inf/nan: parse into the exact
rational value. Hex floats (0xh.hhhp±d) and decimal floats
(d.ddd[e±d], with underscores removed) are both exact."
  (let* ((clean (remove #\_ body))
         (hex-p (and (>= (length clean) 2)
                     (char= (char clean 0) #\0)
                     (or (char= (char clean 1) #\x) (char= (char clean 1) #\X)))))
    (if hex-p
        (%hex-float-value (subseq clean 2))
        (%dec-float-value clean))))

(defun %hex-float-value (s)
  "0x-prefixed body: [hex].[hex]p[±]dec -> exact rational."
  (let* ((p (position #\p s))
         (mant (if p (subseq s 0 p) s))
         (expo (if p (parse-integer s :start (1+ p)) 0))
         (dot (position #\. mant))
         (int-part (if dot (subseq mant 0 dot) mant))
         (frac-part (if dot (subseq mant (1+ dot)) ""))
         (int-v (if (zerop (length int-part))
                    0
                    (parse-integer int-part :radix 16)))
         (frac-v (if (zerop (length frac-part))
                     0
                     (parse-integer frac-part :radix 16)))
         (frac-bits (* 4 (length frac-part))))
    ;; value = (int-v * 2^frac-bits + frac-v) / 2^frac-bits * 2^expo
    (* (+ (* int-v (ash 1 frac-bits)) frac-v)
       (expt 2 (- expo frac-bits)))))

(defun %dec-float-value (s)
  "Decimal float body: d.[d][e±d] -> exact rational."
  (let* ((e (position #\e s))
         (mant (if e (subseq s 0 e) s))
         (expo (if e (parse-integer s :start (1+ e)) 0))
         (dot (position #\. mant))
         (int-part (if dot (subseq mant 0 dot) mant))
         (frac-part (if dot (subseq mant (1+ dot)) ""))
         (digits (concatenate 'string int-part frac-part))
         (int-v (if (zerop (length digits)) 0 (parse-integer digits)))
         (den (expt 10 (length frac-part))))
    ;; value = digits / 10^len(frac) * 10^expo, kept as one fraction
    (/ (* int-v (expt 10 (max 0 expo)))
       (* den (expt 10 (max 0 (- expo)))))))

;;; ---------------------------------------------------------------
;;; Parsing environment: named indices (two namespaces per the spec —
;;; one index space per entity kind) and the accumulating module.

(defstruct penv
  (types (make-hash-table :test #'equal))      ; $id -> typeidx
  (funcs (make-hash-table :test #'equal))     ; $id -> funcidx (imports first)
  (tables (make-hash-table :test #'equal))
  (mems (make-hash-table :test #'equal))
  (globals (make-hash-table :test #'equal))
  (tags (make-hash-table :test #'equal))
  (elems (make-hash-table :test #'equal))
  (datas (make-hash-table :test #'equal))
  (locals (make-hash-table :test #'equal))     ; per-func $id -> localidx
  (labels ())                                  ; innermost-first $id list
  (types-list ())       ; reversed accumulation of wasm-func-type
  (imports-list ())     ; reversed
  (funcs-list ())       ; reversed (wasm-func)
  (tables-list ())
  (mems-list ())
  (globals-list ())
  (tags-list ())        ; reversed typeidx
  (exports-list ())     ; reversed
  (elems-list ())
  (datas-list ())
  (start nil)
  (n-imported-funcs 0)
  (n-imported-tables 0)
  (n-imported-mems 0)
  (n-imported-globals 0)
  (n-imported-tags 0))

(defun %id-or-num (tok table what)
  "Resolve an index token: $id via TABLE (an equal-hash), or a plain
number. Signals decode-error for unknown ids or malformed tokens."
  (let ((text (tok-text tok)))
    (cond
     ((eq (tok-kind tok) :id)
      (or (gethash text table)
          (%text-error (tok-pos tok) "unknown ~a identifier ~a" what text)))
     ((eq (tok-kind tok) :nat)
      (%parse-nat (tok-text tok) (tok-pos tok)))
     (t (%text-error (tok-pos tok) "bad ~a index token ~a" what text)))))

(defun %parse-nat (text pos)
  "Parse an unsigned integer token (dec or 0x hex). STRICT grammar:
underscores separate digits only (never leading, trailing, doubled
or adjacent to the 0x prefix) — int_literals.wast pins 1__000,
0x_100, 0x00_, _0x100, 0xff__ffff as malformed."
  (let ((shape (%num-shape (string-downcase text))))
    (unless (or (eq shape :decint) (eq shape :hexint))
      (%text-error pos "malformed integer ~a" text)))
  (let ((clean (remove #\_ text)))
    (multiple-value-bind (v end)
        (parse-integer clean :radix (if (and (> (length clean) 1)
                                              (char= (char clean 1) #\x))
                                         16 10)
                         :start (if (and (> (length clean) 1)
                                         (char= (char clean 1) #\x))
                                    2 0)
                         :junk-allowed t)
      (unless (and v (= end (length clean)))
        (%text-error pos "malformed integer ~a" text))
      v)))

(defun %parse-int (text pos)
  "Signed integer token: optional +/-, dec or 0x hex, _ separators."
  (let ((neg (and (plusp (length text)) (char= (char text 0) #\-)))
        (body (if (and (plusp (length text))
                       (member (char text 0) '(#\+ #\-)))
                  (subseq text 1) text)))
    (let ((v (%parse-nat body pos)))
      (if neg (- v) v))))

(defun %tok-keyword (x)
  "The lowercased keyword text of a leaf token X, or nil."
  (and (tok-p x) (string-downcase (tok-text x))))

;;; ---------------------------------------------------------------
;;; Value types and typeuses.

(defun %valtype (x)
  "The valtype keyword for token X, or decode-error."
  (let ((k (%tok-keyword x)))
    (cond
     ((string= k "i32") :i32)
     ((string= k "i64") :i64)
     ((string= k "f32") :f32)
     ((string= k "f64") :f64)
     ((string= k "funcref") :funcref)
     ((string= k "externref") :externref)
     ;; EH-scope admission (ref_null.wast): exnref is the nullable
     ;; exception-reference type of Core 3.0 EH. Only the ABSOLUTE
     ;; shorthand is admitted here — typed-ref spellings ((ref $t)
     ;; etc.) remain GC scope.
     ((string= k "exnref") :exnref)
     (t (%text-error (tok-pos x) "unknown value type ~a"
                     (if (tok-p x) (tok-text x) k))))))

(defun %params (node penv)
  "Parse a (param ...) list NODE: named params register in PENV-LOCALS
(with their local index: params come first, in declaration order);
returns the param valtype list. The func parser seeds PENV-LOCALS with
the running param count and clears it between functions."
  (let ((rest (cddr node)))
    ;; (param $x i32) or (param i32 i64)
    (if (and rest (tok-p (car rest)) (eq (tok-kind (car rest)) :id))
        (if (and (= (length rest) 2) (tok-p (second rest)))
            (let ((ty (%valtype (second rest)))
                  (pname (tok-text (car rest))))
              ;; duplicate $id in the func's local namespace (params +
              ;; locals) is malformed text (func.wast ::172-174
              ;; assert_malformed 'duplicate local' batch)
              (when (gethash pname (penv-locals penv))
                (%text-error (tok-pos (car rest))
                             "duplicate local identifier ~a" pname))
              (setf (gethash pname (penv-locals penv))
                    (%next-local-index penv))
              (list ty))
            (%text-error (tok-pos (car rest))
                         "named param must be (param $id type)"))
        (mapcar #'%valtype rest))))

(defun %next-local-index (penv)
  "The next free local index (params first, then locals): the parser
tracks the running count in PENV-LOCALS under the key NIL."
  (let ((h (penv-locals penv)))
    (let ((n (or (gethash nil h) 0)))
      (setf (gethash nil h) (1+ n))
      n)))

(defun %reset-locals (penv)
  "Clear the per-function local namespace before parsing a func body."
  (clrhash (penv-locals penv)))

(defun %typeuse (items penv)
  "Parse a typeuse from ITEMS (a list of toks/lists): (type $t)?
(param)* (result)*, stopping at the first item that is not one of
those. Returns (values params results typeidx-or-nil). When an
explicit (type x) is given the typeidx is resolved; otherwise the
(param)(result) lists are returned for find-or-create at the call
site. NOTE: params register their names in PENV-LOCALS, so a caller
using %TYPEUSE speculatively (blocktype scanning) must tolerate that
(blocktypes have no named params per the grammar).
ORDERING (pinned by the suite: block.wast/loop.wast/func.wast
assert_malformed batches): (type) strictly first, then (param)*,
then (result)*. A (type) after a (param)/(result), or a (param)
after a (result), is 'unexpected token' malformed text — the loop
does not just stop at the first mismatching item, it REJECTS it."
  (let ((typeidx nil) (params '()) (results '()) (rest items)
        (stage 0))                    ; 0=type?, 1=params, 2=results
    (loop while rest
          for x = (car rest)
          for k = (and (tok-elem-p x)
                       (member (%tok-keyword (%first-tok x))
                               '("type" "param" "result")
                               :test #'string=)
                       (%tok-keyword (%first-tok x)))
          while k
          do (cond
              ((string= k "type")
               (unless (zerop stage)
                 (%text-error (tok-pos (%first-tok x))
                              "unexpected token: (type) must come first in a typeuse"))
               (when typeidx
                 (%text-error 0 "duplicate (type) in typeuse"))
               (let ((idtok (third x)))
                 (unless idtok
                   (%text-error 0 "(type) needs an index"))
                 (setf typeidx
                       (%id-or-num idtok (penv-types penv) "type")))
               (pop rest))
              ((string= k "param")
               (when (> stage 1)
                 (%text-error (tok-pos (%first-tok x))
                              "unexpected token: (param) after (result) in a typeuse"))
               (setf stage 1)
               (setf params (append params (%params x penv)))
               (pop rest))
              (t
               (setf stage 2)
               (setf results (append results
                                     (mapcar #'%valtype (cddr x))))
               (pop rest))))
    (values params results typeidx)))

(defun %find-or-add-type (params results penv)
  "Return the typeidx of the functype (params -> results), adding a new
entry when no equal one exists."
  (let ((types (penv-types-list penv)))
    (loop for i from 0
          for ft in (reverse types)
          when (and (equal (wasm-func-type-params ft) params)
                    (equal (wasm-func-type-results ft) results))
            do (return-from %find-or-add-type i)))
  (push (make-wasm-func-type params results) (penv-types-list penv))
  (1- (length (penv-types-list penv))))

(defun %resolve-typeuse (items penv)
  "Resolve a typeuse to a typeidx (explicit (type x) wins; bare
(param)/(result) are find-or-create)."
  (multiple-value-bind (params results typeidx) (%typeuse items penv)
    (cond (typeidx typeidx)
          (t (%find-or-add-type params results penv)))))

(defun %types-at (typeidx penv what pos)
  "The functype at TYPEIDX in PENV's running type list, or a
decode-error. The text parser resolves (type N) indices eagerly;
an out-of-range N must be a catchable decode-error ('unknown type',
func.wast assert_invalid ::91-112 / assert_malformed batches), never
a host TYPE-ERROR on NIL."
  (let ((ft (nth typeidx (reverse (penv-types-list penv)))))
    (unless ft
      (%text-error (or pos 0)
                   "~a: unknown type index ~a" what typeidx))
    ft))

(defun %block-typeuse (items penv)
  "Parse a BLOCKTYPE typeuse from ITEMS (the head items after the
optional label of a block/loop/if). Per the spec text grammar
(parser.mly block/block_param_body) this differs from a func typeuse:
  1. (param) items are anonymous valtype lists only — a (param $id t)
     binder is grammatically impossible ('unexpected token' malformed
     text; block.wast/func.wast assert_malformed batches).
  2. When an explicit (type x) is present AND inline (param)/(result)
     items follow, the inline functype must be syntactically EQUAL to
     the referenced functype — otherwise 'inline function type' is
     malformed (block.wast's (block (type $sig) (result i32) ...) batch;
     func-style find-or-create does NOT apply).
Returns (values params results typeidx-or-nil)."
  (let ((typeidx nil) (params '()) (results '()) (rest items)
        (stage 0))
    (loop while rest
          for x = (car rest)
          for k = (and (tok-elem-p x)
                       (member (%tok-keyword (%first-tok x))
                               '("type" "param" "result")
                               :test #'string=)
                       (%tok-keyword (%first-tok x)))
          while k
          do (cond
              ((string= k "type")
               (unless (zerop stage)
                 (%text-error (tok-pos (%first-tok x))
                              "unexpected token: (type) must come first in a typeuse"))
               (when typeidx
                 (%text-error 0 "duplicate (type) in a typeuse"))
               (let ((idtok (third x)))
                 (unless idtok
                   (%text-error 0 "(type) needs an index"))
                 (setf typeidx
                       (%id-or-num idtok (penv-types penv) "type")))
               (pop rest))
              ((string= k "param")
               (when (> stage 1)
                 (%text-error (tok-pos (%first-tok x))
                              "unexpected token: (param) after (result) in a typeuse"))
               (setf stage 1)
               ;; blocktype params are anonymous: (param $x t) is not
               ;; in the grammar — the token cannot be parsed.
               (let ((ptoks (cddr x)))
                 (when (and ptoks (tok-p (car ptoks))
                            (eq (tok-kind (car ptoks)) :id))
                   (%text-error (tok-pos (car ptoks))
                                "unexpected token: block params cannot be named"))
                 (setf params (append params
                                     (mapcar #'%valtype ptoks))))
               (pop rest))
              (t
               (setf stage 2)
               (setf results (append results
                                     (mapcar #'%valtype (cddr x))))
               (pop rest))))
    ;; (type x) + inline declarations: syntactic equality required.
    ;; An out-of-range (type N) WITHOUT inline declarations parses
    ;; fine — the module is invalid at VALIDATE ('unknown type',
    ;; func.wast assert_invalid ::91-112: the reference parser keeps
    ;; the numeric typeuse and validation reports it). WITH inline
    ;; (param)/(result) the parser must resolve the referenced
    ;; functype for the equality check, so an unresolvable index is
    ;; malformed at DECODE (func.wast ::447 assert_malformed batch) —
    ;; a catchable decode-error, never a host TYPE-ERROR on NIL.
    (when (and typeidx (or params results))
      (let ((ft (%types-at typeidx penv "typeuse" 0)))
        (unless (and (equal (wasm-func-type-params ft) params)
                     (equal (wasm-func-type-results ft) results))
          (%text-error 0
                       "inline function type does not match explicit type"))))
    (values params results typeidx)))

;;; ---------------------------------------------------------------
;;; Instructions.

;;; The full engine opcode set (binary slice). Plain = no immediates.
(defparameter *plain-ops*
  '("unreachable" "nop" "return" "drop"
    "i32.eqz" "i32.eq" "i32.ne" "i32.lt_s" "i32.lt_u" "i32.gt_s"
    "i32.gt_u" "i32.le_s" "i32.le_u" "i32.ge_s" "i32.ge_u"
    "i64.eqz" "i64.eq" "i64.ne" "i64.lt_s" "i64.lt_u" "i64.gt_s"
    "i64.gt_u" "i64.le_s" "i64.le_u" "i64.ge_s" "i64.ge_u"
    "f32.eq" "f32.ne" "f32.lt" "f32.gt" "f32.le" "f32.ge"
    "f64.eq" "f64.ne" "f64.lt" "f64.gt" "f64.le" "f64.ge"
    "i32.clz" "i32.ctz" "i32.popcnt" "i32.add" "i32.sub" "i32.mul"
    "i32.div_s" "i32.div_u" "i32.rem_s" "i32.rem_u" "i32.and"
    "i32.or" "i32.xor" "i32.shl" "i32.shr_s" "i32.shr_u" "i32.rotl"
    "i32.rotr"
    "i64.clz" "i64.ctz" "i64.popcnt" "i64.add" "i64.sub" "i64.mul"
    "i64.div_s" "i64.div_u" "i64.rem_s" "i64.rem_u" "i64.and"
    "i64.or" "i64.xor" "i64.shl" "i64.shr_s" "i64.shr_u" "i64.rotl"
    "i64.rotr"
    "f32.abs" "f32.neg" "f32.ceil" "f32.floor" "f32.trunc"
    "f32.nearest" "f32.sqrt" "f32.add" "f32.sub" "f32.mul" "f32.div"
    "f32.min" "f32.max" "f32.copysign"
    "f64.abs" "f64.neg" "f64.ceil" "f64.floor" "f64.trunc"
    "f64.nearest" "f64.sqrt" "f64.add" "f64.sub" "f64.mul" "f64.div"
    "f64.min" "f64.max" "f64.copysign"
    "i32.wrap_i64" "i32.trunc_f32_s" "i32.trunc_f32_u"
    "i32.trunc_f64_s" "i32.trunc_f64_u"
    "i64.extend_i32_s" "i64.extend_i32_u"
    "i64.trunc_f32_s" "i64.trunc_f32_u" "i64.trunc_f64_s"
    "i64.trunc_f64_u"
    "f32.convert_i32_s" "f32.convert_i32_u" "f32.convert_i64_s"
    "f32.convert_i64_u" "f32.demote_f64"
    "f64.convert_i32_s" "f64.convert_i32_u" "f64.convert_i64_s"
    "f64.convert_i64_u" "f64.promote_f32"
    "i32.reinterpret_f32" "i64.reinterpret_f64"
    "f32.reinterpret_i32" "f64.reinterpret_i64"
    "i32.extend8_s" "i32.extend16_s"
    "i64.extend8_s" "i64.extend16_s" "i64.extend32_s"
    "i32.trunc_sat_f32_s" "i32.trunc_sat_f32_u" "i32.trunc_sat_f64_s"
    "i32.trunc_sat_f64_u" "i64.trunc_sat_f32_s" "i64.trunc_sat_f32_u"
    "i64.trunc_sat_f64_s" "i64.trunc_sat_f64_u"
    "ref.is_null"))

(defun %kw (text)
  "Intern lowercase op text as the IR keyword. Multi-word control ops
use hyphens (:br-if :br-table :call-indirect :ref.is-null); every
other op keeps the text name verbatim (:i32.load8_s keeps its
underscore)."
  (intern (string-upcase
           (cond
            ((string= text "br_if") "br-if")
            ((string= text "br_table") "br-table")
            ((string= text "call_indirect") "call-indirect")
            ((string= text "return_call") "return-call")
            ((string= text "return_call_indirect")
             "return-call-indirect")
            ((string= text "ref.is_null") "ref.is-null")
            ;; EH (Core 3.0): try_table's underscore hyphenates like
            ;; the other multi-word control ops; throw_ref likewise.
            ((string= text "try_table") "try-table")
            ((string= text "throw_ref") "throw-ref")
            (t text)))
          :keyword))

(defparameter *plain-op-keywords*
  (let ((h (make-hash-table :test #'equal)))
    (dolist (s *plain-ops* h)
      ;; Intern through %kw: structured-op names hyphenate
      ;; (ref.is_null -> :ref.is-null) exactly like the folded path,
      ;; so plain and folded forms produce the same IR keyword.
      (setf (gethash s h) (%kw s)))))

(defparameter *memarg-ops*
  '(("i32.load" 4) ("i64.load" 8) ("f32.load" 4) ("f64.load" 8)
    ("i32.load8_s" 1) ("i32.load8_u" 1) ("i32.load16_s" 2)
    ("i32.load16_u" 2) ("i64.load8_s" 1) ("i64.load8_u" 1)
    ("i64.load16_s" 2) ("i64.load16_u" 2) ("i64.load32_s" 4)
    ("i64.load32_u" 4)
    ("i32.store" 4) ("i64.store" 8) ("f32.store" 4) ("f64.store" 8)
    ("i32.store8" 1) ("i32.store16" 2) ("i64.store8" 1)
    ("i64.store16" 2) ("i64.store32" 4)))

(defparameter *memarg-op-keywords*
  (let ((h (make-hash-table :test #'equal)))
    (dolist (p *memarg-ops* h)
      (setf (gethash (car p) h) (intern (string-upcase (car p)) :keyword)))))


(defun %wrap32 (v)
  "Wrap a signed integer into the unsigned 32-bit domain (i32.const)."
  (mod v #x100000000))

(defun %wrap64 (v)
  (mod v #x10000000000000000))

(defun %label-idx (tok penv)
  "Resolve a label token to a relative depth (innermost = 0)."
  (cond
   ((eq (tok-kind tok) :nat) (%parse-nat (tok-text tok) (tok-pos tok)))
   ((eq (tok-kind tok) :id)
    (let ((pos (position (tok-text tok) (penv-labels penv) :test #'equal)))
      (or pos
          (%text-error (tok-pos tok) "label ~a out of scope"
                       (tok-text tok)))))
   (t (%text-error (tok-pos tok) "bad label token")))) 


;;; ---------------------------------------------------------------
;;; Instruction sequences. Items are TOKs (opcode/immediate tokens in
;;; plain form) or :list nodes (folded form). In a plain sequence the
;;; opcode's immediate tokens follow it in the same stream, so
;;; %INSTR-LIST keeps a cursor and lets %SPLIT-IMMS consume tokens.

(defun %plain-head-p (x)
  "Whether TOK item X starts a plain (immediate-bearing) instruction."
  (and (tok-p x)
       (or (gethash (%tok-keyword x) *plain-op-keywords*)
           (gethash (%tok-keyword x) *memarg-op-keywords*)
           (member (%tok-keyword x)
                   '("i32.const" "i64.const" "f32.const" "f64.const"
                     "local.get" "local.set" "local.tee" "global.get"
                    "global.set" "call" "ref.func" "ref.null" "br"
                    "br_if" "br_table" "call_indirect" "select"
                    "return_call" "return_call_indirect"
                    "throw" "throw_ref"
                     "table.get" "table.set" "table.grow" "table.size"
                     "table.fill" "table.init" "table.copy" "elem.drop"
                     "memory.size" "memory.grow" "memory.init" "data.drop"
                     "memory.copy" "memory.fill")
                   :test #'string=))))

(defun %instr-list (items penv)
  "Parse a sequence of instruction items (toks and folded lists) into a
list of wasm-instr. Structured bodies EXCLUDE their :end (contract).
Returns (VALUES INSTRS TERMINATOR REST) where TERMINATOR is NIL (all
items consumed) or (:end . id-tok) / (:else . id-tok) when the sequence
stopped at a plain-form end/else keyword (id-tok the optional trailing
identifier, for the caller's label matching) and REST the items after
it. A stray end/else is only legal inside plain block bodies (block/
loop/if parse them); elsewhere it is a decode-error."
  (let ((out '())
        (cursor items))
    (loop while cursor
          for x = (car cursor)
          do (cond
              ;; plain-form end/else: stop and hand it to the caller
              ((and (tok-p x)
                    (member (%tok-keyword x) '("end" "else") :test #'string=))
               (let* ((kind (if (string= (%tok-keyword x) "end")
                                 :end :else))
                      (rest (cdr cursor))
                      (idtok nil))
                 (when (and rest (tok-p (car rest))
                            (eq (tok-kind (car rest)) :id))
                   (setf idtok (car rest)
                         rest (cdr rest)))
                 (return-from %instr-list
                   (values (nreverse out) (cons kind idtok) rest))))
              ;; folded instruction
              ((tok-elem-p x)
               (dolist (i (%folded x penv)) (push i out))
               (pop cursor))
              ;; plain-form structured ops: block/loop/if/try_table
              ((and (tok-p x)
                    (member (%tok-keyword x)
                            '("block" "loop" "if" "try_table")
                            :test #'string=))
               (multiple-value-bind (instr rest)
                   (%plain-structured x (cdr cursor) penv)
                 (push instr out)
                 (setf cursor rest)))
              ;; plain opcode token (immediates follow in the stream)
              (t
               (multiple-value-bind (instr rest)
                   (%split-imms (%tok-keyword x) (cdr cursor)
                                (tok-pos x) penv)
                 (push instr out)
                 (setf cursor rest)))))
    (values (nreverse out) nil nil)))

(defun %check-trailing-label (id labeltok what pos)
  "Spec: the optional trailing id after end/else must match the
block's label. ID is the block's label (string or nil); LABELTOK the
trailing id token (or nil)."
  (when labeltok
    (unless (and id (string= id (tok-text labeltok)))
      (%text-error (or pos (tok-pos labeltok))
                   "mismatching trailing label ~a after ~a"
                   (tok-text labeltok) what))))

(defun %folded (node penv)
  "Parse one folded instruction list NODE into a LIST of wasm-instr
(ordinary folded ops emit operand instructions first, then the
operator — spec folded-form semantics)."
  (let* ((head (second node))
         (k (and head (%tok-keyword head))))
    (cond
     ((string= k "block") (list (%fold-block node penv :block)))
     ((string= k "loop") (list (%fold-block node penv :loop)))
     ((string= k "if") (%fold-if node penv))
     ((string= k "try_table") (list (%fold-try-table node penv)))
     (t
      ;; immediates first, then the operand expressions (folded lists)
      (multiple-value-bind (instr operands)
          (%split-imms k (cddr node) (and head (tok-pos head)) penv)
        (append
         (loop for o in operands
               if (tok-elem-p o) nconc (%folded o penv)
               else collect (%plain-single o penv))
         (list instr)))))))

(defun %plain-single (tok penv)
  "A bare opcode TOKEN inside a folded operand position (no operands
allowed there — folded operands must themselves be folded lists)."
  (%text-error (tok-pos tok)
               "expected folded operand, found bare token ~a"
               (tok-text tok)))

;;; ---------------------------------------------------------------
;;; Exception handling (Core 3.0): catch clauses of try_table.

(defun %parse-catch-clauses (items penv)
  "Parse the leading (catch x l)/(catch_ref x l)/(catch_all l)/
(catch_all_ref l) clause items of a try_table. Returns (values
catches-vector rest). Each clause record:
  (:catch tagidx label) / (:catch-ref tagidx label)
  (:catch-all label)    / (:catch-all-ref label)
LABEL is an already-resolved RELATIVE depth."
  (let ((clauses '())
        (cursor items))
    (loop
      (unless (and cursor (tok-elem-p (car cursor))) (return))
      (let* ((node (car cursor))
             (k (%tok-keyword (%first-tok node))))
        (cond
         ((string= k "catch")
          (multiple-value-bind (tag rest)
              (%take-index (cddr node) (penv-tags penv) "tag")
            (multiple-value-bind (label rest2)
                (%take-label rest penv "catch")
              (unless (null rest2)
                (%text-error 0 "junk after catch clause"))
              (push (list :catch tag label) clauses)
              (pop cursor))))
         ((string= k "catch_ref")
          (multiple-value-bind (tag rest)
              (%take-index (cddr node) (penv-tags penv) "tag")
            (multiple-value-bind (label rest2)
                (%take-label rest penv "catch_ref")
              (unless (null rest2)
                (%text-error 0 "junk after catch_ref clause"))
              (push (list :catch-ref tag label) clauses)
              (pop cursor))))
         ((string= k "catch_all")
          (multiple-value-bind (label rest)
              (%take-label (cddr node) penv "catch_all")
            (unless (null rest)
              (%text-error 0 "junk after catch_all clause"))
            (push (list :catch-all label) clauses)
            (pop cursor)))
         ((string= k "catch_all_ref")
          (multiple-value-bind (label rest)
              (%take-label (cddr node) penv "catch_all_ref")
            (unless (null rest)
              (%text-error 0 "junk after catch_all_ref clause"))
            (push (list :catch-all-ref label) clauses)
            (pop cursor)))
         (t (return)))))
    (values (coerce (nreverse clauses) 'vector)
            (nthcdr (length clauses) items))))

(defun %fold-try-table (node penv)
  "Parse a folded (try_table $l? typeuse clause* instr*): the label is
in scope for the body AND for the catch clause label resolution (same
depth accounting as %PLAIN-STRUCTURED's plain form); folded try_table
blocks contain no plain end."
  (multiple-value-bind (id bt items)
      (%block-label-and-type node penv)
    (push (or id :unnamed) (penv-labels penv))
    (multiple-value-bind (catches rest)
        (%parse-catch-clauses items penv)
      (multiple-value-bind (body term leftover)
          (%instr-list rest penv)
        (declare (ignore leftover))
        (when term
          (%text-error 0 "unexpected ~a inside folded try_table"
                       (car term)))
        (when (penv-labels penv) (pop (penv-labels penv)))
        (make-wasm-instr :try-table
                         (list :bt bt :catches catches)
                         (coerce body 'vector)
                         0)))))

(defun %plain-structured (tok items penv)
  "Parse a PLAIN-form block/loop/if starting at opcode TOK; ITEMS are
the following items of the enclosing sequence. Handles the optional
label id, typeuse, the body up to the matching end (else for if), and
the trailing-id matching rule. Returns (values instr rest-items)."
  (let* ((k (%tok-keyword tok))
         (pos (tok-pos tok))
         (id nil)
         (cursor items))
    ;; optional label
    (when (and cursor (tok-p (car cursor))
               (eq (tok-kind (car cursor)) :id))
      (setf id (tok-text (car cursor)))
      (pop cursor))
    ;; typeuse: (type)/(param)/(result) lists off the head
    (multiple-value-bind (bt skipped)
        (%parse-block-typeuse-items cursor penv)
      (setf cursor (nthcdr skipped cursor))
      ;; Every structured instruction — named or not — occupies a label
      ;; depth: unnamed scopes push a non-string sentinel that no $id
      ;; can EQUAL, so named-label depths stay correct beneath them
      ;; (spec: label indices count ALL enclosing blocks).
      (push (or id :unnamed) (penv-labels penv))
      (flet ((pop-label () (when (penv-labels penv)
                             (pop (penv-labels penv)))))
        (cond
         ((or (string= k "block") (string= k "loop"))
          (multiple-value-bind (body term rest)
              (%instr-list cursor penv)
            (unless (eq (car term) :end)
              (%text-error pos "plain ~a must end with end" k))
            (%check-trailing-label id (cdr term) "end" pos)
            (pop-label)
            (values (make-wasm-instr
                      (if (string= k "block") :block :loop)
                      (list :bt bt)
                      (coerce body 'vector) 0)
                    rest)))
         ((string= k "try_table")
          ;; try_table $l? blocktype (catch ...)* (catch_all ...)*
          ;; body end. Catch clause labels resolve AFTER the try's own
          ;; label is pushed (spec: the handler labels are branch
          ;; targets OUTSIDE the try block, in the enclosing scope,
          ;; but label depths count the try itself — the clauses are
          ;; parsed before the body while the label is on the stack).
          (multiple-value-bind (catches rest)
              (%parse-catch-clauses cursor penv)
            (multiple-value-bind (body term rest2)
                (%instr-list rest penv)
              (unless (eq (car term) :end)
                (%text-error pos "plain try_table must end with end"))
              (%check-trailing-label id (cdr term) "end" pos)
              (pop-label)
              (values (make-wasm-instr :try-table
                                       (list :bt bt :catches catches)
                                       (coerce body 'vector) 0)
                      rest2))))
         (t                                  ; if
          (multiple-value-bind (then term rest)
              (%instr-list cursor penv)
            (cond
             ((eq (car term) :end)
              ;; no else branch
              (%check-trailing-label id (cdr term) "end" pos)
              (pop-label)
              (values (make-wasm-instr :if (list :bt bt)
                                       (coerce then 'vector) 0)
                      rest))
             ((eq (car term) :else)
              (%check-trailing-label id (cdr term) "else" pos)
              (multiple-value-bind (else term2 rest2)
                  (%instr-list rest penv)
                (unless (eq (car term2) :end)
                  (%text-error pos "plain if must end with end"))
                (%check-trailing-label id (cdr term2) "end" pos)
                (pop-label)
                (values (make-wasm-instr
                         :if
                         (list :bt bt
                               :else-body (coerce else 'vector))
                         (coerce then 'vector) 0)
                        rest2)))
             (t (%text-error pos "plain if must end with end"))))))))))

(defun %parse-block-typeuse-items (items penv)
  "Consume (type)/(param)/(result) items from the head of ITEMS.
Returns (values bt skipped). Blocktype grammar rules (named-param
rejection, (type x)+inline equality) are enforced by %BLOCK-TYPEUSE."
  (multiple-value-bind (params results typeidx)
      (%block-typeuse items penv)
    (let ((skipped 0)
          (scan items))
      (loop while (and scan (tok-elem-p (car scan))
                       (member (%tok-keyword (%first-tok (car scan)))
                               '("type" "param" "result")
                               :test #'string=))
            do (incf skipped) (pop scan))
      (let ((bt (cond
                 (typeidx (cons :typeidx typeidx))
                 ((and (null params) (null results)) nil)
                 ((and (null params) (= (length results) 1)) (car results))
                 (t (cons :typeidx
                          (%find-or-add-type params results penv))))))
        (values bt skipped)))))

(defun %imm-tok-p (x)
  "Whether item X is a plain token usable as an immediate index/label."
  (and (tok-p x) (member (tok-kind x) '(:id :nat :int :float :keyword))))

(defun %take-label (items penv what)
  "Consume one label token from ITEMS; returns (values label rest)."
  (unless (and items (tok-p (car items)))
    (%text-error 0 "missing ~a label" what))
  (values (%label-idx (car items) penv) (cdr items)))

(defun %take-index (items table what)
  "Consume one index token ($id or number) from ITEMS."
  (unless (and items (tok-p (car items)))
    (%text-error 0 "missing ~a index" what))
  (values (%id-or-num (car items) table what) (cdr items)))

(defun %split-imms (k rest pos penv)
  "Parse the immediates of op text K starting at items REST (which are
the items AFTER the op token: following tokens in plain form, inner
items in folded form). Returns (VALUES INSTR REMAINING) where
REMAINING are the operand items (folded lists) of a folded op, or the
rest of the plain sequence."
  (let ((op-key (%kw k))
        (bad (lambda ()
               (%text-error pos "bad or missing immediates for ~a" k))))
    (cond
     ;; plain ops: no immediates
     ((gethash k *plain-op-keywords*)
      (values (make-wasm-instr op-key nil #() 0) rest))
     ;; integer consts: :value keeps the SIGNED literal (the binary
     ;; decoder reads s32/s64; tests/binary expects e.g. -1 for -1).
     ;; STRICT: the immediate must be a well-formed integer token;
     ;; i32.const nan:* / 1x / 0x etc are 'unknown operator'
     ;; malformed text (i32.wast/i64.wast/const.wast assert_malformed).
    ((member k '("i32.const" "i64.const") :test #'string=)
     (unless (and rest (tok-p (car rest)))
       (funcall bad))
     (let* ((tok (car rest))
            (v (%parse-int (tok-text tok) (tok-pos tok))))
       ;; Range check at DECODE: i32 accepts [-2^31, 2^32-1] (the
       ;; s32/u32 encoding), i64 [-2^63, 2^64-1]; wider spellings are
       ;; 'constant out of range' malformed text (const.wast
       ;; assert_malformed batch pins both spellings).
       (unless (if (string= k "i32.const")
                  (<= -2147483648 v 4294967295)
                  (<= -9223372036854775808 v 18446744073709551615))
         (%text-error (tok-pos tok) "constant out of range"))
       (values (make-wasm-instr op-key (list :value v) #() 0)
               (cdr rest))))
     ;; float consts (exact bit patterns). STRICT: the immediate must
     ;; be a :float-classified token; f32.const nan:canonical /
     ;; nan:arithmetic / 0x / 1x are 'unknown operator' malformed text
     ;; (f32.wast/f64.wast/const.wast assert_malformed cases).
     ((member k '("f32.const" "f64.const") :test #'string=)
      (unless (and rest (tok-p (car rest)))
        (funcall bad))
      (let ((tok (car rest)))
        ;; WAT float grammar accepts integer spellings (f64.const 0
        ;; = 0.0); :nat/:int tokens are valid float immediates.
        ;; :keyword rejects nan:canonical/nan:arithmetic (script-only
        ;; directives) and other malformed spellings.
        (unless (member (tok-kind tok) '(:float :nat :int))
          (funcall bad))
        (let* ((text (tok-text tok))
               (f32-p (string= k "f32.const")))
          (values (make-wasm-instr op-key
                                   (list :bits (%float-bits text
                                                            (if f32-p 24 53)
                                                            (if f32-p 127 1023)))
                                   #() 0)
                  (cdr rest)))))
     ;; memarg ops: optional leading memory index ($id or number —
     ;; multi-memory, Core 3.0), then offset=/align= (both optional;
     ;; align a power of 2)
     ((gethash k *memarg-op-keywords*)
      (let ((offset 0) (align-hint nil) (items rest) (memidx 0))
        ;; optional memory index: a bare $id/nat BEFORE any memarg
        ;; keyword (instance.wast's (i32.store $mem1 ...) form).
        ;; offset=/align= are :keyword tokens, so an :id/:nat here is
        ;; unambiguous.
        (when (and items (tok-p (car items))
                   (member (tok-kind (car items)) '(:id :nat)))
          (setf memidx (%id-or-num (car items) (penv-mems penv) "memory")
                items (cdr items)))
        (loop while (and items (tok-p (car items))
                         (eq (tok-kind (car items)) :keyword))
              for tok = (car items)
              for txt = (tok-text tok)
              when (and (> (length txt) 7) (string= txt "offset=" :end1 7))
                do (setf offset (%parse-nat (subseq txt 7) (tok-pos tok))
                         items (cdr items))
              else when (and (> (length txt) 6) (string= txt "align=" :end1 6))
                do (let ((a (%parse-nat (subseq txt 6) (tok-pos tok))))
                     (unless (and (plusp a) (= 1 (logcount a)))
                       (%text-error (tok-pos tok)
                                    "alignment ~a is not a power of 2" a))
                     (setf align-hint a
                           items (cdr items)))
              else
                do (return))
        (let* ((width (second (assoc k *memarg-ops* :test #'string=)))
               (align (or align-hint width)))
          (values (make-wasm-instr
                   op-key
                   (list :align (1- (integer-length align))
                         :offset offset :memidx memidx)
                   #() 0)
                  items))))
     ;; br / br-if: one label
     ((member k '("br" "br_if") :test #'string=)
      (multiple-value-bind (label items) (%take-label rest penv k)
        (values (make-wasm-instr op-key (list :label label) #() 0) items)))
     ;; br_table: label* default — in FOLDED form the trailing items
     ;; are operand lists and the labels are the leading tokens; the
     ;; LAST immediate label is the default, the trailing folded
     ;; list is the index operand.
     ((string= k "br_table")
      (let ((labels '()) (items rest))
        ;; consume leading immediate label tokens
        (loop while (and items (tok-p (car items))
                         (member (tok-kind (car items)) '(:id :nat)))
              do (push (car items) labels)
                 (pop items))
        (when (null labels)
          (%text-error pos "br_table needs at least a default label"))
        (let ((toks (nreverse labels)))
          (values (make-wasm-instr
                   op-key
                   (list :targets
                         (coerce (mapcar (lambda (l) (%label-idx l penv))
                                         (butlast toks))
                                 'vector)
                         :default (%label-idx (car (last toks)) penv))
                   #() 0)
                  items))))
     ;; call / return_call / ref.func: one funcidx
     ((member k '("call" "return_call" "ref.func") :test #'string=)
      (multiple-value-bind (f items)
          (%take-index rest (penv-funcs penv) "func")
        (values (make-wasm-instr op-key (list :func f) #() 0) items)))
     ;; throw: one tagidx (EH, Core 3.0)
     ((string= k "throw")
      (multiple-value-bind (x items)
          (%take-index rest (penv-tags penv) "tag")
        (values (make-wasm-instr op-key (list :tag x) #() 0) items)))
     ;; local/global index ops
     ((member k '("local.get" "local.set" "local.tee") :test #'string=)
      (multiple-value-bind (x items)
          (%take-index rest (penv-locals penv) "local")
        (values (make-wasm-instr op-key (list :local x) #() 0) items)))
     ((member k '("global.get" "global.set") :test #'string=)
      (multiple-value-bind (x items)
          (%take-index rest (penv-globals penv) "global")
        (values (make-wasm-instr op-key (list :global x) #() 0) items)))
     ;; table ops with optional table index (default 0)
     ((member k '("table.get" "table.set" "table.grow" "table.size"
                  "table.fill") :test #'string=)
      (let ((table 0) (items rest))
        (when (and items (tok-p (car items))
                   (member (tok-kind (car items)) '(:id :nat)))
          (setf table (%id-or-num (car items) (penv-tables penv) "table")
                items (cdr items)))
        (values (make-wasm-instr op-key (list :table table) #() 0) items)))
     ;; table.init: optional tableidx then elemidx (text order is
     ;; table-first: table.init $t $e; the one-index abbreviation is
     ;; elemidx with table 0)
     ((string= k "table.init")
      (let ((table 0) (elem 0) (items rest) (one nil))
        ;; first optional index token: tableidx if a second index
        ;; token follows, else elemidx (the one-index abbreviation
        ;; table.init x ≡ table.init 0 x)
        (when (and items (tok-p (car items))
                   (member (tok-kind (car items)) '(:id :nat)))
          (setf one (car items) items (cdr items))
          (when (and items (tok-p (car items))
                     (member (tok-kind (car items)) '(:id :nat)))
            (setf table (%id-or-num one (penv-tables penv) "table")
                  elem (%id-or-num (car items) (penv-elems penv) "elem")
                  one nil
                  items (cdr items)))
        (when one
          (setf elem (%id-or-num one (penv-elems penv) "elem")))
        (values (make-wasm-instr op-key
                                 (list :elem elem :table table) #() 0)
                items))))
     ;; table.copy: two optional table indices (dst src)
     ((string= k "table.copy")
      (let ((dst 0) (src 0) (items rest))
        (when (and items (tok-p (car items))
                   (member (tok-kind (car items)) '(:id :nat)))
          (setf dst (%id-or-num (car items) (penv-tables penv) "table")
                items (cdr items))
          (when (and items (tok-p (car items))
                     (member (tok-kind (car items)) '(:id :nat)))
            (setf src (%id-or-num (car items) (penv-tables penv) "table")
                  items (cdr items))))
        (values (make-wasm-instr op-key (list :dst dst :src src) #() 0)
                items)))
     ((string= k "elem.drop")
      (multiple-value-bind (x items)
          (%take-index rest (penv-elems penv) "elem")
        (values (make-wasm-instr op-key (list :elem x) #() 0) items)))
     ;; ref.null func|extern
     ((string= k "ref.null")
      (unless (and rest (tok-p (car rest)))
        (funcall bad))
      (let ((ht (%tok-keyword (car rest))))
        (values (make-wasm-instr op-key
                                 (list :type (cond
                                              ((string= ht "func") :funcref)
                                              ((string= ht "extern") :externref)
                                              ;; EH (ref_null.wast): (ref.null exn)
                                              ((string= ht "exn") :exnref)
                                              (t (%text-error
                                                  (tok-pos (car rest))
                                                  "bad ref.null type ~a"
                                                  (tok-text (car rest))))))
                                 #() 0)
                (cdr rest))))
     ;; call_indirect / return_call_indirect: optional table index then
     ;; typeuse. The typeuse follows BLOCKTYPE semantics (func.wast
     ;; ::108-111, call_indirect ::143-146): an explicit (type x) plus
     ;; inline (param)/(result) items must be syntactically EQUAL to
     ;; the referenced functype ('inline function type' malformed at
     ;; DECODE); named params in the inline items are 'unexpected
     ;; token' (blocktype grammar).
     ((member k '("call_indirect" "return_call_indirect") :test #'string=)
      (let ((table 0) (items rest))
        (when (and items (tok-p (car items))
                   (member (tok-kind (car items)) '(:id :nat)))
          (setf table (%id-or-num (car items) (penv-tables penv) "table")
                items (cdr items)))
        (multiple-value-bind (params results typeidx)
            (%block-typeuse items penv)
          (let ((tail nil))
            ;; the typeuse consumed only (type)/(param)/(result) items;
            ;; everything after it is operands (folded) / rest of the
            ;; plain sequence. %block-typeuse does not report how many
            ;; items it consumed, so re-scan the typeuse prefix.
            (setf tail items)
            (loop while (and tail (tok-elem-p (car tail))
                             (member (%tok-keyword (%first-tok (car tail)))
                                     '("type" "param" "result")
                                     :test #'string=))
                  do (pop tail))
            (values (make-wasm-instr op-key
                                     (list :type (or typeidx
                                                     (%find-or-add-type
                                                      params results penv))
                                           :table table)
                                     #() 0)
                    tail)))))
     ;; select: (result t)* immediates
     ((string= k "select")
      (let ((types '()) (items rest))
        (loop while (and items (tok-elem-p (car items))
                         (string= (%tok-keyword (%first-tok (car items)))
                                  "result"))
              do (setf types (append types
                                    (mapcar #'%valtype (cddr (car items))))
                     items (cdr items)))
        (values (make-wasm-instr op-key
                                 (list :types (and types (coerce types 'vector)))
                                 #() 0)
                items)))
     ;; memory ops (single-memory slice: memidx 0)
     ((member k '("memory.size" "memory.grow" "memory.fill") :test #'string=)
      (values (make-wasm-instr op-key (list :memidx 0) #() 0) rest))
     ((string= k "memory.copy")
      (values (make-wasm-instr op-key (list :dst 0 :src 0) #() 0) rest))
     ((string= k "memory.init")
      (multiple-value-bind (x items)
          (%take-index rest (penv-datas penv) "data")
        (values (make-wasm-instr op-key (list :data x :memidx 0) #() 0)
                items)))
     ((string= k "data.drop")
      (multiple-value-bind (x items)
          (%take-index rest (penv-datas penv) "data")
        (values (make-wasm-instr op-key (list :data x) #() 0) items)))
     (t (funcall bad)))))

(defun %block-label-and-type (node penv)
  "Common prefix parsing for a folded (block|loop|if [$id]? typeuse ...)
list NODE: returns (values id bt body-items). Does NOT push the label
(the caller does, so it can pop it on error paths)."
  (let* ((rest (cddr node))
         (id nil)
         (items rest))
    (when (and items (tok-p (car items)) (eq (tok-kind (car items)) :id))
      (setf id (tok-text (car items)))
      (pop items))
    (multiple-value-bind (params results typeidx)
        (%block-typeuse items penv)
      (let ((skipped 0)
            (scan items))
        (loop while (and scan (tok-elem-p (car scan))
                         (member (%tok-keyword (%first-tok (car scan)))
                                 '("type" "param" "result")
                                 :test #'string=))
              do (incf skipped) (pop scan))
        (let ((body-items (nthcdr skipped items))
              (bt (cond
                 (typeidx (cons :typeidx typeidx))
                 ((and (null params) (null results)) nil)
                 ((and (null params) (= (length results) 1)) (car results))
                 (t (cons :typeidx
                          (%find-or-add-type params results penv))))))
          (values id bt body-items))))))

(defun %fold-block (node penv op)
  "Parse a folded (block ...)/(loop ...) list: the label is in scope
for the body; folded blocks contain no plain end (their own closing
parenthesis closes them)."
  (multiple-value-bind (id bt items)
      (%block-label-and-type node penv)
    (push (or id :unnamed) (penv-labels penv))
    (multiple-value-bind (body term rest)
        (%instr-list items penv)
      (declare (ignore rest))
      (when term
        (%text-error 0 "unexpected ~a inside folded block" (car term)))
    (when (penv-labels penv) (pop (penv-labels penv)))
      (make-wasm-instr op (list :bt bt) (coerce body 'vector) 0))))

(defun %fold-if (node penv)
  "Parse a folded (if ...): optional label + typeuse, then zero or
more CONDITION operand expressions (folded lists), then (then ...) and
an optional (else ...). Condition instructions are emitted BEFORE the
:if (folded-if semantics: the if consumes its condition from the
operand stack)."
  (multiple-value-bind (id bt items)
      (%block-label-and-type node penv)
    (push (or id :unnamed) (penv-labels penv))
    ;; condition operands: folded lists before (then ...)
    (let ((conds '()))
      (loop while (and items (tok-elem-p (car items))
                       (not (member (%tok-keyword (%first-tok (car items)))
                                    '("then" "else") :test #'string=)))
            do (setf conds (nconc conds (%folded (car items) penv))
                   items (cdr items)))
      (let ((then '()) (else '()) (else-p nil))
        (dolist (x items)
          (cond
           ((and (tok-elem-p x)
                 (string= (%tok-keyword (%first-tok x)) "then"))
            (multiple-value-bind (b term rest)
                (%instr-list (cddr x) penv)
              (declare (ignore rest))
              (when term
                (%text-error 0 "unexpected ~a in folded then" (car term)))
              (setf then b)))
           ((and (tok-elem-p x)
                 (string= (%tok-keyword (%first-tok x)) "else"))
            (setf else-p t)
            (multiple-value-bind (b term rest)
                (%instr-list (cddr x) penv)
              (declare (ignore rest))
              (when term
                (%text-error 0 "unexpected ~a in folded else" (car term)))
              (setf else b)))
           (t (%text-error (if (tok-p x) (tok-pos x) 0)
                           "bad if item"))))
        (when (penv-labels penv) (pop (penv-labels penv)))
        (append conds
                (list (make-wasm-instr
                       :if
                       (if else-p
                           (list :bt bt :else-body (coerce else 'vector))
                           (list :bt bt))
                       (coerce then 'vector)
                       0)))))))

;;; ---------------------------------------------------------------
;;; Module fields. Every field parser takes its :list node (including
;;; the leading keyword token) plus the shared PENV, registers ids,
;;; and appends to the PENV's accumulation lists.

(defun %register-id (id table what pos idx)
  "Register $ID -> IDX in TABLE. The index-space pre-scan
(%prescan-ids) already registered every top-level id with its FINAL
index, so a pre-registered id is kept as-is; IDX (the caller's true
list position, never a hash count) is used only for ids the prescan
could not know."
  (declare (ignore what pos))
  (when id
    (multiple-value-bind (present value) (gethash (tok-text id) table)
      (declare (ignore present))
      (unless value
        (setf (gethash (tok-text id) table) idx)))))

(defun %string-tok (x)
  "The decoded byte vector of a :string token X, or decode-error."
  (unless (and (tok-p x) (eq (tok-kind x) :string))
    (%text-error (if (tok-p x) (tok-pos x) 0)
                 "expected a string, found ~a"
                 (if (tok-p x) (tok-text x) x)))
  (tok-text x))

(defun %name-tok (x)
  "A NAME string token as a UTF-8-validated byte vector: names must
be well-formed UTF-8 per the spec, and \\xx escapes can spell invalid
sequences which assert_malformed cases require rejected at decode."
  (%utf8-validate (%string-tok x) (if (tok-p x) (tok-pos x) 0)))

(defun %name-string (bytes)
  "The Lisp string for a validated UTF-8 name byte vector (the IR
stores strings)."
  (let ((out (make-array (max 4 (length bytes))
                         :element-type 'character
                         :fill-pointer 0 :adjustable t))
        (i 0)
        (n (length bytes)))
    (declare (type (integer 0) i))
    (loop while (< i n)
          for b = (aref bytes i)
          do (cond
              ((< b #x80) (vector-push-extend (code-char b) out) (incf i))
              ((and (>= b #xC2) (<= b #xDF))
               (vector-push-extend
                (code-char (logior (ash (- b #xC0) 6)
                                   (- (aref bytes (+ i 1)) #x80)))
                out)
               (incf i 2))
              ((and (>= b #xE0) (<= b #xEF))
               (vector-push-extend
                (code-char (logior (ash (- b #xE0) 12)
                                   (ash (- (aref bytes (+ i 1)) #x80) 6)
                                   (- (aref bytes (+ i 2)) #x80)))
                out)
               (incf i 3))
              (t
               (vector-push-extend
                (code-char (logior (ash (- b #xF0) 18)
                                   (ash (- (aref bytes (+ i 1)) #x80) 12)
                                   (ash (- (aref bytes (+ i 2)) #x80) 6)
                                   (- (aref bytes (+ i 3)) #x80)))
                out)
               (incf i 4))))
    (coerce out 'string)))

(defun %opt-id (items)
  "Split ITEMS into (values id-tok-or-nil rest-items) when the head is
an $id token."
  (if (and items (tok-p (car items)) (eq (tok-kind (car items)) :id))
      (values (car items) (cdr items))
      (values nil items)))

(defun %limits-tail (items pos what)
  "Parse n | n n limits from ITEMS: the min is a nat token; a SECOND
nat token is the max (a reftype/id after the min is NOT a max)."
  (unless (and (>= (length items) 1) (tok-p (car items)))
    (%text-error pos "~a needs limits" what))
  (let ((min (%parse-nat (tok-text (car items)) (tok-pos (car items)))))
    (if (and (cdr items) (tok-p (second items))
             (eq (tok-kind (second items)) :nat))
        (let ((max (%parse-nat (tok-text (second items))
                               (tok-pos (second items)))))
          (values (make-wasm-limits min max) (cddr items)))
        (values (make-wasm-limits min) (cdr items)))))

(defun %type-field-signature (node)
  "The (params . results) signature of an explicit (type $id? (func
...)) field node, or a decode-error when the shape is wrong. Split
out of %PARSE-TYPE-FIELD so the two-pass scheme (pass 1 registers
every explicit type before any body parses; pass 2 parses the rest)
can compute signatures without growing the penv."
  (multiple-value-bind (id rest) (%opt-id (cddr node))
    (let ((funclist (first rest)))
      (unless (and funclist (tok-elem-p funclist)
                   (string= (%tok-keyword (%first-tok funclist)) "func"))
        (%text-error (or (and id (tok-p id) (tok-pos id)) 0)
                     "(type) needs a (func ...) list"))
      (multiple-value-bind (params results typeidx)
          (%typeuse (cddr funclist) (make-penv))
        (declare (ignore typeidx))
        (values id params results)))))

(defun %prescan-types (nodes penv)
  "Pass 1 of the two-pass type scheme: parse every explicit (type ...)
field IN FIELD ORDER and append its functype to the type list,
registering $id -> typeidx. Explicit types therefore occupy indices
0..n-1 BEFORE any implicit typeuse materializes; a find-or-add MISS
in pass 2 appends after them (spec: the inline typeuse rule appends
the new functype, deduplicating against the whole current list, so a
later explicit type with an equal signature is REUSED, never
duplicated). Because the $id hash now holds explicit ids before any
body parses, forward references like (func (type $forward)) resolve
to their final index (func.wast module 1, imports.wast)."
  (dolist (node nodes)
    (let* ((head (%first-tok node))
           (k (and head (%tok-keyword head))))
      (when (and k (string= k "type"))
        (multiple-value-bind (id params results)
            (%type-field-signature node)
          (push (make-wasm-func-type params results)
                (penv-types-list penv))
          (when (and id (tok-p id) (eq (tok-kind id) :id))
            (when (gethash (tok-text id) (penv-types penv))
              (%text-error (tok-pos id) "duplicate identifier ~a"
                           (tok-text id)))
            (setf (gethash (tok-text id) (penv-types penv))
                  (1- (length (penv-types-list penv))))))))))

(defun %parse-type-field (node penv)
  "Pass-2 handler for an explicit (type ...) field: pass 1
(%PRESCAN-TYPES) already appended its functype and registered its
$id, so this only re-checks the shape (keeping duplicate-$id and
missing-(func) diagnostics at the same source positions)."
  (declare (ignore penv))
  (%type-field-signature node))

(defun %import-desc (desc penv kind)
  "Parse an import descriptor list ((func|table|memory|global|tag) $id?
type...) for KIND, returning the type payload: typeidx for funcs and
tags, wasm-table/wasm-memory/(type . mut) otherwise. Registers the id
at the kind's running import count (its true index-space position)."
  (let* ((head (%first-tok desc))
         (k (and head (%tok-keyword head)))
         (items (cddr desc)))
    (multiple-value-bind (id rest) (%opt-id items)
      (let ((idx (ecase kind
                   (:func (penv-n-imported-funcs penv))
                   (:table (penv-n-imported-tables penv))
                   (:memory (penv-n-imported-mems penv))
                   (:global (penv-n-imported-globals penv))
                   (:tag (penv-n-imported-tags penv)))))
        (when id
          (%register-id id (case kind
                             (:func (penv-funcs penv))
                             (:table (penv-tables penv))
                             (:memory (penv-mems penv))
                             (:global (penv-globals penv))
                             (:tag (penv-tags penv)))
                           (string-downcase kind) (tok-pos id) idx))
        (ecase kind
          (:func
           (setf (penv-n-imported-funcs penv) (1+ idx))
           (%resolve-typeuse rest penv))
          (:table
           (setf (penv-n-imported-tables penv) (1+ idx))
           (%import-table-type k rest (and id (tok-pos id))))
          (:memory
           (setf (penv-n-imported-mems penv) (1+ idx))
           (make-wasm-memory (%import-memory-type rest (and id (tok-pos id)))))
          (:global
           (setf (penv-n-imported-globals penv) (1+ idx))
           (%import-global-type rest (and id (tok-pos id))))
          ;; A tag's typeuse is (param)* with EMPTY results — the
          ;; grammar has no (result) in a tagtype; a stray (result)
          ;; item is junk the typeuse scanner leaves behind, and the
          ;; tag field parser rejects it (parse-time 'unexpected
          ;; token', matching the reference's malformed diagnostics).
          (:tag
           (setf (penv-n-imported-tags penv) (1+ idx))
           (%resolve-typeuse rest penv)))))))

(defun %import-table-type (k rest pos)
  "Table import type: limits (n n?) then reftype, per the text grammar
((table n n? funcref) in the official suite)."
  (declare (ignore k))
  (unless (and rest (tok-p (car rest)))
    (%text-error pos "imported table needs limits and a type"))
  (multiple-value-bind (limits tail)
      (%limits-tail rest pos "imported table")
    (let ((rt (and tail (car tail))))
      (unless (and rt (tok-p rt))
        (%text-error pos "imported table needs a reference type"))
      (let ((rty (%valtype rt)))
        (unless (member rty '(:funcref :externref))
          (%text-error pos "table type must be a reference type"))
        (make-wasm-table rty limits)))))

(defun %import-memory-type (rest pos)
  (unless (and rest (tok-p (car rest)))
    (%text-error pos "imported memory needs limits"))
  (multiple-value-bind (limits tail)
      (%limits-tail rest pos "imported memory")
    (declare (ignore tail))
    limits))

(defun %import-global-type (rest pos)
  "Global import type: valtype or (mut valtype); returns (type . mut)."
  (cond
   ((and rest (tok-p (car rest)))
    (cons (%valtype (car rest)) nil))
   ((and rest (tok-elem-p (car rest))
         (string= (%tok-keyword (%first-tok (car rest))) "mut"))
    (cons (%valtype (third (car rest))) t))
   (t (%text-error pos "imported global needs a type"))))

(defun %parse-import-field (node penv)
  "Parse an (import \"m\" \"f\" desc) field."
  (let ((module (%name-string (%name-tok (third node))))
        (field (%name-string (%name-tok (fourth node))))
        (desc (fifth node)))
    (unless (and desc (tok-elem-p desc))
      (%text-error 0 "import needs a descriptor"))
    (let* ((head (%first-tok desc))
           (k (and head (%tok-keyword head))))
      (let ((kind (cond
                   ((string= k "func") :func)
                   ((string= k "table") :table)
                   ((string= k "memory") :memory)
                   ((string= k "global") :global)
                   ((string= k "tag") :tag)
                   (t (%text-error (and head (tok-pos head))
                                   "bad import kind ~a" k)))))
        (push (make-wasm-import module field kind
                                (%import-desc desc penv kind))
              (penv-imports-list penv))))))

(defun %tag-typeuse (items penv)
  "The typeuse of a tag field: (type x)? (param)* — results are
  grammatically impossible in a tagtype. Returns (values typeidx
  leftovers); LEFTOVERS is non-empty when an unrecognized item (e.g. a
  stray (result ...) or junk) follows, which the caller reports as a
  parse error (the reference parser rejects it as 'unexpected token')."
  (multiple-value-bind (params results typeidx)
      (%typeuse items penv)
    (declare (ignorable results))
    ;; %TYPEUSE consumes (result) items silently; a tagtype must not
    ;; carry any. Re-scan to detect them explicitly.
    (let ((stray-result nil)
          (consumed 0)
          (scan items))
      (loop while (and scan (tok-elem-p (car scan))
                       (member (%tok-keyword (%first-tok (car scan)))
                               '("type" "param" "result")
                               :test #'string=))
            do (when (string= (%tok-keyword (%first-tok (car scan)))
                              "result")
                 (setf stray-result t))
               (incf consumed)
               (pop scan))
      (let ((leftovers (nthcdr consumed items)))
        (cond (stray-result
               ;; The typeuse grammar admits (result); a tag type with
               ;; non-empty results is a VALIDATION error (spec: tag
               ;; types are [t*] -> []), not decode — the inline form
               ;; registers the full type and check-tags rejects it.
               (values (or typeidx
                          (%find-or-add-type params results penv))
                       leftovers))
              (leftovers
               ;; junk after the typeuse (the loop stopped at a
               ;; non-type/param/result item): the caller decides
               (values (or typeidx
                          (%find-or-add-type params '() penv))
                       leftovers))
              (t
               (values (or typeidx
                          (%find-or-add-type params '() penv))
                       leftovers)))))))

(defun %parse-tag-field (node penv)
  "Parse a (tag ...) field (Core 3.0 exception handling):
  (tag $id? (export \"n\")* (import \"m\" \"f\")? typeuse).
  An inline (import) turns the tag into an import entry; the typeuse
  is (type x)? (param)* with EMPTY results. A defined tag appends its
  typeidx to the tag list; imports register $id at the running import
  count (via %IMPORT-DESC's :TAG arm)."
  (let* ((items (cddr node))
         (pos (or (and items (tok-p (car items)) (tok-pos (car items)))
                  0)))
    (multiple-value-bind (id rest) (%opt-id items)
      (setf rest (%inline-exports rest penv :tag (penv-tags penv)))
      (if (and rest (tok-elem-p (car rest))
               (string= (%tok-keyword (%first-tok (car rest))) "import"))
          (let* ((im (car rest))
                 (module (%name-string (%name-tok (third im))))
                 (field (%name-string (%name-tok (fourth im))))
                 (tail (cdr rest)))
            (when id
              (%register-id id (penv-tags penv) "tag" (tok-pos id)
                             (penv-n-imported-tags penv)))
            (multiple-value-bind (typeidx leftovers)
                (%tag-typeuse tail penv)
              (declare (ignore leftovers))
              (push (make-wasm-import module field :tag typeidx)
                    (penv-imports-list penv))))
          (progn
            (when id
              (%register-id id (penv-tags penv) "tag" (tok-pos id)
                             (+ (penv-n-imported-tags penv)
                                (length (penv-tags-list penv)))))
            (multiple-value-bind (typeidx leftovers)
                (%tag-typeuse rest penv)
              (unless (null leftovers)
                (%text-error pos "junk after tag type"))
              (push typeidx (penv-tags-list penv))))))))


(defun %parse-func-field (node penv)
  "Parse a (func ...) field with inline import/export abbreviations:
(func $id? (export \"n\")* (import \"m\" \"f\")? typeuse locals* body).
An inline (import) turns the func into an import entry instead."
  (let* ((items (cddr node))
         (pos (and (first items) (tok-p (first items))
                   (tok-pos (first items))))
         (id nil))
    (multiple-value-bind (i rest) (%opt-id items)
      (setf id i)
      ;; inline exports: (func (export \"n\") ...). Multiple names
      ;; export the SAME func index.
      (loop while (and rest (tok-elem-p (car rest))
                       (string= (%tok-keyword (%first-tok (car rest)))
                                "export"))
            do (let* ((ex (car rest))
                      (name (%name-string (%name-tok (third ex))))
                      (idx (+ (penv-n-imported-funcs penv)
                              (length (penv-funcs-list penv)))))
                 (declare (ignore id))
                 (push (make-wasm-export name :func idx)
                       (penv-exports-list penv))
                 (pop rest)))
      ;; inline import: becomes an import entry, no body
      (if (and rest (tok-elem-p (car rest))
               (string= (%tok-keyword (%first-tok (car rest))) "import"))
          (let* ((im (car rest))
                 (module (%name-string (%name-tok (third im))))
                 (field (%name-string (%name-tok (fourth im))))
                 (body-items (cdr rest)))
            (when id
              (%register-id id (penv-funcs penv) "func" (tok-pos id)
                             (penv-n-imported-funcs penv)))
            ;; the typeuse (and locals?) of an inline-imported func:
            ;; per the abbreviation, (type/param/result) only, no body
            (let ((typeidx (%resolve-typeuse body-items penv)))
              (incf (penv-n-imported-funcs penv))
              (push (make-wasm-import module field :func typeidx)
                    (penv-imports-list penv))))
          ;; ordinary defined func
          (progn
            ;; $id -> funcidx was pre-registered by %prescan-ids with
            ;; its FINAL index (imports + defined, field order); the
            ;; inline-export loop above used the same list-length
            ;; computation. Nothing to register here.
            (%reset-locals penv)
            (multiple-value-bind (params results typeidx)
                (%typeuse rest penv)
              ;; func.wast ::108-111: when an explicit (type x) AND
              ;; inline (param)/(result) items are both present, the
              ;; inline functype must be syntactically EQUAL to the
              ;; referenced one ('inline function type' malformed at
              ;; DECODE) — plain find-or-create does not apply.
              ;; func.wast ::91-112 (assert_invalid "unknown type"):
              ;; an out-of-range (type N) WITHOUT inline declarations
              ;; parses fine — the VALIDATOR rejects it ('unknown
              ;; type index'), matching the suite's expected phase.
              ;; WITH inline (param)/(result) the parser must resolve
              ;; the functype for the equality check, so an
              ;; unresolvable index is malformed at DECODE — a
              ;; catchable decode-error, never a host TYPE-ERROR on
              ;; NIL from the unchecked lookups below.
              (let ((type-exists-p
                      (and typeidx
                           (>= typeidx 0)
                           (< typeidx (length (penv-types-list penv))))))
                (when (and typeidx (not type-exists-p)
                           (or params results))
                  (%text-error pos "unknown type index ~a" typeidx))
                (when (and typeidx (or params results))
                  (let ((ft (%types-at typeidx penv "func" pos)))
                    (unless (and (equal (wasm-func-type-params ft) params)
                                 (equal (wasm-func-type-results ft) results))
                      (%text-error pos
                                   "inline function type does not match explicit type")))))
              (let ((skipped 0)
                    (scan rest))
                (loop while (and scan (tok-elem-p (car scan))
                                 (member (%tok-keyword (%first-tok (car scan)))
                                         '("type" "param" "result")
                                         :test #'string=))
                      do (incf skipped) (pop scan))
                (let ((body-items (nthcdr skipped rest)))
                  ;; local indices start AFTER the params: when the
                  ;; signature comes from an explicit (type x) (no
                  ;; inline (param $id ...) registration happened),
                  ;; seed the running local counter with the type's
                  ;; param count (func.wast ::94-95: a func with
                  ;; (type $sig) and (local $var i32) must map $var
                  ;; to param-count + 0). An out-of-range (type N)
                  ;; (deferred to the validator above) seeds nothing;
                  ;; the module never reaches local-index use.
                  (let ((ft (and typeidx
                                 (>= typeidx 0)
                                 (< typeidx (length (penv-types-list penv)))
                                 (nth typeidx
                                      (reverse (penv-types-list penv))))))
                    (let ((nparams (if ft
                                       (length (wasm-func-type-params ft))
                                       (length params))))
                      (declare (ignorable nparams))
                      (when (and nparams (plusp nparams))
                        (setf (gethash nil (penv-locals penv)) nparams))))
                  ;; locals: (local $id? t)* / (local t*)
                  (let ((locals '())
                        (cursor body-items))
                    (loop while (and cursor (tok-elem-p (car cursor))
                                     (string= (%tok-keyword
                                               (%first-tok (car cursor)))
                                              "local"))
                          do (let ((l (car cursor)))
                               ;; named local: (local $x i32) single type;
                               ;; unnamed: (local i32 i64 ...)
                               (let ((lrest (cddr l)))
                                 (if (and lrest (tok-p (car lrest))
                                          (eq (tok-kind (car lrest)) :id))
                                    (progn
                                      ;; duplicate $id in the func's
                                      ;; local namespace (params +
                                      ;; locals) is malformed text
                                      ;; (func.wast ::172-174
                                      ;; assert_malformed 'duplicate
                                      ;; local' batch)
                                      (let ((lname (tok-text (car lrest))))
                                        (when (gethash lname (penv-locals penv))
                                          (%text-error
                                           (tok-pos (car lrest))
                                           "duplicate local identifier ~a"
                                           lname))
                                        (setf (gethash lname (penv-locals penv))
                                              (%next-local-index penv)))
                                       (push (cons (%valtype (second lrest)) 1)
                                             locals))
                                     (dolist (t_ lrest)
                                       (push (cons (%valtype t_) 1)
                                             locals))))
                               (pop cursor)))
                    ;; remaining items are the body (folded and/or plain)
                    ;; A plain body ends with the final `end` of the func.
                    (multiple-value-bind (body term terminator-rest)
                        (%instr-list cursor penv)
                      (declare (ignore terminator-rest))
                      (when term
                        ;; a func body has no block end of its own in
                        ;; the folded style; a plain-style func body IS
                        ;; terminated by end
                        (unless (eq (car term) :end)
                          (%text-error pos "func body cannot end with ~a"
                                       (car term)))
                        ;; tolerate both: the binary body excludes end
                        (setf body body))
                      (let ((type-idx (or typeidx
                                          (%find-or-add-type params results
                                                             penv))))
                        ;; contract: func bodies INCLUDE the trailing
                        ;; :end (like the binary decoder's exprs)
                        (push (make-wasm-func
                               type-idx
                               (nreverse locals)
                               (coerce (append body
                                               (list (make-wasm-instr :end
                                                                      nil #() 0)))
                                       'vector)
                               0)
                              (penv-funcs-list penv)))))))))))))

(defun %inline-exports (rest penv kind table)
  "Consume leading (export \"n\") items from REST, pushing export
entries for the NEXT index of KIND (defined list length + imports).
Returns the remaining items."
  (loop while (and rest (tok-elem-p (car rest))
                   (string= (%tok-keyword (%first-tok (car rest)))
                            "export"))
        do (let* ((ex (car rest))
                  (name (%name-string (%name-tok (third ex))))
                  (idx (+ (ecase kind
                            (:func (penv-n-imported-funcs penv))
                            (:table (penv-n-imported-tables penv))
                            (:memory (penv-n-imported-mems penv))
                            (:global (penv-n-imported-globals penv))
                            (:tag (penv-n-imported-tags penv)))
                          (ecase kind
                            (:func (length (penv-funcs-list penv)))
                            (:table (length (penv-tables-list penv)))
                            (:memory (length (penv-mems-list penv)))
                            (:global (length (penv-globals-list penv)))
                            (:tag (length (penv-tags-list penv)))))))
             (declare (ignore table))
             (push (make-wasm-export name kind idx)
                   (penv-exports-list penv))
             (pop rest))
        finally (return rest)))

(defun %table-type-items (items pos penv)
  "Parse a defined table's type from ITEMS: reftype then limits
(n [n]), OR the inline (elem ...) abbreviation. Returns (values
wasm-table inits) where INITS is the elem segment's init list for the
inline form, or NIL."
  (cond
   ;; inline elem: (table reftype (elem item*)) — the elem list itself
   ;; is the LAST item
   ((and (>= (length items) 1)
         (tok-elem-p (car (last items)))
         (string= (%tok-keyword (%first-tok (car (last items)))) "elem"))
    (let* ((elemlist (car (last items)))
           (eitems (cddr elemlist))
           (n (length eitems)))
      (values (make-wasm-table :funcref (make-wasm-limits n))
              eitems)))
   (t
    ;; (table n n? reftype): limits first, reftype last
    (unless (and (>= (length items) 2) (tok-p (car items)))
      (%text-error pos "table needs limits and a reference type"))
    (multiple-value-bind (limits tail)
        (%limits-tail items pos "table")
      (let ((rt (and tail (car tail))))
        (unless (and rt (tok-p rt))
          (%text-error pos "table needs a reference type"))
        (let ((rty (%valtype rt)))
          (unless (member rty '(:funcref :externref))
            (%text-error pos "table type must be a reference type"))
          (if (and (cdr tail) (tok-elem-p (second tail)))
              ;; (table n n? reftype <expr>): the WAT extended table
              ;; initializer — every entry is initialized from the ONE
              ;; constant expression. Returns the table plus a THIRD
              ;; value: the pre-parsed init expr (each entry is a copy;
              ;; the caller emits the active elem segment directly).
              ;; Constness/type checks fire at VALIDATION (the shape
              ;; parses per the grammar — table.wast ::13/14,
              ;; global.wast ::113/120).
              (values (make-wasm-table rty limits)
                      nil
                      (%parse-expr (cdr tail) penv))
              (progn
                (unless (null (cdr tail))
                  (%text-error pos "junk after table type"))
                (values (make-wasm-table rty limits) nil nil)))))))))

(defun %parse-table-field (node penv)
  "Parse a (table ...) field: $id?, inline exports, optional inline
import, then the type (reftype limits) or the inline (elem)
abbreviation (which also emits the elem segment)."
  (let* ((items (cddr node))
         (pos (or (and items (tok-p (car items)) (tok-pos (car items))) 0)))
    (multiple-value-bind (id rest) (%opt-id items)
      (setf rest (%inline-exports rest penv :table (penv-tables penv)))
      (if (and rest (tok-elem-p (car rest))
               (string= (%tok-keyword (%first-tok (car rest))) "import"))
          ;; inline import abbreviation
          (let* ((im (car rest))
                 (module (%name-string (%name-tok (third im))))
                 (field (%name-string (%name-tok (fourth im))))
                 (tail (cdr rest)))
            (when id
              (%register-id id (penv-tables penv) "table" (tok-pos id)
                             (penv-n-imported-tables penv)))
            (incf (penv-n-imported-tables penv))
            (let ((ty (%import-table-type "table" tail (and id (tok-pos id)))))
              (push (make-wasm-import module field :table ty)
                    (penv-imports-list penv))))
          ;; defined table
          (progn
            (when id
              (%register-id id (penv-tables penv) "table" (tok-pos id)
                             (+ (penv-n-imported-tables penv)
                                (length (penv-tables-list penv)))))
            (multiple-value-bind (tab inits init-expr)
                (%table-type-items rest pos penv)
              (push tab (penv-tables-list penv))
              (let ((tableidx (+ (penv-n-imported-tables penv)
                                 (1- (length (penv-tables-list penv))))))
                ;; inline (elem ...): an ACTIVE elem segment targeting
                ;; this table at offset 0
                (when inits
                  (push (make-wasm-elem
                         (list :active tableidx
                               (vector (make-wasm-instr :i32.const
                                                        (list :value 0) #() 0)
                                       (make-wasm-instr :end nil #() 0)))
                         :funcref
                         (%elem-items inits :funcref penv))
                        (penv-elems-list penv)))
                ;; extended initializer expr: [expr x min] segment,
                ;; marked from-table-init so check-elems applies the
                ;; imported-globals-only rule to its items
                (when init-expr
                  (push (make-wasm-elem
                         (list :active tableidx
                               (vector (make-wasm-instr :i32.const
                                                        (list :value 0) #() 0)
                                       (make-wasm-instr :end nil #() 0)))
                         (wasm-table-type tab)
                         (loop repeat (wasm-limits-min
                                       (wasm-table-limits tab))
                               collect init-expr)
                         t)
                        (penv-elems-list penv))))))))))

(defun %parse-memory-field (node penv)
  "Parse a (memory ...) field: $id?, inline exports, optional inline
import, then limits (n [n]) or the (data ...) abbreviation."
  (let* ((items (cddr node))
         (pos (or (and items (tok-p (car items)) (tok-pos (car items))) 0)))
    (multiple-value-bind (id rest) (%opt-id items)
      (setf rest (%inline-exports rest penv :memory (penv-mems penv)))
      (if (and rest (tok-elem-p (car rest))
               (string= (%tok-keyword (%first-tok (car rest))) "import"))
          (let* ((im (car rest))
                 (module (%name-string (%name-tok (third im))))
                 (field (%name-string (%name-tok (fourth im))))
                 (tail (cdr rest)))
            (when id
              (%register-id id (penv-mems penv) "memory" (tok-pos id)
                             (penv-n-imported-mems penv)))
            (incf (penv-n-imported-mems penv))
            (let ((ty (%import-memory-type tail (and id (tok-pos id)))))
              (push (make-wasm-import module field :memory
                                      (make-wasm-memory ty))
                    (penv-imports-list penv))))
          (progn
            (when id
              (%register-id id (penv-mems penv) "memory" (tok-pos id)
                             (+ (penv-n-imported-mems penv)
                                (length (penv-mems-list penv)))))
            ;; (data "...") abbreviation: memory sized to the data
            (if (and rest (tok-elem-p (car rest))
                     (string= (%tok-keyword (%first-tok (car rest)))
                              "data"))
                (let* ((datalist (car rest))
                       (bytes (%data-bytes (cddr datalist))))
                  (let ((pages (ash (+ (length bytes) #xFFFF) -16)))
                    ;; no min-1 floor: (memory (data)) is a valid
                    ;; 0-page memory (memory.wast ::8/:10).
                    (push (make-wasm-memory
                           (make-wasm-limits pages))
                          (penv-mems-list penv))
                    ;; active data segment at offset 0
                    (let ((memidx (+ (penv-n-imported-mems penv)
                                     (1- (length (penv-mems-list penv))))))
                      (push (make-wasm-data
                             (list :active memidx
                                   (vector (make-wasm-instr :i32.const
                                                            (list :value 0) #() 0)
                                           (make-wasm-instr :end nil #() 0)))
                             bytes)
                            (penv-datas-list penv)))))
                (progn
                  (multiple-value-bind (limits tail)
                      (%limits-tail rest pos "memory")
                    (unless (null tail)
                      (%text-error pos "junk after memory limits"))
                    (push (make-wasm-memory limits)
                          (penv-mems-list penv))))))))))

(defun %global-type (item pos)
  "A global type is a valtype or a (mut valtype) list. Returns
(values type mutable-p)."
  (cond
   ((tok-p item) (values (%valtype item) nil))
   ((and (tok-elem-p item)
         (string= (%tok-keyword (%first-tok item)) "mut"))
    (unless (third item)
      (%text-error pos "(mut) needs a type"))
    (values (%valtype (third item)) t))
   (t (%text-error pos "bad global type"))))

(defun %parse-global-field (node penv)
  "Parse a (global $id? type init) field. Init is an expression (a
single folded list or a plain instruction sequence ending with end)."
  (let* ((items (cddr node))
         (pos (or (and items (tok-p (car items)) (tok-pos (car items))) 0)))
    (multiple-value-bind (id rest) (%opt-id items)
      (setf rest (%inline-exports rest penv :global (penv-globals penv)))
      (if (and rest (tok-elem-p (car rest))
               (string= (%tok-keyword (%first-tok (car rest))) "import"))
          (let* ((im (car rest))
                 (module (%name-string (%name-tok (third im))))
                 (field (%name-string (%name-tok (fourth im))))
                 (tail (cdr rest)))
            (when id
              (%register-id id (penv-globals penv) "global" (tok-pos id)
                             (penv-n-imported-globals penv)))
            (incf (penv-n-imported-globals penv))
            (multiple-value-bind (type mut) (%global-type (car tail) pos)
              (push (make-wasm-import module field :global (cons type mut))
                    (penv-imports-list penv))))
          ;; defined global: type then init expr
          (progn
            (when id
              (%register-id id (penv-globals penv) "global" (tok-pos id)
                             (+ (penv-n-imported-globals penv)
                                (length (penv-globals-list penv)))))
            (unless (and rest (or (tok-p (car rest)) (tok-elem-p (car rest))))
              (%text-error pos "global needs a type and init"))
            (multiple-value-bind (type mut) (%global-type (car rest) pos)
              (let ((expr (%parse-expr (cdr rest) penv)))
                (push (make-wasm-global :type type :mutable mut
                                         :init expr)
                      (penv-globals-list penv)))))))))

(defun %parse-expr (items penv)
  "Parse a constant expression from ITEMS. Shapes (WAT spec):
  - zero or more instruction forms — folded lists ((i32.const 0) ...),
    plain tokens (ref.func $f; an (item ...) abbreviation wraps these),
  - a plain instruction sequence terminated by end.
Either way the returned instr vector INCLUDES the trailing :end
(contract for const expressions). Multi-instruction and non-const
shapes PARSE here and fail at VALIDATION (const-expr-type reports
arity/constness); junk the instruction parser rejects is a decode
error."
  (cond
   ((and (= (length items) 1) (tok-elem-p (car items)))
    ;; single folded expression
    (let ((instrs (%folded (car items) penv)))
      (coerce (append instrs (list (make-wasm-instr :end nil #() 0)))
              'vector)))
   ((and items (every #'tok-elem-p items))
    ;; folded sequence: parse each list; no end keyword involved
    (let ((instrs (mapcar (lambda (x) (%folded x penv)) items)))
      (coerce (append (reduce #'append instrs :from-end t)
                      (list (make-wasm-instr :end nil #() 0)))
              'vector)))
   (t
    ;; plain sequence: instructions then optional end
    (multiple-value-bind (body term rest)
        (%instr-list items penv)
      (declare (ignore rest))
      (unless (or (eq (car term) :end) (null term))
        (%text-error 0 "constant expression must end with end"))
      (coerce (append body (list (make-wasm-instr :end nil #() 0)))
              'vector)))))

(defun %parse-export-field (node penv)
  "Parse an (export \"n\" (kind idx)) field."
  (let ((name (%name-string (%name-tok (third node))))
        (desc (fourth node)))
    (unless (and desc (tok-elem-p desc))
      (%text-error 0 "export needs a descriptor"))
    (let* ((head (%first-tok desc))
           (k (and head (%tok-keyword head)))
           (idxtok (third desc)))
      (let ((kind (cond
                   ((string= k "func") :func)
                   ((string= k "table") :table)
                   ((string= k "memory") :memory)
                   ((string= k "global") :global)
                   ((string= k "tag") :tag)
                   (t (%text-error (and head (tok-pos head))
                                   "bad export kind ~a" k)))))
        (let ((idx (case kind
                     (:func (%id-or-num idxtok (penv-funcs penv) "func"))
                     (:table (%id-or-num idxtok (penv-tables penv) "table"))
                     (:memory (%id-or-num idxtok (penv-mems penv) "memory"))
                     (:global (%id-or-num idxtok (penv-globals penv)
                                           "global"))
                     (:tag (%id-or-num idxtok (penv-tags penv) "tag")))))
          (push (make-wasm-export name kind idx)
                (penv-exports-list penv)))))))

(defun %parse-start-field (node penv)
  "Parse a (start $f) field."
  (let ((idxtok (third node)))
    (when (penv-start penv)
      (%text-error 0 "multiple start sections"))
    (unless idxtok
      (%text-error 0 "start needs a func index"))
    (setf (penv-start penv)
          (%id-or-num idxtok (penv-funcs penv) "func"))))

;;; ---------------------------------------------------------------
;;; Element and data segments.

(defun %data-bytes (items)
  "Concatenate the :string tokens of a data segment into a byte
vector. String literals ARE byte sequences (the tokenizer's byte
vector is taken verbatim): \\xx escapes are raw bytes and \\u{...}
escapes are already UTF-8 encoded (token.wast pins astral-plane code
points in data strings; their byte form is what lands in memory)."
  (let* ((chunks (mapcar (lambda (x) (%string-tok x)) items))
         (total (reduce #'+ (mapcar #'length chunks) :initial-value 0))
         (out (make-array (max 16 total)
                          :element-type '(unsigned-byte 8)
                          :fill-pointer 0 :adjustable t)))
    (declare (ignore total))
    (dolist (v chunks)
      (loop for b across v do (vector-push-extend b out)))
    (coerce out 'vector)))

(defun %elem-items (items rt penv)
  "Elem init items: funcidx tokens ($f/0) abbreviate (ref.func x);
folded lists are full expressions. Returns the list of init
expression vectors (each INCLUDING its :end)."
  (when (and items (tok-p (car items))
             (member (tok-kind (car items)) '(:id :nat)))
    ;; legacy funcidx list form (spec: only valid for funcref, but the
    ;; runtime treats ref types uniformly here)
    (return-from %elem-items
      (mapcar (lambda (x)
                (unless (and (tok-p x) (member (tok-kind x) '(:id :nat)))
                  (%text-error (if (tok-p x) (tok-pos x) 0)
                               "bad elem funcidx item"))
                (vector (make-wasm-instr :ref.func
                                         (list :func (%id-or-num x
                                                             (penv-funcs penv)
                                                             "func"))
                                         #() 0)
                        (make-wasm-instr :end nil #() 0)))
              items)))
  (mapcar (lambda (x)
            (unless (tok-elem-p x)
              (%text-error (if (tok-p x) (tok-pos x) 0)
                           "elem item must be an expression"))
            ;; (item e*): the spec's item wrapper around one
            ;; instruction sequence; unwrap to a folded const expr
            ;; whose arity/constness fails at VALIDATION (elem.wast
            ;; pins (item (ref.null func) (ref.null func)) etc).
            (if (and (tok-p (%first-tok x))
                     (string= (%tok-keyword (%first-tok x)) "item"))
                (%parse-expr (cddr x) penv)
                (%parse-expr (list x) penv)))
          items))

(defun %elem-offset-expr (items penv)
  "The offset of an active elem/data: an (offset expr*) wrapper, or
the single-instruction abbreviation — ANY leading folded list (strings
are :string tokens, reftypes keywords, so a leading list is always the
offset per the WAT grammar's offset abbreviation). Returns (values
expr rest). The expression's arity/constness is checked at
VALIDATION, not here."
 (cond
  ((and items (tok-elem-p (car items))
        (string= (%tok-keyword (%first-tok (car items))) "offset"))
   (values (%parse-expr (cddr (car items)) penv) (cdr items)))
  ((and items (tok-elem-p (car items)))
   (values (%parse-expr (list (car items)) penv) (cdr items)))
  (t (values nil items))))

(defun %parse-elem-field (node penv)
  "Parse an (elem ...) field, all forms:
 (elem $id? (table x)? (offset e|e) reftype? item*)   active
 (elem $id? reftype item*)                            passive
 (elem $id? declare reftype? item*)                   declarative
 (elem $id? funcidx*)                                 active on table 0
 item := funcidx | (ref.func x) | (offset ...)-free expr | (item expr)"
  (let* ((items (cddr node))
         (pos (or (and items (tok-p (car items)) (tok-pos (car items))) 0)))
    (multiple-value-bind (id rest) (%opt-id items)
      (when id
        (setf (gethash (tok-text id) (penv-elems penv))
              (length (penv-elems-list penv))))
      ;; declare form
      (cond
       ((and rest (tok-p (car rest))
             (string= (%tok-keyword (car rest)) "declare"))
        (let* ((tail (cdr rest))
               (rt :funcref)
               (tail2 tail))
          ;; optional reftype (funcref/externref) or 'func'
          (when (and tail2 (tok-p (car tail2))
                     (member (%tok-keyword (car tail2))
                             '("funcref" "externref" "func")
                             :test #'string=))
            (when (string= (%tok-keyword (car tail2)) "externref")
              (setf rt :externref))
            (setf tail2 (cdr tail2)))
          (push (make-wasm-elem '(:declarative) rt
                                (%elem-items tail2 rt penv))
                (penv-elems-list penv))))
       (t
        ;; (table x)? prefix
        (let ((tableidx 0) (tail rest) (active nil))
          (when (and tail (tok-elem-p (car tail))
                     (string= (%tok-keyword (%first-tok (car tail))) "table"))
            (setf tableidx (%id-or-num (third (car tail))
                                       (penv-tables penv) "table")
                  active t
                  tail (cdr tail)))
          ;; offset / bare expression
          (multiple-value-bind (expr tail2)
              (%elem-offset-expr tail penv)
            (if active
                ;; ACTIVE: expr required
                (let* ((rt :funcref)
                       (tail3 tail2))
                  (when (and tail3 (tok-p (car tail3))
                             (member (%tok-keyword (car tail3))
                                     '("funcref" "externref" "func")
                                     :test #'string=))
                    (when (string= (%tok-keyword (car tail3)) "externref")
                      (setf rt :externref))
                    (setf tail3 (cdr tail3)))
                  (push (make-wasm-elem (list :active tableidx expr) rt
                                        (%elem-items tail3 rt penv))
                        (penv-elems-list penv)))
                ;; no (table x): if an offset was found it is active on
                ;; table 0; else funcref/passive decision by head
                (let ((rt :funcref))
                  (cond
                   (expr
                    (let ((tail3 tail2))
                      (when (and tail3 (tok-p (car tail3))
                                 (member (%tok-keyword (car tail3))
                                         '("funcref" "externref" "func")
                                         :test #'string=))
                        (when (string= (%tok-keyword (car tail3)) "externref")
                          (setf rt :externref))
                        (setf tail3 (cdr tail3)))
                      (push (make-wasm-elem (list :active 0 expr) rt
                                            (%elem-items tail3 rt penv))
                            (penv-elems-list penv))))
                   ((and tail2 (tok-p (car tail2))
                         (member (%tok-keyword (car tail2))
                                 '("funcref" "externref") :test #'string=))
                    ;; passive with explicit reftype
                    (push (make-wasm-elem '(:passive)
                                          (%valtype (car tail2))
                                          (%elem-items (cdr tail2)
                                                      (%valtype (car tail2))
                                                      penv))
                          (penv-elems-list penv)))
                   ((and tail2 (tok-p (car tail2))
                         (string= (%tok-keyword (car tail2)) "func"))
                    ;; passive funcidx list
                    (push (make-wasm-elem '(:passive) :funcref
                                          (%elem-items (cdr tail2) :funcref
                                                       penv))
                          (penv-elems-list penv)))
                   (t
                    ;; bare funcidx list: active on table 0
                    (push (make-wasm-elem
                           (list :active 0
                                 (vector (make-wasm-instr :i32.const
                                                         (list :value 0) #() 0)
                                         (make-wasm-instr :end nil #() 0)))
                           :funcref
                           (%elem-items tail2 :funcref penv))
                          (penv-elems-list penv)))))))))))
      ;; (index space bookkeeping is by list length; nothing to sync)
      ))

(defun %parse-data-field (node penv)
  "Parse a (data ...) field:
 (data $id? (memory x)? (offset e|e) \"s\"*)  active
 (data $id? \"s\"*)                           passive"
  (let* ((items (cddr node))
         (pos (or (and items (tok-p (car items)) (tok-pos (car items))) 0)))
    (multiple-value-bind (id rest) (%opt-id items)
      (when id
        (setf (gethash (tok-text id) (penv-datas penv))
              (length (penv-datas-list penv))))
      (let ((memidx 0) (tail rest) (mem-p nil))
        (when (and tail (tok-elem-p (car tail))
                   (string= (%tok-keyword (%first-tok (car tail))) "memory"))
          (setf memidx (%id-or-num (third (car tail)) (penv-mems penv)
                                   "memory")
                mem-p t
                tail (cdr tail)))
        (multiple-value-bind (expr tail2)
            (%elem-offset-expr tail penv)
          (if (or mem-p expr)
              ;; active
              (progn
                (unless expr
                  (%text-error pos "active data segment needs an offset"))
                (push (make-wasm-data (list :active memidx expr)
                                      (%data-bytes tail2))
                      (penv-datas-list penv)))
              ;; passive
              (push (make-wasm-data '(:passive) (%data-bytes tail2))
                    (penv-datas-list penv))))))))

;;; ---------------------------------------------------------------
;;; Module assembly and entry points.
(defun %prescan-ids (nodes penv)
  "Pre-register every top-level $id -> final index for the six index
spaces BEFORE parsing field bodies, so forward references (legal WAT)
resolve. Imports count toward their kind's index space in field
order, interleaved with defined entities. Type ids stay deferred
(implicit typeuses shift the list during body parsing)."
  (let ((nfuncs 0) (ntables 0) (nmems 0) (nglobals 0) (ntags 0)
        (nelems 0) (ndatas 0))
    (flet ((reg (id table idx)
             (when (and id (tok-p id) (eq (tok-kind id) :id))
               (when (gethash (tok-text id) table)
                 (%text-error (tok-pos id) "duplicate identifier ~a"
                              (tok-text id)))
               (setf (gethash (tok-text id) table) idx)))
           (node-id (node)
             (let ((t3 (third node)))
               (and (tok-p t3) (eq (tok-kind t3) :id) t3))))
      (dolist (node nodes)
        (let* ((head (%first-tok node))
               (k (and head (%tok-keyword head))))
          (cond
           ((string= k "func")
            ;; every func (inline-import or defined) occupies the next
            ;; index in field order; inline-imports also bump the
            ;; n-imported counter the field parser uses.
            (let* ((items (cddr node))
                   (rest (multiple-value-bind (id r) (%opt-id items)
                           (declare (ignore id))
                           r)))
              (loop while (and rest (tok-elem-p (car rest))
                                (string= (%tok-keyword (%first-tok (car rest)))
                                         "export"))
                    do (pop rest))
              (reg (node-id node) (penv-funcs penv) nfuncs)
              (incf nfuncs)))
           ((string= k "import")
            ;; the descriptor is the FIFTH child: (import "m" "f" (desc ...))
            (let ((desc (fifth node)))
              (when (and desc (tok-elem-p desc))
                (let* ((dk (%tok-keyword (%first-tok desc)))
                       (did (third desc)))
                  (cond
                   ((string= dk "func")
                    (reg did (penv-funcs penv) nfuncs) (incf nfuncs))
                   ((string= dk "table")
                    (reg did (penv-tables penv) ntables) (incf ntables))
                   ((string= dk "memory")
                    (reg did (penv-mems penv) nmems) (incf nmems))
                   ((string= dk "global")
                    (reg did (penv-globals penv) nglobals)
                    (incf nglobals))
                   ((string= dk "tag")
                    (reg did (penv-tags penv) ntags)
                    (incf ntags)))))))
           ((string= k "table")
            (reg (node-id node) (penv-tables penv) ntables)
            (incf ntables))
           ((string= k "memory")
            (reg (node-id node) (penv-mems penv) nmems)
            (incf nmems))
           ((string= k "global")
            (reg (node-id node) (penv-globals penv) nglobals)
            (incf nglobals))
           ((string= k "tag")
            ;; inline-import tags occupy the index space like the
            ;; other kinds (field order)
            (reg (node-id node) (penv-tags penv) ntags)
            (incf ntags))
           ((string= k "elem")
            (reg (node-id node) (penv-elems penv) nelems)
            (incf nelems))
           ((string= k "data")
            (reg (node-id node) (penv-datas penv) ndatas)
            (incf ndatas))))))))

(defun %inline-import-p (node k)
  "Whether a func/table/memory/global field node carries the inline
(import \"m\" \"f\") abbreviation: skip the optional $id and any
leading (export ...) items, then look for an (import ...) item."
  (declare (ignore k))
  (let ((rest (cddr node)))
    (multiple-value-bind (id rest2) (%opt-id rest)
      (declare (ignore id))
      (loop while (and rest2 (tok-elem-p (car rest2))
                       (string= (%tok-keyword (%first-tok (car rest2)))
                                "export"))
            do (pop rest2))
      (and rest2 (tok-elem-p (car rest2))
           (string= (%tok-keyword (%first-tok (car rest2))) "import")))))

(defun %intern-key (k)
  (intern (string-upcase k) :keyword))

(defun %parse-module-fields (nodes penv)
  "Dispatch each top-level field :list node to its parser. IMPORTS
FIRST: the WAT grammar requires every (import ...) field to precede
all func/table/memory/global definitions (their inline (import ...)
abbreviations expand to import fields at the same position).
imports.wast pins the 16 'import after function/global/table/memory'
assert_malformed cases."
  (let ((seen-def nil))
    (dolist (node nodes)
      (let* ((head (%first-tok node))
             (k (and head (%tok-keyword head))))
        (cond
         ((string= k "type") (%parse-type-field node penv))
         ((string= k "import")
          (when seen-def
            (%text-error (and head (tok-pos head))
                         "import after function/table/memory/global definition"))
          (%parse-import-field node penv))
         ((member k '("func" "table" "memory" "global" "tag") :test #'string=)
          ;; inline-import abbreviations ((func (import ...)) etc.)
          ;; expand to import fields at this position: after any
          ;; definition they are malformed exactly like a bare
          ;; (import ...) there. Otherwise this field IS a
          ;; definition and closes the import prefix.
          (let ((inline-import-p (%inline-import-p node k)))
            (cond
             ((and inline-import-p seen-def)
              (%text-error (and head (tok-pos head))
                           "import after function/table/memory/global definition"))
             ((null inline-import-p)
              (setf seen-def t))))
          (case (%intern-key k)
            (:func (%parse-func-field node penv))
            (:table (%parse-table-field node penv))
            (:memory (%parse-memory-field node penv))
            (:global (%parse-global-field node penv))
            (:tag (%parse-tag-field node penv))))
         ((string= k "export") (%parse-export-field node penv))
         ((string= k "start") (%parse-start-field node penv))
         ((string= k "elem") (%parse-elem-field node penv))
         ((string= k "data") (%parse-data-field node penv))
         (t (%text-error (and head (tok-pos head))
                         "unknown module field ~a" k)))))))

(defun parse-wat (s)
  "Parse WAT text (a string) into a wasm-module IR identical to the
binary decoder's output (CONTRACTS.md). Signals wasmdown:decode-error
with :offset (token index) on malformed text."
  (let* ((tokens (%tokenize s))
         (nodes (%parse-tokens tokens))
         (penv (make-penv)))
    (unless (= (length nodes) 1)
      (%text-error 0 "expected exactly one (module ...)"))
    (let ((node (first nodes)))
      (unless (and (tok-elem-p node)
                   (string= (%tok-keyword (%first-tok node)) "module"))
        (%text-error 0 "expected a (module ...) form")))
    ;; NOTE: the module keyword may be omitted?  No: require it.
    ;; items of the module list: skip the optional $id name (the WAT
    ;; (module $id ...) grammar; linking.wast/elem.wast name modules)
    (let ((items (cddr (first nodes))))
      (when (and items (tok-p (first items))
                 (eq (tok-kind (first items)) :id))
        (setf items (rest items)))
      ;; two-pass types: pass 1 registers every explicit (type ...)
      ;; field (indices 0..n-1, field order) BEFORE any body parses,
      ;; so forward $type references resolve to final indices and
      ;; implicit find-or-add misses dedup-append AFTER the explicit
      ;; block (suite pin: func.wast module 2, (func (type 0)) is the
      ;; LATER explicit $t; module 3's implicit index 3 is the first
      ;; complex-sig use).
      (%prescan-types items penv)
      ;; forward $id references: pre-register the six index spaces
      ;; before any field body parses (WAT allows forward refs)
      (%prescan-ids items penv)
      (%parse-module-fields items penv)
      (make-wasm-module
       :types (coerce (reverse (penv-types-list penv)) 'vector)
       :imports (coerce (reverse (penv-imports-list penv)) 'vector)
       :funcs (coerce (reverse (penv-funcs-list penv)) 'vector)
       :tables (coerce (reverse (penv-tables-list penv)) 'vector)
       :mems (coerce (reverse (penv-mems-list penv)) 'vector)
       :tags (coerce (reverse (penv-tags-list penv)) 'vector)
       :globals (coerce (reverse (penv-globals-list penv)) 'vector)
       :exports (coerce (reverse (penv-exports-list penv)) 'vector)
       :start (penv-start penv)
       :elems (coerce (reverse (penv-elems-list penv)) 'vector)
       :datas (coerce (reverse (penv-datas-list penv)) 'vector)
       :data-count (let ((n (length (penv-datas-list penv))))
                     (if (zerop n) nil n))))))

(defun parse-wat-file (path)
  "Read PATH (pathname or namestring) as UTF-8 text and parse it as a
WAT module via PARSE-WAT."
  (let ((text (with-open-file (in path :external-format :utf-8)
                (let ((out (make-array 4096 :element-type 'character
                                       :fill-pointer 0 :adjustable t)))
                  (loop for ch = (read-char in nil nil)
                        while ch
                        do (vector-push-extend ch out))
                  (coerce out 'string)))))
    (parse-wat text)))
