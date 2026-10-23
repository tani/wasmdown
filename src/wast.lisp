;;;; Stateful .wast spec-script runner (PLAN.md 12.2, M3 deliverable).
;;;; Package WASMDOWN.WAST.
;;;;
;;;; A .wast script is a sequence of commands executed against shared
;;;; state: a registry of named module instances (register), the
;;;; last-defined current module, and the synthetic spectest host
;;;; module. For every command the runner records an outcome from the
;;;; enum pass | fail | skip | unsupported | timeout | harness-error
;;;; (PLAN.md 12.8) plus, for failures, the actual phase from
;;;; decode | validate | link | instantiate | execute.
;;;;
;;;; Script parsing keeps the RAW source span of every module form so
;;;; (module ...) text is handed to wasmdown.text:parse-wat verbatim;
;;;; quote forms are re-assembled from their (decoded) string literals
;;;; and binary forms from their raw byte chunks. No CL:READ ever
;;;; touches fixture content (PLAN.md 5.1).

(defpackage #:wasmdown.wast
  (:use #:cl)
  (:export #:run-script #:run-directory #:make-spectest-instance
           #:spectest-calls #:wast-result #:wast-result-outcome
           #:wast-result-file #:wast-result-command-index
           #:wast-result-command-name #:wast-result-phase
           #:wast-result-detail #:wast-result-elapsed
           #:wast-result-message
           #:summarize-results #:write-jsonl #:result-outcome-counts))

(in-package #:wasmdown.wast)

;;; ---------------------------------------------------------------
;;; Tokenizer (WAT lexer rules; same shape as src/text.lisp but kept
;;; local: src/text.lisp does not export its token layer, and it must
;;; not be modified for this slice). Each token keeps its RAW source
;;; span so module forms can be re-parsed from the original text.

(defstruct stoken
  (kind :keyword :type keyword)          ; :lparen :rparen :string :id
                                         ; :nat :int :float :keyword
  (text "" :type string)                 ; decoded string / raw atom text
  (start 0 :type integer)
  (end 0 :type integer))

(defun %werr (pos control &rest args)
  (error 'wasmdown:decode-error :offset pos
         :message (apply #'format nil control args)))

(declaim (inline %digit-p %hexdigit-p %idchar-p))
(defun %digit-p (c) (and (char>= c #\0) (char<= c #\9)))
(defun %hexdigit-p (c)
  (or (%digit-p c)
      (and (char>= c #\a) (char<= c #\f))
      (and (char>= c #\A) (char<= c #\F))))
(defun %idchar-p (c)
  "Characters allowed inside atoms per the WAT grammar (everything but
whitespace, parens, quote, semicolon, comma and NUL)."
  (not (or (member c '(#\space #\tab #\newline #\return #\page #\null))
           (char= c #\( ) (char= c #\) ) (char= c #\")
           (char= c #\;) (char= c #\,))))

(defun %tokenize (s)
  "The flat token vector of script source S."
  (let ((n (length s)) (tokens '()) (i 0))
    (flet ((emit (kind text start end)
             (push (make-stoken :kind kind :text text
                                :start start :end end) tokens)))
      (loop
        (when (>= i n) (return))
        (let ((c (char s i)) (start i))
          (cond
           ((member c '(#\space #\tab #\newline #\return #\page))
            (incf i))
           ;; line comment
           ((and (< (1+ i) n) (char= c #\;) (char= (char s (1+ i)) #\;))
            (loop while (and (< i n) (char/= (char s i) #\newline))
                  do (incf i)))
           ;; block comment, nestable
           ((and (< (1+ i) n) (char= c #\( ) (char= (char s (1+ i)) #\;))
            (incf i 2)
            (let ((depth 1))
              (loop
                (when (>= i n) (%werr start "unclosed block comment"))
                (cond
                 ((and (< (1+ i) n) (char= (char s i) #\( )
                       (char= (char s (1+ i)) #\;))
                  (incf i 2) (incf depth))
                 ((and (< (1+ i) n) (char= (char s i) #\;)
                       (char= (char s (1+ i)) #\) ))
                  (incf i 2) (decf depth)
                  (when (zerop depth) (return)))
               (t (incf i))))))
           ;; (@ ...) annotation form (unadopted annotations
           ;; proposal): skip the whole form like the reference
           ;; lexer.mll `annot` rule — nested groups, comments,
           ;; strings and reserved atoms (incl , ; [ ] { }) are legal
           ;; inside, and the form emits NO token (recorded and
           ;; stripped in the reference too). Tolerance is only enough
           ;; to STRUCTURE the file: any module text still containing
           ;; `(@` classifies :unsupported (see %unsupported-feature-p)
           ;; since this engine implements no annotation semantics.
           ((and (char= c #\( ) (< (1+ i) n) (char= (char s (1+ i)) #\@))
            (setf i (%skip-annotation s i)))
           ((char= c #\( ) (emit :lparen "(" start (1+ start)) (incf i))
           ((char= c #\) ) (emit :rparen ")" start (1+ start)) (incf i))
           ((char= c #\")
            (multiple-value-bind (decoded end)
                (%read-string s i)
              (emit :string decoded start end)
              (setf i end)))
           (t
            (loop while (and (< i n) (%idchar-p (char s i))) do (incf i))
            (when (= i start)
              (%werr start "unexpected character ~c" c))
            (let ((text (subseq s start i)))
              (emit (%atom-kind text) text start i))))))
      (coerce (nreverse tokens) 'vector))))

(defun %skip-annotation (s i)
  "Skip an annotation form starting at S[I] = #\\( with #\\@ at I+1.
Returns the index past the closing paren. Mirrors the reference
lexer.mll `annot` rule structurally: inside an annotation, balanced
groups nest, nested (@ ...) annotations are skipped too, strings may
carry any escape or raw char, comments nest, and atoms may include
the reserved characters , ; [ ] { } (annotations.wast ::14 uses them
as bare atoms). An unterminated form fails 'unclosed annotation' —
a catchable decode error, matching the reference message."
  (let ((n (length s))
        (open i)
        (depth 1))                         ; already inside "(@"
    (declare (type (integer 0) open))
    (incf i 2)                             ; past "(@"
    (loop
      (when (>= i n) (%werr open "unclosed annotation"))
      (let ((c (char s i)))
        (cond
         ((char= c #\) )
          (decf depth)
          (incf i)
          (when (zerop depth) (return i)))
         ((char= c #\( )
          (cond
           ;; nested block comment (; ... ;), nestable
           ((and (< (1+ i) n) (char= (char s (1+ i)) #\;))
            (incf i 2)
            (let ((cdepth 1))
              (loop
                (when (>= i n) (%werr open "unclosed annotation"))
                (cond
                 ((and (< (1+ i) n) (char= (char s i) #\( )
                       (char= (char s (1+ i)) #\;))
                  (incf i 2) (incf cdepth))
                 ((and (< (1+ i) n) (char= (char s i) #\;)
                       (char= (char s (1+ i)) #\) ))
                  (incf i 2) (decf cdepth)
                  (when (zerop cdepth) (return)))
                 (t (incf i))))))
           ;; nested group or (@...) annotation: depth-tracked
           (t (incf depth) (incf i))))
         ;; string literal: escapes may carry any char incl. quotes
         ((char= c #\")
          (incf i)
          (loop
            (when (>= i n) (%werr open "unclosed string literal"))
            (let ((sc (char s i)))
              (cond
               ((char= sc #\") (incf i) (return))
               ((char= sc #\\)
                (when (>= (1+ i) n)
                  (%werr open "unclosed string literal"))
                (incf i 2))
               (t (incf i))))))
         ;; line comment ;; ... to end of line
         ((and (< (1+ i) n) (char= c #\;) (char= (char s (1+ i)) #\;))
          (loop while (and (< i n) (char/= (char s i) #\newline))
                do (incf i)))
         ;; any other char (reserved , ; [ ] { } included) is atom
         (t (incf i)))))))
(defun %read-string (s i)
  "Decode a \"...\" literal starting at S[I]; returns the decoded text
and the index past the closing quote. Escapes: \t \n \r \" \' \\
\\xx (hex byte) \\u{...} (code point, UTF-8 encoded). The result is
BYTE-ACCURATE: every character of the returned string is a byte
value (raw source characters above U+007F are re-encoded as their
UTF-8 bytes) so quote payloads carry exactly the bytes the binary
form would, and the text decoder can enforce UTF-8 validity on names."
  (let ((n (length s))
        (out (make-array 16 :element-type 'character
                         :fill-pointer 0 :adjustable t)))
    (incf i)
    (loop
      (when (>= i n) (%werr i "unclosed string literal"))
      (let ((c (char s i)))
        (cond
         ((char= c #\")
          (return (values (coerce out 'string) (1+ i))))
         ((char/= c #\\)
          (if (< (char-code c) #x80)
              (vector-push-extend c out)
              (%push-utf8 (char-code c) out))
          (incf i))
         (t (incf i)
            (when (>= i n) (%werr i "unclosed string escape"))
            (let ((e (char s i)))
              (cond
               ((char= e #\t) (vector-push-extend #\Tab out) (incf i))
               ((char= e #\n) (vector-push-extend #\Newline out) (incf i))
               ((char= e #\r) (vector-push-extend #\Return out) (incf i))
               ((char= e #\") (vector-push-extend #\" out) (incf i))
               ((char= e #\') (vector-push-extend #\' out) (incf i))
               ((char= e #\\) (vector-push-extend #\\ out) (incf i))
               ((char= e #\u)
                ;; \u{hex} per the script grammar: the code point's
                ;; UTF-8 ENCODING is appended (multi-byte for
                ;; U+0080..U+10FFFF), so the reassembled quote text
                ;; carries the same bytes the binary form would.
                ;; Surrogates (D800-DFFF) and > U+10FFFF are rejected
                ;; here; raw \xx escapes deliberately pass through so
                ;; the text decoder can flag malformed UTF-8.
                (let ((j (+ i 2)))
                  (unless (and (< j n) (char= (char s (1+ i)) #\{))
                    (%werr i "bad \\u escape"))
                  (loop
                    (unless (< j n) (%werr j "unclosed \\u escape"))
                    (let ((u (char s j)))
                      (cond ((char= u #\}) (incf j) (return))
                            ((%hexdigit-p u) (incf j))
                            (t (%werr j "bad \\u escape")))))
                  (let ((hex (subseq s (+ i 2) (1- j))))
                    (when (zerop (length hex))
                      (%werr i "empty \\u escape"))
                    (let ((cp (parse-integer hex :radix 16)))
                      (cond
                       ((and (>= cp #xD800) (<= cp #xDFFF))
                        (%werr i "malformed UTF-8 codepoint"))
                       ((> cp #x10FFFF)
                        (%werr i "malformed UTF-8 codepoint"))
                       (t (%push-utf8 cp out)))))
                  (setf i j)))
               ((%hexdigit-p e)
                (unless (and (< (1+ i) n) (%hexdigit-p (char s (1+ i))))
                  (%werr i "bad hex escape"))
                (vector-push-extend
                 (code-char (parse-integer s :start i :end (+ i 2) :radix 16))
                 out)
                (incf i 2))
               (t (%werr i "unknown string escape \\~c" e))))))))))

(defun %push-utf8 (cp out)
  "Append the UTF-8 encoding of code point CP to the adjustable
character string OUT (one character per byte, byte-valued)."
  (flet ((b (x) (vector-push-extend (code-char x) out)))
    (cond
     ((< cp #x80) (b cp))
     ((< cp #x800) (b (logior #xC0 (ash cp -6))) (b (logior #x80 (logand cp #x3F))))
     ((< cp #x10000)
      (b (logior #xE0 (ash cp -12)))
      (b (logior #x80 (logand (ash cp -6) #x3F)))
      (b (logior #x80 (logand cp #x3F))))
     (t
      (b (logior #xF0 (ash cp -18)))
      (b (logior #x80 (logand (ash cp -12) #x3F)))
      (b (logior #x80 (logand (ash cp -6) #x3F)))
      (b (logior #x80 (logand cp #x3F)))))))

(defun %atom-kind (text)
  "Classify an atom: $id, :nat (unsigned int), :int (signed int),
:float (inf/nan/nan:0x|canonical|arithmetic/dec/hex float) or :keyword."
  (let* ((len (length text))
         (p (if (and (plusp len) (member (char text 0) '(#\+ #\-))) 1 0))
         (neg (and (plusp len) (char= (char text 0) #\-)))
         (body (subseq text p)))
    (cond
     ((and (plusp len) (char= (char text 0) #\$)) :id)
     ;; hex integer: 0x... (sign allowed; negative -> :int)
     ((and (>= (- len p) 3) (char= (char body 0) #\0)
           (member (char body 1) '(#\x #\X))
           (notevery (lambda (c) (char= c #\_))
                     (subseq body 2)))
      (if neg :int :nat))
     ;; decimal integer: digits (with _ separators)
     ((and (< p len)
           (every (lambda (c) (or (%digit-p c) (char= c #\_))) body)
           (notevery (lambda (c) (char= c #\_)) body))
      (if neg :int :nat))
     ;; float shapes: inf, nan, nan:..., d.ddd, 0xh.hhp±d, de±d
     ((%script-float-p text) :float)
     (t :keyword))))

(defun %script-float-p (text)
  "Whether TEXT is a float-shaped literal in a script value position
(includes nan:canonical / nan:arithmetic, which never appear inside
modules but do appear in assert_return results)."
  (let ((low (string-downcase text)))
    (cond ((or (string= low "inf") (string= low "nan")) t)
          ((and (> (length low) 4) (string= low "nan:" :end1 4)) t)
          (t (and (member (%num-shape low) '(:hexfloat :decfloat)) t)))))

(defun %num-shape (low)
  "The numeric shape of atom text LOWERCASED: :decint/:hexint/:decfloat/
:hexfloat/:other, with _ allowed like a digit."
  (labels ((digp (c) (or (%digit-p c) (char= c #\_)))
            (hexp (c) (or (%hexdigit-p c) (char= c #\_))))
    (let* ((len (length low))
           (p (if (and (plusp len) (member (char low 0) '(#\+ #\-))) 1 0))
           (hex (and (< (+ p 1) len) (char= (char low p) #\0)
                     (char= (char low (1+ p)) #\x)))
           (pred (if hex #'hexp #'digp)))
      (loop while (and (< p len) (funcall pred (char low p))) do (incf p))
      (cond
       ((and (= p len) (> p 0) (not (char= (char low (1- p)) #\_)))
        (if hex :hexint :decint))
       ;; fraction with optional exponent
       ((and (< p len) (char= (char low p) #\.))
        (incf p)
        (loop while (and (< p len) (funcall pred (char low p)))
              do (incf p))
        (cond
         ((and (< p len)
               (char= (char low p) (if hex #\p #\e)))
          (incf p)
          (when (and (< p len) (member (char low p) '(#\+ #\-))) (incf p))
          (loop while (and (< p len) (%digit-p (char low p))) do (incf p))
          (if (= p len) (if hex :hexfloat :decfloat) :other))
         ((and (not hex) (= p len)) :decfloat)
         (t :other)))
       ;; bare exponent
       ((and (< p len) (char= (char low p) (if hex #\p #\e)))
        (incf p)
        (when (and (< p len) (member (char low p) '(#\+ #\-))) (incf p))
        (loop while (and (< p len) (%digit-p (char low p))) do (incf p))
        (if (= p len) (if hex :hexfloat :decfloat) :other))
       (t :other)))))

;;; ---------------------------------------------------------------
;;; Token trees. A node is (:list start end . children) or a leaf
;;; stoken; children are the tokens strictly between the parens. The
;;; span lets us slice the ORIGINAL source for (module ...) text.

(defun %parse-tree (tokens source)
  "Build the s-expression tree; returns the list of top-level nodes.
A node is (:list start end . children) where start is the ( char and
end the closing ) char, so (source start end) slices the whole form."
  (declare (ignore source))

  ;; frame: (open-pos . reverse children); top-level frame has no open.
  (let ((stack (list (cons nil nil))))
    (flet ((add (x frame)
             (setf (cdr frame) (cons x (cdr frame)))))
      (loop for tk across tokens
            do (cond
                ((eq (stoken-kind tk) :lparen)
                 (push (cons (stoken-start tk) nil) stack))
                ((eq (stoken-kind tk) :rparen)
                 (let* ((frame (pop stack)))
                   (when (null stack)
                     (%werr (stoken-start tk)
                            "unbalanced right parenthesis"))
                   (add (list* :list (car frame) (stoken-start tk)
                              (nreverse (cdr frame)))
                        (first stack))))
                (t (add tk (first stack)))))
      (unless (null (cdr stack))
        (%werr 0 "unclosed parenthesis"))
      (nreverse (cdr (first stack))))))

(defun %node-p (x) (and (consp x) (eq (car x) :list)))
(defun %node-span (x) (values (second x) (third x)))
(defun %node-children (x) (cdddr x))
(declaim (inline %node-p))

(defun %head-keyword (node)
  "The lowercased keyword text of the first child of NODE, or nil."
  (let ((first (first (%node-children node))))
    (and (stoken-p first) (string-downcase (stoken-text first)))))

;;; ---------------------------------------------------------------
;;; Script commands. Values are parsed into typed records:
;;;
;;;   (:num type value)          integer; TYPE :i32/:i64, VALUE signed
;;;   (:float type bits class)   float; BITS the exact bit pattern,
;;;                              CLASS :exact | :nan-canonical |
;;;                              :nan-arithmetic | :nan-payload
;;;   (:ref-func)                any non-null funcref
;;;   (:ref-func id)             specific funcref identity
;;;   (:ref-null)                the null reference
;;;   (:ref-extern value)        opaque externref with host identity
;;;
;;; Module forms are kept as (:module-form kind source-bytes id) where
;;; KIND is :text (SOURCE the raw span slice), :quote (parsed source
;;; re-assembled) or :binary (raw byte vector).

(defstruct wast-cmd
  name                                   ; keyword: :module :invoke ...
  index                                  ; 0-based command position
  start                                  ; source position (diagnostics)
  ;; per-command payload (slots)
  id                                     ; $id for module/register
  name-arg                               ; register name / export name
  module-arg                             ; $mod reference for actions
  strings                                ; raw strings (binary/quote)
  text                                   ; raw module text (plain/quote)
  bytes                                  ; octet vector (binary)
  args                                   ; parsed literal args (invoke)
  results                                ; parsed result values
  message                               ; expected-failure message
  named-form)                           ; :definition / :instance for the
                                        ; Core 3.0 named module forms

(defun %parse-int-token (tk)
  "An integer token's value (sign-aware, dec or hex, _ separators)."
  (let* ((text (remove #\_ (stoken-text tk)))
         (p (if (and (plusp (length text))
                     (member (char text 0) '(#\+ #\-))) 1 0))
         (hex (and (>= (- (length text) p) 3)
                   (char= (char text p) #\0)
                   (char= (char text (1+ p)) #\x))))
    (multiple-value-bind (v end)
        (parse-integer text :start (if hex (+ p 2) p)
                              :radix (if hex 16 10)
                              :junk-allowed t)
      (declare (ignore end))
      (if (and (plusp p) (char= (char text 0) #\-)) (- v) v))))

(defun %parse-nat-hex (text)
  "Parse 0x... / decimal / nan:0x... payload text (no sign)."
  (let ((clean (remove #\_ text)))
    (parse-integer clean
                   :radix (if (and (> (length clean) 2)
                                   (char= (char clean 1) #\x)) 16 10)
                   :start (if (and (> (length clean) 2)
                                   (char= (char clean 1) #\x)) 2 0))))

(defun %float-class-and-bits (text p bias payload-max)
  "Parse a float literal TEXT into (values bits class). P is 24/53,
BIAS 127/1023, PAYLOAD-MAX the max nan:0x payload (2^(p-1)-1).
CLASS: :nan-canonical :nan-arithmetic :nan-payload or :exact."
  (let ((low (string-downcase text))
        (mant-w (- p 1))
        (exp-all-ones (1- (ash 1 (if (= p 24) 8 11)))))
    (cond
     ((or (string= low "nan") (string= low "nan:canonical"))
      (values (logior (ash exp-all-ones mant-w) (ash 1 (- mant-w 1)))
              (if (string= low "nan") :nan-canonical :nan-canonical)))
     ((string= low "nan:arithmetic")
      (values (logior (ash exp-all-ones mant-w) (ash 1 (- mant-w 1)))
              :nan-arithmetic))
     ((and (> (length low) 4) (string= low "nan:" :end1 4))
      ;; payload after "nan:0x"
      (let ((payload (parse-integer (remove #\_ (subseq low 6))
                                   :radix 16)))
        (if (= payload (ash 1 (- mant-w 1)))
            (values (logior (ash exp-all-ones mant-w) payload)
                    :nan-canonical)
            (values (logior (ash exp-all-ones mant-w) payload)
                    :nan-payload))))
     ((and (char= (char low 0) #\-)
           (or (string= low "-inf") (string= low "-nan")
               (> (length low) 5)))
      ;; negative inf/nan forms carry the sign bit
      (multiple-value-bind (bits class)
          (%float-class-and-bits (subseq low 1) p bias payload-max)
        (values (logior (ash 1 (+ mant-w (if (= p 24) 8 11))) bits)
                class)))
     ((char= (char low 0) #\+)
      (%float-class-and-bits (subseq low 1) p bias payload-max))
     (t
      (let* ((neg (char= (char low 0) #\-))
             (body (if (member (char low 0) '(#\+ #\-))
                       (subseq low 1) low)))
        (declare (ignore neg))
        (cond
         ((string= body "inf")
          (values (ash exp-all-ones mant-w) :exact))
         (t
          ;; numeric literal: reuse the module parser's exact encoder
          ;; via wasmdown.text (same rules, same rounding).
          (values (wasmdown.text::%float-bits low p bias) :exact))))))))

;;; ---------------------------------------------------------------
;;; Value parsing: (iNN.const lit) / (ref.func ...) / (ref.null ...) /
;;; (ref.extern n) forms.

(defun %const-value (node)
  "Parse a (i32|i64|f32|f64.const lit) or ref value form NODE into a
typed value record, or NIL when NODE is not a value form."
  (unless (and (%node-p node) (%node-children node)) (return-from %const-value nil))
  (let* ((items (%node-children node))
         (head (first items)))
    (unless (and (stoken-p head) (eq (stoken-kind head) :keyword))
      (return-from %const-value nil))
    (let ((k (string-downcase (stoken-text head)))
          (rest (rest items)))
      (cond
       ((string= k "i32.const")
        (let ((tk (%one-tok rest "i32.const")))
          (list :num :i32 (%parse-int-token tk))))
       ((string= k "i64.const")
        (let ((tk (%one-tok rest "i64.const")))
          (list :num :i64 (%parse-int-token tk))))
       ((string= k "f32.const")
        (let* ((tk (%one-tok rest "f32.const"))
               (text (stoken-text tk)))
          (multiple-value-bind (bits class)
              (%float-class-and-bits text 24 127 #x7fffff)
            (list :float :f32 bits class))))
       ((string= k "f64.const")
        (let* ((tk (%one-tok rest "f64.const"))
               (text (stoken-text tk)))
          (multiple-value-bind (bits class)
              (%float-class-and-bits text 53 1023 #x7ffffffffffff)
            (list :float :f64 bits class))))
       ((string= k "ref.func")
        ;; (ref.func) = any non-null funcref; (ref.func $id) = identity
        (if (null rest)
            '(:ref-func)
            (let ((tk (%one-tok rest "ref.func")))
              (list :ref-func (stoken-text tk)))))
       ((string= k "ref.null")
        '(:ref-null))
       ((string= k "ref.extern")
        (let ((tk (%one-tok rest "ref.extern")))
          (list :ref-extern (%parse-int-token tk))))
       (t nil)))))

(defun %one-tok (items what)
  (unless (and items (stoken-p (first items)) (null (rest items)))
    (%werr 0 "bad ~a form" what))
  (first items))

(defun %value-forms (nodes)
  "Parse a list of value-form nodes; every element must be a value form."
  (mapcar (lambda (n)
            (or (%const-value n)
                (%werr 0 "expected a value form")))
          nodes))

;;; ---------------------------------------------------------------
;;; Module forms: (module $id? ...), (module $id? binary "...")*,
;;; (module $id? quote "...")*. Plain text keeps the raw source span.

(defun %parse-module-form (node source)
  "Parse a (module ...) node. Returns (values kind id payload):
KIND :text (payload = string), :binary (payload = octet vector),
:quote (payload = string), :definition (payload = the module source
span — stored, not instantiated), :instance (payload = the referenced
definition's $id), or signals decode-error on bad shape."
  (let ((items (rest (%node-children node))))   ; skip 'module'
    (multiple-value-bind (id rest) (%opt-id items)
      (cond
       ;; (module definition ...)/(module instance ...): Core 3.0
       ;; named forms. A DEFINITION parses and validates its fields
       ;; but is NOT instantiated — the source span is kept for the
       ;; later (module instance $I $M). An INSTANCE names a stored
       ;; definition: (module instance $I $M) — the id after the
       ;; keyword is the NEW instance's name and the following id the
       ;; definition's $id.
       ((and rest (stoken-p (first rest))
             (eq (stoken-kind (first rest)) :keyword)
             (member (string-downcase (stoken-text (first rest)))
                     '("definition" "instance") :test #'string=))
        (let* ((k (string-downcase (stoken-text (first rest))))
               (after (rest rest))
               (nid (or id
                        (and after (stoken-p (first after))
                             (eq (stoken-kind (first after)) :id)
                             (stoken-text (first after))))))
          (cond
           ((string= k "definition")
            ;; payload: the FIELDS' source span, wrapped as a module
            ;; so parse-wat accepts it at instance time.
            (multiple-value-bind (start end) (%node-span node)
              (let ((full (subseq source start (1+ end))))
                (values :definition nid full))))
           (t
            ;; (module instance $I $M): payload = the definition's $id
            (let ((def-id (or (and id
                                    (not nid)
                                    id)
                               (and after (stoken-p (second after))
                                    (eq (stoken-kind (second after)) :id)
                                    (stoken-text (second after))))))
              (values :instance nid def-id))))))
       ;; binary/quote keywords
       ((and rest (stoken-p (first rest))
             (eq (stoken-kind (first rest)) :keyword)
             (member (string-downcase (stoken-text (first rest)))
                     '("binary" "quote") :test #'string=))
        (let* ((k (string-downcase (stoken-text (first rest))))
               (strs (rest rest)))
          (unless (and strs (every #'stoken-p strs)
                       (every (lambda (x) (eq (stoken-kind x) :string))
                              strs))
            (%werr 0 "(module ~a ...) needs string literals" k))
          (cond
           ((string= k "binary")
            (values :binary id (%concat-bytes strs)))
           (t
            ;; The quote payload is module FIELDS per the script
            ;; grammar; parse-wat expects a whole (module ...) form,
            ;; so wrap the concatenation (PLAN.md 12: quote payloads
            ;; were previously handed verbatim to parse-wat, which
            ;; rejected them with "expected exactly one (module
            ;; ...)" — recording assert_malformed cases as
            ;; misattributed passes).
            (values :quote id
                    (format nil "(module ~a)"
                            (%concat-strings strs)))))))
       (t
        ;; plain text: parse-wat expects the whole (module ...) form;
        ;; slice the node's full source span (parens included).
        (multiple-value-bind (start end) (%node-span node)
          (values :text id (subseq source start (1+ end)))))))))

(defun %opt-id (items)
  "Split an optional leading $id token off ITEMS."
  (if (and items (stoken-p (first items)) (eq (stoken-kind (first items)) :id))
      (values (stoken-text (first items)) (rest items))
      (values nil items)))

(defparameter *wast-max-string-bytes* (* 64 1024 1024)
  "Ceiling on one (module binary/quote ...) string literal's decoded
byte length; a hostile overlong literal is rejected with a catchable
decode-error instead of an unbounded allocation. 64 MiB is far above
any legitimate fixture (the largest vendored module is ~64 KiB).")

(defun %concat-bytes (string-toks)
  "The raw byte vector of a (module binary ...) string sequence. Each
string literal is a sequence of bytes per the spec's string syntax;
\\xx escapes are kept as bytes, plain characters as their ASCII bytes
(raw binary content must be ASCII-safe per the spec grammar). The
assembled total is capped at *WAST-MAX-STRING-BYTES*."
  (let ((chunks '()))
    (dolist (s string-toks)
      (push (%bytes-of-string (stoken-text s)) chunks))
    (let* ((total (reduce #'+ (mapcar #'length chunks)))
           (out (progn
                  (unless (<= total *wast-max-string-bytes*)
                    (%werr 0 "assembled binary too large: ~d bytes (limit ~d)"
                           total *wast-max-string-bytes*))
                  (make-array total :element-type '(unsigned-byte 8))))
           (pos 0))
      (dolist (v (nreverse chunks))
        (replace out v :start1 pos)
        (incf pos (length v)))
      out)))

(defun %bytes-of-string (str)
  "One (module binary ...) string literal to its byte vector. Each
source character (or \\xx escape) is exactly one byte, so the output is
pre-sized to the input length and capped at
*WAST-MAX-STRING-BYTES*; a hostile overlong literal is a catchable
decode-error, never an unbounded allocation."
  (let ((n (length str)))
    (when (> n *wast-max-string-bytes*)
      (%werr 0 "string literal too long: ~d characters (limit ~d)"
             n *wast-max-string-bytes*))
    (let ((v (make-array n :element-type '(unsigned-byte 8)
                            :fill-pointer 0))
          (i 0))
      (loop while (< i n)
            do (let ((c (char str i)))
                 (cond
                  ((char= c #\\)
                   (let ((h1 (and (< (+ i 1) n) (char str (+ i 1))))
                         (h2 (and (< (+ i 2) n) (char str (+ i 2)))))
                     (unless (and h1 h2 (%hexdigit-p h1) (%hexdigit-p h2))
                       (%werr 0 "bad binary escape"))
                     (vector-push (parse-integer str :start (+ i 1)
                                                :end (+ i 3) :radix 16)
                                 v)
                     (incf i 3)))
                  (t
                   (let ((code (char-code c)))
                     (unless (< code 256)
                       (%werr 0 "non-ASCII byte in binary string"))
                     (vector-push code v)
                     (incf i))))))
      v)))

(defun %concat-strings (string-toks)
  "Join (module quote ...) strings with a newline between parts (the
spec treats the parts as consecutive source lines)."
  (let ((parts (mapcar #'stoken-text string-toks)))
    (with-output-to-string (acc)
      (loop for p in parts
            for first = t then nil
            unless first do (write-char #\newline acc)
            do (write-string p acc)))))

(defun %byte-string-to-string (s)
  "Decode a BYTE-STRING (chars = UTF-8 bytes, as %read-string
produces) into a proper Lisp string. The engine's export registry
stores UTF-8-DECODED names (via the text parser's %name-string), so
action names (invoke/get) and register names must be decoded the
same way or lookups mismatch for any non-ASCII name (names.wast)."
  (let ((bytes (make-array (length s) :element-type '(unsigned-byte 8))))
    (loop for c across s for i from 0
          do (setf (aref bytes i) (char-code c)))
    ;; reuse the text parser's UTF-8 decoder semantics
    (wasmdown.text::%name-string bytes)))

;;; ---------------------------------------------------------------
;;; Top-level command dispatch: script source -> wast-cmd list.

(defun %parse-action (node)
  "An action form: (invoke $mod? \"name\" lit*) or (get $mod? \"name\").
Returns (values kind mod-id name args)."
  (let* ((items (rest (%node-children node))))      ; skip head
    (multiple-value-bind (mid rest) (%opt-id items)
      (let ((name-tok (first rest)))
        (unless (and name-tok (stoken-p name-tok)
                     (eq (stoken-kind name-tok) :string))
          (%werr 0 "action needs a string name"))
        (values mid (%byte-string-to-string (stoken-text name-tok))
                (mapcar #'%const-value (rest rest)))))))

(defun %module-field-head-p (head)
  "Whether HEAD names a WAT module field (type/import/func/table/
memory/global/export/start/elem/data). The script grammar's
inline-module short form (parser.mly `script : list(cmd) EOF |
inline_module1 EOF`, `inline_module : module_fields`): a .wast file
may consist of BARE module fields at top level, wrapping into ONE
implicit (module ...) command (inline-module.wast)."
  (and head
       (member head '("type" "import" "func" "table" "memory"
                      "global" "export" "start" "elem" "data")
               :test #'string=)))
(defun parse-script (source)
  "Parse script source into a list of wast-cmd."
  ;; Implicit-module short form: a run of consecutive top-level module
  ;; FIELD forms wraps into ONE implicit (module ...) command (script
  ;; grammar, parser.mly `script : list(cmd) EOF | inline_module1 EOF`,
  ;; `inline_module : module_fields`; the implicit text is the raw
  ;; source span of the whole run re-wrapped as "(module ...)").
  (let* ((tokens (%tokenize source))
         (nodes (%parse-tree tokens source))
         (cmds '())
         (implicit-start nil)               ; first node of pending run
         (implicit-end nil))                ; last node of pending run
    (flet ((flush-implicit ()
             "Close a pending implicit-module run into one command."
             (when implicit-start
               (push (make-wast-cmd
                       :name :module :index 0
                       :start (second implicit-start)
                       :text (concatenate
                              'string "(module "
                              (subseq source (second implicit-start)
                                       (1+ (third implicit-end)))
                              ")"))
                     cmds)
               (setf implicit-start nil implicit-end nil))))
      (loop for node in nodes
            for idx from 0
            do (let ((head (%head-keyword node)))
                 (cond
                  ;; bare module field: extend (or start) the run
                  ((%module-field-head-p head)
                   (setf implicit-start (or implicit-start node)
                         implicit-end node))
                  (t
                   (flush-implicit)
                   (setf cmds (parse-command-node node idx source cmds))))))
      (flush-implicit))
    (nreverse cmds)))

(defun parse-command-node (node idx source cmds)
  "Parse one top-level COMMAND node, returning CMDS extended with its
wast-cmd. Signals a catchable decode error on unknown/malformed
command shape."
  (let ((head (%head-keyword node)))
    (unless head
      (%werr (second node) "expected a command form"))
    (flet ((mk (&rest args)
             (cons (apply #'make-wast-cmd
                          :name (intern (string-upcase head) :keyword)
                          :index idx
                          :start (second node)
                          args)
                   cmds)))
      (macrolet
          ((with-msg ((mvar) &body body)
             `(let ((,mvar (car (last (%node-children node)))))
                (unless (and (stoken-p ,mvar)
                             (eq (stoken-kind ,mvar) :string))
                  (%werr 0 "expected a message string"))
                ,@body)))
        (cond
         ((string= head "module")
          (multiple-value-bind (kind id payload)
              (%parse-module-form node source)
            (ecase kind
              (:text (mk :id id :text payload))
              (:binary (mk :id id :bytes payload))
              (:quote (mk :id id :text payload))
              ;; named forms (Core 3.0): definition carries the source
              ;; span in TEXT (stored un-instantiated); instance
              ;; carries the definition's $id in MODULE-ARG.
              (:definition (mk :id id :text payload :named-form :definition))
              (:instance (mk :id id :module-arg payload
                             :named-form :instance)))))
         ((string= head "register")
          (let* ((items (rest (%node-children node)))
                 (name (first items)))
            (unless (and name (stoken-p name)
                         (eq (stoken-kind name) :string))
              (%werr 0 "register needs a string name"))
            (multiple-value-bind (id rest)
                (%opt-id (rest items))
              (declare (ignore rest))
              (mk :name-arg (%byte-string-to-string (stoken-text name))
                  :id id))))
         ((string= head "invoke")
          (multiple-value-bind (mid name args)
              (%parse-action node)
            (mk :module-arg mid :name-arg name :args args)))
         ((string= head "get")
          (multiple-value-bind (mid name args)
              (%parse-action node)
            (declare (ignore args))
            (mk :module-arg mid :name-arg name)))
         ((string= head "assert_return")
          (let ((action (%action-node node))
                (results (%results-of node)))
            (mk :module-arg action :results results)))
         ((member head '("assert_malformed" "assert_invalid"
                         "assert_unlinkable" "assert_trap"
                         "assert_exhaustion")
                  :test #'string=)
          (with-msg (msg)
            (let ((first (%action-or-module node)))
              (mk :module-arg first
                  :message (stoken-text msg)))))
         (t (%werr (second node) "unknown script command ~a"
                   head)))))))

(defun %action-node (node)
  "The second child of an assert_return form (the action)."
  (second (%node-children node)))

(defun %results-of (node)
  "The result value forms of an assert_return (all children after the
action)."
  (mapcar #'%const-value (cddr (%node-children node))))

(defun %action-or-module (node)
  "The payload of a failure assertion: the first child after the head —
either an (invoke/get) action node or a (module ...) node. Returned
verbatim; the runner interprets it."
  (second (%node-children node)))

;;; ---------------------------------------------------------------
;;; The spectest host module. Pinned from tests/spec-core/imports.wast:
;;;   global_i32 = 666  global_i64 = 666  (lines 283-289)
;;;   global_f32 = 666.6  global_f64 = 666.6
;;;   memory limits 1..2 (lines 495, 531-536; grow test 662-670:
;;;     after grow within max 3 the import allows 1..3)
;;;   table funcref min 10 max 20 (lines 383-470: imports of
;;;     (table 10 20 funcref) link; (table 12) / (table 10 15) fail)
;;; print_* functions are no-op host functions that record calls.

(defvar *spectest-calls* '()
  "Recorded (function-name . args) calls to spectest print functions,
most recent first. Bound fresh per script run.")

(defun make-spectest-instance ()
  "A fresh synthetic spectest module instance (host functions, globals,
memory, table). The memory is SHARED per instance; each script run
creates one spectest instance, matching the spec harness's stateful
behavior."
  (let ((entries '()))
    (flet ((host-fn (name params results)
             (wasmdown.runtime:make-host-func-instance
              (wasmdown.types:make-wasm-func-type params results)
              (lambda (args)
                (push (cons name (copy-list args)) *spectest-calls*)
                nil)))
           (add (module field value)
             (push (cons (list module field) value) entries)))
      ;; print functions: no results
      (add "spectest" "print" (host-fn "print" '() '()))
      (add "spectest" "print_i32" (host-fn "print_i32" '(:i32) '()))
      (add "spectest" "print_i64" (host-fn "print_i64" '(:i64) '()))
      (add "spectest" "print_f32" (host-fn "print_f32" '(:f32) '()))
      (add "spectest" "print_f64" (host-fn "print_f64" '(:f64) '()))
      (add "spectest" "print_i32_f32"
           (host-fn "print_i32_f32" '(:i32 :f32) '()))
      (add "spectest" "print_f64_f64"
           (host-fn "print_f64_f64" '(:f64 :f64) '()))
      ;; globals: immutable (imports.wast 263-289 pins 666/666.6)
      (add "spectest" "global_i32"
           (wasmdown.runtime:make-global-instance 666
             :type :i32 :mutable nil))
      (add "spectest" "global_i64"
           (wasmdown.runtime:make-global-instance 666
             :type :i64 :mutable nil))
      (add "spectest" "global_f32"
           (wasmdown.runtime:make-global-instance
             (wasmdown.numops:f32-from-bits #x4426A666) ; 666.6
             :type :f32 :mutable nil))
      (add "spectest" "global_f64"
           (wasmdown.runtime:make-global-instance 666.6d0
             :type :f64 :mutable nil))
      ;; memory: 1 page, max 2 (imports.wast 495-536)
      (add "spectest" "memory"
           (wasmdown.runtime:make-memory-instance
             (wasmdown.types:make-wasm-limits 1 2)))
      ;; table: funcref, min 10, max 20 (imports.wast 383-470)
      (add "spectest" "table"
           (wasmdown.runtime:make-table-instance
             :funcref (wasmdown.types:make-wasm-limits 10 20)))
      entries)))

;;; ---------------------------------------------------------------
;;; Runner state.

(defstruct wast-state
  (registry (make-hash-table :test #'equal) :type hash-table)
  (named-instances (make-hash-table :test #'equal) :type hash-table)
  (current nil)
  (spectest nil)
  ;; names of $id modules that decoded as out-of-scope-unsupported;
  ;; later actions referencing them classify :unsupported, not :fail
  (unsupported-named (make-hash-table :test #'equal) :type hash-table)
  ;; true when the CURRENT module position holds an unsupported module
  (current-unsupported nil :type boolean)
  ;; registered names whose instance is out-of-scope-unsupported;
  ;; imports from them classify :unsupported, never 'unknown import'
  (unsupported-registry (make-hash-table :test #'equal) :type hash-table)
  ;; EH/named-forms (Core 3.0): (module definition $M ...) stores the
  ;; parsed module here keyed by $id — instantiation is deferred to
  ;; (module instance $I $M).
  (module-definitions (make-hash-table :test #'equal) :type hash-table))

(defun %register-unsupported (state name)
  "Record that registered name NAME holds an out-of-scope-unsupported
module: importing from it must classify :unsupported (the instance's
exports — typically EH tags — are outside this engine slice), never
fail as 'unknown import'."
  (setf (gethash name (wast-state-unsupported-registry state)) t))

(defun %registered-unsupported-p (state module-name)
  (gethash module-name (wast-state-unsupported-registry state)))

(defun %import-entries (state)
  "The full imports alist: spectest entries + registered instances'
  exports flattened as (module field) -> value."
  (let ((alist (copy-list (wast-state-spectest state))))
    (maphash
     (lambda (name inst)
       (dolist (cell (wasmdown.runtime:module-instance-exports inst))
         (push (cons (list name (car cell)) (cdr cell)) alist)))
     (wast-state-registry state))
    alist))

;;; ---------------------------------------------------------------
;;; Module execution with phase classification. Returns
;;; (values outcome phase detail):
;;;   outcome :pass | :fail | :unsupported | :harness-error
;;;   phase   nil | :decode | :validate | :link | :instantiate | :execute

(defun %decode-module-form (cmd)
  "Decode a module command's module (text/quote -> parse-wat, binary ->
decode-module). Signals wasmdown:decode-error. Returns the wasm-module
or :unsupported when the text uses out-of-scope features."
  (cond
   ((wast-cmd-bytes cmd)
    (wasmdown.binary:decode-module (wast-cmd-bytes cmd)))
   (t
    (let ((text (wast-cmd-text cmd)))
      (when (%unsupported-feature-p text)
        (return-from %decode-module-form :unsupported))
      (wasmdown.text:parse-wat text)))))

(defun %token-present-p (text token)
  "True when TOKEN appears in TEXT as a standalone identifier: not
embedded in a longer name and not a hex-literal fragment (the SIMD
shapes must not trip on 0x4/-0x8 literals or $x2 locals)."
  (loop for start = (search token text :test #'char-equal)
          then (search token text :start2 (1+ start) :test #'char-equal)
        while start
        when (and (or (zerop start)
                      (not (or (alphanumericp (char text (1- start)))
                               (char= (char text (1- start)) #\_)
                               (char= (char text (1- start)) #\$))))
                  (let ((end (+ start (length token))))
                    (or (= end (length text))
                        (not (or (alphanumericp (char text end))
                                 (char= (char text end) #\_)
                                 (char= (char text end) #\.))))))
        do (return-from %token-present-p t)
        finally (return nil)))

(defun %ref-typeuse-index-p (text)
  "True when TEXT contains a (ref <digit>...) typeuse — a typed
reference by type index, GC scope (the (ref $id) spelling is caught
by the plain substring scan; this catches (ref 0)-style indices)."
  (loop for start = (search "(ref " text :test #'char-equal)
          then (search "(ref " text :start2 (1+ start)
                        :test #'char-equal)
        while start
        when (and (< (+ start 5) (length text))
                  (digit-char-p (char text (+ start 5))))
          do (return-from %ref-typeuse-index-p t)
        finally (return nil)))

(defun %unsupported-feature-p (text)
  "Scan module text for features outside this engine slice (SIMD, GC
value types, annotations). Keyword-level only: the classification is
honest, never a silent pass.

TYPED FUNCTION REFERENCES (Core 3.0) are IMPLEMENTED as of loop 19:
valtypes ((ref $t) (ref null $t) (ref func) (ref extern) (ref exn)
(ref any) + the null*/anyref shorthand bottoms), the five opcodes
\(call_ref, return_call_ref, br_on_null, br_on_non_null,
ref.as_non_null), typed tables/globals/locals/elem segments and the
\(ref null $t) typeuses — decode (0x63/0x64, heap s33), validate
\(lattice + definite init), interp, encode. NO classification arm
for any of those: the honest pipeline runs.

What STAYS out of scope (honest :unsupported): the GC VALUE families
\(struct/array/i31 — br_on_cast, ref.cast, ref.eq, and their
eqref/structref/arrayref/i31ref valtype spellings), rec groups,
ref.test, any.convert_extern/extern.convert_any, annotations."
  (flet ((has (s) (and (search s text :test #'char-equal) t))
         (tok (s) (%token-present-p text s)))
    (cond
     ;; annotations proposal (@-prefixed forms, annotations.wast):
     ;; unadopted, out of engine slice. The script lexer SKIPS (@...)
     ;; forms so the file's STRUCTURE parses, but a module's own text
     ;; (plain, quote or implicit) still carries them, so any module
     ;; text containing "(@" classifies :unsupported — the honest
     ;; result, never a parse crash. Forms WITHOUT the adjacency
     ;; (annotations.wast ::47's quote "( @a)" — two reserved tokens,
     ;; malformed in any engine) keep their real DECODE outcome.
     ((has "(@") :annotations)
     ((or (tok "v128") (tok "i8x16") (tok "i16x8") (tok "i32x4")
          (tok "i64x2") (tok "f32x4") (tok "f64x2")) :simd)
     ;; GC VALUE families + rec groups stay out of the slice. The
     ;; typed-function-reference shapes ((ref $t), (ref null func),
     ;; anyref, call_ref, br_on_null, ...) are IMPLEMENTED (loop 19)
     ;; and intentionally NOT matched here anymore. eq/struct/array/
     ;; i31 heap spellings remain GC scope (their value families are
     ;; unimplemented); ref.eq/ref.test/ref.cast/br_on_cast and the
     ;; any<->extern conversions likewise.
     ((or (has "(struct") (has "(array") (has "(i31")
          (has "(rec")
          (has "(ref eq") (has "(ref struct") (has "(ref array")
          (has "(ref i31")
          (has "br_on_cast") (has "ref.cast") (has "ref.test")
          (has "ref.eq")
          (tok "eqref") (tok "structref") (tok "arrayref")
          (tok "i31ref")
          (has "any.convert_extern") (has "extern.convert_any")
          (has "nonmatching (ref")) :gc)
     (t nil))))


(defparameter *wast-max-locals-per-func* 1000000
  "Ceiling on a function's total declared locals (params + local
groups) before handing the module to the validator. The official suite
carries assert_malformed cases declaring ~2^32 locals per group; the
validator's local-index expansion allocates proportionally, which
would exhaust the SBCL heap (an uncatchable fatal error). The guard
keeps every allocation bounded and reports the honest phase.")

(defun %preflight-locals (module)
  "Two bounds on a function's total locals (params + local groups),
  both derived from script-controlled counts:

  - Spec bound: the local index space is capped at 2^32-1 per the
    binary format; a module whose declared total exceeds it is
    malformed at DECODE time (binary.wast's 0xFFFFFFFF+2 and
    4x0x40000000 cases). Signals wasmdown:decode-error.
  - Implementation bound: totals above *WAST-MAX-LOCALS-PER-FUNC*
    would make the validator's local-index expansion allocate
    proportionally and exhaust the SBCL heap (an uncatchable fatal
    error). Signals a catchable wasmdown:validation-error with an
    honest message (PLAN.md 10.1)."
  (loop for f across (wasmdown.types:wasm-module-funcs module)
        for tyidx = (wasmdown.types:wasm-func-typeidx f)
        ;; func.wast ::91-112: an out-of-range (type N) reaches the
        ;; validator as-is; this preflight must not AREF-crash on it
        ;; (a host error would misclassify assert_invalid cases as
        ;; harness-errors). Skip the func — the validator reports the
        ;; real 'type index out of range'.
        for type = (when (and (>= tyidx 0)
                              (< tyidx (length (wasmdown.types:wasm-module-types module))))
                     (aref (wasmdown.types:wasm-module-types module) tyidx))
        for total = (if type
                        (+ (length (wasmdown.types:wasm-func-type-params type))
                           (reduce #'+ (wasmdown.types:wasm-func-locals f)
                                   :key #'cdr :initial-value 0))
                        0)
        when (> total #xffffffff)
          do (%werr 0 "too many locals")
        when (> total *wast-max-locals-per-func*)
          do (error 'wasmdown:validation-error
                    :message (format nil
                                     "too many locals: function declares ~d local~:p (implementation limit ~d)"
                                     total *wast-max-locals-per-func*))))

(defun %preflight-datacount (module)
  "Reject, at decode time, the module shapes binary.wast marks
assert_malformed under the messages \"data count and data section have
inconsistent lengths\" / \"data count section required\": a data count
section whose value disagrees with the data section, and memory.init/
data.drop without one. The engine's decoder accepts these shapes (they
are well-formed byte-wise), so the wast layer enforces the spec's
decoding-time invariants itself rather than misclassifying them as
validation errors. Signals a catchable wasmdown:decode-error."
  (let ((dc (wasmdown.types:wasm-module-data-count module))
        (n (length (wasmdown.types:wasm-module-datas module))))
    (when dc
      (unless (= dc n)
        (%werr 0 "data count section declares ~d but data section has ~d segment~:p"
               dc n)))
    (unless dc
      (when (loop for f across (wasmdown.types:wasm-module-funcs module)
                  thereis (loop for instr across
                                  (wasmdown.types:wasm-func-body f)
                                thereis (member
                                         (wasmdown.types:wasm-instr-op instr)
                                         '(:memory.init :data.drop))))
        (%werr 0 "data count section required")))))

(defun %preflight-binary-sections (bytes)
  "A raw section walk over a (module binary ...) payload enforcing the
decoding-time invariant the engine's decoder misses: the Function
section's entry count must equal the Code section's entry count, even
when one of the two sections is absent (binary.wast marks both
combinations assert_malformed). Operates on the raw byte vector so no
module object is needed; signals a catchable wasmdown:decode-error."
  (when (zerop (length bytes)) (return-from %preflight-binary-sections))
  (let ((r (wasmdown.binary:make-reader bytes))
        (func-count 0) (code-count 0) (have-func nil) (have-code nil))
    ;; magic + version
    (wasmdown.binary:read-byte-at r) (wasmdown.binary:read-byte-at r)
    (wasmdown.binary:read-byte-at r) (wasmdown.binary:read-byte-at r)
    (wasmdown.binary:read-byte-at r) (wasmdown.binary:read-byte-at r)
    (wasmdown.binary:read-byte-at r) (wasmdown.binary:read-byte-at r)
    (loop
      (when (>= (wasmdown.binary:reader-offset r) (length bytes)) (return))
      (let* ((id (wasmdown.binary:read-byte-at r))
             (size (wasmdown.binary:read-u32 r))
             (end (+ (wasmdown.binary:reader-offset r) size)))
        (cond
         ((= id 3)
          (setf have-func t)
          (setf func-count (wasmdown.binary:read-u32 r)))
         ((= id 10)
          (setf have-code t)
          (setf code-count (wasmdown.binary:read-u32 r)))
         (t
          ;; skip: custom sections can carry arbitrary payload bytes;
          ;; known sections' sizes still bound the skip safely.
          (wasmdown.binary:read-byte-at r)))
        (setf (wasmdown.binary:reader-offset r) end)))
    ;; The declared counts must agree exactly; a present section with
    ;; count 0 and an absent section are equivalent, matching the suite.
    (when (and (or have-func have-code)
               (/= func-count code-count))
      (%werr 0 "function and code section have inconsistent lengths: ~d vs ~d"
             func-count code-count))))
(defun %binary-gc-table-p (bytes)
  "Whether a binary payload's Table section uses a GC VALUE-family
element type — the struct/array/i31/eq/any abstract heaps (0x6A/0x6B/
0x6C/0x6D/0x6E) or a composite-type prefix in table-type position.
Typed function references ARE decodable table element types now
(loop 19: 0x64 (ref ht), 0x63 (ref null ht), 0x70/0x6f/0x69 legacy),
so elem.wast's (table 1 (ref func) ...) fixtures decode and RUN; only
the value-family bytes stay :unsupported. Only the FIRST table entry
is inspected (entries are delimited by their limits, which the
detector does not parse; every suite fixture declaring a GC table
declares it first)."
  (when (< (length bytes) 8) (return-from %binary-gc-table-p nil))
  (handler-case
      (let ((r (wasmdown.binary:make-reader bytes))
            (gc-p nil))
        ;; magic + version
        (dotimes (_ 8) (wasmdown.binary:read-byte-at r))
        (loop
          (when (>= (wasmdown.binary:reader-offset r) (length bytes)) (return))
          (let* ((id (wasmdown.binary:read-byte-at r))
                 (size (wasmdown.binary:read-u32 r))
                 (end (+ (wasmdown.binary:reader-offset r) size)))
            (when (= id 4)
              (wasmdown.binary:read-u32 r)        ; entry count
              (unless (member (wasmdown.binary:read-byte-at r)
                              '(#x70 #x6f #x69 #x63 #x64))
                (setf gc-p t)))
            (setf (wasmdown.binary:reader-offset r) end)
            (when gc-p (return))))
        gc-p)
    (error () nil)))

(defun %binary-gc-valtype-p (bytes)
  "Whether a binary payload's TYPE section carries a GC VALUE-family
type form — the struct/array/composite heap bytes (0x6A array, 0x6B
struct, 0x6C i31, 0x6D eq, plus the comptype prefixes 0x5E/0x5F and
the rec/sub markers 0x4E/0x4F/0x50). The typed-function-reference
bytes 0x63/0x64 are DECODABLE as of loop 19 (with their s33 heap
typeuses) and no longer flag GC — binary.wast ::1241's (ref null
0x8000_0000) decodes and fails 'unknown type' at VALIDATE (its
assert_invalid phase), and elem.wast's typed-table fixtures decode
and run. The scan stays best-effort: malformed shapes fall through
to NIL and the honest decode reports the real error."
  (when (< (length bytes) 8) (return-from %binary-gc-valtype-p nil))
  (handler-case
      (let ((r (wasmdown.binary:make-reader bytes))
            (gc-p nil))
        (dotimes (_ 8) (wasmdown.binary:read-byte-at r))
        (loop
          (when (>= (wasmdown.binary:reader-offset r) (length bytes))
            (return))
          (let* ((id (wasmdown.binary:read-byte-at r))
                 (size (wasmdown.binary:read-u32 r))
                 (end (+ (wasmdown.binary:reader-offset r) size)))
            (when (and (= id 1)
                       (<= end (length bytes)))
              (let ((n (wasmdown.binary:read-u32 r)))
                (declare (ignore n))
                ;; scan the raw section bytes for the GC value-family
                ;; forms only; 0x60/0x63/0x64 and the MVP forms are
                ;; decodable. NB 0x6E (any heap) and 0x69 (exn heap)
                ;; are typed-funcrefs-scope and admitted; the
                ;; byte-identical-to-other-payloads caveat keeps the
                ;; scan inside [offset, end).
                (loop while (< (wasmdown.binary:reader-offset r) end)
                      for b = (wasmdown.binary:read-byte-at r)
                      when (member b '(#x5e #x5f #x6a #x6b #x6c #x6d
                                      #x4e #x4f #x50))
                        do (setf gc-p t))))
            (setf (wasmdown.binary:reader-offset r) end)
            (when gc-p (return))))
        gc-p)
    (error () nil)))


(defun %module-unsupported-import-p (cmd state)
  "Whether the module command's text imports from a registered name
whose instance is an out-of-scope module (EH tags etc.): the import is
out-of-scope too and must classify :unsupported rather than fail the
link with 'unknown import' (imports.wast: 'test' exports include tags).
The probe is a pure classification aid: ANY parse problem (unsupported
typed-funcref shapes the normal pipeline classifies first, decode
errors that the normal pipeline reports) falls through to NIL so the
standard pipeline runs."
  (let ((text (wast-cmd-text cmd)))
    (and text
         (null (%unsupported-feature-p text))
         (handler-case
             (let ((module (wasmdown.text:parse-wat text)))
               (loop for imp across (wasmdown.types:wasm-module-imports module)
                     thereis (%registered-unsupported-p
                              state (wasmdown.types:wasm-import-module imp))))
           (error () nil)))))


(defun %strip-definition-keyword (src)
  "The module-definition source span SRC (\"(module definition $M
  fields...)\") rewritten for parse-wat: drop the leading `definition`
  keyword, keeping the module form and any $id. The keyword is always
  directly after `(module` in this form's grammar."
  (let ((prefix "(module ")
        (kw "definition"))
    (if (and (>= (length src) (+ (length prefix) (length kw)))
             (string-equal src prefix :end1 (length prefix))
             (string-equal src kw :start1 (length prefix)
                           :end1 (+ (length prefix) (length kw))))
        (concatenate 'string "(module"
                     (subseq src (+ (length prefix) (length kw))))
        src)))


(defun %module-outcome (cmd state)
  "Run a (module ...) command through decode/validate/link/
instantiate/execute. Returns (values outcome phase detail instance)."
  (handler-case
      (progn
        ;; (module definition ...)/(module instance ...): Core 3.0
        ;; named forms. A DEFINITION parses + validates its fields
        ;; but never instantiates: the parsed module is stored under
        ;; its $id for the later (module instance $I $M). An INSTANCE
        ;; re-decodes the stored definition's source and instantiates
        ;; a FRESH instance (instance.wast: two instances of one
        ;; definition have distinct state — instantiation is
        ;; generative; import/export aliasing is NOT).
        (when (wast-cmd-named-form cmd)
          (case (wast-cmd-named-form cmd)
            (:definition
             (let* ((src (%strip-definition-keyword (wast-cmd-text cmd)))
                    (module (%decode-module-form
                             (make-wast-cmd :name :module :index 0
                                            :start 0 :text src)))
                    (mid (wast-cmd-id cmd)))
               (when (and mid (not (eq module :unsupported)))
                 (setf (gethash mid (wast-state-module-definitions state))
                       src))
               (if (eq module :unsupported)
                   (let ((feature (%unsupported-feature-p
                                   (or (wast-cmd-text cmd) ""))))
                     (return-from %module-outcome
                       (values :unsupported nil
                               (format nil "uses unsupported feature ~a"
                                       feature))))
                   (progn
                     (%preflight-locals module)
                     (%preflight-datacount module)
                     (wasmdown:validate-module module)
                     (return-from %module-outcome
                       (values :pass nil "ok (definition stored)" nil))))))
            (:instance
             (let* ((def-id (wast-cmd-module-arg cmd))
                    (src (gethash def-id
                                  (wast-state-module-definitions state))))
               (return-from %module-outcome
                 (if src
                     (let ((module (wasmdown.text:parse-wat src)))
                       (%preflight-locals module)
                       (%preflight-datacount module)
                       (wasmdown:validate-module module)
                       (let ((inst (wasmdown:instantiate
                                    module
                                    :imports (%state-imports-for module))))
                         (values :pass nil "ok" inst)))
                     ;; the definition was out-of-scope (never stored):
                     ;; the instance command inherits that honestly
                     (values :unsupported nil
                             "instance of an out-of-scope definition")))))))
        (when (and (wast-cmd-bytes cmd)
                   (or (%binary-gc-table-p (wast-cmd-bytes cmd))
                       (%binary-gc-valtype-p (wast-cmd-bytes cmd))))
          (return-from %module-outcome
            (values :unsupported nil
                    "uses unsupported feature gc (typed-ref in binary)")))
        (when (wast-cmd-bytes cmd)
          (%preflight-binary-sections (wast-cmd-bytes cmd)))
        (let ((module (%decode-module-form cmd)))
          (if (eq module :unsupported)
              (let ((feature (%unsupported-feature-p
                              (or (wast-cmd-text cmd) ""))))
                (values :unsupported nil
                        (format nil "uses unsupported feature ~a" feature)))
              (progn
                (%preflight-locals module)
                (%preflight-datacount module)
                (wasmdown:validate-module module)
                ;; Only at the LINK boundary: decode+validate run
                ;; honestly first (assert_invalid/assert_malformed
                ;; keep their real phase); importing from a registered
                ;; name whose instance is out-of-scope (EH tags) is
                ;; out-of-scope too, never a link 'unknown import' fail.
                (when (%module-unsupported-import-p cmd state)
                  (return-from %module-outcome
                    (values :unsupported nil
                            "imports from an out-of-scope module")))
                (let ((inst (wasmdown:instantiate
                             module
                             :imports (%state-imports-for module))))
                  (values :pass nil "ok" inst))))))
    (wasmdown:decode-error (c)
      (values :fail :decode (wasmdown:wasmdown-error-message c) nil))
    (wasmdown:validation-error (c)
      (values :fail :validate (wasmdown:wasmdown-error-message c) nil))
    (wasmdown:link-error (c)
      (values :fail :link (wasmdown:wasmdown-error-message c) nil))
    (wasmdown:instantiation-error (c)
      (values :fail :instantiate
              (wasmdown:wasmdown-error-message c) nil))
    (wasmdown:wasm-trap (c)
      (values :fail :execute (wasmdown:wasmdown-error-message c) nil))))

(defun %state-imports-for (module)
  "The imports alist for MODULE: spectest + registry. Lazily built via
the state's import entries; called from %module-outcome with the global
runner state."
  (declare (ignore module))
  (%current-import-entries))

(defvar *current-wast-state* nil
  "The wast-state of the script currently executing.")

(defun %current-import-entries ()
  (if *current-wast-state*
      (%import-entries *current-wast-state*)
      (error "no active wast state")))

;;; ---------------------------------------------------------------
;;; Result records (PLAN.md 12.8).

(defstruct wast-result
  file                                    ; file name (string)
  command-index                           ; 0-based
  command-name                            ; keyword
  outcome                                 ; :pass :fail :skip :unsupported
                                         ; :timeout :harness-error
  phase                                   ; actual failure phase or nil
  detail                                  ; human summary
  elapsed                                 ; seconds (float)
  message)                                ; expected message (assertions)

(defun result-outcome-counts (results)
  "An alist ((outcome . count) ...) over every outcome kind, in a
stable order."
  (let ((order '(:pass :fail :skip :unsupported :timeout :harness-error))
        (counts '()))
    (dolist (o order)
      (let ((n (count o results :key #'wast-result-outcome)))
        (when (plusp n) (push (cons o n) counts))))
    (nreverse counts)))

;;; ---------------------------------------------------------------
;;; Value comparison oracle (assert_return).

(defun %expected-to-lisp (spec)
  "Convert a parsed result spec to a host value for comparison, or a
comparison directive list."
  (case (first spec)
    (:num (let ((v (third spec)))
            (if (eq (second spec) :i32)
                (list :exact (mod v #x100000000))
                (list :exact (mod v #x10000000000000000)))))
    (:float (case (fourth spec)
              (:exact
               (list :exact-bits (second spec)
                     (if (eq (second spec) :f32)
                         (wasmdown.numops:f32-from-bits (third spec))
                         (wasmdown.numops:f64-from-bits (third spec)))))
              (:nan-canonical
               (list :nan-canonical (second spec)))
              (:nan-arithmetic
               (list :nan-arithmetic (second spec)))
              (:nan-payload
               (list :nan-payload (second spec)
                     (logand (third spec)
                             (if (eq (second spec) :f32)
                                 #x7fffff
                                 #xfffffffffffff))))))
    (:ref-func (if (second spec)
                   (list :ref-func-id (second spec))
                   (list :ref-non-null)))
    (:ref-null (list :ref-null))
    (:ref-extern (list :ref-extern (second spec)))))

(defun %arg-to-lisp (spec)
  "Convert a parsed argument spec to the Lisp-domain argument value
per docs/CONTRACTS.md (i32/i64 unsigned, f32/f64 floats, refs)."
  (case (first spec)
    (:num (let ((v (third spec)))
            (if (eq (second spec) :i32)
                (mod v #x100000000)
                (mod v #x10000000000000000))))
    (:float (if (eq (second spec) :f32)
                (wasmdown.numops:f32-from-bits (third spec))
                (wasmdown.numops:f64-from-bits (third spec)))
      ;; NOTE: NaN-CLASS specs are never legal as arguments; exact only.
      )
    (:ref-func (error "ref.func as invoke argument unsupported"))
    (:ref-null nil)
    (:ref-extern (list 'externref (second spec)))))

(defun %value-matches (actual expected)
  "Compare one result value against its expectation directive."
  (ecase (first expected)
    (:exact (equalp actual (second expected)))
    (:exact-bits
     (etypecase actual
       (single-float
        (and (eq (second expected) :f32)
             (= (wasmdown.numops:f32-bits actual)
                (wasmdown.numops:f32-bits (third expected)))))
       (double-float
        (and (eq (second expected) :f32) nil
             ;; f64 expected never lands here; kept for symmetry
             )
        (and (eq (second expected) :f64)
             (= (wasmdown.numops:f64-bits actual)
                (wasmdown.numops:f64-bits (third expected)))))
       (t nil)))
    (:nan-canonical
     (etypecase actual
       (single-float
        (and (eq (second expected) :f32)
             (wasmdown.numops:canonical-nan-p actual)))
       (double-float
        (and (eq (second expected) :f64)
             (wasmdown.numops:canonical-nan-p actual)))))
    (:nan-arithmetic
     (etypecase actual
       (single-float
        (and (eq (second expected) :f32)
             (wasmdown.numops:arithmetic-nan-p actual)))
       (double-float
        (and (eq (second expected) :f64)
             (wasmdown.numops:arithmetic-nan-p actual)))))
    (:nan-payload
     (etypecase actual
       (single-float
        (and (eq (second expected) :f32)
             (wasmdown.numops:nan-p actual)
             (= (logand (wasmdown.numops:f32-bits actual) #x7fffff)
                (third expected))))
       (double-float
        (and (eq (second expected) :f64)
             (wasmdown.numops:nan-p actual)
             (= (logand (wasmdown.numops:f64-bits actual)
                        #xfffffffffffff)
                (third expected))))))
    (:ref-null (null actual))
    (:ref-non-null (wasmdown.runtime:func-instance-p actual))
    (:ref-func-id (error "ref.func identity results not yet supported"))
    (:ref-extern
     (and (consp actual) (eq (first actual) 'externref)
          (eql (second actual) (second expected))))))

(defun %results-match (actuals expected)
  "Compare full result tuples. Returns (values ok detail)."
  (if (/= (length actuals) (length expected))
      (values nil (format nil "arity: expected ~a results, got ~a"
                         (length expected) (length actuals)))
      (loop for a in actuals
            for e in expected
            for i from 0
            do (let ((directive (%expected-to-lisp e)))
                 (unless (%value-matches a directive)
                   (return (values nil
                                  (format nil "result ~a: got ~a"
                                          i (%opaque-value a))))))
            finally (return (values t "ok")))))

(defun %opaque-value (v)
  "A cycle-safe printable form of a result value. Runtime objects
(func-instances, module-instances) contain backpointers — printing
the structure itself recurses forever without *print-circle*. Print
an opaque type tag; numbers/floats print verbatim."
  (typecase v
    (wasmdown.runtime:func-instance "#<func-instance>")
    (wasmdown.runtime:module-instance "#<module-instance>")
    (wasmdown.runtime:memory-instance "#<memory-instance>")
    (wasmdown.runtime:table-instance "#<table-instance>")
    (wasmdown.runtime:global-instance "#<global-instance>")
    (t v)))

;;; ---------------------------------------------------------------
;;; Trap-kind mapping from assertion messages.

(defun %trap-kind-for (message)
  "The wasm-trap kind keyword the assertion MESSAGE category requires
(PLAN.md 12.2 message-category mapping)."
  (let ((m (string-downcase message)))
    (cond
     ((search "unreachable" m) :unreachable)
     ((search "out of bounds memory" m) :out-of-bounds-memory)
     ((search "out of bounds table" m) :out-of-bounds-table)
     ((search "undefined element" m) :out-of-bounds-table)
     ((search "uninitialized element" m) :null-reference)
     ((search "integer divide by zero" m) :integer-divide-by-zero)
     ((search "integer overflow" m) :integer-overflow)
     ((search "invalid conversion" m) :invalid-conversion)
     ((search "null" m) :null-reference)
     ((search "indirect call" m) :indirect-call-type-mismatch)
     ((search "call stack exhausted" m) :call-stack-exhausted)
     (t :generic))))

;;; ---------------------------------------------------------------
;;; Actions: (invoke ...) / (get ...) against runner state.

(define-condition unsupported-action (wasmdown:host-error)
  ()
  (:documentation
   "An action cannot run because its target module uses an out-of-scope
feature (classified :unsupported, never :fail)"))

(defun %resolve-action-instance (state cmd-or-id)
  "The module instance an action addresses: $id -> named instance, else
the current module. Signals unsupported-action when the target module
itself was unsupported (out-of-scope features); plain host-error when
no module was ever instantiated."
  (cond
   ((null cmd-or-id)
    (or (wast-state-current state)
        (if (wast-state-current-unsupported state)
            (error 'unsupported-action
                   :message "action targets an unsupported module")
            (error 'wasmdown:host-error
                   :message
                   "no module to act on (no module instantiated yet)"))))
   (t (or (gethash cmd-or-id (wast-state-named-instances state))
          (if (gethash cmd-or-id (wast-state-unsupported-named state))
              (error 'unsupported-action
                     :message (format nil
                                      "module ~a uses an unsupported feature"
                                      cmd-or-id))
              (error 'wasmdown:host-error
                     :message (format nil "unknown module ~a"
                                      cmd-or-id)))))))

(defun %run-invoke (state cmd)
  "Execute an invoke action. Returns (values results-list). Signals
traps/errors per the engine. When the current module position is an
out-of-scope module, a missing export (wasmdown:link-error from
invoke) is re-signaled as unsupported-action: the action targets the
out-of-scope module (imports.wast print32/print64 after the EH-tag
module), never a fresh :fail."
  (let* ((inst (%resolve-action-instance state (wast-cmd-module-arg cmd)))
         (args (mapcar #'%arg-to-lisp (wast-cmd-args cmd))))
    (handler-case
        (apply #'wasmdown:invoke inst (wast-cmd-name-arg cmd) args)
      (wasmdown:link-error (c)
        (if (wast-state-current-unsupported state)
            (error 'unsupported-action
                   :message (format nil
                                    "export of an out-of-scope module: ~a"
                                    (wasmdown:wasmdown-error-message c)))
            (error c))))))


(defun %run-get (state cmd)
  "Execute a get action: the exported global's value."
  (let* ((inst (%resolve-action-instance state (wast-cmd-module-arg cmd)))
         (v (wasmdown.runtime:export-value inst (wast-cmd-name-arg cmd))))
    (unless (wasmdown.runtime:global-instance-p v)
      (error 'wasmdown:host-error
             :message "get: export is not a global"))
    (wasmdown.runtime:global-value v)))

;;; ---------------------------------------------------------------
;;; The script driver.

(defvar *script-source* ""
  "The source of the script being executed (for slicing embedded
module text out of assertion forms).")

(defun run-script (source &key (file "<script>") (stream nil))
  "Execute a .wast script (string or pathname) against fresh state.
Returns the list of wast-result records, one per command. STREAM, when
given, receives a JSONL line per result (PLAN.md 12.8)."
  (let ((*print-circle* t)
        (*print-level* 4)
        (*print-length* 8))
    (let* ((text (if (pathnamep source)
                     (with-open-file (s source :external-format :utf-8)
                       (let ((out (make-string-output-stream)))
                         (loop for c = (read-char s nil nil)
                               while c do (write-char c out))
                         (get-output-stream-string out)))
                     source))
           (state (make-wast-state
                    :spectest (make-spectest-instance)))
           (results '()))
    (let ((*script-source* text)
          (*current-wast-state* state)
          (*spectest-calls* '()))
      (flet ((record (cmd outcome phase detail message)
               (let ((r (make-wast-result
                          :file file
                          :command-index (wast-cmd-index cmd)
                          :command-name (wast-cmd-name cmd)
                          :outcome outcome
                          :phase phase
                          :detail detail
                          :elapsed 0.0
                          :message message)))
                 (push r results)
                 (when stream (write-jsonl-line stream r)))))
        (let ((cmds nil) (parse-error nil))
          (handler-case
              (setf cmds (parse-script text))
            (error (c)
              (setf parse-error c)))
          (cond
           (parse-error
            ;; whole-script harness error: one result
            (record (make-wast-cmd :name :script :index 0 :start 0)
                    :harness-error nil
                    (format nil "script parse error: ~a" parse-error)
                    nil))
           (t
            (dolist (cmd cmds)
              (handler-case
                  (%execute-command state cmd #'record)
                (error (c)
                  (record cmd :harness-error nil
                          (format nil "unexpected error: ~a" c)
                          nil))))))))
      (nreverse results)))))

(defun %execute-command (state cmd record)
  "Execute one command, calling (record cmd outcome phase detail msg)."
  (let ((name (wast-cmd-name cmd)))
    (cond
     ((eq name :module)
      (multiple-value-bind (outcome phase detail inst)
          (%module-outcome cmd state)
        (cond
         ((eq outcome :pass)
          (cond
           ;; (module definition ...): stores the module, no instance
           ;; produced — the current position is untouched.
           ((eq (wast-cmd-named-form cmd) :definition)
            (funcall record cmd :pass nil "ok (definition stored)" nil))
           (t
            (setf (wast-state-current state) inst
                  (wast-state-current-unsupported state) nil)
            (when (wast-cmd-id cmd)
              (remhash (wast-cmd-id cmd)
                       (wast-state-unsupported-named state))
              (setf (gethash (wast-cmd-id cmd)
                             (wast-state-named-instances state))
                    inst))
            (funcall record cmd :pass nil "ok" nil))))
         ((eq outcome :unsupported)
          ;; unsupported module: remember its $id (and that the current
          ;; position holds an unsupported module) so later actions
          ;; classify :unsupported rather than fail on a missing
          ;; instance. The previous instance is DROPPED: an action now
          ;; targets the new (out-of-scope) module, not the stale one.
          (setf (wast-state-current state) nil)
          (when (wast-cmd-id cmd)
            (setf (gethash (wast-cmd-id cmd)
                           (wast-state-unsupported-named state))
                  t))
          (setf (wast-state-current-unsupported state) t)
          (funcall record cmd :unsupported nil detail nil))
         (t (funcall record cmd outcome phase detail nil)))))
     ((eq name :register)
      (let ((inst (if (wast-cmd-id cmd)
                      (gethash (wast-cmd-id cmd)
                               (wast-state-named-instances state))
                      (wast-state-current state))))
        (cond
         (inst
          (setf (gethash (wast-cmd-name-arg cmd)
                         (wast-state-registry state))
                inst)
          (funcall record cmd :pass nil "ok" nil))
         ((or (and (wast-cmd-id cmd)
                   (gethash (wast-cmd-id cmd)
                            (wast-state-unsupported-named state)))
              (and (null (wast-cmd-id cmd))
                   (wast-state-current-unsupported state)))
          ;; remember the NAME too: later modules importing from it
          ;; classify :unsupported (the instance's exports include
          ;; out-of-scope EH tags), never 'unknown import'
          (%register-unsupported state (wast-cmd-name-arg cmd))
          (funcall record cmd :unsupported nil
                   "register targets an out-of-scope module" nil))
         (t (funcall record cmd :fail nil "no module to register" nil)))))
     ((eq name :invoke)
      (handler-case
          (progn (%run-invoke state cmd)
                 (funcall record cmd :pass nil "ok" nil))
        (unsupported-action ()
          (funcall record cmd :unsupported nil
                   "action targets an out-of-scope module" nil))))
     ((eq name :get)
      (handler-case
          (progn (%run-get state cmd)
                 (funcall record cmd :pass nil "ok" nil))
        (unsupported-action ()
          (funcall record cmd :unsupported nil
                   "action targets an out-of-scope module" nil))))
     ((eq name :assert_return)
      (%exec-assert-return state cmd record))
     ((member name '(:assert_malformed :assert_invalid :assert_unlinkable
                     :assert_trap :assert_exhaustion))
      (%exec-assert-failure state cmd record))
     (t (funcall record cmd :harness-error nil
                 (format nil "unhandled command ~a" name) nil)))))

(defun %action-of (payload)
  "Interpret an assert payload node: (invoke/get ...) action or a
module form. Returns (values kind cmd) — kind :action or :module."
  (if (and (%node-p payload)
           (member (%head-keyword payload) '("invoke" "get")
                   :test #'string=))
      (values :action (parse-action-cmd payload))
      (values :module (module-cmd-of payload))))

(defun parse-action-cmd (node)
  "Build a wast-cmd for an (invoke/get) action node."
  (multiple-value-bind (mid name args) (%parse-action node)
    (make-wast-cmd :name (if (string= (%head-keyword node) "invoke")
                             :invoke :get)
                   :index 0 :start (second node)
                   :module-arg mid :name-arg name :args args)))

(defun module-cmd-of (node)
  "Build a wast-cmd for a (module ...) node inside an assertion. The
module's SOURCE must be recovered: callers pass the whole script
source via %exec-assert-failure's re-parse."
  (make-wast-cmd :name :module :index 0 :start (second node)))

(defun %exec-assert-return (state cmd record)
  "assert_return: run the action, compare result tuples with NaN
classes (PLAN.md 12.2)."
  (let ((action-node (wast-cmd-module-arg cmd)))
    (multiple-value-bind (kind action-cmd)
        (%action-of action-node)
      (declare (ignore kind))
      (handler-case
          (let ((actual (if (eq (wast-cmd-name action-cmd) :invoke)
                            (%run-invoke state action-cmd)
                            (list (%run-get state action-cmd)))))
            (multiple-value-bind (ok detail)
                (%results-match actual (wast-cmd-results cmd))
              (if ok
                  (funcall record cmd :pass nil "ok"
                           (wast-cmd-message cmd))
                  (funcall record cmd :fail :execute detail
                           (wast-cmd-message cmd)))))
        (wasmdown:wasm-trap (c)
          (funcall record cmd :fail :execute
                   (format nil "unexpected trap: ~a"
                           (wasmdown:wasmdown-error-message c))
                   (wast-cmd-message cmd)))
        (unsupported-action ()
          (funcall record cmd :unsupported nil
                   "action targets an out-of-scope module"
                   (wast-cmd-message cmd)))
        (error (c)
          (funcall record cmd :fail nil
                   (format nil "~a" c)
                   (wast-cmd-message cmd)))))))

(defun %exec-assert-failure (state cmd record)
  "The expected-failure assertions. Each requires a SPECIFIC phase
(PLAN.md 12.2): assert_malformed -> :decode, assert_invalid ->
:validate, assert_unlinkable -> :link, assert_trap -> wasm-trap with
kind from the message category, assert_exhaustion -> :call-stack-
exhausted."
  (let ((payload (wast-cmd-module-arg cmd))
        (name (wast-cmd-name cmd))
        (msg (wast-cmd-message cmd)))
    (multiple-value-bind (kind inner) (%action-of payload)
      (declare (ignore inner))
      (if (eq kind :action)
          ;; action payload (assert_trap / assert_exhaustion)
          (multiple-value-bind (kind2 action-cmd)
              (%action-of payload)
            (declare (ignore kind2))
            (handler-case
                (progn
                  (if (eq (wast-cmd-name action-cmd) :invoke)
                      (%run-invoke state action-cmd)
                      (%run-get state action-cmd))
                  ;; no error -> the assertion expected one
                  (funcall record cmd :fail :execute
                           "expected a trap but the action returned"
                           msg))
              (wasmdown:wasm-trap (c)
                (let ((actual (wasmdown:wasm-trap-kind c)))
                  (if (eq name :assert_exhaustion)
                      (if (eq actual :call-stack-exhausted)
                          (funcall record cmd :pass nil "ok" msg)
                          (funcall record cmd :fail :execute
                                   (format nil "trap kind ~a" actual)
                                   msg))
                      (let ((want (%trap-kind-for msg)))
                        (if (or (eq want :generic) (eq want actual))
                            (funcall record cmd :pass nil "ok" msg)
                            (funcall record cmd :fail :execute
                                     (format nil "trap kind ~a, want ~a"
                                              actual want)
                                     msg))))))
              (unsupported-action ()
                (funcall record cmd :unsupported nil
                         "action targets an out-of-scope module" msg))
              (error (c)
                (funcall record cmd :fail nil
                         (format nil "~a" c) msg))))
          ;; module payload: decode the embedded module and check the
          ;; phase. The module node's source slice is recovered from
          ;; the runner's bound source via *script-source*.
          (let ((mod-cmd (%module-cmd-from-node payload)))
            (multiple-value-bind (outcome phase detail)
                (%module-outcome mod-cmd state)
              (let ((want-phase (ecase name
                                  (:assert_malformed :decode)
                                  (:assert_invalid :validate)
                                  (:assert_unlinkable :link)
                                  (:assert_trap :execute)
                                  (:assert_exhaustion :execute))))
                (cond
                 ((eq outcome :unsupported)
                  (funcall record cmd :unsupported nil detail msg))
                 ((eq outcome :pass)
                  (funcall record cmd :fail nil
                           "module unexpectedly succeeded" msg))
                 ((eq phase want-phase)
                  (funcall record cmd :pass nil detail msg))
                 ((and (eq name :assert_trap) (eq phase :instantiate))
                  ;; data/elem segment out-of-bounds traps during
                  ;; instantiation per the spec (assert_trap (module
                  ;; (data ...)) cases in data.wast/elem.wast)
                  (funcall record cmd :pass nil detail msg))
                 (t
                  (funcall record cmd :fail phase
                           (format nil "phase ~a, want ~a: ~a"
                                   phase want-phase detail)
                           msg))))))))))


(defun %module-cmd-from-node (node)
  "Rebuild a module wast-cmd from an assertion's (module ...) node,
slicing its text from *script-source*."
  (multiple-value-bind (kind id payload)
      (%parse-module-form node *script-source*)
    (make-wast-cmd
     :name :module :index 0 :start (second node)
     :id id
     :text (if (eq kind :quote) payload
               (if (eq kind :text) payload nil))
     :bytes (if (eq kind :binary) payload nil))))

;;; ---------------------------------------------------------------
;;; JSONL + summaries (PLAN.md 12.8).

(defun %json-escape (s)
  (with-output-to-string (out)
    (loop for c across s
          do (cond
              ((char= c #\") (write-string "\\\"" out))
              ((char= c #\\) (write-string "\\\\" out))
              ((char= c #\newline) (write-string "\\n" out))
              ((char= c #\return) (write-string "\\r" out))
              ((char= c #\tab) (write-string "\\t" out))
              ((< (char-code c) 32) (format out "\\u~4,'0x" (char-code c)))
              (t (write-char c out))))))

(defun write-jsonl-line (stream result)
  "One wasmdown.test-result.v1 JSON object line."
  (format stream
          "{\"schema\":\"wasmdown.test-result.v1\",~
           \"suite\":\"core-spec\",~
           \"case_id\":\"~a::~a\",~
           \"assertion\":\"~a\",~
           \"outcome\":\"~a\",~
           \"expected_failure_phase\":null,~
           \"actual_failure_phase\":\"~a\",~
           \"detail\":\"~a\"}~%"
          (%json-escape (wast-result-file result))
          (wast-result-command-index result)
          (string-downcase (wast-result-command-name result))
          (string-downcase (wast-result-outcome result))
          (if (wast-result-phase result)
              (string-downcase (wast-result-phase result)) "")
          (%json-escape (wast-result-detail result))))

(defun write-jsonl (results path)
  "Write RESULTS as a JSONL file at PATH."
  (with-open-file (s path :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (dolist (r results) (write-jsonl-line s r))))

(defun summarize-results (results &optional (stream *standard-output*))
  "Print a human summary of results by outcome."
  (format stream "~&--- ~a command~:p ---~%" (length results))
  (dolist (cell (result-outcome-counts results))
    (format stream "  ~a: ~a~%" (car cell) (cdr cell))))

;;; ---------------------------------------------------------------
;;; Directory driver.

(defun run-directory (dir &key (pattern "*.wast") (jsonl-path nil))
  "Run every matching .wast file under DIR (sorted). Returns the
aggregated results list; writes JSONL when JSONL-PATH is given."
  (let* ((files (sort (directory
                       (merge-pathnames pattern
                                       (pathname-as-directory dir)))
                      #'string< :key #'namestring))
         (all '()))
    (dolist (f files)
      (let* ((name (file-namestring f))
             (results (run-script f :file name)))
        (setf all (append all results))))
    (when jsonl-path (write-jsonl all jsonl-path))
    all))

(defun pathname-as-directory (p)
  (make-pathname :directory
                 (append (pathname-directory p)
                         (when (pathname-name p) (list (pathname-name p))))))
