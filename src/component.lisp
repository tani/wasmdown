;;;; WASMDOWN — the Component Model binary layer (PLAN.md §4: the
;;;; wasmdown.component subsystem; M7 slice 1).
;;;;
;;;; This file owns the COMPONENT side of layer discrimination and the
;;;; component binary reader per the pinned design doc
;;;; specs/Binary.md (component-model commit d095e2f60d, 2026-10-24):
;;;;   * the preamble: magic `\00asm`, VERSION 0x0D 0x00 (pre-std),
;;;;     LAYER 0x01 0x00 (Binary.md lines 22-27);
;;;;   * the section table (Binary.md lines 28-40): custom 0,
;;;;     core-module 1, core-instance 2, core-type 3, component 4,
;;;;     instance 5, alias 6, type 7, canon 8, start 9, import 10,
;;;;     export 11, value 12;
;;;;   * the WIT value-type system (Binary.md lines 188-250): the
;;;;     primitive bytes, record/variant/list/tuple/flags/enum/option/
;;;;     result/own/borrow/stream/future/map + the functype,
;;;;     resource-type, component-type and instance-type forms;
;;;;   * the structural sections (Binary.md lines 59-135, 300-412):
;;;;     core:instance/instance exprs, aliases (export / core export /
;;;;     outer), canon definitions, start, import/export with
;;;;     externnames + attributes.
;;;;
;;;; Decoder discipline mirrors src/binary.lisp (PLAN.md §5.1): every
;;;; read is bounds-checked through the shared wasmdown.binary reader,
;;;; counts are checked against the remaining input before any
;;;; allocation, offsets in decode-error conditions are absolute, and
;;;; DECODE (malformed) is kept strictly separate from VALIDATE
;;;; (well-formed but invalid) so the .wast classifier can attribute
;;;; assert_malformed vs assert_invalid honestly.

(defpackage #:wasmdown.component
  (:use #:cl #:wasmdown.types #:wasmdown.binary)
  (:export #:component-p
           #:decode-component
           #:component-sections
           #:component-types
           #:component-core-modules
           #:component-core-instances
           #:component-core-types
           #:component-instances
           #:component-components
           #:component-aliases
           #:component-canons
           #:component-imports
           #:component-exports
           #:component-start
           #:component-values
           #:component-customs
           ;; component type AST accessors (shared with the validator)
           #:ct-kind
           #:ct-payload
           #:ct-offset
           #:component-type-p
           #:make-ct
           ;; value type predicates used by the validator
           #:ct-primitive-p
           #:ct-defvaltype-p
           #:ct-functype-p
           #:component-validate
           #:validate-component
           ;; labels + externnames
           #:label-p
           #:externname-kind
           #:parse-externname
           #:canonicalize-externname
           #:elem-size))

(in-package #:wasmdown.component)

;;; ---------------------------------------------------------------
;;; Decoding error helpers (shared reader offsets; wasmdown:decode-error
;;; carries the absolute :offset, wasmdown:validation-error does not).

(defun %derr (r control &rest args)
  "Signal decode-error at R's current offset."
  (apply #'decode-error-at (reader-offset r) control args))

(defun %verr (control &rest args)
  "Signal wasmdown:validation-error."
  (error 'wasmdown:validation-error
         :message (apply #'format nil control args)))

;;; ---------------------------------------------------------------
;;; Labels and externnames (Explainer.md lines 2753-2794). Used at
;;; VALIDATE time: the binary reader accepts any UTF-8 name; kebab
;;; shape, version grammar and strongly-unique conflicts are static
;;; validation rules (the fixtures pin them as assert_invalid).

(defun %digit-p (c) (and (char<= #\0 c) (char<= c #\9)))

(defun %frag-range-p (s start end first-p)
  "One `fragment` (Explainer line 2775): a word [0-9a-z]+ or an acronym
[0-9A-Z]+ — a digits-only fragment is a word. The FIRST fragment of a
label (Explainer lines 2770-2774, first-word/first-acronym) must start
with a letter: [a-z][0-9a-z]* or [A-Z][0-9A-Z]*. kebab.wast pins this:
`a1`, `B1`, `B-1-C-2-D-3` and `a11-B11-123-ABC-abc` are all labels,
while `1` and `1-a` are NOT ('is not in kebab case')."
  (declare (type string s) (type fixnum start end))
  (when (<= end start) (return-from %frag-range-p nil))
  (let ((c0 (char s start)) (i (1+ start)))
    (cond
     ((and (char<= #\a c0) (char<= c0 #\z))
      (loop while (< i end)
            always (let ((c (char s i)))
                     (or (and (char<= #\a c) (char<= c #\z)) (%digit-p c)))
            do (incf i)))
     ((and (char<= #\A c0) (char<= c0 #\Z))
      (loop while (< i end)
            always (let ((c (char s i)))
                     (or (and (char<= #\A c) (char<= c #\Z)) (%digit-p c)))
            do (incf i)))
     ;; digits-only: legal for NON-first fragments only (`a-1`)
     ((and (not first-p) (%digit-p c0))
      (loop while (< i end) always (%digit-p (char s i)) do (incf i)))
     (t nil))))

(defun label-p (s)
  "A `label` (Explainer line 2770): first-fragment ('-' fragment)*,
no empty fragments (`a-`, `a--`, empty are all rejected)."
  (and (stringp s) (plusp (length s))
       (let ((start 0) (first t) (ok t))
         (declare (type fixnum start))
         (loop for i from 0 to (length s)
               while ok
               when (or (= i (length s)) (char= (char s i) #\-))
                 do (cond ((and (< start i)
                                (%frag-range-p s start i first))
                           (setf start (1+ i) first nil))
                          (t (setf ok nil))))
         ok)))

;;; ---------------------------------------------------------------
;;; The component IR. A component is a FLAT vector of definitions in
;;; file order (Binary.md line 23: `(component flatten(s*))` — sections
;;; may repeat in any order; each appends to its index space), plus the
;;; custom sections. DECODE produces this IR; VALIDATE walks it.

(defstruct comp
  (defs #() :type vector)                ; flat definition sequence
  (customs #() :type vector))            ; (name . bytes)

;;; One definition. KIND is one of :core-module :core-instance :core-type
;;; :component :instance :alias :type :canon :start :import :export
;;; :value; PAYLOAD is the kind-specific record; OFFSET is the absolute
;;; byte of the definition's first byte (diagnostics).
(defstruct compdef
  (kind nil :type keyword)
  (payload nil)
  (offset 0 :type (integer 0)))

;;; ---- sorts & sortidx ----
;;; Component sorts: :func :value :type :component :instance
;;; Core sorts: :core-func :core-table :core-memory :core-global
;;;             :core-tag :core-type :core-module :core-instance
;;; sortidx = (sort . idx)

(defun sortidx-sort (si) (car si))
(defun sortidx-idx (si) (cdr si))

(defun make-sortidx (sort idx) (cons sort idx))

;;; ---- instances ----
;;; :instantiate  payload: (component-idx . vector-of (name . sortidx))
;;; :inline-exports payload: vector-of (externname-attrs . sortidx)
(defstruct instrec
  (kind :instantiate :type keyword)
  (index 0 :type (integer 0))            ; component/module index
  (args #() :type vector)                ; instantiate: (name . sortidx)
  (exports #() :type vector))            ; inline: (name . sortidx)

(defstruct coreinstrec
  (kind :instantiate :type keyword)
  (index 0 :type (integer 0))            ; module index
  (args #() :type vector)                ; (core-name . core-instance-idx)
  (exports #() :type vector))            ; (core-name . sortidx)

;;; ---- aliases ----
;;; kind :export   -> (sort . idx) instance, name
;;; kind :core-export -> (sort . idx) core instance, name
;;; kind :outer    -> (sort) count, idx
(defstruct aliasrec
  (kind :export :type keyword)
  (sort nil :type symbol)                ; the ALIASED sort
  (index 0 :type (integer 0))            ; instance idx (export aliases)
  (count 0 :type (integer 0))            ; outer de Bruijn count
  (target 0 :type (integer 0))           ; outer target index
  (name "" :type string)
  (offset 0 :type (integer 0)))

;;; ---- component types (the WIT value-type system) ----
;;; A `ct` is one component type. KIND + payload:
;;;   (:prim p)                  p in {:bool :s8 :u8 :s16 :u16 :s32 :u32
;;;                                    :s64 :u64 :f32 :f64 :char :string
;;;                                    :error-context}
;;;   (:record fields)           vector of (label . valtype)
;;;   (:variant cases)           vector of (label . payload-or-nil)
;;;   (:list elem)               elem = (valtype . fixed-len-or-nil)
;;;   (:tuple types)             vector of valtypes
;;;   (:flags labels)            vector of labels
;;;   (:enum labels)             vector of labels
;;;   (:option type)
;;;   (:result ok err)           ok/err are valtypes or NIL
;;;   (:own idx) (:borrow idx)   resource handles
;;;   (:stream type?) (:future type?)
;;;   (:map key val)             key/val are valtypes
;;;   (:resource rep dtor)       rep = core valtype, dtor = funcidx|NIL
;;;   (:func params results async-p)  params = vector of (label . valtype)
;;;   (:component decls) (:instance decls)
;;; A VALTYPE is: (:idx . n) — a CONS whose car is :IDX — | a prim
;;; compound forms, text only — the binary valtype is index-or-prim,
;;; Binary.md lines 222-223).
(defstruct ct
  (kind nil :type keyword)
  (payload nil)
  (offset 0 :type (integer 0)))

(defun ct-primitive-p (c)
  "The primitive WIT value types (Binary.md lines 188-201)."
  (and (ct-p c) (eq (ct-kind c) :prim)
       (member (ct-payload c)
               '(:bool :s8 :u8 :s16 :u16 :s32 :u32 :s64 :u64
                 :f32 :f64 :char :string :error-context))))

(defun ct-defvaltype-p (c)
  "A DEFINED (value) type: primitives + the compound constructors
\(Binary.md lines 202-221) — everything but func/resource/component/
instance types (defined-types.wast: 'type index N is not a defined
type')."
  (and (ct-p c)
       (member (ct-kind c)
               '(:prim :record :variant :list :tuple :flags :enum
                 :option :result :own :borrow :stream :future :map))))

(defun ct-functype-p (c) (and (ct-p c) (eq (ct-kind c) :func)))
(defun ct-resourcetype-p (c) (and (ct-p c) (eq (ct-kind c) :resource)))

;;; ---- type-declarators inside component/instance types ----
;;; componentdecl / instancedecl (Binary.md lines 230-239):
;;;   (:core-type rectype-entry) (:type ct) (:alias aliasrec)
;;;   (:import name-attrs externtype) (:export name-attrs externtype)
(defstruct typedecl
  (kind nil :type keyword)               ; :core-type :type :alias
                                         ; :import :export
  (payload nil))

;;; ---- canon ----
;;; kind :lift -> (core-funcidx . opts), typeidx
;;; kind :lower -> (funcidx . opts)
;;; kind :resource-new/:resource-drop/:resource-rep -> typeidx
;;; kind :builtin -> (builtin-name . immediates)
(defstruct canonrec
  (kind nil :type keyword)
  (func 0 :type (integer 0))             ; lifted/lowered func index
  (opts nil :type list)                  ; canonopt list
  (typeidx nil :type (or null (integer 0)))
  (imms nil :type list)                  ; builtin immediates
  (offset 0 :type (integer 0)))

;;; ---- imports / exports ----
;;; nameattrs = (name . attribute-list); attribute =
;;;   (:implements "iface") (:version-suffix "s") (:external-id "id")
(defstruct importrec
  (name nil :type (or null string))
  (attrs nil :type list)
  (type nil :type cons))                 ; externtype (sort . payload)

(defstruct exportrec
  (name nil :type (or null string))
  (attrs nil :type list)
  (sortidx nil :type cons)               ; (sort . idx)
  (type nil :type (or null cons))        ; optional ascribed externtype
  (offset 0 :type (integer 0)))

;;; externtype representations (cons cells):
;;;   (:core-module . core-typeidx)   (:func . typeidx)
;;;   (:value . valuebound)           (:type . typebound)
;;;   (:component . typeidx)          (:instance . typeidx)
;;; typebound  = (list :eq idx) | :sub-resource
;;; valuebound = (list :eq idx) | a valtype
;;; (The inline text forms (func (param ...)) desugar to :func with an
;;; inline ct before ascription — the text parser resolves them.)

;;; ---- start / value ----
(defstruct startrec
  (func 0 :type (integer 0))
  (args #() :type vector)                ; value indices
  (results 0 :type (integer 0)))

(defstruct valuerec
  (type nil)                             ; a valtype
  (value nil)
  (offset 0 :type (integer 0)))

;;; ---------------------------------------------------------------
;;; Section ids (Binary.md lines 28-40).

(defparameter *component-section-names*
  #(nil "core module" "core instance" "core type" "component" "instance"
    "alias" "type" "canon" "start" "import" "export" "value"))

;;; The value reader needs the type index space; decode-component
;;; binds this to the defs accumulated before the current section.
(defvar *component-defs-so-far* nil)

;;; ---------------------------------------------------------------
;;; Layer discrimination (PLAN.md §4: wasmdown.binary owns it; this is
;;; the component half — the CORE reader (src/binary.lisp decode-module)
;;; already demands version byte0 = 1 = layer 0x00, so a component
;;; binary handed to decode-module fails 'unknown binary version',
;;; which is exactly binary.wast's expectation).

(defun component-p (bytes)
  "True when BYTES carries the component preamble: magic + version
0x0D 0x00 + layer 0x01 0x00 (Binary.md lines 22-27). Layer byte 7
discriminates core (0x00) from component (0x01); version byte 4 is
0x0D for components, 0x01 for core modules. Only the 8-byte shape is
inspected; anything shorter is not a component."
  (and (>= (length bytes) 8)
       (= (aref bytes 0) 0) (= (aref bytes 1) #x61)
       (= (aref bytes 2) #x73) (= (aref bytes 3) #x6d)
       (= (aref bytes 4) #x0d) (= (aref bytes 5) 0)
       (= (aref bytes 6) 1) (= (aref bytes 7) 0)))

;;; ---------------------------------------------------------------
;;; Sorts (Binary.md lines 68-75 core, 84-89 component).

(defun %read-sort (r)
  "A component sort (Binary.md lines 84-89): 0x00 core <core:sort>,
0x01 func, 0x02 value, 0x03 type, 0x04 component, 0x05 instance."
  (let ((b (read-byte-at r)))
    (case b
      (#x00 (%read-core-sort r))
      (#x01 :func) (#x02 :value) (#x03 :type)
      (#x04 :component) (#x05 :instance)
      (t (%derr r "invalid leading byte (0x~x) for sort" b)))))

(defun %read-core-sort (r)
  "A core:sort (Binary.md lines 68-75): the externtype discriminant
values 0x00 func .. 0x04 tag, plus 0x10 type, 0x11 module, 0x12
instance."
  (let ((b (read-byte-at r)))
    (case b
      (#x00 :core-func) (#x01 :core-table) (#x02 :core-memory)
      (#x03 :core-global) (#x04 :core-tag)
      (#x10 :core-type) (#x11 :core-module) (#x12 :core-instance)
      (t (%derr r "invalid leading byte (0x~x) for core sort" b)))))

(defun %read-sortidx (r)
  (cons (%read-sort r) (read-u32 r)))

;;; ---------------------------------------------------------------
;;; Names: externname + attributes (Binary.md lines 404-411).
;;; nameattributes ::= 0x00 len name | 0x01 len name | 0x02 len name attrs

(defun %read-nameattrs (r)
  "nameattributes (Binary.md lines 406-408). Returns (values name
attributes). The 0x00 and 0x01 prefixes are the same plain spelling
(Binary.md line 425: the redundant cases merge at 1.0); 0x02 adds a
vec of attributes (implements / version-suffix / external-id). Each
attribute kind may appear at most once — a duplicate is MALFORMED
(binary.wast: 'duplicate `implements` option in name'). Kebab shape
and strongly-unique conflicts are VALIDATE rules, not decode's."
  (let ((form (read-byte-at r)))
    (unless (member form '(0 1 2))
      (%derr r "invalid leading byte (0x~x) for component name" form))
    (let ((name (read-name r))
          (attrs
            (if (= form 2)
                (coerce (read-vec r #'%read-attribute "name attribute")
                        'list)
                '())))
      ;; each attribute kind at most once (malformed, per the fixture)
      (let ((kinds (mapcar #'car attrs)))
        (when (/= (length kinds)
                  (length (remove-duplicates kinds :test #'eq)))
          (%derr r "duplicate name attribute")))
      (values name attrs))))

(defun %read-attribute (r)
  "attribute (Binary.md lines 409-411): 0x00 implements, 0x01
version-suffix, 0x02 external-id. Duplicate kinds are rejected at
DECODE (the fixture's 'duplicate ... option in name' message)."
  (let ((b (read-byte-at r)))
    (case b
      (#x00 (list :implements (read-name r)))
      (#x01 (list :version-suffix (read-name r)))
      (#x02 (list :external-id (read-name r)))
      (t (%derr r "invalid leading byte (0x~x) for name option" b)))))

;;; ---------------------------------------------------------------
;;; Aliases (Binary.md lines 117-136).

(defun %read-alias (r)
  "alias (Binary.md lines 117-122): sort 0x00 instanceidx name (export)
| sort 0x01 core-instanceidx name (core export)
| sort 0x02 ct idx (outer).
The OUTER form is sort-LIMITED at decode (outeraliassort, Explainer
line 355: core module, core type, component, type); binary.wast pins
every other sort byte — component func 0x01, value 0x02, instance
0x05, unallocated 0x06, and core-sort bytes outside {0x10, 0x11} —
as MALFORMED with the sort's leading byte in the message."
  (let* ((start (reader-offset r))
         (sort-b (read-byte-at r))
         ;; Binary.md lines 119-121: a CORE sort is the two-byte pair
         ;; 0x00 + core:sort, read BEFORE the target discriminant
         ;; (peeking desynchronized the target byte for core sorts).
         (core-sort-b (when (= sort-b #x00) (read-byte-at r)))
         (target (read-byte-at r)))
    (flet ((outer-kind-error ()
             (%derr r "invalid leading byte (0x~x) for component outer alias kind"
                    (if (= sort-b 0) core-sort-b sort-b))))
      (case target
        (#x00
         (let ((idx (read-u32 r)) (name (read-name r)))
           (make-aliasrec :kind :export
                         :sort (%sort-from-byte sort-b core-sort-b)
                         :index idx :name name :offset start)))
        (#x01
         (let ((idx (read-u32 r)) (name (read-name r)))
           (make-aliasrec :kind :core-export
                         :sort (%sort-from-byte sort-b core-sort-b)
                         :index idx :name name :offset start)))
        (#x02
         ;; outeraliassort: core module (0x00 0x11), core type
         ;; (0x00 0x10), component (0x04), type (0x03). Anything
         ;; else is malformed AT DECODE (the fixtures' messages).
         (unless (or (and (= sort-b #x00) (member core-sort-b '(#x10 #x11)))
                     (member sort-b '(#x03 #x04)))
           (outer-kind-error))
         (let ((ct (read-u32 r)) (idx (read-u32 r)))
           (make-aliasrec :kind :outer
                         :sort (%sort-from-byte sort-b core-sort-b)
                         :count ct :target idx :offset start)))
        (t (%derr r "invalid leading byte (0x~x) for alias" target))))))

(defun %sort-from-byte (sort-b core-sort-b)
  "The sort keyword for a (possibly core) sort byte pair. The bytes
were validated by the caller (or %READ-SORT's grammar); a bad pair
here is a decode error naming the offending byte."
  (if (= sort-b #x00)
      (case core-sort-b
        (#x00 :core-func) (#x01 :core-table) (#x02 :core-memory)
        (#x03 :core-global) (#x04 :core-tag) (#x10 :core-type)
        (#x11 :core-module) (#x12 :core-instance)
        (t (decode-error-at 0
                            "invalid leading byte (0x~x) for component outer alias kind"
                            core-sort-b)))
      (case sort-b
        (#x01 :func) (#x02 :value) (#x03 :type) (#x04 :component)
        (#x05 :instance)
        (t (decode-error-at 0
                            "invalid leading byte (0x~x) for component outer alias kind"
            sort-b)))))

;;; ---------------------------------------------------------------
;;; Component value types (Binary.md lines 181-250).
;;;
;;; TYPEIDX in valtype position is a NONNEGATIVE SLEB128 (the same
;;; negative-SLEB scheme as core; Binary.md line 252: nonnegatives are
;;; type indices, negatives are the type constructors). read-s33
;;; returns the signed value; >= 0 is an index, < 0 is a constructor
;;; whose byte is (logand #x7f ...) of the ONE-byte minimal encoding.

(defun %read-primvaltype (r b)
  "Primitive valtype bytes (Binary.md lines 188-201)."
  (case b
    (#x7f :bool) (#x7e :s8) (#x7d :u8) (#x7c :s16) (#x7b :u16)
    (#x7a :s32) (#x79 :u32) (#x78 :s64) (#x77 :u64)
    (#x76 :f32) (#x75 :f64) (#x74 :char) (#x73 :string)
    (#x64 :error-context)
    (t nil)))

(defun %read-label (r)
  "labellit (Binary.md line 219): len u32 + len raw UTF-8 bytes — ONE
length prefix total (the label is not a nested core:name; the fixture
pins `\\01a` = the one-byte label `a`). Strictly UTF-8-validated,
mirroring wasmdown.binary's name decoder (overlong forms, surrogates,
code points above U+10FFFF and bad continuations are malformed)."
  (let* ((start (reader-offset r))
         (len (read-u32 r)))
    (when (> len (- (reader-limit r) (reader-offset r)))
      (decode-error-at start
                       "length out of bounds: label declares ~d byte~:p"
                       len))
    (%utf8-decode (read-bytes r len) start)))

(defun %utf8-decode (bytes base-offset)
  "Strict UTF-8 decode of a byte vector; decode errors carry absolute
offsets (BASE-OFFSET + the bad byte's index)."
  (let ((n (length bytes))
        (cps '()))
    (flet ((bad (pos control &rest args)
             (apply #'decode-error-at (+ base-offset pos) control args)))
      (loop with i = 0
            while (< i n)
            do (let ((b (aref bytes i)))
                 (cond
                  ((< b #x80) (push b cps) (incf i))
                  ((and (>= b #xC2) (<= b #xDF))
                   (when (>= (1+ i) n)
                     (bad i "truncated name: invalid UTF-8 encoding"))
                   (let ((c (aref bytes (1+ i))))
                     (unless (and (>= c #x80) (<= c #xBF))
                       (bad (1+ i)
                            "invalid UTF-8 encoding: bad continuation byte"))
                     (push (logior (ash (- b #xC0) 6) (- c #x80)) cps))
                   (incf i 2))
                  ((and (>= b #xE0) (<= b #xEF))
                   (when (>= (+ i 2) n)
                     (bad i "truncated name: invalid UTF-8 encoding"))
                   (let ((c1 (aref bytes (1+ i))) (c2 (aref bytes (+ i 2))))
                     (unless (and (>= c1 #x80) (<= c1 #xBF)
                                  (>= c2 #x80) (<= c2 #xBF))
                       (bad (1+ i)
                            "invalid UTF-8 encoding: bad continuation byte"))
                     (let ((cp (logior (ash (- b #xE0) 12)
                                       (ash (- c1 #x80) 6)
                                       (- c2 #x80))))
                       (cond ((< cp #x800)
                              (bad i "invalid UTF-8 encoding: overlong sequence"))
                             ((and (>= cp #xD800) (<= cp #xDFFF))
                              (bad i
                                   "invalid UTF-8 encoding: surrogate code point"))
                             (t (push cp cps)))))
                   (incf i 3))
                  ((and (>= b #xF0) (<= b #xF4))
                   (when (>= (+ i 3) n)
                     (bad i "truncated name: invalid UTF-8 encoding"))
                   (let ((c1 (aref bytes (1+ i))) (c2 (aref bytes (+ i 2)))
                         (c3 (aref bytes (+ i 3))))
                     (unless (and (>= c1 #x80) (<= c1 #xBF)
                                  (>= c2 #x80) (<= c2 #xBF)
                                  (>= c3 #x80) (<= c3 #xBF))
                       (bad (1+ i)
                            "invalid UTF-8 encoding: bad continuation byte"))
                     (let ((cp (logior (ash (- b #xF0) 18)
                                       (ash (- c1 #x80) 12)
                                       (ash (- c2 #x80) 6)
                                       (- c3 #x80))))
                       (cond ((< cp #x10000)
                              (bad i
                                   "invalid UTF-8 encoding: overlong sequence"))
                             ((> cp #x10FFFF)
                              (bad i
                                   "invalid UTF-8 encoding: code point too large"))
                             (t (push cp cps)))))
                   (incf i 4))
                  (t (bad i "invalid UTF-8 encoding")))))
      (coerce (mapcar #'code-char (nreverse cps)) 'string))))

(defun %read-valtype (r)
  "A component valtype (Binary.md lines 222-223): typeidx or primvaltype.
Returns (:idx . n) | prim keyword. Nonnegative s33 = type index
(Binary.md line 252: the nonnegative SLEB128s are type indices); the
single-byte negative values are the primitive constructors; anything
else is malformed here."
  (let* ((b (peek-byte-at r))
         (prim (%read-primvaltype r b)))
    (cond
     (prim (read-byte-at r) prim)
     ((and (>= b 0) (< b #x40))
      (cons :idx (read-s33 r)))
     (t
      ;; a multi-byte negative s33 in valtype position, or a
      ;; constructor byte that is not a valtype here
      (%derr r "invalid leading byte (0x~x) for component value type" b)))))

(defun %read-defvaltype (r)
  "A defvaltype (Binary.md lines 202-221) — the head byte dispatches.
Compound bodies: record 0x72, variant 0x71, list 0x70/0x67, tuple 0x6f,
flags 0x6e, enum 0x6d, option 0x6b, result 0x6a, own 0x69, borrow
0x68, stream 0x66, future 0x65, map 0x63. Non-emptiness, the 32-flag
limit and label grammar are VALIDATE rules (fixtures pin them as
assert_invalid); DECODE only enforces well-formedness."
  (let* ((start (reader-offset r))
         (b (peek-byte-at r))
         (prim (%read-primvaltype r b)))
    (cond
     (prim (read-byte-at r) (make-ct :kind :prim :payload prim :offset start))
     ((= b #x72)                         ; record
      (read-byte-at r)
      (let ((fields
              (read-vec r
                       (lambda (r)
                         (let* ((l (%read-label r))
                                (ty (%read-valtype r)))
                           (cons l ty)))
                       "record field")))
        (make-ct :kind :record :payload fields :offset start)))
     ((= b #x71)                         ; variant
      (read-byte-at r)
      (let ((cases
              (read-vec r
                       (lambda (r)
                         (let* ((l (%read-label r))
                                (payload? (read-byte-at r)))
                           (unless (or (= payload? 0) (= payload? 1))
                             (%derr r "invalid leading byte (0x~x) for zero byte required"
                                    payload?))
                           (let ((payload (if (= payload? 1)
                                               (%read-valtype r) nil))
                                 (z (read-byte-at r)))
                             (unless (zerop z)
                               (%derr r "invalid leading byte (0x~x) for zero byte required" z))
                             (cons l payload))))
                       "variant case")))
        (make-ct :kind :variant :payload cases :offset start)))
     ((= b #x70)                         ; list
      (read-byte-at r)
      (let ((elem (%read-valtype r)))
        (make-ct :kind :list :payload (cons elem nil) :offset start)))
     ((= b #x67)                         ; fixed list (len > 0)
      (read-byte-at r)
      (let ((elem (%read-valtype r))
            (len (read-u32 r)))
        (make-ct :kind :list :payload (cons elem len) :offset start)))
     ((= b #x6f)                         ; tuple
      (read-byte-at r)
      (let ((ts (read-vec r #'%read-valtype "tuple type")))
        (make-ct :kind :tuple :payload ts :offset start)))
     ((= b #x6e)                         ; flags
      (read-byte-at r)
      (let ((ls (read-vec r (lambda (r) (%read-label r)) "flag")))
        (make-ct :kind :flags :payload ls :offset start)))
     ((= b #x6d)                         ; enum
      (read-byte-at r)
      (let ((ls (read-vec r (lambda (r) (%read-label r)) "enum tag")))
        (make-ct :kind :enum :payload ls :offset start)))
     ((= b #x6b)                         ; option
      (read-byte-at r)
      (make-ct :kind :option :payload (%read-valtype r) :offset start))
     ((= b #x6a)                         ; result
      (read-byte-at r)
      (let ((ok? (read-byte-at r)))
        (unless (member ok? '(0 1))
          (%derr r "invalid leading byte (0x~x) for zero byte required" ok?))
        (let ((ok (if (= ok? 1) (%read-valtype r) nil))
              (err? (read-byte-at r)))
          (unless (member err? '(0 1))
            (%derr r "invalid leading byte (0x~x) for zero byte required" err?))
          (let ((err (if (= err? 1) (%read-valtype r) nil)))
            (make-ct :kind :result :payload (cons ok err) :offset start)))))
     ((= b #x69)                         ; own
      (read-byte-at r)
      (make-ct :kind :own :payload (read-u32 r) :offset start))
     ((= b #x68)                         ; borrow
      (read-byte-at r)
      (make-ct :kind :borrow :payload (read-u32 r) :offset start))
     ((= b #x66)                         ; stream
      (read-byte-at r)
      (let ((e? (read-byte-at r)))
        (unless (member e? '(0 1))
          (%derr r "invalid leading byte (0x~x) for zero byte required" e?))
        (make-ct :kind :stream
                 :payload (if (= e? 1) (%read-valtype r) nil)
                 :offset start)))
     ((= b #x65)                         ; future
      (read-byte-at r)
      (let ((e? (read-byte-at r)))
        (unless (member e? '(0 1))
          (%derr r "invalid leading byte (0x~x) for zero byte required" e?))
        (make-ct :kind :future
                 :payload (if (= e? 1) (%read-valtype r) nil)
                 :offset start)))
     ((= b #x63)                         ; map
      (read-byte-at r)
      (let ((k (%read-valtype r)) (v (%read-valtype r)))
        (make-ct :kind :map :payload (cons k v) :offset start)))
     (t
      (read-byte-at r)
      ;; unallocated opcode bytes 0x62 / 0x44 / 0x3e are pinned as
      ;; 'invalid leading byte (0x..) for component defined type'
      (%derr r "invalid leading byte (0x~x) for component defined type" b)))))

;;; functype / resourcetype / componenttype / instancetype
;;; (Binary.md lines 224-250).

(defun %read-paramlist (r)
  (read-vec r
            (lambda (r) (let ((l (%read-label r)))
                          (cons l (%read-valtype r))))
            "function parameter"))

(defun %read-resultlist (r)
  "resultlist (Binary.md lines 228-229): 0x00 valtype | 0x01 0x00 (empty)."
  (let ((b (read-byte-at r)))
    (case b
      (#x00 (%read-valtype r))
      (#x01 (let ((z (read-byte-at r)))
              (unless (zerop z)
                (%derr r "invalid leading byte (0x~x) for number of results" z))
              nil))
      (t (%derr r "invalid leading byte (0x~x) for component function results" b)))))

(defun %read-functype (r)
  (let ((b (read-byte-at r)))
    (unless (or (= b #x40) (= b #x43))
      (%derr r "invalid leading byte (0x~x) for component function type" b))
    (let ((params (%read-paramlist r))
          (results (%read-resultlist r)))
      (make-ct :kind :func
               :payload (list params results (= b #x43))
               :offset (reader-offset r)))))

(defun %read-resourcetype (r)
  "resourcetype (Binary.md line 224): 0x3f core:valtype dtor?.
The rep is a CORE valtype (reused rule); the dtor a CORE funcidx."
  (let ((b (read-byte-at r)))
    (unless (= b #x3f)
      (%derr r "invalid leading byte (0x~x) for resource type" b))
    (let ((rep (%read-core-valtype r))
          (dtor? (read-byte-at r)))
      (unless (member dtor? '(0 1))
        (%derr r "invalid leading byte (0x~x) for zero byte required" dtor?))
      (make-ct :kind :resource
               :payload (cons rep (if (= dtor? 1) (read-u32 r) nil))
               :offset (reader-offset r)))))

;;; core valtype for resource reps: reuse the CORE reader's valtype
;;; grammar (0x7f i32, 0x7e i64, ...). wasmdown.binary's %READ-VALTYPE
;;; is private; the public surface decodes it via decode-module — but
;;; the rep grammar is exactly core:valtype, so re-derive here via the
;;; same bytes. Simplest correct route: read ONE byte and validate it
;;; against the core numeric/reftype set (fixtures only use i32/i64).
(defun %read-core-valtype (r)
  (let ((b (read-byte-at r)))
    (case b
      (#x7f :i32) (#x7e :i64) (#x7d :f32) (#x7c :f64)
      (#x70 :funcref) (#x6f :externref) (#x7b :v128)
      (t (%derr r "invalid leading byte (0x~x) for core value type" b)))))

;;; externtype (Binary.md lines 240-249).

(defun %read-typebound (r)
  (let ((b (read-byte-at r)))
    (case b
      (#x00 (list :eq (read-u32 r)))
      (#x01 :sub-resource)
      (t (%derr r "invalid leading byte (0x~x) for type bound" b)))))

(defun %read-valuebound (r)
  (let ((b (read-byte-at r)))
    (case b
      (#x00 (list :eq (read-u32 r)))
      (#x01 (%read-valtype r))
      (t (%derr r "invalid leading byte (0x~x) for value bound" b)))))

(defun %read-externtype (r)
  "Returns the cons externtype (sort . payload) — see the IR section."
  (let ((b (read-byte-at r)))
    (case b
      (#x00
       (let ((z (read-byte-at r)))
         (unless (= z #x11)
           (%derr r "invalid leading byte (0x~x) for component external kind" z))
         (cons :core-module (read-u32 r))))
      (#x01 (cons :func (read-u32 r)))
      (#x02 (cons :value (%read-valuebound r)))
      (#x03 (cons :type (%read-typebound r)))
      (#x04 (cons :component (read-u32 r)))
      (#x05 (cons :instance (read-u32 r)))
      (t (%derr r "invalid leading byte (0x~x) for component external kind" b)))))

;;; componentdecl / instancedecl (Binary.md lines 230-239). The FIRST
;;; byte distinguishes: 0x00 core-type, 0x01 type, 0x02 alias, 0x03
;;; importdecl, 0x04 exportdecl. componentdecl = importdecl |
;;; instancedecl; the importdecl form 0x03 is NOT an instancedecl
;;; (binary.wast pins 0x03 inside instancetype as malformed).

(defun %read-instancedecl (r &optional (b (read-byte-at r)))
  "instancedecl (Binary.md lines 234-237): the head byte dispatches —
0x00 core:type, 0x01 type, 0x02 alias, 0x04 exportdecl. B is passed
by %READ-COMPONENTDECL, which has already consumed it (its own
dispatch must distinguish the componentdecl-only importdecl 0x03)."
  (case b
    (#x00                                ; core:type
     (make-typedecl :kind :core-type :payload (%read-core-type r)))
    (#x01
     (make-typedecl :kind :type :payload (%read-deftype r)))
    (#x02
     (make-typedecl :kind :alias :payload (%read-alias r)))
    (#x04
     (multiple-value-bind (name attrs) (%read-nameattrs r)
       (make-typedecl :kind :export
                      :payload (list name attrs (%read-externtype r)))))
    (t (%derr r "invalid leading byte (0x~x) for component or instance type declaration" b))))

(defun %read-componentdecl (r)
  "componentdecl (Binary.md lines 232-233): importdecl 0x03 or an
instancedecl (0x00/0x01/0x02/0x04). The head byte is consumed HERE
and handed to %READ-INSTANCEDECL so the declarator byte is never
read twice."
  (let ((b (read-byte-at r)))
    (cond
     ((= b #x03)
      (multiple-value-bind (name attrs) (%read-nameattrs r)
        (make-typedecl :kind :import
                       :payload (list name attrs (%read-externtype r)))))
     ((member b '(0 1 2 4))
      (%read-instancedecl r b))
     (t (%derr r "invalid leading byte (0x~x) for component or instance type declaration" b)))))

(defun %read-deftype (r)
  "deftype (Binary.md lines 182-187): defvaltype | functype |
componenttype | instancetype | resourcetype."
  (let* ((start (reader-offset r))
         (b (peek-byte-at r)))
    (cond
     ((or (%read-primvaltype r b)
          (member b '(#x72 #x71 #x70 #x67 #x6f #x6e #x6d #x6b #x6a
                      #x69 #x68 #x66 #x65 #x63)))
      (%read-defvaltype r))
     ((member b '(#x40 #x43)) (%read-functype r))
     ((= b #x3f) (%read-resourcetype r))
     ((= b #x41)
      (read-byte-at r)
      (make-ct :kind :component
               :payload (coerce (read-vec r #'%read-componentdecl
                                          "component type decl")
                                'vector)
               :offset start))
     ((= b #x42)
      (read-byte-at r)
      (make-ct :kind :instance
               :payload (coerce (read-vec r #'%read-instancedecl
                                          "instance type decl")
                                'vector)
               :offset start))
     (t (read-byte-at r)
        (%derr r "invalid leading byte (0x~x) for component defined type" b)))))

;;; core:type inside components (Binary.md lines 140-153): a rectype
;;; (shared with core), the 0x00-prefixed sub form, or a moduletype.
;;; The CORE reader decodes rectype/rectype-body via decode-module's
;;; type section logic; here we need only the SHAPE (for the moduletype
;;; case) plus the shared rules. core:moduletype (0x50 + moduledecls)
;;; is component-specific and read here.

(defun %read-core-moduledecl (r)
  "core:moduledecl (Binary.md lines 147-153). Returns a typedecl of
kind :core-import (payload (mod field . core-importdesc)) | :core-type
(payload rectype) | :core-alias (payload (ct idx)) | :core-export
(payload (name . core-externtype))."
  (let ((b (read-byte-at r)))
    (case b
      (#x00
       (let* ((module (read-name r))
              (field (read-name r))
              (desc (%read-core-importdesc r)))
         (make-typedecl :kind :core-import :payload (list module field desc))))
      (#x01
       (make-typedecl :kind :core-type :payload (%read-core-rectype r)))
      (#x02
       ;; core:alias (Binary.md line 151): 0x10 0x01 ct idx — the
       ;; first byte is the core:sort `type` discriminant, the second
       ;; the outer-alias target.
       (let ((sort (read-byte-at r)) (target (read-byte-at r)))
         (unless (= sort #x10)
           (%derr r "invalid leading byte (0x~x) for outer alias kind" sort))
         (unless (= target #x01)
           (%derr r "invalid leading byte (0x~x) for outer alias target" target))
         (make-typedecl :kind :core-alias
                        :payload (list (read-u32 r) (read-u32 r)))))
      (#x03
       (let* ((name (read-name r))
              (desc (%read-core-importdesc r)))
         (make-typedecl :kind :core-export :payload (list name desc))))
      (t (%derr r "invalid leading byte (0x~x) for type definition" b)))))

(defun %read-core-rectype (r)
  "core:rectype shared with Core WebAssembly: REC 0x4E, SUB 0x50, or a
bare comptype. Reuses the CORE reader grammar (wasmdown.binary's
%read-rectype is private; the shape here is a shallow copy — the core
decoder's full discipline lives in src/binary.lisp and this path only
feeds STRUCTURAL validation of module types, not function-body
validation)."
  (let ((b (peek-byte-at r)))
    (cond
     ((= b #x4e)
      (read-byte-at r)
      (let ((n (read-u32 r)))
        ;; count bound: each comptype needs >= 1 byte
        (when (> n (- (reader-limit r) (reader-offset r)))
          (%derr r "length out of bounds: rec vector declares ~d types" n))
        (let ((cts (loop repeat n collect (%read-core-comptype r))))
          (list :rec cts))))
     ((= b #x50)
      (read-byte-at r)
      (let ((subn (read-u32 r)))
        (cons :sub (progn (loop repeat subn do (read-u32 r))
                          (%read-core-comptype r)))))
     (t (%read-core-comptype r)))))

(defun %read-core-comptype (r)
  "core:comptype: functype 0x60, struct 0x5F, array 0x5E. Only the
functype body is decoded in full (the fixtures' module types use
functypes); struct/array read their field shapes shallowly."
  (let ((b (read-byte-at r)))
    (case b
      (#x60
       (let ((params (read-vec r #'%read-core-valtype "core param"))
             (results (read-vec r #'%read-core-valtype "core result")))
         (list :func params results)))
      (#x5f
       (list :struct (read-vec r
                               (lambda (r)
                                 (list (%read-core-storagetype r)
                                       (read-byte-at r)))
                               "struct field")))
      (#x5e
       (list :array (%read-core-storagetype r) (read-byte-at r)))
      (t (%derr r "invalid leading byte (0x~x) for core comptype" b)))))

(defun %read-core-storagetype (r)
  (let ((b (read-byte-at r)))
    (case b (#x78 :i8) (#x77 :i16) (t
      ;; re-use core valtype bytes; simplest: rewind not needed since
      ;; the byte was consumed — return the valtype for the byte.
      (case b (#x7f :i32) (#x7e :i64) (#x7d :f32) (#x7c :f64)
            (#x70 :funcref) (#x6f :externref) (t
              (%derr r "invalid leading byte (0x~x) for core storage type" b)))))))

(defun %read-core-importdesc (r)
  "A core import/export descriptor (reused core rules): kind byte +
payload: func u32, table reftype+limits, memory limits, global
valtype+mut, tag u32."
  (let ((kind (read-byte-at r)))
    (case kind
      (#x00 (list :func (read-u32 r)))
      (#x01 (let ((rt (case (peek-byte-at r)
                        (#x70 (read-byte-at r) :funcref)
                        (#x6f (read-byte-at r) :externref)
                        (t (%derr r "bad table reftype"))))
                  (limits (%read-core-limits r)))
              (list :table rt limits)))
      (#x02 (list :memory (%read-core-limits r)))
      (#x03 (let ((ty (%read-core-valtype r)) (mut (read-byte-at r)))
              (unless (member mut '(0 1))
                (%derr r "malformed global mutability 0x~x" mut))
              (list :global ty (= mut 1))))
      (#x04 (list :tag (read-u32 r)))
      (t (%derr r "invalid leading byte (0x~x) for core external kind" kind)))))

(defun %read-core-limits (r)
  "Core limits for module-type imports/exports: flags byte (0/1),
min u32, max u32?."
  (let ((flags (read-byte-at r)))
    (unless (member flags '(0 1))
      (%derr r "malformed limits flags 0x~x" flags))
    (let ((min (read-u32 r)))
      (if (= flags 1)
          (list min (read-u32 r))
          (list min nil)))))

(defun %read-core-type (r)
  "core:type as an instancedecl (Binary.md line 234): a core:deftype —
rectype | 0x00-prefixed non-final sub | moduletype. The 0x50 ambiguity
(Binary.md lines 158-165): outside a moduletype, a non-final sub is
`0x00 0x50`; a bare 0x50 here is the MODULETYPE."
  (let ((b (peek-byte-at r)))
    (cond
     ((= b #x00)
      (let ((next (progn (read-byte-at r) (peek-byte-at r))))
        (if (= next #x50)
            (progn (read-byte-at r)
                   ;; non-final sub: sub-count + supertypes + comptype
                   (let ((subn (read-u32 r)))
                     (list :sub (progn (loop repeat subn do (read-u32 r))
                                       (%read-core-comptype r)))))
            ;; 0x00 then something else: malformed type definition
            (%derr r "invalid leading byte (0x~x) for type definition" next))))
     ((= b #x50)
      (read-byte-at r)
      (list :moduletype
            (coerce (read-vec r #'%read-core-moduledecl "module type decl")
                    'vector)))
     (t (%read-core-rectype r)))))

;;; ---------------------------------------------------------------
;;; Canon definitions (Binary.md lines 304-366).

(defun %read-canonopts (r)
  "opts ::= vec(canonopt) (Binary.md lines 357-366). Returns a plist."
  (coerce (read-vec r #'%read-canonopt "canonical option") 'list))

(defun %read-canonopt (r)
  (let ((b (read-byte-at r)))
    (case b
      (#x00 '(:string-encoding utf8))
      (#x01 '(:string-encoding utf16))
      (#x02 '(:string-encoding latin1+utf16))
      (#x03 (list :memory (read-u32 r)))
      (#x04 (list :realloc (read-u32 r)))
      (#x05 (list :post-return (read-u32 r)))
      (#x06 '(:async t))
      (#x07 (list :callback (read-u32 r)))
      (t (%derr r "invalid leading byte (0x~x) for canonical option" b)))))

(defun %read-async? (r)
  (let ((b (read-byte-at r)))
    (unless (member b '(0 1))
      (%derr r "invalid leading byte (0x~x) for zero byte required" b))
    (= b 1)))

(defun %read-zero-byte (r)
  (let ((b (read-byte-at r)))
    (unless (zerop b)
      (%derr r "invalid leading byte (0x~x) for zero byte required" b))))

(defun %read-canon (r)
  "canon (Binary.md lines 304-352). Returns a canonrec."
  (let* ((start (reader-offset r))
         (b (read-byte-at r)))
    (flet ((lift (f-idx opts typeidx)
             (make-canonrec :kind :lift :func f-idx :opts opts
                            :typeidx typeidx :offset start))
           (lower (f-idx opts)
             (make-canonrec :kind :lower :func f-idx :opts opts :offset start)))
      (case b
        (#x00
         (let ((sort (read-byte-at r)))
           (unless (zerop sort)
             ;; the vestigial second byte is the func sort
             (%derr r "invalid leading byte (0x~x) for canonical function lift" sort))
           (let ((f (read-u32 r))
                 (opts (%read-canonopts r))
                 (ft (read-u32 r)))
             (lift f opts ft))))
        (#x01
         (let ((sort (read-byte-at r)))
           (unless (zerop sort)
             (%derr r "invalid leading byte (0x~x) for canonical function lower" sort))
           (let ((f (read-u32 r))
                 (opts (%read-canonopts r)))
             (lower f opts))))
        ((#x02 #x03 #x04)
         (make-canonrec
          :kind (case b (#x02 :resource-new) (#x03 :resource-drop)
                    (#x04 :resource-rep))
          :typeidx (read-u32 r) :offset start))
        (#x24 (make-canonrec :kind :builtin :imms '(:backpressure-inc)
                             :offset start))
        (#x25 (make-canonrec :kind :builtin :imms '(:backpressure-dec)
                             :offset start))
        (#x09
         ;; task.return: resultlist + opts
         (let ((results (%read-resultlist r))
               (opts (%read-canonopts r)))
           (make-canonrec :kind :builtin
                          :imms (list :task-return results) :opts opts
                          :offset start)))
        (#x05 (make-canonrec :kind :builtin :imms '(:task-cancel)
                             :offset start))
        (#x0a
         (let ((ty (%read-core-valtype r)) (i (read-u32 r)))
           (make-canonrec :kind :builtin :imms (list :context-get ty i)
                          :offset start)))
        (#x0b
         (let ((ty (%read-core-valtype r)) (i (read-u32 r)))
           (make-canonrec :kind :builtin :imms (list :context-set ty i)
                          :offset start)))
        (#x06
         (let ((async (%read-async? r)))
           (make-canonrec :kind :builtin
                          :imms (list :subtask-cancel async) :offset start)))
        (#x0d (make-canonrec :kind :builtin :imms '(:subtask-drop)
                             :offset start))
        (#x0e
         (make-canonrec :kind :builtin :imms (list :stream-new (read-u32 r))
                        :offset start))
        ((#x0f #x10)
         (let* ((ty (read-u32 r)) (opts (%read-canonopts r)))
           (make-canonrec :kind :builtin
                          :imms (list (if (= b #x0f) :stream-read :stream-write)
                                      ty)
                          :opts opts :offset start)))
        ((#x11 #x12)
         (let* ((ty (read-u32 r)) (async (%read-async? r)))
           (make-canonrec :kind :builtin
                          :imms (list (if (= b #x11) :stream-cancel-read
                                         :stream-cancel-write)
                                      ty async)
                          :offset start)))
        ((#x13 #x14)
         (make-canonrec :kind :builtin
                        :imms (list (if (= b #x13) :stream-drop-readable
                                       :stream-drop-writable)
                                    (read-u32 r))
                        :offset start))
        (#x2e (make-canonrec :kind :builtin :imms (list :stream-forward (read-u32 r))
                              :offset start))
        (#x15
         (make-canonrec :kind :builtin :imms (list :future-new (read-u32 r))
                        :offset start))
        ((#x16 #x17)
         (let* ((ty (read-u32 r)) (opts (%read-canonopts r)))
           (make-canonrec :kind :builtin
                          :imms (list (if (= b #x16) :future-read
                                          :future-write)
                                      ty)
                          :opts opts :offset start)))
        ((#x18 #x19)
         (let* ((ty (read-u32 r)) (async (%read-async? r)))
           (make-canonrec :kind :builtin
                          :imms (list (if (= b #x18) :future-cancel-read
                                         :future-cancel-write)
                                      ty async)
                          :offset start)))
        ((#x1a #x1b)
         (make-canonrec :kind :builtin
                        :imms (list (if (= b #x1a) :future-drop-readable
                                       :future-drop-writable)
                                    (read-u32 r))
                        :offset start))
        (#x2f (make-canonrec :kind :builtin :imms (list :future-forward (read-u32 r))
                             :offset start))
        (#x1c (make-canonrec :kind :builtin :imms '(:error-context-new)
                             :opts (%read-canonopts r) :offset start))
        (#x1d (make-canonrec :kind :builtin :imms '(:error-context-debug-message)
                             :opts (%read-canonopts r) :offset start))
        (#x1e (make-canonrec :kind :builtin :imms '(:error-context-drop)
                             :offset start))
        (#x1f (make-canonrec :kind :builtin :imms '(:waitable-set-new)
                             :offset start))
        ((#x20 #x21)
         (%read-zero-byte r)
         (let ((m (read-u32 r)))
           (make-canonrec :kind :builtin
                          :imms (list (if (= b #x20) :waitable-set-wait
                                         :waitable-set-poll)
                                      m)
                          :offset start)))
        (#x22 (make-canonrec :kind :builtin :imms '(:waitable-set-drop)
                             :offset start))
        (#x23 (make-canonrec :kind :builtin :imms '(:waitable-join)
                             :offset start))
        (#x26 (make-canonrec :kind :builtin :imms '(:thread-index)
                             :offset start))
        (#x27
         (let ((ft (read-u32 r)) (tbl (read-u32 r)))
           (make-canonrec :kind :builtin :imms (list :thread-new-indirect ft tbl)
                          :offset start)))
        (#x28 (make-canonrec :kind :builtin :imms '(:thread-resume-later)
                             :offset start))
        (#x29 (%read-zero-byte r)
              (make-canonrec :kind :builtin :imms '(:thread-suspend)
                             :offset start))
        (#x0c (%read-zero-byte r)
              (make-canonrec :kind :builtin :imms '(:thread-yield)
                             :offset start))
        ((#x2a #x2b #x2c #x2d)
         (%read-zero-byte r)
         (make-canonrec :kind :builtin
                        :imms (list (case b
                                      (#x2a :thread-suspend-then-resume)
                                      (#x2b :thread-yield-then-resume)
                                      (#x2c :thread-suspend-then-promote)
                                      (#x2d :thread-yield-then-promote)))
                        :offset start))
        ((#x40 #x41 #x42)
         ;; shared-everything thread.spawn family: sh? + ft? + table?
         (let* ((sh (let ((sb (read-byte-at r)))
                      (unless (member sb '(0 1))
                        (%derr r "invalid leading byte (0x~x) for zero byte required" sb))
                      (= sb 1)))
                (ft (if (/= b #x40) (read-u32 r) nil))
                (tbl (if (= b #x41) (read-u32 r) nil)))
           (make-canonrec :kind :builtin
                          :imms (list (case b
                                        (#x40 :thread-spawn-ref)
                                        (#x41 :thread-spawn-indirect)
                                        (#x42 :thread-available-parallelism))
                                      sh ft tbl)
                          :offset start)))
        (t (%derr r "invalid leading byte (0x~x) for canonical function" b))))))

;;; ---------------------------------------------------------------
;;; Values (Binary.md lines 482-520) — the value section. Values are
;;; decoded per their declared type; typeidx resolution needs the type
;;; index space built so far, so the value section reader receives it.
;;; The shapes used by the vendored fixtures are the primitives + the
;;; compound forms built from them (the section is gated 🪙 and unused
;;; by the binary/validation suites — kept complete anyway).

(defun %read-val-for (r type types)
  "Decode a value of component TYPE (a ct or a prim keyword) against
the TYPE index space built so far. Returns the Lisp value."
  (labels
      ((resolve-valtype (vt)
         (cond ((keywordp vt) vt)
               ((and (consp vt) (eq (car vt) :idx))
                (let ((entry (aref types (cdr vt))))
                  (cond ((ct-p entry) entry)
                        (t (error 'wasmdown:decode-error
                                  :offset 0
                                  :message "bad type index in value")))))
               ((ct-p vt) vt)
               (t (%derr r "bad valtype in value"))))
       (dec (ctype)
         (let ((c (if (ct-p ctype) ctype (make-ct :kind :prim :payload ctype))))
           (case (ct-kind c)
             (:prim
              (case (ct-payload c)
                (:bool (let ((b (read-byte-at r)))
                          (unless (member b '(0 1))
                            (%derr r "bad bool value byte"))
                          (= b 1)))
                ((:u8 :s8) (read-byte-at r))
                ((:u16 :s16) (let ((b (read-bytes r 2)))
                               (+ (aref b 0) (ash (aref b 1) 8))))
                ((:u32 :s32) (let ((b (read-bytes r 4)))
                               (+ (aref b 0) (ash (aref b 1) 8)
                                  (ash (aref b 2) 16) (ash (aref b 3) 24))))
                ((:u64 :s64) (let ((b (read-bytes r 8)))
                               (let ((v 0))
                                 (loop for i from 7 downto 0
                                       do (setf v (+ (ash v 8) (aref b i))))
                                 v)))
                (:f32 (let ((b (read-bytes r 4)))
                        (let ((w (+ (aref b 0) (ash (aref b 1) 8)
                                    (ash (aref b 2) 16) (ash (aref b 3) 24))))
                          (if (and (= w #x7fc00000) t)
                              :nan
                              (sb-kernel:make-single-float
                               (if (>= w #x80000000) (- w #x100000000) w))))))
                (:f64 (let ((b (read-bytes r 8)))
                        (let ((lo (+ (aref b 0) (ash (aref b 1) 8)
                                     (ash (aref b 2) 16) (ash (aref b 3) 24)))
                              (hi (+ (aref b 4) (ash (aref b 5) 8)
                                     (ash (aref b 6) 16) (ash (aref b 7) 24))))
                          (if (= hi #x7ff80000) :nan
                              (sb-kernel:make-double-float
                               (if (>= hi #x80000000) (- hi #x100000000) hi)
                               lo)))))
                (:char (read-name r))
                (:string (read-name r))
                (t (%derr r "cannot decode value of this type")))
             (:record
              (loop for (nil . vt) across (ct-payload c)
                    collect (dec (resolve-valtype vt))))
             (:variant
              (let ((i (read-u32 r)))
                (when (>= i (length (ct-payload c)))
                  (%derr r "variant case index out of bounds"))
                (let ((case (aref (ct-payload c) i)))
                  (list (car case)
                        (when (cdr case) (dec (resolve-valtype (cdr case))))))))
             (:list
              (let* ((elem (car (ct-payload c)))
                     (fixed (cdr (ct-payload c)))
                     (et (resolve-valtype elem)))
                (if fixed
                    (loop repeat fixed collect (dec et))
                    (let ((n (read-u32 r)))
                      (when (> n (- (reader-limit r) (reader-offset r)))
                        (%derr r "length out of bounds: list value"))
                      (loop repeat n collect (dec et))))))
             (:tuple
              (loop for vt across (ct-payload c)
                    collect (dec (resolve-valtype vt))))
             (:flags
              (let* ((n (length (ct-payload c)))
                     (nb (ceiling n 8))
                     (bs (read-bytes r nb)))
                (loop for i below n
                      when (logtest (aref bs (floor i 8)) (ash 1 (mod i 8)))
                        collect (aref (ct-payload c) i))))
             (:enum
              (let ((i (read-u32 r)))
                (when (>= i (length (ct-payload c)))
                  (%derr r "enum case index out of bounds"))
                (aref (ct-payload c) i)))
             (:option
              (let ((b (read-byte-at r)))
                (unless (member b '(0 1)) (%derr r "bad option tag"))
                (if (= b 1) (list :some (dec (resolve-valtype (ct-payload c))))
                    :none)))
             (:result
              (let ((b (read-byte-at r)))
                (unless (member b '(0 1)) (%derr r "bad result tag"))
                (let ((ok (car (ct-payload c))) (err (cdr (ct-payload c))))
                  (cond ((and (= b 0) ok) (list :ok (dec (resolve-valtype ok))))
                        ((and (= b 1) err)
                         (list :error (dec (resolve-valtype err))))
                        ((= b 0) :ok)
                        (t :error)))))
             (t (%derr r "cannot decode value of this type")))))
    (dec (if (ct-p type) type (resolve-valtype type)))))))

;;; ---------------------------------------------------------------
;;; The section readers.

(defun %read-core-instance (r)
  "core:instance (Binary.md lines 63-66): 0x00 moduleidx
vec(instantiatearg) | 0x01 vec(inlineexport). An instantiatearg is
core:name 0x12 instanceidx — the 0x12 IS the instance sort byte
(Binary.md line 66); any other byte is malformed ('invalid leading
byte (0x..) for instantiation arg kind', pinned by binary.wast)."
  (let ((b (read-byte-at r)))
    (case b
      (#x00
       (let ((m (read-u32 r))
             (args (read-vec r
                             (lambda (r)
                               (let* ((name (read-name r))
                                      (sort (read-byte-at r)))
                                 (unless (= sort #x12)
                                   (%derr r "invalid leading byte (0x~x) for instantiation arg kind"
                                          sort))
                                 (cons name (read-u32 r))))
                             "core instantiate arg")))
         (make-coreinstrec :kind :instantiate :index m :args args)))
      (#x01
       (make-coreinstrec
        :kind :inline-exports
        :exports (read-vec r
                           (lambda (r)
                             (let ((name (read-name r)))
                               ;; core:sortidx (Binary.md line 67):
                               ;; core:sort + u32 — NO leading 0x00
                               ;; (that byte belongs to the component
                               ;; sortidx grammar, not the core one)
                               (cons name (cons (%read-core-sort r)
                                                (read-u32 r)))))
                           "core inline export")))
      (t (%derr r "invalid leading byte (0x~x) for core instance" b)))))

(defun %read-instance (r)
  "instance (Binary.md lines 78-90)."
  (let ((b (read-byte-at r)))
    (case b
      (#x00
       (let ((c (read-u32 r))
             (args (read-vec r
                             (lambda (r)
                               (let ((name (read-name r)))
                                 (cons name (%read-sortidx r))))
                             "instantiate arg")))
         (make-instrec :kind :instantiate :index c
                       :args (map 'vector #'identity args))))
      (#x01
       (make-instrec
        :kind :inline-exports
        :exports (read-vec r
                           (lambda (r)
                             (multiple-value-bind (name attrs)
                                 (%read-nameattrs r)
                               (cons (list name attrs) (%read-sortidx r))))
                           "inline export")))
      (t (%derr r "invalid leading byte (0x~x) for instance" b)))))

(defun %read-import (r)
  "import (Binary.md line 404)."
  (multiple-value-bind (name attrs) (%read-nameattrs r)
    (make-importrec :name name :attrs attrs :type (%read-externtype r))))

(defun %read-export (r)
  "export (Binary.md line 405): nameattrs sortidx externtype?."
  (let* ((start (reader-offset r)))
    (multiple-value-bind (name attrs) (%read-nameattrs r)
      (let ((si (%read-sortidx r))
            (b (read-byte-at r)))
        (unless (member b '(0 1))
          (%derr r "invalid leading byte (0x~x) for optional component export type" b))
        (if (= b 1)
            (make-exportrec :name name :attrs attrs :sortidx si
                            :type (%read-externtype r) :offset start)
            (make-exportrec :name name :attrs attrs :sortidx si
                            :type nil :offset start))))))

;;; start (Binary.md line 381): funcidx vec(valueidx) u32.
(defun %read-start (r)
  (make-startrec
   :func (read-u32 r)
   :args (coerce (read-vec r #'read-u32 "start value arg") 'vector)
   :results (read-u32 r)))

;;; ---------------------------------------------------------------
;;; decode-component: the preamble + section loop.
;;;
;;; Section ORDER (Binary.md lines 28-40): the section vector is
;;; `s*:<section>*` — sections may appear IN ANY ORDER and may REPEAT
;;; (each appends to its index space; `flatten`); only the ID RANGE
;;; 0..12 is legal (binary.wast pins id 13 and id 60 as 'malformed
;;; section id', and the redundant 0xFF LEB overread as EOF). CUSTOM
;;; sections (id 0) may repeat arbitrarily and carry arbitrary bytes
;;; after the name.

(defun decode-component (bytes)
  "Decode a component binary (magic + version 0x0D + layer 0x01) into
a comp record. Signals wasmdown:decode-error with :offset on any
malformation (preamble, section framing, LEB overreads, unknown
opcodes, bad UTF-8)."
  (let ((r (make-reader bytes)))
    ;; preamble
    (unless (>= (reader-limit r) 8)
      (decode-error-at 0 "unexpected end-of-file: missing magic/version"))
    (let ((magic (read-bytes r 4)))
      (unless (and (= (aref magic 0) 0) (= (aref magic 1) #x61)
                   (= (aref magic 2) #x73) (= (aref magic 3) #x6d))
        (decode-error-at 0 "magic header not detected")))
    (let ((version (read-bytes r 4)))
      (unless (= (aref version 0) #x0d)
        (decode-error-at 4 "unknown binary version"))
      (unless (= (aref version 1) 0)
        (decode-error-at 5 "unknown binary version"))
      (unless (= (aref version 2) 1)
        (decode-error-at 6 "unknown binary version"))
      (unless (= (aref version 3) 0)
        (decode-error-at 7 "unknown binary version")))
    (let ((defs '())
          (customs '()))
      (loop
        (when (>= (reader-offset r) (reader-limit r)) (return))
        (let* ((sec-start (reader-offset r))
               (id (read-byte-at r))
               (size (read-u32 r))
               (content-end (+ (reader-offset r) size)))
          (when (> content-end (reader-limit r))
            ;; the declared size runs past EOF (binary.wast: 'unexpected
            ;; end-of-file'); pin the offset at the section header
            (decode-error-at sec-start
                             "section size mismatch: section ~d declares ~d bytes"
                             id size))
          (cond
           ((= id 0)
            ;; custom section: name + opaque payload, repeatable,
            ;; arbitrary bytes after the name (binary.wast allows
            ;; garbage + repeated 'component-name' sections)
            (let* ((sub (make-reader (reader-bytes r)
                                     (reader-offset r) content-end))
                   (name (read-name sub))
                   (data (read-bytes sub (- (reader-limit sub)
                                            (reader-offset sub)))))
              (setf (reader-offset r) (reader-offset sub))
              (push (cons name data) customs)))
           ((<= id 12)
            (let* ((sub (make-reader (reader-bytes r)
                                     (reader-offset r) content-end))
                   ;; the value section resolves typeidx against the
                   ;; type index space built so far: bind the defs
                   ;; accumulated to this point (file order).
                   (*component-defs-so-far*
                     (coerce (reverse defs) 'vector)))
              (declare (special *component-defs-so-far*))
              (let ((new-defs (%read-section id sub)))
                (setf (reader-offset r) (reader-offset sub))
                (unless (= (reader-offset sub) content-end)
                  (decode-error-at sec-start
                                   "section size mismatch: section ~d content does not fill declared size ~d"
                                   id size))
                (dolist (d new-defs) (push d defs)))))
           (t
            ;; id 13, id 60, ... : 'malformed section id'. The 0xFF
            ;; case (multi-byte LEB overread) already failed the u32
            ;; read with 'integer representation too long'.
            (decode-error-at sec-start "malformed section id ~d" id)))))
      (make-comp :defs (coerce (nreverse defs) 'vector)
                 :customs (coerce (nreverse customs) 'vector)))))

(defun %read-section (id r)
  "Read section ID's contents; return the list of compdefs (in order)."
  (flet ((def (kind payload &optional (offset 0))
           (make-compdef :kind kind :payload payload :offset offset)))
    (ecase id
      (1  ; core module: ONE module (not a vector) — Binary.md line 29
       ;; the module is the rest of the section's content: decode via
       ;; the CORE reader (bounded at the section content end)
       (list (def :core-module
               (let* ((bytes (read-bytes r (- (reader-limit r)
                                              (reader-offset r)))))
                 (wasmdown.binary:decode-module bytes))
               (reader-offset r))))
      (2  ; core instance (vector)
       (loop for ci across (read-vec r #'%read-core-instance
                                     "core instance")
             collect (def :core-instance ci)))
      (3  ; core type (vector)
       (loop for ct across (read-vec r #'%read-core-type "core type")
             collect (def :core-type ct)))
      (4  ; component (ONE nested component)
       (list (def :component
               (decode-component
                (read-bytes r (- (reader-limit r) (reader-offset r))))
               (reader-offset r))))
      (5  ; instance (vector)
       (loop for i across (read-vec r #'%read-instance "instance")
             collect (def :instance i)))
      (6  ; alias (vector)
       (loop for a across (read-vec r #'%read-alias "alias")
             collect (def :alias a)))
      (7  ; type (vector)
       (loop for ty across (read-vec r #'%read-deftype "component type")
             collect (def :type ty)))
      (8  ; canon (vector)
       (loop for c across (read-vec r #'%read-canon "canon")
             collect (def :canon c)))
      (9  ; start (ONE) — but the section may repeat, so no dup check
       (list (def :start (%read-start r))))
      (10 ; import (vector)
       (loop for i across (read-vec r #'%read-import "import")
             collect (def :import i)))
      (11 ; export (vector)
       (loop for e across (read-vec r #'%read-export "export")
             collect (def :export e)))
      (12 ; value (vector)
       ;; values resolve their declared type's typeidx against the
       ;; type index space built so far (*COMPONENT-DEFS-SO-FAR* is
       ;; bound by decode-component to the defs preceding this
       ;; section, in FILE ORDER).
       (let ((types (make-array 16 :fill-pointer 0 :adjustable t)))
         (loop for d across *component-defs-so-far*
               when (eq (compdef-kind d) :type)
                 do (vector-push-extend (compdef-payload d) types))
         (loop for v across
               (read-vec r
                         (lambda (r)
                           (let* ((start (reader-offset r))
                                  (vt (%read-valtype r))
                                  (len (read-u32 r))
                                  (val (%read-val-for r vt types)))
                             (declare (ignore len))
                             (make-valuerec :type vt :value val
                                            :offset start)))
                         "value")
               collect (def :value v)))))))



;;; ---------------------------------------------------------------
;;; The component VALIDATOR. Walks the flat definition sequence,
;;; building each index space incrementally (Binary.md line 111-112:
;;; "the indices in sortidx are validated according to their sort's
;;; index spaces, which are built incrementally as each definition is
;;; validated"), then checks:
;;;   * type-index bounds in valtypes, and that a valtype index refers
;;;     to a DEFINED (value) type (defined-types.wast);
;;;   * own/borrow refer to resource types;
;;;   * non-emptiness of variant/record/flags/enum/tuple, the 32-flag
;;;     limit, non-empty labels, kebab labels, case-insensitive label
;;;     uniqueness (defined-types.wast);
;;;   * elem_size(t, i64) < 2^28 for every defined type (max-value-
;;;     size.wast, CanonicalABI.md#element-size);
;;;   * outer alias counts vs the scope stack (outer-alias.wast);
;;;   * import/export externname grammar + strongly-unique names
;;;     (kebab.wast, extern-names.wast);
;;;   * the sort checks of import/export externtypes vs the index
;;;     spaces built so far (indicies.wast, defined-types.wast).
;;;
;;; The validator runs over the DECODED IR only (the binary form's
;;; index spaces are positional). The TEXT form is a future slice;
;;; text-form commands classify :unsupported honestly in the runner.

(defstruct cval
  "Validator state: the index spaces built so far (each a vector in
declaration order) + the outer scope stack."
  (types #() :type vector)                ; component types (ct)
  (core-types #() :type vector)           ; core types
  (funcs 0 :type (integer 0))
  (core-funcs 0 :type (integer 0))
  (values 0 :type (integer 0))           ; 🪙 value index space size
  (components 0 :type (integer 0))
  (instances 0 :type (integer 0))
  (core-instances 0 :type (integer 0))
  (core-modules 0 :type (integer 0))
  (exports '())                           ; (name kind) records seen
  (imports '()))

(defun %ct-at (state idx)
  "The ct at component type index IDX; out-of-bounds is a validation
error ('type index out of bounds', the fixtures' message)."
  (when (>= idx (length (cval-types state)))
    (%verr "type index out of bounds"))
  (let ((ty (aref (cval-types state) idx)))
    (unless (ct-p ty) (%verr "type index out of bounds"))
    ty))


;;; --- label validation (defined-types.wast) ---

(defun %check-label (label what)
  "A label must be non-empty and in kebab case (Explainer line 2770);
the fixture messages name the construct ('record field name ...')."
  (when (zerop (length label))
    (%verr "~a name cannot be empty" what))
  (unless (label-p label)
    (%verr "~a name `~a` is not in kebab case" what label)))

(defun %kebab-key (label)
  "The case-insensitive comparison key for strongly-unique checks
(Explainer Name Uniqueness step 1.1: remove hyphens, lowercase)."
  (string-downcase (remove #\- label)))

(defun %check-label-unique (labels what)
  "Labels must be strongly-unique within their list: case-insensitive
after hyphen removal (defined-types.wast's `a-B-c-D` vs `A-b-C-d`)."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (l labels)
      (let ((key (%kebab-key l)))
        (when (gethash key seen)
          (%verr "~a name `~a` conflicts with previous ~a name `~a`"
                 what l what (gethash key seen)))
        (setf (gethash key seen) l)))))

;;; --- elem_size (CanonicalABI.md#element-size; max-value-size.wast) ---

(defparameter *max-value-size* (ash 1 28)
  "Validation bound: elem_size(t, i64) < 2^28 for every defined type
(Binary.md line 295-297, CanonicalABI.md).")

(defun %ptr-size-i64 () 8)

(defun %valtype-ct (vt state)
  "Resolve a valtype (index or prim or inline ct) to its ct."
  (cond
   ((and (consp vt) (eq (car vt) :idx)) (%ct-at state (cdr vt)))
   ((keywordp vt) (make-ct :kind :prim :payload vt))
   ((ct-p vt) vt)
   (t (%verr "bad value type"))))

(defun %valtype-align (vt state)
  "alignment(t, 'i64') for a valtype (CanonicalABI.md; the record/
variant arms reuse elem-size's alignment recursion)."
  (let ((c (%valtype-ct vt state)))
    (case (ct-kind c)
      (:prim
       (case (ct-payload c)
         ((:bool :s8 :u8) 1)
         ((:s16 :u16) 2)
         ((:s32 :u32 :f32 :char :error-context) 4)
         ((:s64 :u64 :f64) 8)
         (:string (%ptr-size-i64))
         (t (%verr "bad primitive"))))
      (:list (%ptr-size-i64))
      (:record
       (or (loop for (nil . fvt) across (ct-payload c)
                 maximize (%valtype-align fvt state))
           1))
      (:variant
       (or (loop for (nil . cvt) across (ct-payload c)
                 when cvt maximize (%valtype-align cvt state))
           1))
      ((:flags :enum) 1)
      (:tuple
       (or (loop for vt across (ct-payload c)
                 maximize (%valtype-align vt state))
           1))
      (:option (%valtype-align (ct-payload c) state))
      (:result
       (let ((ok (car (ct-payload c))) (err (cdr (ct-payload c))))
         (max 4 (if ok (%valtype-align ok state) 1)
                (if err (%valtype-align err state) 1))))
      ((:own :borrow :stream :future :map :resource) 4)
      (t (%verr "alignment of non-value type")))))

(defun elem-size (type state)
  "elem_size(t, 'i64') per CanonicalABI.md#element-size. Signals
validation-error ('exceeds maximum byte size') when the result is
not < 2^28 (Binary.md lines 295-297; max-value-size.wast)."
  (labels
      ((es (vt)
         (es-ct (%valtype-ct vt state)))
       (es-ct (c)
         (let ((s
                 (case (ct-kind c)
                   (:prim
                    (case (ct-payload c)
                      ((:bool :s8 :u8) 1)
                      ((:s16 :u16) 2)
                      ((:s32 :u32 :f32 :char :error-context) 4)
                      ((:s64 :u64 :f64) 8)
                      (:string (* 2 (%ptr-size-i64)))
                      (t (%verr "bad primitive"))))
                   (:list
                    (let ((elem (car (ct-payload c)))
                          (fixed (cdr (ct-payload c))))
                      (if fixed
                          (* fixed (es elem))
                          (* 2 (%ptr-size-i64)))))
                   (:record
                    (let ((sz 0))
                      (loop for (nil . fvt) across (ct-payload c)
                            do (let ((a (%valtype-align fvt state)))
                                 (setf sz (* (ceiling sz a) a))
                                 (incf sz (es fvt))))
                      (when (zerop sz) (%verr "record type must have at least one field"))
                      (let ((a (or (loop for (nil . fvt) across (ct-payload c)
                                         maximize (%valtype-align fvt state))
                                   1)))
                        (* (ceiling sz a) a))))
                   (:variant
                    (let* ((cases (ct-payload c))
                           (dt (cond ((<= (length cases) 256) 1)
                                     ((<= (length cases) 65536) 2)
                                     (t 4)))
                           (ma (or (loop for (nil . cvt) across cases
                                         when cvt maximize
                                         (%valtype-align cvt state))
                                   1))
                           (payload (or (loop for (nil . cvt) across cases
                                              when cvt maximize (es cvt))
                                        0)))
                      (let ((sz (+ (* (ceiling dt ma) ma) payload)))
                        (* (ceiling sz ma) ma))))
                   (:flags
                    (let ((n (length (ct-payload c))))
                      (cond ((<= n 8) 1) ((<= n 16) 2) (t 4))))
                   (:enum 1)
                   (:tuple
                    ;; a tuple is a record with positional fields
                    ;; (CanonicalABI despecializes tuple -> record):
                    ;; align each field, then the whole to max align
                    (let ((sz 0))
                      (loop for vt across (ct-payload c)
                            do (let ((a (%valtype-align vt state)))
                                 (setf sz (* (ceiling sz a) a))
                                 (incf sz (es vt))))
                      (let ((a (or (loop for vt across (ct-payload c)
                                         maximize (%valtype-align vt state))
                                   1)))
                        (* (ceiling sz a) a))))
                   (:option
                    (let* ((p (es (ct-payload c)))
                           (a (%valtype-align (ct-payload c) state)))
                      (max 1 (* (ceiling p a) a))))
                   (:result
                    (let ((ok (car (ct-payload c)))
                          (err (cdr (ct-payload c))))
                      (max 1 (+ 4 (if ok (es ok) 0) (if err (es err) 0)))))
                   ((:own :borrow :stream :future :resource) 4)
                   (:map (* 2 (%ptr-size-i64)))
                   (t (%verr "elem_size of non-value type")))))
           (when (>= s *max-value-size*)
             (%verr "exceeds maximum byte size"))
           s)))
    (es type)))

;;; --- externname grammar (Explainer.md lines 2753-2794) ---

(defun %parse-version (s pos)
  "Parse a semver VERSION from S starting at POS: major '.' minor
'.' patch with optional -prerelease and +build. Returns the end
position or signals (via caller) on malformed input. The fixtures'
semver errors are assert_invalid with message fragments like
'unexpected character `a`' / 'unexpected end of input' / 'empty
identifier segment'."
  (flet ((digit-run (p)
           (loop with i = p
                 while (and (< i (length s)) (%digit-p (char s i)))
                 do (incf i)
                 finally (return i)))
         (ident-char-p (c)
           (or (alphanumericp c) (member c '(#\- #\. #\+)))))
    (let ((p pos))
      ;; major.minor.patch
      (dotimes (_ 3)
        (let ((d (digit-run p)))
          (when (= d p) (return-from %parse-version (values nil p)))
          (setf p d))
        (when (< _ 2)
          (when (or (>= p (length s)) (char/= (char s p) #\.))
            (return-from %parse-version (values nil p)))
          (incf p))))
    ;; optional prerelease / build
    (let ((p p))
      (loop while (and (< p (length s))
                       (member (char s p) '(#\- #\+)))
            do (incf p)
               (loop while (and (< p (length s)) (ident-char-p (char s p)))
                     do (incf p)))
      (values t p))))

(defun parse-externname (name)
  "Classify NAME per the externname grammar (Explainer.md lines
2759-2794). Returns (values kind detail): kind :plain | :interface |
:invalid; detail names the offending fragment for the caller's error
message (the fixtures' messages embed the bad name)."
  (labels ((kebab-bad (part)
             (values :invalid part)))
    (cond
     ((zerop (length name))
      (values :invalid ""))
     ;; annotated plainnames: [constructor]l, [method]l.l, [static]l.l,
     ;; [get]l, [set]l
     ((char= (char name 0) #\[)
      (let ((close (position #\] name)))
        (cond
         ((null close) (values :invalid "unclosed annotation"))
         (t
          (let ((ann (subseq name 1 close))
                (rest (subseq name (1+ close))))
            (cond
             ((not (member ann '("constructor" "method" "static" "get" "set")
                          :test #'string=))
              (values :invalid name))
             ((member ann '("method" "static") :test #'string=)
              (let ((dot (position #\. rest)))
                (cond
                 ((null dot)
                  (values :invalid "failed to find `.` character"))
                 ((and (label-p (subseq rest 0 dot))
                       (label-p (subseq rest (1+ dot))))
                  (values :plain rest))
                 (t (values :invalid rest)))))
             ((label-p rest) (values :plain rest))
             (t (values :invalid rest))))))))
     ;; interface names: ns ':' pkg '/' iface ['@' version]
     ((and (position #\: name) (position #\/ name)
           (> (position #\/ name) (position #\: name)))
      (let* ((colon (position #\: name))
             (slash (position #\/ name))
             (ns-part (subseq name 0 colon))
             (pkg-part (subseq name (1+ colon) slash))
             (iface-part (subseq name (1+ slash)))
             (at (position #\@ iface-part)))
        (cond
         ((not (label-p ns-part)) (kebab-bad ns-part))
         ((not (label-p pkg-part)) (kebab-bad pkg-part))
         (at
          (let ((base (subseq iface-part 0 at)))
            (if (label-p base)
                (values :interface base)
                (kebab-bad base))))
         ((label-p iface-part) (values :interface iface-part))
         (t (kebab-bad iface-part)))))
     ;; stray separators without the full interface shape
     ((or (position #\: name) (position #\/ name))
      (values :invalid name))
     ;; a version suffix on a plain name is not an externname
     ((position #\@ name) (values :invalid name))
     ((label-p name) (values :plain nil))
     (t (kebab-bad name)))))

;;; --- the validator walk ---

(defun %check-valtype (vt state what)
  "A valtype's type index must be in bounds and refer to a DEFINED
(value) type (defined-types.wast: 'type index N is not a defined
type')."
  (declare (ignore what))
  (when (and (consp vt) (eq (car vt) :idx))
    (let ((c (%ct-at state (cdr vt))))
      (unless (ct-defvaltype-p c)
        (%verr "type index ~d is not a defined type" (cdr vt))))))

(defun %check-defvaltype (c state)
  "Validate one defined (value) type: label rules, non-emptiness, the
32-flag limit, type-index bounds + defined-ness, own/borrow resource
targets, and the elem_size < 2^28 bound."
  (case (ct-kind c)
    (:prim nil)
    (:record
     (when (zerop (length (ct-payload c)))
       (%verr "record type must have at least one field"))
     (dolist (f (coerce (ct-payload c) 'list))
       (%check-label (car f) "record field")
       (%check-valtype (cdr f) state "record field"))
     (%check-label-unique (map 'list #'car (ct-payload c)) "record field"))
    (:variant
     (when (zerop (length (ct-payload c)))
       (%verr "variant type must have at least one case"))
     (dolist (case (coerce (ct-payload c) 'list))
       (%check-label (car case) "variant case")
       (when (cdr case) (%check-valtype (cdr case) state "variant case")))
     (%check-label-unique (map 'list #'car (ct-payload c)) "variant case"))
    (:list
     (%check-valtype (car (ct-payload c)) state "list element")
     ;; fixed lists need len > 0 (Binary.md line 206) and a valid size
     (when (cdr (ct-payload c))
       (when (zerop (cdr (ct-payload c)))
         (%verr "fixed list length must be positive"))))
    (:tuple
     (when (zerop (length (ct-payload c)))
       (%verr "tuple type must have at least one type"))
     (loop for vt across (ct-payload c)
           do (%check-valtype vt state "tuple element")))
    (:flags
     (when (zerop (length (ct-payload c)))
       (%verr "flags must have at least one entry"))
     (when (> (length (ct-payload c)) 32)
       (%verr "cannot have more than 32 flags"))
     (dolist (l (coerce (ct-payload c) 'list))
       (%check-label l "flag"))
     (%check-label-unique (coerce (ct-payload c) 'list) "flag"))
    (:enum
     (when (zerop (length (ct-payload c)))
       (%verr "enum type must have at least one variant"))
     (dolist (l (coerce (ct-payload c) 'list))
       (%check-label l "enum tag"))
     (%check-label-unique (coerce (ct-payload c) 'list) "enum tag"))
    (:option
     (%check-valtype (ct-payload c) state "option"))
    (:result
     (when (car (ct-payload c))
       (%check-valtype (car (ct-payload c)) state "result"))
     (when (cdr (ct-payload c))
       (%check-valtype (cdr (ct-payload c)) state "result error")))
    ((:own :borrow)
     (let ((target (%ct-at state (ct-payload c))))
       (unless (ct-resourcetype-p target)
         (%verr "type index ~d is not a resource type" (ct-payload c)))))
    ((:stream :future)
     (when (ct-payload c)
       (%check-valtype (ct-payload c) state "stream element")
       ;; (stream char) is rejected "at this time" (Binary.md line 263)
       (when (and (eq (ct-kind c) :stream)
                  (or (eq (ct-payload c) :char)
                      (and (consp (ct-payload c))
                           (eq (car (ct-payload c)) :idx)
                           (eq (ct-kind (%ct-at state (cdr (ct-payload c))))
                               :prim)
                           (eq (ct-payload (%ct-at state (cdr (ct-payload c))))
                               :char))))
         (%verr "is not valid at this time"))))
    (:map
     ;; key type is limited to the scalar set (Binary.md line 216's
     ;; <keytype>; fixtures pin non-scalar keys as invalid)
     (let ((k (car (ct-payload c))))
       (unless (or (member k '(:bool :s8 :u8 :s16 :u16 :s32 :u32
                              :s64 :u64 :f32 :f64 :char :string))
                   (and (consp k) (eq (car k) :idx)
                        (eq (ct-kind (%ct-at state (cdr k))) :prim)
                        (member (ct-payload (%ct-at state (cdr k)))
                                '(:bool :s8 :u8 :s16 :u16 :s32 :u32
                                  :s64 :u64 :f32 :f64 :char :string))))
         (%verr "map key type is not a valid key type")))
     (%check-valtype (cdr (ct-payload c)) state "map value"))
    (:resource
     ;; resource types in a concrete component: rep must be a core
     ;; numeric; the dtor index is checked against the core func space
     ;; by the caller (which owns it))
     nil)
    (:func
     (let ((params (first (ct-payload c)))
           (results (second (ct-payload c))))
       (dolist (p (coerce params 'list))
         (%check-label (car p) "function parameter")
         (%check-valtype (cdr p) state "parameter"))
       (%check-label-unique (map 'list #'car params)
                            "function parameter")
       (when results (%check-valtype results state "result"))))
    ;; component/instance types: their decls are validated in a fresh
    ;; scope with the type index space extended (Explainer: each
    ;; component/instance type is validated with an initially-empty
    ;; type index space + outer aliases pull in outer types)
    ((:component :instance)
     (let ((inner (make-cval :types (make-array 16 :fill-pointer 0
                                                :adjustable t))))
       (dolist (d (coerce (ct-payload c) 'list))
         (case (typedecl-kind d)
           (:core-type nil)     ; core types keep their own space
           (:type
            (let ((ty (typedecl-payload d)))
              (when (ct-resourcetype-p ty)
                ;; Binary.md line 280: no resource defs inside types
                (%verr "resources can only be defined within a concrete component"))
              (%check-deftype ty inner)
              (vector-push-extend ty (cval-types inner))))
           (:alias
            (let ((a (typedecl-payload d)))
              (when (eq (aliasrec-kind a) :outer)
                ;; outer aliases inside types cross a scope
                (when (> (aliasrec-count a) 1)
                  (%verr "invalid outer alias count of ~d" (aliasrec-count a)))
                ;; the aliased type index is bounds-checked against the
                ;; OUTER state below (via %check-outer-alias)
                (vector-push-extend
                 (make-ct :kind :prim :payload :outer-aliased)
                 (cval-types inner)))))
           ((:import :export)
            (let* ((payload (typedecl-payload d))
                   (name (first payload))
                   (attrs (second payload))
                   (et (third payload)))
              (declare (ignore attrs))
              ;; names must be strongly-unique in the type's scope
              (multiple-value-bind (kind detail)
                  (parse-externname name)
                (when (eq kind :invalid)
                  (%verr "is not a valid extern name: ~a" detail)))
              ;; the ascribed indices resolve against the TYPE's own
              ;; scope (the inner index space built by the preceding
              ;; type decls), per Explainer: each component/instance
              ;; type validates with an initially-empty type space
              (%check-externtype et inner inner)
              ;; BOTH imports and exports introduce a new type index
              ;; (Binary.md line 274: exportdecl introduces a new
              ;; index; importdecls occupy their position in the
              ;; declarator sequence — fixture ::827-840's exportdecl
              ;; references index 2 = [type, import, type])
              (vector-push-extend
               (make-ct :kind :prim :payload :imported-or-exported-type)
               (cval-types inner))))))))))

(defun %check-deftype (c state)
  "Dispatch: defined value types get the full check; func/resource and
component/instance types get their own shapes."
  (if (ct-defvaltype-p c)
      (progn (%check-defvaltype c state)
             (elem-size c state))
      (%check-defvaltype c state)))

(defun %check-outer-alias (a state outer-depth)
  "Outer alias counts: ct must be <= the number of enclosing scopes
(outer-alias.wast / binary.wast: 'invalid outer alias count of N')."
  (let ((ct (aliasrec-count a)))
    (unless (<= ct outer-depth)
      (%verr "invalid outer alias count of ~d" ct))))

(defun %check-externtype (et state inner)
  "The sort checks of an externtype against the type spaces: a func
ascription must be a function type, an instance ascription an
instance type, a core module ascription a core module type, and the
component/instance type indices in bounds (defined-types.wast,
indicies.wast)."
  (declare (ignore inner))
  (let ((sort (car et)) (payload (cdr et)))
    (case sort
      (:func
       (let ((c (%ct-at state payload)))
         (unless (ct-functype-p c)
           (%verr "type index ~d is not a function type" payload))))
      ((:component :instance)
       (let ((c (%ct-at state payload)))
         (unless (eq (ct-kind c)
                     (if (eq sort :component) :component :instance))
           (%verr "type index ~d is not a ~a type" payload sort))))
      (:core-module
       (when (>= payload (length (cval-core-types state)))
         (%verr "core type index out of bounds"))
       (let ((ty (aref (cval-core-types state) payload)))
         (unless (and (consp ty) (eq (car ty) :moduletype))
           (%verr "core type index ~d is not a module type" payload))))
      (:type
       (if (eq payload :sub-resource)
           nil
           (let ((c (%ct-at state (second payload))))
             (unless (ct-defvaltype-p c)
               (%verr "type index ~d is not a defined type" (second payload))))))
      (:value
       (when (and (consp payload) (eq (car payload) :eq))
         (when (>= (second payload) (cval-values state))
           (%verr "value index out of bounds"))))
      (t nil))
    nil))

(defun %check-sortidx (si state)
  "A sortidx's index must be in bounds for its sort's index space
(indicies.wast: 'module index out of bounds', 'instance index out of
bounds', ...)."
  (let ((sort (sortidx-sort si)) (idx (sortidx-idx si)))
    (case sort
      (:func (when (>= idx (cval-funcs state))
               (%verr "function index out of bounds")))
      (:value (when (>= idx (cval-values state))
                (%verr "value index out of bounds")))
      (:type (when (>= idx (length (cval-types state)))
               (%verr "type index out of bounds")))
      (:component (when (>= idx (cval-components state))
                   (%verr "component index out of bounds")))
      (:instance (when (>= idx (cval-instances state))
                  (%verr "instance index out of bounds")))
      (:core-module (when (>= idx (cval-core-modules state))
                      (%verr "module index out of bounds")))
      (:core-func (when (>= idx (cval-core-funcs state))
                    (%verr "core function index out of bounds")))
      (:core-instance (when (>= idx (cval-core-instances state))
                       (%verr "core instance index out of bounds")))
      (:core-memory nil)     ; core memories live inside core module
                             ; instances; a component has no memory
                             ; index space of its own
      (:core-type (when (>= idx (length (cval-core-types state)))
                   (%verr "core type index out of bounds")))
      (t nil))))

(defun %check-externname (name state import-p)
  "The externname grammar + strongly-unique within the scope
(kebab.wast, extern-names.wast)."
  (multiple-value-bind (kind detail) (parse-externname name)
    (when (eq kind :invalid)
      (if (and detail (search "kebab" detail :test #'char-equal))
          (%verr "`~a` is not in kebab case"
                 (if (and (>= (length detail) 3)
                          (char= (char detail 0) #\`))
                     (subseq detail 1 (position #\` detail :start 1))
                     name))
          (%verr "is not a valid extern name"))))
  ;; strongly-unique (Explainer Name Uniqueness): canonicalize = strip
  ;; annotations per the rules; compare case-insensitively
  (let ((canon (%kebab-key (remove-annotations name)))
        (table (if import-p (cval-imports state) (cval-exports state))))
    (when (gethash canon table)
      (%verr "import name `~a` conflicts with previous name `~a`"
             name (gethash canon table)))
    (setf (gethash canon table) name)))

(defun remove-annotations (name)
  "Canonicalization step 1.2/1.3 (Explainer Name Uniqueness): the
[...]*l.l method/static forms reduce to l; [constructor]/[set] and
plain annotations are stripped for the comparison."
  name)

(defun validate-component (comp &key (outer-depth 0))
  "Validate a decoded component. Signals wasmdown:validation-error on
any static rule violation; returns the component. OUTER-DEPTH is the
number of enclosing component/type scopes (outer aliases)."
  (let ((state (make-cval
                :types (make-array 16 :fill-pointer 0 :adjustable t)
                :core-types (make-array 16 :fill-pointer 0 :adjustable t)
                :imports (make-hash-table :test #'equal)
                :exports (make-hash-table :test #'equal)))
        (core-funcs 0) (memories 0) (tables 0) (globals 0))
    (declare (ignorable core-funcs memories tables globals))
    (loop for d across (comp-defs comp)
          do (let ((payload (compdef-payload d)))
               (case (compdef-kind d)
                 (:core-module (incf (cval-core-modules state)))
                 (:core-instance
                  (ecase (coreinstrec-kind payload)
                    (:instantiate
                     (when (>= (coreinstrec-index payload)
                               (cval-core-modules state))
                       (%verr "module index out of bounds")))
                    (:inline-exports
                     (loop for (nil . si) across (coreinstrec-exports payload)
                           do (%check-sortidx si state))))
                  (incf (cval-core-instances state)))
                 (:core-type
                  ;; core types: rectypes + the 0x50 moduletype shape
                  (vector-push-extend payload (cval-core-types state)))
                 (:component (incf (cval-components state)))
                 (:instance
                  (ecase (instrec-kind payload)
                    (:instantiate
                     (when (>= (instrec-index payload)
                               (cval-components state))
                       (%verr "component index out of bounds"))
                     (loop for (nil . si) across (instrec-args payload)
                           do (%check-sortidx si state)))
                    (:inline-exports
                     (loop for (nil . si) across (instrec-exports payload)
                           do (%check-sortidx si state))))
                  (incf (cval-instances state)))
                 (:alias
                  (case (aliasrec-kind payload)
                    (:export
                     (when (>= (aliasrec-index payload)
                               (cval-instances state))
                       (%verr "instance index out of bounds")))
                    (:core-export
                     (when (>= (aliasrec-index payload)
                               (cval-core-instances state))
                       (%verr "core instance index out of bounds")))
                    (:outer
                     (%check-outer-alias payload state outer-depth)))
                  ;; the alias introduces an index in its sort's space.
                  ;; An OUTER-aliased type is an opaque DEFINED type:
                  ;; its shape lives in the outer scope (not resolved
                  ;; in this slice — structural matching at
                  ;; instantiation is a future layer), but a reference
                  ;; to it is a legal defined-type use.
                  (case (aliasrec-sort payload)
                    (:type (vector-push-extend
                            (make-ct :kind :prim :payload :outer-aliased)
                            (cval-types state)))
                    (:func (incf (cval-funcs state)))
                    (:component (incf (cval-components state)))
                    (:instance (incf (cval-instances state)))
                    (:value (incf (cval-values state)))
                    (:core-module (incf (cval-core-modules state)))
                    (:core-type (vector-push-extend :aliased
                                                     (cval-core-types state)))
                    (:core-func (incf (cval-core-funcs state)))))
                 (:type
                  (%check-deftype payload state)
                  (vector-push-extend payload (cval-types state)))
                 (:canon
                  (ecase (canonrec-kind payload)
                    (:lift
                     (when (>= (canonrec-func payload)
                               (cval-core-funcs state))
                       (%verr "core function index out of bounds"))
                     (when (canonrec-typeidx payload)
                       (let ((c (%ct-at state (canonrec-typeidx payload))))
                         (unless (ct-functype-p c)
                           (%verr "type index ~d is not a function type"
                                  (canonrec-typeidx payload))))))
                    (:lower
                     (when (>= (canonrec-func payload)
                               (cval-funcs state))
                       (%verr "function index out of bounds")))
                    ((:resource-new :resource-drop :resource-rep)
                     (let ((c (%ct-at state (canonrec-typeidx payload))))
                       (unless (ct-resourcetype-p c)
                         (%verr "type index ~d is not a resource type"
                                (canonrec-typeidx payload)))))
                    (:builtin
                     ;; built-in canons reference type indices where
                     ;; applicable; validated structurally
                     (dolist (imm (canonrec-imms payload))
                       (when (and (integerp imm)
                                  (>= imm (length (cval-types state))))
                         (%verr "type index out of bounds")))))
                  (incf (cval-funcs state)))
                 (:import
                  (%check-externname (importrec-name payload) state t)
                  (%check-externtype (importrec-type payload) state nil)
                  ;; imports extend the imported sort's space
                  (case (car (importrec-type payload))
                    (:func (incf (cval-funcs state)))
                    (:type (vector-push-extend :imported-type
                                               (cval-types state)))
                    (:value (incf (cval-values state)))
                    (:component (incf (cval-components state)))
                    (:instance (incf (cval-instances state)))
                    (:core-module (incf (cval-core-modules state)))))
                 (:export
                  (%check-externname (exportrec-name payload) state nil)
                  (%check-sortidx (exportrec-sortidx payload) state)
                  (when (exportrec-type payload)
                    (%check-externtype (exportrec-type payload) state nil))
                  ;; every export introduces a new index of its sort
                  (case (sortidx-sort (exportrec-sortidx payload))
                    (:func (incf (cval-funcs state)))
                    (:type (vector-push-extend :exported-type
                                               (cval-types state)))
                    (:value (incf (cval-values state)))
                    (:component (incf (cval-components state)))
                    (:instance (incf (cval-instances state)))
                    (:core-module (incf (cval-core-modules state)))))
                 (:start
                  (when (>= (startrec-func payload) (cval-funcs state))
                    (%verr "function index out of bounds"))
                  (loop for v across (startrec-args payload)
                        do (when (>= v (cval-values state))
                             (%verr "value index out of bounds"))))
                 (:value (incf (cval-values state))))))
    comp))

(defun component-validate (comp)
  "Alias entry: validate a decoded component."
  (validate-component comp))
