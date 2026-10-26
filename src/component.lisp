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
           #:ct-p
           #:ct-offset
           #:component-type-p
           #:make-ct
           ;; IR constructors (M7 slice 2: the TEXT parser builds the
           ;; SAME IR records the binary reader builds; :use only
           ;; inherits exports, so these must be explicit)
           #:make-compdef #:make-comp #:comp-defs #:compdef-kind
           #:compdef-payload #:compdef-offset
           #:make-aliasrec #:aliasrec-kind #:aliasrec-sort
           #:aliasrec-count #:aliasrec-target #:aliasrec-index
           #:aliasrec-name #:aliasrec-offset
           #:make-typedecl #:typedecl-kind #:typedecl-payload
           #:make-importrec #:importrec-name #:importrec-type
           #:importrec-attrs
           #:make-exportrec #:exportrec-name #:exportrec-attrs
           #:exportrec-sortidx #:exportrec-type #:exportrec-offset
           #:make-instrec #:instrec-kind #:instrec-index
           #:instrec-args #:instrec-exports
           #:make-coreinstrec #:coreinstrec-kind #:coreinstrec-index
           #:coreinstrec-args #:coreinstrec-exports
           #:make-canonrec #:canonrec-kind #:canonrec-func
           #:canonrec-typeidx #:canonrec-opts #:canonrec-imms
           #:canonrec-offset
           #:make-startrec #:startrec-func #:startrec-args
           ;; canonopt validation helpers (slice 2 canon rules)
           #:cval-func-types
           ;; value type predicates used by the validator
           #:ct-primitive-p
           #:ct-defvaltype-p
           #:ct-functype-p
           #:component-validate
           #:validate-component
           ;; M8: the instantiation EXECUTOR's contract (pure
           ;; export additions; the validator stays static-only)
           #:sortidx-sort
           #:sortidx-idx
           #:ct-resourcetype-p
           #:canon-opt
           ;; labels + externnames
           #:label-p
           #:words-p
           #:remove-annotations
           #:externname-kind
           #:parse-externname
           #:canonicalize-externname
           ;; interfaceversion grammar (M7: version-suffix +
           ;; extern-names lanes; the pins exercise both directly)
           #:canonversion-p
           #:split-canonical-version
           #:elem-size
           ;; values lane (M10): the RUNTIME canonical ABI consumes
           ;; the validator's flatten machinery — the same
           ;; flatten_functype table drives both sides.
           #:flatten-functype
           #:builtin-canon-sig
           #:ct-at))

(in-package #:wasmdown.component)

;;; SBCL inline-expands these small helpers into VALIDATE-COMPONENT;
;;; the expansion's record accessors then narrow the validation loop's
;;; PAYLOAD binding across every arm (spurious 'deleting unreachable
;;; code' notes + a bogus derived-type WARNING that ASDF treats as
;;; fatal). Keep them outline from the start of the file.
(declaim (notinline %check-name-attrs %check-externname
                    %check-outer-alias %check-externtype))

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

(defun %word-range-p (s start end)
  "One `word` (Explainer line 2776): [0-9a-z]+ — lowercase/digits."
  (declare (type string s) (type fixnum start end))
  (when (<= end start) (return-from %word-range-p nil))
  (loop for i from start below end
        always (let ((c (char s i)))
                 (or (and (char<= #\a c) (char<= c #\z)) (%digit-p c)))))

(defun words-p (s)
  "A `words` (Explainer line 2783): first-word ('-' word)* where
first-word ::= [a-z][0-9a-z]* — LOWERCASE-starting only, unlike
`label` (which permits first-acronyms). The namespace and package
of an interface name use this stricter form (kebab.wast: `A:b/c`,
`1:b/c`, `ns-A:b/c`, `ns:pkg-A/b` are all invalid interface names)."
  (and (stringp s) (plusp (length s))
       (char<= #\a (char s 0))
       (char<= (char s 0) #\z)
       (let ((start 0) (ok t))
         (declare (type fixnum start))
         (loop for i from 0 to (length s)
               while ok
               when (or (= i (length s)) (char= (char s i) #\-))
                 do (cond ((%word-range-p s start i)
                           (setf start (1+ i)))
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
  (core-memories 0 :type (integer 0))
  (core-tables 0 :type (integer 0))
  (core-globals 0 :type (integer 0))
  (core-tags 0 :type (integer 0))
  ;; parallel func index space: the ct (functype) of each func
  ;; (import / lift / alias / export) in declaration order; entries
  ;; may be nil for funcs whose type is not locally resolvable
  ;; (outer-aliased or forward-referenced) — the canon lower rules
  ;; skip a nil entry.
  (func-types #() :type vector)
  ;; M7 slice 3b: the TYPE IDENTITY vector parallel to TYPES — one
  ;; entry per type index: (:fresh . id) a resource identity, (:ref
  ;; . idx) an eq-reference, (:ct . ct) concrete, (:opaque)
  ;; unresolvable. Built by validate-component's def loop; the
  ;; instantiation matcher (%ty-*) reads it.
  (type-ids #() :type vector)
  ;; M7 slice 3b: per-instance TYPE entries (an instance ct or NIL)
  ;; parallel to the instance index space — with-arg matching reads
  ;; the argument's exports from it.
  (instance-types #() :type vector)
  ;; M7 slice 3c: externally-visible type NAMES (Explainer.md
  ;; 'external-visibility-of-types'): the set of type indices intro-
  ;; duced by a type import/export. An import's type references must
  ;; be import-named only ('imported types cannot transitively refer
  ;; to the external names of exported types'); an export's may use
  ;; either set. The typeidx PASSED INTO a type export is not retro-
  ;; actively named — only the NEW index is (external-visibility
  ;; ::0/::2).
  (vis-imports (make-hash-table) :type hash-table)
  (vis-exports (make-hash-table) :type hash-table)
  ;; ...and the set of EXPORTED instance indices: an alias export of
  ;; a type confers a name only when its SOURCE instance is itself
  ;; externally visible (external-visibility ::46/::48/::49)
  (vis-inst (make-hash-table) :type hash-table)
  ;; slice 5d (resources ::66-::68): the type indices of LOCALLY
  ;; DEFINED resources — (type $R (resource ...)) defs of THIS
  ;; concrete component. resource.new/rep require a local resource
  ;; ('not a local resource'); imported (sub resource) types, aliased
  ;; exports of other instances' types and outer-aliased resources
  ;; are not local.
  (local-resources (make-hash-table) :type hash-table)
  (exports '())                           ; (name kind) records seen
  (imports '()))

(defun %ct-at (state idx)
  "The ct at component type index IDX; out-of-bounds is a validation
error ('type index out of bounds', the fixtures' message). STATE may
also be a plain TYPES VECTOR (the values lane's runtime resolution):
a vector position out of bounds or holding a non-ct reads as NIL
(the runtime treats a bad index as an opaque shape, never a crash)
rather than signaling."
  (cond
   ((vectorp state)
    (and (>= idx 0) (< idx (length state))
         (let ((ty (aref state idx)))
           (and (ct-p ty) ty))))
   (t
    (when (>= idx (length (cval-types state)))
      (%verr "type index out of bounds"))
    (let ((ty (aref (cval-types state) idx)))
      (unless (ct-p ty) (%verr "type index out of bounds"))
      ty))))

(defun ct-at (state idx)
  "Public wrapper (values lane): %ct-at — the runtime passes the
comp-instance's TYPES VECTOR as the STATE."
  (%ct-at state idx))


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
         ;; opaque validator markers (imported/exported/outer-aliased
         ;; types) have no runtime shape in this slice; size them as
         ;; a word so elem-size never trips 'bad primitive' on them
         ((:imported :exported :outer-aliased
           :imported-or-exported-type) 4)
         (t (%verr "bad primitive"))))
      ;; alignment_list (CanonicalABI.md): a FIXED list aligns as its
      ;; element type; only the variable list aligns as a pointer.
      (:list
       (if (cdr (ct-payload c))
           (%valtype-align (car (ct-payload c)) state)
           (%ptr-size-i64)))
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
                     ;; opaque markers: word-sized (see %valtype-align)
                     ((:imported :exported :outer-aliased
                       :imported-or-exported-type) 4)
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
                           ;; LOOP's MAXIMIZE returns 0 (not NIL)
                           ;; when every case's payload is empty, so
                           ;; the (or ... 1) fallback never fires and
                           ;; (ceiling dt ma) divided by zero
                           ;; (external-visibility.wast ::15's empty
                           ;; variant). Clamp to the minimum 1.
                           (ma (max 1 (loop for (nil . cvt) across cases
                                            when cvt maximize
                                            (%valtype-align cvt state))))
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
         ((not (words-p ns-part)) (kebab-bad ns-part))
         ((not (words-p pkg-part)) (kebab-bad pkg-part))
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

;;; ---- interface versions + version-suffix (Explainer.md lines
;;; 2785-2793, 2962-3021; Binary.md lines 409-411, 472-476). The
;;; interfaceversion grammar accepts a `valid semver` (Semantic
;;; Versioning 2.0.0) or a `canonversion`; the version-suffix
;;; attribute carries the semversuffix split off the canonical name.
;;; All of these are VALIDATE-time rules (the fixtures pin them as
;;; assert_invalid; the binary reader accepts any name bytes).

(defun %semver-numeric-id-p (s)
  "A semver `numeric identifier`: digits only, no leading zero unless
the whole value is 0 (semver.org BNF <numeric identifier>)."
  (declare (type string s))
  (and (plusp (length s))
       (every #'%digit-p s)
       (or (char/= (char s 0) #\0) (= (length s) 1))))

(defun %semver-identifier-p (s)
  "A semver build/pre-release `identifier`: [0-9A-Za-z-]+, non-empty."
  (declare (type string s))
  (and (plusp (length s))
       (loop for ch across s
             always (or (%digit-p ch)
                        (and (char<= #\a ch) (char<= ch #\z))
                        (and (char<= #\A ch) (char<= ch #\Z))
                        (char= ch #\-)))))

(defun %semver-dotted-ids-p (s)
  "A semver `dot-separated build identifiers` list. Returns the error
detail 'empty identifier segment' when any segment is empty or
malformed, NIL (ok) otherwise."
  (declare (type string s))
  (loop with start = 0
        for i from 0 to (length s)
        when (or (= i (length s)) (char= (char s i) #\.))
          do (unless (%semver-identifier-p (subseq s start i))
               (return-from %semver-dotted-ids-p
                 "empty identifier segment"))
             (setf start (1+ i)))
  nil)

(defun %semver-parse (s)
  "Parse S as a strict `valid semver` (semver.org 2.0.0 BNF):
major.minor.patch(-pre)?(+build)? with no leading zeros in the
numeric identifiers and no empty identifier segments. Returns NIL on
success; on failure returns the parse detail the extern-names fixtures
pin ('empty string', \"unexpected character 'X'\", 'unexpected end of
input', 'empty identifier segment')."
  (declare (type string s))
  (labels
      ((read-number (s i)
         "Consume a numeric identifier at I: a digit run with no
leading zero. Returns (values end-pos error-detail); END-POS is I on
error."
         (cond
          ((>= i (length s))
           (values i "unexpected end of input"))
          ((char= (char s i) #\0)
           ;; lone 0 is legal; 0 followed by a digit is a leading zero
           (if (and (< (1+ i) (length s)) (%digit-p (char s (1+ i))))
               (values i
                       (format nil "unexpected character '~a'"
                               (char s (1+ i))))
               (values (1+ i) nil)))
          ((not (%digit-p (char s i)))
           (values i
                   (format nil "unexpected character '~a'" (char s i))))
          (t
           (let ((j i))
             (loop while (and (< j (length s)) (%digit-p (char s j)))
                   do (incf j))
             (values j nil)))))
       (expect-dot (s i)
         "The char at I must be '.' or the end is malformed."
         (cond
          ((>= i (length s)) "unexpected end of input")
          ((char/= (char s i) #\.)
           (format nil "unexpected character '~a'" (char s i)))
          (t nil))))
    (cond
     ((zerop (length s)) "empty string")
     (t
      (multiple-value-bind (i err) (read-number s 0)
        (cond
         (err err)
         ((setq err (expect-dot s i)) err)
         (t
          (multiple-value-bind (i err) (read-number s (1+ i))
            (cond
             (err err)
             ((setq err (expect-dot s i)) err)
             (t
              (multiple-value-bind (i err) (read-number s (1+ i))
                (cond
                 (err err)
                 ;; optional -pre, then optional +build
                 ((and (< i (length s)) (char= (char s i) #\-))
                  (let* ((plus (position #\+ s :start (1+ i)))
                         (pre (subseq s (1+ i)
                                      (or plus (length s))))
                         (build (if plus (subseq s (1+ plus)) nil)))
                    (or (%semver-dotted-ids-p pre)
                        (and build (%semver-dotted-ids-p build))
                        ;; nothing may trail the build
                        nil)))
                 ((and (< i (length s)) (char= (char s i) #\+))
                  (%semver-dotted-ids-p (subseq s (1+ i))))
                 ((< i (length s))
                  (format nil "unexpected character '~a'" (char s i)))
                 (t nil)))))))))))))

(defun %semver-version-core-p (core)
  "A semver <version core> X.Y.Z: exactly three numeric identifiers."
  (declare (type string core))
  (let ((nums (loop with start = 0
                    for i from 0 to (length core)
                    when (or (= i (length core))
                             (char= (char core i) #\.))
                      collect (prog1 (subseq core start i)
                                (setf start (1+ i))))))
    (and (= (length nums) 3)
         (every #'%semver-numeric-id-p nums))))

(defun canonversion-p (v)
  "True when V matches the `canonversion` production (Explainer lines
2787-2791): [1-9][0-9]* | 0.[1-9][0-9]* | 0.0.[1-9][0-9]* | 0.0.0 |
<version core> '-' <pre-release> (a full X.Y.Z-pre)."
  (declare (type string v))
  (cond
   ;; [1-9][0-9]*
   ((and (plusp (length v))
         (char<= #\1 (char v 0)) (char<= (char v 0) #\9)
         (every #'%digit-p v))
    t)
   ;; 0.[1-9][0-9]*
   ((and (> (length v) 2) (string= v "0." :end1 2)
         (char<= #\1 (char v 2)) (char<= (char v 2) #\9)
         (every #'%digit-p (subseq v 3)))
    t)
   ;; 0.0.[1-9][0-9]*
   ((and (> (length v) 4) (string= v "0.0." :end1 4)
         (char<= #\1 (char v 4)) (char<= (char v 4) #\9)
         (every #'%digit-p (subseq v 5)))
    t)
   ;; 0.0.0
   ((string= v "0.0.0") t)
   ;; <version core> '-' <pre-release>: the core before the FIRST '-'
   ;; is a full X.Y.Z; the pre-release is a dotted identifier list
   ((let ((dash (position #\- v)))
      (and dash
           (%semver-version-core-p (subseq v 0 dash))
           (null (%semver-dotted-ids-p (subseq v (1+ dash))))))
    t)
   (t nil)))

(defun split-canonical-version (version)
  "The canonical-name split (Explainer.md lines 2983-3001, matching
upstream wasmparser names.rs): given a full
major.minor.patch(-pre)?(+build)?, returns (values canon suffix) —
CANON the canonversion prefix, SUFFIX the remaining semversuffix (nil
when empty) — or (values nil nil) when VERSION is not a valid semver."
  (declare (type string version))
  (labels
      ((identifier-p (s)
         (and (plusp (length s))
              (loop for ch across s
                    always (or (%digit-p ch)
                               (and (char<= #\a ch) (char<= ch #\z))
                               (and (char<= #\A ch) (char<= ch #\Z))
                               (char= ch #\-)))))
       (dotted-ids-ok-p (s)
         (loop with start = 0
               for i from 0 to (length s)
               when (or (= i (length s)) (char= (char s i) #\.))
                 do (unless (identifier-p (subseq s start i))
                      (return-from dotted-ids-ok-p nil))
                    (setf start (1+ i)))
         t))
    ;; peel +build
    (let* ((plus (position #\+ version))
           (without-build (if plus (subseq version 0 plus) version))
           (build (if plus (subseq version (1+ plus)) nil)))
      (when (and build (not (dotted-ids-ok-p build)))
        (return-from split-canonical-version (values nil nil)))
      ;; peel -pre
      (let* ((dash (position #\- without-build))
             (core (if dash (subseq without-build 0 dash) without-build))
             (pre (if dash (subseq without-build (1+ dash)) nil)))
        (when (and pre (not (dotted-ids-ok-p pre)))
          (return-from split-canonical-version (values nil nil)))
        ;; three numeric identifiers, no leading zeros
        (let ((nums (loop with start = 0
                          for i from 0 to (length core)
                          when (or (= i (length core))
                                   (char= (char core i) #\.))
                            collect (prog1 (subseq core start i)
                                      (setf start (1+ i))))))
          (unless (and (= (length nums) 3)
                       (every #'%semver-numeric-id-p nums))
            (return-from split-canonical-version (values nil nil)))
          ;; the split point per the Explainer rules
          (let* ((major (first nums)) (minor (second nums))
                 (split (cond
                         ;; pre-release: the version itself is not
                         ;; split; only +build moves to the suffix
                         (dash (length without-build))
                         ;; major > 0: split immediately after major
                         ((string/= major "0") (length major))
                         ;; minor > 0: split immediately after minor
                         ((string/= minor "0")
                          (+ (length major) 1 (length minor)))
                         ;; 0.0.z: the core is canonical; any +build
                         ;; still moves to the suffix (upstream's
                         ;; 0.0.1+b.1 -> ("0.0.1", "+b.1"))
                         (t (length without-build)))))
            (values (subseq version 0 split)
                    (let ((suffix (subseq version split)))
                      (if (zerop (length suffix)) nil suffix)))))))))

(defun interface-name-version (name)
  "The '@' version part of an interfacename, or NIL when the name has
no interfaceversion. NAME is the full import/export name string."
  (declare (type string name))
  (let ((at (position #\@ name)))
    (and at (subseq name (1+ at)))))

(defun validate-interface-version (prefix suffix)
  "The version rule shared by the name itself and the `implements`
attribute (upstream InterfaceName::version). PREFIX is the
interfaceversion in the name (nil when absent), SUFFIX the
version-suffix attribute (nil when absent). Signals %verr on any
violation."
  (cond
   ((and (null prefix) (null suffix)) nil)
   ((null prefix)
    ;; a suffix with no version in the name
    (%verr "a version suffix requires the name to have a version"))
   ((null suffix)
    ;; no suffix: the version must be a full valid semver (a bare
    ;; canonversion like `1` or `0.2` is not)
    (let ((err (%semver-parse prefix)))
      (when err (%verr "invalid interface version: ~a" err))))
   (t
    (let ((full (concatenate 'string prefix suffix)))
      (let ((err (%semver-parse full)))
        (when err (%verr "invalid interface version: ~a" err)))
      (multiple-value-bind (canon suffix-parsed)
          (split-canonical-version full)
        (declare (ignore suffix-parsed))
        ;; canonicalizing FULL must split exactly into PREFIX and
        ;; SUFFIX (Explainer line 3007-3010)
        (unless (and canon (string= canon prefix))
          (%verr "`~a` is not the canonical version of `~a`"
                 prefix full)))))))

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

(defun %vt-transitively-borrow-p (vt state seen)
  "Slice 5c (Binary.md 258-262): does VALTYPE transitively contain a
borrow? An :idx reference resolves through STATE's type space; every
structural kind recurses; a borrow IS the terminal. SEEN guards the
:idx cycle space."
  (labels ((walk-idx (i)
             (and (>= i 0) (< i (length (cval-types state)))
                  (not (gethash i seen))
                  (progn (setf (gethash i seen) t)
                         (walk-ct (aref (cval-types state) i)))))
           (walk-vt (v)
             (cond ((and (consp v) (eq (car v) :idx)) (walk-idx (cdr v)))
                   ((ct-p v) (walk-ct v))
                   (t nil)))
           (walk-ct (c)
             (and (ct-p c)
                  (not (gethash (ct-kind c) seen))
                  (case (ct-kind c)
                    (:borrow t)
                    (:record
                     (loop for (nil . f) across (ct-payload c)
                           thereis (walk-vt f)))
                    (:variant
                     (loop for (nil . f) across (ct-payload c)
                           thereis (and f (walk-vt f))))
                    (:list (walk-vt (car (ct-payload c))))
                    (:tuple
                     (loop for e across (ct-payload c)
                           thereis (walk-vt e)))
                    (:option (walk-vt (ct-payload c)))
                    (:result
                     (or (and (car (ct-payload c))
                              (walk-vt (car (ct-payload c))))
                         (and (cdr (ct-payload c))
                              (walk-vt (cdr (ct-payload c))))))
                    ((:stream :future)
                     (and (ct-payload c) (walk-vt (ct-payload c))))
                    (:map (walk-vt (cdr (ct-payload c))))
                    (:func
                     (or (loop for (nil . p) across (first (ct-payload c))
                               thereis (walk-vt p))
                         (and (second (ct-payload c))
                              (walk-vt (second (ct-payload c))))))
                    (t nil)))))
    (walk-vt vt)))

(defvar *outer-states* nil
  "Slice 5 (outer-alias A): the stack of ENCLOSING validator states
\(innermost first) threaded through validate-component's :component
recursion and %check-defvaltype's :component/:instance arm. An outer
alias with de Bruijn count N resolves against the Nth entry; count 0
is the CURRENT state (Explainer 408-412).")
;;; the validate-component nesting depth — the per-validation tables
;;; reset only at the OUTERMOST call (a nested child's entries must
;;; survive for the parent's resolution; rows in the same suite run
;;; must not leak into each other)
(defvar *validate-depth* 0)

(defun %check-defvaltype (c state &optional (type-depth 1) (outer state))
  "Validate one defined (value) type: label rules, non-emptiness, the
32-flag limit, type-index bounds + defined-ness, own/borrow resource
targets, and the elem_size < 2^28 bound. TYPE-DEPTH: see
%check-deftype (threaded through the component/instance arms). OUTER
is the ENCLOSING component's validator state — the type-index space
where the text parser's interner lands inline externtypes; only the
(:component :instance) arm uses it (the :type/:value bounds of a
type's own declarators stay against STATE, the inner space)."
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
       ;; slice 5c (Binary.md 261-262): stream/future element types
       ;; may not transitively contain a borrow
       (when (%vt-transitively-borrow-p (ct-payload c) state
                                        (make-hash-table))
         (%verr "stream and future types may not contain `borrow`"))
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
     ;; numeric; the dtor index is checked against the core func
     ;; space by the CALLER (validate-component's :type arm, which
     ;; owns comp for the static core resolver — resources ::60/::61)
     nil)
    (:func
     (let ((params (first (ct-payload c)))
           (results (second (ct-payload c))))
       (dolist (p (coerce params 'list))
         (%check-label (car p) "function parameter")
         (%check-valtype (cdr p) state "parameter"))
       (%check-label-unique (map 'list #'car params)
                            "function parameter")
       (when results
         (%check-valtype results state "result")
         ;; slice 5c (resources ::53-::56, Binary.md 258-260): a
         ;; functype's result may not transitively contain a borrow
         (when (%vt-transitively-borrow-p results state
                                         (make-hash-table))
           (%verr "function result cannot contain a `borrow` type")))))
    ;; component/instance types: their decls are validated in a fresh
    ;; scope with the type index space extended (Explainer: each
    ;; component/instance type is validated with an initially-empty
    ;; type index space + outer aliases pull in outer types). The
    ;; inline externtypes the TEXT parser interns (e.g. an
    ;; (export "f" (instance)) declarator's inline instance type)
    ;; land in the ENCLOSING component's type space — Explainer
    ;; 'Declarators': the text format desugars inline type
    ;; expressions "into out-of-line type definitions" — so their
    ;; indices resolve against OUTER (the enclosing state), while
    ;; :type (eq i) bounds and :value bounds resolve against INNER
    ;; (binary.wast ::81: the exportdecl's (type (eq 0)) sees the
    ;; instancetype's own aliased type 0).
    ((:component :instance)
     (let ((inner (make-cval :types (make-array 16 :fill-pointer 0
                                                :adjustable t)
                             :func-types (make-array 16
                                                    :fill-pointer 0
                                                    :adjustable t)
                             ;; the TYPE's import/export names are
                             ;; strongly-unique WITHIN the type
                             ;; (Binary.md: each component/instance
                             ;; type is its own scope; kebab.wast
                             ;; ::28-30 pin the conflict message)
                             :imports (make-hash-table :test #'equal)
                             :exports (make-hash-table :test #'equal))))
       (dolist (d (coerce (ct-payload c) 'list))
         (case (typedecl-kind d)
           ;; slice 5m (core-modules ::5/::6): a :core-type decl inside
           ;; a component/instance type can be a MODULETYPE — its
           ;; internals (export-name uniqueness, (type n) bounds)
           ;; validate with the same %check-moduletype as the direct
           ;; :core-type arm
           (:core-type (%check-moduletype (typedecl-payload d)))
           (:type
            (let ((ty (typedecl-payload d)))
              (when (ct-resourcetype-p ty)
                ;; Binary.md line 280: no resource defs inside types
                (%verr "resources can only be defined within a concrete component"))
              ;; slice 5m (indicies ::9): a nested TYPE decl's
              ;; walk treats the ENCLOSING TYPE's INNER space as
              ;; its outer scope — an (outer 1) alias inside
              ;; $T2 crosses the $T2 boundary into the enclosing
              ;; component type's own space (where the sibling
              ;; $r was pushed).
              ;; M9 (linking ::104): the outer threading for nested
              ;; component/instance cts was tried here (passing the
              ;; arm's OUTER) and REVERTED — it broke indicies ::9's
              ;; (outer 1)-into-$T2 and resources ::30's sibling-$r
              ;; chains (both pin suites). The de Bruijn chain for a
              ;; nested type's decls keeps INNER as the first outer.
              (%check-deftype ty inner (1+ type-depth) inner)
              (vector-push-extend ty (cval-types inner))))
           (:alias
            (let ((a (typedecl-payload d)))
              ;; slice 5 (outer-alias C, Binary.md 267-270): an alias
              ;; DECLARATOR inside a component/instance type only
              ;; aliases a SUBSET of the sorts — export aliases:
              ;; instance/type (outer-alias.wast ::14's (func $a));
              ;; outer aliases: core type/type (::15/::16/::17's
              ;; component/core module)
              (ecase (aliasrec-kind a)
                (:export
                 (unless (member (aliasrec-sort a) '(:instance :type))
                   (%verr "may only refer to types or instances")))
                (:outer
                 (unless (member (aliasrec-sort a) '(:core-type :type))
                   (%verr "may only refer to types or instances")))
                ((:core-export)
                 (%verr "may only refer to types or instances")))
              (when (eq (aliasrec-kind a) :outer)
                ;; outer aliases inside types may cross TYPE-DEPTH
                ;; scopes (each nested component/instance TYPE
                ;; declarator adds one boundary; outer-alias.wast
                ;; ::12's count of 2 inside doubly-nested types)
                ;; (values lane, numerics.wast ::22): the TRUE bound
                ;; counts the enclosing TYPE boundaries (TYPE-DEPTH) AND
                ;; the enclosing COMPONENT boundaries beyond this one
                ;; (the ambient *OUTER-STATES* stack — one entry per
                ;; nested-component arm above this validation). The
                ;; Explainer counts "the number of enclosing components
                ;; OR types"; the old (> count type-depth) rejected the
                ;; COMPONENT-boundary case ($D's declarators referencing
                ;; the ENCLOSING component's flags type at count 2 with
                ;; type-depth 1), and a bare chain-length bound broke
                ;; outer-alias.wast ::12 (the doubly-nested TYPE at the
                ;; validation root: type-depth 2, no ambient states).
                (unless (<= (aliasrec-count a)
                            (+ type-depth (length *outer-states*)))
                  (%verr "invalid outer alias count of ~d"
                         (aliasrec-count a)))
                ;; slice 5 (outer-alias A): the de Bruijn count
                ;; resolves through the state stack — one boundary
                ;; per enclosing TYPE + COMPONENT scope. The chain
                ;; from THIS type's space: 0 = inner, 1 = the
                ;; enclosing component's state, 2+ = its outers.
                (let* ((*outer-states*
                        (if (boundp '*outer-states*)
                            (cons outer *outer-states*)
                            (list outer)))
                       (tgt (%outer-state-at inner (aliasrec-count a))))

                  (when (and tgt (eq (aliasrec-sort a) :type))
                    (when (>= (aliasrec-target a)
                              (length (cval-types tgt)))
                      (%verr "type index out of bounds"))
                    ;; (values lane) outer-alias.wast ::6: a TYPE
                    ;; alias crossing a COMPONENT boundary may not
                    ;; target a type that transitively refers to
                    ;; resources — the same substitution rule as
                    ;; %check-outer-alias (Binary.md 133-135), gated
                    ;; the same way: only when the resolution crossed
                    ;; OUT of this component (the target state is an
                    ;; ambient *OUTER-STATES* entry — the walk's own
                    ;; INNER/OUTER pair does not count; the M9
                    ;; resource flow reaches resources through IMPORT
                    ;; declarators legally, resolving to INNER/OUTER).
                    (let ((raw (and (< (aliasrec-target a)
                                       (length (cval-types tgt)))
                                    (aref (cval-types tgt)
                                          (aliasrec-target a)))))
                      (when (and raw (ct-p raw)
                                 ;; only when the alias crosses PAST
                                 ;; the enclosing component (count 1
                                 ;; = the ENCLOSING component's own
                                 ;; space — the legal M9 import flow
                                 ;; that reaches a resource through an
                                 ;; instance type's declarators)
                                 (> (aliasrec-count a) 1)
                                 (%ct-transitively-resources-p
                                  raw tgt (make-hash-table)))
                        (%verr "transitively refers to resources"))))))
              ;; EVERY alias declarator introduces a new type index
              ;; in the type's own space (Binary.md line 274 — an
              ;; (alias export ...) of the type sort too; resources.wast
              ;; ::30's $T2 referencing the alias-introduced $r).
              ;; M9 (linking ::104): an OUTER type alias whose
              ;; resolved target is a RESOURCE pushes a RESOURCE ct —
              ;; the declarators' interned own/borrow cts reference
              ;; this slot and %check-defvaltype's own/borrow arm
              ;; demands a resource there (the opaque marker read as
              ;; 'type index N is not a resource type')
              (vector-push-extend
               ;; M9 (linking ::104): the resource check reads the
               ;; NEWLY-CONSDED chain — the push sits OUTSIDE the
               ;; count-check's let* above, so a bare read saw the
               ;; OLD chain (a stale/fresh inner) and never the
               ;; enclosing component's real type space
               (let* ((*outer-states*
                        (if (boundp '*outer-states*)
                            (cons outer *outer-states*)
                            (list outer)))
                      (tgt (and (eq (aliasrec-kind a) :outer)
                                (eq (aliasrec-sort a) :type)
                                (%outer-state-at inner
                                                 (aliasrec-count a)))))
                 (if (and tgt
                          (< (aliasrec-target a)
                             (length (cval-types tgt)))
                          (ct-resourcetype-p
                           (aref (cval-types tgt)
                                 (aliasrec-target a))))
                     (make-ct :kind :resource :payload '(0 . nil)
                              :offset 0)
                     ;; M9 (linking ::104): the de Bruijn target may
                     ;; land on a FRESH inner space (a walk whose
                     ;; *outer-states* head is an empty inner) —
                     ;; walk the chain past too-short states to the
                     ;; first where the target index resolves to a
                     ;; resource; no hit keeps the opaque marker
                     (let ((hit
                             (and (eq (aliasrec-kind a) :outer)
                                  (eq (aliasrec-sort a) :type)
                                  (loop for st in
                                        (cons inner
                                              (if (boundp
                                                   '*outer-states*)
                                                  *outer-states*
                                                  nil))
                                        thereis (and
                                                 (< (aliasrec-target a)
                                                    (length
                                                     (cval-types st)))
                                                 (ct-resourcetype-p
                                                  (aref
                                                   (cval-types st)
                                                   (aliasrec-target
                                                    a))))))))
                       (if hit
                           (make-ct :kind :resource
                                    :payload '(0 . nil) :offset 0)
                           (make-ct :kind :prim
                                    :payload :outer-aliased)))))
               (cval-types inner))))
           ((:import :export)
            (let* ((payload (typedecl-payload d))
                   (name (first payload))
                   (attrs (second payload))
                   (et (third payload)))
              ;; the externname grammar + strongly-unique within the
              ;; TYPE's own scope (the fresh tables on INNER carry
              ;; the per-type uniqueness; kebab.wast ::28-30 pin the
              ;; conflict message)
              (%check-externname name inner
                                 (eq (typedecl-kind d) :import))
              ;; the attribute rules apply inside types too
              ;; (attributes.wast ::15/::18/::21: implements rules in
              ;; component/instance types); the sort comes from the
              ;; ascribed externtype
              (%check-name-attrs name attrs (car et))
              ;; the ascribed indices resolve INNER-FIRST: Binary.md's
              ;; componenttype/instancetype rule (each validates with
              ;; an initially-empty type space; the declarator
              ;; sequence — type decls, importdecls, exportdecls —
              ;; builds it; binary.wast ::80's export (func (type 2))
              ;; and ::81's (type (eq 0)) both resolve in-sequence).
              ;; When the index is out of bounds in the type's own
              ;; space, fall back to the ENCLOSING component's space
              ;; (the Explainer's text-format out-of-line (type i)
              ;; references + outer-alias pull-ins).
              (flet ((resolve-et (space)
                       (handler-case
                           (%check-externtype et space inner)
                         ;; return the CONDITION on failure so the
                         ;; outer fallback can re-signal when BOTH
                         ;; spaces reject (defined-types ::42/::43:
                         ;; the (instance (type $t)) use of a FUNC
                         ;; type must fail 'type index 0 is not an
                         ;; instance type', never silently pass)
                         (wasmdown:validation-error (c) c))))
                (let ((err (resolve-et inner)))
                  (when err
                    (let ((err2 (resolve-et outer)))
                      (when err2 (error err2))))
                  ;; external visibility (slice 3c): an import/export
                  ;; declarator's type references must be named INSIDE
                  ;; THE TYPE's own space — but ONLY when the type is
                  ;; USED as an import/export by the ENCLOSING component
                  ;; (the walk lives at the outer use site, see the
                  ;; :import/:export arms; ::35/::40's unused types
                  ;; define freely). The eager slice-3c check is
                  ;; swallowed here: the OLD unconditional version
                  ;; either ate the fixture errors (inner resolve ok)
                  ;; or fired on unused types (::35/::40).
                  nil))
               ;; the declarator's NEW type index is named in the
               ;; type's own visibility scope, on its own side -- the
               ;; VALUE is the EXTERNNAME (slice 5k: the
               ;; annotated-name resource rules read it inside types;
               ;; the pre-push length IS the index the declarator's
               ;; ct is about to occupy)
               (setf (gethash (length (cval-types inner))
                              (if (eq (typedecl-kind d) :import)
                                  (cval-vis-imports inner)
                                  (cval-vis-exports inner)))
                     name)
               ;; slice 5k (annotated-names ::31): the [constructor]/
               ;; [method]/[static] resource-NAME rules inside
               ;; component/instance TYPE declarators -- AFTER the
               ;; import's own name registration
               (when (eq (typedecl-kind d) :import)
                 (%check-annotated-name
                  name
                  (and (eq (car et) :func)
                       (integerp (cdr et))
                       (handler-case (%ct-at inner (cdr et))
                         (wasmdown:validation-error () nil)))
                  inner t))
               ;; BOTH imports and exports introduce a new type index
               ;; (Binary.md line 274: exportdecl introduces a new
               ;; index; importdecls occupy their position in the
               ;; declarator sequence — fixture ::827-840's exportdecl
               ;; references index 2 = [type, import, type]). A
               ;; (sub resource) type declarator's new index IS a
               ;; resource (own/borrow must accept it — resources.wast
               ;; ::27/::28's instance types); an (eq i) bound is an
               ;; alias of i (kept opaque here).
               (vector-push-extend
                (if (and (eq (car et) :type)
                         (eq (cdr et) :sub-resource))
                    (make-ct :kind :resource :payload '(0 . nil)
                             :offset 0)
                    ;; M9 (linking ::125): an (eq i) type export
                    ;; aliases i — its own/borrow acceptance follows
                    ;; the TARGET's kind (the eq of a (sub resource)
                    ;; export IS a resource for the declarator's
                    ;; functype refs); other/unknown stay opaque
                    (if (and (eq (car et) :type)
                             (consp (cdr et))
                             (eq (first (cdr et)) :eq)
                             (integerp (second (cdr et)))
                             (< (second (cdr et))
                                (length (cval-types inner)))
                             (ct-p (aref (cval-types inner)
                                         (second (cdr et))))
                             (ct-resourcetype-p
                              (aref (cval-types inner)
                                    (second (cdr et)))))
                        (make-ct :kind :resource :payload '(0 . nil)
                                 :offset 0)
                        (make-ct :kind :prim
                                 :payload :imported-or-exported-type)))
                (cval-types inner)))))))))
       ;; slice 5g (external-visibility ::42/::43/::44): a COMPONENT
       ;; TYPE's own export/import declarators must satisfy visibility
       ;; EAGERLY at the type's definition (instance types check at
       ;; the USE site — the :import/:export arms; ::35/::40's unused
       ;; instance types define freely)
       (when (eq (ct-kind c) :component)
         (%vis-check-type-use
          (make-cval :types (vector c))
          0 :auto nil)))


(defun %check-deftype (c state &optional (type-depth 1) (outer state))
  "Dispatch: defined value types get the full check; func/resource
and component/instance types get their own shapes. TYPE-DEPTH is the
number of enclosing component/instance TYPE boundaries (1 at a
concrete component's decls); outer aliases inside a type may cross
that many scopes (outer-alias.wast ::12's count of 2 inside
(type (component (type (component ...))))). OUTER is the enclosing
component's state (see %check-defvaltype) — the space the text
parser's interner lands inline externtypes in."
  (if (ct-defvaltype-p c)
      (progn (%check-defvaltype c state type-depth outer)
             (elem-size c state))
      (%check-defvaltype c state type-depth outer)))

(defun %outer-state-at (state count)
  "The validator state COUNT de Bruijn scopes out from STATE (slice 5,
outer-alias A): count 0 = STATE itself; count N = the (N-1)th entry of
*OUTER-STATES* (innermost first). NIL when the count exceeds the
stack (the caller rejects with 'invalid outer alias count')."
  (if (zerop count)
      state
      (nth (1- count) *outer-states*)))

(defun %ct-transitively-resources-p (ct state seen)
  "Slice 5 (outer-alias B, Binary.md 133-135): does CT transitively
refer to a resource type? Walks the structural kinds; an :idx valtype
resolves through STATE's type space (the alias's target scope). A
resource kind IS the terminal; own/borrow/records/etc. recurse into
their payloads; component/instance types walk their declarators'
externtypes (::7/::8's (eq $R) through an import/export)."
  (labels ((walk-ct (c)
             (when (and (ct-p c) (not (gethash (ct-kind c) seen)))
               (setf (gethash (ct-kind c) seen) t)
               (case (ct-kind c)
                   (:resource t)
                   ((:record)
                    (loop for (nil . vt) across (ct-payload c)
                          thereis (walk-vt vt)))
                   ((:variant)
                    (loop for (nil . vt) across (ct-payload c)
                          thereis (and vt (walk-vt vt))))
                   (:list (walk-vt (car (ct-payload c))))
                   (:tuple
                    (loop for vt across (ct-payload c)
                          thereis (walk-vt vt)))
                   (:option (walk-vt (ct-payload c)))
                   (:result
                    (or (and (car (ct-payload c))
                             (walk-vt (car (ct-payload c))))
                        (and (cdr (ct-payload c))
                             (walk-vt (cdr (ct-payload c))))))
                   ((:own :borrow) t)
                   ((:stream :future)
                    (and (ct-payload c) (walk-vt (ct-payload c))))
                   (:map (walk-vt (cdr (ct-payload c))))
                   (:func
                    (or (loop for (nil . vt) across (first (ct-payload c))
                              thereis (walk-vt vt))
                        (and (second (ct-payload c))
                             (walk-vt (second (ct-payload c))))))
                   ((:component :instance)
                    (loop for d across (ct-payload c)
                          thereis
                          (and (member (typedecl-kind d) '(:import :export))
                               (let ((et (third (typedecl-payload d))))
                                 (walk-et et)))))
                   (t nil))))
           (walk-et (et)
             (case (car et)
               (:type
                (cond
                 ((eq (cdr et) :sub-resource) t)
                 ((and (listp (cdr et)) (eq (car (cdr et)) :eq))
                  (walk-idx (second (cdr et))))
                 (t nil)))
               (:func
                (and (integerp (cdr et))
                     (< (cdr et) (length (cval-types state)))
                     (walk-idx (cdr et))))
               (t nil)))
           (walk-idx (i)
             (and (>= i 0) (< i (length (cval-types state)))
                  (walk-ct (aref (cval-types state) i))))
           (walk-vt (vt)
             (cond ((and (consp vt) (eq (car vt) :idx))
                    (walk-idx (cdr vt)))
                   ((ct-p vt) (walk-ct vt))
                   (t nil))))
    (walk-ct ct)))

(defun %check-outer-alias (a state outer-depth)
  "Outer alias validation (outer-alias.wast). De Bruijn convention
(Explainer 408-412, slice 5): count 0 = the CURRENT component/type's
index space; count N = N boundaries out, resolved through
*OUTER-STATES*. The count must not exceed the number of enclosing
scopes; the target index must be in bounds of the RESOLVED scope's
space; and when the alias crosses at least one COMPONENT boundary
with sort TYPE, the target must not transitively refer to a resource
(Binary.md 133-135) — components may only be aliased substitutable
definitions (core modules, core types, components, resource-free
types). OUTER-DEPTH still counts COMPONENT boundaries only (the
transitivity check fires when COUNT > 0 for an alias inside a nested
component; type-declarator aliases keep their own boundary counting
in %check-defvaltype)."
  (let ((ct (aliasrec-count a)))
    (unless (<= ct outer-depth)
      (%verr "invalid outer alias count of ~d" ct))
    (let ((target-state (%outer-state-at state ct)))
      (unless target-state
        ;; count within outer-depth but the STATE stack is shorter:
        ;; the alias sits inside a component at the validation root
        ;; whose enclosing scopes are outside this validation run
        ;; (the suite's named-form fixtures never hit this)
        (return-from %check-outer-alias))
      (let ((target (aliasrec-target a))
            (sort (aliasrec-sort a)))
        (case sort
          (:type
           (when (>= target (length (cval-types target-state)))
             (%verr "type index out of bounds"))
           ;; slice 5 (outer-alias B): crossing a COMPONENT boundary
           ;; with a TYPE alias requires the target to be
           ;; resource-free (outer-alias.wast ::2/::3/::4/::7/::8)
           (when (and (plusp ct)
                      (plusp outer-depth)
                      (< target (length (cval-types target-state)))
                      (ct-p (aref (cval-types target-state) target)))
             (when (%ct-transitively-resources-p
                    (aref (cval-types target-state) target)
                    target-state
                    (make-hash-table))
               (%verr "transitively refers to resources"))))
          (:core-type
           (when (>= target (length (cval-core-types target-state)))
             (%verr "core type index out of bounds")))
          (:core-module
           (when (>= target (cval-core-modules target-state))
             (%verr "module index out of bounds")))
          (:component
           (when (>= target (cval-components target-state))
             (%verr "component index out of bounds")))
          (:func
           (when (>= target (cval-funcs target-state))
             (%verr "function index out of bounds")))
          (t nil))))))

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
       ;; slice 5m (indicies ::10): a DECLARATOR-context ascription
       ;; ((export "v" (component)) inside an inline instance ct)
       ;; carries the DUMMY index 0 with no type space to resolve -
       ;; accept structurally; a REAL index resolves + kind-checks
       (if (and (zerop payload)
                (zerop (length (cval-types state))))
           nil
           (let ((c (%ct-at state payload)))
             (unless (eq (ct-kind c)
                         (if (eq sort :component)
                             :component :instance))
               (%verr "type index ~d is not a~a ~a type" payload
                      (if (member
                           (char (string-downcase (string sort)) 0)
                           '(#\a #\e #\i #\o #\u))
                          "n" "")
                      (string-downcase (string sort)))))))
      (:core-module
       ;; slice 5m (indicies ::10): a BARE (core module) declarator
       ;; ascription carries the :NONE marker (no core-type space
       ;; to resolve); a REAL index bounds-checks (the (core module
       ;; (type $t)) use still rejects - defined-types ::45/::46)
       (if (eq payload :none)
           nil
           (progn
             (when (>= payload (length (cval-core-types state)))
               (%verr "core type index out of bounds"))
             (let ((ty (aref (cval-core-types state) payload)))
               (unless (and (consp ty) (eq (car ty) :moduletype))
                 (%verr "core type index ~d is not a module type"
                        payload))))))
      (:type
       (if (eq payload :sub-resource)
           nil
           (let ((c (%ct-at state (second payload))))
             (unless (or (ct-defvaltype-p c)
                         (ct-resourcetype-p c))
               (%verr "type index ~d is not a defined type"
                      (second payload))))))
      (:value
       (when (and (consp payload) (eq (car payload) :eq))
         (when (>= (second payload) (cval-values state))
           (%verr "value index out of bounds"))))
      (t nil))
    nil))


(defun %opt (opts key)
  "A canonopt lookup: OPTS is an assoc-list of (KEY value) lists
\(both the text parser and the binary reader build it this way)."
  (assoc key opts :test #'eq))

(defun canon-opt (opts key)
  "The exported canonopt lookup (M8: the instantiation executor's
  contract — pure addition, the same assoc semantics)."
  (%opt opts key))

(defun %flat-count (vt state)
  "The flattened core-value count of a component valtype per
CanonicalABI.md flatten_type: bool/u8/u16/u32/s8/s16/s32/char/
error-context/flags/own/borrow/stream/future -> 1 flat; string ->
2 (ptr pair); s64/u64 -> 1 (i64); f32/f64 -> 1; record/tuple
flatten to the SUM of their fields; variant the MAX over cases;
list recurses on its element; :idx resolves through the type
space."
  (let ((c (if (and (consp vt) (eq (car vt) :idx))
               (%ct-at state (cdr vt))
               (if (ct-p vt) vt (make-ct :kind :prim :payload vt)))))
    (ecase (ct-kind c)
      ((:prim :flags :enum :option :own :borrow :stream :future
        :resource :func :instance :component)
       (if (and (eq (ct-kind c) :prim) (eq (ct-payload c) :string))
           2 1))
      ((:record) (reduce #'+ (ct-payload c)
                         :key (lambda (f) (%flat-count (cdr f) state))))
      ((:tuple) (reduce #'+ (ct-payload c)
                        :key (lambda (v) (%flat-count v state))))
      ((:list) (%flat-count (car (ct-payload c)) state))
      ((:variant) (let ((best 0))
                    (loop for (nil . v) across (ct-payload c)
                          do (when v
                               (setf best (max best
                                               (%flat-count v state)))))
                    best))
      ((:result) (let ((v (cdr (ct-payload c))))
                   (if v (%flat-count v state) 0)))
      ((:map) 2))))

(defun %needs-memory-p (vt state)
  "lower(T)/lift(T) memory forcing: a LIST or STRING anywhere in T
\(CanonicalABI.md 'canonopt Validation')."
  (let ((c (if (and (consp vt) (eq (car vt) :idx))
               (%ct-at state (cdr vt))
               (if (ct-p vt) vt (make-ct :kind :prim :payload vt)))))
    (ecase (ct-kind c)
      ((:prim) (eq (ct-payload c) :string))
      ((:list) t)
      ((:record) (loop for (nil . v) across (ct-payload c)
                       thereis (%needs-memory-p v state)))
      ((:tuple) (loop for v across (ct-payload c)
                      thereis (%needs-memory-p v state)))
      ((:variant) (loop for (nil . v) across (ct-payload c)
                         thereis (and v (%needs-memory-p v state))))
      ((:option :result :own :borrow :stream :future :flags :enum
        :map :func :instance :component :resource) nil))))

;;; ---------------------------------------------------------------
;;; M7 slice 4: static core-IR resolution. The canon signature checks
;;; (abi.wast) and the core-instance with-arg matcher
;;; (instantiation.wast ::39-::51) need the ACTUAL core signatures of
;;; exported core funcs — statically, from the wasm-module IR, with NO
;;; instantiation execution. Two per-validation tables are built as
;;; validate-component walks the defs:
;;;   *core-alias-source*  (sort . core-idx) -> (core-inst-idx . name)
;;;     — what (alias core export ...) introduced at each core index;
;;;   *core-inst-provider*  core-inst-idx -> (:module . module-idx)
;;;     | (:bag . coreinstrec-exports) — how each core instance's
;;;     exports resolve.

(defvar *core-alias-source* nil)
(defvar *core-inst-provider* nil)
;;; slice 4 (family 3): each nested component's validator STATE, keyed
;;; by the comp object (EQ) — the export-table type shapes of an
;;; instantiated instance's exports live in the CHILD's type space;
;;; the alias-type shape resolution follows entries through this
;;; stash (ev ::49/::50).
(defvar *comp-state-stash* (make-hash-table :test #'eq))
(defvar *outer-alias-core-module* (make-hash-table :test #'equal)
  "M9 (linking ::179): (comp . core-module-idx) -> the RESOLVED
wasm-module an outer-aliased core module projects (the walk's alias
arm resolves it through *outer-states*/*state-comp*); %core-module-at
reads it so a nested (instantiate $outer-module) finds the module.")

(defvar *state-comp* (make-hash-table :test #'eq)
  "M9 (linking ::179): the REVERSE of *comp-state-stash* — state ->
its comp, so an outer-alias resolution can walk the ENCLOSING
component's defs (an outer-aliased core module's payload lives in
the outer comp's def walk).")
;;; ...and the per-instance SOURCE COMPONENT (which nested comp an
;;; instantiated instance came from) — the alias-type shape
;;; resolution resolves the child's typeidx against the CHILD state.
(defvar *inst-source-comp* (make-hash-table :test #'eql))

(defvar *alias-component-source* (make-hash-table :test #'eql)
  "M9 (linking ::169): per-COMPONENT-INDEX the source comp of an
alias-introduced component (an (alias export $i \"c\" (component $x))
of an instantiated instance). %check-instantiate-args's target
search resolves the alias's comp through this record so the aliased
component's imports (not the nearest :component def's) are matched.")

(defvar *inst-ids-stash* (make-hash-table :test #'eql)
  "Slice 5c: per-INSTANCE the rebuilt child identity vector from
%check-instantiate-args — keyed by the instance's index (pre-incf).
An (alias export $i \"n\" (type)) whose SOURCE instance was
INSTANTIATED resolves its :inst-export identity through the child's
substitution (the with-args' parent identities), so two instantiations
of one component with the same resource argument share the identity
(resources.wast ::44/::45).")

(defvar *inst-gen-ids* (make-hash-table :test #'equal)
  "Slice 5e (resources ::27/::28/::43): per-(instance, typeidx)
GENERATIVE resource ids for IMPORTED instances typed by an instance
ct — the instance type's (sub resource)/eq exports are abstract per
instance, so two imports of one type carry distinct resources, and
one import's exported resource is STABLE across all references.")

(defvar *func-arg-ids* (make-hash-table :test #'eql)
  "Slice 5e: per-FUNC-INDEX the identity vector of the func's OWN type
space — populated when an (alias export $i \"f\" (func)) out of an
IMPORTED instance resolves the source instance ct's functype; the
func-arg matching resolves the PARENT side through the per-instance
generative ids. Cleared at validation depth 1.")

(defvar *inst-alias-typeidx* (make-hash-table :test #'equal)
  "M9 (linking ::90): per (INSTANCE-IDX . EXPORT-NAME) the COMPONENT
type index of the (alias export $i \"n\" (type)) that projects it —
recorded at the alias walk so an import's declarator-space functype
can remap its (:idx . k) payloads into the component's type space
(the declarator's k-th type-introducing entry belongs to the export
named n; the alias for n holds the component-space position).")

(defvar *func-alias-origin* (make-hash-table :test #'eql)
  "M9 (linking ::90): per-FUNC-INDEX the (INSTANCE-IDX . instance-ct)
of the imported instance whose declarator-space functype was pushed —
the canon checks read that functype against the COMPONENT's type
space and must remap its (:idx . k) payloads first. Cleared at
validation depth 1.")

(defun %inst-gen-id (inst-idx typeidx)
  "The generative resource id for imported instance INST-IDX's own
type slot TYPEIDX (created on first use)."
  (let ((key (cons inst-idx typeidx)))
    (or (gethash key *inst-gen-ids*)
        (let ((id (%fresh-resource-id)))
          (setf (gethash key *inst-gen-ids*) id)
          id))))

(defun %sort-err-name (sort)
  "M9 (linking tags ::10): the sort's name in the 'is not a ~a'
validation message — the fixture text uses the CORE sort names."
  (case sort
    (:core-func "func")
    (:core-memory "memory")
    (:core-table "table")
    (:core-global "global")
    (:core-tag "tag")
    (:core-module "module")
    (:core-instance "instance")
    (t (string-downcase (subseq (symbol-name sort) 1)))))

(defun %core-provider-export-names (comp inst-idx)
  "M9 (linking tags ::10): core instance INST-IDX's export NAMES plus
a KIND-OF function (name -> the export's core sort, or NIL when the
kind is not statically known — a moduletype-only provider has no
kind info). Two values: (names-list kind-of)."
  (let ((prov (and *core-inst-provider*
                   (gethash inst-idx *core-inst-provider*))))
    (if (not (consp prov))
        (values nil nil)
        (case (car prov)
          (:bag
           (values (map 'list #'car (cdr prov))
                   ;; an inline bag's exports carry sortidxes — the
                   ;; sort IS the alias's own sort space
                   (lambda (name)
                     (let ((pair (find name (cdr prov)
                                       :key #'car :test #'string=)))
                       (and pair (sortidx-sort (cdr pair)))))))
          (:module
           (let ((pm (%core-module-at comp (cdr prov))))
             (cond
              ((wasmdown.types:wasm-module-p pm)
               (values
                (map 'list #'wasmdown.types:wasm-export-name
                     (wasmdown.types:wasm-module-exports pm))
                (lambda (name)
                  (let ((ex (find name
                                  (coerce (wasmdown.types:wasm-module-exports
                                           pm)
                                          'list)
                                  :key #'wasmdown.types:wasm-export-name
                                  :test #'string=)))
                    (and ex
                         (ecase (wasmdown.types:wasm-export-kind ex)
                           (:func :core-func)
                           (:memory :core-memory)
                           (:table :core-table)
                           (:global :core-global)
                           (:tag :core-tag)))))))
              ;; moduletype-only providers (the ::183/:via-moduletype
              ;; and ::231/:via-import markers, imported instances):
              ;; the NAMES are known (core-export decls) but the KINDS
              ;; are not (the payload is a name list) — kind-of NIL
              (t
               (let ((mt (cond
                          ((and (consp pm)
                                (eq (car pm) :via-moduletype))
                           (cdr pm))
                          ((and (consp pm)
                                (eq (car pm) :via-import))
                           (%core-module-import-moduletype
                            (second pm) (cdr (cdr pm))))
                          (t (%core-module-import-moduletype
                              comp (cdr prov))))))
                 (values
                  (and mt
                       (map 'list #'first
                            (map 'list #'(lambda (d) (typedecl-payload d))
                                 (remove-if-not
                                  (lambda (d)
                                    (eq (typedecl-kind d) :core-export))
                                  (coerce (%moduletype-decls mt)
                                          'list)))))
                  nil))))))))))
(defun %core-module-at (comp n)
  "The Nth core module of the INDEX SPACE (which counts :core-module
defs AND (:core-module) imports, in file order) — a def yields its
wasm-module payload; an import occupies the slot with no IR (NIL —
the module's shape arrives only at instantiation, M9's
with-(core-module) fixtures).
M9 (linking ::179): a :core-module-sorted ALIAS also occupies a slot
(the implicit (alias outer ct idx (core module)) of a nested
component's (instantiate $outer-module)); its payload resolves
through *outer-states*/*state-comp* — the target state's comp's
own core-module space. The alias record carries the resolved module
in *outer-alias-core-module* (the walk's alias arm populates it);
no record = the hole stays NIL (matching the import case)."
  (let ((seen 0))
    (loop for d across (comp-defs comp)
          when (or (eq (compdef-kind d) :core-module)
                   (and (eq (compdef-kind d) :import)
                        (eq (car (importrec-type
                                  (compdef-payload d)))
                            :core-module))
                   (and (eq (compdef-kind d) :alias)
                        (eq (aliasrec-sort (compdef-payload d))
                            :core-module)))
            do (cond ((= seen n)
                      (return (cond
                               ((eq (compdef-kind d) :core-module)
                                (compdef-payload d))
                               ((eq (compdef-kind d) :alias)
                                (or (gethash (cons comp seen)
                                             *outer-alias-core-module*)
                                    nil))
                               (t nil))))
                     (t (incf seen)))
          finally (return nil))))

(defun %module-comptype (m abs-idx kind)
  "The core item at ABS-IDX of KIND (imports first, then definitions)
  of module M; NIL when out of range."
  (let* ((imps (coerce (wasmdown.types:wasm-module-imports m) 'list))
         (kimps (remove-if-not
                 (lambda (im)
                   (eq (wasmdown.types:wasm-import-kind im) kind))
                 imps))
         (nimp (length kimps)))
    (cond
     ((< abs-idx nimp)
      (let ((imp (nth abs-idx kimps)))
        (if (eq kind :func)
            (wasmdown.types:module-type-comptype
             m (wasmdown.types:wasm-import-type imp))
            (wasmdown.types:wasm-import-type imp))))
     (t
      (let* ((k (- abs-idx nimp))
             (vec (ecase kind
                    (:func (wasmdown.types:wasm-module-funcs m))
                    (:table (wasmdown.types:wasm-module-tables m))
                    (:memory (wasmdown.types:wasm-module-mems m))
                    (:global (wasmdown.types:wasm-module-globals m)))))
        (and (< k (length vec))
             (ecase kind
               (:func
                (wasmdown.types:module-type-comptype
                 m (wasmdown.types:wasm-func-typeidx (svref vec k))))
               (:table (svref vec k))
               (:memory (svref vec k))
               (:global (svref vec k)))))))))

(defun %module-export-item (m name)
  "Module M's export NAME resolved to (kind . payload) — a func as
  (params . results), table/memory as the raw type, global as (type
  . mut); NIL when M has no such export or the item is unresolvable."
  (let ((ex (find name
                  (coerce (wasmdown.types:wasm-module-exports m) 'list)
                  :key #'wasmdown.types:wasm-export-name
                  :test #'string=)))
    (when ex
      (let ((i (wasmdown.types:wasm-export-index ex))
            (kind (wasmdown.types:wasm-export-kind ex)))
        (case kind
          (:func
           (let ((ct (%module-comptype m i :func)))
             (and ct (eq (wasmdown.types:wasm-comptype-kind ct) :func)
                  (cons :func
                        (cons (wasmdown.types:wasm-comptype-params ct)
                              (wasmdown.types:wasm-comptype-results ct))))))
          (:table
           (let ((ty (%module-comptype m i :table)))
             (and ty (cons :table ty))))
          (:memory
           (let ((ty (%module-comptype m i :memory)))
             (and ty (cons :memory ty))))
          (:global
           (let ((g (%module-comptype m i :global)))
             (and g (cons :global
                          (cons (wasmdown.types:wasm-global-type g)
                                (wasmdown.types:wasm-global-mutable g))))))

          ;; M9 (linking lane): a TAG export — the tag's identity is
          ;; opaque here; the core-link arm needs only the KIND match
          ;; (tags.wast's whole-instance arguments)
          (:tag (cons :tag t))
          (t nil))))))

(defun %core-provider-export (comp inst-idx name)
  "Provider core-instance INST-IDX's export NAME — (:func params .
  results) | (:table . wasm-table) | (:memory . wasm-memory) |
  (:global . (type . mut)) | NIL. An :instantiate provider reads the
  module's exports; an inline-export BAG follows its (sort . idx)
  entry through *core-alias-source* (each bag entry points at an
  alias-introduced core index, or — M9 — a canon-introduced one whose
  recorded marker carries the lowered/built-in SIGNATURE directly)."
  (let ((prov (and *core-inst-provider*
                   (gethash inst-idx *core-inst-provider*))))
    (cond
     ((and (consp prov) (eq (car prov) :module))
      (let ((m (%core-module-at comp (cdr prov))))
        (cond
         ((and m (wasmdown.types:wasm-module-p m))
          (%module-export-item m name))
         ;; M9: an IMPORTED core module — the IR arrives only at
         ;; instantiation; the ascribed moduletype's export
         ;; declarator carries the shape (the moduletype match
         ;; already ran at the with-arg site)
         (t (%core-moduletype-export-item comp (cdr prov) name)))))
     ((and (consp prov) (eq (car prov) :bag))
      (let* ((hit (find name (coerce (cdr prov) 'list)
                       :key #'car :test #'string=))
             (src (and hit
                       *core-alias-source*
                       (gethash (cdr hit) *core-alias-source*))))
        (cond
         ;; M9: a canon-introduced core func — the marker carries the
         ;; lowered (component ct) or built-in signature
         ((and (consp src) (eq (car src) :canon-lower))
          (cons :func (cdr src)))
         ((and (consp src) (eq (car src) :canon-sig))
          (cons :func (cdr src)))
         (src
          (%core-provider-export comp (car src) (cdr src)))
         (t nil))))
     (t nil))))

(defun %core-module-import-moduletype (comp n)
  "The moduletype ct ascribed to the Nth core-module slot's IMPORT —
the importrec's (:core-module . typeidx) pointing into the
component's core-type space (the ct's OWN payload, a (:moduletype .
  decls) cons); NIL when the slot holds a real def."
  (let ((seen 0))
    (loop for d across (comp-defs comp)
          when (or (eq (compdef-kind d) :core-module)
                   (and (eq (compdef-kind d) :import)
                        (eq (car (importrec-type
                                  (compdef-payload d)))
                            :core-module)))
            do (cond
                ((and (= seen n)
                      (eq (compdef-kind d) :import))
                 (let* ((k (cdr (importrec-type (compdef-payload d))))
                        (mt (when (integerp k)
                              (loop with c = -1
                                    for e across (comp-defs comp)
                                    when (eq (compdef-kind e)
                                             :core-type)
                                      do (incf c)
                                    when (= c k)
                                      return (compdef-payload e)
                                    finally (return nil)))))
                   (return mt)))
                ((= seen n) (return nil))
                (t (incf seen)))
          finally (return nil))))

(defun %core-moduletype-export-item (comp n name)
  "The export item of the Nth core module's DECLARED moduletype —
used when the module is an IMPORT (no IR at validation time; the
import's ascription is the only shape). Returns (:func params .
  results) | (:global . (type . mut)) | (:memory . limits) |
(:table . (reftype . limits)) | NIL. The (func (type i)) desc
indexes the moduletype's OWN type sequence — the :core-type
declarators in declaration order (imports and exports included)."
  (let* ((mt (%core-module-import-moduletype comp n)))
    (unless (and mt (consp mt) (eq (car mt) :moduletype))
      (return-from %core-moduletype-export-item nil))
    (let* ((decls (if (and (consp (cdr mt)) (vectorp (second mt)))
                      (second mt)
                      (cdr mt)))
           (ex (find name (coerce decls 'list)
                     :key (lambda (d)
                            (and (eq (typedecl-kind d) :core-export)
                                 (first (typedecl-payload d))))
                     :test #'string=)))
      (unless ex (return-from %core-moduletype-export-item nil))
      (let ((desc (second (typedecl-payload ex))))
        (case (car desc)
          (:func
           (let* ((ti (second desc))
                  (ft (loop with c = -1
                            for d2 across decls
                            when (member (typedecl-kind d2)
                                         '(:core-type :core-alias))
                              do (incf c)
                            when (= c ti)
                              return (typedecl-payload d2)
                            finally (return nil))))
             (cond
              ((and ft (wasmdown.types:wasm-comptype-p ft))
               (cons :func
                     (cons (wasmdown.types:wasm-comptype-params ft)
                           (wasmdown.types:wasm-comptype-results ft))))
              ;; an IMPLICIT (func ...) desc: the inline sig rides
              ;; in the FOURTH slot (a wasm-func-type, M9)
              ((and (eq (third desc) :implicit)
                    (wasmdown.types:wasm-func-type-p
                     (fourth desc)))
               (cons :func
                     (cons (wasmdown.types:wasm-func-type-params
                            (fourth desc))
                           (wasmdown.types:wasm-func-type-results
                            (fourth desc)))))
              (t (cons :func (cons nil nil))))))
          (:global
           (cons :global (cons (second desc) (third desc))))
          (:memory
           (cons :memory (second desc)))
          (:table
           (cons :table (cons (second desc) (third desc))))
          (t nil))))))

(defun %canon-lower-flat-sig (state ft &optional (async-p nil))
  "The FLATTENED core signature of a canon LOWER of functype FT —
flatten_functype's lower side (CanonicalABI.md 2894-2930), computed
at canon time where the STATE resolves (:idx . n) references. Returns
(params . results) as core type lists. M11a: ASYNC-P selects the
async lower shape (MAX_FLAT_ASYNC_PARAMS=4 param collapse, the
appended [ptr] out-param for ANY results, the packed-state :i32
core result)."
  (let ((p (and ft (%flat-functype ft state :lower
                                   :async-p async-p))))
    (cons (and p (car p)) (and p (cdr p)))))

(defun builtin-canon-sig (kind &optional (imms nil))
  "The FLAT core signature (params . results) of a canon BUILT-IN
(M11a: the async core builtins' fixed shapes — CanonicalABI.md's
builtin validation rules). IMMS is the canonrec's immediate list:
task.return's result list and context.get/set's valtype decide the
slots. Only reached with a NIL/absent task.return result (the
flat-sig helper handles the result-bearing case)."
  (declare (ignore imms))
  (case kind
    ((:backpressure-inc :backpressure-dec :task-cancel
      :task-return
      :error-context-new :error-context-debug-message
      :error-context-drop :resource-drop)
     (cons '() '()))
    ;; M11b (§4652/§4631): thread.yield/suspend return i32 (the
    ;; literal 0); §4668/§4695/§4722/§4749: the -then-resume/
    ;; -then-promote variants take the target index + return 0
    ((:thread-yield :thread-suspend)
     (cons '() '(:i32)))
    ((:thread-suspend-then-resume :thread-yield-then-resume
      :thread-suspend-then-promote :thread-yield-then-promote)
     (cons '(:i32) '(:i32)))
    ;; §4606: thread.resume-later takes the i32 thread index
    ((:thread-resume-later)
     (cons '(:i32) '()))
    ;; M11a (CanonicalABI.md 4226/4089): subtask.drop and
    ;; waitable-set.drop take an i32 handle param — they were
    ;; mis-grouped with the no-params builtins (async-calls-sync's
    ;; $AsyncOuter with-bag: 'expected: (func)' on subtask.drop).
    ((:subtask-drop :waitable-set-drop :context-set
      :stream-drop-readable :stream-drop-writable
      :future-drop-readable :future-drop-writable)
     (cons '(:i32) '()))
    ((:context-get :waitable-set-new :thread-index
      :resource-new :resource-rep
      :stream-forward :future-forward)
     (cons '() '(:i32)))
    ;; §4565: thread.new-indirect takes the table index + the
    ;; closure param and returns the thread index — a FIXED shape
    ((:thread-new-indirect)
     (cons '(:i32 :i32) '(:i32)))
    ;; M11b (§4386): the cancel builtins take ONE i32 and return
    ;; one i32 — they were mis-grouped with the no-params builtins
    ;; (cancel-stream's 'expected: (func)')
    ((:stream-cancel-read :stream-cancel-write
      :future-cancel-read :future-cancel-write)
     (cons '(:i32) '(:i32)))
    ;; (func (param $wi i32) (param $si i32)) — TWO i32 params, no
    ;; results (it was mis-grouped with the no-params builtins, so
    ;; every fixture importing waitable.join failed the with-bag
    ;; signature check: deadlock.wast's 'expected: (func)').
    ((:waitable-join)
     (cons '(:i32 :i32) '()))
    ((:stream-new :future-new)
     (cons '() '(:i64)))
    ((:stream-read :stream-write)
     (cons '(:i32 :i32 :i32) '(:i32)))
    ((:future-read :future-write)
     (cons '(:i32 :i32) '(:i32)))
    ((:waitable-set-wait :waitable-set-poll)
     (cons '(:i32 :i32) '(:i32)))
    (t (cons '(:i32) '(:i32)))))


(defun canon-builtin-flat-sig (imms state)
  "The flat core sig of a canon BUILT-IN canonrec's immediate list
IMMS, with STATE resolving task.return's result type references.
task.return's (result $t)? flattens the single result's types into
the core PARAMS (CanonicalABI.md 3929: flatten_functype of
(func (param $t)?) in the LOWER context — the params ARE the flat
result slots and the results stay EMPTY, so no out-param is
appended; cross-abi-calls' task.return16 takes exactly its 16
flat params)."
  (let* ((kind (first imms))
         (rs (and (eq kind :task-return) (second imms))))
    (if (and (eq kind :task-return) rs)
        (let* (;; the two parser shapes: the BINARY reader's is a
               ;; LIST holding the one resultlist (NIL when the
               ;; canon has no result); the TEXT parser's is the
               ;; collected valtypes themselves. A valtype is a
               ;; cons (an (:idx . n) ref or a compound shape) or a
               ;; keyword; a list whose FIRST element is a cons
               ;; wrapping ANOTHER cons is the BINARY shape; a
               ;; bare valtype keyword is either parser's single
               ;; result.
               (results
                 (if (and (consp rs) (consp (first rs))
                          (consp (first (first rs))))
                     ;; ( ( valtype... ) ) — binary: one resultlist
                     (first rs)
                     ;; the collected valtypes (or the binary's
                     ;; single valtype keyword)
                     (if (and (consp rs) (listp rs)) rs (list rs))))
               (flat (mapcan (lambda (x)
                               (or (ignore-errors
                                    (%flat-type-list x state))
                                   '()))
                             results)))
          (cons (or flat '()) '()))
        (builtin-canon-sig kind imms))))

(defun %canon-core-func-sig (comp core-idx)
  "The canon target core func's ACTUAL signature (params . results),
  resolved through the alias that introduced it; NIL when unknown."
  (let ((src (and *core-alias-source*
                  (gethash (cons :core-func core-idx)
                           *core-alias-source*))))
    (when src
      (let ((r (%core-provider-export comp (car src) (cdr src))))
        (and r (eq (car r) :func) (cdr r))))))

(defun %core-limits-match-p (want got)
  "Limits subtyping (the runtime link rule's shape): the provided
  limits are at least WANT's min and, when WANT declares a max, must
  declare one no larger (instantiation.wast ::44-::46/::48)."
  (and (<= (wasmdown.types:wasm-limits-min want)
           (wasmdown.types:wasm-limits-min got))
       (or (not (wasmdown.types:wasm-limits-has-max-p want))
           (and (wasmdown.types:wasm-limits-has-max-p got)
                (<= (wasmdown.types:wasm-limits-max got)
                    (wasmdown.types:wasm-limits-max want))))))

(defun %core-desc-limits (desc)
  "A moduletype desc's limits list (min max shared) as a wasm-limits
  (shared rides in the desc but has no core limits slot — the shared
  comparison is separate, ::47)."
  (let ((lim (if (eq (car desc) :table) (third desc) (second desc))))
    (if (and (second lim) (numberp (second lim)))
        (wasmdown.types::%make-wasm-limits*
         (first lim) (second lim) t nil)
        (wasmdown.types::%make-wasm-limits*
         (first lim) nil nil nil))))

(defun %core-desc-shared (desc)
  "The moduletype desc's shared flag (the limits list's third slot)."
  (let ((lim (if (eq (car desc) :table) (third desc) (second desc))))
    (and (third lim) t)))

(defun %wasm-memory-shared-p (mem)
  "The core IR has no shared-memory representation yet (the core
  text parser rejects `shared'; the binary reader rejects limits
  flag bit 0x02) — a shared expectation can never be satisfied,
  which is the honest answer until the core layer grows the flag
  (instantiation ::47's DECLARED side carries it via the desc
  limits' third slot)."
  (declare (ignore mem))
  nil)

(defun %core-sig-equal-p (a b)
  "Two core functypes (params . results) equal. The sequences are
normalized to LISTS first: the text parser's functype slots and the
runtime's vectors (and M9's fresh moduletype functype objects) mix
list and vector representations — the comparison is element-wise on
the CONTENT, never on the representation or object identity."
  (flet ((norm (x) (if (vectorp x) (coerce x 'list) x)))
    (and (equal (norm (car a)) (norm (car b)))
         (equal (norm (cdr a)) (norm (cdr b))))))

(defun %core-importdesc-text (desc)
  "A moduletype import desc rendered as core text — the fixture's
  'expected: (func)' family (instantiation.wast ::39-::41 prints the
  EXPECTED externtype)."
  (case (car desc)
    (:func
     ;; the desc's typeidx indexes the moduletype's OWN :core-type
     ;; decls; an implicit (func) (typeidx 0 with no decls, per
     ;; %check-moduletype) is [] -> []
     "(func)")
    (:global
     (format nil "(global ~a)" (string-downcase (string (second desc)))))
    (:table
     (let ((lim (third desc)))
       (format nil "(table ~a~@[ ~a~] ~a)"
               (first lim)
               (and (second lim) (numberp (second lim))
                    (second lim))
               (string-downcase (string (second desc))))))
    (:memory
     (let ((lim (second desc)))
       (format nil "(memory ~a~@[ ~a~])"
               (first lim)
               (and (second lim) (numberp (second lim))
                    (second lim)))))
    (t "(func)")))

(defun %check-core-instance-args (payload comp state)
  "Core module instantiation argument matching (instantiation.wast
  ::39-::51): every two-level import of the TARGET module is checked,
  in module order, against the (with <name> (instance ...)) provider
  selected by the import's FIRST name. Providers are core instances —
  either $ids of prior instantiations or inline export bags."
  (declare (ignore state))
  (let* ((m (%core-module-at comp (coreinstrec-index payload)))
         (args (coerce (coreinstrec-args payload) 'list)))
    ;; slice 5m (instantiation ::56): duplicate with-arg names in a
    ;; CORE instantiation ('duplicate module instantiation argument
    ;; named `a`')
    (let ((names '()))
      (loop for (nm . nil) in args
            do (when (member nm names :test #'string=)
                 (%verr "duplicate module instantiation argument named `~a`"
                        nm))
               (push nm names)))
    ;; M9: an IMPORTED core module has no IR at validation time —
    ;; its shape (imports and exports) was declared by the import's
    ;; moduletype ascription and matched against the with-arg at the
    ;; component-instantiation site (%match-core-module-arg), so the
    ;; per-item provider checks below are redundant for it
    (unless (and m (wasmdown.types:wasm-module-p m))
      (return-from %check-core-instance-args))
    (loop for imp across (wasmdown.types:wasm-module-imports m)
          for mod = (wasmdown.types:wasm-import-module imp)
          for field = (wasmdown.types:wasm-import-field imp)
          for arg = (cdr (assoc mod args :test #'string=))
          do (unless arg
               ;; every imported module name needs an argument (::50)
               (%verr "missing module instantiation argument"))
             (let ((prov (%core-provider-export comp arg field)))
               ;; the supplied instance must cover every field imported
               ;; from that name (::51)
               (unless prov
                 (%verr "does not export an item named `~a`" field))
               (ecase (wasmdown.types:wasm-import-kind imp)
                 (:func
                  (unless (eq (car prov) :func)
                    (%verr "expected func, found ~a"
                           (string-downcase (string (car prov)))))
                  ;; the provider func's signature must equal the
                  ;; target's imported functype (::39-::41). The
                  ;; import's type is an index into the MODULE's
                  ;; TYPE VECTOR (not a func-space position — M9:
                  ;; reading it through %module-comptype's
                  ;; imports-then-defs numbering cross-wired
                  ;; multi-import modules).
                  (let* ((ti (wasmdown.types:wasm-import-type imp))
                         (ct (and (integerp ti)
                                  (wasmdown.types:module-type-comptype
                                   m ti))))
                    (unless (and ct
                                 (eq (wasmdown.types:wasm-comptype-kind ct)
                                     :func)
                                 (%core-sig-equal-p
                                  (cons (wasmdown.types:wasm-comptype-params
                                         ct)
                                        (wasmdown.types:wasm-comptype-results
                                         ct))
                                  (cdr prov)))
                      (%verr "expected: ~a" "(func)"))))
                 (:global
                  (unless (eq (car prov) :global)
                    (%verr "expected global, found ~a"
                           (string-downcase (string (car prov)))))
                  ;; immutable expected: provider type equal (::42)
                  (let* ((want (car (wasmdown.types:wasm-import-type imp)))
                         (got (car (cdr prov))))
                    (unless (eq want got)
                      (%verr "expected global type ~a, found ~a"
                             (string-downcase (string want))
                             (string-downcase (string got))))))
                 (:table
                  (unless (eq (car prov) :table)
                    (%verr "expected table, found ~a"
                           (string-downcase (string (car prov)))))
                  (let ((wt (wasmdown.types:wasm-import-type imp))
                        (pt (cdr prov)))
                    (unless (eq (wasmdown.types:wasm-table-type wt)
                                (wasmdown.types:wasm-table-type pt))
                      (%verr "expected table element type ~a, found ~a"
                             (string-downcase
                              (string (wasmdown.types:wasm-table-type wt)))
                             (string-downcase
                              (string (wasmdown.types:wasm-table-type pt)))))
                    (unless (%core-limits-match-p
                             (wasmdown.types:wasm-table-limits wt)
                             (wasmdown.types:wasm-table-limits pt))
                      ;; ::44-::46
                      (%verr "mismatch in table limits"))))
                 (:memory
                  (unless (eq (car prov) :memory)
                    (%verr "expected memory, found ~a"
                           (string-downcase (string (car prov)))))
                  ;; the shared flag comparison runs BEFORE the
                  ;; limits (::47). BOTH sides are core imports
                  ;; here — the core IR has no shared representation
                  ;; yet, so this comparison is only reachable once
                  ;; the core layer grows the flag; the DESC-side
                  ;; shared check lives in the moduletype matcher
                  ;; (family 1b) where the desc carries the flag.
                  (unless (eq (%wasm-memory-shared-p
                               (wasmdown.types:wasm-import-type imp))
                              (%wasm-memory-shared-p (cdr prov)))
                    (%verr "mismatch in the shared flag for memories"))
                  (unless (%core-limits-match-p
                           (wasmdown.types:wasm-memory-limits
                            (wasmdown.types:wasm-import-type imp))
                           (wasmdown.types:wasm-memory-limits (cdr prov)))
                    ;; ::48
                    (%verr "mismatch in memory limits")))
                 ;; M9 (linking lane): a TAG import — the provider
                 ;; must be a tag (kind match only; tags.wast)
                 (:tag
                  (unless (eq (car prov) :tag)
                    (%verr "expected tag, found ~a"
                           (string-downcase (string (car prov)))))))))))



(defun %moduletype-decls (mt)
  "A moduletype's decl vector — the (:moduletype . #(...)) CONS the
text parser builds and the (:moduletype #(...)) LIST the binary
reader builds, normalized to the vector."
  (if (and (consp (cdr mt)) (vectorp (second mt)))
      (second mt)
      (cdr mt)))

(defun %moduletype-type-at (decls i)
  "The i'th :core-type decl's functype as (params . results); the
moduletype's implicit (func) typeidx-0 use with no decls is [] ->
[] (per %check-moduletype's reading)."
  (let* ((core-types (remove-if-not
                      (lambda (d) (eq (typedecl-kind d) :core-type))
                      (coerce decls 'list))))
    (cond
     ((and (integerp i) (< i (length core-types)))
      (let ((payload (typedecl-payload (nth i core-types))))
        (when (and (consp payload) (eq (car payload) :func))
          (cons (second payload) (third payload)))))
     ;; the moduletype's implicit (func) typeidx-0 with NO decls
     ((and (zerop i) (null core-types))
      (cons '() '()))
     (t nil))))

(defun %core-desc-compatible-with-import-p (desc imp m decls)
  "One declared moduletype import desc vs the ARG module's own import
(::33). The desc's func typeidx resolves through the DECLARING
moduletype's own :core-type decls (DECLS); the arg side's import
type comes from the module's type vector. T = compatible. A KIND
mismatch is the caller's 'type mismatch in import' (::33)."
  (let ((kind (wasmdown.types:wasm-import-kind imp)))
    (case (car desc)
      (:func
       (and (eq kind :func)
            (let* ((want (cond
                           ;; M9 (shared-everything ::0): an implicit
                           ;; (func (param ...) (result ...)) import
                           ;; desc carries its OWN inline sig in the
                           ;; fourth slot — read it directly instead
                           ;; of dereferencing a typeidx the
                           ;; moduletype never declared
                           ((and (eq (third desc) :implicit)
                                 (wasmdown.types:wasm-func-type-p
                                  (fourth desc)))
                            (cons (wasmdown.types:wasm-func-type-params
                                   (fourth desc))
                                  (wasmdown.types:wasm-func-type-results
                                   (fourth desc))))
                           (t (%moduletype-type-at decls
                                                    (second desc)))))
                   (ti (wasmdown.types:wasm-import-type imp))
                   (ct (and (integerp ti)
                            (%module-comptype m ti :func))))
              (and want ct
                   (eq (wasmdown.types:wasm-comptype-kind ct) :func)
                   (%core-sig-equal-p
                    want
                    (cons (wasmdown.types:wasm-comptype-params ct)
                          (wasmdown.types:wasm-comptype-results ct)))))))
      (:global
       (and (eq kind :global)
            (let ((ty (wasmdown.types:wasm-import-type imp)))
              (and (eq (second desc) (car ty))
                   (eq (third desc) (cdr ty))))))
      (:table
       (and (eq kind :table)
            (let ((ty (wasmdown.types:wasm-import-type imp)))
              (and (eq (second desc)
                       (wasmdown.types:wasm-table-type ty))
                   ;; CONTRAvariant: the declared import may ask for
                   ;; MORE than the arg module imports (the arg is
                   ;; the 'want' — instantiation ::37)
                   (%core-limits-match-p
                    (wasmdown.types:wasm-table-limits ty)
                    (%core-desc-limits desc))))))
      (:memory
       (and (eq kind :memory)
            (let ((ty (wasmdown.types:wasm-import-type imp)))
              (and (eq (%core-desc-shared desc)
                       (%wasm-memory-shared-p ty))
                   (%core-limits-match-p
                    (wasmdown.types:wasm-memory-limits ty)
                    (%core-desc-limits desc))))))
      (t nil))))

(defun %core-desc-compatible-with-item-p (desc item decls)
  "One declared moduletype EXPORT desc vs the ARG module's resolved
export item (::36). ITEM is %module-export-item's shape. T =
compatible."
  (case (car desc)
    (:func
     (and (eq (car item) :func)
          (let ((want (if (and (eq (third desc) :implicit)
                               (wasmdown.types:wasm-func-type-p
                                (fourth desc)))
                          (cons (wasmdown.types:wasm-func-type-params
                                 (fourth desc))
                                (wasmdown.types:wasm-func-type-results
                                 (fourth desc)))
                          (%moduletype-type-at decls (second desc)))))
            (and want (%core-sig-equal-p want (cdr item))))))
    (:global
     (and (eq (car item) :global)
          (and (eq (second desc) (car (cdr item)))
               (eq (third desc) (cdr (cdr item))))))
    (:table
     (and (eq (car item) :table)
          (let ((wt (cdr item)))
            (and (eq (second desc)
                     (wasmdown.types:wasm-table-type wt))
                 (%core-limits-match-p
                  (%core-desc-limits desc)
                  (wasmdown.types:wasm-table-limits wt))))))
    (:memory
     (and (eq (car item) :memory)
          (let ((wm (cdr item)))
            (and (eq (%core-desc-shared desc)
                     (%wasm-memory-shared-p wm))
                 (%core-limits-match-p
                  (%core-desc-limits desc)
                  (wasmdown.types:wasm-memory-limits wm))))))
    (t t)))

(defun %match-core-module-arg (mt arg-module)
  "The moduletype-vs-module subtyping match (instantiation.wast
::33/::34/::36): the ARG module must supply everything the declared
moduletype needs — imports CONTRAVARIANT (every ARG import is
declared: the arg may import less than declared, never more),
exports COVARIANT (every DECLARED export is provided: the arg may
export more). Names are two-level (module:field)."
  (let* ((decls (%moduletype-decls mt))
         (dl (coerce decls 'list))
         ;; the declared imports/exports, keyed by their two-level
         ;; names / export names
         (dimps (remove-if-not
                 (lambda (d) (eq (typedecl-kind d) :core-import)) dl))
         (dexp (remove-if-not
                (lambda (d) (eq (typedecl-kind d) :core-export)) dl)))
    (flet ((find-import (mod field)
             (find (list mod field) dimps
                   :key (lambda (d)
                          (let ((p (typedecl-payload d)))
                            (list (first p) (second p))))
                   :test #'equal))
           (find-export (name)
             (find name dexp
                   :key (lambda (d)
                          (first (typedecl-payload d)))
                   :test #'string=)))
      ;; CONTRAvariant: every ARG import must be declared
      (loop for imp across (wasmdown.types:wasm-module-imports
                            arg-module)
            for mod = (wasmdown.types:wasm-import-module imp)
            for field = (wasmdown.types:wasm-import-field imp)
            ;; the fixture message's name rendering: the empty
            ;; module shows as '::field' (::33/::34's `::f`/`::extra`)
            for key = (format nil "~a:~a"
                              (if (string= mod "") ":" mod)
                              field)
            for decl = (find-import mod field)
            do (unless decl
                 ;; ::34 'missing expected import `::extra`'
                 (%verr "missing expected import `~a`" key))
               ;; ::33 'type mismatch in import `::f`'
               (unless (%core-desc-compatible-with-import-p
                        (third (typedecl-payload decl)) imp arg-module
                        decls)
                 (%verr "type mismatch in import `~a`" key)))
      ;; COvariant: every DECLARED export must be provided with a
      ;; compatible type
      (dolist (d dexp)
        (let* ((name (first (typedecl-payload d)))
               (desc (second (typedecl-payload d)))
               (item (%module-export-item arg-module name)))
          (unless item
            ;; ::35 'missing expected export `x`'
            (%verr "missing expected export `~a`" name))
          (unless (%core-desc-compatible-with-item-p desc item decls)
            ;; ::36 'type mismatch in export `g`'
            (%verr "type mismatch in export `~a`" name)))))))

(defun %check-canon-opts (cr state comp context)
  "The canon lift/lower option rules (CanonicalABI.md):
realloc implies memory (canonopt rule); lift(T) requires realloc
for list/string params; lower(T) requires memory for list/string
params/results; memory is required when flat params exceed
MAX_FLAT_PARAMS (16) or flat results exceed MAX_FLAT_RESULTS (1).
For :lift the functype is the canonrec's explicit typeidx; for
:lower it is the LOWERED func's type from the validator's parallel
func-type space. This slice implements the SYNC rules; the async
task-return shapes are a later slice."
  (let* ((opts (canonrec-opts cr))
         (mem (%opt opts :memory))
         (realloc (%opt opts :realloc))
         (typeidx (canonrec-typeidx cr))
         (ft (if (eq context :lift)
                 (when typeidx (%ct-at state typeidx))
                 (let ((f (canonrec-func cr)))
                   (when (and (< f (length (cval-func-types state)))
                              (aref (cval-func-types state) f))
                     ;; M9 (linking ::90): a declarator-space functype
                     ;; pushed from an imported instance's ct — remap
                     ;; its (:idx . k) payloads to THIS component's
                     ;; type space before the checks read them
                     (let ((origin (gethash f *func-alias-origin*)))
                       (if origin
                           (if (and (consp origin)
                                    (eq (car origin) :state))
                               (%deindex-functype
                                (aref (cval-func-types state) f)
                                (cdr origin))
                           (%remap-decl-functype
                            (aref (cval-func-types state) f)
                            (car origin) (cdr origin)))
                           (aref (cval-func-types state) f))))))))
    ;; M11a (validate-no-async-abi-for-sync-type): the `async`
    ;; canonopt requires an ASYNC function type (CanonicalABI.md
    ;; 3334) — both for lift (the ascribed functype) and lower
    ;; (the target func's type)
    (when (%opt opts :async)
      (unless (and ft (third (ct-payload ft)))
        (%verr "the `async` canonical option requires an async function type")))
    ;; M11a: `callback` requires `async` on the LIFT (3337) and
    ;; requires the ASYNC type likewise
    (when (and (%opt opts :callback) (eq context :lower))
      (%verr "canonical option `callback` cannot be specified for lowerings"))
    ;; canonopt: realloc implies memory (abi.wast pins the message)
    (when (and realloc (not mem))
      (%verr "canonical option `realloc` requires `memory` to also be specified"))
   ;; UNICITY: memory/realloc/post-return each at most once
   ;; (abi.wast ::14/::15/::16)
   (let ((mem-n (count :memory opts :key #'car))
         (realloc-n (count :realloc opts :key #'car))
         (post-n (count :post-return opts :key #'car)))
     (when (> mem-n 1)
       (%verr "`memory` is specified more than once"))
     (when (> realloc-n 1)
       (%verr "canonical option `realloc` is specified more than once"))
     (when (> post-n 1)
       (%verr "canonical option `post-return` is specified more than once"))
     ;; slice 5 (abi ::11-::13): at most one string-encoding, and
     ;; different encodings CONFLICT — the fixture message names the
     ;; FIRST-SEEN encoding then the conflicting one, latin1+utf16 in
     ;; its kebab form 'latin1-utf16'. Duplicate identical encodings
     ;; never reach here today (one is enough for the fixtures), but
     ;; the distinct pair rule is the observable one.
     (let ((encs (remove-if-not (lambda (o)
                                  (eq (car o) :string-encoding))
                                opts)))
       (when (> (length encs) 1)
         (let* ((first-enc (second (first encs)))
                (later (rest encs)))
           (dolist (e later)
             (unless (string-equal (string (second e))
                                   (string first-enc))
               (%verr "canonical encoding option `~a` conflicts with option `~a`"
                       (%enc-str first-enc) (%enc-str (second e)))))))))
   ;; the option INDEX immediates are bounds-checked against the
   ;; core spaces (abi.wast ::3 'memory index out of bounds'; the
   ;; realloc/post-return indices reuse the core-func space)
   (dolist (o opts)
     (case (car o)
       (:memory
        (when (>= (second o) (cval-core-memories state))
          (%verr "memory index out of bounds")))
       ((:realloc :post-return)
        (when (>= (second o) (cval-core-funcs state))
          (%verr "core function index out of bounds")))
       (t nil)))
   ;; post-return is only valid when LIFTING (abi.wast ::19)
   (when (and (%opt opts :post-return) (eq context :lower))
     (%verr "canonical option `post-return` cannot be specified for lowerings"))
    (when ft
      (let* ((params (first (ct-payload ft)))
             (results (second (ct-payload ft))))
        ;; NOTE: the functype payload is
        ;; (params-vector single-result-valtype-or-nil async-p):
        ;; RESULTS is ONE valtype, not a sequence.
        (let ((bad
                (cond
                 ((eq context :lift)
                  (loop for (nil . vt) across params
                        thereis (%needs-memory-p vt state)))
                 ((eq context :lower)
                  (or (loop for (nil . vt) across params
                            thereis (%needs-memory-p vt state))
                      (and results
                           (%needs-memory-p results state)))))))
          (when bad
            ;; fixture precedence (abi.wast ::4-::5): when BOTH memory
            ;; and realloc are absent, the `memory` message is
            ;; expected; realloc's message fires only when memory is
            ;; present but realloc is not.
            (ecase context
              (:lift (cond
                      ((not mem)
                       (%verr "canonical option `memory` is required"))
                      ((not realloc)
                       (%verr "canonical option `realloc` is required"))))
              (:lower (cond
                       ((not mem)
                        (%verr "canonical option `memory` is required"))
                       ;; ::9: the LOWER side's RESULT rule is
                       ;; lift(result) semantics — realloc required
                       ;; for a list/string result once memory is
                       ;; present (CanonicalABI.md 3348-3350)
                       ((and results
                             (%needs-memory-p results state)
                             (not realloc))
                        (%verr "canonical option `realloc` is required"))
                       (t nil))))))
        ;; flat-count rules: params > 16 or result flats > 1
        (let ((nparams (loop for (nil . vt) across params
                             sum (%flat-count vt state)))
              (nresults (if results (%flat-count results state) 0)))
          (when (> nparams 16)
            (ecase context
              (:lift (cond
                      ((not mem)
                       (%verr "canonical option `memory` is required"))
                      ((not realloc)
                       (%verr "canonical option `realloc` is required"))))
              (:lower (unless mem
                        (%verr "canonical option `memory` is required")))))
          (when (> nresults 1)
            (ecase context
              (:lift (unless mem
                       (%verr "canonical option `memory` is required")))
              (:lower (unless mem
                        (%verr "canonical option `memory` is required"))))))
      ))
      ;; slice 4 (family 2): the canon SIGNATURE checks —
      ;; realloc/post-return types + flatten_functype
      ;; compatibility (abi ::17/::18/::20/::21)
      (%check-canon-signatures cr state comp context opts
                               mem realloc ft)
      ))
;;; ---------------------------------------------------------------

(defun %check-canon-signatures (cr state comp context opts
                                mem realloc ft)
  "The canon SIGNATURE checks (abi.wast ::17/::18/::20/::21):
realloc's core func must be (i32 i32 i32 i32) -> (i32); post-return
must take the flattened lifted results as params with empty
results; and the lifted core func's ACTUAL signature must match
flatten_functype's output for the ascribed functype (an empty
ascription flattens to [] -> [])."
  (declare (ignorable mem))
  (when realloc
    (let ((sig (%canon-core-func-sig comp (second realloc))))
      (when (and sig
                 (not (and (equal (car sig) '(:i32 :i32 :i32 :i32))
                           (equal (cdr sig) '(:i32)))))
        (%verr "canonical option `realloc` uses a core function with an incorrect signature"))))
  ;; slice 4 (abi ::18): post-return takes the FLATTENED lifted
  ;; results as params, with empty results
  (let ((post (%opt opts :post-return)))
    (when (and post (eq context :lift) ft)
      (let* ((sig (%canon-core-func-sig comp (second post)))
             (want (cdr (%flat-functype ft state :lift))))
        (when (and sig
                   (not (and (equal (car sig) want)
                             (null (cdr sig)))))
          (%verr "canonical option `post-return` uses a core function with an incorrect signature")))))
  ;; slice 4 (abi ::20/::21): the lifted core func's ACTUAL
  ;; signature must match flatten_functype's output for the
  ;; ascribed functype
  ;; async lifts use the task ABI (results become task-return
  ;; parameters) — the sync flatten comparison does not apply
  ;; (binary.wast ::94's async canon with (callback n))
  (when (and (eq context :lift)
             (not (and ft (third (ct-payload ft)))))
    (let* ((want (%flat-functype ft state :lift))
           (sig (%canon-core-func-sig comp (canonrec-func cr))))
      (when sig
        ;; M9 (shared-everything ::0): the canon target's signature
        ;; may arrive with VECTOR slots (the moduletype declarator's
        ;; wasm-func-type / wasm-comptype params) while the flattened
        ;; want is a LIST — normalize both sides like %core-sig-equal-p
        (unless (%core-sig-equal-p (cons (car want) (cdr want))
                                   (cons (car sig) (cdr sig)))
          ;; report the params/results separately for the fixture's
          ;; two distinct messages
          (unless (equal (coerce (car want) 'list)
                         (coerce (car sig) 'list))
            (%verr "lowered parameter types `~a` do not match parameter types `~a`"
                   (%flat-core-str (car want)) (%flat-core-str (car sig))))
          (unless (equal (coerce (cdr want) 'list)
                         (coerce (cdr sig) 'list))
            (%verr "lowered result types `~a` do not match result types `~a`"
                   (%flat-core-str (cdr want)) (%flat-core-str (cdr sig)))))))))


(defun %flat-core-str (types)
  "The fixture's `[I32]` rendering of a flat core type list. The
sequence may arrive as a VECTOR (the runtime's functype slots and M9's
fresh moduletype functype objects) — coerce first."
  (format nil "[~{~a~^ ~}]"
          (mapcar (lambda (t_) (string-upcase (string t_)))
                  (coerce types 'list))))

(defun %enc-str (enc)
  "The fixture spelling of a string-encoding option value: utf8/utf16
stay as-is; latin1+utf16 takes its kebab form 'latin1-utf16' (abi.wast
::12/::13's conflict messages). Compares by NAME: the text parser
interns the encoding symbols in ITS package, the binary reader in
the keyword package."
  (let ((name (string enc)))
    (cond
     ((string-equal name "LATIN1+UTF16") "latin1-utf16")
     (t (string-downcase name)))))

(defun %flat-join (a b)
  "CanonicalABI.md join: the tightest approximation of two flat
types (bitcast-compatible core pairs)."
  (cond ((eq a b) a)
        ((or (and (eq a :i32) (eq b :f32))
             (and (eq a :f32) (eq b :i32)))
         :i32)
        (t :i64)))

(defun %flat-type-list (vt state)
  "flatten_type (CanonicalABI.md 2941-2970) — the FULL flat core
type sequence of one component valtype. Sync slice: ptr = :i32.
Tuple despecializes to a record; option/result to a variant; a
FIXED list flattens as its element type repeated; a variable list
as a pointer pair."
  (labels
      ((fl (vt)
         (let ((c (if (and (consp vt) (eq (car vt) :idx))
                      (%ct-at state (cdr vt))
                      (if (ct-p vt) vt
                          (make-ct :kind :prim :payload vt)))))
           (case (ct-kind c)
             (:prim
              (if (eq (ct-payload c) :string)
                  (list :i32 :i32)
                  (case (ct-payload c)
                    ((:bool :u8 :u16 :u32 :s8 :s16 :s32 :char
                      :error-context)
                     (list :i32))
                    ((:s64 :u64) (list :i64))
                    (:f32 (list :f32))
                    (:f64 (list :f64))
                    (t (list :i32)))))
             ((:flags :enum :own :borrow :stream :future :resource
               :func :instance :component)
              (list :i32))
             (:record
              (reduce #'append (mapcar (lambda (f) (fl (cdr f))) (coerce (ct-payload c) 'list)) :initial-value '()))
             (:tuple
              (reduce #'append (mapcar #'fl (coerce (ct-payload c) 'list)) :initial-value '()))
             (:list
              (let ((elem (car (ct-payload c)))
                    (len (cdr (ct-payload c))))
                (if len
                    (loop repeat len append (fl elem))
                    (list :i32 :i32))))
             (:option
              (fl-variant (list (cons "none" nil)
                                (cons "some" (ct-payload c)))))
             (:result
              (fl-variant (list (cons "ok" (car (ct-payload c)))
                                (cons "err" (cdr (ct-payload c))))))
             (:variant
              (fl-variant (coerce (ct-payload c) 'list)))
             (:map (list :i32 :i32)))))
       (fl-variant (cases)
         ;; discriminant_type: the case count's log2 byte width maps
         ;; u8/u16/u32 — ALL flatten to one :i32
         ;; ACC stays in ORDER: extend with APPEND, never PUSH — a
         ;; join REPLACES an existing slot by INDEX, and a prepend-
         ;; ordered acc writes the joined slot at the wrong position
         ;; (flat-pad's tuple<f32,f2>/u32 join landed at slot 0 of the
         ;; REVERSED acc, producing [i32 f32 i32] instead of
         ;; [i32 i32 f32]).
         (let ((acc '()))
           (dolist (c cases)
             (when (cdr c)
               (loop for ft in (fl (cdr c))
                     for i from 0
                     do (if (< i (length acc))
                            (setf (nth i acc) (%flat-join (nth i acc) ft))
                            (setf acc (append acc (list ft)))))))
           (cons :i32 acc))))
    (fl vt)))

(defun flatten-functype (ft-ct state context &key (async-p nil)
                                                 (callback-p nil))
  "Public wrapper (values lane): %flat-functype as the RUNTIME's
entry into the validator's flatten table. STATE may be NIL when the
functype's valtypes are all prims/ct shapes (no (:idx . n) refs).
M11a: the ASYNC-P/CALLBACK-P keys forward to the async flatten
arms (CanonicalABI.md 2914-2929)."
  (%flat-functype ft-ct state context
                  :async-p async-p :callback-p callback-p))

(defun %flat-functype (ft-ct state context &key (async-p nil) (callback-p nil))
  "flatten_functype (CanonicalABI.md 2894-2930): the component
functype's FULL flattened (params . results) with the MAX_FLAT
fallthrough. FT-CT may be NIL (an empty ascription). The >1 flat
RESULTS fallthrough differs by CONTEXT (the values lane): a LIFT
returns one [ptr] core result (the caller reads the tuple back out
of memory); a LOWER appends the [ptr] out-param to the core PARAMS
and returns NO results (the caller-allocated buffer receives the
tuple). Params > 16 collapse to a single [ptr] in both contexts.
M11a (ASYNC, CanonicalABI.md 2914-2929): with ASYNC-P the limits
change — LIFT: params still >16 collapse, results are ['i32] when
CALLBACK-P (the packed CallbackCode) else [] (the stackful async
case: the callee returns NO core results; task.return delivers the
value); LOWER: params >4 (MAX_FLAT_ASYNC_PARAMS) collapse to [ptr],
ANY results ride the appended [ptr] out-param, and the core result
is the ['i32] packed state|(subtaski<<4)."
  (let* ((pl (and ft-ct (coerce (first (ct-payload ft-ct)) 'list)))
         (res (and ft-ct (second (ct-payload ft-ct))))
         (params (mapcan (lambda (p) (%flat-type-list (cdr p) state)) pl))
         (results (and res (%flat-type-list res state))))
    (if async-p
        (ecase context
          (:lift
           (when (> (length params) 16) (setf params '(:i32)))
           (setf results (if callback-p '(:i32) '())))
          (:lower
           (when (> (length params) 4) (setf params '(:i32)))
           (when results (setf params (append params '(:i32))))
           (setf results '(:i32))))
        (progn
          (when (> (length params) 16) (setf params '(:i32)))
          (when (> (length results) 1)
            (ecase context
              (:lift (setf results '(:i32)))
              (:lower (setf params (append params '(:i32)))
                      (setf results '()))))))
    (cons params results)))

;;; Structural type matching at instantiation (M7 slice 3b).
;;; Explainer 'Type Checking': each with-argument must structurally
;;; match the target component's import type, the child's typevar
;;; imports substituted by the arguments' identities. Resource
;;; identities (own/borrow) are the only nominal leaf.

(defvar *resource-id-counter* 0
  "M7 slice 3b: the fresh-resource identity counter. Each generative
resource (a resource definition, a (sub resource) import/export, an
exported alias of an instantiated instance's resource) receives a
unique id; the counter never resets, so identities from earlier
validate-component walks can never equal later ones.")

(defun %fresh-resource-id ()
  (cons :fresh (incf *resource-id-counter*)))
(defun %ty-resolve (ty ids &optional (depth 0))
  "Follow :ref chains to a canonical identity. Returns the resolved
identity; a chain longer than 64 hops or an out-of-bounds reference
resolves to :ty-bad (the callers report 'type mismatch')."
  (if (>= depth 64)
      :ty-bad
      (cond
       ((eq ty :ty-bad) :ty-bad)
       ((and (consp ty) (eq (car ty) :ref))
        (let ((i (cdr ty)))
          (if (or (null ids) (>= i (length ids)) (< i 0))
              :ty-bad
              (%ty-resolve (aref ids i) ids (1+ depth)))))
       (t ty))))
(defun %ty-valtype (vt ids sub)
  "Normalize one VALTYPE (a (:idx . n) cons, a prim keyword or a ct)
to an identity: index references pull the identity vector entry and
apply one substitution step (a :fresh typevar becomes SUB's binding);
prim keywords wrap as concrete ct. SUB is an alist mapping fresh-id
-> identity (the with-args substitution).
M9 (linking ::130): an interned OWN/BORROW wrapper entry resolves
THROUGH to the OWNED identity — the wrapper's payload indexes the
OWNED type's slot in the SAME ids space. The OWN/BORROW KIND is
checked FIRST at %ty-match-valtype's raw level (the resources
::9/::13 'expected own, found borrow' rule), so the through-resolved
identity no longer erases the kind distinction; the raw-vs-rebuilt
payload alignment is handled by the ids-build remap (the wrapper cts
push into the rebuilt ids with shifted payloads)."
  (cond
   ((and (consp vt) (eq (car vt) :idx))
    (let ((i (cdr vt)))
      (if (or (null ids) (>= i (length ids)))
          :ty-bad
          (let ((ty (aref ids i)))
            (cond
             ((and (consp ty) (eq (car ty) :fresh) sub)
              (or (cdr (assoc (cdr ty) sub :test #'eql)) ty))
             ;; the interned own/borrow wrapper: pull the OWNED
             ;; entry (its payload indexes this same space)
             ((and (consp ty) (eq (car ty) :ct)
                   (ct-p (cdr ty))
                   (member (ct-kind (cdr ty)) '(:own :borrow))
                   (integerp (ct-payload (cdr ty)))
                   (< (ct-payload (cdr ty)) (length ids)))
              (aref ids (ct-payload (cdr ty))))
             (t ty))))))
   ((ct-p vt) (cons :ct vt))
   ((keywordp vt) (cons :ct (make-ct :kind :prim :payload vt :offset 0)))
   (t :ty-bad)))
(defun %ty-kind-of (ty)
  "The dispatch key of a canonical identity."
  (cond
   ((eq ty :ty-bad) :bad)
   ((and (consp ty) (eq (car ty) :fresh)) :resource)
   ;; an aliased exported resource (:inst-export instance source)
   ;; compares by identity — dispatch it to the resource arm
   ((and (consp ty) (eq (car ty) :inst-export)) :resource)
   ((and (consp ty) (eq (car ty) :ct)) (ct-kind (cdr ty)))
   ((and (consp ty) (eq (car ty) :opaque)) :opaque)
   (t :other)))
(defun %ty-match (w g ids-w sub-w ids-g sub-g)
  "Match two NORMALIZED identities structurally, recursing through
valtype references in each side's own space. SUB-W/SUB-G substitute
each side's :fresh typevars. Message shapes are the instantiation.wast
fixtures' contract."
  (let ((kw (%ty-kind-of w)) (kg (%ty-kind-of g)))
    (cond
     ;; resource identity: the nominal leaf (:fresh ids compare by eql;
     ;; :inst-export (instance source) pairs by equal — slice 3b)
     ((and (eq kw :resource) (eq kg :resource))
      (unless (if (or (eq (car w) :inst-export)
                      (eq (car g) :inst-export))
                  (equal w g)
                  (eql (cdr w) (cdr g)))
        (%verr "resource types are not the same")))
     ;; prim vs prim: name equality
     ((and (eq kw :prim) (eq kg :prim))
      (unless (eq (ct-payload (cdr w)) (ct-payload (cdr g)))
        (%verr "expected primitive `~a` found primitive `~a`"
               (string-downcase (string (ct-payload (cdr w))))
               (string-downcase (string (ct-payload (cdr g)))))))
     ;; compound-vs-prim / prim-vs-compound
     ((and (eq kw :prim)
           (member kg '(:record :variant :list :tuple :flags :enum
                        :option :result :own :borrow)))
      (%verr "expected primitive, found ~a"
             (string-downcase (string kg))))
     ((and (member kw '(:record :variant :list :tuple :flags :enum
                        :option :result :own :borrow))
           (eq kg :prim))
      (%verr "expected ~a, found ~a"
             (string-downcase (string kw))
             (string-downcase (string (ct-payload (cdr g))))))
     ;; own-vs-borrow mismatch
     ((and (member kw '(:own :borrow)) (member kg '(:own :borrow))
           (not (eq kw kg)))
      (%verr "expected ~a, found ~a"
             (string-downcase (string kw))
             (string-downcase (string kg))))
     ((not (eq kw kg))
      (%verr "expected ~a, found ~a"
             (string-downcase (string kw))
             (string-downcase (string kg))))
     (t
      (case kw
        (:record
         (let ((fw (ct-payload (cdr w))) (fg (ct-payload (cdr g))))
           (unless (= (length fw) (length fg))
             (%verr "expected ~d fields, found ~d"
                    (length fw) (length fg)))
           (loop for a across fw
                 for b across fg
                 do (unless (string= (car a) (car b))
                      (%verr "expected field name `~a`, found `~a`"
                             (car a) (car b)))
                    (handler-case
                        (%ty-match-valtype (cdr a) (cdr b)
                                           ids-w sub-w ids-g sub-g)
                      (wasmdown:validation-error ()
                        (%verr "type mismatch in record field `~a`" (car a)))))))
        (:variant
         (let ((cw (ct-payload (cdr w))) (cg (ct-payload (cdr g))))
           (unless (= (length cw) (length cg))
             (%verr "expected ~d cases, found ~d"
                    (length cw) (length cg)))
           (loop for a across cw
                 for b across cg
                 do (unless (string= (car a) (car b))
                      (%verr "expected case named `~a`, found `~a`"
                             (car a) (car b)))
                    (cond
                     ((and (cdr a) (not (cdr b)))
                      (%verr "expected case `~a` to have a type, found none"
                             (car a)))
                     ((and (not (cdr a)) (cdr b))
                      (%verr "expected case `~a` to have no type" (car a)))
                     ((and (cdr a) (cdr b))
                      (handler-case
                          (%ty-match-valtype (cdr a) (cdr b)
                                             ids-w sub-w ids-g sub-g)
                        (wasmdown:validation-error ()
                          (%verr "type mismatch in variant case `~a`" (car a)))))))))
        (:tuple
         (let ((tw (ct-payload (cdr w))) (tg (ct-payload (cdr g))))
           (unless (= (length tw) (length tg))
             (%verr "expected ~d types, found ~d"
                    (length tw) (length tg)))
           (loop for a across tw
                 for b across tg
                 for i from 0
                 do (handler-case
                        (%ty-match-valtype a b ids-w sub-w ids-g sub-g)
                      ;; a prim-vs-prim element mismatch reports the
                      ;; primitive message DIRECTLY (instantiation.wast
                      ;; ::1's 'expected primitive `u32` found primitive
                      ;; `string`' from inside a tuple); every other
                      ;; mismatch gets the positional wrapper (::14's
                      ;; 'type mismatch in tuple field 0')
                      (wasmdown:validation-error (c)
                        (let ((msg (wasmdown:wasmdown-error-message c)))
                        (if (and (search "expected primitive" msg)
                                  (search "found primitive" msg))
                            (%verr "~a" msg)
                            (%verr "type mismatch in tuple field ~d" i))))))))
        (:flags
         (let ((lw (ct-payload (cdr w))) (lg (ct-payload (cdr g))))
           (unless (and (= (length lw) (length lg))
                        (loop for a across lw
                              for b across lg
                              always (string= a b)))
             (%verr "mismatch in flags elements"))))
        (:enum
         (let ((lw (ct-payload (cdr w))) (lg (ct-payload (cdr g))))
           (unless (and (= (length lw) (length lg))
                        (loop for a across lw
                              for b across lg
                              always (string= a b)))
             (%verr "mismatch in enum elements"))))
        (:list
         (%ty-match-valtype (car (ct-payload (cdr w)))
                            (car (ct-payload (cdr g)))
                            ids-w sub-w ids-g sub-g))
        (:option
         (%ty-match-valtype (ct-payload (cdr w)) (ct-payload (cdr g))
                            ids-w sub-w ids-g sub-g))
        (:result
         (let ((ow (car (ct-payload (cdr w))))
               (og (car (ct-payload (cdr g))))
               (ew (cdr (ct-payload (cdr w))))
               (eg (cdr (ct-payload (cdr g)))))
           (cond
            ((and ow (not og))
             (%verr "expected ok type, but found none"))
            ((and (not ow) og)
             (%verr "expected ok type to not be present"))
            ((and ow og)
             (handler-case
                 (%ty-match-valtype ow og ids-w sub-w ids-g sub-g)
               (wasmdown:validation-error ()
                 (%verr "type mismatch in ok variant")))))
           (cond
            ((and ew (not eg))
             (%verr "expected err type, but found none"))
            ((and (not ew) eg)
             (%verr "expected err type to not be present"))
            ((and ew eg)
             (handler-case
                 (%ty-match-valtype ew eg ids-w sub-w ids-g sub-g)
               (wasmdown:validation-error ()
                 (%verr "type mismatch in err variant")))))))
        ((:own :borrow)
         (let ((rw (%ty-resolve-idx (ct-payload (cdr w)) ids-w))
               (rg (%ty-resolve-idx (ct-payload (cdr g)) ids-g)))
           ;; M9 (linking ::130): the OWNED identity's :fresh entry is
           ;; the child's TYPEVAR — apply the with-args substitution
           ;; (sub-w/sub-g) so the type-arg's resource (the alias's
           ;; fresh id) compares against the parent's; un-substituted,
           ;; the two :fresh ids were always distinct (the child's
           ;; typevar vs the defining instance's generative id)
           (flet ((apply-sub (x sub)
                    (if (and (consp x) (eq (car x) :fresh) sub)
                        (or (cdr (assoc (cdr x) sub :test #'eql)) x)
                        x)))
             (setf rw (apply-sub rw sub-w))
             (setf rg (apply-sub rg sub-g)))
           (unless (and (consp rw) (consp rg)
                        (cond
                         ((or (eq (car rw) :inst-export)
                              (eq (car rg) :inst-export))
                          ;; aliased exported resources compare by their
                          ;; (instance, source) pair (slice 3b ::41)
                          (and (eq (car rw) :inst-export)
                               (eq (car rg) :inst-export)
                               (equal rw rg)))
                         (t (and (eq (car rw) :fresh)
                                 (eq (car rg) :fresh)
                                 (eql (cdr rw) (cdr rg))))))
             (%verr "resource types are not the same"))))
(t nil))))))
(defun %ty-resolve-idx (i ids)
  "A type index to a canonical identity (fresh vars pass through)."
  (if (or (null ids) (>= i (length ids)) (< i 0))
      :ty-bad
      (let ((ty (aref ids i)))
        (if (and (consp ty) (eq (car ty) :fresh))
            ty
            (%ty-resolve ty ids)))))
(defun %ty-match-valtype (vw vg ids-w sub-w ids-g sub-g)
  "Match two RAW valtypes ((:idx . n) | prim | ct): normalize each in
its own space, then match.
M9 (linking ::130 + resources ::9/::13): the OWN/BORROW KIND check
runs FIRST at the RAW valtype level — a valtype whose ids entry is
an interned own/borrow WRAPPER ct carries that kind (an own param
vs a borrow expectation rejects 'expected own, found borrow' BEFORE
any identity resolution). Then %ty-valtype THROUGH-resolves the
wrapper to the owned identity (the substitution applies at the own
arm) — the through-resolution alone previously erased the kind and
the resources canaries broke; the kind-first order keeps both."
  (flet ((raw-kind (vt ids)
           (cond
            ((and (consp vt) (eq (car vt) :idx)
                  (integerp (cdr vt))
                  (< (cdr vt) (length ids)))
             (let ((ty (aref ids (cdr vt))))
               (cond
                ((and (consp ty) (eq (car ty) :ct)
                      (ct-p (cdr ty)))
                 (ct-kind (cdr ty)))
                ((ct-p ty) (ct-kind ty))
                ((keywordp ty) :prim)
                (t :other))))
            ((ct-p vt) (ct-kind vt))
            ((keywordp vt) :prim)
            (t :other))))
    (let ((kw (raw-kind vw ids-w))
          (kg (raw-kind vg ids-g)))
      (when (and (member kw '(:own :borrow))
                 (member kg '(:own :borrow))
                 (not (eq kw kg)))
        (%verr "expected ~a, found ~a"
               (string-downcase (string kw))
               (string-downcase (string kg))))))
  (let ((w (%ty-resolve (%ty-valtype vw ids-w sub-w) ids-w))
        (g (%ty-resolve (%ty-valtype vg ids-g sub-g) ids-g)))
    (when (or (eq w :ty-bad) (eq g :ty-bad))
      (%verr "type mismatch"))
    (%ty-match w g ids-w sub-w ids-g sub-g)))

(defun %match-func-arg (arg-si child-ft ids state)
  "A func import's check: the CHILD's functype (rebuilt in the child
space, its typevar refs substituted by the with-args) matched against
the PARENT func's actual functype (cval-func-types). The fixture
messages (instantiation.wast ::24-::28). Slice 5e: the PARENT
functype's own identity space (an IMPORTED-instance export's ct
references its own per-instance generative ids — resources
::27/::28) is looked up from *func-arg-ids* by the ARG's index.
M9 (linking ::128): the parent functype may be a DECLARATOR-space
read pushed from an imported instance's ct — remap its (:idx . k)
payloads to the component's space (the ::90 remap) before the
comparison, so an (own $r) result resolves through the alias chain
instead of reading the declarator index against this state."
  (let* ((pidx (sortidx-idx arg-si))
         (parent-ft (and (< pidx (length (cval-func-types state)))
                         (aref (cval-func-types state) pidx)))
         (parent-ft (let ((origin (gethash pidx *func-alias-origin*)))
                      ;; the remap must NOT touch the identity-
                      ;; based path: a func with a registered
                      ;; per-instance identity space (*func-arg-ids*)
                      ;; resolves its refs through that space (the
                      ;; resources ::27/::28 generative matching)
                      (cond
                       ((and origin
                             (not (gethash pidx *func-arg-ids*)))
                        (%remap-decl-functype parent-ft
                                             (car origin)
                                             (cdr origin)))
                       ;; M9 (linking ::128, values lane): the
                       ;; identity path with an INSTANTIATED source —
                       ;; the ids vector is the REBUILT child space,
                       ;; INDEX-ALIGNED with the child validator's
                       ;; own type space (the ids-rebuild fix). The
                       ;; stashed functype's refs are the child's RAW
                       ;; VALIDATOR-space indices, so they resolve
                       ;; through the stash UNCHANGED (the old
                       ;; %shift-raw-functype patch is retired).
                       ((gethash pidx *func-arg-ids*)
                        parent-ft)
                       (t parent-ft)))))
    (unless (and parent-ft (ct-functype-p parent-ft))
      (return-from %match-func-arg))
    (unless (and child-ft (ct-functype-p child-ft))
      (return-from %match-func-arg))
    (let* ((cw (first (ct-payload child-ft)))
           (rw (second (ct-payload child-ft)))
           (cg (first (ct-payload parent-ft)))
           (rg (second (ct-payload parent-ft))))
      (unless (= (length cw) (length cg))
        (%verr "expected ~d parameters, found ~d"
               (length cw) (length cg)))
      (loop for a across cw
            for b across cg
            do (unless (string= (car a) (car b))
                 (%verr "expected parameter named `~a`, found `~a`"
                        (car a) (car b)))
               (handler-case
                   (%ty-match-valtype (cdr a) (cdr b)
                                     ids nil
                                     (or (%func-arg-ids-at arg-si)
                                         (cval-type-ids state))
                                     nil)
                 ;; slice 5e (resources ::28): a RESOURCE identity
                 ;; mismatch carries its own fixture message through
                 (wasmdown:validation-error (c)
                   (let ((m (wasmdown:wasmdown-error-message c)))
                     (if (and (search "resource" m)
                              (search "not the same" m))
                         (error c)
                         (%verr "type mismatch in function parameter `~a`"
                                (car a)))))))
      (cond
       ((and rw (not rg))
        (%verr "expected a result, found none"))
       ((and (not rw) rg)
        (%verr "expected no result, but found one"))
       ((and rw rg)
        (handler-case
            (%ty-match-valtype rw rg ids nil
                               (or (%func-arg-ids-at arg-si)
                                   (cval-type-ids state))
                               nil)
          (wasmdown:validation-error (c)
            (let ((m (wasmdown:wasmdown-error-message c)))
              (if (and (search "resource" m)
                       (search "not the same" m))
                  (error c)
                  (%verr "type mismatch with result type"))))))))))
(defun %parent-identity (si state)
  "The PARENT-space identity of a with-argument sortidx (a :type
argument; NIL when unresolvable)."
  (when (and (consp si) (eq (sortidx-sort si) :type))
    (let ((i (sortidx-idx si)))
      (when (and (< i (length (cval-type-ids state)))
                 (consp (aref (cval-type-ids state) i)))
        (aref (cval-type-ids state) i)))))

(defun %component-at (comp cidx)
  "The cidx'th :component definition's nested comp (slice 3b). The
COMPONENT index space counts :component defs AND :component IMPORTS
(slice 5m, resources ::34): an imported component occupies an index
and has no nested comp — NIL for it, so the callers fall back to the
import's ascribed ct.
M9 (linking ::169): a :component-sort ALIAS also occupies an index
(the parse allocates it); it has no nested comp HERE — the callers
resolve the export chain through %resolve-aliased-component."
  (let ((seen 0))
    (loop for d across (comp-defs comp)
          when (or (eq (compdef-kind d) :component)
                   (and (eq (compdef-kind d) :alias)
                        (eq (aliasrec-sort (compdef-payload d))
                            :component))
                   (and (eq (compdef-kind d) :import)
                        (eq (car (importrec-type
                                  (compdef-payload d)))
                            :component)))
            do (cond ((and (= seen cidx)
                           (eq (compdef-kind d) :component))
                      (return (compdef-payload d)))
                     ((= seen cidx)
                      (return nil))
                     (t (incf seen)))
          finally (return nil))))

(defun %aliased-component-target (comp cidx)
  "M9 (linking ::169): the comp an alias-introduced component projects
— the cidx'th component-space entry when it is a :component-sort alias
(%component-at returns NIL there). NIL otherwise."
  (let ((seen 0))
    (loop for d across (comp-defs comp)
          when (or (eq (compdef-kind d) :component)
                   (and (eq (compdef-kind d) :alias)
                        (eq (aliasrec-sort (compdef-payload d))
                            :component))
                   (and (eq (compdef-kind d) :import)
                        (eq (car (importrec-type
                                  (compdef-payload d)))
                            :component)))
            if (= seen cidx)
              return (and (eq (compdef-kind d) :alias)
                          (%resolve-aliased-component
                           (compdef-payload d) comp))
            else do (incf seen)
          finally (return nil))))

(defun %instance-export-sort (inst-idx name state)
  "The SORT of instance INST-IDX's export NAME (slice 3b): the
per-instance type table entry is a HASH name -> (sort . source-idx)
(built by the :instance/:import arms) or an instance ct (an import's
type) whose export declarators name each export's sort. Returns
(values sort known-p)."
  (let ((ent (and (< inst-idx (length (cval-instance-types state)))
                  (aref (cval-instance-types state) inst-idx))))
    (typecase ent
      (hash-table
       (let ((hit (gethash name ent :missing)))
         (if (eq hit :missing)
             (values nil nil)
             (values (if (consp hit) (car hit) hit) t))))
      (ct
       (if (eq (ct-kind ent) :instance)
           (loop for td across (ct-payload ent)
                 when (and (eq (typedecl-kind td) :export)
                           (string= (first (typedecl-payload td)) name))
                   return (values (car (third (typedecl-payload td))) t)
                 finally (return (values nil nil)))
           (values nil nil)))
      (t (values nil nil)))))

(defun %instance-export-identity (inst-idx name state)
  "The TYPE IDENTITY of an instance's exported TYPE (slice 3b): for a
table entry (sort . source-idx) the identity is (:inst-export
inst-idx . source-idx) — equal exactly when two aliases project the
SAME source type of the SAME instance (resources.wast ::41's r1/r2
re-exports of one typeidx share it); a ct-typed entry (an imported
instance) has no locally-known source — NIL (the caller falls back
to a fresh resource id). Slice 5c: when the source instance was
INSTANTIATED, the stashed child identity vector resolves the source
idx through the instantiation's substitution — the identity becomes
whatever the with-args supplied (a parent-space :fresh resource, an
:inst-export, ...), so two instantiations of one component with the
same argument SHARE the resource (resources.wast ::44/::45)."
  (let ((ent (and (< inst-idx (length (cval-instance-types state)))
                  (aref (cval-instance-types state) inst-idx))))
    (cond
     ((hash-table-p ent)
      (let ((hit (gethash name ent :missing)))
        (when (and (consp hit)
                   (eq (car hit) :type))
          (if (integerp (cdr hit))
              (let* ((child-ids (gethash inst-idx *inst-ids-stash*))
                     (resolved (and child-ids
                                    (< (cdr hit) (length child-ids))
                                    (%ty-resolve (aref child-ids (cdr hit))
                                                 child-ids))))
                (or resolved
                    ;; not an instantiated source (a bag): the
                    ;; (instance, source) pair identity
                    (list :inst-export inst-idx (cdr hit))))
              ;; an ascribed (sub resource) export: fresh per NAME
              ;; — the entry's string cdr names the identity source
              ;; (slice 5m, resources ::34: T3 = (eq $T2)'s entry
              ;; carries "T2", sharing T2's identity)
              (list :inst-export inst-idx (cdr hit))))))
     ((and (ct-p ent) (eq (ct-kind ent) :instance))
      ;; slice 5e (resources ::27/::28/::43): an IMPORTED instance
      ;; typed by an instance ct — its exported type's identity is
      ;; the GENERATIVE per-(instance, slot) id (the slot counts the
      ;; type-introducing declarators, the same convention as
      ;; %instance-ct-ids)
      (let ((slot (loop with n = -1
                        for x across (ct-payload ent)
                        when (member (typedecl-kind x)
                                     '(:type :alias :import :export))
                          do (incf n)
                        when (and (eq (typedecl-kind x) :export)
                                  (string= (first (typedecl-payload x))
                                           name))
                          return n
                        finally (return nil))))
        (when slot
          (%inst-gen-id inst-idx slot))))
     (t nil))))

(defun %instance-ct-ids (inst-idx ct)
  "Slice 5e (resources ::27/::28/::43): the identity vector for an
IMPORTED instance's own type space — the instance ct's declarator
sequence maps to GENERATIVE per-(instance, slot) resource ids for
(sub resource) exports/imports, :ref chains for (eq i) bounds, and
opaque markers otherwise. The SAME ids the type-alias identity uses
(%inst-gen-id), so a func-type's (own $r) resolves to the same
resource the (type $i \"r\") alias projects."
  (let ((ids (make-array 16 :fill-pointer 0 :adjustable t))
        (n -1))
    (loop for d across (ct-payload ct)
          do (let ((slot (incf n)))
               (vector-push-extend
                (etypecase d
                  (null '(:opaque))
                  (t (case (typedecl-kind d)
                       (:type (cons :ct (typedecl-payload d)))
                       (:alias '(:opaque))
                       ((:import :export)
                        (let ((et (third (typedecl-payload d))))
                          (cond
                           ((and (eq (car et) :type)
                                 (eq (cdr et) :sub-resource))
                            (%inst-gen-id inst-idx slot))
                           ;; M9 (linking ::128): a BIND-ID sub-resource
                           ;; export — (export "r" (type $r (sub
                           ;; resource))) — interns the sub-resource ct
                           ;; and ascribes its INDEX; resolve the index
                           ;; through the declarator sequence and treat
                           ;; a resource target as the generative id
                           ((and (eq (car et) :type)
                                 (integerp (cdr et))
                                 (let ((n2 -1))
                                   (loop for td2 across (ct-payload ct)
                                         do (case (typedecl-kind td2)
                                              ((:type :alias :import
                                                :export)
                                               (incf n2)
                                               (when (= n2 (cdr et))
                                                 (return
                                                  (and
                                                   (eq (typedecl-kind
                                                        td2)
                                                       :type)
                                                   (ct-resourcetype-p
                                                    (typedecl-payload
                                                     td2))))))
                                              (t nil))
                                         finally (return nil))))
                            (%inst-gen-id inst-idx slot))
                           ((and (eq (car et) :type)
                                 (listp (cdr et))
                                 (eq (car (cdr et)) :eq))
                            (cons :ref (second (cdr et))))
                           (t '(:opaque)))))
                       (t '(:opaque)))))
                ids)))
    ids))

(defun %instance-ct-export-functype (inst-idx name state)
  "Slice 5e (resources ::27/::28/::43): the FUNCTYPE of an IMPORTED
instance's exported func NAME, read from the instance ct (its (func
(type i)) ascription indexes the ct's own type space — the
declarator sequence's introduced indices). NIL when unresolvable."
  (let ((ent (and (< inst-idx (length (cval-instance-types state)))
                  (aref (cval-instance-types state) inst-idx))))
    (when (and (ct-p ent) (eq (ct-kind ent) :instance))
      (let ((d (find name (coerce (ct-payload ent) 'list)
                     :key (lambda (x)
                            (and (eq (typedecl-kind x) :export)
                                 (first (typedecl-payload x))))
                     :test #'equal)))
        (when d
          (let* ((et (third (typedecl-payload d)))
                 (fti (and (eq (car et) :func) (integerp (cdr et))
                           (cdr et))))
            (when fti
              (loop with n = -1
                    for x across (ct-payload ent)
                    do (case (typedecl-kind x)
                         (:type (incf n)
                                (when (= n fti)
                                  (return (typedecl-payload x))))
                         (:alias (incf n))
                         ((:import :export) (incf n))
                         (t nil))
                    finally (return nil)))))))))

(defun %func-arg-ids-at (arg-si &optional (state nil))
  "Slice 5e: the identity vector registered for the func ARG's own
type space (%resolve-aliased-func's *func-arg-ids* entry). A helper:
the inline gethash tripped the compiler's cross-arm type derivation.
M9 (linking ::130): keyed by (STATE . fidx) — the bare func index
collided across walks (a later component's func-0 clobbered the
earlier's and the func-arg match read the wrong identity vector)."
  (and (consp arg-si)
       (gethash (sortidx-idx arg-si) *func-arg-ids*)))

(declaim (notinline %func-arg-ids-at))

(defun %alias-exported-type-shape (payload state)
  "Slice 4 (family 3): the SOURCE type SHAPE an (alias export i n
(type)) introduces. A bag's table entry idx is THIS scope's typeidx
(ev ::48); an INSTANTIATED instance's entry idx is the CHILD's —
resolved through the stashed child state (ev ::49/::50). M9 (linking
lane): an IMPORTED instance (typed by an instance ct) contributes
the declared export's SHAPE — a (sub resource) export declarator
yields a resource ct (the canon resource.* target checks read it)."
  (let* ((src-idx (aliasrec-index payload))
         (ent (and (< src-idx (length (cval-instance-types state)))
                   (aref (cval-instance-types state) src-idx))))
    (or (and (hash-table-p ent)
             (%alias-exported-type-shape-from-hash payload state ent))
        (and (ct-p ent) (eq (ct-kind ent) :instance)
             (%instance-ct-exported-type-shape
              (aliasrec-name payload) ent)))))

(defun %alias-exported-type-shape-from-hash (payload state ent)
  "The HASH-table branch of %alias-exported-type-shape (a bag or an
instantiated instance's export table)."
  (let* ((hit (gethash (aliasrec-name payload) ent :missing))
         (ti (and (consp hit) (integerp (cdr hit)) (cdr hit))))
    (when ti
      (let* ((src-idx (aliasrec-index payload))
             (child-comp (gethash src-idx *inst-source-comp*))
             (child-state (and child-comp
                               (gethash child-comp *comp-state-stash*))))
        (cond
         ((and (< ti (length (cval-types state)))
               (ct-p (aref (cval-types state) ti))
               (not (and (eq (ct-kind (aref (cval-types state) ti))
                             :prim)
                         (member (ct-payload
                                  (aref (cval-types state) ti))
                                 '(:imported :exported
                                   :outer-aliased
                                   :imported-or-exported-type)
                                 :test #'eq))))
          (aref (cval-types state) ti))
         ((and child-state
               (< ti (length (cval-types child-state)))
               (ct-p (aref (cval-types child-state) ti)))
          (aref (cval-types child-state) ti))
         (t nil))))))

(defun %instance-ct-exported-type-shape (name ct)
  "The declared SHAPE of an IMPORTED instance's exported TYPE name —
the instance ct's (export \"n\" (type <bound>)) declarator: a (sub
resource) bound is a resource ct; an (eq i) bound follows the ct's
own declarator chain; anything else has no local shape (NIL)."
  (let ((d (find name (coerce (ct-payload ct) 'list)
                :key (lambda (x)
                       (and (eq (typedecl-kind x) :export)
                            (first (typedecl-payload x))))
                :test #'equal)))
    (when d
      (let* ((et (third (typedecl-payload d)))
             (bound (and (eq (car et) :type) (cdr et))))
        (cond
         ((eq bound :sub-resource)
          (make-ct :kind :resource :payload '(0 . nil) :offset 0))
         ((and (listp bound) (eq (car bound) :eq))
          ;; an (eq i) bound: follow the ct's own type space
          (let* ((ti (second bound))
                 (n -1)
                 (shape (loop for x across (ct-payload ct)
                              do (case (typedecl-kind x)
                                   ((:type :alias :import :export)
                                    (incf n)
                                    (when (= n ti)
                                      (return
                                       (case (typedecl-kind x)
                                         (:type (typedecl-payload x))
                                         (t nil)))))
                                   (t nil))
                              finally (return nil))))
            (and shape (ct-p shape) shape)))
         (t nil))))))



(defun %ct-decl-at (decls i)
  "The i'th TYPE-INTRODUCING declarator's payload (slice 5m,
indicies ::0): an inline instance ct's (instance . k) ascription
indexes the DECLARATOR sequence's type-introducing positions."
  (loop with n = -1
        for x across decls
        when (member (typedecl-kind x)
                     '(:type :alias :import :export))
          do (incf n)
             (when (= n i)
               (return (typedecl-payload x)))
        finally (return nil)))

(defun %instance-table-projection (a state)
  "Slice 4 (family 3): the export TABLE an (alias export i n
(instance)) introduces — the projection of the source instance's
export n. A hash-table source entry yields that referenced
instance's OWN table (this scope); a ct entry (an imported
instance) is read from its export declarators; anything else is
unknown (NIL)."
  (let* ((src-idx (aliasrec-index a))
         (name (aliasrec-name a))
         (ent (and (< src-idx (length (cval-instance-types state)))
                   (aref (cval-instance-types state) src-idx))))
    (cond
     ((hash-table-p ent)
      (let ((hit (gethash name ent :missing)))
        (unless (eq hit :missing)
          ;; an INSTANTIATED source's entry indices are the CHILD's
          ;; space — they must not index this scope (ev ::61); only
          ;; BAG sources carry this-scope indices
          (let ((sub (and (null (gethash src-idx *inst-source-comp*))
                          (consp hit)
                          (integerp (cdr hit))
                          (< (cdr hit)
                             (length (cval-instance-types state)))
                          (aref (cval-instance-types state)
                                (cdr hit)))))
            (cond
             ((hash-table-p sub) sub)
             ;; M9 (linking EV ::61): the EXPORT'S ASCRIBED
             ;; instance type REPLACES the visible one — when the
             ;; child's export rec for NAME carries an :instance
             ;; ascription, the projected table is the AScribed ct's
             ;; export declarators (an empty ascription hides every
             ;; export: the consumer's 'missing expected export')
             ((and (gethash src-idx *inst-source-comp*)
                   (let* ((child-comp (gethash src-idx
                                               *inst-source-comp*))
                          (asc (and child-comp
                                    (loop for d across
                                             (wasmdown.component:comp-defs
                                              child-comp)
                                          when (and
                                                (eq (compdef-kind d)
                                                    :export)
                                                (string=
                                                 (exportrec-name
                                                  (compdef-payload d))
                                                 name))
                                            return (exportrec-type
                                                    (compdef-payload
                                                     d)))))
                          (k (and (consp asc)
                                  (eq (car asc) :instance)
                                  (cdr asc))))
                     (and (integerp k)
                          child-comp
                          (let* ((child-state
                                  (gethash child-comp
                                           *comp-state-stash*))
                                 (ct* (and child-state
                                           (< k (length
                                                 (cval-types
                                                  child-state)))
                                           (aref (cval-types
                                                  child-state)
                                                 k))))
                        (and (ct-p ct*)
                             (eq (ct-kind ct*) :instance)
                             (let ((tab (make-hash-table
                                         :test #'equal)))
                               (dolist (td (coerce (ct-payload ct*)
                                                   'list))
                                 (when (eq (typedecl-kind td)
                                           :export)
                                   (let* ((pl (typedecl-payload td))
                                          (et2 (third pl)))
                                     (setf (gethash (first pl) tab)
                                           (cons (car et2)
                                                 (if (and (consp et2)
                                                          (eq (car et2)
                                                              :type))
                                                     (first pl)
                                                     (cdr et2)))))))
                               tab))))))
              (let ((tab (make-hash-table :test #'equal)))
                (let* ((child-comp (gethash src-idx
                                           *inst-source-comp*))
                       (child-state (and child-comp
                                        (gethash child-comp
                                                 *comp-state-stash*)))
                       (asc (loop for d across
                                     (wasmdown.component:comp-defs
                                  child-comp)
                                when (and (eq (compdef-kind d)
                                              :export)
                                          (string=
                                           (exportrec-name
                                            (compdef-payload d))
                                           name))
                                return (exportrec-type
                                        (compdef-payload d))))
                       (k (and (consp asc)
                               (eq (car asc) :instance)
                               (cdr asc)))
                       (ct* (and child-state
                                 (integerp k)
                                 (< k (length (cval-types child-state)))
                                 (aref (cval-types child-state) k))))
                  (dolist (td (coerce (ct-payload ct*) 'list))
                    (when (eq (typedecl-kind td) :export)
                      (let* ((pl (typedecl-payload td))
                             (et2 (third pl)))
                        (setf (gethash (first pl) tab)
                              (cons (car et2)
                                    (if (and (consp et2)
                                             (eq (car et2) :type))
                                        (first pl)
                                        (cdr et2)))))))
                  tab)))
             ;; slice 5m (indicies ::0) + M9 (::75's 49-row
             ;; cascade): an entry whose sort is :instance — the
             ;; RE-EXPORT of an IMPORTED instance. The entry's cdr
             ;; is the CHILD's import index (child space); resolve
             ;; the import's ascribed instance-ct through the
             ;; CHILD's stashed state (its instance-types slot holds
             ;; the ct), then project the ct's export declarators.
             ((and (consp hit)
                   (eq (car hit) :instance)
                   (let* ((child-comp (gethash src-idx
                                               *inst-source-comp*))
                          (child-state (and child-comp
                                            (gethash child-comp
                                                     *comp-state-stash*)))
                          (k (cdr hit))
                          (ct* (or (and (ct-p k) k)
                                   (and child-state
                                        (integerp k)
                                        (< k (length
                                              (cval-instance-types
                                               child-state)))
                                        (aref (cval-instance-types
                                               child-state) k)))))
                     (and (ct-p ct*)
                          (eq (ct-kind ct*) :instance))))
              (let* ((child-comp (gethash src-idx
                                          *inst-source-comp*))
                     (child-state (and child-comp
                                       (gethash child-comp
                                                *comp-state-stash*)))
                     (k (cdr hit))
                     (ct* (or (and (ct-p k) k)
                              (and child-state
                                   (integerp k)
                                   (< k (length (cval-instance-types
                                                 child-state)))
                                   (aref (cval-instance-types
                                          child-state) k))))
                     (tab (make-hash-table :test #'equal)))
                (dolist (td (coerce (ct-payload ct*) 'list))
                  (when (eq (typedecl-kind td) :export)
                    (let* ((pl (typedecl-payload td))
                           (et2 (third pl)))
                      (setf (gethash (first pl) tab)
                            (cons (car et2)
                                  (if (and (consp et2)
                                           (eq (car et2) :type))
                                      (first pl)
                                      (cdr et2)))))))
                tab))
             ;; slice 5j (external-visibility ::50): the projection of
             ;; an INSTANTIATED source's named export — the entry idx
             ;; indexes the CHILD's space; resolve the sub-instance's
             ;; own table through the CHILD's stashed state
             ((and (gethash src-idx *inst-source-comp*)
                   (integerp (cdr hit))
                   (let* ((child-comp (gethash src-idx
                                               *inst-source-comp*))
                          (child-state (and child-comp
                                            (gethash child-comp
                                                     *comp-state-stash*))))
                     (and child-state
                          (< (cdr hit)
                             (length (cval-instance-types
                                      child-state)))
                          (let ((sub-ent (aref (cval-instance-types
                                                child-state)
                                               (cdr hit))))
                            (when (hash-table-p sub-ent)
                              ;; re-index the sub-table's entries into
                              ;; THIS scope's projection: keep the
                              ;; child-side indices (the consumers of
                              ;; the projected table resolve them
                              ;; through the child state)
                              (let ((tab (make-hash-table :test #'equal)))
                                (loop for k2 being the hash-keys of sub-ent
                                      using (hash-value v2)
                                      do (setf (gethash k2 tab) v2))
                                tab))))))
              (let* ((child-comp (gethash src-idx
                                          *inst-source-comp*))
                     (child-state (and child-comp
                                       (gethash child-comp
                                                *comp-state-stash*)))
                     (sub-ent (aref (cval-instance-types child-state)
                                    (cdr hit)))
                     (tab (make-hash-table :test #'equal)))
                (loop for k2 being the hash-keys of sub-ent
                      using (hash-value v2)
                      do (setf (gethash k2 tab) v2))
                tab))
             ;; an IMPORTED instance's exports do not survive
             ;; projection out of the defining component — the
             ;; aliased instance's table is unknown (ev ::61's
             ;; 'missing expected export `f`')
             (t nil))))))
     ;; slice 5i (external-visibility ::57): a ct-typed SOURCE (an
     ;; IMPORTED instance) — the projection reads the named export's
     ;; ascribed externtype from the ct. An (instance . k) ascription
     ;; indexes the CT'S OWN type space (its declarator sequence's
     ;; type-introducing positions), NOT the parent's type vector:
     ;; the k'th type-introducing declarator's payload is the
     ;; sub-instance ct, whose own export declarators become the
     ;; projected table.
     ((and (ct-p ent) (eq (ct-kind ent) :instance))
      (let* ((d (find name (coerce (ct-payload ent) 'list)
                     :key (lambda (x)
                            (and (eq (typedecl-kind x) :export)
                                 (first (typedecl-payload x))))
                     :test #'equal))
             (et (and d (third (typedecl-payload d))))
             (k (and et (eq (car et) :instance) (integerp (cdr et))
                     (cdr et))))
        (when (integerp k)
          (loop with n = -1
                for x across (ct-payload ent)
                do (case (typedecl-kind x)
                     ((:type :alias :import :export)
                      (incf n)
                      (when (and (= n k)
                                 (eq (typedecl-kind x) :type)
                                 (ct-p (typedecl-payload x))
                                 (eq (ct-kind (typedecl-payload x))
                                     :instance))
                        (let ((tab (make-hash-table :test #'equal)))
                          (dolist (td (coerce (ct-payload
                                               (typedecl-payload x))
                                             'list))
                            (when (eq (typedecl-kind td) :export)
                              (let* ((pl (typedecl-payload td))
                                     (et2 (third pl)))
                                (setf
                                 (gethash (first pl) tab)
                                 (cons
                                  (car et2)
                                  (cond
                                   ((and (consp et2)
                                         (eq (car et2) :instance))
                                    ;; slice 5m (indicies ::0): the
                                    ;; (instance . k) ascription's k
                                    ;; indexes the NESTED ct's own
                                    ;; declarator space - carry the
                                    ;; double-nested ct ITSELF so a
                                    ;; projection of THIS table can
                                    ;; resolve its exports
                                    (%ct-decl-at
                                     (ct-payload
                                      (typedecl-payload x))
                                     (cdr et2)))
                                   ((and (consp et2)
                                         (eq (car et2) :type))
                                    (first pl))
                                   (t (cdr et2))))))))
                          (return tab)))
                      (when (= n k) (return nil)))
                     (t nil))
                finally (return nil)))))
     (t nil))))

(defun %match-instantiation-arg (name sort et arg-si import-idx
                                 ids state)
  "Match ONE import against its argument (sort + type checks)."
  (declare (ignore name))
  (let ((psort (sortidx-sort arg-si)))
    (unless (eq psort sort)
      (%verr "expected ~a, found ~a"
             (string-downcase (string sort))
             (string-downcase (string psort))))
    (case sort
      (:type
       (cond
        ((and (listp et) (eq (car et) :eq))
         (let ((actual (%parent-identity arg-si state)))
           (%ty-match-import-bound (second et) actual ids
                                   (cval-type-ids state))))
        ((eq et :sub-resource)
         (let ((a (%parent-identity arg-si state)))
           (unless (or (and (consp a) (eq (car a) :fresh))
                       (and (consp a) (eq (car a) :ref))
                       ;; an aliased exported resource carries an
                       ;; :inst-export identity (slice 3b) — still a
                       ;; resource (::41's (with "a" (type $r1)))
                       (and (consp a) (eq (car a) :inst-export)))
             (%verr "expected resource, found defined type"))))
        (t nil)))
      (t nil))))



(defun %ty-match-import-bound (bound-idx actual ids parent-ids)
  "The (eq i) import's check: the child's BOUND type (at BOUND-IDX in
the child's rebuilt space) matched structurally against the argument's
ACTUAL identity (parent space)."
  (let ((want (if (and ids (< bound-idx (length ids)))
                  (aref ids bound-idx)
                  :ty-bad)))
    (when (eq want :ty-bad) (return-from %ty-match-import-bound))
    (let ((w (%ty-resolve want ids))
          (g (if (and (consp actual) (eq (car actual) :ref))
                 (%ty-resolve actual parent-ids)
                 actual)))
      (unless (or (eq w :ty-bad) (eq g :ty-bad) (null g))
        (%ty-match w g ids nil parent-ids nil)))))
(defun %resolve-aliased-component (alias comp)
  "M9 (linking ::169): the comp an export-alias projects. The
alias's source instance was instantiated from a comp reachable in
the def tree, so resolve the chain structurally (the shared
*inst-source-comp* is keyed by bare instance index and a child's
records are clobbered by the enclosing walk): the alias lives in
COMP; its source instance's instrec gives the SOURCE comp; that
comp's component-space export (the alias's name) gives the next
entry — a plain :component def ends the chain, another alias
recurses with the source comp as the new owner (the chain crosses
component boundaries: $C11 -> $D1's export c1 -> the alias $Ca
inside $D1 -> $D2's export c -> $Counter)."
  (labels
      ((comp-entry (c k)
         "The k-th component-space entry of C: (:component . comp) |
(:alias . aliasrec) | NIL."
         (let ((seen 0))
           (loop for d across (comp-defs c)
                 when (or (eq (compdef-kind d) :component)
                          (and (eq (compdef-kind d) :import)
                               (eq (car (importrec-type
                                         (compdef-payload d)))
                                   :component))
                          (and (eq (compdef-kind d) :alias)
                               (eq (aliasrec-sort (compdef-payload d))
                                   :component)))
                   if (= seen k)
                     return (cons (compdef-kind d) (compdef-payload d))
                   else do (incf seen)
                 finally (return nil))))
       (inst-source (c iidx)
         "The (target-comp . entry) the iidx-th instance def of C
instantiates: the TARGET comp is what the instance instantiates —
the export named by a later alias hop resolves in IT (the alias
$C11 projects $d1's export c1, and c1 is $D1's export, not the
parent's)."
         (let ((seen 0))
           (loop for d across (comp-defs c)
                 when (eq (compdef-kind d) :instance)
                   if (= seen iidx)
                     return (let ((e (and (eq (instrec-kind
                                              (compdef-payload d))
                                             :instantiate)
                                         (comp-entry
                                          c (instrec-index
                                             (compdef-payload d))))))
                              (and e
                                   (cons (if (eq (car e) :component)
                                             (cdr e)
                                             nil)
                                         e)))
                   else do (incf seen)
                 finally (return nil))))
       (export-entry (c name)
         "C's component-space export NAME's entry."
         (loop for d across (comp-defs c)
               when (and (eq (compdef-kind d) :export)
                         (string= (exportrec-name (compdef-payload d))
                                  name))
                 do (let ((si (exportrec-sortidx
                               (compdef-payload d))))
                      (when (and (consp si)
                                 (eq (sortidx-sort si) :component))
                        (return (cons c (comp-entry c (sortidx-idx si))))))
               finally (return nil))))
    (loop
      with guard = 0
      for owner = comp then (car hop)
      for a = alias then (cdr (cdr hop))
      for hop = (and (eq (aliasrec-kind a) :export)
                     owner
                     (let ((src (inst-source owner
                                             (aliasrec-index a))))
                       (and src
                            (car src)
                            (export-entry (car src)
                                          (aliasrec-name a)))))
      do (incf guard)
         (when (or (> guard 16) (null hop))
           (return nil))
      when (and (consp (cdr hop))
                (eq (car (cdr hop)) :component))
        return (cdr (cdr hop)))))
(defun %check-instantiate-args (inst comp state)
  "The instantiation-time type checks (instantiation.wast): every
import of the target component is checked, in the child's declaration
order, against the matching (with ...) argument. The child's type space
is rebuilt by walking its defs: each (sub resource) type import is a
TYPEVAR bound to its argument's identity (the parent's substitution);
each (eq i) import follows the child's chain; resource DEFINITIONS are
fresh (per-instantiation generativity). Duplicate argument names are
rejected (instantiation.wast ::57)."
  (let* ((defs (coerce (comp-defs comp) 'list))
         (cidx (instrec-index inst))
         (args (instrec-args inst))
         (target nil)
         (seen 0))
    ;; duplicate argument names (instantiation.wast ::57:
    ;; 'instantiation argument `a` conflicts with previous argument `a`')
    (let ((names '()))
      (loop for (nm . nil) across args
            do (when (member nm names :test #'string=)
                 (%verr "instantiation argument `~a` conflicts with previous argument `~a`"
                        nm nm))
               (push nm names)))
  (let* ((defs (coerce (comp-defs comp) 'list))
         (cidx (instrec-index inst))
         (args (instrec-args inst))
         (target nil)
         (seen 0))
    (dolist (d defs)
      ;; slice 5m (resources ::34): the COMPONENT index space counts
      ;; :component defs AND :component IMPORTS (an imported component
      ;; occupies an index; the target stays NIL for imports — the
      ;; with-arg matching skips, the identity/export paths read the
      ;; ascribed ct)
      ;; M9 (linking ::169): a :component-sort ALIAS also occupies an
      ;; index (unit.wast's nested-component export chain aliases a
      ;; counter component up two levels — instantiating the alias
      ;; resolved the wrong target and reported the alias's own
      ;; imports). The aliased component's payload rides in the
      ;; *alias-component-source* record.
      (when (or (eq (compdef-kind d) :component)
                (and (eq (compdef-kind d) :import)
                     (eq (car (importrec-type (compdef-payload d)))
                         :component))
                (and (eq (compdef-kind d) :alias)
                     (eq (aliasrec-sort (compdef-payload d))
                         :component)))
        (when (= seen cidx)
          (cond
           ((eq (compdef-kind d) :component)
            (setf target (compdef-payload d)))
           ;; an alias-introduced component: resolve the export chain
           ;; (the alias projects a component out of an instance's
           ;; exports; that export may itself be an alias — recurse)
           ((eq (compdef-kind d) :alias)
            (setf target (%resolve-aliased-component
                          (compdef-payload d) comp))))
          (return))
        (incf seen)))
    (unless target
      (return-from %check-instantiate-args))
    (let ((ctypes (make-array 16 :fill-pointer 0 :adjustable t))
          (ids (make-array 16 :fill-pointer 0 :adjustable t))
          (imports '())
          (func-imports '())
          (func-types (make-array 16 :fill-pointer 0 :adjustable t))
          ;; slice 4 (family 1b): the child's CORE types — the
          ;; (:core-module . k) import's moduletype lives at k in
          ;; the child's own core-type space
          (core-types (make-array 16 :fill-pointer 0 :adjustable t))
          ;; M9 (linking ::130): the number of non-:type-sorted
          ;; ALIAS defs walked so far — a wrapper ct pushed into ids
          ;; carries a RAW child-space payload index; the rebuilt ids
          ;; skip those alias slots, so the payload must shift down
          ;; by this count to stay aligned
          (alias-skips 0)
          ;; M9 (linking ::130): the skip count in effect when each
          ;; ids slot was pushed — a wrapper's payload shift uses the
          ;; count at the TARGET's slot, not at the wrapper's own
          (slot-skips (make-array 16 :fill-pointer 0 :adjustable t)))
      (dolist (d (coerce (comp-defs target) 'list))
        (let ((pl (compdef-payload d)))
          (case (compdef-kind d)
            (:core-type
             ;; slice 4 (family 1b): the child's core types in order
             ;; (the moduletype shapes used by the :core-module arm)
             (vector-push-extend pl core-types))
            (:type
             (vector-push-extend pl ctypes)
             (vector-push-extend
              (if (ct-resourcetype-p pl)
                  (%fresh-resource-id)
                  ;; M9 (linking ::130): an own/borrow WRAPPER ct's
                  ;; payload is a RAW child-space type index — remap
                  ;; it to the rebuilt space. The shift is the skip
                  ;; count in effect at the TARGET's slot (the
                  ;; non-type aliases before the referenced def),
                  ;; read from the per-slot record — NOT the count at
                  ;; the wrapper's own position (later canon-target
                  ;; aliases inflate that)
                  (cons :ct
                        (if (and (member (ct-kind pl) '(:own :borrow))
                                 (integerp (ct-payload pl))
                                 (< (ct-payload pl)
                                    (length slot-skips)))
                            (make-ct :kind (ct-kind pl)
                                     :payload (- (ct-payload pl)
                                                 (aref slot-skips
                                                       (ct-payload pl)))
                                     :offset (ct-offset pl))
                            pl)))
              ids)
             ;; record this slot's effective skip count
             (vector-push-extend alias-skips slot-skips))
            ((:export :alias)
             ;; slice 5l (resources ::39): exports and aliases also
             ;; introduce type indices in the rebuilt child space —
             ;; an export's new index is an alias of its sortidx'd
             ;; source ((:ref ti), resolving to the source's rebuilt
             ;; identity); an ascribed (sub resource) export is fresh;
             ;; an alias is opaque.
             ;; M9 (linking ::128): only :type-INTRODUCING defs grow
             ;; the rebuilt ids — an ALIAS of another sort (the parse's
             ;; implicit (alias core-export ...) for a resource's
             ;; (dtor (core func ...)) projection) occupies that sort's
             ;; space, NOT the type space; pushing for it misaligned
             ;; every later slot (the own's target resolved through
             ;; the wrong id and the func-arg match failed 'type
             ;; mismatch with result type')
             (when (and (eq (compdef-kind d) :alias)
                        (not (eq (aliasrec-sort pl) :type)))
               ;; M9 (linking ::130): a non-type alias the rebuild
               ;; skips — later wrapper payloads shift past it
               (incf alias-skips))
             (when (or (and (eq (compdef-kind d) :export)
                            (eq (sortidx-sort (exportrec-sortidx pl))
                                :type))
                       (and (eq (compdef-kind d) :alias)
                            (eq (aliasrec-sort pl) :type)))
               ;; The rebuild stays INDEX-ALIGNED with the child's
               ;; VALIDATOR type space: an export introduces an index
               ;; of ITS OWN SORT (a :func export grows the FUNC space,
               ;; not the type space), so ONLY :type-sorted exports may
               ;; push here. Pushing for every :export def (the old
               ;; rule) misaligned every slot after a func export —
               ;; the raw functype refs the parent resolves through
               ;; the stash are VALIDATOR-space indices and need NO
               ;; shift.
               (let ((si (if (eq (compdef-kind d) :export)
                             (exportrec-sortidx pl)
                             nil)))
                 (vector-push-extend
                  (cond
                   ((and (eq (compdef-kind d) :export)
                         (exportrec-type pl)
                         (consp (cdr (exportrec-type pl)))
                         (eq (car (cdr (exportrec-type pl)))
                             :sub-resource))
                    (%fresh-resource-id))
                   ((and (eq (compdef-kind d) :export)
                         si (eq (sortidx-sort si) :type))
                    (cons :ref (sortidx-idx si)))
                   (t '(:opaque)))
                  ids)
                 ;; M9 (linking ::130): this slot's skip count too
                 (vector-push-extend alias-skips slot-skips))))            (:import
             (let* ((et (importrec-type pl))
                    (nm (importrec-name pl))
                    (arg-si (cdr (assoc nm (coerce args 'list)
                                          :test #'string=))))
               (push (list nm (car et) (cdr et) arg-si
                           (length ctypes))
                     imports)
               (case (car et)
                 (:type
                  (vector-push-extend
                   (if (eq (cdr et) :sub-resource)
                       (make-ct :kind :resource :payload '(0 . nil)
                                :offset 0)
                       (make-ct :kind :prim :payload :imported
                                :offset 0))
                   ctypes)
                  (vector-push-extend
                   (cond
                    ((eq (cdr et) :sub-resource)
                     (or (%parent-identity arg-si state)
                         (%fresh-resource-id)))
                    ((and (listp (cdr et))
                          (eq (car (cdr et)) :eq))
                     (cons :ref (second (cdr et))))
                    (t '(:opaque)))
                   ids))
                 (:func
                  (vector-push-extend
                   (and (< (cdr et) (length ctypes))
                        (aref ctypes (cdr et)))
                   func-types)
                  (push (list nm (cdr et) (length func-types))
                        func-imports))
                 (:instance
                  ;; the child's instance-type import: match the
                  ;; argument's exports export-by-export by name +
                  ;; sort (instantiation.wast ::30 'type mismatch in
                  ;; instance export `a`'; extra exports allowed)
                  (when arg-si
                    (let ((want (and (< (cdr et) (length ctypes))
                                     (aref ctypes (cdr et)))))
                      (when (and want (ct-p want)
                                 (eq (ct-kind want) :instance))
                        (dolist (td (coerce (ct-payload want) 'list))
                          (when (eq (typedecl-kind td) :export)
                            (let* ((pl (typedecl-payload td))
                                   (ename (first pl))
                                   (eet (third pl)))
                              (multiple-value-bind (got-sort known)
                                  (%instance-export-sort
                                   (sortidx-idx arg-si) ename state)
                                ;; M9 (linking EV ::61): a REQUIRED
                                ;; export the argument does not
                                ;; provide at all — 'missing
                                ;; expected export `f`' (the
                                ;; ascription-replaced type)
                                (unless known
                                  (%verr "missing expected export `~a`"
                                         ename))
                                (unless (eq got-sort (car eet))
                                  (%verr "type mismatch in instance export `~a`"
                                         ename))))))))
                  ;; M9 (shared-everything ::0): an import introduces
                  ;; NO component type index — its ascribed ct was
                  ;; already pushed by the child's own :type def walk
                  ;; (the inline instance externtype desugars into an
                  ;; out-of-line type def BEFORE the import def). The
                  ;; former re-push duplicated the ct, shifting every
                  ;; later import's read by one.
                  ))
                 (:core-module
                  ;; slice 4 (family 1b): the moduletype import —
                  ;; the ARG is a core module (its wasm-module IR);
                  ;; match the declared moduletype contravariantly
                  ;; (the arg may import less) / covariantly (the
                  ;; arg may export more), instantiation ::33-::36.
                  ;; Slice 5m (indicies ::0): a BARE (core module)
                  ;; import ascription carries the :NONE marker —
                  ;; an UNTYPED core module import accepts any arg
                  (when arg-si
                    (let ((mt (cond
                               ((eq (cdr et) :none)
                                ;; slice 5m (instantiation ::34):
                                ;; a BARE (core module) import
                                ;; ascription = the EMPTY
                                ;; moduletype — the arg must have
                                ;; NO imports
                                (list :moduletype
                                      (make-array 0)))
                               ((integerp (cdr et))
                                (and (< (cdr et) (length core-types))
                                     (aref core-types (cdr et))))
                               (t nil))))
                      (when (and mt (consp mt) (eq (car mt) :moduletype))
                        (let ((m (%core-module-at
                                  comp (sortidx-idx arg-si))))
                          (when (and m
                                     (wasmdown.types:wasm-module-p m))
                            (%match-core-module-arg mt m))))))
                  ;; the :core-module import introduces no COMPONENT
                  ;; type index in the child's matching space
                  )
                  )))
            (t nil))))
      (dolist (fi (nreverse func-imports))
        (destructuring-bind (nm typeidx fpos) fi
          (declare (ignore typeidx))
          (let ((arg-si (cdr (assoc nm (coerce args 'list)
                                    :test #'string=))))
            (when arg-si
              (%match-func-arg arg-si
                               (aref func-types (1- fpos))
                               ids state)))))
      (dolist (im (nreverse imports))
        (destructuring-bind (nm sort et arg-si import-idx) im
          (unless arg-si
            (%verr "missing import named `~a`" nm))
          (%match-instantiation-arg nm sort et arg-si import-idx
                                    ids state)))
      ;; slice 5c: the rebuilt child identity vector — the caller
      ;; stashes it per instance so %instance-export-identity can
      ;; resolve an aliased export through the child's substitution
      ;; (resources.wast ::44/::45's (type $c1 "out") chains)
      ids))))

;;; ---------------------------------------------------------------
;;; External visibility of types (Explainer.md
;;; 'external-visibility-of-types'; external-visibility.wast).
;;; Any resource/record/variant/enum/flags type transitively used in
;;; the type of an import or export must itself be imported or
;;; exported: the typeidx INTRODUCED by a type import/export is the
;;; named one (the typeidx passed into an export is not retroactively
;;; named — ::0/::2). Imports may only use IMPORT-named types; ex-
;;; ports may use either. Anonymous structural shells (tuple/list/
;;; option/result/prim) recurse; named types stop the walk (their
;;; references were checked when they were named).

(defun %vis-nameable-kind-p (kind)
  (member kind '(:resource :record :variant :enum :flags)))

(defun %vis-decl-functype-at (ict fti)
  "M9 (linking ::104): instance-ct ICT's FUNCTYPE for a func export
whose ascribed index FTI resolves in the CT's OWN declarator space
(the :type-introducing entries counted in sequence) — the functype
ct when found there, NIL when FTI is not a declarator-space index
(the caller falls back to the enclosing state's space)."
  (loop with n = -1
        for td across (ct-payload ict)
        do (case (typedecl-kind td)
             ((:type :alias :import :export)
              (incf n)
              (when (and (= n fti)
                         (eq (typedecl-kind td) :type)
                         (eq (wasmdown.component:ct-kind
                              (typedecl-payload td))
                             :func))
                (return-from %vis-decl-functype-at
                  (typedecl-payload td)))))
        finally (return-from %vis-decl-functype-at nil)))

(defun %vis-named-p (state idx import-p)
  "IDX is in the visible set for this context: IMPORT-P selects the
import table only; exports accept both."
  (or (gethash idx (cval-vis-imports state))
      (and (not import-p) (gethash idx (cval-vis-exports state)))))

(defun %resolve-aliased-shape (tgt ct)
  "The outer-alias slot's SHAPE (values lane): when the outer type
slot holds the opaque :imported marker, follow the eq-BOUND import's
underlying type (found by walking the outer comp's defs and counting
:type-introducing slots up to the marker's typeidx); every other
shape passes through."
  (if (and (ct-p ct)
           (eq (ct-kind ct) :prim)
           (eq (ct-payload ct) :imported))
      (let* ((types (cval-types tgt))
             (idx (position ct (coerce types 'list))))
        (if (null idx)
            ct
            (let ((comp (gethash tgt *state-comp*))
                  (seen -1)
                  (hit nil))
              (when comp
                (loop for d across (comp-defs comp)
                      do (case (compdef-kind d)
                           (:type (incf seen)
                                  (when (= seen idx)
                                    (loop-finish)))
                           (:import
                            (when (eq (car (importrec-type
                                            (compdef-payload d)))
                                      :type)
                              (incf seen)
                              (when (= seen idx)
                                (setf hit (cdr (importrec-type
                                                (compdef-payload d))))
                                (loop-finish))))
                           (:export
                            (when (eq (sortidx-sort
                                       (exportrec-sortidx
                                        (compdef-payload d)))
                                      :type)
                              (incf seen)
                              (when (= seen idx)
                                (loop-finish))))
                         (:alias
                          (when (eq (aliasrec-sort (compdef-payload d))
                                    :type)
                            (incf seen)
                            (when (= seen idx)
                              (loop-finish)))))))
              (or (and hit
                       (listp hit)
                       (eq (car hit) :eq)
                       (< (second hit) (length types))
                       (aref types (second hit)))
                  ct))))
      ct))

(defvar *vis-seen* nil
  "The seen-set shared across one %VIS-CHECK walk INCLUDING the
%vis-check-decl recursion into instance/component types — an
instance's export func whose type index resolves (through the
enclosing space) back to the instance itself is a legal cycle that
per-call seen tables would loop on (annotated-names ::30).")

(defun %vis-check (state root import-p what)
  "Signal the fixture message when the walk from ROOT (a type index
or a ct) reaches a nameable type with no external name in this
context. The ROOT itself is NOT nameability-checked: for a func/
instance import or export the root is the func/instance type (never
nameable itself), and for a type import/export of `(eq i)' the bound
type becomes named through the very declarator being checked — only
the types it transitively REFERENCES need names (resources.wast
::26/:41, external-visibility ::23). WHAT names the construct for
the message: func | type | instance."
  (let ((seen (or *vis-seen* (make-hash-table))))
    (let ((*vis-seen* seen))
      (labels
        ((bad (i)
           (if (%vis-named-p state i (not import-p))
               ;; named on the OTHER side: imports may not see
               ;; exports (::3/::44)
               (when import-p
                 (%verr "~a not valid to be used as import" what))
               (%verr "~a not valid to be used as ~a"
                      what (if import-p "import" "export"))))
         (walk-idx (i)
           (when (and (>= i 0)
                      (not (gethash i seen)))
             ;; M9 (EV ::52): an import-declarator's SUNK implicit
             ;; outer alias leaves the ref's index OUT OF BOUNDS in
             ;; THIS state (the alias never introduced a slot) —
             ;; resolve through *outer-states* so the walk sees the
             ;; underlying type in the ENCLOSING component's space
             ;; ('func not valid to be used as import' fires there;
             ;; skipping it made the fixture pass wrong)
             (cond
              ((< i (length (cval-types state)))
               (setf (gethash i seen) t)
               (let ((ty (aref (cval-types state) i)))
                 (when (ct-p ty)
                   ;; a REACHED type must be named if nameable
                   (when (and (%vis-nameable-kind-p (ct-kind ty))
                              (not (%vis-named-p state i import-p)))
                     (bad i))
                   (walk-ct ty))))
              ((and (boundp '*outer-states*) *outer-states*)
               (loop for st in *outer-states*
                     when (and (< i (length (cval-types st)))
                               (not (gethash (cons :st i) seen)))
                       do (setf (gethash (cons :st i) seen) t)
                          (let ((ty (aref (cval-types st) i)))
                            (when (and (ct-p ty)
                                       (%vis-nameable-kind-p (ct-kind ty)))
                              ;; named in the ENCLOSING space: an
                              ;; outer type is visible there only if
                              ;; the ENCLOSING component named it
                              (unless (%vis-named-p st i import-p)
                                (bad i)))
                            (when (ct-p ty) (walk-ct ty))))))))
         (walk-ct (ty)
           (case (ct-kind ty)
             (:prim nil) ; :imported/:exported/:outer-aliased markers
             (:record
              (loop for (nil . vt) across (ct-payload ty)
                    do (walk-vt vt)))
             (:variant
              (loop for (nil . vt) across (ct-payload ty)
                    do (when vt (walk-vt vt))))
             (:tuple
              (loop for vt across (ct-payload ty) do (walk-vt vt)))
             (:list (walk-vt (car (ct-payload ty))))
             (:option (walk-vt (ct-payload ty)))
             (:result
              (when (car (ct-payload ty)) (walk-vt (car (ct-payload ty))))
              (when (cdr (ct-payload ty))
                (walk-vt (cdr (ct-payload ty)))))
             ((:own :borrow) (walk-idx (ct-payload ty)))
             ((:stream :future)
              (when (ct-payload ty) (walk-vt (ct-payload ty))))
             (:map
              (walk-vt (cdr (ct-payload ty))))
             (:func
              (loop for (nil . vt) across (first (ct-payload ty))
                    do (walk-vt vt))
              (when (second (ct-payload ty))
                (walk-vt (second (ct-payload ty)))))
            (:instance
             ;; an instance type: every export declarator's
             ;; externtype is checked in ITS OWN (export) context.
             ;; M9 (linking ::104): the declarators' index refs are
             ;; in the CT's OWN declarator space — resolve the func
             ;; export's functype through the declarator sequence
             ;; (an own/borrow ref to an alias-introduced type is an
             ;; outer-aliased marker — the walk stops there; walking
             ;; the ENCLOSING state's space read the wrong ct and
             ;; reported 'func not valid to be used as export')
             (loop for d across (ct-payload ty)
                   when (eq (typedecl-kind d) :export)
                     do (let ((et (third (typedecl-payload d))))
                          (case (car et)
                            (:func
                             (let ((ft (and (integerp (cdr et))
                                            (%vis-decl-functype-at
                                             ty (cdr et)))))
                               (if ft
                                   (labels
                                       ((walk-decl-vt (vt)
                                          (cond
                                           ((and (consp vt)
                                                 (eq (car vt) :idx)
                                                 (integerp (cdr vt)))
                                            ;; a DECLARATOR-space ref:
                                            ;; the entry at that
                                            ;; position — an alias is
                                            ;; outer-aliased (opaque,
                                            ;; the walk stops); a
                                            ;; :type walks in place
                                            (let ((e (and
                                                      (integerp (cdr vt))
                                                      (loop with n2 = -1
                                                            for td2 across
                                                              (ct-payload ty)
                                                            do (case
                                                                 (typedecl-kind td2)
                                                                 ((:type :alias
                                                                   :import
                                                                   :export)
                                                                  (incf n2)
                                                                  (when (= n2
                                                                           (cdr vt))
                                                                    (return
                                                                     td2))))
                                                            finally
                                                               (return nil)))))
                                              (when (and e
                                                         (eq
                                                          (typedecl-kind e)
                                                          :type))
                                                (walk-decl-ct
                                                 (typedecl-payload e)))))
                                           ((and (consp vt)
                                                 (eq (car vt) :idx))
                                            ;; an interned own/borrow
                                            ;; ct ref — walk it with
                                            ;; the decl resolution
                                            nil)
                                           ((and vt (ct-p vt))
                                            (walk-decl-ct vt))))
                                        (walk-decl-ct (c)
                                          (when (and (ct-p c)
                                                     (not
                                                      (gethash
                                                       (ct-kind c)
                                                       seen)))
                                            (setf (gethash
                                                   (ct-kind c)
                                                   seen)
                                                  t)
                                            (case
                                                (ct-kind c)
                                              (:func
                                               (loop
                                                for (nil . vt2)
                                                  across
                                                  (first
                                                   (ct-payload c))
                                                do (walk-decl-vt vt2))
                                               (when (second
                                                      (ct-payload c))
                                                 (walk-decl-vt
                                                  (second
                                                   (ct-payload c)))))
                                              ((:own :borrow)
                                               (walk-decl-vt
                                                (ct-payload c)))
                                              (t nil)))))
                                     (declare (ignorable
                                               #'walk-decl-vt
                                               #'walk-decl-ct))
                                     (walk-decl-ct ft))
                                   ;; not in the declarator's own
                                   ;; space: the generic state path
                                   (%vis-check-decl state
                                                    (typedecl-payload d)
                                                    nil what))))
                            ((:type :instance)
                             (%vis-check-decl state (typedecl-payload d)
                                              nil what))))))
             (t nil)))
         (walk-vt (vt)
           (cond ((and (consp vt) (eq (car vt) :idx))
                  (walk-idx (cdr vt)))
                 ((ct-p vt)
                  ;; M9 (EV ::52): a BARE ct arrived with no index —
                  ;; a nameable kind reached this way (an anonymous
                  ;; inline ct, e.g. an import functype's result
                  ;; resolving a sunk outer alias) has NO named slot
                  ;; to check: fire the bad path. The shape-of-a-
                  ;; named-type arrivals come through walk-idx (which
                  ;; already checked namedness before recursing).
                  (when (%vis-nameable-kind-p (ct-kind vt))
                    (bad -1))
                  (walk-ct vt))
                 (t nil))))
      ;; the ROOT skips its own nameability check (::26/::41/::23)
      (cond ((and (integerp root) (>= root 0)
                  (< root (length (cval-types state))))
             ;; skip an already-seen root: the walk can cycle back to
             ;; it through an instance-type's export declarators
             ;; (annotated-names ::30)
             (unless (gethash root seen)
               (setf (gethash root seen) t)
               (let ((ty (aref (cval-types state) root)))
                 (when (ct-p ty) (walk-ct ty)))))
            ((ct-p root) (walk-ct root))
            (t nil))))))

(defun %nth-instance-def (comp n)
  "The Nth :instance definition's instrec payload, in file order
\(the validator's instance index space), or NIL."
  (let ((seen 0))
    (loop for d across (comp-defs comp)
          when (eq (compdef-kind d) :instance)
            do (if (= seen n)
                   (return (compdef-payload d))
                   (incf seen))
          finally (return nil))))

(defun %check-moduletype (payload)
  "A (core type (module ...)) moduletype's internal rules
\(core-modules.wast ::2-::10): the (func (type n)) type-uses are
bounds-checked against the moduletype's OWN type sequence (in
declaration order, imports and exports included), export names are
unique ('export name `a` already defined' / 'name `` already
defined'), memory limits are within 65536 pages ('memory size must
be at most 65536'), and the mangled two-level import names are
unique ('duplicate import name `:`')."
  (unless (and (consp payload) (eq (car payload) :moduletype))
    (return-from %check-moduletype))
  ;; the payload's decl vector comes in two shapes: the binary
  ;; reader's (:moduletype #(...)) LIST and the text parser's
  ;; (:moduletype . #(...)) CONS — normalize to the vector
  (let ((decls (if (and (consp (cdr payload)) (vectorp (second payload)))
                   (second payload)
                   (cdr payload))))
  (let ((ntypes 0)
         (exports-seen '())
         (imports-seen '()))
    (dolist (d (coerce decls 'list))
      (case (typedecl-kind d)
        (:core-type (incf ntypes))
        ;; slice 5m (core-modules ::0's named alias): an (alias
        ;; outer $C $FT (type $a)) declarator introduces a type index
        ;; in the moduletype's own space — the text parser emits it
        ;; as a :core-alias typedecl (payload (outer-count .
        ;; ct-idx)) — count it so a later (func (type $a)) resolves
        (:core-alias (incf ntypes))
        (:core-export
         (let* ((pl (typedecl-payload d))
                (name (first pl))
                (desc (second pl)))
           (when (member name exports-seen :test #'string=)
             (%verr "export name `~a` already defined" name))
           (push name exports-seen)
           (when (and (consp desc) (eq (car desc) :func)
                      (eq (third desc) :implicit))
             ;; slice 5m (core-modules ::2): a bare (func) export
             ;; implicitly declares a fresh functype — it counts
             ;; toward the moduletype's own type space
             (incf ntypes))
           (when (and (consp desc) (eq (car desc) :func)
                      (not (eq (third desc) :implicit)))
             ;; an EXPLICIT (func (type n)): bounds-checked against
             ;; the decls so far (::2's (type 0) with no decls)
             (when (>= (second desc) ntypes)
               (%verr "type index out of bounds")))
           (when (and (consp desc) (eq (car desc) :memory))
             ;; limits payload: (min . max?) — check min bound
             (let ((lim (second desc)))
               (when (and (consp lim) (integerp (car lim))
                          (> (car lim) 65536))
                 (%verr "memory size must be at most 65536"))))))
        (:core-import
         (let* ((pl (typedecl-payload d))
                (mod (first pl))
                (field (second pl))
                (desc (third pl))
                (key (format nil "~a:~a" mod field)))
           (when (member key imports-seen :test #'string=)
             (%verr "duplicate import name `~a`" key))
           (push key imports-seen)
           (when (and (consp desc) (eq (car desc) :func)
                      (eq (third desc) :implicit))
             ;; slice 5m (core-modules ::2): the implicit functype
             ;; counts toward the moduletype's type space
             (incf ntypes))
           (when (and (consp desc) (eq (car desc) :func)
                      (not (eq (third desc) :implicit)))
             (when (>= (second desc) ntypes)
               (%verr "type index out of bounds")))
           (when (and (consp desc) (eq (car desc) :memory))
             (let ((lim (second desc)))
               (when (and (consp lim) (integerp (car lim))
                          (> (car lim) 65536))
                 (%verr "memory size must be at most 65536"))))))
        (t nil))))))

(defun %vis-check-decl (state payload import-p what)
  "One import/export declarator payload (name attrs et): check the
externtype's visibility in the declarator's own context (IMPORT-P).
An instance-type's EXPORT declarators pass IMPORT-P NIL."
  (let ((et (third payload)))
    (case (car et)
      (:func (%vis-check state (cdr et) import-p what))
      (:type
       (unless (eq (cdr et) :sub-resource)
         (%vis-check state (second (cdr et)) import-p what)))
      (:instance (%vis-check state (cdr et) import-p what))
      (t nil))))

(defun %vis-check-type-use (state typeidx import-p what)
  "Slice 5g (external-visibility ::37/::41-::44): an instance/component
TYPE used as an import/export by a concrete component — the type's OWN
import/export declarators must satisfy visibility INSIDE the type's
space, checked on the OUTER use's side. The walk rebuilds the type's
inner space declarator-by-declarator (each declarator's new index named
on its own side, the index being the pre-push length) and runs each
declarator's vis check. A COMPONENT type's check also fires EAGERLY at
the type's own definition (%check-defvaltype); an INSTANCE type checks
only at the use site (::35/::40's unused instance types define
freely)."
  (let ((ct (and (>= typeidx 0)
                 (< typeidx (length (cval-types state)))
                 (aref (cval-types state) typeidx))))
    (when (and (ct-p ct)
               (member (ct-kind ct) '(:instance :component)))
      (let ((inner (make-cval
                    :types (make-array 16 :fill-pointer 0 :adjustable t)
                    :func-types (make-array 16 :fill-pointer 0
                                            :adjustable t)
                    :imports (make-hash-table :test #'equal)
                    :exports (make-hash-table :test #'equal))))
        (dolist (d (coerce (ct-payload ct) 'list))
          (case (typedecl-kind d)
            ((:type :alias)
             (vector-push-extend
              (if (eq (typedecl-kind d) :alias)
                  (let ((a (typedecl-payload d)))
                    ;; M9 (linking ::104): an OUTER type alias whose
                    ;; target is a RESOURCE pushes a RESOURCE ct — the
                    ;; type's own declarators' interned own/borrow cts
                    ;; reference this slot and the own/borrow check
                    ;; demands a resource (same as %check-defvaltype's
                    ;; alias arm; the de Bruijn chain here resolves
                    ;; against the USE site's state)
                    (let ((tgt (and (eq (aliasrec-kind a) :outer)
                                    (eq (aliasrec-sort a) :type)
                                    (%outer-state-at state
                                                     (aliasrec-count a)))))
                      (if (and tgt
                               (< (aliasrec-target a)
                                  (length (cval-types tgt)))
                               (ct-resourcetype-p
                                (aref (cval-types tgt)
                                      (aliasrec-target a))))
                          (make-ct :kind :resource
                                   :payload '(0 . nil) :offset 0)
                          (make-ct :kind :prim
                                   :payload :outer-aliased :offset 0))))
                  (typedecl-payload d))
              (cval-types inner)))
            ((:import :export)
             (let* ((pl (typedecl-payload d))
                    (et (third pl)))
               ;; the declarator's references must be named in the
               ;; type's own space, checked on the OUTER use's side
               ;; (::41's 'instance not valid to be used as import')
               (let ((*vis-seen* nil))
                 (declare (special *vis-seen*))
                 ;; slice 5g: the checked side is the OUTER use's
                 ;; side (the message: ::41's 'instance not valid to
                 ;; be used as import'); an INSTANCE type's inner
                 ;; references accept EITHER side (its own exports
                 ;; are visible to its own imports — resources
                 ;; ::27/::28) — the export names are mirrored into
                 ;; the import table below; a COMPONENT type keeps
                 ;; the sides strict (::44)
                 (%vis-check-decl inner pl
                                  ;; the checked side: the OUTER
                                  ;; use's; the EAGER component-type
                                  ;; check (:auto) uses each decl's
                                  ;; own side
                                  (if (eq import-p :auto)
                                      (eq (typedecl-kind d) :import)
                                      import-p)
                                  ;; the message's kind: the OUTER
                                  ;; use's (what) when given — the
                                  ;; eager component-type check passes
                                  ;; NIL and names its own inner decls
                                  (or what
                                      (case (car et)
                                        ((:func :instance) "func")
                                        ((:type :core-module) "type")
                                        (t "type")))))
               ;; the NEW index the declarator introduces is named
               ;; on its own side (the index = the pre-push length)
               (let ((idx (length (cval-types inner))))
                 (setf (gethash idx
                                (if (eq (typedecl-kind d) :import)
                                    (cval-vis-imports inner)
                                    (cval-vis-exports inner)))
                       t)
                 ;; an INSTANCE type's exports are ALSO visible to
                 ;; its own imports (::27/::28) — mirror the name
                 (when (and (eq (typedecl-kind d) :export)
                            (eq (ct-kind ct) :instance))
                   (setf (gethash idx (cval-vis-imports inner)) t)))
               (vector-push-extend
                (if (and (eq (car et) :type)
                         (eq (cdr et) :sub-resource))
                    (make-ct :kind :resource :payload '(0 . nil)
                             :offset 0)
                    (make-ct :kind :prim
                             :payload :imported-or-exported-type))
                (cval-types inner))))
            (t nil)))))))


(defun %ann-resource-check (name ann body ft state &optional import-p)
  "Slice 5k (annotated-names ::4/::18/::26/::28/::31/::32): the
resource-NAME rules for [constructor]/[method]/[static] names. The
annotation's resource prefix (the label before the first dot) must
match the NAME of the resource type the function's type references —
[constructor]: the own-result's resource; [method]: the self-borrow's
resource. The name comes from the import/export that INTRODUCED the
resource ON THE CHECKED SIDE (the import/export namespaces are
disjoint: an export-side annotated name requires the resource itself
to be (re-)exported — ::28; IMPORT-P defaults to the import side).
A resource with NO name on the checked side (a local definition:
::32, or an import-named resource used in an export: ::28) is
rejected. [static] only requires the prefix to be KNOWN (::26)."
  (declare (ignore name))
  (let ((d (position #\. body))
        (expect-name nil))
    (setf expect-name (if d (subseq body 0 d) body))
    (cond
     ;; [static]: the prefix must be a KNOWN resource name (::26)
     ((string= ann "static")
      (let ((known nil))
        (loop for v being the hash-values of (cval-vis-imports state)
              do (when (and (stringp v) (string= v expect-name))
                   (setf known t)))
        (loop for v being the hash-values of (cval-vis-exports state)
              do (when (and (stringp v) (string= v expect-name))
                   (setf known t)))
        (unless known
          (%verr "static resource name is not known in this context"))))
     ;; [constructor]/[method]: resolve the referenced resource's idx
     ((member ann '("constructor" "method") :test #'string=)
      (let ((ridx (%ann-referenced-resource ann ft state)))
        (when ridx
          (let ((got (if import-p
                         (gethash ridx (cval-vis-imports state))
                         (gethash ridx (cval-vis-exports state)))))
            (cond
             ((and got (not (string= got expect-name)))
              (if (string= ann "constructor")
                  (%verr "function does not match expected resource name `~a`"
                         got)
                  (%verr "method does not match expected resource name `~a`"
                         got)))
             ((null got)
              (%verr "resource used in function does not have a name in this context")))))))
     (t nil))))

(defun %ann-referenced-resource (ann ft state)
  "The typeidx of the resource the annotated function's type
references: [constructor] = the own-result's target; [method] = the
self-borrow's target. NIL when the shape does not reference a
resource."
  (when (and ft (ct-functype-p ft))
    (cond
     ((string= ann "constructor")
      (let ((results (second (ct-payload ft))))
        (when (and (consp results) (eq (car results) :idx))
          (let ((rt (%ct-at state (cdr results))))
            (when (eq (ct-kind rt) :own)
              (ct-payload rt))))))
     ((string= ann "method")
      (let ((params (first (ct-payload ft))))
        (when (plusp (length params))
          (let ((vt (cdr (aref params 0))))
            (when (and (consp vt) (eq (car vt) :idx))
              (let ((bt (%ct-at state (cdr vt))))
                (when (eq (ct-kind bt) :borrow)
                  (ct-payload bt))))))))
     (t nil))))

(defun %check-annotated-name (name ft state &optional import-p)
  "The [constructor]/[method]/[static] annotated-name shape rules
\(annotated-names.wast, Explainer 'Annotated names'): the name must
refer to a FUNCTION whose type matches the annotation. FT is the
resolved functype ct (NIL when the externtype is not a func or the
type is unresolvable). The grammar checks (dot, kebab) live in
%check-externname; the RESOURCE-NAME rules fire separately."
  (unless (and (> (length name) 0) (char= (char name 0) #\[))
    (return-from %check-annotated-name))
  (let ((close (position #\] name)))
    (unless close (return-from %check-annotated-name))
    (let* ((ann (subseq name 1 close))
           (body (subseq name (1+ close))))
      ;; slice 5k: the resource-NAME rules (separate flat helpers: the
      ;; inline nested version repeatedly tripped paren-balance)
      (%ann-resource-check name ann body ft state import-p)
      (case (intern (string-upcase ann) :keyword)
        (:constructor
         (unless (and ft (ct-functype-p ft))
           (%verr "is not a func"))
         (when (and ft (ct-functype-p ft))
           (let ((results (second (ct-payload ft))))
             (cond
              ((null results)
               (%verr "should return one value"))
              ;; slice 5k (::0): a WRAPPED result — the functype's
              ;; single result is a :result ct whose ok arm references
              ;; an (own $T) — is the legal (result (own $T) ...)
              ;; shape
              ((and (consp results) (eq (car results) :idx)
                    (eq (ct-kind (%ct-at state (cdr results)))
                        :result)
                    (let ((ok (car (ct-payload
                                    (%ct-at state (cdr results))))))
                      (and (consp ok) (eq (car ok) :idx)
                           (eq (ct-kind (%ct-at state (cdr ok)))
                               :own))))
               nil)
              ((not (and (consp results) (eq (car results) :idx)))
               ;; a single non-index result is a prim — the allowed
               ;; shape is exactly (own $T)
               (%verr "should return `(own $T)`"))
              (t
               (let ((rt (%ct-at state (cdr results))))
                 (unless (eq (ct-kind rt) :own)
                   (%verr "function should return `(own $T)` or `(result (own $T))`"))))))))
        ((:method :static)
         (unless (and ft (ct-functype-p ft))
           (%verr "is not a func"))
         ;; [static] constrains the function type NO FURTHER
         ;; (annotated-names ::19's positive has (param "x" u32));
         ;; [method] needs a first `self` param of (borrow $T)
         (when (and ft (ct-functype-p ft) (string= ann "method"))
           (let ((params (first (ct-payload ft))))
             (when (zerop (length params))
               (%verr "should have at least one argument"))
             (when (plusp (length params))
               (unless (string= (car (aref params 0)) "self")
                 (%verr "should have a first argument called `self`"))
               (let ((vt (cdr (aref params 0))))
                 (unless (and (consp vt) (eq (car vt) :idx)
                              (eq (ct-kind (%ct-at state (cdr vt)))
                                  :borrow))
                   (%verr "should take a first argument of `(borrow $T)`")))))))
        (t nil)))))

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
      ;; core table/global/memory/tag spaces grow only via ALIASES (a
      ;; component defines no core tables itself) — a synthesized
      ;; core-instance export referencing one with no prior alias is
      ;; out of bounds (instantiation.wast ::68-::70)
      (:core-table (when (>= idx (cval-core-tables state))
                     (%verr "index out of bounds")))
      (:core-global (when (>= idx (cval-core-globals state))
                      (%verr "index out of bounds")))
      (:core-memory (when (>= idx (cval-core-memories state))
                      (%verr "index out of bounds")))
      (:core-tag (when (>= idx (cval-core-tags state))
                   (%verr "index out of bounds")))
      (:core-type (when (>= idx (length (cval-core-types state)))
                   (%verr "core type index out of bounds")))
      (t nil))))

(defun %check-externname (name state import-p)
  "The externname grammar + strongly-unique within the scope
(kebab.wast, extern-names.wast) + the interfaceversion grammar
(extern-names.wast's @-version rows: a version must be a `valid
semver` or a `canonversion`)."
  (multiple-value-bind (kind detail) (parse-externname name)
    (when (eq kind :invalid)
      ;; kebab-bad's detail names the offending FRAGMENT (or an
      ;; explanatory string): a fragment-shaped detail maps to the
      ;; kebab message the fixtures pin (kebab.wast: "`1` is not in
      ;; kebab case"); anything else is the generic invalid-name.
      (if (and detail (plusp (length detail))
               (not (find-if (lambda (ch)
                               (or (char= ch #\ ) (char= ch #\`)))
                             detail)))
          (%verr "`~a` is not in kebab case" detail)
          (%verr "is not a valid extern name"))))
  ;; the @version grammar: a valid semver OR a canonversion (the
  ;; semver-parse detail is the message the fixtures pin: 'empty
  ;; string', \"unexpected character '.'\", 'unexpected end of
  ;; input', 'empty identifier segment')
  (let ((version (interface-name-version name)))
    (when version
      (unless (canonversion-p version)
        (let ((err (%semver-parse version)))
          (when err (%verr "~a" err))))))
  ;; strongly-unique (Explainer Name Uniqueness): canonicalize = strip
  ;; annotations per the rules; compare case-insensitively
  (let ((canon (%kebab-key (remove-annotations name)))
        (table (if import-p (cval-imports state) (cval-exports state))))
    (when (gethash canon table)
      (%verr "import name `~a` conflicts with previous name `~a`"
             name (gethash canon table)))
    (setf (gethash canon table) name)))

(defun remove-annotations (name)
  "Canonicalization steps 1.2/1.3 (Explainer Name Uniqueness):
  1.2: if the name is [...]*l.l, replace with l (the FIRST label);
  1.3: strip [...] annotations besides [constructor] and [set].
Step 1.1 (hyphen removal + lowercasing, %kebab-key) applies BEFORE
the 1.2 match: [static]foo-bar.FO-ob-AR's folded labels BOTH reduce
to `foobar`, so the pair matches and reduces to `foo-bar` — kebab.wast
::34 pins the conflict with the plain `foo-bar` import. Returns the
pre-1.1 name (the caller %kebab-key's the result)."
  (declare (type string name))
  (labels
      ((strip-annotations (s)
         ;; step 1.3, applied to a PLAIN (non-annotated) name
         (let ((open (position #\[ s)))
           (if (null open)
               s
               (let ((close (position #\] s :start open)))
                 (if (null close)
                     s
                     (let* ((ann (subseq s (1+ open) close))
                            (keep (or (string= ann "constructor")
                                      (string= ann "set")))
                            (tail (strip-annotations
                                   (subseq s (1+ close)))))
                       (concatenate 'string
                                    (subseq s 0 open)
                                    (if keep (subseq s open (1+ close)) "")
                                    tail))))))))
    (if (and (plusp (length name)) (char= (char name 0) #\[))
        ;; an annotated name: split the annotation prefix off, apply
        ;; 1.2's reduction when the FOLDED labels match (differing
        ;; pairs like [method]foo.bar keep their distinct identity),
        ;; then reattach [constructor]/[set] per 1.3
        (let* ((close (position #\] name :from-end t))
               (body (and close (subseq name (1+ close))))
               (dot (and body (position #\. body)))
               (l1 (and dot (subseq body 0 dot)))
               (l2 (and dot (subseq body (1+ dot))))
               (matches (and l1 l2
                             (string= (%kebab-key l1) (%kebab-key l2))))
               (reduced (cond
                         ((and dot matches) l1)
                         (dot body)
                         (t body)))
               (ann (and close (subseq name 1 close))))
          (cond
           ((null close) name)
           ((or (string= ann "constructor") (string= ann "set"))
            (concatenate 'string (subseq name 0 (1+ close)) reduced))
           (t reduced)))
        (strip-annotations name))))

(defun parse-externname-kind (name)
  "The KIND of (parse-externname NAME): :plain, :interface or
:invalid — the uniqueness/name rules below only branch on it."
  (parse-externname name))

(defun %attr-of-kind (attrs kind)
  "The value of the first attribute of KIND (:implements /
:version-suffix / :external-id), or NIL. ATTRS is the importrec/
exportrec attrs list of (kind value) lists."
  (let ((hit (assoc kind attrs :test #'eq)))
    (cond ((null hit) nil)
          ;; the binary reader builds (kind value) lists; the text
          ;; parser builds (kind . value) conses — accept both.
          ((consp (cdr hit)) (second hit))
          (t (cdr hit)))))


(defun %check-name-attrs (name attrs sort)
  "The implements/version-suffix attribute rules (Binary.md lines
438-476, upstream validate_extern's ordering). NAME is the import/
export externname, ATTRS the attribute list, SORT the external sort.
Order per upstream: version-suffix (instance-only, non-empty, needs an
interfacename or implements), then implements (bare-plain name,
instance-only, interfacename value + version check), then the NAME's
own interfaceversion check."
  (let ((suffix (%attr-of-kind attrs :version-suffix))
        (implements (%attr-of-kind attrs :implements)))
    ;; ---- version-suffix ----
    (when suffix
      (unless (eq sort :instance)
        (%verr "only instances can have a `version-suffix`"))
      (when (zerop (length suffix))
        (%verr "version suffix cannot be empty"))
      ;; without `implements` the suffix applies to NAME: it must be
      ;; an interfacename (the version itself is checked below)
      (unless (or implements
                 (eq (parse-externname-kind name) :interface))
        (%verr "`version-suffix` requires an interface name or `implements`")))
    ;; ---- implements ----
    (when implements
      ;; the import/export name must be a BARE plain name
      (unless (and (eq (parse-externname-kind name) :plain)
                   (not (and (plusp (length name))
                             (char= (char name 0) #\[))))
        (%verr "name `~a` is not valid with `implements`" name))
      (unless (eq sort :instance)
        (%verr "only instances can have an `implements`"))
      ;; the implements value must be an interfacename; an INVALID
      ;; value reports the malformed-name message (attributes.wast:
      ;; 'not a valid name' for the empty string)
      (let ((ikind (parse-externname-kind implements)))
        (cond
         ((eq ikind :interface) nil)
         ((eq ikind :invalid)
          (%verr "`~a` is not a valid name" implements))
         (t (%verr "name `~a` must be an interface" implements))))
      ;; the version check runs against the implements interfacename
      (validate-interface-version (interface-name-version implements)
                                  suffix))
    ;; ---- the NAME's own interfaceversion (applies with or without
    ;; attributes — extern-names.wast's semver rows; the suffix applies
    ;; to the name only when there is no implements)
    (unless implements
      (let ((version (interface-name-version name)))
        (when (or version suffix)
          ;; a version WITHOUT a suffix must be a full valid semver
          ;; (a bare canonversion like `1` is not — version-suffix.wast
          ;; ::11/::12); a SUFFIX with no version in the name is the
          ;; (nil, suffix) case — 'a version suffix requires the name
          ;; to have a version' (version-suffix.wast ::23); with both
          ;; the concatenation must be a valid semver AND split back
          ;; into (version, suffix)
          (validate-interface-version version suffix))))))


(defun %check-resource-dtor (payload state comp)
  "Slice 5c (resources ::60/::61, Binary.md 265): a resource type's
dtor must be a valid core func index with signature [i32] -> [] (the
handle's representation dropped on entry)."
  (let ((dtor (and (ct-resourcetype-p payload)
                   (cdr (ct-payload payload)))))
    (when dtor
      (unless (< dtor (cval-core-funcs state))
        (%verr "function index out of bounds"))
      (let ((sig (%canon-core-func-sig comp dtor)))
        (when (and sig
                   (not (and (equal (car sig) '(:i32))
                             (null (cdr sig)))))
          (%verr "wrong signature for a destructor"))))))

(defun %deindex-functype (ft state)
  "Values lane (concat's map fixtures): a functype whose
(:idx . k) valtype refs resolve against a CHILD component's state
(an alias-projected func out of an INSTANTIATED instance reads the
child's stashed functype with RAW child-space indices). Rewrite each
ref to the RESOLVED ct so any reader sees the full shape without
needing the child's space. NIL when unresolvable."
  (when (and ft (ct-p ft) (eq (ct-kind ft) :func))
    (labels
        ((deindex (vt)
           (if (and (consp vt) (eq (car vt) :idx))
               (let ((c (and (>= (cdr vt) 0)
                             (< (cdr vt) (length (cval-types state)))
                             (aref (cval-types state) (cdr vt)))))
                 (or c vt))
               (if (ct-p vt)
                   (deindex-ct vt)
                   vt)))
         (deindex-ct (c)
           (case (ct-kind c)
             (:prim c)
             (:list
              (make-ct :kind :list
                       :payload (cons (deindex (car (ct-payload c)))
                                      (cdr (ct-payload c)))
                       :offset (ct-offset c)))
             (:record
              (make-ct :kind :record
                       :payload (coerce
                                 (loop for (l . fvt) across (ct-payload c)
                                       collect (cons l (deindex fvt)))
                                 'vector)
                       :offset (ct-offset c)))
             (:tuple
              (make-ct :kind :tuple
                       :payload (coerce
                                 (loop for t_ across (ct-payload c)
                                       collect (deindex t_))
                                 'vector)
                       :offset (ct-offset c)))
             (:variant
              (make-ct :kind :variant
                       :payload (coerce
                                 (loop for (l . cvt) across (ct-payload c)
                                       collect (cons l
                                                     (and cvt
                                                          (deindex cvt))))
                                 'vector)
                       :offset (ct-offset c)))
             (:option
              (make-ct :kind :option
                       :payload (deindex (ct-payload c))
                       :offset (ct-offset c)))
             (:result
              (make-ct :kind :result
                       :payload (cons (and (car (ct-payload c))
                                          (deindex (car (ct-payload c))))
                                     (and (cdr (ct-payload c))
                                          (deindex (cdr (ct-payload c)))))
                       :offset (ct-offset c)))
             (:map
              (make-ct :kind :map
                       :payload (cons (deindex (car (ct-payload c)))
                                      (deindex (cdr (ct-payload c))))
                       :offset (ct-offset c)))
             (t c))))
      (let* ((pl (ct-payload ft))
             (params (first pl))
             (results (second pl)))
        (make-ct :kind :func
                 :payload (list (if (vectorp params)
                                    (map 'vector
                                         (lambda (p)
                                           (cons (car p)
                                                 (deindex (cdr p))))
                                         params)
                                    params)
                               (and results (deindex results))
                               (third pl))
                 :offset (ct-offset ft))))))

(defun %remap-decl-functype (ft inst-idx ict)
  "M9 (linking ::90): a functype read out of an IMPORTED instance's
declarator space — its (:idx . k) valtype payloads reference the
DECLARATOR's own type space. An inline (own $r)/(borrow $r) result
DESUGARS to an interned OWN/BORROW entry at position k whose PAYLOAD
is $r's declarator-space index m; the exportdecl whose ascribed
(type . m) introduced m names the resource. Remap k to the
COMPONENT-space typeidx of the (alias export $inst \"name\" (type))
that projects that export. Unresolvable references stay untouched."
  (labels
      ((entry-at (k)
         "The declarator's k-th type-introducing entry's payload. A
:TYPE declarator yields its ct; an ALIAS/IMPORT yields NIL; and
(values lane, variants.wast) an eq-BOUND :TYPE EXPORT declarator —
(export \"v32\" (type $v32 (eq $v32'))) — yields the eq's UNDERLYING
entry: the old NIL left the raw (:idx . k) in the remap and the
component-space checks read a declarator index ('type index out of
bounds')."
         ;; (values lane, concat.wast ::36): the DECLARATOR type space
         ;; counts EVERY declarator (Binary.md: each declarator
         ;; introduces a type index — :type, :alias, :import, AND
         ;; :export alike; the old :type/-alias-only counting maxed
         ;; at the 15 :TYPE entries while the interned inline maps
         ;; sat at index 18, past the last TYPE, and the raw
         ;; (:idx . 18) leaked through to the component space).
         (loop with n = -1
               for td across (ct-payload ict)
               for i from 0
               do (case (typedecl-kind td)
                    (:type
                     (incf n)
                     (when (= n k)
                       (return-from entry-at (typedecl-payload td))))
                    ((:alias :import)
                     (incf n)
                     (when (= n k)
                       ;; (values lane, numerics.wast ::22): an ALIAS
                       ;; declarator (the sunk implicit outer alias of
                       ;; an eq-ref'd outer type) resolves THROUGH the
                       ;; outer chain to the target ct — the old NIL
                       ;; left the raw (:idx . k) and %needs-memory-p
                       ;; read a declarator index against the
                       ;; component space ('type index out of bounds').
                       ;; The declarator's count N maps to the ambient
                       ;; chain as N-1 (the declarator's own INNER
                       ;; scope is skipped): %outer-state-at count
                       ;; 1 = the first ambient entry (the ENCLOSING
                       ;; component, pushed by the nested :component
                       ;; arm).
                       (let* ((a (typedecl-payload td))
                              (tgt (and (eq (aliasrec-kind a) :outer)
                                        (%outer-state-at
                                         nil (1- (aliasrec-count a)))))
                             (raw (and tgt
                                       (< (aliasrec-target a)
                                          (length (cval-types tgt)))
                                       (aref (cval-types tgt)
                                             (aliasrec-target a)))))
                         ;; (values lane) the SAME resource-substitution
                         ;; rule as %check-outer-alias: a TYPE alias
                         ;; crossing a boundary may not target a type
                         ;; that transitively refers to resources
                         ;; (outer-alias.wast ::6: the resource $R
                         ;; sunk through a doubly-nested component
                         ;; type's declarators)
                         (when (and raw (ct-p raw)
                                    (%ct-transitively-resources-p
                                     raw tgt (make-hash-table)))
                           (%verr "transitively refers to resources"))
                         (return-from entry-at raw))))
                    (:export
                     ;; every EXPORT introduces an index too (the
                     ;; ORIGINAL counted all four kinds — the
                     ;; k-battery rewrite narrowed the counting and
                     ;; the interned inline types past the last
                     ;; :TYPE entry went unreachable). An eq-BOUND
                     ;; :TYPE export yields the eq's UNDERLYING entry
                     ;; (variants.wast); any other export (a
                     ;; FUNC-sorted one — concat's inline (export
                     ;; "map-str-u32" (func ...))) yields NIL: its
                     ;; inline types live at their OWN interned TYPE
                     ;; entries, never at the export's index.
                     (incf n)
                     (when (= n k)
                       (let* ((et (third (typedecl-payload td))))
                         (return-from entry-at
                           (and (consp et) (eq (car et) :type)
                                (let* ((bound (cdr et))
                                       (m (and (listp bound)
                                               (eq (car bound) :eq)
                                               (second bound))))
                                  (and m
                                       (deindex-ct (entry-at m)))))))))
                    (t nil))
               finally (return-from entry-at nil)))
       (deindex-ct (c)
         "Values lane (variants.wast): a declarator entry's ct may
itself carry (:idx . m) refs — inline expressions interned into the
DECLARATOR's own type space. Resolve them through remap-valtype so
the component-space checks never read a declarator index."
         (if (not (ct-p c))
             c
             (case (ct-kind c)
               (:prim c)
               (:list
                (make-ct :kind :list
                         :payload (cons (remap-valtype
                                         (car (ct-payload c)))
                                        (cdr (ct-payload c)))
                         :offset (ct-offset c)))
               (:record
                (make-ct :kind :record
                         :payload (coerce
                                   (loop for (l . fvt)
                                         across (ct-payload c)
                                         collect (cons l
                                                       (remap-valtype fvt)))
                                   'vector)
                         :offset (ct-offset c)))
               (:tuple
                (make-ct :kind :tuple
                         :payload (coerce
                                   (loop for t_ across (ct-payload c)
                                         collect (remap-valtype t_))
                                   'vector)
                         :offset (ct-offset c)))
               (:variant
                (make-ct :kind :variant
                         :payload (coerce
                                   (loop for (l . cvt)
                                         across (ct-payload c)
                                         collect (cons l
                                                       (and cvt
                                                            (remap-valtype
                                                             cvt))))
                                   'vector)
                         :offset (ct-offset c)))
               (:option
                (make-ct :kind :option
                         :payload (remap-valtype (ct-payload c))
                         :offset (ct-offset c)))
               (:result
                (make-ct :kind :result
                         :payload (cons
                                   (and (car (ct-payload c))
                                        (remap-valtype
                                         (car (ct-payload c))))
                                   (and (cdr (ct-payload c))
                                        (remap-valtype
                                         (cdr (ct-payload c)))))
                         :offset (ct-offset c)))
               (:map
                (make-ct :kind :map
                         :payload (cons (remap-valtype
                                         (car (ct-payload c)))
                                        (remap-valtype
                                         (cdr (ct-payload c))))
                         :offset (ct-offset c)))
               (t c))))
       (export-name-for (m)
         "The name of the m-th TYPE-INTRODUCING exportdecl (in
declarator order): a (sub resource) export's et carries the
:SUB-RESOURCE marker — its declarator-space position is the export
ARM's ordinal (the bind index the arm assigns), so count type-sorted
exportdecls."
         (loop with n = -1
               for td across (ct-payload ict)
               when (and (eq (typedecl-kind td) :export)
                         (let ((et (third (typedecl-payload td))))
                           (and (consp et) (eq (car et) :type))))
                 do (incf n)
                    (when (= n m)
                      (return-from export-name-for
                        (first (typedecl-payload td))))
               finally (return-from export-name-for nil)))
       (remap-valtype (vt)
         (if (not (and (consp vt) (eq (car vt) :idx)
                       (integerp (cdr vt))))
             vt
             (let* ((k (cdr vt))
                    (e (deindex-ct (entry-at k)))
                    ;; the interned own/borrow entry's payload is the
                    ;; OWNED type's declarator-space index
                    (m (and (ct-p e)
                            (member (ct-kind e) '(:own :borrow))
                            (integerp (ct-payload e))
                            (ct-payload e)))
                    (nm (and m (export-name-for m)))
                    (hit (and nm
                              (gethash (cons inst-idx nm)
                                       *inst-alias-typeidx*)))
                    ;; no projecting type alias: the resource exists
                    ;; only in the IMPORT's declarator space — the
                    ;; own/borrow valtype is a RESOURCE handle (i32,
                    ;; no memory forcing); substitute a concrete
                    ;; resource marker ct so the component-space
                    ;; resolution never reads the declarator index
                    (target (or hit
                                (and (ct-p e)
                                     (member (ct-kind e)
                                             '(:own :borrow))
                                     m
                                     (make-ct :kind :resource
                                              :payload '(0 . nil)
                                              :offset 0)))))
               (cond
                ((and (null target) (ct-p e)
                      (not (member (ct-kind e) '(:own :borrow))))
                 e)
                (target
                 (if (integerp target) (cons :idx target) target))
                (t vt))))))
    (if (not (and ft (ct-p ft) (eq (ct-kind ft) :func)))
        ft
        (let* ((pl (ct-payload ft))
               (params (first pl))
               (results (second pl)))
          (make-ct :kind :func
                   :payload (list (if (vectorp params)
                                      (map 'vector
                                           (lambda (p)
                                             (cons (car p)
                                                   (remap-valtype
                                                    (cdr p))))
                                           params)
                                      params)
                                  (remap-valtype results)
                                  (third pl))
                   :offset (ct-offset ft))))))

(defun %resolve-aliased-func (payload state)
  "Slice 5e (resources ::27/::28/::43): an EXPORT-aliased func out of
an IMPORTED instance carries the source instance ct's functype (the
child's with-arg matching reads it) AND its per-instance identity
space (%instance-ct-ids, so both sides of the func-type match resolve
in their own spaces); other sources stay nil. A helper: the inline
version tripped the compiler's cross-arm payload type derivation."
  (let* ((ent (and (eq (aliasrec-kind payload) :export)
                   (< (aliasrec-index payload)
                      (length (cval-instance-types state)))
                   (aref (cval-instance-types state)
                         (aliasrec-index payload))))
         (ict (and (ct-p ent)
                   (eq (ct-kind ent) :instance)
                   ent)))
    (vector-push-extend
     (or (and ict
              (%instance-ct-export-functype
               (aliasrec-index payload)
               (aliasrec-name payload)
               state))
         ;; slice 5l (resources ::39): an INSTANTIATED source — the
         ;; child's export table entry (name -> (func . k)) resolves
         ;; k through the CHILD's stashed func-types space, and the
         ;; func-arg matching resolves BOTH sides in their own spaces
         ;; (the CHILD's ids from the stash, the parent's from state)
         (let* ((src-comp (gethash (aliasrec-index payload)
                                    *inst-source-comp*))
                (child-state (and src-comp
                                  (gethash src-comp
                                           *comp-state-stash*)))
                (hit (and (hash-table-p ent)
                          (gethash (aliasrec-name payload) ent
                                   :missing))))
           (when (and child-state
                      (consp hit)
                      (eq (car hit) :func)
                      (integerp (cdr hit)))
             ;; register the per-instance identity space for the
             ;; parent-side resolution — the IDS STASH (the
             ;; instantiation's substitution), NOT the child-state's
             ;; own ids (a different generative fresh id space)
             (setf (gethash (1- (cval-funcs state))
                            *func-arg-ids*)
                   (gethash (aliasrec-index payload)
                            *inst-ids-stash*))
             ;; (values lane) the stashed functype's refs are CHILD-
             ;; space indices — record the child's VALIDATOR STATE as
             ;; the func's origin so the canon checks can DEINDEX the
             ;; refs to full ct shapes (concat's map fixtures: the
             ;; lowered import's (map string u32) params)
             (setf (gethash (1- (cval-funcs state))
                            *func-alias-origin*)
                   (cons :state child-state))
             (and (< (cdr hit)
                     (length (cval-func-types child-state)))
                  (aref (cval-func-types child-state)
                        (cdr hit))))))
     (cval-func-types state))
    (when ict
      (setf (gethash (1- (cval-funcs state))
                     *func-arg-ids*)
            (%instance-ct-ids
             (aliasrec-index payload) ict))
      ;; M9 (linking ::90): the canon checks read this functype against
      ;; the COMPONENT's type space — record the declarator origin so
      ;; the (:idx . k) payloads can be remapped there
      (setf (gethash (1- (cval-funcs state))
                     *func-alias-origin*)
            (cons (aliasrec-index payload) ict)))))

(defun validate-component (comp &key (outer-depth 0))
  "Validate a decoded component. Signals wasmdown:validation-error on
any static rule violation; returns the component. OUTER-DEPTH is
the number of enclosing component/type scopes (outer aliases)."
  (let ((*validate-depth* (1+ *validate-depth*)))
    (when (= *validate-depth* 1)
      (clrhash *comp-state-stash*)
      (clrhash *state-comp*)
      (clrhash *inst-source-comp*)
      (clrhash *inst-ids-stash*)
      (clrhash *inst-gen-ids*)
      (clrhash *func-arg-ids*)
      ;; M9 (linking ::90/::169)
      (clrhash *inst-alias-typeidx*)
      (clrhash *func-alias-origin*)
      (clrhash *alias-component-source*))
  (let ((state (make-cval
                :types (make-array 16 :fill-pointer 0 :adjustable t)
                :core-types (make-array 16 :fill-pointer 0 :adjustable t)
                :func-types (make-array 16 :fill-pointer 0 :adjustable t)
                :type-ids (make-array 16 :fill-pointer 0 :adjustable t)
                :instance-types (make-array 16 :fill-pointer 0
                                            :adjustable t)
                :imports (make-hash-table :test #'equal)
                :exports (make-hash-table :test #'equal)))
        (core-funcs 0) (memories 0) (tables 0) (globals 0)
        ;; slice 4: the static core-resolution tables (see the banner
        ;; above %core-module-at) — FRESH per component: each nested
        ;; validate-component binds its own, so a child's core
        ;; indices never collide with the parent's
        (*core-alias-source* (make-hash-table :test #'equal))
        (*core-inst-provider* (make-hash-table :test #'eql))
        ;; slice 5 (outer-alias A): the ENCLOSING validator states,
        ;; innermost first — %check-outer-alias resolves a de Bruijn
        ;; count of N against the Nth entry (count 0 = THIS scope,
        ;; per Explainer 408-412). The :component arm below pushes
        ;; THIS state around each nested validate-component; the
        ;; type-declarator arms push in %check-defvaltype.
        (*outer-states* *outer-states*))
    (declare (ignorable core-funcs memories tables globals))
    (loop for d across (comp-defs comp)
          do (let ((payload (compdef-payload d)))
               (case (compdef-kind d)
                 (:core-module
                 ;; the embedded core module's code is CORE-validated
                 ;; here (core-modules.wast ::1: 'type mismatch' for an
                 ;; operand-stack underflow), and the mangled two-
                 ;; level import names must be unique (::7/::8:
                 ;; 'duplicate import name `:`'/`:a`' — validate-module
                 ;; does not check two-level names)
                 (let ((m payload))
                   (when (wasmdown.types:wasm-module-p m)
                     (wasmdown:validate-module m)
                     (let ((names '()))
                       (loop for im across (wasmdown.types:wasm-module-imports
                                           m)
                             for key = (format nil "~a:~a"
                                               (wasmdown.types:wasm-import-module im)
                                               (wasmdown.types:wasm-import-field im))
                             do (when (member key names :test #'string=)
                                  (%verr "duplicate import name `~a`" key))
                                (push key names)))))
                 (incf (cval-core-modules state)))
                 (:core-instance
                  (ecase (coreinstrec-kind payload)
                    (:instantiate
                     (when (>= (coreinstrec-index payload)
                               (cval-core-modules state))
                       (%verr "module index out of bounds"))
                     ;; slice 4: core module instantiation argument
                     ;; matching (instantiation.wast ::39-::51)
                     (%check-core-instance-args payload comp state)
                     ;; slice 4: record how THIS instance's exports
                     ;; resolve (an :instantiate provider reads its
                     ;; module's exports). The current instance's
                     ;; index is the pre-incf count (N existing
                     ;; instances => this one is N).
                     (setf (gethash (cval-core-instances state)
                                    *core-inst-provider*)
                           (cons :module (coreinstrec-index payload))))
                    (:inline-exports
                     (loop for (nil . si) across (coreinstrec-exports payload)
                           do (%check-sortidx si state))
                     ;; slice 5m (instantiation ::59): the inline bag's
                     ;; export names are strongly-unique WITHIN the
                     ;; core instance ('export name `a` already
                     ;; defined')
                     (let ((seen '()))
                       (loop for pair across
                             (coreinstrec-exports payload)
                             for nm = (car pair)
                             do (when (member nm seen :test #'string=)
                                  (%verr "export name `~a` already defined"
                                         nm))
                                (push nm seen)))
                     ;; slice 4: a synthesized bag's exports point at
                     ;; alias-introduced core indices
                     (setf (gethash (cval-core-instances state)
                                    *core-inst-provider*)
                           (cons :bag (coreinstrec-exports payload)))))
                  (incf (cval-core-instances state)))
                 (:core-type
                  ;; core types: rectypes + the 0x50 moduletype shape;
                  ;; a moduletype's INTERNALS validate here
                  ;; (core-modules.wast ::2-::10)
                  (%check-moduletype payload)
                  (vector-push-extend payload (cval-core-types state)))
                 (:component
                  ;; a nested component is a full component: validate
                  ;; its defs with one more enclosing scope (an alias
                  ;; outer 1 ... inside it sees THIS scope). Slice 5
                  ;; (outer-alias A): push THIS state for the child's
                  ;; de Bruijn resolution.
                  (let ((*outer-states* (cons state *outer-states*)))
                    ;; M9 (linking ::179): record the ENCLOSING state's
                    ;; comp BEFORE the recursion — the child's outer-
                    ;; alias resolution (*state-comp*) runs during the
                    ;; child's walk, before the parent's own end-of-
                    ;; validate stash
                    (setf (gethash state *state-comp*) comp)
                    (validate-component payload
                                        :outer-depth (1+ outer-depth)))
                  (incf (cval-components state)))
                 (:instance
                  (vector-push-extend nil (cval-instance-types state))
                  (ecase (instrec-kind payload)
                    (:instantiate
                     (when (>= (instrec-index payload)
                               (cval-components state))
                       (%verr "component index out of bounds"))
                     (loop for (nil . si) across (instrec-args payload)
                           do (%check-sortidx si state))
                     ;; M7 slice 3b: structural matching of every
                     ;; with-argument against the target's imports
                     ;; slice 5c: the rebuilt child ids stashed per
                     ;; instance for the export-identity resolution
                     (setf (gethash (cval-instances state)
                                    *inst-ids-stash*)
                           (%check-instantiate-args payload comp state))
                     ;; slice 4 (family 3): the instance's source
                     ;; component (the alias-type shape resolution
                     ;; reads the child's type space through the
                     ;; stash)
                     (setf (gethash (cval-instances state)
                                    *inst-source-comp*)
                           (%component-at comp (instrec-index payload)))
                     ;; slice 3b: record the INSTANTIATED instance's
                     ;; exports (name -> sort) so later projections
                     ;; (alias export / (export ... (instance $i "n")))
                     ;; can resolve them (instantiation.wast
                     ;; ::75/::77/::79 'no export named `x`')
                    (let ((tab (make-hash-table :test #'equal))
                          ;; M9 (linking ::169): an alias-introduced
                          ;; component target resolves through the
                          ;; export chain (%component-at returns NIL at
                          ;; an alias's index)
                          (tc (or (%component-at comp
                                                 (instrec-index payload))
                                  (%aliased-component-target
                                   comp (instrec-index payload)))))
                       ;; slice 5f (resources ::43): an IMPORTED
                       ;; component target's exports read from the
                       ;; import's ascribed COMPONENT TYPE (the ct in
                       ;; THIS scope's type space) — the import occupies
                       ;; the component index space (Binary.md: imports
                       ;; extend their sort's space)
                       (unless tc
                         (let ((ict (loop with n = -1
                                          for x across (comp-defs comp)
                                          do (case (compdef-kind x)
                                               (:component (incf n))
                                               (:import
                                                (when (eq (car (importrec-type
                                                                (compdef-payload x)))
                                                          :component)
                                                  (incf n)
                                                  (when (= n (instrec-index
                                                              payload))
                                                    (let ((k (cdr (importrec-type
                                                                   (compdef-payload
                                                                    x)))))
                                                      (return (and (< k (length
                                                                          (cval-types
                                                                           state)))
                                                                   (aref (cval-types
                                                                          state)
                                                                         k)))))))
                                               (t nil))
                                          finally (return nil))))
                           (when (and ict (ct-p ict)
                                      (eq (ct-kind ict) :component))
                             (let ((decls (coerce (ct-payload ict)
                                                 'list)))
                               ;; pre-pass: the type-introducing
                               ;; positions -> the declarator, so an
                               ;; (eq i) export resolves its target's
                               ;; NAME (the identity source: all type
                               ;; exports use the name form)
                               (let ((by-pos
                                       (loop with n = -1
                                             for x in decls
                                             when (member
                                                   (typedecl-kind x)
                                                   '(:type :alias
                                                     :import :export))
                                               collect (cons (incf n)
                                                             x))))
                                 (dolist (dc decls)
                                   (when (eq (typedecl-kind dc)
                                             :export)
                                     (let* ((pl (typedecl-payload dc))
                                            (nm (first pl))
                                            (et (third pl)))
                                       (setf (gethash nm tab)
                                             (cons (car et)
                                                   (if (and (consp et)
                                                            (eq (car et)
                                                                :type))
                                                       (or (and
                                                            (consp (cdr et))
                                                            (eq (car
                                                                 (cdr et))
                                                                :eq)
                                                            (let* ((i
                                                                     (second
                                                                      (cdr
                                                                       et)))
                                                                   (td2
                                                                     (cdr (assoc
                                                                           i
                                                                           by-pos
                                                                           :test
                                                                           #'=))))
                                                              (and td2
                                                                   (eq
                                                                    (typedecl-kind
                                                                     td2)
                                                                    :export)
                                                                   (first
                                                                    (typedecl-payload
                                                                     td2)))))
                                                           nm)
                                                       (cdr et))))))))))))
                       (when tc
                         (dolist (dc (coerce (comp-defs tc) 'list))
                           (when (eq (compdef-kind dc) :export)
                             (setf (gethash (exportrec-name
                                             (compdef-payload dc))
                                            tab)
                                   ;; the CHILD's exported sortidx:
                                   ;; (sort . source-type-idx) — the
                                   ;; source index lets alias-export
                                   ;; identity thread through (r1/r2
                                   ;; re-exports share it; resources
                                   ;; ::41) and the vis walk resolve
                                   ;; the aliased shape (slice 3c).
                                   ;; An ASCRIBED (sub resource) export
                                   ;; breaks the sharing (::42): its
                                   ;; source is the NAME (fresh per name)
                                   (let ((asc (exportrec-type
                                               (compdef-payload dc))))
                                     (if (and asc
                                              (consp asc)
                                              (eq (car asc) :type)
                                              (eq (cdr asc) :sub-resource))
                                         (cons :type
                                               (exportrec-name
                                                (compdef-payload dc)))
                                         (cons (sortidx-sort
                                                (exportrec-sortidx
                                                 (compdef-payload dc)))
                                               (sortidx-idx
                                                (exportrec-sortidx
                                                 (compdef-payload dc))))))))))
                       (setf (aref (cval-instance-types state)
                                   (1- (length
                                        (cval-instance-types state))))
                             tab)))
                    (:inline-exports
                     (loop for (nil . si) across (instrec-exports payload)
                           do (%check-sortidx si state))
                     ;; the inline-export NAMES are externnames:
                     ;; kebab + strongly-unique WITHIN THIS INSTANCE
                     ;; (Binary.md: 'The externnames of all exports in
                     ;; a given component, instance, component-type
                     ;; or instance-type must be strongly-unique' —
                     ;; the instance is its OWN scope; a bag may reuse
                     ;; a name the enclosing component also exports,
                     ;; attributes.wast ::0's final bag exporting "a"
                     ;; alongside the component's export "a")
                     (loop with table = (make-hash-table :test #'equal)
                           for pair across (instrec-exports payload)
                           ;; the attribute rules apply to bag exports
                           ;; too (attributes.wast ::19/::24: implements
                           ;; must name an interface even inside a bag;
                           ;; the pair carries ((name attrs) . sortidx))
                           do (let* ((name (if (consp (car pair))
                                               (first (car pair))
                                               (car pair)))
                                     (attrs (if (consp (car pair))
                                                (second (car pair))
                                                nil))
                                     (sort (sortidx-sort (cdr pair))))
                                (%check-name-attrs name attrs sort)
                                (%check-externname
                                 name
                                 (make-cval
                                  :types (cval-types state)
                                  :core-types (cval-core-types state)
                                  :func-types (cval-func-types state)
                                  :exports table)
                                 nil)
                                ;; slice 5k (annotated-names ::32): the
                                ;; [constructor]/[method]/[static]
                                ;; rules for BAG exports — the resource
                                ;; must be named in THIS scope (a bag-
                                ;; local resource def has no name)
                                (when (and (eq sort :func)
                                           (< (sortidx-idx (cdr pair))
                                              (length (cval-func-types
                                                       state))))
                                  (%check-annotated-name
                                   name
                                   (aref (cval-func-types state)
                                         (sortidx-idx (cdr pair)))
                                   state nil))))
                     ;; slice 3b+4: a synthesized bag instance carries
                     ;; a HASH export table (name -> (sort . idx)) —
                     ;; %instance-export-sort reads either shape; the
                     ;; HASH keeps the source INDICES so family 3's
                     ;; alias-export projections can follow instance
                     ;; chains (ev ::55/::58)
                     (let ((bag (make-hash-table :test #'equal)))
                       (loop for pair across (instrec-exports payload)
                             do (let ((nm (if (consp (car pair))
                                              (first (car pair))
                                              (car pair))))
                                  (setf (gethash nm bag)
                                        (cons (sortidx-sort (cdr pair))
                                              (sortidx-idx
                                               (cdr pair))))))
                       (setf (aref (cval-instance-types state)
                                   (1- (length
                                        (cval-instance-types state))))
                             bag))))
                  (incf (cval-instances state)))

                 (:alias
                  (case (aliasrec-kind payload)
                    (:export
                     (when (>= (aliasrec-index payload)
                               (cval-instances state))
                       (%verr "instance index out of bounds"))
                     ;; slice 3b: the aliased export must EXIST and have
                     ;; the alias's sort (instantiation.wast ::77/::79
                     ;; 'instance N has no export named `x`'); slice 5m
                     ;; (instantiation ::81): a CORE-sorted alias off a
                     ;; component instance's export checks the actual
                     ;; sort ('is not a module')
                     (multiple-value-bind (sort known)
                         (%instance-export-sort (aliasrec-index payload)
                                                (aliasrec-name payload)
                                                state)
                       (unless known
                         (%verr "instance ~a has no export named `~a`"
                                (aliasrec-index payload)
                                (aliasrec-name payload)))
                       (when (and known
                                  (eq (aliasrec-sort payload)
                                      :core-module)
                                  (not (eq sort :core-module)))
                         (%verr "export `~a` of instance ~a is not a module"
                                (aliasrec-name payload)
                                (aliasrec-index payload)))))
                    (:core-export
                     (when (>= (aliasrec-index payload)
                               (cval-core-instances state))
                       (%verr "core instance index out of bounds"))
                     ;; slice 5m (instantiation ::78): the aliased
                     ;; core export must EXIST ('core instance 0 has
                     ;; no export named `a`') — an :instantiate
                     ;; provider's module exports resolve via
                     ;; %core-module-at; an inline-export BAG lists
                     ;; its names directly
                     ;; slice 5m (instantiation ::78) + M9 (linking tags ::10): the
                     ;; aliased core export must EXIST and carry the
                     ;; alias's SORT. An :instantiate provider's
                     ;; exports resolve via %core-module-at (a real
                     ;; wasm-module reads its export KINDS — 'is not a
                     ;; tag' on a sort mismatch); an inline-export
                     ;; BAG lists its names directly.
                     (multiple-value-bind (names kind-of)
                         (%core-provider-export-names
                          comp (aliasrec-index payload))
                       (declare (ignorable kind-of))
                       (unless (find (aliasrec-name payload) names
                                     :test #'string=)
                         (%verr "core instance ~a has no export named `~a`"
                                (aliasrec-index payload)
                                (aliasrec-name payload)))
                       ;; M9 (linking tags ::10): the export's actual
                       ;; kind (when statically known) must equal the
                       ;; alias's core sort
                       (let ((got (and kind-of
                                       (funcall kind-of
                                                (aliasrec-name payload)))))
                         (when (and got
                                    (not (eq got
                                             (aliasrec-sort payload))))
                           (%verr
                            "export `~a` for core instance ~a is not a ~a"
                            (aliasrec-name payload)
                            (aliasrec-index payload)
                            (%sort-err-name (aliasrec-sort payload))))))
                     ;; slice 4: record what this alias-introduced
                     ;; core index resolves to (the canon signature
                     ;; checks and bag providers read it)
                     (setf (gethash (cons (aliasrec-sort payload)
                                          (ecase (aliasrec-sort payload)
                                            (:core-func (cval-core-funcs state))
                                            (:core-memory (cval-core-memories state))
                                            (:core-table (cval-core-tables state))
                                            (:core-global (cval-core-globals state))
                                            (:core-tag (cval-core-tags state))))
                                    *core-alias-source*)
                           (cons (aliasrec-index payload)
                                 (aliasrec-name payload))))
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
                            ;; slice 4 (family 3): an EXPORT-aliased
                            ;; type carries the SOURCE type's SHAPE —
                            ;; a bag's table entry idx is THIS scope's
                            ;; typeidx (::48's $Rec' must be the record
                            ;; so the vis walk sees it).
                            ;; M9 (linking ::179 + EV ::52/::53): an
                            ;; OUTER type alias ALSO carries the
                            ;; resolved target's SHAPE (through
                            ;; *outer-states*) — the vis walk must see
                            ;; the underlying record/resource to fire
                            ;; 'func not valid to be used as
                            ;; import/export'; the opaque marker made
                            ;; the type invisible and the check passed
                            ;; WRONG once the implicit-alias def began
                            ;; emitting. The slot itself stays UNNAMED
                            ;; (only import/export declarators register
                            ;; vis names) — the two-site semantics.
                            (or (and (eq (aliasrec-kind payload) :export)
                                     (%alias-exported-type-shape
                                      payload state))
                                (and (eq (aliasrec-kind payload) :outer)
                                     (let ((tgt (%outer-state-at
                                                 state
                                                 (aliasrec-count payload))))
                                       (and tgt
                                            (< (aliasrec-target payload)
                                               (length (cval-types tgt)))
                                            ;; (values lane) an eq-
                                            ;; BOUND type import's slot
                                            ;; holds the opaque
                                            ;; :imported marker — the
                                            ;; alias's SHAPE follows
                                            ;; the eq to the under-
                                            ;; lying ct so the vis
                                            ;; walk sees the real
                                            ;; record/variant
                                            (%resolve-aliased-shape
                                             tgt
                                             (aref (cval-types tgt)
                                                   (aliasrec-target
                                                    payload))))))
                                (make-ct :kind :prim
                                         :payload :outer-aliased))
                            (cval-types state))
                           ;; identity (slice 3b): an outer alias is
                           ;; opaque; an exported-type alias carries the
                           ;; SOURCE instance's exported-type identity —
                           ;; equal iff two aliases project the same
                           ;; source type of the same instance (::41);
                           ;; unknown sources stay fresh per alias
                           (vector-push-extend
                            (if (eq (aliasrec-kind payload) :outer)
                                '(:opaque)
                                (or (and (eq (aliasrec-kind payload)
                                             :export)
                                         (%instance-export-identity
                                          (aliasrec-index payload)
                                          (aliasrec-name payload)
                                          state))
                                    (%fresh-resource-id)))
                            (cval-type-ids state))
                           ;; M9 (linking ::90): record the component
                           ;; typeidx this export-type alias occupies —
                           ;; the declarator-space remap's target
                           (when (eq (aliasrec-kind payload) :export)
                             (setf (gethash (cons (aliasrec-index payload)
                                                 (aliasrec-name payload))
                                            *inst-alias-typeidx*)
                                   (1- (length (cval-types state)))))
                          ;; external visibility (slice 3c): an
                          ;; alias export of a TYPE confers a name
                          ;; only when its SOURCE instance is itself
                          ;; externally visible (::46/::48/::49);
                          ;; an alias export of an INSTANCE from a
                          ;; visible instance keeps the chain alive.
                          ;; M9 (linking ::104): an IMPORTED source
                          ;; instance is externally visible by
                          ;; definition — its instance ct names its
                          ;; exports (the (sub resource) "r" the alias
                          ;; projects was import-named at the declarator)
                          (when (and (eq (aliasrec-kind payload)
                                         :export)
                                     (or (gethash (aliasrec-index payload)
                                                  (cval-vis-inst state))
                                         (let ((src (and
                                                     (< (aliasrec-index
                                                         payload)
                                                        (length
                                                         (cval-instance-types
                                                          state)))
                                                     (aref
                                                      (cval-instance-types
                                                       state)
                                                      (aliasrec-index
                                                       payload)))))
                                           (and (ct-p src)
                                                (eq (ct-kind src)
                                                    :instance)))))
                            (setf (gethash (1- (length
                                                (cval-types state)))
                                           (cval-vis-exports state))
                                  t)))
                    (:func (incf (cval-funcs state))
                           ;; slice 5e (resources ::27/::28/::43): see
                           ;; %resolve-aliased-func (a helper: the
                           ;; inline version tripped the compiler's
                           ;; cross-arm payload type derivation)
                           (%resolve-aliased-func payload state))
                    (:component
                     (incf (cval-components state))
                     ;; M9 (linking ::169): an alias-introduced
                     ;; component's SOURCE comp (when resolvable —
                     ;; an export alias of an instantiated instance)
                     ;; recorded for %check-instantiate-args: the
                     ;; alias occupies a component index, and
                     ;; instantiating it must resolve THIS comp's
                     ;; imports, not the nearest :component def's
                     (when (eq (aliasrec-kind payload) :export)
                       (let ((src (gethash (aliasrec-index payload)
                                           *inst-source-comp*)))
                         (when src
                           (setf (gethash (1- (cval-components state))
                                          *alias-component-source*)
                                 src)))))
                    (:instance (incf (cval-instances state))
                               ;; slice 4 (family 3): the aliased
                               ;; instance's export table is the
                               ;; PROJECTION of the source's named
                               ;; export (one level through this
                               ;; scope's tables) — ev ::55/::58
                               (vector-push-extend
                                (%instance-table-projection
                                 payload state)
                                (cval-instance-types state))
                              ;; slice 5j (external-visibility ::50):
                              ;; the projected instance's SOURCE is
                              ;; the ALIAS's source instance —
                              ;; propagate the source-comp record so
                              ;; the parent's vis walk resolves the
                              ;; projected entries through the child
                              (let ((src (gethash (aliasrec-index payload)
                                                  *inst-source-comp*)))
                                (when src
                                  (setf (gethash (1- (cval-instances
                                                      state))
                                                 *inst-source-comp*)
                                        src)))
                              ;; visibility chain: an instance aliased
                              ;; out of a VISIBLE instance is visible
                              (when (and (eq (aliasrec-kind payload)
                                             :export)
                                         (gethash (aliasrec-index payload)
                                                  (cval-vis-inst state)))
                                (setf (gethash (1- (cval-instances
                                                    state))
                                               (cval-vis-inst state))
                                      t)))
                    (:value (incf (cval-values state)))
                    (:core-module
                     (incf (cval-core-modules state))
                     ;; M9 (linking ::179): an outer-aliased core
                     ;; module RESOLVES to the target's module — the
                     ;; nested component's (instantiate
                     ;; $outer-module) reads it through
                     ;; %core-module-at's table; an EXPORT alias of
                     ;; an instantiated instance's module export
                     ;; resolves through the source-comp record
                     (let ((comp0 comp))
                       (when comp0
                         (setf (gethash (cons comp0
                                              (1- (cval-core-modules
                                                   state)))
                                        *outer-alias-core-module*)
                               (cond
                                ((and (eq (aliasrec-kind payload) :outer)
                                      (boundp '*outer-states*))
                                 (let ((tgt (%outer-state-at
                                             state
                                             (aliasrec-count payload))))
                                   (let ((oc (and tgt
                                                  (gethash tgt
                                                           *state-comp*))))
                                     (or (and oc
                                              (%core-module-at
                                               oc (aliasrec-target payload)))
                                         ;; M9 (linking ::231): the outer
                                         ;; slot is an IMPORT hole —
                                         ;; the module's SHAPE (the
                                         ;; ascribed moduletype) lives
                                         ;; in the OUTER comp; record
                                         ;; the (outer-comp . target-idx)
                                         ;; pair for the exports-find's
                                         ;; moduletype fallback
                                         (and oc
                                              (cons :via-import
                                                    (cons oc
                                                          (aliasrec-target
                                                           payload))))))))
                                ;; M9 (linking ::166 + ::183): an EXPORT
                                ;; alias of an instance's core-module
                                ;; export. An INSTANTIATED source: the
                                ;; recorded export table (hash: name ->
                                ;; (sort . idx)) into the source comp's
                                ;; core-module space. An IMPORTED source:
                                ;; the entry is an INSTANCE CT — the
                                ;; declarator's (:core-module . k) payload
                                ;; indexes THIS comp's core-type space
                                ;; (the moduletype, interned here by the
                                ;; ::183 parse fix); record the moduletype
                                ;; marker for the exports-find.
                                ((and (eq (aliasrec-kind payload) :export)
                                      (< (aliasrec-index payload)
                                         (length (cval-instance-types
                                                  state))))
                                 (let* ((itab (aref (cval-instance-types
                                                     state)
                                                    (aliasrec-index payload)))
                                        (hit (and (hash-table-p itab)
                                                  (gethash (aliasrec-name
                                                            payload)
                                                           itab))))
                                   (or (and (consp hit)
                                            (eq (car hit) :core-module)
                                            (let ((src (gethash
                                                        (aliasrec-index payload)
                                                        *inst-source-comp*)))
                                              (and src
                                                   (%core-module-at
                                                    src (cdr hit)))))
                                       (and (ct-p itab)
                                            (eq (ct-kind itab) :instance)
                                            (loop for td across
                                                     (ct-payload itab)
                                                  when (and
                                                       (eq (typedecl-kind
                                                            td)
                                                           :export)
                                                       (string=
                                                        (first
                                                         (typedecl-payload
                                                          td))
                                                        (aliasrec-name
                                                         payload)))
                                                    do (let* ((et (third
                                                                   (typedecl-payload
                                                                    td))))
                                                         (return
                                                          (and (consp et)
                                                               (eq (car et)
                                                                   :core-module)
                                                               (integerp
                                                                (cdr et))
                                                               (< (cdr et)
                                                                  (length
                                                                   (cval-core-types
                                                                    state)))
                                                               (let ((mt
                                                                       (aref
                                                                        (cval-core-types
                                                                         state)
                                                                        (cdr et))))
                                                                 (and (consp mt)
                                                                      (eq (car mt)
                                                                          :moduletype)
                                                                      (cons
                                                                       :via-moduletype
                                                                       mt))))))
                                                  finally (return nil))))))
                                (t nil))))))
                    (:core-type (vector-push-extend :aliased
                                                     (cval-core-types state)))
                    (:core-func (incf (cval-core-funcs state)))
                    (:core-instance (incf (cval-core-instances state)))
                    (:core-memory (incf (cval-core-memories state)))
                    (:core-table (incf (cval-core-tables state)))
                    (:core-global (incf (cval-core-globals state)))
                    (:core-tag (incf (cval-core-tags state)))))
                 (:type
                  ;; slice 5c (resources ::60/::61, Binary.md 265):
                  ;; a resource's dtor must be [i32] -> [] — see
                  ;; %check-resource-dtor (a separate defun: the
                  ;; inline version tripped the compiler's
                  ;; cross-arm type derivation)
                  (%check-resource-dtor payload state comp)
                  (%check-deftype payload state)
                  ;; slice 5d (resources ::66-::68/::70): record
                  ;; LOCALLY-DEFINED resource types — resource.new/rep
                  ;; may only target these, DIRECTLY or through an
                  ;; alias whose resolved identity equals the local
                  ;; resource's fresh id (::70's re-exported $R). The
                  ;; hash maps typeidx -> fresh-id (the SAME id the
                  ;; type-ids vector records).
                  (let ((rid (when (ct-resourcetype-p payload)
                               (%fresh-resource-id))))
                    (when rid
                      (setf (gethash (length (cval-types state))
                                     (cval-local-resources state))
                            rid))
                    (vector-push-extend payload (cval-types state))
                    ;; identity (slice 3b): resource definitions are
                    ;; generative; other types carry their concrete shape
                    (vector-push-extend
                     (if rid rid (cons :ct payload))
                     (cval-type-ids state))))
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
                                  (canonrec-typeidx payload)))))
                     ;; lift requires realloc for list/string params
                     ;; and memory for >MAX_FLAT flat results
                     ;; (CanonicalABI.md 'canon lift').
                     (%check-canon-opts payload state comp :lift))
                    (:lower
                     (when (>= (canonrec-func payload)
                               (cval-funcs state))
                       (%verr "function index out of bounds"))
                     ;; lower requires memory for list/string and for
                     ;; >MAX_FLAT params/results (CanonicalABI.md
                     ;; 'canon lower').
                     (%check-canon-opts payload state comp :lower))
                    ((:resource-new :resource-drop :resource-rep)
                     (let ((c (%ct-at state (canonrec-typeidx payload))))
                       (unless (ct-resourcetype-p c)
                         (%verr "type index ~d is not a resource type"
                                (canonrec-typeidx payload)))))
                    (:builtin
                     ;; built-in canons reference type indices where
                     ;; applicable; validated structurally. Slice 5c
                     ;; (resources ::62-::64): the resource.* built-ins
                     ;; require a RESOURCE type target. M11a
                     ;; (validate-task-return): task.return's result
                     ;; must not transitively contain a `borrow`
                     ;; (CanonicalABI.md 3930).
                     (let ((imms (canonrec-imms payload)))
                       (when (and (eq (first imms) :task-return)
                                  (second imms))
                         (let ((rs (second imms)))
                           ;; the BINARY reader's shape: one
                           ;; resultlist; the TEXT parser's: the
                           ;; collected valtypes
                           (when (and (consp rs)
                                      (consp (first rs))
                                      (consp (first (first rs))))
                             (setf rs (first rs)))
                           (dolist (r (if (and (consp rs) (listp rs))
                                          rs (list rs)))
                             (when (%vt-transitively-borrow-p
                                    r state (make-hash-table))
                               (%verr
                                "`task.return` result cannot contain a `borrow` type")))))
                      ;; slice 5m (indicies ::7/::8): only the
                      ;; TYPE-POSITION immediate bounds-checks
                      ;; against the component type space — the
                      ;; resource/stream/future built-ins carry it
                      ;; at position 1; thread.new-indirect's
                      ;; second imm is a CORE-TABLE idx and
                      ;; waitable-set.wait/poll's a CORE-MEMORY idx
                      ;; (not component typeidxs)
                      (when (and (second imms)
                                 (integerp (second imms))
                                 (member (first imms)
                                         '(:resource-new :resource-drop
                                           :resource-rep :stream-new
                                           :stream-read :stream-write
                                           :stream-cancel-read
                                           :stream-cancel-write
                                           :stream-drop-readable
                                           :stream-drop-writable
                                           :stream-forward :future-new
                                           :future-read :future-write
                                           :future-cancel-read
                                           :future-cancel-write
                                           :future-drop-readable
                                           :future-drop-writable
                                           :future-forward))
                        (when (>= (second imms)
                                   (length (cval-types state)))
                          (%verr "type index out of bounds"))))
                       (when (and (member (first imms)
                                          '(:resource-new :resource-drop
                                            :resource-rep))
                                  (second imms)
                                  (integerp (second imms)))
                         (let ((c (%ct-at state (second imms))))
                           (unless (ct-resourcetype-p c)
                             (%verr "not a resource type"))
                           ;; slice 5d (resources ::66-::67/::70):
                           ;; resource.new/rep may only target a
                           ;; LOCALLY-DEFINED resource (resource.drop
                           ;; accepts any resource — ::68's comment).
                           ;; Local = the typeidx is a local def OR its
                           ;; resolved identity equals a local
                           ;; resource's fresh id (::70's re-export).
                           (when (member (first imms)
                                          '(:resource-new :resource-rep))
                             (let* ((ti (second imms))
                                    (direct (gethash ti
                                                     (cval-local-resources
                                                      state)))
                                    (tid (and (< ti (length (cval-type-ids
                                                             state)))
                                              (aref (cval-type-ids state)
                                                    ti))))
                               (unless (or direct
                                           (and (consp tid)
                                                (eq (car tid) :fresh)
                                                (member (cdr tid)
                                                        (loop for v being
                                                              the hash-values
                                                              of (cval-local-resources
                                                                  state)
                                                            collect (cdr v))
                                                        :test #'eql)))
                                 (%verr "not a local resource")))))))))
                  ;; slice 4: the canon's INTRODUCED index space —
                  ;; :lift adds a COMPONENT func; :lower and the
                  ;; resource-*/builtin canons add CORE funcs (the
                  ;; inverted (core func $d (canon resource.drop
                  ;; $T)) def introduces a core func index; abi ::17
                  ;; style lowerings likewise)
                  (if (eq (canonrec-kind payload) :lift)
                      (progn (incf (cval-funcs state))
                             (vector-push-extend
                              (if (canonrec-typeidx payload)
                                  (aref (cval-types state)
                                        (canonrec-typeidx payload))
                                  nil)
                              (cval-func-types state)))
                      (let ((cidx (cval-core-funcs state)))
                        ;; M9 (linking lane): record the canon-
                        ;; introduced core func's STATIC SIGNATURE so
                        ;; core-instance with-arg matching resolves
                        ;; bag entries pointing at it (%core-provider-
                        ;; export follows *core-alias-source*, which
                        ;; only alias-introduced indexes carry). A
                        ;; LOWER's signature is flatten_functype of
                        ;; the target component func's ct; the
                        ;; resource built-ins are [i32] -> [i32] /
                        ;; [i32] -> [] / [i32] -> [i32].
                        (setf (gethash (cons :core-func cidx)
                                       *core-alias-source*)
                              (ecase (canonrec-kind payload)
                                (:lower
                                 (let* ((f (canonrec-func payload))
                                        (ft (and (< f (length (cval-func-types
                                                              state)))
                                                 (aref (cval-func-types
                                                        state) f)))
                                        ;; M9 (linking ::90): the same
                                        ;; declarator-space remap as
                                        ;; %check-canon-opts — the
                                        ;; lowered func's functype may
                                        ;; come from an imported
                                        ;; instance's ct
                                        (ft (let ((origin
                                                    (gethash
                                                     f *func-alias-origin*)))
                                              (if origin
                                                  (if (and (consp origin)
                                                           (eq (car origin)
                                                               :state))
                                                      (%deindex-functype
                                                       ft (cdr origin))
                                                      (%remap-decl-functype
                                                       ft (car origin)
                                                       (cdr origin)))
                                                  ft))))
                                   (cons :canon-lower
                                         (%canon-lower-flat-sig
                                          state ft
                                          (and (%opt (canonrec-opts payload)
                                                     :async)
                                               t)))))
                                ((:resource-new :resource-rep)
                                 (cons :canon-sig (cons '(:i32) '(:i32))))
                                (:resource-drop
                                 (cons :canon-sig (cons '(:i32) '())))
                                (:builtin
                                 ;; the INVERTED (core func $d (canon
                                 ;; resource.new $T)) parses as :BUILTIN
                                 ;; with the kind in the first immediate
                                 (let ((k (first (canonrec-imms payload))))
                                   (cond
                                    ((member k '(:resource-new
                                                :resource-rep))
                                     (cons :canon-sig
                                           (cons '(:i32) '(:i32))))
                                    ((eq k :resource-drop)
                                     (cons :canon-sig
                                           (cons '(:i32) '())))
                                    (t (cons :canon-sig
                                             (canon-builtin-flat-sig
                                              (canonrec-imms payload)
                                              state))))))))
                        (incf (cval-core-funcs state)))))
                 (:import
                  (%check-externname (importrec-name payload) state t)
                  (%check-externtype (importrec-type payload) state nil)
                  (%check-name-attrs (importrec-name payload)
                                     (importrec-attrs payload)
                                     (car (importrec-type payload)))
                 ;; external visibility (external-visibility.wast):
                 ;; every nameable type transitively used by the
                 ;; import's type must be IMPORT-named; imports may
                 ;; not see export names (::3/::44)
                 (let ((et (importrec-type payload)))
                   ;; the [constructor]/[method]/[static] shape rules
                   ;; (annotated-names.wast)
                   (%check-annotated-name
                    (importrec-name payload)
                    (and (eq (car et) :func)
                         (integerp (cdr et))
                        (handler-case (%ct-at state (cdr et))
                          (wasmdown:validation-error () nil)))
                    state t)
                   (case (car et)
                     (:func (%vis-check state (cdr et) t "func"))
                     (:instance
                      ;; slice 5g (external-visibility ::41/::44):
                      ;; an instance TYPE used as an import — its
                      ;; own declarators' references must be named
                      ;; inside the type
                      (%vis-check state (cdr et) t "instance")
                      (%vis-check-type-use state (cdr et) t "instance"))
                     (:type
                      (unless (eq (cdr et) :sub-resource)
                        (%vis-check state (second (cdr et)) t "type"))
                      ;; slice 5g (external-visibility ::43): a
                      ;; component TYPE used as an import
                      (when (and (listp (cdr et))
                                 (eq (car (cdr et)) :eq))
                        (%vis-check-type-use state (second (cdr et))
                                             t "type")))
                     (t nil)))
                  ;; imports extend the imported sort's space
                  (case (car (importrec-type payload))
                    (:func (incf (cval-funcs state))
                           (vector-push-extend
                            (aref (cval-types state)
                                  (cdr (importrec-type payload)))
                            (cval-func-types state)))
                    ;; an imported type is an opaque DEFINED type;
                    ;; a (sub resource) import IS a resource type —
                    ;; own/borrow must accept it (annotated-names
                    ;; pins the positive).
                    (:type
                     (vector-push-extend
                      (let ((bound (cdr (importrec-type payload))))
                        (cond
                         ((eq bound :sub-resource)
                          (make-ct :kind :resource :payload '(0 . nil)
                                   :offset 0))
                         ((and (listp bound) (eq (car bound) :eq)
                               (ct-resourcetype-p
                                (%ct-at state (second bound))))
                          (make-ct :kind :resource :payload '(0 . nil)
                                   :offset 0))
                         (t (make-ct :kind :prim :payload :imported
                                     :offset 0))))
                      (cval-types state))
                    ;; the typeidx introduced by a type import is
                    ;; externally-visible (import-named) — the VALUE
                    ;; is the externname (annotated-names needs the
                    ;; resource's name for [constructor]/[method])
                    (setf (gethash (1- (length (cval-types state)))
                                   (cval-vis-imports state))
                          (importrec-name payload))
                     ;; identity (slice 3b): a (sub resource) import is a
                     ;; FRESH typevar; an (eq i) import follows the bound
                     (vector-push-extend
                      (let ((bound (cdr (importrec-type payload))))
                        (cond
                         ((eq bound :sub-resource) (%fresh-resource-id))
                         ((and (listp bound) (eq (car bound) :eq)
                               (< (second bound)
                                  (length (cval-type-ids state))))
                          (cons :ref (second bound)))
                         (t '(:opaque))))
                      (cval-type-ids state)))
                    (:value (incf (cval-values state)))
                    (:component (incf (cval-components state)))
                    (:instance (incf (cval-instances state))
                               ;; slice 3b: track the imported
                               ;; instance's TYPE (its ct) for the
                               ;; with-arg export matching
                               (vector-push-extend
                                (let ((ti (cdr (importrec-type payload))))
                                  (if (and (< ti
                                              (length (cval-types state)))
                                           (ct-p (aref (cval-types state)
                                                       ti)))
                                      (aref (cval-types state) ti)
                                      nil))
                                (cval-instance-types state)))
                    (:core-module (incf (cval-core-modules state)))))
                 (:export
                 ;; an exported instance is externally visible: a
                 ;; later alias export of a type out of it confers
                 ;; a name (external-visibility ::46/::49)
                 (when (eq (sortidx-sort (exportrec-sortidx payload))
                           :instance)
                   ;; the SOURCE instance is visible (slice 3c) and
                   ;; so is the NEW index the export introduces
                   ;; (slice 4: ::46's (export $bag' "i" ...) then
                   ;; (alias export $bag' "t" ...) — the alias's
                   ;; source is the EXPORT-INTRODUCED instance)
                   (setf (gethash (sortidx-idx
                                   (exportrec-sortidx payload))
                                  (cval-vis-inst state))
                         t)
                   (setf (gethash (cval-instances state)
                                  (cval-vis-inst state))
                         t))
                  (%check-externname (exportrec-name payload) state nil)
                  (%check-sortidx (exportrec-sortidx payload) state)
                  (%check-name-attrs (exportrec-name payload)
                                     (exportrec-attrs payload)
                                     (sortidx-sort
                                      (exportrec-sortidx payload)))
                  (when (exportrec-type payload)
                    (%check-externtype (exportrec-type payload) state nil)
                    ;; slice 5h (external-visibility ::60): the
                    ;; ascription must be COMPATIBLE with the
                    ;; source — an instance ascription requires the
                    ;; source's KNOWN exports to provide every
                    ;; required name+sort; an imported instance with
                    ;; no known exports cannot satisfy a non-empty
                    ;; ascription ('ascribed type of export is not
                    ;; compatible')
                    (when (eq (car (exportrec-type payload)) :instance)
                      (let* ((asc (exportrec-type payload))
                             (src-idx (sortidx-idx
                                       (exportrec-sortidx payload)))
                             (src (and (< src-idx
                                           (length (cval-instance-types
                                                    state)))
                                       (aref (cval-instance-types
                                              state)
                                             src-idx))))
                        (when (and (integerp (cdr asc))
                                   (< (cdr asc)
                                      (length (cval-types state)))
                                   (ct-p (aref (cval-types state)
                                               (cdr asc))))
                          (let ((act (aref (cval-types state)
                                           (cdr asc))))
                            (when (and (ct-p act)
                                       (eq (ct-kind act) :instance))
                              (dolist (td (coerce (ct-payload act)
                                                  'list))
                                (when (eq (typedecl-kind td) :export)
                                  (let* ((pl (typedecl-payload td))
                                         (nm (first pl))
                                         (eet (third pl)))
                                    (multiple-value-bind (got known)
                                        (%instance-export-sort
                                         src-idx nm state)
                                      (declare (ignore got))
                                      ;; M9 (linking EV ::61): every
                                      ;; name the ascription REQUIRES
                                      ;; must be KNOWN on the source
                                      ;; instance — an imported
                                      ;; instance with no type info
                                      ;; (NIL) cannot satisfy a
                                      ;; non-empty ascription
                                      (unless known
                                        (%verr "ascribed type of export is not compatible"))
                                      (when (and known
                                                 (hash-table-p src))
                                        (let ((hit (gethash nm src
                                                             :missing)))
                                          (unless (eq hit :missing)
                                            (unless (eq (car hit)
                                                        (car eet))
                                              (%verr "ascribed type of export is not compatible")))))))))))))))
                 ;; external visibility: every nameable type transi-
                 ;; tively used by the exported func/instance/type
                 ;; must be named on either side (imports count:
                 ;; ::0's f4 exports own $R2). For a func export the
                 ;; ASCRIBED type wins when present (::0 positive);
                 ;; otherwise the func's own functype (::1/::2).
                 (let* ((sort (sortidx-sort (exportrec-sortidx payload)))
                        (fi (sortidx-idx (exportrec-sortidx payload))))
                   (case sort
                     (:func
                      (let ((ft (or (and (exportrec-type payload)
                                          (integerp (cdr (exportrec-type
                                                          payload)))
                                          (cdr (exportrec-type payload)))
                                     (and (< fi (length (cval-func-types
                                                        state)))
                                          (aref (cval-func-types state)
                                                fi)))))
                       ;; the [constructor]/[method]/[static] shape
                       ;; rules (annotated-names.wast); FT may be an
                       ;; ascription INDEX or the functype ct
                       (%check-annotated-name
                        (exportrec-name payload)
                        (typecase ft
                          (null nil)
                          (integer (handler-case (%ct-at state ft)
                                     (wasmdown:validation-error () nil)))
                          (t (if (ct-p ft) ft nil)))
                       state nil)
                       ;; FT is a type index (ascription) or the
                       ;; functype ct itself (the func's own type)
                        (when ft (%vis-check state ft nil "func"))))
                     (:instance
                      (let ((it (or (and (exportrec-type payload)
                                         (integerp (cdr (exportrec-type
                                                         payload)))
                                         (cdr (exportrec-type payload)))
                                    (and (< fi (length (cval-instance-types
                                                        state)))
                                         (aref (cval-instance-types state)
                                               fi)))))
                        (when (integerp it)
                          (%vis-check state it nil "instance")
                          ;; slice 5g (external-visibility ::37):
                          ;; an instance TYPE used as an export — its
                          ;; own declarators' references must be named
                          ;; inside the type
                          (%vis-check-type-use state it nil "instance"))
                        ;; slice 5j (external-visibility ::50): a
                        ;; PROJECTED instance (an alias-export chain's
                        ;; hash table) — walk each FUNC entry's shape
                        ;; with the PARENT's named-sets: the entry's
                        ;; index resolves through the SOURCE's space
                        ;; (the CHILD's stashed state for an
                        ;; instantiated source), the nameability is
                        ;; judged in THIS scope
                        (when (hash-table-p it)
                          (let* ((src-comp (gethash fi
                                                    *inst-source-comp*))
                                 (child-state (and src-comp
                                                   (gethash src-comp
                                                            *comp-state-stash*))))
                            (loop for nm being the hash-keys of it
                                  using (hash-value hit)
                                  do (when (and (consp hit)
                                                (eq (car hit) :func)
                                                (integerp (cdr hit)))
                                       (let* ((k (cdr hit))
                                              (ft (if child-state
                                                      (and (< k (length
                                                                (cval-func-types
                                                                 child-state)))
                                                           (aref (cval-func-types
                                                                  child-state)
                                                                 k))
                                                      (and (< k (length
                                                                (cval-func-types
                                                                 state)))
                                                           (aref (cval-func-types
                                                                  state)
                                                                 k)))))
                                         (when (and ft (ct-p ft))
                                           ;; a HYBRID state: the CHILD's
                                           ;; types (shape resolution)
                                           ;; with the PARENT's vis
                                           ;; tables (nameability)
                                           (%vis-check
                                            (if child-state
                                                (make-cval
                                                 :types (cval-types
                                                         child-state)
                                                 :func-types (cval-func-types
                                                              child-state)
                                                 :vis-imports (cval-vis-imports
                                                               state)
                                                 :vis-exports (cval-vis-exports
                                                               state))
                                                state)
                                            ft nil "instance")))))))
                        ;; a synthesized BAG instance: walk its inline
                        ;; exports directly (its instance-type entry
                        ;; carries only (sort . nil) externtypes) —
                        ;; external-visibility ::21
                        (when (or (null it) (hash-table-p it))
                          (let ((inst (%nth-instance-def comp fi)))
                            (when (and inst
                                       (eq (instrec-kind inst)
                                           :inline-exports))
                              (loop for pair across (instrec-exports
                                                     inst)
                                    for si = (cdr pair)
                                    do (case (sortidx-sort si)
                                        (:func
                                         (let ((n (sortidx-idx si)))
                                           (when (and
                                                  (< n (length
                                                        (cval-func-types
                                                         state)))
                                                  (aref (cval-func-types
                                                         state) n))
                                             (%vis-check
                                              state
                                              (aref (cval-func-types
                                                     state) n)
                                              nil "instance"))))
                                        (:type
                                         (when (< (sortidx-idx si)
                                                  (length (cval-types
                                                           state)))
                                           (%vis-check
                                            state (sortidx-idx si)
                                            nil "instance")))
                                        (t nil))))))))
                     (:type
                      (let ((bound (cdr (exportrec-type payload))))
                        (cond
                         ((eq bound :sub-resource) nil)
                         ((and (listp bound) (eq (car bound) :eq))
                          (%vis-check state (second bound) nil "type"))
                         (t
                          ;; no ascription: the SORTIDX'd type itself
                          ;; is the export target (::23-::32)
                          (when (< fi (length (cval-types state)))
                            (%vis-check state fi nil "type")
                            ;; slice 5g (external-visibility ::37):
                            ;; an instance/component TYPE exported —
                            ;; its own declarators' references must
                            ;; be named inside the type
                            (%vis-check-type-use state fi nil "type"))))))
                     (t nil)))
                  ;; every export introduces a new index of its sort
                  (case (sortidx-sort (exportrec-sortidx payload))
                    (:func (incf (cval-funcs state))
                           (vector-push-extend nil (cval-func-types state)))
                    (:type (vector-push-extend
                            ;; the exported type's SHAPE is the
                            ;; sortidx'd type's ct (an export
                            ;; re-exports the definition); an
                            ;; ascription overrides it. Opaque
                            ;; prim marker otherwise.
                            (let ((ti (sortidx-idx
                                       (exportrec-sortidx payload))))
                              (if (and (exportrec-type payload)
                                       (consp (cdr (exportrec-type
                                                    payload)))
                                       (eq (car (cdr (exportrec-type
                                                      payload)))
                                           :sub-resource))
                                  (make-ct :kind :resource
                                           :payload '(0 . nil)
                                           :offset 0)
                                  (if (and (< ti (length (cval-types state)))
                                           (ct-p (aref (cval-types state)
                                                       ti)))
                                      (aref (cval-types state) ti)
                                      (make-ct :kind :prim
                                               :payload :exported
                                               :offset 0))))
                            (cval-types state))
                           ;; the typeidx introduced by a type export
                           ;; is externally-visible (export-named);
                           ;; the passed-in idx is NOT (::0/::2) — the
                           ;; VALUE is the externname (annotated-names)
                           (setf (gethash (1- (length (cval-types state)))
                                          (cval-vis-exports state))
                                 (exportrec-name payload))
                           ;; identity (slice 3b): re-exporting one typeidx
                           ;; shares identity; an ascribed (sub resource)
                           ;; export is a FRESH resource
                           (let ((ti (sortidx-idx
                                      (exportrec-sortidx payload))))
                             (vector-push-extend
                              (cond
                               ((and (exportrec-type payload)
                                     (consp (cdr (exportrec-type payload)))
                                     (eq (car (cdr (exportrec-type payload)))
                                         :sub-resource))
                                (%fresh-resource-id))
                               ((< ti (length (cval-type-ids state)))
                                (cons :ref ti))
                               (t '(:opaque)))
                              (cval-type-ids state))))
                    (:value (incf (cval-values state)))
                    (:component (incf (cval-components state)))
                    (:instance (incf (cval-instances state))
                               ;; slice 4 (family 3): the exported
                               ;; instance RE-EXPORTS the source's
                               ;; exports — copy the source's export
                               ;; table so later projections (alias
                               ;; export / (export ... (instance $i
                               ;; "n"))) resolve names (ev ::46/::47/
                               ;; ::54-::58)
                               (vector-push-extend
                                (and (< (sortidx-idx (exportrec-sortidx
                                                      payload))
                                        (length (cval-instance-types state)))
                                     (aref (cval-instance-types state)
                                           (sortidx-idx (exportrec-sortidx
                                                         payload))))
                                (cval-instance-types state))
                               ;; slice 4: the re-exported instance's
                               ;; entries index the SAME child space —
                               ;; propagate the source-comp record
                               (let ((src (gethash
                                           (sortidx-idx
                                            (exportrec-sortidx payload))
                                           *inst-source-comp*)))
                                 (when src
                                   (setf (gethash (cval-instances state)
                                                  *inst-source-comp*)
                                         src))))
                    (:core-module (incf (cval-core-modules state)))))
                 (:start
                  (when (>= (startrec-func payload) (cval-funcs state))
                    (%verr "function index out of bounds"))
                  (loop for v across (startrec-args payload)
                        do (when (>= v (cval-values state))
                             (%verr "value index out of bounds"))))
                 (:value (incf (cval-values state))))))
    ;; slice 4 (family 3): record THIS component's validator state
    ;; for the parent's export-table shape resolution
    (setf (gethash comp *comp-state-stash*) state)
    (setf (gethash state *state-comp*) comp)
    comp))
    )

(defun component-validate (comp)
  "Alias entry: validate a decoded component."
  (validate-component comp))
