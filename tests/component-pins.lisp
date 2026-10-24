;;;; Component-layer unit pins (M7 slice 1): the decoder's preamble
;;;; discrimination, a defined-type decode round-trip, one positive +
;;;; one malformed from each of Binary.md's early sections, and the
;;;; validator's kebab/label/elem_size pins. The binary+validation
;;;; suites remain the end-to-end oracle (tools/run-component-suite.lisp);
;;;; these pins guard the primitives in `make test` (347 lane).

(defpackage #:wasmdown.test.component
  (:use #:cl #:wasmdown.test)
  (:export #:run-component-pins))

(in-package #:wasmdown.test.component)

;;; ---- byte helpers (a (component binary "...") payload builder) ----

(defun %hexb (&rest strs)
  "The byte vector of (component binary ...) string literals — the
spec's \\xx escapes, one byte per escape or ASCII char. Reuses the
wast runner's string-to-bytes decoder (same grammar as the suite)."
  (wasmdown.wast::%concat-bytes
   (mapcar (lambda (s)
             (wasmdown.wast::make-stoken :kind :string :text s))
           strs)))

(defun %u32b (n)
  "Minimal LEB128 of an unsigned integer."
  (loop with v = n with out = '()
        while (>= v 128)
        do (push (logior 128 (logand v 127)) out) (setf v (ash v -7))
        finally (push v out)
                (return (coerce (nreverse out) 'vector))))

(defun %sec (id content)
  "A component section: id byte + u32 size + content."
  (concatenate '(vector (unsigned-byte 8))
               (vector id) (%u32b (length content)) content))

(defun %comp (&rest sections)
  "A component binary: preamble + sections."
  (apply #'concatenate '(vector (unsigned-byte 8))
         (%hexb "\\00asm\\0d\\00\\01\\00")
         sections))

(defun %type-sec (&rest strs)
  "A type section built from hex-string parts."
  (%sec 7 (apply #'%hexb strs)))

;;; ---- pins ----

(test component/preamble-discrimination
  ;; magic + version 0x0D + layer 0x01 = component (Binary.md 22-27);
  ;; the CORE preamble (version 0x01 / layer 0x00) is NOT one.
  (is (wasmdown.component:component-p (%hexb "\\00asm\\0d\\00\\01\\00")))
  (is (not (wasmdown.component:component-p (%hexb "\\00asm\\01\\00\\00\\00"))))
  (is (not (wasmdown.component:component-p (%hexb "\\00asm\\0c\\00\\01\\00"))))
  ;; truncated inputs are not components (component-p is shape-only)
  (is (not (wasmdown.component:component-p (%hexb "\\00asm\\0d\\00\\01"))))
  ;; decode: the good preamble decodes to zero defs; each bad preamble
  ;; byte is a decode error at its exact offset
  (is (= 0 (length (wasmdown.component::comp-defs
                    (wasmdown.component:decode-component
                     (%hexb "\\00asm\\0d\\00\\01\\00"))))))
  (macrolet ((malformed-at (off bytes)
               `(let ((err (catch 'err)))
                  (declare (ignorable err))
                  (block nil
                    (handler-case
                        (wasmdown.component:decode-component ,bytes)
                      (wasmdown:decode-error (c)
                        (return (wasmdown:decode-error-offset c)))))
                  (error "expected decode error"))))
    (declare (ignore #'malformed-at)))
  (flet ((expect-decode-error (bytes)
           (block nil
             (handler-case
                 (wasmdown.component:decode-component bytes)
               (wasmdown:decode-error (c) (return c)))
             (error "expected wasmdown:decode-error"))))
    ;; wrong magic
    (let ((c (expect-decode-error (%hexb "asm\\00\\0d\\00\\01\\00"))))
      (is (= 0 (wasmdown:decode-error-offset c))))
    ;; wrong version (0x0c < 0x0d)
    (let ((c (expect-decode-error (%hexb "\\00asm\\0c\\00\\01\\00"))))
      (is (= 4 (wasmdown:decode-error-offset c))))
    ;; wrong layer (0x02)
    (let ((c (expect-decode-error (%hexb "\\00asm\\0d\\00\\02\\00"))))
      (is (= 6 (wasmdown:decode-error-offset c))))
    ;; truncated preamble
    (let ((c (expect-decode-error (%hexb "\\00asm\\0d\\00\\01"))))
      (is (= 0 (wasmdown:decode-error-offset c))))))

(test component/defined-type-roundtrip
  ;; the 19-type fixture shape (binary.wast ::557-594): every defined
  ;; type constructor decodes to the right kind, in order.
  (let* ((bin (%comp
               (%type-sec
                "\\13"
                "\\3f\\7f\\00"
                "\\72\\02" "\\01a" "\\7f" "\\01b" "\\7d"
                "\\71\\02" "\\01x" "\\01\\7e" "\\00" "\\01y" "\\00" "\\00"
                "\\70\\7b"
                "\\6f\\02\\7c\\79"
                "\\6e\\02" "\\02f1" "\\02f2"
                "\\6d\\02" "\\02e1" "\\02e2"
                "\\6b\\7a"
                "\\6a\\00\\00" "\\6a\\01\\77\\00" "\\6a\\00\\01\\78"
                "\\6a\\01\\76\\01\\75"
                "\\69\\00" "\\68\\00"
                "\\66\\01\\7d" "\\66\\00"
                "\\65\\01\\73" "\\65\\00"
                "\\70\\02")))
         (comp (wasmdown.component:decode-component bin))
         (kinds (map 'list
                     (lambda (d)
                       (wasmdown.component::ct-kind (wasmdown.component::compdef-payload d)))
                     (wasmdown.component::comp-defs comp))))
    (is (equal '(:resource :record :variant :list :tuple :flags :enum
                 :option :result :result :result :result :own :borrow
                 :stream :stream :future :future :list)
               kinds))
    ;; and it validates (elem_size < 2^28 for every defined type)
    (wasmdown.component:validate-component comp)))

(test component/section-positive-and-malformed
  ;; one positive + one malformed from the early sections (preamble,
  ;; customs, section ids, type, alias, instance, core instance).
  (flet ((expect-decode-error (bytes)
           (block nil
             (handler-case
                 (wasmdown.component:decode-component bytes)
               (wasmdown:decode-error (c) (return c)))
             (error "expected wasmdown:decode-error"))))
    ;; POSITIVE: custom section with arbitrary payload bytes
    (let ((comp (wasmdown.component:decode-component
                 (%comp (%sec 0 (%hexb "\\02hi\\ff\\fe\\01"))))))
      (is (= 1 (length (wasmdown.component::comp-customs comp)))))
    ;; POSITIVE: empty type section
    (wasmdown.component:decode-component (%comp (%type-sec "\\00")))
    ;; MALFORMED: section id 13
    (let ((c (expect-decode-error (%comp (%sec 13 (%hexb "\\00"))))))
      (is (search "malformed section id"
                  (wasmdown:wasmdown-error-message c))))
    ;; MALFORMED: custom section whose name overruns its content
    (let ((c (expect-decode-error
               (%comp (%sec 0 (%hexb "\\05\\61\\62"))))))
      (is (search "name" (wasmdown:wasmdown-error-message c))))
    ;; MALFORMED: an unallocated type opcode (0x62)
    (let ((c (expect-decode-error (%comp (%type-sec "\\01\\62")))))
      (is (search "invalid leading byte (0x62) for component defined type"
                  (wasmdown:wasmdown-error-message c))))
    ;; MALFORMED: alias target 0x03
    (let ((c (expect-decode-error
               (%comp (%type-sec "\\01\\73")
                      (%sec 6 (%hexb "\\01\\03\\03\\00\\00"))))))
      (is (search "invalid leading byte (0x3) for alias"
                  (wasmdown:wasmdown-error-message c))))
    ;; MALFORMED: instanceexpr 0x02
    (let ((c (expect-decode-error
               (%comp (%sec 5 (%hexb "\\01\\02\\00\\00"))))))
      (is (search "invalid leading byte (0x2) for instance"
                  (wasmdown:wasmdown-error-message c))))
    ;; MALFORMED: core instanceexpr 0x02
    (let ((c (expect-decode-error
               (%comp (%sec 2 (%hexb "\\01\\02\\00\\00"))))))
      (is (search "invalid leading byte (0x2) for core instance"
                  (wasmdown:wasmdown-error-message c))))))

(test component/validator-label-pins
  ;; kebab grammar (kebab.wast): first fragment starts with a letter,
  ;; later fragments may be digits-only; aBc-style mixes are labels
  ;; (they are only VALIDATION failures when compared case-blind)
  (is (wasmdown.component:label-p "a"))
  (is (wasmdown.component:label-p "a1"))
  (is (wasmdown.component:label-p "B1"))
  (is (wasmdown.component:label-p "a-1-b-2-c-3"))
  (is (wasmdown.component:label-p "a11-B11-123-ABC-abc"))
  (is (not (wasmdown.component:label-p "1")))
  (is (not (wasmdown.component:label-p "1-a")))
  (is (not (wasmdown.component:label-p "")))
  (is (not (wasmdown.component:label-p "a-")))
  (is (not (wasmdown.component:label-p "a--")))
  ;; labels: non-empty + kebab + strongly-unique are VALIDATION rules
  (flet ((expect-invalid (bin)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component:decode-component bin))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; empty record field label
    (let ((c (expect-invalid (%comp (%type-sec "\\01\\72\\01\\00\\7f")))))
      (is (search "record field name cannot be empty"
                  (wasmdown:wasmdown-error-message c))))
    ;; duplicate (case-insensitive) variant cases
    (let ((c (expect-invalid
               (%comp (%type-sec
                       "\\01\\71\\02\\01x\\01\\7a\\00\\01X\\01\\7a\\00")))))
      (is (search "variant case name `X` conflicts"
                  (wasmdown:wasmdown-error-message c))))
    ;; 33 flags (the label bytes built explicitly)
    ;; 33 flags (explicit build)
    (let* ((names (loop for i below 33
                        append (append (list 3)
                                        (map 'list #'char-code
                                             (format nil "f~2,'0d" i)))))
           (content (concatenate '(vector (unsigned-byte 8))
                                 (%hexb "\\01\\6e\\21")
                                 (coerce names 'vector))))
      (let ((c (expect-invalid (%comp (%sec 7 content)))))
        (is (search "cannot have more than 32 flags"
                    (wasmdown:wasmdown-error-message c)))))
    ;; elem_size: (list u8 268435456) exceeds 2^28
    (let ((c (expect-invalid
               (%comp (%type-sec "\\01\\67\\7d\\80\\80\\80\\80\\01")))))
      (is (search "exceeds maximum byte size"
                  (wasmdown:wasmdown-error-message c))))
    ;; (list u8 268435455) is exactly at the bound — VALID
    (wasmdown.component:validate-component
     (wasmdown.component:decode-component
      (%comp (%type-sec "\\01\\67\\7d\\ff\\ff\\ff\\ff\\00"))))))

;;; ---- M7 slice 2 pins: canonopt rules + the TEXT grammar ----

(test component/canonopt-memory-rules
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; (canon lower ...) of an 8-u64-tuple result: flat results 8 > 1
    ;; so memory is REQUIRED (abi.wast).
    (let ((src "(component (import \"foo\" (func $foo (result (tuple u64 u64 u64 u64)))) (canon lower (func $foo) (core func $foo')))"))
      (let ((c (expect-invalid
                (lambda ()
                  (wasmdown.component:validate-component
                   (wasmdown.component.text::parse-component-text src))))))
        (is (search "canonical option `memory` is required"
                    (wasmdown:wasmdown-error-message c)))))
    ;; realloc without memory: the exact abi.wast message
    (let ((src "(component (import \"foo\" (func $foo)) (canon lower (func $foo) (realloc (core func 0)) (core func $foo')))"))
      (let ((c (expect-invalid
                (lambda ()
                  (wasmdown.component:validate-component
                   (wasmdown.component.text::parse-component-text src))))))
        (is (search "canonical option `realloc` requires `memory` to also be specified"
                    (wasmdown:wasmdown-error-message c)))))
    ;; small-result lower with memory+realloc: VALID
    (let ((src "(component (import \"foo\" (func $foo (param \"x\" u32) (result u32))) (core module $M (memory (export \"mem\") 1) (func (export \"realloc\") (param i32 i32 i32 i32) (result i32) unreachable)) (core instance $i (instantiate $M)) (canon lower (func $foo) (memory (core memory $i \"mem\")) (realloc (core func $i \"realloc\")) (core func $foo')))"))
      (wasmdown.component:validate-component
       (wasmdown.component.text::parse-component-text src)))
    ;; inverted lift, list param, no memory: memory message (abi.wast)
    (let ((src "(component (core module $M (func (export \"f\") (param i32 i32))) (core instance $i (instantiate $M)) (func $f (param \"p\" (list u8)) (canon lift (core func $i \"f\"))))"))
      (let ((c (expect-invalid
                (lambda ()
                  (wasmdown.component:validate-component
                   (wasmdown.component.text::parse-component-text src))))))
        (is (search "canonical option `memory` is required"
                    (wasmdown:wasmdown-error-message c)))))))

(test component/externname-grammar-pins
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; interface names: namespace/package use the STRICT words
    ;; grammar (lowercase-start) — kebab.wast's `A:b/c`, `1:b/c`
    (is (wasmdown.component:words-p "ns"))
    (is (wasmdown.component:words-p "ns-pkg"))
    (is (not (wasmdown.component:words-p "A")))
    (is (not (wasmdown.component:words-p "1")))
    (is (not (wasmdown.component:words-p "ns-A")))
    ;; the strongly-unique reduction: [method]foo.foo -> foo (conflict),
    ;; [method]foo.bar keeps its identity (no conflict)
    (is (string= "foo"
                 (wasmdown.component:remove-annotations "[method]foo.foo")))
    (is (string= "foo.bar"
                 (wasmdown.component:remove-annotations "[method]foo.bar")))
    (is (string= "[constructor]foo"
                 (wasmdown.component:remove-annotations
                  "[constructor]foo")))
    (is (string= "foo" (wasmdown.component:remove-annotations "foo")))
    ;; the inline-export names validate: `1` is not a kebab label
    (let ((src "(component (import \"f\" (func $f)) (instance (export \"1\" (func $f))))"))
      (let ((c (expect-invalid
                (lambda ()
                  (wasmdown.component:validate-component
                   (wasmdown.component.text::parse-component-text src))))))
        (is (search "is not in kebab case"
                    (wasmdown:wasmdown-error-message c)))))))




(test component/shape-family-pins
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (expect-decode-error (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:decode-error (c) (return c)))
             (error "expected wasmdown:decode-error")))
         (parse-ok (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; elem-size's variant arm: an all-empty-payload variant must not
    ;; divide by zero (external-visibility.wast ::15's DIVISION crash)
    (parse-ok "(component (type $V (variant (case \"a\") (case \"b\"))))")
    (parse-ok (concatenate 'string "(component"
                " (core module $m (func (export \"f\") (result i32) unreachable))"
                " (core instance $i (instantiate $m))"
                " (type $V (variant (case \"a\") (case \"b\")))"
                " (func $f (result $V) (canon lift (core func $i \"f\")))"
                " (export \"f\" (func $f)))"))
    ;; a NONEMPTY variant still elem-sizes (ma > 1 path)
    (parse-ok (concatenate 'string "(component"
                " (core module $m (func (export \"f\") (result i32) unreachable))"
                " (core instance $i (instantiate $m))"
                " (type $V (variant (case \"a\" u32) (case \"b\")))"
                " (func $f (result $V) (canon lift (core func $i \"f\"))))"))
    ;; the (core module ...) externtype interns a core type; the $id
    ;; binds in the core-module space (indicies.wast ::14's MODULETYPE
    ;; crash + the (with \"x\" (core module $m)) projection)
    (parse-ok (concatenate 'string "(component"
                " (import \"m\" (core module $m))"
                " (import \"C\" (component $c))"
                " (instance (instantiate $c (with \"x\" (core module $m)))))"))
    ;; an inline moduletype import: the empty decl vector reaches the
    ;; validator as a (:moduletype . #()) core type
    (parse-ok "(component (import \"a\" (core module)))")
    ;; the inverted canon forms: (core func $d (canon resource.drop
    ;; $T)) has NO trailing (core func ...) target (resources.wast
    ;; ::69, abi.wast ::3's 'expected (core func ...)' family)
    (parse-ok (concatenate 'string "(component"
                " (type $T (resource (rep i32)))"
                " (core func $d (canon resource.drop $T)))"))
    (parse-ok (concatenate 'string "(component"
                " (import \"f\" (func $f))"
                " (core func $f' (canon lower (func $f))))"))
    ;; (alias core export ... (core func $f)) binds $f in the CORE
    ;; func space, and canon lift then resolves it (indicies.wast ::4)
    (parse-ok (concatenate 'string "(component"
                " (core module $M (func (export \"f\")))"
                " (core instance $m (instantiate $M))"
                " (alias core export $m \"f\" (core func $f))"
                " (canon lift (core func $f) (func)))"))
    ;; the (with \"x\" (instance $j \"k\")) projection chain: the atom
    ;; + export-name shape is an alias chain, NOT an inline-export bag
    ;; — it must PARSE (indicies.wast ::0's STOKEN crash was a DECODE
    ;; TYPE-ERROR, fixed at the parser level; the validator's
    ;; alias-sortidx bounds check is the next slice's work)
    (wasmdown.component.text::parse-component-text
     (concatenate 'string "(component"
       " (import \"j\" (instance $j (export \"k\" (instance))))"
       " (import \"C\" (component $c))"
       " (instance (instantiate $c (with \"x\" (instance $j \"k\")))))"))
    ;; %bind: a $id reused across SORTS is legal (instantiation.wast
    ;; ::51's $libc is both a core module and a core instance); a
    ;; same-sort collision still errors at decode
    (parse-ok (concatenate 'string "(component"
                " (core module $libc (func (export \"f\")))"
                " (core instance $libc (instantiate $libc)))"))
    (let ((c (expect-decode-error
              (lambda ()
                (wasmdown.component.text::parse-component-text
                 "(component (type $t u32) (type $t u32))")))))
      (is (search "duplicate identifier"
                  (wasmdown:wasmdown-error-message c))))))


(test component/kebab-fold-pins
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; Explainer Name Uniqueness 1.1 (the fold) applies BEFORE the
    ;; 1.2 reduction: [static]foo-bar.FO-ob-AR's folded labels match,
    ;; so it reduces to foo-bar and CONFLICTS with the plain import
    ;; (kebab.wast ::34); [method]foo-bar.foobar ditto (::35)
    (is (string= "foo-bar"
                 (wasmdown.component:remove-annotations
                  "[static]foo-bar.FO-ob-AR")))
    ;; distinct folded labels keep their identity: no conflict
    (is (string= "foo-bar.bazbar"
                 (wasmdown.component:remove-annotations
                  "[method]foo-bar.bazbar")))
    (let ((src (concatenate 'string "(component"
                             " (import \"foo-bar\" (type (sub resource)))"
                             " (import \"[static]foo-bar.FO-ob-AR\" (func)))")))
      (let ((c (expect-invalid
                (lambda ()
                  (wasmdown.component:validate-component
                   (wasmdown.component.text::parse-component-text src))))))
        (is (search "conflicts with previous name"
                    (wasmdown:wasmdown-error-message c)))))
    (let ((src (concatenate 'string "(component"
                             " (import \"foo-bar\" (type (sub resource)))"
                             " (import \"[method]foo-bar.foobar\" (func)))")))
      (let ((c (expect-invalid
                (lambda ()
                  (wasmdown.component:validate-component
                   (wasmdown.component.text::parse-component-text src))))))
        (is (search "conflicts with previous name"
                    (wasmdown:wasmdown-error-message c)))))
    ;; the control: a genuinely distinct second label stays OK
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      (concatenate 'string "(component"
        " (import \"foo-bar\" (type (sub resource)))"
        " (import \"[method]foo-bar.bazbar\" (func)))")))))

(test component/version-grammar-pins
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; ---- the canonversion production (Explainer 2787-2791) ----
    (is (wasmdown.component:canonversion-p "1"))
    (is (wasmdown.component:canonversion-p "10"))
    (is (wasmdown.component:canonversion-p "0.2"))
    (is (wasmdown.component:canonversion-p "0.0.12"))
    (is (wasmdown.component:canonversion-p "0.0.0"))
    (is (wasmdown.component:canonversion-p "0.0.1-alpha"))
    (is (wasmdown.component:canonversion-p "1.2.3-alpha.1"))
    (is (not (wasmdown.component:canonversion-p "0")))
    (is (not (wasmdown.component:canonversion-p "01")))
    (is (not (wasmdown.component:canonversion-p "0.0")))
    (is (not (wasmdown.component:canonversion-p "0.02")))
    (is (not (wasmdown.component:canonversion-p "1.2")))
    (is (not (wasmdown.component:canonversion-p "x")))
    ;; ---- the canonical split (Explainer 2983-3001; the
    ;; major-split, minor-split, 0.0.z, pre-release and build cases;
    ;; non-semver inputs split to (nil nil)) ----
    (flet ((split (v)
             (multiple-value-bind (canon suffix)
                 (wasmdown.component:split-canonical-version v)
               (list canon suffix))))
      (is (equal '("1" ".2.3") (split "1.2.3")))
      (is (equal '("1" ".2.3+sha.5114f85") (split "1.2.3+sha.5114f85")))
      (is (equal '("0.2" ".6") (split "0.2.6")))
      (is (equal '("0.2" ".6+sha.5114f85") (split "0.2.6+sha.5114f85")))
      (is (equal '("0.0.1" nil) (split "0.0.1")))
      (is (equal '("0.0.1" "+sha.5114f85") (split "0.0.1+sha.5114f85")))
      (is (equal '("0.0.12" "+b") (split "0.0.12+b")))
      (is (equal '("1.2.3-alpha" nil) (split "1.2.3-alpha")))
      (is (equal '("1.2.3-alpha.1" "+sha.5114") (split "1.2.3-alpha.1+sha.5114")))
      (is (equal '("0.2.6-rc.1" "+b") (split "0.2.6-rc.1+b")))
      (is (equal '(nil nil) (split "1.2")))
      (is (equal '(nil nil) (split "0.1.0-a+b-c+d"))))
    ;; ---- end-to-end: the version-suffix attribute rules
    ;; (version-suffix.wast ::1-::33, extern-names.wast's @ rows) ----
    ;; positive: @1 + .2.3 reconstructs the canonical 1.2.3
    (is (valid "(component (import \"a:b/c@1\" (version-suffix \".2.3\") (instance)))"))
    ;; a pre-release can't be split off the name's canonversion
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"a:b/c@1\" (version-suffix \".2.3-alpha\") (instance)))")))))
      (is (search "is not the canonical version of `1.2.3-alpha`"
                  (wasmdown:wasmdown-error-message c))))
    ;; the empty suffix
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"a:b/c@0.0.1\" (version-suffix \"\") (instance)))")))))
      (is (search "version suffix cannot be empty"
                  (wasmdown:wasmdown-error-message c))))
    ;; a bare canonversion name needs a suffix (::11)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"a:b/c@1\" (instance)))")))))
      (is (search "invalid interface version"
                  (wasmdown:wasmdown-error-message c))))
    ;; the suffix applies to the implements interfacename (::29)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"a\" (implements \"a:b/c@1\") (instance)))")))))
      (is (search "invalid interface version"
                  (wasmdown:wasmdown-error-message c))))
    ;; only instances can have a version-suffix (::24)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"a:b/c@1\" (version-suffix \".2.3\") (func)))")))))
      (is (search "only instances can have a `version-suffix`"
                  (wasmdown:wasmdown-error-message c))))
    ;; the @version grammar in externnames (extern-names.wast ::2-::9)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"a:b/c@\" (func)))")))))
      (is (search "empty string" (wasmdown:wasmdown-error-message c))))
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"a:b/c@2.0.0+\" (func)))")))))
      (is (search "empty identifier segment"
                  (wasmdown:wasmdown-error-message c))))
    ;; the implements-attributed instancetype declarator shape (::28)
    (is (valid "(component (type (instance (export \"f\" (implements \"a:b/c@1\") (version-suffix \".2.3\") (instance)))))"))
    ;; a bag export may reuse a name the component also exports
    ;; (attributes.wast ::0: per-instance uniqueness scope)
    (is (valid "(component (import \"a\" (instance)) (instance $a) (export \"a\" (instance $a)) (instance (export \"a\" (instance $a))))"))
    ;; binary.wast ::80: a componenttype's exportdecl ascription
    ;; (func (type 2)) resolves against the TYPE's OWN declarator
    ;; sequence [type, import, type] — inner-first, outer-fallback
    (is (valid "(component (type (component (type (instance)) (import \"a\" (type (eq 0))) (type (func)) (export \"b\" (func (type 2))))))"))))

(defun run-component-pins ()
  "Run the component pins (registered via TEST; returns T)."
  (let ((wasmdown.test::*tests*
         (remove-if-not
          (lambda (e)
            (member (wasmdown.test::test-entry-name e)
                    '(component/preamble-discrimination
                      component/defined-type-roundtrip
                      component/section-positive-and-malformed
                      component/validator-label-pins
                      component/canonopt-memory-rules
                      component/externname-grammar-pins
                      component/shape-family-pins
                      component/kebab-fold-pins
                      component/version-grammar-pins)))
          wasmdown.test::*tests*)))
    (multiple-value-bind (passed failed)
        (wasmdown.test:run-all-tests)
      (declare (ignore passed))
      (zerop failed))))
