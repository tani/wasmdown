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
                " (func $f (result $V) (canon lift (core func $i \"f\"))))"))
    ;; the same variant EXPORTED is invalid per the external-
    ;; visibility rule (external-visibility.wast ::13: an unnamed
    ;; variant used in an exported func) — the pin guards against
    ;; regressing the rule away
    (let ((c (expect-invalid
              (lambda ()
                (parse-ok (concatenate 'string "(component"
                            " (core module $m (func (export \"f\") (result i32) unreachable))"
                            " (core instance $i (instantiate $m))"
                            " (type $V (variant (case \"a\") (case \"b\")))"
                            " (func $f (result $V) (canon lift (core func $i \"f\")))"
                            " (export \"f\" (func $f)))"))))))
      (is (search "func not valid to be used as export"
                  (wasmdown:wasmdown-error-message c))))
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
    ;; the control: a genuinely distinct second label stays OK (the
    ;; [method] shape needs a first `self` (borrow $T) param —
    ;; kebab.wast ::35's form)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      (concatenate 'string "(component"
        " (import \"foo-bar\" (type $t (sub resource)))"
        " (import \"[method]foo-bar.bazbar\" (func (param \"self\" (borrow $t)))))")))))

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
    ;; sequence [type, import, type] — inner-first, outer-fallback.
    ;; The binary fixture's first decl is the PRIM type `string` (an
    ;; (eq 0) bound may target any defvaltype; `(instance)` was the
    ;; old error-swallowing shape that the honest re-signal rejects)
    (is (valid "(component (type (component (type string) (import \"a\" (type (eq 0))) (type (func)) (export \"b\" (func (type 2))))))"))))

(test component/fixed-list-alignment-pins
  ;; CanonicalABI.md alignment_list: a FIXED list aligns as its
  ;; element type, only a variable list aligns as a pointer.
  ;; max-value-size.wast ::0's tuple — 268435454*1 then +1 — must
  ;; total 268435455 < 2^28 (the ptr alignment computed 2^28+1 and
  ;; wrongly tripped 'exceeds maximum byte size' before this rule).
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; ::0's over-limit twin rows stay invalid at the same message
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (type (tuple (list u8 268435455) (list u8 1))))")))))
      (is (search "exceeds maximum byte size"
                  (wasmdown:wasmdown-error-message c))))
    ;; ...and the just-under row is valid (fixed-list align 1)
    (is (valid "(component (type (tuple (list u8 268435454) (list u8 1))))"))))

(test component/canonopt-unicity-pins
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; memory specified twice (abi.wast ::14)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"f\" (func $f)) (core func $f' (canon lower (func $f) (memory 0) (memory 0))))")))))
      (is (search "`memory` is specified more than once"
                  (wasmdown:wasmdown-error-message c))))
    ;; realloc specified twice (abi.wast ::15)
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                            " (core module $M (memory (export \"mem\") 1)"
                            " (func (export \"f\") (param i32 i32))"
                            " (func (export \"realloc\") (param i32 i32 i32 i32) (result i32) unreachable))"
                            " (core instance $i (instantiate $M))"
                            " (func $f (param \"p\" (list u8))"
                            " (canon lift (core func $i \"f\")"
                            " (memory (core memory $i \"mem\"))"
                            " (realloc (core func $i \"realloc\"))"
                            " (realloc (core func $i \"realloc\")))))"))))))
      (is (search "canonical option `realloc` is specified more than once"
                  (wasmdown:wasmdown-error-message c))))
    ;; memory index out of bounds (abi.wast ::3)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"f\" (func $f)) (core func $f' (canon lower (func $f) (memory 5))))")))))
      (is (search "memory index out of bounds"
                  (wasmdown:wasmdown-error-message c))))
    ;; post-return cannot be specified for lowerings (abi.wast ::19)
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                            " (import \"f\" (func $f (param \"p\" string)))"
                            " (core module $M (memory (export \"mem\") 1)"
                            " (func (export \"realloc\") (param i32 i32 i32 i32) (result i32) unreachable)"
                            " (func (export \"post\") (param i32)))"
                            " (core instance $i (instantiate $M))"
                            " (core func $f' (canon lower (func $f)"
                            " (memory (core memory $i \"mem\"))"
                            " (realloc (core func $i \"realloc\"))"
                            " (post-return (core func $i \"post\")))))"))))))
      (is (search "cannot be specified for lowerings"
                  (wasmdown:wasmdown-error-message c))))))

(test component/core-module-rule-pins
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; the embedded core module's CODE is core-validated inside a
    ;; component (core-modules.wast ::1)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (core module (func i32.add)))")))))
      (is (search "operand stack underflow"
                  (wasmdown:wasmdown-error-message c))))
    ;; mangled two-level import names are unique (::7/::8)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (core module (import \"\" \"a\" (func)) (import \"\" \"a\" (func))))")))))
      (is (search "duplicate import name `:a`"
                  (wasmdown:wasmdown-error-message c))))
    ;; moduletype internals: duplicate export names (::3)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (core type (module (export \"a\" (func)) (export \"a\" (func)))))")))))
      (is (search "export name `a` already defined"
                  (wasmdown:wasmdown-error-message c))))
    ;; moduletype memory limits (::4)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (core type (module (import \"\" \"\" (memory 70000)))))")))))
      (is (search "memory size must be at most 65536"
                  (wasmdown:wasmdown-error-message c))))
    ;; moduletype duplicate mangled import names (::9/::10)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (core type (module (import \"\" \"\" (func)) (import \"\" \"\" (func)))))")))))
      (is (search "duplicate import name `:`"
                  (wasmdown:wasmdown-error-message c))))))

(test component/annotated-name-shape-pins
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; [constructor] must return exactly (own $T)
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"[constructor]a\" (func)))")))))
      (is (search "should return one value"
                  (wasmdown:wasmdown-error-message c))))
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"[constructor]a\" (func (result u32))))")))))
      (is (search "should return `(own $T)`"
                  (wasmdown:wasmdown-error-message c))))
    ;; [method] needs a first `self` (borrow $T) param
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"[method]a.b\" (func (param \"x\" u32))))")))))
      (is (search "should have a first argument called `self`"
                  (wasmdown:wasmdown-error-message c))))
    (let ((c (expect-invalid
              (lambda ()
                (valid "(component (import \"[method]a.b\" (func (param \"self\" u32))))")))))
      (is (search "should take a first argument of `(borrow $T)`"
                  (wasmdown:wasmdown-error-message c))))
    ;; the positives (annotated-names ::0/::8/::19)
    (is (valid (concatenate 'string "(component"
                " (import \"a\" (type $a (sub resource)))"
                " (import \"[constructor]a\" (func (result (own $a)))))")))
    (is (valid (concatenate 'string "(component"
                " (import \"a\" (type $T (sub resource)))"
                " (import \"[method]a.b\" (func (param \"self\" (borrow $T)))))")))
    (is (valid (concatenate 'string "(component"
                " (import \"a\" (type (sub resource)))"
                " (import \"[static]a.b\" (func (param \"x\" u32) (result string))))")))))

(test component/instantiate-matching-pins
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; prim mismatch through an (eq i) import (instantiation.wast ::1)
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (component $c"
                  "   (type $t (tuple u32 u32))"
                  "   (import \"x\" (type (eq $t))))"
                  " (type $x (tuple string string))"
                  " (instance (instantiate $c (with \"x\" (type $x)))))"))))))
      (is (search "expected primitive `u32` found primitive `string`"
                  (wasmdown:wasmdown-error-message c))))
    ;; record field mismatch (instantiation.wast ::5)
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (component $c"
                  "   (type $f (option s32))"
                  "   (type $t (record (field \"x\" $f)))"
                  "   (import \"x\" (type (eq $t))))"
                  " (type $x (record (field \"x\" u32)))"
                  " (instance (instantiate $c (with \"x\" (type $x)))))"))))))
      (is (search "type mismatch in record field `x`"
                  (wasmdown:wasmdown-error-message c))))
    ;; variant case mismatch (instantiation.wast ::12)
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (component $c"
                  "   (type $t (variant (case \"x\" u32)))"
                  "   (import \"x\" (type (eq $t))))"
                  " (type $x (variant (case \"x\" s32)))"
                  " (instance (instantiate $c (with \"x\" (type $x)))))"))))))
      (is (search "type mismatch in variant case `x`"
                  (wasmdown:wasmdown-error-message c))))
    ;; eq-substitution resource identity (resources.wast ::0): the
    ;; (eq $a) import's argument must be the SAME resource as $a's
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (import \"T1\" (type $T1 (sub resource)))"
                  " (import \"T2\" (type $T2 (sub resource)))"
                  " (component $eq"
                  "   (import \"a\" (type $a (sub resource)))"
                  "   (import \"b\" (type (eq $a))))"
                  " (instance (instantiate $eq"
                  "   (with \"a\" (type $T1))"
                  "   (with \"b\" (type $T2)))))"))))))
      (is (search "resource types are not the same"
                  (wasmdown:wasmdown-error-message c))))
    ;; positive: a matching with-arg instantiation validates
    (is (valid (concatenate 'string "(component"
                " (import \"T1\" (type $T1 (sub resource)))"
                " (component $eq"
                "   (import \"a\" (type $a (sub resource)))"
                "   (import \"b\" (type (eq $a))))"
                " (instance (instantiate $eq"
                "   (with \"a\" (type $T1))"
                "   (with \"b\" (type $T1)))))")))
    ;; func parameter-name mismatch (instantiation.wast ::27)
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (import \"f\" (func $f (param \"x\" u32)))"
                  " (component $c (import \"f\" (func (param \"y\" u32))))"
                  " (instance (instantiate $c (with \"f\" (func $f)))))"))))))
      (is (search "expected parameter named `y`, found `x`"
                  (wasmdown:wasmdown-error-message c))))))


(test component/core-module-empty-body-pin
  ;; An EMPTY (core module $id?) body slices to an EMPTY core module:
  ;; the old span math grabbed the char before ')' so "(module m)"
  ;; hit 'unknown module field' at DECODE (instantiation.wast
  ;; ::35/::56/::78, outer-alias.wast ::16, indicies.wast ::15).
  (flet ((valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    (valid "(component (core module $m))")
    (valid "(component (core module))")
    ;; a named module used as a with-arg: the module must SUPPLY the
    ;; declared moduletype's exports (the ::35 shape with the export
    ;; PRESENT — the empty module fails 'missing expected export `x`'
    ;; now that family 1b's covariant check is implemented)
    (valid (concatenate 'string "(component"
                " (core module $m (func (export \"x\")))"
                " (component $c (import \"m\" (core module (export \"x\" (func)))))"
                " (instance (instantiate $c (with \"m\" (core module $m)))))"))
    ;; regression guard: a NON-empty body still slices right
    (valid "(component (core module $m (func (export \"f\"))))")))

(test component/externtype-type-use-pins
  ;; (Explainer line 650) externtype ::= (<sort> (type <idx>)) for
  ;; EVERY sort: an (instance (type $I)) / (component (type $C))
  ;; declarator is an out-of-line type-USE, never a declarator list
  ;; (defined-types ::41-::46, external-visibility ::41,
  ;; annotated-names ::30); a (core module (type $t)) use resolves in
  ;; the CORE type space; and an implicit outer alias fired inside a
  ;; component/instance TYPE's declarators belongs to THAT type's
  ;; own declarator sequence (its de Bruijn count skips the type
  ;; boundary).
  (flet ((invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; a FUNC type used via (instance (type $t)) (defined-types ::41)
    (let ((c (invalid "(component (type $t (func)) (import \"a\" (instance (type $t))))")))
      (is (search "type index 0 is not an instance type"
                  (wasmdown:wasmdown-error-message c))))
    ;; nested in a component type (::42): the implicit outer alias
    ;; lands inside the type's own declarator sequence
    (let ((c (invalid "(component (type $t (func)) (type (component (import \"a\" (instance (type $t))))))")))
      (is (search "type index 0 is not an instance type"
                  (wasmdown:wasmdown-error-message c))))
    ;; a CORE functype used via (core module (type $t)) (::44)
    (let ((c (invalid "(component (core type $t (func)) (import \"a\" (core module (type $t))))")))
      (is (search "core type index 0 is not a module type"
                  (wasmdown:wasmdown-error-message c))))
    ;; positives: an instance type via a type-use; an inline
    ;; declarator list; a doubly-nested type whose inline (func)
    ;; interns in the FLUSHING scope (annotated-names ::30)
    (valid "(component (type $I (instance (export \"f\" (func)))) (import \"a\" (instance (type $I))))")
    (valid "(component (import \"a\" (instance (export \"f\" (func)))))")
    (valid "(component (type (component (type $it (instance (export \"bar\" (type (sub resource))) (export \"[static]bar.a\" (func)))) (export \"x\" (instance (type $it))))))")))

(test component/declarator-bind-id-pins
  ;; A component/instance TYPE's import/export declarators introduce
  ;; a type index (Binary.md line 274) AND bind their bind-id at the
  ;; SORT-MATCHING space: an (export "r" (type $r (sub resource)))
  ;; inside an instance type binds $r for the following declarators
  ;; (resources.wast ::27/::28); an (import "i" (instance $i ...))
  ;; binds $i as an INSTANCE for the following (alias export $i ...)
  ;; (outer-alias.wast ::13/::14, the Explainer's $fs example). A
  ;; NESTED component is its own interning scope (its inline
  ;; externtypes intern into ITS type space — instantiation ::29's
  ;; (component $c (import "i" (instance (export "a" (func)))))).
  (flet ((valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; an export's $r binds and the own/borrow use resolves (the
    ;; declarator's (sub resource) index IS a resource type)
    (valid "(component (type $I (instance (export \"r\" (type $r (sub resource))) (export \"f\" (func (result (own $r)))))))")
    ;; an instance import's $i resolves as an instance by a following
    ;; alias-export declarator
    (valid (concatenate 'string "(component"
                " (type (component"
                "   (import \"i\" (instance $i (export \"t\" (type (sub resource)))))"
                "   (alias export $i \"t\" (type $t)))))"))
    ;; a nested component interns its inline externtypes in ITS OWN
    ;; space — the (func) resolves inside $c, never the root scope
    (valid (concatenate 'string "(component"
                " (import \"i\" (instance $i (export \"a\" (func))))"
                " (component $c (import \"i\" (instance (export \"a\" (func)))))"
                " (instance (instantiate $c (with \"i\" (instance $i)))))"))))


(test component/core-subtype-matching-pins
  ;; M7 slice 4 (family 1): the core-instance with-arg matcher
  ;; (instantiation.wast ::39-::51) — limits subtyping, the kind
  ;; mismatch and the missing-export messages.
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; ::44 'mismatch in table limits' — the provider's min is below
    ;; the declared import's min
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (core module $m1 (import \"\" \"t\" (table 2 2 funcref)))"
                  " (core module $m2 (table (export \"t\") 1 funcref))"
                  " (core instance $i (instantiate $m2))"
                  " (core instance (instantiate $m1"
                  " (with \"\" (instance $i)))))"))))))
      (is (search "mismatch in table limits"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::49 'expected global, found func'
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (core module $m1 (import \"a\" \"b\" (global i32)))"
                  " (core module $m2 (func (export \"b\")))"
                  " (core instance $i (instantiate $m2))"
                  " (core instance (instantiate $m1"
                  " (with \"a\" (instance $i)))))"))))))
      (is (search "expected global, found func"
                  (wasmdown:wasmdown-error-message c))))
    ;; a MATCHING instantiation validates (the positive)
    (valid (concatenate 'string "(component"
      " (core module $m1 (import \"\" \"g\" (global i32)))"
      " (core module $m2 (global (export \"g\") i32 (i32.const 0)))"
      " (core instance $i (instantiate $m2))"
      " (core instance (instantiate $m1 (with \"\" (instance $i)))))"))
    ;; ::33-family positive: a core module with-arg whose moduletype
    ;; matches contravariantly/covariantly
    (valid (concatenate 'string "(component"
      " (core module $m (func (export \"x\")))"
      " (component $c (import \"m\" (core module (export \"x\" (func)))))"
      " (instance (instantiate $c (with \"m\" (core module $m)))))"))))

(test component/canon-signature-pins
  ;; M7 slice 4 (family 2): the canon core-signature checks
  ;; (abi.wast ::17/::18/::20/::21).
  (flet ((expect-invalid (thunk)
           (block nil
             (handler-case (funcall thunk)
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error")))
         (valid (src)
           (wasmdown.component:validate-component
            (wasmdown.component.text::parse-component-text src))))
    ;; ::17 — realloc with the WRONG core signature
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (core module $M (memory (export \"mem\") 1)"
                  " (func (export \"f\") (param i32 i32))"
                  " (func (export \"realloc\")))"
                  " (core instance $i (instantiate $M))"
                  " (func $f (param \"p\" (list u8))"
                  " (canon lift (core func $i \"f\")"
                  " (memory (core memory $i \"mem\"))"
                  " (realloc (core func $i \"realloc\")))))"))))))
      (is (search "realloc` uses a core function with an incorrect signature"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::20 — the flatten compatibility check
    (let ((c (expect-invalid
              (lambda ()
                (valid (concatenate 'string "(component"
                  " (core module $M (func (export \"f\") (param i32)))"
                  " (core instance $i (instantiate $M))"
                  " (func $f (canon lift (core func $i \"f\"))))"))))))
      (is (search "lowered parameter types `[]` do not match parameter types `[I32]`"
                  (wasmdown:wasmdown-error-message c))))
    ;; the positive: a MATCHING flatten validates (ev ::16's shape)
    (valid (concatenate 'string "(component"
      " (core module $m (func (export \"f\") (param i32 i32 i32 i32)))"
      " (core instance $i (instantiate $m))"
      " (func $f (param \"a\" (tuple u32 u32)) (param \"b\" (option u32))"
      " (canon lift (core func $i \"f\"))))"))))

(test component/abi-string-encoding-pins
  ;; slice 5 (abi ::11-::13): the text parser ACCEPTS repeated
  ;; string-encoding opts (the 'string-encoding=' prefix match was
  ;; off by one: 16 chars, not 17 — a single option used to fall to
  ;; the error arm); the validator then rejects DIFFERENT encodings
  ;; with the fixture's conflict message, latin1+utf16 in kebab form.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; utf8 vs utf16 (abi ::11)
    (let ((c (expect-invalid
              "(component (import \"f\" (func $f)) (core func $f' (canon lower (func $f) string-encoding=utf8 string-encoding=utf16)))")))
      (is (search "canonical encoding option `utf8` conflicts with option `utf16`"
                  (wasmdown:wasmdown-error-message c))))
    ;; utf8 vs latin1+utf16 — the kebab spelling (abi ::12)
    (let ((c (expect-invalid
              "(component (import \"f\" (func $f)) (core func $f' (canon lower (func $f) string-encoding=utf8 string-encoding=latin1+utf16)))")))
      (is (search "canonical encoding option `utf8` conflicts with option `latin1-utf16`"
                  (wasmdown:wasmdown-error-message c))))
    ;; a SINGLE encoding still parses (the prefix fix's positive)
    (wasmdown.component.text::parse-component-text
     "(component (import \"f\" (func $f)) (core func $f' (canon lower (func $f) string-encoding=utf8)))")))

(test component/outer-alias-debruijn-pins
  ;; slice 5 (outer-alias A+B, outer-alias.wast ::0/::2-::4/::7/::8/::12):
  ;; the de Bruijn count is 0 = the CURRENT component/type, N = N
  ;; boundaries out — the NAMED-scope sugar resolves the (current .
  ;; outers) chain by POSITION (the old parser used the outers-only
  ;; list, so $C at the root was 'unknown outer scope' and one-level
  ;; aliases carried count 0, resolved vacuously); and a TYPE alias
  ;; crossing a component boundary may not transitively refer to a
  ;; resource (Binary.md 133-135).
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::0's named form at the ROOT: $C names the CURRENT scope
    ;; (count 0) — the root-level named aliases resolve and validate
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component definition $C (core module $M (func (export \"f\"))) (type $T (record (field \"x\" u32))) (alias outer $C $M (core module $cm)) (alias outer $C $T (type $ty)))"))
    ;; ::2 — a resource crossing a component boundary
    (let ((c (expect-invalid
              "(component $C (type $R (resource (rep i32))) (component (alias outer $C $R (type $a))))")))
      (is (search "transitively refers to resources"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::3 — a borrow of the resource, transitively
    (let ((c (expect-invalid
              "(component $C (type $R (resource (rep i32))) (type $B (borrow $R)) (component (alias outer $C $B (type $a))))")))
      (is (search "transitively refers to resources"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::4 — an own inside a record, transitively
    (let ((c (expect-invalid
              "(component $C (type $R (resource (rep i32))) (type $Rec (record (field \"h\" (own $R)))) (component (alias outer $C $Rec (type $a))))")))
      (is (search "transitively refers to resources"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::7 — a component TYPE whose exportdecl mentions (eq $R)
    (let ((c (expect-invalid
              "(component $C (type $R (resource (rep i32))) (type $U (component (export \"a\" (type (eq $R))))) (component (alias outer $C $U (type $a))))")))
      (is (search "transitively refers to resources"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::12 — explicit counts through TYPE boundaries resolve to the
    ;; root scope (count 1 = the enclosing component, count 2 = root)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component definition $C (type $T (record (field \"x\" u32))) (type (component (alias outer 1 0 (type $parent)) (type (component (alias outer 2 0 (type $grandparent)))))) (component (alias outer 1 0 (type $parent))) (component $D (alias outer $C $T (type $aD)) (component (alias outer $D $aD (type $aE)) (type $use (tuple $aE $aE)))))"))
    ;; ::11 — a resource-free type crosses fine; count 0 targets the
    ;; CURRENT scope (the inner component's own $R)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component definition $C (type $Pure (record (field \"x\" u32))) (component (alias outer $C $Pure (type $pure)) (type $R (resource (rep i32))) (alias outer 0 1 (type $localR))))"))
    ;; slice 5 (outer-alias C, Binary.md 267-270): alias DECLARATORS
    ;; inside a component/instance type only alias a SORT SUBSET —
    ;; export aliases: instance/type (::14's (func $a)); outer
    ;; aliases: core type/type (::15/::16/::17's component and core
    ;; module)
    (let ((c (expect-invalid
              "(component (type (component (import \"i\" (instance $i (export \"f\" (func)))) (alias export $i \"f\" (func $a)))))")))
      (is (search "may only refer to types or instances"
                  (wasmdown:wasmdown-error-message c))))
    (let ((c (expect-invalid
              "(component $C (component $D) (type (component (alias outer $C $D (component $a)))))")))
      (is (search "may only refer to types or instances"
                  (wasmdown:wasmdown-error-message c))))
    (let ((c (expect-invalid
              "(component $C (core module $M) (type (component (alias outer $C $M (core module $a)))))")))
      (is (search "may only refer to types or instances"
                  (wasmdown:wasmdown-error-message c))))
    ;; the positives: export aliases of instance/type and outer
    ;; aliases of core type/type inside types stay valid (::13)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component definition $C (type (component (import \"i\" (instance $i (export \"t\" (type (sub resource))) (export \"j\" (instance)))) (alias export $i \"t\" (type $t)) (alias export $i \"j\" (instance $j)))) (core type $FT (func (param i32) (result i32))) (type $T (record (field \"x\" u32))) (type (component (alias outer $C $FT (core type $ct)) (alias outer $C $T (type $t)))))")))
    ;; slice 5 (family D): a $id reused across sorts keeps EVERY
    ;; binding — resources.wast ::44's $c2 is an instance AND a
    ;; component; the (type $c2 \"y\") projection must resolve the
    ;; INSTANCE binding (the old single-entry table let the later
    ;; component bind shadow it -> 'unknown instance \$c2')
    (let ((c (wasmdown.component.text::parse-component-text
              "(component (type $t (resource (rep i32))) (component $c (import \"x\" (type $t (sub resource))) (export \"y\" (type $t))) (instance $c1 (instantiate $c (with \"x\" (type $t)))) (instance $c2 (instantiate $c (with \"x\" (type $t)))) (component $c2 (import \"x1\" (type $t (sub resource))) (import \"x2\" (type (eq $t))) (import \"x3\" (type (eq $t)))) (instance (instantiate $c2 (with \"x1\" (type $t)) (with \"x2\" (type $c1 \"y\")) (with \"x3\" (type $c2 \"y\")))))")))
      ;; the PARSE succeeds — the projection resolved $c2's instance
      ;; binding; the row's remaining failure is the validator's
      ;; resource-identity check (a separate family)
      (is (find :instance
                (map 'list #'wasmdown.component::compdef-kind
                     (wasmdown.component::comp-defs c))))))

(test component/moduletype-outer-alias-pins
  ;; slice 5 (family E, outer-alias.wast ::29 + core-modules ::0):
  ;; a moduletype's (alias outer $C $FT (type $a)) declarator accepts
  ;; the NAMED form (the old parser demanded bare nat ct/idx ->
  ;; 'bad alias declarator'), resolves $FT through the enclosing
  ;; scope, binds $a in the moduletype's OWN type space, and the
  ;; later (export "f" (func (type $a))) type-use resolves there too;
  ;; a named (type $ft (func)) similarly binds for later uses.
  (wasmdown.component:validate-component
   (wasmdown.component.text::parse-component-text
    "(component definition $C (core type $FT (func (param i32) (result i32))) (core type $MT (module (alias outer $C $FT (type $a)) (export \"f\" (func (type $a))))) (core type $MT2 (module (alias outer $C $FT (type)) (import \"a\" \"b\" (func (type 0))) (export \"c\" (func (type 0))))))")))


(test component/resource-identity-pins
  ;; slice 5c (resources ::30/::44/::45/::53-::56/::60-::64):
  ;; the alias-declarator type push (every (alias export ... (type))
  ;; inside a component/instance TYPE introduces a type index), the
  ;; per-instance identity stash (two instantiations of one component
  ;; with the same resource argument share the exported resource),
  ;; the borrow-in-result transitivity, the [i32]->[] destructor
  ;; signature and the resource-target canon rule.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::30 — the alias-introduced $r is referenceable by $T2's (eq $r)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (type $X (component (type $T (instance (export \"r\" (type (sub resource))))) (export \"t\" (instance $t (type $T))) (alias export $t \"r\" (type $r)) (type $T2 (instance (export \"r2\" (type (eq $r))) (export \"r\" (type (sub resource))))) (export \"t2\" (instance (type $T2))))))"))
    ;; ::44 — two instantiations of $c with the same $t: the eq-imports
    ;; x2/x3 both resolve to the SAME resource (no 'not the same')
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (type $t (resource (rep i32))) (component $c (import \"x\" (type $t (sub resource))) (export \"y\" (type $t))) (instance $c1 (instantiate $c (with \"x\" (type $t)))) (instance $c2 (instantiate $c (with \"x\" (type $t)))) (component $c2 (import \"x1\" (type $t (sub resource))) (import \"x2\" (type (eq $t))) (import \"x3\" (type (eq $t)))) (instance (instantiate $c2 (with \"x1\" (type $t)) (with \"x2\" (type $c1 \"y\")) (with \"x3\" (type $c2 \"y\")))))"))
    ;; ::53 — borrow in a result
    (let ((c (expect-invalid
              "(component (type $R (resource (rep i32))) (type $F (func (result (borrow $R)))))")))
      (is (search "function result cannot contain a `borrow` type"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::56 — borrow transitively through record+list+option
    (let ((c (expect-invalid
              "(component (type $R (resource (rep i32))) (type $Rec (record (field \"f\" (borrow $R)))) (type $F (func (result (option (list $Rec))))))")))
      (is (search "function result cannot contain a `borrow` type"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::60 — wrong dtor signature
    (let ((c (expect-invalid
              "(component (core module $M (func (export \"dtor\"))) (core instance $m (instantiate $M)) (type $R (resource (rep i32) (dtor (core func $m \"dtor\")))))")))
      (is (search "wrong signature for a destructor"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::61 — dtor index out of bounds
    (let ((c (expect-invalid
              "(component (type $R (resource (rep i32) (dtor (core func 100)))))")))
      (is (search "function index out of bounds"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::62 — canon resource.drop of a non-resource
    (let ((c (expect-invalid
              "(component (type $T (tuple u32)) (core func (canon resource.drop $T)))")))
      (is (search "not a resource type"
                  (wasmdown:wasmdown-error-message c))))
    ;; slice 5d (resources ::66-::67/::70): resource.new/rep require
    ;; a LOCAL resource — an imported (sub resource) type is not
    ;; local; a re-exported local resource (through an instantiate's
    ;; with-arg + alias export) IS local; resource.drop accepts any
    (let ((c (expect-invalid
              "(component (import \"T\" (type $T (sub resource))) (core func (canon resource.new $T)))")))
      (is (search "not a local resource"
                  (wasmdown:wasmdown-error-message c))))
    (let ((c (expect-invalid
              "(component (component $C (type $R (resource (rep i32))) (export \"r\" (type $R))) (instance $c (instantiate $C)) (alias export $c \"r\" (type $R)) (core func (canon resource.rep $R)))")))
      (is (search "not a local resource"
                  (wasmdown:wasmdown-error-message c))))
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (type $R (resource (rep i32))) (component $C (import \"x\" (type $x (sub resource))) (export \"y\" (type $x))) (instance $c (instantiate $C (with \"x\" (type $R)))) (alias export $c \"y\" (type $R2)) (core func (canon resource.rep $R2)))"))
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component $C (type $R (resource (rep i32))) (export \"r\" (type $R))) (instance $c (instantiate $C)) (alias export $c \"r\" (type $R)) (core func (canon resource.drop $R)))"))
    ;; slice 5e (resources ::27/::28): the generative-per-imported-
    ;; instance identity — two imports of ONE instance type carry
    ;; DISTINCT resources; one import's exported resource is stable
    ;; across its type-alias and its func-type's (own $r)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component definition (type $I (instance (export \"r\" (type $r (sub resource))) (export \"f\" (func (result (own $r)))))) (import \"i1\" (instance $i1 (type $I))) (import \"i2\" (instance $i2 (type $I))) (component $C (import \"r\" (type $r (sub resource))) (import \"f\" (func (result (own $r))))) (instance (instantiate $C (with \"r\" (type $i1 \"r\")) (with \"f\" (func $i1 \"f\")))) (instance (instantiate $C (with \"r\" (type $i2 \"r\")) (with \"f\" (func $i2 \"f\")))))"))
    (let ((c (expect-invalid
              "(component definition (type $I (instance (export \"r\" (type $r (sub resource))) (export \"f\" (func (result (own $r)))))) (import \"i1\" (instance $i1 (type $I))) (import \"i2\" (instance $i2 (type $I))) (component $C (import \"r\" (type $r (sub resource))) (import \"f\" (func (result (own $r))))) (instance (instantiate $C (with \"r\" (type $i1 \"r\")) (with \"f\" (func $i2 \"f\")))))")))
      (is (search "resource types are not the same"
                  (wasmdown:wasmdown-error-message c))))
    ;; slice 5f (resources ::43): an IMPORTED component target's
    ;; exports read from the import's ascribed component type — the
    ;; (alias export $c1 "foo" (func $foo)) projection resolves
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component $P (import \"C1\" (component $C1 (import \"T\" (type $T (sub resource))) (export \"foo\" (func (param \"t\" (own $T)))))) (import \"C2\" (component $C2 (import \"T\" (type $T (sub resource))) (import \"foo\" (func (param \"t\" (own $T)))))) (type $R (resource (rep i32))) (instance $c1 (instantiate $C1 (with \"T\" (type $R)))) (alias export $c1 \"foo\" (func $foo)) (instance $c2 (instantiate $C2 (with \"T\" (type $R)) (with \"foo\" (func $foo))))))"))))


(test component/external-visibility-type-pins
  ;; slice 5g (external-visibility ::37/::41-::44): an instance type's
  ;; declarators are visibility-checked at the USE site (an unused
  ;; instance type defines freely — ::35/::40); a COMPONENT type's own
  ;; import/export declarators are checked EAGERLY at the type's
  ;; definition. The interned inner space is rebuilt declarator-by-
  ;; declarator (the new index named on its own side, the index being
  ;; the pre-push length).
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::37 — an instance type exported with an unnamed record inside
    (let ((c (expect-invalid
              "(component (type $I (instance (type $Rec (record (field \"x\" u32))) (type $Rec2 (record (field \"r\" $Rec))) (export \"t\" (type (eq $Rec2))))) (export \"i\" (type $I)))")))
      (is (search "type not valid to be used as export"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::41 — the same shape as an import
    (let ((c (expect-invalid
              "(component (type $I (instance (type $Rec (record (field \"x\" u32))) (type $Rec2 (record (field \"r\" $Rec))) (export \"t\" (type (eq $Rec2))))) (import \"i\" (instance (type $I))))")))
      (is (search "instance not valid to be used as import"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::42 — a component type's export references an unnamed record
    (let ((c (expect-invalid
              "(component (type (component (type $Rec (record (field \"x\" u32))) (export \"f\" (func (param \"x\" $Rec))))))")))
      (is (search "func not valid to be used as export"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::44 — a component type's import may not see a preceding
    ;; export's resource
    (let ((c (expect-invalid
              "(component (type (component (export \"r\" (type $R (sub resource))) (import \"f\" (func (result (own $R)))))))")))
      (is (search "func not valid to be used as import"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::35/::40 — unused INSTANCE types define freely
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component definition (type (instance (type $Rec (record (field \"x\" u32))) (export \"f\" (func (param \"x\" $Rec))))))"))
    ;; slice 5h (external-visibility ::60): an export ascription must
    ;; be COMPATIBLE with the source — an imported instance (unknown
    ;; exports) cannot satisfy a non-empty instance ascription
    (let ((c (expect-invalid
              "(component (import \"f\" (instance $i)) (export \"f2\" (instance $i) (instance (export \"f\" (func)))))")))
      (is (search "ascribed type of export is not compatible"
                  (wasmdown:wasmdown-error-message c))))
    ;; slice 5i (external-visibility ::57): the DEEP (type $i "i1"
    ;; "i2") chain — an (alias export ct-inst "i1" (instance))
    ;; projects the export's ascribed SUB-INSTANCE ct (its
    ;; (instance . k) ascription indexes the ct's OWN declarator
    ;; space, not the parent's type vector); the sub-ct's exports
    ;; become the projected table. The fixture source is read from
    ;; the suite file itself (the full shape is long).
    (with-open-file
        (s (merge-pathnames
            "tests/component/validation/external-visibility.wast"
            (make-pathname :directory
                           '(:absolute "home" "tani" "ghq"
                                      "github.com" "tani" "wasmdown"))))
      (let ((txt (let ((acc (make-string-output-stream)))
                   (loop for line = (read-line s nil nil) while line
                         do (write-line line acc))
                   (get-output-stream-string acc))))
        ;; the ::57 command is the 58th COLUMN-0 form; slice it
        (let* ((cmds (loop for pos = 0 then (position #\newline txt :start (1+ pos))
                           while pos
                           when (and (< (1+ pos) (length txt))
                                     (char= (char txt (1+ pos)) #\())
                             collect (1+ pos)))
               (st (nth 57 cmds))
               (en (or (nth 58 cmds) (length txt)))
               (src2 (subseq txt st en)))
          (wasmdown.component:validate-component
           (wasmdown.component.text::parse-component-text src2))))))
    ;; slice 5j (external-visibility ::50): a PROJECTED instance's
    ;; func types walk with the PARENT's named-sets — the child's
    ;; bag func references a record named only INSIDE the child; the
    ;; parent's re-export of the projected instance rejects it
    (let ((c (block nil
               (handler-case
                   (wasmdown.component:validate-component
                    (wasmdown.component.text::parse-component-text
                     "(component
    (component $A
      (core module $m (func (export \"f\") (result i32) unreachable))
      (core instance $i (instantiate $m))
      (type $Rec (record (field \"x\" u32)))
      (export $Rec' \"t\" (type $Rec))
      (func $f (result $Rec') (canon lift (core func $i \"f\")))
      (instance $bag (export \"f\" (func $f)))
      (export \"i\" (instance $bag)))
    (instance $a (instantiate $A))
    ;; only \"i\" is re-exported here, not the record type \"t\" behind it
    (export \"i2\" (instance $a \"i\")))"))
                 (wasmdown:validation-error (c) (return c)))
               (error "expected error"))))
      (is (search "instance not valid to be used as export"
                  (wasmdown:wasmdown-error-message c)))))


(test component/annotated-name-resource-pins
  ;; slice 5k (annotated-names ::0/::4/::18/::26/::28/::31/::32): the
  ;; resource-NAME resolution for [constructor]/[method]/[static] —
  ;; the annotation's resource prefix must match the NAME of the
  ;; referenced resource on the CHECKED SIDE (import/export
  ;; namespaces disjoint); a WRAPPED constructor result
  ;; (result (own $T) ...) is legal (::0); a local resource def has
  ;; no name (::32); [static] only needs a KNOWN name (::26).
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::0 — the wrapped constructor results are LEGAL
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component definition (import \"a\" (type $a (sub resource))) (import \"[constructor]a\" (func (result (own $a)))) (import \"b\" (type $b (sub resource))) (import \"[constructor]b\" (func (result (result (own $b) (error string))))) (import \"d\" (type $d (sub resource))) (import \"[constructor]d\" (func (param \"x\" u32) (result (own $d)))))"))
    ;; ::4 — the imported resource is named `b`, not `a`
    (let ((c (expect-invalid
              "(component (import \"b\" (type $a (sub resource))) (import \"[constructor]a\" (func (result (own $a)))))")))
      (is (search "function does not match expected resource name `b`"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::18 — [method]'s self-borrow resource is named `b`, not `a`
    (let ((c (expect-invalid
              "(component (import \"b\" (type $T (sub resource))) (import \"[method]a.b\" (func (param \"self\" (borrow $T)))))")))
      (is (search "method does not match expected resource name `b`"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::26 — [static] with no named resource in scope
    (let ((c (expect-invalid
              "(component (import \"[static]a.b\" (func)))")))
      (is (search "static resource name is not known in this context"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::28 — an EXPORT-side [method] needs the resource (re-)exported
    (let ((c (expect-invalid
              "(component (import \"b\" (type $T (sub resource))) (import \"f\" (func $f (param \"self\" (borrow $T)))) (export \"[method]b.foo\" (func $f)))")))
      (is (search "resource used in function does not have a name in this context"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::31 — the same shape inside a component TYPE's import
    (let ((c (expect-invalid
              "(component (type (component (import \"b\" (type $T (sub resource))) (import \"[constructor]a\" (func (result (own $T)))))))")))
      (is (search "function does not match expected resource name `b`"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::32 — a BAG export's [constructor] with a local resource def
    (let ((c (expect-invalid
              "(component
    (type $T (resource (rep i32)))
    (core module $m (func (export \"a\") (result i32) unreachable))
    (core instance $i (instantiate $m))
    (func $f (result (own $T)) (canon lift (core func $i \"a\")))
    (instance
      (export \"a\" (type $T))
      (export \"[constructor]a\" (func $f))))")))
      (is (search "resource used in function does not have a name in this context"
                  (wasmdown:wasmdown-error-message c))))))


(test component/inline-func-shape-pins
  ;; slice 5l (resources ::39/::40): the parser's inline-functype
  ;; shape for INVERTED lift forms — the synthesized functype node
  ;; needs the "func" head (the old form fed %parse-functype the
  ;; canon keyword) and must EXCLUDE the (export ...)/type/canon
  ;; nodes; the inline (export "n") marker on a lifted FUNC also
  ;; emits an :export def (the child's export table must see the
  ;; func); and an export/alias-introduced type index joins the
  ;; instantiation-rebuilt child space ((:ref ti) for exports).
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::39's positive: the func-projection chain validates
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component $C (type $R (resource (rep i32))) (export $Rp \"r\" (type $R)) (core func $drop (canon resource.drop $R)) (func (export \"f\") (param \"x\" (own $Rp)) (canon lift (core func $drop)))) (instance $c1 (instantiate $C)) (component $D (import \"r\" (type $R (sub resource))) (import \"f\" (func (param \"x\" (own $R))))) (instance (instantiate $D (with \"r\" (type $c1 \"r\")) (with \"f\" (func $c1 \"f\")))))"))
    ;; ::40's negative: two instantiations carry DISTINCT resources
    (let ((c (expect-invalid
              "(component (component $C (type $R (resource (rep i32))) (export $Rp \"r\" (type $R)) (core func $drop (canon resource.drop $R)) (func (export \"f\") (param \"x\" (own $Rp)) (canon lift (core func $drop)))) (instance $c1 (instantiate $C)) (instance $c2 (instantiate $C)) (component $D (import \"r\" (type $R (sub resource))) (import \"f\" (func (param \"x\" (own $R))))) (instance (instantiate $D (with \"r\" (type $c1 \"r\")) (with \"f\" (func $c2 \"f\")))))")))
      (is (search "resource types are not the same"
                  (wasmdown:wasmdown-error-message c))))
    ;; the plain inverted lift WITHOUT an export marker still interns
    ;; the functype and the memory rule fires first
    (let ((c (expect-invalid
              "(component (core module $M (func (export \"f\") (param i32 i32))) (core instance $i (instantiate $M)) (func $f (param \"p\" (list u8)) (canon lift (core func $i \"f\"))))")))
      (is (search "canonical option `memory` is required"
                  (wasmdown:wasmdown-error-message c))))))

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
                      component/version-grammar-pins
                      component/fixed-list-alignment-pins
                      component/canonopt-unicity-pins
                      component/core-module-rule-pins
                      component/annotated-name-shape-pins
                      component/instantiate-matching-pins
                      component/core-module-empty-body-pin
                      component/resource-identity-pins
                      component/external-visibility-type-pins
                      component/annotated-name-resource-pins
                      component/inline-func-shape-pins
                      component/moduletype-outer-alias-pins
                      component/outer-alias-debruijn-pins
                      component/externtype-type-use-pins
                      component/abi-string-encoding-pins
                      component/declarator-bind-id-pins
                      component/core-subtype-matching-pins
                      component/canon-signature-pins)))
          wasmdown.test::*tests*)))
    (multiple-value-bind (passed failed)
        (wasmdown.test:run-all-tests)
      (declare (ignore passed))
      (zerop failed))))
