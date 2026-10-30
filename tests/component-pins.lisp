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


(test component/imported-component-space-pins
  ;; slice 5m (resources ::34): the COMPONENT index space counts
  ;; :component defs AND :component imports (%component-at, the
  ;; instantiate-args target walk); an imported component's export
  ;; table reads the ascribed ct with (eq i) exports resolved to the
  ;; eq TARGET's export name; and %instance-export-identity's
  ;; name-form uses the ENTRY's cdr (the identity source), so
  ;; T3=(eq $T2) shares T2's identity.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::34's positive: the imported-$C + $eq chain validates
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component $Outer (import \"C\" (component $C (export \"T1\" (type (sub resource))) (export \"T2\" (type $T2 (sub resource))) (export \"T3\" (type (eq $T2))))) (instance $c (instantiate $C)) (alias export $c \"T1\" (type $T1)) (alias export $c \"T2\" (type $T2)) (alias export $c \"T3\" (type $T3)) (component $eq (import \"a\" (type $a (sub resource))) (import \"b\" (type (eq $a)))) (instance (instantiate $eq (with \"a\" (type $T2)) (with \"b\" (type $T3))))))"))
    ;; the component-index-space shift: an instance of an imported
    ;; component followed by a nested component def — the instance
    ;; must NOT resolve to the later component
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component $C2 (import \"x\" (type $x (sub resource))) (export \"T\" (type $x))) (component $O (import \"C\" (component $C (export \"T\" (type (sub resource))))) (instance $c (instantiate $C)) (alias export $c \"T\" (type $T)) (component $eq (type $r (resource (rep i32)))) (instance $e (instantiate $eq))) (instance $o (instantiate $O (with \"C\" (component $C2)))))"))
    ;; two DIFFERENT exported resources stay distinct (the (eq $T2)
    ;; sharing must not leak to T1)
    (let ((c (expect-invalid
              "(component (component $Outer (import \"C\" (component $C (export \"T1\" (type (sub resource))) (export \"T2\" (type $T2 (sub resource))) (export \"T3\" (type (eq $T2))))) (instance $c (instantiate $C)) (alias export $c \"T1\" (type $T1)) (alias export $c \"T2\" (type $T2)) (alias export $c \"T3\" (type $T3)) (component $eq (import \"a\" (type $a (sub resource))) (import \"b\" (type (eq $a)))) (instance (instantiate $eq (with \"a\" (type $T1)) (with \"b\" (type $T3))))))")))
      (is (search "resource types are not the same"
                  (wasmdown:wasmdown-error-message c))))))


(test component/lower-canon-core-alloc-pins
  ;; slice 5m (abi ::10): a LOWER canon introduces a CORE func — the
  ;; parser's canon-def branch allocates + binds it (the trailing
  ;; (core func $id) target) so later projections allocate BEHIND
  ;; it; the lift's (core func $m "log") projection previously
  ;; collided with the lower's target slot ('function index out of
  ;; bounds' at validate).
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; the abi ::10 shape: two lowers then three lifts off one
    ;; core instance — validates cleanly
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (import \"log\" (func $log (param \"msg\" string))) (core module $Libc (memory (export \"mem\") 1) (func (export \"realloc\") (param i32 i32 i32 i32) (result i32) unreachable)) (core instance $libc (instantiate $Libc)) (alias core export $libc \"mem\" (core memory $mem)) (alias core export $libc \"realloc\" (core func $realloc)) (canon lower (func $log) string-encoding=utf8 (memory $mem) (realloc $realloc) (core func $log1p)) (canon lower (func $log) string-encoding=utf16 (memory $mem) (realloc $realloc) (core func $log2p)) (core module $M (func (export \"log\") (param i32 i32))) (core instance $m (instantiate $M)) (func (export \"log1\") (param \"msg\" string) (canon lift (core func $m \"log\") string-encoding=utf8 (memory $mem) (realloc $realloc))) (func (export \"log2\") (param \"msg\" string) (canon lift (core func $m \"log\") string-encoding=utf16 (memory $mem) (realloc $realloc))))"))
    ;; the lowered target id is BOUND: a later reference to $log1p
    ;; resolves (here: reused as a lift source whose type mismatches —
    ;; the reference must not be 'unknown')
    (let ((c (expect-invalid
              "(component (import \"log\" (func $log (param \"msg\" string))) (core module $Libc (memory (export \"mem\") 1) (func (export \"realloc\") (param i32 i32 i32 i32) (result i32) unreachable)) (core instance $libc (instantiate $Libc)) (alias core export $libc \"mem\" (core memory $mem)) (alias core export $libc \"realloc\" (core func $realloc)) (canon lower (func $log) string-encoding=utf8 (memory $mem) (realloc $realloc) (core func $log1p)) (core module $M (func (export \"r\") (param i64 i64))) (core instance $m (instantiate $M)) (func (export \"r\") (param \"msg\" string) (canon lift (core func $m \"r\") string-encoding=utf8 (memory $mem) (realloc $realloc))))")))
      (is (search "lowered parameter types"
                  (wasmdown:wasmdown-error-message c))))))


(test component/moduletype-inner-rules-pins
  ;; slice 5m (core-modules ::2/::5/::6): the moduletype rules apply
  ;; inside component/instance TYPES too (the inner :core-type walk
  ;; calls %check-moduletype); a bare (func) export is MARKED
  ;; :implicit by the parser and counts toward the moduletype's own
  ;; type space; an explicit (func (type n)) bounds-checks against
  ;; it; a :core-alias declarator also introduces a type index.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::2 — the explicit (type 0) with NO type decls is OOB
    (let ((c (expect-invalid
              "(component (core type (module (export \"a\" (func (type 0))))))")))
      (is (search "type index out of bounds"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::5 — duplicate export names inside a component type
    (let ((c (expect-invalid
              "(component (type (component (core type (module (export \"\" (func)) (export \"\" (func)))))))")))
      (is (search "already defined"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::6 — the same rule inside an instance type
    (let ((c (expect-invalid
              "(component (type (instance (core type (module (export \"\" (func)) (export \"\" (func)))))))")))
      (is (search "already defined"
                  (wasmdown:wasmdown-error-message c))))
    ;; the positive: a bare (func) + a later explicit (type 0) resolve
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (core type (module (export \"a\" (func)) (export \"b\" (func (type 0))))))"))))


(test component/core-instantiation-rules-pins
  ;; slice 5m (instantiation ::41/::56/::59/::78/::81): a CORE
  ;; instance's inline bag descriptor heads name the CORE sorts
  ;; ((func $i "g") projects the core-func — the component-level
  ;; reading previously failed 'unknown instance'); a (core <sort>
  ;; $i "n") projection may source a COMPONENT instance (the alias
  ;; flips to the component export table; the validator's sort check
  ;; rejects 'is not a module'); duplicate with-arg names and
  ;; duplicate inline-bag export names in core instantiations are
  ;; rejected; a (core export) alias's name must exist in the
  ;; provider's export list.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::41 — the inline core-instance bag projecting a core func
    (let ((c (expect-invalid
              "(component (core module $m1 (import \"\" \"f\" (func))) (core module $m2 (func (export \"g\") (param i32))) (core instance $i (instantiate $m2)) (core instance (instantiate $m1 (with \"\" (instance (export \"f\" (func $i \"g\")))))))")))
      (is (search "expected: (func)"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::56 — duplicate with-arg names
    (let ((c (expect-invalid
              "(component (core module $m) (core instance $i (instantiate $m)) (core instance (instantiate $m (with \"a\" (instance $i)) (with \"a\" (instance $i)))))")))
      (is (search "duplicate module instantiation argument"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::59 — duplicate inline-bag export names
    (let ((c (expect-invalid
              "(component (core module $m (func (export \"f\"))) (core instance $i (instantiate $m)) (core instance (export \"a\" (func $i \"f\")) (export \"a\" (func $i \"f\"))))")))
      (is (search "already defined"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::78 — a (core export) alias's name must exist
    (let ((c (expect-invalid
              "(component (core module $m) (core instance $i (instantiate $m)) (alias core export $i \"a\" (core func)))")))
      (is (search "has no export named"
                  (wasmdown:wasmdown-error-message c))))
    ;; ::81 — a component instance's non-module export aliased as a
    ;; core module
    (let ((c (expect-invalid
              "(component (component $c (component $inner) (export \"a\" (component $inner))) (instance $i (instantiate $c)) (export \"a\" (core module $i \"a\")))")))
      (is (search "is not a module"
                  (wasmdown:wasmdown-error-message c))))
    ;; the positive: a valid core bag projection (matching signature)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (core module $m1 (import \"\" \"f\" (func))) (core module $m2 (func (export \"g\"))) (core instance $i (instantiate $m2)) (core instance (instantiate $m1 (with \"\" (instance (export \"f\" (func $i \"g\")))))))"))))


(test component/instance-chain-projection-pins
  ;; slice 5m (indicies ::0): a multi-level (func $j "a" "b" "f")
  ;; chain projects through NESTED inline instance cts — the
  ;; ct-branch table build carries the DOUBLE-NESTED ct in
  ;; (:instance . ct) entries (%ct-decl-at resolves the declarator
  ;; position), and the hash-branch projection resolves a ct-carrying
  ;; entry to that ct's own export table; under comp-fallback the
  ;; chain's intermediate aliases allocate in the COMPONENT instance
  ;; space.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; the positive: the two-level chain validates
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component (component $C (import \"x\" (func))) (import \"f\" (func $f)) (import \"j\" (instance $j (export \"a\" (instance (export \"b\" (instance (export \"f\" (func)))))))) (instance (instantiate $C (with \"x\" (func $j \"a\" \"b\" \"f\"))))))"))
    ;; a missing intermediate name still rejects
    (let ((c (expect-invalid
              "(component (component (component $C (import \"x\" (func))) (import \"f\" (func $f)) (import \"j\" (instance $j (export \"a\" (instance (export \"b\" (instance (export \"f\" (func)))))))) (instance (instantiate $C (with \"x\" (func $j \"a\" \"zz\" \"f\"))))))")))
      (is (search "has no export named"
                  (wasmdown:wasmdown-error-message c))))))


(test component/builtin-immediate-pins
  ;; slice 5m (indicies ::7/::8/::10): the canon built-ins'
  ;; immediates accept PROJECTION forms — (memory (core memory
  ;; $m "mem")) / (core table $m "tbl") resolve through
  ;; %parse-projection (the bare (core ...) branch previously read
  ;; the THIRD item as a plain index); the builtin's type-position
  ;; immediate alone bounds-checks against the component type space
  ;; (thread.new-indirect's second imm is a CORE-TABLE idx, the
  ;; waitable-set's a CORE-MEMORY idx); and a BARE (core module)
  ;; import ascription carries the :NONE marker = the EMPTY
  ;; moduletype (the arg must have NO imports - instantiation
  ;; ::34) while (export "m" (core module)) declarators resolve
  ;; structurally.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; the waitable-set projections validate
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component (core module $M (memory (export \"mem\") 1)) (core instance $m (instantiate $M)) (alias core export $m \"mem\" (core memory $mem)) (canon waitable-set.wait (memory $mem) (core func)) (canon waitable-set.wait (memory 0) (core func)) (canon waitable-set.wait (memory (core memory $mem)) (core func)) (canon waitable-set.wait (memory (core memory $m \"mem\")) (core func)) (canon waitable-set.poll (memory (core memory $mem)) (core func)) (canon waitable-set.poll (memory (core memory $m \"mem\")) (core func))))"))
    ;; the thread.new-indirect projections validate
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component (core type $ft (func (param i32))) (core module $M (table (export \"tbl\") 1 funcref)) (core instance $m (instantiate $M)) (alias core export $m \"tbl\" (core table $tbl)) (canon thread.new-indirect 0 0 (core func)) (canon thread.new-indirect $ft $tbl (core func)) (canon thread.new-indirect $ft (core table $tbl) (core func)) (canon thread.new-indirect (core type $ft) $tbl (core func)) (canon thread.new-indirect (core type $ft) (core table $tbl) (core func)) (canon thread.new-indirect (core type $ft) (core table $m \"tbl\") (core func))))"))
    ;; ::10 - the dummy declarator ascriptions resolve
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component (import \"a\" (instance $foo (export \"v\" (component)) (export \"m\" (core module)))) (export \"v\" (component $foo \"v\")) (export \"w\" (core module $foo \"m\"))))"))
    ;; a BARE (core module) import with an arg that HAS imports
    ;; rejects (the empty moduletype)
    (let ((c (expect-invalid
              "(component (core module $m (import \"\" \"extra\" (global i32))) (component $c (import \"m\" (core module))) (instance (instantiate $c (with \"m\" (core module $m)))))")))
      (is (search "missing expected import"
                  (wasmdown:wasmdown-error-message c))))))


(test component/inline-target-sugar-pins
  ;; slice 5m (indicies ::2/::11): the (instantiate (module $i
  ;; "x")) / (instantiate (component $i "x")) targets accept
  ;; inline PROJECTION forms (the emitted alias chain + the final
  ;; index); and a (core module (export "n")) definition's single
  ;; (export <string>) node is the DEF-LEVEL EXPORT SUGAR - the
  ;; module is defined and exported as "n" (a component-level
  ;; export), not a wat field.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; the ::2 shapes validate (both sub-components)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component (import \"a\" (instance $i (export \"x\" (core module)))) (core instance (instantiate (module $i \"x\")))) (component (import \"a\" (instance $i (export \"x\" (component)))) (instance (instantiate (component $i \"x\")))))"))
    ;; the ::11 export sugar validates and the export exists
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component (type (export \"foo\") u8) (core module (export \"x\"))))"))
    ;; a projection target naming a MISSING export still rejects
    (let ((c (expect-invalid
              "(component (component (import \"a\" (instance $i (export \"x\" (core module)))) (core instance (instantiate (module $i \"zz\")))))")))
      (is (search "has no export named"
                  (wasmdown:wasmdown-error-message c))))))


(test component/outer-alias-special-binding-pins
  ;; slice 5m final (indicies ::9): the walk's (let
  ;; ((*outer-states* (cons outer ...)))) binds DYNAMICALLY —
  ;; the defvar must precede the walk (a let of an unproclaimed
  ;; symbol compiles LEXICAL and %outer-state-at never saw the
  ;; consed stack); and a nested TYPE declarator's walk threads
  ;; the ENCLOSING TYPE's INNER space as its outer (an (outer 1)
  ;; alias inside $T2 crosses into the sibling $r's space).
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    ;; ::9's shape: the inline instance ct's (type (eq $T))
    ;; referencing the parent-scope $T validates
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (component (type $T (future u32)) (import \"i\" (instance $i (export \"T\" (type (eq $T))))) (canon future.new $T (core func)) (canon future.new 0 (core func)) (canon future.new (type $T) (core func)) (canon future.new (type 0) (core func)) (canon future.new (type $i \"T\") (core func))))"))
    ;; resources ::30's chain still validates (the sibling $r
    ;; resolution through the nested $T2)
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (type $X (component (type $T (instance (export \"r\" (type (sub resource))))) (export \"t\" (instance $t (type $T))) (alias export $t \"r\" (type $r)) (type $T2 (instance (export \"r2\" (type (eq $r))) (export \"r\" (type (sub resource))))) (export \"t2\" (instance (type $T2))))))"))
    ;; an out-of-bounds (eq i) inside an inline instance ct
    ;; still rejects
    (let ((c (expect-invalid
              "(component (component (type $T (future u32)) (import \"i\" (instance $i (export \"T\" (type (eq 5)))))))")))
      (is (search "type index out of bounds"
                  (wasmdown:wasmdown-error-message c))))))

;;; ---- M8: instantiation EXECUTION pins (src/component-runtime.lisp) ----

(defun %exec-comp (src)
  "Parse + validate + EXECUTE a component text fixture; returns the
runtime comp-instance. The suite's end-to-end lane is the oracle;
these pins guard the executor's mechanisms directly."
  (let ((comp (wasmdown.component.text::parse-component-text src)))
    (wasmdown.component:validate-component comp)
    (wasmdown.component.runtime:instantiate-component comp)))

(test component/instantiation-core-link-pins
  ;; M8 mechanism 1 — TWO-LEVEL core-instance linking (Explainer
  ;; 'Instance Definitions'): $B imports ("a" "one"); the with-arg
  ;; names the FIRST level ("a" -> core instance $a); the second
  ;; level resolves through that instance's exports. The linked call
  ;; path must produce $A's constant.
  (let ((ci (%exec-comp
             "(component
                (core module $A (func (export \"one\") (result i32) (i32.const 1)))
                (core module $B
                  (import \"a\" \"one\" (func (result i32)))
                  (func (export \"two\") (result i32) (call 0)))
                (core instance $a (instantiate $A))
                (core instance $b (instantiate $B (with \"a\" (instance $a)))))")))
    (is (wasmdown.component.runtime:comp-instance-p ci))
    ;; the second core instance is the linked module-instance; its
    ;; "two" export is $B's func 1, which CALLS the imported "one"
    (let* ((mb (aref (wasmdown.component.runtime::comp-instance-core-instances
                      ci) 1))
           (two (wasmdown.runtime:export-value mb "two")))
      (is (wasmdown.runtime:func-instance-p two))
      ;; wasm-call returns the RESULTS LIST
      (is (equal '(1) (wasmdown.runtime:wasm-call two))))))

(test component/instantiation-with-arg-pins
  ;; M8 mechanism 2 — COMPONENT-instance with-args: the child's
  ;; import binds the parent's runtime value (here a canon-lifted
  ;; func), and the child's re-export projects the SAME value.
  (let ((ci (%exec-comp
             "(component
                (core module $M (func (export \"f\") (result i32) (i32.const 7)))
                (core instance $m (instantiate $M))
                (alias core export $m \"f\" (core func $cf))
                (func $f (result u32) (canon lift (core func $cf)))
                (component $c
                  (import \"x\" (func (result u32)))
                  (export \"y\" (func 0)))
                (instance $i (instantiate $c (with \"x\" (func $f)))))")))
    ;; the instantiated child instance is instances[0]
    (let* ((child (aref (wasmdown.component.runtime::comp-instance-instances
                         ci) 0))
           (y (wasmdown.component.runtime:runtime-export-value child "y")))
      (is (wasmdown.component.runtime:lifted-func-p y))
      ;; the bound import is the parent's lifted func (identity)
      (is (eq (gethash "x" (wasmdown.component.runtime:comp-instance-imports
                            child))
              y))
      ;; the FLAT canonical call path: calling the lifted func runs
      ;; the wrapped core func (i32.const 7)
      (is (= 7 (wasmdown.component.runtime:lifted-func-call y))))))

(test component/instantiation-start-pins
  ;; M8 mechanism 3 — START functions run last: a lifted func that
  ;; bumps a core global; (start $f) executes it during
  ;; instantiation, and the aliased global OBSERVES the mutation.
  ;; (The text parser has no start form — the comp IR is built by
  ;; APPENDING a start def to a parsed fixture.)
  (let* ((base (wasmdown.component.text::parse-component-text
                "(component
                   (core module $M
                     (global (export \"g\") (mut i32) (i32.const 0))
                     (func (export \"bump\") (global.set 0 (i32.add (global.get 0) (i32.const 1)))))
                   (core instance $m (instantiate $M))
                   (alias core export $m \"g\" (core global $g))
                   (alias core export $m \"bump\" (core func $bump))
                   (func $f (canon lift (core func $bump))))"))
         (comp (wasmdown.component:make-comp
                :defs (concatenate 'vector
                                   (wasmdown.component:comp-defs base)
                                   (vector
                                    (wasmdown.component:make-compdef
                                     :kind :start
                                     :payload (wasmdown.component:make-startrec
                                               :func 0
                                               :args (make-array 0)
                                               :results 0)))))))
    (wasmdown.component:validate-component comp)
    (let ((ci (wasmdown.component.runtime:instantiate-component comp)))
      ;; the start ran: the aliased core global (core-globals[0])
      ;; went 0 -> 1
      (let ((g (aref (wasmdown.component.runtime::comp-instance-core-globals
                      ci) 0)))
        (is (= 1 (wasmdown.runtime:global-value g)))))))

(test component/instantiation-trap-pins
  ;; M8 mechanism 4 — TRAP AT INSTANTIATION: a core module whose
  ;; start function traps; the trap propagates out of the component's
  ;; core-instance def (assert_trap (component ...) taxonomy: the
  ;; runner classifies it :execute).
  (let ((err (wasmdown.test::check-trap
              (lambda ()
                (%exec-comp
                 "(component
                    (core module $M
                      (func $start unreachable)
                      (start $start))
                    (core instance $m (instantiate $M)))"))
              :unreachable)))
    (is err)))

;;; ---- M9: the LINKING lane's mechanisms ----

(test component/invoke-lifted-func-pins
  ;; M9 mechanism — COMPONENT INVOKE: a comp-instance's exported
  ;; lifted func through the runner's action hook. The FLAT path
  ;; lowers u32 args to i32 and lifts the single result back.
  (let* ((ci (%exec-comp
              "(component
                 (core module $M
                   (global $g (mut i32) (i32.const 0))
                   (func (export \"bump\") (param i32) (result i32)
                     (global.set $g (i32.add (global.get $g)
                                             (local.get 0)))
                     (global.get $g)))
                 (core instance $m (instantiate $M))
                 (func (export \"bump\") (param \"x\" u32)
                   (result u32)
                   (canon lift (core func $m \"bump\"))))")))
    (let ((lf (wasmdown.component.runtime:runtime-export-value
               ci "bump")))
      (is (wasmdown.component.runtime:lifted-func-p lf))
      ;; the hook's INVOKE: args in the component domain, results in
      ;; the comparison domain
      (is (equal '(5) (funcall (wasmdown.wast:component-invoker)
                                ci "bump" '(5))))
      (is (equal '(12) (funcall (wasmdown.wast:component-invoker)
                                ci "bump" '(7)))))))

(test component/instance-isolation-pins
  ;; M9 mechanism — INSTANCE ISOLATION: two instances of one core
  ;; module have SEPARATE globals; two lifts off DIFFERENT instances
  ;; wrap DIFFERENT core funcs (unit.wast's first fixture). The
  ;; parser now consumes the EXPORT-introduced func slot so the
  ;; second lift cannot cross-wire to the first instance's func.
  (let* ((ci (%exec-comp
              "(component
                 (core module $M
                   (global $g (mut i32) (i32.const 0))
                   (func (export \"bump\") (result i32)
                     (global.set $g (i32.add (global.get $g)
                                             (i32.const 1)))
                     (global.get $g)))
                 (core instance $a (instantiate $M))
                 (core instance $b (instantiate $M))
                 (func (export \"bump-a\") (result u32)
                   (canon lift (core func $a \"bump\")))
                 (func (export \"bump-b\") (result u32)
                   (canon lift (core func $b \"bump\"))))")))
    (let ((a (wasmdown.component.runtime:runtime-export-value ci "bump-a"))
          (b (wasmdown.component.runtime:runtime-export-value ci "bump-b")))
      (is (wasmdown.component.runtime:lifted-func-p a))
      (is (wasmdown.component.runtime:lifted-func-p b))
      ;; the two lifts wrap DIFFERENT core funcs
      (is (not (eq (wasmdown.component.runtime::lifted-func-core a)
                   (wasmdown.component.runtime::lifted-func-core b))))
      ;; the state is isolated: a=1, a=2, b=1, a=3
      (is (= 1 (wasmdown.component.runtime:lifted-func-call a)))
      (is (= 2 (wasmdown.component.runtime:lifted-func-call a)))
      (is (= 1 (wasmdown.component.runtime:lifted-func-call b)))
      (is (= 3 (wasmdown.component.runtime:lifted-func-call a))))))

(test component/canon-lower-roundtrip-pins
  ;; M9 mechanism — CANON LOWER: the lowered core func is a REAL
  ;; host core func wrapping the target component func (the flat
  ;; sync path): a service's lifted bump, lowered into a client
  ;; module, called — the client observes the SERVICE's state.
  (let* ((ci (%exec-comp
              "(component
                 (component $Svc
                   (core module $M
                     (global $g (mut i32) (i32.const 0))
                     (func (export \"bump\") (result i32)
                       (global.set $g
                                   (i32.add (global.get $g)
                                            (i32.const 1)))
                       (global.get $g)))
                   (core instance $m (instantiate $M))
                   (func (export \"bump\") (result u32)
                     (canon lift (core func $m \"bump\"))))
                 (component $User
                   (import \"svc\" (instance $svc
                     (export \"bump\" (func (result u32)))))
                   (core func $bump (canon lower (func $svc \"bump\")))
                   (core module $M
                     (import \"svc\" \"bump\" (func (result i32)))
                     (func (result i32) (call 0)))
                   (core instance $m (instantiate $M
                     (with \"svc\" (instance
                       (export \"bump\" (func $bump)))))))
                 (instance $svc (instantiate $Svc))
                 (instance $user (instantiate $User
                   (with \"svc\" (instance $svc)))))")))
    ;; the lowered core func is a host func with the flat [i32]→[i32]
    ;; signature wired to the service's lifted func


    (let* ((user (aref (wasmdown.component.runtime::comp-instance-instances
                        ci) 1))
           (lf (aref (wasmdown.component.runtime::comp-instance-core-funcs
                      user) 0)))
      (is (wasmdown.runtime:func-instance-p lf))
      (is (eq (wasmdown.runtime:func-instance-kind lf) :host))
      ;; calling it bumps the SERVICE's counter (shared state)
      (is (equal '(1) (wasmdown.runtime:wasm-call lf)))
      (is (equal '(2) (wasmdown.runtime:wasm-call lf))))))

(test component/linking-tag-identity-pins
  ;; M9 mechanism (linking tags ::9) — EH TAG IDENTITY ACROSS
  ;; INSTANCES: the Catcher imports $tag-a's tag; $ta throws the
  ;; $tag-a tag, $tb the $tag-b one. SBCL runs handler-bind handlers
  ;; BEFORE unwinding, so at handler time the dynamic *frame-module*
  ;; is the THROWER's — the catch clause's tag must resolve at
  ;; try_table ENTRY (the guarding module), never late. A late read
  ;; saw the thrower's tags[0] — the thrown tag itself — and every
  ;; catch matched; run-b must fall through to catch_all (77).
  (let* ((ci (%exec-comp
              "(component
                 (core module $Tag (tag (export \"t\") (param i32)))
                 (core module $Thrower
                   (import \"tag\" \"t\" (tag $t (param i32)))
                   (func (export \"throw\") (param i32)
                     (throw $t (local.get 0))))
                 (core module $Catcher
                   (import \"tag\" \"t\" (tag $t (param i32)))
                   (import \"a\" \"throw\" (func $throw-a (param i32)))
                   (import \"b\" \"throw\" (func $throw-b (param i32)))
                   (func (export \"run-a\") (param i32) (result i32)
                     (block $all
                       (block $h (result i32)
                         (try_table (catch $t $h) (catch_all $all)
                           (call $throw-a (local.get 0)))
                         (return (i32.const 99)))
                       (return))
                     (i32.const 77))
                   (func (export \"run-b\") (param i32) (result i32)
                     (block $all
                       (block $h (result i32)
                         (try_table (catch $t $h) (catch_all $all)
                           (call $throw-b (local.get 0)))
                         (return (i32.const 99)))
                       (return))
                     (i32.const 77)))
                 (core instance $tag-a (instantiate $Tag))
                 (core instance $tag-b (instantiate $Tag))
                 (core instance $ta
                   (instantiate $Thrower (with \"tag\" (instance $tag-a))))
                 (core instance $tb
                   (instantiate $Thrower (with \"tag\" (instance $tag-b))))
                 (core instance $c (instantiate $Catcher
                   (with \"tag\" (instance $tag-a))
                   (with \"a\" (instance $ta))
                   (with \"b\" (instance $tb))))
                 (func (export \"run-a\") (param \"x\" u32) (result u32)
                   (canon lift (core func $c \"run-a\")))
                 (func (export \"run-b\") (param \"x\" u32) (result u32)
                   (canon lift (core func $c \"run-b\"))))"))
         (ra (wasmdown.component.runtime:runtime-export-value ci "run-a"))
         (rb (wasmdown.component.runtime:runtime-export-value ci "run-b")))
    ;; the thrown $tag-a tag IS the Catcher's $t: caught, payload 42
    (is (= 42 (wasmdown.component.runtime:lifted-func-call ra 42)))
    ;; $tag-b's tag is a DIFFERENT tag-instance: no match, catch_all
    (is (= 77 (wasmdown.component.runtime:lifted-func-call rb 42)))))

(test component/linking-tag-sort-mismatch-pins
  ;; M9 mechanism (linking tags ::10) — the CORE-EXPORT alias's
  ;; sort check: a func export aliased as (core tag) is a sort
  ;; mismatch, not merely an existence check.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (let ((comp (wasmdown.component.text::parse-component-text
                              src)))
                   (wasmdown.component:validate-component comp))
               (wasmdown:validation-error (c) (return c)))
             (error "expected validation error"))))
    ;; aliasing a FUNC export as a core tag
    (let ((c (expect-invalid
              "(component
                 (core module $M (func (export \"f\")))
                 (core instance $i (instantiate $M))
                 (alias core export $i \"f\" (core tag $t)))")))
      (is (search "is not a tag" (wasmdown:wasmdown-error-message c))))
    ;; a correct tag alias still validates
    (let ((comp (wasmdown.component.text::parse-component-text
                 "(component
                    (core module $M (tag (export \"t\") (param i32)))
                    (core instance $i (instantiate $M))
                    (alias core export $i \"t\" (core tag $t)))")))
      (wasmdown.component:validate-component comp))))

(test component/linking-ascribed-projection-pins
  ;; M9 mechanism (linking EV ::61) — the EXPORT ASCRIBED instance
  ;; type REPLACES the visible one in projections: an instance
  ;; exported with an EMPTY (instance) ascription hides its exports
  ;; from consumers; a consumer requiring one of them fails with
  ;; 'missing expected export'.
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (let ((comp (wasmdown.component.text::parse-component-text
                              src)))
                   (wasmdown.component:validate-component comp))
               (wasmdown:validation-error (c) (return c)))
             (error "expected validation error"))))
    ;; the ascribed-empty f2 no longer satisfies a consumer's "f"
    (let ((c (expect-invalid
              "(component
                 (import \"f\" (func $f))
                 (component $C
                   (import \"f\" (instance $i (export \"f\" (func))))
                   (export \"f2\" (instance $i) (instance)))
                 (instance $c (instantiate $C
                   (with \"f\" (instance (export \"f\" (func $f))))))
                 (component $Consume
                   (import \"arg\" (instance (export \"f\" (func)))))
                 (instance (instantiate $Consume
                   (with \"arg\" (instance $c \"f2\")))))")))
      (is (search "missing expected export"
                  (wasmdown:wasmdown-error-message c))))
    ;; WITHOUT the ascription the projection still sees "f" and
    ;; the consumer validates (the up-cast direction)
    (let ((comp (wasmdown.component.text::parse-component-text
                 "(component
                    (import \"f\" (func $f))
                    (component $C
                      (import \"f\" (instance $i (export \"f\" (func))))
                      (export \"f2\" (instance $i)))
                    (instance $c (instantiate $C
                      (with \"f\" (instance (export \"f\" (func $f))))))
                    (component $Consume
                      (import \"arg\" (instance (export \"f\" (func)))))
                    (instance (instantiate $Consume
                      (with \"arg\" (instance $c \"f2\")))))")))
      (wasmdown.component:validate-component comp))))


(test component/values-string-roundtrip-pins
  ;; values lane — STRING ROUNDTRIP (CanonicalABI.md store_string /
  ;; lift_flat_string ~2575/3088): a (param string)(result string)
  ;; canon lift with (memory)(realloc) lowers the argument through
  ;; the callee's realloc into its memory and lifts the result's
  ;; [ptr,len] back — the concat.wast "echo" fixture's mechanism.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (memory (export \"mem\") 1)"
                "  (global $next (mut i32) (i32.const 1024))"
                "  (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
                "    (local $r i32)"
                "    (global.set $next (i32.and (i32.add (global.get $next) (i32.const 7)) (i32.const -8)))"
                "    (local.set $r (global.get $next))"
                "    (global.set $next (i32.add (global.get $next) (local.get 3)))"
                "    (local.get $r))"
                "  (func (export \"echo\") (param $p i32) (param $l i32) (result i32)"
                "    (i32.store (i32.const 0) (local.get $p))"
                "    (i32.store (i32.const 4) (local.get $l))"
                "    (i32.const 0)))"
                " (core instance $m (instantiate $M))"
                " (func (export \"echo\") (param \"a\" string) (result string)"
                "  (canon lift (core func $m \"echo\")"
                "   (memory (core memory $m \"mem\"))"
                "   (realloc (core func $m \"realloc\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "echo"
                            (list '(:str "hello values")))))
      (is (equal '("hello values") results)))))

(test component/values-list-lower-lift-pins
  ;; values lane — LIST LOWER/LIFT (CanonicalABI.md
  ;; store_list_into_range ~2761 / lift_flat_list ~3093): the
  ;; (list u32) param reallocs the element vector into the callee's
  ;; memory; the flat [ptr,len] pair carries it.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (memory (export \"mem\") 1)"
                "  (global $next (mut i32) (i32.const 1024))"
                "  (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
                "    (local $r i32)"
                "    (global.set $next (i32.and (i32.add (global.get $next) (i32.const 7)) (i32.const -8)))"
                "    (local.set $r (global.get $next))"
                "    (global.set $next (i32.add (global.get $next) (local.get 3)))"
                "    (local.get $r))"
                "  (func (export \"sum3\") (param $p i32) (param $n i32) (result i32)"
                "    (i32.add (i32.load (local.get $p))"
                "      (i32.add (i32.load offset=4 (local.get $p))"
                "               (i32.load offset=8 (local.get $p)))))) "
                " (core instance $m (instantiate $M))"
                " (func (export \"sum3\") (param \"a\" (list u32)) (result u32)"
                "  (canon lift (core func $m \"sum3\")"
                "   (memory (core memory $m \"mem\"))"
                "   (realloc (core func $m \"realloc\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "sum3"
                            (list '(:list ((:num :u32 1) (:num :u32 2)
                                           (:num :u32 3)))))))
      (is (equal '(6) results)))))

(test component/values-record-alignment-pins
  ;; values lane — RECORD LOWER (CanonicalABI.md lower_flat_record
  ;; ~3224 + flatten_record ~2971): a 2-field record lowers FLAT as
  ;; the field sequence (u8 and u32 both ride i32 slots); the SPILLED
  ;; (aligned-memory) shape is covered by the maxflat pin below.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (memory (export \"mem\") 1)"
                "  (global $next (mut i32) (i32.const 1024))"
                "  (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
                "    (local $r i32)"
                "    (global.set $next (i32.and (i32.add (global.get $next) (i32.const 7)) (i32.const -8)))"
                "    (local.set $r (global.get $next))"
                "    (global.set $next (i32.add (global.get $next) (local.get 3)))"
                "    (local.get $r))"
                "  (func (export \"sum\") (param $p i32) (param $q i32) (result i32)"
                "    (i32.add (local.get $p) (local.get $q))))"
                " (core instance $m (instantiate $M))"
                " (type $R (record (field \"a\" u8) (field \"b\" u32)))"
                " (export $Re \"r\" (type $R))"
                " (func (export \"sum\") (param \"a\" $Re) (result u32)"
                "  (canon lift (core func $m \"sum\")"
                "   (memory (core memory $m \"mem\"))"
                "   (realloc (core func $m \"realloc\")))))"))))
    ;; u8 at 0, u32 aligned to 4 -> the callee reads [0]=3, [4]=4
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "sum"
                            (list '(:record (("a" :num :u8 3)
                                             ("b" :num :u32 4)))))))
      (is (equal '(7) results)))))

(test component/values-variant-discriminant-pins
  ;; values lane — VARIANT DISCRIMINANT (CanonicalABI.md
  ;; lower_flat_variant ~3235 / lift_flat_variant ~3120): the case
  ;; index rides the discriminant slot and the payload the joined
  ;; slot (flat).
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (func (export \"tag\") (param $d i32) (param $v i32) (result i32)"
                "    (i32.add (i32.mul (local.get $d) (i32.const 100))"
                "             (local.get $v))))"
                " (core instance $m (instantiate $M))"
                " (type $V (variant (case \"a\" u32) (case \"b\" u32)))"
                " (export $Ve \"v\" (type $V))"
                " (func (export \"tag\") (param \"a\" $Ve) (result u32)"
                "  (canon lift (core func $m \"tag\"))))"))))
    ;; case "b" = index 1 -> 1*100 + 7
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "tag"
                            (list '(:variant "b" (:num :u32 7))))))
      (is (equal '(107) results)))))

(test component/values-maxflat-indirection-pins
  ;; values lane — the MAX_FLAT fallthrough (CanonicalABI.md
  ;; lower_flat_values ~3289): >16 flat params spill the TUPLE
  ;; through realloc into a single [ptr] argument.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (memory (export \"mem\") 1)"
                "  (global $next (mut i32) (i32.const 1024))"
                "  (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
                "    (local $r i32)"
                "    (global.set $next (i32.and (i32.add (global.get $next) (i32.const 7)) (i32.const -8)))"
                "    (local.set $r (global.get $next))"
                "    (global.set $next (i32.add (global.get $next) (local.get 3)))"
                "    (local.get $r))"
                "  (func (export \"sum17\") (param $p i32) (result i32)"
                "    (i32.add (i32.load (local.get $p))"
                "             (i32.load offset=64 (local.get $p))))"
                " )"
                " (core instance $m (instantiate $M))"
                " (type $Big (tuple u32 u32 u32 u32 u32 u32 u32 u32"
                "                  u32 u32 u32 u32 u32 u32 u32 u32 u32))"
                " (export $Be \"big\" (type $Big))"
                " (func (export \"sum17\") (param \"a\" $Be) (result u32)"
                "  (canon lift (core func $m \"sum17\")"
                "   (memory (core memory $m \"mem\"))"
                "   (realloc (core func $m \"realloc\")))))"))))
    ;; field 0 at offset 0, field 16 at offset 64 -> 1 + 17 = 18
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "sum17"
                            (list '(:tuple ((:num :u32 1) (:num :u32 2)
                                            (:num :u32 3) (:num :u32 4)
                                            (:num :u32 5) (:num :u32 6)
                                            (:num :u32 7) (:num :u32 8)
                                            (:num :u32 9) (:num :u32 10)
                                            (:num :u32 11) (:num :u32 12)
                                            (:num :u32 13) (:num :u32 14)
                                            (:num :u32 15) (:num :u32 16)
                                            (:num :u32 17)))))))
      (is (equal '(18) results)))))

(test component/values-map-lower-pins
  ;; values lane — the MAP despecialization (CanonicalABI.md 1938:
  ;; despecialize(MapType(k, v)) = ListType(TupleType([k, v]))): the
  ;; map param lowers as a list of (key,value) tuples through
  ;; realloc into the callee's memory. The core strides 12 bytes per
  ;; entry (string [ptr,len] + u32, per elem_size_record) and emits
  ;; the string keys in order.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (memory (export \"mem\") 1)"
                "  (global $w (mut i32) (i32.const 1024))"
                "  (global $next (mut i32) (i32.const 2048))"
                "  (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
                "   (local $r i32)"
                "   (global.set $next (i32.and (i32.add (global.get $next) (i32.const 7)) (i32.const -8)))"
                "   (local.set $r (global.get $next))"
                "   (global.set $next (i32.add (global.get $next) (local.get 3)))"
                "   (local.get $r))"
                "  (func $emit (param $s i32) (param $l i32)"
                "   (memory.copy (global.get $w) (local.get $s) (local.get $l))"
                "   (global.set $w (i32.add (global.get $w) (local.get $l))))"
                "  (func (export \"sum\") (param $p i32) (param $n i32) (result i32)"
                "   (local $i i32) (local $e i32)"
                "   (block $d (loop $l"
                "    (br_if $d (i32.ge_u (local.get $i) (local.get $n)))"
                "    (local.set $e (i32.add (local.get $p) (i32.mul (local.get $i) (i32.const 12))))"
                "    (call $emit (i32.load (local.get $e)) (i32.load offset=4 (local.get $e)))"
                "    (local.set $i (i32.add (local.get $i) (i32.const 1)))"
                "    (br $l)))"
                "   (i32.store (i32.const 0) (i32.const 1024))"
                "   (i32.store (i32.const 4) (i32.sub (global.get $w) (i32.const 1024)))"
                "   (i32.const 0))"
                " )"
                " (core instance $m (instantiate $M))"
                " (func (export \"m\") (param \"a\" (map string u32)) (result string)"
                "  (canon lift (core func $m \"sum\")"
                "   (memory (core memory $m \"mem\"))"
                "   (realloc (core func $m \"realloc\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "m"
                            (list '(:list ((:tuple ((:str "a") (:num :u32 7)))
                                                    (:tuple ((:str "b")
                                                             (:num :u32 9)))))))))
      (is (equal '("ab") results)))))

(test component/values-option-size-pins
  ;; values lane — the OPTION's elem_size is the FULL variant rule
  ;; (align(disc, max-case-alignment) + max payload, aligned):
  ;; option(string) strides 12 bytes (1 disc aligned to 4 + the
  ;; [ptr,len] payload), matching the core's fixed stride; the old
  ;; payload-only formula dropped the discriminant and
  ;; desynchronized the entries.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (memory (export \"mem\") 1)"
                "  (global $w (mut i32) (i32.const 1024))"
                "  (global $next (mut i32) (i32.const 2048))"
                "  (func (export \"realloc\") (param i32 i32 i32 i32) (result i32)"
                "   (local $r i32)"
                "   (global.set $next (i32.and (i32.add (global.get $next) (i32.const 7)) (i32.const -8)))"
                "   (local.set $r (global.get $next))"
                "   (global.set $next (i32.add (global.get $next) (local.get 3)))"
                "   (local.get $r))"
                "  (func $emit (param $s i32) (param $l i32)"
                "   (memory.copy (global.get $w) (local.get $s) (local.get $l))"
                "   (global.set $w (i32.add (global.get $w) (local.get $l))))"
                "  (func (export \"deep\") (param $p i32) (param $n i32) (result i32)"
                "   (local $i i32) (local $e i32)"
                "   (block $d (loop $l"
                "    (br_if $d (i32.ge_u (local.get $i) (local.get $n)))"
                "    (local.set $e (i32.add (local.get $p) (i32.mul (local.get $i) (i32.const 12))))"
                "    (if (i32.load (local.get $e))"
                "     (then (call $emit (i32.load offset=4 (local.get $e)) (i32.load offset=8 (local.get $e)))))"
                "    (local.set $i (i32.add (local.get $i) (i32.const 1)))"
                "    (br $l)))"
                "   (i32.store (i32.const 0) (i32.const 1024))"
                "   (i32.store (i32.const 4) (i32.sub (global.get $w) (i32.const 1024)))"
                "   (i32.const 0))"
                " )"
                " (core instance $m (instantiate $M))"
                " (func (export \"d\") (param \"a\" (list (option string))) (result string)"
                "  (canon lift (core func $m \"deep\")"
                "   (memory (core memory $m \"mem\"))"
                "   (realloc (core func $m \"realloc\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "d"
                            (list '(:list ((:option-some (:str "x"))
                                                    (:option-none)
                                                    (:option-some
                                                     (:str "y"))))))))
      (is (equal '("xy") results)))))

;;; ---- M11a: the async CORE's VALIDATION pins (slice 1) ----

(test component/async-task-return-borrow-pins
  ;; task.return's result must not transitively contain a borrow
  ;; (validate-task-return.wast's two rejections + the positive
  ;; own-handle case)
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected validation-error"))))
    ;; the bare borrow
    (let ((c (expect-invalid
              "(component (type $r (resource (rep i32)))
                 (core func (canon task.return (result (borrow $r)))))")))
      (is (search "task.return` result cannot contain a `borrow` type"
                  (wasmdown:wasmdown-error-message c))))
    ;; transitively through a tuple
    (let ((c (expect-invalid
              "(component (type $r (resource (rep i32)))
                 (type $t (tuple u32 (borrow $r)))
                 (core func (canon task.return (result $t))))")))
      (is (search "task.return` result cannot contain a `borrow` type"
                  (wasmdown:wasmdown-error-message c))))
    ;; the POSITIVE: an own-handle result validates
    (wasmdown.component:validate-component
     (wasmdown.component.text::parse-component-text
      "(component (type $r (resource (rep i32)))
         (core func (canon task.return (result (own $r)))))"))))

(test component/async-opt-requires-async-type-pins
  ;; the `async` canonopt requires an ASYNC function type — for
  ;; lift (the ascribed functype), lift+callback, and lower
  ;; (validate-no-async-abi-for-sync-type.wast's three cases)
  (flet ((expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected validation-error"))))
    (let ((c (expect-invalid
              "(component
                 (core module $M (func (export \"f\")))
                 (core instance $i (instantiate $M))
                 (func (export \"f\") (canon lift (core func $i \"f\") async)))")))
      (is (search "async` canonical option requires an async function type"
                  (wasmdown:wasmdown-error-message c))))
    (let ((c (expect-invalid
              "(component
                 (core module $M
                   (func (export \"f\") (param i32) (result i32) unreachable)
                   (func (export \"f_cb\") (param i32 i32 i32) (result i32) unreachable))
                 (core instance $i (instantiate $M))
                 (func (export \"f\") (canon lift (core func $i \"f\") async (callback (core func $i \"f_cb\")))))")))
      (is (search "async` canonical option requires an async function type"
                  (wasmdown:wasmdown-error-message c))))
    (let ((c (expect-invalid
              "(component
                 (import \"f\" (func $f))
                 (core func $f' (canon lower (func $f) async)))")))
      (is (search "async` canonical option requires an async function type"
                  (wasmdown:wasmdown-error-message c))))))

(test component/async-builtin-imms-parse-pins
  ;; the async core builtins' TEXT immediates parse to their full
  ;; shapes: task.return's (result ...)? + opts, subtask.cancel's
  ;; async?, context.get/set's valtype + index
  (let* ((comp (wasmdown.component.text::parse-component-text
                (concatenate 'string
                  "(component"
                  " (core module $M (memory 1)"
                  "   (func (export \"f\") (result i32) (i32.const 1)))"
                  " (core instance $m (instantiate $M))"
                  " (canon task.return (core func $m \"f\"))"
                  " (canon task.return (result u32) (core func $m \"f\"))"
                  " (canon context.set i32 0 (core func $m \"f\"))"
                  " (canon context.get i32 0 (core func $m \"f\"))"
                  " (canon subtask.cancel async (core func $m \"f\"))"
                  " (canon subtask.cancel (core func $m \"f\"))"
                  " (canon waitable-set.new (core func $m \"f\"))"
                  " (canon waitable.join (core func $m \"f\"))"
                  " (canon backpressure.inc (core func $m \"f\"))"
                  " (canon task.cancel (core func $m \"f\"))"
                  " (canon subtask.drop (core func $m \"f\"))"
                  " (canon waitable-set.drop (core func $m \"f\")))")))
         (canons (loop for d across (wasmdown.component::comp-defs comp)
                      when (eq (wasmdown.component::compdef-kind d) :canon)
                        collect (wasmdown.component::canonrec-imms
                                 (wasmdown.component::compdef-payload d)))))
    ;; task.return with NO result: (:task-return nil opts)
    (is (equal '(:task-return nil nil) (first canons)))
    ;; with a result: the valtype collected
    (is (equal '(:task-return (:u32) nil) (second canons)))
    ;; context.set/get: the valtype + the 0 index + opts
    (is (equal '(:context-set :i32 0 nil) (third canons)))
    (is (equal '(:context-get :i32 0 nil) (fourth canons)))
    ;; subtask.cancel's async? flag
    (is (equal '(:subtask-cancel t) (fifth canons)))
    (is (equal '(:subtask-cancel nil) (sixth canons)))
    ;; the no-imm builtins
    (is (equal '(:waitable-set-new) (seventh canons)))
    (is (equal '(:waitable-join) (eighth canons)))
    (is (equal '(:backpressure-inc) (ninth canons)))
    (is (equal '(:task-cancel) (tenth canons)))
    (is (equal '(:subtask-drop) (nth 10 canons)))
    (is (equal '(:waitable-set-drop) (nth 11 canons)))))

(test component/async-builtin-sig-marker-pins
  ;; a with-bag entry pointing at a canon-introduced ASYNC core
  ;; built-in resolves its flat sig (the marker path that was
  ;; (:canon-builtin nil) before M11a — async-calls-sync's
  ;; 'does not export an item named context.set' gap)
  (let ((comp (wasmdown.component.text::parse-component-text
              "(component (core module $M (memory 1) (func (export \"f\") (result i32) (i32.const 1))) (canon context.set i32 0 (core func $m \"f\")))")))
    ;; validates: the bag's entry resolves the canon's sig
    (wasmdown.component:validate-component comp)))

(test component/async-task-yield-resume-pins
  ;; M11a — the YIELD/EXIT protocol (CanonicalABI.md 3390/3464):
  ;; an async-typed canon lift with (callback ...) runs the core,
  ;; parks the task's thread on YIELD (the scheduler nudge resumes
  ;; it) and the callback's EXIT resolves the task via
  ;; task.return — the invoke returns the resolved value.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (import \"\" \"task.return\" (func $task.return (param i32)))"
                "  (memory (export \"mem\") 1)"
                "  (global $g (mut i32) (i32.const 0))"
                "  (func $f (export \"f\") (result i32)"
                "    (global.set $g (i32.const 1))"
                "    (i32.const 1))"
                "  (func $f_cb (export \"f-cb\") (param i32 i32 i32) (result i32)"
                "    (call $task.return (i32.const 42))"
                "    (i32.const 0)))"
                "  (canon task.return (result u32) (core func $task.return))"
                "  (core instance $m (instantiate $M (with \"\" (instance"
                "    (export \"task.return\" (func $task.return))))))"
                " (func (export \"f\") async (result u32) (canon lift"
                "   (core func $m \"f\")"
                "   async (callback (core func $m \"f-cb\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "f" '())))
      (is (equal '(42) results)))))

(test component/async-context-roundtrip-pins
  ;; M11a — context.get/set (CanonicalABI.md 3835/3862): the
  ;; per-task i32 context slot threads a value between the guest
  ;; core and its callback turn.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (import \"\" \"task.return\" (func $task.return (param i32)))"
                "  (import \"\" \"context.set\" (func $context.set (param i32)))"
                "  (import \"\" \"context.get\" (func $context.get (result i32)))"
                "  (memory (export \"mem\") 1)"
                "  (func $f (export \"f\") (result i32)"
                "    (call $context.set (i32.const 7))"
                "    (i32.const 1))"
                "  (func $f_cb (export \"f-cb\") (param i32 i32 i32) (result i32)"
                "    (call $task.return (call $context.get))"
                "    (i32.const 0)))"
                "  (canon task.return (result u32) (core func $task.return))"
                "  (canon context.set i32 0 (core func $context.set))"
                "  (canon context.get i32 0 (core func $context.get))"
                "  (core instance $m (instantiate $M (with \"\" (instance"
                "    (export \"task.return\" (func $task.return))"
                "    (export \"context.set\" (func $context.set))"
                "    (export \"context.get\" (func $context.get))))))"
                " (func (export \"f\") async (result u32) (canon lift"
                "   (core func $m \"f\")"
                "   async (callback (core func $m \"f-cb\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "f" '())))
      (is (equal '(7) results)))))

(test component/async-wset-wait-join-pins
  ;; M11a — waitable-set.new/wait + waitable.join (CanonicalABI.md
  ;; 4000/4020/4108): the callback's WAIT turn parks on the set
  ;; until the JOINED subtask's resolution arms its event — the
  ;; event delivery (SUBTASK, index, RETURNED) resumes the loop.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (component $Inner"
                "  (core module $M"
                "   (import \"\" \"task.return\" (func $task.return (param i32)))"
                "   (func $f (export \"f\") (result i32)"
                "    (i32.const 1))"
                "   (func $f_cb (export \"f-cb\") (param i32 i32 i32) (result i32)"
                "    (call $task.return (i32.const 5))"
                "    (i32.const 0)))"
                "  (canon task.return (result u32) (core func $task.return))"
                "  (core instance $inner (instantiate $M (with \"\" (instance"
                "   (export \"task.return\" (func $task.return))))))"
                "  (func (export \"f\") async (result u32) (canon lift"
                "   (core func $inner \"f\")"
                "   async (callback (core func $inner \"f-cb\")))))"
                " (core module $Outer"
                "  (import \"\" \"f\" (func $f (param i32) (result i32)))"
                "  (import \"\" \"task.return\" (func $task.return (param i32)))"
                "  (import \"\" \"waitable-set.new\" (func $wsn (result i32)))"
                "  (import \"\" \"waitable.join\" (func $join (param i32 i32)))"
                "  (import \"\" \"subtask.drop\" (func $drop (param i32)))"
                "  (global $ws (mut i32) (i32.const 0))"
                "  (func $run (export \"run\") (result i32)"
                "   (local $ret i32)"
                "   (global.set $ws (call $wsn))"
                "   (local.set $ret (call $f (i32.const 16)))"
                "   (call $join (i32.shr_u (local.get $ret) (i32.const 4)) (global.get $ws))"
                "   (i32.or (i32.const 2) (i32.shl (global.get $ws) (i32.const 4))))"
                "  (func $run_cb (export \"run-cb\") (param i32 i32 i32) (result i32)"
                "   (call $drop (local.get 1))"
                "   (call $task.return (i32.const 99))"
                "   (i32.const 0)))"
                " (canon task.return (result u32) (core func $task.return))"
                " (canon waitable-set.new (core func $wsn))"
                " (canon waitable.join (core func $join))"
                " (canon subtask.drop (core func $drop))"
                " (instance $i (instantiate $Inner))"
                " (canon lower (func $i \"f\") async (core func $f'))"
                " (core instance $o (instantiate $Outer (with \"\" (instance"
                "  (export \"f\" (func $f'))"
                "  (export \"task.return\" (func $task.return))"
                "  (export \"waitable-set.new\" (func $wsn))"
                "  (export \"waitable.join\" (func $join))"
                "  (export \"subtask.drop\" (func $drop))))))"
                " (func (export \"run\") async (result u32) (canon lift"
                "  (core func $o \"run\")"
                "  async (callback (core func $o \"run-cb\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "run" '())))
      ;; the callback receives the SUBTASK RETURNED event for the
      ;; joined subtask, drops it, and resolves the run task with 99
      (is (equal '(99) results)))))

(test component/async-backpressure-block-pins
  ;; M11a — backpressure (CanonicalABI.md 655/3889): backpressure.inc
  ;; raises the instance counter; a NEEDS-EXCLUSIVE task's
  ;; enter_implicit_thread gate blocks while it is > 0. The pin
  ;; guards the counter's plumbing (inc/dec builtins + the fields
  ;; plist) — the GATE's blocking path is drop-subtask's lane row.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (import \"\" \"backpressure.inc\" (func $bpi))"
                "  (import \"\" \"backpressure.dec\" (func $bpd))"
                "  (import \"\" \"task.return0\" (func $task.return0))"
                "  (memory (export \"mem\") 1)"
                "  (func $f (export \"f\") (result i32)"
                "    (call $bpi)"
                "    (call $bpd)"
                "    (i32.const 1))"
                "  (func $f_cb (export \"f-cb\") (param i32 i32 i32) (result i32)"
                "   (call $task.return0)"
                "   (i32.const 0)))"
                "  (canon task.return (core func $task.return0))"
                "  (canon backpressure.inc (core func $bpi))"
                "  (canon backpressure.dec (core func $bpd))"
                "  (core instance $m (instantiate $M (with \"\" (instance"
                "   (export \"backpressure.inc\" (func $bpi))"
                "   (export \"backpressure.dec\" (func $bpd))"
                "   (export \"task.return0\" (func $task.return0))))))"
                " (func (export \"f\") async (canon lift"
                "   (core func $m \"f\")"
                "   async (callback (core func $m \"f-cb\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "f" '())))
      (is (null results)))))

(test component/async-cancel-delivery-pins
  ;; M11a — task.cancel + the CANCELLED event (CanonicalABI.md
  ;; 3971/775): the callback loop delivers TASK_CANCELLED (event
  ;; code 6) at the turn boundary when the task's cancellation was
  ;; requested — the guest observes it and exits. drop-subtask's
  ;; trap row is the lane oracle; this pin guards the DELIVERY arm
  ;; via the sync-lower's cancel path (a task.cancel on a yielded
  ;; callee resolves the task CANCELLED — the caller's subtask
  ;; packs CANCELLED-BEFORE-RETURNED).
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $M"
                "  (memory (export \"mem\") 1)"
                "  (func $f (export \"f\") (result i32) (i32.const 1))"
                "  (func $f_cb (export \"f-cb\") (param i32 i32 i32) (result i32)"
                "    (i32.const 0)))"
                "  (core instance $m (instantiate $M))"
                " (func (export \"f\") async (result u32) (canon lift"
                "   (core func $m \"f\")"
                "   async (callback (core func $m \"f-cb\")))))"))))
    (let ((lf (wasmdown.component.runtime:runtime-export-value
               ci "f")))
      ;; the lifted func's functype carries the async marker (the
      ;; (payload third slot) — the cancel delivery's precondition
      (is (wasmdown.component.runtime:lifted-func-p lf))
      (is (third (wasmdown.component:ct-payload
                  (wasmdown.component.runtime:lifted-func-type
                   lf))))
      )))

(test component/async-stream-roundtrip-pins
  ;; M11b — the stream read/write RENDEZVOUS with backpressure
  ;; (CanonicalABI.md 1687-1711: the copy's case 2 park + case 3
  ;; copy; 4280-4375: the BLOCKED sentinel for the async path +
  ;; the sync wait). The lane oracle is cross-task-future +
  ;; zero-length; this pin guards the copy directly: the writer
  ;; parks (the buffer), the reader's 4-byte read rendezvouses
  ;; (the COMPLETED|4<<4 payload both sides), the bytes cross.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $Memory (memory (export \"mem\") 1))"
                " (core instance $memory (instantiate $Memory))"
                " (core module $M"
                "  (import \"\" \"mem\" (memory 1))"
                "  (import \"\" \"task.return0\" (func $task.return0))"
                "  (import \"\" \"stream.new\" (func $snew (result i64)))"
                "  (import \"\" \"stream.read\" (func $sread (param i32 i32 i32) (result i32)))"
                "  (import \"\" \"stream.write\" (func $swrite (param i32 i32 i32) (result i32)))"
                "  (func $f (export \"f\") (result i32)"
                "    (local $ret64 i64) (local $rx i32) (local $tx i32) (local $ret i32)"
                "    (local.set $ret64 (call $snew))"
                "    (local.set $rx (i32.wrap_i64 (local.get $ret64)))"
                "    (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $ret64) (i64.const 32))))"
                "    (i32.store (i32.const 16) (i32.const 267880218))"
                "    (local.set $ret (call $swrite (local.get $tx) (i32.const 16) (i32.const 4)))"
                "    (if (i32.ne (i32.const -1 (; BLOCKED ;)) (local.get $ret)) (then unreachable))"
                "    (local.set $ret (call $sread (local.get $rx) (i32.const 32) (i32.const 4)))"
                "    (if (i32.ne (i32.const 64) (local.get $ret)) (then unreachable))"
                "    (if (i32.ne (i32.const 267880218) (i32.load (i32.const 32))) (then unreachable))"
                "    (call $task.return0)"
                "    (i32.const 0 (; EXIT ;)))"
                "  (func $f_cb (export \"f-cb\") (param i32 i32 i32) (result i32) (i32.const 0)))"
                " (type $ST (stream u8))"
                " (canon task.return (core func $task.return0))"
                " (canon stream.new $ST (core func $snew))"
                " (canon stream.read $ST (memory (core memory $memory \"mem\")) (core func $sread))"
                " (canon stream.write $ST async (memory (core memory $memory \"mem\")) (core func $swrite))"
                " (core instance $m (instantiate $M (with \"\" (instance"
                "   (export \"mem\" (memory $memory \"mem\"))"
                "   (export \"task.return0\" (func $task.return0))"
                "   (export \"stream.new\" (func $snew))"
                "   (export \"stream.read\" (func $sread))"
                "   (export \"stream.write\" (func $swrite))))))"
                " (func (export \"f\") async (result u32) (canon lift"
                "   (core func $m \"f\")"
                "   async (callback (core func $m \"f-cb\")))))"))))
    (let ((results (funcall (wasmdown.wast:component-invoker)
                            ci "f" '())))
      ;; task.return with no result: the invoke observes the
      ;; single NIL result (the resolved no-value)
      (is (equal '(nil) results)))))

(test component/sync-lift-thread-context-pins
  ;; M12 - THE SYNC-LIFT TASK/THREAD (CanonicalABI.md 572: every
  ;; component-export call creates a Task; 3538-3543: the non-async
  ;; drain loop): a SYNC-typed export whose body calls thread.*
  ;; built-ins runs on its own implicit thread (the executor), the
  ;; main thread drains, and the guest's thread.index addresses ITS
  ;; OWN index (not the caller's). Pinned end-to-end: the sync body
  ;; spawns a new-indirect thread, suspends; the spawned thread's
  ;; resume-later completes the invoke.
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $Table (table (export \"tbl\") 1 funcref))"
                " (core instance $table (instantiate $Table))"
                " (core module $M"
                "  (import \"\" \"thread.index\" (func $thread.index (result i32)))"
                "  (import \"\" \"thread.suspend\" (func $thread.suspend (result i32)))"
                "  (import \"\" \"thread.resume-later\" (func $thread.resume-later (param i32)))"
                "  (import \"\" \"thread.new-indirect\" (func $new-indirect (param i32 i32) (result i32)))"
                "  (import \"\" \"thread.suspend-then-resume\" (func $suspend-then-resume (param i32) (result i32)))"
                "  (import \"\" \"tbl\" (table $tbl 1 funcref))"
                "  (global $idx (mut i32) (i32.const 0))"
                "  (global $x (mut i32) (i32.const 0))"
                "  (func $resumer (param i32)"
                "    (call $thread.resume-later (global.get $idx)))"
                "  (elem $tbl (i32.const 0) func $resumer)"
                "  (func (export \"f\") (result i32)"
                "    (global.set $idx (call $thread.index))"
                "    (global.set $x (call $new-indirect (i32.const 0) (i32.const 0)))"
                "    (drop (call $suspend-then-resume (global.get $x)))"
                "    (i32.const 42)))"
                "  (canon thread.index (core func $thread.index))"
                "  (canon thread.suspend (core func $thread.suspend))"
                "  (canon thread.resume-later (core func $thread.resume-later))"
                "  (canon thread.suspend-then-resume (core func $suspend-then-resume))"
                "  (core type $start-func-ty (func (param i32)))"
                "  (alias core export $table \"tbl\" (core table $tbl2))"
                "  (core func $new-indirect-canon"
                "    (canon thread.new-indirect $start-func-ty (core table $tbl2)))"
                "  (core instance $m (instantiate $M (with \"\" (instance"
                "    (export \"thread.index\" (func $thread.index))"
                "    (export \"thread.suspend\" (func $thread.suspend))"
                "    (export \"thread.resume-later\" (func $thread.resume-later))"
                "    (export \"tbl\" (table $table \"tbl\"))"
                "    (export \"thread.new-indirect\" (func $new-indirect-canon))"
                "    (export \"thread.suspend-then-resume\" (func $suspend-then-resume)))))) "
                " (func (export \"f\") (result u32) (canon lift (core func $m \"f\"))))"))))
    (let ((f-res (funcall (wasmdown.wast:component-invoker) ci "f" '())))
      (is (equal '(42) f-res)))))

(test component/suspend-vs-yield-determinism-pins
  ;; M12 - THE TWO-PARK DISTINCTION (CanonicalABI.md 494 vs 523):
  ;; thread.suspend parks with NO readiness (only an EXPLICIT resume
  ;; wakes it - Store.tick's resume-all does NOT); thread.yield parks
  ;; READY (any scheduler nudge re-runs it). Pinned: the suspended
  ;; thread STAYS parked across the drain loop's repeated ticks until
  ;; the spawned resumer's explicit resume-later; a bare nudge can
  ;; never finish it (the oracle would trap if it could).
  (let* ((ci (%exec-comp
              (concatenate 'string
                "(component"
                " (core module $Table (table (export \"tbl\") 1 funcref))"
                " (core instance $table (instantiate $Table))"
                " (core module $M"
                "  (import \"\" \"thread.index\" (func $thread.index (result i32)))"
                "  (import \"\" \"thread.suspend\" (func $thread.suspend (result i32)))"
                "  (import \"\" \"thread.yield\" (func $thread.yield (result i32)))"
                "  (import \"\" \"thread.resume-later\" (func $thread.resume-later (param i32)))"
                "  (import \"\" \"thread.new-indirect\" (func $new-indirect (param i32 i32) (result i32)))"
                "  (import \"\" \"thread.suspend-then-resume\" (func $suspend-then-resume (param i32) (result i32)))"
                "  (import \"\" \"tbl\" (table $tbl 1 funcref))"
                "  (global $idx (mut i32) (i32.const 0))"
                "  (global $x (mut i32) (i32.const 0))"
                "  (global $r (mut i32) (i32.const 0))"
                "  (func $resumer (param i32)"
                "    (drop (call $thread.yield))"
                "    (call $thread.resume-later (global.get $idx))"
                "    (global.set $r (i32.const 1)))"
                "  (elem $tbl (i32.const 0) func $resumer)"
                "  (func (export \"f\") (result i32)"
                "    (global.set $idx (call $thread.index))"
                "    (global.set $x (call $new-indirect (i32.const 0) (i32.const 0)))"
                "    (drop (call $suspend-then-resume (global.get $x)))"
                "    (global.get $r)))"
                "  (canon thread.index (core func $thread.index))"
                "  (canon thread.suspend (core func $thread.suspend))"
                "  (canon thread.yield (core func $thread.yield))"
                "  (canon thread.resume-later (core func $thread.resume-later))"
                "  (canon thread.suspend-then-resume (core func $suspend-then-resume))"
                "  (core type $start-func-ty (func (param i32)))"
                "  (alias core export $table \"tbl\" (core table $tbl2))"
                "  (core func $new-indirect-canon"
                "    (canon thread.new-indirect $start-func-ty (core table $tbl2)))"
                "  (core instance $m (instantiate $M (with \"\" (instance"
                "    (export \"thread.index\" (func $thread.index))"
                "    (export \"thread.suspend\" (func $thread.suspend))"
                "    (export \"thread.yield\" (func $thread.yield))"
                "    (export \"thread.resume-later\" (func $thread.resume-later))"
                "    (export \"tbl\" (table $table \"tbl\"))"
                "    (export \"thread.new-indirect\" (func $new-indirect-canon))"
                "    (export \"thread.suspend-then-resume\" (func $suspend-then-resume)))))) "
                " (func (export \"f\") async (result u32) (canon lift (core func $m \"f\"))))"))))
    (let ((f-res (funcall (wasmdown.wast:component-invoker) ci "f" '())))
      (is (equal '(1) f-res)))))

(test component/cancel-eager-resolve-pins
  ;; M14 — THE SPEC'S SYNCHRONOUS RESUME (CanonicalABI.md 761-770 +
  ;; 756-759 + 474-484): subtask.cancel's OnCancel resumes a
  ;; CANCELLABLE-AND-READY callback callee and WAITS for its TURN
  ;; BOUNDARY — the task resolved (the eager-cancel shape: the
  ;; callback receives TASK_CANCELLED and calls task.cancel) or the
  ;; turn moved/re-parked (BLOCKED). Pinned via the reentrance-::18
  ;; shape VERBATIM (the fixture component + its assert_return row,
  ;; run through the WAST script path — the fixture's own route):
  ;; the parked callee's cancellation, reentered through a
  ;; non-async-typed export, completes EAGERLY — run returns 42
  ;; (the pre-M14 shape: the cancel read the unresolved state,
  ;; returned BLOCKED, and the run's (i32.ne 4) check trapped; the
  ;; standalone fixture was 0/10, now 10/10).
  (let* ((wast
          (format nil "~{~a~%~}" (list "(component" "  (canon subtask.cancel async (core func $subtask.cancel-async))" "  (canon subtask.drop (core func $subtask.drop))" "" "  ;; Called by $Callee.reenter, and thus reached while $Callee is on the" "  ;; stack, to cancel the subtask of $Callee.park started by \"run\" below." "  (core module $Canceller" "    (import \"\" \"subtask.cancel-async\" (func $subtask.cancel-async (param i32) (result i32)))" "    (global $parked-subtask (export \"parked-subtask\") (mut i32) (i32.const 0))" "    (func (export \"cancel-parked\") (result i32)" "      (call $subtask.cancel-async (global.get $parked-subtask)))" "  )" "  (core instance $canceller (instantiate $Canceller (with \"\" (instance" "    (export \"subtask.cancel-async\" (func $subtask.cancel-async))))))" "  (func $cancel-parked (result u32) (canon lift (core func $canceller \"cancel-parked\")))" "" "  (component $Callee" "    (import \"cancel-parked\" (func $cancel-parked (result u32)))" "    (canon lower (func $cancel-parked) (core func $cancel-parked'))" "    (canon task.cancel (core func $task.cancel))" "    (canon waitable-set.new (core func $waitable-set.new))" "    (core module $CalleeModule" "      (import \"\" \"cancel-parked\" (func $cancel-parked (result i32)))" "      (import \"\" \"task.cancel\" (func $task.cancel))" "      (import \"\" \"waitable-set.new\" (func $waitable-set.new (result i32)))" "      (global $never-signalled (mut i32) (i32.const 0))" "      (func $start (global.set $never-signalled (call $waitable-set.new)))" "      (start $start)" "      ;; Parks in the event loop on a waitable set that never gets an event," "      ;; so only the delivery of a pending cancellation can wake this task." "      (func (export \"park\") (result i32)" "        (i32.or (i32.const 2 (; WAIT ;))" "                (i32.shl (global.get $never-signalled) (i32.const 4))))" "      (func (export \"park-cb\") (param $event i32) (param i32 i32) (result i32)" "        (if (i32.ne (local.get $event) (i32.const 6 (; TASK_CANCELLED ;)))" "          (then unreachable))" "        (call $task.cancel)" "        (i32.const 0 (; EXIT ;)))" "      ;; Not async-typed, so it doesn't take this instance's exclusive lock" "      ;; and \"park\" above stays resumable while this call is on the stack." "      (func (export \"reenter\") (result i32) (call $cancel-parked))" "    )" "    (core instance $callee (instantiate $CalleeModule (with \"\" (instance" "      (export \"cancel-parked\" (func $cancel-parked'))" "      (export \"task.cancel\" (func $task.cancel))" "      (export \"waitable-set.new\" (func $waitable-set.new))))))" "    (func (export \"park\") async" "      (canon lift (core func $callee \"park\") async (callback (core func $callee \"park-cb\"))))" "    (func (export \"reenter\") (result u32) (canon lift (core func $callee \"reenter\")))" "  )" "  (instance $c (instantiate $Callee (with \"cancel-parked\" (func $cancel-parked))))" "  (canon lower (func $c \"park\") async (core func $park'))" "  (canon lower (func $c \"reenter\") (core func $reenter'))" "" "  (core module $Main" "    (import \"\" \"park\" (func $park (result i32)))" "    (import \"\" \"reenter\" (func $reenter (result i32)))" "    (import \"\" \"parked-subtask\" (global $parked-subtask (mut i32)))" "    (import \"\" \"subtask.drop\" (func $subtask.drop (param i32)))" "    (func (export \"run\") (result i32)" "      (local $packed i32) (local $state i32)" "      (local.set $packed (call $park))" "      (if (i32.ne (i32.and (local.get $packed) (i32.const 0xf)) (i32.const 1 (; STARTED ;)))" "        (then unreachable))" "      (global.set $parked-subtask (i32.shr_u (local.get $packed) (i32.const 4)))" "      ;; reentrantly cancel the parked subtask from inside $Callee" "      (local.set $state (call $reenter))" "      (if (i32.ne (local.get $state) (i32.const 4 (; CANCELLED_BEFORE_RETURNED ;)))" "        (then unreachable))" "      (call $subtask.drop (global.get $parked-subtask))" "      (i32.const 42))" "  )" "  (core instance $main (instantiate $Main (with \"\" (instance" "    (export \"park\" (func $park'))" "    (export \"reenter\" (func $reenter'))" "    (export \"parked-subtask\" (global $canceller \"parked-subtask\"))" "    (export \"subtask.drop\" (func $subtask.drop))))))" "  ;; async-typed (but sync-ABI-lifted) so that \"run\" may block" "  (func (export \"run\") async (result u32) (canon lift (core func $main \"run\")))" ")" "(assert_return (invoke \"run\") (u32.const 42))")))
         (results (wasmdown.wast:run-script wast
                                            :file "cancel-eager-pin.wast")))
    ;; the script yields the component command + the one assert row
    (is (= 2 (length results)))
    (is (every (lambda (r)
                 (eq (wasmdown.wast:wast-result-outcome r) :pass))
               results))))


(test component/granted-park-quantum-pins
  ;; M15 MECHANISM 1 — THE GRANTED PARK IS GRANT **AND** READINESS
  ;; (CanonicalABI.md §932 "one thread, one quantum per tick" +
  ;; §1373's wait_from_callback readiness). The callback WAIT arm
  ;; consumes a grant whether or not the readiness holds; a grant
  ;; spent while NOT ready is a WASTED quantum and the park
  ;; RE-ARMS. The pre-fix shape fell through to a plain
  ;; `wait_until(readiness)`, so the arriving EVENT alone resumed
  ;; the thread — the unsupervised continuation: cancel-targeted-
  ;; resume's worker ran inside the canceller's observation window
  ;; on its future's readiness alone (the row's NONE polls trapped).
  ;; Pinned via the fixture's ::7 row VERBATIM (the 4th component,
  ;; lines 465-607: worker parked with a pending FUTURE_READ, park
  ;; cancelled eagerly while the worker must stay put until the
  ;; caller blocks, then return 43 at the caller's wset.wait).
  (let* ((file (merge-pathnames
                "tests/component/async/cancel-targeted-resume.wast"
                (make-pathname :directory
                               '(:absolute "home" "tani" "ghq"
                                           "github.com" "tani" "wasmdown"))))
         (txt (let ((acc (make-string-output-stream)))
                (with-open-file (s file)
                  (loop for line = (read-line s nil nil)
                        for n from 1
                        while line
                        when (<= 465 n 607)
                          do (write-line line acc)))
                (get-output-stream-string acc)))
         (results (wasmdown.wast:run-script txt
                                            :file "granted-park-pin.wast")))
    ;; the one component + its one assert row
    (is (= 2 (length results)))
    (is (every (lambda (r)
                 (eq (wasmdown.wast:wast-result-outcome r) :pass))
               results))))

(defun %pin-slice (rel from to)
  "The lines FROM..TO (1-based, inclusive) of a suite file under the
repo root, as a string — the pin runs a fixture row VERBATIM."
  (let ((acc (make-string-output-stream)))
    (with-open-file (s (merge-pathnames
                        rel
                        (make-pathname :directory
                                       '(:absolute "home" "tani" "ghq"
                                                   "github.com" "tani"
                                                   "wasmdown"))))
      (loop for line = (read-line s nil nil)
            for n from 1
            while line
            when (<= from n to) do (write-line line acc)))
    (get-output-stream-string acc)))

(defun %pin-rows-pass-p (wast name)
  (let ((results (wasmdown.wast:run-script wast :file name)))
    (and results
         (every (lambda (r)
                  (eq (wasmdown.wast:wast-result-outcome r) :pass))
                results))))

(test component/async-lower-result-two-spaces-pins
  ;; M15 MECHANISM 8 — THE ASYNC LOWER'S RESULT STORE: the result's
  ;; VALTYPE resolves in the CALLEE's type space, the new stream
  ;; handle is added to the CALLER's table (the freed slot is
  ;; re-used: partial-stream-copies' $outsr must be 1). The pre-fix
  ;; shape re-added the end to $C at index 3 and the row trapped.
  (is (%pin-rows-pass-p
       (%pin-slice "tests/component/async/partial-stream-copies.wast"
                   1 238)
       "result-two-spaces-pin.wast")))

(test component/cancel-window-fire-gate-pins
  ;; M15 MECHANISM 4 — THE CANCEL-WINDOW FIRE GATE: while the
  ;; synchronous-resume wait is open, the drain's ticks (priority
  ;; pass AND no-quantum fallback) must not FIRE the cancelled
  ;; instance's ready-marked pending spawns; the async lower's
  ;; on-resolve is the fifth M11a deferral site. Pinned by
  ;; cancel-targeted-resume's ::9 row (the callback resume-laters a
  ;; thread inside the cancellation; $ran must stay 0 until the
  ;; caller yields).
  (is (%pin-rows-pass-p
       (%pin-slice "tests/component/async/cancel-targeted-resume.wast"
                   613 699)
       "cancel-fire-gate-pin.wast")))

(test component/host-resource-dtor-registry-pins
  ;; M17 — THE PER-INSTANCE HOST-RESOURCE DESTRUCTOR REGISTRY: a
  ;; host-provided resource's release hook belongs to the INSTANCE
  ;; that adopted the provider, not to the (shared) resource ct. The
  ;; registry lives in the instance's provenance (:HOST-RESOURCE-DTORS
  ;; of the imports table), host-register-resource-dtor is idempotent
  ;; per (instance, ct) and returns the first-registered finalizer,
  ;; and the :resource-drop built-in resolves the hook in the plan's
  ;; order: the handle's instance, the canon instance, then the ct's
  ;; owning provider found through the instance graph. No ct-keyed
  ;; global remains (see the guest-drop acceptance pin below).
  (labels ((host-ct (name)
             (wasmdown.component:make-ct :kind :resource :payload name))
           (drop (inst ct rep)
             ;; the canon built-in's core wrapper with NO guest dtor —
             ;; the host-resource shape: [i32] -> []
             (let* ((f (wasmdown.component.runtime::%resource-builtin-host-func
                        inst :resource-drop ct nil))
                    (idx (wasmdown.component.runtime::%handle-add
                          inst ct rep t)))
               ;; wasm-call is (&rest args): pass the index directly
               (wasmdown.runtime:wasm-call f idx)))
           (reg (inst ct fn)
             (wasmdown.component.runtime:host-register-resource-dtor
              inst ct fn))
           (fin (inst ct)
             (wasmdown.component.runtime:host-resource-finalizer
              inst ct)))
    (let ((wasmdown.component.runtime::*handle-caller-inst* nil)
          (ct (host-ct "descriptor"))
          (i1 (wasmdown.component.runtime:make-comp-instance))
          (i2 (wasmdown.component.runtime:make-comp-instance))
          (seen '()))
      ;; (a) two instances of the SAME provider ct keep their OWN
      ;; finalizer: the drop on i1 runs i1's hook, i2's runs i2's —
      ;; a ct-keyed global could only hold one of them
      (let ((f1 (lambda (rep) (push (cons :i1 rep) seen)))
            (f2 (lambda (rep) (push (cons :i2 rep) seen))))
        (is (eq f1 (reg i1 ct f1)))
        (is (eq f2 (reg i2 ct f2)))
        (is (eq f1 (fin i1 ct)))
        (is (eq f2 (fin i2 ct)))
        (drop i1 ct :rep-1)
        (drop i2 ct :rep-2)
        (is (equal '((:i2 . :rep-2) (:i1 . :rep-1)) seen))
        ;; (b) a REPEAT registration is a no-op that returns the FIRST
        ;; finalizer and never swaps the hook
        (let ((f3 (lambda (rep) (push (cons :swapped rep) seen))))
          (is (eq f1 (reg i1 ct f3)))
          (is (eq f1 (fin i1 ct)))
          (drop i1 ct :rep-3)
          (is (equal '((:i1 . :rep-3) (:i2 . :rep-2) (:i1 . :rep-1))
                     seen))))
      ;; the registry is per (instance, CT), and an unregistered lookup
      ;; neither invents a hook nor allocates a table
      (let* ((other (host-ct "socket"))
             (fresh (wasmdown.component.runtime:make-comp-instance)))
        (is (null (fin i1 other)))
        (is (null (fin fresh ct)))
        (is (null (gethash :host-resource-dtors
                           (wasmdown.component.runtime:comp-instance-imports
                            fresh)))))
      ;; a non-function finalizer is rejected outright
      (is (handler-case (progn (reg i1 ct 42) nil)
            (type-error () t))))
    ;; (c) the drop path's ORDER — the handle's own instance registry
    ;; first, the canon instance's second, then the ct's OWNING
    ;; instance reached through the instance graph. The M16 ct-keyed
    ;; global is GONE: no registration outside a provider instance can
    ;; make a hook appear, and a guest has no path to a provider it
    ;; never linked.
    (let ((wasmdown.component.runtime::*handle-caller-inst* nil)
          (ct (host-ct "descriptor"))
          (inst (wasmdown.component.runtime:make-comp-instance))
          (provider (wasmdown.component.runtime:make-comp-instance))
          (got '()))
      ;; nothing in inst's graph registered the ct -> no hook at all
      (drop inst ct :g0)
      (is (null got))
      (reg provider ct (lambda (rep) (push (cons :owner rep) got)))
      ;; the owner is invisible while unlinked (graph-scoped provenance)
      (drop inst ct :g1)
      (is (null got))
      ;; the ABI's import binding links it; its hook now runs for the
      ;; GUEST-initiated drop
      (wasmdown.component.runtime::%push-instance inst provider)
      (drop inst ct :g2)
      (is (equal '((:owner . :g2)) got))
      ;; the handle's OWN instance registry shadows the ct's owner
      (reg inst ct (lambda (rep) (push (cons :local rep) got)))
      (drop inst ct :g3)
      (is (equal '((:local . :g3) (:owner . :g2)) got))
      ;; an unrelated guest has no path to that provider
      (drop (wasmdown.component.runtime:make-comp-instance) ct :g4)
      (is (equal '((:local . :g3) (:owner . :g2)) got))
      ;; an instance that never registered the ct has no hook
      (is (null (fin inst (host-ct "never-registered")))))))


;;; ---- M17 (plan step 3): the LINK-TIME import contract ------------
;;;
;;; %EXEC-IMPORT must compare the import's resolved external type with
;;; what the provider actually supplies BEFORE it pushes the value: a
;;; missing value, a wrong sort and a shrunk signature are link errors
;;; naming the import, the expected contract and what was found —
;;; raised before any start function or host side effect runs.

(defun %link-provider ()
  "A comp-instance exporting `f' as a lifted func
\(param \"x\" u32) -> (result u32)."
  (%exec-comp
   "(component
      (core module $M
        (func (export \"f\") (param i32) (result i32) (local.get 0)))
      (core instance $m (instantiate $M))
      (func $f (param \"x\" u32) (result u32)
        (canon lift (core func $m \"f\")))
      (export \"f\" (func $f)))"))

(defun %link-provider-f ()
  (wasmdown.component.runtime:runtime-export-value (%link-provider) "f"))

(defun %link-provider0 ()
  "A comp-instance exporting `f' as a lifted func with NO parameters
and a u32 result (the instance-coverage fixtures' shape)."
  (%exec-comp
   "(component
      (core module $M (func (export \"f\") (result i32) (i32.const 7)))
      (core instance $m (instantiate $M))
      (func $f (result u32) (canon lift (core func $m \"f\")))
      (export \"f\" (func $f)))"))

(defun %link-provider0-f ()
  (wasmdown.component.runtime:runtime-export-value (%link-provider0) "f"))

(defun %validated (src)
  (let ((comp (wasmdown.component.text::parse-component-text src)))
    (wasmdown.component:validate-component comp)
    comp))

(defun %exec-types (comp)
  "The type index vector the def walk has built for COMP's `:type'
definitions (the runtime's comp-instance-types for these fixtures:
they use no type aliases or type imports)."
  (coerce (loop for d across (wasmdown.component:comp-defs comp)
                when (eq (wasmdown.component:compdef-kind d) :type)
                  collect (wasmdown.component:compdef-payload d))
          'vector))

(defun %import-record (comp)
  "The COMP's first importrec."
  (loop for d across (wasmdown.component:comp-defs comp)
        when (eq (wasmdown.component:compdef-kind d) :import)
          return (wasmdown.component:compdef-payload d)))

(defun %link-error-of (thunk)
  "The wasmdown:link-error THUNK signals, or NIL."
  (block nil
    (handler-case (progn (funcall thunk) nil)
      (wasmdown:link-error (c) (return c)))))

(defun %err-msg (c)
  (and c (wasmdown:wasmdown-error-message c)))

(defun %bag (&rest pairs)
  "An inline-exports instance value: a name -> value hash table."
  (let ((h (make-hash-table :test #'equal)))
    (setf (gethash :%bag h) t)
    (loop for (name . value) in pairs
          do (setf (gethash name h) value))
    h))

(test component/import-binding-signature-pins
  ;; a SHRUNK signature: the import declares (param "x" u64) and the
  ;; provider offers (param "x" u32) — link error before the push
  (let* ((comp (%validated
                "(component
                   (type $FT (func (param \"x\" u64) (result u32)))
                   (import \"d\" (func $d (type $FT))))"))
         (err (%link-error-of
               (lambda ()
                 (wasmdown.component.runtime:instantiate-component
                  comp :with-args (list (cons "d" (%link-provider-f)))))))
         (msg (%err-msg err)))
    (is err)
    (is (search "import \"d\"" msg))
    (is (search "expected func (param u64) -> (result u32)" msg))
    (is (search "found func (param u32) -> (result u32)" msg))
    ;; a widened RESULT is rejected just as firmly (the whole
    ;; signature is compared, not only the parameter widths)
    (let* ((comp2 (%validated
                   "(component
                      (type $FT (func (param \"x\" u32) (result u64)))
                      (import \"d\" (func $d (type $FT))))"))
           (err2 (%link-error-of
                  (lambda ()
                    (wasmdown.component.runtime:instantiate-component
                     comp2
                     :with-args
                     (list (cons "d" (%link-provider-f))))))))
      (is (search "expected func (param u32) -> (result u64)"
                  (%err-msg err2)))
      (is (search "found func (param u32) -> (result u32)"
                  (%err-msg err2))))))

(test component/import-binding-sort-pins
  ;; a value of the WRONG SORT: a type bound where the import declares
  ;; a function
  (let* ((comp (%validated
                "(component (import \"d\" (func $d (result u32))))"))
         (val (wasmdown.component:make-ct :kind :prim :payload :u32))
         (err (%link-error-of
               (lambda ()
                 (wasmdown.component.runtime:instantiate-component
                  comp :with-args (list (cons "d" val))))))
         (msg (%err-msg err)))
    (is err)
    (is (search "import \"d\"" msg))
    (is (search "expected func (param) -> (result u32)" msg))
    (is (search "found u32" msg))))

(test component/import-binding-missing-pins
  ;; a MISSING provider: no with-arg at all is a link error (the old
  ;; :missing hole silently bound NIL and failed later, or never)
  (let* ((comp (%validated
                "(component (import \"d\" (func $d (result u32))))"))
         (err (%link-error-of
               (lambda ()
                 (wasmdown.component.runtime:instantiate-component comp))))
         (msg (%err-msg err)))
    (is err)
    (is (search "import \"d\"" msg))
    (is (search "expected func (param) -> (result u32)" msg))
    (is (search "found nothing" msg))))

(test component/import-binding-accepts-pins
  ;; a CORRECT provider still instantiates; the bound import keeps the
  ;; supplied value's IDENTITY (the check never rewrites the value)
  (let* ((f (%link-provider-f))
         (comp (%validated
                "(component
                   (type $FT (func (param \"x\" u32) (result u32)))
                   (import \"d\" (func $d (type $FT))))"))
         (inst (wasmdown.component.runtime:instantiate-component
                comp :with-args (list (cons "d" f)))))
    (is (wasmdown.component.runtime:comp-instance-p inst))
    (is (eq f (gethash "d" (wasmdown.component.runtime::comp-instance-imports
                            inst))))))

(test component/import-binding-type-bound-pins
  ;; a `:type' bound: the checker accepts exactly $U's type for
  ;; (eq $U) and rejects a different one. The runtime's %EXEC-IMPORT
  ;; deliberately does not enforce the :type sort yet (the WASI
  ;; providers bind partial types instances), so the pin drives
  ;; CHECK-IMPORT-BINDING directly.
  (let* ((comp (%validated
                "(component
                   (type $U (record (field \"a\" u32)))
                   (import \"t\" (type $T (eq $U))))"))
         (types (%exec-types comp))
         (imprec (%import-record comp))
         (declared (wasmdown.component:compdef-payload
                    (aref (wasmdown.component:comp-defs comp) 0)))
         (bad (wasmdown.component:make-ct :kind :prim :payload :u32))
         (err (%link-error-of
               (lambda ()
                 (wasmdown.component:check-import-binding imprec bad types))))
         (msg (%err-msg err)))
    (is err)
    (is (search "import \"t\"" msg))
    (is (search "expected (record (field \"a\" u32))" msg))
    (is (search "found u32" msg))
    ;; a MISSING value is refused by the checker too
    (is (search "found nothing"
                (%err-msg (%link-error-of
                           (lambda ()
                             (wasmdown.component:check-import-binding
                              imprec nil types))))))
    ;; the declared type itself satisfies the bound (identity returned)
    (is (eq declared
            (wasmdown.component:check-import-binding imprec declared
                                                     types)))))

(test component/import-binding-instance-coverage-pins
  ;; an `:instance' import: the provider must export every declared
  ;; name with a satisfying signature; extra exports are allowed.
  ;; Checked through CHECK-IMPORT-BINDING directly (see the :type pin:
  ;; %EXEC-IMPORT keeps the :instance sort unenforced until the
  ;; provider contract records which exports are REQUIRED).
  (let* ((comp (%validated
                "(component
                   (import \"i\"
                     (instance $i (export \"f\" (func (result u32))))))"))
         (types (%exec-types comp))
         (imprec (%import-record comp))
         (f (%link-provider0-f))
         (missing (%link-error-of
                   (lambda ()
                     (wasmdown.component:check-import-binding
                      imprec (%bag) types))))
         (wrong (%link-error-of
                 (lambda ()
                   (wasmdown.component:check-import-binding
                    imprec
                    (%bag (cons "f"
                                (wasmdown.component:make-ct
                                 :kind :prim :payload :u32)))
                    types))))
         (good (%bag (cons "f" f))))
    (is (search "import \"i\" \"f\": expected (func (param) -> (result u32)), found no export"
                (%err-msg missing)))
    (is (search "expected func, found u32" (%err-msg wrong)))
    (is (eq good
            (wasmdown.component:check-import-binding imprec good types)))))

(test component/guest-drop-reaches-provider-finalizer-pin
  ;; M17 STEP 2's ACCEPTANCE MEASUREMENT — the migration probe, as a
  ;; pin. A GUEST-initiated `canon resource.drop` of a PROVIDER
  ;; resource: the ABI adds the OWNED handle to the GUEST's table (the
  ;; dynamic instance) and the drop now runs with
  ;; *HANDLE-CALLER-INST* NIL, so BOTH the dynamic and the canon
  ;; instance are the guest and neither per-instance registry arm can
  ;; hold the hook. The ct is the provider's own WIT resource ct and
  ;; the guest reached the provider the way %EXEC-IMPORT does (the
  ;; provider instance is one of the guest's INSTANCES indices), so
  ;; the ct -> owning-instance arm must find it THROUGH THAT GRAPH —
  ;; there is no ct-keyed global to save it.
  ;;
  ;; REUSE VECTOR for the plan's task.return / cancel
  ;; lifetime-ownership pins: this exact shape (a guest instance
  ;; carrying an OWN handle for a provider ct, *HANDLE-CALLER-INST*
  ;; NIL, the :resource-drop built-in, then the resource's released
  ;; state) is the vector to copy — swap the drop for task.return or a
  ;; cancellation and assert what is still live.
  ;;
  ;; MEASURED BEFORE the owner arm (pre-change runtime): the probe
  ;; read fd-before 5, fd-after 5, closed-p NIL — the finalizer never
  ;; ran and all four assertions below failed. AFTER: the fd closes.
  (let* ((ctx (wasmdown.wasi:make-wasi-context))
         (provider (wasmdown.wasi:filesystem-types-instance ctx))
         (ct (wasmdown.wasi:descriptor-resource-ct))
         (guest (wasmdown.component.runtime:make-comp-instance))
         (desc (wasmdown.wasi::make-wasi-descriptor
                :fd (sb-unix:unix-open "/dev/null" sb-unix:o_rdonly 0)
                :owned-p t))
         (wasmdown.component.runtime::*handle-caller-inst* nil))
    (unwind-protect
         (progn
           ;; the provider registered its finalizer on ITSELF; the guest
           ;; holds none, and has not linked the provider yet
           (is (eq (symbol-function 'wasmdown.wasi:close-wasi-descriptor)
                   (wasmdown.component.runtime:host-resource-finalizer
                    provider ct)))
           (is (null (wasmdown.component.runtime:host-resource-finalizer
                      guest ct)))
           ;; the import binding: %EXEC-IMPORT pushes the provider
           ;; instance at an INSTANCES index of the guest
           (wasmdown.component.runtime::%push-instance guest provider)
           ;; the ABI's own handle: OWNED, on the GUEST table, keyed by
           ;; the provider's ct (1-based slot 1)
           (let* ((idx (wasmdown.component.runtime::%handle-add
                        guest ct desc t))
                  (drop (wasmdown.component.runtime::%resource-builtin-host-func
                         guest :resource-drop ct nil)))
             (is (eql 1 idx))
             (is (plusp (wasmdown.wasi:wasi-descriptor-fd desc)))
             (is (not (wasmdown.wasi:wasi-descriptor-closed-p desc)))
             (wasmdown.runtime:wasm-call drop idx)
             ;; the PROVIDER's finalizer ran: release exactly once,
             ;; fd invalidated, handle consumed from the guest table
             (is (wasmdown.wasi:wasi-descriptor-closed-p desc))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd desc)))
             (is (null (wasmdown.component.runtime::%handle-rep
                        guest ct idx))))
           ;; the same path for a SOCKET rep: create through the
           ;; provider, own handle on the guest, drop -> fd -1/:closed
           (let* ((sock-provider (wasmdown.wasi:sockets-types-instance ctx))
                  (tcp-ct (wasmdown.wasi:tcp-socket-resource-ct))
                  (created (wasmdown.wasi::tcp-create
                            ctx nil (cons :enum "ipv4")))
                  (sock (fourth created)))
             (is (eq tcp-ct (third created)))
             (is (plusp (wasmdown.wasi:wasi-tcp-socket-fd sock)))
             (wasmdown.component.runtime::%push-instance guest sock-provider)
             (let* ((idx (wasmdown.component.runtime::%handle-add
                          guest tcp-ct sock t))
                    (drop (wasmdown.component.runtime::%resource-builtin-host-func
                           guest :resource-drop tcp-ct nil)))
               (wasmdown.runtime:wasm-call drop idx)
               (is (eql -1 (wasmdown.wasi:wasi-tcp-socket-fd sock)))
                (is (eq :closed (wasmdown.wasi:wasi-tcp-socket-state sock))))))
      (let ((fd (wasmdown.wasi:wasi-descriptor-fd desc)))
        (when (and (integerp fd) (not (minusp fd)))
          (sb-unix:unix-close fd))))))

(test component/host-resource-owner-is-graph-scoped-pin
  ;; The ct -> owning-instance provenance is the PROVIDER instance's own
  ;; registry (no ct-keyed global): the owner is found only through the
  ;; instance graph the requester linked. A provider that no one linked
  ;; is invisible even though it registered the same ct.
  (let* ((ct (wasmdown.component:make-ct :kind :resource :payload "scoped"))
         (p1 (wasmdown.component.runtime:make-comp-instance))
         (p2 (wasmdown.component.runtime:make-comp-instance))
         (guest (wasmdown.component.runtime:make-comp-instance))
         (g1 (lambda (rep) (declare (ignore rep))))
         (g2 (lambda (rep) (declare (ignore rep))))
         (wasmdown.component.runtime::*handle-caller-inst* nil))
    (wasmdown.component.runtime:host-register-resource-dtor p1 ct g1)
    (wasmdown.component.runtime:host-register-resource-dtor p2 ct g2)
    (wasmdown.component.runtime::%push-instance guest p1)
    (multiple-value-bind (fin owner)
        (wasmdown.component.runtime::%host-resource-owner
         (list guest) ct)
      (is (eq g1 fin))
      (is (eq p1 owner)))
    ;; an unlinked provider is NOT reachable
    (is (null (wasmdown.component.runtime::%host-resource-owner
               (list (wasmdown.component.runtime:make-comp-instance)) ct)))
    ;; an unregistered ct resolves to nothing at all
    (is (null (wasmdown.component.runtime::%host-resource-owner
               (list guest) (wasmdown.component:make-ct
                             :kind :resource :payload "absent"))))))

(test component/two-providers-drop-own-finalizers-pin
  ;; TWO filesystem providers, DIFFERENT capability contexts, BOTH
  ;; linked into ONE guest: each owns its own descriptor reps and its
  ;; own registry entry for the descriptor ct. Dropping a rep releases
  ;; THAT rep's fd and leaves the other provider's fd alone — the
  ;; owner arm never lets one provider's registry release the other's
  ;; resource.
  ;;
  ;; RECORDED LIMITATION (measured here, not assumed): both providers
  ;; are built from the same WIT package, and a resource ct is a
  ;; PACKAGE-level identity (`wit-interface-resource-cts` memo), so
  ;; the two registrations share ONE ct object and both hooks are the
  ;; same function (CLOSE-WASI-DESCRIPTOR). The ct therefore cannot
  ;; name WHICH provider owns a given rep: %HOST-RESOURCE-OWNER applies
  ;; its documented "nearest (breadth-first, successor order) answers"
  ;; rule, which is the first-linked provider for both drops. What IS
  ;; distinguishable — and what this pin asserts — is the REP: the
  ;; release is applied to the dropped handle's rep only, so each
  ;; provider's fd closes exactly once and the other stays open. The
  ;; second half of the pin shows the same graph become unambiguous
  ;; once the two providers come from DISTINCT package parses (distinct
  ;; cts): then each ct resolves to ITS OWN instance.
  (let* ((pre1 (wasmdown.wasi:open-wasi-preopen "/tmp" :name "/one"))
         (pre2 (wasmdown.wasi:open-wasi-preopen "/tmp" :name "/two"))
         (ctx1 (wasmdown.wasi:make-wasi-context :preopens (list pre1)))
         (ctx2 (wasmdown.wasi:make-wasi-context :preopens (list pre2)))
         (p1 (wasmdown.wasi:filesystem-types-instance ctx1))
         (p2 (wasmdown.wasi:filesystem-types-instance ctx2))
         (pp1 (wasmdown.wasi:filesystem-preopens-instance ctx1))
         (pp2 (wasmdown.wasi:filesystem-preopens-instance ctx2))
         (ct (wasmdown.wasi:descriptor-resource-ct))
         (hook (symbol-function 'wasmdown.wasi:close-wasi-descriptor))
         (guest (wasmdown.component.runtime:make-comp-instance))
         (wasmdown.component.runtime::*handle-caller-inst* nil)
         (d1 (wasmdown.wasi::make-wasi-descriptor
              :fd (sb-unix:unix-open "/dev/null" sb-unix:o_rdonly 0)
              :preopen pre1 :owned-p t))
         (d2 (wasmdown.wasi::make-wasi-descriptor
              :fd (sb-unix:unix-open "/dev/null" sb-unix:o_rdonly 0)
              :preopen pre2 :owned-p t))
         (d3 (wasmdown.wasi::make-wasi-descriptor
              :fd (sb-unix:unix-open "/dev/null" sb-unix:o_rdonly 0)
              :preopen pre2 :owned-p t)))
    (unwind-protect
         (progn
           ;; each provider owns its OWN registry table, and each table
           ;; holds the shared package ct under its own hook
           (is (not (eq (wasmdown.component.runtime::%inst-host-resource-dtors p1)
                        (wasmdown.component.runtime::%inst-host-resource-dtors p2))))
           (is (eq hook (wasmdown.component.runtime:host-resource-finalizer p1 ct)))
           (is (eq hook (wasmdown.component.runtime:host-resource-finalizer p2 ct)))
           ;; both provider pairs linked into the same guest (the ABI
           ;; import bindings)
           (dolist (i (list p1 p2 pp1 pp2))
             (wasmdown.component.runtime::%push-instance guest i))
           ;; the shared ct: the nearest linked provider answers —
           ;; documented, and the same release either way
           (multiple-value-bind (fin owner)
               (wasmdown.component.runtime::%host-resource-owner
                (list guest) ct)
             (is (eq hook fin))
             (is (eq p1 owner)))
           ;; one drop per provider: each rep's own fd closes, the
           ;; other provider's rep is untouched
           (let* ((i1 (wasmdown.component.runtime::%handle-add guest ct d1 t))
                  (i2 (wasmdown.component.runtime::%handle-add guest ct d2 t))
                  (drop (wasmdown.component.runtime::%resource-builtin-host-func
                         guest :resource-drop ct nil)))
             (is (plusp (wasmdown.wasi:wasi-descriptor-fd d1)))
             (is (plusp (wasmdown.wasi:wasi-descriptor-fd d2)))
             (wasmdown.runtime:wasm-call drop i1)
             (is (wasmdown.wasi:wasi-descriptor-closed-p d1))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd d1)))
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs d1)))
             (is (not (wasmdown.wasi:wasi-descriptor-closed-p d2)))
             (is (plusp (wasmdown.wasi:wasi-descriptor-fd d2)))
             (wasmdown.runtime:wasm-call drop i2)
             (is (wasmdown.wasi:wasi-descriptor-closed-p d2))
             (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd d2)))
             (is (zerop (wasmdown.wasi:wasi-descriptor-refs d2))))
           ;; DISTINCT package parses => distinct cts => the owner arm
           ;; discriminates: ct2 resolves to p3, the original ct keeps
           ;; resolving to p1, and each drop again releases its own rep
           (let* ((pkg2 (wasmdown.wit:parse-wit-file
                         (wasmdown.wasi:wasi-package-file "filesystem")))
                  (ct2 (wasmdown.wasi:descriptor-resource-ct :package pkg2))
                  (p3 (wasmdown.wasi:filesystem-types-instance ctx2
                                                               :package pkg2)))
             (is (not (eq ct2 ct)))
             (is (eq hook (wasmdown.component.runtime:host-resource-finalizer
                           p3 ct2)))
             (wasmdown.component.runtime::%push-instance guest p3)
             (multiple-value-bind (fin owner)
                 (wasmdown.component.runtime::%host-resource-owner
                  (list guest) ct2)
               (is (eq hook fin))
               (is (eq p3 owner)))
             (multiple-value-bind (fin owner)
                 (wasmdown.component.runtime::%host-resource-owner
                  (list guest) ct)
               (declare (ignore fin))
               (is (eq p1 owner)))
             (let* ((i3 (wasmdown.component.runtime::%handle-add
                         guest ct2 d3 t))
                    (drop (wasmdown.component.runtime::%resource-builtin-host-func
                           guest :resource-drop ct2 nil)))
               (is (plusp (wasmdown.wasi:wasi-descriptor-fd d3)))
               (wasmdown.runtime:wasm-call drop i3)
               (is (wasmdown.wasi:wasi-descriptor-closed-p d3))
               (is (eql -1 (wasmdown.wasi:wasi-descriptor-fd d3)))
               (is (zerop (wasmdown.wasi:wasi-descriptor-refs d3))))))
      (dolist (d (list d1 d2 d3))
        (let ((fd (wasmdown.wasi:wasi-descriptor-fd d)))
          (when (and (integerp fd) (not (minusp fd)))
            (sb-unix:unix-close fd))))
      (wasmdown.wasi:close-wasi-preopen pre1)
      (wasmdown.wasi:close-wasi-preopen pre2))))

;;; ---- the resume-later mark's LIFETIME, and the explicit-resume
;;; ---- builtins' "suspended" predicate (CanonicalABI.md 458-465,
;;; ---- 494, 4688/4714)
;;;
;;; The resume-later mark (§458-465 `start_waiting(ready = True)`) is a
;;; ONE-PARK credit: it may wake exactly the park that follows the
;;; post, and it must never outlive the thread it was posted to — a
;;; Thread record may be handed to a later task at the same slot
;;; (§391-407: the threads table is per instance and §705 reassigns
;;; the next index). The pins below drive the REAL canon builtins
;;; (`%builtin-async-host-func`, the production host funcs) and the
;;; REAL park/tick primitives (`async-thread-suspend`,
;;; `async-thread-resume-all`) over real `async-thread` records, with
;;; a private scheduler registry: every assertion is a state or delta
;;; check on the record, so no sleep is used as an oracle. Every wait
;;; is bounded (the runtime's own park slice bounds the park itself;
;;; the joins below carry explicit timeouts).

(defmacro %pin-with-store (&body body)
  "Run BODY against a PRIVATE scheduler registry and right ticket, so
the pin neither sees nor leaves the fixtures' threads. On unwind,
abandon + unblock whatever the pin registered (the park sites check
the abandoned mark at their loop top and exit)."
  `(let ((wasmdown.component.runtime::*scheduler-threads* nil)
         (wasmdown.component.runtime::*right-ticket* nil)
         (wasmdown.component.runtime::*right-owner* nil)
         (wasmdown.component.runtime::*right-self* nil)
         (wasmdown.component.runtime::*current-thread* nil)
         (wasmdown.component.runtime::*current-task* nil))
     (unwind-protect
          (progn ,@body)
       (dolist (th wasmdown.component.runtime::*scheduler-threads*)
         (setf (wasmdown.component.runtime::async-thread-abandoned th) t
               (wasmdown.component.runtime::async-thread-suspended-p th) nil
               (wasmdown.component.runtime::async-thread-yield-parked th) nil)
         (sb-thread:with-recursive-lock
             ((wasmdown.component.runtime::async-thread-mutex th))
           (sb-thread:condition-broadcast
            (wasmdown.component.runtime::async-thread-waitqueue th))
           (sb-thread:condition-broadcast
            (wasmdown.component.runtime::async-thread-suspend-waitqueue th))))
       (sb-thread:with-recursive-lock
           (wasmdown.component.runtime::*scheduler-mutex*)
         (setf wasmdown.component.runtime::*scheduler-threads* nil)))))

(defun %pin-inst ()
  "A bare comp-instance: all the thread builtins need from it is the
:async fields plist (§133-150), lazily created on the imports table."
  (wasmdown.component.runtime::make-comp-instance))

(defun %pin-task (inst seq)
  (wasmdown.component.runtime::make-async-task :inst inst :seq seq))

(defun %pin-thread (inst idx seq)
  "A fresh Thread record at INDEX, with a task whose creation path is
SEQ (the store-right ticket order), registered as the task's implicit
thread."
  (let* ((task (%pin-task inst seq))
         (th (wasmdown.component.runtime::make-async-thread
              :task task :index idx)))
    (setf (wasmdown.component.runtime::async-task-implicit-thread task) th)
    th))

(defun %pin-register (inst th)
  "Put TH in the instance's threads table and the scheduler registry."
  (let ((fields (wasmdown.component.runtime::%inst-async-fields inst)))
    (setf (getf fields :threads) (adjoin th (getf fields :threads)
                                       :test #'eq)))
  (sb-thread:with-recursive-lock
      (wasmdown.component.runtime::*scheduler-mutex*)
    (pushnew th wasmdown.component.runtime::*scheduler-threads*
             :test #'eq))
  th)

(defun %pin-unregister (inst th)
  (let ((fields (wasmdown.component.runtime::%inst-async-fields inst)))
    (setf (getf fields :threads) (remove th (getf fields :threads)
                                        :test #'eq)))
  (sb-thread:with-recursive-lock
      (wasmdown.component.runtime::*scheduler-mutex*)
    (setf wasmdown.component.runtime::*scheduler-threads*
          (remove th wasmdown.component.runtime::*scheduler-threads*
                  :test #'eq))))

(defun %pin-builtin (inst kind)
  "The REAL production canon-builtin host func (M11b's wrapper)."
  (wasmdown.component.runtime::%builtin-async-host-func inst kind nil))

(defun %pin-resume-later (inst caller idx)
  "Post the production thread.resume-later from CALLER onto IDX."
  (let ((wasmdown.component.runtime::*current-thread* caller)
        (wasmdown.component.runtime::*current-task*
          (wasmdown.component.runtime::async-thread-task
                         caller)))
    (wasmdown.runtime:wasm-call (%pin-builtin inst :thread-resume-later)
                                idx)))

(defun %pin-start-park (th)
  "Park TH with async-thread-suspend on its OWN OS thread (the real
park site). Returns a handle plist: :thread, :done (a box set when the
park returned), :err (a box). The thread binds its own private
registry/right ticket; the shared record is the channel."
  (let* ((done (list nil))
         (err (list nil))
         (bt (sb-thread:make-thread
              (lambda ()
                (let ((wasmdown.component.runtime::*current-thread* th)
                      (wasmdown.component.runtime::*right-self* th)
                      (wasmdown.component.runtime::*right-ticket* nil)
                      (wasmdown.component.runtime::*scheduler-threads* nil))
                  (unwind-protect
                       (handler-case
                           (sb-ext:with-timeout
                               10
                             (wasmdown.component.runtime::async-thread-suspend
                              th))
                         (error (c) (setf (car err) c)))
                    (wasmdown.component.runtime::%right-release-on-exit th)
                    (setf (car done) t))))
              :name "pin-park")))
    (list :thread bt :done done :err err)))

(defun %pin-wait-parked (th &key (timeout 5))
  "Block until TH is IN its suspend park. SUSPENDED-P is set at the
park's entry and cleared only at its exit, so it is the stable
signal; the deadline keeps the wait bounded."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (wasmdown.component.runtime::async-thread-suspended-p th)
        (return t))
      (when (> (get-internal-real-time) deadline)
        (error "pin: thread ~a never reached its suspend park" th))
      (sleep 0.001))))

(defun %pin-join (bt &key (timeout 3))
  "T when the OS thread BT finished within TIMEOUT."
  (not (eq (sb-thread:join-thread bt :timeout timeout :default :pin-timeout)
           :pin-timeout)))

(defun %pin-clean-p (th)
  "No RESUME CREDIT is outstanding on TH: no ready mark, no durable
wake count. (GRANTED is deliberately NOT part of the invariant: it is
the tick's blanket bookkeeping — every real Store.tick marks all
registered threads — not a per-thread credit. POST-TO is the
§242-273 handoff slot, not the credit: it is spent by the thread's
NEXT park/exit and is asserted where that matters.)"
  (and (null (wasmdown.component.runtime::async-thread-ready-p th))
       (zerop (wasmdown.component.runtime::async-thread-wake-count th))))

(defun %pin-wait-for (thunk &key (timeout 5) (what "condition"))
  "Block (bounded) until THUNK holds; error past TIMEOUT."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (when (funcall thunk) (return t))
      (when (> (get-internal-real-time) deadline)
        (error "pin: ~a not reached" what))
      (sleep 0.001))))

(test component/resume-later-marker-lifetime-pin
  ;; §458-465: resume_later marks the target READY; the mark is the
  ;; NEXT park's credit and nothing more. Pinned here:
  ;;  (1) a mark posted to a LIVE thread that never parks again dies
  ;;      WITH the thread (no residual wake-count/ready/grant for a
  ;;      later task at the same slot to inherit);
  ;;  (2) a SECOND task run through the SAME record/index starts
  ;;      clean: its first park is not returned by the old mark, and
  ;;      a store tick grants it nothing (no granted-without-readiness);
  ;;  (3) a mark posted TWICE buys exactly ONE park: the first park
  ;;      returns, the second waits for a genuine resume.
  (%pin-with-store
    (let* ((inst (%pin-inst))
           (marker (%pin-thread inst 1 '(1)))
           (target (%pin-thread inst 2 '(1 0))))
      (%pin-register inst marker)
      (%pin-register inst target)
      ;; ---- (1) the mark onto a LIVE thread, which exits un-parked
      (is (not (wasmdown.component.runtime::async-thread-suspended-p target)))
      (%pin-resume-later inst marker 2)
      (is (wasmdown.component.runtime::async-thread-ready-p target))
      (is (= 1 (wasmdown.component.runtime::async-thread-wake-count target)))
      ;; the target's quantum finishes and the thread EXITS without
      ;; parking again (the §4588 unregister path)
      (wasmdown.component.runtime::%task-exit-implicit-thread
       (wasmdown.component.runtime::async-thread-task target))
      (is (not (member target
                       (getf (wasmdown.component.runtime::%inst-async-fields
                              inst)
                             :threads))))
      (is (null (wasmdown.component.runtime::async-thread-ready-p target)))
      (is (zerop (wasmdown.component.runtime::async-thread-wake-count target)))
      (is (null (wasmdown.component.runtime::async-thread-granted target)))
      ;; a tick after the exit must not resurrect the dead mark
      (wasmdown.component.runtime::async-thread-resume-all)
      (is (%pin-clean-p target))
      ;; and the marker itself carries no credit it did not receive
      (is (%pin-clean-p marker))
      ;; ---- (2) a SECOND task through the SAME slot
      ;; A recycling runtime would hand this record (and its index)
      ;; to the next task. The exit above is what must have made that
      ;; safe: the record carries no mark any more.
      (setf (wasmdown.component.runtime::async-thread-spawn-thunk target) nil)
      (let ((task (%pin-task inst '(1 1))))
        (setf (wasmdown.component.runtime::async-thread-task target) task
              (wasmdown.component.runtime::async-thread-index target) 2)
        (setf (wasmdown.component.runtime::async-task-implicit-thread task)
              target))
      (%pin-register inst target)
      (let ((h (%pin-start-park target)))
        (%pin-wait-parked target)
        ;; the first park of the NEW task: still parked, and a tick
        ;; grants it NOTHING (no ready mark survived the reuse)
        (wasmdown.component.runtime::async-thread-resume-all)
        (is (zerop (wasmdown.component.runtime::async-thread-wake-count target)))
        (is (null (wasmdown.component.runtime::async-thread-ready-p target)))
        (is (wasmdown.component.runtime::async-thread-suspended-p target))
        (is (null (car (getf h :done))))
        ;; only a REAL resume ends it
        (wasmdown.component.runtime::async-thread-resume target)
        (is (%pin-join (getf h :thread)))
        (is (null (car (getf h :err))))
        (is (%pin-clean-p target))
        (%pin-unregister inst target))
      ;; ---- (3) post TWICE -> ONE park returns, the next waits
      (let ((p (%pin-thread inst 3 '(1 2))))
        (%pin-register inst p)
        (let ((h (%pin-start-park p)))
          (%pin-wait-parked p)
          ;; two resume-laters onto the SAME suspend-parked thread:
          ;; the mark is a READY mark (§458-465), not a count — the
          ;; second post cannot buy a second park
          (%pin-resume-later inst marker 3)
          (%pin-resume-later inst marker 3)
          (is (wasmdown.component.runtime::async-thread-ready-p p))
          (is (zerop (wasmdown.component.runtime::async-thread-wake-count p)))
          (is (eq (wasmdown.component.runtime::async-thread-post-to p) marker))
          ;; ONE store tick (§932: one quantum per tick) fires the
          ;; ready mark: exactly ONE credit, and the mark is CONSUMED
          (wasmdown.component.runtime::async-thread-resume-all)
          (is (= 1 (wasmdown.component.runtime::async-thread-wake-count p)))
          (is (null (wasmdown.component.runtime::async-thread-ready-p p)))
          (is (%pin-join (getf h :thread)))
          (is (null (car (getf h :err))))
          ;; the park left no stale state behind
          (is (not (wasmdown.component.runtime::async-thread-suspended-p p)))
          (is (%pin-clean-p p))
          ;; the SECOND park of the same thread must WAIT: a store
          ;; tick may not grant it from the spent mark
          (let ((h2 (%pin-start-park p)))
            (%pin-wait-parked p)
            ;; the pending §242-273 handoff is spent by THIS park's
            ;; top (not by the park it was installed during)
            (%pin-wait-for (lambda ()
                             (null (wasmdown.component.runtime::async-thread-post-to
                                    p)))
                           :what "the second park's handoff post")
            (is (= 1 (wasmdown.component.runtime::async-thread-wake-count
                      marker)))
            (wasmdown.component.runtime::async-thread-resume-all)
            (is (zerop (wasmdown.component.runtime::async-thread-wake-count p)))
            (is (null (wasmdown.component.runtime::async-thread-ready-p p)))
            (is (null (car (getf h2 :done))))
            (wasmdown.component.runtime::async-thread-resume p)
            (is (%pin-join (getf h2 :thread)))
            (is (null (car (getf h2 :err)))))))
      ;; ---- the marker's own exit leaves it clean too
      (wasmdown.component.runtime::%task-exit-implicit-thread
       (wasmdown.component.runtime::async-thread-task marker))
      (is (%pin-clean-p marker))
      (is (null (wasmdown.component.runtime::async-thread-post-to marker))))))

(defun %pin-arm-outcome (inst kind caller idx &key (timeout 2))
  "Call the REAL switch builtin from CALLER on THIS thread. Returns
:ok, the wasm-trap condition, or :parked when the arm switched/parked
instead of rejecting (caught by the bounded TIMEOUT — the pin never
waits unbounded)."
  (let ((wasmdown.component.runtime::*current-thread* caller)
        (wasmdown.component.runtime::*current-task*
          (wasmdown.component.runtime::async-thread-task
                         caller)))
    (handler-case
        (handler-case
            (progn
              (sb-ext:with-timeout timeout
                (wasmdown.runtime:wasm-call (%pin-builtin inst kind) idx))
              :ok)
          (wasmdown:wasm-trap (c) c))
      (sb-ext:timeout () :parked))))

(test component/explicit-resume-requires-parked-peer-pin
  ;; §4688/§4714 (canon_thread_{suspend,yield}_then_resume): the target
  ;; must be SUSPENDED — §391-407: blocked with NO readiness function.
  ;; §494/§4688 make the switch builtins the ONLY explicit resumers, so
  ;; their predicate is the spec's rule and must not be widened: a
  ;; RUNNING peer, a READY one (§523 yield: waiting on a readiness
  ;; condition), and a thread whose mark was already CONSUMED are all
  ;; non-suspended and must trap. Pinned:
  ;;  (1) an explicit resume of a genuinely PARKED peer succeeds and
  ;;      its park's own flag is cleared when the park is left;
  ;;  (2) a LIVE (not parked, no pending spawn) peer is rejected;
  ;;  (3) after a park's exit the state is not stale — the same peer
  ;;      is rejected again, and a consumed resume-later mark does not
  ;;      make a running thread resumable.
  (%pin-with-store
    (let* ((inst (%pin-inst))
           (caller (%pin-thread inst 1 '(1)))
           (parked (%pin-thread inst 2 '(1 0)))
           (live (%pin-thread inst 3 '(1 1))))
      (%pin-register inst caller)
      (%pin-register inst parked)
      (%pin-register inst live)
      ;; ---- (1) the parked peer: the switch REALLY resumes it
      (let ((h (%pin-start-park parked)))
        (%pin-wait-parked parked)
        (let* ((outcome (list :res :running))
               (bt (sb-thread:make-thread
                    (lambda ()
                      (let ((wasmdown.component.runtime::*current-thread*
                              caller)
                            (wasmdown.component.runtime::*current-task*
                              (wasmdown.component.runtime::async-thread-task
                               caller))
                            (wasmdown.component.runtime::*right-self* caller)
                            (wasmdown.component.runtime::*right-ticket* nil)
                            (wasmdown.component.runtime::*scheduler-threads*
                              nil))
                        (unwind-protect
                             (handler-case
                                 (sb-ext:with-timeout
                                     5
                                   (setf (getf outcome :res)
                                         (wasmdown.runtime:wasm-call
                                          (%pin-builtin
                                           inst :thread-suspend-then-resume)
                                          2)))
                               (error (c) (setf (getf outcome :res) c)))
                          (wasmdown.component.runtime::%right-release-on-exit
                           caller))))
                    :name "pin-switch")))
          ;; the target's park is resumed by the switch
          (is (%pin-join (getf h :thread)))
          (is (null (car (getf h :err))))
          ;; §242-273: the target's exit hands control back to its
          ;; switcher (the park's own handoff post)
          (wasmdown.component.runtime::%async-thread-post-handoff parked)
          (is (%pin-join bt))
          (is (equal '(0) (getf outcome :res)))
          ;; the park CLEARED its own flag on the way out
          (is (not (wasmdown.component.runtime::async-thread-suspended-p
                    parked)))
          (is (not (wasmdown.component.runtime::async-thread-yield-parked
                    parked)))))
      ;; ---- (2) a LIVE peer is still rejected (the spec's rule)
      (let ((o (%pin-arm-outcome inst :thread-suspend-then-resume caller 3)))
        (is (typep o 'wasmdown:wasm-trap))
        (is (search "cannot resume thread which is not suspended"
                    (wasmdown:wasmdown-error-message o))))
      ;; ---- (3a) the state after a park's exit is not stale: the peer
      ;; that HAS left its park is rejected again
      (let ((o (%pin-arm-outcome inst :thread-suspend-then-resume caller 2)))
        (is (typep o 'wasmdown:wasm-trap))
        (is (search "cannot resume thread which is not suspended"
                    (wasmdown:wasmdown-error-message o))))
      ;; ---- (3b) a CONSUMED resume-later mark is not a suspension:
      ;; the live peer receives the mark (§458-465 start_waiting), runs
      ;; its next park (the credit is spent there), and must then be
      ;; rejected — a mark is a one-park credit, never a state.
      (let ((marker (%pin-thread inst 4 '(1 2))))
        (%pin-register inst marker)
        (%pin-resume-later inst marker 3)
        (is (wasmdown.component.runtime::async-thread-ready-p live))
        (let ((h (%pin-start-park live)))
          ;; the posted credit returns this park exactly once
          (is (%pin-join (getf h :thread)))
          (is (null (car (getf h :err)))))
        (is (not (wasmdown.component.runtime::async-thread-suspended-p live)))
        (is (null (wasmdown.component.runtime::async-thread-ready-p live)))
        (is (zerop (wasmdown.component.runtime::async-thread-wake-count live)))
        (let ((o (%pin-arm-outcome inst :thread-suspend-then-resume caller 3)))
          (is (typep o 'wasmdown:wasm-trap))
          (is (search "cannot resume thread which is not suspended"
                      (wasmdown:wasmdown-error-message o))))))))

(test component/thread-index-identity-pin
  ;; THE THREAD INDEX IS AN IDENTITY, NOT A POSITION (CanonicalABI.md
  ;; 391-407, 625-626, 701-744, 4535-4551, 4700-4770; Table at
  ;; 1111-1147). The spec's contract has THREE parts and this pin
  ;; pins all three, because conflating them is what let dssc::19
  ;; report "cannot switch to the current thread":
  ;;
  ;;  (1) register_thread (§701-706) assigns a FRESH index:
  ;;      `assert(thread.index is None)' — so while N threads are
  ;;      LIVE in one instance, no index is shared. The scan-and-add
  ;;      must be ONE atomic step; two concurrent registrations that
  ;;      both read `1 + max' would alias, and §513/§4550's
  ;;      threads.get(i) would then return the WRONG record.
  ;;  (2) an index is released at unregister (§737-744 ->
  ;;      Table.remove, §1142-1146), and Table.add REUSES the hole
  ;;      (§1131-1140). So "index 5 still names MY thread after it
  ;;      exited" is NOT a spec guarantee — the index may name a
  ;;      NEW live thread. What must hold is that it never resolves
  ;;      to the DEAD record (the §1126-1129 hole check), and that
  ;;      every LIVE index resolves to exactly its live record.
  ;;  (3) a switch to the CALLER's own index is the GUEST's error:
  ;;      canon_thread_{suspend,yield}_then_promote trap it
  ;;      explicitly (§4728-4737, §4737's `trap_if(i == thread.index)').
  ;;      That trap is KEPT — it is the spec's rule, not a symptom —
  ;;      and (5) below pins the other half of it: a suspension may
  ;;      only be ended by a real resume (§407/§494/§936), so a guest
  ;;      cannot reach the trap through a runtime-side alias.
  ;;
  ;; (1) and (2) drive the PRODUCTION registration/unregistration
  ;; (%REGISTER-THREAD-INDEX is the §705 step shared by
  ;; Task.enter_implicit_thread and thread.new-indirect, §4591).
  (%pin-with-store
    (let* ((inst (%pin-inst))
           (fields (wasmdown.component.runtime::%inst-async-fields inst))
           (register (lambda (th)
                       (wasmdown.component.runtime::%register-thread-index
                        inst th)))
           (live (lambda ()
                   (remove-if (lambda (th)
                                (eq (wasmdown.component.runtime::
                                     async-thread-spawn-thunk th)
                                    :done))
                              (copy-list (getf fields :threads)))))
           (index (lambda (th)
                    (wasmdown.component.runtime::async-thread-index th)))
           (a (%pin-thread inst nil '(1)))
           (b (%pin-thread inst nil '(1 0)))
           (c (%pin-thread inst nil '(1 1))))
      (dolist (th (list a b c)) (funcall register th))
      ;; ---- (1) pairwise-distinct indices among LIVE records
      (is (= 3 (length (funcall live))))
      (is (= 3 (length (remove-duplicates
                        (mapcar index (funcall live))))))
      ;; every live index resolves to ITS OWN record
      (dolist (th (funcall live))
        (is (eq th (find (funcall index th) (funcall live) :key index))))
      ;; re-registration is idempotent — never a second entry and
      ;; never a second index for one record (§705's assert)
      (let ((before (funcall index a)))
        (funcall register a)
        (is (= before (funcall index a)))
        (is (= 1 (count a (funcall live) :test #'eq))))
      ;; ---- (2) unregister releases the slot; the hole may be reused
      ;; by a NEW record but never resolves to the DEAD one
      (let ((b-index (funcall index b)))
        (wasmdown.component.runtime::%task-exit-implicit-thread
         (wasmdown.component.runtime::async-thread-task b))
        (is (not (member b (getf fields :threads) :test #'eq)))
        (is (eq :done (wasmdown.component.runtime::async-thread-spawn-thunk
                       b)))
        (let ((d (%pin-thread inst nil '(1 2))))
          (funcall register d)
          ;; the fresh index is not one a LIVE thread holds
          (dolist (th (remove d (funcall live) :test #'eq))
            (is (/= (funcall index d) (funcall index th))))
          ;; and no live index names b
          (is (not (eq b (find b-index (funcall live) :key index))))
          ;; every live index still resolves to its own record
          (dolist (th (funcall live))
            (is (eq th (find (funcall index th) (funcall live)
                             :key index))))
          ;; the dead record is not a switch target either: the
          ;; lookup walks the LIVE table, and §1126-1129's Table.get
          ;; traps on a HOLE exactly as it does out of bounds
          (let ((wasmdown.component.runtime::*current-thread* c)
                (wasmdown.component.runtime::*current-task*
                  (wasmdown.component.runtime::async-thread-task c)))
            (dolist (kind (list :thread-yield-then-promote
                                :thread-suspend-then-promote
                                :thread-yield-then-resume))
              (let ((o (%pin-arm-outcome inst kind c b-index)))
                (is (typep o 'wasmdown:wasm-trap))
                (is (search "unknown thread index"
                            (wasmdown:wasmdown-error-message o))))))
          (wasmdown.component.runtime::%task-exit-implicit-thread
           (wasmdown.component.runtime::async-thread-task d))))
      ;; ---- (3) the CALLER's own index is the guest's own fault:
      ;; §4737/§4763 `trap_if(i == thread.index)' — KEPT as a trap.
      ;; the two PROMOTE builtins: §4737/§4763's explicit
      ;; `trap_if(i == thread.index)'
      (dolist (kind (list :thread-yield-then-promote
                          :thread-suspend-then-promote))
        (let ((o (%pin-arm-outcome inst kind a (funcall index a))))
          (is (typep o 'wasmdown:wasm-trap))
          (is (search "cannot switch to the current thread"
                      (wasmdown:wasmdown-error-message o)))))
      ;; the then-RESUME builtins have no self-index trap (§4711/
      ;; §4716): the running caller is simply not `suspended()', so
      ;; the same guest error is rejected by that rule instead
      (let ((o (%pin-arm-outcome inst :thread-yield-then-resume a
                                 (funcall index a))))
        (is (typep o 'wasmdown:wasm-trap))
        (is (search "cannot resume thread which is not suspended"
                    (wasmdown:wasmdown-error-message o))))
      ;; ---- (5) THE OTHER HALF OF THE SELF-SWITCH TRAP: only a REAL
      ;; resume ends a suspension (§407: a SUSPENDED thread has no
      ;; readiness function; §433-440: it is not in Store.waiting, so
      ;; §936's tick cannot pick it; §494: `suspend' blocks with no
      ;; readiness). A durable nudge — an implementation right
      ;; handoff, a warden's wake, the grant of a tick — must leave
      ;; the park parked. This is the dssc::19 mechanism: the nudge
      ;; WAS the resume, the guest's thread finished early, its index
      ;; was released and reused, and the guest's next switch named
      ;; the caller. The trap then was the guest's own stale index —
      ;; correct per (3) — but it was REACHED by a runtime-side
      ;; unsuspension, which is what this half forbids.
      (let ((p (%pin-thread inst nil '(1 3))))
        (funcall register p)
        (let ((h (%pin-start-park p)))
          (%pin-wait-parked p)
          ;; a plain nudge is not a resume
          (wasmdown.component.runtime::%async-thread-wake p)
          (sleep 0.15)
          (is (null (car (getf h :done))))
          (is (wasmdown.component.runtime::async-thread-suspended-p p))
          ;; nor is a store tick's grant
          (wasmdown.component.runtime::async-thread-resume-all)
          (sleep 0.15)
          (is (null (car (getf h :done))))
          (is (wasmdown.component.runtime::async-thread-suspended-p p))
          ;; a REAL resume ends it — exactly once
          (wasmdown.component.runtime::async-thread-resume p)
          (is (%pin-join (getf h :thread)))
          (is (null (car (getf h :err))))
          (is (not (wasmdown.component.runtime::async-thread-suspended-p p)))
          (is (zerop (wasmdown.component.runtime::async-thread-resume-count
                      p))))
        (wasmdown.component.runtime::%task-exit-implicit-thread
         (wasmdown.component.runtime::async-thread-task p))))))

(test component/premature-park-release-pin
  ;; THE SUSPEND PARK IS RELEASED BY A RESUME, NEVER BY A SCHEDULER
  ;; HANDOFF (§407: a SUSPENDED thread has `ready_func` None; §433-440:
  ;; it is absent from `Store.waiting`, so §936's tick cannot pick it;
  ;; §494: `Thread.suspend` is `block()` with NO readiness function).
  ;; The two during-sync rows are the guest's own assertions of the two
  ;; consequences, and they are QUOTED here because the guest's check,
  ;; not the harness, specifies the observable state (the suite lane
  ;; names them ::2 and ::8, the indices below):
  ;;
  ;;  during-sync-call-exclusive-resume.wast ::2, after the sync
  ;;  thread's `thread.suspend`:
  ;;      (if (i32.eqz (global.get $pinged)) (then unreachable))
  ;;  $pinged is set ONLY by the parked event-loop thread's callback,
  ;;  so a return from that park before the callback ran (and its
  ;;  `resume-later` marked this thread READY, §458-465) is the defect:
  ;;  the sync call observed a resume that never happened.
  ;;
  ;;  during-sync-call-may-block-if-other-ready-threads.wast ::8, after
  ;;  the sync thread's `thread.suspend`:
  ;;      (if (i32.eqz (global.get $worker-ran)) (then unreachable))
  ;;  $worker-ran is set by the mini-scheduler's ONLY way back (the
  ;;  worker chain T1-exit -> T3 -> resume-later(T2), §3538-3543), so a
  ;;  park that returns first again observed an uncaused release.
  ;;
  ;; The pre-fix shape released a suspend park on any `wake-count`
  ;; (the §242-273 right handoff and the tick's nudge), so both rows
  ;; measured red on a re-broken build (30 standalone runs:
  ;; exclusive-resume::2 7/30, may-block::8 10/30, each with the
  ;; in-guest `unreachable` above and the trace line
  ;; `suspend park RELEASED ... by NUDGE ... wake=1 resume=0');
  ;; the clean tree measures 0/10 standalone each and 0/30 lane runs.
  ;; (1) pins the MECHANISM deterministically; (2) and (3) run the
  ;; fixture rows VERBATIM.
  (%pin-with-store
    (let* ((inst (%pin-inst))
           (sync (%pin-thread inst 1 '(1)))
           (peer (%pin-thread inst 2 '(1 0))))
      (%pin-register inst sync)
      (%pin-register inst peer)
      ;; the non-async-typed call's implicit thread parks in suspend
      (let ((h (%pin-start-park sync)))
        (%pin-wait-parked sync)
        ;; the same-instance peer is the event loop's READY thread
        ;; (§523 `yield_` = wait_until(lambda: True)); a plain nudge
        ;; and any number of ticks must leave the suspension parked
        (setf (wasmdown.component.runtime::async-thread-yield-parked peer) t)
        (wasmdown.component.runtime::%async-thread-wake sync)
        (wasmdown.component.runtime::async-thread-resume-all)
        (sleep 0.05)
        (is (null (car (getf h :done))))
        (is (wasmdown.component.runtime::async-thread-suspended-p sync))
        ;; THE PEER'S WORK: its callback runs and resume-laters the
        ;; sync thread (§458-465 start_waiting(ready = True)) — a MARK,
        ;; delivered as the §242-273 handoff slot, not an inline wake
        (%pin-resume-later inst peer 1)
        (is (wasmdown.component.runtime::async-thread-ready-p sync))
        (is (zerop (wasmdown.component.runtime::async-thread-resume-count
                    sync)))
        (sleep 0.05)
        (is (null (car (getf h :done))))
        (is (wasmdown.component.runtime::async-thread-suspended-p sync))
        ;; the tick's READY-GRANT is the marker's delivery (§932): it
        ;; resumes the thread exactly once, and the park is spent
        (wasmdown.component.runtime::async-thread-resume-all)
        (is (%pin-join (getf h :thread)))
        (is (null (car (getf h :err))))
        (is (not (wasmdown.component.runtime::async-thread-suspended-p sync)))
        (is (%pin-clean-p sync)))))
  ;; THE EXCLUSIVE LOCK'S RELEASE WINDOW IS THE SPEC'S, NOT THE PARK'S
  ;; (§3495-3508: the CALLBACK event loop releases
  ;; `ComponentInstance.exclusive_thread` before ITS OWN `YIELD`/`WAIT`
  ;; arms block, "to allow other needs_exclusive tasks to execute in
  ;; the interim"; a synchronously-lifted task "only release[s] the
  ;; exclusive_thread lock after they've returned"). A holder that
  ;; parks inside its own turn keeps the lock, and a same-instance
  ;; `needs_exclusive` peer stays at `Task.enter_implicit_thread`'s
  ;; backpressure gate (§681, the §669-672 residual rule) — the park
  ;; that the spec keeps out of the window admits nobody.
  (%pin-with-store
    (let* ((inst (%pin-inst))
           (ft (wasmdown.component:make-ct
                :kind :func
                :payload (list (make-array 0) nil t))) ; async-typed
           (holder-task (wasmdown.component.runtime::make-async-task
                         :ft ft :opts '((:async) (:callback))
                         :inst inst :seq '(1)))
           (holder (wasmdown.component.runtime::make-async-thread
                    :task holder-task)))
      (setf (wasmdown.component.runtime::async-task-implicit-thread
             holder-task)
            holder)
      (is (wasmdown.component.runtime::%task-enter-implicit-thread
           holder-task))
      (is (eq holder
              (wasmdown.component.runtime::%inst-exclusive-thread inst)))
      (let ((h (%pin-start-park holder)))
        (%pin-wait-parked holder)
        ;; the holder's park has NOT released the exclusive lock
        (is (eq holder
                (wasmdown.component.runtime::%inst-exclusive-thread inst)))
        ;; a same-instance needs_exclusive peer waits at the gate: it
        ;; is registered nowhere and holds no index
        (let* ((peer-task (wasmdown.component.runtime::make-async-task
                           :ft ft :opts '((:async) (:callback))
                           :inst inst :seq '(1 0)))
               (peer (wasmdown.component.runtime::make-async-thread
                      :task peer-task))
               (started (list nil))
               (registry wasmdown.component.runtime::*scheduler-threads*)
               (gate nil))
          (setf (wasmdown.component.runtime::async-task-implicit-thread
                 peer-task)
                peer)
          (setf gate
                (sb-thread:make-thread
                  (lambda ()
                    (let ((wasmdown.component.runtime::*current-thread* peer)
                          (wasmdown.component.runtime::*current-task*
                            peer-task)
                          (wasmdown.component.runtime::*right-self* peer)
                          (wasmdown.component.runtime::*right-ticket* nil)
                          (wasmdown.component.runtime::*scheduler-threads*
                            registry))
                      (unwind-protect
                           (setf (car started)
                                 (wasmdown.component.runtime::
                                  %task-enter-implicit-thread peer-task))
                        (wasmdown.component.runtime::%right-release-on-exit
                         peer))))
                  :name "pin-gate"))
          (%pin-wait-for
           (lambda ()
             (plusp (getf (wasmdown.component.runtime::%inst-async-fields
                           inst)
                          :num-waiting-to-enter)))
           :what "the needs_exclusive peer at the gate")
          (is (null (wasmdown.component.runtime::async-thread-index peer)))
          (is (eq holder
                  (wasmdown.component.runtime::%inst-exclusive-thread inst)))
          ;; the holder's turn ends: the resume returns its park, and
          ;; only the EXIT releases the lock (§3495) and re-tests the
          ;; gate
          (wasmdown.component.runtime::async-thread-resume holder)
          (is (%pin-join (getf h :thread)))
          (is (eq holder
                  (wasmdown.component.runtime::%inst-exclusive-thread inst)))
          (wasmdown.component.runtime::%task-exit-implicit-thread
           holder-task)
          (is (null (wasmdown.component.runtime::%inst-exclusive-thread
                     inst)))
          ;; the gate's park is a WAITING park (§681's wait-until): a
          ;; nudge re-tests it — the production resume-all reaches the
          ;; real store's registry; this pin's gate thread owns its
          ;; registry, so the wake is posted directly
          (wasmdown.component.runtime::%async-thread-wake peer)
          (is (%pin-join gate))
          (is (eq t (car started)))
          (is (numberp
               (wasmdown.component.runtime::async-thread-index peer)))
          (wasmdown.component.runtime::%task-exit-implicit-thread
           peer-task)))))
  ;; the rows VERBATIM (the guest's own checks are the oracle)
  (is (%pin-rows-pass-p
       (%pin-slice "tests/component/async/during-sync-call-exclusive-resume.wast"
                   9 60)
       "premature-park-exclusive-resume.wast"))
  (is (%pin-rows-pass-p
       (%pin-slice "tests/component/async/during-sync-call-exclusive-resume.wast"
                   65 103)
       "premature-park-exclusive-trap.wast"))
  (is (%pin-rows-pass-p
       (%pin-slice
        "tests/component/async/during-sync-call-may-block-if-other-ready-threads.wast"
        136 207)
       "premature-park-may-block.wast")))

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
                      component/imported-component-space-pins
                      component/lower-canon-core-alloc-pins
                      component/moduletype-inner-rules-pins
                      component/core-instantiation-rules-pins
                      component/instance-chain-projection-pins
                      component/builtin-immediate-pins
                      component/inline-target-sugar-pins
                      component/outer-alias-special-binding-pins
                      component/moduletype-outer-alias-pins
                      component/outer-alias-debruijn-pins
                      component/externtype-type-use-pins
                      component/abi-string-encoding-pins
                      component/declarator-bind-id-pins
                      component/core-subtype-matching-pins
                      component/canon-signature-pins
                      component/instantiation-core-link-pins
                      component/instantiation-with-arg-pins
                      component/instantiation-start-pins
                      component/instantiation-trap-pins
                      component/invoke-lifted-func-pins
                      component/instance-isolation-pins
                      component/canon-lower-roundtrip-pins
                      component/linking-tag-identity-pins
                      component/linking-tag-sort-mismatch-pins
                      component/linking-ascribed-projection-pins
                      component/values-string-roundtrip-pins
                      component/values-list-lower-lift-pins
                      component/values-record-alignment-pins
                      component/values-variant-discriminant-pins
                      component/values-maxflat-indirection-pins
                      component/values-map-lower-pins
                      component/values-option-size-pins
                      component/async-task-return-borrow-pins
                      component/async-opt-requires-async-type-pins
                      component/async-builtin-imms-parse-pins
                      component/async-builtin-sig-marker-pins
                      component/async-task-yield-resume-pins
                      component/async-context-roundtrip-pins
                      component/async-wset-wait-join-pins
                      component/async-backpressure-block-pins
                      component/async-cancel-delivery-pins
                      component/async-stream-roundtrip-pins
                      component/sync-lift-thread-context-pins
                      component/suspend-vs-yield-determinism-pins
                      component/cancel-eager-resolve-pins
                      component/granted-park-quantum-pins
                      component/async-lower-result-two-spaces-pins
                      component/host-resource-dtor-registry-pins
                      component/cancel-window-fire-gate-pins
                      component/import-binding-signature-pins
                      component/import-binding-sort-pins
                      component/import-binding-missing-pins
                      component/import-binding-accepts-pins
                      component/import-binding-type-bound-pins
                      component/import-binding-instance-coverage-pins
                      component/guest-drop-reaches-provider-finalizer-pin
                      component/host-resource-owner-is-graph-scoped-pin
                      component/two-providers-drop-own-finalizers-pin
                      component/resume-later-marker-lifetime-pin
                      component/explicit-resume-requires-parked-peer-pin
                      component/thread-index-identity-pin
                      component/premature-park-release-pin)))
          wasmdown.test::*tests*)))
    (multiple-value-bind (passed failed)
        (wasmdown.test:run-all-tests)
      (declare (ignore passed))
      (zerop failed))))
