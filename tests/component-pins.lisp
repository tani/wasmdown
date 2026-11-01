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


;;; Pins that manipulate scheduler state run inside a FRESH STORE (the
;;; step-2 ownership model): the registry and the execution right are
;;; the store's own slots, so two pins can never nudge each other's
;;; threads the way process globals did.
(defmacro %pin-in-store (() &body body)
  "Run BODY on a RAW (non-runtime) thread with the CURRENT store bound.
Store ownership is per instantiation graph, and the dynamic binding
does not cross SB-THREAD:MAKE-THREAD, so a pin's bare thread must
carry the store explicitly."
  `(let ((store (wasmdown.component.runtime::%cs)))
     (lambda () (let ((wasmdown.component.runtime::*current-component-store* store))
                  ,@body))))

(defmacro pin-test (name &body body)
  `(test ,name
     (let ((wasmdown.component.runtime::*current-component-store*
             (wasmdown.component.runtime:make-component-store)))
       (unwind-protect (progn ,@body)
         (wasmdown.component.runtime::%store-teardown)))))


;;; step 2: the execution tables live on the STORE. Tests that drive
;;; the runtime directly run on the thread's bound store (or the
;;; process fallback, exactly where a bare host caller is).
(defun %current-store ()
  (or wasmdown.component.runtime::*current-component-store*
      wasmdown.component.runtime::*global-host-store*))

(defun right-handoffs ()
  (wasmdown.component.runtime::component-store-right-handoffs (%current-store)))

(defun (setf right-handoffs) (value)
  (setf (wasmdown.component.runtime::component-store-right-handoffs
         (%current-store))
        value))

(defun ct-defining ()
  (wasmdown.component.runtime::component-store-ct-defining-inst (%current-store)))


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

(test component/binary-declarator-numbering-pin
  ;; A BINARY instance type numbers only its TYPE-SORT declarators into
  ;; its own type space (Binary.md line 274), so a func-sort export
  ;; between two type declarations must not shift the later reference.
  ;; Witness bytes: wasm-tools 1.262 `wasm-tools parse` of
  ;;   (component (type $T (instance
  ;;     (type (func)) (export "f" (func (type 0)))
  ;;     (type (func (param "x" u32))) (export "g" (func (type 1))))))
  ;; minus the custom name section. Reference validators accept it; the
  ;; pre-fix numbering pushed a phantom slot for the func export and
  ;; rejected it with 'type index out of bounds'.
  (let* ((bin (%hexb
                "\\00\\61\\73\\6d\\0d\\00\\01\\00\\07\\1c\\01\\42"
                "\\04\\01\\40\\00\\01\\00\\04\\00\\01\\66\\01\\00"
                "\\01\\40\\01\\01\\78\\79\\01\\00\\04\\00\\01\\67"
                "\\01\\01"))
         (comp (wasmdown.component:decode-component bin)))
    (wasmdown.component:validate-component comp)
    ;; negative control: the same shape with "g" shifted onto the phantom
    ;; slot index (type 2) must be rejected
    (let* ((shifted (copy-seq bin))
           (pos (loop for i below (- (length shifted) 5)
                      when (and (= (aref shifted i) #x04)
                                (= (aref shifted (1+ i)) 0)
                                (= (aref shifted (+ i 2)) 1)
                                (= (aref shifted (+ i 3)) (char-code #\g)))
                        return (+ i 5))))
      (is (integerp pos))
      (setf (aref shifted pos) 2)
      (let ((bad (wasmdown.component:decode-component shifted)))
        (is (handler-case
                (progn (wasmdown.component:validate-component bad) nil)
              (wasmdown:validation-error () t)))))))

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

(test component/remap-outer-compound-functype-owner
  ;; §9.3 validator row: an imported-instance functype whose parameter
  ;; is a COMPOUND type brought in by an outer alias must materialize
  ;; the compound's nested refs in the OWNER's space, not the
  ;; declarator space (the old single-value entry-at deindexed a
  ;; tuple's (:idx . 0) against the instance declarator, resolving an
  ;; unrelated same-numbered type — a spurious memory requirement or a
  ;; missed one).
  (labels ((mk-ct (kind payload)
             (wasmdown.component::make-ct :kind kind :payload payload
                                          :offset 0))
           (mk-decl (kind payload)
             (wasmdown.component::make-typedecl :kind kind :payload payload))
           (mk-alias (count target)
             (wasmdown.component::make-aliasrec
              :kind :outer :sort :type :count count :target target
              :name "" :offset 0))
           (mk-view (ct types &optional (outers nil))
             (wasmdown.component::make-type-view
              :ct ct :types types :identities #() :outers outers))
           (mk-state (types)
             (wasmdown.component::make-cval :types types))
           (check (leaf)
             ;; owner component space: 0 = LEAF, 1 = tuple<LEAF>
             (let* ((owner (mk-state (vector leaf
                                             (mk-ct :tuple
                                                    (vector '(:idx . 0))))))
                    (owner-view (mk-view owner
                                         (wasmdown.component::cval-types owner)))
                    ;; declarator space: 0 = a DIFFERENT decoy type,
                    ;; 1 = outer alias to owner index 1
                    (ict (mk-ct :instance
                                (vector (mk-decl :type
                                                 (mk-ct :prim :string))
                                        (mk-decl :alias (mk-alias 1 1)))))
                    (view (wasmdown.component::%declarator-view
                           ict owner-view))
                    (ft (mk-ct :func (list (vector (cons "x" '(:idx . 1)))
                                           nil nil)))
                    (out (wasmdown.component::%remap-decl-functype
                          ft 0 ict view))
                    (param (cdr (aref (first
                                       (wasmdown.component::ct-payload out))
                                      0))))
               param)))
    ;; the positive: the owner's tuple<u32>, NOT the declarator's decoy
    (let ((param (check (mk-ct :prim :u32))))
      (is (eq :tuple (wasmdown.component::ct-kind param)))
      (is (eq :u32 (wasmdown.component::ct-payload
                    (aref (wasmdown.component::ct-payload param) 0)))))
    ;; the negative direction: a string leaf stays a string (memory is
    ;; then genuinely required downstream)
    (let ((param (check (mk-ct :prim :string))))
      (is (eq :string (wasmdown.component::ct-payload
                       (aref (wasmdown.component::ct-payload param) 0)))))
    (flet ((validate-text (text)
             (wasmdown.component:validate-component
              (wasmdown.component.text::parse-component-text text))))
      ;; end-to-end: the decoy-first text must now validate (no memory
      ;; needed for tuple<u32>), and the real-string text must still
      ;; reject with the memory requirement.
      (is (progn
            (validate-text
             "(component
                (type $x u32)
                (type $t (tuple $x))
                (import \"i\" (instance $i
                  (type $shadow string)
                  (export \"f\" (func (param \"x\" $t)))))
                (alias export $i \"f\" (func $f))
                (core func $lower (canon lower (func $f))))")
            t))
      (is (handler-case
              (progn
                (validate-text
                 "(component
                    (type $x string)
                    (type $t (tuple $x))
                    (import \"i\" (instance $i
                      (type $shadow u32)
                      (export \"f\" (func (param \"x\" $t)))))
                    (alias export $i \"f\" (func $f))
                    (core func $lower (canon lower (func $f))))")
                nil)
            (wasmdown:validation-error (e)
              (search "memory" (wasmdown:wasmdown-error-message e)))))
      ;; the result-position companion: the same outer compound in a
      ;; RESULT slot resolves in the owner's space (positive), and a
      ;; string leaf still forces memory (negative)
      (is (progn
            (validate-text
             "(component
                (type $x u32)
                (type $t (tuple $x))
                (import \"i\" (instance $i
                  (type $shadow string)
                  (export \"f\" (func (result $t)))))
                (alias export $i \"f\" (func $f))
                (core func $lower (canon lower (func $f))))")
            t))
      (is (handler-case
              (progn
                (validate-text
                 "(component
                    (type $x string)
                    (type $t (tuple $x))
                    (import \"i\" (instance $i
                      (type $shadow u32)
                      (export \"f\" (func (result $t)))))
                    (alias export $i \"f\" (func $f))
                    (core func $lower (canon lower (func $f))))")
                nil)
            (wasmdown:validation-error (e)
              (search "memory" (wasmdown:wasmdown-error-message e))))))))

(test component/type-view-owner-pins
  ;; WASI 0.3.2 residuals, plan step 1: a type space is resolved through
  ;; an OWNER-QUALIFIED VIEW (ct, types, identities, outers). A
  ;; reference SWITCHES OWNER as it crosses an instance/component
  ;; boundary and NEVER resolves another scope's type at the same
  ;; numeric index; an unresolvable reference is a validation error.
  (labels ((mk-ct (kind payload)
           (wasmdown.component::make-ct :kind kind :payload payload
                                        :offset 0))
         (mk-decl (kind payload)
           (wasmdown.component::make-typedecl :kind kind :payload payload))
         (mk-alias (count target)
           (wasmdown.component::make-aliasrec
            :kind :outer :sort :type :count count :target target
            :name "" :offset 0))
         (mk-view (ct types &optional (outers nil))
           (wasmdown.component::make-type-view
            :ct ct :types types :identities #() :outers outers))
         (mk-state (types)
           (wasmdown.component::make-cval :types types))
         (rec (label vt)
           (mk-ct :record (vector (cons label vt))))
         (ct-payload (c)
           (wasmdown.component::ct-payload c))
         (decl-payload (c i)
           (wasmdown.component::typedecl-payload
            (aref (wasmdown.component::ct-payload c) i)))
         (expect-invalid (src)
           (block nil
             (handler-case
                 (wasmdown.component:validate-component
                  (wasmdown.component.text::parse-component-text src))
               (wasmdown:validation-error (c) (return c)))
             (error "expected wasmdown:validation-error"))))
    (let* ((u32 (mk-ct :prim :u32))
           ;; two UNRELATED spaces, each with a COMPOUND type at index 0
           (near-type (rec "a" :u32))
           (deep-type (rec "b" :u64))
           (near (mk-state (vector near-type)))
           (deep (mk-state (vector deep-type)))
           (near-view (mk-view near (wasmdown.component::cval-types near)))
           ;; the ring: count 1 = NEAR (the declaring space), count 2 =
           ;; DEEP — two boundaries out, so the SAME numeric index 0
           ;; must resolve in a DIFFERENT space
           (owner (mk-view near (wasmdown.component::cval-types near)
                           (list (mk-view
                                  deep
                                  (wasmdown.component::cval-types deep)))))
           ;; the DECLARATOR space: 0 a local type, 1 an outer alias at
           ;; count 1, 2 the same target index at count 2
           (ict (mk-ct :instance
                       (vector
                        (mk-decl :type u32)
                        (mk-decl :alias (mk-alias 1 0))
                        (mk-decl :alias (mk-alias 2 0))))))
      ;; a local reference resolves in the instance's OWN space ...
      (is (eq (wasmdown.component::resolve-declarator-type ict 0 owner) u32))
      ;; ... and the outer alias's reference resolves in the space it
      ;; CROSSED INTO (count 1 = the declaring component, count 2 = its
      ;; outer) — never index 0 of whatever space is current
      (is (eq (wasmdown.component::resolve-declarator-type ict 1 owner)
              near-type))
      (is (eq (wasmdown.component::resolve-declarator-type ict 2 owner)
              deep-type))
      ;; resolve-valtype resolves in-space refs and rejects an
      ;; out-of-range index (no substitution from the ring)
      (is (eq (wasmdown.component::resolve-valtype '(:idx . 0) near-view)
              near-type))
      (is (handler-case
              (progn (wasmdown.component::resolve-valtype '(:idx . 1)
                                                          near-view)
                     nil)
            (wasmdown:validation-error () t)))
      ;; the REMAP used by the canon/link checks reads the recorded
      ;; provenance (*CT-OWNER-VIEW*) instead of the ambient
      ;; *OUTER-STATES*: the same declarator ref resolves in the space
      ;; that DECLARED the ct
      (let ((remap-ict
              (mk-ct :instance
                     (vector
                      (mk-decl :alias (mk-alias 1 0))
                      (mk-decl :type
                               (mk-ct :func (list #() (cons :idx 0) nil)))))))
        (setf (gethash remap-ict wasmdown.component::*ct-owner-view*)
              (mk-view near (wasmdown.component::cval-types near)))
        (is (eq (wasmdown.component::resolve-declarator-type remap-ict 0
                                                            (mk-view near (wasmdown.component::cval-types near)))
                near-type))
        (remhash remap-ict wasmdown.component::*ct-owner-view*))
      ;; a MISSING alias target (a count past the recorded ring) is a
      ;; validation error, never a raw index left for a later reader
      (is (handler-case
              (progn
                (wasmdown.component::resolve-declarator-type
                 (mk-ct :instance (vector (mk-decl :alias (mk-alias 3 0))))
                 0 owner)
                nil)
            (wasmdown:validation-error () t))))
    ;; resource own/borrow disagreement is rejected (the instantiation
    ;; matcher must not accept a borrow where the import demands own)
    (let ((c (expect-invalid
              "(component
  (import \"T\" (type $T (sub resource)))
  (import \"f\" (func $f (param \"x\" (borrow $T))))
  (component $c
    (import \"T\" (type $T (sub resource)))
    (import \"g\" (func (param \"x\" (own $T)))))
  (instance (instantiate $c (with \"T\" (type $T)) (with \"g\" (func $f)))))")))
      (is (search "type mismatch in function parameter"
                  (wasmdown:wasmdown-error-message c))))))

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

(pin-test component/cancel-window-fire-gate-pins
  ;; M15 MECHANISM 4 + M17's SYNC-CALL FIRE WINDOW.
  ;;
  ;; GUARANTEE (what this pin carries): a thread that a cancellation
  ;; callback makes ready with thread.resume-later takes its FIRST
  ;; TURN only where the reference's host could schedule it — not
  ;; inside the caller's observation. Two windows implement it, and
  ;; the pin asserts BOTH, one deterministically and one end-to-end:
  ;;
  ;;  (1) THE CANCEL WINDOW: while the synchronous-resume wait is
  ;;      open, the drain's ticks (priority pass AND no-quantum
  ;;      fallback) must not FIRE the cancelled instance's
  ;;      ready-marked pending spawns; the async lower's on-resolve
  ;;      is the fifth M11a deferral site.
  ;;
  ;;  (2) THE SYNC-CALL FIRE WINDOW (comp-instance-sync-window-depth,
  ;;      this pin drove): after subtask.cancel returns, the caller's
  ;;      `$ran' probe is a NESTED SYNCHRONOUS component call. §3524
  ;;      resumes that callee synchronously and runs it until it
  ;;      returns or blocks; no other thread of the callee's instance
  ;;      may take its first turn inside the wait. Before the fix the
  ;;      caller released the store right for the callee's executor
  ;;      and the outer invoke drain used that window to FIRE the
  ;;      ready spawn (measured on master: the slice run alone failed
  ;;      10/30 and each failure read $ran = 1 at this probe and
  ;;      trapped `unreachable'; a 30-run pin-alone of the same tree
  ;;      read 0/30, because the pin's other work warms the OS thread
  ;;      pool and the race is a scheduling lottery, not a fixed
  ;;      interleave — so the deterministic boundary below, not the
  ;;      row's rate, is what carries this half).
  ;;
  ;; (c) below drives the tick itself at the deterministic boundary:
  ;; a ready-marked pending spawn, one real resume-all with the
  ;; window OPEN (never fired) and one with it CLOSED (fired exactly
  ;; once), plus the per-instance scoping. No sleep, no retry budget:
  ;; a tick inside the window must leave the spawn UNFIRED (the
  ;; reference cannot switch to another thread of the callee's
  ;; instance while the sync callee runs), and (b) then runs the
  ;; fixture row VERBATIM, end to end.
  (let* ((inst (wasmdown.component.runtime:make-comp-instance))
         (other-inst (wasmdown.component.runtime:make-comp-instance))
         (task (wasmdown.component.runtime::make-async-task
                :inst inst :seq '(1)))
         (th (wasmdown.component.runtime::make-async-thread :task task))
         (other-th
           (wasmdown.component.runtime::make-async-thread
            :task (wasmdown.component.runtime::make-async-task
                   :inst other-inst :seq '(2))))
         (old-threads (wasmdown.component.runtime::%store-threads))
         (old-depth (wasmdown.component.runtime::comp-instance-sync-window-depth
                     inst))
         (fired 0))
    (unwind-protect
         (progn
           (setf (wasmdown.component.runtime::%store-threads)
                 (list th))
           (setf (wasmdown.component.runtime::async-thread-spawn-thunk th)
                 (lambda () (incf fired))
                 (wasmdown.component.runtime::async-thread-ready-p th) t)
           ;; THE WINDOW OPEN: the tick must not fire the spawn
           (wasmdown.component.runtime::%sync-call-window-enter inst)
           (wasmdown.component.runtime::async-thread-resume-all)
           (is (zerop fired))
           (is (functionp (wasmdown.component.runtime::async-thread-spawn-thunk
                           th)))
           ;; THE WINDOW IS PER-INSTANCE: another instance's thread is
           ;; not held by it
           (is (wasmdown.component.runtime::%sync-call-fire-blocked-p th))
           (is (not (wasmdown.component.runtime::%sync-call-fire-blocked-p
                     other-th)))
           ;; THE WINDOW CLOSED: the same tick fires it, once
           (wasmdown.component.runtime::%sync-call-window-exit inst)
           (is (zerop (wasmdown.component.runtime::%sync-call-window-depth-of
                       inst)))
           (wasmdown.component.runtime::async-thread-resume-all)
           (is (= 1 fired))
           (is (not (functionp
                     (wasmdown.component.runtime::async-thread-spawn-thunk th))))
           (wasmdown.component.runtime::async-thread-resume-all)
           (is (= 1 fired)))
      (setf (wasmdown.component.runtime::%store-threads) old-threads
            (wasmdown.component.runtime::comp-instance-sync-window-depth inst)
            old-depth)
      (when (sb-thread:holding-mutex-p
             wasmdown.component.runtime::*store-right*)
        (sb-thread:release-mutex
         wasmdown.component.runtime::*store-right*))))
  ;; (b) THE ROW, VERBATIM (component + assert_return): the callback
  ;; resume-laters a spawn inside the cancellation, subtask.cancel
  ;; returns as soon as the callback is done, and $ran must stay 0
  ;; until the caller yields.
  (is (%pin-rows-pass-p
       (%pin-slice "tests/component/async/cancel-targeted-resume.wast"
                   613 699)
       "cancel-fire-gate-pin.wast")))

(pin-test component/reentrance-14-pin
  ;; THE TARGETED HANDOFF IS A RESERVED WINDOW (reentrance ::14, the
  ;; drift row). The async lower's STARTED-wait releases the store
  ;; right to ONE callee thread for its first turn, then observes the
  ;; packed state. Before this pin, the window was not reserved: the
  ;; release named the callee as the ticket, but when the callee's OS
  ;; thread lost the schedule, a THIRD party (the outer caller's next
  ;; turn) took the right under the `null successor' fallback, ran the
  ;; outer task's callback WAIT arm, released the instance's exclusive
  ;; lock, and the callee — still un-started at the lower — then
  ;; STARTED and even RESOLVED inside the pack. The lower packed an
  ;; eager RETURNED (2) where the fixture pins STARTING (0), and the
  ;; guest's `(i32.ne (i32.and $packed 0xf) STARTING)' check trapped
  ;; `unreachable' (5/20 standalone runs; 14/30 in the clean window).
  ;;
  ;; (a) drives the REAL right builtins: while a handoff frame is on
  ;; top, a peer's %RIGHT-ACQUIRE must NOT take the right (it defers
  ;; without renaming the ticket), and the caller takes it back; the
  ;; target's park/exit names the caller as the next ticket.
  ;; (b) runs the fixture row VERBATIM, repeatedly: the drift window
  ;; is an OS scheduling lottery, so one run would not pin it.
  (let* ((c (wasmdown.component.runtime::make-async-thread
             :task (wasmdown.component.runtime::make-async-task
                    :seq '(1))))
         (tg (wasmdown.component.runtime::make-async-thread
              :task (wasmdown.component.runtime::make-async-task
                     :seq '(1 1))))
         (p (wasmdown.component.runtime::make-async-thread
             :task (wasmdown.component.runtime::make-async-task
                    :seq '(2))))
         (old-threads (wasmdown.component.runtime::%store-threads))
         (old-ticket (wasmdown.component.runtime::%store-right-ticket))
         (old-frames (right-handoffs))
         (old-owner (wasmdown.component.runtime::%store-right-owner))
         (violation nil)
         (peer-took nil))
    (unwind-protect
         (progn
           ;; ---- (a1) a third party may not take the handoff window
           (setf (wasmdown.component.runtime::%store-threads)
                 (list c tg p)
                 (wasmdown.component.runtime::%store-right-ticket) nil
                 (right-handoffs) nil
                 (wasmdown.component.runtime::%store-right-owner) nil)
           (let ((peer
                   (sb-thread:make-thread
                    (%pin-in-store ()
                      (let ((wasmdown.component.runtime::*right-self* p))
                        (unwind-protect
                             (progn
                               (wasmdown.component.runtime::%right-acquire p)
                               ;; acquiring while a frame is on top is
                               ;; the defect this pin forbids
                               (when (right-handoffs)
                                 (setf violation t))
                               (setf peer-took t))
                          (wasmdown.component.runtime::%right-drop p))))
                    :name "pin-handoff-peer")))
             ;; the caller opens the window exactly as
             ;; %RIGHT-HANDOFF-TO does: ticket=target, frame=(caller
             ;; . target), right released
             (sb-thread:grab-mutex wasmdown.component.runtime::*store-right*)
             (wasmdown.component.runtime::%right-take c)
             (setf (wasmdown.component.runtime::%store-right-ticket) tg
                   (right-handoffs)
                   (list (cons c tg))
                   (wasmdown.component.runtime::%store-right-owner) nil)
             (sb-thread:release-mutex
              wasmdown.component.runtime::*store-right*)
             (sleep 0.05)
             (is (not peer-took))       ; the window is not a turn
             ;; the caller takes the right back and closes the window
             (wasmdown.component.runtime::%right-acquire c)
             (is (sb-thread:holding-mutex-p
                  wasmdown.component.runtime::*store-right*))
             (is (eq (wasmdown.component.runtime::%store-right-ticket) c))
             (pop (right-handoffs))
             (is (null (right-handoffs)))
             (wasmdown.component.runtime::%right-drop c)
             (sb-thread:join-thread peer :timeout 5.0)
             (is peer-took)
             (is (not violation)))
           ;; ---- (a2) the target's PARK names the handoff caller
           (setf (right-handoffs)
                 (list (cons c tg))
                 (wasmdown.component.runtime::%store-right-ticket) tg
                 (wasmdown.component.runtime::%store-threads)
                 (list c tg))
           (sb-thread:grab-mutex wasmdown.component.runtime::*store-right*)
           (let ((wasmdown.component.runtime::*right-self* tg))
             (wasmdown.component.runtime::%right-take tg)
             (wasmdown.component.runtime::%right-yield tg))
           (is (eq (wasmdown.component.runtime::%store-right-ticket) c))
           (is (plusp (wasmdown.component.runtime::async-thread-wake-count
                       c)))
           ;; ---- (a3) the target's EXIT names the handoff caller
           (setf (right-handoffs)
                 (list (cons c tg))
                 (wasmdown.component.runtime::%store-right-ticket) tg
                 (wasmdown.component.runtime::%store-threads)
                 (list c tg))
           (sb-thread:grab-mutex wasmdown.component.runtime::*store-right*)
           (let ((wasmdown.component.runtime::*right-self* tg))
             (wasmdown.component.runtime::%right-take tg)
             (wasmdown.component.runtime::%right-release-on-exit tg))
           (is (eq (wasmdown.component.runtime::%store-right-ticket) c)))
      ;; never leave the store right or the frames to the next test
      (when (sb-thread:holding-mutex-p
             wasmdown.component.runtime::*store-right*)
        (sb-thread:release-mutex
         wasmdown.component.runtime::*store-right*))
      (setf (wasmdown.component.runtime::%store-threads) old-threads
            (wasmdown.component.runtime::%store-right-ticket) old-ticket
            (right-handoffs) old-frames
            (wasmdown.component.runtime::%store-right-owner) old-owner))
    ;; ---- (b) the fixture row VERBATIM (component + assert_return)
    (let ((wast (%pin-slice "tests/component/async/reentrance.wast"
                            239 346)))
      (dotimes (i 20)
        (is (%pin-rows-pass-p
             wast (format nil "reentrance-14-pin-~d.wast" i)))))))

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

(pin-test component/two-providers-drop-own-finalizers-pin
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
  `(let ((wasmdown.component.runtime::*current-component-store*
           (wasmdown.component.runtime:make-component-store))
         (wasmdown.component.runtime::*right-self* nil)
         (wasmdown.component.runtime::*current-thread* nil)
         (wasmdown.component.runtime::*current-task* nil))
     (unwind-protect
          (progn ,@body)
       (dolist (th (wasmdown.component.runtime::%store-threads))
         (setf (wasmdown.component.runtime::async-thread-abandoned th) t
               (wasmdown.component.runtime::async-thread-suspended-p th) nil
               (wasmdown.component.runtime::async-thread-yield-parked th) nil
               (wasmdown.component.runtime::async-thread-switch-parked th)
               nil)
         (sb-thread:with-recursive-lock
             ((wasmdown.component.runtime::async-thread-mutex th))
           (sb-thread:condition-broadcast
            (wasmdown.component.runtime::async-thread-waitqueue th))
           (sb-thread:condition-broadcast
            (wasmdown.component.runtime::async-thread-suspend-waitqueue th))))
       (sb-thread:with-recursive-lock
           (wasmdown.component.runtime::*scheduler-mutex*)
         (setf (wasmdown.component.runtime::%store-threads) nil)))))

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
    (pushnew th (wasmdown.component.runtime::%store-threads)
             :test #'eq))
  th)

(defun %pin-unregister (inst th)
  (let ((fields (wasmdown.component.runtime::%inst-async-fields inst)))
    (setf (getf fields :threads) (remove th (getf fields :threads)
                                        :test #'eq)))
  (sb-thread:with-recursive-lock
      (wasmdown.component.runtime::*scheduler-mutex*)
    (setf (wasmdown.component.runtime::%store-threads)
          (remove th (wasmdown.component.runtime::%store-threads)
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
              (%pin-in-store ()
                (let ((wasmdown.component.runtime::*current-thread* th)
                      (wasmdown.component.runtime::*right-self* th))
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

(pin-test component/explicit-resume-requires-parked-peer-pin
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
                    (%pin-in-store ()
                      (let ((wasmdown.component.runtime::*current-thread*
                              caller)
                            (wasmdown.component.runtime::*current-task*
                              (wasmdown.component.runtime::async-thread-task
                               caller))
                            (wasmdown.component.runtime::*right-self* caller))
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

(pin-test component/premature-park-release-pin
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
               (gate nil))
          (setf (wasmdown.component.runtime::async-task-implicit-thread
                 peer-task)
                peer)
          (setf gate
                (sb-thread:make-thread
                  (%pin-in-store ()
                    (let ((wasmdown.component.runtime::*current-thread* peer)
                          (wasmdown.component.runtime::*current-task*
                            peer-task)
                          (wasmdown.component.runtime::*right-self* peer))
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
          ;; the exit releases the holder's lock AND re-tests the gate
          ;; (§3495): the waiting peer may already be entering on its
          ;; own OS thread, so the stable invariant is that the HOLDER
          ;; no longer owns the lock (the join below proves the peer
          ;; got in). Asserting NULL here raced that gate handoff —
          ;; the flake this pin used to exhibit under load.
          (let ((ex (wasmdown.component.runtime::%inst-exclusive-thread
                     inst)))
            (is (not (eq holder ex)))
            (is (or (null ex) (eq peer ex))))
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

(pin-test component/switch-caller-is-not-grantable-pin
  ;; THE EXPLICIT SWITCH IS A TARGET-LIMITED HANDOFF
  ;; (CanonicalABI.md 474-484, 528-565; the §242-273 handler
  ;; discipline). The spec's switch is SYNCHRONOUS: `block(switch_to
  ;; = other)' hands the CPU to `other' inside the SAME Thread.resume
  ;; chain, which runs `other' until it blocks or returns
  ;; (§474-484). Only then does control come back — at the target's
  ;; PARK/EXIT BOUNDARY (a §474 resume from a true block or the
  ;; exit; a turn-boundary bump when the target's first turn ends at
  ;; a READY yield park §523, which hands nothing away), never at a
  ;; §932 tick. A caller parked in a switch is therefore NOT a
  ;; tick/ready-grant candidate (§936: only WAITING+ready threads
  ;; are) and a generic nudge (wake-count) must not release it: it
  ;; is the same exclusion the suspend park has for its own park.
  ;;
  ;; The measured dssc::2 failure was exactly this: a resume-all
  ;; fan-out nudged the yield-parked switch caller while the
  ;; promoted worker had not yet taken its first turn, the yield
  ;; branch of the switch park accepted any wake, and the guest's
  ;; `run-promote' reached its `unreachable' check with the worker
  ;; not yet run. Pinned here on a synthetic pair driven through
  ;; the REAL canon builtin:
  ;;  (1) the target takes its FIRST TURN before the caller's post-
  ;;      switch code, and the caller STAYS parked in that window —
  ;;      a plain nudge AND async-thread-resume-all do not release
  ;;      it; only the target's next park/exit boundary does;
  ;;  (2) the target's RETURN (the production exit path) releases
  ;;      the caller (the exit-side handoff, §4886-4894);
  ;;  (3) the same-index switch is still the guest's trap
  ;;      (§4728/§4749) — the switch-park state did not widen it;
  ;;  (4) the switch-park state is gone on every exit path.
  ;;
  ;; The last during-sync row runs VERBATIM too: its `run-promote' end
  ;; is the guest's own `unreachable' check that the promoted worker
  ;; has taken its first turn (the in-lane rate this pin keeps at 0).
  (is (%pin-rows-pass-p
       (%pin-slice "tests/component/async/during-sync-scheduling-candidates.wast"
                   1 100000)
       "during-sync-scheduling-candidates.wast"))
  (%pin-with-store
    (let* ((inst (%pin-inst))
           (lock (sb-thread:make-mutex :name "pin-switch-order"))
           (order nil)
           (note (lambda (x)
                   (sb-thread:with-mutex (lock) (push x order))))
           (seen-p (lambda (x)
                     (sb-thread:with-mutex (lock)
                       (and (member x order) t)))))
      (labels ((pos (x)
                 (sb-thread:with-mutex (lock)
                   (position x (reverse order))))
               (switch-parked-p (th)
                 (wasmdown.component.runtime::async-thread-switch-parked th))
               (kw (name)
                 (%pin-builtin inst name)))
        ;; ---- (1) release at the target's park boundary
        (let* ((target (%pin-thread inst 2 '(1 0)))
               (caller (%pin-thread inst 1 '(1)))
               (gate (list nil))
               (target-done (list nil))
               (caller-done (list nil))
               (caller-result (list nil))
               (caller-err (list nil)))
          (%pin-register inst target)
          (%pin-register inst caller)
          (let ((bt (sb-thread:make-thread
                     (%pin-in-store ()
                       (let ((wasmdown.component.runtime::*current-thread*
                               target)
                             (wasmdown.component.runtime::*right-self*
                               target))
                         (unwind-protect
                              (handler-case
                                  (progn
                                    ;; the target's first turn = this
                                    ;; yield park being resumed by the
                                    ;; switch (§536/§560)
                                    (wasmdown.component.runtime::async-thread-yield
                                     target)
                                    (funcall note :target-first-turn)
                                    ;; end the quantum, then HOLD the
                                    ;; handoff: the caller must stay
                                    ;; parked in this window
                                    (wasmdown.component.runtime::%right-yield
                                     target)
                                    (%pin-wait-for
                                     (lambda () (car gate))
                                     :timeout 10
                                     :what "the switch pin's gate")
                                    (funcall note :target-post)
                                    ;; THE TURN BOUNDARY: the next park
                                    ;; ends the target's first turn
                                    ;; (§474-484) — its READY park
                                    ;; bumps the turn count and nudges
                                    ;; the switcher (§523: no resume)
                                    (wasmdown.component.runtime::async-thread-yield
                                     target))
                                (error (c) (funcall note (list :target-err c))))
                           (wasmdown.component.runtime::%right-release-on-exit
                            target)
                           (setf (car target-done) t))))
                     :name "pin-switch-target")))
            (%pin-wait-for
             (lambda ()
               (wasmdown.component.runtime::async-thread-yield-parked target))
             :what "the target's yield park")
            (let ((bc (sb-thread:make-thread
                       (%pin-in-store ()
                         (let ((wasmdown.component.runtime::*current-thread*
                                 caller)
                               (wasmdown.component.runtime::*current-task*
                                 (wasmdown.component.runtime::async-thread-task
                                  caller))
                               (wasmdown.component.runtime::*right-self*
                                 caller))
                           (unwind-protect
                                (handler-case
                                    (progn
                                      (setf (car caller-result)
                                            (wasmdown.runtime:wasm-call
                                             (kw :thread-yield-then-promote)
                                             2))
                                      (funcall note :caller-returned))
                                  (error (c) (setf (car caller-err) c)))
                             (wasmdown.component.runtime::%right-release-on-exit
                              caller)
                             (setf (car caller-done) t))))
                       :name "pin-switch-caller")))
              ;; the caller is PARKED IN THE SWITCH, and the target has
              ;; already taken its first turn (the switch resumed it)
              (%pin-wait-for (lambda () (switch-parked-p caller))
                             :what "the caller's switch park")
              (%pin-wait-for (lambda () (funcall seen-p :target-first-turn))
                             :what "the target's first turn")
              (is (eq (wasmdown.component.runtime::async-thread-switch-target
                       caller)
                      target))
              (is (null (car caller-done)))
              ;; (1a) a PLAIN NUDGE does not release it and posts no
              ;; resume credit (§936: not a tick candidate)
              (wasmdown.component.runtime::async-thread-nudge caller)
              (sleep 0.05)
              (is (null (car caller-done)))
              (is (switch-parked-p caller))
              (is (zerop
                   (wasmdown.component.runtime::async-thread-resume-count
                    caller)))
              ;; (1b) a whole resume-all fan-out does not either — the
              ;; switch caller is excluded from the grant set
              (wasmdown.component.runtime::async-thread-resume-all)
              (sleep 0.05)
              (is (null (car caller-done)))
              (is (switch-parked-p caller))
              (is (zerop
                   (wasmdown.component.runtime::async-thread-resume-count
                    caller)))
              (is (not (funcall seen-p :caller-returned)))
              ;; only the target's park/exit BOUNDARY releases it
              (setf (car gate) t)
              (is (%pin-wait-for (lambda () (car caller-done))
                                 :timeout 5
                                 :what "the caller leaving its switch park"))
              (is (null (car caller-err)))
              (is (equal '(0) (car caller-result)))
              (is (member :target-post order))
              (is (< (pos :target-first-turn) (pos :caller-returned)))
              (is (< (pos :target-post) (pos :caller-returned)))
              ;; the switch state died with the park
              (is (not (switch-parked-p caller)))
              (is (null
                   (wasmdown.component.runtime::async-thread-switch-target
                    caller)))
              (is (not (wasmdown.component.runtime::async-thread-yield-parked
                        caller)))
              (is (not (wasmdown.component.runtime::async-thread-suspended-p
                        caller)))
              (is (%pin-clean-p caller))
              (is (%pin-join bc))
              (is (null (car caller-err))))
            (is (%pin-join bt))
            (is (car target-done))))
        ;; ---- (3) the same-index switch is STILL the guest's trap
        (let* ((trap-target (%pin-thread inst 5 '(1 5)))
               (trap-caller (%pin-thread inst 6 '(1 6))))
          (%pin-register inst trap-target)
          (%pin-register inst trap-caller)
          (let ((o (%pin-arm-outcome inst :thread-yield-then-promote
                                     trap-caller 6)))
            (is (typep o 'wasmdown:wasm-trap))
            (is (search "cannot switch to the current thread"
                        (wasmdown:wasmdown-error-message o))))
          (is (not (switch-parked-p trap-caller)))
          (is (null
               (wasmdown.component.runtime::async-thread-switch-target
                trap-caller)))
          (%pin-unregister inst trap-target)
          (%pin-unregister inst trap-caller))
        ;; ---- (2) the target's RETURN releases the caller too
        (let* ((target2 (%pin-thread inst 3 '(1 7)))
               (caller2 (%pin-thread inst 4 '(1 8)))
               (caller2-done (list nil))
               (caller2-result (list nil))
               (caller2-err (list nil)))
          (%pin-register inst target2)
          (%pin-register inst caller2)
          ;; a suspended PENDING SPAWN (§4590): a promote target; the
          ;; switch's resume STARTS it (the thunk is the no-op body)
          (setf (wasmdown.component.runtime::async-thread-spawn-thunk
                 target2)
                (lambda () nil))
          (let ((bc (sb-thread:make-thread
                     (%pin-in-store ()
                       (let ((wasmdown.component.runtime::*current-thread*
                               caller2)
                             (wasmdown.component.runtime::*current-task*
                               (wasmdown.component.runtime::async-thread-task
                                caller2))
                             (wasmdown.component.runtime::*right-self*
                               caller2))
                         (unwind-protect
                              (handler-case
                                  (progn
                                    (setf (car caller2-result)
                                          (wasmdown.runtime:wasm-call
                                           (kw :thread-yield-then-promote)
                                           3))
                                    (funcall note :caller2-returned))
                                (error (c) (setf (car caller2-err) c)))
                           (wasmdown.component.runtime::%right-release-on-exit
                            caller2)
                           (setf (car caller2-done) t))))
                     :name "pin-switch-caller2")))
            (%pin-wait-for (lambda () (switch-parked-p caller2))
                           :what "caller2's switch park")
            (is (null (car caller2-done)))
            (is (eq caller2
                    (wasmdown.component.runtime::async-thread-post-to
                     target2)))
            ;; the TARGET'S RETURN: the production implicit-thread
            ;; exit posts the handoff (§4886-4894)
            (wasmdown.component.runtime::%task-exit-implicit-thread
             (wasmdown.component.runtime::async-thread-task target2))
            (is (%pin-wait-for (lambda () (car caller2-done))
                               :timeout 5
                               :what "caller2 leaving its switch park"))
            (is (null (car caller2-err)))
            (is (equal '(0) (car caller2-result)))
            (is (not (switch-parked-p caller2)))
            (is (%pin-clean-p caller2))
            (is (%pin-join bc))))))))

(pin-test component/crcc-1-pin
  ;; THE OUTER RESUMER WAITS FOR THE WHOLE SWITCH CHAIN
  ;; (CanonicalABI.md 469-484: Thread.resume runs the switch_to chain
  ;; in a loop until a continuation RETURNS or BLOCKS with no thread
  ;; to switch to; only then does resume return to its caller).
  ;; cancel-resumed-callback-switch's guest checks the same boundary:
  ;;
  ;;   (if (i32.eqz (call $ran)) (then unreachable))
  ;;
  ;; after subtask.cancel returns BLOCKED (-1). $ran is stored by the
  ;; callback's switched-to explicit thread before that thread blocks,
  ;; so the canceller's RESUMED callback being SUSPENDED-P/SWITCH-PARKED
  ;; is NOT enough: the arm records its own switch state BEFORE the
  ;; target's first turn, and the canceller's spin used to accept that
  ;; arm-time suspension and return with $ran still 0. The provenance
  ;; (instrumented, target start deliberately widened): a canceller
  ;; boundary read with susp=T switch=T target=2 tparks=0 (the target's
  ;; park count had not advanced past the arm sample) — and the guest
  ;; check at ::1 trapped; ::3 (the cross-task shape) trapped the same
  ;; way. THE FIX: switch-target-parks samples the target's park count
  ;; at the arm, and the canceller's BLOCKS case requires
  ;; %SWITCH-CHAIN-ENDED-P — the target's park/exit, recursively through
  ;; a nested switch.
  ;;
  ;; (a) drives the REAL cancellation + switch builtins on a synthetic
  ;; pair: the target's first turn is GATED, so the pre-fix rule returns
  ;; in the window deterministically (the pin asserts it does not);
  ;; (b) runs the fixture rows VERBATIM, repeatedly.
  (%pin-with-store
    (let* ((inst (%pin-inst))
           (caller (%pin-thread inst 1 '(1)))
           (ctask (wasmdown.component.runtime::async-thread-task caller))
           (target (%pin-thread inst 2 '(1 2)))
           (canceller (%pin-thread inst 3 '(2)))
           (gate (list nil))
           (lock (sb-thread:make-mutex :name "crcc-pin"))
           (order nil)
           (note (lambda (x)
                   (sb-thread:with-mutex (lock) (push x order))))
           (seen-p (lambda (x)
                     (sb-thread:with-mutex (lock)
                       (and (member x order) t))))
           (pos (lambda (x)
                  (sb-thread:with-mutex (lock)
                    (position x (reverse order)))))
           (caller-done (list nil))
           (caller-err (list nil))
           (target-done (list nil))
           (cancel-done (list nil))
           (cancel-err (list nil))
           (bt nil) (bc nil))
      (%pin-register inst caller)
      (%pin-register inst target)
      (%pin-register inst canceller)
      (setf (wasmdown.component.runtime::async-thread-cancellable caller) t
            (wasmdown.component.runtime::async-task-state ctask) :started)
      ;; the target is a SUSPENDED pending spawn whose first turn is
      ;; gated: the arm's resume creates the OS thread, which does not
      ;; run until the gate opens
      (setf (wasmdown.component.runtime::async-thread-spawn-thunk target)
            (lambda ()
              (sb-thread:make-thread
               (%pin-in-store ()
                 (let ((wasmdown.component.runtime::*current-thread* target)
                       (wasmdown.component.runtime::*current-task*
                         (wasmdown.component.runtime::async-thread-task
                          target))
                       (wasmdown.component.runtime::*right-self* target))
                   (unwind-protect
                        (handler-case
                            (progn
                              (%pin-wait-for
                               (lambda () (car gate))
                               :timeout 10
                               :what "the crcc pin's target gate")
                              (funcall note :target-first-turn)
                              ;; the target's first turn ENDS here, at
                              ;; a real park (§474-484's block)
                              (wasmdown.component.runtime::async-thread-yield
                               target))
                          (error (c) (funcall note (cons :target-err c))))
                     (wasmdown.component.runtime::%right-release-on-exit
                      target)
                     (setf (car target-done) t))))
               :name "pin-crcc-target")))
      ;; the caller: the cancelled task's implicit thread parks in a
      ;; REAL WAITING park whose readiness is the pending-cancel mark,
      ;; then SWITCHES to the gated target through the REAL canon
      ;; builtin (suspend_then_resume)
      (setf bt
            (sb-thread:make-thread
             (%pin-in-store ()
               (let ((wasmdown.component.runtime::*current-thread* caller)
                     (wasmdown.component.runtime::*current-task* ctask)
                     (wasmdown.component.runtime::*right-self* caller))
                 (unwind-protect
                      (handler-case
                          (progn
                            ;; the §1373-style WAITING park readiness is
                            ;; also the THREAD slot the cancellation's
                            ;; ready() gate reads (the callback WAIT
                            ;; arm installs it the same way)
                            (setf (wasmdown.component.runtime::async-thread-ready-fn
                                   caller)
                                  (lambda ()
                                    (wasmdown.component.runtime::%task-has-pending-cancel
                                     ctask)))
                            (wasmdown.component.runtime::async-thread-wait-until
                             caller
                             (wasmdown.component.runtime::async-thread-ready-fn
                              caller))
                            (setf (wasmdown.component.runtime::async-thread-ready-fn
                                   caller)
                                  nil)
                            (funcall note :caller-resumed)
                            (wasmdown.runtime:wasm-call
                             (%pin-builtin inst
                                           :thread-suspend-then-resume)
                             2)
                            (funcall note :caller-switch-returned))
                        (error (c) (setf (car caller-err) c)))
                   (wasmdown.component.runtime::%right-release-on-exit
                    caller)
                   (setf (car caller-done) t))))
             :name "pin-crcc-caller"))
      (unwind-protect
           (progn
             (%pin-wait-for
              (lambda ()
                (wasmdown.component.runtime::async-thread-ready-fn caller))
              :what "the caller's WAITING park")
             ;; the canceller: the REAL %TASK-REQUEST-CANCELLATION
             (setf bc
                   (sb-thread:make-thread
                    (%pin-in-store ()
                      (let ((wasmdown.component.runtime::*current-thread*
                              canceller)
                            (wasmdown.component.runtime::*current-task*
                              (wasmdown.component.runtime::async-thread-task
                               canceller))
                            (wasmdown.component.runtime::*right-self*
                              canceller))
                        (unwind-protect
                             (handler-case
                                 (progn
                                   ;; bounded: a rule that never
                                   ;; accepts the chain cannot hang
                                   ;; the pin
                                   (sb-ext:with-timeout
                                       15
                                     (wasmdown.component.runtime::
                                      %task-request-cancellation ctask))
                                   (funcall note :cancel-returned))
                               (error (c) (setf (car cancel-err) c)))
                          ;; the synthetic canceller is not a guest
                          ;; that continues under the right: hand it on
                          (wasmdown.component.runtime::%right-release-on-exit
                           canceller)
                          (setf (car cancel-done) t))))
                    :name "pin-crcc-canceller"))
             ;; the caller is PARKED IN THE SWITCH while the target
             ;; has not taken its first turn
             (is (%pin-wait-for
                  (lambda ()
                    (wasmdown.component.runtime::async-thread-switch-parked
                     caller))
                  :what "the caller's switch park"))
             (is (funcall seen-p :caller-resumed))
             (sleep 0.1)
             ;; (a) THE RULE: no return while the chain has not ended
             (is (not (funcall seen-p :target-first-turn)))
             (is (null (car cancel-done)))
             (funcall note :gate-open)
             (setf (car gate) t)
             (is (%pin-wait-for (lambda () (car cancel-done))
                                :timeout 10
                                :what "the cancellation after the chain parked"))
             (is (null (car cancel-err)))
             (is (funcall seen-p :target-first-turn))
             (let ((p1 (funcall pos :target-first-turn))
                   (p2 (funcall pos :cancel-returned)))
               (is (and p1 p2 (< p1 p2))))
             ;; the target's ordinary park is a bare nudge: the switch
             ;; caller stays parked until an EXPLICIT resume (only the
             ;; target's park/exit hands control on)
             (is (wasmdown.component.runtime::async-thread-switch-parked
                  caller))
             (is (null (car caller-done))))
        ;; release both threads on every path (bounded)
        (setf (car gate) t)
        (ignore-errors
         (wasmdown.component.runtime::async-thread-resume caller))
        (ignore-errors
         (wasmdown.component.runtime::async-thread-nudge target))
        (when bc (ignore-errors (%pin-join bc)))
        (when bt (ignore-errors (%pin-join bt)))
        (is (%pin-wait-for (lambda () (car caller-done))
                           :timeout 5
                           :what "the switch caller's exit"))
        (is (%pin-wait-for (lambda () (car target-done))
                           :timeout 5
                           :what "the target's exit"))
        (is (null (car caller-err)))
        (is (null (car cancel-err))))))
  ;; (b) the fixture rows VERBATIM — both components (::1 and ::3)
  (let ((wast (%pin-slice
               "tests/component/async/cancel-resumed-callback-switch.wast"
               1 100000)))
    (dotimes (i 10)
      (is (%pin-rows-pass-p
           wast (format nil "crcc-1-pin-~d.wast" i))))))

;;; ---------------------------------------------------------------
;;; M17 step 2 (borrow-side): the two lifetime rules that had no
;;; pin. Both reuse the guest-drop pin's construction vector: a
;;; provider instance, the ABI's own resource handle records on a
;;; guest table (the provider ct), the real canon built-ins, and the
;;; released state asserted afterwards.

(defparameter *pin-borrow-return-src*
  ;; The `drop-cross-task-borrow' shape, narrowed to the
  ;; return-with-a-live-borrow rule. THREE components, because the
  ;; callee must NOT be the resource's implementor: $C defines R,
  ;; $Prov (an importer of R) owns the two async funcs taking
  ;; `(borrow $R)', and $Caller creates the resource and drives the
  ;; calls. §2834's same-implementor shortcut (lower_borrow yields
  ;; the REP and no handle when the CALLEE's type space declared the
  ;; resource) is exactly why $Prov cannot be the definer here:
  ;; having imported R by alias, it really owes the drop. The cores
  ;; stash the lent handle and BLOCK on an empty waitable set (so the
  ;; caller's pack is deterministically STARTED); $Caller wakes them
  ;; and waits on the subtask's event. `ret-live' returns WITHOUT
  ;; dropping the borrow (§802 trap_if(num_borrows > 0));
  ;; `ret-dropped' calls the real canon resource.drop first (§3798's
  ;; num_borrows decrement) and must succeed.
    "(component
     (component $C
       (type $R' (resource (rep i32)))
       (canon resource.new $R' (core func $resource.new))
       (core module $RepM (func (export \"id\") (param i32) (result i32) (local.get 0)))
       (core instance $rep (instantiate $RepM))
       (alias core export $rep \"id\" (core func $resource.rep))
       (export $R \"R\" (type $R'))
       (func (export \"R-new\") (param \"rep\" u32) (result (own $R))
         (canon lift (core func $resource.new)))
       (func (export \"R-rep\") (param \"self\" (borrow $R)) (result u32)
         (canon lift (core func $resource.rep))))
     (component $Prov
       (import \"c\" (instance $c
         (export \"R\" (type $R (sub resource)))
         (export \"R-new\" (func (param \"rep\" u32) (result (own $R))))
         (export \"R-rep\" (func (param \"self\" (borrow $R)) (result u32)))))
       (type $FT (future))
       (alias export $c \"R\" (type $R))
       (core module $PM
         (import \"\" \"task.return\" (func $task.return (param i32)))
         (import \"\" \"R-drop\" (func $R-drop (param i32)))
         (import \"\" \"waitable-set.new\" (func $wsn (result i32)))
         (import \"\" \"waitable.join\" (func $join (param i32 i32)))
         (import \"\" \"future.new\" (func $fnew (result i64)))
         (import \"\" \"future.read\" (func $fread (param i32 i32) (result i32)))
         (import \"\" \"future.write\" (func $fwrite (param i32 i32) (result i32)))
         (global $rep (mut i32) (i32.const 0))
         (global $ws (mut i32) (i32.const 0))
         (func (export \"ret-live\") (param $h i32) (result i32)
           (global.set $rep (local.get $h))
           (global.set $ws (call $wsn))
           (i32.or (i32.const 2) (i32.shl (global.get $ws) (i32.const 4))))
         (func (export \"ret-live-cb\") (param i32 i32 i32) (result i32)
           (call $task.return (i32.const 7))
           (i32.const 0))
         (func (export \"ret-dropped-cb\") (param i32 i32 i32) (result i32)
           (call $R-drop (global.get $rep))
           (call $task.return (i32.const 7))
           (i32.const 0))
         (func (export \"wake\")
           (local $r i32) (local $r64 i64) (local $futr i32) (local $futw i32)
           (local.set $r64 (call $fnew))
           (local.set $futr (i32.wrap_i64 (local.get $r64)))
           (local.set $futw (i32.wrap_i64 (i64.shr_u (local.get $r64) (i64.const 32))))
           (local.set $r (call $fread (local.get $futr) (i32.const 0xdeadbeef)))
           (if (i32.ne (i32.const -1) (local.get $r)) (then unreachable))
           (local.set $r (call $fwrite (local.get $futw) (i32.const 0xdeadbeef)))
           (if (i32.ne (i32.const 0) (local.get $r)) (then unreachable))
           (call $join (local.get $futr) (global.get $ws))))
       (canon task.return (result u32) (core func $task.return))
       (canon resource.drop $R (core func $R-drop))
       (canon waitable-set.new (core func $wsn))
       (canon waitable.join (core func $join))
       (canon future.new $FT (core func $fnew))
       (canon future.read $FT async (core func $fread))
       (canon future.write $FT async (core func $fwrite))
       (core instance $pm (instantiate $PM (with \"\" (instance
         (export \"task.return\" (func $task.return))
         (export \"R-drop\" (func $R-drop))
         (export \"waitable-set.new\" (func $wsn))
         (export \"waitable.join\" (func $join))
         (export \"future.new\" (func $fnew))
         (export \"future.read\" (func $fread))
         (export \"future.write\" (func $fwrite))))))
       (func (export \"ret-live\") async (param \"self\" (borrow $R)) (result u32)
         (canon lift (core func $pm \"ret-live\") async (callback (core func $pm \"ret-live-cb\"))))
       (func (export \"ret-dropped\") async (param \"self\" (borrow $R)) (result u32)
         (canon lift (core func $pm \"ret-live\") async (callback (core func $pm \"ret-dropped-cb\"))))
       (func (export \"wake\") (canon lift (core func $pm \"wake\"))))
     (component $Caller
       (import \"c\" (instance $c
         (export \"R\" (type $R (sub resource)))
         (export \"R-new\" (func (param \"rep\" u32) (result (own $R))))))
       (alias export $c \"R\" (type $R))
       (import \"prov\" (instance $p
         (export \"ret-live\" (func async (param \"self\" (borrow $R)) (result u32)))
         (export \"ret-dropped\" (func async (param \"self\" (borrow $R)) (result u32)))
         (export \"wake\" (func))))
       (core module $Memory (memory (export \"mem\") 1))
       (core instance $memory (instantiate $Memory))
       (core module $EM
         (import \"\" \"mem\" (memory 1))
         (import \"\" \"waitable.join\" (func $join (param i32 i32)))
         (import \"\" \"waitable-set.new\" (func $wsn (result i32)))
         (import \"\" \"waitable-set.wait\" (func $wsw (param i32 i32) (result i32)))
         (import \"\" \"R-new\" (func $R-new (param i32) (result i32)))
         (import \"\" \"ret-live\" (func $ret-live (param i32 i32) (result i32)))
         (import \"\" \"ret-dropped\" (func $ret-dropped (param i32 i32) (result i32)))
         (import \"\" \"wake\" (func $wake))
         (func $run (param $which i32) (result i32)
           (local $ret i32) (local $h i32) (local $sub i32)
           (local $ws i32) (local $ev i32)
           (local.set $h (call $R-new (i32.const 10)))
           (if (i32.eqz (local.get $which))
             (then (local.set $ret (call $ret-live (local.get $h) (i32.const 16))))
             (else (local.set $ret (call $ret-dropped (local.get $h) (i32.const 16)))))
           (if (i32.ne (i32.const 1) (i32.and (local.get $ret) (i32.const 0xf)))
             (then unreachable))
           (local.set $sub (i32.shr_u (local.get $ret) (i32.const 4)))
           (local.set $ws (call $wsn))
           (call $join (local.get $sub) (local.get $ws))
           (call $wake)
           (local.set $ev (call $wsw (local.get $ws) (i32.const 32)))
           (if (i32.ne (i32.const 1) (local.get $ev)) (then unreachable))
           (if (i32.ne (i32.const 2) (i32.load offset=4 (i32.const 32))) (then unreachable))
           (if (i32.ne (i32.const 7) (i32.load (i32.const 16))) (then unreachable))
           (i32.const 42))
         (func (export \"run-live\") (result i32) (call $run (i32.const 0)))
         (func (export \"run-dropped\") (result i32) (call $run (i32.const 1))))
       (canon waitable.join (core func $join))
       (canon waitable-set.new (core func $wsn))
       (canon waitable-set.wait (memory (core memory $memory \"mem\")) (core func $wsw))
       (canon lower (func $c \"R-new\") (core func $R-new))
       (canon lower (func $p \"ret-live\") async (memory (core memory $memory \"mem\")) (core func $ret-live))
       (canon lower (func $p \"ret-dropped\") async (memory (core memory $memory \"mem\")) (core func $ret-dropped))
       (canon lower (func $p \"wake\") (core func $wake))
       (core instance $em (instantiate $EM (with \"\" (instance
         (export \"mem\" (memory $memory \"mem\"))
         (export \"waitable.join\" (func $join))
         (export \"waitable-set.new\" (func $wsn))
         (export \"waitable-set.wait\" (func $wsw))
         (export \"R-new\" (func $R-new))
         (export \"ret-live\" (func $ret-live))
         (export \"ret-dropped\" (func $ret-dropped))
         (export \"wake\" (func $wake))))))
       (func (export \"run-live\") async (result u32) (canon lift (core func $em \"run-live\")))
       (func (export \"run-dropped\") async (result u32) (canon lift (core func $em \"run-dropped\"))))
     (instance $c (instantiate $C))
     (instance $p (instantiate $Prov (with \"c\" (instance $c))))
     (instance $e (instantiate $Caller (with \"c\" (instance $c)) (with \"prov\" (instance $p))))
     (func (export \"run-live\") (alias export $e \"run-live\"))
     (func (export \"run-dropped\") (alias export $e \"run-dropped\")))")

(defun %pin-graph-handles (ci pred)
  "Every handle record reachable from the comp-instance graph CI
satisfying PRED — the instance's own Table (where the ABI adds the
lowered borrows/owns and where the lender's SOURCE handle lives,
§1146) plus every instance the graph links. Cycle-safe by EQ."
  (let ((seen (make-hash-table :test #'eq))
        (out '()))
    (labels ((walk (i)
               (when (and (wasmdown.component.runtime:comp-instance-p i)
                          (not (gethash i seen)))
                 (setf (gethash i seen) t)
                 (let ((v (gethash :handles
                                   (wasmdown.component.runtime:comp-instance-imports
                                    i))))
                   (when v
                     (loop for h across v
                           when (and h (funcall pred h))
                             do (push h out))))
                 (loop for x across (wasmdown.component.runtime::comp-instance-instances
                                     i)
                       do (walk x))
                 (maphash (lambda (k v)
                            (declare (ignore k))
                            (walk v))
                          (wasmdown.component.runtime:comp-instance-imports i)))))
      (walk ci))
    out))

(defun %pin-run-borrow-component (name capture-p)
  "Fresh $Test instance; invoke NAME through the runner's action
hook. While CAPTURE-P, %TASK-RETURN is wrapped (it is DECLAIMed
NOTINLINE) to record, at each return ATTEMPT with outstanding
borrows, the SOURCE handles' NUM-LENDS — the instant the rule is
supposed to guard. Returns (values result captured ci); the
instance is torn down."
  (let* ((ci (%exec-comp *pin-borrow-return-src*))
         (orig (symbol-function 'wasmdown.component.runtime::%task-return))
         (captured '())
         (result nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'wasmdown.component.runtime::%task-return)
                 (lambda (task r)
                   (when (and capture-p
                              (plusp (wasmdown.component.runtime::async-task-num-borrows
                                      task)))
                     (push (mapcar
                            #'wasmdown.component.runtime::resource-handle-num-lends
                            (%pin-graph-handles
                             ci
                             (lambda (h)
                               (and (wasmdown.component.runtime::resource-handle-p
                                     h)
                                    (wasmdown.component.runtime::resource-handle-own-p
                                     h)
                                    (eql 10 (wasmdown.component.runtime::resource-handle-rep
                                             h))))))
                           captured))
                   (funcall orig task r)))
           (setf result
                 (handler-case
                     (list :ok
                           (funcall (wasmdown.wast:component-invoker)
                                    ci name '()))
                   (wasmdown:wasm-trap (c) (list :trap c)))))
      (setf (symbol-function 'wasmdown.component.runtime::%task-return) orig)
      (wasmdown.component.runtime::%store-teardown))
    (values result captured ci)))

(test component/live-borrow-blocks-task-return-pin
  ;; §802/§804 `Task.return_': `trap_if(num_borrows > 0)' — a task
  ;; that still holds a BORROW handle cannot return, and `num_borrows'
  ;; is §2838's per-task count of the handles lower_borrow added to
  ;; ITS instance (the source handle's num_lends, §1447, is the
  ;; separate ledger that keeps the SOURCE alive). The ABI's real
  ;; path is driven end to end: $C defines R, $Prov imports it,
  ;; $Caller creates an R through $C and calls $Prov's async funcs
  ;; with it; the callee blocks, then its callback returns while its
  ;; own borrow handle is still in $Prov's table (the callee is NOT
  ;; the implementor, so §2834's shortcut does not apply and the
  ;; drop really is owed).
  ;;
  ;; MEASURED BEFORE (the pre-change runtime): `run-live' RETURNED
  ;; 42 — the callee's task.return saw num_borrows 0, because the
  ;; lower had counted the lend on the LENDING task instead of
  ;; lowering it with borrow_scope = the callee Task; the subtask
  ;; resolved RETURNED, the caller's wait delivered it, and only a
  ;; piggy-backed caller count could ever have traced the rule. The
  ;; trap is now the CALLEE's own, with the source handle still lent
  ;; (num_lends 1) at that instant.
  (multiple-value-bind (result captured ci)
      (%pin-run-borrow-component "run-live" t)
    (is (eq :trap (first result)))
    (is (search "cannot return with outstanding borrows"
                (wasmdown:wasmdown-error-message (second result))))
    ;; the guard fired with the SOURCE handle's num_lends still 1 —
    ;; counted at the moment of the return, not cleared early
    (is (equal '((1)) captured))
    ;; the trap is not a permanent flag: the subtask's
    ;; deliver_resolve (§1469) released the lend on the way to the
    ;; caller's DELIVERY boundary, so no leak survives the trap
    (is (every #'zerop
               (mapcar #'wasmdown.component.runtime::resource-handle-num-lends
                       (%pin-graph-handles
                        ci
                        (lambda (h)
                          (and (wasmdown.component.runtime::resource-handle-p h)
                               (wasmdown.component.runtime::resource-handle-own-p h)
                               (eql 10 (wasmdown.component.runtime::resource-handle-rep
                                        h)))))))))
  ;; the SAME callee, dropping the borrow through the real canon
  ;; resource.drop before returning: §3798's decrement makes the
  ;; subsequent task.return succeed
  (multiple-value-bind (result captured)
      (%pin-run-borrow-component "run-dropped" nil)
    (declare (ignore captured))
    (is (equal '(:ok (42)) result))))

(test component/borrowed-handle-not-joinable-pin
  ;; §4125 `canon_waitable_join': `trap_if(not isinstance(w,
  ;; Waitable))' — a waitable set holds WAITABLES (stream/future
  ;; ends, subtasks, sets, §1250-1300); a resource handle is not one,
  ;; whatever its ownership, so a BORROWED handle (the handle
  ;; lower_borrow adds, §2832) cannot be made waitable. This is the
  ;; §1218-1396 rule's join arm. Same construction as the guest-drop
  ;; pin: a provider ct, the ABI's own handle records on a guest
  ;; table, and the REAL built-ins.
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
           ;; the import binding: the provider at an INSTANCES index
           (wasmdown.component.runtime::%push-instance guest provider)
           (let* ((own (wasmdown.component.runtime::%handle-add
                        guest ct desc t))
                  ;; the REAL lend (§2406/§1447): the source handle's
                  ;; num_lends rises and the lend joins the subtask's
                  ;; ledger
                  (subtask (wasmdown.component.runtime::make-async-subtask))
                  (rep (wasmdown.component.runtime::%handle-lend
                        guest ct own subtask))
                  ;; the borrow handle lower_borrow adds to the
                  ;; callee's table (§2832)
                  (bor (wasmdown.component.runtime::%handle-add
                        guest ct rep nil))
                  (join (wasmdown.component.runtime::%builtin-async-host-func
                         guest :waitable-join nil))
                  (wsn (wasmdown.component.runtime::%builtin-async-host-func
                        guest :waitable-set-new nil))
                  (fnew (wasmdown.component.runtime::%builtin-async-host-func
                         guest :future-new (list nil 0)))
                  (ws (first (wasmdown.runtime:wasm-call wsn))))
             (is (= 1 (wasmdown.component.runtime::resource-handle-num-lends
                       (wasmdown.component.runtime::%handle-resource-at
                        guest own))))
             (is (eq desc rep))
             ;; the BORROWED handle: rejected, and left in the table
             (let ((err (wasmdown.test::check-trap
                         (lambda ()
                           (wasmdown.runtime:wasm-call join bor ws))
                         :generic)))
               (is err)
               (is (search "invalid waitable index"
                           (wasmdown:wasmdown-error-message err))))
             (is (eq :borrow
                     (if (wasmdown.component.runtime::resource-handle-own-p
                          (wasmdown.component.runtime::%handle-resource-at
                           guest bor))
                         :own :borrow)))
             ;; an OWN RESOURCE handle is equally not a Waitable
             ;; (§4125's isinstance test is on the RECORD, not the
             ;; ownership) — join is for ends/subtasks/sets
             (is (wasmdown.test::check-trap
                  (lambda ()
                    (wasmdown.runtime:wasm-call join own ws))
                  :generic))
             ;; a real readable END joins, is in the set, and leaves
             ;; it on join(0) — the set's own element ledger
             (let* ((pair (first (wasmdown.runtime:wasm-call fnew)))
                    (ri (logand pair #xffffffff))
                    (end (aref (wasmdown.component.runtime::%inst-handles guest)
                               (1- ri))))
               (is (wasmdown.component.runtime::async-end-p end))
               (wasmdown.runtime:wasm-call join ri ws)
               (is (wasmdown.component.runtime::%waitable-in-wset end))
               (is (member end
                           (wasmdown.component.runtime::async-waitable-set-elems
                            (wasmdown.component.runtime::async-waitable-wset end))
                   :test #'eq))
               (wasmdown.runtime:wasm-call join ri 0)
               (is (not (wasmdown.component.runtime::%waitable-in-wset end))))))
      (let ((fd (wasmdown.wasi:wasi-descriptor-fd desc)))
        (when (and (integerp fd) (not (minusp fd)))
          (sb-unix:unix-close fd))))))

;;; ---- PLAN step 1: the generic handle walkers, five cases -----------
;;;
;;; The two canon-lower wrappers (%LOWER-HOST-FUNC,
;;; %CANON-LOWER-ASYNC-HOST-FUNC) carried POSITION-DEPENDENT arms for a
;;; top-level stream/future param and a top-level borrow param. Both
;;; arms are now judged by the probe set (the four named fixtures +
;;; the 12-fixture stream/future superset, 295 rows):
;;;
;;;  - the STREAM/FUTURE arm is REDUNDANT and was DELETED: the flat
;;;    walk's recursive %ABI-LIFT (:stream/:future -> %LIFT-ASYNC-END)
;;;    performs the same canonical transfer from the cx's INST (the
;;;    lowering caller), and no probe row moves without the arm.
;;;  - the BORROW arm is a MEASURED NEGATIVE and STAYS: without it the
;;;    async guest body resolves the caller's raw index at the CALLEE's
;;;    own table, moving passing-resources::1 and
;;;    drop-cross-task-borrow::2/::4/::6.
;;;  - an INDIRECT (spilled) handle and a NESTED borrow/own are NOT
;;;    carried: %ABI-LOAD's catch-all reads the i32 index and leaves
;;;    the handle in the caller's table. %ABI-LOAD's memory-path
;;;    (:stream :future) / (:own :borrow :resource) arms are a SECOND
;;;    measured negative (they moved WASI loopback rows); do not
;;;    re-add them to close this gap.
;;;
;;; The pin drives the real walkers with real handle records, and the
;;; wrapper parts build the actual lowered host funcs (a flat stream
;;; crossing, and the borrow lend), so a regression in the recursive
;;; carriers or in the borrow arm goes red here as well as in the
;;; fixtures. The ASYNC wrapper's lend arm is covered end-to-end by
;;; drop-cross-task-borrow (the subtask's num_lends ledger is
;;; component/borrowed-handle-not-joinable-pin's subject).

(defun %pin-memory ()
  "A fresh 1-page memory-instance (the indirect cases' linear memory)."
  (wasmdown.runtime::%make-memory-instance
   :bytes (make-array 65536 :element-type '(unsigned-byte 8)
                      :initial-element 0)
   :pages 1 :max-pages 1 :has-max-p t :i64-p nil))

(defun %pin-async-end (&optional (elem :u8))
  "A fresh READABLE stream end in its IDLE state (§1598-1621). The
element ct is RECORDED the way {stream,future}.new records it: the
lift's `end.t == t' guard (lift_async_value §2444) compares it with
the valtype's element, so a hand-built end without one is a type
mismatch, not a stream<u8>."
  (wasmdown.component.runtime::make-async-end
   :event-code wasmdown.component.runtime::+event-stream-read+
   :t-ct (if elem
             (wasmdown.component.runtime::make-ct* :prim elem)
             nil)
   :state :idle))

(test component/generic-path-five-cases-pin
  (let* ((rt (wasmdown.component.runtime::make-ct* :resource nil))
         (sct (wasmdown.component.runtime::make-ct* :stream :u8))
         (h* (lambda (inst) (wasmdown.component.runtime::%inst-handles inst)))
         (cx-of (lambda (inst &optional mem)
                  (wasmdown.component.runtime::make-abi-cx
                   :inst inst :type-inst inst :memory mem))))
    ;; CASE 1 — a stream<u8> in a FLAT position: the recursive lift
    ;; moves the READABLE end OUT of the caller's table (§2438, the
    ;; end LEAVES), clears its owner, and yields the END record.
    (let* ((guest (wasmdown.component.runtime:make-comp-instance))
           (cx (funcall cx-of guest))
           (end (%pin-async-end))
           (idx (wasmdown.component.runtime::%handle-add-any guest end))
           (vi (make-array 4 :fill-pointer 0 :adjustable t)))
      (vector-push-extend (cons idx :i32) vi)
      (is (eq end
              (first (wasmdown.component.runtime::%abi-lift-values
                      cx 16 vi (list sct)))))
      (is (null (aref (funcall h* guest) (1- idx))))
      (is (null (wasmdown.component.runtime::async-end-owner end))))
    ;; CASE 1b — the SAME flat stream through the REAL sync wrapper,
    ;; now that the wrapper's stream/future arm is GONE: the generic
    ;; walk consumes the caller's slot and the host callee receives the
    ;; END record (not its index).
    (let* ((callee (wasmdown.component.runtime:make-comp-instance))
           (caller (wasmdown.component.runtime:make-comp-instance))
           (ft (wasmdown.component.runtime::make-ct*
                :func (list (list (cons "s" sct))
                            (wasmdown.component.runtime::make-ct*
                             :prim :u32))))
           (end (%pin-async-end))
           (seen nil))
      (wasmdown.component.runtime::%push-func
       caller
       (wasmdown.component.runtime:make-lifted-func
        :type ft :inst callee
        :host (lambda (args) (setf seen (copy-list args)) 7)))
      (let* ((idx (wasmdown.component.runtime::%handle-add-any
                   caller end))
             (wrap (wasmdown.component.runtime::%lower-host-func
                    caller 0 nil)))
        (is (equal '(7) (wasmdown.runtime:wasm-call wrap idx)))
        (is (eq end (first seen)))
        (is (null (aref (funcall h* caller) (1- idx))))
        (is (null (wasmdown.component.runtime::async-end-owner end)))))
    ;; CASE 2 — the SAME stream INDIRECTLY (a spilled tuple in the
    ;; caller's memory). This pin was MIGRATED to the ABI contract:
    ;; load (§2133-2169) reads the i32 and DELEGATES to lift_stream
    ;; (§2161) -> lift_async_value (§2438), so the value is the END
    ;; RECORD, the readable end is REMOVED from the loading
    ;; instance's table (its owner/index cleared) and the slot is
    ;; freed. The previous expectation — the raw index with the end
    ;; left in the table — was the frozen catch-all's measured
    ;; negative, i.e. exactly the ownership leak this pin now
    ;; forbids: a memory-loaded payload never crossed a boundary.
    (let* ((guest (wasmdown.component.runtime:make-comp-instance))
           (mem (%pin-memory))
           (cx (funcall cx-of guest mem))
           (end (%pin-async-end))
           (idx (wasmdown.component.runtime::%handle-add-any guest end))
           (bytes (wasmdown.runtime:memory-instance-bytes mem)))
      (setf (wasmdown.component.runtime::async-end-owner end) guest
            (wasmdown.component.runtime::async-end-index end) idx)
      (loop for k below 4
            do (setf (aref bytes k)
                     (ldb (byte 8 (* 8 k)) idx)))
      (is (eq end (wasmdown.component.runtime::%abi-load cx sct 0)))
      ;; the SOURCE SLOT IS CONSUMED: not resident, freed for reuse,
      ;; and the end is host-owned (owner + index cleared, §2446)
      (is (null (aref (funcall h* guest) (1- idx))))
      (is (eql idx (wasmdown.component.runtime::%handle-add-any
                    guest :reused)))
      (is (null (wasmdown.component.runtime::async-end-owner end)))
      (is (null (wasmdown.component.runtime::async-end-index end)))
      ;; a second load of the SAME index is a STALE index — the slot
      ;; now holds :reused, which is not an end
      (assert-trap :generic
        (wasmdown.component.runtime::%abi-load cx sct 0)))
    ;; CASE 3a — a RECORD carrying a stream, an own and a borrow in a
    ;; FLAT position: the recursion transfers the STREAM field and
    ;; leaves the own/borrow i32 slots as raw indexes (the wrapper's
    ;; top-level borrow arm and lifted-func-call's own transfer are the
    ;; only places those move; neither is a valtype-recursion arm).
    (let* ((guest (wasmdown.component.runtime:make-comp-instance))
           (cx (funcall cx-of guest))
           (uct (wasmdown.component.runtime::make-ct* :own 0))
           (bct (wasmdown.component.runtime::make-ct* :borrow 0))
           (rec (wasmdown.component.runtime::make-ct*
                 :record (vector (cons "s" sct) (cons "o" uct)
                                 (cons "b" bct)
                                 (cons "n"
                                       (wasmdown.component.runtime::make-ct*
                                        :prim :u32)))))
           (end (%pin-async-end))
           (idx (wasmdown.component.runtime::%handle-add-any guest end))
           ;; %ABI-LIFT-VALUES reverses the scratch, so the pairs are in
           ;; FLAT order here: the first field is the first pair
           (vi (vector (cons idx :i32) (cons 9 :i32)
                       (cons 8 :i32) (cons 5 :i32)))
           (v (first (wasmdown.component.runtime::%abi-lift-values
                      cx 16 vi (list rec)))))
      (flet ((fld (label)
               (cdr (assoc label v :test #'string=))))
        (is (eq end (fld "s")))
        (is (= 9 (fld "o")))            ; raw index, no transfer
        (is (= 8 (fld "b")))            ; raw index, no lend
        (is (= 5 (fld "n")))))
    ;; CASE 3b — a fixed list<stream> lowers INSIDE a result: the
    ;; recursive lower adds the end to the lowering caller's table and
    ;; the flat result is (discriminant 0=ok, the new index).
    (let* ((guest (wasmdown.component.runtime:make-comp-instance))
           (cx (funcall cx-of guest))
           (lct (wasmdown.component.runtime::make-ct* :list (cons sct 1)))
           (rct (wasmdown.component.runtime::make-ct*
                 :result (cons lct
                               (wasmdown.component.runtime::make-ct*
                                :prim :u32))))
           (end (%pin-async-end))
           (n0 (length (funcall h* guest)))
           (flat (wasmdown.component.runtime::%abi-lower
                  cx (cons :ok (list end)) rct)))
      (is (equal '(0) (subseq flat 0 1)))
      (is (= (1+ n0) (length (funcall h* guest))))
      (is (eq end (aref (funcall h* guest) (1- (second flat))))))
    ;; CASE 4 — the err tag: the `(:err ...)' domain maps to the
    ;; "error" CASE 1 of result's two cases (flat), and the stored
    ;; discriminant byte is 1 with the payload at the join-aligned
    ;; offset (the HTTP-style authority alternates).
    (let* ((guest (wasmdown.component.runtime:make-comp-instance))
           (rct (wasmdown.component.runtime::make-ct*
                 :result (cons (wasmdown.component.runtime::make-ct*
                                :prim :u32)
                               (wasmdown.component.runtime::make-ct*
                                :prim :u32))))
           (cx (funcall cx-of guest)))
      (is (equal '(1 7) (wasmdown.component.runtime::%abi-lower
                         cx (cons :err 7) rct)))
      (is (equal '(0 3) (wasmdown.component.runtime::%abi-lower
                         cx (cons :ok 3) rct)))
      (let* ((mem (%pin-memory))
             (scx (funcall cx-of guest mem)))
        (wasmdown.component.runtime::%abi-store
         scx (cons :err 7) rct 16)
        (is (= 1 (wasmdown.component.runtime::%abi-load-int scx 16 1)))
        (is (= 7 (wasmdown.component.runtime::%abi-load-int scx 20 4)))))
    ;; CASE 5 — an own handle in a COMPOUND result: lowering the
    ;; abstract (:own-handle rt rep) adds EXACTLY ONE handle to the
    ;; lowering caller's table. A FRESH table has an empty free list,
    ;; so the slot is the fresh append; reuse of freed slots (the
    ;; shared Table policy §1131) is pinned by the plateau pin below.
    (let* ((guest (wasmdown.component.runtime:make-comp-instance))
           (cx (funcall cx-of guest))
           (ct (wasmdown.component.runtime::make-ct*
                :list (cons (wasmdown.component.runtime::make-ct*
                             :own 0)
                            1)))
           (n0 (length (funcall h* guest)))
           (flat (wasmdown.component.runtime::%abi-lower
                  cx (list (list :own-handle rt 42)) ct))
           (h (wasmdown.component.runtime::%handle-resource-at
               guest (first flat) rt)))
      (is (= 1 (length flat)))
      (is (= (1+ n0) (length (funcall h* guest))))
      (is (wasmdown.component.runtime::resource-handle-own-p h))
      (is (eql 42 (wasmdown.component.runtime::resource-handle-rep h))))
    ;; CASE 3c/B — the REAL sync wrapper of a (borrow $R) param whose
    ;; callee is ASYNC-TYPED and NOT the implementor: the arm lends
    ;; into a fresh borrow handle (a HOST callee's lend goes to the
    ;; dynamic caller's table) and the callee receives THAT index, not
    ;; the caller's raw source index. Delete the arm and the closure
    ;; sees the source index (passing-resources::1's 55-from-5).
    (let ((store (ct-defining)))
      (let* ((callee (wasmdown.component.runtime:make-comp-instance))
             (caller (wasmdown.component.runtime:make-comp-instance))
             (bct (wasmdown.component.runtime::make-ct* :borrow 0))
             (ft (wasmdown.component.runtime::make-ct*
                  :func (list (list (cons "self" bct))
                              (wasmdown.component.runtime::make-ct*
                               :prim :u32)
                              t)))
             (seen nil))
        (wasmdown.component.runtime::%push-type callee rt)
        (unwind-protect
             (progn
               ;; the resource ct was ALIAS-PROJECTED into the callee
               ;; (§2834's same-implementor shortcut must NOT apply)
               (setf (gethash (cons callee 0) store) :pin-source)
               (let ((live nil))
                 (wasmdown.component.runtime::%push-func
                caller
                (wasmdown.component.runtime:make-lifted-func
                 :type ft :inst callee
                 :host (lambda (args)
                         (setf seen (copy-list args)
                               ;; the borrow is LIVE for the provider's
                               ;; whole call; read it while it exists
                               live (wasmdown.component.runtime::%handle-resource-at
                                     wasmdown.component.runtime::*handle-caller-inst*
                                     (first args) rt))
                         7)))
               (let* ((src (wasmdown.component.runtime::%handle-add
                            caller rt 42 t))
                      (wrap (wasmdown.component.runtime::%lower-host-func
                             caller 0 nil))
                      (out (wasmdown.runtime:wasm-call wrap src))
                      (bi (first seen))
                      (free (gethash :handle-free
                                     (wasmdown.component.runtime:comp-instance-imports
                                      caller))))
                 (is (equal '(7) out))
                 ;; the callee got a NEW index, a NON-OWNING borrow
                 ;; record naming the source's rep (§2832)
                 (is (= 2 bi))
                 (is (and live
                          (not (wasmdown.component.runtime::resource-handle-own-p
                                live))))
                 (is (eql 42
                          (wasmdown.component.runtime::resource-handle-rep
                           live)))
                 ;; a HOST callee has no Task to own the borrow, so the
                 ;; call's exit RECLAIMED the temporary record: its slot
                 ;; is empty and the index is back on the shared free
                 ;; list (a later add re-uses it)
                 (is (null (aref (wasmdown.component.runtime::%inst-handles
                                  caller)
                                 (1- bi))))
                 (is (member bi free))
                 ;; the source index is untouched and still OWNS the
                 ;; resource
                 (is (wasmdown.component.runtime::resource-handle-own-p
                      (wasmdown.component.runtime::%handle-resource-at
                       caller src rt)))))
          (remhash (cons callee 0) store)))))))

(test component/indirect-async-payload-ownership-pin
  ;; CanonicalABI.md load (§2133-2169) + lift_async_value (§2438):
  ;; a stream/future payload read out of MEMORY is LIFTED — the
  ;; readable end leaves the loading instance's handles table, its
  ;; owner/index clear, and the value is the END record. Three things
  ;; the frozen catch-all could not give are pinned here: (a) no raw
  ;; index escapes a memory-loaded payload through a REAL host lower
  ;; (sync, with the tuple FORCED into memory past MAX_FLAT_PARAMS),
  ;; (b) the async RESULT lowering resolves a NESTED (:idx . n) in the
  ;; CALLEE's type space (the result context's :type-inst), and (c)
  ;; lift_async_value's guard set (stale / wrong class / wrong
  ;; element type / writable / non-idle / in a waitable set).
  (let ((u64 (wasmdown.component.runtime::make-ct* :prim :u64))
        (u8 (wasmdown.component.runtime::make-ct* :prim :u8))
        (u32 (wasmdown.component.runtime::make-ct* :prim :u32))
        (stream-read wasmdown.component.runtime::+event-stream-read+)
        (stream-write wasmdown.component.runtime::+event-stream-write+))
    ;; LABELS: RESIDENT-P calls H*, and flet bindings cannot see each
    ;; other (the flet shape compiled H* as an undefined function)
    (labels ((h* (inst) (wasmdown.component.runtime::%inst-handles inst))
             (mem32 (mem ptr)
               (loop for k below 4
                     sum (ash (aref (wasmdown.runtime:memory-instance-bytes
                                     mem)
                                    (+ ptr k))
                              (* 8 k))))
             (new-end (&key (event stream-read) (state :idle) (t-ct u8))
               (wasmdown.component.runtime::make-async-end
                :event-code event :state state :t-ct t-ct))
             (resident-p (end inst)
               (notany (lambda (x) (eq x end))
                       (coerce (h* inst) 'list))))
      ;; ---- (a) SYNC HOST LOWER, FORCED INDIRECT ----
      ;; 16 u64 params + option<stream<u8>> flatten to 18 slots, past
      ;; MAX_FLAT_PARAMS: the real canon-lower wrapper takes one [ptr]
      ;; and reads the whole tuple out of the caller's memory.
      (let* ((caller (wasmdown.component.runtime:make-comp-instance))
             (callee (wasmdown.component.runtime:make-comp-instance))
             (mem (%pin-memory))
             (sct (wasmdown.component.runtime::make-ct* :stream :u8))
             (sidx (wasmdown.component.runtime::%push-type callee sct))
             ;; the nested payload is a RAW ref: resolving it needs the
             ;; CALLEE's type space (the result/argument type lives
             ;; there, not in the caller's)
             (opt (wasmdown.component.runtime::make-ct*
                   :variant (vector (cons "none" nil)
                                    (cons "some" (cons :idx sidx)))))
             (oidx (wasmdown.component.runtime::%push-type callee opt))
             (params (append
                      (loop for i below 16
                            collect (cons (format nil "a~d" i) u64))
                      (list (cons "s" (cons :idx oidx)))))
             (ft (wasmdown.component.runtime::make-ct*
                  :func (list params (wasmdown.component.runtime::make-ct*
                                      :prim :u32)
                              nil)))
             (ts (loop for p in params collect (cdr p)))
             (tuple-width (wasmdown.component.runtime::make-ct*
                           :tuple (coerce ts 'vector)))
             (end (new-end))
             (seen nil))
        ;; a DIFFERENT type at the caller's index 0: if a nested ref
        ;; resolved in the CALLER's space it would read this u32
        (wasmdown.component.runtime::%push-type caller u32)
        (wasmdown.component.runtime::%push-core-memory caller mem)
        (wasmdown.component.runtime::%push-func
         caller
         (wasmdown.component.runtime:make-lifted-func
          :type ft :inst callee
          :host (lambda (args) (setf seen (copy-list args)) 7)))
        (let* ((scx (wasmdown.component.runtime::make-abi-cx
                     :inst caller :type-inst callee :memory mem))
               (n-callee (length (h* callee))))
          ;; the caller's compiled lower spills the tuple; the option's
          ;; stream end is LOWERED into the CALLER's table first
          (wasmdown.component.runtime::%abi-store
           scx (append (loop for i below 16 collect i)
                       (list (cons :some end)))
           tuple-width 0)
          (let ((idx (mem32 mem 132)))  ; 16*u64, then disc 1 + pad 3
            (is (eql idx (length (h* caller))))
            (is (eq end (aref (h* caller) (1- idx))))
            (let* ((canon (wasmdown.component:make-canonrec
                           :kind :lower :func 0
                           :opts (list (list :memory 0))))
                   (wrap (wasmdown.component.runtime::%lower-host-func
                          caller 0 canon)))
              (is (equal '(7) (wasmdown.runtime:wasm-call wrap 0)))
              ;; the host provider saw the END RECORD nested in the
              ;; option — never the raw table index
              (is (eq end (cdr (nth 16 seen))))
              (is (not (integerp (cdr (nth 16 seen)))))
              ;; ownership: the SOURCE slot is consumed and the end is
              ;; host-owned (not left resident anywhere)
              (is (null (aref (h* caller) (1- idx))))
              (is (null (wasmdown.component.runtime::async-end-owner end)))
              (is (null (wasmdown.component.runtime::async-end-index end)))
              ;; the PROVIDER's table never saw it
              (is (= n-callee (length (h* callee))))
              (is (resident-p end callee))))))
      ;; ---- (b) ASYNC RESULT: a NESTED option<stream<u8>> ----
      ;; The async lower's result is always spilled (MAX_FLAT_RESULTS
      ;; 0), so the result context stores the nested option in the
      ;; CALLER's memory. Its (:idx . n) payload must resolve in the
      ;; CALLEE's space (FT-INST = the result context's :type-inst).
      (let* ((caller (wasmdown.component.runtime:make-comp-instance))
             (callee (wasmdown.component.runtime:make-comp-instance))
             (mem (%pin-memory))
             (sct (wasmdown.component.runtime::make-ct* :stream :u8))
             (sidx (wasmdown.component.runtime::%push-type callee sct))
             (opt (wasmdown.component.runtime::make-ct*
                   :variant (vector (cons "none" nil)
                                    (cons "some" (cons :idx sidx)))))
             (oidx (wasmdown.component.runtime::%push-type callee opt))
             (ft (wasmdown.component.runtime::make-ct*
                  :func (list (list (cons "n" u64))
                              (cons :idx oidx)
                              t)))
             (end (new-end))
             (seen nil)
             ;; the provider's answer — mutated for the NONE case
             (reply (cons :some end)))
        ;; the caller's type index 0 is a DIFFERENT type: a ref that
        ;; resolved in the caller's space would store an integer here
        (wasmdown.component.runtime::%push-type caller u32)
        (wasmdown.component.runtime::%push-core-memory caller mem)
        (wasmdown.component.runtime::%push-func
         caller
         (wasmdown.component.runtime:make-lifted-func
          :type ft :inst callee
          :host (lambda (args)
                  (setf seen (copy-list args))
                  reply)))
        (let* ((canon (wasmdown.component:make-canonrec
                       :kind :lower :func 0
                       :opts (list (list :memory 0))))
               (wrap (wasmdown.component.runtime::%canon-lower-async-host-func
                      caller 0 canon))
               (n-callee (length (h* callee)))
               (bytes (wasmdown.runtime:memory-instance-bytes mem)))
          (is (equal '(2) (wasmdown.runtime:wasm-call wrap 5 16)))
          (is (equal '(5) seen))
          ;; discriminant 1 = "some" at 16, the payload at 20 (join-
          ;; aligned), and that index is the CALLER's new handle
          (is (= 1 (aref bytes 16)))
          (let ((idx (mem32 mem 20)))
            (is (plusp idx))
            (is (eq end (aref (h* caller) (1- idx))))
            (is (eql caller (wasmdown.component.runtime::async-end-owner end)))
            (is (eql idx (wasmdown.component.runtime::async-end-index end))))
          ;; the provider's table is untouched by the transfer
          (is (= n-callee (length (h* callee))))
          (is (resident-p end callee))
          ;; NONE ALLOCATES NOTHING: no handle on either side, the
          ;; discriminant is 0 and the payload slot stays untouched
          (let ((n-caller (length (h* caller))))
            (setf reply :none)
            (is (equal '(2) (wasmdown.runtime:wasm-call wrap 6 32)))
            (is (= 0 (aref bytes 32)))
            (is (zerop (mem32 mem 36)))
            (is (= n-caller (length (h* caller))))
            (is (= n-callee (length (h* callee)))))))
      ;; ---- (c) lift_async_value's GUARD SET, THROUGH load ----
      (let* ((guest (wasmdown.component.runtime:make-comp-instance))
             (mem (%pin-memory))
             (cx (wasmdown.component.runtime::make-abi-cx
                  :inst guest :type-inst guest :memory mem))
             (sct (wasmdown.component.runtime::make-ct* :stream :u8))
             (fct (wasmdown.component.runtime::make-ct* :future :u8))
             (bytes (wasmdown.runtime:memory-instance-bytes mem)))
        (flet ((seat (end)
                 (let ((idx (wasmdown.component.runtime::%handle-add-any
                             guest end)))
                   (loop for k below 4
                         do (setf (aref bytes k)
                                  (ldb (byte 8 (* 8 k)) idx)))
                   idx)))
          ;; stale: an index past every addable slot
          (assert-trap :generic
            (wasmdown.component.runtime::%abi-load cx sct 256))
          ;; wrong CLASS: a resource handle is not an end
          (let ((rt (wasmdown.component.runtime::make-ct* :resource nil)))
            (seat nil)
            (setf (aref (h* guest) (1- (length (h* guest))))
                  (wasmdown.component.runtime::make-resource-handle
                   rt 1 t))
            (assert-trap :generic
              (wasmdown.component.runtime::%abi-load cx sct 0)))
          ;; wrong async class: a STREAM end through a future<u8>
          (seat (new-end))
          (assert-trap :generic
            (wasmdown.component.runtime::%abi-load cx fct 0))
          ;; wrong element type: stream<u8> valtype, stream<u32> end
          (seat (new-end :t-ct u32))
          (assert-trap :generic
            (wasmdown.component.runtime::%abi-load cx sct 0))
          ;; WRITABLE end: not a readable end
          (seat (new-end :event stream-write))
          (assert-trap :generic
            (wasmdown.component.runtime::%abi-load cx sct 0))
          ;; NON-IDLE
          (seat (new-end :state :copying))
          (assert-trap :generic
            (wasmdown.component.runtime::%abi-load cx sct 0))
          ;; IN A WAITABLE SET
          (let* ((end (new-end))
                 (wset (wasmdown.component.runtime::make-async-waitable-set)))
            (seat end)
            (wasmdown.component.runtime::%waitable-join end wset)
            (assert-trap :generic
              (wasmdown.component.runtime::%abi-load cx sct 0))
            (wasmdown.component.runtime::%waitable-join end nil))
          ;; the guards never consumed the slot: the last (now
          ;; readable, idle, out-of-set) end still lifts
          (let ((end (new-end)))
            (seat end)
            (is (eq end (wasmdown.component.runtime::%abi-load cx sct 0)))))))))

(pin-test component/switched-target-exit-releases-pin
  ;; §474-484: the switch_to chain runs the target until it BLOCKS OR
  ;; RETURNS; its caller is released only at that boundary, and the
  ;; §242-273 exit post normally carries the release.
  ;;
  ;; THE CAPTURE (reentrance::20, the last open row): reproduced 2/30
  ;; in-lane (batch re20-master-D runs 11/15, both
  ;; `unexpected trap: deadlock detected: event loop cannot make
  ;; further progress`) and 4/4 under a repeated battery load. The
  ;; oracle state was the same every time:
  ;;   (:idx 1 :seq N :yield T :switch T :sw-tgt 2 :rc 0 :wc 0
  ;;    :pc 198 :sw-sample 1)
  ;;   target: (:spawn :done :pc 1 :rc 0 :wc 0 :post NIL
  ;;            :registered NIL)
  ;; i.e. the caller was parked in its yield-then-resume switch with
  ;; ZERO resume credit and its target ALREADY EXITED. The exit post
  ;; had no switcher to wake, and the caller's only two release terms
  ;; (resume-count, the yield target's park-count advance) can never
  ;; fire: the exit does not bump park-count. The rule the spec states
  ;; ("returns") was simply not among the release terms — the chain-end
  ;; test %SWITCH-CHAIN-ENDED-P already applies to the OUTER resumer.
  ;;
  ;; (a) drives the REAL builtin deterministically: a PENDING SPAWN
  ;; target whose thunk runs the PRODUCTION exit
  ;; (%TASK-EXIT-IMPLICIT-THREAD) inside the resume's spawn fire. The
  ;; resume installs POST-TO only AFTER the start fire, so the exit's
  ;; handoff post has no switcher to wake — the captured lost post by
  ;; construction. The caller must still return 0 at that chain end.
  ;; NEGATIVE CONTROL: with the chain-end term removed (the pre-fix
  ;; tree) the caller parks forever and this bounded wait fails.
  (%pin-with-store
    (let* ((inst (%pin-inst))
           (target (%pin-thread inst 2 '(1 0)))
           (caller (%pin-thread inst 1 '(1)))
           (caller-done (list nil))
           (caller-result (list nil))
           (caller-err (list nil)))
      (%pin-register inst target)
      (%pin-register inst caller)
      ;; a suspended PENDING SPAWN (§4590): the promote's readiness
      ;; accepts it, and the switch resume STARTS it
      (setf (wasmdown.component.runtime::async-thread-spawn-thunk target)
            (lambda ()
              ;; THE PRODUCTION EXIT, before the arm installs POST-TO
              (wasmdown.component.runtime::%task-exit-implicit-thread
               (wasmdown.component.runtime::async-thread-task target))))
      (let ((bc (sb-thread:make-thread
                 (%pin-in-store ()
                   (let ((wasmdown.component.runtime::*current-thread* caller)
                         (wasmdown.component.runtime::*current-task*
                           (wasmdown.component.runtime::async-thread-task
                            caller))
                         (wasmdown.component.runtime::*right-self* caller))
                     (unwind-protect
                          (handler-case
                              (progn
                                (setf (car caller-result)
                                      (wasmdown.runtime:wasm-call
                                       (%pin-builtin
                                        inst :thread-yield-then-promote)
                                       2)))
                            (error (c) (setf (car caller-err) c)))
                       (wasmdown.component.runtime::%right-release-on-exit
                        caller)
                       (setf (car caller-done) t))))
                 :name "pin-target-exit-caller")))
        (is (%pin-wait-for (lambda () (car caller-done))
                           :timeout 5
                           :what "the switch caller's release at its target's EXIT"))
        (is (null (car caller-err)))
        (is (equal '(0) (car caller-result)))
        (is (eq :done
                (wasmdown.component.runtime::async-thread-spawn-thunk
                 target)))
        (is (not (wasmdown.component.runtime::async-thread-switch-parked
                  caller)))
        (is (%pin-clean-p caller))
        (is (%pin-join bc)))))
  ;; (b) THE ROW, VERBATIM (component + assert_return), repeated: the
  ;; in-lane failure is a scheduling lottery, so one run is no pin.
  (let ((wast (%pin-slice "tests/component/async/reentrance.wast"
                          530 658)))
    (dotimes (i 20)
      (is (%pin-rows-pass-p
           wast (format nil "switched-target-exit-pin-~d.wast" i))))))

;;; ---- THE LOCK ORDER (the scheduler <-> thread <-> store-right cycle) ----
;;;
;;; The instrumented run exposed a 3-thread cycle between
;;; *SCHEDULER-MUTEX*, an async-thread's own mutex and *STORE-RIGHT*.
;;; Its cycle-closing edge is ONE site: %STORE-TEARDOWN called the
;;; exit path (%TASK-EXIT-IMPLICIT-THREAD -> ASYNC-THREAD-RESUME-ALL ->
;;; %WITHOUT-RIGHT -> %RIGHT-ACQUIRE) while holding the RECURSIVE
;;; scheduler mutex, i.e. SCHED -> RIGHT; %RIGHT-RELEASE-ON-EXIT /
;;; %THREAD-BIRTH / %REGISTER-THREAD-INDEX supply the reverse
;;; RIGHT -> SCHED edge. The two pins below are the two halves of the
;;; guarantee: one is EXACT (a source-level pin of the invariant, with
;;; a negative control proving the detector fires on the pre-fix
;;; shape), one EXERCISES the path end to end.

(defun %pin-file-text (rel)
  "The UTF-8 text of REL (a repo-relative source file)."
  (with-open-file (in rel :direction :input :external-format :utf-8)
    (let ((buf (make-string (file-length in))))
      (subseq buf 0 (read-sequence buf in)))))

(defun %pin-skip-string (text open)
  "Index just past the string literal opening at TEXT[OPEN]."
  (loop with n = (length text) with i = (1+ open)
        while (< i n)
        do (cond ((char= (char text i) #\\)
                  (incf i 2))
                 ((char= (char text i) #\")
                  (return (1+ i)))
                 (t (incf i)))
        finally (return n)))

(defun %pin-skip-block-comment (text open)
  "Index just past the #| ... |# comment opening at TEXT[OPEN]."
  (loop with n = (length text) with i = (+ open 2)
        while (< i n)
        do (if (and (char= (char text i) #\|)
                    (< (1+ i) n)
                    (char= (char text (1+ i)) #\#))
               (return (+ i 2))
               (incf i))
        finally (return n)))

(defun %pin-paren-close (text open)
  "Index of the ')' matching the '(' at TEXT[OPEN], or NIL. Skips
strings, ; comments, #| |# block comments and #\\ char literals, so a
docstring inside a body cannot spoof the match."
  (loop with n = (length text) with i = (1+ open) with depth = 1
        while (< i n)
        do (let ((c (char text i)))
             (cond
               ((char= c #\") (setf i (%pin-skip-string text i)))
               ((char= c #\;)
                (setf i (or (position #\Newline text :start i) n)))
               ((and (char= c #\#) (< (1+ i) n)
                     (char= (char text (1+ i)) #\|))
                (setf i (%pin-skip-block-comment text i)))
               ((and (char= c #\#) (< (1+ i) n)
                     (char= (char text (1+ i)) #\\))
                (incf i 3))             ; #\x (an escaped #\( is skipped)
               ((char= c #\() (incf depth) (incf i))
               ((char= c #\)) (decf depth) (incf i)
                (when (zerop depth) (return (1- i))))
               (t (incf i))))
        finally (return nil)))

(defun %pin-scheduler-spans (text)
  "Every span (START . END) of a (sb-thread:with-recursive-lock
(*scheduler-mutex*) ...) form in TEXT."
  (let ((out '()) (from 0))
    (loop for pos = (search "(sb-thread:with-recursive-lock" text
                            :start2 from)
          while pos
          do (when (search "(*scheduler-mutex*)" text :start2 pos
                           :end2 (min (length text) (+ pos 64)))
               (push (cons pos (or (%pin-paren-close text pos)
                                   (length text)))
                     out))
             (setf from (+ pos 2)))
    (nreverse out)))

(defun %pin-code-only (text)
  "TEXT with every comment and string literal blanked out (same length,
same offsets), so only CODE is pattern-matched."
  (let* ((n (length text))
         (out (make-string n :initial-element #\Space))
         (i 0))
    (loop while (< i n)
          do (let ((c (char text i)))
               (cond
                 ((char= c #\;) (setf i (or (position #\Newline text :start i) n)))
                 ((char= c #\") (setf i (%pin-skip-string text i)))
                 ((and (char= c #\#) (< (1+ i) n)
                       (char= (char text (1+ i)) #\|))
                  (setf i (%pin-skip-block-comment text i)))
                 ((and (char= c #\#) (< (1+ i) n)
                       (char= (char text (1+ i)) #\\))
                  (setf i (min n (+ i 3))))
                 (t (setf (char out i) c) (incf i)))))
    out))

(defun %pin-lock-order-violation-p (text)
  "Does ANY scheduler-mutex section of TEXT contain the source text of
a store-right grab, a resume-all, or the exit call that reaches one?
THE LOCK ORDER rule (b): those are RIGHT acquisitions, and a held
(recursive) scheduler frame would make them SCHED -> RIGHT."
  (let ((tokens '("(async-thread-resume-all" "(%without-right"
                  "(%right-acquire" "(%right-handoff-to"
                  "(%task-exit-implicit-thread")))
    (some (lambda (span)
            (let* ((body (subseq text (car span)
                                 (min (length text) (1+ (cdr span)))))
                   (code (%pin-code-only body)))
              (some (lambda (tk) (and (search tk code) t)) tokens)))
          (%pin-scheduler-spans text))))

(test component/lock-order-no-scheduler-held-across-store-right-pin
  ;; EXACT: the source-level invariant, so a revert of the two-phase
  ;; teardown is red even when no thread happens to hit the window.
  ;; THE DETECTOR IS NOT VACUOUS: both pre-fix shapes are flagged.
  (is (%pin-lock-order-violation-p
       (format nil "(defun bad () (sb-thread:with-recursive-lock (*scheduler-mutex*) ~
                   (dolist (th ths) (%task-exit-implicit-thread th))))")))
  (is (%pin-lock-order-violation-p
       (format nil "(defun bad () (sb-thread:with-recursive-lock (*scheduler-mutex*) ~
                   (%without-right s (lambda () nil))))")))
  ;; a LEGAL scheduler section (a leaf/peer section only) is not flagged
  (is (not (%pin-lock-order-violation-p
            (format nil "(defun ok () (sb-thread:with-recursive-lock (*scheduler-mutex*) ~
                        (async-thread-nudge th)))"))))
  ;; a docstring mentioning the tokens inside the span does not spoof it
  (is (not (%pin-lock-order-violation-p
            (format nil "(defun ok () (sb-thread:with-recursive-lock (*scheduler-mutex*) ~
                        (x) ; (%without-right is documented here~%))"))))
  (let ((text (%pin-file-text "src/component-runtime.lisp")))
    ;; positive controls: real text, and the exit call really is there
    (is (search "(defun %store-teardown" text))
    (is (search "(%task-exit-implicit-thread" text))
    (is (search "(defun %without-right" text))
    ;; ANCHOR ROT IS RED, NOT BLIND: the detector must actually find
    ;; scheduler sections in the real source. Renaming the lock variable
    ;; (or reformatting the section) without updating the anchor would
    ;; otherwise turn this control into a vacuous pass.
    (is (plusp (length (%pin-scheduler-spans text))))
    (is (not (%pin-lock-order-violation-p text)))))

(pin-test component/store-teardown-runs-with-right-held-pin
  ;; EXERCISE: the previously cyclic path, end to end. A thread that
  ;; OWNS the store right (and has *RIGHT-SELF* bound, the shape the
  ;; incident had) runs the REAL %STORE-TEARDOWN with async exclusive
  ;; tasks registered (so the exit path reaches resume-all ->
  ;; %without-right -> %right-acquire); a competitor holds the right
  ;; and then takes the scheduler mutex (the RIGHT -> SCHED edge that
  ;; closes the cycle). On the fixed tree the teardown holds no
  ;; scheduler frame while it re-acquires the right, so the competitor
  ;; gets the scheduler mutex; on the nested shape the teardown would
  ;; block on the right while the competitor blocks on the scheduler.
  ;; The competitor's scheduler grab is BOUNDED: the broken shape
  ;; reports a failed grab instead of wedging the suite (no sleep).
  (let* ((inst (wasmdown.component.runtime:make-comp-instance))
         (ft (wasmdown.component:make-ct
              :kind :functype :payload (list nil nil t)))
         (threads
           (loop repeat 48
                 collect (let* ((task
                                  (wasmdown.component.runtime::make-async-task
                                   :inst inst :ft ft :seq '(1)))
                                (th
                                  (wasmdown.component.runtime::make-async-thread
                                   :task task)))
                           (setf (wasmdown.component.runtime::async-task-implicit-thread
                                  task)
                                 th)
                           th)))
         (old-threads (wasmdown.component.runtime::%store-threads))
         (old-ticket (wasmdown.component.runtime::%store-right-ticket))
         (old-frames (right-handoffs))
         (old-owner (wasmdown.component.runtime::%store-right-owner))
         (self (wasmdown.component.runtime::make-async-thread
                :task (wasmdown.component.runtime::make-async-task
                       :seq '(0))))
         (go (sb-thread:make-semaphore))
         (armed (sb-thread:make-semaphore))
         (got-right nil)
         (sched-ok nil)
         (teardown-finished nil))
    (unwind-protect
         (progn
           (setf (wasmdown.component.runtime::%store-threads) threads
                 (wasmdown.component.runtime::%store-right-ticket) nil
                 (right-handoffs) nil
                 (wasmdown.component.runtime::%store-right-owner) nil)
           (let* ((competitor
                    (sb-thread:make-thread
                     (%pin-in-store ()
                       (sb-thread:wait-on-semaphore go)
                       ;; ARM, then contend: the signal is posted one
                       ;; pair of instructions before the mutex wait,
                       ;; so the teardown enters its (several hundred
                       ;; writes long) phase 1 with this thread already
                       ;; queued on the right.
                       (sb-thread:signal-semaphore armed)
                       (sb-thread:grab-mutex
                        wasmdown.component.runtime::*store-right*)
                       (setf got-right t)
                       (setf sched-ok
                             (sb-thread:grab-mutex
                              wasmdown.component.runtime::*scheduler-mutex*
                              :timeout 2.0))
                       (when sched-ok
                         (sb-thread:release-mutex
                          wasmdown.component.runtime::*scheduler-mutex*))
                       (sb-thread:release-mutex
                        wasmdown.component.runtime::*store-right*))
                     :name "pin-lock-order-competitor"))
                  (teardown
                    (sb-thread:make-thread
                     (%pin-in-store ()
                       (let ((wasmdown.component.runtime::*right-self* self))
                         (sb-thread:grab-mutex
                          wasmdown.component.runtime::*store-right*)
                         (wasmdown.component.runtime::%right-take self)
                         ;; the competitor is now contending for the
                         ;; right (see ARMED above)
                         (sb-thread:signal-semaphore go)
                         (sb-thread:wait-on-semaphore armed)
                         (wasmdown.component.runtime::%store-teardown)
                         (when (sb-thread:holding-mutex-p
                                wasmdown.component.runtime::*store-right*)
                           (sb-thread:release-mutex
                            wasmdown.component.runtime::*store-right*))
                         (setf teardown-finished t)))
                     :name "pin-lock-order-teardown")))
             (is (not (eq :pin-timeout
                          (sb-thread:join-thread teardown :timeout 10.0
                                                 :default :pin-timeout))))
             (is (not (eq :pin-timeout
                          (sb-thread:join-thread competitor :timeout 10.0
                                                 :default :pin-timeout))))
             (is teardown-finished)
             (is got-right)
             ;; THE ORDER ASSERTION: the competitor held the right and
             ;; still took the scheduler mutex — the teardown did not
             ;; hold it across its right re-acquisition.
             (is sched-ok)))
      (when (sb-thread:holding-mutex-p
             wasmdown.component.runtime::*store-right*)
        (sb-thread:release-mutex
         wasmdown.component.runtime::*store-right*))
      (setf (wasmdown.component.runtime::%store-threads) old-threads
            (wasmdown.component.runtime::%store-right-ticket) old-ticket
            (right-handoffs) old-frames
            (wasmdown.component.runtime::%store-right-owner) old-owner)
      (dolist (th threads)
        (setf (wasmdown.component.runtime::async-thread-abandoned th) t
              (wasmdown.component.runtime::async-thread-spawn-thunk th)
              :done)))))

;;; ---- THE NESTED SYNC LOWER / STORE-RIGHT PIN (sync-streams::1) ----
;;; THE DECODE: a SYNC lower of an ASYNC-typed callee returns to its
;;; caller once the callee has RESOLVED and the caller can take the
;;; store right — and the caller can take it only at the callee's
;;; PARK or EXIT (the right's turn reservation forbids a mid-turn
;;; grab). The caller's turn then CONTINUES: it must KEEP the right
;;; until its own next park/exit. The pre-fix cleanup released it at
;;; the lower's return, so a GRANTED sibling could take a turn
;;; inside the caller's turn. In sync-streams.wast that sibling is
;;; $C.get's post-task.return continuation: it returned from its
;;; sync write, EXITed, and released $C's exclusive lock BEFORE the
;;; caller's next async lower looked at the backpressure gate, so
;;; the lower packed STARTED(1) where the fixture pins STARTING(0)
;;; and the guest's `unreachable' fired.
;;; THE CAPTURE (in-memory, printed once at the script boundary):
;;; the failing pair read `TRET (912 1) ... COPY-PARK 3 ...
;;; COPY-WAKE 3 65 ... DROP 3 :DONE ... TEXIT (912 1) :RESOLVED T`
;;; then `PACK 1 1 NIL T NIL` (packed STARTED, started-at-flip T)
;;; and `GTRAP (912) "unreachable executed"`.
;;; THE RULE FIX: %canon-lift-async keeps the right for a GUEST
;;; caller (a driver caller still releases: it leaves the store).
;;; NEGATIVE CONTROL: with the keep-right test forced to NIL the
;;; crafted fixture below traps 10/10 runs; with the rule it passes.
;;; THE PAIR PROBE that reproduced the row (predecessor ->
;;; sync-streams in one process, the historical cross-file store
;;; coupling): pre-fix 6 failing sync-streams::1 rows / 500
;;; iterations (3/100 after sync-barges-in, 2/100 after empty-wait,
;;; 1/200 in the instrumented capture run; 0/100 after
;;; switch-to-ready-callback); post-fix 0 / 1300 (1000
;;; sync-barges-in, 300 empty-wait).
(defparameter *ss1-nested-sync-lower-fixture*
  ";; A deterministic discriminator for the sync-lower / store-right rule
;; seen in sync-streams.wast: after a SYNC lower of an ASYNC-typed
;; callee returns, the CALLER's turn is not over, so it must still hold
;; the store right; otherwise the callee's post-task.return continuation
;; runs concurrently and can release the instance's exclusive lock
;; before the caller's next async lower looks at the backpressure gate.
(component
  (component $C
    (core module $Memory (memory (export \"mem\") 1))
    (core instance $memory (instantiate $Memory))
    (core module $CM
      (import \"\" \"mem\" (memory 1))
      (import \"\" \"task.return0\" (func $task.return0))
      (import \"\" \"task.return1\" (func $task.return1 (param i32)))
      (import \"\" \"stream.new\" (func $stream.new (result i64)))
      (import \"\" \"stream.write\" (func $stream.write (param i32 i32 i32) (result i32)))
      (import \"\" \"stream.drop-writable\" (func $stream.drop-writable (param i32)))
      (func (export \"get\") (result i32)
        (local $ret i32) (local $ret64 i64) (local $tx i32) (local $rx i32)
        (local.set $ret64 (call $stream.new))
        (local.set $rx (i32.wrap_i64 (local.get $ret64)))
        (local.set $tx (i32.wrap_i64 (i64.shr_u (local.get $ret64) (i64.const 32))))
        (call $task.return1 (local.get $rx))
        (i32.store (i32.const 16) (i32.const 0x01234567))
        (local.set $ret (call $stream.write (local.get $tx) (i32.const 16) (i32.const 4)))
        (if (i32.ne (i32.const 0x41 (; DROPPED=1 | (4<<4) ;)) (local.get $ret))
          (then unreachable))
        (call $stream.drop-writable (local.get $tx))
        (return (i32.const 0 (; EXIT ;))))
      (func (export \"get_cb\") (param i32 i32 i32) (result i32) unreachable)
      (func (export \"ping\") (result i32)
        (return (i32.const 0 (; EXIT ;))))
      (func (export \"ping_cb\") (param i32 i32 i32) (result i32) unreachable)
    )
    (type $ST (stream u8))
    (canon task.return (memory (core memory $memory \"mem\")) (core func $task.return0))
    (canon task.return (result $ST) (memory (core memory $memory \"mem\")) (core func $task.return1))
    (canon stream.new $ST (core func $stream.new))
    (canon stream.write $ST (memory (core memory $memory \"mem\")) (core func $stream.write))
    (canon stream.drop-writable $ST (core func $stream.drop-writable))
    (core instance $cm (instantiate $CM (with \"\" (instance
      (export \"mem\" (memory $memory \"mem\"))
      (export \"task.return0\" (func $task.return0))
      (export \"task.return1\" (func $task.return1))
      (export \"stream.new\" (func $stream.new))
      (export \"stream.write\" (func $stream.write))
      (export \"stream.drop-writable\" (func $stream.drop-writable))
    ))))
    (func (export \"get\") async (result (stream u8)) (canon lift
      (core func $cm \"get\")
      async (memory (core memory $memory \"mem\")) (callback (core func $cm \"get_cb\"))
    ))
    (func (export \"ping\") async (canon lift
      (core func $cm \"ping\")
      async (memory (core memory $memory \"mem\")) (callback (core func $cm \"ping_cb\"))
    ))
  )
  (component $D
    (import \"get\" (func $get async (result (stream u8))))
    (import \"ping\" (func $ping async))
    (import \"spin\" (func $spin async))

    (core module $Memory (memory (export \"mem\") 1))
    (core instance $memory (instantiate $Memory))
    (core module $DM
      (import \"\" \"mem\" (memory 1))
      (import \"\" \"stream.read\" (func $stream.read (param i32 i32 i32) (result i32)))
      (import \"\" \"stream.drop-readable\" (func $stream.drop-readable (param i32)))
      (import \"\" \"get\" (func $get (result i32)))
      (import \"\" \"ping\" (func $ping (result i32)))
      (import \"\" \"spin\" (func $spin (result i32)))
      (func (export \"run\") (result i32)
        (local $ret i32) (local $rx i32) (local $cnt i32)
        ;; a ready sibling in the STORE (spawned FIRST, so it is
        ;; already yield-parked): its yield park makes the driver
        ;; keep spending ticks while the caller runs.
        (drop (call $spin))
        ;; $rx = $C.get() -- the sync lower of an ASYNC-typed callee
        (local.set $rx (call $get))
        ;; the callee's post-task.return write has staged: this is the
        ;; rendezvous and it must complete without blocking
        (local.set $ret (call $stream.read (local.get $rx) (i32.const 20) (i32.const 4)))
        (if (i32.ne (i32.const 0x40 (; COMPLETED=0 | (4<<4) ;)) (local.get $ret))
          (then unreachable))
        (if (i32.ne (i32.const 0x01234567) (i32.load (i32.const 20)))
          (then unreachable))
        (call $stream.drop-readable (local.get $rx))
        ;; a long caller-side window: if the caller did NOT keep the
        ;; right, $C.get's parked continuation runs here, returns from
        ;; its sync write and EXITs, releasing $C's exclusive lock.
        (local.set $cnt (i32.const 200000))
        (loop $L
          (local.set $cnt (i32.sub (local.get $cnt) (i32.const 1)))
          (br_if $L (local.get $cnt)))
        ;; the async lower of $C.ping must still see $C's exclusive
        ;; lock held: STARTING(0). STARTED(1) means $C.get already ran
        ;; the continuation past the caller's turn.
        (local.set $ret (call $ping))
        (if (i32.ne (i32.const 0 (; STARTING ;)) (i32.and (local.get $ret) (i32.const 0xf)))
          (then unreachable))
        (i32.const 42)
      )
    )
    (type $ST (stream u8))
    (canon stream.read $ST (memory (core memory $memory \"mem\")) (core func $stream.read))
    (canon stream.drop-readable $ST (core func $stream.drop-readable))
    (canon lower (func $get) (core func $get'))
    (canon lower (func $ping) async (memory (core memory $memory \"mem\")) (core func $ping'))
    (canon lower (func $spin) async (core func $spin'))
    (core instance $dm (instantiate $DM (with \"\" (instance
      (export \"mem\" (memory $memory \"mem\"))
      (export \"stream.read\" (func $stream.read))
      (export \"stream.drop-readable\" (func $stream.drop-readable))
      (export \"get\" (func $get'))
      (export \"ping\" (func $ping'))
      (export \"spin\" (func $spin'))
    ))))
    (func (export \"run\") async (result u32) (canon lift (core func $dm \"run\")))
  )

  (component $Y
    (core module $YM
      (func (export \"spin\") (result i32) (i32.const 1 (; YIELD ;)))
      (func (export \"spin_cb\") (param i32 i32 i32) (result i32)
        (i32.const 1 (; YIELD ;))))
    (core instance $ym (instantiate $YM))
    (func (export \"spin\") async (canon lift
      (core func $ym \"spin\")
      async (callback (core func $ym \"spin_cb\"))
    ))
  )
  (instance $c (instantiate $C))
  (instance $y (instantiate $Y))
  (instance $d (instantiate $D
    (with \"get\" (func $c \"get\"))
    (with \"ping\" (func $c \"ping\"))
    (with \"spin\" (func $y \"spin\"))
  ))
  (func (export \"run\") (alias export $d \"run\"))
)
(assert_return (invoke \"run\") (u32.const 42))
")

(pin-test component/nested-sync-lower-keeps-guest-right-pin
  ;; (1) THE RULE at the real entry point: after %canon-lift-async
  ;; (the sync lower of an ASYNC-typed callee) returns, a GUEST
  ;; caller still holds the store right and the ticket names it; a
  ;; DRIVER caller still releases (it is leaving the store). The
  ;; pre-fix shape fails the first pair (the negative control).
  (%pin-with-store
    (let* ((ci (%exec-comp
                "(component
                   (core module $M (func (export \"f\")))
                   (core instance $m (instantiate $M))
                   (func (export \"f\") async (canon lift (core func $m \"f\"))))"))
           (f (wasmdown.component.runtime:runtime-export-value ci "f")))
      ;; THE GUEST CALLER
      (let* ((task (wasmdown.component.runtime::make-async-task
                    :inst ci :seq '(1)))
             (th (wasmdown.component.runtime::make-async-thread
                  :task task :index 1)))
        (setf (wasmdown.component.runtime::async-task-implicit-thread task) th)
        (let ((wasmdown.component.runtime::*current-thread* th)
              (wasmdown.component.runtime::*current-task* task)
              (wasmdown.component.runtime::*right-self* th))
          (wasmdown.component.runtime::%right-acquire th)
          (unwind-protect
               (progn
                 (wasmdown.component.runtime:lifted-func-call f)
                 (is (sb-thread:holding-mutex-p
                      wasmdown.component.runtime::*store-right*))
                 (is (eq (wasmdown.component.runtime::%store-right-ticket) th)))
            (when (sb-thread:holding-mutex-p
                   wasmdown.component.runtime::*store-right*)
              (sb-thread:release-mutex
               wasmdown.component.runtime::*store-right*)
              (setf (wasmdown.component.runtime::%store-right-ticket) nil)))))
      ;; THE DRIVER CALLER
      (let ((wasmdown.component.runtime::*current-thread* nil)
            (wasmdown.component.runtime::*current-task* nil)
            (wasmdown.component.runtime::*right-self* nil))
        (let ((drv (wasmdown.component.runtime::%driver-self)))
          (wasmdown.component.runtime::%right-acquire drv)
          (unwind-protect
               (progn
                 (wasmdown.component.runtime:lifted-func-call f)
                 (is (not (sb-thread:holding-mutex-p
                           wasmdown.component.runtime::*store-right*))))
            (when (sb-thread:holding-mutex-p
                   wasmdown.component.runtime::*store-right*)
              (sb-thread:release-mutex
               wasmdown.component.runtime::*store-right*)))))))
  ;; (2) THE OBSERVED SHAPE through guest code: a granted sibling
  ;; runs inside the caller's turn only if the caller dropped the
  ;; right; the fixture then sees the callee started before its
  ;; next async lower (STARTED) and traps. With the rule: STARTING,
  ;; run returns 42. (Negative control: keep-right NIL -> 10/10
  ;; trap; keep-right on -> 10/10 pass.)
  (dotimes (i 5)
    (is (%pin-rows-pass-p *ss1-nested-sync-lower-fixture*
                          "nested-sync-lower-pin.wast")))
  ;; (3) THE FIXTURE ROWS VERBATIM (the component + its one
  ;; assert_return from the vendored sync-streams.wast).
  (dotimes (i 5)
    (is (%pin-rows-pass-p
         (%pin-slice "tests/component/async/sync-streams.wast" 1 208)
         "sync-streams-pin.wast"))))

;;; ---------------------------------------------------------------
;;; §9.3 — RESOURCE METADATA AND TEMPORARY-BORROW RECLAMATION.
;;;
;;; Two lifetime rules, measured by COUNTS only (no heap timing):
;;;
;;;  (1) METADATA IS THE STORE'S. The resource destructor table, the
;;;      alias-provenance records and the component closures key on
;;;      (and retain) the graph's instances; one store owns exactly
;;;      one instantiation graph, so %STORE-TEARDOWN retires them
;;;      with the graph. A LIVE store's records are untouched by a
;;;      single handle drop — two handles of one type both release
;;;      through the same registered dtor, and tearing down another
;;;      store cannot remove them.
;;;
;;;  (2) A HOST-TEMPORARY BORROW IS REMOVED ONCE. A lowering stages
;;;      the borrow handle it adds for an async-typed callee; a guest
;;;      callee's Task adopts it (%ADOPT-BORROW-HANDLES), but a HOST
;;;      callee has no Task, so the call's exit removes it — on the
;;;      immediate return and on a trap, and for a HOST-PENDING only
;;;      at its TERMINAL outcome (a cancel REQUEST is not terminal —
;;;      §host-pending's state doc). The SOURCE handle's num_lends
;;;      ledger stays the subtask's, cleared by
;;;      %SUBTASK-DELIVER-RESOLVE, never by this cleanup.
;;;
;;; The resource path also uses the shared Table free list (§1131), so
;;; the handle vector and free list plateau across new/drop cycles.

;; $C defines a resource type with a real core destructor and exports
;; new/drop; the root aliases the exported type, so the store records
;; both the destructor and an alias-provenance row.
(defparameter *pin-dtor-component-src*
  "(component
     (component $C
       (core module $M
         (memory (export \"mem\") 1)
         (func (export \"dtor\") (param i32)
           (i32.store (i32.const 0)
             (i32.add (i32.load (i32.const 0)) (i32.const 1)))))
       (core instance $m (instantiate $M))
       (type $R' (resource (rep i32) (dtor (core func $m \"dtor\"))))
       (export $R \"R\" (type $R'))
       (canon resource.new $R' (core func $new))
       (canon resource.drop $R' (core func $drop))
       (func (export \"new\") (param \"rep\" u32) (result (own $R))
         (canon lift (core func $new)))
       (func (export \"drop\") (param \"self\" (own $R))
         (canon lift (core func $drop))))
     (instance $c (instantiate $C))
     (alias export $c \"R\" (type $R)))")

(defun %pin-dtor-component ()
  (let ((comp (wasmdown.component.text::parse-component-text
               *pin-dtor-component-src*)))
    (is (wasmdown.component:validate-component comp))
    comp))

(defun %pin-dtor-graph (comp store)
  "Instantiate COMP into STORE; return (values ROOT CALLEE NEW DROP)
where CALLEE is the nested $C instance and NEW/DROP its lifted
exports."
  (let* ((root (wasmdown.component.runtime:instantiate-component
                comp :store store))
         (callee (aref (wasmdown.component.runtime::comp-instance-instances
                        root) 0)))
    (values root callee
            (cdr (gethash "new"
                          (wasmdown.component.runtime::comp-instance-exports
                           callee)))
            (cdr (gethash "drop"
                          (wasmdown.component.runtime::comp-instance-exports
                           callee))))))

(defun %pin-dtor-count (callee)
  "The value $C's dtor core func wrote into its own memory (a count of
dtor runs, read without any heap instrumentation)."
  (let* ((core (aref (wasmdown.component.runtime::comp-instance-core-instances
                      callee)
                     0))
         (mem (wasmdown.runtime:export-value core "mem"))
         (bytes (wasmdown.runtime:memory-instance-bytes mem)))
    (loop for k below 4 sum (ash (aref bytes k) (* 8 k)))))

(defun %pin-store-metadata (store)
  "The store's graph-metadata entry counts (dtors provenance closures)."
  (list (hash-table-count
         (wasmdown.component.runtime::component-store-resource-dtor-table
          store))
        (hash-table-count
         (wasmdown.component.runtime::component-store-ct-defining-inst store))
        (hash-table-count
         (wasmdown.component.runtime::component-store-comp-closure store))))

(defun %pin-live-resource-counts (inst)
  "The number of LIVE owning and borrowed RESOURCE records in INST's
table (the table also holds waitables/subtasks)."
  (let ((own 0) (borrow 0))
    (loop for h across (wasmdown.component.runtime::%inst-handles inst)
          when (wasmdown.component.runtime::resource-handle-p h)
            do (if (wasmdown.component.runtime::resource-handle-own-p h)
                   (incf own)
                   (incf borrow)))
    (values own borrow)))

(defun %pin-free-slot-count (inst)
  (length (gethash :handle-free
                   (wasmdown.component.runtime:comp-instance-imports inst))))

(pin-test component/store-metadata-retires-with-store-pin
  ;; (1) A WARM-UP GRAPH, then repeated instantiate->new->drop->
  ;; release-graph cycles: the live counts are CONSTANT (the graph
  ;; shape, never the iteration), the handle vector and free list
  ;; plateau (the resource path reuses §1131's free list), and the
  ;; store-retirement clears every metadata entry. The SECOND live
  ;; graph inside the cycle proves a drop does not remove the type's
  ;; dtor while another handle of the same type is live.
  (let ((comp (%pin-dtor-component)))
    (flet ((cycle ()
             (let* ((store (%current-store))
                    (base (list 1 2 0)))
               (multiple-value-bind (root callee new drop)
                   (%pin-dtor-graph comp store)
                 (declare (ignore root))
                 (is (equal base (%pin-store-metadata store)))
                 ;; TWO owning handles of the SAME type, allocated
                 ;; through new (which consumes and frees its own
                 ;; core-side handle) — the free list is reused, so
                 ;; the vector is two slots wide, never wider
                 (let* ((v1 (wasmdown.component.runtime:lifted-func-call
                             new 5))
                        (rt (second v1))
                        (h1 (wasmdown.component.runtime::%handle-add
                             callee rt (third v1) t))
                        (v2 (wasmdown.component.runtime:lifted-func-call
                             new 6))
                        (h2 (wasmdown.component.runtime::%handle-add
                             callee rt (third v2) t))
                        (capacity (length
                                   (wasmdown.component.runtime::%inst-handles
                                    callee))))
                   (is (eql 1 h1))
                   (is (eql 2 h2))
                   (is (= 2 capacity))
                   (is (equal '(2 0)
                              (multiple-value-list
                               (%pin-live-resource-counts callee))))
                   ;; ONE drop: the dtor ran, the type's record is
                   ;; STILL registered (the second handle is live),
                   ;; and the provenance is untouched
                   (wasmdown.component.runtime:lifted-func-call drop h1)
                   (is (= 1 (%pin-dtor-count callee)))
                   (is (equal base (%pin-store-metadata store)))
                   (is (equal '(1 0)
                              (multiple-value-list
                               (%pin-live-resource-counts callee))))
                   (is (= 1 (%pin-free-slot-count callee)))
                   ;; the second drop: the SAME registered dtor runs
                   (wasmdown.component.runtime:lifted-func-call drop h2)
                   (is (= 2 (%pin-dtor-count callee)))
                   (is (equal '(0 0)
                              (multiple-value-list
                               (%pin-live-resource-counts callee))))
                   (is (= 2 (%pin-free-slot-count callee)))
                   ;; repeated new/drop cycles in the LIVE graph do
                   ;; not grow capacity or the free list
                   (dotimes (i 4)
                     (let* ((v (wasmdown.component.runtime:lifted-func-call
                                new (+ 10 i)))
                            (h (wasmdown.component.runtime::%handle-add
                                callee (second v) (third v) t)))
                       (wasmdown.component.runtime:lifted-func-call drop h))
                     (is (= capacity
                            (length (wasmdown.component.runtime::%inst-handles
                                     callee))))
                     (is (= 2 (%pin-free-slot-count callee))))
                   (is (= 6 (%pin-dtor-count callee)))
                   ;; release-graph: every entry is retired
                   (let ((live (%pin-store-metadata store)))
                     (wasmdown.component.runtime:store-teardown store)
                     (values live (%pin-store-metadata store))))))))
      (multiple-value-bind (warmup after-warmup) (cycle)
        (is (equal '(1 2 0) warmup))
        (is (equal '(0 0 0) after-warmup))
        (dotimes (i 4)
          (multiple-value-bind (live after) (cycle)
            (is (equal warmup live))
            (is (equal '(0 0 0) after))))))))

(pin-test component/resource-metadata-live-graph-isolation-pin
  ;; TWO LIVE graphs in TWO stores: dropping a handle in A runs A's
  ;; destructor and leaves both stores' metadata intact; tearing down
  ;; B cannot strip A's provenance — A's SAME dtor still runs on its
  ;; next drop. Each store retires only its own records.
  (let* ((comp (%pin-dtor-component))
         (store-a (wasmdown.component.runtime:make-component-store))
         (store-b (wasmdown.component.runtime:make-component-store))
         (md (list 1 2 0)))
    (multiple-value-bind (ra ca na da) (%pin-dtor-graph comp store-a)
      (declare (ignore ra))
      (multiple-value-bind (rb cb nb db) (%pin-dtor-graph comp store-b)
        (declare (ignore rb nb db))
        (is (equal md (%pin-store-metadata store-a)))
        (is (equal md (%pin-store-metadata store-b)))
        (flet ((drop-one (callee new drop)
                 (let* ((v (wasmdown.component.runtime:lifted-func-call new 7))
                        (rt (second v))
                        (h (wasmdown.component.runtime::%handle-add
                            callee rt (third v) t)))
                   (wasmdown.component.runtime:lifted-func-call drop h))))
          ;; A's drop: A's dtor count rises, B's graph is untouched
          (drop-one ca na da)
          (is (= 1 (%pin-dtor-count ca)))
          (is (zerop (%pin-dtor-count cb)))
          (is (equal md (%pin-store-metadata store-a)))
          (is (equal md (%pin-store-metadata store-b)))
          ;; retiring B leaves A's records and its dtor resolution
          (wasmdown.component.runtime:store-teardown store-b)
          (is (equal '(0 0 0) (%pin-store-metadata store-b)))
          (is (equal md (%pin-store-metadata store-a)))
          (drop-one ca na da)
          (is (= 2 (%pin-dtor-count ca)))
          (is (zerop (%pin-dtor-count cb)))
          (is (equal md (%pin-store-metadata store-a)))
          (is (equal '(0 0 0) (%pin-store-metadata store-b))))
        (wasmdown.component.runtime:store-teardown store-a)
        (is (equal '(0 0 0) (%pin-store-metadata store-a)))))))

;;; ---- host-temporary borrows ----

(defun %pin-host-borrow-setup (store provider &optional result)
  "CALLER holds a host lifted func of an ASYNC-typed functype taking
(borrow $R); CALLEE declares $R as type 0 AND the alias-provenance
record makes it ALIAS-PROJECTED, so §2834's same-implementor shortcut
must not apply and lower_borrow really creates a borrow handle. The
provider host closure is PROVIDER (args -> value | host-pending)."
  (let* ((caller (wasmdown.component.runtime:make-comp-instance))
         (callee (wasmdown.component.runtime:make-comp-instance))
         (rt (wasmdown.component.runtime::make-ct* :resource nil))
         (bct (wasmdown.component.runtime::make-ct* :borrow 0))
         (ft (wasmdown.component.runtime::make-ct*
              :func (list (list (cons "self" bct)) result t))))
    (wasmdown.component.runtime::%push-type callee rt)
    (wasmdown.component.runtime::%push-type caller rt)
    (setf (gethash (cons callee 0)
                   (wasmdown.component.runtime::component-store-ct-defining-inst
                    store))
          :pin-source)
    (wasmdown.component.runtime::%push-func
     caller
     (wasmdown.component.runtime:make-lifted-func
      :type ft :inst callee :host provider))
    (values caller callee rt)))

(defun %pin-borrows-of (inst rt)
  (let ((n 0))
    (loop for h across (wasmdown.component.runtime::%inst-handles inst)
          when (and (wasmdown.component.runtime::resource-handle-p h)
                    (eql rt (wasmdown.component.runtime::resource-handle-rt
                             h))
                    (not (wasmdown.component.runtime::resource-handle-own-p
                          h)))
            do (incf n))
    n))

(defun %pin-any-borrow-count (inst)
  "Every live NON-OWNING resource record in INST's table — the shape a
provider can check without closing over the resource identity."
  (let ((n 0))
    (loop for h across (wasmdown.component.runtime::%inst-handles inst)
          when (and (wasmdown.component.runtime::resource-handle-p h)
                    (not (wasmdown.component.runtime::resource-handle-own-p
                          h)))
            do (incf n))
    n))

(defun %pin-own-handle (inst rt rep)
  (wasmdown.component.runtime::%handle-add inst rt rep t))

(defun %pin-source-lends (inst rt)
  (wasmdown.component.runtime::resource-handle-num-lends
   (wasmdown.component.runtime::%handle-resource-at inst 1 rt)))

(pin-test component/host-borrow-reclaim-pin
  ;; (2) The temporary BORROW handle of a lowered call to a HOST
  ;; callee: no Task adopts it, so the call's exit removes it exactly
  ;; once. The provider still sees a live, non-owning handle during
  ;; the call; the SOURCE handle is untouched.
  (let ((comp-store (%current-store)))
    ;; (a) SYNC lower, immediate value
    (multiple-value-bind (caller callee rt)
        (%pin-host-borrow-setup
         comp-store
         (lambda (args)
           (declare (ignore args))
           (is (= 1 (%pin-any-borrow-count
                     wasmdown.component.runtime::*handle-caller-inst*)))
           7)
         (wasmdown.component.runtime::make-ct* :prim :u32))
      (declare (ignore callee))
      (let* ((src (%pin-own-handle caller rt 42))
             (wrap (wasmdown.component.runtime::%lower-host-func
                    caller 0 nil))
             (out (wasmdown.runtime:wasm-call wrap src)))
        (is (equal '(7) out))
        (is (= 1 src))
        (is (zerop (%pin-borrows-of caller rt)))
        (is (null (aref (wasmdown.component.runtime::%inst-handles caller)
                        (1- 2))))
        (is (member 2 (gethash :handle-free
                               (wasmdown.component.runtime:comp-instance-imports
                                caller))))
        (is (wasmdown.component.runtime::resource-handle-own-p
             (wasmdown.component.runtime::%handle-resource-at
              caller src rt)))))
    ;; (b) SYNC lower, provider TRAP: the unwind removes the borrow
    (multiple-value-bind (caller callee rt)
        (%pin-host-borrow-setup
         comp-store
         (lambda (args)
           (declare (ignore args))
           (error 'wasmdown:host-error :message "provider trap")))
      (declare (ignore callee))
      (let* ((src (%pin-own-handle caller rt 43))
             (wrap (wasmdown.component.runtime::%lower-host-func
                    caller 0 nil)))
        (is (handler-case
                (progn (wasmdown.runtime:wasm-call wrap src) nil)
              (wasmdown:host-error () t)))
        (is (zerop (%pin-borrows-of caller rt)))
        (is (= 1 (%pin-free-slot-count caller)))
        (is (wasmdown.component.runtime::resource-handle-own-p
             (wasmdown.component.runtime::%handle-resource-at
              caller src rt)))))
    ;; (c) ASYNC lower, immediate value: the callee resolves EAGERLY
    ;; (packed RETURNED), the borrow is removed at the same exit and
    ;; the eager deliver_resolve clears the lender ledger
    (multiple-value-bind (caller callee rt)
        (%pin-host-borrow-setup
         comp-store
         (lambda (args)
           (declare (ignore args))
           (is (= 1 (%pin-any-borrow-count
                     wasmdown.component.runtime::*handle-caller-inst*)))
           7))
      (declare (ignore callee))
      (let* ((src (%pin-own-handle caller rt 47))
             (wrap (wasmdown.component.runtime::%canon-lower-async-host-func
                    caller 0
                    (wasmdown.component:make-canonrec :kind :lower :func 0
                                                      :opts nil)))
             (out (let ((wasmdown.component.runtime::*current-component-store*
                          comp-store))
                    (wasmdown.runtime:wasm-call wrap src))))
        (is (equal '(2) out))           ; Subtask.State.RETURNED, eager
        (is (zerop (%pin-borrows-of caller rt)))
        (is (member 2 (gethash :handle-free
                               (wasmdown.component.runtime:comp-instance-imports
                                caller))))
        (is (zerop (%pin-source-lends caller rt)))))))

(pin-test component/host-borrow-retained-across-pending-pin
  ;; (2 cont.) ASYNC lower + HOST-PENDING: the borrow is RETAINED
  ;; while the pending is live (a cancel REQUEST is recorded but is
  ;; NOT terminal), and removed exactly once at the terminal outcome
  ;; (resolve or acknowledged cancel). The lender ledger is the
  ;; subtask's: num_lends stays 1 until the subtask's delivery.
  (flet ((async-lower (caller)
           (wasmdown.component.runtime::%canon-lower-async-host-func
            caller 0
            (wasmdown.component:make-canonrec :kind :lower :func 0
                                              :opts nil)))
         (subtask-of (caller out)
           (aref (wasmdown.component.runtime::%inst-handles caller)
                 (1- (ash (first out) -4)))))
    (let ((store (%current-store)))
      ;; ---- pending -> RESOLVE ----
      (let (resolve! pending)
        (multiple-value-bind (caller callee rt)
            (%pin-host-borrow-setup
             store
             (lambda (args)
               (declare (ignore args))
               (setf pending
                     (wasmdown.component.runtime:make-host-pending
                      :start (lambda (res rej ack)
                               (declare (ignore rej ack))
                               (setf resolve! res)
                               (lambda () nil))))))
          (declare (ignore callee))
          (let* ((src (%pin-own-handle caller rt 44))
                 (wrap (async-lower caller))
                 (out (let ((wasmdown.component.runtime::*current-component-store*
                              store))
                        (wasmdown.runtime:wasm-call wrap src)))
                 (subtask (subtask-of caller out)))
            (is (= 1 (%pin-borrows-of caller rt)))
            (is (= 1 (%pin-source-lends caller rt)))
            (is (= 1 (length (wasmdown.component.runtime::async-subtask-lenders
                              subtask))))
            (funcall resolve! :value)
            (wasmdown.component.runtime::%host-pump store)
            (is (wasmdown.component.runtime::host-pending-delivered-p pending))
            (is (zerop (%pin-borrows-of caller rt)))
            (is (member 2 (gethash :handle-free
                                   (wasmdown.component.runtime:comp-instance-imports
                                    caller))))
            ;; the loan is the subtask's until its DELIVERY clears it
            (is (= 1 (%pin-source-lends caller rt)))
            (is (= 1 (length (wasmdown.component.runtime::async-subtask-lenders
                              subtask))))
            (funcall (wasmdown.component.runtime::async-waitable-pending-event
                      subtask))
            (is (zerop (%pin-source-lends caller rt)))
            (is (null (wasmdown.component.runtime::async-subtask-lenders
                       subtask))))))
      ;; ---- pending -> ACKNOWLEDGED CANCEL ----
      (let (pending)
        (multiple-value-bind (caller callee rt)
            (%pin-host-borrow-setup
             store
             (lambda (args)
               (declare (ignore args))
               (setf pending
                     (wasmdown.component.runtime:make-host-pending
                      :start (lambda (res rej ack)
                               (declare (ignore res rej))
                               (lambda () (funcall ack) t))))))
          (declare (ignore callee))
          (let* ((src (%pin-own-handle caller rt 45))
                 (wrap (async-lower caller))
                 (out (let ((wasmdown.component.runtime::*current-component-store*
                              store))
                        (wasmdown.runtime:wasm-call wrap src)))
                 (subtask (subtask-of caller out)))
            ;; the REQUEST alone keeps the borrow and the operation
            (funcall (wasmdown.component.runtime::async-subtask-on-cancel
                      subtask))
            (is (= 1 (%pin-borrows-of caller rt)))
            (is (eq :waiting
                    (wasmdown.component.runtime::host-pending-state pending)))
            (wasmdown.component.runtime::%host-pump store)
            (is (eq :cancelled
                    (wasmdown.component.runtime::host-pending-state pending)))
            (is (zerop (%pin-borrows-of caller rt)))
            (is (= 1 (%pin-source-lends caller rt)))
            (funcall (wasmdown.component.runtime::async-waitable-pending-event
                      subtask))
            (is (zerop (%pin-source-lends caller rt)))
            (is (null (wasmdown.component.runtime::async-subtask-lenders
                       subtask))))))
      ;; ---- an UNABLE-TO-STOP provider: the cancel is requested but
      ;; NOT acknowledged, so the borrow stays until the normal result
      (let (resolve! pending)
        (multiple-value-bind (caller callee rt)
            (%pin-host-borrow-setup
             store
             (lambda (args)
               (declare (ignore args))
               (setf pending
                     (wasmdown.component.runtime:make-host-pending
                      :start (lambda (res rej ack)
                               (declare (ignore rej ack))
                               (setf resolve! res)
                               (lambda () nil))))))
          (declare (ignore callee))
          (let* ((src (%pin-own-handle caller rt 46))
                 (wrap (async-lower caller))
                 (out (let ((wasmdown.component.runtime::*current-component-store*
                              store))
                        (wasmdown.runtime:wasm-call wrap src)))
                 (subtask (subtask-of caller out)))
            (funcall (wasmdown.component.runtime::async-subtask-on-cancel
                      subtask))
            (wasmdown.component.runtime::%host-pump store)
            (is (wasmdown.component.runtime::host-pending-cancel-requested
                 pending))
            (is (not (wasmdown.component.runtime::host-pending-terminal-p
                      pending)))
            (is (= 1 (%pin-borrows-of caller rt)))
            (funcall resolve! :late)
            (wasmdown.component.runtime::%host-pump store)
            (is (wasmdown.component.runtime::host-pending-delivered-p pending))
            (is (zerop (%pin-borrows-of caller rt)))))))))

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
                      component/premature-park-release-pin
                      component/reentrance-14-pin
                      component/switch-caller-is-not-grantable-pin
                      component/crcc-1-pin
                      component/live-borrow-blocks-task-return-pin
                      component/generic-path-five-cases-pin
                      component/type-view-owner-pins
                      component/borrowed-handle-not-joinable-pin
                      component/switched-target-exit-releases-pin
                      component/nested-sync-lower-keeps-guest-right-pin
                      component/store-metadata-retires-with-store-pin
                      component/resource-metadata-live-graph-isolation-pin
                      component/host-borrow-reclaim-pin
                      component/host-borrow-retained-across-pending-pin
                      component/lock-order-no-scheduler-held-across-store-right-pin
                      component/store-teardown-runs-with-right-held-pin)))          wasmdown.test::*tests*)))
    (multiple-value-bind (passed failed)
        (wasmdown.test:run-all-tests)
      (declare (ignore passed))
      (zerop failed))))

(test component/abi-variant-tag-matching
  ;; A host domain value tags its case with a KEYWORD (a P2 provider
  ;; returns (:closed . nil)); the WIT names the case with a string.
  ;; Matching on the normalised name is what makes the right
  ;; discriminant land in the right payload slot: the old
  ;; (string= :closed "closed") fell through to case 0 and stored the
  ;; NIL payload of the WRONG case's resource slot — the trap that
  ;; blocked the real 0.2 SQLite artifact.
  (let ((cases '(("last-operation-failed" . :error)
                 ("closed" . nil))))
    (wasmdown.test:is
     (= 1 (wasmdown.component.runtime::%abi-case-index :closed cases)))
    (wasmdown.test:is
     (= 0 (wasmdown.component.runtime::%abi-case-index
           :last-operation-failed cases)))
    ;; string tags (the ABI's own "ok"/"error" spelling) keep working
    (wasmdown.test:is
     (= 0 (wasmdown.component.runtime::%abi-case-index "last-operation-failed"
                                                      cases)))
    ;; an unknown tag keeps the spec's fall-through default
    (wasmdown.test:is
     (= 0 (wasmdown.component.runtime::%abi-case-index :nope cases)))))

(test component/abi-result-layout
  ;; result<T,E> is a VARIANT: alignment = max(discriminant size, the
  ;; payloads' alignments), size = aligned(discriminant + payload).
  ;; The old alignment hardcoded 4 and the old size dropped the
  ;; discriminant, so result<enum,error-code> (alignment 1, size 2)
  ;; trapped "unaligned pointer (store)" on an adapter-supplied retptr
  ;; and then disagreed with itself about the layout.
  (multiple-value-bind (mk-ct mk-prim)
      (values (find-symbol "MAKE-CT" '#:wasmdown.component)
              (find-symbol "MAKE-CT*" '#:wasmdown.component))
    (declare (ignore mk-prim))
    (flet ((prim (p) (funcall mk-ct :kind :prim :payload p :offset 0))
           (enum (labels) (funcall mk-ct :kind :enum
                                   :payload (coerce labels 'vector)
                                   :offset 0)))
      (let* ((code (enum '("no-entry" "access")))
             (res (funcall mk-ct :kind :result
                           :payload (cons code code) :offset 0))
             (inst (wasmdown.component.runtime:make-comp-instance)))
        (wasmdown.test:is
         (= 1 (wasmdown.component.runtime::%abi-alignment res inst)))
        (wasmdown.test:is
         (= 2 (wasmdown.component.runtime::%abi-elem-size res inst)))
        ;; a 32-bit payload keeps its 4-alignment for both
        (let ((res32 (funcall mk-ct :kind :result
                              :payload (cons (prim :u32) (prim :u32))
                              :offset 0)))
          (wasmdown.test:is
           (= 4 (wasmdown.component.runtime::%abi-alignment res32 inst)))
          (wasmdown.test:is
           (= 8 (wasmdown.component.runtime::%abi-elem-size res32 inst))))))))

;;; -----------------------------------------------------------------
;;; Canonical ABI: compound RESULT shapes at the top level.
;;;
;;; The error-result discriminant must survive compound payloads: a
;;; nested result, a record payload in the ok arm, and a compound
;;; payload in the err arm all lower flat and store/load through the
;;; canonical layout with the same discriminant.

(test component/abi-compound-result-lowering
  (let* ((mk (symbol-function 'wasmdown.component.runtime::make-ct*))
         (inst (wasmdown.component.runtime:make-comp-instance))
         (cx (wasmdown.component.runtime::make-abi-cx
              :inst inst :type-inst inst :memory nil)))
    (flet ((c (&rest a) (apply mk a))
           (lower (v vt)
             (wasmdown.component.runtime::%abi-lower cx v vt)))
      ;; result<result<u32,u32>, u32>: the OUTER discriminant leads, the
      ;; INNER one follows the join arity.
      (let* ((inner (c :result (cons (c :prim :u32) (c :prim :u32))))
             (outer (c :result (cons inner (c :prim :u32)))))
        (is (equal '(0 0 5) (lower (cons :ok (cons :ok 5)) outer)))
        (is (equal '(0 1 6) (lower (cons :ok (cons :err 6)) outer)))
        (is (equal '(1 7 0) (lower (cons :err 7) outer))))
      ;; a record in the ok arm flattens through the join
      (let* ((rec (c :record (vector (cons "a" (c :prim :u32))
                                     (cons "b" (c :prim :u32)))))
             (r (c :result (cons rec (c :prim :u32)))))
        (is (equal '(0 1 2)
                   (lower (cons :ok (list (cons "a" 1) (cons "b" 2))) r)))
        (is (equal '(1 9 0) (lower (cons :err 9) r))))
      ;; a list in the err arm keeps the pointer+length join
      (let* ((lst (c :list (cons (c :prim :u8) nil)))
             (r (c :result (cons (c :prim :u32) lst))))
        (is (equal '(0 3 0) (lower (cons :ok 3) r))))
      ;; AUDIT (2026-11-02): the former residual note said a compound
      ;; OUTER result could lose its top-level discriminant. No
      ;; reproducer exists against this tree: the :result arm lowers to
      ;; variant("ok","error") and the join always leads with the
      ;; discriminant. These shapes pin that contract explicitly.
      (let* ((empty (c :record (vector)))
             (r (c :result (cons empty empty))))
        (is (equal '(0) (lower (cons :ok nil) r)))
        (is (equal '(1) (lower (cons :err nil) r))))
      (let* ((t1 (c :tuple (vector (c :prim :u32))))
             (t2 (c :tuple (vector (c :prim :u32) (c :prim :u32))))
             (r (c :result (cons t1 t2))))
        (is (equal '(0 5 0) (lower (cons :ok (list 5)) r)))
        (is (equal '(1 6 7) (lower (cons :err (list 6 7)) r))))
      (let* ((var (c :variant
                     (vector (cons "a" (c :prim :u32))
                             (cons "b" (c :record
                                           (vector (cons "x" (c :prim :u32))
                                                   (cons "y" (c :prim :u32))))))))
             (r (c :result (cons var (c :prim :u32)))))
        (is (equal '(0 0 5 0) (lower (cons :ok (cons "a" 5)) r)))
        (is (equal '(0 1 6 7)
                   (lower (cons :ok (cons "b" (list (cons "x" 6)
                                                     (cons "y" 7))))
                          r)))
        (is (equal '(1 8 0 0) (lower (cons :err 8) r))))
      (let* ((o (c :option (c :prim :u32)))
             (r (c :result (cons o o))))
        (is (equal '(0 1 3) (lower (cons :ok (cons :some 3)) r)))
        (is (equal '(0 0 0) (lower (cons :ok :none) r)))
        (is (equal '(1 1 4) (lower (cons :err (cons :some 4)) r)))))))

(test component/abi-bare-variant-label
  ;; A bare label is the native providers' shorthand for a PAYLOAD-LESS
  ;; case (e.g. (:err . "no-entry") -> "no-entry"). It must never be
  ;; accepted for a case WITH a payload: that would store only the
  ;; discriminant and leave the payload slot untouched.
  (let* ((mk (symbol-function 'wasmdown.component.runtime::make-ct*))
         (inst (wasmdown.component.runtime:make-comp-instance))
         (mem (wasmdown.runtime:make-memory-instance
               (wasmdown.types:make-wasm-limits 1)))
         (cx (wasmdown.component.runtime::make-abi-cx
              :inst inst :type-inst inst :memory mem)))
    (flet ((c (&rest a) (apply mk a)))
      (let* ((v (c :variant (vector (cons "none" nil)
                                    (cons "some" (c :prim :u32))))))
        ;; payload-less: bare label is the shorthand, discriminant only
        (wasmdown.component.runtime::%abi-store cx "none" v 0)
        (is (= 0 (wasmdown.component.runtime::%abi-load-int cx 0 1)))
        ;; payload-bearing: bare label is refused, never half-stored
        (handler-case
            (progn
              (wasmdown.component.runtime::%abi-store cx "some" v 8)
              (is nil))
          (wasmdown:host-error (e)
            (is (search "bare variant label" (wasmdown:wasmdown-error-message e)))))
        ;; the tagged form still stores discriminant + payload
        (wasmdown.component.runtime::%abi-store cx (cons "some" 7) v 8)
        (is (= 1 (wasmdown.component.runtime::%abi-load-int cx 8 1)))
        (is (= 7 (wasmdown.component.runtime::%abi-load-int cx 12 4)))))))

(test component/abi-compound-result-store-load
  ;; the canonical LAYOUT (not just the flat path): discriminant byte,
  ;; join-aligned payload, and the exact round trip for both arms.
  (let* ((mk (symbol-function 'wasmdown.component.runtime::make-ct*))
         (inst (wasmdown.component.runtime:make-comp-instance))
         (mem (wasmdown.runtime:make-memory-instance
               (wasmdown.types:make-wasm-limits 1)))
         (cx (wasmdown.component.runtime::make-abi-cx
              :inst inst :type-inst inst :memory mem)))
    (flet ((c (&rest a) (apply mk a)))
      (let* ((rec (c :record (vector (cons "a" (c :prim :u32))
                                     (cons "b" (c :prim :u8)))))
             (r (c :result (cons rec (c :prim :u32))))
             (val (cons :ok (list (cons "a" 7) (cons "b" 9)))))
        (wasmdown.component.runtime::%abi-store cx val r 0)
        (is (= 0 (wasmdown.component.runtime::%abi-load-int cx 0 1)))
        (is (equal val
                   (wasmdown.component.runtime::%abi-load cx r 0))))
      (let* ((rec (c :record (vector (cons "a" (c :prim :u32)))))
             (r (c :result (cons (c :prim :u32) rec)))
             (val (cons :err (list (cons "a" 5)))))
        (wasmdown.component.runtime::%abi-store cx val r 32)
        (is (= 1 (wasmdown.component.runtime::%abi-load-int cx 32 1)))
        (is (equal val
                   (wasmdown.component.runtime::%abi-load cx r 32)))))))
