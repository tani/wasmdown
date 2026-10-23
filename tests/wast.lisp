;;;; .wast spec-script runner tests (src/wast.lisp): stateful command
;;;; execution, failure-phase oracle, spectest imports, NaN classes.

(defpackage #:wasmdown.test.wast
  (:use #:cl #:wasmdown.types)
  (:nicknames #:wd.test.wast))

(in-package #:wasmdown.test.wast)

(defun %outcomes (script)
  (map 'list #'wasmdown.wast:wast-result-outcome
       (wasmdown.wast:run-script script)))

(defun %first-outcome (script)
  (first (%outcomes script)))

(wasmdown.test:test wast/module-invoke-assert-return
  (let ((os (%outcomes
             "(module
                (func (export \"add\") (param i32 i32) (result i32)
                  (i32.add (local.get 0) (local.get 1))))
              (invoke \"add\" (i32.const 40) (i32.const 2))
              (assert_return (invoke \"add\" (i32.const 40) (i32.const 2))
                             (i32.const 42))")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os))))

(wasmdown.test:test wast/assert-malformed-bad-magic
  (let ((o (%first-outcome
            "(assert_malformed (module binary \"\\00\\01\") \"missing magic\")")))
    (wasmdown.test:is (eq o :pass))))

(wasmdown.test:test wast/assert-invalid-type-mismatch
  (let ((o (%first-outcome
            "(assert_invalid (module (func (result i32) (i64.const 1))) \"type mismatch\")")))
    (wasmdown.test:is (eq o :pass))))

(wasmdown.test:test wast/assert-unlinkable-missing-import
  (let ((o (%first-outcome
            "(assert_unlinkable (module (import \"spectest\" \"no_such_thing\" (func))) \"unknown import\")")))
    (wasmdown.test:is (eq o :pass))))

(wasmdown.test:test wast/assert-trap-unreachable
  (let ((o (%first-outcome
            "(module (func (export \"boom\") unreachable))
             (assert_trap (invoke \"boom\") \"unreachable\")")))
    (wasmdown.test:is (eq o :pass))))

(wasmdown.test:test wast/register-cross-module
  (let ((os (%outcomes
             "(module (func (export \"f\") (result i32) (i32.const 7)))
              (register \"M\")
              (module (import \"M\" \"f\" (func (result i32)))
                      (func (export \"call\") (result i32) (call 0)))
              (assert_return (invoke \"call\") (i32.const 7))")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os))))

(wasmdown.test:test wast/register-named-module
  ;; (module $Mf ...)(register "Mf" $Mf): the alias must reach the
  ;; import registry (second module links against "Mf"."f") AND the
  ;; named instance must remain invocable as $Mf (linking.wast
  ;; convention).
  (let ((os (%outcomes
             "(module $Mf (func (export \"f\") (result i32) (i32.const 7)))
              (register \"Mf\" $Mf)
              (module (import \"Mf\" \"f\" (func (result i32)))
                      (func (export \"call\") (result i32) (call 0)))
              (assert_return (invoke \"call\") (i32.const 7))
              (assert_return (invoke $Mf \"f\") (i32.const 7))")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os))))

(wasmdown.test:test wast/spectest-import-capture
  ;; spectest print_i32 is a host function; a module importing and
  ;; invoking it must link, instantiate and execute without error.
  ;; (The call-capture list is script-scoped by design; observable
  ;; behavior here is that the invoke PASSES.)
  (let ((os (%outcomes
             "(module (import \"spectest\" \"print_i32\" (func (param i32)))
                      (func (export \"go\") (i32.const 42) (call 0)))
              (invoke \"go\")")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os))))
(wasmdown.test:test wast/nan-canonical-class
  ;; f32.abs of -nan must produce a canonical-class NaN (per spec the
  ;; payload bit is forced); assert_return with nan:canonical passes.
  (let ((o (%first-outcome
            "(module (func (export \"n\") (result f32)
                           (f32.abs (f32.const nan:0x200000))))
              (assert_return (invoke \"n\") (f32.const nan:canonical))")))
    (wasmdown.test:is (eq o :pass))))

(wasmdown.test:test wast/get-exported-global
  (let ((o (%first-outcome
            "(module (global (export \"g\") i32 (i32.const 123)))
              (assert_return (get \"g\") (i32.const 123))")))
    (wasmdown.test:is (eq o :pass))))

(wasmdown.test:test wast/assert-exhaustion-deep-recursion
  (let ((o (%first-outcome
            "(module (func $f (export \"f\") (call $f)))
              (assert_exhaustion (invoke \"f\") \"call stack exhausted\")")))
    (wasmdown.test:is (eq o :pass))))

;;; --- Regression: script-parser robustness (hostile/truncated input) ---

(wasmdown.test:test wast/parse-script-multi-line-binary
  ;; Multi-line (module binary ...) forms with interleaved comments and
  ;; uppercase \\xx / BOM escapes must parse to one command each (the
  ;; original command-collapse crasher shape, binary.wast).
  (let ((src (with-output-to-string (out)
               (write-line "(module binary" out)
               (write-line "  \"\\00asm\\01\\00\\00\\00\" ;; magic+version" out)
               (write-line "  \"\\ef\\bb\\bf\")" out)
               (write-line "(assert_malformed (module binary \"\\00ASM\\01\\00\\00\\00\")" out)
               (write-line "  \"magic header not detected\")" out))))

    (wasmdown.test:is
     (= 2 (length (wasmdown.wast::parse-script src))))))

(wasmdown.test:test wast/parse-script-binary-wast-17-commands
  ;; The first 20 lines of the vendored binary.wast must parse to
  ;; exactly 17 commands (verified against the fixture).
  (let* ((path (merge-pathnames
                 "tests/spec-core/binary.wast"
                 (make-pathname
                  :directory (butlast
                              (pathname-directory
                               (or *compile-file-pathname*
                                   *load-pathname*))))))
         (full (with-open-file (s path :external-format :utf-8)
                 (with-output-to-string (out)
                   (loop for l = (read-line s nil nil)
                         repeat 20 while l do (write-line l out)))))
         (cmds (wasmdown.wast::parse-script full)))
    (wasmdown.test:is (= 17 (length cmds)))))

(wasmdown.test:test wast/parse-script-truncated-signals-decode-error
  ;; A prefix cutting a multi-line form mid-string must signal a
  ;; CATCHABLE decode-error (PLAN.md 10.1), never a crash.
  (let ((src (with-output-to-string (out)
               (write-line "(module binary" out)
               (write-line "  \"\\00asm\\01\\00\\00\\00\"" out)
               (write-line
                 "(assert_malformed (module binary \"\\00asm\" \"\\01\") \"m\")" out))))
    (wasmdown.test:is
     (handler-case
         (progn (wasmdown.wast::parse-script src) nil)
       (wasmdown:decode-error () :caught)))))

(wasmdown.test:test wast/assert-malformed-too-many-locals-no-crash
  ;; binary.wast's 0xFFFFFFFF+2 locals case crashed the host by making
  ;; the validator allocate a 2^32-entry vector. The wast layer must
  ;; bound it and pass the assertion at the decode phase.
  (let ((o (%first-outcome
            "(assert_malformed
               (module binary
                 \"\\00asm\\01\\00\\00\\00\"
                 \"\\01\\04\\01\\60\\00\\00\"
                 \"\\03\\02\\01\\00\"
                 \"\\0a\\0c\\01\"
                 \"\\0a\\02\"
                 \"\\ff\\ff\\ff\\ff\\0f\\7f\"
                 \"\\02\\7e\"
                 \"\\0b\")
               \"too many locals\")")))
    (wasmdown.test:is (eq o :pass))))

(wasmdown.test:test wast/nan-payload-comparator
  ;; The assert_return nan:0x... directive must compare the PAYLOAD
  ;; FIELD (not raw bits) against the actual value's payload. Pin the
  ;; mask semantics both ways: matching payload passes; mismatching
  ;; payload fails (raw-bits comparison would fail BOTH ways, so this
  ;; pair proves the mask).
  ;; f32: bits 0x7FA00000 = nan with payload 0x200000.
  (let ((os (%outcomes
             "(module (func (export \"f\") (result f32)
                            (f32.reinterpret_i32 (i32.const 0x7fa00000))))
              (assert_return (invoke \"f\") (f32.const nan:0x200000))")))
    (wasmdown.test:is (equal '(:pass :pass) os)))
  ;; negative: payload 0x400000 must NOT match payload 0x200000
  (let ((os (%outcomes
             "(module (func (export \"f\") (result f32)
                            (f32.reinterpret_i32 (i32.const 0x7fa00000))))
              (assert_return (invoke \"f\") (f32.const nan:0x400000))")))
    (wasmdown.test:is (equal '(:pass :fail) os)))
  ;; f64 mirror: bits 0x7FF4000000000000 = nan payload 0x4000000000000.
  (let ((os (%outcomes
             "(module (func (export \"g\") (result f64)
                            (f64.reinterpret_i64 (i64.const 0x7ff4000000000000))))
              (assert_return (invoke \"g\") (f64.const nan:0x4000000000000))")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

(wasmdown.test:test wast/definition-instance-implemented
  ;; Loop 18: (module definition $M ...) / (module instance $I $M)
  ;; are implemented Core 3.0 named-form script commands (instance.
  ;; wast). A definition parses, validates and stores WITHOUT
  ;; instantiating; an instance re-instantiates the stored definition
  ;; (generativity: fresh state per instance); register publishes it
  ;; and a get on the instance reads its own copy of the state.
  (let ((os (%outcomes
             "(module definition $M
                (global (export \"glob\") (mut i32) (i32.const 0)))
              (module instance $I1 $M)
              (module instance $I2 $M)
              (register \"I1\" $I1)
              (register \"I2\" $I2)")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass :pass) os))))

(wasmdown.test:test wast/typed-ref-typeuse-unsupported-no-crash
  ;; Regression (loop 16, the biggest single-fix family): (ref $t),
  ;; (ref null $t) and (ref 0)/(ref null 0) typeuses are GC-scope
  ;; typed references. They used to reach %valtype with a (:LIST ...)
  ;; node and crash the host with a TYPE-ERROR, misclassifying the
  ;; suite's assert_invalid cases as harness-errors (ref.wast,
  ;; type-equivalence.wast, type-rec.wast, func.wast ::117). The
  ;; %unsupported-feature-p scan must classify them :unsupported
  ;; BEFORE parse — honest GC scope, never a host crash.
  (let ((os (%outcomes
             "(assert_invalid
                (module (type $t (func))
                  (func $f (local $x (ref $t)) (drop (local.get $x))))
                \"uninitialized local\")")))
    (wasmdown.test:is (equal '(:unsupported) os)))
  ;; (ref <digit>) index spelling, same family:
  (let ((os (%outcomes
             "(assert_invalid
                (module (type (func (param (ref 1)))) )
                \"unknown type\")")))
    (wasmdown.test:is (equal '(:unsupported) os)))
  ;; named-param carrier (type-equivalence ::0 shape): the module is
  ;; GC-scope even though the param spelling is (param $r (ref $t1)).
  (let ((os (%outcomes
             "(module (type $t1 (func))
                (func $f1 (param $r (ref $t1)) (call $f1 (local.get $r))))")))
    (wasmdown.test:is (equal '(:unsupported) os))))

(wasmdown.test:test wast/binary-gc-valtype-section-unsupported
  ;; Regression (loop 16): binary.wast ::127's type section uses the GC
  ;; typed-ref valtype forms (0x63 (ref null x) / 0x64 (ref x)) inside a
  ;; functype. The decoder reports 'unknown type form 0x63' at DECODE,
  ;; which misclassified the assert_invalid fixture as a phase FAIL;
  ;; these bytes are GC scope and must classify :unsupported before
  ;; the decode pipeline runs.
  (let ((os (%outcomes
             "(assert_invalid
                (module binary
                  \"\\00asm\\01\\00\\00\\00\"
                  \"\\01\\0a\\01\"
                  \"\\60\\01\\63\\80\\80\\80\\80\\08\\00\")
                \"unknown type\")")))
    (wasmdown.test:is (equal '(:unsupported) os))))

(wasmdown.test:test wast/table-init-expr-parses
  ;; Regression (loop 16, cluster 2): the WAT extended table
  ;; initializer (table n reftype <expr>) — table.wast ::13/14's
  ;; (table 1 funcref (ref.null func)) — is a VALID shape the parser
  ;; used to reject as 'junk after table type' at DECODE. It parses to
  ;; the table plus an active elem segment [expr x min] at offset 0;
  ;; the non-const shape (global.wast ::120) still fails at
  ;; VALIDATION, never DECODE.
  (let ((os (%outcomes
             "(module (table 1 funcref (ref.null func)))
              (assert_invalid
                (module (global $g funcref (ref.null func))
                        (table $t 10 funcref (global.get $g)))
                \"unknown global\")")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

(wasmdown.test:test wast/elem-data-offset-defined-global
  ;; Regression (loop 16, cluster 2): elem/data OFFSET and item const
  ;; exprs may global.get any immutable global INCLUDING defined ones
  ;; (data.wast ::9/10, elem.wast ::15/16, global.wast ::113) — the
  ;; imported-or-previously-defined restriction applies only to
  ;; global INIT exprs, but the validator had passed the imported
  ;; count as the limit for elem/data too, failing valid modules.
  (let ((os (%outcomes
             "(module (memory 1) (global i32 (i32.const 0))
                (data (global.get 0) \"a\"))
              (module (table 1 funcref) (global i32 (i32.const 0))
                (func $f) (elem (global.get 0) $f))")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

(wasmdown.test:test wast/declared-refs-include-global-inits
  ;; Regression (loop 16): a ref.func in a GLOBAL init declares its
  ;; target for the whole module (spec funcidx(globals) — elem.wast
  ;; ::135's (global (export \"f\") funcref (ref.func 0)), ref_func
  ;; ::14). The validator only collected elem/export declarations, so
  ;; valid modules failed 'is not declared by an element segment or
  ;; export' and cascaded register/import failures after them.
  (let ((os (%outcomes
             "(module $m4 (func (result i32) (i32.const 42))
                (global (export \"f\") funcref (ref.func 0)))
              (register \"m4\" $m4)
              (module (import \"m4\" \"f\" (global funcref))
                (table 1 funcref)
                (elem (i32.const 0) funcref (global.get 0)))")))
    (wasmdown.test:is (equal '(:pass :pass :pass) os))))

(wasmdown.test:test wast/elem-active-seg-implicitly-dropped
  ;; Regression (loop 17, M3): ACTIVE element segments are implicitly
  ;; dropped after instantiation — table.init from them traps OOB
  ;; (elem.wast ::706). DECLARATIVE segments are dropped without
  ;; initializing anything (elem.wast ::716); PASSIVE segments stay
  ;; initable (elem.wast ::1119 positive case below).
  (let ((os (%outcomes
             "(module
                (table 10 funcref)
                (elem $e (i32.const 0) func $f)
                (func $f)
                (func (export \"init\")
                  (table.init $e (i32.const 0) (i32.const 0) (i32.const 1))))
              (assert_trap (invoke \"init\") \"out of bounds table access\")
              (module
                (table 10 funcref)
                (elem $e declare func $f)
                (func $f)
                (func (export \"init\")
                  (table.init $e (i32.const 0) (i32.const 0) (i32.const 1))))
              (assert_trap (invoke \"init\") \"out of bounds table access\")")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass) os))))

(wasmdown.test:test wast/passive-elem-still-initable
  ;; Counterpart to wast/elem-active-seg-implicitly-dropped: a PASSIVE
  ;; elem segment remains table.init-able after instantiation
  ;; (elem.wast ::1113-1125 — the inline-table module's passive $e).
  (let ((os (%outcomes
             "(module
                (func $f (result i32) i32.const 0xAB)
                (func $g (result i32) i32.const 0xCD)
                (table funcref (elem (ref.func $f)))
                (elem $e funcref (ref.func $g))
                (func (export \"init\")
                  (table.init $e (i32.const 0) (i32.const 0) (i32.const 1)))
                (func (export \"run\") (result i32)
                  (call_indirect (result i32) (i32.const 0))))
              (invoke \"init\")
              (assert_return (invoke \"run\") (i32.const 0xCD))")))
    (wasmdown.test:is (equal '(:pass :pass :pass) os))))

(wasmdown.test:test wast/table-init-immediate-order
  ;; Regression (loop 17, M4): the WAT grammar is
  ;; table.init tableidx elemidx — the TABLE index comes FIRST
  ;; (elem.wast ::988/:997 assert_invalid: a $t table id in the first
  ;; slot must resolve against the table namespace, not elem). The
  ;; one-index abbreviation table.init x ≡ table.init 0 x keeps the
  ;; elem-only form working (elem.wast ::703).
  (let ((os (%outcomes
             "(assert_invalid
                (module
                  (func $f)
                  (table $t 1 externref)
                  (elem $e funcref (ref.func $f))
                  (func (table.init $t $e (i32.const 0) (i32.const 0) (i32.const 1))))
                \"type mismatch\")
              (assert_invalid
                (module
                  (table $t 1 funcref)
                  (elem $e externref (ref.null extern))
                  (func (table.init $t $e (i32.const 0) (i32.const 0) (i32.const 1))))
                \"type mismatch\")")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

(wasmdown.test:test wast/memory-data-abbrev-zero-pages
  ;; Regression (loop 17, M5): the (memory (data ...)) abbreviation
  ;; sizes the memory to the data — empty data must produce a 0-page
  ;; memory with NO min-1 floor (memory.wast ::12/:14 expect
  ;; memory.size 0; ::16 expects 1 for one byte). The active data
  ;; segment at offset 0 still initializes normally.
  (let ((os (%outcomes
             "(module (memory (data)) (func (export \"memsize\") (result i32) (memory.size)))
              (assert_return (invoke \"memsize\") (i32.const 0))
              (module (memory (data \"\")) (func (export \"memsize\") (result i32) (memory.size)))
              (assert_return (invoke \"memsize\") (i32.const 0))
              (module (memory (data \"x\")) (func (export \"memsize\") (result i32) (memory.size)))
              (assert_return (invoke \"memsize\") (i32.const 1))")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass :pass :pass) os))))

(wasmdown.test:test wast/token-string-adjacency-malformed
  ;; Regression (loop 17, M1): spec lexer.mll reserved = (idchar |
  ;; string)+ — a string ABUTTING another string or an atom with no
  ;; delimiter is ONE reserved token: 'unknown operator' malformed at
  ;; DECODE (token.wast ::23/:26 `$"l"0`, ::46-::56 `(data "a""b")`,
  ;; ::57/:58/:60 `(func "a"x)`). Previously string-string lexed as
  ;; two strings (module unexpectedly succeeded) and string-atom
  ;; crashed the name parser (`#(97) is not a string designator`).
  ;; The string-then-delimiter shapes stay valid (::21 `(func nop(nop))`,
  ;; ::45 `(data (i32.const 0)"a")`, ::48 `(import "m" "f"(func))`).
  (let ((os (%outcomes
             "(assert_malformed
                (module quote \"(data \\\"a\\\"\\\"b\\\")\") \"unknown operator\")
              (assert_malformed
                (module quote \"(data \\\"a \\\"\\\"b\\\")\") \"unknown operator\")
              (assert_malformed
                (module quote \"(func \\\"a\\\"x)\") \"unknown operator\")
              (assert_malformed
                (module quote \"(func \\\"a\\\"0)\") \"unknown operator\")
              (assert_malformed
                (module quote \"(func \\\"a\\\"$x)\") \"unknown operator\")
              (assert_malformed
                (module quote
                  \"(func (block $l (i32.const 0) (br_table $\\\"l\\\"0)))\")
                \"unknown operator\")
              (assert_malformed
                (module quote
                  \"(func (block $l (i32.const 0) (br_table $\\\"l\\\"$l)))\")
                \"unknown operator\")
              (module (memory 1) (data (i32.const 0)\"a\"))
              (module (data \"a\" \"b\"))
              (module (func $\"l\") (func (export \"call\") nop))
              (assert_return (invoke \"call\") )
              (module (import \"spectest\" \"print\"(func)))")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass :pass :pass :pass
                               :pass :pass :pass :pass :pass)
                            os))))

(wasmdown.test:test wast/atom-string-adjacency-still-malformed
  ;; Counterpart (loop 15's rule, must not regress under M1): an ATOM
  ;; abutting a string is likewise one reserved token — token.wast
  ;; ::59 `(func 0"a")` and the `(data "a")`-family of
  ;; atom-string pairs (token.wast ::30-::44) stay 'unknown operator'.
  (let ((os (%outcomes
             "(assert_malformed
                (module quote \"(func 0\\\"a\\\")\") \"unknown operator\")
              (assert_malformed
                (module quote \"(data\\\"a\\\")\") \"unknown operator\")
              (assert_malformed
                (module quote \"(data $l\\\"a\\\")\") \"unknown operator\")
              (assert_malformed
                (module quote
                  \"(func (block $l (i32.const 0) (br_table 0$\\\"l\\\")))\")
                \"unknown operator\")")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass) os))))

(wasmdown.test:test wast/float-unary-nan-quieting
  ;; Regression (loop 17, M2): trunc/ceil/floor/nearest with a NaN
  ;; operand return that NaN QUIETED — payload and sign preserved,
  ;; quiet bit forced (spec exec/fxx.ml determine_unary_nan +
  ;; canonicalize_nan; f32.wast/f64.wast ::2498/:2500 nearest
  ;; nan:0x200000 -> nan:arithmetic). fnearest used to return the
  ;; sNaN unquieted; the comparator rejected it as not-arithmetic.
  ;; Canonical NaN in stays canonical (::2497/:2499, nan ->
  ;; nan:canonical) and non-NaN behavior is unchanged (ties-to-even).
  (let ((os (%outcomes
             "(module
                (func (export \"nearest\") (param $x f32) (result f32)
                  (f32.nearest (local.get $x)))
                (func (export \"trunc\") (param $x f32) (result f32)
                  (f32.trunc (local.get $x)))
                (func (export \"ceil\") (param $x f32) (result f32)
                  (f32.ceil (local.get $x)))
                (func (export \"floor\") (param $x f32) (result f32)
                  (f32.floor (local.get $x))))
              (assert_return (invoke \"nearest\" (f32.const nan:0x200000))
                             (f32.const nan:arithmetic))
              (assert_return (invoke \"nearest\" (f32.const -nan:0x200000))
                             (f32.const nan:arithmetic))
              (assert_return (invoke \"nearest\" (f32.const nan))
                             (f32.const nan:canonical))
              (assert_return (invoke \"trunc\" (f32.const nan:0x200000))
                             (f32.const nan:arithmetic))
              (assert_return (invoke \"ceil\" (f32.const nan:0x200000))
                             (f32.const nan:arithmetic))
              (assert_return (invoke \"floor\" (f32.const nan:0x200000))
                             (f32.const nan:arithmetic))
              (assert_return (invoke \"nearest\" (f32.const 0x1.921fb6p+2))
                             (f32.const 0x1.8p+2))
              (assert_return (invoke \"nearest\" (f32.const -0x1p-1))
                             (f32.const -0x0p+0))")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass :pass :pass
                               :pass :pass :pass)
                            os))))

(wasmdown.test:test wast/f64-unary-nan-quieting
  ;; f64 counterpart of wast/float-unary-nan-quieting: f64.wast
  ;; ::2498/:2500 (nearest nan:0x4000000000000 -> nan:arithmetic)
  ;; plus trunc/ceil/floor payload-NaN quieting and the exact-value
  ;; nearest cases (f64.wast ::2502/:2504 subnormal ties).
  (let ((os (%outcomes
             "(module
                (func (export \"nearest\") (param $x f64) (result f64)
                  (f64.nearest (local.get $x)))
                (func (export \"trunc\") (param $x f64) (result f64)
                  (f64.trunc (local.get $x)))
                (func (export \"ceil\") (param $x f64) (result f64)
                  (f64.ceil (local.get $x)))
                (func (export \"floor\") (param $x f64) (result f64)
                  (f64.floor (local.get $x))))
              (assert_return (invoke \"nearest\" (f64.const nan:0x4000000000000))
                             (f64.const nan:arithmetic))
              (assert_return (invoke \"nearest\" (f64.const -nan:0x4000000000000))
                             (f64.const nan:arithmetic))
              (assert_return (invoke \"nearest\" (f64.const nan))
                             (f64.const nan:canonical))
              (assert_return (invoke \"trunc\" (f64.const nan:0x4000000000000))
                             (f64.const nan:arithmetic))
              (assert_return (invoke \"ceil\" (f64.const nan:0x4000000000000))
                             (f64.const nan:arithmetic))
              (assert_return (invoke \"floor\" (f64.const nan:0x4000000000000))
                             (f64.const nan:arithmetic))
              (assert_return (invoke \"nearest\" (f64.const 0x1.921fb54442d18p+2))
                             (f64.const 0x1.8p+2))
              (assert_return (invoke \"nearest\" (f64.const 0x1p-1022))
                             (f64.const 0x0p+0))")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass :pass :pass
                               :pass :pass :pass)
                            os))))

(wasmdown.test:test wast/custom-section-zero-size-bounded
  ;; Regression (loop 17, M6): a custom section's name and payload
  ;; must fit the section's DECLARED size. For "\00\00" (id 0, size
  ;; 0) the name-length read used to run on the whole-module reader,
  ;; consuming bytes of the NEXT section and computing a NEGATIVE
  ;; payload span, which crashed make-array as a harness error
  ;; (custom.wast ::5: -1 is not of type (OR LIST
  ;; (UNSIGNED-BYTE 41))). The sub-reader now bounds the decode at
  ;; the declared end: the case fails 'truncated' at DECODE — the
  ;; honest assert_malformed outcome ("unexpected end").
  (let ((os (%outcomes
             ";; custom.wast ::0's first module: multiple custom sections
              (module binary
                \"\\00asm\\01\\00\\00\\00\"
                \"\\00\\24\\10\" \"a custom section\" \"this is the payload\")
              ;; custom.wast ::3 (case ::5): the zero-size custom
              ;; section's name read must fail TRUNCATED at DECODE
              ;; inside the declared section end — never eat the next
              ;; section's bytes (the make-array crash)
              (assert_malformed
                (module binary \"\\00asm\\01\\00\\00\\00\" \"\\00\\00\")
                \"unexpected end\")
              ;; custom.wast ::4: two zero-size custom sections
              (assert_malformed
                (module binary \"\\00asm\\01\\00\\00\\00\" \"\\00\\00\\00\")
                \"unexpected end\")
              ;; custom.wast ::6: custom name length 0x26 exceeds the
              ;; section's declared size 0x10 — 'length out of bounds'
              (assert_malformed
                (module binary
                  \"\\00asm\\01\\00\\00\\00\"
                  \"\\00\\26\\10\" \"a custom section\" \"this is the payload\")
                \"length out of bounds\")")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass) os))))

(wasmdown.test:test wast/annotation-forms-unsupported
  ;; Regression (loop 17, M7): (@...) annotation forms are the
  ;; unadopted annotations proposal. The SCRIPT lexer previously
  ;; crashed on the reserved characters legal inside annotations
  ;; (annotations.wast ::14's , ; ] [ }} }x{ ({) ,{{};}] ;) — a
  ;; whole-file harness error. It now skips (@...) forms like the
  ;; reference lexer.mll `annot` rule (nestable groups, comments,
  ;; strings), so the file PARSES; a module whose text contains "(@"
  ;; classifies :unsupported honestly (annotations.wast: 74 results,
  ;; no harness error).
  (let ((os (%outcomes
             "(module (@a) (@a , ; ] [ }} }x{ ({) ,{{};}] ;) (func))
              (assert_malformed (module quote \"( @a)\") \"unknown operator\")")))
    (wasmdown.test:is (equal '(:unsupported :pass) os))))

(wasmdown.test:test wast/implicit-module-script-short-form
  ;; Regression (loop 17, M7): the script grammar's inline-module
  ;; short form (parser.mly `script : list(cmd) EOF | inline_module1
  ;; EOF`, `inline_module : module_fields`): consecutive top-level
  ;; module FIELD forms wrap into ONE implicit (module ...) command
  ;; (inline-module.wast = \"(func) (memory 0) (func (export \\\"f\\\"))\").
  ;; Previously 'unknown script command func' crashed the whole file.
  ;; The wrapped module must instantiate and be invocable, and a
  ;; field run interrupted by a command stays separate from a later
  ;; run.
  (let ((os (%outcomes
             "(func (export \"f\") (result i32) (i32.const 42))
              (memory 0)
              (invoke \"f\")
              (assert_return (invoke \"f\") (i32.const 42))
              (func) (table 1 funcref)")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass) os))))

(wasmdown.test:test wast/return-call-deep-self-recursion
  ;; Tail calls (loop 18, PLAN.md §3.1/§6.2): return_call replaces the
  ;; current guest frame via the interp-invoke trampoline (THROW
  ;; *TAIL-CALL-TAG* unwinds to the loop; re-entry keeps the SAME
  ;; *CALL-DEPTH*), so counting to 1_000_000 runs in constant stack
  ;; (return_call.wast ::61-66/::126) instead of trapping exhausted.
  (let ((os (%outcomes
             "(module
                (func $count (export \"count\") (param i64) (result i64)
                  (if (result i64) (i64.eqz (local.get 0))
                    (then (local.get 0))
                    (else (return_call $count
                            (i64.sub (local.get 0) (i64.const 1)))))))
              (assert_return (invoke \"count\" (i64.const 1000000))
                             (i64.const 0))")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

(wasmdown.test:test wast/return-call-mutual-indirect-recursion
  ;; return_call_indirect through a table: mutual even/odd to 1M
  ;; iterations must stay constant-stack (return_call_indirect.wast
  ;; ::204-225/::294-301), and the runtime type check still traps
  ;; 'indirect call type mismatch' on a wrong-typed table slot
  ;; (::268-269 assert_trap batch) and 'undefined element' OOB
  ;; (::270-272).
  (let ((os (%outcomes
             "(module
                (type $over-i64 (func (param i64) (result i64)))
                (type $out-i32 (func (result i32)))
                (table funcref (elem $even $odd $out))
                (func $even (export \"even\") (param i64) (result i64)
                  (if (result i64) (i64.eqz (local.get 0))
                    (then (i64.const 44))
                    (else (return_call_indirect (type $over-i64)
                           (i64.sub (local.get 0) (i64.const 1))
                           (i32.const 1)))))
                (func $odd (export \"odd\") (param i64) (result i64)
                  (if (result i64) (i64.eqz (local.get 0))
                    (then (i64.const 99))
                    (else (return_call_indirect (type $over-i64)
                           (i64.sub (local.get 0) (i64.const 1))
                           (i32.const 0)))))
                (func $out (result i32) (i32.const 1))
                (func (export \"d\") (param i32 i64) (result i64)
                  (return_call_indirect (type $over-i64)
                                        (local.get 1) (local.get 0))))
              (assert_return (invoke \"even\" (i64.const 1000001))
                             (i64.const 99))
              (assert_return (invoke \"odd\" (i64.const 1000000))
                             (i64.const 99))
              (assert_trap (invoke \"d\" (i32.const 2) (i64.const 2))
                           \"indirect call type mismatch\")
              (assert_trap (invoke \"d\" (i32.const 9) (i64.const 2))
                           \"undefined element\")")))
    (wasmdown.test:is
     (equal '(:pass :pass :pass :pass :pass) os))))

(wasmdown.test:test wast/return-call-non-tail-growth-still-exhausts
  ;; Depth accounting edge: the trampoline restores *CALL-DEPTH* to
  ;; the invoke's entry value on every tail-call re-entry, so a tail
  ;; chain never falsely exhausts — but NON-tail :call recursion still
  ;; increments per frame and must trap :call-stack-exhausted
  ;; (return_call.wast's assert_exhaustion contract; call.wast ::425+).
  (let ((os (%outcomes
             "(module
                (func $f (export \"f\") (param i64) (result i64)
                  (i64.add (i64.const 1)
                    (call $f (local.get 0))))
                (func $tail (export \"tail\") (param i64) (result i64)
                  (if (result i64) (i64.eqz (local.get 0))
                    (then (local.get 0))
                    (else (return_call $tail
                            (i64.sub (local.get 0) (i64.const 1)))))))
              (assert_return (invoke \"tail\" (i64.const 2000))
                             (i64.const 0))
              (assert_exhaustion (invoke \"f\" (i64.const 0))
                                 \"call stack exhausted\")")))
    (wasmdown.test:is (equal '(:pass :pass :pass) os))))

(wasmdown.test:test wast/return-call-validation-result-match
  ;; Validation (loop 18): like call/call_indirect, PLUS the callee's
  ;; results must EQUAL the caller's declared results (return_call.wast
  ;; ::146-258 assert_invalid batch: result-more/fewer/i64-vs-i32 and
  ;; the void-vs-num shapes). Arity-only differences in PARAMS stay
  ;; valid (::184-192 module batch); the callee-index bounds check is
  ;; 'unknown function' (::252-258).
  (let ((os (%outcomes
             "(assert_invalid
                (module
                  (func $more (result i32 i32) unreachable)
                  (func (result i32) (return_call $more)))
                \"type mismatch\")
              (assert_invalid
                (module
                  (func $fewer (result i32) unreachable)
                  (func (result i32 i32) (return_call $fewer)))
                \"type mismatch\")
              (assert_invalid
                (module
                  (func $i64 (result i64) unreachable)
                  (func (result i32) (return_call $i64)))
                \"type mismatch\")
              (module
                (func $arity-1-vs-0 (i32.const 1) (return_call 1))
                (func $void))
              (assert_invalid
                (module (func $unbound (return_call 1)))
                \"unknown function\")
              (assert_invalid
                (module (func $big (return_call 1012321300)))
                \"unknown function\")")))
    (wasmdown.test:is
     (equal '(:pass :pass :pass :pass :pass :pass) os))))

(wasmdown.test:test wast/return-call-indirect-validation-and-malformed
  ;; return_call_indirect typing: table must be funcref ('unknown
  ;; table' when absent, return_call_indirect.wast ::435-441; the
  ;; externref table is 'type mismatch' ::593-600), the typeuse
  ;; grammar keeps the call_indirect malformed shapes ('unexpected
  ;; token' ordering batch ::309-343, 'inline function type' when
  ;; (type)+inline are unequal ::388-431), and a bare result-only
  ;; typeuse binds fine (::71-95 syntax batch).
  (let ((os (%outcomes
             "(assert_invalid
                (module (type (func))
                        (func $no-table (return_call_indirect (type 0)
                                                           (i32.const 0))))
                \"unknown table\")
              (assert_invalid
                (module (type (func))
                        (table 10 externref)
                        (func $x (return_call_indirect (type 0)
                                                       (i32.const 0))))
                \"type mismatch\")
              (assert_malformed
                (module quote
                  \"(type $sig (func))\"
                  \"(table 0 funcref)\"
                  \"(func (result i32)\"
                  \"  (return_call_indirect (type $sig) (result i32)\"
                  \"    (i32.const 0))\"
                  \")\")
                \"inline function type\")
              (assert_malformed
                (module quote
                  \"(table 0 funcref)\"
                  \"(func (return_call_indirect (param $x i32)\"
                  \"  (i32.const 0) (i32.const 0)))\")
                \"unexpected token\")
              (module
                (type $proc (func))
                (table funcref (elem))
                (func (return_call_indirect (result) (i32.const 0)))
                (func (result i32)
                  (return_call_indirect (param) (param i64) (param)
                    (result) (result i32) (result)
                    (i64.const 0) (i32.const 0))))")))
    (wasmdown.test:is
     (equal '(:pass :pass :pass :pass :pass) os))))

(wasmdown.test:test wast/return-call-host-tail-target
  ;; A tail call to a HOST function (spectest print) must return its
  ;; results as the caller's results — the host body cannot grow the
  ;; guest stack, so %DO-TAIL-HOST throws to the function frame like
  ;; :return (return_call.wast ::82-84 tailprint_i32_f32 ::140).
  (let ((os (%outcomes
             "(module
                (import \"spectest\" \"print_i32_f32\"
                  (func $print (param i32 f32)))
                (func (export \"go\") (param i32 f32)
                  (return_call $print (local.get 0) (local.get 1))))
              (assert_return (invoke \"go\" (i32.const 5)
                                     (f32.const 91.0)))")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

(wasmdown.test:test wast/return-call-multi-result-and-binary-roundtrip
  ;; Multi-value tail calls (return_call.wast ::86-93/::141) and the
  ;; binary path: 0x12 funcidx / 0x13 typeidx tableidx decode and the
  ;; encoder mirrors them (encode == re-encode after decode).
  (let ((os (%outcomes
             "(module
                (func $swizzle (param f64 i64) (result i32 f32)
                  (i32.wrap_i64 (local.get 1))
                  (f32.demote_f64 (local.get 0)))
                (func (export \"s\") (param f64 i64) (result i32 f32)
                  (return_call $swizzle (local.get 0) (local.get 1))))
              (assert_return (invoke \"s\" (f64.const 4.2) (i64.const 99))
                             (i32.const 99) (f32.const 4.2))")))
    (wasmdown.test:is (equal '(:pass :pass) os)))
  (let* ((m (wasmdown.text:parse-wat
             "(module (func $f (result i32) (return_call $g))
                     (func $g (result i32) (i32.const 7)))"))
         (bytes (wasmdown.binary:encode-module m))
         (m2 (wasmdown.binary:decode-module bytes))
         (bytes2 (wasmdown.binary:encode-module m2)))
    (wasmdown.test:is (equalp bytes bytes2))
    (let ((instr (aref (wasmdown.types:wasm-func-body
                        (aref (wasmdown.types:wasm-module-funcs m2) 0))
                       0)))
      (wasmdown.test:is (eq (wasmdown.types:wasm-instr-op instr)
                            :return-call))
      (wasmdown.test:is (equal (wasmdown.types:wasm-instr-imms instr)
                               '(:func 1))))))

(wasmdown.test:test wast/return-call-ref-still-gc-unsupported
  ;; return_call_ref (typed function references) is OUT of the engine
  ;; slice: with the TAIL-CALLS arm gone (return_call/return_call_
  ;; indirect are implemented), the call_ref-family token keeps
  ;; return_call_ref.wast's modules honestly classified :unsupported
  ;; via the GC arm — never a decode-phase fail.
  (let ((os (%outcomes
             "(module (type $t (func))
                      (elem declare func $f)
                      (func $f (result (ref $t)) (ref.func $f))
                      (func (export \"g\") (result i32)
                        (return_call_ref $t (ref.func $f))))
              (invoke \"g\")")))
    (wasmdown.test:is (equal '(:unsupported :unsupported) os))))
