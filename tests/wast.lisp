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

(wasmdown.test:test wast/definition-instance-unsupported
  ;; Regression (loop 15): (module definition $M ...) / (module
  ;; instance $I $M) are Core 3.0 named-form script commands outside
  ;; this engine slice; they must classify :unsupported BEFORE decode
  ;; (not crash parse-wat on a NIL payload), the $id is recorded, and
  ;; a register naming the recorded id classifies :unsupported too.
  (let ((os (%outcomes
             "(module definition $M
                (global (export \"glob\") (mut i32) (i32.const 0)))
              (module instance $I1 $M)
              (register \"I1\" $I1)")))
    (wasmdown.test:is (equal '(:unsupported :unsupported :unsupported)
                             os))))

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
