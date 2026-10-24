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

(wasmdown.test:test wast/typed-ref-typeuse-implemented
  ;; Loop 19: typed function references are IMPLEMENTED. The loop-16
  ;; pin asserted :unsupported (honest then: the forms crashed
  ;; %valtype). Now: a (ref $t) local read before set is a REAL
  ;; validation error ('uninitialized local' — non-nullable typed
  ;; refs are not defaultable), the (ref 1) out-of-range typeuse
  ;; fails at VALIDATE ('unknown type'), and the param carrier
  ;; parses+validates cleanly.
  (let ((os (%outcomes
             "(assert_invalid
                (module (type $t (func))
                  (func $f (local $x (ref $t)) (drop (local.get $x))))
                \"uninitialized local\")")))
    (wasmdown.test:is (equal '(:pass) os)))
  ;; (ref <digit>) index spelling: out-of-range = 'unknown type' at
  ;; VALIDATE (ref.wast's assert_invalid batch).
  (let ((os (%outcomes
             "(assert_invalid
                (module (type (func (param (ref 1)))) )
                \"unknown type\")")))
    (wasmdown.test:is (equal '(:pass) os)))
  ;; named-param carrier (type-equivalence ::0 shape): implements.
  (let ((os (%outcomes
             "(module (type $t1 (func))
                (func $f1 (param $r (ref $t1)) (call $f1 (local.get $r))))")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/binary-typed-ref-valtype-decodable
  ;; Loop 19: 0x63/0x64 typed-ref valtype bytes are DECODABLE (0x63 =
  ;; (ref null ht), 0x64 = (ref ht); heap = s33, positive is a typeuse
  ;; index, negatives are abstract heaps). The loop-16 pin asserted
  ;; :unsupported for 0x63 (honest then: decode crashed). binary.wast
  ;; ::127's shape — a huge positive typeuse in a functype param —
  ;; now decodes and fails 'unknown type' at VALIDATE, the fixture's
  ;; own expected phase.
  (let ((os (%outcomes
             "(assert_invalid
                (module binary
                  \"\\00asm\\01\\00\\00\\00\"
                  \"\\01\\0a\\01\"
                  \"\\60\\01\\63\\80\\80\\80\\80\\08\\00\")
                \"unknown type\")")))
    (wasmdown.test:is (equal '(:pass) os))))

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

(wasmdown.test:test wast/return-call-ref-implemented
  ;; Loop 19: return_call_ref is IMPLEMENTED (typing: the ref operand
  ;; must be <: (ref null? $t) for the instruction's typeuse; results
  ;; must EQUAL the caller's declared results; interp goes through
  ;; the loop-18 tail-call trampoline). The positive shape declares
  ;; the callee (type $t) so ref.func's static type matches the
  ;; typeuse (spec: ref.func : (ref <callee-typeidx>) — an undeclared
  ;; func's auto-type would mismatch $t, honestly assert_invalid).
  (let ((os (%outcomes
             "(module (type $t (func (result i32)))
                      (elem declare func $f)
                      (func $f (type $t) (result i32) (i32.const 7))
                      (func (export \"g\") (result i32)
                        (return_call_ref $t (ref.func $f))))
              (assert_return (invoke \"g\") (i32.const 7))")))
    (wasmdown.test:is (equal '(:pass :pass) os))
    ;; the mismatched-typeuse shape rejects: the callee's signature
    ;; DIFFERS from $t ((result i64) vs (result i32)) — find-or-add
    ;; gives it a distinct typeidx, so ref.func's (ref (:type 1)) is
    ;; not <: (ref null (:type 0)). (An identical signature would
    ;; DEDUP onto $t's index and validate — not a mismatch.)
    (let ((os2 (%outcomes
                "(assert_invalid
                   (module (type $t (func (result i32)))
                     (elem declare func $f)
                     (func $f (result i64) (i64.const 7))
                     (func (export \"g\") (result i32)
                       (return_call_ref $t (ref.func $f))))
                   \"type mismatch\")")))
      (wasmdown.test:is (equal '(:pass) os2)))))

(wasmdown.test:test wast/tag-import-type-mismatch
  ;; Loop 18 EH pin 1 (imports.wast shapes): tag imports type-match
  ;; at LINK — (import "m" "tag" (func)) against a registered tag
  ;; export is 'incompatible import type' (assert_unlinkable),
  ;; and an unknown tag field import fails at LINK too.
  (let ((os (%outcomes
             "(module (tag (export \"tag\")))
              (register \"test\")
              (assert_unlinkable
                (module (import \"test\" \"tag\" (func)))
                \"incompatible import type\")
              (assert_unlinkable
                (module (tag (import \"test\" \"tag\") (param f32)))
                \"incompatible import type\")
              (assert_unlinkable
                (module (tag (import \"test\" \"unknown\")))
                \"unknown import\")")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass :pass) os))))

(wasmdown.test:test wast/try-table-catch-dispatch
  ;; Loop 18 EH pin 2 (instance.wast shapes): try_table catch
  ;; dispatch — a matching (catch $t l) branches to its label with
  ;; the tag's payload; catch_all catches anything; a non-matching
  ;; tag with only (catch $other l) is uncaught (traps); the _ref
  ;; clauses deliver an exnref. The catch_all clause record is
  ;; [kind label] — its label reads at SECOND, not THIRD (the
  ;; instance.wast ::5-14 fail block was exactly that bug).
  (let ((rs (wasmdown.wast:run-script
             "(module
                (tag $e (param i32))
                (tag $other)
                (func $thrower (param i32) (throw $e (local.get 0)))
                (func (export \"payload\") (result i32)
                  (block $h (result i32)
                    (try_table (catch $e $h)
                      (call $thrower (i32.const 42)))
                    (i32.const 0)))
                (func (export \"all\") (result i32)
                  (block $h2
                    (try_table (catch_all $h2)
                      (throw $other)))
                  (i32.const 7))
                (func (export \"miss\") (result i32)
                  (block $h3 (result i32)
                    (try_table (catch $e $h3)
                      (throw $other))
                    (i32.const 0))))
              (assert_return (invoke \"payload\") (i32.const 42))
              (assert_return (invoke \"all\") (i32.const 7))
              (assert_trap (invoke \"miss\") \"uncaught exception\")")))
    ;; module + three asserts
    (wasmdown.test:is (equal '(:pass :pass :pass :pass)
                             (map 'list #'wasmdown.wast:wast-result-outcome
                                  rs)))))

(wasmdown.test:test wast/definition-instance-generativity
  ;; Loop 18 pin 3 (instance.wast): instantiation is generative —
  ;; two instances of one definition have DISTINCT state: writing
  ;; through one import does not read through the other.
  (let ((rs (wasmdown.wast:run-script
             "(module definition $M
                (global (export \"glob\") (mut i32) (i32.const 0)))
              (module instance $I1 $M)
              (module instance $I2 $M)
              (register \"I1\" $I1)
              (register \"I2\" $I2)
              (module
                (import \"I1\" \"glob\" (global $g1 (mut i32)))
                (import \"I2\" \"glob\" (global $g2 (mut i32)))
                (func (export \"both\") (result i32)
                  (global.set $g1 (i32.const 1))
                  (global.get $g2)))
              (assert_return (invoke \"both\") (i32.const 0))")))
    (wasmdown.test:is (equal '(:pass :pass :pass :pass :pass :pass :pass)
                             (map 'list #'wasmdown.wast:wast-result-outcome
                                  rs)))))

(wasmdown.test:test wast/tag-type-results-empty
  ;; Loop 18 pin 4: a tag's type must be a function type with EMPTY
  ;; results — (tag (param i32) (result i32)) is a VALIDATION error,
  ;; never decode.
  (let ((os (%outcomes
             "(assert_invalid
                (module (tag $bad (param i32) (result i32)))
                \"tag type must be empty\")
              (assert_invalid
                (module (type $t (func (param i32) (result i32)))
                        (tag $bad2 (type $t)))
                \"tag type must be empty\")")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

(wasmdown.test:test wast/binary-eh-roundtrip
  ;; Loop 18 pin 5: a module with a tag section, throw and try_table
  ;; encodes to bytes and decodes back to the SAME bytes (canonical
  ;; section order; tag section id 13 after data).
  (let* ((m (wasmdown.text:parse-wat
             "(module (tag $e (param i32))
                      (func $t (param i32) (throw $e (local.get 0)))
                      (func (export \"c\") (result i32)
                        (block $h (result i32)
                          (try_table (catch $e $h) (call $t (i32.const 5)))
                          (i32.const 0))))"))
         (b1 (wasmdown.binary:encode-module m))
         (m2 (wasmdown.binary:decode-module b1))
         (b2 (wasmdown.binary:encode-module m2)))
    (wasmdown.test:is (equalp b1 b2))))

(wasmdown.test:test wast/fac-rec-exhaustion-guard
  ;; Loop 18 pin 6 (fac.wast): deep non-tail recursion traps
  ;; :call-stack-exhausted — the guard must fire BEFORE the host
  ;; control stack faults. The limit was re-measured after the
  ;; tail-call trampoline + EH handler-bind grew per-invoke host
  ;; frames (ceiling ~1400 at the 2MB default control stack; limit
  ;; 1000 with margin; the suite lane also runs --control-stack-size
  ;; 16). This pin would catch a per-frame cost regression.
  (let ((os (%outcomes
             "(module (func (export \"fac-rec\") (param i64) (result i64)
                (if (result i64) (i64.eq (local.get 0) (i64.const 0))
                  (then (i64.const 1))
                  (else (i64.mul (local.get 0)
                          (call 0 (i64.sub (local.get 0)
                                           (i64.const 1))))))))
              (assert_exhaustion (invoke \"fac-rec\" (i64.const 1073741824))
                                \"call stack exhausted\")")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

;;; ================================================================
;;; Loop 19, slice A pins: typed function references — text grammar,
;;; validation lattice, definite initialization, the five opcodes.
;;; Oracle: ref.wast, ref_null.wast, local_init.wast, call_ref.wast,
;;; br_on_null.wast, br_on_non_null.wast, ref_as_non_null.wast,
;;; select.wast, return_call_ref.wast.

(wasmdown.test:test wast/typed-ref-grammar-accepts
  ;; ref.wast module 0: the full (ref null? heaptype) grammar in param
  ;; position — abstract heaps, typeuse indices (both spellings) and
  ;; the null forms, mixed with the legacy keywords.
  (let ((os (%outcomes
             "(module (type $t (func))
                (func (param funcref externref (ref func) (ref extern)
                                  (ref 0) (ref $t) (ref 0) (ref $t)
                                  (ref null func) (ref null extern)
                                  (ref null 0) (ref null $t))))")))
    (wasmdown.test:is (equal '(:pass) os)))
  ;; ref.wast's 'unknown type' assert_invalid batch: every position
  ;; (param/result/local/global/table/elem/blocktype/select) rejects
  ;; an out-of-range typeuse heap at VALIDATION — the parser keeps
  ;; the raw index, check-valtype reports it.
  (let ((os (%outcomes
             "(assert_invalid (module (type $a (func (param (ref 1))))) \"unknown type\")
              (assert_invalid (module (type $a (func (result (ref 1))))) \"unknown type\")
              (assert_invalid (module (global $g (ref null 1) (ref.null 1))) \"unknown type\")
              (assert_invalid (module (table $t 10 (ref null 1))) \"unknown type\")
              (assert_invalid (module (elem $e (ref 1))) \"unknown type\")
              (assert_invalid (module (func $f (param (ref 1)))) \"unknown type\")
              (assert_invalid (module (func $f (result (ref 1)))) \"unknown type\")
              (assert_invalid (module (func $f (local (ref null 1)))) \"unknown type\")
              (assert_invalid (module (func $f (drop (block (result (ref 1)) (unreachable))))) \"unknown type\")
              (assert_invalid (module (func $f (drop (loop (result (ref 1)) (unreachable))))) \"unknown type\")
              (assert_invalid (module (func $f (drop (if (result (ref 1)) (then) (else))))) \"unknown type\")
              (assert_invalid (module (func $f (drop (select (result (ref 1)) (unreachable))))) \"unknown type\")")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os))))

(wasmdown.test:test wast/typed-ref-null-spellings
  ;; ref_null.wast module 0 + module 1: every null spelling — the
  ;; abstract heaps, the shorthand bottoms (nullref/nullfuncref/
  ;; nullexternref/nullexnref/anyref), the typeuse forms, and the
  ;; lattice flows ((ref.null nofunc) initializes a (ref null $t)
  ;; global — the null bottom is below every typeuse in the bucket).
  (let ((os (%outcomes
             "(module (type $t (func))
                (func (export \"anyref\") (result anyref) (ref.null any))
                (func (export \"funcref\") (result funcref) (ref.null func))
                (func (export \"exnref\") (result exnref) (ref.null exn))
                (func (export \"externref\") (result externref) (ref.null extern))
                (func (export \"ref\") (result (ref null $t)) (ref.null $t))
                (global anyref (ref.null any))
                (global funcref (ref.null func))
                (global exnref (ref.null exn))
                (global externref (ref.null extern))
                (global (ref null $t) (ref.null $t)))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(module (type $t (func))
                (global $null nullref (ref.null none))
                (global $nullfunc nullfuncref (ref.null nofunc))
                (global $nullexn nullexnref (ref.null noexn))
                (global $nullextern nullexternref (ref.null noextern))
                (func (export \"anyref\") (result anyref) (global.get $null))
                (func (export \"nullref\") (result nullref) (global.get $null))
                (func (export \"funcref\") (result funcref) (global.get $nullfunc))
                (func (export \"ref\") (result (ref null $t)) (global.get $nullfunc))
                (global anyref (ref.null any))
                (global anyref (ref.null none))
                (global funcref (ref.null func))
                (global funcref (ref.null nofunc))
                (global exnref (ref.null exn))
                (global exnref (ref.null noexn))
                (global externref (ref.null extern))
                (global externref (ref.null noextern))
                (global nullref (ref.null none))
                (global nullfuncref (ref.null nofunc))
                (global nullexnref (ref.null noexn))
                (global nullexternref (ref.null noextern))
                (global (ref null $t) (ref.null $t))
                (global (ref null $t) (ref.null nofunc)))")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/local-definite-init
  ;; local_init.wast, the full battery: non-defaultable locals
  ;; ((ref extern)) require PROVEN initialization before local.get;
  ;; the ctrl-frame snapshot restores at every block end (a set inside
  ;; a block is not proven after the join — $uninit-after-end; the
  ;; if/else joins are as conservative as the entry — $uninit-in-else
  ;; and $uninit-from-if), while sets/tees at the CURRENT level carry
  ;; (get-after-set, get-after-tee, tee-init) and nested reads inside
  ;; the same block see the set (get-in-block-after-set).
  (let ((os (%outcomes
             "(module
                (func (export \"get-after-set\") (param $p (ref extern)) (result (ref extern))
                  (local $x (ref extern))
                  (local.set $x (local.get $p))
                  (local.get $x))
                (func (export \"get-after-tee\") (param $p (ref extern)) (result (ref extern))
                  (local $x (ref extern))
                  (drop (local.tee $x (local.get $p)))
                  (local.get $x))
                (func (export \"get-in-block-after-set\") (param $p (ref extern)) (result (ref extern))
                  (local $x (ref extern))
                  (local.set $x (local.get $p))
                  (block (result (ref extern)) (local.get $x))))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (dolist (script
           '("(assert_invalid
                (module (func $uninit (local $x (ref extern)) (drop (local.get $x))))
                \"uninitialized local\")"
             "(assert_invalid
                (module (func $uninit-after-end (param $p (ref extern))
                  (local $x (ref extern))
                  (block (local.set $x (local.get $p)) (drop (local.tee $x (local.get $p))))
                  (drop (local.get $x))))
                \"uninitialized local\")"
             "(assert_invalid
                (module (func $uninit-in-else (param $p (ref extern))
                  (local $x (ref extern))
                  (if (i32.const 0)
                    (then (local.set $x (local.get $p)))
                    (else (local.get $x)))))
                \"uninitialized local\")"
             "(assert_invalid
                (module (func $uninit-from-if (param $p (ref extern))
                  (local $x (ref extern))
                  (if (i32.const 0)
                    (then (local.set $x (local.get $p)))
                    (else (local.set $x (local.get $p))))
                  (drop (local.get $x))))
                \"uninitialized local\")"))
    (wasmdown.test:is (eq :pass (first (%outcomes script)))))
  (let ((os (%outcomes
             "(module
                (func (export \"tee-init\") (param $p (ref extern)) (result (ref extern))
                  (local $x (ref extern))
                  (drop (local.tee $x (local.get $p)))
                  (local.get $x)))")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/call-ref-typing
  ;; call_ref.wast: the callee's ref operand must be <: (ref null? $t)
  ;; for the instruction's OWN typeuse — ref.func's minimal type
  ;; ((ref <callee-typeidx>) via vctx-func-typeidxs) flows into
  ;; (ref $ii) locals/globals via the lattice, and a bare funcref (or
  ;; externref) operand is REJECTED (funcref is the bucket's nullable
  ;; TOP, not a subtype of a typeuse's ref).
  (let ((os (%outcomes
             "(module (type $ii (func (param i32) (result i32)))
                (func $apply (param $f (ref $ii)) (param $x i32) (result i32)
                  (call_ref $ii (local.get $x) (local.get $f)))
                (func $f (type $ii) (i32.mul (local.get 0) (local.get 0)))
                (func $g (type $ii) (i32.sub (i32.const 0) (local.get 0)))
                (elem declare func $f $g))")))
    (wasmdown.test:is (equal '(:pass) os)))
  ;; local.set of ref.func into a (ref null $ii) local, then call_ref
  ;; through it (call_ref.wast ::13-19's run shape).
  (let ((os (%outcomes
             "(module (type $ii (func (param i32) (result i32)))
                (func $f (type $ii) (i32.mul (local.get 0) (local.get 0)))
                (elem declare func $f)
                (func (export \"run\") (param $x i32) (result i32)
                  (local $rf (ref null $ii))
                  (local.set $rf (ref.func $f))
                  (call_ref $ii (local.get $x) (local.get $rf))))")))
    (wasmdown.test:is (equal '(:pass) os)))
  ;; the operand-rejection batch (::200-217).
  (let ((os (%outcomes
             "(assert_invalid
                (module (type $t (func))
                  (func $f (param $r funcref) (call_ref $t (local.get $r))))
                \"type mismatch\")
              (assert_invalid
                (module (type $t (func))
                  (func $f (param $r externref) (call_ref $t (local.get $r))))
                \"type mismatch\")")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os))))

(wasmdown.test:test wast/typed-select-join
  ;; select.wast join-funcnull: typed select's (result (ref null
  ;; func)) joins ref.func's minimal type with ref.null func — both
  ;; operands <: the declared result through the lattice.
  (let ((os (%outcomes
             "(module (type $t (func))
                (func $tf) (elem declare func $tf)
                (func (export \"join-funcnull\") (param i32) (result (ref null func))
                  (select (result (ref null func))
                    (ref.func $tf)
                    (ref.null func)
                    (local.get 0))))")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/br-on-null-typing
  ;; br_on_null: [t* (ref null ht)] -> [t* (ref ht)] fallthrough
  ;; (the non-null view feeds the call_ref), branch label [t*]; both
  ;; nullable and non-nullable operands accepted ((ref ht) <:
  ;; (ref null ht)); gc#516: the label check runs through the full
  ;; pop/push cycle, catching the two-funcref stack confusion.
  (let ((os (%outcomes
             "(module (type $t (func (result i32)))
                (func $nn (param $r (ref $t)) (result i32)
                  (block $l
                    (return (call_ref $t (br_on_null $l (local.get $r)))))
                  (i32.const -1))
                (func $n (param $r (ref null $t)) (result i32)
                  (block $l
                    (return (call_ref $t (br_on_null $l (local.get $r)))))
                  (i32.const -1))
                (elem func $f)
                (func $f (result i32) (i32.const 7)))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module (type $t (func))
                  (func $f (param (ref null $t)) (result funcref) (local.get 0))
                  (func (param funcref) (result funcref)
                    (ref.null $t)
                    (local.get 0)
                    (br_on_null 0)
                    (drop)
                    (call $f)))
                \"type mismatch\")")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/br-on-non-null-typing
  ;; br_on_non_null: [t* (ref null ht)] -> [t*] fallthrough, branch
  ;; label [t* (ref ht)] — the label's LAST slot is the operand's
  ;; non-null view (a block (result (ref $t)) with nothing else on
  ;; the stack validates); gc#516's two-funcref mirror.
  (let ((os (%outcomes
             "(module (type $t (func (result i32)))
                (func $nn (param $r (ref $t)) (result i32)
                  (call_ref $t
                    (block $l (result (ref $t))
                      (br_on_non_null $l (local.get $r))
                      (return (i32.const -1)))))
                (func $n (param $r (ref null $t)) (result i32)
                  (call_ref $t
                    (block $l (result (ref $t))
                      (br_on_non_null $l (local.get $r))
                      (return (i32.const -1)))))
                (elem func $f)
                (func $f (result i32) (i32.const 7)))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module (type $t (func))
                  (func $f (param (ref null $t)) (result funcref) (local.get 0))
                  (func (param funcref) (result funcref funcref)
                    (ref.null $t)
                    (local.get 0)
                    (br_on_non_null 0)
                    (call $f)
                    (local.get 0)))
                \"type mismatch\")")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/ref-as-non-null-typing
  ;; ref.as_non_null: [(ref null ht)] -> [(ref ht)]; a null operand
  ;; trapped (interp); the (ref.null $t)-into-(ref $t)-param mismatch
  ;; rejects at VALIDATION.
  (let ((os (%outcomes
             "(module (type $t (func (result i32)))
                (func $nn (param $r (ref $t)) (result i32)
                  (call_ref $t (ref.as_non_null (local.get $r))))
                (func $n (param $r (ref null $t)) (result i32)
                  (call_ref $t (ref.as_non_null (local.get $r))))
                (elem func $f)
                (func $f (result i32) (i32.const 7)))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module (type $t (func (result i32)))
                  (func $g (param $r (ref $t)) (drop (ref.as_non_null (local.get $r))))
                  (func (call $g (ref.null $t))))
                \"type mismatch\")")))
    (wasmdown.test:is (equal '(:pass) os)))
  ;; the abstract-heap forms (ref func)/(ref extern) with nullable
  ;; operands (ref_as_non_null.wast ::41-46).
  (let ((os (%outcomes
             "(module (type $t (func))
                (func (param $r (ref $t)) (drop (ref.as_non_null (local.get $r))))
                (func (param $r (ref func)) (drop (ref.as_non_null (local.get $r))))
                (func (param $r (ref extern)) (drop (ref.as_non_null (local.get $r)))))")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/return-call-ref-result-lattice
  ;; return_call_ref: the callee's results must be SUBTYPES of the
  ;; caller's declared results (return_call_ref.wast ::213-228's
  ;; t1/t2/t3/t4 lattice batch — (ref $t) results flow into (ref null
  ;; $t) and (ref null func) callers), the arity/i64-vs-i32 mismatches
  ;; reject, and a funcref operand rejects like call_ref's.
  (let ((os (%outcomes
             "(module (type $t (func))
                (type $t1 (func (result (ref $t))))
                (type $t2 (func (result (ref null $t))))
                (type $t3 (func (result (ref func))))
                (type $t4 (func (result (ref null func))))
                (elem declare func $f11 $f22 $f33 $f44)
                (func $f11 (result (ref $t)) (return_call_ref $t1 (ref.func $f11)))
                (func $f21 (result (ref null $t)) (return_call_ref $t1 (ref.func $f11)))
                (func $f22 (result (ref null $t)) (return_call_ref $t2 (ref.func $f22)))
                (func $f31 (result (ref func)) (return_call_ref $t1 (ref.func $f11)))
                (func $f33 (result (ref func)) (return_call_ref $t3 (ref.func $f33)))
                (func $f41 (result (ref null func)) (return_call_ref $t1 (ref.func $f11)))
                (func $f42 (result (ref null func)) (return_call_ref $t2 (ref.func $f22)))
                (func $f43 (result (ref null func)) (return_call_ref $t3 (ref.func $f33)))
                (func $f44 (result (ref null func)) (return_call_ref $t4 (ref.func $f44))))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module (type $t (func))
                  (type $t2 (func (result (ref null $t))))
                  (elem declare func $f22)
                  (func $f12 (result (ref $t)) (return_call_ref $t2 (ref.func $f22)))
                  (func $f22 (result (ref null $t)) (return_call_ref $t2 (ref.func $f22))))
                \"type mismatch\")
              (assert_invalid
                (module (type $type-result-more (func (result i32 i32)))
                  (func $type-result-more (param (ref $type-result-more)) (result i32)
                    local.get 0
                    return_call_ref $type-result-more))
                \"type mismatch\")
              (assert_invalid
                (module (type $type-result-i64-vs-i32 (func (result i64)))
                  (func $f (param (ref $type-result-i64-vs-i32)) (result i32)
                    local.get 0
                    return_call_ref $type-result-i64-vs-i32))
                \"type mismatch\")
              (assert_invalid
                (module (type $t (func))
                  (func $f (param $r funcref) (return_call_ref $t (local.get $r))))
                \"type mismatch\")")))    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os))))

;;; ---------------------------------------------------------------
;;; Loop 20: recursion groups, canonicalization, iso-recursive
;;; equivalence (the wasmdown.types engine + validate canon map).
;;; Fixture shapes verified against the engine; the rec-carrying
;;; GC composite (struct/array) fixtures stay out of scope at the
;;; classifier, so every shape below is FUNC-only.

(wasmdown.test:test wast/rec-group-parse-validate
  ;; (rec ...) groups parse and validate: mutual forward refs INSIDE
  ;; a group are legal (the group is the unit of recursion; member
  ;; typeidxs consecutive in field order, one shared group object).
  ;; NEGATIVE: the same mutual recursion WITHOUT the (rec ...) wrapper
  ;; is 'unknown type' at VALIDATION (type-equivalence.wast ::76-82;
  ;; type-rec.wast ::21-33) — a non-rec forward reference to a later
  ;; typeidx is out of scope by the group-start bound.
  (let ((os (%outcomes
             "(module
                (rec (type $t1 (func (param i32 (ref $t2))))
                     (type $t2 (func (param i32 (ref $t1)))))
                (func $f (type $t1)))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module
                  (type $t1 (func (param (ref $t2))))
                  (type $t2 (func (param (ref $t1)))))
                \"unknown type\")")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module
                  (rec (type $t1 (func (param (ref $t2)))))
                  (rec (type $t2 (func))))
                \"unknown type\")")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/canonicalization-simple-indirect
  ;; Canonicalization (canon map: typeidx -> min equivalent): two
  ;; structurally-equal functypes are ONE canonical type, so values
  ;; flow through each other's (ref $t) slots — the simple named-param
  ;; twins (type-equivalence.wast ::4-11), the indirect nested-ref
  ;; equivalence (::16-24), and the RUNTIME shape of the 'Semantic
  ;; types (run time)' section: call_indirect through the twin types
  ;; (::89-102's cross-typed table calls).
  (let ((os (%outcomes
             "(module
                (type $t1 (func (param f32 f32) (result f32)))
                (type $t2 (func (param $x f32) (param $y f32) (result f32)))
                (func $f1 (param $r (ref $t1)) (call $f2 (local.get $r)))
                (func $f2 (param $r (ref $t2)) (call $f1 (local.get $r))))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(module
                (type $s0 (func (param i32) (result f32)))
                (type $s1 (func (param i32 (ref $s0)) (result (ref $s0))))
                (type $s2 (func (param i32 (ref $s0)) (result (ref $s0))))
                (type $t1 (func (param (ref $s1)) (result (ref $s2))))
                (type $t2 (func (param (ref $s2)) (result (ref $s1))))
                (func $f1 (param $r (ref $t1)) (call $f2 (local.get $r)))
                (func $f2 (param $r (ref $t2)) (call $f1 (local.get $r))))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(module
                (type $t1 (func (param f32 f32)))
                (type $t2 (func (param $x f32) (param $y f32)))
                (func $f1 (type $t1))
                (func $f2 (type $t2))
                (table funcref (elem $f1 $f2))
                (func (export \"run\")
                  (call_indirect (type $t1) (f32.const 1) (f32.const 2) (i32.const 1))
                  (call_indirect (type $t2) (f32.const 1) (f32.const 2) (i32.const 0))))
              (assert_return (invoke \"run\"))")))
    (wasmdown.test:is (equal '(:pass :pass) os))))

(wasmdown.test:test wast/iso-recursive-two-member-groups
  ;; The iso-recursive rule: two rec groups are equivalent iff their
  ;; members match positionally with internal refs mapped to the
  ;; other group's same position (the recursive hypothesis). The
  ;; isomorphic two-member case (type-equivalence.wast ::49-71's
  ;; $f0-$f3 chain) validates; NEGATIVE: the arity mismatch (2-member
  ;; vs 3-member group) REJECTS — group shape is part of the type
  ;; (type-rec.wast ::114-122's family).
  (let ((os (%outcomes
             "(module
                (rec
                  (type $t0 (func (param i32 (ref $t1))))
                  (type $t1 (func (param i32 (ref $t0)))))
                (rec
                  (type $t2 (func (param i32 (ref $t3))))
                  (type $t3 (func (param i32 (ref $t2)))))
                (func $f0 (param $r (ref $t0))
                  (call $f2 (local.get $r)))
                (func $f1 (param $r (ref $t1))
                  (call $f3 (local.get $r)))
                (func $f2 (param $r (ref $t2))
                  (call $f0 (local.get $r)))
                (func $f3 (param $r (ref $t3))
                  (call $f1 (local.get $r))))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module
                  (rec
                    (type $t0 (func (param i32 (ref $t1))))
                    (type $t1 (func (param i32 (ref $t0)))))
                  (rec
                    (type $a (func (param i32 (ref $b))))
                    (type $b (func (param i32 (ref $a))))
                    (type $c (func)))
                  (func (param $r (ref $t0))
                    (call $f (local.get $r)))
                  (func $f (param $r (ref $a)) (call $f (local.get $r))))
                \"type mismatch\")")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/iso-internal-vs-external-ref
  ;; The internal/external separation (the heart of the iso rule): a
  ;; ref whose target is in the compared member's OWN group maps
  ;; POSITIONALLY; a ref to a type outside the group is compared by
  ;; index pair. A MIX — one side internal, the other external — is
  ;; NOT equivalent even when the referenced type is itself the same
  ;; (the func-only analogue of type-rec.wast ::93-122: a rolled-up
  ;; group never equals its unrolled copy). $a's (ref $b) is
  ;; internal; $c's (ref $b) is external — not equivalent. The
  ;; isomorphic control (both internal, same positions) validates.
  (let ((os (%outcomes
             "(module
                (rec (type $a (func (param (ref $b)))) (type $b (func)))
                (rec (type $c (func (param (ref $d)))) (type $d (func)))
                (func $f (type $a))
                (global (ref $c) (ref.func $f)))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module
                  (rec (type $a (func (param (ref $b)))) (type $b (func)))
                  (rec (type $c (func (param (ref $b)))) (type $d (func)))
                  (func $f (type $a))
                  (global (ref $c) (ref.func $f)))
                \"type mismatch\")")))
    (wasmdown.test:is (equal '(:pass) os))))

(wasmdown.test:test wast/cross-module-func-type-match
  ;; Link-time equivalence: func-type-match-p compares the importing
  ;; module's declared type against the PROVIDING module's func
  ;; instance type structurally (cross-module typeuse indices are
  ;; meaningless positionally — the iso rule runs with each side's own
  ;; group context). Equal-signature types across modules LINK
  ;; (type-equivalence.wast ::191-228's N family), the recursive iso
  ;; case links too (::231-241's Mr1); NEGATIVE: a non-singleton rec
  ;; member does NOT match a plain singleton functype across modules
  ;; (the func-only analogue of type-rec.wast ::148-162's
  ;; 'incompatible import type').
  (let ((os (%outcomes
             "(module
                (type $s0 (func (param i32) (result f32)))
                (type $s1 (func (param i32 (ref $s0)) (result (ref $s0))))
                (type $s2 (func (param i32 (ref $s0)) (result (ref $s0))))
                (type $t1 (func (param (ref $s1)) (result (ref $s2))))
                (type $t2 (func (param (ref $s2)) (result (ref $s1))))
                (func (export \"f1\") (param (ref $t1)))
                (func (export \"f2\") (param (ref $t1))))
              (register \"N\")
              (module
                (type $s0 (func (param i32) (result f32)))
                (type $s1 (func (param i32 (ref $s0)) (result (ref $s0))))
                (type $s2 (func (param i32 (ref $s0)) (result (ref $s0))))
                (type $t1 (func (param (ref $s1)) (result (ref $s2))))
                (type $t2 (func (param (ref $s2)) (result (ref $s1))))
                (func (import \"N\" \"f1\") (param (ref $t1)))
                (func (import \"N\" \"f1\") (param (ref $t2))))")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os)))
  (let ((os (%outcomes
             "(module $Mr1
                (rec (type $t1 (func (param i32 (ref $t1)))))
                (func (export \"f\") (param (ref $t1))))
              (register \"Mr1\" $Mr1)
              (module
                (rec (type $t2 (func (param i32 (ref $t2)))))
                (func (import \"Mr1\" \"f\") (param (ref $t2))))")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os)))
  (let ((os (%outcomes
             "(module $M
                (rec (type $f1 (func)) (type $g (func)))
                (func (export \"f\") (type $f1)))
              (register \"M\" $M)
              (assert_unlinkable
                (module
                  (type $f2 (func))
                  (func (import \"M\" \"f\") (type $f2)))
                \"function type mismatch\")")))
    (wasmdown.test:is (every (lambda (o) (eq o :pass)) os))))

(wasmdown.test:test wast/implicit-typeuse-singleton-unification
  ;; %find-or-add-type's rec rule: an IMPLICIT typeuse (bare
  ;; (param)/(result), no (type x)) unifies with an existing type only
  ;; when that type's rec group is a SINGLETON of equal signature —
  ;; the func-only analogue of type-rec.wast ::197-202 (accept: the
  ;; plain (func (param (ref $s))) typeuse IS the singleton rec $t)
  ;; vs ::216-226 (reject: a member of a NON-singleton group never
  ;; absorbs the implicit use).
  (let ((os (%outcomes
             "(module
                (type $s (func (param i32)))
                (rec (type $t (func (param (ref $s)))))
                (func $f (param (ref $s)))
                (global (ref $t) (ref.func $f)))")))
    (wasmdown.test:is (equal '(:pass) os)))
  (let ((os (%outcomes
             "(assert_invalid
                (module
                  (rec (type $x (func (param i32))) (type $t (func)))
                  (func $f)
                  (global (ref $t) (ref.func $f)))
                \"type mismatch\")")))
    (wasmdown.test:is (equal '(:pass) os))))
