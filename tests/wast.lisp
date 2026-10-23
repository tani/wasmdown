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
