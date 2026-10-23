;;;; .wat text-format parser tests (src/text.lisp): parse -> encode ->
;;;; decode -> canonical-encode round-trips, decode-error cases, and
;;;; an execution smoke test per the text slice contract.

(defpackage #:wasmdown.test.text
  (:use #:cl #:wasmdown.types)
  (:nicknames #:wd.test.text))

(in-package #:wasmdown.test.text)

(defun %wat (s)
  (wasmdown.text:parse-wat s))

(defun %round-trip (s)
  "parse -> encode -> decode -> re-encode; the two byte vectors must be
EQUALP (canonical-encode agreement oracle)."
  (let* ((m (%wat s))
         (b1 (wasmdown.binary:encode-module m))
         (m2 (wasmdown.binary:decode-module b1))
         (b2 (wasmdown.binary:encode-module m2)))
    (wasmdown.test:is (equalp b1 b2))
    m))

(defun %instrs (m fidx)
  "The instr list of func FIDX of module M (as a list)."
  (coerce (wasm-func-body (aref (wasm-module-funcs m) fidx)) 'list))

(defun %find-br-table (m)
  "Immediates of the first br_table in func 0 of M, or NIL."
  (labels ((scan (instrs)
             (loop for i across instrs
                   thereis (or (and (eq (wasm-instr-op i) :br-table)
                                    (wasm-instr-imms i))
                               (scan (wasm-instr-body i))
                               (let ((eb (getf (wasm-instr-imms i)
                                               :else-body)))
                                 (and eb (scan eb)))))))
    (scan (coerce (wasm-func-body (aref (wasm-module-funcs m) 0))
                  'vector))))

(wasmdown.test:test text/empty-module
  (let ((m (%round-trip "(module)")))
    (wasmdown.test:is (zerop (length (wasm-module-funcs m))))
    (wasmdown.test:is (zerop (length (wasm-module-types m))))))

(wasmdown.test:test text/add-func
  (let ((m (%round-trip "(module (func (export \"add\") (param $a i32) (param $b i32) (result i32) local.get $a local.get $b i32.add))")))
    (wasmdown.test:is (= 1 (length (wasm-module-funcs m))))
    (let ((body (%instrs m 0)))
      (wasmdown.test:is (equal '(:local.get :local.get :i32.add :end)
                               (mapcar #'wasm-instr-op body)))
      (wasmdown.test:is (equal '(0 1)
                               (mapcar (lambda (i)
                                         (getf (wasm-instr-imms i) :local))
                                       (subseq body 0 2)))))))

(wasmdown.test:test text/block-labels-plain
  ;; plain-form block/loop with label ids and br out
  (%round-trip "(module (func (export \"count\") (param i32) (result i32)
      block $top (result i32)
        local.get 0
        br $top
      end))"))

(wasmdown.test:test text/if-else-folded
  ;; folded if with a label and condition operand before (then ...)
  (%round-trip "(module (func (export \"f\") (param i32) (result i32)
      (if $l (result i32) (local.get 0)
        (then i32.const 1)
        (else i32.const 2))))"))

(wasmdown.test:test text/br-table-folded
  ;; folded br_table: labels are the leading immediates, the trailing
  ;; list is the index operand
  (let ((m (%round-trip "(module (func (export \"pick\") (param i32) (result i32)
      block $a (result i32)
        block $b (result i32)
          block $c (result i32)
            (br_table $a $b $c $a (local.get 0))
            i32.const 0
          end
          i32.const 1
        end
        i32.const 2
      end))")))
    (let ((imm (%find-br-table m)))
      (wasmdown.test:is imm)
      (wasmdown.test:is (equalp #(2 1 0) (getf imm :targets)))
      (wasmdown.test:is (= 2 (getf imm :default))))))

(wasmdown.test:test text/br-table-plain
  ;; plain-form br_table: label* default then the index operand
  (%round-trip "(module (func (export \"p\") (param i32)
      block $a
        block $b
          local.get 0
          br_table $a $b $a
        end
      end))"))

(wasmdown.test:test text/call-indirect
  (let ((m (%round-trip "(module
      (type $ii (func (param i32) (result i32)))
      (table 2 funcref)
      (func $f (type $ii) (i32.const 7))
      (elem (i32.const 0) $f)
      (func (export \"call\") (param i32) (result i32)
        (call_indirect (type $ii) (local.get 0))))")))
    (wasmdown.test:is (= 1 (length (wasm-module-tables m))))
    (wasmdown.test:is (= 1 (length (wasm-module-elems m))))))

(wasmdown.test:test text/elem-forms
  ;; passive, declarative and active element segments
  (let ((m (%round-trip "(module
      (func $f)
      (table 10 funcref)
      (elem funcref (ref.func $f) (ref.null func))
      (elem $p func $f $f)
      (elem declare func $f)
      (elem (table 0) (i32.const 0) func $f))")))
    (wasmdown.test:is (= 4 (length (wasm-module-elems m))))
    (let ((modes (mapcar #'wasm-elem-mode
                         (coerce (wasm-module-elems m) 'list))))
      (wasmdown.test:is (eq :passive (first (first modes))))
      (wasmdown.test:is (eq :passive (first (second modes))))
      (wasmdown.test:is (equal '(:declarative) (third modes)))
      (wasmdown.test:is (eq :active (first (fourth modes))))))
  (let ((m (%round-trip "(module (func $f) (table 4 funcref)
              (elem (table 0) (offset (i32.const 0)) funcref (ref.func $f))
              (elem funcref (ref.func $f)))")))
    (wasmdown.test:is (= 2 (length (wasm-module-elems m))))))

(wasmdown.test:test text/data-forms
  (let ((m (%round-trip "(module (memory 1)
      (data (i32.const 0) \"abc\")
      (data \"xyz\")
      (data (memory 0) (offset (i32.const 8)) \"d\"))")))
    (wasmdown.test:is (= 3 (length (wasm-module-datas m))))
    (let ((modes (mapcar #'wasm-data-mode
                         (coerce (wasm-module-datas m) 'list))))
      (wasmdown.test:is (eq :active (first (first modes))))
      (wasmdown.test:is (equal '(:passive) (second modes)))
      (wasmdown.test:is (eq :active (first (third modes)))))))

(wasmdown.test:test text/globals
  (let ((m (%round-trip "(module
      (global $g (mut i32) (i32.const 10))
      (global $c i64 (i64.const 20))
      (func (export \"bump\") (result i32)
        (global.set $g (i32.add (global.get $g) (i32.const 1)))
        (global.get $g)))")))
    (let ((gs (coerce (wasm-module-globals m) 'list)))
      (wasmdown.test:is (= 2 (length gs)))
      (wasmdown.test:is (eq :i32 (wasm-global-type (first gs))))
      (wasmdown.test:is (wasm-global-mutable (first gs)))
      (wasmdown.test:is (eq :i64 (wasm-global-type (second gs))))
      (wasmdown.test:is (not (wasm-global-mutable (second gs)))))))

(wasmdown.test:test text/memory-min-max
  (let ((m (%round-trip "(module (memory 1 7))")))
    (let ((lim (wasm-memory-limits (aref (wasm-module-mems m) 0))))
      (wasmdown.test:is (= 1 (wasm-limits-min lim)))
      (wasmdown.test:is (= 7 (wasm-limits-max lim)))
      (wasmdown.test:is (wasm-limits-has-max-p lim)))))

(wasmdown.test:test text/table-min-max
  (let ((m (%round-trip "(module (table 2 5 funcref) (func $f) (elem (i32.const 0) $f))")))
    (let ((lim (wasm-table-limits (aref (wasm-module-tables m) 0))))
      (wasmdown.test:is (= 2 (wasm-limits-min lim)))
      (wasmdown.test:is (= 5 (wasm-limits-max lim))))))

(wasmdown.test:test text/float-bit-exactness
  ;; 1.5 -> exact 0x3FC00000 (f32) / 0x3FF8000000000000 (f64)
  (let ((m (%round-trip "(module (func (result f32) (f32.const 1.5)))")))
    (wasmdown.test:is (= #x3FC00000
                         (loop for i across (wasm-func-body
                                            (aref (wasm-module-funcs m) 0))
                               thereis (and (eq (wasm-instr-op i) :f32.const)
                                            (getf (wasm-instr-imms i)
                                                  :bits))))))
  (let ((m (%round-trip "(module (func (result f64) (f64.const 1.5)))")))
    (wasmdown.test:is (= #x3FF8000000000000
                         (loop for i across (wasm-func-body
                                            (aref (wasm-module-funcs m) 0))
                               thereis (and (eq (wasm-instr-op i) :f64.const)
                                            (getf (wasm-instr-imms i)
                                                  :bits))))))
  ;; hex float 0x1.921fb6p+2 as f32 = 0x40C90FDB (2*pi)
  (let ((m (%round-trip "(module (func (result f32) (f32.const 0x1.921fb6p+2)))")))
    (wasmdown.test:is (= #x40C90FDB
                         (loop for i across (wasm-func-body
                                            (aref (wasm-module-funcs m) 0))
                               thereis (and (eq (wasm-instr-op i) :f32.const)
                                            (getf (wasm-instr-imms i)
                                                  :bits))))))
  ;; nan:0x200000 payload preserved bit-exactly (f32)
  (let ((m (%round-trip "(module (func (result f32) (f32.const nan:0x200000)))")))
    (wasmdown.test:is (= (logior #x7F800000 #x200000)
                         (loop for i across (wasm-func-body
                                            (aref (wasm-module-funcs m) 0))
                               thereis (and (eq (wasm-instr-op i) :f32.const)
                                            (getf (wasm-instr-imms i)
                                                  :bits))))))
  ;; nan:0x400000 (quiet bit) and nan:0x7fffff (all-ones) are legal
  ;; payloads: WAT allows 0 < payload < 2^significand (float_literals)
  (let ((m (%round-trip "(module (func (result f32) (f32.const nan:0x400000)))")))
    (wasmdown.test:is (= (logior #x7F800000 #x400000)
                         (loop for i across (wasm-func-body
                                            (aref (wasm-module-funcs m) 0))
                               thereis (and (eq (wasm-instr-op i) :f32.const)
                                            (getf (wasm-instr-imms i)
                                                  :bits))))))
  (let ((m (%round-trip "(module (func (result f32) (f32.const -nan:0x7fffff)))")))
    (wasmdown.test:is (= (logior #xFF800000 #x7FFFFF)
                         (loop for i across (wasm-func-body
                                            (aref (wasm-module-funcs m) 0))
                               thereis (and (eq (wasm-instr-op i) :f32.const)
                                            (getf (wasm-instr-imms i)
                                                  :bits))))))
  ;; -inf
  (let ((m (%round-trip "(module (func (result f32) (f32.const -inf)))")))
    (wasmdown.test:is (= #xFF800000
                         (loop for i across (wasm-func-body
                                            (aref (wasm-module-funcs m) 0))
                               thereis (and (eq (wasm-instr-op i) :f32.const)
                                            (getf (wasm-instr-imms i)
                                                  :bits)))))))

(wasmdown.test:test text/i32-const-negative-wraps
  ;; i32.const -1 -> (:value -1): :value is the SIGNED literal
  ;; (docs/CONTRACTS.md); the binary decoder reads s32 and the
  ;; encoder wraps to unsigned at encode time.
  (let ((m (%round-trip "(module (func (export \"m1\") (result i32) (i32.const -1)))")))
    (wasmdown.test:is (= -1
                         (loop for i across (wasm-func-body
                                            (aref (wasm-module-funcs m) 0))
                               thereis (and (eq (wasm-instr-op i) :i32.const)
                                            (getf (wasm-instr-imms i)
                                                  :value)))))))

(wasmdown.test:test text/flat-and-folded
  ;; the same module body in both textual forms encodes identically
  (let* ((flat (%wat "(module (func (export \"x\") (param i32) (result i32)
                                local.get 0 i32.const 3 i32.add))"))
         (folded (%wat "(module (func (export \"x\") (param i32) (result i32)
                                (i32.add (local.get 0) (i32.const 3))))")))
    (wasmdown.test:is (equalp (wasmdown.binary:encode-module flat)
                              (wasmdown.binary:encode-module folded)))))

(wasmdown.test:test text/inline-import-export
  ;; (func (import ...) ...) and (func (export ...) ...) abbreviations
  (let ((m (%round-trip "(module
      (func $imp (import \"m\" \"f\") (param i32))
      (func (export \"loc\") (param i32) (result i32) (local.get 0)))")))
    (wasmdown.test:is (= 1 (length (wasm-module-imports m))))
    (wasmdown.test:is (eq :func (wasm-import-kind
                                (aref (wasm-module-imports m) 0))))
    (wasmdown.test:is (equal "m" (wasm-import-module
                                  (aref (wasm-module-imports m) 0))))
    (wasmdown.test:is (= 1 (length (wasm-module-funcs m)))))
  ;; table/memory/global export abbreviations
  (let ((m (%round-trip "(module
      (table (export \"t\") 2 funcref)
      (memory (export \"mem\") 1)
      (global (export \"g\") i32 (i32.const 5)))")))
    (wasmdown.test:is (= 1 (length (wasm-module-tables m))))
    (wasmdown.test:is (= 1 (length (wasm-module-mems m))))
    (wasmdown.test:is (= 1 (length (wasm-module-globals m))))))

(wasmdown.test:test text/imports-forms
  (let ((m (%round-trip "(module
      (import \"m\" \"f\" (func $f (param i32)))
      (import \"m\" \"t\" (table 1 2 funcref))
      (import \"m\" \"mem\" (memory 3))
      (import \"m\" \"g\" (global i32)))")))
    (wasmdown.test:is (= 4 (length (wasm-module-imports m))))
    (wasmdown.test:is (eq :func (wasm-import-kind
                                 (aref (wasm-module-imports m) 0))))
    (wasmdown.test:is (eq :table (wasm-import-kind
                                  (aref (wasm-module-imports m) 1))))
    (wasmdown.test:is (eq :memory (wasm-import-kind
                                   (aref (wasm-module-imports m) 2))))
    (wasmdown.test:is (eq :global (wasm-import-kind
                                   (aref (wasm-module-imports m) 3))))))

(wasmdown.test:test text/offset-align-memargs
  ;; offset=/align= lex correctly inside folded loads
  (let ((m (%round-trip "(module (memory 1)
               (func (export \"l\") (param i32) (result i32)
                 (i32.load8_u offset=4 align=1 (local.get 0))))")))
    (let ((imm (loop for i across (wasm-func-body
                                   (aref (wasm-module-funcs m) 0))
                     thereis (and (eq (wasm-instr-op i) :i32.load8_u)
                                  (wasm-instr-imms i)))))
      (wasmdown.test:is imm)
      (wasmdown.test:is (= 4 (getf imm :offset)))
      (wasmdown.test:is (= 0 (getf imm :align))))))

(wasmdown.test:test text/decode-errors
  ;; unclosed paren
  (wasmdown.test:assert-raises wasmdown:decode-error
    (%wat "(module (func"))
  ;; bad string escape
  (wasmdown.test:assert-raises wasmdown:decode-error
    (%wat "(module (import \"a\\q\" \"b\" (func)))"))
  ;; unknown opcode
  (wasmdown.test:assert-raises wasmdown:decode-error
    (%wat "(module (func i32.bogusop))"))
  ;; label out of scope
  (wasmdown.test:assert-raises wasmdown:decode-error
    (%wat "(module (func (br $nowhere)))"))
  ;; unknown $type
  (wasmdown.test:assert-raises wasmdown:decode-error
    (%wat "(module (func (type $missing)))"))
  ;; duplicate $id
  (wasmdown.test:assert-raises wasmdown:decode-error
    (%wat "(module (func $f) (func $f))"))
  ;; unknown module field
  (wasmdown.test:assert-raises wasmdown:decode-error
    (%wat "(module (bogus))")))

(wasmdown.test:test text/execution-smoke
  ;; parse add(a,b) -> validate -> instantiate -> invoke 40 2 = 42
  (let* ((m (%wat "(module (func (export \"add\") (param $a i32) (param $b i32) (result i32)
                                local.get $a local.get $b i32.add))"))
         (inst (wasmdown:instantiate m)))
    (wasmdown.test:is (equal '(42) (wasmdown:invoke inst "add" 40 2)))))

(wasmdown.test:test text/named-elem-resolution
  ;; (elem $id ...) registers the id: elem.drop $id resolves to the
  ;; elem segment index.
  (let ((m (%round-trip "(module
      (table 4 funcref)
      (func $f)
      (elem $e (i32.const 0) $f)
      (func (result i32)
        (table.size 0)
        (elem.drop $e)
        (drop)))")))
    (wasmdown.test:is (= 1 (length (wasm-module-elems m))))
    (let* ((body (wasm-func-body (aref (wasm-module-funcs m) 1)))
           (ed (find :elem.drop body :key #'wasmdown.types:wasm-instr-op)))
      (wasmdown.test:is ed)
      (wasmdown.test:is (= 0 (getf (wasmdown.types:wasm-instr-imms ed) :elem))))))

(wasmdown.test:test text/parse-wat-file
  ;; parse-wat-file reads a UTF-8 file and parses it as WAT.
  (let ((path (merge-pathnames ".tmp-parse-wat-file.wat"
                                (make-pathname :directory (pathname-directory *load-truename*)))))
    (unwind-protect
         (progn
           (with-open-file (out path :direction :output
                                :if-exists :supersede :external-format :utf-8)
             (write-line "(module (func (export \"answer\") (result i32) (i32.const 42)))" out))
           (let ((m (wasmdown.text:parse-wat-file path)))
             (wasmdown.test:is (= 1 (length (wasm-module-funcs m))))
             (let ((ops (map 'list #'wasmdown.types:wasm-instr-op
                            (wasm-func-body (aref (wasm-module-funcs m) 0)))))
               (wasmdown.test:is (find :i32.const ops)))))
      (when (probe-file path)
        (delete-file path)))))

(wasmdown.test:test text/multi-result-order
  ;; Regression (loop 3): (result i32 i64) must keep DECLARATION
  ;; order in the functype; the parser used to nreverse the collected
  ;; list, inverting every multi-result/multi-param signature
  ;; (br.wast::0 "expected I32, got I64" cluster).
  (let* ((m (%round-trip
             "(module (func (result i32 i64) (i32.const 1) (i64.const 7)))"))
         (ft (aref (wasm-module-types m)
                   (wasm-func-typeidx (aref (wasm-module-funcs m) 0)))))
    (wasmdown.test:is (equal '(:i32 :i64) (wasm-func-type-results ft))))
  ;; multi-param order (unnamed and named forms)
  (let* ((m (%round-trip
             "(module (func (param i32 i64) (result i64) (local.get 1)))"))
         (ft (aref (wasm-module-types m)
                   (wasm-func-typeidx (aref (wasm-module-funcs m) 0)))))
    (wasmdown.test:is (equal '(:i32 :i64) (wasm-func-type-params ft))))
  (let* ((m (%round-trip
             "(module (func (param $a i32) (param $b i64) (result i64) (local.get $b)))"))
         (ft (aref (wasm-module-types m)
                   (wasm-func-typeidx (aref (wasm-module-funcs m) 0)))))
    (wasmdown.test:is (equal '(:i32 :i64) (wasm-func-type-params ft))))
  ;; the whole module must also validate
  (let ((m (%wat "(module (func (result i32 i64) (block (result i32 i64) (return (br 0 (i32.const 1) (i64.const 7))))))")))
    (wasmdown.test:is (wasmdown:validate-module m))))

(wasmdown.test:test text/folded-call-indirect-operands
  ;; Regression (loop 3): folded (call_indirect (type $t) op...) used
  ;; to drop ALL operand expressions from the emitted instruction
  ;; stream (validation "operand stack underflow" cluster).
  (let* ((m (%round-trip
             "(module (type $t (func (param i32))) (table 1 funcref) (func (call_indirect (type $t) (i32.const 5) (i32.const 0))))"))
         (body (%instrs m 0)))
    (wasmdown.test:is
     (equal '(:i32.const :i32.const :call-indirect :end)
            (mapcar #'wasm-instr-op body)))
    (wasmdown.test:is (= 0 (getf (wasm-instr-imms (third body)) :table)))
    (wasmdown.test:is (= 0 (getf (wasm-instr-imms (third body)) :type))))
  ;; no-typeuse folded form still works and keeps the index operand
  (let* ((m (%round-trip
             "(module (table 1 funcref) (func (i32.const 0) (call_indirect)))"))
         (body (%instrs m 0)))
    (wasmdown.test:is
     (equal '(:i32.const :call-indirect :end)
            (mapcar #'wasm-instr-op body)))))

(wasmdown.test:test text/forward-type-reference
  ;; Regression (loop 5): (func (type $forward)) referencing a
  ;; (type $forward ...) defined LATER in the module (func.wast
  ;; module 1, imports.wast). Two-pass scheme: explicit (type)
  ;; fields register before any body parses.
  ;; (a) forward typeuse in a func
  (let* ((m (%round-trip
             "(module (func (type $f)) (type $f (func)) )"))
         (ft (aref (wasm-module-types m)
                   (wasm-func-typeidx (aref (wasm-module-funcs m) 0)))))
    (wasmdown.test:is (null (wasm-func-type-params ft)))
    (wasmdown.test:is (null (wasm-func-type-results ft))))
  ;; (b) forward type in call_indirect (the $t definition comes
  ;; AFTER the use; only ONE definition, or it is a genuine
  ;; duplicate-identifier error)
  (let* ((m (%round-trip
             "(module (table 1 funcref)
                        (func (call_indirect (type $t) (i32.const 0) (i32.const 0)))
                        (type $t (func (param i32) (result i32))))"))
         (body (%instrs m 0))
         (ci (find :call-indirect body :key #'wasm-instr-op)))
    (wasmdown.test:is (= 0 (getf (wasm-instr-imms ci) :type))))
  ;; (d) the func.wast module-1 slice parses and validates
  (let ((m (%wat "(module (type $sig (func)) (func $dummy) (func (type $sig)) (func (type $forward)) (type $forward (func)))")))
    (wasmdown.test:is (wasmdown:validate-module m))))

(wasmdown.test:test text/explicit-types-precede-implicit
  ;; Regression (loop 5): the suite's numeric pins (func.wast module 2)
  ;; prove explicit (type) fields occupy indices 0..n-1 in field order
  ;; and implicit find-or-add entries dedup-append AFTER them:
  ;;   (func $f (result f64))  -> implicit (result f64)
  ;;   (func $g (param i32))   -> REUSES the later explicit $t
  ;;   (type $t (func (param i32))) is index 0, implicit is index 1.
  ;; (c) mixed explicit+implicit ordering creates no duplicates.
  (let* ((m (%round-trip
             "(module (func $f (result f64) (f64.const 0)) (func $g (param i32)) (type $t (func (param i32))) (func $h (result f64) (f64.const 1)))"))
         (types (wasm-module-types m)))
    (wasmdown.test:is (= 2 (length types)))
    (wasmdown.test:is (equal '(:i32) (wasm-func-type-params (aref types 0))))
    (wasmdown.test:is (null (wasm-func-type-results (aref types 0))))
    (wasmdown.test:is (null (wasm-func-type-params (aref types 1))))
    (wasmdown.test:is (equal '(:f64) (wasm-func-type-results (aref types 1))))
    ;; $g's typeidx must be 0 (the explicit $t), $f/$h's must be 1
    (let ((idxs (map 'list #'wasm-func-typeidx (wasm-module-funcs m))))
      (wasmdown.test:is (equal '(1 0 1) idxs)))))

(wasmdown.test:test text/named-module-id-parse
  ;; Regression (loop 4): (module $M ...) — the optional $id after
  ;; the module keyword must be skipped, not parsed as a field.
  ;; Previously EVERY named module failed with "unknown module
  ;; field NIL" (linking.wast/elem.wast cascade).
  (let* ((m (%round-trip
             "(module $M (func (export \"f\") (result i32) (i32.const 1)))"))
         (inst (wasmdown:instantiate m)))
    (wasmdown.test:is (= 1 (length (wasm-module-funcs m))))
    (wasmdown.test:is (equal '(1) (wasmdown:invoke inst "f")))))

(wasmdown.test:test text/simd-substring-false-positive
  ;; Regression (loop 4): hex literals like 0x80000000 and $x2-style
  ;; ids must NOT trip the SIMD unsupported-feature scan (bare
  ;; substring 'x8'/'x2' matching). The module is plain integer
  ;; code; classification must see NO SIMD.
  (let ((rs (wasmdown.wast:run-script
             "(module (func (export \"f\") (param $x2 i32) (result i32)
                            (i32.add (local.get $x2) (i32.const -0x80000000))))
              (assert_return (invoke \"f\" (i32.const 1)) (i32.const -2147483647))")))
    (wasmdown.test:is (every (lambda (r) (eq r :pass))
                             (map 'list #'wasmdown.wast:wast-result-outcome
                                  rs)))))

(wasmdown.test:test text/loop-param-blocktype-not-nil
  ;; Regression (loop 6, loop.wast 42->121 cascade): a blocktype
  ;; (loop/block (param i32) ...) with params but NO results must
  ;; resolve to (cons :typeidx n), NOT nil. The old cond branch
  ;; ((null results) nil) silently dropped the params, so every
  ;; multi-value param loop desynced the end-of-block stack check
  ;; ('1 extra'/'underflow') — loop.wast module 0 failed wholesale.
  (labels ((%find-loop (instrs)
             (loop for i across instrs
                   thereis (or (and (eq (wasm-instr-op i) :loop) i)
                               (%find-loop (wasm-instr-body i))))))
    (let* ((m (%wat "(module (func (export \"f\") (result i32) (block (result i32) (i32.const 0) (loop (param i32) (block (br 2 (i32.const 18))) (br 0 (i32.const 20))) (i32.const 19))))"))
           (loop (%find-loop (wasm-func-body (aref (wasm-module-funcs m) 0))))
           (bt (and loop (getf (wasm-instr-imms loop) :bt))))
      (wasmdown.test:is (>= (length (wasm-module-types m)) 2))
      (wasmdown.test:is (and (consp bt) (eq (car bt) :typeidx)))
      (wasmdown.test:is
       (equal '(:i32)
              (wasm-func-type-params
               (aref (wasm-module-types m) (cdr bt)))))
      (wasmdown:validate-module m)
      (let ((inst (wasmdown:instantiate m)))
        (wasmdown.test:is (equal '(18) (wasmdown:invoke inst "f")))))))

(wasmdown.test:test text/typeuse-ordering-malformed
  ;; Regression (loop 6): WAT grammar pins typeuse order
  ;; (type)? (param)* (result)*. block.wast/loop.wast/func.wast
  ;; assert_malformed batches ("unexpected token") require rejecting
  ;; (type) after (param)/(result) and (param) after (result).
  (dolist (bad
           '("(module (type $s (func (param i32) (result i32))) (func (i32.const 0) (loop (param i32) (type $s) (result i32))))"
             "(module (type $s (func (param i32) (result i32))) (func (i32.const 0) (loop (type $s) (result i32) (param i32))))"
             "(module (type $s (func (param i32) (result i32))) (func (i32.const 0) (loop (param i32) (result i32) (type $s))))"
             "(module (func (i32.const 0) (loop (result i32) (param i32))))"))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (wasmdown.text:parse-wat bad)))
  ;; the LEGAL order (type) (param)* (result)* keeps working
  ;; (loop.wast type-use func, stack.wast call_indirect typeuses)
  (wasmdown:validate-module
   (%wat "(module (type $s (func (param i32 f64 i32) (result i32 f64 i32))) (func (i32.const 0) (f64.const 0) (i32.const 0) (loop (type $s) (param i32) (param f64 i32) (result i32 f64) (result i32)) (drop) (drop) (drop)))"))
  (wasmdown:validate-module
   (%wat "(module (type $p (func)) (table 1 funcref) (func (block (call_indirect (type $p) (param) (result) (i32.const 0)))))")))

(wasmdown.test:test text/ref-is-null-keyword
  ;; Regression (loop 8): %kw interned ref.is_null VERBATIM as
  ;; :REF.IS_NULL while every consumer (binary decoder, validator,
  ;; interp, codegen) dispatches on the hyphenated :ref.is-null —
  ;; table_get.wast's whole module-0 failed validation with
  ;; 'unknown opcode REF.IS_NULL'. Structured-op hyphenation
  ;; (CONTRACTS.md) must cover ref.is_null like br_if/br_table/
  ;; call_indirect.
  (let* ((m (%round-trip
             "(module (table $t 1 externref)
                        (func (export \"n\") (param $i i32) (result i32)
                          (ref.is_null (table.get $t (local.get $i)))))"))
         (ops (map 'list #'wasm-instr-op (%instrs m 0))))
    (wasmdown.test:is (find :ref.is-null ops :test #'eq))
    (wasmdown.test:is (not (find :ref.is_null ops :test #'eq))))
  ;; the whole (minimal) table_get shape validates + executes
  (let ((inst (wasmdown:instantiate
               (%wat "(module (table $t 2 externref)
                        (func (export \"g\") (param $i i32) (result i32)
                          (ref.is_null (table.get $t (local.get $i)))))"))))
    (wasmdown.test:is (equal '(1) (wasmdown:invoke inst "g" 0)))))

(wasmdown.test:test text/multi-instr-offset-exprs
  ;; Regression (loop 9): const-expr contexts (offset/item) accept
  ;; multi-instruction folded sequences and empty exprs — they parse
  ;; and fail VALIDATION (arity/constness), not DECODE. data.wast
  ;; ::53-60 wanted phase VALIDATE.
  (wasmdown.test:assert-raises wasmdown:validation-error
    (wasmdown:validate-module
     (%wat "(module (memory 1) (data (offset (i32.const 0) (nop)) \"a\"))")))
  ;; legal abbreviation: any leading folded list is the offset
  (wasmdown:validate-module
   (%wat "(module (memory 1) (data (i32.const 0) \"a\"))")))

(wasmdown.test:test text/utf8-data-literals
  ;; Loop-10 byte-accurate strings: \\xx escapes are RAW BYTES in
  ;; data segments (spec); \\u{...} escapes carry the code point's
  ;; UTF-8 encoding. The old pin asserted the loop-9 interim
  ;; behavior (UTF-8 re-encoding of every char) — superseded.
  ;; \\e9 = one raw byte 0xE9:
  (let ((m (%wat "(module (memory 1) (data (i32.const 0) \"\\e9\"))")))
    (wasmdown:validate-module m)
    (let ((d (aref (wasmdown.types:wasm-module-datas m) 0)))
      (wasmdown.test:is (equalp #(233)
                                (wasmdown.types:wasm-data-bytes d)))))
  ;; \\u{e9} = e-acute's UTF-8 encoding (C3 A9):
  (let ((m (%wat "(module (memory 1) (data (i32.const 0) \"\\u{e9}\"))")))
    (wasmdown:validate-module m)
    (let ((d (aref (wasmdown.types:wasm-module-datas m) 0)))
      (wasmdown.test:is (equalp #(195 169)
                                (wasmdown.types:wasm-data-bytes d)))))
  ;; surrogate escapes REJECT at decode (utf8-invalid-encoding):
  (wasmdown.test:assert-raises wasmdown:decode-error
    (%wat "(module (memory 1) (data (i32.const 0) \"\\u{d800}\"))")))

(wasmdown.test:test text/blocktype-named-param-malformed
  ;; Regression (loop 12, block.wast ::59/::219/::220): blocktype
  ;; (param) items are anonymous valtype lists per the grammar
  ;; (parser.mly block_param_body: LPAR PARAM valtype_list RPAR);
  ;; (param $x i32) in a block/loop/if typeuse is 'unexpected token'
  ;; malformed text (folded and plain forms).
  (dolist (bad
           '("(module (func (i32.const 0) (block (param $x i32) (drop))))"
             "(module (func (param i32) (result i32) block (param $x i32) end))"
             "(module (func (param i32) (result i32) (block (param $x i32))))"))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (%wat bad)))
  ;; anonymous block params stay legal (multi-value).
  (wasmdown:validate-module
   (%wat "(module (func (i32.const 0) (block (param i32) (drop))))")))

(wasmdown.test:test text/blocktype-inline-type-mismatch-malformed
  ;; Regression (loop 12, block.wast ::60-63): when a blocktype gives
  ;; an explicit (type x) AND inline (param)/(result) items, the inline
  ;; functype must be syntactically EQUAL to the referenced type
  ;; (parser.mly inline_functype_explicit); a mismatch is the
  ;; 'inline function type' malformed case at DECODE.
  (dolist (bad
           '("(module (type $sig (func)) (func (block (type $sig) (result i32) (i32.const 0)) (unreachable)))"
             "(module (type $sig (func (param i32) (result i32))) (func (block (type $sig) (result i32) (i32.const 0)) (unreachable)))"
             "(module (type $sig (func (param i32) (result i32))) (func (i32.const 0) (block (type $sig) (param i32) (drop)) (unreachable)))"
             "(module (type $sig (func (param i32 i32) (result i32))) (func (i32.const 0) (block (type $sig) (param i32) (result i32)) (unreachable)))"))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (%wat bad)))
  ;; MATCHING inline declarations next to (type x) are legal.
  (wasmdown:validate-module
   (%wat "(module (type $sig (func (result i32))) (func (block (type $sig) (result i32) (i32.const 0)) (drop)))")))

(wasmdown.test:test text/int-const-range-malformed
  ;; Regression (loop 14, const.wast 'constant out of range' batch):
  ;; i32 accepts [-2^31, 2^32-1] (the s32/u32 encoding); i64 likewise.
  ;; Wider spellings are DECODE-time 'constant out of range' malformed
  ;; text, not silent bignum truncation.
  (dolist (bad
           '("(module (func (i32.const 0x100000000) drop))"
             "(module (func (i32.const -0x80000001) drop))"
             "(module (func (i32.const 4294967296) drop))"
             "(module (func (i32.const -2147483649) drop))"
             "(module (func (i64.const 0x10000000000000000) drop))"
             "(module (func (i64.const -0x8000000000000001) drop))"
             "(module (func (i64.const 18446744073709551616) drop))"
             "(module (func (i64.const -9223372036854775809) drop))"))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (%wat bad)))
  ;; Boundary spellings stay legal (the valid modules around each batch).
  (dolist (ok
           '("(module (func (i32.const 0xffffffff) drop))"
             "(module (func (i32.const -0x80000000) drop))"
             "(module (func (i64.const 18446744073709551615) drop))"
             "(module (func (i64.const -9223372036854775808) drop))"))
    (%wat ok)))

(wasmdown.test:test text/float-const-range-malformed
  ;; Regression (loop 14, const.wast 'constant out of range' batch):
  ;; a float literal that rounds to infinity overflows the target
  ;; format and is DECODE-time malformed text: f32 (0x1p128, 0x1.ffffffp127,
  ;; 1e39, the >2^128 decimal) and the f64 counterparts (0x1p1024,
  ;; 0x1.fffffffffffff8p1023, 1e309, the >2^1024 decimal).
  (dolist (bad
           '("(module (func (f32.const 0x1p128) drop))"
             "(module (func (f32.const -0x1p128) drop))"
             "(module (func (f32.const 0x1.ffffffp127) drop))"
             "(module (func (f32.const 1e39) drop))"
             "(module (func (f32.const 340282356779733661637539395458142568448) drop))"
             "(module (func (f64.const 0x1p1024) drop))"
             "(module (func (f64.const 0x1.fffffffffffff8p1023) drop))"
             "(module (func (f64.const 1e309) drop))"
             "(module (func (f64.const 269653970229347356221791135597556535197105851288767494898376215204735891170042808140884337949150317257310688430271573696351481990334196274152701320055306275479074865864826923114368235135583993416113802762682700913456874855354834422248712838998185022412196739306217084753107265771378949821875606039276187287552) drop))"))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (%wat bad)))
  ;; Largest finite spellings stay legal: exact max, RNE-up-to-max,
  ;; and sticky-tail hex forms (const.wast valid modules pin these).
  (dolist (ok
           '("(module (func (f32.const 0x1p127) drop))"
             "(module (func (f32.const 0x1.fffffep127) drop))"
             "(module (func (f32.const 0x1.fffffe7p127) drop))"
             "(module (func (f32.const 0x1.fffffefffffff8000000p127) drop))"
             "(module (func (f32.const 1e38) drop))"
             "(module (func (f64.const 0x1p1023) drop))"
             "(module (func (f64.const 0x1.fffffffffffffp1023) drop))"
             "(module (func (f64.const 0x1.fffffffffffff7p1023) drop))"
             "(module (func (f64.const 1e308) drop))"
             "(module (func (f64.const inf) drop))"
             "(module (func (f32.const nan:0x1) drop))"))
    (%wat ok)))

(wasmdown.test:test text/call-indirect-typeuse-malformed
  ;; Regression (loop 14, call_indirect.wast ::142-146 'unexpected
  ;; token' / 'inline function type' + func.wast ::108-111): a
  ;; call_indirect typeuse follows BLOCKTYPE grammar — an explicit
  ;; (type x) plus inline (param)/(result) items must be syntactically
  ;; equal to the referenced functype, inline params cannot be named,
  ;; and (type) must come first.
  (dolist (bad
           '("(module (type $sig (func)) (table 0 funcref) (func (result i32) (call_indirect (type $sig) (result i32) (i32.const 0))))"
             "(module (type $sig (func (param i32) (result i32))) (table 0 funcref) (func (result i32) (call_indirect (type $sig) (result i32) (i32.const 0))))"
             "(module (type $sig (func (param i32) (result i32))) (table 0 funcref) (func (result i32) (call_indirect (type $sig) (param i32) (i32.const 0))))"
             "(module (type $sig (func (param i32 i32) (result i32))) (table 0 funcref) (func (result i32) (call_indirect (type $sig) (param i32) (result i32) (unreachable))))"
             "(module (table 0 funcref) (func (call_indirect (param $x i32) (i32.const 0) (i32.const 0))))"
             "(module (type $sig (func (param i32) (result i32))) (table 0 funcref) (func (result i32) (call_indirect (param i32) (type $sig) (result i32) (i32.const 0) (i32.const 0))))"))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (%wat bad)))
  ;; Matching inline typeuse next to (type x), and bare inline
  ;; (param)/(result) (find-or-create), stay legal.
  (dolist (ok
           '("(module (type $sig (func (param i32) (result i32))) (table 0 funcref) (func (result i32) (call_indirect (type $sig) (param i32) (result i32) (i32.const 0) (i32.const 0))))"
             "(module (table 0 funcref) (func (call_indirect (param i32) (i32.const 0) (i32.const 0))))"))
    (%wat ok)))

(wasmdown.test:test text/func-inline-typeuse-malformed
  ;; Regression (loop 14, func.wast ::108-111 'inline function type'):
  ;; a (func ...) field's explicit (type x) plus inline
  ;; (param)/(result) items must be syntactically EQUAL to the
  ;; referenced functype — plain find-or-create does not apply.
  (dolist (bad
           '("(module (type $sig (func)) (func (type $sig) (result i32) (i32.const 0)))"
             "(module (type $sig (func (param i32) (result i32))) (func (type $sig) (result i32) (i32.const 0)))"
             "(module (type $sig (func (param i32) (result i32))) (func (type $sig) (param i32) (i32.const 0)))"
             "(module (type $sig (func (param i32 i32) (result i32))) (func (type $sig) (param i32) (result i32) (unreachable)))"))
    (wasmdown.test:assert-raises wasmdown:decode-error
      (%wat bad)))
  ;; Matching inline declarations (imports.wast p6) stay legal.
  (%wat "(module (type $t (func (param i32))) (func (export \"p6\") (import \"spectest\" \"print_i32\") (type $t) (param i32) (result)))"))

(wasmdown.test:test text/func-typeonly-param-local-indexing
  ;; Regression (loop 14, func.wast ::94-95 'result 0: got 42'): when
  ;; a func's signature comes from an explicit (type x) (no inline
  ;; (param $id ...) items), the running local-index counter must be
  ;; seeded with the type's param count BEFORE (local $id ...) items
  ;; register; otherwise (local $var i32) maps to the param's index.
  (let ((m (%wat "(module
  (type $sig (func (param i32) (result i32)))
  (func (export \"f\") (type $sig)
    (local $var i32)
    (local.get $var)))"))
        (fidx 0))
    (let* ((f (aref (wasm-module-funcs m) fidx))
           (body (coerce (wasm-func-body f) 'list))
           (lg (find :local.get body :key #'wasm-instr-op)))
      (wasmdown.test:is (= 1 (getf (wasm-instr-imms lg) :local))))
    ;; and executing f yields 0 (the local default), not the argument.
    (let ((inst (wasmdown:instantiate m)))
      (wasmdown.test:is (equal '(0) (wasmdown:invoke inst "f" 42))))))
