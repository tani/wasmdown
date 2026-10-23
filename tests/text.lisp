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
