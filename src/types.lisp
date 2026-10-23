;;;; Decoded-module IR: the cross-slice data contract between binary,
;;;; validate, runtime, interp and codegen (PLAN.md §5.3 DecodedModule).
;;;;
;;;; Representation policy:
;;;; - Value types are the keywords :i32 :i64 :f32 :f64 :v128 and
;;;;   reference types as lists (:ref null|no-null heaptype-keyword).
;;;; - Instructions are records {op, imms, body, offset}:
;;;;     op     - opcode keyword, e.g. :i32.add, :br, :block
;;;;     imms   - plist of immediate values (labelidx etc.)
;;;;     body   - (vector instr) for structured ops only
;;;;     offset - absolute byte offset in the original binary (source map)
;;;; - Expressions are (vector instr), terminated by :end.
(in-package #:wasmdown.types)

(defstruct (wasm-instr (:constructor make-wasm-instr (op imms &optional body offset)))
  "One decoded instruction."
  (op :nop :type keyword)
  (imms nil :type list)
  (body #() :type (vector wasm-instr))
  (offset 0 :type (integer 0)))

(defstruct (wasm-func-type (:constructor make-wasm-func-type (params results)))
  "A function type: params and results are lists of value types."
  params
  results)

(defstruct (wasm-limits (:constructor make-wasm-limits (min &optional (max nil max-p) &aux (has-max max-p))))
  (min 0 :type (integer 0))
  (max nil :type (or null (integer 0)))
  (has-max nil :type boolean))

(defun wasm-limits-has-max-p (limits)
  "Exported alias for the HAS-MAX slot reader."
  (wasm-limits-has-max limits))

(defstruct (wasm-table (:constructor make-wasm-table (type limits)))
  "Table type: element reference type plus limits."
  type
  limits)

(defstruct (wasm-memory (:constructor make-wasm-memory (limits))
                        (:predicate wasm-memory-p))
  "Memory type: limits (memory64 excluded from this slice: 32-bit only)."
  limits)

(defstruct wasm-global
  "A global definition from the globals section."
  (type :i32 :type keyword)
  (mutable nil :type boolean)
  (init nil :type (vector wasm-instr)))

(defstruct (wasm-import (:constructor make-wasm-import (module field kind type)))
  "One import. KIND is :func/:table/:memory/:global; TYPE is the typeidx or type."
  module
  field
  kind
  type)

(defstruct (wasm-export (:constructor make-wasm-export (name kind index)))
  name
  kind
  index)

(defstruct (wasm-func (:constructor make-wasm-func (typeidx locals body offset)))
  "A defined function body from the code section."
  typeidx
  locals                                 ; list of (valtype . count)
  body                                   ; (vector instr)
  offset)                                ; byte offset of body start in binary

(defstruct (wasm-elem (:constructor make-wasm-elem (mode type inits &optional from-table-init)))
  "Element segment. MODE: (:active tableidx expr) | (:passive) | (:declarative).
TYPE is the reference valtype; INITS is a list of expressions (vectors of instr).
FROM-TABLE-INIT marks a segment desugared from a table field's inline
initializer expr (the function-references extended syntax): per the
reference its const exprs may only use IMPORTED globals (global.wast
::120 expects 'unknown global' at VALIDATION for a defined-global
global.get)."
  mode
  type
  inits
  from-table-init)

(defstruct (wasm-data (:constructor make-wasm-data (mode bytes)))
  "Data segment. MODE: (:active memidx expr) | (:passive)."
  mode
  bytes)

(defstruct wasm-custom
  "Custom section kept as inert bytes (PLAN.md §5.1)."
  (name "" :type string)
  (bytes #() :type (vector (unsigned-byte 8)))
  (offset 0 :type (integer 0)))


(defstruct (wasm-module (:constructor %make-wasm-module))
  "A fully decoded (not yet validated) module."
  (types #() :type (vector wasm-func-type))
  (imports #() :type (vector wasm-import))
  (funcs #() :type (vector wasm-func))
  (tables #() :type (vector wasm-table))
  (mems #() :type (vector wasm-memory))
  ;; tag section (id 13): vector of typeidx; tag i's type is
  ;; (aref types (svref tags i)) and MUST be a functype with empty
  ;; results (validated in wasmdown.validate:check-tags).
  (tags #() :type (vector (integer 0)))
  (globals #() :type (vector wasm-global))
  (exports #() :type (vector wasm-export))
  (start nil :type (or null (integer 0)))
  (elems #() :type (vector wasm-elem))
  (datas #() :type (vector wasm-data))
  (customs #() :type (vector wasm-custom))
  (data-count nil :type (or null (integer 0))))

(defun make-wasm-module (&key (types #()) (imports #()) (funcs #()) (tables #())
                             (mems #()) (tags #()) (globals #()) (exports #())
                             (start nil) (elems #()) (datas #()) (customs #())
                             (data-count nil))
  (%make-wasm-module :types types :imports imports :funcs funcs :tables tables
                     :mems mems :tags tags :globals globals :exports exports
                     :start start :elems elems :datas datas :customs customs
                     :data-count data-count))


(defun func-type-arity-matches-p (ft params results)
  "Function-type equality on lists of value types."
  (and (equal (wasm-func-type-params ft) params)
       (equal (wasm-func-type-results ft) results)))

(defun valtype-width (t_)
  (ecase t_
    (:i32 32) (:i64 64) (:f32 32) (:f64 64)
    ((:ref) (error "reference types are opaque"))))

(defun defaultable-valtype-p (t_)
  "Whether the default value exists for the type in this slice.
Numeric types and nullable refs default; non-nullable refs do not."
  (declare (ignore t_))
  t)
