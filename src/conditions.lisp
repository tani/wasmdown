;;;; Public condition taxonomy (docs/design/PLAN.md §11.1).
;;;; Errors carry structured data, never raw strings only.

(in-package #:wasmdown)

(define-condition wasmdown-error (error)
  ((message :initarg :message :reader wasmdown-error-message))
  (:documentation "Root of all WASMDOWN errors.")
  (:report (lambda (c s)
             (format s "wasmdown: ~a" (wasmdown-error-message c)))))

(define-condition decode-error (wasmdown-error)
  ((offset :initarg :offset :initform 0 :reader decode-error-offset))
  (:documentation "Malformed binary: rejected at the decoding stage, before validation.")
  (:report (lambda (c s)
             (format s "wasmdown: decode error at byte ~d: ~a"
                     (decode-error-offset c) (wasmdown-error-message c)))))

(define-condition validation-error (wasmdown-error)
  ()
  (:documentation "Well-formed binary rejected by static validation.")
  (:report (lambda (c s)
             (format s "wasmdown: validation error: ~a" (wasmdown-error-message c)))))

(define-condition link-error (wasmdown-error)
  ()
  (:documentation "Valid module whose imports could not be matched at link time.")
  (:report (lambda (c s)
             (format s "wasmdown: link error: ~a" (wasmdown-error-message c)))))

(define-condition instantiation-error (wasmdown-error)
  ()
  (:documentation "Valid, linked module that failed during instantiation.")
  (:report (lambda (c s)
             (format s "wasmdown: instantiation error: ~a" (wasmdown-error-message c)))))

(define-condition wasm-trap (wasmdown-error)
  ((kind :initarg :kind :reader wasm-trap-kind)
   (message :initarg :message :reader wasmdown-error-message))
  (:default-initargs :kind :generic)
  (:documentation "Runtime trap: unreachable, out-of-bounds, integer-exception, etc.")
  (:report (lambda (c s)
             (format s "wasmdown: trap (~a): ~a"
                     (wasm-trap-kind c) (wasmdown-error-message c)))))

(define-condition wasm-exception (wasmdown-error)
  ((tag :initarg :tag :reader wasm-exception-tag)
   (payload :initarg :payload :reader wasm-exception-payload))
  (:documentation "A guest exception thrown by throw/throw_ref; distinct from traps.")
  (:report (lambda (c s)
             (format s "wasmdown: wasm exception on tag ~a" (wasm-exception-tag c)))))

(define-condition resource-exhausted (wasmdown-error)
  ()
  (:documentation "A documented implementation limit was hit; never a validation error.")
  (:report (lambda (c s)
             (format s "wasmdown: resource limit: ~a" (wasmdown-error-message c)))))

(define-condition codegen-error (wasmdown-error)
  ((function-index :initarg :function-index :initform nil
                   :reader codegen-error-function-index)
   (offset :initarg :offset :initform nil
           :reader codegen-error-offset)
   (op :initarg :op :initform nil
       :reader codegen-error-op))
  (:documentation
   "The baseline backend refused to compile a function: an instruction
has no lowering. This is a compile-time refusal at the module boundary,
distinct from decode/validate (the module is valid Wasm) and from a
trap (no guest code ran). There is no interpreter fallback: a function
the compiler cannot lower makes :mode :baseline fail before start.")
  (:report (lambda (c s)
             (format s "wasmdown: codegen error~@[ in function ~a~]~@[ at byte ~a~]~@[: ~a~]: ~a"
                     (codegen-error-function-index c)
                     (codegen-error-offset c)
                     (codegen-error-op c)
                     (wasmdown-error-message c)))))

(define-condition host-error (wasmdown-error)
  ()
  (:documentation "Host-side failure distinct from guest decode/validate/trap outcomes.")
  (:report (lambda (c s)
             (format s "wasmdown: host error: ~a" (wasmdown-error-message c)))))
