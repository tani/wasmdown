;;;; SBCL baseline compiler backend (PLAN.md 6): validated IR to
;;;; whitelisted Common Lisp forms, one Lisp function per Wasm
;;;; function, compiled by SBCL COMPILE. Package WASMDOWN.CODEGEN.
;;;;
;;;; Interface (consumed by wasmdown.api / tests):
;;;;   (compile-module-functions module instance &key emit)
;;;;     -> simple-vector of compiled closures, one per DEFINED
;;;;        function (element i = wasm-module-funcs index i, so the
;;;;        absolute funcidx is #imported-funcs + i).
;;;;   (compiled-func vector funcidx) -> the compiled closure.
;;;;   (reset-codegen) -> clear the gensym counter (test hygiene).
;;;;
;;;; Calling convention (the :HOST shape of runtime.lisp WASM-CALL):
;;;; each compiled closure is (lambda (args) results) where ARGS is the
;;;; argument list in push order and the return value is the RESULTS
;;;; list. Install with (make-host-func-instance <wasm-func-type>
;;;; closure). The :WASM / *INVOKE-WASM-FUNC* hook is NEVER touched:
;;;; the interpreter stays the default executor; compiled functions
;;;; dispatch through WASM-CALL's :HOST path. Install closures into
;;;; the SAME module-instance the module was compiled against: the
;;;; generated code resolves memories/tables/globals/segments/funcs
;;;; through that instance at RUN time (via its inst parameter), so
;;;; post-compilation substitution of func-instances (api.lisp's
;;;; baseline mode) is picked up by every direct call, ref.func and
;;;; export automatically.
;;;;
;;;; Honesty: input must be VALIDATED (%VALIDATE; a validation error
;;;; aborts compilation; unvalidated modules never compile).
;;;; Emission is a closed whitelist: LET/BLOCK/TAGBODY/GO/SETF/IF/
;;;; CASE/WHEN/UNLESS/RETURN-FROM/QUOTEd constants plus calls to CL
;;;; primitives (NTH, SVREF, LIST, +, EQUAL, ...) and to the audited
;;;; opcode-to-primitive maps in this file (wasmdown.numops and
;;;; wasmdown.runtime only). Temporaries are uninterned gensyms, so
;;;; guest identifiers never become Lisp symbols; no READ, no EVAL
;;;; of strings. Value representation follows CONTRACTS.md: i32/i64
;;;; unsigned Lisp integers, f32/f64 Lisp floats, refs func-instances
;;;; or NIL.
;;;;
;;;; Known shared-slice discrepancies (reported, NOT worked around in
;;;; emission): interp.lisp's signed sub-word loads wrap back to the
;;;; load width instead of the result width, and its f32/f64 loads and
;;;; stores pass raw bits where floats belong. This backend implements
;;;; the CONTRACTS.md/spec shapes (%SIGN-EXTEND, F32-BITS/F32-FROM-
;;;; BITS at the memory boundary); parity fixtures avoid those opcode
;;;; shapes until interp.lisp is corrected.

(in-package #:wasmdown.codegen)

;;; ---------------------------------------------------------------
;;; Public entry points.

(defvar *temp-counter* 0
  "Gensym counter for emitted temporaries; RESET-CODEGEN clears it.")

(defun reset-codegen ()
  "Reset the emitted-temporary counter (test hygiene only)."
  (setf *temp-counter* 0)
  (values))

(defun compiled-func (vector funcidx)
  "The compiled closure for DEFINED function FUNCIDX (0-based into the
module's defined functions)."
  (svref vector funcidx))

(defun compile-module-functions (module instance &key emit)
  "Compile every defined function of MODULE (against the already
instantiated INSTANCE) to native code via SBCL COMPILE. Returns a
simple-vector of (lambda (args) results) closures in defined-func
order. Signals wasmdown:validation-error when the module is invalid.
EMIT, when a pathname or stream, receives the pretty-printed lambda
form of each function (inspectable generated source, PLAN.md 6.1)."
  (%validate module)
  (let* ((n (length (wasm-module-funcs module)))
         (vectors (make-array n))
         (stream (etypecase emit
                   (null nil)
                   (pathname (open emit :direction :output
                                  :if-exists :supersede))
                   (stream emit))))
    (unwind-protect
         (progn
           (loop for i below n
                 do (setf (svref vectors i)
                          (%compile-one module instance i stream)))
           (coerce vectors 'simple-vector))
      (when (and stream (pathnamep emit)) (close stream)))))

;;; ---------------------------------------------------------------
;;; Validation gate: the shared static validator (PLAN.md: never
;;; compile an unvalidated module).

(defun %validate (module)
  (wasmdown.validate:validate-module module))

;;; ---------------------------------------------------------------
;;; Audited opcode-to-primitive maps (PLAN.md 6.1: "may generate a
;;; call to a runtime primitive only from a finite, audited mapping").
;;; Numeric ops: opcode -> (numops-function nargs).

(defparameter *numop-map*
  '(:i32.add (wasmdown.numops:i32-add 2)
    :i32.sub (wasmdown.numops:i32-sub 2)
    :i32.mul (wasmdown.numops:i32-mul 2)
    :i32.div_s (wasmdown.numops:i32-div-s 2)
    :i32.div_u (wasmdown.numops:i32-div-u 2)
    :i32.rem_s (wasmdown.numops:i32-rem-s 2)
    :i32.rem_u (wasmdown.numops:i32-rem-u 2)
    :i32.and (wasmdown.numops:i32-and 2)
    :i32.or (wasmdown.numops:i32-or 2)
    :i32.xor (wasmdown.numops:i32-xor 2)
    :i32.shl (wasmdown.numops:i32-shl 2)
    :i32.shr_s (wasmdown.numops:i32-shr-s 2)
    :i32.shr_u (wasmdown.numops:i32-shr-u 2)
    :i32.rotl (wasmdown.numops:i32-rotl 2)
    :i32.rotr (wasmdown.numops:i32-rotr 2)
    :i32.clz (wasmdown.numops:i32-clz 1)
    :i32.ctz (wasmdown.numops:i32-ctz 1)
    :i32.popcnt (wasmdown.numops:i32-popcnt 1)
    :i32.eqz (wasmdown.numops:i32-eqz 1)
    :i32.eq (wasmdown.numops:i32-eq 2)
    :i32.ne (wasmdown.numops:i32-ne 2)
    :i32.lt_s (wasmdown.numops:i32-lt-s 2)
    :i32.lt_u (wasmdown.numops:i32-lt-u 2)
    :i32.gt_s (wasmdown.numops:i32-gt-s 2)
    :i32.gt_u (wasmdown.numops:i32-gt-u 2)
    :i32.le_s (wasmdown.numops:i32-le-s 2)
    :i32.le_u (wasmdown.numops:i32-le-u 2)
    :i32.ge_s (wasmdown.numops:i32-ge-s 2)
    :i32.ge_u (wasmdown.numops:i32-ge-u 2)
    :i64.add (wasmdown.numops:i64-add 2)
    :i64.sub (wasmdown.numops:i64-sub 2)
    :i64.mul (wasmdown.numops:i64-mul 2)
    :i64.div_s (wasmdown.numops:i64-div-s 2)
    :i64.div_u (wasmdown.numops:i64-div-u 2)
    :i64.rem_s (wasmdown.numops:i64-rem-s 2)
    :i64.rem_u (wasmdown.numops:i64-rem-u 2)
    :i64.and (wasmdown.numops:i64-and 2)
    :i64.or (wasmdown.numops:i64-or 2)
    :i64.xor (wasmdown.numops:i64-xor 2)
    :i64.shl (wasmdown.numops:i64-shl 2)
    :i64.shr_s (wasmdown.numops:i64-shr-s 2)
    :i64.shr_u (wasmdown.numops:i64-shr-u 2)
    :i64.rotl (wasmdown.numops:i64-rotl 2)
    :i64.rotr (wasmdown.numops:i64-rotr 2)
    :i64.clz (wasmdown.numops:i64-clz 1)
    :i64.ctz (wasmdown.numops:i64-ctz 1)
    :i64.popcnt (wasmdown.numops:i64-popcnt 1)
    :i64.eqz (wasmdown.numops:i64-eqz 1)
    :i64.eq (wasmdown.numops:i64-eq 2)
    :i64.ne (wasmdown.numops:i64-ne 2)
    :i64.lt_s (wasmdown.numops:i64-lt-s 2)
    :i64.lt_u (wasmdown.numops:i64-lt-u 2)
    :i64.gt_s (wasmdown.numops:i64-gt-s 2)
    :i64.gt_u (wasmdown.numops:i64-gt-u 2)
    :i64.le_s (wasmdown.numops:i64-le-s 2)
    :i64.le_u (wasmdown.numops:i64-le-u 2)
    :i64.ge_s (wasmdown.numops:i64-ge-s 2)
    :i64.ge_u (wasmdown.numops:i64-ge-u 2)
    :f32.abs (wasmdown.numops:f32-abs 1)
    :f32.neg (wasmdown.numops:f32-neg 1)
    :f32.ceil (wasmdown.numops:f32-ceil 1)
    :f32.floor (wasmdown.numops:f32-floor 1)
    :f32.trunc (wasmdown.numops:f32-trunc 1)
    :f32.nearest (wasmdown.numops:f32-nearest 1)
    :f32.sqrt (wasmdown.numops:f32-sqrt 1)
    :f32.add (wasmdown.numops:f32-add 2)
    :f32.sub (wasmdown.numops:f32-sub 2)
    :f32.mul (wasmdown.numops:f32-mul 2)
    :f32.div (wasmdown.numops:f32-div 2)
    :f32.min (wasmdown.numops:f32-min 2)
    :f32.max (wasmdown.numops:f32-max 2)
    :f32.copysign (wasmdown.numops:f32-copysign 2)
    :f32.eq (wasmdown.numops:f32-eq 2)
    :f32.ne (wasmdown.numops:f32-ne 2)
    :f32.lt (wasmdown.numops:f32-lt 2)
    :f32.gt (wasmdown.numops:f32-gt 2)
    :f32.le (wasmdown.numops:f32-le 2)
    :f32.ge (wasmdown.numops:f32-ge 2)
    :f64.abs (wasmdown.numops:f64-abs 1)
    :f64.neg (wasmdown.numops:f64-neg 1)
    :f64.ceil (wasmdown.numops:f64-ceil 1)
    :f64.floor (wasmdown.numops:f64-floor 1)
    :f64.trunc (wasmdown.numops:f64-trunc 1)
    :f64.nearest (wasmdown.numops:f64-nearest 1)
    :f64.sqrt (wasmdown.numops:f64-sqrt 1)
    :f64.add (wasmdown.numops:f64-add 2)
    :f64.sub (wasmdown.numops:f64-sub 2)
    :f64.mul (wasmdown.numops:f64-mul 2)
    :f64.div (wasmdown.numops:f64-div 2)
    :f64.min (wasmdown.numops:f64-min 2)
    :f64.max (wasmdown.numops:f64-max 2)
    :f64.copysign (wasmdown.numops:f64-copysign 2)
    :f64.eq (wasmdown.numops:f64-eq 2)
    :f64.ne (wasmdown.numops:f64-ne 2)
    :f64.lt (wasmdown.numops:f64-lt 2)
    :f64.gt (wasmdown.numops:f64-gt 2)
    :f64.le (wasmdown.numops:f64-le 2)
    :f64.ge (wasmdown.numops:f64-ge 2)
    :i32.wrap_i64 (wasmdown.numops:i32-wrap-i64 1)
    :i64.extend_i32_s (wasmdown.numops:i64-extend-i32-s 1)
    :i64.extend_i32_u (wasmdown.numops:i64-extend-i32-u 1)
    :i32.trunc_f32_s (wasmdown.numops:i32-trunc-f32-s 1)
    :i32.trunc_f32_u (wasmdown.numops:i32-trunc-f32-u 1)
    :i32.trunc_f64_s (wasmdown.numops:i32-trunc-f64-s 1)
    :i32.trunc_f64_u (wasmdown.numops:i32-trunc-f64-u 1)
    :i64.trunc_f32_s (wasmdown.numops:i64-trunc-f32-s 1)
    :i64.trunc_f32_u (wasmdown.numops:i64-trunc-f32-u 1)
    :i64.trunc_f64_s (wasmdown.numops:i64-trunc-f64-s 1)
    :i64.trunc_f64_u (wasmdown.numops:i64-trunc-f64-u 1)
    :f32.convert_i32_s (wasmdown.numops:f32-convert-i32-s 1)
    :f32.convert_i32_u (wasmdown.numops:f32-convert-i32-u 1)
    :f32.convert_i64_s (wasmdown.numops:f32-convert-i64-s 1)
    :f32.convert_i64_u (wasmdown.numops:f32-convert-i64-u 1)
    :f64.convert_i32_s (wasmdown.numops:f64-convert-i32-s 1)
    :f64.convert_i32_u (wasmdown.numops:f64-convert-i32-u 1)
    :f64.convert_i64_s (wasmdown.numops:f64-convert-i64-s 1)
    :f64.convert_i64_u (wasmdown.numops:f64-convert-i64-u 1)
    :f32.demote_f64 (wasmdown.numops:f32-demote-f64 1)
    :f64.promote_f32 (wasmdown.numops:f64-promote-f32 1)
    :i32.reinterpret_f32 (wasmdown.numops:i32-reinterpret-f32 1)
    :i64.reinterpret_f64 (wasmdown.numops:i64-reinterpret-f64 1)
    :f32.reinterpret_i32 (wasmdown.numops:f32-reinterpret-i32 1)
    :f64.reinterpret_i64 (wasmdown.numops:f64-reinterpret-i64 1)
    :i32.extend8_s (wasmdown.numops:i32-extend8-s 1)
    :i32.extend16_s (wasmdown.numops:i32-extend16-s 1)
    :i64.extend8_s (wasmdown.numops:i64-extend8-s 1)
    :i64.extend16_s (wasmdown.numops:i64-extend16-s 1)
    :i64.extend32_s (wasmdown.numops:i64-extend32-s 1)
    :i32.trunc_sat_f32_s (wasmdown.numops:i32-trunc-sat-f32-s 1)
    :i32.trunc_sat_f32_u (wasmdown.numops:i32-trunc-sat-f32-u 1)
    :i32.trunc_sat_f64_s (wasmdown.numops:i32-trunc-sat-f64-s 1)
    :i32.trunc_sat_f64_u (wasmdown.numops:i32-trunc-sat-f64-u 1)
    :i64.trunc_sat_f32_s (wasmdown.numops:i64-trunc-sat-f32-s 1)
    :i64.trunc_sat_f32_u (wasmdown.numops:i64-trunc-sat-f32-u 1)
    :i64.trunc_sat_f64_s (wasmdown.numops:i64-trunc-sat-f64-s 1)
    :i64.trunc_sat_f64_u (wasmdown.numops:i64-trunc-sat-f64-u 1))
  "Wasm numeric opcode -> (numops-function nargs).")

;;; Memory access shapes: opcode -> (width-bytes signed-p result-bits
;;; float-type). signed-p loads sign-extend from width to result-bits;
;;; float-type converts at the memory boundary per CONTRACTS.md.

(defparameter *mem-shape*
  '(:i32.load (4 nil 32 nil) :i64.load (8 nil 64 nil)
    :f32.load (4 nil 32 :f32) :f64.load (8 nil 64 :f64)
    :i32.load8_s (1 t 32 nil) :i32.load8_u (1 nil 32 nil)
    :i32.load16_s (2 t 32 nil) :i32.load16_u (2 nil 32 nil)
    :i64.load8_s (1 t 64 nil) :i64.load8_u (1 nil 64 nil)
    :i64.load16_s (2 t 64 nil) :i64.load16_u (2 nil 64 nil)
    :i64.load32_s (4 t 64 nil) :i64.load32_u (4 nil 64 nil)
    :i32.store (4 nil 32 nil) :i64.store (8 nil 64 nil)
    :f32.store (4 nil 32 :f32) :f64.store (8 nil 64 :f64)
    :i32.store8 (1 nil 32 nil) :i32.store16 (2 nil 32 nil)
    :i64.store8 (1 nil 64 nil) :i64.store16 (2 nil 64 nil)
    :i64.store32 (4 nil 64 nil)))

;;; ---------------------------------------------------------------
;;; Emission state. The operand stack is a COMPILE-TIME list of symbols
;;; (top at head); every instruction result is a fresh temporary bound
;;; by an emitted SETF. Generated code never touches a runtime stack.

(defun %temp (cctx prefix)
  "A fresh uninterned symbol for an emitted local/temporary, recorded
in the context so the function's LET binds every temp exactly once."
  (incf *temp-counter*)
  (let ((sym (gensym (format nil "~a~d-" prefix *temp-counter*))))
    (when cctx
      (unless (cctx-all-temps cctx)
        (setf (cctx-all-temps cctx) (make-hash-table)))
      (setf (gethash sym (cctx-all-temps cctx)) t))
    sym))

(defstruct cctx
  module                                 ; wasm-module (validated)
  instance                               ; module-instance (execution home)
  fn                                     ; wasm-func being compiled
  type                                   ; its wasm-func-type
  locals                                 ; simple-vector of local symbols
  inst-sym                               ; emitted symbol holding instance
  fn-sym                                 ; emitted BLOCK name for RETURN-FROM
  blocks                                 ; block-state list, innermost first
  operands                               ; compile-time operand stack
  pending                                ; reversed straight-line forms
  tagbody                                ; reversed tagbody items
  results-sym                            ; emitted symbol for results list
  all-temps)                             ; hash of every emitted temp

(defstruct block-state
  kind                                   ; :block | :loop
  entry-tag                              ; TAGBODY tag for br targets
  nparams                                ; block params (loop br arity)
  nresults                               ; block results (br arity)
  results                                ; result temp symbols (phi)
  entry-temps                            ; loop entry temp symbols
  stack-height                           ; operand depth below params
  tail)                                  ; the operand LIST below params

(defun %push (cctx sym)
  (push sym (cctx-operands cctx)))

(defun %pop (cctx)
  "Pop the top operand symbol. NIL (dead-region fill) is returned as-is:
  after an unconditional branch the code is unreachable and validation
  typed it polymorphically; emitted forms there are dead by the GO."
  (pop (cctx-operands cctx)))

(defun %pop-n (cctx n)
  "Pop N operand symbols, returning them in PUSH order (bottom first)."
  (let ((syms '()))
    (dotimes (i n)
      (push (%pop cctx) syms))
    syms))

(defun %result-temp (cctx)
  "A fresh temporary symbol for one instruction result."
  (%temp cctx "t"))

(defun %emit (cctx form)
  "Append one straight-line form to the pending tail."
  (push form (cctx-pending cctx)))

(defun %emit-flush (cctx)
  "Move pending forms into the tagbody list (order preserved)."
  (dolist (form (reverse (cctx-pending cctx)))
    (push form (cctx-tagbody cctx)))
  (setf (cctx-pending cctx) nil))

(defun %emit-tag (cctx tag)
  "Append a TAGBODY tag (a gensym GO target)."
  (%emit-flush cctx)
  (push tag (cctx-tagbody cctx)))

(defun %inst (cctx)
  "Emitted symbol bound to the module-instance."
  (cctx-inst-sym cctx))

;;; Runtime instance accessors emitted into generated code (resolved
;;; against the instance at RUN time so post-compilation func-instance
;;; substitution is honored).

(defun %mem (cctx idx)
  `(svref (wasmdown.runtime:module-instance-memories ,(%inst cctx)) ,idx))

(defun %table (cctx idx)
  `(svref (wasmdown.runtime:module-instance-tables ,(%inst cctx)) ,idx))

(defun %global (cctx idx)
  `(svref (wasmdown.runtime:module-instance-globals ,(%inst cctx)) ,idx))

(defun %func (cctx idx)
  `(svref (wasmdown.runtime:module-instance-funcs ,(%inst cctx)) ,idx))

(defun %data-seg (cctx idx)
  `(svref (wasmdown.runtime::module-instance-data-segs ,(%inst cctx)) ,idx))

(defun %elem-seg (cctx idx)
  `(svref (wasmdown.runtime::module-instance-elem-segs ,(%inst cctx)) ,idx))

(defun %blocktype-shape (cctx bt)
  "Blocktype -> (values nparams nresults)."
  (cond ((null bt) (values 0 0))
        ((and (consp bt) (eq (car bt) :typeidx))
         (let ((ft (svref (wasm-module-types (cctx-module cctx)) (cdr bt))))
           (values (length (wasm-func-type-params ft))
                   (length (wasm-func-type-results ft)))))
        (t (values 0 1))))

;;; ---------------------------------------------------------------
;;; Traps from generated code (audited runtime primitives).

(defun %trap (kind format-control &rest args)
  "Runtime side of emitted traps: signal wasmdown:wasm-trap of KIND."
  (error 'wasmdown:wasm-trap :kind kind
         :message (apply #'format nil format-control args)))

(defun %sign-extend (raw in-bits out-bits)
  "Sign-extend the low IN-BITS of RAW into the OUT-BITS unsigned
domain (CONTRACTS.md unsigned storage; spec load semantics)."
  (let* ((sv (if (>= raw (ash 1 (1- in-bits)))
                 (- raw (ash 1 in-bits))
                 raw))
         (mask (1- (ash 1 out-bits))))
    (mod (logand sv mask) (ash 1 out-bits))))

;;; ---------------------------------------------------------------
;;; Branches. A br moves its branch operands into the target frame's
;;; phi temporaries (block results / loop entry temps) and GOes to the
;;; target tag: block/if tags sit AFTER the body (forward exit), loop
;;; tags BEFORE it (backedge). A depth past every block is :return.

(defun %emit-br (cctx depth)
  (let* ((frame (nth depth (cctx-blocks cctx)))
         (loop-p (eq (block-state-kind frame) :loop))
         (arity (if loop-p (block-state-nparams frame)
                    (block-state-nresults frame)))
         (targets (if loop-p (block-state-entry-temps frame)
                      (block-state-results frame)))
         (vals (%pop-n cctx arity)))
    (loop for v in vals
          for tt in targets
          do (%emit cctx `(setf ,tt ,v)))
    (%emit cctx `(go ,(block-state-entry-tag frame)))
    ;; Dead region: fill with the target's phis atop nils down to the
    ;; target frame's base height, so any residual (unreachable)
    ;; instructions still emit well-formed dead forms.
    ;; restore the operand list that existed BELOW the target's params
    ;; at its entry: the symbols there are still live (a block used as
    ;; an operand keeps the earlier stack entries), so filling with
    ;; NILs lost them and the following reachable code consumed NIL —
    ;; e.g. (i32.add (local.get 0) (block ...)) added NIL.
    (setf (cctx-operands cctx)
          (append (reverse (copy-list targets))
                  (block-state-tail frame)))))

(defun %emit-return (cctx vals)
  "Emit the function epilogue: return the results LIST (the :host
convention of wasm-call). VALS are in push order."
  (%emit cctx `(return-from ,(cctx-fn-sym cctx) (list ,@vals)))
  (setf (cctx-operands cctx) nil))

(defun %emit-seq (cctx instrs)
  "Emit a straight-line instruction sequence (a block body without
its :end)."
  (loop for instr across instrs
        do (%emit-instr cctx instr)))

;;; ---------------------------------------------------------------
;;; Simple-opcode emitters.

(defun %emit-numop (cctx op)
  (let* ((entry (getf *numop-map* op))
         (fn (first entry))
         (nargs (second entry))
         (args (%pop-n cctx nargs))       ; push order
         (tmp (%result-temp cctx)))
    (%emit cctx `(setf ,tmp (,fn ,@args)))
    (%push cctx tmp)))

(defun %emit-load (cctx instr)
  (let* ((op (wasm-instr-op instr))
         (shape (getf *mem-shape* op))
         (width (first shape))
         (signedp (second shape))
         (result-bits (third shape))
         (float-type (fourth shape))
         (addr (%pop cctx))
         (imms (wasm-instr-imms instr))
         (offset (getf imms :offset 0))
         (memidx (getf imms :memidx 0))
         (ea (%temp cctx "ea"))
         (raw (%temp cctx "raw"))
         (out (%result-temp cctx)))
    (%emit cctx `(setf ,ea (+ ,addr ,offset)))
    (%emit cctx `(setf ,raw (wasmdown.runtime:memory-load ,(%mem cctx memidx)
                                                          ,ea ,width)))
    (cond (float-type
           ;; NB: no placeholder emit here. A `(setf out (f32-bits 0))'
           ;; used to sit above as a type hint, but it is a real CALL
           ;; at runtime: every float LOAD (f64.load included) then
           ;; signalled "0 is not a SINGLE-FLOAT" in baseline mode.
           (case float-type
             (:f32 (%emit cctx `(setf ,out
                                      (wasmdown.numops:f32-from-bits ,raw))))
             (:f64 (%emit cctx `(setf ,out
                                      (wasmdown.numops:f64-from-bits ,raw))))))
          (signedp
           (%emit cctx `(setf ,out (%sign-extend ,raw ,(* width 8)
                                                 ,result-bits))))
          (t (%emit cctx `(setf ,out ,raw))))
    (%push cctx out)))

(defun %emit-store (cctx instr)
  (let* ((op (wasm-instr-op instr))
         (shape (getf *mem-shape* op))
         (width (first shape))
         (float-type (fourth shape))
         (value (%pop cctx))
         (addr (%pop cctx))
         (imms (wasm-instr-imms instr))
         (offset (getf imms :offset 0))
         (memidx (getf imms :memidx 0))
         (ea (%temp cctx "ea"))
         (bits (%temp cctx "bits")))
    (%emit cctx `(setf ,ea (+ ,addr ,offset)))
    (ecase float-type
      (:f32 (%emit cctx `(setf ,bits (wasmdown.numops:f32-bits ,value))))
      (:f64 (%emit cctx `(setf ,bits (wasmdown.numops:f64-bits ,value))))
      ((nil) (%emit cctx `(setf ,bits ,value))))
    (%emit cctx `(wasmdown.runtime:memory-store ,(%mem cctx memidx)
                                                ,ea ,width ,bits))))

(defun %emit-const (cctx instr)
  (let* ((op (wasm-instr-op instr))
         (imms (wasm-instr-imms instr))
         (tmp (%result-temp cctx)))
    (ecase op
      (:i32.const
       (let ((v (getf imms :value)))
         (%emit cctx `(setf ,tmp ,(if (< v 0) (+ v #x100000000) v)))))
      (:i64.const
       (let ((v (getf imms :value)))
         (%emit cctx `(setf ,tmp ,(if (< v 0) (+ v #x10000000000000000) v)))))
      (:f32.const
       (%emit cctx `(setf ,tmp (wasmdown.numops:f32-from-bits
                                ,(getf imms :bits)))))
      (:f64.const
       (%emit cctx `(setf ,tmp (wasmdown.numops:f64-from-bits
                                ,(getf imms :bits))))))
    (%push cctx tmp)))

(defun %emit-call (cctx instr)
  (let* ((funcidx (getf (wasm-instr-imms instr) :func))
         (n-imported (count :func (wasm-module-imports (cctx-module cctx))
                            :key #'wasm-import-kind))
         (type (if (< funcidx n-imported)
                   ;; imported callee: the func-import with ordinal
                   ;; FUNCIDX among func imports carries the typeidx
                   (svref (wasm-module-types (cctx-module cctx))
                          (wasm-import-type
                           (nth funcidx
                                (loop for im across (wasm-module-imports
                                                     (cctx-module cctx))
                                      when (eq (wasm-import-kind im) :func)
                                        collect im))))
                   (svref (wasm-module-types (cctx-module cctx))
                          (wasm-func-typeidx
                           (svref (wasm-module-funcs (cctx-module cctx))
                                  (- funcidx n-imported))))))
         (nparams (length (wasm-func-type-params type)))
         (args (%pop-n cctx nparams))     ; push order
         (results (loop for r in (wasm-func-type-results type)
                        collect (%result-temp cctx)))
         (list-sym (%temp cctx "call-args")))
    (%emit cctx `(setf ,list-sym (list ,@args)))
    (%emit cctx `(multiple-value-setq (,@results)
                   (values-list
                    (apply #'wasmdown.runtime:wasm-call
                           ,(%func cctx funcidx) ,list-sym))))
    (dolist (r results) (%push cctx r))))

(defun %emit-call-indirect (cctx instr)
  (let* ((imms (wasm-instr-imms instr))
         (typeidx (getf imms :type))
         (expected (svref (wasm-module-types (cctx-module cctx)) typeidx))
         (eparams (wasm-func-type-params expected))
         (eresults (wasm-func-type-results expected))
         (idx (%pop cctx))
         (params (%pop-n cctx (length eparams)))  ; push order
         (ref (%temp cctx "ref"))
         (actual (%temp cctx "ft"))
         (results (loop for r in eresults collect (%result-temp cctx)))
         (list-sym (%temp cctx "ci-args")))
    ;; table type must be funcref (interp parity)
    (%emit cctx `(unless (eq (wasmdown.runtime:table-instance-type
                              ,(%table cctx (getf imms :table)))
                             :funcref)
                   (%trap :indirect-call-type-mismatch
                          "call_indirect on non-funcref table")))
    (%emit cctx `(setf ,ref (wasmdown.runtime:table-get
                             ,(%table cctx (getf imms :table)) ,idx)))
    (%emit cctx `(unless (wasmdown.runtime:func-instance-p ,ref)
                   (%trap :null-reference
                          "call_indirect on null reference")))
    (%emit cctx `(setf ,actual (wasmdown.runtime:func-instance-type ,ref)))
    (%emit cctx
           `(unless (and (equal ',eparams
                                (wasm-func-type-params ,actual))
                         (equal ',eresults
                                (wasm-func-type-results ,actual)))
              (%trap :indirect-call-type-mismatch
                     "call_indirect type mismatch")))
    (%emit cctx `(setf ,list-sym (list ,@params)))
    (%emit cctx `(multiple-value-setq (,@results)
                   (values-list
                    (apply #'wasmdown.runtime:wasm-call ,ref ,list-sym))))
    (dolist (r results) (%push cctx r))))

(defun %emit-memory-op (cctx instr)
  "memory.size/grow/fill/copy/init and data.drop, table.* and elem.drop."
  (let* ((op (wasm-instr-op instr))
         (imms (wasm-instr-imms instr))
         (three (lambda ()            ; pop (a b c) -> push order
                  (%pop-n cctx 3)))
         (two (lambda ()
                (%pop-n cctx 2))))
    (case op
      (:memory.size
       (let ((tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp (wasmdown.runtime:memory-size
                                  ,(%mem cctx (getf imms :memidx 0)))))
         (%push cctx tmp)))
      (:memory.grow
       (let ((delta (%pop cctx)) (tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp (wasmdown.runtime:memory-grow
                                  ,(%mem cctx (getf imms :memidx 0))
                                  ,delta)))
         (%push cctx tmp)))
      (:memory.fill
       (destructuring-bind (dst val len) (funcall three)
         (%emit cctx `(wasmdown.runtime:memory-fill
                       ,(%mem cctx (getf imms :memidx 0)) ,dst ,val ,len))))
      (:memory.copy
       (destructuring-bind (dst src len) (funcall three)
         (let ((dst-idx (getf imms :dst 0)) (src-idx (getf imms :src 0)))
           (if (= dst-idx src-idx)
               (%emit cctx `(wasmdown.runtime:memory-copy
                             ,(%mem cctx dst-idx) ,dst ,src ,len))
               ;; Core 3.0 multi-memory: bounds on both backings
               (%emit cctx `(wasmdown.runtime:memory-copy2
                             ,(%mem cctx dst-idx) ,dst
                             ,(%mem cctx src-idx) ,src ,len))))))
      (:memory.init
       (destructuring-bind (dst src len) (funcall three)
         (%emit cctx `(wasmdown.runtime:memory-init
                       ,(%mem cctx (getf imms :memidx 0))
                       ,(%data-seg cctx (getf imms :data)) ,dst ,src ,len))))
      (:data.drop
       (%emit cctx `(wasmdown.runtime:drop-data ,(%inst cctx)
                                                ,(getf imms :data))))
      (:table.get
       (let ((idx (%pop cctx)) (tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp (wasmdown.runtime:table-get
                                  ,(%table cctx (getf imms :table)) ,idx)))
         (%push cctx tmp)))
      (:table.set
       (destructuring-bind (idx ref) (funcall two)
         (%emit cctx `(wasmdown.runtime:table-set
                       ,(%table cctx (getf imms :table)) ,idx ,ref))))
      (:table.size
       (let ((tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp (wasmdown.runtime:table-size
                                  ,(%table cctx (getf imms :table)))))
         (%push cctx tmp)))
      (:table.grow
       (destructuring-bind (delta ref) (funcall two)
         (let ((tmp (%result-temp cctx)))
           (%emit cctx `(setf ,tmp (wasmdown.runtime:table-grow
                                    ,(%table cctx (getf imms :table))
                                    ,ref ,delta)))
           (%push cctx tmp))))
      (:table.fill
       (destructuring-bind (dst ref len) (funcall three)
         (%emit cctx `(wasmdown.runtime:table-fill
                       ,(%table cctx (getf imms :table)) ,dst ,ref ,len))))
      (:table.copy
       (destructuring-bind (dst src len) (funcall three)
         (let ((dst-idx (getf imms :dst 0)) (src-idx (getf imms :src 0)))
           (unless (= dst-idx src-idx)
             (error 'wasmdown:validation-error
                    :message "table.copy across distinct tables"))
           (%emit cctx `(wasmdown.runtime:table-copy
                         ,(%table cctx dst-idx) ,dst ,src ,len)))))
      (:table.init
       (destructuring-bind (dst src len) (funcall three)
         (%emit cctx `(wasmdown.runtime:table-init
                       ,(%table cctx (getf imms :table))
                       ,(%elem-seg cctx (getf imms :elem)) ,dst ,src ,len))))
      (:elem.drop
       (%emit cctx `(wasmdown.runtime:drop-elem ,(%inst cctx)
                                                ,(getf imms :elem)))))))

;;; ---------------------------------------------------------------
;;; Structured control flow lowering.
;;;
;;;   :block/:if -> forward tag after the body; br = move operands to
;;;                 the block's result temps + GO.
;;;   :loop       -> backward tag before the body; br = move operands
;;;                 to the loop's entry temps + GO (iterate).
;;;   :return / br past all blocks -> epilogue return.
;;; Result temps are written by every predecessor (fall-through and
;;; branches), so the tag continuation just reads them (phi temps).

(defun %emit-block (cctx instr)
  (multiple-value-bind (nparams nresults)
      (%blocktype-shape cctx (getf (wasm-instr-imms instr) :bt))
    (let* ((params (%pop-n cctx nparams))
           (entry-temps (loop repeat nparams collect (%temp cctx "bp")))
           (result-temps (loop repeat nresults collect (%temp cctx "br")))
           (tag (gensym "B"))
           (base (length (cctx-operands cctx))))
      (loop for p in params
            for tp in entry-temps
            do (%emit cctx `(setf ,tp ,p)))
      (setf (cctx-operands cctx)
            (append (reverse entry-temps) (cctx-operands cctx)))
      (push (make-block-state :kind :block :entry-tag tag
                              :nparams nparams :nresults nresults
                              :results result-temps
                              :entry-temps entry-temps
                              :stack-height base
                              :tail (cctx-operands cctx))
            (cctx-blocks cctx))
      (%emit-seq cctx (wasm-instr-body instr))
      ;; the DEAD code after a branch may have consumed operands; the
      ;; block's entry tail is the only reliable record of what was
      ;; below it (br.wast's as-return-values: the dead `return` ate
      ;; the `2` and the block then dropped it)
      (let ((tail (block-state-tail (first (cctx-blocks cctx)))))
        (pop (cctx-blocks cctx))
        ;; fall-through: top NRESULTS are the block results
        (let ((fallthrough (%pop-n cctx nresults)))
          (loop for ft in fallthrough
                for rt in result-temps
                do (%emit cctx `(setf ,rt ,ft))))
        (%emit-tag cctx tag)
        (setf (cctx-operands cctx)
              (append (reverse result-temps) tail))))))

(defun %emit-loop (cctx instr)
  (multiple-value-bind (nparams nresults)
      (%blocktype-shape cctx (getf (wasm-instr-imms instr) :bt))
    (let* ((params (%pop-n cctx nparams))
           (entry-temps (loop repeat nparams collect (%temp cctx "lp")))
           (result-temps (loop repeat nresults collect (%temp cctx "br")))
           (tag (gensym "L"))
           (base (length (cctx-operands cctx))))
      (loop for p in params
            for tp in entry-temps
            do (%emit cctx `(setf ,tp ,p)))
      (%emit-tag cctx tag)
      (setf (cctx-operands cctx)
            (append (reverse entry-temps) (cctx-operands cctx)))
      (push (make-block-state :kind :loop :entry-tag tag
                              :nparams nparams :nresults nresults
                              :results result-temps
                              :entry-temps entry-temps
                              :stack-height base
                              :tail (cctx-operands cctx))
            (cctx-blocks cctx))
      (%emit-seq cctx (wasm-instr-body instr))
      (pop (cctx-blocks cctx))
      ;; fall-through: top NRESULTS leave the loop
      (let ((fallthrough (%pop-n cctx nresults)))
        (loop for ft in fallthrough
              for rt in result-temps
              do (%emit cctx `(setf ,rt ,ft)))
        (%emit-tag cctx (gensym "LEND"))   ; unreachable landing pad
        (setf (cctx-operands cctx)
              (append (reverse result-temps)
                      (cctx-operands cctx)))))))

(defun %emit-if (cctx instr)
  (multiple-value-bind (nparams nresults)
      (%blocktype-shape cctx (getf (wasm-instr-imms instr) :bt))
    (let* ((cond-v (%pop cctx))
           (params (%pop-n cctx nparams))
           (entry-temps (loop repeat nparams collect (%temp cctx "bp")))
           (result-temps (loop repeat nresults collect (%temp cctx "br")))
           (tag (gensym "I"))
           (else-tag (gensym "ELSE"))
           (base (length (cctx-operands cctx))))
      (loop for p in params
            for tp in entry-temps
            do (%emit cctx `(setf ,tp ,p)))
      (%emit-flush cctx)
      (%emit cctx `(unless (plusp ,cond-v) (go ,else-tag)))
      ;; then arm
      (setf (cctx-operands cctx)
            (append (reverse entry-temps) (cctx-operands cctx)))
      (push (make-block-state :kind :block :entry-tag tag
                              :nparams nparams :nresults nresults
                              :results result-temps
                              :entry-temps entry-temps
                              :stack-height base
                              :tail (cctx-operands cctx))
            (cctx-blocks cctx))
      (%emit-seq cctx (wasm-instr-body instr))
      (pop (cctx-blocks cctx))
      (let ((then-vals (%pop-n cctx nresults)))
        (loop for tv in then-vals
              for rt in result-temps
              do (%emit cctx `(setf ,rt ,tv))))
      (%emit cctx `(go ,tag))
      ;; else arm
      (%emit-tag cctx else-tag)
      (setf (cctx-operands cctx)
            (append (reverse entry-temps)
                    (cctx-operands cctx)))
      (push (make-block-state :kind :block :entry-tag tag
                              :nparams nparams :nresults nresults
                              :results result-temps
                              :entry-temps entry-temps
                              :stack-height base
                              :tail (cctx-operands cctx))
            (cctx-blocks cctx))
      (%emit-seq cctx (or (getf (wasm-instr-imms instr) :else-body) #()))
      (pop (cctx-blocks cctx))
      (let ((else-vals (%pop-n cctx nresults)))
        (loop for ev in else-vals
              for rt in result-temps
              do (%emit cctx `(setf ,rt ,ev))))
      (%emit-tag cctx tag)
      (setf (cctx-operands cctx)
            (append (reverse result-temps)
                    (cctx-operands cctx))))))

;;; ---------------------------------------------------------------
;;; Main instruction dispatcher.

(defun %emit-instr (cctx instr)
  (let* ((op (wasm-instr-op instr))
         (imms (wasm-instr-imms instr)))
    (cond
      ((eq op :nop))
      ((eq op :end))                        ; function terminator
      ((eq op :unreachable)
       (%emit cctx `(%trap :unreachable "unreachable executed")))
      ((eq op :drop) (%pop cctx))
      ((eq op :select)
       (let ((c (%pop cctx)) (b (%pop cctx)) (a (%pop cctx))
             (tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp (if (plusp ,c) ,a ,b)))
         (%push cctx tmp)))
      ((eq op :local.get)
       (%push cctx (svref (cctx-locals cctx) (getf imms :local))))
      ((eq op :local.set)
       (let ((v (%pop cctx)))
         (%emit cctx `(setf ,(svref (cctx-locals cctx) (getf imms :local))
                            ,v))))
      ((eq op :local.tee)
       (let ((v (%pop cctx))
             (l (svref (cctx-locals cctx) (getf imms :local))))
         (%emit cctx `(setf ,l ,v))
         (%push cctx l)))
      ((eq op :global.get)
       (let ((tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp (wasmdown.runtime:global-value
                                  ,(%global cctx (getf imms :global)))))
         (%push cctx tmp)))
      ((eq op :global.set)
       (let ((v (%pop cctx)))
         (%emit cctx `(setf (wasmdown.runtime:global-value
                             ,(%global cctx (getf imms :global)))
                            ,v))))
      ((member op '(:i32.const :i64.const :f32.const :f64.const))
       (%emit-const cctx instr))
      ((eq op :block) (%emit-block cctx instr))
      ((eq op :loop) (%emit-loop cctx instr))
      ((eq op :if) (%emit-if cctx instr))
      ((eq op :br)
       (let ((depth (getf imms :label)))
         (cond ((>= depth (length (cctx-blocks cctx)))
                ;; br past every block: return with function results
                (%emit-return cctx
                              (%pop-n cctx (length (wasm-func-type-results
                                                     (cctx-type cctx))))))
               (t
                (%emit-br cctx depth)))))
      ((eq op :br-if)
       (let* ((depth (getf imms :label))
              (c (%pop cctx))
              (nresults (length (wasm-func-type-results (cctx-type cctx)))))
         (cond ((>= depth (length (cctx-blocks cctx)))
                ;; conditional return; fall-through keeps operands
                (let* ((vals (%pop-n cctx nresults))
                       (dup (copy-list vals)))
                  (dolist (s dup) (%push cctx s))  ; restore fall-through
                  (%emit cctx `(when (plusp ,c)
                                 (return-from ,(cctx-fn-sym cctx)
                                   (list ,@dup))))))
               (t
                (let* ((frame (nth depth (cctx-blocks cctx)))
                       (loop-p (eq (block-state-kind frame) :loop))
                       (arity (if loop-p (block-state-nparams frame)
                                  (block-state-nresults frame)))
                       (targets (if loop-p (block-state-entry-temps frame)
                                    (block-state-results frame)))
                       (vals (%pop-n cctx arity))
                       (dup (copy-list vals)))
                  (dolist (s dup) (%push cctx s))  ; fall-through keeps them
                  (%emit-flush cctx)
                  (%emit cctx `(when (plusp ,c)
                                 ,@(loop for v in dup
                                         for tt in targets
                                         collect `(setf ,tt ,v))
                                 (go ,(block-state-entry-tag frame)))))))))
      ((eq op :br-table)
       (let* ((targets (getf imms :targets))
              (default (getf imms :default))
              (idx (%pop cctx))
              (dframe (nth default (cctx-blocks cctx)))
              (dar (if (eq (block-state-kind dframe) :loop)
                       (block-state-nparams dframe)
                       (block-state-nresults dframe)))
              (vals (%pop-n cctx dar))
              (saved (copy-list vals)))
         ;; fall-through keeps the operands; arms share the symbols
         (dolist (s saved) (%push cctx s))
         (%emit-flush cctx)
         (flet ((arm (depth)
                  (let* ((frame (nth depth (cctx-blocks cctx)))
                         (tgts (if (eq (block-state-kind frame) :loop)
                                   (block-state-entry-temps frame)
                                   (block-state-results frame))))
                    (values tgts
                            (block-state-entry-tag frame)))))
           (flet ((arm-form (depth)
                    (multiple-value-bind (tgts tag) (arm depth)
                      `(,@(loop for v in saved
                                for tt in tgts
                                collect `(setf ,tt ,v))
                        (go ,tag)))))
             (%emit cctx
                    `(case ,idx
                       ,@(loop for target across targets
                               for i from 0
                               collect `(,i ,@(arm-form target)))
                       (t ,@(arm-form default))))))))
      ((eq op :return)
       (%emit-return cctx
                     (%pop-n cctx (length (wasm-func-type-results
                                            (cctx-type cctx))))))
      ((eq op :call) (%emit-call cctx instr))
      ((eq op :call-indirect) (%emit-call-indirect cctx instr))
      ((eq op :ref.null)
       (let ((tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp nil))
         (%push cctx tmp)))
      ((eq op :ref.is-null)
       (let ((v (%pop cctx)) (tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp (if ,v 0 1)))
         (%push cctx tmp)))
      ((eq op :ref.func)
       (let ((tmp (%result-temp cctx)))
         (%emit cctx `(setf ,tmp ,(%func cctx (getf imms :func))))
         (%push cctx tmp)))
      ((member op '(:table.get :table.set :table.size :table.grow
                    :table.fill :table.copy :table.init :elem.drop
                    :memory.size :memory.grow :memory.fill
                    :memory.copy :memory.init :data.drop))
       (%emit-memory-op cctx instr))
      ((getf *mem-shape* op)
       (if (search "STORE" (symbol-name op))
           (%emit-store cctx instr)
           (%emit-load cctx instr)))
      ((getf *numop-map* op) (%emit-numop cctx op))
      (t (error 'wasmdown:host-error
                :message (format nil "codegen: unimplemented opcode ~a"
                                 op))))))

;;; ---------------------------------------------------------------
;;; Whole-function emission.

(defun %default-value (valtype)
  (case valtype
    (:i32 0) (:i64 0)
    (:f32 (wasmdown.numops:f32-from-bits 0))
    (:f64 (wasmdown.numops:f64-from-bits 0))
    (t nil)))

(defun %expand-local-types (fn type)
  "Local index space: params then declared locals, types expanded."
  (append (wasm-func-type-params type)
          (loop for (valtype . count) in (wasm-func-locals fn)
                append (make-list count :initial-element valtype))))

(defun %build-func-form (module instance funcidx)
  "Emit the lambda form for defined function FUNCIDX:
  (LAMBDA (ARGS)
    (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 1)))
    (LET (locals incl. inst/fn temps)
      (BLOCK <fn-sym>
        (SYMBOL-MACROLET-free TAGBODY ...))))"
  (let* ((fn (svref (wasm-module-funcs module) funcidx))
         (type (svref (wasm-module-types module) (wasm-func-typeidx fn)))
         (local-types (%expand-local-types fn type))
         (params (wasm-func-type-params type))
         (nparams (length params))
         (fn-sym (gensym "WASM-FN"))
         (cctx (make-cctx :module module :instance instance
                          :fn fn :type type :locals nil
                          :inst-sym nil :fn-sym fn-sym
                          :blocks nil :operands nil
                          :pending nil :tagbody nil
                          :all-temps (make-hash-table)))
         (args-sym (%temp cctx "args"))
         (inst-sym (%temp cctx "inst"))
         (local-syms (coerce (loop repeat (length local-types)
                                   collect (%temp cctx "v"))
                             'vector)))
    (setf (cctx-locals cctx) local-syms
          (cctx-inst-sym cctx) inst-sym)
    ;; THE FUNCTION BODY IS ITSELF A LABEL (Wasm: `br N' with N equal
    ;; to the number of enclosing blocks targets the function body, and
    ;; fixtures use it - br.wast's as-return-values, br_table's default
    ;; arms). Without a frame for it, (nth depth blocks) was NIL and the
    ;; compiler blew up on a BLOCK-STATE type error.
    (let* ((nresults (length (wasm-func-type-results type)))
           (fn-tag (gensym "FNTAG"))
           (fn-results (loop repeat nresults collect (%temp cctx "fr"))))
      (push (make-block-state :kind :return :entry-tag fn-tag
                              :nparams 0 :nresults nresults
                              :results fn-results
                              :entry-temps nil
                              :stack-height 0
                              :tail nil)
            (cctx-blocks cctx))
      (%emit-seq cctx (wasm-func-body fn))
      (pop (cctx-blocks cctx))
      ;; fall-through: the top NRESULTS become this body's results
      (let ((fallthrough (%pop-n cctx nresults)))
        (loop for v in fallthrough
              for tt in fn-results
              do (%emit cctx `(setf ,tt ,v))))
      (%emit-tag cctx fn-tag)
      (let ((results-sym (%temp cctx "results")))
        (%emit-flush cctx)
        (%emit cctx `(setf ,results-sym (list ,@fn-results)))
        (%emit cctx `(return-from ,fn-sym ,results-sym))
        (%emit-flush cctx)
        (setf (cctx-results-sym cctx) results-sym)))
    (let* ((tagbody-items (reverse (cctx-tagbody cctx)))
           (param-binds
            (loop for i below nparams
                  collect `(,(svref local-syms i) (nth ,i ,args-sym))))
           (default-binds
            (loop for i from nparams below (length local-types)
                  collect `(,(svref local-syms i)
                            ',(%default-value (nth i local-types)))))
           (plain-temps
            (loop for sym being each hash-key of (cctx-all-temps cctx)
                  unless (or (member sym param-binds :key #'first)
                             (member sym default-binds :key #'first)
                             (eq sym args-sym)
                             (eq sym inst-sym))
                    collect sym)))
      `(lambda (,args-sym)
         (declare (optimize (speed 3) (safety 1)))
         (let (,@param-binds
               ,@default-binds
               (,inst-sym ',instance)
               ,@(loop for sym in plain-temps
                       collect `(,sym nil)))
           (declare (ignorable ,inst-sym))
           (block ,fn-sym
             (tagbody
              ,@tagbody-items)
             ,(cctx-results-sym cctx)))))))

(defun %compile-one (module instance funcidx stream)
  "Emit and COMPILE one function; pretty-print the form to STREAM when
given. *PRINT-CIRCLE* is required: the emitted form embeds the instance
as a quoted constant and module-instances are circular (their :wasm
func-instances backpoint to the instance), so a plain pprint would
recurse until the host stack runs out. Returns the compiled closure."
  (let* ((form (%build-func-form module instance funcidx))
         (closure (compile nil form)))
    (when stream
      (let ((*print-circle* t))
        (format stream "~&;;; function ~a~%" funcidx)
        (pprint form stream)
        (format stream "~&~%")))
    closure))