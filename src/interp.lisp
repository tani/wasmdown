;;;; Reference interpreter (PLAN.md §7, CONTRACTS.md). Package
;;;; WASMDOWN.INTERP.
;;;;
;;;; interp-invoke executes a wasm function body with an explicit
;;;; operand stack (Lisp list, top at the head) and a locals vector.
;;;; Structured control flow (block/loop/if) is executed recursively;
;;;; br/br_if/br_table/return exit through unique catch tags (the
;;;; per-execution block-frame objects themselves) thrown with
;;;; CATCH/THROW to the owning frame, so a branch is a non-local exit
;;;; to the labeled block only.
;;;;
;;;; Nested guest calls count against *CALL-DEPTH-LIMIT*: reaching it
;;;; traps :call-stack-exhausted (CONTRACTS.md) instead of exhausting
;;;; the host control stack, whose SBCL error is fatal and uncatchable.
;;;;
;;;; Every numeric opcode maps to its wasmdown.numops primitive; traps
;;;; propagate as wasmdown:wasm-trap. Values follow CONTRACTS.md: i32
;;;; is 0..2^32-1, i64 0..2^64-1, f32/f64 Lisp floats, refs are
;;;; func-instances or NIL.
;;;;
;;;; At load time this file installs the reference implementations of
;;;; the runtime hooks *INVOKE-WASM-FUNC* and *CONST-EXPR-EVALUATOR*,
;;;; making wasm-call execute through the interpreter by default
;;;; (codegen replaces *INVOKE-WASM-FUNC* later).

(in-package #:wasmdown.interp)

;;; ---------------------------------------------------------------
;;; Frame state, kept in dynamic variables during a function body.

(declaim (ftype (function (t) t)))
(defvar *frame-module* nil
  "The module-instance of the function being executed (for funcs,
tables, memories, globals).")
(defvar *frame-funcidx* 0
  "Index of the executing function into the module's defined funcs
 (wasm-module-funcs), for name-based debug only.")
(defvar *return-tag* nil
  "Sentinel catch tag thrown by :return; bound per invocation to a
 fresh tag object by INTERP-INVOKE.")
(defvar *frame-nresults* 0
  "Number of results of the currently executing function; a :return
 carries exactly that many values.")

(declaim (inline push-val pop-val))

(defun push-val (v stack) (cons v stack))
(defun pop-val (stack)
  (if (null stack)
      (error 'wasmdown:wasm-trap :kind :unreachable
             :message "operand stack underflow (unreachable code reached)")
      (values (car stack) (cdr stack))))

(defun %pop-n (stack n)
  "Pop N values; returns (values popped-list new-stack). The popped list
 is in push order (bottom first)."
  (if (< (length stack) n)
      (error 'wasmdown:wasm-trap :kind :unreachable
             :message "operand stack underflow (unreachable code reached)")
      (values (reverse (subseq stack 0 n)) (nthcdr n stack))))

(declaim (inline %local-ref))
(defun %local-ref (locals idx)
  (unless (< idx (length locals))
    (error 'wasmdown:wasm-trap :kind :unreachable
           :message "local index out of range"))
  (svref locals idx))

(defun %trap (kind format-control &rest args)
  (error 'wasmdown:wasm-trap :kind kind
         :message (apply #'format nil format-control args)))

;;; ---------------------------------------------------------------
;;; Call depth: guest recursion is host recursion here, and a fatal,
;;; uncatchable SBCL "Control stack exhausted" crash must never leak
;;; out. Count guest call frames against *CALL-DEPTH-LIMIT* instead.

(defvar *call-depth* 0
  "Guest call frames currently executing, counting the top-level
 INTERP-INVOKE frame as 1. Restored to its caller's value by every
 frame's UNWIND-PROTECT exit, so each top-level invoke starts from a
 clean state even after a trapped (or host-thrown) unwind.")
(declaim (type fixnum *call-depth*))

(defvar *call-depth-limit* 1500
  "Maximum guest call nesting. Reaching it traps :call-stack-exhausted
 (CONTRACTS.md) instead of exhausting the host control stack, whose
 SBCL error is fatal and uncatchable. 1500 sits far below the ~150k
 host frames a default SBCL control stack tolerates, leaving ample
 margin for deeply nested non-recursive programs (block frames add
 host frames too).")
(declaim (type (and fixnum unsigned-byte) *call-depth-limit*))

(declaim (inline %call-depth-enter))
(defun %call-depth-enter ()
  (let ((depth (1+ *call-depth*)))
    (when (>= depth *call-depth-limit*)
      (%trap :call-stack-exhausted
             "call depth limit ~d exceeded"
             *call-depth-limit*))
    depth))

(declaim (inline %call-depth-exit))
(defun %call-depth-exit (saved)
  (setf *call-depth* saved))

;;; ---------------------------------------------------------------
;;; Constant expressions (globals' inits, segment offsets, elem exprs).

(defun interp-const-expr (instance expr)
  "Evaluate a constant expression EXPR (vector of instr incl. the
 trailing :end) against INSTANCE. Supported: i32.const/i64.const/
 f32.const/f64.const/global.get (imported or any already-instantiated
 global)/ref.null/ref.func, plus extended-const i32/i64 add/sub/mul
 (Core 3.0) with numops semantics. Returns the single value."
  (let ((stack '()))
    (loop for instr across expr
          for op = (wasm-instr-op instr)
          until (eq op :end)
          do (case op
               (:i32.const
                (let ((v (getf (wasm-instr-imms instr) :value)))
                  (push (if (< v 0) (+ v #x100000000) v) stack)))
               (:i64.const
                (let ((v (getf (wasm-instr-imms instr) :value)))
                  (push (if (< v 0) (+ v #x10000000000000000) v) stack)))
               (:f32.const
                (push (f32-from-bits (getf (wasm-instr-imms instr) :bits)) stack))
               (:f64.const
                (push (f64-from-bits (getf (wasm-instr-imms instr) :bits)) stack))
               (:global.get
                (let ((g (svref (wasmdown.runtime:module-instance-globals instance)
                                (getf (wasm-instr-imms instr) :global))))
                  (push (wasmdown.runtime:global-value g) stack)))
               (:ref.null
                (push nil stack))
               (:ref.func
                (push (svref (wasmdown.runtime:module-instance-funcs instance)
                             (getf (wasm-instr-imms instr) :func))
                      stack))
               ;; Extended-const (Core 3.0): i32/i64 add/sub/mul with
               ;; numops semantics (the same functions %numops uses).
               (t
                (let ((entry (gethash op *numops*)))
                  (unless entry
                    (error 'wasmdown:validation-error
                           :message (format nil
                                           "not a constant expression: ~a"
                                           op)))
                  ;; pop args in wasm order: the first popped is
                  ;; the top (second operand); numops takes (a b).
                  (let ((nargs (car entry))
                        (fn (cdr entry))
                        (args '()))
                    (dotimes (i nargs)
                      (push (pop stack) args))
                    (push (apply fn args) stack))))))
    (first stack)))

;;; ---------------------------------------------------------------
;;; Effective addresses and memory access.

(defun %memarg (instr)
  "Effective address = popped i32 address + the memarg's static offset
 (both in unsigned 32-bit domain; the sum stays an exact integer)."
  (let ((imms (wasm-instr-imms instr)))
    (values (getf imms :offset 0) (getf imms :memidx 0))))

(defun %mem-at (memidx)
  (let ((mems (wasmdown.runtime:module-instance-memories *frame-module*)))
    (if (< memidx (length mems))
        (svref mems memidx)
        (%trap :out-of-bounds-memory "unknown memory ~d" memidx))))

(defun %table-at (tableidx)
  (let ((tabs (wasmdown.runtime:module-instance-tables *frame-module*)))
    (if (< tableidx (length tabs))
        (svref tabs tableidx)
        (%trap :out-of-bounds-table "unknown table ~d" tableidx))))

(defun %func-at (funcidx)
  (let ((funcs (wasmdown.runtime:module-instance-funcs *frame-module*)))
    (if (< funcidx (length funcs))
        (svref funcs funcidx)
        (%trap :unreachable "unknown function ~d" funcidx))))

(defun %data-state-at (dataidx)
  (let ((segs (wasmdown.runtime::module-instance-data-segs *frame-module*)))
    (if (< dataidx (length segs))
        (svref segs dataidx)
        (%trap :out-of-bounds-memory "unknown data segment ~d" dataidx))))

(defun %elem-state-at (elemidx)
  (let ((segs (wasmdown.runtime::module-instance-elem-segs *frame-module*)))
    (if (< elemidx (length segs))
        (svref segs elemidx)
        (%trap :out-of-bounds-table "unknown element segment ~d" elemidx))))

;;; ---------------------------------------------------------------
;;; Numeric operations: op keyword -> numops call on the popped args.
;;; OPS maps op -> (nargs . function).

(defun %numop-table ()
  (let ((h (make-hash-table :test #'eq)))
    (macrolet
        ((def (op fn &optional (nargs 2))
           `(setf (gethash ,op h) (cons ,nargs (fdefinition ,fn)))))
      ;; i32
      (def :i32.add 'i32-add) (def :i32.sub 'i32-sub)
      (def :i32.mul 'i32-mul)
      (def :i32.div_s 'i32-div-s) (def :i32.div_u 'i32-div-u)
      (def :i32.rem_s 'i32-rem-s) (def :i32.rem_u 'i32-rem-u)
      (def :i32.and 'i32-and) (def :i32.or 'i32-or)
      (def :i32.xor 'i32-xor)
      (def :i32.shl 'i32-shl) (def :i32.shr_s 'i32-shr-s)
      (def :i32.shr_u 'i32-shr-u)
      (def :i32.rotl 'i32-rotl) (def :i32.rotr 'i32-rotr)
      (def :i32.clz 'i32-clz 1) (def :i32.ctz 'i32-ctz 1)
      (def :i32.popcnt 'i32-popcnt 1)
      (def :i32.eqz 'i32-eqz 1) (def :i32.eq 'i32-eq)
      (def :i32.ne 'i32-ne) (def :i32.lt_s 'i32-lt-s)
      (def :i32.lt_u 'i32-lt-u) (def :i32.gt_s 'i32-gt-s)
      (def :i32.gt_u 'i32-gt-u) (def :i32.le_s 'i32-le-s)
      (def :i32.le_u 'i32-le-u) (def :i32.ge_s 'i32-ge-s)
      (def :i32.ge_u 'i32-ge-u)
      ;; i64
      (def :i64.add 'i64-add) (def :i64.sub 'i64-sub)
      (def :i64.mul 'i64-mul)
      (def :i64.div_s 'i64-div-s) (def :i64.div_u 'i64-div-u)
      (def :i64.rem_s 'i64-rem-s) (def :i64.rem_u 'i64-rem-u)
      (def :i64.and 'i64-and) (def :i64.or 'i64-or)
      (def :i64.xor 'i64-xor)
      (def :i64.shl 'i64-shl) (def :i64.shr_s 'i64-shr-s)
      (def :i64.shr_u 'i64-shr-u)
      (def :i64.rotl 'i64-rotl) (def :i64.rotr 'i64-rotr)
      (def :i64.clz 'i64-clz 1) (def :i64.ctz 'i64-ctz 1)
      (def :i64.popcnt 'i64-popcnt 1)
      (def :i64.eqz 'i64-eqz 1) (def :i64.eq 'i64-eq)
      (def :i64.ne 'i64-ne) (def :i64.lt_s 'i64-lt-s)
      (def :i64.lt_u 'i64-lt-u) (def :i64.gt_s 'i64-gt-s)
      (def :i64.gt_u 'i64-gt-u) (def :i64.le_s 'i64-le-s)
      (def :i64.le_u 'i64-le-u) (def :i64.ge_s 'i64-ge-s)
      (def :i64.ge_u 'i64-ge-u)
      ;; f32
      (def :f32.abs 'f32-abs 1) (def :f32.neg 'f32-neg 1)
      (def :f32.ceil 'f32-ceil 1) (def :f32.floor 'f32-floor 1)
      (def :f32.trunc 'f32-trunc 1)
      (def :f32.nearest 'f32-nearest 1)
      (def :f32.sqrt 'f32-sqrt 1)
      (def :f32.add 'f32-add) (def :f32.sub 'f32-sub)
      (def :f32.mul 'f32-mul) (def :f32.div 'f32-div)
      (def :f32.min 'f32-min) (def :f32.max 'f32-max)
      (def :f32.copysign 'f32-copysign)
      (def :f32.eq 'f32-eq) (def :f32.ne 'f32-ne)
      (def :f32.lt 'f32-lt) (def :f32.gt 'f32-gt)
      (def :f32.le 'f32-le) (def :f32.ge 'f32-ge)
      ;; f64
      (def :f64.abs 'f64-abs 1) (def :f64.neg 'f64-neg 1)
      (def :f64.ceil 'f64-ceil 1) (def :f64.floor 'f64-floor 1)
      (def :f64.trunc 'f64-trunc 1)
      (def :f64.nearest 'f64-nearest 1)
      (def :f64.sqrt 'f64-sqrt 1)
      (def :f64.add 'f64-add) (def :f64.sub 'f64-sub)
      (def :f64.mul 'f64-mul) (def :f64.div 'f64-div)
      (def :f64.min 'f64-min) (def :f64.max 'f64-max)
      (def :f64.copysign 'f64-copysign)
      (def :f64.eq 'f64-eq) (def :f64.ne 'f64-ne)
      (def :f64.lt 'f64-lt) (def :f64.gt 'f64-gt)
      (def :f64.le 'f64-le) (def :f64.ge 'f64-ge)
      ;; conversions
      (def :i32.wrap_i64 'i32-wrap-i64 1)
      (def :i64.extend_i32_s 'i64-extend-i32-s 1)
      (def :i64.extend_i32_u 'i64-extend-i32-u 1)
      (def :i32.trunc_f32_s 'i32-trunc-f32-s 1)
      (def :i32.trunc_f32_u 'i32-trunc-f32-u 1)
      (def :i32.trunc_f64_s 'i32-trunc-f64-s 1)
      (def :i32.trunc_f64_u 'i32-trunc-f64-u 1)
      (def :i64.trunc_f32_s 'i64-trunc-f32-s 1)
      (def :i64.trunc_f32_u 'i64-trunc-f32-u 1)
      (def :i64.trunc_f64_s 'i64-trunc-f64-s 1)
      (def :i64.trunc_f64_u 'i64-trunc-f64-u 1)
      (def :f32.convert_i32_s 'f32-convert-i32-s 1)
      (def :f32.convert_i32_u 'f32-convert-i32-u 1)
      (def :f32.convert_i64_s 'f32-convert-i64-s 1)
      (def :f32.convert_i64_u 'f32-convert-i64-u 1)
      (def :f64.convert_i32_s 'f64-convert-i32-s 1)
      (def :f64.convert_i32_u 'f64-convert-i32-u 1)
      (def :f64.convert_i64_s 'f64-convert-i64-s 1)
      (def :f64.convert_i64_u 'f64-convert-i64-u 1)
      (def :f32.demote_f64 'f32-demote-f64 1)
      (def :f64.promote_f32 'f64-promote-f32 1)
      (def :i32.reinterpret_f32 'i32-reinterpret-f32 1)
      (def :i64.reinterpret_f64 'i64-reinterpret-f64 1)
      (def :f32.reinterpret_i32 'f32-reinterpret-i32 1)
      (def :f64.reinterpret_i64 'f64-reinterpret-i64 1)
      ;; sign extension
      (def :i32.extend8_s 'i32-s 1)
      (def :i32.extend16_s 'i32-s 1)
      (def :i64.extend8_s 'i64-s 1)
      (def :i64.extend16_s 'i64-s 1)
      (def :i64.extend32_s 'i64-s 1)
      ;; saturating truncation
      (def :i32.trunc_sat_f32_s 'i32-trunc-sat-f32-s 1)
      (def :i32.trunc_sat_f32_u 'i32-trunc-sat-f32-u 1)
      (def :i32.trunc_sat_f64_s 'i32-trunc-sat-f64-s 1)
      (def :i32.trunc_sat_f64_u 'i32-trunc-sat-f64-u 1)
      (def :i64.trunc_sat_f32_s 'i64-trunc-sat-f32-s 1)
      (def :i64.trunc_sat_f32_u 'i64-trunc-sat-f32-u 1)
      (def :i64.trunc_sat_f64_s 'i64-trunc-sat-f64-s 1)
      (def :i64.trunc_sat_f64_u 'i64-trunc-sat-f64-u 1))
    h))

(defparameter *numops* (%numop-table)
  "Op keyword -> (nargs . numops function).")

;;; ---------------------------------------------------------------
;;; Memory load/store widths: op -> (width-bytes . signed-p).

(defun %load-widths ()
  (let ((h (make-hash-table :test #'eq)))
    (flet ((add (op width signedp)
             (setf (gethash op h) (cons width signedp))))
      (add :i32.load 4 nil) (add :i64.load 8 nil)
      (add :f32.load 4 nil) (add :f64.load 8 nil)
      (add :i32.load8_s 1 t) (add :i32.load8_u 1 nil)
      (add :i32.load16_s 2 t) (add :i32.load16_u 2 nil)
      (add :i64.load8_s 1 t) (add :i64.load8_u 1 nil)
      (add :i64.load16_s 2 t) (add :i64.load16_u 2 nil)
      (add :i64.load32_s 4 t) (add :i64.load32_u 4 nil))
    h))

(defparameter *load-widths* (%load-widths))

(defun %store-widths ()
  (let ((h (make-hash-table :test #'eq)))
    (flet ((add (op width)
             (setf (gethash op h) width)))
      (add :i32.store 4) (add :i64.store 8)
      (add :f32.store 4) (add :f64.store 8)
      (add :i32.store8 1) (add :i32.store16 2)
      (add :i64.store8 1) (add :i64.store16 2) (add :i64.store32 4))
    h))

(defparameter *store-widths* (%store-widths))

;;; ---------------------------------------------------------------
;;; Block types -> result arity.

(defun %blocktype-arity (bt)
  "Number of results a block/loop/if of blocktype BT produces."
  (cond ((null bt) 0)
        ((and (consp bt) (eq (car bt) :typeidx))
         (length (wasm-func-type-results
                  (svref (wasm-module-types (wasmdown.runtime:module-instance-module *frame-module*))
                         (cdr bt)))))
        (t 1)))                          ; a single valtype result

(defun %blocktype-param-arity (bt)
  "Number of parameters a typed block consumes from the stack."
  (if (and (consp bt) (eq (car bt) :typeidx))
      (length (wasm-func-type-params
               (svref (wasm-module-types (wasmdown.runtime:module-instance-module *frame-module*))
                      (cdr bt))))
      0))

;;; ---------------------------------------------------------------
;;; Execution core. A (block ...) instruction runs its body in a
;;; recursive EXEC call; a br inside unwinds by THROWing a unique tag
;;; object caught by the target block/loop frame. Branch depth counts
;;; innermost-first within the current function body, so each block
;;; frame intercepts only depth 0 relative to itself and decrements
;;; for outer blocks.
;;;
;;; Block/loop semantics:
;;; - br to a BLOCK exits with the block's top N results.
;;; - br to a LOOP restarts it with its top P params.

(defstruct block-frame
  ;; The struct instance itself is the frame's catch tag: fresh per
  ;; block execution, EQ-unique, and allocated anyway — no GENSYM
  ;; string formatting (the old hot-path cost) and no counter state.
  kind                                     ; :block or :loop
  params                                   ; count of stack values consumed on entry
  results                                  ; count of results on br/fallthrough
  (height 0 :type fixnum))                 ; operand-stack depth below the frame's params

;;; Branch protocol: THROW of (list tag vals rest) to the target
;;; frame, where VALS are the branch operands in TOP-AT-HEAD order
;;; (the natural order of the stack representation) and REST is the
;;; truncated stack below them. Stack-rebuilding catches prepend VALS
;;; directly; INTERP-INVOKE reverses once to the wasm-call push-order
;;; results convention.

(defun %br-depth-throw (depth stack block-stack)
  "Perform a branch at relative DEPTH from the innermost block. Pops the
branch operands per the target frame's arity (loop: params; block:
results) and TRUNCATES the stack to the target frame's entry height:
the bottom HEIGHT values of the below-operands stack are the frame's
below-stack (values the target frame was entered above, in the flat
body-stack model); anything above them belongs to intervening frames
and is discarded — spec: br unwinds the operand stack to the branch
target. Returns the operands plus the truncated remainder via THROW."
  (let ((frame (nth depth block-stack)))
    (unless frame
      (%trap :unreachable "branch depth ~d exceeds control stack" depth))
    (let* ((nvals (if (eq (block-frame-kind frame) :loop)
                      (block-frame-params frame)
                      (block-frame-results frame)))
           (vals (subseq stack 0 nvals))
           (base (block-frame-height frame))
           (rest (nthcdr nvals stack))
           ;; list tail-to-head, top-at-head: the frame's below-stack
           ;; is the LAST BASE elements of REST, not the first.
           (rest (if (> (length rest) base)
                     (subseq rest (- (length rest) base))
                     rest)))
      (throw frame (values vals rest)))))

(defun %exec-seq (instrs locals block-stack stack)
  "Execute a straight-line instruction vector (a block body with the
 trailing :end removed). Returns the resulting operand stack (Lisp list,
 top at head). Branches exit via THROW to the enclosing block frames."
  (loop with s = stack
        for instr across instrs
        for op = (wasm-instr-op instr)
        for imms = (wasm-instr-imms instr)
        do (case op
             ((:end)
              ;; function body terminator: nothing to do
              )
             (:nop)
             (:unreachable
              (%trap :unreachable "unreachable executed"))
             ;; ---- stack ----
             (:drop
              (pop-val s)
              (setf s (cdr s)))
            (:select
             (multiple-value-bind (cond-v s1) (pop-val s)
               (multiple-value-bind (b s2) (pop-val s1)
                 (multiple-value-bind (a s3) (pop-val s2)
                   (setf s (push (if (zerop cond-v) b a) s3))))))
             ;; ---- variables ----
             (:local.get
              (push (%local-ref locals (getf imms :local)) s))
             (:local.set
              (multiple-value-bind (v s1) (pop-val s)
                (setf (svref locals (getf imms :local)) v)
                (setf s s1)))
             (:local.tee
              (multiple-value-bind (v s1) (pop-val s)
                (setf (svref locals (getf imms :local)) v)
                (push v s1)))
             (:global.get
              (push (wasmdown.runtime:global-value
                     (svref (wasmdown.runtime:module-instance-globals *frame-module*)
                            (getf imms :global)))
                    s))
             (:global.set
              (multiple-value-bind (v s1) (pop-val s)
                (setf (wasmdown.runtime:global-value
                       (svref (wasmdown.runtime:module-instance-globals *frame-module*)
                              (getf imms :global)))
                      v)
                (setf s s1)))
             ;; ---- constants ----
             (:i32.const
              (let ((v (getf imms :value)))
                (push (if (< v 0) (+ v #x100000000) v) s)))
             (:i64.const
              (let ((v (getf imms :value)))
                (push (if (< v 0) (+ v #x10000000000000000) v) s)))
             (:f32.const (push (f32-from-bits (getf imms :bits)) s))
             (:f64.const (push (f64-from-bits (getf imms :bits)) s))
             ;; ---- control flow ----
             (:block
              (let* ((bt (getf imms :bt))
                     (nparams (%blocktype-param-arity bt))
                     (nresults (%blocktype-arity bt))
                     (below (nthcdr nparams s))
                     (frame (make-block-frame :kind :block
                                              :params nparams :results nresults
                                              :height (length below))))
                (multiple-value-bind (vals leftover)
                    (catch frame
                      ;; Flat model (as :loop below): the body runs on
                      ;; the FULL entry stack — params on top of the
                      ;; below-params stack. Validated code never pops
                      ;; below its params, and a br recovers each
                      ;; frame's below-stack from the stack's bottom.
                      ;; Fallthrough: the body's top NRESULTS values
                      ;; are the block results, stacked on BELOW. The
                      ;; fallthrough path returns (:fallthrough
                      ;; new-stack); a br THROW supplies (values
                      ;; branch-operands truncated-below). Wasm values
                      ;; are never keywords, so the marker is
                      ;; unambiguous.
                      (let ((s2 (%exec-seq (wasm-instr-body instr) locals
                                           (cons frame block-stack) s)))
                        (values :fallthrough
                                (append (subseq s2 0 nresults) below))))
                  (if (eq vals :fallthrough)
                      (setf s leftover)
                      ;; br to this block: VALS are the branch operands
                      ;; (top-at-head), LEFTOVER the below-stack.
                      (setf s (append vals leftover))))))
             (:loop
              (let* ((bt (getf imms :bt))
                     (nparams (%blocktype-param-arity bt))
                     (nresults (%blocktype-arity bt))
                     (frame (make-block-frame :kind :loop
                                              :params nparams :results nresults
                                              :height (- (length s) nparams))))
                (setf s (loop
                  (multiple-value-bind (vals leftover)
                      (catch frame
                        ;; one iteration; fallthrough finishes the
                        ;; loop: its top NRESULTS values are the
                        ;; results, stacked on the stack below the
                        ;; loop's params. Fallthrough returns
                        ;; (:fallthrough new-stack); a br to the loop
                        ;; THROWs the new params to restart it.
                        (let ((s2 (%exec-seq (wasm-instr-body instr) locals
                                             (cons frame block-stack) s)))
                          (values :fallthrough
                                  (append (subseq s2 0 nresults)
                                          (nthcdr nparams s)))))
                    (if (eq vals :fallthrough)
                        ;; finished: the loop's result stack — the
                        ;; body's results stacked below the consumed
                        ;; params — NOT the entry stack (a resultful
                        ;; loop would drop its results; a resultless
                        ;; loop is unchanged since the tail IS the
                        ;; entry stack minus params).
                        (return leftover)
                        ;; br to the loop: restart with the branch
                        ;; operands as the new loop params.
                        (setf s (append vals leftover))))))))
             (t
              (multiple-value-setq (s) (%exec-other op instr imms locals block-stack s))))
        finally (return s)))


;;; ---------------------------------------------------------------
;;; Remaining opcode families.

(defun %mem-addr (instr s)
  "Pop the i32 address, add the memarg's static offset. Returns
 (values ea memidx new-stack)."
  (multiple-value-bind (addr s1) (pop-val s)
    (let* ((imms (wasm-instr-imms instr))
           (ea (+ addr (getf imms :offset 0))))
      (values ea (getf imms :memidx 0) s1))))

(defun %load-op (instr s)
  (multiple-value-bind (ea memidx s1) (%mem-addr instr s)
    (let* ((mem (%mem-at memidx))
           (spec (gethash (wasm-instr-op instr) *load-widths*))
           (width (car spec))
           (signedp (cdr spec))
           (op (wasm-instr-op instr))
           (bits (* width 8))
           ;; Result-type domain: 32-bit ops produce u32, 64-bit u64.
           (result-bits (case op
                          ((:i32.load8_s :i32.load8_u :i32.load16_s
                            :i32.load16_u :i32.load :f32.load) 32)
                          (t 64)))
           (raw (wasmdown.runtime:memory-load mem ea width)))
      (values (cond ((member op '(:f32.load))
                     (wasmdown.numops:f32-from-bits raw))
                    ((member op '(:f64.load))
                     (wasmdown.numops:f64-from-bits raw))
                    (signedp
                     ;; Sign-extend from the load width, then wrap into
                     ;; the unsigned domain of the RESULT type.
                     (let ((sv (if (>= raw (ash 1 (1- bits)))
                                   (- raw (ash 1 bits))
                                   raw)))
                       (mod sv (ash 1 result-bits))))
                    (t raw))
              s1))))

(defun %store-op (instr s)
  ;; Spec stack order is [addr, val] with the VALUE on top: pop the
  ;; value first, then the address.
  (multiple-value-bind (value s1) (pop-val s)
    (multiple-value-bind (ea memidx s2) (%mem-addr instr s1)
      (let ((op (wasm-instr-op instr)))
        (wasmdown.runtime:memory-store (%mem-at memidx) ea
                                       (gethash op *store-widths*)
                                       (cond ((member op '(:f32.store))
                                              (wasmdown.numops:f32-bits value))
                                             ((member op '(:f64.store))
                                              (wasmdown.numops:f64-bits value))
                                             (t value))))
      s2)))

(defun %do-call (func s)
  "Pop the callee's args (reverse push order), call through the shared
 trampoline, push the results."
  (let* ((type (wasmdown.runtime:func-instance-type func))
         (nparams (length (wasm-func-type-params type)))
         (args (reverse (subseq s 0 nparams)))
         (s1 (nthcdr nparams s))
         (results (apply #'wasmdown.runtime:wasm-call func args)))
    (append (reverse results) s1)))

(defun %exec-other (op instr imms locals block-stack s)
  "Dispatch every opcode family not handled inline in %EXEC-SEQ.
 Returns the new operand stack."
  (declare (type list s))
  (cond
    ;; ---- if / else ----
    ((eq op :if)
     (multiple-value-bind (cond-v s1) (pop-val s)
       (let* ((bt (getf imms :bt))
              (nparams (%blocktype-param-arity bt))
              (nresults (%blocktype-arity bt))
              (below (nthcdr nparams s1))
              (frame (make-block-frame :kind :block
                                       :params nparams :results nresults
                                       :height (length below))))
         (multiple-value-bind (vals leftover)
             (catch frame
               ;; Flat model (as :block/:loop): the chosen body runs on
               ;; the FULL stack after the condition — params on top of
               ;; the below-params stack.
               (let ((body (if (zerop cond-v)
                               (or (getf imms :else-body) #())
                               (wasm-instr-body instr))))
                 (let ((s2 (%exec-seq body locals (cons frame block-stack) s1)))
                   (return-from %exec-other
                     (append (subseq s2 0 nresults) below))))
               nil)
           (declare (type list vals leftover))
           (append vals leftover)))))
    ;; ---- branches. Depth 0 = the innermost enclosing block frame,
    ;; whose entry is at the head of BLOCK-STACK.
    ((eq op :br)
     (%br-depth-throw (getf imms :label) s block-stack)
     (%trap :unreachable "br fell through"))
    ((eq op :br-if)
     (multiple-value-bind (cond-v s1) (pop-val s)
       (if (zerop cond-v)
           s1
           (progn (%br-depth-throw (getf imms :label) s1 block-stack)
                  (%trap :unreachable "br_if fell through")))))
    ((eq op :br-table)
     (multiple-value-bind (idx s1) (pop-val s)
       (let* ((targets (getf imms :targets))
              (default (getf imms :default))
              (n (length targets))
              (label (if (< idx n) (svref targets idx) default)))
         (%br-depth-throw label s1 block-stack)
         (%trap :unreachable "br_table fell through"))))
    ((eq op :return)
     ;; return unwinds the whole function: throw to a sentinel frame
     ;; installed by interp-invoke
    (throw *return-tag*
      (values (subseq s 0 *frame-nresults*) nil)))
    ;; ---- calls ----
    ((eq op :call)
     (%do-call (%func-at (getf imms :func)) s))
    ((eq op :call-indirect)
     (let* ((tableidx (getf imms :table))
            (typeidx (getf imms :type))
            (table (%table-at tableidx)))
       (unless (eq (wasmdown.runtime:table-instance-type table) :funcref)
         (%trap :indirect-call-type-mismatch
                "call_indirect on non-funcref table"))
       (multiple-value-bind (elemidx s1) (pop-val s)
         (let ((ref (wasmdown.runtime:table-get table elemidx)))
           (unless (wasmdown.runtime:func-instance-p ref)
             (%trap :null-reference
                    "call_indirect on null reference ~d" elemidx))
           ;; structural type check against the expected type
           (let* ((expected (svref (wasm-module-types
                                   (wasmdown.runtime:module-instance-module *frame-module*))
                                  typeidx))
                  (actual (wasmdown.runtime:func-instance-type ref)))
             (unless (and (equal (wasm-func-type-params expected)
                                 (wasm-func-type-params actual))
                          (equal (wasm-func-type-results expected)
                                 (wasm-func-type-results actual)))
               (%trap :indirect-call-type-mismatch
                      "call_indirect type mismatch at index ~d" elemidx)))
           (%do-call ref s1)))))
    ;; ---- references ----
    ((eq op :ref.null) (push nil s))
    ((eq op :ref.is-null)
     (multiple-value-bind (v s1) (pop-val s)
       (push (if v 0 1) s1)))
    ((eq op :ref.func)
     (push (%func-at (getf imms :func)) s))
    ;; ---- table ----
    ((eq op :table.get)
     (multiple-value-bind (idx s1) (pop-val s)
       (push (wasmdown.runtime:table-get (%table-at (getf imms :table)) idx) s1)))
    ((eq op :table.set)
     (multiple-value-bind (idx s1) (pop-val s)
       (multiple-value-bind (ref s2) (pop-val s1)
         (wasmdown.runtime:table-set (%table-at (getf imms :table)) idx ref)
         s2)))
    ;; ---- memory ----
    ((gethash op *load-widths*)
     (multiple-value-bind (v s1) (%load-op instr s)
       (push v s1)))
    ((gethash op *store-widths*) (%store-op instr s))
    ((eq op :memory.size)
     (push (wasmdown.runtime:memory-size (%mem-at (getf imms :memidx 0))) s))
    ((eq op :memory.grow)
     (multiple-value-bind (delta s1) (pop-val s)
       (push (wasmdown.runtime:memory-grow (%mem-at (getf imms :memidx 0))
                                            delta)
             s1)))
    ((eq op :memory.fill)
     (multiple-value-bind (len s1) (pop-val s)
       (multiple-value-bind (val s2) (pop-val s1)
         (multiple-value-bind (dst s3) (pop-val s2)
           (wasmdown.runtime:memory-fill (%mem-at (getf imms :memidx 0))
                                         dst val len)
           s3))))
    ((eq op :memory.copy)
     ;; (memory-copy mem dst src len): this slice implements the
     ;; same-memory form; distinct memidx operands share one instance
     ;; only when both indices address the same memory, which the
     ;; spec restricts to equal indices anyway.
     (multiple-value-bind (len s1) (pop-val s)
       (multiple-value-bind (src s2) (pop-val s1)
         (multiple-value-bind (dst s3) (pop-val s2)
           (let ((dst-idx (getf imms :dst 0))
                 (src-idx (getf imms :src 0)))
             (unless (= dst-idx src-idx)
               (%trap :out-of-bounds-memory
                      "memory.copy across distinct memories unsupported"))
             (wasmdown.runtime:memory-copy (%mem-at dst-idx) dst src len))
           s3))))
    ((eq op :memory.init)
     (multiple-value-bind (len s1) (pop-val s)
       (multiple-value-bind (src s2) (pop-val s1)
         (multiple-value-bind (dst s3) (pop-val s2)
           (wasmdown.runtime:memory-init (%mem-at (getf imms :memidx 0))
                                          (%data-state-at (getf imms :data))
                                          dst src len)
           s3))))
    ((eq op :data.drop)
     (wasmdown.runtime:drop-data *frame-module* (getf imms :data))
     s)
    ((eq op :table.init)
     (multiple-value-bind (len s1) (pop-val s)
       (multiple-value-bind (src s2) (pop-val s1)
         (multiple-value-bind (dst s3) (pop-val s2)
           (wasmdown.runtime:table-init (%table-at (getf imms :table))
                                         (%elem-state-at (getf imms :elem))
                                         dst src len)
           s3))))
    ((eq op :elem.drop)
     (wasmdown.runtime:drop-elem *frame-module* (getf imms :elem))
     s)
    ((eq op :table.grow)
     (multiple-value-bind (delta s1) (pop-val s)
       (multiple-value-bind (ref s2) (pop-val s1)
         (push (wasmdown.runtime:table-grow (%table-at (getf imms :table))
                                             ref delta)
               s2))))
    ((eq op :table.size)
     (push (wasmdown.runtime:table-size (%table-at (getf imms :table))) s))
    ((eq op :table.fill)
     (multiple-value-bind (len s1) (pop-val s)
       (multiple-value-bind (ref s2) (pop-val s1)
         (multiple-value-bind (dst s3) (pop-val s2)
           (wasmdown.runtime:table-fill (%table-at (getf imms :table))
                                        dst ref len)
           s3))))
    ((eq op :table.copy)
     (multiple-value-bind (len s1) (pop-val s)
       (multiple-value-bind (src s2) (pop-val s1)
         (multiple-value-bind (dst s3) (pop-val s2)
           (let ((dst-idx (getf imms :dst 0))
                 (src-idx (getf imms :src 0)))
             (unless (= dst-idx src-idx)
               (%trap :out-of-bounds-table
                      "table.copy across distinct tables unsupported"))
             (wasmdown.runtime:table-copy (%table-at dst-idx) dst src len))
           s3))))
    ;; ---- numeric ----
    ((gethash op *numops*)
     (let* ((entry (gethash op *numops*))
            (nargs (car entry))
            (fn (cdr entry)))
       (multiple-value-bind (args s1) (%pop-n s nargs)
         (push (apply fn args) s1))))
    (t
     (%trap :unreachable "unimplemented opcode ~a" op))))

;;; ---------------------------------------------------------------
;;; Function invocation. *RETURN-TAG* / *FRAME-NRESULTS* are defined
;;; at the top of this file.

(defun %default-value (valtype)
  (case valtype
    (:i32 0) (:i64 0)
    (:f32 (f32-from-bits 0)) (:f64 (f64-from-bits 0))
    (:funcref nil) (:externref nil)
    (t (if (and (consp valtype) (eq (first valtype) :ref))
            nil
            (%trap :unreachable "no default for type ~a" valtype)))))

(defun %locals-vector (func args)
  "Build the locals vector: parameters first (in order), then declared
 locals expanded by count, all default-initialized."
  (let* ((type (svref (wasm-module-types (wasmdown.runtime:module-instance-module *frame-module*))
                      (wasm-func-typeidx func)))
         (params (wasm-func-type-params type))
         (declared (loop for (valtype . count) in (wasm-func-locals func)
                         append (make-list count :initial-element valtype)))
         (all (append params declared))
         (v (make-array (length all))))
    (loop for val in args
          for i from 0
          do (setf (svref v i) val))
    (loop for valtype in (nthcdr (length args) all)
          for i from (length args)
          do (setf (svref v i) (%default-value valtype)))
    v))

(defun interp-invoke (func args)
  "Execute a :wasm func-instance with ARGS (a list in push order).
 Returns the results list. This is the reference implementation the
 runtime's *INVOKE-WASM-FUNC* hook points at."
  (let* ((module (wasmdown.runtime:func-instance-module func))
         (funcidx (wasmdown.runtime:func-instance-wasm-funcidx func))
         (wasm-fn (svref (wasm-module-funcs
                          (wasmdown.runtime:module-instance-module module))
                         funcidx))
         (type (svref (wasm-module-types
                       (wasmdown.runtime:module-instance-module module))
                      (wasm-func-typeidx wasm-fn)))
         (saved-depth *call-depth*)
         (frame-depth (%call-depth-enter)))
    ;; Decrement on any exit (normal, trap, or host throw): the
    ;; dynamic *CALL-DEPTH* binding restores the caller's value by
    ;; construction; the UNWIND-PROTECT setf keeps the counter
    ;; explicit so the global can never stay inflated across
    ;; top-level invokes.
    (unwind-protect
         (let ((*frame-module* module)
               (*frame-funcidx* funcidx)
               (*call-depth* frame-depth))
           (let* ((locals (%locals-vector wasm-fn args))
                  ;; function-level frame: a br past every block targets
                  ;; it and exits the function with its results. The
                  ;; frame doubles as the catch tag (no GENSYM string
                  ;; formatting in this hot path); *RETURN-TAG* holds it
                  ;; for the :return handler.
                  (fn-frame (make-block-frame :kind :block
                                              :params (length (wasm-func-type-params type))
                                              :results (length (wasm-func-type-results type))))
                  (*return-tag* fn-frame)
                  (*frame-nresults* (block-frame-results fn-frame)))
             (multiple-value-bind (vals leftover)
                 (catch fn-frame
                   ;; fallthrough: top NRESULTS values are the results
                   (let ((s (%exec-seq (wasm-func-body wasm-fn) locals
                                       (list fn-frame) '())))
                     (return-from interp-invoke
                       (reverse (subseq s 0 *frame-nresults*))))
                   nil)
               ;; br/return targeting the function frame: VALS arrive
               ;; top-at-head; wasm-call wants push order.
               (declare (ignore leftover))
               (reverse vals))))
      (%call-depth-exit saved-depth))))

(defun interp-func (func &rest args)
  "Convenience: call a func-instance through the trampoline."
  (apply #'wasmdown.runtime:wasm-call func args))

;;; ---------------------------------------------------------------
;;; Hook installation: the interpreter becomes the default executor
;;; the moment this file is loaded. Codegen replaces
;;; WASMDOWN.RUNTIME:*INVOKE-WASM-FUNC* later; const-expr evaluation
;;; always uses the interpreter (it is the reference semantics).

(setf wasmdown.runtime::*invoke-wasm-func* #'interp-invoke)
(setf wasmdown.runtime::*const-expr-evaluator* #'interp-const-expr)
