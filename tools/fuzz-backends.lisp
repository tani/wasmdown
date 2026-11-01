;;;; Differential property/fuzz lane: generated core modules run through
;;;; BOTH explicitly selected backends and must agree on results, traps and
;;;; observable memory.
;;;;
;;;; This is an independent oracle for backend parity, not a conformance
;;;; claim: the modules are generated here from a typed WAT builder, so a
;;;; shared semantic bug can pass. What it catches is what a fixed suite
;;;; cannot: shapes the hand-written pins never combined.
;;;;
;;;; Determinism: a xorshift64 PRNG seeded per iteration; a failure prints
;;;; its seed and WAT, so `--iterations 1 --seed <n>` reproduces it.
;;;;
;;;; Usage:
;;;;   sbcl --script tools/fuzz-backends.lisp [--iterations N] [--seed S]
;;;;                                          [--verbose]
;;;; Exit: 0 when every generated case agrees, 1 on any divergence or
;;;; generator error.

(require :asdf)

(defparameter *root*
  (make-pathname :directory (butlast (pathname-directory *load-truename*))))

(pushnew *root* asdf:*central-registry* :test #'equal)

(asdf:load-system :wasmdown)

(defun %option (argv flag default)
  (let ((pos (position flag argv :test #'string=)))
    (if (and pos (< (1+ pos) (length argv)))
        (nth (1+ pos) argv)
        default)))

(defparameter *argv* (rest sb-ext:*posix-argv*))
(defparameter *iterations* (parse-integer (%option *argv* "--iterations" "300")))
(defparameter *seed0* (parse-integer (%option *argv* "--seed" "20261101")))
(defparameter *verbose* (and (position "--verbose" *argv* :test #'string=) t))

;;; ---------------------------------------------------------------
;;; A deterministic PRNG (xorshift64*), so a seed fully reproduces a case.

(defun xorshift (state)
  (let* ((s (ldb (byte 64 0) state)))
    (setf s (logxor s (ash s -12))
          s (logxor s (ldb (byte 64 0) (ash s 25)))
          s (logxor s (ash s -27)))
    (ldb (byte 64 0) s)))

(defun rnd (state n)
  (setf (car state) (xorshift (car state)))
  (mod (ash (car state) -11) n))

(defun pick (state items) (nth (rnd state (length items)) items))

;;; ---------------------------------------------------------------
;;; Typed WAT generation: every node is an i32-valued expression over the
;;; function's params (0,1) and — when the caller allows — locals 2,3.
;;; Constants stay small so addresses never leave the page.

(defparameter *ops* '("i32.add" "i32.sub" "i32.mul"
                      "i32.and" "i32.or" "i32.xor"
                      "i32.shl" "i32.shr_s" "i32.shr_u"
                      "i32.rotl" "i32.rotr"))

(defun leaf (state nlocals)
  (append (list "(local.get 0)" "(local.get 1)")
          (when (> nlocals 2) (list "(local.get 2)" "(local.get 3)"))
          (list (format nil "(i32.const ~d)" (rnd state 256)))))

(defun gen-expr (state depth nlocals)
  "An i32 expression. NLOCALS is the function's usable local count
\(2 for a params-only helper, 4 for the generated export body)."
  (if (zerop depth)
      (pick state (leaf state nlocals))
      (let ((sub (lambda () (gen-expr state (1- depth) nlocals))))
        (case (rnd state 6)
          (0 (format nil "(~a ~a ~a)" (pick state *ops*) (funcall sub) (funcall sub)))
          ;; trapping forms: both backends must agree on the trap kind
          (1 (format nil "(i32.div_s ~a ~a)" (funcall sub)
                     (pick state (list "(i32.const 1)" "(i32.const 0)"
                                       "(i32.const -1)"))))
          (2 (format nil "(if (result i32) (i32.ne (i32.and ~a (i32.const 1))
                                                   (i32.const 0))
                          (then ~a) (else ~a))"
                     (funcall sub) (funcall sub) (funcall sub)))
          ;; a value-less inner block with an early exit; the outer block
          ;; supplies this expression's value
          (3 (format nil "(block (result i32)
                          (block (br_if 0 (i32.ne (i32.and ~a (i32.const 2))
                                                  (i32.const 0))))
                          ~a)"
                     (funcall sub) (funcall sub)))
          (4 (format nil "(select ~a ~a ~a)" (funcall sub) (funcall sub) (funcall sub)))
          (t ;; store then load back, inside a block that yields the load
           (format nil "(block (result i32)
                          (i32.store offset=~d (i32.and ~a (i32.const 65532)) ~a)
                          (i32.load offset=~d (i32.and ~a (i32.const 65532))))"
                   (* 4 (rnd state 4)) (funcall sub) (funcall sub)
                   (* 4 (rnd state 4)) (funcall sub)))))))

(defun gen-module (state)
  "A module with a helper (params only), a 1-page memory, and an export
`f` with two params and two locals whose body is the generated expression."
  (let* ((helper (format nil "(func $h (param i32 i32) (result i32) ~a)"
                         (gen-expr state 2 2)))
         (set0 (format nil "(local.set 2 ~a)" (gen-expr state 2 4)))
         (set1 (format nil "(local.set 3 ~a)" (gen-expr state 2 4)))
         (body (gen-expr state 4 4)))
    (format nil "(module (memory 1) ~a
                   (func (export \"f\") (param i32 i32) (result i32)
                     (local i32 i32)
                     ~a ~a
                     ~a))"
            helper set0 set1 body)))

;;; ---------------------------------------------------------------
;;; Execution and comparison.

(defun call (mode module a b)
  "Run export `f` under MODE: (:ok results memory-checksum) or (:trap kind)."
  (handler-case
      (let* ((inst (wasmdown:instantiate module :mode mode))
             (results (wasmdown:invoke inst "f" a b))
             (mem (first (coerce (wasmdown.runtime:module-instance-memories inst)
                                 'list)))
             (bytes (and mem (wasmdown.runtime:memory-instance-bytes mem)))
             (sum (loop for i below (min 64 (length (or bytes #())))
                        sum (aref bytes i))))
        (list :ok results sum))
    (wasmdown:wasm-trap (c) (list :trap (wasmdown:wasm-trap-kind c)))))

(defun run-case (state seed)
  (let* ((wat (gen-module state))
         (module (handler-case
                     (wasmdown:validate-module (wasmdown.text:parse-wat wat))
                   (error (e)
                     (format t "~&GENERATOR ERROR seed=~d~%~a~%~a~%" seed wat e)
                     (return-from run-case :error))))
         (a (rnd state 1000))
         (b (rnd state 1000))
         (i (call :interpret module a b))
         (c (call :compiled module a b)))
    (when *verbose*
      (format t "~&seed=~d a=~d b=~d interpret=~a compiled=~a~%" seed a b i c))
    (if (equal i c)
        :ok
        (progn
          (format t "~&DIVERGENCE seed=~d a=~d b=~d~%  interpret=~s~%  compiled=~s~%~a~%"
                  seed a b i c wat)
          :diverge))))

;;; ---------------------------------------------------------------
;;; Main.

(let ((ok 0) (bad 0) (errors 0) (diverge-seeds '()))
  (dotimes (n *iterations*)
    (let* ((seed (+ *seed0* n))
           (state (list (logior (ash (ldb (byte 64 0) seed) 1) 1))))
      (dotimes (i 4) (xorshift (car state)))  ; decorrelate nearby seeds
      (case (run-case state seed)
        (:ok (incf ok))
        (:diverge (incf bad) (push seed diverge-seeds))
        (:error (incf errors)))))
  (format t "~&fuzz: ~d cases — ~d agree, ~d diverge, ~d generator errors~%"
          *iterations* ok bad errors)
  (when diverge-seeds
    (format t "  reproduce: --iterations 1 --seed ~d~%" (first diverge-seeds)))
  (sb-ext:exit :code (if (or (plusp bad) (plusp errors)) 1 0)))
