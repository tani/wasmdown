;;;; Integration tests for the public API (src/api.lisp) and CLI
;;;; (src/cli.lisp) per docs/design/PLAN.md §11. Modules are built programmatically
;;;; per src/types.lisp, encoded to real .wasm binaries, and run through
;;;; the public entry points; CLI paths are exercised in-process via
;;;; wasmdown.cli:main (no subprocess).

(defpackage #:wasmdown.test.api
  (:use #:cl #:wasmdown.types)
  (:nicknames #:wd.test.api))

(in-package #:wasmdown.test.api)

;;; src/api.lisp and src/cli.lisp are not yet in wasmdown.asd's
;;; component list (tracked by the system owner); load them relative
;;; to this test file when the packages are not already present.
(eval-when (:load-toplevel :execute)
  (unless (and (find-package :wasmdown)
               (find-symbol "INSTANTIATE" :wasmdown)
               (fboundp (find-symbol "INSTANTIATE" :wasmdown)))
    (let ((root (make-pathname
                 :directory (butlast (pathname-directory *load-truename*)))))
      (load (merge-pathnames "src/api" root))
      (load (merge-pathnames "src/cli" root))
      (unless (and (find-package :wasmdown.codegen)
                   (find-symbol "COMPILE-MODULE-FUNCTIONS" :wasmdown.codegen)
                   (fboundp (find-symbol "COMPILE-MODULE-FUNCTIONS"
                                         :wasmdown.codegen)))
        (let ((cg (merge-pathnames "src/codegen" root)))
          (when (probe-file cg) (load cg)))))))

;;; -----------------------------------------------------------------
;;; Builders.

(defun i (op &optional imms body)
  (make-wasm-instr op imms (or body #()) 0))

(defun ft (params results)
  (make-wasm-func-type params results))

(defun mmod (&rest keys &key types imports funcs tables mems globals
                 exports start elems datas &allow-other-keys)
  (apply #'make-wasm-module
         :types (or types #()) :imports (or imports #())
         :funcs (or funcs #()) :tables (or tables #())
         :mems (or mems #()) :globals (or globals #())
         :exports (or exports #()) :start start
         :elems (or elems #()) :datas (or datas #()) keys))

(defparameter *add-mod*
  (mmod
   :types (vector (ft '(:i32 :i32) '(:i32)))
   :funcs (vector (make-wasm-func 0 '()
                   (vector (i :local.get '(:local 0)) (i :local.get '(:local 1))
                           (i :i32.add) (i :end)) 0))
   :exports (vector (make-wasm-export "add" :func 0))))

(defparameter *trap-mod*
  (mmod
   :types (vector (ft '() '()))
   :funcs (vector (make-wasm-func 0 '() (vector (i :unreachable) (i :end)) 0))
   :exports (vector (make-wasm-export "boom" :func 0))))

(defparameter *imp-mod*
  (mmod
   :types (vector (ft '(:i32) '()) (ft '(:i32) '(:i32)) (ft '() '(:i32)))
   :imports (vector (make-wasm-import "env" "log" :func 0)
                    (make-wasm-import "env" "double" :func 1))
   :funcs (vector (make-wasm-func 2 '()
                   (vector (i :i32.const '(:value 21))
                           (i :call '(:func 1))
                           (i :call '(:func 0))
                           (i :i32.const '(:value 3))
                           (i :call '(:func 1))
                           (i :end)) 0))
   :exports (vector (make-wasm-export "run" :func 2))))

(defun add-bytes ()
  (wasmdown.binary:encode-module *add-mod*))

;;; -----------------------------------------------------------------
;;; read-module / module-bytes / run-string.

(wasmdown.test:test api/read-module-bytes
  (let ((m (wasmdown:read-module (add-bytes))))
    (wasmdown.test:is (= 1 (length (wasm-module-funcs m))))
    (wasmdown.test:is (= 1 (length (wasm-module-exports m))))
    (let ((m2 (wasmdown:read-module (wasmdown:module-bytes m))))
      (wasmdown.test:is
       (equal (wasm-func-type-params (svref (wasm-module-types m) 0))
              (wasm-func-type-params (svref (wasm-module-types m2) 0)))))))

(wasmdown.test:test api/read-module-file
  (let ((path (merge-pathnames "tmp-api-add.wasm" *default-pathname-defaults*)))
    (unwind-protect
         (progn
           (with-open-file (s path :element-type '(unsigned-byte 8)
                              :direction :output :if-exists :supersede)
             (write-sequence (add-bytes) s))
           (let ((m (wasmdown:read-module path)))
             (wasmdown.test:is (= 1 (length (wasm-module-funcs m))))
             (wasmdown.test:is (wasmdown.types:wasm-module-p
                               (wasmdown:read-module (namestring path))))))
      (when (probe-file path) (delete-file path)))))

(wasmdown.test:test api/read-module-decode-error
  (wasmdown.test:assert-raises wasmdown:decode-error
    (wasmdown:read-module #(0 1 2 3 1 0 0 0))))

(wasmdown.test:test api/run-string-hex
  (let ((m (wasmdown:run-string "0061736d 01000000")))
    (wasmdown.test:is (wasmdown.types:wasm-module-p m)))
  (wasmdown.test:assert-raises wasmdown:decode-error
    (wasmdown:run-string "00ff"))
  (wasmdown.test:assert-raises wasmdown:host-error
    (wasmdown:run-string "0g"))
  (wasmdown.test:assert-raises wasmdown:host-error
    (wasmdown:run-string "006")))

;;; -----------------------------------------------------------------
;;; validate-module.

(wasmdown.test:test api/validate-positive
  ;; validate-module returns its (decoded) module argument
  (let ((m (wasmdown:read-module (add-bytes))))
    (wasmdown.test:is (eq m (wasmdown:validate-module m)))))

(wasmdown.test:test api/validate-negative
  (wasmdown.test:assert-raises wasmdown:validation-error
    (wasmdown:validate-module
     (mmod :types (vector (ft '() '(:i32)))
           :funcs (vector (make-wasm-func 0 '()
                           (vector (i :i32.const '(:value 1))
                                   (i :i64.const '(:value 2))
                                   (i :i32.add) (i :end)) 0))))))

;;; -----------------------------------------------------------------
;;; instantiate / invoke.

(wasmdown.test:test api/invoke-add
  (let ((inst (wasmdown:instantiate *add-mod*)))
    (wasmdown.test:is (equal '(55) (wasmdown:invoke inst "add" 42 13)))))

(wasmdown.test:test api/invoke-wrong-arity-host-error
  (let ((inst (wasmdown:instantiate *add-mod*)))
    (wasmdown.test:assert-raises wasmdown:host-error
      (wasmdown:invoke inst "add" 1))))

(wasmdown.test:test api/invoke-trap-passthrough
  (let ((inst (wasmdown:instantiate *trap-mod*)))
    (wasmdown.test:assert-trap :unreachable
      (wasmdown:invoke inst "boom"))))

(wasmdown.test:test api/instantiate-rejects-unvalidated
  (wasmdown.test:assert-raises wasmdown:validation-error
    (wasmdown:instantiate
     (mmod :types (vector (ft '() '(:i32)))
           :funcs (vector (make-wasm-func 0 '()
                           (vector (i :i32.const '(:value 1))
                                   (i :i64.const '(:value 2))
                                   (i :i32.add) (i :end)) 0))
           :exports (vector (make-wasm-export "f" :func 0))))))

;;; -----------------------------------------------------------------
;;; imports + host side effects.

(wasmdown.test:test api/imports-set-function
  (let* ((calls '())
         (table (wasmdown:make-imports)))
    ;; imports-set-function returns the host func-instance
    (wasmdown.test:is (wasmdown.runtime:func-instance-p
                       (wasmdown:imports-set-function
                        table "env" "log"
                        (lambda (x) (push x calls)) '(:i32) '())))
    (wasmdown:imports-set-function table "env" "double"
      (lambda (x) (* 2 x)) '(:i32) '(:i32))
    (let ((inst (wasmdown:instantiate *imp-mod* :imports table)))
      (wasmdown.test:is (equal '(6) (wasmdown:invoke inst "run")))
      (wasmdown.test:is (equal '(42) (reverse calls)))))
  (wasmdown.test:assert-raises wasmdown:link-error
    (wasmdown:instantiate *imp-mod* :imports (wasmdown:make-imports)))
  (let ((bad (wasmdown:make-imports)))
    (wasmdown:imports-set-function bad "env" "log" (lambda (x) x) '(:i64) '())
    (wasmdown:imports-set-function bad "env" "double"
      (lambda (x) x) '(:i32) '(:i32))
    (wasmdown.test:assert-raises wasmdown:link-error
      (wasmdown:instantiate *imp-mod* :imports bad))))

(wasmdown.test:test api/imports-multi-value-and-no-result
  ;; host fn returning multiple values feeds a multi-result import type
  (let* ((mod (mmod
               :types (vector (ft '() '(:i32 :i32)))
               :imports (vector (make-wasm-import "env" "pair" :func 0))
               :funcs (vector (make-wasm-func 0 '()
                               (vector (i :call '(:func 0)) (i :end)) 0))
               :exports (vector (make-wasm-export "p" :func 1))))
         (tab (wasmdown:make-imports)))
    (wasmdown:imports-set-function tab "env" "pair"
      (lambda () (values 3 4)) '() '(:i32 :i32))
    (wasmdown.test:is (equal '(3 4) (wasmdown:invoke
                                     (wasmdown:instantiate mod :imports tab)
                                     "p"))))
  ;; host fn with a declared () result type: return value discarded
  (let* ((mod (mmod
               :types (vector (ft '() '()))
               :imports (vector (make-wasm-import "env" "side" :func 0))
               :funcs (vector (make-wasm-func 0 '()
                               (vector (i :call '(:func 0)) (i :end)) 0))
               :exports (vector (make-wasm-export "go" :func 1))))
         (tab (wasmdown:make-imports)))
    (wasmdown:imports-set-function tab "env" "side"
      (lambda () 99) '() '())
    (wasmdown.test:is (null (wasmdown:invoke
                             (wasmdown:instantiate mod :imports tab)
                             "go")))))

;;; -----------------------------------------------------------------
;;; instance-export / module-features.

(wasmdown.test:test api/instance-export
  (let ((inst (wasmdown:instantiate *add-mod*)))
    (wasmdown.test:is (wasmdown.runtime:func-instance-p
                       (wasmdown:instance-export inst "add")))
    (wasmdown.test:assert-raises wasmdown:link-error
      (wasmdown:instance-export inst "nope"))))

(wasmdown.test:test api/module-features
  (wasmdown.test:is (null (wasmdown:module-features (mmod))))
  (let ((m (mmod
            :types (vector (ft '() '(:i32 :i64)) (ft '() '(:i32)))
            :funcs (vector (make-wasm-func 0 '()
                            (vector (i :i32.const '(:value 1))
                                    (i :i32.extend8_s) (i :drop)
                                    (i :i32.trunc_sat_f32_s) (i :end)) 0))
            :mems (vector (make-wasm-memory (make-wasm-limits 1 2)))
            :tables (vector (make-wasm-table :funcref
                                             (make-wasm-limits 2)))
            :globals (vector (make-wasm-global :type :i32 :mutable nil
                                               :init (vector
                                                      (i :i32.const
                                                         '(:value 7))
                                                      (i :end))))
            :datas (vector (make-wasm-data '(:passive) #(1 2 3)))
            :data-count 1)))
    (wasmdown.test:is
     (equal '(:memory :table :globals :multi-value :sign-extension-ops
              :saturating-float-to-int :bulk-memory)
            (wasmdown:module-features m)))))

;;; -----------------------------------------------------------------
;;; Baseline codegen mode (only when the backend is loaded).

(defparameter *codegen-present-p*
  (and (find-package :wasmdown.codegen)
       (find-symbol "COMPILE-MODULE-FUNCTIONS" :wasmdown.codegen)
       (fboundp (find-symbol "COMPILE-MODULE-FUNCTIONS" :wasmdown.codegen))))

(when *codegen-present-p*
  (wasmdown.test:test api/compiled-mode
    (let ((inst (wasmdown:instantiate *add-mod* :mode :compiled)))
      (wasmdown.test:is (equal '(55) (wasmdown:invoke inst "add" 42 13))))
    (wasmdown.test:assert-trap :unreachable
      (wasmdown:invoke (wasmdown:instantiate *trap-mod* :mode :compiled)
                       "boom"))
    (let* ((mod (mmod
                 :types (vector (ft '(:i32) '(:i32)) (ft '() '(:i32)))
                 :imports (vector (make-wasm-import "env" "double" :func 0))
                 :funcs (vector (make-wasm-func 1 '()
                                 (vector (i :i32.const '(:value 5))
                                         (i :call '(:func 0)) (i :end)) 0))
                 :exports (vector (make-wasm-export "run" :func 1))))
           (tab (wasmdown:make-imports)))
      (wasmdown:imports-set-function tab "env" "double"
        (lambda (x) (* 2 x)) '(:i32) '(:i32))
      (wasmdown.test:is (equal '(10) (wasmdown:invoke
                                      (wasmdown:instantiate
                                       mod :imports tab :mode :compiled)
                                      "run"))))
    (let* ((mod (mmod
                 :types (vector (ft '() '(:i32)))
                 :funcs (vector (make-wasm-func 0 '()
                                 (vector (i :i32.const '(:value 42))
                                         (i :end)) 0)
                                (make-wasm-func 0 '()
                                 (vector (i :i32.const '(:value 0))
                                         (i :call-indirect
                                            '(:type 0 :table 0))
                                         (i :end)) 0))
                 :tables (vector (make-wasm-table :funcref
                                                  (make-wasm-limits 1 1)))
                 :elems (vector (make-wasm-elem
                                 (list :active 0
                                       (vector (i :i32.const '(:value 0))
                                               (i :end)))
                                 :funcref
                                 (list (vector (i :ref.func '(:func 0))
                                               (i :end)))))
                 :exports (vector (make-wasm-export "go" :func 1)))))
      (wasmdown.test:is (equal '(42) (wasmdown:invoke
                                      (wasmdown:instantiate
                                       mod :mode :compiled)
                                      "go"))))))

(when (not *codegen-present-p*)
  (wasmdown.test:test api/compiled-absent-honest-error
    (wasmdown.test:assert-raises wasmdown:host-error
      (wasmdown:instantiate *add-mod* :mode :compiled))))

;;; -----------------------------------------------------------------
;;; CLI (in-process via wasmdown.cli:main).

(defun %tmp-wasm (name bytes)
  (let ((path (merge-pathnames name *default-pathname-defaults*)))
    (with-open-file (s path :element-type '(unsigned-byte 8)
                       :direction :output :if-exists :supersede)
      (write-sequence bytes s))
    path))

(wasmdown.test:test api/cli-validate-ok
  (let ((path (%tmp-wasm "tmp-cli-add.wasm" (add-bytes))))
    (unwind-protect
         (wasmdown.test:is (eq 0 (wasmdown.cli:main
                                 (list "validate" (namestring path)))))
      (delete-file path))))

(wasmdown.test:test api/cli-validate-bad
  ;; missing file: usage error (exit 2)
  (wasmdown.test:is (eq 2 (wasmdown.cli:main
                           (list "validate" "tmp-cli-nope.wasm"))))
  ;; malformed binary: guest error (exit 1)
  (let ((path (%tmp-wasm "tmp-cli-bad.wasm" #(1 2 3 4 5 6 7 8))))
    (unwind-protect
         (wasmdown.test:is (eq 1 (wasmdown.cli:main
                                 (list "validate" (namestring path)))))
      (delete-file path))))

(wasmdown.test:test api/cli-inspect
  (let* ((path (%tmp-wasm "tmp-cli-insp.wasm" (add-bytes)))
         (out (with-output-to-string (s)
                (let ((*standard-output* s))
                  (wasmdown.test:is
                   (eq 0 (wasmdown.cli:main
                          (list "inspect" (namestring path)))))))))
    (delete-file path)
    (wasmdown.test:is (search "exports" out))
    (wasmdown.test:is (search "add" out))))

(wasmdown.test:test api/cli-run-invoke
  (let* ((path (%tmp-wasm "tmp-cli-run.wasm" (add-bytes)))
         (out (with-output-to-string (s)
                (let ((*standard-output* s))
                  (wasmdown.test:is
                   (eq 0 (wasmdown.cli:main
                          (list "run" (namestring path)
                                "--invoke" "add" "42" "13"))))))))
    (delete-file path)
    (wasmdown.test:is (search "55" out))))

(wasmdown.test:test api/cli-run-trap-exit-1
  (let ((path (%tmp-wasm "tmp-cli-trap.wasm"
                         (wasmdown.binary:encode-module *trap-mod*))))
    (unwind-protect
         (wasmdown.test:is (eq 1 (wasmdown.cli:main
                                 (list "run" (namestring path)
                                       "--invoke" "boom"))))
      (delete-file path))))

(wasmdown.test:test api/cli-usage-errors
  (wasmdown.test:is (eq 2 (wasmdown.cli:main nil)))
  (wasmdown.test:is (eq 2 (wasmdown.cli:main (list "frobnicate" "x"))))
  (wasmdown.test:is (eq 0 (wasmdown.cli:main (list "help"))))
  (wasmdown.test:is (eq 2 (wasmdown.cli:main
                           (list "validate" "--bogus" "x.wasm"))))
  (let ((path (%tmp-wasm "tmp-cli-int.wasm" (add-bytes))))
    (unwind-protect
         (wasmdown.test:is (eq 2 (wasmdown.cli:main
                                 (list "run" (namestring path)
                                       "--invoke" "add" "forty"))))
      (delete-file path))))

(wasmdown.test:test api/cli-compile
  ;; The emit path (codegen :emit) is exercised in a CHILD process:
  ;; the launcher is the product surface, and a crash inside codegen's
  ;; pprint of the emitted form (circular module-instance backpointer,
  ;; reported to the codegen owner) must fail the exit-code check
  ;; instead of killing the whole test run.
  (let ((path (%tmp-wasm "tmp-cli-comp.wasm" (add-bytes)))
        (out (merge-pathnames "tmp-cli-comp-out.lisp"
                              *default-pathname-defaults*))
        (script (merge-pathnames "tools/wasmdown.lisp"
                                 (make-pathname
                                  :directory
                                  (butlast (pathname-directory
                                            *load-truename*))))))
    (unwind-protect
         (if (probe-file script)
             (let* ((proc (sb-ext:run-program
                           (namestring (or (probe-file
                                            "/home/tani/.nix-profile/bin/sbcl")
                                           "sbcl"))
                           (list "--script" (namestring script)
                                 "compile" (namestring path)
                                 "--emit-lisp" (namestring out))
                           :output :stream :error :output :wait t))
                    (exit (sb-ext:process-exit-code proc)))
               (wasmdown.test:is (if *codegen-present-p* (eq 0 exit) (eq 1 exit)))
               (when (and *codegen-present-p* (eq 0 exit))
                 (wasmdown.test:is (probe-file out))))
             ;; no launcher in this checkout (unusual): fall back to
             ;; in-process and accept a codegen crash is possible
             (let ((code (wasmdown.cli:main
                          (list "compile" (namestring path)
                                "--emit-lisp" (namestring out)))))
               (wasmdown.test:is (if *codegen-present-p*
                                     (eq 0 code)
                                     (eq 1 code)))))
      (when (probe-file path) (delete-file path))
      (when (probe-file out) (delete-file out)))))

;;; -----------------------------------------------------------------
;;; M1: backend identity and instantiation ordering.
;;;
;;; The selected backend is a property of the FUNCTION, not of a
;;; replacement object: instantiation fills BACKEND/COMPILED-ENTRY on
;;; the defined func-instances and runs the Wasm start section through
;;; that backend. A refusal is a codegen-error raised before the start
;;; section and before active segments are applied.

(defun %wat-instance (text &key (mode :interpret) imports before-start)
  (wasmdown:instantiate (wasmdown:validate-module
                         (wasmdown.text:parse-wat text))
                        :mode mode :imports imports
                        :before-start before-start))

(when *codegen-present-p*
  (wasmdown.test:test api/compiled-start-section-runs-compiled
    ;; the start function sets a global; backend entry counters prove it
    ;; executed through the compiled entry, not the interpreter.
    (wasmdown.runtime:reset-backend-entry-counts)
    (let ((inst (%wat-instance
                 "(module (global $g (mut i32) (i32.const 0))
                          (func $init (global.set $g (i32.const 7)))
                          (start $init)
                          (func (export \"get\") (result i32)
                            (global.get $g)))"
                 :mode :compiled)))
      (wasmdown.test:is (equal '(7) (wasmdown:invoke inst "get")))
      (wasmdown.test:is (= 0 (wasmdown.runtime:backend-entry-count :interpret)))
      (wasmdown.test:is (>= (wasmdown.runtime:backend-entry-count :compiled) 2))
      (wasmdown.test:is (eq :compiled
                            (wasmdown.runtime:func-instance-backend
                             (wasmdown:instance-export inst "get"))))))

  (wasmdown.test:test api/compiled-compilation-precedes-start-and-segments
    ;; every defined function is compiled BEFORE the start section
    ;; runs: the start function's memory write is the observable, and
    ;; the v128 function (formerly the canonical compiler refusal) is
    ;; installed as compiled code and answers like the interpreter.
    (let* ((text "(module (import \"env\" \"mem\" (memory 1))
                         (func $init (i32.store (i32.const 4) (i32.const 1)))
                         (start $init)
                         (func (export \"f\") (result v128)
                           (v128.const i32x4 1 2 3 4)))")
           (module (wasmdown:validate-module (wasmdown.text:parse-wat text)))
           (mem (wasmdown.runtime:make-memory-instance
                 (wasmdown.types:make-wasm-limits 1)))
           (inst (wasmdown:instantiate module :mode :compiled
                                       :imports (list (cons '("env" "mem") mem)))))
      (wasmdown.test:is (= 1 (aref (wasmdown.runtime:memory-instance-bytes mem) 4)))
      (wasmdown.test:is (eq :compiled
                            (wasmdown.runtime:func-instance-backend
                             (wasmdown:instance-export inst "f"))))
      (wasmdown.test:is (equalp '(#(1 0 0 0 2 0 0 0 3 0 0 0 4 0 0 0))
                                (wasmdown:invoke inst "f")))))

  (wasmdown.test:test api/compiled-lowers-formerly-refused-simd-and-gc
    ;; the M4 coverage closure: v128.const and struct.new used to make
    ;; :mode :compiled refuse the whole module at instantiation. Both
    ;; backends now agree on the values. (The codegen-error path itself
    ;; is pinned in tests/codegen.lisp; every opcode the decoder and
    ;; validator can produce is lowered, so no valid module reaches it.)
    (let* ((text "(module (type $s (struct (field i32)))
                         (func (export \"f\") (result i32 v128)
                           (struct.get $s 0 (struct.new $s (i32.const 7)))
                           (v128.const i32x4 1 2 3 4)))")
           (i (wasmdown:invoke (%wat-instance text :mode :interpret) "f"))
           (b (wasmdown:invoke (%wat-instance text :mode :compiled) "f")))
      (wasmdown.test:is (equalp i b))))

  (wasmdown.test:test api/compiled-keeps-function-identity
    ;; one func-instance object serves the export, the table element and
    ;; ref.func; compilation fills its entry instead of replacing it.
    (let* ((inst (%wat-instance
                  "(module (table 2 funcref)
                           (elem (i32.const 0) $f)
                           (func $f (result i32) (i32.const 9))
                           (export \"f\" (func $f)))"
                  :mode :compiled))
           (exported (wasmdown:instance-export inst "f"))
           (tab (svref (wasmdown.runtime:module-instance-tables inst) 0)))
      (wasmdown.test:is (eq exported (wasmdown.runtime:table-get tab 0)))
      (wasmdown.test:is (eq :compiled
                            (wasmdown.runtime:func-instance-backend exported)))
      (wasmdown.test:is (functionp
                         (wasmdown.runtime:func-instance-compiled-entry exported)))))

  (wasmdown.test:test api/imported-wasm-function-keeps-its-backend
    (let* ((a (%wat-instance "(module (func (export \"f\") (result i32)
                                          (i32.const 5)))"
                             :mode :compiled))
           (fv (wasmdown:instance-export a "f")))
      (wasmdown.test:is (eq :compiled (wasmdown.runtime:func-instance-backend fv)))
      (wasmdown.runtime:reset-backend-entry-counts)
      (let ((b (%wat-instance
                "(module (import \"a\" \"f\" (func $f (result i32)))
                         (func (export \"g\") (result i32) (call $f)))"
                :mode :interpret
                :imports (list (cons '("a" "f") fv)))))
        (wasmdown.test:is (equal '(5) (wasmdown:invoke b "g")))
        ;; the imported object was not recompiled: it keeps its own
        ;; backend, while B's own function stays interpreted.
        (wasmdown.test:is (eq :interpret
                              (wasmdown.runtime:func-instance-backend
                               (wasmdown:instance-export b "g"))))
        (wasmdown.test:is (eq :compiled
                              (wasmdown.runtime:func-instance-backend fv)))
        (wasmdown.test:is (>= (wasmdown.runtime:backend-entry-count :compiled) 1)))))

  (wasmdown.test:test api/before-start-runs-once-before-start
    (dolist (mode '(:interpret :compiled))
      (let ((calls 0))
        (let ((inst (%wat-instance
                     "(module (global $g (mut i32) (i32.const 0))
                              (func $init (global.set $g (i32.const 1)))
                              (start $init)
                              (func (export \"get\") (result i32)
                                (global.get $g)))"
                     :mode mode
                     :before-start (lambda (i)
                                     (declare (ignorable i))
                                     (incf calls)))))
          (wasmdown.test:is (= 1 calls))
          (wasmdown.test:is (equal '(1) (wasmdown:invoke inst "get")))))))
)

;;; -----------------------------------------------------------------
;;; CLI capability context ownership (M8): the CLI builds (and closes)
;;; its own reactor, so clock waits and socket readiness have a watch to
;;; register on instead of failing for want of a capability.

(defun %cli-options (&rest tokens)
  "Parse TOKENS the way the `run' subcommand does."
  (multiple-value-bind (file invokes options guest-args)
      (wasmdown.cli::%parse-run-args tokens)
    (declare (ignore invokes))
    (values file options guest-args)))

(wasmdown.test:test api/cli-component-context-owns-a-reactor
  (multiple-value-bind (context reactor)
      (multiple-value-bind (file options guest-args)
          (%cli-options "app.wasm" "--wasi" "--" "arg")
        (wasmdown.cli::%component-context options guest-args file))
    (wasmdown.test:is (not (null reactor)))
    (wasmdown.test:is (eq reactor
                          (wasmdown.wasi:wasi-context-reactor context)))
    (unwind-protect
         ;; a live reactor can arm a deadline and be pumped once
         (let ((fired nil))
           (wasmdown.wasi:reactor-watch-deadline
            reactor 0 (lambda () (setf fired t)))
           (multiple-value-bind (progress pending)
               (wasmdown.wasi:reactor-step reactor 0)
             (declare (ignore pending))
             (wasmdown.test:is (not (null progress))))
           (wasmdown.test:is (eq t fired)))
      (wasmdown.wasi:reactor-close reactor)))
  ;; without --wasi there is no capability, hence no reactor
  (multiple-value-bind (file options guest-args)
      (%cli-options "app.wasm")
    (declare (ignore file guest-args))
    (multiple-value-bind (context reactor)
        (wasmdown.cli::%component-context options nil "app.wasm")
      (declare (ignore context))
      (wasmdown.test:is (null reactor)))))
