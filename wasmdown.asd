;;;; WASMDOWN — a Common Lisp–native WebAssembly runtime.
;;;; System definition. Zero mandatory third-party dependencies.

(asdf:defsystem #:wasmdown
  :description "WASMDOWN: a Common Lisp–native WebAssembly runtime (partial core slice)."
  :version "0.0.1"
  :licence "TBD"
  :depends-on ()
  :serial t
  :components
  ((:file "src/packages")
   (:file "src/conditions")
   (:file "src/types")
   (:file "src/numops")
   (:file "src/simd")
   (:file "src/binary")
   (:file "src/component")
   (:file "src/binary-encode")
   (:file "src/validate")
   (:file "src/runtime")
   (:file "src/interp")
   (:file "src/codegen")
   (:file "src/api")
   (:file "src/cli")
   (:file "src/text")
   (:file "src/wast")
   (:file "src/component-runtime")
   (:file "src/component-text")
   (:file "src/wit")
   (:file "src/wasi")))

(asdf:defsystem #:wasmdown/test
  :description "WASMDOWN unit and integration tests."
  :depends-on (#:wasmdown)
  :serial t
  :components
  ((:file "tests/harness")
   (:file "tests/numops")
   (:file "tests/binary")
   (:file "tests/validate")
   (:file "tests/runtime")
   (:file "tests/interp")
   (:file "tests/codegen")
   (:file "tests/api")
   (:file "tests/text")
   (:file "tests/simd")
   (:file "tests/wast")
   (:file "tests/wit")
   (:file "tests/wasi")))
