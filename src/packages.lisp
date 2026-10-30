;;;; Package definitions. Internal packages are flat files for now; the
;;;; subsystem boundaries of PLAN.md §4 are enforced by dependency order
;;;; in wasmdown.asd and the contracts in docs/CONTRACTS.md.

(defpackage #:wasmdown
  ;; Public API + conditions. Functions live in src/api.lisp.
  (:use #:cl)
  (:export #:wasmdown-error
           #:wasmdown-error-message
           #:decode-error
           #:decode-error-offset
           #:validation-error
           #:link-error
           #:instantiation-error
           #:wasm-trap
           #:wasm-trap-kind
           #:wasm-trap-message
           #:wasm-exception
           #:wasm-exception-tag
           #:wasm-exception-payload
           #:resource-exhausted
           #:host-error
           ;; API (implemented in src/api.lisp)
           #:read-module
           #:module-bytes
           #:validate-module
           #:instantiate
           #:invoke
           #:make-imports
           #:imports-set-function
           #:instance-export
           #:module-features
           #:run-string))

(defpackage #:wasmdown.types
  (:use #:cl)
  (:export
   ;; func types
   #:make-wasm-func-type #:wasm-func-type #:wasm-func-type-p
   #:wasm-func-type-params #:wasm-func-type-results
   #:copy-wasm-func-type
   ;; limits
   #:make-wasm-limits #:wasm-limits #:wasm-limits-p
   #:wasm-limits-min #:wasm-limits-max #:wasm-limits-has-max-p
   #:wasm-limits-i64-p
   #:copy-wasm-limits
   ;; table / memory types
   #:make-wasm-table #:wasm-table #:wasm-table-p
   #:wasm-table-type #:wasm-table-limits #:copy-wasm-table
   #:make-wasm-memory #:wasm-memory #:wasm-memory-p
   #:wasm-memory-limits #:copy-wasm-memory
   ;; globals
   #:make-wasm-global #:wasm-global #:wasm-global-p
   #:wasm-global-type #:wasm-global-mutable #:wasm-global-init
   #:copy-wasm-global
   ;; imports / exports
   #:make-wasm-import #:wasm-import #:wasm-import-p
   #:wasm-import-module #:wasm-import-field #:wasm-import-kind
   #:wasm-import-type #:copy-wasm-import
   #:make-wasm-export #:wasm-export #:wasm-export-p
   #:wasm-export-name #:wasm-export-kind #:wasm-export-index
   #:copy-wasm-export
   ;; functions
   #:make-wasm-func #:wasm-func #:wasm-func-p
   #:wasm-func-typeidx #:wasm-func-locals #:wasm-func-body
   #:wasm-func-offset #:copy-wasm-func
   ;; segments
   #:make-wasm-elem #:wasm-elem #:wasm-elem-p
   #:wasm-elem-mode #:wasm-elem-type #:wasm-elem-inits #:copy-wasm-elem
   #:wasm-elem-from-table-init
   #:make-wasm-data #:wasm-data #:wasm-data-p
   #:wasm-data-mode #:wasm-data-bytes #:copy-wasm-data
   #:make-wasm-custom #:wasm-custom #:wasm-custom-p
   #:wasm-custom-name #:wasm-custom-bytes #:wasm-custom-offset
   #:copy-wasm-custom
   ;; instructions
   #:make-wasm-instr #:wasm-instr #:wasm-instr-p
   #:wasm-instr-op #:wasm-instr-imms #:wasm-instr-body #:wasm-instr-offset
   #:copy-wasm-instr
   ;; module
   #:make-wasm-module #:wasm-module #:wasm-module-p
   #:wasm-module-types #:wasm-module-imports #:wasm-module-funcs
   #:wasm-module-tables #:wasm-module-mems #:wasm-module-globals
   #:wasm-module-exports #:wasm-module-start #:wasm-module-elems
   #:wasm-module-datas #:wasm-module-customs #:wasm-module-data-count
   #:wasm-module-tags
   #:copy-wasm-module
   ;; helpers
   #:func-type-arity-matches-p
   #:valtype-width
   #:defaultable-valtype-p
   #:typed-ref-p #:typed-ref-nullable-p #:typed-ref-heap
   #:reftype-sub-p
   ;; recursion groups + iso-recursive equivalence (loop 20)
   #:make-wasm-comptype #:wasm-comptype #:wasm-comptype-p
   #:wasm-comptype-kind #:wasm-comptype-heap
   #:wasm-comptype-fields #:wasm-comptype-params
   #:wasm-comptype-results
   #:make-wasm-rectype-group #:wasm-rectype-group #:wasm-rectype-group-p
   #:wasm-rectype-group-types
   #:wasm-rec-type #:wasm-rec-type-p
   #:wasm-rec-type-comptype #:wasm-rec-type-group
   #:wasm-rec-type-group-idx #:wasm-rec-type-group-start
   #:make-rec-type-entry #:make-singleton-rec-type #:wrap-func-type-as-rec
   #:module-type-comptype #:rec-group-of
   #:type-equiv-p #:func-type-match-p #:func-type-equal-p))

(defpackage #:wasmdown.numops
  (:use #:cl)
  (:export #:i32-> #:i32-wrap #:i64-> #:i64-wrap
           #:i32-s #:i32-u #:i64-s #:i64-u
           #:i32-add #:i32-sub #:i32-mul
           #:i32-div-s #:i32-div-u #:i32-rem-s #:i32-rem-u
           #:i32-and #:i32-or #:i32-xor #:i32-shl #:i32-shr-s #:i32-shr-u
           #:i32-rotl #:i32-rotr #:i32-clz #:i32-ctz #:i32-popcnt
           #:i32-eqz #:i32-eq #:i32-ne #:i32-lt-s #:i32-lt-u #:i32-gt-s
           #:i32-gt-u #:i32-le-s #:i32-le-u #:i32-ge-s #:i32-ge-u
           #:i64-add #:i64-sub #:i64-mul
           #:i64-div-s #:i64-div-u #:i64-rem-s #:i64-rem-u
           #:i64-and #:i64-or #:i64-xor #:i64-shl #:i64-shr-s #:i64-shr-u
           #:i64-rotl #:i64-rotr #:i64-clz #:i64-ctz #:i64-popcnt
           #:i64-eqz #:i64-eq #:i64-ne #:i64-lt-s #:i64-lt-u #:i64-gt-s
           #:i64-gt-u #:i64-le-s #:i64-le-u #:i64-ge-s #:i64-ge-u
           #:f32-abs #:f32-neg #:f32-ceil #:f32-floor #:f32-trunc
           #:f32-nearest #:f32-sqrt #:f32-add #:f32-sub #:f32-mul
           #:f32-div #:f32-min #:f32-max #:f32-copysign
           #:f32-eq #:f32-ne #:f32-lt #:f32-gt #:f32-le #:f32-ge
           #:f64-abs #:f64-neg #:f64-ceil #:f64-floor #:f64-trunc
           #:f64-nearest #:f64-sqrt #:f64-add #:f64-sub #:f64-mul
           #:f64-div #:f64-min #:f64-max #:f64-copysign
           #:f64-eq #:f64-ne #:f64-lt #:f64-gt #:f64-le #:f64-ge
           #:i32-wrap-i64 #:i64-extend-i32-s #:i64-extend-i32-u
           #:i32-extend8-s #:i32-extend16-s
           #:i64-extend8-s #:i64-extend16-s #:i64-extend32-s
           #:i32-trunc-f32-s #:i32-trunc-f32-u
           #:i32-trunc-f64-s #:i32-trunc-f64-u
           #:i64-trunc-f32-s #:i64-trunc-f32-u
           #:i64-trunc-f64-s #:i64-trunc-f64-u
           #:f32-convert-i32-s #:f32-convert-i32-u
           #:f32-convert-i64-s #:f32-convert-i64-u
           #:f64-convert-i32-s #:f64-convert-i32-u
           #:f64-convert-i64-s #:f64-convert-i64-u
           #:f32-demote-f64 #:f64-promote-f32
           #:i32-reinterpret-f32 #:i64-reinterpret-f64
           #:f32-reinterpret-i32 #:f64-reinterpret-i64
           #:i32-trunc-sat-f32-s #:i32-trunc-sat-f32-u
           #:i32-trunc-sat-f64-s #:i32-trunc-sat-f64-u
           #:i64-trunc-sat-f32-s #:i64-trunc-sat-f32-u
           #:i64-trunc-sat-f64-s #:i64-trunc-sat-f64-u
           #:f32-bits #:f32-from-bits #:f64-bits #:f64-from-bits
           #:nan-p #:canonical-nan-p #:arithmetic-nan-p
           #:quiet-nan #:ftrunc #:fnearest #:fmin #:fmax
           #:float-eq-nan-lax
           #:mem-load-le #:mem-store-le))

(defpackage #:wasmdown.binary
  (:use #:cl #:wasmdown.types)
  (:export #:decode-module #:encode-module #:decode-error-at
           #:reader-offset #:make-reader
           #:read-u32 #:read-u64 #:read-s32 #:read-s33 #:read-s64
           #:read-name #:read-byte-at #:read-bytes
           ;; M7 (component layer): the shared reader primitives the
           ;; component decoder builds on — bounds-checked windowed
           ;; reads, the hostile-count-checked vector reader and the
           ;; peek used by the head-byte dispatches. OFF-LIMIT /
           ;; PEEK-BYTE-AT / READ-VEC / READ-BYTE-VEC are part of the
           ;; wasmdown.binary contract per PLAN.md §4 (the component
           ;; reader shares the core's decoder discipline; these were
           ;; unexported internal helpers before the component slice).
           #:reader-bytes #:reader-limit #:peek-byte-at #:read-vec #:read-byte-vec
           ;; loop 24 (SIMD): the 0xFD decoder routes memarg/lane
           ;; immediates through the host reader; %read-memarg gets an
           ;; exported alias for the same grammar. The ENCODER side
           ;; mirrors it (wasmdown.simd:encode-simd-imms).
           #:read-memarg
           #:enc-byte #:enc-u32 #:enc-memarg))

(defpackage #:wasmdown.validate
  (:use #:cl #:wasmdown.types)
  (:export #:validate-module #:validate-function-body
           #:const-expr-type #:describe-module-features))

(defpackage #:wasmdown.runtime
  (:use #:cl #:wasmdown.types)
  (:export
   #:make-instance-store
   #:make-module-instance
   #:module-instance
           #:module-instance-memories #:module-instance-tables
           #:module-instance-globals #:module-instance-funcs
           #:module-instance-tags
           #:module-instance-module #:module-instance-exports
           #:tag-instance #:tag-instance-p #:make-tag-instance
           #:tag-instance-type #:tag-instance-module
   #:func-instance #:func-instance-p
   #:func-instance-type #:func-instance-module
   #:func-instance-kind #:func-instance-wasm-funcidx
   #:func-instance-host-fn
   #:make-wasm-func-instance #:make-host-func-instance
   #:memory-instance #:memory-instance-p
   #:memory-instance-bytes #:memory-instance-pages
   #:memory-instance-max-pages #:memory-instance-has-max-p
   #:make-memory-instance
   #:memory-load #:memory-store #:memory-size #:memory-grow #:memory-fill
   #:memory-copy #:memory-copy2 #:memory-init
   #:table-instance #:table-instance-p
   #:table-instance-elems #:table-instance-type #:table-instance-max
   #:table-instance-has-max-p
   #:make-table-instance #:table-get #:table-set #:table-grow
   #:table-size #:table-fill #:table-copy #:table-init
   #:drop-elem #:drop-data #:elem-dropped-p #:data-dropped-p
   #:take-elem-ref #:take-data-bytes
   #:global-instance #:global-instance-p
   #:make-global-instance #:global-value #:global-set-value
   ;; GC value objects (loop 22): struct/array/i31 heap values.
   #:wasm-struct-instance #:wasm-struct-instance-p
   #:wasm-struct-instance-typeidx #:wasm-struct-instance-fields
   #:make-wasm-struct-value #:struct-field-count
   #:struct-field-get #:struct-field-set
   #:wasm-array-instance #:wasm-array-instance-p
   #:wasm-array-instance-typeidx #:wasm-array-instance-contents
   #:make-wasm-array-value #:array-length
   #:array-element-get #:array-element-set
   #:make-i31 #:i31-get-s #:i31-get-u #:i31-p
   #:export-value
   #:wasm-call))

(defpackage #:wasmdown.interp
  (:use #:cl #:wasmdown.types #:wasmdown.numops)
  (:export #:interp-func #:interp-invoke #:interp-const-expr))

(defpackage #:wasmdown.codegen
  (:use #:cl #:wasmdown.types)
  (:export #:compile-module-functions #:compiled-func
           #:reset-codegen))

(defpackage #:wasmdown.cli
  (:use #:cl)
  (:export #:main))

;;; ---- M16: the WIT frontend + the WASI host-provider framework ----
;;; The component subsystem's packages are defined at their own file
;;; heads (src/component.lisp, src/component-runtime.lisp); these two
;;; are listed here because they are the M16 public surface. Both use
;;; only CL (the component ct constructors are referenced by their
;;; package-qualified names), so there is no load-order dependency.

(defpackage #:wasmdown.wit
  (:use #:cl)
  (:export
   ;; reader entry points
   #:parse-wit-file
   #:parse-wit-string
   #:wit-error
   #:wit-error-path
   #:wit-error-offset
   ;; package nodes
   #:wit-package
   #:wit-package-p
   #:wit-package-name
   #:wit-package-namespace
   #:wit-package-basename
   #:wit-package-version
   #:wit-package-interfaces
   #:wit-package-worlds
   #:wit-package-annotations
   #:package-interface
   ;; interface nodes
   #:wit-interface
   #:wit-interface-p
   #:wit-interface-name
   #:wit-interface-package
   #:wit-interface-items
   #:wit-interface-types
   #:wit-interface-funcs
   #:wit-interface-func-list
   #:wit-interface-resources
   #:wit-interface-resource-list
   #:wit-resource
   #:wit-resource-p
   #:wit-resource-name
   #:wit-resource-methods
   #:wit-func-name
   #:wit-func-params
   #:wit-func-result
   #:wit-func-async-p
   #:wit-func-static-p
   #:wit-func-constructor-p
   #:wit-interface-uses
   #:wit-interface-annotations
   ;; type / func / resource / world nodes
   #:wit-type
   #:wit-type-p
   #:wit-type-name
   #:wit-type-expr
   #:wit-type-annotations
   #:wit-func
   #:wit-func-p
   #:wit-func-name
   #:wit-func-params
   #:wit-func-result
   #:wit-func-async-p
   #:wit-func-static-p
   #:wit-func-constructor-p
   #:wit-func-annotations
   #:wit-resource
   #:wit-resource-p
   #:wit-resource-name
   #:wit-resource-methods
   #:wit-resource-annotations
   #:wit-use
   #:wit-use-p
   #:wit-use-namespace
   #:wit-use-package
   #:wit-use-interface
   #:wit-use-version
   #:wit-use-items
   #:wit-use-renames
   #:wit-use-annotations
   #:wit-world
   #:wit-world-p
   #:wit-world-name
   #:wit-world-package
   #:wit-world-imports
   #:wit-world-exports
   #:wit-world-annotations
   #:wit-world-items
   #:wit-world-includes
   #:wit-world-uses
   #:wit-world-types
   #:wit-world-resource-cts
   #:wit-world-item
   #:wit-world-item-p
   #:wit-world-item-sort
   #:wit-world-item-kind
   #:wit-world-item-name
   #:wit-world-item-namespace
   #:wit-world-item-package
   #:wit-world-item-interface
   #:wit-world-item-version
   #:wit-world-item-target
   #:wit-world-item-implements
   #:wit-world-item-external-id
   #:wit-world-item-attributes
   #:wit-world-item-annotations
   #:wit-world-include
   #:wit-world-include-p
   #:wit-world-include-target
   #:wit-world-include-renames
   #:wit-world-include-annotations
   #:wit-annotation
   #:wit-annotation-p
   #:wit-annotation-name
   #:wit-annotation-args
   #:wit-annotation-values
   ;; type resolution / conversion
   #:resolve-wit-type
   #:wit-type->ct
   #:wit-func->ct
   #:interface-import-name
   ;; version selection (plan step 3)
   #:wit-version
   #:wit-version-p
   #:parse-wit-version
   #:wit-version<
   #:wit-version=
   #:wit-version<=
   #:wit-selection
   #:wit-selection-p
   #:make-wit-selection
   #:wit-selection-target-versions
   #:wit-selection-enabled-features
   #:validate-wit-package
   #:select-wit-package
   #:resolve-wit-world))

(defpackage #:wasmdown.wasi
  (:use #:cl)
  (:export
   ;; the capability context (PLAN.md §9.1: an explicit authority
   ;; object, never ambient Lisp globals)
   #:wasi-context
   #:wasi-context-p
   #:make-wasi-context
   #:wasi-context-arguments
   #:wasi-context-environment
   #:wasi-context-cwd
   #:wasi-context-stdin
   #:wasi-context-stdout
   #:wasi-context-stderr
   #:wasi-context-preopens
   #:wasi-context-networks
   ;; host exit
   #:wasi-exit
   #:wasi-exit-p
   #:wasi-exit-status
   ;; the host-provider framework
   #:provider-instance
   #:provider-instance-function
   #:make-host-lifted-func
   #:wasi-package
   #:wasi-package-file
   #:*wasi-wit-root*
   ;; clocks
   #:monotonic-now
   #:monotonic-resolution
   #:system-now
   #:system-resolution
   #:clocks-provider
   #:monotonic-clock-instance
   #:system-clock-instance
   ;; random (OS entropy only — never CL:RANDOM)
   #:os-random-bytes
   #:random-bytes
   #:insecure-random-bytes
   #:random-u64
   #:insecure-random-u64
   #:insecure-seed
   #:random-provider
   #:random-instance
   #:insecure-random-instance
   #:insecure-seed-instance
   ;; cli (environment + exit; the streams are slice 2)
   #:environment-get-environment
   #:environment-get-arguments
   #:environment-get-initial-cwd
   #:cli-environment-instance
   #:cli-exit-instance
   #:cli-provider
   ;; cli (M16 slice 6): the terminals, the command-world provider set,
   ;; the `run' runner, and the CLI-side authority helpers
   #:wasi-terminal
   #:wasi-terminal-p
   #:wasi-terminal-fd
   #:terminal-input-instance
   #:terminal-output-instance
   #:terminal-stdin-instance
   #:terminal-stdout-instance
   #:terminal-stderr-instance
   #:cli-terminal-provider
   #:command-world
   #:compose-world-providers
   #:command-provider
   #:run-component
   #:component-run-export
   #:parse-wasi-endpoint
   #:open-wasi-preopen
   #:close-wasi-preopen
   #:wasi-preopen-p
   #:wasi-preopen-name
   ;; sockets (M16 slice 4): the network authority, the tcp/udp
   ;; resources and ip-name-lookup
   #:wasi-network
   #:wasi-network-p
   #:make-wasi-network
   #:wasi-network-outbound
   #:wasi-network-listen
   #:wasi-network-dns
   #:wasi-network-allows-outbound-p
   #:wasi-network-allows-listen-p
   #:wasi-network-allows-dns-p
   #:wasi-endpoint
   #:wasi-endpoint-p
   #:make-wasi-endpoint
   #:wasi-endpoint-address
   #:wasi-endpoint-port
   #:wasi-tcp-socket
   #:wasi-tcp-socket-p
   #:wasi-tcp-socket-fd
   #:wasi-tcp-socket-state
   #:wasi-tcp-socket-family
   #:wasi-tcp-socket-network
   #:wasi-udp-socket
   #:wasi-udp-socket-p
   #:wasi-udp-socket-fd
   #:wasi-socket-error
   #:wasi-socket-error-code
   #:sockets-error-code
   #:tcp-socket-resource-ct
   #:udp-socket-resource-ct
   #:close-wasi-tcp-socket
   #:close-wasi-udp-socket
   #:tcp-create
   #:tcp-bind
   #:tcp-connect
   #:tcp-listen
   #:tcp-send
   #:tcp-receive
   #:tcp-get-local-address
   #:tcp-get-remote-address
   #:tcp-get-is-listening
   #:tcp-get-address-family
   #:tcp-set-listen-backlog-size
   #:udp-create
   #:udp-bind
   #:udp-connect
   #:udp-disconnect
   #:udp-send
   #:udp-receive
   #:udp-get-local-address
   #:udp-get-remote-address
   #:udp-get-address-family
   #:ip-name-lookup-resolve
   #:sockets-types-instance
   #:ip-name-lookup-instance
   #:sockets-provider
   #:ip-socket-address->sockaddr
   #:sockaddr->ip-socket-address
   ;; reactor backend (plan step 6): poll(2) + the monotonic clock +
   ;; the self-pipe wake channel, with injectable time/readiness
   #:wasi-reactor
   #:wasi-reactor-p
   #:make-wasi-reactor
   #:reactor-step
   #:reactor-watch-fd
   #:reactor-watch-deadline
   #:reactor-close
   #:reactor-wake))
