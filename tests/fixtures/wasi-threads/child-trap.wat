;; A trap in a CHILD terminates the main thread blocked in
;; memory.atomic.wait32, so the host reports a trap instead of hanging.
(module
  (import "wasi_snapshot_preview1" "wasi_thread_spawn"
          (func $spawn (param i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit"
          (func $exit (param i32)))
  (memory (export "memory") 1 1 shared)

  (func (export "wasi_thread_start") (param $tid i32) (param $arg i32)
    (drop (memory.atomic.wait32 (i32.const 0) (i32.const 0)
                                (i64.const 20000000)))
    (unreachable))

  (func (export "_start")
    (if (i32.le_s (call $spawn (i32.const 0)) (i32.const 0))
      (then (call $exit (i32.const 1))))
    (drop (memory.atomic.wait32 (i32.const 0) (i32.const 0)
                                (i64.const -1)))
    (call $exit (i32.const 2))))
