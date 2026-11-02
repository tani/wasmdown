;; Negative spawn: the module imports wasi_thread_spawn but does not
;; export the required wasi_thread_start entry point, so every spawn
;; returns a negative error. The guest prints "neg\n" and exits 0 when
;; it observes the error.
(module
  (import "wasi_snapshot_preview1" "wasi_thread_spawn"
          (func $spawn (param i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_write"
          (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit"
          (func $exit (param i32)))
  (memory (export "memory") 1 1 shared)
  (data (i32.const 64) "neg\n")
  (data (i32.const 68) "bad\n")

  (func $write (param $ptr i32) (param $len i32)
    (i32.store (i32.const 48) (local.get $ptr))
    (i32.store (i32.const 52) (local.get $len))
    (drop (call $fd_write (i32.const 1) (i32.const 48) (i32.const 1)
                          (i32.const 56))))

  (func (export "_start")
    (if (i32.ge_s (call $spawn (i32.const 0)) (i32.const 0))
      (then (call $write (i32.const 68) (i32.const 4))
            (call $exit (i32.const 1))))
    (call $write (i32.const 64) (i32.const 4))))
