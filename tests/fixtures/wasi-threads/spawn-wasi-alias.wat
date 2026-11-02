;; The proposal's own ABI name: module `wasi', field `thread-spawn'
;; (what wasi-libc's __wasi_thread_spawn imports). start_arg = 777 is
;; opaque: the child stores the TID at 0 and 777 at 4.
(module
  (import "wasi" "thread-spawn" (func $spawn (param i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_write"
          (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit"
          (func $exit (param i32)))
  (memory (export "memory") 1 1 shared)
  (data (i32.const 64) "ok\n")
  (data (i32.const 68) "bad\n")

  (func $write (param $ptr i32) (param $len i32)
    (i32.store (i32.const 48) (local.get $ptr))
    (i32.store (i32.const 52) (local.get $len))
    (drop (call $fd_write (i32.const 1) (i32.const 48) (i32.const 1)
                          (i32.const 56))))

  (func (export "wasi_thread_start") (param $tid i32) (param $arg i32)
    (i32.store (i32.const 0) (local.get $tid))
    (i32.store (i32.const 4) (local.get $arg))
    (i32.atomic.store (i32.const 8) (i32.const 1))
    (drop (memory.atomic.notify (i32.const 8) (i32.const 1))))

  (func (export "_start") (local $tid i32)
    (local.set $tid (call $spawn (i32.const 777)))
    (if (i32.le_s (local.get $tid) (i32.const 0))
      (then (call $write (i32.const 68) (i32.const 4))
            (call $exit (i32.const 1))))
    (block $done
      (loop $wait
        (br_if $done (i32.atomic.load (i32.const 8)))
        (drop (memory.atomic.wait32 (i32.const 8) (i32.const 0)
                                    (i64.const -1)))
        (br $wait)))
    (if (i32.or (i32.ne (i32.load (i32.const 0)) (local.get $tid))
                (i32.ne (i32.load (i32.const 4)) (i32.const 777)))
      (then (call $write (i32.const 68) (i32.const 4))
            (call $exit (i32.const 2))))
    (call $write (i32.const 64) (i32.const 3))))
