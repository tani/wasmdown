;; wasi-threads spawn probe: two children, one shared linear memory.
;; Linear memory layout:
;;   0     atomic counter of started children
;;   4, 8  TID recorded by the child at the start_arg address
;;   20,24 TID returned to the parent by wasi_thread_spawn
;;   64    iovec; 72 nwritten; 80 "ok\n"; 84 "bad\n"
(module
  (import "wasi_snapshot_preview1" "wasi_thread_spawn"
          (func $spawn (param i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_write"
          (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit"
          (func $exit (param i32)))
  (memory (export "memory") 1 1 shared)
  (data (i32.const 80) "ok\n")
  (data (i32.const 84) "bad\n")

  (func $write (param $ptr i32) (param $len i32)
    (i32.store (i32.const 64) (local.get $ptr))
    (i32.store (i32.const 68) (local.get $len))
    (drop (call $fd_write (i32.const 1) (i32.const 64) (i32.const 1)
                          (i32.const 72))))

  ;; The host calls this with its TID and the opaque start_arg (an
  ;; address in the shared memory). Returning ends only this thread.
  (func (export "wasi_thread_start") (param $tid i32) (param $arg i32)
    (i32.store (local.get $arg) (local.get $tid))
    (drop (i32.atomic.rmw.add (i32.const 0) (i32.const 1)))
    (drop (memory.atomic.notify (i32.const 0) (i32.const 1))))

  (func (export "_start")
    (local $tid0 i32)
    (local $tid1 i32)
    (local.set $tid0 (call $spawn (i32.const 4)))
    (local.set $tid1 (call $spawn (i32.const 8)))
    (if (i32.or (i32.le_s (local.get $tid0) (i32.const 0))
                (i32.le_s (local.get $tid1) (i32.const 0)))
      (then (call $write (i32.const 84) (i32.const 4))
            (call $exit (i32.const 1))))
    (i32.store (i32.const 20) (local.get $tid0))
    (i32.store (i32.const 24) (local.get $tid1))
    ;; Wait until both children have run.
    (block $done
      (loop $wait
        (br_if $done (i32.ge_u (i32.atomic.load (i32.const 0)) (i32.const 2)))
        (drop (memory.atomic.wait32 (i32.const 0) (i32.const 0)
                                    (i64.const -1)))
        (br $wait)))
    ;; The child saw the host TID, and the two TIDs are distinct.
    (if (i32.or (i32.ne (i32.load (i32.const 4)) (local.get $tid0))
                (i32.or
                  (i32.ne (i32.load (i32.const 8)) (local.get $tid1))
                  (i32.eq (local.get $tid0) (local.get $tid1))))
      (then (call $write (i32.const 84) (i32.const 4))
            (call $exit (i32.const 2))))
    (call $write (i32.const 80) (i32.const 3))))
