;;;; ASYNC INTERLEAVE PROBE (watchdog) — the bisect harness for the
;;;; async lane's timing-pinned rows. Run N times against one
;;;; fixture and stamp the runtime's interleave events (the
;;;; watchdog variant dumps the permit/scheduler state on a
;;;; hang). Paths are workspace-relative via *load-truename*.
;;;; Measurement discipline: SERIAL runs only — a concurrent lane
;;;; distorts the rates by CPU contention.

(require :asdf)
(pushnew #P"/home/tani/ghq/github.com/tani/wasmdown/" asdf:*central-registry* :test #'equal)
(asdf:load-system :wasmdown)
(setf *print-circle* t)
(setf wasmdown.component.runtime::*acs-log-p* t)
(defvar *run-i* 0)
(sb-thread:make-thread
 (lambda ()
   (loop
     (sleep 8)
     (format t "~&=== WATCHDOG at run ~a ===~%" *run-i*)
     (format t "permit owner=~a depth=~a queue=~a~%"
             (let ((o wasmdown.component.runtime::*permit-owner*))
               (and o (wasmdown.component.runtime::%permit-path o)))
             wasmdown.component.runtime::*permit-depth*
             (mapcar (lambda (c) (car c))
                     wasmdown.component.runtime::*permit-queue*))
     (let ((o wasmdown.component.runtime::*permit-owner*))
       (when o
         (format t "OWNER fields: block-depth=~a yield-parked=~a susp=~a ready=~a turn-done=~a ready-fn=~a task-state=~a resolved=~a~%"
                 (wasmdown.component.runtime::async-thread-block-depth o)
                 (wasmdown.component.runtime::async-thread-yield-parked o)
                 (wasmdown.component.runtime::async-thread-suspended-p o)
                 (wasmdown.component.runtime::async-thread-ready-p o)
                 (wasmdown.component.runtime::async-thread-turn-done o)
                 (and (wasmdown.component.runtime::async-thread-ready-fn o) t)
                 (let ((tk (wasmdown.component.runtime::async-thread-task o)))
                   (and tk (wasmdown.component.runtime::async-task-state tk)))
                 (let ((tk (wasmdown.component.runtime::async-thread-task o)))
                   (and tk (wasmdown.component.runtime::async-task-resolved-p tk))))))
     (sb-thread:with-recursive-lock (wasmdown.component.runtime::*scheduler-mutex*)
       (format t "threads=~%")
       (dolist (th wasmdown.component.runtime::*scheduler-threads*)
         (format t "  path=~a blocked=~a yp=~a susp=~a ready=~a turn=~a state=~a~%"
                 (wasmdown.component.runtime::%permit-path th)
                 (wasmdown.component.runtime::async-thread-block-depth th)
                 (wasmdown.component.runtime::async-thread-yield-parked th)
                 (wasmdown.component.runtime::async-thread-suspended-p th)
                 (wasmdown.component.runtime::async-thread-ready-p th)
                 (wasmdown.component.runtime::async-thread-turn-done th)
                 (let ((tk (wasmdown.component.runtime::async-thread-task th)))
                   (and tk (wasmdown.component.runtime::async-task-state tk))))))
     (dolist (th (sb-thread:list-all-threads))
       (unless (eq th sb-thread:*current-thread*)
         (ignore-errors
          (sb-thread:interrupt-thread
           th (lambda ()
                (format t "~&--- THREAD ~a ---~%" (sb-thread:thread-name th))
                (sb-debug:print-backtrace :count 12 :stream *standard-output*)
                (finish-output))))))
     (sleep 2)
     (format t "last events:~%~{  ~a~%~}"
             (let ((l (reverse wasmdown.component.runtime::*acs-log*)))
               (last l (min 8 (length l)))))
     (finish-output)
     (sb-ext:exit :code 3))))
(dotimes (i 40)
  (setf *run-i* i)
  (setf wasmdown.component.runtime::*acs-log* nil)
  (let ((rs (wasmdown.wast:run-script
             #P"/home/tani/ghq/github.com/tani/wasmdown/tests/component/async/async-calls-sync.wast"
             :file "async-calls-sync.wast")))
    (declare (ignore rs))))
(format t "~&DONE~%")
