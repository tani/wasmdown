;;;; ASYNC INTERLEAVE PROBE (interleave) — the bisect harness for the
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
(defvar *fails* 0)
(defvar *good-log* nil)
(dotimes (i 300)
  (format t "~&START ~a~%" i) (finish-output)
  (setf wasmdown.component.runtime::*acs-log* nil)
  (let* ((rs (wasmdown.wast:run-script
              #P"/home/tani/ghq/github.com/tani/wasmdown/tests/component/async/async-calls-sync.wast"
              :file "async-calls-sync.wast"))
         (bad (remove-if-not (lambda (r) (eq (wasmdown.wast:wast-result-outcome r) :fail)) rs))
         (log (reverse wasmdown.component.runtime::*acs-log*)))
    (cond (bad
           (incf *fails*)
           (format t "~&RUN ~a FAIL (~a)~%" i (wasmdown.wast:wast-result-detail (first bad)))
           (dolist (l log) (format t "    ~a~%" l)))
          ((null *good-log*) (setf *good-log* log)))))
(format t "~&TOTAL ~a/300~%" *fails*)
(format t "~&--- ONE GREEN RUN ---~%")
(dolist (l *good-log*) (format t "    ~a~%" l))
