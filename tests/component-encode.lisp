;;;; Round-trip proof for tools/component-encode.lisp: the text TCP echo
;;;; fixture is encoded to binary, decoded, validated and RUN against a
;;;; loopback echo — text and binary agree semantically (connect/send/
;;;; receive). This is the guard for the encoder, not extra provider
;;;; coverage.

(defpackage #:wasmdown.test.component-encode
  (:use #:cl #:wasmdown.test)
  (:nicknames #:wd.test.component-encode))

(in-package #:wasmdown.test.component-encode)

(defun %wasi-sym (name) (find-symbol name "WASMDOWN.TEST.WASI"))

(test wasi/component-text-encoder-roundtrip
  (multiple-value-bind (port fd) (funcall (%wasi-sym "SOCK-HOST-LISTENER"))
    (let* ((got (list nil))
           (thread (funcall (%wasi-sym "SOCK-ECHO-THREAD") fd got)))
      (unwind-protect
           (let* ((text (funcall (%wasi-sym "TCP-ECHO-FIXTURE") port))
                  (comp (wasmdown.component.text::parse-component-text text))
                  (bytes (component-encode:encode-component comp))
                  (dec (wasmdown.component:decode-component bytes)))
             (is (wasmdown.component:component-p bytes))
             (is (= (length (wasmdown.component:comp-defs comp))
                    (length (wasmdown.component:comp-defs dec))))
             (wasmdown.component:validate-component dec)
             (let* ((net (funcall (%wasi-sym "SOCK-NETWORK") :dns '("localhost")))
                    (ctx (wasmdown.wasi:make-wasi-context
                          :networks (list net)))
                    (inst (wasmdown.component.runtime:instantiate-component
                           dec :with-args
                           (wasmdown.wasi:sockets-provider ctx)))
                    (f1 (wasmdown.component.runtime:runtime-export-value
                         inst "f1"))
                    (f2 (wasmdown.component.runtime:runtime-export-value
                         inst "f2")))
               (is (eql 0 (wasmdown.component.runtime:lifted-func-call f1)))
               (sb-thread:join-thread thread)
               (is (equal '(112 105 110 103) (car got)))
               (is (eql 112
                        (wasmdown.component.runtime:lifted-func-call f2)))))
        (ignore-errors (sb-thread:join-thread thread :timeout 2))
        (ignore-errors
         (funcall (%wasi-sym "%CLOSE-FD") fd))))))
