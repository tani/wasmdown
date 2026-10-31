;;;; Pinned P2 artifacts: what the BINARY layer must accept today.
;;;;
;;;; tests/fixtures/wasi/sqlite3-p2.wasm is the SQLite 3.54.1 shell
;;;; componentized with the pinned Wasmtime v50.0.1 command adapter
;;;; (tools/build-wasi-fixtures.sh). It is a real, large component:
;;;; 280 definitions and a type section with far more than 63 types.
;;;;
;;;; DECODE is pinned here because it exposed a real decoder defect:
;;;; a component valtype that names a type index >= 64 is encoded as a
;;;; multi-byte s33 whose first byte has the CONTINUATION bit set
;;;; (64 = 0xC0 0x00); the decoder used to reject every such byte as
;;;; "invalid leading byte ... for component value type", so EVERY
;;;; adapter-produced component was undecodable.
;;;;
;;;; VALIDATION of this artifact is deliberately NOT asserted yet:
;;;; the envelope's `wasi:io/streams.input-stream.subscribe' resolves
;;;; to [] -> [] instead of [i32] -> [i32] until the component
;;;; type-space normalization (PLAN.md step 4) lands, so asserting it
;;;; today would pin a bug.

(defpackage #:wasmdown.test.wasi-p2-fixtures
  (:use #:cl #:wasmdown.test))

(in-package #:wasmdown.test.wasi-p2-fixtures)

(defparameter *p2-artifact*
  (merge-pathnames "../tests/fixtures/wasi/sqlite3-p2.wasm"
                   (or *load-truename* *default-pathname-defaults*)))

(defun %slurp (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(test wasi-p2-fixture/decodes
  (is (probe-file *p2-artifact*))
  (let* ((bytes (%slurp *p2-artifact*))
         (comp (wasmdown.component:decode-component bytes)))
    (is (wasmdown.component:component-p bytes))
    (is (= 280 (length (wasmdown.component:comp-defs comp))))
    ;; the first import is a P2 interface instance, not a P3 one
    (let* ((defs (wasmdown.component:comp-defs comp))
           (import (find :import defs
                         :key #'wasmdown.component:compdef-kind)))
      (is import)
      (is (search "@0.2.12"
                  (wasmdown.component::importrec-name
                   (wasmdown.component:compdef-payload import)))))))

(test wasi-p2-fixture/truncation-is-a-decode-error
  ;; a truncated binary is a decode error, never a Lisp condition
  (let* ((bytes (%slurp *p2-artifact*))
         (cut (subseq bytes 0 (floor (length bytes) 2))))
    (is (handler-case (progn (wasmdown.component:decode-component cut) nil)
          (wasmdown:decode-error () t)))))

(test wasi-p2-fixture/producer-numbering-is-spec-conformant
  ;; The phantom-slot defect is in the CONSUMER (the type space
  ;; contains an extra :OPAQUE entry per func/value/instance
  ;; import/export), not in the producer: every (func . i) reference
  ;; in the artifact's instance/component types is < the number of
  ;; type-INTRODUCING declarators in that same type. Removing the
  ;; phantom arm alone therefore changes no index the producer emits;
  ;; it changes which consumer slots those indices hit.
  (let ((comp (wasmdown.component:decode-component
               (%slurp *p2-artifact*)))
        (checked 0)
        (bad '()))
    (flet ((check (ct)
             (when (member (wasmdown.component::ct-kind ct)
                           '(:instance :component))
               (let* ((decls (coerce (wasmdown.component::ct-payload ct)
                                     'list))
                      (n-types
                        (count-if
                         (lambda (d)
                           (case (wasmdown.component::typedecl-kind d)
                             ((:type :alias) t)
                             ((:import :export)
                              (let ((et (third
                                         (wasmdown.component::typedecl-payload
                                          d))))
                                (and (consp et) (eq (car et) :type))))
                             (t nil)))
                         decls))
                      (max-ref
                        (loop for d in decls
                              for kind = (wasmdown.component::typedecl-kind d)
                              when (member kind '(:import :export))
                                maximize
                                (let ((et (third
                                           (wasmdown.component::typedecl-payload
                                            d))))
                                  (if (and (consp et)
                                           (eq (car et) :func)
                                           (integerp (cdr et)))
                                      (cdr et)
                                      -1)))))
                 (incf checked)
                 (when (>= max-ref n-types)
                   (push (list (wasmdown.component::ct-offset ct)
                               n-types max-ref)
                         bad))))))
      (loop for d across (wasmdown.component:comp-defs comp)
            when (eq (wasmdown.component:compdef-kind d) :type)
              do (let ((payload (wasmdown.component:compdef-payload d)))
                   (if (wasmdown.component::ct-p payload)
                       (check payload)
                       ;; a :TYPE def may hold several cts (rec groups)
                       (loop for c across (if (vectorp payload)
                                              payload
                                              (vector payload))
                             do (check c))))))
    (is (plusp checked))
    (is (null bad))))


;;; ---------------------------------------------------------------
;;; The step-4 verdict: the REAL adapter artifact now validates, its
;;; imports compose against the versioned providers, and it
;;; instantiates. Execution of the guest command is NOT asserted here
;;; (it stops in the sync-lift path today); the pinned facts are the
;;; pipeline steps that pass, each with its own assertion.

(defun %declared-imports (comp)
  (loop for d across (wasmdown.component:comp-defs comp)
        when (eq (wasmdown.component:compdef-kind d) :import)
          collect (wasmdown.component::importrec-name
                   (wasmdown.component:compdef-payload d))))

(test wasi-p2-fixture/validates
  ;; Before step 4 this stopped at the `input-stream.subscribe'
  ;; signature; the type-space normalization resolved it and the whole
  ;; 280-definition envelope validates.
  (let* ((bytes (%slurp *p2-artifact*))
         (comp (wasmdown.component:decode-component bytes)))
    (is (wasmdown.component:validate-component comp))))

(test wasi-p2-fixture/instantiates-with-versioned-providers
  ;; Every declared import is bound by a provider of its OWN version
  ;; (the artifact imports @0.2.12; the 0.3.2 builder set does not
  ;; apply), the instance is created, and its command `run' export
  ;; resolves. The store is the run's ownership boundary.
  (let* ((bytes (%slurp *p2-artifact*))
         (comp (wasmdown.component:decode-component bytes))
         (imports (%declared-imports comp))
         (context (wasmdown.wasi:make-wasi-context))
         (store (wasmdown.component.runtime:make-component-store)))
    (is (wasmdown.component:validate-component comp))
    ;; the versioned provider table answers every declared import
    (let ((providers
            (wasmdown.wasi::preview2-command-provider context
                                                      :imports imports)))
      (is (plusp (length providers)))
      (dolist (name imports)
        (let ((iface (wasmdown.wasi::p2-interface-by-name name)))
          ;; a types-only import needs no binding; everything else is
          ;; composed under its EXACT declared name
          (when (and iface
                     (or (wasmdown.wit:wit-interface-func-list iface)
                         (wasmdown.wit:wit-interface-resource-list iface)))
            (is (assoc name providers :test #'string=)))))
      (unwind-protect
           (let ((inst (wasmdown.component.runtime:instantiate-component
                        comp :with-args providers :store store)))
             (is (wasmdown.component.runtime:comp-instance-p inst))
             (is (eq store
                     (wasmdown.component.runtime:component-instance-store
                      inst)))
             ;; the command export resolves by PREFIX, at 0.2.12
             (is (wasmdown.wasi:component-run-export inst)))
        (wasmdown.component.runtime:store-teardown store)))))

(test wasi-p2-fixture/cli-provider-dispatch-follows-declared-imports
  ;; The CLI must choose the provider graph from the component's
  ;; DECLARED import names, never from the file name: a 0.2 command
  ;; gets the versioned P2 tables (each import bound at its own
  ;; version), and no name is silently dropped.
  (let* ((bytes (%slurp *p2-artifact*))
         (comp (wasmdown.component:decode-component bytes))
         (imports (wasmdown.wasi:declared-component-imports comp))
         (context (wasmdown.wasi:make-wasi-context))
         (providers (wasmdown.wasi:component-command-providers
                     context comp)))
    (is (plusp (length imports)))
    (is (plusp (length providers)))
    ;; every composed provider is bound under an EXACT declared name
    (dolist (pair providers)
      (is (member (car pair) imports :test #'string=)))
    ;; the 0.2 names never come from the 0.3 world
    (is (notany (lambda (pair) (search "@0.3" (car pair)))
                providers))))
