;;;; A MINIMAL component text -> binary encoder (tooling).
;;;;
;;;; The text parser already builds the full IR; no binary serializer
;;;; existed, so every resource-shaped fixture was hand-assembled byte by
;;;; byte (docs/status.md records the cost). This encoder writes the IR
;;;; back out section by section, preserving IR order (the decoder accepts
;;;; repeated sections), and is scoped to the shapes the P3 fixtures use:
;;;; types (incl. resource/own/borrow/stream/future/result/variant/tuple),
;;;; imports/exports, aliases (export/core-export/outer), canon lift/lower
;;;; and the async builtins, core modules (via wasmdown.binary), core
;;;; instances (instantiate args carry the bare 0x12 core-sort byte; inline
;;;; exports carry a bare core:sort byte), and component instances.
;;;;
;;;; VALIDATED by round-trip: tests/component-encode encodes the text
;;;; tcp-echo fixture and RUNS the decoded component (connect/send/receive
;;;; over a loopback echo) — text and binary agree semantically.
;;;;
;;;; Limitations (explicit): core:type declarators inside instance types,
;;;; nested components (section 4), start/value sections, and canon
;;;; builtins outside the listed set are NOT encoded; each raises.
;;;;
;;;; Usage: sbcl --script tools/component-encode.lisp IN.wat OUT.wasm

(require :asdf)
(defparameter *root*
  (if *load-truename*
      (make-pathname :directory (butlast (pathname-directory *load-truename*)))
      (asdf:system-source-directory :wasmdown)))
(pushnew *root* asdf:*central-registry* :test #'equal)
(asdf:load-system :wasmdown)

(defpackage :component-encode
  (:use :cl)
  (:local-nicknames (:c :wasmdown.component))
  (:export #:encode-component))
(in-package :component-encode)

(defun u32 (n) (let ((out '())) (loop while (>= n 128)
                                      do (push (logior 128 (logand n 127)) out)
                                         (setf n (ash n -7))
                                      finally (push n out)
                                              (return (coerce (nreverse out) '(vector (unsigned-byte 8)))))))
(defun by (&rest xs) (coerce xs '(vector (unsigned-byte 8))))
(defun cat (&rest vs) (apply #'concatenate '(vector (unsigned-byte 8)) vs))
(defun nm (s) (cat (u32 (length s)) (map 'vector #'char-code s)))
(defun na (s) (cat (by 0) (nm s)))
(defun vecb (xs) (cat (u32 (length xs)) (apply #'cat (coerce xs 'list))))
(defun sec (id content) (cat (by id) (u32 (length content)) content))

(defparameter *prim-bytes*
  '((:bool . #x7f) (:s8 . #x7e) (:u8 . #x7d) (:s16 . #x7c) (:u16 . #x7b)
    (:s32 . #x7a) (:u32 . #x79) (:s64 . #x78) (:u64 . #x77)
    (:f32 . #x76) (:f64 . #x75) (:char . #x74) (:string . #x73)
    (:error-context . #x64)))

(defparameter *comp-sort* '((:func . #x01) (:value . #x02) (:type . #x03)
                            (:component . #x04) (:instance . #x05)))
(defparameter *core-sort* '((:core-func . #x00) (:core-table . #x01)
                            (:core-memory . #x02) (:core-global . #x03)
                            (:core-tag . #x04) (:core-type . #x10)
                            (:core-module . #x11) (:core-instance . #x12)))

(defun sort-bytes (sort)
  (cond ((assoc sort *core-sort*)
         (by 0 (cdr (assoc sort *core-sort*))))
        ((cdr (assoc sort *comp-sort*))
         (by (cdr (assoc sort *comp-sort*))))
        (t (error "unknown sort ~s" sort))))

(defun vt (v)
  "component valtype: prim keyword, (:idx . n), or an inline ct."
  (typecase v
    (keyword (by (cdr (assoc v *prim-bytes*))))
    (cons (if (eq (car v) :idx) (u32 (cdr v))
              (vt (c::make-ct :kind :prim :payload v))))
    (c::ct (enc-deftype v))
    (t (error "unknown valtype ~s" v))))

(defun enc-deftype (ct)
  (ecase (c::ct-kind ct)
    (:prim (by (cdr (assoc (c::ct-payload ct) *prim-bytes*))))
    (:record (cat (by #x72)
                  (vecb (mapcar (lambda (f) (cat (nm (car f)) (vt (cdr f))))
                                (coerce (c::ct-payload ct) 'list)))))
    (:variant (cat (by #x71)
                   (vecb (mapcar (lambda (v)
                                   (cat (nm (car v))
                                        (if (cdr v) (cat (by 1) (vt (cdr v)) (by 0))
                                            (by 0 0))))
                                 (coerce (c::ct-payload ct) 'list)))))
    (:tuple (cat (by #x6f) (vecb (mapcar #'vt (coerce (c::ct-payload ct) 'list)))))
    (:list (let* ((p (c::ct-payload ct)) (elem (car p)) (len (cdr p)))
             (if (and (integerp len) (plusp len))
                 (cat (by #x67) (vt elem) (u32 len))
                 (cat (by #x70) (vt elem)))))
    (:option (cat (by #x6b) (vt (c::ct-payload ct))))
    (:result (let ((ok (car (c::ct-payload ct))) (err (cdr (c::ct-payload ct))))
               (cat (by #x6a)
                    (if ok (cat (by 1) (vt ok)) (by 0))
                    (if err (cat (by 1) (vt err)) (by 0)))))
    (:own (cat (by #x69) (u32 (c::ct-payload ct))))
    (:borrow (cat (by #x68) (u32 (c::ct-payload ct))))
    (:stream (let ((e (c::ct-payload ct)))
               (if e (cat (by #x66 1) (vt e)) (cat (by #x66 0)))))
    (:future (let ((e (c::ct-payload ct)))
               (if e (cat (by #x65 1) (vt e)) (cat (by #x65 0)))))
    (:enum (cat (by #x6d) (vecb (mapcar #'nm (coerce (c::ct-payload ct) 'list)))))
    (:flags (cat (by #x6e) (vecb (mapcar #'nm (coerce (c::ct-payload ct) 'list)))))
    (:resource (let ((rep (car (c::ct-payload ct))) (dtor (cdr (c::ct-payload ct))))
                 (cat (by #x3f) (by (case rep (:i32 #x7f) (:i64 #x7e) (t #x7f)))
                      (if dtor (cat (by 1) (u32 dtor)) (by 0)))))
    (:func (let* ((p (c::ct-payload ct)) (params (first p))
                  (results (second p)) (async (third p)))
             (cat (by (if async #x43 #x40))
                  (vecb (mapcar (lambda (x) (cat (nm (car x)) (vt (cdr x))))
                                (coerce params 'list)))
                  (if results (cat (by 0) (vt results)) (by 1 0)))))
    (:instance (cat (by #x42) (vecb (mapcar #'enc-typedecl
                                            (coerce (c::ct-payload ct) 'list)))))
    (:component (cat (by #x41) (vecb (mapcar #'enc-typedecl
                                             (coerce (c::ct-payload ct) 'list)))))
    (t (error "cannot encode ct kind ~s" (c::ct-kind ct)))))

(defun enc-typebound (bound)
  (cond ((eq bound :sub-resource) (by 1))
        ((and (consp bound) (eq (car bound) :eq)) (cat (by 0) (u32 (cdr bound))))
        (t (error "unknown typebound ~s" bound))))

(defun enc-externtype (et)
  (let ((sort (car et)) (payload (cdr et)))
    (case sort
      (:core-module (cat (by 0 #x11) (u32 payload)))
      (:func (cat (by #x01) (u32 payload)))
      (:value (if (and (consp payload) (eq (car payload) :eq))
                  (cat (by #x02 0) (u32 (cdr payload)))
                  (cat (by #x02 1) (vt payload))))
      (:type (cat (by #x03) (enc-typebound payload)))
      (:component (cat (by #x04) (u32 payload)))
      (:instance (cat (by #x05) (u32 payload)))
      (t (error "unknown externtype ~s" et)))))

(defun enc-typedecl (d)
  (let ((k (c::typedecl-kind d)) (p (c::typedecl-payload d)))
    (ecase k
      (:type (cat (by #x01) (enc-deftype p)))
      (:alias (cat (by #x02) (enc-alias p)))
      (:import (destructuring-bind (name attrs et) p
                 (declare (ignore attrs))
                 (cat (by #x03) (na name) (enc-externtype et))))
      (:export (destructuring-bind (name attrs et) p
                 (declare (ignore attrs))
                 (cat (by #x04) (na name) (enc-externtype et))))
      (:core-type (cat (by #x00) (enc-coretype p)))
      (t (error "unknown typedecl kind ~s" k)))))

(defun enc-coretype (ct)
  ;; rectype bodies arrive as wasmdown.types comptypes in moduletype
  ;; positions only; our fixtures use only core:type aliases inline.
  (error "core type declarators are not supported by this encoder: ~s" ct))

(defun enc-alias (a)
  (let ((kind (c::aliasrec-kind a)) (sort (c::aliasrec-sort a)))
    (ecase kind
      (:export (cat (sort-bytes sort) (by #x00) (u32 (c::aliasrec-index a))
                    (nm (c::aliasrec-name a))))
      (:core-export (cat (sort-bytes sort) (by #x01) (u32 (c::aliasrec-index a))
                         (nm (c::aliasrec-name a))))
      (:outer (cat (sort-bytes sort) (by #x02) (u32 (c::aliasrec-count a))
                   (u32 (c::aliasrec-target a)))))))

(defun enc-opts (opts)
  (vecb (mapcar (lambda (o)
                  (case (car o)
                    (:string-encoding (by (case (second o)
                                            (:utf8 0) (:utf16 1) (:latin1+utf16 2)
                                            (t 0))))
                    (:memory (cat (by #x03) (u32 (second o))))
                    (:realloc (cat (by #x04) (u32 (second o))))
                    (:post-return (cat (by #x05) (u32 (second o))))
                    (:async (by #x06))
                    (:callback (cat (by #x07) (u32 (second o))))
                    (t (error "unknown canonopt ~s" o))))
                (coerce opts 'list))))

(defun resultlist-bytes (results)
  (if results (cat (by 0) (vt results)) (by 1 0)))

(defun enc-canon (cr)
  (let ((kind (c::canonrec-kind cr))
        (opts (c::canonrec-opts cr))
        (imms (c::canonrec-imms cr)))
    (ecase kind
      (:lift (cat (by 0 0) (u32 (c::canonrec-func cr)) (enc-opts opts)
                  (u32 (c::canonrec-typeidx cr))))
      (:lower (cat (by 1 0) (u32 (c::canonrec-func cr)) (enc-opts opts)))
      (:resource-new (cat (by 2) (u32 (c::canonrec-typeidx cr))))
      (:resource-drop (cat (by 3) (u32 (c::canonrec-typeidx cr))))
      (:resource-rep (cat (by 4) (u32 (c::canonrec-typeidx cr))))
      (:builtin (enc-builtin imms opts)))))

(defun enc-builtin (imms opts)
  (let ((name (first imms)) (a (second imms)) (b (third imms)))
    (flet ((opts-of (x) (if (and (listp x) (consp (car x))) x nil)))
      (case name
        (:task-return (cat (by #x09)
                           (resultlist-bytes (if (listp a) (first a) a))
                           (enc-opts (or (opts-of b) (when (consp opts) opts) '()))))
        (:stream-new (cat (by #x0e) (u32 a)))
        (:stream-read (cat (by #x0f) (u32 a) (enc-opts (or (opts-of b) '()))))
        (:stream-write (cat (by #x10) (u32 a) (enc-opts (or (opts-of b) '()))))
        (:stream-drop-readable (cat (by #x13) (u32 a)))
        (:stream-drop-writable (cat (by #x14) (u32 a)))
        (:future-read (cat (by #x16) (u32 a) (enc-opts (or (opts-of b) '()))))
        (:future-write (cat (by #x17) (u32 a) (enc-opts (or (opts-of b) '()))))
        (:future-drop-readable (cat (by #x1a) (u32 a)))
        (:future-drop-writable (cat (by #x1b) (u32 a)))
        (:subtask-cancel (cat (by #x06) (by (if a 1 0))))
        (:subtask-drop (by #x0d))
        (:waitable-join (by #x23))
        (:waitable-set-new (by #x1f))
        (:waitable-set-wait (cat (by #x20 0) (u32 a)))
        (:waitable-set-poll (cat (by #x21 0) (u32 a)))
        (t (error "unsupported builtin ~s" name))))))

(defun enc-sortidx (si)
  (cat (sort-bytes (car si)) (u32 (cdr si))))

(defun enc-coreinst (ci)
  (ecase (c::coreinstrec-kind ci)
    (:instantiate (cat (by 0) (u32 (c::coreinstrec-index ci))
                       (vecb (mapcar (lambda (arg)
                                       (cat (nm (car arg)) (by #x12) (u32 (cdr arg))))
                                     (coerce (c::coreinstrec-args ci) 'list)))))
    (:inline-exports (cat (by 1)
                          (vecb (mapcar (lambda (e)
                                          (let ((sort (car (cdr e)))
                                                (idx (cdr (cdr e))))
                                            ;; core:sort is ONE byte, no
                                            ;; leading 0x00 (component
                                            ;; sortidx grammar differs)
                                            (cat (nm (car e))
                                                 (by (cdr (assoc sort *core-sort*)))
                                                 (u32 idx))))
                                        (coerce (c::coreinstrec-exports ci) 'list)))))))

(defun enc-inst (i)
  (ecase (c::instrec-kind i)
    (:instantiate (cat (by 0) (u32 (c::instrec-index i))
                       (vecb (mapcar (lambda (arg)
                                       (cat (nm (car arg)) (enc-sortidx (cdr arg))))
                                     (coerce (c::instrec-args i) 'list)))))
    (:inline-exports (cat (by 1)
                          (vecb (mapcar (lambda (e)
                                          (cat (nm (car e)) (enc-sortidx (cdr e))))
                                        (coerce (c::instrec-exports i) 'list)))))))

(defun enc-import (ir)
  (cat (na (c::importrec-name ir)) (enc-externtype (c::importrec-type ir))))

(defun enc-export (er)
  (cat (na (c::exportrec-name er))
       (enc-sortidx (c::exportrec-sortidx er))
       ;; the binary exportdecl's optional extern-name is absent
       (by 0)))

(defun encode-component (comp)
  (let ((out (list (by 0 #x61 #x73 #x6d #x0d 0 1 0))))
    (loop for d across (c::comp-defs comp)
          do (let ((k (c::compdef-kind d)) (p (c::compdef-payload d)))
               (push (case k
                       (:type (sec 7 (vecb (list (enc-deftype p)))))
                       (:import (sec 10 (vecb (list (enc-import p)))))
                       (:export (sec 11 (vecb (list (enc-export p)))))
                       (:alias (sec 6 (vecb (list (enc-alias p)))))
                       (:canon (sec 8 (vecb (list (enc-canon p)))))
                       (:core-module (sec 1 (wasmdown.binary:encode-module p)))
                       (:core-instance (sec 2 (vecb (list (enc-coreinst p)))))
                       (:instance (sec 5 (vecb (list (enc-inst p)))))
                       (t (error "unsupported component def kind ~s" k)))
                     out)))
    (apply #'cat (nreverse out))))

;;; CLI: encode a .wat component file to a .wasm component.
(let ((args (rest sb-ext:*posix-argv*)))
  (when (and (= (length args) 2)
             (string= (pathname-type (pathname (first args))) "wat"))
    (unless (string= (pathname-type (pathname (second args))) "wasm")
      (format *error-output* "usage: ~a IN.wat OUT.wasm~%"
              (or (first sb-ext:*posix-argv*) "component-encode"))
      (sb-ext:exit :code 2))
  (let* ((text (with-open-file (s (first args) :external-format :utf-8)
                 (let ((b (make-string (file-length s))))
                   (read-sequence b s) b)))
         (comp (wasmdown.component.text::parse-component-text text))
         (bytes (encode-component comp)))
    (with-open-file (s (second args) :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
      (write-sequence bytes s))
      (format t "~a: ~d bytes~%" (second args) (length bytes)))))
