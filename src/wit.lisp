;;;; WASMDOWN — the WIT text frontend (PLAN.md §8.1, M16 slice 1).
;;;;
;;;; A real tokenizer + recursive-descent reader for the WIT surface
;;;; the vendored WASI 0.3.2 snapshot uses:
;;;;
;;;;   package name@version;
;;;;   interface name { use ...; type ...; record/enum/variant/flags;
;;;;                    resource name { methods }; funcs }
;;;;   world name { import ...; export ...; }
;;;;   annotations @since(version = ...) / @unstable(feature = ...)
;;;;
;;;; The reader produces plain CL nodes (wit-package/interface/world/
;;;; func/type/resource/use structs); it does NOT validate the WIT or
;;;; resolve cross-package references (wit-type->ct does that on
;;;; demand). The node graph is deliberately boring so the later
;;;; filesystem/sockets/http slices can extend it without rewriting.

(in-package #:wasmdown.wit)

;;; ---------------------------------------------------------------
;;; Conditions.

(define-condition wit-error (wasmdown:wasmdown-error)
  ((path :initarg :path :initform nil :reader wit-error-path)
   (offset :initarg :offset :initform nil :reader wit-error-offset))
  (:default-initargs :message "WIT error")
  (:report (lambda (c s)
             (format s "WIT read error~@[ in ~a~]~@[ at char ~d~]: ~a"
                     (wit-error-path c) (wit-error-offset c)
                     (wasmdown:wasmdown-error-message c)))))

;;; ---------------------------------------------------------------
;;; Tokens and the reader.

(defstruct wit-token
  (kind nil :type keyword) ; :ident :punct :version :string :arrow
  (text "" :type string)
  (start 0 :type integer))

(defstruct wit-reader
  (tokens #() :type vector)
  (pos 0 :type integer)
  (path nil))

(defun wit-reader-peek (r &optional (k 0))
  (let ((i (+ (wit-reader-pos r) k)))
    (and (< i (length (wit-reader-tokens r)))
         (aref (wit-reader-tokens r) i))))

(defun %peek (r) (wit-reader-peek r))

(defun %next (r)
  (let ((tk (%peek r)))
    (when tk (incf (wit-reader-pos r)))
    tk))

(defun %werr (r control &rest args)
  (error 'wit-error
         :path (wit-reader-path r)
         :offset (and (%peek r) (wit-token-start (%peek r)))
         :message (apply #'format nil control args)))

(defun %punct-p (tk ch)
  (and tk (eq (wit-token-kind tk) :punct)
       (string= (wit-token-text tk) ch)))

(defun %ident-p (tk &optional text)
  (and tk (eq (wit-token-kind tk) :ident)
       (or (null text) (string= (wit-token-text tk) text))))

(defun %expect-punct (r ch)
  (let ((tk (%peek r)))
    (unless (%punct-p tk ch)
      (%werr r "expected `~a`~@[ (found ~a)~]" ch
             (and tk (wit-token-text tk))))
    (%next r)))

(defun %ident-name (text)
  "The WIT identifier's semantic name: a leading `%` is the escape
sigil and is not part of the name."
  (if (and (plusp (length text)) (char= (char text 0) #\%))
      (subseq text 1)
      text))

(defun %expect-ident (r)
  (let ((tk (%peek r)))
    (unless (eq (and tk (wit-token-kind tk)) :ident)
      (%werr r "expected an identifier~@[ (found ~a)~]"
             (and tk (wit-token-text tk))))
    (%next r)
    (%ident-name (wit-token-text tk))))

(defun %expect-keyword (r word)
  (let ((tk (%peek r)))
    (unless (%ident-p tk word)
      (%werr r "expected `~a`~@[ (found ~a)~]" word
             (and tk (wit-token-text tk))))
    (%next r)
    word))

(defun %keyword-p (r word)
  (%ident-p (%peek r) word))

;;; ---------------------------------------------------------------
;;; Tokenizer.

(defun %blank-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page) :test #'char=))

(defun %punct-char-p (c)
  (member c '(#\{ #\} #\( #\) #\< #\> #\, #\; #\: #\= #\. #\/
              #\@ #\? #\+ #\* #\_)
          :test #'char=))

(defun wit-tokenize (string &key (path nil))
  (let ((n (length string))
        (i 0)
        (out (make-array 64 :fill-pointer 0 :adjustable t)))
    (labels
        ((at (k) (and (< (+ i k) n) (char string (+ i k))))
         (emit (kind text start)
           (vector-push-extend (make-wit-token :kind kind :text text
                                               :start start)
                               out)))
      (loop while (< i n)
            do (let ((c (char string i)))
                 (cond
                   ((%blank-p c) (incf i))
                   ;; line comment (/// doc comments included)
                   ((and (char= c #\/) (char= (or (at 1) #\Nul) #\/))
                    (loop while (and (< i n)
                                     (not (member (char string i)
                                                  '(#\Newline #\Return)
                                                  :test #'char=)))
                          do (incf i)))
                   ;; block comment (nested)
                   ((and (char= c #\/) (char= (or (at 1) #\Nul) #\*))
                    (incf i 2)
                    (loop with depth = 1
                          while (and (< i n) (plusp depth))
                          do (cond
                               ((and (char= (char string i) #\/)
                                     (char= (or (at 1) #\Nul) #\*))
                                (incf i 2) (incf depth))
                               ((and (char= (char string i) #\*)
                                     (char= (or (at 1) #\Nul) #\/))
                                (incf i 2) (decf depth))
                               (t (incf i)))))
                   ((char= c #\")
                    (let ((start i))
                      (incf i)
                      (let ((buf (make-array 16 :element-type 'character
                                                :fill-pointer 0 :adjustable t)))
                        (loop while (and (< i n) (not (char= (char string i) #\")))
                              do (if (char= (char string i) #\\)
                                     (progn
                                       (incf i)
                                       (let ((e (and (< i n) (char string i))))
                                         (vector-push-extend
                                          (case e
                                            (#\n #\Newline)
                                            (#\t #\Tab)
                                            (#\r #\Return)
                                            (t (or e #\?)))
                                          buf))
                                       (incf i))
                                     (progn
                                       (vector-push-extend (char string i) buf)
                                       (incf i))))
                        (when (>= i n)
                          (error 'wit-error :path path :offset start
                                 :message "unterminated string literal"))
                        (incf i) ; closing quote
                        (emit :string (coerce buf 'string) start))))
                   ((and (char= c #\-) (char= (or (at 1) #\Nul) #\>))
                    (emit :arrow "->" i)
                    (incf i 2))
                   ((%punct-char-p c)
                    (emit :punct (string c) i)
                    (incf i))
                   ((or (alpha-char-p c) (char= c #\%))
                    (let ((start i))
                      (loop while (and (< i n)
                                       (let ((d (char string i)))
                                         (or (alphanumericp d)
                                             (member d '(#\- #\_ #\%)
                                                     :test #'char=))))
                            do (incf i))
                      (emit :ident (subseq string start i) start)))
                   ((digit-char-p c)
                    (let ((start i))
                      (loop while (and (< i n)
                                       (let ((d (char string i)))
                                         (or (alphanumericp d)
                                             (and (member d '(#\. #\- #\+)
                                                          :test #'char=)
                                                  (< (+ i 1) n)
                                                  (alphanumericp
                                                   (char string (+ i 1)))))))
                            do (incf i))
                      (emit :version (subseq string start i) start)))
                   (t (error 'wit-error :path path :offset i
                             :message (format nil "unexpected character ~s"
                                              c))))))
      out)))

;;; The default constructor MAKE-WIT-READER builds a reader from
;;; pre-tokenized input; MAKE-WIT-READER* is the string-taking helper.
(defun make-wit-reader* (string &key (path nil))
  (make-wit-reader :tokens (wit-tokenize string :path path) :path path))

;;; ---------------------------------------------------------------
;;; Annotations.

(defun %parse-annotation-value (r)
  (let ((tk (%peek r)))
    (cond ((null tk) (%werr r "expected an annotation value"))
          ((member (wit-token-kind tk) '(:ident :version :string))
           (%next r)
           (wit-token-text tk))
          (t (%werr r "expected an annotation value (found ~a)"
                    (wit-token-text tk))))))

(defun %parse-annotations (r)
  (let ((out '()))
    (loop while (%punct-p (%peek r) "@")
          do (%next r)
             (let ((name (%expect-ident r))
                   (args '()))
               (%expect-punct r "(")
               (unless (%punct-p (%peek r) ")")
                 (loop
                   (let ((key (%expect-ident r)))
                     (%expect-punct r "=")
                     (push (cons key (%parse-annotation-value r)) args))
                   (if (%punct-p (%peek r) ",")
                       (%next r)
                       (return))))
               (%expect-punct r ")")
               (push (make-wit-annotation :name name :args (nreverse args))
                     out)))
    (nreverse out)))

(defstruct wit-annotation
  (name "" :type string)
  (args nil))                            ; alist (key . value) strings

;;; ---------------------------------------------------------------
;;; Type expressions.
;;;
;;; AST:
;;;   :u8 :u16 ... :string :bool :char :error-context   primitives
;;;   :wildcard                                          `_`
;;;   (:name "x")                                        a type reference
;;;   (:list T) (:option T) (:stream T) (:future T)
;;;   (:tuple T1 ...) (:map K V) (:result OK ERR)
;;;   (:own "r") (:borrow "r")
;;;   (:record (name . T) ...) (:enum "a" ...)
;;;   (:variant (name . payload-or-nil) ...) (:flags "a" ...)
;;;   (:resource name)                                   a resource head

(defparameter *wit-primitive-names*
  '("u8" "u16" "u32" "u64" "s8" "s16" "s32" "s64"
    "f32" "f64" "bool" "char" "string" "error-context"))

(defun %parse-type (r)
  (let ((tk (%peek r)))
    (cond
      ((null tk) (%werr r "expected a type"))
      ((%punct-p tk "_") (%next r) :wildcard)
      ((eq (wit-token-kind tk) :ident)
       (let ((name (%expect-ident r)))
         (cond
           ((member name *wit-primitive-names* :test #'string=)
            (intern (string-upcase name) :keyword))
           ((string= name "list")
            (list :list (%parse-generic-one r)))
           ((string= name "option")
            (list :option (%parse-generic-one r)))
           ((string= name "stream")
            ;; `stream<error-context>` is written; allow bare `stream`
            ;; as the all-errors form for forward compatibility
            (if (%punct-p (%peek r) "<")
                (list :stream (%parse-generic-one r))
                (list :stream :error-context)))
           ((string= name "future")
            (if (%punct-p (%peek r) "<")
                (list :future (%parse-generic-one r))
                (list :future :error-context)))
           ((string= name "own")
            (list :own (%parse-generic-name r)))
           ((string= name "borrow")
            (list :borrow (%parse-generic-name r)))
           ((string= name "tuple")
            (%expect-punct r "<")
            (let ((items '()))
              (unless (%punct-p (%peek r) ">")
                (loop
                  (push (%parse-type r) items)
                  (if (%punct-p (%peek r) ",")
                      (%next r)
                      (return))))
              (%expect-punct r ">")
              (cons :tuple (nreverse items))))
           ((string= name "result")
            (if (%punct-p (%peek r) "<")
                (progn
                  (%expect-punct r "<")
                  (let ((ok (%parse-type r))
                        (err nil))
                    (when (%punct-p (%peek r) ",")
                      (%next r)
                      (setf err (%parse-type r)))
                    (%expect-punct r ">")
                    (list :result
                          (if (eq ok :wildcard) nil ok)
                          (if (eq err :wildcard) nil err))))
                (list :result nil nil)))
           ((string= name "map")
            (%expect-punct r "<")
            (let ((k (%parse-type r)))
              (%expect-punct r ",")
              (let ((v (%parse-type r)))
                (%expect-punct r ">")
                (list :map k v))))
           (t (list :name name)))))
      ((eq (wit-token-kind tk) :version)
       (%werr r "unexpected version token ~a in a type position"
              (wit-token-text tk)))
      (t (%werr r "expected a type (found ~a)" (wit-token-text tk))))))

(defun %parse-generic-one (r)
  (%expect-punct r "<")
  (let ((ty (%parse-type r)))
    (%expect-punct r ">")
    ty))

(defun %parse-generic-name (r)
  (%expect-punct r "<")
  (let ((name (%expect-ident r)))
    (%expect-punct r ">")
    name))

;;; ---------------------------------------------------------------
;;; Package / interface / world parsing.

(defstruct wit-package
  (namespace "wasi" :type string)
  (basename "" :type string)
  (name "wasi:" :type string)            ; namespace:basename
  (version nil)
  (path nil)
  (annotations nil)
  (interfaces (make-hash-table :test #'equal))
  (worlds (make-hash-table :test #'equal))
  (interface-list nil))

(defstruct wit-interface
  (name "" :type string)
  (package nil)
  (annotations nil)
  (items nil)                            ; declaration order
  (types (make-hash-table :test #'equal))
  (funcs (make-hash-table :test #'equal))
  (func-list nil)                        ; declaration order
  (resources (make-hash-table :test #'equal))
  (resource-list nil)
  (uses nil))

(defstruct wit-type
  (name "" :type string)
  (expr nil)
  (annotations nil))

(defstruct wit-func
  (name "" :type string)
  (params nil)                           ; alist (name . type)
  (result nil)                           ; type or NIL
  (async-p nil)
  (static-p nil)
  (constructor-p nil)
  (annotations nil))

(defstruct wit-resource
  (name "" :type string)
  (methods nil)                          ; list of wit-func
  (annotations nil))

(defstruct wit-use
  (namespace nil)
  (package nil)
  (interface "" :type string)
  (version nil)
  (items nil)                            ; list of imported names
  (annotations nil))

(defstruct wit-world
  (name "" :type string)
  (package nil)
  (annotations nil)
  (imports nil)
  (exports nil))

(defstruct wit-world-item
  (sort :import :type keyword)           ; :import | :export
  (namespace nil)
  (package nil)
  (interface "" :type string)            ; the interface/world name
  (version nil)
  (annotations nil))

(defun %parse-version-opt (r)
  (when (%punct-p (%peek r) "@")
    (%next r)
    (let ((tk (%peek r)))
      (unless (and tk (eq (wit-token-kind tk) :version))
        (%werr r "expected a version after `@`"))
      (%next r)
      (wit-token-text tk))))

(defun %parse-path (r)
  "Parse a use/import/export target. Returns (values namespace package
interface version). A bare name has NIL namespace/package."
  (let ((first (%expect-ident r)))
    (cond
      ((%punct-p (%peek r) ":")
       (%next r)
       (let ((pkg (%expect-ident r)))
         (%expect-punct r "/")
         (let ((iname (%expect-ident r)))
           (values first pkg iname (%parse-version-opt r)))))
      ((%punct-p (%peek r) "@")
       (values nil nil first (%parse-version-opt r)))
      (t (values nil nil first nil)))))

(defun %parse-use (r annotations)
  (multiple-value-bind (ns pkg iname version)
      (progn (%expect-keyword r "use") (%parse-path r))
    (%expect-punct r ".")
    (%expect-punct r "{")
    (let ((items '()))
      (unless (%punct-p (%peek r) "}")
        (loop
          (push (%expect-ident r) items)
          (if (%punct-p (%peek r) ",")
              (%next r)
              (return))))
      (%expect-punct r "}")
      (%expect-punct r ";")
      (make-wit-use :namespace ns :package pkg :interface iname
                    :version version :items (nreverse items)
                    :annotations annotations))))

(defun %parse-record-fields (r)
  (let ((fields '()))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (%parse-annotations r)     ; field annotations recorded nowhere
             (let ((name (%expect-ident r)))
               (%expect-punct r ":")
               (push (cons name (%parse-type r)) fields))
             (%expect-punct r ","))
    (%expect-punct r "}")
    (nreverse fields)))

(defun %parse-labels (r)
  "An enum/flags body: labels separated by commas."
  (let ((labels '()))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (%parse-annotations r)
             (push (%expect-ident r) labels)
             (unless (%punct-p (%peek r) "}")
               (%expect-punct r ",")))
    (%expect-punct r "}")
    (nreverse labels)))

(defun %parse-variant-cases (r)
  (let ((cases '()))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (%parse-annotations r)
             (let ((name (%expect-ident r))
                   (payload nil))
               (when (%punct-p (%peek r) "(")
                 (%next r)
                 (setf payload (%parse-type r))
                 (%expect-punct r ")"))
               (push (cons name payload) cases))
             (unless (%punct-p (%peek r) "}")
               (%expect-punct r ",")))
    (%expect-punct r "}")
    (nreverse cases)))

(defun %parse-func-signature (r name annotations &key (constructor-p nil))
  (let ((async-p nil) (static-p nil))
    (unless constructor-p
      (loop for kw = (%peek r)
            while (and (%ident-p kw)
                       (member (wit-token-text kw) '("async" "static")
                               :test #'string=))
            do (%next r)
               (cond ((string= (wit-token-text kw) "async")
                      (setf async-p t))
                     ((string= (wit-token-text kw) "static")
                      (setf static-p t)))))
    (%expect-keyword r "func")
    (%expect-punct r "(")
    (let ((params '()))
      (unless (%punct-p (%peek r) ")")
        (loop
          (let ((pname (%expect-ident r)))
            (%expect-punct r ":")
            (push (cons pname (%parse-type r)) params))
          (if (%punct-p (%peek r) ",")
              (%next r)
              (return))))
      (%expect-punct r ")")
      (let ((result nil))
        (when (eq (and (%peek r) (wit-token-kind (%peek r))) :arrow)
          (%next r)
          (setf result (%parse-type r)))
        (%expect-punct r ";")
        (make-wit-func :name name :params (nreverse params)
                       :result result :async-p async-p
                       :static-p static-p
                       :constructor-p constructor-p
                       :annotations annotations)))))

(defun %parse-resource (r name annotations)
  (let ((methods '()))
    (if (%punct-p (%peek r) ";")
        (%next r)
        (progn
          (%expect-punct r "{")
          (loop until (%punct-p (%peek r) "}")
                do (let ((as (%parse-annotations r)))
                     (cond
                       ((%keyword-p r "constructor")
                        (%next r)
                        (%expect-punct r "(")
                        (let ((params '()))
                          (unless (%punct-p (%peek r) ")")
                            (loop
                              (let ((pname (%expect-ident r)))
                                (%expect-punct r ":")
                                (push (cons pname (%parse-type r)) params))
                              (if (%punct-p (%peek r) ",")
                                  (%next r)
                                  (return))))
                          (%expect-punct r ")")
                          (%expect-punct r ";")
                          (push (make-wit-func :name ""
                                               :params (nreverse params)
                                               :constructor-p t
                                               :annotations as)
                                methods)))
                       (t
                        (let ((mname (%expect-ident r)))
                          (%expect-punct r ":")
                          (push (%parse-func-signature r mname as) methods))))))
          (%expect-punct r "}")))
    (make-wit-resource :name name :methods (nreverse methods)
                       :annotations annotations)))

(defun %parse-interface-item (r iface annotations)
  (let ((item
          (cond
            ((%keyword-p r "use")
             (let ((u (%parse-use r annotations)))
               (push u (wit-interface-uses iface))
               (push u (wit-interface-items iface))
               u))
            ((%keyword-p r "type")
             (%next r)
             (let* ((name (%expect-ident r))
                    (ty (progn (%expect-punct r "=")
                               (make-wit-type
                                :name name :expr (%parse-type r)
                                :annotations annotations))))
               (%expect-punct r ";")
               (setf (gethash name (wit-interface-types iface)) ty)
               (push ty (wit-interface-items iface))
               ty))
            ((%keyword-p r "record")
             (%next r)
             (let* ((name (%expect-ident r))
                    (ty (make-wit-type
                         :name name
                         :expr (cons :record (%parse-record-fields r))
                         :annotations annotations)))
               (setf (gethash name (wit-interface-types iface)) ty)
               (push ty (wit-interface-items iface))
               ty))
            ((%keyword-p r "enum")
             (%next r)
             (let* ((name (%expect-ident r))
                    (ty (make-wit-type
                         :name name :expr (cons :enum (%parse-labels r))
                         :annotations annotations)))
               (setf (gethash name (wit-interface-types iface)) ty)
               (push ty (wit-interface-items iface))
               ty))
            ((%keyword-p r "flags")
             (%next r)
             (let* ((name (%expect-ident r))
                    (ty (make-wit-type
                         :name name :expr (cons :flags (%parse-labels r))
                         :annotations annotations)))
               (setf (gethash name (wit-interface-types iface)) ty)
               (push ty (wit-interface-items iface))
               ty))
            ((%keyword-p r "variant")
             (%next r)
             (let* ((name (%expect-ident r))
                    (ty (make-wit-type
                         :name name
                         :expr (cons :variant (%parse-variant-cases r))
                         :annotations annotations)))
               (setf (gethash name (wit-interface-types iface)) ty)
               (push ty (wit-interface-items iface))
               ty))
            ((%keyword-p r "resource")
             (%next r)
             (let* ((name (%expect-ident r))
                    (res (%parse-resource r name annotations))
                    (ty (make-wit-type :name name
                                       :expr (list :resource name)
                                       :annotations annotations)))
               (setf (gethash name (wit-interface-types iface)) ty)
               (setf (gethash name (wit-interface-resources iface)) res)
               (push res (wit-interface-resource-list iface))
               (push ty (wit-interface-items iface))
               res))
            (t
             ;; a plain function: name: [static] [async] func(...) ...
             (let* ((name (%expect-ident r))
                    (fn (progn (%expect-punct r ":")
                               (%parse-func-signature r name annotations))))
               (setf (gethash name (wit-interface-funcs iface)) fn)
               (push fn (wit-interface-func-list iface))
               (push fn (wit-interface-items iface))
               fn)))))
    item))

(defun %parse-interface (r package annotations)
  (%expect-keyword r "interface")
  (let* ((name (%expect-ident r))
         (iface (make-wit-interface :name name :package package
                                    :annotations annotations)))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (%parse-interface-item r iface (%parse-annotations r)))
    (%expect-punct r "}")
    (setf (wit-interface-func-list iface)
          (nreverse (wit-interface-func-list iface)))
    (setf (wit-interface-resource-list iface)
          (nreverse (wit-interface-resource-list iface)))
    (setf (wit-interface-items iface)
          (nreverse (wit-interface-items iface)))
    (setf (wit-interface-uses iface)
          (nreverse (wit-interface-uses iface)))
    (setf (gethash name (wit-package-interfaces package)) iface)
    (push iface (wit-package-interface-list package))
    iface))

(defun %parse-world (r package annotations)
  (%expect-keyword r "world")
  (let* ((name (%expect-ident r))
         (world (make-wit-world :name name :package package
                                :annotations annotations)))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (let ((as (%parse-annotations r)))
               (let ((sort (cond ((%keyword-p r "import") :import)
                                 ((%keyword-p r "export") :export)
                                 (t nil))))
                 (unless sort
                   (%werr r "expected `import` or `export` in world ~a"
                          name))
                 (%next r)
                 (multiple-value-bind (ns pkg iname version)
                     (%parse-path r)
                   (%expect-punct r ";")
                   (let ((item (make-wit-world-item
                                :sort sort :namespace ns :package pkg
                                :interface iname :version version
                                :annotations as)))
                     (if (eq sort :import)
                         (push item (wit-world-imports world))
                         (push item (wit-world-exports world))))))))
    (%expect-punct r "}")
    (setf (wit-world-imports world) (nreverse (wit-world-imports world)))
    (setf (wit-world-exports world) (nreverse (wit-world-exports world)))
    (setf (gethash name (wit-package-worlds package)) world)
    world))

(defun %parse-package (r)
  (let* ((annotations (%parse-annotations r))
         (ns nil) (base nil) (version nil))
    (%expect-keyword r "package")
    (let ((first (%expect-ident r)))
      (if (%punct-p (%peek r) ":")
          (progn
            (%next r)
            (setf ns first)
            (setf base (%expect-ident r)))
          (setf base first)))
    (setf version (%parse-version-opt r))
    (%expect-punct r ";")
    (let ((package (make-wit-package
                    :namespace (or ns "")
                    :basename base
                    :name (if ns (format nil "~a:~a" ns base) base)
                    :version version
                    :path (wit-reader-path r)
                    :annotations annotations)))
      (loop while (%peek r)
            do (let ((as (%parse-annotations r)))
                 (cond
                   ((%keyword-p r "interface")
                    (%parse-interface r package as))
                   ((%keyword-p r "world")
                    (%parse-world r package as))
                   (t (%werr r "expected `interface` or `world`~@[ (found ~a)~]"
                             (and (%peek r) (wit-token-text (%peek r))))))))
      (setf (wit-package-interface-list package)
            (nreverse (wit-package-interface-list package)))
      package)))

;;; ---------------------------------------------------------------
;;; Public entry points.

(defun parse-wit-string (string &key (path nil))
  "Parse one WIT source unit (one package) into a wit-package."
  (%parse-package (make-wit-reader* string :path path)))

(defun read-file-string (path)
  (with-open-file (in path :direction :input
                           :external-format :utf-8
                           :if-does-not-exist :error)
    (with-output-to-string (out)
      (loop for line = (read-line in nil nil)
            while line
            do (write-string line out) (terpri out)))))

(defun parse-wit-file (path)
  "Parse the WIT file at PATH into a wit-package."
  (parse-wit-string (read-file-string path)
                    :path (namestring path)))

;;; ---------------------------------------------------------------
;;; Node lookups.

(defun package-interface (package name)
  (and package (gethash name (wit-package-interfaces package))))

(defun interface-import-name (interface)
  "The component-model import name of INTERFACE: ns:pkg/iface@version."
  (let ((package (wit-interface-package interface)))
    (format nil "~a/~a~@[@~a~]"
            (if (plusp (length (wit-package-namespace package)))
                (wit-package-name package)
                (wit-package-basename package))
            (wit-interface-name interface)
            (wit-package-version package))))

;;; ---------------------------------------------------------------
;;; Type resolution + conversion to the runtime's component ct.
;;;
;;; The reader keeps `(... :name "x")` references unresolved; here they
;;; are resolved through the interface's own definitions and its
;;; `use` imports. PACKAGES is an optional list of wit-packages used to
;;; resolve a `use` that names another package (e.g. filesystem's
;;; `use wasi:clocks/system-clock@0.3.2.{instant}`).

(defun %find-package (namespace package-name version packages)
  (find-if (lambda (p)
             (and (string= (wit-package-basename p) package-name)
                  (or (null namespace)
                      (string= (wit-package-namespace p) namespace))
                  (or (null version)
                      (equal (wit-package-version p) version))))
           packages))

(defun %used-interface (use scope packages)
  (if (null (wit-use-package use))
      ;; same package
      (package-interface (wit-interface-package scope)
                         (wit-use-interface use))
      (let ((pkg (%find-package (wit-use-namespace use)
                                (wit-use-package use)
                                (wit-use-version use)
                                packages)))
        (and pkg (package-interface pkg (wit-use-interface use))))))

(defun %lookup-type (scope name packages)
  "Resolve NAME in SCOPE (an interface): (values type-node interface).
Follows `use` imports; NIL when unknown."
  (let ((ty (gethash name (wit-interface-types scope))))
    (if ty
        (values ty scope)
        (dolist (use (wit-interface-uses scope) (values nil nil))
          (when (member name (wit-use-items use) :test #'string=)
            (let ((target (%used-interface use scope packages)))
              (when target
                (let ((hit (gethash name (wit-interface-types target))))
                  (when hit (return (values hit target)))))))))))

(defun resolve-wit-type (interface name &key packages)
  "The wit-type node NAMED by NAME in INTERFACE (or NIL)."
  (nth-value 0 (%lookup-type interface name packages)))

(defun %named-expr (scope name packages seen)
  (when (member name seen :test #'string=)
    (error 'wit-error
           :message (format nil "recursive WIT type reference ~a" name)))
  (multiple-value-bind (ty target) (%lookup-type scope name packages)
    (unless ty
      (error 'wit-error
             :message (format nil "unknown WIT type ~a in interface ~a"
                              name (wit-interface-name scope))))
    (values (wit-type-expr ty) target)))

(defun wit-type->ct (interface expr &key packages)
  "Convert the WIT type EXPR (resolved in INTERFACE) to a
wasmdown.component ct. Named references are resolved recursively."
  (labels
      ((conv (expr scope seen)
         (cond
           ((keywordp expr)
            (if (eq expr :wildcard)
                nil
                (wasmdown.component:make-ct :kind :prim :payload expr)))
           ((not (consp expr)) expr)
           (t
            (case (car expr)
              (:name
               (multiple-value-bind (inner target)
                   (%named-expr scope (second expr) packages seen)
                 (conv inner target
                       (cons (second expr) seen))))
              (:list
               (wasmdown.component:make-ct
                :kind :list
                :payload (cons (conv (second expr) scope seen) nil)))
              (:tuple
               (wasmdown.component:make-ct
                :kind :tuple
                :payload (coerce (loop for e in (cdr expr)
                                       collect (conv e scope seen))
                                 'vector)))
              (:option
               (wasmdown.component:make-ct
                :kind :option :payload (conv (second expr) scope seen)))
              (:stream
               (wasmdown.component:make-ct
                :kind :stream :payload (conv (second expr) scope seen)))
              (:future
               (wasmdown.component:make-ct
                :kind :future :payload (conv (second expr) scope seen)))
              (:result
               (wasmdown.component:make-ct
                :kind :result
                :payload (cons (conv (second expr) scope seen)
                               (conv (third expr) scope seen))))
              (:map
               (wasmdown.component:make-ct
                :kind :map
                :payload (cons (conv (second expr) scope seen)
                               (conv (third expr) scope seen))))
              (:record
               (wasmdown.component:make-ct
                :kind :record
                :payload (coerce
                          (loop for (name . te) in (cdr expr)
                                collect (cons name (conv te scope seen)))
                          'vector)))
              (:enum
               (wasmdown.component:make-ct
                :kind :enum
                :payload (coerce (cdr expr) 'vector)))
              (:flags
               (wasmdown.component:make-ct
                :kind :flags
                :payload (coerce (cdr expr) 'vector)))
              (:variant
               (wasmdown.component:make-ct
                :kind :variant
                :payload (coerce
                          (loop for (name . te) in (cdr expr)
                                collect (cons name
                                              (and te (conv te scope seen))))
                          'vector)))
              ((:own :borrow)
               ;; slice 1 does not implement resource-typed providers;
               ;; the name is kept so the caller sees the shape.
               (wasmdown.component:make-ct :kind (car expr)
                                           :payload (second expr)))
              (:resource
               (wasmdown.component:make-ct :kind :resource
                                           :payload (second expr)))
              (t
               (error 'wit-error
                      :message (format nil "unknown WIT type form ~s"
                                       expr))))))))
    (conv expr interface nil)))

(defun wit-func->ct (interface func &key packages)
  "The component FUNCTYPE ct of FUNC, with (params . result) derived
from the WIT and the async flag recorded in the payload's third slot."
  (let* ((params (coerce
                  (loop for (name . te) in (wit-func-params func)
                        collect (cons name
                                      (wit-type->ct interface te
                                                    :packages packages)))
                  'vector))
         (result (and (wit-func-result func)
                      (wit-type->ct interface (wit-func-result func)
                                    :packages packages))))
    (wasmdown.component:make-ct
     :kind :func
     :payload (list params result (wit-func-async-p func)))))
