;;;; WASMDOWN — the WIT text frontend (docs/design/PLAN.md §8.1, M16 slice 1).
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
;;;; filesystem/sockets slices can extend it without rewriting.

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
  "Read the `@name(...)` forms at the reader position. Both the named
form `@since(version = 0.2.0)` and the positional form
`@external-id(\"https://...\")` are accepted; NAMED arguments go to the
annotation's ARGS alist and positional ones to its VALUES list. One
annotation may not mix the two spellings."
  (let ((out '()))
    (loop while (%punct-p (%peek r) "@")
          do (%next r)
             (let ((name (%expect-ident r))
                   (args '())
                   (values '()))
               (%expect-punct r "(")
               (unless (%punct-p (%peek r) ")")
                 (loop
                   (let ((tk (%peek r)))
                     (if (and (eq (and tk (wit-token-kind tk)) :ident)
                              (%punct-p (wit-reader-peek r 1) "="))
                         (let ((key (%expect-ident r)))
                           (%expect-punct r "=")
                           (push (cons key (%parse-annotation-value r)) args))
                         (push (%parse-annotation-value r) values)))
                   (if (%punct-p (%peek r) ",")
                       (%next r)
                       (return))))
               (%expect-punct r ")")
               (when (and args values)
                 (%werr r "annotation `@~a` mixes positional and named arguments"
                        name))
               (push (make-wit-annotation :name name
                                          :args (nreverse args)
                                          :values (nreverse values))
                     out)))
    (nreverse out)))

(defstruct wit-annotation
  (name "" :type string)
  (args nil)                             ; alist (key . value) strings
  (values nil))                          ; positional argument strings

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
  ;; M16 slice 3: resource-name -> the ONE ct object that names it.
  ;; A host provider's handle table keys resource handles by the ct
  ;; object (the runtime's own `rt' convention), so every occurrence
  ;; of `descriptor' — a method's self borrow, `open-at's own
  ;; result, the exported type — MUST resolve to the same object or
  ;; the guest's canon resource.drop would see a type mismatch.
  (resource-cts (make-hash-table :test #'equal))
  (uses nil))

(defstruct wit-type
  (name "" :type string)
  (expr nil)
  (annotations nil)
  ;; member-name -> the member's own annotations (alist), for record
  ;; fields, enum/flags labels and variant cases. A member gate must
  ;; never be silently dropped (that would change the layout), so the
  ;; selection layer inspects this table instead of re-reading source.
  (member-annotations nil))

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
  (items nil)                            ; list of imported SOURCE names
  (renames nil)                          ; alist (source . local)
  (annotations nil))

(defstruct wit-world
  (name "" :type string)
  (package nil)
  (annotations nil)
  (imports nil)
  (exports nil)
  ;; declaration order over every world definition: wit-world-item and
  ;; wit-world-include nodes, interleaved. INCLUDES holds just the
  ;; include nodes (for callers that want the raw union declarations).
  (items nil)
  (includes nil)
  (uses nil)                             ; list of wit-use
  ;; world-level typedefs share the interface scope operations (see
  ;; %SCOPE-TYPES), so a world is a type-resolution scope too
  (types (make-hash-table :test #'equal))
  (resource-cts (make-hash-table :test #'equal)))

(defstruct wit-world-item
  (sort :import :type keyword)           ; :import | :export | :use | :type
  ;; :interface         a path import/export (bare or `name: path')
  ;; :inline-interface  `import name: interface { ... }'
  ;; :func              `import name: func(...)'
  ;; :type / :resource  a world-level typedef
  ;; :use               a world-level `use'
  (kind :interface :type keyword)
  ;; the explicit plain binding name (`name:' prefix), NIL for a bare
  ;; interface-path binding
  (name nil)
  (namespace nil)
  (package nil)
  (interface "" :type string)            ; the interface/world name
  (version nil)
  ;; after resolution: the resolved wit-interface / wit-func / wit-type
  ;; / wit-resource / wit-use node; at parse time only the inline
  ;; (self-contained) kinds carry it
  (target nil)
  ;; for `name: use-path' the component-level (implements "...") value,
  ;; in the same string shape %ATTR-OF / %CHECK-NAME-ATTRS carry
  (implements nil)
  ;; the positional @external-id("...") string
  (external-id nil)
  (annotations nil))

(defstruct wit-world-include
  ;; (namespace package interface version), the world being included
  (target nil)
  ;; (source . local) pairs; a rename rewrites only the included
  ;; world's plain binding labels
  (renames nil)
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
  "Parse `use <path>.{a, b as c};`. ITEMS keeps the source names (so
existing lookups keep working); RENAMES records (source . local)."
  (multiple-value-bind (ns pkg iname version)
      (progn (%expect-keyword r "use") (%parse-path r))
    (%expect-punct r ".")
    (%expect-punct r "{")
    (let ((items '())
          (renames '()))
      (unless (%punct-p (%peek r) "}")
        (loop
          (let ((item (%expect-ident r)))
            (push item items)
            (when (%keyword-p r "as")
              (%next r)
              (push (cons item (%expect-ident r)) renames)))
          (if (%punct-p (%peek r) ",")
              (%next r)
              (return))))
      (%expect-punct r "}")
      (%expect-punct r ";")
      (make-wit-use :namespace ns :package pkg :interface iname
                    :version version :items (nreverse items)
                    :renames (nreverse renames)
                    :annotations annotations))))

(defun %parse-record-fields (r)
  "Parse a record body. Returns (values FIELDS MEMBER-ANNOTATIONS):
FIELDS is (name . type) in declaration order, MEMBER-ANNOTATIONS maps
a field name to its own annotations (only when it has any)."
  (let ((fields '())
        (member-annotations '()))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (let ((as (%parse-annotations r)))
               (let ((name (%expect-ident r)))
                 (%expect-punct r ":")
                 (push (cons name (%parse-type r)) fields)
                 (when as (push (cons name as) member-annotations))))
             ;; a trailing comma before `}' is legal
             (unless (%punct-p (%peek r) "}")
               (%expect-punct r ",")))
    (%expect-punct r "}")
    (values (nreverse fields) (nreverse member-annotations))))

(defun %parse-labels (r)
  "An enum/flags body: labels separated by commas. Returns (values
LABELS MEMBER-ANNOTATIONS); see %PARSE-RECORD-FIELDS."
  (let ((labels '())
        (member-annotations '()))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (let ((as (%parse-annotations r)))
               (let ((label (%expect-ident r)))
                 (push label labels)
                 (when as (push (cons label as) member-annotations))))
             (unless (%punct-p (%peek r) "}")
               (%expect-punct r ",")))
    (%expect-punct r "}")
    (values (nreverse labels) (nreverse member-annotations))))

(defun %parse-variant-cases (r)
  "A variant body. Returns (values CASES MEMBER-ANNOTATIONS); CASES is
(name . payload-or-nil) in declaration order."
  (let ((cases '())
        (member-annotations '()))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (let ((as (%parse-annotations r)))
               (let ((name (%expect-ident r))
                     (payload nil))
                 (when (%punct-p (%peek r) "(")
                   (%next r)
                   (setf payload (%parse-type r))
                   (%expect-punct r ")"))
                 (push (cons name payload) cases)
                 (when as (push (cons name as) member-annotations))))
             (unless (%punct-p (%peek r) "}")
               (%expect-punct r ",")))
    (%expect-punct r "}")
    (values (nreverse cases) (nreverse member-annotations))))

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
          ;; a trailing comma before `)' is legal in WIT
          (if (%punct-p (%peek r) ",")
              (progn
                (%next r)
                (when (%punct-p (%peek r) ")")
                  (return)))
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
             (let ((name (%expect-ident r)))
               (multiple-value-bind (fields member-annotations)
                   (%parse-record-fields r)
                 (let ((ty (make-wit-type
                            :name name
                            :expr (cons :record fields)
                            :annotations annotations
                            :member-annotations member-annotations)))
                   (setf (gethash name (wit-interface-types iface)) ty)
                   (push ty (wit-interface-items iface))
                   ty))))
            ((%keyword-p r "enum")
             (%next r)
             (let ((name (%expect-ident r)))
               (multiple-value-bind (labels member-annotations)
                   (%parse-labels r)
                 (let ((ty (make-wit-type
                            :name name :expr (cons :enum labels)
                            :annotations annotations
                            :member-annotations member-annotations)))
                   (setf (gethash name (wit-interface-types iface)) ty)
                   (push ty (wit-interface-items iface))
                   ty))))
            ((%keyword-p r "flags")
             (%next r)
             (let ((name (%expect-ident r)))
               (multiple-value-bind (labels member-annotations)
                   (%parse-labels r)
                 (let ((ty (make-wit-type
                            :name name :expr (cons :flags labels)
                            :annotations annotations
                            :member-annotations member-annotations)))
                   (setf (gethash name (wit-interface-types iface)) ty)
                   (push ty (wit-interface-items iface))
                   ty))))
            ((%keyword-p r "variant")
             (%next r)
             (let ((name (%expect-ident r)))
               (multiple-value-bind (cases member-annotations)
                   (%parse-variant-cases r)
                 (let ((ty (make-wit-type
                            :name name :expr (cons :variant cases)
                            :annotations annotations
                            :member-annotations member-annotations)))
                   (setf (gethash name (wit-interface-types iface)) ty)
                   (push ty (wit-interface-items iface))
                   ty))))
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

(defun %finalize-interface (iface)
  (setf (wit-interface-func-list iface)
        (nreverse (wit-interface-func-list iface)))
  (setf (wit-interface-resource-list iface)
        (nreverse (wit-interface-resource-list iface)))
  (setf (wit-interface-items iface)
        (nreverse (wit-interface-items iface)))
  (setf (wit-interface-uses iface)
        (nreverse (wit-interface-uses iface)))
  iface)

(defun %parse-interface (r package annotations)
  (%expect-keyword r "interface")
  (let* ((name (%expect-ident r))
         (iface (make-wit-interface :name name :package package
                                    :annotations annotations)))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (%parse-interface-item r iface (%parse-annotations r)))
    (%expect-punct r "}")
    (%finalize-interface iface)
    (when (package-interface package name)
      (%werr r "duplicate interface `~a` in package ~a" name
             (wit-package-name package)))
    (setf (gethash name (wit-package-interfaces package)) iface)
    (push iface (wit-package-interface-list package))
    iface))

;;; ---------------------------------------------------------------
;;; Worlds.
;;;
;;; The world grammar (specs/WIT.md, "Item: world") is wider than the
;;; vendored WASI snapshot uses. The raw AST records every definition
;;; verbatim:
;;;
;;;   import <path>;                     a bare interface-name binding
;;;   import name: <path>;               a named interface binding
;;;   import name: interface { ... };    an inline interface
;;;   import name: func(...) -> T;       a world function
;;;   export ...                         the same three
;;;   use <path>.{a, b as c};            a world-level type import
;;;   type/record/enum/flags/variant/resource ...
;;;   include <path> [with { a as b, ... }];
;;;
;;; `implements' is NOT a WIT keyword: a named interface binding yields
;;; one, exactly the attribute %ATTR-OF/%CHECK-NAME-ATTRS already
;;; carry, so the item stores its value directly.

(defun %token-adjacent-p (a b)
  "Do tokens A and B touch in the source (no blank between them)?"
  (and a b (= (wit-token-start b)
              (+ (wit-token-start a) (length (wit-token-text a))))))

(defun %world-named-binding-p (r)
  "After `import'/'export', is the next form `name: <extern-type>'?
The spec's lexing rule distinguishes `a:b' (a package path) from
`a: b' (a named binding) by the blank AFTER the colon."
  (let ((tk (%peek r))
        (colon (wit-reader-peek r 1))
        (after (wit-reader-peek r 2)))
    (and (%ident-p tk)
         (%punct-p colon ":")
         (or (null after) (not (%token-adjacent-p colon after))))))

(defun %world-external-id (r annotations)
  "The positional @external-id(\"...\") value in ANNOTATIONS, or NIL.
The annotation itself stays in the raw list."
  (let ((a (find "external-id" annotations
                 :key #'wit-annotation-name :test #'string=)))
    (cond
      ((null a) nil)
      ((or (wit-annotation-args a)
           (not (eql 1 (length (wit-annotation-values a)))))
       (%werr r "@external-id takes exactly one positional string argument"))
      (t (first (wit-annotation-values a))))))

(defun %interface-reference-string (package ns pkg iface version)
  "The interface name of a path: ns:pkg/iface@version. A same-package
reference gets the package's own name (specs/WIT.md: `a' is short for
`local:demo/a')."
  (if (or ns pkg)
      (format nil "~@[~a:~]~a/~a~@[@~a~]" ns pkg iface version)
      (format nil "~a/~a~@[@~a~]" (wit-package-name package) iface version)))

(defun %parse-inline-interface (r package name annotations)
  (%expect-keyword r "interface")
  (let ((iface (make-wit-interface :name name :package package
                                   :annotations annotations)))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (%parse-interface-item r iface (%parse-annotations r)))
    (%expect-punct r "}")
    (%finalize-interface iface)))

(defun %parse-include (r annotations)
  (%expect-keyword r "include")
  (multiple-value-bind (ns pkg iname version) (%parse-path r)
    (let ((renames '()))
      (when (%keyword-p r "with")
        (%next r)
        (%expect-punct r "{")
        (loop until (%punct-p (%peek r) "}")
              do (let ((source (%expect-ident r)))
                   (%expect-keyword r "as")
                   (push (cons source (%expect-ident r)) renames))
                 (unless (%punct-p (%peek r) "}")
                   (%expect-punct r ",")))
        (%expect-punct r "}"))
      (%expect-punct r ";")
      (make-wit-world-include
       :target (list ns pkg iname version)
       :renames (nreverse renames)
       :annotations annotations))))

(defun %parse-world-extern (r world sort annotations)
  "Parse the body of an `import'/'export' after its keyword."
  (%next r)
  (let ((external-id (%world-external-id r annotations)))
    (if (%world-named-binding-p r)
        (let ((binding (%expect-ident r)))
          (%expect-punct r ":")
          (cond
            ((%keyword-p r "func")
             (let ((fn (%parse-func-signature r binding annotations)))
               (%world-add-item world
                                (make-wit-world-item
                                 :sort sort :kind :func :name binding
                                 :target fn :external-id external-id
                                 :annotations annotations))))
            ((%keyword-p r "interface")
             (let ((iface (%parse-inline-interface r
                                                   (wit-world-package world)
                                                   binding annotations)))
               (%world-add-item world
                                (make-wit-world-item
                                 :sort sort :kind :inline-interface
                                 :name binding :target iface
                                 :external-id external-id
                                 :annotations annotations))))
            (t
             (multiple-value-bind (ns pkg iname version) (%parse-path r)
               (%expect-punct r ";")
               (%world-add-item world
                                (make-wit-world-item
                                 :sort sort :kind :interface :name binding
                                 :namespace ns :package pkg
                                 :interface iname :version version
                                 :implements
                                 (%interface-reference-string
                                  (wit-world-package world)
                                  ns pkg iname version)
                                 :external-id external-id
                                 :annotations annotations))))))
        (multiple-value-bind (ns pkg iname version) (%parse-path r)
          (%expect-punct r ";")
          (%world-add-item world
                           (make-wit-world-item
                            :sort sort :kind :interface
                            :namespace ns :package pkg
                            :interface iname :version version
                            :external-id external-id
                            :annotations annotations))))))

(defun %world-add-item (world item)
  "Append ITEM to WORLD's per-sort list and to its declaration-order
ITEMS list."
  (push item (wit-world-items world))
  (case (wit-world-item-sort item)
    (:import (push item (wit-world-imports world)))
    (:export (push item (wit-world-exports world))))
  item)

(defun %parse-world (r package annotations)
  (%expect-keyword r "world")
  (let* ((name (%expect-ident r))
         (world (make-wit-world :name name :package package
                                :annotations annotations))
         ;; world-level typedefs reuse the interface-item reader; the
         ;; scratch scope only collects the parsed nodes
         (scratch (make-wit-interface :name name :package package)))
    (%expect-punct r "{")
    (loop until (%punct-p (%peek r) "}")
          do (let ((as (%parse-annotations r)))
               (cond
                 ((%keyword-p r "include")
                  (let ((inc (%parse-include r as)))
                    (push inc (wit-world-items world))
                    (push inc (wit-world-includes world))))
                 ((%keyword-p r "use")
                  (let ((u (%parse-use r as)))
                    (push u (wit-world-uses world))
                    (push (make-wit-world-item :sort :use :kind :use
                                               :target u :annotations as)
                          (wit-world-items world))))
                 ((or (%keyword-p r "type") (%keyword-p r "record")
                      (%keyword-p r "enum") (%keyword-p r "flags")
                      (%keyword-p r "variant") (%keyword-p r "resource"))
                  (let ((node (%parse-interface-item r scratch as)))
                    (typecase node
                      (wit-resource
                       (let ((ty (gethash (wit-resource-name node)
                                          (wit-interface-types scratch))))
                         (setf (gethash (wit-resource-name node)
                                        (wit-world-types world))
                               ty)
                         (push (make-wit-world-item
                                :sort :type :kind :resource
                                :name (wit-resource-name node)
                                :target node :annotations as)
                               (wit-world-items world))))
                      (t
                       (setf (gethash (wit-type-name node)
                                      (wit-world-types world))
                             node)
                       (push (make-wit-world-item
                              :sort :type :kind :type
                              :name (wit-type-name node)
                              :target node :annotations as)
                             (wit-world-items world))))))
                 ((%keyword-p r "import")
                  (%parse-world-extern r world :import as))
                 ((%keyword-p r "export")
                  (%parse-world-extern r world :export as))
                 (t
                  (%werr r "expected a world definition in world ~a~@[ (found ~a)~]"
                         name (and (%peek r) (wit-token-text (%peek r))))))))
    (%expect-punct r "}")
    (setf (wit-world-items world) (nreverse (wit-world-items world))
          (wit-world-imports world) (nreverse (wit-world-imports world))
          (wit-world-exports world) (nreverse (wit-world-exports world))
          (wit-world-includes world) (nreverse (wit-world-includes world))
          (wit-world-uses world) (nreverse (wit-world-uses world)))
    (when (gethash name (wit-package-worlds package))
      (%werr r "duplicate world `~a` in package ~a" name
             (wit-package-name package)))
    (setf (gethash name (wit-package-worlds package)) world)
    world))

(defun %parse-package-header (r)
  "Parse one `package ns:name@version;' header and return the empty
wit-package. Declarations are parsed separately so several source
units can contribute to ONE package."
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
    (make-wit-package
     :namespace (or ns "")
     :basename base
     :name (if ns (format nil "~a:~a" ns base) base)
     :version version
     :path (wit-reader-path r)
     :annotations annotations)))

(defun %parse-package-declarations (r package)
  "Parse every interface/world declaration R still holds into PACKAGE.
A duplicate definition is a wit-error naming the collision."
  (loop while (%peek r)
        do (let ((as (%parse-annotations r)))
             (cond
               ((%keyword-p r "interface")
                (let ((iface (%parse-interface r package as)))
                  (declare (ignore iface))))
               ((%keyword-p r "world")
                (let ((world (%parse-world r package as)))
                  (declare (ignore world))))
               (t (%werr r "expected `interface` or `world`~@[ (found ~a)~]"
                         (and (%peek r) (wit-token-text (%peek r)))))))))

(defun %finish-package (package)
  (setf (wit-package-interface-list package)
        (nreverse (wit-package-interface-list package)))
  package)

(defun %parse-package (r)
  (let ((package (%parse-package-header r)))
    (%parse-package-declarations r package)
    (%finish-package package)))

;;; ---------------------------------------------------------------
;;; Public entry points.

(defun parse-wit-string (string &key (path nil) stable-only features)
  "Parse one WIT source unit (one package) into a wit-package.

With STABLE-ONLY, the declared surface is run through the stability
selection with FEATURES enabled: `@unstable(feature = ...)' interfaces
and items whose feature is not listed are omitted, and so are items
whose `@since' is newer than the package's declared version. The
default (NIL) returns the full declared surface, annotations intact."
  (%apply-stability (%parse-package (make-wit-reader* string :path path))
                    stable-only features))

(defun read-file-string (path)
  (with-open-file (in path :direction :input
                           :external-format :utf-8
                           :if-does-not-exist :error)
    (with-output-to-string (out)
      (loop for line = (read-line in nil nil)
            while line
            do (write-string line out) (terpri out)))))

(defun parse-wit-file (path &key stable-only features)
  "Parse the WIT file at PATH into a wit-package. See
PARSE-WIT-STRING for STABLE-ONLY and FEATURES."
  (parse-wit-string (read-file-string path)
                    :path (namestring path)
                    :stable-only stable-only
                    :features features))

;;; ---------------------------------------------------------------
;;; Multi-file packages.
;;;
;;; A released WIT package may split its declarations over several
;;; files, some WITHOUT a `package' header (the official WASI 0.2
;;; releases do exactly that). The directory loader reads every .wit
;;; file in lexical order, takes the package identity from the header
;;; file(s), and parses the headerless files as continuations of that
;;; package. Nothing is rewritten and no header is stripped from the
;;; vendored sources: each file is read by its own reader.

(defun %wit-directory-pathname (path)
  "PATH as a directory pathname (a file pathname becomes its parent +
name as the last directory component)."
  (let ((pn (pathname path)))
    (if (or (null (pathname-name pn)) (null (pathname-type pn)))
        pn
        (make-pathname :directory (append (or (pathname-directory pn) '(:relative))
                                          (list (pathname-name pn)))
                       :name nil :type nil :defaults pn))))

(defun %wit-directory-p (path)
  (handler-case (let ((p (probe-file path)))
                  (and p (null (pathname-name p)) (null (pathname-type p))))
    (error () nil)))

(defun %wit-source-files (path)
  "The .wit files of a package directory (lexical order), or PATH
itself when it names a file."
  (let ((pn (pathname path)))
    (cond
      ((and (probe-file pn) (not (%wit-directory-p pn)))
       (list pn))
      (t
       (sort (directory (merge-pathnames "*.wit" (%wit-directory-pathname pn)))
             #'string< :key #'namestring)))))

(defun %source-package-header-p (text)
  "Does TEXT start with a `package ...;' header (after annotations)?"
  (handler-case
      (let ((r (make-wit-reader* text)))
        (%parse-annotations r)
        (let ((tk (%peek r)))
          (and tk (string= "package" (wit-token-text tk)))))
    (error () nil)))

(defun parse-wit-directory (path &key stable-only features)
  "Load a WIT package spread over one or more .wit files. Returns the
wit-package. Errors: no usable file, no package identity, conflicting
package headers, duplicate definitions. See PARSE-WIT-STRING for
STABLE-ONLY and FEATURES."
  (parse-wit-files (%wit-source-files path) :origin (namestring path)
                                          :stable-only stable-only
                                          :features features))

(defun parse-wit-files (files &key (origin nil) stable-only features)
  "Load the WIT package whose source files are FILES (already in the
intended order). See PARSE-WIT-DIRECTORY."
  (let* ((readers '())
         (package nil)
         (identity nil))
    (when (null files)
      (error 'wit-error
             :message (format nil "no .wit files in ~a" (or origin "the given file list"))))
    (dolist (file files)
      (let* ((text (read-file-string file))
             (r (make-wit-reader* text :path (namestring file))))
        (if (%source-package-header-p text)
            (let* ((pkg (%parse-package-header r))
                   (id (list (wit-package-namespace pkg)
                             (wit-package-basename pkg)
                             (wit-package-version pkg))))
              (cond
                ((null package)
                 (setf package pkg identity id))
                ((not (equal id identity))
                 (error 'wit-error
                        :path (namestring file)
                        :message
                        (format nil "conflicting package header ~a in ~a (already ~a:~a@~a)"
                                (wit-package-name pkg) (file-namestring file)
                                (first identity) (second identity) (third identity)))))
              (push r readers))
            ;; headerless continuation: parsed against the identity the
            ;; header file(s) establish
            (push r readers))))
    (unless package
      (error 'wit-error
             :message (format nil "no `package' header in ~a" (or origin "the given file list"))))
    (dolist (r (nreverse readers))
      (%parse-package-declarations r package))
    (%apply-stability (%finish-package package) stable-only features)))

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

;;; A world is a type-resolution scope as well as an interface: a
;;; world-level `use' and world-level typedefs live in the same tables
;;; the interface reader fills. These accessors keep %LOOKUP-TYPE and
;;; friends scope-polymorphic without duplicating the resolution code.

(defun %scope-package (scope)
  (if (wit-world-p scope)
      (wit-world-package scope)
      (wit-interface-package scope)))

(defun %scope-name (scope)
  (if (wit-world-p scope)
      (wit-world-name scope)
      (wit-interface-name scope)))

(defun %scope-kind (scope)
  (if (wit-world-p scope) "world" "interface"))

(defun %scope-types (scope)
  (if (wit-world-p scope)
      (wit-world-types scope)
      (wit-interface-types scope)))

(defun %scope-uses (scope)
  (if (wit-world-p scope)
      (wit-world-uses scope)
      (wit-interface-uses scope)))

(defun %scope-resources (scope)
  "NAME -> resource node for a scope. A WORLD has no dedicated
resource table: its `(resource $r ...)' ITEMS are folded on demand."
  (if (wit-world-p scope)
      (let ((table (make-hash-table :test #'equal)))
        (dolist (item (wit-world-items scope) table)
          (when (and (eq (wit-world-item-kind item) :resource)
                     (wit-world-item-name item))
            (setf (gethash (wit-world-item-name item) table)
                  (or (wit-world-item-target item)
                      (make-wit-resource
                       :name (wit-world-item-name item)))))))
      (wit-interface-resources scope)))

(defun %lookup-resource (scope name packages &optional (seen '()))
  "Resolve NAME as a RESOURCE in SCOPE: its own resources first, then
through `use' imports (recursively — a used interface may itself only
RE-EXPORT the resource, e.g. wasi:io/streams re-exporting
wasi:io/error). Returns (values resource-node declaring-scope)."
  (when (member (list (%scope-kind scope) (%scope-name scope) name)
                seen :test #'equal)
    (return-from %lookup-resource (values nil nil)))
  (let ((own (gethash name (%scope-resources scope))))
    (if own
        (values own scope)
        (dolist (use (%scope-uses scope) (values nil nil))
          (let ((source (%use-source-name use name)))
            (when source
              (let ((target (%used-interface use scope packages)))
                (when target
                  (multiple-value-bind (node tgt)
                      (%lookup-resource target source packages
                                        (cons (list (%scope-kind scope)
                                                    (%scope-name scope) name)
                                              seen))
                    (when node (return (values node tgt))))))))))))

(defun %scope-resource-cts (scope)
  (if (wit-world-p scope)
      (wit-world-resource-cts scope)
      (wit-interface-resource-cts scope)))

(defun %used-interface (use scope packages)
  (if (null (wit-use-package use))
      ;; same package
      (package-interface (%scope-package scope)
                         (wit-use-interface use))
      (let ((pkg (%find-package (wit-use-namespace use)
                                (wit-use-package use)
                                (wit-use-version use)
                                packages)))
        (and pkg (package-interface pkg (wit-use-interface use))))))

(defun %use-source-name (use name)
  "The SOURCE name a `use' imports under the local alias NAME, or NIL.
A plain import binds its own name; `a as b' binds B locally to A."
  (or (car (find name (wit-use-renames use) :key #'cdr :test #'string=))
      (and (member name (wit-use-items use) :test #'string=) name)))

(defun %lookup-type (scope name packages)
  "Resolve NAME in SCOPE (a wit-interface or a wit-world): (values
type-node declaring-scope). Follows `use' imports, including their
`as' aliases; NIL when unknown."
  (let ((ty (gethash name (%scope-types scope))))
    (if ty
        (values ty scope)
        (dolist (use (%scope-uses scope) (values nil nil))
          (let ((source (%use-source-name use name)))
            (when source
              (let ((target (%used-interface use scope packages)))
                (when target
                  (let ((hit (gethash source (wit-interface-types target))))
                    (when hit (return (values hit target))))))))))))

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
             :message (format nil "unknown WIT type ~a in ~a ~a"
                              name (%scope-kind scope) (%scope-name scope))))
    (values (wit-type-expr ty) target)))

(defun %resource-ct (scope name)
  "The single ct object naming the resource NAME in SCOPE (M16 slice
3: nominal identity per declaring scope, so every WIT occurrence of
the resource — and every method's self borrow — shares one handle
type identity)."
  (let ((h (%scope-resource-cts scope)))
    (or (gethash name h)
        (setf (gethash name h)
              (wasmdown.component:make-ct :kind :resource
                                          :payload name)))))

(defun %resolve-resource-ct (scope expr packages seen)
  "Resolve the payload of `own<...>' / `borrow<...>' to the declaring
resource's ct. EXPR is a string type name or an already-typed resource
expression; a name that does not resolve, or that resolves to a
non-resource, is a `wit-error' (never a raw string payload)."
  (cond
    ((stringp expr)
     (when (member expr seen :test #'string=)
       (error 'wit-error
              :message (format nil "recursive WIT type reference ~a" expr)))
     (multiple-value-bind (res target) (%lookup-resource scope expr packages)
       (unless res
         (when (nth-value 0 (%lookup-type scope expr packages))
           (error 'wit-error
                  :message (format nil "`~a` in ~a ~a does not name a resource"
                                   expr (%scope-kind scope)
                                   (%scope-name scope))))
         (error 'wit-error
                :message (format nil "unknown resource type `~a` in ~a ~a"
                                 expr (%scope-kind scope)
                                 (%scope-name scope))))
       ;; a re-exported resource keeps the DECLARING scope's ct identity
       (%resource-ct target (wit-resource-name res))))
    ((and (consp expr) (eq (car expr) :resource))
     (%resource-ct scope (second expr)))
    (t
     (error 'wit-error
            :message (format nil "~s is not a resource reference" expr)))))

(defun wit-type->ct (interface expr &key packages)
  "Convert the WIT type EXPR (resolved in INTERFACE) to a
wasmdown.component ct. Named references are resolved recursively.
A resource reference resolves to its interface's ONE resource ct,
and `own'/'borrow' of a resource carry that ct (not a bare name) so
the runtime's handle table has a stable type identity."
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
                 (if (and (consp inner) (eq (car inner) :resource))
                     (%resource-ct target (second inner))
                     (conv inner target
                           (cons (second expr) seen)))))
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
               ;; the payload is the resource's ct (M16 slice 3) — the
               ;; handle table's type identity. A string name goes
               ;; through the scope's type resolution; an unknown name
               ;; or a non-resource type is a WIT error, never a raw
               ;; string payload.
               (wasmdown.component:make-ct
                :kind (car expr)
                :payload (%resolve-resource-ct scope (second expr) packages
                                               seen)))
              (:resource
               (%resource-ct scope (second expr)))
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

;;; ---------------------------------------------------------------
;;; Version selection (plan step 3).
;;;
;;; The reader above records @since / @unstable / @deprecated
;;; annotations but does not interpret them: every package is the full
;;; declared surface. This section turns those annotations into a
;;; selectable contract. A `wit-selection' carries the target SemVer
;;; per package plus the enabled @unstable feature names; gating keeps
;;; an item when its @since version is <= the target and (for
;;; @unstable) its feature is enabled. Deprecated items are always
;;; kept. Nothing here filters members of a record/variant/enum/flags:
;;; dropping one would silently change the layout, so an excluded
;;; member is a `wit-error' instead.
;;;
;;; The strict semver syntax check is the component layer's
;;; %SEMVER-PARSE (src/component.lisp, same BNF); only the ordering
;;; relation lives here.

(defstruct (wit-version (:constructor %make-wit-version
                                      (major minor patch prerelease build
                                             source)))
  (major 0 :type integer)
  (minor 0 :type integer)
  (patch 0 :type integer)
  (prerelease nil)                       ; list of identifier strings
  (build nil)                            ; list of identifier strings
  (source "" :type string))

(defun %split-dotted (string)
  (loop with start = 0
        for i from 0 to (length string)
        when (or (= i (length string)) (char= (char string i) #\.))
          collect (prog1 (subseq string start i)
                    (setf start (1+ i)))))

(defun parse-wit-version (version)
  "Parse VERSION (a string `a.b.c[-pre][+build]') into a wit-version.
Signals `wit-error' when it is not a strict SemVer 2.0.0 version (the
component layer's parser supplies the syntax check)."
  (unless (stringp version)
    (error 'wit-error
           :message (format nil "invalid WIT version ~s: not a string"
                            version)))
  (let ((detail (wasmdown.component::%semver-parse version)))
    (when detail
      (error 'wit-error
             :message (format nil "invalid WIT version ~s: ~a"
                              version detail))))
  (let* ((plus (position #\+ version))
         (without-build (if plus (subseq version 0 plus) version))
         (build (and plus (subseq version (1+ plus))))
         (dash (position #\- without-build))
         (core (if dash (subseq without-build 0 dash) without-build))
         (prerelease (and dash (subseq without-build (1+ dash))))
         (numbers (mapcar #'parse-integer (%split-dotted core))))
    (%make-wit-version (first numbers) (second numbers) (third numbers)
                       (and prerelease (%split-dotted prerelease))
                       (and build (%split-dotted build))
                       version)))

(defun %ensure-wit-version (v)
  (cond ((wit-version-p v) v)
        ((stringp v) (parse-wit-version v))
        (t (error 'wit-error
                  :message (format nil "invalid WIT version designator ~s" v)))))

(defun wit-version-string (v)
  (let ((v (%ensure-wit-version v)))
    (wit-version-source v)))

(defun %compare-numeric-identifier (a b)
  "Compare two no-leading-zero digit strings numerically (length first,
so arbitrarily large identifiers work)."
  (cond ((< (length a) (length b)) -1)
        ((> (length a) (length b)) 1)
        ((string= a b) 0)
        ((string< a b) -1)
        (t 1)))

(defun %compare-prerelease-identifier (a b)
  "SemVer 2.0.0 11.4: numeric identifiers compare numerically and are
always lower than alphanumeric ones; alphanumeric ones compare in
ASCII lexical order."
  (let ((na (every #'digit-char-p a))
        (nb (every #'digit-char-p b)))
    (cond ((and na nb) (%compare-numeric-identifier a b))
          (na -1)
          (nb 1)
          ((string= a b) 0)
          ((string< a b) -1)
          (t 1))))

(defun %compare-prerelease (a b)
  "Compare two prerelease identifier lists (NIL = no prerelease)."
  (cond
    ((and (null a) (null b)) 0)
    ((null a) 1)                         ; a release outranks a prerelease
    ((null b) -1)
    (t (loop for x in a
             for y in b
             do (let ((c (%compare-prerelease-identifier x y)))
                  (when (/= c 0) (return c)))
             finally (return (cond ((< (length a) (length b)) -1)
                                   ((> (length a) (length b)) 1)
                                   (t 0)))))))

(defun wit-version-compare (a b)
  "Three-way compare of two version designators (strings or
wit-versions). Build metadata is not part of the order."
  (let* ((a (%ensure-wit-version a))
         (b (%ensure-wit-version b)))
    (flet ((cmp (x y) (cond ((< x y) -1) ((> x y) 1) (t 0))))
      (or (let ((c (cmp (wit-version-major a) (wit-version-major b))))
            (and (/= c 0) c))
          (let ((c (cmp (wit-version-minor a) (wit-version-minor b))))
            (and (/= c 0) c))
          (let ((c (cmp (wit-version-patch a) (wit-version-patch b))))
            (and (/= c 0) c))
          (%compare-prerelease (wit-version-prerelease a)
                               (wit-version-prerelease b))))))

(defun wit-version< (a b)
  (minusp (wit-version-compare a b)))

(defun wit-version= (a b)
  (zerop (wit-version-compare a b)))

(defun wit-version<= (a b)
  (not (plusp (wit-version-compare a b))))

;;; ---------------------------------------------------------------
;;; The selection record + annotation gates.

(defstruct wit-selection
  ;; package name (or basename) -> a version designator
  (target-versions nil)
  ;; the enabled @unstable feature names
  (enabled-features nil))

(defparameter *default-wit-selection* (make-wit-selection)
  "The selection with no version overrides and no enabled features: a
package's own declared version is its target.")

(defun %annotation-of (annotations name)
  (find name annotations :key #'wit-annotation-name :test #'string=))

(defun %annotation-arg (annotation key)
  (and annotation
       (cdr (assoc key (wit-annotation-args annotation) :test #'string=))))

(defun %gate-since (annotations)
  "The @since version string of ANNOTATIONS, or NIL."
  (%annotation-arg (%annotation-of annotations "since") "version"))

(defun %gate-feature (annotations)
  "The @unstable feature of ANNOTATIONS: a string, :unstable when the
annotation carries no feature argument, or NIL when not unstable."
  (let ((a (%annotation-of annotations "unstable")))
    (and a (or (%annotation-arg a "feature") :unstable))))

(defun %selection-target-version (package selection)
  "The selected target version of PACKAGE: the selection's override,
else the package's own declared version (a wit-version or NIL)."
  (let* ((selection (or selection *default-wit-selection*))
         (versions (wit-selection-target-versions selection))
         (hit (or (assoc (wit-package-name package) versions :test #'string=)
                  (assoc (wit-package-basename package) versions
                         :test #'string=)))
         (version (if hit (cdr hit) (wit-package-version package))))
    (and version (%ensure-wit-version version))))

(defun %gate-kept-fields-p (since feature selection target-version)
  "The field-level gate rule: @since is kept iff its version <=
TARGET-VERSION; @unstable is kept iff its feature is enabled.
@deprecated never removes an item, so it has no field here."
  (and (or (null since)
           (null target-version)
           (not (wit-version< target-version since)))
       (or (null feature)
           (member feature (wit-selection-enabled-features selection)
                   :test #'string=))))

(defun %gate-kept-p (annotations selection target-version)
  "Is an item carrying ANNOTATIONS part of the selected surface? See
%GATE-KEPT-FIELDS-P."
  (%gate-kept-fields-p (%gate-since annotations) (%gate-feature annotations)
                       selection target-version))

(defun %gate-at-least-as-strict-p (child parent)
  "May an item whose own gate is CHILD live inside a container gated
by PARENT? Availability and feature gates are independent dimensions:
an omitted dimension inherits the container's, and an explicit one may
not be weaker (earlier @since, or a different unstable feature)."
  (let ((c-since (%gate-since child))
        (p-since (%gate-since parent))
        (c-feature (%gate-feature child))
        (p-feature (%gate-feature parent)))
    (and (or (null c-since) (null p-since)
             (not (wit-version< c-since p-since)))
         (or (null c-feature) (null p-feature)
             (eq p-feature :unstable) (eq c-feature :unstable)
             (string= c-feature p-feature)))))

(defun %container-version (package annotations-list)
  "The nearest @since in ANNOTATIONS-LIST (innermost first), else the
package's declared version. A wit-version or NIL."
  (or (loop for annotations in annotations-list
            for since = (%gate-since annotations)
            when since return (%ensure-wit-version since))
      (and (wit-package-version package)
           (%ensure-wit-version (wit-package-version package)))))

;;; ---------------------------------------------------------------
;;; Item introspection shared by validation and selection.

(defun %item-annotations (item)
  (cond ((wit-type-p item) (wit-type-annotations item))
        ((wit-func-p item) (wit-func-annotations item))
        ((wit-resource-p item) (wit-resource-annotations item))
        ((wit-use-p item) (wit-use-annotations item))
        (t nil)))

(defun %item-binding-names (item)
  "The plain names ITEM binds in its interface scope."
  (cond ((wit-type-p item) (list (wit-type-name item)))
        ((wit-func-p item) (list (wit-func-name item)))
        ((wit-resource-p item) (list (wit-resource-name item)))
        ((wit-use-p item) (wit-use-items item))
        (t nil)))

(defun %item-display-name (item)
  (cond ((wit-type-p item) (wit-type-name item))
        ((wit-func-p item) (wit-func-name item))
        ((wit-resource-p item) (wit-resource-name item))
        ((wit-use-p item) (format nil "use ~a" (wit-use-interface item)))
        (t "?")))

(defun %collect-type-refs (expr)
  "Every named type reference in the type expression EXPR."
  (cond
    ((not (consp expr)) nil)
    (t
     (case (car expr)
       (:name (list (second expr)))
       ((:own :borrow)
        (if (stringp (second expr))
            (list (second expr))
            (%collect-type-refs (second expr))))
       ((:record :variant)
        (loop for entry in (cdr expr)
              append (%collect-type-refs (cdr entry))))
       ((:enum :flags) nil)
       ((:list :option :stream :future)
        (%collect-type-refs (second expr)))
       ((:result :map)
        (append (%collect-type-refs (second expr))
                (%collect-type-refs (third expr))))
       (:tuple
        (loop for e in (cdr expr) append (%collect-type-refs e)))
       (t
        ;; a bare sequence of type expressions (params, tuple-less lists)
        (loop for e in expr append (%collect-type-refs e)))))))

(defun %func-type-refs (func)
  (append (loop for (nil . te) in (wit-func-params func)
                append (%collect-type-refs te))
          (%collect-type-refs (wit-func-result func))))

(defun %item-type-refs (item)
  (cond
    ((wit-type-p item) (%collect-type-refs (wit-type-expr item)))
    ((wit-func-p item) (%func-type-refs item))
    ((wit-resource-p item)
     (loop for m in (wit-resource-methods item)
           append (%func-type-refs m)))
    ;; a use's imported names are checked against the target interface
    ;; separately; they are not references into this interface's scope
    (t nil)))

(defun %type-feature (type-node target-interface)
  "The @unstable feature guarding TYPE-NODE, including the gate of the
interface that declares it."
  (or (%gate-feature (wit-type-annotations type-node))
      (%gate-feature (wit-interface-annotations target-interface))))

;;; ---------------------------------------------------------------
;;; Validation.

(defun %check-gated-reference (feature ref-feature problem context ref)
  "An @unstable REF may only be used by an item gated at least as
strictly."
  (when ref-feature
    (cond
      ((null feature)
       (funcall problem
                "~a references @unstable `~a` but is not itself @unstable"
                context ref))
      ((and (not (eq feature :unstable))
            (not (eq ref-feature :unstable))
            (not (string= feature ref-feature)))
       (funcall problem
                "~a is gated by @unstable(feature = ~a) but references `~a` gated by feature ~a"
                context feature ref ref-feature)))))

(defun %validate-refs (iface refs feature problem context packages)
  (dolist (ref refs)
    (multiple-value-bind (ty target) (%lookup-type iface ref packages)
      (cond
        ((null ty)
         (funcall problem "unresolvable type reference `~a` in ~a of interface `~a`"
                  ref context (wit-interface-name iface)))
        (t
         (%check-gated-reference feature (%type-feature ty target)
                                 problem context ref))))))

(defun %validate-interface (iface package packages problem)
  (let ((dupes (make-hash-table :test #'equal))
        (container (wit-interface-annotations iface))
        (container-version (%container-version package (list (wit-interface-annotations iface))))
        (name (wit-interface-name iface)))
    ;; uses: the referenced package/interface must exist and every
    ;; imported name must be a type in it.
    (dolist (use (wit-interface-uses iface))
      (when (wit-use-package use)
        (unless (%find-package (wit-use-namespace use) (wit-use-package use)
                               (wit-use-version use) packages)
          (funcall problem "unknown package reference `~a:~a~@[@~a~]` in interface `~a`"
                   (or (wit-use-namespace use) "")
                   (wit-use-package use) (wit-use-version use) name)))
      (let ((target (%used-interface use iface packages)))
        (if (null target)
            (funcall problem "unresolvable interface reference `~a` in interface `~a`"
                     (wit-use-interface use) name)
            (dolist (item (wit-use-items use))
              (let ((ty (gethash item (wit-interface-types target))))
                (if (null ty)
                    (funcall problem "unresolvable type reference `~a` imported by interface `~a`"
                             item name)
                    (%check-gated-reference
                     (or (%gate-feature (wit-use-annotations use))
                         (%gate-feature container))
                     (%type-feature ty target)
                     problem (format nil "`use ~a` in interface `~a`"
                                     (wit-use-interface use) name)
                     item)))))))
    (dolist (item (wit-interface-items iface))
      (let ((annotations (%item-annotations item)))
        ;; duplicate plain-name bindings
        (dolist (binding (%item-binding-names item))
          (if (gethash binding dupes)
              (funcall problem "duplicate binding `~a` in interface `~a`"
                       binding name)
              (setf (gethash binding dupes) t)))
        ;; @since may not exceed the container's version
        (let ((since (%gate-since annotations)))
          (when (and since container-version
                     (wit-version< container-version (%ensure-wit-version since)))
            (funcall problem "@since(~a) on `~a` in interface `~a` exceeds its container's version ~a"
                     since (%item-display-name item) name
                     (wit-version-string container-version))))
        (unless (wit-use-p item)
          (let ((feature (or (%gate-feature annotations)
                             (%gate-feature container))))
            (cond
              ((wit-resource-p item)
               ;; methods inherit the resource's gate when they have none
               (let ((method-container (%container-version
                                        package
                                        (list (wit-resource-annotations item)
                                              (wit-interface-annotations iface))))
                     (resource-feature (or (%gate-feature (wit-resource-annotations item))
                                           (%gate-feature container))))
                 (dolist (method (wit-resource-methods item))
                   (let ((m-since (%gate-since (wit-func-annotations method))))
                     (when (and m-since method-container
                                (wit-version< method-container
                                              (%ensure-wit-version m-since)))
                       (funcall problem "@since(~a) on method `~a` of resource `~a` in interface `~a` exceeds its container's version ~a"
                                m-since (wit-func-name method)
                                (wit-resource-name item) name
                                (wit-version-string method-container))))
                   (%validate-refs iface (%func-type-refs method)
                                   (or (%gate-feature (wit-func-annotations method))
                                       resource-feature)
                                   problem
                                   (format nil "method `~a` of resource `~a`"
                                           (wit-func-name method)
                                           (wit-resource-name item))
                                   packages))))
              (t
               (%validate-refs iface (%item-type-refs item) feature
                               problem
                               (format nil "`~a`" (%item-display-name item))
                               packages)))))))))

(defun %world-item-reference (item)
  "The component-level binding reference of ITEM: the plain binding
name when it has one (a named interface binding, a func, a typedef),
else the full interface name of a bare path binding."
  (or (wit-world-item-name item)
      (if (wit-world-item-package item)
          (format nil "~a:~a/~a~@[@~a~]"
                  (or (wit-world-item-namespace item) "")
                  (wit-world-item-package item)
                  (wit-world-item-interface item)
                  (wit-world-item-version item))
          (wit-world-item-interface item))))

(defun wit-world-item-attributes (item)
  "The component import/export attributes of ITEM, in the exact
(kind . value) shape %ATTR-OF and %CHECK-NAME-ATTRS consume: the
`implements' value of a named interface binding and the positional
`@external-id'. Reusing the representation keeps a second
attribute vocabulary out of the WIT layer."
  (remove nil
          (list (and (wit-world-item-implements item)
                     (cons :implements (wit-world-item-implements item)))
                (and (wit-world-item-external-id item)
                     (cons :external-id (wit-world-item-external-id item))))))

(defun %validate-world (world package packages problem)
  (let ((container (wit-world-annotations world))
        (container-version (%container-version package (list (wit-world-annotations world))))
        (name (wit-world-name world)))
    (dolist (sort '(:import :export))
      (let ((seen (make-hash-table :test #'equal))
            (items (if (eq sort :import)
                       (wit-world-imports world)
                       (wit-world-exports world))))
        (dolist (item items)
          (let* ((key (%world-item-reference item))
                 (kind (wit-world-item-kind item))
                 (annotations (wit-world-item-annotations item)))
            (if (gethash key seen)
                (funcall problem "duplicate world binding `~a` in world `~a`"
                         key name)
                (setf (gethash key seen) t))
            (let ((since (%gate-since annotations)))
              (when (and since container-version
                         (wit-version< container-version
                                       (%ensure-wit-version since)))
                (funcall problem "@since(~a) on `~a ~a` in world `~a` exceeds its container's version ~a"
                         since (string-downcase (symbol-name sort)) key name
                         (wit-version-string container-version))))
            ;; only a path interface binding has a package/interface
            ;; reference to check; inline interfaces and world
            ;; functions are self-contained
            (when (eq kind :interface)
              (let ((pkg (if (wit-world-item-package item)
                             (%find-package (wit-world-item-namespace item)
                                            (wit-world-item-package item)
                                            (wit-world-item-version item)
                                            packages)
                             package)))
                (cond
                  ((null pkg)
                   (funcall problem "unknown package reference `~a` in world `~a`"
                            key name))
                  (t
                   (let ((target (package-interface
                                  pkg (wit-world-item-interface item))))
                     (if (null target)
                         (funcall problem "unresolvable interface reference `~a` in world `~a`"
                                  key name)
                         (%check-gated-reference
                          (or (%gate-feature annotations)
                              (%gate-feature container))
                          (%gate-feature (wit-interface-annotations target))
                          problem (format nil "world item `~a ~a`" sort key)
                          (wit-world-item-interface item))))))))))))))

(defun validate-wit-package (package &key packages)
  "Validate PACKAGE (with PACKAGES as the resolvable package set) and
return a list of problem strings; an empty list means valid. Checks
unknown package references, unresolvable type references, duplicate
plain-name bindings, @since gates that exceed their container's
version, and @unstable items referenced by items that are not gated at
least as strictly."
  (let ((problems '())
        (all (cons package packages)))
    (flet ((problem (control &rest args)
             (push (apply #'format nil control args) problems)))
      (dolist (iface (wit-package-interface-list package))
        (%validate-interface iface package all #'problem))
      (let ((worlds '()))
        (maphash (lambda (name world)
                   (declare (ignore name))
                   (push world worlds))
                 (wit-package-worlds package))
        (dolist (world (nreverse worlds))
          (%validate-world world package all #'problem))))
    (nreverse problems)))

;;; ---------------------------------------------------------------
;;; Selection.

(defun %check-member-gates (type selection target)
  "A record/variant/enum/flags member that falls outside the selection
would change the layout if dropped; refuse instead of dropping it."
  (dolist (entry (wit-type-member-annotations type))
    (unless (%gate-kept-p (cdr entry) selection target)
      (error 'wit-error
             :message
             (format nil "cannot exclude gated member `~a` of `~a`: dropping a member would change its layout"
                     (car entry) (wit-type-name type))))))

(defun %select-item (item selection target)
  (cond
    ((wit-type-p item)
     (%check-member-gates item selection target)
     item)
    ((wit-resource-p item)
     (let ((resource (copy-wit-resource item)))
       (setf (wit-resource-methods resource)
             (remove-if-not
              (lambda (method)
                (%gate-kept-p (wit-func-annotations method)
                              selection target))
              (wit-resource-methods item)))
       resource))
    (t item)))

(defun %select-interface (iface selection target)
  (let ((out (make-wit-interface
              :name (wit-interface-name iface)
              :package (wit-interface-package iface)
              :annotations (wit-interface-annotations iface)
              ;; sharing the memo keeps one resource ct identity per
              ;; resource name across the selected and original graph
              :resource-cts (wit-interface-resource-cts iface))))
    (dolist (item (wit-interface-items iface))
      (when (%gate-kept-p (%item-annotations item) selection target)
        (let ((item (%select-item item selection target)))
          (typecase item
            (wit-type
             (setf (gethash (wit-type-name item)
                            (wit-interface-types out))
                   item)
             (push item (wit-interface-items out)))
            (wit-func
             (setf (gethash (wit-func-name item)
                            (wit-interface-funcs out))
                   item)
             (push item (wit-interface-func-list out))
             (push item (wit-interface-items out)))
            (wit-resource
             (setf (gethash (wit-resource-name item)
                            (wit-interface-resources out))
                   item)
             (push item (wit-interface-resource-list out))
             (push item (wit-interface-items out)))
            (wit-use
             (push item (wit-interface-uses out))
             (push item (wit-interface-items out)))))))
    (setf (wit-interface-items out) (nreverse (wit-interface-items out))
          (wit-interface-func-list out) (nreverse (wit-interface-func-list out))
          (wit-interface-resource-list out)
          (nreverse (wit-interface-resource-list out))
          (wit-interface-uses out) (nreverse (wit-interface-uses out)))
    out))

(defun %world-source-entries (world)
  "The world's definitions in declaration order. A world read by this
reader always carries ITEMS; a world assembled by hand may not, so
fall back to the per-sort lists plus includes and uses."
  (or (wit-world-items world)
      (append (wit-world-includes world)
              (wit-world-uses world)
              (wit-world-imports world)
              (wit-world-exports world))))

(defun %select-world (world selection target)
  (let ((out (copy-wit-world world))
        (kept '()))
    (dolist (entry (%world-source-entries world))
      (let ((annotations (typecase entry
                           (wit-world-include
                            (wit-world-include-annotations entry))
                           (t (wit-world-item-annotations entry)))))
        (when (%gate-kept-p annotations selection target)
          (push entry kept))))
    (setf kept (nreverse kept))
    (setf (wit-world-items out) kept
          (wit-world-includes out)
          (remove-if-not #'wit-world-include-p kept)
          (wit-world-imports out)
          (remove-if-not (lambda (e)
                           (and (wit-world-item-p e)
                                (eq (wit-world-item-sort e) :import)))
                         kept)
          (wit-world-exports out)
          (remove-if-not (lambda (e)
                           (and (wit-world-item-p e)
                                (eq (wit-world-item-sort e) :export)))
                         kept)
          (wit-world-uses out)
          (loop for e in kept
                when (and (wit-world-item-p e)
                          (eq (wit-world-item-kind e) :use))
                  collect (wit-world-item-target e)))
    ;; the world's own typedefs gate too; resource cts keep the shared
    ;; memo so resource identity survives selection
    (let ((types (make-hash-table :test #'equal)))
      (maphash (lambda (name ty)
                 (when (%gate-kept-p (wit-type-annotations ty)
                                     selection target)
                   (setf (gethash name types) ty)))
               (wit-world-types world))
      (setf (wit-world-types out) types))
    out))

(defun select-wit-package (package selection &key packages)
  "The PACKAGE surface selected by SELECTION (default: the package's
declared version, no features): @since items newer than the target and
@unstable items whose feature is not enabled are removed from the item
lists, resources lose excluded methods, and worlds lose excluded
items. Deprecated items are kept. PACKAGES is accepted for symmetry
with VALIDATE-WIT-PACKAGE; selection is purely local to PACKAGE."
  (declare (ignore packages))
  (let* ((selection (or selection *default-wit-selection*))
         (target (%selection-target-version package selection))
         (out (make-wit-package
               :namespace (wit-package-namespace package)
               :basename (wit-package-basename package)
               :name (wit-package-name package)
               :version (wit-package-version package)
               :path (wit-package-path package)
               :annotations (wit-package-annotations package))))
    (dolist (iface (wit-package-interface-list package))
      (when (%gate-kept-p (wit-interface-annotations iface) selection target)
        (let ((selected (%select-interface iface selection target)))
          (setf (gethash (wit-interface-name selected)
                         (wit-package-interfaces out))
                selected)
          (push selected (wit-package-interface-list out)))))
    (setf (wit-package-interface-list out)
          (nreverse (wit-package-interface-list out)))
    (maphash (lambda (name world)
               (when (%gate-kept-p (wit-world-annotations world)
                                   selection target)
                 (setf (gethash name (wit-package-worlds out))
                       (%select-world world selection target))))
             (wit-package-worlds package))
    out))

(defun %apply-stability (package stable-only features)
  "The reader's opt-in stability gate. With STABLE-ONLY, PACKAGE's
declared surface is narrowed by SELECT-WIT-PACKAGE using FEATURES (the
enabled @unstable feature names); otherwise PACKAGE is returned
unchanged. Kept separate from parsing so the default AST is always the
full declared surface."
  (if stable-only
      (select-wit-package package
                          (make-wit-selection :enabled-features features))
      package))

;;; ---------------------------------------------------------------
;;; World resolution (plan step 3, parser half).
;;;
;;; The raw AST keeps includes, renames and positional external-ids
;;; verbatim; resolution builds a NEW world graph without touching it:
;;;
;;;  * includes expand in declaration order, so the union is the
;;;    textual order of definitions (`include' fills the position of
;;;    the include statement itself);
;;;  * a rename rewrites only an included item's plain binding LABEL —
;;;    never its interface target or a nested type name;
;;;  * every interface/type reference is resolved against the package
;;;    set: an unresolved package version, interface or type is a
;;;    `wit-error' naming the world and the binding;
;;;  * an identical bare interface binding arriving from an include is
;;;    de-duplicated (specs/WIT.md "De-duplication of interfaces"),
;;;    while a plain-name collision is an error;
;;;  * two named bindings of the same interface stay two distinct
;;;    bindings, and paths that alias ONE resource declaration yield
;;;    one resource ct (see %RESOURCE-CT's per-scope memo).

(defun %world-binding-name (item)
  "The plain label a rename may rewrite: the explicit `name:' prefix,
else a bare binding's final path segment."
  (or (wit-world-item-name item)
      (wit-world-item-interface item)))

(defun %resolve-world-gate-check (annotations container selection target
                                  world-name sort reference)
  "Inherit the world's gate for an item with no gate of its own;
reject an explicitly weaker one. Returns whether the item is kept."
  (when (and annotations container
             (not (%gate-at-least-as-strict-p annotations container)))
    (error 'wit-error
           :message
           (format nil "world `~a`: ~a `~a` carries a weaker gate than its world"
                   world-name sort reference)))
  (%gate-kept-fields-p (or (%gate-since annotations) (%gate-since container))
                       (or (%gate-feature annotations) (%gate-feature container))
                       selection target))

(defun %resolve-world-declared-target (package item all strict world-name)
  "Resolve a path binding's target: (values package interface).
PACKAGE and INTERFACE are NIL only for a non-strict resolve of a
target whose package is not supplied. The reference is exact-version:
there is no nearest-version fallback."
  (let ((ns (wit-world-item-namespace item))
        (pkgn (wit-world-item-package item))
        (iname (wit-world-item-interface item))
        (version (wit-world-item-version item))
        (ref (%world-item-reference item)))
    (cond
      ((or ns pkgn)
       (let ((pkg (%find-package ns pkgn version all)))
         (if (null pkg)
             (if strict
                 (error 'wit-error
                        :message
                        (format nil "world `~a`: unresolved package version `~a` in binding `~a`"
                                world-name
                                (or version "any")
                                ref))
                 (values nil nil))
             (let ((iface (package-interface pkg iname)))
               (unless iface
                 (error 'wit-error
                        :message
                        (format nil "world `~a`: unresolvable interface reference `~a`"
                                world-name ref)))
               (values pkg iface)))))
      (t
       (when (and version
                  (not (equal version (wit-package-version package))))
         (error 'wit-error
                :message
                (format nil "world `~a`: unresolved version `~a` in binding `~a`"
                        world-name version ref)))
       (let ((iface (package-interface package iname)))
         (unless iface
           (error 'wit-error
                  :message
                  (format nil "world `~a`: unresolvable interface reference `~a`"
                          world-name ref)))
         (values package iface))))))

(defun %check-world-type-expr (expr scope packages world-name what)
  "Resolve every named reference in EXPR against SCOPE. An `own' /
`borrow' name must resolve to a resource. Every error names the world
and WHAT."
  (labels
      ((ref (name resource-p)
         (multiple-value-bind (ty target) (%lookup-type scope name packages)
           (declare (ignore target))
           (unless ty
             (error 'wit-error
                    :message
                    (format nil "world `~a`: unresolvable type reference `~a` in ~a"
                            world-name name what)))
           (when resource-p
             (let ((e (wit-type-expr ty)))
               (unless (and (consp e) (eq (car e) :resource))
                 (error 'wit-error
                        :message
                        (format nil "world `~a`: `own'/`borrow' of `~a` in ~a does not name a resource"
                                world-name name what)))))
           ty))
       (walk (e)
         (cond
           ((not (consp e)) nil)
           (t
            (case (car e)
              (:name (ref (second e) nil))
              ((:own :borrow)
               (if (stringp (second e))
                   (ref (second e) t)
                   (walk (second e))))
              ((:list :option :stream :future) (walk (second e)))
              ((:result :map) (walk (second e)) (walk (third e)))
              (:tuple (dolist (x (cdr e)) (walk x)))
              ((:record :variant)
               (dolist (entry (cdr e)) (walk (cdr entry))))
              ((:enum :flags :resource) nil)
              (t (dolist (x (cdr e)) (walk x))))))))
    (walk expr)))

(defun %validate-world-use (world use all strict world-name)
  "A world-level `use' must name a reachable interface and import types
that interface actually declares."
  (let ((target (%used-interface use world all)))
    (cond
      ((null target)
       (when (or strict (null (wit-use-package use)))
         (error 'wit-error
                :message
                (format nil "world `~a`: unresolvable interface reference `~a` in `use`"
                        world-name (wit-use-interface use)))))
      (t
       (dolist (item (wit-use-items use))
         (unless (gethash item (wit-interface-types target))
           (error 'wit-error
                  :message
                  (format nil "world `~a`: unresolvable type reference `~a` imported by `use`"
                          world-name item))))))))

(defun %world-include-reference (package include)
  "The textual reference of an include target."
  (destructuring-bind (ns pkg iname version)
      (wit-world-include-target include)
    (format nil "~@[~a:~]~a/~a~@[@~a~]"
            ns (or pkg (wit-package-name package)) iname version)))

(defun %world-include-target-world (package include all strict world-name)
  "The world an include names (a wit-world), or NIL when a non-strict
resolve cannot reach its package."
  (destructuring-bind (ns pkg iname version)
      (wit-world-include-target include)
    (let* ((target-package (if (or ns pkg)
                               (%find-package ns pkg version all)
                               package))
           (ref (%world-include-reference package include)))
      (cond
        ((null target-package)
         (if strict
             (error 'wit-error
                    :message
                    (format nil "world `~a`: include target `~a` names an unknown package version"
                            world-name ref))
             nil))
        (t
         (or (gethash iname (wit-package-worlds target-package))
             (error 'wit-error
                    :message
                    (format nil "world `~a`: include target `~a` is not a world"
                            world-name ref))))))))

(defun %rename-world-binding (item local package)
  "A renamed copy of ITEM whose plain label is LOCAL. A renamed BARE
path binding becomes a named interface binding, so it gains the
`implements' value the component layer records."
  (let ((copy (copy-wit-world-item item)))
    (setf (wit-world-item-name copy) local)
    (when (and (eq (wit-world-item-kind copy) :interface)
               (null (wit-world-item-implements copy)))
      (setf (wit-world-item-implements copy)
            (%interface-reference-string
             package
             (wit-world-item-namespace copy)
             (wit-world-item-package copy)
             (wit-world-item-interface copy)
             (wit-world-item-version copy))))
    copy))

(defun %apply-include-renames (items renames included-package
                               world-name included-name)
  "Rewrite only the plain binding labels RENAMES names. Returns
(values ITEMS RENAMED-ITEMS); every rename source must exist and name
exactly one plain binding."
  (let ((seen '()))
    (dolist (pair renames)
      (when (member (car pair) seen :test #'string=)
        (error 'wit-error
               :message
               (format nil "world `~a`: include of world `~a`: rename source `~a` is listed more than once"
                       world-name included-name (car pair))))
      (push (car pair) seen)))
  (let ((renamed '()))
    (dolist (pair renames)
      (let* ((source (car pair))
             (hits (remove-if-not
                    (lambda (item)
                      (string= source (%world-binding-name item)))
                    items)))
        (cond
          ((null hits)
           (error 'wit-error
                  :message
                  (format nil "world `~a`: include of world `~a`: rename source `~a` does not exist"
                          world-name included-name source)))
          ((rest hits)
           (error 'wit-error
                  :message
                  (format nil "world `~a`: include of world `~a`: rename source `~a` is ambiguous"
                          world-name included-name source)))))
      (let* ((source (car pair))
             (hit (find source items :key #'%world-binding-name
                                     :test #'string=))
             (copy (%rename-world-binding hit (cdr pair) included-package)))
        (push (cons hit copy) renamed)))
    (values (loop for item in items
                  for pair = (assoc item renamed)
                  collect (if pair (cdr pair) item))
            (mapcar #'cdr renamed))))

(defun %resolve-world (package world all strict selection stack)
  "Resolve WORLD (declared in PACKAGE) into a fresh world. ALL is the
package set, STRICT says every external reference must resolve, and
STACK is the include chain used for cycle detection."
  (let* ((name (wit-world-name world))
         (container (wit-world-annotations world))
         (target (%selection-target-version package selection))
         (entries (%world-source-entries world))
         (out (make-wit-world :name name
                              :package (wit-world-package world)
                              :annotations (wit-world-annotations world)))
         (bindings (list (cons :import nil) (cons :export nil)))
         (imports '())
         (exports '())
         (out-items '())
         (out-includes '()))
    (flet ((kept-p (annotations sort reference)
             (%resolve-world-gate-check annotations container selection
                                        target name sort reference))
           (node-type (entry)
             (let ((node (wit-world-item-target entry)))
               (if (eq (wit-world-item-kind entry) :resource)
                   (or (gethash (wit-resource-name node)
                                (wit-world-types world))
                       node)
                   node)))
           (add (item explicit-p origin renamed-p)
             "Add ITEM to OUT, de-duplicating identical bare interface
bindings that an include introduced and rejecting every other
collision."
             (let* ((sort (wit-world-item-sort item))
                    (ref (%world-item-reference item))
                    (bucket (cdr (assoc sort bindings)))
                    (existing (find ref bucket
                                    :key (lambda (e)
                                           (%world-item-reference
                                            (getf e :item)))
                                    :test #'string=)))
               (cond
                 ((null existing)
                  (push (list :item item :explicit explicit-p
                              :origin origin)
                        bucket)
                  (setf (cdr (assoc sort bindings)) bucket)
                  (push item out-items)
                  (ecase sort
                    (:import (push item imports))
                    (:export (push item exports))))
                 ((and (eq (wit-world-item-kind item) :interface)
                       (eq (wit-world-item-kind (getf existing :item))
                           :interface)
                       (null (wit-world-item-name item))
                       (null (wit-world-item-name
                              (getf existing :item)))
                       (let ((a (wit-world-item-target item))
                             (b (wit-world-item-target
                                 (getf existing :item))))
                         (or (and a b (eq a b))
                             (string= ref (%world-item-reference
                                           (getf existing :item)))))
                       (or (not explicit-p)
                           (not (getf existing :explicit))))
                  ;; specs/WIT.md "De-duplication of interfaces": two
                  ;; included worlds sharing an interface name keep one
                  ;; copy. Explicit duplicates of a world's own items
                  ;; stay errors.
                  nil)
                 (renamed-p
                  (error 'wit-error
                         :message
                         (format nil "world `~a`: include of world `~a`: renamed binding `~a` collides with another binding"
                                 name (or origin "?") ref)))
                 (t
                  (error 'wit-error
                         :message
                         (format nil "world `~a`: duplicate binding `~a`~@[ (from include of world `~a`)~]"
                                 name ref origin)))))))
      ;; -- pass 1: the world-level type scope ------------------------
      (dolist (entry entries)
        (when (and (wit-world-item-p entry)
                   (member (wit-world-item-kind entry)
                           '(:use :type :resource)))
          (when (kept-p (wit-world-item-annotations entry)
                        (string-downcase
                         (symbol-name (wit-world-item-kind entry)))
                        (%world-item-reference entry))
            (if (eq (wit-world-item-kind entry) :use)
                (progn
                  (%validate-world-use out (wit-world-item-target entry)
                                       all strict name)
                  (push (wit-world-item-target entry)
                        (wit-world-uses out)))
                (let ((ty (node-type entry)))
                  (when ty
                    (setf (gethash (wit-type-name ty)
                                   (wit-world-types out))
                          ty)))))))
      (setf (wit-world-uses out) (nreverse (wit-world-uses out)))
      ;; -- pass 1b: type references, now that the scope is whole ----
      (dolist (entry entries)
        (when (and (wit-world-item-p entry)
                   (member (wit-world-item-kind entry)
                           '(:type :resource :func)))
          (when (kept-p (wit-world-item-annotations entry)
                        (string-downcase
                         (symbol-name (wit-world-item-kind entry)))
                        (%world-item-reference entry))
            (let ((node (wit-world-item-target entry)))
              (cond
                ((eq (wit-world-item-kind entry) :func)
                 (dolist (param (wit-func-params node))
                   (%check-world-type-expr
                    (cdr param) out all name
                    (format nil "function `~a'" (wit-func-name node))))
                 (when (wit-func-result node)
                   (%check-world-type-expr
                    (wit-func-result node) out all name
                    (format nil "function `~a'" (wit-func-name node)))))
                ((eq (wit-world-item-kind entry) :resource)
                 (dolist (method (wit-resource-methods node))
                   (dolist (param (wit-func-params method))
                     (%check-world-type-expr
                      (cdr param) out all name
                      (format nil "method `~a' of resource `~a'"
                              (wit-func-name method)
                              (wit-resource-name node))))
                   (when (wit-func-result method)
                     (%check-world-type-expr
                      (wit-func-result method) out all name
                      (format nil "method `~a' of resource `~a'"
                              (wit-func-name method)
                              (wit-resource-name node))))))
                (t
                 (%check-world-type-expr
                  (wit-type-expr node) out all name
                  (format nil "type `~a'" (wit-type-name node)))))))))
      ;; -- pass 2: includes and extern bindings, declaration order --
      (dolist (entry entries)
        (typecase entry
          (wit-world-include
           (let ((ref (%world-include-reference package entry)))
             (when (kept-p (wit-world-include-annotations entry)
                           "include" ref)
               (let ((included (%world-include-target-world
                                package entry all strict name)))
                 (when included
                   (when (member included stack :test #'eq)
                     (error 'wit-error
                            :message
                            (format nil "world `~a`: include cycle: ~{~a~^ -> ~}"
                                    name
                                    (mapcar #'wit-world-name
                                            (reverse (cons included
                                                           (cons world
                                                                 stack)))))))
                   (let* ((included-package (wit-world-package included))
                          (resolved (%resolve-world included-package included
                                                    all strict selection
                                                    (cons world stack)))
                          (items (remove-if-not
                                  (lambda (it)
                                    (and (wit-world-item-p it)
                                         (member (wit-world-item-sort it)
                                                 '(:import :export))))
                                  (wit-world-items resolved))))
                     (multiple-value-bind (renamed renamed-set)
                         (%apply-include-renames
                          items (wit-world-include-renames entry)
                          included-package name (wit-world-name included))
                       (dolist (item renamed)
                         (add item nil (wit-world-name included)
                              (member item renamed-set :test #'eq)))))))))
           (push entry out-includes))
          (wit-world-item
           (let ((sort (wit-world-item-sort entry)))
             (cond
               ((member sort '(:import :export))
                (when (kept-p (wit-world-item-annotations entry)
                              (string-downcase (symbol-name sort))
                              (%world-item-reference entry))
                  (add (%resolve-world-item package entry all strict name)
                       t nil nil)))
               ((member (wit-world-item-kind entry) '(:use :type :resource))
                (when (kept-p (wit-world-item-annotations entry)
                              (string-downcase
                               (symbol-name (wit-world-item-kind entry)))
                              (%world-item-reference entry))
                  (push (copy-wit-world-item entry) out-items))))))))
      (setf (wit-world-items out) (nreverse out-items)
            (wit-world-imports out) (nreverse imports)
            (wit-world-exports out) (nreverse exports)
            (wit-world-includes out) (nreverse out-includes))
      out)))

(defun %resolve-world-item (package item all strict world-name)
  "A resolved copy of ITEM: a path binding gains its target interface."
  (let ((copy (copy-wit-world-item item)))
    (when (eq (wit-world-item-kind item) :interface)
      (multiple-value-bind (target-package iface)
          (%resolve-world-declared-target package copy all strict world-name)
        (declare (ignore target-package))
        (when iface
          (setf (wit-world-item-target copy) iface))))
    copy))

(defun resolve-wit-world (package name &key packages selection)
  "Resolve world NAME in PACKAGE under SELECTION: gate the world's
definitions with the world's own annotations inherited, expand every
`include' in declaration order (applying its `with { a as b }'
renames to the included world's plain bindings), and resolve each
interface/type reference against PACKAGE and PACKAGES.

The raw AST is never mutated; the result is a fresh world whose
imports/exports carry resolved targets, `implements' and
external-id values. A same-package reference is always checked; a
reference into another package is checked when PACKAGES is supplied
(then an unresolved package version, interface or type is a
`wit-error' naming the world and the binding), and otherwise left
unresolved for a later link step."
  (let ((world (gethash name (wit-package-worlds package))))
    (unless world
      (error 'wit-error
             :message (format nil "unknown world ~s in package ~a"
                              name (wit-package-name package))))
    (%resolve-world package world (cons package packages)
                    (and packages t)
                    (or selection *default-wit-selection*)
                    nil)))
