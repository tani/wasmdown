;;;; M16 slice 1: the WIT reader's pins against the vendored WASI
;;;; 0.3.2 snapshot (specs/wasi/wasi-wit-0.3.2/, SHA-256 pinned in
;;;; specs/wasi/MANIFEST.md). Every package must parse; the assertions
;;;; below pin the surface the providers build on.

(defpackage #:wasmdown.test.wit
  (:use #:cl #:wasmdown.test))

(in-package #:wasmdown.test.wit)

(defmacro %source-file ()
  (or *compile-file-truename* *load-truename* *default-pathname-defaults*))

(defparameter *wit-root*
  (merge-pathnames #p"../specs/wasi/wasi-wit-0.3.2/" (%source-file)))

(defun wit-file (relative)
  (merge-pathnames relative *wit-root*))

(defun parse-pkg (relative)
  (wasmdown.wit:parse-wit-file (wit-file relative)))

(defun iface (pkg name)
  (wasmdown.wit:package-interface pkg name))

(defun func (interface name)
  (gethash name (wasmdown.wit:wit-interface-funcs interface)))

(defun type-node (interface name)
  (gethash name (wasmdown.wit:wit-interface-types interface)))

(defun annotation (annotations name)
  (find name annotations :key #'wasmdown.wit:wit-annotation-name
                        :test #'string=))

(defun ct-kind-of (interface expr)
  (wasmdown.component:ct-kind
   (wasmdown.wit:wit-type->ct interface expr)))

(defun func-ct (interface name)
  (wasmdown.wit:wit-func->ct interface (func interface name)))

;;; ---------------------------------------------------------------

(test wit/all-six-vendored-packages-parse
  ;; The acceptance gate: the reader walks the whole vendored tree.
  (dolist (spec '(("clocks/clocks.wit" "clocks")
                  ("random/random.wit" "random")
                  ("cli/cli.wit" "cli")
                  ("filesystem/filesystem.wit" "filesystem")
                  ("sockets/sockets.wit" "sockets")
                  ("http/http.wit" "http")))
    (let ((pkg (parse-pkg (first spec))))
      (is (wasmdown.wit:wit-package-p pkg))
      (is (string= (wasmdown.wit:wit-package-basename pkg) (second spec)))
      (is (string= (wasmdown.wit:wit-package-namespace pkg) "wasi"))
      (is (string= (wasmdown.wit:wit-package-version pkg) "0.3.2"))
      (is (plusp (hash-table-count
                  (wasmdown.wit:wit-package-interfaces pkg))))
      (is (plusp (hash-table-count
                  (wasmdown.wit:wit-package-worlds pkg)))))))

(test wit/clocks-package
  (let* ((pkg (parse-pkg "clocks/clocks.wit"))
         (types (iface pkg "types"))
         (mono (iface pkg "monotonic-clock"))
         (sys (iface pkg "system-clock"))
         (tz (iface pkg "timezone")))
    (is (string= (wasmdown.wit:wit-package-name pkg) "wasi:clocks"))
    ;; types.duration = u64
    (is (eq (wasmdown.wit:wit-type-expr (type-node types "duration")) :u64))
    ;; monotonic-clock: now/get-resolution sync, wait-until/wait-for async
    (is (null (wasmdown.wit:wit-func-params (func mono "now"))))
    (is (equal (wasmdown.wit:wit-func-result (func mono "now")) '(:name "mark")))
    (is (equal (wasmdown.wit:wit-func-result (func mono "get-resolution"))
               '(:name "duration")))
    (is (not (wasmdown.wit:wit-func-async-p (func mono "now"))))
    (is (wasmdown.wit:wit-func-async-p (func mono "wait-until")))
    (is (wasmdown.wit:wit-func-async-p (func mono "wait-for")))
    (is (equal (wasmdown.wit:wit-func-params (func mono "wait-until"))
               '(("when" . (:name "mark")))))
    ;; the use: use types.{duration}
    (let ((use (first (wasmdown.wit:wit-interface-uses mono))))
      (is (equal (wasmdown.wit:wit-use-items use) '("duration")))
      (is (string= (wasmdown.wit:wit-use-interface use) "types"))
      (is (null (wasmdown.wit:wit-use-package use))))
    ;; system-clock.instant record
    (let ((instant (wasmdown.wit:wit-type-expr (type-node sys "instant"))))
      (is (eq (car instant) :record))
      (is (equal (second instant) '("seconds" . :s64)))
      (is (equal (third instant) '("nanoseconds" . :u32))))
    ;; the @unstable feature gate is recorded, not rejected
    (let ((ann (annotation (wasmdown.wit:wit-interface-annotations tz)
                           "unstable")))
      (is ann)
      (is (equal (cdr (assoc "feature" (wasmdown.wit:wit-annotation-args ann)
                             :test #'string=))
                 "clocks-timezone")))
    ;; the world's imports include the versioned interface paths
    (let* ((world (gethash "imports"
                           (wasmdown.wit:wit-package-worlds pkg)))
           (names (mapcar #'wasmdown.wit:wit-world-item-interface
                          (wasmdown.wit:wit-world-imports world))))
      (is (member "monotonic-clock" names :test #'string=))
      (is (member "system-clock" names :test #'string=)))))

(test wit/random-package
  (let* ((pkg (parse-pkg "random/random.wit"))
         (random (iface pkg "random"))
         (insecure (iface pkg "insecure"))
         (seed (iface pkg "insecure-seed")))
    (is (= 3 (hash-table-count
              (wasmdown.wit:wit-package-interfaces pkg))))
    (is (equal (wasmdown.wit:wit-func-params (func random "get-random-bytes"))
               '(("max-len" . :u64))))
    (is (equal (wasmdown.wit:wit-func-result (func random "get-random-bytes"))
               '(:list :u8)))
    (is (eq (wasmdown.wit:wit-func-result (func random "get-random-u64")) :u64))
    (is (equal (wasmdown.wit:wit-func-result
                (func insecure "get-insecure-random-bytes"))
               '(:list :u8)))
    (is (equal (wasmdown.wit:wit-func-result
                (func seed "get-insecure-seed"))
               '(:tuple :u64 :u64)))))

(test wit/cli-package
  (let* ((pkg (parse-pkg "cli/cli.wit"))
         (env (iface pkg "environment"))
         (exit (iface pkg "exit"))
         (run (iface pkg "run"))
         (types (iface pkg "types"))
         (stdin (iface pkg "stdin"))
         (command (gethash "command"
                           (wasmdown.wit:wit-package-worlds pkg))))
    (is (equal (wasmdown.wit:wit-func-result
                (func env "get-environment"))
               '(:list (:tuple :string :string))))
    (is (equal (wasmdown.wit:wit-func-result (func env "get-arguments"))
               '(:list :string)))
    (is (equal (wasmdown.wit:wit-func-result (func env "get-initial-cwd"))
               '(:option :string)))
    ;; exit: func(status: result) -- the status is the PARAMETER
    (is (null (wasmdown.wit:wit-func-result (func exit "exit"))))
    (is (equal (wasmdown.wit:wit-func-params (func exit "exit"))
               '(("status" . (:result nil nil)))))
    (is (equal (wasmdown.wit:wit-func-params (func exit "exit-with-code"))
               '(("status-code" . :u8))))
    (is (wasmdown.wit:wit-func-async-p (func run "run")))
    ;; enum error-code { io, illegal-byte-sequence, pipe }
    (let ((expr (wasmdown.wit:wit-type-expr (type-node types "error-code"))))
      (is (eq (car expr) :enum))
      (is (equal (cdr expr) '("io" "illegal-byte-sequence" "pipe"))))
    ;; stdin's tuple<stream<u8>, future<result<_, error-code>>>
    (let ((res (wasmdown.wit:wit-func-result
                (func stdin "read-via-stream"))))
      (is (eq (car res) :tuple))
      (is (eq (car (first (cdr res))) :stream))
      (is (eq (car (second (cdr res))) :future)))
    ;; the command world exports run and imports the versioned deps
    (is (equal (mapcar #'wasmdown.wit:wit-world-item-interface
                       (wasmdown.wit:wit-world-exports command))
               '("run")))
    (let ((mono (find "monotonic-clock"
                      (wasmdown.wit:wit-world-imports command)
                      :key #'wasmdown.wit:wit-world-item-interface
                      :test #'string=)))
      (is mono)
      (is (string= (wasmdown.wit:wit-world-item-version mono) "0.3.2"))
      (is (string= (wasmdown.wit:wit-world-item-package mono) "clocks")))))

(test wit/filesystem-package
  (let* ((pkg (parse-pkg "filesystem/filesystem.wit"))
         (types (iface pkg "types"))
         (preopens (iface pkg "preopens")))
    ;; use wasi:clocks/system-clock@0.3.2.{instant}
    (let ((use (first (wasmdown.wit:wit-interface-uses types))))
      (is (string= (wasmdown.wit:wit-use-interface use) "system-clock"))
      (is (string= (wasmdown.wit:wit-use-package use) "clocks"))
      (is (string= (wasmdown.wit:wit-use-namespace use) "wasi"))
      (is (string= (wasmdown.wit:wit-use-version use) "0.3.2"))
      (is (equal (wasmdown.wit:wit-use-items use) '("instant"))))
    ;; variant error-code's catch-all case carries a payload
    (let* ((expr (wasmdown.wit:wit-type-expr (type-node types "error-code")))
           (other (assoc "other" (cdr expr) :test #'string=)))
      (is (eq (car expr) :variant))
      (is other)
      (is (equal (cdr other) '(:option :string))))
    ;; flags descriptor-flags
    (let ((expr (wasmdown.wit:wit-type-expr
                 (type-node types "descriptor-flags"))))
      (is (eq (car expr) :flags))
      (is (= 6 (length (cdr expr))))
      (is (string= (second expr) "read")))
    ;; resource descriptor with its methods -- including an async one
    (let ((res (gethash "descriptor"
                        (wasmdown.wit:wit-interface-resources types))))
      (is (wasmdown.wit:wit-resource-p res))
      (is (> (length (wasmdown.wit:wit-resource-methods res)) 20))
      (let ((advise (find "advise" (wasmdown.wit:wit-resource-methods res)
                          :key #'wasmdown.wit:wit-func-name
                          :test #'string=)))
        (is advise)
        (is (wasmdown.wit:wit-func-async-p advise))))
    ;; preopens.get-directories -> list<tuple<descriptor, string>>
    (is (equal (wasmdown.wit:wit-func-result
                (func preopens "get-directories"))
               '(:list (:tuple (:name "descriptor") :string))))))

(test wit/sockets-package
  (let* ((pkg (parse-pkg "sockets/sockets.wit"))
         (types (iface pkg "types"))
         (lookup (iface pkg "ip-name-lookup"))
         (tcp (gethash "tcp-socket"
                       (wasmdown.wit:wit-interface-resources types)))
         (udp (gethash "udp-socket"
                       (wasmdown.wit:wit-interface-resources types))))
    (is (string= (wasmdown.wit:wit-use-package
                  (first (wasmdown.wit:wit-interface-uses types)))
                 "clocks"))
    ;; static func on a resource
    (let ((create (find "create" (wasmdown.wit:wit-resource-methods tcp)
                        :key #'wasmdown.wit:wit-func-name
                        :test #'string=)))
      (is create)
      (is (wasmdown.wit:wit-func-static-p create))
      (is (equal (wasmdown.wit:wit-func-result create)
                 '(:result (:name "tcp-socket") (:name "error-code")))))
    (is (> (length (wasmdown.wit:wit-resource-methods udp)) 10))
    ;; type ipv4-address = tuple<u8, u8, u8, u8>
    (is (equal (wasmdown.wit:wit-type-expr
                (type-node types "ipv4-address"))
               '(:tuple :u8 :u8 :u8 :u8)))
    ;; variant ip-address with two payload cases
    (let ((expr (wasmdown.wit:wit-type-expr (type-node types "ip-address"))))
      (is (eq (car expr) :variant))
      (is (equal (mapcar #'car (cdr expr)) '("ipv4" "ipv6"))))
    (is (wasmdown.wit:wit-func-async-p
         (func lookup "resolve-addresses")))))

(test wit/http-package
  (let* ((pkg (parse-pkg "http/http.wit"))
         (types (iface pkg "types"))
         (client (iface pkg "client"))
         (fields (gethash "fields"
                          (wasmdown.wit:wit-interface-resources types)))
         (request (gethash "request"
                           (wasmdown.wit:wit-interface-resources types)))
         (service (gethash "service"
                           (wasmdown.wit:wit-package-worlds pkg))))
    ;; constructor() and static funcs on resources
    (is (find-if #'wasmdown.wit:wit-func-constructor-p
                 (wasmdown.wit:wit-resource-methods fields)))
    (let ((from-list (find "from-list"
                           (wasmdown.wit:wit-resource-methods fields)
                           :key #'wasmdown.wit:wit-func-name
                           :test #'string=)))
      (is from-list)
      (is (wasmdown.wit:wit-func-static-p from-list)))
    ;; request.new is static and returns a tuple<request, future<...>>
    (let ((new (find "new" (wasmdown.wit:wit-resource-methods request)
                     :key #'wasmdown.wit:wit-func-name
                     :test #'string=)))
      (is new)
      (is (wasmdown.wit:wit-func-static-p new))
      (is (eq (car (wasmdown.wit:wit-func-result new)) :tuple)))
    ;; variant method has uppercase labels + an `other(string)` payload
    (let ((expr (wasmdown.wit:wit-type-expr (type-node types "method"))))
      (is (member "get" (cdr expr) :key #'car :test #'string=))
      (is (eq (cdr (assoc "other" (cdr expr) :test #'string=))
              :string)))
    ;; type alias field-name = string
    (is (eq (wasmdown.wit:wit-type-expr (type-node types "field-name"))
            :string))
    ;; world service exports handler
    (is (equal (mapcar #'wasmdown.wit:wit-world-item-interface
                       (wasmdown.wit:wit-world-exports service))
               '("handler")))
    (is (wasmdown.wit:wit-func-async-p (func client "send")))))

(test wit/type-conversion-to-ct
  (let* ((pkg (parse-pkg "clocks/clocks.wit"))
         (types (iface pkg "types"))
         (mono (iface pkg "monotonic-clock"))
         (sys (iface pkg "system-clock")))
    ;; duration = u64 -> (:prim :u64)
    (let ((ct (wasmdown.wit:wit-type->ct
               types (wasmdown.wit:wit-type-expr (type-node types "duration")))))
      (is (eq (wasmdown.component:ct-kind ct) :prim))
      (is (eq (wasmdown.component:ct-payload ct) :u64)))
    ;; the named reference resolves through the use import
    (let ((ct (wasmdown.wit:wit-func->ct mono (func mono "get-resolution")
                                       :packages (list pkg))))
      (is (eq (wasmdown.component:ct-kind ct) :func))
      (is (eq (wasmdown.component:ct-kind
               (second (wasmdown.component:ct-payload ct)))
              :prim))
      (is (eq (wasmdown.component:ct-payload
               (second (wasmdown.component:ct-payload ct)))
              :u64)))
    ;; instant -> a record ct with the two fields
    (let* ((ct (wasmdown.wit:wit-func->ct sys (func sys "now")
                                     :packages (list pkg)))
           (rt (second (wasmdown.component:ct-payload ct))))
      (is (eq (wasmdown.component:ct-kind rt) :record))
      (let ((fields (wasmdown.component:ct-payload rt)))
        (is (= 2 (length fields)))
        (is (string= (car (aref fields 0)) "seconds"))
        (is (eq (wasmdown.component:ct-payload (cdr (aref fields 0))) :s64))
        (is (eq (wasmdown.component:ct-payload (cdr (aref fields 1))) :u32))))
    ;; an async functype keeps its async flag in slot 3
    (let ((ct (wasmdown.wit:wit-func->ct mono (func mono "wait-until")
                                       :packages (list pkg))))
      (is (third (wasmdown.component:ct-payload ct))))
    ;; list<u8>, tuple<u64,u64>, option<string>, result<_, E>
    (let* ((rpkg (parse-pkg "random/random.wit"))
           (random (iface rpkg "random"))
           (res (second (wasmdown.component:ct-payload
                         (wasmdown.wit:wit-func->ct
                          random (func random "get-random-bytes")
                          :packages (list rpkg))))))
      (is (eq (wasmdown.component:ct-kind res) :list))
      (is (eq (wasmdown.component:ct-kind (car (wasmdown.component:ct-payload res)))
              :prim)))
    (let* ((rpkg (parse-pkg "random/random.wit"))
           (seed (iface rpkg "insecure-seed"))
           (res (second (wasmdown.component:ct-payload
                         (wasmdown.wit:wit-func->ct
                          seed (func seed "get-insecure-seed")
                          :packages (list rpkg))))))
      (is (eq (wasmdown.component:ct-kind res) :tuple))
      (is (= 2 (length (wasmdown.component:ct-payload res)))))
    (let* ((cpkg (parse-pkg "cli/cli.wit"))
           (env (iface cpkg "environment"))
           (res (second (wasmdown.component:ct-payload
                         (wasmdown.wit:wit-func->ct
                          env (func env "get-initial-cwd")
                          :packages (list cpkg))))))
      (is (eq (wasmdown.component:ct-kind res) :option))
      (is (eq (wasmdown.component:ct-kind
               (wasmdown.component:ct-payload res))
              :prim))
      (is (eq (wasmdown.component:ct-payload
               (wasmdown.component:ct-payload res))
              :string)))
    ;; interface import names carry the version suffix
    (is (string= (wasmdown.wit:interface-import-name mono)
                 "wasi:clocks/monotonic-clock@0.3.2"))))
