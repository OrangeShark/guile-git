;;; Copyright © Ludovic Courtès <ludo@gnu.org>
;;; Released under the GNU GPL version 3 or later.

(define-module (guix git)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:export (repository?
            open-repository
            reference?
            repository-head
            reference-target
            oid?
            commit-signature))

;; DRAFT!

(define libgit2
  (dynamic-link "/gnu/store/g8r0qwnzf2j17hd84cchc6cmr51sflz8-libgit2-0.24.1/lib/libgit2.so"))

(define (libgit2->procedure return name params)
  (pointer->procedure return (dynamic-func name libgit2) params))

(define-inlinable (libgit2->procedure* name params)
  (let ((proc (libgit2->procedure int name params)))
    (lambda args
      (let ((ret (apply proc args)))
        (unless (zero? ret)
          (throw 'git-error ret))))))

(define initialize!
  (libgit2->procedure int "git_libgit2_init" '()))

(define-syntax define-libgit2-type
  (lambda (s)
    "Define a wrapped pointer type for an opaque type of libgit2."
    (syntax-case s ()
      ((_ name)
       (let ((symbol     (syntax->datum #'name))
             (identifier (lambda (symbol)
                           (datum->syntax #'name symbol))))
         (with-syntax ((rtd    (identifier (symbol-append '< symbol '>)))
                       (pred   (identifier (symbol-append symbol '?)))
                       (wrap   (identifier (symbol-append 'pointer-> symbol)))
                       (unwrap (identifier (symbol-append symbol '->pointer))))
           #`(define-wrapped-pointer-type rtd
               pred
               wrap unwrap
               (lambda (obj port)
                 (format port "#<git-~a ~a>"
                         #,(symbol->string symbol)
                         (number->string (pointer-address (unwrap obj))
                                         16))))))))))

(define-libgit2-type repository)

(define open-repository
  (let ((proc (libgit2->procedure* "git_repository_open" '(* *))))
    (lambda (file)
      (let ((result (bytevector->pointer (make-bytevector (sizeof '*)))))
        (proc result (string->pointer file))
        (pointer->repository (dereference-pointer result))))))

(define-libgit2-type reference)

(define repository-head
  (let ((proc (libgit2->procedure* "git_repository_head" '(* *))))
    (lambda (repository)
      (let ((result (bytevector->pointer (make-bytevector (sizeof '*)))))
        (proc result (repository->pointer repository))
        (pointer->reference (dereference-pointer result))))))

(define-libgit2-type oid)

(define reference-target
  (let ((proc (libgit2->procedure '* "git_reference_target" '(*))))
    (lambda (reference)
      (pointer->oid (proc (reference->pointer reference))))))

(define-libgit2-type commit)

(define lookup-commit
  (let ((proc (libgit2->procedure* "git_commit_lookup" `(* * *))))
    (lambda (repository oid)
      (let ((result (bytevector->pointer (make-bytevector (sizeof '*)))))
        (proc result (repository->pointer repository) (oid->pointer oid))
        (pointer->commit (dereference-pointer result))))))

(define commit-raw-header
  (let ((proc (libgit2->procedure '* "git_commit_raw_header" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define %buffer-struct                            ;git_buf
  (list '* size_t size_t))

(define free-buffer
  (libgit2->procedure void "git_buf_free" '(*)))

(define (buffer-content buf)
  (match (parse-c-struct buf %buffer-struct)
    ((pointer asize size)
     (pointer->bytevector pointer size))))

(define (buffer-content/string buf)
  (match (parse-c-struct buf %buffer-struct)
    ((pointer asize size)
     (pointer->string pointer size "UTF-8"))))

(define commit-signature
  (let ((proc (libgit2->procedure* "git_commit_extract_signature"
                                   '(* * * * *))))
    (lambda* (repository oid #:optional (field "gpgsig"))
      (let ((signature (make-c-struct %buffer-struct
                                      `(,%null-pointer 0 0)))
            (data      (make-c-struct %buffer-struct
                                      `(,%null-pointer 0 0))))
        (proc signature data (repository->pointer repository)
              (oid->pointer oid)
              (string->pointer field))
        (let ((signature* (buffer-content/string signature))
              (data*      (buffer-content/string data)))
          (free-buffer signature)
          (free-buffer data)
          (values signature* data*))))))


(define-libgit2-type object)

(define GIT_OBJ_ANY -2)

(define lookup-object
  (let ((proc (libgit2->procedure* "git_object_lookup" `(* * * ,int))))
    (lambda* (repository oid #:optional (type GIT_OBJ_ANY))
      (let ((result (bytevector->pointer (make-bytevector (sizeof '*)))))
        (proc result (repository->pointer repository) (oid->pointer oid)
              type)
        (pointer->object (dereference-pointer result))))))

(initialize!)
