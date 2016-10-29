(define-module (git reference)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (reference-name
            reference-target
            reference-name->oid
            reference-shorthand))


;;; FIXME: reference https://libgit2.github.com/libgit2/#HEAD/group/reference

(define reference-name
  (let ((proc (libgit2->procedure '* "git_reference_name" '(*))))
    (lambda (reference)
      (pointer->string (proc (reference->pointer reference))))))

(define reference-target
  (let ((proc (libgit2->procedure '* "git_reference_target" '(*))))
    (lambda (reference)
      (pointer->oid (proc (reference->pointer reference))))))

(define reference-name->oid
  (let ((proc (libgit2->procedure* "git_reference_name_to_id" '(* * *))))
    (lambda (repository name)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer name))
        (pointer->oid out)))))

(define reference-shorthand
  (let ((proc (libgit2->procedure '* "git_reference_shorthand" '(*))))
    (lambda (reference)
      (pointer->string (proc (reference->pointer reference))))))
