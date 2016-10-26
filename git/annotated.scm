
(define-module (git annotated)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types))

;;; annotated

(define annotated-commit-free
  (let ((proc (libgit2->procedure void "git_annotated_commit_free" '(*))))
    (lambda (commit)
      (proc (annotated-commit->pointer commit)))))

(define annotated-commit-from-fetchhead
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_fetchhead" '(* * * * *))))
    (lambda (repository branch-name remote-url id)
      (let ((out (make-double-pointer)))
	(proc out
	      (repository->pointer repository)
	      (string->pointer branch-name)
	      (string->pointer remote-url)
	      (oid->pointer id))
	(pointer->annotated-commit (dereference-pointer out))))))

(define annotated-commit-from-ref
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_ref" '(* * *))))
    (lambda (repository reference)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) (reference->pointer reference))
	(pointer->annotated-commit (dereference-pointer out))))))

(define annotated-commit-from-revspec
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_revspec" '(* * *))))
    (lambda (repository revspec)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) (string->pointer revspec))))))

(define annotated-commit-id
  (let ((proc (libgit2->procedure '* "git_annotated_commit_id" '(*))))
    (lambda (commit)
      (pointer->oid (proc (annotated-commit->pointer commit))))))

(define annotated-commit-lookup
  (let ((proc (libgit2->procedure* "git_annotated_commit_lookup" '(* * *))))
    (lambda (repository id)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) (oid->pointer id))
	(pointer->annotated-commit (dereference-pointer out))))))
