
(define-module (git branch)
  #:use-module (ice-9 receive)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git enums)
  #:use-module (git reference)
  #:use-module (git types)
  #:export (branch-list
            branch-lookup
            branch-name))

;;; branch https://libgit2.github.com/libgit2/#HEAD/group/branch

(define branch-create
  (let ((proc (libgit2->procedure* "git_branch_create" `(* * * * ,int))))
    (lambda (repository branch-name target force)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer branch-name)
              (commit->pointer target)
              (if force 1 0))
        (pointer->reference (dereference-pointer out))))))

(define branch-create-from-annotated
  (let ((proc (libgit2->procedure* "git_branch_create_from_annotated" `(* * * * ,int))))
    (lambda (repository branch-name commit force)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer branch-name)
              (annotated-commit->pointer commit)
              (if force 1 0))
        (pointer->reference (dereference-pointer out))))))

(define branch-delete
  (let ((proc (libgit2->procedure* "git_branch_delete" '(*))))
    (lambda (branch)
      (proc (reference->pointer branch)))))

(define branch-is-head?
  (let ((proc (libgit2->procedure int "git_branch_is_head" '(*))))
    (lambda (branch)
      (case (proc (reference->pointer branch))
        ((0) #f)
        ((1) #t)
        (else => (lambda (code) (throw 'git-error code)))))))

(define branch-iterator-free
  (let ((proc (libgit2->procedure void "git_branch_iterator_free" '(*))))
    (lambda (iterator)
      (proc (branch-iterator->pointer iterator)))))

(define branch-iterator-new
  (let ((proc (libgit2->procedure* "git_branch_iterator_new" `(* * ,int))))
    (lambda (repository flags)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) flags)
        (pointer->branch-iterator (dereference-pointer out))))))

(define branch-lookup
  (let ((proc (libgit2->procedure* "git_branch_lookup" `(* * * ,int))))
    (lambda* (repository branch-name #:optional (type GIT-BRANCH-ALL))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer branch-name)
              type)
        (pointer->reference (dereference-pointer out))))))

(define branch-move
  (let ((proc (libgit2->procedure* "git_branch_move" `(* * * ,int))))
    (lambda (reference new-branch-name force)
      (let ((out (make-double-pointer)))
        (proc out
              (reference->pointer reference)
              (string->pointer new-branch-name)
              (if force 1 0))
        (pointer->reference (dereference-pointer out))))))

(define branch-name
  (let ((proc (libgit2->procedure* "git_branch_name" '(* *))))
    (lambda (reference)
      (let ((out (make-double-pointer)))
        (proc out (reference->pointer reference))
        (pointer->string (dereference-pointer out))))))

(define branch-next
  (let ((proc (libgit2->procedure* "git_branch_next" '(* * *))))
    (lambda (iterator)
      (let ((out (make-double-pointer))
            (out-type (make-double-pointer)))
        (proc out out-type (branch-iterator->pointer iterator))
        (values (pointer->reference (dereference-pointer out))
                (pointer-address (dereference-pointer out-type)))))))

(define branch-set-upstream
  (let ((proc (libgit2->procedure* "git_branch_set_upstream" '(* *))))
    (lambda (branch upstream-name)
      (proc (reference->pointer branch) (string->pointer upstream-name)))))

(define branch-upstream
  (let ((proc (libgit2->procedure* "git_branch_upstream" '(* *))))
    (lambda (branch)
      (let ((out (make-double-pointer)))
        (proc out (reference->pointer branch))
        (pointer->reference (dereference-pointer out))))))

(define* (branch-list repository #:optional (flag GIT-BRANCH-ALL))
  (let ((iterator (branch-iterator-new repository flag)))
    (let ((lst (let loop ((lst '()))
                 (catch 'git-error
                   (lambda ()
                     (receive (reference _) (branch-next iterator)
                       (loop (cons (branch-name reference) lst))))
                   (lambda (key code)
                     (if (eq? code -31)
                         lst
                         (throw 'git-error code)))))))
      (branch-iterator-free iterator)
      lst)))
