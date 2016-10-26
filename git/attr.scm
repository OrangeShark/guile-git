
(define-module (git attr)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types))

;;; attr

(define attr-add-macro
  (let ((proc (libgit2->procedure* "git_attr_add_macro" '(* * *))))
    (lambda (repository name values)
      (proc (repository->pointer repository)
	    (string->pointer name)
	    (string->pointer values)))))

(define attr-cache-flush
  (let ((proc (libgit2->procedure void "git_attr_cache_flush" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_foreach

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_get

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_get_many

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_value
