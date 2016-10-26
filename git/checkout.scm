
(define-module (git checkout)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types))


;;; checkout https://libgit2.github.com/libgit2/#HEAD/group/checkout

(define checkout-head
  (let ((proc (libgit2->procedure* "git_checkout_head" '(* *))))
    (lambda (repository options)
      (proc (repository->pointer repository) %null-pointer))))

(define checkout-index
  (let ((proc (libgit2->procedure* "git_checkout_index" '(* * *))))
    (lambda (repository index options)
      (proc (repository->pointer repository)
	    (index->pointer index)
	    %null-pointer))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/checkout/git_checkout_init_options

(define checkout-tree
  (let ((proc (libgit2->procedure* "git_checkout_tree" `(* * *))))
    (lambda (repository treeish)
      (proc (repository->pointer repository)
	    (object->pointer treeish)
	    %null-pointer))))
