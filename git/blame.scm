
(define-module (git blame)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types))


;;; blame

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_buffer

(define blame-file
  (let ((proc (libgit2->procedure* "git_blame_file" '(* * * *))))
    (lambda (repository path options)
      (let ((out (make-double-pointer)))
	(proc out
	      (repository->pointer repository)
	      (string->pointer path)
	      (blame-options->pointer options))
	(pointer->blame (dereference-pointer out))))))

(define blame-free
  (let ((proc (libgit2->procedure void "git_blame_free" '(*))))
    (lambda (blame)
      (proc (blame->pointer blame)))))

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_byindex

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_byline

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_count

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_init_options
