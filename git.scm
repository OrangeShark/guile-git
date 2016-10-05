(define-module (git))

(use-modules (system foreign))


(define* (dynamic-link* #:optional library-name)
   (let ((shared-object (if library-name (dynamic-link library-name)
(dynamic-link))))
     (lambda (return-value function-name . arguments)
       (let ((function (dynamic-func function-name shared-object)))
         (pointer->procedure return-value function arguments)))))

(define libgit2 (dynamic-link* "/gnu/store/6nh2s1siw6vxlzg4zgnf349gpyipwn9f-libgit2-0.24.1/lib/libgit2.so"))

(define (check code)
  (unless (eq? code 0)
    (throw 'libgit2 code)))

(define git-init
  (let ((f (libgit2 int "git_libgit2_init")))
    (lambda ()
      (f))))

(define git-repository-open
  (let ((f (libgit2 int "git_repository_open" '* '*)))
    (lambda (path)
      (let* ((pointer (u64vector 0))
	     (out (bytevector->pointer pointer))
             ;; convert arguments to c types
	     (path (string->pointer path)))
	(check (f out path))
	pointer))))

(git-init)
(pk 'output (git-repository-open "/home/amirouche/src/guile/wiredtiger/"))
	    
