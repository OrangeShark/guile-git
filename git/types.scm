
(define-module (git types)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (annotated-commit? pointer->annotated-commit  annotated-commit->pointer
            blame? pointer->blame blame->pointer
            blame-options? pointer->blame-options blame-options->pointer
            blob? pointer->blob blob->pointer
            branch-iterator? pointer->branch-iterator branch-iterator->pointer
            checkout-options? pointer->checkout-options branch-iterator->pointer
            commit? pointer->commit commit->pointer
            config? pointer->config config->pointer
            cred? pointer->cred cred->pointer
            diff? pointer->diff diff->pointer
            diff-delta? pointer->diff-delta diff-delta->pointer
            diff-options? pointer->diff-options diff-options->pointer
            index? pointer->index index->pointer
            object? pointer->object object->pointer
            oid? pointer->oid oid->pointer
            patch? pointer->patch patch->pointer
            refdb? pointer->refdb refdb->pointer
            reference? pointer->reference reference->pointer
            repository? pointer->repository repository->pointer
            signature? pointer->signature signature->pointer
            tree? pointer->tree tree->pointer))


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

(define-libgit2-type annotated-commit)
(define-libgit2-type blame)
(define-libgit2-type blame-options)
(define-libgit2-type blob)
(define-libgit2-type branch-iterator)
(define-libgit2-type checkout-options)
(define-libgit2-type commit)
(define-libgit2-type config)
(define-libgit2-type cred)
(define-libgit2-type diff)
(define-libgit2-type diff-delta)
(define-libgit2-type diff-options)
(define-libgit2-type index)
(define-libgit2-type object)
(define-libgit2-type oid)
(define-libgit2-type patch)
(define-libgit2-type refdb)
(define-libgit2-type reference)
(define-libgit2-type repository)
(define-libgit2-type signature)
(define-libgit2-type tree)
