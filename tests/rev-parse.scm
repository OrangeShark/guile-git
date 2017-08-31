(define-module (tests rev-parse)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git)
             (git object))

(test-begin "rev-parse")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "revparse-single"
    #t
    (let* ((repository (repository-open directory))
           (master (reference-target (repository-head repository)))
           (other (object-id (revparse-single repository "master"))))
      (oid=? master other))))

(libgit2-shutdown!)

(test-end)
