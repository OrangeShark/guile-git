(define-module (tests oid)
  #:use-module (srfi srfi-64)
  #:use-module (tests helpers)
  #:use-module (git))

(test-begin "oid")

(libgit2-init!)

(with-repository "simple" directory

  (test-assert "oid=?"
    (let* ((repository (repository-open directory))
           (oid        (reference-target (repository-head repository)))
           (head       (commit-lookup repository oid))
           (head^      (commit-parent head)))
      (and (oid=? oid oid)
           (oid=? oid (reference-target (repository-head repository)))
           (not (oid=? (commit-id head^) oid))))))

(test-end)
