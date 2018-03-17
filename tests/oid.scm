(define-module (tests oid)
  #:use-module (srfi srfi-64)
  #:use-module (tests helpers)
  #:use-module (git)
  #:use-module (git object))

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
           (not (oid=? (commit-id head^) oid)))))

  (test-assert "object-lookup"
    (let* ((repository (repository-open directory))
           (oid        (reference-target (repository-head repository)))
           (short      (string->oid (string-take (oid->string oid) 7)))
           (obj1       (object-lookup repository oid))
           (obj2       (object-lookup-prefix repository short 7)))
      (and obj1
           (eq? obj1 obj2)
           (eqv? OBJ-COMMIT (object-type obj1))
           (oid=? oid (object-id obj1))))))

(test-end)
