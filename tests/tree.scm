(define-module (tests tree))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "tree")

(libgit2-init!)

(with-repository "simple"

  (test-equal "commit-tree tree-id"
    "f68558a3c807fc657528a466b4f051596e7ca182"
    (let* ((repository (repository-open "tmp/simple/"))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (oid->string (tree-id tree))))

  (test-equal "tree-walk list files"
    (list "README")
    (let* ((repository (repository-open "tmp/simple"))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (let ((files '()))
        (tree-walk tree GIT-TREEWALK-PRE (lambda (root entry)
                                           (set! files (cons (tree-entry-name entry) files))
                                           0))
        files)))

  )


(libgit2-shutdown)

(test-end)
