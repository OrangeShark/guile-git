(define-module (tests tree))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "tree")

(libgit2-init!)

(with-repository "simple"

  (test-equal "commit-tree tree-id"
    "d40674e05d114e5eb0df0f358ebeec47b8782ced"
    (let* ((repository (repository-open "tmp/simple/"))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (oid->string (tree-id tree))))

  (test-equal "tree-walk list files"
    (list "message" "directory" "README")
    (let* ((repository (repository-open "tmp/simple"))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (let ((files '()))
        (tree-walk tree TREEWALK-PRE (lambda (root entry)
                                           (set! files (cons (tree-entry-name entry) files))
                                           0))
        files)))

  (test-equal "tree-list"
    (list "directory/message" "directory" "README")
    (let* ((repository (repository-open "tmp/simple"))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (tree-list tree)))

  (test-equal "tree-entry-bypath"
    "message"
    (let* ((repository (repository-open "tmp/simple"))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (tree-entry-name (tree-entry-bypath tree "directory/message"))))
  )


(libgit2-shutdown)

(test-end)
