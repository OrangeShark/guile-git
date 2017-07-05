(define-module (tests branch)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "branch")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "branch-list"
    (list "master")
    (let* ((repository (repository-open directory)))
      (map branch-name (branch-list repository))))

  (test-equal "branch-lookup"
    #t
    (let* ((repository (repository-open directory))
           (master (reference-target (repository-head repository)))
           (other (reference-target (branch-lookup repository "master"))))
      (apply equal? (map oid->string (list master other)))))

  (test-equal "branch-name"
    "master"
    (let* ((repository (repository-open directory))
           (master (repository-head repository)))
      (branch-name master)))

  )


(libgit2-shutdown!)

(test-end)
