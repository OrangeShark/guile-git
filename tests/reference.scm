(define-module (tests reference))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "reference")

(libgit2-init!)

(with-repository "empty-repo"

  (test-equal "reference-name"
    "refs/heads/master"
    (let* ((repository (repository-open "tmp/empty-repo/")))
      (reference-name (repository-head repository))))

  (test-equal "reference-target"
    "4a53b71060aa9b11ecf4050d2e7379456f0fbfb1"
    (let* ((repository (repository-open "tmp/empty-repo/")))
      (oid->string (reference-target (repository-head repository)))))

  (test-equal "reference-name->oid"
    "4a53b71060aa9b11ecf4050d2e7379456f0fbfb1"
    (let* ((repository (repository-open "tmp/empty-repo/")))
      (oid->string (reference-name->oid repository "refs/heads/master"))))

  (test-equal "reference-shorthand"
    "master"
    (let* ((repository (repository-open "tmp/empty-repo/")))
      (reference-shorthand (repository-head repository))))

  )

(libgit2-shutdown)

(test-end)
