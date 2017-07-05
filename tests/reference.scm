(define-module (tests reference)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "reference")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "reference-name"
    "refs/heads/master"
    (let* ((repository (repository-open directory)))
      (reference-name (repository-head repository))))

  (test-equal "reference-target"
    "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
    (let* ((repository (repository-open directory)))
      (oid->string (reference-target (repository-head repository)))))

  (test-equal "reference-name->oid"
    "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
    (let* ((repository (repository-open directory)))
      (oid->string (reference-name->oid repository "refs/heads/master"))))

  (test-equal "reference-shorthand"
    "master"
    (let* ((repository (repository-open directory)))
      (reference-shorthand (repository-head repository))))

  )

(libgit2-shutdown!)

(test-end)
