(define-module (tests remote)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git)
             (git object))

(test-begin "remote")

(libgit2-init!)

(with-repository "simple-bare" directory

  (test-equal "remote lookup & name"
    "origin"
    (let* ((repository (repository-open directory))
           (remote (remote-lookup repository "origin")))
      (remote-name remote))))

(libgit2-shutdown!)

(test-end)
