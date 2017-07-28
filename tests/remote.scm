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
      (remote-name remote)))

  (test-equal "remote lookup, not found"
    (list GIT_ENOTFOUND GITERR_CONFIG)
    (catch 'git-error
      (lambda ()
        (let ((repository (repository-open directory)))
          (clear-git-error!)
          (remote-lookup repository "does-not-exist")))
      (lambda (key err)
        (list (git-error-code err) (git-error-class err))))))

(libgit2-shutdown!)

(test-end)
