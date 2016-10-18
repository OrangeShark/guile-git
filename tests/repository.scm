(define-module (tests repository))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "repository")

(libgit2-init!)

(with-directory "tmp"
  (test-equal "repository-init"
    #t
    (let ((repository (repository-init "tmp"))
          (out (path-exists? "tmp/.git")))
      out)))

(with-directory "tmp"
  (test-equal "repository-init bare"
    #t
    (let* ((repository (repository-init "tmp" #t))
           (out (repository-is-bare? repository)))
      out)))

(with-repository "empty-repo"

  (test-equal "repository-is-empty?"
    #f
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (empty? (repository-is-empty? repository)))
      empty?))

  (test-equal "repository-is-bare?"
    #f
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (bare? (repository-is-bare? repository)))
      bare?))
  (test-equal "repository-is-shallow?"
    #f
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (shallow? (repository-is-shallow? repository)))
      shallow?))

  (test-equal "repository-path"
    (string-append (getcwd) "/tmp/empty-repo/.git/")
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (out (repository-path repository)))
      out))

  )

(libgit2-shutdown)

(test-end)
