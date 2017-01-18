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

(with-repository "simple"

  (test-equal "repository-is-empty?"
    #f
    (let* ((repository (repository-open "tmp/simple/"))
           (empty? (repository-is-empty? repository)))
      empty?))

  (test-equal "repository-is-bare?"
    #f
    (let* ((repository (repository-open "tmp/simple/"))
           (bare? (repository-is-bare? repository)))
      bare?))
  (test-equal "repository-is-shallow?"
    #f
    (let* ((repository (repository-open "tmp/simple/"))
           (shallow? (repository-is-shallow? repository)))
      shallow?))

  (test-equal "repository-path"
    (string-append (getcwd) "/tmp/simple/.git/")
    (let* ((repository (repository-open "tmp/simple/"))
           (out (repository-path repository)))
      out))

  (test-equal "repository-discover"
    (string-append (getcwd) "/tmp/simple/.git/")
    (let ((path (repository-discover "tmp/simple/directory/")))
      path)))

(with-repository "simple-bare"

  (test-equal "repository-is-bare?"
    #t
    (let* ((repository (repository-open "tmp/simple-bare/"))
           (bare? (repository-is-bare? repository)))
      bare?)))

(libgit2-shutdown)

(test-end)
