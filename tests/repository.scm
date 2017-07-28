(define-module (tests repository)
  #:use-module (srfi srfi-64))

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
           (out (repository-bare? repository)))
      out)))

(with-repository "simple" directory

  (test-equal "repository-empty?"
    #f
    (let* ((repository (repository-open directory))
           (empty? (repository-empty? repository)))
      empty?))

  (test-equal "repository-open, non-existent file"
    (list GIT_ENOTFOUND GITERR_OS)
    (catch 'git-error
      (lambda ()
        (clear-git-error!)
        (repository-open "/does/not/exist"))
      (lambda (key err)
        (let ((last (last-git-error)))
          (list err (git-error-class last))))))

  (test-equal "repository-bare?"
    #f
    (let* ((repository (repository-open directory))
           (bare? (repository-bare? repository)))
      bare?))
  (test-equal "repository-shallow?"
    #f
    (let* ((repository (repository-open directory))
           (shallow? (repository-shallow? repository)))
      shallow?))

  (test-equal "repository-directory"
    (canonicalize-path (string-append directory "/.git"))
    (let* ((repository (repository-open directory))
           (out (repository-directory repository)))
      (string-trim-right out #\/)))

  (test-equal "repository-discover"
    (canonicalize-path (string-append directory "/.git"))
    (let ((path (repository-discover
                 (string-append directory "/directory"))))
      (string-trim-right path #\/))))

(with-repository "simple-bare" directory

  (test-equal "repository-is-bare?"
    #t
    (let* ((repository (repository-open directory))
           (bare? (repository-bare? repository)))
      bare?)))

(libgit2-shutdown!)

(test-end)
