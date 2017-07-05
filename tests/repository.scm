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
           (out (repository-is-bare? repository)))
      out)))

(with-repository "simple" directory

  (test-equal "repository-is-empty?"
    #f
    (let* ((repository (repository-open directory))
           (empty? (repository-is-empty? repository)))
      empty?))

  (test-equal "repository-is-bare?"
    #f
    (let* ((repository (repository-open directory))
           (bare? (repository-is-bare? repository)))
      bare?))
  (test-equal "repository-is-shallow?"
    #f
    (let* ((repository (repository-open directory))
           (shallow? (repository-is-shallow? repository)))
      shallow?))

  (test-equal "repository-path"
    (canonicalize-path (string-append directory "/.git"))
    (let* ((repository (repository-open directory))
           (out (repository-path repository)))
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
           (bare? (repository-is-bare? repository)))
      bare?)))

(libgit2-shutdown!)

(test-end)
