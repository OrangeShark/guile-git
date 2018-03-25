(define-module (tests repository)
  #:use-module (git)
  #:use-module (tests helpers)
  #:use-module ((ice-9 ftw) #:select (scandir))
  #:use-module (srfi srfi-64))


(define %top-srcdir
  (canonicalize-path (dirname (search-path %load-path "git.scm"))))

(define %top-git-directory
  (string-append %top-srcdir "/.git"))

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
        (list (git-error-code err) (git-error-class err)))))

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
      (string-trim-right path #\/)))

  (unless (and (string-suffix? "-linux-gnu" %host-type)
               (file-exists? %top-git-directory))
    (test-skip 1))
  (test-assert "repository-close!"
    ;; Make sure 'repository-close!' closes file descriptors associated with
    ;; the repo.  For this test, we need a big enough repo, where libgit2
    ;; typically keeps open file descriptors for the 'pack' files.
    (let* ((fd-before  (length (scandir "/proc/self/fd")))
           (repository (repository-open %top-srcdir))
           (commits    (fold-commits (lambda (commit count)
                                       (+ 1 count))
                                     0
                                     repository))
           (fd-after   (length (scandir "/proc/self/fd"))))
      (pk 'file-descriptors fd-before fd-after)
      (repository-close! repository)
      (<= (pk 'after-close (length (scandir "/proc/self/fd")))
          fd-before))))

(with-repository "simple-bare" directory

  (test-equal "repository-is-bare?"
    #t
    (let* ((repository (repository-open directory))
           (bare? (repository-bare? repository)))
      bare?)))

(libgit2-shutdown!)

(test-end)
