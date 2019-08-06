;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guile-Git.
;;;
;;; Guile-Git is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Guile-Git is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guile-Git.  If not, see <http://www.gnu.org/licenses/>.

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
          fd-before)))

  (test-equal "openable-repository?, does not exist"
    #f
    (openable-repository? "/does/not/exist"))

  (test-assert "openable-repository?"
    (openable-repository? directory)))

(with-repository "simple-bare" directory

  (test-equal "repository-is-bare?"
    #t
    (let* ((repository (repository-open directory))
           (bare? (repository-bare? repository)))
      bare?)))

(libgit2-shutdown!)

(test-end)
