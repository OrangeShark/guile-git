;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2019 Marius Bakke <marius@devup.no>
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

(define-module (tests describe)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git))

(test-begin "describe")

(libgit2-init!)

(with-repository "simple" directory

  ;; Create an annotated tag for the second commit.
  (test-assert "create annotated tag"
    (let* ((repository (repository-open directory))
           (name "0.1")
           (target (revparse-single repository "HEAD^"))
           (tagger (signature-default repository))
           (message "This tag is annotated!"))
      (oid->string (tag-create repository name
                               target tagger message))))

  (test-equal "describe HEAD with default options"
    "0.1-1-g3f848a1"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid)))
      (describe-format (describe-commit commit))))

  (test-equal "describe workdir with default options"
    "0.1-1-g3f848a1"
    (let* ((repository (repository-open directory)))
      (describe-format (describe-workdir repository))))

  ;; Create an un-annotated tag for the last commit.
  (test-equal "create lightweight tag"
    "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
    (let* ((repository (repository-open directory))
           (name "0.2-rc1")
           (target (revparse-single repository "HEAD")))
      (oid->string (tag-create-lightweight repository name target))))

  (test-equal "describe HEAD, strategy tags"
    "0.2-rc1"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (options (describe-init-options #:strategy 'tags)))
      (describe-format (describe-commit commit options))))

  (test-equal "describe workdir, strategy and pattern"
    "0.1-1-g3f848a1"
    (let ((repository (repository-open directory))
          (options (describe-init-options #:strategy 'all
                                          #:pattern "0.1")))
    (describe-format (describe-workdir repository options))))

  (test-equal "describe workdir, invalid strategy"
    #f
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (options (describe-init-options
                     #:strategy 'foo)))
      (describe-format (describe-commit commit options))))

  (test-equal "describe HEAD^, default options"
    "0.1"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (head (commit-lookup repository oid))
           (head^ (commit-parent head)))
      (describe-format (describe-commit head^))))

  (test-equal "describe HEAD^, long format"
    "0.1-0-gb70d891"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (head (commit-lookup repository oid))
           (head^ (commit-parent head))
           (format-options (describe-format-init-options
                            #:always-use-long-format? #t)))
      (describe-format (describe-commit head^) format-options)))

  (test-equal "describe HEAD, max-candidates"
    (list GIT_ENOTFOUND GITERR_DESCRIBE)
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (options (describe-init-options
                     #:max-candidates 0)))
      (catch 'git-error
        (lambda ()
          (clear-git-error!)
          (describe-format (describe-commit commit options)))
        (lambda (key err)
          (list (git-error-code err) (git-error-class err))))))

  (test-equal "describe the root commit, fallback and size"
    "354bc"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (head (commit-lookup repository oid))
           (head^ (commit-parent head))
           (root (commit-parent head^))
           (options (describe-init-options #:fallback-to-oid? #t))
           (format-options (describe-format-init-options
                            #:abbreviated-size 5)))
      (describe-format (describe-commit root options) format-options)))

  ;; ;; Modify a tracked file.
  (call-with-output-file (string-append directory "/" "README")
    (lambda (port)
      (format port "Hello, World?\n")))

  (test-equal "describe workdir, dirty suffix"
    "0.1-1-g3f848a1-dirty"
    (let* ((repository (repository-open directory))
           (format-options (describe-format-init-options
                            #:dirty-suffix "-dirty")))
      (describe-format (describe-workdir repository) format-options))))

(libgit2-shutdown!)

(test-end)
