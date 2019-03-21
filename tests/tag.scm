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

(test-begin "tag")

(libgit2-init!)

(with-repository "simple" directory

  (test-assert "lightweight tag"
    (let* ((repository (repository-open directory))
           (name "root")
           (target (revparse-single repository "HEAD~2")))
      (tag-create-lightweight repository name target)))

  (test-assert "tag is identical to root commit"
    (let* ((repository (repository-open directory))
           (oid (reference-name->oid repository "refs/tags/root"))
           (root (string->oid "354bcdf85d661533f28dae0e78ce0be99a9dfb9d")))
      (oid=? oid root)))

  (test-equal "create lightweight tag, invalid specification"
    (list GIT_EINVALIDSPEC GITERR_REFERENCE)
    (let* ((repository (repository-open directory))
           (name "root/")
           (target (revparse-single repository "HEAD")))
      (catch 'git-error
        (lambda ()
          (clear-git-error!)
          (tag-create-lightweight repository name target))
        (lambda (key err)
          (list (git-error-code err) (git-error-class err))))))

  (test-equal "create lightweight tag, name already exists"
    (list GIT_EEXISTS GITERR_TAG)
    (let* ((repository (repository-open directory))
           (name "root")
           (target (revparse-single repository "HEAD")))
      (catch 'git-error
        (lambda ()
          (clear-git-error!)
          (tag-create-lightweight repository name target))
        (lambda (key err)
          (list (git-error-code err) (git-error-class err))))))

  (test-equal "overwrite lightweight tag"
    "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
    (let* ((repository (repository-open directory))
           (name "root")
           (target (revparse-single repository "HEAD")))
      (oid->string (tag-create-lightweight! repository name target))))

  (test-assert "create annotated tag"
    (let* ((repository (repository-open directory))
           (name "1.0")
           (target (revparse-single repository "HEAD"))
           (tagger (signature-default repository))
           (message "This tag is annotated!"))
      (tag-create repository name target tagger message)))

  (test-equal "tag-message"
    "This tag is annotated!"
    (let* ((repository (repository-open directory))
           (oid (reference-name->oid repository "refs/tags/1.0")))
      (tag-message (tag-lookup repository oid))))

  )

(libgit2-shutdown!)

(test-end)
