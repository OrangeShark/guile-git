;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests tree)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "tree")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "commit-tree tree-id"
    "d40674e05d114e5eb0df0f358ebeec47b8782ced"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (oid->string (tree-id tree))))

  (test-equal "tree-walk list files"
    (list "message" "directory" "README")
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (let ((files '()))
        (tree-walk tree TREEWALK-PRE (lambda (root entry)
                                           (set! files (cons (tree-entry-name entry) files))
                                           0))
        files)))

  (test-equal "tree-list"
    (list "directory/message" "directory" "README")
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (tree-list tree)))

  (test-equal "tree-entry-bypath"
    "message"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (commit (commit-lookup repository oid))
           (tree (commit-tree commit)))
      (tree-entry-name (tree-entry-bypath tree "directory/message"))))
  )


(libgit2-shutdown!)

(test-end)
