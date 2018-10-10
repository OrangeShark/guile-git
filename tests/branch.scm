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

(define-module (tests branch)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "branch")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "branch-list"
    (list "master")
    (let* ((repository (repository-open directory)))
      (map branch-name (branch-list repository))))

  (test-equal "branch-lookup"
    #t
    (let* ((repository (repository-open directory))
           (master (reference-target (repository-head repository)))
           (other (reference-target (branch-lookup repository "master"))))
      (apply equal? (map oid->string (list master other)))))

  (test-equal "branch-name"
    "master"
    (let* ((repository (repository-open directory))
           (master (repository-head repository)))
      (branch-name master)))

  )


(libgit2-shutdown!)

(test-end)
