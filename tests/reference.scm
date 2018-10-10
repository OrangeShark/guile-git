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

(define-module (tests reference)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "reference")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "reference-name"
    "refs/heads/master"
    (let* ((repository (repository-open directory)))
      (reference-name (repository-head repository))))

  (test-equal "reference-target"
    "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
    (let* ((repository (repository-open directory)))
      (oid->string (reference-target (repository-head repository)))))

  (test-equal "reference-name->oid"
    "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
    (let* ((repository (repository-open directory)))
      (oid->string (reference-name->oid repository "refs/heads/master"))))

  (test-equal "reference-shorthand"
    "master"
    (let* ((repository (repository-open directory)))
      (reference-shorthand (repository-head repository))))

  )

(libgit2-shutdown!)

(test-end)
