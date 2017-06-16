;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (git))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((git bindings)
        (git branch)
        (git clone)
        (git commit)
        (git oid)
        (git reference)
        (git repository)
        (git reset)
        (git remote)
        (git status)
        (git structs)
        (git tag)
        (git tree)))

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface m))))
              %public-modules)))
