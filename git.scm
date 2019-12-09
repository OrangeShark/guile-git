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

(define-module (git)
  #:use-module (git bindings))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((git auth)
        (git bindings)
        (git branch)
        (git clone)
        (git commit)
        (git describe)
        (git errors)
        (git fetch)
        (git object)
        (git oid)
        (git reference)
        (git repository)
        (git reset)
        (git remote)
        (git rev-parse)
        (git settings)
        (git signature)
        (git status)
        (git structs)
        (git submodule)
        (git tag)
        (git tree)))

    (let* ((current-module (current-module))
          (current-module-interface (resolve-interface (module-name current-module))))
      (for-each
       (lambda (git-submodule)
         (let ((git-submodule-interface (resolve-interface git-submodule)))
           (module-use! current-module git-submodule-interface)
           (module-use! current-module-interface git-submodule-interface)))
       %public-modules))))

(libgit2-init!)
