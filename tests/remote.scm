;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (tests remote)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git)
             (git object))

(test-begin "remote")

(libgit2-init!)

(with-repository "simple-bare" directory

  (test-equal "remote lookup & name"
    "origin"
    (let* ((repository (repository-open directory))
           (remote (remote-lookup repository "origin")))
      (remote-name remote)))

  (test-equal "remote lookup, not found"
    (list GIT_ENOTFOUND GITERR_CONFIG)
    (catch 'git-error
      (lambda ()
        (let ((repository (repository-open directory)))
          (clear-git-error!)
          (remote-lookup repository "does-not-exist")))
      (lambda (key err)
        (list (git-error-code err) (git-error-class err))))))

(libgit2-shutdown!)

(test-end)
