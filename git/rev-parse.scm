;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (git rev-parse)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (revparse-single))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/revparse/git_revparse

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/revparse/git_revparse_ext

(define revparse-single
  (let ((proc (libgit2->procedure* "git_revparse_single" '(* * *))))
    (lambda (repository spec)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) (string->pointer spec))
        (pointer->object! (dereference-pointer out))))))
