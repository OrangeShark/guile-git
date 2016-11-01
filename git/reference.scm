;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016 Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (git reference)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (reference-name
            reference-target
            reference-name->oid
            reference-shorthand))


;;; FIXME: reference https://libgit2.github.com/libgit2/#HEAD/group/reference

(define reference-name
  (let ((proc (libgit2->procedure '* "git_reference_name" '(*))))
    (lambda (reference)
      (pointer->string (proc (reference->pointer reference))))))

(define reference-target
  (let ((proc (libgit2->procedure '* "git_reference_target" '(*))))
    (lambda (reference)
      (pointer->oid (proc (reference->pointer reference))))))

(define reference-name->oid
  (let ((proc (libgit2->procedure* "git_reference_name_to_id" '(* * *))))
    (lambda (repository name)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer name))
        (pointer->oid out)))))

(define reference-shorthand
  (let ((proc (libgit2->procedure '* "git_reference_shorthand" '(*))))
    (lambda (reference)
      (pointer->string (proc (reference->pointer reference))))))
