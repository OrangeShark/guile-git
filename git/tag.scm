;;; Guile-Git --- GNU Guile bindings of libgit2
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

(define-module (git tag)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (tag-lookup
            tag-lookup-prefix
            tag-id
            tag-message
            tag-name))


(define %tag-free (dynamic-func "git_tag_free" libgit2))

(define tag-lookup
  (let ((proc (libgit2->procedure* "git_tag_lookup" '(* * *))))
    (lambda (repository oid)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer out) (oid->pointer oid))
        (pointer->tag (pointer-gc (dereference-pointer out) %tag-free))))))

(define tag-lookup-prefix
  (let ((proc (libgit2->procedure* "git_tag_lookup_prefix" `(* * * ,size_t))))
    (lambda (repository oid length)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer out) (oid->pointer oid) length)
        (pointer->tag (pointer-gc (dereference-pointer out) %tag-free))))))

(define tag-id
  (let ((proc (libgit2->procedure '* "git_tag_id" '(*))))
    (lambda (tag)
      (pointer->oid (proc (tag->pointer tag))))))

(define tag-message
  (let ((proc (libgit2->procedure '* "git_tag_message" '(*))))
    (lambda (tag)
      (pointer->string (proc (tag->pointer tag))))))

(define tag-name
  (let ((proc (libgit2->procedure '* "git_tag_name" '(*))))
    (lambda (tag)
      (pointer->string (proc (tag->pointer tag))))))
