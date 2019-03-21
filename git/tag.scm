;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (git tag)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (tag-lookup
            tag-lookup-prefix
            tag-id
            tag-target-id
            tag-message
            tag-name

            tag-create
            tag-create!
            tag-create-lightweight
            tag-create-lightweight!))

(define %tag-free (dynamic-func "git_tag_free" libgit2))

(define (pointer->tag! pointer)
  (set-pointer-finalizer! pointer %tag-free)
  (pointer->tag pointer))

(define tag-lookup
  (let ((proc (libgit2->procedure* "git_tag_lookup" '(* * *))))
    (lambda (repository oid)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) (oid->pointer oid))
        (pointer->tag! (dereference-pointer out))))))

(define tag-lookup-prefix
  (let ((proc (libgit2->procedure* "git_tag_lookup_prefix" `(* * * ,size_t))))
    (lambda (repository oid length)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) (oid->pointer oid) length)
        (pointer->tag! (dereference-pointer out))))))

(define tag-id
  (let ((proc (libgit2->procedure '* "git_tag_id" '(*))))
    (lambda (tag)
      (pointer->oid (proc (tag->pointer tag))))))

(define tag-target-id
  (let ((proc (libgit2->procedure '* "git_tag_target_id" '(*))))
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

(define* (tag-create repository name target tagger message
                    #:optional (force? #f))
  (let ((proc (libgit2->procedure* "git_tag_create" `(* * * * * * ,int)))
        (oid (make-oid-pointer)))
    (proc oid
          (repository->pointer repository)
          (string->pointer name)
          (object->pointer target)
          (signature->pointer tagger)
          (string->pointer message)
          (if force? 1 0))
    (pointer->oid oid)))

(define (tag-create! repository name target tagger message)
  (tag-create repository name target tagger message #t))

(define* (tag-create-lightweight repository name target
                                 #:optional (force? #f))
  (let ((proc (libgit2->procedure* "git_tag_create_lightweight" `(* * * * ,int)))
        (oid (make-oid-pointer)))
    (proc oid
          (repository->pointer repository)
          (string->pointer name)
          (object->pointer target)
          (if force? 1 0))
    (pointer->oid oid)))

(define (tag-create-lightweight! repository name target)
  (tag-create-lightweight repository name target #t))
