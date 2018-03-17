;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (git object)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (git bindings)
  #:use-module (git config)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (OBJ-ANY
            OBJ-BAD
            OBJ-EXT1
            OBJ-COMMIT
            OBJ-TREE
            OBJ-BLOB
            OBJ-TAG
            OBJ-EXT2
            OBJ-OFS-DELTA
            OBJ-REF-DELTA
            object-id
            object-lookup
            object-lookup-prefix
            object-owner
            object-short-id
            object-type))

;; Git Object types
(define OBJ-ANY -2)
(define OBJ-BAD -1)
(define OBJ-EXT1 0)
(define OBJ-COMMIT 1)
(define OBJ-TREE 2)
(define OBJ-BLOB 3)
(define OBJ-TAG 4)
(define OBJ-EXT2 5)
(define OBJ-OFS-DELTA 6)
(define OBJ-REF-DELTA 7)

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object__size

(define object-dup
  (let ((proc (libgit2->procedure* "git_object_dup" '(* *))))
    (lambda (object)
      (let ((out (make-double-pointer)))
        (proc out (object->pointer object))
        (pointer->object! (dereference-pointer out))))))

(define object-id
  (let ((proc (libgit2->procedure '* "git_object_id" '(*))))
    (lambda (object)
      (pointer->oid (proc (object->pointer object))))))

(define object-lookup
  (let ((proc (libgit2->procedure* "git_object_lookup" `(* * * ,int))))
    (lambda* (repository oid #:optional (type OBJ-ANY))
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
        (proc out (repository->pointer repository) (oid->pointer oid)
              type)
        (pointer->object! (dereference-pointer out))))))

(define object-lookup-prefix
  (let ((proc (libgit2->procedure* "git_object_lookup_prefix"
                                   `(* * * ,size_t ,int))))
    (lambda* (repository oid length #:optional (type OBJ-ANY))
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
        (proc out (repository->pointer repository) (oid->pointer oid)
              length type)
        (pointer->object! (dereference-pointer out))))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_lookup_bypath

(define object-owner
  (let ((proc (libgit2->procedure '* "git_object_owner" '(*))))
    (lambda (object)
      (pointer->repository (proc (object->pointer object))))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_peel

(define object-short-id
  (let ((proc (libgit2->procedure* "git_object_short_id" '(* *))))
    (lambda (object)
      (let ((out (make-buffer)))
        (proc out (object->pointer object))
        (let ((out* (buffer-content/string out)))
          (free-buffer out)
          out*)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_string2type

(define object-type
  (let ((proc (libgit2->procedure int "git_object_type" '(*))))
    (lambda (object)
      (proc (object->pointer object)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_type2string

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_typeisloose
