;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (git structs)
  #:export (reference-name
            reference-target
            reference-name->oid
            reference-shorthand
            reference-peel
            pointer->reference!
            reference-lookup
            reference-iterator-new
            reference-iterator-glob-new
            reference-next
            reference-fold
            reference-branch?
            reference-note?
            reference-remote?
            reference-tag?
            reference-eq?))


;;; FIXME: reference https://libgit2.github.com/libgit2/#HEAD/group/reference

(define (reference-name reference)
  (let ((proc (libgit2->procedure '* "git_reference_name" '(*))))
    (pointer->string (proc (reference->pointer reference)))))

(define (reference-target reference)
  (let ((proc (libgit2->procedure '* "git_reference_target" '(*))))
    (pointer->oid (proc (reference->pointer reference)))))

(define (reference-name->oid repository name)
  (let ((proc (libgit2->procedure* "git_reference_name_to_id" '(* * *)))
        (out (make-oid-pointer)))
    (proc out
          (repository->pointer repository)
          (string->pointer name))
    (pointer->oid out)))

(define (reference-shorthand reference)
  (let ((proc (libgit2->procedure '* "git_reference_shorthand" '(*))))
    (pointer->string (proc (reference->pointer reference)))))

(define (reference-peel reference obj-type)
  (let ((proc (libgit2->procedure* "git_reference_peel" `(* * ,int)))
        (out (make-double-pointer)))
    (proc out (reference->pointer reference) obj-type)
    (pointer->object! (dereference-pointer out))))

(define (%reference-free)
  (dynamic-func "git_reference_free" (libgit2)))

(define (pointer->reference! pointer)
  (set-pointer-finalizer! pointer (%reference-free))
  (pointer->reference pointer))

(define (reference-lookup repository name)
  (let ((proc (libgit2->procedure* "git_reference_lookup" '(* * *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (string->pointer name))
    (pointer->reference! (dereference-pointer out))))


(define (%reference-iterator-free)
  (dynamic-func "git_reference_iterator_free" (libgit2)))

(define (pointer->reference-iterator! pointer)
  (set-pointer-finalizer! pointer (%reference-iterator-free))
  (pointer->reference-iterator pointer))

(define (reference-iterator-new repository)
  (let ((proc (libgit2->procedure* "git_reference_iterator_new" '(* *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository))
    (pointer->reference-iterator! (dereference-pointer out))))

(define (reference-iterator-glob-new repository glob)
  (let ((proc (libgit2->procedure* "git_reference_iterator_glob_new" '(* * *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (string->pointer glob))
    (pointer->reference-iterator! (dereference-pointer out))))

(define (reference-next iterator)
  (let ((proc (libgit2->procedure* "git_reference_next" '(* *)))
        (out (make-double-pointer)))
    (proc out (reference-iterator->pointer iterator))
    (pointer->reference (dereference-pointer out))))

(define* (reference-fold proc init repository #:key (glob #f))
  (let ((iterator (if glob
                      (reference-iterator-glob-new repository glob)
                      (reference-iterator-new repository))))
    (let loop ((acc init))
      (let ((reference (false-if-exception (reference-next iterator))))
        (if reference
            (loop (proc reference acc))
            acc)))))

(define (reference-branch? reference)
  (let ((proc (libgit2->procedure int "git_reference_is_branch" '(*))))
    (case (proc (reference->pointer reference))
      ((1) #t)
      (else #f))))

(define (reference-note? reference)
  (let ((proc (libgit2->procedure int "git_reference_is_note" '(*))))
    (case (proc (reference->pointer reference))
      ((1) #t)
      (else #f))))

(define (reference-remote? reference)
  (let ((proc (libgit2->procedure int "git_reference_is_remote" '(*))))
    (case (proc (reference->pointer reference))
      ((1) #t)
      (else #f))))

(define (reference-tag? reference)
  (let ((proc (libgit2->procedure int "git_reference_is_tag" '(*))))
    (case (proc (reference->pointer reference))
      ((1) #t)
      (else #f))))

(define (reference-eq? ref1 ref2)
  (let ((proc (libgit2->procedure int "git_reference_cmp" '(* *))))
    (case (proc (reference->pointer ref1) (reference->pointer ref2))
      ((0) #t)
      (else #f))))
