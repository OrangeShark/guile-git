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
            reference-shorthand
            reference-peel
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

(define reference-peel
  (let ((proc (libgit2->procedure* "git_reference_peel" `(* * ,int))))
    (lambda (reference obj-type)
      (let ((out (make-double-pointer)))
        (proc out (reference->pointer reference) obj-type)
        (pointer->object (dereference-pointer out))))))

(define %reference-free (dynamic-func "git_reference_free" libgit2))

(define reference-lookup
  (let ((proc (libgit2->procedure* "git_reference_lookup" '(* * *))))
    (lambda (repository name)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) (string->pointer name))
        (pointer->reference (pointer-gc (dereference-pointer out) %reference-free))))))


(define %reference-iterator-free (dynamic-func "git_reference_iterator_free" libgit2))

(define reference-iterator-new
  (let ((proc (libgit2->procedure* "git_reference_iterator_new" '(* *))))
    (lambda (repository)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository))
        (pointer->reference-iterator (pointer-gc (dereference-pointer out) %reference-iterator-free))))))

(define reference-iterator-glob-new
  (let ((proc (libgit2->procedure* "git_reference_iterator_glob_new" '(* * *))))
    (lambda (repository glob)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) (string->pointer glob))
        (pointer->reference-iterator (pointer-gc (dereference-pointer out) %reference-iterator-free))))))

(define reference-next
  (let ((proc (libgit2->procedure* "git_reference_next" '(* *))))
    (lambda (iterator)
      (let ((out (make-double-pointer)))
        (proc out (reference-iterator->pointer iterator))
        (pointer->reference (dereference-pointer out))))))

(define* (reference-fold proc init repository #:key (glob #f))
  (let ((iterator (if glob
                      (reference-iterator-glob-new repository glob)
                      (reference-iterator-new repository))))
    (let loop ((acc init))
      (let ((reference (false-if-exception (reference-next iterator))))
        (if reference
            (loop (proc reference acc))
            acc)))))

(define reference-branch?
  (let ((proc (libgit2->procedure int "git_reference_is_branch" '(*))))
    (lambda (reference)
      (case (proc (reference->pointer reference))
        ((1) #t)
        (else #f)))))

(define reference-note?
  (let ((proc (libgit2->procedure int "git_reference_is_note" '(*))))
    (lambda (reference)
      (case (proc (reference->pointer reference))
        ((1) #t)
        (else #f)))))

(define reference-remote?
  (let ((proc (libgit2->procedure int "git_reference_is_remote" '(*))))
    (lambda (reference)
      (case (proc (reference->pointer reference))
        ((1) #t)
        (else #f)))))

(define reference-tag?
  (let ((proc (libgit2->procedure int "git_reference_is_tag" '(*))))
    (lambda (reference)
      (case (proc (reference->pointer reference))
        ((1) #t)
        (else #f)))))

(define reference-eq?
  (let ((proc (libgit2->procedure int "git_reference_cmp" '(* *))))
    (lambda (ref1 ref2)
      (case (proc (reference->pointer ref1) (reference->pointer ref2))
        ((0) #t)
        (else #f)))))
