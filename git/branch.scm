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

(define-module (git branch)
  #:use-module (ice-9 receive)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git reference)
  #:use-module (git types)
  #:export (BRANCH-LOCAL
            BRANCH-REMOTE
            BRANCH-ALL
            branch-create
            branch-create-from-annotated
            branch-delete
            branch-is-head?
            branch-iterator-new
            branch-list
            branch-fold
            branch-lookup
            branch-move
            branch-name
            branch-next
            branch-set-upstream
            branch-upstream))

;;; branch https://libgit2.github.com/libgit2/#HEAD/group/branch

(define BRANCH-LOCAL 1)
(define BRANCH-REMOTE 2)
(define BRANCH-ALL (logior BRANCH-LOCAL BRANCH-REMOTE))

(define branch-create
  (let ((proc (libgit2->procedure* "git_branch_create" `(* * * * ,int))))
    (lambda (repository branch-name target force)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer branch-name)
              (commit->pointer target)
              (if force 1 0))
        (pointer->reference! (dereference-pointer out))))))

(define branch-create-from-annotated
  (let ((proc (libgit2->procedure* "git_branch_create_from_annotated" `(* * * * ,int))))
    (lambda (repository branch-name commit force)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer branch-name)
              (annotated-commit->pointer commit)
              (if force 1 0))
        (pointer->reference! (dereference-pointer out))))))

(define branch-delete
  (let ((proc (libgit2->procedure* "git_branch_delete" '(*))))
    (lambda (branch)
      (proc (reference->pointer branch)))))

(define branch-is-head?
  (let ((proc (libgit2->procedure int "git_branch_is_head" '(*))))
    (lambda (branch)
      (case (proc (reference->pointer branch))
        ((0) #f)
        ((1) #t)
        (else => (lambda (code) (raise-git-error code)))))))

(define %branch-iterator-free (libgit2->pointer "git_branch_iterator_free"))

(define (pointer->branch-iterator! pointer)
  (set-pointer-finalizer! pointer %branch-iterator-free)
  (pointer->branch-iterator pointer))

(define branch-iterator-new
  (let ((proc (libgit2->procedure* "git_branch_iterator_new" `(* * ,int))))
    (lambda (repository flags)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) flags)
        (pointer->branch-iterator (dereference-pointer out))))))

(define branch-lookup
  (let ((proc (libgit2->procedure* "git_branch_lookup" `(* * * ,int))))
    (lambda* (repository branch-name #:optional (type BRANCH-ALL))
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer branch-name)
              type)
        (pointer->reference! (dereference-pointer out))))))

(define branch-move
  (let ((proc (libgit2->procedure* "git_branch_move" `(* * * ,int))))
    (lambda (reference new-branch-name force)
      (let ((out (make-double-pointer)))
        (proc out
              (reference->pointer reference)
              (string->pointer new-branch-name)
              (if force 1 0))
        (pointer->reference! (dereference-pointer out))))))

(define branch-name
  (let ((proc (libgit2->procedure* "git_branch_name" '(* *))))
    (lambda (reference)
      (let ((out (make-double-pointer)))
        (proc out (reference->pointer reference))
        (pointer->string (dereference-pointer out))))))

(define branch-next
  (let ((proc (libgit2->procedure* "git_branch_next" '(* * *))))
    (lambda (iterator)
      (let ((out (make-double-pointer))
            (out-type (make-double-pointer)))
        (proc out out-type (branch-iterator->pointer iterator))
        (values (pointer->reference (dereference-pointer out))
                (pointer-address (dereference-pointer out-type)))))))

(define branch-set-upstream
  (let ((proc (libgit2->procedure* "git_branch_set_upstream" '(* *))))
    (lambda (branch upstream-name)
      (proc (reference->pointer branch) (string->pointer upstream-name)))))

(define branch-upstream
  (let ((proc (libgit2->procedure* "git_branch_upstream" '(* *))))
    (lambda (branch)
      (let ((out (make-double-pointer)))
        (proc out (reference->pointer branch))
        (pointer->reference (dereference-pointer out))))))


(define* (branch-fold proc init repository #:optional (flag BRANCH-ALL))
  (let ((iterator (branch-iterator-new repository flag)))
    (let loop ((acc init))
      (let ((branch (false-if-exception (branch-next iterator))))
        (if branch
            (loop (proc branch acc))
            acc)))))

(define* (branch-list repository #:optional (flag BRANCH-ALL))
  (branch-fold cons '() repository flag))
