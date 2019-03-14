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


(define-module (git annotated)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (annotated-commit-from-fetchhead
            annotated-commit-from-ref
            annotated-commit-from-revspec
            annotated-commit-id
            annotated-commit-lookup))

;;; annotated

(define (%annotated-commit-free)
  (dynamic-func "git_annotated_commit_free" (libgit2)))

(define (pointer->annotated-commit! pointer)
  (set-pointer-finalizer! pointer (%annotated-commit-free))
  (pointer->annotated-commit pointer))

(define (annotated-commit-from-fetchhead repository branch-name remote-url id)
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_fetchhead" '(* * * * *)))
        (out (make-double-pointer)))
    (proc out
          (repository->pointer repository)
          (string->pointer branch-name)
          (string->pointer remote-url)
          (oid->pointer id))
    (pointer->annotated-commit! (dereference-pointer out))))

(define (annotated-commit-from-ref repository reference)
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_ref" '(* * *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (reference->pointer reference))
    (pointer->annotated-commit! (dereference-pointer out))))

(define (annotated-commit-from-revspec repository revspec)
  (let ((proc (libgit2->procedure* "git_annotated_commit_from_revspec" '(* * *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (string->pointer revspec))
    (pointer->annotated-commit (dereference-pointer out))))

(define (annotated-commit-id commit)
  (let ((proc (libgit2->procedure '* "git_annotated_commit_id" '(*))))
    (pointer->oid (proc (annotated-commit->pointer commit)))))

(define (annotated-commit-lookup repository id)
  (let ((proc (libgit2->procedure* "git_annotated_commit_lookup" '(* * *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (oid->pointer id))
    (pointer->annotated-commit! (dereference-pointer out))))
