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

(define-module (git tree)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (TREEWALK-PRE
            TREEWALK-POST
            pointer->tree!
            tree-dup
            tree-fold
            tree-entry-byid
            tree-entry-byindex
            tree-entry-bypath
            tree-entry-name
            tree-id
            tree-list
            tree-lookup
            tree-walk))

(define TREEWALK-PRE 0)
(define TREEWALK-POST 1)

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_create_updated

;; XXX: only found in HEAD
;;
;; (define tree-dup
;;   (let ((proc (libgit2->procedure* "git_tree_dup" '(* *))))
;;     (lambda (source)
;;       (let ((out (make-double-pointer)))
;;         (proc out
;;               (tree->pointer source))
;;         (pointer->tree (dereference-pointer out))))))

(define (tree-entry-byid tree id)
  (let* ((proc (libgit2->procedure '* "git_tree_entry_byid" '(* *)))
         (ret (proc (tree->pointer tree) (oid->pointer id))))
    (if (null-pointer? ret)
        #f
        (pointer->tree-entry ret))))

(define (tree-entry-byindex tree idx)
  (let* ((proc (libgit2->procedure '* "git_tree_entry_byindex" `(* ,size_t)))
         (ret (proc (tree->pointer tree) idx)))
    (if (null-pointer? ret)
        #f
        (pointer->tree-entry ret))))

(define (tree-entry-byname tree filename)
  (let* ((proc (libgit2->procedure '* "git_tree_entry_byname" '(* *)))
         (ret (proc (tree->pointer tree) (string->pointer filename))))
    (if (null-pointer? ret)
        #f
        (pointer->tree-entry ret))))

(define (tree-entry-bypath tree path)
  (let ((proc (libgit2->procedure* "git_tree_entry_bypath" '(* * *)))
        (out (make-double-pointer)))
    (proc out (tree->pointer tree) (string->pointer path))
    (pointer->tree-entry! (dereference-pointer out))))

(define (tree-entry-cmp e1 e2)
  (let ((proc (libgit2->procedure int "git_tree_entry_cmp" '(* *))))
    (proc (tree-entry->pointer e1) (tree-entry->pointer e2))))

(define (tree-entry-dup source)
  (let ((proc (libgit2->procedure* "git_tree_entry_dup" '(* *)))
        (dest (make-double-pointer)))
    (proc dest (tree-entry->pointer source))
    (pointer->tree-entry! (dereference-pointer dest))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_entry_filemode

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_entry_filemode_raw

(define (%tree-entry-free)
  (dynamic-func "git_tree_entry_free" (libgit2)))

(define (pointer->tree-entry! pointer)
  (set-pointer-finalizer! pointer (%tree-entry-free))
  (pointer->tree-entry pointer))

(define (tree-entry-id entry)
  (let* ((proc (libgit2->procedure '* "git_tree_entry_id" '(*)))
         (ret (proc (tree-entry->pointer entry))))
    (pointer->oid ret)))

(define (tree-entry-name entry)
  (let* ((proc (libgit2->procedure '* "git_tree_entry_name" '(*)))
         (ret (proc (tree-entry->pointer entry))))
    (pointer->string ret)))

(define (tree-entry->object repository entry)
  (let ((proc (libgit2->procedure* "git_tree_entry_to_object" '(* * *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (tree-entry->pointer entry))
    (pointer->object! (dereference-pointer out))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_entry_type

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_entrycount

(define (%tree-free)
  (dynamic-func "git_tree_free" (libgit2)))

(define (pointer->tree! pointer)
  (set-pointer-finalizer! pointer (%tree-free))
  (pointer->tree pointer))

(define (tree-id tree)
  (let ((proc (libgit2->procedure '* "git_tree_id" '(*))))
    (pointer->oid (proc (tree->pointer tree)))))

(define (tree-lookup repository id)
  (let ((proc (libgit2->procedure* "git_tree_lookup" '(* * *)))
        (out (make-double-pointer)))
    (proc out
          (repository->pointer repository)
          (oid->pointer id))
    (pointer->tree! (dereference-pointer out))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_lookup_prefix

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_owner

(define (tree-walk tree mode callback)
  (let ((proc (libgit2->procedure* "git_tree_walk" `(* ,int * *)))
        ;; If the callback returns a positive value, the passed entry will
        ;; be skipped on the traversal (in pre mode). A negative value stops
        ;; the walk.
        (callback*
         (procedure->pointer int
                             (lambda (root entry _)
                               (callback (pointer->string root)
                                         (pointer->tree-entry entry)))
                             (list '* '* '*))))
    (proc (tree->pointer tree) mode callback* %null-pointer)))

(define (tree-fold proc knil tree)
  (let ((out knil))
    (tree-walk tree TREEWALK-PRE
               (lambda (root entry)
                 ;; XXX: this is not portable
                 (let ((filepath (string-append root (tree-entry-name entry))))
                   (set! out (proc filepath out))
                   0)))
    out))

(define (tree-list tree)
  (tree-fold cons '() tree))
