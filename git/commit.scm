;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (git commit)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (git bindings)
  #:use-module (git structs)
  #:use-module (git oid)
  #:use-module (git tree)
  #:use-module (git types)
  #:use-module (git object)
  #:use-module (git reference)
  #:use-module (git repository)
  #:export (object->commit
            commit-amend
            commit-author
            commit-body
            commit-committer
            commit-extract-signature
            commit-header-field
            commit-id
            commit-lookup
            commit-lookup-prefix
            commit-message
            commit-message-encoding
            commit-message-raw
            commit-owner
            commit-parent
            commit-parent-id
            commit-parentcount
            commit-parents
            commit-raw-header
            commit-summary
            commit-time
            commit-time-offset
            commit-tree
            commit-tree-id

            fold-commits))

;; commit https://libgit2.github.com/libgit2/#HEAD/group/commit

(define (print-commit commit port)
  ;; Don't print the address of COMMIT since identical commits are 'eq?'.
  (format port "#<git-commit ~a>"
          (oid->string (commit-id commit))))

(set-record-type-printer! (@@ (git types) <commit>)
                          print-commit)

(define (object->commit object)
  (and (= (object-type object) OBJ-COMMIT)
       (pointer->commit (object->pointer object))))

(define (commit-amend id commit update-ref author
                      commiter message-encoding message tree)
  (let ((proc (libgit2->procedure* "git_commit_amend" '(* * * * * * * *))))
    (proc (oid->pointer id)
          (commit->pointer commit)
          (string->pointer update-ref)
          (signature->pointer author)
          (signature->pointer commiter)
          (string->pointer message-encoding)
          (string->pointer message)
          (tree->pointer tree))))

(define (commit-author commit)
  (let ((proc (libgit2->procedure '* "git_commit_author" '(*))))
    (pointer->signature (proc (commit->pointer commit)))))

(define (commit-body commit)
  (let* ((proc (libgit2->procedure '* "git_commit_body" '(*)))
         (out (proc (commit->pointer commit))))
    (if (eq? out %null-pointer)
        ""
        (pointer->string out))))

(define (commit-committer commit)
  (let ((proc (libgit2->procedure '* "git_commit_committer" '(*))))
    (pointer->signature (proc (commit->pointer commit)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_buffer

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_from_callback

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_v

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_with_signature

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_dup

(define* (commit-extract-signature repository oid #:optional (field "gpgsig"))
  (let ((proc (libgit2->procedure* "git_commit_extract_signature" '(* * * * *)))
        (signature (make-buffer))
        (data (make-buffer)))
    (proc signature
          data
          (repository->pointer repository)
          (oid->pointer oid)
          (string->pointer field))
    (let ((signature* (buffer-content/string signature))
          (data*      (buffer-content/string data)))
      (free-buffer signature)
      (free-buffer data)
      (values signature* data*))))

(define (%commit-free)
  (dynamic-func "git_commit_free" (libgit2)))

(define (pointer->commit! pointer)
  (set-pointer-finalizer! pointer (%commit-free))
  (pointer->commit pointer))

(define (commit-header-field commit field)
  (let ((proc (libgit2->procedure* "git_commit_header_field" '(* * *)))
        (out (make-buffer)))
    (proc out (commit->pointer commit) (string->pointer field))
    (let ((out* (buffer-content/string out)))
      (free-buffer out)
      out*)))

(define (commit-id commit)
  (let ((proc (libgit2->procedure '* "git_commit_id" '(*))))
    (pointer->oid (proc (commit->pointer commit)))))

(define (commit-lookup repository oid)
  (let ((proc (libgit2->procedure* "git_commit_lookup" `(* * *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (oid->pointer oid))
    (pointer->commit! (dereference-pointer out))))

(define (commit-lookup-prefix repository id len)
  (let ((proc (libgit2->procedure* "git_commit_lookup_prefix" `(* * * ,size_t)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (oid->pointer id) len)
    (pointer->commit! (dereference-pointer out))))

(define (commit-message commit)
  (let ((proc (libgit2->procedure '* "git_commit_message" '(*))))
    (pointer->string (proc (commit->pointer commit)))))

(define (commit-message-encoding commit)
  (let* ((proc (libgit2->procedure '* "git_commit_message_encoding" '(*)))
         (out (proc (commit->pointer commit))))
    (if (eq? out %null-pointer)
        #f
        (pointer->string out))))

(define (commit-message-raw commit)
  (let ((proc (libgit2->procedure '* "git_commit_message_raw" '(*))))
    (pointer->string (proc (commit->pointer commit)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_nth_gen_ancestor

(define (commit-owner commit)
  (let ((proc (libgit2->procedure '* "git_commit_owner" '(*))))
    (pointer->repository (proc (commit->pointer commit)))))

(define* (commit-parent commit #:optional (n 0))
  (let ((proc (libgit2->procedure* "git_commit_parent" `(* * ,unsigned-int)))
        (out (make-double-pointer)))
    (proc out (commit->pointer commit) n)
    (pointer->commit! (dereference-pointer out))))

(define* (commit-parent-id commit #:optional (n 0))
  (let ((proc (libgit2->procedure '* "git_commit_parent_id" `(* ,unsigned-int))))
    (pointer->oid (proc (commit->pointer commit) n))))

(define (commit-parentcount commit)
  (let ((proc (libgit2->procedure unsigned-int "git_commit_parentcount" '(*))))
    (proc (commit->pointer commit))))

(define (commit-parents commit)
  "Return the list of all the parent commits of COMMIT."
  (unfold (cute >= <> (commit-parentcount commit))
          (cut commit-parent commit <>)
          1+
          0))

(define* (fold-commits proc seed repo
                       #:key
                       (start (reference-target
                               (repository-head repo)))
                       end)
  "Call PROC once on each commit of REPO, starting at START (an OID) and
until END (an OID) included; if END is omitted, stop at the root commit.
This procedure performs a breadth-first traversal of the commit graph."
  ;; Note: We rely on the fact that identical commits yield commit objects
  ;; that are 'eq?'.
  (let loop ((commits (list (commit-lookup repo start)))
             (result seed)
             (visited vlist-null))
    (match commits
      ((commit . rest)
       (cond ((vhash-assq commit visited)
              (loop rest result visited))
             ((and end (oid=? (commit-id commit) end))
              (loop rest
                    (proc commit result)
                    (vhash-consq commit #t visited)))
             (else
              (loop (append (commit-parents commit) rest)
                    (proc commit result)
                    (vhash-consq commit #t visited)))))
      (()
       result))))

(define (commit-raw-header commit)
  (let ((proc (libgit2->procedure '* "git_commit_raw_header" '(*))))
    (pointer->string (proc (commit->pointer commit)))))

(define (commit-summary commit)
  (let ((proc (libgit2->procedure '* "git_commit_summary" '(*))))
    (pointer->string (proc (commit->pointer commit)))))

(define (commit-time commit)
  (let ((proc (libgit2->procedure int64 "git_commit_time" '(*))))
    (proc (commit->pointer commit))))

(define (commit-time-offset commit)
  (let ((proc (libgit2->procedure int "git_commit_time_offset" '(*))))
    (proc (commit->pointer commit))))

(define (commit-tree commit)
  (let ((proc (libgit2->procedure* "git_commit_tree" '(* *)))
        (out (make-double-pointer)))
    (proc out (commit->pointer commit))
    (pointer->tree! (dereference-pointer out))))

(define (commit-tree-id commit)
  (let ((proc (libgit2->procedure '* "git_commit_tree_id" '(*))))
    (pointer->oid (proc (commit->pointer commit)))))
