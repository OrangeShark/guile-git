;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (git status)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git structs)
  #:use-module (git types)
  #:use-module (srfi srfi-26)

  #:export (STATUS-SHOW-INDEX-AND-WORKDIR
            STATUS-SHOW-INDEX-ONLY
            STATUS-SHOW-WORKDIR-ONLY

            STATUS-FLAG-INCLUDE-UNTRACKED
            STATUS-FLAG-INCLUDE-IGNORED
            STATUS-FLAG-INCLUDE-UNMODIFIED
            STATUS-FLAG-EXCLUDE-SUBMODULES
            STATUS-FLAG-RECURSE-UNTRACKED-DIRS
            STATUS-FLAG-DISABLE-PATHSPEC-MATCH
            STATUS-FLAG-RECURSE-IGNORED-DIRS
            STATUS-FLAG-RENAMES-HEAD-TO-INDEX
            STATUS-FLAG-RENAMES-INDEX-TO-WORKDIR
            STATUS-FLAG-SORT-CASE-SENSITIVELY
            STATUS-FLAG-SORT-CASE-INSENSITIVELY
            STATUS-FLAG-RENAMES-FROM-REWRITES
            STATUS-FLAG-NO-REFRESH
            STATUS-FLAG-UPDATE-INDEX
            STATUS-FLAG-INCLUDE-UNREADABLE
            STATUS-FLAG-INCLUDE-UNREADABLE-AS-UNTRACKED

            status-init-options
            status-list-new
            status-list-entry-count
            status-byindex
            status-list->status-entries))

(define STATUS-OPTIONS-VERSION 1)

(define STATUS-SHOW-INDEX-AND-WORKDIR  0)
(define STATUS-SHOW-INDEX-ONLY         1)
(define STATUS-SHOW-WORKDIR-ONLY       2)

(define STATUS-FLAG-INCLUDE-UNTRACKED                1)
(define STATUS-FLAG-INCLUDE-IGNORED                  2)
(define STATUS-FLAG-INCLUDE-UNMODIFIED               4)
(define STATUS-FLAG-EXCLUDE-SUBMODULES               8)
(define STATUS-FLAG-RECURSE-UNTRACKED-DIRS           16)
(define STATUS-FLAG-DISABLE-PATHSPEC-MATCH           32)
(define STATUS-FLAG-RECURSE-IGNORED-DIRS             64)
(define STATUS-FLAG-RENAMES-HEAD-TO-INDEX            128)
(define STATUS-FLAG-RENAMES-INDEX-TO-WORKDIR         256)
(define STATUS-FLAG-SORT-CASE-SENSITIVELY            512)
(define STATUS-FLAG-SORT-CASE-INSENSITIVELY          1024)
(define STATUS-FLAG-RENAMES-FROM-REWRITES            2048)
(define STATUS-FLAG-NO-REFRESH                       4096)
(define STATUS-FLAG-UPDATE-INDEX                     8192)
(define STATUS-FLAG-INCLUDE-UNREADABLE               16384)
(define STATUS-FLAG-INCLUDE-UNREADABLE-AS-UNTRACKED  32768)

(define (%status-list-free)
  (dynamic-func "git_status_list_free" (libgit2)))

(define (pointer->status-list! pointer)
  (set-pointer-finalizer! pointer (%status-list-free))
  (pointer->status-list pointer))

(define* (status-init-options
          #:optional
          (show STATUS-SHOW-INDEX-AND-WORKDIR)
          (flags STATUS-FLAG-INCLUDE-UNTRACKED))
  (let ((proc (libgit2->procedure* "git_status_init_options" `(* ,unsigned-int)))
        (status-options (make-status-options)))
    (proc (status-options->pointer status-options) STATUS-OPTIONS-VERSION)
    (set-status-options-show! status-options show)
    (set-status-options-flags! status-options flags)
    status-options))

(define (status-list-new repository status-options)
  (let ((proc (libgit2->procedure* "git_status_list_new" '(* * *)))
        (out (make-double-pointer)))
    (proc out (repository->pointer repository) (status-options->pointer status-options))
    (pointer->status-list! (dereference-pointer out))))

(define (status-list-entry-count status-list)
  (let ((proc (libgit2->procedure size_t "git_status_list_entrycount" '(*))))
    (proc (status-list->pointer status-list))))

(define (status-byindex status-list index)
  (let ((proc (libgit2->procedure '* "git_status_byindex" `(* ,size_t))))
    (pointer->status-entry (proc (status-list->pointer status-list) index))))

(define (status-list->status-entries status-list)
  (map (cut status-byindex status-list <>)
       (iota (status-list-entry-count status-list))))

;;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/status/git_status_file

;;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/status/git_status_foreach

;;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/status/git_status_foreach_ext

;;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/status/git_status_list_get_perfdata

;;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/status/git_status_should_ignore
