;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (git remote)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (remote-name
            remote-lookup
            remote-fetch))

(define %remote-free (dynamic-func "git_remote_free" libgit2))

(define (pointer->remote! pointer)
  (set-pointer-finalizer! pointer %remote-free)
  (pointer->remote pointer))

(define remote-name
  (let ((proc (libgit2->procedure '* "git_remote_name" '(*))))
    (lambda (remote)
      (pointer->string (proc (remote->pointer remote))))))

(define remote-lookup
  (let ((proc (libgit2->procedure* "git_remote_lookup" '(* * *))))
    (lambda* (repository remote-name)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer remote-name))
        (pointer->remote! (dereference-pointer out))))))

(define remote-fetch
  (let ((proc (libgit2->procedure* "git_remote_fetch" '(* * * *))))
    (lambda* (remote #:optional (reflog-message ""))
      (proc (remote->pointer remote)
            ;; FIXME https://libgit2.github.com/libgit2/#HEAD/type/git_strarray
            %null-pointer
            ;; FIXME https://libgit2.github.com/libgit2/#HEAD/type/git_fetch_options
            %null-pointer
            (string->pointer reflog-message)))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/reset/git_reset_default
