;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Jelle Licht <jlicht@fsfe.org>
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
  #:use-module (git structs)
  #:use-module (git types)
  #:export (remote-name
            remote-lookup
            remote-fetch
            remote-create-anonymous
            remote-connected?
            remote-connect
            remote-disconnect
            remote-ls))

(define (%remote-free)
  (dynamic-func "git_remote_free" (libgit2)))

(define (pointer->remote! pointer)
  (set-pointer-finalizer! pointer (%remote-free))
  (pointer->remote pointer))

(define (remote-name remote)
  (let ((proc (libgit2->procedure '* "git_remote_name" '(*))))
    (pointer->string (proc (remote->pointer remote)))))

(define* (remote-lookup repository remote-name)
  (let ((proc (libgit2->procedure* "git_remote_lookup" '(* * *)))
        (out (make-double-pointer)))
    (proc out
          (repository->pointer repository)
          (string->pointer remote-name))
    (pointer->remote! (dereference-pointer out))))

(define* (remote-create-anonymous repository url)
  (let ((proc (libgit2->procedure* "git_remote_create_anonymous" '(* * *)))
        (out (make-double-pointer)))
    (proc out
          (repository->pointer repository)
          (string->pointer url))
    (pointer->remote! (dereference-pointer out))))


(define* (remote-connected? remote)
  (let ((proc (libgit2->procedure int "git_remote_connected" '(*))))
    (case (proc (remote->pointer remote))
      ((1) #t)
      (else #f))))

(define GIT_DIRECTION_FETCH 0)

(define* (remote-connect remote)
  (let ((proc (libgit2->procedure* "git_remote_connect" `(* ,int * * * )))  ;; XXX: actual types
        (remote-callbacks (make-remote-callbacks)))
    (set-remote-callbacks-version! remote-callbacks 1)
    (proc (remote->pointer remote)
          GIT_DIRECTION_FETCH
          (remote-callbacks->pointer remote-callbacks)
          %null-pointer
          %null-pointer)))

(define (remote-disconnect remote)
  (let ((proc (libgit2->procedure void "git_remote_disconnect" '(*))))
    (proc (remote->pointer remote))))

(define* (remote-ls remote)
  (let ((proc (libgit2->procedure* "git_remote_ls" '(* * *)))
        (out (make-double-pointer))
        (size-ptr (make-size_t-pointer)))
    (proc out size-ptr
          (remote->pointer remote))
    (pointer->remote-head-list (dereference-pointer out)
                               (pointer->size_t size-ptr))))

(define* (remote-fetch remote #:key (reflog-message "") (fetch-options #f))
  (let ((proc (libgit2->procedure* "git_remote_fetch" '(* * * *))))
    (proc (remote->pointer remote)
          ;; FIXME https://libgit2.github.com/libgit2/#HEAD/type/git_strarray
          %null-pointer
          (if fetch-options
              (fetch-options->pointer fetch-options)
              %null-pointer)
          (string->pointer reflog-message))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/reset/git_reset_default
