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

(define-module (git blame)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (blame-file))


;;; blame

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_buffer

(define blame-file
  (let ((proc (libgit2->procedure* "git_blame_file" '(* * * *))))
    (lambda (repository path options)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (string->pointer path)
              (blame-options->pointer options))
        (pointer->blame! (dereference-pointer out))))))

(define %blame-free (dynamic-func "git_blame_free" libgit2))

(define (pointer->blame! pointer)
  (set-pointer-finalizer! pointer %blame-free)
  (pointer->blame pointer))

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_byindex

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_byline

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_get_hunk_count

;; https://libgit2.github.com/libgit2/#HEAD/group/blame/git_blame_init_options
