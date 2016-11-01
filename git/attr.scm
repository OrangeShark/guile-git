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

(define-module (git attr)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types))

;;; attr

(define attr-add-macro
  (let ((proc (libgit2->procedure* "git_attr_add_macro" '(* * *))))
    (lambda (repository name values)
      (proc (repository->pointer repository)
	    (string->pointer name)
	    (string->pointer values)))))

(define attr-cache-flush
  (let ((proc (libgit2->procedure void "git_attr_cache_flush" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_foreach

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_get

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_get_many

;; https://libgit2.github.com/libgit2/#HEAD/group/attr/git_attr_value
