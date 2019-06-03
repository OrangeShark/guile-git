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

(define-module (git checkout)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types))


;;; checkout https://libgit2.github.com/libgit2/#HEAD/group/checkout

(define checkout-head
  (let ((proc (libgit2->procedure* "git_checkout_head" '(* *))))
    (lambda (repository options)
      (proc (repository->pointer repository) %null-pointer))))

(define checkout-index
  (let ((proc (libgit2->procedure* "git_checkout_index" '(* * *))))
    (lambda (repository index options)
      (proc (repository->pointer repository)
	    (index->pointer index)
	    %null-pointer))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/checkout/git_checkout_init_options

(define checkout-tree
  (let ((proc (libgit2->procedure* "git_checkout_tree" `(* * *))))
    (lambda (repository treeish)
      (proc (repository->pointer repository)
	    (object->pointer treeish)
	    %null-pointer))))
