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

(define-module (git clone)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git structs)
  #:use-module (git types)
  #:use-module (git repository)
  #:export (clone
            clone-init-options ;deprecated!
            make-clone-options))

;;; clone https://libgit2.github.com/libgit2/#HEAD/group/clone

(define CLONE-OPTIONS-VERSION 1)

(define clone
  (let ((proc (libgit2->procedure* "git_clone" '(* * * *))))
    (lambda* (url directory #:optional (clone-options (make-clone-options)))
      "Clones a remote repository found at URL into DIRECTORY.

Returns the repository on success or throws an error on failure."
      (let ((out (make-double-pointer)))
        (proc out
              (string->pointer url)
              (string->pointer directory)
              (clone-options->pointer clone-options))
        (pointer->repository! (dereference-pointer out))))))

(define (make-clone-options)
  (let ((proc (libgit2->procedure* "git_clone_init_options" `(* ,unsigned-int)))
        (clone-options (make-clone-options-bytestructure)))
    (proc (clone-options->pointer clone-options) CLONE-OPTIONS-VERSION)
    clone-options))

(define clone-init-options
  ;; Deprecated alias for compatibility with 0.2.
  make-clone-options)
