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

(define-module (git oid)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (oid-cmp
            oid=?
            oid-zero?
            oid-ncmp?
            oid-strcmp
            oid-str=?
            oid->string))

;;; oid https://libgit2.github.com/libgit2/#HEAD/group/oid

(define oid-cmp
  (let ((proc (libgit2->procedure int "git_oid_cmp" '(* *))))
    (lambda (a b)
      (proc (oid->pointer a) (oid->pointer b)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_cpy

(define (oid=? a b)
  (let ((proc (libgit2->procedure int "git_oid_equal" '(* *))))
    (lambda (a b)
      (eq? (proc (oid->pointer a) (oid->pointer b)) 1))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_fmt

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_fromraw

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_fromstr

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_fromstrn

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_fromstrp

(define oid-zero?
  (let ((proc (libgit2->procedure int "git_oid_iszero" '(*))))
    (lambda (id)
      (eq? (proc (oid->pointer id)) 1))))

(define oid-ncmp?
  (let ((proc (libgit2->procedure int "git_oid_ncmp" `(* * ,size_t))))
    (lambda (a b len)
      (eq? (proc (oid->pointer a) (oid->pointer b) len) 0))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_nfmt

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_pathfmt

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_shorten_add

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_shorten_free

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_shorten_new

(define oid-strcmp
  (let ((proc (libgit2->procedure int "git_oid_strcmp" '(* *))))
    (lambda (id string)
      (proc (oid->pointer id) (string->pointer string)))))

(define oid-str=?
  (let ((proc (libgit2->procedure int "git_oid_streq" '(* *))))
    (lambda (id string)
      (proc (oid->pointer id) (string->pointer string)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_tostr

(define oid->string
  (let ((proc (libgit2->procedure '* "git_oid_tostr_s" '(*))))
    (lambda (id)
      (pointer->string (proc (oid->pointer id))))))

;;; FIXME: oidarray https://libgit2.github.com/libgit2/#HEAD/group/oidarray
