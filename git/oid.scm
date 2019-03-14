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

(define-module (git oid)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:use-module (srfi srfi-9 gnu)
  #:re-export (oid=?)
  #:export (oid-cmp
            string->oid
            oid-zero?
            oid-ncmp?
            oid-strcmp
            oid-str=?
            oid->string))

;;; oid https://libgit2.github.com/libgit2/#HEAD/group/oid

(define (oid-cmp a b)
  (let ((proc (libgit2->procedure int "git_oid_cmp" '(* *))))
    (proc (oid->pointer a) (oid->pointer b))))

(define (oid-copy src)
  (let ((proc (libgit2->procedure void "git_oid_cpy" '(* *)))
        (out (make-oid-pointer)))
    (proc out (oid->pointer src))
    (pointer->oid out)))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_fmt

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_fromraw

(define (string->oid str)
  (let ((proc (libgit2->procedure* "git_oid_fromstrn" `(* * ,size_t)))
        (out (make-oid-pointer)))
    (proc out (string->pointer str "US-ASCII") (string-length str))
    (pointer->oid out)))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_fromstrp

(define (oid-zero? id)
  (let ((proc (libgit2->procedure int "git_oid_iszero" '(*))))
    (eq? (proc (oid->pointer id)) 1)))

(define (oid-ncmp? a b len)
  (let ((proc (libgit2->procedure int "git_oid_ncmp" `(* * ,size_t))))
    (eq? (proc (oid->pointer a) (oid->pointer b) len) 0)))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_nfmt

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_pathfmt

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_shorten_add

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_shorten_free

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_shorten_new

(define (oid-strcmp id string)
  (let ((proc (libgit2->procedure int "git_oid_strcmp" '(* *))))
    (proc (oid->pointer id) (string->pointer string))))

(define (oid-str=? id string)
  (let ((proc (libgit2->procedure int "git_oid_streq" '(* *))))
    (proc (oid->pointer id) (string->pointer string))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/oid/git_oid_tostr

(define (oid->string id)
  (let ((proc (libgit2->procedure '* "git_oid_tostr_s" '(*))))
    (pointer->string (proc (oid->pointer id)))))

(define (print-oid oid port)
  (format port "#<oid ~a>" (oid->string oid)))

(set-record-type-printer! (@@ (git structs) <oid>) print-oid)

;;; FIXME: oidarray https://libgit2.github.com/libgit2/#HEAD/group/oidarray
