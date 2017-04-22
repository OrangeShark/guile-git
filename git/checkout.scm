;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Amirouche Boubekki <amirouche@hypermove.net>
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
  #:use-module (rnrs arithmetic bitwise)
  #:use-module ((system foreign) #:prefix ffi:)
  #:use-module (bytestructures guile)
  #:use-module (git bindings)
  #:use-module (git structs)
  #:use-module (git strarray)
  #:use-module (git types))

(define << bitwise-arithmetic-shift)

;; git_checkout_strategy_t

(define-public GIT-CHECKOUT-NONE 0)
(define-public GIT-CHECKOUT-SAFE (<< 1 0))
(define-public GIT-CHECKOUT-FORCE (<< 1 1))
(define-public GIT-CHECKOUT-RECREATE-MISSING (<< 1 2))
(define-public GIT-CHECKOUT-ALLOW-CONFLICTS (<< 1 4))
(define-public GIT-CHECKOUT-REMOVE-UNTRACKED (<< 1 5))
(define-public GIT-CHECKOUT-IGNORED (<< 1 6))
(define-public GIT-CHECKOUT-UPDATE-ONLY (<< 1 7))
(define-public GIT-CHECKOUT-DONT-UPDATE-INDEX (<< 1 8))
(define-public GIT-CHECKOUT-NO-REFERSH (<< 1 9))
(define-public GIT-CHECKOUT-SKIP-UNMERGED (<< 1 10))
(define-public GIT-CHECKOUT-USE-OURS (<< 1 11))
(define-public GIT-CHECKOUT-USE-THEIRS (<< 1 12))
(define-public GIT-CHECKOUT-DISABLE-PATHSPEC-MATCH (<< 1 13))
(define-public GIT-CHECKOUT-SKIP-LOCKED-DIRECTORIES (<< 1 18))
(define-public GIT-CHECKOUT-DONT-OVERWRITE-IGNORED (<< 1 19))
(define-public GIT-CHECKOUT-CONFLICT-STYLE-MERGE (<< 1 20))
(define-public GIT-CHECKOUT-CONFLICT-STYLE-DIFF3 (<< 1 21))
(define-public GIT-CHECKOUT-DONT-REMOVE-EXISTING (<< 1 22))
(define-public GIT-CHECKOUT-DONT-WRITE-INDEX (<< 1 23))

;; The following options are not yet implemented
(define GIT-CHECKOUT-UPDATE-SUBMODULES (<< 1 16))
(define GIT-CHECKOUT-UPDATE-SUBMODULES-IF-CHANGED (<< 1 17))

;; git_checkout_notify_t

(define-public GIT-CHECKOUT-NOTIFY-NONE 0)
(define-public GIT-CHECKOUT-NOTIFY-CONFLICT (<< 1 0))
(define-public GIT-CHECKOUT-NOTIFY-DIRTY (<< 1 1))
(define-public GIT-CHECKOUT-NOTIFY-UPDATED (<< 1 2))
(define-public GIT-CHECKOUT-NOTIFY-UNTRACKED (<< 1 3))
(define-public GIT-CHECKOUT-NOTIFY-IGNORED (<< 1 4))

(define-public GIT-CHECKOUT-NOTIFY-ALL #xFFFF)


;; git_checkout_perfdata

(define %checkout-perfdata (bs:struct `((mkdir-calls ,size_t)
                                        (stat-calls ,size_t)
                                        (chmod-calls ,size_t))))



;; git_checkout_options

(define %checkout-options (bs:struct `((version ,unsigned-int)
                                       (checkout-strategy ,unsigned-int)
                                       (disable-filters ,int)
                                       (dir-mode ,unsigned-int)
                                       (file-mode ,unsigned-int)
                                       (file-open-flags ,int)
                                       (notify-flags ,unsigned-int)
                                       (notify-cb ,address)
                                       (notify-payload ,address)
                                       (progress-cb ,address)
                                       (progress-payload ,address)
                                       (paths ,%strarray)
                                       (baseline ,address)
                                       (baseline-index ,address)
                                       (target-directory ,address)
                                       (ancestor-label ,address)
                                       (our-label ,address)
                                       (their-label ,address)
                                       (perfdata-cb ,address)
                                       (perfdata-payload ,address))))


;;; checkout https://libgit2.github.com/libgit2/#HEAD/group/checkout

(define-public checkout-head
  (let ((proc (libgit2->procedure* "git_checkout_head" '(* *))))
    (lambda* (repository #:key options)
      (let ((options* (if options (bytestructure->pointer options) ffi:%null-pointer)))
        (proc (repository->pointer repository) options*)))))

(define-public checkout-index
  (let ((proc (libgit2->procedure* "git_checkout_index" '(* * *))))
    (lambda* (repository #:key index options)
      (let ((index* (if index (index->pointer index) ffi:%null-pointer))
            (options* (if options (bytestructure->pointer options) ffi:%null-pointer)))
        (proc (repository->pointer repository) index* options*)))))

(define GIT-CHECKOUT-OPTION-VERSION 1)

(define-public checkout-init-options
  (let ((proc (libgit2->procedure* "git_checkout_init_options" '(* ,uint))))
    (lambda ()
      (let ((options (bytestructure %checkout-options)))
        (proc (bytestructure->pointer options) GIT-CHECKOUT-OPTION-VERSION)
        options))))

(define-public checkout-tree
  (let ((proc (libgit2->procedure* "git_checkout_tree" `(* * *))))
    (lambda* (repository #:key treeish options)
      (let ((treeish* (if treeish (object->pointer treeish) ffi:%null-pointer))
            (options* (if options (bytestructure->pointer options) ffi:%null-pointer)))
      (proc (repository->pointer repository) treeish* options*)))))
