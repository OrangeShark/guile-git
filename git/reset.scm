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

(define-module (git reset)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (RESET_SOFT
            RESET_MIXED
            RESET_HARD
            reset))

(define RESET_SOFT  1)
(define RESET_MIXED 2)
(define RESET_HARD  3)

(define reset
  (let ((proc (libgit2->procedure* "git_reset" `(* * ,unsigned-int *))))
    (lambda (repository target type)
      (proc (repository->pointer repository)
            (object->pointer target)
            type
            ;; FIXME https://libgit2.github.com/libgit2/#HEAD/type/git_checkout_options
            %null-pointer))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/reset/git_reset_default

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/reset/git_reset_from_annotated
