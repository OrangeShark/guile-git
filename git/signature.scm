;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2019 Marius Bakke <marius@devup.no>
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

(define-module (git signature)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (signature-default
            signature-new
            signature-now))

;;; https://libgit2.org/libgit2/#HEAD/group/signature

(define %signature-free (libgit2->pointer "git_signature_free"))

(define (pointer->signature! pointer)
  (set-pointer-finalizer! pointer %signature-free)
  (pointer->signature pointer))

;; Return a <signature> for the current user in REPOSITORY.  Throws an error
;; if no configuration is found.
(define signature-default
  (let ((proc (libgit2->procedure* "git_signature_default" '(* *))))
    (lambda (repository)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository))
        (pointer->signature! (dereference-pointer out))))))

(define signature-new
  (let ((proc (libgit2->procedure* "git_signature_new" `(* * * ,int64 ,int))))
    (lambda (name email time offset)
      (let ((out (make-double-pointer)))
        (proc out
              (string->pointer name)
              (string->pointer email)
              time offset)
        (pointer->signature! (dereference-pointer out))))))

(define signature-now
  (let ((proc (libgit2->procedure* "git_signature_now" '(* * *))))
    (lambda (name email)
      (let ((out (make-double-pointer)))
        (proc out
              (string->pointer name)
              (string->pointer email))
        (pointer->signature! (dereference-pointer out))))))
