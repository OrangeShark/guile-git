;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (git blob)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git structs)
  #:use-module (git types)
  #:re-export (blob?)
  #:export (blob-id
            blob-size
            blob-content

            blob-lookup
            blob-lookup-prefix))

;;; blob https://libgit2.github.com/libgit2/#HEAD/group/blob

(define blob-id
  (let ((proc (libgit2->procedure '* "git_blob_id" '(*))))
    (lambda (blob)
      (pointer->oid (proc (blob->pointer blob))))))

(define %blob-free (libgit2->pointer "git_blob_free"))

(define (pointer->blob! pointer)
  (set-pointer-finalizer! pointer %blob-free)
  (pointer->blob pointer))

(define blob-lookup
  (let ((proc (libgit2->procedure* "git_blob_lookup" `(* * *))))
    (lambda (repository oid)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) (oid->pointer oid))
        (pointer->blob! (dereference-pointer out))))))

(define blob-lookup-prefix
  (let ((proc (libgit2->procedure* "git_blob_lookup_prefix" `(* * * ,size_t))))
    (lambda (repository id len)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) (oid->pointer id) len)
        (pointer->blob! (dereference-pointer out))))))

(define blob-size
  (let ((proc (libgit2->procedure size_t "git_blob_rawsize" '(*))))
    (lambda (blob)
      "Return the size in bytes of the contents of BLOB."
      (proc (blob->pointer blob)))))

(define %blob-contents
  ;; Map a bytevector (blob content) to the blob it belongs to.
  (make-weak-value-hash-table))

(define blob-content
  (let ((proc (libgit2->procedure '* "git_blob_rawcontent" '(*))))
    (lambda (blob)
      "Return the content of BLOB as a bytevector."
      (let* ((ptr (proc (blob->pointer blob)))
             (bv  (pointer->bytevector ptr (blob-size blob))))
        ;; Since PTR belongs to BLOB, ensure BLOB is live as long as BV is.
        (hashq-set! %blob-contents bv blob)
        bv))))
