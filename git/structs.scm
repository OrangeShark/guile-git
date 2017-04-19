;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (git structs)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module ((system foreign) #:select (bytevector->pointer
                                           make-pointer
                                           pointer->bytevector
                                           pointer->string))
  #:use-module (bytestructures guile)
  #:export (time->pointer pointer->time time-time time-offset
            signature->pointer pointer->signature signature-name signature-email signature-when
            oid? oid->pointer pointer->oid make-oid-pointer))


;;; bytestructures helper

(define bytestructure->pointer
  (compose bytevector->pointer bytestructure-bytevector))

(define (pointer->bytestructure pointer struct)
  (make-bytestructure (pointer->bytevector pointer (bytestructure-descriptor-size struct))
                      0
                      struct))

;;; git-time

(define %time (bs:struct `((time ,int64) ;; time in seconds since epoch
                           (offset ,int)))) ;; timezone offset, in minutes

(define-record-type <time>
  (%make-time bytestructure)
  time?
  (bytestructure time-bytestructure))

(define (pointer->time pointer)
  (%make-time (pointer->bytestructure pointer %time)))

(define (time->pointer time)
  (bytestructure->pointer (time-bytestructure time)))

(define (time-time time)
  (bytestructure-ref (time-bytestructure time) 'time))

(define (time-offset time)
  (bytestructure-ref (time-bytestructure time) 'offset))

;;; git-signature

(define %signature (bs:struct `((name ,(bs:pointer uint8)) ;; char *
                                (email ,(bs:pointer uint8)) ;; char *
                                (when ,%time))))

(define-record-type <signature>
  (%make-signature bytestructure)
  signature?
  (bytestructure signature-bytestructure))

(define (pointer->signature pointer)
  (%make-signature (pointer->bytestructure pointer %signature)))

(define (signature->pointer signature)
  (bytestructure->pointer (signature-bytestructure signature)))

(define (signature-name signature)
  (pointer->string (make-pointer (bytestructure-ref (signature-bytestructure signature) 'name))))

(define (signature-email signature)
  (pointer->string (make-pointer (bytestructure-ref (signature-bytestructure signature) 'email))))

(define (signature-when signature)
  (let ((when* (bytestructure-ref (signature-bytestructure signature) 'when)))
    (%make-time when*)))

;;; git oid

(define-syntax GIT-OID-RAWSZ
  (identifier-syntax 20))

(define-record-type <oid>
  (%make-oid bytevector)
  oid?
  (bytevector oid-bytevector))

(define (pointer->oid pointer)
  (%make-oid (pointer->bytevector pointer GIT-OID-RAWSZ)))

(define (oid->pointer oid)
  (bytevector->pointer (oid-bytevector oid)))

(define (make-oid-pointer)
  (bytevector->pointer (make-bytevector GIT-OID-RAWSZ)))
