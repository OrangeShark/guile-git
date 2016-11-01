;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche <amirouche@hypermove.net>
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

(define-module (git web querystring)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (querystring))

(define (string-replace string char value)
  (string-map (lambda (<>) (if (eq? <> char) value <>)) string))

(define (spacy string) (string-replace string #\+ #\space))

(define (hex->char a b)
  (integer->char (string->number (list->string (list a b)) 16)))

(define (unhex string)
  (if (not (string-index string #\%))
      string
      (let loop ((chars (string->list string))
                 (out '()))
        (match chars
          ((#\% a b . rest) (loop rest (cons (hex->char a b) out)))
          ((a . rest) (loop rest (cons a out)))
          (() (list->string (reverse out)))))))

(define decode (compose unhex spacy))

(define (acons-list k v alist)
  (let ((value (assoc-ref alist k)))
    (if value
        (let ((alist (alist-delete k alist)))
          (acons k (cons v value) alist))
        (acons k (list v) alist))))

(define (list->alist lst)
  (let next ((lst lst)
             (out '()))
    (if (null? lst)
        out
        (next (cdr lst) (acons-list (caar lst) (cdar lst) out)))))

(define (querystring bv)
  ;; semi-colon and amp can be used as pair separator
  (let* ((string (utf8->string bv))
         (pairs (map (cut string-split <> #\=)
                     (append-map (cut string-split <> #\;) (string-split string #\&)))))
    (list->alist (map (match-lambda ((key value) (cons (decode key) (decode value))))
                      pairs))))
