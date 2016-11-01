;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Amirouche <amirouche@hypermove.net>
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

(define-module (git web)
  #:use-module (git web html)
  #:use-module (git web mime-types)
  #:use-module (git web querystring)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)  
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web server)
  #:use-module (web uri))

(define (request-path-components request)
  "Split the URI path of REQUEST into a list of component strings.  For
example: \"/foo/bar\" yields '(\"foo\" \"bar\")."
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (render-html sxml)
  (values '((content-type . (text/html)))
          (lambda (port)
            (sxml->html sxml port))))

(define (not-found uri)
  (values (build-response #:code 404)
          (string-append "Resource not found: " uri)))

(define (redirect uri)
  (values (build-response #:code 303 #:headers `((Location . ,uri))) ""))

(define (error)
  (values (build-response #:code 500)
          "Server error"))


;;;
;;; template
;;;

(define (template body-class body)
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (title "hypermove")
      (link (@ (rel "stylesheet") (href "/static/normalize.css")))
      (link (@ (rel "stylesheet") (href "/static/main.css"))))
     (body (@ (class ,body-class))
           (div (h1 "guile-git web"))
           (div (@ (id "container"))
                ,body)))))

;;;
;;; static assets rendering
;;;

(define (directory? filename)
  (string=? filename (dirname filename)))

(define (render-static-asset path)
  (let ((filename (string-join (cons* (dirname (current-filename)) "web/static" path) "/")))
    (if (and (file-exists? filename) (not (directory? filename)))
        (values `((content-type ,(mime-type filename)))
                (call-with-input-file filename get-bytevector-all))
        (not-found (string-join (cons "static" path) "/" 'prefix)))))


;;; route context

(define (make-context request body)
  "Create a route context"
  (let ((context `((request . ,request))))
    (let ((context (if body (acons 'POST (querystring body) context) context)))
      (if (uri-query (request-uri request))
          (acons 'GET (querystring (string->utf8 (uri-query (request-uri request)))) context)
          context))))

(define (context-post context key)
  "Get KEY from CONTEXT"
  (assoc-ref (assoc-ref context 'POST) key))

(define (context-get context key)
  "Get KEY from CONTEXT"
  (assoc-ref (assoc-ref context 'GET) key))

(define (context-method context)
  "get request method from CONTEXT"
  (request-method (assoc-ref context 'request)))

(define (handler request body)
  (let ((context (make-context request body)))
    (match (request-path-components request)
      (() (render-html (template "index" "Hello World")))
      (("static" path ...) (render-static-asset path))
      (_ (render-static-asset (list "index.html"))))))

(format #t "server running on http://localhost:8080")
(run-server handler)
