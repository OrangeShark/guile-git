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
  #:use-module (git web http)
  #:use-module (git web repository)
  #:use-module (git web template)
  #:use-module (git web config)
  #:use-module (git bindings)
  #:use-module (git repository)
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
  #:use-module (web uri)
  #:export (run-gitweb))

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
;;; static assets rendering
;;;

(define (directory? filename)
  (string=? filename (dirname filename)))

(define (render-static-asset path)
  (let ((filename (string-join (cons* (static-dir) "static" path) "/")))
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

(define (render-repo-list)
  (define (description repo)
    "no description")

  (define (valid-repo? name)
    (openable-repository? (string-append (repository-dir) "/" name)))

  (define (render-repo-div repo)
    `(a (@ (href ,(string-append "/" repo)))
        (h3 ,repo)
        (p ,(description repo))))

  (let ((maybe-repos (scandir (repository-dir))))
    `(div (@ (class "repositories"))
          ,(map render-repo-div (filter valid-repo? maybe-repos)))))

(define (render-index)
  (respond (render-repo-list)
           #:title "guile git"
           #:template (cut main-template <> <> "index")))

(define (handler request body)
  (let ((context (make-context request body)))
    (match (request-path-components request)
      (() (render-index))
      (("static" path ...) (render-static-asset path))
      ((repo path ...) (repo-handler repo path))
      (_ (render-static-asset (list "index.html"))))))

(define* (run-gitweb #:key (port 8080))
  (format #t "server running on http://localhost:~d" port)
  (newline)
  (libgit2-init!)
  (run-server (lambda args (apply handler args))
              'http `(#:port , port)))
