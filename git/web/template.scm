;;; Guile-Git --- GNU Guile bindings of libgit2
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

(define-module (git web template)
  #:export (main-template))


;;;
;;; template
;;;

(define* (main-template title body #:optional (body-class "index") (sub-title "Projects"))
  `((doctype "html")
    (html
     (head
      (meta (@ (charset "utf-8")))
      (title ,title)
      (link (@ (rel "stylesheet") (href "/static/main.css"))))
     (body (@ (class ,body-class))
           (div (h1 ,title))
           (div (h2 ,sub-title))
           (div (@ (id "container")) ,body)
           (div (span "Powered by GNU Guile"))))))
