;;; Guile-Git --- GNU Guile bindings of libgit2
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

(define-module (git web repository)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (git bindings)
  #:use-module (git repository)
  #:use-module (git branch)
  #:use-module (git enums)
  #:use-module (git web http)
  #:use-module (git web html)
  #:use-module (git web config)
  #:use-module (git web template)
  #:export (repo-handler))


(define (render-commits foo) foo)

(define (render-summary repo-name branches)
  (define (render-branch branch)
    (let ((name (branch-name branch)))
      `(li (div (a (@ (href ,(string-append "/" repo-name "/" name))) ,name)))))
  `(div (@ (class "branches"))
        (h2 "Branches")
        (ul ,(map render-branch branches))))


(define (render-repo-index repo-name repository)
  (let ((branches (branch-list repository GIT-BRANCH-LOCAL)))
    (respond (render-summary repo-name branches)
             #:title repo-name
             #:template (cut main-template <> <> "summary" "Summary"))))

(define (handle-repo repo-name repository path)
  (match path
    (() (render-repo-index repo-name repository))
    (rest (respond (string-append " path:"
                                  (fold (lambda (str prev) (string-append prev "/" str)) "" path))
                   #:title repo-name
                   #:template main-template))))

(define (get-repository repo)
  (false-if-exception
   (repository-open (string-append (repository-dir) "/" repo))))

(define (repo-handler repo path)
  (let ((repository (get-repository repo)))
    (if repository
       (handle-repo repo repository path)
       (respond "Repo not found"
                #:title repo
                #:status 404
                #:template main-template))))
