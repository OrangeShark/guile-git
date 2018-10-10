;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests oid)
  #:use-module (srfi srfi-64)
  #:use-module (tests helpers)
  #:use-module (git)
  #:use-module (git object))

(test-begin "oid")

(libgit2-init!)

(with-repository "simple" directory

  (test-assert "oid=?"
    (let* ((repository (repository-open directory))
           (oid        (reference-target (repository-head repository)))
           (head       (commit-lookup repository oid))
           (head^      (commit-parent head)))
      (and (oid=? oid oid)
           (oid=? oid (reference-target (repository-head repository)))
           (not (oid=? (commit-id head^) oid)))))

  (test-assert "object-lookup"
    (let* ((repository (repository-open directory))
           (oid        (reference-target (repository-head repository)))
           (short      (string->oid (string-take (oid->string oid) 7)))
           (obj1       (object-lookup repository oid))
           (obj2       (object-lookup-prefix repository short 7)))
      (and obj1
           (eq? obj1 obj2)
           (eqv? OBJ-COMMIT (object-type obj1))
           (oid=? oid (object-id obj1))))))

(test-end)
