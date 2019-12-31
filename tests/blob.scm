;;; Guile-Git --- GNU Guile bindings of libgit2
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

(define-module (tests blob)
  #:use-module (git)
  #:use-module (tests helpers)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match))

(test-begin "blob")

(with-repository "simple" directory

  (test-assert "blob-lookup, blob-content"
    (let* ((repository (repository-open directory))
           (head       (repository-head repository))
           (commit     (commit-lookup repository (reference-target head)))
           (tree       (commit-tree commit))
           (entry      (tree-entry-bypath tree "README"))
           (oid        (tree-entry-id entry))
           (blob       (blob-lookup repository oid)))
      (and (blob? blob)
           (oid=? (blob-id blob) oid)
           (bytevector=? (call-with-input-file (string-append directory
                                                              "/README")
                           get-bytevector-all)
                         (blob-content blob))))))

(test-end "blob")
