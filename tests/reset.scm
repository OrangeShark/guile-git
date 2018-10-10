;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (tests reset)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git)
             (git object))

(test-begin "reset")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "reset hard"
    #t
    (let* ((repository (repository-open directory))
           (commit "b70d89182da3b2019c3fd6755c794ee65921b4a8")
           (commit-oid (string->oid commit))
           (commit-object (object-lookup repository commit-oid))
           (ret (reset repository commit-object RESET_HARD))
           (head-oid (reference-target (repository-head repository))))
      (and (oid=? commit-oid head-oid) ;; had HEAD moved ?
           ;; directory/message is introduced by commit 3f848a
           ;; and should be removed by reseting hard to b70d89.
           (not (path-exists? "directory/message"))))))

(with-repository "simple" directory

  (test-equal "reset soft"
    #t
    (let* ((repository (repository-open directory))
           (commit "b70d89182da3b2019c3fd6755c794ee65921b4a8")
           (commit-oid (string->oid commit))
           (commit-object (object-lookup repository commit-oid))
           (ret (reset repository commit-object RESET_SOFT))
           (head-oid (reference-target (repository-head repository))))
      (and (oid=? commit-oid head-oid) ;; has HEAD moved ?
           ;; directory/message is introduced by commit 3f848a
           ;; and should be kept by reseting soft to b70d89.
           (path-exists?
            (string-append directory "/directory/message"))))))

(libgit2-shutdown!)

(test-end)
