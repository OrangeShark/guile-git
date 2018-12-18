;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (tests submodule)
  #:use-module (tests helpers)
  #:use-module (git)
  #:use-module (srfi srfi-64))


(test-begin "submodule")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "repository-submodules, empty"
    '()
    (repository-submodules (repository-open directory)))

  (test-equal "lookup-submodule, not found"
    #f
    (submodule-lookup (repository-open directory) "does-not-exist"))

  (test-equal "submodule-add"
    '("submod")
    ;; Add a submodule, which includes making a checkout of it, and make sure
    ;; it is visible.
    (let* ((repository     (repository-open directory))
           (head           (reference-target (repository-head repository)))
           (submodule-file (string-append directory "/.gitmodules")))
      (and (not (file-exists? submodule-file))
           (not (file-exists? (string-append directory "/submod")))
           (let* ((url       (string-append "file://"
                                            (canonicalize-path directory)))
                  (submodule (submodule-add-setup repository url "submod"))
                  (subrepo   (repository-open (string-append directory
                                                             "/submod"))))
             (remote-fetch (remote-lookup subrepo "origin"))
             (reset subrepo (object-lookup subrepo head)
                    RESET_HARD)
             (submodule-add-finalize submodule)
             (and (file-exists? submodule-file)
                  (repository-submodules (repository-open directory)))))))

  (test-equal "lookup-submodule"
    '("submod" "submod")
    (let* ((repository (repository-open directory))
           (submodule  (submodule-lookup repository "submod")))
      (and (eq? repository (submodule-owner submodule))
           (list (submodule-name submodule)
                 (submodule-path submodule)))))

  (test-assert "submodule-update"
    (let* ((repository  (repository-open directory))
           (head        (reference-target (repository-head repository)))
           (head^       (commit-id (commit-parent
                                    (commit-lookup repository head))))
           (submodule   (submodule-lookup repository "submod"))
           (child       (repository-open (string-append directory "/submod"))))
      ;; Force the sub-repo to HEAD^.
      (reset child (object-lookup child head^) RESET_HARD)
      (and (oid=? head^ (reference-target (repository-head child)))
           (begin
             ;; Now update the submodule.
             (submodule-update submodule)
             (oid=? head (reference-target (repository-head child))))))))

(libgit2-shutdown!)

(test-end)
