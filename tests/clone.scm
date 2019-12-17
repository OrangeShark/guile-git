;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (tests clone)
  #:use-module (git)
  #:use-module (tests helpers)
  #:use-module (tests ssh)
  #:use-module (srfi srfi-64))

(test-begin "clone")

(libgit2-init!)

(define (make-ssh-url dir port)
  (format #f "ssh://localhost:~a/~a" port dir))

(define ssh-server-port 8899)

(define (clone-test directory auth-method)
  (let* ((repo-dir (in-vicinity (getcwd) directory))
         (clone-dir (in-vicinity repo-dir "out")))
    (clone (make-ssh-url repo-dir ssh-server-port)
           clone-dir
           (make-clone-options #:fetch-options
                               (make-fetch-options auth-method)))
    (let* ((repository (repository-open clone-dir))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-id (commit-lookup repository oid))))))

(if (not (sshd-available?))
    (test-skip)
    (with-sshd-server
     ssh-server-port
     (with-repository "simple-bare" directory
       (test-equal "clone-auth-ssh-credentials"
         "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
         (clone-test directory (make-client-ssh-auth))))

     (with-repository "simple-bare" directory
       (test-equal "clone-auth-ssh-agent"
         "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
         (with-ssh-agent
          (clone-test directory (%make-auth-ssh-agent)))))

     (with-repository "simple-bare" directory
       (test-assert "clone-and-fetch-auth-ssh-credentials"
         (let* ((auth (make-client-ssh-auth))
                (do-clone (clone-test directory auth))
                (clone-dir (in-vicinity directory "out"))
                (repository (repository-open clone-dir))
                (remote (remote-lookup repository "origin")))
           (remote-fetch remote #:fetch-options (make-fetch-options auth))
           #t)))))

(libgit2-shutdown!)

(test-end)
