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

(define-module (git auth)
  #:use-module (srfi srfi-9)
  #:export (%make-auth-ssh-credentials
            auth-ssh-credentials?
            auth-ssh-credentials-public-key
            auth-ssh-credentials-private-key

            %make-auth-ssh-agent
            auth-ssh-agent?))

(define-record-type <auth-ssh-credentials>
  (%make-auth-ssh-credentials public-key private-key)
  auth-ssh-credentials?
  (public-key    auth-ssh-credentials-public-key)
  (private-key   auth-ssh-credentials-private-key))

(define-record-type <auth-ssh-agent>
  (%make-auth-ssh-agent)
  auth-ssh-agent?)
