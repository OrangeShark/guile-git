;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (git fetch)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git cred)
  #:use-module (git structs)
  #:use-module (git types)
  #:use-module (srfi srfi-26)

  #:export (fetch-init-options
            set-fetch-auth-with-ssh-agent!
            set-fetch-auth-with-default-ssh-key!))

(define FETCH-OPTIONS-VERSION 1)

(define (fetch-init-options)
  (let ((proc (libgit2->procedure* "git_fetch_init_options" `(* ,unsigned-int)))
        (fetch-options (make-fetch-options)))
    (proc (fetch-options->pointer fetch-options) FETCH-OPTIONS-VERSION)
    fetch-options))

(define (set-fetch-auth-callback fetch-options callback)
  (let ((callbacks (fetch-options-callbacks fetch-options)))
    (set-remote-callbacks-credentials! callbacks
                                       (pointer-address callback))))

(define (set-fetch-auth-with-ssh-agent! fetch-options)
  (set-fetch-auth-callback
   fetch-options
   (cred-acquire-cb
    (lambda (cred url username allowed payload)
      (cred-ssh-key-from-agent cred
                               (pointer->string username))))))

(define (set-fetch-auth-with-default-ssh-key! fetch-options)
  (let* ((home (getenv "HOME"))
         (ssh-dir (in-vicinity home ".ssh"))
         (pub-key (in-vicinity ssh-dir "id_rsa.pub"))
         (pri-key (in-vicinity ssh-dir "id_rsa")))
    (set-fetch-auth-callback
     fetch-options
     (cred-acquire-cb
      (lambda (cred url username allowed payload)
        (cred-ssh-key-new cred
                          (pointer->string username)
                          pub-key
                          pri-key
                          ""))))))
