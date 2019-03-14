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

(define-module (git cred)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (srfi srfi-26)
  #:export (cred-default-new
            cred-free
            cred-has-username?
            cred-ssh-custom-new
            cred-ssh-key-from-agent
            cred-ssh-key-from-memory-new
            cred-ssh-key-new
            cred-username-new
            cred-userpass
            cred-userpass-paintext-new
            cred-acquire-cb))

(define (cred-default-new cred-double-pointer)
  (let ((proc (libgit2->procedure* "git_cred_default_new" '(*))))
    (proc cred-double-pointer)))

(define (cred-free cred-double-pointer)
  (let ((proc (libgit2->procedure void "git_cred_free" '(*))))
    (proc cred-double-pointer)))

(define (cred-has-username? cred-double-pointer)
  (let ((proc (libgit2->procedure int "git_cred_has_username" '(*))))
    (eq? (proc cred-double-pointer) 1)))

(define (cred-ssh-custom-new cred-double-pointer username
                             publickey sign-callback)
  (let ((proc (libgit2->procedure* "git_cred_ssh_custom_new"
                                   `(* * * ,size_t * *))))
    (proc cred-double-pointer
          (string->pointer username)
          (string->pointer publickey)
          (string-length publickey)
          (procedure->pointer int
                              (lambda (session sig sig-len data data-len abstract)
                                (sign-callback session sig data abstract))
                              '(* * * * * *)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/cred/git_cred_ssh_interactive_new

(define (cred-ssh-key-from-agent cred-double-pointer username)
  (let ((proc (libgit2->procedure int "git_cred_ssh_key_from_agent" '(* *))))
    (proc cred-double-pointer (string->pointer username))))

(define (cred-ssh-key-from-memory-new cred-double-pointer username
                                      publickey privatekey passphrase)
  (let ((proc (libgit2->procedure* "git_cred_ssh_key_memory_new" '(* * * * *))))
    (proc cred-double-pointer
          (string->pointer username)
          (string->pointer publickey)
          (string->pointer privatekey)
          (string->pointer passphrase))))

(define (cred-ssh-key-new cred-double-pointer username
                          publickey privatekey passphrase)
  (let ((proc (libgit2->procedure int "git_cred_ssh_key_new" '(* * * * *))))
    (proc cred-double-pointer
          (string->pointer username)
          (string->pointer publickey)
          (string->pointer privatekey)
          (string->pointer passphrase))))

(define (cred-username-new cred-double-pointer username)
  (let ((proc (libgit2->procedure* "git_cred_username_new" '(* *))))
    (proc cred-double-pointer (string->pointer username))))

(define (cred-userpass cred-double-pointer url user-from-url allowed-types)
  (let ((proc (libgit2->procedure* "git_cred_userpass" `(* * * ,unsigned-int *))))
    (proc cred-double-pointer
          (string->pointer url)
          (string->pointer user-from-url)
          allowed-types
          %null-pointer)))

(define (cred-userpass-paintext-new cred-double-pointer username password)
  (let ((proc (libgit2->procedure* "git_cred_userpass_plaintext_new" '(* * *))))
    (proc cred-double-pointer
          (string->pointer username)
          (string->pointer password))))

(define (cred-acquire-cb callback)
  (procedure->pointer
   int
   (lambda (cred url username allowed payload)
     (callback cred url username allowed payload))
   `(* * * ,unsigned-int *)))
