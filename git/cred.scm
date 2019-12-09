;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:export (CREDTYPE-USERPASS-PLAINTEXT
            CREDTYPE-SSH-KEY
            CREDTYPE-SSH-CUSTOM
            CREDTYPE-SSH-DEFAULT
            CREDTYPE-SSH-INTERACTIVE
            CREDTYPE-SSH-USERNAME
            CREDTYPE-SSH-MEMORY

            cred-default-new
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

(define CREDTYPE-USERPASS-PLAINTEXT 1)
(define CREDTYPE-SSH-KEY 2)
(define CREDTYPE-SSH-CUSTOM 4)
(define CREDTYPE-SSH-DEFAULT 8)
(define CREDTYPE-SSH-INTERACTIVE 16)
(define CREDTYPE-SSH-USERNAME 32)
(define CREDTYPE-SSH-MEMORY 64)

(define cred-default-new
  (let ((proc (libgit2->procedure* "git_cred_default_new" '(*))))
    (lambda (cred-double-pointer)
      (proc cred-double-pointer))))

(define cred-free
  (let ((proc (libgit2->procedure void "git_cred_free" '(*))))
    (lambda (cred-double-pointer)
      (proc cred-double-pointer))))

(define cred-has-username?
  (let ((proc (libgit2->procedure int "git_cred_has_username" '(*))))
    (lambda (cred-double-pointer)
      (eq? (proc cred-double-pointer) 1))))

(define cred-ssh-custom-new
  (let ((proc (libgit2->procedure int "git_cred_ssh_custom_new"
                                  `(* * * ,size_t * *))))
    (lambda (cred-double-pointer username publickey sign-callback)
      (proc cred-double-pointer
            (string->pointer username)
            (string->pointer publickey)
            (string-length publickey)
            (procedure->pointer
             int
             (lambda (session sig sig-len data data-len abstract)
               (sign-callback session sig data abstract))
             '(* * * * * *))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/cred/git_cred_ssh_interactive_new

(define cred-ssh-key-from-agent
  (let ((proc (libgit2->procedure int "git_cred_ssh_key_from_agent"
                                  '(* *))))
    (lambda (cred-double-pointer username)
      (proc cred-double-pointer (string->pointer username)))))

(define cred-ssh-key-from-memory-new
  (let ((proc (libgit2->procedure int "git_cred_ssh_key_memory_new"
                                  '(* * * * *))))
    (lambda (cred-double-pointer username publickey privatekey passphrase)
      (proc cred-double-pointer
            (string->pointer username)
            (string->pointer publickey)
            (string->pointer privatekey)
            (string->pointer passphrase)))))

(define cred-ssh-key-new
  (let ((proc (libgit2->procedure int "git_cred_ssh_key_new" '(* * * * *))))
    (lambda (cred-double-pointer username publickey privatekey passphrase)
      (proc cred-double-pointer
            (string->pointer username)
            (string->pointer publickey)
            (string->pointer privatekey)
            (string->pointer passphrase)))))

(define cred-username-new
  (let ((proc (libgit2->procedure int "git_cred_username_new" '(* *))))
    (lambda (cred-double-pointer username)
      (proc cred-double-pointer (string->pointer username)))))

(define cred-userpass
  (let ((proc (libgit2->procedure int "git_cred_userpass"
                                  `(* * * ,unsigned-int *))))
    (lambda (cred-double-pointer url user-from-url allowed-types)
      (proc cred-double-pointer
            (string->pointer url)
            (string->pointer user-from-url)
            allowed-types
            %null-pointer))))

(define cred-userpass-paintext-new
  (let ((proc (libgit2->procedure int "git_cred_userpass_plaintext_new"
                                  '(* * *))))
    (lambda (cred-double-pointer username password)
      (proc cred-double-pointer
            (string->pointer username)
            (string->pointer password)))))

(define cred-acquire-cb
  (lambda (callback)
    (procedure->pointer
     int
     (lambda (cred url username allowed payload)
       (callback cred url username allowed payload))
    `(* * * ,unsigned-int *))))
