;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
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

(define-module (git repository)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git reference)
  #:export (repository-config
            repository-config-snapshot
            repository-detach-head
            repository-discover
            repository-get-namespace
            repository-head
            repository-head-detached?
            repository-head-unborn?
            repository-ident
            repository-index
            repository-init
            repository-is-bare?
            repository-is-empty?
            repository-is-shallow?
            repository-open
            repository-open-ext
            openable-repository?
            repository-path
            repository-refdb
            repository-set-ident
            repository-state
            repository-workdir
            pointer->repository!))

;;; repository

(define repository-config
  (let ((proc (libgit2->procedure* "git_repository_config" '(* *))))
    (lambda (repository)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository))
        (pointer->config (dereference-pointer out))))))

(define repository-config-snapshot
  (let ((proc (libgit2->procedure* "git_repository_config_snapshot" '(* *))))
    (lambda (repository)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository))
        (pointer->config (dereference-pointer out))))))

(define repository-detach-head
  (let ((proc (libgit2->procedure* "git_repository_detach_head" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

(define repository-discover
  (let ((proc (libgit2->procedure* "git_repository_discover" `(* * ,int *))))
    (lambda* (start-path #:optional (across-fs #t) ceiling-dirs)
      (let ((out (make-buffer)))
        (proc out
              (string->pointer start-path)
              (if across-fs 1 0)
              (if ceiling-dirs
                  (string->pointer ceiling-dirs)
                  %null-pointer))
        (let ((out* (buffer-content/string out)))
          (free-buffer out)
          out*)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_fetchhead_foreach

(define %repository-free (dynamic-func "git_repository_free" libgit2))

(define (pointer->repository! pointer)
  (set-pointer-finalizer! pointer %repository-free)
  (pointer->repository pointer))

(define repository-get-namespace
  (let ((proc (libgit2->procedure '* "git_repository_get_namespace" '(*))))
    (lambda (repository)
      (let ((res (proc (repository->pointer repository))))
        (if (null-pointer? res)
            #f
            (pointer->string res))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_hashfile

(define repository-head
  (let ((proc (libgit2->procedure* "git_repository_head" '(* *))))
    (lambda (repository)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository))
        (pointer->reference! (dereference-pointer out))))))

(define repository-head-detached?
  (let ((proc (libgit2->procedure int "git_repository_head_detached" '(*))))
    (lambda (repository)
      (case (proc (repository->pointer repository))
        ((0) #f)
        ((1) #t)
        (else => (lambda (code) (throw 'git-error code)))))))

(define repository-head-unborn?
  (let ((proc (libgit2->procedure int "git_repository_head_unborn" '(*))))
    (lambda (repository)
      (case (proc (repository->pointer repository))
        ((0) #f)
        ((1) #t)
        (else => (lambda (code) (throw 'git-error code)))))))

(define repository-ident
  (let ((proc (libgit2->procedure* "git_repository_ident" '(* * *))))
    (lambda (repository)
      (let* ((name (make-bytevector (sizeof '*)))
             (name* ((bytevector->pointer name)))
             (email (make-bytevector (sizeof '*)))
             (email* ((bytevector->pointer email))))
        (proc name email (repository->pointer repository))
        (values (pointer->string (make-pointer (u64vector-ref name 0)))
                (pointer->string (make-pointer (u64vector-ref email 0))))))))

(define repository-index
  (let ((proc (libgit2->procedure* "git_repository_index" '(* *))))
    (lambda (repository)
      (let ((out ((make-double-pointer))))
        (proc (repository->pointer repository))
        (pointer->index (dereference-pointer out))))))

(define repository-init
  (let ((proc (libgit2->procedure* "git_repository_init" `(* * ,int))))
    (lambda* (path #:optional (is-bare #f))
      (let ((out (make-double-pointer)))
        (proc out (string->pointer path) (if is-bare 1 0))
        (pointer->repository! (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_init_ext

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_init_init_options

(define repository-is-bare?
  (let ((proc (libgit2->procedure int "git_repository_is_bare" '(*))))
    (lambda (repository)
      (eq? (proc (repository->pointer repository)) 1))))

(define repository-is-empty?
  (let ((proc (libgit2->procedure int "git_repository_is_empty" '(*))))
    (lambda (repository)
      (eq? (proc (repository->pointer repository)) 1))))

(define repository-is-shallow?
  (let ((proc (libgit2->procedure int "git_repository_is_shallow" '(*))))
    (lambda (repository)
      (eq? (proc (repository->pointer repository)) 1))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_mergehead_foreach

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_message

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_message_remove

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_new

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_odb

(define repository-open
  (let ((proc (libgit2->procedure* "git_repository_open" '(* *))))
    (lambda (file)
      "Open repository found at FILE"
      (let ((out (make-double-pointer)))
        (proc out (string->pointer file))
        (pointer->repository! (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_open_baer

(define REPOSITORY_OPEN_NO_SEARCH (ash #b1 0))
(define REPOSITORY_OPEN_CROSS_FS  (ash #b1 1))
(define REPOSITORY_OPEN_BARE      (ash #b1 2))
(define REPOSITORY_OPEN_NO_DOTGIT (ash #b1 3))
(define REPOSITORY_OPEN_FROM_ENV  (ash #b1 4))

(define repository-open-ext
  (let ((proc (libgit2->procedure* "git_repository_open_ext" `(* * ,unsigned-int *))))
    (lambda* (directory flags #:optional ceiling-path)
      "Find and open a repository at DIRECTORY with extended controls.
DIRECTORY may be a subdirectory of the repository and searches parent
directories for the repository unless 'no-search flag is used.  DIRECTORY
may also be #f when using 'from-env flag.
FLAGS is a list of the following symbols:
* 'no-search - Do not search parent directories.
* 'cross-fs  - Search across filesystem boundaries.
* 'bare      - Open repository as a bare repo.
* 'no-.git   - Do not check by appending .git to directory.
* 'from-env  - Use git environment variables.
CEILING-PATH is a string of delimited directory names where the search
should terminate.  The delimiter is ';' on Windows and ':' on others.

Returns the repository or throws an error if no repository could be found."
      (let ((out (make-double-pointer)))
        (proc out (if directory (string->pointer directory) %null-pointer)
              (apply logior
                     (map (match-lambda
                            ('no-search REPOSITORY_OPEN_NO_SEARCH)
                            ('cross-fs  REPOSITORY_OPEN_CROSS_FS)
                            ('bare      REPOSITORY_OPEN_BARE)
                            ('no-.git   REPOSITORY_OPEN_NO_DOTGIT)
                            ('from-env  REPOSITORY_OPEN_FROM_ENV))
                          flags))
              (if ceiling-path
                  (string->pointer ceiling-path)
                  %null-pointer))
        (pointer->repository! (dereference-pointer out))))))

(define openable-repository?
  (let ((proc (libgit2->procedure* "git_repository_open_ext" `(* * ,unsigned-int *))))
    (lambda (path)
      (catch 'git-error
        (lambda ()
          (proc %null-pointer (string->pointer path) REPOSITORY_OPEN_NO_SEARCH %null-pointer)
          #t)
        (lambda _ #f)))))

(define repository-path
  (let ((proc (libgit2->procedure '* "git_repository_path" '(*))))
    (lambda (repository)
      (pointer->string (proc (repository->pointer repository))))))

(define repository-refdb
  (let ((proc (libgit2->procedure* "git_repository_refdb" `(* *))))
    (lambda (repository)
      (let ((out ((make-double-pointer))))
        (proc out (repository->pointer repository))
        (pointer->refdb (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_reinit_filesystem

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_bare

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_config

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_head

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_head_detached

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_head_detached_from_annotated

(define repository-set-ident
  (let ((proc (libgit2->procedure* "git_repository_set_ident" '(* * *))))
    (lambda (repository name email) ;;; FIXE: make name and email optional
      (proc (repository->pointer repository)
            (string->pointer name "UTF-8")
            (string->pointer email "UTF-8")))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_index

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_namespace

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_odb

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_refdb

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_set_workdir

(define repository-state
  (let ((proc (libgit2->procedure int "git_repository_state" '(*))))
    (lambda (repository)
      (proc (repository->pointer repository)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_state_cleanup

(define repository-workdir
  (let ((proc (libgit2->procedure '* "git_repository_workdir" '(*))))
    (lambda (repository)
      (pointer->string (proc (repository->pointer repository))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/repository/git_repository_wrap_odb
