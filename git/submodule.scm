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

(define-module (git submodule)
  #:use-module (system foreign)
  #:use-module (git bindings)
  #:use-module (git types)
  #:use-module (git errors)
  #:use-module (git structs)
  #:export (repository-submodules
            submodule?
            submodule-lookup
            submodule-name
            submodule-path
            submodule-owner
            submodule-head-id
            submodule-init
            submodule-reload
            submodule-add-setup
            submodule-add-finalize
            submodule-add-to-index
            submodule-set-branch!
            submodule-update))

;; https://libgit2.org/libgit2/#HEAD/group/submodule

(define %submodule-free (dynamic-func "git_submodule_free" libgit2))

(define (pointer->submodule! pointer)
  (set-pointer-finalizer! pointer %submodule-free)
  (pointer->submodule pointer))

(define submodule-map
  (let ((proc (libgit2->procedure* "git_submodule_foreach" '(* * *))))
    (lambda (repository callback)
      (let* ((result     '())
             (trampoline (lambda (submodule name payload)
                           ;; We can't capture SUBMODULE here because its
                           ;; lifetime is limited to the dynamic extent of
                           ;; the 'git_submodule_foreach' call.
                           (set! result
                             (cons (callback (pointer->string name))
                                   result))
                           0)))
        (proc (repository->pointer repository)
              (procedure->pointer int trampoline '(* * *))
              %null-pointer)
        (reverse result)))))

(define (repository-submodules repository)
  "Return the list of submodule names of REPOSITORY."
  (submodule-map repository identity))

(define submodule-name
  (let ((proc (libgit2->procedure '* "git_submodule_name" '(*))))
    (lambda (submodule)
      "Get the file name of SUBMODULE."
      (pointer->string (proc (submodule->pointer submodule))))))

(define submodule-path
  (let ((proc (libgit2->procedure '* "git_submodule_path" '(*))))
    (lambda (submodule)
      "Get the file name of SUBMODULE."
      (pointer->string (proc (submodule->pointer submodule))))))

(define submodule-owner
  (let ((proc (libgit2->procedure '* "git_submodule_owner" '(*))))
    (lambda (submodule)
      "Return the repository that contains SUBMODULE."
      (pointer->repository (proc (submodule->pointer submodule))))))

(define submodule-head-id
  (let ((proc (libgit2->procedure '* "git_submodule_head_id" '(*))))
    (lambda (submodule)
      "Return the OID for SUBMODULE in the current HEAD tree.  Return #f if
that information isn't available, for instance if SUBMODULE is not fully set
up."
      (let ((ptr (proc (submodule->pointer submodule))))
        (if (null-pointer? ptr)
            #f
            (pointer->oid ptr))))))

(define submodule-lookup
  (let ((proc (libgit2->procedure* "git_submodule_lookup" `(* * *))))
    (lambda (repository name)
      "Look up submodule NAME under REPOSITORY.  Return the submodule object
on success and #f if NAME could not be found."
      (let ((submodule (make-double-pointer)))
        (catch 'git-error
          (lambda ()
            (proc submodule
                  (repository->pointer repository)
                  (string->pointer name))
            (pointer->submodule! (dereference-pointer submodule)))
          (lambda (key error . rest)
            ;; For convenience return #f in the common case.
            (if (= GIT_ENOTFOUND (git-error-code error))
                #f
                (apply throw key error rest))))))))

(define submodule-init
  (let ((proc (libgit2->procedure* "git_submodule_init" `(* ,int))))
    (lambda* (submodule #:optional overwrite?)
      "Copy submodule info into \".git/config\" file, just like \"git
submodule init\"."
      (proc (submodule->pointer submodule)
            (if overwrite? 1 0)))))

(define submodule-reload
  (let ((proc (libgit2->procedure* "git_submodule_reload" `(* ,int))))
    (lambda* (submodule #:optional force?)
      "Reload SUBMODULE from '.git/config', etc."
      (proc (submodule->pointer submodule)
            (if force? 1 0)))))

(define submodule-add-setup
  (let ((proc (libgit2->procedure* "git_submodule_add_setup"
                                   `(* * * * ,int))))
    (lambda* (repository url path #:key use-gitlink?)
      "Set up a new submodule in REPOSITORY for the repository URL at PATH.
This does \"git submodule add\" up to the fetch and checkout of the submodule
contents.  It preps a new submodule, creates an entry in .gitmodules and
creates an empty initialized repository either at the given path in the
working directory or in .git/modules with a gitlink from the working
directory to the new repo."
      (let ((submodule (make-double-pointer)))
        (proc submodule
              (repository->pointer repository)
              (string->pointer url)
              (string->pointer path)
              (if use-gitlink? 1 0))
        (pointer->submodule! (dereference-pointer submodule))))))

(define submodule-add-finalize
  (let ((proc (libgit2->procedure* "git_submodule_add_finalize" '(*))))
    (lambda (submodule)
      "Resolve the setup of SUBMODULE.  This should be called on a submodule
once you have called add setup and done the clone of the submodule.  This
adds the '.gitmodules' file and the newly cloned submodule to the index to be
ready to be committed (but doesn't actually do the commit)."
      (proc (submodule->pointer submodule)))))

(define submodule-add-to-index
  (let ((proc (libgit2->procedure* "git_submodule_add_to_index" `(* ,int))))
    (lambda* (submodule #:optional (write-index? #t))
      "Add current submodule HEAD commit to index of superproject."
      (proc (submodule->pointer submodule)
            (if write-index? 1 0)))))

(define submodule-set-branch!
  (let ((proc (libgit2->procedure* "git_submodule_set_branch" '(* * *))))
    (lambda (repository name branch)
      "Change to BRANCH the branch of submodule NAME in REPOSITORY."
      (proc (repository->pointer repository)
            (string->pointer name)
            (string->pointer branch)))))

(define submodule-update
  (let ((proc (libgit2->procedure* "git_submodule_update" `(* ,int *))))
    (lambda* (submodule #:key (initialize? #t))
      "Update SUBMODULE.  This will clone it and check out the subrepository
to the commit specified in the index of the containing repository.  If
SUBMODULE doesn't contain the target commit, then the submodule is fetched using the
fetch options supplied in OPTIONS."
      (proc (submodule->pointer submodule)
            (if initialize? 1 0)
            %null-pointer))))
