;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (git bindings)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (git config)
  #:use-module (git types)
  #:use-module (git structs)
  #:export (libgit2->pointer
            libgit2->procedure
            raise-git-error
            libgit2->procedure*
            make-buffer
            free-buffer
            buffer-content
            buffer-content/string
            make-path
            pointer->object!))

;; DRAFT!

(define path-separator ":")

(define (make-path dirs)
  (reduce (lambda (dir path) (string-append path path-separator dir))
          %null-pointer
          dirs))

(define (libgit2->pointer name)
  (catch #t
    (lambda ()
      (dynamic-func name (dynamic-link %libgit2)))
    (lambda args
      (lambda _
        (throw 'system-error name  "~A" (list (strerror ENOSYS))
               (list ENOSYS))))))

(define (libgit2->procedure return name params)
  (catch #t
    (lambda ()
      (let ((ptr (dynamic-func name (dynamic-link %libgit2))))
        (pointer->procedure return ptr params)))
    (lambda args
      (lambda _
        (throw 'system-error name  "~A" (list (strerror ENOSYS))
               (list ENOSYS))))))

(define last-git-error
  (let ((proc (libgit2->procedure '* "giterr_last" '())))
    (lambda (code)
      "Return a <git-error> structure representing the last error, or #f."
      (pointer->git-error (proc) code))))

(define (raise-git-error code)
  "Raise a 'git-error' exception for the given code."
  (throw 'git-error (last-git-error code)))

(define-inlinable (libgit2->procedure* name params)
  (let ((proc (libgit2->procedure int name params)))
    (lambda args
      (let ((ret (apply proc args)))
        (unless (zero? ret)
          (raise-git-error ret))))))

;;; git-buf

(define %buffer-struct                            ;git_buf
  (list '* size_t size_t))

(define (make-buffer)
  (make-c-struct %buffer-struct `(,%null-pointer 0 0)))

(define free-buffer
  (libgit2->procedure void "git_buf_free" '(*)))

(define (buffer-content buf)
  (match (parse-c-struct buf %buffer-struct)
    ((pointer asize size)
     (pointer->bytevector pointer size))))

(define (buffer-content/string buf)
  (match (parse-c-struct buf %buffer-struct)
    ((pointer asize size)
     (pointer->string pointer size "UTF-8"))))


;;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/config

;;; FIXME: descript_commit https://libgit2.github.com/libgit2/#HEAD/group/describe

;;; FIXME: diff https://libgit2.github.com/libgit2/#HEAD/group/diff

(define diff-free
  (let ((proc (libgit2->procedure void "git_diff_free" '(*))))
    (lambda (diff)
      (proc (diff->pointer diff)))))

(define diff-get-delta
  (let ((proc (libgit2->procedure '* "git_diff_get_delta" '(*))))
    (lambda (diff)
      (pointer->diff-delta (proc (diff->pointer diff))))))

(define diff-num-deltas
  (let ((proc (libgit2->procedure size_t "git_diff_num_deltas" '(*))))
    (lambda (diff)
      (proc (diff->pointer diff)))))

;;; FIXME: fetch https://libgit2.github.com/libgit2/#HEAD/group/fetch

;;; FIXME: filter https://libgit2.github.com/libgit2/#HEAD/group/filter

;;; FIXME: giterr https://libgit2.github.com/libgit2/#HEAD/group/giterr

;;; FIXME: graph https://libgit2.github.com/libgit2/#HEAD/group/graph

;;; FIXME: hashsig https://libgit2.github.com/libgit2/#HEAD/group/hashsig

;;; FIXME: ignore https://libgit2.github.com/libgit2/#HEAD/group/ignore

;;; FIXME: index https://libgit2.github.com/libgit2/#HEAD/group/index

;;; FIXME: indexer https://libgit2.github.com/libgit2/#HEAD/group/indexer

;;; libgit2

(define libgit2-features
  (libgit2->procedure int "git_libgit2_features" '()))

(define-public libgit2-init!
  (libgit2->procedure int "git_libgit2_init" '()))

(define libgit2-opts
  (libgit2->procedure int "git_libgit2_init" `(,int)))

(define-public libgit2-shutdown!
  (libgit2->procedure int "git_libgit2_shutdown" '()))

(define libgit2-version
  (let ((proc (libgit2->procedure void "git_libgit2_version" '(* * *))))
    (lambda ()
      (let ((major (make-double-pointer))
      (minor (make-double-pointer))
      (rev (make-double-pointer)))
        (proc major minor rev)
        (map (compose pointer-address dereference-pointer) (list major minor rev))))))

(define %object-free (libgit2->pointer "git_object_free"))

(define (pointer->object! pointer)
  (set-pointer-finalizer! pointer %object-free)
  (pointer->object pointer))

;;; FIXME: mempack https://libgit2.github.com/libgit2/#HEAD/group/mempack

;;; FIXME: merge https://libgit2.github.com/libgit2/#HEAD/group/merge

;;; FIXME: message https://libgit2.github.com/libgit2/#HEAD/group/message

;;; FIXME: note https://libgit2.github.com/libgit2/#HEAD/group/note

;;; object https://libgit2.github.com/libgit2/#HEAD/group/object



;;; FIXME: odb https://libgit2.github.com/libgit2/#HEAD/group/odb

;;; FIXME: packbuilder https://libgit2.github.com/libgit2/#HEAD/group/packbuilder

;;; FIXME: patch https://libgit2.github.com/libgit2/#HEAD/group/patch

(define patch-free
  (let ((proc (libgit2->procedure void "git_patch_free" '(*))))
    (lambda (patch)
      (proc (patch->pointer patch)))))

(define patch->string
  (let ((proc (libgit2->procedure* "git_patch_to_buf" '(*))))
    (lambda (patch)
      (let ((out (make-buffer)))
        (proc out (patch->pointer patch))
        (let ((out* (buffer-content/string out)))
          (free-buffer out)
          out*)))))

;;; FIXME: pathspec https://libgit2.github.com/libgit2/#HEAD/group/pathspec

;;; FIXME: proxy https://libgit2.github.com/libgit2/#HEAD/group/proxy

;;; FIXME: push https://libgit2.github.com/libgit2/#HEAD/group/push

;;; FIXME: rebase https://libgit2.github.com/libgit2/#HEAD/group/rebase

;;; FIXME: refdb https://libgit2.github.com/libgit2/#HEAD/group/refdb


;;; FIXME: reflog https://libgit2.github.com/libgit2/#HEAD/group/reflog

;;; FIXME: refspec https://libgit2.github.com/libgit2/#HEAD/group/refspec

;;; FIXME: remote https://libgit2.github.com/libgit2/#HEAD/group/remote
