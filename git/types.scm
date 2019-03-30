;;; Guile-Git --- GNU Guile bindings of libgit2
;;; Copyright © 2016 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2016, 2017 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2018 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Marius Bakke <marius@devup.no>
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

(define-module (git types)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (annotated-commit? pointer->annotated-commit  annotated-commit->pointer
            blame? pointer->blame blame->pointer
            blame-options? pointer->blame-options blame-options->pointer
            blob? pointer->blob blob->pointer
            branch-iterator? pointer->branch-iterator branch-iterator->pointer
            checkout-options? pointer->checkout-options branch-iterator->pointer
            commit? pointer->commit commit->pointer
            config? pointer->config config->pointer
            cred? pointer->cred cred->pointer
            describe-result? pointer->describe-result describe-result->pointer
            diff? pointer->diff diff->pointer
            diff-delta? pointer->diff-delta diff-delta->pointer
            diff-options? pointer->diff-options diff-options->pointer
            index? pointer->index index->pointer
            object? pointer->object object->pointer
            patch? pointer->patch patch->pointer
            refdb? pointer->refdb refdb->pointer
            reference? pointer->reference reference->pointer
            reference-iterator? pointer->reference-iterator reference-iterator->pointer
            repository? pointer->repository repository->pointer
            remote? pointer->remote remote->pointer
            status-list? pointer->status-list status-list->pointer
            tag? pointer->tag tag->pointer
            tree? pointer->tree tree->pointer
            tree-entry? pointer->tree-entry tree-entry->pointer
            submodule? pointer->submodule submodule->pointer
            pointer->size_t
            make-size_t-pointer
            make-double-pointer))


(define-syntax define-libgit2-type
  (lambda (s)
    "Define a wrapped pointer type for an opaque type of libgit2."
    (syntax-case s ()
      ((_ name)
       (let ((symbol     (syntax->datum #'name))
             (identifier (lambda (symbol)
                           (datum->syntax #'name symbol))))
         (with-syntax ((rtd    (identifier (symbol-append '< symbol '>)))
                       (pred   (identifier (symbol-append symbol '?)))
                       (wrap   (identifier (symbol-append 'pointer-> symbol)))
                       (unwrap (identifier (symbol-append symbol '->pointer))))
           #`(define-wrapped-pointer-type rtd
               pred
               wrap unwrap
               (lambda (obj port)
                 (format port "#<git-~a ~a>"
                         #,(symbol->string symbol)
                         (number->string (pointer-address (unwrap obj))
                                         16))))))))))

(define-libgit2-type annotated-commit)
(define-libgit2-type blame)
(define-libgit2-type blame-options)
(define-libgit2-type blob)
(define-libgit2-type branch-iterator)
(define-libgit2-type checkout-options)
(define-libgit2-type commit)
(define-libgit2-type config)
(define-libgit2-type cred)
(define-libgit2-type describe-result)
(define-libgit2-type diff)
(define-libgit2-type diff-delta)
(define-libgit2-type diff-options)
(define-libgit2-type index)
(define-libgit2-type object)
(define-libgit2-type patch)
(define-libgit2-type refdb)
(define-libgit2-type reference)
(define-libgit2-type reference-iterator)
(define-libgit2-type repository)
(define-libgit2-type remote)
(define-libgit2-type status-list)
(define-libgit2-type tag)
(define-libgit2-type tree)
(define-libgit2-type tree-entry)
(define-libgit2-type submodule)

;;; helpers

(define (make-double-pointer)
  (bytevector->pointer (make-bytevector (sizeof '*))))

(define (make-size_t-pointer)
  (bytevector->pointer (make-bytevector (sizeof size_t))))

(define (pointer->size_t ptr)
  (bytevector-uint-ref (pointer->bytevector ptr (sizeof size_t))
                       0
                       (endianness little)
                       (sizeof size_t)))
