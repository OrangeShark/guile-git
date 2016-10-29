;;; Copyright © Ludovic Courtès <ludo@gnu.org>
;;; Released under the GNU GPL version 3 or later.

(define-module (git bindings)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (ice-9 match)
  #:use-module (git config)
  #:use-module (git types)
  #:export (libgit2
            libgit2->procedure
            libgit2->procedure*
            make-buffer
            free-buffer
            buffer-content
            buffer-content/string
            commit-signature))

;; DRAFT!

(define libgit2
  (dynamic-link %libgit2))

(define (libgit2->procedure return name params)
  (pointer->procedure return (dynamic-func name libgit2) params))

(define-inlinable (libgit2->procedure* name params)
  (let ((proc (libgit2->procedure int name params)))
    (lambda args
      (let ((ret (apply proc args)))
	(unless (zero? ret)
	  (throw 'git-error ret))))))


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


;;; blob https://libgit2.github.com/libgit2/#HEAD/group/blob

;;; cherrypick https://libgit2.github.com/libgit2/#HEAD/group/cherrypick

(define cherrypick
  (let ((proc (libgit2->procedure* "git_cherrypick" '(* * *))))
    (lambda (repository commit)
      (proc (repository->pointer repository)
	    (commit->pointer commit)
	    %null-pointer))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/cherrypick/git_cherrypick_commit

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/cherrypick/git_cherrypick_init_options

;;; clone https://libgit2.github.com/libgit2/#HEAD/group/clone

(define clone
  (let ((proc (libgit2->procedure* "git_clone" '(* * * *))))
    (lambda (url local-path)
      (let ((out (make-double-pointer)))
	(proc out (string->pointer url) (string->pointer local-path) %null-pointer)))))

;;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/config

;;; https://libgit2.github.com/libgit2/#HEAD/group/cred

(define cred-defaul-new
  (let ((proc (libgit2->procedure* "git_cred_default_new" '(*))))
    (lambda ()
      (let ((out (make-double-pointer)))
	(proc out)
	(pointer->cred (dereference-pointer out))))))

(define cred-free
  (let ((proc (libgit2->procedure void "git_cred_free" '(*))))
    (lambda (cred)
      (proc (cred->pointer cred)))))

(define cred-has-username?
  (let ((proc (libgit2->procedure int "git_cred_has_username" '(*))))
    (lambda (cred)
      (eq? (proc (cred->pointer cred)) 1))))

(define cred-ssh-custom-new
  (let ((proc (libgit2->procedure* "git_cred_ssh_custom_new" `(* * * ,size_t * *))))
    (lambda (username publickey sign-callback)
      (let ((out (make-double-pointer)))
	(proc out
	      (string->pointer username)
	      (string->pointer publickey)
	      (string-length publickey)
	      (procedure->pointer int
                                  (lambda (session sig sig-len data data-len abstract)
                                    (sign-callback session sig data abstract))
                                  '(* * * * * *)))
	(pointer->cred (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/cred/git_cred_ssh_interactive_new

(define cred-ssh-key-from-agent
  (let ((proc (libgit2->procedure* "git_cred_ssh_key_from_agent" '(* *))))
    (lambda (username)
      (let ((out (make-double-pointer)))
	(proc out (string->pointer username))
	(pointer->cred (dereference-pointer out))))))

(define cred-ssh-key-from-memory-new
  (let ((proc (libgit2->procedure* "git_cred_ssh_key_memory_new" '(* * * * *))))
    (lambda (username publickey privatekey passphrase)
      (let ((out (make-double-pointer)))
	(proc out
	      (string->pointer username)
	      (string->pointer publickey)
	      (string->pointer privatekey)
	      (string->pointer passphrase))
	(pointer->cred (dereference-pointer out))))))

;; XXX: duplicate of the above?
(define cred-ssh-key-new
  (let ((proc (libgit2->procedure* "git_cred_ssh_key_new" '(* * * * *))))
    (lambda (username publickey privatekey passphrase)
      (let ((out (make-double-pointer)))
	(proc out
	      (string->pointer username)
	      (string->pointer publickey)
	      (string->pointer privatekey)
	      (string->pointer passphrase))
	(pointer->cred (dereference-pointer out))))))

(define cred-username-new
  (let ((proc (libgit2->procedure* "git_cred_username_new" '(* *))))
    (lambda (username)
      (let ((out (make-double-pointer)))
	(proc out (string->pointer username))
	(pointer->cred (dereference-pointer out))))))

(define cred-userpass
  (let ((proc (libgit2->procedure* "git_cred_userpass" `(* * * ,unsigned-int *))))
    (lambda (url user-from-url allowed-types)
      (let ((out (make-double-pointer)))
	(proc out
	      (string->pointer url)
	      (string->pointer user-from-url)
	      allowed-types
	      %null-pointer)
	(pointer->cred (dereference-pointer out))))))

(define cred-userpass-paintext-new
  (let ((proc (libgit2->procedure* "git_cred_userpass_plaintext_new" '(* * *))))
    (lambda (username password)
      (let ((out (make-double-pointer)))
	(proc out (string->pointer username) (string->pointer password))
	(pointer->cred (dereference-pointer out))))))

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

(define-public libgit2-shutdown
  (libgit2->procedure int "git_libgit2_shutdown" '()))

(define libgit2-version
  (let ((proc (libgit2->procedure void "git_libgit2_version" '(* * *))))
    (lambda ()
      (let ((major (make-double-pointer))
	    (minor (make-double-pointer))
	    (rev (make-double-pointer)))
	(proc major minor rev)
	(map (compose pointer-address dereference-pointer) (list major minor rev))))))

;;; FIXME: mempack https://libgit2.github.com/libgit2/#HEAD/group/mempack

;;; FIXME: merge https://libgit2.github.com/libgit2/#HEAD/group/merge

;;; FIXME: message https://libgit2.github.com/libgit2/#HEAD/group/message

;;; FIXME: note https://libgit2.github.com/libgit2/#HEAD/group/note

;;; object https://libgit2.github.com/libgit2/#HEAD/group/object


;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object__size

(define GIT_OBJ_ANY -2)

(define object-dup
  (let ((proc (libgit2->procedure* "git_object_dup" '(* *))))
    (lambda (object)
      (let ((out (make-double-pointer)))
	(proc out (object->pointer object))
	(pointer->object (dereference-pointer out))))))

(define object-free
  (let ((proc (libgit2->procedure void "git_object_free" '(*))))
    (lambda (object)
      (proc (object->pointer object)))))

(define object-id
  (let ((proc (libgit2->procedure '* "git_object_id" '(*))))
    (lambda (object)
      (pointer->oid (proc (object->pointer object))))))

(define object-lookup
  (let ((proc (libgit2->procedure* "git_object_lookup" `(* * * ,int))))
    (lambda* (repository oid #:optional (type GIT_OBJ_ANY))
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (repository->pointer repository) (oid->pointer oid)
	      type)
	(pointer->object (dereference-pointer out))))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_lookup_bypath

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_lookup_prefix

(define object-owner
  (let ((proc (libgit2->procedure '* "git_object_owner" '(*))))
    (lambda (object)
      (pointer->repository (proc (object->pointer object))))))

;; FIXME https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_peel

(define object-short-id
  (let ((proc (libgit2->procedure* "git_object_short_id" '(*))))
    (lambda (object)
      (let ((out (make-buffer)))
	(proc out (object->pointer object))
	(let ((out* (buffer-content/string out)))
	  (free-buffer out)
	  out)))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_string2type

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_type

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_type2string

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/object/git_object_typeisloose

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


