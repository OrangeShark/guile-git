
(define-module (git commit)
  #:use-module (git bindings)
  #:use-module (git types)
  #:export (commit-amend
            commit-author
            commit-body
            commit-committer
            commit-extract-signature
            commit-header-field
            commid-id
            commit-lookup
            commit-lookup-prefix
            commit-message
            commit-message-encoding
            commit-message-raw
            commit-owner
            commit-parent
            commit-parent-id
            commit-parentcount
            commit-raw-header
            commit-summary
            commit-time-offset
            commit-tree
            commit-tree-id))

;; commit https://libgit2.github.com/libgit2/#HEAD/group/commit

(define commit-amend
  (let ((proc (libgit2->procedure* "git_commit_amend" '(* * * * * * * *))))
    (lambda (id commit update-ref author commiter message-encoding message tree)
      (proc (oid->pointer id)
	    (commit->pointer commit)
	    (string->pointer update-ref)
	    (signature->pointer author)
	    (signature->pointer commiter)
	    (string->pointer message-encoding)
	    (string->pointer message)
	    (tree->pointer tree)))))

(define commit-author
  (let ((proc (libgit2->procedure '* "git_commit_author" '(*))))
    (lambda (commit)
      (pointer->signature (proc (commit->pointer commit))))))

(define commit-body
  (let ((proc (libgit2->procedure '* "git_commit_body" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define commit-committer
  (let ((proc (libgit2->procedure '* "git_commit_committer" '(*))))
    (lambda (commit)
      (pointer->signature (proc (commit->pointer commit))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_buffer

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_from_callback

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_v

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_create_with_signature

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_dup

(define commit-extract-signature
  (let ((proc (libgit2->procedure* "git_commit_extract_signature" '(* * * * *))))
    (lambda* (repository oid #:optional (field "gpgsig"))
      (let ((signature (make-buffer))
	    (data      (make-buffer)))
	(proc signature data (repository->pointer repository)
	      (oid->pointer oid)
	      (string->pointer field))
	(let ((signature* (buffer-content/string signature))
	      (data*      (buffer-content/string data)))
	  (free-buffer signature)
	  (free-buffer data)
	  (values signature* data*))))))

(define commit-free
  (let ((proc (libgit2->procedure void "git_commit_free" '(*))))
    (lambda (commit)
      (proc (commit->pointer commit)))))

(define commit-header-field
  (let ((proc (libgit2->procedure* "git_commit_header_field" '(* * *))))
    (lambda (commit field)
      (let ((out (make-buffer)))
	(proc out (commit->pointer commit) (string->pointer field))
	(let ((out* (buffer-content/string out)))
	  (free-buffer out)
	  out*)))))

(define commit-id
  (let ((proc (libgit2->procedure '* "git_commit_id" '(*))))
    (lambda (commit)
      (pointer->oid (proc (commit->pointer commit))))))

(define commit-lookup
  (let ((proc (libgit2->procedure* "git_commit_lookup" `(* * *))))
    (lambda (repository oid)
      (let ((out (bytevector->pointer (make-bytevector (sizeof '*)))))
	(proc out (repository->pointer repository) (oid->pointer oid))
	(pointer->commit (dereference-pointer out))))))

(define commit-lookup-prefix
  (let ((proc (libgit2->procedure* "git_commit_lookup_prefix" `(* * * ,size_t))))
    (lambda (repository id len)
      (let ((out (make-double-pointer)))
	(proc out (repository->pointer repository) (oid->pointer id) len)))))

(define commit-message
  (let ((proc (libgit2->procedure '* "git_commit_message" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define commit-message-encoding
  (let ((proc (libgit2->procedure '* "git_commit_message_encoding" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define commit-message-raw
  (let ((proc (libgit2->procedure '* "git_commit_message_raw" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_nth_gen_ancestor

(define commit-owner
  (let ((proc (libgit2->procedure '* "git_commit_owner" '(*))))
    (lambda (commit)
      (pointer->repository (proc (commit->pointer commit))))))

(define commit-parent
  (let ((proc (libgit2->procedure* "git_commit_parent" `(* ,unsigned-int))))
    (lambda (commit n)
      (let ((out (make-double-pointer)))
	(proc out (commit->pointer commit) n)
	(pointer->commit (dereference-pointer out))))))

(define commit-parent-id
  (let ((proc (libgit2->procedure '* "git_commit_parent_id" `(* ,unsigned-int))))
    (lambda (commit n)
      (pointer->oid (proc (commit->pointer commit) n)))))

(define commit-parentcount
  (let ((proc (libgit2->procedure unsigned-int "git_commit_parentcount" '(*))))
    (lambda (commit)
      (proc (commit->pointer commit)))))

(define commit-raw-header
  (let ((proc (libgit2->procedure '* "git_commit_raw_header" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

(define commit-summary
  (let ((proc (libgit2->procedure '* "git_commit_summary" '(*))))
    (lambda (commit)
      (pointer->string (proc (commit->pointer commit))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/commit/git_commit_time

(define commit-time-offset
  (let ((proc (libgit2->procedure int "git_commit_time_offset" '(*))))
    (lambda (commit)
      (proc (commit->pointer commit)))))

(define commit-tree
  (let ((proc (libgit2->procedure* "git_commit_tree" '(*))))
    (lambda (commit)
      (let ((out (make-double-pointer)))
	(proc out (commit->pointer commit))
	(pointer->tree (dereference-pointer out))))))

(define commit-tree-id
  (let ((proc (libgit2->procedure '* "git_commit_tree_id" '(*))))
    (lambda (commit)
      (pointer->oid (proc (commit->pointer commit))))))
