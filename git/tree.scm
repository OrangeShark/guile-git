
(define-module (git tree)
  #:use-module (system foreign)
  #:use-module (git types)
  #:use-module (git bindings)
  #:export (%tree-free
            tree-lookup
            tree-dup
            tree-entry-byid            
            tree-entry-byindex
            tree-entry-name
            tree-id
            tree-walk))


;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_create_updated

;; XXX: only found in HEAD
;;
;; (define tree-dup
;;   (let ((proc (libgit2->procedure* "git_tree_dup" '(* *))))
;;     (lambda (source)
;;       (let ((out (make-double-pointer)))
;;         (proc out
;;               (tree->pointer source))
;;         (pointer->tree (dereference-pointer out))))))

(define tree-entry-byid
  (let ((proc (libgit2->procedure '* "git_tree_entry_byid" '(* *))))
    (lambda (tree id)
      (let ((ret (proc (tree->pointer tree) (oid->pointer id))))
        (if (null-pointer? ret)
            #f
            (pointer->tree-entry ret))))))

(define tree-entry-byindex
  (let ((proc (libgit2->procedure '* "git_tree_entry_byindex" `(* ,size_t))))
    (lambda (tree idx)
      (let ((ret (proc (tree->pointer tree) idx)))
        (if (null-pointer? ret)
            #f
            (pointer->tree-entry ret))))))

(define tree-entry-byname
  (let ((proc (libgit2->procedure '* "git_tree_entry_byname" '(* *))))
    (lambda (tree filename)
      (let ((ret (proc (tree->pointer tree) (string->pointer filename))))
        (if (null-pointer? ret)
            #f
            (pointer->tree-entry ret))))))

(define tree-entry-bypath
  (let ((proc (libgit2->procedure* "git_tree_entry_bypath" '(* * *))))
    (lambda (tree path)
      (let ((out (make-double-pointer)))
        (proc out (tree->pointer tree) (string->pointer path))
        (pointer->tree-entry (pointer-gc (dereference-pointer out) %tree-entry-free))))))

(define tree-entry-cmp
  (let ((proc (libgit2->procedure int "git_tree_entry_cmp" '(* *))))
    (lambda (e1 e2)
      (proc (tree-entry->pointer e1) (tree-entry->pointer e2)))))

(define tree-entry-dup
  (let ((proc (libgit2->procedure* "git_tree_entry_dup" '(* *))))
    (lambda (source)
      (let ((dest (make-double-pointer)))
        (proc dest (tree-entry->pointer source))
        (pointer->tree-entry (pointer-gc (dereference-pointer dest) %tree-entry-free))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_entry_filemode

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_entry_filemode_raw

(define %tree-entry-free (dynamic-func "git_tree_entry_free" libgit2))

(define tree-entry-id
  (let ((proc (libgit2->procedure '* "git_tree_entry_id" '(*))))
    (lambda (entry)
      (let ((ret (proc (tree-entry->pointer entry))))
        (pointer->oid ret)))))

(define tree-entry-name
  (let ((proc (libgit2->procedure '* "git_tree_entry_name" '(*))))
    (lambda (entry)
      (let ((ret (proc (tree-entry->pointer entry))))
        (pointer->string ret)))))

(define tree-entry->object
  (let ((proc (libgit2->procedure* "git_tree_entry_to_object" '(* * *))))
    (lambda (repository entry)
      (let ((out (make-double-pointer)))
        (proc out (repository->pointer repository) (tree-entry->pointer entry))
        (pointer->object (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_entry_type

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_entrycount

(define %tree-free (dynamic-func "git_tree_free" libgit2))

(define tree-id
  (let ((proc (libgit2->procedure '* "git_tree_id" '(*))))
    (lambda (tree)
      (pointer->oid (proc (tree->pointer tree))))))

(define tree-lookup
  (let ((proc (libgit2->procedure* "git_tree_lookup" '(* * *))))
    (lambda (repository id)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (oid->pointer id))
        (pointer->tree (dereference-pointer out))))))

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_lookup_prefix

;; FIXME: https://libgit2.github.com/libgit2/#HEAD/group/tree/git_tree_owner

(define tree-walk
  (let ((proc (libgit2->procedure* "git_tree_walk" `(* ,int * *))))
    (lambda (tree mode callback)
      ;; If the callback returns a positive value, the passed entry will
      ;; be skipped on the traversal (in pre mode). A negative value stops
      ;; the walk.
      (let ((callback* (procedure->pointer int
                                           (lambda (root entry _)
                                             (callback (pointer->string root)
                                                       (pointer->tree-entry entry)))
                                           (list '* '* '*))))
        (proc (tree->pointer tree) mode callback* %null-pointer)))))
