
(define-module (git tree)
  #:use-module (system foreign)
  #:use-module (git types)
  #:use-module (git bindings))

(define tree-lookup
  (let ((proc (libgit2->procedure* "git_tree_lookup" '(* * *))))
    (lambda (repository id)
      (let ((out (make-double-pointer)))
        (proc out
              (repository->pointer repository)
              (oid->pointer id))
        (pointer->tree (dereference-pointer out))))))

(define tree-dup
  (let ((proc (libgit2->procedure* "git_tree_dup" '(* *))))
    (lambda (source)
      (let ((out (make-double-pointer)))
        (proc out
              (tree->pointer source))
        (pointer->tree (dereference-pointer out))))))

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

(define tree-entry-byindex
  (let ((proc (libgit2->procedure '* "git_tree_entry_byname" '(* *))))
    (lambda (tree filename)
      (let ((ret (proc (tree->pointer tree) (string->pointer filename))))
        (if (null-pointer? ret)
            #f
            (pointer->tree-entry ret))))))
