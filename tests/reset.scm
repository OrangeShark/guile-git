(define-module (tests reset)
  #:use-module (srfi srfi-64))

(use-modules (tests helpers))
(use-modules (git)
             (git object))

(test-begin "reset")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "reset hard"
    #t
    (let* ((repository (repository-open directory))
           (commit "b70d89182da3b2019c3fd6755c794ee65921b4a8")
           (commit-oid (string->oid commit))
           (commit-object (object-lookup repository commit-oid))
           (ret (reset repository commit-object RESET_HARD))
           (head-oid (reference-target (repository-head repository))))
      (and (oid=? commit-oid head-oid) ;; had HEAD moved ?
           ;; directory/message is introduced by commit 3f848a
           ;; and should be removed by reseting hard to b70d89.
           (not (path-exists? "directory/message"))))))

(with-repository "simple" directory

  (test-equal "reset soft"
    #t
    (let* ((repository (repository-open directory))
           (commit "b70d89182da3b2019c3fd6755c794ee65921b4a8")
           (commit-oid (string->oid commit))
           (commit-object (object-lookup repository commit-oid))
           (ret (reset repository commit-object RESET_SOFT))
           (head-oid (reference-target (repository-head repository))))
      (and (oid=? commit-oid head-oid) ;; has HEAD moved ?
           ;; directory/message is introduced by commit 3f848a
           ;; and should be kept by reseting soft to b70d89.
           (path-exists?
            (string-append directory "/directory/message"))))))

(libgit2-shutdown!)

(test-end)
