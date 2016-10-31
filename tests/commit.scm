(define-module (tests commit))

(use-modules (ice-9 receive))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "commit")

(libgit2-init!)

(with-repository "empty-repo"

  (test-equal "commit-author signature-name"
    "Amirouche"
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (signature-name (commit-author (commit-lookup repository oid)))))

  (test-equal "commit-author signature-email"
    "amirouche@hypermove.net"
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (signature-email (commit-author (commit-lookup repository oid)))))

  (test-equal "commit-author signature-when"
    '(1476014645 120)
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository)))
           (when* (signature-when (commit-author (commit-lookup repository oid)))))
      (list (time-time when*) (time-offset when*))))
  
  (test-equal "commit-body"
    ""
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (commit-body (commit-lookup repository oid))))

  (test-equal "commit-id"
    "4a53b71060aa9b11ecf4050d2e7379456f0fbfb1"
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-id (commit-lookup repository oid)))))

  (test-equal "commit-message"
    "root\n"
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (commit-message (commit-lookup repository oid))))

  (test-equal "commit-message-encoding"
    #f
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (commit-message-encoding (commit-lookup repository oid))))

  (test-equal "commit-owner"
    #t
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (eq? (commit-owner (commit-lookup repository oid))
           repository)))

  (test-equal "commit-parentcount"
    0
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (commit-parentcount (commit-lookup repository oid))))

  (test-equal "commit-raw-header"
    "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904\nauthor Amirouche <amirouche@hypermove.net> 1476014645 +0200\ncommitter Amirouche <amirouche@hypermove.net> 1476014645 +0200\n"
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (commit-raw-header (commit-lookup repository oid))))

  (test-equal "commit-summary"
    "root"
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (commit-summary (commit-lookup repository oid))))

  (test-equal "commit-time-offset"
    120
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (commit-time-offset (commit-lookup repository oid))))

  (test-equal "commit-time-offset"
    "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
    (let* ((repository (repository-open "tmp/empty-repo/"))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-tree-id (commit-lookup repository oid)))))

  )


(with-repository "simple"

  (test-equal "commit-parent"
    "354bcdf85d661533f28dae0e78ce0be99a9dfb9d"
    (let* ((repository (repository-open "tmp/simple/"))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-id (commit-parent (commit-lookup repository oid))))))

  (test-equal "commit-parent-id"
    "354bcdf85d661533f28dae0e78ce0be99a9dfb9d"
    (let* ((repository (repository-open "tmp/simple/"))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-parent-id (commit-lookup repository oid)))))

  (test-equal "commit-parentcount"
    1
    (let* ((repository (repository-open "tmp/simple/"))
           (oid (reference-target (repository-head repository))))
      (commit-parentcount (commit-lookup repository oid))))

  )


(libgit2-shutdown)

(test-end)
