(define-module (tests commit)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(use-modules (ice-9 receive))

(use-modules (tests helpers))
(use-modules (git))


(test-begin "commit")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "commit-author signature-name"
    "Amirouche"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (signature-name (commit-author (commit-lookup repository oid)))))

  (test-equal "commit-author signature-email"
    "amirouche@hypermove.net"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (signature-email (commit-author (commit-lookup repository oid)))))

  (test-equal "commit-author signature-when"
    '(1477978598 60)
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository)))
           (when* (signature-when (commit-author (commit-lookup repository oid)))))
      (list (time-time when*) (time-offset when*))))
  
  (test-equal "commit-body"
    ""
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (commit-body (commit-lookup repository oid))))
  
  (test-equal "commit-id"
    "3f848a1a52416ac99a5c5bf2e6bd55eb7b99d55b"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-id (commit-lookup repository oid)))))

  (test-assert "commit & eq?"
    ;; Make sure identical commits yield commit objects that are 'eq?'.
    (let* ((repository (repository-open directory))
           (oid        (reference-target (repository-head repository)))
           (head       (commit-lookup repository oid))
           (parent     (commit-parent head)))
      (and (eq? head
                (commit-lookup repository oid)
                (commit-lookup repository oid))
           (eq? parent
                (commit-parent head)
                (let ((oid (commit-id parent)))
                  (commit-lookup repository oid))))))

  (test-equal "commit-message"
    "Add directory/message\n"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (commit-message (commit-lookup repository oid))))

  (test-equal "commit-message-encoding"
    #f
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (commit-message-encoding (commit-lookup repository oid))))

  (test-equal "commit-owner"
    #t
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (eq? (commit-owner (commit-lookup repository oid))
           repository)))

  (test-equal "commit-raw-header"
    "tree d40674e05d114e5eb0df0f358ebeec47b8782ced\nparent b70d89182da3b2019c3fd6755c794ee65921b4a8\nauthor Amirouche <amirouche@hypermove.net> 1477978598 +0100\ncommitter Amirouche <amirouche@hypermove.net> 1477978598 +0100\ngpgsig -----BEGIN PGP SIGNATURE-----\n Version: GnuPG v1\n \n iQEcBAABAgAGBQJYGCn5AAoJEK2P8jNAoMGZMnIH/0F4+8POTeNNNmyWq3ZdHSY5\n wS0IXvUAEkhpS1CvqEpungfeO7JccjX5hJ5FypLKV/3Qhyrkylhdij2rCaTOL2kq\n YE3GefB87ER5tSgqCeezeg8XfB4JeJOsnMzG/t7mrqpGPpQ5f0BL3P6Ti3bsM9Dy\n wiIDtwUJ2Eof2itS+dDEgIN6n/fhNb7eOf+yANZNnetVUc3OLrWqNwKecuypa3Gr\n e38LDuDqF/e5ZXdMuFv34IErS7VOryC+aJ/YoQbbXRKj8jhQALdiTWQ985ay1hNt\n rGFM5ZsuC/zdNk8jnkl7w8g1PsaZdBFu9478z4EoRAT6oHR7OdV9edmEhm/ysz8=\n =zXYT\n -----END PGP SIGNATURE-----\n"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (commit-raw-header (commit-lookup repository oid))))

  (test-equal "commit-summary"
    "Add directory/message"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (commit-summary (commit-lookup repository oid))))

  (test-equal "commit-time-offset"
    60
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (commit-time-offset (commit-lookup repository oid))))

  (test-equal "commit-tree-id"
    "d40674e05d114e5eb0df0f358ebeec47b8782ced"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-tree-id (commit-lookup repository oid)))))

  (test-equal "commit-parent"
    "b70d89182da3b2019c3fd6755c794ee65921b4a8"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-id (commit-parent (commit-lookup repository oid))))))

  (test-equal "commit-parent-id"
    "b70d89182da3b2019c3fd6755c794ee65921b4a8"
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (oid->string (commit-parent-id (commit-lookup repository oid)))))

  (test-equal "commit-parentcount"
    1
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (commit-parentcount (commit-lookup repository oid))))

  (test-equal "commit-parents"
    "354bcdf85d661533f28dae0e78ce0be99a9dfb9d"    ;root commit
    (let* ((repository (repository-open directory))
           (oid (reference-target (repository-head repository))))
      (let loop ((commit (commit-lookup repository oid)))
        (match (commit-parents commit)
          (()
           (oid->string (commit-id commit)))
          ((parent)
           ;; There are no merge commits in this repo, so each commit but the
           ;; root commit has exactly one parent.
           (loop parent)))))))


(libgit2-shutdown)

(test-end)
