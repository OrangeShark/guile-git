(define-module (tests status)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(use-modules (tests helpers))
(use-modules (git))

(test-begin "status")

(libgit2-init!)

(with-repository "simple" directory

  (test-equal "empty status list"
    0
    (let ((repository (repository-open directory))
          (opts (status-init-options)))
      (status-list-entry-count (status-list-new repository opts))))

  ;; Create a new, untracked, file test.txt in simple directory.
  (call-with-output-file (string-append directory "/" "test.txt")
        (lambda (port)
          (display "Hello !\n")))

  (test-equal "untracked file count"
    1
    (begin
      (let* ((repository (repository-open directory))
             (opts (status-init-options)))
        (status-list-entry-count (status-list-new repository opts)))))

  (test-equal "untracked file name"
    "test.txt"
    (let* ((repository (repository-open directory))
           (opts (status-init-options))
           (status-entry (status-byindex
                          (status-list-new repository opts) 0)))
      (diff-file-path
       (diff-delta-new-file
        (status-entry-index-to-workdir status-entry)))))

  (test-equal "status-entries length"
    1
    (let* ((repository (repository-open directory))
           (opts (status-init-options))
           (status-list (status-list-new repository opts)))
      (length (status-list->status-entries status-list))))

  (test-equal "status-entries type"
    #t
    (let* ((repository (repository-open directory))
           (opts (status-init-options))
           (status-list (status-list-new repository opts)))
      (match (status-list->status-entries status-list)
        ((e) (status-entry? e))
        (_ #f))))

  (test-equal "untracked file status"
    '(wt-new)
    (let* ((repository (repository-open directory))
           (opts (status-init-options))
           (status-entry (status-byindex
                          (status-list-new repository opts) 0)))
      (status-entry-status status-entry)))

  ;; Remove untracked file.
  (delete-file (string-append directory "/" "test.txt"))

  (test-equal "empty status list"
    0
    (let ((repository (repository-open directory))
          (opts (status-init-options)))
      (status-list-entry-count (status-list-new repository opts))))

  ;; Modify a tracked file.
  (call-with-output-file (string-append directory "/" "README")
        (lambda (port)
          (display "Hello World?")))

  (test-equal "status-entries length"
    1
    (let* ((repository (repository-open directory))
           (opts (status-init-options))
           (status-list (status-list-new repository opts)))
      (length (status-list->status-entries status-list))))

  (test-equal "unstaged file status"
    '(wt-modified)
    (let* ((repository (repository-open directory))
           (opts (status-init-options))
           (status-entry (status-byindex
                          (status-list-new repository opts) 0)))
      (status-entry-status status-entry))))

(libgit2-shutdown)

(test-end)
