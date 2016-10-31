(define-module (git))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((git bindings)
        (git commit)
        (git oid)
        (git reference)
        (git repository)
        (git struct)))

    (for-each (let ((i (module-public-interface (current-module))))
                (lambda (m)
                  (module-use! i (resolve-interface m))))
              %public-modules)))
