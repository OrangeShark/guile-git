(define-module (tests helpers))


(use-modules (ice-9 optargs))


(define-public (path-exists? path)
  "Return #true if path is a file or directory.
   #false if it doesn't exists"
  (access? path F_OK))


(define-public (path-join . rest)
  "Return the absolute path made of REST. If the first item
   of REST is not absolute the current working directory
   will be prepended"
  (let ((path (string-join rest "/")))
    (if (string-prefix? "/" path)
        path
        (string-append (getcwd) "/" path))))


(define-public (path-split path)
  (let ((parts (string-split path #\/)))
    (if (equal? (car parts) "")
        (cons (string-append "/" (cadr parts)) (cddr parts))
        parts)))


(define*-public (path-mkdir dirpath #:optional (parents #false))
  "Create DIRPATH directory and its parents if PARENTS is true"
  (if parents
      (let* ((parts (path-split dirpath))
             (paths (let loop ((dirs (cdr parts))
                               (out (list (car parts))))
                      (if (null? dirs)
                          (reverse out)
                          (loop (cdr dirs) (cons (apply path-join (list (car out) (car dirs))) out))))))
        (and (map (lambda (p) (if (not (path-exists? p)) (mkdir p))) paths) #true))
      (if (not (path-exists? dirpath)) (and (mkdir dirpath) #true))))


(define-public (path-walk dirpath proc)
  (define dir (opendir dirpath))
  (let loop ()
    (let ((entry (readdir dir)))
      (cond
       ((eof-object? entry))
       ((or (equal? entry ".") (equal? entry "..")) (loop))
       (else (let ((path (path-join dirpath entry)))
               (if (equal? (stat:type (stat path)) 'directory)
                   (begin (path-walk path proc) (loop))
                   (begin (proc path) (loop))))))))
  (closedir dir)
  (proc (path-join dirpath)))

(define-public (rmtree path)
  (path-walk path (lambda (path)
                    (if (equal? (stat:type (stat path)) 'directory)
                        (rmdir path)
                        (delete-file path)))))

(define-syntax-rule (with-directory path body ...)
  (begin
    (when (access? path F_OK)
      (rmtree path))
    (path-mkdir path #true)
    body ...
    (rmtree path)))

(export with-directory)

(define-syntax-rule (with-repository name body ...)
  (with-directory "tmp"
    (let ((path (string-append "tests/data/" name ".tgz")))
      (system* "tar" "xvf" path "-C" "tmp"))
    body ...))

(export with-repository)
