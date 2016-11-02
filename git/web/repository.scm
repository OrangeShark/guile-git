(define-module (git web repository)
  #:use-module (srfi srfi-1)
  #:use-module (git bindings)
  #:use-module (git repository)
  #:use-module (git web http)
  #:export (repo-handler))

(define (repo-handler repo path)
  (http-response
   (string-append repo " path:" (fold (lambda (str prev) (string-append prev "/" str)) "" path))))
