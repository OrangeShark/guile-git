(define-module (git web app)
  #:use-module (web server)
  #:use-module (git web routes)
  #:export (run-gitweb))


(define (run-gitweb port)
  (run-server (lambda args (apply route-handler args)) 'http `(#:port ,port)))
