(define-module (git web routes)
  #:use-module (ice-9 match)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (git web repository)
  #:use-module (git web http)
  #:export (route-handler))


(define (index-handler request request-body)
  (http-response "Index page"))

(define (not-found request)
  (http-response (string-append "Not Found: " (uri->string (request-uri request)))
                 #:status-code 404))


(define (route-handler request request-body)
  (match (split-and-decode-uri-path (uri-path (request-uri request)))
    (() (index-handler request request-body))
    ((repo rest ...) (repo-handler repo rest))
    (_ (not-found request))))
