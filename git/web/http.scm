
(define-module (git web http)
  #:use-module (web response)
  #:export (http-response))


(define* (http-response #:optional body #:key (status-code 200)
                        (content-type  '(text/html)))
  (values (build-response #:code status-code
                          #:headers `((content-type . ,content-type)))
          body))
