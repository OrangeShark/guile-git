(define-module (git strarray))

(use-modules (bytestructures guile))
(use-modules (git bindings))


(define-public %strarray `((strings . ,address)
                           (count . ,size_t)))

;; (let ((strings-address (bytestructure-ref bs 'strings))
;;       (count (bytestructure-ref bs 'count)))
;;   (let ((strings-bv (pointer->bytevector
;;                      (make-pointer strings-address)
;;                      (* count (bytestructure-descriptor-size intptr_t)))))
;;     (make-bytestructure strings-bv 0 (bs:vector count cstring-pointer))))
