#lang racket

(require net/url
         json
         "server-startup.rkt"
         "tokenized-result.rkt"
         (submod "tokenized-result.rkt" private)
         )

(provide (contract-out
          [do-request/tokenizer-revision
           (-> (promise/c jsexpr?))] ;; more specific?
          [make-do-request/tokenize
           (-> (-> tokenize-arg/c
                   (promise/c tokenization-results/c)))]
          ))

;; These functions return promises so that exceptions
;; are raised at the "force" site, not on a worker thread.

;; TODO: retry/backoff , error handling

(define (do-request/tokenizer-revision)
  (define-values {status hs body-in}
    (http-sendrecv/url
     (struct-copy url (flask-root-url)
                  [path (list (path/param "tokenizer-revision"
                                          null))])))
  (delay/sync
   ;; TODO: error handling
   (read-json body-in)))



(define (make-do-request/tokenize)
  (define u
    (struct-copy url (flask-root-url)
                 [path (list (path/param "tokenize" null))]))
  (λ (to-send)
    (define-values {status hs body-in}
      (http-sendrecv/url u
                         #:data (jsexpr->bytes
                                 (tokenize-arg->jsexpr to-send))
                         #:method #"POST"
                         #:headers '(#"Content-Type: application/json")))
    (delay/sync
     ;; TODO: error handling
     (handle-tokenize-jsexpr-result
      (read-json body-in)))))
  
