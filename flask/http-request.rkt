#lang racket/base

(require net/url
         json
         adjutor
         "server-startup.rkt"
         "interface.rkt"
         (submod "interface.rkt" private)
         syntax/parse/define
         racket/contract
         racket/promise
         (for-syntax racket/base
                     ))

(provide (contract-out
          [do-request/tokenizer-revision
           (-> (promise/c jsexpr?))] ;; more specific?
          [make-do-request/tokenize
           (-> (-> (listof tokenize-arg?)
                   (promise/c (listof tokenize-result?))))]
          ))

;; These functions return promises so that exceptions
;; are raised at the "force" site, not on a worker thread.

;; TODO: retry/backoff , error handling

(define (do-request/tokenizer-revision)
  (with-failure->promise
   (define u
     (struct-copy url (flask-root-url)
                  [path (list (path/param "tokenizer-revision"
                                          null))]))
   (with-network-retry/backoff
    (define-values {status hs body-in}
      (http-sendrecv/url u))
    (delay/sync
     ;; TODO: error handling
     (read-json body-in)))))


(define (make-do-request/tokenize)
  (define u
    (struct-copy url (flask-root-url)
                 [path (list (path/param "tokenize" null))]))
  (λ (to-send)
    (with-failure->promise
     (with-network-retry/backoff
      (define-values {status hs body-in}
        (http-sendrecv/url u
                           #:data (jsexpr->bytes
                                   (map tokenize-arg->jsexpr to-send))
                           #:method #"POST"
                           #:headers '(#"Content-Type: application/json")))
      (delay/thread
       ;; TODO: error handling
       (define rslt
         (try-parse-result-jsexpr 
          (read-json body-in)))
       (unless rslt
         (error 'make-do-request/tokenize "malformed result jsexpr"))
       rslt)))))

;;;;;;;;

(define (call-with-failure->promise thunk)
  (define (handle e)
    (delay/sync (raise e)))
  (with-handlers ([exn:fail? handle])
    (thunk)))

(define-syntax-parser with-failure->promise
  [(_ body:expr ...+)
   #'(call-with-failure->promise
      (λ () body ...))])

(define (call-with-network-retry/backoff thunk)
  (let retry ([wait 1])
    (with-handlers ([exn:fail:network?
                     (λ (e)
                       (cond
                         [(infix: wait < 5)
                          (sleep wait)
                          (retry (add1 wait))]
                         [else
                          (raise e)]))])
      (thunk))))

(define-syntax-parser with-network-retry/backoff
  [(_ body:expr ...+)
   #'(call-with-network-retry/backoff
      (λ () body ...))])

