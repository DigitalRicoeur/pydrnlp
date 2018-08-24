#lang racket/base

(require "server-startup.rkt"
         "http-request.rkt"
         (submod "interface.rkt" private)
         adjutor
         racket/contract
         racket/match
         racket/promise
         )

(provide basic-pydrnlp?
         (recontract-out flask-quiet?)
         (recontract-out flask-port)
         (contract-out
          [rename make-basic-pydrnlp basic-pydrnlp
                  (->* {}
                       {#:port ip-port-num/c
                        #:quiet? any/c}
                       correct-pydrnlp/c)]
          ))

;; THINK!

;; For background threads, do I also need
;; 3rd parties' custodians?
;; I don't think so, but I should read
;; "Kill-Safe Abstractions" again.

;; What is actually the right behavior for
;; the event produced by pydrnlp-tokenize-evt?
;; Currently it requests only when a sync is attempted
;; and aborts if the sync stops, which means that the event
;; would have to be reconstructed on each call to sync.
;; I think you could get the "stubborn"
;; semantics by just syncing in a background thread.
;; Would that result in the value potentially being sent
;; multiple times? Maybe the worker should keep a weak
;; cache to prevent that.

(struct basic-pydrnlp (cust dead-evt dead?-proc revision-evt worker)
  #:methods gen:pydrnlp
  [(define pydrnlp-kill
     (match-lambda
       [(basic-pydrnlp cust _ _ _ _)
        (custodian-shutdown-all cust)]))
   (define pydrnlp-dead-evt
     (match-lambda
       [(basic-pydrnlp _ dead-evt _ _ _)
        dead-evt]))
   (define pydrnlp-dead?
     (match-lambda
       [(basic-pydrnlp _ _ dead?-proc _ _)
        (dead?-proc)]))
   (define pydrnlp-tokenizer-revision-evt
     (match-lambda
       [(basic-pydrnlp _ _ _ revision-evt _)
        revision-evt]))
   (define (pydrnlp-tokenize-evt this arg)
     (make-tokenize-evt (basic-pydrnlp-worker this)
                        arg))
   #|END #:methods gen:pydrnlp|#])

(define (make-basic-pydrnlp #:quiet? [quiet? (flask-quiet?)]
                            #:port [port (flask-port)])
  (define cust
    (make-custodian))
  (define-values {control worker}
    (parameterize ([current-custodian cust]
                   [flask-quiet? quiet?]
                   [flask-port port])
      (values (start-server-process)
              (make-worker))))
  (make-watcher control cust)
  (basic-pydrnlp cust
                 (make-dead-evt cust control)
                 (make-dead?-proc control)
                 (make-revision-evt worker)
                 worker))

(define (make-dead-evt cust control)
  (wrap-evt (make-custodian-box cust 'live)
            (λ (_)
              (control 'exit-code))))

(define (make-dead?-proc control)
  (λ () (control 'exit-code)))


(struct msg (reply-ch gave-up-evt)
  #:transparent)

(struct msg:tokenizer-revision msg ()
  #:transparent)

(struct msg:tokenize msg (arg)
  #:transparent)

(define (make-worker)
  (thread 
   (λ ()
     (define do-request/tokenize
       (make-do-request/tokenize))
     (let loop ()
       (match (skip-gave-up (thread-receive))
         [#f (void)]
         [(msg:tokenizer-revision reply-ch gave-up-evt)
          (send-reply reply-ch gave-up-evt
                      (do-request/tokenizer-revision))]
         [(msg:tokenize reply-ch gave-up-evt arg)
          (send-reply reply-ch gave-up-evt
                      (do-request/tokenize arg))]
         [bad-message
          (eprintf "basic-pydrnlp: bad internal message\n given: ~e\n"
                   bad-message)])
       (loop)))))

(define (send-reply reply-ch gave-up-evt rslt)
  (thread (λ ()
            (sync (channel-put-evt reply-ch rslt)
                  gave-up-evt))))

(define skip-gave-up
  values
  #;(match-lambda
    [(and this (msg _ gave-up-evt))
     (or (sync/timeout 0 gave-up-evt)
          this)]
    [bad bad]))

(define lib-cust
  (current-custodian))

(define (make-revision-evt worker)
  (define promise
    (parameterize ([current-custodian lib-cust])
      (delay/thread
       (sync
        (make-worker-do-evt worker
                            msg:tokenizer-revision)))))
  (wrap-evt promise
            (λ (_)
              (force promise))))

(define (make-watcher control cust)
  (parameterize ([current-custodian lib-cust])
    (thread (λ ()
              (control 'wait)
              (custodian-shutdown-all cust))))
  (void))
  
          
(define (make-tokenize-evt worker arg)
  (make-worker-do-evt worker
                      (λ (reply-ch gave-up-evt)
                        (msg:tokenize reply-ch
                                      gave-up-evt
                                      arg))))

(define (make-worker-do-evt worker make-message)
  (nack-guard-evt
   (λ (gave-up-evt)
     (define reply-ch
       (make-channel))
     (thread-send worker
                  (make-message reply-ch
                                gave-up-evt)
                  #f)
     reply-ch)))

