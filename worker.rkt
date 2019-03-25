#lang racket/base

(require json
         racket/list
         racket/contract
         racket/match
         racket/async-channel
         racket/port
         racket/system
         "conda.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(provide python-worker?
         define-python-worker
         (contract-out
          [python-worker-running?
           (-> python-worker? boolean?)]
          [python-worker-kill
           (-> python-worker? any)]
          ))
 
(struct python-worker (cust worker)
  #:transparent)

(define (write-json-line js out)
  (write-json js out)
  (newline out)
  (flush-output out))

;; TODO: think about exn:break
;; Think about thread-resume / kill-safety

(define ((make-launcher #:who who mod ctor) #:quiet? [quiet? #t])
  (define cust
    (make-custodian))
  (with-handlers ([exn:fail? (λ (e)
                               (custodian-shutdown-all cust)
                               (raise e))])
    (match-define (list in-from-py out-to-py pid _ control)
      (parameterize ([current-custodian cust]
                     [current-subprocess-custodian-mode 'kill]
                     [current-environment-variables (conda-environment-variables)]
                     [current-directory py-dir])
        (process*/ports #:set-pwd? #t
                        #f
                        #f
                        (if quiet?
                            (open-output-nowhere)
                            (current-error-port))
                        python3
                        #"-m"
                        mod)))
    (thread
     (λ ()
       (control 'wait)
       (custodian-shutdown-all cust)))
    (define revision
      (read-json in-from-py))
    (when (eof-object? revision)
      (error who "the Python process failed to launch properly"))
    (define worker
      (parameterize ([current-custodian cust])
        (thread
         (λ () 
           (let loop ()
             (match-define (cons js reply-ach)
               (thread-receive))
             (write-json-line js out-to-py) ;; includes flush-output
             (define rslt
               (with-handlers ([exn:fail? values])
                 (read-json in-from-py)))
             (async-channel-put reply-ach rslt)
             (loop))))))
    (thread
     (λ ()
       (sync (thread-dead-evt worker))
       (custodian-shutdown-all cust)))
    (ctor cust worker revision)))

(define (python-worker-running? it)
  (thread-running? (python-worker-worker it)))

(define (python-worker-kill it)
  (custodian-shutdown-all (python-worker-cust it)))




(define (python-worker-send #:who who it raw-arg arg->jsexpr)
  (define js-arg
    (arg->jsexpr raw-arg))
  (define reply-ach
    (make-async-channel))
  (define worker
    (python-worker-worker it))
  (thread-send worker
               (cons js-arg reply-ach)
               (λ ()
                 (raise-argument-error who
                                       "python-worker-running?"
                                       1
                                       it
                                       raw-arg)))
  (handle-worker-result #:who who
                        reply-ach
                        (thread-dead-evt worker)))


(define (handle-worker-result #:who who reply-ach worker-dead-evt)
  (define handle-reply-evt
    (handle-evt reply-ach
                (λ (js)
                  (if (exn:fail? js)
                      (raise js)
                      js))))
  (sync
   handle-reply-evt
   (handle-evt
    worker-dead-evt
    (λ (worker-dead-evt)
      (or (sync/timeout 0 handle-reply-evt)
          (error who
                 "the python worker died before returning a result"))))))


(define-for-syntax (compound-id #:ctxt ctxt . parts)
  (define new-id
    (datum->syntax
     ctxt
     (string->symbol
      (apply string-append
             (for/list ([v (in-list parts)])
               (if (identifier? v)
                   (symbol->string (syntax-e v))
                   v))))))
  (syntax-property
   new-id
   'sub-range-binders
   (for/fold ([lst null]
              [prefix-len 0]
              #:result lst)
             ([v (in-list parts)])
     (cond
       [(identifier? v)
        (define v-len
          (string-length (symbol->string (syntax->datum v))))
        (values
         (cons (vector (syntax-local-introduce new-id)
                       prefix-len v-len 0.5 0.5
                       (syntax-local-introduce v)
                       0 v-len 0.5 0.5)
               lst)
         (+ v-len prefix-len))]
       [else
        (values lst (+ (string-length v) prefix-len))]))))


(define-syntax-parser define-python-worker
  [(_ name:id action:id
      mod:bytes
      convert-arg:expr convert-result:expr)
   #:with name-action (compound-id #:ctxt #'name #'name "-" #'action)
   #:with name? (compound-id #:ctxt #'name #'name "?")
   #:with name-revision (compound-id #:ctxt #'name #'name "-revision")
   #:with launch-name (compound-id #:ctxt #'name "launch-" #'name)
   #`(begin
       (struct name python-worker (revision)
         #:constructor-name ctor
         #:name static-name)
       (define launch-name
         (make-launcher #:who 'launch-name mod ctor))
       (define convert-arg* convert-arg)
       (define convert-result* convert-result)
       (define (name-action it raw-arg)
         (convert-result*
          (python-worker-send
           #:who 'name-action it raw-arg convert-arg*))))])
         


   
                             






   
