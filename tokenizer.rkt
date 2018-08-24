#lang racket/base

(require json
         racket/contract
         racket/match
         racket/promise
         racket/port
         racket/system
         "conda.rkt")

(provide tokenizer?
         (contract-out
          [launch-tokenizer
           (->* {}
                {#:quiet? any/c}
                tokenizer?)]
          [tokenizer-revision
           (-> tokenizer? jsexpr?)]
          [tokenizer-tokenize!
           (-> tokenizer?
               (listof tokenize-arg?)
               (promise/c (listof tokenize-result?)))]
          [tokenizer-kill!
           (-> tokenizer? any)]
          [tokenizer-accepting?
           (-> tokenizer? boolean?)]
          [tokenizer-promise
           (-> tokenizer? (promise/c (listof tokenize-result?)))]
          [struct tokenize-arg
            ([lang (or/c 'en 'fr)]
             [key jsexpr?]
             [text string?])]
          [struct tokenize-result
            ([key jsexpr?]
             [body (listof token?)])]
          [struct token
            ([lemma symbol?]
             [text (and/c string? immutable?)])]
          ))

(struct tokenize-arg (lang key text)
  #:transparent)

(struct tokenize-result (key body)
  #:transparent)

(struct token (lemma text)
  #:transparent)

(define tokenize-arg->jsexpr
  (match-lambda
    [(tokenize-arg lang key text)
     (hasheq 'lang (case lang
                     [(en) "en"]
                     [(fr) "fr"])
             'key key
             'body text)]))

(define try-jsexpr->tokenize-result
  (match-lambda
    [(hash-table ['key key]
                 ['tokenized (list (hash-table
                                    ['lemma (app string->symbol lemma...)]
                                    ['text (app datum-intern-literal text...)])
                                   ...)])
     (tokenize-result key (map token lemma... text...))]
    [_
     #f]))

(define (try-parse-result-jsexpr js)
  (and (list? js)
       (let/ec return
         (for/list ([j (in-list js)])
           (define rslt
             (try-jsexpr->tokenize-result j))
           (unless rslt
             (return #f))
           rslt))))




(struct tokenizer (revision cust sema bx ch promise)
  #:transparent)

(define tokenizer-env
  (conda-environment-variables))

(define orig-cust
  (current-custodian))

(define (launch-tokenizer #:quiet? [quiet? #t])
  (define cust
    (make-custodian))
  (define bx
    (box 'new))
  (thread-resume
   (thread (λ ()
             (sync (make-custodian-box cust 'live))
             (set! bx 'shut-down)))
   orig-cust)
  (parameterize ([current-custodian cust])
    (match-define (list in-from-py out-to-py pid _ control)
      (parameterize ([current-subprocess-custodian-mode 'kill]
                     [current-environment-variables tokenizer-env]
                     [current-directory py-dir])
        (process*/ports #:set-pwd? #t
                        #f
                        #f
                        (if quiet?
                            (open-output-nowhere)
                            (current-error-port))
                        python3
                        #"-m"
                        #"pydrnlp.stdio")))
    (define revision
      (read-json in-from-py))
    (define ch
      (make-channel))
    (define promise
      (delay/thread
       (define l-args
         (channel-get ch))
       (write-json (map tokenize-arg->jsexpr l-args) out-to-py)
       (close-output-port out-to-py)
       (define js-rslt
         (read-json in-from-py))
       (close-input-port in-from-py)
       (define rslt
         (try-parse-result-jsexpr js-rslt))
       (unless rslt
         (error 'pydrnlp "Python gave a bad result"))
       rslt))
    (tokenizer revision
               cust
               (make-semaphore 1)
               bx
               ch
               promise)))


(define tokenizer-accepting?
  (match-lambda
    [(tokenizer _ _ _ (box 'new) _ _)
     #t]
    [_
     #f]))

(define (tokenizer-tokenize! it l-args)
  (match-define (tokenizer _ _ sema bx ch promise)
    it)
  (call-with-semaphore sema
    (λ (bx ch promise l-args)
      (case (unbox bx)
        [(new)
         (set-box! bx 'used)
         (channel-put ch l-args)
         promise]
        [else
         (error 'tokenizer-tokenize!
                "the tokenizer has already been used")]))
    #f
    bx
    ch
    promise
    l-args))

(define tokenizer-kill!
  (match-lambda
    [(tokenizer _ cust _ bx _ _)
     (custodian-shutdown-all cust)
     (set-box! bx 'killed)]))

