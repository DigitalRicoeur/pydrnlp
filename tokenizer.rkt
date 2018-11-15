#lang racket/base

(require json
         racket/list
         racket/contract
         racket/match
         racket/async-channel
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
          [tokenizer-tokenize
           (-> tokenizer?
               (listof tokenize-arg?)
               (listof tokenize-result?))]
          [tokenizer-running?
           (-> tokenizer? boolean?)]
          [tokenizer-kill
           (-> tokenizer? any)]
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

;; TODO: think about exn:break
;; Think about thread-resume / kill-safety

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures:

(struct tokenize-arg (lang key text)
  #:transparent)

(struct tokenize-result (key body)
  #:transparent)

(struct token (lemma text)
  #:transparent)

(define (tokenize-args->jsexpr l-args)
  (for/hasheq ([grp (in-list (group-by tokenize-arg-lang
                                       l-args
                                       eq?))])
    (values (tokenize-arg-lang (car grp))
            (map (match-lambda
                   [(tokenize-arg _ key text)
                    (hasheq 'key key
                            'body text)])
                 grp))))

(define (try-parse-result-jsexpr js)
  (let/ec return
    (and
     (list? js)
     (for/list ([j (in-list js)])
       (match j
         [(hash-table
           ['key key]
           ['tokenized (list (hash-table
                              ['lemma (app string->symbol lemma...)]
                              ['text (app datum-intern-literal text...)])
                             ...)])
          (tokenize-result key (map token lemma... text...))]
         [_
          (return #f)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tokenizer:

(struct tokenizer (revision cust worker)
  #:transparent)

(define tokenizer-env
  (conda-environment-variables))

(define (write-json-line js out)
  (write-json js out)
  (newline out)
  (flush-output out))

(define (launch-tokenizer #:quiet? [quiet? #t])
  (define cust
    (make-custodian))
  (with-handlers ([exn:fail? (λ (e)
                               (custodian-shutdown-all cust)
                               (raise e))])
    (match-define (list in-from-py out-to-py pid _ control)
      (parameterize ([current-custodian cust]
                     [current-subprocess-custodian-mode 'kill]
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
                        #"pydrnlp.tokenizer.run")))
    (thread
     (λ ()
       (control 'wait)
       (custodian-shutdown-all cust)))
    (define revision
      (read-json in-from-py))
    (when (eof-object? revision)
      (error 'launch-tokenizer
             "the Python process failed to launch properly"))
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
    (tokenizer revision cust worker)))

(define (tokenizer-running? it)
  (thread-running? (tokenizer-worker it)))

(define (tokenizer-kill it)
  (custodian-shutdown-all (tokenizer-cust it)))

(define (tokenizer-tokenize it l-args)
  (define js-arg
    (tokenize-args->jsexpr l-args))
  (define reply-ach
    (make-async-channel))
  (define worker
    (tokenizer-worker it))
  (thread-send worker
               (cons js-arg reply-ach)
               (λ ()
                 (raise-argument-error 'tokenizer-tokenize
                                       "tokenizer-running?"
                                       1
                                       it
                                       l-args)))
  (handle-result reply-ach
                 (thread-dead-evt worker)))

(define (handle-result reply-ach worker-dead-evt)
  (define handle-reply-evt
    (handle-evt
     reply-ach
     (λ (js)
       (cond
         [(exn:fail? js)
          (raise js)]
         [(try-parse-result-jsexpr js)]
         [else
          (error 'tokenizer-tokenize
                 "Python returned an invalid result\n  given: ~e"
                 js)]))))
  (sync
   handle-reply-evt
   (handle-evt
    worker-dead-evt
    (λ (worker-dead-evt)
      (or (sync/timeout 0 handle-reply-evt)
          (error 'tokenizer-tokenize
                 "the tokenizer died before returning a result"))))))




