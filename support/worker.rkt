#lang racket/base

(require ricoeur/stdlib/json
         racket/list
         racket/contract
         racket/match
         racket/async-channel
         racket/stream
         racket/port
         racket/system
         "conda.rkt"
         "revision-contract.rkt"
         "../py/environment.rkt"
         syntax/parse/define
         (for-syntax racket/base))

(provide python-worker?
         define-python-worker
         python-revision-value/c
         (contract-out
          [python-worker-running?
           (-> python-worker? boolean?)]
          [python-worker-kill
           (-> python-worker? any)]
          [python-worker-dead-evt
           (-> python-worker? (evt/c (or/c #f exn:fail?)))]
          ))
 
(struct python-worker (cust worker dead-evt)
  #:transparent)

(define (write-json-line js out)
  (write-json js out)
  (newline out)
  (flush-output out))

(define (do-launch-worker mod ctor py-args
                          #:who who #:quiet? quiet?)
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
        (apply process*/ports
               #:set-pwd? #t
               #f
               #f
               (if quiet?
                   (open-output-nowhere)
                   (current-error-port))
               python3
               #"-m"
               mod
               py-args)))
    (thread
     (λ ()
       (control 'wait)
       (custodian-shutdown-all cust)))
    (define fatal-error-box
      (box #f))
    (define worker
      (parameterize ([current-custodian cust])
        (thread/suspend-to-kill
         (λ ()
           (parameterize-break #f
             (with-handlers ([exn:fail? (λ (e)
                                          (set-box! fatal-error-box e)
                                          (raise e))])
               (let loop ()
                 (match-define (cons js reply-ach)
                   (thread-receive))
                 (write-json-line js out-to-py) ;; includes flush-output
                 (for ([rslt (in-producer (λ () (read-json in-from-py)))]
                       #:final (eq? 'null rslt))
                   (when (eof-object? rslt)
                     ;; FIXME use raise-read-eof-error
                     (error who "unexpected eof"))
                   (async-channel-put reply-ach rslt))
                 (loop))))))))
    (thread
     (λ ()
       (sync (thread-dead-evt worker))
       (custodian-shutdown-all cust)))
    (ctor cust
          worker
          (wrap-evt (thread-dead-evt worker)
                    (λ (e) (unbox fatal-error-box))))))

(define (python-worker-running? it)
  (thread-running? (python-worker-worker it)))

(define (python-worker-kill it)
  (custodian-shutdown-all (python-worker-cust it)))




(define (python-worker-send #:who who it js-arg)
  (define reply-ach
    (make-async-channel))
  (match-define (python-worker _ worker dead-evt) it)
  (thread-resume worker (current-thread))
  (thread-send worker
               (cons js-arg reply-ach)
               (λ ()
                 (raise-argument-error who
                                       "python-worker-running?"
                                       it)))
  (handle-worker-result #:who who
                        reply-ach
                        dead-evt))


(define (handle-worker-result #:who who reply-ach worker-dead-evt)
  (define handle-dead-evt
    (handle-evt
     worker-dead-evt
     (λ (maybe-fatal-error)
       (or (sync/timeout 0 reply-ach)
           (error who
                  "~a\n  ~a: ~e"
                  "the python worker died before returning a result"
                  "final exception"
                  (or maybe-fatal-error
                      (unquoted-printing-string "not recorded")))))))
  (define (get-result)
    (sync reply-ach handle-dead-evt))
  (for/stream ([js (in-producer get-result 'null)])
    js))


(define-for-syntax (compound-id #:ctxt ctxt . parts)
  ;; TODO: format-id subsumes this in Racket 7.5
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

(define (build-worker-revision mod-str rev)
  (if (let loop ([rev rev])
        (if (list? rev)
            (andmap loop rev)
            rev))
      (list spacy-revision
            model-revisions
            mod-str
            rev)
      #false))

(define-syntax-parser define-python-worker
  [(_ name:id
      mod:bytes arg:bytes ...)
   #:with name? (compound-id #:ctxt #'name #'name "?")
   #:with name-revision (compound-id #:ctxt #'name #'name "-revision")
   #:with launch-name (compound-id #:ctxt #'name "launch-" #'name)
   #:with name-send/raw (compound-id #:ctxt #'name #'name "-send/raw")
   #:do [(define mod-str (bytes->string/utf-8 (syntax-e #'mod)))
         (define rev-id
           (datum->syntax #f (string->symbol
                              (string-append mod-str ".revision"))
                          #'mod #'mod))]
   #:with imported-rev
   (syntax-local-lift-require
    #`(rename (lib #,(string-append "pydrnlp/py/"
                                    (regexp-replace* #rx"\\." mod-str "/")
                                    ".py"))
              #,rev-id
              revision)
    rev-id)
   #:declare imported-rev (expr/c #'python-revision-function/c
                                  #:name rev-id)
   #:do [;; to avoid exposing struct:name
         (define introduce (make-syntax-introducer))]
   #:with hygienic-name (introduce #'name)
   #:with hygienic-name? (introduce #'name?)
   #`(begin
       (struct hygienic-name python-worker ()
         #:constructor-name ctor
         #:reflection-name 'name
         #:omit-define-syntaxes)
       (define-syntax name?
         (make-rename-transformer (quote-syntax hygienic-name?)))
       (define name-revision
         (build-worker-revision '#,mod-str (imported-rev.c)))
       (define (launch-name #:quiet? [quiet? #t])
         (do-launch-worker #:who 'launch-name
                           #:quiet? quiet?
                           'mod ctor '(arg ...)))
       (with-contract
        #:region define-python-worker name
        ([name-send/raw
          (->* [name? jsexpr?] [#:who symbol?] (stream/c jsexpr?))])
        (define (name-send/raw it js-arg #:who [who 'name-send/raw])
          (python-worker-send #:who who it js-arg))))])
         



