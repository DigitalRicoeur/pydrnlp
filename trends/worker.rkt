#lang racket

(require json
         racket/list
         racket/contract
         racket/match
         racket/stream
         racket/async-channel
         racket/port
         racket/system
         adjutor
         pydrnlp/worker)

(provide trends-engine?
         (contract-out
          [launch-trends-engine
           (->* {}
                {#:quiet? any/c}
                trends-engine?)]
          [trends-engine-revision
           (-> trends-engine? jsexpr?)]
          [trends-engine-tokenize
           (-> trends-engine?
               (listof json-segment?)
               (stream/c trends-segment-result?))]
          [struct trends-segment-result
            ([key jsexpr?]
             [tokens (listof token?)])]
          [struct token
            ([lemma symbol?]
             [text (and/c string? immutable?)])]
          [struct json-segment
            ([lang (or/c 'en 'fr)]
             [key jsexpr?]
             [text string?])]
          ))

(struct json-segment (lang key text)
  #:transparent)

(struct trends-segment-result (key tokens)
  #:transparent)

(struct token (lemma text)
  #:transparent)

(define-python-worker trends-engine
  #"pydrnlp.trends")

(define trends-engine-tokenize
  (letrec ([trends-engine-tokenize
            (λ (it segs)
              (for/fold/define ([keys-table #hasheqv()]
                                [js-arg #hasheq()])
                               ([i (in-naturals)]
                                [seg (in-list segs)])
                (match-define (json-segment lang key text) seg)
                (values (hash-set keys-table i key)
                        (hash-set js-arg
                                  lang
                                  (cons (list i text)
                                        (hash-ref js-arg lang null)))))
              (convert-results
               keys-table
               (trends-engine-send/raw it js-arg #:who 'trends-engine-tokenize)))]
           [convert-results
            (λ (keys-table js-results)
              (cond
                [(stream-empty? js-results)
                 (if (< 0 (hash-count keys-table))
                     empty-stream
                     (error 'trends-engine-tokenize
                            "~a\n  missing keys: ~e"
                            "stream ended without producing all results"
                            keys-table))]
                [else
                 (match-define (list key (list (list lema... text...) ...))
                   (stream-first js-results))
                 (define this
                   (trends-segment-result (hash-ref keys-table key)
                                          (map token lema... text...)))
                 (let ([keys-table (hash-remove keys-table key)])
                   (stream-cons this
                                (convert-results keys-table
                                                 (stream-rest js-results))))]))])
    trends-engine-tokenize))
  
