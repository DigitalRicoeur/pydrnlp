#lang racket

(require ricoeur/stdlib/json
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
              (for/fold/define ([expect-count 0]
                                [js-arg #hasheq()])
                               ([i (in-naturals)]
                                [seg (in-list segs)])
                (match-define (json-segment lang key text) seg)
                (values (add1 expect-count)
                        (hash-set js-arg
                                  lang
                                  (cons (list i text)
                                        (hash-ref js-arg lang null)))))
              (convert-results
               expect-count
               (trends-engine-send/raw it js-arg #:who 'trends-engine-tokenize)))]
           [convert-results
            (λ (expect-count js-results)
              (let loop ([count 0]
                         [js-results js-results])
                (cond
                  [(stream-empty? js-results)
                   (if (= count expect-count)
                       empty-stream
                       (error 'trends-engine-tokenize
                              "~a\n  expected: ~e\n  given: ~e"
                              "stream ended without producing all results"
                              expect-count
                              count))]
                  [else
                   (match-define (list key (list (list (app string->symbol lema...)
                                                       (app string->immutable-string text...))
                                                 ...))
                     (stream-first js-results))
                   (define this
                     (trends-segment-result key (map token lema... text...)))
                   (define new-count (add1 count))
                   (stream-cons this
                                (loop new-count
                                      (stream-rest js-results)))])))])
    trends-engine-tokenize))
  
