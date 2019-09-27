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
         pydrnlp/support)

(provide trends-engine?
         (contract-out
          [launch-trends-engine
           (->* {}
                {#:quiet? any/c}
                trends-engine?)]
          [trends-engine-revision
           jsexpr?]
          [trends-engine-tokenize
           (-> trends-engine?
               (listof (hash/c (or/c 'en 'fr) (listof string?)
                               #:immutable #t))
               (stream/c (listof token?)))]
          [struct token
            ([lemma symbol?]
             [text (and/c string? immutable?)])]
          ))

(struct token (lemma text)
  #:transparent)

(define-python-worker trends-engine
  #"pydrnlp.trends")


(define (trends-engine-tokenize it js-arg)
  (define expect-count
    (for/sum ([lst (in-immutable-hash-values js-arg)])
      (length lst)))
  (convert-results
   expect-count
   (trends-engine-send/raw it js-arg #:who 'trends-engine-tokenize)))


(define (convert-results expect-count js-results)
  ;; TODO: could this use for/foldr ??
  ;; ?? "promised" / "produced" ??
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
      [(< count expect-count)
       (define this
         (map (match-lambda
                [(list (app string->symbol lemma)
                       (app datum-intern-literal text))
                 (cons lemma text)])
              (stream-first js-results)))
       (stream-cons this
                    (loop (add1 count)
                          (stream-rest js-results)))]
      [else
       (error 'trends-engine-tokenize
              "~a\n  expected: ~e\n  given: more"
              "stream produced too many results"
              expect-count)])))
  
