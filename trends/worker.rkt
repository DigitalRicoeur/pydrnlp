#lang racket

(require json
         racket/list
         racket/contract
         racket/match
         racket/async-channel
         racket/port
         racket/system
         pydrnlp/worker)

(provide trends-analyzer?
         (contract-out
          [launch-trends-analyzer
           (->* {}
                {#:quiet? any/c}
                trends-analyzer?)]
          [trends-analyzer-revision
           (-> trends-analyzer? jsexpr?)]
          [trends-analyzer-analyze
           (-> trends-analyzer?
               (listof json-segment?)
               (listof trends-segment-result?))]
          [struct trends-segment-result
            ([key jsexpr?]
             [tokens (listof token?)])]
          [struct token
            ([lemma symbol?]
             [text (and/c string? immutable?)])]
          ;;;;
          ;; reusable
          [struct json-segment
            ([lang (or/c 'en 'fr)]
             [key jsexpr?]
             [text string?])]
          [json-segments->jsexpr
           (-> (listof json-segment?) jsexpr?)]
          ))

(struct json-segment (lang key text)
  #:transparent)

(define (json-segments->jsexpr args)
  (for/hasheq ([grp (in-list (group-by json-segment-lang args eq?))])
    (values (json-segment-lang (car grp))
            (map (match-lambda
                   [(json-segment _ key text)
                    (hasheq 'key key
                            'body text)])
                 grp))))

(struct trends-segment-result (key tokens)
  #:transparent)

(struct token (lemma text)
  #:transparent)

(define-python-worker trends-analyzer analyze
  #"pydrnlp.trends"
  json-segments->jsexpr
  (λ (js)
    (define (error!)
      (error 'trends-analyzer-analyze
             "Python returned an invalid result\n  given: ~e"
             js))
    (unless (list? js)
      (error!))
    (for/list ([j (in-list js)])
      (match j
        [(hash-table
          ['key key]
          ['tokenized (list (hash-table
                             ['lemma (app string->symbol lemma...)]
                             ['text (app datum-intern-literal text...)])
                            ...)])
         (trends-segment-result key (map token lemma... text...))]
        [_
         (error!)]))))
