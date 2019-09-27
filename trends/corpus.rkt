#lang racket

(require ricoeur/tei
         gregor
         ricoeur/stdlib/json
         "analyze.rkt")

(provide trends-corpus<%>
         (contract-out
          [make-trends-corpus-mixin
           (-> (-> string? string?)
               (and/c (make-mixin-contract corpus%)
                      (-> (or/c (implementation?/c trends-corpus<%>)
                                (class/c
                                 (absent trends-term->href
                                         trends-data-ready-evt
                                         write-trends-data)))
                          (implementation?/c trends-corpus<%>))))]
          [abstract-trends-corpus-mixin
           (and/c (make-mixin-contract corpus%)
                  (-> (class/c
                       (absent trends-term->href
                               trends-data-ready-evt
                               write-trends-data))
                      (implementation?/c trends-corpus<%>)))]
          [make-trends-corpus-mixin*
           (-> (-> string? string?)
               (make-mixin-contract trends-corpus<%>))]
          ))

(define-corpus-mixin+interface [abstract-trends-corpus-mixin
                                trends-corpus<%>]
  [] []
  (interface ()
    [trends-term->href (->m string? string?)]
    [trends-data-ready-evt (->m (evt/c (recursive-contract
                                        (is-a?/c trends-corpus<%>))))]
    [write-trends-data (->*m [output-port?] any)])
  (abstract trends-term->href)
  (define pr:data-bytes
    ;; or use a temporary file?
    (delay/thread
     (define-values [all-years-with-titles listof-text+%+sparse-data]
       (make-trends-data (sync (super-docs-evt))))
     (jsexpr->bytes
      (hasheq 'years-with-titles all-years-with-titles
              'terms
              (list '("href" "text" "%" "sparse-data")
                    (for/list ([row (in-list listof-text+%+sparse-data)])
                      (cons (trends-term->href (car row))
                            row)))))))
  (define evt
    (wrap-evt pr:data-bytes
              (λ (x) this)))
  (super-new)
  (define/public-final (trends-data-ready-evt)
    evt)
  (define/public-final (write-trends-data [out (current-output-port)])
    (write-bytes (force pr:data-bytes) out)
    (void)))

(define (make-trends-corpus-mixin* do-trends-term->href)
  (mixin [trends-corpus<%>] []
    (super-new)
    (define/override (trends-term->href term)
      (do-trends-term->href term))))

(define (make-trends-corpus-mixin do-trends-term->href)
  (define the-mixin
    (make-trends-corpus-mixin* do-trends-term->href))
  (λ (%)
    (the-mixin (if (implementation? % trends-corpus<%>)
                   %
                   (abstract-trends-corpus-mixin %)))))

