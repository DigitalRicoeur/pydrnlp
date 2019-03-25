#lang racket

(require ricoeur/tei
         gregor
         json
         "types.rkt"
         "get-build.rkt")

(provide tokenizer-demo-corpus-mixin)

(module+ main
  (define c
    (new (tokenizer-demo-corpus-mixin directory-corpus%)
         [path "/Users/philip/code/ricoeur/texts/TEI/"]))
  (time (send c get-tokenizer-demo-data values)))
  
#|
to build tokenized-corpus of English books from scratch:
cpu time: 73575 real time: 924563 gc time: 4587

from cached:
cpu time: 7765 real time: 10329 gc time: 1031
cpu time: 7702 real time: 10580 gc time: 999
|#

(struct year-data (year total lemma/count)
  #:transparent)

(define (instance-orig-publication-year it)
  (->year (instance-orig-publication-date it)))

(define (tokenized-corpus->l-year-data t-c)
  (for/list ([grp (in-list (group-by instance-orig-publication-year
                                     (set->list (tokenized-corpus-docs t-c))
                                     =))])
    (define l/c
      (apply union:lemma/count
             (map tokenized-document-lemma/count grp)))
    (year-data (instance-orig-publication-year (car grp))
               (total:lemma/count l/c)
               l/c)))

(define (compute-percent count total)
  (exact->inexact (* 100 (/ count total))))

(define (make-tokenizer-demo-data all-docs)
  (define t-c
    (get/build-tokenized-corpus
     (for/instance-set ([doc (in-instance-set all-docs)]
                        #:when (eq? 'en (instance-language doc))
                        #:when (eq? 'book (instance-book/article doc)))
       doc)))
  (define corpus:lemma/string (tokenized-corpus-lemma/string t-c))
  (define corpus:lemma/count (tokenized-corpus-lemma/count t-c))
  (define grand-total (total:lemma/count corpus:lemma/count))
  (define top100
    ;; TODO: handle ties
    (take (sort #:key cdr (hash->list (lemma/count-hsh corpus:lemma/count)) >) 100))
  (define l-year-data
    (tokenized-corpus->l-year-data t-c))
  (define initial-lemma-year-numbers
    (for/list ([pr (in-list top100)])
      (define lemma (car pr))
      (filter-map (match-lambda
                    [(year-data year total lemma/count)
                     (define count (lemma/count-ref lemma/count lemma))
                     (and count (cons year (cons count (compute-percent count total))))])
                  l-year-data)))
  (for*/fold/define ([first-year #f]
                     [last-year #f])
                    ([lst (in-list initial-lemma-year-numbers)]
                     [pr (in-list lst)]
                     [yr (in-value (car pr))])
    (values (if first-year (min yr first-year) yr)
            (if last-year (max yr last-year) yr)))
  (define hsh:year->total
    (for/hasheqv ([y-d (in-list l-year-data)])
      (values (year-data-year y-d)
              (year-data-total y-d))))
  (define stop-year (add1 last-year))
  (define year-totals-js
    (for/list ([yr (in-range first-year stop-year)])
      (list yr (hash-ref hsh:year->total yr 0))))
  (define full-lemma-year-numbers
    (for/list ([init-nums (in-list initial-lemma-year-numbers)])
      (for*/list ([yr (in-range first-year stop-year)]
                  [js (in-value
                       (match (assv yr init-nums)
                         [(cons _ (cons count %))
                          (hasheq 'year yr 'count count '% %)]
                         [#f
                          (and (hash-has-key? hsh:year->total yr)
                               (hasheq 'year yr 'count 0 '% 0))]))]
                  #:when js)
        js)))
  (λ (term->href)
    (hasheq 'grand-total grand-total
            'year-totals year-totals-js
            'terms
            (for/list ([pr (in-list top100)]
                       [l-data (in-list full-lemma-year-numbers)])
              (match-define (cons lemma total) pr)
              (define text (lemma/string-ref corpus:lemma/string lemma))
              (hasheq 'text text
                      'href (term->href text)
                      'total total
                      '% (compute-percent total grand-total)
                      'data l-data)))))


(define tokenizer-demo-corpus-mixin
  (let* ([initialize-tokens-method-key (generate-member-key)]
         [pre-mixin (make-corpus-mixin initialize-tokens-method-key)])
    (define-member-name initialize-tokens initialize-tokens-method-key)
    (define tokenizer-demo<%>
      (interface ()
        [get-tokenizer-demo-data
         (->m (-> string? string?) jsexpr?)]))
    (λ (%)
      (class* (pre-mixin %) [tokenizer-demo<%>]
        (inherit get-checksum-table)
        (define ch (make-channel))
        (define pr:tokenized
          (delay/thread
           ;; We need to block waiting for docs before
           ;; we evaluate (get-checksum-table) or there
           ;; will be a use-before-definition error.
           (make-tokenizer-demo-data (sync ch))))
        (super-new)
        (define/override-final (initialize-tokens docs)
          (channel-put ch docs))
        (define/public-final (get-tokenizer-demo-data term->href)
          ((force pr:tokenized) term->href))))))
