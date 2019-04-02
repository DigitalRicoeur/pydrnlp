#lang racket

(require ricoeur/tei
         gregor
         json
         "types.rkt"
         "get-build.rkt")



(module+ main
  (define trends-corpus-mixin
    (let* ([initialize-tokens-method-key (generate-member-key)]
           [pre-mixin (make-corpus-mixin initialize-tokens-method-key)])
      (define-member-name initialize-tokens initialize-tokens-method-key)
      (define trends-corpus<%>
        (interface ()
          get-trends-demo-term-table))
      (λ (%)
        (class* (pre-mixin %) [trends-corpus<%>]
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
          (define/public-final (get-trends-demo-term-table term->href)
            (list '("href" "text" "%" "data")
                  (for/list ([row (in-list (force pr:tokenized))])
                    (cons (term->href (car row)) row))))))))
  (define c
    (new (trends-corpus-mixin directory-corpus%)
         [path "/Users/philip/code/ricoeur/texts/TEI/"]))
  (write-json (send c get-trends-demo-term-table (λ (_) "/todo"))))

(define (compute-percent count total)
  (exact->inexact (* 100 (/ count total))))

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
  (define sorted
    (sort #:key cdr (hash->list (lemma/count-hsh corpus:lemma/count)) >))
  (define l-year-data
    (tokenized-corpus->l-year-data t-c))
  (define initial-lemma-year-numbers
    (for/list ([pr (in-list sorted)])
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
  (define full-lemma-year-numbers
    (for/list ([init-nums (in-list initial-lemma-year-numbers)])
      (for*/list ([yr (in-range first-year stop-year)]
                  [js (in-value
                       (cons yr (match (assv yr init-nums)
                                  [(cons _ (cons count %))
                                   (list %)]
                                  [#f
                                   '(0)])))])
        js)))
  (for/list ([pr (in-list sorted)]
             [l-data (in-list full-lemma-year-numbers)])
    (match-define (cons lemma total) pr)
    (define text (lemma/string-ref corpus:lemma/string lemma))
    (list #;'text text
          #;'% (compute-percent total grand-total)
          #;'data l-data)))




  
