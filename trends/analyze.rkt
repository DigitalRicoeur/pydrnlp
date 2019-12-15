#lang racket

(require ricoeur/tei
         gregor
         ricoeur/stdlib/json
         "types.rkt"
         (submod "types/lemma-table.rkt" hacks)
         "cache-tokenize-corpus.rkt")

(provide trends-data-internal?
         (contract-out
          [make-trends-data
           (make-trends-data/c (instance-set/c tei-document?))]
          ;; low-level
          [make-trends-data-internal
           (-> (instance-set/c tei-document?)
               trends-data-internal?)]
          [trends-data-internal->data
           (make-trends-data/c trends-data-internal?)]
          [trends-data-internal->count-table
           (-> trends-data-internal?
               (and/c
                (cons/c
                 (cons/c "Term" (cons/c "Overall" (listof exact-positive-integer?)))
                 (cons/c (cons/c "TOTAL" (listof natural-number/c))
                         (listof (cons/c string? (listof natural-number/c)))))
                (λ (lsts)
                  (define len0 (length (car lsts)))
                  (andmap (λ (lst) (= len0 (length lst))) lsts))))]
          ))

(define (make-trends-data/c arg/c)
  (-> arg/c
      (values (listof (list/c exact-positive-integer?
                              (listof string?)))
              (listof (list/c string?
                              (and/c (between/c 0 100)
                                     inexact?)
                              (listof (list/c exact-positive-integer?
                                              (and/c (between/c 0 100)
                                                     inexact?))))))))

(struct trends-data-internal (tokenized-corpus l-year-data))

(define (make-trends-data-internal all-docs)
  (define t-c
    (tokenized-corpus-enforce-threshold
     (get/build-tokenized-corpus
      (for/instance-set ([doc (in-instance-set all-docs)]
                         #:when (eq? 'en (instance-language doc))
                         #:when (eq? 'book (instance-book/article doc)))
        doc))))
  (trends-data-internal t-c
                        (sort (tokenized-corpus->l-year-data t-c) #:key year-data-year <)))


(define (make-trends-data all-docs)
  (trends-data-internal->data
   (make-trends-data-internal all-docs)))

(define (trends-data-internal->data ir)
  (match-define
    (trends-data-internal (and t-c (tokenized-corpus corpus-tbl _))
                          l-year-data)
    ir)
  (define grand-total (lemma/count-total corpus-tbl))
  (define all-years-with-titles
    (map (match-lambda 
           [(year-data year _ titles _)
            (list year titles)])
         l-year-data))
  (define listof-text+%+sparse-data
    (for/list ([pr (in-list 
                    (sort (hash->list (lemma/count-hsh
                                       (lemma-table->lemma/count corpus-tbl)))
                          #:key cdr >))])
      (match-define (cons lemma total) pr)
      (define text (lemma/string-ref corpus-tbl lemma))
      (define % (compute-percent total grand-total))
      (define sparse-data
        (for*/list ([y-d (in-list l-year-data)]
                    [count (in-value
                            (lemma/count-ref (year-data-lemma/count y-d) lemma))]
                    #:when count)
          (match-define (year-data year total _ _) y-d)
          (list year (compute-percent count total))))
      (list text % sparse-data)))
  (values all-years-with-titles
          listof-text+%+sparse-data))


(define (compute-percent count total)
  (exact->inexact (* 100 (/ count total))))

(struct year-data (year total titles lemma/count)
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
               (lemma/count-total l/c)
               (sort (map instance-title grp) title<?)
               l/c)))

(define (tokenized-corpus->threshold t-c)
  ;; will keep top 1000
  190)

(define (tokenized-corpus-enforce-threshold t-c)
  (define threshold (tokenized-corpus->threshold t-c))
  (match-define (tokenized-corpus old-corpus-tbl docs) t-c)
  (define new-corpus-tbl
    (lemma-table-filter old-corpus-tbl
                        (λ (k n str) (not (< n threshold)))))
  (define HACK-new-corpus-count-hsh
    (lemma/count-hsh (lemma-table->lemma/count new-corpus-tbl)))
  (tokenized-corpus
   new-corpus-tbl
   (for/instance-set ([d (in-instance-set docs)])
     (match-define (tokenized-document info old-doc-l/c) d)
     (tokenized-document
      info
      (HACK-make-lemma/count
       ;; iterate over what is likely the smaller hash
       (for*/hasheq ([k (in-immutable-hash-keys HACK-new-corpus-count-hsh)]
                     [v (in-value (lemma/count-ref old-doc-l/c k))]
                     #:when v)
         (values k v)))))))



(define (trends-data-internal->count-table ir)
  (match-define
    (trends-data-internal (and t-c (tokenized-corpus tbl _))
                          l-year-data)
    ir)
  (list*
   (list* "Term" "Overall" (map year-data-year l-year-data))
   (list* "TOTAL" (lemma/count-total tbl) (map year-data-total l-year-data))
   (sort
    (for/list ([{k corpus-count}
                (in-immutable-hash ;; ugly
                 (lemma/count-hsh (lemma-table->lemma/count tbl)))])
      (list* (lemma/string-ref tbl k)
             corpus-count
             (map (match-lambda
                    [(year-data _ _ _ l/c)
                     (or (lemma/count-ref l/c k) 0)])
                  l-year-data)))
    #:key car
    string-ci<?)))

  
