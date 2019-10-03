#lang racket

(require ricoeur/tei
         gregor
         ricoeur/stdlib/json
         "types.rkt"
         "cache-tokenize-corpus.rkt")

(provide make-trends-data)

(define (make-trends-data all-docs)
  (define t-c
    (tokenized-corpus-enforce-threshold
     (get/build-tokenized-corpus
      (for/instance-set ([doc (in-instance-set all-docs)]
                         #:when (eq? 'en (instance-language doc))
                         #:when (eq? 'book (instance-book/article doc)))
        doc))))
  (define corpus:lemma/string (tokenized-corpus-lemma/string t-c))
  (define corpus:lemma/count (tokenized-corpus-lemma/count t-c))
  (define grand-total (total:lemma/count corpus:lemma/count))
  (define l-year-data
    (sort (tokenized-corpus->l-year-data t-c) #:key year-data-year <))
  (define all-years-with-titles
    (map (match-lambda 
           [(year-data year _ titles _)
            (list year titles)])
         l-year-data))
  (define listof-text+%+sparse-data
    (for/list ([pr (in-list 
                    (sort (hash->list (lemma/count-hsh corpus:lemma/count))
                          #:key cdr >))])
      (match-define (cons lemma total) pr)
      (define text (lemma/string-ref corpus:lemma/string lemma))
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
               (total:lemma/count l/c)
               (sort (map instance-title grp) title<?)
               l/c)))

(define (tokenized-corpus->threshold t-c)
  ;; will keep top 1000
  190)

(define (tokenized-corpus-enforce-threshold t-c)
  (define threshold (tokenized-corpus->threshold t-c))
  (define (l/c-enforce-threshold l/c)
    (lemma/count (for/hasheq ([{k v} (in-immutable-hash (lemma/count-hsh l/c))]
                              #:unless (< v threshold))
                   (values k v))))
  (match-define (tokenized-corpus docs l/c l/s) t-c)
  (tokenized-corpus (for/instance-set ([d (in-instance-set docs)])
                      (match-define (tokenized-document info l/c) d)
                      (tokenized-document info (l/c-enforce-threshold l/c)))
                    (l/c-enforce-threshold l/c)
                    l/s))
   




  
