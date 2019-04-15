#lang racket

(require ricoeur/tei
         "worker.rkt"
         "cache-db.rkt"
         "tokenize-one-doc.rkt"
         "types.rkt")

(provide (contract-out
          [get/build-tokenized-corpus
           (-> (instance-set/c tei-document?)
               tokenized-corpus?)]
          ))

(define (get/build-tokenized-corpus docs)
  (define checksum-table (tei-document-set->checksum-table docs))
  (with-tokenizer+cache-db
   #:quiet? #f
   #:tokenizer py
   (or (try-select-cached-tokenized-corpus #:docs docs
                                           #:checksum-table checksum-table)
       (build-tokenized-corpus #:docs docs
                               #:tokenizer py
                               #:checksum-table checksum-table))))


(define/db (build-tokenized-corpus #:docs docs
                                   #:tokenizer py
                                   #:checksum-table checksum-table)
  (for/fold/define ([l-tokenized-docs null]
                    [corpus:lemma/count empty:lemma/count]
                    [corpus:lemma/string empty:lemma/string])
                   ([doc (in-instance-set docs)])
    (match-define (doc+strings tokenized-document doc:lemma/string)
      (get/build-doc+strings doc #:tokenizer py))
    (values (cons tokenized-document
                  l-tokenized-docs)
            (union:lemma/count corpus:lemma/count
                               (tokenized-document-lemma/count tokenized-document))
            (union:lemma/string corpus:lemma/string
                                doc:lemma/string)))
  (define corpus
    (tokenized-corpus (instance-set l-tokenized-docs)
                      corpus:lemma/count
                      corpus:lemma/string))
  (insert-tokenized-corpus! corpus #:checksum-table checksum-table)
  corpus)
                                
(TODO/void add tei-document-checksum/string ?)

(define/db (get/build-doc+strings doc #:tokenizer py)
  (cond
    [(try-select-doc+strings doc)]
    [else
     (define d+s
       (tokenize-one-doc doc #:tokenizer py))
     (insert-doc+strings! d+s #:checksum-string (symbol->string
                                                 (tei-document-checksum doc)))
     d+s]))


(define (tei-document-set->checksum-table docs)
  (TODO/void tei-document-set->checksum-table #: probably better elsewhere)
  (for/hasheq ([doc (in-instance-set docs)])
    (values (instance-title/symbol doc)
            (tei-document-checksum doc))))
