#lang racket

(require ricoeur/tei
         "worker.rkt"
         "types.rkt")

(provide (contract-out
          [tokenize-one-doc
           (-> tei-document?
               #:tokenizer trends-engine?
               lemma-table?)]
          ))

(define (tokenize-one-doc doc #:tokenizer py)
  (define lang (instance-language doc))
  (define info (get-plain-instance-info doc))
  (define js-arg
    (hasheq lang (map base-segment-body (tei-document-segments doc))))
  (for*/fold ([tbl empty-lemma-table])
             ([seg-rslt (in-stream (trends-engine-tokenize py js-arg))]
              [tkn (in-list seg-rslt)])
    (match-define (token lemma text) tkn)
    (lemma-table-update tbl lemma text)))

