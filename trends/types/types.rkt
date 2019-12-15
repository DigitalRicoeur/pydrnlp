#lang racket

(require ricoeur/tei
         racket/fasl
         racket/serialize
         "lemma-table.rkt")

(provide (all-from-out "lemma-table.rkt")
         (contract-out
          [struct tokenized-corpus
           ([lemma-table lemma-table?]
            [docs (instance-set/c tokenized-document?)])]
          [struct tokenized-document
            ([info plain-instance-info?]
             [lemma/count lemma/count?])]
          [segment-meta->fasl
           (-> segment-meta? (and/c bytes? immutable?))]
          [fasl->segment-meta
           (-> bytes? segment-meta?)]
          ))

(struct tokenized-corpus (lemma-table docs)
  #:transparent)

(struct tokenized-document (info lemma/count)
  #:property prop:instance-info
  (λ (this) (tokenized-document-info this))
  #:transparent)


(define (segment-meta->fasl it)
  (bytes->immutable-bytes
   (s-exp->fasl (serialize it))))

(define (fasl->segment-meta fasl)
  (deserialize (fasl->s-exp fasl)))
