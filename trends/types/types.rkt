#lang racket

(require ricoeur/tei
         racket/fasl
         racket/serialize
         "lemma-hash-tables.rkt")

(provide (all-from-out "lemma-hash-tables.rkt")
         (contract-out
          [struct tokenized-corpus
           ([docs (instance-set/c tokenized-document?)]
            [lemma/count lemma/count?]
            [lemma/string lemma/string?])]
          [struct tokenized-document
            ([info plain-instance-info?]
             [lemma/count lemma/count?])]
          [struct doc+strings
            ([tokenized-document tokenized-document?]
             [lemma/string lemma/string?])]
          [lemma/count->fasl
           (-> lemma/count? (and/c bytes? immutable?))]
          [lemma/string->fasl
           (-> lemma/string? (and/c bytes? immutable?))]
          [segment-meta->fasl
           (-> segment-meta? (and/c bytes? immutable?))]
          [fasl->lemma/count
           (-> bytes? lemma/count?)]
          [fasl->lemma/string
           (-> bytes? lemma/string?)]
          [fasl->segment-meta
           (-> bytes? segment-meta?)]
          ))

(struct tokenized-corpus (docs lemma/count lemma/string)
  #:transparent)

(TODO/void separate lemma/string by language)

(struct tokenized-document (info lemma/count)
  #:property prop:instance-info
  (λ (this) (tokenized-document-info this))
  #:transparent)

(struct doc+strings (tokenized-document lemma/string)
  #:transparent)

(define (lemma/count->fasl it)
  (bytes->immutable-bytes
   (s-exp->fasl (lemma/count-hsh it))))

(define (lemma/string->fasl it)
  (bytes->immutable-bytes
   (s-exp->fasl (lemma/string-hsh it))))

(define (segment-meta->fasl it)
  (bytes->immutable-bytes
   (s-exp->fasl (serialize it))))

(define (fasl->lemma/count fasl)
  (lemma/count (fasl->s-exp fasl)))

(define (fasl->lemma/string fasl)
  (lemma/string (fasl->s-exp fasl)))

(define (fasl->segment-meta fasl)
  (deserialize (fasl->s-exp fasl)))
