#lang racket

(require ricoeur/tei
         "worker.rkt"
         "types.rkt")

(provide (contract-out
          [tokenize-one-doc
           (-> tei-document?
               #:tokenizer trends-engine?
               doc+strings?)]
          ))

(define (tokenize-one-doc doc #:tokenizer py)
  (define lang (instance-language doc))
  (for/fold/define ([hsh:i->meta #hasheqv()]
                    [t-args null])
                   ([seg (in-list (tei-document-segments doc))]
                    [i (in-naturals)])
    (match-define (base-segment meta body) seg)
    (values (hash-set hsh:i->meta i meta)
            (cons (json-segment lang i body) t-args)))
  (for/fold/define ([segs null]
                    [doc:lemma/count empty:lemma/count]
                    [doc:lemma/string empty:lemma/string])
                   ([rslt (in-list (trends-engine-tokenize py t-args))])
    (match-define (trends-segment-result i l-tkns) rslt)
    (define meta (hash-ref hsh:i->meta i))
    (for/fold ([seg:lemma/count empty:lemma/count]
               [doc:lemma/string doc:lemma/string]
               #:result
               (values (cons (tokenized-segment meta seg:lemma/count)
                             segs)
                       (union:lemma/count doc:lemma/count seg:lemma/count)
                       doc:lemma/string))
              ([tkn (in-list l-tkns)])
      (match-define (token lemma text) tkn)
      (values (add1:lemma/count seg:lemma/count lemma)
              (update:lemma/string doc:lemma/string lemma text))))
  (doc+strings (tokenized-document
                (get-plain-instance-info doc)
                doc:lemma/count
                segs) ;; these are already sorted
               doc:lemma/string))

