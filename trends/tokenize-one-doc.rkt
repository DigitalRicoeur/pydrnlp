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
  (define info (get-plain-instance-info doc))
  (define js-arg
    (hasheq lang (map base-segment-body (tei-document-segments doc))))
  (for*/fold ([doc:lemma/count empty:lemma/count]
              [doc:lemma/string empty:lemma/string]
              #:result (doc+strings (tokenized-document info doc:lemma/count)
                                    doc:lemma/string))
             ([seg-rslt (in-stream (trends-engine-tokenize py js-arg))]
              [tkn (in-list seg-rslt)])
    (match-define (token lemma text) tkn)
    (values (add1:lemma/count doc:lemma/count lemma)
            (update:lemma/string doc:lemma/string lemma text))))

