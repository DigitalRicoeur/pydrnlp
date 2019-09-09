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
  (for/fold/define ([t-args null])
                   ([seg (in-list (tei-document-segments doc))]
                    [i (in-naturals)])
    (match-define (base-segment _ body) seg)
    (values (cons (json-segment lang i body) t-args)))
  (for*/fold ([doc:lemma/count empty:lemma/count]
              [doc:lemma/string empty:lemma/string]
              #:result (doc+strings (tokenized-document info doc:lemma/count)
                                    doc:lemma/string))
             ([rslt (in-stream (trends-engine-tokenize py t-args))]
              [tkn (in-list (trends-segment-result-tokens rslt))])
    (match-define (token lemma text) tkn)
    (values (add1:lemma/count doc:lemma/count lemma)
            (update:lemma/string doc:lemma/string lemma text))))

