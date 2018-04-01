#lang racket

(require racket/hash
         adjutor
         )

(provide tokenizer?
         lemma->count/c
         lemma->string/c
         (contract-out
          [tokenizer-revision
           (-> tokenizer? real?)]
          [tokenize
           (-> tokenizer?
               (listof (cons/c string?
                               (listof (cons/c natural-number/c
                                               string?))))
               tokenized-result?)]
          [struct tokenized-result
            ([lemma->string lemma->string/c]
             [results (listof (cons/c string
                                      (listof (cons/c natural-number/c
                                                      lemma->count/c))))])
            #:omit-constructor]
          [union:lemma->count
           (-> lemma->count/c lemma->count/c ... lemma->count/c)]
          [union:lemma->string
           (-> lemma->string/c lemma->string/c ... lemma->string/c)]
          ))

(define lemma->count/c
  (hash/c symbol?
          exact-positive-integer?
          #:immutable #t))

(define lemma->string/c
  (hash/c symbol?
          string?
          #:immutable #t))

(define (tokenizer? v)
  #f)

(define (tokenizer-revision tokenizer)
  (error 'tokenizer-revision "TODO"))

(define (tokenize tokenizer arg)
  (error 'tokenize "TODO"))


(define (arg->jsexpr arg)
  (map (match-lambda
         [(cons key segments)
          (hasheq 'key key
                  'segments (map (match-lambda
                                   [(cons counter body)
                                    (hasheq 'counter counter
                                            'body body)])
                                 segments))])
       arg))


(struct tokenized-result (lemma->string results) #:transparent)

(define (handle-jsexpr-result js)
  (for/fold/define ([super:lemma->string #hasheq()]
                    [results null])
                   ([doc (in-list js)])
    (match-define (hash-table ['key key]
                              ['segments segments])
      doc)
    (for/fold/define ([lemma->string super:lemma->string]
                      [seg-results null])
                     ([seg (in-list segments)])
      (match-define (hash-table ['counter counter]
                                ['tokenized tokenized])
        seg)
      (define-values {new:lemma->string lemma->count}
        (handle-tokenized tokenized))
      (values (union:lemma->string lemma->string new:lemma->string)
              (cons (cons counter lemma->count)
                    seg-results)))
    (values (union:lemma->string super:lemma->string lemma->string)
            (cons (cons key seg-results)
                  results)))
  (tokenized-result super:lemma->string results))


(define (handle-tokenized l-objects)
  (for/fold ([lemma->string #hasheq()]
             [lemma->count #hasheq()])
            ([obj (in-list l-objects)])
    (match obj
      [(hash-table ['text text]
                   ['lemma (app string->symbol lemma)]
                   ['count count])
       (values (hash-set lemma->string lemma text)
               (hash-set lemma->count lemma count))])))

(define (union:lemma->count . hashes)
  (apply hash-union
         #:combine +
         hashes))

(define (union:lemma->string . hashes)
  (apply hash-union
         #:combine (λ (a b)
                     (if (equal? a (string-foldcase a))
                         a
                         b))
         hashes))

