#lang racket

(require racket/hash
         adjutor
         json
         )

(provide lemma->count/c
         lemma->string/c
         tokenize-arg/c
         tokenization-results/c
         (contract-out
          [struct tokenized-document
            ([lemma->string lemma->string/c]
             [segments (hash/c natural-number/c
                               lemma->count/c
                               #:immutable #t)])
            #:omit-constructor]
          [union:lemma->count
           (-> lemma->count/c lemma->count/c ... lemma->count/c)]
          [union:lemma->string
           (-> lemma->string/c lemma->string/c ... lemma->string/c)]
          ))

(module+ private
  (provide tokenizer-jsexpr-result/c
           (contract-out
            [tokenize-arg->jsexpr
             (-> tokenize-arg/c jsexpr?)]
            [handle-tokenize-jsexpr-result
             (-> tokenizer-jsexpr-result/c
                 tokenization-results/c)]
            )))

(struct tokenized-document (lemma->string segments)
  #:transparent)

(define tokenize-arg/c
  (hash/c string?
          (hash/c natural-number/c
                  string?)
          #:immutable #t))

(define lemma->count/c
  (hash/c symbol?
          exact-positive-integer?
          #:immutable #t))

(define lemma->string/c
  (hash/c symbol?
          string?
          #:immutable #t))

(define tokenization-results/c
  (hash/c string?
          tokenized-document?
          #:immutable #t))

(define (tokenize-arg->jsexpr arg)
  (for/list ([{key segments} (in-immutable-hash arg)])
    (hasheq 'key key
            'segments
            (for/list ([{counter body}
                        (in-immutable-hash segments)])
              (hasheq 'counter counter
                      'body body)))))


(define tokenizer-jsexpr-result/c
  ;; TODO: better contract abstraction
  (listof (hash/dc
           [k symbol?]
           [v (k)
              (case k
                [(key)
                 string?]
                [(segments)
                 (listof tokenized-segment-jsexpr/c)]
                [else
                 none/c])]
           #:immutable #t)))

(define tokenized-segment-jsexpr/c
  ;; TODO: better contract abstraction
  (hash/dc
   [k symbol?]
   [v (k)
      (case k
        [(counter)
         natural-number/c]
        [(tokenized)
         (listof text+lemma+count-jsexpr/c)]
        [else
         none/c])]
   #:immutable #t))

(define text+lemma+count-jsexpr/c
  ;; TODO: better contract abstraction
  (hash/dc
   [k symbol?]
   [v (k)
      (case k
        [(text) string?]
        [(lemma) string?]
        [(count) natural-number/c]
        [else none/c])]
   #:immutable #t))
  

(define (handle-tokenize-jsexpr-result js)
  (for/hash ([doc (in-list js)])
    (match-let ([(hash-table ['key key]
                             ['segments segments])
                 doc])
      (for/fold ([lemma->string #hasheq()]
                 [seg-results #hasheqv()]
                 #:result
                 (values key
                         (tokenized-document lemma->string
                                             seg-results)))
                ([seg (in-list segments)])
        (match-let*-values ([{(hash-table ['counter counter]
                                          ['tokenized tokenized])}
                             seg]
                            [{new:lemma->string lemma->count}
                             (handle-tokenized tokenized)])
          (values (union:lemma->string lemma->string new:lemma->string)
                  (hash-set seg-results counter lemma->count)))))))



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




(define (make-union-proc combine)
  (case-lambda
    ;; Trivial
    [(x) x]
    ;; Common
    [(a b)
     (hash-union a b #:combine combine)]
    ;; General
    [hashes
     (apply hash-union
            #:combine combine
            hashes)]))

(define union:lemma->count
  (make-union-proc +))

(define cache:string-folds-to-self?
  ;; should this be a weak set ?
  (make-weak-hash))

(define (string-folds-to-self? str)
  (hash-ref cache:string-folds-to-self?
            str
            (λ ()
              (let ([rslt (equal? str (string-foldcase str))])
                (hash-set! cache:string-folds-to-self? str rslt)
                rslt))))

(define union:lemma->string
  (make-union-proc (λ (a b)
                     (if (or (equal? a b)
                             (string-folds-to-self? a))
                         a
                         b))))
    




