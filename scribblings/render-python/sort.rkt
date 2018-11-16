#lang racket

(require pydrnlp/doc
         adjutor)

(module+ test
  (require rackunit
           (submod "..")))

(provide order-spec/c
         (contract-out
          [organize-package-docs
           (-> (hash/c #:immutable #t
                       (listof symbol?)
                       (or/c 'not-found
                             'error-during-import
                             module-doc?))
               order-spec/c
               (listof package-doc-node?))]
          [struct package-doc-node
            ([modpath (listof symbol?)]
             [name symbol?]
             [v (or/c 'not-found
                      'error-during-import
                      module-doc?)]
             [children (listof package-doc-node?)])]
          ))

(struct module-to-be-sorted (prefix name modpath v)
  #:transparent)

(struct package-doc-node (modpath name v children)
  #:transparent)

(define (parsed->to-be-sorted parsed)
  (for/list ([{modpath v} (in-immutable-hash parsed)])
    (module-to-be-sorted (drop-right modpath 1)
                         (last modpath)
                         modpath
                         v)))

(define (organize-package-docs parsed specs)
  (define ot (normalize-order-specs specs))
  (define (mtbs-modpath-length v)
    (length (module-to-be-sorted-modpath v)))
  (order-tree-sort-nodes
   ot
   (let loop ([to-go (parsed->to-be-sorted parsed)]
              [ot ot])
     (match (sort to-go < #:key mtbs-modpath-length)
       ['() '()]
       [(cons (module-to-be-sorted prefix name modpath v)
              to-go)
        (define-values {children others}
          (partition (match-lambda
                       [(module-to-be-sorted _ _ child-mp _)
                        (modpath-is-ancestor-of? modpath child-mp)])
                     to-go))
        (define sorted-children
          (let ([children-ot (cdr (or (assq name ot) '(_)))])
            (order-tree-sort-nodes children-ot
                                   (loop children children-ot))))
        (cons (package-doc-node modpath
                                name
                                v
                                sorted-children)
              (loop others ot))]))))

(define (modpath-is-ancestor-of? mpa mpb)
  (and (infix: (length mpa) < (length mpb))
       (for/and ([a (in-list mpa)]
                 [b (in-list mpb)])
         (eq? a b))))

(module+ test
  (check-true
   (modpath-is-ancestor-of? '(pydrnlp)
                            '(pydrnlp tokenizer run)))
  (check-true
   (modpath-is-ancestor-of? '(pydrnlp tokenizer)
                            '(pydrnlp tokenizer run))))


(define/final-prop order-spec/c
  (flat-rec-contract order-specs/c
    (listof
     (or/c symbol?
           (list/c symbol? order-specs/c)))))


(define/final-prop order-tree/c
  (flat-rec-contract order-tree/c
    (listof (cons/c symbol?
                    order-tree/c))))

(define/contract (normalize-order-specs l-specs)
  (-> order-spec/c order-tree/c)
  (map (match-lambda
         [(list sym l-specs)
          (cons sym (normalize-order-specs l-specs))]
         [sym
          (list sym)])
       l-specs))

(define/contract (order-tree-sort-nodes ot nodes)
  (-> order-tree/c (listof package-doc-node?) 
      (listof package-doc-node?))
  (define-values {specified others}
    (partition (match-lambda
                 [(package-doc-node _ name _ _)
                  (assq name ot)])
               nodes))
  (append
   (filter-map (match-lambda
                 [(cons name _)
                  (for/first ([n (in-list specified)]
                              #:when (eq? name (package-doc-node-name n)))
                    n)])
               ot)
   (sort others
         symbol<?
         #:key package-doc-node-name)))


              

  
