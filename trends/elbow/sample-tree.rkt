#lang typed/racket

(provide Sample-Tree
         sample-tree-add
         sequence->sample-tree
         sample-tree-total
         sample-tree-iterate-descending
         sample-tree-iterate-ascending)

(require "types.rkt")

;; ?? does it make sense to use floats for more things?

(define-type Sample-Tree (U #f sample-tree-node))

;; TODO move -total to a wrapper struct nonempty-sample-tree
;; TODO is there an alternative data structure that would
;;   be directly iterable w/o intermediate allocation?

(struct sample-tree-node
  ([total : Positive-Integer]
   [k : Element]
   [v : Weight]
   [left : Sample-Tree]
   [right : Sample-Tree])
  #:transparent)

(: sample-tree-total (-> Sample-Tree Nonnegative-Integer))
(define (sample-tree-total it)
  (if it
      (sample-tree-node-total it)
      0))

(: sample-tree-add (-> Sample-Tree Element sample-tree-node))
(define (sample-tree-add it k)
  (cond
    [it
     (define node-k (sample-tree-node-k it))
     (define node-total (sample-tree-node-total it))
     (cond
       [(< k node-k)
        (struct-copy
         sample-tree-node it
         [total (+ k node-total)]
         [left (sample-tree-add (sample-tree-node-left it) k)])]
       [(> k node-k)
        (struct-copy
         sample-tree-node it
         [total (+ k node-total)]
         [right (sample-tree-add (sample-tree-node-right it) k)])]
       [else
        (struct-copy
         sample-tree-node it
         [total (+ k node-total)]
         [v (add1 (sample-tree-node-v it))])])]
    [else
     (sample-tree-node k k 1 #f #f)]))

(: sequence->sample-tree (-> (Sequenceof Element) Sample-Tree))
(define (sequence->sample-tree e*)
  (for/fold ([it : Sample-Tree #f])
            ([e : Element e*])
    (sample-tree-add it e)))


(: make-sample-tree-iterate-direction
   (-> #:descending? Boolean (-> Sample-Tree
                                 (values (Listof Element)
                                         (Listof Weight)))))
(define (make-sample-tree-iterate-direction #:descending? descending?)
  (define-values [get-start-child get-end-child]
    (if descending?
        (values sample-tree-node-left sample-tree-node-right)
        (values sample-tree-node-right sample-tree-node-left)))
  (λ (this)
    (let loop : (values (Listof Element)
                        (Listof Weight)) ([e* : (Listof Element) null]
                                          [w* : (Listof Weight) null]
                                          [this : Sample-Tree this])
      (cond
        [this
         (define k (sample-tree-node-k this))
         (define v (sample-tree-node-v this))
         (define end-child (get-end-child this))
         (let-values ([{e* w*} (loop e* w* (get-start-child this))])
           (loop (cons k e*) (cons v w*) end-child))]
        [else
         (values e* w*)]))))

(: sample-tree-iterate-descending (-> Sample-Tree
                                      (values (Listof Element)
                                              (Listof Weight))))
(define sample-tree-iterate-descending
  (make-sample-tree-iterate-direction #:descending? #t))

(: sample-tree-iterate-ascending (-> Sample-Tree
                                      (values (Listof Element)
                                              (Listof Weight))))
(define sample-tree-iterate-ascending
  (make-sample-tree-iterate-direction #:descending? #f))
