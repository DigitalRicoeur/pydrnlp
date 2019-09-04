#lang typed/racket

(provide Sample-Tree
         sample-tree-add
         sequence->sample-tree
         sample-tree-iterate-descending
         sample-tree-iterate-ascending)

(require (submod racket/performance-hint begin-encourage-inline))

(define-type Sample-Tree (U #f sample-tree-node))

;; TODO is there an alternative data structure that would
;;   be directly iterable w/o intermediate allocation?

;; Red-Black (self-balancing) might help if insertion
;; tends to be ordered (vid. Okasaki 3.3 p24).

(struct sample-tree-node
  ([k : Positive-Integer]
   [v : Positive-Integer]
   [left : Sample-Tree]
   [right : Sample-Tree])
  #:transparent)

(: sample-tree-add (-> Sample-Tree Positive-Integer sample-tree-node))
(define (sample-tree-add it k)
  ;; TODO: (Okasaki p14 ex2.2)
  ;; Current worst-case is (* 2 depth) comparisons;
  ;; can do worst-case (add1 depth) instead by remembering
  ;; possibly-equal element and checking = only on hitting bottom.
  (cond
    [it
     (define node-k (sample-tree-node-k it))
     (cond
       [(< k node-k)
        (struct-copy
         sample-tree-node it
         [left (sample-tree-add (sample-tree-node-left it) k)])]
       [(> k node-k)
        (struct-copy
         sample-tree-node it
         [right (sample-tree-add (sample-tree-node-right it) k)])]
       [else
        (struct-copy
         sample-tree-node it
         [v (add1 (sample-tree-node-v it))])])]
    [else
     (sample-tree-node k 1 #f #f)]))

(: sequence->sample-tree (-> (Sequenceof Positive-Integer) Sample-Tree))
(define (sequence->sample-tree e*)
  (for/fold ([it : Sample-Tree #f])
            ([e : Positive-Integer e*])
    (sample-tree-add it e)))



(: sample-tree-iterate-descending (-> Sample-Tree
                                      (values (Listof Positive-Integer)
                                              (Listof Positive-Integer))))
(: sample-tree-iterate-ascending (-> Sample-Tree
                                     (values (Listof Positive-Integer)
                                             (Listof Positive-Integer))))
(define-values [sample-tree-iterate-descending sample-tree-iterate-ascending]
  (let ()
    
    (define-syntax-rule (make-iterate name get-start-child get-end-child)
      (let ([name
             (λ ([this : Sample-Tree])
               (let loop : (values (Listof Positive-Integer)
                                   (Listof Positive-Integer))
                 ([e* : (Listof Positive-Integer) null]
                  [w* : (Listof Positive-Integer) null]
                  [this : Sample-Tree this])
                 (cond
                   [this
                    (define k (sample-tree-node-k this))
                    (define v (sample-tree-node-v this))
                    (define end-child (get-end-child this))
                    (let-values ([{e* w*} (loop e* w* (get-start-child this))])
                      (loop (cons k e*) (cons v w*) end-child))]
                   [else
                    (values e* w*)])))])
        name))
    (values (make-iterate sample-tree-iterate-descending
                          sample-tree-node-left
                          sample-tree-node-right)
            (make-iterate sample-tree-iterate-ascending
                          sample-tree-node-right
                          sample-tree-node-left))))
