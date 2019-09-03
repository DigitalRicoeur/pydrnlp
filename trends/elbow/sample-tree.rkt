#lang typed/racket

(provide Sample-Tree
         sample-tree-node?
         sample-tree-add
         sequence->sample-tree
         in-sample-tree)

(require syntax/parse/define)

(define-type Element Positive-Integer)
(define-type Weight Positive-Integer)

(define-type Sample-Tree (U #f sample-tree-node))

(struct sample-tree-node
  ([k : Element]
   [v : Weight]
   [left : Sample-Tree]
   [right : Sample-Tree])
  #:transparent)

(: sample-tree-add (-> Sample-Tree Element sample-tree-node))
(define (sample-tree-add it k)
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

(: sequence->sample-tree (-> (Sequenceof Element) Sample-Tree))
(define (sequence->sample-tree e*)
  (for/fold ([it : Sample-Tree #f])
            ([e : Element e*])
    (sample-tree-add it e)))



(define-type Position
  (U #f (-> (values Element Weight (-> Position)))))

(: make-do-iterate-direction
   (-> (-> sample-tree-node Sample-Tree)
       (-> sample-tree-node Sample-Tree)
       (-> Sample-Tree Position)))
(define (make-do-iterate-direction get-start-child
                                   get-end-child)
  (λ (this)
    (let loop : Position ([this : Sample-Tree this]
                          [continue-position : Position #f])
      (cond
        [this
         (define k (sample-tree-node-k this))
         (define v (sample-tree-node-v this))
         (define end-child (get-end-child this))
         ;; can we minimize allocation ?
         (define (get-new-continue-position)
           (loop end-child continue-position))
         (loop
          (get-start-child this)
          (λ ()
            (values k v get-new-continue-position)))]
        [else
         continue-position]))))


(: do-iterate-high2low (-> Sample-Tree Position))
(define do-iterate-high2low
  (make-do-iterate-direction sample-tree-node-right
                             sample-tree-node-left))

(: do-iterate-low2high (-> Sample-Tree Position))
(define do-iterate-low2high
  (make-do-iterate-direction sample-tree-node-left
                             sample-tree-node-right))




(define-sequence-syntax in-sample-tree 
  (λ ()
    ;; not well supported by TR
    (error 'in-sample-tree/proc "TODO ?"))
  (syntax-parser
    [[{e-high:id w-high:id e-low:id w-low:id}
      (_ sample-tree-expr:expr)]
     #'[{e-high w-high e-low w-low}
        (:do-in ([{it} (ann sample-tree-expr Sample-Tree)])
                #true
                ([high2low (do-iterate-high2low it)]
                 [low2high (do-iterate-low2high it)])
                (and (ann high2low Position) (ann low2high Position))
                ([{e-high w-high k-high e-low w-low k-low}
                  (let-values ([{e-high w-high k-high} (high2low)]
                               [{e-low w-low k-low} (low2high)])
                    (values e-high w-high k-high e-low w-low k-low))])
                #true
                #true
                [(k-high) (k-low)])]]))




#;
(for/list : (Listof Any) ([{e-high w-high e-low w-low}
                           (in-sample-tree
                            (sequence->sample-tree '(5 2 4 3 5 5 1 2 9)))])
  (vector-immutable e-high w-high e-low w-low))
