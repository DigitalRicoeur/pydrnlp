#lang typed/racket

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


(define-type Iterate-Direction-Position
  (U #f (-> (Values Element Weight Iterate-Direction-Continue))))
(define-type Iterate-Direction-Continue
  (-> Iterate-Direction-Position))

(: do-iterate-high2low (-> Sample-Tree Iterate-Direction-Position))
(: do-iterate-high2low (-> Sample-Tree Iterate-Direction-Position))
(define-values [do-iterate-high2low do-iterate-low2high]
  (let ()
    (define-type Continue Iterate-Direction-Continue)
    (define-type Position Iterate-Direction-Position)
    (: make-do-iterate-direction
       (-> (-> sample-tree-node Sample-Tree)
           (-> sample-tree-node Sample-Tree)
           (-> Sample-Tree Position)))
    (define (make-do-iterate-direction get-start-child
                                       get-end-child)
      (: do-iterate (-> Sample-Tree Position))
      (define (do-iterate this)
        (iterate-advance-to-start this (λ () #f)))
      (: iterate-advance-to-start (-> Sample-Tree Continue Position))
      (define (iterate-advance-to-start this continue)
        (match this
          [(sample-tree-node k v _ _)
           (define end-child (get-end-child this))
           (iterate-advance-to-start
            (get-start-child this)
            (λ ()
              (iterate-up k v end-child continue)))]
          [_
           (continue)]))
      (: iterate-up (-> Element Weight Sample-Tree Continue Position))
      (define (iterate-up k v end-child continue)
        (λ ()
          (values k v (λ ()
                        (iterate-advance-to-start end-child
                                                  continue)))))
      do-iterate)
    (values
     ;; do-iterate-high2low
     (make-do-iterate-direction sample-tree-node-right
                                sample-tree-node-left)
     ;; do-iterate-low2high
     (make-do-iterate-direction sample-tree-node-left
                                sample-tree-node-right))))

                
    