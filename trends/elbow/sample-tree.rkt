#lang typed/racket

(provide Sample-Tree
         sample-tree-node?
         sample-tree-add
         sequence->sample-tree
         sample-tree-sequence-length
         sample-tree-max-element
         sample-tree-effective-samples-count
         sample-tree-iterate/arrays/ascending
         sample-tree-iterate/arrays/descending
         sample-tree-iterate/flvectors/ascending
         sample-tree-iterate/flvectors/descending)

(require math/array
         math/flonum
         syntax/parse/define
         (for-syntax racket/base))

(define-type Sample-Tree (U #f sample-tree-node))

(struct sample-tree-node
  ([nodes-count : Positive-Integer]
   [k : Positive-Integer]
   [v : Positive-Integer]
   [left : Sample-Tree]
   [right : Sample-Tree])
  #:transparent)

;; We can expect insertion order to be fairly random,
;; since it will come from iterating over lemma/count tables,
;; so we shouldn't need to worry about balancing these.

(: sample-tree-add (-> Sample-Tree Positive-Integer sample-tree-node))
(define (sample-tree-add it k)
  ;; TODO: (Okasaki p14 ex2.2)
  ;; Current worst-case is (* 2 depth) comparisons;
  ;; can do worst-case (add1 depth) instead by remembering
  ;; possibly-equal element and checking = only on hitting bottom.
  (cond
    [it
     (define node-k (sample-tree-node-k it))
     (define (update-children [left : Sample-Tree] [right : Sample-Tree])
       (struct-copy
        sample-tree-node it
        [nodes-count (add1 (+ (sample-tree-sequence-length left)
                              (sample-tree-sequence-length right)))]
        [left left]
        [right right]))
     (cond
       [(< k node-k)
        (update-children (sample-tree-add (sample-tree-node-left it) k)
                         (sample-tree-node-right it))]
       [(> k node-k)
        (update-children (sample-tree-node-left it)
                         (sample-tree-add (sample-tree-node-right it) k))]
       [else
        (struct-copy
         sample-tree-node it
         [v (add1 (sample-tree-node-v it))])])]
    [else
     (sample-tree-node 1 k 1 #f #f)]))


(: sample-tree-sequence-length (-> Sample-Tree Nonnegative-Integer))
(define (sample-tree-sequence-length it)
  (if it (sample-tree-node-nodes-count it) 0))


(: sequence->sample-tree (-> (Sequenceof Positive-Integer) Sample-Tree))
(define (sequence->sample-tree e*)
  (for/fold ([it : Sample-Tree #f])
            ([e : Positive-Integer e*])
    (sample-tree-add it e)))


(: sample-tree-max-element (-> Sample-Tree Nonnegative-Integer))
(define (sample-tree-max-element it)
  (cond
    [(not it)
     0]
    [(sample-tree-node-right it)
     => sample-tree-max-element]
    [else
     (sample-tree-node-k it)]))


(: sample-tree-effective-samples-count (-> Sample-Tree Nonnegative-Integer))
(define (sample-tree-effective-samples-count it)
  (if it
      (+ (sample-tree-node-v it)
         (sample-tree-effective-samples-count (sample-tree-node-left it))
         (sample-tree-effective-samples-count (sample-tree-node-right it)))
      0))
    

(define-type Emit! (-> Positive-Integer Positive-Integer AnyValues))

(: do-iterate-descending! (-> Sample-Tree Emit! AnyValues))
(: do-iterate-ascending! (-> Sample-Tree Emit! AnyValues))
(define-values [do-iterate-descending! do-iterate-ascending!]
  (let ()
    (define-syntax-rule (make-do-iterate! name get-start-child get-end-child)
      (let ()
        (define (name [this : Sample-Tree] [emit! : Emit!])
          (let loop! : Void ([this : Sample-Tree this])
            (cond
              [this
               (define k (sample-tree-node-k this))
               (define v (sample-tree-node-v this))
               (define end-child (get-end-child this))
               ;; don't keep this reachable
               (loop! (get-start-child this))
               (emit! k v)
               (loop! end-child)]
              [else
               (void)])))
        name))
    (values (make-do-iterate! do-iterate-descending!
                              sample-tree-node-left
                              sample-tree-node-right)
            (make-do-iterate! do-iterate-ascending!
                              sample-tree-node-right
                              sample-tree-node-left))))


(: run-iterate-flvectors (-> Sample-Tree
                             (-> Sample-Tree Emit! AnyValues)
                             (values FlVector FlVector)))
(define (run-iterate-flvectors this do-iterate!)
  (define len (sample-tree-sequence-length this))
  (if (= 0 len)
      (values (flvector) (flvector))
      (let* ([ve (make-flvector len)]
             [vw (make-flvector len)]
             [len : Index (flvector-length ve)]
             [mut-index : Nonnegative-Fixnum 0])
        (do-iterate! this (λ ([e : Positive-Integer]
                              [w : Positive-Integer])
                            (define i mut-index)
                            (assert (< i len))
                            (flvector-set! ve i (fl e))
                            (flvector-set! vw i (fl w))
                            (set! mut-index (add1 i))))
        (assert (= mut-index len))
        (values ve vw))))


(: run-iterate-arrays (-> Sample-Tree
                          (-> Sample-Tree Emit! AnyValues)
                          (values (Array Positive-Integer)
                                  (Array Positive-Integer))))
(define (run-iterate-arrays this do-iterate!)
  (define len (sample-tree-sequence-length this))
  (if (= 0 len)
      (values (array #[]) (array #[]))
      (let* ([ve : (Mutable-Vectorof Positive-Integer)
                 (make-vector len 1)]
             [vw : (Mutable-Vectorof Positive-Integer)
                 (make-vector len 1)]
             [len : Index (vector-length ve)]
             [mut-index : Nonnegative-Fixnum 0])
        (do-iterate! this (λ ([e : Positive-Integer]
                              [w : Positive-Integer])
                            (define i mut-index)
                            (assert (< i len))
                            (vector-set! ve i e)
                            (vector-set! vw i w)
                            (set! mut-index (add1 i))))
        (assert (= mut-index len))
        (values (vector->array ve)
                (vector->array vw)))))


(define-syntax-parser define-iterators
  [(_ [T:expr run:id ([name:id do!:id] ...)])
   #'(begin (define (name [this : Sample-Tree]) : (values T T)
              (run this do!))
            ...)]
  [(_ clause:expr ...)
   #'(begin (define-iterators clause) ...)])


(define-iterators
  [(Array Positive-Integer)
   run-iterate-arrays
   ([sample-tree-iterate/arrays/ascending do-iterate-ascending!]
    [sample-tree-iterate/arrays/descending do-iterate-descending!])]
  [FlVector
   run-iterate-flvectors
   ([sample-tree-iterate/flvectors/ascending do-iterate-ascending!]
    [sample-tree-iterate/flvectors/descending do-iterate-descending!])])
