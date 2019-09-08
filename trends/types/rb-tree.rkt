#lang typed/racket

;; ??? is symbol<? actually especially efficient?

;; after Okasaki 3.3 p24

(define-type (RB-Tree A)
  (U #f (node A)))

(struct (A) node
  ([color : (U 'red 'black)]
   [left : (RB-Tree A)]
   [k : Symbol]
   [v : A]
   [right : (RB-Tree A)])
  #:transparent)

(: rb-tree-color (∀ (A) (-> (RB-Tree A) (U 'red 'black))))
(define (rb-tree-color this)
  (if this (node-color this) 'black))

(: insert (∀ (A B) (-> (RB-Tree A) Symbol (-> (U A B) A) B (RB-Tree A))))
(define #:∀ (A B) (insert this sym update-value default)
  (: insert-help (-> (RB-Tree A) (node A)))
  (define insert-help
    (match-lambda
      [#f
       (node 'red #f sym (update-value default) #f)]
      [(node c left k v right)
       #:when (symbol<? sym k)
       (balance (node c (insert-help left) k v right))]
      [(node c left k v right)
       #:when (eq? sym k)
       (node c left k (update-value v) right)]
      [(node c left k v right)
       ;;#:when (symbol>? sym k) ;; doesn't exist
       (balance (node c left k v (insert-help right)))]))
  (struct-copy node (insert-help this)
               [color 'black]))

(: balance (∀ (A) (-> (node A) (node A))))
(define balance
  (match-lambda
    ;; 1. No red node has a red child.
    ;; 2. Every path from the root to an empty node
    ;;    contains the same number of black nodes.
    [(or (node 'black (node 'red (node 'red a xk xv b) yk yv c) zk zv d)
         (node 'black (node 'red a xk xv (node 'red b yk yv c)) zk zv d)
         (node 'black a xk xv (node 'red (node 'red b yk yv c) zk zv d))
         (node 'black a xk xv (node 'red b yk yv (node 'red c zk zv d))))
     ;; type inference not working well here:
     (assert xk) (assert xv) (assert yk) (assert yv) (assert zk) (assert zv)
     (node 'red (node 'black a xk xv b) yk yv (node 'black c zk zv d))]
    [ok
     ok]))

;; TODO: (Okasaki p14 ex2.2)
;; Current worst-case is (* 2 depth) comparisons;
;; can do worst-case (add1 depth) instead by remembering
;; possibly-equal element and checking = only on hitting bottom.
;; SEE ALSO: ex3.10
;; reduce superfluous tests in ballance
