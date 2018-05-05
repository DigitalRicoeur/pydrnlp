#lang typed/racket

(require adjutor
         )

(provide (struct-out region)
         region-width
         region-height
         region-center
         rotate-region
         ;;;;
         Quadtree
         quadtree?
         empty-quadtree
         quadtree-insert
         quadtree-collision?
         )

(module+ test
  (require typed/rackunit))

(struct region ([x0 : Float]
                [x1 : Float]
                [y0 : Float]
                [y1 : Float])
  #:transparent)

(: region-width (-> region Float))
(define region-width
  (match-lambda
    [(region x0 x1 _ _)
     (- x1 x0)]))
    
(: region-height (-> region Float))
(define region-height
  (match-lambda
    [(region _ _ y0 y1)
     (- y1 y0)]))

(: region-center (-> region (Pairof Float Float)))
(define region-center
  (match-lambda
    [(and (region x0 _ y0 _) r)
     (cons (+ x0 (/ (region-width r) 2))
           (+ y0 (/ (region-height r) 2)))]))

(: rotate-region (-> region region))
(define rotate-region
  (match-lambda
    [(and (region x0 x1 y0 y1) r)
     (def
       [w (region-width r)]
       [h (region-height r)]
       [w/2 (/ w 2)]
       [h/2 (/ h 2)]
       [horizontal-mid (+ x0 w/2)]
       [vertical-mid (+ y0 h/2)]
       [new-x0 (- horizontal-mid h/2)]
       [new-y0 (- vertical-mid w/2)])
     (region new-x0
             (+ new-x0 h)
             new-y0
             (+ new-y0 w))]))

(module+ test
  (let ([r (region 2.0 3.0 9.0 5.0)])
    (check-equal? (region-center (rotate-region r))
                  (region-center r))))


(struct quadtree-leaf ([state : (U 'empty 'full)])
  #:transparent)

(struct quadtree-node ([midpoint-x : Float]
                       [midpoint-y : Float]
                       [nw : Quadtree]
                       [ne : Quadtree]
                       [sw : Quadtree]
                       [se : Quadtree])
  #:transparent)

(define-type Quadtree (U quadtree-leaf
                         quadtree-node))

(define-predicate quadtree? Quadtree)

(define empty-quadtree
  (quadtree-leaf 'empty))

(: midpoint-status (-> Float Float Float
                       (Values Boolean Boolean)))
(define (midpoint-status ?0 ?1 midpoint-?)
  (cond
    [(infix: ?1 < midpoint-?)
     (values #t #f)]
    [(infix: ?0 > midpoint-?)
     (values #f #t)]
    [else
     (values #t #t)]))

(: quadtree-collision? (-> Quadtree region
                           Boolean))
(define (quadtree-collision? qt r)
  (match qt
    [(quadtree-leaf 'full)
     #t]
    [(quadtree-leaf 'empty)
     #f]
    [(quadtree-node midpoint-x midpoint-y nw ne sw se)
     (match-define (region x0 x1 y0 y1)
       r)
     (define-values {x:before? x:after?}
       (midpoint-status x0 x1 midpoint-x))
     (define-values {y:before? y:after?}
       (midpoint-status y0 y1 midpoint-y))
     (or (and x:before?
              y:before?
              (quadtree-collision? nw r))
         (and x:after?
              y:before?
              (quadtree-collision? ne r))
         (and x:before?
              y:after?
              (quadtree-collision? sw r))
         (and x:after?
              y:after?
              (quadtree-collision? se r)))]))

(: quadtree-insert (-> Quadtree region Quadtree))
(define (quadtree-insert qt r)
  (do-quadtree-insert qt r))


(: do-quadtree-insert (->* {Quadtree region}
                           {Float Float Float Float}
                           Quadtree))
(define (do-quadtree-insert qt
                            r
                            [xmin -inf.0]
                            [xmax +inf.0]
                            [ymin -inf.0]
                            [ymax +inf.0])
  (match-define (region x0 x1 y0 y1)
    r)
  (match qt
    [(quadtree-leaf 'full)
     qt]
    [_
     #:when (and (infix: x0 <= xmin)
                 (infix: xmax <= x1)
                 (infix: y0 <= ymin)
                 (infix: ymax <= y1))
     (quadtree-leaf 'full)]
    [(quadtree-leaf 'empty)
     (: midpoint (-> Float Float Float Float
                     Float))
     (define (midpoint ?min ?max ?0 ?1)
       (cond
         [(= -inf.0 ?min)
          ?0]
         [(= +inf.0 ?max)
          ?1]
         [(= ?0 ?min)
          ?1]
         [(= ?1 ?max)
          ?0]
         [else
          (/ (+ ?min ?max) 2)]))
     (define midpoint-x
       (midpoint xmin xmax x0 x1))
     (define midpoint-y
       (midpoint ymin ymax y0 y1))
     (do-quadtree-insert (quadtree-node midpoint-x
                                        midpoint-y
                                        qt qt qt qt)
                         r
                         xmin xmax ymin ymax)]
    [(quadtree-node midpoint-x midpoint-y nw ne sw se)
     (define-values {x:before? x:after?}
       (midpoint-status x0 x1 midpoint-x))
     (define-values {y:before? y:after?}
       (midpoint-status y0 y1 midpoint-y))
     (quadtree-node
      midpoint-x
      midpoint-y
      (if (and x:before?
               y:before?)
          (do-quadtree-insert nw
                              (region x0 midpoint-x
                                      y0 midpoint-y)
                              xmin midpoint-x
                              ymin midpoint-y)
          nw)
      (if (and x:after?
               y:before?)
          (do-quadtree-insert ne
                              (region midpoint-x x1
                                      y0 midpoint-y)
                              midpoint-x xmax
                              ymin midpoint-y)
          ne)
      (if (and x:before?
               y:after?)
          (do-quadtree-insert sw
                              (region x0 midpoint-x
                                      midpoint-y y1)
                              xmin midpoint-x
                              midpoint-y ymax)
          sw)
      (if (and x:after?
               y:after?)
          (do-quadtree-insert se
                              (region midpoint-x x1
                                      midpoint-y y1)
                              midpoint-x xmax
                              midpoint-y ymax)
          se))]))
  






