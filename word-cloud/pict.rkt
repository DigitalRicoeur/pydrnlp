#lang typed/racket

(require adjutor
         typed/pict
         "quadtree.rkt"
         )

(provide make-cloud
         (struct-out placed-pict)
         (struct-out quasistream)
         adjust-origin
         rotate*
         )

(struct placed-pict ([p : pict]
                     [placement : region]
                     [regions : (Listof region)])
  ;; The placement defines the border of
  ;; the pict but is not considered "full".
  ;; The regions must all be contained within
  ;; the placement.
  #:transparent)

(struct quasistream ([first : placed-pict]
                     [rest : (Promise quasistream)])
  #:transparent)


(: adjust-region-origin (-> region Float Float region))
(define (adjust-region-origin r x-offset y-offset)
  (match r
    [(region x0 x1 y0 y1)
     (region (+ x0 x-offset)
             (+ x1 x-offset)
             (+ y0 y-offset)
             (+ y1 y-offset))]))

(: adjust-origin (-> placed-pict Float Float
                     placed-pict))
(define (adjust-origin it x-offset y-offset)
  (match it
    [(placed-pict p placement regions)
     (placed-pict p
                  (adjust-region-origin placement
                                        x-offset y-offset)
                  (map (λ ([r : region])
                         (adjust-region-origin r
                                               x-offset y-offset))
                       regions))]))

(: rotate* (-> placed-pict placed-pict))
(define rotate*
  ;; keep the center the same
  (match-lambda
    [(placed-pict p placement regions)
     (placed-pict (rotate p (- (/ pi 2)))
                  (rotate-region placement)
                  (map rotate-region regions))]))


(struct pre-cloud ([picts : (Listof placed-pict)]
                   [quadtree : Quadtree])
  #:transparent)

(define empty-pre-cloud
  (pre-cloud null empty-quadtree))

(: try-place (-> placed-pict pre-cloud (U False pre-cloud)))
(define (try-place p c)
  (match-define (pre-cloud picts qt)
    c)
  (define regions
    (placed-pict-regions p))
  (and (not (for/or : Boolean ([r (in-list regions)])
              (quadtree-collision? qt r)))
       (pre-cloud (cons p picts)
                  (for/fold ([qt qt])
                            ([r (in-list regions)])
                    (quadtree-insert qt r)))))


(: render-placed (-> (Listof placed-pict) pict))
(define (render-placed l-placed)
  (define-values {picts placements}
    (for/lists ([picts : (Listof pict)]
                [placements : (Listof region)])
               ([placed (in-list l-placed)])
      (match placed
        [(placed-pict p placement _)
         (values p placement)])))
  (let* ([x-offset (- (apply min (map region-x0 placements)))]
         [y-offset (- (apply min (map region-y0 placements)))]
         [placements (map (λ ([r : region])
                            (adjust-region-origin r
                                                  x-offset
                                                  y-offset))
                          placements)]
         [w (apply max (map region-x1 placements))]
         [h (apply max (map region-y1 placements))])
    (for/fold ([target (blank w h)])
              ([p (in-list picts)]
               [r (in-list placements)])
      (match r
        [(region x0 _ y0 _)
         (pin-over target x0 y0 p)]))))
      

(: pict->region (-> pict region))  
(define (pict->region p)
  (region 0.0 (real->double-flonum (pict-width p))
          0.0 (real->double-flonum (pict-height p))))


(: make-cloud (->* {(Listof pict)
                    (-> placed-pict quasistream)}
                   {#:get-initial-position
                    (-> placed-pict (values Float Float))}
                   pict))
(define (make-cloud picts
                    placed->quasistream
                    #:get-initial-position
                    [get-initial-position
                     (λ (x) (values 0.0 0.0))])
  (define l-qs
    (for/list : (Listof quasistream) ([p (in-list picts)])
      (define r
        (pict->region p))
      (define placed
        (placed-pict p r (list r)))
      (define-values {x-offset y-offset}
        (get-initial-position placed))
      (placed->quasistream
       (adjust-origin placed x-offset y-offset))))
  (render-placed
   (pre-cloud-picts
    (for/fold : pre-cloud
      ([cloud empty-pre-cloud])
      ([qs (in-list l-qs)])
      (let loop : pre-cloud ([qs qs])
        (match-define (quasistream placed rest)
          qs)
        (or (try-place placed cloud)
            (loop (force rest))))))))
