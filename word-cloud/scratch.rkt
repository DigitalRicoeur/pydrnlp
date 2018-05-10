#lang typed/racket

(require "rect.rkt"
         "pict.rkt"
         "world.rkt"
         "stream.rkt"
         "test-data.rkt"
         typed/pict
         adjutor
         )

(module+ main
  (scale-to-fit (make-cloud example-picts)
                600
                600))



(: make-cloud (-> (Listof pict) pict))
(define (make-cloud picts)
  (render-placed-picts
   (cons origin-dot
         (arrange-placed-pict-streams
          (make-placed-pict-stream picts)))))



(: render-placed-picts (-> (Listof Placed-Pict)
                           pict))
(define (render-placed-picts raw)
  (match-define (cons placed (rect _ _ w h))
    (normalize-placed-picts raw))
  (overlay-placed placed (blank w h)))


(: normalize-placed-picts (-> (Listof Placed-Pict)
                              (Pairof (Listof Placed-Pict)
                                      rect)))
(define (normalize-placed-picts placed)
  (define adjusted-origin
    (let-values ([{x-so-far y-so-far}
                  (for/fold ([x-so-far : Float 0.0]
                             [y-so-far : Float 0.0])
                            ([p (in-list placed)])
                    (match (placed-pict-bounds p)
                      [(rect x y _ _)
                       (values (min x x-so-far)
                               (min y y-so-far))]))])
      (let ([x-offset (- x-so-far)]
            [y-offset (- y-so-far)])
        (for/list : (Listof Placed-Pict) ([p (in-list placed)])
          (placed-pict-adjust-origin p x-offset y-offset)))))
  (define-values {w h}
    (for/fold ([w : Float 0.0]
               [h : Float 0.0])
              ([p (in-list adjusted-origin)])
      (define r (placed-pict-bounds p))
      (values (max w (rect-x1 r))
              (max h (rect-y1 r)))))
  (cons adjusted-origin
        (rect 0.0
              0.0
              (assert w nonnegative-float?)
              (assert h nonnegative-float?))))









(: make-placed-pict-stream (-> (Listof pict)
                               (Listof Placed-Pict-Stream)))
(define (make-placed-pict-stream picts)
  (let* ([placed (map pict->placed picts)]
         [get-initial-position
          (default-make-get-initial-position placed)]
         [placed 
          (for/list : (Listof Placed-Pict) ([p (in-list placed)])
            (match-define (cons x-offset y-offset)
              (get-initial-position p))
            (placed-pict-adjust-origin p x-offset y-offset))]
         [arch (make-archimedean-spiral 8.0)])
    (for/list ([p (in-list placed)])
      (placed-pict-stream
       p
       (λ ()
         (let loop : Placed-Pict-Stream ([i 1])
           (match-define (cons x y)
             (arch (real->double-flonum i)))
           (define adjusted
             (placed-pict-adjust-origin p x y))
           (placed-pict-stream
            adjusted
            (λ () 
              (if (= 0 (modulo i 4))
                  (placed-pict-stream (rotate-placed-pict adjusted)
                                      (λ () (loop (add1 i))))
                  (loop (add1 i)))))))))))





(: total-area (-> (Listof Placed-Pict) Nonnegative-Float))
(define (total-area lst)
  ;; for/sum expects integers for some reason
  (for/fold ([a : Nonnegative-Float 0.0])
            ([p (in-list lst)])
    (+ a (placed-pict-area p))))


(: make-archimedean-spiral (-> Float (-> Float (Pairof Float Float))))
(define ((make-archimedean-spiral r) t)
  (cons (* r t (cos t)) 
        (* r t (sin t))))


(: area->width (->* {Nonnegative-Float} {Positive-Float}
                    Nonnegative-Float))
(define (area->width a [aspect-ratio 1.0])
  (if (= 0 a)
      a
      (sqrt (/ a aspect-ratio))))

(: area+width->height (-> Nonnegative-Float Nonnegative-Float
                          Nonnegative-Float))
(define (area+width->height a w)
  (if (or (= 0.0 a)
          (= 0.0 w))
      0.0
      (/ a w)))


(: make-make-get-initial-position (->* {}
                                       {Positive-Float
                                        #:x-spread Nonnegative-Float
                                        #:y-spread Nonnegative-Float}
                                       (-> (Listof Placed-Pict)
                                           (-> Placed-Pict
                                               (Pairof Float Float)))))
(define ((make-make-get-initial-position [aspect-ratio 1.0]
                                         #:x-spread [x-spread 0.3]
                                         #:y-spread [y-spread 0.1])
         lst)
  ;; Initial positions are centered around origin and fall within
  ;; (* x-spread w) and (* y-spread h), half positive and half negative.
  (let* ([a (total-area lst)]
         [w (area->width a aspect-ratio)]
         [h (area+width->height a w)]
         [random-x (make-random-in-spread w x-spread)]
         [random-y (make-random-in-spread h y-spread)])
    (λ (_)
      (cons (random-x)
            (random-y)))))


(define default-make-get-initial-position
  (make-make-get-initial-position))

(: make-random-in-spread (-> Nonnegative-Float Nonnegative-Float
                             (-> Float)))
(define (make-random-in-spread dim spread)
  (cond
    [(= 0.0 spread)
     (λ () 0.0)]
    [(infix: 1.0 < spread)
     (error 'make-random-in-spread
            "out of range")]
    [else
     (let* ([absolute-spread (* dim spread)]
            [low (- (/ absolute-spread 2.0))])
       (λ ()
         (+ low (* (random) absolute-spread))))]))


(define origin-dot
  (let ([dot (pict->placed (colorize (disk 25) "cyan"))])
    (placed-pict-adjust-origin
     dot
     (- (/ (placed-pict-width dot) 2.0))
     (- (/ (placed-pict-height dot) 2.0)))))






















  
