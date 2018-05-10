#lang typed/racket

(require "rect.rkt"
         "pict.rkt"
         "world.rkt"
         typed/pict
         adjutor
         )


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






(define-type Placed-Pict-Stream (Pairof Placed-Pict
                                        (-> Placed-Pict-Stream)))


(: arrange-placed-pict-streams (-> (Listof Placed-Pict-Stream)
                                   (Listof Placed-Pict)))
(define (arrange-placed-pict-streams to-go)
  (let arrange/recur ([world empty-world]
                      [to-go to-go])
    (match to-go
      ['()
       (world-placed-picts world)]
      [(cons stm to-go)
       (arrange/recur
        (let loop ([stm stm])
          (match-define (cons this get-next)
            stm)
          (or (world-try-insert world this)
              (loop (get-next))))
        to-go)])))





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
      (cons
       p
       (λ ()
         (let loop : Placed-Pict-Stream ([i 1])
           (match-define (cons x y)
             (arch (real->double-flonum i)))
           (define adjusted
             (placed-pict-adjust-origin p x y))
           (cons adjusted
                 (λ () 
                   (if (= 0 (modulo i 4))
                       (cons (rotate-placed-pict adjusted)
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




(: terms (Listof (Pairof String Positive-Integer)))
(define terms
  '(("time" . 11600)
    ("history" . 10011)
    ("sense" . 9542)
    ("work" . 9193)
    ("way" . 8868)
    ("meaning" . 8489)
    ("language" . 8399)
    ("relations" . 8056)
    ("question" . 7434)
    ("philosophy" . 7195)
    ("action" . 6979)
    ("world" . 6894)
    ("terms" . 6767)
    ("narrative" . 6752)
    ("form" . 6717)
    ("texts" . 6423)
    ("point" . 6377)
    ("discourses" . 6294)
    ("word" . 6284)
    ("man" . 6259)
    ("died" . 6166)
    ("new" . 6136)
    ("problem" . 6115)
    ("theory" . 6046)
    ("human" . 6005)
    ("level" . 5977)
    ("order" . 5948)
    ("experience" . 5819)
    ("place" . 5785)
    ("idea" . 5752)
    ("interpretation" . 5723)
    ("spoke" . 5395)
    ("life" . 5368)
    ("thing" . 5300)
    ("acting" . 5289)
    ("concept" . 5245)
    ("means" . 5211)
    ("consciousness" . 5210)
    ("object" . 5000)
    ("analysis" . 4962)
    ("fact" . 4870)
    ("historical" . 4859)
    ("present" . 4718)
    ("self" . 4713)))


(scale-to-fit
 (let* ([counts (map (inst cdr String Positive-Integer)
                     terms)]
        [mx (apply max counts)]
        [mn (apply min counts)]
        [rel-max (- mx mn)])
   (define (count->font-size [c : Integer])
     (+ 10 (floor (* (- c mn)
                     (/ 90 rel-max)))))
   (make-cloud
    (map (λ ([pr : (Pairof String Positive-Integer)])
           (match pr
             [`(,s . ,c)
              (text s null (assert (count->font-size c)
                                   index?))]))
         terms)))
 600
 600)
















  
