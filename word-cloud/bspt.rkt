#lang racket

(require pict
         racket/draw
         "rect.rkt"
         adjutor
         "test-data.rkt"
         )

(module+ main
  (map show example-picts)

  (define word
    (term-pict "interpretation" 48))

  (define simple
    (let ([row (hc-append (blank 1 1)
                          (colorize (filled-rectangle 1 1) "white"))])
      (vc-append row row row))))


(define (get-opaque p
                    [cost-per-leaf 50] ;50
                    [min-area 250]) ;250
  ;; Maybe cost-per-leaf and min-area should
  ;; be functions of the size of p.
  ;; cost-per-leaf : 50 ; min-area : 250
  (let* ([w (inexact->exact
             (ceiling (pict-width p)))]
         [h (inexact->exact
             (ceiling (pict-height p)))]
         [row-length (* 4 w)]
         [bs (pict->argb-pixels p)])
    (define (get-alpha x y)
      (bytes-ref bs (+ (* x 4) (* y row-length))))
    (bspt->rects
     (classify
      (make-pointdata
       (for*/list ([x (in-range w)]
                   [y (in-range h)]
                   [alpha (in-value (get-alpha x y))]
                   #:unless (= 0 alpha))
         (cons x y)))
      0
      0
      w
      h
      cost-per-leaf
      min-area))))


;; A binary space partitioning tree is either:
(struct bspt (natbounds cost)
  ;; Abstract!
  #:transparent)
(struct tree bspt (split a b)
  ;; split : (U 'ud 'lr)
  #:transparent)
(struct leaf bspt (full?)
  #:transparent)

(struct natbounds (x y w h)
  ;; where x and y are nonnegative int
  ;; and w and h are positive int
  #:transparent)

(define (partition-at dim cutoff lst)
  (define get
    (case dim
      [(x) car]
      [(y) cdr]))
  (partition (λ (pr) 
               (infix: (get pr) < cutoff))
             lst))



#|
(struct pointdata (x-sorted y-sorted)
  #:transparent)

(define (pointdata-length pd)
  (length (pointdata-x-sorted pd)))

(define (sort-points get lst)
  (sort lst < #:key get))

(define (make-pointdata lst)
  (pointdata (sort-points car lst)
             (sort-points cdr lst)))

(define (partition-pointdata dim cutoff pd)
  (define-values {lst pred wrap-split}
    (case dim
      [(x)
       (values (pointdata-x-sorted pd)
               (λ (pr)
                 (infix: (car pr) < cutoff))
               (λ (lst)
                 (pointdata lst (sort-points cdr lst))))]
      [(y)
       (values (pointdata-y-sorted pd)
               (λ (pr)
                 (infix: (cdr pr) < cutoff))
               (λ (lst)
                 (pointdata (sort-points car lst) lst)))]))
  (define-values {before after}
    (splitf-at lst pred))
  (values (wrap-split before)
          (wrap-split after)))
|#
(define make-pointdata values)
(define pointdata-length length)
(define partition-pointdata partition-at)


(define (classify data x y w h cost-per-leaf min-area)
  (let/ec return
    (define total-points
      (* w h))
    (define opaque-points
      (pointdata-length data))
    (define these-bounds
      (natbounds x y w h))
    (define-values {as-leaf/lr leaf/lr-cost}
      (let ()
        (define-values {as-leaf as-leaf-cost}
          (let ()
            (if (= 0 opaque-points)
                (return (leaf these-bounds cost-per-leaf #f))
                (let* ([transparent-points
                        (- total-points opaque-points)]
                       [as-leaf-cost
                        (+ cost-per-leaf transparent-points)]
                       [as-leaf
                        (leaf these-bounds as-leaf-cost #t)])
                  (if (or (infix: total-points < min-area)
                          (= total-points opaque-points))
                      (return as-leaf)
                      (values as-leaf as-leaf-cost))))))
        ;; 'lr split
        (define left-width
          (quotient w 2))
        (define x-midpoint
          (+ x left-width))
        (define-values {left-data right-data}
          (partition-pointdata 'x x-midpoint data))
        (define left-child
          (classify left-data
                    x y left-width h
                    cost-per-leaf min-area))
        (define right-child
          (classify right-data
                    x-midpoint y (- w left-width) h
                    cost-per-leaf min-area))
        (define lr-cost
          (+ (bspt-cost left-child)
             (bspt-cost right-child)))
        ;;;;;;;;;
        (if (infix: as-leaf-cost <= lr-cost)
            (values as-leaf as-leaf-cost)
            (values (tree these-bounds
                          lr-cost
                          'lr
                          left-child
                          right-child)
                    lr-cost))))
    ;; 'ud split
    (define up-height
      (quotient h 2))
    (define y-midpoint
      (+ y up-height))
    (define-values {up-data down-data}
      (partition-pointdata 'y y-midpoint data))
    (define up-child
      (classify up-data
                x y w up-height
                cost-per-leaf min-area))
    (define down-child
      (classify down-data
                x y-midpoint w (- h up-height)
                cost-per-leaf min-area))
    (define ud-cost
      (+ (bspt-cost up-child)
         (bspt-cost down-child)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (if (infix: leaf/lr-cost <= ud-cost)
        as-leaf/lr
        (tree these-bounds
              ud-cost
              'ud
              up-child
              down-child))))


(define bspt->rects
  ;; TODO: handle fractional w / h
  (match-lambda
    [(leaf _ _ #f)
     null]
    [(leaf (natbounds x y w h) _ #t)
     (list (rect (real->double-flonum x)
                 (real->double-flonum y)
                 (real->double-flonum w)
                 (real->double-flonum h)))]
    [(tree _ _ _ a b)
     (append (bspt->rects a)
             (bspt->rects b))]))

(define (show p)
  (define rects
    (get-opaque p))
  (cc-superimpose
   (for/fold ([bkgd (filled-rectangle (pict-width p)
                                      (pict-height p)
                                      #:draw-border? #f
                                      #:color "gray")])
             ([r (in-list rects)])
     (match-define (rect x y w h)
       r)
     (pin-over bkgd x y (filled-rectangle w h
                                          #:draw-border? #t
                                          #:border-width 1
                                          #:border-color "blue"
                                          #:color "cyan")))
   p))



