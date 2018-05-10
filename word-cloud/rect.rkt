#lang typed/racket

;; The rect datatype represents rectangles
;; on a Cartesian plane.

(provide (struct-out rect)
         rect-x1
         rect-y1
         rect-area
         rect-center
         rect-intersects?
         rect-adjust-origin
         rotate-rect
         nonnegative-float?
         )

(require adjutor
         )

(module+ test
  (require typed/rackunit))

(struct rect ([x0 : Float]
              [y0 : Float]
              [w : Nonnegative-Float]
              [h : Nonnegative-Float])
  #:transparent)


(: rect-x1 (-> rect Float))
(define rect-x1
  (match-lambda
    [(rect x0 _ w _)
     (+ x0 w)]))


(: rect-y1 (-> rect Float))
(define rect-y1
  (match-lambda
    [(rect _ y0 _ h)
     (+ y0 h)]))


(: rect-area (-> rect Nonnegative-Float))
(define rect-area
  (match-lambda
    [(rect _ _ w h)
     (* w h)]))


(: rect-center (-> rect (Pairof Float Float)))
(define rect-center
  (match-lambda
    [(rect x0 y0 w h)
     (cons (+ x0 (/ w 2))
           (+ y0 (/ h 2)))]))


(: rect-intersects? (-> rect rect Boolean))
(define/match (rect-intersects? this other)
  ;; https://developer.mozilla.org/en-US/docs/
  ;;   Games/Techniques/2D_collision_detection
  [{(rect 1x0 1y0 1w 1h)
    (rect 2x0 2y0 2w 2h)}
   (and (infix: 1x0 < (+ 2x0 2w))
        (infix: (+ 1x0 1w) > 2x0)
        (infix: 1y0 < (+ 2y0 2h))
        (infix: (+ 1y0 1h) > 2y0))])


(: rect-adjust-origin (-> rect Float Float rect))
(define (rect-adjust-origin r x-offset y-offset)
  (match r
    [(rect x0 y0 w h)
     (rect (+ x0 x-offset)
           (+ y0 y-offset)
           w
           h)]))


(: rotate-rect (-> rect rect))
(define rotate-rect
  (match-lambda
    ;; rotate 90º clockwise, keeping center the same
    [(rect x0 y0 w h)
     (def
       [w/2 (/ w 2)]
       [h/2 (/ h 2)]
       [horizontal-mid (+ x0 w/2)]
       [vertical-mid (+ y0 h/2)]
       [new-x (- horizontal-mid h/2)]
       [new-y (- vertical-mid w/2)])
     (rect new-x
           new-y
           h
           w)]))

(module+ test
  (let ([r (rect 2.0 3.0 9.0 5.0)])
    (check-equal? (rect-center (rotate-rect r))
                  (rect-center r))))


(define-predicate nonnegative-float?* Nonnegative-Float)
(: nonnegative-float? (-> Float Boolean : Nonnegative-Float))
(define nonnegative-float?
  nonnegative-float?*)

;; rect-contains?
;; would have to decide if rect contains itself
