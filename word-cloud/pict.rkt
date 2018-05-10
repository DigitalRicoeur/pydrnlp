#lang typed/racket

;; The Placed-Pict abstract datatype encapsulates:
;;  - a pict;
;;  - its placement on a Cartesian plane;
;;  - its overall size; and
;;  - the rectangle(s) within it that are considered "full".
;; The constructor is private to preserve invariants.
;; Right now the whole pict is considered "full", but
;; this could support finer-grained hierarchial bounding boxes
;; in the future.

(provide placed-pict?
         placed-pict-pict
         placed-pict-bounds
         placed-pict-filled
         (rename-out [placed-pict* placed-pict])
         Placed-Pict
         placed-pict-area
         placed-pict-width
         placed-pict-height
         pict->placed
         placed-pict-adjust-origin
         rotate-placed-pict
         overlay-placed
         )

(require typed/pict
         "rect.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(struct placed-pict ([pict : pict]
                     [bounds : rect]
                     [filled : (Listof rect)])
  ;; The bounds rect defines the border of the
  ;; pict and its placement w/r/t the origin,
  ;; but it is not considered "full".
  ;; The filled rects must all be inside 
  ;; the bounds rect.
  #:transparent)

(define-type Placed-Pict placed-pict)

(define-match-expander placed-pict*
  (syntax-parser
    [(_ p bounds filled)
     #'(placed-pict p bounds filled)]))


(: placed-pict-area (-> placed-pict Nonnegative-Float))
(define (placed-pict-area placed)
  (rect-area (placed-pict-bounds placed)))

(: placed-pict-width (-> placed-pict Nonnegative-Float))
(define (placed-pict-width placed)
  (rect-w (placed-pict-bounds placed)))

(: placed-pict-height (-> placed-pict Nonnegative-Float))
(define (placed-pict-height placed)
  (rect-h (placed-pict-bounds placed)))


(: pict->placed (-> pict placed-pict))
(define (pict->placed p)
  (define w
    (assert (real->double-flonum (pict-width p))
            nonnegative-float?))
  (define h
    (assert (real->double-flonum (pict-height p))
            nonnegative-float?))
  (define r
    (rect 0.0 0.0 w h))
  (placed-pict p r (list r)))


(: placed-pict-adjust-origin (-> placed-pict Float Float placed-pict))
(define (placed-pict-adjust-origin placed x-offset y-offset)
  (match placed
    [(placed-pict p bounds filled)
     (placed-pict p
                  (rect-adjust-origin bounds x-offset y-offset)
                  (for/list : (Listof rect) ([r (in-list filled)])
                    (rect-adjust-origin r x-offset y-offset)))]))


(: rotate-placed-pict (-> placed-pict placed-pict))
(define rotate-placed-pict
  (match-lambda
    ;; rotate 90º clockwise, keeping center the same
    [(placed-pict p bounds filled)
     (placed-pict (rotate p (- (/ pi 2)))
                  (rotate-rect bounds)
                  (map rotate-rect filled))]))


(: overlay-placed (-> (Listof placed-pict) pict pict))
(define (overlay-placed lst target)
  (match lst
    ['() target]
    [(cons (placed-pict p (rect x y _ _) _)
           lst)
     (overlay-placed lst
                     (pin-over target x y p))]))

     
