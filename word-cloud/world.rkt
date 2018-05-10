#lang typed/racket

;; The World abstract datatype manages non-intersecting
;; Placed-Picts on a Cartesian plane.
;; Its purpose is to abstract over the details of
;; the collision-detection strategy.
;; It does not deal with constraining the Placed-Picts
;; to be inside any specific boundaries on the Cartesian plane.

(provide World
         world?
         world-placed-picts
         empty-world
         world-try-insert
         )

(require "rect.rkt"
         "pict.rkt"
         )

(struct world ([placed-picts : (Listof Placed-Pict)]
               [filled : (Listof rect)]))

(define-type World world)

(define empty-world
  (world null null))


(: world-try-insert (-> World Placed-Pict
                        (U False World)))
(define (world-try-insert w placed)
  (match-define (world placed-so-far filled-so-far)
    w)
  (define filled
    (placed-pict-filled placed))
  (and (not (any-intersects? filled filled-so-far))
       (world (cons placed placed-so-far)
              (append filled
                      filled-so-far))))


(: any-intersects? (-> (Listof rect) (Listof rect) Boolean))
(define (any-intersects? l1 l2)
  ;; for*/or doesn't typecheck for some reason
  (for/or : Boolean ([r1 (in-list l1)])
    (for/or : Boolean ([r2 (in-list l2)])
      (rect-intersects? r1 r2))))
