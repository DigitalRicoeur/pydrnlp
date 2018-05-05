#lang racket

(require racket/draw
         pict
         )

(define text-path%
  (class dc-path%
    (super-new)
    (init text font)
    (inherit text-outline)
    (text-outline font text 0 0 #t)))

(define p
  (new text-path%
       [text "crab-apple"]
       [font (send the-font-list
                   find-or-create-font
                   80
                   'roman
                   'normal
                   'normal)]))

(define (path-pict p)
  (define-values {x0 y0 w h}
    (send p get-bounding-box))
  (dc (λ (it x y)
         (send it draw-path p (- x x0) (- y y0)))
       w
       h))

(define drawn
  (colorize (path-pict p) "green"))

(cc-superimpose drawn
                (blank (+ 10 (pict-width drawn))
                       (+ 10 (pict-height drawn))))




