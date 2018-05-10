#lang typed/racket

;; The Placed-Pict-Stream datatype is a fairly low-level
;; means to abstract over the part of the layout algorithm
;; that involves giving each word an initial "desired" position
;; and updating its target position in the event of a collision.
;; It separates these considerations from the collision
;; detection itself, which is encapsulated in the World datatype.
;; Note that a Placed-Pict-Stream is **not** a stream in the
;; sense of racket/stream because TR doesn't have generics or
;; struct type properties.

(provide Placed-Pict-Stream
         (struct-out placed-pict-stream)
         arrange-placed-pict-streams
         )

(require "pict.rkt"
         "world.rkt"
         )

(struct placed-pict-stream ([first : Placed-Pict]
                            [rest : (-> Placed-Pict-Stream)])
  #:transparent)

(define-type Placed-Pict-Stream placed-pict-stream)


(: arrange-placed-pict-streams (->* {(Listof Placed-Pict-Stream)}
                                    {World}
                                    (Listof Placed-Pict)))
(define (arrange-placed-pict-streams to-go [empty-world empty-world])
  (let arrange/recur ([world empty-world]
                      [to-go to-go])
    (match to-go
      ['()
       (world-placed-picts world)]
      [(cons stm to-go)
       (arrange/recur
        (let loop ([stm stm])
          (match-define (placed-pict-stream this get-next)
            stm)
          (or (world-try-insert world this)
              (loop (get-next))))
        to-go)])))


