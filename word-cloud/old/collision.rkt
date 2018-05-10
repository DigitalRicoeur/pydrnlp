#lang racket

(require pict
         adjutor
         )

(module+ test
  (require rackunit))

(struct rectangle (x0 y0 w h)
  #:transparent)

(struct pict+rects (p abstract-rect rects)
  ;; The abstract-rect defines the border of
  ;; the pict but is not considered "full".
  ;; The rects must all be contained within
  ;; the abstract-rect.
  #:transparent)
  
(struct union (children)
  #:transparent)

(define find-maxes
  (match-lambda
    [(pict+rects _ (rectangle x0 y0 w h) rects)
     (values (+ x0 w) (+ y0 h))]
    [(union (cons this children))
     (define-values {w h}
       (find-maxes this))
     (for/fold ([w-so-far w]
                [h-so-far h])
               ([this (in-list children)])
       (define-values {w h}
         (find-maxes this))
       (values (max w w-so-far)
               (max h h-so-far)))]))
       
(define find-mins
  (match-lambda
    [(pict+rects _ (rectangle x0 y0 _ _) rects)
     (values x0 y0)]
    [(union (cons this children))
     (define-values {x y}
       (find-mins this))
     (for/fold ([x-so-far x]
                [y-so-far y])
               ([this (in-list children)])
       (define-values {x y}
         (find-mins this))
       (values (min x x-so-far)
               (min y y-so-far)))]))

(define (wrap-pict p)
  ;; Eventually there could be multiple rects
  ;; if we do hierarchial bounding boxes.
  (let* ([w (pict-width p)]
         [h (pict-height p)]
         [r (rectangle 0 0 w h)])
    (pict+rects p r (list r))))
                

(define (try-place p+r target)
  (and (not (for/or ([r (in-list (pict+rects-rects p+r))])
              (rect-intersects? r target)))
       (match target
         [(union children)
          (union (cons p+r children))]
         [_
          (union (list p+r target))])))

(define (rect-intersects? r other)
  ;; https://developer.mozilla.org/en-US/docs/
  ;;   Games/Techniques/2D_collision_detection
  (match-define (rectangle 1x0 1y0 1w 1h)
    r)
  (let intersects? ([other other])
    (match other
      [(rectangle 2x0 2y0 2w 2h)
       (and (infix: 1x0 < (+ 2x0 2w))
            (infix: (+ 1x0 1w) > 2x0)
            (infix: 1y0 < (+ 2y0 2h))
            (infix: (+ 1y0 1h) > 2y0))]
      [(or (pict+rects _ _ more)
           (union more))
       (for/or ([other (in-list more)])
         (intersects? other))])))       

(define (adjust-origin it x-offset y-offset)
  (let adjust-origin ([it it])
    (match it
      [(rectangle x0 y0 w h)
       (rectangle (+ x0 x-offset)
                  (+ y0 y-offset)
                  w
                  h)]
      [(pict+rects p abstract-rect rects)
       (pict+rects p
                   (adjust-origin abstract-rect)
                   (map adjust-origin
                        rects))]
      [(union children)
       (union (map adjust-origin children))])))

(define rotate-rect
  (match-lambda
    [(rectangle x0 y0 w h)
     (def
       [w/2 (/ w 2)]
       [h/2 (/ h 2)]
       [horizontal-mid (+ x0 w/2)]
       [vertical-mid (+ y0 h/2)]
       [new-x (- horizontal-mid h/2)]
       [new-y (- vertical-mid w/2)])
     (rectangle new-x
                new-y
                h
                w)]))

(module+ test
  (define rect-center
    (match-lambda
      [(rectangle x0 y0 w h)
       (cons (+ x0 (/ w 2))
             (+ y0 (/ h 2)))]))
  (let ([r (rectangle 2 3 9 5)])
    (check-equal? (rect-center (rotate-rect r))
                  (rect-center r))))

(define rotate*
  ;; keep the center the same
  (match-lambda
    [(pict+rects p abstract-rect rects)
     ;; old top left is now top right
     (pict+rects (rotate p (- (/ pi 2)))
                 (rotate-rect abstract-rect)
                 (map rotate-rect rects))]))

(define OLD-rotate*
  (match-lambda
    [(pict+rects p abstract-rect rects)
     (define rotate-rect
       (match-lambda
         [(rectangle x0 y0 w h)
          (rectangle (- x0 h)
                     y0
                     h
                     w)]))
     ;; old top left is now top right
     (pict+rects (rotate p (- (/ pi 2)))
                 (rotate-rect abstract-rect)
                 (map rotate-rect rects))]))

(define samples
  (map (match-lambda
         [`(,s ,n)
          (text s null n)])
       '(("birthday" 10)
         ("peaches" 30)
         ("plums" 10)
         ("apples" 60)
         ("pears" 30)
         ("open" 27)
         ("shut" 15)
         ("sleeping" 19)
         ("chocolate" 22)
         ("tell" 10))))

(define (pict-area p)
  (* (pict-width p)
     (pict-height p)))

(define (total-area picts)
  (for/sum ([p (in-list picts)])
    (pict-area p)))

(define (area->width a [ratio 1])
  (sqrt (/ a ratio)))

(define ((make-archimedean-spiral r) t)
  (cons (* r t (cos t)) 
        (* r t (sin t))))

(define show-origin?
  (make-parameter #t))

(define (place-all picts)
  (define a
    (total-area picts))
  (define w
    (area->width a))
  (define h
    (/ a w))
  (define arch
    (make-archimedean-spiral 8))
  (define (guard-random x)
    (min 4294967087
         (inexact->exact (floor x))))
  (define +rects
    (let* ([k (guard-random (* 0.3 w))]
           [vertical-k (guard-random (* 0.1 h))])
      (for/list ([p (in-list picts)])
        (adjust-origin (wrap-pict p)
                       (random k)
                       (if (= 0 vertical-k)
                           0
                           (random vertical-k))))))
  (let loop ([so-far (union null)]
             [to-go +rects])
    (match to-go
      ['()
       (if (show-origin?)
           (union (list (wrap-pict (colorize (disk 25) "red"))
                        so-far))
           so-far)]
      [(cons this to-go)
       (loop (let place ([i 0])
               (define flip?
                 (and (not (= 0 i))
                      (= 0 (modulo i 4))))
               (match-define (cons x y)
                 (arch i))
               (define adjusted
                 (adjust-origin this x y))
               (or (try-place adjusted
                              so-far)
                   (and flip?
                        (try-place (rotate* adjusted)
                                   so-far))
                   (place (add1 i))))
             to-go)])))

(define (make-cloud picts)
  (let ([placed (place-all picts)])
    (define-values {x y}
      (find-mins placed))
    (let ([placed (adjust-origin placed (- x) (- y))])
      (define-values {w h}
        (find-maxes placed))
      (let loop ([to-go placed]
                 [target (blank w h)])
        (match to-go
          ['() target]
          [(pict+rects p (rectangle x y _ _) _)
           (pin-over target x y p)]
          [(union children)
           (loop children target)]
          [(cons this to-go)
           (loop to-go
                 (loop this target))])))))


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





;(make-cloud samples)

(scale-to-fit
 (let* ([counts (map cdr terms)]
        [mx (apply max counts)]
        [mn (apply min counts)]
        [rel-max (- mx mn)])
   (define (count->font-size c)
     (+ 10 (floor (* (- c mn)
                     (/ 90 rel-max)))))
   (make-cloud
    (map (match-lambda
           [`(,s . ,c)
            (text s null (count->font-size c))])
         terms)))
 600
 600)

