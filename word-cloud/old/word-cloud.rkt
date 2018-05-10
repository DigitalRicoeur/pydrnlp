#lang racket

(require pict
         math/statistics
         adjutor
         pict/convert
         )

(define words
  '(["birthday" . 1]
    ["peaches" . 3]
    ["plums" . 1]
    ["apples" . 6]
    ["pears" . 3]
    ["tell" . 1]))

(define (max-count ls)
  (for/fold ([so-far 1])
            ([pr (in-list ls)])
    (max so-far (cdr pr))))

(define (pict-area p)
  (* (pict-width p)
     (pict-height p)))

(define (turn-clockwise p)
  (rotate p (* pi 3/2)))

(define (word-pict word #:size [size 12]) ;size: (integer-in 1 1024)
  (text word null size))

(define (word-pict/area word target-area)
  (let* ([p (word-pict word)]
         [old-h (pict-height p)]
         [old-w (pict-width p)]
         [h/w (/ old-h old-w)]
         [new-w (sqrt (/ target-area h/w))]
         [new-h (* h/w new-w)])
    (scale/improve-new-text (word-pict word)
                            (/ new-w old-w)
                            (/ new-h old-h))))

(define (render-picts/area words)
  (let ([denom (max-count words)])
    (map (match-lambda
           [(cons w count)
            (define target-area
              (* 10000 (/ count denom)))
            (cons (word-pict/area w target-area) count)])
         words)))

(define/contract (weights->weight->font-size weights)
  (-> (listof (>=/c 1))
      (-> (>=/c 1) (integer-in 1 1024)))
  (define stats
    (update-statistics* empty-statistics (map cdr words)))
  (define min-weight
    (statistics-min stats))
  (define ratio
    (/ (statistics-max stats)
       min-weight))
  (compose1
   inexact->exact
   floor
   (cond
     [(infix: (* 10 ratio) <= 1024)
      (λ (weight)
        (* 10 (/ weight min-weight)))]
     [else
      (define weight-range
        (statistics-range stats))
      (define font-range
        (- 1024 10))
      (define font-points-per-weight
        (/ font-range weight-range))
      (λ (weight)
        (+ 10 (* font-points-per-weight
                 (- weight min-weight))))])))


(define (render-picts/font-size words #:count->weight [count->weight values])
  (define weight->font-size
    (weights->weight->font-size
     (map (compose1 count->weight cdr) words)))
  (map (match-lambda
         [(cons w count)
          (define weight
            (count->weight count))
          (define font-size
            (weight->font-size weight))
          (cons (word-pict w #:size font-size) count)])
       words))

(define current-render-mode
  (make-parameter 'area))

(define (render-picts words [mode (current-render-mode)])
  (case mode
    [(area)
     (render-picts/area words)]
    [(font-size)
     (render-picts/font-size words)]
    [(font-size/sqrt)
     (render-picts/font-size words #:count->weight sqrt)]))   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct field (pict rects)
  #:transparent
  #:property prop:pict-convertible
  (λ (this) (field-pict this)))

(struct rect (x0 y0 x1 y1) #:transparent)

(define (field* pict)
  (field pict null))

(define (make-playing-field l-picts [w/h 1])
  (define total-area
    (for/sum ([p (in-list l-picts)])
      (pict-area p)))
  (define h
    (sqrt (/ total-area w/h)))
  (define w
    (* h w/h))
  (define longest-word-width
    (for/fold ([so-far 0])
              ([p (in-list l-picts)])
      (max so-far (pict-width p))))
  (field*
   (cond
     [(or (infix: longest-word-width <= w)
          (infix: longest-word-width <= h))
      (blank w h)]
     [(infix: w >= h)
      (define h
        (/ longest-word-width w/h))
      (blank longest-word-width h)]
     [else
      (define w
        (* w/h longest-word-width))
      (blank w longest-word-width)])))


(define (pack-picts/naive word-assocs)
  (match-define (cons so-far to-go)
    (map car word-assocs))
  (for/fold ([so-far so-far])
            ([p (in-list to-go)])
    (cond
      [(infix: (pict-height so-far) < (pict-width so-far))
       (vc-append 2 p so-far)]
      [else
       (hc-append 2 so-far (turn-clockwise p))])))
#|
(define (pack-picts/basic raw-words)
  ;; https://www.codeproject.com/Articles/210979/
  ;;   Fast-optimizing-rectangle-packing-algorithm-for-bu
  (define word-picts
    (sort (map car raw-words) > #:key pict-height))
|#

(define (archimedean-spiral width height)
  (let ([e (/ width height)])
    (λ (t)
      (let ([t (* 0.1 t)])
        (cons (* e t (cos t)) 
              (* t (sin t)))))))
#;
(define (rectangular-spiral width height)
  (let* ([dy 4]
         [dx (* dy (/ width height))]
         [x 0]
         [y 0])
    (λ (t)
      (define sign
        (if (infix: t < 0)
            -1
            1))
      #|
;; don't understand the bitwise and
switch ((Math.sqrt(1 + 4 * sign * t) - sign) & 3) {
      case 0:  x += dx; break;
      case 1:  y += dy; break;
      case 2:  x -= dx; break;
      default: y -= dy; break;
    }
return [x, y];
|#)))

(define (pack-picts word-assocs [w/h 1])
  (define word-picts
    (map car (sort word-assocs > #:key cdr)))
  (define playing-field
    (make-playing-field word-picts w/h))
  (panorama
   (for/fold ([playing-field playing-field])
             ([w (in-list word-picts)])
     (place-word-pict w playing-field))))


(define (place-word-pict w playing-field)
  (error 'place-word-pict "TODO"))


(pack-picts/naive (render-picts words 'area))

(pack-picts/naive (render-picts words 'font-size))

(pack-picts/naive (render-picts words 'font-size/sqrt))





