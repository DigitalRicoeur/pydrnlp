#lang typed/racket/base

(provide sample-tree->bandwidth->kde)

;; Forked by Philip from plot/private/common/kde.
;; Used under license LGPL v2.

(require math/base
         math/flonum
         "sample-tree.rkt")

#|
(define-struct/exec kde ([proc : (-> Flonum Flonum)]
                         [x-min : (U Flonum #f)]
                         [x-max : (U Flonum #f)])
  [(λ ([this : Kde][x : Flonum])
     ((kde-proc this) x))
   : (-> Kde Flonum Flonum)]
  #:type-name Kde)
|#

(: sample-tree->bandwidth->kde (-> Sample-Tree (-> Nonnegative-Flonum
                                                   (-> Flonum Flonum))))
(define (sample-tree->bandwidth->kde this)
  (define-values [elements weights]
    (sample-tree-iterate/flvectors/ascending this))
  (flvectors->bandwidth->kde elements weights))


(: make-kde/windowed (-> FlVector FlVector Flonum Flonum (-> Flonum Flonum)))
;; Can assume that xs is sorted.
;; Make a naive KDE, but uses windows to keep from adding
;; Gaussians more than max-dist away
(define ((make-kde/windowed xs ws h max-dist) y)
  (define i : (U #f Nonnegative-Integer)
    (flvector-find-index (λ ([x : Float])
                           (<= (abs (- x y)) max-dist))
                         xs))
  (cond
    [(not i)
     0.0]
    [else
     (define j : (U #f Nonnegative-Integer)
       (flvector-find-index
        (λ ([x : Float]) (> (abs (- x y)) max-dist))
        #:start i
        xs))
     (for/fold ([p : Flonum  0.0])
               ([x : Flonum (in-flvector xs i j)]
                [w : Flonum (in-flvector ws i j)])
       (define z (/ (- x y) h))
       (+ p (* w (flexp (- (sqr z))))))]))

(: flvector-find-index (->* [(-> Float Boolean)
                             FlVector]
                            [#:start Nonnegative-Integer]
                            (U #f Nonnegative-Integer)))
(define (flvector-find-index stop? flv #:start [start 0])
  (for/fold ([old : (U #f Nonnegative-Integer) #f])
            ([i : Nonnegative-Integer (in-naturals start)]
             [x : Float (in-flvector flv start)]
             #:when (stop? x)
             #:final #t)
    i))

;; Below this amount, we're fine with a kernel not contributing to the sum
(define eps 1e-06)
;; 1e-06 is the density returned at just over 5 standard deviations away from zero. If the estimate
;; needs more sensitivity, then KDE is almost certainly the wrong thing to do.

(: weight-max-dist (-> Flonum Flonum Flonum))
;; Returns the maximum distance at which unnormalized kernel (with weight w and width h) will
;; contribute at least eps to the sum
(define (weight-max-dist w h)
  (define a (/ w eps))
  (if (a . > . 1.0)
      (* h (* (flsqrt 2.0) (flsqrt (fllog a))))
      0.0))


(: flvectors->bandwidth->kde (-> FlVector
                                 FlVector
                                 (-> Nonnegative-Flonum (-> Flonum Flonum))))
(define (flvectors->bandwidth->kde xs ws)
  ;; assume sorted ASCENDING and same length
  (cond
    [(= 0 (flvector-length xs))
     (λ (h)
       ;(kde (λ (y) 0.0) #f #f)
       (λ (y) 0.0))]
    [else
     (let* ([ws (let ([sum-ws (flvector-sum ws)])
                  (inline-flvector-map
                   (λ ([w : Float]) (/ w sum-ws))
                   ws))]
            [x-min-base (flvector-ref xs 0)]
            [x-max-base (flvector-ref xs (sub1 (flvector-length xs)))])
       (define dist-coeffcients
         (flvector-map (λ ([w : Float])
                         (define a (/ w eps))
                         (if (a . > . 1.0)
                             (* (flsqrt 2.0) (flsqrt (fllog a)))
                             0.0))
                       ws))
       (λ (h)
         (define max-dist
           ;;(for/fold ([acc : Flonum -inf.0])
           ;;          ([w : Flonum (in-flvector ws)])
           ;;  (max acc (weight-max-dist w h))))
           (for/fold ([acc : Flonum -inf.0])
                     ([coef : Flonum (in-flvector dist-coeffcients)])
             (max acc (* h coef))))
         (define c (/ 1.0 (* (flsqrt pi) h)))
         ;; The range of non-zero KDE values
         (define x-min (- x-min-base max-dist))
         (define x-max (+ x-max-base max-dist))
         ;; Parameters for fast-gauss
         ;; Make the KDE functions
         (define kde/windowed
           (make-kde/windowed xs ws h max-dist))
         (define f
           (λ ([y : Flonum])
             (if (< x-min y x-max)
                 (* c (kde/windowed y))
                 0.0)))
         ;(kde f x-min x-max)
         f))]))
