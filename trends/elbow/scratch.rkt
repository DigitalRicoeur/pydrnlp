#lang racket

(require math/array)

#;
(define (find-peaks-cwt src-arr widths-lst)
  ;; based on the implementation of scipy.signal.find_peaks_cwt
  (let* ([gap-thresh (ceiling (car widths-lst))]
         [max-distances (map (λ (n) (/ n 4.0)) widths-lst)]
         [cwt-arr (cwt src-arr widths-lst)]
         [ridge-lines (identify-ridge-lines cwt-arr max-distances gap-thresh)]
         )
    ...))

;(define (identify-ridge-lines cwt-arr max-distances gap-thresh)
;; based on the implementation of scipy.signal.find_peaks_cwt
;; all_max_cols = _boolrelextrema(matr, np.greater, axis=1, order=1)

(define (boolrelextrema data)
  ;; based on the implementation of scipy.signal.find_peaks_cwt
  ;comparator=np.greater, axis=1, order=1, mode='clip'
  (define data-shape (array-shape data))
  (define (transform proc)
    (array-transform data
                     data-shape
                     (match-lambda
                       [(vector row col)
                        (vector-immutable row (proc col))])))
  (define last-col (sub1 (vector-ref data-shape 1)))
  (define plus
    (transform
     (λ (col)
       (min (add1 col) last-col))))
  (define minus
    (transform
     (λ (col)
       (max (sub1 col) 0))))
  (array-and
   (array> data plus)
   (array> data minus)))

(module+ test
  ;(array #[#[#f #f #t #f #f]])
  #;
  (boolrelextrema
   (array #[#[1 2 3 2 1]])))
      

(define (arange stop)
  ;; numpy.arange work-alike
  (axis-index-array (vector-immutable stop) 0))
  
(define (cwt data-arr widths-lst)
  ;; based on the implementation of scipy.signal.cwt
  (define wavelet ricker)
  (define data-len (vector-ref (array-shape data-arr) 0))
  (array-append*
   (for/list ([i (in-naturals)]
              [w (in-list widths-lst)])
     (define wavelet-data
       (wavelet (min (* 10 w) data-len) w))
     (array-axis-insert
      (fftconvolve-same data-arr wavelet-data)
      0))))

(define (fftconvolve-same a b)
  ;; based in the implementation of scipy.signal.fftconvolve
  ;; with mode='same'
  (let* ([a-shape (array-shape a)]
         [a (array-fft a)]
         [b (array-fft b)]
         [ret (array-inverse-fft (array* a b))])
    ;; FIXME shapes power-of-two?
    (array-slice-ref
     ret
     (for/list ([a-dim (in-vector a-shape)]
                [ret-dim (in-vector (array-shape ret))])
       (define start (quotient (- ret-dim a-dim) 2))
       (:: start (+ start a-dim) 1)))))

(define (ricker num-points width)
  ;; based on the implementation of scipy.signal.ricker
  (let* ([A (/ 2 (* (sqrt (* 3 width))
                    (expt pi 0.25)))]
         [wsq (sqr width)]
         [xsq (let ([offset (/ (sub1 num-points) 2)])
                (build-array (vector-immutable num-points)
                             (match-lambda
                               [(vector i)
                                (sqr (- i offset))])))]
         [mod (array-map (λ (n) (/ (- 1 n) wsq)) xsq)]
         [gauss (array-map (λ (n) (exp (/ (- n) (* 2 wsq)))) xsq)])
    (array* (array-scale mod A) gauss)))
  
