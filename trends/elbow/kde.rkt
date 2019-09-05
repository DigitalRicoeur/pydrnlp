#lang typed/racket

(provide sample-tree-threshold/kde)

(require "sample-tree.rkt"
         "make-kde.rkt"
         math/array
         math/flonum)

(module+ test
  (require typed/rackunit))

(define max-attempts : Positive-Index
  1)
(define bandwidth-scale-factor : Positive-Flonum
  (assert 0.9
          flprobability?))

(: sample-tree-threshold/kde (-> Sample-Tree Positive-Integer))
(define (sample-tree-threshold/kde tree)
  ;; inspired by numpy.linspace
  ;; see https://github.com/numpy/numpy/issues/5437
  ;; for lots of things I'm not dealing with
  (define x-max (sample-tree-max-element tree))
  (cond
    [(or (= x-max 0) (= x-max 1)) ;; prove Positive-Integer in else
     ;; bail out on 0 or 1
     1]
    [else
     (define bandwidth->kde 
       (sample-tree->bandwidth->kde tree))
     (define test-xs
       (make-uniform-array x-max
                           ;; ??? this seems like a lot
                           (* 20 (sample-tree-sequence-length tree))))
     (define bandwidth (sample-tree->init-bandwidth/scott tree))
     (let loop : Positive-Integer ([bandwidth : Nonnegative-Flonum bandwidth]
                                   [attempt : Positive-Fixnum 1])
       (define kde (bandwidth->kde bandwidth))
       (define peaks (find-relative-minima (flarray-map kde test-xs)))
       (cond
         [(< 0 (array-size peaks))
          (define fl-threshold
            (array-all-min (array-indexes-ref test-xs peaks)))
          ;; an int x such that (>= x fl-threshold) will also work with:
          (max 1 (exact-ceiling fl-threshold))]
         [(< attempt max-attempts)
          (loop (* bandwidth bandwidth-scale-factor)
                (add1 attempt))]
         [else
          ;; give up
          1]))]))


(: sample-tree->init-bandwidth/scott (-> Sample-Tree Nonnegative-Flonum))
(define (sample-tree->init-bandwidth/scott tree)
  (define num-dimensions 1.0)
  (expt (max (fl (sample-tree-effective-samples-count tree)) 1.0) ;; prove to typechecker
        (/ -1.0 (+ num-dimensions 4.0))))
     
(: make-uniform-array (-> Positive-Integer Nonnegative-Integer FlArray))
(define (make-uniform-array max num-samples)
  ;; inspired by numpy.linspace
  ;; see https://github.com/numpy/numpy/issues/5437
  ;; for lots of things I'm not dealing with
  ;; bail out on max of 0 or 1
  (define ret
    (array->flarray
     (make-array (vector-immutable (+ 2 num-samples))
                 0.0)))
  (define data (flarray-data ret))
  (define len : Index
    (flvector-length data))
  (define last-index
    (sub1 len))
  (flvector-set! data last-index (fl max))
  (define step : Float
    (let ([step : Exact-Rational (/ (sub1 max) num-samples)])
      (fl step)))
  ;; don't iteratively add step: avoid weird float issues
  (for ([i (in-range 1 last-index)])
    (flvector-set! data i (* (fl i) step)))
  ret)

(: find-relative-minima (-> (Array Flonum) (Array Indexes)))
(define (find-relative-minima data)
  ;; Based on the implementation of `scipy.signal.argrelmin`
  ;; and `scipy.signal.argrelextrema`.
  ;; NOTE: The docs for `argrelmin` say that
  ;;   "flat minima (more than one sample wide) are not detected.
  ;;    In case of one-dimensional data `find_peaks`
  ;;    can be used to detect all local minima, including flat ones,
  ;;    by calling it with negated data."
  ;; Does this matter for us?
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Relative extrema are calculated by finding locations where
  ;; ``comparator(data[n], data[n+1:n+order+1])`` is True.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; comparator=np.less, axis=0, order=1, mode='clip'
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; 'clip' mode means ref out of edges of array is treated as first/last element
  (define size (array-size data))
  (cond
    [(= 0 size) ;; proves (sub1 size) is an index
     (array #[])]
    [else
     (define shape : Indexes
       (vector-immutable size))
     (define (shift [column-transform : (-> Index Index)]) : (Array Flonum)
       (array-transform data shape (λ ([v : Indexes])
                                     (vector-immutable
                                      (column-transform (vector-ref v 0))))))
     (define last-col : Index
       (sub1 size))
     (define plus : (Array Flonum)
       (shift (λ ([col : Index])
                (define col* (add1 col))
                (if (< col* last-col) col* last-col))))
     (define minus : (Array Flonum)
       (shift (λ ([col : Index])
                (if (= 0 col) 0 (sub1 col)))))
     (define mask : (Array (U #f Indexes)) 
       (array-and
        (array-map fl< data plus)
        (array-map fl< data minus)
        (indexes-array shape)))
     (list-array->array
      (array-axis-reduce
       mask 0
       (λ ([len : Index][ref : (-> Integer (U #f Indexes))])
         (for*/fold ([acc : (Listof Indexes) null])
                    ([i (in-range (sub1 len) -1 -1)]
                     [v (in-value (ref i))]
                     #:when v)
           (cons v acc)))))]))

(module+ test
  (check-true
   (equal? (find-relative-minima (array #[3.0 2.0 1.0 2.0 3.0]))
           (array #['#(2)]))))

