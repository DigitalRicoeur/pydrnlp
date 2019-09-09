#lang typed/racket

(provide sample-tree-threshold/kde)

;; Adapted by Philip McGrath
;; from the Python implementation by James Broda.

(require "sample-tree.rkt"
         "make-kde.rkt"
         math/array
         math/flonum)

(module+ test
  (require typed/rackunit)
  (require/typed
   rackunit
   [#:struct string-info ([value : String])]))

(define max-attempts : Positive-Index
  1000)
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
     (eprintf "bail out on ~a\n" x-max)
     1]
    [else
     (define bandwidth->kde 
       (sample-tree->bandwidth->kde tree))
     (define test-xs
       (make-uniform-array x-max
                           ;; doesn't seem to work well:
                           ;; (* 20 (sample-tree-sequence-length tree))))
                           ;; (exact-floor (/ x-max 5))))
                           (* 2 x-max)))
     (define bandwidth (sample-tree->init-bandwidth/scott tree))
     (let loop : Positive-Integer ([bandwidth : Nonnegative-Flonum bandwidth]
                                   [attempt : Positive-Fixnum 1])
       (define kde (bandwidth->kde bandwidth))
       (define peaks
         ;; ??
         (find-relative-minima/handle-flat (flarray-map kde test-xs)))
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
          (eprintf "give up\n")
          1]))]))


(: sample-tree->init-bandwidth/scott (-> Sample-Tree Nonnegative-Flonum))
(define (sample-tree->init-bandwidth/scott tree)
  (define num-dimensions 1.0)
  (expt (max (fl (sample-tree-effective-samples-count tree)) 1.0) ;; prove to typechecker
        (/ -1.0 (+ num-dimensions 4.0))))
     
(: make-uniform-array (-> Positive-Integer Nonnegative-Integer FlArray))
(define (make-uniform-array max num-samples)
  ;; inspired by numpy.linspace and linear-seq from plot/utils
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
  (for ([i (in-range 0 last-index)])
    (flvector-set! data i (add1 (* (fl i) step))))
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

(: find-relative-minima/handle-flat (-> (Array Flonum) (Array In-Indexes)))
(define (find-relative-minima/handle-flat data)
  ;; see notes on find-relative-minima
  ;; this is like `scipy.signal.find_peaks`
  (define size : Index
    (array-size data))
  (define (data-ref [i : Integer]) : Flonum
    (array-ref data (vector-immutable i)))
  (for*/array: ([i (in-range 1 (sub1 size))]
                [this (in-value (data-ref i))]
                #:when (< this (data-ref (sub1 i)))
                #:when (< this (for*/fold ([next-different : Flonum this])
                                          ([j (in-range (add1 i) size)]
                                           [next (in-value (data-ref j))]
                                           #:unless (= next this)
                                           #:final #t)
                                 next)))
    : In-Indexes
    (vector-immutable i)))

(module+ test
  (for ([arr : (Array Flonum) (list (array #[3.0 2.0 1.0 2.0 3.0])
                                    (array #[3.0 2.0 1.0 1.0 2.0 3.0])
                                    (array #[3.0 2.0 1.0 1.0 1.0 2.0 3.0]))])
    (check-true
     (equal? (find-relative-minima/handle-flat arr)
             (array #['#(2)])))))
