#lang typed/racket

(provide sample-tree-threshold/otsu)

;; Ported by Philip McGrath
;; from the Python implementation by James Broda.

(require math/statistics
         math/array
         math/flonum
         "sample-tree.rkt")

(module+ test
  (require typed/rackunit))

;; All of the components of the intra-class variance are
;; actually constrained to be Nonnegative-Float,
;; but we accept a weaker type in exchange for the storage
;; efficiency of FlVector / FlArray.

(: sample-tree-threshold/otsu (-> Sample-Tree Positive-Integer))
(define (sample-tree-threshold/otsu tree)
  (define disable-lazyness? #false)
  ;; descending order
  (define-values [elements weights]
    (sample-tree-iterate/arrays/descending tree))
  (cond
    [(< (array-size elements) 2)
     1]
    [else
     (: e*w-arr (Array Float)) ;; Positive-Float
     (: grand-total Positive-Float)
     (define-values [e*w-arr grand-total]
       (let* ([e*w-arr (array->flarray
                        ;; strict because it is forced for grand-total anyway
                        (parameterize ([array-strictness disable-lazyness?])
                          (array* elements weights)))]
              [grand-total ;; ?? assert flrational? ?? re flonum overflow
               (flvector-sum (flarray-data e*w-arr))])
         (values e*w-arr (assert grand-total positive?))))
     (define threshold-arr
       (parameterize ([array-strictness disable-lazyness?])
         (array-drop-last elements)))
     (define intra-class-variance-arr : (Array Float) ;; Nonnegative-Float
       (parameterize ([array-strictness disable-lazyness?])
         (let ([high-relevance-class-arr
                (make-relevance-class-array
                 threshold-arr
                 (array-drop-last weights)
                 (array-drop-last e*w-arr)
                 grand-total)]
               [low-relevance-class-arr
                (array-reverse
                 (make-relevance-class-array
                  (array-reverse (array-rest elements))
                  (array-reverse (array-rest weights))
                  (array-reverse (array-rest e*w-arr))
                  grand-total))])
           (inline-array-map fl+
                             high-relevance-class-arr
                             low-relevance-class-arr))))
     (define-values [min-variance best-threshold]
       (for/fold ([old-variance : Float +inf.0] ;; Nonnegative-Float
                  [old-threshold : Positive-Integer 1])
                 ([new-threshold (in-array threshold-arr)]
                  [new-variance (in-array intra-class-variance-arr)])
         (cond
           [(>= old-variance new-variance)
            ;; In the = case, prefer the smaller threshold.
            (values new-variance new-threshold)]
           [else
            ;; handles nan
            (values old-variance old-threshold)])))
     (eprintf "otsu:\n")
     (eprintf "    min-variance = ~a\n" min-variance)
     (eprintf "  best-threshold = ~a\n" best-threshold)
     best-threshold]))

(: make-relevance-class-array (-> (Array Positive-Integer)
                                  (Array Positive-Integer)
                                  (Array Float) ;; Positive-Float
                                  Positive-Float
                                  (Array Float))) ;; Nonnegative-Float)
(define (make-relevance-class-array e-arr w-arr e*w-arr grand-total)
  (: ->r-c-weighted-variance (-> statistics Positive-Float Nonnegative-Float))
  (define (->r-c-weighted-variance stats running-total)
    (define bias-mode #true) ;; correct for frequency weights
    (* (statistics-variance stats #:bias bias-mode)
       (ann (/ running-total grand-total) Nonnegative-Float)))
  (: get-this-stats (-> statistics Indexes statistics))
  (define (get-this-stats old-stats js)
    (update-statistics old-stats
                       (array-ref e-arr js)
                       (array-ref w-arr js)))
  (define e*w-running-totals
    (flvector-sums (flarray-data (array->flarray e*w-arr))))
  (define shape (array-shape e-arr))
  (define ret
    (array->flarray (make-array shape 0.0)))
  (define data
    (flarray-data ret))
  (for/fold ([stats : statistics empty-statistics])
            ([js : Indexes (in-array-indexes shape)])
    (let* ([j (vector-ref js 0)]
           [running-total (assert (flvector-ref e*w-running-totals j)
                                  positive?)]
           [stats (get-this-stats stats js)])
      (flvector-set! data
                     j
                     (->r-c-weighted-variance stats running-total))
      stats))
  ret)

        
(: make-slicer (-> (Listof Slice-Spec) (∀ (A) (-> (Array A) (Array A)))))
(define ((make-slicer spec) arr)
  (array-slice-ref arr spec))
(: array-rest (∀ (A) (-> (Array A) (Array A))))
(define array-rest
  (make-slicer (list (:: 1 #f))))
(module+ test
  (check-true
   (equal? (array-rest (array #['a 'b 'c]))
           (array #['b 'c])))
  (check-true
   (equal? (array-rest (array #[]))
           (array #[]))))

(: array-reverse (∀ (A) (-> (Array A) (Array A))))
(define array-reverse
  (make-slicer (list (:: #f #f -1))))
(module+ test
  (check-true
   (equal? (array-reverse (array #['a 'b 'c]))
           (array #['c 'b 'a]))))

(: array-drop-last (∀ (A) (-> (Array A) (Array A))))
(define (array-drop-last arr)
  (array-reverse (array-rest (array-reverse arr))))
(module+ test
  (check-true
   (equal? (array-drop-last (array #['a 'b 'c]))
           (array #['a 'b]))))
    
