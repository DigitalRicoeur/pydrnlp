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
     (define e*w-arr : (Array Positive-Float)
       ;; strict because it is forced for grand-total anyway
       (array-strict
        (array-map (λ ([e : Positive-Integer]
                       [w : Positive-Integer])
                     (fl (* e w)))
                   elements
                   weights)))
     (define grand-total : Positive-Float
       ;; ?? assert flrational? ?? re flonum overflow
       (array-all-sum e*w-arr))
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
     best-threshold]))
    
;; doesn't actually work :( Positive-Float-No-NaN isn't exported
;;(define-type Nonnegative-Float-No-NaN
;;  ;; Excludes +nan.0, includes +inf.0
;;  ;; Rationale: ∀α (not (or (< α +nan.0) (< +nan.0 α)))
;;  ;; ... which is a problem when minimizing variance
;;  (U Positive-Float Float-Zero))

(define-type __make-relevance-class-array_t
  (-> (Array Positive-Integer)
      (Array Positive-Integer)
      (Array Positive-Float)
      Positive-Float
      (Array Float))) ;; Nonnegative-Float
(: make-relevance-class-array __make-relevance-class-array_t)
(define make-relevance-class-array
  (let ()
    (: wrap (-> Indexes
                (-> statistics Nonnegative-Float Indexes
                    (values statistics Positive-Float))
                (-> statistics Positive-Float Nonnegative-Float)
                (Array Float)))
    (define (wrap shape get-this ->r-c-weighted-variance)
      (define ret
        (array->flarray (make-array shape 0.0)))
      (define data
        (flarray-data ret))
      (for/fold ([stats : statistics empty-statistics]
                 [running-total : Nonnegative-Float 0.0])
                ([js : Indexes (in-array-indexes shape)])
        (let-values ([{stats running-total}
                      (get-this stats running-total js)])
          (flvector-set! data
                         (vector-ref js 0)
                         (->r-c-weighted-variance stats running-total))
          (values stats running-total)))
      ret)
    (: make-relevance-class-array __make-relevance-class-array_t)
    (define (make-relevance-class-array e-arr w-arr e*w-arr grand-total)
      (wrap (array-shape e-arr)
            (λ ([old-stats : statistics]
                [old-running-total : Nonnegative-Float]
                [js : Indexes])
              : (values statistics Positive-Float)
              (values (update-statistics old-stats
                                         (array-ref e-arr js)
                                         (array-ref w-arr js))
                      ;; TODO consider flvector-sums
                      (+ old-running-total (array-ref e*w-arr js))))
            (λ ([stats : statistics]
                [running-total : Positive-Float])
              : Nonnegative-Float
              (define bias-mode #true) ;; correct for frequency weights
              (* (statistics-variance stats #:bias bias-mode)
                 (ann (/ running-total grand-total) Nonnegative-Float)))))
    make-relevance-class-array))



        
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
    
