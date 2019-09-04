#lang typed/racket

(provide threshold/otsu)

(require math/statistics
         math/array
         racket/flonum
         math/flonum
         "sample-tree.rkt")

(module+ test
  (require typed/rackunit))

;; All of the components of the intra-class variance are
;; actually constrained to be Nonnegative-Float,
;; but we accept a weaker type in exchange for the storage
;; efficiency of FlVector / FlArray.

(: threshold/otsu (-> (Sequenceof Positive-Integer) Positive-Integer))
(define (threshold/otsu unsorted)
  (define disable-lazyness? #false)
  (let/ec return : Positive-Integer
    ;; descending order
    (define-values [elements weights]
      (let-values ([{elements weights}
                    (sample-tree-iterate-descending
                     (sequence->sample-tree unsorted))])
        (when (or (null? elements) (null? (cdr elements)))
          ;; guaranteed length at least 2
          (return 1))
        (values (list->array elements)
                (list->array weights))))
    (define e*w-arr : (Array Positive-Float)
      (array-strict
       (array-map (λ ([e : Positive-Integer]
                      [w : Positive-Integer])
                    (fl (* e w)))
                  elements
                  weights)))
    (define grand-total : Positive-Float
      ;; ?? assert flrational? ?? re flonum overflow
      (array-all-sum e*w-arr))
    (parameterize ([array-strictness disable-lazyness?])
      (define threshold-arr
        (array-drop-last elements))
      (define-values [high-relevance-class-var-arr
                      high-relevance-class-probability-arr]
        (values (arrays-foldlmap-variance threshold-arr (array-drop-last weights))
                (make-probability-array grand-total (array-drop-last e*w-arr))))
      (define-values [low-relevance-class-var-arr
                      low-relevance-class-probability-arr]
        (let ([elements (array-reverse (array-rest elements))]
              [weights (array-reverse (array-rest weights))]
              [e*w-arr (array-reverse (array-rest e*w-arr))])
          (values (array-reverse 
                   (arrays-foldlmap-variance elements weights))
                  (array-reverse
                   (make-probability-array grand-total e*w-arr)))))
      (define intra-class-variance-arr : (Array Float)
        (array-map (λ ([high-v : Float]
                       [high-p : Float]
                       [low-v : Float]
                       [low-p : Float])
                     (+ (* high-v high-p) (* low-v low-p)))
                   high-relevance-class-var-arr
                   high-relevance-class-probability-arr
                   low-relevance-class-var-arr
                   low-relevance-class-probability-arr))
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
      best-threshold)))
    
;; doesn't actually work :( Positive-Float-No-NaN isn't exported
;;(define-type Nonnegative-Float-No-NaN
;;  ;; Excludes +nan.0, includes +inf.0
;;  ;; Rationale: ∀α (not (or (< α +nan.0) (< +nan.0 α)))
;;  ;; ... which is a problem when minimizing variance
;;  (U Positive-Float Float-Zero))


(: arrays-foldlmap
   (∀ (A B) (case->
             (-> (Array A) (Array A) B (-> A A B B) (Array B))
             (-> (Array A) B (-> A B B) (Array B)))))
(define arrays-foldlmap
  (let ([wrap
         : (∀ (A B) (-> (Array A) B (-> Indexes B B) (Array B)))
         (λ #:∀ (A B) (xs init proc*)
           ;; like array-lazy, but avoids promises
           (define len : Index (array-size xs))
           (define shape : Indexes (array-shape xs))
           (define vec : (Mutable-Vectorof B) (make-vector len init))
           (for/fold ([prev : B init])
                     ([js (in-array-indexes shape)])
             (define this (proc* js prev))
             (vector-set! vec (vector-ref js 0) this)
             this)
           (build-simple-array
            shape
            (λ ([js : Indexes])
              (vector-ref vec (vector-ref js 0)))))])
    (case-lambda
      #:∀ (A B)
      [(xs ys init proc)
       (wrap xs init (λ ([js : Indexes]
                         [prev : B])
                       (proc (array-ref xs js)
                             (array-ref ys js)
                             prev)))]
      [(xs init proc)
       (wrap xs init (λ ([js : Indexes]
                         [prev : B])
                       (proc (array-ref xs js)
                             prev)))])))


(: fold/build-flarray (∀ (A) (-> Indexes A (-> A Indexes A) (-> A Flonum) FlArray)))
(define #:∀ (A) (fold/build-flarray shape init get-this this->flonum)
  (define ret
    (array->flarray (make-array shape 0.0)))
  (define data
    (flarray-data ret))
  (for/fold ([prev : A init])
            ([js : Indexes (in-array-indexes shape)])
    (define this (get-this prev js))
    (flvector-set! data (vector-ref js 0) (this->flonum this))
    this)
  ret)

(: make-probability-array (-> Positive-Float
                                  (Array Positive-Float)
                                  (Array Float))) ;; Nonnegative-Float
(define (make-probability-array grand-total e*w-arr)
  ;; length guaranteed elsewhere
  (let ([e*w0 : Positive-Float (array-ref e*w-arr #(0))] 
        [e*w-arr (array-rest e*w-arr)])
  (fold/build-flarray
   (array-shape e*w-arr)
   e*w0
   (λ ([prev : Positive-Float] [js : Indexes]) : Positive-Float
     (+ prev (array-ref e*w-arr js)))
   (λ ([running-total : Positive-Float]) : Nonnegative-Float
     (/ running-total grand-total)))))
        
(: arrays-foldlmap-variance (-> (Array Positive-Integer)
                                (Array Positive-Integer)
                                (Array Float))) ;; Nonnegative-Float
(define (arrays-foldlmap-variance elements weights)
  (fold/build-flarray
   (array-shape elements)
   empty-statistics
   (λ ([prev : statistics]
       [js : Indexes])
     (update-statistics prev (array-ref elements js) (array-ref weights js)))
   (λ ([stats : statistics]) : Nonnegative-Float
     (define bias-mode #true) ;; correct for frequency weights
     (statistics-variance stats #:bias bias-mode))))

        
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
    
