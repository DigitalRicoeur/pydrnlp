#lang typed/racket

(provide threshold/otsu)

(require math/statistics
         math/array
         math/flonum
         "sample-tree.rkt")

(module+ test
  (require typed/rackunit))

(define-type Nonnegative-Float-No-Nan
  ;; Excludes +nan.0, includes +inf.0
  ;; Rationale: ∀α (not (or (< α +nan.0) (< +nan.0 α)))
  ;; ... which is a problem when minimizing variance
  (U Positive-Float Float-Zero))

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
      (array-all-sum e*w-arr))
    (parameterize ([array-strictness disable-lazyness?])
      (define threshold-arr elements)
      (define high-relevance-class-v/n-arr
        (arrays-foldl-variance/nan elements weights))
      (define high-relevance-class-probability-arr
        (array-foldlmap-probability grand-total e*w-arr))
      (define-values [low-relevance-class-v/n-arr
                      low-relevance-class-probability-arr]
        (let ([elements (array-reverse (array-rest elements))]
              [weights (array-reverse (array-rest weights))]
              [e*w-arr (array-reverse (array-rest e*w-arr))])
          (: reverse/append-final (∀ (A) (-> (Array A) (Array A) (Array A))))
          (define (reverse/append-final arr final)
            (array-append* (list (array-reverse arr) final)))
          (values (reverse/append-final 
                   (arrays-foldl-variance/nan elements weights)
                   (array #[+nan.0])) ;; same as for empty-statistics
                  (reverse/append-final
                   (array-foldlmap-probability grand-total e*w-arr)
                   (array #[0.0])))))
      (define intra-class-variance/nan-arr : (Array Nonnegative-Float)
        (array-map (λ ([high-v/n : Nonnegative-Float]
                       [high-p : Nonnegative-Float-No-Nan]
                       [low-v/n : Nonnegative-Float]
                       [low-p : Nonnegative-Float-No-Nan])
                     (+ (* high-v/n high-p) (* low-v/n low-p)))
                   high-relevance-class-v/n-arr
                   high-relevance-class-probability-arr
                   low-relevance-class-v/n-arr
                   low-relevance-class-probability-arr))
      (define-values [min-variance best-threshold]
        (for/fold ([old-variance : Nonnegative-Float-No-Nan +inf.0]
                   [old-threshold : Positive-Integer 1])
                  ([new-threshold (in-array threshold-arr)]
                   [new-variance/nan (in-array intra-class-variance/nan-arr)])
          (cond
            [(>= old-variance new-variance/nan)
             ;; In the = case, prefer the smaller threshold.
             (values new-variance/nan new-threshold)]
            [else
             ;; handles nan
             (values old-variance old-threshold)])))
      best-threshold)))
    
        
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
              (vector-ref vec (vector-ref js 0)))))
         #;
         (λ #:∀ (A B) (xs init proc*)
           (define this : (Array B)
             (array-lazy
              (build-simple-array
               (array-shape xs)
               (λ ([js : Indexes])
                 (define j (vector-ref js 0))
                 (proc* js (if (= j 0)
                               init
                               (array-ref this (vector-immutable (sub1 j)))))))))
           this)])
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


(: array-foldlmap-probability (-> Positive-Float
                                  (Array Positive-Float)
                                  (Array Nonnegative-Float-No-Nan)))
(define (array-foldlmap-probability grand-total e*w-arr)
  ;; lengths guaranteed elsewhere
  (define running-totals : (Array Positive-Float)
    (arrays-foldlmap
     (array-rest e*w-arr) (array-ref e*w-arr #(0))
     (λ ([e*w : Positive-Float]
         [prev : Positive-Float])
       (+ prev e*w))))
  (array-map 
   (λ ([running-total : Positive-Float]) : Nonnegative-Float-No-Nan
     (let ([x (/ running-total grand-total)])
       (if (nan? x) ;; (/ +inf.0 +inf.0)
           +inf.0
           x)))
   running-totals))
        
(: arrays-foldl-variance/nan (-> (Array Positive-Integer)
                                 (Array Positive-Integer)
                                 (Array Nonnegative-Float)))
(define (arrays-foldl-variance/nan elements weights)
  (define stats-arr : (Array statistics)
    (arrays-foldlmap
     elements weights empty-statistics
     (λ ([e : Positive-Integer]
         [w : Positive-Integer]
         [s : statistics])
       (update-statistics s e w))))
  (define bias-mode #true) ;; correct for frequency weights
  (array-strict
   (array-map (λ ([stats : statistics])
                (statistics-variance stats #:bias bias-mode))
              stats-arr)))

           
        
(: make-slicer (-> (Listof Slice-Spec) (∀ (A) (-> (Array A) (Array A)))))
(define ((make-slicer spec) arr)
  (array-slice-ref arr spec))
(: array-rest (∀ (A) (-> (Array A) (Array A))))
(define array-rest
  (make-slicer (list ::... (:: 1 #f))))
(module+ test
  (check-true
   (equal? (array-rest (array #['a 'b 'c]))
           (array #['b 'c])))
  (check-true
   (equal? (array-rest (array #[]))
           (array #[]))))

(: array-reverse (∀ (A) (-> (Array A) (Array A))))
(define array-reverse
  (make-slicer (list ::... (:: #f #f -1))))
(module+ test
  (check-true
   (equal? (array-reverse (array #['a 'b 'c]))
           (array #['c 'b 'a])))
  (check-true
   (equal? (array-reverse (array #[#['a 'b 'c]
                                   #['d 'e 'f]]))
           (array #[#['c 'b 'a]
                    #['f 'e 'd]]))))

;; On a 32-bit system, the max Index is:
;;   (sub1 (expt 2 28)) = 268,435,455
;; Is that large enough?
;; On a 64-bit system, a maximally-long vector
;; would be over 2 million TiB,
;; so don't worry about that.
