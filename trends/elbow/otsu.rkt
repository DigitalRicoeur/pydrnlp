#lang typed/racket

(require math/statistics
         math/flonum
         "sample-tree.rkt"
         "types.rkt")

;; TODO: what about flonum overflow?

(: find-threshold/otsu (-> (Sequenceof Element) Threshold))
(define (find-threshold/otsu unsorted)
  (define tree
    (sequence->sample-tree unsorted))
  (define grand-total
    (sample-tree-total tree))
  (define-values [thresholds
                  high-relevance-classes
                  low-relevance-classes]
    (let-values ([{elements weights} (sample-tree-iterate-descending tree)])
      (values elements
              (reverse (build-sample-class-list elements weights))
              (build-sample-class-list (reverse elements) (reverse weights)))))
  (define-values [min-variance best-threshold]
    (for/fold ([old-variance : Variance +inf.0]
               [old-threshold : Element 1])
              ([new-threshold (in-list thresholds)]
               [high-relevance (in-list high-relevance-classes)]
               [low-relevance (in-list low-relevance-classes)])
      (define new-variance/nan
        (calculate-intra-class-variance/nan high-relevance low-relevance))
      (cond
        [(flnan? new-variance/nan)
         ;; keep old variance but use smaller threshold
         (values old-variance new-threshold)]
        [(< old-variance new-variance/nan)
         (values old-variance old-threshold)]
        [else
         ;; in the = case, we prefer the smaller threshold
         (values new-variance/nan new-threshold)])))
best-threshold)

(: calculate-intra-class-variance/nan (-> sample-class sample-class Variance/Nan))
(define (calculate-intra-class-variance/nan a b)
  (+ (* (sample-class-variance/nan a)
        ;; probability w/o division by grand-total
        ;; b/c division makes it hard to exclude infinities/nans
        (fl (sample-class-total a)))
     (* (sample-class-variance/nan b)
        (fl (sample-class-total b)))))
        

(struct sample-class ([total : Nonnegative-Integer]
                      [variance/nan : Variance/Nan])
  #:transparent)

(: build-sample-class-list (-> (Listof Element) (Listof Weight) (Listof sample-class)))
(define (build-sample-class-list elements weights)
  (: make-sample-class (-> Nonnegative-Integer statistics sample-class))
  (define (make-sample-class total stats)
    (define bias-mode #true) ;; correct for frequency weights
    (sample-class total
                  (statistics-variance stats #:bias bias-mode)))
  (define-values [total-with-final stats-with-final classes-before-final]
    (for/fold ([total-with-prev : Nonnegative-Integer 0]
               [stats-with-prev : statistics empty-statistics]
               [classes-before-prev : (Listof sample-class) null])
              ([e (in-list elements)]
               [w (in-list weights)])
      (define prev-sample-class
        (make-sample-class total-with-prev stats-with-prev))
      (values (+ total-with-prev (* e w))
              (update-statistics stats-with-prev e w)
              (cons prev-sample-class
                    classes-before-prev))))
  (cons (make-sample-class total-with-final stats-with-final)
        classes-before-final))
      
#;
(define (make-weighted-variance/nan-list grand-total elements weights)
  (: statistics->variance/nan (-> Nonnegative-Integer statistics Variance/Nan))
  (define (weighted-variance/nan running-total stats)
    (define bias-mode #true) ;; correct for frequency weights
    (statistics-variance stats #:bias bias-mode))
  (define-values [running-total stats acc]
    (for/fold ([running-total : Nonnegative-Integer 0]
               [stats : statistics empty-statistics]
               [acc : (Listof Variance/Nan) null])
              ([e (in-list elements)]
               [w (in-list weights)])
      (values (+ running-total (* e w))
              (update-statistics stats e w)
              (cons (weighted-variance/nan running-total stats)
                    acc))))
  (cons (weighted-variance/nan running-total stats)
        acc))

#|
(: make-weighted-variance/nan-list
   (-> Nonnegative-Integer (Listof Element) (Listof Weight) (Listof Variance/Nan)))
(define (make-weighted-variance/nan-list grand-total elements weights)
  (: weighted-variance/nan (-> Nonnegative-Integer statistics Variance/Nan))
  (define (weighted-variance/nan running-total stats)
    (define bias-mode #true) ;; correct for frequency weights
    (* (statistics-variance stats #:bias bias-mode)
       (fl (/ running-total grand-total))))
  (define-values [running-total stats acc]
    (for/fold ([running-total : Nonnegative-Integer 0]
               [stats : statistics empty-statistics]
               [acc : (Listof Variance/Nan) null])
              ([e (in-list elements)]
               [w (in-list weights)])
      (values (+ running-total (* e w))
              (update-statistics stats e w)
              (cons (weighted-variance/nan running-total stats)
                    acc))))
  (reverse (cons (weighted-variance/nan running-total stats)
                 acc)))
|#
