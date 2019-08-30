#lang typed/racket

(provide otsu-threshold)

(require math/statistics
         racket/flonum)

(define-type Element Positive-Integer)
(define-type Weight Positive-Integer)
(define-type Threshold Element)
(define-type Variance
  ;; excludes +nan.0, includes +inf.0
  ;; rationale: ∀α (not (or (< α +nan.0) (< +nan.0 α)))
  ;; ... which is a problem when minimizing variance
  (U Positive-Float Float-Zero))
(define-type Variance/Nan (U Variance Float-Nan))

(: otsu-threshold (-> (Listof Element) Threshold))
(define (otsu-threshold unsorted)
  (define-values [elements weights]
    (sort-and-weight-elements unsorted))
  (minimize-intra-class-variance elements weights
                                 #:old-high-relevance empty-statistics
                                 #:old-threshold 1
                                 #:old-variance +inf.0))


(: minimize-intra-class-variance (-> (Listof Element) (Listof Weight)
                                     #:old-high-relevance statistics
                                     #:old-threshold Threshold
                                     #:old-variance Variance
                                     Threshold))
(define (minimize-intra-class-variance elements weights
                                       #:old-high-relevance high-relevance-stats
                                       #:old-threshold old-threshold
                                       #:old-variance old-variance)
  (if (or (null? elements)
          (null? weights))
      old-threshold
      (let* ([new-threshold (first elements)]
             [high-relevance-stats (update-statistics high-relevance-stats
                                                      new-threshold
                                                      (first weights))]
             [elements (rest elements)]
             [weights (rest weights)]
             [new-variance/nan
              (let ([bias-mode #true]) ;; correct for frequency weights
                (fl+ ;; optimizer could do this, but let's make it explicit
                 (statistics-variance high-relevance-stats #:bias bias-mode)
                 (real->double-flonum
                  (variance elements weights #:bias bias-mode))))])
        (define-values [min-variance best-threshold]
          (cond
            [(nan? new-variance/nan)
             ;; keep old variance but use smaller threshold
             (values old-variance new-threshold)]
            [(< old-variance new-variance/nan)
             (values old-variance old-threshold)]
            [else
             ;; in the = case, we prefer the smaller threshold
             (values new-variance/nan new-threshold)]))
        (minimize-intra-class-variance
         #:old-variance min-variance
         #:old-threshold best-threshold
         #:old-high-relevance high-relevance-stats
         elements
         weights))))
     
(: sort-and-weight-elements (-> (Listof Element)
                                (Values (Listof Element)
                                        (Listof Weight))))
(define (sort-and-weight-elements unsorted)
  (count-samples (sort unsorted >)))


