#lang racket

(require math/statistics)

#;
(for/fold ([old-variance +inf.0]
           [old-threshold 1]
           #:result old-threshold)
          ([{new-threshold
             new-variance/nan}
            ...])
  (cond
    [(nan? new-variance/nan)
     ;; keep old variance but use smaller threshold
     (values old-variance new-threshold)]
    [(< old-variance new-variance/nan)
     (values old-variance old-threshold)]
    [else
     ;; in the = case, we prefer the smaller threshold
     (values new-variance/nan new-threshold)]))

#;
(define-values [elements weights]
  (sort-and-weight-elements unsorted))

(define (make-variance/nan-sequence elements weights)
  (let ([bias-mode #true]) ;; correct for frequency weights
    (for/fold ([stats empty-statistics]
               [acc null]
               #:result (for/list ([stats (cons stats acc)])
                          (statistics-variance stats #:bias bias-mode)))
              ([e (in-list elements)]
               [w (in-list weights)])
      (values (update-statistics stats e w)
              (cons stats acc)))))

#|
(require math/statistics)

(define-type Element Positive-Integer)
(define-type Weight Positive-Integer)

(define (??? unsorted)
  (define-values [elements-sorted-< weights-sorted-<]
    (count-samples (sort unsorted <)))
  (for/fold ([low-relevance-stats empty-statistics]
             [low-relevance-stats* null]
             [elements null]
             [weights null])
            ([e (in-list elements-sorted-<)]
             [w (in-list elements-sorted-<)])
    (values (update-statistics low-relevance-stats e w)
            (cons low-relevance-stats low-relevance-stats*)
            (cons e elements)
            (cons w weights))
|#
  