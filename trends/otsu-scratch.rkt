#lang typed/racket

(module types typed/racket/base
  (provide (all-defined-out))
  (define-type Element Positive-Integer)
  (define-type Weight Positive-Integer)
  (define-type Threshold Element)
  (define-type Variance
    ;; excludes +nan.0, includes +inf.0
    ;; rationale: ∀α (not (or (< α +nan.0) (< +nan.0 α)))
    ;; ... which is a problem when minimizing variance
    (U Positive-Float Float-Zero))
  (define-type Variance/Nan (U Variance Float-Nan)))
(require 'types)
(module fake:math/statistics typed/racket
  (provide count-samples
           statistics
           empty-statistics
           update-statistics
           statistics-variance)
  (require (submod ".." types)
           (only-in math/statistics count-samples))
  (struct _statistics ([lst : (Listof (Pairof Element Weight))])
    #:transparent
    #:type-name statistics)
  (define empty-statistics (_statistics null))
  (: update-statistics (-> statistics Element Weight statistics))
  (define (update-statistics stats e w)
    (_statistics (cons (cons e w) (_statistics-lst stats))))
  (: statistics-variance (-> statistics #:bias Any Variance/Nan))
  (define (statistics-variance stats #:bias b)
    (define lst (_statistics-lst stats))
    (if (null? lst) +nan.0 (real->double-flonum (caar lst)))))
(require 'fake:math/statistics)





(module stats-accum typed/racket
  (provide stats-accum
           empty-stats-accum
           update-stats-accum
           stats-accum-variance/nan-sequence)
  (require (submod ".." types)
           (submod ".." fake:math/statistics))
  (struct _stats-accum ([stats : statistics]
                        [lst : (Listof Variance/Nan)])
    #:type-name stats-accum
    #:transparent)
  (: make-stats-accum (-> statistics (Listof Variance/Nan) stats-accum))
  (define (make-stats-accum stats lst)
    (define bias-mode #true) ;; correct for frequency weights
    (define variance/nan : Variance/Nan
      (statistics-variance stats #:bias bias-mode))
    (_stats-accum stats
                  (if (null? lst)
                      (list variance/nan)
                      (cons variance/nan lst))))
  (define empty-stats-accum (make-stats-accum empty-statistics '()))
  (: update-stats-accum (-> stats-accum Element Weight stats-accum))
  (define (update-stats-accum acc e w)
    (match-let ([(_stats-accum stats lst) acc])
      (make-stats-accum (update-statistics stats e w) lst)))
  (: stats-accum-variance/nan-sequence (-> stats-accum (Listof Variance/Nan)))
  (define (stats-accum-variance/nan-sequence acc)
    (_stats-accum-lst acc)))
(require 'stats-accum)

(: sort-and-weight-elements (-> (Listof Element)
                                (Values (Listof Element)
                                        (Listof Weight))))
(define (sort-and-weight-elements unsorted)
  (count-samples (sort unsorted >)))

(module+ test
  (define elements '(4 3 2 1))
  (define weights '(94 93 92 91))
  (define unsorted
   (shuffle
    (for/fold ([unsorted : (Listof Element) null])
              ([e (in-list elements)]
               [w (in-list weights)]
               #:when #t
               [i (in-range w)])
     (cons e unsorted))))
  (sort-and-weight-elements unsorted))
  


(: calculate-intra-class-variance/nan
   (-> (Listof Variance/Nan) (Listof Variance/Nan)
       #;(Non-Empty-Listof Variance/Nan)
       ;; FAKE!!
       #;(Listof (Pairof Variance/Nan Variance/Nan))
       Any))
(define (calculate-intra-class-variance/nan high-relevance-seq
                                            low-relevance-seq)
  (for/list : (Listof Any) (;; <--- FAKE!!!
                            [high-variance/nan : Variance/Nan (in-list high-relevance-seq)]
                            [low-variance/nan : Variance/Nan (in-list low-relevance-seq)])
    ;; optimizer could do this, but let's make it explicit
    #;(fl+ high-variance/nan low-variance/nan)
    ;; FAKE!!
    (cons high-variance/nan low-variance/nan)))


(define handle-unsorted
  (letrec
      ([sorted->high+low-seqs
        : (-> (Listof Element) (Listof Weight)
              (Values (Listof Variance/Nan) (Listof Variance/Nan)))
        (λ (elements weights)
          (let prep-high/init-low
            ([high-relevance-acc empty-stats-accum]
             [elements elements]
             [weights weights]
             [low-kont stats-accum-variance/nan-sequence])
            (match* {elements weights}
              [{(cons e elements) (cons w weights)}
               (prep-high/init-low
                (update-stats-accum high-relevance-acc e w)
                elements
                weights
                (λ (low-relevance-acc)
                  (low-kont
                   (update-stats-accum low-relevance-acc e w))))]
              [{'() '()}
               (values
                (stats-accum-variance/nan-sequence high-relevance-acc)
                (low-kont empty-stats-accum))])))]
       [handle-unsorted
        : (-> (Listof Element) AnyValues) #;Threshold
        (λ (unsorted)
          (define-values [elements weights]
            (sort-and-weight-elements unsorted))
          (sorted->high+low-seqs elements weights))])
    handle-unsorted))
       
(module+ test
  (handle-unsorted unsorted))
