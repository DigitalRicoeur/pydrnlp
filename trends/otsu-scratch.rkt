#lang typed/racket

(require math/statistics)

(define-type Element Positive-Integer)
(define-type Weight Positive-Integer)
(define-type Threshold Element)
(define-type Variance
  ;; excludes +nan.0, includes +inf.0
  ;; rationale: ∀α (not (or (< α +nan.0) (< +nan.0 α)))
  ;; ... which is a problem when minimizing variance
  (U Positive-Float Float-Zero))
(define-type Variance/Nan (U Variance Float-Nan))

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
  (sort-and-weight-elements unsorted)
  (handle-unsorted unsorted))



(struct stats-accum ([stats : statistics]
                     [lst : (Listof Variance/Nan)])
  ;; TODO: lexically protect constructor
  #:transparent)

(define empty-stats-accum
  (stats-accum empty-statistics '()))

(: update-stats-accum (-> stats-accum Element Weight stats-accum))
(define (update-stats-accum old e w)
  (let* ([stats (update-statistics (stats-accum-stats old) e w)]
         [bias-mode #true] ;; correct for frequency weights
         [variance/nan (statistics-variance stats #:bias bias-mode)])
    (stats-accum stats (cons variance/nan (stats-accum-lst old)))))

(define stats-accum-variance/nan-sequence stats-accum-lst)



(: sort-and-weight-elements (-> (Listof Element)
                                (Values (Listof Element)
                                        (Listof Weight))))
(define (sort-and-weight-elements unsorted)
  (count-samples (sort unsorted >)))
  


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

(: sorted->high+low-seqs
   (-> (Listof Element) (Listof Weight)
       (Values (Listof Variance/Nan) (Listof Variance/Nan))))
(define (sorted->high+low-seqs elements weights)
  (let prep-high/init-low
    ([high-relevance-acc empty-stats-accum]
     [elements elements]
     [weights weights]
     [low-kont : (-> stats-accum (Listof Variance/Nan))
               stats-accum-variance/nan-sequence])
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
        (low-kont empty-stats-accum))])))

(: handle-unsorted (-> (Listof Element) AnyValues))
(define (handle-unsorted unsorted)
  (define-values [elements weights]
    (sort-and-weight-elements unsorted))
  (sorted->high+low-seqs elements weights))
