#lang racket

(define empty-statistics null)
(define (update-statistics stats e w)
  (cons (cons e w) stats))
(define (statistics-variance stats #:bias b)
  (caar stats))

(struct stats-accum (stats lst)
  #:transparent)

(define empty-stats-accum
  (stats-accum empty-statistics '(+nan.0)))
(define (update-stats-accum acc e w)
  (match-let* ([(stats-accum stats lst) acc]
               [stats (update-statistics stats e w)]
               [bias-mode #true] ;; correct for frequency weights
               [variance (statistics-variance stats #:bias bias-mode)])
  (stats-accum stats (cons variance lst))))

(define elements '(4 3 2 1))
(define weights '(a b c d))


(let update-high/init-low ([high-relevance-acc empty-stats-accum]
                           [elements elements]
                           [weights weights])
  (match* {elements weights}
    [{(cons e elements) (cons w weights)}
     (let-values ([{low-relevance-acc high-relevance-acc}
                   (update-high/init-low
                    (update-stats-accum high-relevance-acc e w)
                    elements
                    weights)])
       (values (update-stats-accum low-relevance-acc e w)
               high-relevance-acc))]
    [{'() '()}
     (values empty-stats-accum
             high-relevance-acc)]))


