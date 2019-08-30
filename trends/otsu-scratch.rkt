#lang racket

(require syntax/parse/define)

(define-for-syntax (unlet-var-transformer stx)
  ;; include outer form in error?
  (raise-syntax-error #f "reference to unlet variable" stx))

(define-simple-macro (unlet [name:id ...] body:expr ...+)
  (let-syntax ([name unlet-var-transformer] ...) body ...))

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
(define (in-stats-accum-variances acc)
  (stats-accum-lst acc))

(define elements '(4 3 2 1))
(define weights '(94 93 92 91))

(let prep-high/init-low
  ([high-relevance-acc empty-stats-accum]
   [elements elements]
   [weights weights]
   [low-kont in-stats-accum-variances])
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
     (vector-immutable
      (in-stats-accum-variances high-relevance-acc)
      (low-kont empty-stats-accum))]))
