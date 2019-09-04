#lang typed/racket/base

(provide Element
         Weight
         Threshold
         Nonnegative-Float-No-Nan)

(define-type Element Positive-Integer)
(define-type Weight Positive-Integer)
(define-type Threshold Element)
(define-type Nonnegative-Float-No-Nan
  ;; Excludes +nan.0, includes +inf.0
  ;; Rationale: ∀α (not (or (< α +nan.0) (< +nan.0 α)))
  ;; ... which is a problem when minimizing variance
  (U Positive-Float Float-Zero))
