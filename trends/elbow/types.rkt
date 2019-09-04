#lang typed/racket/base

(provide Element
         Weight
         Threshold
         Variance
         Variance/Nan)

(define-type Element Positive-Integer)
(define-type Weight Positive-Integer)
(define-type Threshold Element)
(define-type Variance
  ;; excludes +nan.0, includes +inf.0
  ;; rationale: ∀α (not (or (< α +nan.0) (< +nan.0 α)))
  ;; ... which is a problem when minimizing variance
  (U Positive-Float Float-Zero))
(define-type Variance/Nan (U Variance Float-Nan))
