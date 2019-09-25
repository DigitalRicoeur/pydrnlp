#lang racket/base

(require racket/contract)

(provide python-revision-value/c
         python-revision-function/c)

(define/final-prop python-revision-value/c
  (flat-rec-contract python-revision-value/c
    #f
    exact-integer?
    (listof python-revision-value/c)))

(define/final-prop python-revision-function/c
  (-> python-revision-value/c))
