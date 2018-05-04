#lang racket/base

;; It turns out there isn't actually anything about
;; annotations we want to change between passes.

(define-syntax-rule (export lib ...)
  (begin (require lib ...)
         (provide (all-from-out lib) ...)))

(export "kernel.rkt"
        (submod "annotation/compound.rkt"
                contracted))

(module* typed typed/racket/base
  (define-syntax-rule (export lib ...)
    (begin (require lib ...)
           (provide (all-from-out lib) ...)))
  (export (submod "kernel.rkt"
                  typed)
          (submod "annotation/compound.rkt"
                  typed)))
