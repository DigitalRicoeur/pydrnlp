#lang racket/base

(define-syntax-rule (export lib ...)
  (begin (require lib ...)
         (provide (all-from-out lib) ...)))

(export "kernel.rkt"
        "annotation.rkt"
        "signature.rkt"
        "pass0/doc.rkt"
        )

(module* typed typed/racket/base
  (define-syntax-rule (export lib ...)
    (begin (require lib ...)
           (provide (all-from-out lib) ...)))
  (export (submod "kernel.rkt" typed)
          (submod "annotation.rkt" typed)
          (submod "signature.rkt" typed)
          "pass0/doc.rkt"
          ))
