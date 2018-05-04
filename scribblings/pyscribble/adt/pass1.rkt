#lang racket/base

(define-syntax-rule (export lib ...)
  (begin (require lib ...)
         (provide (all-from-out lib) ...)))

(export "kernel.rkt"
        "annotation.rkt"
        "signature.rkt"
        "proto-content.rkt"
        "pass1/export.rkt"
        "pass1/module.rkt"
        )

(module* typed typed/racket/base
  (define-syntax-rule (export lib ...)
    (begin (require (submod lib typed) ...)
           (provide (all-from-out (submod lib typed)) ...)))
  (export "kernel.rkt" 
          "annotation.rkt" 
          "signature.rkt" 
          "proto-content.rkt"
          "pass1/export.rkt"
          "pass1/module.rkt"
          ))

