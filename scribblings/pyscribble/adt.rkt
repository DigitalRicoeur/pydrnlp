#lang racket/base

(require "adt/pass1.rkt")
(provide (all-from-out "adt/pass1.rkt"))

(module* typed typed/racket/base
  (require (submod "adt/pass1.rkt" typed))
  (provide (all-from-out (submod "adt/pass1.rkt" typed))))

