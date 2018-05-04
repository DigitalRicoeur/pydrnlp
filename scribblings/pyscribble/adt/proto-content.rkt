#lang racket/base

(require (submod "proto-content/types.rkt"
                 contracted))
(provide (all-from-out
          (submod "proto-content/types.rkt"
                  contracted)))

(module* typed typed/racket/base
  (require (submod "proto-content/types.rkt"
                   typed))
  (provide (all-from-out
            (submod "proto-content/types.rkt"
                    typed))))

