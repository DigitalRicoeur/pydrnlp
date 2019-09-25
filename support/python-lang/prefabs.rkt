#lang racket/base

(provide (struct-out definition)
         (struct-out var-definition)
         (struct-out fun-definition)
         (struct-out class-definition)
         (struct-out bad-return))

(struct definition (name docstring)
  #:prefab)

(struct var-definition definition ()
  #:prefab)

(struct fun-definition definition (formals async? maybe-return)
  #:prefab)

(struct class-definition definition (supers body)
  #:prefab)

(struct bad-return (stx)
  #:prefab)
