#lang info

(define collection "pydrnlp")
(define pkg-desc "Racket interface to Python NLP library for Digital Ricoeur")
(define version "0.0")
(define pkg-authors '(philip))

(define install-collection "conda.rkt")

(define compile-omit-paths '("py/condaenv/"
                             "py/pydrnlp/"
                             ))

(define deps '("base"
               ("adjutor" #:version "0.2.2")
               "math-lib"
               "pict-lib"
               "srfi-lite-lib"
               "draw-lib"
               "typed-racket-lib"
               "typed-racket-more"
               ))

(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "at-exp-lib"
                     "functional-lib"
                     "rackunit-typed"
                     ))

(define scribblings '(("scribblings/pydrnlp.scrbl"
                       ()
                       ("Digital Ricœur" 0)
                       )))

