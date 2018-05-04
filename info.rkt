#lang info

(define collection "pydrnlp")

(define deps '("base"
               ("adjutor" #:version "0.2.2")
               "math-lib"
               "pict-lib"
               "srfi-lite-lib"
               ))

(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     ))

(define scribblings '(("scribblings/pydrnlp.scrbl"
                       ()
                       ("Digital Ricœur" 0)
                       )))

(define compile-omit-paths '("py/condaenv/"
                             "py/pydrnlp/"
                             ))

(define install-collection
  "conda.rkt")

(define pkg-desc "Racket interface to Python NLP library for Digital Ricoeur")

(define version "0.0")

(define pkg-authors '(philip))
