#lang info

(define collection "pydrnlp")

(define deps '("base"
               "adjutor"
               "math-lib"
               "pict-lib"
               "srfi-lite-lib"
               ))

(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))

(define scribblings '(("scribblings/pydrnlp.scrbl"
                       ()
                       ("Digital Ricœur" 0)
                       )))

(define pkg-desc "Racket interface to Python NLP library for Digital Ricoeur")

(define version "0.0")

(define pkg-authors '(philip))
