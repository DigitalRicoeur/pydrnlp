#lang info

(define collection "pydrnlp")
(define pkg-desc "Racket interface to Python NLP library for Digital Ricoeur")
(define version "0.0.1")
(define pkg-authors '(philip))

(define install-collection "conda.rkt")

(define compile-omit-paths '("py/condaenv/"
                             "py/pydrnlp/"))

(define deps '("base"
               ("adjutor" #:version "0.2.2")
               "math-lib"
               "pict-lib"
               "srfi-lite-lib"
               "draw-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "markdown"
               "reprovide-lang"
               ("db-lib" #:version "1.4")
               ("sql" #:version "1.5")
               "data-lib"
               ("ricoeur-tei-utils" #:version "0.5.7")
               "gregor-lib"))

(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "at-exp-lib"
                     "functional-lib"
                     "rackunit-typed"
                     "_-exp"
                     "rackjure"))

(define scribblings '(("scribblings/pydrnlp.scrbl"
                       (multi-page)
                       ("Digital Ricœur" 0)
                       )))

