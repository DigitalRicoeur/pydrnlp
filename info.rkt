#lang info

(define pkg-name "pydrnlp")
(define collection "pydrnlp")
(define pkg-desc
  "Racket interface to Python NLP library for Digital Ricoeur")
(define version "0.1")
(define pkg-authors '(philip))

(define install-collection
  "support/conda.rkt")

(define compile-omit-paths '("py/condaenv/"))

(define scribblings
  '(("scribblings/pydrnlp.scrbl"
     (multi-page)
     ("Digital Ricœur" 0))))

(define deps
  '(["base" #:version "7.4"]
    ["ricoeur-kernel" #:version "0.0.1"]
    ["ricoeur-tei-utils" #:version "0.5.90"]
    ["adjutor" #:version "0.2.5"]
    "python-tokenizer"
    "math-lib"
    "pict-lib"
    "draw-lib"
    "typed-racket-lib"
    "typed-racket-more"
    "reprovide-lang"
    ["db-lib" #:version "1.4"]
    ["sql" #:version "1.5"]
    "gregor-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"
    "markdown"
    "rackunit-typed"
    "_-exp"
    "at-exp-lib"
    "rackjure"))

