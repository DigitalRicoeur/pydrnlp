#lang scribble/manual

@title[#:version ""]{
 pydrnlp: NLP Library for Digital Ricœur
}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[pydrnlp]

@(require scribble/core
          "bibliography.rkt"
          "lib.rkt"
          (for-label (except-in racket
                                date
                                date?)
                     pydrnlp))

The @racketmodname[pydrnlp] repository contains:
@itemlist[
 #:style 'ordered
 @item{A Python 3 package providing some NLP functionality 
  needed by Digital Ricœur, which is implemented using
  @spaCy; and
 }
 @item{A Racket package that manages the Python-implemented
  functionality and integrates it with other Digital Ricoeur tools.
  }]

The initial goal is to support the NLP functionality needed
to implement our own versions of some of the widgets that
Digital Ricœur currently gets from Voyant.

@margin-note{
 The functionality described in this document
 is still at an experimental stage.
}

To use the Python functionality, you must have a @exec{conda}
executable in your @envvar{PATH} for the Conda package manager.
The recommended way to obtain this is by installing the
@hyperlink["https://conda.io/miniconda.html"]{Miniconda}
distribution, though the larger Anaconda distribution
should work as well.
As long as you have @exec{conda} installed,
@seclink["setup" #:doc '(lib "scribblings/raco/raco.scrbl")
         @exec{raco setup}]
will manage the Python dependencies
and virtual environment automatically.
Note that you @italic{do not} need to have Conda
or even Python installed merely to build @racketmodname[pydrnlp]
(including the Python documentation)
or to use the Racket-implemented functionality.

@(define (nested/inset #:depth n . body)
   (for/fold ([body body])
             ([i (in-range n)])
     (nested #:style 'inset body)))
@nested/inset[#:depth 4]{
 @elem[#:style (style #f (list (background-color-property "yellow")))]{
  @bold{NOTE:}}
 Much of this document is badly out-of-date.
 We are in the process of updating it in conjunction
 with the initial release.
}

@(local-table-of-contents)

@include-section["trends.scrbl"]
@include-section["support.scrbl"]
@|bibliography-section|

