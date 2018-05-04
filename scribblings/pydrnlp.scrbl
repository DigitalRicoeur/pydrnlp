#lang scribble/manual

@title[#:version ""]{
 pydrnlp: NLP Library for Digital Ricœur
}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[pydrnlp]

@(require (for-label (except-in racket
                                date
                                date?)
                     pydrnlp
                     json
                     ))

The @racketmodname[pydrnlp] repository contains:
@itemlist[
 #:style 'ordered
 @item{A Python 3 library providing some NLP functionality 
  needed by Digital Ricoeur.
 }
 @item{A Racket package that serves as a wrapper
  for the Python library.
  }]

@margin-note{The functionality described in this document
is still at an experimental stage.
}

The initial goal is to support the NLP functionality needed
to implement our own versions of the widgets that
Digital Ricoeur currently gets from Voyant.

To use the Python functionality, you must have a @exec{conda}
executable in your PATH for the Conda package manager.
The recommended way to obtain this is by installing the
@hyperlink["https://conda.io/miniconda.html"]{Miniconda}
distribution, though the larger Anaconda distribution
should work as well.
As long as you have @exec{conda} installed, Racket will automatically
take care of the details of creating or updating the
virtual environment and managing Python dependencies.

@;{
    
 The directory `py/compare-env-vars/` contains Racket
 scripts and data that were used interactively
 to see what environment variables are set by activating
 a Conda or venv virtual environment.
    
 The `old/` directory contains various things
}

@include-section["racket.scrbl"]

