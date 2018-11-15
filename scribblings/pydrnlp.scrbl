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
  needed by Digital Ricœur.
 }
 @item{A Racket package that serves as a wrapper
  for the Python library.
  }]

@margin-note{
 The functionality described in this document
 is still at an experimental stage.
}

The initial goal is to support the NLP functionality needed
to implement our own versions of the widgets that
Digital Ricœur currently gets from Voyant.
By design, @racketmodname[pydrnlp] aims to provide
a minimal layer over the needed functionality from Python.
Any features that don't strictly have to be here
should usually be implemented instead by higher-level clients
of this library.
Accordingly, @racketmodname[pydrnlp] has no dependencies on
other Digital Ricœur Racket packages.
(Of course, determining which features ``strictly have to be here''
involves interpretation.
One guiding principle is that @racketmodname[pydrnlp] should
abstract over the specifics of the communication format
expected by the Python layer.)

To use the Python functionality, you must have a @exec{conda}
executable in your PATH for the Conda package manager.
The recommended way to obtain this is by installing the
@hyperlink["https://conda.io/miniconda.html"]{Miniconda}
distribution, though the larger Anaconda distribution
should work as well.
As long as you have @exec{conda} installed, Racket will automatically
take care of the details of creating or updating the
virtual environment and managing Python dependencies.


@(local-table-of-contents)

@include-section["racket.scrbl"]
@include-section["python.scrbl"]


