# pydrnlp #
## NLP library for Digital Ricoeur ##

This repository contains:

1. A Python 3 library providing some NLP functionality needed
   by Digital Ricoeur.

2. A Racket package that serves as a wrapper for the Python library.

The implementation is still experimental.
The initial goal is to support the NLP functionality needed
to implement our own versions of the widgets that
Digital Ricoeur currently gets from Voyant.

The Python portion of this repository is in the `py` directory,
particularly the `py/pydrnlp/` directory.
It is built on the `spaCy` NLP library.
Currently, the Racket code expects but does not check
that a Conda environment exist at `py/condaenv/` based
on the specification at `py/environment.yml`.

For more information, see the documentation built from
`scribblings/pydrnlp.scrbl`.


