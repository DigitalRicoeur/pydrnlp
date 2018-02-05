# tokenize #

A (currently experimental) attempt at doing tokenization
for use by future Digital Ricoeur widgets.

Currently the implementation of the hard work is in the `segtokenize`
Python package (which expects Python 3).
It is built on the `spacy` NLP library.
There are interfaces using standard IO or as a RESTful web application
using Flask (which seems to be faster, even for tiny examples).
Both interfaces communicate in JSON.

The `use-segtokenize.rkt` Racket module contains examples of using
both interfaces.


