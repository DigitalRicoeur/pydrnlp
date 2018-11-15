# -*- coding: utf-8 -*-
"""**TODO: Update this docstring.**

A simple interface to pydrnlp.segtokenize based on standard IO.

This module does nothing unless __name__ == "__main__".

When run, it immediately writes the JSON result of
pydrnlp.segtokenize.tokenizerRevision() to standard out.
It then enters an JSON IO loop (see pydrnlp.jsonio.mapJsonLines),
processing the values with pydrnlp.segtokenize.tokenizeSegmentList().

See pydrnlp.jsonio for invariants about the JSON IO format.
"""

from pydrnlp.jsonio import mapJsonLines, writeJsonLine
from pydrnlp.tokenizer.tokenize import revision, tokenize

if __name__ == "__main__":
    writeJsonLine(revision())
    mapJsonLines(tokenize)

