# -*- coding: utf-8 -*-
"""A simple interface to pydrnlp.segtokenize based on standard IO.

This module does nothing unless __name__ == "__main__".

When run, it immediately writes the JSON result of
pydrnlp.segtokenize.tokenizerRevision() to standard out.
It then enters an IO loop, reading JSON from standard in,
processing the values with pydrnlp.segtokenize.tokenizeSegmentList(),
and writing the JSON results to standard out.

This module imposes the invariant that JSON values must be
delimited by newlines (i.e. "\n") and that the JSON values
in its input may not use the newline character internally,
even where insignificant whitespace is allowed by the JSON spec.
Using newlines as a delimiter avoids a limitation of Python's
json.load(), which blocks until it encounters an EOF.
This module also writes JSON with a terminating newline,
though Racket's JSON parser doesn't need this.
"""

import json
import sys
from pydrnlp.segtokenize import tokenizerRevision, tokenizeSegmentList

def dumpln(jsOut):
    """Writes the JSON form of jsOut to sys.stdout,
    followed by a newline, then flushes the output.
    """
    json.dump(jsOut,sys.stdout)
    sys.stdout.write("\n")
    sys.stdout.flush()

if __name__ == "__main__":
    dumpln(tokenizerRevision())
    for line in sys.stdin:
        jsIn = json.loads(line)
        dumpln(tokenizeSegmentList(jsIn))
