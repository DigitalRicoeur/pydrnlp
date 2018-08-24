# -*- coding: utf-8 -*-
"""A simple interface to pydrnlp.segtokenize based on standard IO.

This module does nothing unless __name__ == "__main__".
When run, it:

- Immediately writes the JSON result of
  pydrnlp.segtokenize.tokenizerRevision() to standard out.

- Blocks reading a JSON value from standard in.

- Calls pydrnlp.segtokenize.tokenizeSegmentList() on the
  input value and writes the JSON result to standard out.

- Exits with a 0 exit code.
"""

import json
import sys
from pydrnlp.segtokenize import tokenizerRevision, tokenizeSegmentList

if __name__ == "__main__":
    def dumpln(jsOut):
        json.dump(jsOut,sys.stdout)
        sys.stdout.write("\n")
        sys.stdout.flush()
    dumpln(tokenizerRevision())
    jsIn = json.load(sys.stdin)
    dumpln(tokenizeSegmentList(jsIn))
    sys.exit(0)
