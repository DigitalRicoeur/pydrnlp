# -*- coding: utf-8 -*-
"""
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
