# -*- coding: utf-8 -*-
"""Provides a Standard-IO–based interface to pydrnlp.segtokenize.

This module does nothing unless ƒpyflow{__name__ == "__main__"}.

It expects to be invoked with exactly one command-line argument:

* ƒpyflow{"tokenizer-revision"}: Prints the JSON result of
  pydrnlp.segtokenize.tokenizerRevision() to standard out.

* ƒpyflow{"tokenize"}: Reads a JSON-formatted argument for
  pydrnlp.segtokenize.tokenizeDocList() and prints the
  JSON result to standard out.

In any other case, the module prints JSON ƒpyflow{False} 
to standard out and exits with a non-zero exit code.
"""

import json
import sys
from pydrnlp.segtokenize import tokenizerRevision, tokenizeSegmentList


if __name__ == "__main__":
    def dumpln(jsOut):
        json.dump(jsOut,sys.stdout)
        sys.stdout.write("\n")
        
    def badUsage():
        jdumpln(False)
        sys.exit(1)
        
    if (2 != len(sys.argv)):
        badUsage()
    elif (sys.argv[1] == "tokenize"):
        jsIn = json.load(sys.stdin)
        dumpln(tokenizeSegmentList(jsIn))
    elif (sys.argv[1] == "tokenizer-revision"):
        dumpln(tokenizerRevision())
    else:
        badUsage()
        


        
