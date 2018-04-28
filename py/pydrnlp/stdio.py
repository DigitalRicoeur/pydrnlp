# -*- coding: utf-8 -*-

import json
import sys
from pydrnlp import segtokenize

def dumpln(jsOut):
    json.dump(jsOut,sys.stdout)
    sys.stdout.write("\n")

    
if __name__ == "__main__":
    if sys.argv[1] == "tokenize":
        jsIn = json.load(sys.stdin)
        dumpln(segtokenize.tokenizeDocList(jsIn))
    elif sys.argv[1] == "tokenizer-revision":
        dumpln(segtokenize.revision())
    else:
        jdumpln(False)
        sys.exit(1)
        


        
