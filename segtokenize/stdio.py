# coding: utf8

import json
import sys
from segtokenize import segtokenize

if __name__ == "__main__":
    if sys.argv[1] == "tokenize":
        jsIn = json.load(sys.stdin)
        print(json.dumps(segtokenize.handle(jsIn)))
    elif sys.argv[1] == "revision":
        print(json.dumps(segtokenize.revision()))
    else:
        print(json.dumps(False))
        sys.exit(1)
        
