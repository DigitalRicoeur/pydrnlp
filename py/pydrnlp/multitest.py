# -*- coding: utf-8 -*-
"""See if we can read multiple JSON values.
"""

import json
import sys

def increment(x):
    return x + 1

def dumpln(jsOut):
    json.dump(jsOut,sys.stdout)
    sys.stdout.write("\n")
    sys.stdout.flush()

# json.load expects an EOF

if __name__ == "__main__":
    dumpln(["Hi","world"])
    for line in sys.stdin:
        jsIn = json.loads(line)
        jsOut = list(map(increment,jsIn))
        dumpln(jsOut)
