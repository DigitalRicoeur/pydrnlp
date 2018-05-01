# -*- coding: utf-8 -*-
"""Used for interactive development while refining pydrnlp.drtoken.

The purpose is to help find tokens that are not filtered out,
but probably should be.
Examining these can help to identify possible improvements to
pydrnlp.drtoken.tokenShouldUse.

While this module is documented, it is not intended to be a stable API
or behavior specification: it should be freely modified for interactive
experimentation.

When run with ƒpyflow{__name__ == "__main__"}, this module expects
exactly one command-line argument, which should be a path
to which to write the output.
It uses readWriteSuspiciousTokens to process input from sys.stdin.read().
Progress messages are written to the standard output.
If it is run with the wrong number of command-line arguments,
this module exits with a non-zero exit code.
"""

import spacy
from spacy.lang.en.stop_words import STOP_WORDS
import json
import sys
from pydrnlp.drtoken import tokenQuicklyFails
from pydrnlp.annotations import IteratorOf

nlp = spacy.load('en', disable=['parser','ner'])


def getSuspiciousTokens(str : str) -> IteratorOf(
        {"text": str,
         "lemma": str,
         "pos": str,
         "lemmaIsStop": bool,
         "lowerIsStop": bool}):
    """Returns an iterator of values representing suspicious
    tokens in the given string.

    The result values are JSON-compatible dictionaries.
    The ƒpyflow{"pos"} key denotes the "Part Of Speach".

    Specifically (though this is subject to change),
    after tokenizing a string with spacy,
    getSuspiciousTokens discards any tokens determined to
    be uninteresting by pydrnlp.drtoken.tokenQuicklyFails.
    Of those remaining, it retains only those for which
    either the lemma or the lowercase form is in
    spacy.lang.en.stop_words.STOP_WORDS.
    The presence of one of those forms in STOP_WORDS
    suggests that the token should be excluded, ideally
    by identifying some characteristic that can be more 
    quickly checked.

    Note that getSuspiciousTokens prints progress messages to
    standard output.
    """
    print("Tokenizing")
    sys.stdout.flush()
    firstOne = True
    for token in nlp(str):
        if firstOne:
            firstOne = False
            sys.stdout.write("Analyzing")
            sys.stdout.flush()
        if not tokenQuicklyFails(token):
            thisLemma = token.lemma_
            thisLower = token.lower_
            lemmaIsStop = (thisLemma in STOP_WORDS)
            lowerIsStop = (thisLower in STOP_WORDS)
            if (lemmaIsStop or lowerIsStop):
                yield {"text": token.text,
                       "lemma": thisLemma,
                       "pos": token.pos_,
                       "lemmaIsStop": lemmaIsStop,
                       "lowerIsStop": lowerIsStop}


def readWriteSuspiciousTokens(input : IteratorOf(str),
                              destPath : str) -> None:
    """Calls getSuspiciousTokens on the input strings
    and writes the results to destPath as a JSON array.

    To avoid memory issues on large inputs,
    readWriteSuspiciousTokens reads the input and writes
    the output incrementally, without accumulating the
    whole result in memory.

    Note that readWriteSuspiciousTokens prints progress messages to
    standard output.
    """
    with open(destPath,'w') as dest:
        dest.write("[")
        print("Working")
        sys.stdout.flush()
        firstOne = True
        flushCounter = 0
        for js in getSuspiciousTokens(input):
            if firstOne:
                firstOne = False
            else:
                dest.write(",")
            json.dump(js,dest)
            sys.stdout.write(".")
            if flushCounter >= 20:
                sys.stdout.flush()
                flushCounter = 0
            else:
                flushCounter = 1 + flushCounter
        dest.write("]\n")
        print("\nDone")

                
if __name__ == "__main__":
    if (2 == len(sys.argv)):
        readWriteSuspiciousTokens(sys.stdin.read(), sys.argv[1])
    else:
        exit(1)

        
