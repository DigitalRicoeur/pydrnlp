# -*- coding: utf-8 -*-

import spacy
from spacy.lang.en.stop_words import STOP_WORDS
import json
import sys
from pydrnlp.drtoken import tokenQuicklyFails


nlp = spacy.load('en', disable=['parser','ner'])


def getSuspiciousTokens(str):
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
                yield {"text":token.text,
                       "lemma":thisLemma,
                       "pos":token.pos_,
                       "lemmaIsStop":lemmaIsStop,
                       "lowerIsStop":lowerIsStop}


if __name__ == "__main__":
    destPath = sys.argv[1]
    with open(destPath,'w') as dest:
        dest.write("[")
        print("Working")
        sys.stdout.flush()
        firstOne = True
        flushCounter = 0
        for js in getSuspiciousTokens(sys.stdin.read()):
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
