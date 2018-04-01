# -*- coding: utf-8 -*-

import spacy
from spacy.lang.en.stop_words import STOP_WORDS
from sys import stdout

nlp = spacy.load('en')

def tokenizeStopWords():
    for w in STOP_WORDS:
        for token in nlp(w):
            yield (token.text,token.lemma_)

firstTime = True
for text,lemma in tokenizeStopWords():
    if firstTime:
        firstTime = False
        stdout.write("(")
    else:
        stdout.write("\n ")
    stdout.write("(\"")
    stdout.write(lemma)
    stdout.write("\" . \"")
    stdout.write(text)
    stdout.write("\")")
stdout.write(")\n")
