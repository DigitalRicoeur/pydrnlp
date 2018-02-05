#!/usr/bin/env python3
# coding: utf8

import spacy
from spacy.lang.en.stop_words import STOP_WORDS
import json

nlp = spacy.load('en')

def tokenShouldUse(token):
    return not (token.is_stop or
                token.is_punct or
                token.is_space or
                token.pos_ == "PART" or
                (token.lower_ in STOP_WORDS) or
                (token.lemma_ in STOP_WORDS))

def tokensFilterUniqueLemma(seq):
    lemmaDict = {}
    tkns = []
    for token in seq:
        if tokenShouldUse(token):
            if token.lemma in lemmaDict:
                lemmaDict[token.lemma] = 1 + lemmaDict[token.lemma]
            else:
                lemmaDict[token.lemma] = 1
                tkns.append(token)
    for token in tkns:
        yield (token.lemma_ , token.text , lemmaDict[token.lemma])

text = """But Google is starting from behind. The company made a late push
into hardware, and Apple’s Siri, available on iPhones, and Amazon’s Alexa
software, which runs on its Echo and Dot devices, have clear leads in
consumer adoption. Adoption is a word meaning adoption."""

doc = nlp(text)

print(json.dumps(list({"lemma":lemma,"text":text,"count":count}
                      for lemma, text, count in tokensFilterUniqueLemma(doc))))
   
