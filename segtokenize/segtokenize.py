# coding: utf8

import spacy
from spacy.lang.en.stop_words import STOP_WORDS

def revision():
    return 0

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

def tokenizeSegment(sgmnt):
    doc = nlp(sgmnt["body"])
    tokenized = list({"lemma":lemma,"text":text,"count":count}
                     for lemma, text, count in tokensFilterUniqueLemma(doc))
    return {"counter":sgmnt["counter"],
            "tokenized":tokenized}
        
def handleInput(jsIn):
    return {"key":jsIn["key"],
            "segments":list(map(tokenizeSegment,jsIn["segments"]))}

def handle(jsIn):
    return list(map(handleInput,jsIn))

   
