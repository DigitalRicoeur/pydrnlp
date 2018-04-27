# -*- coding: utf-8 -*-

import spacy
from spacy.lang.en.stop_words import STOP_WORDS
from pydrnlp.drtoken import tokenShouldUse


# Could a custom pipeline do less work faster?
# Also, is garbage an issue?
nlp = spacy.load('en', disable=['parser','ner'])


##########################################################
# revision : -> natural
##########################################################
# Returns a natural number identifying the current revision 
# of this module. The intended purpose is for clients to
# be able to cache responses: as long as revision()
# returns the same number, calling this API on the same
# input should return equivalent output.
# When the result of revision() changes, any cache is stale.
def revision():
    return 0


##########################################################
# a LemmaWithCount is a 
#    {"lemma":string,
#     "text":string,
#     "count":positive-integer}
##########################################################


##########################################################
# tokensFilterUniqueLemma :
#   (sequenceof token) -> (iteratorof LemmaWithCount)
##########################################################
# Considering only tokens in the argument sequence which
# satisfy tokenShouldUse, counts the number of occurences
# of each lemma.
# The resulting iterator has one LemmaWithCount dictionary 
# for each unique significant lemma.
# The purpose of the "text" field is to provide an exemplar
# of an actual use of the word, as the lemma is always
# normalized, but some words (e.g. "DuFay") shouldn't be.
# (Also, some lemmas are strange, like "whatev".)
# More thought should be given to the right way to chose
# an exemplar, which is an issue both here and in client code.
def tokensFilterUniqueLemma(seq):
    # countDict maps ints (the hash code of a lemma)
    #   to positive-ints (the count of its occurrences)
    # tkns accumulates one token object per lemma
    countDict = {}
    tkns = []
    # Loop over seq, mutating countDict and tkns
    for token in seq:
        if tokenShouldUse(token):
            thisLemmaK = token.lemma
            thisCount = countDict.get(thisLemmaK,0)
            countDict[thisLemmaK] = 1 + thisCount
            if (0 == thisCount):
                tkns.append(token)
    # Loop over tkns, yielding a LemmaWithCount for each
    for token in tkns:
        yield {"lemma":token.lemma_ ,
               "text":token.text ,
               "count":countDict[token.lemma]}


##########################################################        
# a TokenizedSegment is a
#    {"counter":json,
#     "tokenized":(listof LemmaWithCount)} 
##########################################################


##########################################################
# tokenizeSegment :
#   {"counter":json,"body":string} -> TokenizedSegment
##########################################################
# Given the JSON value representing a segment, 
# uses the spacy Language object to process the body,
# calls tokensFilterUniqueLemma to obtain
# LemmaWithCount dictionaries, and wraps the results into
# a JSON value.
def tokenizeSegment(sgmnt):
    doc = nlp(sgmnt["body"])
    tokenized = list(tokensFilterUniqueLemma(doc))
    return {"counter":sgmnt["counter"],
            "tokenized":tokenized}


##########################################################
# an InputDoc is a
#    {"key":json,
#     "segments":(listof {"counter":json,"body":string})}
#
# a TokenizedDoc is a
#    {"key":json,
#     "segments":(listof TokenizedSegment)}
##########################################################


##########################################################
# handleInputDoc : InputDoc -> TokenizedDoc
##########################################################
# Uses tokenizeSegment to process each segment JSON value
# in the given document JSON value.
def handleInputDoc(jsDocIn):
    return {"key":jsDocIn["key"],
            "segments":list(map(tokenizeSegment,
                                jsDocIn["segments"]))}


##########################################################
# handle : (listof InputDoc) -> (listof TokenizedDoc)
##########################################################
# Simply maps handleInputDoc over the list of JSON values.
def handle(jsIn):
    return list(map(handleInputDoc,jsIn))

   
