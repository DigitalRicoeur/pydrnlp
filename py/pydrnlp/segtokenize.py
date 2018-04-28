# -*- coding: utf-8 -*-

import spacy
from spacy.lang.en.stop_words import STOP_WORDS
from pydrnlp.drtoken import tokenShouldUse
from pydrnlp.contract import ModuleContract, ListOf, IteratorOf, Predicate
from spacy.tokens import Token


# Could a custom pipeline do less work faster?
# Also, is garbage an issue?
nlp = spacy.load('en', disable=['parser','ner'])

Contract = ModuleContract(__name__)

@Predicate
def Natural(any) -> bool:
    """Recognizes integers where ƒpyflow{any >= 0}"""
    return ((isinstance(any,int)) and
            (any >= 0))


# revision : -> natural
def revision() -> Natural:
    """Returns a natural number identifying the current revision

    The intended purpose is for clients to
    be able to cache responses: 
    As long as revision() returns the same number, 
    calling this API on the same input should return equivalent output.
    This enables clients to cache responses.

    When the result of revision() changes, any cache is stale.

    For implementors: The number returned by revision()
    should be incremented if their is any change to the
    behavior *either* of this library *or* of its dependencies.
    """
    return 0


@Predicate
def PositiveInt(any) -> bool:
    """Recognizes integers where ƒpyflow{any > 0}"""
    return ((isinstance(any,int)) and
            (any > 0))

# for doc
LemmaWithCount = Contract("LemmaWithCount",
                          {"lemma": str,
                           "text": str,
                           "count": PositiveInt})
IterLemmaCount = IteratorOf(LemmaWithCount)


# tokensFilterUniqueLemma :
#   IteratorOf(Token) -> IteratorOf(LemmaWithCount)
def tokensFilterUniqueLemma(seq : IteratorOf(Token)) -> IterLemmaCount:
    """Counts the number of occurences of each lemma.

    Only tokens in the argument sequence which satisfy
    tokenShouldUse are considered.

    The resulting iterator has one LemmaWithCount dictionary 
    for each unique significant lemma.

    The purpose of the "text" field is to provide an exemplar
    of an actual use of the word, as the lemma is always
    normalized, but some words (e.g. "DuFay") shouldn't be.
    (Also, some lemmas are strange, like "whatev".)

    More thought should be given to the right way to chose
    an exemplar, which is an issue both here and in client code.
    """
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

# for doc
TokenizedSegment = Contract("TokenizedSegment",
                            {"counter": Natural,
                             "tokenized": ListOf(LemmaWithCount)})
InputSegment = Contract("InputSegment",
                        {"counter": Natural,
                         "body": str})

# tokenizeSegment :
def tokenizeSegment(sgmnt : InputSegment) -> TokenizedSegment:
    """Processes the JSON value representing a segment.

    Uses the spacy Language object to process the body,
    calls tokensFilterUniqueLemma to obtain LemmaWithCount dictionaries,
    and wraps the results into a JSON value.
    """
    doc = nlp(sgmnt["body"])
    tokenized = list(tokensFilterUniqueLemma(doc))
    return {"counter":sgmnt["counter"],
            "tokenized":tokenized}


# for doc
InputDoc = Contract("InputDoc",
                    {"key": str,
                     "segments": ListOf(InputSegment)})
TokenizedDoc = Contract("TokenizedDoc",
                        {"key": str,
                         "segments": ListOf(TokenizedSegment)})



# tokenizeDoc : InputDoc -> TokenizedDoc
def tokenizeDoc(jsDocIn : InputDoc) -> TokenizedDoc:
    """Tokenizes one document.

    Uses tokenizeSegment to process each segment JSON value
    in the given document JSON value.
    """
    return {"key":jsDocIn["key"],
            "segments":list(map(tokenizeSegment,
                                jsDocIn["segments"]))}


# tokenizeDocList : ListOf(InputDoc) -> ListOf(TokenizedDoc)
def tokenizeDocList(jsIn : ListOf(InputDoc)) -> ListOf(TokenizedDoc):
    """Simply maps tokenizeDoc over the list of JSON values.
    """
    return list(map(tokenizeDoc,jsIn))

   
