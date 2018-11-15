# -*- coding: utf-8 -*-
"""Core functionality for tokenizing segmented documents.

Would it be useful for JsToken to track the token's
  a) start and end positions and/or
  b) index among significant tokens in the document?


Types used in this module:
--------------------------

JsToken:
    {"lemma": str, "text": str}
JsOutSegment:
    {"key": JsExpr,
     "tokenized": Listof[JsToken]}
JsOut:
    Listof[JsOutSegment]


JsInSegment:
    {"key": JsExpr, "body": str}
JsIn:
    {LangStr: Listof[JsInSegment]}
"""

from pydrnlp.tokenizer.usetoken import tokenShouldUseForLang
from pydrnlp.tokenizer.usetoken import revision as usetoken_revision
from pydrnlp.language import getLanguage, revision as language_revision

# revision : -> RevisionJsexpr
def revision():
    """Returns a non-False JSON value identifying the current revision.

    The intended purpose is for clients to
    be able to cache responses:
    As long as tokenizerRevision() returns the same value,
    calling this API on the same input should return equivalent output.
    This enables clients to cache responses.
    The returned value incorporates the results of
    pydrnlp.drtoken.tokenFilterRevision() and
    pydrnlp.drlanguage.drLanguageRevision().

    When the result of tokenizerRevision() changes, any cache is stale.
    """
    thisModuleRevision = 2
    return [thisModuleRevision, usetoken_revision(), language_revision()]


def _languageDocToListofJsToken(nlp, doc):
    """Returns an iterator of JSON-convertable token dictionaries.
    
    Tokens which do not satisfy 
    `pydrnlp.tokenizer.usetoken.tokenShouldUseForLang`
    with this language are discarded.

    The purpose of the "text" field is to provide an example
    of an actual use of the word, as the lemma is always
    normalized, but some words (e.g. "DuFay") shouldn't be.
    (Also, some lemmas are strange, like "whatev".)
    """
    for token in doc:
        if tokenShouldUseForLang(token, nlp):
            yield {"lemma":token.lemma_ ,
                   "text":token.text}

            
def _languageTokenizeSegments(nlp, segs):
    """ """
    args = [(jsInSeg["body"], jsInSeg["key"]) for jsInSeg in segs]
    for (doc, key) in nlp.pipe(args, as_tuples = True, n_threads = -1):
        yield {"key": key,
               "segments": list(_languageDocToListofJsToken(nlp, doc))}


def tokenize(jsIn):
    """ """
    ret = []
    for langStr, segs in jsIn.items():
        nlp = getLanguage(langStr)
        ret.extend(_languageTokenizeSegments(nlp, segs))
    return ret
