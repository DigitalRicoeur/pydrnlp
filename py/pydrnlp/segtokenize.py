# -*- coding: utf-8 -*-
"""Core functionality for tokenizing segmented documents.
"""

from spacy.tokens import Token
from pydrnlp.drtoken import tokenFilterRevision
from pydrnlp.drlanguage import drLanguageRevision, Lemma, get_drLanguage


# tokenizerRevision : -> RevisionJsexpr
def tokenizerRevision() -> "RevisionJsexpr":
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
    thisModuleRevision = 1
    return [thisModuleRevision , drLanguageRevision(), tokenFilterRevision()]


# for doc
TokenizedSegment = {"key": "JSON",
                    "tokenized": "ListOf(Lemma)"}
InputSegment = {"key": "JSON",
                "lang": str,
                "body": str}

# tokenizeSegment :
def tokenizeSegment(sgmnt : InputSegment) -> TokenizedSegment:
    """Processes the JSON value representing a segment.

    Uses get_drLanguage to get the drLanguage instance specified
    by the "lang" field of the input,
    then calls its tokenize method to obtain Lemma dictionaries,
    and wraps the results into a JSON value.
    """
    lang = get_drLanguage(sgmnt["lang"])
    tokenized = lang.tokenize(sgmnt["body"])
    return {"key":sgmnt["key"],
            "tokenized":tokenized}


# tokenizeSegmentList : ListOf(InputSegment) -> ListOf(TokenizedSegment)
def tokenizeSegmentList(jsIn : "ListOf(InputSegment)") -> "ListOf(TokenizedSegment)":
    """Simply maps tokenizeSegment over the list of JSON values.
    """
    return list(map(tokenizeSegment,jsIn))
