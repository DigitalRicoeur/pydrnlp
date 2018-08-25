# -*- coding: utf-8 -*-
"""The drLanguage classs encapsulates tokenization and filtering tokens
for stop words across multiple languages.
This module provides the instances drEnglish and drFrench.
The function get_drLanguage selects the appropriate instance given
a lanugage string.

Models from SpaCy are loaded lazily.
"""

import spacy
from spacy.tokens import Token
from spacy.lang.en.stop_words import STOP_WORDS as _en_stop_words
from spacy.lang.fr.stop_words import STOP_WORDS as _fr_stop_words
from pydrnlp.drtoken import tokenShouldUseGivenStopWords


def drLanguageRevision() -> "RevisionJsexpr":
    """Returns a non-False JSON value identifying the current revision
    of the drLanguage class.
    """
    return 1


# for doc
Lemma = {"lemma": str, "text": str}


class drLanguage:
    """Encapsulates tokenization and filtering tokens
    for stop words across multiple languages.

    The public method is tokenize.
    """
    def __init__(self, nlpThunk, stop_words):
        self.__nlpForced = False
        self.__nlpThunk = nlpThunk
        self.__stop_words = stop_words
    # _forceNLP : self -> nlp
    # I wish I could force this in the background
    # while waiting for IO.
    def _forceNLP(self):
        forced = self.__nlpForced
        if forced:
            return forced
        else:
            nlp = self.__nlpThunk()
            self.__nlpForced = nlp
            return nlp
    # _doNLP : self str -> IteratorOf(Token)
    def _doNLP(self, s : str) -> "IteratorOf(Token)":
        nlp = self._forceNLP()
        return nlp(s)
    # tokenizedPassageLemmas : self IteratorOf(Token) -> IteratorOf(Lemma)
    def _tokenizedPassageToLemmas(self, doc : "IteratorOf(Token)") -> "IteratorOf(Lemma)":
        """Converts tokens to JSON-convertable Lemma dictionaries.

        Tokens which do not satisfy tokenShouldUseGivenStopWords()
        with this language's stop words are discarded.

        The purpose of the "text" field is to provide an example
        of an actual use of the word, as the lemma is always
        normalized, but some words (e.g. "DuFay") shouldn't be.
        (Also, some lemmas are strange, like "whatev".)
        """
        for token in doc:
            if tokenShouldUseGivenStopWords(token, self.__stop_words):
                yield {"lemma":token.lemma_ ,
                       "text":token.text}
    # tokenize : self str -> ListOf(Lemma)
    def tokenize(self, body : str) -> "ListOf(Lemma)":
        """Tokenizes a string into a list of JSON-convertable Lemma dictionaries.
        """
        doc = self._doNLP(body)
        return list(self._tokenizedPassageToLemmas(doc))


# Could a custom pipeline do less work faster?
# Also, is garbage an issue?
_disableThese = ['parser','ner']
_en_nlpThunk = lambda: spacy.load('en_core_web_sm', disable=_disableThese)
_fr_nlpThunk = lambda: spacy.load('fr_core_news_sm', disable=_disableThese)


drEnglish = drLanguage(_en_nlpThunk, _en_stop_words)

drFrench = drLanguage(_fr_nlpThunk, _fr_stop_words)


# get_drLanguage : str -> drLanguage
def get_drLanguage(s : str) -> drLanguage:
    """Returns the drLanguage instance specified by the
    given string.
    """
    if "en" == s:
        return drEnglish
    elif "fr" == s:
        return drFrench
    # else do some useful error
