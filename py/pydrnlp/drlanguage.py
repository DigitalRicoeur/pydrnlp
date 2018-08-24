# -*- coding: utf-8 -*-
"""The drLanguage classs encapsulates tokenization and filtering tokens
for stop words across multiple languages.
This module provides the instances drEnglish and drFrench.
The function get_drLanguage selects the appropriate instance given
a lanugage string.
"""

import spacy
from spacy.tokens import Token
from spacy.lang.en.stop_words import STOP_WORDS as _en_stop_words
from spacy.lang.fr.stop_words import STOP_WORDS as _fr_stop_words
from pydrnlp.drtoken import tokenShouldUseWithStopWords


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
    def __init__(self, nlp, stop_words):
        self.__nlp = nlp
        self.__stop_words = stop_words
    # _tokenShouldUse : self Token -> bool
    def _tokenShouldUse(self, token : Token) -> bool:
        return tokenShouldUseWithStopWords(token, self.__stop_words)
    # tokenizedPassageLemmas : self IteratorOf(Token) -> IteratorOf(Lemma)
    def _tokenizedPassageToLemmas(self, doc : "IteratorOf(Token)") -> "IteratorOf(Lemma)":
        """Converts tokens to JSON-convertable Lemma dictionaries.

        Tokens which do not satisfy tokenShouldUse are discarded.

        The purpose of the "text" field is to provide an example
        of an actual use of the word, as the lemma is always
        normalized, but some words (e.g. "DuFay") shouldn't be.
        (Also, some lemmas are strange, like "whatev".)

        Currently, choosing the best example string is delegated to
        Racket, which avoids logic duplication and lets the Python
        layer return faster.
        The downside is the transmitted JSON message may be longer than
        it would otherwise have to be.
        """
        for token in doc:
            if self._tokenShouldUse(token):
                yield {"lemma":token.lemma_ ,
                       "text":token.text}
    # tokenize : self str -> ListOf(Lemma)
    def tokenize(self, body : str) -> "ListOf(Lemma)":
        """Tokenizes a string into a list of JSON-convertable Lemma dictionaries.
        """
        doc = self.__nlp(body)
        return list(self._tokenizedPassageToLemmas(doc))

        
# Could a custom pipeline do less work faster?
# Also, is garbage an issue?
_en_nlp = spacy.load('en_core_web_sm', disable=['parser','ner'])
_fr_nlp = spacy.load('fr_core_news_sm', disable=['parser','ner'])


drEnglish = drLanguage(_en_nlp, _en_stop_words)

drFrench = drLanguage(_fr_nlp, _fr_stop_words)


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
