# -*- coding: utf-8 -*-
"""Functions for filtering out uninteresting tokens.
"""

import pydrnlp.stop_words as stop_words
from pydrnlp.stop_words import tokenAnyIsStopForLanguage


def revision():
    """Identifies the revision of this module.

    Incorporates `pydrnlp.stop_words.revision`.
    """
    thisModuleRevision = 0
    return [thisModuleRevision, stop_words.revision()]


# posIsInteresting : str -> bool
def posIsInteresting(str):
    """Recognizes the interesting potential values from
    `spacy.tokens.Token.pos_`.

    Notable exclusions: "NUM"->numeral, "SYM"=>symbol

    "pos" stands for "part of speech"
    """
    return ((str == "ADJ") or # adjective
            (str == "ADV") or # adverb
            (str == "INTJ") or # interjection
            (str == "NOUN") or # noun
            (str == "PROPN") or # proper noun
            (str == "VERB") or # verb
            (str == "X")) # other


# posIsBoring : str -> bool
def posIsBoring(str):
    """The opposite of `pydrnlp.tokenizer.usetoken.posIsInteresting`."""
    return not posIsInteresting(str)


# tokenQuicklyFails : Token -> bool
def tokenQuicklyFails(token):
    """Predicate recognizing tokens which obviously should NOT be counted.

    Specifically, recognizes tokens which are immeduate stop words,
    punctuation, or whitespace, or which have an uninteresting
    part of speech tag according to `pydrnlp.tokenizer.usetoken.posIsBoring`.

    Used to implement `pydrnlp.tokenizer.usetoken.tokenShouldUseForLang`.
    """
    return (token.is_stop or
            token.is_punct or
            token.is_space or
            (token.lemma_ == "-PRON-") or # sometimes classed as "ADJ"
            posIsBoring(token.pos_))


# tokenShouldUseForLang : Token Language -> bool
def tokenShouldUseForLang(token, lang):
    """Recognizes tokens which should be included in counting
    with respect to the given `spacy.language.Language` instance.

    Currently, it checks that neither
    `pydrnlp.tokenizer.usetoken.tokenQuicklyFails`
    nor `pydrnlp.stop_words.tokenAnyIsStopForLanguage`
    return `True` for the given token.
    """
    return not (tokenQuicklyFails(token) or
                tokenAnyIsStopForLanguage(token, lang))
