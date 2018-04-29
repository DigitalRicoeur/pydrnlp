# -*- coding: utf-8 -*-
"""Functions for filtering out uninteresting tokens.
"""

from spacy.lang.en.stop_words import STOP_WORDS
from spacy.tokens import Token
from pydrnlp.mkdoc import JSON
from pydrnlp.annotations import ModuleAnnotationNamer, Or, And, Not


NamedAnnotation = ModuleAnnotationNamer(__name__)
RevisionJsexpr = NamedAnnotation(
    "RevisionJsexpr",
    And(JSON, Not(False)))


def tokenFilterRevision() -> RevisionJsexpr:
    return 0

    
# interestingPOS : str -> bool
def interestingPOS(str : str) -> bool:
    """Recognizes the interesting potential values from token.pos_

     Notable exclusions: "NUM"->numeral, "SYM"=>symbol

    "pos"/"POS" stands for "Part of speach"
    """
    return ((str == "ADJ") or # adjective
            (str == "ADV") or # adverb
            (str == "INTJ") or # interjection
            (str == "NOUN") or # noun
            (str == "PROPN") or # proper noun
            (str == "VERB") or # verb
            (str == "X")) # other


# boringPOS : str -> bool
def boringPOS(str : str) -> bool:
    """The opposite of interestingPOS
    """
    return not interestingPOS(str)


# tokenQuicklyFails : Token -> bool
def tokenQuicklyFails(token : Token) -> bool:
    """Predicate recognizing tokens which obviously should NOT be counted.

    Specifically, recognizes tokens which are stop words, punctuation, 
    or space, or which have an uninteresting part of speach tag
    according to boringPOS.

    Used to implement tokenShouldUse.
    """
    return (token.is_stop or
            token.is_punct or
            token.is_space or
            (token.lemma_ == "-PRON-") or # sometimes classed as "ADJ"
            boringPOS(token.pos_))


# tokenShouldUse : Token -> bool
def tokenShouldUse(token : Token) -> bool:
    """Predicate recognizing tokens which should be included in counting.

    In addition to returning False for any token for which
    tokenQuicklyFails would return True,
    also checks for the lemma and lowercase forms of the token
    in STOP_WORDSspacy.lang.en.stop_words. 
    This is needed because token.is_stop seems to be False
    when the token itself is not a stop word, even if
    a more normalized form might be.

    Maybe there is opportunity for optimization here?
    In most cases, both token.lemma_ and token.lower_
    are in STOP_WORDS if either of them is.
    It seems to be very rare for only token.lower_
    to be in STOP_WORDS. Investigate further.
    """
    return not (tokenQuicklyFails(token) or
                (token.lemma_ in STOP_WORDS) or
                (token.lower_ in STOP_WORDS))
