# -*- coding: utf-8 -*-
"""Functions for filtering out uninteresting tokens.
"""

from spacy.lang.en.stop_words import STOP_WORDS as STOP_WORDS
#from spacy.lang.fr.stop_words import STOP_WORDS as fr_STOP_WORDS
from spacy.tokens import Token


def tokenFilterRevision() -> "RevisionJsexpr":
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


# tokenShouldUseWithStopWords : Token STOP_WORDS -> bool
def tokenShouldUseWithStopWords(token : Token, stop_words : "STOP_WORDS") -> bool:
    """Recognizes tokens which should be included in counting
    with respect to the given STOP_WORDS.

    In addition to returning False for any token for which
    tokenQuicklyFails would return True,
    also checks for the lemma and lowercase forms of the token
    in STOP_WORDS, which might come from spacy.lang.en.stop_words
    or spacy.lang.fr.stop_words.

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
                (token.lemma_ in stop_words) or
                (token.lower_ in stop_words))

# tokenShouldUse : Token -> bool
def tokenShouldUse(token : Token) -> bool:
    """Like tokenShouldUseWithStopWords, but allways uses
    STOP_WORDS from spacy.lang.en.stop_words.
    """
    return tokenShouldUseWithStopWords(token, STOP_WORDS)
