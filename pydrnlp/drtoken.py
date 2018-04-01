# -*- coding: utf-8 -*-

from spacy.lang.en.stop_words import STOP_WORDS


# interestingPOS : string -> bool
# Recognizes the interesting potential values from token.pos_
# Notable exclusions: "NUM"->numeral, "SYM"=>symbol
def interestingPOS(str):
    return ((str == "ADJ") or # adjective
            (str == "ADV") or # adverb
            (str == "INTJ") or # interjection
            (str == "NOUN") or # noun
            (str == "PROPN") or # proper noun
            (str == "VERB") or # verb
            (str == "X")) # other


# boringPOS : string -> bool
# The opposite of interestingPOS
def boringPOS(str):
    return not interestingPOS(str)


# tokenQuicklyFails : token -> bool
def tokenQuicklyFails(token):
    return (token.is_stop or
            token.is_punct or
            token.is_space or
            (token.lemma_ == "-PRON-") or # sometimes classed as "ADJ"
            boringPOS(token.pos_))


##########################################################
# tokenShouldUse : token -> boolean
##########################################################
# Rejects tokens which are stop words, punctuation, space,
# or which have an uninteresting part of speach tag.
# Checks for the lemma and lowercase forms of the token
# in STOP_WORDS because token.is_stop seems to be False
# when the token itself is not a stop word, even if
# a more normalized form might be.
# Maybe there is opportunity for optimization here?
# In most cases, both token.lemma_ and token.lower_
# are in STOP_WORDS if either of them is.
# It seems to be very rare for only token.lower_
# to be in STOP_WORDS. Investigate further.
def tokenShouldUse(token):
    return not (tokenQuicklyFails(token) or
                (token.lemma_ in STOP_WORDS) or
                (token.lower_ in STOP_WORDS))
