# -*- coding: utf-8 -*-
"""Provides the function `pydrnlp.language.getLanguage`.

TODO: en_core_web_lg sounds like it has pre-trained models and may give
better accuracy. OTOH it is an order of magnitude bigger (667 vs 36 MB).
"""

import spacy

def revision():
    return 0

class _Promise:
    def __init__(self, thunk):
        self.__thunkUnlessForced = thunk
        self.__rslt = None
    def force(self):
        thunkUnlessForced = self.__thunkUnlessForced
        if thunkUnlessForced:
            rslt = thunkUnlessForced()
            self.__rslt = rslt
            self.__thunkUnlessForced = False
            return rslt
        else:
            return self.__rslt


_all_pr_languages = {langStr: _Promise(lambda: spacy.load(shortcut))
                     for langStr, shortcut in
                     {"en": "en_core_web_sm",
                      "fr": "fr_core_news_sm",
                      "de": "de_core_news_sm",
                     }.items()}

    
# getLanguage : str -> spacy.language.Language
def getLanguage(langStr):
    """Returns a `spacy.language.Language` instance for the given IANA string.

    Models from SpaCy are loaded lazily.
    """
    return _all_pr_languages[langStr].force()
    # else some useful error

