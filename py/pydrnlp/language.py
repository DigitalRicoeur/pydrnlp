#lang pydrnlp/support/python-lang # -*- coding: utf-8 -*-
"""Uniform, lazy loading of SpaCy language models

The primary entry point for Spacy functionality
is through a `spacy.language.Language` object,
which must be loaded through a language-specific
model package.
Loading a language is expensive and should not be repeated.
Additionally, pydrnlp should be change which model we
use for each supported language without necessitating changes
in every Python module that needs to use `spacy.language.Language` objects.

The functionality is supported through `pydrnlp.language.get()`,
which is similar in spirit to `spacy.load()`.

**TODO:** en_core_web_lg sounds like it has pre-trained models and may give
better accuracy. OTOH it is an order of magnitude bigger (667 vs 36 MB).
"""

import en_core_web_md
import fr_core_news_sm
import de_core_news_sm

_model_modules = {"en": en_core_web_md,
                  "fr": fr_core_news_sm,
                  "de": de_core_news_sm}

def revision():
    return 0

class _Promise:
    def __init__(self, mod):
        self.__mod = mod
        self.__rslt = False
    def force(self):
        ret = self.__rslt
        if ret:
            return ret
        else:
            ret = self.__mod.load()
            self.__rslt = ret
            return ret

_all_promised_languages = {langStr: _Promise(mod)
                           for langStr, mod in _model_modules.items()}

# get : str -> spacy.language.Language
def get(langStr):
    """Returns a `spacy.language.Language` instance for the given IANA string.

    Models from SpaCy are loaded lazily.
    """
    return _all_promised_languages[langStr].force()
    # else some useful error
