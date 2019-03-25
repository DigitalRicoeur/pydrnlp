# -*- coding: utf-8 -*-
"""Utilities for working with spaCy "stop words."
"""

def revision():
    """Identifies the revision of `pydrnlp.tokenizer.stop_words`."""
    return 0

def tokenAnyIsStopForLanguage(token, nlp):
    """Returns `True` if any form of `token` is a stop word
    for the `spacy.language.Language` instance `nlp`.

    This is needed because `spacy.tokens.Token.is_stop`
    returns `False` when the token itself is not a stop word,
    even if a more normalized form might be.

    Since spaCy v2.1.0, `spacy.tokens.Token.is_stop`
    should be properly case-insensitive, so the primary job of this
    function is now to also check `token.lemma_ in nlp.Defaults.stop_words`.
    Comments on an old version of this function claim that test will
    sometimes pass even when `token.is_stop is False`,
    though we should collect examples of when/why, because I've forgotten ...

    As a temporary measure while upgrading spaCy v2.0.0 to v2.1.0,
    this function currently also does the case-insensitivity
    checks that should no longer be needed,
    so that we can confirm that they are never necessary.

    TODO: if this function is as trivial as it seems given spaCy v2.1.0,
    unless some other tool is likely to reuse this logic,
    it might be better to inline this into `pydrnlp.trends`.
    """
    if token.is_stop:
        return True
    elif (token.lemma_ in nlp.Defaults.stop_words):
        _cache_is_stop_by_lemma[nlp.lang].add(token)
        return True
    elif ((token.lower_ != token.lemma_)
          and (token.lower_ in nlp.Defaults.stop_words)):
        # shouldn't get here since spaCy v2.1.0
        # thanks to https://github.com/explosion/spaCy/pull/1891
        # ---but let's make sure
        _cache_is_stop_by_explicit_lower[nlp.lang].add(token)
        return True
    else:
        return False

    
class _LanguageCaches():
    def __init__(self, mkEmpty = set):
        self._cache = {}
        self._mkEmpty = mkEmpty
    def __getitem__(self, key):
        found = self._cache.get(lang, False)
        if found:
            return found
        else:
            ret = self._mkEmpty()
            self._cache[lang] = ret
            return ret

_cache_is_stop_by_lemma = _LanguageCaches()
_cache_is_stop_by_explicit_lower = _LanguageCaches()

