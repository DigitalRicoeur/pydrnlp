# -*- coding: utf-8 -*-
"""Provides `pydrnlp.stop_words.tokenAnyIsStopForLanguage`.
"""

def revision():
    """Identifies the revision of `pydrnlp.tokenizer.stop_words`."""
    return 0


_cache = {}


def _get_cache_for_lang(lang):
    found = _cache.get(lang, False)
    if found:
        return found
    else:
        ret = {}
        _cache[lang] = ret
        return ret


def _do_token_in_stop_words(token, stop_words):
    """The actual logic for the slow path."""
    # Maybe there is opportunity for optimization here?
    # In most cases, both token.lemma_ and token.lower_
    # are in STOP_WORDS if either of them is.
    # It seems to be very rare for only token.lower_
    # to be in STOP_WORDS. Investigate further.
    return ((token.lemma_ in stop_words) or
            # TODO revisit this after spaCy v2.1.0
            ((token.lower_ != token.lemma_)
             and (token.lower_ in stop_words)))


def tokenAnyIsStopForLanguage(token, nlp):
    """Returns `True` if any form of `token` is a stop word
    for the `spacy.language.Language` instance `nlp`.

    This is needed because `spacy.tokens.Token.is_stop`
    returns `False` when the token itself is not a stop word, even if
    a more normalized form might be.
    Apparently this is by design: see
    https://stackoverflow.com/q/47523112/2523780.
    Therefore, this function also checks the lemma and lowercase forms
    of the token. It tries to do so efficiently.

    N.B. spaCy v2.1.0 will include a patch to make stop words
    case-insensitive: https://github.com/explosion/spaCy/pull/1891
    When it is released, we can simplify this implementation.
    """
    if token.is_stop:
        return True
    else:
        txt = token.text
        langCache = _get_cache_for_lang(nlp.lang)
        found = langCache.get(txt, None)
        if (found is None):
            ret = _do_token_in_stop_words(token, nlp.Defaults.stop_words)
            langCache[txt] = ret
            return ret
        else:
            return found
