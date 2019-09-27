#lang pydrnlp/support/python-lang # -*- coding: utf-8 -*-
"""Core engine for the "Trends" tool.
"""

import pydrnlp.language
import pydrnlp.jsonio
import regex

# Python revision function
def revision():
    this_module_revision = 8
    return [this_module_revision,
            pydrnlp.language.revision()]



def analyze_all(jsexpr):
    """Tokenizes jsexpr. **TODO: document this.**

    Tokens which do not satisfy `should_use_token`
    with this language are discarded.

    The purpose of the "text" field is to provide an example
    of an actual use of the word, as the lemma is always
    normalized, but some words (e.g. "DuFay") shouldn't be.
    (Also, some lemmas are strange, like "whatev".)
    """
    for lang_str, segs in jsexpr.items():
        nlp = pydrnlp.language.get(lang_str)
        for doc in nlp.pipe(segs, as_tuples = False):
            yield [(t.lemma_, t.text)
                   for t in doc if should_use_token(t, lang = nlp)]



# should_use_token(Token, lang = Language) -> bool
def should_use_token(token, *, lang):
    """Recognizes tokens which should be included in counting
    with respect to the given `spacy.language.Language` instance.

    Some kinds of tokens which should be excluded:

    - punctuation;
    - whitespace;
    - stop words (see `pydrnlp.tokenAnyIsStopForLanguage`); and
    - tokens which have a "boring" part-of-speech tag.

    Part-of-speech tags that are considered "boring"
    notably include `"NUM"` (numeral) and `"SYM"` (symbol).
    """
    return not (token.is_punct
                or token.is_space
                or token.is_stop # token.lemma_ checked below
                # "-PRON-" sometimes classed as "ADJ"
                or (token.lemma_ == "-PRON-")
                or (token.pos_ not in _interesting_pos_strings)
                # token.is_stop checked above
                or (token.lemma_ in lang.Defaults.stop_words)
                or (not _check_enough_alphabetic_chars(token.lemma_)))


_min_alphabetic_chars = 3 # must be positive
_char_alphabetic_regex = regex.compile("\\p{Alphabetic=Yes}")

# _check_enough_alphabetic_chars : str -> bool
# Returns True IFF its arg has at least _min_alphabetic_chars
# characters that match _char_alphabetic_regex.
def _check_enough_alphabetic_chars(lemma):
    if len(lemma) < _min_alphabetic_chars:
        return False
    else:
        count = 0
        for mtch in _char_alphabetic_regex.finditer(lemma):
            count = 1 + count
            if count == _min_alphabetic_chars:
                return True
        return False


_interesting_pos_strings = frozenset({
    "NOUN",
    "PROPN"
})


if __name__ == "__main__":
    pydrnlp.jsonio.start_loop(analyze_all,
                              description = 'engine for "Trends" tool')
