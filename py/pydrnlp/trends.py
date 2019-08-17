# -*- coding: utf-8 -*-
"""Core engine for the "Trends" tool.

Types used in this module:
--------------------------

- `JsToken`:
    `{"lemma": str, "text": str}`

- `JsOutSegment`:
    `{"key": JsExpr, "tokenized": Listof[JsToken]}`

- `JsOut`:
    `Listof[JsOutSegment]`


- `JsInSegment`:
    `{"key": JsExpr, "body": str}`

- `JsIn`:
    `{LangStr: Listof[JsInSegment]}`

Would it be useful for JsToken to track the token's

  a) start and end positions and/or

  b) index among significant tokens in the document?

Maybe preserving the ordering within the segment is good enough.
"""

import pydrnlp.language
import regex

# revision : -> RevisionJsexpr
def revision():
    """Returns a non-False JSON value identifying the current revision.

    The intended purpose is for clients to
    be able to cache responses:
    As long as tokenizerRevision() returns the same value,
    calling this API on the same input should return equivalent output.
    This enables clients to cache responses.
    The returned value incorporates the results of
    pydrnlp.drtoken.tokenFilterRevision() and
    pydrnlp.drlanguage.drLanguageRevision().

    When the result of tokenizerRevision() changes, any cache is stale.
    """
    thisModuleRevision = 5
    return [thisModuleRevision,
            pydrnlp.language.revision()]




def analyze_all(jsIn, verbose=False):
    """Tokenizes JsIn. **TODO: document this.**"""
    ret = []
    for langStr, segs in jsIn.items():
        nlp = pydrnlp.language.get(langStr)
        ret.extend(_analyze_language_segments(nlp, segs, verbose=verbose))
    return ret


def _analyze_language_segments(nlp, segs, verbose=False):
    """Tokenizes a Listof[JsInSegment] segs using the language nlp."""
    args = ((jsInSeg["body"], jsInSeg["key"]) for jsInSeg in segs)
    for (doc, key) in nlp.pipe(args, as_tuples = True):
        yield {"key": key,
               "tokenized": list(_analyze_spacy_doc(nlp,
                                                    doc,
                                                    verbose=verbose))}

        
def _analyze_spacy_doc(nlp, doc, verbose=False):
    """Returns an iterator of JSON-convertable token dictionaries.
    
    Tokens which do not satisfy 
    `pydrnlp.tokenizer.usetoken.tokenShouldUseForLang`
    with this language are discarded.

    The purpose of the "text" field is to provide an example
    of an actual use of the word, as the lemma is always
    normalized, but some words (e.g. "DuFay") shouldn't be.
    (Also, some lemmas are strange, like "whatev".)
    """
    for token in doc:
        if tokenShouldUseForLang(token, nlp):
            yield _token_to_json(token, verbose=verbose)

            
def _token_to_json(token, verbose=False):
    if verbose:
        return {"lemma": token.lemma_ ,
                "pos": token.pos_,
                "tag": token.tag_,
                "norm": token.norm_,
                "is_oov": token.is_oov, # oov = out of vocabulary
                "text": token.text}
    else:
        return {"lemma": token.lemma_ ,
                "text": token.text}
            
# tokenShouldUseForLang : Token Language -> bool
def tokenShouldUseForLang(token, lang):
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
    if (token.is_punct or
        token.is_space or
        token.is_stop or # token.lemma_ checked below
        (token.lemma_ == "-PRON-")): # "-PRON-" sometimes classed as "ADJ"
        return False
    elif (token.pos_ not in _interesting_pos_strings):
        return False
    elif (token.lemma_ in lang.Defaults.stop_words):
        # token.is_stop checked above
        return False
    elif (not check_enough_alphabetic_chars(token.lemma_)):
        return False
    else:
        return True

_min_alphabetic_chars = 3 # must be positive
_char_alphabetic_regex = regex.compile("\\p{Alphabetic=Yes}")

# check_enough_alphabetic_chars : str -> bool
# Returns True IFF its arg has at least _min_alphabetic_chars
# characters that match _char_alphabetic_regex.
def check_enough_alphabetic_chars(lemma):
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
    import argparse
    from pydrnlp.jsonio import map_json_lines, write_json_line
    parser = argparse.ArgumentParser(description="engine for Trends tool")
    parser.add_argument("--verbose",
                        action="store_true",
                        help="include debugging details in JSON output")
    args = parser.parse_args()
    write_json_line(revision())
    map_json_lines(lambda js: analyze_all(js, verbose=args.verbose))


