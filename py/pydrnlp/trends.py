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
import pydrnlp.stop_words

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
    thisModuleRevision = 3
    return [thisModuleRevision,
            pydrnlp.stop_words.revision(),
            pydrnlp.language.revision()]


def analyze_all(jsIn):
    """Tokenizes JsIn. **TODO: document this.**"""
    ret = []
    for langStr, segs in jsIn.items():
        nlp = pydrnlp.language.get(langStr)
        ret.extend(_languageTokenizeSegments(nlp, segs))
    return ret


def _languageTokenizeSegments(nlp, segs):
    """Tokenizes a Listof[JsInSegment] segs using the language nlp."""
    args = ((jsInSeg["body"], jsInSeg["key"]) for jsInSeg in segs)
    for (doc, key) in nlp.pipe(args, as_tuples = True):
        yield {"key": key,
               "tokenized": list(_languageDocToListofJsToken(nlp, doc))}

        
def _languageDocToListofJsToken(nlp, doc):
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
            yield {"lemma":token.lemma_ ,
                   "text":token.text}

            
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
        (token.lemma_ == "-PRON-")): # "-PRON-" sometimes classed as "ADJ"
        return False
    elif (token.pos_ not in _interesting_pos_strings):
        return False
    elif tokenAnyIsStopForLanguage(token, lang):
        return False
    else:
        return True
    

_interesting_pos_strings = frozenset({
    "ADJ", # adjective
    "ADV", # adverb
    "INTJ", # interjection
    "NOUN", # noun
    "PROPN", # proper noun
    "VERB", # verb
    "X" # other
    })


if __name__ == "__main__":
    from pydrnlp.jsonio import map_json_lines, write_json_line
    write_json_line(revision())
    map_json_lines(analyze_all)


