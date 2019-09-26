#lang scribble/manual

@title{``Trends'' Tool}
@defmodule[pydrnlp/trends]

@(require "include-python.rkt"
          scribble/decode
          (for-label (except-in racket
                                date
                                date?)
                     ricoeur/tei
                     pydrnlp
                     pydrnlp/support))

@italic{Documentation forthcoming.}

@;{
 Specifically, @deftech{tokenization} splits some text into
 @deftech{tokens} (words, to a first approximation)
 and identifies the @deftech{lemma},
 or normalized base form, for each token.
 The implementation attempts to ignore lemmas which are
 excessively common and uninteresting (e.g. ``the'').

 The @racket[token] datastructure includes both the
 @tech{lemma} (as a symbol, for easy comparison)
 and an immutable copy of the original string in the
 @racket[token-text] field.
 This provides an example of an actual use of the word,
 which is needed because the lemma is always normalized,
 but some words (e.g. ``DuFay'') shouldn't be.
 (Also, some lemmas are strange, like ``whatev''.)
}

@(decode @list{
 @title{Implementation Details}

 @include-python-section[pydrnlp.trends #:title]{Python Layer}

 })
