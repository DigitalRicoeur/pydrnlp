#lang scribble/manual

@title{Racket Interface}

@(require (for-label (except-in racket
                                date
                                date?)
                     pydrnlp
                     adjutor
                     json
                     ))

@(define (rkttech . args)
   (apply tech 
          #:doc '(lib "scribblings/reference/reference.scrbl")
          args))

@deftogether[
 (@defproc[(launch-tokenizer [#:quiet? quiet? any/c #t])
           tokenizer?]
   @defproc[(tokenizer? [v any/c])
            boolean?])]{
 A @deftech{tokenizer} encapsulates a Python process
 for performing @tech{tokenization}
 via @racket[tokenizer-tokenize].

 Tokenizer values are thread-safe: calls to
 @racket[tokenizer-tokenize] are handled sequentially,
 but @racket[tokenizer-tokenize] can be called 
 with the same @tech{tokenizer} concurrently
 from multiple threads, even before earlier calls to
 @racket[tokenizer-tokenize] have returned.
 
 Creating a @tech{tokenizer} allocates system-level
 resources, which are placed in the custody of
 the @racket[current-custodian].
 These resources must be freed when the tokenizer is
 no longer needed, either by shutting down the custodian
 or by calling @racket[tokenizer-kill].
}

@defproc[(tokenizer-revision [t tokenizer?])
         jsexpr?]{
 Returns a JSON-convertable value representing the
 tokenization implementation used by the
 @tech{tokenizer} @racket[t].
 
 Tokenization is a fairly expensive operation:
 the purpose of @racket[tokenizer-revision] is to
 support caching.
 If two @tech{tokenizers} return @racket[equal?]
 values from @racket[tokenizer-revision],
 they are guaranteed to be equivalent in terms of
 @racket[tokenizer-tokenize], even across multiple
 runs of a program.
 When the result of @racket[tokenizer-revision] changes,
 any cache is stale.

 Calling @racket[tokenizer-revision] does not block:
 the value is computed when the @tech{tokenizer} is created.
}

@deftogether[
 (@defproc[(tokenizer-tokenize [t tokenizer?]
                               [args (listof tokenize-arg?)])
           (listof tokenize-result?)]
   @defstruct*[tokenize-arg
               ([lang (or/c 'en 'fr)]
                [key jsexpr?]
                [text string?])]
   @defstruct*[tokenize-result
               ([key jsexpr?]
                [body (listof token?)])]
   @defstruct*[token
               ([lemma symbol?]
                [text (and/c string? immutable?)])])]{
 Performs @tech{tokenization} on @racket[args] using
 the @tech{tokenizer} @racket[t].
                                                      
 Specifically, @deftech{tokenization} splits some text into
 @deftech{tokens} (words, to a first approximation)
 and identifies the @deftech{lemma},
 or normalized base form, for each token.
 The implementation attempts to ignore lemmas which are
 excessively common and uninteresting (e.g. ``the'').

 The segments of text to be tokenized are communicated
 using @racket[tokenize-arg] values.
 The @racket[tokenize-arg-lang] identifies the
 language of the text.
 English and French are currently supported.
 The @racket[tokenize-arg-text] is the actual text
 to be tokenized, and the @racket[tokenize-arg-key]
 is an arbitrary JSON-convertable value that is
 propigated to the @racket[tokenize-result-key] field
 of the corresponding @racket[tokenize-result] value.

 The @racket[token] datastructure includes both the
 @tech{lemma} (as a symbol, for easy comparison)
 and an immutable copy of the original string in the
 @racket[token-text] field.
 This provides an example of an actual use of the word,
 which is needed because the lemma is always normalized,
 but some words (e.g. ``DuFay'') shouldn't be.
 (Also, some lemmas are strange, like ``whatev''.)
 Clients will need to implement a heuristic for choosing
 the best representitive string for each lemma, but
 that is beyond the scope of @racketmodname[pydrnlp].

 If the @tech{tokenizer} @racket[t] is not running
 (in the sense of @racket[tokenizer-running?])
 when @racket[tokenizer-tokenize] is called,
 or if it stops running before returning a result,
 an exception is raised.

 Note that @racket[tokenizer-tokenize] will block
 while waiting for the Python process.
}


@defproc[(tokenizer-running? [t tokenizer?])
         boolean?]{
 Returns @racket[#true] unless the Python process
 encapsulated by the @tech{tokenizer} @racket[t]
 has terminated.
 Termination can be triggerd from Racket by calling
 @racket[(tokenizer-kill t)] or by shutting down @racket[t]'s
 controlling @rkttech{custodian}, but a @tech{tokenizer}
 may also terminate for external reasons, such as
 an unhandled exception in the Python process.
}

@defproc[(tokenizer-kill [t tokenizer?])
         any]{
 Releases the system-level resources assosciated with
 the @tech{tokenizer} @racket[t],
 equivalently to shutting down its controlling custodian.
}
          


