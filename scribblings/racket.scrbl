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
 A @deftech{tokenizer instance} encapsulates a Python process
 for performing @tech{tokenization}.
 Tokenizer instances are stateful:
 each instance supports at most one call
 to @racket[tokenizer-tokenize!].
 
 System-level resources encapsulated by the
 @tech{tokenizer instance} are placed in the custody of
 the @racket[current-custodian].
 Tokenization can be interrupted if the custodian
 is shut down, or alternatively if
 the tokenizer instance is explicitly terminated
 with @racket[tokenizer-kill!].
 
 When a @tech{tokenizer instance} has been created
 that will not be used with @racket[tokenizer-tokenize!],
 one of these mechanisms must be used to free
 the system-level resources.
}

@defproc[(tokenizer-revision [t tokenizer?])
         jsexpr?]{
 Returns a JSON-convertable value representing the
 tokenization implementation used by the
 @tech{tokenizer instance} @racket[t].
 
 Tokenization is a fairly expensive operation:
 the purpose of @racket[tokenizer-revision] is to
 support caching.
 If two @tech{tokenizer instances} return @racket[equal?]
 values from @racket[tokenizer-revision],
 they are guaranteed to be equivalent in terms of
 @racket[tokenizer-tokenize!], even across multiple
 runs of a program.
}

@deftogether[
 (@defproc[(tokenizer-tokenize! [t tokenizer?]
                                [args (listof tokenize-arg?)])
           (promise/c (listof tokenize-result?))]
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
 The purpose of the Python process encapsulated by a
 @tech{tokenizer instance} is to perform a a form of
 @deftech{tokenization}: splitting some text into
 @deftech{tokens} (words, to a first approximation)
 and identifying the @deftech{lemma},
 or normalized base form, for each token.
 The implementation attempts to ignore lemmas which are
 excessively common and uninteresting (e.g. ``the'').

 This functionality is accessed through
 @racket[tokenizer-tokenize!].
 It is called with a list of @racket[tokenize-arg]
 values. The @racket[tokenize-arg-lang] identifies the
 language to be used (currently English or French),
 the @racket[tokenize-arg-text] is the actual text
 to be tokenized, and the @racket[tokenize-arg-key]
 is an arbitrary JSON-convertable value that is
 propigated to the @racket[tokenize-result-key] field
 of the corresponding @racket[tokenize-result] value.\

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

 A given @tech{tokenizer instance} can be used with
 @racket[tokenizer-tokenize!] at most once:
 any subsequent call will raise an exception.
}

@defproc[(tokenizer-promise [t tokenizer?])
         (promise/c (listof tokenize-result?))]{
 Returns the same promise that @racket[tokenizer-tokenize!]
 would return.

 Unlike @racket[tokenizer-tokenize!], @racket[tokenizer-promise]
 can be called repeatedly and at any time.
 If the promise is obtained via @racket[tokenizer-promise]
 before @racket[tokenizer-tokenize!] has been called,
 forcing the promise will block until @racket[tokenizer-tokenize!]
 is called or the @tech{tokenizer instance} @racket[t]
 otherwise becomes unable to compute a value
 (in which case forcing the promise will raise an exception).
}

@defproc[(tokenizer-accepting? [t tokenizer?])
         boolean?]{
 Returns @racket[#true] if and only if
 @racket[tokenizer-tokenize!] would not raise
 an exception when called with @racket[t],
 which implies that neither
 @racket[tokenizer-tokenize!] nor @racket[tokenizer-kill!]
 have ever been called with @racket[t]
 and that @racket[t]'s controlling custodian has not
 been shut down.
}

@defproc[(tokenizer-kill! [t tokenizer?])
         any]{
Releases the system-level resources assosciated with
the @tech{tokenizer instance} @racket[t],
equivalently to shutting down its controlling custodian.
}
          


