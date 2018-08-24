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
 (@defproc[(pydrnlp? [v any/c]) any/c]
   @defproc[(pydrnlp-kill [instance pydrnlp?]) any]
   @defproc[(pydrnlp-dead? [instance pydrnlp?])
            (or/c #f natural-number/c)]
   @defproc[(pydrnlp-dead-evt [instance pydrnlp?])
            (evt/c natural-number/c)])]{
 At the Racket level, the functionality provided from
 the Python layer is accesed via @deftech{pydrnlp instances},
 which are recognized by the @racket[pydrnlp?] predicate.
 Currently the only way to create an instance is via
 @racket[basic-pydrnlp], but other types of instances may be
 added in the future.

 Constructing a @tech{pydrnlp instance} typically allocates
 some system-level resources (such as a Python process)
 that must be released, either by calling @racket[pydrnlp-kill]
 or by shutting down its controlling @rkttech{custodian}.
 For an @deftech{instance} which has been shut down,
 @racket[pydrnlp-dead?] responds with the exit code of the
 Python process (so @racket[0] means success):
 such an instance is said to be @deftech{dead}.
 An instance can also become dead without having been 
 explicitly shut down (for example, if the underlying
 process exited for some other reason).
 Using @racket[pydrnlp-dead-evt] returns a
 @rkttech{synchronizable event} that becomes ready when
 the instance has become dead.
 The synchronization result of the event is the exit code
 that would have been returned by @racket[pydrnlp-dead?].

 Importantly, synchronizable events awaiting the results
 of computations from the Python layer @bold{do not} become
 ready when a @tech{pydrnlp instance} becomes @tech{dead},
 nor do functions that request such results raise exceptions
 when applied to dead instances.
 Clients can implement such functionality when desired
 using Racket's synchronization primitives.
}


@deftogether[
 (@defproc[(basic-pydrnlp [#:port port ip-port-num/c (flask-port)]
                          [#:quiet? quiet? any/c (flask-quiet?)])
           (and/c basic-pydrnlp?
                  pydrnlp?)]
   @defproc[(basic-pydrnlp? [v any/c]) any/c]
   @defparam[flask-port port ip-port-num/c #:value 5005]
   @defboolparam[flask-quiet? quiet? #:value #t])]{
 Constructs a @tech{pydrnlp instance} which communicates with
 Python by launching Flask's development webserver
 (which does not support concurrency, among other limitations).
 Whether specified via parameters or keyword arguments,
 the @racket[port] value specifies the port on which the
 Flask server runs and the @racket[quiet?] value determines
 whether Flask is allowed to write to
 @racket[current-output-port] and @racket[current-error-port].
}










@section{Tokenization}

@deftogether[
 (@defproc[(pydrnlp-tokenize-evt [instance pydrnlp?]
                                 [segments (listof tokenize-arg?)])
           (evt/c (promise/c (listof tokenize-result?)))]
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
 The core functionality provided by the Python layer
 is a form of tokenization: splitting some text into @deftech{tokens}
 (words, to a first approximation); identifying the @deftech{lemma},
 or normalized base form, for each token; and counting the number of
 occurences of each lemma in the text.
 The implementation should attempt to ignore lemmas which are
 excessively common and uninteresting (e.g. ``the'').

 This functionality is accessed through @racket[pydrnlp-tokenize-evt].
 @;{@;TODO: Update this
 The structure of its @racket[segments] argument is to support sending
 multiple segments at once.
 Its outer hash table represents a collection of documents, where
 each document is identified by a string key. This string is
 mapped to an inner hash table representing the document's collection
 of segments: it maps numbers identifying the segments to strings
 containing their text.
 }
 
 Calling @racket[pydrnlp-tokenize-evt] produces a
 @rkttech{synchronizable event} that @bold{TODO: think through more
  specific details}. The event becomes ready for synchronization
 when a result is received from the Python layer. Its synchronization
 result is a promise that, when forced, either raises an exception or
 returns @bold{TODO: document this better}.
 @;{@;TODO: Update this
 returns a hash table mapping each string key that identified a document
 in the @racket[segments] argument to a
 corresponding @racket[tokenized-result] (documented below).
 }
}


@defproc[(pydrnlp-tokenizer-revision-evt [instance pydrnlp?])
         (evt/c (promise/c jsexpr?))]{
 Tokenizing documents with @racket[pydrnlp-tokenize-evt]
 is a fairlt expensive operation: results should likely
 be cached. However, this library remains a work in
 progress: we may improve our implementation over time.
 To support caching, this library guarantees that,
 when two @tech{pydrnlp instances} return @racket[equal?]
 values inside the promise from
 @racket[pydrnlp-tokenizer-revision-evt], those instances
 are interchangable with respect to
 @racket[pydrnlp-tokenize-evt]: @racket[equal?] arguments
 will return @racket[equal?] results, even across multiple
 runs of a program.

 Calling @racket[pydrnlp-tokenizer-revision-evt] repeatedly
 on the same @tech{pydrnlp instance} always returns the same
 promise (i.e. it does not send multiple
 messages to the underlying Python process),
 and the event produced by calling
 @racket[pydrnlp-tokenizer-revision-evt] on a @tech{dead}
 instance will eventually become ready for synchronization
 if the value was retreived before the instance became dead.
}


@deftogether[
 (@defproc[(pydrnlp-tokenize [instance pydrnlp?]
                             [segments (listof tokenize-arg?)])
           (listof tokenize-result?)]
   @defproc[(pydrnlp-tokenizer-revision [instance pydrnlp?])
            jsexpr?])]{
 Blocking versions of @racket[pydrnlp-tokenize-evt] and
 @racket[pydrnlp-tokenizer-revision-evt].
}

















