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
                                 [segments tokenize-arg/c])
           (evt/c (promise/c tokenization-results/c))]
   @defthing[tokenize-arg/c flat-contract?
             #:value (hash/c string?
                             (hash/c natural-number/c
                                     string?)
                             #:immutable #t)]
   @defthing[tokenization-results/c flat-contract?
             #:value (hash/c string?
                             tokenized-document?
                             #:immutable #t)])]{
 The core functionality provided by the Python layer
 is a form of tokenization: splitting some text into @deftech{tokens}
 (words, to a first approximation); identifying the @deftech{lemma},
 or normalized base form, for each token; and counting the number of
 occurences of each lemma in the text.
 The implementation should attempt to ignore lemmas which are
 excessively common and uninteresting (e.g. ``the'').

 This functionality is accessed through @racket[pydrnlp-tokenize-evt].
 The structure of its @racket[segments] argument is to support sending
 multiple segments at once.
 Its outer hash table represents a collection of documents, where
 each document is identified by a string key. This string is
 mapped to an inner hash table representing the document's collection
 of segments: it maps numbers identifying the segments to strings
 containing their text.
 
 Calling @racket[pydrnlp-tokenize-evt] produces a
 @rkttech{synchronizable event} that @bold{TODO: think through more
  specific details}. The event becomes ready for synchronization
 when a result is received from the Python layer. Its synchronization
 result is a promise that, when forced, either raises an exception or
 returns a hash table mapping each string key that identified a document
 in the @racket[segments] argument to a
 corresponding @racket[tokenized-result] (documented below).
}

@deftogether[
 (@defstruct*[tokenized-document ([lemma->string lemma->string/c]
                                  [segments (hash/c natural-number/c
                                                    lemma->count/c
                                                    #:immutable #t)])
              #:omit-constructor]
   @defthing[lemma->count/c flat-contract?
             #:value (hash/c symbol?
                             exact-positive-integer?
                             #:immutable #t)]
   
   @defproc[(union:lemma->count [hsh lemma->count/c] ...)
            lemma->count/c])]{
 A @racket[tokenized-document] represents the result of
 tokenizing a single document with @racket[pydrnlp-tokenize-evt].
 A @tech{lemma} is represented by a symbol, representing
 its essential uniqueness properties.
 The @racket[lemma->string] field provides a mapping from
 lemmas to strings; see @racket[lemma->string/c] below
 for more details.

 The hash table in the @racket[segments] field maps the numbers
 used to identify the document's segments to hash tables of
 results, which satisfy @racket[lemma->count/c].
 These map each lemma that occured in the segment to the
 number of times it occurred.

 Clients of this library will often want to combine
 @racket[lemma->count/c] hash tables, e.g. to compute the
 results for the document as a whole.
 Use @racket[union:lemma->count] for that purpose.

 Note that the @racket[tokenized-document] constructor is
 not provided.
}

@deftogether[
 (@defthing[lemma->string/c flat-contract?
            #:value (hash/c symbol?
                            string?
                            #:immutable #t)]
   @defproc[(union:lemma->string [hsh lemma->string/c] ...)
            lemma->string/c])]{
 One might think that a @tech{lemma} would be suitable
 for display to an end user, but the ammount of normalization
 means that this is not the case.
 Some words are case-folded that shouldn't be
 (e.g. @racket["DuFay"]), and some lemmas are strange in
 other ways (e.g. @racket["whatev"]).

 A hash table satisfying @racket[lemma->string/c] maps the
 symbolic lemma to a @deftech{candidate representative}
 string that appeared in the actual text from which the
 table was generated.

 Unlike with @racket[lemma->count/c] hash tables, where more
 specific tables represent granularity in the data,
 a lemma should always be displayed to the user using the
 best candidate representative string from @italic{all}
 available text in the corpus, even in a context when 
 only a subset of the corpus is being analyzed.
 Some particular passage might only contain a mediocre
 candidate representative for a lemma that has a better
 candidate from elsewhere in the corpus.
 The function @racket[union:lemma->string] merges several
 such hash tables, choosing for each lemma the best
 available candidate representative.
 (The precise notion of ``best'' is unspecified and
 subject to refinement over time.)
 These results are reported at a per-document level
 primarily to support caching.
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
                             [segments tokenize-arg/c])
           tokenization-results/c]
   @defproc[(pydrnlp-tokenizer-revision [instance pydrnlp?])
            jsexpr?])]{
 Blocking versions of @racket[pydrnlp-tokenize-evt] and
 @racket[pydrnlp-tokenizer-revision-evt].
}

















