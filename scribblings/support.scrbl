#lang scribble/manual

@title[#:tag "framework"]{
 Framework for Implementing Additional Tools
}

@(require "include-python.rkt"
          "bibliography.rkt"
          "lib.rkt"
          scribble/decode
          scribble/core
          scribble/racket
          (for-syntax racket/base)
          (for-label (except-in racket
                                date
                                date?)
                     json ;; FIXME ricoeur/stdlib/json
                     pydrnlp
                     pydrnlp/support))

@section{Managing Python Processes from Racket}
@defmodule[pydrnlp/support
           #:use-sources {pydrnlp/support/revision-contract}]

@(define jsexpr
   ;; FIXME ricoeur/stdlib/json
   (tech #:doc '(lib "json/json.scrbl") "jsexpr"))

@(define (rkttech . args)
   (apply tech
          #:doc '(lib "scribblings/reference/reference.scrbl")
          args))

@(letrec-syntaxes+values
     ([{id?:tag}
       (make-element-id-transformer (λ (x) #'id?:tag-))]
      [{id-revision:tag}
       (make-element-id-transformer (λ (x) #'id-revision:tag-))]
      [{launch-id:tag}
       (make-element-id-transformer (λ (x) #'launch-id:tag-))]
      [{id-send/raw:tag}
       (make-element-id-transformer (λ (x) #'id-send/raw:tag-))])
   ([{mk} (λ (a b [swap? #f])
            (let*-values ([{a b} (if swap?
                                     (values b a)
                                     (values a b))])
              (define tag (generated-tag))
              (define (mk ctor style)
                (let*-values ([{b} (element symbol-color
                                            (element style b))]
                              [{a b} (if swap?
                                         (values b a)
                                         (values a b))])
                  (ctor tag (elem #:style 'no-break a b))))
              (values (mk elemtag value-def-color)
                      (mk elemref symbol-color))))]
    [{id?:tag- id?} (mk (racket _id) (racketid ?))]
    [{id-revision:tag- id-revision} (mk (racket _id) "-revision")]
    [{launch-id:tag- launch-id} (mk "launch-" (racket _id) 'swap)]
    [{id-send/raw:tag- id-send/raw} (mk (racket _id) "-send/raw")])
   @deftogether[[
 @defproc[(python-worker? [v any/c])
          boolean?]
 @defform[#:literals {...}
          (define-python-worker id
            mod-bytes-literal arg-bytes-literal ...)
          #:contracts
          ([id?:tag (-> any/c boolean?)]
           [id-revision:tag jsexpr?]
           [launch-id:tag (->* [] [#:quiet? any/c] #,id?)]
           [id-send/raw:tag (->* [#,id? jsexpr?]
                                 [#:who symbol?]
                                 (stream/c jsexpr?))])]
 @defproc[(python-worker-running? [worker python-worker?])
          boolean?]
 @defproc[(python-worker-kill [worker python-worker?])
          any]
 @defproc[(python-worker-dead-evt [worker python-worker?])
          (evt/c (or/c #f exn:fail?))]]]{
                                         
 A @deftech{Python worker} value encapsulates a Python process 
 running one of @racketmodname[pydrnlp]'s Python modules and
 manages request--response communication between Racket and Python.
 All Python workers are recognized by the predicate
 @racket[python-worker?] and share the same abstract interface.

 Every Python worker is an instance of some
 concrete @deftech{Python worker type},
 which specifies a particular Python module
 with which to communicate.
 A Python worker type is defined by the
 @racket[define-python-worker] form, which binds
 @|id?|, @|id-revision|, @|launch-id|, and @|id-send/raw|
 (synthesized with the lexical context of @racket[id])
 to values implementing the type-specific portion of the interface.  
 The @racket[mod-bytes-literal] must name one of
 @racketmodname[pydrnlp]'s Python modules using the
 same syntax as @exec{python -m}:
 for example, @racket[#"pydrnlp.trends"].
 Any additional @racket[arg-bytes-literal]s are 
 passed as command-line arguments to the Python module.
 The Python module named by @racket[mod-bytes-literal]
 must define a @tech{Python revision function}
 (see @racketmodname[pydrnlp/support/python-lang]
 and @racket[python-revision-value/c]):
 if it does not, the @racket[define-python-worker] form
 will raise a syntax error.

 The @|launch-id| function creates @tech{Python worker}
 values of the concrete @tech{Python worker type},
 which are recognized by the predicate @|id?|.
 If the @racket[#:quiet] argument is given and is not @racket[#false],
 @; CMS 16ed. 7.21 (p 355) says "process'" would be "not recommended"
 the Python process's standard error output is written to
 the @racket[current-error-port]; otherwise, it is discarded.
 Creating a @tech{Python worker} allocates system-level
 resources---in particular, it starts an OS subprocess.
 These resources are placed in the custody of
 the @rkttech{current custodian}, and they
 must be freed when the worker is no longer needed,
 typically by calling either @racket[python-worker-kill]
 or @racket[custodian-shutdown-all].
 More generally, the system-level resources are freed
 when the worker becomes @tech{dead}.
 
 Calling @|id-send/raw| sends a @jsexpr as a request
 to the process encapsulated by the @tech{Python worker}
 and returns its response as a lazy @rkttech{stream} of @jsexpr values.
 @margin-note*{To implement the Python side of this interaction,
  use @pythonmodlink{pydrnlp.jsonio}.}
 Messages are sent to the Python process asynchronously,
 in sequential order, and @|id-send/raw| returns immediately,
 though forcing the returned stream (e.g@._ with @racket[stream-first]
 or @racket[stream-rest]) will block until the request has been sent
 and the response has begun to be received.
 @tech{Python worker} values are thread-safe:
 @|id-send/raw| can be called with the same worker concurrently 
 from multiple Racket @rkttech{threads}, and one client thread
 being terminated, blocking, @etc will not interfere with use of
 the worker from other client threads.
 However, workers intentionally are @italic{not} fully ``kill-safe'' 
 in the sense of @citation[|Kill Safe|]:
 shutting down the managing @rkttech{custodian} must release
 the system-level resources, and clients can cause the worker to
 become @tech{dead} (intentionally or not) in various other ways.

 @margin-note{Process-level parallelism can be obtained by
  using @|launch-id| to create multiple workers of the same
  @tech{Python worker type}. However, note that the
  @hyperlink["https://digitalricoeur.org"]{digitalricoeur.org}
  server currently doesn't have very many cores, anyway.
 }

 The functions implemented by @tech{Python worker types}
 are often expensive.
 The @id-revision value is defined to support caching
 and avoid redundant calls to @|id-send/raw|.
 When @id-revision is @racket[#false], any cached value
 should be ignored.
 Otherwise, if @id-revision is @racket[equal?] to
 a cached value of @id-revision from a previous run,
 it means that the Python module encapsulated
 by the @tech{Python worker type} promises that
 calling @id-send/raw with ``the same'' request
 would produce ``the same'' response,
 and therefore cached responses can be used.
 @margin-note*{Of course, the Python module must take care
 to live up to this promise when implementing
 its @tech{Python revision function}.}
 Note that the applicable notion of ``the same''
 is specific to the Python module and @tech{Python worker type}:
 ``the same'' may mean something either stronger or
 weaker that @racket[equal?].
 In addition to the @tech{Python revision function},
 the value of @id-revision also reflects the versions
 of the @spaCy library and the language models being used.

 Note that access to @id-revision @italic{does not} require running
 Python (or even having it installed), even though
 @id-revision incorporates values defined in Python code.
 Instead, by enforcing constraints on the syntax of
 @tech{Python revision functions}, @racketmodname[pydrnlp/support/python-lang]
 is able to analyse their definitions statically and compile
 them to Racket code.

 The name of @|id-send/raw| reflects the fact that it enforces
 only the raw, @|jsexpr|-based communication protocol common
 to all @tech{Python workers}.
 In practice, the Racket and Python parties to a particular interaction
 will both have invariants about the messages they expect to send
 and receive.
 When designing a new @tech{Python worker type}, @|id-send/raw|
 should be used to implement higher-level communication functions,
 which can enforce specific contracts and convert values
 to and from the @jsexpr representation used for communication.
 To facilitate such wrapper functions, @|id-send/raw| will
 report errors that cannot be detected by first-order tests
 using its @racket[#:who] argument, if given,
 rather than its own symbolic name.
 On the other hand, @id-send/raw does enforce its documented
 contract and will blame its callers for violating their obligations.
 
 @elemtag["python-worker-running? race"]{
  For example, a test like:}
 @racketblock[
 (when (python-worker-running? a-worker)
   (#,id-send/raw a-worker "Hi, world!"))]
 is not sufficient to ensure that @racket[a-worker] is running
 when @|id-send/raw| is called, because @racket[a-worker]
 could have become @tech{dead} concurrently,
 after @racket[python-worker-running?] returns
 but before @id-send/raw is called.

 If the @tech{Python worker} given to @id-send/raw
 becomes @tech{dead} before @id-send/raw can enqueue
 the @jsexpr value to be sent asynchronously,
 @id-send/raw raises an exception,
 which will refer to its @racket[#:who] argument, if given.
 If the @tech{Python worker} becomes @tech{dead} before it has finished
 producing its response, an exception is raised when
 the corresponding part of the @rkttech{stream} returned
 by @id-send/raw is forced.

 A @tech{Python worker} is @deftech{dead} when it has
 freed all of its system-level resources, and thus is no
 longer in communication with a Python process.
 Programmers must ensure that the worker is @tech{dead}
 when they no longer need it, and generally they will
 need to do so explicitly.
 The function @racket[python-worker-kill] causes its
 argument to become @tech{dead} immediately;
 calling it on a @tech{Python worker} that is
 already @tech{dead} has no effect.
 Using @racket[python-worker-kill] is equivalent
 to shutting down the worker's managing @rkttech{custodian},
 except that @racket[python-worker-kill] only effects
 resources encapsulated by the given @tech{Python worker} value.

 It is almost always best to make a @tech{Python worker}
 @tech{dead} with @racket[python-worker-kill]
 or @racket[custodian-shutdown-all] as soon as you can
 determine that the worker value is no longer needed.
 However, calls to @launch-id incur significant overhead,
 so it is much better to reuse @tech{Python workers}
 than to create and free them repeatedly.
 Even better, by consulting @id-revision,
 you may be able to avoid calling @launch-id in the first place.

 Even if it is never subjected to @racket[python-worker-kill]
 or @racket[custodian-shutdown-all], a @tech{Python worker}
 may still become @tech{dead} for other reasons.
 In particular, a @tech{Python worker} will become dead
 if the Python process it manages exits of its own accord,
 either successfully or, for example,
 due to an unhandled exception.
 Nonetheless, this possibility does not relieve programmers
 of the burden of ensuring that the all @tech{Python workers}
 do, in fact, actually become @tech{dead}.
 Even if a @tech{Python worker type} implements a comunication
 protocol in which the Python module is expected to exit,
 the Racket side of the communication should still check that
 the worker actually is @tech{dead}: if it isn't, the Racket
 side should clean up and signal that the invariants of the
 communication protocol have been violated.
 
 The function @racket[python-worker-dead-evt]
 takes any @tech{Python worker} value and produces a
 @rkttech{synchronizable event} that becomes
 @rkttech{ready for synchronization} when the worker is @tech{dead}.
 The event's @rkttech{synchronization result} is either @racket[#false]
 or an @racket[exn:fail] that caused the worker to become @tech{dead}.
 Currently, a @racket[#false] result does not necessarily mean that
 the worker became @tech{dead} ``normally'':
 this may be improved in the future.
 
 Conversely, the predicate @racket[python-worker-running?]
 recognizes any @tech{Python worker} that is @italic{not}
 currently @tech{dead}.
 Taking a point-in-time snapshot
 @elemref["python-worker-running? race"]{has some limitations},
 but @racket[python-worker-running?] has the benefit of being
 an inexpensive first-order test.

 })

@defthing[python-revision-value/c flat-contract?
          #:value (or/c #f
                        exact-integer?
                        (listof python-revision-value/c))]{
                                                           
 @italic{Documentation forthcoming.}
  
}



@section{Python--Racket Bridge Language}
@defmodule[pydrnlp/support/python-lang #:lang]

@italic{Documentation forthcoming.}

If you wish to begin your module with
both a @racketcommentfont{#!} line and a
@hyperlink["https://www.python.org/dev/peps/pep-0263/"
           @racketplainfont{coding}]
declaration, the following lines satisfy the Python,
Racket, and Emacs parsers simultaneously:
@codeblock{
 #!/usr/bin/env python3
 #lang pydrnlp/support/python-lang # -*- coding: utf-8 -*-
 """Module docstring

 Lots of great documentation here ...
 """
}

@margin-note{In Python 3,
 @hyperlink["https://docs.python.org/3/howto/unicode.html#unicode-literals-in-python-source-code"]{
  the default encoding is UTF-8}.
}

@deftech{Python revision function}




@section{Scribbling Python Documentation}

@italic{Documentation forthcoming.}




@(decode @list{
 @title{Python Utility Modules}
 @include-python-section{pydrnlp.language}
 @include-python-section{pydrnlp.jsonio}
 })

