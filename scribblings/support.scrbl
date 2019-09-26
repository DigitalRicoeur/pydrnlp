#lang scribble/manual

@title[#:tag "framework"]{
 Framework for Implementing Additional Tools
}

@(require "include-python.rkt"
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

@section{Racket Interface}
@defmodule[pydrnlp/support
           #:use-sources {pydrnlp/support/revision-contract}]

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
                                         
 A @deftech{Python worker} encapsulates a Python process 
 running one of @racketmodname[pydrnlp]'s Python modules.
 All Python workers are recognized by the predicate
 @racket[python-worker?] and share the same interface.

 A concrete @deftech{Python worker type} is defined
 using @racket[define-python-worker], which binds
 @|id?|, @|id-revision|, @|launch-id|, and @|id-send/raw|
 with the lexical context of @racket[id].  
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

 The @|launch-id|
 @racket[(current-error-port)]
 
 
 @rkttech{custodian}
 
 @pythonmodlink{pydrnlp.jsonio}

 @;{@; Old stuff:

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
    
  If the @tech{tokenizer} @racket[t] is not running
  (in the sense of @racket[tokenizer-running?])
  when @racket[tokenizer-tokenize] is called,
  or if it stops running before returning a result,
  an exception is raised.



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
 }

 })

@defthing[python-revision-value/c flat-contract?
          #:value (or/c #f
                        exact-integer?
                        (listof python-revision-value/c))]{
                                                           
 @italic{Documentation forthcoming.}
  
}

@section{Scribbling Python Documentation}

@italic{Documentation forthcoming.}

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
 ...
 """
}

@margin-note{In Python 3,
 @hyperlink["https://docs.python.org/3/howto/unicode.html#unicode-literals-in-python-source-code"]{
  the default encoding is UTF-8}.
}

@deftech{Python revision function}

@(decode @list{
 @title{Python Utility Modules}
 @include-python-section{pydrnlp.language}
 @include-python-section{pydrnlp.jsonio}
 })
