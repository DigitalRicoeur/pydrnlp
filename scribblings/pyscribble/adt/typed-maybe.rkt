#lang typed/racket

;; This should go somewhere else.

(provide just
         nothing
         maybe?
         just?
         nothing?
         maybe
         from-just
         from-just!
         filter-just
         map-maybe
         false->maybe
         with-maybe-handler
         exn->maybe
         fmap
         ;;;;
         Maybe
         Just
         Nothing
         NoValues
         )

(module interop racket
  (provide (all-from-out data/maybe)
           struct:just
           just-value
           nothing-val
           )
  (require data/maybe
           (for-syntax syntax/parse
                       racket/struct-info
                       racket/match
                       ))

  (define-syntax define-just-info
    (syntax-parser
      [(_ descriptor:id accessor:id)
       (match-define (list struct:just
                           _
                           _
                           (list just-value)
                           _
                           _)
         (extract-struct-info (syntax-local-value #'just)))
       #`(begin (define descriptor #,struct:just)
                (define accessor #,just-value))]))

  (define-just-info struct:just just-value)

  (define nothing-val nothing)

  ;(println (nothing? nothing-val))
  
  #|END module interop|#)

(module nothing typed/racket/base
  (provide NoValues)
  (define-type NoValues Nothing))

(require typed/racket/unsafe
         (submod "." nothing)
         (for-syntax syntax/parse
                     ))

(require/typed 
 (submod "." interop)
 [(from-just! from-just!/error) (-> Nothing NoValues)])

(unsafe-require/typed
 (submod "." interop)
 [#:opaque Nothing nothing?]
 [nothing-val Nothing]
 [#:struct (A) just ([value : A])
  #:type-name Just])



(define-type (Maybe A) (U Nothing (Just A)))

(define-predicate maybe? (Maybe Any))

(define-match-expander nothing
  (syntax-parser
    [(_) #'(? nothing?)])
  (syntax-parser
    [(~or* (_) _:id)
     #'nothing-val]
    [(_ arg:expr ...)
     #'(let () (void arg ...) nothing-val)]))

(: fmap (All (A B) (-> (-> A B) (Maybe A) (Maybe B))))
(define (fmap proc v)
  (match v
    [(just it)
     (just (proc it))]
    [_
     (nothing)]))

(: maybe (All (A B C) (-> A (-> B C) (Maybe B) (U A C))))
(define (maybe default proc v)
  (match v
    [(just it)
     (proc it)]
    [_
     default]))

(: from-just (All (A B) (-> A (Maybe B) (U A B))))
(define (from-just default v)
  (match v
    [(just it)
     it]
    [_
     default]))

(: from-just! (All (A) (-> (Maybe A) A)))
(define from-just!
  (match-lambda
    [(just it)
     it]
    [_
     (from-just!/error (nothing))]))
    
(: filter-just (All (A) (-> (Listof (Maybe A)) (Listof A))))
(define filter-just
  (match-lambda
    ['() '()]
    [(cons (just it) more)
     (cons it (filter-just more))]
    [(cons _ more)
     (filter-just more)]))
         
(: map-maybe (All (A B) (-> (-> A (Maybe B)) (Listof A) (Listof B))))
(define (map-maybe proc lst)
  (match lst
    [(cons this lst)
     (match (proc this)
       [(just it)
        (cons it (map-maybe proc lst))]
       [_
        (map-maybe proc lst)])]
    [_
     null]))

(: false->maybe (All (A) (-> (U False A) (Maybe A))))
(define (false->maybe x)
  (if x
      (just x)
      (nothing)))

(define-syntax with-maybe-handler
  (syntax-parser
    [(_ exn?:expr body:expr ...+)
     #`(with-handlers ([exn? (λ (_) nothing)])
         (just (let ()
                 body ...)))]))

(: exn->maybe (All (A B) (-> (-> Any Any)
                             (-> A * B)
                             A *
                             (Maybe B))))
(define (exn->maybe pred proc . args)
  (with-maybe-handler pred
    (apply proc args)))




