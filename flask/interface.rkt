#lang racket/base

(require racket/generic
         json
         syntax/parse/define
         racket/contract
         racket/match
         racket/promise
         (for-syntax syntax/parse
                     racket/base
                     ))

;; SEE provide at end of file (due to macros)

(module+ private
  (provide gen:pydrnlp
           correct-pydrnlp/c
           (contract-out
            [tokenize-arg->jsexpr
             (-> tokenize-arg? jsexpr?)]
            [try-parse-result-jsexpr
             (-> any/c (or/c #f (listof tokenize-result?)))]
           )))

(define-generics pydrnlp
  (pydrnlp-dead-evt pydrnlp)
  (pydrnlp-dead? pydrnlp)
  (pydrnlp-kill pydrnlp)
  ;;;;
  (pydrnlp-tokenizer-revision-evt pydrnlp)
  (pydrnlp-tokenize-evt pydrnlp to-send)
  #|END define-generics pydrnlp|#)


(define-syntax provide/contract+define-generic-contract
  (syntax-parser
    [(_ (~describe "name to bind" name:id)
        (~describe "define-generics contract constructor"constructor:id)
        [(~describe "method id" method:id)
         (~describe "contract expression" contract:expr)]
        ...)
     #`(begin
         (define name
           (constructor
            [method contract] ...))
         (provide (contract-out
                   [method contract] ...)))]))

(define-syntax-parser define-blocking
  [(_ blocking:id nonblocking:id)
   #'(define-blocking blocking () nonblocking)]
  [(_ blocking:id (extra-formal:id ...) nonblocking:id)
   #'(define (blocking pydrnlp extra-formal ...)
       (force (sync (nonblocking pydrnlp extra-formal ...))))])
(define-blocking pydrnlp-tokenizer-revision
  pydrnlp-tokenizer-revision-evt)
(define-blocking pydrnlp-tokenize (to-send)
  pydrnlp-tokenize-evt)




(struct tokenize-arg (lang key text)
  #:transparent)

(struct tokenize-result (key body)
  #:transparent)

(struct token (lemma text)
  #:transparent)

(define tokenize-arg->jsexpr
  (match-lambda
    [(tokenize-arg lang key text)
     (hasheq 'lang (case lang
                     [(en) "en"]
                     [(fr) "fr"])
             'key key
             'body text)]))

(define try-jsexpr->tokenize-result
  (match-lambda
    [(hash-table ['key key]
                 ['tokenized (list (hash-table
                                    ['lemma (app string->symbol lemma...)]
                                    ['text (app datum-intern-literal text...)])
                                   ...)])
     (tokenize-result key (map token lemma... text...))]
    [_
     #f]))

(define (try-parse-result-jsexpr js)
  (and (list? js)
       (let/ec return
         (for/list ([j (in-list js)])
           (define rslt
             (try-jsexpr->tokenize-result j))
           (unless rslt
             (return #f))
           rslt))))

(provide pydrnlp?
         (contract-out
          [struct tokenize-arg
            ([lang (or/c 'en 'fr)]
             [key jsexpr?]
             [text string?])]
          [struct tokenize-result
            ([key jsexpr?]
             [body (listof token?)])]
          [struct token
            ([lemma symbol?]
             [text (and/c string? immutable?)])]
          [pydrnlp-tokenizer-revision
           (-> pydrnlp?
               jsexpr?)]
          [pydrnlp-tokenize
           (-> pydrnlp? (listof tokenize-arg?)
               (listof tokenize-result?))]
          ))

(provide/contract+define-generic-contract
  correct-pydrnlp/c pydrnlp/c
  [pydrnlp-kill
   (-> pydrnlp? any)]
  [pydrnlp-dead-evt
   (-> pydrnlp?
       (evt/c natural-number/c))]
  [pydrnlp-dead?
   (-> pydrnlp?
       (or/c #f natural-number/c))]
  [pydrnlp-tokenizer-revision-evt
   (-> pydrnlp?
       (evt/c (promise/c jsexpr?)))]
  [pydrnlp-tokenize-evt
   (-> pydrnlp? (listof tokenize-arg?)
       (evt/c (promise/c (listof tokenize-result?))))]
  )




