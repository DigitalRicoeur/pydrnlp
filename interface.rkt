#lang racket

(require racket/generic
         json
         "tokenized-result.rkt"
         syntax/parse/define
         racket/provide-syntax
         (for-syntax syntax/parse
                     ))

;; SEE provide at end of file (due to macros)

(module+ private
  (provide gen:pydrnlp
           correct-pydrnlp/c
           ))

(define-generics pydrnlp
  (pydrnlp-dead-evt pydrnlp)
  (pydrnlp-dead? pydrnlp)
  (pydrnlp-kill pydrnlp)
  ;;;;
  (pydrnlp-tokenizer-revision-evt pydrnlp)
  (pydrnlp-tokenize-evt pydrnlp to-send)
  ;;;;
  (pydrnlp-tokenizer-revision pydrnlp)
  (pydrnlp-tokenize pydrnlp to-send)
  #:fallbacks
  [(define-syntax-parser define-blocking
     [(_ blocking:id nonblocking:id)
      #'(define-blocking blocking () nonblocking)]
     [(_ blocking:id (extra-formal:id ...) nonblocking:id)
      #'(begin
          (define/generic super-nonblocking nonblocking)
          (define (blocking pydrnlp extra-formal ...)
            (force (sync (super-nonblocking pydrnlp extra-formal ...)))))])
   (define-blocking pydrnlp-tokenizer-revision
     pydrnlp-tokenizer-revision-evt)
   (define-blocking pydrnlp-tokenize (to-send)
     pydrnlp-tokenize-evt)
   #|END #:fallbacks|#]
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

(provide pydrnlp?
         )

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
  [pydrnlp-tokenizer-revision
   (-> pydrnlp?
       jsexpr?)]
  [pydrnlp-tokenize-evt
   (-> pydrnlp? tokenize-arg/c
       (evt/c (promise/c tokenization-results/c)))]
  [pydrnlp-tokenize
   (-> pydrnlp? tokenize-arg/c
       tokenization-results/c)]
  )




