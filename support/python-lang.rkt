#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [modbegin #%module-begin]))

(require syntax/parse/define
         racket/contract
         "revision-contract.rkt"
         (for-syntax racket/base
                     racket/list
                     racket/string
                     racket/sequence
                     racket/match
                     syntax/contract
                     "python-lang/stx.rkt"
                     "python-lang/prefabs.rkt"))

(define-syntax-parser modbegin
  [(_ :python-module-forms)
   #'(#%plain-module-begin
      (module configure-runtime '#%kernel
        (#%require racket/runtime-config)
        (configure #f))
      (maybe-define-revision form ...)
      (module* doc pydrnlp/scribblings/python-doc-lang
        form ...)
      (module* test #f
        (require (submod ".." doc))))])

(define-syntax-parser maybe-define-revision
  ;; so syntax-local-lift require works
  [(_ :python-module-forms)
   (or (for/first ([d (in-list (attribute definitions))]
                   #:when (equal? "revision" (syntax-e (definition-name d))))
         (parse-revision d
                         (attribute module-imports-table)
                         (attribute value-imports-table)))
       #'(begin))])

   
(define-for-syntax (parse-revision fun module-imports-table value-imports-table)
  (define (reject! srcs suffix)
    (raise-syntax-error 'pydrnlp/support/python-lang
                        "bad definition for revision function"
                        (definition-name fun)
                        #f
                        srcs
                        suffix))
  (match fun
    [(fun-definition name-str-stx _ formals async? maybe-return)
     (unless (null? formals)
       (reject! (map cdr formals)
                (format "\n  expected: 0 formal parameters\n  given: ~a"
                        (length formals))))
     (when async?
       (reject! (list async?) ";\n async modifier not allowed"))
     (unless maybe-return
       (reject! null ";\n unsupported syntax in function body"))
     (when (bad-return? maybe-return)
       (reject! (list (bad-return-stx maybe-return))
                (bad-return-message-suffix)))
     (define/syntax-parse return-expr
       (fixup-static-return
        maybe-return
        #:module-imports-table module-imports-table
        #:value-imports-table value-imports-table
        #:on-error reject!
        #:on-import-found
        (λ (src context)
          (let* ([ctx (last context)]
                 [revision-id
                  (datum->syntax #f (string->symbol
                                     (string-join
                                      src "." #:after-last ".revision"))
                                 ctx ctx)]
                 [revision-id
                  (syntax-local-lift-require
                   #`(rename #,(pydrnlp-raw-root-module-path src)
                             #,revision-id
                             revision)
                   revision-id)])
            #`(#,(wrap-expr/c
                  #'python-revision-function/c
                  revision-id
                  #:name revision-id))))))
     (define/syntax-parse name-from-src
       (datum->syntax name-str-stx 'revision name-str-stx name-str-stx))
     #'(begin (provide name-from-src)
              (define name-from-src
                (let ([v return-expr])
                  (λ () v))))]
    [_
     (reject! null
              (format "\n  expected: a function definition\n  given: a ~a definition"
                      (if (var-definition? fun)
                          "variable"
                          "class")))]))


(module indentation racket
  (provide determine-spaces)
  (define (determine-spaces drr pos)
    (define para
      (send drr position-paragraph pos))
    (define start-pos
      (send drr paragraph-start-position para))
    (define end-pos
      (send drr paragraph-end-position para))
    (for*/sum ([pos (in-range start-pos end-pos 1)]
               [ch (in-value (send drr get-character pos))]
               [num (in-value (case ch
                                [(#\space) 1]
                                [(#\tab) 4]
                                [else #f]))]
               #:break (not num))
      num)))

(module reader syntax/module-reader
  pydrnlp/support/python-lang
  #:read python-body-read
  #:read-syntax python-body-read-syntax
  #:whole-body-readers? #true
  #:info (λ (key default default-info)
           (define (nope)
             (default-info key default))
           (case key
             [(color-lexer)
              color-lexer]
             [(drracket:indentation)
              (dynamic-require indentation-mpi
                               'determine-spaces
                               nope)]
             [else
              (nope)]))
  
  (require "python-lang/read.rkt"
           racket/runtime-path
           (for-syntax racket/base)
           racket/port)

  (define-runtime-module-path-index indentation-mpi
    '(submod ".." indentation))
  
  (define (color-lexer in)
    (cond
      [(eof-object? (peek-char in))
       (values eof 'eof #f #f #f)]
      [else
       (define-values [line col pos]
         (port-next-location in))
       (define lexeme
         (port->string in))
       (values lexeme 'text #f
               pos
               (+ pos (string-length lexeme)))]))

  #|END module reader|#)

