#lang racket/base

(provide (except-out (all-from-out racket/base)
                     #%module-begin)
         (rename-out [modbegin
                      #%module-begin]))

(require syntax/parse/define
         (for-syntax racket/base))

(define-syntax-parser modbegin
  [(_ form ...) ;; #%plain-module-begin
   #'(#%module-begin '(form ...))])

;; PEP 257

;; A docstring is a string literal that occurs as
;; the first statement in a module, function, class,
;; or method definition.

;; String literals occurring immediately after
;; a simple assignment at the top level of a module,
;; class, or __init__ method are called "attribute docstrings".

;; String literals occurring immediately after
;; another docstring are called "additional docstrings".

;; see also: PEP 258
;; Guido: "the reason for PEP 258's rejection is not that
;;         it is invalid, but that it's not slated for
;;         stdlib inclusion"
;; https://bugs.python.org/issue35651

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

