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

(module reader syntax/module-reader
  pydrnlp/support/python-lang
  #:read python-body-read
  #:read-syntax python-body-read-syntax
  #:whole-body-readers? #true
  
  (require racket/match
           python-tokenizer)

  ;; FIXME python-tokenizer is based on
  ;;   python2, not python3, but seems to work for now.
  (define (python-body-read in)
    (map syntax->datum
         (python-body-read-syntax #f in)))
  
  (define (python-body-read-syntax src in)
    (for/list ([token (generate-tokens in)])
      (match-define (list type
                          lexeme
                          (list start-ln start-col)
                          (list end-ln end-col)
                          current-line-str)
        token)
      ;; TODO: convert lexeme to datum (e.g. un-escape strings)
      (define loc
        ;; TODO: position span
        (vector-immutable src start-ln start-col #f #f))
      (define (->syntax datum)
        ;; ?? current-line-str as property ??
        (datum->syntax #f datum loc))
      (->syntax (list (->syntax type)
                      (->syntax lexeme)))))
  
  #|END module reader|#)
#|
(begin-for-syntax
  (define-literal-set token-type-literals
    #:datum-literals {NAME NUMBER STRING OP COMMENT
                           NL NEWLINE DEDENT INDENT
                           ERRORTOKEN ENDMARKER}
    ())
  (define-splicing-syntax-class dotted-name
    #:literal-sets {token-type-literals}
    (pattern (~seq (NAME :str)
                   (~seq (OP ".") (NAME :str)) ...)))
  (define-splicing-syntax-class import-as-names-nodots
    #:literal-sets {token-type-literals}
    (pattern (~seq (NAME :str)
                   (~optional (~seq (NAME "as") 
  (define-splicing-syntax-class import-stmnt
    #:literal-sets {token-type-literals}
    (pattern (~seq (NAME "import")
                   (NAME mod:str)
                   (~seq (OP ".") (NAME mod:str)) ...
                   
  #|END begin-for-syntax|#)
|#
