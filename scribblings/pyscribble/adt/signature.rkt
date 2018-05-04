#lang racket/base

(require racket/serialize
         racket/contract
         data/maybe
         "annotation.rkt"
         )

(provide (contract-out
          [struct doc-signature
            ([parameters (listof doc-parameter?)]
             [return (maybe/c annotation?)])]
          [struct doc-parameter
            ([formal-name string?]
             [annotation (maybe/c annotation?)]
             [kind (or/c 'positional-only
                         'positional-or-keyword
                         'var-positional
                         'keyword-only
                         'var-keyword)]
             [default (maybe/c string?)])]
          ))

(module* typed typed/racket/base
  (require (submod "annotation.rkt" typed)
           "typed-maybe.rkt")
  (require/typed/provide
   (submod "..")
   [#:struct doc-signature
    ([parameters : (Listof doc-parameter)]
     [return : (Maybe Annotation)])]
   [#:struct doc-parameter
    ([formal-name : String]
     [annotation : (Maybe Annotation)]
     [kind : (U 'positional-only
                'positional-or-keyword
                'var-positional
                'keyword-only
                'var-keyword)]
     [default : (Maybe String)])]
   #|END module* typed|#))


(serializable-struct doc-signature (parameters return)
  #:transparent)

(serializable-struct doc-parameter (formal-name
                                    annotation
                                    kind
                                    default)
  #:transparent)
