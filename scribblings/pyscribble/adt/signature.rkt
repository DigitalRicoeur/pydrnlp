#lang racket/base

(require racket/serialize
         racket/contract
         "annotation.rkt"
         )

;; TODO: use maybe if unsafe-require/typed is ok
;; (or if you give up on the whole types thing)

(provide (contract-out
          [struct doc-signature
            ([parameters (listof doc-parameter?)]
             [return (or/c #f annotation?)])]
          [struct doc-parameter
            ([formal-name string?]
             [annotation (or/c #f annotation?)]
             [kind (or/c 'positional-only
                                  'positional-or-keyword
                                  'var-positional
                                  'keyword-only
                                  'var-keyword)]
             [default (or/c #f string?)])]
          ))

(module* typed typed/racket/base
  (require (submod "annotation.rkt" typed))
  (require/typed/provide
   (submod "..")
   [#:struct doc-signature
    ([parameters : (Listof doc-parameter)]
     [return : (U #f Annotation)])]
   [#:struct doc-parameter
    ([formal-name : String]
                       [annotation : (U #f Annotation)]
                       [kind : (U 'positional-only
                                  'positional-or-keyword
                                  'var-positional
                                  'keyword-only
                                  'var-keyword)]
                       [default : (U #f String)])]
   #|END module* typed|#))


(serializable-struct doc-signature (parameters return)
  #:transparent)

(serializable-struct doc-parameter (formal-name
                                    annotation
                                    kind
                                    default)
  #:transparent)
