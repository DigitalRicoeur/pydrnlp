#lang racket

(require racket/serialize
         racket/contract
         "../annotation.rkt"
         "../signature.rkt"
         "../proto-content.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide export-doc?
         function-doc*?
         class-doc*?
         (contract-out
          [export-doc-name
           (-> export-doc? full-value-name?)]
          [export-doc-info
           (-> export-doc? proto-content?)]
          [struct (named-ann-doc export-doc)
            ([name full-value-name?]
             [info proto-content?]
             [annotation/singleton (or/c 'singleton
                                         annotation?)])]
          [function-doc*-signature
           (-> function-doc*? doc-signature?)]
          [struct (function-doc function-doc*)
            ([name full-value-name?]
             [info proto-content?]
             [signature doc-signature?])]
          [struct (predicate-doc function-doc*)
            ([name full-value-name?]
             [info proto-content?]
             [signature doc-signature?])]
          [class-doc*-signature
           (-> function-doc*? doc-signature?)]
          [struct (class-doc class-doc*)
            ([name full-value-name?]
             [info proto-content?]
             [signature doc-signature?])]
          [struct (annotation-class-doc class-doc*)
            ([name full-value-name?]
             [info proto-content?]
             [signature doc-signature?]
             [annotation-class-type
              (or/c 'annotation-constructor
                    'special-annotation)])]
          ))
(module* private-for-typed #f
  ;; abstract struct types
  (provide (struct-out export-doc)
           (struct-out function-doc*)
           (struct-out class-doc*)
           ))
(module* typed typed/racket/base
  (require (submod "../annotation.rkt" typed)
           (submod "../signature.rkt" typed)
           (submod "../proto-content.rkt" typed))
  (require/typed
   (submod ".." private-for-typed)
   [#:struct export-doc ([name : full-value-name]
                         [info : Proto-Content])]
   [#:struct (function-doc* export-doc)
    ([signature : doc-signature])]
   [#:struct (class-doc* export-doc)
    ([signature : doc-signature])])
  (define-type Export-Doc export-doc)
  (define-type Function-Doc* function-doc*)
  (define-type Class-Doc* class-doc*)
  ;; Exports:
  (provide Export-Doc
           export-doc?
           export-doc-name
           export-doc-info
           Function-Doc*
           function-doc*?
           function-doc*-signature
           Class-Doc*
           class-doc*?
           class-doc*-signature
           )
  (require/typed/provide
   (submod "..")
   [#:struct (named-ann-doc export-doc)
    ([annotation/singleton : (U 'singleton
                                Annotation)])]
   [#:struct (function-doc function-doc*) ()]
   [#:struct (predicate-doc function-doc*) ()]
   [#:struct (class-doc class-doc*) ()]
   [#:struct (annotation-class-doc class-doc*)
    ([annotation-class-type :  (U 'annotation-constructor
                                  'special-annotation)])])
  #|END module* typed|#)

(define-syntax Struct
  (syntax-parser
    [(_ name:id (field:id ...))
     #'(serializable-struct name (field ...)
         #:transparent)]
    [(_ name:id super:id (field:id ...))
     #'(serializable-struct name super (field ...)
         #:transparent)]))

(Struct export-doc (name info)) ;; Abstract!
        
(Struct named-ann-doc export-doc (annotation/singleton))

(Struct function-doc* export-doc (signature)) ;; Abstract!!

(Struct function-doc function-doc* ())

(Struct predicate-doc function-doc* ())

(Struct class-doc* export-doc (signature)) ;; Abstract!!

(Struct class-doc class-doc* ())

(Struct annotation-class-doc class-doc* (annotation-class-type))

