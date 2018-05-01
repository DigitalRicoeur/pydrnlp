#lang racket

(require data/maybe
         racket/serialize
         adjutor
         )

(require-provide "annotation-adt.rkt")

(provide value-doc?
         (contract-out
          [struct modpath-doc
            ([name full-mod-name?]
             [doc/error (or/c module-doc?
                              'ErrorDuringImport
                              #f)])]
          [struct module-doc
            ([intro docstring/comment/missing?]
             [public module-section?]
             [private module-section?])]
          [struct module-section
            ([classes
              (listof
               (and/c class-doc?
                      (not/c class-doc-is-annotation-class?)))]
             [annotation-classes
              (listof
               (and/c class-doc?
                      class-doc-is-annotation-class?))]
             [functions
              (listof
               (and/c function-doc?
                      (not/c function-doc-is-predicate?)))]
             [predicates
              (listof
               (and/c function-doc?
                      function-doc-is-predicate?))]
             [named-annotations
              (listof named-ann-doc?)])]
          [struct docstring ([string string?])]
          [struct docstring/comment/missing
            ([v (or/c #f string? docstring?)])]
          [struct signature-doc
            ; check error
            ([parameters (listof parameter-doc?)]
             [return (maybe/c annotation?)])]
          [struct parameter-doc
            ([formal-name string?]
             [annotation (maybe/c annotation?)]
             ; check kind
             [kind (or/c 'positional-only
                         'positional-or-keyword
                         'var-positional
                         'keyword-only
                         'var-keyword)]
             [default (maybe/c string?)])]
          ;;;;
          [struct (function-doc value-doc)
            ([docstring docstring/comment/missing?]
             [local-name symbol?]
             [signature signature-doc?]
             [is-predicate? boolean?])]
          [struct (named-ann-doc value-doc)
            ([docstring docstring/comment/missing?]
             [name full-value-name?]
             [declared-name symbol?]
             [annotation/singleton (or/c 'singleton annotation?)])]
          [struct (class-doc value-doc)
            ([docstring docstring/comment/missing?]
             [local-name symbol?]
             [signature signature-doc?]
             [is-annotation-class?
              (or/c #f 'annotation-constructor 'special-annotation)])]
          ))

(serializable-struct modpath-doc (name doc/error)
  #:transparent)

(serializable-struct module-doc (intro public private)
  ;; consider a module-doc* for things like
  ;; posix-seconds, path, md5, warnings
  #:transparent)

(serializable-struct module-section (classes
                                     annotation-classes
                                     functions
                                     predicates
                                     named-annotations)
                                     
  #:transparent)

(serializable-struct docstring (string)
  #:transparent)

(serializable-struct docstring/comment/missing (v)
  #:transparent)

(serializable-struct signature-doc (parameters return)
  #:transparent)

(serializable-struct parameter-doc (formal-name annotation kind default)
  #:transparent)

(serializable-struct value-doc (docstring)
  ;; Abstract!
  #:transparent)

(serializable-struct named-ann-doc value-doc (name
                                              declared-name
                                              annotation/singleton)
  #:transparent)

(serializable-struct function-doc value-doc (local-name
                                             signature
                                             is-predicate?)
  #:transparent)
  
(serializable-struct class-doc value-doc (local-name
                                          signature
                                          is-annotation-class?)
  #:transparent)




