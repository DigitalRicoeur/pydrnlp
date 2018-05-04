#lang racket/base

(require racket/serialize
         racket/contract
         "export.rkt"
         "../proto-content.rkt"
         (only-in "../kernel.rkt" full-mod-name?)
         )

;; TODO: modpath-doc once I decide on doc/error contract

(provide (contract-out
          [struct modpath-doc
            ([name full-mod-name?]
             [doc/error (or/c module-doc?
                              module-error?)])]
          [struct module-error
            ([type (or/c 'not-found
                         'ErrorDuringImport
                         'from-json
                         )]
             [message string?])]
          [struct module-doc
            ([info proto-content?]
             [public module-section?]
             [private module-section?])]
          [struct module-section
            ([classes (listof class-doc?)]
             [annotation-classes (listof annotation-class-doc?)]
             [functions (listof function-doc?)]
             [predicates (listof predicate-doc?)]
             [named-annotations (listof named-ann-doc?)])]
          ))
(module* typed typed/racket
  (require (only-in (submod "../kernel.rkt" typed)
                    full-mod-name)
           (submod "../proto-content.rkt" typed)
           (submod "export.rkt" typed))
  (require/typed/provide
   (submod "..")
   [#:struct modpath-doc
    ([name : full-mod-name]
     [doc/error : (U module-doc
                     module-error)])]
   [#:struct module-error
    ([type : (U 'not-found
                'ErrorDuringImport
                'from-json
                )]
     [message : String])]
   [#:struct module-doc
    ([info : Proto-Content]
     [public : module-section]
     [private : module-section])]
   [#:struct module-section
    ([classes : (Listof class-doc)]
     [annotation-classes : (Listof annotation-class-doc)]
     [functions : (Listof function-doc)]
     [predicates : (Listof predicate-doc)]
     [named-annotations : (Listof named-ann-doc)])])
  #|END module* typed|#)

(serializable-struct modpath-doc (name doc/error)
  #:transparent)

(serializable-struct module-error (type message)
  #:transparent)

(serializable-struct module-doc (info public private)
  #:transparent)

(serializable-struct module-section (classes
                                     annotation-classes
                                     functions
                                     predicates
                                     named-annotations)
  #:transparent)

