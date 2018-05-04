#lang typed/racket/base

(require (submod "../annotation.rkt" typed)
         (submod "../signature.rkt" typed)
         )

;; Abstract base types
(provide Function-Doc*
         Class-Doc*
         (except-out (structs-out function-doc*
                                  class-doc*)
                     function-doc*
                     class-doc*)
         ;; Module-level types
         (structs-out modpath-doc
                      module-doc
                      module-section
                      ;; Common sub-components
                      docstring
                      docstring/comment/missing
                      ;; Essential documentation types
                      named-ann-doc
                      function-doc
                      predicate-doc
                      class-doc
                      annotation-class-doc
                      ))

;; Module-level types

(Struct modpath-doc ([name : full-mod-name]
                     [doc/error : (U module-doc
                                     exn:fail
                                     'ErrorDuringImport
                                     #f)]))

(Struct module-doc ([intro : docstring/comment/missing]
                    [public : module-section]
                    [private : module-section]))

(Struct module-section ([classes : (Listof class-doc)]
                        [annotation-classes : (Listof annotation-class-doc)]
                        [functions : (Listof function-doc)]
                        [predicates : (Listof predicate-doc)]
                        [named-annotations : (Listof named-ann-doc)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common sub-components

(Struct docstring ([string : String]))

(Struct docstring/comment/missing ([v : (U #f String docstring)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Essential documentation types

(Struct named-ann-doc ([docstring : docstring/comment/missing]
                       [name : full-value-name]
                       [declared-name : Symbol]
                       [annotation/singleton : (U 'singleton
                                                  Annotation)]))

;; Abstract base type for function-doc and predicate-doc

(Struct function-doc* ([docstring : docstring/comment/missing]
                       [local-name : Symbol]
                       [signature : doc-signature]))

(define-type Function-Doc* function-doc*)

(Struct function-doc function-doc* ())

(Struct predicate-doc function-doc* ())

;; Abstract base type for class-doc and annotation-class-doc

(Struct class-doc* ([docstring : docstring/comment/missing]
                    [local-name : Symbol]
                    [signature : doc-signature]))

(define-type Class-Doc* class-doc*)

(Struct class-doc class-doc* ())

(Struct annotation-class-doc class-doc*
        ([annotation-class-type : (U 'annotation-constructor
                                     'special-annotation)]))

