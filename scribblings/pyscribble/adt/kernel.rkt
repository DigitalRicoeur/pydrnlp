#lang racket/base

(require racket/serialize
         racket/contract
         )

;; These are things that use the same representation
;; across all passes, so they are defined using
;; serializable-struct.
;; The typed submodule serves as an adapter, plus 
;; provides some extra convinience forms.

(provide (contract-out
          [struct full-mod-name ([v (non-empty-listof symbol?)])]
          [struct full-value-name ([mod full-mod-name?]
                                   [name symbol?])]
          [struct atomic-ann
            ([datum (or/c #t #f 'None string? exact-integer?
                          (and/c inexact-real? rational?))])]
          [struct other-ann 
            ([string string?])]
          [struct class-ann
            ([name full-value-name?])]
          [struct function-ann 
            ([name full-value-name?])]
          [struct named-ann 
            ([name full-value-name?])]
          ))

(module* typed typed/racket/base
  (require (submod ".." typed-extras))
  (provide Kernel-Annotation
           (all-from-out (submod ".." typed-extras)))
  (require/typed/provide
   (submod "..")
   [#:struct full-mod-name ([v : (Nonempty-Listof Symbol)])]
   [#:struct full-value-name ([mod : full-mod-name]
                              [name : Symbol])]
   [#:struct atomic-ann ([datum : (U Boolean
                                     'None
                                     String
                                     Integer
                                     Inexact-Real)])]
   [#:struct other-ann ([string : String])]
   [#:struct class-ann ([name : full-value-name])]
   [#:struct function-ann ([name : full-value-name])]
   [#:struct named-ann ([name : full-value-name])])
  (define-type Kernel-Annotation (U atomic-ann
                                    other-ann
                                    class-ann
                                    function-ann
                                    named-ann))
  #|END module* typed|#)


(serializable-struct full-mod-name (v)
  ;; v : (listof symbol?)
  #:transparent)

(serializable-struct full-value-name (mod name)
  ;; mod full-mod-name?
  ;; name symbol?
  #:transparent)

(serializable-struct atomic-ann (datum)
  ;; datum  (or/c #t #f 'None string? number?)
  #:transparent)

(serializable-struct other-ann (string)
  #:transparent)

(serializable-struct class-ann (name)
  ;; name full-value-name?
  #:transparent)

(serializable-struct function-ann (name)
  ;; name full-value-name?
  #:transparent)

(serializable-struct named-ann (name)
  ;; name full-value-name?
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extras for typed
(module typed-extras typed/racket/base
  (require racket/provide-syntax
           (for-syntax racket/base
                       syntax/parse))
  (provide structs-out
           Nonempty-Listof
           Struct)
  (define-provide-syntax structs-out
    (syntax-parser
      [(_ s:id ...)
       #'(combine-out (struct-out s) ...)]))
  (define-type (Nonempty-Listof A)
    (Pairof A (Listof A)))
  (define-syntax Struct
    (syntax-parser
      #:literals {:}
      [(_ name:id ([field:id : type] ...))
       #'(struct name ([field : type] ...)
           #:transparent)]
      [(_ name:id super:id ([field:id : type] ...))
       #'(struct name super ([field : type] ...)
           #:transparent)]))
  #|END module typed-extras|#)
