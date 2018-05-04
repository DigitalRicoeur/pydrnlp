#lang racket

(require racket/serialize
         racket/provide-syntax
         (for-syntax racket/base
                     syntax/parse
                     ))

;; Do not require this module directly.
;; Instead, use the typed or contracted submodule.

;; This is organized so that the define-predicate
;; predicates can be used in the contracts.

(module* typed typed/racket/base
  (provide Proto-Content
           proto-content?
           )
  (require/typed/provide
   (submod "..")
   [#:struct text ([body : (Listof Proto-Content)])]
   [#:struct verbatim ([indent : Natural]
                       [string-body : (Listof String)])]
   [#:struct literal ([string-body : (Listof String)])]
   [#:struct b ([body : (Listof Proto-Content)])]
   [#:struct pyflow ([body : (Listof Proto-Content)])]
   )
  (define-type Proto-Content
    (U String
       (Listof Proto-Content)
       text
       verbatim
       literal
       b
       pyflow
       ))
  (define-predicate proto-content? Proto-Content)
  #|END module+ typed|#)
(module* contracted racket/base
  (require racket/contract
           (submod "..")
           (only-in (submod ".." typed)
                    proto-content?))
  (provide proto-content?
           (contract-out
            [struct text ([body (listof proto-content?)])]
            [struct verbatim ([indent natural-number/c]
                              [string-body (listof string?)])]
            [struct literal ([string-body (listof string?)])]
            [struct b ([body (listof proto-content?)])]
            [struct pyflow ([body (listof proto-content?)])]
            ))
  #|END module* contracted|#)

(provide (structs-out text
                      literal
                      verbatim
                      b
                      pyflow
                      ))

(define-syntax Struct
  (syntax-parser
    [(_ name:id (field:id ...))
     #'(serializable-struct name (field ...)
         #:transparent)]))

(define-provide-syntax structs-out
    (syntax-parser
      [(_ s:id ...)
       #'(combine-out (struct-out s) ...)]))

(Struct text (body))

(Struct verbatim (indent string-body))

(Struct literal (string-body))

(Struct b (body))

(Struct pyflow (body))

