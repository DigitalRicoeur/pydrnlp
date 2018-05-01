#lang racket

(require "adt.rkt"
         "from-json-utils.rkt"
         racket/serialize
         data/maybe
         (rename-in (only-in data/functor map)
                    [map fmap])
         json
         (for-syntax syntax/parse
                     ))

;; use syntax-parse.rkt, not this (this is broken)

(module+ main
  (for ([pth json-paths])
    (call-with-input-file* pth
      (λ (in) (map jsexpr->modpath-doc
                   (read-json in))))
    (displayln pth)))




(define-match-expander full-value-name-jsexpr
  (syntax-parser
    [(_ mod:id name:id pat:expr ...)
     #'(hash-table ['module (app string->full-mod-name
                                 mod)]
                   ['name (app string->symbol
                               name)]
                   pat ...
                   [_ _] (... ...))]))

(define jsexpr->annotation
  (match-lambda
    ;; atomic
    [`("True")
     (atomic-ann #t)]
    [`("False")
     (atomic-ann #f)]
    [`("None")
     (atomic-ann 'None)]
    [(or `("string" ,it)
         `("number" ,it))
     (atomic-ann it)]
    ;; other
    [`("other" ,str)
     (other-ann str)]
    ;; class + function
    [`("class" ,rhs)
     (match-let ([(full-value-name-jsexpr mod name) rhs])
       (class-ann (full-value-name mod name)))]
    [`("function" ,rhs)
     (match-let ([(full-value-name-jsexpr mod name) rhs])
       (function-ann (full-value-name mod name)))]
    ;; special
    [`("named-annotation" ,rhs)
     (match-let ([(full-value-name-jsexpr mod name) rhs])
       (named-ann (full-value-name mod name)))]
    [`("annotation-constructor" ,rhs)
     (match-let ([(full-value-name-jsexpr
                   mod name
                   ['args (list args ...)])
                  rhs])
       (constructor-ann (full-value-name mod name)
                        (map jsexpr->annotation args)))]
    ;; structural
    [`("dict" ,rhs)
     (match-let ([(list (list keys vals) ...) rhs])
       (dict-ann (for/list ([k (in-list keys)]
                            [v (in-list vals)])
                   (cons (jsexpr->annotation k)
                         (jsexpr->annotation v)))))]
    [`("list" ,rhs)
     (match-let ([(list content ...) rhs])
       (list-ann (map jsexpr->annotation
                      content)))]
    [`("tuple" ,rhs)
     (match-let ([(list content ...) rhs])
       (tuple-ann (map jsexpr->annotation
                       content)))]
    [bad
     (error 'jsexpr->annotation
            "no match\n  given...:\n   ~e\n"
            bad)]))

(define jsexpr->signature-doc
  (match-lambda
    [(and (or "TypeError" "ValueError") it)
     (error 'jsexpr->signature-doc it)]
    [(hash-table
      ['parameters
       (list (and param-jsexpr*
                  (hash-table
                   ['name formal-name*]
                   ['annotation (app false->maybe maybe-ann-jsexpr*)]
                   ['kind (and (or "POSITIONAL_ONLY"
                                   "POSITIONAL_OR_KEYWORD"
                                   "VAR_POSITIONAL"
                                   "KEYWORD_ONLY"
                                   "VAR_KEYWORD"
                                   #f)
                               kind*)]
                   ['default (app false->maybe default*)]
                   [_ _] ...))
             ...)]
      ['return (app false->maybe maybe-return-jsexpr)]
      [_ _] ...)
     (signature-doc
      (for/list ([formal-name (in-list formal-name*)]
                 [maybe-ann-jsexpr (in-list maybe-ann-jsexpr*)]
                 [kind (in-list kind*)]
                 [default (in-list default*)])
        (parameter-doc formal-name
                       (fmap jsexpr->annotation
                             maybe-ann-jsexpr)
                       (case kind
                         [("POSITIONAL_ONLY") 'positional-only]
                         [("POSITIONAL_OR_KEYWORD") 'positional-or-keyword]
                         [("VAR_POSITIONAL") 'var-positional]
                         [("KEYWORD_ONLY") 'keyword-only]
                         [("VAR_KEYWORD") 'var-keyword]
                         [(#f) (error 'jsexpr->signature-doc
                                      "unknown parameter kind")])
                       default))
      (fmap jsexpr->annotation
            maybe-return-jsexpr))]
    [bad
     (error 'jsexpr->signature-doc
            "no match\n  given...:\n   ~e\n"
            bad)]))


  

(define jsexpr->named-ann-doc
  (match-lambda
    [(full-value-name-jsexpr
      mod name
      ['value (hash-table
               ['name (app string->symbol declared-name)]
               ['docstring (or (and #f (app docstring/comment/missing
                                            doc))
                               (? string? (app docstring-docstring doc)))]
               ['value raw-value]
               [_ _] ...)])
     (named-ann-doc doc
                    (full-value-name mod name)
                    declared-name
                    (if (equal? '("singleton") raw-value)
                        'singleton
                        (jsexpr->annotation raw-value)))]
    [bad
     (error 'jsexpr->named-ann-doc
            "no match\n  given...:\n   ~e\n"
            bad)]))

(define (docstring-docstring str)
  (docstring/comment/missing
   (docstring str)))

(define-match-expander docstring/comment/missing-jsexpr
  (syntax-parser
    [(_ doc:id)
     #`(or (and #f (app docstring/comment/missing doc))
           `("comments" ,(? string? (app docstring/comment/missing
                                         doc)))
           `("docstring" ,(? string? (app docstring-docstring doc))))]))

(define-match-expander fun/class-jsexpr
  (syntax-parser
    [(_ (~alt (~once (~seq #:docstring doc:id))
              (~once (~seq #:local-name local-name:id))
              (~once (~seq #:signature signature:id)))
        ...
        pat ...)
     #`(hash-table
        ['name (app string->symbol local-name)]
        ['signature (app jsexpr->signature-doc signature)]
        ['text (docstring/comment/missing-jsexpr doc)]
        pat ...
        [_ _] (... ...))]))


(define jsexpr->function-doc
  (match-lambda
    [(fun/class-jsexpr
      #:docstring docstring
      #:local-name local-name
      #:signature signature
      ['ispredicate (? boolean? is-predicate?)])
     (function-doc docstring
                   local-name
                   signature
                   is-predicate?)]))
                         

(define jsexpr->class-doc
  (match-lambda
    [(fun/class-jsexpr
      #:docstring docstring
      #:local-name local-name
      #:signature signature
      ['isSpecialAnnotationClass
       (and (or "special-annotation"
                "annotation-constructor"
                #f)
            raw-is-ann)])
     (class-doc docstring
                local-name
                signature
                (and raw-is-ann
                     (string->symbol raw-is-ann)))]))



(define jsexpr->modpath-doc
  (match-lambda
    [`(,modpath #f)
     (modpath-doc modpath #f)]
    [`(,modpath "ErrorDuringImport")
     (modpath-doc modpath 'ErrorDuringImport)]
    [`(,modpath
       ,(hash-table
         ['text text]
         ['functions functions]
         ['named-annotations named-annotations]
         ['classes classes]
         [_ _] ...))
     (match-let
         ([(docstring/comment/missing-jsexpr intro)
           text]
          [(app^ (partition-map (private? function-doc-local-name)
                                jsexpr->function-doc)
                 (app^ (partition-by function-doc-is-predicate?)
                       private-predicates
                       private-functions)
                 (app^ (partition-by function-doc-is-predicate?)
                       public-predicates
                       public-functions))
           functions]
          [(app^ (partition-map (private? named-ann-doc-declared-name)
                                jsexpr->named-ann-doc)
                 private-named-annotations
                 public-named-annotations)
           named-annotations]
          [(app^ (partition-map (private? class-doc-local-name)
                                jsexpr->class-doc)
                 (app^ (partition-by class-doc-is-annotation-class?)
                       private-annotation-classes
                       private-classes)
                 (app^ (partition-by class-doc-is-annotation-class?)
                       public-annotation-classes
                       public-classes))
           classes])
       (modpath-doc
        modpath
        (module-doc
         intro
         (module-section public-classes
                         public-annotation-classes
                         public-functions
                         public-predicates
                         public-named-annotations)
         (module-section private-classes
                         private-annotation-classes
                         private-functions
                         private-predicates
                         private-named-annotations))))]))


