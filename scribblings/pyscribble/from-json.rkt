#lang racket

(require "adt.rkt"
         "from-json-utils.rkt"
         json
         syntax/parse
         data/maybe
         (for-syntax syntax/parse
                     ))

(define (parse-file pth)
  (call-with-input-file* pth
    (λ (in) (map jsexpr->modpath-doc
                 (read-json in)))))

(module+ main
  (map parse-file json-paths))
#|
(module+ main
  (for-each parse-file json-paths)
  'done)
  
#;
(module+ main
  (for/list ([pth (cdr json-paths)]
             [i (in-naturals)])
    (println i)
    (match-define-values {_ name _}
      (split-path pth))
    (cons (path->string name)
          (with-handlers ([exn:fail? exn-message])
            (parse-file pth)
            'parsed))))
|#

(define-syntax-class hash
  #:description "a hash table"
  (pattern h:expr
           #:do [(define unwrapped
                   (syntax->datum #'h))]
           #:fail-unless (hash? unwrapped)
           "not a hash table"
           #:with ([k v] ...)
           (for/list ([{k v} (in-hash unwrapped)])
             (list k v))))

(define-syntax ~hash-object
  (pattern-expander
   (syntax-parser
     [(_ [key:id rhs]
         ...
         (~optional (~seq #:post post-pat ...)
                    #:defaults ([(post-pat 1) null])))
      #`(~and
         it:hash
         (~parse ((~alt (~once (~seq [(~datum key)
                                      ~!
                                      rhs]))
                        ...
                        (~seq [(~and _:id
                                     (~not (~or* (~datum key)
                                                 ...)))
                               _:expr]))
                  (... ...))
                 #'([it.k it.v] (... ...)))
         post-pat ...)])))

(define-syntax-class modpath
  #:description #f
  #:attributes {parsed}
  (pattern mod:str
           #:attr parsed
           (string->full-mod-name (syntax->datum #'mod))))

(define-syntax ~full-value-name-jsexpr
  (pattern-expander
   (syntax-parser
     [(_ bind:id
         [key:id rhs]
         ...
         (~optional (~seq #:post post-pat ...)
                    #:defaults ([(post-pat 1) null])))
      #`(~hash-object
         [module mod:modpath]
         [name n:str]
         [key rhs] ...
         #:post
         (~do (define bind
                (full-value-name
                 (attribute mod.parsed)
                 (string->symbol (syntax->datum #'n)))))
         post-pat ...)])))

(define-syntax-class annotation
  #:description #f ;"annotation"
  #:attributes {parsed}
  (pattern (~describe "\"True\" annotation"
                      ("True" ~!))
           #:attr parsed (atomic-ann #t))
  (pattern (~describe "\"False\" annotation"
                      ("False" ~!))
           #:attr parsed (atomic-ann #f))
  (pattern (~describe "\"None\" annotation"
                      ("None" ~!))
           #:attr parsed (atomic-ann 'None))
  (pattern (~or* (~describe "string annotation"
                            ("string" ~! it:str))
                 (~describe "number annotation"
                            ("number" ~! it:number)))
           #:attr parsed (atomic-ann (syntax->datum #'it)))
  (pattern (~describe "other annotation"
                      ("other" ~! it:str))
           #:attr parsed (other-ann (syntax->datum #'it)))
  (pattern (~describe "class annotation"
                      ("class" ~! (~full-value-name-jsexpr name)))
           #:attr parsed (class-ann name))
  (pattern (~describe "function annotation"
                      ("function" ~! (~full-value-name-jsexpr name)))
           #:attr parsed (function-ann name))
  (pattern (~describe "named annotation use"
                      ("named-annotation"
                       ~!
                       (~full-value-name-jsexpr name)))
           #:attr parsed (named-ann name))
  ;; The rest are recursive
  (pattern (~describe "annotation-constructor annotation"
                      ("annotation-constructor"
                       ~!
                       (~full-value-name-jsexpr
                        name
                        [args (args:annotation ...)])))
           #:attr parsed
           (constructor-ann name (attribute args.parsed)))
  (pattern (~describe "dictionary annotation"
                      ("dict" ~! ([keys:annotation
                                   vals:annotation]
                                  ...)))
           #:attr parsed
           (dict-ann (map cons
                          (attribute keys.parsed)
                          (attribute vals.parsed))))
  (pattern (~describe "list annotation"
                      ("list" ~! (content:annotation ...)))
           #:attr parsed
           (list-ann (attribute content.parsed)))
  (pattern (~describe "tuple annotation"
                      ("tuple" ~! (content:annotation ...)))
           #:attr parsed
           (tuple-ann (attribute content.parsed)))
  #|END annotation|#)


(define-syntax-class maybe-annotation
  #:description "annotation or #f"
  #:attributes {parsed}
  (pattern #f
           #:attr parsed (nothing))
  (pattern ann:annotation
           #:attr parsed
           (just (attribute ann.parsed))))

(define-syntax-class maybe-string
  #:description "string or #f"
  #:attributes {parsed}
  (pattern #f
           #:attr parsed (nothing))
  (pattern s:str
           #:attr parsed
           (just (syntax->datum #'s))))

(define-syntax-class parameter-js
  #:description "parameter"
  #:attributes {parsed}
  (pattern (~hash-object
            [name formal-name-stx:str]
            [annotation maybe-ann-jsexpr:maybe-annotation]
            [kind (~and kind-stx
                        (~or* "POSITIONAL_ONLY"
                              "POSITIONAL_OR_KEYWORD"
                              "VAR_POSITIONAL"
                              "KEYWORD_ONLY"
                              "VAR_KEYWORD"
                              #f))]
            [default default:maybe-string])
           #:do [(define kind (syntax->datum #'kind-stx))
                 (define formal-name (syntax->datum #'formal-name-stx))]
           #:fail-unless kind
           "unknown parameter kind (from Python level)"
           #:attr parsed
           (parameter-doc formal-name
                          (attribute maybe-ann-jsexpr.parsed)
                          (case kind
                            [("POSITIONAL_ONLY") 'positional-only]
                            [("POSITIONAL_OR_KEYWORD") 'positional-or-keyword]
                            [("VAR_POSITIONAL") 'var-positional]
                            [("KEYWORD_ONLY") 'keyword-only]
                            [("VAR_KEYWORD") 'var-keyword])
                          (attribute default.parsed))))
  
(define-syntax-class signature-doc-js
  #:description "signature"
  #:attributes {parsed}
  (pattern (~hash-object
            [parameters (params:parameter-js ...)]
            [return return:maybe-annotation])
           #:attr parsed
           (signature-doc
            (attribute params.parsed)
            (attribute return.parsed))))

(define-syntax-class missing-docstring
  ;#:description 
  #:attributes {parsed}
  (pattern #f
           #:attr parsed (docstring/comment/missing #f)))

(define-syntax-class docstring/comment/missing-js
  ;#:description 
  #:attributes {parsed}
  (pattern m:missing-docstring
           #:attr parsed (attribute m.parsed))
  (pattern ("comments" ~! v:str)
           #:attr parsed
           (docstring/comment/missing
            (syntax->datum #'v)))
  (pattern ("docstring" ~! v:str)
           #:attr parsed
           (docstring/comment/missing
            (docstring
             (syntax->datum #'v)))))

(define-syntax-class maybe-named-ann-docstring
  ;#:description 
  #:attributes {parsed}
  (pattern m:missing-docstring
           #:attr parsed (attribute m.parsed))
  (pattern lit:str
           #:attr parsed
           (docstring/comment/missing
            (docstring
             (syntax->datum #'lit)))))

(define-syntax-class named-named-annotation-value-js
  ;#:description 
  #:attributes {parsed}
  (pattern ("singleton" ~!)
           #:attr parsed 'singleton)
  (pattern ann:annotation
           #:attr parsed (attribute ann.parsed)))

(define-syntax-class named-annotation-js
  #:description "named annotation definition"
  #:attributes {parsed}
  (pattern (~full-value-name-jsexpr
            name
            [value (~hash-object
                    [name declared-name-str-stx:str]
                    [docstring doc:maybe-named-ann-docstring]
                    [value value:named-named-annotation-value-js])])

           #:attr parsed
           (named-ann-doc (attribute doc.parsed)
                          name
                          (string->symbol
                           (syntax->datum
                            #'declared-name-str-stx))
                          (attribute value.parsed))))

(define-syntax ~fun/class-jsexpr
  (pattern-expander
   (syntax-parser
     [(_ (~alt (~once (~seq #:docstring doc:id))
               (~once (~seq #:local-name local-name:id))
               (~once (~seq #:signature sig:id)))
         ...
         [key:id rhs]
         ...
         (~optional (~seq #:post post-pat ...)
                    #:defaults ([(post-pat 1) null])))
      #`(~hash-object
         [name local-name-str-stx:str]
         [signature sig-pat:signature-doc-js]
         [text doc-pat:docstring/comment/missing-js]
         [key rhs] ...
         #:post
         (~do (define doc
                (attribute doc-pat.parsed))
              (define local-name
                (string->symbol
                 (syntax->datum #'local-name-str-stx)))
              (define sig
                (attribute sig-pat.parsed)))
         post-pat ...)])))

(define-syntax-class function-js
  #:description "function"
  #:attributes {parsed}
  (pattern (~fun/class-jsexpr
            #:docstring docstring
            #:local-name local-name
            #:signature signature
            [ispredicate (~and pred (~or* #t #f))])
           #:attr parsed
           (function-doc docstring
                         local-name
                         signature
                         (syntax->datum #'pred))))

(define-syntax-class class-js
  #:description "class"
  #:attributes {parsed}
  (pattern (~fun/class-jsexpr
            #:docstring docstring
            #:local-name local-name
            #:signature signature
            [isSpecialAnnotationClass
             (~and (~or* "special-annotation"
                         "annotation-constructor"
                         #f)
                   raw-is-ann)])
           #:attr parsed
           (class-doc docstring
                      local-name
                      signature
                      (let ([ann-datum (syntax->datum #'raw-is-ann)])
                        (and ann-datum
                             (string->symbol ann-datum))))))


(define (jsexpr->modpath-doc arg)
  (syntax-parse (datum->syntax #f arg)
    #:context 'jsexpr->modpath-doc
    [(mod:modpath #f)
     (modpath-doc (attribute mod.parsed) #f)]
    [(mod:modpath "ErrorDuringImport")
     (modpath-doc (attribute mod.parsed) 
                  'ErrorDuringImport)]
    [(mod:modpath
      (~hash-object
       [text intro:docstring/comment/missing-js]
       [functions (funcs:function-js ...)]
       [classes (classes:class-js ...)]
       [named-annotations (n-a*:named-annotation-js ...)]))
     (match-let
         ([(app^ (partition-by (private? function-doc-local-name))
                 (app^ (partition-by function-doc-is-predicate?)
                       private-predicates
                       private-functions)
                 (app^ (partition-by function-doc-is-predicate?)
                       public-predicates
                       public-functions))
           (attribute funcs.parsed)]
          [(app^ (partition-by (private? named-ann-doc-declared-name))
                 private-named-annotations
                 public-named-annotations)
           (attribute n-a*.parsed)]
          [(app^ (partition-by (private? class-doc-local-name))
                 (app^ (partition-by class-doc-is-annotation-class?)
                       private-annotation-classes
                       private-classes)
                 (app^ (partition-by class-doc-is-annotation-class?)
                       public-annotation-classes
                       public-classes))
           (attribute classes.parsed)])
       (modpath-doc
        (attribute mod.parsed)
        (module-doc
         (attribute intro.parsed)
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
         
  
