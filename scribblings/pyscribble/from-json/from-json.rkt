#lang racket

(require json
         syntax/parse
         adjutor
         data/maybe
         "../adt/pass0.rkt"
         (for-syntax syntax/parse
                     ))

(provide (contract-out
          [jsexpr->pass0
           (-> (list/c string? jsexpr?)
               modpath-doc?)]
          ))

;; Functional support
;; (This needs to come first b/c of app^.)

(def
  [((partition-by pred?) lst)
   (partition pred? lst)]
  [((private? get-sym) it)
   (regexp-match? #rx"^_" (symbol->string (get-sym it)))]
  [function-doc*-private?
   (private? function-doc*-local-name)]
  [class-doc*-private?
   (private? class-doc*-local-name)]
  [named-ann-doc-private?
   (private? named-ann-doc-declared-name)])

(define (string->full-mod-name str)
  (full-mod-name (map string->symbol
                      (regexp-split #rx"\\." str))))

(define-match-expander app^
  (syntax-parser
    [(_ pred:expr pat ...)
     #`(app #,(syntax-local-lift-expression #'pred)
            pat
            ...)]))


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;       ;;;                                    ;                           
;     ;;                                       ;;                          
;   ;;;;;;; ;; ;;;   ;;;   ; ;; ;;          ;;;;;     ;;     ;;;    ;; ;   
;     ;;    ;;;     ;   ;  ;; ;; ;             ;;   ;;  ;   ;   ;   ;;; ;  
;     ;;    ;;      ;   ;  ;; ;; ;;            ;;    ;      ;   ;   ;;  ;; 
;     ;;    ;;     ;;   ;; ;; ;; ;;            ;;     ;;   ;;   ;;  ;;  ;; 
;     ;;    ;;      ;   ;  ;; ;; ;;            ;;       ;;  ;   ;   ;;  ;; 
;     ;;    ;;      ;   ;  ;; ;; ;;            ;;   ;   ;   ;   ;   ;;  ;; 
;     ;;    ;;       ;;;   ;; ;; ;;            ;;    ;;;     ;;;    ;;  ;; 
;                                              ;;                          
;                                              ;                           
;                                           ;;;                            
;                                                                          

  
(define (jsexpr->pass0 arg)
  (match-define (list (app string->full-mod-name
                           mod)
                      body)
    arg)
  (modpath-doc
   mod
   (with-handlers ([exn:fail? values])
     (syntax-parse (datum->syntax #f body)
       #:context 'jsexpr->modpath-doc
       [#f #f]
       ["ErrorDuringImport" 'ErrorDuringImport]
       [(~hash-object
         [text intro:docstring/comment/missing-js]
         [functions (funcs:function-js ...)]
         [classes (classes:class-js ...)]
         [named-annotations (n-a*:named-annotation-js ...)])
        (match-let
            ([(app^ (partition-by function-doc*-private?)
                    (app^ (partition-by predicate-doc?)
                          private-predicates
                          private-functions)
                    (app^ (partition-by predicate-doc?)
                          public-predicates
                          public-functions))
              (attribute funcs.parsed)]
             [(app^ (partition-by named-ann-doc-private?)
                    private-named-annotations
                    public-named-annotations)
              (attribute n-a*.parsed)]
             [(app^ (partition-by class-doc*-private?)
                    (app^ (partition-by annotation-class-doc?)
                          private-annotation-classes
                          private-classes)
                    (app^ (partition-by annotation-class-doc?)
                          public-annotation-classes
                          public-classes))
              (attribute classes.parsed)])
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
                           private-named-annotations)))]))))
         

;                                                  
;                                                  
;                                                  
;                                                  
;                            ;;                    
;                            ;;                    
;     ;;   ;     ;  ;; ;   ;;;;;;;    ;;   ;;   ;; 
;   ;;  ;   ;   ;   ;;; ;    ;;      ;  ;    ;  ;  
;    ;      ;   ;   ;;  ;;   ;;         ;;   ; ;   
;     ;;     ;  ;   ;;  ;;   ;;       ;;;;    ;    
;       ;;   ; ;    ;;  ;;   ;;      ;  ;;   ; ;   
;   ;   ;    ; ;    ;;  ;;    ;     ;;  ;;  ;   ;  
;    ;;;      ;     ;;  ;;     ;;;   ;;; ; ;;   ;; 
;             ;                                    
;            ;                                     
;          ;;                                      
;                                                  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hash-table "object" patterns
;; --------------------------------------------------------
;; The pattern-expanders must come behave strangely
;; if they aren't this high in the file.

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

(define-syntax ~hash-object•full-value-name
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

(define-syntax ~hash-object•fun/class
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outermost classes: function, class, and named annotation

(define-syntax-class named-annotation-js
  #:description "named annotation definition"
  #:attributes {parsed}
  (pattern (~hash-object•full-value-name
            name
            [value (~hash-object
                    [name declared-name-str-stx:str]
                    [docstring pre-doc:string-or-false]
                    [value value:named-annotation-value/singleton-js])])
           #:attr parsed
           (named-ann-doc (docstring/comment/missing
                           (let ([x (attribute pre-doc.parsed)])
                             (and x
                                  (docstring x))))
                          name
                          (string->symbol
                           (syntax->datum
                            #'declared-name-str-stx))
                          (attribute value.parsed))))


(define-syntax-class function-js
  #:description "function"
  #:attributes {parsed}
  (pattern (~hash-object•fun/class
            #:docstring docstring
            #:local-name local-name
            #:signature signature
            [ispredicate (~and pred (~or* #t #f))])
           #:attr parsed
           ((if (syntax->datum #'pred)
                predicate-doc
                function-doc)
            docstring
            local-name
            signature)))


(define-syntax-class class-js
  #:description "class"
  #:attributes {parsed}
  (pattern (~hash-object•fun/class
            #:docstring docstring
            #:local-name local-name
            #:signature signature
            [isSpecialAnnotationClass
             (~and (~or* "special-annotation"
                         "annotation-constructor"
                         #f)
                   raw-is-ann)])
           #:attr parsed
           (let ([ann-datum (syntax->datum #'raw-is-ann)])
             (cond
               [ann-datum
                (annotation-class-doc docstring
                                      local-name
                                      signature
                                      (string->symbol ann-datum))]
               [else
                (class-doc docstring
                           local-name
                           signature)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Signature and parameter

(define-syntax-class signature-doc-js
  #:description "signature"
  #:attributes {parsed}
  (pattern (~hash-object
            [parameters (params:parameter-js ...)]
            [return return:maybe-annotation])
           #:attr parsed
           (doc-signature
            (attribute params.parsed)
            (attribute return.parsed))))

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
            [default default:string-or-false])
           #:do [(define kind (syntax->datum #'kind-stx))
                 (define formal-name (syntax->datum #'formal-name-stx))]
           #:fail-unless kind
           "unknown parameter kind (from Python level)"
           #:attr parsed
           (doc-parameter formal-name
                          (attribute maybe-ann-jsexpr.parsed)
                          (case kind
                            [("POSITIONAL_ONLY") 'positional-only]
                            [("POSITIONAL_OR_KEYWORD") 'positional-or-keyword]
                            [("VAR_POSITIONAL") 'var-positional]
                            [("KEYWORD_ONLY") 'keyword-only]
                            [("VAR_KEYWORD") 'var-keyword])
                          (false->maybe
                           (attribute default.parsed)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotation

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
                      ("class" ~! (~hash-object•full-value-name name)))
           #:attr parsed (class-ann name))
  (pattern (~describe "function annotation"
                      ("function" ~! (~hash-object•full-value-name name)))
           #:attr parsed (function-ann name))
  (pattern (~describe "named annotation use"
                      ("named-annotation"
                       ~!
                       (~hash-object•full-value-name name)))
           #:attr parsed (named-ann name))
  ;; The rest are recursive
  (pattern (~describe "annotation-constructor annotation"
                      ("annotation-constructor"
                       ~!
                       (~hash-object•full-value-name
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small helpers

(define-syntax-class modpath
  #:description #f
  #:attributes {parsed}
  (pattern mod:str
           #:attr parsed
           (string->full-mod-name (syntax->datum #'mod))))

(define-syntax-class docstring/comment/missing-js
  ;#:description 
  #:attributes {parsed}
  (pattern #f
           #:attr parsed (docstring/comment/missing #f))
  (pattern ("comments" ~! v:str)
           #:attr parsed
           (docstring/comment/missing
            (syntax->datum #'v)))
  (pattern ("docstring" ~! v:str)
           #:attr parsed
           (docstring/comment/missing
            (docstring
             (syntax->datum #'v)))))

(define-syntax-class named-annotation-value/singleton-js
  ;#:description 
  #:attributes {parsed}
  (pattern ("singleton" ~!)
           #:attr parsed 'singleton)
  (pattern ann:annotation
           #:attr parsed (attribute ann.parsed)))

(define-syntax-class maybe-annotation
  #:description "annotation or #f"
  #:attributes {parsed}
  (pattern #f
           #:attr parsed (nothing))
  (pattern ann:annotation
           #:attr parsed (just (attribute ann.parsed))))

(define-syntax-class string-or-false
  #:description "string or #f"
  #:attributes {parsed}
  (pattern #f
           #:attr parsed #f)
  (pattern s:str
           #:attr parsed
           (syntax->datum #'s)))

  
