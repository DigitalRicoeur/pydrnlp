#lang racket/base

(require syntax/parse
         (only-in syntax/parse [attribute $])
         racket/sequence
         racket/string
         racket/match
         "prefabs.rkt"
         (for-label racket/base)
         (for-template racket/base))

(provide python-module-forms
         string/false
         returnable
         pydrnlp-raw-root-module-path
         bad-return-message-suffix
         fixup-static-return)

(define-splicing-syntax-class python-module-forms
  #:description #f
  #:attributes {[form 1]
                docstring definitions module-imports-table value-imports-table}
  (pattern (~and (~seq form ...)
                 (~seq docstring:string/false
                       (~alt (~seq import:import-decl)
                             (~seq (~or* definition:var-decl
                                         definition:def-decl
                                         definition:class-decl)))
                       ...))
           #:attr module-imports-table (hash-append* ($ import.module-imports-table))
           #:attr value-imports-table (hash-append* ($ import.value-imports-table))
           #:attr definitions ($ definition.parsed)))

(define (hash-append* lst)
  (if (null? lst)
      #hash()
      (for*/fold ([acc (car lst)])
                 ([hsh (in-list (cdr lst))]
                  [{k v} (in-immutable-hash hsh)])
        (hash-set acc k v))))

(define-syntax-class string/false
  #:description #f
  (pattern _:str)
  (pattern #f))

(define-syntax-class import-decl
  #:description "import form"
  #:attributes {module-imports-table value-imports-table}
  #:datum-literals {import |.|}
  (pattern (import (~seq (|.| src-part:str ...+)
                         (~optional (~seq #:as as-name:str)))
                   ...)
           #:attr value-imports-table #hash()
           #:attr module-imports-table
           (for/hash ([as (in-list (attribute as-name))]
                      [srcs (in-list (syntax->datum #'((src-part ...) ...)))])
             (values (if as (list (syntax->datum as)) srcs)
                     srcs)))
  (pattern (import #:from (|.| src-part:str ...+)
                   ;; TODO maybe support this
                   #:*)
           #:attr value-imports-table #hash()
           #:attr module-imports-table #hash())
  (pattern (import #:from (|.| src-part:str ...+)
                   (~seq name:str (~optional (~seq  #:as as-name:str)))
                   ...)
           #:attr module-imports-table #hash()
           #:attr value-imports-table
           (let ([src (syntax->datum #'(src-part ...))])
             (for/hash ([n (in-list (syntax->datum #'(name ...)))]
                        [as (in-list (attribute as-name))])
               (values (if as (syntax->datum as) n)
                       (cons src n))))))

(define-syntax-class var-decl
  #:description "variable declaration"
  #:attributes {parsed}
  #:datum-literals {=}
  (pattern (= name:str docstring:string/false)
           #:attr parsed (var-definition #'name #'docstring)))
  

(define-syntax-class class-decl
  #:description "class form"
  #:attributes {parsed}
  #:datum-literals {class |.|}
  (pattern (class name:str ((|.| super-part:str ...+) ...)
             docstring:string/false
             ((~or* child:def-decl child:var-decl) ...))
           #:attr parsed
           (class-definition #'name #'docstring
                             (map syntax->list (syntax->list #'((super-part ...) ...)))
                             ($ child.parsed))))

(define-syntax-class def-decl
  #:description "def form"
  #:attributes {parsed}
  #:datum-literals {def}
  (pattern (def name:str (arg:python-formal ...)
             (~optional (~and #:async async?))
             docstring:string/false
             (~optional (~seq #:return ret:static-return)))
           #:attr parsed
           (fun-definition #'name #'docstring
                           ($ arg.parsed) ($ async?) ($ ret.parsed))))

(define-syntax-class python-formal
  #:description "formal parameter"
  #:attributes {parsed}
  (pattern name:str
           #:attr parsed (list 'required #'name))
  (pattern #:*
           #:attr parsed (list '*))
  (pattern [#:? name:str #:TODO]
           #:attr parsed (list 'optional #'name))
  (pattern [#:* name:str]
           #:attr parsed (list '* #'name))
  (pattern [#:** name:str]
           #:attr parsed (list '** #'name)))

(define-syntax-class static-return
  #:attributes {parsed}
  #:datum-literals {let*}
  (pattern (let* ([lhs:id rhs:returnable/error] ...)
             body:returnable/error)
           #:attr parsed
           (let ([e (or ($ body.any-error?)
                        (findf values ($ rhs.any-error?)))])
             (if e (bad-return e) this-syntax))))

(define-syntax-class returnable/error
  #:attributes {any-error?}
  #:datum-literals {list #%error}
  (pattern parsed:returnable
           #:attr any-error? #f)
  (pattern (#%error any-error?))
  (pattern (list sub:returnable/error ...)
           #:attr any-error? (findf values (attribute sub.any-error?))))

(define-syntax-class returnable
  #:datum-literals {|.| list}
  (pattern #f)
  (pattern _:exact-integer)
  (pattern _:id)
  (pattern ((|.| other-ref-part:str ...+)))
  (pattern (list _:returnable ...)))

(define (fixup-static-return stx
                             #:module-imports-table module-imports-table
                             #:value-imports-table value-imports-table
                             #:on-import-found on-import-found
                             #:on-error on-error)
  (define/syntax-parse ((~datum let*) ([lhs:id rhs:returnable] ...)
                                      body:returnable)
    stx)
  (define local-names ($ lhs))
  (let/ec return
    (define (return-error! stxs msg)
      (return (on-error stxs msg)))
    (let ([dup (check-duplicate-identifier local-names)])
      (when dup
        (return-error! (list dup) ";\n mutated variables not allowed")))
    (define fixup-returnable
      (syntax-parser
        #:datum-literals {|.| list}
        [#false
         #'#false]
        [int:exact-integer
         #'int]
        [(list elem ...)
         #:with (fixed ...) (map fixup-returnable ($ elem))
         #'(list fixed ...)]
        [constant-ref:id
         (unless (member #'constant-ref local-names bound-identifier=?)
           (return-error! (list #'constant-ref)
                          ";\n reference to a variable not defined as a local constant"))
         #'constant-ref]
        [((|.| name-part:str ...+))
         (cond
           [(match (syntax->datum #'(name-part ...))
              [(list single)
               (match (hash-ref value-imports-table single #f)
                 [(cons src "revision")
                  src]
                 [_
                  #f])]
              [(list part... ... "revision")
               (hash-ref module-imports-table part... #f)])
            => (λ (src)
                 (on-import-found src (syntax->list #'(name-part ...))))]
           [else
            (return-error!
             (syntax->list #'(name-part ...))
             (string-append ";\n"
                            " unsupported application form\n"
                            "  expected: an imported revision function\n"
                            "  given: "
                            (string-join (syntax->datum #'(name-part ...))
                                         ".")))])]))
    (define/syntax-parse ([lhs* rhs*] ...)
      (for/list ([id (in-syntax #'(lhs ...))]
                 [expr (in-syntax #'(rhs ...))])
        (list id (fixup-returnable expr))))
    (define/syntax-parse body*
      (fixup-returnable #'body))
    #'(let* ([lhs* rhs*] ...)
        body*)))

(define (pydrnlp-raw-root-module-path parts)
  #`(lib #,(string-join #:before-first "pydrnlp/py/"
                        parts
                        "/"
                        #:after-last ".py")))

(define (bad-return-message-suffix)
  (string-append ";\n"
                 " value not supported\n"
                 "  expected:\n"
                 "   False;\n"
                 "   a literal number;\n"
                 "   a local constant reference; or\n"
                 "   an imported revision function application\n"
                 "  given: something else"))

