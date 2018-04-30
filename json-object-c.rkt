#lang racket

(require json
         adjutor
         (for-syntax syntax/parse
                     racket/syntax
                     ))

#|
Issues with json module / documentation:

;; no contract violation for
(write-json '() 'not-a-port)


## Hash Table Types

Documentation says:
   (and/c hash-eq? (hash/c symbol? jsexpr?))
I intend to change the documentation to say:
   (hash/c symbol? jsexpr?)

Implementation accepts all hash tables:
   (jsexpr? #hash())

The documented contract is buggy:
(define/contract bad
  (and/c hash-eq? (hash/c symbol? jsexpr?))
  "not a hash")
attempts to call hash-eq? on a non-hash-table
value, which violates hash-eq?'s contract.

Because keys are constrained to be symbols,
no semantic issues are raised by permitting
hash-eqv? and hash-equal?
(vs. https://github.com/racket/racket/issues/1876).


## json-null

The value of the (json-null) parameter is compared using eq?.
Should it be?
If so, it should be documented.

## Streams

> (string->jsexpr "1 2 3")
1

## Numbers

Here there are both a clear bug and a clear
documentation inaccuracy, but looking at the
two of them leads me to propose some extended
functionality.

Documentation says:
  (or/c exact-integer? inexact-real?)

Implementation does:
  (or/c exact-integer?
        (and/c inexact-real?
               (not/c +nan.0 +inf.0 -inf.0)))

I think the implementation means to do
  `(or/c exact-integer? (and/c inexact? rational?))

;; Thought: supporting non-integer exact rationals
;; would break the round-trip functionality.

However, per [RFC 4625](http://www.ietf.org/rfc/rfc4627.txt)
(via https://github.com/racket/racket/pull/1937),
"""
Numeric values that cannot be represented as sequences of digits
(such as Infinity and NaN) are not permitted.
"""

The implementation tries to conform to this (though there is a
bug), so at a minimum the documentation should be changed to
reflect the intended behavior correctly.

The bug is in:
```racket
(define (real-real? x) ; not nan or inf
  (and (inexact-real? x) (not (member x '(+nan.0 +inf.0 -inf.0)))))
```
which fails to test for the single-precision variants,
`+inf.f`, `-inf.f`, and `+nan.f`.

|#

(module+ test
  (require rackunit))

(define-syntax (let+ stx)
  (define-syntax-class binding
    #:description "binding pair"
    #:attributes {var rhs}
    (pattern (var:id rhs:expr)))
  (define-syntax-class clause
    #:description #f
    (pattern v:binding)
    (pattern #:))
  (define-splicing-syntax-class distinct-bindings
    #:description "sequence of distinct binding pairs"
    #:attributes {[var 1] [rhs 1]}
    (pattern (~seq b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
             "duplicate variable name"
             #:with (var ...) #'(b.var ...)
             #:with (rhs ...) #'(b.rhs ...)))
  (syntax-parse stx
    [(_ (bind:distinct-bindings)
        body ...+)
     #`(let ([bind.var bind.rhs] ...)
         body ...)]
    [(_ (bind:distinct-bindings
         #:
         more:clause ...)
        body ...+)
     #`(let ([bind.var bind.rhs] ...)
         (let+ (more ...)
           body ...))]))



(define-syntax-rule (lam proc)
  (λ (x) (proc x)))


#;
(struct json-contract/c (type)
  #:transparent
  #:property prop:custom-write
  contract-custom-write-property-proc
  #:property prop:contract
  (let ([sym->pred
         (λ (sym)
           (case sym
             [(flat) flat-contract?]
             [(chaperone) chaperone-contract?]
             [(impersonator) contract?]
             [else (raise-argument-error
                    'json-contract/c
                    "(or/c 'flat 'chaperone 'impersonator)"
                    sym)]))]
  (build-flat-contract-property
   #:name (λ (this)
            (build-compound-type-name
             'json-contract/c
             (coerce-flat-contract
              (json-contract/c-type this))))
   ;#:stronger
   #:first-order (λ (this)
                   (sym->pred (json-contract/c-type this)))
   #:late-neg-projection
   (λ (this)
     (λ (blame)
       (λ (value neg-party)
         (define pred
           (sym->pred (json-contract/c-type this)))
         (unless (pred value)
           (raise-blame-error
            blame value #:missing-party neg-party
            '(expected: "~e" given: "~e")
            (contract-name pred)
            value))
         ;; jsexpr? doesn't cooperate with contract-stronger?
         ;; Actually we can't even assume jsexpr? will always
         ;; give the same answer for the same argument
         ;; b/c of 
         (define inner-contract-name
           (contract-name value))
         (define inner-contract-pred
           ))))))))
   

(struct json-object-contract (name
                              exact-keys-hash
                              first-order
                              late-neg)
  #:transparent
  #:property prop:custom-write
  contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (lam json-object-contract-name)
   #:first-order (lam json-object-contract-first-order)
   #:late-neg-projection (lam json-object-contract-late-neg)
   ;#:stronger
   ;#:generate
   #|END json-object-contract|#))


(define-syntax (json-object/c stx)
  ;; Post-MVP features: #:ignore-extra, #:optional
  (define-syntax-class attribute-clause
    #:description "attribute clause"
    #:attributes {name value-contract-expr value-contract
                       value-contract-pred value-contract-late-neg
                       value-contract+blame plain-value-contract}
    (pattern [(~describe "attribute name" name:id)
              raw-value-contract-expr]
             #:declare raw-value-contract-expr
             (expr/c #'flat-contract?
                     #:name "value contract expression")
             #:with value-contract-expr
             #`raw-value-contract-expr.c
             #:with (value-contract
                     plain-value-contract
                     value-contract-pred
                     value-contract-late-neg
                     value-contract+blame)
             (generate-temporaries
              (map (λ (sym)
                     (format-id #f "~a:~a" #'name sym))
                   '(value-contract
                     plain-value-contract
                     value-contract-pred
                     value-contract-late-neg
                     value-contract+blame)))))
  (syntax-parse stx
    [(_ spec:attribute-clause ...+)
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(spec.name ...)))
     "duplicate attribute name"
     #:with exact-keys-hash-expr
     (datum->syntax stx (datum-intern-literal
                         (for/hasheq ([sym (in-list
                                            (syntax->datum
                                             #'(spec.name ...)))])
                           (values sym #t))))
     #`(let+ ([exact-keys-hash exact-keys-hash-expr]
              [spec.plain-value-contract
               spec.value-contract-expr]
              ...
              #:
              [the-contract-name
               `(json-object/c
                 [spec.name ,(contract-name spec.plain-value-contract)]
                 ...)]
              [spec.value-contract
               (and/c spec.plain-value-contract jsexpr?)]
              ...
              #:
              [spec.value-contract-pred
               (flat-contract-predicate spec.value-contract)]
              ...
              [spec.value-contract-late-neg
               (get/build-late-neg-projection spec.value-contract)]
              ...
              #:
              [first-order
               (λ (val)
                 (and (hash? val)
                      (hash-eq? val)
                      (immutable? val)
                      (infix: val has-all-keys-from? exact-keys-hash)
                      ;; the loop tests that val has ONLY
                      ;; keys from exact-keys-hash
                      (for/and ([{k v} (in-immutable-hash val)])
                        (case k
                          [(spec.name)
                           (spec.value-contract-pred v)]
                          ...
                          [else #f]))))])
         (define (late-neg blame)
           (define spec.value-contract+blame
             (spec.value-contract-late-neg
              (blame-add-key-context blame 'spec.name)))
           ...
           (λ (val neg-party)
             (check-is-immutable-hasheq blame val neg-party)
             (unless (infix: val has-all-keys-from? exact-keys-hash)
               (raise-missing-key-error blame val neg-party
                                        exact-keys-hash))
             ;; the loop tests that val has ONLY
             ;; keys from exact-keys-hash
             (for ([{k v} (in-immutable-hash val)])
               (case k
                 [(spec.name)
                  (spec.value-contract+blame v neg-party)]
                 ...
                 [else
                  (raise-bad-key-error blame val neg-party
                                       k v
                                       exact-keys-hash)]))
             val))
         ;;;;
         (json-object-contract the-contract-name
                               exact-keys-hash
                               first-order
                               late-neg))
     #|END json-object/c|#]))



(define (has-all-keys-from? a b)
  ;; (infix: a has-all-keys-from? b)
  (hash-keys-subset? b a))

(module+ test
  (let ([smaller #hasheq([a . 1])]
        [larger #hasheq([a . 1][b . 2])])
    (check-true
     (infix: larger has-all-keys-from? smaller))
    (check-false
     (infix: smaller has-all-keys-from? larger))))


(define (blame-add-key-context blame key)
  (blame-add-context blame
                     (format "the value for ~e in" key)))


(define (check-is-immutable-hasheq blame val neg-party)
  (unless (hash? val)
    (raise-blame-error
     blame val
     #:missing-party neg-party
     '(expected: "hash?" given: "~e")
     val))
  (unless (immutable? val)
    (error 'TODO))
  (unless (hash-eq? val)
    (error 'TODO))
  val)


(define (raise-bad-key-error blame val neg-party
                             bad-key bad-key-value
                             exact-keys-hash)
  (error 'TODO))


(define (raise-missing-key-error blame val neg-party
                                 exact-keys-hash)
  (error 'TODO))




(define/contract bad
  (json-object/c
   [a string?]
   [b number?]
   [c "apples"])
  #hasheq([a . "a"]
          [b . 1]
          [c . "bad"]))





