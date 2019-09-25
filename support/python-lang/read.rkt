#lang typed/racket

(provide python-body-read-syntax
         python-body-read)

;; PEP 257

;; A docstring is a string literal that occurs as
;; the first statement in a module, function, class,
;; or method definition.

;; String literals occurring immediately after
;; a simple assignment at the top level of a module,
;; class, or __init__ method are called "attribute docstrings".

;; String literals occurring immediately after
;; another docstring are called "additional docstrings".

;; see also: PEP 258
;; Guido: "the reason for PEP 258's rejection is not that
;;         it is invalid, but that it's not slated for
;;         stdlib inclusion"
;; https://bugs.python.org/issue35651

(require syntax/parse/define)

;; TR has trouble generating a contract for Datum
;; because it can include immutable stuff.

(: python-body-read-syntax (-> Any Input-Port (Listof Syntax)))
(define (python-body-read-syntax src in)
  (map (λ ([d : Datum])
         (strip-context (datum->syntax #f d)))
       (parse
        (enforest
         (tokenize src in)))))

(: python-body-read (-> Input-Port (Listof Sexp)))
(define (python-body-read in)
  (map syntax->datum
       (python-body-read-syntax #f in)))

(define-simple-macro (define-token-structs
                       [sym-type:id sym->ctor:id ->datum:id]
                       name:id ...)
  (begin (define-type sym-type
           (U 'name ...))
         (: sym->ctor (-> sym-type (-> (Syntaxof String)
                                       Raw-Token)))
         (define sym->ctor
           (match-lambda
             ['name name] ...))
         (: ->datum (-> Raw-Token Datum))
         (define ->datum
           (match-lambda
             [(name e) (list 'name e)] ...))
         (struct name ([e : (Syntaxof String)])
           #:transparent)
         ...))

(define-token-structs [Token-Symbol symbol->constructor token-struct->datum]
  NAME NUMBER STRING OP COMMENT NL NEWLINE
  DEDENT INDENT ERRORTOKEN ENDMARKER)

(define-match-expander syntax:
  (syntax-parser
    [(_ pat ...+)
     #'(app syntax-e (or pat ...))]))
(define-match-expander name:
  (syntax-parser
    [(_ pat ...+)
     #'(NAME (syntax: pat ...))]))
(define-match-expander op:
  (syntax-parser
    [(_ pat ...+)
     #'(OP (syntax: pat ...))]))

(define-type Token
  (U NAME NUMBER STRING OP ERRORTOKEN))

(define-type Newline-Token
  (U NL NEWLINE ENDMARKER))
(: newline-token? (-> Any Boolean : Newline-Token))
(define (newline-token? it)
  (or (NEWLINE? it)
      (NL? it)
      (ENDMARKER? it)))

(define-type (List+of A)
  (Pairof A (Listof A)))

(struct (A) suite ([body : (Listof A)])
  #:transparent)

(struct line ([body : (List+of (U Token Parens*))])
  #:transparent)

(define-type Enforested
  (U line (suite Enforested)))

(define-type Parens*
  (U parens braces brackets))
(struct parens ([body : (Listof (Listof (U Token Parens*)))])
  #:transparent)
(struct braces ([body : (Listof (Listof (U Token Parens*)))])
  #:transparent)
(struct brackets ([body : (Listof (Listof (U Token Parens*)))])
  #:transparent)

(define-type Raw-Token
  (U Without-Comments COMMENT))

(define-type Showable
  (U Token Parens* Enforested (Listof Showable)))

(: datum-show (-> Showable Datum))
(define (datum-show it)
  (define (show* [lst : (Listof Showable)]) : Datum
    (map datum-show lst))
  (cond
    [(list? it)
     (show* it)]
    [(parens? it)
     (cons '#%parens (map show* (parens-body it)))]
    [(brackets? it)
     (cons '#%brackets (map show* (brackets-body it)))]
    [(braces? it)
     (cons '#%braces (map show* (braces-body it)))]
    [(or (line? it) (suite? it)) ;; for typechecker
     (if (line? it)
         (cons '#%line (map datum-show (line-body it)))
         (cons '#%suite (map datum-show (suite-body it))))]
    [else
     (token-struct->datum it)]))


;                                               
;                                               
;                                               
;                                               
;   ;;         ;;                 ;;            
;   ;;         ;;                               
;  ;;;;; ;;;   ;;  ;; ;;   ; ;;;  ;;;;;;;   ;;  
;   ;;  ;   ;  ;;  ; ;  ;  ;;  ;  ;;   ;   ;  ; 
;   ;;  ;   ;  ;; ;  ;  ;  ;;  ;; ;;   ;   ;  ; 
;   ;; ;;   ;; ;;;; ;;;;;; ;;  ;; ;;  ;   ;;;;;;
;   ;;  ;   ;  ;;  ; ;     ;;  ;; ;; ;     ;    
;    ;  ;   ;  ;;  ; ;     ;;  ;; ;; ;     ;    
;    ;;; ;;;   ;;   ; ;;;  ;;  ;; ;;;;;;;   ;;; 
;                                               
;                                               
;                                               
;                                               

(require/typed
 python-tokenizer
 [generate-tokens
  (-> Input-Port (Sequenceof (List Token-Symbol
                                   String
                                   (List Natural Natural)
                                   (List Natural Natural)
                                   String)))])
(: tokenize (-> Any Input-Port (Listof Raw-Token)))
(define (tokenize src in)
  (define line-start-pos-tbl : (HashTable Natural Natural)
    (let ([in (peeking-input-port in)])
      (port-count-lines! in)
      (let loop ([hsh : (HashTable Natural Natural) #hasheqv()]
                 [i : Natural 1])
        (define-values [ln col pos]
          (port-next-location in))
        (assert pos)
        (let* ([hsh (hash-set hsh i pos)])
          (if (eof-object? (read-line in 'any))
              hsh
              (loop hsh (add1 i)))))))
  (for/list ([lst : (List Token-Symbol
                          String
                          (List Natural Natural)
                          (List Natural Natural)
                          String)
                  (generate-tokens in)])
    ;; generate-tokens is lazy
    (match-define (list type
                        datum-lexeme
                        (list start-ln start-col)
                        (list end-ln end-col)
                        _)
      lst)
    (define start-pos
      (+ start-col (hash-ref line-start-pos-tbl start-ln)))
    (define end-pos
      (+ end-col (hash-ref line-start-pos-tbl end-ln)))
    (define span
      (- end-pos start-pos))
    (define loc
      (vector-immutable src start-ln start-col start-pos span))
    ((symbol->constructor
      (cond
        [(and (eq? 'OP type)
              (regexp-match? #px"^[a-zA-Z_]\\w*$" datum-lexeme))
         ;; python-tokenizer improperly classifies NAMEs
         ;; beginning with _ as OPs
         'NAME]
        [else
         type]))
     (cast (datum->syntax #f datum-lexeme loc)
           (Syntaxof String)))))

(define the-tab-replacement-string
  (string->immutable-string (make-string 8 #\space)))

(: clean-docstring (-> (Syntaxof String) (Syntaxof String)))
(define (clean-docstring stx)
  (define (empty-string? [str : String])
    (not (non-empty-string? str)))
  (match-let*
      ([str (syntax-e stx)]
       [(regexp "(?<=^\"\"\").*(?=\"\"\"$)" (list trimmed))
        (regexp-replace* #rx"\t" str the-tab-replacement-string)]
       [(cons line0 lines)
        (regexp-split #rx"\n" trimmed)]
       [line0
        (string-trim line0 #rx" +" #:right? #f)]
       [indexes
        (for/list : (Listof (U #f Index)) ([line (in-list lines)])
          (let ([pr (regexp-match-positions #rx"[^ ]" line)])
            (and pr (caar pr))))]
       [min-index
        (or (foldl (λ ([this : (U #f Index)]
                       [prev : (U #f Index)])
                     (if (and this prev)
                         (min this prev)
                         (or this prev)))
                   #f
                   indexes)
            0)]
       [lines
        (for/list : (Listof String) ([line : String (in-list lines)]
                                     [idx : (U #f Index) (in-list indexes)])
          ;; we know non-blank lines are long enough
          (if idx
              (substring line min-index)
              ""))]
       [lines
        (cons line0 lines)]
       [lines
        (dropf-right (dropf lines empty-string?) empty-string?)])
    (cast (datum->syntax* stx (string-join lines "\n"))
          (Syntaxof String))))

;                                                               
;                                                               
;                                                               
;                                                               
;                     ;;                  ;;           ;;       
;                     ;;                  ;;           ;;       
;    ;; ;    ; ; ;;; ;;;;; ;;   ;;  ;;    ;;;;;   ;;   ;; ; ;;  
;  ;;  ; ;  ;  ;;  ;  ;;  ;  ;   ;  ;     ;;  ;  ;  ;  ;; ;;  ; 
;   ;    ;  ;  ;;  ;; ;;     ;;   ;;      ;;  ;; ;  ;  ;; ;;  ; 
;    ;;  ;  ;  ;;  ;; ;;   ;;;;   ;       ;;  ;;;;;;;; ;; ;;  ;;
;      ;; ; ;  ;;  ;; ;;  ;  ;;   ;;      ;;  ;; ;     ;; ;;  ; 
;  ;   ;  ;;   ;;  ;;  ; ;;  ;;  ;  ;     ;;  ;; ;     ;; ;;  ; 
;   ;;;    ;   ;;  ;;  ;;;;;; ; ;   ;;    ;;  ;;  ;;;   ; ;;;;  
;          ;                                              ;;    
;         ;                                               ;;    
;       ;;                                                ;;    
;                                                               

(: datum->syntax* (case->
                   (-> (U (Syntaxof Any) #f) Symbol Identifier)
                   (-> (U (Syntaxof Any) #f) Datum Syntax)))
(define (datum->syntax* ctx v)
  (datum->syntax ctx v ctx ctx))

(: ->id (-> (Syntaxof String) Identifier))
(define (->id stx)
  (datum->syntax* stx (string->symbol (syntax-e stx))))

(: strip-context (-> Syntax Syntax))
(define strip-context values)
#;
(define (strip-context v)
  ;; like syntax/strip-context, but TR can't contract that
  ;; this is simplified to make the types easier
  (define (inner [v : Syntax-E]) : Syntax-E
    (cond
      [(list? v)
       (map strip-context v)]
      [(pair? v)
       (cons (strip-context (car v))
             (strip-context (cdr v)))]
      [(box? v)
       (box (strip-context (unbox v)))]
      [(vector? v)
       (vector-map strip-context v)]
      [else
       v]))
  (datum->syntax #f (inner (syntax-e v)) v v))

;                                
;                                
;                                
;                                
;                                
;                                
;   ; ;;    ;;    ;; ;  ;; ; ;;  
;   ;;  ;  ;  ;   ;;; ;;  ; ;  ; 
;   ;;  ;     ;;  ;;   ;    ;  ; 
;   ;;  ;;  ;;;;  ;;    ;; ;;;;;;
;   ;;  ;  ;  ;;  ;;      ;;;    
;   ;;  ; ;;  ;;  ;;  ;   ; ;    
;   ;;;;   ;;; ;  ;;   ;;;   ;;; 
;   ;;                           
;   ;;                           
;   ;;                           
;                                

(: parse (-> (Listof Enforested) (Listof Datum)))
(define (parse enforested)
  (match enforested
    [(cons (line (list (STRING docstring)))
           enforested)
     (cons (clean-docstring docstring)
           (parse-continue enforested))]
    [_
     (cons #f (parse-continue enforested))]))

(: parse-continue (->* [(Listof Enforested)]
                       [#:for-class? Boolean]
                       (Listof Datum)))
(define (parse-continue enforested #:for-class? [for-class? #f])
  (match enforested
    ['()
     '()]
    [(list-rest (line (and ln (cons (name: "import" "from") _)))
                enforested)
     #:when (not for-class?)
     (cons (parse-import ln)
           (parse-continue enforested))]
    [(list-rest (line (and ln (or (list-rest (name: "async")
                                             (name: "def")
                                             _)
                                  (list-rest (name: "def")
                                             _))))
                (suite s-b)
                enforested)
     (cons (parse-def ln s-b)
           (parse-continue enforested))]
    [(list-rest (line (and ln (cons (name: "class") _)))
                (suite s-b)
                enforested)
     #:when (not for-class?)
     (cons (parse-class ln s-b)
           (parse-continue enforested))]
    [(list-rest (line (list-rest (name: n) (op: "=") _))
                (line (STRING docstring))
                enforested)
     (cons `(= ,n ,(clean-docstring docstring))
           (parse-continue enforested))]
    [(list-rest (line (list-rest (name: n) (op: "=") _))
                enforested)
     (cons `(= ,n #f)
           (parse-continue enforested))]
    [(list-rest _ enforested)
     (parse-continue enforested)]))

(: parse-import (-> (List+of (U Token Parens*))
                    Datum))
(define parse-import
  (match-lambda
    [(cons (name: "import") lst)
     (letrec ([start
               : (-> (Listof (U Token Parens*))
                     (Listof Datum))
               (match-lambda
                 ['() '()]
                 [(cons (name: init) lst)
                  (loop (list init) lst)])]
              [loop
               : (-> (List+of String)
                     (Listof (U Token Parens*))
                     (Listof Datum))
               (λ (acc lst)
                 (define (finish)
                   (cons '|.| (reverse acc)))
                 (match lst
                   ['()
                    (list (finish))]
                   [(list-rest (name: "as")
                               (name: as-name)
                               (or (cons (op: ",") lst)
                                   lst))
                    (list* (finish)
                           '#:as as-name
                           (start lst))]
                   [(list-rest (op: ",") lst)
                    (cons (finish) (start lst))]
                   [(list-rest (op: ".") (name: n) lst)
                    (loop (cons n acc) lst)]))])
       (cons 'import (start lst)))]
    [(cons (name: "from") lst)
     (let-values
         ([{acc lst}
           (match lst
             [(cons (op: (and rel "." "..")) lst)
              (assert rel)
              (values (list rel) lst)]
             [_
              (values null lst)])])
       (let loop ([acc : (Listof String) acc]
                  [lst lst])
         (match lst
           [(cons (name: "import") lst)
            `(import #:from ,(cons '|.| (reverse acc))
                     ,@(parse-import-from-rhs lst))]
           [(cons (op: ".") lst)
            (loop acc lst)]
           [(cons (name: n) lst)
            (loop (cons n acc) lst)])))]))

(: parse-import-from-rhs (-> (Listof (U Token Parens*))
                             (Listof Datum)))
(define (parse-import-from-rhs lst)
  (: group-commas (-> (Listof (U Token Parens*))
                      (Listof (Listof (U Token Parens*)))))
  (define group-commas
    (match-lambda
      ['()
       '()]
      [(cons this lst)
       (let loop ([acc (list this)]
                  [lst lst])
         (match lst
           ['()
            (list (reverse acc))]
           [(cons (op: ",") lst)
            (cons (reverse acc) (group-commas lst))]
           [(cons this lst)
            (loop (cons this acc) lst)]))]))
  (: help (-> (Listof (Listof (U Token Parens*)))
              (Listof Datum)))
  (define (help lst)
    (append*
     (map
      (match-lambda
        [(list-rest (name: n)
                    (name: "as")
                    (name: as))
         (list n '#:as as)]
        [(list (name: n))
         (list n)])
      lst)))
  (match lst
    [(list (op: "*"))
     '(#:*)]
    [(list (parens lst))
     (help lst)]
    [_
     (help (group-commas lst))]))


(: parse-def (-> (List+of (U Token Parens*))
                 (Listof Enforested)
                 Datum))
(define (parse-def lst the-suite-body)
  (let*-values
      ([{async? lst}
        (match lst
          [(list-rest (name: "def")
                      lst)
           (values #f lst)]
          [(list-rest (name: "async")
                      (name: "def")
                      lst)
           (values #t lst)])]
       [{name params lst}
        (match lst
          [(list (name: n)
                 (parens params)
                 (op: ":"))
           (values n params lst)])]
       [{params}
        (let loop : (Listof Datum) ([params params])
          (match params
            ['()
             '()]
            [(cons (list (name: n)) params)
             (cons n (loop params))]
            [(cons (list (op: "*")) params)
             ;; doesn't correspond to an argument,
             ;; just means the following are keyword-only
             (cons '#:* (loop params))]
            [(cons (list (op: "*") (name: n))
                   params)
             (cons `[#:* ,n] (loop params))]
            [(cons (list (op: "**") (name: n))
                   params)
             (list* `[#:** ,n] (loop params))]
            [(cons (list-rest (name: n) (op: "=") default)
                   params)
             ;; TODO: render simple default values
             (cons (list '#:? n '#:TODO) (loop params))]))]
       [{docstring/false the-suite-body}
        (match the-suite-body
          [(cons (line (list (STRING docstring)))
                 the-suite-body)
           (values (clean-docstring docstring)
                   the-suite-body)]
          [_
           (values #f the-suite-body)])]
       [{maybe-return}
        (parse-maybe-return the-suite-body)])
    `(def ,name ,params
       ,@(if async? '(#:async) null)
       ,docstring/false
       ,@(if maybe-return `(#:return ,maybe-return) null))))

(: parse-maybe-return (-> (Listof Enforested) (U #f Datum)))
(define (parse-maybe-return body)
  (let loop ([bindings : (Listof (List Datum Datum)) null]
             [body : (Listof Enforested) body])
    (match body
      [(cons (line (list-rest (NAME lhs) (op: "=") rhs/raw))
             body)
       (loop (cons (list (->id lhs) (expression-parts->returnable rhs/raw))
                   bindings)
             body)]
      [(list (line (cons (name: "return") returned/raw)))
       `(let* ,(reverse bindings)
          ,(expression-parts->returnable returned/raw))]
      [_
       #f])))

(: expression-parts->returnable (-> (Listof (U Token Parens*)) Datum))
(define (expression-parts->returnable parts)
  (define (error-result)
    `(#%error ,(datum-show parts)))
  (match parts
    [(list (brackets inside))
     `(list ,@(map expression-parts->returnable inside))]
    [(list (NUMBER stx))
     (define n (string->number (syntax-e stx)))
     (if (exact-integer? n)
         (datum->syntax stx n stx stx)
         (error-result))]
    [(list (and (name: "False") (NAME stx)))
     (datum->syntax* stx #f)]
    [(list (NAME constant-ref))
     (->id constant-ref)]
    ;; other.module.revision()
    [(cons (NAME this) parts)
     (let/ec return : Datum
       `((|.| ,this
              ,@(let loop : (Listof Datum)
                  ([parts : (Listof (U Token Parens*)) parts])
                  (match parts
                    [(list-rest (op: ".")
                                (NAME this)
                                parts)
                     (cons this (loop parts))]
                    [(list (parens '()))
                     null]
                    [_
                     (return (error-result))])))))]
    [_
     (error-result)]))


(: parse-class (-> (List+of (U Token Parens*))
                   (Listof Enforested)
                   Datum))
(define (parse-class ln the-suite-body)
  (define-values [name supers]
    (match ln
      [(list (name: "class")
             (NAME name)
             (parens raw-supers)
             (op: ":"))
       (values name (map list->dotted-name raw-supers))]
      [(list (name: "class")
             (NAME name)
             (op: ":"))
       (values name null)]))
  (define docstring/false
    (match the-suite-body
      [(cons (line (list (STRING docstring))) _)
       (clean-docstring docstring)]
      [_
       #f]))
  `(class ,name ,supers
     ,docstring/false
     ,(parse-continue the-suite-body #:for-class? #t)))


(: list->dotted-name (-> (Listof (U Token Parens*)) Datum))
(define (list->dotted-name lst)
  (match-let ([(cons (NAME n) lst) lst])
    `(|.| ,n
          ,@(let loop : (Listof Datum) ([lst : (Listof (U Token Parens*)) lst])
              (match lst
                ['()
                 '()]
                [(list-rest (op: ".") (NAME n) lst)
                 (cons n (loop lst))])))))

;                                              
;                                              
;                                              
;                                              
;                 ;;;                       ;; 
;                ;;                         ;; 
;    ;;   ; ;;; ;;;;  ;;;   ;; ;  ;;    ;; ;;;;
;   ;  ;  ;;  ;  ;;  ;   ;  ;;;  ;  ; ;;  ; ;; 
;   ;  ;  ;;  ;; ;;  ;   ;  ;;   ;  ;  ;    ;; 
;  ;;;;;; ;;  ;; ;; ;;   ;; ;;  ;;;;;;  ;;  ;; 
;   ;     ;;  ;; ;;  ;   ;  ;;   ;        ;;;; 
;   ;     ;;  ;; ;;  ;   ;  ;;   ;    ;   ;  ; 
;    ;;;  ;;  ;; ;;   ;;;   ;;    ;;;  ;;;   ;;
;                                              
;                                              
;                                              
;                                              


(: enforest (-> (Listof Raw-Token) (Listof Enforested)))
(define (enforest tokens)
  (tag-lines
   (group-suites
    (tag-paren-shapes
     (strip-comments tokens)))))

(define-type Without-Comments
  (U Token Newline-Token DEDENT INDENT))

(define-type With-Paren-Shapes
  (U Parens* Without-Comments))

(define-type With-Initial-Suites
  (U Token Newline-Token Parens* (suite With-Initial-Suites)))

(: strip-comments (-> (Listof Raw-Token)
                      (Listof Without-Comments)))
(define (strip-comments tokens)
  (filter (ann (λ (it) (not (COMMENT? it)))
               (-> Raw-Token Boolean : Without-Comments))
          tokens))

(: tag-paren-shapes (-> (Listof Without-Comments)
                        (Listof With-Paren-Shapes)))
(define (tag-paren-shapes tokens)
  (: group-inside (-> String
                      (Listof Without-Comments)
                      (values Parens* (Listof Without-Comments))))
  (define (group-inside open tokens)
    (define-values [tag close]
      (match open
        ["(" (values parens ")")]
        ["[" (values brackets "]")]
        ["{" (values braces "}")]))
    (let loop ([acc : (Listof With-Paren-Shapes) null]
               [tokens : (Listof Without-Comments) tokens])
      (match tokens
        [(cons (op: (== close)) tokens)
         (values (tag (fixup-group (reverse acc))) tokens)]
        [(cons (op: (and open (or "(" "[" "{"))) tokens)
         (assert open)
         (let-values ([{this tokens}
                       (group-inside open tokens)])
           (loop (cons this acc) tokens))]
        [(cons this tokens)
         (loop (cons this acc) tokens)])))
  (define-type Without-Comments+Parens*
    (U Without-Comments Parens*))
  (: fixup-group (-> (Listof Without-Comments+Parens*)
                     (Listof (Listof (U Token Parens*)))))
  (define (fixup-group grp)
    (define-type Sans-NL
      (U Token DEDENT INDENT Parens*))
    (let* ([grp
            : (Listof Sans-NL)
            (filter (ann (λ (it)
                           (not (newline-token? it)))
                         (-> Without-Comments+Parens* Boolean : Sans-NL))
                    grp)]
           [grp
            (let partition-commas : (Listof (Listof (U Token Parens*)))
              ([acc : (Listof (U Token Parens*)) null]
               [grp : (Listof (U Token Parens* DEDENT INDENT)) grp])
              (match grp
                ['()
                 (list (reverse acc))]
                [(cons (op: ",") grp)
                 (cons (reverse acc)
                       (partition-commas null grp))]
                [(cons this grp)
                 (assert (not (or (DEDENT? this)
                                  (INDENT? this))))
                 (partition-commas (cons this acc)
                                   grp)]))])
      (match grp
        ['(())
         null]
        [_
         grp])))
  (match tokens
    ['()
     '()]
    [(cons (op: (and open (or "(" "[" "{"))) tokens)
     (assert open)
     (let-values ([{this tokens}
                   (group-inside open tokens)])
       (cons this (tag-paren-shapes tokens)))]
    [(cons this tokens)
     (cons this (tag-paren-shapes tokens))]))



(: group-suites (-> (Listof With-Paren-Shapes)
                    (Listof With-Initial-Suites)))
(define (group-suites tokens)
  (: group-within (-> (Listof With-Paren-Shapes)
                      (values (suite With-Initial-Suites)
                              (Listof With-Paren-Shapes))))
  (define (group-within tokens)
    (let loop ([acc : (Listof With-Initial-Suites) null]
               [tokens : (Listof With-Paren-Shapes) tokens])
      (match-let ([(cons this tokens) tokens])
        (cond
          [(DEDENT? this)
           (values (suite (reverse acc)) tokens)]
          [(INDENT? this)
           (let-values ([{this tokens}
                         (group-within tokens)])
             (loop (cons this acc) tokens))]
          [else
           (loop (cons this acc) tokens)]))))
  (match tokens
    ['()
     '()]
    [(cons this tokens)
     (cond
       [(INDENT? this)
        (let-values ([{this tokens}
                      (group-within tokens)])
          (cons this (group-suites tokens)))]
       [else
        (assert (not (DEDENT? this)))
        (cons this (group-suites tokens))])]))
      

(: reverse+ (∀ (A) (-> (List+of A) (List+of A))))
(define (reverse+ lst)
  (let loop ([acc : (List+of A) (list (car lst))]
             [lst : (Listof A) (cdr lst)])
    (if (null? lst)
        acc
        (loop (cons (car lst) acc)
              (cdr lst)))))

(: tag-lines (-> (Listof With-Initial-Suites)
                 (Listof Enforested)))
(define (tag-lines tokens)
  (let loop ([acc : (Listof (U Token Parens*)) null]
             [tokens : (Listof With-Initial-Suites) tokens])
    (define (finish [get-rest : (-> (Listof Enforested))])
      (if (null? acc)
          (get-rest)
          (cons (line (reverse+ acc))
                (get-rest))))
    (if (null? tokens)
        (finish (λ () null))
        (let ([this (car tokens)]
              [tokens (cdr tokens)])
          (cond
            [(newline-token? this)
             (finish (λ () (tag-lines tokens)))]
            [(suite? this)
             (finish (λ ()
                       (cons (suite (tag-lines
                                     (suite-body this)))
                             (tag-lines tokens))))]
            [else
             (loop (cons this acc) tokens)])))))

