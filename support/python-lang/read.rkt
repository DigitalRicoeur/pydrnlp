#lang typed/racket

(require syntax/parse/define)

(provide python-body-read-syntax
         python-body-read)

;; TODO: un-escape doc-strings

(: python-body-read-syntax (-> Any Input-Port (Listof Syntax)))
(define (python-body-read-syntax src in)
  (map (λ ([d : Datum])
         ;; otherwise TR has trouble generating a contract
         (datum->syntax #f d))
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
             [(name e) e] ...))
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

(: datum-show (-> (U Token Parens* Enforested) Datum))
(define (datum-show it)
  (define (show* [lst : (Listof (U Token Parens*))]) : Datum
    (map datum-show lst))
  (cond
    [(parens? it)
     (cons '#%parens (map show* (parens-body it)))]
    [(brackets? it)
     (cons '#%brackets (map show* (brackets-body it)))]
    [(braces? it)
     (cons '#%braces (map show* (braces-body it)))]
    [(line? it)
     (cons '#%line (map datum-show (line-body it)))]
    [(suite? it)
     (cons '#%suite (map datum-show (suite-body it)))]
    [else
     (token-struct->datum (cast it Token))]))


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
     (cons docstring (parse-continue enforested))]
    [_
     (parse-continue enforested)]))

(: parse-continue (-> (Listof Enforested) (Listof Datum)))
(define (parse-continue enforested)
  (match enforested
    ['()
     '()]
    [(list-rest (line (and ln (cons (name: "import" "from") _)))
                enforested)
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
     (cons (parse-class ln s-b)
           (parse-continue enforested))]
    [(list-rest (line (list-rest (name: n) (op: "=") _))
                (line (STRING docstring))
                enforested)
     (cons (cons `(= ,n ,docstring))
           enforested)]
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
            [(cons (list (op: "*") (name: n))
                   params)
             (list* '#:* n (loop params))]
            [(cons (list (op: "**") (name: n))
                   params)
             (list* '#:** n (loop params))]
            [(cons (list-rest (name: n) (op: "=") _)
                   params)
             (cons (list n) (loop params))]))]
       [{docstring/false the-suite-body}
        (match the-suite-body
          [(cons (line (list (STRING docstring)))
                 the-suite-body)
           (values docstring the-suite-body)]
          [_
           (values #f the-suite-body)])]
       [{maybe-return}
        (parse-maybe-return the-suite-body)])
    `(def ,name ,params
       ,@(if async? '(#:async) null)
       ,docstring/false
       ,@(if maybe-return `(#:return ,maybe-return) null))))

(: parse-maybe-return (-> (Listof Enforested) (U #f Datum)))
(define (parse-maybe-return the-suite-body)
  (map datum-show the-suite-body))

(: parse-class (-> (List+of (U Token Parens*))
                   (Listof Enforested)
                   Datum))
(define (parse-class ln the-suite-body)
  (list '#%class
        (map datum-show ln)
        (map datum-show the-suite-body)))

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

