#lang racket

(require python-tokenizer
         syntax/parse/define)

(provide python-body-read-syntax
         python-body-read)

;; TODO: un-escape doc-strings

(define (python-body-read-syntax src in)
  (parse
   (enforest
    (tokenize src in))))

(define (python-body-read in)
  (syntax->datum
   (datum->syntax
    (python-body-read-syntax #f in))))

(define (enforest tokens)
  (tag-lines
   (tag-paren-shapes
    (group-suites
     (drop-leading-newlines
      (strip-comments tokens))))))

(define (parse enforested)
  (match enforested
    [(list-rest `(STRING ,docstring) enforested)
     (cons docstring (parse-continue enforested))]
    [_
     (parse-continue enforested)]))

(define-match-expander syntax:
  (syntax-parser
    [(_ pat ...+)
     #'(app syntax-e (or pat ...))]))
(define-match-expander name:
  (syntax-parser
    [(_ pat ...+)
     #'(list 'NAME (syntax: pat ...))]))
(define-match-expander op:
  (syntax-parser
    [(_ pat ...+)
     #'(list 'OP (syntax: pat ...))]))


(define (parse-continue enforested)
  (filter-map (λ (line)
                (match line
                  [(list-rest '#%line
                              (name: "import" "from")
                              _)
                   (parse-import line)]
                  [(or (list-rest '#%line
                                  (name: "async")
                                  (name: "def")
                                  _)
                       (list-rest '#%line
                                  (name: "def")
                                  _))
                   (parse-def line)]
                  [(list-rest '#%line (name: "class") _)
                   (parse-class line)]
                  [_
                   #f]))
              enforested))

(define handle-attribute-docstrings
  (match-lambda
    [(list-rest (list-rest '#%line
                           `(NAME ,name)
                           `(OP ,(app syntax-e "="))
                           _)
                (list '#%line
                      `(STRING ,docstring))
                enforested)
     (cons `(= ,name ,docstring)
           (parse-continue enforested))]
    [enforested
     enforested]))

(define (tokenize src in)
  (define line-start-pos-tbl
    (let ([in (peeking-input-port in)])
      (port-count-lines! in)
      (for/hasheqv ([i (in-naturals 1)])
        (define-values [ln col pos]
          (port-next-location in))
        #:final (eof-object? (read-line in 'any))
        (values i pos))))
  (for/list ([lst (generate-tokens in)])
    ;; generate-tokens is lazy
    (match-define (list type
                        datum-lexeme
                        (list start-ln start-col)
                        (list end-ln end-col)
                        current-line-str)
      lst)
    (define start-pos
      (+ start-col (hash-ref line-start-pos-tbl start-ln)))
    (define end-pos
      (+ end-col (hash-ref line-start-pos-tbl end-ln)))
    (define span
      (- end-pos start-pos))
    (define loc
      (vector-immutable src start-ln start-col start-pos span))
    (list (cond
            [(and (eq? 'OP type)
                  (regexp-match? #px"^[a-zA-Z_]\\w*$" datum-lexeme))
             ;; python-tokenizer improperly classifies NAMEs
             ;; beginning with _ as OPs
             'NAME]
            [else
             type])
          (datum->syntax #f datum-lexeme loc))))

(define (drop-leading-newlines tokens)
  (dropf tokens (match-lambda
                  [(list (or 'NEWLINE 'NL) _) #t]
                  [_ #f])))

(define (strip-comments tokens)
  (filter (match-lambda
            [`(COMMENT ,_) #f]
            [_ #t])
          tokens))
  
(define group-suites
  (letrec
      ([group-within
        (λ (tokens)
          (let loop ([acc '(#%suite)]
                     [tokens tokens])
          (match tokens
            [(list-rest `(DEDENT ,_) tokens)
             (values (reverse acc) tokens)]
            [(list-rest (list (or 'NEWLINE 'NL) _)
                        ...
                        `(INDENT ,_)
                        tokens)
             (let-values ([{this tokens}
                           (group-within tokens)])
               (loop (cons this acc) tokens))]
            [(list-rest this tokens)
             (loop (cons this acc) tokens)])))]
       [group-suites
        (match-lambda
          ['()
           '()]
          [(list-rest (list (or 'NEWLINE 'NL) _)
                      ...
                      `(INDENT ,_)
                      tokens)
           (let-values ([{this tokens}
                         (group-within tokens)])
             (cons this (group-suites tokens)))]
          [(list-rest (and this (not `(DEDENT ,_)))
                      tokens)
           (cons this (group-suites tokens))])])
    group-suites))

(define tag-paren-shapes
  (letrec
      ([tag-paren-shapes
        (match-lambda
          ['()
           '()]
          [(list-rest (list-rest '#%suite suite-body) tokens)
           (cons (cons '#%suite (tag-paren-shapes suite-body))
                 (tag-paren-shapes tokens))]
          [(list-rest `(OP ,(and open (app syntax-e
                                           (or "(" "[" "{"))))
                      tokens)
           (let-values ([{this tokens}
                         (group-inside open tokens)])
             (cons this (tag-paren-shapes tokens)))]
          [(list-rest this tokens)
           (cons this (tag-paren-shapes tokens))])]
       [group-inside
        (λ (open-stx tokens)
          (define-values [tag close]
            (match (syntax-e open-stx)
              ["(" (values '#%parens ")")]
              ["[" (values '#%brackets "]")]
              ["{" (values '#%braces "}")]))
          (let loop ([acc null]
                     [tokens tokens])
            (match tokens
              ['()
               '()]
              [(list-rest `(OP ,(app syntax-e (== close)))
                          tokens)
               (values (cons tag (fixup-group (reverse acc)))
                       tokens)]
              [(list-rest `(OP ,(and open (app syntax-e
                                               (or "(" "[" "{"))))
                tokens)
               (let-values ([{this tokens}
                             (group-inside open tokens)])
                 (loop (cons this acc) tokens))]
              [(list-rest this tokens)
               (loop (cons this acc) tokens)])))]
       [fixup-group
        (λ (grp)
          (let partition-commas
            ([acc null]
             [grp (filter (match-lambda
                            [(cons (or 'NEWLINE 'NL) _)
                             #f]
                            [_
                             #t])
                          grp)])
            (match grp
              ['()
               (list (reverse acc))]
              [(cons `(OP ,(app syntax-e ",")) grp)
               (cons (reverse acc)
                     (partition-commas null grp))]
              [(cons this grp)
               (partition-commas (cons this acc) grp)])))])
       tag-paren-shapes))
            

(define tag-lines
  (letrec ([tag-lines
            (λ (tokens)
              (let loop ([acc null]
                         [tokens tokens])
                (match tokens
                  ['()
                   (continue acc #f)]
                  [(cons (list (or 'NEWLINE 'NL 'ENDMARKER)
                               _)
                         tokens)
                   (continue acc tokens)]
                  [(cons (cons '#%suite suite-body)
                         tokens)
                   (define suite-body* (tag-lines suite-body))
                   (loop (if (null? suite-body*)
                             acc
                             (cons (cons '#%suite suite-body*)
                                   acc))
                         tokens)]
                  [(cons this tokens)
                   (loop (cons this acc) tokens)])))]
           [continue
            (λ (acc maybe-tokens)
              (define (get-rest)
                (if maybe-tokens
                    (tag-lines maybe-tokens)
                    null))
              (match acc
                ['()
                 (get-rest)]
                [_
                 (cons (cons '#%line (reverse acc))
                       (get-rest))]))])
    tag-lines))


(define parse-import
  (match-lambda
    [(list-rest '#%line
                (name: "import")
                lst) ;; dotted-as?-name+
     (letrec ([start
               (match-lambda
                 ['() '()]
                 [(cons (name: init) lst)
                  (loop (list init) lst)])]
              [loop
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
    [(list-rest '#%line
                (name: from)
                lst)
     (let-values
         ([{acc lst}
           (match lst
             [(cons (op: (and rel "." "..")) lst)
              (values (list rel) lst)]
             [_
              (values null lst)])])
       (let loop ([acc acc]
                  [lst lst])
         (match lst
           [(cons (name: "import") lst)
            `(import #:from ,(cons '|.| (reverse acc))
                     ,@(parse-import-from-rhs lst))]
           [(cons (op: ".") lst)
            (loop acc lst)]
           [(cons (name: n) lst)
            (loop (cons n acc) lst)])))]))

(define parse-import-from-rhs
  (letrec ([parse-import-from-rhs
            (match-lambda
              [(list (op: "*"))
               '(#:*)]
              [(list (op: "(") lst ... (op: ")"))
               (help lst)]
              [lst
               (help lst)])]
           [help
            (match-lambda
              ['()
               null]
              [(list-rest (name: n)
                          (name: "as")
                          (name: as)
                          lst)
               (list* n '#:as as (continue lst))]
              [(list-rest (name: n) lst)
               (cons n (continue lst))])]
           [continue
            (match-lambda
              ['()
               null]
              [(cons (op: ",") lst)
               (help lst)])])
    parse-import-from-rhs))
            
  
  

(define parse-def values)

(define parse-class values)


