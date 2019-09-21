#lang racket

(require python-tokenizer
         syntax/parse
         syntax/strip-context
         racket/match)

(provide python-body-read-syntax
         python-body-read)

;; TODO: un-escape doc-strings

(define (python-body-read-syntax src in)
  (parse src in))

(define (python-body-read in)
  (map syntax->datum
       (parse #f in)))

#;
(modbegin
 (~optional :str)
 (or* import
      =
      def
      class))

(struct token (type datum stx)
  #:transparent)

(define (tokenize src in)
  (define line-start-pos-tbl
    (let ([in (peeking-input-port in)])
      (port-count-lines! in)
      (for/hasheqv ([i (in-naturals 1)])
        (define-values [ln col pos]
          (port-next-location in))
        #:final (eof-object? (read-line in 'any))
        (values i pos))))
  (datum->syntax
   #f
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
     (list type
           (datum->syntax #f datum-lexeme loc)))))
                         
(define-literal-set token-type-literals
  ;; The only difference between 'NEWLINE and 'NL
;; is that 'NEWLINE will only occurs if the
;; indentation level is at 0.
    #:datum-literals {NAME OP NUMBER STRING COMMENT
                           NL NEWLINE DEDENT INDENT
                           ERRORTOKEN ENDMARKER}
    ())

(define-syntax-class comment/newline
  #:literal-sets {token-type-literals}
  (pattern (COMMENT _))
  (pattern (NEWLINE _))
  (pattern (NL _)))

(define-syntax-class op-open
  #:literal-sets {token-type-literals}
  (pattern (OP "(")))
(define-syntax-class op-close
  #:literal-sets {token-type-literals}
  (pattern (OP ")")))
(define-syntax-class op-comma
  #:literal-sets {token-type-literals}
  (pattern (OP ",")))
(define-syntax-class op-dot
  #:literal-sets {token-type-literals}
  (pattern (OP ".")))
(define-syntax-class op-...
  #:literal-sets {token-type-literals}
  (pattern (OP "...")))

(define-splicing-syntax-class dotted-name
  #:attributes {[name 1]}
  #:literal-sets {token-type-literals}
  (pattern (~seq (NAME n0)
                 (~seq :op-dot (NAME n)) ...)
           #:with (name ...) #'(n0 n ...)))

(define-splicing-syntax-class dotted-as?-name
  #:attributes {parsed}
  #:literal-sets {token-type-literals}
  (pattern (~seq :dotted-name
                 (~optional (~seq #:as (NAME as-name))))
           #:with parsed
           #'(name ... (~? (~@ #:as as-name)))))

(define-splicing-syntax-class plain-as?-name
  #:attributes {parsed}
  #:literal-sets {token-type-literals}
  (pattern (~seq (NAME name)
                 (~optional (~seq #:as (NAME as-name))))
           #:with parsed
           #'(name (~? (~@ #:as as-name)))))

(define-splicing-syntax-class import-from-lhs
  #:attributes {[name 1]}
  #:literal-sets {token-type-literals}
  (pattern (~seq (OP (~and rel (~or* "." ".."))) ...
                 n:dotted-name)
           #:with (name ...) #'(rel ... n.name ...))
  (pattern (~seq (OP (~and name (~or* "." ".."))) ...+)))

(define-splicing-syntax-class import-from-rhs
  #:attributes {parsed}
  #:literal-sets {token-type-literals}
  (pattern (~seq (OP "*"))
           #:with parsed #'#:*)
  (pattern (~seq :op-open
                 n0:plain-as?-name
                 (~seq :op-comma n:plain-as?-name) ...
                 :op-close)
           #:with parsed #'(n0.parsed n.parsed ...))
  (pattern (~seq n0:plain-as?-name
                 (~seq :op-comma n:plain-as?-name) ...)
           #:with parsed #'(n0.parsed n.parsed ...)))

(define-splicing-syntax-class import-stmt
  #:attributes {parsed}
  #:literal-sets {token-type-literals}
  (pattern (~seq (NAME "import")
                 ~!
                 spec0:dotted-as?-name
                 (~seq op-comma spec:dotted-as?-name) ...)
           #:with parsed #'(import spec0.parsed
                                   spec.parsed
                                   ...))
  (pattern (~seq (NAME "from")
                 ~!
                 lhs:import-from-lhs
                 (NAME "import")
                 rhs:import-from-rhs)
           #:with parsed
           #'(import #:from (lhs.name ...) rhs.parsed)))


(define-splicing-syntax-class fun-arg
  #:attributes {parsed}
  #:literal-sets {token-type-literals}
  (pattern (~seq (NAME parsed)))
  (pattern (~seq (NAME name)
                 (OP "=")
                 (~or* (OP (~not ","))
                       (NAME _)
                       (NUMBER _)
                       (STRING _)
                       :comment/newline)
                 ...)
           #:with parsed #'[name])
  (pattern (~seq (OP "*") (NAME name))
           #:with parsed #'[#:* name])
  (pattern (~seq (OP "**") (NAME name))
           #:with parsed #'[#:** name]))

(define-syntax-class num-positive-int
  #:attributes {parsed}
  #:literal-sets {token-type-literals}
  (pattern (NUMBER s)
           #:do [(define n
                   (string->number (syntax->datum #'s)))]
           #:fail-unless (exact-nonnegative-integer? n)
           "expected an exact-nonnegative-integer?"
           #:with parsed (datum->syntax #f n #'s)))
       

(define-splicing-syntax-class misc-form
  #:literal-sets {token-type-literals}
  (pattern (~seq (or* (STRING _)
                      (NUMBER _)
                      (OP _)
                      (NAME _))))
  (pattern :suite))

(define-splicing-syntax-class suite
  #:literal-sets {token-type-literals}
  (pattern (~seq (INDENT _)
                 (~or* :misc-form
                       ;; need ~seq to avoid internal error
                       (~seq :comment/newline))
                 ...
                 (DEDENT _))))

(define-splicing-syntax-class fun-stmnt
  #:attributes {parsed}
  #:literal-sets {token-type-literals}
  ;; TODO: support annotations
  (pattern (~seq (~optional (NAME (~and async
                                        "async")))
                 (NAME "def")
                 ~!
                 (NAME name)
                 :op-open :comment/newline ...
                 arg0:fun-arg
                 (~seq :comment/newline ...
                       (OP ",")
                       :comment/newline ...
                       arg:fun-arg)
                 ...
                 _:comment/newline ...
                 _:op-close
                 (OP ":")
                 (~or* (NEWLINE _) (NL _))
                 (INDENT _)
                 _:comment/newline ...
                 (~optional (~seq (STRING docstring)
                                  :comment/newline ...+))
                 (~optional (~seq (NAME "return")
                                  ret:num-positive-int
                                  :comment/newline ...+))
                 _:misc-form
                 (DEDENT _))
           #:with async-kw (and (attribute async)
                                #'#:async)
           #:with parsed 
           #'(def name
               (arg0.parsed arg.parsed ...)
               (~? async-kw)
               (~? docstring #f)
               (~? (~@ #:return ret.parsed)))))

(define (parse src in)
  (map
   strip-context
   (syntax-parse (tokenize src in)
     #:literal-sets {token-type-literals}
     #:context 'python-body-read-syntax
     [(:comment/newline ... (STRING docstring) token ...)
      (cons #'docstring (parse-more #'(token ...)))]
     [_
      (parse-more this-syntax)])))

(define chomp-comment/newline
  (syntax-parser
    #:literal-sets {token-type-literals}
    [(:comment/newline ...+ token ...)
     #'(token ...)]
    [_
     this-syntax]))

(define parse-more
  (syntax-parser
    #:literal-sets {token-type-literals}
    #:context 'python-body-read-syntax:parse-more
    [()
     null]
    [((NAME (~or* "import" "from")) _ ...)
     (syntax-parse this-syntax
       #:literal-sets {token-type-literals}
       #:context 'python-body-read-syntax:import
       [(import:import-stmt token ...)
        (cons #'import.parsed
              (parse-more #'(token ...)))])]
    [((~optional (NAME "async")) (NAME "def") _ ...)
     (syntax-parse this-syntax
       #:literal-sets {token-type-literals}
       #:context 'python-body-read-syntax:fun
       [(fun:fun-stmnt token ...)
        (cons #'fun.parsed
              (parse-more #'(token ...)))])]
    [((~or* :misc-form (COMMENT _)) ...
      (~or* (NEWLINE _) (NL _))
      token ...)
     (parse-more #'(token ...))]))
       
#;
(define (parse-more stx)
  (syntax-parse (chomp-comment/newline stx)
    #:literal-sets {token-type-literals}
    #:context 'python-body-read-syntax
    [()
     null]
    [(import:import-stmt token ...)
     (cons #'import.parsed
           (parse-more #'(token ...)))]
    [(fun:fun-stmnt token ...)
     (cons #'fun.parsed
           (parse-more #'(token ...)))]
    ;; TODO: class, attribute docstrings
    [(:misc-form token ...)
     (parse-more #'(token ...))]))



                  