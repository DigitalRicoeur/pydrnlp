#lang racket

(require parser-tools/lex
         parser-tools/yacc
         python-tokenizer
         racket/match
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(define current-source
  (make-parameter #f))

;; FIXME python-tokenizer is based on
;;   python2, not python3, but seems to work for now.

;; https://docs.python.org/3.7/library/ast.html
;;   "Warning: It is possible to crash the Python interpreter
;;    with a sufficiently large/complex string due to
;;    stack depth limitations in Python’s AST compiler."
;; And you still don't think you need tail calls?

(define-syntax (define-python-tokens stx)
  (define (make-ctor-id id)
    (format-id id "token-~a" id))
  (define-syntax-class clause
    #:attributes {[name 1] [match*-clause 1]}
    (pattern base:id
             #:with (name ...) #'(base)
             #:with ctor (make-ctor-id #'base)
             #:with (match*-clause ...)
             #'([{'base _} (ctor lexeme)]))
    (pattern [base:id ([special:id val:str] ...)]
             #:with (name ...) #'(base special ...)
             #:with (base-ctor special-ctor ...)
             (map make-ctor-id
                  (syntax->list #'(base special ...)))
             #:with (match*-clause ...)
             #'([{'special val} (special-ctor lexeme)]
                ...
                [{'base _} (base-ctor lexeme)]))) 
  (syntax-parse stx
    [(_ [grp ->ctor]
        :clause ...)
     #'(begin (define-tokens grp ((~@ name ...) ...))
              (define (->ctor type lexeme)
                (match* {type (syntax-e lexeme)}
                  (~@ match*-clause ...) ...)))]))

(define (lex/thunk orig-in)
  (define src (current-source))
  (define-values [new-in line-start-pos-tbl]
    (let-values ([{new-in out1} (make-pipe #f src)]
                 [{tbl-in out2} (make-pipe #f src)])
      (port-count-lines! new-in)
      (port-count-lines! tbl-in)
      (thread (λ ()
                (copy-port orig-in out1 out2)
                (close-output-port out1)
                (close-output-port out2)))
      (values new-in
              (for/hasheqv ([i (in-naturals)])
                (define-values [ln col pos]
                  (port-next-location tbl-in))
                #:final (eof-object? (read-line tbl-in 'any))
                (values i pos)))))
  (define-values [avail? get]
    (sequence-generate (generate-tokens new-in)))
  (define (->position lst)
    (match-define (list line col) lst)
    (position (+ col (hash-ref line-start-pos-tbl line))
              line
              col))
  (if (avail?)
      (match (get)
        [(list type
               datum-lexeme
               (app ->position
                    (and start-pos-struct
                         (position start-pos start-ln start-col)))
               (app ->position end-pos-struct)
               current-line-str)
         (define lexeme
           (datum->syntax #f lexeme
                          (list src start-ln start-col start-pos
                                (- (position-offset end-pos-struct)
                                   start-pos))))
         (position-token
          (construct-python-token type lexeme)
          start-pos-struct
          end-pos-struct)])
      eof))

(define-python-tokens [python-tokens construct-python-token]
  [NAME ([IMPORT "import"]
         [FROM "from"]
         [AS "as"]
         [DEF "def"]
         [CLASS "class"]
         [ASYNC "async"])]
  [OP ([DOT "."]
       [ELLIPSIS "..."]
       [COMMA ","]
       [EQUAL "="]
       [ARROW "->"]
       [COLON ":"]
       [STAR "*"]
       [STAR-STAR "**"]
       [AT "@"]
       [IF "if"]
       [ELSE "else"]
       [LAMBDA "lambda"]
       [OPEN "("]
       [CLOSE ")"])]
  NUMBER STRING COMMENT
  NL NEWLINE DEDENT INDENT
  ERRORTOKEN ENDMARKER)

;; The only difference between 'NEWLINE and 'NL
;; is that 'NEWLINE will only occurs if the
;; indentation level is at 0.

(define parse-thunk
  ;; https://docs.python.org/3/reference/grammar.html
  (parser
   [src-pos]
   [tokens python-tokens]
   [start file-input]
   [end ENDMARKER]
   [error (λ (tok-ok? tok-name tok-value start-pos end-pos)
            (error 'ERROR "todo"))]
   [grammar
    (file-input [{}
                 null]
                [{import-stmt file-input}
                 (cons $1 $2)]
                [{STRING file-input}
                 (cons $1 $2)]
                ;; plain assignment
                ;; funcdef | classdef | decorated
                )
    (dotted-name [{NAME}
                  (list $1)]
                 [{NAME DOT dotted-name}
                  (list* $1 $2 $3)])
    (newline [{NEWLINE}
              $1]
             [{NL}
              $1])
    ;; funcdef
    (complete-funcdef [{decorator* ASYNC plain-funcdef}
                       (list $1 $2 $3)]
                      [{decorator* plain-funcdef}
                       (list $1 $2)])
    (plain-funcdef [{DEF NAME fun-sig COLON NEWLINE
                         INDENT suite-rest}
                    (list $1 $2 $3 $7)])
    (fun-sig [{OPEN typedargslist CLOSE}
              (list $2)]
             [{OPEN typedargslist CLOSE ARROW test}
              (list $2 $5)])
    (typedargslist [{}
                    null]
                   [{COMMA}
                    null]
                   [{STAR-STAR tfpdef typedargslist}
                    (cons (list $1 $2) $3)]
                   [{STAR tfpdef typedargslist}
                    (cons (list $1 $2) $3)]
                   [{tfpdef EQUAL test typedargslist}
                    (cons (list $1 $3) $4)]
                   [{tfpdef typedargslist}
                    (cons (list $1) $2)])
    (tfpdef [{NAME COLON test}
             (list $1 $3)]
            [{NAME}
             (list $1 #f)])
    (suite-rest [{DEDENT}
                 #f]
                [{INDENT suite-rest DEDENT suite-rest}
                 #f]
                [{STRING suite-rest}
                 $1]
                [{COMMENT suite-rest}
                 $2]
                [{newline suite-rest}
                 $2]
                [{suite-ignored suite-rest}
                 ;; FAKE
                 #f])
    ;; decorator
    (decorator* [{}
                 null]
                [{decorator decorator*}
                 (cons $1 $2)])
    (decorator [{AT dotted-name OPEN arglist CLOSE newline}
                (cons $2 $4)]
               [{AT dotted-name newline}
                (list $2)])
    (arglist [{argument}
              (list $1)]
             [{argument COMMA} ;; apparently
              (list $1)]
             [{argument COMMA arglist}
              (cons $1 $3)])
    ;; import
    (import-stmt [{IMPORT dotted-as?-name+}
                  (list $1 $2)]
                 [{FROM from-import-lhs IMPORT from-import-rhs}
                  (list $1 $2 $3 $4)])
    (from-import-lhs [{rel-path-elem* dotted-name}
                      (append $1 $2)]
                     [{rel-path-elem+}
                      $1])
    (from-import-rhs [{STAR}
                      $1]
                     [{OPEN plain-as?-name+ CLOSE}
                      $2]
                     [{plain-as?-name+}
                      $1])
    (plain-as?-name+ [{plain-as?-name COMMA}
                      ;; apparently
                      (list $1)] 
                     [{plain-as?-name}
                      (list $1)]
                     [{plain-as?-name COMMA plain-as?-name+}
                      (cons $1 $3)])
    (dotted-as?-name+ [{dotted-as?-name}
                       (list $1)]
                      [{dotted-as?-name COMMA dotted-as?-name+}
                       (cons $1 $3)])
    (plain-as?-name [{NAME}
                     (list $1)]
                    [{NAME AS NAME}
                      (list $1 $2 $3)])
    (dotted-as?-name [{dotted-name}
                      (list $1)]
                     [{dotted-name AS dotted-name}
                      (list $1 $2 $3)])
    (rel-path-elem* [{}
                     null]
                    [{rel-path-elem rel-path-elem*}
                     (cons $1 $2)])
    (rel-path-elem+ [{rel-path-elem}
                     (list $1)]
                    [{rel-path-elem rel-path-elem+}
                     (cons $1 $2)])
    (rel-path-elem [{DOT}
                    $1]
                   [{ELLIPSIS}
                    $1])]))
