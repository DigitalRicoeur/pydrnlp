#lang racket

(require db
         sql
         adjutor
         racket/stxparam
         syntax/parse/define)

(provide define/db
         with-db
         for/insert-statement
         call-with-transaction/db
         table-exists?/db
         query-exec/db
         in-query/db
         query-maybe-row/db
         query-row/db 
         query-list/db
         query-maybe-value/db
         query-value/db)

(define-syntax-parameter local-tokenizer-cache-db #f)

(define-for-syntax (get-local-tokenizer-cache-db orig-stx)
  (or (syntax-parameter-value #'local-tokenizer-cache-db)
      (raise-syntax-error #f "used outside of a local-tokenizer-cache-db context" orig-stx)))

(define-for-syntax (make-query/db-transformer proc-id)
  (syntax-parser
    [ho:id
     #:with proc proc-id
     #:with db (get-local-tokenizer-cache-db this-syntax)
     #'(curry proc db)]
    [(_ arg ...+)
     #:with proc proc-id
     #:with db (get-local-tokenizer-cache-db this-syntax)
     #'(proc db arg ...)]))

(define-syntax-parser define-query/db
  [(_ (~seq name/db:id name:id) ...)
   #'(begin (define-syntax name/db
              (make-query/db-transformer (quote-syntax name)))
            ...)])

(define-query/db
  call-with-transaction/db call-with-transaction
  table-exists?/db table-exists?
  query-exec/db query-exec
  *in-query/db in-query
  query-maybe-row/db query-maybe-row
  query-row/db query-row
  query-list/db query-list
  query-maybe-value/db query-maybe-value
  query-value/db query-value)

(define-sequence-syntax in-query/db
  (λ () #'*in-query/db)
  (syntax-parser
    [[lhs (_ arg ...+)]
     #:with db (get-local-tokenizer-cache-db this-syntax)
     #'[lhs (in-query db arg ...)]]))


(define-syntax-parser with-db
  [(_ #:db db:id body:expr ...+)
   #'(syntax-parameterize ([local-tokenizer-cache-db #'db])
       body ...)])

(define-syntax-parser define/db
  [(_ (name:id kw-formal ...) body:expr ...+)
   #:with proc-name (syntax-local-introduce #'name)
   #:with ooo #'(... ...)
   #'(begin
       (define (proc-name kw-formal ... #:db db)
         (with-db #:db db body ...))
       (define-syntax-parser name
         [(_ (~alt (~once (~seq #:db _:expr))
                   (~not #:db))
             ooo)
          #`(proc-name #,@(cdr (syntax->list this-syntax)))]
         [(_ (~and arg (~not #:db)) ooo)
          #:fail-unless (syntax-parameter-value #'local-tokenizer-cache-db)
          "#:db argument required outside of local-tokenizer-cache-db context"
          #:with db (syntax-parameter-value #'local-tokenizer-cache-db)
          #'(proc-name #:db db arg ooo)]
         [:id
          (define db-stx (syntax-parameter-value #'local-tokenizer-cache-db))
          (if db-stx #`(curry proc-name #:db #,db-stx) #'proc-name)]))])

(TODO/void for/insert-statement #: avoid copying code)

(define-syntax-parser for/insert-statement
  ;; copied and pasted from ricoeur/tei/search/postgresql
  [(_ (#:into name-ast-form
       for-clause ...)
      body-or-break ...
      #:set
      [column-ident-form
       scalar-expr-form] ...)
   #`(insert
      #:into name-ast-form
      #:columns column-ident-form ...
      #:from
      (TableExpr:AST
       ,(make-values*-table-expr-ast
         (for/fold/derived #,this-syntax
                           ([so-far '()])
           (for-clause ...)
           body-or-break ...
           (cons (list (scalar-expr-qq scalar-expr-form)
                       ...)
                 so-far)))))])
