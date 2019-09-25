#lang racket

(require racket/runtime-path
         racket/fixnum
         db
         sql
         racket/fasl
         ricoeur/tei
         "tokenize-one-doc.rkt"
         "worker.rkt"
         "types.rkt"
         pydrnlp/support
         racket/stxparam
         syntax/parse/define
         (for-syntax racket/base
                     syntax/transformer))

(provide (contract-out
          [get/build-tokenized-corpus
           (->* [(instance-set/c tei-document?)]
                [#:quiet? any/c]
                tokenized-corpus?)]
          ))

(define-runtime-path trends-cache-db.sqlite
  "trends-cache-db.sqlite")

(define (get/build-tokenized-corpus docs #:quiet? [quiet? #t])
  (define cust (make-custodian))
  (define db
    (when trends-engine-revision
      (sqlite3-connect #:database trends-cache-db.sqlite
                       #:use-place #t
                       #:mode 'create)))
  (dynamic-wind
   void
   (λ ()
     (parameterize ([current-custodian cust])
       (if trends-engine-revision
           (call-with-transaction
            db
            (λ ()
              (query-exec db "PRAGMA foreign_keys = ON")
              (check-cache-db-format! db)
              (do-get/build docs #:db db #:quiet? quiet?)))
           (do-get/build docs #:db db #:quiet? quiet?))))
   (λ ()
     (custodian-shutdown-all cust)
     (when trends-engine-revision
       (disconnect db)))))


(define-simple-macro (define/db (name:id kw-formal ...) body:expr ...+)
  (define (name #:db db kw-formal ...)
    (syntax-parameterize ([local-db (make-variable-like-transformer #'db)])
      body ...)))

(define/db (do-get/build docs #:quiet? quiet?)
  (define cacheTokenizerRevisionFasl
    (s-exp->fasl trends-engine-revision))
  (define checksum-table
    (tei-document-set->checksum-table docs))
  (define corpusChecksumTableFasl
    (s-exp->fasl checksum-table))
  (match (and trends-engine-revision
              (query-maybe-row/db
               (select corpusLemmaCountFasl
                       corpusLemmaStringFasl
                       #:from tTokenizedCorpora
                       #:where
                       (= corpusChecksumTableFasl
                          ,corpusChecksumTableFasl)
                       (= cacheTokenizerRevisionFasl
                          ,cacheTokenizerRevisionFasl))))
    [(vector (app fasl->lemma/count corpus:l/c)
             (app fasl->lemma/string corpus:l/s))
     (define plain-info
       (instance-set->plain docs))
     (query-exec/db
      (update tTokenizedCorpora
              #:set [accessedPosix ,(current-seconds)]
              #:where
              (= corpusChecksumTableFasl
                 ,corpusChecksumTableFasl)
              (= cacheTokenizerRevisionFasl
                 ,cacheTokenizerRevisionFasl)))
     (query-exec/db
      (update tTokenizedDocuments
              #:set [accessedPosix ,(current-seconds)]
              #:where
              (exists
               (select 1
                       #:from tTokenizedCorporaDocuments
                       #:where
                       (= tTokenizedDocuments.docTitle
                          tTokenizedCorporaDocuments.docTitle)
                       (= tTokenizedDocuments.docChecksum
                          tTokenizedCorporaDocuments.docChecksum)
                       (= corpusChecksumTableFasl
                          ,corpusChecksumTableFasl)
                       (= cacheTokenizerRevisionFasl
                          ,cacheTokenizerRevisionFasl)))))
     (define tokenized-docs
       (for/instance-set ([{title-str l/c-fasl}
                           (in-query/db
                            (select docTitle docLemmaCountFasl
                                    #:from (inner-join tTokenizedDocuments
                                                       tTokenizedCorporaDocuments
                                                       #:natural)
                                    #:where
                                    (= corpusChecksumTableFasl
                                       ,corpusChecksumTableFasl)
                                    (= cacheTokenizerRevisionFasl
                                       ,cacheTokenizerRevisionFasl)))])
         (tokenized-document (instance-set-ref plain-info (string->symbol title-str))
                             (fasl->lemma/count l/c-fasl))))
     (tokenized-corpus tokenized-docs
                       corpus:l/c
                       corpus:l/s)]
    [_
     (build-tokenized-corpus docs
                             #:db local-db
                             #:revision-fasl cacheTokenizerRevisionFasl
                             #:checksum-table-fasl corpusChecksumTableFasl
                             #:checksum-table checksum-table
                             #:quiet? quiet?)]))


(define/db (build-tokenized-corpus docs
                                   #:revision-fasl cacheTokenizerRevisionFasl
                                   #:checksum-table-fasl corpusChecksumTableFasl
                                   #:checksum-table checksum-table
                                   #:quiet? quiet?)
  (define-values [found corpus:lemma/count corpus:lemma/string]
    (if trends-engine-revision
        (get-cached-documents #:db local-db
                              #:revision-fasl cacheTokenizerRevisionFasl
                              #:checksum-table-fasl corpusChecksumTableFasl
                              #:checksum-table checksum-table)
        (values (instance-set) empty:lemma/count empty:lemma/string)))
  (define docs-to-do
    (set-subtract docs found))
  (define py (launch-trends-engine #:quiet? quiet?))
  (define corpus
    (for/fold ([found found]
               [corpus:lemma/count corpus:lemma/count]
               [corpus:lemma/string corpus:lemma/string]
               #:result (tokenized-corpus found
                                          corpus:lemma/count
                                          corpus:lemma/string))
              ([d (in-instance-set docs-to-do)])
      (define info (get-plain-instance-info d))
      (define checksum-str (symbol->string (tei-document-checksum d)))
      (match-define (doc+strings tokenized l/s)
        (tokenize-one-doc d #:tokenizer py))
      (define l/c (tokenized-document-lemma/count tokenized))
      (when trends-engine-revision
        (define now (current-seconds))
        (query-exec/db
         (insert #:into tTokenizedDocuments
                 #:set 
                 [docTitle ,(instance-title info)]
                 [docChecksum ,checksum-str]
                 [cacheTokenizerRevisionFasl ,cacheTokenizerRevisionFasl]
                 [docLemmaCountFasl ,(lemma/count->fasl l/c)]
                 [docLemmaStringFasl ,(lemma/string->fasl l/s)]
                 [createdPosix ,now]
                 [accessedPosix ,now])))
      (values (set-add found tokenized)
              (union:lemma/count corpus:lemma/count l/c)
              (union:lemma/string corpus:lemma/string l/s))))
  (python-worker-kill py)
  (when trends-engine-revision
    (let ([now (current-seconds)])
      (query-exec/db
       (insert #:into tTokenizedCorpora
               #:set
               [corpusChecksumTableFasl ,corpusChecksumTableFasl]
               [cacheTokenizerRevisionFasl ,cacheTokenizerRevisionFasl]
               [corpusLemmaCountFasl ,(lemma/count->fasl
                                       (tokenized-corpus-lemma/count corpus))]
               [corpusLemmaStringFasl ,(lemma/string->fasl
                                        (tokenized-corpus-lemma/string corpus))]
               [createdPosix ,now]
               [accessedPosix ,now])))
    (query-exec/db
     (for/insert-statement (#:into tTokenizedCorporaDocuments
                            [{title-sym checksum-sym}
                             (in-immutable-hash checksum-table)])
       #:set
       [corpusChecksumTableFasl ,corpusChecksumTableFasl]
       [cacheTokenizerRevisionFasl ,cacheTokenizerRevisionFasl]
       [docTitle ,(symbol->string title-sym)]
       [docChecksum ,(symbol->string checksum-sym)])))
  corpus)



(define/db (get-cached-documents docs
                                 #:revision-fasl cacheTokenizerRevisionFasl
                                 #:checksum-table-fasl corpusChecksumTableFasl
                                 #:checksum-table checksum-table)
  (define wanted-ast
    (make-values*-table-expr-ast
     (for/list ([d (in-instance-set docs)])
       (list (value->scalar-expr-ast (instance-title d))
             (value->scalar-expr-ast (symbol->string
                                      (tei-document-checksum d)))))))
  (query-exec/db
   (update tTokenizedDocuments
           #:set [accessedPosix ,(current-seconds)]
           #:where
           (= cacheTokenizerRevisionFasl ,cacheTokenizerRevisionFasl)
           (in (%row docTitle docChecksum)
               #:from (TableExpr:AST ,wanted-ast))))
  (for/fold ([found (instance-set)]
             [corpus:lemma/count empty:lemma/count]
             [corpus:lemma/string empty:lemma/string])
            ([{title-str l/c-fasl l/s-fasl}
              (in-query/db
               (select docTitle docLemmaCountFasl docLemmaStringFasl
                       #:from tTokenizedDocuments
                       #:where
                       (= cacheTokenizerRevisionFasl ,cacheTokenizerRevisionFasl)
                       (in (%row docTitle docChecksum)
                           #:from (TableExpr:AST ,wanted-ast))))])
    (define l/c (fasl->lemma/count l/c-fasl))
    (define l/s (fasl->lemma/string l/s-fasl))
    (values (set-add found
                     (tokenized-document (get-plain-instance-info
                                          (instance-set-ref docs (string->symbol title-str)))
                                         l/c))
            (union:lemma/count corpus:lemma/count l/c)
            (union:lemma/string corpus:lemma/string l/s))))
                                        
;                                         ;; 
;                                         ;; 
;    ;; ;;; ;;  ; ;;   ; ;;    ;;;   ;; ;;;;;
;  ;;  ; ;; ;;  ;;  ;  ;;  ;  ;   ;  ;;;  ;; 
;   ;    ;; ;;  ;;  ;  ;;  ;  ;   ;  ;;   ;; 
;    ;;  ;; ;;  ;;  ;; ;;  ;;;;   ;; ;;   ;; 
;      ;;;; ;;  ;;  ;  ;;  ;  ;   ;  ;;   ;; 
;  ;   ;  ; ;;  ;;  ;  ;;  ;  ;   ;  ;;    ; 
;   ;;;   ;;;;  ;;;;   ;;;;    ;;;   ;;    ;;
;               ;;     ;;                    
;               ;;     ;;                    
;               ;;     ;;                    
;                                            


(define (tei-document-set->checksum-table docs)
  (TODO/void tei-document-set->checksum-table #: probably better elsewhere)
  (for/hasheq ([doc (in-instance-set docs)])
    (values (instance-title/symbol doc)
            (tei-document-checksum doc))))

(define-syntax-parameter local-db
  (λ (stx) (raise-syntax-error #f "used out of context" stx)))

(define-simple-macro (define-query/db (~seq name/db:id name:id) ...)
  (begin (define-simple-macro (name/db s:expr)
           (name local-db s))
         ...))

(define-query/db
  query-exec/db query-exec
  query-maybe-row/db query-maybe-row
  query-row/db query-row
  query-list/db query-list
  query-maybe-value/db query-maybe-value
  query-value/db query-value)

(define-sequence-syntax in-query/db
  (λ (stx) (raise-syntax-error #f "only allowed in for-like clauses" stx))
  (syntax-parser
    [[lhs (_ arg ...+)]
     #'[lhs (in-query local-db arg ...)]]))



(define (check-cache-db-format! db)
  (define ident-ast:tCacheDbFormatVersion
    (ident-qq tCacheFormatVersion))
  (match (and (table-exists? db (ast->sqlite3 ident-ast:tCacheDbFormatVersion))
              (query-list
               db
               (select cacheDbFormatVersion
                       #:from (Ident:AST ,ident-ast:tCacheDbFormatVersion))))
    [(list it)
     #:when (eqv? it cache-db-format-version)
     (void)]
    [_
     (query-exec db (ident-ast->drop-statement ident-ast:tCacheDbFormatVersion))
     (for ([stmnt (in-list drop-cache-tables-statements)])
       (query-exec db stmnt))
     (query-exec db
                 (create-table (Ident:AST ,ident-ast:tCacheDbFormatVersion)
                               #:columns [cacheDbFormatVersion int #:not-null]))
     (query-exec db
                 (insert #:into (Ident:AST ,ident-ast:tCacheDbFormatVersion)
                         #:set [cacheDbFormatVersion ,cache-db-format-version]))
     (for ([stmnt (in-list create-cache-tables-statements)])
       (query-exec db stmnt))]))


(define (ast->sqlite3 ast)
  (sql-ast->string ast 'sqlite3))

(define (ident-ast->drop-statement ast)
  (string-append "DROP TABLE IF EXISTS "
                 (ast->sqlite3 ast)))

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



(define-syntax-parser define-cache-tables
  #:literals {create-table}
  #:track-literals
  [(_ {(~alt (~optional (~seq #:create create-cache-tables-statements:id))
             (~optional (~seq #:drop drop-cache-tables-statements:id))
             (~optional (~seq #:clear clear-cache-tables-statements:id)))
       ...}
      (create-table create-name:id create-body ...)
      ...)
   #:with (delete-name ...) (reverse (syntax->list #'(create-name ...)))
   #'(begin
       (~? (define create-cache-tables-statements
             (list (create-table create-name create-body ...) ...)))
       (~? (define clear-cache-tables-statements
             (list (delete #:from delete-name) ...)))
       (~? (define drop-cache-tables-statements
             (map ident-ast->drop-statement
                  (list (ident-qq delete-name) ...)))))])

(define/contract cache-db-format-version
  fixnum-for-every-system?
  1)

(define-cache-tables {#:create create-cache-tables-statements
                      #:drop drop-cache-tables-statements}
  (create-table tTokenizedDocuments
                #:columns
                [docTitle text #:not-null]
                [docChecksum text #:not-null]
                [cacheTokenizerRevisionFasl blob #:not-null]
                [docLemmaCountFasl blob #:not-null]
                [docLemmaStringFasl blob #:not-null]
                [createdPosix int #:not-null]
                [accessedPosix int #:not-null]
                #:constraints
                (primary-key docTitle docChecksum cacheTokenizerRevisionFasl))
  (create-table tTokenizedCorpora
                #:columns
                [corpusChecksumTableFasl blob #:not-null]
                [cacheTokenizerRevisionFasl blob #:not-null]
                [corpusLemmaCountFasl blob #:not-null]
                [corpusLemmaStringFasl blob #:not-null]
                [createdPosix int #:not-null]
                [accessedPosix int #:not-null]
                #:constraints
                (primary-key corpusChecksumTableFasl cacheTokenizerRevisionFasl))
  (create-table tTokenizedCorporaDocuments
                #:columns
                [corpusChecksumTableFasl blob #:not-null]
                [cacheTokenizerRevisionFasl blob #:not-null]
                [docTitle text #:not-null]
                [docChecksum text #:not-null]
                #:constraints
                (primary-key corpusChecksumTableFasl cacheTokenizerRevisionFasl docTitle)
                (foreign-key corpusChecksumTableFasl cacheTokenizerRevisionFasl
                             #:references tTokenizedCorpora
                             #:on-delete #:cascade)
                (foreign-key docTitle docChecksum cacheTokenizerRevisionFasl
                             #:references tTokenizedDocuments
                             #:on-delete #:restrict)))

