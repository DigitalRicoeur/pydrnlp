#lang racket

(require ricoeur/tei
         pydrnlp
         sql
         db
         racket/runtime-path
         racket/fasl
         data/order
         racket/splicing
         "types.rkt"
         "db-query-support.rkt"
         syntax/parse/define)

;; no contracts so that the local db inference will work

(provide with-tokenizer+cache-db
         define/db
         try-select-cached-tokenized-corpus
         try-select-doc+strings
         insert-tokenized-corpus!
         insert-doc+strings!)

(define/contract tokenizer-cache-format-version
  fixnum? ;; better: fixnum32?
  0)

(define (ast->sqlite3 ast)
  (sql-ast->string ast 'sqlite3))

(define (ident-ast->drop-statement ast)
  (string-append "DROP TABLE IF EXISTS "
                 (ast->sqlite3 ast)))

(define-syntax-parser define-cache-tables
  #:literals {create-table}
  #:track-literals
  [(_ {#:create create-cache-tables-statements:id
       #:drop drop-cache-tables-statements:id
       #:clear clear-cache-tables-statements:id}
      (create-table create-name:id create-body ...)
      ...)
   #:with (delete-name ...)
   (reverse (syntax->list #'(create-name ...)))
   #'(begin
       (define create-cache-tables-statements
         (list (create-table create-name create-body ...) ...))
       (define clear-cache-tables-statements
         (list (delete #:from delete-name) ...))
       (define drop-cache-tables-statements
         (map ident-ast->drop-statement
              (list (ident-qq delete-name) ...))))])

(define-cache-tables {#:create create-cache-tables-statements
                      #:drop drop-cache-tables-statements
                      #:clear clear-cache-tables-statements}
  (create-table tCacheTokenizerRevision
                #:columns
                [cacheTokenizerRevisionFasl blob #:not-null])
  (create-table tTokenizedDocuments
                #:columns
                [docTitle text #:not-null]
                [docChecksum text #:not-null]
                [docLemmaCountFasl blob #:not-null]
                [docLemmaStringFasl blob #:not-null]
                #:constraints
                (primary-key docTitle))
  (create-table tTokenizedCorpora
                #:columns
                [corpusChecksumTableFasl blob #:not-null]
                [corpusLemmaCountFasl blob #:not-null]
                [corpusLemmaStringFasl blob #:not-null]
                #:constraints
                (primary-key corpusChecksumTableFasl))
  (create-table tTokenizedCorporaDocuments
                #:columns
                [corpusChecksumTableFasl blob #:not-null]
                [docTitle text #:not-null]
                #:constraints
                (primary-key corpusChecksumTableFasl docTitle)
                (foreign-key corpusChecksumTableFasl
                             #:references tTokenizedCorpora
                             #:on-delete #:cascade)
                (foreign-key docTitle
                             #:references tTokenizedDocuments
                             #:on-delete #:restrict))
  (create-table tTokenizedSegments
                #:columns
                [docTitle text #:not-null]
                [segSegmentMetaFasl blob #:not-null]
                [segLemmaCountFasl blob #:not-null]
                #:constraints
                (primary-key docTitle segSegmentMetaFasl)
                (foreign-key docTitle
                             #:references tTokenizedDocuments
                             #:on-delete #:cascade)))


;                                                  
;                                                  
;                                                  
;                                                  
;                   ;;;;                     ;;    
;                     ;;                     ;;    
;     ;;      ;;;     ;;      ;;;      ;;; ;;;;;;; 
;   ;;  ;   ;;   ;    ;;    ;;   ;   ;;   ;  ;;    
;    ;      ;    ;    ;;    ;    ;   ;       ;;    
;     ;;   ;;;;;;;;   ;;   ;;;;;;;; ;;       ;;    
;       ;;  ;         ;;    ;        ;       ;;    
;   ;   ;   ;;   ;     ;    ;;   ;   ;;   ;   ;    
;    ;;;      ;;;       ;;    ;;;      ;;;     ;;; 
;                                                  
;                                                  
;                                                  
;                                                  
                    


(define/db (try-select-cached-tokenized-corpus #:docs docs
                                               #:checksum-table checksum-table)
  (match (query-maybe-row/db
          (select corpusLemmaCountFasl
                  corpusLemmaStringFasl
                  #:from tTokenizedCorpora
                  #:where (= corpusChecksumTableFasl
                             ,(s-exp->fasl checksum-table))))
    [(vector (app fasl->lemma/count corpus:l/c)
             (app fasl->lemma/string corpus:l/s))
     (define tokenized-docs
       (for/instance-set ([doc (in-instance-set docs)])
         (define title-str (instance-title doc))
         (define doc:l/c
           (fasl->lemma/count
            (query-value/db
             (select docLemmaCountFasl
                     #:from tTokenizedDocuments
                     #:where (= docTitle ,title-str)))))
         (tokenized-document
          (get-plain-instance-info doc)
          doc:l/c
          (title->tokenized-segments title-str))))
     (tokenized-corpus tokenized-docs corpus:l/c corpus:l/s)]
    [_
     #f]))


(define/db (try-select-doc+strings doc)
  (define title-str (instance-title doc))
  (match (query-maybe-row/db
          (select docLemmaCountFasl docLemmaStringFasl
                  #:from tTokenizedDocuments
                  #:where
                  (= docTitle ,title-str)
                  (= docChecksum ,(symbol->string
                                   (tei-document-checksum doc)))))
    [(vector (app fasl->lemma/count doc:l/c)
             (app fasl->lemma/string doc:l/s))
     (doc+strings
      (tokenized-document
       (get-plain-instance-info doc)
       doc:l/c
       (title->tokenized-segments title-str))
      doc:l/s)]
    [_
     (call-with-transaction/db
      (λ ()
        (query-exec/db
         (delete #:from tTokenizedCorpora
                 #:where (in corpusChecksumTableFasl
                             #:from (select corpusChecksumTableFasl
                                            #:from tTokenizedCorporaDocuments
                                            #:where (= docTitle ,title-str)))))
        (query-exec/db
         (delete #:from tTokenizedDocuments
                 #:where (= docTitle ,title-str)))
        #false))]))


(define/db (title->tokenized-segments title-str)
  (sort (for/list ([{meta-fasl l/c-fasl}
                    (in-query/db
                     (select segSegmentMetaFasl
                             segLemmaCountFasl
                             #:from tTokenizedSegments
                             #:where (= docTitle ,title-str)))])
          (tokenized-segment
           (fasl->segment-meta meta-fasl)
           (fasl->lemma/count l/c-fasl)))
        (order-<? segment-order)))


;                                                  
;                                                  
;                                                  
;                                                  
;      ;                                     ;;    
;      ;;                                    ;;    
;   ;;;;;   ;; ;      ;;      ;;;   ;; ;;; ;;;;;;; 
;      ;;   ;;; ;   ;;  ;   ;;   ;  ;;;      ;;    
;      ;;   ;;  ;;   ;      ;    ;  ;;       ;;    
;      ;;   ;;  ;;    ;;   ;;;;;;;; ;;       ;;    
;      ;;   ;;  ;;      ;;  ;       ;;       ;;    
;      ;;   ;;  ;;  ;   ;   ;;   ;  ;;        ;    
;      ;;   ;;  ;;   ;;;      ;;;   ;;         ;;; 
;                                                  
;                                                  
;                                                  
;                                                  


(define/db (insert-doc+strings! d+s #:checksum-string checksum-str)
  (match-define (doc+strings (tokenized-document
                              (app instance-title title-str)
                              doc:l/c
                              segs)
                             doc:l/s)
    d+s)
  (call-with-transaction/db
   (λ ()
     (query-exec/db
      (insert #:into tTokenizedDocuments
              #:set
              [docTitle ,title-str]
              [docChecksum ,checksum-str]
              [docLemmaCountFasl ,(lemma/count->fasl doc:l/c)]
              [docLemmaStringFasl ,(lemma/string->fasl doc:l/s)]))
     (for ([segs (in-slice 1000 segs)])
       (query-exec/db
        (for/insert-statement (#:into tTokenizedSegments
                               [seg (in-list segs)])
          (match-define (tokenized-segment meta l/c) seg)
          #:set
          [docTitle ,title-str]
          [segSegmentMetaFasl ,(segment-meta->fasl meta)]
          [segLemmaCountFasl ,(lemma/count->fasl l/c)]))))))


(define/db (insert-tokenized-corpus! it
                                     #:checksum-table checksum-table)
  (define checksum-fasl (s-exp->fasl checksum-table))
  (match-define (tokenized-corpus docs l/c l/s) it)
  (call-with-transaction/db
   (λ ()
     (query-exec/db
      (insert #:into tTokenizedCorpora
              #:set 
              [corpusChecksumTableFasl ,checksum-fasl]
              [corpusLemmaCountFasl ,(lemma/count->fasl l/c)]
              [corpusLemmaStringFasl ,(lemma/string->fasl l/s)]))
     (for ([docs (in-slice 1000 docs)])
       (TODO/void in-slice documented as returning sequence?
                  #: does it actually return list?)
       (query-exec/db
        (for/insert-statement (#:into tTokenizedCorporaDocuments
                               [doc (in-list docs)])
          #:set
          [corpusChecksumTableFasl ,checksum-fasl]
          [docTitle ,(instance-title doc)]))))))

;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;      ;               ;     ;;        ;            ;;;;       ;                   
;      ;;              ;;    ;;        ;;             ;;       ;;                  
;   ;;;;;   ;; ;    ;;;;;  ;;;;;;;  ;;;;;     ;;      ;;    ;;;;;   ;;;;;;    ;;;  
;      ;;   ;;; ;      ;;    ;;        ;;    ;  ;     ;;       ;;       ;   ;;   ; 
;      ;;   ;;  ;;     ;;    ;;        ;;       ;;    ;;       ;;      ;    ;    ; 
;      ;;   ;;  ;;     ;;    ;;        ;;     ;;;;    ;;       ;;     ;    ;;;;;;;;
;      ;;   ;;  ;;     ;;    ;;        ;;    ;  ;;    ;;       ;;     ;     ;      
;      ;;   ;;  ;;     ;;     ;        ;;   ;;  ;;     ;       ;;    ;      ;;   ; 
;      ;;   ;;  ;;     ;;      ;;;     ;;    ;;; ;      ;;     ;;   ;;;;;;    ;;;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  

(define-runtime-path tokenizer-cache-db.sqlite
  "tokenizer-cache-db.sqlite")

(define-syntax-parser with-tokenizer+cache-db
  [(_ (~alt (~once (~seq #:tokenizer py:id))
            (~optional (~seq #:db db:id))
            (~optional (~seq #:quiet? quiet?:expr)))
      ...
      body:expr ...+)
   #'(let ([raw-db (tokenizer-cache-db-connect/raw)])
       (dynamic-wind
        void
        (λ ()
          (define raw-py
            (launch-tokenizer (~? (~@ #:quiet? quiet?))))
          (dynamic-wind
           void
           (λ ()
             (with-db
              #:db raw-db
              (call-with-transaction/db
               (λ ()
                 (tokenizer-cache-db-initialize! #:tokenizer raw-py)
                 (call-with-continuation-barrier
                  (λ ()
                    (let ([py raw-py]
                          (~? [db raw-db]))
                      body ...)))))))
           (λ () (tokenizer-kill raw-py))))
        (λ () (disconnect raw-db))))])

(define (tokenizer-cache-db-connect/raw)
  (define db
    (sqlite3-connect #:database tokenizer-cache-db.sqlite
                     #:use-place #t
                     #:mode 'create))
  (query-exec db "PRAGMA foreign_keys = ON")
  db)

(define/db (tokenizer-cache-db-initialize! #:tokenizer py)
  (cond
    [(not (tokenizer-cache-db-format-ok?))
     (tokenizer-cache-db-drop/rebuild! #:tokenizer py)]
    [(not (tokenizer-cache-db-tokenizer-revision-ok? #:tokenizer py))
     (tokenizer-cache-db-clear! #:tokenizer py)]
    [else
     (void)]))

(define ident-ast:tTokenizerCacheFormatVersion
  (ident-qq tTokenizerCacheFormatVersion))

;; definitions of
;;   - tokenizer-cache-db-clear!
;;   - tokenizer-cache-db-drop/rebuild!
;; (These keep set-revision! private.)
(splicing-local
    [(define/db (set-revision! py)
       (query-exec/db
        (insert #:into tCacheTokenizerRevision
                #:set [cacheTokenizerRevisionFasl
                       ,(s-exp->fasl (tokenizer-revision py))])))]
  (define/db (tokenizer-cache-db-clear! #:tokenizer py)
    (call-with-transaction/db
     (λ ()
       (for ([stmnt (in-list clear-cache-tables-statements)])
         (query-exec/db stmnt))
       (set-revision! py))))
  (define/db (tokenizer-cache-db-drop/rebuild! #:tokenizer py)
    (call-with-transaction/db
     (λ ()
       (query-exec/db (ident-ast->drop-statement ident-ast:tTokenizerCacheFormatVersion))
       (for ([stmnt (in-list drop-cache-tables-statements)])
         (query-exec/db stmnt))
       (query-exec/db
        (create-table (Ident:AST ,ident-ast:tTokenizerCacheFormatVersion)
                      #:columns [cacheFormatVersion int #:not-null]))
       (query-exec/db
        (insert #:into (Ident:AST ,ident-ast:tTokenizerCacheFormatVersion)
                #:set [cacheFormatVersion ,tokenizer-cache-format-version]))
       (for ([stmnt (in-list create-cache-tables-statements)])
         (query-exec/db stmnt))
       (set-revision! py))))
  #|END splicing-local|#)

(define/db (tokenizer-cache-db-format-ok?)
  (and (table-exists?/db (ast->sqlite3 ident-ast:tTokenizerCacheFormatVersion))
       (match (query-list/db
               (select cacheFormatVersion
                       #:from (Ident:AST ,ident-ast:tTokenizerCacheFormatVersion)))
         [(list it)
          (eqv? it tokenizer-cache-format-version)]
         [_
          #f])))

(define/db (tokenizer-cache-db-tokenizer-revision-ok? #:tokenizer py)
  (match (query-list/db
          (select cacheTokenizerRevisionFasl #:from tCacheTokenizerRevision))
    [(list fasl)
     (equal? (tokenizer-revision py)
             (fasl->s-exp fasl))]
    [_
     #f]))

