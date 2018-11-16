#lang racket

(require json
         racket/contract
         racket/match
         racket/async-channel
         racket/port
         racket/system
         syntax/parse/define
         markdown/parse
         xml/xexpr
         "conda.rkt")

(provide text/c
         (contract-out
          [get-pydrnlp-docs
           (-> python-docs/c)]
          [parse-pydrnlp-docs
           (-> python-docs/c
               (hash/c #:immutable #t
                       (listof symbol?)
                       (or/c 'not-found
                             'error-during-import
                             module-doc?)))]
          [struct docstring ([xexpr* (listof xexpr/c)])]
          [struct comments ([str string?])]
          [struct module-doc ([modpath (listof symbol?)]
                              [text text/c]
                              [classes (listof class-doc?)]
                              [functions (listof function-doc?)])]
          [struct class-doc ([name symbol?]
                             [bases (listof base-class-ref?)]
                             [text text/c]
                             [methods (listof function-doc?)])]
          [struct base-class-ref ([module (listof symbol?)]
                                  [name symbol?])]
          [struct function-doc
            ([name symbol?]
             [text text/c]
             [parameters (listof parameter-doc?)]
             [return (or/c #f string?)])]
          [struct parameter-doc
            ([formal-name string?]
             [kind (or/c 'POSITIONAL_ONLY 'POSITIONAL_OR_KEYWORD
                         'VAR_POSITIONAL 'KEYWORD_ONLY
                         'VAR_KEYWORD)]
             [annotation (or/c #f string?)]
             [default (or/c #f string?)])]
          ))

(define (get-pydrnlp-docs)
  (for/hash ([js (in-list (document-modules
                           #:who 'get-pydrnlp-docs
                           (pydrnlp-modules)))])
    (match-define (list "modpath"
                        (hash-table ['modpath (app split-python-modpath mp)]
                                    ['module m]
                                    [_ _] ...))
      js)
    (values mp
            (match m
              [#f 'not-found]
              ["ErrorDuringImport" 'error-during-import]
              [_ m]))))

(define (parse-pydrnlp-docs hsh)
  (for/hash ([{mp js} (in-immutable-hash hsh)])
    (values mp (if (symbol? js) js (parse-module mp js)))))

(define doc-env
  (conda-environment-variables))

(define (pydrnlp-modules)
  (let* ([pydrnlp (build-path py-dir "pydrnlp")]
         [scratch (build-path pydrnlp "scratch")])
    (for/list ([p (in-directory
                   (build-path py-dir)
                   (λ (dir-pth)
                     (and (not (equal? dir-pth scratch))
                          (file-exists? (build-path dir-pth "__init__.py")))))]
               #:when (equal? #".py" (path-get-extension p)))
      (define parts
        (map path-element->string
             (explode-path
              (path-replace-extension (find-relative-path py-dir p) #""))))
      (string-join (if (equal? "__init__" (last parts))
                       (drop-right parts 1)
                       parts)
                   "."))))

(define (document-modules modules #:who [who 'document-modules])
  (define-values {in-from-pipe out-to-pipe}
    (make-pipe))
  (define err
    (open-output-string))
  (cond
    [(parameterize ([current-subprocess-custodian-mode 'kill]
                    [current-environment-variables doc-env]
                    [current-directory py-dir]
                    [current-input-port (open-input-bytes #"")]
                    [current-error-port err]
                    [current-output-port out-to-pipe])
       (system* #:set-pwd? #t
                python3
                #"-m"
                #"pydrnlp.doc"
                (jsexpr->bytes modules)))
     (close-output-port out-to-pipe)
     (read-json in-from-pipe)]
    [else
     (write-string (get-output-string err) (current-error-port))
     (error who "python3 exited abnormally")]))

                                        
;                                          
;                                          
;                                          
;   ; ;;      ;;    ;; ;;;    ;;      ;;;  
;   ;;  ;    ;  ;   ;;;     ;;  ;   ;;   ; 
;   ;;  ;       ;;  ;;       ;      ;    ; 
;   ;;  ;;    ;;;;  ;;        ;;   ;;;;;;;;
;   ;;  ;    ;  ;;  ;;          ;;  ;      
;   ;;  ;   ;;  ;;  ;;      ;   ;   ;;   ; 
;   ;;;;     ;;; ;  ;;       ;;;      ;;;  
;   ;;                                     
;   ;;                                     
;   ;;                                     
;                                          


(define (split-python-modpath mp)
  (map string->symbol (regexp-split #rx"\\." mp)))


(define-syntax-parser json-object/c
  [(_ [key:id val/c:expr] ...)
   #`(let ([all-keys #,(for/hasheq ([k (in-list (syntax->datum #'(key ...)))])
                         (values k #t))]
           [key val/c] ...)
       (and/c jsexpr?
              (hash/dc
               [k (or/c 'key ...)]
               [v (k) (case k
                        [(key) key] ...)]
               #:immutable #t
               #:kind 'flat)
              (λ (hsh)
                (hash-keys-subset? all-keys hsh))))])

(define/final-prop doc-text/c
  (or/c #f
        (list/c "docstring" string?)
        (list/c "comments" string?)))

(struct docstring (xexpr*)
  #:transparent)
(struct comments (str)
  #:transparent)

(define/final-prop text/c
  (or/c #f docstring? comments?))

(define parse-text
  (match-lambda
    [#f #f]
    [(list "comments" it)
     (comments it)]
    [(list "docstring" it)
     (docstring (parse-markdown it))]))

(define/final-prop parameter/c
  (list/c "parameter"
          (json-object/c
           [name string?]
           [kind (or/c "POSITIONAL_ONLY" "POSITIONAL_OR_KEYWORD"
                       "VAR_POSITIONAL" "KEYWORD_ONLY"
                       "VAR_KEYWORD")]
           [annotation (or/c #f string?)]
           [default (or/c #f string?)])))

(struct parameter-doc (formal-name kind annotation default)
  #:transparent)

(define parse-parameter
  (match-lambda
    [(list "parameter" (hash-table
                        ['name n]
                        ['kind (app string->symbol kind)]
                        ['annotation ann]
                        ['default default]
                        [_ _] ...))
     (parameter-doc n kind ann default)]))
     

(define/final-prop signature/c
  (list/c "signature"
          (json-object/c
           [parameters (listof parameter/c)]
           [return (or/c #f string?)])))

(define/final-prop function/c
  (list/c "function"
          (json-object/c
           [name string?]
           [text doc-text/c]
           [signature signature/c])))

(struct function-doc (name text parameters return)
  #:transparent)

(define parse-function
  (match-lambda
    [(list "function"
           (hash-table
            ['name (app string->symbol name)]
            ['text (app parse-text t)]
            ['signature
             (list "signature" (hash-table
                                ['parameters
                                 (list (app parse-parameter param...) ...)]
                                ['return return?]
                                [_ _] ...))]
            [_ _] ...))
     (function-doc name t param... return?)]))

(define/final-prop base-class-ref/c
  (list/c "base"
          (json-object/c
           [name string?]
           [module string?])))

(struct base-class-ref (module name)
  #:transparent)

(define parse-base-class-ref
  (match-lambda
    [(list "base"
           (hash-table ['name (app string->symbol name)]
                       ['module (app split-python-modpath m)]
                       [_ _] ...))
     (base-class-ref m name)]))

(define/final-prop class/c
  (list/c "class"
          (json-object/c
           [name string?]
           [bases (listof base-class-ref/c)]
           [text doc-text/c]
           [methods (listof function/c)])))

(struct class-doc (name bases text methods)
  #:transparent)

(define parse-class
  (match-lambda
    [(list "class" (hash-table
                    ['name (app string->symbol name)]
                    ['bases (list (app parse-base-class-ref base...) ...)]
                    ['text (app parse-text t)]
                    ['methods (list (app parse-function fun...) ...)]
                    [_ _] ...))
     (define init
       (findf (λ (f) (eq? '__init__ (function-doc-name f)))
              fun...))
     (class-doc name base... t (if init
                                   (cons init (remove init fun...))
                                   fun...))]))

(define/final-prop module/c
  (list/c "module"
          (json-object/c
           [text doc-text/c]
           [classes (listof class/c)]
           [functions (listof function/c)])))

(struct module-doc (modpath text classes functions)
  #:transparent)

(define (parse-module name js)
  (match js
    [(list "module" (hash-table
                     ['text (app parse-text t)]
                     ['classes (list (app parse-class class...) ...)]
                     ['functions (list (app parse-function fun...) ...)]
                     [_ _] ...))
     (module-doc name t class... fun...)]))

(define/final-prop modpath/c
  (list/c "modpath"
          (json-object/c
           [modpath string?]
           [module (or/c #f "ErrorDuringImport" module/c)])))

(define/final-prop python-docs/c
  (hash/c #:immutable #t
          (listof symbol?)
          (or/c 'not-found
                'error-during-import
                module/c)))









