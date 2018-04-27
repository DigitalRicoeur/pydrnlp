#lang racket

(require racket/runtime-path
         net/url
         adjutor
         (for-syntax syntax/parse
                     ))

(provide ip-port-num/c 
         (contract-out
          [start-server-process
           (-> (-> (or/c 'status
                         'exit-code
                         'wait
                         'interrupt
                         'kill)
                   any))]
          [flask-quiet?
           (parameter/c any/c)]
          [flask-root-url
           (-> url?)]
          [flask-port 
           (parameter/c ip-port-num/c)]
          [environment-variables-set*
           ;; maybe to adjutor?
           (->* {environment-variables?}
                #:rest environment-variables-args/c
                environment-variables?)]
          ))
            

(define flask-quiet?
  (make-parameter #t any->boolean))

(define (make-root-url port)
  (url "http" #f "localhost" port #t (list (path/param "" '())) '() #f))

(define flask-root-url
  (make-parameter (make-root-url 5005)))

(define flask-port
  (make-derived-parameter flask-root-url
                          make-root-url
                          url-port))

(define ip-port-num/c
  ;; maybe to adjutor?
  (integer-in 0 65535))

(define init-env
  (current-environment-variables))

(define-runtime-path py-dir
  "py")

(define condaenv-dir
  (build-path py-dir "condaenv"))

(define condaenv-bin
  (build-path condaenv-dir "bin"))

(define environment-variables-args/c
  (flat-rec-contract environment-variables-args/c
    (list)
    (cons/c bytes-environment-variable-name?
            (cons/c (or/c bytes-no-nuls? #f)
                    environment-variables-args/c))))

(define/contract (environment-variables-set* env . args)
  (->* {environment-variables?}
       #:rest environment-variables-args/c
       environment-variables?)
  (let ([env (environment-variables-copy env)])
    (let loop ([args args])
      (match args
        [(list-rest k v args)
         (environment-variables-set! env k v)
         (loop args)]
        ['()
         env]))))

(define conda-env
  (environment-variables-set*
   init-env
   #"CONDA_DEFAULT_ENV" (path->bytes condaenv-dir)
   #"CONDA_PREFIX" (path->bytes condaenv-dir)
   #"CONDA_PYTHON_EXE" #"/usr/local/miniconda3/bin/python" ;;;
   #"CONDA_EXE" #"/usr/local/miniconda3/bin/conda" ;;;
   #"CONDA_SHLVL" #"1"
   #"PATH" (bytes-append (path->bytes condaenv-bin)
                         #":"
                         (or (environment-variables-ref init-env #"PATH")
                             #""))
   ;; see http://click.pocoo.org/5/python3/#python-3-surrogate-handling
   ;; Apparently Mac OS doesn't set locale for GUI programs.
   #"LANG" (or (environment-variables-ref init-env #"LANG")
               #"en_US.UTF-8")))

(define python3
  (parameterize ([current-environment-variables conda-env])
    (find-executable-path "python3")))

(define flask-env
  (environment-variables-set*
   conda-env
   #"FLASK_APP" #"pydrnlp.restful"))

(define (start-server-process)
  (match-define (list _ _ pid _ control)
    (parameterize ([current-subprocess-custodian-mode 'kill]
                   [current-environment-variables flask-env]
                   [current-directory py-dir])
      (define-values {stdout-arg stderr-arg}
        (if (flask-quiet?)
            (values (open-output-nowhere) 'stdout)
            (values (current-output-port) (current-error-port))))
      (process*/ports #:set-pwd? #t
                      stdout-arg
                      (open-input-string "")
                      stderr-arg
                      python3
                      #"-m"
                      #"flask"
                      #"run"
                      #"-p"
                      (number->string (flask-port)))))
  control)

#|
(define-syntax (environment-variables-set!* stx)
  (define-splicing-syntax-class spec
    #:description "key/value pair"
    #:attributes {key value}
    (pattern (~seq (~describe "key expression" k)
                   (~describe "value expression" v))
             #:declare k (expr/c #'bytes-environment-variable-name?
                                 #:name "key expression")
             #:declare v (expr/c #'(or/c bytes-no-nuls? #f)
                                 #:name "value expression")
             #:with key #'k.c
             #:with value #'v.c))
  (define-syntax-class envvars
    #:description "environment variables expression"
    #:attributes {c}
    (pattern env
             #:declare env (expr/c #'environment-variables?
                                   #:name "environment variables expression")
             #:with c #'env.c))
  (syntax-parse stx
    [(_ env:envvars) #'(void env.c)]
    [(_ env-expr:envvars
        pair:spec ...+)
     #`(let ([env env-expr.c])
         (environment-variables-set! env pair.key pair.value)
         ...)]))
|#


(define (test-start-server port)
  (parameterize ([flask-port port]
                 [flask-quiet? #f])
    (start-server-process)))

















