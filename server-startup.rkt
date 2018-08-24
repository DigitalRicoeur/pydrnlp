#lang racket/base

(require net/url-structs
         adjutor
         "conda.rkt"
         racket/contract
         racket/match
         racket/system
         racket/port
         )

(provide (contract-out
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
          ))

(define flask-env
  (environment-variables-set*
   (conda-environment-variables)
   #"FLASK_APP" #"pydrnlp.restful"))            

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


(define (test-start-server port)
  (parameterize ([flask-port port]
                 [flask-quiet? #f])
    (start-server-process)))

















