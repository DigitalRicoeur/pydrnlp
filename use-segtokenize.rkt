#lang racket

(require json
         racket/runtime-path
         net/url
         adjutor
         )

(provide try-stdio
         try-rest
         )

(define to-send
  '(#hasheq([key . "my-key"]
            [segments
             .
             (#hasheq([counter . 0]
                      [body . "Apples are good for you."])
              #hasheq([counter . 1]
                      [body . "An apple a day keeps the doctor away. Eat some apples."]))])))

(define-runtime-path this-dir
  ".")

(define (try-stdio)
  (define run
    (let ([python3 (find-executable-path "python3")])
      (λ (arg)
        (string->jsexpr
         (with-output-to-string
           (λ ()
             (system* python3 
                      "-m"
                      "segtokenize.stdio"
                      arg)))))))
  (parameterize ([current-directory this-dir])
                 
     
    (values (run "revision")
            (parameterize ([current-input-port (open-input-string
                                                (jsexpr->string to-send))])
              (run "tokenize")))))


(define (try-rest)
  (match-define (list _ _ pid _ control)
    (parameterize ([current-subprocess-custodian-mode 'kill]
                   [current-environment-variables
                    (let ([env (environment-variables-copy
                                (current-environment-variables))])
                      (environment-variables-set! env #"FLASK_APP" #"segtokenize.rest")
                      env)]
                   [current-directory this-dir])
      (process*/ports #:set-pwd? #t
                      (open-output-nowhere) ;(current-output-port)
                      (open-input-string "")
                      'stdout ;(current-error-port)
                      (find-executable-path "python3")
                      "-m"
                      "flask"
                      "run"
                      "-p"
                      "5001")))
  (dynamic-wind
   void
   (λ ()
     (define root-url
       (string->url "http://127.0.0.1:5001/"))
     ;(flush-output (current-output-port))
     ;(flush-output (current-error-port))
     (define (request pth
                      #:headers [headers null]
                      #:method [method #"GET"]
                      #:data [data #f])
       (let retry ([wait 1])
         (with-handlers ([exn:fail:network?
                          (λ (e)
                            (cond
                              [(infix: wait < 5)
                               (sleep wait)
                               (retry (add1 wait))]
                              [else
                               (raise e)]))])
           (define-values {status hs body-in}
             (http-sendrecv/url (struct-copy
                                 url root-url
                                 [path-absolute? #t]
                                 [path (list (path/param pth null))])
                                #:headers headers
                                #:method method
                                #:data data))
           (read-json body-in))))
     (values (request "revision")
             (request "tokenize"
                      #:method #"POST"
                      #:headers '(#"Content-Type: application/json")
                      #:data (jsexpr->bytes to-send))))
   (λ ()
     (control 'kill))))





