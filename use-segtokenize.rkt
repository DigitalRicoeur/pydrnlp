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

(define python3
  (let* ([env (environment-variables-copy
               (current-environment-variables))]
         [PATH (bytes-append (environment-variables-ref env #"PATH")
                             #":/usr/local/bin")])
    (environment-variables-set! env #"PATH" PATH)
    (parameterize ([current-environment-variables env])
      (find-executable-path "python3"))))



(define (try-stdio)
  (parameterize ([current-directory this-dir])
    (define (run arg [str ""])
      (string->jsexpr
       (parameterize ([current-input-port (open-input-string str)])
         (with-output-to-string
           (λ ()
             (system* python3 
                      "-m"
                      "pydrnlp.stdio"
                      arg))))))
    (values (run "revision")
            (run "tokenize" (jsexpr->string to-send)))))



(define flask-port
    "5001")

(define flask-quiet?
  #t)

(define (try-rest)
  ;(define control void)
  (match-define (list _ _ pid _ control)
    (parameterize ([current-subprocess-custodian-mode 'kill]
                   [current-environment-variables
                    (let ([env (environment-variables-copy
                                (current-environment-variables))])
                      (environment-variables-set! env #"FLASK_APP" #"pydrnlp.rest")
                      ;; see http://click.pocoo.org/5/python3/#python-3-surrogate-handling
                      ;; Mac OS doesn't set locale for GUI programs, it seems.
                      (unless (environment-variables-ref env #"LANG")
                        (environment-variables-set! env #"LANG" #"en_US.UTF-8"))
                      env)]
                   [current-directory this-dir])
      (define-values {stdout-arg stderr-arg}
        (if flask-quiet?
            (values (open-output-nowhere) 'stdout)
            (values (current-output-port) (current-error-port))))
      (process*/ports #:set-pwd? #t
                      stdout-arg
                      (open-input-string "")
                      stderr-arg
                      python3
                      "-m"
                      "flask"
                      "run"
                      "-p"
                      flask-port)))
  (dynamic-wind
   void
   (λ ()
     (define root-url
       (string->url (string-append "http://127.0.0.1:" flask-port "/")))
     (unless flask-quiet?
       (flush-output (current-output-port))
       (flush-output (current-error-port)))
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





