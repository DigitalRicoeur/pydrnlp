#lang racket

(require json
         racket/runtime-path
         net/url
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
  (parameterize ([current-subprocess-custodian-mode 'kill]
                 [current-environment-variables
                  (let ([env (environment-variables-copy
                              (current-environment-variables))])
                    (environment-variables-set! env #"FLASK_APP" #"segtokenize.rest")
                    env)]
                 [current-directory this-dir])
    (match-define (list _ _ pid stderr control)
      (process*/ports #:set-pwd? #t
                      (open-output-nowhere)
                      (open-input-string "")
                      #f
                      (find-executable-path "python3")
                      "-m"
                      "flask"
                      "run"
                      "-p"
                      "5000"))
    (dynamic-wind
     void
     (λ ()
       (define root-url
         (string->url "http://127.0.0.1:5000/"))
       (define (request pth
                        #:headers [headers null]
                        #:method [method #"GET"]
                        #:data [data #f])
         (define-values {status hs body-in}
           (http-sendrecv/url (struct-copy
                               url root-url
                               [path-absolute? #t]
                               [path (list (path/param pth null))])
                              #:headers headers
                              #:method method
                              #:data data))
         (read-json body-in))
       (values (request "revision")
               (request "tokenize"
                        #:method #"PUT"
                        #:headers '(#"Content-Type: application/json")
                        #:data (jsexpr->bytes to-send))))
     (λ ()
       (control 'kill)
       (close-input-port stderr)))))





