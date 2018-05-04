#lang racket/base

(require scribble/reader
         racket/match
         racket/contract
         "../adt/proto-content.rkt"
         "../adt/pass0.rkt"
         )

(provide (contract-out
          [docstring/comment/missing->proto-content
           (-> docstring/comment/missing?
               proto-content?)]
          ))

(define docstring/comment/missing->proto-content
  (match-lambda
    [(docstring/comment/missing
      (docstring (app read-docstring body-list)))
     (text (map parse-docstring/recur
                body-list))]
    [(docstring/comment/missing #f)
     null]
    [(docstring/comment/missing comment-string)
     (verbatim 0 (list comment-string))]))

(define parse-docstring/recur
  (match-lambda
    [(? string? str)
     str]
    [(cons 'text body)
     (text (map parse-docstring/recur body))]
    [(cons 'b body)
     (b (map parse-docstring/recur body))]
    [(cons 'pyflow body)
     (pyflow (map parse-docstring/recur body))]
    [(cons 'literal string-body)
     (literal string-body)]
    [(list-rest 'verbatim '#:indent indent string-body)
     (verbatim indent string-body)]))

     
(define read-docstring-port
  (make-at-reader #:inside? #t
                  #:syntax? #f
                  #:command-char #\ƒ))

(define (read-docstring s)
  (read-docstring-port (open-input-string s)))
