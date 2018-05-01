#lang racket

(require "adt.rkt"
         racket/runtime-path
         (for-syntax syntax/parse
                     ))

(provide string->full-mod-name
         json-paths
         partition-map
         private?
         partition-by
         app^
         json:annotations
         json:restful
         json:suspiciousToken
         json:drtoken
         json:segtokenize
         json:mkdoc
         json:stdio
         )


(define (string->full-mod-name str)
  (full-mod-name (map string->symbol
                      (regexp-split #rx"\\." str))))

(define-runtime-path-list json-paths
  (map (λ (x) (format "extracted/~a.json" x))
       '(annotations
         restful
         suspiciousToken 
         drtoken
         segtokenize
         mkdoc
         stdio
         )))

(match-define (list json:annotations
                    json:restful
                    json:suspiciousToken 
                    json:drtoken
                    json:segtokenize
                    json:mkdoc
                    json:stdio)
  json-paths)

(define ((private? get-sym) it)
  (regexp-match? #rx"^_" (symbol->string (get-sym it))))
      
      
(define ((partition-map pred? transform) to-go)
  (let loop ([to-go to-go]
             [trues null]
             [falses null])
    (match to-go
      ['()
       (values (reverse trues)
               (reverse falses))]
      [(cons (app transform this) to-go)
       (if (pred? this)
           (loop to-go
                 (cons this trues)
                 falses)
           (loop to-go
                 trues
                 (cons this falses)))])))

(define ((partition-by pred?) lst)
  (partition pred? lst))

(define-match-expander app^
  (syntax-parser
    [(_ pred:expr pat ...)
     #`(app #,(syntax-local-lift-expression #'pred)
            pat
            ...)]))

