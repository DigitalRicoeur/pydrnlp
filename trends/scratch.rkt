#lang racket

(require json racket/cmdline)

(define de-objectify-term
  (match-lambda
    [(hash-table ['text text] ['% %] ['data data])
     (list text % (map (match-lambda
                         [(hash-table ['year yr] ['% %])
                          (list yr %)])
                       data))]))

(module+ main
  (command-line
   #:args (pth)
   (write-json
    (map de-objectify-term
         (call-with-input-file* pth read-json)))))
