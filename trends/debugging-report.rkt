#lang racket

(require ricoeur/tei
         gregor
         "analyze.rkt")

(module+ main
  (void
   (write-string
    (make-csv
     (send (new (trends-debugging-report-mixin
                 directory-corpus%)
                [path (build-path (find-system-path 'home-dir)
                                  "code"
                                  "ricoeur"
                                  "texts"
                                  "TEI")])
           get-data)))))



(define trends-debugging-report-mixin
  (corpus-mixin [] []
    (super-new)
    (define data
      (let ()
        (match-define-values [(app (λ (lst) (sort (map car lst) <)) all-years)
                              listof-text+%+sparse-data]
          (make-trends-data (super-docs)))
        (cons (list* "Term" "Overall" (map number->string all-years))
              (sort (map (match-lambda
                           [(list text (app number->string overall%) sparse-data)
                            (list* text
                                   overall%
                                   (for/list ([year (in-list all-years)])
                                     (cond
                                       [(assv year sparse-data)
                                        => (λ (v) (number->string (second v)))]
                                       [else
                                        ""])))])
                         listof-text+%+sparse-data)
                    #:key car
                    string<?))))
    (define/public-final (get-data)
      data)))


(define (make-csv table)
  (string-join
   (for/list ([row (in-list table)])
     (string-join
      (for/list ([field (in-list row)])
        (string-append "\""
                       (regexp-replace* #rx"\"" field "\"\"")
                       "\""))
      ","
      #:after-last "\r\n"))
   ""))
