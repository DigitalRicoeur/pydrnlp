#!/usr/bin/env racket
#lang racket

(require ricoeur/tei
         gregor
         "analyze.rkt")

(module+ main
  (require racket/cmdline)
  (let ([%-pth #f]
        [count-pth #f]
        [tei-dir (build-path (find-system-path 'home-dir)
                             "code"
                             "ricoeur"
                             "texts"
                             "TEI")])
    (command-line
     #:usage-help "generate Trends tool debugging reports"
     #:ps "If no flags are given, this command does nothing."
     #:once-each
     [{"--percent-out"} file "write CSV of percents to <file>"
                        (set! %-pth file)]
     [{"--count-out"} file "write CSV of counts to <file>"
                      (set! count-pth file)]
     [{"--tei-dir"} dir "use TEI documents from <dir> instead of default"
                    (set! tei-dir dir)]
     #:args ()
     (define ir*
       (delay (send (new (trends-debugging-report-mixin
                          directory-corpus%)
                         [path tei-dir])
                    get-trends-data-internal)))
     (when %-pth
       (write-csv-to-file (trends-data-internal->%-table (force ir*))
                          %-pth))
     (when count-pth
       (write-csv-to-file (stringify-count-table
                           (trends-data-internal->count-table (force ir*)))
                          count-pth)))))


(define trends-debugging-report-mixin
  (corpus-mixin [] []
    (define ir
      (delay/thread
       (make-trends-data-internal (sync (super-docs-evt)))))
    (super-new)
    (define/public-final (get-trends-data-internal)
      (force ir))))


(define (trends-data-internal->%-table ir)
  (match-define-values [(app (λ (lst) (sort (map car lst) <)) all-years)
                              listof-text+%+sparse-data]
    (trends-data-internal->data ir))
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
                    string-ci<?)))

(define (stringify-count-table tbl)
  (for/list ([row (in-list tbl)])
    (for/list ([v (in-list row)])
      (if (string? v)
          v
          (number->string v)))))

(define (write-csv-to-file table pth)
  (define str (make-csv table))
  (call-with-output-file* pth
    #:exists 'truncate/replace
    (λ (out) (write-string str out)))
  (void))

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
