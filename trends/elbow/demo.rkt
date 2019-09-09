#lang racket

(require ricoeur/tei
         racket/runtime-path
         racket/fasl
         racket/flonum
         plot
         "sample-tree.rkt"
         "kde.rkt"
         "otsu.rkt"
         "../types.rkt"
         "../cache-tokenize-corpus.rkt")
;
(module+ main
  (output-english-books)
  (map (λ (dir)
         (match-define (data _ kde otsu _ _)
           (variant-directory->data dir))
         (cons kde otsu))
       (variant-directories))
  ;(plot-variant (car (variant-directories)))
  #;(map plot-variant (variant-directories)))

;; fasl doesn't handle flvectors
(struct data (name kde otsu elements weights) #:prefab)

(define save-plots? #false)

(define-runtime-path out-dir
  "out/")

(define (plot-variant dir)
  (match-define (data name kde otsu elements weights)
    (variant-directory->data dir))
  (define out-file
    (and save-plots? (build-path dir "plot.png")))
  ;; kde red, otsu blue, data greenish
  (parameterize (#;[plot-x-transform log-transform]
                 #;[plot-x-ticks (log-ticks)])
    (plot
     #:title (or name "Combined Books in English")
     #:x-label "# Occurrences"
     #:y-label "# Lemmas w/ X Occurrences"
     #:out-file out-file
     #:height 400
     #:width 800
     (list (vrule kde #:color "red")
           (vrule otsu #:color "blue")
           (points (for/stream ([x (in-flvector elements)]
                                [y (in-flvector weights)])
                     (list x y)))))))
   

(define (variant-directories)
  (call-with-input-file* (build-path out-dir "index.fasl*.rktd")
    (λ (in)
      (for/list ([pth (in-port (λ (in)
                                 (if (eof-object? (peek-byte in))
                                     eof
                                     (fasl->s-exp in)))
                               in)])
        pth))))

(define (variant-directory->data dir)
  (file->value (build-path dir "data-for-plot.rktd")))

(define (output-english-books)
  (match-define (tokenized-corpus docs counts strings)
    (tokenize-english-books (get-docs)))
  (make-directory* out-dir)
  (call-with-output-file* (build-path out-dir "index.fasl*.rktd")
    (λ (out)
      (s-exp->fasl (output-variant #f counts strings) out)
      (for ([d (in-instance-set docs)])
        (s-exp->fasl (output-variant (instance-title d)
                                     (tokenized-document-lemma/count d)
                                     strings)
                     out)))))

(define (output-variant name counts strings)
  (define dir
    (if name
        (build-path out-dir "threshold-demo" "separate-books" name)
        (build-path out-dir "threshold-demo" "combined")))
  (make-directory* dir)
  (let ([candidate-terms
         (for/fold ([lst null]
                    #:result (sort lst > #:key cdr))
                   ([{lemma count}
                     (in-immutable-hash (lemma/count-hsh counts))])
           (cons (cons (regexp-replace* #rx"\"" (lemma/string-ref strings lemma) "\"\"")
                       count)
                 lst))])
    (with-output-to-file (build-path dir "candidate-terms.csv")
      (λ ()
        (printf "\"Lemma\",\"# Occurrences\"\r\n")
        (for-each
         (match-lambda
           [(cons str n)
            (printf "\"~a\",~a\r\n" str n)])
         candidate-terms))))
  (define tree
    (sequence->sample-tree
     (in-immutable-hash-values (lemma/count-hsh counts))))
  (define-values [elements weights]
    (sample-tree-iterate/flvectors/descending tree))
  (define kde
    (sample-tree-threshold/kde tree))
  (define otsu
    (sample-tree-threshold/otsu tree))
  (write-to-file (data name kde otsu elements weights)
                 (build-path dir "data-for-plot.rktd"))
  (with-output-to-file (build-path dir "aggregate-frequencies.csv")
    (λ ()
      (printf "\"X (# Occurrences)\",\"Y (# Lemmas w/ X Occurrences)\"\r\n")
      (for ([x (in-flvector elements)]
            [y (in-flvector weights)])
        (printf "~a,~a\r\n" x y))))
  (with-output-to-file (build-path dir "thresholds.txt")
    (λ () (printf "   KDE = ~a\nOtsu's = ~a\n" kde otsu)))
  dir)
  
(define (tokenize-english-books all-docs)
  (get/build-tokenized-corpus
   #:quiet? #f
   (for/instance-set ([doc (in-instance-set all-docs)]
                      #:when (eq? 'en (instance-language doc))
                      #:when (eq? 'book (instance-book/article doc)))
     doc)))

(define (get-docs)
  (send (new ((corpus-mixin [] []
                (define e (super-docs-evt))
                (super-new)
                (define/public-final (get-docs)
                  (sync e)))
              directory-corpus%)
             [path "/Users/philip/code/ricoeur/texts/TEI/"])
        get-docs))


  
  
