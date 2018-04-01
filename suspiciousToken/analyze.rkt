#lang racket

(require racket/runtime-path
         (only-in srfi/13 string-pad)
         json
         )

(define-runtime-path here
  ".")

(define total
  (for/sum ([pth (in-directory here)]
            #:when (equal? #".json"
                           (path-get-extension pth))
            [obj (in-list (call-with-input-file pth
                            read-json))])
    1))

(define data
  (file->value (build-path here "all.rktd")))

(define lower-stop-but-not-lemma
  (for/list ([obj (in-list data)]
             #:when (hash-ref obj 'lowerIsStop)
             #:unless (hash-ref obj 'lemmaIsStop))
    obj))

(define lemma-stop-but-not-lower
  (for/list ([obj (in-list data)]
             #:unless (hash-ref obj 'lowerIsStop)
             #:when (hash-ref obj 'lemmaIsStop))
    obj))

(define assocs:lemma->stopword
  (file->value (build-path here "stopword-lemmas.rktd")))

(define hsh:lemma->stopwords
  (for/hash ([grp (in-list (group-by car assocs:lemma->stopword))])
    (values (caar grp)
            (map cdr grp))))

(define (filter-with-stopword-lemmas l-objs)
  (for/list ([obj (in-list l-objs)]
             #:unless (hash-has-key? hsh:lemma->stopwords
                                   (hash-ref obj 'lemma)))
    obj))

(module+ main
  (define num:unique-total
    (length data))
  (define (% n)
    (real->decimal-string
     (* 100 (/ n num:unique-total))))
  (define num:lemma-stop-but-not-lower
    (length lemma-stop-but-not-lower))
  (define num:lower-stop-but-not-lemma
    (length lower-stop-but-not-lemma))
  (define (make-padder a b)
    (let ([w (max (string-length a)
                  (string-length b))])
      (λ (str)
        (string-pad str w))))
  (printf "Total Suspicious Tokens:\t~a\n" total)
  (printf "Unique Suspicious Tkns.:\t~a\n" (length data))
  (printf "Number where only ...\n")
  (let* ([%:lemma-stop-but-not-lower
          (% num:lemma-stop-but-not-lower)]
         [%:lower-stop-but-not-lemma
          (% num:lower-stop-but-not-lemma)]
         [pad (make-padder %:lemma-stop-but-not-lower
                           %:lower-stop-but-not-lemma)]
         [%:lemma-stop-but-not-lower
          (pad %:lemma-stop-but-not-lower)]
         [%:lower-stop-but-not-lemma
          (pad %:lower-stop-but-not-lemma)])
         
    (printf "      Lemma is stopword:\t~a\t~a%\n"
            num:lemma-stop-but-not-lower
            %:lemma-stop-but-not-lower)
    (printf "      Lowercase is stop:\t~a\t~a%\n\n"
            num:lower-stop-but-not-lemma
            %:lower-stop-but-not-lemma)))
#|
Total Suspicious Tokens:	14653
Unique Suspicious Tkns.:	370
Number where only ...
      Lemma is stopword:	130	35.14%
      Lowercase is stop:	3	 0.81%
|#
  