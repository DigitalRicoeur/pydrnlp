#lang _-exp racket

(require pydrnlp/doc
         pydrnlp/conda
         "render-python/render.rkt"
         adjutor
         scribble/manual
         scribble/base
         scribble/core
         scribble/decode
         scribble/html-properties
         racket/runtime-path)

(provide rendered-docs)

;; "https://docs.python.org/%d.%d/library"

(define-runtime-path cached-python-docs
  ".cached-python-docs.rktd")
 
(define parsed-docs
  (cond
    [conda-available?
     (define raw (get-pydrnlp-docs))
     (define parsed (parse-pydrnlp-docs raw))
     (when (for/first ([v (in-immutable-hash-values parsed)]
                       #:when (module-doc? v))
             #t)
       (write-to-file raw cached-python-docs
                      #:exists 'replace))
     parsed]
    [(file-exists? cached-python-docs)
     (parse-pydrnlp-docs
      (file->value cached-python-docs))]
    [else
     #hash()]))

(define order-spec
  '[(pydrnlp [language
              stop_words
              jsonio
              (tokenizer [run
                          tokenize
                          usetoken])
              doc])])

(define to-splice
  '([pydrnlp]))


(define rendered-docs
  (render-parsed-docs parsed-docs
                      #:order order-spec
                      #:splice to-splice))
  
