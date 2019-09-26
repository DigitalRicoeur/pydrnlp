#lang racket

(require scribble/manual
         scribble/decode
         scribble/core
         scribble/racket
         syntax/parse/define
         (for-syntax (only-in syntax/parse [attribute $])))

(provide include-python-section
         pythonmodlink)

(begin-for-syntax
  (define-syntax-class string-ish
    #:description #f
    #:attributes {parsed datum}
    (pattern parsed:str
             #:attr datum (syntax->datum #'parsed))
    (pattern sym:id
             #:attr datum (symbol->string (syntax->datum #'sym))
             #:with parsed (datum->syntax #'sym ($ datum) #'sym #'sym))))

(define-for-syntax dotted->lib-pth
  (case-lambda
    [(ctxt str)
     (datum->syntax ctxt (dotted->lib-pth str) ctxt ctxt)]
    [(str)
     (string-append "pydrnlp/py/"
                    (regexp-replace* #rx"\\." str "/")
                    ".py")]))

(define-syntax-parser pythonmodlink
  [(_ :string-ish)
   #`(racketmodlink (lib #,(dotted->lib-pth ($ datum)))
                    (->pythonmodlink-elem parsed))])
(define (->pythonmodlink-elem str)
  (element module-link-color
           (racketmodfont str)))


(define-syntax-parser include-python-section
  [(_ lib:string-ish #:title pre-new-title ...)
   #:declare pre-new-title (expr/c #'pre-content?
                                   #:name "title expression")
   #:with doc (syntax/loc this-syntax
                (include-python-section lib))
   #'(re-title doc pre-new-title.c ...)]
  [(_ :string-ish)
   (syntax-local-lift-require
    #`(only (submod (lib #,(dotted->lib-pth ($ datum)))
                    doc)
            doc)
    #'doc)])

(define (re-title doc . pre-content)
  (struct-copy part doc
               [title-content (decode-content pre-content)]))
