#lang racket

(require scribble/manual
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

(define-syntax-parser include-python-section
  [(_ :string-ish)
   (syntax-local-lift-require
    #`(only (submod (lib #,(dotted->lib-pth ($ datum)))
                    doc)
            doc)
    #'doc)])

(define-syntax-parser pythonmodlink
  [(_ :string-ish)
   #`(racketmodlink (lib #,(dotted->lib-pth ($ datum))) parsed)])

