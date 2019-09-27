#lang at-exp racket

(require (rename-in scribble/manual
                    [cite cite-elem])
         syntax/parse/define)

(provide citation) ;; see also define/provide-bibliography at bottom

(begin-for-syntax
  (struct citation-transformer (key)
    #:transparent
    #:property prop:procedure
    (λ (stx)
      (raise-syntax-error #f "only allowed inside a citation form" stx))))

(define-simple-macro (citation cite-transform ...+)
  #:declare cite-transform (static citation-transformer?
                                   "citation transformer")
  #:with (key ...) (map citation-transformer-key
                        (attribute cite-transform.value))
  (cite-elem key ...))

(define-simple-macro (define/provide-bibliography bibliography-section:id
                       [cite-entry ((~alt (~once (~seq #:key key:str))
                                          (~once (~seq #:title title:expr))
                                          (~optional (~seq #:is-book? is-book?:expr))
                                          (~optional (~seq #:author author:expr))
                                          (~optional (~seq #:location location:expr))
                                          (~optional (~seq #:date date:expr))
                                          (~optional (~seq #:url url:expr))
                                          (~optional (~seq #:note note:expr)))
                                    ...)]
                       ...)
  #:fail-when (check-duplicate-identifier
               (syntax->list #'(bibliography-section cite-entry ...)))
  "duplicate identifier"
  (begin
    (provide bibliography-section cite-entry ...)
    (define-syntax cite-entry
      (citation-transformer (quote-syntax key)))
    ...
    (define bibliography-section
      (bibliography
       (bib-entry #:key key
                  #:title title
                  (~? (~@ #:is-book? is-book?))
                  (~? (~@ #:author author))
                  (~? (~@ #:location location))
                  (~? (~@ #:date date))
                  (~? (~@ #:url url))
                  (~? (~@ #:note note)))
       ...))))
           
;                   
;   ;;     ;; ;;    
;   ;;        ;;    
;   ;;;;   ;; ;;;;  
;   ;;  ;  ;; ;;  ; 
;   ;;  ;  ;; ;;  ; 
;   ;;  ;; ;; ;;  ;;
;   ;;  ;  ;; ;;  ; 
;   ;;  ;  ;; ;;  ; 
;   ; ;;   ;; ; ;;  
;                   


(define/provide-bibliography bibliography-section
  [|Kill Safe| (#:key "Flatt04"
                #:author "Matthew Flatt and Robert Bruce Findler"
                #:title "Kill-Safe Synchronization Abstractions"
                #:location "Programming Language Design and Implementation" 
                #:date "2004"
                #:url "http://www.cs.utah.edu/plt/publications/pldi04-ff.pdf")])

