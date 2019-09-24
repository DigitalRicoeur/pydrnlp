#lang _-exp racket

(require pydrnlp/doc
         pydrnlp/support/conda
         "markdown.rkt"
         "sort.rkt"
         adjutor
         scribble/manual
         scribble/base
         scribble/core
         scribble/decode
         scribble/html-properties
         racket/runtime-path)

(provide (contract-out
          [render-parsed-docs
           (->* [(hash/c #:immutable #t
                         (listof symbol?)
                         (or/c 'not-found
                               'error-during-import
                               module-doc?))]
                [#:order order-spec/c
                 #:splice (listof (listof symbol?))]
                (listof pre-part?))]
          ))

(struct content+width (content w)
  #:transparent)

;; racket-doc/scribblings/inside/utils.rkt

(match-define (list |(| |)| |.| |,| : =-elem return->)
  (map (compose1 racketparenfont
                 symbol->string)
       '(|(| |)| |.| |,| : = ->)))

(define space
  (tt (literal " ")))

(define (render-parsed-docs parsed
                            #:order [order-spec null]
                            #:splice [to-splice null])
  (render-package-doc-nodes
   (organize-package-docs parsed order-spec)
   to-splice))

;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                                                                                          
;                           ;;                                                          ;;                 
;                           ;;                                                          ;;                 
;   ; ;;      ;;       ;;;  ;;   ;;   ;;      ;;;;;   ;;;           ;; ;     ;;;     ;;;;;    ;;;     ;;   
;   ;;  ;    ;  ;    ;;   ; ;;  ;    ;  ;    ;  ;   ;;   ;          ;;; ;   ;   ;   ;   ;;  ;;   ;  ;;  ;  
;   ;;  ;       ;;   ;      ;; ;        ;;  ;;  ;;  ;    ;          ;;  ;;  ;   ;   ;   ;;  ;    ;   ;     
;   ;;  ;;    ;;;;  ;;      ;;;;      ;;;;   ;  ;  ;;;;;;;;         ;;  ;; ;;   ;; ;;   ;; ;;;;;;;;   ;;   
;   ;;  ;    ;  ;;   ;      ;;  ;    ;  ;;    ;;    ;               ;;  ;;  ;   ;   ;   ;;  ;           ;; 
;   ;;  ;   ;;  ;;   ;;   ; ;;   ;  ;;  ;;  ;;      ;;   ;          ;;  ;;  ;   ;   ;   ;;  ;;   ;  ;   ;  
;   ;;;;     ;;; ;     ;;;  ;;   ;;  ;;; ;   ;;;;;    ;;;           ;;  ;;   ;;;     ;;; ;    ;;;    ;;;   
;   ;;                                      ;    ;;                                                        
;   ;;                                     ;;    ;                                                         
;   ;;                                       ;;;;                                                          
;                                                                                                          


(define (decode-part* stuff)
  (decode-part stuff null #f 0))

(define (render-package-doc-nodes lst modpaths-to-splice)
  (let loop ([lst lst])
    (for/list ([node (in-list lst)])
      (match-define
        (package-doc-node modpath name v children)
        node)
      (define modpath-str (format-dotted-name modpath))
      (define body+children
        (append* (match v
                   ['not-found
                    (list ƒbold{Module not found})]
                   ['error-during-import
                    (list ƒbold{Python encountered an error while
                     importing this module.})]
                   [_
                    (render-module-doc v)])
                 (loop children)))
      (if (member modpath modpaths-to-splice)
          body+children
          (list (decode-part*
                 (cons (title (tt modpath-str))
                       body+children)))))))
    
(define render-module-doc
  (match-lambda
    [(module-doc modpth text classes functions)
     (define modpth-str (format-dotted-name modpth))
     (define prefix
       (content+width (italic (string-append modpth-str "."))
                      (add1 (string-length modpth-str))))
     (define headings?
       (and (pair? classes) (pair? functions)))
     (append (render-text text)
             (list-when headings?
               (list (section "Classes")))
             (map (class-renderer prefix)
                  classes)
             (list-when headings?
               (list (section "Functions")))
             (map (functon-renderer prefix)
                  functions))]))


;                                          
;                                          
;                                          
;                                          
;           ;;;;                           
;             ;;                           
;      ;;;    ;;      ;;      ;;      ;;   
;    ;;   ;   ;;     ;  ;   ;;  ;   ;;  ;  
;    ;        ;;        ;;   ;       ;     
;   ;;        ;;      ;;;;    ;;      ;;   
;    ;        ;;     ;  ;;      ;;      ;; 
;    ;;   ;    ;    ;;  ;;  ;   ;   ;   ;  
;      ;;;      ;;   ;;; ;   ;;;     ;;;   
;                                          
;                                          
;                                          
;                                          


(define (class-renderer [prefix (content+width "" 0)])
  (match-lambda
    [(class-doc name bases text methods)
     (apply nested
            (make-blue-box
             "Python class"
             (list (content+width-content prefix)
                   (symbol->id-content name)
                   |(|
                   (add-between (add-commas
                                 (map (match-lambda
                                        [(base-class-ref _ name)
                                         (symbol->id-content name)])
                                      bases))
                                space)
                   |)|))
            (append (render-text text)
                    (map (functon-renderer #:method? #t)
                         methods)))]))

;                                                                  
;                                                                  
;                                                                  
;                                                                  
;       ;;;                          ;;        ;                   
;     ;;                             ;;        ;;                  
;   ;;;;;;; ;;  ;;  ;; ;       ;;; ;;;;;;;  ;;;;;    ;;;    ;; ;   
;     ;;    ;;  ;;  ;;; ;    ;;   ;  ;;        ;;   ;   ;   ;;; ;  
;     ;;    ;;  ;;  ;;  ;;   ;       ;;        ;;   ;   ;   ;;  ;; 
;     ;;    ;;  ;;  ;;  ;;  ;;       ;;        ;;  ;;   ;;  ;;  ;; 
;     ;;    ;;  ;;  ;;  ;;   ;       ;;        ;;   ;   ;   ;;  ;; 
;     ;;     ; ;;;  ;;  ;;   ;;   ;   ;        ;;   ;   ;   ;;  ;; 
;     ;;      ; ;;  ;;  ;;     ;;;     ;;;     ;;    ;;;    ;;  ;; 
;                                                                  
;                                                                  
;                                                                  
;                                                                  


(define (functon-renderer [prefix (content+width "" 0)]
                          #:method? [method? #f])
  (match-lambda
    [(function-doc name text parameters return)
     (apply nested
            #:style (and method? 'inset)
            (make-blue-box
             (if method? "Python method" "Python function")
             (append (list (content+width-content prefix)
                           (symbol->id-content name)
                           |(|
                           (add-between (map content+width-content
                                             (render-parameter-list parameters))
                                        space)
                           |)|)
                     (if return
                         (list space return-> space (plain-code return))
                         null)))
            (render-text text))]))

(define (render-parameter-list lst)
  ;; based on https://github.com/python/cpython/blob/
  ;;   25221b328339fb1726b58742e91b6e49c178023a/Lib/inspect.py#L3033
  (define (pos-only-sep)
    (content+width (racketparenfont "/") 1))
  (add-commas
   (for/fold ([rslt null]
              [render_pos_only_separator #f]
              [render_kw_only_separator #t]
              #:result
              (reverse (if render_pos_only_separator
                           (cons (pos-only-sep) rslt)
                           rslt)))
             ([param (in-list lst)])
     (let*-values
         ([{rslt} (cons (render-parameter param) rslt)]
          [{kind} (parameter-doc-kind param)]
          [{rslt render_pos_only_separator}
           (cond
             [(eq? 'POSITIONAL_ONLY kind)
              (values rslt #t)]
             [render_pos_only_separator
              (values (cons (pos-only-sep) rslt) #f)]
             [else
              (values rslt render_pos_only_separator)])]
          [{rslt render_kw_only_separator}
           (cond
             [(eq? 'VAR_POSITIONAL kind)
              (values rslt #f)]
             [(and (eq? 'KEYWORD_ONLY kind) render_kw_only_separator)
              (values (cons (content+width (racketvarfont "*") 1) rslt)
                      #f)]
             [else
              (values rslt render_kw_only_separator)])])
       (values rslt
               render_pos_only_separator
               render_kw_only_separator)))))

(define render-parameter
  (match-lambda
    [(parameter-doc name kind ann default)
     ;; based on https://github.com/python/cpython/blob/
     ;;   25221b328339fb1726b58742e91b6e49c178023a/Lib/inspect.py#L2549
     (let*-values
         ([{w} (string-length name)]
          [{name w} (case kind
                      [(VAR_POSITIONAL)
                       (values (string-append "*" name)
                               (+ 1 w))]
                      [(VAR_KEYWORD)
                       (values (string-append "**" name)
                               (+ 2 w))]
                      [else
                       (values name w)])]
          [{name} (racketvarfont (literal name))]
          [{ann w} (if ann
                       (values (plain-code ann)
                               (+ w (string-length ann)))
                       (values #f w))]
          [{default w} (if default
                           (values (plain-code default)
                                   (+ w (string-length default)))
                           (values #f w))]
          [{annotated w} (if ann
                             (values (list name : space ann)
                                     (+ 2 w))
                             (values (list name) w))])
       (cond
         [default
          (define (mk mid n)
            (content+width (append annotated mid (list default)) (+ w n)))
          (if ann
              (mk (list space =-elem space) 3)
              (mk (list =-elem) 1))]
         [else
          (content+width annotated w)]))]))


;                                  
;                                  
;                                  
;                                  
;    ;;                      ;;    
;    ;;                      ;;    
;  ;;;;;;;    ;;;  ;;   ;; ;;;;;;; 
;    ;;     ;;   ;   ;  ;    ;;    
;    ;;     ;    ;   ; ;     ;;    
;    ;;    ;;;;;;;;   ;      ;;    
;    ;;     ;        ; ;     ;;    
;     ;     ;;   ;  ;   ;     ;    
;      ;;;    ;;;  ;;   ;;     ;;; 
;                                  
;                                  
;                                  
;                                  

(define render-text
  (match-lambda
    [(docstring xs)
     (render-markdown-xexprs xs)]
    [(comments s)
     (list (verbatim s))]
    [_ null]))

(define (render-markdown-xexprs xs)
  (xexprs->scribble-pres
   xs
   (match-lambda
     [`(,(and sec (or 'h1 'h2 'h3)) ,_ . ,body)
      (para (bold body))]
     [(or `(code () ,s)
          `(code ([class "brush: python"]) ,s))
      (TODO/void inline python)
      #f]
     [`(pre ([class "brush: python"]) (code () ,s))
      (TODO/void block python)
      #f]
     [_ #f])))

;                                                          
;                                                          
;                                                          
;                                                          
;   ;;              ;;;;                                   
;   ;;                ;;                                   
;   ;; ;      ;;;     ;;    ; ;;      ;;;   ;; ;;;    ;;   
;   ;;; ;   ;;   ;    ;;    ;;  ;   ;;   ;  ;;;     ;;  ;  
;   ;;  ;;  ;    ;    ;;    ;;  ;   ;    ;  ;;       ;     
;   ;;  ;; ;;;;;;;;   ;;    ;;  ;; ;;;;;;;; ;;        ;;   
;   ;;  ;;  ;         ;;    ;;  ;   ;       ;;          ;; 
;   ;;  ;;  ;;   ;     ;    ;;  ;   ;;   ;  ;;      ;   ;  
;   ;;  ;;    ;;;       ;;  ;;;;      ;;;   ;;       ;;;   
;                           ;;                             
;                           ;;                             
;                           ;;                             
;                                                          


(define (add-commas lst)
  (match lst
    ['() null]
    [(list a) lst]
    [(list (app (match-lambda
                  [(content+width c w)
                   (content+width (append c (list |,|)) (add1 w))])
                a...)
           ...
           b)
     (append a... (list b))]))

(define (plain-code s)
  (racketplainfont (literal s)))

(define (symbol->id-content s)
  (racketidfont
   (literal (symbol->string s))))

(define (format-dotted-name parts [post #f])
  (define sep ".\u200B")
  (string->immutable-string
   (let ([s (string-join (map symbol->string parts) sep)])
     (if post
         (string-append s sep (symbol->string post))
         s))))

(define*/contract make-blue-box
  (-> string? content? any)
  (let ()
    (def
      [RBackgroundLabel
       (style "RBackgroundLabel"
              (list 'decorative 'command (alt-tag "div")
                    (attributes '((class . "SIEHidden")))))]
      [RBackgroundLabelInner
       (style "RBackgroundLabelInner" (list (alt-tag "div")))]
      [RForeground
       (style #f (list (attributes '((class . "RForeground")))))]
      [omitable
       (style #f '('omitable))]
      [noop-style (style #f null)])
    (define (make-labeled-row it label)
      ; Based on scribble/private/manual-vars
      (nested-flow
       noop-style
       (list
        (nested-flow RBackgroundLabel
                     (list (nested-flow RBackgroundLabelInner
                                        (list (paragraph omitable label)))))
        (paragraph RForeground it))))
    (define (make-blue-box label content)
      (nested-flow
       (style 'vertical-inset null)
       (list (tabular
              #:style (style 'boxed (list (attributes '([class . "RBoxed"]))))
              (list (list (make-labeled-row content label)))))))
    make-blue-box))



