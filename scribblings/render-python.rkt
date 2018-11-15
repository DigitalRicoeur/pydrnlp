#lang _-exp racket

(require pydrnlp/doc
         pydrnlp/conda
         "markdown.rkt"
         adjutor
         scribble/manual
         scribble/base
         scribble/core
         scribble/decode
         scribble/html-properties
         racket/runtime-path)

(provide rendered)

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

;; "https://docs.python.org/%d.%d/library"

(match-define (list |(| |)| |.| |,| : =-elem return->)
  (map (compose1 racketparenfont
                 symbol->string)
       '(|(| |)| |.| |,| : = ->)))

(define space
  (tt (literal " ")))

(define (plain-code s)
  (racketplainfont (literal s)))

(struct content+width (content w)
  #:transparent)

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


(define render-text
  (match-lambda
    [(docstring xs)
     (render-markdown-xexprs xs)]
    [(comments s)
     (list (verbatim s))]
    [_ null]))

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

(define (symbol->id-content s)
  (racketidfont
   (litchar (symbol->string s))))

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



(define (decode-part* stuff)
  (decode-part stuff null #f 0))

(struct pre-mod-part (pth body)
  #:transparent)

(define pre-lst
  (for/list ([{modpth m} (in-immutable-hash parsed-docs)])
    (define modpth-str (format-dotted-name modpth))
    (pre-mod-part
     modpth
     (cons
      (title (tt modpth-str))
      (match m
        ['not-found
         (list ƒbold{Module not found})]
        ['error-during-import
         (list ƒbold{Python encountered an error while
          importing this module.})]
        [(module-doc name text classes functions)
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
                      functions))])))))

(define order-specs
  '[(pydrnlp [language
              stop_words
              jsonio
              (tokenizer [tokenize
                          usetoken])
              stdio
              doc])])

(define order-tbl
  (let register-specs ([tbl #hash()]
                       [parent null]
                       [lst order-specs])
    (define (snoc v prefix)
      (append prefix (list v)))
    (define (update pth tbl)
      (if (null? parent)
          tbl
          (hash-update tbl
                       parent
                       (λ (before) (snoc pth before))
                       null)))
    (for/fold ([tbl tbl])
              ([spec (in-list lst)])
      (match spec
        [(list sym lst)
         (define pth (snoc sym parent))
         (register-specs (update pth tbl) pth lst)]
        [sym
         (update (snoc sym parent) tbl)]))))

(define (organize-child-parts pth children)
  (define fixed-order
    (hash-ref order-tbl pth null))
  (define-values {in more}
    (partition (match-lambda
                 [(cons pth _)
                  (member pth fixed-order)])
               children))
  (append
   (for/list ([pth (in-list fixed-order)])
     (findf (λ (pr) (equal? pth (car pr))) in))
   (sort more string-ci<?
         #:key (compose1 format-dotted-name car))))

(define rendered
  (map
   cdr
   (let combine-and-render
     ([lst (sort pre-lst <
                 #:key (compose1 length pre-mod-part-pth))])
     (match lst
       ['() null]
       [(cons (pre-mod-part pth body) lst)
        (define pth-len (length pth))
        (define children
          (filter (λ (pre-m)
                    (equal? pth (take (pre-mod-part-pth pre-m)
                                      pth-len)))
                  lst))
        (let* ([lst (remove* children lst)]
               [children (combine-and-render children)]
               [children (organize-child-parts pth children)]
               [children (map cdr children)])
          (cons
           (cons pth (decode-part* (append body children)))
           (combine-and-render lst)))]))))
               
  
