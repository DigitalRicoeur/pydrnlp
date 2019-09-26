#lang racket

(provide (except-out (all-from-out racket)
                     #%module-begin)
         (rename-out [modbegin #%module-begin]))

;; TODO: may want to make .s breakable w/ \u200B

;; "https://docs.python.org/%d.%d/library"
;; racket-doc/scribblings/inside/utils.rkt

(module reader syntax/module-reader
  pydrnlp/scribblings/python-doc-lang)

(require scribble/manual
         scribble/decode
         scribble/core
         scribble/manual-struct
         scribble/html-properties
         scribble/racket
         (only-in scribble/bnf [optional optional-brackets])
         setup/collects
         racket/undefined
         pkg/path
         pydrnlp/support/python-lang/prefabs
         syntax/parse/define
         markdown/parse
         "markdown.rkt"
         (for-label racket/base
                    (only-in pydrnlp/support/worker
                             python-revision-value/c))
         (for-syntax pydrnlp/support/python-lang/stx
                     pydrnlp/support/python-lang/prefabs
                     racket/string
                     racket/match
                     (only-in syntax/parse [attribute $])))

(define-syntax-parser modbegin
  [(_ :python-module-forms)
   #:do [(define public-definitions
           (filter (λ (d) (public-name? (syntax-e (definition-name d))))
                   ($ definitions)))
         (define maybe-revision-fun-definition
           (findf (match-lambda
                    [(fun-definition (app syntax-e "revision") _ _ _ _)
                     #t]
                    [_
                     #f])
                  public-definitions))]
   #:with ((revision-fun-label-requires ...) revision-fun-expr)
   (if maybe-revision-fun-definition
       (prepare-revision-fun-stx
        maybe-revision-fun-definition
        #:module-imports-table ($ module-imports-table)
        #:value-imports-table ($ value-imports-table))
       #'(() '()))
   #`(#%plain-module-begin
      (module configure-runtime '#%kernel
        (#%require racket/runtime-config)
        (configure #f))
      (require (for-label revision-fun-label-requires ...
                          (submod "..")))
      (provide doc)
      (define doc
        (parameterize ([current-module-imports-table
                        '#,($ module-imports-table)]
                       [current-value-imports-table
                        '#,($ value-imports-table)])
          (make-python-section (#%variable-reference)
                               'docstring
                               revision-fun-expr
                               '#,(remove maybe-revision-fun-definition
                                          public-definitions)))))])

(define-for-syntax (prepare-revision-fun-stx
                    maybe-revision-fun-definition
                    #:module-imports-table module-imports-table
                    #:value-imports-table value-imports-table)
  (define/syntax-parse escape #'unsyntax)
  (define (prune-srcloc stx)
    (define (strip stx)
      (cond
        [(syntax->list stx)
         => (λ (lst) (map strip lst))]
        [else
         (datum->syntax stx (syntax->datum stx))]))
    (strip stx))
  (define mut-label-requires null)
  (define/syntax-parse return-form
    (match (fun-definition-maybe-return maybe-revision-fun-definition)
      [(? syntax? stx)
       (prune-srcloc
        (fixup-static-return
         stx
         #:module-imports-table module-imports-table
         #:value-imports-table value-imports-table
         #:on-import-found
         (λ (src context)
           (define id
             ((make-syntax-introducer)
              (datum->syntax #f
                             (string->symbol
                              (string-join src "."
                                           #:after-last ".revision"))
                             (car context))))
           (set! mut-label-requires
                 (cons #`(only-in #,(pydrnlp-raw-root-module-path src)
                                  [revision #,id])
                       mut-label-requires))
           (define/syntax-parse their-revision id)
           #'(their-revision))
         #:on-error (λ (stxs msg)
                      #`(escape (render-static-return-error '#,msg)))))]
      [(bad-return _)
       #`(escape (render-static-return-error '#,(bad-return-message-suffix)))]
      [_
       #`(escape (render-static-return-error
                  '";\n unsupported syntax in function body"))]))
  (list mut-label-requires
        ;; target-element w/ python id ?
        #`(defproc (revision)
                   python-revision-value/c
                   #:value return-form
                   (convert-docstring '#,(definition-docstring
                                          maybe-revision-fun-definition)))))

(define current-module-imports-table
  (make-parameter undefined))
(define current-value-imports-table
  (make-parameter undefined))

(define-for-syntax (public-name? str)
  (and (non-empty-string? str)
       (or (not (eqv? #\_ (string-ref str 0)))
           (regexp-match? #rx"^__.*__$" str))))

(define (render-static-return-error msg-suffix)
  (nested
   #:style error-color
   (verbatim "render: unsupported syntax" msg-suffix)))

(define (convert-docstring docstring/false)
  (if docstring/false
      (with-handlers ([exn:fail? (λ (e)
                                   (verbatim docstring/false))])
        (render-markdown-xexprs
         (parse-markdown docstring/false)))
      null))

(define (make-python-section varref docstring revision-fun-expr definitions)
  (define pkg-cache (make-hash))
  (define normalized-module-path
    (let* ([mod (resolved-module-path-name
                 (variable-reference->resolved-module-path
                  varref))]
           [mod (if (pair? mod)
                    (car mod)
                    mod)])
      (if (path? mod)
          (path->module-path mod #:cache pkg-cache)
          `',mod)))
  (define python-module-name
    (let ([pth-str
           (match normalized-module-path
             [`',sym
              (symbol->string sym)]
             [`(lib ,str)
              (string-trim str #rx"pydrnlp/py/" #:right? #f)]
             [(? path? pth)
              (path->string pth)])])
      (regexp-replace* #rx"/"
                       (string-trim pth-str #rx"\\.py" #:left? #f)
                       ".")))
  (define maybe-pkg
    (match normalized-module-path
      [(or `(lib ,p) (? path? p))
       (path->pkg p #:cache pkg-cache)]
      [_
       #f]))
  (define mod-path-tag
    (intern-taglet `(mod-path ,(format "~s" normalized-module-path))))
  (define mod-link
    (link-element module-link-color
                  (racketmodfont python-module-name)
                  mod-path-tag))
  (decode
   (list
    (title (racketmodfont python-module-name))
    (declare-exporting ,normalized-module-path)
    (make-defmodule-box mod-link
                        mod-path-tag
                        python-module-name
                        maybe-pkg)
    (part-tag-decl mod-path-tag)
    (convert-docstring docstring)
    revision-fun-expr
    (parameterize ([current-mod-path-tag mod-path-tag])
      (map render-child
           definitions)))))


(define current-class-name
  (make-parameter #f))
(define current-mod-path-tag
  (make-parameter undefined))


(define (render-child it)
  (cond
    [(fun-definition? it)
     (render-fun-definition it)]
    [(class-definition? it)
     (render-class-definition it)]
    [else
     (render-var-definition it)]))
  
(define (render-class-definition this)
  ;; TODO: consider content-width block-width current-display-width
  (match-define (class-definition name docstring supers body) this)
  (list (make-blue-box #:label "Python class"
                       (list (racketidfont "class")
                             spacer
                             (defpythonid name)
                             open
                             (add-between (map (λ (parts)
                                                 (racketidfont
                                                  (string-join parts ".")))
                                               supers)
                                          comma-space)
                             close))
        (convert-docstring docstring)
        (parameterize ([current-class-name name])
          (nested #:style 'inset
                  (map render-child body)))))


(define (render-var-definition this)
  (match-define (var-definition name docstring) this)
  (list (make-blue-box #:label (if (current-class-name)
                                   "Python class attribute"
                                   "Python value")
                       (defpythonid (literal name)))
        (convert-docstring docstring)))


(define (render-fun-definition this)
  (match-define (fun-definition name docstring formals async? _) this)
  ;; TODO: consider content-width block-width current-display-width
  (list (make-blue-box #:label (if (current-class-name)
                                   "Python function"
                                   "Python method")
                       (list (racketidfont (if async? "async def" "def"))
                             spacer
                             (defpythonid name)
                             open
                             (render-fun-formals formals)
                             close))
        (convert-docstring docstring)))

;; References:
;;   - for formals list:
;;       https://github.com/python/cpython/blob/25221b3/Lib/inspect.py#L3033
;;   - for individual formal:
;;       https://github.com/python/cpython/blob/25221b3/Lib/inspect.py#L2549
;;  We ignore by-position-only arguments because they can only
;;  come from C extensions: there is no syntax to create them in Python.
(define/contract (render-fun-formals formals)
  (-> list? (listof content?))
  (add-between
   (map (match-lambda
          [(list '*)
           star]
          [(list 'required arg)
           (racketvarfont arg)]
          [(list-rest 'optional arg _)
           ;; TODO: render simple defaults
           (optional-brackets (racketvarfont arg))]
          [(list '* arg)
           (list star (racketvarfont arg))]
          [(list '** arg)
           (list star-star (racketvarfont arg))])
        formals)
   comma-space))


(define (defpythonid str)
  (define class-name (current-class-name))
  (define tag (python-id-tag str))
  (toc-target2-element value-def-color
                       (element symbol-color
                                (link-element value-def-color str tag))
                       tag
                       (if class-name
                           (list (italic (literal class-name "."))
                                 (element symbol-color str))
                           (element symbol-color str))))

(define (python-id-tag #:mod-path-tag [mod (current-mod-path-tag)]
                       #:class [class-name (current-class-name)]
                       name)
  (intern-taglet (list 'python-id (if class-name
                                      (list mod class-name name)
                                      (list mod name)))))


(define open (racketparenfont "("))
(define close (racketparenfont ")"))
(define comma (racketparenfont ","))
(define star (racketparenfont "*"))
(define star-star (racketparenfont "**"))
(define spacer (hspace 1))
(define comma-space (list comma spacer))
(define omitable-style (style #f '(omitable)))
(define/contract (omitable-para . args)
  (-> content? ... block?)
  (paragraph omitable-style args))
(define vertical-inset-style
  (style 'vertical-inset null))
  
(define RBackgroundLabel
  (style "RBackgroundLabel"
         (list 'decorative 'command (alt-tag "div")
               (attributes '((class . "SIEHidden"))))))
(define RBackgroundLabelInner
  (style "RBackgroundLabelInner" (list (alt-tag "div"))))
(define RForeground
  (style #f (list (attributes '((class . "RForeground"))))))
(define RBoxed
  (style 'boxed (list (attributes '([class . "RBoxed"])))))
(define/contract (make-blue-box #:label label content)
  (-> #:label string? content? block?)
  ;; Based on scribble/private/manual-vars
  (define (make-labeled-row it #:label label)
    (nested-flow
     plain
     (list
      (nested-flow RBackgroundLabel
                   (list (nested-flow RBackgroundLabelInner
                                      (list (omitable-para label)))))
      (paragraph RForeground it))))
  (nested-flow
   vertical-inset-style
   (list (tabular
          #:style RBoxed
          (list (list (make-labeled-row content #:label label)))))))


(define/contract (make-defmodule-box mod-link
                                     mod-path-tag
                                     python-module-name
                                     maybe-pkg)
  (unconstrained-domain-> pre-part?)
  ;; copied from scribble/private/manual-mod
  (define lib-para
    (omitable-para spacer
                   (racketmodfont "import") 
                   spacer
                   (index-element #f
                                  mod-link
                                  mod-path-tag
                                  (list python-module-name)
                                  (list mod-link)
                                  (module-path-index-desc))))
  (define pkg-para
    (and maybe-pkg
         (omitable-para (elem #:style "RpackageSpec"
                              (list (smaller 'nbsp "package:")
                                    " "
                                    (racketpkgname maybe-pkg))))))
  (table
   (style "defmodule"
          (list (table-columns (list (make-style #f '(left))
                                     (make-style #f '(right))))))
   (cond
     [(not maybe-pkg)
      (list
       (list lib-para 'cont))]
     [(< (+ (block-width lib-para) (block-width pkg-para) 8)
         (current-display-width))
      (list
       (list lib-para pkg-para))]
     [else
      (list
       (list lib-para 'cont)
       (list (omitable-para 'nbsp) pkg-para))])))
   

(define (racketpkgname pkg)
  ;; copied from scribble/private/manual-mod
  (link
   (format "https://pkgs.racket-lang.org/package/~a" pkg)
   (tt pkg)
   #:style (style #f (list "plainlink"
                           (hover-property
                            (format "Install this package using `raco pkg install ~a`"
                                    pkg))))))

(define (render-markdown-xexprs xs)
  (xexprs->scribble-pres
   xs
   (match-lambda
     [`(,(and sec (or 'h1 'h2 'h3)) ,_ . ,body)
      (para (bold body))]
     [(or `(code () ,s)
          `(code ([class "brush: python"]) ,s))
      ;; inline python
      #f]
     [`(pre ([class "brush: python"]) (code () ,s))
      ;; block python
      #f]
     [_
      #f])))
