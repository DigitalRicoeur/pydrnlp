#lang typed/racket

(require (submod "adt/pass1.rkt" typed)
         (prefix-in pass0:
                    "adt/pass0/doc.rkt")
         )

(require/typed
 "parse-docstring.rkt"
 [docstring/comment/missing->proto-content
  (-> pass0:docstring/comment/missing
      Proto-Content)])

(: modpath-doc:pass0->pass1 (-> pass0:modpath-doc
                                modpath-doc))
(define modpath-doc:pass0->pass1
  (match-lambda
    [(pass0:modpath-doc name 'ErrorDuringImport)
     (modpath-doc
      name
      (module-error 'ErrorDuringImport
                    "A Python error occured while importing the module\n"))]
    [(pass0:modpath-doc name #f)
     (modpath-doc name
                  (module-error 'not-found
                                "Python could not find the module\n"))]
    [(pass0:modpath-doc name (exn:fail message _))
     (modpath-doc name
                  (module-error 'from-json message))]
    [(pass0:modpath-doc name
                        (pass0:module-doc d/c/m
                                          public
                                          private))
     ;; TODO: catch more types of errors
     (modpath-doc name
                  (module-doc (docstring/comment/missing->proto-content
                               d/c/m)
                              (convert-section name public)
                              (convert-section name private)))]))


(: convert-section (-> full-mod-name
                       pass0:module-section
                       module-section))
(define (convert-section mod-name sec)
  (match-define (pass0:module-section classes ann-classes
                                      funcs preds
                                      n-anns)
    sec)
  (module-section
   (map (λ ([it : pass0:class-doc])
          (define-values {name info sig}
            (class-doc*->name+info+signature
             mod-name it))
          (class-doc name info sig))
        classes)
   (map (λ ([it : pass0:annotation-class-doc])
          (define-values {name info sig}
            (class-doc*->name+info+signature
             mod-name it))
          (annotation-class-doc
           name info sig
           (pass0:annotation-class-doc-annotation-class-type it)))
        ann-classes)
   (convert-func*-list mod-name funcs function-doc)
   (convert-func*-list mod-name preds predicate-doc)
   (map (make-convert-named-ann mod-name) n-anns)))


(: class-doc*->name+info+signature (-> full-mod-name
                                       pass0:Class-Doc*
                                       (Values full-value-name
                                               Proto-Content
                                               doc-signature)))
(define (class-doc*->name+info+signature mod-name it)
  (values (full-value-name mod-name
                           (pass0:class-doc*-local-name it))
          (docstring/comment/missing->proto-content
           (pass0:class-doc*-docstring it))
          (pass0:class-doc*-signature it)))


(: convert-func*-list (All (A)
                           (-> full-mod-name
                               (Listof pass0:Function-Doc*)
                               (-> full-value-name
                                   Proto-Content
                                   doc-signature
                                   A)
                               (Listof A))))
(define (convert-func*-list mod-name lst make)
  (map (λ ([it : pass0:Function-Doc*])
         (make (full-value-name mod-name
                                (pass0:function-doc*-local-name it))
               (docstring/comment/missing->proto-content
                (pass0:function-doc*-docstring it))
               (pass0:function-doc*-signature it)))
       lst))

(: make-convert-named-ann (-> full-mod-name
                              (-> pass0:named-ann-doc named-ann-doc)))
(define (make-convert-named-ann mod-name)
  (match-lambda
    [(pass0:named-ann-doc
      (app docstring/comment/missing->proto-content
           info)
      (and name (full-value-name intended-mod intended-declared))
      declared-name
      annotation/singleton)
     (assert (equal? mod-name intended-mod))
     (assert (equal? intended-declared declared-name))
     (named-ann-doc name info annotation/singleton)]))
