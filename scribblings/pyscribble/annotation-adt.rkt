#lang racket

(require racket/serialize
         )

(provide annotation?
         structural-ann?
         special-ann?
         (contract-out
          [struct full-mod-name ([v (listof symbol?)])]
          [struct full-value-name ([mod full-mod-name?]
                                   [name symbol?])]
          [struct (atomic-ann ann)
            ([datum (or/c #t #f 'None string? number?)])]
          [struct (other-ann ann)
            ([string string?])]
          [struct (dict-ann structural-ann)
            ([assocs (listof (cons/c annotation? annotation?))])]
          [struct (list-ann structural-ann)
            ([content (listof annotation?)])]
          [struct (tuple-ann structural-ann)
            ([content (listof annotation?)])]
          [struct (class-ann ann)
            ([name full-value-name?])]
          [struct (function-ann ann)
            ([name full-value-name?])]
          [special-ann-name (-> any/c full-value-name?)]
          [struct (named-ann special-ann)
            ([name full-value-name?])]
          [struct (constructor-ann special-ann)
            ([name full-value-name?]
             [args (listof annotation?)])]
          ))

(serializable-struct full-mod-name (v)
  ;; v : (listof symbol?)
  #:transparent)

(serializable-struct full-value-name (mod name)
  ;; mod full-mod-name?
  ;; name symbol?
  #:transparent)

(serializable-struct ann ()
  ;; Abstract!
  #:transparent)

(define-syntax annotation?
  (make-rename-transformer #'ann?))

(serializable-struct atomic-ann ann (datum)
  ;; datum  (or/c #t #f 'None string? number?)
  #:transparent)

(serializable-struct other-ann ann (string)
  #:transparent)

(serializable-struct structural-ann ann ()
  ;; Abstract !!
  #:transparent)

(serializable-struct dict-ann structural-ann (assocs)
  ;; assocs (listof (cons/c annotation? annotation?))
  #:transparent)

(serializable-struct list-ann structural-ann (content)
  ;; content (listof annotation?)
  #:transparent)

(serializable-struct tuple-ann structural-ann (content)
  ;; content (listof annotation?)
  #:transparent)

(serializable-struct class-ann ann (name)
  ;; name full-value-name?
  #:transparent)

(serializable-struct function-ann ann (name)
  ;; name full-value-name?
  #:transparent)

(serializable-struct special-ann ann (name)
  ;; Abstract!
  ;; name full-value-name?
  #:transparent)
  
(serializable-struct named-ann special-ann ()
  #:transparent)

(serializable-struct constructor-ann special-ann (args)
  ;; args (listof annotation?)
  #:transparent)

