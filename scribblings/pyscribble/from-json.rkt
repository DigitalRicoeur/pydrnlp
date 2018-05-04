#lang racket

(require json
         "from-json/from-json.rkt"
         "from-json/to-pass1.rkt"
         "adt.rkt"
         )

(provide (contract-out
          [jsexpr->modpath-doc
           (-> (list/c string? jsexpr?)
               modpath-doc?)]
          ))

(define (jsexpr->modpath-doc js)
  (modpath-doc:pass0->pass1
   (jsexpr->pass0 js)))

