#lang racket/base

(require json
         racket/runtime-path
         racket/set
         racket/path
         racket/file
         racket/contract
         )

(define-runtime-path here
  ".")

(write-to-file	
 (set->list
  (for/set ([pth (in-directory here)]
            #:when (equal? #".json"
                           (path-get-extension pth))
            [obj (in-list
                  (with-handlers ([exn:fail? (λ (e)
                                               (eprintf "while working on: ~e\n" pth)
                                               (raise e))])
                    (invariant-assertion list?
                                         (call-with-input-file pth
                                           read-json))))])
    (hash-update obj
                 'pos
                 string->symbol)))
 (build-path here "all.rktd")
 #:exists 'replace)

