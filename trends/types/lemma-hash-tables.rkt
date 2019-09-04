#lang typed/racket

(provide (struct-out lemma/count)
         empty:lemma/count
         lemma/count-ref
         union:lemma/count
         add1:lemma/count
         total:lemma/count
         (struct-out lemma/string)
         empty:lemma/string
         lemma/string-ref
         union:lemma/string
         update:lemma/string)

;; TODO: is there a fancy data structure that might make
;; the union:lemma/* functions better?
;; (& be directly iterable?)

;; vid.
;;  - "Batched Rebuilding" Okasaki 8.1 p99
;;  - "Structural Abstraction" Okasaki 10.2 p151
;;      "typically used to extend an implementation of collections
;;      ... with an efficient join function"


;; Using typed wrappers for lemma/count and lemma/string
;; should avoid overhead for contract checks.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lemma/count
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A lemma/count table maps lemmas (Symbol)
;; to numbers of occurances (Exact-Positive-Integer).

;; The function union:lemma/count combines some number of lemma/count
;; tables by taking the sum of all occurances for all lemmas.

;; The function add1:lemma/count registers an additional occurance
;; of the given lemma (possibly the first occurance).

(define-type Count-Hash
  (Immutable-HashTable Symbol Exact-Positive-Integer))

(struct lemma/count ([hsh : Count-Hash])
  #:transparent)

(define empty:lemma/count
  (lemma/count #hasheq()))

(: lemma/count-ref (-> lemma/count Symbol (U False Exact-Positive-Integer)))
(define (lemma/count-ref this k)
  (hash-ref (lemma/count-hsh this) k #f))

(: union:lemma/count (->* (lemma/count) #:rest lemma/count lemma/count))
(define union:lemma/count
  (case-lambda
    [(base) base]
    [(base . more)
     (lemma/count
      (for*/fold ([base : Count-Hash (lemma/count-hsh base)])
                 ([this (in-list more)]
                  [{lemma n} (in-immutable-hash
                              (lemma/count-hsh this))])
        (hash-set base
                  lemma
                  (+ n (hash-ref base lemma (λ () 0))))))]))

(: add1:lemma/count (-> lemma/count Symbol lemma/count))
(define (add1:lemma/count this lemma)
  (define hsh (lemma/count-hsh this))
  (lemma/count
   (hash-set hsh lemma (add1 (hash-ref hsh lemma (λ () 0))))))


(: total:lemma/count (-> lemma/count Exact-Nonnegative-Integer))
(define (total:lemma/count this)
  (for/fold ([ret : Exact-Nonnegative-Integer 0])
            ([i : Exact-Positive-Integer (in-immutable-hash-values (lemma/count-hsh this))])
    (+ i ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lemma/string
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A lemma/string table maps lemmas (Symbol)
;; to the best representative String form observed in some scope
;; (a corpus, a document, etc.).

;; The lemma/string supports two public operations:
;; update:lemma/string and union:lemma/string.

(define-type String-Hash
  (Immutable-HashTable Symbol String))

(struct lemma/string ([hsh : String-Hash])
  #:transparent)

(define empty:lemma/string
  (lemma/string #hasheq()))

(: lemma/string-ref (-> lemma/string Symbol (U False String)))
(define (lemma/string-ref this k)
  (hash-ref (lemma/string-hsh this) k #f))

;; The function update:lemma/string extends a lemma/string
;; to incorporate an additional observed string form of some lemma symbol.
;; If the string argument to update:lemma/string is a better representative
;; for the lemma than the representative string previously recorded,
;; the lemma/string table is updated.
;; If the lemma hasn't been observed before,
;; this strng form is the best by definition and is simply added to the table.
;; If the old representative string is as good or better than the new one,
;; the lemma/string argument is returned unchanged.
;; For the definition of "better," see which-representative-string-is-better?.

(: update:lemma/string (-> lemma/string Symbol String lemma/string))
(define (update:lemma/string l/s lemma text)
  (cond
    [(maybe-update-string-hash (lemma/string-hsh l/s)
                               lemma
                               text)
     => lemma/string]
    [else
     l/s]))


(: maybe-update-string-hash (-> String-Hash Symbol String (U String-Hash False)))
(define (maybe-update-string-hash hsh lemma text)
  ;; Returns an updated String-Hash or #f if no change is needed.
  ;; This is a helper for both update:lemma/string and union:lemma/string
  ;; to avoid allocation.
  ;; (Note: Optimization Coach reports this isn't inlined,
  ;;  and begin-encourage-inline doesn't help.
  ;;  Is it worth making this a macro?)
  (define maybe-old
    (hash-ref hsh lemma #f))
  (case (which-representative-string-is-better? #:old maybe-old
                                                #:new text)
    [(old)
     #f]
    [else
     (hash-set hsh lemma text)]))


(: which-representative-string-is-better?
   (-> #:new String #:old (U String False) (U 'old 'new)))
(define (which-representative-string-is-better? #:new new #:old maybe-old)
  (cond
    ;; If there was no old string, the new one is better by definition.
    [(not maybe-old)
     'new]
    ;; Optimization: if the two are eq?,
    ;; the new one is not "better" (they are the same).
    ;; This is particularly useful because these strings should be interned,
    ;; and most of the time we expect to be dealing with the case-folded form.
    [(eq? maybe-old new)
     'old]
    ;; If a string ever occurs in case-folded form, we prefer the case-folded form.
    ;; (Some strings, like "DeWert", won't and shouldn't ever appear in case-folded form,
    ;; which is why all of this logic is necessary.)
    ;; Thus, if the old version is already case-folded, we keep it and avoid allocation.
    [(string-folds-to-self? maybe-old)
     'old]
    ;; If the old version is not case-folded, we replace it with the new one.
    ;; The new one may or may not already be case-folded:
    ;; we don't check now (a) because we have no principled basis on which
    ;; to say that one non-case-folded string is "better" than another,
    ;; and (b) potentially avoiding string-folds-to-self? may be faster.
    [else
     'new]))

;; The function union:lemma/string combines several lemma/string tables,
;; choosing the overall best representative strings via maybe-update-string-hash.

(: union:lemma/string (->* (lemma/string) #:rest lemma/string lemma/string))
(define union:lemma/string
  (case-lambda
    [(base) base]
    [(base . more)
     (lemma/string
      (for*/fold ([base : String-Hash (lemma/string-hsh base)])
                 ([this (in-list more)]
                  [{lemma text} (in-immutable-hash
                                 (lemma/string-hsh this))])
        (or (maybe-update-string-hash base lemma text)
            base)))]))

;; Support for recognizing case-folded strings:
;; string-folds-to-self? is memoized with an eq?-based
;; cache, which should help across scopes,
;; especially as we expect to deal with interned strings
;; that mostly will be case-folded.

(: cache:string-folds-to-self (Weak-HashTable String Boolean))
(define cache:string-folds-to-self
  (make-weak-hasheq))

(: string-folds-to-self? (-> String Boolean))
(define (string-folds-to-self? str)
  (define cached
    (hash-ref cache:string-folds-to-self str (λ () 'missing)))
  (cond
    [(not (eq? 'missing cached))
     cached]
    [else
     (define ?
       (equal? str (string-foldcase str)))
     (hash-set! cache:string-folds-to-self str ?)
     ?]))










