#lang typed/racket

;; The types `Lemma-Table` and `Lemma/Count` count
;; occurances of lemmas and track the best representative
;; string forms observed in some scope (a corpus, a document, etc.).

;; Start with `empty-lemma-table` and build up tables using
;; `lemma-table-update` and `lemma-table-union`.
;; The `Lemma/Count` type uses less memory: switch to it when you are ready
;; to discard the string components.
;; The serialization approach of `lemma-table->blobs` lets you
;; avoid even reading the string data into memory unless necessary,
;; while still maintaining strong invariants.

;; FIXME: separate by language?

(provide lemma-table?
         Lemma-Table
         lemma/count?
         Lemma/Count
         empty-lemma-table
         ;; access
         lemma/string-ref
         lemma/count-ref
         lemma/count-total
         ;; update
         lemma-table-update
         lemma-table-union
         ;; conversion
         lemma-table->lemma/count
         lemma-table->blobs
         blobs->lemma-table
         blob->lemma/count)

(module+ hacks
  ;; until I work out a good API
  (provide lemma/count-hsh
           union:lemma/count
           lemma-table-filter ;; ???
           (rename-out [lemma/count HACK-make-lemma/count])))

(require/typed
 typed/racket
 [unquoted-printing-string
  (-> String Any)])

(require/typed
 racket/fasl
 [s-exp->fasl
  (-> Any Bytes)]
 [fasl->s-exp
  (-> Bytes Any)])

;; Using typed wrappers for lemma/count and lemma/string
;; should avoid overhead for contract checks.

;; TODO: is there a fancy data structure that might make
;; the union:lemma/* functions better?
;; (& be directly iterable?)

;; vid.
;;  - "Batched Rebuilding" Okasaki 8.1 p99
;;  - "Structural Abstraction" Okasaki 10.2 p151
;;      "typically used to extend an implementation of collections
;;      ... with an efficient join function"

;                              
;                              
;   ;;                         
;   ;;                         
;  ;;;;;    ; ; ;;    ;;    ;; 
;   ;;  ;  ;  ;;  ;  ;  ; ;;  ;
;   ;;  ;  ;  ;;  ;  ;  ;  ;   
;   ;;  ;  ;  ;;  ;;;;;;;;  ;; 
;   ;;   ; ;  ;;  ;  ;        ;
;    ;   ;;   ;;  ;  ;    ;   ;
;    ;;;  ;   ;;;;    ;;;  ;;; 
;         ;   ;;               
;        ;    ;;               
;      ;;     ;;               
;                              

(define-type Lemma-Table lemma-table)
(define-type Lemma/Count lemma/count)

(struct lemma-table ([hsh : Lemma-Table-Hash])
  #:transparent)
(define-type Lemma-Table-Hash
  (Immutable-HashTable Symbol (Pairof Exact-Positive-Integer String)))

(struct lemma/count ([hsh : Lemma/Count-Hash])
  #:transparent)
(define-type Lemma/Count-Hash
  (Immutable-HashTable Symbol Exact-Positive-Integer))

(define empty-lemma-table
  (lemma-table #hasheq()))


(: lemma/string-ref (-> lemma-table Symbol (U #f String)))
(define (lemma/string-ref tbl k)
  (cond
    [(hash-ref (lemma-table-hsh tbl) k #f)
     => cdr]
    [else
     #f]))

(: lemma/count-ref (-> (U lemma-table lemma/count) Symbol (U #f Exact-Positive-Integer)))
(define (lemma/count-ref tbl k)
  (cond
    [(lemma/count? tbl)
     (hash-ref (lemma/count-hsh tbl) k #f)]
    [(hash-ref (lemma-table-hsh tbl) k #f)
     => car]
    [else
     #f]))

(: lemma/count-total (-> (U lemma-table lemma/count) Exact-Nonnegative-Integer))
(define (lemma/count-total tbl)
  (define-type E-P-I Exact-Positive-Integer)
  (define #:∀ (V α) (hash-sum [hsh : (Immutable-HashTable α V)]
                              [unwrap : (-> V Exact-Positive-Integer)])
    : Exact-Nonnegative-Integer
    (for/fold ([acc : Exact-Nonnegative-Integer 0])
              ([v : V (in-immutable-hash-values hsh)])
      (+ acc (unwrap v))))
  (cond
    [(lemma/count? tbl)
     ((inst hash-sum E-P-I) (lemma/count-hsh tbl) (λ (x) x))]
    [else
     ((inst hash-sum (Pairof E-P-I String)) (lemma-table-hsh tbl) car)]))

;                                        
;                                        
;                     ;;        ;;       
;                     ;;        ;;       
;   ;; ;;  ; ;;    ;;;;;  ;;   ;;;;; ;;  
;   ;; ;;  ;;  ;  ;   ;; ;  ;   ;;  ;  ; 
;   ;; ;;  ;;  ;  ;   ;;    ;;  ;;  ;  ; 
;   ;; ;;  ;;  ;;;;   ;;  ;;;;  ;; ;;;;;;
;   ;; ;;  ;;  ;  ;   ;; ;  ;;  ;;  ;    
;    ; ;;  ;;  ;  ;   ;;;;  ;;   ;  ;    
;    ;;;;  ;;;;    ;;; ; ;;; ;   ;;; ;;; 
;          ;;                            
;          ;;                            
;          ;;                            
;                                        

(: lemma-table-update (-> lemma-table Symbol String lemma-table))
(define (lemma-table-update tbl k v)
  (define hsh (lemma-table-hsh tbl))
  (lemma-table
   (hash-set hsh k (match (hash-ref hsh k #f)
                     [(cons i old)
                      (cons (add1 i) (choose old v))]
                     [_
                      (cons 1 v)]))))

(define lemma-table-union
  (case-lambda
    [()
     empty-lemma-table]
    [([tbl : lemma-table])
     tbl]
    [([a : lemma-table] [b : lemma-table])
     (lemma-table
      (lemma-table-union* (lemma-table-hsh a)
                          (lemma-table-hsh b)))]
    [([tbl0 : lemma-table] [tbl1 : lemma-table] [tbl2 : lemma-table] . [tbl* : lemma-table *])
     ;; arities must be distinct for TR to make a contract
     (lemma-table
      (for/fold ([acc : Lemma-Table-Hash (lemma-table-hsh tbl0)])
                ([tbl (in-list (list* tbl1 tbl2 tbl*))])
        (lemma-table-union* acc
                            (lemma-table-hsh tbl))))]))

(: lemma-table-union* (-> Lemma-Table-Hash Lemma-Table-Hash Lemma-Table-Hash))
(define (lemma-table-union* dest src)
  (let-values ([{src dest}
                ;; iterate over smaller table
                (if (< (hash-count dest) (hash-count src))
                    (values dest src)
                    (values src dest))])
    (for/fold ([dest : Lemma-Table-Hash dest])
              ([{k pr} (in-immutable-hash src)])
      (hash-set dest
                k
                (match (hash-ref dest k #f)
                  [(cons n str)
                   (cons (+ n (car pr))
                         (choose str (cdr pr)))]
                  [_
                   pr])))))


(: choose (-> String String String))
(define (choose old new)
  ;; we now rely on SpaCy to do the right thing re case-folding
  (if (< (string-length new) (string-length old))
      new
      old))


;; HACK for transition: find a better API
(: union:lemma/count (-> lemma/count * lemma/count))
(define (union:lemma/count . args)
  (match args
    ['() (lemma/count #hasheq())]
    [(list it) it]
    [(cons (lemma/count acc) args)
     (lemma/count
      (for*/fold ([acc : Lemma/Count-Hash acc])
                 ([this (in-list args)]
                  [{k v} (in-immutable-hash (lemma/count-hsh this))])
        (hash-set acc k (+ v (hash-ref acc k (λ () 0))))))]))

(: lemma-table-filter (-> lemma-table
                          (-> Symbol Exact-Positive-Integer String Any)
                          lemma-table))
(define (lemma-table-filter tbl proc)
  (lemma-table
   (for/hasheq : Lemma-Table-Hash ([{k pr} (in-immutable-hash (lemma-table-hsh tbl))]
                                  #:when (proc k (car pr) (cdr pr)))
     (values k pr))))


;                                                             
;                                                             
;                                             ;;              
;                                                             
;    ;;;   ;;;   ; ;;; ;    ;  ;;   ;; ;  ;; ;;;  ;;;   ; ;;; 
;   ;   ; ;   ;  ;;  ;  ;  ;  ;  ;  ;;; ;;  ; ;; ;   ;  ;;  ; 
;   ;     ;   ;  ;;  ;; ;  ;  ;  ;  ;;   ;    ;; ;   ;  ;;  ;;
;  ;;    ;;   ;; ;;  ;; ;  ; ;;;;;; ;;    ;;  ;;;;   ;; ;;  ;;
;   ;     ;   ;  ;;  ;;  ; ;  ;     ;;      ;;;; ;   ;  ;;  ;;
;   ;   ; ;   ;  ;;  ;;  ;;   ;     ;;  ;   ; ;; ;   ;  ;;  ;;
;    ;;;   ;;;   ;;  ;;  ;;    ;;;  ;;   ;;;  ;;  ;;;   ;;  ;;
;                                                             
;                                                             

(: lemma-table->lemma/count (-> lemma-table lemma/count))
(define (lemma-table->lemma/count tbl)
  (lemma/count
   (for/hasheq : Lemma/Count-Hash ([{k pr} (in-immutable-hash (lemma-table-hsh tbl))])
     (values k (car pr)))))


(define-type Counts-Blob-Payload
  (Pairof (Immutable-Vectorof Symbol)
          (Immutable-Vectorof Exact-Positive-Integer)))
(define-type Strings-Blob-Payload
  (Pairof Bytes
          (Immutable-Vectorof String)))
          

(: lemma-table->blobs (-> lemma-table (values Bytes Bytes)))
(define (lemma-table->blobs tbl)
  (define hsh (lemma-table-hsh tbl))
  (define count : Index (hash-count hsh))
  (define keys : (Mutable-Vectorof Symbol)
    (make-vector count '|shouldn't get here|))
  (define strings : (Mutable-Vectorof String)
    (make-vector count "shouldn't get here"))
  (define counts : (Mutable-Vectorof Exact-Positive-Integer)
    (make-vector count 42))
  (for ([i (in-range 0 count)]
        [{k pr} (in-immutable-hash hsh)])
    (vector-set! keys i k)
    (vector-set! counts i (car pr))
    (vector-set! strings i (cdr pr)))
  (define a
    (bytes->immutable-bytes
     (s-exp->fasl (ann (cons (vector->immutable-vector keys)
                             (vector->immutable-vector counts))
                       Counts-Blob-Payload))))
  (values a
          (bytes->immutable-bytes
           (s-exp->fasl (ann (cons (sha256-bytes a)
                                   (vector->immutable-vector strings))
                             Strings-Blob-Payload)))))


(: blob->lemma/count (-> Bytes lemma/count))
(define (blob->lemma/count bs)
  (define-values [symbols counts]
    (unpack-counts-blob bs #:who 'blob->lemma/count #:blob2 #f))
  (lemma/count
   (for/hasheq : Lemma/Count-Hash ([k (in-vector symbols)]
                                   [i (in-vector counts)])
     (values k i))))


(: blobs->lemma-table (-> Bytes Bytes lemma-table))
(define (blobs->lemma-table count-bs str-bs)
  (define who 'blobs->lemma-table)
  (define-syntax-rule (bad! arg ...)
    (bad*! who count-bs str-bs 'second arg ...))
  (define checksum-again
    (sha256-bytes count-bs))
  (define-values [symbols counts]
    (unpack-counts-blob count-bs #:who who #:blob2 str-bs))
  (define strings-payload (cast (fasl->s-exp str-bs) Strings-Blob-Payload))
  (unless (bytes=? checksum-again (car strings-payload))
    (bad! #:msg "checksums do not match" (car strings-payload) checksum-again))
  (define strings (cdr strings-payload))
  (check-vectors-same-length! symbols strings "strings" bad!)
  (lemma-table
   (for/hasheq : Lemma-Table-Hash ([k (in-vector symbols)]
                                   [i (in-vector counts)]
                                   [str (in-vector strings)])
     (values k (cons i str)))))



(: unpack-counts-blob (-> Bytes #:who Symbol #:blob2 (U #f Bytes)
                          (values (Immutable-Vectorof Symbol)
                                  (Immutable-Vectorof Exact-Positive-Integer))))
(define (unpack-counts-blob bs #:who who #:blob2 blob2)
  (define-syntax-rule (bad! arg ...)
    (bad*! who bs blob2 'first arg ...))
  (define pr (cast (fasl->s-exp bs) Counts-Blob-Payload))
  (define symbols (car pr))
  (define counts (cdr pr))
  (check-vectors-same-length! symbols counts "counts" bad!)
  (values symbols counts))




(: bad*! (->* [Symbol Bytes (U #f Bytes) (U 'first 'second) Any Any]
              [#:msg (U #f String)]
              Nothing))
(define (bad*! who bs extra-bs which expected given #:msg [msg+ #f])
  (define msg "malformed blob")
  (apply raise-arguments-error
         who
         (if msg+ (string-append msg ";\n " msg+) msg)
         "expected" (if (string? expected)
                        (unquoted-printing-string expected)
                        expected)
         "given" given
         (cond
           [(not extra-bs)
            (list "blob..." bs)]
           [(eq? 'first which)
            (list "blob..." bs
                  "second blob..." extra-bs)]
           [else
            (list "blob..." extra-bs
                  "first blob..." bs)])))



(define (veclen-help [a : (Immutable-Vectorof Any)] [b : (Immutable-Vectorof Any)]
                     [str : String] [bad! : (-> Index Index String Nothing)])
  (unless (= (vector-length a) (vector-length b))
    (define msg (string-append "vector of " str " is the wrong length"))
    (bad! (vector-length a) (vector-length b) msg)))
(define-syntax-rule (check-vectors-same-length! a b of bad!)
  (veclen-help a b of (λ (e g [msg+ : String]) (bad! e g #:msg msg+))))

