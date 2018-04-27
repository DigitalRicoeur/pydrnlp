#lang racket

(require racket/runtime-path
         racket/hash
         )

(define-syntax-rule (define-runtime-file->value name rhs)
  (begin (define-runtime-path pth rhs)
         (define name (file->value pth))))

(define-runtime-file->value plain
  "plain-env.rktd")

(define-runtime-file->value conda
  "conda-env.rktd")

(define-runtime-file->value venv
  "venv-env.rktd")

(struct var (name plain conda venv)
  #:transparent)

(define (make-var name)
  (let ([p (hash-ref plain name #f)]
        [c (hash-ref conda name #f)]
        [v (hash-ref venv name #f)])
    (cond
      [(and (equal? p c)
            (equal? p v))
       #f]
      [else
       (var name p c v)])))

(for*/list ([name (in-immutable-hash-keys
                   (hash-union plain conda venv
                               #:combine (λ (a b) 'combined)))]
            [var (in-value (make-var name))]
            #:when var)
  var)


