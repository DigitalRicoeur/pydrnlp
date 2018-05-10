#lang typed/racket

(require adjutor
         typed/pict
         "pict.rkt"
         )

(: make-archimedean-spiral (-> Float (-> Float (Pairof Float Float))))
(define ((make-archimedean-spiral r) t)
  (cons (* r t (cos t)) 
        (* r t (sin t))))

(define arch
  (make-archimedean-spiral 8.0))

(define-syntax-rule (quasistream+ a b)
  (quasistream a (delay b)))

(: placed->quasistream (-> placed-pict quasistream))
(define (placed->quasistream placed)
  (quasistream+
   placed
   (let loop : quasistream ([i 1])
     (define flip?
       (= 0 (modulo i 4)))
     (match-define (cons x y)
       (arch (exact->inexact i)))
     (define adjusted
       (adjust-origin placed x y))
     (quasistream+
      adjusted
      (if flip?
          (quasistream+ (rotate* adjusted)
                        (loop (add1 i)))
          (loop (add1 i)))))))


(define-predicate nonnegative-flonum? Nonnegative-Float)

(: picts->get-initial-position (-> (Listof pict)
                                   (-> placed-pict (values Float Float))))
(define (picts->get-initial-position picts)
  (let* ([a (total-area picts)]
         [w (area->width (assert a nonnegative-flonum?))]
         [h (/ a w)]
         [k (guard-random (* 0.3 w))]
         [vertical-k (guard-random (* 0.1 h))])
    (λ ([p : placed-pict])
      (values (real->double-flonum (random k))
              (real->double-flonum (random vertical-k))))))

(: total-area (-> (Listof pict) Float))
(define (total-area picts)
  (for/fold ([a 0.0])
            ([p (in-list picts)])
    (+ a
       (* (real->double-flonum (pict-width p))
          (real->double-flonum (pict-height p))))))

(: area->width (->* {Nonnegative-Float} {Positive-Float} Float))
(define (area->width a [ratio 1.0])
  (if (= 0 a)
      a
      (sqrt (/ a ratio))))

(: guard-random (-> Float Integer))
(define (guard-random x)
  (min 4294967087
       (floor (inexact->exact  x))))



(define terms
  '(("time" . 11600)
    ("history" . 10011)
    ("sense" . 9542)
    ("work" . 9193)
    ("way" . 8868)
    ("meaning" . 8489)
    ("language" . 8399)
    ("relations" . 8056)
    ("question" . 7434)
    ("philosophy" . 7195)
    ("action" . 6979)
    ("world" . 6894)
    ("terms" . 6767)
    ("narrative" . 6752)
    ("form" . 6717)
    ("texts" . 6423)
    ("point" . 6377)
    ("discourses" . 6294)
    ("word" . 6284)
    ("man" . 6259)
    ("died" . 6166)
    ("new" . 6136)
    ("problem" . 6115)
    ("theory" . 6046)
    ("human" . 6005)
    ("level" . 5977)
    ("order" . 5948)
    ("experience" . 5819)
    ("place" . 5785)
    ("idea" . 5752)
    ("interpretation" . 5723)
    ("spoke" . 5395)
    ("life" . 5368)
    ("thing" . 5300)
    ("acting" . 5289)
    ("concept" . 5245)
    ("means" . 5211)
    ("consciousness" . 5210)
    ("object" . 5000)
    ("analysis" . 4962)
    ("fact" . 4870)
    ("historical" . 4859)
    ("present" . 4718)
    ("self" . 4713)))



(scale-to-fit
 (let* ([counts (map (inst cdr String Positive-Integer) terms)]
        [mx (apply max counts)]
        [mn (apply min counts)]
        [rel-max (- mx mn)])
   (: count->font-size (-> Integer Integer))
   (define (count->font-size c)
     (+ 10 (floor (* (- c mn)
                     (/ 90 rel-max)))))
   (define picts
     (map (λ ([x : (Pairof String Integer)])
                      (match x
                        [`(,s . ,c)
                         (text s null (assert (count->font-size c) index?))]))
                    terms))
   (make-cloud picts
               #:get-initial-position
               (picts->get-initial-position picts)
               placed->quasistream))
 600
 600)








     
