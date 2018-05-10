#lang typed/racket

(require typed/pict
         typed/racket/draw
         )

(provide terms
         term-pict
         example-picts
         )

(: terms (Listof (Pairof String Positive-Integer)))
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

(: term-pict (-> String Integer pict))
(define (term-pict s size)
  (text s
        (cons 'combine
              (send the-font-list
                    find-or-create-font
                    size
                    "Plantin MT Std"
                    'roman
                    'normal
                    'normal
                    #f ;no underline
                    'smoothed
                    #t ;size in pixels
                    'unaligned)) ; to be scalable
        12)) ;ignored with a font%



(define example-picts
  (let* ([counts (map (inst cdr String Positive-Integer)
                      terms)]
         [mx (apply max counts)]
         [mn (apply min counts)]
         [rel-max (- mx mn)])
    (define (count->font-size [c : Integer])
      (+ 10 (floor (* (- c mn)
                      (/ 90 rel-max)))))
    (map (λ ([pr : (Pairof String Positive-Integer)])
           (match pr
             [`(,s . ,c)
              (term-pict s (count->font-size c))]))
         terms)))



