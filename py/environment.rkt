#!/usr/bin/env racket
#lang racket/base

(provide spacy-revision
         model-revisions
         environment-jsexpr)

;; The conda environment should generally be managed by raco setup.
;; For manual use:
;;   conda env create --file environment.yml --prefix condaenv
;;   conda env update --file environment.yml --prefix condaenv
;;   conda activate condaenv/
;;   conda deactivate

(define spacy-revision
  "2.2.1")

(define model-revisions
  #hasheq([en_core_web_md . "2.2.0"]
          [fr_core_news_sm . "2.2.0"]
          [de_core_news_sm . "2.2.0"]))

(define (string-append/intern . args)
  (datum-intern-literal (apply string-append args)))

(define (model->package-source name version)
  (define spec
    (string-append (symbol->string name) "-" version))
  (string-append/intern
   "https://github.com/explosion/spacy-models/releases/download/"
   spec "/" spec ".tar.gz#egg=" spec))

;; for elbow: scipy>=1.3.1 numpy>=1.16.4
;; for elbow demo (don't want graphics in production): matplotlib

(define environment-jsexpr
  `#hasheq([name . "pydrnlp"]
           [channels . ("defaults"
                        "conda-forge")] ;; for spacy
           [dependencies
            . ("python=3.7"
               ,(string-append/intern "spacy=" spacy-revision)
               "srsly>=0.1.0"
               "regex>=2019.03.12"
               "pip>=19.0.3"
               #hasheq([pip . ,(hash-map model-revisions
                                         model->package-source
                                         'try-order)]))]))

                         
           
