#lang racket

(require racket/runtime-path
         openssl/md5
         setup/path-to-relative
         )

(define init-env
  (current-environment-variables))

(define-runtime-path py-dir
  "py/")

(define-runtime-path environment-md5-file
  ".environment.yaml.md5")

(define environment.yml
  (build-path py-dir "environment.yml"))

(define condaenv-dir
  (build-path py-dir "condaenv/"))

(define condaenv-bin
  (build-path condaenv-dir "bin/"))

(define conda
  (find-executable-path "conda"))

(define conda-python-exe
  (and conda
       (let ()
         (match-define-values {dir _ _}
           (split-path conda))
         (define pth
           (build-path dir "python"))
         (and (or (file-exists? pth)
                  (link-exists? pth))
              pth))))

(define (update-conda-environment)
  (parameterize ([current-directory py-dir]
                 [current-input-port (open-input-string "")])
    (cond
      [(not (directory-exists? condaenv-dir))
       (print-notification)
       (system* conda #"env" #"create"
                #"--file" environment.yml
                #"--prefix" condaenv-dir)
       (save-environment-file-md5!)]
      [(not (environment-up-to-date?))
       (print-notification "Updating")
       (system* conda #"env" #"update"
                #"--file" environment.yml
                #"--prefix" condaenv-dir)
       (save-environment-file-md5!)]
      [else
       (void)])))

(define (current-environment-file-md5)
  (call-with-input-file environment.yml
    md5))

(define (environment-up-to-date?)
  (and (file-exists? environment-md5-file)
       (equal? (file->string environment-md5-file)
               (current-environment-file-md5))))
  
(define (save-environment-file-md5!)
  (call-with-output-file environment-md5-file
    #:exists 'truncate/replace
    (λ (out)
      (void
       (write-string (current-environment-file-md5)
                     out)))))

(define (print-notification [verb "Creating"])
  (define cache
    (make-hash))
  (printf "~a conda environment\n  at: ~a\n  based on: ~a\n"
          verb
          (path->relative-string/library condaenv-dir #:cache cache)
          (path->relative-string/library environment.yml #:cache cache)))
