#lang racket

(require racket/runtime-path
         openssl/md5
         setup/path-to-relative
         adjutor
         )

(provide py-dir
         python3
         conda-environment-variables
         installer
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

(define conda-env
  (environment-variables-set*
   init-env
   #"CONDA_DEFAULT_ENV" (path->bytes condaenv-dir)
   #"CONDA_PREFIX" (path->bytes condaenv-dir)
   #"CONDA_EXE" (path->bytes conda)
   #"CONDA_PYTHON_EXE" (if conda-python-exe
                           (path->bytes conda-python-exe)
                           (environment-variables-ref init-env
                                                      #"CONDA_PYTHON_EXE"))
   #"CONDA_SHLVL" #"1"
   #"PATH" (bytes-append (path->bytes condaenv-bin)
                         #":"
                         (or (environment-variables-ref init-env #"PATH")
                             #""))
   ;; see http://click.pocoo.org/5/python3/#python-3-surrogate-handling
   ;; Apparently Mac OS doesn't set locale for GUI programs.
   #"LANG" (or (environment-variables-ref init-env #"LANG")
               #"en_US.UTF-8")))

(define python3
  (parameterize ([current-environment-variables conda-env])
    (find-executable-path "python3")))

(define (conda-environment-variables)
  (environment-variables-copy conda-env))

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

(define (check-conda)
  (printf "Checking conda configuration ...\n")
  (unless conda
    (error 'pydrnlp
           "no conda executable found"))
  (unless conda-python-exe
    (match-define-values {dir _ _}
      (split-path conda))
    (eprintf "~a;\n ~a\n  conda \"/bin/\": ~e\n"
             "WARNING: pydrnlp: no \"python\" executable in conda \"/bin/\""
             "check your \"CONDA_PYTHON_EXE\" environment variable"
             dir))
  (update-conda-environment))

(define (installer a b c d)
  (check-conda))



             