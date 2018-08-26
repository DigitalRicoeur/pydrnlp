#!/usr/bin/env racket
#lang racket/base

(require racket/pretty
         racket/cmdline)

(define (write-env-vars [out (current-output-port)])
  (define env
    (current-environment-variables))
  (pretty-write
   (for/hash ([name (in-list (environment-variables-names env))])
     (values name
             (environment-variables-ref env name)))
   out))

(define (run-file-symbol)
  (define-values {base name dir}
    (split-path (find-system-path 'run-file)))
  (string->symbol (path-element->string name)))

(define (check-path-string file)
  (unless (path-string? file)
    (raise-argument-error (run-file-symbol)
                          "path-string?"
                          file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main
  (let ([out-file? #f]
        [exists-flag 'error])
    (command-line
     #:usage-help
     "Writes the current environment variables as a `read`-able hash table"
     "  to the standard output (or to <file>, if given)."
     #:once-each
     [("--file") file
                 "Write to <file> instead of the standard output."
                 (check-path-string file)
                 (set! out-file? file)]
     [("--replace") ("Overwrite <file> if it already exists."
                     "(Ignored when writing to the standard output.)")
                    (set! exists-flag 'replace)]
     #:args ()
     (cond
       [out-file?
        (call-with-output-file* out-file?
          #:exists exists-flag
          write-env-vars)]
       [else
        (write-env-vars (current-output-port))]))))

