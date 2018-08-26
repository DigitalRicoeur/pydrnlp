#!/usr/bin/env racket
#lang racket/base

(require racket/pretty racket/cmdline)

(command-line
 #:usage-help
 "writes the current environment variables to standard output"
 "  (as a `read`-able hash table)"
 #:args ()
 (define env
   (current-environment-variables))
 (pretty-write
  (for/hash ([name (environment-variables-names env)])
    (values name
            (environment-variables-ref env name)))))

