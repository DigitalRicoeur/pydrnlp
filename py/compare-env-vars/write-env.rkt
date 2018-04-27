#!/usr/bin/env racket
#lang racket/base

(require racket/pretty)

(define env
  (current-environment-variables))

(pretty-write
 (for/hash ([name (environment-variables-names env)])
   (values name
           (environment-variables-ref env name))))







