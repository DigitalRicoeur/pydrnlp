#lang racket

(require racket/runtime-path
         json
         "adt.rkt"
         "from-json.rkt"
         )

(module+ main
  (flatten (map parse-file
                json-paths)))

(define (parse-file pth)
  (call-with-input-file* pth
    (λ (in) (map jsexpr->modpath-doc
                 (read-json in)))))

(define-runtime-path-list json-paths
  (map (λ (x) (format "extracted/~a.json" x))
       '(annotations-v1
         restful
         suspiciousToken 
         drtoken
         segtokenize
         mkdoc
         stdio
         )))

(match-define (list json:annotations
                    json:restful
                    json:suspiciousToken 
                    json:drtoken
                    json:segtokenize
                    json:mkdoc
                    json:stdio)
  json-paths)

