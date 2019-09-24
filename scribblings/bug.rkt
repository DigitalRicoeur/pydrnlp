#lang racket

(require markdown
         markdown/scrib)

(xexprs->scribble-pres
 (parse-markdown "..."))

(xexprs->scribble-pres
   '[(pre () (code () "- punctuation;\n- whitespace;\n- stop words (see `pydrnlp.tokenAnyIsStopForLanguage`); and\n- tokens which have a \"boring\" part-of-speech tag."))])