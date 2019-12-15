import en_core_web_md

srcs = ["An historian employs most of these words at one time or another.",
        "Our first task is to understand our own times.",
        "Time is therefore that mediating order.",
        "Times are changing."]

nlp = en_core_web_md.load()

#for s in srcs:
#    print(nlp.tokenizer.explain(s))

rslts = [[{"lemma": t.lemma_, "norm": t.norm_, "text": t.text, "tag": t.tag_}
            for t in doc if "time" == t.norm_[0:4]]
         for doc in nlp.pipe(srcs)]

if __name__ == "__main__":
    import sys
    import json
    json.dump(rslts, sys.stdout, indent=1, sort_keys=True)

#Fallible Man p 42
#'Time is therefore that mediating order, homogeneous both with the sensible whose very style of dispersion and distention it is, and with the intelligible for which it is the condition of intuition since it lends itself to that intelligible determination that we call "series."'
