# -*- coding: utf-8 -*-
"""
"""

import pydrnlp.jsonio
import pydrnlp.language.get as 

def start_worker(revision, proc):
    def handle(jsIn):
        {lang: proc(pydrnlp.language.get(lang), segs)
         for lang, segs in jsIn.items()}
    def handle_lang(lang, segs):
        nlp = pydrnlp.language.get(lang)
        [{"key", 
        
    write_json_line(revision())
    map_json_lines(lambda jsIn: {lang: handle_lang(lang, segs)
                                 for lang, segs in jsIn.items()})

