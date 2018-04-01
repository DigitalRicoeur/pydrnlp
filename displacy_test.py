# -*- coding: utf-8 -*-

import spacy
from spacy import displacy
import os.path

nlp = spacy.load('en')

def getDoc():
    with open("/Users/philip/code/ricoeur/texts/plain-text/freud-and-philosophy.txt") as inf:
        return nlp(inf.read())


doc = getDoc()

print("Got",flush=True)

displacy.serve(doc,style="ent")

    
