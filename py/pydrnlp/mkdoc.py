# -*- coding: utf-8 -*-

import inspect
import pydoc
import sys
import json
import pydrnlp.contract as cntct


def run():
    """Implements the command-line interface.

    Called when ƒpyflow{__name__ == "__main__"}.
    """
    def docFromArgs():
        return list(map(docModpath,sys.argv[2::]))
    if len(sys.argv) <= 1:
        print(docModpath("pydrnlp.drtoken"))
    elif sys.argv[1] == "--print":
        print(docFromArgs())
    elif sys.argv[1] == "--json":
        dumpln(docFromArgs())
    elif ((sys.argv[1] == "-") or
          (sys.argv[1] == "--stdin")):
        jsIn = json.load(sys.stdin)
        dumpln(map(docModpath,jsIn))
    elif ((sys.argv[1] == "-h") or
          (sys.argv[1] == "--help")):
        # TODO: show usage
        exit(0)
    else:
        # TODO: show usage
        exit(1)

    
def docModpath(modpath : str):
    try:
        # pydoc.safeimport might return None or
        # raise an ErrorDuringImport exception
        mod = pydoc.safeimport(modpath)
        if mod is None:
            # module not found
            return [modpath , False]
        else:
            return [modpath , docMod(mod)]
    except pydoc.ErrorDuringImport as e:
        # better to describe the error message
        return [modpath , "ErrorDuringImport"]

    
def docMod(mod : inspect.ismodule):
    return {"functions": list(docFunctions(mod)),
            "contracts": list(docContracts(mod)),
            "classes": list(docClasses(mod))}


def getmembersFromHere(mod,pred):
    def andFromHere(mod,pred):
        return (lambda x:
                (pred(x) and
                 (mod ==  inspect.getmodule(x))))
    return inspect.getmembers(mod,andFromHere(mod,pred))


def iscontractFrom(mod):
    modName = mod.__name__
    return (lambda x: (cntct.iscontract(x) and
                       (modName == x.modName())))


def docContracts(mod):
    for (name , value) in inspect.getmembers(mod, iscontractFrom(mod)):
        yield {"name": name,
               "module": cntct.getModName(value),
               "value": value.docSelf()}

        
def docClasses(mod):
    for (name , value) in getmembersFromHere(mod,inspect.isclass):
        yield {"name": name,
               "signature": docSignature(value),
               "isContractClass?": cntct.isContractClass(value),
               "text": getDocText(value)}

        
def docFunctions(mod):
    for (name , value) in getmembersFromHere(mod, inspect.isfunction):
        yield {"name": name,
               "signature": docSignature(value),
               "isPredicate?": cntct.ispredicate(value),
               "text": getDocText(value)}

        
def getDocText(obj):
    docstr = inspect.getdoc(obj)
    if ((docstr is None) or (docstr == "")):
        cmts = inspect.getcomments(obj)
        if ((cmts is None) or (cmts == "")):
            return False
        else:
            return ["comments", cmts]
    else:
        return ["docstring", docstr]


def docSignature(proc):
    def tryGetSig():
        try:
            return (True , inspect.signature(proc))
        except ValueError:
            # if no signature can be provided
            return (False , "ValueError")
        except TypeError:
            # if that type of object is not supported
            return (False , "TypeError")
    ok , sig = tryGetSig()
    if ok:
        return {"parameters": list(docParameters(sig.parameters)),
                "return": docReturn(sig.return_annotation)}
    else:
        return sig
    

def docParameters(params):
    #print(params)
    def getDefault(p):
        it = p.default
        if (it == inspect.Parameter.empty):
            return False
        else:
            return str(it)
    def getAnnotation(p):
        it = p.annotation
        if (it == inspect.Parameter.empty):
            return False
        else:
            return cntct.docAnnotation(it)
    def getKind(p):
        k = p.kind
        if (k == p.POSITIONAL_ONLY):
            return "POSITIONAL_ONLY"
        elif (k == p.POSITIONAL_OR_KEYWORD):
            return "POSITIONAL_OR_KEYWORD"
        elif (k == p.VAR_POSITIONAL):
            return "VAR_POSITIONAL"
        elif (k == p.KEYWORD_ONLY):
            return "KEYWORD_ONLY"
        elif (k == p.VAR_KEYWORD):
            return "VAR_KEYWORD"
        else:
            return False
    for name , p in params.items():
        #print(p)
        yield {"name": name,
               "annotation": getAnnotation(p),
               "kind": getKind(p),
               "default": getDefault(p)}


def docReturn(return_ann):
    if (return_ann == inspect.Signature.empty):
        return False
    else:
        return cntct.docAnnotation(return_ann)

    
def dumpln(jsOut):
    json.dump(jsOut,sys.stdout)
    sys.stdout.write("\n")


## The implementation is based heavily on
## https://medium.com/python-pandemonium/
##     python-introspection-with-the-inspect-module-2c85d5aa5a48


if __name__ == "__main__":
    #print(sys.argv)
    run()
    
