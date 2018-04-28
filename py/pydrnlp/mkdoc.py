# -*- coding: utf-8 -*-

import inspect
import pydoc
import sys
import json
import pydrnlp.annotations as an
from pydrnlp.annotations import Any, LazyAnnotation, NoDoc
from pydrnlp.annotations import Or, ListOf, IteratorOf


NamedAnnotation = an.ModuleAnnotationNamer(__name__)


def run() -> None:
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


ModpathDoc = [str, Or(False,
                      "ErrorDuringImport",
                      LazyAnnotation(lambda: ModuleDoc))]
        
    
def docModpath(modpath : str) -> ModpathDoc:
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


Module = inspect.ismodule
_RecursiveTextDoc = LazyAnnotation(lambda: TextDoc)
ModuleDoc = {"text": _RecursiveTextDoc,
             "functions":
             ListOf(LazyAnnotation(lambda: FunctionDefinitionDoc)),
             "named-annotations":
             ListOf(LazyAnnotation(lambda: NamedAnnotationDefinitionDoc)),
             "classes":
             ListOf(LazyAnnotation(lambda: ClassDefinitionDoc))}


def docMod(mod : Module) -> ModuleDoc:
    return {"text": getDocText(mod),
            "functions": list(docFunctions(mod)),
            "named-annotations": list(docNamedAnnotations(mod)),
            "classes": list(docClasses(mod))}


@NoDoc
def _andShouldDoc(pred):
    return (lambda x:
            (pred(x) and
             an.maybeShouldDoc(x)))

@NoDoc
def _getMembersResultOf(value : Any) -> an.isSpecialAnnotation:
    return an.DictOf(str, value)

@NoDoc
def getmembersFromHere(mod : Module , pred) -> _getMembersResultOf(
        # would be nicer to show that the vals satisfy pred
        Any):
    return inspect.getmembers(mod,
                              _andShouldDoc(
                                  lambda x:
                                  (pred(x) and
                                   (mod ==  inspect.getmodule(x)))))


def getNamedAnnotations(mod : Module) -> _getMembersResultOf(
        an.isNamedAnnotation):
    modName = mod.__name__
    return inspect.getmembers(mod,
                              _andShouldDoc(
                                  lambda x:
                                  (an.isNamedAnnotation(x) and
                                   (modName == x.modName()))))

NamedAnnotationDefinitionDoc = {"name": str,
                                "module": str,
                                "value": an.NamedAnnotationDefinitionInsideDoc}

def docNamedAnnotations(mod : Module) -> IteratorOf(
        NamedAnnotationDefinitionDoc):
    for (name , value) in getNamedAnnotations(mod):
        yield {"name": name,
               "module": value.modName(),
               "value": value.docSelf()}

_RecursiveSignatureDoc = an.LazyAnnotation(lambda: SignatureDoc)
ClassDefinitionDoc = {"name": str,
                      "signature": _RecursiveSignatureDoc,
                      "isSpecialAnnotationClass":
                      an.IsSpecialAnnotationClassResultDoc,
                      "text": _RecursiveTextDoc}
        
def docClasses(mod : Module) -> IteratorOf(ClassDefinitionDoc):
    for (name , value) in getmembersFromHere(mod,inspect.isclass):
        yield {"name": name,
               "signature": docSignature(value),
               "isSpecialAnnotationClass":
               an.isSpecialAnnotationClass(value),
               "text": getDocText(value)}


FunctionDefinitionDoc = {"name": str,
                         "signature": _RecursiveSignatureDoc,
                         "ispredicate": bool,
                         "text": _RecursiveTextDoc}
        
def docFunctions(mod : Module) -> IteratorOf(FunctionDefinitionDoc):
    for (name , value) in getmembersFromHere(mod, inspect.isfunction):
        yield {"name": name,
               "signature": docSignature(value),
               "ispredicate": an.ispredicate(value),
               "text": getDocText(value)}

TextDoc = Or(False,
             ["comments", str],
             ["docstring", str])
        
def getDocText(obj : Any) -> TextDoc:
    docstr = inspect.getdoc(obj)
    if ((docstr is None) or (docstr == "")):
        cmts = inspect.getcomments(obj)
        if ((cmts is None) or (cmts == "")):
            return False
        else:
            return ["comments", cmts]
    else:
        return ["docstring", docstr]

_MaybeAnnotationDoc = Or(False, an.AnnotationDoc)
SignatureDoc = Or(
    {"parameters": an.ListOf(LazyAnnotation(lambda: ParameterDoc)),
     "return": _MaybeAnnotationDoc},
    "ValueError",
    "TypeError")    


def docSignature(proc : Any) -> SignatureDoc:
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

ParameterKindDoc = Or("POSITIONAL_ONLY",
                      "POSITIONAL_OR_KEYWORD",
                      "VAR_POSITIONAL",
                      "KEYWORD_ONLY",
                      "VAR_KEYWORD",
                      False)
ParameterDoc = {"name": str,    
                "annotation": _MaybeAnnotationDoc,
                "kind": ParameterKindDoc,
                "default": Or(False, str)}


def docParameters(params : inspect.Parameter) -> IteratorOf(
        ParameterDoc):
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
            return an.docAnnotation(it)
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


def docReturn(return_ann : Any) -> _MaybeAnnotationDoc:
    if (return_ann == inspect.Signature.empty):
        return False
    else:
        return an.docAnnotation(return_ann)


JSON = NamedAnnotation.singleton(
    "JSON",
    """An annotation for the kinds of values handled by the json module.
    """)
    
    
def dumpln(jsOut : JSON) -> None:
    json.dump(jsOut,sys.stdout)
    sys.stdout.write("\n")


## The implementation is based heavily on
## https://medium.com/python-pandemonium/
##     python-introspection-with-the-inspect-module-2c85d5aa5a48


if __name__ == "__main__":
    #print(sys.argv)
    run()
    
