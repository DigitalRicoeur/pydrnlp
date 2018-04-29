# -*- coding: utf-8 -*-
"""Library for extracting Python documentation and rendering to JSON.

The functions in this library extract documentation from
Python modules and render it to a structured JSON format.
It primarily relies on docstrings and annotations to build
the documentation, including the special annotations from
pydrnlp.annotations.

Documentation is currently extracted for functions, classes,
and named annotations, as well as the module as a whole.

The primary entry point to the library is docModpath(),
though docMod() is also a viable choice.
The interactive command-line behavior is encapsulated in run().

The resulting JSON data should be convertable to a Scribble document.

TODO: Need to determine if running this module as __main__
causes bootstrapping issues for its named annotations etc.
"""

import inspect
import pydoc
import sys
import json
import pydrnlp.annotations as an
from pydrnlp.annotations import Any, LazyAnnotation, NoDoc
from pydrnlp.annotations import Or, ListOf, IteratorOf, DictOf


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
    """Extracts documentation from the module at the given modpath.

    The first element of the ModpathDoc is always the given modpath.
    The second element might be False, meaning that the module
    was not found, or "ErrorDuringImport", meaning that the module
    was found, but an exception occured while trying to run it to 
    obtain a module object.
    """
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
    """Like docModpath, but operates on a module object.
    """
    return {"text": getDocText(mod),
            "functions": list(_docFunctions(mod)),
            "named-annotations": list(_docNamedAnnotations(mod)),
            "classes": list(_docClasses(mod))}


@NoDoc
def _andShouldDoc(pred):
    """Composes pred with a check to respect @NoDoc"""
    return (lambda x:
            (pred(x) and
             an.maybeShouldDoc(x)))

@NoDoc
def _getMembersResultOf(value : Any) -> an.isSpecialAnnotation:
    return an.DictOf(str, value)

@NoDoc
def _getmembersFromHere(mod : Module , pred) -> _getMembersResultOf(
        # would be nicer to show that the vals satisfy pred
        Any):
    # pred should be either inspect.isfunction or inspect.isclass:
    # note that inspect.getmodule doesn't work on arbitrary values
    return inspect.getmembers(mod,
                              _andShouldDoc(
                                  lambda x:
                                  (pred(x) and
                                   (mod ==  inspect.getmodule(x)))))

def _getFunctions(mod : Module) -> _getMembersResultOf(
        inspect.isfunction):
    """Extracts the members of mod that are functions.

    The raw listing is filtered to eliminate values 
    imported from other modules and to respect @NoDoc.
    """
    return _getmembersFromHere(mod, inspect.isfunction)

def _getClasses(mod : Module) -> _getMembersResultOf(
        inspect.isclass):
    """Like _getFunctions, but for classes"""
    return _getmembersFromHere(mod, inspect.isclass)

def _getNamedAnnotations(mod : Module) -> _getMembersResultOf(
        an.isNamedAnnotation):
    """Like _getFunctions, but for named annotations
    (which require a fairly different implementation).
    """
    modName = mod.__name__
    return inspect.getmembers(mod,
                              _andShouldDoc(
                                  lambda x:
                                  (an.isNamedAnnotation(x) and
                                   (modName == x.modName()))))

NamedAnnotationDefinitionDoc = {"name": str,
                                "module": str,
                                "value": an.NamedAnnotationDefinitionInsideDoc}

def _docNamedAnnotations(mod : Module) -> IteratorOf(
        NamedAnnotationDefinitionDoc):
    """Extracts documentation for the named annotations from mod.
    
    The documented values are filtered as with _getNamedAnnotations.
    """
    for (name , value) in _getNamedAnnotations(mod):
        yield {"name": name,
               "module": value.modName(),
               "value": value.docSelf()}

_RecursiveSignatureDoc = an.LazyAnnotation(lambda: SignatureDoc)
ClassDefinitionDoc = {"name": str,
                      "signature": _RecursiveSignatureDoc,
                      "isSpecialAnnotationClass":
                      an.IsSpecialAnnotationClassResultDoc,
                      "text": _RecursiveTextDoc}
        
def _docClasses(mod : Module) -> IteratorOf(ClassDefinitionDoc):
    """Like _docNamedAnnotations, but for classes."""
    for (name , value) in _getClasses(mod):
        yield {"name": name,
               "signature": docSignature(value),
               "isSpecialAnnotationClass":
               an.isSpecialAnnotationClass(value),
               "text": getDocText(value)}


FunctionDefinitionDoc = {"name": str,
                         "signature": _RecursiveSignatureDoc,
                         "ispredicate": bool,
                         "text": _RecursiveTextDoc}
        
def _docFunctions(mod : Module) -> IteratorOf(FunctionDefinitionDoc):
    """Like _docNamedAnnotations, but for functions."""
    for (name , value) in _getFunctions(mod):
        yield {"name": name,
               "signature": docSignature(value),
               "ispredicate": an.ispredicate(value),
               "text": getDocText(value)}

TextDoc = Or(False,
             ["comments", str],
             ["docstring", str])
        
def getDocText(obj : Any) -> TextDoc:
    """Attempts to get some text documenting obj.

    The result is tagged based on what was obtained:
    getDocText() first tries to get a docstring,
    then tries to extract source-code comments near
    the definition of the object.
    If both options fail or return empty strings,
    returns False.
    """
    def resultIsEmpty(x):
        return ((x is None) or
                (x == "") or
                x.isspace())
    docstr = inspect.getdoc(obj)
    if resultIsEmpty(docstr):
        cmts = inspect.getcomments(obj)
        if resultIsEmpty(cmts):
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
    """Tries to extract documentation about the signature
    of the given object.

    The result may be "TypeError" if the given object
    does not support signatures or "ValueError" if it does,
    but no signature could be found.
    Otherwise, the dictionary gives information about the 
    parameters and return annotation.
    The return annotation may be False to indicate that no
    annotation was provided: otherwise, it will be a 
    pydrnlp.annotations.AnnotationDoc.
    """
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
                "return": _docReturn(sig.return_annotation)}
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


def docParameters(params : DictOf(str, inspect.Parameter)) -> IteratorOf(
        ParameterDoc):
    """Extracts documentation for parameters from a signature object.

    The "annotation" field may be False if no annotation was provided:
    otherwise, it will be a pydrnlp.annotations.AnnotationDoc.

    The "default" field will be False if there is no default
    value for that argument: otherwise, it is the default
    value converted to string form.

    The "kind" field will only be False if the raw kind value is
    unknown, which should likely trigger an exception in higher-level
    code. (Currently, this is deferred to the Racket side.)
    """
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


def _docReturn(return_ann : Any) -> _MaybeAnnotationDoc:
    """Used to implement docSignature"""
    if (return_ann == inspect.Signature.empty):
        return False
    else:
        return an.docAnnotation(return_ann)


JSON = NamedAnnotation.singleton(
    "JSON",
    """An annotation values that can be converted to or from
    JSON by the json module.
    """)
    
    
def dumpln(jsOut : JSON) -> None:
    """Writes jsOut to standard output as JSON, 
    followed by a newline.
    """
    json.dump(jsOut,sys.stdout)
    sys.stdout.write("\n")


## The implementation is based heavily on
## https://medium.com/python-pandemonium/
##     python-introspection-with-the-inspect-module-2c85d5aa5a48


if __name__ == "__main__":
    #print(sys.argv)
    run()
    
