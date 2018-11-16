"""Tools for extracting Python documentation to structured JSON.

While this module exports certain public functions,
it is primarily intended for programatic use with `__name__=="__main__"`.

All non-trivial JSON values produced by this module are
designed as type-tagged objects: that is, each is a two-element
array, with the first element being a string identifying the type
of the value and the second element being a JSON object
containing the value's payload.
"""
import inspect
import pydoc
import sys
import json

## The implementation is based heavily on
## https://medium.com/python-pandemonium/
##     python-introspection-with-the-inspect-module-2c85d5aa5a48
## as well as the implementation of pydoc.
## The design is also inspired by pdoc.

def docModpath(modpath):
    """Extracts documentation from the module at the given modpath.

    The resulting object is tagged `"modpath"`.
    The value of its `"modpath"` key is always the given modpath.
    The value of its `"module"` key will usually be the
    same kind of value produced by `docModule`, but it might be
    `False`, meaning that the module was not found, or 
    `"ErrorDuringImport"`, meaning that the module was found, 
    but an exception occured while trying to run it to 
    obtain a module object.
    """
    def modpathDoc(mod):
        return ["modpath",{"modpath":modpath,"module":mod}]
    try:
        # pydoc.safeimport might return None or
        # raise an ErrorDuringImport exception
        mod = pydoc.safeimport(modpath)
        if mod is None:
            # module not found
            return modpathDoc(False)
        else:
            return modpathDoc(docModule(mod))
    except pydoc.ErrorDuringImport as e:
        # better to describe the error message
        return modpathDoc("ErrorDuringImport")


def getDocText(obj):
    """Attempts to get some text documenting obj.

    The result is based on what was obtained:

    - If there was a docstring, it is tagged `"docstring"`.

    - If comments were found, they are tagged `"comments"`.

    - If both of those are missing or empty, returns `False`.
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

    
def docModule(mod):
    """Extracts the documentation for a given module object.

    The resulting object is tagged `"module"`.
    It has the following keys:

    - `"text"`: the result of `getDocText(mod)`.

    - `"functions"`: a list of results from `docFunction`.
    """
    def _docMembers(pred, docIt):
        for (name , value) in _getPublicMembersFromHere(mod,pred):
            yield docIt(name, value)
    return ["module",
            {"text": getDocText(mod),
             "classes": list(_docMembers(inspect.isclass,docClass)),
             "functions": list(_docMembers(inspect.isfunction,docFunction))}]   

def _nameIsPrivate(name):
    return (("" == name) 
            or (("_" == name[0])
                and not (name.startswith('__') and name.endswith('__'))))


def _getPublicMembersFromHere(mod, pred):
    # pred should be either inspect.isfunction or inspect.isclass:
    # note that inspect.getmodule doesn't work on arbitrary values
    def pred2(x):
        return pred(x) and (mod ==  inspect.getmodule(x))
    for (name , value) in inspect.getmembers(mod,pred2):
        if not _nameIsPrivate(name):
            yield (name , value)


def docClass(name, value):
    """Documents the class `value` as `name`.

    The resulting object is tagged `"class"` and has 
    the following keys:

    - `"name"`: `name`.

    - `"text"`: the result of `getDocText(value)`.

    - `"bases"`: a list of base classes, where each item is tagged `"base"`
      and has fields `"name"` and `"module"`, both of which are mapped to
      strings.

    - `"methods"`: a list of function documentation objects
      (see `docFunction`) for the class's public methods, including `__init__`.
    """
    def docMethods(value):
        for (name, value) in inspect.getmembers(value,inspect.isfunction):
            if not _nameIsPrivate(name):
                yield docFunction(name, value)
    def docBases(value):
        for c in value.__bases__:
            (n,m) = (c.__name__,c.__module__)
            if not (("object" == n) and ("builtins" == m)):
                yield ["base", {"name":n, "module":m}]
    return ["class",
            {"name": name,
             "text": getDocText(value),
             "bases": list(docBases(value)),
             "methods": list(docMethods(value))}]


def docFunction(name, value):
    """Documents the function `value` as `name`.

    The resulting object is tagged `"function"`.
    It has the following keys:

    - `"name"`: `name`.

    - `"text"`: the result of `getDocText(value)`.

    - `"signature"`: the result of `docSignature(value)`.
    """
    return ["function",
            {"name":name,
             "text":getDocText(value),
             "signature": docSignature(value)}]


def docSignature(proc):
    """Tries to extract documentation about the signature
    of the given object.

    The result may be `"TypeError"` if the given object
    does not support signatures or `"ValueError"` if it does,
    but no signature could be found.
    Otherwise, it will be an object tagged `"signature"`
    with the following fields:

    - `"parameters"`: a list of objects tagged `"parameter"` (see below).

    - `"return"`: either a string representing the function's return
      annotation or `False` if the function had none.

    A parameter object has the following fields:

    - `"name"`: a string naming the formal argument.

    - `"kind"`: one of `"POSITIONAL_ONLY"`, `"POSITIONAL_OR_KEYWORD"`,
      `"VAR_POSITIONAL"`, `"KEYWORD_ONLY"`, or `"VAR_KEYWORD"`.

    - `"annotation"`: a string representing the argument's annotation,
      or `False` if it has none.

    - `"default"`: a string representing the argument's default value,
      or `False` if it has none.
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
    def docReturn(sig):
        ann = sig.return_annotation
        if (ann == inspect.Signature.empty):
            return False
        else:
            return str(it)
    ok , sig = tryGetSig()
    if ok:
        return ["signature",
                {"parameters": list(_docParameters(sig)),
                 "return": docReturn(sig)}]
    else:
        return sig


def _docParameters(sig):
    def stringify(it):
        if (it == inspect.Parameter.empty):
            return False
        else:
            return str(it)
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
            raise ValueError("unknown parameter kind\n" +
                             "  given:" + repr(k))
    for name, p in sig.parameters.items():
        yield ["parameter",
               {"name": name,
                "kind": getKind(p),
                "annotation": stringify(p.annotation),
                "default": stringify(p.default)}]


if __name__ == "__main__":
    def show_usage_help():
        sys.stdout.write("TODO: write a usage help message.\n")
        pass
    if len(sys.argv) != 2:
        show_usage_help()
        exit(1)
    elif ((sys.argv[1] == "-h") or
          (sys.argv[1] == "--help")):
        show_usage_help()
        exit(0)
    else:
        json.dump(list(map(docModpath,json.loads(sys.argv[1]))),
                  sys.stdout)
        sys.stdout.write("\n")
        exit(0)

