# -*- coding: utf-8 -*-
"""Special annotations that support rendering in documentation.
"""

import inspect

## IMPORTANT NOTE: the order of definitions in this module
## is highly sensitive to bootstrapping issues.
## Please be careful when making changes.


class _AbstractSpecialAnnotation:
    """Abstract base class for special annotations.

    You probably shouldn't use this.
    See also isSpecialAnnotation.
    """
    # Concrete subclasses should implement _docSpecialAnnotation
    pass


#@NoDoc # Manually added to _doNotDoc for bootstrapping
class _BootstrappingLazyAnnotation(_AbstractSpecialAnnotation):
    """Like LazyAnnotation, but it has no annotations itself.

    Having some sort of lazy annotation early makes bootstrapping
    much easier, but to give it its own annotations creates
    dependencies on things that really need to use it.
    For example, there would be a cyclic dependency between
    LazyAnnotation and Any.
    This non-documented, internal-use-only class breaks the cycle.

    To ensure consistency, the real LazyAnnotation inherits from
    _BootstrappingLazyAnnotation.
    LazyAnnotation adds annotations to the methods, but defers to
    this class for the implementation.
    """
    def __init__(self, thunk):
        self.__thunk = thunk
        self.__forced = False
    def _docSpecialAnnotation(self):
        forced = self.__forced
        if forced:
            return forced[0]
        else:
            rslt = docAnnotation(self.__thunk())
            self.__forced = (rslt)
            return rslt    

        
## Any, Predicate, and NoDoc need to come here for bootstrapping

## @Predicate # manually added to _predicates for bootstrapping
def Any(any : _BootstrappingLazyAnnotation(lambda: Any)) -> bool:
    """Accepts all Python values.
    """
    return True

_predicates = { Any }

def Predicate(proc : Any) -> Any:
    """Declares that its argument is a predicate function.

    Functions registered with Predicate will be documented
    as annotations.
    Predicate functions that have not been registered with 
    Predicate can still be used as annotations in exactly the same
    way: the only effect of Predicate is to change the section
    in which the function will be documented.

    Predicate returns its argument unchanged for convienient
    use as a decorator.
    """
    _predicates.add(proc)
    return proc


def ispredicate(any : Any) -> bool:
    """Recognizes arguments which have been redistered with Predicate.
    """
    return (any in _predicates)

                       
_doNotDoc = { _BootstrappingLazyAnnotation }

def NoDoc(any : Any) -> Any:
    """Causes its argument not to be included in documentation.

    NoDoc returns its argument unchanged for convienient
    use as a decorator.
    """
    _doNotDoc.add(any)
    ## The following extra step probably isn't worth it:
    ## it would be very silly (and perhaps should be an error)
    ## to try to apply both Predicate and NoDoc to the same value.
    #if (any in __predicates):
    #    __predicates.remove(any)
    return any

def maybeShouldDoc(any : Any) -> bool:
    """Returns True unless the argument has been registered with NoDoc.

    A True result doesn't mean the argument should definitely be
    documented — it might not be a type of thing that should ever
    be documented, or it might not be from the right module — 
    but a False result definitively means the argument should never
    be documented: hence the name.
    """
    return (any not in _doNotDoc)







def isSpecialAnnotation(any : Any) -> bool:
    """Recognizes special annotation values.
    """
    return isinstance(any, _AbstractSpecialAnnotation)


class _AbstractAnnotationConstructor(_AbstractSpecialAnnotation):
    """Annotation constructors are subclasses of this class.
    """
    # Concrete subclasses must add name and modName methods
    def __init__(self, args : _BootstrappingLazyAnnotation(
            lambda: IteratorOf(Any))):
        self._args = args
    def _docSpecialAnnotation(self) -> _BootstrappingLazyAnnotation(
            lambda: _AnnotationConstructorDoc):
        return ["annotation-constructor",
                {"module": self.modName(), 
                 "name": self.name(),
                 "args": list(map(docAnnotation,self._args))}]


@NoDoc    
class _LocalAbstractAnnConst(_AbstractAnnotationConstructor):
    """For convienience, supplies a modName method for this module.
    """
    @staticmethod
    def modName() -> str:
        return __name__


class ThunkOf(_LocalAbstractAnnConst):
    """An annotation constructor for functions of no arguments
    where the result satisfies rslt.
    """
    # Needed by LazyAnnotation
    def __init__(self, rslt : Any):
        _AbstractAnnotationConstructor.__init__(self, [rslt])
    @staticmethod
    def name():
        return "ThunkOf"    


class LazyAnnotation(_BootstrappingLazyAnnotation):
    """A special annotation that delays the evaluation of its value.
    
    Useful for defining recursive annotations.

    A LazyAnnotation is completely transparent: it effectively
    replaces itself by the annotation value returned by the thunk.
    The thunk is guaranteed to be called at most once.
    """
    def __init__(self, thunk : ThunkOf(Any)):
        _BootstrappingLazyAnnotation.__init__(self, thunk)
    def _docSpecialAnnotation(self) -> _BootstrappingLazyAnnotation(
            lambda: AnnotationDoc):
        return _BootstrappingLazyAnnotation._docSpecialAnnotation(self)


class Or(_LocalAbstractAnnConst):
    """An annotation constructor for the union of the 
    args annotations.

    The Or annotation is satisfied if any of its args 
    annotations are satisfied.
    """
    def __init__(self, *args : Any):
        _AbstractAnnotationConstructor.__init__(self,args)
    @staticmethod
    def name():
        return "Or"


class And(_LocalAbstractAnnConst):
    """An annotation constructor for the intersection of the 
    args annotations.

    The And annotation is satisfied if and only if all of 
    its args annotations are satisfied.
    """
    def __init__(self, *args : Any):
        _AbstractAnnotationConstructor.__init__(self,args)
    @staticmethod
    def name():
        return "And"     
            

@NoDoc
class _Singleton:
    pass


_MaybeStr = Or(str, False)
        

class _NamedAnnotation(_AbstractSpecialAnnotation):
    """The private class used to implement named annotations.

    Use ModuleAnnotationNamer rather than using
    _NamedAnnotation directly.
    """
    def __init__(self,
                 modName : str,
                 name : str,
                 value : Any,
                 docstring : _MaybeStr = False):
        self.__modName = modName
        self.__name = name
        self._value = value
        self._docstring = docstring
    def modName(self) -> str:
        return self.__modName
    def name(self) -> str:
        return self.__name
    def _docSpecialAnnotation(self) -> LazyAnnotation(
            lambda: NamedAnnotationUseDoc):
        return ["named-annotation",
                {"module": self.modName(), 
                 "name": self.name(),
                 "string": str(self)}]
    def docSelf(self) -> LazyAnnotation(
            lambda: NamedAnnotationDefinitionInsideDoc):
        v = self._value
        docValue = (["singleton"]
                    if isinstance(v,_Singleton)
                    else docAnnotation(v))
        return {"name": self.name() ,
                "docstring": self._docstring,
                "value": docValue}


def isNamedAnnotation(any : Any) -> bool:
    """Recognizes named annotations.
    """
    return isinstance(any, _NamedAnnotation)
        

class ListOf(_LocalAbstractAnnConst):
    """An annotation constructor for lists whose elements
    satisfy the inner annotation.
    """
    def __init__(self, inner : Any):
        _AbstractAnnotationConstructor.__init__(self, [inner])
    @staticmethod        
    def name() -> str:
        return "ListOf"


class IteratorOf(_LocalAbstractAnnConst):
    """An annotation constructor for iterators whose elements
    satisfy the inner annotation.
    """
    def __init__(self, inner : Any):
        _AbstractAnnotationConstructor.__init__(self, [inner])
    @staticmethod
    def name():
        return "IteratorOf" 


class Not(_LocalAbstractAnnConst):
    """An annotation constructor that is satisfied if and only if
    its inner annotation is ƒb{not} satisfied.
    """
    def __init__(self, inner : Any):
        _AbstractAnnotationConstructor.__init__(self, [inner])
    @staticmethod
    def name():
        return "Not"  


class DictOf(_LocalAbstractAnnConst):
    """An annotation constructor for dictionaries where the keys
    match the key annotation and the values match the value annotation.
    """
    def __init__(self, key : Any , value : Any):
        _AbstractAnnotationConstructor.__init__(self, (key , value))
    @staticmethod
    def name():
        return "DictOf"   


class ModuleAnnotationNamer:
    """Create an instance of ModuleAnnotationNamer to construct
    named annotations for a particular module.

    This should generally be used in a declaration like:
    _NamedAnnotation = ModuleAnnotationNamer(__name__)

    TODO: explain __call__ vs singleton.
    """
    # would ModuleAnnotater be a better name for this?
    def __init__(self, modName : str):
        self.__modName = modName
    def __call__(self,
                 name : str,
                 value : Any,
                 docstring : _MaybeStr = False):
        return _NamedAnnotation(self.__modName, name, value, docstring)
    def singleton(self, name : str, docstring : _MaybeStr = False):
        return _NamedAnnotation(self.__modName,
                                name,
                                _Singleton(),
                                docstring)        


LocalNamedAnnotation = ModuleAnnotationNamer(__name__)  

_RecursiveAnnotationDoc = LazyAnnotation(lambda: AnnotationDoc)

NamedAnnotationDefinitionInsideDoc = LocalNamedAnnotation(
    "NamedAnnotationDefinitionDoc",
    {"name": str,
     "docstring": _MaybeStr,
     "value": Or(["singleton"], _RecursiveAnnotationDoc)})


_NamedAnnotationUseDoc = LocalNamedAnnotation(
    "_NamedAnnotationUseDoc",
    ["named-annotation", {"module": str, 
                          "name": str,
                          "string": str}])


_AnnotationConstructorDoc = LocalNamedAnnotation(
    "_AnnotationConstructorDoc",
    ["annotation-constructor", {"module": str, 
                                "name": str,
                                "args": ListOf(_RecursiveAnnotationDoc)}])


IsSpecialAnnotationClassResultDoc = Or(False,
                                       "annotation-constructor",
                                       "special-annotation")
_AISAnnotationClassResult = IsSpecialAnnotationClassResultDoc

def isSpecialAnnotationClass(cls : Any) -> _AISAnnotationClassResult:
    """Recognizes classes the instances of which are special annotations.

    Annotation constructor classes produce the more specific
    result "annotation-constructor".
    """
    if (not inspect.isclass(cls)):
        return False
    elif issubclass(cls, _AbstractAnnotationConstructor):
        return "annotation-constructor"
    elif issubclass(cls, _AbstractSpecialAnnotation):
        return "special-annotation"
    else:
        return False
    

def getModName(any : Any) -> Or(str,False):
    # where is this used
    mod = inspect.getmodule(any)
    if (mod is not None):
        return mod.__name__
    else:
        return False


_ClassAnnotationDoc = ["class" , {"module": str, 
                                  "name": str,
                                  "string": str}]
@NoDoc
def _docAnnotation_class(ann : inspect.isclass) -> _ClassAnnotationDoc:
    # _docAnnotation helper for classes to be near _ClassAnnotationDoc
    return ["class" , {"module": getModName(ann) , 
                       "name": ann.__name__ ,
                       "string": str(ann)}]


_FuncAnnotationDoc = ["function" , {"module": str, 
                                        "name": str}]
@NoDoc
def _docAnnotation_function(ann : inspect.isfunction) -> _FuncAnnotationDoc:
    # _docAnnotation helper for functions to be near _FuncAnnotationDoc
    return ["function" , {"module": getModName(ann) , 
                          "name": ann.__name__}]
    

_StructuralAnnotationDoc = [ Or("dict","list","tuple") ,
                             ListOf(_RecursiveAnnotationDoc) ]
_AtomicAnnotationDoc = Or(["True"],
                          ["False"],
                          ["None"],
                          ["string" , str],
                          ["number" , Or(int,float)],
                          ["other" , str])


AnnotationDoc = LocalNamedAnnotation(
    "AnnotationDoc",
    Or(_NamedAnnotationUseDoc,
       _AnnotationConstructorDoc,
       _ClassAnnotationDoc,
       _FuncAnnotationDoc,
       _StructuralAnnotationDoc,
       _AtomicAnnotationDoc))


def docAnnotation(ann : Any) -> AnnotationDoc:
    """Extracts structured, JSON-convertable documentation
    for an annotation.

    See also pydrnlp.mkdoc.
    """
    if isSpecialAnnotation(ann):
        return ann._docSpecialAnnotation()
    elif inspect.isclass(ann):
        return _docAnnotation_class(ann)
    elif inspect.isfunction(ann):
        return _docAnnotation_function(ann)
    elif isinstance(ann,dict):
        return ["dict" ,
               list([docAnnotation(k),docAnnotation(v)]
                    for k , v in ann.items())]
    elif isinstance(ann,list):
        return ["list" ,
               list(map(docAnnotation,ann))]
    elif isinstance(ann,tuple):
        return ["tuple" ,
                list(map(docAnnotation,ann))]
    elif (ann is True):
        return ["True"]
    elif (ann is False):
        return ["False"]
    elif (ann is None):
        return ["None"]
    elif isinstance(ann,str):
        return ["string" , ann]
    elif (isinstance(ann,int) or isinstance(ann,float)):
        return ["number" , ann]
    else:
        return ["other" , str(ann)]



