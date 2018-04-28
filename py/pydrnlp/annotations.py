# -*- coding: utf-8 -*-
"""Special annotations that support rendering in documentation.
"""

import inspect

## IMPORTANT NOTE: the order of definitions in this module
## is highly sensitive to bootstrapping issues.
## Please be careful when making changes.


## Any, Predicate, and NoDoc need to come first for bootstrapping

## @Predicate # manually added to __predicates for bootstrapping
def Any(any) -> bool:
    """Accepts all Python values.
    
    (Unfortunately the argument cannot be annotated with Any
    due to bootstrapping issues.)
    """
    return True

__predicates = { Any }

def Predicate(proc):
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
    __predicates.add(proc)
    return proc


def ispredicate(any : Any) -> bool:
    """Recognizes arguments which have been redistered with Predicate.
    """
    return (any in __predicates)

                       
__doNotDoc = set()

def NoDoc(any : Any) -> Any:
    """Causes its argument not to be included in documentation.

    NoDoc returns its argument unchanged for convienient
    use as a decorator.
    """
    __doNotDoc.add(any)
    if (any in __predicates):
        __predicates.remove(any)
    return any

def maybeShouldDoc(any : Any) -> bool:
    """Returns True unless the argument has been registered with NoDoc.
    """
    return (any not in __doNotDoc)











class _AbstractSpecialAnnotation:
    # Concrete subclasses should implement _docSpecialAnnotation
    pass

    
@NoDoc
class _BootstrappingLazyAnnotation:
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


def isSpecialAnnotation(any : Any) -> bool:
    """Recognizes special annotation values.
    """
    return (isinstance(any, _AbstractSpecialAnnotation) or
            isinstance(any, _BootstrappingLazyAnnotation))


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
    def __init__(self, rslt : Any):
        _AbstractAnnotationConstructor.__init__(self, [rslt])
    @staticmethod
    def name():
        return "ThunkOf"    


class LazyAnnotation(_BootstrappingLazyAnnotation,
                     _AbstractSpecialAnnotation):
    def __init__(self, thunk : ThunkOf(Any)):
        _BootstrappingLazyAnnotation.__init__(self, thunk)
    def _docSpecialAnnotation(self) -> _BootstrappingLazyAnnotation(
            lambda: AnnotationDoc):
        return _BootstrappingLazyAnnotation._docSpecialAnnotation(self)


class Or(_LocalAbstractAnnConst):
    def __init__(self, *args : Any):
        _AbstractAnnotationConstructor.__init__(self,args)
    @staticmethod
    def name():
        return "Or"


class And(_LocalAbstractAnnConst):
    def __init__(self, *args : Any):
        _AbstractAnnotationConstructor.__init__(self,args)
    @staticmethod
    def name():
        return "And"     
            

@NoDoc
class _Singleton:
    def __init__(self,docstring : Or(False, str) ):
        self._docstring = docstring


class _NamedAnnotation(_AbstractSpecialAnnotation):
    def __init__(self, modName : str, name : str, value : Any):
        self.__modName = modName
        self.__name = name
        self._value = value
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
        docValue = (["singleton", v._docstring]
                    if isinstance(v,_Singleton)
                    else docAnnotation(v))
        return [self.name() , docValue]


def isNamedAnnotation(any : Any) -> bool:
    """Recognizes named annotations.
    """
    return isinstance(any, _NamedAnnotation)
        

class ListOf(_LocalAbstractAnnConst):
    def __init__(self,inner : Any):
        _AbstractAnnotationConstructor.__init__(self, [inner])
    @staticmethod        
    def name() -> str:
        return "ListOf"


class IteratorOf(_LocalAbstractAnnConst):
    def __init__(self, inner : Any):
        _AbstractAnnotationConstructor.__init__(self, [inner])
    @staticmethod
    def name():
        return "IteratorOf" 


class Not(_LocalAbstractAnnConst):
    def __init__(self, inner : Any):
        _AbstractAnnotationConstructor.__init__(self, [inner])
    @staticmethod
    def name():
        return "Not"  


class DictOf(_LocalAbstractAnnConst):
    def __init__(self, key : Any , value : Any):
        _AbstractAnnotationConstructor.__init__(self, (key , value))
    @staticmethod
    def name():
        return "DictOf"   


class ModuleAnnotationNamer:
    def __init__(self, modName : str):
        self.__modName = modName
    def __call__(self, name : str, value : Any):
        return _NamedAnnotation(self.__modName, name, value)
    def singleton(self, name : str, docstring : Or(str,False) = False):
        return _NamedAnnotation(self.__modName,
                                name,
                                _Singleton(docstring))        


LocalNamedAnnotation = ModuleAnnotationNamer(__name__)  

_RecursiveAnnotationDoc = LazyAnnotation(lambda: AnnotationDoc)

NamedAnnotationDefinitionInsideDoc = LocalNamedAnnotation(
    "NamedAnnotationDefinitionDoc",
    Or([str , _RecursiveAnnotationDoc],
       ["singleton" , str]))


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
    return ["class" , {"module": getModName(ann) , 
                       "name": ann.__name__ ,
                       "string": str(ann)}]


_FuncAnnotationDoc = ["function" , {"module": str, 
                                        "name": str}]
@NoDoc
def _docAnnotation_function(ann : inspect.isfunction) -> _FuncAnnotationDoc:
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



