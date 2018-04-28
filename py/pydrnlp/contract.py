# -*- coding: utf-8 -*-

import inspect


class _AbstractBaseContract:
    # make the defaults raise errors
    def modName(self):
        pass
    def name(self):
        pass
    def _docContAnnotation(self):
        pass

    
def _isBaseContract(any):
    return isinstance(_AbstractBaseContract)


class _Contract(_AbstractBaseContract):
    def __init__(self, modName, name, value):
        self.__modName = modName
        self.__name = name
        self._value = value
    def modName(self):
        return self.__modName
    def name(self):
        return self.__name
    def _docContAnnotation(self):
        return ["contract",
                {"module": self.modName(), 
                 "name": self.name(),
                 "string": str(self)}]
    def docSelf(self):
        return [self.name() , docAnnotation(self._value)]


def iscontract(any) -> bool:
    return isinstance(any, _Contract)


class ModuleContract:
    def __init__(self, modName):
        self.__modName = modName
    def __call__(self, name, value):
        return _Contract(self.__modName, name, value)

    
class _AbstractContractConstructor(_AbstractBaseContract):
    # Concrete subclasses must add name and modName methods
    def __init__(self,args):
        self._args = args
    def _docContAnnotation(self):
        return ["contract-constructor",
                {"module": self.modName(), 
                 "name": self.name(),
                 "args": list(map(docAnnotation,self._args))}]


class ListOf(_AbstractContractConstructor):
    def __init__(self,inner):
        _AbstractContractConstructor.__init__(self,[inner])
    def modName(self):
        return __name__
    def name(self):
        return "ListOf"


class IteratorOf(_AbstractContractConstructor):
    def __init__(self,inner):
        _AbstractContractConstructor.__init__(self,[inner])
    def modName(self):
        return __name__
    def name(self):
        return "IteratorOf"   


def isContractClass(cls):
    if issubclass(cls, _AbstractContractConstructor):
        return "contract-constructor"
    elif issubclass(cls, _Contract):
        return "contract"
    else:
        return False
    

def getModName(obj):
    # can inspect.getmodule fail ?
    return inspect.getmodule(obj).__name__

                
def docAnnotation(ann):
    if inspect.isclass(ann):
        return ["class" ,
                {"module": getModName(ann), 
                 "name": ann.__name__,
                 "string": str(ann)}]
    elif inspect.isfunction(ann):
        return ["function" ,
                {"module": getModName(ann), 
                 "name": ann.__name__}]
    elif isBaseContract(ann):
        return ann._docContAnnotation()
    elif isinstance(ann,str):
        return ["string" , ann]
    elif isinstance(ann,dict):
        ret = ["dict" ,
               list([docAnnotation(k),docAnnotation(v)]
                    for k , v in ann.items())]
    elif isinstance(ann,list):
        ret = ["list" ,
               list(map(docAnnotation,ann))]
    else:
        return ["other" , str(ann)]


__predicates = []

def Predicate(proc):
    __predicates.append(proc)
    return proc


def ispredicate(x):
    return (x in __predicates)
