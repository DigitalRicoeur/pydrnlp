#hash(((pydrnlp tokenizer) . ("module" #hasheq((classes . ()) (functions . ()) (text . ("docstring" "**TODO: Update this docstring.**"))))) ((pydrnlp language) . ("module" #hasheq((classes . ()) (functions . (("function" #hasheq((name . "get") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "langStr"))))) (return . #f)))) (text . ("docstring" "Returns a `spacy.language.Language` instance for the given IANA string.\n\nModels from SpaCy are loaded lazily.")))) ("function" #hasheq((name . "revision") (signature . ("signature" #hasheq((parameters . ()) (return . #f)))) (text . #f))))) (text . ("docstring" "Provides the function `pydrnlp.language.getLanguage`.\n\nTODO: en_core_web_lg sounds like it has pre-trained models and may give\nbetter accuracy. OTOH it is an order of magnitude bigger (667 vs 36 MB)."))))) ((pydrnlp tokenizer run) . ("module" #hasheq((classes . ()) (functions . ()) (text . ("docstring" "**TODO: Update this docstring.**\n\nA simple interface to pydrnlp.segtokenize based on standard IO.\n\nThis module does nothing unless __name__ == \"__main__\".\n\nWhen run, it immediately writes the JSON result of\npydrnlp.segtokenize.tokenizerRevision() to standard out.\nIt then enters an JSON IO loop (see pydrnlp.jsonio.mapJsonLines),\nprocessing the values with pydrnlp.segtokenize.tokenizeSegmentList().\n\nSee pydrnlp.jsonio for invariants about the JSON IO format."))))) ((pydrnlp doc) . ("module" #hasheq((classes . ()) (functions . (("function" #hasheq((name . "docClass") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "name"))) ("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "value"))))) (return . #f)))) (text . ("docstring" "Documents the class `value` as `name`.\n\nThe resulting object is tagged `\"class\"` and has \nthe following keys:\n- `\"name\"`: `name`.\n- `\"text\"`: the result of `getDocText(value)`.\n- `\"bases\"`: a list of base classes, where each item is tagged `\"base\"`\n  and has fields `\"name\"` and `\"module\"`, both of which are mapped to\n  strings.\n- `\"methods\"`: a list of function documentation objects\n  (see `docFunction`) for the class's public methods, including `__init__`.")))) ("function" #hasheq((name . "docFunction") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "name"))) ("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "value"))))) (return . #f)))) (text . ("docstring" "Documents the function `value` as `name`.\n\nThe resulting object is tagged `\"function\"`.\nIt has the following keys:\n- `\"name\"`: `name`.\n- `\"text\"`: the result of `getDocText(value)`.\n- `\"signature\"`: the result of `docSignature(value)`.")))) ("function" #hasheq((name . "docModpath") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "modpath"))))) (return . #f)))) (text . ("docstring" "Extracts documentation from the module at the given modpath.\n\nThe resulting object is tagged `\"modpath\"`.\nThe value of its `\"modpath\"` key is always the given modpath.\nThe value of its `\"module\"` key will usually be the\nsame kind of value produced by `docModule`, but it might be\nFalse, meaning that the module was not found, or \n\"ErrorDuringImport\", meaning that the module was found, \nbut an exception occured while trying to run it to \nobtain a module object.")))) ("function" #hasheq((name . "docModule") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "mod"))))) (return . #f)))) (text . ("docstring" "Extracts the documentation for a given module object.\n\nThe resulting object is tagged `\"module\"`.\nIt has the following keys:\n- `\"text\"`: the result of `getDocText(mod)`.\n- `\"functions\"`: a list of results from `docFunction`.")))) ("function" #hasheq((name . "docSignature") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "proc"))))) (return . #f)))) (text . ("docstring" "Tries to extract documentation about the signature\nof the given object.\n\nThe result may be \"TypeError\" if the given object\ndoes not support signatures or \"ValueError\" if it does,\nbut no signature could be found.\nOtherwise, it will be an object tagged `\"signature\"`\nwith the following fields:\n- `\"parameters\"`: a list of objects tagged `\"parameter\"` (see below).\n- `\"return\"`: either a string representing the function's return\n  annotation or `False` if the function had none.\n\nA parameter object has the following fields:\n- `\"name\"`: a string naming the formal argument.\n- `\"kind\"`: one of `\"POSITIONAL_ONLY\"`, `\"POSITIONAL_OR_KEYWORD\"`,\n  `\"VAR_POSITIONAL\"`, `\"KEYWORD_ONLY\"`, or `\"VAR_KEYWORD\"`.\n- `\"annotation\"`: a string representing the argument's annotation,\n  or `False` if it has none.\n- `\"default\"`: a string representing the argument's default value,\n  or `False` if it has none.")))) ("function" #hasheq((name . "getDocText") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "obj"))))) (return . #f)))) (text . ("docstring" "Attempts to get some text documenting obj.\n\nThe result is based on what was obtained:\n- If there was a docstring, it is tagged `\"docstring\"`.\n- If comments were found, they are tagged `\"comments\"`.\n- If both of those are missing or empty, returns `False`.")))))) (text . ("docstring" "Tools for extracting Python documentation to structured JSON.\n\nWhile this module exports certain public functions,\nit is primarily intended for programatic use with __name__=\"__main__\".\n\nAll non-trivial JSON values produced by this module are\ndesigned as type-tagged objects: that is, each is a two-element\narray, with the first element being a string identifying the type\nof the value and the second element being a JSON object\ncontaining the value's payload."))))) ((pydrnlp) . ("module" #hasheq((classes . ()) (functions . ()) (text . #f)))) ((pydrnlp jsonio) . ("module" #hasheq((classes . ()) (functions . (("function" #hasheq((name . "inJsonLines") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . "False") (kind . "POSITIONAL_OR_KEYWORD") (name . "fIn"))))) (return . #f)))) (text . ("docstring" "Reads newline-delimited JSON from input and \nreturns an iterator of parsed values.\n\nIf fIn is False, and by default, input will be read from sys.stdin.")))) ("function" #hasheq((name . "mapJsonLines") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "proc"))) ("parameter" #hasheq((annotation . #f) (default . "False") (kind . "POSITIONAL_OR_KEYWORD") (name . "fIn"))) ("parameter" #hasheq((annotation . #f) (default . "False") (kind . "POSITIONAL_OR_KEYWORD") (name . "fOut"))))) (return . #f)))) (text . ("docstring" "Runs a newline-delimited JSON IO loop, \nusing proc to transform the parsed values.\n\nIf fIn is False, and by default, input will be read from sys.stdin.\n\nIf fOut is False, and by default, output will be written to sys.stdout.")))) ("function" #hasheq((name . "writeJsonLine") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "jsOut"))) ("parameter" #hasheq((annotation . #f) (default . "False") (kind . "POSITIONAL_OR_KEYWORD") (name . "fOut"))))) (return . #f)))) (text . ("docstring" "Writes the JSON form of jsOut, followed by a newline, \nthen flushes the output.\n\nIf fOut is False, and by default, output will be written to sys.stdout.")))))) (text . ("docstring" "Provides JSON IO functions.\n\nThis module imposes the invariant that JSON values must be\ndelimited by newlines (i.e. \"\n\") and that the JSON values\nin input may not use the newline character internally,\neven where insignificant whitespace is allowed by the JSON spec.\nUsing newlines as a delimiter avoids a limitation of Python's\njson.load(), which blocks until it encounters an EOF.\nThis module also writes JSON with a terminating newline,\nthough Racket's JSON parser doesn't need this."))))) ((pydrnlp tokenizer tokenize) . ("module" #hasheq((classes . ()) (functions . (("function" #hasheq((name . "revision") (signature . ("signature" #hasheq((parameters . ()) (return . #f)))) (text . ("docstring" "Returns a non-False JSON value identifying the current revision.\n\nThe intended purpose is for clients to\nbe able to cache responses:\nAs long as tokenizerRevision() returns the same value,\ncalling this API on the same input should return equivalent output.\nThis enables clients to cache responses.\nThe returned value incorporates the results of\npydrnlp.drtoken.tokenFilterRevision() and\npydrnlp.drlanguage.drLanguageRevision().\n\nWhen the result of tokenizerRevision() changes, any cache is stale.")))) ("function" #hasheq((name . "tokenize") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "jsIn"))))) (return . #f)))) (text . ("docstring" "Tokenizes JsIn. **TODO: document this.**")))))) (text . ("docstring" "Core functionality for tokenizing segmented documents.\n\nWould it be useful for JsToken to track the token's\n  a) start and end positions and/or\n  b) index among significant tokens in the document?\n\n\nTypes used in this module:\n--------------------------\n\nJsToken:\n    {\"lemma\": str, \"text\": str}\nJsOutSegment:\n    {\"key\": JsExpr,\n     \"tokenized\": Listof[JsToken]}\nJsOut:\n    Listof[JsOutSegment]\n\n\nJsInSegment:\n    {\"key\": JsExpr, \"body\": str}\nJsIn:\n    {LangStr: Listof[JsInSegment]}"))))) ((pydrnlp tokenizer usetoken) . ("module" #hasheq((classes . ()) (functions . (("function" #hasheq((name . "posIsBoring") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "str"))))) (return . #f)))) (text . ("docstring" "The opposite of `pydrnlp.tokenizer.posIsInteresting`.")))) ("function" #hasheq((name . "posIsInteresting") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "str"))))) (return . #f)))) (text . ("docstring" "Recognizes the interesting potential values from \n`spacy.tokens.Token.pos_`.\n\nNotable exclusions: \"NUM\"->numeral, \"SYM\"=>symbol\n\n\"pos\" stands for \"part of speech\"")))) ("function" #hasheq((name . "revision") (signature . ("signature" #hasheq((parameters . ()) (return . #f)))) (text . ("docstring" "Identifies the revision of this module.\n\nIncorporates `pydrnlp.stop_words.revision`.")))) ("function" #hasheq((name . "tokenQuicklyFails") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "token"))))) (return . #f)))) (text . ("docstring" "Predicate recognizing tokens which obviously should NOT be counted.\n\nSpecifically, recognizes tokens which are immeduate stop words, \npunctuation, or whitespace, or which have an uninteresting \npart of speech tag according to `pydrnlp.tokenizer.posIsBoring`.\n\nUsed to implement `pydrnlp.tokenizer.tokenShouldUseForLang`.")))) ("function" #hasheq((name . "tokenShouldUseForLang") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "token"))) ("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "lang"))))) (return . #f)))) (text . ("docstring" "Recognizes tokens which should be included in counting\nwith respect to the given `spacy.language.Language` instance.\n\nCurrently, it checks that neither `pydrnlp.tokenizer.tokenQuicklyFails`\nnor `pydrnlp.stop_words.tokenAnyIsStopForLanguage`\nreturn `True` for the given token.")))))) (text . ("docstring" "Functions for filtering out uninteresting tokens."))))) ((pydrnlp stop_words) . ("module" #hasheq((classes . ()) (functions . (("function" #hasheq((name . "revision") (signature . ("signature" #hasheq((parameters . ()) (return . #f)))) (text . ("docstring" "Identifies the revision of `pydrnlp.stop_words`.")))) ("function" #hasheq((name . "tokenAnyIsStopForLanguage") (signature . ("signature" #hasheq((parameters . (("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "token"))) ("parameter" #hasheq((annotation . #f) (default . #f) (kind . "POSITIONAL_OR_KEYWORD") (name . "nlp"))))) (return . #f)))) (text . ("docstring" "Returns `True` if any form of `token` is a stop word\nfor the `spacy.language.Language` instance `nlp`.\n\nThis is needed because `spacy.tokens.Token.is_stop`\nreturns `False` when the token itself is not a stop word, even if\na more normalized form might be.\nApparently this is by design: see \nhttps://stackoverflow.com/q/47523112/2523780.\nTherefore, this function also checks the lemma and lowercase forms\nof the token. It tries to do so efficiently.")))))) (text . ("docstring" "Provides `pydrnlp.stop_words.tokenAnyIsStopForLanguage`."))))))