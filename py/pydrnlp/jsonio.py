#lang pydrnlp/support/python-lang # -*- coding: utf-8 -*-
"""Provides classes for programs that loop over JSON IO.

This module imposes the invariant that JSON values must be
delimited by newlines (i.e. "\n") and that the JSON values
in input may not use the newline character internally,
even where insignificant whitespace is allowed by the JSON spec.
Using newlines as a delimiter avoids a limitation of Python
JSON parsers, which block until they encounter an EOF.
This module also writes JSON with a terminating newline,
though Racket's JSON parser doesn't need this.
"""

from abc import ABC, abstractmethod
import multiprocessing
import srsly
import srsly.ujson
import sys


def start_loop(revision, on_input):
    def emit(js):
        _dump_json_line(js, sys.stdout)
    emit(revision())
    for js in _yield_json_lines(sys.stdin):
        for ret in on_input(js):
            emit(ret)
        emit(None)


# FIXME
# line 92, in _yield_json_lines
#    for line in fIn:
# ValueError: I/O operation on closed file.
class _MultiprocessWorker():
    """**BROKEN** Like `Worker`, but uses helper processes to
    handle IO without blocking the main loop.
    This might be helpful when the input and output JSON values are big.
    """
    def _after_prelude(self):
        inputQ = multiprocessing.SimpleQueue()
        outputQ = multiprocessing.SimpleQueue()
        inputP = multiprocessing.Process(target=_queue_read_json_lines,
                                         args=(inputQ, sys.stdin))
        inputP.start()
        outputP = multiprocessing.Process(target=_queue_dump_json_lines,
                                          args=(outputQ, sys.stdout))
        outputP.start()
        for jsIn in _queue_iterate(inputQ):
            jsOut = self.each(jsIn)
            outputQ.put(jsOut)



def _queue_iterate(q):
    while True:
        yield q.get()



def _queue_read_json_lines(q, fIn):
    for js in _yield_json_lines(fIn):
        q.put(js)

def _queue_dump_json_lines(q, fOut):
    for js in _queue_iterate(q):
        _dump_json_line(js, fOut)



def _yield_json_lines(fIn):
    # based on srsly.read_jsonl, but doesn't skip blank lines
    for line in fIn:
        yield srsly.json_loads(line)

def _dump_json_line(js, fOut):
    # based on srsly.write_jsonl, but avoids intermediate string
    srsly.ujson.dump(js, fOut)
    fOut.write("\n")
    fOut.flush()
