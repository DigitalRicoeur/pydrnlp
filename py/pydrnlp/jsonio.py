# -*- coding: utf-8 -*-
"""Provides JSON IO functions.

This module imposes the invariant that JSON values must be
delimited by newlines (i.e. "\\n") and that the JSON values
in input may not use the newline character internally,
even where insignificant whitespace is allowed by the JSON spec.
Using newlines as a delimiter avoids a limitation of Python's
json.load(), which blocks until it encounters an EOF.
This module also writes JSON with a terminating newline,
though Racket's JSON parser doesn't need this.
"""

#import srsly
import json
import sys

def map_json_lines(proc): #, fIn = False, fOut = False):
    """Runs a newline-delimited JSON IO loop,
    using proc to transform the parsed values.
    """
    #If fIn is False, and by default, input will be read from sys.stdin.
    #If fOut is False, and by default, output will be written to sys.stdout.
    for jsIn in _inJsonLines():
        write_json_line(proc(jsIn))


def write_json_line(jsOut): #, fOut = False):
    """Writes the JSON form of jsOut, followed by a newline,
    then flushes the output.

    If fOut is False, and by default, output will be written to sys.stdout.
    """
    fOut_ = sys.stdout
    json.dump(jsOut,fOut_)
    fOut_.write("\n")
    fOut_.flush()


def _inJsonLines(fIn = False):
    """Reads newline-delimited JSON from input and
    returns an iterator of parsed values.

    If fIn is False, and by default, input will be read from sys.stdin.
    """
    for line in (fIn if fIn else sys.stdin):
        yield json.loads(line)
