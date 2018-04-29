# -*- coding: utf-8 -*-
"""Provides a RESTful HTTP interface to pydrnlp.segtokenize.

This module is intended to be set as the "FLASK_APP"
environment variable.
"""

import pydrnlp.segtokenize as seg
from pydrnlp.annotations import NoDoc
from flask import Flask, request, jsonify

app = Flask(__name__)


@NoDoc
@app.route("/tokenizer-revision")
def tokenizerRevision():
    return jsonify(seg.tokenizerRevision())


@NoDoc
@app.route("/tokenize", methods = ["POST"])
def tokenize():
    return jsonify(seg.tokenizeDocList(request.get_json(force=True)))


