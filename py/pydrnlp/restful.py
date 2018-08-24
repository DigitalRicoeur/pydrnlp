# -*- coding: utf-8 -*-
"""Provides a RESTful HTTP interface to pydrnlp.segtokenize.

This module is intended to be set as the "FLASK_APP"
environment variable.
"""

import pydrnlp.segtokenize as seg
from flask import Flask, request, jsonify

app = Flask(__name__)


@app.route("/tokenizer-revision")
def tokenizerRevision():
    return jsonify(seg.tokenizerRevision())


@app.route("/tokenize", methods = ["POST"])
def tokenize():
    return jsonify(seg.tokenizeSegmentList(request.get_json(force=True)))


