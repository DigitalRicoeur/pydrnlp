# -*- coding: utf-8 -*-

from pydrnlp import segtokenize
from flask import Flask, request, jsonify


app = Flask(__name__)


@app.route("/tokenizer-revision")
def revision():
    return jsonify(segtokenize.revision())


@app.route("/tokenize", methods = ["POST"])
def tokenize():
    return jsonify(segtokenize.tokenizeDocList(request.get_json(force=True)))


