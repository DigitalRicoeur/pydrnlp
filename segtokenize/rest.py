# coding: utf8

from segtokenize import segtokenize
from flask import Flask, request, jsonify

app = Flask(__name__)

@app.route("/revision")
def revision():
    return jsonify(segtokenize.revision())

@app.route("/tokenize", methods = ["POST"])
def tokenize():
    return jsonify(segtokenize.handle(request.get_json(force=True)))
