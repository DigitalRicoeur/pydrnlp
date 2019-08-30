# -*- coding: utf-8 -*-
"""
Created on Thu Nov 15 20:39:25 2018

@author: James

Code for determining a threshold value that
minimizes intra-class variance (equivalent to
maximizing inter-cass variance).

We consider two classes, high relevance and low relevance,
based on the pseudocounts returned by the LDA
assignment of words to topics.

Based on Otsu's method
"""

import numpy as np
import scipy.stats
from scipy import signal
import pydrnlp.jsonio

def revision():
    thisModuleRevision = 0
    return thisModuleRevision

def threshold(pseudocounts):
    """The input for the threshold function is an array of pseudocounts.
    (This can be changed)
    The ouput is a threshold (cutoff value);
    words with pseudocounts less than this threshold are
    deemed as low relevance for the topic (concept).
    """
    density = scipy.stats.gaussian_kde(pseudocounts)
    xs = np.linspace(0, 1, 2000)
    ys = density(xs)
    np.seterr(divide='ignore')
    peaks = signal.find_peaks_cwt(-ys, np.array([0.001, 0.01, 0.1]))
    if peaks.any():
        return min(xs[peaks])
    else:
        return 0


def demo():
    # 0.5917958979489745
    data = {"mine": [0.9, 0.93, 0.87, 0.82, 0.1, 0.2, 0.35, 0.12, 0.14, 0.25],
            "evil": [485, 129, 126, 100, 96, 88, 80, 63, 59, 52],
            "symbol": [227, 185, 100, 87, 73, 73, 70, 68, 57, 54],
            "test1": [400, 401, 402, 350, 55, 34],
            "test2": [200, 189, 178, 150, 45, 50]}
    return {k:threshold(v) for (k,v) in data.items()}


class ElbowWorker(pydrnlp.jsonio.Worker):
    def prelude(self):
        return [ revision() ]
    def each(self, pseudocounts):
        return threshold(pseudocounts)


if __name__ == "__main__":
    ElbowWorker().start()
