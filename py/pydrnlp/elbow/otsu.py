# -*- coding: utf-8 -*-

import numpy as np

def threshold(pseudocounts):

    sum_psc = sum(pseudocounts)
    ### t from pseudocounts
    high_relevance_slice = [i  for i in pseudocounts if i >= t]
    low_relevance_slice = [i  for i in pseudocounts if i < t]
    high_relevance_probability = sum(high_relevance_slice)/sum_psc
    low_relevance_probability = 1 - high_relevance_probability
    ###
    def relev_class_calc(
    
def _slice_variance(s):
    return np.var(s) if len(s) else 0


