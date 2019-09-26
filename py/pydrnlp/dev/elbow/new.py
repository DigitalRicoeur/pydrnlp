# -*- coding: utf-8 -*-
"""
@author: James Broda
"""

import numpy as np
import scipy.stats # for KDE
from scipy.signal import argrelextrema # for KDE
'''
Code for determining a threshold value that
minimizes intra-class variance (equivalent to
maximizing inter-cass variance). 
 
We consider two classes, high relevance and low relevance, 
based on the pseudocounts returned by the LDA 
assignment of words to topics.

Based on Otsu's method
'''
'''
The input for the threshold function is an array of pseudocounts. (This can be changed)
The ouput is a threshold (cutoff value); 
words with pseudocounts less than this threshold are
deemed as low relevance for the topic (concept). 
'''

def threshold_OTSU(pseudocounts):
    number_of_tokens = len(pseudocounts)
    intra_class_variance = np.ones(number_of_tokens)
    counter = 0
    for t in pseudocounts:     
        high_relevance_slice = [i  for i in pseudocounts if i >= t]
        low_relevance_slice = [i  for i in pseudocounts if i < t]
        high_relevance_probability = sum(high_relevance_slice)/sum(pseudocounts)
        #high_relevance_probability = len(high_relevance_slice)/number_of_tokens
        low_relevance_probability = 1 - high_relevance_probability
        if len(high_relevance_slice):
            high_relevance_variance = np.var(high_relevance_slice)
        else:
            high_relevance_variance = 0
        if len(low_relevance_slice):
            low_relevance_variance = np.var(low_relevance_slice)
        else:
            low_relevance_variance = 0
        intra_class_variance[counter] = low_relevance_probability*low_relevance_variance+high_relevance_probability*high_relevance_variance
        counter += 1
    #return(pseudocounts[np.argmin(intra_class_variance)], intra_class_variance)
    return(pseudocounts[np.argmin(intra_class_variance)])       
        
'''
Below is an alternative threshold algorithm 
that requires as input a vector of values between
0 and 1.  It creates a KDE of the pdf profile of the
data values and that trims the first 'mode' to determine
a threshold.  The logic here is different than in the previous 
threshold algorithm; this approach does not assume that there are two classes,
relevant and irrelevant, but simply that there is a cluster of irrelevant
data, which perhaps should be discarded/
'''
'''
Note: sample_values should be between 0 and 1
'''
def threshold_KDE(sample_values):
    ## the xs will be used as the input values for the estimated density
    xs = np.linspace(0, 1, 20*len(sample_values))
    ## c is the factor by which the KDE bandwidth is reduced in case mulitple modes are not detected initially
    ##  KDE is sensitive to bandwidth, shorter bandwidths are less 'smooth'
    c = 0.9
    ## create a denisty function using the default bandwidth, based on Scott's Rule
    density = scipy.stats.gaussian_kde(sample_values)
    ## ys is a vector of values equal to the height of the denisty functions, evaluated at each xs
    ys = density(xs)
    ## The steps below are for determining the first local minimum
    peaks = argrelextrema(ys, np.less)
    if peaks:
        threshold1 = min(xs[peaks])        
    else:
        while not(peaks):
            ## If no local minima are found, the bandiwdth is reduced by 10%
            print('!!')
            density.set_bandwidth(bw_method=density.factor * c)        
            ys = density(xs)
            peaks = argrelextrema(ys, np.less)    
        threshold1 = min(xs[peaks])   
    
    return(threshold1)
