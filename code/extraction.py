#  ####################################################################### 
#       File-Name:      extraction.py
#       Version:        Python 2.7
#       Date:           Dec 23, 2014
#       Author:         MM
#       Purpose:        Code to parse keywords out of text in  
#                       #YaMeCansePorEsoPropongo data
#       Input Files:    Postcard Dashboard  Orders - Movement Postcards_converted.txt                         
#       Output Files:   NONE
#       Data Output:    NONE
#       Previous files: NONE
#       Dependencies:   NONE
#       Status:         COMPLETED
#       Machine:        Mac
#  ####################################################################### 


# APPROACH 1: POINTWISE MUTUAL INFORMATION (PMI)

import nltk
from nltk.collocations import *
bigram_measures = nltk.collocations.BigramAssocMeasures()
trigram_measures = nltk.collocations.TrigramAssocMeasures()

# change this to read in your data
finder = BigramCollocationFinder.from_words(
   nltk.corpus.genesis.words('english-web.txt'))

# only bigrams that appear 3+ times
finder.apply_freq_filter(3) 

# return the 10 n-grams with the highest PMI
finder.nbest(bigram_measures.pmi, 10)  



# APPROACH 2: TWO-STAGE MULTIWORD TAGGING 	

# stage 1: pool all postcard texts 



# stage 2: pool all postcard texts 



# text  level 
PMI(term, tweet) = log [ P(term, tweet) / (P(term)*P(tweet))

