'# -*- coding: 'latin-1' -*-'

#  ##################################################################################### 
#       File-Name:      extraction.py
#       Version:        Python 2.7
#       Date:           Dec 23, 2014
#       Author:         MM
#       Purpose:        Code to parse keywords out of text in  
#                       #YaMeCansePorEsoPropongo data
#       Input Files:    Postcard1K_tab.txt                         
#       Output Files:   NONE
#       Data Output:    NONE
#       Previous files: NONE
#       Dependencies:   NONE
#       Status:         IN PROGRESS
#       Machine:        Mac
#  #################################################################################### 

import pandas as pd
from pandas.io.parsers import read_csv
import string
import nltk
from nltk.collocations import *
from nltk.probability import FreqDist
from nltk.corpus import stopwords
from nltk.metrics import NgramAssocMeasures


%cd '~/Documents/#YaMeCanse_postales'

data = pd.read_csv('data/Postcard1K_tab.txt', delimiter = '\t', encoding = 'mac_latin2') 
# NOTE: data must be read in mac_latin2 econding to preserve Spanish accents

#data = pd.read_csv('/Users/marco_morales/Documents/#YaMeCanse_postales/data/Postcard_test_tab.txt', 
#	delimiter = '\t', encoding = 'mac_latin2') 
len(data)					# verifies all data was loaded
data.head()					# verifies all data was loaded

# PROCESSING TEXT INTO USABLE FORMATS

proposals = data['Body']	# subsets only text
len(proposals)   			# verifies all data was loaded 

# Step 1: create a single text with all postcard messages 

proposals_corp = []
counter = 0
for i in range(0, len(proposals)):
    if type(proposals[i]) != float:
        proposals_corp.append(proposals[i])
    else:
        counter = counter + 1
        print counter, 'missing line'
all_proposals = u' '.join(proposals_corp)

# Step 2: convert text into NLTK-usable objects 

tokens = nltk.wordpunct_tokenize(all_proposals)          # tokenizes text
tokens = [t.lower() for t in tokens]                     # makes lowercase

# Step 3: create list of words (stopwords, additional words, puntuation)
# to be eliminated from tokenized text

all_stops = []
stops = stopwords.words('spanish')                       # gets list of stopwords in Spanish

more_words = ["yamecanse", "yamecansé", "yamecansepor", "poresopropongo", 
          "yamecanseporesopropongo", "yamecanséporesopropongo", "por", 
          "eso", "propongo", "que", "canse", "cansé", "ser", "así", "cada", 
          "mas", "solo", "cualquier", "sólo", "etc", "yamecansè", 
          "yamecansépor", "tan", "yame", "yamecanséde", "yamecansédetener", 
          "yamécansépor", "yamecanséyporesopropongo", "yamecase", 
          "yopropongoquien", "?...", "!!!", "!!!!"]
for i in range(0, len(more_words)):
    more_words[i] = more_words[i].decode('mac_latin2')   # converts strings to unicode	

punct = []                                            # gets list of punctuation signs
for i in range(0, len(string.punctuation)):
	punct.append(string.punctuation[i])
    punct[i] = punct[i].decode('mac_latin2')          # converts strings to unicode	

remove_words  = stops + more_words + punct            # list w/ stopwords, useless words, punctuation
print len(stops), len(more_words), len(punct), len(remove_words)									  


# find out why these aren't eliminated:
''' [u'!!',
 u'!!,',
 u'")',
 u'",',
 u'".',
 u'"...',
 u'";',
 u'%)',
 u'%,',
 u'%.',
 u"''",
 u"'',",
 u"'.",
 u')",',
 u'),',
 u').',
 u')...',
 u',!!',
 u',,,',
 u'.!!!',
 u'."',
 u'.#',
 u'.(',
 u'.)',
 u'.).',
 u'.,',
 u'.,)',
 u'.-',
 u'..',
 u'..!!',
 u'...',
 u'...!!',
 u'....',
 u'....!!!',
 u'.....',
 u'......',
 u'./',
 u'.:',
 u'.\\' '''



# Step 4: filter out unnecessary words from tokenized text

filtered_tokens = []                                 
for word in tokens: 
    if word not in remove_words: 
        filtered_tokens.append(word) 	             

filtered_text = nltk.Text(filtered_tokens)			# generates filtered text
print len(filtered_text)
text = nltk.Text(tokens)							# generates unfiltered text
print len(text)

filtered_vocab = sorted(set(filtered_text))
vocab = sorted(set(text))

# function to estimate proportion of stopwords in text
# def content_fraction(text):
#	stopwords = nltk.corpus.stopwords.words('spanish')
#	content = [w for w in text if w.lower() not in stopwords]
#	return len(content) / len(text)
# content_fraction(text)

# Step 4: stem words


# SOME INITIAL EXPLORATIONS OF THE TEXT

sorted(set(text))  	             # displays sorted unique words
fdist = FreqDist(text)           # creates a frequency distribution for words
vocabulary = fdist.keys()        # creates frequency distributions vocabularies
vocabulary[:50]                  # displays 50 most frequent words in text 
fdist.plot(50, cumulative=True)  # frequency distribution for 50 most frequent words 
text.collocations()              # common word collocations



# APPROACH 1: POINTWISE MUTUAL INFORMATION (PMI)

bigram_measures   = nltk.collocations.BigramAssocMeasures()
trigram_measures  = nltk.collocations.TrigramAssocMeasures()
#quadgram_measures = nltk.collocations.QuadgramAssocMeasures()


finder_bi   = BigramCollocationFinder.from_words(text)
finder_tri  = TrigramCollocationFinder.from_words(text)
finder_quad = QuadgramCollocationFinder.from_words(text)

finder_bi_f   = BigramCollocationFinder.from_words(filtered_text)
finder_tri_f  = TrigramCollocationFinder.from_words(filtered_text)
finder_quad_f = QuadgramCollocationFinder.from_words(filtered_text)

# only n-grams that appear 3+ times
finder_bi.apply_freq_filter(3) 
finder_tri.apply_freq_filter(3)
finder_quad.apply_freq_filter(3)

finder_bi_f.apply_freq_filter(3) 
finder_tri_f.apply_freq_filter(3)
finder_quad_f.apply_freq_filter(3)



# returns 10 n-grams with the highest PMI
finder_bi.nbest(bigram_measures.pmi, 10)  
finder_tri.nbest(trigram_measures.pmi, 10)  
finder_quad.nbest(quadgram_measures.pmi, 10)  

finder_bi_f.nbest(bigram_measures.pmi, 10)  
finder_tri_f.nbest(trigram_measures.pmi, 10)  
finder_quad_f.nbest(quadgram_measures.pmi, 10)  



# APPROACH 2: TWO-STAGE MULTIWORD TAGGING 	
# with stage 1 (pooling all postcard texts) done, proceed to create
# bigram/trigram tags and apply PMI for each message in postcard 


# text  level 
PMI(term, tweet) = log [ P(term, tweet) / (P(term)*P(tweet))

