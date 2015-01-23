#  ####################################################################### 
#       File-Name:      WordClouds.R
#       Version:        R 3.1.2
#       Date:           Dec 15, 2014
#       Author:         MM
#       Purpose:        Process postcard text using NLP tools to distill  
#                       phrases that syntesize proposals, and creates 
#                       WordCloud graphs for 
#                       #YaMeCansePorEsoPropongo data
#       Input Files:    Postcard5.5K_tab_150121.txt                         
#       Output Files:   wordcloud_all.pdf
#       Data Output:    raw_phrases_YYMMDD.txt
#                       data_phrases_YYMMDD.txt
#       Previous files: NONE
#       Dependencies:   CreateDictionary.R (dictionary for stem completion)
#       Status:         IN PROGRESS
#       Machine:        Mac
#  ####################################################################### 

# TO DO:
# 1) find better way to complete stemmed words 
# 2) benchmark algorithm against n-grams and PMI 

rm(list=ls(all=TRUE))

library(foreign)
library(data.table)
library(tm)
library(wordcloud)
library(RWeka)

setwd("~/Documents/#YaMeCanse_postales")
getwd()

# NOTE: data must be entered in UTF-16 format so that accented letters can be properly read. 
#       Also, file is tab-delimited text to avoid confusions with commas in the text

data <- as.data.table(read.delim(
    "data/raw/Postcard5.5K_tab_150121.txt",
    header=TRUE, sep="\t", na.strings = "NA", fileEncoding = "UTF-16"))
names(data)                                 # verifies all variables are loaded properly
length(data$ID)                             # verifies that no observations were lost
head(data)                                  # checks for possible errors in matrix
#data[, unique.id:=.GRP, by=data$Customer]   # creates unique user identifier

# PROCESSING DATA WITH NLP TOOLS

proposals <- data$Body[!is.na(data$Body)]      # eliminates empty postcards
print(paste0(length(data$ID)-length(proposals), " empty postcards eliminated"))
nonempty <- length(proposals)
proposals <- proposals[!duplicated(proposals)] # eliminates duplicates 
print(paste0(nonempty-length(proposals), " duplicate postcards eliminated"))
print(paste0("Currently, ", length(proposals), " unique postcards in database"))


text <- VCorpus(VectorSource(proposals), readerControl= list(language = "spanish"))
inspect(text[1:10])

# complements list of stopwords
stopwords = c(stopwords("spanish"), "#", "yamecanse", "yamecansé", "yamecansepor", "poresopropongo", 
          "yamecanseporesopropongo", "yamecanséporesopropongo", "por", "eso", "propongo", "que", 
          "canse", "cansé", "ser", "así", "asi", "cada", "mas", "solo", "cualquier", "sólo", "etc", 
          "yamecansè", "yamecansépor", "tan", "yame", "yamecanséde", "yamecansédetener", "aunque", 
          "yamécansépor", "yamecanséyporesopropongo", "yamecase", "yopropongoquien", "yanomás",
          "queremos", "quiera", "tal", "pido", "yopropongo", "º")

# transform text in corpus to usable bits
text <- tm_map(text, content_transformer(tolower), mc.cores=1) # makes lowercase
text <- tm_map(text, removeNumbers)                            # removes numbers
text <- tm_map(text, removePunctuation)                        # removes punctuation
text <- tm_map(text, removeWords, stopwords, lazy=TRUE)        # removes expendable words
text <- tm_map(text, stripWhitespace)                          # removes white space
# text <- tm_map(text, stemDocument, language ="spanish")        # stems text
# text <- tm_map(text, stemCompletion, dictionary = dictionary ) # completes stems w/ ad hoc dic
inspect(text[1:10])

# perform manual replacements of text as a quicker alternative to stemming/completing 
ReplaceText <- content_transformer(function(x, from, to) gsub(from, to, x))
#text <- tm_map(text, ReplaceText, "\\bpresidentes\\b", "presidente")
text <- tm_map(text, ReplaceText, "\\breducción\\b", "reducir")
text <- tm_map(text, ReplaceText, "\\breduzcan\\b", "reducir")
text <- tm_map(text, ReplaceText, "\\beducacion\\b", "educación")
text <- tm_map(text, ReplaceText, "\\bmejores\\b", "mejor")
text <- tm_map(text, ReplaceText, "\\brenuncia\\b", "renuncie")
text <- tm_map(text, ReplaceText, "\\beliminación\\b", "eliminar")
text <- tm_map(text, ReplaceText, "\\belimine\\b", "eliminar")
#text <- tm_map(text, ReplaceText, "\\bpolítico\\b", "políticos")
text <- tm_map(text, ReplaceText, "\\bpensiones\\b", "pensión")
text <- tm_map(text, ReplaceText, "\\bvitalicias\\b", "vitalicia")
text <- tm_map(text, ReplaceText, "\\bcandidato\\b", "candidatos")
text <- tm_map(text, ReplaceText, "\\bdisminución\\b", "reducir")
text <- tm_map(text, ReplaceText, "\\bdisminuir\\b","reducir") 
text <- tm_map(text, ReplaceText, "\\bdisminuya\\b", "reducir") 
text <- tm_map(text, ReplaceText, "\\breduzca\\b", "reducir")
text <- tm_map(text, ReplaceText, "\\bbajen\\b", "reducir")
text <- tm_map(text, ReplaceText, "\\bbajar\\b", "reducir")
text <- tm_map(text, ReplaceText, "\\bnumero\\b", "número")
#text <- tm_map(text, ReplaceText, "\\bderechos\\b", "derecho")
text <- tm_map(text, ReplaceText, "\\bfamilias\\b", "familia")
text <- tm_map(text, ReplaceText, "\\bpolicias\\b", "policía")
text <- tm_map(text, ReplaceText, "\\bpolicías\\b", "policía")
text <- tm_map(text, ReplaceText, "\\bpoliticos\\b", "políticos")
text <- tm_map(text, ReplaceText, "\\bpolitica\\b", "política")
text <- tm_map(text, ReplaceText, "\\bpublicos\\b", "público")
text <- tm_map(text, ReplaceText, "\\bpublico\\b", "público")
text <- tm_map(text, ReplaceText, "\\bpúblicos\\b", "público")
text <- tm_map(text, ReplaceText, "\\bsalarios\\b", "salario")
text <- tm_map(text, ReplaceText, "\\bsueldo\\b", "salario")
text <- tm_map(text, ReplaceText, "\\bsueldos\\b", "salario")
text <- tm_map(text, ReplaceText, "\\bmexico\\b", "méxico")
text <- tm_map(text, ReplaceText, "\\bpaís\\b", "méxico")
text <- tm_map(text, ReplaceText, "\\bpais\\b", "país")
text <- tm_map(text, ReplaceText, "\\bfuncionarios\\b", "funcionario")
text <- tm_map(text, ReplaceText, "\\bservidor\\b", "funcionario")
text <- tm_map(text, ReplaceText, "\\bservidores\\b", "funcionario")
text <- tm_map(text, ReplaceText, "\\bpresupuesto\\b", "dinero")
text <- tm_map(text, ReplaceText, "\\brecursos\\b", "dinero")
text <- tm_map(text, ReplaceText, "\\bdineros\\b", "dinero")
text <- tm_map(text, ReplaceText, "\\bmexicano\\b", "mexicanos")
text <- tm_map(text, ReplaceText, "\\brepublica\\b", "república")
#text <- tm_map(text, ReplaceText, "\\bciudadanos\\b", "mexicanos")
text <- tm_map(text, ReplaceText, "\\bciudadano\\b", "mexicanos")
text <- tm_map(text, ReplaceText, "\\bgente\\b", "mexicanos")
text <- tm_map(text, ReplaceText, "\\bpersonas\\b", "mexicanos")
text <- tm_map(text, ReplaceText, "\\bpueblo\\b", "mexicanos")
#text <- tm_map(text, ReplaceText, "\\bsociedad\\b", "mexicanos")
text <- tm_map(text, ReplaceText, "\\bciudadanía\\b", "mexicanos")
text <- tm_map(text, ReplaceText, "\\baño\\b", "años")
text <- tm_map(text, ReplaceText, "\\bcreación\\b", "crear")
text <- tm_map(text, ReplaceText, "\\bcree\\b", "crear")
text <- tm_map(text, ReplaceText, "\\bcumplan\\b", "cumplir")
text <- tm_map(text, ReplaceText, "\\bevaluaciones\\b", "evaluación")
text <- tm_map(text, ReplaceText, "\\bdeben\\b", "debe")
text <- tm_map(text, ReplaceText, "\\bhagan\\b", "hacer")
text <- tm_map(text, ReplaceText, "\\bepn\\b", "EPN")
text <- tm_map(text, ReplaceText, "\\benrique peña nieto\\b", "EPN")
text <- tm_map(text, ReplaceText, "\\benrique pena nieto\\b", "EPN")
text <- tm_map(text, ReplaceText, "\\bpeña nieto\\b", "EPN")
text <- tm_map(text, ReplaceText, "\\bpena nieto\\b", "EPN")
text <- tm_map(text, ReplaceText, "\\benrique peña\\b", "EPN")
text <- tm_map(text, ReplaceText, "\\benrique pena\\b", "EPN")
text <- tm_map(text, ReplaceText, "\\bpeña\\b", "EPN")
inspect(text[1:10])

# CREATE TERM DOCUMENT MATRIX TO EXPLORE WORD FREQUENCIES

tdm <- TermDocumentMatrix(text)          # creates matrix
nDocs(tdm)                               # verifies number of unique documents (postcards)
nTerms(tdm)                              # gets number of unique terms in corpus
inspect(tdm[1:10, 1:10])                 # verifies sample contents of matrix
inspect(removeSparseTerms(tdm[1:10, 1:10], 0.8))
FreqTerms <- findFreqTerms(tdm, 50)      # identify most common words in dataset
# dict <- unique(rownames(as.matrix(tdm)))   # creates dictionary of unique words

frequencies <- rowSums(as.matrix(tdm))
names(frequencies) <- rownames(as.matrix(tdm))
frequencies <- sort(frequencies, decreasing=T)
frequencies <- as.data.frame(frequencies)
names(frequencies) <- c("freq")
frequencies[1:150, "freq", drop=FALSE]    # shows 150 most common words
# write.csv(frequencies, file="data/word_frequencies.csv", fileEncoding = "UTF-16")


# Yet another approach to exctract phrases that distill common proposals 
# in the postcards:
# 1) gather 50-100 most-common-terms
# 2) find highly-associated words to most-common-terms
# 3) create lists of highly-associated terms as bags-of-words to construct phrases
# 4) clean and construct phrases from the bag for each most-common-term
# 5) classify and subclassify phrases

# Create a function that distills phrase elements related to most common words with 
# two filters: words correlated above 0.10, and with sparsity > 10% 

PhrasePrimer <- function(wordlist, corr = 0.1, perc = 0.1) {
    word <- NA
    phrase <- NA
    phrase.list <- data.frame(word, phrase)    
    for (w in wordlist) {
        correlates <- as.data.frame(findAssocs(tdm, w, corr))      # gets list of correlated words
        bag <- merge(correlates, frequencies, by = "row.names")    # creates dataframe w corr/freq    
        bag$perc <- bag$freq/frequencies$freq[rownames(frequencies) == w]  # computes percentages
        word <- w
        phrase <- w
        for (j in bag$Row.names) {
            if (bag$perc[bag$Row.names == j] >= perc){              # drops sparse terms
            phrase <- c(phrase, j)
            }
        }    
        phrase <- paste(phrase, collapse = " ")                    # makes phrase elements
        newrow <- c(word, phrase) 
        phrase.list <- rbind(phrase.list, newrow)
        print(w)
        print(bag)
        print(phrase)
    }
    phrase.list <- phrase.list[-1, ]
    enc2utf8(as(phrase.list$word, "character"))
    enc2utf8(as(phrase.list$phrase, "character"))
    write.table(phrase.list$phrase, "data/processed/raw_phrases_150121.txt", sep="\t", quote = FALSE, 
                row.names = FALSE, fileEncoding = "UTF-8")
    write.table(phrase.list, "data/processed/data_phrases_150121.txt", sep="\t", quote = FALSE, 
                row.names = FALSE, fileEncoding = "UTF-8")
}

common.words <- rownames(frequencies)[1:200]          # gets 100 most common terms in text
PhrasePrimer(common.words, corr = 0.1, perc = 0.15)   # applies function to distill phrase elements


# A couple of useful fuctions to explore other terms, their clusters and frequencies

FrequencyFinder <-function(term) {                             # produces frequency of word
    f <- frequencies$freq[rownames(frequencies) == term]
    print(paste0(term, " appears ", f, " times"))
}

# a function to visualize relevant information on ngram cluster

ClusterFinder <- function(wordlist, corr = 0.1, perc = 0.1) { 
    word <- NA
    phrase <- NA
    phrase.list <- data.frame(word, phrase)    
    for (w in wordlist) {
        correlates <- as.data.frame(findAssocs(tdm, w, corr))   # gets list of correlated ngrams
        bag <- merge(correlates, frequencies, by = "row.names")    # creates dataframe w corr/freq    
        bag$perc <- bag$freq/frequencies$freq[rownames(frequencies) == w]  # computes percentages
        word <- w
        phrase <- w
        for (j in bag$Row.names) {
            if (bag$perc[bag$Row.names == j] > perc){              # drops sparse terms
                phrase <- c(phrase, j)
            }
        }    
        phrase <- paste(phrase, collapse = " ")                    # makes phrase elements
        newrow <- c(word, phrase) 
        phrase.list <- rbind(phrase.list, newrow)
        f <- frequencies$freq[rownames(frequencies) == wordlist]
        print(paste0(w, " appears ", f, " times"))
        print(bag)
        print(paste0("cluster: ", phrase))
    }
}

# AN INITIAL LOOK INTO NGRAMS
# BIGRAMS 

# 1) Generate bigrams from the original NLP-processed text

options(mc.cores=1)                 # Sets the default number of threads to use
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
BigramTDM <- TermDocumentMatrix(text, control = list(tokenize = BigramTokenizer))
nDocs(BigramTDM)                               # verifies number of unique documents (postcards)
nTerms(BigramTDM)                              # provides number of unique bigrams 
inspect(BigramTDM[1:10, 1:10])                 # inspection of sample of matrix

# 2) A closer look at most frequent bigrams

Bi.frequencies <- rowSums(as.matrix(BigramTDM))
names(Bi.frequencies) <- rownames(as.matrix(BigramTDM))
Bi.frequencies <- sort(Bi.frequencies, decreasing=T)
Bi.frequencies <- as.data.frame(Bi.frequencies)
names(Bi.frequencies) <- c("freq")
Bi.frequencies[1:10, "freq", drop=FALSE]    # shows 150 most common words


BigramClusterFinder <- function(wordlist, corr = 0.1, perc = 0.1) { 
    word <- NA
    phrase <- NA
    phrase.list <- data.frame(word, phrase)    
    for (w in wordlist) {
        Bi.correlates <- as.data.frame(findAssocs(BigramTDM, w, corr))   
        bag <- merge(Bi.correlates, Bi.frequencies, by = "row.names")    
        bag$perc <- bag$freq/Bi.frequencies$freq[rownames(Bi.frequencies) == w]  
        word <- w
        phrase <- w
        for (j in bag$Row.names) {
            if (bag$perc[bag$Row.names == j] >= perc){              # drops sparse terms
                phrase <- c(phrase, j)
            }
        }    
        phrase <- paste(phrase, collapse = " ")                    # makes phrase elements
        newrow <- c(word, phrase) 
        phrase.list <- rbind(phrase.list, newrow)
        f <- Bi.frequencies$freq[rownames(Bi.frequencies) == wordlist]
        print(paste0(w, " appears ", f, " times"))
        print(bag)
        print(paste0("cluster: ", phrase))
    }
}

common.bigrams <- rownames(Bi.frequencies)[1:10]
BigramClusterFinder("mexicanos mexicanos", corr = 0.2, perc = 0.1)


# CREATES WORDCLOUDS
# first, the full wordcloud for words appearing at least 100 times

setwd("~/Documents/#YaMeCanse_postales/graphs")
pdf("wordcloud_all.pdf")
png("wordcloud_all.png", width=5.25,height=5.25,units="in",res=1200)
#jpeg("wordcloud_all.jpeg", width=5.25,height=5.25,units="in",res=1200)
set.seed(1235)
wordcloud(text, scale=c(4.5,0.6), min.freq=90, random.order=FALSE,
          rot.per=0.35, use.r.layout=TRUE, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

# then to visualize clouds for the most common words, a function is defined to 
# plot wordclouds for each word and those words most related to it

smallClouds <- function(word) {
    correlates <- as.data.frame(findAssocs(tdm, word, 0.1))           # gets list of correlated words
    newrow = c(1)                                                    
    correlates = rbind(correlates, newrow)
    rownames(correlates)[nrow(correlates)] <- c(word)                 # adds main word to dataframe 
    names(correlates) <- c("corr")                                    
    idx <- merge(correlates, frequencies, by = "row.names")           # gets subset corr/freqs
    setwd("~/Documents/#YaMeCanse_postales/graphs")
    pdf(paste(word,".pdf", sep = ""))
    wordcloud(idx$Row.names,idx$freq, 
              scale=c(3,0.5), max.words=50, random.order=FALSE, 
              rot.per=0.35, use.r.layout=FALSE, 
              colors=brewer.pal(8, "Dark2"))                          # creates wordcloud
    dev.off()
}

for (i in FreqTerms) {
    smallClouds(i)
}

# finally, an example of how a wordcloud for bigrams would look like

wordcloud(rownames(Bi.frequencies), Bi.frequencies$freq, scale=c(2.5,0.6), 
          min.freq=20, random.order=FALSE,
          rot.per=0.35, use.r.layout=TRUE, 
          colors=brewer.pal(8, "Dark2"))

