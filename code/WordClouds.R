#  ####################################################################### 
#       File-Name:      WordClouds.R
#       Version:        R 3.1.2
#       Date:           Dec 15, 2014
#       Author:         MM
#       Purpose:        Process postcard text using NLP tools to distill  
#                       phrases that syntesize proposals, and creates 
#                       WordCloud graphs for 
#                       #YaMeCansePorEsoPropongo data
#       Input Files:    Textos al 11 de enero de 2015.csv                         
#       Output Files:   wordcloud_all.pdf
#       Data Output:    raw_phrases.txt
#                       data_phrases.txt
#       Previous files: NONE
#       Dependencies:   NONE
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

setwd("~/Documents/#YaMeCanse_postales")
getwd()

# NOTE: data must be entered in UTF-16 format so that accented letters can be properly read. 
#       Also, file is tab-delimited text to avoid confusions with commas in the text

data <- as.data.table(read.delim(
    "data/Textos al 11 de enero de 2015.csv",
    header=TRUE, sep="\t", na.strings = "NA", fileEncoding = "UTF-16"))
names(data)                                 # verifies all variables are loaded properly
length(data$ID)                             # verifies that no observations were lost
head(data)                                  # checks for possible errors in matrix
#data[, unique.id:=.GRP, by=data$Customer]   # creates unique user identifier

# PROCESSING DATA WITH NLP TOOLS

proposals <- data$Body[!is.na(data$Body)]      # eliminates empty postcards
proposals <- proposals[!duplicated(proposals)] # eliminates duplicates 
length(proposals)                              # verifies how many lines were lost

text <- VCorpus(VectorSource(proposals), readerControl= list(language = "spanish"))
inspect(text[1:10])

# add needed words to stopwords
words = c(stopwords("spanish"), "#", "yamecanse", "yamecansé", "yamecansepor", "poresopropongo", 
          "yamecanseporesopropongo", "yamecanséporesopropongo", "por", "eso", "propongo", "que", 
          "canse", "cansé", "ser", "así", "asi", "cada", "mas", "solo", "cualquier", "sólo", "etc", 
          "yamecansè", "yamecansépor", "tan", "yame", "yamecanséde", "yamecansédetener", 
          "yamécansépor", "yamecanséyporesopropongo", "yamecase", "yopropongoquien")

# transform text in corpus to usable bits
text <- tm_map(text, content_transformer(tolower), mc.cores=1) # makes lowercase
text <- tm_map(text, removeNumbers)                            # removes numbers
text <- tm_map(text, removePunctuation)                        # removes punctuation
text <- tm_map(text, removeWords, words, lazy=TRUE)            # removes useless words
text <- tm_map(text, stripWhitespace)                          # removes white space
# text.copy <- text # creates copy to be used as dictionary
# text <- tm_map(text, stemDocument, language ="spanish")      # stems text
# text <- tm_map(text, stemCompletion, dictionary = text.copy ) # completes stems w/most freq word
inspect(text[1:10])

# perform manual replacements of text as a quicker alternative to stemming/completing 
ReplaceText <- content_transformer(function(x, from, to) gsub(from, to, x))
text <- tm_map(text, ReplaceText, "derechos", "derecho")
text <- tm_map(text, ReplaceText, "familias", "familia")
text <- tm_map(text, ReplaceText, "politicos", "políticos")
text <- tm_map(text, ReplaceText, "publicos", "públicos")
text <- tm_map(text, ReplaceText, "publico", "público")
text <- tm_map(text, ReplaceText, "salarios", "salario")
text <- tm_map(text, ReplaceText, "sueldo", "salario")
text <- tm_map(text, ReplaceText, "sueldos", "salario")
text <- tm_map(text, ReplaceText, "mexico", "méxico")
text <- tm_map(text, ReplaceText, "país", "méxico")
text <- tm_map(text, ReplaceText, "pais", "país")
text <- tm_map(text, ReplaceText, "funcionarios", "funcionario")
text <- tm_map(text, ReplaceText, "funcionarioes", "funcionario")
text <- tm_map(text, ReplaceText, "servidor", "funcionario")
text <- tm_map(text, ReplaceText, "servidores", "funcionario")
text <- tm_map(text, ReplaceText, "presupuesto", "dinero")
text <- tm_map(text, ReplaceText, "recursos", "dinero")
text <- tm_map(text, ReplaceText, "dineros", "dinero")
text <- tm_map(text, ReplaceText, "ciudadanos", "mexicanos")
text <- tm_map(text, ReplaceText, "ciudadano", "mexicanos")
text <- tm_map(text, ReplaceText, "gente", "mexicanos")
text <- tm_map(text, ReplaceText, "personas", "mexicanos")
text <- tm_map(text, ReplaceText, "pueblo", "mexicanos")
text <- tm_map(text, ReplaceText, "sociedad", "mexicanos")
text <- tm_map(text, ReplaceText, "ciudadanía", "mexicanos")
text <- tm_map(text, ReplaceText, "año", "años")
text <- tm_map(text, ReplaceText, "añosss", "años")
text <- tm_map(text, ReplaceText, "creación", "crear")
text <- tm_map(text, ReplaceText, "cree", "crear")
text <- tm_map(text, ReplaceText, "cumplan", "cumplir")
text <- tm_map(text, ReplaceText, "disminución", "disminuir")
text <- tm_map(text, ReplaceText, "evaluaciones", "evaluación")
text <- tm_map(text, ReplaceText, "deben", "debe")
text <- tm_map(text, ReplaceText, "hagan", "hacer")
text <- tm_map(text, ReplaceText, "epn", "EPN")
text <- tm_map(text, ReplaceText, "enrique peña nieto", "EPN")
text <- tm_map(text, ReplaceText, "enrique pena nieto", "EPN")
text <- tm_map(text, ReplaceText, "peña nieto", "EPN")
text <- tm_map(text, ReplaceText, "pena nieto", "EPN")
text <- tm_map(text, ReplaceText, "enrique peña", "EPN")
text <- tm_map(text, ReplaceText, "enrique pena", "EPN")
text <- tm_map(text, ReplaceText, "peña", "EPN")
#text <- tm_map(text, ReplaceText, "enrique", "EPN")
#text <- tm_map(text, ReplaceText, "peña", "EPN")
#text <- tm_map(text, ReplaceText, "nieto", "EPN")
#text <- tm_map(text, ReplaceText, "enrique", "EPN")
#text <- tm_map(text, ReplaceText, "peña", "EPN")
#text <- tm_map(text, ReplaceText, "nieto", "EPN")
inspect(text[1:10])

# CREATE TERM DOCUMENT MATRIX TO EXPLORE WORD FREQUENCIES

tdm <- TermDocumentMatrix(text)          # creates matrix
FreqTerms <- findFreqTerms(tdm, 50)      # identify most common words in dataset
# dict <- unique(rownames(as.matrix(tdm)))   # creates dictionary of unique words

# creates a file with (sorted) word frequencies 

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

# Create a function that distill phrase elements related to most common words with 
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
            if (bag$perc[bag$Row.names == j] > perc){              # drops sparse terms
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
    write.table(phrase.list$phrase, "data/raw_phrases.txt", sep="\t", quote = FALSE, 
                row.names = FALSE, fileEncoding = "UTF-8")
    write.table(phrase.list, "data/data_phrases.txt", sep="\t", quote = FALSE, 
                row.names = FALSE, fileEncoding = "UTF-8")
}

common.words <- rownames(frequencies)[1:100]          # gets 100 most common terms in text
PhrasePrimer(common.words, corr = 0.1, perc = 0.15)   # applies function to distill phrase elements


# CREATES WORDCLOUDS
# first, the full wordcloud for words appearing at least 100 times

setwd("~/Documents/#YaMeCanse_postales/graphs")
pdf("wordcloud_all.pdf")
jpeg("wordcloud_all.jpeg", quality = 100)
png("wordcloud_all.png")
wordcloud(text, scale=c(3,0.3), min.freq=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, 
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

plot(tdm, terms =findFreqTerms(dtm, lowfreq =100)[1:50],corThreshold=0.5)
