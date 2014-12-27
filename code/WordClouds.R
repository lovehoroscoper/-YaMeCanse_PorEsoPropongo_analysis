#  ####################################################################### 
#       File-Name:      WordClouds.R
#       R version:      3.1.2
#       Date:           Dec 15, 2014
#       Author:         MM
#       Purpose:        Code to start generating WordCloud graphs for 
#                       #YaMeCansePorEsoPropongo data
#       Input Files:    Postcard Dashboard  Orders - Movement Postcards_converted.txt                         
#       Output Files:   NONE
#       Data Output:    NONE
#       Previous files: NONE
#       Dependencies:   NONE
#       Status:         COMPLETED
#       Machine:        Mac
#  ####################################################################### 

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
    "data/Postcard Dashboard  Orders - Movement Postcards_converted (utf16).txt", 
    header=TRUE, sep="\t", na.strings = "NA", fileEncoding = "UTF-16"))
names(data)                                 # verifies all variables are loaded properly
length(data$ID)                             # verifies that no observations were lost
data[, unique.id:=.GRP, by=data$Customer]   # creates unique user identifier

# PROCESSING DATA WITH TEXT MINING

proposals <- data$Body[!is.na(data$Body)]   # eliminates empty postcards
text <- VCorpus(VectorSource(proposals), readerControl= list(language = "spanish"))
inspect(text[1:10])

# add needed words to stopwords
words = c(stopwords("spanish"), "#", "yamecanse", "yamecansé", "yamecansepor", "poresopropongo", 
          "yamecanseporesopropongo", "yamecanséporesopropongo", "por", "eso", "propongo", "que", 
          "canse", "cansé", "ser", "así", "cada", "mas", "solo", "cualquier", "sólo", "etc", 
          "yamecansè", "yamecansépor", "tan", "yame", "yamecanséde", "yamecansédetener", 
          "yamécansépor", "yamecanséyporesopropongo", "yamecase", "yopropongoquien")

# transform text in corpus to usable bits
text <- tm_map(text, content_transformer(tolower), mc.cores=1)
text <- tm_map(text, removeNumbers)
text <- tm_map(text, removePunctuation)
text <- tm_map(text, removeWords, words, lazy=TRUE)
text <- tm_map(text, stripWhitespace)
inspect(text[1:10])

# perform manual replacements of text
ReplaceText <- content_transformer(function(x, from, to) gsub(from, to, x))
text <- tm_map(text, ReplaceText, "derechos", "derecho")
text <- tm_map(text, ReplaceText, "familias", "familia")
text <- tm_map(text, ReplaceText, "politicos", "políticos")
text <- tm_map(text, ReplaceText, "publicos", "públicos")
text <- tm_map(text, ReplaceText, "salarios", "salario")
text <- tm_map(text, ReplaceText, "sueldo", "salario")
text <- tm_map(text, ReplaceText, "sueldos", "salario")
text <- tm_map(text, ReplaceText, "país", "méxico")
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
removeSparseTerms(tdm, 0.4)              # removes sparse terms

# creates a file with (sorted) word frequencies 

frequencies <- rowSums(as.matrix(tdm))
names(frequencies) <- rownames(as.matrix(tdm))
frequencies <- sort(frequencies, decreasing=T)
frequencies <- as.data.frame(frequencies)
names(frequencies) <- c("freq")
# write.csv(frequencies, file="data/word_frequencies.csv", fileEncoding = "UTF-16")


# CREATES WORDCLOUDS

smallClouds <- function(word) {
    correlates <- as.data.frame(findAssocs(tdm, word, 0.3))   # gets list of correlated words
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



# first, the full wordcloud
setwd("~/Documents/#YaMeCanse_postales/graphs")
pdf("wordcloud_all.pdf")
wordcloud(text, scale=c(3,0.01), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))
dev.off()

# then for the most common words

for (i in FreqTerms) {
    smallClouds(i)
}

