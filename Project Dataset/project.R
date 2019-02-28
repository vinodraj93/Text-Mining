
#install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest"))
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages(c("tidytext","dplyr"))
#install.packages(c("tidyverse","stringr"))
#install.packages('stringdist')
#install.packages('NLP')
library(tidytext)
library(dplyr)
library(RColorBrewer)
library(tm)
library(NLP)
library(tidyverse)
library(wordcloud)
library(stringr)
setwd("C:\\Users\\Vinod\\Desktop\\MIS\\Project")
amazon_review <- read.csv("amazon.csv", stringsAsFactors = FALSE)
names(amazon_review) <- c("Date", "Review")
with_reviews <- length(which(complete.cases(amazon_review)))
with_reviews
without_reviews <- length(which(!complete.cases(amazon_review)))
without_reviews
amazon_text <- paste(amazon_review$Review, collapse=" ")

amazon_source <- VectorSource(amazon_text)
amazon_corpus <- VCorpus(amazon_source)
amazon_corpus <- tm_map(amazon_corpus, content_transformer(tolower))
amazon_corpus <- tm_map(amazon_corpus, removePunctuation)
amazon_corpus <- tm_map(amazon_corpus,stripWhitespace)
amazon_corpus <- tm_map(amazon_corpus, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(amazon_corpus)
dtm2 <- as.matrix(dtm)
v <- sort(colSums(dtm2), decreasing = TRUE )
d <- data.frame(word = names(v), freq=v)


split1 <- str_split(v, pattern = "\\s+")
class(split1)
split1 <- unlist(split1)

str(split1)

poswords <- scan('pos.txt',what='character', comment.char=";")
#a <- lapply(v, function(v) poswords[match(poswords, split1)])
#b <- str_extract(v, paste(poswords, collapse = "|"))


str(poswords)
negwords <- scan('neg.txt',what='character', comment.char=";")
str(negwords)
positive <- match(split1, poswords)
positive <- str_extract(split1, paste(positive, collapse = "|"))
negative <- match(split1, negwords)
positive

#contains(split1, poswords)
sum(is.na(positive))
sum(is.na(Negative))
wordlist <- sort(colSums(dtm2))
word = names(v)
freq=v
wordcloud(word[1:100], freq[1:100])
barplot(d[5:15,]$freq, las = 2, names.arg = d[5:15,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
#install.packages(c("ggplot2", "e1071", "caret", "quanteda", "irlba", "randomForest"))
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages(c("tidytext","dplyr"))
#install.packages(c("tidyverse","stringr"))
#install.packages('stringdist')
#install.packages('NLP')
library(tidytext)
library(dplyr)
library(RColorBrewer)
library(tm)
library(NLP)
library(tidyverse)
library(wordcloud)
library(stringr)
setwd("C:\\Users\\Vinod\\Desktop\\MIS\\Project")
amazon_review <- read.csv("amazon.csv", stringsAsFactors = FALSE)
names(amazon_review) <- c("Date", "Review")
#amazon_text <- paste(amazon_review$Review, collapse=" ")
amazon_text <- c(amazon_review$Review)
#amazon_text <- str_split(amazon_review$Review, pattern = "\\s+")
class(amazon_text)
amazon_text<-as.list(amazon_text)
class(amazon_text)
#amazon_text <- as.matrix(amazon_text)

sentiments

#for (i in 1:5){
# afinn <- get_sentiments("afinn")
#bing = data.frame(bing$word,bing$sentiment)
#afinn <- as.matrix(afinn)
#afinnpos <- c(which(afinn[,2]<0))
#afinnposwords <- afinn[(afinnpos),1]

#amazon_text <- as.list(str_split(amazon_text[i], pattern = "\\s+"))
#amazon_source <- VectorSource(amazon_text)
#amazon_corpus <- VCorpus(amazon_source)
#match <- intersect(amazon_corpus,afinnposwords)
#next()
#}
#grep(afinnposwords, amazon_text[1])
#intersect(amazon_text, bingposwords)
#bingposwords <- paste(bingposwords, collapse=" ")
#bingposwords <- str_split(bingposwords, pattern = "\\s+")
bingneg <- c(which(bing[,2]=="negative"))
bingnegwords <- bing[(bingneg),1]
length(bingposwords)

bing <- get_sentiments("bing")
#bing = data.frame(bing$word,bing$sentiment)
bing <- as.matrix(bing)
bingpos <- c(which(bing[,2]=="positive"))
bingposwords <- bing[(bingpos),1]
#bingposwords <- paste(bingposwords, collapse=" ")
#bingposwords <- str_split(bingposwords, pattern = "\\s+")
bingneg <- c(which(bing[,2]=="negative"))
bingnegwords <- bing[(bingneg),1]
length(bingposwords)

nrc <- get_sentiments("nrc")
#nrc = data.frame(nrc$word,nrc$sentiment)
nrc <- as.matrix(nrc)
nrcpos <- c(which((nrc[,2]=="positive")|(nrc[,2]=="trust")|(nrc[,2]=="joy")))
nrcposwords <- nrc[(nrcpos),1]
nrcneg <- c(which((nrc[,2]=="negative")|(nrc[,2]=="fear")|(nrc[,2]=="anger")|(nrc[,2]=="disgust")|(nrc[,2]=="sadness")))
nrcnegwords <- nrc[(nrcneg),1]


amazon_source <- VectorSource(amazon_text)
amazon_corpus <- VCorpus(amazon_source)
amazon_corpus <- tm_map(amazon_corpus, content_transformer(tolower))
amazon_corpus <- tm_map(amazon_corpus, removePunctuation)
amazon_corpus <- tm_map(amazon_corpus,stripWhitespace)
amazon_corpus <- tm_map(amazon_corpus, removeWords, stopwords("english"))
dtm <- DocumentTermMatrix(amazon_corpus)
dtm2 <- as.matrix(dtm)
amazon_words <- sort(colSums(dtm2), decreasing = TRUE )
frequency <- data.frame(word = names(amazon_words), freq=amazon_words)
class(amazon_text)
class(bingposwords)
class(bingnegwords)
poswords <- scan('pos.txt',what='character', comment.char=";")
class(poswords)

a<-c(intersect(amazon_text, bingposwords))
a
b<-c(intersect(amazon_text, bingnegwords))
b
c<-c(intersect(amazon_text, nrcposwords))
c
d<-c(intersect(amazon_text, nrcnegwords))
d

positive_words <- c(a, c)
positive_words
negative_words <- c(b, d)
negative_words
sum(!is.na(match (amazon_text, bingposwords)))
sum(!is.na(match (amazon_text, nrcposwords)))
sum(!is.na(match (amazon_text, bingnegwords)))
sum(!is.na(match (amazon_text, nrcnegwords)))



#wordcloud(word[1:100], freq[1:100])
wordcloud(positive_words, min.freq = 1, scale = c(2,1))
barplot(d[5:15,]$freq, las = 2, names.arg = d[5:15,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
