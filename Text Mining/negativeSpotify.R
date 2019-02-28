install.packages(c('tidytext', 'dplyr', 'ggplot2','tm','NLP',"e1071", "caret", "quanteda", "irlba", "randomForest"))
install.packages("wordcloud")
install.packages("RColorBrewer")
library(ggplot2)
library(tidytext) 
library(dplyr)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
install.packages("stringi")
library(stringi)
install.packages("topicmodels")
install.packages("quanteda")
library(topicmodels)
library(quanteda)
install.packages('janeaustenr')
library(janeaustenr)
library(tidytext)
library(xlsx)
spotifyRev <- read.xlsx("/Users/mona/Desktop/spotifyRev.xlsx",sheetName="spotifyRev", stringsAsFactors = FALSE)

## 1. Cleaning and Preprossing the data ##
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}
# fix (expand) contractions
reviews <- sapply(spotifyRev$Review, fix.contractions)

#Concatenate vectors after converting to character.
reviews <- paste(spotifyRev$Review, collapse=" ")
#Create a vector source
reviews_source <- VectorSource(reviews)
# Create volatile corpora.
reviews_corpus <- VCorpus(reviews_source)
# Convert the text to lower case
reviews_corpus<- tm_map(reviews_corpus, content_transformer(tolower))
# Remove numbers
reviews_corpus <- tm_map(reviews_corpus, removeNumbers)
# Remove punctuations
reviews_corpus<- tm_map(reviews_corpus, removePunctuation)
# Eliminate extra white spaces
reviews_corpus <- tm_map(reviews_corpus,stripWhitespace)
# Remove english common stopwords
reviews_corpus <- tm_map(reviews_corpus, removeWords, stopwords("english"))
reviews_corpus <- tm_map(reviews_corpus, removeWords, c("will", "also")) 
#Stem document
reviews_corpus<- tm_map(reviews_corpus,stemDocument)
# Create Term-Document Matrixdtm
dtm<- DocumentTermMatrix(reviews_corpus)
# to get the frequency of occurrence of each word in the corpus, we simply sum over all rows to give column sums
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)
#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]
#inspect least frequently occurring terms
freq[tail(ord)]
findFreqTerms(dtm,lowfreq=100)
#Setting a seed number ensures that you get the same look each time
set.seed(30)
#wordcloud
wordcloud(names(freq),freq,min.freq=10,colors=brewer.pal(6,"Dark2"))


#Try1

tdm <- TermDocumentMatrix(reviews_corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)
# Plot the word cloud
set.seed(1234)
wordcloud(d$word,d$freq, min.freq= 70,
          random.order=FALSE, rot.per=0.35, 
          use.r.layout=FALSE, colors=brewer.pal(6,"Dark2"))

#You could also analyze the correlation (or association) between frequent terms.
#The R code below identifies which words are associated with "want" , "Feature"

findAssocs(tdm, terms = "", corlimit = 0.0)
