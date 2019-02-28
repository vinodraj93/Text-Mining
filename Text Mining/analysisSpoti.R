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
install.packages("plotrix")
library(plotrix)
PosRev <- read.xlsx("/Users/mona/Desktop/spotifyRev.xlsx",sheetName="positive", stringsAsFactors = FALSE)
negsRev <- read.xlsx("/Users/mona/Desktop/spotifyRev.xlsx",sheetName="negative", stringsAsFactors = FALSE)



## Combine both corpora: all reviews
all_yes <- paste(PosRev$Review, collapse = "")
all_no <- paste(negsRev$Review, collapse = "")
all_combine <- c(all_yes, all_no)
## Creating corpus for combination
corpus_review_all=Corpus(VectorSource(all_combine)) 


## Pre-processing corpus - all
#Convert to lower-case
corpus_review_all<- tm_map(corpus_review_all, tolower)
#Remove punctuation
corpus_review_all<- tm_map(corpus_review_all, removePunctuation)
#Remove stopwords
corpus_review_all<- tm_map(corpus_review_all, removeWords, stopwords("english"))
corpus_review_all<-  tm_map(corpus_review_all, removeWords,c("also", "get","app","spotifi","music",
"company", "made", "can", "im", "dress","just","i","let","dont","cant","pandora","doesnt","kindl","well","want","wouldnt","kindl","often"))
#Stem document
corpus_review_all<- tm_map(corpus_review_all, stemDocument)
review_tdm_all <- TermDocumentMatrix(corpus_review_all)
all_m=as.matrix(review_tdm_all)
colnames(all_m)=c("Positive","Negative")
#Sum rows and frequency data frame
review_term_freq_all <- rowSums(all_m)
review_word_freq_all <- data.frame(term=names(review_term_freq_all), num = review_term_freq_all)
#Make commonality cloud
commonality.cloud(all_m, 
                  colors = brewer.pal(6,"Dark2"),
                  max.words = 100)
# Create comparison cloud
comparison.cloud(all_m,
                 colors = c("green", "red"),
                 max.words = 100)
findAssocs(review_tdm_all, terms = "great", corlimit = .90)



# Identify terms shared by both documents
common_words <- subset(all_m, all_m[, 1] > 0 & all_m[, 2] > 0)
# calculate common words and difference
difference <- abs(common_words[, 1] - common_words[, 2])
common_words <- cbind(common_words, difference)
common_words <- common_words[order(common_words[, 3],
                                   decreasing = T), ]
head(common_words)

review_tdm2 <- removeSparseTerms(review_tdm_all, sparse = 0.01)
hc <- hclust(d = dist(review_tdm2, method = "euclidean"), method = "complete")
# Plot a dendrogram
plot(hc)

# Create associations
associations <- findAssocs(review_tdm_all, "selection", 0.05)
# Create associations_df
str(associations)

