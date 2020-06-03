#install.packages("ggplot2")
#install.packages("tm")
#install.packages("wordcloud")
#install.packages("syuzhet")
#install.packages("SnowballC")
#install.packages("openNLP")
#install.packages("NLP")
#install.packages("RColorBrewer")
#install.packages("tidyverse")
#install.packages("tibble")
library(tidyverse)
library(tibble)
library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)
library(NLP)
library(openNLP)
library(RColorBrewer)

texts <- tibble(readLines("location/chat.txt"))
docs <- VCorpus(VectorSource(texts))
trans <- content_transformer(function(x, pattern) gsub(pattern, "", x))

#Clean-up
docs <- tm_map(docs, trans, "/")
docs <- tm_map(docs, trans, "@")
docs <- tm_map(docs, trans, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

#Converting to TermDocument Matrix
dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat), decreasing = T) #Converting to Dataframes
d <- data.frame(word = names(v), freq = v)
set.seed(1056)

#Constructing wordcloud
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 200, random.order = F, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

#Analysing Sentiment in text
sentiment <- (get_nrc_sentiment(as.character(texts)))
text <- cbind(texts, sentiment)
TotalSentiment <- data.frame(colSums(text[, c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
print(TotalSentiment)
rownames(TotalSentiment) <- NULL

#Bar Chart
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("sentiment") +
  ylab("TotalCount") +
  ggtitle("Total Sentiment Score")



