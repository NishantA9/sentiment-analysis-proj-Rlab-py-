

#used Amazon Reviews Exporter in Google Chrome to extract Reviews:
#chrome-extension://njlppnciolcibljfdobcefcngiampidm/welcome.html
#
#importing file into r


#LOAD PACKAGES in R
library(tm) #text analytics - text mining 
library(wordcloud) #create wordcloud
library(syuzhet)

#library(lubridate)
#library(ggplot2)
#library(scales)
#library(reshape2)
#library(dplyr)

#import data in R
reviews <- read.csv(file.choose(), header = T)

#check the structure of file
str(reviews)

#creating corpus
#this function uses the base package function iconv to translate value into a specified format
corpus <- iconv(reviews$text)
corpus <- Corpus(VectorSource(corpus))

#to see the corpus
inspect(corpus[1:5])

#cleaning corpus
corpus <- tm_map(corpus, tolower)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
#inspect(corpus[1:5])

corpus <- tm_map(corpus, removeWords, stopwords("english"))
#inspect(corpus[1:5])

#remove some common words not to used in text analysis - replace word 1. word 2 by actual words
corpus <- tm_map(corpus, removeWords, c("book","read","life"))
#inspect(corpus[1:5])

corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

reviews_final <- corpus


#create term document
tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:10, 1:5]




#Bar plot of words
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w, las = 2, col = "blue")


#create word cloud
w <- sort(rowSums(tdm), decreasing = T)
set.seed(2000)
wordcloud(words = names(w),
          freq = w,
          max.words = 50,
          random.order = T,
          min.freq = 5,
          colors = brewer.pal(25,"Dark2"),
          scale = c(3,0.3))




#obtain sentiment scores
sentiment_data <- iconv(reviews$text)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

#calculate review wise score
s$score <- s$positive - s$negative
s[1:10,]

#write scores into a csv file
write.csv(x = s, file = "C:/Users/FireEmperorYT/Documents/R/proj2 amazon/final_scores.csv")





#check product sentiment

#check overall sentiment of the product
review_Score <- colSums(s[,])
print(review_Score)


#plot product sentiment

#bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment')