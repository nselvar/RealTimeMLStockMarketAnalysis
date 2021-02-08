#Twitter Stock Market Sentimental Analysis
#https://github.com/AmoghKatwe/Stock-Market-Sentiment-Analysis
rm(list=ls());
cat("\014")

#Libraries
install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("SnowballC")

library("twitteR")
library("ROAuth")
library(tm)
library(wordcloud)
library(SnowballC)
library(stringr)

t.api.key <- "hxpKi79OMfIcEJXpuZpq0xpKP"
t.api.secret <-"dVn11zXzWhjfZ06sZKqJMliwsNaNT8AzKIcoMvPT8b39OWCL4p"

setup_twitter_oauth(t.api.key, t.api.secret, access_token = 
"762626485-ngcTFHX6wAUcMFCFANUS0Lr4GKmwCwmdDA4VSssz", access_secret = "8D2IkCtKz1zCAKKjl8Pcnmsa0RPe7hCdDcx4wGOxldYO8")  
save(list = (c("t.api.key", "t.api.secret")), file="twitter_credentials")

#Part A Search for the 100 tweets 
#Gainer stocks are WINS, NAV and PTLA

#For WINS
tweets_WINS <- searchTwitter('$ROKU', n = 5000)
display.tweet<- function (tweet) 
{
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}


#Display 100 Tweets 
for (t in tweets_WINS) 
{
  display.tweet(t)
}

#For NAV
tweets_NAV <- searchTwitter('$NET', n = 5000)
display.tweet <- function (tweet) 
{
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}


#Display 100 Tweets 
for (t in tweets_NAV) 
{
  display.tweet(t)
}

#For PTLA
tweets_PTLA <- searchTwitter('$UBER', n = 5000) 
display.tweet <- function (tweet)
{
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}


#Display 100 Tweets 
for (t in tweets_PTLA) 
{
  display.tweet(t)
}

#Loser stocks are UBIA, FDS and SLCA

#For UBIA
tweets_UBIA <- searchTwitter('$EA', n = 5000) 
display.tweet <- function (tweet) 
{
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}


#Display 100 Tweets 
for (t in tweets_UBIA) 
{
  display.tweet(t)
}


#For FDS
tweets_FDS <- searchTwitter('$TWLO', n = 5000) 
display.tweet <- function (tweet) 
{
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}


#Display 100 Tweets 
for (t in tweets_FDS) 
{
  display.tweet(t)
}


#For SLCA
tweets_SLCA <- searchTwitter('$CCEP', n = 5000)
display.tweet <- function (tweet) 
{
  cat("Screen name:", tweet$getScreenName(), "\nText:", tweet$getText(), "\n\n")
}


#Display 100 Tweets 
for (t in tweets_SLCA) 
{
  display.tweet(t)
}

#Combine all the gainers into a set of 300 tweets
tweets_gainers <- c(tweets_WINS, tweets_NAV, tweets_PTLA)
tweets_gainers

str(tweets_gainers)
#Combine all the losers into a set of 300 tweets
tweets_losers <- c(tweets_UBIA, tweets_FDS, tweets_SLCA)
tweets_losers


#Part B
#Create Corpus for Gainer Tweets
tweets_gainers_text <- lapply(tweets_gainers, function(t) {t$getText()})
tweets_gainers_source <- VectorSource(tweets_gainers_text)

data.corpus1 <- Corpus(tweets_gainers_source)
data.corpus1 <- tm_map(data.corpus1, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
data.corpus1

#Writing Corpus
writeCorpus(data.corpus1,'/Users/nselvarajan/Desktop/R/corpus/')


#Create Corpus for Loser Tweets
tweets_losers_text <- lapply(tweets_losers, function(t) {t$getText()})
tweets_losers_source <- VectorSource(tweets_losers_text)

data.corpus2 <- Corpus(tweets_losers_source)
data.corpus2 <- tm_map(data.corpus2, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
data.corpus2

#Writing Corpus
writeCorpus(data.corpus2,'/Users/nselvarajan/Desktop/R/corpus/')

#Part C Preprocessing

#Gainer Stock Data Corpus 1
getTransformations()
content_transformer()


data.corpus1 <- tm_map(data.corpus1, content_transformer(tolower))
removeURL <- function(x){gsub("(http[^ ]*)","",x)}

data.corpus1 <- tm_map(data.corpus1, content_transformer(removeURL))
data.corpus1 <- tm_map(data.corpus1, content_transformer(removePunctuation))
english.stopwords <- stopwords("en")

data.corpus1 <- tm_map(data.corpus1, content_transformer(removeWords), english.stopwords)
removeNumberWords <- function(x){gsub("([[:digit:]]+)([[:alnum:]])*","", x)}

data.corpus1 <- tm_map(data.corpus1, content_transformer(removeNumberWords))
data.corpus1 <- tm_map(data.corpus1, content_transformer(stemDocument))
data.corpus1 <- tm_map(data.corpus1, content_transformer(stripWhitespace))
inspect(data.corpus1[1:2])


#Loser Stock Data Corpus 2
getTransformations()
content_transformer()

data.corpus2 <- tm_map(data.corpus2, content_transformer(tolower))
removeURL <- function(x){gsub("(http[^ ]*)","",x)}

data.corpus2 <- tm_map(data.corpus2, content_transformer(removeURL))
data.corpus2 <- tm_map(data.corpus2, content_transformer(removePunctuation))
english.stopwords <- stopwords("en")

data.corpus2 <- tm_map(data.corpus2, content_transformer(removeWords), english.stopwords)
removeNumberWords <- function(x){gsub("([[:digit:]]+)([[:alnum:]])*","",x)}
data.corpus2 <- tm_map(data.corpus2, content_transformer(removeNumberWords))
data.corpus2 <- tm_map(data.corpus2, content_transformer(stemDocument))
data.corpus2 <- tm_map(data.corpus2, content_transformer(stripWhitespace))
inspect(data.corpus2[1:2])


#Part D Term Document Matrix 
#For gainers
tdm1 <- TermDocumentMatrix(data.corpus1)
tdm1

#Save the term document matrix
mat_tdm1 <- as.matrix(tdm1)
write.csv(mat_tdm1, file = "tdm1.csv")


#For losers
tdm2 <- TermDocumentMatrix(data.corpus2)
tdm2

mat_tdm2 <- as.matrix(tdm2)
write.csv(mat_tdm2, file = "tdm2.csv")


#Part E Frequent Terms 

#For gainers
findFreqTerms(tdm1, lowfreq = 25)
matrix_gainers <- as.matrix(tdm1)
wordFreq_gainers <- rowSums(matrix_gainers)
wordFreq_gainers <- sort(wordFreq_gainers, decreasing = TRUE)
cbind(wordFreq_gainers[1:10])

#Wordcloud for Gainers
palette <- brewer.pal(8, "Dark2")
set.seed(137)
wordcloud(words = names(wordFreq_gainers), freq = wordFreq_gainers, min.freq=25, random.order = F, colors = palette )



#For losers
findFreqTerms(tdm2, lowfreq = 25)
matrix_losers <- as.matrix(tdm2)
wordFreq_losers <- rowSums(matrix_losers)
wordFreq_losers <- sort(wordFreq_losers, decreasing = TRUE)
cbind(wordFreq_losers[1:10])

#Wordcloud for Losers
palette <- brewer.pal(8, "Dark2")
set.seed(137)
wordcloud(words = names(wordFreq_losers), freq=wordFreq_losers, min.freq=25, random.order = F, colors = palette )


#Part F Compute the sentiment score

#For Gainers and Losers
getwd()
setwd("/Users/nselvarajan/Desktop/R/finals/")
dir()

pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

sentiment <- function(text, pos.words, neg.words)
{
  text <- gsub('[[:punct:]]', '',text)
  text <- gsub('[[:cntrl:]]', '',text)
  text <- gsub('\\d+', '',text)
  text <- tolower(text)
  
  #Split the text into a vector of words  
  words <- strsplit(text,'\\s+')
  words <- unlist(words)
  
  #Find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  
  #Find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  
  #Calculate the sentiment score
  score_return <- sum(pos.matches) - sum(neg.matches)
  cat("Positive:", words[pos.matches], "\n")
  cat("Negative:", words[neg.matches], "\n")
  return(score_return)
}

sentiment(names(wordFreq_gainers), pos.words, neg.words)
sentiment(names(wordFreq_losers), pos.words, neg.words)


