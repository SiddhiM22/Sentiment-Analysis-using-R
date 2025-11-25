library("twitteR")
library("syuzhet")
library("tm")
library("SnowballC")
library("ROAuth")
library("wordcloud")
library("ggplot2")

## Twitter authentication
api_key <- ""

api_secret <- ""

access_token <- ""

access_token_secret <- ""

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#********FACEBOOK******#

#extracting tweets
facebook <- searchTwitter("#facebook", n=1000,lang = "en")

#converting into data frame
facebook_tweets <- twListToDF(facebook)
View(facebook_tweets)
facebook_text<- facebook_tweets$text

#text cleaning
facebook_text<- tolower(facebook_text)
facebook_text <- gsub("rt", "", facebook_text)
facebook_text <- gsub("@\\w+", "", facebook_text)
facebook_text <- gsub("[[:punct:]]", "", facebook_text)
facebook_text <- gsub("http\\w+", "", facebook_text)
facebook_text <- gsub("[ |\t]{2,}", "", facebook_text)
facebook_text <- gsub("^ ", "", facebook_text)
facebook_text <- gsub(" $", "", facebook_text)

#creation of feature vector
facebook_tweets.text.corpus <- Corpus(VectorSource(facebook_text))

#removing stopwords that do not contribute in analysing the sentiments
facebook_tweets.text.corpus <- tm_map(facebook_tweets.text.corpus, function(x)removeWords(x,stopwords()))

#generating wordcloud
wordcloud(facebook_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#emotions through built in functions and dictionaries
mysentiment_facebook<-get_nrc_sentiment((facebook_text))

#calculating scored for each sentiment
Sentimentscores_facebook<-data.frame(colSums(mysentiment_facebook[,]))

names(Sentimentscores_facebook)<-"Score"
Sentimentscores_facebook<-cbind("sentiment"=rownames(Sentimentscores_facebook),Sentimentscores_facebook)
rownames(Sentimentscores_facebook)<-NULL

#graphical reprenstation
ggplot(data=Sentimentscores_facebook,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Social Netwoking site FACEBOOK")

#**********AMAZON*******#
#extracting tweets
amazon <- searchTwitter("#amazon", n=1000,lang = "en")

#converting into data frame and viewing
amazon_tweets <- twListToDF(amazon)
View(amazon_tweets)

#text cleaning
amazon_text<- amazon_tweets$text
amazon_text<- tolower(amazon_text)
amazon_text <- gsub("rt", "", amazon_text)
amazon_text <- gsub("@\\w+", "", amazon_text)
amazon_text <- gsub("[[:punct:]]", "", amazon_text)
amazon_text <- gsub("http\\w+", "", amazon_text)
amazon_text <- gsub("[ |\t]{2,}", "", amazon_text)
amazon_text <- gsub("^ ", "", amazon_text)
amazon_text <- gsub(" $", "", amazon_text)

#feature vector
amazon_tweets.text.corpus <- Corpus(VectorSource(amazon_text))
amazon_tweets.text.corpus <- tm_map(amazon_tweets.text.corpus, function(x)removeWords(x,stopwords()))

#generating wordcloud
wordcloud(amazon_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#scoring sentiments according to in built functions
mysentiment_amazon<-get_nrc_sentiment((amazon_text))

Sentimentscores_amazon<-data.frame(colSums(mysentiment_amazon[,]))

names(Sentimentscores_amazon)<-"Score"
Sentimentscores_amazon<-cbind("sentiment"=rownames(Sentimentscores_amazon),Sentimentscores_amazon)
rownames(Sentimentscores_amazon)<-NULL

#graphical representation
ggplot(data=Sentimentscores_amazon,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on ecomerce giant AMAZON")

#**********GOOGLE******#

#extracting tweets
google <- searchTwitter("#google", n=1000,lang = "en")

#converting into data frame and viewing
google_tweets <- twListToDF(google)
View(google_tweets)

#text cleaning
google_text<- google_tweets$text
google_text<- tolower(google_text)
google_text <- gsub("rt", "", google_text)
google_text <- gsub("@\\w+", "", google_text)
google_text <- gsub("[[:punct:]]", "", google_text)
google_text <- gsub("http\\w+", "", google_text)
google_text <- gsub("[ |\t]{2,}", "", google_text)
google_text <- gsub("^ ", "", google_text)
google_text <- gsub(" $", "", google_text)

#feature vector creation
google_tweets.text.corpus <- Corpus(VectorSource(google_text))
google_tweets.text.corpus <- tm_map(google_tweets.text.corpus, function(x)removeWords(x,stopwords()))

#generarting wordcloud
wordcloud(google_tweets.text.corpus,min.freq = 1,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 100)

#assigning sentiment score according to in-built function
mysentiment_google<-get_nrc_sentiment((google_text))

Sentimentscores_google<-data.frame(colSums(mysentiment_google[,]))

names(Sentimentscores_google)<-"Score"
Sentimentscores_google<-cbind("sentiment"=rownames(Sentimentscores_google),Sentimentscores_google)
rownames(Sentimentscores_google)<-NULL

#graphical representation
ggplot(data=Sentimentscores_google,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech giant GOOGLE")

#********TECHNOLOGY********#
#extracting tweets
technology <- searchTwitter("#technology", n=1000,lang = "en")

#converting into data frame and viewing
tech_tweets <- twListToDF(technology)
View(tech_tweets)

#text cleaning
tech_text<- tech_tweets$text
tech_text<- tolower(tech_text)
tech_text <- gsub("rt", "", tech_text)
tech_text <- gsub("@\\w+", "", tech_text)
tech_text <- gsub("[[:punct:]]", "", tech_text)
tech_text <- gsub("http\\w+", "", tech_text)
tech_text <- gsub("[ |\t]{2,}", "", tech_text)
tech_text <- gsub("^ ", "", tech_text)
tech_text <- gsub(" $", "", tech_text)

#creating feature vector
tech_tweets.text.corpus <- Corpus(VectorSource(tech_text))

tech_tweets.text.corpus <- tm_map(tech_tweets.text.corpus, function(x)removeWords(x,stopwords()))

#generating wordcloud
wordcloud(tech_tweets.text.corpus,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

#scoring sentiment with the help of in built functions
mysentiment_tech<-get_nrc_sentiment((tech_text))

Sentimentscores_tech<-data.frame(colSums(mysentiment_tech[,]))
names(Sentimentscores_tech)<-"Score"

Sentimentscores_tech<-cbind("sentiment"=rownames(Sentimentscores_tech),Sentimentscores_tech)
rownames(Sentimentscores_tech)<-NULL

#graphical representation
ggplot(data=Sentimentscores_tech,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on tech as a whole")

