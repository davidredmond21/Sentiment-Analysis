api_key ="Mb9mtmUaUwAxQPnwFI7zeY03i"
api_secret ="oDYdsfqx90Al8j9uVdDIyMIuaehPauOMqGqIBoN4cBO2XGu93R"
access_token="1109902478567501826-RkXYmszx20ZLd4Sm9k7jNE9SKRYYsC"
access_secret="VThG8add6Xn3RXs72cV7eler2GcV6RoMuFWeM2YNXYMzY"
library(twitteR)
library(magrittr)
library(tm)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)

setup_twitter_oauth(api_key,api_secret,access_token,access_secret)
#getting tweets
tweets.df<-searchTwitter("Cristiano",n=100,lang = "en")%>%twListToDF()
glimpse(tweets.df)

write.csv(tweets.df,file="Twitter_Cristiano_feed.csv")
### Trend locationsa
#trend <-availableTrendLocations()
#
#world <- getTrends(1)
#
#boston <- getTrends(2367105)
# user timeline
#read.csv(choose.files(), header = T)
str(tweets.df)

corpus <- iconv(tweets.df$text, to="utf-8")
s      <- get_nrc_sentiment(corpus)
corpus <- Corpus(VectorSource(corpus))
#
# clean text
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
cleanset<- tm_map(corpus,removeWords,stopwords("english"))
cleanset  <- tm_map(cleanset,content_transformer(removeURL))
cleanset  <- tm_map(cleanset,stripWhitespace)
cleanset  <- tm_map(corpus,removeWords,c("cristiano","ronaldo"))
#cleanset  <- tm_map(corpus,gsub,pattern="stock",c("adi","analog","devices"))
#
tdm <- TermDocumentMatrix(cleanset)
tdm <-as.matrix(tdm)
#
head(tdm)
#
w <-rowSums(tdm)
tdm[1:10,1:10]
w <- subset(w,w>=75) # picks the words with more than 25 words
barplot(w,las=2, col=rainbow(40))
#
#obtain sentiment scores
head(s)
get_nrc_sentiment("ugly")
barplot(colSums(s)%>%sort,las=2,col=rainbow(10),ylab="count",main="sentiment for http://twitter.com/Cristiano")






#############
#

#  slack
client_id ="196615394113.602901226596"
client_secret = "74e2b6284b339ed85ce49ff899d19c84"
sign_secret = "2070122d94554f9ce22ed59908fbceb7"
verif_token = "2070122d94554f9ce22ed59908fbceb7"

library(here)
library(tidyverse)
library(rvest)
library(reshape2)
library(magrittr)
library(monkeylearn)
library(glue)
library(knitr)
library(dobtools)
library(tidytext)
library(kableExtra)

slack_url <- "https://www.capterra.com/p/135003/Slack/"




library(stringr)
###mailbox ##########################################
mail <- read.csv("C:/Users/dredmond/Documents/pacemail.csv",header=T)
#str(mail)
glimpse(mail)
## pick out the useful headings
mail%<>%select(c(Subject,From...Name.,Body))
#
mail%<>%rename("Subject"=Subject,"From"=From...Name.,Body="Body")
glimpse(mail)
#
corpus.sub <- iconv(mail$Subject, to="utf-8")
corpus.bdy <- iconv(mail$Body, to="utf-8")
corpus.frm <- iconv(mail$From, to="utf-8")
#
#
s.bdy      <- get_nrc_sentiment(corpus.bdy)
s.sub      <- get_nrc_sentiment(corpus.sub)
#
s.bdy%>%melt%>% ggplot(aes(variable,value),col=variable) + geom_point()+
  xlab("Sentiment")+
  ggtitle("Sentiment analysis of PACE: email")
s.sub%>%melt%>% ggplot(aes(variable,value),col=variable) + geom_point()+
  ggtitle("Sentiment analysis of PACE: email")
#
barplot(colSums(s.bdy),las=2,col=rainbow(50),ylab="count",main="sentiment for PACE mail BODY")
barplot(s.sub,las=2,col=rainbow(50),ylab="count",main="sentiment for PACE mail SUBJECT")
#

###### in this section we'll use UNIGRAMS only- so basically a bag of words
library(tidytext)
body <-  list()
for (i in 1:length(corpus.bdy)){
  body[[i]]<- str_split(corpus.bdy[[i]],"\\\n")
  body[[i]]<- gsub("\t", "", body[[i]])
  body[[i]]<- gsub("\t|\n", "", body[[i]])
  body[[i]]<- gsub(pattern = "projects",replacement = "project", body[[i]])

 }
glimpse(body[[1]])

corpus <- Corpus(VectorSource(corpus.bdy))
corpus <- Corpus(VectorSource(body))

# clean text
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
#
cleanset  <- tm_map(corpus,removeWords,stopwords("english"))
cleanset  <- tm_map(cleanset,stripWhitespace)
cleanset  <- tm_map(cleanset,removeWords,c("david","dave","analogcom","biederwolf","kathy","ryan","law","redmond","colin","kathy","kshatri","vidushi","mulqueen","pace","singh","mike","jonathan","regards","scott","raka","lyden","mark","forde"))
cleanset  <- tm_map(cleanset,removeWords,c("davidryananalogcom","davidredmondanalogcom","kathyfordeanalogcom","strzegowski","vidushikshatrianalogcom", "mailtokathyfordeanalogcom","mikemulqueenanalogcom","mailtodavidredmondanalogcom","rakasinghanalogcom","mailtovidushikshatrianalogcom","colinlydenanalogcom","mailtodavidryananalogcom","scottbiederwolfanalogcom","jonathanlawanalogcom","markstrzegowskianalogcom","monday","friday","adi"))
cleanset  <- tm_map(cleanset,gsub,"projects","project")
#cleanset  <- tm_map(corpus,gsub,pattern="stock",c("adi","analog","devices"))

#
tdm <- TermDocumentMatrix(cleanset, control=list(wordlengths=(c(2,Inf))))
tdm <-as.matrix(tdm)
#glimpse(tdm)
#
w <- rowSums(tdm)
w <- subset(w,w>=85) # picks the words with more than 25 words
barplot(sort(w),las=2, col=rainbow(60), main="Bag of Words")
#

library(tidytext)
library(dplyr)
body[[1]] %>%  count(word) %>%arrange(desc(n))
body. %>%
  select(created_at,text) %>%
  unnest_tokens("word", text)
  unnest_tokens(bigram,tdm,token="ngrams",n=2)




