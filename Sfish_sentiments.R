#
library(magrittr)
library(tm)
library(syuzhet)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(tidyverse)
library(magrittr)
library(tidytext)
library(stringr)
require(tidyverse)
require(tidytext)
require(gplots)
require(SnowballC)
require(sqldf)
##
removeURL  <- function(x) gsub('http[[:alnum:]]*','',x)
removeMAIL <- function(x) gsub('@\\w+','',x)
#
###mailbox ##########################################
#mail <- read.csv("C:/Users/dredmond/Documents/pacemail.csv",header=T)
mail <- read.csv("C:/Users/dredmond/Downloads/Swordfish_Emails.CSV",header=T)
## pick out the BODY only headings
# mail<- mail[1:200,]
mail%<>%select(c(Subject,Body,To...Name.,From...Name.,From...Address.))%>%
  rename(Subject="Subject",Body="Body", To="To...Name.",From="From...Name.",Address="From...Address.")
glimpse(mail)
## for the timestamp--- need to make a list of all valid dates for the project
#
#
valid_months <- seq(as.Date("2013/1/1"),as.Date("2013/12/1"),"months")%>%months()
coarse_time  <- tibble()
#
corpus.bdy    <-iconv(mail$Body, to="utf-8")
corpus.sbj    <-iconv(mail$Subject, to="utf-8")
corpus.nameFr <-iconv(mail$From,   to="utf-8")
corpus.nameTo <-iconv(mail$To,   to="utf-8")
#
# get rid og SKYPE meeting notices
#
corpus.bdy     <- str_split(corpus.bdy,"\\\n")  # this breaks up the corpus page into sentences
corpus.sbj    <- str_split(corpus.sbj,"\\\n")  # this breaks up the corpus page into sentences

corpus.bdy   <- corpus.bdy[!str_detect(corpus.bdy,"Skype Meeting")]
corpus.bdy   <- corpus.bdy[!str_detect(corpus.bdy,"http[[:alnum:]]*")]
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"Join the meeting online")]
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"http[[:alnum:]]*")]
## Some of the emails to 3rd parts are messed up
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"From:Neary")]# | "Sent:Monday")
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"Sent:Monday")]
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"Regards")]
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"VoiP:")]
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"Project Manager")]
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"Product Engineering Services (PES)")]
corpus.bdy   <- corpus.bdy[!str_detect(corpus.bdy,"@analog.com")]
corpus.sbj   <- corpus.sbj[!str_detect(corpus.sbj,"-----Original Appointment-----")]
# Breaks up each entry into component sentences based on "\n"
# So for each email in the corpus we get a list of sentences [[]]
body     <- str_split(corpus.bdy,"\\\n")  # this breaks up the corpus page into sentences
subject  <- str_split(corpus.sbj,"\\\n")  # this breaks up the corpus page into sentences
#
# Frst pass tidy up
for (i in 1:length(body)){
  body[[i]] <- body[[i]][!str_detect(body[[i]],"Regards")]  # not usually in the BODY
  body[[i]] <- body[[i]][!str_detect(body[[i]],"Cc:")]  # Not ususlly in the BODY
  body[[i]]<-  gsub("\t|\n", "  ", body[[i]])  #
}
# Frst pass tidy up
for (i in 1:length(subject)){
  subject[[i]] <- subject[[i]][!str_detect(subject[[i]],"Regards")]  # not usually in the BODY
  subject[[i]] <- subject[[i]][!str_detect(subject[[i]],"Cc:")]  # Not ususlly in the BODY
  subject[[i]]<-  gsub("\t|\n", "  ", subject[[i]])  #
}
# split the mail to BAG of WORDs, throw everything together BODY and SUBJECT and remove duplicates
corpus.bdy <-  gsub("\t|\n|RE|FW", "  ", corpus.bdy)%>%unique()  #
corpus.sbj <-  gsub("\t|\n|RE|FW", "  ", corpus.sbj)%>%unique()  #
#
s.bdy      <- get_nrc_sentiment(corpus.bdy)
s.sbj      <- get_nrc_sentiment(corpus.sbj)
# - need to figure if this is correct
senti       <- s.bdy
senti       <- s.sbj
senti       <- rbind(s.bdy,s.sbj)

#
bod.plt <- cbind(colnames(senti),colSums(senti))%>%as.tibble
#
colnames(bod.plt) <- c( "Sentiment","Score")
bod.plt$Score%<>%as.integer()
bod.plt%>%
  ggplot(aes(Sentiment,Score,colour=Sentiment)) + 
  geom_col(aes(Sentiment,fill=Sentiment))+
  theme(legend.position = "none")+
  ggtitle("OVERALL Sentiment analysis of Swordfish: email Unigrams full program  ")
#
#
###### in this section we'll use UNIGRAMS only- so basically a bag of words
#
body    <- str_split(corpus.bdy,"\\\n")  # this breaks up the corpus page into sentences
subject <- str_split(corpus.sbj,"\\\n")  # this breaks up the corpus page into sentences
sfish   <- append(body,subject)
#
write_rds(sfish,"C:/Users/dredmond/Documents/sfish.rds")
#
corpus <- Corpus(VectorSource(sfish))
# clean text
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus,stripWhitespace)
# some obvious Nouns not useful
corpus  <- tm_map(corpus,removeWords,c("monday","tuesday","wednesday","thursday","friday","saturday","sunday","rgds", "norwood"))
corpus  <- tm_map(corpus,removeWords,c("january","feburary","march","april","may","june","july","regards","rgds"))
#####   heading down the path of UNIGRAMS
cleanset  <- tm_map(corpus,removeWords,stopwords("english"))
##
cleanset  <- tm_map(cleanset,content_transformer(function(x) gsub(x, pattern = "issues", replacement = "issue")))
cleanset  <- tm_map(cleanset,removeWords,c("swordfish","adi","martin","analogcom","usa","mailtoanalogcom"))
 #####   heading down the path of UNIGRAMS
#
# Step .1  Look at UNIGRAM word frequency to make sure there isnt anything dominant or duplicated
#
tdm <- TermDocumentMatrix(cleanset, control=list(wordlengths=(c(2,Inf))))
tdm <-as.matrix(tdm)
# glimpse(tdm)
n <- 25L
w <- rowSums(tdm)
word.plt <- attributes(w)%>%as.vector
word.plt <- cbind(w%>%data.frame,word.plt)
colnames(word.plt) <- c("freq","words")
word.plt$freq%<>%as.integer()
word.plt%<>%arrange(-freq)
word.plt[1:n,] %>%
  ggplot(aes(x=words,y=freq,colour=words)) +  geom_col(aes(words,fill=words))+
  theme(axis.text.x =element_text(angle=45,hjust = 1))+
  theme(legend.position = "none")+
  #coord_flip()+
  ggtitle("Most Frequency of words (top 25) `Unigrams` from SwordFish email")

############ HEAT MAP
theme_set(theme_bw(12))
sd_scale <- function(x){(x - mean(x))/sd(x)}
#
n   <- length(sfish)
m <- 40
idx <- seq(from=as.POSIXct("2018-01-01"), to=as.POSIXct("2019-03-01"),length.out = m)
index <- 0
index <-vector()
for( i in seq_along(idx)) { index <-append(index,rep(idx[i],n/m))}
sf.df  <- do.call(rbind, Map(data.frame,index=index, text=c(sfish)))
sf.df$text %<>%as.character()
#
emotions <- sf.df%>%
  unnest_tokens(word, text) %>%
  left_join(get_sentiments("nrc"), by = "word") %>% na.omit()%>%
  group_by(index, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) %>%
  spread(sentiment, percent, fill=0) %>%
  ungroup()
#
# Plots of EMOTIONS as project moved forward
#
emotions%>%melt(id.vars=("index"))%>%
  ggplot(aes(index,value,col=variable))+geom_point()+
  geom_smooth(method = "auto",se=FALSE)+
  facet_wrap(~variable,scales="free")+
  ylab("Sentiment Score")+
  ggtitle("Sentiment Progression through Project Timeline , (grouped 10 day) sentiment calculation")
#
## Normalize data
#
emotions[,c(2:11)] <- apply(emotions[,c(2:11)], 2, sd_scale)
emotions <- as.data.frame(emotions)
rownames(emotions) <- emotions[,1]
emotions3 <- emotions[,-1]
emotions3 <- as.matrix(emotions3)
## Using a heatmap and clustering to visualize and profile emotion terms expression data
heatmap.2(
  emotions3,
  dendrogram = "both",scale = "none", trace = "none",key = TRUE,
  col    = colorRampPalette(c("green", "yellow", "red"))
)
#########################   POSITIVE or NEGATIVE
#
clean.word  <- tibble(text=sfish) %>% unnest_tokens(word,text)
clean.word %>%
  mutate(word_count = 1:n(),
         index = word_count %/% 50 + 1) %>%
  inner_join(get_sentiments("bing")) %>%
  count( index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative )%>%
  ggplot(aes(index, sentiment)) +
  xlab("project days")+
  geom_bar(aes(fill=sentiment), alpha = 1.0, stat = "identity", show.legend = TRUE)+
  ggtitle("Sentiment score for words using BING lexicon ")

bing_word_counts <- clean.word %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
#

## ------------------------------------------------------------------------
#    SENTENCE SENTIMENT
#
#
#install.packages('sentimentr')
library(sentimentr)
library(lubridate)
#
sf.sentence  <- tibble(indx = sf.df$index,  text=sf.df$text) %>%
  unnest_tokens(sentence ,text, token="sentences")
#
# FOR each sentence in the corpus
sf.sentence$sentence%>%sentiment_by(by=NULL)%>%
  ggplot(aes(element_id,ave_sentiment)) +
  theme(axis.text.x =element_text(angle=20,hjust = 1))  +
  geom_bar(aes(fill=ave_sentiment), alpha = 0.8, stat = "identity")+
  xlab("Project elapsed time for individual sentences")+
  geom_smooth(method = "auto")+
  ylim(-1, 1.5)+
  ggtitle("Sentiment score for each sentence ")

# group by the bi-weekly dates
sf.plt <- sf.sentence%>%get_sentences() %$% sentiment_by(sentence,list(indx))

sf.plt$indx %<>%as.Date(tz='UTC',origin="1970-01-01 00:00:00")

ggplot(sf.plt,aes(indx,ave_sentiment)) +
  geom_bar(aes(fill=ave_sentiment),alpha = 0.8, stat = "identity", show.legend = T) +
  scale_x_date(date_breaks = "1 month")+
  theme(axis.text.x =element_text(angle=45,hjust=1))+
  xlab("Date")+
  ggtitle("Sentiment of Sentences for BI-Weekly")
#
#
sf_deep  <- sf.sentence %>% group_by(indx)%>%
  mutate(sentence_num= 1:n(),index= round(sentence_num/n(),2))%>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(indx, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(desc(sentiment))
#
ggplot(sf_deep, aes(index, factor(indx, levels = sort(unique(indx), decreasing = TRUE)), fill = sentiment)) +
  geom_tile(color = "white") +
  scale_fill_gradient2() +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Project Timeline", y = "Sentiment") +
  ggtitle("Sentiment",
          subtitle = "Summary of the net sentiment score") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")







#
#
#
#
##########################
###
#
#
#


## -------------------------- Moving to BiGrams
library(tokenizers)

ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE)
  {
  stopifnot(is.numeric(n), is.finite(n), n > 0)
  options <- stringi::stri_opts_brkiter(type="word", skip_word_none = skip_word_none)
  function(x) {
    stopifnot(is.character(x))
    # Split into word tokens
    tokens <- unlist(stringi::stri_split_boundaries(x, opts_brkiter=options))
    len <- length(tokens)
    if(all(is.na(tokens)) || len < n) {
      # If we didn't detect any words or number of tokens is less than n return empty vector
      character(0)
    } else {
      sapply(
        1:max(1, len - n + 1),
        function(i) stringi::stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
      )
    }
  }
}
#
BigramTokenizer   <- function(x) ngram_tokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer  <- function(x) ngram_tokenizer(x, Weka_control(min = 3, max = 3))
FourgramTokenizer <- function(x) ngram_tokenizer(x, Weka_control(min = 4, max = 4))


# x <- ngram_tokenizer(4)(sample.list$blog)
# put into data frame
# reorder by descending frequency
# library(tidytext)
# library(dplyr)
# body[[1]] %>%  count(word) %>%arrange(desc(n))
# body. %>% select(created_at,text) %>%  unnest_tokens("word", text)
# unnest_tokens(bigram,tdm,token="ngrams",n=2)


library(stringi)
library(tm)
library(qdap)
library(ggplot2)
library(slam)
library(gridExtra)
generate_nGrams <- function(thisDF, nValue)
  {
  thisDF <- unlist(thisDF)
  nGramsList <- vector(mode = "character")
  for (i in 1:length(thisDF)) {
    this_nGramsList <- tokenize_ngrams(
      thisDF[i], n = nValue, simplify = FALSE)
    nGramsList <- c(nGramsList, this_nGramsList[[1]])
  }
  return(nGramsList)
}
generate_nGramsDF <- function(thisCorpus, nValue)
  {
  thisDF <- data.frame(text = sapply(thisCorpus, as.character), stringsAsFactors = FALSE)
  thisNGrams <- unname(unlist(sapply(thisDF, generate_nGrams, nValue)))
  thisGramsDF <- data.frame(table(thisNGrams))
  thisGramsDF$percentage <- (thisGramsDF$Freq/sum(thisGramsDF$Freq))
  thisGramsDF <- thisGramsDF[order(-thisGramsDF$Freq),]
  colnames(thisGramsDF) <- c("words","freq","percentage")
  return(thisGramsDF)
}

n=25
newGigram.df<- generate_nGramsDF(cleanset,3)
newGigram.df%<>% arrange(-freq)
glimpse(newGigram.df)
# Filter out some of the following
newGigram.df%<>% filter(words != "weekly meeting minutes")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "yes yes yes")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "weekly meeting notes")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "weekly minutes actions")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "utc dublin edinburgh")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "dublin edinburgh lisbon")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "edinburgh lisbon london")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "tx weekly minutes")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "time zone utc")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "rx tx weekly")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "meeting forwarded neary")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "following time zones")# ,"sent pm subject","pm subject re"))



newGigram.df%<>% filter(words != "forwarded meeting request")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "check email attachments")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "attachments message intended")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "attachments contain proprietary")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "attachments warning computer")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "attachments presence viruses")# ,"sent pm subject","pm subject re"))
newGigram.df%<>% filter(words != "contain proprietary confidential")# ,"sent pm subject","pm subject re"))



newGigram.df[1:n,] %>%
  ggplot(aes(words,freq,colour=words)) +  geom_col(aes(words,fill=words))+
  theme(axis.text.x =element_text(angle=45,hjust = 1))+
  theme(legend.position = "none")+
  coord_flip()+
  ggtitle("Most Frequency of words (top 15) `Trigrams` from SwordFish email")

# iGram Sentiment score with NOT or negative values
Bigram.df<- generate_nGramsDF(cleanset,2)
#Bigram.df$words%<>% as.String()
#unnest_tokens(Bigram.df, output=bigram,input=words,token="ngrams",n=2)
bigram <- separate(Bigram.df, col=words, into = c( "word1","word2"),sep=" ")
afinn <- get_sentiments("afinn")
#
not_freq_bigrams <- bigram %>% filter(word1 == "dont" | word1 == "not"|  word1 == "wouldnt" |   word1 == "shouldnt" )
#
not_freq_bigrams_stem <- not_freq_bigrams %>%
  mutate(word2 = str_replace_all(word2,   pattern = "ing$", replacement = ""))
#
not_freq_bigrams_stem <- not_freq_bigrams_stem %>%
  anti_join(stop_words , by = c(word2 = "word") ) %>%  count(word2,sort = TRUE)
#

top_10 <- not_freq_bigrams %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  mutate(x = n * score )  %>%
  arrange(desc(x))

bottom_10 <- not_freq_bigrams %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  mutate(x = n * score )  %>%
  arrange((x))

rbind(top_10,bottom_10) %>%
  mutate(fill = ifelse(x>0,"positive score","negative score") ) %>%

  ggplot() +
  geom_col(aes(y = x , x = reorder(word2,x), fill = factor(fill))
  ) +
  coord_flip() +
  theme_linedraw() +
  xlab(label="Words preceeded by not") +
  ggtitle("Sentiment Score * Frequency of the word")



