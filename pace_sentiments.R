#
library(magrittr)
library(tm)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(tidyverse)
library(magrittr)
library(tidytext)
library(stringr)
##
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
removeMAIL <- function(x) gsub('@\\w+','',x)
#
###mailbox ##########################################
mail <- read.csv("C:/Users/dredmond/Documents/pacemail.csv",header=T)
#mail <- read.csv("C:/Users/dredmond/Downloads/slack-export.csv",header=F)
## pick out the BODY only headings
mail%<>%select(c(Body,To...Name.,From...Name.,From...Address.))%>%
  rename(Body="Body", To="To...Name.",From="From...Name.",Address="From...Address.")
glimpse(mail)
#
## for the timestamp--- need to make a list of all valid dates for the project
#  use str_detect(mail$Body, valid_dates)

valid_months <- seq(as.Date("2013/1/1"),as.Date("2013/12/1"),"months")%>%months()

for( i in 1: length(mail$Body))  {
  coarse_time[i] <-valid_months[str_detect(mail$Body[i],valid_months)][1]
  coarse_time[i] <-replace_na(coarse_time[i] ,valid_months[(i%%12+1)])
  }


corpus.bdy <- iconv(mail$Body, to="utf-8")
corpus.nameFr <-iconv(mail$From,   to="utf-8")
corpus.nameTo <-iconv(mail$To,   to="utf-8")

#
# split the mail to BAG of WORDs
s.bdy      <- get_nrc_sentiment(corpus.bdy)%>%as.tibble
#
bod.plt <- cbind(colnames(s.bdy),colSums(s.bdy))%>%as.tibble()
colnames(bod.plt) <- c( "Sentiment","Score")
bod.plt$Score%<>%as.integer()
bod.plt%>%ggplot(aes(Sentiment,Score,colour=Sentiment)) + geom_col(aes(Sentiment,fill=Sentiment))+
theme(legend.position = "none")+
ggtitle("Sentiment analysis of PACE: email Unigrams upto April-14th")
#
###### in this section we'll use UNIGRAMS only- so basically a bag of words
#
body<- str_split(corpus.bdy,"\\\n")  # this breaks up the corpus page into sentences
team <- str_split(corpus.name,"\\,|\\;") # have a list of team recipients

for (i in 1:length(corpus.bdy)){
#  body[[i]] <- body[[i]][!str_detect(body[[i]],"analog.com")] # looks for evidence of mail address- then removes
#  body[[i]] <- body[[i]][!str_detect(body[[i]],"\\\t")]  # remove \\t sentences
  body[[i]] <- body[[i]][!str_detect(body[[i]],"To:")]  # not usually in the BODY
  body[[i]] <- body[[i]][!str_detect(body[[i]],"Cc:")]  # Not ususlly in the BODY
  body[[i]]<-  gsub("\t|\n", "  ", body[[i]])  #
  body[[i]]<- gsub(pattern = "projects",replacement = "project", body[[i]])
  }
write_rds(body,"C:/Users/dredmond/Documents/pace_body.rds")

# generates a list of team names we want to drop automatically
team.df <- data.frame()
for (i in 1:length(corpus.name)){
  line.df <- team[i][1]%>%unlist()%>%unique()
  team.df <- c(line.df,team.df)%>%unlist%>%unique()
  team.df <- gsub(" ", "", team.df)
}
#
#
corpus <- Corpus(VectorSource(body))
# clean text
corpus  <- tm_map(corpus, removeWords,team.df)
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
cleanset  <- tm_map(cleanset,removeWords,c("pace","adi","analogcom","usa","mailtoanalogcom"))
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
  ggtitle("Most Frequency of words (top 25) `Unigrams` from pace email")




#cleanset  <- tm_map(cleanset,removeWords,c("mike","colin","regards","kathy","dave","subject","sent","vidushi"))
#
#cleanset  <- tm_map(cleanset,removeWords,c("pace","adi", "sharepoint","don't"))

#cleanset  <- tm_map(cleanset,content_transformer(function(x) gsub(x, pattern = "projects", replacement = "project")))
#cleanset  <- tm_map(cleanset,content_transformer(function(x) gsub(x, pattern = "dont", replacement = "dont")))
#cleanset  <- tm_map(cleanset,content_transformer(function(x) gsub(x, pattern = "issues", replacement = "issue")))

#cleanset  <- tm_map(cleanset,removeWords,c("davidryananalogcom","davidredmondanalogcom","kathyfordeanalogcom","strzegowski","vidushikshatrianalogcom", "mailtokathyfordeanalogcom","mikemulqueenanalogcom","mailtodavidredmondanalogcom","rakasinghanalogcom","mailtovidushikshatrianalogcom","colinlydenanalogcom","mailtodavidryananalogcom","scottbiederwolfanalogcom","jonathanlawanalogcom","markstrzegowskianalogcom","monday","friday","adi"))
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

#### plot of TRI GRAMS
#cleanset  <- tm_map(cleanset,removeWords,c("mailto","analogcom","mailtokathyanalogcom","kathyanalogcom"))
#cleanset  <- tm_map(cleanset,removeWords,c("david","dave","analog","adi","biederwolf","kathy","ryan","law","adi","strzegowski","redmond","colin","kathy","kshatri","vidushi","mulqueen","pace","singh","mike","jonathan","regards","scott","raka","lyden","mark","forde"))
#cleanset  <- tm_map(cleanset,removeWords,c("usa","english","dial"))


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

n=15
newGigram.df<- generate_nGramsDF(cleanset,3)
newGigram.df%<>% arrange(-freq)
glimpse(newGigram.df)
# Filter out some of the following
newGigram.df%<>% filter(words != "sent subject re")# ,"sent pm subject","pm subject re"))
newGigram.df[1:n,] %>%
  ggplot(aes(words,freq,colour=words)) +  geom_col(aes(words,fill=words))+
  theme(axis.text.x =element_text(angle=45,hjust = 1))+
  theme(legend.position = "none")+
  coord_flip()+
  ggtitle("Most Frequency of words (top 15) `Trigrams` from pace email")




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





data("movie_review")
N = 100
tokens = word_tokenizer(tolower(movie_review$review[1:N]))
dtm = create_dtm(itoken(tokens), hash_vectorizer())
model_tfidf = TfIdf$new()
dtm_tfidf = model_tfidf$fit_transform(dtm)


# ------------------------
# snippet from https://datascienceplus.com/unsupervised-learning-and-text-mining-of-emotion-terms-using-r/
# -----------------------

## Retrieve the letters
library(pdftools)
library(rvest)
library(XML)
# Getting & Reading in HTML Letters
urls_77_97 <- paste('http://www.berkshirehathaway.com/letters/', seq(1977, 1997), '.html', sep='')
html_urls <- c(urls_77_97,
               'http://www.berkshirehathaway.com/letters/1998htm.html',
               'http://www.berkshirehathaway.com/letters/1999htm.html',
               'http://www.berkshirehathaway.com/2000ar/2000letter.html',
               'http://www.berkshirehathaway.com/2001ar/2001letter.html')

letters_html <- lapply(html_urls, function(x) read_html(x) %>% html_text())
# Getting & Reading in PDF Letters
urls_03_18 <- paste('http://www.berkshirehathaway.com/letters/', seq(2003, 2018), 'ltr.pdf', sep = '')
pdf_urls <- data.frame('year' = seq(2002, 2018),'link' = c('http://www.berkshirehathaway.com/letters/2002pdf.pdf', urls_03_18))
download_pdfs <- function(x) {
  myfile = paste0(x['year'], '.pdf')
  download.file(url = x['link'], destfile = myfile, mode = 'wb')
  return(myfile)
}
pdfs <- apply(pdf_urls, 1, download_pdfs)
letters_pdf <- lapply(pdfs, function(x) pdf_text(x) %>% paste(collapse=" "))
tmp <- lapply(pdfs, function(x) if(file.exists(x)) file.remove(x))
# Combine letters in a data frame
letters <- do.call(rbind, Map(data.frame, year=seq(1977, 2018), text=c(letters_html, letters_pdf)))
letters$text <- as.character(letters$text)

## Load additional required packages
require(tidyverse)
require(tidytext)
require(gplots)
require(SnowballC)
require(sqldf)
theme_set(theme_bw(12))
### pull emotion words and aggregate by year and emotion terms

emotions <- letters %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive")) %>%
  group_by(year, sentiment) %>%
  summarize( freq = n()) %>%
  mutate(percent=round(freq/sum(freq)*100)) %>%
  select(-freq) %>%
  spread(sentiment, percent, fill=0) %>%
  ungroup()
## Normalize data
sd_scale <- function(x) {(x - mean(x))/sd(x)}
emotions[,c(2:9)] <- apply(emotions[,c(2:9)], 2, sd_scale)
emotions <- as.data.frame(emotions)
rownames(emotions) <- emotions[,1]
emotions3 <- emotions[,-1]
emotions3 <- as.matrix(emotions3)
## Using a heatmap and clustering to visualize and profile emotion terms expression data

rc <- rainbow(nrow(emotions3), start=0, end=.3)
cc <- rainbow(ncol(emotions3), start=0, end=.3)
heatmap.2(emotions3, col=cm.colors(255), scale="column",
          RowSideColors=rc, ColSideColors=cc, margin=c(5, 10),
          xlab="specification variables", ylab= " Models",
          main="heatmap scale=\"column\")",
          tracecol="green", density="density")

heatmap.2(emotions3,keysize = 2)

heatmap.2(
  emotions3,
  dendrogram = "both",
  scale      = "none",
  trace      = "none",
  key        = TRUE,
  col    = colorRampPalette(c("green", "yellow", "red"))
)


Before <- c("produce",  "produces", "produced", "producing", "product", "products", "production")
wstem <- as.data.frame(wordStem(Before))
names(wstem) <- "After"
table(cbind(Before, wstem))


## pull emotions words for selected heatmap groups and apply stemming
set.seed(456)
emotions_final <- letters  %>%
  unnest_tokens(word, text) %>%
  filter(!grepl('[0-9]', word)) %>%
  left_join(get_sentiments("nrc"), by = "word") %>%
  filter(!(sentiment == "negative" | sentiment == "positive" | sentiment == "NA")) %>%
  subset(year==1987 | year==1989 |  year==2001 |  year==2008 | year==2012 | year==2013) %>%
  mutate(word = wordStem(word)) %>%
  ungroup()

group1 <- emotions_final %>%
  subset(year==1987| year==1989 ) %>%
  select(-year, -sentiment) %>%
  unique()
group4 <- emotions_final %>%
  subset(year==2012 | year==2013) %>%
  select(-year, -sentiment) %>%
  unique()
group5 <- emotions_final %>%
  subset(year==2001 | year==2008 ) %>%
  select(-year, -sentiment) %>%
  unique()

# common and unique words between two groups
emo_venn2 <- venn(list(group1$word, group4$word))


# common and unique words among three groups
emo_venn3 <- venn(list(group1$word, group4$word, group5$word))

## The code below pulled a list of all common/unique emotion words expressed
## in all possible combinations of the the three heatmap groups
venn_list <- (attr(emo_venn3, "intersection"))
## and then print only the list of unique emotion words expressed in group-5.
print(venn_list$'C')

## Confirmation of unique emotion words in heatmap group-1
group1_U <- as.data.frame(venn_list$'A')
library(sqldf)
names(group1_U) <- "terms"
uniq1 <- sqldf( "select t1.*, g1.terms
                from emotions_final t1
                left join
                group1_U g1
                on t1.word = g1.terms ")
uniq1a <- !is.na(uniq1$terms)
uniqs1 <- rep(NA, length(emotions_final))
uniqs1[uniq1a] <- 1
plot(uniqs1, main="Dispersion plot of emotions words \n unique to heatmap group 1 ", xlab="Length (Word count)", ylab=" ", col="red", type='h', ylim=c(0,1), yaxt='n')

## confirmation of unique emotion words in heatmap group-5
group5_U <- as.data.frame(venn_list$'C')
names(group5_U) <- "terms"
uniq5 <- sqldf( "select t1.*, g5.terms
                from emotions_final t1
                left join
                group5_U g5
                on t1.word = g5.terms ")
uniq5a <- !is.na(uniq5$terms)
uniqs5 <- rep(NA, length(emotions_final))
uniqs5[uniq5a] <- 1

plot(uniqs5, main="Dispersion plot of emotions words \n unique to heatmap group 5 ", xlab="Length (Word count)", ylab=" ", col="red", type='h', ylim=c(0,1), yaxt='n')


## confirmation of unique emotion words in heatmap group-4
group4_U <- as.data.frame(venn_list$'B')
names(group4_U) <- "terms"
uniq4 <- sqldf( "select t1.*, g4.terms
                from emotions_final t1
                left join
                group4_U g4
                on t1.word = g4.terms "
)
uniq4a <- !is.na(uniq4$terms)
uniqs4 <- rep(NA, length(emotions_final))
uniqs4[uniq4a] <- 1

plot(uniqs4, main="Dispersion plot of emotions words \n unique to heatmap group 4 ", xlab="Length (Word count)", ylab=" ", col="red", type='h', ylim=c(0,1), yaxt='n')

library(readxl)
sp500 <- read_excel("C:/Users/dredmond/Downloads/histretSP.xls",skip=0)
ggplot(sp500[50:91,], aes(x=Year, y=return, colour=return>0)) +
  geom_segment(aes(x=Year, xend=Year, y=0, yend=return),
               size=1.1, alpha=0.8) +
  geom_point(size=1.0) +
  xlab("Investment Year") +
  ylab("S&P500 Annual Returns") +
  labs(title="Annual Returns on Investment in S&P500", subtitle= "source: http://pages.stern.nyu.edu/~adamodar/New_Home_Page/datafile/histretSP.html") +
  theme(legend.position="none") +
  coord_flip()



devtools::install_github("statsmaths/coreNLP")







##########
# Algorithm development
#  Step #1 RNN classification to pos/neg sentiment on each sentence
# use a trained RNN to identify 5-grams into a particular sentiment
#  Step # 2 within each 5-gram generate a sentiment weight based on the
# key words "very" ..... extra or strong adjectives

install.packages('devtools')
devtools::install_github("statsmaths/coreNLP")
library(coreNLP)
initCoreNLP()
coreNLP::downloadCoreNLP()
library(coreNLP)
initCoreNLP()
catInHat = c("the sun did not shine.", "it was too wet to play.",
             "so we sat in the house all that cold, cold, wet day.")
output = annotateString(catInHat)
output
getToken(output)[,c(1:3,6:7)]
getSentiment(output)
getDependency(output)
devtools::install_github("bradleyboehmke/harrypotter")
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(harrypotter)
philosophers_stone[1:2]
philosophers_stone[1:2]
glimpse(philosophers_stone[1:2])
?sentiments
get_sentiments("nrc")
get_sentiments("nrc")$sentiment
get_sentiments("nrc")$sentiment%>%levels()
get_sentiments("nrc")$sentiment%>%as.factor()%>%levels
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")
titles
list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
     goblet_of_fire, order_of_the_phoenix, half_blood_prince,
     deathly_hallows)
books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)
series <- tibble()
for(i in seq_along(titles)) {
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  series <- rbind(series, clean)
}
series$book <- factor(series$book, levels = rev(titles))
series
series %>%right_join(get_sentiments("nrc"))%>% filter(!is.na(sentiments))%>%count(sentiments,sort=TRUE)
series %>%right_join(get_sentiments("nrc"))%>% filter(!is.na(sentiment))%>%count(sentiments,sort=TRUE)
series %>%right_join(get_sentiments("nrc"))%>% filter(!is.na(sentiment))%>%count(sentiment,sort=TRUE)
bing_word_counts <- series %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts
tibble(text = philosophers_stone) %>%
  unnest_tokens(sentence, text, token = "sentences")
?unnest_tokens
ps_sentences <- tibble(chapter = 1:length(philosophers_stone),
                       text = philosophers_stone) %>%
  unnest_tokens(sentence, text, token = "sentences")
glimpse(ps_sentences)
ps_sentences %>%
  group_by(chapter) %>%
  mutate(sentence_num = 1:n(),
         index = round(sentence_num / n(), 2))
ps_sentences %>%
  group_by(chapter) %>%
  mutate(sentence_num = 1:n(),
         index = round(sentence_num / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(chapter, index)
book_sent <- ps_sentences %>%
  group_by(chapter) %>%
  mutate(sentence_num = 1:n(),
         index = round(sentence_num / n(), 2)) %>%
  unnest_tokens(word, sentence) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(chapter, index) %>%
  summarise(sentiment = sum(score, na.rm = TRUE)) %>%
  arrange(desc(sentiment))
book_sent
ggplot(book_sent, aes(index, factor(chapter, levels = sort(unique(chapter), decreasing = TRUE)), fill = sentiment)) +
  geom_tile(color = "white") +
  scale_fill_gradient2() +
  scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Chapter Progression", y = "Chapter") +
  ggtitle("Sentiment of Harry Potter and the Philosopher's Stone",
          subtitle = "Summary of the net sentiment score as you progress through each chapter") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top")
savehistory("~/hist.R")
