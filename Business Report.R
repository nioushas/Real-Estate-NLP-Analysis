############################ DOWNLOADING LIBRARIES ############################ 
library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)
library(pdftools)
library(rtweet)
library(tidyr)
library(scales)
library(textreadr)
library(textdata)
library(wordcloud)
library(RColorBrewer)
library(reshape2)
library(tidyr)
library(ggplot2)
library(syuzhet)

############################ IMPORTING FILES ############################ 
setwd("/Users/nioushashahsavan/Desktop/PDF1")
nm <- list.files(path="/Users/nioushashahsavan/Desktop/PDF1")
#all together
my_pdf_text <- do.call(rbind, lapply(nm, function(x) paste(pdf_text(x), collapse = " ")))
colnames(my_pdf_text) <- c("text")
mydf <- data.frame(line=1:6, text = my_pdf_text[,1])


############################ Tokenization ############################ 
token_list <-  mydf %>% 
  unnest_tokens(word, text)

#Removing stop words
data(stop_words)
tidy_df <- token_list %>%
  anti_join(stop_words)
print(tidy_df)

#printing the count frequencies for each token without stop words
tidy_df %>%
  count(word, sort=TRUE)


#Making a histogram plot
freq_hist <-tidy_df %>%
  count(word, sort=TRUE) %>%
  filter(n>200) %>% 
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)


############################ TWITTER FILE ############################ 
housing <- search_tweets(
  "#bayarearealestate", n = 18000, include_rts = FALSE
)
head(housing$text)
housing$stripped_text <- gsub("http.*","",  housing$text)
housing$stripped_text <- gsub("https.*","", housing$stripped_text)

#Tokenizing housing tweets
housing_clean <- housing %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# removing stop words
data(stop_words)
tidy_housing <- housing_clean %>%
  anti_join(stop_words)
print(tidy_housing)

#printing the count frequencies for each token without stop words
tidy_housing %>%
  count(word, sort=TRUE)

#Graphing the clean tweets - twitter histogram
tidy_housing %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")


############################ Text Files ############################ 
setwd("/Users/nioushashahsavan/Desktop/TXT")
nm2 <- list.files(path="/Users/nioushashahsavan/Desktop/TXT")
nm2

my_data2 <- read_document(file=nm[1]) #This comes out as a vector, 

y_data_together <- paste(my_data2, collapse = " ") # This will give us a concatenated vector

my_txt_text2 <- do.call(rbind, lapply(nm2, function(x) paste(read_document(file=x), collapse = " ")))

mydf2 <- data.frame(text=my_txt_text2)
print(mydf2)

#Tokenizing
token_list2 <-  mydf2 %>% 
  unnest_tokens(word, text)

#Removing stop words
data(stop_words)
tidy_df2 <- token_list2 %>%
  anti_join(stop_words)
print(tidy_df2)

#printing the count frequencies for each token without stop words
tidy_df2 %>%
  count(word, sort=TRUE)


#Making a histogram plot - text histogram
freq_hist2 <-tidy_df2 %>%
  count(word, sort=TRUE) %>%
  filter(n>50) %>% 
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

############################ COMBINING TWITTER AND PDF FILES ############################ 
frequency <- bind_rows(mutate(tidy_df, source="PDF"),
                       mutate(tidy_df2, source= "TXT")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(source, word) %>%
  group_by(source) %>%
  mutate(proportion = n/sum(n))%>% # we care how frequent is this word in the entire book. This is frequency divided by all the counts/proportion
  select(-n) %>% # we are unselecting n, we will use proportion instead of n
  spread(source, proportion) %>%
  gather(source, proportion, `PDF`, `TXT`)

#Graphing plot
ggplot(frequency, aes(x=proportion, y=`PDF`, 
                      color = abs(`PDF- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~source, ncol=2)+
  theme(legend.position = "none")+
  labs(y= tidy_df, x=NULL)



############################ SENTIMENTS ############################ 
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)

sentiments %>%
  filter(lexicon == "nrc")

unique(sentiments$value) #this is the qualitative
summary(sentiments$value)

# Bar plot for full sentiments
review <- as.character(tidy_df)
s <- get_nrc_sentiment(review)
barplot(colSums(s), col = rainbow(10), ylab = 'count', main = 'Sentiment Score for PDF Files')

##################### looking at negative nrc sentiments ##################### 
nrcnegative <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")

#inner joining the pdf file and the negative sentiments
tidy_df %>%
  inner_join(nrcnegative) %>%
  count(word, sort=T)

##################### looking at positive nrc sentiments  ##################### 
nrcpositive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

#inner joining the pdf file and the positive sentiments
tidy_df %>%
  inner_join(nrcpositive) %>%
  count(word, sort=T)

##################### looking at anger nrc sentiments  ##################### 
nrcanger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")

#inner joining the pdf file and the anger sentiments
tidy_df %>%
  inner_join(nrcanger) %>%
  count(word, sort=T)

##################### looking at anticipation nrc sentiments  ##################### 
nrcanticipation <- get_sentiments("nrc") %>%
  filter(sentiment == "anticipation")

#inner joining the pdf file and the anticipation sentiments
tidy_df %>%
  inner_join(nrcanticipation) %>%
  count(word, sort=T)

##################### looking at trust nrc sentiments  ##################### 
nrctrust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")

#inner joining the pdf file and the anticipation sentiments
tidy_df %>%
  inner_join(nrctrust) %>%
  count(word, sort=T)

##################### looking at bing sentiments  ##################### 
df_bing <- tidy_df %>%
  inner_join(get_sentiments("bing"))

count_df <- df_bing %>%
  count(word, sentiment, sort=TRUE)

count_df %>%
  group_by(sentiment) %>%
  top_n(25) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, fill = sentiment)) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "contribution to sentiment",
       x = NULL)+
  coord_flip()



################################# WORD CLOUD ################################# 
#Initial wordcloud
tidy_df %>%
  with(wordcloud(word,max.words = 100))

# Wordcloud for nrc
tidy_df %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>% 
  acast(word ~sentiment, value.var="n", fill=0) %>% 
  comparison.cloud(colors = c("grey20", "grey80"),
                   max.words=100, scale = c(1,0.1))

# Wordcloud for bing
tidy_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sprt=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                      max.words=100)











