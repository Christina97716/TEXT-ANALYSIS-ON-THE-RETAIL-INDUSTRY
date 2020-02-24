install.packages('pdftools')
install.packages('shapeR')
install.packages('tidytext')
install.packages('tidyverse')
install.packages("textreadr")
install.packages("textshape")
install.packages("dplyr")
install.packages("textdata")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm")
install.packages("packages")
rm(list=ls())
#Instaling libraries 
library(pdftools)
library(shapeR)
library(tidyverse)
library(textshape)
library(textreadr)
library(tidytext)
library(dplyr)


library(tidyr)
library(scales)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(textdata)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(NLP)
library(reshape2)




#file for assignment 
setwd("C:/Users/Christina/Desktop/MSBA SPRING/TEXT ANALTICS/pdf")
nm <- list.files("C:/Users/Christina/Desktop/MSBA SPRING/TEXT ANALTICS/pdf")

#Read read and save as a data frame
my_pdf_text <- as.data.frame(do.call(rbind, lapply(nm,function(x) pdf_text(x))))
View(my_pdf_text)
#Transposing and divinding company wise 
store_names <- c('Macy','Sears','Forever 21','Toys R Us')

my_pdf_text <- data_frame(line=1:ncol(my_pdf_text),text=t(as.matrix(my_pdf_text)),store=store_names) 
view(my_pdf_text)


token_list <- my_pdf_text %>%
  unnest_tokens(word, text)
print(token_list)
#frequencies 
frequencies_tokens <- my_pdf_text %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE)
print(frequencies_tokens)



#frequency 
data(stop_words)
frequency_tokens_nostop <- my_pdf_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(word, sort=TRUE)
print(frequency_tokens_nostop)






#WORDCLOUD
wordcloud2(data=frequency_tokens_nostop, size=0.5, color='random-dark')


#plot histogram
freq_hist <- my_pdf_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(word, sort=FALSE) %>%
  filter(n >4) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

#sentiment analysis


print(sentiments)

afinn <-get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")
table(nrc$sentiment)
table(bing$sentiment)

sentiments <- bind_rows(
  (mutate(afinn,lexicon="afinn")),
  (mutate(nrc,lexicon="nrc")),
  (mutate(bing,lexicon="bing"))
)

my_sentiment <-my_pdf_text %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("afinn")) %>%
  #summary mean value
  summarise(mean(value))
View(my_sentiment)

tidy_store <- my_pdf_text %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
tidy_store

tidy_store <- my_pdf_text %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()
tidy_store

#plot the graph 
tidy_store %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="different types of sentiment", x=NULL)+
  coord_flip()



#wordcloud
install.packages("wordcloud")
library(wordcloud)
data("stop_words")

tidy_store %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale<-c(0.5,0.5),
                   fixed.asp=TRUE,
                   title.size =2)


#quadrogram

quadrogram <- my_pdf_text %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram


quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") 
quadrogram_tf_idf <- quadrogram_united %>%
  count(store, quadrogram) %>%
  bind_tf_idf(quadrogram, store, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf