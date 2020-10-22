#+
#LDA_textlab.R
# this is just a primer on using LDA
#
# 10/22/2020
# Laura Wolton
#-


#####LDA model next week###########
#######LDA model on presidencies
install.packages(c('tm', 'SnowballC', 'wordcloud', 'topicmodels'))
library(tm)
library(tidytext)
library(dplyr)
library(topicmodels)
library(ggplot2)
library(readxl)

#read in the data we created last week with
#the NYT abstracts that were about women from 2001-2020
#- we called it womenPres.csv 
# I changed it manually to a XLXS file for ease of use
#
NYTwomenfile<-'C:/Users/vegan/Documents/R_dir/TextMiniLab/womenPres.xlsx'
wdat<-read_xlsx(NYTwomenfile,col_names = TRUE)

#first turn text into the right shape-> DocumentTerm Matrix
# and clean out stop words and junk
all_corpus = Corpus(VectorSource(wdat$docs.abstract))
all_corpus = tm_map(all_corpus, content_transformer(tolower))
all_corpus = tm_map(all_corpus, removeNumbers)
all_corpus = tm_map(all_corpus, removePunctuation)
all_corpus = tm_map(all_corpus, removeWords, c("the", "and","[\\'s]","[\\']","iht",'[[:punct:]]',"can","will", stopwords("english")))
all_corpus =  tm_map(all_corpus, stripWhitespace)

all_dtm <- DocumentTermMatrix(all_corpus)

####THE FIRST THING IS TO APPROXIMATE A GOOD K 
###TAKES VERY LONG ON BIG DOCUMENTS####
## YOU MAY NEED MORE MEMORY DEPNDING ON THE SIZE

#######find good k##########
#measures of topic coherence
mod_log_lik = numeric(20)
mod_perplexity = numeric(20)
for (i in 5:20) {
  mod <- LDA(all_dtm, k=i, method="Gibbs",
             control=list(alpha=0.5))
  mod_log_lik[i] = logLik(mod)
  mod_perplexity[i] = perplexity(mod, all_dtm)
  print(i)
}
########
#check perplexity, looking for sudden jumps
#would be the number before

plot(c(5:20),mod_log_lik[5:20])

plot(c(5:20),mod_perplexity[5:20])



ap_lda <- LDA(all_dtm, k = 11, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread
