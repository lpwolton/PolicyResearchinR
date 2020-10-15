#+
#NYT_API_TextMiniLab.R
#working with APIs
#creating dictionaries
#Creating pretty plots
# Laura Wolton
# -
install.packages("devtools")
devtools::install_github("mkearney/nytimes")
install.packages("jsonlite")
install.packages("rlang")

library(jsonlite)
library(devtools)
library(rlang)
library(dplyr)
library(ggplot2)
library(SentimentAnalysis)

#you need a key to work with most APIs
#it tells the API what you are allowed to do and what permissions you have

NYTIMES_KEY="YourKeyHere" 

###generally following https://www.storybench.org/working-with-the-new-york-times-api-in-r/
#the shape of your call is very important
#question mark is the beginning of the query
#the things you are allowed to search for are API and app dependent
baseurl<-paste('https://api.nytimes.com/svc/search/v2/articlesearch.json?begin_date=20010101&end_date=20200520&fq=news_desk:("Editorial" "Op-Ed")&q=women&api-key=',NYTIMES_KEY,sep="")

initialQuery <- fromJSON(baseurl,flatten=TRUE) %>% data.frame() 
maxPages <- round((initialQuery$response.meta.hits[1] / 10)-1) 

pages <- list()
for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  message("Retrieving page ", i)
  pages[[i+1]] <- nytSearch 
  Sys.sleep(6) 
}
allNYTSearch <- rbind_pages(pages)

#pull in the data from the API
#adjust the dates to remove the timestamp on the end
allNYTSearch$dates<-gsub("T.*","",allNYTSearch$response.docs.pub_date)

###you might want to save this data for later
##requires flattening it out
keyw<-vector()
for (i in c(1:dim(allNYTSearch)[1])) {
  if (is_empty(allNYTSearch$response.docs.keywords[[i]])==FALSE) keyw[i]<-paste(unlist(allNYTSearch$response.docs.keywords[[i]]['value']),collapse="/")
}

#if you want to write this out or simplify to
#an unnested data frame you can edit your variables here
df_all<-data.frame(date<-allNYTSearch$response.docs.pub_date,abstract<-allNYTSearch$response.docs.abstract, headline<-allNYTSearch$response.docs.headline.main,url<-allNYTSearch$response.docs.web_url,keyw,news_desk<-allNYTSearch$response.docs.news_desk,wd_ct<-allNYTSearch$response.docs.word_count, byline<-allNYTSearch$response.docs.byline.original,allNYTSearch$response.docs.byline.organization, uri<-allNYTSearch$response.docs.uri)

#this will write out to your working directory
write.csv(x=df_all, file="womenPres.csv")

#wortking with the data

#I decided that I wanted to label the articles from each presidency
#assign president to a new column
#create a new column in existing Dataframe that is for strings/characters
allNYTSearch$president<-'NA'
allNYTSearch$president[(allNYTSearch$dates> "2001-01-01" & allNYTSearch$dates < "2009-01-20")]<-"Bush"
allNYTSearch$president[(allNYTSearch$dates>= "2009-01-01" & allNYTSearch$dates < "2017-01-20")]<-"Obama"
allNYTSearch$president[(allNYTSearch$dates>= "2017-01-20" & allNYTSearch$dates < "2020-05-20")]<-"Trump"

#libraries for this section
library(tau); library(corpus);
library(dplyr)
library(tidytext)
library(SentimentAnalysis)
library(ggplot2)
library(lubridate)

##THIS SECTION IS TO CREATE A LIST OF 
#FREQUENT PHRASES TO CREATE A DICTIONARY
#first clean up text for corpus
text_df <- tibble(text = allNYTSearch$response.docs.abstract)
bigramsd<-text_df %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)
wdfreq<-bigramsd %>% count(bigram, sort = TRUE)
#if you want to say only a certain wdfreq matters
#wdfreq<-wdfreq[wdfreq$n>10,]
outfile<-('wdphrase2.csv')
write.csv(wdfreq,outfile,row.names = FALSE)

###IF WE HAVE TIME WE'll GO THROUGH HOW TO USE DICTIONARIES
#######READ THE FILE IN AFTER YOU HAVE CATEGORIZED######
library(readxl)
issue_dict<-read_xlsx('C:/Users/vegan/OneDrive - The University of Colorado Denver/Documents/SPA_Work/R_dir/SICSS/women_dict.xlsx',col_names=TRUE,col_types=NULL)

#rename cols
colnames(issue_dict)[c(2:7)]<-c("word","ww","cs","sl","ca","wact")
#make the columns friendly the grep search
#have to swap 0 to 1 and 1 to 0
issue_dict$cs<-replace(issue_dict$cs,issue_dict$cs==1,"FALSE")
issue_dict$cs<-replace(issue_dict$cs,issue_dict$cs==0,"TRUE")

#if the word should only use the first characters
ind=which(issue_dict$sl==1)
issue_dict$word[ind]<-paste("",issue_dict$word[ind])

issue_dict$word<-gsub("\\s+"," ",issue_dict$word)

#if the word is the whole word
ind=which(issue_dict$ww==1)
issue_dict$word[ind]<-paste("",issue_dict$word[ind],"")

#make matrices that are big enough to hold the characters
lev.names<-unique(issue_dict$Category)
ct_matrix<-array(0,dim=c(48,length(lev.names)))

####count the number of references for each candidate###
#for each category 
iss_segs<-data.frame(matrix(ncol=6,nrow=0))
issue_dict$count<-0

for (wind in 1:length(issue_dict$word)){
  #look for the word in tweets, tag it
  ind<-grep(issue_dict$word[wind],allNYTSearch$response.docs.abstract,value=FALSE,ignore.case=issue_dict$cs[wind])
  issue_dict$count[wind]<-length(ind)
  #add to the output dataframe
  #pull out the columns we want to carry with it
  iss_segs<-rbind(iss_segs,cbind(allNYTSearch$response.docs.pub_date[ind],cbind(allNYTSearch$response.docs.headline.main[ind]),cbind(cbind(rep(issue_dict$Category[wind],length(ind)),rep(issue_dict$word[wind],length(ind))),allNYTSearch$response.docs.abstract[ind])))
} 
#rename the columns in iss_segs
colnames(iss_segs)<-c("date_short","headline","category","dict_word","abstract")
#fix the dates and assign president 
#THis may be a duplicate but just to be sure
iss_segs$date_short<-gsub("T.*","",iss_segs$date_short)
iss_segs$president<-'NA'
iss_segs$president[(iss_segs$date_short> "2001-01-01" & iss_segs$date_short < "2009-01-20")]<-"Bush"
iss_segs$president[(iss_segs$date_short>= "2009-01-01" & iss_segs$date_short < "2017-01-20")]<-"Obama"
iss_segs$president[(iss_segs$date_short>= "2017-01-20" & iss_segs$date_short < "2020-05-20")]<-"Trump"

#Perform sentiment analysis on the characters
#This first step is sorting out actors, issues and solutions
issues<-iss_segs[grep("ISSUE",iss_segs$category,ignore.case=FALSE),]
issues$category<-gsub("ISSUE_","",issues$category)
actors<-iss_segs[grep("ACTOR",iss_segs$category,ignore.case=FALSE),]
actors$category<-gsub("_ACTOR","",actors$category)
solns<-iss_segs[grep("SOL",iss_segs$category,ignore.case = FALSE),]

#####ACTORS####
actors$senti<-0.00
actors$senti<-analyzeSentiment(actors$abstract)$SentimentGI

ggplot(actors, aes(x=as.Date(date_short),y=senti,colour=factor(category)))+
  geom_smooth(method="loess", se=FALSE, fullrange=FALSE, level=0.95)+
  theme_bw() +
  labs(x = "Date", y = "Sentiment") +
  scale_linetype_discrete(name="category")

####plot issue frames over time

df<-issues %>%
  group_by(year=floor_date(as.Date(date_short),"year"),category) %>% 
  tally

ggplot(df, aes(x = year, y = n, fill=category)) + 
  geom_bar(stat = "identity",position="stack") + 
  scale_x_date(NULL, date_labels = "%Y", breaks = "year")+
  scale_y_continuous("Frequency")+
  theme(legend.position="right")

df<-solns %>%
  group_by(year=floor_date(as.Date(date_short),"year"),category) %>% 
  tally

ggplot(df, aes(x = year, y = n, fill=category)) + 
  geom_bar(stat = "identity",position="stack") + 
  scale_x_date(NULL, date_labels = "%Y", breaks = "year")+
  scale_y_continuous("Frequency")+
  theme(legend.position="right")

#####LDA model next week###########
#######LDA model on presidencies
install.packages(c('tm', 'SnowballC', 'wordcloud', 'topicmodels'))
library(tm)
library(tidytext)
library(dplyr)
library(topicmodels)

#first turn text into the right shape-> DocumentTerm Matrix
all_corpus = Corpus(VectorSource(allNYTSearch$response.docs.abstract))
all_corpus = tm_map(all_corpus, content_transformer(tolower))
all_corpus = tm_map(all_corpus, removeNumbers)
all_corpus = tm_map(all_corpus, removePunctuation)
all_corpus = tm_map(all_corpus, removeWords, c("the", "and","[\\'s]","[\\']","iht",'[[:punct:]]',"can","will", stopwords("english")))
all_corpus =  tm_map(all_corpus, stripWhitespace)
all_dtm <- DocumentTermMatrix(all_corpus)

#
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


