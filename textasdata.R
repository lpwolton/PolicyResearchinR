#+
# textasdata.R
# Oct. 8, 2020
# 
# basics on Text analysis
#Laura Wolton
#-

#Today's tasks:
#We will review data type and class basics, 
#run sentiment analysis, 
#and explore statistical packages.
#read in a number of files
#create a word frequency list
#
#
#

#uncomment these and run them if you have never installed these
#install.packages("readxl")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("RColorBrewer")
#install.packages("SentimentAnalysis")
#install.packages("fBasics")
#install.packages("lexRankr")
library(readxl)
library(stringr)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(SentimentAnalysis)
library(fBasics)
library(lexRankr)
library(ggplot2)

#get the working directory
getwd()
# you can paste strings together to create another
#create a string with the data directory in it
datadir<-paste(getwd(),"/data/",sep="")

#create an object with the list of files
dfiles<-list.files(datadir,include.dirs=TRUE,full.names=TRUE,pattern=".txt")


####Read the files into a dataframe########
#create an empty date frame the length of the 
text_df<-data.frame(matrix(nrow=length(dfiles), ncol=2))
colnames(text_df)<-c("title","text")
for (j in 1:length(dfiles)){
  #start reading in the files
  raw_ascii<- readLines(dfiles[j],n=-1)
  
  #remove all the empty lines
  raw_ascii<-raw_ascii[which(raw_ascii!="")]
  
  #save the title in the first column of the data
  text_df$title[j]<-raw_ascii[1]
  
  #I decided to clean my files within the reading for loop
  #this command splits the file off at the word Classification and 
  #the writer's email on the bottom and the word "Body" under the top header 
  startind<-which(raw_ascii=="Body")+1
  botind<-min(grep("\\w+\\.\\w+\\@\\w+\\.\\w+|Classification",raw_ascii))-1
  
  #store the text as a string in the second column
  text_df$text[j]<-paste(unlist(raw_ascii[startind:botind]),collapse='')
}
####Now all our text is stored in a dataframe####
View(text_df)

#Let's find out what words are in it
# some of this follows 
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
#for this particular exercise, we need to turn this into a Corpus
#Corpus are collections of documents containing text
txtCorp<-Corpus(VectorSource(text_df$text))
inspect(txtCorp)
#clean the corpus
#remove punctuation and common words
#if this doesn't work it's because you haven't run the cleanCorpus function
txtCorp<-cleanCorpus(txtCorp)

#Create a word frequency array
wf<-wordFreqArray(txtCorp)
#you could print this to aid your creation of a dictionary
write.csv(wf,"blm_wf.csv")

#this creates a word cloud with the word cloud function
set.seed(1234)
wordcloud(words = wf$word, freq = wf$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#this creates a barplot
barplot(wf[1:20,]$freq, las = 2, names.arg = wf[1:20,]$word,
        col ="orange", main ="Most frequent words",
        ylab = "Word frequencies")

#let's run some sentiment analysis
#this requires we go back to the data frame

#so this is not entirely meaningful, but you could 
#analyze the sentimteent of each whole article text
text_sen<-unnest_sentences(text_df,sents,text,doc_id=title)
sentres<-analyzeSentiment(text_sen$sents)

#I usually work with SentimentGI values
#it's based on a psychology dictionary
#so let's add a column to save some sentiment values
text_sen$senti<-0.0
text_sen$senti<-sentres$SentimentGI

#we can also store those word counts
text_sen$wc<-0.0
text_sen$wc<-sentres$WordCount

#these are some things we can do statistics on sentiment or word count
#comes from fBasics package
summary(mean(text_sen$senti))
summary(mean(text_sen$wc))

  ggplot(text_sen, aes(x=title, y=senti, fill=title)) +
  geom_violin(width=1.) +
  geom_boxplot(width=0.6, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("")+ylab("Sentiment")+ggtitle("ALL") +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())


