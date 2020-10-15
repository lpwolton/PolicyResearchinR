#+
#corpusFunctions.R
# Laura Wolton
#functions we may need for working with corpus(corpora?)
#-

#this function will remove stopwords, extra space, and puncutation
cleanCorpus <- function(corp){
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  corp <- tm_map(corp, toSpace, "/")
  corp <- tm_map(corp, toSpace, "@")
  corp <- tm_map(corp, toSpace, "\\|")
  corp <- tm_map(corp, toSpace, "\'")
  # Convert the text to lower case
  corp <- tm_map(corp, content_transformer(tolower))
  # Remove numbers
  corp <- tm_map(corp, removeNumbers)
  # Remove english common stopwords
  corp <- tm_map(corp, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your stopwords as a character vector
  corp <- tm_map(corp, removeWords, c("will", "get","must","need","new",
      "also","said","say","can","make","I'll","and")) 
  # Remove punctuations
  corp <- tm_map(corp, removePunctuation)
  # Eliminate extra white spaces
  corp <- tm_map(corp, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  return(corp)
}

#this function will create a word Freq df
# must run library(NLP) & library(tm) with it for TermDocumentMatrix function
wordFreqArray<-function(corp){
  dtm <- TermDocumentMatrix(corp)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  head(d, 10)
  return(d)
}