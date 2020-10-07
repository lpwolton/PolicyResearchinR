#+
# text_intro.R
# Oct. 1, 2020
# Always remind yourself what's in your file
# This file contains tricks to working with text 
#Laura Wolton
#-

#Today's tasks:
#check in on directories 
# read in a file
#look for strings
#work with regex
#create a word frequency table
#print out the file
#create a few basic plots

#you only need to install once but you need to call your #directories every time
install.packages("stringr")
#now you need to tell R you are going to use those libraries
library(stringr)

#get the working directory
getwd()
#if you want to change it set a new one 
#you can directly input a directory
datadir<-"C:/Users/YOURdIRECTORY/Documents/R_dir/"
# you can paste strings together to create another
datadir<-paste(getwd(),"/data/",sep="")

#now list the files
dfiles<-list.files(datadir,include.dirs=TRUE,full.names=TRUE)
head()

#this example is for ascii files
#pdfs,html or excel require different reading function

txt<-readLines(dfiles[1])
head(txt)
#these are separated by line breaks
length(txt)
#this is a simple vector
#try looking at different lines using indices
txt[20]


#the grep command finds indices of sentences that have a search term
grep("lives",txt,ignore.case=TRUE)
#these indcies can be used to inspect those sentences
txt[grep("lives",txt,ignore.case=TRUE)]

# use a pipe to say OR
wtxt<-txt[grep("lives | racism",txt,ignore.case=TRUE)]
length(wtxt)

# can find unknown patterns
wtxt<-txt[grep("[A-Z][A-Z][A-Z]\\s",txt,ignore.case=FALSE)]
wtxt[1]
#regex is the keyword
#it takes some real time to construct a good regex statement
#let's try another
nums<-txt[grep("20[0-9][0-9]\\s",txt,ignore.case=TRUE)]

#can use str_extract or str_extract_all to see what you are extracting
unlist(str_extract_all(txt,"\\.\\s[A-Z]\\w+\\s"))

#can use gsub or str_replace or str_replace_all to see what you are extracting
# str_replace_all(txt,"\\.\\s[A-Z]\\w+\\s","\\!\\!\\!")
#but that command didn't save it, it just showed you
#try saving it to a new vector
rtxt<-str_replace_all(txt,"\\.\\s[A-Z]\\w+\\s","\\!\\!\\!")
rtxt[21]
#or use 
rtxt<-gsub("lives","important lives",txt,ignore.case=TRUE)
rtxt[3]

#can use gsub or str_replace or str_replace_all to see what you are extracting
# str_replace_all(txt,"\\.\\s[A-Z]\\w+\\s","\\!\\!\\!")
#but that command didn't save it, it just showed you
#or use 
rtxt<-gsub("[[:upper:]]","\\?",txt)
rtxt[3]
#say we want to remove a colon
txt[35]
str_remove_all(txt[35],"\\:")

#can use gsub or str_replace or str_replace_all to see what you are extracting
# str_replace_all(txt,"\\.\\s[A-Z]\\w+\\s","\\!\\!\\!")
#but that command didn't save it, it just showed you
#or use 
txsent<-" this is a sentence with extra whitespace   "
str_trim(txsent)
#can nest these kinds of things for efficiency
paste(str_trim(txsent),".",sep="")


