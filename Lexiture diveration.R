library("data.table")
library("base")
library("quanteda")
library("dplyr")
library("ggplot2")
library("caret")
library("LaF")

## testing the lexical diversity
inTxt <- fread("~/datasciencecouresa/Final project/final/en_US/en_US.news.txt", sep = "\t", header = FALSE , stringsAsFactors=FALSE, encoding ="UTF-8")

#inTxt <- "final/en_US/en_US.blogs.txt"
#num_lines <- determine_nlines(inTxt)
num_lines <- length(inTxt$V1)
ratio <- c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,1)
len <- length(ratio)
ldiv <- data.frame(ratio=ratio,TTR=rep(0,len),CTTR=rep(0,len),U=rep(0,len))

for (n in 1:len) {
        txt <- sample_lines(as.character(inTxt), as.integer(num_lines*ratio[n]))
        txt <- cleanUpTxt(txt)
        txt <- toString(txt)
        tdfm <- dfm(txt, verbose=TRUE)
        ldiv$TTR[n] <- lexdiv(tdfm, measure="TTR")
        ldiv$CTTR[n] <- lexdiv(tdfm, measure="CTTR")
        ldiv$U[n] <- lexdiv(tdfm, measure="U")
        print(ldiv[n])
}
write.csv(ldiv, file="ldiv.csv")
library(ggplot2)
g <- ggplot(ldiv, aes(ratio, CTTR)) + stat_smooth()
g

cleanInput <-function(word) {
        ##
        ## This function call takes in a user passes parameter and does basic
        ## preprocessing such as remove punctuation (retaining apostrophes),
        ## remove and preceding spaces, and remove multiple spaces in phrase.
        ##
        ## It returns the processed user input
        ##
        word <- tolower(word) #ensure input is in lower case
        word <- gsub("[^[:alnum:][:space:]\']", "",word)
        word <- gsub("^[ ]{1,10}","",word)
        word <- gsub("[ ]{2,10}"," ",word)
        word <- gsub(" $","",word)        
        return(word)
}
