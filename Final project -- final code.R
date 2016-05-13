#####################################
## Library
## 
#####################################
library("data.table")
library("base")
library("quanteda")
library("dplyr")
library("ggplot2")
library("caret")
library("stringr")

#####################################
## Step 1:Data loading and prepare n-gram for News
## 
#####################################

DT_News <- fread("~/datasciencecouresa/Final project/final/en_US/en_US.news.txt", sep = "\t", header = FALSE , stringsAsFactors=FALSE, encoding ="UTF-8")

DT_News <- DT_News$V1
TR <- round(0.25*length(DT_News))
NewsTrain <- DT_News[1:TR]

DT_News_cleansing <- iconv(NewsTrain, "latin1", "ASCII","")
DT_News_cleansing <- gsub("[[:punct:]]", " ", DT_News_cleansing) 
DT_News_cleansing <- gsub('[0-9]+', '', DT_News_cleansing)  ## remove number
DT_News_cleansing <- gsub("[^[:alnum:]]", " ", DT_News_cleansing) 
DT_News_cleansing <- gsub(" [b-hj-z] "," ", DT_News_cleansing) # removes all single letters except "a" and "i"
corpus <- corpus(DT_News_cleansing)
tt <- collocations (corpus, method="lr", size = 3)
tt1 <- tt %>% filter(word1 != word2)
tt1 <- tt1 %>% filter(word2 != word3)
tt1 <- tt1 %>% filter(word1 != word3)
tt1 <- tt1 %>% filter(word1 != "u")
tt1 <- tt1 %>% filter(word2 != "u")

#tt2 <- head(tt1,1000)
tt1$order <- paste(tt1$word1,tt1$word2,tt1$G2,sep="")
require(plyr)
tt2 <- ddply(tt1, .(tt1$word1,tt1$word2), function(x) head(x[order(x$order, decreasing = TRUE), ], 5))

top <- paste(tt2$word1,tt2$word2,tt2$word3,sep = "_")  
threegramDF<-data.frame(Tri = top)
threegramDF$count <- tt2$count

bigram<-sub("_","@@@@",threegramDF$Tri)
bigram<-sub("_.*","",bigram)
threegramDF$Bi<-sub("@@@@"," ",bigram)
threegramDF$Uni<-sub(".* ","",threegramDF$Bi)
threegramDF$Tri <- gsub("_"," ",threegramDF$Tri)
threegramDF$w3<-sub(".* ","",threegramDF$Tri) # provides the suggested word

#write.csv(threegramDF,"threegramDF-News_25.csv")

tt <- threegramDF %>% filter(count > 1)
write.csv(tt,"threegramDF-News_25_REMOVE1.csv")

#####################################
## Step 2:Data loading and prepare n-gram for Blogs
## 
#####################################

DT_Blogs <- readLines("~/datasciencecouresa/Final project/final/en_US/en_US.blogs.txt", n = -1L, ok = TRUE, warn = TRUE,encoding = "UTF-8", skipNul = FALSE)

#DT_Blogs <- DT_Blogs$V1
TR <- round(0.25*length(DT_Blogs))
BlogsTrain <- DT_Blogs[1:TR]

DT_Blogs_cleansing <- iconv(BlogsTrain, "latin1", "ASCII","")
DT_Blogs_cleansing <- gsub("[[:punct:]]", " ", DT_Blogs_cleansing) 
DT_Blogs_cleansing <- gsub('[0-9]+', '', DT_Blogs_cleansing)  ## remove number
DT_Blogs_cleansing <- gsub("[^[:alnum:]]", " ", DT_Blogs_cleansing) 
DT_Blogs_cleansing <- gsub(" [b-hj-z] "," ", DT_Blogs_cleansing) # removes all single letters except "a" and "i"
corpus <- corpus(DT_Blogs_cleansing)
tt <- collocations (corpus, method="lr", size = 3)
tt1 <- tt %>% filter(word1 != word2)
tt1 <- tt1 %>% filter(word2 != word3)
tt1 <- tt1 %>% filter(word1 != word3)
tt1 <- tt1 %>% filter(word1 != "u")
tt1 <- tt1 %>% filter(word2 != "u")

#tt2 <- head(tt1,1000)
tt1$order <- paste(tt1$word1,tt1$word2,tt1$G2,sep="")
require(plyr)
tt2 <- ddply(tt1, .(tt1$word1,tt1$word2), function(x) head(x[order(x$order, decreasing = TRUE), ], 5))

top <- paste(tt2$word1,tt2$word2,tt2$word3,sep = "_")  
threegramDF<-data.frame(Tri = top)
threegramDF$count <- tt2$count

bigram<-sub("_","@@@@",threegramDF$Tri)
bigram<-sub("_.*","",bigram)
threegramDF$Bi<-sub("@@@@"," ",bigram)
threegramDF$Uni<-sub(".* ","",threegramDF$Bi)
threegramDF$Tri <- gsub("_"," ",threegramDF$Tri)
threegramDF$w3<-sub(".* ","",threegramDF$Tri) # provides the suggested word

#write.csv(threegramDF,"threegramDF-News_25.csv")

tt <- threegramDF %>% filter(count > 1)
write.csv(tt,"threegramDF-Blogs_25_REMOVE1.csv")

#####################################
## Step 3:Data loading and prepare n-gram for Twitter
## 
#####################################

DT_Twitter <- readLines("~/datasciencecouresa/Final project/final/en_US/en_US.twitter.txt", n = -1L, ok = TRUE, warn = TRUE,encoding = "UTF-8", skipNul = FALSE)

#DT_Twitter <- DT_Blogs$V1
TR <- round(0.25*length(DT_Twitter))
TwitterTrain <- DT_Twitter[1:TR]

DT_Twitter_cleansing <- iconv(TwitterTrain, "latin1", "ASCII","")
DT_Twitter_cleansing <- gsub("[[:punct:]]", " ", DT_Twitter_cleansing) 
DT_Twitter_cleansing <- gsub('[0-9]+', '', DT_Twitter_cleansing)  ## remove number
DT_Twitter_cleansing <- gsub("[^[:alnum:]]", " ", DT_Twitter_cleansing) 
DT_Twitter_cleansing <- gsub(" [b-hj-z] "," ", DT_Twitter_cleansing) # removes all single letters except "a" and "i"
corpus <- corpus(DT_Twitter_cleansing)
tt <- collocations (corpus, method="lr", size = 3)
tt1 <- tt %>% filter(word1 != word2)
tt1 <- tt1 %>% filter(word2 != word3)
tt1 <- tt1 %>% filter(word1 != word3)
tt1 <- tt1 %>% filter(word1 != "u")
tt1 <- tt1 %>% filter(word2 != "u")

#tt2 <- head(tt1,1000)
tt1$order <- paste(tt1$word1,tt1$word2,tt1$G2,sep="")
require(plyr)
tt2 <- ddply(tt1, .(tt1$word1,tt1$word2), function(x) head(x[order(x$order, decreasing = TRUE), ], 5))

top <- paste(tt2$word1,tt2$word2,tt2$word3,sep = "_")  
threegramDF<-data.frame(Tri = top)
threegramDF$count <- tt2$count

bigram<-sub("_","@@@@",threegramDF$Tri)
bigram<-sub("_.*","",bigram)
threegramDF$Bi<-sub("@@@@"," ",bigram)
threegramDF$Uni<-sub(".* ","",threegramDF$Bi)
threegramDF$Tri <- gsub("_"," ",threegramDF$Tri)
threegramDF$w3<-sub(".* ","",threegramDF$Tri) # provides the suggested word

#write.csv(threegramDF,"threegramDF-News_25.csv")

tt <- threegramDF %>% filter(count > 1)
write.csv(tt,"threegramDF-TWitter_25_REMOVE1.csv")

#####################################
## Step 4: Merge final dataset
## 
#####################################

threegramDF_all<-read.csv("threegramDF-News_25_REMOVE1.csv",encoding = "UTF-8")
threegramDF_all<-threegramDF_all[,-1]

twitter_df<-read.csv("threegramDF-TWitter_25_REMOVE1.csv",encoding = "UTF-8")
twitter_df<-twitter_df[-1]

blog_df<-read.csv("threegramDF-Blogs_25_REMOVE1.csv",encoding = "UTF-8")
blog_df<-blog_df[,-1]

head(threegramDF_all)
tail(threegramDF_all)

head(twitter_df)
tail(twitter_df)

head(blog_df)
tail(blog_df)

threegramDF_all<-rbind(threegramDF_all,twitter_df)
threegramDF_all<-rbind(threegramDF_all,blog_df)
rm(twitter_df)
rm(blog_df)

tt1 <- threegramDF_all
tt1$Tri <- as.character(tt1$Tri)
tt1$Bi <- as.character(tt1$Bi)        
tt1$Uni <- as.character(tt1$Uni)        
tt1$w3 <- as.character(tt1$w3)        
tt1$count <- as.numeric(tt1$count)  
Encoding(tt1$Tri) <-"UTF-8"
Encoding(tt1$Bi) <-"UTF-8"
Encoding(tt1$Uni) <-"UTF-8"
Encoding(tt1$w3) <-"UTF-8"

# tt1$order <- paste(tt1$Bi,tt1$count,sep="")
# tt2 <- ddply(tt1, .(tt1$Tri,tt1$Bi), function(x) head(x[order(x$order, decreasing = TRUE), ], 5))

write.csv(threegramDF_all,"threegramDF-finaldata.csv")

