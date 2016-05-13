
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

#####################################
## Calculate lexical diversity
## 
#####################################

DT_News <- fread("~/datasciencecouresa/Final project/final/en_US/en_US.news.txt", sep = "\t", header = FALSE , stringsAsFactors=FALSE, encoding ="UTF-8")

DT_News <- DT_News$V1

TR <- round(0.4*length(DT_News))
NewsTrain <- DT_News[1:TR]
#NewsTest<-DT_News[-(1:TR)]
#TR1 <- round(0.2*length(NewsTest))
#NewsValidation<-NewsTest[(1:TR1)]
#NewsTest<-NewsTest[-(1:TR1)]

corpus <- corpus(NewsTrain)
mydfm <- dfm(corpus, verbose = TRUE, toLower = TRUE, removePunct = TRUE, removeTwitter = TRUE, removeNumbers = TRUE)

# mydfm_Trigrams <- dfm(corpus, ngrams = 3, verbose = TRUE, toLower = TRUE, removePunct = TRUE, removeTwitter = TRUE, removeNumbers = TRUE)
# 
# mydfm_Trigrams_test <- dfm(NewsTest, ngrams = 3, verbose = TRUE, toLower = TRUE, removePunct = TRUE, removeTwitter = TRUE, removeNumbers = TRUE)

results <- data.frame(TTR = lexdiv(mydfm, "TTR"),
                      CTTR = lexdiv(mydfm, "CTTR"),
                      U = lexdiv(mydfm, "U"))

results <- lexdiv(mydfm, "TTR", drop = TRUE)

results <- lexdiv(mydfm_Trigrams, c("CTTR", "TTR", "U"),log.base = 10, drop = TRUE)
cor(lexdiv(mydfm_Trigrams, "all"))

#####################################
## Step 1:Data loading and prepare n-gram for News
## 
#####################################

## data loading
#DT_News1 <- readLines("~/datasciencecouresa/Final project/final/en_US/en_US.news.txt", n = -1L, ok = TRUE, warn = TRUE,encoding = "UTF-8", skipNul = TRUE)

DT_News <- fread("~/datasciencecouresa/Final project/final/en_US/en_US.news.txt", sep = "\t", header = FALSE , stringsAsFactors=FALSE, encoding ="UTF-8")

#length(DT_News$V1)

DT_News <- DT_News$V1

TR <- round(0.4*length(DT_News))
NewsTrain <- DT_News[1:TR]
#NewsTest<-DT_News[-(1:TR)]
#TR1 <- round(0.2*length(NewsTest))
#NewsValidation<-NewsTest[(1:TR1)]
#NewsTest<-NewsTest[-(1:TR1)]


DT_News_cleansing <- iconv(NewsTrain, "latin1", "ASCII","")
DT_News_cleansing <- gsub("[[:punct:]]", " ", DT_News_cleansing)  
DT_News_cleansing <- gsub("[^[:alnum:]]", " ", DT_News_cleansing) 
DT_News_cleansing <- gsub(" [b-hj-z] "," ", DT_News_cleansing) # removes all single letters except "a" and "i"




# DT_News_cleansing <- gsub("?", "", DT_News_cleansing)
# DT_News_cleansing <- gsub("-", " ", DT_News_cleansing)
# DT_News_cleansing <- gsub('[0-9]+', '', DT_News_cleansing)  ## remove number
# DT_News_cleansing <- gsub(':', ' ', DT_News_cleansing)
# DT_News_cleansing <- gsub("\'", '', DT_News_cleansing)
# DT_News_cleansing <- gsub("\\", '', DT_News_cleansing)
# DT_News_cleansing <- gsub('/', ' ', DT_News_cleansing)

# x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
# str_replace_all(x, "[[:punct:]]", " ")

corpus <- corpus(NewsTrain)
mydfm <- dfm(corpus, ngrams = 3, verbose = TRUE, toLower = TRUE, removeTwitter = TRUE, removeNumbers = TRUE)


## data cleansing
# DT_News_cleansing <- gsub("-", " ", DT_News)
# DT_News_cleansing <- gsub('[0-9]+', '', DT_News_cleansing)  ## remove number
# DT_News_cleansing <- gsub(':', ' ', DT_News_cleansing)  
# DT_News_cleansing <- gsub("^\'", '', DT_News_cleansing)  
# DT_News_cleansing <- gsub('/', ' ', DT_News_cleansing)  

length(DT_News_cleansing)
Loop_News <- DT_News_cleansing
rm(DT_News, DT_News_cleansing)

for (i in (1:20)) {
        #i<-1        
        ## generate corpus and dfm
        if (i !=20) {
                DT <- Loop_News[1:50000] 
                Loop_News <- Loop_News[50001:length(Loop_News)]
        }
        if (i == 20) {DT <- Loop_News[1:length(Loop_News)]}
        
        ## generate corpus and dfm
        corpus <- corpus(DT)
        summary(corpus, n = 5)
        
        mydfm_Trigrams <- dfm(corpus, ngrams = 3, verbose = TRUE, toLower = TRUE, removeTwitter = TRUE, removeNumbers = TRUE)
        
        c(ndoc(mydfm_Trigrams), nfeature(mydfm_Trigrams))
        
        ## prepare n-gram table
        top <- topfeatures(mydfm_Trigrams,nfeature(mydfm_Trigrams))
        threegramDF<-data.frame(Tri = names(top))   
        threegramDF$count <- unclass(top)
        
        threegramDF$Tri <- as.character(threegramDF$Tri)
        threegramDF$count <- as.numeric(threegramDF$count)  
        Encoding(threegramDF$Tri) <-"UTF-8"
        
        index <- grep("\'", threegramDF$Tri , ignore.case=TRUE, fixed=FALSE)   ### remove dirty data
        threegramDF <- threegramDF[-index,]
        
        bigram<-sub("_","@@@@",threegramDF$Tri)
        bigram<-sub("_.*","",bigram)
        threegramDF$Bi<-sub("@@@@"," ",bigram)
        threegramDF$Uni<-sub(".* ","",threegramDF$Bi)
        threegramDF$Tri <- gsub("_"," ",threegramDF$Tri)
        threegramDF$w3<-sub(".* ","",threegramDF$Tri) # provides the suggested word
        
        write.csv(threegramDF,paste("threegramDF-News_",i,".csv",sep=''))
        print(paste("Iteration",i,"of 20")) #provide user progress
}       

rm(list=ls())

#####################################
## Step 2:Data loading and prepare n-gram for Twitter
## 
#####################################


## data loading
DT_Twitter <- readLines("~/datasciencecouresa/Final project/final/en_US/en_US.twitter.txt", n = -1L, ok = TRUE, warn = TRUE,encoding = "UTF-8", skipNul = FALSE)

## data cleansing
DT_Twitter_cleansing <- gsub("-", " ", DT_Twitter)
DT_Twitter_cleansing <- gsub('[0-9]+', '', DT_Twitter_cleansing)  ## remove number
DT_Twitter_cleansing <- gsub(':', ' ', DT_Twitter_cleansing)  
DT_Twitter_cleansing <- gsub("^\'", '', DT_Twitter_cleansing)  
DT_Twitter_cleansing <- gsub('/', ' ', DT_Twitter_cleansing)  
DT_Twitter_cleansing <- gsub('xa', ' ', DT_Twitter_cleansing)
DT_Twitter_cleansing <- gsub('xb', ' ', DT_Twitter_cleansing) 
DT_Twitter_cleansing <- gsub('xc', ' ', DT_Twitter_cleansing) 
DT_Twitter_cleansing <- gsub('xe', ' ', DT_Twitter_cleansing) 
DT_Twitter_cleansing <- gsub(" www(.+) ", " ", DT_Twitter_cleansing)    # removes web site URLs
DT_Twitter_cleansing <-gsub(" [b-hj-z] "," ", DT_Twitter_cleansing) # removes all single letters except "a" and "i"


length(DT_Twitter_cleansing)
rm(DT_Twitter)
Loop_Twitter <- DT_Twitter_cleansing

for (i in (1:11)) {
        #i<-1        
        ## generate corpus and dfm
        if (i !=11) {
                DT <- Loop_Twitter[1:230000] 
                Loop_Twitter <- Loop_Twitter[230001:length(Loop_Twitter)]
        }
        if (i == 11) {DT <- Loop_Twitter[1:length(Loop_Twitter)]}
        
        corpus <- corpus(DT)
        summary(corpus, n = 5)
        mydfm_Trigrams <- dfm(corpus, ngrams = 3, verbose = TRUE, toLower = TRUE, removeTwitter = TRUE, removeNumbers = TRUE)
        c(ndoc(mydfm_Trigrams), nfeature(mydfm_Trigrams))
        rm(corpus)  ## release memory
        
        ## prepare n-gram table
        top <- topfeatures(mydfm_Trigrams,nfeature(mydfm_Trigrams))
        threegramDF<-data.frame(Tri = names(top))   
        threegramDF$count <- unclass(top)
        
        index <- grep("\'", threegramDF$Tri , ignore.case=TRUE, fixed=FALSE)   ### remove dirty data
        threegramDF <- threegramDF[-index,]
        
        index <- grep("xe", threegramDF$Tri , ignore.case=TRUE, fixed=FALSE)   ### remove dirty data
        threegramDF <- threegramDF[-index,]
        index <- grep("xa", threegramDF$Tri , ignore.case=TRUE, fixed=FALSE)   ### remove dirty data
        threegramDF <- threegramDF[-index,]
        index <- grep("xb", threegramDF$Tri , ignore.case=TRUE, fixed=FALSE)   ### remove dirty data
        threegramDF <- threegramDF[-index,]
        
        threegramDF$Tri <-gsub(" ","",  threegramDF$Tri) # removes all space        
        
        #        index <-  grep("threegramDF$Tri", iconv(threegramDF$Tri, "UTF-8", sub="threegramDF$Tri")) ### remove non ASCII data
        # threegramDF <- threegramDF[-index,]
        bigram<-sub("_","@@@@",threegramDF$Tri)
        bigram<-sub("_.*","",bigram)
        threegramDF$Bi<-sub("@@@@"," ",bigram)
        threegramDF$Uni<-sub(".* ","",threegramDF$Bi)
        threegramDF$Tri <- gsub("_"," ",threegramDF$Tri)
        threegramDF$w3<-sub(".* ","",threegramDF$Tri) # provides the suggested word
        write.csv(threegramDF,paste("threegramDF-Twitter_",i,".csv",sep=''))
        print(paste("Iteration",i,"of 10")) #provide user progress
        
}

rm(list=ls())

#####################################
## Step 3:Data loading and prepare n-gram for Blogs
## 
#####################################

## data loading
DT_Blogs <- readLines("~/datasciencecouresa/Final project/final/en_US/en_US.blogs.txt", n = -1L, ok = TRUE, warn = TRUE,encoding = "UTF-8", skipNul = FALSE)

## data cleansing
DT_Blogs_cleansing <- gsub("-", " ", DT_Blogs)
DT_Blogs_cleansing <- gsub('[0-9]+', '', DT_Blogs_cleansing)  ## remove number
DT_Blogs_cleansing <- gsub(':', ' ', DT_Blogs_cleansing)  
DT_Blogs_cleansing <- gsub("^\'", '', DT_Blogs_cleansing)  
DT_Blogs_cleansing <- gsub('/', ' ', DT_Blogs_cleansing)  
DT_Blogs_cleansing <- gsub(" www(.+) ", " ", DT_Blogs_cleansing)    # removes web site URLs
DT_Blogs_cleansing <-gsub(" [b-hj-z] "," ", DT_Blogs_cleansing) # removes all single letters 

length(DT_Blogs_cleansing)
rm(DT_Blogs)
Loop_Blogs <- DT_Blogs_cleansing

for (i in (1:20)) {
        #i <- 1
        ## generate corpus and dfm
        if (i !=20) {
                DT <- Loop_Blogs[1:45000] 
                Loop_Blogs <- Loop_Blogs[45001:length(Loop_Blogs)]
        }
        if (i == 20) {DT <- Loop_Blogs[1:length(Loop_Blogs)]}
        
        
        
        ## generate corpus and dfm
        corpus <- corpus(DT)
        summary(corpus, n = 5)
        
        tt <- collocations (corpus, method="lr", size = 3)
        
        #mydfm_Trigrams <- dfm(corpus, ngrams = 3, verbose = TRUE, toLower = TRUE, removeTwitter = TRUE, removeNumbers = TRUE)
        
        #c(ndoc(mydfm_Trigrams), nfeature(mydfm_Trigrams))
        rm(corpus)   ## release memory
        
        ## prepare n-gram table
        #top <- topfeatures(mydfm_Trigrams,nfeature(mydfm_Trigrams))
        top <- paste(tt$word1,tt$word2,tt$word3,sep = "_")
        
        threegramDF<-data.frame(Tri = top)
        threegramDF$count <- tt$count
        
        #threegramDF<-data.frame(Tri = names(top))   
        #threegramDF$count <- unclass(top)
        bigram<-sub("_","@@@@",threegramDF$Tri)
        bigram<-sub("_.*","",bigram)
        threegramDF$Bi<-sub("@@@@"," ",bigram)
        threegramDF$Uni<-sub(".* ","",threegramDF$Bi)
        threegramDF$Tri <- gsub("_"," ",threegramDF$Tri)
        threegramDF$w3<-sub(".* ","",threegramDF$Tri) # provides the suggested word
        
        index <- grep("[^[:alnum:][:space:]\']", threegramDF$Tri)
        threegramDF <- threegramDF[-index,]
        write.csv(threegramDF,paste("threegramDF-Blogs_",i,".csv",sep=''))
        print(paste("Iteration",i,"of 20")) #provide user progress
}

rm(list=ls())

#####################################
## Step 4:Merge n-gram for News, Blogs and Twitter
## Ready Markov matrix
#####################################

######## merge twitter
twitter_df <- NA

for (i in (1:11)) {
        #i<-1
        tmp_df <-read.csv(paste("threegramDF-Twitter_",i,".csv",sep=''))
        tmp_df <-tmp_df[-1]
        index <- grep("<U", tmp_df$Tri , ignore.case=TRUE, fixed=FALSE)   ### remove dirty data
        tmp_df <- tmp_df[-index,]
        index <- grep("-[a-z]",tmp_df$Tri)
        tmp_df <- tmp_df[-index,]
        
        tmp_df$Tri <- as.character(tmp_df$Tri)
        tmp_df$Bi <- as.character(tmp_df$Bi)        
        tmp_df$Uni <- as.character(tmp_df$Uni)        
        tmp_df$w3 <- as.character(tmp_df$w3)        
        tmp_df$count <- as.numeric(tmp_df$count)  
        
        Encoding(tmp_df$Tri) <-"UTF-8"
        Encoding(tmp_df$Bi) <-"UTF-8"
        Encoding(tmp_df$Uni) <-"UTF-8"
        Encoding(tmp_df$w3) <-"UTF-8"
        twitter_df <- rbind(twitter_df,tmp_df)
        print(paste("Merged",i,"of 11")) #provide user progress
}        
write.csv(twitter_df,"threegramDF-Twitter_all.csv")
rm(list=ls())


tmp_df <-read.csv("threegramDF-Twitter_all.csv",encoding = "UTF-8")
tmp_df <- tmp_df[-1,-1]
tmp_df$Tri <- as.character(tmp_df$Tri)
tmp_df$Bi <- as.character(tmp_df$Bi)        
tmp_df$Uni <- as.character(tmp_df$Uni)        
tmp_df$w3 <- as.character(tmp_df$w3)        
tmp_df$count <- as.numeric(tmp_df$count)  
Encoding(tmp_df$Tri) <-"UTF-8"
Encoding(tmp_df$Bi) <-"UTF-8"
Encoding(tmp_df$Uni) <-"UTF-8"
Encoding(tmp_df$w3) <-"UTF-8"

tt <- tmp_df %>% group_by(Tri,Bi, Uni, w3) %>% summarise(count = sum(count))
write.csv(tt,"threegramDF-Twitter_sum.csv")


######## merge blog

blog_df <- NA

for (i in (1:20)) {
        #i<-1
        tmp_df <-read.csv(paste("threegramDF-Blogs_",i,".csv",sep=''),encoding = "UTF-8")
        tmp_df <-tmp_df[-1]
        index <- grep("<U", tmp_df$Tri , ignore.case=TRUE, fixed=FALSE)   ### remove dirty data
        if ( min(index) >0 ) {tmp_df <- tmp_df[-index,]}
        index <- grep('.com',tmp_df$Tri)
        if ( min(index) >0 ) {tmp_df <- tmp_df[-index,]}
        index <- grep('.ca',tmp_df$Tri)
        if ( min(index) >0 ) {tmp_df <- tmp_df[-index,]}
        index <- grep('amazon.',tmp_df$Tri)
        if ( min(index) >0 ) {tmp_df <- tmp_df[-index,]}

        tmp_df$Tri <- as.character(tmp_df$Tri)
        tmp_df$Bi <- as.character(tmp_df$Bi)        
        tmp_df$Uni <- as.character(tmp_df$Uni)        
        tmp_df$w3 <- as.character(tmp_df$w3)        
        tmp_df$count <- as.numeric(tmp_df$count)  
        
        Encoding(tmp_df$Tri) <-"UTF-8"
        Encoding(tmp_df$Bi) <-"UTF-8"
        Encoding(tmp_df$Uni) <-"UTF-8"
        Encoding(tmp_df$w3) <-"UTF-8"
        blog_df <- rbind(blog_df,tmp_df)
        print(paste("Merged",i,"of 20")) #provide user progress
}        
write.csv(blog_df,"threegramDF-Blogs_all.csv")
rm(list=ls())


tmp_df <-read.csv("threegramDF-Blogs_all.csv",encoding = "UTF-8")
tmp_df <- tmp_df[-1,-1]
tmp_df$Tri <- as.character(tmp_df$Tri)
tmp_df$Bi <- as.character(tmp_df$Bi)        
tmp_df$Uni <- as.character(tmp_df$Uni)        
tmp_df$w3 <- as.character(tmp_df$w3)        
tmp_df$count <- as.numeric(tmp_df$count)  
Encoding(tmp_df$Tri) <-"UTF-8"
Encoding(tmp_df$Bi) <-"UTF-8"
Encoding(tmp_df$Uni) <-"UTF-8"
Encoding(tmp_df$w3) <-"UTF-8"

index <- grep('_',tmp_df$Tri)
tmp_df <- tmp_df[-index,]

#tt1 <- subset(tt1,1000,10000)
tt <- tmp_df %>% group_by(Tri,Bi, Uni, w3) %>% summarise(count = sum(count))
write.csv(tt,"threegramDF-Blogs_sum.csv")


######## merge News

news_df <- NA

for (i in (1:20)) {
        #i<-1
        tmp_df <-read.csv(paste("threegramDF-News_",i,".csv",sep=''),encoding = "UTF-8")
        tmp_df <-tmp_df[-1]
        index <- grep("<U", tmp_df$Tri , ignore.case=TRUE, fixed=FALSE)   ### remove dirty data
        if ( min(index) >0 ) {tmp_df <- tmp_df[-index,]}
        index <- grep('.com',tmp_df$Tri)
        if ( min(index) >0 ) {tmp_df <- tmp_df[-index,]}
        index <- grep('.ca',tmp_df$Tri)
        if ( min(index) >0 ) {tmp_df <- tmp_df[-index,]}
        index <- grep('amazon.',tmp_df$Tri)
        if ( min(index) >0 ) {tmp_df <- tmp_df[-index,]}

        
        tmp_df$Tri <- as.character(tmp_df$Tri)
        tmp_df$Bi <- as.character(tmp_df$Bi)        
        tmp_df$Uni <- as.character(tmp_df$Uni)        
        tmp_df$w3 <- as.character(tmp_df$w3)        
        tmp_df$count <- as.numeric(tmp_df$count)  
        
        Encoding(tmp_df$Tri) <-"UTF-8"
        Encoding(tmp_df$Bi) <-"UTF-8"
        Encoding(tmp_df$Uni) <-"UTF-8"
        Encoding(tmp_df$w3) <-"UTF-8"
        news_df <- rbind(news_df,tmp_df)
        print(paste("Merged",i,"of 20")) #provide user progress
}        


tmp_df <-read.csv("threegramDF-News_sum.csv",encoding = "UTF-8")
tmp_df <- tmp_df[-1,-1]
tmp_df$Tri <- as.character(tmp_df$Tri)
tmp_df$Bi <- as.character(tmp_df$Bi)        
tmp_df$Uni <- as.character(tmp_df$Uni)        
tmp_df$w3 <- as.character(tmp_df$w3)        
tmp_df$count <- as.numeric(tmp_df$count)  
Encoding(tmp_df$Tri) <-"UTF-8"
Encoding(tmp_df$Bi) <-"UTF-8"
Encoding(tmp_df$Uni) <-"UTF-8"
Encoding(tmp_df$w3) <-"UTF-8"

#index <- grep('_',tmp_df$Tri)
tmp_df <- tmp_df[-index,]


news_df <- tmp_df

tt <- news_df %>% group_by(Tri,Bi, Uni, w3) %>% summarise(count = sum(count))

index <- grep("_",tt$Tri)
tt <- tt[-index,]

index <- grep("^ ",tt$Tri)
tt <- tt[-index,]

index <- grep("[^[:alnum:][:space:]\']", tt$Tri)
tt <- tt[-index,]

index <- grep("aaa", tt$Tri)
tt <- tt[-index,]

index <- grep("aa ", tt$Tri)
tt <- tt[-index,]

write.csv(tt,"threegramDF-News_sum.csv")
rm(list=ls())

######## merge three data together

threegramDF_all<-read.csv("threegramDF-News_sum.csv",encoding = "UTF-8")
threegramDF_all<-threegramDF_all[,-1]

twitter_df<-read.csv("threegramDF-Twitter_sum.csv",encoding = "UTF-8")
twitter_df<-twitter_df[-1]

blog_df<-read.csv("threegramDF-Blogs_sum.csv",encoding = "UTF-8")
#blog_df<-blog_df[-1,]
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

threegramDF_all$Tri <- as.character(threegramDF_all$Tri)
threegramDF_all$Bi <- as.character(threegramDF_all$Bi)        
threegramDF_all$Uni <- as.character(threegramDF_all$Uni)        
threegramDF_all$w3 <- as.character(threegramDF_all$w3)        
threegramDF_all$count <- as.numeric(threegramDF_all$count)  

Encoding(threegramDF_all$Tri) <-"UTF-8"
Encoding(threegramDF_all$Bi) <-"UTF-8"
Encoding(threegramDF_all$Uni) <-"UTF-8"
Encoding(threegramDF_all$w3) <-"UTF-8"


tt <- threegramDF_all %>% group_by(Tri,Bi, Uni, w3) %>% summarise(count = sum(count))
tt$Tri <- as.character(tt$Tri)
tt$Bi <- as.character(tt$Bi)        
tt$Uni <- as.character(tt$Uni)        
tt$w3 <- as.character(tt$w3)        
tt$count <- as.numeric(tt$count)  
Encoding(tt$Tri) <-"UTF-8"
Encoding(tt$Bi) <-"UTF-8"
Encoding(tt$Uni) <-"UTF-8"
Encoding(tt$w3) <-"UTF-8"

### remove dirty data
index <- grep("_",tt$Tri)
tt <- tt[-index,]

index <- grep("^ ",tt$Tri)
tt <- tt[-index,]

index <- grep("[^[:alnum:][:space:]\']", tt$Tri)
tt <- tt[-index,]

index <- grep("aaa", tt$Tri)
tt <- tt[-index,]

index <- grep("aa ", tt$Tri)
tt <- tt[-index,]

##################################
##
##################################

small_tt <- filter(tt, count <5)
write.csv(small_tt,"threegramDF_sum_small.csv")
normal_tt <- filter(tt, count >=5)
write.csv(normal_tt,"threegramDF_sum_normal.csv")

rm(list=ls())





##################################
##  reduce size
##################################

threegramDF_all<-read.csv("threegramDF_sum_small.csv",encoding = "UTF-8")
threegramDF_all<-threegramDF_all[,-1]

normal_df<-read.csv("threegramDF_sum_normal.csv",encoding = "UTF-8")
normal_df<-normal_df[-1]

head(threegramDF_all)
tail(threegramDF_all)

head(normal_df)
tail(normal_df)

threegramDF_all<-rbind(threegramDF_all,normal_df)
rm(normal_df)

write.csv(threegramDF_all,"threegram_final.csv",sep='')


tt <- threegramDF_all

#tt <- threegramDF_all %>% group_by(Tri,Bi, Uni, w3) %>% summarise(count = sum(count))
tt$Tri <- as.character(tt$Tri)
tt$Bi <- as.character(tt$Bi)        
tt$Uni <- as.character(tt$Uni)        
tt$w3 <- as.character(tt$w3)        
tt$count <- as.numeric(tt$count)  
Encoding(tt$Tri) <-"UTF-8"
Encoding(tt$Bi) <-"UTF-8"
Encoding(tt$Uni) <-"UTF-8"
Encoding(tt$w3) <-"UTF-8"

# threegramDF_all$Tri <- as.character(threegramDF_all$Tri)
# threegramDF_all$Bi <- as.character(threegramDF_all$Bi)        
# threegramDF_all$Uni <- as.character(threegramDF_all$Uni)        
# threegramDF_all$w3 <- as.character(threegramDF_all$w3)        
# threegramDF_all$count <- as.numeric(threegramDF_all$count)  
# Encoding(threegramDF_all$Tri) <-"UTF-8"
# Encoding(threegramDF_all$Bi) <-"UTF-8"
# Encoding(threegramDF_all$Uni) <-"UTF-8"
# Encoding(threegramDF_all$w3) <-"UTF-8"
# 
# ## while loop for i in (1:dim(threegramDF_all)[1])
# tt <- threegramDF_all

#tt<- threegramDF_all %>% filter(Bi == "aamir khan")
dim(tt)
final_tt <- "NA"
i <-1


for (i in (1: 1000)) {
        if (tt[1,1] != 'NA') {
                #i <- i+1
                inputsize <- length(strsplit(as.character(tt[1,1]), " ")[[1]])
                input <- tt[1,2]
                input
                if (inputsize != 3) {
                        tt <- tt[-1,]  
                        break
                } else {
                        #nCount <- sum(tt[which(tt$Bi==input),5])
                        seekTri<-grepl(paste("^",input,"$",sep=""),tt$Bi)
                        subTri<-tt[seekTri,] #subset relevant outputs
                        p <- paste(i,dim(final_tt)[1],dim(tt)[1],sep="--")
                        print(p)
                        
                        if (dim(subTri)[1] == 1) {
                                final_tt <- rbind(final_tt,subTri)  ## add to final set
                                tt <- tt[-seekTri,]  ## remove from tt
                                #break
                        } else {
                                subTri<-aggregate(subTri$count,list(subTri$w3),sum)
                                names(subTri)<-c("w3","counts")
                                subTri<-subTri[order(subTri$counts,decreasing=T),]
                                useTri <- head(subTri,5)
                                tt_tmp <- tt %>% filter(tt$Bi == input , tt$w3 %in% useTri$w3)
                                final_tt <- rbind(final_tt,tt_tmp)
                                tt <- tt %>% filter(Bi != input)
                                #break
                        }
                }        
        }
}


write.csv(final_tt,"threegramDF-reduced.csv")


##################################
##  Calculate Accuracy
##################################

## test the accuracy of prediction

# DT_News <- fread("~/datasciencecouresa/Final project/final/en_US/en_US.Blogs.txt", sep = "\t", header = FALSE , stringsAsFactors=FALSE, encoding ="UTF-8")
DT_Twitter <- readLines("~/datasciencecouresa/Final project/final/en_US/en_US.twitter.txt", n = -1L, ok = TRUE, warn = TRUE,encoding = "UTF-8", skipNul = FALSE)


DT_News <- fread("~/datasciencecouresa/Final project/final/en_US/en_US.news.txt", sep = "\t", header = FALSE , stringsAsFactors=FALSE, encoding ="UTF-8")

DT_Blogs <- readLines("~/datasciencecouresa/Final project/final/en_US/en_US.blogs.txt", n = -1L, ok = TRUE, warn = TRUE,encoding = "UTF-8", skipNul = FALSE)

nline <- 30
#txt <- as.character(sample_n(DT_Twitter, nline))
txt <- tail(DT_Blogs,30)
txt <- strsplit(as.character(txt), split = " ")
tmatch <- 0

i <- 1
for (i in 1:nline) {
        match <- 0
        ntxt <- length(txt[[i]])
        sentance <- ""
        for (j in (1:ntxt-1)) {
                sentance <- paste (sentance, txt[[i]][j], sep=" ")
                if (j > 1) {
                        n_words <- predict(cleanInput(tolower(sentance)))
                        if (n_words != "NA") {
                                #                n_words <- nextNWords(sentance, tweet_ws, 5)
                                if (tolower(txt[[i]][j+1]) %in% as.character(n_words[[1]])) match <- match+1
                        }   
                }
        }
        #print (c(match, length(txt[[i]])))
        tmatch <- tmatch + match
}


acc_news <- c("News", tmatch/sum(sapply(txt, length)))
acc_news  #23%

acc_Blogs <- c("Blogs", tmatch/sum(sapply(txt, length)))
acc_Blogs  #22%

acc_Twitter <- c("Twitter", tmatch/sum(sapply(txt, length)))
acc_Twitter  17%

