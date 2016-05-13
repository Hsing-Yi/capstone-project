threegramDF_all<-read.csv("threegramDF-finaldata.csv",encoding = "UTF-8")
threegramDF_all<-threegramDF_all[,-1]

threegramDF_all$Tri <- as.character(threegramDF_all$Tri)
threegramDF_all$Bi <- as.character(threegramDF_all$Bi)        
threegramDF_all$Uni <- as.character(threegramDF_all$Uni)        
threegramDF_all$w3 <- as.character(threegramDF_all$w3)        
threegramDF_all$count <- as.numeric(threegramDF_all$count)  
Encoding(threegramDF_all$Tri) <-"UTF-8"
Encoding(threegramDF_all$Bi) <-"UTF-8"
Encoding(threegramDF_all$Uni) <-"UTF-8"
Encoding(threegramDF_all$w3) <-"UTF-8"

#calculate the most_common_word 
tt <- threegramDF_all %>% select(w3,count) %>% group_by(w3) %>% summarize(count = sum(count))
tt <- aggregate(threegramDF_all$count,list(threegramDF_all$w3),sum)
names(tt)<-c("w3","counts")
fre_use<-tt[order(tt$count,decreasing=T),]
fre_use <- head(fre_use,5)



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



################# predict

# input <- "  He wasn't"

predict <-function(input,cluster=5){
        useTri <- "NA"
        not_found <- 0
        input <- cleanInput(input)
        inputSize<-length(strsplit(input, " ")[[1]])
#        if (inputSize != 2) stop("Please input exactly two words.")    # error handling
        input <- paste(strsplit(input, " ")[[1]][[inputSize-1]], strsplit(input, " ")[[1]][[inputSize]], sep = " ")
        nCount <- sum(threegramDF_all[which(threegramDF_all$Bi==input),2])
        ## cannot find from Bi, find from Uni
        if (nCount == 0) {     
                input <- gsub(".* ","",input)    # isolates w2 as unigram
                nCount <- sum(threegramDF_all[which(threegramDF_all$Uni==input),2])
                if (nCount == 0) {useTri$w3 <- fre_use$w3}
                if (nCount > 0) { #stop("Can't find it in modeling.")     # error handling
                
                        seekTri<-grepl(paste("^",input,"$",sep=""),threegramDF_all$Uni)
                        subTri<-threegramDF_all[seekTri,] #subset relevant outputs
                        subTri<-aggregate(subTri$count,list(subTri$w3),sum)
                        names(subTri)<-c("w3","counts")
                        subTri<-subTri[order(subTri$counts,decreasing=T),]
                        useTri <- head(subTri,cluster)
                        #useTri <- buildTable (input,subTri,cluster)
                        for (i in 1:length(useTri$counts)) {
                                count = useTri[i,2]
                        }
                } else {not_found <- 1}
        } else {
                seekTri<-grepl(paste("^",input,"$",sep=""),threegramDF_all$Bi)
                subTri<-threegramDF_all[seekTri,] #subset relevant 3-grams
                subTri<-aggregate(subTri$count,list(subTri$w3),sum)
                names(subTri)<-c("w3","counts")
                subTri<-subTri[order(subTri$count,decreasing=T),]
                useTri <- head(subTri,cluster)
                #useTri <- buildTable (input,subTri,cluster)
                for (i in 1:length(useTri$counts)) {
                        count = useTri[i,2]
                }
        }
        
        predictWord <- "NA"
        predictWord <- data.frame(paste("[",Word=useTri$w3,"]"), stringsAsFactors=FALSE)
        #predictWord <- data.frame(Word=useTri$w3) 
        print(paste("Predict words starting with: ",toupper(input)))
        return(predictWord)
}






# cleanInput("sorry to   ")
#nwords <-predict(cleanInput("i'd like"))
