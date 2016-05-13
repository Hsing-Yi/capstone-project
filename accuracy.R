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
                        if ( ! n_words == "NA") {
                                #                n_words <- nextNWords(sentance, tweet_ws, 5)
                                if (tolower(txt[[i]][j+1]) %in% as.character(n_words[[1]])) {
                                        match <- match+1
                                        print(paste(tolower(txt[[i]][j+1]),n_words[[1]], sep = " "))
                                        print(paste("tmatch:",tmatch,match,sep = "-"))
                                }        
                        }   
                }
        }
        #print (c(match, length(txt[[i]])))
        tmatch <- tmatch + match
}
