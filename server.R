

source('data_loading.R', local=TRUE)


shinyServer(function(input, output) {

         output$cleanresult <- renderText({
                input$goButton
                t <- cleanInput(input$inputtext)
                print(t)
        })        
        
        output$outputtext <- renderText({
                p <- predict(cleanInput(input$inputtext))
                p1_pred <- as.integer(as.numeric(p[1,2]) * 100)
                p2_pred <- as.integer(as.numeric(p[2,2]) * 100)  
                p3_pred <- as.integer(as.numeric(p[3,2]) * 100)                
                p1 <- paste(p[1,1],"  ",p1_pred, "    ",p[2,1],"  ",p2_pred,"    ",p[3,1],"  ",p3_pred,sep="")
                print(p1)
                
                })
                

}
)




