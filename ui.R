library(shiny)
library(ggplot2)


shinyUI(
        pageWithSidebar(
                # Application title

        headerPanel("Next Word Prediction"),

        sidebarPanel(
                textInput("inputtext", "Text Input: ", "go to school"),
                
                actionButton("goButton", "SUBMIT"),

                helpText("Note: The algorithm will predict next word based on the last two words from you input"),
                
                helpText("If the algorithm cannot predict the next word based on the last two words",
                         "It will auto search the possible word from the last one word."),  
                
                helpText("It only consider about the problibility based on the input",
                         "No sementic check mechanism and capabilities.")
                
        ),

        mainPanel(
                
                helpText("The initial loading takes around 2 min, please wait"),
                
                helpText("Once the prediction word shown, you can choose one from prediction as next input word. And the system will auto based on the input to predict next word."),
                
                h3('The input text for algorithm:'),
                h4('Predict words starting with:'),
                textOutput("cleanresult"),
                
                h3('Next word prediction result:'),
                textOutput("outputtext") 

        )
        )
)



