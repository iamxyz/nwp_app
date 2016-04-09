# coding by Yulong Deng, updated 2016-4-8 
#=====================================================================================

library(shiny)

#library(DT)

examples <- 
    c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", 
    "You're the reason why I smile everyday. Can you follow me please? It would mean the",
    "Hey sunshine, can you follow me and make me the",
    "Very early observations on the Bills game: Offense still struggling but the",
    "Go on a romantic date at the",
    "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
    "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
    "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
    "Be grateful for the good times and keep the faith during the",
    "If this isn't the cutest thing you've ever seen, then you must be",
    "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
    "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
    "I'd give anything to see arctic monkeys this",
    "Talking to your mom has the same effect as a hug and helps reduce your",
    "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
    "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
    "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
    "Every inch of you is perfect from the bottom to the",
    "I’m thankful my childhood was filled with imagination and bruises from playing",
    "I like how the same people are in almost all of Adam Sandler's"
    )

 



shinyUI(pageWithSidebar(
    headerPanel("Predicition for Next Word"),
    sidebarPanel(
        
        #verbatimTextOutput("value"),
        radioButtons("type", label = h4("Select a input type"),
                     choices = list("Choose a sample phrase" = 1, "Input a new phrase" = 2),
                     selected = 1),
     
        #verbatimTextOutput('out2'),
        selectInput("text1", h4("Choose a sample phrase"), examples, selectize=FALSE),
        textInput(inputId="text2", label = h4("Input a new phrase")),
     
        actionButton("gobutton", "Predict Next Word"),
        
        HTML("<hr>"),
        HTML("<Font Color=blue>"),
        h4("Introduction:"),
        h5("The application is used to predict the next word based on the input phrase."),
        h4("Four steps:"),
        h5("Step 1: Select one of input types: choose a phrase from samples or type a new phrase."),
        h5("Step 2: According to the input type selected, choose a sample phrase from the options box or type a new phrase in the text input box."),
        h5("Step 3: Press the 'Predict Next Word' button and wait a minute for the application to work out the prediction."),
        h5("Step 4: The next word ( 1-3 candidate words provided ) will be shown."),
        HTML("<hr>"),
        h6("Author: Yulong Deng, Updated: 2016-4-10"),
        HTML("</Font>")
        #h6("Visit github for source code of the shiny app:"),
        #a("https://github.com/iamxyz/nwp_app")
    ),
    mainPanel(
       
        h4("Message Window:"),
        HTML("<Font Color=red>"),
        h5("The data will be loaded during the first time to lunch the application, please wait..."),
        HTML("</Font>"),
        HTML("<Font Color=green>"),
        h5(textOutput('messages')),
        HTML("</Font>"),
        HTML("<hr>"),
        h4('The phrase you inputed:'),
        textOutput('phrase'),
        HTML("<hr>"),
        h4("The candidate next word:"),
        DT::dataTableOutput("words_table"),
        HTML("<Font Color=blue>"),
        h5("Notes: The candidate words ( up to three ) are ordered by possibility , you can pick up one of them as the next suitable word."),
        HTML("<hr>"),
        HTML("</Font>")
       
        
        )
))



