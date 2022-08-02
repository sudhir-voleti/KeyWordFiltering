#################################################
#               KeyWord Filtering               #
#################################################

library(shiny)
library(purrr)
library(tidyr)
library(DT)
library(stringr)
library(tools)
require(dplyr)
require(tidytext)


shinyUI(fluidPage(
  title = "Keyword Filtering",
  titlePanel(title=div(img(src="logo.png",align='right'),"Keyword Filtering")),
  
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload text file"),
    uiOutput('id_var'),
    uiOutput("doc_var"),
    textInput("wordl", ("Enter wordlist to be filtered separated by comma(,)"), value = "camera,screen,app,music,google"),
    
    #htmlOutput("pre_proc1"),
    #htmlOutput("pre_proc2"),
    
    
    actionButton(inputId = "apply",label = "Apply Changes", icon("refresh"))
    
  ),
  
  # Main Panel:
  mainPanel( 
    tabsetPanel(type = "tabs",
                #
                tabPanel("Overview & Example Dataset",h4(p("How to use this App")),
                         verbatimTextOutput('wordl'),
                         p("To use this app you need a document corpus in txt file format. Make sure each document is separated from another document with a new line character.
                           To do basic Text Analysis in your text corpus, click on Browse in left-sidebar panel and upload the txt file. Once the file is uploaded it will do the computations in 
                            back-end with default inputs and accordingly results will be displayed in various tabs.", align = "justify"),
                         p("If you wish to change the input, modify the input in left side-bar panel and click on Apply changes. Accordingly results in other tab will be refreshed
                           ", align = "Justify"),
                         h5("Note"),
                         p("You might observe no change in the outputs after clicking 'Apply Changes'. Wait for few seconds. As soon as all the computations
                           are over in back-end results will be refreshed",
                           align = "justify"),
                         #, height = 280, width = 400
                         br(),
                         h4(p("Download Sample text file")),
                         downloadButton('downloadData1', 'Download Nokia Lumia reviews txt file'),br(),br(),
                         #downloadButton('downloadData2', 'Download One Plus reviews txt file'), br(), br(),
                         #downloadButton('downloadData3', 'Download Uber App Store reviews CSV file'), br(), br(),
                         #downloadButton('downloadData4', 'Download Airline Tweets CSV file'), br(), 
                         br()
                         
                         
                ),
                tabPanel("Data Summary",
                         h4("Uploaded data size"),
                         verbatimTextOutput("up_size"),
                         h4("Sentence level summary"),
                         htmlOutput("text"),
                         hr(),
                         h4("Token level summary"),
                         htmlOutput("text2"),
                         hr(),
                         h4("Sample of uploaded datasest"),
                         DT::dataTableOutput("samp_data")
                ),
                tabPanel("Filtered Corpus",
                         h4("Sample Output"),
                         br() 
                )
                
    )
  )
)
)
