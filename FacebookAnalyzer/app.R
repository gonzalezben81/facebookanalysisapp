#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(jsonlite)
library(tm)
library(syuzhet)
library(DT)
library(dplyr)
library(shinycssloaders)
options(shiny.maxRequestSize=30*1024^2) 
# Define UI for application that processes Facebook Data
ui <- fluidPage(
   
   # Application title
   titlePanel("Data Analyzer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        wellPanel(fileInput(inputId = "file1",label = "Upload Facebook Json File:"),
        hr(),
        actionButton(inputId = "go",label = "Get Json Data"),
        actionButton(inputId = "emotions_table",label = "Get Emotions Table")),
        wellPanel(
        textInput(inputId = "name",label = "Participant Name",placeholder = "Nobody"),
        hr(),
        downloadButton('downloadfacebookreport',label = "Download Analysis Report"))
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
         tabPanel(title = 'Raw Text Data:',textOutput("text")%>% withSpinner(color="#0dc5c1",type = 6)),
         tabPanel(title = 'Emotional Percentages Table:',DT::DTOutput("json_text")%>% withSpinner(color="#0dc5c1",type = 6)),
         tabPanel(title = 'Sentence Table:',DT::DTOutput("sentence_table")%>% withSpinner(color="#0dc5c1",type = 6)),
         tabPanel(title = 'Emotions Bar Plot:',plotOutput("plot_one")%>% withSpinner(color="#0dc5c1",type = 6)),
         tabPanel(title = 'Pos vs Neg Bar Plot:',plotOutput("plot_two")%>% withSpinner(color="#0dc5c1",type = 6)),
         tabPanel(title = 'Heartbeat Plot:',plotOutput("heartbeat")%>% withSpinner(color="#0dc5c1",type = 6))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   ###Takes the uploaded Json data and processes it into a character list
  text_content<- eventReactive(input$go,{
    
    # inFile<- input$file1$datapath
    
    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)
    
    str(text$messages$content)
    
    text_content<- as.list(text$messages$content)
    
    text_content <- unlist(text_content)
    print(text_content)
    return(text_content)
    
  })
  
  
  ###Finds the sender and receivers of the message content and places it into a dataframe
  text_content_send<- eventReactive(input$go,{


    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)

    str(text$messages$content)

    text_content<- as.list(text$messages$content)
    text_sender<- as.list(text$messages$sender_name)

    text_content <- cbind(text_sender,text_content)

    return(text_content)

  })
  
  ##Pulls the name of the participants in the message being analyzed
  text_sender<- eventReactive(input$go,{


    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)


    text_participant<- text$participants$name[1]


    return(text_participant)

  })
  
  text_sender_two<- eventReactive(input$go,{
    
    
    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)
    
    
    text_participant<- text$participants$name[2]
    
    
    return(text_participant)
    
  })
  
  ###Update the participants name in the textInput field
  observe({updateTextInput(session, inputId = "name",
                  value = text_sender())})
  
  output$text <- renderText({

    text_content()
  })
  
  
  ###Sentiment reactive function
  stuff<- eventReactive(input$emotions_table,{

    text<- text_content()
    value <- get_nrc_sentiment(text)
    print(value)
    prop.table(value[,1:8])
    ##Create the sentiment scores table
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    # print(sentimentscores)
    ##Create a dataframe that contains the sentiment scores
    sentimentscores <- as.data.frame(sentimentscores)
    ##Rename the column names of the sentiment scores
    colnames(sentimentscores) <- c("Percentages")
    #
    # return(sentimentscores)
    sentimentscores <- as.data.frame(sentimentscores)
  })
  
  
  output$json_text<- DT::renderDT({

    value<- stuff()

    DT::datatable(value)
  })
  
  
 ####Emotions Output: Trust, fear, anger, sadness, happiness, joy, 
 output$plot_one <- renderPlot({

   text<- text_content()
   value <- get_nrc_sentiment(text)

   barplot_one<- barplot(
     sort(colSums(prop.table(value[, 1:8]))),
     cex.names = 0.7,
     las = 1,
     main = " Emotional Sentiment by Word",
     col = "lightgreen"

   )
   text(barplot_one, 0, round(sort(colSums(prop.table(value[, 1:8]))), 2),cex=1,pos=3)

   barplot_one
 })

 ##Positive and Negative Emotions Output
 output$plot_two <- renderPlot({

   text<- text_content()
   value <- get_nrc_sentiment(text)

   barplot_two<- barplot(
     sort(colSums(prop.table(value[, 9:10]))),
     cex.names = 0.7,
     las = 1,
     main = " Emotional Sentiment by Word",
     col = "lightgreen"

   )
   text(barplot_two, 0, round(sort(colSums(prop.table(value[, 9:10]))), 2),cex=1,pos=3)

   barplot_two
 })

 ###Sentence Table Output: Gets the sentences and displays them in a searchable DT table. 
 output$sentence_table <- DT::renderDataTable({

   text<- text_content_send()
   # values <- get_sentences(text)
   values <- as.data.frame(text)

   DT::datatable(values)

 })

 ###Heartbeat Plot: Take the sentiment of the overall message timeline and creates a emotional valence plot
 output$heartbeat <- renderPlot({

   docs<- text_content()
   s_v <- get_sentences(docs)
   s_v_sentiment <- get_sentiment(s_v)
   # name<- gsub(pattern = './messages',replacement = "",x = file_name)
   # name <- gsub(pattern = ".txt*",replacement = "",x = name)
   # name <- gsub(pattern = "/",replacement = "",x = name)
   # myfile_path<- file.path(".","nrc_heartbeat",paste0(name,"timeline heartbeat.pdf"))
   # pdf(file = myfile_path)
   plot(
     s_v_sentiment,
     type="l",
     main= " Messenger Timeline",
     xlab = "Messenger Timeline",
     ylab= "Emotional Valence"
   )

 })
 # 
 ###Rmarkdown Report File
 output$downloadfacebookreport <- downloadHandler(
   filename = function() {
     
     withProgress(message = 'Generating Facebook Analysis Report',
                  value = 0, {
                    for (i in 1:10) {
                      incProgress(1/10)
                      Sys.sleep(0.25)
                    }
                  },env = parent.frame(n=1))
     
     paste('Facebook Sentiment Analysis Report','pdf', sep = '.'
           #       switch(
           #   input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
           # )
     )
   },




   content = function(file) {
     src <- normalizePath('./facebook.Rmd')

     tempReporters <- file.path(tempdir(), "./facebook.Rmd")
     file.copy("./facebook.Rmd", tempReporters, overwrite = TRUE)

     #
     library(rmarkdown)
     out <- render(input = 'facebook.Rmd',output_format = pdf_document())
     file.rename(out, file)

     #     # Set up parameters to pass to Rmd document
     params <- list(table = text_content() ,sentiment = text_content(),set_author = text_sender(),
                    set_author_two = text_sender_two())
     rmarkdown::render(tempReporters, output_file = file,
                       params = params,
                       envir = new.env(parent = globalenv())
     )

   }
 )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

