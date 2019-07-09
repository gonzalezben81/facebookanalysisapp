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
options(shiny.maxRequestSize=30*1024^2) 
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Data Analyzer"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "file1",label = "Upload File"),
        actionButton(inputId = "go",label = "Get Json Data")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
         tabPanel(title = 'Raw Text Data:',textOutput("text")),
         tabPanel(title = 'Emotional Percentages Table:',DT::DTOutput("json_text")),
         tabPanel(title = 'Sentence Table:',DT::DTOutput("sentence_table")),
         tabPanel(title = 'Emotions Bar Plot:',plotOutput("plot_one")),
         tabPanel(title = 'Pos vs Neg Bar Plot:',plotOutput("plot_two"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

  text_content<- eventReactive(input$go,{
    
    
    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)
    
    str(text$messages$content)
    
    text_content<- as.list(text$messages$content)
    
    text_content <- unlist(text_content)
    
    return(text_content)
    
  })
  
  
  text_content_send<- eventReactive(input$go,{
    
    
    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)
    
    str(text$messages$content)
    
    text_content<- as.list(text$messages$content)
    text_sender<- as.list(text$messages$sender_name)
    
    text_content <- cbind(text_sender,text_content)
    
    return(text_content)
    
  })
  output$text <- renderText({

    text_content()
  })
  
  
  stuff<- eventReactive(input$go,{
    
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
    
    # prop.table(value[,1:8])
    # ##Create the sentiment scores table
    # sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    # # print(sentimentscores)
    # ##Create a dataframe that contains the sentiment scores
    # sentimentscores <- as.data.frame(sentimentscores)
    # ##Rename the column names of the sentiment scores
    # colnames(sentimentscores) <- c("Percentages")
    
    DT::datatable(value)
  })
  
  
  
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
 
 
 output$sentence_table <- DT::renderDataTable({
   
   text<- text_content_send()
   # values <- get_sentences(text)
   values <- as.data.frame(text)
   
   DT::datatable(values)
   
 })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

