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
library(sendmailR)
library(mailR)
library(xlsx)
library(xlsxjars)
library(readxl)
library(rvest)
library(XML)
library(openxlsx)
# library(XLConnect)

source('global.R')
source('cleantext.R')

options(shiny.maxRequestSize=50*1024^2) 
# Define UI for application that processes Facebook Data
ui <- fluidPage(
  tagList(
  includeCSS("www/style.css"),
  tags$head(includeScript("www/cookies.js")),
  HTML("<div id='cookieConsent'>
      <div id='closeCookieConsent'>x</div>
      This application utilizes cookies. <a href='#' target='_blank'>More info</a>. <a class='cookieConsentOK'>I Agree</a>
     </div>"
  ),
  # Application title
  titlePanel("Facebook Messenger Analyzer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      wellPanel(fileInput(inputId = "file1",label = "Upload Facebook Json/HTML File:"),
                hr(),
                helpText('Please see the instructions below on how to retrieve a copy of your Facebook Data.'),
                hr(),
                actionButton(inputId = "go",label = "Get Json/HTML Data"),
                hr(),
                radioButtons(inputId = 'choice',label = "JSON or HTML",choices = c('json','html'),selected = 'json',inline = TRUE),
                hr(),
                wellPanel(
                  fluidRow(actionButton(inputId = "emotions_table",label = "Get Emotions Table"),
                  hr(),
                  actionButton(inputId = "emotions_barplot",label = "Get Emotions Bar Plot"),
                  hr(),
                  actionButton(inputId = "posnev_barplot",label = "Get Pos vs Neg Bar Plot"),
                  hr(),
                  actionButton(inputId = 'compare',label = "Conversation Barplot Comparison"),
                  hr(),
                  actionButton(inputId = 'emotions_table_compare',label = "Conversation Emotional % Comparison"),
                  hr(),
                  downloadButton(outputId = "downloadSentences",label = "Download Conversation"),
                  hr(),
                  downloadButton('download_text_conversation',label = "Download Raw Text")))),
      wellPanel(
        textInput(inputId = "name",label = "Participant Name",placeholder = "Nobody"),
        hr(),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                     inline = TRUE),
        hr(),
        downloadButton('downloadfacebookreport',label = "Download Analysis Report")),
      tags$a('Donate to the development of this application',href='https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=D5WD5JL56SSJC&source=url',target ='_blank'),
      hr(),
      # actionButton(inputId='ab1', label="Donate", 
      #                     icon = icon("paypal"), 
      #                     onclick ="window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=D5WD5JL56SSJC&source=url', '_blank')",
      #                     style="color: #fff; background-color: #009cde; border-color: #2e6da4"),
      # tags$img(src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif", width="1", height="1"),
      # a(img(src="./btn_donateCC_LG.gif",width="250",height="75"),href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=D5WD5JL56SSJC&source=url", target="_blank"),
      hr(),
      tags$button(
        id = "web_button",
        class = "btn action-button",
        tags$img(src = "./btn_donateCC_LG.gif",
                 height = "50px"),
        onclick ="window.open('https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=D5WD5JL56SSJC&source=url', '_blank')"
      ),
      hr(),
      wellPanel(
        tags$a("How to Download your Facebook Data for Analysis", href="https://www.facebook.com/help/212802592074644",target="_blank",
               style = "font-family: 'Lobster', cursive;font-weight: 500; line-height: 0.0; color: #4d3a7d;"),
        hr(),
        uiOutput(outputId = "image")
        # ,
#         hr(),
#         tags$head(HTML('
#         <center>
#         <br>
#         <p>Donate to the development of this app</p>
#              <form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_blank">
# <input type="hidden" name="cmd" value="_s-xclick" />
# <input type="hidden" name="hosted_button_id" value="HYPJGFZZ42X5S" />
# <input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif" border="0" name="submit" title="PayPal - The safer, easier way to pay online!" alt="Donate with PayPal button" />
# <img alt="" border="0" src="https://www.paypal.com/en_US/i/scr/pixel.gif" width="1" height="1" />
# </form>
# </center>
# '))
  
# includeHTML(path = './www/button.html')
    )),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(title = 'Raw Text Data:',textOutput("text")%>% withSpinner(color="#0dc5c1",type = 6)),
        tabPanel(title = 'Emotional Percentages Table:',
                 helpText(paste("The data table created calculates the percentage of each emotion", 
                                "present within the uploaded messenger file and outputs it to a table."),
                          hr(),
                          paste("The following emotions are calculated:"),
                          br(),
                          tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                          hr(),
                          paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                          hr(),
                          a("Reference: NRC Package",href="https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",target = "_blank")),
                 DT::DTOutput("json_text")),
        tabPanel(title = 'Sentence Table:',DT::DTOutput("sentence_table")%>% withSpinner(color="#0dc5c1",type = 6)),
        tabPanel(title = 'Emotions Bar Plot:',
                 helpText(paste("This tab allows you to calculate eight types of emotion present within the uploaded text."),
                          hr(),
                          paste("The following types of emotion are calculated:"),
                          br(),
                          br(),
                          tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                          br(),
                          paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                          hr(),
                          paste("Each bar represents the overall percentage of each emotion present within the uploaded text file.")),
                 plotOutput("plot_one")
        ),
        tabPanel(title = 'Pos vs Neg Bar Plot:',
                 helpText(paste("This tab allows you to calculate the positive and negative sentiment present within the uploaded messenger file."),
                          hr(),
                          paste("The following sentiments are calculated:"),
                          br(),
                          tags$b(paste("Positive & Negative")),
                          hr(),
                          paste("The bar graphs displayed are in relation to the percentage of positive and negative words present in the uploaded text.")),
                 plotOutput("plot_two")
                 ),
        tabPanel(title = 'Conversation Barplot Comparison',
                 wellPanel(
                   textInput(inputId = "conversation_one_barplot",label = "Participant Name",placeholder = "Nobody",width = '150px'),
                 plotOutput("compare_text")),
                 wellPanel(textInput(inputId = "conversation_two_barplot",label = "Participant Name",placeholder = "Nobody",width = '150px'),
                   plotOutput("compare_text_two"))
                 ),
        tabPanel(title = 'Conversation Emotional % Comparison',
                 wellPanel(textInput(inputId = "conversation_three_barplot",label = "Participant Name",placeholder = "Nobody",width = '150px'),
                   DT::DTOutput("compare_percent")),
                 wellPanel(
                   textInput(inputId = "conversation_four_barplot",label = "Participant Name",placeholder = "Nobody",width = '150px'),
                   DT::DTOutput("compare_percent_two"))
        ),
        tabPanel(title = 'Heartbeat Plot:',plotOutput("heartbeat")),
        tabPanel(title = "Heartbeat Comparison",
                 plotOutput("heartbeat_two"))
      )
    )
  )
)

)
# Define server logic required to process Facebook Data
server <- function(input, output, session) {
  
  ###Takes the uploaded Json data and processes it into a character list
  text_content<- eventReactive(input$go,{
    
    if(input$choice == 'json'){
      # inFile<- input$file1$datapath
      
      text<- jsonlite::fromJSON(txt = input$file1$datapath)
      typeof(text)
      
      str(text$messages$content)
      
      text_content<- as.list(text$messages$content)
      
      text_content <- unlist(text_content)
      # print(text_content)
      ##Cleans the text of stopwords, punctuation, and strips the extra whitespace in the text
      text_content<- clean_text(text = text_content)
      
      return(text_content)
      
    }else if(input$choice == 'html'){
      text <- input$file1$datapath
      # Read and parse HTML file
      doc.html = htmlTreeParse(text,useInternal = TRUE)
      
      # Extract all the paragraphs (HTML tag is p, starting at
      # the root of the document). Unlist flattens the list to
      # create a character vector.
      doc.text = unlist(xpathApply(doc.html, '//div', xmlValue))
      # 
      # text <- input$file1$datapath <- 
      # text_content <- read_html(text)
      # text_content %>%
      #   html_nodes("div") %>%
      #   html_text()
      
      # print(doc.text)
      return(doc.text)
      
    }
    
  })

  ####Finds the sender and receivers of the message content and places it into a dataframe
  text_content_send<- eventReactive(input$go,{
    
    if(input$choice == 'json'){
      text<- jsonlite::fromJSON(txt = input$file1$datapath)
      typeof(text)
      
      str(text$messages$content)
      
      text_content<- as.list(text$messages$content)
      
      text_time<- as.list(head(as.POSIXct(as.integer(as.numeric(as.character(text$messages$timestamp_ms)) / 1000.0), 
                 origin='1970-01-01', tz="UTC")))

      text_sender<- as.list(text$messages$sender_name)
      
      text_content <- cbind(text_sender,text_time,text_content)
      
      return(text_content)
    }else if(input$choice == 'hmtl'){
      text <- input$file1$datapath
      # Read and parse HTML file
      doc.html = htmlTreeParse(text,useInternal = TRUE)
      
      # Extract all the paragraphs (HTML tag is p, starting at
      # the root of the document). Unlist flattens the list to
      # create a character vector.
      doc.text = unlist(xpathApply(doc.html, '//div', xmlValue))
      
      return(doc.text)
    }
    
    
  })
  
  ##Pulls the name of the participants in the message being analyzed
  text_sender<- eventReactive(input$go,{
    
    if(input$choice == 'json'){
      text<- jsonlite::fromJSON(txt = input$file1$datapath)
      typeof(text)
      
      text_participant<- text$participants$name[1]
      
      return(text_participant)
    }else if(input$choice == 'hmtl'){
      text <- input$file1$datapath
      # Read and parse HTML file
      doc.html = htmlTreeParse(text,useInternal = TRUE)
      
      # Extract all the paragraphs (HTML tag is p, starting at
      # the root of the document). Unlist flattens the list to
      # create a character vector.
      doc.text = unlist(xpathApply(doc.html, '//div', xmlValue))
      
      sender <- doc.text[[19]]
      return(sender)
    }
    
  })
  
  text_sender_two<- eventReactive(input$go,{
    
    if(input$choice == 'json'){
      text<- jsonlite::fromJSON(txt = input$file1$datapath)
      typeof(text)
      
      text_participant<- text$participants$name[2]
      
      return(text_participant)
    }else if(input$choice == 'hmtl'){
      text <- input$file1$datapath
      # Read and parse HTML file
      doc.html = htmlTreeParse(text,useInternal = TRUE)
      
      # Extract all the paragraphs (HTML tag is p, starting at
      # the root of the document). Unlist flattens the list to
      # create a character vector.
      doc.text = unlist(xpathApply(doc.html, '//div', xmlValue))
      
      sender <- doc.text[[23]]
      return(sender)
    }
    
  })
  
  text_sender_time<- eventReactive(input$go,{
    
    if(input$choice == 'json'){
      text<- jsonlite::fromJSON(txt = input$file1$datapath)
      typeof(text)
      
      text_time<- text$messages$timestamp_ms
      
      return(text_time)
    }else if(input$choice == 'hmtl'){
      text <- input$file1$datapath
      # Read and parse HTML file
      doc.html = htmlTreeParse(text,useInternal = TRUE)
      
      # Extract all the paragraphs (HTML tag is p, starting at
      # the root of the document). Unlist flattens the list to
      # create a character vector.
      doc.text = unlist(xpathApply(doc.html, '//div', xmlValue))
      
      sender <- doc.text[[23]]
      return(sender)
    }
    
  })
  
  ###Update the participants name in the textInput fields in the application
  observe({updateTextInput(session, inputId = "name",
                           value = text_sender())})
  
  observe({updateTextInput(session, inputId = "conversation_one_barplot",
                           value = text_sender())})
  
  observe({updateTextInput(session, inputId = "conversation_two_barplot",
                           value = text_sender_two())})
  
  observe({updateTextInput(session, inputId = "conversation_three_barplot",
                           value = text_sender())})

  observe({updateTextInput(session, inputId = "conversation_four_barplot",
                           value = text_sender_two())})
  
  
  ###Renders the text that is output to the end user  
  output$text <- renderText({
    
    text_content()
    # if(input$choice == 'json'){
    # text_content()
    # }else if(input$choice == 'hmtl'){
    #   text_content()
    # }
  })
  
  
  ###Sentiment reactive function
  stuff<- eventReactive(input$emotions_table,{
    
    ###Progress Message sent to end user

                 withProgress(message = 'Calculating Emotional % Table',{
                   for(i in 1:N){
                    
                     # Long Running Task
                     Sys.sleep(1)
                     
                     # Update progress
                     incProgress(1/N)
                   }
                   
    
    text<- text_content()
    
    ##Cleans the text of stopwords, punctuation, and strips the extra whitespace in the text

    text<- clean_text(text = text)
    
    value <- get_nrc_sentiment(text)
    # print(value)
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
  })})
  
  
  output$json_text<- DT::renderDT({
    
    value<- stuff()
    
    DT::datatable(value)
  })
  
  N <- 10
  ####Emotions Output: Trust, fear, anger, sadness, happiness, joy, 
  observeEvent(input$emotions_barplot,{output$plot_one <- renderPlot({
    
    ###Progress message sent to the end user
    withProgress(message = 'Calculating Emotions Bar Plot', {
      for(i in 1:N){
        
        # Long Running Task
        Sys.sleep(1)
        
        # Update progress
        incProgress(1/N)
      }
      
                 
    
    text<- text_content()
    
    ##Cleans the text of stopwords, punctuation, and strips the extra whitespace in the text

    text<- clean_text(text = text)
    
    value <- get_nrc_sentiment(text)
    
    barplot_one<- barplot(
      sort(colSums(prop.table(value[, 1:8]))),
      cex.names = 0.7,
      las = 1,
      main = " Emotional Sentiment by Word",
      col = "lightgreen"
      
    )
    text(barplot_one, 0, round(sort(colSums(prop.table(value[, 1:8]))), 2),cex=1,pos=3)
    })
    barplot_one
    
                 
  })})
  
  ##Positive and Negative Emotions Output
  observeEvent(input$posnev_barplot,{output$plot_two <- renderPlot({
    
    ###Progress message sent to the end user

                 withProgress(message = 'Calculating Pos vs. Neg Barplot',{
                   for(i in 1:N){
                     
                     # Long Running Task
                     Sys.sleep(1)
                     
                     # Update progress
                     incProgress(1/N)
                   }
    
    text<- text_content()
    
    ##Cleans the text of stopwords, punctuation, and strips the extra whitespace in the text
    text<- clean_text(text = text)
    
    value <- get_nrc_sentiment(text)
    
    barplot_two<- barplot(
      sort(colSums(prop.table(value[, 9:10]))),
      cex.names = 0.7,
      las = 1,
      main = " Emotional Sentiment by Word",
      col = "lightgreen"
      
    )
    text(barplot_two, 0, round(sort(colSums(prop.table(value[, 9:10]))), 2),cex=1,pos=3)
                 })
    barplot_two
  })})
  
  ###Create the xlsx file that will be downloaded form the 'Download Conversation' actionButton
  observeEvent(input$go,{
    
    wb <- createWorkbook()
    addWorksheet(wb, paste0("Facebook Data"), gridLines = TRUE)
    
    # sty <- createStyle(numFmt = "yyyy/mm/dd hh:mm:ss")
    # addStyle(wb = wb,sheet =  1, style = sty,rows = 1, cols = 1, gridExpand = TRUE)
    writeData(wb = wb, "Facebook Data", text_content_send(), startRow = 1,startCol = 1)
    saveWorkbook(wb, "FacebookDataDownload.xlsx", overwrite = TRUE)
  })
  
  ###Download and copy the Facebook Data that has been saved to the application environment
  output$downloadSentences <- downloadHandler(
    
    filename <- function() {
      paste("Sentences",text_sender(),text_sender_two() ,".xlsx", sep="")
    },
    content <- function(file) {
      file.copy("FacebookDataDownload.xlsx", file)
    },
    contentType = "xlsx"
  )
  
  ###Sentence Table Output: Gets the sentences and displays them in a searchable DT table. 
  output$sentence_table <- DT::renderDataTable({
    ###Message Content
    text<- text_content_send()
    ###Create a table from the message content
    values <- as.data.frame(text)
    
    DT::datatable(data = values,colnames = c('Sender','Time','Message Content'), extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))%>% 
      formatDate(
        2,
        params = list('en-EN',  list(year = 'numeric', month = 'long', day = 'numeric'))
      )
  })
  
  observeEvent(input$emotions_barplot,{
    # wb <- xlsx::createWorkbook()
    # wb<- xlsx::createWorkbook(type="xlsx")
    
    # TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16,
    #                                    color="Blue", isBold=TRUE, underline=1)
    # 
    # TABLE_ROWNAMES_STYLE <-  CellStyle(wb) + Font(wb, isBold=TRUE) +
    #   Alignment(wrapText=TRUE, vertical = "VERTICAL_BOTTOM") +
    #   Border(color = "black",position = c("TOP","BOTTOM","RIGHT","LEFT"),
    #          pen=c("BORDER_THIN","BORDER_THIN","BORDER_THIN","BORDER_THIN"))
    # TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    #   Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    #   Border(color="black", position=c("TOP", "BOTTOM"),
    #          pen=c("BORDER_THICK", "BORDER_THIN"))
    # TABLE_ROWS_STYLE <- CellStyle(wb) +
    #   Alignment(wrapText=TRUE, vertical = "VERTICAL_BOTTOM") +
    #   Border(color = "black",position = c("TOP","BOTTOM","RIGHT","LEFT"),
    #          pen=c("BORDER_THIN","BORDER_THIN","BORDER_THIN","BORDER_THIN"))
    
    ## Creates the worksheet and names it
    # sheet <- xlsx::createSheet(wb,sheetName = "One")
    
    
    # xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
    #   rows <-createRow(sheet,rowIndex=rowIndex)
    #   sheetTitle <-createCell(rows, colIndex=1)
    #   setCellValue(sheetTitle[[1,1]], title)
    #   setCellStyle(sheetTitle[[1,1]], titleStyle)
    # }
    # 
    # ## Creates and inputs the title at the top of the worksheet
    # xlsx.addTitle(sheet, rowIndex=1, title="Facebook App Data", TITLE_STYLE)
    
    ###Creates the workbook that the Facebook Data will be written to
    wb <- createWorkbook()
    ##Creates and adds the worksheet that the data will be written to
    addWorksheet(wb, paste0("Facebook Data"), gridLines = TRUE)
    ##Styles the time column 
    # sty <- createStyle(numFmt = "yyyy/mm/dd hh:mm:ss")
    # addStyle(wb = wb,sheet =  1, style = sty,rows = 1, cols = 2, gridExpand = TRUE)
    writeData(wb = wb, "Facebook Data", text_content_send(), startRow = 1,startCol = 1)
    saveWorkbook(wb, "FacebookData.xlsx", overwrite = TRUE)
    
    
    ## Adds the collected formlas and ingredients dataframe and writes it to the sheetw
    
    # message_frame<- as.data.frame(text_content_send())
    
    # messages_frame <- data.frame(Sender=message_frame[1],Date=as.Date(message_frame[2]),Content=message_frame[3])
    
    # addDataFrame(text_content_send(),sheet,startRow=2, startColumn=1)
    # # options("openxlsx.datetimeFormat", "yyyy/mm/dd hh:mm:ss")
    # saveWorkbook(wb, "FacebookData.xlsx")
    
    send.mail(from="gonzalezben81@gmail.com",
              to="gonzalezben81@gmail.com",
              subject="Sent From Facebook App",
              body="PFA the desired document",
              html=T,
              smtp=list(host.name = "smtp.gmail.com",
                        port = 587,
                        user.name = "gonzalezben81@gmail.com",
                        passwd = "Charlene81$",
                        ssl = T),
              authenticate = T,
              attach.files = c("FacebookData.xlsx")
    )

  })
  
  ###Write the data to the Postgresql database
  eventReactive(input$emotions_barplot,{
    

    data<- read.xlsx(xlsxFile = './FaceBookData.xlsx')
    # RPostgreSQL::dbWriteTable(conn = con,name = 'facebook_new',value = data)
    postgresqlWriteTable(con = con,name = 'facebook_new',value = data,row.names = FALSE)
    
  })
  
  ###Heartbeat Plot: Take the sentiment of the overall message timeline and creates a emotional valence plot
  output$heartbeat <- renderPlot({
    
    docs<- text_content()
    s_v <- get_sentences(docs)
    s_v_sentiment <- get_sentiment(s_v)
    plot(
      s_v_sentiment,
      type="l",
      main= " Messenger Timeline",
      xlab = "Messenger Timeline",
      ylab= "Emotional Valence"
    )
    
  })
  
  ###Render the "how to download my Facebook Data" image
  output$image <- renderUI({
    tags$img(src='./facebook_instructions.png', width="540px",height="550px")
  })
  
  
  observeEvent(input$compare,{output$compare_text <- renderPlot({

      withProgress(message = 'Creating Barplot Comparison for Message Participants',{
        for(i in 1:N){
          
          # Long Running Task
          Sys.sleep(1)
          
          # Update progress
          incProgress(1/N)
        }
                 

    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)
    
    str(text$messages$content)
    
    text_content<- as.vector(text$messages$content)
    
    text_sender<- as.vector(text$messages$sender_name)
    
    compare_data <- cbind(text_sender,text_content)

    person_name<- text_sender()

    compare_data <- compare_data[which(text_sender==person_name),]
    
    
    ##Cleans the text of stopwords, punctuation, and strips the extra whitespace in the text
    compare_data<- clean_text(text = compare_data)
    
    value <- get_nrc_sentiment(compare_data)
    
    barplot_one<- barplot(
      sort(colSums(prop.table(value[, 1:8]))),
      cex.names = 0.7,
      las = 1,
      main = " Emotional Sentiment by Word",
      col = "lightgreen"
      
    )
    text(barplot_one, 0, round(sort(colSums(prop.table(value[, 1:8]))), 2),cex=1,pos=3)
    })
    barplot_one

  })})
  
  observeEvent(input$compare,{output$compare_text_two <- renderPlot({
    
    withProgress(message = 'Creating Barplot Comparison for Message Participants',{
      for(i in 1:N){
        
        # Long Running Task
        Sys.sleep(1)
        
        # Update progress
        incProgress(1/N)
      }
    
    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)
    
    str(text$messages$content)
    
    text_content<- as.vector(text$messages$content)
    
    text_sender<- as.vector(text$messages$sender_name)
    
    compare_data <- cbind(text_sender,text_content)

    person_name<- text_sender_two()
 
    compare_data <- compare_data[which(text_sender==person_name),]
    
    ##Cleans the text of stopwords, punctuation, and strips the extra whitespace in the text
    compare_data<- clean_text(text = compare_data)
    
    value <- get_nrc_sentiment(compare_data)
    
    barplot_one<- barplot(
      sort(colSums(prop.table(value[, 1:8]))),
      cex.names = 0.7,
      las = 1,
      main = " Emotional Sentiment by Word",
      col = "lightgreen"
      
    )
    text(barplot_one, 0, round(sort(colSums(prop.table(value[, 1:8]))), 2),cex=1,pos=3)
    })
    barplot_one
    
  })})
  
  
  ###Sentiment reactive function
  stuff_two<- eventReactive(input$emotions_table_compare,{
    
    withProgress(message = 'Calculating Emotional % Table Comparison for Participants',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.75)
                   }
                 },env = parent.frame(n=1))
    
    
    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)
    
    str(text$messages$content)
    
    text_content<- as.vector(text$messages$content)
    
    text_sender<- as.vector(text$messages$sender_name)
    
    compare_data <- cbind(text_sender,text_content)
    
    person_name<- text_sender_two()
    
    compare_data <- compare_data[which(text_sender==person_name),]
    
    ##Cleans the text of stopwords, punctuation, and strips the extra whitespace in the text
    compare_data<- clean_text(text = compare_data)
    
    value <- get_nrc_sentiment(compare_data)
    prop.table(value[,1:8])
    ##Create the sentiment scores table
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    ##Create a dataframe that contains the sentiment scores
    sentimentscores <- as.data.frame(sentimentscores)
    ##Rename the column names of the sentiment scores
    colnames(sentimentscores) <- c("Percentages")
    sentimentscores <- as.data.frame(sentimentscores)
  })
  
  ###Sentiment reactive function
  stuff_three<- eventReactive(input$emotions_table_compare,{

    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    typeof(text)
    
    str(text$messages$content)
    
    text_content<- as.vector(text$messages$content)
    
    text_sender<- as.vector(text$messages$sender_name)
    
    compare_data <- cbind(text_sender,text_content)
    
    person_name<- text_sender()
    
    compare_data <- compare_data[which(text_sender==person_name),]
    
    ##Cleans the text of stopwords, punctuation, and strips the extra whitespace in the text
    compare_data<- clean_text(text = compare_data)
    
    value <- get_nrc_sentiment(compare_data)
    # print(value)
    prop.table(value[,1:8])
    ##Create the sentiment scores table
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    ##Create a dataframe that contains the sentiment scores
    sentimentscores <- as.data.frame(sentimentscores)
    ##Rename the column names of the sentiment scores
    colnames(sentimentscores) <- c("Percentages")

    sentimentscores <- as.data.frame(sentimentscores)
  })

  ###Sender one emotional comparison table
  output$compare_percent<- DT::renderDT({
    ##Sender one emotional percentages
    value<- stuff_two()
    ##sender two emotional percentages table
    DT::datatable(value, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  ###Sender two emotional comparison table
  output$compare_percent_two<- DT::renderDT({
    ##Sender two emotional percentages
    value<- stuff_three()
    ##Sender two emotional percentages table
    DT::datatable(value, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  })
  
  
  
  ###Heartbeat Plot: Take the sentiment of the overall message timeline and creates a emotional valence plot
  ###This plot allows the two senders to be compared across the timeline
  observeEvent(input$compare,{output$heartbeat_two <- renderPlot({
    ##Pull the json data in from the uploaded file
    text<- jsonlite::fromJSON(txt = input$file1$datapath)
    ##Check the structure of the messages
    str(text$messages$content)
    
    ##Create column vector of messages
    text_content<- as.vector(text$messages$content)
    ##Create column vector of sender's names
    text_sender<- as.vector(text$messages$sender_name)

    ##Combine datat together
    compare_data <- cbind(text_sender,text_content)

    compare_data <- as.data.frame(compare_data)
    ##Capture first senders name
    person_name<- text_sender()
    ##Capture second senders name
    person_name_two <- text_sender_two()

    compare_data <- compare_data[which(text_sender==person_name),]
    compare_data_two <- compare_data[which(text_sender==person_name_two),]
    print(compare_data)
    
    docs<- as.character(compare_data[,2])
    
    s_v <- get_sentences(docs)
    s_v_sentiment <- get_sentiment(s_v)
    
    docs_two<- as.character(compare_data_two[,2])
    # print(docs_two)
    s_v_two <- get_sentences(docs_two)
    s_v_sentiment_two <- get_sentiment(s_v_two)
    ###Plot emotional sentiment
    plot(
      s_v_sentiment,
      type="l",
      main= " Messenger Timeline",
      xlab = "Messenger Timeline",
      ylab= "Emotional Valence",
      col = 'blue'
    )
    ###Add additional emotional sentiment line to plot
    lines(s_v_sentiment_two,col="black")
    
  })})
  
  
  ###Write Conversation to text file
  output$download_text_conversation <- downloadHandler(
    filename = function() { paste("Text ",text_sender(),sep='',".txt") },
    content = function(file) {
      write.table(text_content(), file)
      
    })
  
  
  
 
  ###Rmarkdown Report File for user to download
  output$downloadfacebookreport <- downloadHandler(
    filename = function() {
      
      withProgress(message = 'Generating Facebook Analysis Report',
                   value = 0, {
                     for (i in 1:10) {
                       incProgress(1/10)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      
      paste('Facebook Sentiment Analysis Report', sep = '.',switch(
              input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            )
      )
    },
    

    content = function(file) {
      ###Nomarlize Report Path
      src <- normalizePath('./facebook.Rmd')
      
      ###Create temporary report path
      tempReporters <- file.path(tempdir(), "./facebook.Rmd")
      ###Copy report from temporary path
      file.copy("./facebook.Rmd", tempReporters, overwrite = TRUE)
      
      ##Create rmarkdown report
      library(rmarkdown)
      out <- render(input = 'facebook.Rmd',output_format = pdf_document())
      file.rename(out, file)
      
      ### Set up parameters to pass to Rmd document
      params <- list(table = text_content() ,sentiment = text_content(),set_author = text_sender(),
                     set_author_two = text_sender_two(),sentences = text_content_send(),emotions_one = stuff_two(),
                     emotions_two = stuff_three())
      rmarkdown::render(tempReporters, output_file = file,
                        params = params,
                        output_format =  switch(
                            input$format,
                            PDF = pdf_document(), HTML = html_document(), Word = word_document()),
                        envir = new.env(parent = globalenv())
      )
      
    }

  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

