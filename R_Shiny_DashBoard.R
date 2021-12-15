library("ggplot2")
library(readxl)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(shiny)
library(kableExtra)
library(dplyr)
bitcoin=read_excel('bitcoin.xlsx')
bitcoin=select(bitcoin,-'link')
bitcoin$UserID=1:nrow(bitcoin)
bitcoin=select(bitcoin,c('UserID','Username','Date','Tweet','Favorites','Retweets','ANGER','FEAR',
                         'SADNESS','DISGUST','JOY','TRUST','SURPRISE','ANTICIPATION','POSITIVE','NEGATIVE'))
ANGER=filter(bitcoin,ANGER==1)
FEAR=filter(bitcoin,FEAR==1)
SADNESS=filter(bitcoin,SADNESS==1)
DISGUST=filter(bitcoin,DISGUST==1)
JOY=filter(bitcoin,JOY==1)
TRUST=filter(bitcoin,TRUST==1)
SURPRISE=filter(bitcoin,SURPRISE==1)
ANTICIPATION=filter(bitcoin,ANTICIPATION==1)
POSITIVE=filter(bitcoin,POSITIVE==1)
NEGATIVE=filter(bitcoin,NEGATIVE==1)
text_data=read_excel('bitcoin.xlsx')
#create corpus
text_data=Corpus(VectorSource(text_data))
#convert the into lower case
text=tm_map(text_data,content_transformer(tolower))
inspect(text_data)
#remove numbers
text=tm_map(text,removeNumbers)
#Remove stop words
text=tm_map(text,removeWords,stopwords("english"))
#remove unwanted words
text=tm_map(text,removeWords,c("AAAA","BBB"))
#remove punctuations
text=tm_map(text,removePunctuation)
#eliminate extra white space
text=tm_map(text,stripWhitespace)
#text stemming
text=tm_map(text,stemDocument)
#Term document matrix
tdm=TermDocumentMatrix(text)
tdm_matrix=as.matrix(tdm)
#to get rowsum
row_sum=sort(rowSums(tdm_matrix),decreasing = TRUE)
#form data frame
ui <- fluidPage(
  # Application title
  titlePanel(h4('Word Cloud of  "My Social Pulse" ',align='center')),
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 10,  max = 500, value = 100),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 300,  max = nrow(bitcoin),  value = 1000),
      selectInput('dataset','select the dataset',choices = c('bitcoin','ANGER','FEAR','SADNESS','DISGUST','JOY',
                                                             'TRUST','SURPRISE','ANTICIPATION','POSITIVE','NEGATIVE')),
      sliderInput("mpg",
                  "Maximum Number of Fetch data:",
                  min = 1,  max = nrow(bitcoin),  value = 100),
      selectInput(inputId="color1",label="Choose Plot Color",choices = c("Red"="Red","Blue"="Blue","Green"="Green"),
                  selected = "Blue",multiple = F),
      selectInput(inputId = "border1",label = "Select Border Color",choices = c("Black"="#000000","Yellow"="#FFFF00","Red"="#ff3300")),
      selectInput(inputId="channel1",label="Choose Feature",choices = c("Favorites"="Favorites","Retweets"="Retweets",
                                                                        "ANGER"="ANGER","FEAR"="FEAR",
                                                                        "SADNESS"="SADNESS","DISGUST"="DISGUST",
                                                                        "JOY"="JOY","TRUST"="TRUST",
                                                                        "SURPRISE"="SURPRISE","ANTICIPATION"="ANTICIPATION",
                                                                        "POSITIVE"="POSITIVE","NEGATIVE"="NEGATIVE"),
                  selected = "POSITIVE",multiple = F)
    ),
    # Show Word Cloud
    mainPanel(
      # plotOutput("plot")
      tabsetPanel(type = "tabs",
                  tabPanel("Word Cloud Plot", plotOutput("plot")),
                  tabPanel("Tweet", tableOutput("table")),
                  tabPanel("Plot", plotOutput(outputId = "distPlot"))
      )
    )
  )
)
server <- function(input, output) {
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  output$plot <- renderPlot({
    wordcloud_rep(names(row_sum),freq=row_sum,scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  datasetInput <- reactive({
    switch(input$dataset,
           'bitcoin'=bitcoin %>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'ANGER'=ANGER%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'FEAR'=FEAR%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'SADNESS'=SADNESS%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'DISGUST'=DISGUST%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'JOY'=JOY%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'TRUST'=TRUST%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'SURPRISE'=SURPRISE%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'ANTICIPATION'=ANTICIPATION%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'POSITIVE'=POSITIVE%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),
           'NEGATIVE'=NEGATIVE%>%dplyr::select(everything()) %>%dplyr::filter(UserID <= input$mpg),)
  })
  output$table <- renderTable({
    datasetInput()
  })
  output$distPlot <- renderPlot({
    if(input$color1=="Red"){
      sColor = "#ff3300"
    }else if(input$color1=="Blue"){
      sColor = "#3399ff"
    }else if(input$color1=="Green"){
      sColor = "#00FF00"
    }
    p2 <- bitcoin %>%ggplot()
    p2
    if(input$channel1 == "Favorites"){
      p2 <- p2 + geom_bar(aes(x=Favorites),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "Retweets"){
      p2 <- p2 + geom_bar(aes(x=Retweets),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "ANGER"){
      p2 <- p2 + geom_bar(aes(x=ANGER),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "FEAR"){
      p2 <- p2 + geom_bar(aes(x=FEAR),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "SADNESS"){
      p2 <- p2 + geom_bar(aes(x=SADNESS),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "DISGUST"){
      p2 <- p2 + geom_bar(aes(x=DISGUST),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "JOY"){
      p2 <- p2 + geom_bar(aes(x=JOY),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "TRUST"){
      p2 <- p2 + geom_bar(aes(x=TRUST),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "SURPRISE"){
      p2 <- p2 + geom_bar(aes(x=SURPRISE),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "ANTICIPATION"){
      p2 <- p2 + geom_bar(aes(x=ANTICIPATION),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "POSITIVE"){
      p2 <- p2 + geom_bar(aes(x=POSITIVE),bins = input$bins1xz,col=input$border1,fill=sColor)
    }else if(input$channel1 == "NEGATIVE"){
      p2 <- p2 + geom_bar(aes(x=NEGATIVE),bins = input$bins1xz,col=input$border1,fill=sColor)
    }
    p2 <- p2 +  theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(x=input$channel1,y='Count',title=paste(input$channel1,"Count Plot",sep = " "))
    p2
  })
}
shinyApp(ui = ui, server = server)

