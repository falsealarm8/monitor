#####Emerging Market News Watch
##install.packages('tm')
#install.packages('wordcloud')
#install.packages('tidyr')
#install.packages('splus2R')
#install.packages('rvest')
#install.packages('magrittr')
#install.packages('DT')
#install.packages('shiny')
#install.packages('ggplot2')
#install.packages('NMF')





for (i in em){
  newsurl = paste0('https://news.google.com/search?q=',
                   i,'+coronavirus')
  
  download.file(newsurl, destfile = paste0(i,'_ncv', '.html'), quiet =TRUE)
}



num_heads<-c(1:10)

news<-""    
for (i in em){
  c.headlines<-data.frame()
  for (j in num_heads){
    
    headlines<-read_html(paste0(i,'_ncv','.html')) %>%
      html_nodes(xpath = paste0('//*[@id="yDmH0d"]/c-wiz/div/div[2]/div[2]/div/main/c-wiz/div[1]/div[',j,']/div/article/h3')) %>% 
      html_text() %>%
      iconv('latin1','ASCII',sub="")
    c.headlines<-rbind(c.headlines,data.frame(headlines))
    
    
  }
  
  news<-cbind(news,data.frame(c.headlines))
  
  
}


#####
library(DT)
library(rvest)
library(magrittr)
library(shiny)

##########
#Scraping
em<- c('US','China','Italy','Spain','Germany','Iran','UK','Europe')
ems<- c('United States','Chinas','Italy','Spain','Germany','Iran','UK','Australia','Europe')
for (i in em){
  newsurl = paste0('https://www.google.com.au/search?q=',i,'+coronavirus','&source=lnms&tbm=nws&sa=X&ved=0ahUKEwiQspanw5zeAhUhiFQKHTXMC8AQ_AUIDigB&biw=1920&bih=969')
  
  download.file(newsurl, destfile = paste0(i,'_ncv', '.html'), quiet =TRUE)
}

num_heads<-c(3:12)

news<-""    
for (i in em){
  c.headlines<-data.frame()
  for (j in num_heads){
    
    headlines<-read_html(paste0(i,'_ncv','.html')) %>%
      html_nodes(xpath = paste0('//*[@id="main"]/div[',j,']/div/div[1]/a/div[1]')) %>% 
      html_text() %>%
      iconv('latin1','ASCII',sub="")
    c.headlines<-rbind(c.headlines,data.frame(headlines))
    
    
  }
  
  news<-cbind(news,data.frame(c.headlines))
  
  
}

news<-news[,-1]

colnames(news)<-em

news1 <-news[,1:9]

########

#WORDCLOUD
text<-gather(news)
#text<-t(news.t$id)
myCorpus = Corpus(VectorSource(text$value))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,
                  c(stopwords("SMART"), "the", "and", "but", "year", "economy","economic", "south", "greek", "polish","pct","gdp","north","south", "quarter",lowerCase(em), lowerCase(ems)))
myDTM = TermDocumentMatrix(myCorpus,
                           control = list(minWordLength = 1))

m = as.matrix(myDTM)

v<-sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)

ui <- fluidPage(
  h1(paste0("COVID19 News Watch","-",format(Sys.Date(),"%m/%d/%y"))),
 
  mainPanel(
    tabsetPanel(
      tabPanel("Headlines Preview", DT::dataTableOutput("mytable"))
      
      # tabPanel("wordCloud",
      #          fluidRow(column(12,
      #                          sliderInput("freq", "Minimum Frequency:", min = 1,  max = 5, value = 2),
      #                          sliderInput("max", "Maximum Number of Words:",min = 1,  max = 200,  value = 50)
      #          )),  
      #          plotOutput("plot")
      # )
      )
  )
)


server <- function(input, output) {
  output$mytable = DT::renderDataTable({
    DT::datatable(news, options = list(
      pageLength = 5, autoWidth = FALSE))
  })
  
  
  
  
  # wordcloud_rep <- repeatable(wordcloud2)
  # output$plot <- renderPlot({
  #   
  #   wordcloud_rep(d,
  #                 minSize = input$freq, gridSize=input$max,
  #                 background ='black', color='random-light')
  # })
  
}



shinyApp(ui, server)


##########################
#WORDCLOUD


# Run the app ----
shinyApp(ui = ui, server = server)                                                                                                                                                                                                                                            