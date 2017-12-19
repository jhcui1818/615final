library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(wordcloud)
library(reshape2)
library(dplyr)
library(ggmap)



ui <- shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("Tax Bill Related Tweets Analysis"),
  navbarPage(title = "Content",
             tabPanel("Overview",
                      h1("Senate Republican tax bill"),
                      hr(),
                      hr(),
                      h3("The recently released tax bill have raised a big controversy."),
                      splitLayout(img(src = "pic1.png",height=600, width = 600),
                                  img(src = "pic2.png",height=600, width = 600),
                                  img(src = "pic3.png",height=600, width = 600)),
                      br(),
                      h4("The recently released tax bill have raised a big controversy. 
                        The objective of my project is to analyze how public comments change over two weeks after the tax bill released. 
                        The data is pulled from twitter during the the first week (12.01-12.08) and the second week (12.10-12.18). 
                        I use text mining, plotly, sentiment analysis, word clouds and map to show the public attitude's 
                        trend over the two weeks.")
                      ), # end of overview
             tabPanel("Barplot",
                      h4("A lot of people worry the new tax bill would cause economic deficit and more debt of the United States."),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "date",
                                      label="select date:",
                                      choices = c("first_week", "last_week"))
                        ),
                        mainPanel(
                          plotOutput("barplot")
                        )
                      )),# end of barplot
             tabPanel("Sentiment Analysis",
                      h4("There are more positive words comparing to the first day. Was the public become more positive to the new tax bill?"),
                      hr(),
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("date1","select date:",
                                      choices = c("first week", "last week")),
                          actionButton(inputId="sentiment",
                                       label="Show Sentiment Flow")
                        ),
                        mainPanel(
                          plotOutput("sentiment")
                        )
                      )),# end of sentiment2
             tabPanel("First Week Trend",
                      h4("Above 0 means there are more positive words than negative words and below 0 means there are more negative words than positive words."),
                      hr(),
                      p("select a date to plot",style="color:red"),
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("date2", "Date range:",
                                         choices = c("2017-12-01", "2017-12-02", "2017-12-03", "2017-12-04", "2017-12-05", "2017-12-06", "2017-12-07"))
                        ),
                      mainPanel(
                        plotOutput("senti2")
                      )
                     )),# end of sentiment2
             tabPanel("Word Clouds",
                      h4("Negative words are blue, and postive words are red."),
                      hr(),
                      p("The more a specific word appears, the bigger and bolder the word will appear in the visualization"),
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId = "date3",
                                      label = "select date:",
                                      choices = c("first_week", "last_week"))
                        ),
                        mainPanel(
                          plotOutput("clouds")
                        )
                      )), # end of word clouds
             tabPanel("Map",
                      sidebarLayout(
                        p("There are only two tweets with locations in 20000 tweets I searched for. 
                        Thoes two are just examples of plot on map. Further analysis can be conducted if device 
                        is able to process larger data."),
                      mainPanel(
                        plotOutput("map")
                      )
                      )) # end of map
             ))
)


text.df8.map <- readRDS(file="text.df8.map.rds")
total_reduced <- readRDS(file = "total_reduced.rds")
#total1_reduced <- readRDS(file = "total1_reduced.rds")
#total2_reduced <- readRDS(file = "total2_reduced.rds")
total2_sentiment <- readRDS(file = "total2.sentiment.rds")
total1_sentiment2 <- readRDS(file = "total1.sentiment2.rds")
total1_sentiment3 <- readRDS(file="total1_sentiment3.rds")
total_reduced$date <- as.factor(total_reduced$date)

server <- shinyServer(function(input, output) {
  output$barplot <- renderPlot({
    total_plot <- filter(total_reduced, date == input$date)
    ggplot(total_plot,aes(x=reorder(words, -n), y=n, fill=n)) +
      geom_col(fill = "steelblue2") +
      theme(axis.text.x=element_text(angle=45, hjust=1, size = 9)) +
      labs(x = "Frequency", y = "Words")
    })

## Sentiment Analysis  
  output$sentiment <- renderPlot(
    {if(input$date1 == "first week"){
    total1_sentiment2 %>%
      group_by(sentiment) %>%
      top_n(10,n) %>%
      ungroup() %>%
      mutate(word = reorder(words, n)) %>%
      ggplot(.,aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  }
  else {
    
    total2_sentiment %>%
      group_by(sentiment) %>%
      top_n(10,n) %>%
      ungroup() %>%
      mutate(word = reorder(words, n)) %>%
      ggplot(.,aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  }})
  
## trend sentiment
  output$senti2 <- renderPlot({
    
    total_for_plot <- filter(total1_sentiment3, date == input$date2)
    ggplot(total_for_plot, aes(date,sentiment)) +
      geom_col(show.legend = FALSE, fill="steelblue2")
  })
  
## Word Clouds
  output$clouds <- renderPlot({
    if(input$date3 == "first_week"){
    total1_sentiment2 %>%
      acast(words ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("steelblue2", "tomato3"),
                       max.words = 100)
  }
  
  else{
    total2_sentiment %>%
      acast(words ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("steelblue2", "tomato3"),
                       max.words = 100)
  }})
## Map
  output$map <- renderPlot({
   
    tmp <- readRDS(file = "tmp.rds")
    ggmap(tmp) +
      geom_point(data=text.df8.map, aes(x=longitude, y= latitude))
  })
})

shinyApp(ui = ui, server = server)
