library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(wordcloud)
library(reshape2)
library(dplyr)
library(ggmap)



ui <- shinyUI(fluidPage(
  titlePanel("Tax Bill Related Tweets Analysis"),
  navbarPage(title = "Content",
             tabPanel("Overview",
                      h1("Senate Republican tax bill"),
                      h3("The recently released tax bill have raised a big controversy."),
                      splitLayout(img(src = "pic1.png",height=600, width = 600),
                                  img(src = "pic2.png",height=600, width = 600),
                                  img(src = "pic3.png",height=600, width = 600)),
                      h4("The recently released tax bill have raised a big controversy. 
                        The objective of my project is to analyze how public comments change over two weeks after the tax bill released. 
                        The data is pulled from twitter during the the first week (12.01-12.08) and the second week (12.10-12.18). 
                        I use text mining, plotly, sentiment analysis, word clouds and map to show the public attitude's 
                        trend over the two weeks.")
                      ), # end of overview
             tabPanel("Barplot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "date",
                                      label="select date:",
                                      choices = c("first_week", "last_week"))
                        ),
                        mainPanel = plotOutput("barplot")
                      )),# end of barplot
             tabPanel("Sentiment Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "date",
                                      label = "select date:",
                                      choices = c("first_week", "last_week"))
                        ),
                        mainPanel = plotOutput("senti")
                      )), # end of sentiment
             tabPanel("Word Clouds",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "date",
                                      label = "select date:",
                                      choices = c("first_week", "last_week"))
                        ),
                        mainPanel = plotOutput("clouds")
                      )), # end of word clouds
             tabPanel("Map",
                      mainPanel = plotOutput("map")
                      ))
             )
  
)


server <- function(input, output) {
  output$barplot <- renderPlot({if(input$date == 'first_week'){
    total1_reduced <- readRDS(file = "total1_reduced.rds")
    ggplot(total1_reduced,aes(x=reorder(words, -n), y=n, fill=n)) +
      geom_col(fill = "steelblue2") +
      theme(axis.text.x=element_text(angle=45, hjust=1, size = 9)) +
      labs(x = "Frequency", y = "Words")
  }
    if(input$date == "last_week"){  
      total2_reduced <- readRDS(file = "total2_reduced.rds")
      ggplot(total2_reduced,aes(x=reorder(words,-n), y=n)) +
        geom_col(fill = "steelblue2") +
        theme(axis.text.x=element_text(angle=45, hjust=1, size = 9)) +
        labs(x = "Frequency", y = "Words")
    }}
  )

## Sentiment Analysis  
  output$senti <- renderPlot({if(input$date == 'first_week'){
    total1_sentiment2 <- readRDS(file = "total1.sentiment2.rds")
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
  if(input$date == "last_week"){
    total2_sentiment <- readRDS(file = "total2.sentiment.rds")
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

## Word Clouds
  output$clouds <- renderPlot({if(input$date == "first_week"){
    total1_sentiment2 <- readRDS(file = "total1.sentiment2.rds")
    total1_sentiment2 %>%
      acast(words ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("steelblue2", "tomato3"),
                       max.words = 100)
  }
  
  if(input$date == "last_week"){
    total2_sentiment <- readRDS(file = "total2.sentiment.rds")
    total2_sentiment %>%
      acast(words ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("steelblue2", "tomato3"),
                       max.words = 100)
  }})
## Map
  output$map <- renderPlot({
    text.df8.map <- readRDS(file="text.df8.map.rds")
    text.df8.map$longitude <- as.numeric(text.df8.map$longitude)
    text.df8.map$latitude <- as.numeric(text.df8.map$latitude)
    tmp <- get_map(location = text.df8.map, zoom = 5)
    ggmap(tmp) +
      geom_point(data=text.df8.map, aes(x=longitude, y= latitude))
  })
}

shinyApp(ui = ui, server = server)
