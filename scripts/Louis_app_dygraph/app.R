## app.R ##
library(shiny)
library(shinydashboard)
library(memisc) 
source('global.R')

######### Load Data ############

word_count <- read.csv("../../data/Louis/word_count_per_year.csv")

##############UI ########################
ui <- fluidPage(
  # Application title
  titlePanel("Word count over time"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("word1", 
                  "Choose a word:",
                  choices = c("boy" = "boy",
                              "girl" = "girl",
                              "like" = "like",
                              "love" = "love",
                              "man" = "man",
                              "peace" = "peace",
                              "war" = "war",
                              "woman" = "woman",
                              " " = " "), c("love")),
      selectInput("word2", "Choose another word:", choices = c("boy" = "boy",
                                                               "girl" = "girl",
                                                               "like" = "like",
                                                               "love" = "love",
                                                               "man" = "man",
                                                               "peace" = "peace",
                                                               "war" = "war",
                                                               "woman" = "woman",
                                                               " " = " "), c("like"))
    ),
    
    mainPanel(
      plotOutput("word_graph"),
      uiOutput("video")
    )
  )
)





server <- function(input, output) {
  
  
  ####################
  #Dashboard 3 graphs

  #Line Graphs
  
  output$word_graph <- renderPlot({
      
      word_1 <- input$word1
      word_2 <- input$word2
      
      if (word_1 == " " & word_2 == " "){}
      else if (word_1 == " "){
        data = filter(word_count, word_count$word == word_2)
        ggplot(data, aes(x = year, y = count)) +
          geom_line() +
          theme(legend.position="bottom") +
          ggtitle(word_2) +
          theme(plot.title = element_text(hjust = 0.5)) +
          xlab("Year") + 
          ylab("Count")
      }
      else if (word_2 == " " | word_1 == word_2){
        data = filter(word_count, word_count$word == word_1)
        ggplot(data, aes(x = year, y = count)) +
          geom_line() +
          theme(legend.position="bottom") +
          ggtitle(word_1) +
          theme(plot.title = element_text(hjust = 0.5)) +
          xlab("Year") + 
          ylab("Count")
      }
      else {
        data = filter(word_count, word_count$word == word_1 | word_count$word == word_2)
        ggplot(data, aes(x = year, y = count, col = word)) +
          geom_line() +
          theme(legend.position="bottom") +
          ggtitle(paste(word_1,word_2, sep="/")) +
          theme(plot.title = element_text(hjust = 0.5)) +
          xlab("Year") + 
          ylab("Count")
      }
  })
  
  output$video <- renderUI({
    word_1 <- input$word1
    word_2 <- input$word2
    if (word_1 == " " & word_2 == " "){
    HTML(paste0('<iframe width="500" height="300" src="https://www.youtube.com/embed/SHJ1HL4eMLQ", frameborder="0" allowfullscreen></iframe>'))
    }
    else{}
  }
  )
  #End marker for server         
}

shinyApp(ui, server)