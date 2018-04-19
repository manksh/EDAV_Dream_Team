## app.R ##
library(shiny)
library(shinydashboard)
library(memisc) 
source('global.R')

######### Load Data ############
cover <- read.csv("../../data/Louis/cover_year.csv")
##############UI ########################
ui <- fluidPage(
  # Application title
  titlePanel("Number of covers over time"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      sliderInput("binwidth",
                  "Binwidth:",
                  min = 1,  max = 15, value = 1)
    ),
    
    mainPanel(
      plotOutput("cover_hist")
    )
  )
)





server <- function(input, output) {
  
  
  ####################
  #Dashboard 3 graphs

  #Line Graphs
  
  output$cover_hist <- renderPlot({
      
      bin_width <- input$binwidth
      ggplot(cover, aes(year)) +
        geom_histogram(color = "black", fill = "lightblue", binwidth = bin_width) +
        theme(legend.position="bottom") +
        ggtitle("Cover") +
        theme(plot.title = element_text(hjust = 0.5)) +
        xlab("Year") + 
        ylab("Count")
  }
  )
  #End marker for server         
}

shinyApp(ui, server)