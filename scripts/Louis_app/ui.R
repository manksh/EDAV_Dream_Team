fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", 
                  "Choose your n-gram range:",
                  choices = grams),
      selectInput("decade", "Choose your decade:", choices = decades),
      actionButton("update", "Change")
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)