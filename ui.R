shinyUI(fluidPage(
  # Application title
  titlePanel("Wine Varietal Characteristics Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a wine:",
                  choices = wines),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 10, value = 3),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 75,  value = 30)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
))