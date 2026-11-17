#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sliderInput(inputId = "num", #ID number for input
              label = "Choose a number", #label above input
              value = 25, min = 1, max = 100),#values for slider
  textInput(inputId = "title", #new ID 
            label = "write a title",
            value = "histogram of random normal values"), # starting title
  plotOutput("hist"), # creates space for a plot called hist
  verbatimTextOutput("stats") #create a space for stats
  ) 

  
# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive ({
    tibble(x = rnorm(input$num)) # number of points set by the slider 
                                  # turn tibble into a reactive 
  })
  output$hist <- renderPlot({ 
    # {} allows us to put all our R code in one nice chunk
    
    ggplot(data(), aes (x = x)) + # make a histogram
      geom_histogram() +
      labs(title = input$title)
    
  }) # code
  
  output$stats <- renderPrint ({ #own function < this is a different code but within the function
    base::summary(data()) # calculate summary stats based on the numbers
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)



#inputs functions 
#outputs functions
#remember the 