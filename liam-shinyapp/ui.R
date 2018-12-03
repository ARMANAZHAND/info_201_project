library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Crisis Report in Seattle"),
  sidebarLayout(
    sidebarPanel(
      # slider widget that allows user to pick hour range (0-23)
      sliderInput("time", label = h3("Select time range:"), 
                  min = 0, max = 23, value = c(0, 23))
    ),
    # line graph of crime frequency per hour
    mainPanel(
      plotOutput("graph")
    )
  )
))
