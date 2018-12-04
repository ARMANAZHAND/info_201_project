library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Proportion of Crime by Year and Month in Seattle"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h3("Select month"), 
                  choices = list("January" = 1, "February" = 2, "March" = 3,
                                 "April" = 4, "May" = 5, "June" = 6,
                                 "July" = 7, "August" = 8, "September" = 9,
                                 "October" = 10, "November" = 11, "December" = 12), 
                  selected = 1)
    ),
    mainPanel(
      plotOutput("graph")
    )
  )
))
