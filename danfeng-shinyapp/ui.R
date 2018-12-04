library(shiny)

my_ui <- fluidPage(
   titlePanel("Percentage of Officer Dispatched Regarding to Initial Call Type"),
   sidebarLayout(
      sidebarPanel(
        uiOutput("types")
      ),
      mainPanel(
         plotOutput("map")
      )
   )
)
shinyUI(my_ui)