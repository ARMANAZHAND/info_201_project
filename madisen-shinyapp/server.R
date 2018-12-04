library(dplyr)
library(shiny)
library(lubridate)
library(ggplot2)
library(treemapify)
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Ouputs a tree map for frequency of crime by month and year
  output$graph <- renderPlot({
    data <- read.csv("./data/crisis-data.csv", stringsAsFactors = FALSE)
    
    call_data <- data %>% select(Reported.Date, Initial.Call.Type) %>% 
                 mutate(Year=year(as.Date(Reported.Date))) %>% 
                 mutate(Month=month(as.Date(Reported.Date)))
    
    grouped <- arrange(call_data, Year) %>%
               filter(Year > 1900) %>%
               filter(Month == input$select)
    
    yearWeights <- count(grouped, Year)
    crimeSum <- sum(yearWeights$n)
    yearWeights <- mutate(yearWeights, weight=n/crimeSum)
    
    # Construct a tree plot with years and weights
    ggplot(yearWeights, aes(area = n, fill = weight, label = Year)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre",
                        grow = FALSE) 
  })
})





