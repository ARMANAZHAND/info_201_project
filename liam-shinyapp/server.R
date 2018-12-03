library(dplyr)
library(shiny)
library(lubridate)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Ouputs line graph of frequency of crime rates per hour
  output$graph <- renderPlot({
    data <- read.csv("./data/crisis-data.csv", stringsAsFactors = FALSE)
    data$Reported.Time <- hour(as.POSIXct(data$Reported.Time, format="%H:%M:%S"))
    data <- data %>% 
            filter(data$Reported.Time >= input$time[1], data$Reported.Time <= input$time[2])
    subset <- data %>% group_by(data$Reported.Time) %>% count()
    ggplot(data=subset, aes(x=subset$`data$Reported.Time`, y = subset$n, group = 1)) + geom_line() +
      xlab("Hour of the Day") + ylab("Frequency") + ggtitle("Frequency of crime rate per hour")
  })
})