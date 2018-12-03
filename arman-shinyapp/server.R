#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(shinythemes)

seattleCrime <- data.frame(read.csv("data/crisis-data.csv", header = TRUE), stringAsFactors = FALSE)
seattleCrime$Occurred.Date...Time <- gsub("T", " ", seattleCrime$Occurred.Date...Time)
seattleCrime$Occurred.Date...Time <- as.POSIXct(strptime(seattleCrime$Occurred.Date...Time, "%Y-%m-%d %H:%M:%S"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$crimeTime <- renderPlot({
    dateTimeMin <- as.POSIXct(strptime(paste(c(str_c(input$dates[1]), input$timemin), collapse = " "), "%Y-%m-%d %H:%M:%S"))
    dateTimeMax <- as.POSIXct(strptime(paste(c(str_c(input$dates[2]), input$timemax), collapse = " "), "%Y-%m-%d %H:%M:%S"))
    
    df <- filter(seattleCrime, Occurred.Date...Time >= dateTimeMin) %>%
      filter(., Occurred.Date...Time <= dateTimeMax) %>%
      count(., Final.Call.Type)
    
    
    ggplot(data = df, aes(x = Final.Call.Type, y = n)) +
      geom_bar(stat = "identity") +
      xlab("Type(s) of Crime") +
      ylab("# of Reports") +
      ggtitle("Comparison of Crimes at Time Range") +
      theme(axis.text.x = element_text(face="bold", color="#993333", size=12, angle=90),
            axis.title = element_text(size = 15),
            title = element_text(size = 20)) +
      geom_text(aes(label=n), vjust=-1) +
      scale_fill_discrete()
    
  })
  
})
