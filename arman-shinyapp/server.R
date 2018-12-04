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
library(lubridate)

seattleCrime <- data.frame(read.csv("data/crisis-data.csv", header = TRUE), stringAsFactors = FALSE)
seattleCrime$Occurred.Date...Time <- gsub("T", " ", seattleCrime$Occurred.Date...Time)
seattleCrime$Occurred.Date...Time <- as.POSIXct(strptime(seattleCrime$Occurred.Date...Time, "%Y-%m-%d %H:%M:%S"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$graph1 <- renderPlot({
    data <- read.csv("./data/crisis-data.csv", stringsAsFactors = FALSE)
    data$Reported.Time <- hour(as.POSIXct(data$Reported.Time, format="%H:%M:%S"))
    data <- data %>% 
      filter(data$Reported.Time >= input$time1[1], data$Reported.Time <= input$time1[2])
    subset <- data %>% group_by(data$Reported.Time) %>% count()
    ggplot(data=subset, aes(x=subset$`data$Reported.Time`, y = subset$n, group = 1)) + geom_line() +
      xlab("Hour of the Day") + ylab("Frequency") + ggtitle("Frequency of crime rate per hour")
  })
  
  output$aa <- renderText({
    "Curated by Arman Azhand"
  })
   
  output$crimeTime <- renderPlot({
    dateTimeMin <- as.POSIXct(strptime(paste(c(str_c(input$dates[1]),
                                               input$timemin),
                                             collapse = " "),
                                       "%Y-%m-%d %H:%M:%S"))
    dateTimeMax <- as.POSIXct(strptime(paste(c(str_c(input$dates[2]),
                                               input$timemax),
                                             collapse = " "),
                                       "%Y-%m-%d %H:%M:%S"))
    
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
  
  ## make your own desc for yourselves
  output$memberDesc <- renderText({
    if(input$person == "Arman Azhand") {
      desc <- paste(c(input$person,
                      "is a Junior pursuing a degree in Data Analytics at WSU.",
                      "This marks his final quarter at UW after many failed attempts ",
                      "in getting into a major. He was born in Iran, but grew up mostly",
                      "in the UK and California. He loves playing video games,",
                      " baking, cooking, programming, and playing musical instruments.",
                      " He also has a super duper cute cat called Sydney."))
    } else if (input$person == "Liam O'Keeffe") {
      desc <- paste(c(input$person,
                      "is a Sophomore at the University of Wasington, interested ",
                      "in earning a degree in Informatics. In his free time he likes ",
                      "to play basketball, read books and hang out with friends. A fun ",
                      "fact about him is that he was born in Tokyo, Japan and visits ",
                      "frequently."))
    } else {
      desc <- ""
    }
  })
  
  output$memberImg <- renderImage({
    if(input$person == "Arman Azhand") {
      list(src = "pics/arman.jpg", width = 400, height = 400)
    } else if (input$person == "Liam O'Keeffe") {
      list(src = "pics/liam.jpg", width = 400, height = 400)
    } else {
      
    }
  }, deleteFile = FALSE)
  
  output$dataset <- renderText({
    desc <- paste(c("The dataset in use is provided by the city of Seattle's open database.",
              "It is maintained by kaggle.com, where our team was able to access it.",
              "The dataset contains", nrow(seattleCrime), "reported crisis in the city of Seattle.",
              "The data ranges from May 15th, 2015 to November 28th, 2018 and accounts for any",
              "reported crisis - being 911 calls or other alerts that law enforcement received."),
              sep = " ")
  })
  
  output$audience <- renderText({
    desc <- paste(c("Our main audience for our analysis of this dataset are law enforcement",
                    "in the Seattle area. Since our dataset focuses on reported crisis and crimes",
                    "in the Seattle area, we thought it would be appropriate to gear our analysis",
                    "towards a group that could use this data to not only aid them in their job,",
                    "but to also save more lives and limit the possibilities of certain crisis from",
                    "escalating in the future. In other words, our analysis of the dataset will prove",
                    "to be most useful to not only our audience, but the safety and well-being of",
                    "the citizens of Seattle."), 
                  sep = " ")
  })
  
  output$why <- renderText({
    desc <- paste(c("With the visualizations of this data, we hope that",
                    "trends in crisis and crimes can be made clearer for law enforcement",
                    "to be able to do their jobs more efficiently, safely, and effectively. Our",
                    "biggest wish is for there to be less risks of harm for any group in any",
                    "situation that may present itself with a faster response time."),
                  sep = " ")
  })
  
  output$img1 <- renderImage({
    list(src = "pics/police1.jpg", width = 1000, height = 390)
  }, deleteFile = FALSE)
  
  
  output$img2 <- renderImage({
    list(src = "pics/police2.jpg", width = 1000, height = 390)
  }, deleteFile = FALSE)
  
})
