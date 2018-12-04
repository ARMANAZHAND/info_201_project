#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(shinythemes)
library(lubridate)

## Converts CSV to Data Frame for dplyr analysis
seattleCrime <- data.frame(read.csv("data/crisis-data.csv", header = TRUE), stringAsFactors = FALSE)

## time choices for the second visualization
timeChoices <- c("00:00:00", "00:15:00", "00:30:00", "00:45:00",
                 "01:00:00", "01:15:00", "01:30:00", "01:45:00",
                 "02:00:00", "02:15:00", "02:30:00", "02:45:00",
                 "03:00:00", "03:15:00", "03:30:00", "03:45:00",
                 "04:00:00", "04:15:00", "04:30:00", "04:45:00",
                 "05:00:00", "05:15:00", "05:30:00", "05:45:00",
                 "06:00:00", "06:15:00", "06:30:00", "06:45:00",
                 "07:00:00", "07:15:00", "07:30:00", "07:45:00",
                 "08:00:00", "08:15:00", "08:30:00", "08:45:00",
                 "09:00:00", "09:15:00", "09:30:00", "09:45:00",
                 "10:00:00", "10:15:00", "10:30:00", "10:45:00",
                 "11:00:00", "11:15:00", "11:30:00", "11:45:00",
                 "12:00:00", "12:15:00", "12:30:00", "12:45:00",
                 "13:00:00", "13:15:00", "13:30:00", "13:45:00",
                 "14:00:00", "14:15:00", "14:30:00", "14:45:00",
                 "15:00:00", "15:15:00", "15:30:00", "15:45:00",
                 "16:00:00", "16:15:00", "16:30:00", "16:45:00",
                 "17:00:00", "17:15:00", "17:30:00", "17:45:00",
                 "18:00:00", "18:15:00", "18:30:00", "18:45:00",
                 "19:00:00", "19:15:00", "19:30:00", "19:45:00",
                 "20:00:00", "20:15:00", "20:30:00", "20:45:00",
                 "21:00:00", "21:15:00", "21:30:00", "21:45:00",
                 "22:00:00", "22:15:00", "22:30:00", "22:45:00",
                 "23:00:00", "23:15:00", "23:30:00", "23:45:00",
                 "23:59:59")

members <- c("Arman Azhand", "Danfeng Yang", "Madisen Arurang", "Liam O'Keeffe")

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

# Define UI for application that draws a histogram
shinyUI(navbarPage("Seattle Crisis Statistics",
  theme = shinytheme("superhero"),
  
  ## Purpose panel for our project
  ## Briefly explains what our data is looking at, how we are looking at it
  ## and what we hope to achieve with our analysis
  tabPanel("Purpose",
    titlePanel("Purpose of Our Project"),
    
    verticalLayout(
      alignCenter(column(12, align = "justify", wellPanel(
        textOutput("dataset")
      ))),
      
      mainPanel(
          imageOutput("img1")
      ),
      
      alignCenter(column(12, align = "justify", wellPanel(
          textOutput("audience")
      ))),
      
      mainPanel(
          imageOutput("img2")
      ),
      
      alignCenter(column(12, align = "justify", wellPanel(
          textOutput("why")
      )))
    )
  ),
  
  ## First Visualization: Liam
  ## Allows user to look at hours throughout the day
  ## from 00:00:00 to 23:00:00 and see the frequency
  ## of crimes and crisis reported
  tabPanel("Crime Frequency",
    titlePanel("Crime Frequency Per Hour"),
    sidebarLayout(
     sidebarPanel(
       # slider widget that allows user to pick hour range (0-23)
       sliderInput("time1", label = h3("Select time range:"), 
                   min = 0, max = 23, value = c(0, 23)),
       textOutput("lo")
     ),
     # line graph of crime frequency per hour
     mainPanel(
       plotOutput("graph1")
     )
    )),
  
  ## Second Visualization: Arman
  ## Allows user to pick a date and time range
  ## to see which types of crimes are most prevalent.
  ## The barplot shows all types of crimes/crisis
  ## reported throughout that time period
  tabPanel("Crimes at Times",
    titlePanel("Crime Prevalency Throughout The Day"),
    
    # Sidebar with date range input and time range input 
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("dates",
                       "Date Range",
                       start = "2017-05-05",
                       end = "2017-05-05",
                       min = "2015-05-15",
                       max = "2018-11-28"),
        
        selectInput("timemin",
                    "Time of Day From (@ starting date):",
                    choices = timeChoices),
        
        selectInput("timemax",
                    "Time of Day To (@ ending date):",
                    choices = timeChoices,
                    selected = timeChoices[97]),
        textOutput("aa")
      ),
      
      # Show a barplot of the crimes in the date-time range
      mainPanel(
        plotOutput("crimeTime", width = "100%", height = "850px")
      )
    )
  ),
  
  ## Third Visualization: Danfeng
  ## Allows user to pick a date and time range
  ## to see which types of crimes are most prevalent.
  ## The barplot shows all types of crimes/crisis
  ## reported throughout that time period
  tabPanel("Officer Dispatched",
    titlePanel("Percentage of Officer Dispatched Regarding to Initial Call Type"),
    sidebarLayout(
     sidebarPanel(
       uiOutput("types")
     ),
     mainPanel(
       plotOutput("map")
     )
    )),
  
  
  ## Fourth Visualization: Madisen
  ## Allows user to pick a month and cross identify
  ## the proportion of reported crimes and crisis 
  ## during that month throughout the years kept track
  ## of in the dataset.
  tabPanel("Crime Proportion",
    titlePanel("Proportion of Crime by Year and Month in Seattle"),
    sidebarLayout(
     sidebarPanel(
       selectInput("select4", label = h3("Select month"), 
                   choices = list("January" = 1, "February" = 2, "March" = 3,
                                  "April" = 4, "May" = 5, "June" = 6,
                                  "July" = 7, "August" = 8, "September" = 9,
                                  "October" = 10, "November" = 11, "December" = 12), 
                   selected = 1),
       textOutput("ma")
     ),
     mainPanel(
       plotOutput("graph4")
     )
  )),
  
  ## About the team panel for our project
  ## Gives insight on who worked on this project.
  ## Provides a small informative paragraph and
  ## a picture of the team member
  tabPanel("About the Team",
    titlePanel("About the Team"),
    
    ## Lets you choose a team member
    sidebarLayout(
      sidebarPanel(
        selectInput("person",
                    "Choose a team member:",
                    choices = members)
      ),
      
      alignCenter(column(6, align = "center", mainPanel(
        imageOutput("memberImg"),
        textOutput("memberDesc")
      )))
    )
    
  )
  
))
