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
library(shinythemes)
library(lubridate)
library(treemapify)
library(stringr)
library(rsconnect)

seattleCrime <- data.frame(read.csv("data/crisis-data.csv", header = TRUE), stringAsFactors = FALSE)
seattleCrime$Occurred.Date...Time <- gsub("T", " ", seattleCrime$Occurred.Date...Time)
seattleCrime$Occurred.Date...Time <- as.POSIXct(strptime(seattleCrime$Occurred.Date...Time, "%Y-%m-%d %H:%M:%S"))



rsconnect::setAccountInfo(name='armanazhand', token='75CF958551D0400A9FACC5DACC1986A7', secret='CMONJ3Sh0nB6f9hyVfzpBeN7rd5fTdKNxWvTBwN1')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$graph1 <- renderPlot({
    data <- read.csv("./data/crisis-data.csv", stringsAsFactors = FALSE)
    data$Reported.Time <- hour(as.POSIXct(data$Reported.Time, format="%H:%M:%S"))
    data <- data %>% 
      filter(data$Reported.Time >= input$time1[1], data$Reported.Time <= input$time1[2])
    subset <- data %>% group_by(data$Reported.Time) %>% count()
    ggplot(data=subset, aes(x=subset$`data$Reported.Time`, y = subset$n, group = 1)) + geom_line() +
      xlab("Hour of the Day") + ylab("Frequency") + ggtitle("Frequency of crime rate per hour")
  })
  ## calculate the percentage of officer dispatched with
  ## the most 15 initial call types
  readData <- reactive({
    full_data <- data.frame(read.csv("data/crisis-data.csv", header = TRUE), 
                       stringAsFactors = FALSE)
    
    
    dispatched <- filter(full_data, str_detect(full_data$CIT.Officer.Dispatched, "Y"))
    
    group_all <- full_data %>% 
      group_by(.$Initial.Call.Type) %>%
      count() 
    group_all <- group_all[order(-group_all$n), c(1,2)]
    first_15th <- head(group_all, 15)
    
    data_first_15th <- full_data %>% 
      filter(full_data$Initial.Call.Type %in% first_15th$`.$Initial.Call.Type`)
    group_dispatched <- filter(data_first_15th, str_detect(data_first_15th$CIT.Officer.Dispatched, "Y"))
    group_dispatched <- group_by(group_dispatched, group_dispatched$Initial.Call.Type) %>%
      count()
    
    colnames(first_15th) <- c("Initial.Call.Type", "All")
    colnames(group_dispatched) <- c("Initial.Call.Type", "Dispatched")
    total <- merge(first_15th, group_dispatched, by = "Initial.Call.Type")
    
    total <- mutate(total, total$Dispatched / total$All * 100)
    total <- total[order(total[4]), c(1,4)]
    colnames(total) <- c("Initial.Call.Type", "Percentage")
    
    total
  })
  
  ## allow user to select the initial call types
  output$types <- renderUI({
      data_15 <- readData()
      checkboxGroupInput("types", "Choose Types(Max 5; Min 2 types)", choices = data_15$Initial.Call.Type,
      selected = data_15$Initial.Call.Type[4:5])
  })
  
  ## make a limit of how many initial call types the user
  ## can choose. max 5 and min 2 types
  observe({
      data_15 <- readData()
      if(length(input$types) > 5)
      {
          updateCheckboxGroupInput(session, "types", selected = tail(input$types,5))
      }
      if(length(input$types) < 2)
      {
          updateCheckboxGroupInput(session, "types", selected = data_15$Initial.Call.Type[4:5])
      }
  })
  
  ## make a bar plot of the percentage of officer dispatched
  ## regarding to the initial call type.
  output$dispatched <- renderPlot({
    data_15 <- readData()
    
    data_15 <- filter(data_15, data_15$Initial.Call.Type %in% input$types)
    par(mar = c(4,4,4,25))
    
    barplot(
      data_15$Percentage,
      main = "Percentage of Officer Dispatched", xlab = "Initial Call Type", 
      ylab = "Percentage", 
      legend = data_15$Initial.Call.Type, 
      args.legend = list(title = "Initial Call Type", 
                         x = "right", inset=c(-0.40,0),
                         bty="n", xpd = TRUE),
      col = c("blue", "red", "pink", "black", "yellow"), font.main = 4, font.lab = 4
    )
    
  })
  
  output$aa <- renderText({
    "Curated by Arman Azhand"
  })
  
  output$lo <- renderText({
    "Curated by Liam O'Keeffe"
  })
  
  output$ma <- renderText({
    "Curated by Madisen Arurang"
  })
  
  output$dy <- renderText({
    "Curated by Danfeng Yang"
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
  
  output$graph4 <- renderPlot({
    data <- read.csv("./data/crisis-data.csv", stringsAsFactors = FALSE)
    
    call_data <- data %>% select(Reported.Date, Initial.Call.Type) %>% 
      mutate(Year=year(as.Date(Reported.Date))) %>% 
      mutate(Month=month(as.Date(Reported.Date)))
    
    grouped <- arrange(call_data, Year) %>%
      filter(Year == input$select4) 
    
    crime_counts <- count(grouped, Month) %>%
      mutate(month_name=month.abb[Month])
    
    crime_sum <- sum(crime_counts$n)
    crime_counts <- mutate(crime_counts, proportion=-n/crimeSum)
    
    # Construct a tree plot with years and weights
    ggplot(crime_counts, aes(area = n, fill = proportion, label = month_name)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre",
                        grow = FALSE) + theme(legend.position="none")
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
    } else if (input$person == "Danfeng Yang") {
      desc <- paste(c(input$person,
                      "is a Sophomore at the University of Wasington, intended ",
                      "to major in Informatics. She likes to go hiking with friends ",
                      "every weekend, and have written many travel notes of different ",
                      "places. Most of them are published on TripAdvisor."))
    } else if (input$person == "Madisen Arurang") {
      desc <- paste(c(input$person,
                      "is a Senior at the University of Wasington, studying ",
                      "Human Centered Design & Engineering. In her free time, she enjoys ",
                      "going for runs, playing on an IM basketball team with friends, and ",
                      "going out for bubble tea or sushi."))
    } else {
      desc <- ""
    }
  })
  
  output$memberImg <- renderImage({
    if(input$person == "Arman Azhand") {
      list(src = "pics/arman.jpg", width = 400, height = 400)
    } else if (input$person == "Liam O'Keeffe") {
      list(src = "pics/liam.jpg", width = 400, height = 400)
    } else if (input$person == "Danfeng Yang") {
      list(src = "pics/danfeng.jpg", width = 500, height = 400)
    } else if (input$person == "Madisen Arurang") {
      list(src = "pics/madisen.jpg", width = 300, height = 400)
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
