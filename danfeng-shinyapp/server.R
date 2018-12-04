library("shiny")
library("lubridate")
library("dplyr")
library("stringr")
library("R.utils")
library("data.table")

my_server <- function(input, output, session) {
  
  ## calculate the percentage of officer dispatched with
  ## the most 15 initial call types
  readData <- reactive({
    data <- data.frame(read.csv("data/crisis-data.csv", header = TRUE), 
                               stringAsFactors = FALSE)
    
  
    dispatched <- filter(data, str_detect(data$CIT.Officer.Dispatched, "Y"))
    
    group_all <- data %>% 
      group_by(.$Initial.Call.Type) %>%
      count() 
    group_all <- group_all[order(-group_all$n), c(1,2)]
    first_15th <- head(group_all, 15)
    
    data_first_15th <- data %>%
      filter(data$Initial.Call.Type %in% first_15th$`.$Initial.Call.Type`)
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
    data <- readData()
    checkboxGroupInput("types", "Choose Types(Max 5; Min 2 types)", choices = data$Initial.Call.Type,
                       selected = data$Initial.Call.Type[13:14])
  })
  
  ## make a limit of how many initial call types the user
  ## can choose. max 5 and min 2 types
  observe({
    if(length(input$types) > 5)
    {
      updateCheckboxGroupInput(session, "types", selected = tail(input$types,5))
    }
    if(length(input$types) < 2)
    {
      updateCheckboxGroupInput(session, "types", selected = data$Initial.Call.Type[13:14])
    }
  })
  
  ## make a bar plot of the percentage of officer dispatched
  ## regarding to the initial call type.
  output$dispatched <- renderPlot({
    data <- readData()
    
    data <- filter(data, data$Initial.Call.Type %in% input$types)
    par(mar = c(4,4,4,20))
    
    barplot(
      data$Percentage,
      main = "Percentage of Officer Dispatched", xlab = "Initial Call Type", 
      ylab = "Percentage", 
      legend = data$Initial.Call.Type, 
      args.legend = list(title = "Initial Call Type", cex = 0.75,
                         x = "right", inset=c(-0.80,0),
                         bty="n", xpd = TRUE),
      col = c("blue", "red", "pink", "black", "yellow"), font.main = 4, font.lab = 4
    )

  })

}
shinyServer(my_server)
