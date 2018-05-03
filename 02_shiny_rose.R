library(tidyverse)
library(openair)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(digest)
library(htmltools)
library(rsconnect)

data <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/NTP_Training/NTF_learn_R/data/NM_air_data.csv")

#setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/NTP_Training/NTF_learn_R/data")
  Pollutant <- unique(data$Pollutant)
  Year <- unique(year(data$date))
  Site <- unique(data$ReportName)
  
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(
                     tabPanel("  1. UPLOAD DATA ",
                              div(
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("CSS//styles.css")
                                ),
                                
                                h3("Select a .csv file", style = "margin-top:-5px;"),
                                div(fileInput("master", label = NULL, width = "220px"), 
                                    id = "file_rsk",
                                    style = "margin-top:-3px;"),
                                
                                
                                h3("Enter column names", style = "margin-top:-3px;"), "Separate column names with a comma",
                                div(textInput("new_names", label = NULL, width = "520px", 
                                              value = "Latitude, Longitude, Result, wd"), 
                                    id = "new_names2",
                                    style = "margin-top: 4px;"),
                                
                                br(),
                     column(3,
                            style = "padding-bottom: 20px;",
                            inputPanel(
                              selectInput("Pollutant", label = "Choose a pollutant", choices = Pollutant),
                              selectInput("Site", label = "Choose a site", choices = Site),
                              selectInput("Year", label = "Choose a year", choices = Year))),
                     column(4, tags$strong("Twin Cities Air Toxics Air Monitoring Sites"),
                            leafletOutput('normviz', height = "500px")),
                     column(4, 
                            plotOutput('siteviz', height = "500px")))))),
    
    
    server = function(input, output) {
      
      new.names <- reactive({
      data <- readLines(input$master$datapath)
      
      if(length(strsplit(input$new_names, ",")[[1]]) == (ncol(data) - 7)) {
        
        names(data)[8:ncol(data)] <- unlist(strsplit(gsub(" ", "", input$new_names), ","))
      
      }
      })
      
      output$normviz <- renderLeaflet({
        print(input$Pollutant)
        print(input$Year)
        print(input$Site)
        
        
        data_sub = filter(data, Pollutant == input$Pollutant, Year == input$Year, !is.na(Result), ReportName == input$Site)
        data_sub = data_sub %>% select(ReportName, Latitude, Longitude) 
        leaflet(data = data_sub) %>%
        addTiles() %>%
        addMarkers()
        })
        
  output$siteviz <- renderPlot({
    data_sub = filter(data, Pollutant == input$Pollutant, Year == input$Year, !is.na(Result), ReportName == input$Site)
    data_sub = data_sub %>% mutate(MDL = max(DetectionLimit, na.rm = T), minimum = min(Result, na.rm = T), maximum = max(Result, na.rm = T), Result = ifelse(Censored, 1e-16, Result))
    breaks_site = c(0, 
                    round(data_sub$minimum[1], digits = 3), 
                    round(data_sub$MDL[1], digits = 3),
                    round(data_sub$maximum[1] - data_sub$MDL[1], digits = 3),
                    round(0.5*data_sub$maximum[1], digits = 3),
                    round(0.75 * data_sub$maximum[1], digits = 3), 
                    round(data_sub$maximum[1], digits = 3)) 
    
    pollutionRose(data_sub, statistic = "abs.count", pollutant = "Result", breaks = breaks_site, key.footer = "ug/m3", main = paste("Daily Average Pollution Rose for", data_sub$Pollutant[1],"\n", data_sub$ReportName[1]))
    })
    }
  )
     
