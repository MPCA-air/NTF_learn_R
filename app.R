library(tidyverse)
library(openair)
library(shiny)
library(RColorBrewer)
library(lubridate)
library(digest)
library(htmltools)
library(leaflet)


data <- read_csv("NM_air_data.csv")

data <- filter(data, as.Date(date) < as.Date("2016-01-01"))

swap_names <- function(df, old_name, new_name) {
  
  if (!old_name %in% c(" ", "")) {
    
    col_num <- grep(trimws(tolower(old_name)), trimws(tolower(names(df))))
    
    names(df)[col_num[[1]]] <- new_name
    
  }
  
  return(df)
  
  
}

#setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/NTP_Training/NTF_learn_R/data")
  pollutants <- unique(data$Pollutant)
  years <- unique(year(data$date))
  sites <- unique(data$ReportName)
  
  col_names <- names(data)
  
shinyApp(
   ui = fluidPage(
         tags$head(includeCSS("css/styles.css")),
         titlePanel("Pollution wind roses"),
          br(),
         fluidRow(column(12, 
                  wellPanel(
           fluidRow(column(12, h3("Load data", style = "margin-top: -2px;"),
                  fileInput("master", label = NULL, placeholder = "Choose a .CSV file...", width = "320px"))), 
                  fluidRow(column(12, h4("Select column names", style = "margin-top: -2px;")), 
                         column(1, h5("Concentration", style = "margin-top: -3px;"), 
                                selectInput("conc_col", label = NULL, width = "200px",  choices = col_names), 
                                style = "margin-top: 8px; margin-bottom: 0px;"),
                         
                        column(1, h5("Wind speed", style = "margin-top: -3px;"), 
                               selectInput("wind_speed", label = NULL, width = "200px", choices = col_names), 
                                    style = "margin-top: 8px; margin-bottom: 0px;"),

                      column(1, h5("Wind direction", style = "margin-top: -3px;"), 
                             selectInput("wind_direct", label = NULL, width = "200px", choices = col_names),  
                             style = "margin-top: 8px; margin-bottom: 0px;"),
                  
                  column(1, h5("Date ", style = "margin-top: -3px;"), 
                         selectInput("date_col", label = NULL, width = "200px", choices = col_names), 
                         style = "margin-top: 8px; margin-bottom: 0px;"),
              
              column(1, h5("Sites ", style = "margin-top: -3px;"), 
                     selectInput("site_col", label = NULL, width = "200px", choices = col_names), 
                     style = "margin-top: 8px; margin-bottom: 0px;"),
         
               column(1, h5("Pollutants ", style = "margin-top: -3px;"), 
                selectInput("pol_col", label = NULL, width = "200px", choices = col_names), 
                style = "margin-top: 8px; margin-bottom: 0px;"),
              
              column(1, h5("Latitude", style = "margin-top: -3px;"), 
                     selectInput("lat_col", label = NULL, width = "200px", choices = col_names), 
                     style = "margin-top: 8px; margin-bottom: 0px;"),
              
              column(1, h5("Longitude", style = "margin-top: -3px;"), 
                     selectInput("long_col", label = NULL, width = "200px", choices = col_names), 
                     style = "margin-top: 8px; margin-bottom: 0px;")),
               style = "background-color: #eafeea")
              )),
         br(),
         
        fluidRow(column(12, column(4, wellPanel(fluidRow(column(4, h4("Select data", style = "margin-top: -2px;"))),
                              selectInput("Site",      label = "Monitoring site", choices = sites),
                              selectInput("Pollutant", label = "Pollutant",       choices = pollutants),
                              selectInput("Year",      label = "Year",            choices = years))),
                  
      column(3, h4("Air Monitoring Site"),
                            leafletOutput('normviz', height = "400px")),
      column(3, h4("Pollution wind rose"),
                            plotOutput('siteviz', height = "400px"))))
      ),
    
    
    server = function(input, output, session) {
      
      new.names <- reactive({
        
        print(is.null(input$master))
        
        if (!is.null(input$master)) {
          
          print(input$master$datapath)
          
          data <- read_csv(input$master$datapath)
        
          data <- swap_names(data, input$conc_col, "Result")
          
          data <- swap_names(data, input$wind_speed, "ws")
          
          data <- swap_names(data, input$wind_direct, "wd")
          
          data <- swap_names(data, input$date_col, "date")
          
          data <- swap_names(data, input$site_col, "ReportName")
          
          data <- swap_names(data, input$pol_col, "Pollutant")
          
        }
        
        print(nrow(data))
        
        return(data)
        
      })
      
      
      site.data <- reactive({
        
        if (!is.null(input$Site)) sub_data <- dplyr::filter(new.names(), ReportName == input$Site)
        else new.names()
        
        
      })
      
      observe({
        updateSelectInput(session, "Site", choices = unique(new.names()$ReportName))
      })
      
      observe({
        updateSelectInput(session, "Pollutant", choices = unique(site.data()$Pollutant))
      })
      
      observe({
        updateSelectInput(session, "Year", choices = unique(year(site.data()$date)))
      })
      
      
      output$normviz <- renderLeaflet({
        
        print(input$Pollutant)
        print(input$Year)
        print(input$Site)
        
        data_sub <- filter(new.names(), 
                            Pollutant  == input$Pollutant, 
                            format(as.Date(date), "%Y")  == input$Year, 
                            !is.na(Result), 
                            ReportName == input$Site)
        
        data_sub <- data_sub %>% select(ReportName, Latitude, Longitude) 
        
        leaflet(data = data_sub[1, ]) %>% 
            addProviderTiles(providers$Esri.WorldImagery)  %>% 
            addCircles(fillOpacity = 0.25, opacity = 0.7, radius = 25) 
        })
        
  output$siteviz <- renderPlot({
    
    data_sub <- filter(new.names(), 
                       Pollutant  == input$Pollutant, 
                       format(as.Date(date), "%Y")  == input$Year, 
                       !is.na(Result), 
                       ReportName == input$Site)
    
    data_sub <-  data_sub  %>% 
                  mutate(MDL = max(DetectionLimit, na.rm = T), 
                         minimum = min(Result, na.rm = T), 
                         maximum = max(Result, na.rm = T), 
                         Result = ifelse(Censored, 1e-16, Result))
    
    breaks_site = c(0, 
                    round(data_sub$minimum[1], digits = 3), 
                    round(data_sub$MDL[1], digits = 3),
                    round(data_sub$maximum[1] - data_sub$MDL[1], digits = 3),
                    round(0.5*data_sub$maximum[1], digits = 3),
                    round(0.75 * data_sub$maximum[1], digits = 3), 
                    round(data_sub$maximum[1], digits = 3)) 
    
    pollutionRose(data_sub, statistic = "abs.count", pollutant = "Result", breaks = breaks_site, key.footer = "ug/m3")
    })
    }
  )
     
