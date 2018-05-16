library(shiny)
library(lubridate)
library(digest)
library(htmltools)
library(leaflet)
library(openair)
library(readr)
library(dplyr)


#setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/NTP_Training/NTF_learn_R/data")
data <- read_csv("NM_air_data.csv")

data <- filter(data, as.Date(date) < as.Date("2016-01-01"), !SITENAME %in% c("Harding High School", "Humboldt Ave."))

names(data)[c(14,17,18)] <- c("ReportName_xx", "Pollutant_xx", "Concentration_xx")

options(digits = 10)

swap_names <- function(df, old_name, new_name) {
  
  if (!old_name %in% c(" ", "") & !new_name %in% names(df)) {
    
    
    col_num <- grep(trimws(tolower(old_name)), trimws(tolower(names(df))))
    
    names(df)[col_num[1]] <- new_name
    
  }
  
  return(df)
}


pollutants <-  unique(data$Pollutant_xx)
years      <-  unique(year(data$date))
sites      <-  unique(data$ReportName_xx)
  
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
                         
                           column(1, h5("Sites ", style = "margin-top: -3px;"), 
                                  selectInput("site_col", label = NULL, width = "200px", choices = col_names), 
                                  style = "margin-top: 8px; margin-bottom: 0px;"),
                           
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
         
               column(1, h5("Pollutants ", style = "margin-top: -3px;"), 
                selectInput("pol_col", label = NULL, width = "200px", choices = col_names), 
                style = "margin-top: 8px; margin-bottom: 0px;"),
              
              column(1, h5("Latitude", style = "margin-top: -3px;"), 
                     selectInput("lat_col", label = NULL, width = "200px", choices = col_names), 
                     style = "margin-top: 8px; margin-bottom: 0px;"),
              
              column(1, h5("Longitude", style = "margin-top: -3px;"), 
                     selectInput("long_col", label = NULL, width = "200px", choices = col_names), 
                     style = "margin-top: 8px; margin-bottom: 0px;"),
              column(1, actionButton("go", "Go!"))),
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
      
      options(shiny.maxRequestSize = 50*1024^2)
      
      
      new.data <- reactive({
        
        print(paste("Load file is set to:", !is.null(input$master)))
          
        if (!is.null(input$master)) {
          print(paste("Load file is set to:", input$master$datapath))
          
          data <- read_csv(input$master$datapath)
        }
        
        data
        
      })
      
      
      new.names <- reactive({
        
        print(input$go)
        
        #print(paste("New data is NULL:", is.null(new.data())))
        
        isolate(
       if (!is.null(new.data())) {
         
          print(paste("Concentration column name is set to:", input$conc_col))
         
          data <- new.data()
          
          data <- swap_names(data, input$conc_col, "Concentration_xx")
          
          data <- swap_names(data, input$wind_speed, "ws")
          
          data <- swap_names(data, input$wind_direct, "wd")
          
          data <- swap_names(data, input$date_col, "date")
          
          data <- swap_names(data, input$site_col, "ReportName_xx")
          
          data <- swap_names(data, input$pol_col, "Pollutant_xx")
          
          data <- swap_names(data, input$lat_col, "Latitude")
          
          data <- swap_names(data, input$long_col, "Longitude")
          
        }
        )
        
        print(paste("Data n-rows:", nrow(data)))
        
        return(data)
        
      })
      
    
      observeEvent(input$go, {
        updateSelectInput(session, "Site", choices = sort(unique(new.names()$ReportName_xx)))
      })
      
      site.data <- reactive({
        
        if (!is.null(input$Site) & "ReportName_xx" %in% names(new.names())) {
          print(max(new.names()$Concentration_xx))
          
          sub_data <- dplyr::mutate(new.names(), Concentration_xx = as.numeric(Concentration_xx))
          
          sub_data <- dplyr::filter(sub_data,
                             ReportName_xx == input$Site | input$Site == " ",
                             !is.na(Concentration_xx), 
                             Concentration_xx > 0)
          
          return(sub_data)
          
        } else return(new.names())
        
      })
      
      observe({
        updateSelectInput(session, "Pollutant", choices = sort(unique(site.data()$Pollutant_xx)))
      })
      
      
      site.pollutant.data <- reactive({
        
        if (!is.null(input$Pollutant) & "Pollutant_xx" %in% names(site.data())) {
          
          print(max(site.data()$Concentration_xx))
          
          sub_data <- dplyr::mutate(site.data(), Concentration_xx = as.numeric(Concentration_xx))
          
          print(max(sub_data$Concentration_xx))
          
          sub_data <- dplyr::filter(sub_data, 
                             Pollutant_xx == input$Pollutant | input$Pollutant == " ", 
                             !is.na(Concentration_xx),
                             Concentration_xx > 0)
          
          return(sub_data)
          
        } else return(site.data())
        
      })
      
    
     observe({
       
        if ("Date" %in% class(site.pollutant.data()$date) | "POSIXt" %in% class(site.pollutant.data()$date)) {
          updateSelectInput(session, "Year", choices = sort(unique(year(site.pollutant.data()$date))))
        } else {
          updateSelectInput(session, "Year", choices = sort(unique(site.data()$date)))
        }
      
      })
      
      
      if (T) {
      observe({
        updateSelectInput(session, "date_col", choices = c(" ", names(new.data())))
      
        updateSelectInput(session, "site_col", choices = c(" ", names(new.data())))
      
        updateSelectInput(session, "pol_col", choices = c(" ", names(new.data())))
     
        updateSelectInput(session, "conc_col", choices = c(" ", names(new.data())))
     
        updateSelectInput(session, "wind_speed", choices = c(" ", names(new.data())))
  
        updateSelectInput(session, "wind_direct", choices = c(" ", names(new.data())))
     
        updateSelectInput(session, "lat_col", choices = c(" ", names(new.data())))
     
        updateSelectInput(session, "long_col", choices = c(" ", names(new.data())))
      })
      }
        
      output$normviz <- renderLeaflet({
        
        print(input$Pollutant)
        print(input$Year)
        print(input$Site)
        
        if ("ReportName_xx" %in% names(site.data())) {
          
        data_sub <- filter(new.names(), #format(as.Date(date), "%Y") == input$Year, 
                            ReportName_xx == input$Site)
        
        data_sub <- data_sub %>% select(ReportName_xx, Latitude, Longitude) 
        
        leaflet(data = data_sub[1, ]) %>% 
            addProviderTiles(providers$Esri.WorldImagery)  %>% 
            addCircles(fillOpacity = 0.2, opacity = 0.7, radius = 35) 
        }
        
        })
        

  output$siteviz <- renderPlot({
    
    if ("ReportName_xx" %in% names(site.data()) & !is.null(input$Pollutant) & input$Pollutant %in% site.data()$Pollutant_xx) {
    
      print("Making rose...")
        
    data_sub <- filter(new.names(), 
                       ReportName_xx == input$Site,
                       Pollutant_xx  == input$Pollutant, 
                       format(as.Date(date), "%Y")  == input$Year | input$Year == " ", 
                       !is.na(ws),
                       !is.na(wd)) %>% 
                mutate(Concentration_xx = ifelse(is.na(Concentration_xx) | Concentration_xx < 1e-12, 1e-12, Concentration_xx))
    
    if (F) {
    data_sub <-  data_sub  %>% 
                  mutate(MDL = max(DetectionLimit, na.rm = T), 
                         minimum = min(Result, na.rm = T), 
                         maximum = max(Result, na.rm = T), 
                         Result = ifelse(Censored, 1e-16, Result))
    
    breaks_site = c(0, 
                    round(data_sub$minimum[1], 3), 
                    round(data_sub$MDL[1], 3),
                    round(data_sub$maximum[1] - data_sub$MDL[1], 3),
                    round(0.5*data_sub$maximum[1], 3),
                    round(0.75 * data_sub$maximum[1], 3), 
                    round(data_sub$maximum[1], 3)) 
    
    }
    
    data_sub <- select(data_sub, ws, wd, Concentration_xx)
    
    breaks_site <- c(0, quantile(data_sub$Concentration_xx, c(0.15, 0.35, 0.55, 0.75)), quantile(data_sub$Concentration_xx, 1)*1.01) %>% as.numeric() %>% signif(1)
    
    if (length(unique(breaks_site)) < 3) breaks_site <- NULL
      
    print(arrange(data_sub, desc(Concentration_xx))[1, ])
    
    pollutionRose(data_sub, statistic = "abs.count", pollutant = "Concentration_xx", key.footer = "", breaks = breaks_site)
    }
    })
    }
  )
     
