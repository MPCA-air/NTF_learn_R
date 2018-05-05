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

pollutants <- unique(data$Pollutant)
years <- unique(year(data$date))
sites <- unique(data$ReportName)
  
col_names <- names(data)
  
shinyApp(
   ui = fluidPage(
         tags$head(includeCSS("css/styles.css")),
         titlePanel("Pollution wind roses"),
          br(),
         
        fluidRow(column(12, column(4, wellPanel(
              fluidRow(column(4, h3("Select data", style = "margin-top: -2px;"))),
                              selectInput("Site",      label = "Monitoring site", choices = sites),
                              selectInput("Pollutant", label = "Pollutant",       choices = pollutants),
                              selectInput("Year",      label = "Year",            choices = years))),
                  
      column(3, h4("Air Monitoring Site"),
                            leafletOutput('normviz', height = "400px")),
      column(3, h4("Pollution wind rose"),
                            plotOutput('siteviz', height = "400px"))))
      ),
    
    
    server = function(input, output, session) {
      
      
      observe({
        updateSelectInput(session, "Pollutant", choices = unique(filter(data, ReportName == input$Site)$Pollutant))
      })
      
      observe({
        updateSelectInput(session, "Year", choices = unique(year(filter(data, ReportName == input$Site, Pollutant == input$Pollutant)$date)))
      })
      
     output$normviz <- renderLeaflet({
        
        print(input$Pollutant)
        print(input$Year)
        print(input$Site)
        
        data_sub <- filter(data, 
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
    
    data_sub <- filter(data, 
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
     
