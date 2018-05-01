library(tidyverse)
library(openair)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(lubridate)

sites <- read_csv("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/Air Data Analysis Supporting Information/Information Tables/MPCA Air Monitoring Sites 2015.csv")

sites <- sites %>%
  select(SITENAME, CITY, ADDRESS, UTMZONE, UTMEASTING, UTMNORTH, Latitude, Longitude, MPCA_SITE_ID, AQS_Site_ID) %>%
  filter(CITY == "Minneapolis")

data_2014 <- read_csv("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/Air Toxics/Annual Data for Public Requests/MN_airtoxics_2014.csv")

data_2015 <- read_csv("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/Air Toxics/Annual Data for Public Requests/MN_airtoxics_2015.csv")

data_2016 <- read_csv("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/Air Toxics/Annual Data for Public Requests/MN_airtoxics_2016.csv")

data_2017 <- read_csv("X:/Programs/Air_Quality_Programs/Air Monitoring Data and Risks/6 Air Data/Monitoring Data/Air Toxics/Annual Data for Public Requests/MN_airtoxics_2017.csv")

data_air <- bind_rows(data_2014, data_2015, data_2016, data_2017)

data_air$Date <- ymd(data_air$Date)

met_2014 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/Air_Modeling/MNwx Observations/Single site files/2014/MSP 2014 Processed MET -ASOS.csv")

met_2015 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/Air_Modeling/MNwx Observations/Single site files/2015/MSP 2015 Processed MET -ASOS.csv")

met_2016 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/Air_Modeling/MNwx Observations/Single site files/2016/MSP 2016 Processed MET -ASOS.csv")

met_2017 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/Air_Modeling/MNwx Observations/Single site files/2017/MSP 2017 Processed MET -ASOS.csv")

met_all <- rbind(met_2014, met_2015, met_2016, met_2017)

met_all <- met_all %>%
             select(ValidTime, WindDir, WindSpdMPH) %>%
             rename(wd = WindDir, ws = WindSpdMPH)

met_all$ValidTime <- mdy_hm(met_all$ValidTime)
met_all$date <- met_all$ValidTime
met_all <- timeAverage(met_all, avg.time = "day", statistic = "mean", vector.ws = TRUE)
met_all$ValidTime <- NULL
met_all$date <- as.Date(met_all$date)

data_all <- left_join(met_all, data_air, by = c("date" = "Date"))
data_filter <- data_all %>%
  select(date, wd, ws, AQSID, ReportName, Parameter, DetectionLimit, Pollutant, Result, Censored) %>%
  mutate(Result = ifelse(Result == "<MDL", 0, Result))

data_filter <- left_join(sites, data_filter, by = c("AQS_Site_ID" = "AQSID"))
data_filter <- data_filter %>%
  filter(!is.na(ReportName)) %>%
  mutate(ReportName = ifelse(ReportName == "Minneapolis Fire Station 20", "Fire Station 20", ReportName))

data_filter$Result <- as.numeric(data_filter$Result)
  data <- data_filter
  Pollutant <- unique(data$Pollutant)
  Year <- unique(year(data$date))
  Site <- unique(data$ReportName)
  
  shinyApp(
    ui = fluidPage(responsive = FALSE,
                   fluidRow(
                     column(3,
                            style = "padding-bottom: 20px;",
                            inputPanel(
                              selectInput("Pollutant", label = "Choose a pollutant", choices = Pollutant),
                              selectInput("Site", label = "Choose a site", choices = Site),
                              selectInput("Year", label = "Choose a year", choices = Year))),
                     column(4, tags$strong("Minneapolis Air Monitoring Site"),
                            leafletOutput('normviz', height = "500px")),
                     column(4, 
                            plotOutput('siteviz', height = "500px")))),
    
    
    server = function(input, output) {
      
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
     
