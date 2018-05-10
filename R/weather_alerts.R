library(tidyverse)
library(weatherAlerts)
library(leaflet)

alerts <- getAlerts("LA") # takes about 20 seconds

severity <- alerts@data$severity

colorMap <- c(Minor    = "green",
              Moderate = "yellow",
              Severe   = "red",
              Extreme  = "magenta",
              Unknown  = "grey")

severityColors <- unname(colorMap[severity])

leaflet() %>%
  addTiles() %>%
  addPolygons(data = alerts, color = "black", fillColor = severityColors, weight = 1, popup = ~paste0("<strong>", event, "</strong>", "<br><br>", summary))
