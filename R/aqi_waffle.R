#devtools::install_github("hrbrmstr/waffle")
library(waffle)

iron(
  
waffle(data_2014, rows = 7,  size = 0.5, 
       colors = aqi_colors,
       xlab   = "1 square = 1 day", title  = "AQI days for Fond du Lac\n2014",
       flip   = TRUE)

waffle(data_2015, rows = 7,  size = 0.5, 
       colors = aqi_colors,
       xlab   = "1 square = 1 day", title  = "AQI days for Fond du Lac\n2015",
       flip   = TRUE)

)