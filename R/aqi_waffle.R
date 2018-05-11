library(waffle) #devtools::install_github("hrbrmstr/waffle")

pm_sums <- group_by(pm_data, site_catid, aqi_color) %>% summarize(count = n()) %>% arrange(desc(aqi_color)) %>% ungroup() %>% mutate(aqi_color = forcats::fct_recode(aqi_color, Good = "#53BF33", Moderate = "#FFEE00", `Unhealthy` = "#DB6B1A"))

iron(
  waffle(filter(pm_sums, site_catid == "27-017-7417") %>% select(aqi_color, count), 
         rows = 7,  size = 0.5, colors = c("#65B345", "#f2d01a"),
         title  = "Fond du Lac AQI :: 2015"),
  
  waffle(filter(pm_sums, site_catid == "27-137-7549") %>% select(aqi_color, count), 
         rows = 7,  size = 0.5, colors = rev(c("#F26522", "#F2D01A", "#65B345")),
         xlab   = "1 square = 1 day", title  = "Duluth AQI :: 2015")
)
