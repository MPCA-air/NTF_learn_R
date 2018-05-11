library(waffle) #devtools::install_github("hrbrmstr/waffle")

pm_sums <- group_by(pm_data, site_catid, aqi_color) %>% summarize(count = n()) %>% arrange(desc(aqi_color)) %>% ungroup() %>% mutate(aqi_color = forcats::fct_recode(aqi_color, Good = "#53BF33", Moderate = "#FFEE00", `Unhealthy for some` = "#DB6B1A", `Unhealthy` = "#ff0000"))

iron(
  waffle(filter(pm_sums, site_catid == "27-017-7417") %>% select(aqi_color, count), 
         rows = 7,  size = 0.5, colors = c("#65B345", "#f2d01a"),
         title  = "Fond du Lac AQI :: 2015"),
  
  waffle(filter(pm_sums, site_catid == "27-137-7549") %>% select(aqi_color, count), 
         rows = 7,  size = 0.5, colors = rev(c("#F26522", "#F2D01A", "#65B345")),
         xlab   = "1 square = 1 day", title  = "Duluth AQI :: 2015")
)


pm_sums <- group_by(pm_data, site_catid, aqi_color) %>% summarize(count = n()) %>% arrange(desc(aqi_color)) %>% ungroup() %>% mutate(aqi_color = forcats::fct_recode(aqi_color, Good = "#53BF33", Moderate = "#FFEE00", `Unhealthy for some` = "#DB6B1A", Unhealthy = "#ff0000")) %>%  bind_rows(data_frame(site_catid=c("27-017-7417","27-137-7554"), aqi_color=aqi_levels[c(3,4)], count=c(0,0))) %>% arrange(desc(aqi_color))

iron(
  waffle(filter(pm_sums, site_catid == "27-017-7417") %>% select(aqi_color, count), rows = 7,  size = 0.5, colors = c("#65B345","#F2D01A","#F26522","#B11117"), title = "Fond du Lac AQI :: 2015") + theme(legend.position="left", legend.text=element_text(size=15), plot.title=element_text(size=21)),
  
  waffle(filter(pm_sums, site_catid == "27-137-7554") %>% select(aqi_color, count), rows = 7,  size = 0.5, colors = rev(c("#B11117","#F26522","#F2D01A","#65B345")), xlab = "1 square = 1 day", title  = "Duluth AQI :: 2015") +  theme(legend.position="left", legend.text=element_text(size=15), plot.title=element_text(size=21), axis.title=element_text(size=15)))
