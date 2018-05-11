library(tidyverse)

air_met <- read_csv("air_met.csv")

ozone <- filter(air_met, Parameter == 44201, site_catid == "27-017-7417")

pairs(select(ozone, temperature, windSpeed, Conc, Hour, humidity), col=rgb(0,100,0,22,maxColorValue=255), cex = 1.3, cex.labels=3, cex.axis = 2.2) 
