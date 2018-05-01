library(sp)
library(sf)
library(readr)
library(dplyr)

setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/Data Analysis/R/useR/useR 2017")

sites <- read_csv("forecast_sites.csv")

#sites <- sites %>% 
#         rowwise() %>% 
#         mutate(geometry = list(c(monitor_lat, monitor_long)))
  
#sites <- st_sfc(sites$geometry)

#sites$geometry

coordinates(sites) <- ~monitor_long + monitor_lat 

sites <- st_as_sf(sites)

sites$geometry

# Assign a projection to original data
st_crs(sites) <- "+proj=longlat +datum=WGS84 +no_defs"

sites$geometry


# Transform to new projection
transformers <- sites %>% st_transform(2154)

transformers <- sites %>% st_transform("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

transformers$geometry

options(digits = 16)

t <- transformers 

t <- t %>% rowwise() %>% mutate(x = geometry[1],
                                y = geometry[2] )
