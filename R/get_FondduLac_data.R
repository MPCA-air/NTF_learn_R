```{r get-Fond-data, eval=F}

#sites <- c("27-017-7417", "27-017-7416")
setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/NTP_Training/NTF_learn_R")

years <- 2014:2016

#fond  <- readRDS("data/PM_and_Ozone/Hourly_PM25_Ozone.Rdata")
#fond  <- filter(fond, site_catid %in% sites, year %in% years)

if (F) {
  fond  <- select(fond, 
                  "site_catid", "air_monitor", "start_date", "year", "month", 
                  "max_avg8hr", "pm25_avg", "weekday",
                  "monitor_lat", "monitor_long", 
                  "sfc_weather", "upper_air", 
                  "temp_f_max", "temp_f_avg",
                  "winddir_mode", "windspd_mph_med", "wind_calm",
                  "precip_in_sum", "rh_avg", "dewpoint_f_avg", 
                  "cloud_cover_avg", "overcast_height_min")
  
  fond <- rename(fond, 
                 ozone_8hr_ppb = max_avg8hr, 
                 sfc_wx_station = sfc_weather, 
                 upper_air_station = upper_air)
}


# Hourly data
fond <- readRDS("data/Hourly/AQS for FondduLac and Duluth.Rdata") %>% filter(StateCode == 27)

fond$CountyCode <- ifelse(fond$CountyCode == 17, "017", "137")

fond$SiteNum <- ifelse(fond$SiteNum == 34, 9000, fond$SiteNum)

fond$SiteNum <- ifelse(fond$SiteNum == 7416, 7417, fond$SiteNum)

fond$site_catid <- paste(fond$StateCode, fond$CountyCode, fond$SiteNum, sep = "-")

unique(fond$site_catid)

# Drop voyaguers
fond <- filter(fond, site_catid != "27-137-9000")

# Add year column
fond$Year <-  format(as.Date(fond$Date), "%Y")


#  Load detailed MET data
met <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/Staff Folders/Dorian/AQI/MET data/DarkSky database/AQI MET archive.rdata")

# Drop other sites and early years
range(met$time)

# Filter to NE sites
sort(unique(met$site_catid))

met <- filter(met, 
              site_catid %in% unique(fond$site_catid), 
              time > "2012-12-31 00:00:00 CST", 
              time < "2017-01-01 00:00:00 CST")

write_csv(met, "data/PM_and_Ozone/MET_data_2013-2017.csv")


for (i in unique(year(fond$Date))) {
  
  print(i)
  
  temp_aqs <- filter(fond, Year == i)
  
  write_csv(temp_aqs, paste0("data/Hourly/", i, "_Hourly_FondduLac.csv"))
  
}

```
