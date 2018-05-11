##geom_errorbars

library(ggplot2)
library(tidyverse)
library(readxl)

data_2015 <- read_excel("AQS_FondduLac.xlsx", sheet = "2015")
pm_data <- filter(data_2015, Parameter == 88101) %>% 
  group_by(site_catid) %>% 
  summarize(conc_mean = mean(Conc, na.rm = T), StdDev = sd(Conc, na.rm = T), count = n(), se = StdDev/count^0.5)

ggplot(data = pm_data, aes(x = site_catid, y = conc_mean)) +
  geom_col(fill = "cornflowerblue") + labs(y = "ug/m3", title = "2015 PM2.5 Concentrations", subtitle = "Error bars are standard errors") +
  geom_errorbar(aes(ymin = conc_mean - se, ymax = conc_mean + se), width = 0.15, size = 1)

ggplot(data = pm_data, aes(x = site_catid, y = conc_mean)) +
  geom_col(fill = "cornflowerblue"") + labs(y = "ug/m3", title = "2015 PM2.5 Concentrations", subtitle = "Error bars are standard deviations") +
  geom_errorbar(aes(ymin = conc_mean - StdDev, ymax = conc_mean + StdDev), width = 0.15, size = 1)
