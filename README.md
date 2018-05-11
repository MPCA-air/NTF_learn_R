---
title: "R Demo Handout | National Tribal Air Forum"
author: Dorian Kvale and Kristie Ellickson
date: May 16, 2018
output: 
  html_document:
    theme: "spacelab"
    highlight: tango
    code_folding: hide
---

```{r setup, include = FALSE, echo = FALSE, eval = T}

library(rmarkdown)
library(knitr)
knitr::opts_chunk$set(echo=FALSE)

```

## Today's presentation is online:

[https://mpca-air.github.io/NTF_learn_R/NTF_Demo.html](https://mpca-air.github.io/NTF_learn_R/NTF_Demo.html)

---

## Upcoming Training!

- 2-Day hands-on course
- Using real air monitoring data
- Includes criteria pollutants and air toxics

---

## Previous R Training Materials _R Camp!_

Find tutorials and workshops at https://MPCA-air.github.io/RCamp.

---

## Resources to Learn R

- library(swirl)
- Intro to R with Pirates!
  library(YaRrr) @ http://nathanieldphillips.com/thepiratesguidetor/
- Coursera online courses @ https://www.coursera.org/specializations/jhu-data-science
- R for Data Science by Hadley Wickham (paper book or online)
- R Bloggers (Great articles online)
- '#rstats Twitter' (follow this!)
- R cookbooks @ cookbook-r.com (a variety of simple recipes for data and charts)

---

## Instructions to Install R

https://mpca-air.github.io/NTF_learn_R/00_Install.html

---

## Shiny Tools in Today's Presentation

- Pollution roses
- Site comparisons

---

## R Packages in Today's Presentation

<div class="columns-2">


__For data reading__

* library(readxl)

* library(readr)

__For air specific analysis, processing and charts__

* library(openair)

__For data processing and cleaning up__

* library(tidyverse)

* library(dplyr)

* library(lubridate)

__To make dynamic documents and presentations__

* library(rmarkdown)

* library(knitr)


__To make dynamic tools__

* library(RShiny)

__To make charts__

* library(ggplot2)

* library(ggbeeswarm)

* library(waffle)

* library(ggpomological)

__To make maps__

* library(leaflet)

__To provide other information or just fun__

* library(weatherAlerts)

* library(cowsay)

</div>

---

## Contacts
Kristie Ellickson, kristie.ellickson@state.mn.us

Dorian Kvale, dorian.kvale@state.mn.us

---

## Notes

* We wrote a data analysis methods book to make sure we were all using the same methodologies to calculate means, compare sites, etc. It's posted online here:
https://mpca-air.github.io/air-methods/ 

