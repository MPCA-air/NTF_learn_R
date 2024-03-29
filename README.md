
# R for Data Analysis | NTFAQ

__Speakers:__ _Melinda Ronca-Battist, Dorian Kvale, Kristie Ellickson_

##### _May 16, 2018_


### Follow along online 

---

https://mpca-air.github.io/NTF_learn_R/NTF_Demo.html




### Install R

---

https://itep-r.netlify.com/page/install.html

<br>



### Upcoming R Training!

---

<div class="col2">

- 3-Day hands-on course
- Using real air monitoring data
- Includes criteria pollutants and air toxics
- Make charts, maps, and pollution roses

</div>

_Contact Melinda for more information._


### Previous training materials :: _R Camp!_

---

Tutorials and workshops online at `https://MPCA-air.github.io/RCamp`



### Resources for learning R

---

<div class="col2">

- RStudio: `https://www.rstudio.com/online-learning/`
- `library(swirl)` for interactive lessons in R
- Intro to R with Pirates! at `http://tryr.codeschool.com/`
- Coursera data science: `https://www.coursera.org/jhu`

- R for Data Science by Hadley - `http://r4ds.had.co.nz/`
- R Bloggers articles at `https://www.r-bloggers.com/`
- `#rstats` on Twitter

</div>


### A guide for Air data methods

---

We’ve started an online community guide for air data analysis methods. We use it as a resource to provide similar methodologies to calculate means, compare sites, and other common air data tasks. Everyone is invited and encouraged to contribute. It is shared online at `https://mpca-air.github.io/air-methods/`. 



<img src="images/your_package.png" align="right" style="margin-left: 32px; margin-right: -22px; margin-top: 2px; float: right;">


### Shiny tools in presentation

---

Wind pollution roses: `https://air-data.shinyapps.io/pollution-roses/`



### Key terms

---

`package` An add-on for R that contains new functions someone created to help you. It’s like an App for your phone.

`library` The name of the folder that stores your R packages.

`script` A text file used to record the step-by-step instructions of your data analysis.

`RStudio` A helpful user interface that organizes your data, charts, R scripts and packages into one bundle. 


<br>

### R Packages in presentation

---

__Core packages__

![](images/core_packages.png){width="64%" style="margin-bottom: 0px; max-width: 64% margin-top: 0px;"}

<br>


__Data reading__

* `library(readxl)`

__Air specific analysis and charts__

* `library(openair)`

__Data processing and cleaning__

* `library(tidyverse, lubridate)`


__Charts__

* `library(ggbeeswarm, waffle, ggpomological)`

__Maps__

* `library(leaflet, sf)`

__Interactive tools__

* `library(shiny)`

__Just for fun__

* `library(weatherAlerts, cowsay)`


__Dynamic documents with charts__

* `library(rmarkdown)`



### Cheatsheets

---

RStudio: https://github.com/rstudio/cheatsheets/raw/master/rstudio-ide.pdf   
Getting started: https://www.datacamp.com/community/data-science-cheatsheet   
Charts: https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf     
Data cleaning: https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf   



### Sample R commands

```{r, eval=F, echo=T}
# Load Excel data
read_excel("air_data.xlsx")

# Scatterplot of Ozone vs. Temperature
ggplot(data, aes(y = O3_ppm, x = Temp_F)) + geom_point()

# Filter data
filter(data, Concentration > 99, site == "Big Woods Monitor")

# Add new column to data
mutate(data, O3_ppb = O3_ppm * 1000)
```



### Contacts

---

Melinda Ronca-Battista: <Melinda.Roncabattista@gmail.com>  
Kristie Ellickson: <Kristie.Ellickson@state.mn.us>  
Dorian Kvale: <Dorian.Kvale@state.mn.us>



