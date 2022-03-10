# CS424_Project2_Spring22_Chicago_CTA
This project showed the statistic entries of Chicago CTA stations from 2001 to 2021 with leaflet map and barchar and table forms
<h2>Shinyapps Link: https://kqi3.shinyapps.io/chicago_cta_project2/</h2>
# Instructions
<h2>Software Reqirement</h2>
<h3>Rstudio 4.1.2</h3>
<p>This project used Rstudio IDE version 4.1.2, but you can download the latest version of Rstudio for your OS: https://www.rstudio.com/products/rstudio/download/ Install the following packages: ggplot2,lubridate,DT,shiny,shinnydashborad,scales,DT,dplyr,plyr,reshape2,tidyverse,tidyr,leaflet. You can do this by calling library(packagename), or using RStudio's UI by navigating to "Packages", searching for them, and then installing them.</p>
<h3>Data Download</h3>
<p>The original data is available from the Chicago Data Portal at:</p>
<p>https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f </p>
<p>The Chicago Data Portal also has a file on CTA L stations including their latitude and longitude:
    https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme </p>
<p>as a 48KB file called CTA_-_System_Information_-_List_of__L__Stops.csv</p>
<h3>How to Use</h3>
<p># Reading in the main file and breaking it into chunks of 4.8 MB, Only run this code once below:
#my_file <- read.csv("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv")
#grps <- (split(my_file, (seq(nrow(my_file))-1) %/% 95000))
#for (i in seq_along(grps)) {
#write.csv(grps[[i]], paste0("CTA_Data", i, ".csv"))
#}</p>
