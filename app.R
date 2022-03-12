
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(lubridate)
library(dplyr)
library(plyr)
library(tidyr)
library(scales)
library(DT)
library(leaflet)

# Reading in the main file and breaking it into chunks of 4.8 MB
# Only run this code once
#my_file <- read.csv("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv")
#grps <- (split(my_file, (seq(nrow(my_file))-1) %/% 95000))
#for (i in seq_along(grps)) {
#write.csv(grps[[i]], paste0("CTA_Data", i, ".csv"))
#}
#Delete original big CTA entries csv file in your folder after you got small csv files.

temp = list.files(pattern="*.csv")
data_joined2 <- lapply(temp, read.csv)
data_joined <- rbind.fill(data_joined2)
data_joined$newDate <- as.Date(data_joined$date, "%m/%d/%Y")
data_joined$date<-NULL
data_joined$day_week <- weekdays(as.Date(data_joined$newDate))
data_joined$year <- year(as.Date(data_joined$newDate))
data_joined$month <- month(as.Date(data_joined$newDate))
data_joined$day <- day(as.Date(data_joined$newDate))
data_joined$month <- month.abb[data_joined$month]
data_joined<- data_joined[,-5]

station_fac <- as.factor(data_joined$stationname)
station_level <- levels(station_fac)
years <- c(2001:2021)
febSeq <- c(1:29)
thirtySeq <- c(1:30)
thirtyOneSeq <- c(1:31)
stationSeq <- c(1:148)
station_fac <- as.factor(data_joined$stationname)
station_level <- levels(station_fac)

cta_info <- NULL
cta_info <- read.csv("./cta/CTA_-_System_Information_-_List_of__L__Stops.csv")
cta_location<- cta_info[c("MAP_ID","Location")]
cta_location[nrow(cta_location) + 1,] = list("40200","(41.884431,-87.626149)")
cta_location[nrow(cta_location) + 1,] = list("40640","(41.882023 -87.626098)")
cta_location[nrow(cta_location) + 1,] = list("40500","(41.883139,-87.628744)")
cta_location[nrow(cta_location) + 1,] = list("41580","(41.884914,-87.711327)")
colnames(cta_location) <- c("station_id","location")
cta_location$station_id <- as.numeric(cta_location$station_id)
cta_location <-unique(cta_location)

cta_location <- filter(cta_location, cta_location$location !="(41.867368, -87.627402)")
#left join with two dataset combining latitude and longitude as location column
data_joined <- dplyr::left_join(x=data_joined,y= cta_location,by="station_id")

location_clean <- data_joined$location
#delete all left '('  in location col
location_clean <-gsub("\\(()","",location_clean)
#delete all right ')' in location_clean
location2<-gsub(")$","",location_clean)
#Get latitude
latitude <-gsub(",.*$", "", location2)
#get longitude
longitude<-gsub("^.*,", "", location2) 

data_joined$lat <- latitude
data_joined$lng <-longitude
data_joined$lat<- as.double(data_joined$lat)
data_joined$lng<- as.double(data_joined$lng)
location2<-NULL
location_clean<-NULL
latitude<-NULL
longitude<-NULL
cat_info<-NULL
cat_location<-NULL
allData<-NULL
data_joined2<-NULL



weekdaySequence<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
monthSequence <- c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sept","Oct","Nov","Dec")
# convert the date codes to more memorable date names
barorderSeq<- c("Alphabetical","MintoMax")

#default chicago location values 
initial_chicago_lat = 41.881832
initial_chicago_lng = -87.623177
initial_chicago_zoom = 12
# Create the menu items to select the different years and the different chart


#date_data < -subset(data_joined_uic, year(data_joined_uic$year) == input$Year )

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="CS 424 Project 2 Spring 2022"),
  dashboardSidebar(
    HTML("<br><br><br>"),
    HTML("<br><br><br>"),
    HTML("<br><br><br>"),
    sidebarMenu( id="menu1",
                 HTML("<br><br><br><br><br>"),
                 menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                 menuItem("About", tabName = "about", icon = icon("info"))
    ),
    
    
    # Date Input Widget
    dateInput("Date",
              label="Select Date Input",
              value = "2021-08-23",
              min= "2001-01-01",
              max = "2021-11-30"
              
              
    ),
    HTML("<br>"),
    actionButton("previous_day","Previous Day"),
    actionButton("next_day","Next Day"),
    
    textOutput("Day_week"),
    HTML("<br><br>"),
    selectInput("Barorder", "Select the order to visualize", barorderSeq, selected = "Alphabetical"),
    tags$style(type="text/css", "#Day_week { height: 50px; width: 100%; text-align:center; font-size: 30px;}"),
    HTML("<br><br><br>"),
    selectInput("Year", "Select the year to visualize", years, selected = 2021),
    selectInput("Stations","Select the station",station_level,selected ="UIC-Halsted"),
    HTML("<br>"),
    actionButton("reset_button_first_page", "Reset View"),
    # Date Input Widget
    dateInput("Date2",
              label="Select Second Date Input",
              value = "2021-08-23",
              min= "2001-01-01",
              max = "2021-11-30"
              
              
    ),
    textOutput("Day_week2"),
    tags$style(type="text/css", "#Day_week2 { height: 50px; width: 100%; text-align:center; font-size: 30px;}")
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard", 
              fluidRow(
                column(6, 
                       fluidRow(
                         box(title = "Total Entriees all Stations in this day", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 12,
                             plotOutput("totalbystations"),height = 500),
                         
                       ), # row 1   /col 1
                       fluidRow(
                         box(title = "Chicago CTA Leaflet", solidHeader = TRUE, status = "primary", width = 8,  
                             leafletOutput("leaf_cta", height = 850)
                             
                         ),
                         box(title = "Total Entriees by All Station", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 4,
                             dataTableOutput("total_stations_table"),height=900)
                         
                       ),
                       
                ), # row 5 / column 1,col 2
                HTML("<br>"),
                HTML("<br>"),
                HTML("<br>"),
                HTML("<br>"),
                HTML("<br>"),
                HTML("<br>"),
                column(6,
                       
                       fluidRow(
                         box(title = "Total Entriees by Years", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 3,
                             plotOutput("totalbyyear"),height = 600),
                         box(title = "Total Entriees by Year(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 1,
                             dataTableOutput("total_year_table"),height = 600),
                         box(title = "Total Entries by Each Day in One Year", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 6,
                             plotOutput("totalbyday"),height = 600),
                         box(title = "Entries at by Each Day in a year(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width =2 ,
                             dataTableOutput("total_day_table"),height = 650)
                       ), # row 1
                       
                       
                       fluidRow(
                         box(title = "Total Entriees by Each Month", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 4,
                             plotOutput("totalbymonth"),height = 600),
                         box(title = "Total Entries by Each Month(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 2,
                             dataTableOutput("total_month_table"),height = 600),
                         box(title = "Total Entriees by Each Weekday of Week", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 4,
                             plotOutput("totalbyday_week"),height = 600),
                         box(title = "Total Entriees by Each WeekDay(Table)", status = "primary", solidHeader = TRUE, collapsible=TRUE, width = 2,
                             dataTableOutput("total_dayweek_table"),height = 600)
                         
                       ), # row 4  /col1,col2
                       
                ) # end col 2
                
              )  # end fluidRow
      ), # end tab item 
      
      
      tabItem(tabName = "about",
              h2("About Me"),
              p("This project was made by Kai Qi for CS 424 Spring 2022"),
              p("Original data available from https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
              p("The Chicago Data Portal also has a file on CTA L stations including their latitude and longitude:
                https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme
                as a 48KB file called CTA_-_System_Information_-_List_of__L__Stops.csv"),
              p("The project was writen during at the end of Feb to March,5th,2022")
      ) # end tab item
    ) # end tab items
  )# end body
)# end page

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  # increase the default font size
  theme_set(theme_grey(base_size = 15))  
  # new icon style
  my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  
  
  chicagoOnedayReactive <-reactive({ subset(data_joined, data_joined$newDate== input$Date) })
  
  #justOneYearReactive <- reactive(if("All" %in% input$sources){ subset(data_joined_uic,year(data_joined_uic$year) == input$Year)  }
  # else{subset(data_joined_uic$input$sources, year(data_joined_uic$year)== input$Year ) } )
  justOneYearReactive <- reactive({subset(data_joined,data_joined$year==input$Year & data_joined$stationname==input$Stations) })
  justManyYearReactive <- reactive({subset(data_joined, data_joined$stationname==input$Stations )})                               
  justOneDayAllReactive <- reactive( {subset(data_joined,data_joined$newDate==input$Date)} )
  
  # Update dateInput with previous day button
  observeEvent(input$previous_day,{
    date1 <- as.Date(input$Date)
    updateDateInput(session,"Date",
                    label="Select Date Input",
                    value = date1-1,
                    min= "2001-01-01",
                    max = "2021-11-30"
    )
    
  })
  # Update date to next day when user press next day button
  observeEvent(input$next_day,{
    date2 <- as.Date(input$Date)
    updateDateInput(session,"Date",
                    label="Select Date Input",
                    value = date2+1,
                    min= "2001-01-01",
                    max = "2021-11-30"
    )
    
  })   
  
  output$Day_week <- renderText({
    paste("Selected Date is:",input$Date)
    
    paste("  Day of Week is: ", weekdays(input$Date))
  })
  
  output$Day_week2 <- renderText({
    paste("Selected Date is:",input$Date2)
    
    paste("  Day of Week is: ", weekdays(input$Date2))
  })
  
  
  output$totalbystations <- renderPlot({
    justOneDayAll <- justOneDayAllReactive()
    if(input$Barorder=="Alphabetical"){
      #justOneDayAll$stationname <- factor(justOneDayAll$stationname, levels=stationSeq)
      ggplot(justOneDayAll, aes(x=justOneDayAll$stationname, y=justOneDayAll$rides)) + geom_bar(fill="steelblue",stat = 'identity',width=0.7)+labs(title="The total entries in one day for all stations ", 
                                                                                                                                                   x="All L stations", y = "The total entries  unit: person")+theme(plot.title = element_text(hjust=0.5, face="bold"),axis.text.x=element_text(angle=45,hjust=1,vjust=1))+scale_y_continuous(labels=label_comma())
    }
    else{
      justOneDayAll$stationname<- factor(justOneDayAll$stationname, levels=justOneDayAll$stationname[order(justOneDayAll$rides)])
      ggplot(justOneDayAll, aes(x=justOneDayAll$stationname, y=justOneDayAll$rides)) + geom_bar(fill="steelblue",stat = 'identity',width=0.7)+labs(title="The total entries in one day for all stations ", 
                                                                                                                                                   x="All L stations", y = "The total entries  unit: person")+theme(plot.title = element_text(hjust=0.5, face="bold"),axis.text.x=element_text(angle=45,hjust=1,vjust=1))+scale_y_continuous(labels=label_comma())
    }
  })
  
  output$totalbyyear <- renderPlot({
    justManyYear <- justManyYearReactive()
    
    ggplot(justManyYear, aes(x=justManyYear$year, y=justManyYear$rides)) + geom_bar(fill="steelblue",stat = 'identity',width=0.7)+labs(title="The total entries from 2001 to 2021", 
                                                                                                                                       x="The year from 2001 to 2021", y = "The total entries  unit: person")+theme(plot.title = element_text(hjust=0.5, face="bold"))+scale_y_continuous(labels=label_comma())
  })
  
  output$totalbyday <- renderPlot({
    justOneYear <- justOneYearReactive()
    ggplot(justOneYear, aes(x=justOneYear$newDate, y=justOneYear$rides)) + geom_bar(fill="steelblue",stat = 'identity',width=0.7)+labs(title="The entry each day in a year ", 
                                                                                                                                       x="The day from January 01 to Dec 31 ", y = "The total entries  unit: person")+theme(plot.title = element_text(hjust=0.5, face="bold"))+scale_y_continuous(labels=label_comma())
  })
  output$totalbymonth <- renderPlot({
    justOneYear <- justOneYearReactive()
    #justOneYear$month<- factor(justOneYear$month,levels=monthSequence)
    ggplot(justOneYear,  aes(x=fct_inorder(justOneYear$month), y=justOneYear$rides)) + geom_col(fill="steelblue")+labs(title="The total entry  each month in a year", 
                                                                                                                       x="Month from January to December", y = "The total entries  unit: person")+scale_fill_hue(breaks=monthSequence)+theme(plot.title = element_text(hjust=0.5, face="bold"))+scale_y_continuous(labels=label_comma())
  })
  output$totalbyday_week <- renderPlot({
    justOneYear <- justOneYearReactive()
    justOneYear$day_week <- factor(justOneYear$day_week,levels=weekdaySequence)
    ggplot(justOneYear, aes(fill=justOneYear$day_week,x=justOneYear$day_week, y=justOneYear$rides)) + geom_col(fill="steelblue")+labs(title=" The total  entry each weekday of the week", 
                                                                                                                                      x="From Monday to Sunday", y = "The total entries  unit: person")+scale_fill_hue(breaks=weekdaySequence)+theme(plot.title = element_text(hjust=0.5, face="bold"))+scale_y_continuous(labels=label_comma())
  })
  # Total entries at one day for Chicago all cta stations
  output$total_stations_table <- renderDataTable({
    justOneDayAll <- justOneDayAllReactive()
    byallStationsOneDay<- subset(justOneDayAll,select=c("stationname","rides"))
    colnames(byallStationsOneDay)<-c("Station","Rides")
    byallStationsOneDay
    
  },rownames=FALSE,options=list(lengthMenu = list(c(5, 10), c('5', '10')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
  
  
  
  #table 1 at the first row right for total entries each year  
  output$total_year_table <- renderDataTable({
    justManyYear <- justManyYearReactive()
    byyear <- aggregate.data.frame(justManyYear$rides, by=list(justManyYear$year), FUN=sum)
    colnames(byyear) <- c("Year","rides")
    byyear
    
  },rownames=FALSE,options=list(lengthMenu = list(c(5,8), c('5', '8')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
  # table 2 at the second row right for daily entries at specific year  
  output$total_day_table <- renderDataTable({
    justOneYear <- justOneYearReactive()
    byday <- aggregate.data.frame(justOneYear$rides, by=list(justOneYear$newDate), FUN=sum)
    colnames(byday) <- c("Day","Rides")
    byday
    
  },rownames=FALSE,options=list(lengthMenu = list(c(5, 7), c('5', '7')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
  # table 3 at the third row right for monthly total entries at specific year  
  output$total_month_table <- renderDataTable({
    justOneYear <- justOneYearReactive()
    bymonth <- aggregate.data.frame(justOneYear$rides, by=list(justOneYear$month), FUN=sum)
    colnames(bymonth) <- c("Month","Rides")
    bymonth
    
  },rownames=FALSE,options=list( lengthMenu = list(c(6,12), c('6', '12')),list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))  
  
  # table 4 at the fourth row right for weekdays total entries at specific year  
  output$total_dayweek_table <- renderDataTable({
    justOneYear <- justOneYearReactive()
    byweekday <- aggregate.data.frame(justOneYear$rides, by=list(justOneYear$day_week), FUN=sum)
    colnames(byweekday) <- c("Weekday","Rides")
    byweekday
    
  },rownames=FALSE,
  
  options=list(lengthMenu = list(c(7), c('7')),list(columnDefs = list(list(targets = c(1, 3), searchable = FALSE))),
               autoWidth=TRUE, order=list(1, 'asc'))) 
  
  
  
  
  #displays chicago cta map
  observe({
    
    
    chicago_map <- leaflet(data_joined) %>%
      
      addTiles(
        group = "OSM"
      ) %>%  # Add default OpenStreetMap map tiles
      
      addProviderTiles(
        providers$CartoDB.Positron, group = "Light"
      ) %>%
      addProviderTiles(
        providers$CartoDB.DarkMatter, group = "Dark"
      ) %>%
      addCircles(data=subset(data_joined, newDate==input$Date),lng = ~lng, lat = ~lat,popup=~subset(data_joined,newDate==input$Date)$rides, color="Blue",radius = 80 ,fillOpacity = 0.3, stroke = TRUE,label =~subset(data_joined,newDate==input$Date)$rides )%>%
      addMarkers(popup = as.character(subset(data_joined,newDate==input$Date)$rides),layerId=as.character(subset(data_joined,newDate==input$Date)$station_id),lng=subset(data_joined,newDate==input$Date)$lng,lat=subset(data_joined,newDate==input$Date)$lat,icon=my_icon)%>%
      setView(
        lng = initial_chicago_lng,
        lat = initial_chicago_lat, 
        zoom = initial_chicago_zoom
      ) %>%
      
      addLayersControl(
        baseGroups = c("OSM", "Light", "Dark"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    output$leaf_cta <-renderLeaflet({ 
      chicago_map
    })
    
  })
  #reset button
  observe({
    input$reset_button_first_page
    leafletProxy("leaf_cta") %>% setView(lat = initial_chicago_lat, lng = initial_chicago_lng, zoom = initial_chicago_zoom)
    #addMarkers(popup = as.character(subset(data_joined,newDate==input$Date)$rides),layerId=as.character(subset(data_joined,newDate==input$Date)$station_id),lng=subset(data_joined,newDate==input$Date)$lng,lat=subset(data_joined,newDate==input$Date)$lat,icon=my_icon)
    
  })
  
  
  new_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
  #add observe to listen select station changed.If user change the station,remove all marker first,then add different markers
  observe({
    input$Stations
    proxy <- leafletProxy('leaf_cta')
    if(is.null(input$Stations)){
      proxy%>% clearMarkers() %>%
        
        #addMarkers(popup = as.character(subset(data_joined,newDate==input$Date)$rides),layerId=as.character(subset(data_joined,newDate==input$Date)$station_id),lng=subset(data_joined,newDate==input$Date)$lng,lat=subset(data_joined,newDate==input$Date)$lat,icon=my_icon)
        addCircles(data=subset(data_joined,newDate==input$Date),lng=~lng,lat=~lat,group="new",popup=~rides, color="Blue",label=~rides,fillOpacity = 0.3,stroke = FALSE,radius = ~sqrt(abs(rides)/10)*50,weight = 1)
      
    }
    else{
      proxy %>% clearMarkers()%>%
        #addMarkers(popup = as.character(subset(data_joined,newDate==input$Date&stationname!=input$Stations)$rides),layerId=as.character(subset(data_joined,newDate==input$Date&stationname!=input$Stations)$station_id),
        # lng=subset(data_joined,newDate==input$Date&stationname!=input$Stations)$lng,lat=subset(data_joined,newDate==input$Date&stationname!=input$Stations)$lat,icon=my_icon) %>%
        addCircles(data= subset(data_joined,newDate==input$Date & stationname!=input$stationname),lng=~lng,lat=~lat,group="new",popup=~rides, color="Blue",label=paste(~stationname,~rides),fillOpacity = 0.3,stroke = FALSE,radius = ~sqrt(abs(rides/10)*50),weight = 1)%>%
        addAwesomeMarkers(popup = as.character(subset(data_joined,newDate==input$Date&stationname==input$Stations)$rides),layerId=as.character(subset(data_joined,newDate==input$Date&stationname==input$Stations)$station_id),
                          lng=subset(data_joined,newDate==input$Date&stationname==input$Stations)$lng,lat=subset(data_joined,newDate==input$Date&stationname==input$Stations)$lat,group="new",icon=new_icon)
    }
    
    
  })
  
  twoday_data<-NULL
  day1_data<-NULL
  day2_data<-NULL
  observe({
    input$Stations
    day2_lng = initial_chicago_lng
    day2_lat = initial_chicago_lat
    day2_zoom = 11
    day1_data <- subset(data_joined,newDate==input$Date)
    day1_data[is.na(day1_data)]=0
    #print(day1_data$rides)
    day2_data <- subset(data_joined,newDate==input$Date2)
    day2_data[is.na(day2_data)]=0
    #print(day2_data$rides)
    daydiff_rides <- day2_data$rides - day1_data$rides
    twoday_data$stationname<- day1_data$stationname
    twoday_data$ride_diff<-daydiff_rides
    twoday_data$lng <-day1_data$lng
    twoday_data$lat <-day1_data$lat
    twoday_data<-data.frame(twoday_data)
    twoday_data <- mutate(twoday_data,color = case_when(
      daydiff_rides>=0 ~ "Blue",
      daydiff_rides<0 ~ "Red"
    ))
    
    twodaydiff_map<-leafletProxy('leaf_cta')
    #print(daydiff_rides)
    
    
    twodaydiff_map%>%
      clearShapes()%>%
      addCircles(data=twoday_data,lng=~lng,lat=~lat,group="new",popup=~paste("<b>Station name: </b>",stationname,"<br>","<b>Different:<b>",ride_diff),labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px", textsize = "15px", direction = "auto" ) ),color=~color,label=~paste(stationname,ride_diff),fillOpacity = 0.3,stroke = FALSE,radius = sqrt(abs(daydiff_rides)/10)*50,weight = 1)%>%
      setView(lat = initial_chicago_lat, lng = initial_chicago_lng, zoom = day2_zoom)
    if(twoday_data$ride_diff==0){
      leafletProxy("leaf_cta")%>% addCircles(data= day1_data,lng=~lng,lat=~lat,group="new",popup=~paste("<b>Station name: </b>",stationname,"<br>","<b>Daily Entries:<b>",rides),labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px", textsize = "15px", direction = "auto" ) ), color="Blue",label=~paste(stationname,rides),fillOpacity = 0.3,stroke = FALSE,radius = ~sqrt(abs(rides/5)*50),weight = 1)
    }
    
    
  })
  
  
  twoday_data<-NULL
  
  observeEvent(input$Date2,{
    day1_data <- subset(data_joined,newDate==input$Date)
    #print(day1_data$rides)
    day1_data[is.na(day1_data)]=0
    day2_data <- subset(data_joined,newDate==input$Date2)
    day2_data[is.na(day2_data)]=0
    daydiff_rides <- day2_data$rides - day1_data$rides
    twoday_data$stationname<- day1_data$stationname
    twoday_data$ride_diff<-daydiff_rides
    twoday_data$lng <-day1_data$lng
    twoday_data$lat <-day1_data$lat
    twoday_data<-data.frame(twoday_data)
    #print(day2_data$rides)
    daydiff_rides= day2_data$rides - day1_data$rides
    twoday_data$ride_diff <- daydiff_rides
    twoday_data <- mutate(twoday_data,color = case_when(
      daydiff_rides>=0 ~ "Blue",
      daydiff_rides<0 ~ "Red"
    ))
    if(twoday_data$ride_diff==0){
      output$total_stations_table <- renderDataTable({
        justOneDayAll <- justOneDayAllReactive()
        byallStationsOneDay<- subset(justOneDayAll,select=c("stationname","rides"))
        colnames(byallStationsOneDay)<-c("Station","Rides")
        byallStationsOneDay
        
      },rownames=FALSE,options=list(lengthMenu = list(c(5, 10), c('5', '10')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
      
      leafletProxy("leaf_cta")%>% addCircles(data= day1_data,lng=~lng,lat=~lat,group="new",popup=~paste("<b>Station name: </b>",stationname,"<br>","<b>Daily Entries:<b>",rides),labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px", textsize = "15px", direction = "auto" ) ), color="Blue",label=~paste(stationname,rides),fillOpacity = 0.3,stroke = FALSE,radius = ~sqrt(abs(rides/5)*50),weight = 1)
      
      
    }
    else{
      output$total_stations_table <- renderDataTable({
        bytwodaydiff<- subset(twoday_data,select=c("stationname","ride_diff"))
        colnames(bytwodaydiff)<-c("Station","Diff Rides")
        bytwodaydiff
      },rownames=FALSE,options=list(lengthMenu = list(c(5, 10), c('5', '10')), list(searching = FALSE),autoWidth=TRUE, order=list(1, 'asc')))
    }
    
    #update bar chart on top left for different entries by two days
    output$totalbystations <- renderPlot({
      oneday_data<-subset(data_joined,data_joined$newDate==input$Date)
      day1_data <- subset(data_joined,newDate==input$Date)
      #print(day1_data$rides)
      day1_data[is.na(day1_data)]=0
      day2_data <- subset(data_joined,newDate==input$Date2)
      day2_data[is.na(day2_data)]=0
      daydiff_rides <- day2_data$rides - day1_data$rides
      twoday_data$stationname<- day1_data$stationname
      twoday_data$ride_diff<-daydiff_rides
      twoday_data$lng <-day1_data$lng
      twoday_data$lat <-day1_data$lat
      twoday_data<-data.frame(twoday_data)
      #print(day2_data$rides)
      daydiff_rides= day2_data$rides - day1_data$rides
      twoday_data$ride_diff <- daydiff_rides
      twoday_data <- mutate(twoday_data,color = case_when(
        daydiff_rides>=0 ~ "Blue",
        daydiff_rides<0 ~ "Red"
      ))
      if(daydiff_rides==0){
        if(input$Barorder=="Alphabetical"){
          ggplot(oneday_data, aes(x=oneday_data$stationname, y=oneday_data$rides)) + geom_bar(fill="steelblue",stat = 'identity',width=0.7)+labs(title="The total entries in one day for all stations ", 
                                                                                                                                                 x="All L stations", y = "The total entries  unit: person")+theme(plot.title = element_text(hjust=0.5, face="bold"),axis.text.x=element_text(angle=45,hjust=1,vjust=1))+scale_y_continuous(labels=label_comma())
        }
        else{
          
          oneday_data$stationname<- factor(oneday_data$stationname, levels=oneday_data$stationname[order(oneday_data$rides)])
          ggplot(oneday_data, aes(x=oneday_data$stationname, y=oneday_data$rides)) + geom_bar(fill="steelblue",stat = 'identity',width=0.7)+labs(title="The total entries in one day for all stations ", 
                                                                                                                                                 x="All L stations", y = "The total entries  unit: person")+theme(plot.title = element_text(hjust=0.5, face="bold"),axis.text.x=element_text(angle=45,hjust=1,vjust=1))+scale_y_continuous(labels=label_comma())
          
        }
      }
      else{
        if(input$Barorder=="Alphabetical"){
          #justOneDayAll$stationname <- factor(justOneDayAll$stationname, levels=stationSeq)
          ggplot(data.frame(twoday_data), aes(x=twoday_data$stationname, y=twoday_data$ride_diff)) +geom_bar(fill=factor(twoday_data$color),stat = 'identity',width=0.7)+scale_fill_brewer(palette="Set2")+labs(title="The total different entries between two day for all stations ", 
           x="All L stations", y = "The total different entries  unit: person")+theme(plot.title = element_text(hjust=0.5, face="bold"),axis.text.x=element_text(angle=45,hjust=1,vjust=1))+scale_y_continuous(labels=label_comma())
        }
        else{
          twoday_data$stationname<- factor(twoday_data$stationname, levels=twoday_data$stationname[order(twoday_data$ride_diff)])
          ggplot(data.frame(twoday_data), aes(x=twoday_data$stationname, y=twoday_data$ride_diff)) + geom_bar(fill=factor(twoday_data$color),stat = 'identity',width=0.7)+scale_fill_brewer(palette="Set2")+labs(title="The total different entries between two day for all stations ", 
         x="All L stations", y = "The total entries  unit: person")+theme(plot.title = element_text(hjust=0.5, face="bold"),axis.text.x=element_text(angle=45,hjust=1,vjust=1))+scale_y_continuous(labels=label_comma())
        }
      } 
      
    })
    
    
  })
  
}
# end server
# Run the application 
shinyApp(ui = ui, server = server)
