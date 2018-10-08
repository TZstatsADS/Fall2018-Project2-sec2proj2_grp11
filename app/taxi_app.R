#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
  leafletOutput("taxiMap"),
  
  numericInput("location_lat", "Latitude Location:", 40.74325, step = 0.00001),
  numericInput("location_long", "Longitude Location", -73.99138, step= 0.00001),
  textInput("date", "Date:", "01/01/2016"),
  selectInput("weekend", "Day of Week:",
              c("Monday" = FALSE,
                "Tuesday" = FALSE,
                "Wednesday" = FALSE,
                "Thursday" = FALSE,
                "Friday" = FALSE,
                "Saturday" = TRUE,
                "Sunday" = TRUE), "Monday"),
  textInput("time", "Time:", "12:00"),
  selectInput("ampm:", "", c("AM" = "AM", "PM" = "PM"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #data=read.csv("subsetted_data.csv")
  
  data$tpep_pickup_datetime=as.character(data$tpep_pickup_datetime)
  data$tpep_dropoff_datetime=as.character(data$tpep_dropoff_datetime)
  
  date_pickup=sapply(strsplit(x=data$tpep_pickup_datetime, " "), `[`, 1)
  date_dropoff=sapply(strsplit(x=data$tpep_dropoff_datetime, " "), `[`, 1)
  
  time_pickup=sapply(strsplit(x=data$tpep_pickup_datetime, " "), `[`, 2)
  am_pm_pickup=sapply(strsplit(x=data$tpep_pickup_datetime, " "), `[`, 3)
  
  time_dropoff=sapply(strsplit(x=data$tpep_dropoff_datetime, " "), `[`, 2)
  am_pm_dropoff=sapply(strsplit(x=data$tpep_dropoff_datetime, " "), `[`, 3)
  
  
  min_since_midnight = function(time)
  {
    numbers=sapply(strsplit(x=time, " "), `[`, 1)
    am_pm=sapply(strsplit(x=time, " "), `[`, 2)
    hour = as.numeric(sapply(strsplit(x=numbers, ":"), `[`, 1))
    minute = as.numeric(sapply(strsplit(x=numbers, ":"), `[`, 2))
    
    pm = which(am_pm == "PM" & hour != 12)
    hour[pm] = hour[pm] + 12
    
    midnight=which(am_pm == "AM" & hour == 12)
    hour[midnight] = 0
    
    return(60*hour + minute)
    
  }
  
  min_since_midnight_pickup=min_since_midnight(paste(time_pickup, am_pm_pickup))
  min_since_midnight_dropoff=min_since_midnight(paste(time_dropoff, am_pm_dropoff))
  
  count_days = function(date) #counts the number of days from January 1st, 2016, to the specified date
  {
    month=as.numeric(sapply(strsplit(x=date, "/"), `[`, 1))
    day=as.numeric(sapply(strsplit(x=date, "/"), `[` , 2))
    total=0
    i=1
    while (i < month)
    {
      if (i %in% c(1,3,5,7,8,10,12))
      {
        total = total + 31
      }
      else if (i==2)
      {
        total = total + 29
      }
      else
      {
        total = total+30
      }
      i=i+1
    }
    total=total + day
    return(total)
  }
  
  days_since_jan12016_pickup = sapply(date_pickup, count_days)
  days_since_jan12016_dropoff = sapply(date_dropoff, count_days)
  
  data=data.frame(data, min_since_midnight_pickup, min_since_midnight_dropoff, days_since_jan12016_pickup, days_since_jan12016_dropoff)
  
  #Want to find all taxi locations within .004 degrees of latitude (equivalent to within 5 north-south blocks), 
  #and .0057 degrees of longitude (equivalent to within 2 east-west blocks) for all days in the last 30 days of similar 
  #day type (weekday or weekend) within 30 minutes of the current time. 
  
  nearest_taxis=function(location_lat, location_long, date, weekend, time, ampm)
  {
    minutes_num = min_since_midnight(paste(time, ampm))
    if (weekend == TRUE)
    {
      day_type = which(data$days_since_jan12016_dropoff %% 7 %in% c(2,3))
    } else 
    {
      day_type = which(data$days_since_jan12016_dropoff %% 7 %in% c(0,1,4,5,6))
    }
    days_num = count_days(date)
    near_time  = which(abs(minutes_num - data$min_since_midnight_dropoff) <= 60) #within one hour (earlier or later) of the specified time
    previous_days = which(days_num - data$days_since_jan12016_dropoff <=60) # within 2 months of today
    near_latitude = which(abs(location_lat - data$dropoff_latitude) <= .004)
    near_longitude = which(abs(location_long - data$dropoff_longitude)<=.0057)
    
    combined = intersect(day_type, intersect(near_time, intersect(previous_days, intersect(near_latitude, near_longitude))))
    return(data[combined,])
  }
  
  #nearest_taxis(location_lat = 40.75,location_long = -74, date = "3/8/2016", weekend = TRUE, time = "5:00", ampm = "PM")
  location_lat = reactive({input$location_lat})
  location_long = reactive({input$location_long})
  date = reactive({input$date})
  weekend = reactive({input$weekend})
  time = reactive({input$time})
  ampm = reactive({input$ampm})
  taxi_data = nearest_taxis(location_lat = location_lat(), location_long = location_long(), 
                          date = date(), weekend = weekend(), time = time(), ampm = ampm())
  map = leaflet() %>% addTiles() %>% setView(lng = taxi_data$dropoff_longitude, lat = taxi_data$dropoff_latitude)
  output$taxiMap = renderLeaflet(map)
}

# Run the application 
shinyApp(ui = ui, server = server)

