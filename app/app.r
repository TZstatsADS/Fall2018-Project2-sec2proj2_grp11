packages.used=c("shiny","leaflet","readr","DT",
                "lubridate","ggmap","googleway",
                "measurements","geosphere",
                "shinyjs", "dplyr")

# check packages that need to be installed.
packages.needed=setdiff(packages.used,
                        intersect(installed.packages()[,1],
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

#load libraries
library(shiny)
library(leaflet)
library(readr)
library(DT)
library(lubridate)
library(ggmap)
library(googleway)
library(measurements)
library(geosphere)
library(shinyjs)
library(dplyr)

# ==== fonction allowing geolocalisation
jsCode <- '
shinyjs.geoloc = function() {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);
    function onError (err) {
        Shiny.onInputChange("geolocation", false);
    }
    function onSuccess (position) {
        setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
        }, 5)
    }
};
'


#set key for google api
set_key("AIzaSyCFZGyEThOulZbOxXQhgfdJFURBEnnZvYM")
#read the processed data
per = read_csv("../data/per20160104.csv")
#set current longitude and latitude
cu_loc = c(40.8077, -73.9597)

#user interface
ui <- fluidPage(
  tabPanel(title = "Take a Yellow Taxi",
           div(class = "outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               # Use geolocalization
               # Tell shiny we will use some Javascript
               useShinyjs(),
               extendShinyjs(text = jsCode),
               # One button and one map3
               br(),
               
               #set the whole-page map outline
               leafletOutput(outputId = "map", width="100%", height="100%"),
           
               #a draggable panel put on the left side
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = FALSE, top = 60, left = "auto", right = 0, bottom = "auto",
                           width = 330, height = "auto",
                           
                           #title on the absolutepanel
                           h5(
                             "Current Time",
                             textOutput(outputId = "currentTime")
                             ),
                           
                           # actionButton("geoloc", "Localize me", class="btn btn-primary", onClick="shinyjs.geoloc()"),
                           
                           #input origin
                           textInput(inputId = "start",label = "From: ", value = ""),
                           #input destination
                           textInput(inputId = "end",label = "To: ", value = ""),
                           
                           em("Default Location:"),
                           br(),
                           em("IAB, Columbia University"),

                           br(),
                           br(),
                           
                           #input past time period to show drop-off locations
                           numericInput(inputId = "nextmin", label = "Show drop-off locations in the next minute(s)", value = 2, min = 1, max = 5),
                           #input radius of the shpere to show drop-off locations
                           sliderInput(inputId = "radius", label = "Show drop-off locations with radius", min = 0.1, max = 0.5, value = 0.2, step = 0.1, round = -1),
                           #input of unit
                           selectInput(inputId = "units", label = "Choose a Unit", choices = c("km","mi"), selected = "mi"),
                           #indicate whether or not to show past routes of the taxis
                           checkboxInput(inputId = "showroutes", label = "Show past routes", value = FALSE),
                           #indicate whether or not to show the walking routes to the drop-off locations
                           checkboxInput(inputId = "showwalk", label = "Show walking routes", value = FALSE),
                           #indicate whether or not to compare with the the public transit duration
                           # checkboxInput(inputId = "transit", label = "Compare with public transit duration", value = TRUE),
                           
                           # submitButton(text = "Search"),
                           # actionButton("search", "Search"),
                           
                           #output fare estimation
                           textOutput(outputId = "fares"),
                           #output google estimated distance
                           textOutput(outputId = "ggdis"),
                           #output google estimated duration
                           textOutput(outputId = "ggD"),
                           #output google estimated duration for public transit
                           textOutput(outputId = "ggDt")
                           ),
               
               absolutePanel(id = "user_interactive", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE,top = "auto", left = 0, right = "auto", bottom = 0,
                             width = 320, height = "auto",
                             plotOutput('plotDrivingSpeed', height=200)
               )
           )
  )
)




#server interface
server<-function(input, output, session){
  
  #reactive google direction dataframe(list)
  df = reactive({
    
    #default location: current location
    
    if(nchar(input$start)>0){
      ori = paste(input$start,", New York", sep = "")
    } else{
      ori = cu_loc
    }
    
    if(nchar(input$end)>0){
      en = paste(input$end,", New York", sep = "")
    } else{
      en = cu_loc
    }
    
    google_directions(origin = ori,
                      destination = en,
                      mode = "driving")
    
    })
  
  #reactive google distance dataframe(list)
  dis = reactive({
    
    #default location: current location
    
    if(nchar(input$start)>0){
      ori = paste(input$start,", New York", sep = "")
    } else{
      ori = cu_loc
    }
    
    if(nchar(input$end)>0){
      en = paste(input$end,", New York", sep = "")
    } else{
      en = cu_loc
    }
    
    google_distance(origin = ori,
                    destination = en,
                    mode = "driving",
                    units = "imperial")
                 })
  
  #reactive google distance for public transit dataframe(list)
  dis_transit = reactive({
    
    #default location: current location
    if(nchar(input$start)>0){
      ori = paste(input$start,", New York", sep = "")
    } else{
      ori = cu_loc
    }
    
    if(nchar(input$end)>0){
      en = paste(input$end,", New York", sep = "")
    } else{
      en= cu_loc
    }
    
    google_distance(origin = ori,
                    destination = en,
                    mode = "transit",
                    units = "imperial")

      
  })
  
  #reactive past time period of drop-offs
  next_min = reactive({
    input$nextmin
  })
  
  #reactive unit
  uni = reactive({
    input$units
  })
  
  #reactive radius
  rad = reactive({
    input$radius
  })
  
  #reactive show-route idicator for past drives
  showroutes_check = reactive({
    input$showroutes
  })
  
  #reactive indicator for showing walking routes
  showwalk_check = reactive({
    input$showwalk
  })
  
  #show system time
  output$currentTime <- renderText({
    # invalidateLater causes this output to automatically
    # become invalidated when input$interval milliseconds
    # have elapsed
    invalidateLater(1000, session)
    
    format(Sys.time(),"%H:%M:%S")
  })
  
  
  #output fare esmitation
  output$fares = renderText({
    
    #obtain distance number
    mile = as.data.frame(dis()$rows[[1]])[1,1][1,1]
    mi = as.numeric(substr(mile, start = 1, stop = nchar(mile)-3))
    
    #if google unit is ft, convert to mi
    if(substr(mile, start = nchar(mile)-1, stop = nchar(mile))=="ft"){
      mi = conv_unit(mi, "ft", "mi")
    }
    
    #convert current time to the time available in the dataset(Jan - June, 2016)
    #convert July to June, August to May...etc.
    t = Sys.time()
    t = as.POSIXct(format(t),tz="UTC")
    year(t) = 2016
    month(t) = 1
    day(t) = 4
    
    #since we have only 1 day's data, the previous 5 min data is not completely available for time earlier than 00:05:00
    if (hour(t)==0 & minute(t) <5){
      prev_5min = na.omit(per%>%subset(tpep_dropoff_datetime>=(t-5*60)+24*60*60 | tpep_dropoff_datetime<=t))
    } else{
      #get previous 5 minutes data for fare estimation
      prev_5min = na.omit(per%>%subset(tpep_dropoff_datetime>=(t-5*60) & tpep_dropoff_datetime<=t))
    }
    
    
    #remove possible disturbing value
    prev_5min = prev_5min[(prev_5min$mph != Inf 
                           & prev_5min$dollar_per_mile != Inf
                           & prev_5min$mph != (-Inf)
                           & prev_5min$dollar_per_mile != (-Inf))
                           & prev_5min$dollar_per_mile > 0 ,]
    #calculate average dolloar per mile
    avg_dpm = mean(prev_5min$dollar_per_mile, na.rm = TRUE)
    #calculate total dollar amount
    do = mi*avg_dpm
    #print fare estimation
    if (is.na(do) | is.null(do)){
      "Fare amount: "
    } else
    paste("Fare amount: $",ifelse(is.na(do),0,round(do,2)), sep = "")
  })
  
  #output google distance estimation
  output$ggdis = renderText({
    #obtain distance number
    mile = as.data.frame(dis()$rows[[1]])[1,1][1,1]
    mi = as.numeric(substr(mile, start = 1, stop = nchar(mile)-3))
    
    #different number output for different units (mi, km), also considering ft & m
    if(uni()=="mi"){
      paste("Distance: ", mile, sep = "")
    } else if(substr(mile, start = nchar(mile)-1, stop = nchar(mile))=="ft"){
      m = conv_unit(mi, from = "ft", to = "m")
      paste("Distance: ", round(m), " m", sep = "")
    } else if (mi < 0.621371){#less than 1 km but still in unit "mile"
      m = conv_unit(mi, from = "mi", to = "m")
      paste("Distance: ", round(m), " m", sep = "")
    }
    else{
      km = conv_unit(mi, from = "mi", to = "km")
      paste("Distance: ", round(km,1), " km", sep = "")
    }
  })
  
  #output google estimated duration
  output$ggD = renderText({
    
      durat = as.data.frame(dis()$rows[[1]])[1,2][1,1]
      paste("Duration: ", durat, sep = "")

  })
  
  #output google estimated duration for public transit
  output$ggDt = renderText({
      durat_transit = as.data.frame(dis_transit()$rows[[1]])[1,2][1,1]
      paste("Duration for public transit: ", durat_transit, sep = "")
  })
  
  
  #output map
  output$map = renderLeaflet({
    
    #progress bar
    # withProgress(message = 'Plotting the map in progress',
    #              value = 0, {
    #                for (i in 1:15) {
    #                  incProgress(1/15)
    #                  Sys.sleep(0.1)
    #                }
    #              })
    
    #convert current time to the time available in the dataset(Jan - June, 2016)
    #convert July to June, August to May...etc.
    t = Sys.time()
    t = as.POSIXct(format(t),tz="UTC")
    year(t) = 2016
    month(t) = 1
    day(t) = 4
    
    #since we have only 1 day's data, the next x min data is not completely available for time later than 23:(60-x):00
    if (hour(t)== 23 & minute(t) + next_min() > 60 ){
      next_xmin = na.omit(per%>%subset(tpep_dropoff_datetime<=(t+60*next_min())-24*60*60 | tpep_dropoff_datetime>=t))
    } else{
      #get previous x minutes data for fare estimation
      next_xmin = na.omit(per%>%subset(tpep_dropoff_datetime<=(t+60*next_min()) & tpep_dropoff_datetime>=t))
    }
    
    #remove possible disturbing value for map plotting
    next_xmin = next_xmin[next_xmin$dropoff_longitude != 0 & next_xmin$dropoff_latitude != 0,]
    #get drop-off longitude and latitude
    lng_5 = next_xmin$dropoff_longitude
    lat_5 = next_xmin$dropoff_latitude

    #make an icon for start location
    startIcon <- makeIcon(
      iconUrl = "../doc/figs/navigation.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 15
    )
    
    #make an icon for end location
    endIcon <- makeIcon(
      iconUrl = "../doc/figs/end.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 30
    )
    
    #make an icon for end location of past drives (icon color : black)
    yellowendIcon <- makeIcon(
      iconUrl = "../doc/figs/yellowend.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 30
    )
    
    #decode google directions' polylines
    polyl = direction_polyline(df())
    pl_decode = decode_pl(polyl)
    
    #get start location longitude and latitude
    startpoint = c(pl_decode$lon[1], pl_decode$lat[1])
    
    #draw a map
    MAP=leaflet(pl_decode) %>%
          addTiles()%>%
          setView(lng = startpoint[1], lat = startpoint[2], zoom = 14)
    
    
    
    #get within circle data for drop-offs
    r = conv_unit(rad(), uni(),"m")
    within_circle = next_xmin[as.vector(distm(startpoint, cbind(lng_5, lat_5)))<r,]
    #within circle drop off time
    wc_time = format(within_circle$tpep_dropoff_datetime,"%H:%M:%S")
    time_diff_in_sec = within_circle$tpep_dropoff_datetime - t
    time_diff_in_min = as.numeric(time_diff_in_sec)%/%60
    plural = ifelse(time_diff_in_min>1, "mins", "min")
    
    #check if there is drop-off data within circle to avoid warning message
    if(nrow(within_circle)>0){
      if(!is.null(input$start) & !is.null(input$end)){
        
       #for each drop-off within circle
       for (i in 1:nrow(within_circle)){
      
         #if the app user wants to see the past routes of the drop offs, show them with black polylines
         if (showroutes_check() == TRUE){
           df2 = google_directions(origin = c(within_circle$pickup_latitude[i], within_circle$pickup_longitude[i]),
                      destination = c(within_circle$dropoff_latitude[i], within_circle$dropoff_longitude[i]),
                      mode = "driving")
           polyl2 = direction_polyline(df2)
           pl_decode2 = decode_pl(polyl2)
           MAP = MAP%>%
             addPolylines(lat = pl_decode2[,1], lng = pl_decode2[,2], color = "lightsteelblue3", opacity = 0.5)
         }
         
         #set default location: current location
         
         if(nchar(input$start)>0){
           ori = paste(input$start,", New York", sep = "")
         } else{
           ori = cu_loc
         }
        
         #if the app user wants to see the walking routes to the drop offs, show them with green polylines
         if (showwalk_check() == TRUE){
           df3 = google_directions(origin = ori,
                                   destination = c(within_circle$dropoff_latitude[i], within_circle$dropoff_longitude[i]),
                                   mode = "walking")
          
           polyl3 = direction_polyline(df3)
           pl_decode3 = decode_pl(polyl3)
           MAP = MAP%>%
             addPolylines(lat = pl_decode3[,1], lng = pl_decode3[,2], color = "forestgreen", opacity = 0.5)
         }
        
         #calculate walking time and distance from google
         walk_dist = google_distance(origin = ori,
                                  destination = c(within_circle$dropoff_latitude[i], within_circle$dropoff_longitude[i]),
                                  mode = "walking",
                                  units = "imperial")
         walk_time = as.data.frame(walk_dist$rows[[1]])[1,2][1,1]
         walk_dis = as.data.frame(walk_dist$rows[[1]])[1,1][1,1]
         
         
         mi = as.numeric(substr(walk_dis, start = 1, stop = nchar(walk_dis)-3))
         #different number output for different units (mi, km), also considering ft & m
         if(uni()=="mi"){
           w_d = paste("Distance: ", walk_dis, sep = "")
         } else if(substr(walk_dis, start = nchar(walk_dis)-1, stop = nchar(walk_dis))=="ft"){
           m = conv_unit(mi, from = "ft", to = "m")
           w_d = paste("Distance: ", round(m), " m", sep = "")
         } else if (mi < 0.621371){#less than 1 km but still in unit "mile"
           m = conv_unit(mi, from = "mi", to = "m")
           w_d = paste("Distance: ", round(m), " m", sep = "")
         }
         else{
           km = conv_unit(mi, from = "mi", to = "km")
           w_d = paste("Distance: ", round(km,1), " km", sep = "")
         }
      
         #add within circle drop-off icons
         MAP = MAP%>%
         addMarkers(lat = within_circle$dropoff_latitude[i], 
                 lng = within_circle$dropoff_longitude[i], 
                 icon = yellowendIcon,
                 popup = paste(paste("drop-off time: ",wc_time[i]),
                               paste("(",time_diff_in_min[i]," ", plural[i], " from now)"),
                               paste("walking time: ", walk_time),
                               paste("walking distance: ", w_d),
                               sep = "<br/>"))
        }
      }
    }
    
    if(nchar(input$start)>0){
        start_loc = input$start
      } else{
        start_loc = "current location"
      }
      
    
    if(nchar(input$end)>0){
      end_loc = input$start
    } else{
      end_loc = "current location"
    }
    
      #draw THE origin-destination route with start & end icons and a big circle
      MAP = MAP %>%
        addPolylines(lng = ~lon, lat = ~lat)%>%
        addMarkers(lng = pl_decode$lon[1], 
                   lat = pl_decode$lat[1], 
                   popup = start_loc,icon = startIcon)%>%
        addMarkers(lng = pl_decode$lon[nrow(pl_decode)], 
                   lat = pl_decode$lat[nrow(pl_decode)], 
                   popup = end_loc,icon = endIcon)%>%
        addCircles(lng = pl_decode$lon[1], 
                   lat = pl_decode$lat[1],
                   radius = r,
                   opacity = 0.1,
                   stroke = FALSE)
      #show the map
      MAP
  })
  
  observeEvent(input$geoloc, {
    js$geoloc()
  })
  
  observe({
    ## localize myself
    
    if(!is.null(input$lat)){
      map = leafletProxy("map")
      dist = 0.0008
      lat = input$lat
      lng = input$long
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    }

  })
  
  output$plotDrivingSpeed = renderPlot({
    
    t1 = Sys.time()
    t1 = as.POSIXct(format(t1),tz="UTC")
    year(t1) = 2016
    month(t1) = 1
    day(t1) = 4
    
    if(hour(t1)==0 & minute(t1) < 7){
      hour(t1)=23
      minute(t1)=59
    }
    
    newdata <- per
    
    currentDate_time = t1
    data <- newdata[(newdata$trip_duration_inMins < 180) & (newdata$speed_milesPerMin < 5), ]
    
    # drawDrivingSpeed <- function(data, currentDate_time){
      one_data = data[(data$tpep_pickup_datetime >=  format(currentDate_time, '%y-%m-%d')) & (data$tpep_dropoff_datetime <= currentDate_time), ]
      one_data.summary = one_data %>% group_by(by3=cut(tpep_dropoff_datetime, "3 min")) %>%
        summarise(mean_speed =mean(speed_milesPerMin, na.rm = TRUE)) %>% as.data.frame
      
      one_data2 <- one_data[(one_data$tip_amount>0) & (one_data$tip_amount< 20), ]
      one_data2$mean_tip_amount_percent <- one_data2$tip_amount/(one_data2$total_amount - one_data2$tip_amount)
      
      one_data.summary2 = one_data2 %>% group_by(by3=cut(tpep_dropoff_datetime, "3 min")) %>%
        summarise(mean_tip =mean(mean_tip_amount_percent, na.rm = TRUE)) %>% as.data.frame      
      
      times <- format(strptime(one_data.summary$by3, "%Y-%m-%d %H:%M:%S"), "%H:%M")
      par(mfrow = c(1,1),mar=c(4, 4, 3.5, 1))
      
      plot(one_data.summary$mean_speed, type = 'l', col = 'blue',
           ylab = "Value", xlab = 'Time',xaxt='n', main = 'Driving Speed vs Tip Amount')
      lines(one_data.summary2$mean_tip, col = 'red' )
      axis(1,at=seq(1,length(times), by = 6),labels=times[seq(1,length(times), by = 6)])
      legend("topright", legend = c('Speed (miles/min)','Tip'), col = c('blue', 'red'), lty=1:2, cex=0.8)
    # }
    
    
    
    
    # drawDrivingSpeed(newdata, t1)
    
  })
  
}

shinyApp(ui = ui, server = server)


