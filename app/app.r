packages.used=c("shiny","leaflet","readr","DT",
                "lubridate","ggmap","googleway",
                "measurements","geosphere")

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

#set key for google api
set_key("AIzaSyCFZGyEThOulZbOxXQhgfdJFURBEnnZvYM")
#read the processed data
per = read_csv("../data/per.csv")
#set current longitude and latitude
cu_loc = c(40.8077, -73.9597)

#user interface
ui <- fluidPage(
  tabPanel(title = NULL,
           div(class = "outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               #set the whole-page map outline
               leafletOutput(outputId = "map", width="100%", height="100%"),
           
               #a draggable panel put on the left side
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                           width = 330, height = "auto",
                           
                           #title on the absolutepanel
                           h3("Welcome to Yellow Taxi"),
                           #input origin
                           textInput(inputId = "start",label = "From: ", value = ""),
                           #input destination
                           textInput(inputId = "end",label = "To: ", value = ""),
                           #input past time period to show drop-off locations
                           numericInput(inputId = "lastmin", label = "Show drop-off locations in the past (mins)", value = 5, min = 0, max = 30),
                           #input radius of the shpere to show drop-off locations
                           sliderInput(inputId = "radius", label = "Show drop-off locations with radius", min = 0, max = 3, value = 2, step = 0.2, round = -1),
                           #input of unit
                           selectInput(inputId = "units", label = "Choose a Unit", choices = c("km","mi"), selected = "mi"),
                           #indicate whether or not to show past routes of the taxis
                           checkboxInput(inputId = "showroutes", label = "Show past routes", value = TRUE),
                           #indicate whether or not to show the walking routes to the drop-off locations
                           checkboxInput(inputId = "showwalk", label = "Show walking routes", value = FALSE),
                           #indicate whether or not to compare with the the public transit duration
                           checkboxInput(inputId = "transit", label = "Compare with public transit duration", value = TRUE),
                           
                           # submitButton(text = "Search"),
                           
                           #output fare estimation
                           textOutput(outputId = "fares"),
                           #output google estimated distance
                           textOutput(outputId = "ggdis"),
                           #output google estimated duration
                           textOutput(outputId = "ggD"),
                           #output google estimated duration for public transit
                           textOutput(outputId = "ggDt")
                           )
           )
  )
)




#server interface
server<-function(input, output, session){
  
  #reactive start location
  start_loc = reactive({
    if(nchar(input$start)>0){
      input$start
    } else{
      "current location"
    }
  })
  
  #reactive end location
  end_loc = reactive({
    if(nchar(input$end)>0){
      input$end
    } else{
      "current location"
    }
  })
  
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
    
    if(input$transit){
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
    }
  })
  
  #reactive past time period of drop-offs
  last_min = reactive({
    input$lastmin
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
  
  #output fare esmitation
  output$fares = renderText({
    #update fare estimation every minute (60 seconds)
    invalidateLater(60000, session)
    
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
    if(month(t)==7){
      if(day(t)==31){
        day(t)=30
      }
      month(t)=6
    }else if(month(t)==11){
      if(day(t)>29){
        day(t) = day(t)-2
      }
      month(t)=2
    }else if(month(t)>=8 & month(t)<=12){
      month(t) = 13-month(t)
    }
    
    #get previous 5 minutes data for fare estimation
    prev_5min = na.omit(per%>%subset(tpep_dropoff_datetime>=(t-300) & tpep_dropoff_datetime<=t))
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
    }
    else if(substr(mile, start = nchar(mile)-1, stop = nchar(mile))=="ft"){
      m = conv_unit(mi, from = "ft", to = "m")
      paste("Distance: ", round(m,1), " m", sep = "")
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
    if(input$transit){
      durat_transit = as.data.frame(dis_transit()$rows[[1]])[1,2][1,1]
      paste("Duration for public transit: ", durat_transit, sep = "")}
  })
  
  
  #output map
  output$map = renderLeaflet({
    
    #convert current time to the time available in the dataset(Jan - June, 2016)
    #convert July to June, August to May...etc.
    t = Sys.time()
    t = as.POSIXct(format(t),tz="UTC")
    year(t) = 2016
    if(month(t)==7){
      if(day(t)==31){
        day(t)=30
      }
      month(t)=6
    }else if(month(t)==11){
      if(day(t)>29){
        day(t) = day(t)-2
      }
      month(t)=2
    }else if(month(t)>=8 & month(t)<=12){
      month(t) = 13-month(t)
    }
    
    #get previous x minutes data for drop-off locations (default: 5 min)
    prev_5min = na.omit(per%>%subset(tpep_dropoff_datetime>=(t-60*last_min()) & tpep_dropoff_datetime<=t))
    #remove possible disturbing value for map plotting
    prev_5min = prev_5min[prev_5min$dropoff_longitude != 0 & prev_5min$dropoff_latitude != 0,]
    #get drop-off longitude and latitude
    lng_5 = prev_5min$dropoff_longitude
    lat_5 = prev_5min$dropoff_latitude

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
    blackendIcon <- makeIcon(
      iconUrl = "../doc/figs/blackend.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 30
    )
    
    #decode google directions' polylines
    polyl = direction_polyline(df())
    pl_decode = decode_pl(polyl)
    
    #draw a map
    MAP=leaflet(pl_decode) %>%
          addTiles()%>%
          setView(lng = -73.971035, lat = 40.775659, zoom = 12)
    
    #get start location longitude and latitude
    startpoint = c(pl_decode$lon[1], pl_decode$lat[1])
    
    #get within circle data for drop-offs
    r = conv_unit(rad(), uni(),"m")
    within_circle = prev_5min[as.vector(distm(startpoint, cbind(lng_5, lat_5)))<r,]
    #within circle drop off time
    wc_time = format(within_circle$tpep_dropoff_datetime,"%H:%M:%S")
    
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
      
         #add within circle drop-off icons
         MAP = MAP%>%
         addMarkers(lat = within_circle$dropoff_latitude[i], 
                 lng = within_circle$dropoff_longitude[i], 
                 icon = blackendIcon,
                 popup = paste(paste("drop-off time: ",wc_time[i]),
                               paste("walking time: ", walk_time),
                               paste("walking distance: ", walk_dis),
                               sep = "<br/>"))
        }
      }
    }
    
    
      #draw THE origin-destination route with start & end icons and a big circle
      MAP = MAP %>%
        addPolylines(lng = ~lon, lat = ~lat)%>%
        addMarkers(lng = pl_decode$lon[1], 
                   lat = pl_decode$lat[1], 
                   popup = start_loc(),icon = startIcon)%>%
        addMarkers(lng = pl_decode$lon[nrow(pl_decode)], 
                   lat = pl_decode$lat[nrow(pl_decode)], 
                   popup = end_loc(),icon = endIcon)%>%
        addCircles(lng = pl_decode$lon[1], 
                   lat = pl_decode$lat[1],
                   radius = r,
                   opacity = 0.1,
                   stroke = FALSE)
      #show the map
      MAP
  })
  
}

shinyApp(ui = ui, server = server)

