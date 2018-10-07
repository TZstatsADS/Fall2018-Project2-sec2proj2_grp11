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


library(shiny)
library(leaflet)
library(readr)
library(DT)
library(lubridate)
library(ggmap)
library(googleway)
library(measurements)
library(geosphere)

set_key("AIzaSyCFZGyEThOulZbOxXQhgfdJFURBEnnZvYM")

per = read_csv("../data/per.csv")

ui <- fluidPage(
  tabPanel("Calculating",
           div(class = "outer",
               
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               leafletOutput(outputId = "map", width="100%", height="100%"),
           
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                           draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
                           width = 330, height = "auto",
                           
                           h3("Welcome to Yellow Taxi"),
                           textInput(inputId = "start",label = "From: ", value = NULL),
                           textInput(inputId = "end",label = "To: ", value = NULL),
                           numericInput(inputId = "lastmin", label = "Show drop-off locations in the past (mins)", value = 5, min = 0, max = 15),
                           sliderInput(inputId = "radius", label = "Show drop-off locations with radius", min = 0, max = 10,value = 2, round = TRUE),
                           selectInput(inputId = "units", label = "Choose a Unit", choices = c("km","mi"), selected = "mi"),
                           checkboxInput(inputId = "showroutes", label = "Show past routes", value = TRUE),
                           checkboxInput(inputId = "transit", label = "Compare with public transit duration", value = TRUE),
                           # submitButton(text = "Search"),
                           textOutput(outputId = "fares"),
                           textOutput(outputId = "ggdis"),
                           textOutput(outputId = "ggD"),
                           textOutput(outputId = "ggDt")
                          )
           )
           
  )
  
)





server<-function(input, output, session) {
  
  start_loc = reactive({
    input$start
  })
  
  end_loc = reactive({
    input$end
  })
  
  cu_loc = c(40.8077, -73.9597)
  
  df = reactive({
    
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
    
    
    google_directions(origin = ori,
                      destination = en,
                      mode = "driving")
    
    # if(nchar(input$start)>0 & nchar(input$end)>0){
    #   google_directions(origin = paste(input$start,", New York", sep = ""),
    #                      destination = paste(input$end,", New York", sep = ""),
    #                      mode = "driving")
    # }
    # else if(nchar(input$start)==0 & nchar(input$end)>0) {
    #   google_directions(origin = cu_loc,
    #                     destination = paste(input$end,", New York", sep = ""),
    #                     mode = "driving")
    # }
    # else if(nchar(input$start)==0 & nchar(input$end)==0) {
    #   google_directions(origin = cu_loc,
    #                     destination = cu_loc,
    #                     mode = "driving")
    # }
    # else if(nchar(input$start)>0 & nchar(input$end)==0) {
    #   google_directions(origin = paste(input$start,", New York", sep = ""),
    #                     destination = cu_loc,
    #                     mode = "driving")
    # }
    
    
    
    })
  
  dis = reactive({
    
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
                    mode = "driving",
                    units = "imperial")
    
    # if(nchar(input$start)>0 & nchar(input$end)>0){
    #   google_distance(origin = paste(input$start,", New York", sep = ""),
    #                     destination = paste(input$end,", New York", sep = ""),
    #                     mode = "driving",
    #                     units = "imperial")
    # }
    # else if(nchar(input$start)==0 & nchar(input$end)>0) {
    #   google_distance(origin = cu_loc,
    #                     destination = paste(input$end,", New York", sep = ""),
    #                     mode = "driving",
    #                     units = "imperial")
    # }
    # else if(nchar(input$start)==0 & nchar(input$end)==0) {
    #   google_distance(origin = cu_loc,
    #                     destination = cu_loc,
    #                     mode = "driving",
    #                     units = "imperial")
    # }
    # else if(nchar(input$start)>0 & nchar(input$end)==0) {
    #   google_distance(origin = paste(input$start,", New York", sep = ""),
    #                     destination = cu_loc,
    #                     mode = "driving",
    #                     units = "imperial")
    # }
    
                 })
  
  dis_transit = reactive({
    
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
  
  last_min = reactive({
    input$lastmin
  })
  
  uni = reactive({
    input$units
  })
  
  rad = reactive({
    input$radius
  })
  
  showroutes_check = reactive({
    input$showroutes
  })
  
  output$fares = renderText({
    invalidateLater(60000, session)
    
    mile = as.data.frame(dis()$rows[[1]])[1,1][1,1]
    mi = as.numeric(substr(mile, start = 1, stop = nchar(mile)-3))
    
    if(substr(mile, start = nchar(mile)-1, stop = nchar(mile))=="ft"){
      mi = conv_unit(mi, "ft", "mi")
    }
    
    
    
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
    
    prev_5min = na.omit(per%>%subset(tpep_dropoff_datetime>=(t-300) & tpep_dropoff_datetime<=t))
    prev_5min = prev_5min[(prev_5min$mph != Inf 
                           & prev_5min$dollar_per_mile != Inf
                           & prev_5min$mph != (-Inf)
                           & prev_5min$dollar_per_mile != (-Inf))
                           & prev_5min$dollar_per_mile > 0 ,]
    avg_dpm = mean(prev_5min$dollar_per_mile, na.rm = TRUE)
    do = mi*avg_dpm
    if (is.na(do) | is.null(do)){
      "Estimated fare amount: "
    } else
    paste("Estimated fare amount: $",ifelse(is.na(do),0,round(do,2)), sep = "")
  })
  
  
  output$ggdis = renderText({
    mile = as.data.frame(dis()$rows[[1]])[1,1][1,1]
    mi = as.numeric(substr(mile, start = 1, stop = nchar(mile)-3))
    if(uni()=="mi"){
      paste("Estimated distance: ", mile, sep = "")
    }
    else if(substr(mile, start = nchar(mile)-1, stop = nchar(mile))=="ft"){
      m = conv_unit(mi, from = "ft", to = "m")
      paste("Estimated distance: ", round(m,1), " m", sep = "")
    }
    else{
      km = conv_unit(mi, from = "mi", to = "km")
      paste("Estimated distance: ", round(km,1), " km", sep = "")
    }
  })
  
  output$ggD = renderText({
    # if(is.null(start_loc()) | is.null(end_loc())){
    #   "Estimated duration: "
    # }
    # else{
      durat = as.data.frame(dis()$rows[[1]])[1,2][1,1]
      paste("Estimated duration: ", durat, sep = "")
      
    # }
    
  })
  
  output$ggDt = renderText({
    if(input$transit){
      durat_transit = as.data.frame(dis_transit()$rows[[1]])[1,2][1,1]
      paste("Estimated duration for public transit: ", durat_transit, sep = "")}
  })
  
  
  
  output$map = renderLeaflet({
    
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
    
    prev_5min = na.omit(per%>%subset(tpep_dropoff_datetime>=(t-60*last_min()) & tpep_dropoff_datetime<=t))
    prev_5min = prev_5min[(prev_5min$mph != Inf
                           & prev_5min$dollar_per_mile != Inf
                           & prev_5min$mph != (-Inf)
                           & prev_5min$dollar_per_mile != (-Inf)),]
    prev_5min = prev_5min[prev_5min$dropoff_longitude != 0 & prev_5min$dropoff_latitude != 0,]
    lng_5 = prev_5min$dropoff_longitude
    lat_5 = prev_5min$dropoff_latitude

    taxiIcon <- makeIcon(
      iconUrl = "../doc/figs/taxi.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 15
    )
    
    startIcon <- makeIcon(
      iconUrl = "../doc/figs/navigation.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 15
    )
    
    endIcon <- makeIcon(
      iconUrl = "../doc/figs/end.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 30
    )
    
    blackendIcon <- makeIcon(
      iconUrl = "../doc/figs/blackend.png",
      iconWidth = 30, iconHeight = 30,
      iconAnchorX = 15, iconAnchorY = 30
    )
    
 
    polyl = direction_polyline(df())
    pl_decode = decode_pl(polyl)
    
    startpoint = c(pl_decode$lon[1], pl_decode$lat[1])
    
    r = conv_unit(rad(), uni(),"m")
    within_circle = prev_5min[as.vector(distm(startpoint, cbind(lng_5, lat_5)))<r,]
    wc_time = format(within_circle$tpep_dropoff_datetime,"%H:%M:%S")
    
      MAP=leaflet(pl_decode) %>%
          addTiles()%>%
          setView(lng = -73.971035, lat = 40.775659, zoom = 12)

    if(!is.null(input$start) & !is.null(input$end)){
      for (i in 1:nrow(within_circle)){
      
        if (showroutes_check() == TRUE){
          df2 = google_directions(origin = c(within_circle$pickup_latitude[i], within_circle$pickup_longitude[i]),
                      destination = c(within_circle$dropoff_latitude[i], within_circle$dropoff_longitude[i]),
                      mode = "driving")
          polyl2 = direction_polyline(df2)
          pl_decode2 = decode_pl(polyl2)
          MAP = MAP%>%
            addPolylines(lat = pl_decode2[,1], lng = pl_decode2[,2], color = "lightsteelblue3", opacity = 0.5)
        }
        
      
      MAP = MAP%>%
      addMarkers(lat = within_circle$dropoff_latitude[i], 
                 lng = within_circle$dropoff_longitude[i], 
                 icon = blackendIcon,
                 popup = wc_time[i])
    }
    }
    
      
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
      MAP
  })
  
}

shinyApp(ui = ui, server = server)
