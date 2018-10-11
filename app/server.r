packages.used=c("shiny","leaflet","readr","DT",
                "lubridate","ggmap","googleway",
                "measurements","geosphere",
                "shinyjs", "dplyr","shinydashboard",
                "googleVis","corrplot","flexdashboard",
                "rgdal","geojsonio","gridExtra","plotly",
                "mapsapi", "xml2",'data.table',"fasttime",
                'maptools', 'reshape2','ggplot2')

# check packages that need to be installed.
packages.needed=setdiff(packages.used,
                        intersect(installed.packages()[,1],
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

#load libraries
library(shiny);library(leaflet);library(readr);library(DT);library(lubridate);
library(ggmap);library(googleway);library(measurements);library(geosphere)
library(shinyjs);library(dplyr);library(shinydashboard);library(googleVis)
library(corrplot);library(flexdashboard);library(rgdal);library(geojsonio)
library(gridExtra);library(plotly);library(mapsapi);library(xml2);library(data.table)
library(fasttime);library(maptools);library(reshape2);library(ggplot2)
source('../lib/functions.R')

##########Load all the RData file 
load('../output/meanFarePerDistance.RData')
load('../output/meanTipsPerDistance.RData')
load('../output/MeanTipPerDistanceBorough.RData')
load('../output/MeanFarePerDistanceBorough.RData')
load('../output/month_fare_matrix.RData')
load('../output/picks.RData')
rownames(FPD_final) <- c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")
rownames(TPD_final) <-c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")
boroughs <- readOGR(dsn="../data/BB.geojson")
boroughs <- spTransform(boroughs,CRS("+init=epsg:4326"))
bounds <- bbox(boroughs)


week_data = combine_data()



#set key for google api
set_key("AIzaSyCFZGyEThOulZbOxXQhgfdJFURBEnnZvYM")

#set current longitude and latitude
cu_loc = c(40.8077, -73.9597) #iab

shinyServer(function(input, output, session) {
  
  per = reactive({
    if(input$day == "Monday"){
      read_csv("../data/per20160104.csv")
    } else if(input$day == "Tuesday"){
      read_csv("../data/per20160105.csv")
    } else if(input$day == "Wednesday"){
      read_csv("../data/per20160106.csv")
    } else if(input$day == "Thursday"){
      read_csv("../data/per20160107.csv")
    } else if(input$day == "Friday"){
      read_csv("../data/per20160108.csv")
    } else if(input$day == "Saturday"){
      read_csv("../data/per20160109.csv")
    } else if(input$day == "Sunday"){
      read_csv("../data/per20160110.csv")
    }
  })

  t = reactive({
    t00 = Sys.time()
    t00 = as.POSIXct(format(t00),tz="UTC")

    #convert day to the days available in the dataset

    year(t00) = 2016
    month(t00) = 1
    if(input$day == "Monday"){
      day(t00) = 4
    } else if (input$day == "Tuesday"){
      day(t00) = 5
    } else if (input$day == "Wednesday"){
      day(t00) = 6
    } else if (input$day == "Thursday"){
      day(t00) = 7
    } else if (input$day == "Friday"){
      day(t00) = 8
    } else if (input$day == "Saturday"){
      day(t00) = 9
    } else if (input$day == "Sunday"){
      day(t00) = 10
    }
    t00
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

  #output fare esmitation
  output$fares = renderText({

    #obtain distance number
    mile = as.data.frame(dis()$rows[[1]])[1,1][1,1]
    mi = as.numeric(substr(mile, start = 1, stop = nchar(mile)-3))

    #if google unit is ft, convert to mi
    if(substr(mile, start = nchar(mile)-1, stop = nchar(mile))=="ft"){
      mi = conv_unit(mi, "ft", "mi")
    }

    #since we have only 1 day's data, the previous 30 mins data is not completely available for time earlier than 00:05:00
    if (hour(t())==0 & minute(t()) <30){
      prev_30min = na.omit(per()%>%subset(tpep_dropoff_datetime>=(t()-30*60)+24*60*60 | tpep_dropoff_datetime<=t()))
    } else{
      #get previous 30 minutes data for fare estimation
      prev_30min = na.omit(per()%>%subset(tpep_dropoff_datetime>=(t()-30*60) & tpep_dropoff_datetime<=t()))
    }

    #remove possible disturbing value
    prev_30min = prev_30min[(prev_30min$mph != Inf
                             & prev_30min$dollar_per_mile != Inf
                             & prev_30min$mph != (-Inf)
                             & prev_30min$dollar_per_mile != (-Inf))
                            & prev_30min$dollar_per_mile > 0 ,]
    #calculate average dolloar per mile
    avg_dpm = mean(prev_30min$dollar_per_mile, na.rm = TRUE)
    #calculate total dollar amount
    do = mi*avg_dpm
    #print fare estimation
    if (is.na(do) | is.null(do)){
      " Estimated Fare amount: "
    } else
      paste(" Estimated Fare amount: $",ifelse(is.na(do),0,round(do,2)), sep = "")
  })
  
  output$notes = renderText({
    "(Estimated from last 30 mins fare rate)"
  })

  #output google distance estimation
  output$ggdis = renderText({
    #obtain distance number
    mile = as.data.frame(dis()$rows[[1]])[1,1][1,1]
    mi = as.numeric(substr(mile, start = 1, stop = nchar(mile)-3))

    #different number output for different units (mi, km), also considering ft & m
    if(uni()=="mi"){
      paste(" Distance: ", mile, sep = "")
    } else if(substr(mile, start = nchar(mile)-1, stop = nchar(mile))=="ft"){
      m = conv_unit(mi, from = "ft", to = "m")
      paste(" Distance: ", round(m), " m", sep = "")
    } else if (mi < 0.621371){#less than 1 km but still in unit "mile"
      m = conv_unit(mi, from = "mi", to = "m")
      paste(" Distance: ", round(m), " m", sep = "")
    }
    else{
      km = conv_unit(mi, from = "mi", to = "km")
      paste(" Distance: ", round(km,1), " km", sep = "")
    }
  })

  #output google estimated duration
  output$ggD = renderText({

    durat = as.data.frame(dis()$rows[[1]])[1,2][1,1]
    paste(" Duration: ", durat, sep = "")

  })

  #output google estimated duration for public transit
  output$ggDt = renderText({
    durat_transit = as.data.frame(dis_transit()$rows[[1]])[1,2][1,1]
    paste(" Duration for public transit: ", durat_transit, sep = "")
  })


  #output map
  output$map0 = renderLeaflet({

    #since we have only 1 day's data, the next x min data is not completely available for time later than 23:(60-x):00
    if (hour(t())== 23 & minute(t()) + next_min() > 60 ){
      next_xmin = na.omit(per()%>%subset(tpep_dropoff_datetime<=(t()+60*next_min())-24*60*60 | tpep_dropoff_datetime>=t()))
    } else{
      #get previous x minutes data for fare estimation
      next_xmin = na.omit(per()%>%subset(tpep_dropoff_datetime<=(t()+60*next_min()) & tpep_dropoff_datetime>=t()))
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
    yellowendIcon = list()

    for (i in 1:6){
      yellowendIcon[[i]] <- makeIcon(
        iconUrl = "../doc/figs/yellowend.png",
        iconWidth = 60-10*(i-1), iconHeight = 60-10*(i-1),
        iconAnchorX = 30-5*(i-1), iconAnchorY = 60-10*(i-1)
      )
    }

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
    time_diff_in_sec = within_circle$tpep_dropoff_datetime - t()
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
            w_d = walk_dis
          } else if(substr(walk_dis, start = nchar(walk_dis)-1, stop = nchar(walk_dis))=="ft"){
            m = conv_unit(mi, from = "ft", to = "m")
            w_d = paste(round(m), " m", sep = "")
          } else if (mi < 0.621371){#less than 1 km but still in unit "mile"
            m = conv_unit(mi, from = "mi", to = "m")
            w_d = paste(round(m), " m", sep = "")
          }
          else{
            km = conv_unit(mi, from = "mi", to = "km")
            w_d = paste(round(km,1), " km", sep = "")
          }

          for(j in 1:6){
            if(time_diff_in_min[i]==(j-1)){
              ic = yellowendIcon[[j]]
            }
          }

          #add within circle drop-off icons
          MAP = MAP%>%
            addMarkers(lat = within_circle$dropoff_latitude[i],
                       lng = within_circle$dropoff_longitude[i],
                       icon = ic,
                       popup = paste(paste("drop-off time: ",wc_time[i]),
                                     # paste("(arriving in ",time_diff_in_min[i]," ", plural[i], ")"),
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
      end_loc = input$end
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

  output$plotDrivingSpeed = renderPlot({

    t1 = t()

    if(hour(t1)==0 & minute(t1) < 7){
      hour(t1)=23
      minute(t1)=59
    }

    newdata <- per()

    currentDate_time = t1
    data <- newdata[(newdata$trip_duration_inMins < 180) & (newdata$speed_milesPerMin < 5), ]

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
         ylab = "Value", xlab = 'Time',xaxt='n', main = 'Driving Speed vs Tip Proportion', ylim = c(0.1,0.55))
    lines(one_data.summary2$mean_tip, col = 'red' )
    axis(1,at=seq(1,length(times), by = 6),labels=times[seq(1,length(times), by = 6)])
    legend("topright", legend = c('Speed (miles/min)','Tip'), col = c('blue', 'red'), lty=1:2, cex=0.8)
  })
  
  ########################################### Borough Statistics Dashboard  ##########################################
  fare_data_borough <- reactive({
    if(!is.null(input$Month)){
      nyc_fare_borough <- data.frame(FPD=FPD_final[,input$Month],TPD=TPD_final[,input$Month],boro_name=c("Manhattan","Brooklyn","Queens","Bronx","Staten Island"))
      
    }
    else{
      nyc_fare_borough <- data.frame(FPD=FPD_final[,1],TPD=TPD_final[,1],boro_name=c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")) 
    }
    joinedDataset <- boroughs
    joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, nyc_fare_borough, by="boro_name"))
    
    #boroughs<-SpatialPolygonsDataFrame(boroughs, data=boroughs1)
    return(joinedDataset)
  })
  month_fare <- reactive({
    if(!is.null(input$Month)){
      month_fare_neighbor <- month_fare_matrix[,input$Month]
    }
    else{
      month_fare_neighbor <- month_fare_matrix[,1]
    }
    month_fare_neighbor_new <- data.frame(count=month_fare_neighbor,group=c("0-2","2-4","4-6","6-8","8-10"))
    return(month_fare_neighbor_new)
  })
  
  
  output$plot1 <- renderPlot({
    mymonth = c("January","Febrary","March","April","May","June","July","September","October","November","December")
    text <- mymonth[input$Month]
    if(input$Month <= 6){
      df <- data.frame(Count=as.vector(borough_final[,input$Month]),Borough=c("Manhattan","Brooklyn","Queens","Bronx","Staten Island"))
      df$Borough = factor(df$Borough, levels = c("Manhattan","Queens","Brooklyn","Bronx", "Staten Island"))
      ggplot(df,aes(x=Borough,y=Count,fill=Borough))+
        geom_bar(stat="identity")+
        ggtitle(paste("Pick-up Numbers in Each Borough in",text, sep=" ")) +
        theme(plot.title = element_text(color="black", face="bold", hjust = 0.5))+
        labs(x = "Borough", y = "Pick-up Numbers")
    }
    else{
      barplot(0,main=paste("No Data Available For",text))
    }
  })
  
  output$plot2 <- renderPlot({
    mymonth = c("January","Febrary","March","April","May","June","July","September","October","November","December")
    text <- mymonth[input$Month]
    if(input$Month<=6){
      DF <- month_fare()
      cols <- c("lavender","royalblue","rosybrown","salmon1","yellowgreen")
      percent <- round(100*(DF$count/sum(DF$count)),1)
      percentlbls <- paste(percent,"%",sep="")
      pie(DF$count,labels=percentlbls,main=paste("Distribution Chart of Fare Per Mile in",text),cex=1,col=cols, border = FALSE)
      legend(x=1.2,y=0.2,c("0-2 ","2-4","4-6","6-8","8-10"),cex=1,fill=cols,ncol=1, border = FALSE, title = "dollars/mile")
    }
    else{
      pie(1,main=paste("No Data Available For",text ))
    }})
  
  output$maxbox <- renderInfoBox(
    {
      DF <- fare_data_borough()
      outData <- DF$FPD
      max_value <- max(outData,na.rm = T)
      max_neigh <- DF$boro_name[outData==max_value]
      infoBox(max_neigh,paste("$",round(max_value,2),"per mile"),icon = icon("chevron-up"),color='yellow')
    }
  )
  output$minbox <- renderInfoBox(
    {
      DF <- fare_data_borough()
      outData <- DF$FPD
      min_value <- min(outData,na.rm = T)
      min_neigh <- DF$boro_name[outData==min_value]
      infoBox(min_neigh,paste("$",round(min_value,2),"per mile"),icon = icon("chevron-down"),color='blue')
    }
  )
  output$medbox <- renderInfoBox(
    {
      DF <- fare_data_borough()
      mymonth = c("January","Febrary","March","April","May","June","July","August","September","October","November","December")
      text <- mymonth[input$Month]
      outData <- DF$FPD
      mean_value <- mean(outData,na.rm = T)
      mean_neigh <- DF$boro_name[outData==mean_value]
      infoBox(paste("Mean Fare for", text),paste("$",round(mean_value,2),"per mile"),icon = icon("calculator"),color='red')
    }
  )
  
  
  
  
  
  ########################################### Taxi Animation Dashboard  ##########################################
  output$map <- renderLeaflet({
    leaflet() %>%
      #addTiles() %>%
      setView(lat=40.76241, lng=-73.98961, zoom=11) %>%
      addProviderTiles('CartoDB.DarkMatter')
    # check provider choices: https://leaflet-extras.github.io/leaflet-providers/preview/
  })  # End of output$map
  
  
  observe({
    # Update slider when date change
    updateSliderInput(session, "time_range",   
                      min = as.POSIXct(paste(unlist(strsplit(input$selectDate, ' '))[1], "00:00:00", sep = ' ')),
                      max = as.POSIXct(paste(unlist(strsplit(input$selectDate, ' '))[1], "23:59:59", sep = ' ')),
                      value = c(as.POSIXct(paste(unlist(strsplit(input$selectDate, ' '))[1], "00:00:00", sep = ' ')))
    )
  })
  
  
  sub.data <- reactive({
    # choose the subdata which taxi is currently on trip
    return(week_data[(week_data$tpep_pickup_datetime <= fastPOSIXct(input$time_range)) & (week_data$tpep_dropoff_datetime > fastPOSIXct(input$time_range)),])
    
  })
  
  
  observe({
    sub_data <- sub.data()
    leafletProxy("map") %>% 
      clearShapes() %>% 
      addCircles(sub_data$pickup_longitude,sub_data$pickup_latitude, weight = 3, radius=40, 
                 color="#ffa500", stroke = TRUE, fillOpacity = 0.8)
  }) # End of  observe
  
  sub.data2 <- reactive({
    # choose the subdata which taxi is currently on trip
    begin <- as.POSIXct(paste(unlist(strsplit(input$selectDate, ' '))[1], "00:00:00", sep = ' ')) 
    end <- as.POSIXct(paste(unlist(strsplit(input$selectDate, ' '))[1], "23:59:59", sep = ' '))
    
    return(week_data[(week_data$tpep_pickup_datetime >= begin) & 
                       (week_data$tpep_pickup_datetime<= end) &
                       (week_data$tpep_dropoff_datetime <= end), ])
    
    
  })
  
  output$histPlot1 <- renderPlot({
    subdata2 <- sub.data2()
    
    #Extract Time
    subdata2$hour = hour(subdata2$tpep_pickup_datetime) + minute(subdata2$tpep_pickup_datetime)/60 + second(subdata2$tpep_pickup_datetime)/3600
    
    #Create Bins
    bins=c(paste0(rep(c(paste0(0,0:9),10:23), each=4),".", c("00",25,50,75))[-1],"24:00")
    
    #Divide Data Into Bins
    subdata2$bins = cut(subdata2$hour, breaks=seq(0, 24, 0.25), labels=bins)
    
    #Reformat to Numeric
    subdata2$bins <- as.numeric(as.character(subdata2$bins))
    
    #Histogram
    hist(subdata2$bins)
    
    #With ggplot
    library(ggplot2)
    ggplot(subdata2, aes(bins)) +
      geom_histogram(color = '#337ab7', fill="#337ab7", alpha=0.5) +
      xlab('Hour of Day') + 
      labs(title = "Taxi Demand Histogram")
  })
  
  
  
  output$histPlot2 <- renderPlot({
    subdata2 <- sub.data2()
    one_data <- subdata2[,c('tpep_dropoff_datetime', 'speed_milesPerMin')]
    one_data$speed_milesPerMin <- round(one_data$speed_milesPerMin*60, 2)  # miles/hour
    one_data$tpep_dropoff_datetime <- as.POSIXct(one_data$tpep_dropoff_datetime)
    
    one_data.summary = one_data %>% group_by(by3=cut(tpep_dropoff_datetime, "3 min")) %>%
      summarise(mean_speed =mean(speed_milesPerMin)) %>% as.data.frame
    
    times <- format(strptime(one_data.summary$by3, "%Y-%m-%d %H:%M:%S"), "%H:%M")
    
    plot(one_data.summary$mean_speed, type = 'l', col = 'tan3',
         ylab = "Speed (miles/hour)", xlab = 'Time',xaxt='n', main = 'Learning Traffic From Driving Speed')
    axis(1,at=seq(1,length(times), by = 6),labels=times[seq(1,length(times), by = 6)])
    
  })
  
  
  output$taxibox <- renderInfoBox(
    {
      sub_data <- sub.data()
      infoBox('Taxi on Trip:',nrow(sub_data),icon = icon("taxi"),color='yellow')
    }
  )
  
  
  output$meanSpeedbox <- renderInfoBox(
    {
      sub_data <- sub.data()
      infoBox('Driving Speed:', paste(round(mean(sub_data$speed_milesPerMin*60),2), ' miles/hour'),
              icon = icon("refresh"),color='red')
    }
  )
  
  output$meanDurationbox <- renderInfoBox(
    {
      sub_data <- sub.data()
      tip_percent= (sum(sub_data$tip_amount)/sum(sub_data $total_amount - sub_data $tip_amount))*100
      infoBox('Mean Tip Percent:', paste(round(mean(tip_percent),2), '%'),
              icon = icon("dollar"),color='blue')
    }
  )
  
})


