#install.packages("RGraphics")
#install.packages("gridExtra")

packages.used=c("mapsapi", "xml2", "leaflet", "shiny",'data.table',"fasttime",'ggmap','dplyr',
                'maptools','plotly', 'reshape2','shinydashboard','ggplot2','flexdashboard','rgal',
                'lubridate','geojsonio','gridExtra')

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(mapsapi); library(xml2); library(data.table)
library(fasttime); library(dplyr); library(ggmap)
library(maptools); library(plotly); library(leaflet)
library(shiny); library(reshape2); library(shinydashboard)
library(ggplot2); library(flexdashboard); library(rgdal)
library(lubridate); library(geojsonio); library(gridExtra)
source('functions.R')

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



shinyServer(function(input, output, session) {
  
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
      p <- barplot(as.vector(borough_final[,input$Month]),ylim=c(0,45000),main=paste("Number of pick-ups in", text),xlab="Boroughs",ylab="Counts",names.arg=c("Manhattan","Brooklyn","Queens","Bronx","Staten Island"),col="pink")
      text(p,as.vector(borough_final[,input$Month])+10*sign(as.vector(borough_final[,input$Month])),labels=as.vector(borough_final[,input$Month]),xpd=TRUE)
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
      pie(DF$count,labels=percentlbls,main=paste("Distribution Chart of Fare Per Mile in",text),cex=1,col=cols)
      legend("topleft",c("0-2 ","2-4","4-6","6-8","8-10"),cex=0.75,fill=cols,ncol=1)
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
      setView(lat=40.75241, lng=-73.99961, zoom=11) %>%
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


