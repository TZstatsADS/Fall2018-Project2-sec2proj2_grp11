#install.packages("RGraphics")
#install.packages("gridExtra")

packages.used=c("mapsapi", "xml2", "leaflet", "shiny",'data.table',"fasttime",'ggmap','dplyr',
                'maptools','plotly', 'reshape2','shinydashboard','ggplot2','flexdashboard','rgdal',
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


