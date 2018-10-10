#install.packages("RGraphics")
#install.packages("gridExtra")
library(ggmap)
library(maptools)
library(plotly)
library(leaflet)
library(shiny)
library(reshape2)
library(shinydashboard)
library(ggplot2)
library(flexdashboard)
library(rgdal)
library(lubridate)
library(geojsonio)
library(gridExtra)
##########Load all the RData file 
#shapefile <- readShapePoly('MyApp/data/nynta_shapefile/nynta.shp')
setwd("../")
load('output/meanFarePerDistance.RData')
load('output/meanTipsPerDistance.RData')
load('output/MeanTipPerDistanceBorough.RData')
load('output/MeanFarePerDistanceBorough.RData')
load('output/month_fare_matrix.RData')
load('output/picks.RData')
rownames(FPD_final) <- c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")
rownames(TPD_final) <-c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")
boroughs <- readOGR(dsn="data/BB.geojson")
boroughs <- spTransform(boroughs,CRS("+init=epsg:4326"))

bounds <- bbox(boroughs)

shinyServer(function(input, output, session) {
  
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
  
  ############Can't find the bug for the nyc borough map.........So sad
  output$nycmap<-renderLeaflet({
  leaflet() %>%
   addTiles() %>%
  
  #Centre the map in the middle of our co-ordinates
  setView(mean(bounds[1,]),
   mean(bounds[2,]),
  zoom=5 # set to 10 as 9 is a bit too zoomed out
  )       
  
  })
  
  #observe({
  #theData<-fare_data_borough()
  #pal <- colorQuantile("YlGn", theData$FPD, n = 10)
  #text <- as.character(month(ymd(010101) + months(input$Month-1),label=TRUE,abbr=FALSE))
  #set text for the clickable popup labels
  #borough_popup <- paste0("<strong>Borough: </strong>", 
  #theData$boro_name, 
  #FO "<br><strong>",
  #text," 
  #Fare Per Distance: </strong>", theData$FPD
  
  #)
  
  
  #leafletProxy("nycmap",theData) %>%
  # clearShapes() %>%
  # addPolygons(
  # fillColor = theData$FPD, 
  # fillOpacity = 1, 
  #color = "#BDBDC3", 
  #weight = 2,
  # popup = borough_popup)  
  
  #})
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
  
})


