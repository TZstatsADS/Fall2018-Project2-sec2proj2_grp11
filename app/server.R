#install.packages("lubridate")
library(ggmap)
library(maptools)
library(qmap)
library(plotly)
library(leaflet)
library(dplyr)
library(shiny)
library(reshape2)
library(shinydashboard)
library(plyr)
library(ggplot2)
library(flexdashboard)
library(rgdal)
library(lubridate)
library(geojsonio)

##########Load all the RData file 
#shapefile <- readShapePoly('MyApp/data/nynta_shapefile/nynta.shp')
setwd("../")
load('output/meanFarePerDistance.RData')
load('output/meanTipsPerDistance.RData')
load('output/MeanTipPerDistanceBorough.RData')
load('output/MeanFarePerDistanceBorough.RData')
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
      nyc_fare_borough <- data.frame(FPD=FPD_final[,6],TPD=TPD_final[,6],boro_name=c("Manhattan","Brooklyn","Queens","Bronx","Staten Island")) 
    }
   joinedDataset <- boroughs
   joinedDataset@data <- suppressWarnings(left_join(joinedDataset@data, nyc_fare_borough, by="boro_name"))
   
   #boroughs<-SpatialPolygonsDataFrame(boroughs, data=boroughs1)
   return(joinedDataset)
  })
 
 ############Can't find the bug.........
 #output$nycmap<-renderLeaflet({
   #leaflet() %>%
    # addTiles() %>%
     
     # Centre the map in the middle of our co-ordinates
     #setView(mean(bounds[1,]),
            # mean(bounds[2,]),
             #zoom=5 # set to 10 as 9 is a bit too zoomed out
     #)       
   
 #})
   
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
 
 
output$maxbox <- renderInfoBox(
    {
      DF <- fare_data_borough()
      outData <- DF$FPD
      max_value <- max(outData,na.rm = T)
      max_neigh <- DF$boro_name[outData==max_value]
      infoBox(max_neigh,round(max_value,2),icon = icon("chevron-up"),color='yellow')
    }
  )
  output$minbox <- renderInfoBox(
    {
      DF <- fare_data_borough()
      outData <- DF$FPD
      min_value <- min(outData,na.rm = T)
      min_neigh <- DF$boro_name[outData==min_value]
      infoBox(min_neigh,round(min_value,2),icon = icon("chevron-down"),color='blue')
    }
  )
  output$medbox <- renderInfoBox(
    {
      DF <- fare_data_borough()
      outData <- DF$FPD
      mean_value <- mean(outData,na.rm = T)
      mean_neigh <- DF$boro_name[outData==mean_value]
      text <- as.character(month(ymd(010101) + months(input$Month-1),label=TRUE,abbr=FALSE))
      infoBox(paste("Mean Fare for", text),round(mean_value,2),icon = icon("calculator"),color='red')
    }
  )
  
})


