packages.used=c("rgeos", "sp", "rgdal", 
                "leaflet", "htmlwidgets", "shiny",
                "ggplot2", "dplyr", "data.table","DT", "lubridate")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(lubridate)
library(leaflet)
library(htmlwidgets)
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)


setwd("~/Documents/Github/ADS/Fall2018-Project2-sec2_proj2_grp11/") ############

test01<-fread("test01.csv", header = T, stringsAsFactors = F)

count <- NULL

shinyServer(function(input, output,session) { 
  
    output$map3 <- renderLeaflet({
      leaflet() %>%
        setView(lat=40.806549, lng=-73.964513, zoom=13) %>%
        addTiles() 
    })
    
    # Find geolocalisation coordinates when user clicks
    observeEvent(input$geoloc, {
      js$geoloc()
    })
    
    # zoom on the corresponding area
    observe({
      ## localize myself
      if(!is.null(input$lat)){
        map3 <- leafletProxy("map3")
        dist <- 0.0002
        lat <- input$lat
        lng <- input$long
        map3 %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
      }
    })
    
    ##### popup function #######
    observe({
      ## popup event observer
      proxy <- leafletProxy("map3") %>% clearPopups()
      event <- input$map3_click
      proxy %>% removeMarker(as.character(count))
      
      if (is.null(event))
        return()
      isolate({
        showZipcodePopup(lat = round(as.numeric(event$lat), 4), lng =  round(as.numeric(event$lng), 4))
      })
    }) 
    
    showZipcodePopup <- function(lat, lng) {
      content = as.character(paste(lat, lng, sep = ","))
      #print(content)
      leafletProxy("map3") %>% removeMarker(as.character(count)) %>% addPopups(lat = lat, lng = lng, popup = content)
    }
    
    ######### search neignborhood part ######
    
    observeEvent(input$begin_searching, {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'Will Begin searching the taxis available in neighborhood')
      showTaxisPopup(input$time, input$map3_click)
      }
    )
    
    showTaxisPopup <- function(current_time, click_event){
      lat = round(as.numeric(click_event$lat), 4)
      lng = round(as.numeric(click_event$lng), 4)
      
      time = format(current_time, format = "%H:%M")
      
      # select the data points
      temp_taxi_data <- test01 %>% 
        subset(( (abs(pickup_longitude-lng) < 0.0001 
                  & abs(pickup_latitude - lat) < 0.0001)) | ((abs(dropoff_longitude-lng) < 0.0001 
                                                              & abs(dropoff_latitude - lat) < 0.0001 )) ) %>%
        subset( (abs(minute(hm(pickup_time) - hm(time))) <= 15)  | 
                  (abs(minute(hm(dropoff_time) - hm(time))) <= 15) )
      
      count <- nrow(temp_taxi_data)
      print(as.character(paste("count1", count, sep = ":")))
      
      # pop up generation
      if (count > 5) {
        
        content = as.character(paste(count, "taxi",sep = " "))  
        leafletProxy("map3") %>% addMarkers(layerId=as.character(1:count), lat = temp_taxi_data$pickup_latitude, 
                                            lng = temp_taxi_data$pickup_longitude, 
                                            popup = content) 
      } else {
        temp_taxi_data <- test01 %>% 
          subset(( (abs(pickup_longitude-lng) < 0.0007 
                    & abs(pickup_latitude - lat) < 0.0007)) | ((abs(dropoff_longitude-lng) < 0.0007 
                                                                & abs(dropoff_latitude - lat) < 0.0007 )) ) %>%
          subset( (abs(minute(hm(pickup_time) - hm(time))) <= 15)  | 
                    (abs(minute(hm(dropoff_time) - hm(time))) <= 15) )
        
        count <- nrow(temp_taxi_data)
        print(as.character(paste("count2", count, sep = ":")))
        
        content = as.character(paste(count, "taxi",sep = " ")) 
        leafletProxy("map3") %>% addCircleMarkers(layerId=as.character(1:count), lat = temp_taxi_data$pickup_latitude, 
                                                  lng = temp_taxi_data$pickup_longitude, radius = 3, fill = FALSE, 
                                                  popup = content) 
      }
      
      
    }

    

  
})


