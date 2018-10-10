packages.used=c("mapsapi", "xml2", "leaflet", "shiny",'data.table',"fasttime",'ggmap')

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(mapsapi)
library(xml2)
library(leaflet)
library(shiny)
library(data.table)
library(fasttime)
library(ggmap)
library(dplyr)
source('functions.R')

data = combine_data()  # data for one week


# Map Defaut Set up
destination_icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = 'red')


############## Shiny page design ##################
shinyServer(function(input, output, session) {
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat=40.75241, lng=-73.99961, zoom=12) #%>%
      #addProviderTiles('CartoDB.Positron')
      # check provider choices: https://leaflet-extras.github.io/leaflet-providers/preview/
  })  # End of output$map


  
  RV <- reactiveValues(origin=0, destination=0, min_route = 0)
  
  observe({
    if(input$enterCurrentLoc){
      RV[['origin']] <- geocode(input$current_enter, source = "dsk")
      leafletProxy("map") %>% 
        clearShapes() %>% 
        clearMarkers()  %>% 
        addMarkers(RV[['origin']]$lon , RV[['origin']]$lat) 
      
    }else{
      
      if(input$current != "Choose Your location") {
        RV[['origin']] <- geocode(input$current, source = "dsk")
        leafletProxy("map") %>% 
          clearShapes() %>% 
          clearMarkers()  %>% 
          addMarkers(RV[['origin']]$lon , RV[['origin']]$lat) 
      }else{
        leafletProxy("map") %>% 
          clearShapes() %>% 
          clearMarkers()  
        
      }
    }

    if ((input$destination != "Choose Your Destination") & (input$destination != "Click From Map")  ){
      
      RV[['destination']] <- geocode(input$destination, source = "dsk")
      
      leafletProxy("map") %>% 
        addAwesomeMarkers(RV[['destination']]$lon , RV[['destination']]$lat,icon =destination_icons) 
    }
  })
  
  
  observeEvent(input$map_click,{
    if(input$enterCurrentLoc){
      RV[['origin']] <- geocode(input$current_enter, source = "dsk")} 
    
    else{
      RV[['origin']] <- geocode(input$current, source = "dsk")
      }
    
    if (input$destination == "Click From Map"){
      RV[['destination']]  <- unlist(input$map_click)[1:2]
      RV[['destination']]  <- data.frame('lon' = as.numeric(RV[['destination']]['lng']),
                                        'lat' = as.numeric(RV[['destination']]['lat']))
      
      #print(count(input$map_click_count))
      leafletProxy("map") %>% 
        addAwesomeMarkers(RV[['destination']]$lon , RV[['destination']]$lat,icon =destination_icons)
    }
  })


  observeEvent(input$searchButton,{

    RV[['min_route']] <- get_route(as.vector(t(RV[['origin']])), as.vector(t(as.vector(t(RV[['destination']])))))
    est_distance = as.numeric(gsub(" mi", "", RV[['min_route']]$distance_text))
    regular_duration = as.numeric(gsub(" mins", "", RV[['min_route']]$duration_text))  # duration in mins
    currentDate_time <- fastPOSIXct(paste(input$`choose current date`, input$`choose current time`, sep=" "))  
    
    # Past 5 Mins historical stats info
    estFareVars = est_amount_past5mins(data, currentDate_time)
    current_mean_speed = estFareVars[1]   # speed : miles per mins
    tip_percent = estFareVars[2]
    other_fixed_charge = estFareVars[3]
    
    standard_fare_charge = 2.5 + 0.5*est_distance*5 
    
    est_duration = est_distance/current_mean_speed
    
    if (est_duration > regular_duration){ # case when it is in traffic
      standard_fare_charge <- standard_fare_charge + 0.5 *(est_duration - regular_duration)
    }
    
    totalEstFare <- standard_fare_charge + other_fixed_charge


    leafletProxy("map") %>% 
      clearShapes() %>% 
      clearMarkers()  %>% 
      addMarkers(RV[['origin']]$lon , RV[['origin']]$lat) %>%
      addAwesomeMarkers(RV[['destination']]$lon , RV[['destination']]$lat,icon =destination_icons) %>%
      addPolylines(data = RV[['min_route']], opacity = 1, weight = 7, color = '#339999')
    
    output$plotDrivingSpeed <- renderPlot({
      drawDrivingSpeed(data,currentDate_time)
    })
    
    output$estimated_fare <- renderText( { 
      paste(paste("Your Estimated Fare Before Tip is: ", round(totalEstFare,2)), 'dollars.')
    })
    
    output$estimated_tip <- renderText( { 
      paste(paste("Your Estimated Tip is: ", round(tip_percent*100,2)), '%.')
    })
    
    output$total_amount <- renderText( { 
      paste(paste("Your Total Suggested Amount is: ", round((1+tip_percent)*totalEstFare,2)), 'dollars.')
    })
    
    output$estimated_distance <- renderText({
      paste(paste("Your Estimated Distance is: ", round(est_distance,2)), 'miles.')
      #}
    })
    
    output$estimated_duration <- renderText({
      paste(paste("You Estimated Duration is: ", round(est_duration)), 'mins.')
    })
    
    output$regular_duration <- renderText({
      paste(paste("You Regular Duration is: ", round(regular_duration)), 'mins.')
    })

  }) # End of  observe
  

  

}) # End of shinyServer





