library(leaflet)
library(shinydashboard)
library(shiny)
library(reshape2)
library("googleVis")
library("gridExtra")
library("ggplot2")
library(shinydashboard)
library(DT)
library(reshape2)
library(corrplot)
library(plyr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(maps)
library(flexdashboard)
library(leaflet)
library(googleVis)
library(plotly)

shinyUI(
  dashboardPage(
  dashboardHeader(title = "NYC Taxi App"),
  
  dashboardSidebar(

    #sliderInput("Month",label = "Choose Month:",min=1,max=6, value=1, animate=T),
    

    sidebarMenu(
      menuItem("Taxis Fare Borough Stats", tabName = "taxis_fare", icon = icon("dollar"), badgeColor='light-blue'),
      menuItem("Taxi Animation", tabName = "taxi_animation", icon = icon("taxi"), badgeColor='light-blue')
      
    ), # End of sidebarMenu 
    
    selectInput("selectDate", "Choose Animation Date", 
                c("2016-01-04 Mon","2016-01-05 Tues","2016-01-06 Wed",
                  "2016-01-07 Thur","2016-01-08 Fri","2016-01-09 Sat","2016-01-10 Sun")),
    
    #selectInput("selectSpeed", "Choose Animation Speed", 
    #            c("1X","2X","3X","4X")),
    
    sliderInput("time_range", 
                "Choose Time Range:", 
                min = as.POSIXct("2016-01-04 00:00:00"),
                max = as.POSIXct("2016-01-04 23:59:59"),
                value = c(as.POSIXct("2016-01-04 00:00:00")),
                timeFormat = "%F %T", ticks = F, animate = animationOptions(interval=450, loop = T),
                timezone = 'GMT', step = 60*2)
  ), # End of dashboardSidebar
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "taxis_fare",
              fluidRow(infoBoxOutput("maxbox"),
                       infoBoxOutput("medbox"),
                       infoBoxOutput("minbox")),
              
              fluidRow(splitLayout(cellWidths=c("50%","50%"),plotOutput("plot1"),plotOutput("plot2"))),
                
              fluidRow(column(10,sliderInput("Month",label = "Choose Month:",min=1,max=12, value=1, animate=T)))
              
              ), # End of taxis_fare tab

      
      tabItem(tabName = "taxi_animation",
              
              fluidRow(infoBoxOutput("taxibox"),
                       infoBoxOutput("meanSpeedbox"),
                       infoBoxOutput("meanDurationbox")), # fluidRow1

              box(
                title = "On-Trip NYC Yellow Taxi",
                collapsible = TRUE,
                width = "100%",
                height = "100%",
                leafletOutput("map")
              ) # end of box
              
      )# end of Taxi_animation tab 
      
      
      
      
    )# end of tab items
      
      
    
    # other tabItems to be added
    
  )# end of dashboardBody
)# end of dashboardPage




)# end of Shiny UI
  
