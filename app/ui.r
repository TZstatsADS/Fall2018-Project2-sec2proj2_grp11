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

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Yellow Taxi NYC"),
    
    dashboardSidebar(
      
      sidebarMenu(id="menu1",
                  menuItem("Hail a Taxi", tabName = "maptab", icon = icon("map-marker", lib = "glyphicon"), badgeColor='light-blue'),
                  menuItem("Taxis Fare Borough Stats", tabName = "taxis_fare", icon = icon("dollar"), badgeColor='light-blue'),
                  menuItem("Taxi Animation", tabName = "taxi_animation", icon = icon("taxi"), badgeColor='light-blue')
                  
      ), # End of sidebarMenu 
      
      conditionalPanel(
        condition = "input.menu1 == 'taxi_animation'",
        selectInput("selectDate", "Choose Animation Date", 
                    c("2016-01-04 Mon","2016-01-05 Tues","2016-01-06 Wed",
                      "2016-01-07 Thur","2016-01-08 Fri","2016-01-09 Sat","2016-01-10 Sun")),
        
        sliderInput("time_range", 
                    "Choose Time Range:", 
                    min = as.POSIXct("2016-01-04 00:00:00"),
                    max = as.POSIXct("2016-01-04 23:59:59"),
                    value = c(as.POSIXct("2016-01-04 00:00:00")),
                    timeFormat = "%F %T", ticks = F, animate = animationOptions(interval=450, loop = T),
                    timezone = 'GMT', step = 60*2))
      
    ), # End of dashboardSidebar
    
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "maptab",
                div(class = "outer",

                    tags$head(
                      # include our custom CSS
                      includeCSS("styles.css"),
                      includeScript("gomap.js")
                    ),
                
                    leafletOutput(outputId = "map0", width="100%", height="100%"),

                    #a long panel put on the right side
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = FALSE, top = "auto", left = "auto", right = 0, bottom = 0,
                                  width = 330, height = "auto",

                                  selectInput(inputId = "day", label = " Today is",
                                              choices = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                          "Friday", "Saturday", "Sunday"),
                                              selected = "Monday"),

                                  #input origin
                                  textInput(inputId = "start",label = "From: ", value = ""),
                                  #input destination
                                  textInput(inputId = "end",label = "To: ", value = ""),

                                  div("Default Location:", style = "color:blue"),
                                  div("IAB, Columbia University", style = "color:blue"),
                                  br(),

                                  #input past time period to show drop-off locations
                                  numericInput(inputId = "nextmin",
                                               label = "Show drop-off locations in the next minute(s)",
                                               value = 3, min = 1, max = 5),
                                  div("Larger yellow icons indicate sooner arrival.", style = "color:blue"),
                                  br(),

                                  #input radius of the shpere to show drop-off locations
                                  sliderInput(inputId = "radius",
                                              label = "Show drop-off locations with radius",
                                              min = 0.1, max = 0.5, value = 0.2, step = 0.1, round = -1),

                                  #input of unit
                                  selectInput(inputId = "units", label = "Choose a Unit", choices = c("km","mi"), selected = "mi"),
                                  #indicate whether or not to show past routes of the taxis
                                  checkboxInput(inputId = "showroutes", label = "Show past routes", value = FALSE),
                                  #indicate whether or not to show the walking routes to the drop-off locations
                                  checkboxInput(inputId = "showwalk", label = "Show walking routes", value = FALSE)
                                  # submitButton(text = "Search")
                    ),


                    absolutePanel(id = "user_interactive", class = "panel panel-default", fixed = TRUE,
                                  draggable = FALSE,top = "auto", left = 0, right = "auto", bottom = 0,
                                  width = 320, height = "auto",

                                  # h3("Current Time",
                                  #    textOutput(outputId = "currentTime")
                                  # ),

                                  strong(
                                    #output google estimated distance
                                    textOutput(outputId = "ggdis"),
                                    #output google estimated duration
                                    textOutput(outputId = "ggD"),
                                    #output google estimated duration for public transit
                                    textOutput(outputId = "ggDt"),
                                    #output fare estimation
                                    textOutput(outputId = "fares")
                                    # textOutput(outputId = "rtloc")
                                  ),

                                  textOutput(outputId = "notes"),
                                  # p("(Estimated from last 30 mins fare rate)"),

                                  plotOutput('plotDrivingSpeed', height=200)
                    )
                )),
        
        tabItem(tabName = "taxis_fare",
                fluidRow(infoBoxOutput("maxbox"),
                         infoBoxOutput("medbox"),
                         infoBoxOutput("minbox")),
                
                fluidRow(splitLayout(cellWidths=c("50%","50%"),plotOutput("plot1"),plotOutput("plot2"))),
                
                fluidRow(column(10,sliderInput("Month",label = "Choose Month:",min=1,max=6, value=1, animate=T)))
                
        ), # End of taxis_fare tab
        
        
        tabItem(tabName = "taxi_animation",
                
                fluidRow(infoBoxOutput("taxibox"),
                         infoBoxOutput("meanSpeedbox"),
                         infoBoxOutput("meanDurationbox")), # fluidRow1
                
                fluidRow(column(8,
                                box(
                                  title = "On-Trip NYC Yellow Taxi",
                                  collapsible = TRUE,
                                  width = "100%",
                                  height = "100%",
                                  leafletOutput("map",width = "100%", height = 550)) # end of box
                ), 
                
                column(4,plotOutput('histPlot1',height = "310px"),plotOutput('histPlot2',height = "300px")
                       
                )
                )# fluidRow2
                
        )# end of Taxi_animation tab

      )# end of tab items

      # other tabItems to be added
      
    )# end of dashboardBody
  )# end of dashboardPage
)