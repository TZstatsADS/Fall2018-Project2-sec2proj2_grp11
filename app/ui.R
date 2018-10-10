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
shinyUI(dashboardPage(
  dashboardHeader(title = "NYC taxi App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Taxis Fare Statistics", tabName = "taxis_fare", icon = icon("dollar"), badgeColor='light-blue')
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "taxis_fare",
              fluidRow(infoBoxOutput("maxbox"),
                       infoBoxOutput("medbox"),
                       infoBoxOutput("minbox")),
              fluidRow(splitLayout(cellWidths=c("50%","50%"),plotOutput("plot1"),plotOutput("plot2"))),
              fluidRow(column(10,
                              sliderInput("Month",
                                          label = "Choose Month:",min=1,max=12, value=1, animate=T)
              )
              
      )
              
              )
    )
  )
))
  
