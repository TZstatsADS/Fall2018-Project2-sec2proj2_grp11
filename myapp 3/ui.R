library(leaflet)



navbarPage("NYC TAXI", id="nav",  #title = 'taxi menu'
           
           # start of 1st tabPanel
           tabPanel("Interactive Map",
                    
                    div(class="outer",
            
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")),
                        
                        leafletOutput("map", width="100%", height="100%"),
 
                        
                        absolutePanel(id = "user_interactive", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = 30, right = "auto", bottom = "auto",
                                      width = 320, height = "auto",
                                      
                                      h2(" Estimate Your Taxi Fare"),
                                      
                  
                                      checkboxInput(inputId = "enterCurrentLoc",label = strong("Enter Current Address"),value = FALSE),
                                      conditionalPanel(condition = "input.enterCurrentLoc == false",                                       
                                                       selectInput("current", "From", c('Choose Your location',"Soho", "JFK",'Empire State Building',
                                                                                        "Columbia University", "MoMA New York",'Wall Street New York'))),
                                      conditionalPanel(condition = "input.enterCurrentLoc == true",
                                                       textInput(inputId = "current_enter", label = 'From')),
                                      
                                      checkboxInput(inputId = "enterDestination",label = strong("Enter Destination"),value = FALSE),
                                      
                                      conditionalPanel(condition = "input.enterDestination == false",                                       
                                                       selectInput("destination", "To", c('Choose Your Destination','Click From Map', "Soho", "JFK",
                                                                                          'Empire State Building',"Columbia University", "MoMA New York",
                                                                                          'Wall Street New York'))),
                                      
                                      conditionalPanel(condition = "input.enterDestination == true",
                                                       textInput(inputId = "enterDestination", label = 'To')),
                                      
                                      
                                      selectInput("choose current date", "Select Current Date", c("2016-01-04","2016-01-05","2016-01-06",
                                                                                                 "2016-01-07","2016-01-08","2016-01-09","2016-01-10")),
                                      textInput("choose current time", "Enter Current Time (H:M:S)", "01:10:00"),
                                      actionButton("searchButton", "Search",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      
                                      h6(textOutput("estimated_fare")),
                                      h6(textOutput("estimated_tip")),
                                      h6(textOutput("total_amount")),
                                      h6(textOutput("estimated_distance")),
                                      h6(textOutput("estimated_duration")),
                                      h6(textOutput("regular_duration"))),
                        
                        absolutePanel(id = "user_interactive", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE,top = 60, left = "auto", right = 0, bottom = "auto",
                                      width = 320, height = "auto",
                                      plotOutput('plotDrivingSpeed', height=200)
                                      )
                        
                        
        
                                                            
                    ) # end of div
            ) # End of 1st tabPanel
           
           # start of 2nd tabPanel
           

) # End of narbarPage
           
                  

                                 


