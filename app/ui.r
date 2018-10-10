library(shiny)
library(leaflet)
library(shinyjs)
library(shinyTime)

# ==== fonction allowing geolocalisation
jsCode <- '
shinyjs.geoloc = function() {
navigator.geolocation.getCurrentPosition(onSuccess, onError);
function onError (err) {
Shiny.onInputChange("geolocation", false);
}
function onSuccess (position) {
setTimeout(function () {
var coords = position.coords;
console.log(coords.latitude + ", " + coords.longitude);
Shiny.onInputChange("geolocation", true);
Shiny.onInputChange("lat", coords.latitude);
Shiny.onInputChange("long", coords.longitude);
}, 5)
}
};
'


#Choices for drop-downs
vars <- c(
  "Business Day" = 1,
  "Not Business Day" = 2

)
#

navbarPage("NYC TAXI", id="nav", 
           #title = 'taxi menu',
           
           # tabPanel("Interactive Map",
           #          div(class="outer",
           #              
           #              tags$head(
           #                # Include our custom CSS
           #                includeCSS("styles.css"),
           #                includeScript("gomap.js")
           #              ),
           #              
           #              leafletOutput("map", width="100%", height="100%"),
           #              
           #              # Shiny versions prior to 0.11 should use class="modal" instead.
           #              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
           #                            draggable = F, top = 60, left = "auto", right = 0, bottom = "auto",
           #                            width = 160, height = 180,
           #                            
           #                            radioButtons("CF", label = "Layers",
           #                                         choices = list("Count Number" = "count", "Fare Per Distance" = "FPD","Cluster" = "cluster1" ,"Cash Paying Percentage" = "cash"), 
           #                                         selected = "count")
           #                            
           #                            
           #              ),
           #              
           #              
           #              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
           #                            draggable = TRUE, top = 60, left = 0, right = "auto", bottom = "auto",
           #                            width = 330, height = "auto",
           #                            
           #                            h3("Panel"),
           #                            
           # 
           #                            selectInput("days", "Days", c("All Day", "Business Day", "Not Business Day"),selected = "All Day"),
           # 
           #                            
           #                            checkboxInput(inputId = "showhr",
           #                                          label = strong("Show hours"),
           #                                          value = FALSE),
           #                            
           #                              conditionalPanel(condition = "input.showhr == false"
           #                    
           #                              ),
           #                            
           #                            
           #                            conditionalPanel(condition = "input.showhr == true",
           #                                             sliderInput(inputId = "hr_adjust",
           #                                                         label = "Choose the time of the day:",
           #                                                         min = 0, max = 23, value = NULL, step = 1)
           #                            ),
           #                            
           #                            
           #                            
           #                            
           #                            checkboxInput("top15count", "Top 5 Count", FALSE),
           #                            checkboxInput("top15FPD", "Top 5 FPD", FALSE),
           #                            
           #                            
           #                            checkboxInput(inputId = "showbr",
           #                                          label = strong("Show Borough for Top 5 counts/FPD"),
           #                                          value = FALSE),
           #                            
           #                            conditionalPanel(condition = "input.showbr == true",
           #                                             selectInput("boroSelect", "Borough for Top 5 counts/FPD", 
           #                                                         c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island", "All"), 
           #                                                         selected = "All")
           #                            ),
           #                            
           #                            
           #                          
           #                            
           #                            radioButtons("subway", label = h4("Subway Station : "),
           #                                         choices = list("Do not appear" = 1, "Show all stations" = 2, "Show unique station" = 3), 
           #                                         selected = 1),
           #                            
           #                            plotOutput("districttimeplot", height = 280),
           #                            helpText(   a("Analysis",
           #                                          href="https://github.com/TZstatsADS/Spr2017-proj2-grp2/blob/master/doc/analysis.html")
           #                            )
           #              )
           # 
           #              # absolutePanel(id="graphstuff",class = "panel panel-default", fixed=TRUE,
           #              #               draggable = TRUE, top=55, left="auto", right= 5, bottom="auto",width=300,
           #              #               height=100, style="opacity:0.65",
           #              #               
           #              #               
           #              #               h4("hourly flow change", align = "center"),
           #              #               plotOutput("districttimeplot",height = 200))
           #              
           #          )
           # ),
           # 
           # 
           # tabPanel("Dynamic Map",
           #          div(class="outer",
           #              
           #              tags$head(
           #                # Include our custom CSS
           #                includeCSS("styles.css"),
           #                includeScript("gomap.js")
           #              ),
           #              
           #              leafletOutput("map2", width="100%", height="100%"),
           #              
           #              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
           #                            draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
           #                            width = 330, height = "auto",
           #                            
           #                            h2("Dynamic map of NYC taxi hourly flow change"),
           #                            
           #                            
           #                            selectInput("pd", "pick up or drop off", c("Pick up", "Drop off", "All"), selected = "Pick up"),
           #                            
           #                            textInput("choose date", "Choose date", "1/1/2015"),
           #                            
           #                            sliderInput("hours", "Hours of Day:", 
           #                                        min = 0, max = 23, value = 0, step = 1,
           #                                        animate=animationOptions(interval = 500)),
           #                            helpText("Click play button to see dynamic flow data")
           #              )
           # 
           #              
           #          )
           # ),
           # 
           # 
           # tabPanel("Raw Data",
           #          hr(),
           #          DT::dataTableOutput("rawtable")
           # ),
           
           # new module begins
           
           tabPanel("New",
                    
                    # shinyapp
                    
                    # Use geolocalization
                    # Tell shiny we will use some Javascript
                    useShinyjs(),
                    extendShinyjs(text = jsCode),
                    # One button and one map3
                    br(),
                    leafletOutput("map3", height="600px"),
                    
                    # ref: https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 60, left = 20, right = "auto", bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h2("Taxi Helper: Help you to get taxi easier"),
                                  actionButton("geoloc", "Localize me", class="btn btn-primary", onClick="shinyjs.geoloc()"),
                                  timeInput("time", "Time:", value = Sys.time()),
                                  actionButton("begin_searching", "Searching neighbors"),
                                  helpText("Click Localize me button to localize myself"),
                                  helpText("Click Searching button to begin searching neighborhood taxis")
                    )
                    # datetime input & click, 1.5h
                    # Ref: https://leafletjs.com/examples/quick-start/
                    
                    # get the lati and log of clicked point(server) 
                    # pass the geo info to a variable (server)
                    
                    
                    # give a popped up for the geo info of the point you clicked 
                    # (popped out in ui, poped up text in server)
                    # 0.5h
                    
                    # 1h
                    # filter whole dataset < X00m
                    # Ref: https://www.movable-type.co.uk/scripts/latlong.html
                    
                    ## calc < 10m count, if > 10 stay
                    ## popped out, will be more than 10 cars within XXX minutes
                    
                    #########################################################
                    
                    ## direction calculation
                    ## table(1,2,3,4)->largest
                    
                    # scatterplot the cars (sampled)
                    
                    # poped up an popped up for direction, arrow image to be downloaded
                    
           )
           
           
           
)
