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

navbarPage("NYC TAXI", id="nav", 
          
           # new module begins
           
           tabPanel("Taxi Around",
                    
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
