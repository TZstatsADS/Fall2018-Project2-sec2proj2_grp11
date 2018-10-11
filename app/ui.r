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
                                  
                                  h2("Head Where to Get a Taxi?"),
                                  actionButton("geoloc", "Localize me", class="btn btn-primary", onClick="shinyjs.geoloc()"),
                                  timeInput("time", "Time:", value = Sys.time()),
                                  actionButton("begin_searching", "Search Neighbor"),
                                  actionButton("delete1", "Clean",value=FALSE),
                                  helpText("Click Localize me button to localize yourself"),
                                  helpText("Click on the map to show where you are"),
                                  helpText("Click Search Neighbor button to begin searching the neighborhood taxi"),
                                  helpText("Click Clean button to clean the map")
                    )
                   
                    
           )
           
           
           
)
