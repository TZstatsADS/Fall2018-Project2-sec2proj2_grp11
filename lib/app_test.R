ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput("delete1", "Delete ID=1 and 2",value=FALSE),
      checkboxInput("delete3", "Delete ID=3",value=FALSE)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
))

df <- data.frame(id=c(1,2,3),lng = rnorm(3, -106.1039361, 0.5) ,
                 lat = rnorm(3, 50.543981, 0.5))

server <- shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet(
    leaflet() %>% 
      addTiles() %>% addCircleMarkers(layerId=as.character(df$id),df$lng,df$lat, group='marker', radius=2, fill = TRUE,color='red') 
    
    
  )
  
  observeEvent(input$delete1, {
    proxy <- leafletProxy('map')
    if (input$delete1){ proxy %>% removeMarker(c("1","2"))
    }
  })
  
  observeEvent(input$delete3, {
    proxy <- leafletProxy('map')
    if (input$delete3){ proxy %>% removeMarker("3")}
  })
  
})

shinyApp(ui, server)