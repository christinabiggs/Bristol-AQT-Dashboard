# Bristol Air Quality and Traffic Dashboard
# Shiny Map with Popup graphs, date choice and download facility
# Created by Christina Biggs
# With funding from the Jean Golding Institute, University of Bristol
# Last edited 29/03/2024 and uploaded to shiny.io

source('Librarysetup.R')
source('FetchFromTelraam.R')
source('FetchDEFRA.R')
source('FetchLuftdatenData.R')
source('FetchVivaCityData.R')


# Based on input coordinates 
ui = fluidPage(
  
  # App title ----
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
  
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      titlePanel("Bristol Air Quality and Traffic (AQT) Dashboard"),
  
      dateRangeInput('dateRange',
                   label = 'Select date range for data download: for one day\'s data 
                   put the same date in both boxes',
                   format = "dd-mm-yyyy",
                   start = Sys.Date() - 2, end = Sys.Date() - 1,max = Sys.Date()-1
      ),

      # Input: Choose dataset ----
      selectInput("dataset", "Choose a sensor type for data download:",
                  c("DEFRA sensor list","DEFRA data download",
                  "SensorCommunity sensor list","SensorCommunity data download",
                  "Telraam sensor list","Telraam data download",
                  "UoBristol sensor list","UoBristol data download")),

    # Button for data download
      downloadButton("downloadData", "Download"),
    
    # Checkbox for showing the dormant sensors on the map
      checkboxInput("showdormantsensors", "Show dormant sensors", TRUE),
    
    card(
      card_image(file="IconKey.png"),
      card_footer(class="text-info","This dashboard was created by Dr Christina Biggs with
                  funding from the Jean Golding Institute, Bristol.
                  For further information contact Dr James Matthews, Dr Nikolai Bode or Dr Anwar Khan,
                  University of Bristol")
      )
   
     
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
  # Where leaflet map will be rendered
    fluidRow(
    leafletOutput("map", height = 400)
    ),
    
    fluidRow( 
    tableOutput("table")
    )
    )
  )
)


server <- function(input, output, session) {
  
  p_luft <- as.list(NULL)
  print("Fetching Luftdaten graphs for yesterday")
  p_luft <- lapply(1:nrow(luft_sensorlist), function(i) {
    get_id <- luft_sensorlist$sensor_id[i]
    p_luft[[i]] <- PickGraphFromLuftdaten(luft_yesterday,DEFRA_yesterday,get_id)
  })
  
  p_vivacity <- as.list(NULL)
  print("Fetching Vivacity graphs for yesterday")
  p_vivacity <- lapply(1:nrow(vivacity_sensorlist), function(i) {
    get_id <- vivacity_sensorlist$sensor_id[i]
    p_vivacity[[i]] <- PickGraphFromVivaCity(vivacity_yesterday,get_id)
  })
 
  p_DEFRA <- as.list(NULL)
  print("Fetching DEFRA graphs for yesterday")
  p_DEFRA <- lapply(1:nrow(DEFRA_sensorlist), function(i){
    get_id <- DEFRA_sensorlist$code[i]
    p_DEFRA[[i]] <- PickGraphFromDEFRA(DEFRA_yesterday,get_id)
  })
  
  p_telraam <- as.list(NULL)
  print("Fetching Telraam graphs for yesterday")
  p_telraam <- lapply(1:nrow(telraam_sf), function(i) {    
    get_id <- telraam_sf$sensor_id[i]
    p_telraam[[i]] <- PickGraphFromTelraam(telraam_yesterday,get_id)
  })
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMapPane("SensorComm",zIndex = 410)  %>%
      addMapPane("telraam",zIndex = 412) %>%
      addMapPane("vivacity",zIndex = 414) %>%
      addMapPane("DEFRA",zIndex = 416) %>%
      addAwesomeMarkers(
        data = luft_sensorlist,
        icon = awesomeIcons(icon="cloud", library="fa", iconColor = "white",
              markerColor = ifelse(luft_sensorlist$yday_pm2.5>0|luft_sensorlist$yday_pm10>0,
              "blue","grey"),text=signif(luft_sensorlist$yday_pm2.5,digits=2)),
        lng = luft_sensorlist$lon,
        lat = luft_sensorlist$lat,
        options = pathOptions(pane = "SensorComm"),
        popup = popupGraph(p_luft, type = "svg"))  %>%
       addPolygons(
        data = telraam_sf, 
        fillColor = "pink",
        color = ifelse(telraam_sf$car>0,"pink","darkred"), 
        options = pathOptions(pane = "telraam"),
        popup = popupGraph(p_telraam, type = "svg")) %>%
       addAwesomeMarkers(
        data = telraam_sensorlist,
        lat = telraam_sensorlist$lat,
        lng = telraam_sensorlist$long,        
        icon = awesomeIcons(
          icon = "car", library="fa",iconColor = "white",
          markerColor = ifelse(!is.na(telraam_sensorlist$yday_cars),
                "pink","lightgray"),squareMarker = TRUE,
             text=as.integer(telraam_sensorlist$yday_cars)),
        options = markerOptions(opacity=ifelse(!input$showdormantsensors&
                  is.na(telraam_sensorlist$yday_cars),0,1)),
        popup = popupGraph(p_telraam, type = "svg")) %>% 
      addAwesomeMarkers(
        data = vivacity_sensorlist,
        lng = vivacity_sensorlist$lon,
        lat = vivacity_sensorlist$lat,
        icon = awesomeIcons(
                icon="car", library="fa", iconColor = "white",
                markerColor = ifelse(!is.na(vivacity_sensorlist$yday_cars),
                  "lightgreen","lightgray"),squareMarker = TRUE,
                text=as.integer(vivacity_sensorlist$yday_cars)),
        options = markerOptions(opacity=ifelse(!input$showdormantsensors&
                   is.na(vivacity_sensorlist$yday_cars),0,1)),
       popup = popupGraph(p_vivacity, type = "svg"))  %>%
      addAwesomeMarkers(
        data = DEFRA_sensorlist,
        lng = DEFRA_sensorlist$longitude,
        lat = DEFRA_sensorlist$latitude,
        icon = awesomeIcons(icon = "cloud", library="fa", iconColor = "white",
              markerColor = ifelse(DEFRA_sensorlist$yday_pm2.5>0.0,
            "orange","lightgray"),text=signif(DEFRA_sensorlist$yday_pm2.5,digits=2)),
        options = pathOptions(pane = "DEFRA"),
        popup = popupGraph(p_DEFRA, type = "svg"))
  })
  
  # Reactive value for selected dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "DEFRA sensor list" = DEFRA_sensorlist,
           "DEFRA data download" = FetchCountsFromDEFRA(input$dateRange[[1]],
                                                      input$dateRange[[2]]),
           "SensorCommunity sensor list" = luft_sensorlist,
           "SensorCommunity data download" = 
             FetchCountsFromLuftdaten(input$dateRange[[1]],input$dateRange[[2]]),
           "Telraam sensor list" = telraam_sensorlist,
           "Telraam data download" = FetchCountsFromTelraam(input$dateRange[[1]],
                                                          input$dateRange[[2]]),
           "UoBristol sensor list" = vivacity_sensorlist,
           "UoBristol data download" = FetchCountsFromVivaCity(input$dateRange[[1]],
                                                             input$dateRange[[2]]))
  })
  
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )  
} 
  
  # Create Shiny app ----
shinyApp(ui = ui, server = server)