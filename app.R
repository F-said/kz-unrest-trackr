library(shiny)
library(htmltools)

# Data struct packages
library(broom)
library(collections, warn.conflicts = FALSE)
library(sf)
library(raster)
library(dplyr)

# Mapping packages
library(ggplot2)
library(ggmap)
library(geojsonio)
library(tmap) 
library(leaflet)
library(geojsonR)

# time slider
library(leaflet.extras2)

# init global vars and funcs
source("visualize.R")

# Create the UI to display data
ui <- fluidPage(
  # theme = bslib::bs_theme(bootswatch="lumen"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  uiOutput("warning"),
  sidebarLayout(
    position = "right",
    sidebarPanel(style = "overflow-y:scroll; height: 100vh;",
      tabsetPanel(selected = "Legend",
        tabPanel("Legend",
            uiOutput("eventLegend"),
            uiOutput("interLegend"),
            h4("Extracting Information"),
            p("Videos and pictures are represented in 12-hour intervals, dates 
            have been determined through timestamps or descriptions, while 
            cooridinates are determined from descriptions or geographic 
              details."),
            p("The location of events in Kazakhstan are fuzzy estimates 
              that are only reliable to the city level, while international 
              events are only reliable to the nation level.")
        )
        # tabPanel("Social Media"), TODO
        )
    ),
    mainPanel(style = "overflow-y:scroll; height: 100vh;",
      # Render interactive map of Central Asia
      leafletOutput("map", width="104%", height = "100vh"),
    
      # Render absolute slider
      absolutePanel(top=10, left=100,
                    sliderInput("DayHour", "Date & Hour (UTC)",width=400,
                                min=as.POSIXct("2022-01-02 20:00","%Y-%m-%d %H:%M", tz="UTC"),
                                max=as.POSIXct("2022-01-07 8:00","%Y-%m-%d %H:%M", tz="UTC"), 
                                value=as.POSIXct("2022-01-02 20:00","%Y-%m-%d %H:%M", tz="UTC"),
                                timeFormat="%Y-%m-%d %H:%M", timezone="UTC", step=hour_interv*sec_in_hour,
                                animate=TRUE)
      ),
      
      # Render absolute checkbox
      absolutePanel(bottom=50, right=-100, class="well-select",
                    checkboxGroupInput("markerSelect", 
                                       "", 
                                       choices = list("Non-Violent Protests"=levels(factor("NonViolentProtests")), 
                                                      "Violent Protests"=levels(factor("ViolentProtests")), 
                                                      "Movement of Security Forces"=levels(factor("MovementofSecurityForces")),
                                                      "Violent Supression"=levels(factor("ViolentSupression")),
                                                      "Foreign Intervention"=levels(factor("ForeignIntervention"))),
                                        selected=c(levels(factor("NonViolentProtests")),
                                                   levels(factor("ViolentProtests")),
                                                   levels(factor("MovementofSecurityForces")),
                                                   levels(factor("ViolentSupression")),
                                                   levels(factor("ForeignIntervention"))))
      )
    )
  )
)

# Define the server to update data
server <- function(input, output, session) {
  # Modal warning
  output$warning <- renderUI({
    showModal(modalDialog(
      title = "Please be advised!",
      easyClose = TRUE,
      p("The events marked as:", span(class="dot", style="background-color:#FEC50C80"),tags$b('Violent Protests'),
      "and", span(class="dot", style="background-color:#FF000080"),tags$b('Violent Suppression'),
      "may contain scenes of injury, and possibly death."),
      p("These videos were included to capture the violence that underwent
      during this time of turmoil in Kazakhstan."),
      p("If you do not wish to view this content, please refrain from clicking 
        on the above mentioned markers on the map, or exit the app.")
    ))
  })

  # filter event_sf according to greater than hour_lim hours before event and 
  # strictly before or equal to event
  sliderData <- reactive({
    event_sf[event_sf$datetime_UTC >= input$DayHour-(hour_interv*sec_in_hour) &
             event_sf$datetime_UTC <= input$DayHour & event_sf$label %in% input$markerSelect, ]
    
  })
  
  output$map <- renderLeaflet({
    protest_map()
  })
  
  observeEvent({input$DayHour
    input$markerSelect}, {
    # Display markers according to date
    leafletProxy("map", session, data=(sliderData() %>% filter(!st_is_empty(.)))) %>%
      clearMarkers() %>%
      clearPopups() %>%
      clearMarkerClusters() %>%
      addCircleMarkers(
        group=~label,
        color=~pall(label),
        popup=~popup_vid(location, description, datetime_UTC, file, type),
        stroke=FALSE,
        fillOpacity=0.6,
        clusterOptions=markerClusterOptions(
          maxClusterRadius=jitter_fact,
          iconCreateFunction=JS(cluster_script))
      )
  
    # Display internet status according to date
    if(input$DayHour >= INTERNET_BLACKOUT & !color_change) {
      leafletProxy("map") %>%
        hideGroup(group=kz_on_id) %>%
        showGroup(group=kz_off_id)
      color_change <<- TRUE
    } else if (input$DayHour < INTERNET_BLACKOUT & color_change){
      leafletProxy("map") %>% 
        hideGroup(group=kz_off_id) %>%
        showGroup(group=kz_on_id)
      color_change <<- FALSE
    }

  })
  
  output$eventLegend <- renderUI({
    legend_map()
  })
  
  output$interLegend <- renderUI({
    internet_legend()
  })
}


# Start the app
shinyApp(ui = ui, server = server)
