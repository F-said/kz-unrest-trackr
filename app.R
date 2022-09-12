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
      tabsetPanel(selected = "About",
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
        ),
        # tabPanel("Social Media"), TODO
        tabPanel("About",
            h3("The 2022 Kazakh Protests"),
            p("On January 2nd citizens and workers from the city of Zhanaozen began 
            demonstrating to demand the lowering of prices following a 
              100% price hike on fuel."),
            p("This wasn't the first time that demonstrations began in Zhanaozen.
              In 2011, oil workers began protesting for better working conditions and
              equitable pay. In response, authorites ",
              tags$a(href="https://en.wikipedia.org/wiki/Zhanaozen_massacre","shot and killed at least 14 protesters.")
              ),
            p("The government of Kazakhstan has long operated through violence and 
            nepotism, and has retained this status-quo through its president who
            has retained his power since the fall of the Soviet Union 30 years ago. 
            Even though the president has nominally stepped down from his position 
            in 2019, he has maintained control over the country through a hand-picked successor."), 
            p("Kazakhstan's ",
              tags$a(href="https://freedomhouse.org/country/kazakhstan/freedom-world/2021", 
                                  "freedom status"),
            ", a metric which measures political rights and liberties, is 
            23 out of 100. For reference, Russia scores 20 out of 100.  
            "),
            p("For these reasons, demonstrations of Zhanaozen evolved into protests 
            all over the country. Protesters demanded the selection of government 
            officials appointed by fair election, the full resignation of the government, 
              and affordable living conditions."),
            p("Protests however eventually devolved into shootings, maruading, and general destruction.
              Once the ", 
              tags$a(href="https://twitter.com/netblocks/status/1478694849440358400?ref_src=twsrc%5Etfw%7Ctwcamp%5Etweetembed%7Ctwterm%5E1478694849440358400%7Ctwgr%5E%7Ctwcon%5Es1_&ref_url=https%3A%2F%2Fnetblocks.org%2Freports%2Finternet-disrupted-in-kazakhstan-amid-energy-price-protests-oy9YQgy3", "the internet was shut down"),
              "the demonstrations in the city of Almaty gave way to even more extreme violence."
            ),
            p("The current president of Kazakhstan in response ordered the crackdown
              of protests through ", tags$a(href="https://eurasianet.org/kazakhstan-shoot-to-kill-protesters-orders-tough-talking-tokayev", "indiscrimant shooting"),
              " and the introduction of ", tags$a(href="https://eurasianet.org/csto-agrees-to-intervene-in-kazakhstan-unrest", "foreign security forces.")),
            p("Following the conclusion of these events, the government has warned 
            its citizens of posting ", tags$a(href="https://www.akorda.kz/en/president-kassym-jomart-tokayevs-address-to-the-people-of-kazakhstan-801221",
                                              "'inflammatory content'")," on the web, 
            and has touted the idea that 'terrorists' participated in these protests.
            Their proof were harshly beaten",
              tags$a(href="https://www.rferl.org/a/kazakhstan-kyrgyz-musician-severely-beaten/31668455.html","touring musicians who confessed under 
              duress.")),
            p("Other sources however claim that this movement ", 
            tags$a(href="https://politicalviolenceataglance.org/2022/01/14/the-hidden-role-of-organized-crime-in-kazakhstans-unrest/", 
            " became hijacked by interclan feuds"), " for power. While others
            claim that this was an ", tags$a(href="https://thecradle.co/Article/news/5668" ,
            "attempt at a western-orchestrated revolution.")),
            h3("Video Timeline"),
            p("The shutdown of the internet disrupted the free-flow of information,
             while untrustworthy narratives flowed freely. However, a visible 
              timeline of events emerges when considering videos & pictures posted
              on TikTok, Reddit, Twitter, and Telegram."),
            p("The data available starts to hint that demonstrations began peacefully. 
            This however turned into scenes of gunfire and looting once reaching Almaty, 
              a city known for ",
              tags$a(href="https://jamestown.org/program/a-steppe-too-far-kazakhstans-gangland-power/",
                     "state-embedded organized crime.")),
            p("This hints that the turmoil that occured most notoriously
              in Almaty was not the result of maruading protesters, embedded revolutionaries, 
              or terrorists, but perhaps interclan rivalries that sensed a shift in power
              once anti-government protests spread across the country."),
           ))
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
