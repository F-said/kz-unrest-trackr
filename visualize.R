# initialize country and event data
source("map_compose.R")
source("event_compose.R")


# Generate leaflet of map 
protest_map <- function() {
  # init tmap and add KZ map indicating internet is on
  central_asia <- tm_shape(KZ_map) + 
    tm_fill(col=inter_on_color, alpha = 0.2, group=kz_on_id, interactive=FALSE) +
    tm_borders(col="blue", alpha = 0.2) + tm_layout(outer.margins = 0) 
  
  # add hidden layer indicating internet is off
  central_asia <- central_asia + tm_shape(KZ_map) +
    tm_fill(col=inter_off_color, alpha = 0.2, group=kz_off_id, interactive=FALSE) +
    tm_borders(col="purple", alpha = 0.2)
  
  # iterate through all countries and draw maps
  for (key in geo_maps$keys()) {
    if (key == "Kazakhstan") {
      next  
    }
    path <- paste(maps_path,geo_maps$get(key)[[json_pos]],sep="")
    geo_map <- geojson_read(path, what="sp")
    central_asia <- central_asia + tm_shape(geo_map) + 
      tm_polygons(alpha = 0.2, group=group_id, interactive=FALSE) 
  }
  
  # output leaflet map with layers prepped for markers
  tmap_leaflet(central_asia, in.shiny=TRUE) %>% removeLayersControl() %>%
    hideGroup(group=kz_off_id)
}


# Generate legend of events
legend_map <- function() {
  # pull copy of legend
  legend_dat <- data.frame(legend_dat)

  lapply(1:dim(legend_dat)[[1]], function(i) {
    div(class="legend",
      fluidRow(
        column(2,
               span(class="dot", style=paste(
                 "background-color:",legend_dat$color[[i]],sep=""))
        ),
        column(10,
               p(tags$b(legend_dat$label[[i]]))
        )
      ),
      fluidRow(
        column(12,
               p(legend_dat$description[[i]])
        )
      )
    )
  })
}


# Generate legend of internet status
internet_legend <- function() {
  div(class="legend",
      fluidRow(
        column(2, 
               span(class="square", style=paste(
                 "background-color:",inter_on_color,sep=""))
        ),
        column(4,
               p(tags$b("Internet Online"))
        ),
        column(2, 
               span(class="square", style=paste(
                 "background-color:",inter_off_color,sep=""))
        ),
        column(4,
               p(tags$b("Internet Blackout"))
        )
      )
  )
}


# Generate html content of description
popup_vid <- function(location, description, time, link, type) {
  paste0(
    "<div>
      <div class='row'>
        <div class='col-sm-4'>
          <b>Location</b>
        </div>
        <div class='col-sm-8'>
          <p>",location,"</p>
        </div>
      </div>
      <div class='row'>
        <div class='col-sm-4'>
          <b>Description</b>
        </div>
        <div class='col-sm-8'>
          <p>",description,"</p>
        </div>
      </div>
      <div class='row'>
        <div class='col-sm-4'>
          <b>Time-stamp(UTC)</b>
        </div>
        <div class='col-sm-8'>
          <p>",time,"</p>
        </div>
      </div>",
      ifelse(type=="picture", 
      paste0("<img src='",link,"' width='70%'>",sep=""), 
      paste0("<video width='300' height='225' type='video/mp4' src='",link,"' controls='controls'></video>",sep="")),"
    </div>"
  )
}