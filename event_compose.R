# initialize and transform event data 
event_dat <- read.csv('data/events.csv')

jitter_fact <- 5
# # jitter to prevent overlap. Coordinates represent general area
event_dat$latitude <- jitter(event_dat$latitude, factor=jitter_fact)
event_dat$longitude <- jitter(event_dat$longitude, factor=jitter_fact)

# Factor labels
event_dat$label <- gsub("-", "", event_dat$label)
event_dat$label <- gsub(" ", "", event_dat$label)
event_dat$label <- factor(event_dat$label, levels=legend_dat_html$label)

# convert to sf for use with tmap
event_sf <- st_as_sf(event_dat, coords=c("longitude", "latitude"), crs="WGS84")

# convert to proper datetime
event_sf$datetime_UTC <- 
  as.POSIXct(event_sf$datetime_UTC, format="%m/%d/%Y %H:%M", tz="UTC")

sec_in_hour <- 3600

INTERNET_BLACKOUT <- as.POSIXct("2022-01-05 11:48","%Y-%m-%d %H:%M", tz="UTC")
color_change <- FALSE

hour_interv <- 12