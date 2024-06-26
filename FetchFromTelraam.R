# ------------------------------------------------------------------------------

# Accessing the Telraam API
# Tina Biggs
# With thanks to
# Pete Arnold, Swansea University
# 29.02.2024

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Accessing the Telraam API - GeoJSON snapshot

# Pete Arnold, Swansea University
# Tina Biggs, Bristol University
# 23.12.2023

# ------------------------------------------------------------------------------

# Get Telraam geojson - it works only with httr at present.
# Telraam geojson gives the street shape but only as
# a snapshot.


rate <- rate_delay(1.5)

today <- Sys.time()
yesterday <- parse_date_time(paste0(date(Sys.time()-86400)," 00:00:00"),
                             "YmdHMS",tz="UTC")
rate_sleep(rate, quiet = FALSE)
request_body <- list(
  "time"="live",
  "contents"="minimal",
  "area"="-3.2,51.3,-2.3,51.7"
)

request_body_json <- toJSON(request_body, auto_unbox=TRUE)

request <- POST(
  "https://telraam-api.net/v1/reports/traffic_snapshot",
  body = request_body_json,
  add_headers(
    "X-Api-Key"="<put-API-token-here>"
  ),
  verbose()
)

telraam_geojson  <- content(request, as="text", encoding="UTF-8")
telraam_sf <- geojson_sf(telraam_geojson)
telraam_sf <- mutate(telraam_sf,sensor_id=segment_id)
telraam_sf <- select(telraam_sf,-segment_id)

# Get metadata using httr2 - this extracts the centrepoint of the street
# for the icon and popup graph


url <- "https://telraam-api.net"

# Get metadata from Telraam with sensor list for lon-lat area, 
# co-ordinates and last data reading

rate_sleep(rate, quiet = FALSE)
query_resp <- 
  # Build URL
  request(url) %>% 
  # Add extension to base url
  req_url_path("/v1/reports/traffic_snapshot") %>% 
  # Add body
  req_body_json(list(
    "time"="live",
    "contents"="minimal",
    "area"="-3.2,51.3,-2.3,51.7"
  )) %>% 
  # Add headers
  req_headers(
    "X-Api-Key"="z7jmplxRiu2679NE5Xcg44ZPPsmA10Ie8xwFuCvp",
    "Content-Type"="application/json"
  ) %>% 
  # Perform request
  req_perform() %>%
  resp_body_json()


telraam_list_all_raw  <- query_resp$features
nsensor <- length(telraam_list_all_raw)
telraam_sensorlist <- data.frame(list('sensor_id' = character(),
                                'lat' = double(),
                                'long' = double(),
                                'last_date' = character(),
                                'last_cars' = numeric(),
                                'period' = character(),
                                'is_live' = logical()))
for (isensor in 1:nsensor){
  telraam_list_all_temp <- telraam_list_all_raw[[isensor]]
  telraam_coordinates <- telraam_list_all_temp$geometry$coordinates
  telraam_sensorlist[isensor,'sensor_id'] <- as.character(telraam_list_all_temp$properties$segment_id)
  telraam_sensorlist[isensor,'long'] <- telraam_coordinates[[1]][[2]][[1]]
  telraam_sensorlist[isensor,'lat'] <- telraam_coordinates[[1]][[2]][[2]]
  telraam_sensorlist[isensor,'last_date'] <- substr(telraam_list_all_temp$properties$last_data_package,1,19)
  telraam_sensorlist[isensor,'last_cars'] <- as.numeric(telraam_list_all_temp$properties$car)
  telraam_sensorlist[isensor,'period'] <- telraam_list_all_temp$properties$period
  # The is_live flag is for the map and relates only to yesterday's reading.
  if (telraam_sensorlist[isensor,'last_date'] > yesterday){
    telraam_sensorlist[isensor,'is_live'] <- TRUE
  }else{
    telraam_sensorlist[isensor,'is_live'] <- FALSE
  }
}

# Get counts from live sensors----------------------------------------

FetchCountsFromTelraam <- function(get_date_from,get_date_to){
print("Fetching count data from Telraam")
url <- "https://telraam-api.net"
date_from <- substr(get_date_from,1,10)
# Date to is midnight = 00:00:00 on the day following.
date_to <- date(substr(get_date_to,1,10))+1
# Sample string for date 2021-06-01 07:00:00Z
date_from_t <- paste0(date_from," 00:00:00Z")
date_to_t <-  paste0(substr(date_to,1,10)," 00:00:00Z")

telraam_temp <- data.frame(list('sensor_id' = character(), 
                                          'date_to' =  character(),
                                          'cars_ph' = integer(),
                                          'pdav_cars_ph' = numeric()))
telraam_livesensor_dateperiod <- telraam_temp
for (isensor in (1:nrow(telraam_sensorlist))){
  if (parse_date_time(telraam_sensorlist[isensor,'last_date'],"YmdHMS",tz="UTC")>
    get_date_from){
    # Re-initialise tempfile for new sensor
    telraam_temp <- data.frame(list('sensor_id' = integer(), 
                                              'date_to' = character(),
                                              'cars_ph' = integer(),
                                              'pdav_cars_ph' = numeric()))
    sensorid_req <- telraam_sensorlist[isensor,'sensor_id']
    rate_sleep(rate, quiet = FALSE) 
    query_resp <- 
      # Build URL
      request(url) %>% 
      # Add extension to base url
      req_url_path("/v1/reports/traffic") %>% 
      # Add body
      req_body_json(list(
        "level"="segments",
        "format"="per-hour",
        "id"= sensorid_req,
        "time_start"= date_from_t,
        "time_end"= date_to_t
      )) %>% 
      # Add headers
      req_headers(
        "X-Api-Key"="z7jmplxRiu2679NE5Xcg44ZPPsmA10Ie8xwFuCvp",
        "Content-Type"="application/json"
      ) %>% 
      # Perform request
      req_perform() %>% 
      # Encode JSON response into usable R list
      resp_body_json()
    
    telraam_plot_raw <- query_resp$report
    ntimepoints <- length(telraam_plot_raw)

  # Now to format data points. We end by leaving the date in character format for export
  # We also take the average car count over the date period requested.
    if(ntimepoints>2){
    for (itime in 1:ntimepoints){
        telraam_temp[itime,'sensor_id'] <- sensorid_req
        telraam_temp[itime,'date_to'] <- substr(telraam_plot_raw[[itime]]$date,1,19)
        telraam_temp[itime,'cars_ph'] <- as.integer(telraam_plot_raw[[itime]]$car)
    }
    telraam_temp <- mutate(.data=telraam_temp,
        date_to=parse_date_time(date_to,"YmdHMS",tz="UTC"))
    telraam_temp[,'pdav_cars_ph'] <- base::mean(telraam_temp[,'cars_ph'],na.rm=TRUE)
    telraam_livesensor_dateperiod <- rbind(telraam_livesensor_dateperiod,telraam_temp)
    }
  } # Next sensor
} # End of FetchTelraamCounts function
telraam_livesensor_dateperiod <- mutate(telraam_livesensor_dateperiod,
                  date_to = format(date_to,"%Y-%m-%d %H:%M:%S",tz="UTC"))
print("Finished fetching Telraam data")
return(telraam_livesensor_dateperiod)
}


today <- Sys.time()
yesterday <- Sys.time()-86400
date_yesterday <- date(yesterday)
telraam_yesterday <- FetchCountsFromTelraam(yesterday,yesterday)
telraam_yesterday <- mutate(telraam_yesterday,yday_cars=pdav_cars_ph)
telraam_yesterday <- select(.data=telraam_yesterday,-pdav_cars_ph)
telraam_temp <- select(.data=telraam_yesterday,sensor_id,yday_cars)
telraam_temp <- dplyr::summarise(.data=telraam_temp,.by=c(sensor_id,yday_cars))
telraam_sensorlist <- left_join(telraam_sensorlist,
                                telraam_temp,by='sensor_id')


PickGraphFromTelraam <- function(telraam_yesterday,get_id){
  telraam_plotdata <- filter(telraam_yesterday,sensor_id==get_id)
  ntimepoints <- nrow(telraam_plotdata)
  if (ntimepoints>0){
    telraam_plotdata <- mutate(telraam_plotdata,
           date_to=parse_date_time(date_to,orders="YmdHMS",tz="UTC"))
    min_time <- telraam_plotdata[1,'date_to']
    this_day <- date(min_time)
    midday <- parse_date_time(paste0(as.character(this_day)," 12:00:00"),
                              orders="YmdHMS",tz="UTC")
    max_time <- telraam_plotdata[ntimepoints,'date_to']
    telraam_graph <- ggplot(data=telraam_plotdata,aes(date_to,cars_ph,ymin=0),na.rm=TRUE) +
      geom_point(na.rm=TRUE) +
      scale_x_datetime(breaks=c(min_time,midday,max_time),
        labels=c(format(min_time,"%H:%M:%S",tz="UTC"),
                  format(midday,"%d/%m/%Y %H:%M:%S",tz="UTC"),
                  format(max_time,"%H:%M:%S",tz="UTC"))) + 
      labs(x="Time (midnight to midnight)",
           y=paste0("Cars per hour (both directions)"),
             title=paste0("Telraam #",get_id," yesterday"))
    return(telraam_graph)
  }else{ 
    null_graph <- ggplot() +
      scale_x_datetime() +
      labs(x="Time to (midnight to midnight)",
           y=paste0("Cars per hour"),
           title=paste0("No data for Telraam #",get_id," yesterday")
      )
    return(null_graph)
  }
}  


get_id <- '9000006174'
plot(PickGraphFromTelraam(telraam_yesterday,get_id))
