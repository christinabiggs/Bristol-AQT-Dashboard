# Vivacity historic count request
# Tina Biggs
# With thanks to Leo Gorman, Jean Golding Institute, Bristol 
# 27/02/2024


# Authentication ----------------------------------------------------------
# This fetches a token which expires after one hour.

url <- "https://api.vivacitylabs.com"

auth_response <- 
  # Build URL
  request(url) %>% 
  # Add extension to base url
  req_url_path("/get-token") %>% 
  # Add body
  req_body_json(list(
    "grant_type"="password",
    "username"="university-of-bristol-api-user",
    "password"="<put-UoB-password-here>"
  )) %>% 
  # Add headers
  req_headers(
    "Content-Type"="application/x-www-form-urlencoded"
  ) %>% 
  # Ensure encoded as form
  req_body_form() %>% 
  # Perform request
  req_perform() %>% 
  # Encode JSON response into usable R list
  resp_body_json()

# Fetch countline metadata -----------------------------------------------------

query_resp <- 
  # Build URL
  request(url) %>% 
  # Add extension to base url
  req_url_path("/countline") %>% 
  # Add parameters
  req_url_query(
    `api-version`=2
  ) %>% 
  # Add headers
  req_headers(
    "accept"="application/json",
    "api-version"=2,
    "Authorization"= paste0("Bearer ",auth_response$access_token)
  ) %>% 
  # Perform request
  req_perform()  %>%
  # Encode JSON response into usable R list
  resp_body_json()


vivacity_countlines_raw  <- query_resp 

vivacity_sensorlist <- data.frame(sensor_id=character(),
      name=character(), lat=double(), lon=double(),
      stringsAsFactors=FALSE)
sensorlist_temp_df <- vivacity_sensorlist

for (isensor in 1:length(vivacity_countlines_raw)){
   sensorlist_raw <- vivacity_countlines_raw[[isensor]] 
   sensorlist_temp_df[1,'sensor_id'] <- sensorlist_raw$id
   sensorlist_temp_df[1,'name'] <- sensorlist_raw$name
   sensorlist_temp_df[1,'lat'] <- sensorlist_raw$location$centre$lat
   sensorlist_temp_df[1,'lon'] <- sensorlist_raw$location$centre$long
   
   vivacity_sensorlist <- rbind(vivacity_sensorlist,sensorlist_temp_df)
}

print("Fetched metalist")

# Fetch Counts ------------------------------------------------------------

# As there is a 48-hour limit on single requests, and as the hour binning
# works best using a 24-hour clock, we just download 24 hours at a time.

FetchCountsFromVivaCity <- function(get_date_from,get_date_to){
print("Fetching count data from Vivacity")
# Authorise once per function call (as token may have expired)
  
  url <- "https://api.vivacitylabs.com"
  auth_response <- 
    # Build URL
    request(url) %>% 
    # Add extension to base url
    req_url_path("/get-token") %>% 
    # Add body
    req_body_json(list(
      "grant_type"="password",
      "username"="university-of-bristol-api-user",
      "password"="hroCp542e`3`"
    )) %>% 
    # Add headers
    req_headers(
      "Content-Type"="application/x-www-form-urlencoded"
    ) %>% 
    # Ensure encoded as form
    req_body_form() %>% 
    # Perform request
    req_perform() %>% 
    # Encode JSON response into usable R list
    resp_body_json()
  
# Initialise the final dataframe  
vivacity_allsensor_dateperiod <- data.frame(sensor_id=character(),
    date_from=as.character(),date_to=as.character(),countIn=integer(),countOut=integer(),
    cars_ph=integer(),pdav_cars=numeric(),stringsAsFactors=FALSE)

get_date_from <- date(get_date_from)
get_date_to <- date(get_date_to)
ndays <- as.integer(get_date_to-get_date_from)+1

for (idays in 1:ndays){
get_date_r <- get_date_from + (idays-1)
get_reqdate <- as.POSIXct(get_date_r)
get_reqdate_plus1 <- get_reqdate + 86400
get_date_rplus1 <- format(get_reqdate_plus1,"%Y-%m-%d",tz="UTC")
# Sample string "2020-03-08T16:45:00.000Z"
get_date_from_vc <- paste0(get_date_r,"T00:00:00.000Z")
get_date_to_vc <- paste0(get_date_rplus1,"T00:00:00.000Z")
  
query_resp <- 
  # Build URL
  request(url) %>% 
  # Add extension to base url
  req_url_path("/counts") %>% 
  # Add parameters
  req_url_query(
    `class`='car',
    `includeZeroCounts`=TRUE,
    `timeFrom`=get_date_from_vc,
    `timeTo`=get_date_to_vc,
    `vivacityNotebookSource`="counts",
    `api-version`=2
  ) %>% 
  # Add headers
  req_headers(
    "accept"="application/json",
    "api-version"=2,
    "Authorization"= paste0("Bearer ",auth_response$access_token)
  ) %>% 
  # Perform request
  req_perform() %>% 
  # Encode JSON response into usable R list
  resp_body_json()


# Write Result ------------------------------------------------------------
vivacity_raw  <- query_resp %>% as_tibble(flatten=TRUE) 

print("Fetched raw data")

# Pulling out car count data
# Sensor ids are found as the column names of the tibble 
vivacity_sensorids <- colnames(vivacity_raw)
# Create a blank data frame with row names as sensorid
sensortemp_r <- data.frame(sensor_id=character(),
    date_from=as.character(),date_to=as.character(),countIn=integer(),countOut=integer(),
            cars_ph=integer(),pdav_cars=numeric(),stringsAsFactors=FALSE)
for (isensor in 1:length(vivacity_sensorids)){
  #Initialise the time series for the new sensor
  vivacity_sensor_temp <- 
    data.frame(sensor_id=character(),date_from=as.character(),
    date_to=as.character(),countIn=integer(),countOut=integer(),
    cars_ph=integer(),pdav_cars=numeric(),stringsAsFactors=FALSE)
  sensorid <- vivacity_sensorids[isensor]
  num_timepoints <- length(vivacity_raw[[isensor]])
  if(num_timepoints>0){
  for (itime in 1:num_timepoints){
    sensor_temp <- vivacity_raw[[isensor]][[itime]]
    #sensor_temp is a sub-object that works well to pull out data for each sensor
    sensortemp_r[1,'sensor_id'] <- vivacity_sensorids[isensor]
    sensortemp_r[1,'date_from'] <- sensor_temp$from
    sensortemp_r[1,'date_to'] <- sensor_temp$to
    sensortemp_r[1,'countIn'] <- as.numeric(sensor_temp$counts[[1]]$countIn)
    sensortemp_r[1,'countOut'] <- as.numeric(sensor_temp$counts[[1]]$countOut)
    # Add to the day's readings for that sensor
    vivacity_sensor_temp <- rbind(vivacity_sensor_temp,sensortemp_r)
    }#Now do next timepoint
    no_data=FALSE
    # If all the values are zero, we want to delete the readings for this sensor
    if(mean(vivacity_sensor_temp[,'countIn']+vivacity_sensor_temp[,'countOut'])==0){
      no_data=TRUE
      vivacity_sensor_temp <- 
        data.frame(sensor_id=character(),date_from=as.character(),
                   date_to=as.character(),countIn=integer(),countOut=integer(),
                   cars_ph=integer(),pdav_cars=numeric(),stringsAsFactors=FALSE)
    }
  # Now we have all the readings for one sensor for a 24-hour period
  # Now we bin the timepoint data to once per hour
  if(!no_data){
  vivacity_sensor_day <- vivacity_sensor_temp
  vivacity_sensor_pdata <- dplyr::select(vivacity_sensor_day,date_from,date_to,countIn,countOut)
  vivacity_sensor_pdata <- mutate(.data=vivacity_sensor_pdata,date_from=
                                    parse_date_time(date_from,"YmdHMS"))
  vivacity_sensor_pdata <- mutate(.data=vivacity_sensor_pdata,hourbin=hour(date_from))
  vivacity_sensor_pdata <- dplyr::select(.data=vivacity_sensor_pdata,hourbin,countIn,countOut)
  # The crucial bit is that car count data adds up when binning over the hour
  vivacity_sensor_pdata <- aggregate(.~hourbin,data=vivacity_sensor_pdata,FUN=sum)
  # Also as countIn and countOut are different directions, we leave both in 
  # dataset but add in the total cars per hour and calculate the day average
  vivacity_sensor_pdata <- mutate(.data=vivacity_sensor_pdata, sensor_id=sensorid,
     date_from=paste0(get_date_r," ",ifelse(hourbin<10,"0",""),hourbin,":00:00"),
    date_to=format(3600+parse_date_time(date_from,"YmdHMS",tz="UTC"),"%Y-%m-%d %H:%M:%S",tz="UTC"),
     cars_ph=countIn+countOut,pdav_cars=mean(cars_ph))
  vivacity_sensor_day <- dplyr::select(.data=vivacity_sensor_pdata,
                sensor_id,date_to,countIn,countOut,cars_ph,pdav_cars,-hourbin)
  #Add the day's readings to the plotting data
  vivacity_allsensor_dateperiod <- rbind(vivacity_allsensor_dateperiod,vivacity_sensor_day)

  } # End if !no_data  
  } #End if num_timepoints>0
} #Now do the next sensor    

} #Now do the next day
# As this mixes up the day order, we rearrange according to sensorid and then date
if(ndays>1){
  vivacity_allsensor_dateperiod <- dplyr::arrange(.data=vivacity_allsensor_dateperiod,sensor_id,date_to)
}
return(vivacity_allsensor_dateperiod)
print("Finished fetching count data from Vivacity")
} # End of fetch data from VivaCity


today <- Sys.time()
yesterday <- Sys.time()-86400
yesterday_date <- substr(yesterday,1,10)
yesterday_date_v <- parse_date_time(yesterday_date,"Ymd",tz="UTC")

vivacity_yesterday <- FetchCountsFromVivaCity(yesterday_date_v,yesterday_date_v)
vivacity_yesterday <- mutate(vivacity_yesterday,yday_cars=pdav_cars)
vivacity_yesterday <- select(.data=vivacity_yesterday,-pdav_cars)
vivacity_temp <- select(.data=vivacity_yesterday,sensor_id,yday_cars)
vivacity_temp <- dplyr::summarise(.data=vivacity_temp,.by=c(sensor_id,yday_cars))
vivacity_temp <- mutate(vivacity_temp,date=yesterday_date)
vivacity_sensorlist <- left_join(vivacity_sensorlist,vivacity_temp,by='sensor_id')

PickGraphFromVivaCity <- function(vivacity_yesterday,get_id){
  vivacity_plotdata <- filter(vivacity_yesterday,sensor_id==get_id)
  vivacity_plotdata <- mutate(.data=vivacity_plotdata,
                              date_to=parse_date_time(date_to,"YmdHMS",tz="UTC"))
  ntimepoints <- nrow(vivacity_plotdata)
  min_time <- vivacity_plotdata[1,'date_to']
  days_date <- date(min_time)
  midday <- parse_date_time(paste0(days_date," 12:00:00"),"%YmdHMS")
  max_time <- vivacity_plotdata[ntimepoints,'date_to']
  if (ntimepoints>0){
    vivacity_graph <- ggplot(data=vivacity_plotdata,aes(date_to,cars_ph,ymin=0)) +
      geom_point(na.rm=TRUE) +
      scale_x_datetime(breaks=c(min_time,midday,max_time),
                       labels=c(format(min_time,"%H:%M:%S"),
                                format(midday,"%d/%m/%Y %H:%M:%S"),
                                format(max_time,"%H:%M:%S"))) + 
      labs(x="Time (midnight to midnight)",y="Cars per hour (both directions)",
           title=paste0("VivaCity #",get_id," yesterday"))
    return(vivacity_graph)
  }else{
    null_graph <- ggplot() +
      labs(title=paste0("No car count data for Vivacity #",get_id))
    return(null_graph)
  }
}

# Test script for checking plots

get_id <- '16826'
plot(PickGraphFromVivaCity(vivacity_yesterday,get_id))

