# Using OpenAir to get DEFRA sensor data
# Note: KCL data also available but it duplicates DEFRA data and
# the other sensors are no longer operational (though we can find historic data)
# and there is a warning that the data may not be validated.

# 30/09/2023


today <- Sys.Date()
this_year <- lubridate::year(today)
yesterday <- today - 1

# DEFRA have a separate importMeta function to get the metadata for all UK
# We filter this later to get regional sensors using lat and long

MetaSiteDEFRA <- importMeta(source = "aurn", all = FALSE, duplicate = FALSE, 
                            year = this_year)

get_date_from <- lubridate::date(yesterday)
get_date_to <- lubridate::date(yesterday)


FetchCountsFromDEFRA <- function(get_date_from,get_date_to){
print("Fetching count data from DEFRA")
  
# Get_date_from and get_date_to arrive as their date objects,
# but we need to put the start and finish times as 00:00:00 and 23:00:00 respectively.
# DEFRA only store up to 23:00:00 for yesterday's data. 
  
get_datetime_from <- as.POSIXct(paste0(base::format(get_date_from,"%Y-%m-%d ",tz="UTC"),
                                       "00:00:00"),tz="UTC")
get_datetime_to <- as.POSIXct(paste0(base::format(get_date_to,"%Y-%m-%d ",tz="UTC"),
                                     "23:00:00"),tz="UTC")

startyear <- lubridate::year(get_date_from)
endyear <- lubridate::year(get_date_to)
nyear <- endyear-startyear+1
j <- 0 
for (isensor in 1:nrow(MetaSiteDEFRA)){
    if (((MetaSiteDEFRA$latitude[isensor] < 51.7) & 
       (MetaSiteDEFRA$latitude[isensor] > 51.3))&
      ((MetaSiteDEFRA$longitude[isensor] < -2.35)& 
       (MetaSiteDEFRA$longitude[isensor] > -3.2)))
    {
    # As we are taking a subset of the full UK list, we count using j:
    j <- j + 1
    # We can only get DEFRA data one year at a time, and one sensor at a time.
    pm2.5_present <- FALSE
    pm10_present <- FALSE
    for (iyear in (1:nyear)){
      this_year <- as.integer(startyear) + iyear - 1
      # DEFRA stores one year at a time  
      DEFRA_currentsensor <- importAURN(
        site = MetaSiteDEFRA$code[isensor],
        year = this_year,
        data_type = "hourly",
        pollutant = "all",
        hc = FALSE,
        meta = FALSE,
        meteo = TRUE,
        ratified = FALSE,
        to_narrow = FALSE,
        verbose = FALSE,
        progress = TRUE
      )
      pollutants <- colnames(DEFRA_currentsensor)
      
      # The column names for that particular sensor will 
      # indicate if pm2.5 is present.
      for (k in 1:ncol(DEFRA_currentsensor)){
      if(colnames(DEFRA_currentsensor[k])=="pm2.5"){pm2.5_present <- TRUE}
      if(colnames(DEFRA_currentsensor[k])=="pm10"){pm10_present <- TRUE}
      }
      # The DEFRA data is exported as hourly readings for the whole year for that sensor
      # We now need to select the data within the time period requested.
      
      DEFRA_date <- dplyr::filter(DEFRA_currentsensor, 
                                  ((date >= get_datetime_from) & (date <= get_datetime_to)))
      
      # The DEFRA data has "date" as a POSIXct object.
      # For the purposes of data export We create date_to, a character 
      # string object and remove the original date
      # This is supposing that the time given by DEFRA against a reading is the time when
      # the sensor reports the data to DEFRA, at the end of the hour
      # during which the date is collected.
      
      DEFRA_date <- dplyr::mutate(DEFRA_date,date_to=
                       base::format(date,"%Y-%m-%d %H:%M%:%S",tz="UTC"))
      DEFRA_date <- dplyr::select(DEFRA_date,-date)
      
      # Get mean pm2.5 and pm10 for the date period (within the same year) specified
      if(pm2.5_present){
        DEFRA_date <- dplyr::mutate(.data=DEFRA_date,pdav_pm2.5=mean(pm2.5,na.rm=TRUE))
      }else{
        DEFRA_date <- dplyr::mutate(.data=DEFRA_date,pdav_pm2.5=0)
      }
      if(pm10_present){
        DEFRA_date <- dplyr::mutate(.data=DEFRA_date,pdav_pm10=mean(pm10,na.rm=TRUE))
      }else{
        DEFRA_date <- dplyr::mutate(.data=DEFRA_date,pdav_pm10=0)
      }
      
    # We initialise the output data using the first dataset within the
    # lat-long range, whether or not pm2.5 is present.
    if (j==1){
        DEFRA_allsensor_dateperiod <- DEFRA_date
    }else{
        DEFRA_allsensor_dateperiod <- plyr::rbind.fill(DEFRA_allsensor_dateperiod,DEFRA_date)
    }
    } # We then do the next year (if the data period is over 
    # more than one year)- this keeps sensor readings together
  } # End-if sensor is in lat-long range
} # Next sensor
print("Finished fetching DEFRA data")
return(DEFRA_allsensor_dateperiod)
} # End of FetchDataFromDEFRA

today <- Sys.time()
yesterday <- Sys.time()-86400
yesterday_date <- lubridate::date(yesterday)
today_date <- lubridate::date(today)
DEFRA_yesterday <- FetchCountsFromDEFRA(yesterday_date,yesterday_date)
#For clarity we are renaming the averaged pm2.5 and pm10 columns in DEFRA_yesterday
DEFRA_yesterday <- dplyr::mutate(DEFRA_yesterday,
                      yday_pm2.5=pdav_pm2.5,yday_pm10=pdav_pm10)
DEFRA_yesterday <- dplyr::select(DEFRA_yesterday,-pdav_pm2.5,-pdav_pm10)

# Now we want to extract yesterday's average readings to add to the DEFRA metalist
DEFRA_sensorlist <- dplyr::select(DEFRA_yesterday,code,yday_pm2.5,yday_pm10)
DEFRA_sensorlist <- dplyr::mutate(DEFRA_sensorlist,date_to=format(yesterday_date,"%d-%m-%Y",tz="UTC"))
DEFRA_sensorlist <- dplyr::summarise(.data=DEFRA_sensorlist,
          .by=c(code,date_to,yday_pm2.5,yday_pm10))
DEFRA_sensorlist <- left_join(DEFRA_sensorlist,MetaSiteDEFRA,by='code')


PickGraphFromDEFRA <- function(DEFRA_yesterday,get_id){
  DEFRA_plotdata <- dplyr::filter(DEFRA_yesterday,code==get_id)
  DEFRA_pmdata <- dplyr::select(DEFRA_plotdata,date_to,pm2.5)
  DEFRA_pmdata <- dplyr::mutate(DEFRA_pmdata,date_to=base::as.POSIXct(date_to,tz="UTC"))
  site <- DEFRA_plotdata$site[1]
  ntimepoints <- nrow(DEFRA_pmdata)
  if(ntimepoints>1){
    has_pm2.5 <- FALSE
    for (itime in 1:nrow(DEFRA_pmdata)){
      if (!is.na(DEFRA_pmdata$pm2.5[itime])){
        has_pm2.5 <- TRUE
      }
    }
    min_time <- DEFRA_pmdata[1,'date_to']
    days_date <- lubridate::date(min_time)
    midday <- base::as.POSIXct(paste0(days_date," 12:00:00"),"YmdHMS",tz="UTC")
    max_time <- DEFRA_pmdata[ntimepoints,'date_to']
    if (has_pm2.5){
      DEFRA_graph <- ggplot(data=DEFRA_pmdata,aes(date_to,pm2.5,ymin=0)) +
          geom_point(na.rm=TRUE) +
          scale_x_datetime(breaks=c(min_time,midday,max_time),
                         labels=c(base::format(min_time,"%H:%M:%S",tz="UTC"),
                        base::format(midday,"%d/%m/%Y %H:%M:%S",tz="UTC"),
                        base::format(max_time,"%H:%M:%S",tz="UTC"))) +
          geom_hline(aes(yintercept=quantile(pm2.5,0.05,na.rm=TRUE))) +
          labs(x="Time (midnight to midnight)",y="PM2.5 (ug/m3)",
             title=paste0("DEFRA #",get_id," yesterday"))
      return(DEFRA_graph)
    }else{
    null_graph <- ggplot() +
      labs(x="Time (midnight to midnight)",y="PM2.5 (ug/m3)",
           title=paste0("No pm2.5 data for DEFRA #",get_id," yesterday"))
    return(null_graph)
    }
  }
}  
 
# Test plot script 
get_id <- "BRS8"
plot(PickGraphFromDEFRA(DEFRA_yesterday,get_id))


