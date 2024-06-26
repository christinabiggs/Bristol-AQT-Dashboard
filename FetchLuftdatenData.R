# Code to import data from Luftdaten
# for given sensor list
# Tina Biggs

library(httr)
options(stringsAsFactors = FALSE)
#set_config(config(ssl_verifypeer = 0L))


FetchCountsFromLuftdaten <- function(get_date_from,get_date_to){
print("Fetching Luftdaten/SensorCommunity data")
url_dir<-"http://archive.sensor.community" 
date_list <- seq(get_date_from, by = "day", length.out = (get_date_to-get_date_from)+1)

luft_sensorlist_id=c("7559","78434","78947","66970","10491","57227","60816","48881",
            "69775","48987","24147","56255","26044","69350","20842","48987",
            "67665","7685","69513","66966","56255","7675")
luft_sensorlist_type=c("sds011","pms5003","sds011_indoor","sds011","sds011","sds011",
            "pms5003","sds011","sds011","sds011","sds011","sds011","sds011",
      "pms7003","sds011","sds011","sds011","sds011","sds011","sds011","sds011","sds011")

data_found <- FALSE
# We only need to find one dataset to start off with, but need to search through all sensors
for(i in 1:length(luft_sensorlist_id)){
  # SensorCommunity (Luftdaten) have directories for years before 2023
  get_year <- year(get_date_from)
  datestring <- format(get_date_from,"%Y-%m-%d",tz="UTC")
  dateyr <- paste0(as.character(get_year),"/")
  if(get_year>2022){
    diryr <- ""
  }
  url_test<-paste(url_dir,"/",diryr,datestring,"/",datestring,"_",luft_sensorlist_type[i],
                  "_","sensor","_",luft_sensorlist_id[i],".csv",sep="")
  x<-GET(url_test)
  http_status(x)
  if(x$status_code==200){
    luft_sensor_test <- fread(url_test[1])
    luft_sensor_pdata_test <- dplyr::select(.data=luft_sensor_test,timestamp,P1,P2)
    luft_sensor_pdata_test <- dplyr::mutate(.data=luft_sensor_pdata_test,
                                          pm2.5=P2,pm10=P1)
    luft_sensor_pdata_test <- dplyr::select(.data=luft_sensor_pdata_test,-P1,-P2)
    #Pick out metadata for retention, discard the rest
    luft_sensor_metadata_test <- dplyr::select(.data=luft_sensor_test,sensor_id,
                                            sensor_type,location,lat,lon)
    luft_sensor_test <- data.frame(luft_sensor_metadata_test,luft_sensor_pdata_test,
                                   stringsAsFactors = FALSE)
    # As luft_sensor_test will be the template for the final file, we
    # create a character-string date_to and create a new column for pm2.5
    # as the lower of the P1 and P2 readings. 
    luft_sensor_test <- mutate(.data=luft_sensor_test,
          date_to=format(timestamp,"%Y-%m-%d %H:%M:%S"),
          pdav_pm2.5=mean(pm2.5),pdav_pm10=mean(pm10))
    luft_sensor_test <- select(luft_sensor_test, -timestamp)
    data_found <- TRUE
    #Once we have found a sensor datafile we can stop searching
    break
  }else{
  print("Trying a different sensor")
  }
}

# We strip the rows from luft_sensor_test and use this
# to initialise the final data file.
luft_allsensor_dateperiod <- luft_sensor_test[0,]
# Reading each sensor for the required day, extracting the final reading of the day for
# the sensor_list_r master file

for (i in (1:length(luft_sensorlist_id))){
  # For each sensor we re-initialise the dateperiod intermediate file
  luft_sensor_dateperiod <- luft_sensor_test[0,]
  data_found <- FALSE
  # Cycle through the days requested
  for (iday in 1:length(date_list)){
    # For each day we reinitialise the day's readings
    luft_sensor_day <- luft_sensor_test[0,]
    get_year <- year(date_list[[iday]])
    datestring <- format(date_list[[iday]],"%Y-%m-%d")
    dateyr <- paste0(as.character(get_year),"/")
    if(get_year>2022){
      diryr <- ""
    }
    url_sensor<-paste0(url_dir,"/",diryr,datestring,"/",datestring,"_",
          luft_sensorlist_type[i],"_","sensor","_",luft_sensorlist_id[i],".csv")
    x<-GET(url_sensor)
    http_status(x)
    if(x$status_code==200){
      # This reads the day's readings for that sensor.
      data_found <- TRUE
      luft_sensor_raw <- fread(url_sensor)
      luft_sensor_day <- luft_sensor_raw
      # Now we have a day's readings for this sensor, we 
      # bin it into hour intervals. To do this we have to 
      # split it off from the metadata and then reattach it at the end.
      luft_sensor_pdata <- dplyr::select(luft_sensor_day,timestamp,P1,P2)
      luft_sensor_pdata <- dplyr::mutate(luft_sensor_pdata,pm2.5=P2,pm10=P1)
      luft_sensor_pdata <- dplyr::select(luft_sensor_pdata,-P1,-P2)
      luft_sensor_metadata <- dplyr::select(luft_sensor_day,sensor_id,
                                            sensor_type,location,lat,lon)
    # Here we aggregate on the hour, but record aggregated time as "Time to" the next hour
      luft_sensor_pdata <- mutate(.data=luft_sensor_pdata,hourbin=hour(timestamp))
      luft_sensor_pdata <- dplyr::select(.data=luft_sensor_pdata,hourbin,pm2.5,pm10)
      luft_sensor_pdata <- aggregate(.~hourbin,data=luft_sensor_pdata,FUN=mean)
      # This reconstructs the time of the binned measurement as at the end of 
      # current hour - being the beginning of the next hour (hence 3600+)
      luft_sensor_pdata <- mutate(.data=luft_sensor_pdata,date_to=
        3600+as.POSIXct(paste0(datestring," ",ifelse(hourbin<10,"0",""),hourbin,":00:00")),tz="UTC")
      luft_sensor_pdata <- dplyr::select(.data=luft_sensor_pdata,date_to,pm2.5,pm10,-hourbin)
      ntpoints <- nrow(luft_sensor_pdata)
      # Here we copy over the lat-long metadata and concatenate it to the new time bins 
      # as the metadata is the same for all sensor readings for the day
      luft_sensor_day <- data.frame(luft_sensor_metadata[1:ntpoints,],luft_sensor_pdata,
                      stringsAsFactors=FALSE)
      # Now we find the day average of pm2.5 and pm10 (which we may overwrite later
      # but need to create a placeholder for now)
      # We also turn date_to into a character string for export
      # If we need date as a POSIXct object we need to convert it the other end.
        luft_sensor_day <- mutate(.data=luft_sensor_day,date_to=
                format(date_to,"%Y-%m-%d %H:%M:%S"),pdav_pm2.5=mean(pm2.5),pdav_pm10=mean(pm10))
        luft_sensor_dateperiod <- rbind(luft_sensor_dateperiod,luft_sensor_day)
      }
  } # Next day in series of dates. We do this first so all the readings
  # for each sensor are in the same block
  # Once we have all the sensor readings for the custom date period, we take the mean 
  # and then add it onto the total file for the requested date period
if(data_found){
  luft_sensor_dateperiod <- mutate(luft_sensor_dateperiod,pdav_pm2.5=mean(pm2.5),pdav_pm10=mean(pm10))
  luft_allsensor_dateperiod <- rbind(luft_allsensor_dateperiod,luft_sensor_dateperiod)
}
} # Next sensor in list
print("Finished fetching count data from Luftdaten/SensorCommunity")
luft_allsensor_dateperiod <- dplyr::arrange(.data=luft_allsensor_dateperiod,
                                            sensor_id,date_to)
return(luft_allsensor_dateperiod)
} # End of FetchCountsFromLuftdaten


todays_date <- Sys.time()
yesterday <- Sys.time()-86400
yesterdays_date_ld <- substr(yesterday,1,10)
get_date_from <- parse_date_time(yesterdays_date_ld,"Ymd")
get_date_to <- get_date_from

# Each FetchCounts function takes a POSIXct object for get_date_from
# and get_date_to. Here we pull out the data for all sensors for yesterday 
# (one day, so get_date_from=get_date_to) so it is ready for the plot function.
# We rename the average counts for yesterday as yday_pm2.5 for clarity

luft_yesterday <- FetchCountsFromLuftdaten(get_date_from,get_date_to)
luft_yesterday <- mutate(luft_yesterday,yday_pm2.5=pdav_pm2.5,yday_pm10=pdav_pm10)
luft_yesterday <- select(.data=luft_yesterday,-pdav_pm2.5,-pdav_pm10)

# As Luftdaten has no easily-accessible metadata, just archive, we create the
# metadata using the timepoint readings. This will be only for sensors
# with readings yesterday.

luft_sensorlist <- select(.data=luft_yesterday,-date_to)
luft_sensorlist <- dplyr::summarise(.data=luft_sensorlist,
                            .by=c(sensor_id,sensor_type,yday_pm2.5,yday_pm10,lat,lon,location))
luft_sensorlist <- mutate(luft_sensorlist,date_to=format(get_date_to,"%Y-%m-%d",tz="UTC"))


# PickGraphFromLuftdaten is the function to get yesterday's sensor plot 
# data for a given sensor number, ready for use as a popup graph.
# We plot P1 and P2, plus the reference DEFRA data from St Paul's
# as a line plot

PickGraphFromLuftdaten <- function(luft_yesterday,DEFRA_yesterday,get_id){
  luft_plotdata <- dplyr::filter(luft_yesterday,sensor_id==get_id)
  # Find first, middle and last timepoints for x-axis labelling
  if (nrow(luft_plotdata>0)){
    # We use BRS8, the Bristol St Paul's sensor, as the standard for comparing to Luftdaten
    DEFRA_plotdata <- dplyr::filter(DEFRA_yesterday,code=='BRS8')
    DEFRA_plotdata <- dplyr::select(.data=DEFRA_plotdata,date_to,pm2.5)
    # Convert date_to into a date-time object for plotting
    DEFRA_plotdata <- mutate(.data=DEFRA_plotdata,
          date_to=parse_date_time(date_to,"YmdHMS",tz="UTC"),DEFRA_pm2.5=pm2.5)
    DEFRA_plotdata <- select(.data=DEFRA_plotdata,-pm2.5)
    luft_plotdata <- dplyr::select(.data=luft_plotdata,date_to,pm2.5)
    luft_plotdata <- mutate(.data=luft_plotdata,
              date_to=parse_date_time(date_to,"YmdHMS",tz="UTC"),
              luft_pm2.5=pm2.5)
    luft_plotdata <- select(luft_plotdata,-pm2.5)
    plot_data <- full_join(luft_plotdata,DEFRA_plotdata,by='date_to')
    plot_data <- arrange(plot_data,date_to)
    ntimepoints <- nrow(plot_data)
    min_time <- plot_data[1,'date_to']
    get_date <- date(min_time)
    midday <- parse_date_time(paste0(get_date," 12:00:00"),"%YmdHMS",tz="UTC")
    max_time <- plot_data[ntimepoints,'date_to']
    DEFRA_ymax <- max(plot_data$DEFRA_pm2.5,na.rm=TRUE)
    luft_ymax <- max(plot_data$luft_pm2.5,na.rm=TRUE)
    ymax=max(DEFRA_ymax,luft_ymax)
    luft_graph <- ggplot(data=plot_data,aes(date_to,luft_pm2.5,ymin=0)) +
      geom_point(na.rm=TRUE) +
      geom_line(aes(date_to,DEFRA_pm2.5)) +
      scale_x_datetime(breaks=c(min_time,midday,max_time),
                       labels=c(format(min_time,"%H:%M:%S"),
                                format(midday,"%d/%m/%Y %H:%M:%S"),
                                format(max_time,"%H:%M:%S"))) + 
      geom_hline(aes(yintercept=quantile(luft_pm2.5,0.05,na.rm=TRUE))) +
      annotate("text",x=min_time+0.5*(max_time-min_time),y=1.1*ymax,label="--- PM2.5 DEFRA Bristol St Pauls") + 
      labs(x="Time (midnight to midnight)",y="PM2.5 (ug/m3)",
         title=paste0("SensorCommunity #",get_id," yesterday")
      )
  return(luft_graph)
  }else{
    null_graph <- ggplot() +
      labs(x="Time (midnight to midnight)",y="PM2.5 (ug/m3)",
           title=paste0("No data for SensorCommunity ",get_id,"yesterday")
      )
  return(null_graph)  
  }
}

# This is a test to make sure the plot function works.

get_id <- "48987"
plot(PickGraphFromLuftdaten(luft_yesterday,DEFRA_yesterday,get_id))
