# Weather functions

library(ncdf4)
library(tidyverse)
library("ncdf4.helpers")
library(aws.s3)

get_past_weather_data <- function(init_file, lat_farmer, lon_farmer){
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = init_file$AWS_ACCESS_KEY_ID,
    "AWS_SECRET_ACCESS_KEY" = init_file$AWS_SECRET_ACCESS_KEY,
    "AWS_DEFAULT_REGION" = init_file$AWS_DEFAULT_REGION
  )
  past_weather_data = s3read_using(FUN = nc_open, object = paste(init_file$weatherDB_loc,"ERA5_Land_monthly_averaged_data_1950_2021.nc",sep=""))
  
  past_evap<- ncvar_get(past_weather_data, varid = "e",
                        start= c(which.min(abs(past_weather_data$dim$longitude$vals - lon_farmer)), # look for closest long
                                 which.min(abs(past_weather_data$dim$latitude$vals - lat_farmer)),  # look for closest lat
                                 1),
                        count = c(1,1,-1))
  past_pevap<- ncvar_get(past_weather_data, varid = "pev",
                    start= c(which.min(abs(past_weather_data$dim$longitude$vals - lon_farmer)),
                             which.min(abs(past_weather_data$dim$latitude$vals - lat_farmer)),  # look for closest lat
                             1),
                    count = c(1,1,-1))
  past_precipitation<- ncvar_get(past_weather_data, varid = "tp",
                                 start= c(which.min(abs(past_weather_data$dim$longitude$vals - lon_farmer)), # look for closest long
                                          which.min(abs(past_weather_data$dim$latitude$vals - lat_farmer)),  # look for closest lat
                                          1),
                                 count = c(1,1,-1))
  past_temperature<- ncvar_get(past_weather_data, varid = "t2m",
                               start= c(which.min(abs(past_weather_data$dim$longitude$vals - lon_farmer)), # look for closest long
                                        which.min(abs(past_weather_data$dim$latitude$vals - lat_farmer)),  # look for closest lat
                                        1),
                               count = c(1,1,-1))
  obsdatadates = as.character(nc.get.time.series(past_weather_data))
  datafinal <- data.frame(dates= as.Date(obsdatadates))
  datafinal['month'] <- unlist(as.integer(strsplit(format(datafinal$dates,'%m'),'-')))
  days_in_a_month = data.frame(month=c(1:12),days_in_a_month=c(31,28.25,31,30,31,30,31,31,30,31,30,31))
  datafinal = left_join(datafinal,days_in_a_month,by="month")
  datafinal = datafinal %>% mutate(
    past_evap= - past_evap*1e3*days_in_a_month,
    past_pevap= - past_pevap*1e3*days_in_a_month,
    past_precipitation=past_precipitation*1e3*days_in_a_month,
    past_temperature=past_temperature-273.15
  ) %>%
    group_by(month) %>% 
    summarise(past_temperature=mean(past_temperature),
              past_precipitation=mean(past_precipitation),
              past_evap=mean(past_evap),
              past_pevap=mean(past_pevap))
  return(datafinal)
}

get_future_weather_data <- function(init_file, lat_farmer, lon_farmer, scenario = "rcp4.5"){
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = init_file$AWS_ACCESS_KEY_ID,
    "AWS_SECRET_ACCESS_KEY" = init_file$AWS_SECRET_ACCESS_KEY,
    "AWS_DEFAULT_REGION" = init_file$AWS_DEFAULT_REGION
  )
  if (scenario=="rcp8.5"){
    future_weather_data = s3read_using(FUN = nc_open, object = paste(init_file$weatherDB_loc,"ERA5_Land_extrapolated_future_rcp8.5.nc",sep=""))
  } else if(scenario=="rcp4.5"){
    future_weather_data = s3read_using(FUN = nc_open, object = paste(init_file$weatherDB_loc,"ERA5_Land_extrapolated_future_rcp4.5.nc",sep=""))
  } else{stop("wrong scenario spelling")}
  
  
  future_evap<- ncvar_get(future_weather_data, varid = "evap",
                          start= c(which.min(abs(future_weather_data$dim$lon$vals - lon_farmer)), # look for closest long
                                   which.min(abs(future_weather_data$dim$lat$vals - lat_farmer)),  # look for closest lat
                                   1),
                          count = c(1,1,-1))
  future_pevap<- ncvar_get(future_weather_data, varid = "pevap",
                    start= c(which.min(abs(future_weather_data$dim$lon$vals - lon_farmer)),
                             which.min(abs(future_weather_data$dim$lat$vals - lat_farmer)),  # look for closest lat
                             1),
                    count = c(1,1,-1))
  future_precipitation<- ncvar_get(future_weather_data, varid = "rainfall",
                                   start= c(which.min(abs(future_weather_data$dim$lon$vals - lon_farmer)), # look for closest long
                                            which.min(abs(future_weather_data$dim$lat$vals - lat_farmer)),  # look for closest lat
                                            1),
                                   count = c(1,1,-1))
  future_temperature<- ncvar_get(future_weather_data, varid = "temperature",
                                 start= c(which.min(abs(future_weather_data$dim$lon$vals - lon_farmer)), # look for closest long
                                          which.min(abs(future_weather_data$dim$lat$vals - lat_farmer)),  # look for closest lat
                                          1),
                                 count = c(1,1,-1))
  
  datafinal <- data.frame(months=future_weather_data$dim$time$vals)
  datafinal[paste('future_temperature',scenario,sep="_")] <- future_temperature
  datafinal[paste('future_precipitation',scenario,sep="_")] <- future_precipitation
  datafinal[paste('future_evap',scenario,sep="_")] <- future_evap
  datafinal[paste('future_pevap',scenario,sep="_")] <- future_pevap
  
  return(datafinal)
}
# yearly <- data.frame(dates= as.Date(obsdatadates), 
#                      pevap = pevap*1e3*30.4,
#                      evap=evap*1e3*30.4,
#                      tp=tp*1e3*30.4,
#                      temp=temp-273.15)
# yearly['year'] <- unlist(strsplit(format(yearly$dates,'%Y'),'-'))
# yearly = yearly %>% group_by(year) %>% 
#   summarise(temp_monthly=mean(temp),
#             precip_monthly=sum(tp),
#             evap_monthly=-sum(evap))