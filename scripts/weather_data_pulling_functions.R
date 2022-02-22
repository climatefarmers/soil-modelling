# Weather functions

library(ncdf4)
library(tidyverse)


working_dir <- getwd()

parameters <- fromJSON(file.path(working_dir, "parameter_files", "init.json"))
# All the data should be upload to 'H:/Shared Drives/Climate Farmers/07_Tech/Modelling/WeatherDB'
weatherDB_loc <- parameters$weatherDB_loc

get_monthly_mean_temperature <- function(lon_farmer,lat_farmer){
  ########################
  # Takes the longitude-latitude of the farm location
  # Return two lists :
  # - FIRST, the monthly mean temperature between 2000 and 2020, as a proxy of PAST weather for each month
  # - SECOND, the monthly mean temperature between 2020 and 2040, as a proxy of FUTURE weather for each month
  # Spatial resolution is 1 x 1 km. Temporal resolution is the month, but averaged over 20 years.
  #######################
    # set path and name of netCDF file
  ncdir <- "sis-biodiversity-cmip5-regional_data/sis-biodiversity-cmip5-regional-rainfall-temperature"
  ncname <- "downscaled_monthly_rainfall_temperature_1x1km.nc"#"downscaled_monthly_rainfall_temperature_1x1km"
  ncpath <- file.path(weatherDB_loc, ncdir, ncname)
  # open a netCDF file
  obsdata <- nc_open(ncpath)
  # get dates
  obsdatadates <- as.Date(obsdata$dim$time$vals, origin = '1970-01-01')
  # choosing variable name
  dname <- "temperature" 
  # get values at location lonlat
  obs_temperature <- ncvar_get(obsdata, varid = dname,
                         start= c(which.min(abs(obsdata$dim$x$vals - lon_farmer)), # look for closest long
                                  which.min(abs(obsdata$dim$y$vals - lat_farmer)),  # look for closest lat
                                  1),
                         count = c(1,1,-1)) #count '-1' means 'all values along that dimension'
  # create dataframe
  datafinal <- data.frame(dates= obsdatadates, temperature = obs_temperature)
  datafinal['year'] <- unlist(strsplit(format(datafinal$dates,'%Y'),'-')) #extract the year
  datafinal['month'] <- unlist(strsplit(format(datafinal$dates,'%m'),'-')) #extract the month
  temperature_2000_2020 <- datafinal %>% filter(year=="2009") %>% summarise(month, temperature)
  temperature_2020_2040 <- datafinal %>% filter(year=="2030") %>% summarise(month, temperature)
  return(c(temperature_2000_2020$temperature,temperature_2020_2040$temperature))
  
}


get_monthly_mean_precipitation <- function(lon_farmer,lat_farmer){
  ########################
  # Takes the longitude-latitude of the farm location
  # Return two lists :
  # - FIRST, the monthly mean precipitation between 2000 and 2020, as a proxy of PAST weather for each month
  # - SECOND, the monthly mean precipitation between 2020 and 2040, as a proxy of FUTURE weather for each month
  # Spatial resolution is 1 x 1 km. Temporal resolution is the month, but averaged over 20 years.
  #######################
  
  # set path and name of netCDF file
  ncdir <- "sis-biodiversity-cmip5-regional_data/sis-biodiversity-cmip5-regional-rainfall-temperature"
  ncname <- "downscaled_monthly_rainfall_temperature_1x1km.nc"#"downscaled_monthly_rainfall_precipitation_1x1km"
  ncpath <- file.path(weatherDB_loc, ncdir, ncname)
  # open a netCDF file
  obsdata <- nc_open(ncpath)
  # get dates
  obsdatadates <- as.Date(obsdata$dim$time$vals, origin = '1970-01-01')
  # choosing variable name
  dname <- "precipitation" 
  # get values at location lonlat
  obs_precipitation <- ncvar_get(obsdata, varid = dname,
                                 start= c(which.min(abs(obsdata$dim$x$vals - lon_farmer)), # look for closest long
                                          which.min(abs(obsdata$dim$y$vals - lat_farmer)),  # look for closest lat
                                          1),
                                 count = c(1,1,-1)) #count '-1' means 'all values along that dimension'
  # create dataframe
  datafinal <- data.frame(dates= obsdatadates, precipitation = obs_precipitation)
  datafinal['year'] <- unlist(strsplit(format(datafinal$dates,'%Y'),'-')) #extract the year
  datafinal['month'] <- unlist(strsplit(format(datafinal$dates,'%m'),'-')) #extract the month
  precipitation_2000_2020 <- datafinal %>% filter(year=="2009") %>% summarise(month, precipitation)
  precipitation_2020_2040 <- datafinal %>% filter(year=="2030") %>% summarise(month, precipitation)
  return(c(precipitation_2000_2020$precipitation,precipitation_2020_2040$precipitation))
  
}




get_monthly_mean_pevap <- function(lon_farmer,lat_farmer){
  ########################
  # Takes the longitude-latitude of the farm location
  # Return two lists :
  # - FIRST, the monthly mean potential evaporation between 2000 and 2020, as a proxy of PAST weather for each month
  # - SECOND, the monthly mean potential evaporation between 2020 and 2040, as a proxy of FUTURE weather for each month
  # Spatial resolution is 1 x 1 km. 
  # Temporal resolution was quarter to year, averaged over 20 years.
  # We assumed that January to March is the wettest quarter, July to September is the driest quarter 
  # while April to June and Octover to December corresponds to the annual mean.
  #######################
  
  # set ncpath and 
  ncdir <- "sis-biodiversity-cmip5-regional_data/sis-biodiversity-cmip5-regional-potential_evapotranspiration"
  ncname <- "downscaled_monthly_pevaporation_1x1km.nc"
  ncpath <- file.path(weatherDB_loc, ncdir, ncname)
  # open a netCDF file
  obsdata <- nc_open(ncpath)
  # get dates
  obsdatadates <- as.Date(obsdata$dim$time$vals, origin = '1960-01-01')
  # get values at location lonlat
  pevap_annual_mean <- ncvar_get(obsdata, varid = "potential-evaporation_annual-mean",
                                 start= c(which.min(abs(obsdata$dim$x$vals - lon_farmer)), # look for closest long
                                          which.min(abs(obsdata$dim$y$vals - lat_farmer)),  # look for closest lat
                                          1),
                                 count = c(1,1,-1)) #count '-1' means 'all values along that dimension'
  pevap_driest_quarter <- ncvar_get(obsdata, varid = "potential-evaporation_driest_quarter",
                                    start= c(which.min(abs(obsdata$dim$x$vals - lon_farmer)), # look for closest long
                                             which.min(abs(obsdata$dim$y$vals - lat_farmer)),  # look for closest lat
                                             1),
                                    count = c(1,1,-1)) #count '-1' means 'all values along that dimension'
  pevap_wettest_quarter <- ncvar_get(obsdata, varid = "potential-evaporation_wettest_quarter",
                                     start= c(which.min(abs(obsdata$dim$x$vals - lon_farmer)), # look for closest long
                                              which.min(abs(obsdata$dim$y$vals - lat_farmer)),  # look for closest lat
                                              1),
                                     count = c(1,1,-1)) #count '-1' means 'all values along that dimension'
  # create dataframe
  datafinal <- data.frame(dates= obsdatadates, pevap_annual_mean = pevap_annual_mean, pevap_driest_quarter=pevap_driest_quarter, pevap_wettest_quarter=pevap_wettest_quarter)
  datafinal['year'] <- unlist(strsplit(format(datafinal$dates,'%Y'),'-'))
  datafinal['month'] <- unlist(strsplit(format(datafinal$dates,'%m'),'-'))
  # Then I create monthly data from annual mean, driest quarter and wettest quarter assuming that 
  # January to March is the wettest quarter, July to September is the driest quarter and April to June and Octover to December corresponds to the annual mean
  pevap_monthly_2000_2020 <- datafinal %>% filter(year=="2009") %>% mutate(pevap_monthly=list(c(rep(pevap_wettest_quarter,3),rep(pevap_annual_mean,3),rep(pevap_driest_quarter,3),rep(pevap_annual_mean,3))))
  pevap_monthly_2020_2040 <- datafinal %>% filter(year=="2030") %>% mutate(pevap_monthly=list(c(rep(pevap_wettest_quarter,3),rep(pevap_annual_mean,3),rep(pevap_driest_quarter,3),rep(pevap_annual_mean,3))))
  return(c(pevap_monthly_2000_2020$pevap_monthly,pevap_monthly_2020_2040$pevap_monthly))
}
