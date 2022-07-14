# SoilR functions
library(rjson)
library(SoilR)
library(rgee)
library(gdalUtils)      # some useful utilities for GDAL
library(readr)          # tidyverse functions to read files
library(sf, warn.conflicts = FALSE)             # Simple Features spatial data
library(sp)             # spatial data types in R
library(dplyr, warn.conflicts = FALSE)          # another way to handle tabular data
library(dbplyr, warn.conflicts = FALSE)    # databases from dplyr
library(DBI)            # R database interface
library(RSQLite)
library(rgdal)          # interface to GDAL Geographic Data Abstraction Language

# converts x to tonnes ----
convert_to_tonnes <- function(value, conversion_factor = 3.67){
  
  tonnes = value * conversion_factor
  
  return(tonnes)
  
}

# get_monthly_dataframe ----
# given a set of years, this function returns a dataset with the number of months
# that would occur on that time span. 
get_monthly_dataframe <- function(time_horizon = 10, add_month = T){
  
  years <- seq(1/12, time_horizon+1/12, by = 1/12)
  
  if (add_month == F){  years <- seq(1/12, time_horizon, by = 1/12)}
  
}

prep_soil_moisture_factor <- function(
  time_horizon, 
  bare = TRUE,   # This can be a logical, or a string of 12 logicals
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  pE=pE){
  
  # Monthly data frame
  years <- get_monthly_dataframe(time_horizon, add_month = F)
  
  # Calculate monthly temperature effects
  fT <- fT.RothC(temp) 
  
  # Calculate effect of coverage
  
  fCov = ifelse(bare == TRUE, 1, 0.6)
  
  if(length(bare) == 1){
    # Calculate monthly moisture effects
    fW <- fW.RothC(
      P = (precip), 
      E = (evap),
      S.Thick = soil_thick, 
      pClay = clay,
      pE = pE, 
      bare = bare
    )$b 
  }else if(length(bare) == 12){ 
    # Use the modified version if there is monthly variation in coverage
    fW <- fW.RothC.Modified(
      P = (precip), 
      E = (evap),
      S.Thick = soil_thick, 
      pClay = clay,
      pE = pE, 
      bare_profile = bare
    )$b 
  }
  
  # Moisture factors over time
  xi_frame <- data.frame(years, moisture_factor = rep(fT * fW * fCov, length.out = length(years)))
  return(xi_frame)
  
}

calc_soil_carbon <- function(
  time_horizon = 10,
  xi_frame,
  c_inputs = c_inputs,
  dr_ratio = 1.44,
  fym_inputs = 0,
  pE = 1.0,
  clay = 20,
  PS = c(DPM=0,RPM=0,BIO=0,HUM=0,IOM=0)
){
  # Monthly data frame
  years <- get_monthly_dataframe(time_horizon, add_month = F)
  
  # Loads the model
  Model <- RothCModel(
    t = years, 
    ks = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02, k.ION = 0),
    C0 = c(DPM = PS),
    In = c_inputs, 
    DR = dr_ratio,
    clay = clay, 
    xi = xi_frame
  ) 
  
  # Calculates stocks for each pool per month  
  c_t <- getC(Model) 
  
  c_t <- as_tibble(c_t)
  names(c_t) <- c("DPM", "RPM", "BIO", "HUM", "IOM")
  
  return(c_t)
  
}

normalise_c_inputs <- function(
  c_in = 0, 
  fym_in = 0,
  dr_ratio_crops = 1.44,
  dr_ratio_fym = 1){
  
  if(c_in + fym_in != 0){
    dr_ratio = (dr_ratio_crops*c_in + dr_ratio_fym*fym_in) / (c_in + fym_in)
  }else{
    dr_ratio = 1
  }  
  return(dr_ratio)
  
}

get_total_C <- function(c_df){
  
  final_row <- as.numeric(tail(c_df, 1))
  
  tot_value <- sum(final_row)
  
  return(tot_value)
  
}

get_initial_C <- function(c_df){
  
  initial_row <- as.numeric(head(c_df,1))
  
  init_value <- sum(initial_row)
  
  return(init_value)
  
}

estimate_starting_soil_content <- function(
  SOC = 1,
  clay = 1
){
  
  RPM = (0.1847 * SOC + 0.1555)*(clay + 1.2750)^-0.1158 
  HUM = (0.7148 * SOC + 0.5069)*(clay + 0.3421)^0.0184 
  BIO = (0.0140 * SOC + 0.0075)*(clay + 8.8473)^0.0567 
  
  FallIOM <- 0.049 * SOC^(1.139)
  
  starting_soc = c(0,RPM, BIO, HUM, FallIOM)
  
  return(starting_soc)
  
}


calc_tilling_factor <- function(
  climate_zone = "temperate moist",
  practice = "no till",
  tilling_factors = tilling_factors
){
  
  climate_zone <- tolower(climate_zone)
  
  if(!climate_zone %in% unique(tilling_factors$climate)){stop("Check climate zone in tilling factors")}
  if(!practice %in% unique(tilling_factors$new_practice)){stop("Check practice in tilling factors")}
  
  
  tilling_factor <- tilling_factors %>% 
    filter(climate == !!climate_zone,
           previous_practice == "conventional till",
           new_practice == !!practice) %>% 
    pull(factor)
  
  if(length(tilling_factor) > 1){stop("Tilling factor not unique")}
  
  return(tilling_factor)
  
}


calc_tilling_impact <- function(tilling_factor = 1, 
                                all_c){
  
  # Take the initial c values
  c_init <- as_tibble(head(all_c,1))
  
  # calculate the difference at each time point from t = 0
  all_c_diff <- all_c %>% 
    mutate_if(is.numeric, funs(.-first(.)))
  
  all_c_diff <- all_c_diff * tilling_factor
  
  all_c_diff <- all_c_diff %>% 
    mutate(DPM = DPM + c_init$DPM,
           RPM = RPM + c_init$RPM,
           HUM = HUM + c_init$HUM,
           BIO = BIO + c_init$BIO,
           IOM = IOM + c_init$IOM)
  
  return(all_c_diff)
  
}



calc_carbon_over_time <- function(time_horizon = 10, 
                                  field_carbon_in = c(10,10,10,10,10,10,10,10,10,10), # annual input
                                  dr_ratios = c(1,1,1,1,1,1,1,1,1), # annual dr ratio
                                  bare_profile = c(T,T,T,T,T,T,T,T,T,T,T,T), # monthly coverage value
                                  temp = temp,
                                  precip = precip,
                                  evap = evap,
                                  soil_thick = soil_thick,
                                  clay = clay,
                                  pE = pE,
                                  PS = starting_soil_content,
                                  tilling_factor = 1){
  
  
  
  if(length(dr_ratios) != length(field_carbon_in)){stop("Field_carbon_in and dr_ratios should have same length, 1 entry per year")}
  
  xi_frame <- prep_soil_moisture_factor(
    time_horizon = 1, 
    bare = bare_profile,   # This can be a logical, or a string of 12 logicals
    temp = temp,
    precip = precip,
    evap = evap,
    soil_thick = soil_thick,
    clay = clay,
    pE=pE)
  
  for (t in 1: time_horizon){
    
    c_in <- field_carbon_in[t]
    dr_in <- dr_ratios[t]
    
    # Runs the model for a single year, taking the inputs from the previous year as the SOC
    c_df <- calc_soil_carbon(
      time_horizon = 1,
      xi_frame,
      c_inputs = c_in,
      dr_ratio = dr_in,
      clay=clay,
      pE = pE,
      PS = PS
    )
    
    # Sets new starting_soil_content
    PS <- as.numeric(tail(c_df, 1))
    
    if(t == 1){
      all_c = c_df
    }else{
      all_c <- rbind(all_c, c_df)
    }
  }
  
  # apply tilling losses
  all_c <- calc_tilling_impact(tilling_factor, all_c)
  
  all_c <- all_c %>% rowwise() %>% mutate(TOT = sum(DPM, RPM, BIO, HUM, IOM))
  
  return(all_c)
}


get_bare_profile_single <- function(field_parameters){
  
  # input_parameters should be a single line of the input_parameter file
  
  ip0 <- field_parameters %>% select(contains("bare_profile"))
  
  if(ncol(ip0) != 12){stop("Missing information about the bare profile months. ")}
  
  bare_profile <- as.data.frame(t(ip0))$V1
  
  return(bare_profile)    
}

get_bare_profile_df <- function(field_parameters){
  
  ip <- field_parameters %>% 
    select(contains("bare_profile")) 
  
  bare_profile <- do.call(paste, c(ip[], sep = ", ")) 
  
  field_parameters <- field_parameters %>% 
    select(!contains("bare_profile")) %>% 
    cbind(bare_profile)
  
  return(field_parameters)
}





download_clean_Wosis <- function(wosis.dir.name=NULL, europe=TRUE){
  
  # Check if Wosis has already been downloaded
  # if it has not, download. 
  if(is.null(wosis.dir.name)) wosis.dir.name <- "./wosis2019" else wosis.dir.name<-wosis.dir.name
  
  if (!file.exists(wosis.dir.name)) dir.create(wosis.dir.name)
  zip.file.name <- "WoSIS_2019_September.zip"
  snapshot.zip <- paste0("https://files.isric.org/public/wosis_snapshot/", zip.file.name)
  target.zip <- paste0(wosis.dir.name, "/", zip.file.name)
  if (!file.exists(target.zip)) {
    download.file(snapshot.zip, destfile=target.zip)
  }
  
  if (!file.exists(paste0(wosis.dir.name, "/wosis_201909.gpkg"))) {
    system.time(unzip(target.zip, exdir=wosis.dir.name, junkpaths=TRUE))  
  }
  
  # load profile information from WoSIS    
  profiles <- read_tsv(paste0(wosis.dir.name, "/wosis_201909_profiles.tsv"))
  # profiles %>% select(profile_id, country_id, longitude, latitude, geom_accuracy, dsds)
  
  attributes <- read.table(paste0(wosis.dir.name, "/wosis_201909_attributes.tsv"),
                           header=TRUE,
                           sep="\t",
                           stringsAsFactors=FALSE)
  
  ix <- grep("Phosphorus", attributes$attribute)
  bd <- grep("Bulk", attributes$attribute)
  st <- grep("Soil", attributes$attribute)
  org <- grep("carbon", attributes$attribute)
  
  # Input physical and chemical layers ----
  physical <- readr::read_tsv(paste0(wosis.dir.name, "/wosis_201909_layers_physical.tsv"),
                              col_types="iiddclcdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccc")
  chemical <- readr::read_tsv(paste0(wosis.dir.name, "/wosis_201909_layers_chemical.tsv"),
                              col_types="iiddclcdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccccdccccc")
  
  
  (profiles.sel <- dplyr::filter(profiles) %>%
      select(profile_id, longitude, latitude, dsds, cfao_major_group))
  
  (layers.sel <- left_join(profiles.sel, physical) %>% 
     select(profile_id, upper_depth, latitude, longitude, dsds:layer_name,sand_value_avg, silt_value_avg, clay_value_avg,
            bdfiod_value_avg, bdfiod_method))
  layers.sel<-left_join(layers.sel, chemical[ , c("profile_id","orgc_value_avg","totc_value_avg")], by="profile_id") 
  layers.sel<-as.data.frame(layers.sel)
  
  # 2. Clean WoSIS database ----
  
  
  # Organic Carbon was measured in many more points than Total carbon. 
  # We select only those points with totalC values. 
  layers.sel<-layers.sel[!is.na(layers.sel$orgc_value_avg ) & !is.na(layers.sel$clay_value_avg ), ]
  
  # orgc_value_avg is in g/km 1/1000g == 100/1000
  layers.sel$orgc_value_avg<-layers.sel$orgc_value_avg*0.1
  
  # Limit results to those measured from the surface
  layers.sel.0<-layers.sel[layers.sel$upper_depth==0 & !is.na(layers.sel$upper_depth), ]
  
  # Remove duplicates
  layers.sel.uq<-distinct(layers.sel.0, profile_id ,lower_depth, clay_value_avg,orgc_value_avg, .keep_all= TRUE)
  # Limit to Europe
  if (europe==TRUE){ 
    eu.lat  <-layers.sel.uq$latitude>30 & layers.sel.uq$latitude<75 
    eu.long <-layers.sel.uq$longitude>=-20 & layers.sel.uq$longitude<50
    layers.sel.uq<-layers.sel.uq[eu.lat & eu.long, ]
  }
  
  # This loop averages several values of orgc_value_avg for a given profile. Equal depths.  
  spp<-data.frame(layers.sel.uq[1,])
  profile_ids<-unique(layers.sel.uq$profile_id)
  for (i in 1:length(profile_ids)){
    ss<-layers.sel.uq[layers.sel.uq$profile_id==profile_ids[i], ]
    spp[i, 1:16]<-ss[1, ]
    spp[i,"orgc_value_avg"]<-mean(ss$orgc_value_avg)
    spp$orgc_sd[i]<-sd(ss$orgc_value_avg)
    
  }
  
  # Bulk density and carbon stock ----   
  # we applied an adapted version of the Adams-Stewart conceptual model developed by 
  # Tranter et al. (2007):# Organic matter = organic carbon x1.72 (roughly)
  # The calibration of the Tranter et al (2007) for agricultural soils was done by 
  #  https://doi.org/10.1111/j.1365-2389.2011.01412.x
  
  LOI <- spp$orgc_value_avg*1.72
  x<-spp$sand_value_avg
  y<-100-(spp$clay+spp$silt_value_avg)
  
  
  mineraldensity<- 1.3406 + 0.0031*y + ((38.6530-y)^2)*0.000021 - 0.05880*log(spp$lower_depth)
  bulkdensity<-100/((LOI/0.224) + ((100 - LOI)/mineraldensity))
  
  # This is in g/100g
  
  # Carbon stock calculated as: 
  carbonstock<- 10000*(spp$lower_depth/100) * bulkdensity * (spp$orgc_value_avg/100)
  
  names(spp)
  spp$cstock_t.ha <- carbonstock
  spp$bulkdensity_calc <- bulkdensity
  write.csv(spp, paste0(wosis.dir.name, "/wosis_cleaned.csv"))
  spp
  
}

overlap_wosis_envz<-function(wosis.download=FALSE, wosis.dir.name=NULL, soil_loc, tech_loc){
  # If the clean wosis file doesn't exist, trigger a warning to run download_clean_wosis 
  if(file.exists(paste0(tech_loc, "/Modelling/Validation_data/wosisEnvZ.csv"))) warning(paste("A file containing an overlap between wosis and the ESA environmental zones exists in", paste0(tech.dir, "/Modelling/Validation_data/wosisEnvZ.csv")))
  if(!file.exists(paste0(wosis.dir.name, "/wosis_cleaned.csv"))) warning("Cleaned version of WoSIS cannot be found in wosis.dir.name; adjust directory, or download and clean wosis (download_clean_wosis)")
  wosis<-read.csv(paste0(wosis.dir.name, "/wosis_cleaned.csv")) #check
  
  # Check environmental zones for Wosis profiles ----
  # loading the function is not necessary if stored together.  
  load(paste(soil_loc, "/scripts/Climatic_zone_check_function.R", sep="")) 
  
  clim.zone <- st_read(paste(tech_loc, "Modelling/Climatic_zone/EnSv8/ens_v8.shp", sep=""), stringsAsFactors=FALSE)
  
  for (i in 1:dim(wosis)[1]){
    df<-data.frame(x=wosis[i,5], y=wosis[i,4])
    wosis$environmentalzone[i]<-clime.zone.check(clim.zone=clim.zone, coord=df)}
  
  # Some NAs because sites outside of Europe don't have an EnvZ in the map
  wosis<-wosis[!is.na(wosis$environmentalzone),]
  # print file
  wosis}

# for initialisation we use data from Wosis, that is expressed in 25cm. 
# we compare this to data from woris, in the same depth. 

# then we have the historical data. We take the initial C, as predicted by initialise model. 
# that is for 25cm. We then compare to 25cms. 

# is the input from dt used anywhere in the model? 

intialise.model.nv<-function(latitude, 
                             longitude, 
                             clay, 
                             soil_loc, 
                             modelling_data_loc, 
                             tech_loc, 
                             dr_ratio=0.67, 
                             soiltype="all",
                             EnvZ=NULL,
                             weather_data=NULL){
  # read in wosis, env.zone and land cover file
  source(file.path(soil_loc, "modified_functions.R"))
  source(file.path(soil_loc, "scripts/calc_functions_soil_modelling.R"))
  # source(file.path(soil_loc, "scripts/Climatic_zone_check_function.R"))
  source(file.path(modelling_data_loc, "scripts/weather_data_pulling_functions.R"))
  
  weatherDB_loc<-paste0(tech_loc, "/Modelling/WeatherDB")  
  if (is.null(weather_data)) {
    weather_data = data.frame(past_temperature=rep(NA,12))
    weather_data[,c("past_temperature", "future_temperature_rcp4.5")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer, scenario="rcp4.5")
    weather_data[,c("past_precipitation", "future_precipitation_rcp4.5")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer,scenario="rcp4.5")
    weather_data[,c("past_pevap", "future_pevap_rcp4.5")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer,scenario="rcp4.5")}
  
  
  wosis<-read.csv(paste0(tech_loc, "/Modelling/Validation_data/wosisEnvZLC.csv"))
  if (is.null(EnvZ)){
  EnvZ <-clime.zone.check(paste0(tech_loc, "/Modelling/Climatic_zone"), longitude, latitude)}
  if (sum(wosis$environmentalzone==EnvZ)<10) stop("There are fewer than 10 record available in the given environmental zone, use a broader environmental zone")
  wosis<-wosis[wosis$environmentalzone==EnvZ, ]

  if(sum(wosis$land_use_cover==">75 - Forest")>10) {
    forest.sel<- wosis$land_use_cover==">75 - Forest"} else {
      forest.sel<- wosis$land_use_cover==">75 - Forest" | wosis$land_use_cover=="50-75 - Forest"
    }

  validation.set <- wosis[forest.sel,] #attention
  
  # Phase 1 - model initialization ----
  validation.set<-validation.set[validation.set$lower_depth>10, ]
  validation.set$cstock25<-validation.set$cstock_t.ha*25/validation.set$lower_depth
  validation.set<-validation.set[!is.na(validation.set$cstock25), ]
  
  # if soil type is specified, then subsets accordingly, else subsets according to clay content 
  if (soiltype=="all")  validation.set<-validation.set[ order(abs(validation.set$clay_value_avg-clay))[1:10], ] else{   
    validation.set<-validation.set[validation.set$cfao_major_group==soiltype,]
    if (sum(validation.set$cfao_major_group==soiltype & !is.na(validation.set$cfao_major_group))<10) warning("fewer than 10 profiles available infor the specified soil type")
    }

  
  tree.clay=median(validation.set$clay_value_avg)
  SOC_nveg= median(validation.set$cstock25)
  clay.sd = sd(validation.set$clay_value_avg)
  cstock.sd= sd(validation.set$cstock25)
  dr_ratio_forest = 0.25
  dr_ratios_savanna = 0.67
  time_horizon = 1000
  
  FallIOM=0.049*SOC_nveg^(1.139) #IOM using Falloon method
  # TO CHECK: CODE USED CLAY; i CHAGED IT TO TREE.CLAY 
  # We use pedotransfer functions for compartment equilibrium 
  # From Weihermuller et al 2013
  RPM = (0.1847*SOC_nveg+0.1555)*((tree.clay+1.2750)^-0.1158)
  HUM = (0.7148*SOC_nveg+0.5069)*((tree.clay+0.3421)^0.0184)
  BIO = (0.0140*SOC_nveg+0.0075)*((tree.clay+8.8473)^0.0567)
  # And let the model get to equilibrium 
  
  
  # Phase 1 - solve for C inputs ---- 
  # we model 10 C inputs. and plot them
  mean_input<-data.frame(run=rep(0,12), 
                         dr_ratios=c(0.67,rep(NA,11)), 
                         bare=as.factor(c(logical(12))), 
                         past_temp=weather_data$past_temperature, 
                         past_precip=weather_data$past_precipitation, 
                         past_evap=weather_data$past_pevap, 
                         soil_thick=c(25,rep(NA,11)), 
                         clay=c(clay,rep(NA,11)), 
                         pE=c(0.75,rep(NA,11)), 
                         tilling_factor=c(1.0,rep(NA,11)))
  
  
  
  c_cinput_balance<-data.frame(matrix(nrow=2, ncol=2))
  names(c_cinput_balance)<-c("carboninput", "carbonstock")
  c_cinput_balance$carboninput=c(10, 20)   #Annual C inputs to soil in Mg/ha/yr
  pedotransfer_ini_soil_content=c(DPM=0, RPM=RPM, BIO=BIO, HUM=HUM, IOM=FallIOM) # this step might not be necessary Attention: experiment whether having it provides any better results than not having it. 
  
  model1 <- calc_carbon_over_time(time_horizon,
                                  field_carbon_in = rep(c_cinput_balance[1,1],time_horizon),
                                  dr_ratios = rep(dr_ratio_forest,time_horizon),
                                  bare = mean_input$bare,
                                  temp = mean_input$past_temp,
                                  precip = mean_input$past_precip,
                                  evap = mean_input$past_evap,
                                  soil_thick = mean_input$soil_thick[1],
                                  clay = mean_input$clay[1],
                                  pE = mean_input$pE[1],
                                  PS = pedotransfer_ini_soil_content,
                                  tilling_factor = mean_input$tilling_factor[1])
  c_cinput_balance[1,"carbonstock"]<-tail(model1$TOT, 1)
  
  model2 <- calc_carbon_over_time(time_horizon,
                                  field_carbon_in = rep(c_cinput_balance[2,1],time_horizon),
                                  dr_ratios = rep(dr_ratio_forest,time_horizon),
                                  bare = mean_input$bare,
                                  temp = mean_input$past_temp,
                                  precip = mean_input$past_precip,
                                  evap = mean_input$past_evap,
                                  soil_thick = mean_input$soil_thick[1],
                                  clay = mean_input$clay[1],
                                  pE = mean_input$pE[1],
                                  PS = pedotransfer_ini_soil_content,
                                  tilling_factor = mean_input$tilling_factor[1])
  c_cinput_balance[2,"carbonstock"]<-tail(model2$TOT, 1)
  
  slope=(c_cinput_balance[2,2]-c_cinput_balance[1,2])/(c_cinput_balance[2,1]-c_cinput_balance[1,1])
  Cinput_leading_to_observed_SOC_past_land_use = SOC_nveg/slope
  
  # Natural area initialisation ---- 
  mean_input$field_carbon_in <- rep(Cinput_leading_to_observed_SOC_past_land_use,12) # not sure why the mean input data frame exists. 
  starting_soil_content <- estimate_starting_soil_content(SOC=Cinput_leading_to_observed_SOC_past_land_use,clay=mean_input$clay[1])
  time_horizon = 1000
  
  
  C0_df <- calc_carbon_over_time(time_horizon,
                                 field_carbon_in = rep(mean_input$field_carbon_in[1],time_horizon), 
                                 dr_ratios = rep(dr_ratio_forest,time_horizon),
                                 bare = mean_input$bare,
                                 temp = mean_input$past_temp,
                                 precip = mean_input$past_precip,
                                 evap = mean_input$past_evap,
                                 soil_thick = mean_input$soil_thick[1],
                                 clay = mean_input$clay[1],
                                 pE = mean_input$pE[1],
                                 PS = starting_soil_content,
                                 tilling_factor = mean_input$tilling_factor[1])
  pool.distribution.t0 <- as.numeric(tail(C0_df,1))[c(1:5)]
  simulatedC.t0<- as.numeric(tail(C0_df,1))[6]
  res<-list(pool.distribution.t0, simulatedC.t0, SOC_nveg, Cinput_leading_to_observed_SOC_past_land_use, weather_data)
  names(res)<-c("Carbon distribution in different pools","simulated total carbon after 1000 years", "median observed total C", "Carbon input in nature", "weather predictions")
  res
}







#currently this is only one experiment, but we might need to correct this when more are added. 
# what about an inverse logarithmic loss? where the loss of C inputs is not linear?


historical.land.use.M<-function(pool.distribution.t0, 
                                Cinput.nature, 
                                weather_data=NULL, 
                                dt, 
                                tech_loc, 
                                soil_loc, 
                                modelling_data_loc, 
                                EnvZ=c("Mediterranean south", "Mediterranean north", "Mediterranean mountains"), 
                                current.land.use){
  if(is.null(EnvZ) | length(EnvZ)>1) stop("Specify a subzone of the mediterranean zone: Mediterranean south, north or mountain")
  
  source(file.path(soil_loc, "modified_functions.R"))
  source(file.path(soil_loc, "scripts/calc_functions_soil_modelling.R"))
  source(file.path(modelling_data_loc, "scripts/weather_data_pulling_functions.R"))
  
  # Probably need to add the source for WoSIS.   
  
  lat_farmer <- dt$latitude [1]
  lon_farmer <- dt$longitude [1]
  clay_c     <- dt$clay_content[1] # TO BE PICKED FROM YOUR DATA
  lower.depth <- dt$lower_depth_cm[1]
  # if object exists in space:  don't run 
  
  if (is.null(weather_data)) {
    weatherDB_loc<-paste0(tech_loc, "/Modelling/WeatherDB")  
    weather_data = data.frame(past_temperature=rep(NA,12))
    weather_data[,c("past_temperature", "future_temperature_rcp4.5")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer, scenario="rcp4.5")
    weather_data[,c("past_precipitation", "future_precipitation_rcp4.5")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer,scenario="rcp4.5")
    weather_data[,c("past_pevap", "future_pevap_rcp4.5")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer,scenario="rcp4.5")}
  
  parcel_Cinputs <- read.csv(paste0(tech_loc, "/Modelling/Validation_data/historical_land_use.csv"))
  if (sum(parcel_Cinputs$environmentalzone==EnvZ)==0) {
    parcel_Cinputs <- parcel_Cinputs[grep("Mediterranean", parcel_Cinputs$environmentalzone), ]
    cinputcols<-c("add_manure_Cinputs", "agroforestry_Cinputs", "animal_Cinputs", "crop_Cinputs", "pasture_Cinputs", "tot_Cinputs")
    agrCinputs<-aggregate(parcel_Cinputs[,cinputcols], list(parcel_Cinputs$parcel_ID), FUN=mean)
    parcel_characteristics<-data.frame(matrix(nrow=length(unique(parcel_Cinputs$parcel_ID)), ncol=6))
    names(parcel_characteristics)<-c("parcel_ID","dr_ratio", "tillage", "bare", "year_t0", "year_tend")
    for (i in 1:length(agrCinputs$Group.1)){
      sel<-parcel_Cinputs[parcel_Cinputs$parcel_ID==agrCinputs$Group.1[i], names(parcel_characteristics)]
      parcel_characteristics[i,] <- sel[1, ]}

    parcel_Cinputs <- cbind(agrCinputs, parcel_characteristics[,2:6] )
    names(parcel_Cinputs)[1]<-"parcel_ID"
  } else {
  parcel_Cinputs <- parcel_Cinputs[parcel_Cinputs$environmentalzone==EnvZ, ] }
  
  
  
  # All inputs for initialisation ----  
  ################# Initialisation by making the model reach SOC of natural areas of the pedo-climatic area
  batch<-data.frame(run=rep(0,12), 
                    dr_ratios=c(0.67,rep(NA,11)), 
                    bare=as.factor(c(logical(12))), 
                    past_temp=weather_data$past_temperature, 
                    past_precip=weather_data$past_precipitation, 
                    past_evap=weather_data$past_pevap, 
                    soil_thick=c(25,rep(NA,11)), 
                    clay=c(clay_c,rep(NA,11)), 
                    pE=c(0.75,rep(NA,11)), 
                    tilling_factor=c(1.0,rep(NA,11)))
  
  initialized_soil_content <- pool.distribution.t0 
  
  # Mediterranean areas were cork forests to begin with, with decreasing inputs
  # later on from input at nature to input at baseline.
  # Attention: Instead of using C input at T0, I used C input at agroforestry. 
  # makes more sense. 
  cinput_t350 <- parcel_Cinputs$tot_Cinputs[parcel_Cinputs$parcel_ID==current.land.use]
  # our assessment of Cinputs at the baseline may be waaay too low! 
  
  
  # Anthropic influence starts
  time_horizon = 350
  for(i in c(1:time_horizon)){
    batch$field_carbon_in <- (time_horizon-i)/time_horizon*Cinput.nature +
      i/time_horizon*cinput_t350
    C0_df <- calc_carbon_over_time(1,
                                   field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                   dr_ratio = rep(0.67,time_horizon),
                                   bare = batch$bare,
                                   temp = batch$past_temp,
                                   precip = batch$past_precip,
                                   evap = batch$past_evap,
                                   soil_thick = batch$soil_thick[1],
                                   clay = batch$clay[1],
                                   pE = batch$pE[1],
                                   PS = initialized_soil_content,
                                   tilling_factor = batch$tilling_factor[1])
    initialized_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
  }
  
  # This C input is half of the minimum reported in the experiment! 
  
  cinput_t0 <- parcel_Cinputs$tot_Cinputs[parcel_Cinputs$parcel_ID==current.land.use]
  dr_ratio <- parcel_Cinputs$dr_ratio[parcel_Cinputs$parcel_ID==current.land.use]
  bare0 <- parcel_Cinputs$bare[parcel_Cinputs$parcel_ID==current.land.use]
  bare <- as.logical(strsplit(bare0, ";")[[1]])
  
  time_horizon = min(dt$year_t0)-1950 # change to reflect beginning of experiment. 
  
  C0_df <- calc_carbon_over_time(time_horizon,
                                 field_carbon_in = rep(cinput_t0,time_horizon),
                                 dr_ratios = rep(batch$dr_ratios[1],time_horizon),
                                 bare = bare,
                                 temp = batch$past_temp,
                                 precip = batch$past_precip,
                                 evap = batch$past_evap,
                                 soil_thick = batch$soil_thick[1],
                                 clay = batch$clay[1],
                                 pE = batch$pE[1],
                                 PS = initialized_soil_content,
                                 tilling_factor = batch$tilling_factor[1]) # Attention: change: tilling occurs at least 1/2 times a year. 
  predictedCt0 <- as.numeric(tail(C0_df,1))[c(1:6)] 
  names(predictedCt0)<-names(C0_df)
  ObservedC<-(dt$toc_end*25/lower.depth)[1]
  res<-list(predictedCt0, ObservedC, weather_data)
  names(res)<-c("Carbon stock predicted at time 0", "Carbon observed at time 0", "weather data")
  message("Calculations were made using the first row. If this is not the control, please select the right row before running the function")
  res}


Cstock_in_time_catchC <- function(pool.distribution.t0, 
                                  weather_data=NULL,
                                  dt,
                                  tilling.factor=1,
                                  year.last.measurement,
                                  soil_loc, tech_loc, 
                                  modelling_data_loc){
  
  # Weather inputs ---- 
  source(file.path(soil_loc, "modified_functions.R"))
  source(file.path(soil_loc, "scripts/calc_functions_soil_modelling.R"))
  source(file.path(modelling_data_loc, "scripts/weather_data_pulling_functions.R"))
  
  # Probably need to add the source for WoSIS.   
  
  lat_farmer <- dt$latitude [1]
  lon_farmer <- dt$longitude[1]
  clay_c     <- dt$clay_content [1] # TO BE PICKED FROM YOUR DATA
  lower.depth <- dt$lower_depth_cm[1]
  
  if (is.null(weather_data)) {
    weatherDB_loc<-paste0(tech_loc, "/Modelling/WeatherDB")  
    weather_data = data.frame(past_temperature=rep(NA,12))
    weather_data[,c("past_temperature", "future_temperature_rcp4.5")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer, scenario="rcp4.5")
    weather_data[,c("past_precipitation", "future_precipitation_rcp4.5")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer,scenario="rcp4.5")
    weather_data[,c("past_pevap", "future_pevap_rcp4.5")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer,scenario="rcp4.5")}
  
  # All inputs for initialisation ----  
  ################# Initialisation by making the model reach SOC of natural areas of the pedo-climatic area
  batch<-data.frame(run=rep(0,12),
                         dr_ratios=c(dt$dpm_rpm_future,rep(NA,11)),
                         bare=as.factor(c(logical(12))),
                         past_temp=weather_data$past_temperature,
                         past_precip=weather_data$past_precipitation,
                         past_evap=weather_data$past_pevap,
                         soil_thick=c(25,rep(NA,11)),
                         clay=c(clay_c,rep(NA,11)),
                         pE=c(0.75,rep(NA,11)),
                         tilling_factor=c(1.0,rep(NA,11)))

  # Here is where we divide results for arable farms and grasslands    

  cinput <- sum(dt[ ,grep("c_input", names(dt))])
  initialized_soil_content <- pool.distribution.t0
  bare0 <- dt$soil_cover
  bare <- as.logical(strsplit(bare0, ";")[[1]])
  time_horizon = dt$datetime-year.last.measurement # Attention - previous measuring time, or time 0 needs to exist

      C0_df <- calc_carbon_over_time(time_horizon,
                                     field_carbon_in = rep(cinput,time_horizon),
                                     dr_ratios = rep(batch$dr_ratios[1],time_horizon),
                                     bare = bare,
                                     temp = batch$past_temp,
                                     precip = batch$past_precip,
                                     evap = batch$past_evap,
                                     soil_thick = batch$soil_thick[1],
                                     clay = batch$clay[1],
                                     pE = batch$pE[1],
                                     PS = initialized_soil_content,
                                     tilling_factor = batch$tilling_factor[1]) # Attention: change: tilling occurs at least 1/2 times a year. 
      
      
  c.prediction <-  as.numeric(tail(C0_df,1))[c(1:6)]
  names(c.prediction)<-names(C0_df)

  ObservedC<-(dt$toc_end*25/lower.depth)[1]
  
  res<-list(c.prediction, ObservedC, weather_data)
  names(res)<-c("Carbon stock predicted at time 1", "Carbon observed at time 0", "weather data")
  message("C prediction was done for the first row only")
  res
}

library(sf)

clime.zone.check<-function(climatic_zone_loc, lon_farmer, lat_farmer){
  # Function to extract the Environmental zone at a given coordinate ----
  # Inputs are the local modelling-data repo folder path and longitude and latitude of the location considered. 
  clim.zone.map <- st_read(file.path(climatic_zone_loc,"EnSv8/ens_v8.shp"), stringsAsFactors=FALSE)
  enz.name<-c("Alpine north", "Boreal", "Nemoral", "Atlantic north", "Alpine south", "Continental", "Atlantic central", 
              "Pannonian", "Lusitanian", "Anatolian", "Mediterranean mountains", "Mediterranean north", "Mediterranean south")
  # The map requires transformations to prevent error messages. 
  clim.zone_ll <- sf::st_transform(clim.zone.map, crs = 3857) 
  clim.zone_ll <- sf::st_transform(clim.zone_ll, crs = 4326) 
  # points are transformed to the same projection
  coord <- data.frame(x=lon_farmer, y=lat_farmer)
  coords_t1 <- st_as_sf(coord, coords = c("x", "y"), crs= 4326)
  
  # st assumes planar, so one last transformation to planar
  coords_planar <-st_transform(coords_t1, 2163) 
  clim.zone_planar <- sf::st_transform(clim.zone_ll, crs = 2163) 
  
  EnZ.class <- st_join(coords_planar, clim.zone_planar)$EnZ 
  enz.name[EnZ.class] 
  
}



