# Code to run bias estimation 

# Description of the script ----
## Dependencies
## model_functions.R
## modified_functions.R
## scripts/calc_functions_soil_modelling.R
## scripts/weather_data_pulling_functions.R
## Weather database should be accessible. 
## json file should have all paths, including correct path for database. 
## Data with farmer information - must have the following columns: 
## latitude, longitude 
## C inputs are taken from modelling the baseline. 
## G:\Shared drives\Climate Farmers\02_Product\01_02_Transition_Finance\Carbon_Credits\FarmerData\Francisco Alves\result\parcel_Cinput.csv


# Future amends -----
########################### AUTOMATED SOIL MODEL RUNNING SCRIPT
# Missing automated pull of DPM/RPM ratio from input (aboveground crop/pasture/root exudates) type
# Missing automated pull of bare soil
# Unsure about use of soil depth in model: no effect on sensitivity analysis
# Missing automated pull of clay content: it is not set where we will find it
# Missing automated pull of standard errors for input parameters
# Missing observed SOC values for bias analysis
# Missing change RothC parameters to semi-arid according to Farina et al. 2013

library(rjson)

# Function ----
soil_loc <- "C:/Users/vazqu003/OneDrive - Wageningen University & Research/Climate farmers/GithubForks/soil-modelling"
project_loc <- "G:/Shared drives/Climate Farmers/02_Product/01_02_Transition_Finance/Carbon_Credits/FarmerData"
# Check the loc for weather DB. 
project_name<- "/Francisco Alves"
modelling_data_loc <- "C:/Users/vazqu003/OneDrive - Wageningen University & Research/Climate farmers/GithubForks/modelling-data"
weatherDB_loc <- "G:/Shared drives/Climate Farmers/07_Tech/Modelling/WeatherDB/sis-biodiversity-cmip5-regional_data"

run_soil_model <- function(soil_loc,project_loc,project_name,modelling_data_loc,weatherDB_loc, lat, long, land_use){
  
  source(file.path(soil_loc, "model_functions.R"))
  source(file.path(soil_loc, "modified_functions.R"))
  source(file.path(soil_loc, "scripts/calc_functions_soil_modelling.R"))
  source(file.path(modelling_data_loc, "scripts/weather_data_pulling_functions.R"))
  
  
  lat_farmer <- lat # SELECT LAT
  lon_farmer <- long # SELECT LON
  
  ################# Weather data pulling, YOU NEED ONLY PAST WEATHER
  
  weather_data = data.frame(past_temperature=rep(NA,12))
  
  weather_data[,c("past_temperature", "future_temperature_rcp4.5")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer, scenario="rcp4.5")
  weather_data[,c("past_precipitation", "future_precipitation_rcp4.5")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer,scenario="rcp4.5")
  weather_data[,c("past_pevap", "future_pevap_rcp4.5")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer,scenario="rcp4.5")
  
  ############################################ HERE IS C INPUTS CALC <- I WOULD BETTER PROVIDE YOU A NUMBER DIRECTLY
  

  # ################# Pulling calculation factors
  # 
  # animal_factors <- read_csv(file.path(modelling_data_loc,"data", "carbon_share_manure.csv")) %>% filter(type=="manure") %>% 
  #   rename(species=manure_source)
  # agroforestry_factors <- read_csv(file.path(modelling_data_loc,"data", "agroforestry_factors.csv")) 
  # crop_data <- read_csv(file.path(modelling_data_loc,"data", "crop_factors.csv"))#, col_types =  "cdddddddd")
  # pasture_data <- read_csv(file.path(modelling_data_loc,"data", "pasture_factors.csv"))
  # 
  # ################# Pulling calculation inputs
  # animal_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "animal_inputs.csv"))
  # agroforestry_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "agroforestry_inputs.csv"))
  # crop_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "crop_inputs.csv"))
  # pasture_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "pasture_inputs.csv"))
  # parcel_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "parcel_inputs.csv"))
  # ################# Calculations per parcel and scenario
  # 
  # parcel_Cinputs =data.frame(parcel_ID=c(),scenario=c(),agroforestry_Cinput=c(),animal_Cinput=c(),crop_Cinputs=c(),pasture_Cinputs=c())
  # for(parcel in parcel_inputs$parcel_ID){
  #   for(scenario in c("current","future","baseline")){
  #     parcel_Cinputs<-rbind(parcel_Cinputs,data.frame(parcel_ID=parcel,
  #                                                     scenario=scenario,
  #                                                     agroforestry_Cinput=get_monthly_Cinputs_agroforestry(agroforestry_inputs, agroforestry_factors, 
  #                                                                                                          scenario, parcel, lat_farmer),
  #                                                     animal_Cinput=get_monthly_Cinputs_animals(animal_inputs, animal_factors, scenario, parcel),
  #                                                     crop_Cinputs=get_monthly_Cinputs_crop(crop_inputs, crop_data, scenario, parcel),
  #                                                     pasture_Cinputs=get_monthly_Cinputs_pasture(pasture_inputs, pasture_data, scenario, parcel)))
  #   }
  # }
  # parcel_Cinputs <- parcel_Cinputs %>% mutate(tot_Cinputs=agroforestry_Cinput+animal_Cinput+crop_Cinputs+pasture_Cinputs)
  # 

  # 
  Cinput_baseline = 2 # TO BE DETERMINED
    
  ################# Initialisation by making the model reach SOC of natural areas of the pedo-climatic area
  
  clay_c = 15 # TO BE PICKED FROM YOUR DATA
  
  mean=c(list(rep(0,12)),
         list(c(0.67,rep(NA,11))),
         list(as.factor(c(logical(12)))),
         list(weather_data$past_temperature-273.15),
         list(weather_data$past_precipitation*365*30.4*24*3600),
         list(weather_data$past_pevap*365*30.4*24*3600),
         list(c(30,rep(NA,11))),
         list(c(clay_c,rep(NA,11))), 
         list(c(0.75,rep(NA,11))),
         list(c(1.0,rep(NA,11))))
  colnames_ranges=c("run","dr_ratios","bare","past_temp","past_precip","past_evap","soil_thick","clay","pE","tilling_factor")
  mean_input = data.frame(mean)
  colnames(mean_input) = colnames_ranges
  
  # Studying the BIAS in the Roth C model
  # Read in management files ----
  # We need a table with input values for tree-covered areas
  #                      input values for cropland
  #                      input values for grassland
  #                      input values for shrubland (?)
  
  # It needs the "forest" inputs as well in order to have proper initialisation. 

  LU<-read.csv(file.path(modelling_data_loc,"data", "/soil_landuse.portugal.csv"))
  LU<-LU[LU$latitude < 39.5,] # Roughly where dry temperate weather starts in portugal 
  table(LU$landcover)
  cropland.vs <- LU[LU$landcover=="Cropland", ]
  tree.vs     <- LU[LU$landcover=="Tree cover", ]
  grassland.vs<- LU[LU$landcover=="Grassland", ]

  # Phase 1 - model initialization ----
  # I could average all "tree cover" areas. I don't need to validate these. 
  # The baseline scenario for calibration should be ind. from the baseline sc. in validation
  # Phase 1 - a) Understanding input data ----
  tree.vs<-tree.vs[tree.vs$lower_depth>10, ]
  cstock25<-tree.vs$cstock_t.ha*25/tree.vs$lower_depth
  clay=median(tree.vs$clay_value_avg)        #Percent clay
  depth = 25
  cstock= median(cstock25)
  clay.sd = sd(tree.vs$clay_value_avg)
  cstock.sd= sd(cstock25)
  median(cstock25[tree.vs$clay_value_avg>10.3 & tree.vs$clay_value_avg<16.3])

  FallIOM=0.049*cstock^(1.139) #IOM using Falloon method
  DMP_RPM<- 0.25 # For forests
  # Two approaches: We use C input calculated by paper
  # We use pedotransfer functions for compartment equilibrium 
  # From Weihermuller et al 2013
  RPM = (0.1847*cstock+0.1555)*((clay+1.2750)^-0.1158)
  HUM = (0.7148*cstock+0.5069)*((clay+0.3421)^0.0184)
  BIO = (0.0140*cstock+0.0075)*((clay+8.8473)^0.0567)
  # And let the model get to equilibrium 
  soil.thick=25  #Soil thickness (organic layer topsoil), in cm
  SOC_nveg=median(cstock25)       #Soil organic carbon in Mg/ha 
  dr_ratio_forest = 0.25
  dr_ratios_savanna = 0.67
  time_horizon = 1000
  ##### TO DO : change RothC parameters to semi-arid according to Farina et al. 2013

    # d) Phase 1 - solve for C inputs ---- 
  # we model 10 C inputs. and plot them
  Cinputs=c(10, 20)   #Annual C inputs to soil in Mg/ha/yr
  c_cinput_balance<-data.frame(matrix(nrow=length(Cinputs), ncol=2))
  names(c_cinput_balance)<-c("carboninput", "carbonstock")
  pedotransfer_ini_soil_content=c(DPM=0, RPM=median(RPM), BIO=median(BIO), HUM=median(HUM), IOM=median(FallIOM))
  for(i in 1:length(Cinputs)){
    field_carbon_in=Cinputs[i]
    c_cinput_balance[i,"carboninput"]<-Cinputs[i]
    model1 <- calc_carbon_over_time(time_horizon,
                                    field_carbon_in = rep(field_carbon_in,time_horizon),
                                    dr_ratios = rep(dr_ratio_forest,time_horizon),
                                    bare = mean_input$bare,
                                    temp = mean_input$past_temp,
                                    precip = mean_input$past_precip,
                                    evap = mean_input$past_evap,
                                    soil_thick = soil.thick,
                                    clay = mean_input$clay[1],
                                    pE = mean_input$pE[1],
                                    PS = pedotransfer_ini_soil_content,
                                    tilling_factor = mean_input$tilling_factor[1])
    end_stock=tail(model1$TOT, 1) #Calculates stocks for each pool per month
    c_cinput_balance[i,"carbonstock"]<-end_stock
  }
  plot(c_cinput_balance$carboninput, c_cinput_balance$carbonstock)
  slope=(c_cinput_balance[2,2]-c_cinput_balance[1,2])/(c_cinput_balance[2,1]-c_cinput_balance[1,1])
  
  Cinput_leading_to_observed_SOC_past_land_use = SOC_nveg/slope
  
 # Initialisation
  mean_input$field_carbon_in <- rep(Cinput_baseline,12)
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
  print(tail(C0_df,1))
  nveg_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
  
  mean_input$field_carbon_in <- 0.5*(Cinput_leading_to_observed_SOC_past_land_use+Cinput_baseline)
  time_horizon = 350
  
  C0_df <- calc_carbon_over_time(time_horizon,
                                 field_carbon_in = rep(mean_input$field_carbon_in[1],time_horizon),
                                 dr_ratios = rep(dr_ratios_savanna,time_horizon),
                                 bare = mean_input$bare,
                                 temp = mean_input$past_temp,
                                 precip = mean_input$past_precip,
                                 evap = mean_input$past_evap,
                                 soil_thick = mean_input$soil_thick[1],
                                 clay = mean_input$clay[1],
                                 pE = mean_input$pE[1],
                                 PS = nveg_soil_content,
                                 tilling_factor = mean_input$tilling_factor[1])
  print(tail(C0_df,1))
  initialized_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
  
  time_horizon = 70
  C0_df <- calc_carbon_over_time(time_horizon,
                                 field_carbon_in = rep(Cinput_baseline,time_horizon),
                                 dr_ratios = rep(mean_input$dr_ratios[1],time_horizon),
                                 bare = mean_input$bare,
                                 temp = mean_input$past_temp,
                                 precip = mean_input$past_precip,
                                 evap = mean_input$past_evap,
                                 soil_thick = mean_input$soil_thick[1],
                                 clay = mean_input$clay[1],
                                 pE = mean_input$pE[1],
                                 PS = initialized_soil_content,
                                 tilling_factor = mean_input$tilling_factor[1])
  print(tail(C0_df,1))
  soil_C_content <- as.numeric(tail(C0_df,1))[c(1:5)]
    
  }