run_soil_model <- function(init_file, pars, farms_everything, farm_EnZ){ 
  # browser()  # debugging
  ## Log starting run message
  log4r::info(my_logger, "run_soil_model.R started running")
  
  ## Define paths
  soil_loc <- init_file$soil_loc
  modelling_data_loc <- init_file$soil_loc
  climatic_zone_loc <- init_file$climatic_zone_loc

  ## Sourcing code from files
  source(file.path(soil_loc, "model_semiArid_functions.R"), local = TRUE)
  source(file.path(soil_loc, "modified_semiArid_functions.R"), local = TRUE)
  source(file.path(soil_loc, "scripts/calc_functions_soil_modelling.R"), local = TRUE)
  source(file.path(soil_loc, "scripts/mongodb_extraction_functions.R"), local = TRUE)
  #source(file.path(modelling_data_loc, "legacy/scripts/Climatic_zone_check_function.R"), local = TRUE)
  source(file.path(soil_loc, "scripts/weather_data_pulling_functions.R"), local = TRUE)

  ## Extracting livestock, landUseSummaryOrPractices and soilAnalysis from the farms_everything variable
  livestock = farms_everything$liveStock
  landUseSummaryOrPractices = farms_everything$landUse$landUseSummaryOrPractices
  soilAnalysis = farms_everything$soilAnalysis

  ## Copying data from baseline to future depending on settings chosen 
  if (copy_yearX_to_following_years_landUse == TRUE){
    for(i in c(yearX_landuse+1:10)){
      landUseSummaryOrPractices[[1]][[paste("year", i, sep="")]] = 
        landUseSummaryOrPractices[[1]][[paste("year", yearX_landuse, sep="")]]}
    log4r::info(my_logger, paste("MODIF: EVERY PARCELS: Data from year", yearX_landuse,
                                 "was pasted to every following years", sep=" "))
  }
  if (copy_yearX_to_following_years_livestock == TRUE){
    for(i in c(yearX_livestock+1:10)){
      livestock[["futureManagement"]][[1]][[paste("year",i,sep="")]] =
        livestock[["futureManagement"]][[1]][[paste("year",yearX_livestock,sep="")]]}
    log4r::info(my_logger, paste("MODIF: LIVESTOCK: Data from year", yearX_livestock,
                                 "was pasted to every following years", sep=" "))
  }
  
  ## Reading in calculation factors (parameters) from csv files
  animal_factors <- read_csv(file.path(modelling_data_loc,"data", "carbon_share_manure.csv")) %>%
    filter(type=="manure") %>% rename(species=manure_source)
  agroforestry_factors <- read_csv(file.path(modelling_data_loc,"data", "agroforestry_factors.csv")) 
  crop_data <- read_csv(file.path(modelling_data_loc,"data", "crop_factors.csv"))
  grazing_factors <- read_csv(file.path(modelling_data_loc,"data", "grazing_factors.csv"))
  manure_factors <- read_csv(file.path(modelling_data_loc,"data", "carbon_share_manure.csv"))
  natural_area_factors <- read_csv(
    file.path(modelling_data_loc, "data", "natural_area_factors.csv")
    ) %>% filter(pedo_climatic_area==farm_EnZ) 
  pasture_data <- read_csv(file.path(modelling_data_loc,"data", "pasture_factors.csv"))
  tilling_factors <- read_csv(file.path(modelling_data_loc,"data", "tilling_factors.csv"))
  soil_cover_data <- read_csv(file.path(modelling_data_loc,"data", "soil_cover_factors.csv"))
  
  ## Creating a data frame to hold soil data
  #soilMapsData <- data.frame(SOC=c(1), clay=c(25), silt =c(30), bulk_density=c(1.2)) # to be pulled and processed from S3 bucket
  
  ## Getting parcel inputs dataframe
  parcel_inputs = get_parcel_inputs(landUseSummaryOrPractices)
  
  ## Getting Land use type (variable not used!)
  landUseType = get_land_use_type(landUseSummaryOrPractices, parcel_inputs)
  
  ## Getting mean lon and lat
  lon_farmer <- mean(parcel_inputs$longitude)
  lat_farmer <- mean(parcel_inputs$latitude)
  
  ## Getting grazing data dataframe
  total_grazing_table = get_total_grazing_table(
    landUseSummaryOrPractices,
    livestock, 
    animal_factors,
    parcel_inputs
    )

  ## Extraction of C inputs per parcel and scenario
  #farm_EnZ = clime.zone.check(init_file, lat_farmer, lon_farmer)
  # C inputs from additional organic matter: hay, compost, manure
  add_manure_inputs = get_add_manure_inputs(landUseSummaryOrPractices)
  # C inputs from tree biomass turnover
  agroforestry_inputs = get_agroforestry_inputs(landUseSummaryOrPractices)
  # C inputs from animal manure
  animal_inputs = get_animal_inputs(landUseSummaryOrPractices,livestock, parcel_inputs)
  # C inputs from crop (cash/cover crop) and residues left biomass turnover
  crop_inputs = get_crop_inputs(landUseSummaryOrPractices, pars)
  crop_inputs = get_baseline_crop_inputs(landUseSummaryOrPractices, crop_inputs, crop_data, my_logger, farm_EnZ)
  # C inputs from pasture biomass turnover
  pasture_inputs <- get_pasture_inputs(landUseSummaryOrPractices, grazing_factors, farm_EnZ, total_grazing_table, my_logger, pars)
  
  ## Soil data
  # Bare field inputs
  bare_field_inputs = get_bare_field_inputs(landUseSummaryOrPractices, soil_cover_data, farm_EnZ)
  # Gets soil data from https://maps.isric.org/ (AWS)
  OCS_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/",farmId,"/ocs.csv",sep=""))
  clay_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/",farmId,"/clay.csv",sep=""))
  silt_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/",farmId,"/silt.csv",sep=""))
  bdod_df = s3read_using(FUN = read_csv, object = paste("s3://soil-modelling/soil_variables/",farmId,"/bdod.csv",sep=""))
  # Fill soil maps data frame
  soilMapsData = data.frame(SOC=mean(OCS_df$`ocs_0-30cm_mean`), SOC_Q0.05=mean(OCS_df$`ocs_0-30cm_Q0.05`), SOC_Q0.95=mean(OCS_df$`ocs_0-30cm_Q0.95`),
                            clay=mean(clay_df$`clay_5-15cm_mean`)/10, clay_Q0.05=mean(clay_df$`clay_5-15cm_Q0.05`)/10, clay_Q0.95=mean(clay_df$`clay_5-15cm_Q0.95`)/10,
                            silt=mean(silt_df$`silt_5-15cm_mean`)/10, silt_Q0.05=mean(silt_df$`silt_5-15cm_Q0.05`)/10, silt_Q0.95=mean(silt_df$`silt_5-15cm_Q0.95`)/10,
                            bulk_density=mean(bdod_df$`bdod_5-15cm_mean`)/100, bdod_Q0.05=mean(bdod_df$`bdod_5-15cm_Q0.05`)/100, bdod_Q0.95=mean(bdod_df$`bdod_5-15cm_Q0.95`)/100)# waiting for values from soil maps
  # Final soil inputs
  soil_inputs <- get_soil_inputs(landUseSummaryOrPractices, soilAnalysis, soilMapsData)

  ## Tilling inputs
  tilling_inputs <- get_tilling_inputs(landUseSummaryOrPractices, tilling_factors, farm_EnZ)
  
  ################# Calculations of C inputs per parcel and scenario
  # Attention: YEARLY C inputs are calculated, naming misleading
  baseline_chosen="baseline"
  parcel_Cinputs =data.frame(parcel_ID=c(),
                             scenario=c(),
                             add_manure_Cinputs=c(),
                             agroforestry_Cinput=c(),
                             animal_Cinput=c(),
                             crop_Cinputs=c(),
                             pasture_Cinputs=c())
  
  # load("parcel_Cinputs.RData")  # for testing only
  
  for(parcel in unique(parcel_inputs$parcel_ID)){
    for(i in c(0:10)){
      scenario = paste("year",i,sep="")
      parcel_Cinputs<-rbind(parcel_Cinputs,
                            data.frame(parcel_ID=parcel,
                                       scenario=scenario,
                                       add_manure_Cinputs=get_monthly_Cinputs_add_manure(add_manure_inputs, manure_factors, scenario, parcel),
                                       agroforestry_Cinputs=0,#get_monthly_Cinputs_agroforestry(agroforestry_inputs, agroforestry_factors,
                                       #                              scenario, parcel, lat_farmer),
                                       animal_Cinputs=get_monthly_Cinputs_animals(animal_inputs, animal_factors, scenario, parcel),
                                       crop_Cinputs=get_monthly_Cinputs_crop(crop_inputs, crop_data, scenario, parcel, farm_EnZ),
                                       pasture_Cinputs=get_monthly_Cinputs_pasture(pasture_inputs, pasture_data, scenario, parcel)))
    }
    scenario = baseline_chosen
    parcel_Cinputs<-rbind(parcel_Cinputs,
                          data.frame(parcel_ID=parcel,
                                     scenario=scenario,
                                     add_manure_Cinputs=get_monthly_Cinputs_add_manure(add_manure_inputs, manure_factors, scenario, parcel),
                                     # TREES NOT COUNTED BEFORE GOOD CHECK OF DATA QUALITY
                                     agroforestry_Cinputs=0,#get_monthly_Cinputs_agroforestry(agroforestry_inputs, agroforestry_factors,
                                     #                               scenario, parcel, lat_farmer),
                                     animal_Cinputs=get_monthly_Cinputs_animals(animal_inputs, animal_factors, scenario, parcel),
                                     crop_Cinputs=get_monthly_Cinputs_crop(crop_inputs, crop_data, scenario, parcel, farm_EnZ),
                                     pasture_Cinputs=get_monthly_Cinputs_pasture(pasture_inputs, pasture_data, scenario, parcel)))
  }
  parcel_Cinputs <- parcel_Cinputs %>% mutate(tot_Cinputs=add_manure_Cinputs+agroforestry_Cinputs+animal_Cinputs+crop_Cinputs+pasture_Cinputs)
  if (length(apply(is.na(parcel_Cinputs), 2, which))==0){
    log4r::info(my_logger,'parcel C inputs calculations have no NAs.',sep=" ")
  } else {
    log4r::error(my_logger, paste(length(apply(is.na(parcel_Cinputs), 2, which)),'NAs were found in parcel C inputs calculation results.'))
  }
  #write.csv(parcel_Cinputs,file.path(project_loc,project_name,"results/parcel_Cinputs.csv"), row.names = TRUE)
  ################# Calculations of additional C inputs compared to baseline per parcel and scenario

  parcel_Cinputs_addition = merge(x= parcel_Cinputs, 
                                  y= parcel_inputs %>% 
                                    select(parcel_ID,area) %>%
                                    mutate(farm_frac= paste(round(area/sum(area)*100),'%')), by="parcel_ID") %>% 
    group_by(parcel_ID,farm_frac) %>% 
    mutate(Cinput_per_ha_project = sum(tot_Cinputs[scenario!=baseline_chosen & scenario!="year0"])/10) %>%
    filter(scenario==baseline_chosen) %>%
    mutate(additional_Cinput_per_ha = round(Cinput_per_ha_project - tot_Cinputs,2), 
           relative_increase=paste(as.character(ifelse(tot_Cinputs==0,NA,as.integer((Cinput_per_ha_project- tot_Cinputs)
                                                                                    /tot_Cinputs*100))),'%'),
           additional_Cinput_total = round(unique(area)*(Cinput_per_ha_project - tot_Cinputs),1)) 
  additional_Cinput_total_farm = sum(parcel_Cinputs_addition$additional_Cinput_total)
  parcel_Cinputs_addition = parcel_Cinputs_addition %>%
    mutate(absolute_contribution_perc = round(100*additional_Cinput_total/additional_Cinput_total_farm)) %>%
    select(parcel_ID, farm_frac, additional_Cinput_per_ha, relative_increase, additional_Cinput_total, absolute_contribution_perc)
  #write.csv(parcel_Cinputs_addition,file.path(project_loc,project_name,"results/parcel_Cinputs_addition.csv"), row.names = TRUE)
  
  ## Calculation of total c inputs for the whole farm
  # Sum over all parcels
  yearly_Cinputs_farm = merge(x= parcel_Cinputs, 
                              y= parcel_inputs,
                              by="parcel_ID") %>%
                        group_by(scenario) %>%
                        summarise(tot_Cinputs=sum(tot_Cinputs*area),
                                  add_manure_Cinputs=sum(area*add_manure_Cinputs),
                                  animal_Cinputs=sum(area*animal_Cinputs),
                                  crop_Cinputs=sum(area*crop_Cinputs),
                                  pasture_Cinputs=sum(area*pasture_Cinputs),
                                  agroforestry_Cinputs=sum(area*agroforestry_Cinputs))
  
  ################# Weather data pulling
  if(exists("debug_mode")) {
    if(debug_mode){  # will skip fetching climate data and use dummy data if debug_mode is set
      weather_data <- read_csv("test_weather_data.csv") # For testing only
    } else {
      weather_data=cbind(get_past_weather_data(init_file, lat_farmer, lon_farmer),
                         get_future_weather_data(init_file, lat_farmer, lon_farmer, scenario="rcp4.5"),
                         get_future_weather_data(init_file, lat_farmer, lon_farmer, scenario="rcp8.5"))
    }
  }
  
  ################# Initialisation by making the model reach SOC of natural areas of the pedo-climatic area
  
  ## Calculating the average soil parameters among parcels
  mean_SOC = mean(soil_inputs$SOC)
  mean_clay = mean(soil_inputs$clay)
  mean_silt = mean(soil_inputs$silt)
  mean_bulk_density = mean(soil_inputs$bulk_density)
  
  ## Pulling DMP/RPM ratios from different kind of land use in corresponding pedoclimatic area 
  dr_ratio_agroforestry = unique(natural_area_factors$dr_ratio_agroforestry)
  dr_ratio_non_irrigated = unique(natural_area_factors$dr_ratio_non_irrigated)
  dr_ratio_irrigated = unique(natural_area_factors$dr_ratio_irrigated)
  
  ## Building a mean input dataframe to feed RothC
  # Mean value for each model input parameter
  mean=c(list(rep(0,12)),
         list(c(dr_ratio_non_irrigated,rep(NA,11))),
         list(as.factor(c(logical(12)))),
         list(weather_data$past_temperature),
         list(weather_data$future_temperature_rcp4.5),
         list(weather_data$past_precipitation),
         list(weather_data$future_precipitation_rcp4.5),
         list(weather_data$past_evap),
         list(weather_data$future_evap_rcp4.5),
         list(c(30,rep(NA,11))), # modelled for 30 cm depth as recommended in IPCC Guidelines 2006
         list(c(soilMapsData$SOC,rep(NA,11))),
         list(c(soilMapsData$clay,rep(NA,11))),
         list(c(soilMapsData$silt,rep(NA,11))),
         list(c(soilMapsData$bulk_density,rep(NA,11))),
         list(c(0.75,rep(NA,11))), # mean potential transpiration to open-pan evaporation convertion rate
         list(c(1.0,rep(NA,11))))
  colnames_ranges=c("run","dr_ratio","bare","past_temp","future_temp","past_precip","future_precip","past_evap","future_evap","soil_thick","SOC","clay","silt","bulk_density","pE","tilling_factor")
  mean_input = data.frame(mean)
  colnames(mean_input) = colnames_ranges
  
  ## Standard deviation for each input
  # Modelling perform several times with different inputs
  # Let's define standard deviation for each input representing extrinsic uncertainty of the model
  sd=data.frame(field_carbon_in=pars$sd_field_carbon_in,
                dr_ratio = 0.025,
                temp = 0.025,
                precip = 0.025,
                evap = 0.025,
                soil_thick = 0.025,
                SOC = 0.025,#(soilMapsData$SOC_Q0.95-soilMapsData$SOC_Q0.05)/3, 
                clay = 0.025,#(soilMapsData$clay_Q0.95-soilMapsData$clay_Q0.05)/5,# to be refined
                silt = 0.025,#(soilMapsData$silt_Q0.95-soilMapsData$silt_Q0.05)/5, 
                bulk_density = 0.025,#(soilMapsData$bdod_Q0.95-soilMapsData$bdod_Q0.05)/3,
                pE = 0.025,
                tilling_factor = 0.025)
  
  ## Initialising data structures
  # Data frame per year that includes soc per year, co2 per year and co2 difference per year
  step_in_table <- data.frame(run=c(),
                              year=c(),
                              baseline_step_SOC_per_hectare=c(),
                              holistic_step_SOC_per_hectare=c(),
                              baseline_step_total_CO2=c(),
                              holistic_step_total_CO2=c(),
                              yearly_CO2diff=c())
  # Data frame that includes total soc per parcel per scenario 
  all_results<-data.frame(run=c(),parcel_ID=c(),time=c(),SOC=c(),scenario=c(),farm_frac=c())
  # Data frame that includes total soc per farm
  farm_results<-data.frame(run=c(),time=c(),scenario=c(),SOC_farm=c())
  
  # if (length(unique(is.na(parcel_Cinputs)))==1){
  #   log4r::info(my_logger,'Parcels loop starting smoothly. No NA in C inputs.',sep=" ")
  # } else {
  #   log4r::error(my_logger, 'CAUTION: there is NA in C inputs (before entering in calculation loops).')
  # }
  
  ## Chossing model version
  model_version = ifelse(sum(weather_data$past_precipitation)/sum(weather_data$past_pevap)<0.65 &
                           sum(weather_data$past_precipitation)<600,"Semi-arid","Normal")
  
  ## Initialisation of model runs
  # Initialising run counter
  run_ID = 0
  # Choosing a number of run to perform extrinsic uncertainty
  n_run = pars$n_run # moved out to pars argument
  
  ## Model runs
  for (n in c(1:n_run)){
    run_ID = run_ID + 1
    all_results_batch <- data.frame(run=c(), parcel_ID=c(), time=c(), SOC=c(), scenario=c(), farm_frac=c())
    farm_results_batch <- data.frame(run=c(), time=c(), SOC_farm=c(), scenario=c())
    #Choice of a random factor to normally randomize input values
    batch_coef=data.frame(field_carbon_in = rnorm(1,1,sd$field_carbon_in),
                          dr_ratio = rnorm(1,1,sd$dr_ratio),
                          temp = rnorm(1,1,sd$temp),
                          precip = rnorm(1,1,sd$precip),
                          evap = rnorm(1,1,sd$evap),
                          soil_thick = rnorm(1,1,sd$soil_thick),
                          SOC = rnorm(1,1,sd$SOC),
                          clay = rnorm(1,1,sd$clay),
                          pE = rnorm(1,1,sd$pE),
                          tilling_factor = rnorm(1,1,sd$tilling_factor),
                          silt = rnorm(1,1,sd$silt),
                          bulk_density = rnorm(1,1,sd$bulk_density))
    
    #Choose randomly one of the two climate scenario
    climate_scenario = ifelse(sample(0:1,1)==0, 'rcp4.5', 'rcp8.5')
    if (climate_scenario=='rcp4.5'){
      mean_input$future_temp = weather_data$future_temperature_rcp4.5
      mean_input$future_precip = weather_data$future_precipitation_rcp4.5
      mean_input$future_evap = weather_data$future_evap_rcp4.5
    }
    if (climate_scenario=='rcp8.5'){
      mean_input$future_temp = weather_data$future_temperature_rcp8.5
      mean_input$future_precip = weather_data$future_precipitation_rcp8.5
      mean_input$future_evap = weather_data$future_evap_rcp8.5
    }
    #Apply factors to inputs average
    batch=data.frame(run=run_ID,
                     bare = mean_input$bare,
                     past_temp = mean_input$past_temp*batch_coef$temp,
                     past_precip = mean_input$past_precip*batch_coef$precip,
                     past_evap = mean_input$past_evap*batch_coef$evap,
                     future_temp = mean_input$future_temp*batch_coef$temp,
                     future_precip = mean_input$future_precip*batch_coef$precip,
                     future_evap = mean_input$future_evap*batch_coef$evap,
                     soil_thick = mean_input$soil_thick*batch_coef$soil_thick,
                     SOC = mean_input$SOC*batch_coef$SOC,
                     clay = mean_input$clay*batch_coef$clay,
                     silt = mean_input$silt*batch_coef$silt,
                     bulk_density = mean_input$bulk_density*batch_coef$bulk_density,
                     pE = mean_input$pE*batch_coef$pE,
                     tilling_factor = mean_input$tilling_factor*batch_coef$tilling_factor)
    batch = data.frame(batch)
    
    for(i in c(1:nrow(parcel_inputs))){
      parcel = parcel_inputs$parcel_ID[i]
      farm_frac = parcel_inputs$area[i]/sum(parcel_inputs$area)
      #Select parcel's fixed values
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*batch_coef$field_carbon_in)
      batch$dr_ratio = ifelse((batch_parcel_Cinputs %>% filter (scenario==baseline_chosen & parcel_ID==parcel))$agroforestry_Cinputs>0, dr_ratio_agroforestry, 
                              ifelse((soil_inputs %>% filter(parcel_ID==parcel))$irrigation==TRUE, dr_ratio_irrigated, dr_ratio_non_irrigated))*batch_coef$dr_ratio
      # choice of scenario = baseline
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==baseline_chosen & parcel_ID==parcel))$tot_Cinputs
      batch$bare = as.factor(t(bare_field_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))[c(3:14)])
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor
      starting_soil_content = estimate_starting_soil_content(SOC=batch$SOC[1], clay=batch$clay[1]) 
      time_horizon = 1
      C0_df <- calc_carbon_over_time(time_horizon,
                                     field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                     dr_ratio = rep(batch$dr_ratio[1],time_horizon),
                                     bare = batch$bare,
                                     temp = batch$future_temp,
                                     precip = batch$future_precip,
                                     evap = batch$future_evap,
                                     soil_thick = batch$soil_thick[1],
                                     clay = batch$clay[1],
                                     pE = batch$pE[1],
                                     PS = starting_soil_content,
                                     tilling_factor = batch$tilling_factor[1],
                                     version=model_version,
                                     silt = batch$silt[1],
                                     bulk_density = batch$bulk_density[1])
      starting_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
      time_horizon = 10
      C0_df_mdf <- calc_carbon_over_time(time_horizon,
                                         field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                         dr_ratio = rep(batch$dr_ratio[1],time_horizon),
                                         bare = batch$bare,
                                         temp = batch$future_temp,
                                         precip = batch$future_precip,
                                         evap = batch$future_evap,
                                         soil_thick = batch$soil_thick[1],
                                         clay = batch$clay[1],
                                         pE = batch$pE[1],
                                         PS = starting_soil_content,
                                         tilling_factor = batch$tilling_factor[1],
                                         version=model_version,
                                         silt = batch$silt[1],
                                         bulk_density = batch$bulk_density[1])
      
      # For future, C_inputs are more uncertain: base uncertainty**1.5
      N_1 = 1 #first year of future modelling
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*(batch_coef$field_carbon_in)**pars$sd_future_mod)
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==paste("year",N_1,sep="") & parcel_ID==parcel))$tot_Cinputs
      batch$bare = as.factor(t(bare_field_inputs %>% filter(scenario==paste("year",N_1,sep="") & parcel_ID==parcel))[c(3:14)])
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==paste("year",N_1,sep="") & parcel_ID==parcel))$tilling_factor
      time_horizon = 1
      C0_df_holistic_yearly <- calc_carbon_over_time(time_horizon,
                                                     field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                                     dr_ratio = rep(batch$dr_ratio[1],time_horizon),
                                                     bare = batch$bare,
                                                     temp = batch$future_temp,
                                                     precip = batch$future_precip,
                                                     evap = batch$future_evap,
                                                     soil_thick = batch$soil_thick[1],
                                                     clay = batch$clay[1],
                                                     pE = batch$pE[1],
                                                     PS = starting_soil_content,
                                                     tilling_factor = batch$tilling_factor[1],
                                                     version=model_version,
                                                     silt = batch$silt[1],
                                                     bulk_density = batch$bulk_density[1])
      starting_holistic_soil_content <- as.numeric(tail(C0_df_holistic_yearly ,1))[c(1:5)]
      C0_df_holistic = C0_df_holistic_yearly
      # next years
      for (N in c((N_1+1):10)){
        batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*(batch_coef$field_carbon_in)**1.5)
        batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==paste("year",N,sep="") & parcel_ID==parcel))$tot_Cinputs
        batch$bare = as.factor(t(bare_field_inputs %>% filter(scenario==paste("year",N,sep="") & parcel_ID==parcel))[c(3:14)])
        batch$tilling_factor = (tilling_inputs %>% filter(scenario==paste("year",N,sep="") & parcel_ID==parcel))$tilling_factor
        time_horizon = 1
        C0_df_holistic_yearly <- calc_carbon_over_time(time_horizon,
                                                       field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                                       dr_ratio = rep(batch$dr_ratio[1],time_horizon),
                                                       bare = batch$bare,
                                                       temp = batch$future_temp,
                                                       precip = batch$future_precip,
                                                       evap = batch$future_evap,
                                                       soil_thick = batch$soil_thick[1],
                                                       clay = batch$clay[1],
                                                       pE = batch$pE[1],
                                                       PS = starting_holistic_soil_content,
                                                       tilling_factor = batch$tilling_factor[1],
                                                       version=model_version,
                                                       silt = batch$silt[1],
                                                       bulk_density = batch$bulk_density[1])
        starting_holistic_soil_content <- as.numeric(tail(C0_df_holistic_yearly ,1))[c(1:5)]
        C0_df_holistic= rbind(C0_df_holistic,C0_df_holistic_yearly)
      }
      
      all_results_batch <- rbind(all_results_batch, data.frame(run=run_ID,
                                                              parcel_ID=rep(parcel, 264),
                                                              time=rep(seq(as.Date("2020-1-1"), as.Date("2030-12-31"), by = "month"),2),
                                                              SOC=c(C0_df$TOT, C0_df_mdf$TOT, C0_df$TOT, C0_df_holistic$TOT),
                                                              scenario=c(rep("baseline", 132), rep("holistic", 132)),
                                                              farm_frac=rep(farm_frac, 264)))#,C0_df_baseline$TOT#,rep("current",120)
    }

    farm_results_batch <- data.frame(unique(all_results_batch %>% group_by(time, scenario) %>% mutate(SOC_farm=sum(SOC*farm_frac)) %>% select(run, time, scenario, SOC_farm)))
    months <- format(farm_results_batch$time, format="%m")
    step_in_results <- farm_results_batch[months==12,]  # SOC content at end of year (December, month 12) is selected.
    step_in_results <- step_in_results %>% mutate(year = format(time, format="%Y"))
    step_baseline <- diff(step_in_results$SOC_farm[step_in_results$scenario=="baseline"])
    step_holistic <- diff(step_in_results$SOC_farm[step_in_results$scenario=="holistic"])
    year_temp <- step_in_results$year[step_in_results$scenario=="holistic"][2:11]
    
    step_in_table <- rbind(step_in_table, (data.frame(run=run_ID,
                                                     year=year_temp,
                                                     baseline_step_SOC_per_hectare=step_baseline,
                                                     holistic_step_SOC_per_hectare=step_holistic,
                                                     baseline_step_total_CO2=step_baseline*sum(parcel_inputs$area)*44/12,
                                                     holistic_step_total_CO2=step_holistic*sum(parcel_inputs$area)*44/12) %>%
                                            mutate(yearly_CO2diff=holistic_step_total_CO2-baseline_step_total_CO2)))
    all_results <- rbind(all_results_batch, all_results)
    farm_results <- rbind(farm_results_batch, farm_results)
    print(paste(paste(paste(paste("Run ", n), "over"), n_run), "done"))
    
  } # End of model runs
  
  ## Final data frames by taking the average over the runs
  # Results of soc and co2 per year
  step_in_table_final <- step_in_table %>% group_by(year) %>% 
    summarise(yearly_CO2diff_mean=mean(yearly_CO2diff),
              yearly_CO2diff_sd=sd(yearly_CO2diff),
              baseline_step_total_CO2_mean=mean(baseline_step_total_CO2),
              baseline_step_total_CO2_var=var(baseline_step_total_CO2),
              holistic_step_total_CO2_mean=mean(holistic_step_total_CO2),
              holistic_step_total_CO2_var=var(holistic_step_total_CO2),
              cov_step_total_CO2=cov(baseline_step_total_CO2, holistic_step_total_CO2),
              sd_diff=sqrt(baseline_step_total_CO2_var+holistic_step_total_CO2_var-2*cov_step_total_CO2)#this equal yearly_CO2diff_sd
    )%>%
    mutate(yearly_CO2diff_final=round(yearly_CO2diff_mean-1.96*yearly_CO2diff_sd)) %>%
    select(year, yearly_CO2diff_final, yearly_CO2diff_mean, 
           yearly_CO2diff_sd, baseline_step_total_CO2_mean, 
           baseline_step_total_CO2_var, holistic_step_total_CO2_mean, 
           holistic_step_total_CO2_var, cov_step_total_CO2,sd_diff)

  # Results of soc per parcel per scenario/year
  all_results_final <- all_results %>% group_by(scenario, parcel_ID, time, farm_frac) %>% 
    summarise(SOC_mean=mean(SOC), SOC_sd=sd(SOC)) %>%
    select(parcel_ID, farm_frac, time, scenario, SOC_mean, SOC_sd)
  
  # Results of soc on farm level
  farm_results_final <- farm_results %>% group_by(time, scenario) %>% 
    summarise(SOC_farm_mean=mean(SOC_farm),
              SOC_farm_sd=sd(SOC_farm)) %>%
    select(time, scenario, SOC_farm_mean, SOC_farm_sd)
  
  log4r::info(my_logger,'Total soil CO2eq: ', 
              sum(step_in_table_final$yearly_CO2diff_final),
              '.\nCredits per year (before emission reductions): ', 
              list(step_in_table_final$yearly_CO2diff_final),
              '.\nArea considered: ', round(sum(parcel_inputs$area), 2), ' ha.', 
              "\nNumber of runs: ", run_ID,
              ".\nGrazing estimations by CF (Y/N): ", pars$CFmade_grazing_estimations_Yes_No,
              "\nStandard deviation used for extrinsic uncertainty of practices (Cinputs): ",
              sd$field_carbon_in,
              ifelse(copy_yearX_to_following_years_landUse==TRUE,
                     paste("\nCAUTION: Duplicated and applied land use from 'year",
                           yearX_landuse,"' to following years in EVERY parcel.",sep=""),""),
              ifelse(copy_yearX_to_following_years_livestock==TRUE,
                     paste("\nCAUTION: Duplicated and applied livestock from 'year",
                           yearX_livestock,"' to ALL following years.",sep=""),""),sep="")
  
  write.csv(landUseType, file.path(init_file$soil_loc, "logs",
                                    paste("landUseType_", 
                                          farms_everything$farmInfo$farmManagerFirstName, 
                                          farms_everything$farmInfo$farmManagerLastName,
                                          ".csv",sep="")
                                   ), row.names = FALSE)
  
  # write.csv(all_results_final, file.path(project_loc, project_name, "results", paste0(name,"_per_parcel.csv")), row.names = TRUE)
  # write.csv(farm_results_final, file.path(project_loc, project_name, "results", paste0(name,".csv")), row.names = TRUE)
  # write.csv(step_in_table_final, file.path(project_loc, project_name, "results", paste0(name,"_yearly_steps.csv")), row.names = TRUE)
  # save.image(file.path(project_loc, project_name, "results", paste0(project_name, ".RData")))
  
  # Use of parcels coordinates for other applications like co-benefits
  #jsonFileOfParcelsCoordinates = toJSON(landUseSummaryOrPractices[[1]]$coordinates[c(2,3)])
  #write(jsonFileOfParcelsCoordinates, paste("parcelsCoordinates_",farmId,".json",sep=""))
  
  if (length(apply(is.na(step_in_table_final), 2, which))==0){
    log4r::info(my_logger,'soil_run_model.R calculations ran smoothly.',sep=" ")
  } else {
    log4r::error(my_logger, 'NAs in results.')
  }
  
  return(list(step_in_table_final=step_in_table_final,
              farm_results_final=farm_results_final,
              all_results_final=all_results_final))
}
