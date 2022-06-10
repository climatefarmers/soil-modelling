########################### AUTOMATED SOIL MODEL RUNNING SCRIPT
# Missing automated pull of standard errors for input parameters
# Missing change RothC parameters to semi-arid according to Farina et al. 2013

run_soil_model <- function(soil_loc,project_loc,project_name,modelling_data_loc,weatherDB_loc){
  
  source(file.path(soil_loc, "model_functions.R"))
  source(file.path(soil_loc, "modified_functions.R"))
  source(file.path(soil_loc, "scripts/calc_functions_soil_modelling.R"))
  source(file.path(modelling_data_loc, "scripts/weather_data_pulling_functions.R"))
  
  
  farm_details <- fromJSON(file.path(project_loc,project_name,"inputs", "farm_details.json"))
  lat_farmer <- as.numeric(farm_details$latitude)
  lon_farmer <- as.numeric(farm_details$longitude)
  
  ################# Weather data pulling
  
  weather_data = data.frame(past_temperature=rep(NA,12))
  
  weather_data[,c("past_temperature", "future_temperature_rcp4.5")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer,scenario="rcp4.5", weatherDB_loc)
  weather_data[,c("past_precipitation", "future_precipitation_rcp4.5")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer,scenario="rcp4.5", weatherDB_loc)
  weather_data[,c("past_pevap", "future_pevap_rcp4.5")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer,scenario="rcp4.5", weatherDB_loc)
  weather_data[,c("future_temperature_rcp8.5")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer,scenario="rcp8.5", weatherDB_loc)[13:24]
  weather_data[,c("future_precipitation_rcp8.5")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer,scenario="rcp8.5", weatherDB_loc)[13:24]
  weather_data[,c("future_pevap_rcp8.5")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer,scenario="rcp8.5", weatherDB_loc)[2]
  
  
  ################# Pulling calculation factors
  animal_factors <- read_csv(file.path(modelling_data_loc,"data", "carbon_share_manure.csv")) %>% filter(type=="manure") %>% 
    rename(species=manure_source)
  agroforestry_factors <- read_csv(file.path(modelling_data_loc,"data", "agroforestry_factors.csv")) 
  crop_data <- read_csv(file.path(modelling_data_loc,"data", "crop_factors.csv"))#, col_types =  "cdddddddd")
  manure_factors <- read_csv(file.path(modelling_data_loc,"data", "carbon_share_manure.csv"))
  natural_area_factors <- read_csv(file.path(modelling_data_loc,"data", "natural_area_factors.csv"))
  pasture_data <- read_csv(file.path(modelling_data_loc,"data", "pasture_factors.csv"))
  
  ################# Pulling calculation inputs
  add_manure_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "additional_manure_inputs.csv"))
  animal_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "animal_inputs.csv"))
  agroforestry_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "agroforestry_inputs.csv"))
  bare_field_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "bare_field_inputs.csv"))
  crop_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "crop_inputs.csv"))
  parcel_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "parcel_inputs.csv"))
  pasture_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "pasture_inputs.csv"))
  soil_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "soil_inputs.csv"))
  tilling_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "tilling_inputs.csv"))
  
  ################# Calculations of C inputs per parcel and scenario
  parcel_Cinputs =data.frame(parcel_ID=c(),
                             scenario=c(),
                             add_manure_Cinputs=c(),
                             agroforestry_Cinput=c(),
                             animal_Cinput=c(),
                             crop_Cinputs=c(),
                             pasture_Cinputs=c())
  for(parcel in unique(parcel_inputs$parcel_ID)){
    for(scenario in c("current","future","baseline","previous_years")){
      parcel_Cinputs<-rbind(parcel_Cinputs,
                            data.frame(parcel_ID=parcel,
                                       scenario=scenario,
                                       add_manure_Cinputs=get_monthly_Cinputs_add_manure(add_manure_inputs, manure_factors, scenario, parcel),
                                       agroforestry_Cinputs=get_monthly_Cinputs_agroforestry(agroforestry_inputs, agroforestry_factors, 
                                                                                             scenario, parcel, lat_farmer),
                                       animal_Cinputs=get_monthly_Cinputs_animals(animal_inputs, animal_factors, scenario, parcel),
                                       crop_Cinputs=get_monthly_Cinputs_crop(crop_inputs, crop_data, scenario, parcel),
                                       pasture_Cinputs=get_monthly_Cinputs_pasture(pasture_inputs, pasture_data, scenario, parcel)))
    }
  }
  parcel_Cinputs <- parcel_Cinputs %>% mutate(tot_Cinputs=add_manure_Cinputs+agroforestry_Cinputs+animal_Cinputs+crop_Cinputs+pasture_Cinputs)
  write.csv(parcel_Cinputs,file.path(project_loc,project_name,"results/parcel_Cinputs.csv"), row.names = TRUE)
  
  ################# Calculations of additionnal C inputs compared to baseline per parcel and scenario
  baseline_chosen="baseline"
  
  parcel_Cinputs_addition = merge(x= parcel_Cinputs, 
                                  y= parcel_inputs %>% 
                                    select(parcel_ID,area) %>%
                                    mutate(farm_frac= paste(round(area/sum(area)*100),'%')), by="parcel_ID") %>% 
    group_by(parcel_ID,farm_frac) %>% 
    summarise(additional_Cinput_per_ha = round(tot_Cinputs[scenario=="future"] - tot_Cinputs[scenario==baseline_chosen],2), 
              relative_increase=paste(as.character(as.integer((tot_Cinputs[scenario=="future"] 
                                                               - tot_Cinputs[scenario==baseline_chosen])
                                                              /tot_Cinputs[scenario==baseline_chosen]*100)),'%'),
              additional_Cinput_total = round(unique(area)*(tot_Cinputs[scenario=="future"] - tot_Cinputs[scenario==baseline_chosen]),1))
  parcel_Cinputs_addition = parcel_Cinputs_addition %>%
    mutate(absolute_contibution = paste(round(100*additional_Cinput_total/sum(parcel_Cinputs_addition$additional_Cinput_total)),'%'))
  write.csv(parcel_Cinputs_addition,file.path(project_loc,project_name,"results/parcel_Cinputs_addition.csv"), row.names = TRUE)
  
  ################# Initialisation by making the model reach SOC of natural areas of the pedo-climatic area
  # Calculating the average clay content among parcels
  mean_clay = mean(soil_inputs$clay)
  # Pulling DMP/RPM ratios from different kind of land use in corresponding pedoclimatic area 
  dr_ratio_trees = unique((natural_area_factors %>% filter(pedo_climatic_area==farm_details$pedo_climatic_area))$DMP_RPM_ratio)
  dr_ratio_non_irrigated = unique((natural_area_factors %>% filter(pedo_climatic_area==farm_details$pedo_climatic_area))$dr_ratio_non_irrigated)
  dr_ratio_irrigated = unique((natural_area_factors %>% filter(pedo_climatic_area==farm_details$pedo_climatic_area))$dr_ratio_irrigated)
  # Building a mean input dataframe to feed RothC
  mean=c(list(rep(0,12)),
         list(c(dr_ratio_trees,rep(NA,11))),
         list(as.factor(c(logical(12)))),
         list(weather_data$past_temperature),
         list(weather_data$future_temperature_rcp4.5),
         list(weather_data$past_precipitation),
         list(weather_data$future_precipitation_rcp4.5),
         list(weather_data$past_pevap),
         list(weather_data$future_pevap_rcp4.5),
         list(c(30,rep(NA,11))), # modelled for 30 cm depth as recommended in IPCC Guidelines 2006
         list(c(mean_clay,rep(NA,11))),
         list(c(0.75,rep(NA,11))), # mean potential transpiration to open-pan evaporation convertion rate
         list(c(1.0,rep(NA,11))))
  colnames_ranges=c("run","dr_ratio","bare","past_temp","future_temp","past_precip","future_precip","past_evap","future_evap","soil_thick","clay","pE","tilling_factor")
  mean_input = data.frame(mean)
  colnames(mean_input) = colnames_ranges
  # Studying the BIAS in the Roth C model
  # Read in management files ----
  # Should have 3 scenarios: Natural vegetation, Baseline farm, future farm. 
  # For each scenario we need input variables:
  ##### Natural vegetation: Clay, silt, total carbon, either bulk density or organic matter, climate 
  ##### Baseline farm: Clay, silt, carbon inputs, tillage & other management, climate. 
  ##### Future farm: Clay, silt, carbon inputs, tillage and other management, future climate. 
  
  LU<-read.csv(file.path(modelling_data_loc,"data", "/soil_landuse.portugal.csv"))
  LU<-LU[LU$latitude < 39.5,] # Roughly where dry temperate weather starts in portugal 
  table(LU$landcover)
  validation.dt.baseline<-LU[LU$landcover=="Cropland", ]
  calibration.dt.nveg<-LU[LU$landcover=="Tree cover", ]
  # Phase 1 - model initialization ----
  # I could average all "tree cover" areas. I don't need to validate these. 
  # The baseline scenario for calibration should be ind. from the baseline sc. in validation
  # Phase 1 - a) Understanding input data ----
  ##hist(calibration.dt.nveg$cstock_t.ha)
  median(calibration.dt.nveg$cstock_t.ha)
  # To improve: extrapolation of C in deeper layers. 
  validation.dt.baseline<-validation.dt.baseline[validation.dt.baseline$lower_depth>10, ]
  calibration.dt.nveg<-calibration.dt.nveg[calibration.dt.nveg$lower_depth>10, ]
  # We're supposed to use Mg per ha = tons per ha. 
  cstock30<-calibration.dt.nveg$cstock_t.ha*30/calibration.dt.nveg$lower_depth
  clay=median(calibration.dt.nveg$clay_value_avg)        #Percent clay
  depth = 30
  cstock= median(cstock30)
  clay.sd = sd(calibration.dt.nveg$clay_value_avg)
  cstock.sd= sd(cstock30)
  median(cstock30[calibration.dt.nveg$clay_value_avg>10.3 & calibration.dt.nveg$clay_value_avg<16.3])
  # hist(calibration.dt.nveg$clay_value_avg)
  # hist(cstock30)
  # plot(cstock30~calibration.dt.nveg$clay_value_avg)
  # hist(calibration.dt.nveg$clay_value_avg)
  # plot(cstock30, calibration.dt.nveg$clay_value_avg)
  # Phase 1 - b) Initialise model - pedotransfer functions. ----
  FallIOM=0.049*cstock^(1.139) #IOM using Falloon method
  # Two approaches: We use C input calculated by paper
  # We use pedotransfer functions for compartment equilibrium 
  # From Weihermuller et al 2013
  RPM = (0.1847*cstock+0.1555)*((clay+1.2750)^-0.1158)
  HUM = (0.7148*cstock+0.5069)*((clay+0.3421)^0.0184)
  BIO = (0.0140*cstock+0.0075)*((clay+8.8473)^0.0567)
  # And let the model get to equilibrium 
  # from arosa et al 2015 https://www.publish.csiro.au/sr/SR15347
  # input is 617 kg leaves/ha x year  
  # C content is 1001mg C in 2g of leaves. Which means each gram of leaves has 500mg of C = 0.5g of C. 
  # That means for each Kg of leaves, half is Carbon. 
  # That's 0.505kg per kg of litterfall. 
  soil.thick=30  #Soil thickness (organic layer topsoil), in cm
  SOC_nveg=median(cstock30)       #Soil organic carbon in Mg/ha 
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
                                    dr_ratio = rep(dr_ratio_trees,time_horizon),
                                    bare = mean_input$bare,
                                    temp = mean_input$past_temp,
                                    precip = mean_input$past_precip,
                                    evap = mean_input$past_evap,
                                    soil_thick = mean_input$soil_thick[1],
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
  
  ################# Run model per parcel and store graphs, absolute result and step-in tables for each scenario
  # Initialisation as a forest
  batch <- mean_input
  batch$field_carbon_in <- rep(Cinput_leading_to_observed_SOC_past_land_use,12)
  starting_soil_content <- estimate_starting_soil_content(SOC=Cinput_leading_to_observed_SOC_past_land_use,clay=mean_input$clay[1])
  time_horizon = 1000
  C0_df <- calc_carbon_over_time(time_horizon,
                                 field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                 dr_ratio = rep(dr_ratio_trees,time_horizon),
                                 bare = batch$bare,
                                 temp = batch$past_temp,
                                 precip = batch$past_precip,
                                 evap = batch$past_evap,
                                 soil_thick = batch$soil_thick[1],
                                 clay = batch$clay[1],
                                 pE = batch$pE[1],
                                 PS = starting_soil_content,
                                 tilling_factor = batch$tilling_factor[1])
  nveg_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
  
  
  ## Modelling perform several times with different inputs
  # Let's define standard deviation for each input representing extrinsic uncertainty of the model
  sd=data.frame(field_carbon_in=0.05,
                dr_ratio = 0.05,
                temp = 0.025,
                precip = 0.025,
                evap = 0.025,
                soil_thick = 0.025,
                clay = 0.025,
                pE = 0.025,
                tilling_factor = 0.025)
  # Initialising data structures
  step_in_table <- data.frame(run=c(),
                              year=c(),
                              baseline_step_SOC_per_hectare=c(),
                              holistic_step_SOC_per_hectare=c(),
                              baseline_step_total_CO2=c(),
                              holistic_step_total_CO2=c(),
                              yearly_certificates=c())
  all_results<-data.frame(run=c(),parcel_ID=c(),time=c(),SOC=c(),scenario=c(),farm_frac=c())
  farm_results<-data.frame(run=c(),time=c(),scenario=c(),SOC_farm=c())
  # Initialising run counter
  run_ID = 0
  # Choosing a number of run to perform extrinsic uncertainty
  n_run = 100
  for (n in c(1:n_run)){
    run_ID = run_ID + 1
    all_results_batch<-data.frame(run=c(),parcel_ID=c(),time=c(),SOC=c(),scenario=c(),farm_frac=c())
    farm_results_batch<-data.frame(run=c(),time=c(),SOC_farm=c(),scenario=c())
    #Choice of a random factor to normally randomize input values
    batch_coef=data.frame(field_carbon_in = rnorm(1,1,sd$field_carbon_in),
                          dr_ratio = rnorm(1,1,sd$dr_ratio),
                          temp = rnorm(1,1,sd$temp),
                          precip = rnorm(1,1,sd$precip),
                          evap = rnorm(1,1,sd$evap),
                          soil_thick = rnorm(1,1,sd$soil_thick),
                          clay = rnorm(1,1,sd$clay),
                          pE = rnorm(1,1,sd$pE),
                          tilling_factor = rnorm(1,1,sd$tilling_factor))
    #Choose randomly one of the two climate scenari
    climate_scenario = ifelse(sample(0:1,1)==0, 'rcp4.5', 'rcp8.5')
    if (climate_scenario=='rcp4.5'){
      mean_input$future_temp = weather_data$future_temperature_rcp4.5
      mean_input$future_precip = weather_data$future_precipitation_rcp4.5
      mean_input$future_evap = weather_data$future_pevap_rcp4.5
    }
    if (climate_scenario=='rcp8.5'){
      mean_input$future_temp = weather_data$future_temperature_rcp8.5
      mean_input$future_precip = weather_data$future_precipitation_rcp8.5
      mean_input$future_evap = weather_data$future_pevap_rcp8.5
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
                     soil_thick = mean_input$soil_thick[1]*batch_coef$soil_thick,
                     clay = mean_input$clay[1]*batch_coef$clay,
                     pE = mean_input$pE[1]*batch_coef$pE,
                     tilling_factor = mean_input$tilling_factor[1]*batch_coef$tilling_factor)
    batch = data.frame(batch)
    
    for(i in c(1:nrow(parcel_inputs))){
      parcel = parcel_inputs$parcel_ID[i]
      farm_frac = parcel_inputs$area[i]/sum(parcel_inputs$area)
      #Select parcel's clay content
      batch$clay = (soil_inputs %>% filter(parcel_ID==parcel))$clay
      batch$dr_ratio = ifelse((batch_parcel_Cinputs %>% filter (scenario=="baseline" & parcel_ID==parcel))$agroforestry_Cinputs>0, dr_ratio_trees, 
                              ifelse((soil_inputs %>% filter(parcel_ID==parcel))$irrigation==TRUE, dr_ratio_irrigated, dr_ratio_non_irrigated))*batch_coef$dr_ratio
      # Progressive transisiton from the general forest to the specific baseline land use over 350 years
      initialized_soil_content <- nveg_soil_content
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*batch_coef$field_carbon_in)
      time_horizon = 350
      for(i in c(1:time_horizon)){
        batch$field_carbon_in <- (time_horizon-i)/time_horizon*Cinput_leading_to_observed_SOC_past_land_use+
          i/time_horizon*(batch_parcel_Cinputs %>% filter (scenario=="baseline" & parcel_ID==parcel))$tot_Cinputs
        C0_df <- calc_carbon_over_time(1,
                                       field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                       dr_ratio = rep(dr_ratio_non_irrigated,time_horizon),
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
      initialized_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
      
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==baseline_chosen & parcel_ID==parcel))$tot_Cinputs
      batch$bare = as.factor(t(bare_field_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))[c(2:13)])
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor
      time_horizon = 75
      C0_df <- calc_carbon_over_time(time_horizon,
                                     field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                     dr_ratio = rep(batch$dr_ratio[1],time_horizon),
                                     bare = batch$bare,
                                     temp = batch$past_temp,
                                     precip = batch$past_precip,
                                     evap = batch$past_evap,
                                     soil_thick = batch$soil_thick[1],
                                     clay = batch$clay[1],
                                     pE = batch$pE[1],
                                     PS = initialized_soil_content,
                                     tilling_factor = batch$tilling_factor[1]) # Attention: is there any tillage in the scenarios?
      new_starting_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
      
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario=="current" & parcel_ID==parcel))$tot_Cinputs
      batch$bare = as.factor(t(bare_field_inputs %>% filter(scenario=="current" & parcel_ID==parcel))[c(2:13)])
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor
      time_horizon = 1
      C0_df_1sty <- calc_carbon_over_time(time_horizon,
                                          field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                          dr_ratio = rep(batch$dr_ratio[1],time_horizon),
                                          bare = batch$bare,
                                          temp = batch$future_temp,
                                          precip = batch$future_precip,
                                          evap = batch$future_evap,
                                          soil_thick = batch$soil_thick[1],
                                          clay = batch$clay[1],
                                          pE = batch$pE[1],
                                          PS = new_starting_soil_content,
                                          tilling_factor = batch$tilling_factor[1])
      new_starting_soil_content <- as.numeric(tail(C0_df_1sty,1))[c(1:5)]
      
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario==baseline_chosen & parcel_ID==parcel))$tot_Cinputs
      batch$bare = as.factor(t(bare_field_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))[c(2:13)])
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor
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
                                         PS = new_starting_soil_content,
                                         tilling_factor = batch$tilling_factor[1])
      
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario=="current" & parcel_ID==parcel))$tot_Cinputs
      batch$bare = as.factor(t(bare_field_inputs %>% filter(scenario=="current" & parcel_ID==parcel))[c(2:13)])
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor
      time_horizon = 1
      C0_df_1sty <- calc_carbon_over_time(time_horizon,
                                          field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                          dr_ratio = rep(batch$dr_ratio[1],time_horizon),
                                          bare = batch$bare,
                                          temp = batch$future_temp,
                                          precip = batch$future_precip,
                                          evap = batch$future_evap,
                                          soil_thick = batch$soil_thick[1],
                                          clay = batch$clay[1],
                                          pE = batch$pE[1],
                                          PS = new_starting_soil_content,
                                          tilling_factor = batch$tilling_factor[1])
      new_starting_soil_content <- as.numeric(tail(C0_df_1sty,1))[c(1:5)]
      
      # For future, C_inputs are more uncertain
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*(batch_coef$field_carbon_in)**1.5)
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario=="future" & parcel_ID==parcel))$tot_Cinputs
      batch$bare = as.factor(t(bare_field_inputs %>% filter(scenario=="future" & parcel_ID==parcel))[c(2:13)])
      batch$tilling_factor = (tilling_inputs %>% filter(scenario==baseline_chosen & parcel_ID==parcel))$tilling_factor
      time_horizon = 9
      C0_df_holistic <- calc_carbon_over_time(time_horizon,
                                              field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                              dr_ratio = rep(batch$dr_ratio[1],time_horizon),
                                              bare = batch$bare,
                                              temp = batch$future_temp,
                                              precip = batch$future_precip,
                                              evap = batch$future_evap,
                                              soil_thick = batch$soil_thick[1],
                                              clay = batch$clay[1],
                                              pE = batch$pE[1],
                                              PS = new_starting_soil_content,
                                              tilling_factor = batch$tilling_factor[1])
      C0_df_holistic <- rbind(C0_df_1sty,C0_df_holistic)
      
      all_results_batch <- rbind(all_results_batch,data.frame(run=run_ID,
                                                              parcel_ID=rep(parcel,264),time=rep(seq(as.Date("2020-1-1"), as.Date("2030-12-31"), by = "month"),2),
                                                              SOC=c(tail(C0_df,12)$TOT,C0_df_mdf$TOT,tail(C0_df,12)$TOT,C0_df_holistic$TOT),
                                                              scenario=c(rep("baseline",132),rep("holistic",132)),
                                                              farm_frac=rep(farm_frac,264)))#,C0_df_baseline$TOT#,rep("current",120)
    }
    farm_results_batch <- data.frame(unique(all_results_batch %>% group_by(time, scenario) %>% mutate(SOC_farm=sum(SOC*farm_frac)) %>% select(run, time,scenario,SOC_farm)))
    step_in_results <- unique(farm_results_batch %>% 
                                mutate(year =format(time, format="%Y")) %>% 
                                group_by(scenario, year) %>% 
                                mutate(yearly_mean=mean(SOC_farm)) %>%
                                select(scenario, year, yearly_mean))
    temp_baseline <- step_in_results %>% filter(scenario=="baseline")
    year_temp <- temp_baseline$year[c(2:length(temp_baseline$year))]
    step_baseline <- diff(temp_baseline$yearly_mean)
    temp_holistic <- step_in_results %>% filter(scenario=="holistic")
    step_holistic <- diff(temp_holistic$yearly_mean)
    step_in_table <- rbind(step_in_table,(data.frame(run=run_ID,
                                                     year=year_temp,
                                                     baseline_step_SOC_per_hectare=step_baseline,
                                                     holistic_step_SOC_per_hectare=step_holistic,
                                                     baseline_step_total_CO2=step_baseline*sum(parcel_inputs$area)*44/12,
                                                     holistic_step_total_CO2=step_holistic*sum(parcel_inputs$area)*44/12)%>%
                                            mutate(yearly_certificates=holistic_step_total_CO2-baseline_step_total_CO2)))
    all_results <- rbind(all_results_batch,all_results)
    farm_results <- rbind(farm_results_batch,farm_results)
    print(paste(paste(paste(paste("Run ",n),"over"),n_run),"done"))
  }
  step_in_table_final <- step_in_table %>% group_by(year) %>% 
    summarise(yearly_certificates_average=mean(yearly_certificates),
              yearly_certificates_sd=sd(yearly_certificates),
              baseline_step_total_CO2_mean=mean(baseline_step_total_CO2),
              baseline_step_total_CO2_var=var(baseline_step_total_CO2),
              holistic_step_total_CO2_mean=mean(holistic_step_total_CO2),
              holistic_step_total_CO2_var=var(holistic_step_total_CO2),
              cov_step_total_CO2=cov(baseline_step_total_CO2,holistic_step_total_CO2),
              sd_diff=sqrt(baseline_step_total_CO2_var+holistic_step_total_CO2_var-2*cov_step_total_CO2)#this equal yearly_certificates_sd
    )%>%
    mutate(yearly_certificates_mean=yearly_certificates_average-1.96*yearly_certificates_sd) %>%
    select(year,yearly_certificates_mean,yearly_certificates_average,yearly_certificates_sd,baseline_step_total_CO2_mean,baseline_step_total_CO2_var,holistic_step_total_CO2_mean,holistic_step_total_CO2_var,cov_step_total_CO2,sd_diff)
  all_results_final <- all_results %>% group_by(scenario,parcel_ID,time,farm_frac) %>% 
    summarise(SOC_mean=mean(SOC), SOC_sd=sd(SOC)) %>%
    select(parcel_ID,farm_frac,time,scenario,SOC_mean,SOC_sd)
  farm_results_final <- farm_results %>% group_by(time,scenario) %>% 
    summarise(SOC_farm_mean=mean(SOC_farm),
              SOC_farm_sd=sd(SOC_farm)) %>%
    select(time,scenario,SOC_farm_mean,SOC_farm_sd)
  
  name<-paste("SOC_results_farm_",project_name,sep = "")
  graph <- ggplot(data = farm_results_final, aes(x = time, y = SOC_farm_mean, colour=scenario)) + 
    geom_line()+ 
    #geom_errorbar(aes(ymin=SOC_farm_mean-SOC_farm_sd, ymax=SOC_farm_mean+SOC_farm_sd), width=.1) +
    scale_color_manual(values = c("darkred","#5CB85C"),labels = c("Modern-day","Holistic"))+ 
    theme(legend.position = "bottom")+
    labs(title = name)+
    xlab("Time")+
    ylab("SOC (in tons per hectare)")
  print(graph)
  # png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  # print(graph)
  # dev.off()
  
  name<-paste("Results_farm_",project_name,sep = "")
  graph <- ggplot(data = step_in_table_final, aes(x=year, group = 1)) + 
    geom_bar(aes(y = baseline_step_total_CO2_mean), stat="identity", fill="darkred", alpha=0.7)+  
    geom_errorbar(aes(ymin = baseline_step_total_CO2_mean-1.96*sqrt(baseline_step_total_CO2_var),
                      ymax = baseline_step_total_CO2_mean+1.96*sqrt(baseline_step_total_CO2_var)), colour="black", width=.5)+
    geom_bar(aes(y = holistic_step_total_CO2_mean), stat="identity", fill="#5CB85C", alpha=0.7)+   
    geom_errorbar(aes(ymin = holistic_step_total_CO2_mean-1.96*sqrt(holistic_step_total_CO2_var),
                      ymax = holistic_step_total_CO2_mean+1.96*sqrt(holistic_step_total_CO2_var), color = "95% CI"), colour="black", width=.5, show.legend = T)+ 
    labs(title = name)+
    xlab("Time")+
    ylab("tCO2 sequestered (each year)")
  print(graph)
  # png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  # print(graph)
  # dev.off()
  
  histogram <- ggplot(step_in_table_final, aes(x=year, group = 1)) +
    geom_bar( aes(y=yearly_certificates_average), stat="identity", fill="#5CB85C", alpha=0.7)+
    geom_errorbar(aes(ymin = yearly_certificates_average-1.96*yearly_certificates_sd,
                      ymax = yearly_certificates_average+1.96*yearly_certificates_sd, color = "95% CI"), colour="black", width=.5, show.legend = T)+ 
    xlab("Time")+
    ylab("Number of certificates issuable (per year)")
  print(histogram)
  
  name<-paste("Certificates_farm_",project_name,sep = "")
  png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  print(histogram)
  dev.off()
  write.csv(all_results_final,file.path(project_loc,project_name,"results",paste(name,"_per_parcel.csv",sep="")), row.names = TRUE)
  write.csv(farm_results_final,file.path(project_loc,project_name,"results",paste(name,".csv",sep="")), row.names = TRUE)
  write.csv(step_in_table_final,file.path(project_loc,project_name,"results",paste(name,"_yearly_steps.csv",sep="")), row.names = TRUE)
  save.image(file.path(project_loc,project_name,"results",paste(project_name,".RData",sep="")))
  
}
