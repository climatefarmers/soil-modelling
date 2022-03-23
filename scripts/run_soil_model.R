########################### AUTOMATED SOIL MODEL RUNNING SCRIPT
# Missing automated pull of DPM/RPM ratio from input (aboveground crop/pasture/root exudates) type
# Missing automated pull of bare soil
# Unsure about use of soil depth in model: no effect on sensitivity analysis
# Missing automated pull of clay content: it is not set where we will find it
# Missing automated pull of standard errors for input parameters
# Missing observed SOC values for bias analysis

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
  
  weather_data[,c("past_temperature", "future_temperature_rcp4.5")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer,scenario="rcp4.5")
  weather_data[,c("past_precipitation", "future_precipitation_rcp4.5")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer,scenario="rcp4.5")
  weather_data[,c("past_pevap", "future_pevap_rcp4.5")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer,scenario="rcp4.5")
  weather_data[,c("future_temperature_rcp8.5")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer,scenario="rcp8.5")[13:24]
  weather_data[,c("future_precipitation_rcp8.5")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer,scenario="rcp8.5")[13:24]
  weather_data[,c("future_pevap_rcp8.5")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer,scenario="rcp8.5")[2]
  
  
  ################# Pulling calculation factors
  
  animal_factors <- read_csv(file.path(modelling_data_loc,"data", "carbon_share_manure.csv")) %>% filter(type=="manure") %>% 
    rename(species=manure_source)
  agroforestry_factors <- read_csv(file.path(modelling_data_loc,"data", "agroforestry_factors.csv")) 
  crop_data <- read_csv(file.path(modelling_data_loc,"data", "crop_factors.csv"))#, col_types =  "cdddddddd")
  pasture_data <- read_csv(file.path(modelling_data_loc,"data", "pasture_factors.csv"))
  
  ################# Pulling calculation inputs
  animal_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "animal_inputs.csv"))
  agroforestry_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "agroforestry_inputs.csv"))
  crop_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "crop_inputs.csv"))
  pasture_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "pasture_inputs.csv"))
  parcel_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "parcel_inputs.csv"))
  ################# Calculations per parcel and scenario
  
  parcel_Cinputs =data.frame(parcel_ID=c(),scenario=c(),agroforestry_Cinput=c(),animal_Cinput=c(),crop_Cinputs=c(),pasture_Cinputs=c())
  for(parcel in parcel_inputs$parcel_ID){
    for(scenario in c("current","future","baseline")){
      parcel_Cinputs<-rbind(parcel_Cinputs,data.frame(parcel_ID=parcel,
                                                      scenario=scenario,
                                                      agroforestry_Cinput=get_monthly_Cinputs_agroforestry(agroforestry_inputs, agroforestry_factors, 
                                                                                                           scenario, parcel),
                                                      animal_Cinput=get_monthly_Cinputs_animals(animal_inputs, animal_factors, scenario, parcel),
                                                      crop_Cinputs=get_monthly_Cinputs_crop(crop_inputs, crop_data, scenario, parcel),
                                                      pasture_Cinputs=get_monthly_Cinputs_pasture(pasture_inputs, pasture_data, scenario, parcel)))
    }
  }
  parcel_Cinputs <- parcel_Cinputs %>% mutate(tot_Cinputs=agroforestry_Cinput+animal_Cinput+crop_Cinputs+pasture_Cinputs)
  
  write.csv(parcel_Cinputs,file.path(project_loc,project_name,"results/parcel_Cinputs.csv"), row.names = TRUE)
  
  Cinput_leading_to_observed_SOC_past_land_use = 6
  
  ################# Run model per parcel and store graphs, absolute result and step-in tables for each scenario
  mean=c(list(rep(0,12)),
         list(c(0.5,rep(NA,11))),
         list(as.factor(c(logical(12)))),
         list(weather_data$past_temperature),
         list(weather_data$future_temperature_rcp4.5),
         list(weather_data$past_precipitation*365*30.4*24*3600),
         list(weather_data$future_precipitation_rcp4.5*365*30.4*24*3600),
         list(weather_data$past_pevap*365*30.4*24*3600),
         list(weather_data$future_pevap_rcp4.5*365*30.4*24*3600),
         list(c(30,rep(NA,11))),
         list(c(15,rep(NA,11))),
         list(c(0.75,rep(NA,11))),
         list(c(1.0,rep(NA,11))))
  colnames_ranges=c("run","dr_ratios","bare","past_temp","future_temp","past_precip","future_precip","past_evap","future_evap","soil_thick","clay","pE","tilling_factor")
  mean_input = data.frame(mean)
  colnames(mean_input) = colnames_ranges
  
  sd=data.frame(field_carbon_in=0.15,
                dr_ratios = 0.1,
                temp = 0.025,
                precip = 0.025,
                evap = 0.05,
                soil_thick = 0.1,
                clay = 0.1,
                pE = 0.025,
                tilling_factor = 0.025)
  
  step_in_table <- data.frame(run=c(),
                              year=c(),
                              baseline_step_SOC_per_hectare=c(),
                              holistic_step_SOC_per_hectare=c(),
                              baseline_step_total_CO2=c(),
                              holistic_step_total_CO2=c(),
                              yearly_certificates=c())
  all_results<-data.frame(run=c(),parcel_ID=c(),time=c(),SOC=c(),scenario=c(),farm_frac=c())
  farm_results<-data.frame(run=c(),time=c(),scenario=c(),SOC_farm=c())
  
  n_run = 2
  
  for (run_ID in c(1:n_run)){
    all_results_batch<-data.frame(run=c(),parcel_ID=c(),time=c(),SOC=c(),scenario=c(),farm_frac=c())
    farm_results_batch<-data.frame(run=c(),time=c(),SOC_farm=c(),scenario=c())
    #Choice of a random factor to normally randomize input values
    batch_coef=data.frame(field_carbon_in = rnorm(1,1,sd$field_carbon_in),
                          dr_ratios = rnorm(1,1,sd$dr_ratios),
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
      mean_input$future_precip = weather_data$future_precipitation_rcp4.5*365*30.4*24*3600
      mean_input$future_evap = weather_data$future_pevap_rcp4.5*365*30.4*24*3600
      }
    if (climate_scenario=='rcp8.5'){
      mean_input$future_temp = weather_data$future_temperature_rcp8.5
      mean_input$future_precip = weather_data$future_precipitation_rcp8.5*365*30.4*24*3600
      mean_input$future_evap = weather_data$future_pevap_rcp8.5*365*30.4*24*3600
    }
    #Apply factors to inputs average
    batch=data.frame(run=run_ID,
                     dr_ratios = mean_input$dr_ratios[1]*batch_coef$dr_ratios,
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
    
    #Starts modelling each parcel
    for(i in c(1:nrow(parcel_inputs))){
      parcel = parcel_inputs$parcel_ID[i]
      farm_frac = parcel_inputs$area[i]/sum(parcel_inputs$area)
      # Initialisation
      batch$field_carbon_in <- rep(Cinput_leading_to_observed_SOC_past_land_use,12)
      starting_soil_content <- estimate_starting_soil_content(SOC=10,clay=mean_input$clay[1])
      time_horizon = 500
      C0_df <- calc_carbon_over_time(time_horizon,
                                     field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                     dr_ratios = rep(batch$dr_ratios[1],time_horizon),
                                     bare = batch$bare,
                                     temp = batch$past_temp,
                                     precip = batch$past_precip,
                                     evap = batch$past_evap,
                                     soil_thick = batch$soil_thick[1],
                                     clay = batch$clay[1],
                                     pE = batch$pE[1],
                                     PS = starting_soil_content,
                                     tilling_factor = batch$tilling_factor[1])
      print(tail(C0_df,1))
      initialized_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
      
      batch_parcel_Cinputs = parcel_Cinputs %>% mutate(tot_Cinputs=tot_Cinputs*batch_coef$field_carbon_in)
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario=="baseline" & parcel_ID==parcel))$tot_Cinputs
      time_horizon = 30
      C0_df <- calc_carbon_over_time(time_horizon,
                                     field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                     dr_ratios = rep(batch$dr_ratios[1],time_horizon),
                                     bare = batch$bare,
                                     temp = batch$past_temp,
                                     precip = batch$past_precip,
                                     evap = batch$past_evap,
                                     soil_thick = batch$soil_thick[1],
                                     clay = batch$clay[1],
                                     pE = batch$pE[1],
                                     PS = initialized_soil_content,
                                     tilling_factor = batch$tilling_factor[1])
      print(tail(C0_df,1))
      new_starting_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
      
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario=="baseline" & parcel_ID==parcel))$tot_Cinputs
      time_horizon = 10
      C0_df_mdf <- calc_carbon_over_time(time_horizon,
                                         field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                         dr_ratios = rep(batch$dr_ratios[1],time_horizon),
                                         bare = batch$bare,
                                         temp = batch$future_temp,
                                         precip = batch$future_precip,
                                         evap = batch$future_evap,
                                         soil_thick = batch$soil_thick[1],
                                         clay = batch$clay[1],
                                         pE = batch$pE[1],
                                         PS = new_starting_soil_content,
                                         tilling_factor = batch$tilling_factor[1])
      print("Modern-day farming:")
      print(tail(C0_df_mdf,1))
      
      batch$field_carbon_in <- (batch_parcel_Cinputs %>% filter (scenario=="future" & parcel_ID==parcel))$tot_Cinputs
      time_horizon = 10
      C0_df_holistic <- calc_carbon_over_time(time_horizon,
                                         field_carbon_in = rep(batch$field_carbon_in[1],time_horizon),
                                         dr_ratios = rep(batch$dr_ratios[1],time_horizon),
                                         bare = batch$bare,
                                         temp = batch$future_temp,
                                         precip = batch$future_precip,
                                         evap = batch$future_evap,
                                         soil_thick = batch$soil_thick[1],
                                         clay = batch$clay[1],
                                         pE = batch$pE[1],
                                         PS = new_starting_soil_content,
                                         tilling_factor = batch$tilling_factor[1])
      print("Holictic planning:")
      print(tail(C0_df_holistic,1))
      
      
      all_results_batch <- rbind(all_results_batch,data.frame(run=run_ID,
                                                  parcel_ID=rep(parcel,264),time=rep(seq(as.Date("2021-1-1"), as.Date("2031-12-31"), by = "month"),2),
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
  }
  step_in_table_final <- step_in_table %>% group_by(year) %>% 
    summarise(baseline_step_total_CO2_mean=mean(baseline_step_total_CO2),
              baseline_step_total_CO2_sd=sd(baseline_step_total_CO2),
              holistic_step_total_CO2_mean=mean(holistic_step_total_CO2),
              holistic_step_total_CO2_sd=sd(holistic_step_total_CO2))%>%
    mutate(lower_limit_holistic=holistic_step_total_CO2_mean-1.96*holistic_step_total_CO2_sd,
           upper_limit_baseline=baseline_step_total_CO2_mean+1.96*baseline_step_total_CO2_sd,
           yearly_certificates_mean=lower_limit_holistic-upper_limit_baseline) %>%
    select(year,baseline_step_total_CO2_mean,baseline_step_total_CO2_sd,holistic_step_total_CO2_mean,holistic_step_total_CO2_sd,yearly_certificates_mean)
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
  png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  print(graph)
  dev.off()
  
  name<-paste("Results_farm_",project_name,sep = "")
  graph <- ggplot(data = step_in_table_final, aes(x=year, group = 1)) + 
    geom_bar(aes(y = baseline_step_total_CO2_mean), stat="identity", fill="darkred", alpha=0.7)+  
    geom_errorbar(aes(ymin = baseline_step_total_CO2_mean-1.96*baseline_step_total_CO2_sd,
                      ymax = baseline_step_total_CO2_mean+1.96*baseline_step_total_CO2_sd), colour="black", width=.5)+
    geom_bar(aes(y = holistic_step_total_CO2_mean), stat="identity", fill="#5CB85C", alpha=0.7)+   
    geom_errorbar(aes(ymin = holistic_step_total_CO2_mean-1.96*holistic_step_total_CO2_sd,
                      ymax = holistic_step_total_CO2_mean+1.96*holistic_step_total_CO2_sd, color = "95% CI"), colour="black", width=.5, show.legend = T)+ 
    labs(title = name)+
    xlab("Time")+
    ylab("tCO2 sequestered (each year)")
  print(graph)
  png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  print(graph)
  dev.off()
  
  histogram <- ggplot(step_in_table_final) +
    geom_bar( aes(x=year, y=yearly_certificates_mean), stat="identity", fill="#5CB85C", alpha=0.7)+
    xlab("Time")+
    ylab("Number of certificates issuable (per year)")
    #geom_errorbar( aes(x=year, ymin=yearly_certificates_mean-1.96*yearly_certificates_sd, ymax=yearly_certificates_mean+1.96*yearly_certificates_sd))#, width=0.4, colour="black", alpha=0.9, size=1.3)
  print(histogram)
  name<-paste("Certificates_farm_",project_name,sep = "")
  png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  print(graph)
  dev.off()
  write.csv(all_results_final,file.path(project_loc,project_name,"results",paste(name,"_per_parcel.csv",sep="")), row.names = TRUE)
  write.csv(farm_results_final,file.path(project_loc,project_name,"results",paste(name,".csv",sep="")), row.names = TRUE)
  write.csv(step_in_table_final,file.path(project_loc,project_name,"results",paste(name,"_yearly_steps.csv",sep="")), row.names = TRUE)
  
}