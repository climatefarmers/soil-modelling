run_soil_model <- function(soil_loc,project_loc,project_name,modelling_data_loc,weatherDB_loc){
  
  source(file.path(soil_loc, "model_functions.R"))
  source(file.path(soil_loc, "modified_functions.R"))
  source(file.path(soil_loc, "scripts/calc_functions_soil_modelling.R"))
  source(file.path(modelling_data_loc, "scripts/weather_data_pulling_functions.R"))
  
  
  farm_details <- fromJSON(file.path(project_loc,project_name,"inputs", "farm_details.json"))
  lat_farmer <- as.numeric(farm_details$latitude)
  lon_farmer <- as.numeric(farm_details$longitude)
  
  ################# Weather data pulling
  
  weather_data = data.frame(past_temperature=rep(NA,12),future_temperature=rep(NA,12),
                            past_precipitation=rep(NA,12),future_precipitation=rep(NA,12),
                            past_pevap=rep(NA,12),future_pevap=rep(NA,12))
  
  weather_data[,c("past_temperature", "future_temperature")] <- get_monthly_mean_temperature(lon_farmer,lat_farmer)
  weather_data[,c("past_precipitation", "future_precipitation")] <- get_monthly_mean_precipitation(lon_farmer,lat_farmer)
  weather_data[,c("past_pevap", "future_pevap")] <- get_monthly_mean_pevap(lon_farmer,lat_farmer)
  
  
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
  
  ################# Run model per parcel and store graphs, absolute result and step-in tables for each scenario
  all_results<-data.frame(parcel_ID=c(),time=c(),SOC=c(),variable=c(),farm_frac=c())
  for(i in c(1:nrow(parcel_inputs))){
    parcel = parcel_inputs$parcel_ID[i]
    farm_frac = parcel_inputs$area[i]/sum(parcel_inputs$area)
    Cinput <- (parcel_Cinputs %>% filter (scenario=="baseline" & parcel_ID==parcel))$tot_Cinputs
    mean=c(list(c(Cinput,rep(NA,11))),
           list(c(1.44,rep(NA,11))),
           list(c(12/12,rep(NA,11))),
           list(weather_data$past_temperature),
           list(weather_data$past_precipitation*365*30.4*24*3600),
           list(weather_data$past_pevap*365*30.4*24*3600),
           list(c(30,rep(NA,11))),
           list(c(15,rep(NA,11))),
           list(c(0.75,rep(NA,11))),
           list(c(1.0,rep(NA,11))))
    colnames_ranges=c("field_carbon_in","dr_ratios","perc_cover","temp","precip","evap","soil_thick","clay","pE","tilling_factor")
    mean_input = data.frame(mean)
    colnames(mean_input) = colnames_ranges
    starting_soil_content <- estimate_starting_soil_content(SOC=10,clay=mean_input$clay[1])
    time_horizon = 500
    C0_df <- calc_carbon_over_time(time_horizon,
                                   field_carbon_in = rep(mean_input$field_carbon_in[1],time_horizon),
                                   dr_ratios = rep(mean_input$dr_ratios[1],time_horizon),
                                   bare = as.factor(c(logical(round(12*mean_input$perc_cover[1])),!logical(round(12*(1-mean_input$perc_cover[1]))))),
                                   temp = mean_input$temp,
                                   precip = mean_input$precip,
                                   evap = mean_input$evap,
                                   soil_thick = mean_input$soil_thick[1],
                                   clay = mean_input$clay[1],
                                   pE = mean_input$pE[1],
                                   PS = starting_soil_content,
                                   tilling_factor = mean_input$tilling_factor[1])
    print(tail(C0_df,1))
    new_starting_soil_content <- as.numeric(tail(C0_df,1))[c(1:5)]
    #new_starting_soil_content <- estimate_starting_soil_content(SOC=18.1,clay=mean_input$clay[1])
    
    mean=c(list(c(Cinput,rep(NA,11))),
           list(c(1.44,rep(NA,11))),
           list(c(12/12,rep(NA,11))),
           list(weather_data$future_temperature),
           list(weather_data$future_precipitation*365*30.4*24*3600),
           list(weather_data$future_pevap*365*30.4*24*3600),
           list(c(30,rep(NA,11))),
           list(c(15,rep(NA,11))),
           list(c(0.75,rep(NA,11))),
           list(c(1.0,rep(NA,11))))
    colnames_ranges=c("field_carbon_in","dr_ratios","perc_cover","temp","precip","evap","soil_thick","clay","pE","tilling_factor")
    mean_input = data.frame(mean)
    colnames(mean_input) = colnames_ranges
    time_horizon = 10
    C0_df_mdf <- calc_carbon_over_time(time_horizon,
                                       field_carbon_in = rep(mean_input$field_carbon_in[1],time_horizon),
                                       dr_ratios = rep(mean_input$dr_ratios[1],time_horizon),
                                       bare = as.factor(c(logical(round(12*mean_input$perc_cover[1])),!logical(round(12*(1-mean_input$perc_cover[1]))))),
                                       temp = mean_input$temp,
                                       precip = mean_input$precip,
                                       evap = mean_input$evap,
                                       soil_thick = mean_input$soil_thick[1],
                                       clay = mean_input$clay[1],
                                       pE = mean_input$pE[1],
                                       PS = new_starting_soil_content,
                                       tilling_factor = mean_input$tilling_factor[1])
    print("Modern-day farming:")
    print(tail(C0_df_mdf,1))
    
    
    # Cinput <- (parcel_Cinputs %>% filter (scenario=="current" & parcel_ID==parcel))$tot_Cinputs
    # mean=c(list(c(Cinput,rep(NA,11))),list(c(1.44,rep(NA,11))),list(c(12/12,rep(NA,11))),list(weather_data$future_temperature),list(weather_data$future_precipitation*365*30.4*24*3600),list(weather_data$future_pevap*365*30.4*24*3600),list(c(30,rep(NA,11))),list(c(15,rep(NA,11))),list(c(0.75,rep(NA,11))),list(c(1.0,rep(NA,11))))
    # colnames_ranges=c("field_carbon_in","dr_ratios","perc_cover","temp","precip","evap","soil_thick","clay","pE","tilling_factor")
    # mean_input = data.frame(mean)
    # colnames(mean_input) = colnames_ranges
    # time_horizon = 10
    # C0_df_baseline <- calc_carbon_over_time(time_horizon,
    #                                         field_carbon_in = rep(mean_input$field_carbon_in[1],time_horizon),
    #                                         dr_ratios = rep(mean_input$dr_ratios[1],time_horizon),
    #                                         bare = as.factor(c(logical(round(12*mean_input$perc_cover[1])),!logical(round(12*(1-mean_input$perc_cover[1]))))),
    #                                         temp = mean_input$temp,
    #                                         precip = mean_input$precip,
    #                                         evap = mean_input$evap,
    #                                         soil_thick = mean_input$soil_thick[1],
    #                                         clay = mean_input$clay[1],
    #                                         pE = mean_input$pE[1],
    #                                         PS = new_starting_soil_content,
    #                                         tilling_factor = mean_input$tilling_factor[1])
    # print("Current practices:")
    # print(tail(C0_df_baseline,1))
    
    Cinput <- (parcel_Cinputs %>% filter (scenario=="future" & parcel_ID==parcel))$tot_Cinputs
    mean=c(list(c(Cinput,rep(NA,11))),
           list(c(1.44,rep(NA,11))),
           list(c(12/12,rep(NA,11))),
           list(weather_data$future_temperature),
           list(weather_data$future_precipitation*365*30.4*24*3600),
           list(weather_data$future_pevap*365*30.4*24*3600),
           list(c(30,rep(NA,11))),
           list(c(15,rep(NA,11))),
           list(c(0.75,rep(NA,11))),
           list(c(1.0,rep(NA,11))))
    colnames_ranges=c("field_carbon_in","dr_ratios","perc_cover","temp","precip","evap","soil_thick","clay","pE","tilling_factor")
    mean_input = data.frame(mean)
    colnames(mean_input) = colnames_ranges
    time_horizon = 10
    C0_df_holistic <- calc_carbon_over_time(time_horizon,
                                            field_carbon_in = rep(mean_input$field_carbon_in[1],time_horizon),
                                            dr_ratios = rep(mean_input$dr_ratios[1],time_horizon),
                                            bare = as.factor(c(logical(round(12*mean_input$perc_cover[1])),!logical(round(12*(1-mean_input$perc_cover[1]))))),
                                            temp = mean_input$temp,
                                            precip = mean_input$precip,
                                            evap = mean_input$evap,
                                            soil_thick = mean_input$soil_thick[1],
                                            clay = mean_input$clay[1],
                                            pE = mean_input$pE[1],
                                            PS = new_starting_soil_content,
                                            tilling_factor = mean_input$tilling_factor[1])
    print("Holictic planning:")
    print(tail(C0_df_holistic,1))
    
    
    all_results <- rbind(all_results,data.frame(parcel_ID=rep(parcel,264),time=rep(seq(as.Date("2021-1-1"), as.Date("2031-12-31"), by = "month"),2),
                                                SOC=c(tail(C0_df,12)$TOT,C0_df_mdf$TOT,tail(C0_df,12)$TOT,C0_df_holistic$TOT),
                                                scenario=c(rep("baseline",132),rep("holistic",132)),
                                                farm_frac=rep(farm_frac,264)))#,C0_df_baseline$TOT#,rep("current",120)
  }
  farm_results <- unique(all_results %>% group_by(time, scenario) %>% mutate(SOC_farm=sum(SOC*farm_frac)) %>% select(time,scenario,SOC_farm))
  name<-"Results_farm_Franscisco_Ales"
  graph <- ggplot(data = farm_results, aes(x = time, y = SOC_farm, colour=scenario)) + 
    geom_line()+ 
    scale_color_manual(values = c("darkred","#5CB85C"),labels = c("Modern-day","Holistic"))+ 
    theme(legend.position = "bottom")+
    labs(title = name)+
    xlab("Time")+
    ylab("SOC (in tons per hectare)")
  print(graph)
  png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
  print(graph)
  dev.off()
  step_in_results <- unique(farm_results %>% 
                              mutate(year =format(time, format="%Y")) %>% 
                              group_by(scenario, year) %>% 
                              mutate(yearly_mean=mean(SOC_farm)) %>%
                              select(scenario, year, yearly_mean))
  temp_baseline <- step_in_results %>% filter(scenario=="baseline")
  year_temp <- temp_baseline$year[c(2:length(temp_baseline$year))]
  step_baseline <- diff(temp_baseline$yearly_mean)
  temp_holistic <- step_in_results %>% filter(scenario=="holistic")
  step_holistic <- diff(temp_holistic$yearly_mean)
  step_in_table <- data.frame(year=year_temp,
                              baseline_step_SOC_per_hectare=step_baseline,
                              holistic_step_SOC_per_hectare=step_holistic,
                              baseline_step_total_CO2=step_baseline*sum(parcel_inputs$area)*44/12,
                              holistic_step_total_CO2=step_holistic*sum(parcel_inputs$area)*44/12,
                              yearly_certificates=holistic_step_total_CO2-baseline_step_total_CO2)
  write.csv(all_results,file.path(project_loc,project_name,"results",paste(name,"_per_parcel.csv",sep="")), row.names = TRUE)
  write.csv(farm_results,file.path(project_loc,project_name,"results",paste(name,".csv",sep="")), row.names = TRUE)
  write.csv(step_in_table,file.path(project_loc,project_name,"results",paste(name,"_yearly_steps.csv",sep="")), row.names = TRUE)
  
}