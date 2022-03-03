run_soil_model <- function(soil_loc,project_loc,project_name,modelling_data_loc,weatherDB_loc, parcels_list){
  
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
  
  animal_factors <- read_csv(file.path(modelling_data_loc,"data", "carbon_share_manure.csv")) %>% filter(type=="manure") %>% rename(species=manure_source)
  agroforestry_factors <- read_csv(file.path(modelling_data_loc,"data", "agroforestry_factors.csv")) 
  crop_data <- read_csv(file.path(modelling_data_loc,"data", "crop_factors.csv"))#, col_types =  "cdddddddd")
  pasture_data <- read_csv(file.path(modelling_data_loc,"data", "pasture_factors.csv"))
  
  ################# Pulling calculation inputs
  animal_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "animal_inputs.csv"))
  agroforestry_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "agroforestry_inputs.csv"))
  crop_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "crop_inputs.csv"))
  pasture_inputs <- read_csv(file.path(project_loc,project_name,"inputs", "pasture_inputs.csv"))
  
  ################# Calculations per parcel and scenario
  
  parcels_Cinput =data.frame(parcel_ID=c(),scenario=c(),agroforestry_Cinput=c(),animal_Cinput=c(),crop_Cinputs=c(),pasture_Cinputs=c())
  for(parcel in parcels_list){
    for(scenario in c("current","future","baseline")){
      parcels_Cinput<-rbind(parcels_Cinput,data.frame(parcel_ID=parcel,
                                                      scenario=scenario,
                                                      agroforestry_Cinput=get_monthly_Cinputs_agroforestry(agroforestry_inputs, agroforestry_factors, scenario, parcel),
                                                      animal_Cinput=get_monthly_Cinputs_animals(animal_inputs, animal_factors, scenario, parcel),
                                                      crop_Cinputs=get_monthly_Cinputs_crop(crop_inputs, crop_data, scenario, parcel),
                                                      pasture_Cinputs=get_monthly_Cinputs_pasture(pasture_inputs, pasture_data, scenario, parcel)))
    }
  }
  parcels_Cinput <- parcels_Cinput %>% mutate(tot_Cinputs=agroforestry_Cinput+animal_Cinput+crop_Cinputs+pasture_Cinputs)
  
  write.csv(parcels_Cinput,file.path(project_loc,project_name,"results/parcels_Cinput.csv"), row.names = TRUE)
  
  ################# Run model per parcel and store graphs, absolute result and step-in tables for each scenario
  
  step_in_table <- data.frame(parcel_ID=c(),year=c(),baseline=c(),holistic=c())
  
  for(parcel in parcels_list){
    Cinput <- (parcels_Cinput %>% filter (scenario=="baseline" & parcel_ID==parcel))$tot_Cinputs
    mean=c(list(c(Cinput,rep(NA,11))),list(c(1.44,rep(NA,11))),list(c(12/12,rep(NA,11))),list(weather_data$past_temperature),list(weather_data$past_precipitation*365*30.4*24*3600),list(weather_data$past_pevap*365*30.4*24*3600),list(c(30,rep(NA,11))),list(c(15,rep(NA,11))),list(c(0.75,rep(NA,11))),list(c(1.0,rep(NA,11))))
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
    
    mean=c(list(c(Cinput,rep(NA,11))),list(c(1.44,rep(NA,11))),list(c(12/12,rep(NA,11))),list(weather_data$future_temperature),list(weather_data$future_precipitation*365*30.4*24*3600),list(weather_data$future_pevap*365*30.4*24*3600),list(c(30,rep(NA,11))),list(c(15,rep(NA,11))),list(c(0.75,rep(NA,11))),list(c(1.0,rep(NA,11))))
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
    
    
    Cinput <- (parcels_Cinput %>% filter (scenario=="current" & parcel_ID==parcel))$tot_Cinputs
    mean=c(list(c(Cinput,rep(NA,11))),list(c(1.44,rep(NA,11))),list(c(12/12,rep(NA,11))),list(weather_data$future_temperature),list(weather_data$future_precipitation*365*30.4*24*3600),list(weather_data$future_pevap*365*30.4*24*3600),list(c(30,rep(NA,11))),list(c(15,rep(NA,11))),list(c(0.75,rep(NA,11))),list(c(1.0,rep(NA,11))))
    colnames_ranges=c("field_carbon_in","dr_ratios","perc_cover","temp","precip","evap","soil_thick","clay","pE","tilling_factor")
    mean_input = data.frame(mean)
    colnames(mean_input) = colnames_ranges
    time_horizon = 10
    C0_df_baseline <- calc_carbon_over_time(time_horizon,
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
    print("Current practices:")
    print(tail(C0_df_baseline,1))
    
    Cinput <- (parcels_Cinput %>% filter (scenario=="future" & parcel_ID==parcel))$tot_Cinputs
    mean=c(list(c(Cinput,rep(NA,11))),list(c(1.44,rep(NA,11))),list(c(12/12,rep(NA,11))),list(weather_data$future_temperature),list(weather_data$future_precipitation*365*30.4*24*3600),list(weather_data$future_pevap*365*30.4*24*3600),list(c(30,rep(NA,11))),list(c(15,rep(NA,11))),list(c(0.75,rep(NA,11))),list(c(1.0,rep(NA,11))))
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
    
    
    all_results <- data.frame(time=rep(seq(as.Date("2021-1-1"), as.Date("2030-12-31"), by = "month"),3),SOC=c(C0_df_mdf$TOT,C0_df_baseline$TOT,C0_df_holistic$TOT),variable=c(rep("baseline",120),rep("current",120),rep("holistic",120)))
    name<-paste("Results_farm_Franscisco_Ales-parcel_",parcel,sep="")
    graph <- ggplot(data = all_results, aes(x = time, y = SOC, colour=variable)) + 
      geom_line()+ 
      scale_color_manual(values = c("darkgrey", "#5CB85C","darkred"),labels = c("Current","Holistic","Modern-day"))+ 
      #theme_classic()+
      theme(legend.position = "bottom")+
      labs(title = name)+
      xlab("Time")+
      ylab("SOC (in tons)")
    print(graph)
    png(file.path(project_loc,project_name,"results",paste(name,".png",sep="")))
    print(graph)
    dev.off()
    write.csv(all_results,file.path(project_loc,project_name,"results",paste(name,".csv",sep="")), row.names = TRUE)
    step_in_results <- all_results %>% 
      mutate(year =format(time, format="%Y")) %>% 
      group_by(variable, year) %>% 
      mutate(yearly_mean=mean(SOC)) %>%
      summarize(variable, year, yearly_mean)
    step_in_results <- step_in_results[!duplicated(step_in_results),]
    temp_baseline <- step_in_results %>% filter(variable=="baseline")
    year_temp <- temp_baseline$year[c(2:length(temp_baseline$year))]
    step_baseline <- diff(temp_baseline$yearly_mean)
    temp_holistic <- step_in_results %>% filter(variable=="holistic")
    step_holistic <- diff(temp_holistic$yearly_mean)
    step_in_table <- rbind(step_in_table,data.frame(parcel_ID=parcel,
                                                    year=year_temp,
                                                    baseline=step_baseline,
                                                    holistic=step_holistic))
  }
  
}

# #### SENSITIVITY ANALYSIS
# n=30 #number of values tested within the range of possibles for each variable
# time_horizon = 500
# mean_all_c=c()
# for (Cinputs in linspace(5,15,n)){
#   all_c <- calc_carbon_over_time(time_horizon,
#                                  field_carbon_in = rep(Cinputs,time_horizon),
#                                  dr_ratios = rep(mean_input$dr_ratios[1],time_horizon),
#                                  bare = as.factor(c(logical(round(12*mean_input$perc_cover[1])),!logical(round(12*(1-mean_input$perc_cover[1]))))),
#                                  temp = mean_input$temp,
#                                  precip = mean_input$precip,
#                                  evap = mean_input$evap,
#                                  soil_thick = mean_input$soil_thick[1],
#                                  clay = mean_input$clay[1],
#                                  pE = mean_input$pE[1],
#                                  PS = starting_soil_content,
#                                  tilling_factor = mean_input$tilling_factor[1])
#   mean_all_c <- append(mean_all_c, mean(all_c$TOT))
# }
# all_results <- data.frame(tC_inputs=linspace(5,15,20),SOC_at_model_equilibrium=mean_all_c)
# name<-"Sensitivity_to_C_inputs_Franciscos_farm"
# graph <- ggplot(data = all_results, aes(x = tC_inputs, y = SOC_at_model_equilibrium)) + 
#   geom_line()+ 
#   theme_classic()+
#   theme(legend.position = "none")+
#   labs(title = name)+
#   xlab("C inputs (in tons)")+
#   ylab("SOC (in tons)")
# print(graph)
# png(paste("graphs/",paste(name,".png",sep=""),sep=""))
# print(graph)
# dev.off()
# 
# time_horizon = 500
# mean_all_c=c()
# for (dr_ratios in linspace(0.3,2,20)){
#   all_c <- calc_carbon_over_time(time_horizon,
#                                  field_carbon_in = rep(mean_input$field_carbon_in[1],time_horizon),
#                                  dr_ratios = rep(dr_ratios,time_horizon),
#                                  bare = as.factor(c(logical(round(12*mean_input$perc_cover[1])),!logical(round(12*(1-mean_input$perc_cover[1]))))),
#                                  temp = mean_input$temp,
#                                  precip = mean_input$precip,
#                                  evap = mean_input$evap,
#                                  soil_thick = mean_input$soil_thick[1],
#                                  clay = mean_input$clay[1],
#                                  pE = mean_input$pE[1],
#                                  PS = starting_soil_content,
#                                  tilling_factor = mean_input$tilling_factor[1])
#   mean_all_c <- append(mean_all_c, mean(all_c$TOT))
# }
# all_results <- data.frame(dr_ratios=linspace(0.3,2,20),SOC_at_model_equilibrium=mean_all_c)
# name<-"Sensitivity_to_dr_ratio_Franciscos_farm"
# graph <- ggplot(data = all_results, aes(x = dr_ratios, y = SOC_at_model_equilibrium)) + 
#   geom_line()+ 
#   theme_classic()+
#   theme(legend.position = "none")+
#   labs(title = name)+
#   xlab("dr_ratios (DPM/RPM)")+
#   ylab("SOC (in tons)")
# print(graph)
# png(paste(,paste(name,".png",sep=""),sep=""))
# print(graph)
# dev.off()

# 
# weather_data_francisco = data.frame(past_temperature=c(280.64557, 280.4041 , 280.56754, 281.34552, 282.73373,
#                                              284.63458, 286.25836, 286.77496, 286.05923, 284.0589 ,
#                                              282.54324, 281.59363),future_temperature=c(281.11234, 280.90283, 280.9647 ,
#                                                                                         281.8368 , 283.08707, 284.9624 , 286.5127 , 287.3063 ,
#                                                                                         286.5774 , 284.67715, 282.87692, 281.93784),
#                           past_precipitation=c(5.5902980e-08, 5.1779676e-08, 4.2648630e-08,
#                                                3.1973045e-08, 2.8401118e-08, 2.9915835e-08,
#                                                2.9334942e-08, 3.4176274e-08, 4.6860201e-08,
#                                                5.2112462e-08, 5.0436483e-08, 5.7581069e-08),future_precipitation=c(
#                                                  5.7237230e-08, 5.4193606e-08, 4.5199066e-08,
#                                                  3.3957960e-08, 2.7312819e-08, 3.2036088e-08,
#                                                  3.0086369e-08, 3.4217152e-08, 4.3973778e-08,
#                                                  5.4498010e-08, 5.0014613e-08, 5.9648940e-08),
#                           past_pevap=c(rep(1.9e-8,3),rep(3.83e-8,3),rep(5.36e-8,3),rep(3.83e-8,3)),future_pevap=c(rep(1.93e-8,3),rep(3.88e-8,3),rep(5.7e-8,3),rep(3.88e-8,3)))
