run_soil_model <- function(
  inputs_loc,
  results_loc
){
  
  if (!require("pacman")) install.packages("pacman"); library(pacman)
  p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve, readr, pracma, jsonlite, kableExtra)
  
  working_dir <- getwd()
  
  source(file.path(working_dir, "model_functions.R"))
  source(file.path(working_dir, "estimate_carbon_input.R"))
  source(file.path(working_dir, "uncertainty_functions.R"))
  
  
  crop_data <- clean_crop_variable_data(crop_data = read_csv("data/crop_values.csv", col_types =  "cdddd"))
  tilling_factors <- read_csv("data/tilling_factors.csv", col_types = "cccd") 
  
  project_name <- "dobimar"
  farm_location <- "Schwerin"
  
  # Constant variables
  # Set these once here. 
  hectares <- 72
  soil_thick <- 25
  pE <- 1    # Evaporation coefficient - 0.75 open pan evaporation or 1.0 potential evaporation
  time_horizon <- 10
  field_id <- 1 # If more than one field, need to update code. 
  
  # Add in the values obtained from the soil lab
  # These values are added automatically to the uncertainty analysis
  # soc_samples <- c(34.62, 34.21, 34.51, 28.17, 31.20)
  soc_perc <- c(1.1, 1.1, 1.1, 0.9, 1) # %
  clay_samples <- c(17, 6, 11, 9, 9) # %
  bulk_density_samples <- c(1259, 1244, 1255, 1252, 1248) #kg/m3
  
  soc_stock <- soil_thick/100 * soc_perc/100 * bulk_density_samples * 10000/1000 # tC/ha
  
  clay_bounds <- calculate_confidence_interval(clay_samples)
  soc_bounds <- calculate_confidence_interval(soc_stock)
  
  mean_soc_perc <- mean(soc_perc)
  mean_clay <- mean(clay_samples)
  mean_soc <- mean(soc_stock)
  
  # Add additional yields as necessary
  # Convert the values to kg/ha
  yield_rape <- c(51.15, 42.01, 32.9, 36.04)*.1*1000 
  yield_wheat <- c(98.02, 85.87, 92.71, 78.23, 67.55)*.1*1000
  
  # Add the following to the carbon_inputs csv file
  # These represent the minimum, maximum expected values within a 90% confidence interval
  yield_rape_bounds <- calculate_confidence_interval(yield_rape)
  yield_wheat_bounds <- calculate_confidence_interval(yield_wheat)
  
  # Also add these values to the carbon_inputs csv file 
  mean_rape <- mean(yield_rape)
  mean_wheat <- mean(yield_wheat)

  
  # TODO: Get new weather data being called in  
  weather_data <- read.csv(paste0("data/weather_average/",tolower(farm_location),"_uncertainty.csv"))
  
  
  # TODO: update source of field_parameters 
  field_parameters <- read_csv(file.path(working_dir, "parameter_files", paste0(project_name,"_field_parameters.csv")),
                               col_types = "dcccdddddcllllllllllll")
  
  field_parameters$climate_zone <- tolower(field_parameters$climate_zone)
  
  field_parameters <-  field_parameters %>% 
    left_join(tilling_factors, by = c("climate_zone", "tilling" = "new_practice")) %>% 
    rename(tilling_factor = factor)
  
  field_parameters <- get_bare_profile_df(field_parameters)
  
  
  carbon_input_data <- read_csv(file.path(working_dir, "parameter_files", paste0(project_name,"_carbon_inputs.csv")),
                                col_types = "dccddddddddddddddd") 
  
  check_field_differences(field_parameters, carbon_input_data)
  
  carbon_input_data <- clean_carbon_input_data(carbon_input_data, crop_data) 
  
  carbon_inputs <- summarise_carbon_inputs(carbon_input_data,
                                           crop_data)
  
  fields <- carbon_inputs %>% select(field_id, case) %>% distinct()
  
  uncertainty_list <- expand.grid(
    "field_id" = unique(fields$field_id),
    "case" = unique(fields$case),
    "clay" = clay_bounds,
    "soc" = soc_bounds,
    "carbon_inputs" = c("min_yield", "max_yield"),
    "evap" = c("min_evap", "max_evap"),
    "temp" = c("min_temp", "max_temp"),
    "precip" = c("min_precip", "max_precip")
  )  
  
  uncertainty_list <- uncertainty_list %>% 
    left_join(field_parameters %>% select(field_id, case, tilling_factor, bare_profile), by = c("field_id", "case")) %>% 
    mutate(label = 1:nrow(uncertainty_list))
  
  
  for (i in 1:nrow(uncertainty_list)){
    
    # Set parameters
    label <- uncertainty_list$label[i]
    case <- uncertainty_list$case[i]
    soc <- uncertainty_list$soc[i]
    clay <- uncertainty_list$clay[i]
    tilling_factor <- uncertainty_list$tilling_factor[i]
    bare_profile <- uncertainty_list$bare_profile[i]
    
    starting_soil_content <- estimate_starting_soil_content(soc, clay)
    
    field_carbon_in <- carbon_inputs %>% 
      filter(name == uncertainty_list$carbon_inputs[i],
             case == !!case,
             field_id == !!field_id) %>% 
      select(carbon_inputs) %>% pull()
    
    dr_ratios <- carbon_inputs %>% 
      filter(name == uncertainty_list$carbon_inputs[i],
             case == !!case,
             field_id == !!field_id) %>% 
      select(dr_ratio) %>% pull()
    
    temp <- weather_data %>% select(uncertainty_list$temp[i]) %>% pull()
    precip <- weather_data %>% select(uncertainty_list$precip[i]) %>% pull()
    evap <- weather_data %>% select(uncertainty_list$evap[i]) %>% pull()
    
    all_c <- calc_carbon_over_time(time_horizon,
                                   field_carbon_in,
                                   dr_ratios,
                                   bare_profile,
                                   temp = temp,
                                   precip = precip,
                                   evap = evap,
                                   soil_thick = soil_thick,
                                   clay = clay,
                                   pE = pE,
                                   PS = starting_soil_content,
                                   tilling_factor)
    
    if (label == 1){
      all_results <- data.frame(V1 = all_c$TOT)
    }else{
      all_results[label] <- all_c$TOT
    }
    
    
  }
  
  
  # Set parameters
  case_list <- field_parameters$case
  
  for(case in case_list){
    soc <- mean_soc
    clay <- mean_clay
    tilling_factor <- field_parameters$tilling_factor[field_parameters$case == case]
    bare_profile <- field_parameters$bare_profile[field_parameters$case == case]
    
    starting_soil_content <- estimate_starting_soil_content(soc, clay)
    
    field_carbon_in <- carbon_inputs %>% 
      filter(name == "mean_yield",
             case == !!case,
             field_id == !!field_id) %>% 
      select(carbon_inputs) %>% pull()
    
    dr_ratios <- carbon_inputs %>% 
      filter(name == "mean_yield",
             case == !!case,
             field_id == !!field_id) %>% 
      select(dr_ratio) %>% pull()
    
    temp <- weather_data %>% select(mean_temp) %>% pull()
    precip <- weather_data %>% select(mean_precip) %>% pull()
    evap <- weather_data %>% select(mean_evap) %>% pull()
    
    all_c <- calc_carbon_over_time(time_horizon,
                                   field_carbon_in,
                                   dr_ratios,
                                   bare_profile,
                                   temp = temp,
                                   precip = precip,
                                   evap = evap,
                                   soil_thick = soil_thick,
                                   clay = clay,
                                   pE = pE,
                                   PS = starting_soil_content,
                                   tilling_factor)
    
    
    
    if (case == "base"){
      all_results["base"] <- all_c$TOT
    }else if (case == "regen"){
      all_results["regen"] <- all_c$TOT
    }
    
    
    return(all_results)
  }
  
  
  
}