
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve, readr, pracma, jsonlite)

working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "estimate_carbon_input.R"))


################################################################################
## Set Up Parameters and Directories for a project
################################################################################

project_name <- "dobimar"

if(!dir.exists(file.path("results", project_name))){dir.create(file.path("results", project_name))}
if(!dir.exists(file.path("plots", project_name))){dir.create(file.path("plots", project_name))}

field_parameters <- read_csv(file.path(working_dir, "parameter_files", paste0(project_name,"_field_parameters.csv")),
                             col_types = "dcccdddddccllllllllllll")

carbon_input_data <- read_csv(file.path(working_dir, "parameter_files", paste0(project_name,"_carbon_inputs.csv")),
                              col_types = "dccddddddddddddd") 

# Check whether data exists for all fields in both files
check_field_differences(field_parameters, carbon_input_data)

crop_data <- clean_crop_variable_data(crop_data = read_csv("data/crop_values.csv", col_types =  "cdddd"))

tilling_factors <- read_csv("data/tilling_factors.csv", col_types = "cccd")

carbon_input_data <- clear_carbon_input_data(carbon_input_data, crop_data) 


carbon_inputs <- summarise_carbon_inputs(carbon_input_data,
                                         crop_data)

fields <- carbon_inputs %>% select(field_id, case) %>% distinct()
case_inputs <- list()

for (i in 1:nrow(fields)){
  
  case_select <- fields$case[i]
  field_id <- fields$field_id[i]
  desc <- paste0(field_id, "_",case_select)
  
  hectares <- field_parameters$hectares[field_parameters$field_id == field_id]
  soil_thick <- field_parameters$soil_thick[field_parameters$field_id == field_id]
  SOC <- field_parameters$SOC[field_parameters$field_id == field_id]
  clay <- field_parameters$clay[field_parameters$field_id == field_id]
  pE <- field_parameters$pE[field_parameters$field_id == field_id]    # Evaporation coefficient - 0.75 open pan evaporation or 1.0 potential evaporation
  hectares <- field_parameters$hectares[field_parameters$field_id == field_id]
  new_tilling_practice <- field_parameters$new_tilling_practice[field_parameters$field_id == field_id]
  prev_tilling_practice <- field_parameters$prev_tilling_practice[field_parameters$field_id == field_id]
  climate_zone <- field_parameters$climate_zone[field_parameters$field_id == field_id]
  
  bare_profile <- get_bare_profile(field_parameters[field_parameters$field_id == field_id,])
  starting_soil_content <- estimate_starting_soil_content(SOC, clay)
  
  # process carbon inputs data
  field_carbon_in <- carbon_inputs %>% 
    filter(field_id == !!field_id, case == case_select) %>% 
    select(carbon_inputs) %>% pull()
  
  dr_ratios <- carbon_inputs %>% 
    filter(field_id == !!field_id, case == case_select) %>% 
    select(dr_ratio) %>% pull()
  
  if(case_select == "base"){practice = prev_tilling_practice}else{practice = new_tilling_practice}
  
  tilling_factor <- calc_tilling_factor(climate_zone = climate_zone,
                                        practice = practice,
                                        tilling_factors)
  
  
  time_horizon = length(field_carbon_in)
  
  case_inputs[[i]] <- list("time_horizon" = time_horizon,
                           "field_carbon_in" = field_carbon_in,
                           "dr_ratios" = dr_ratios,
                           "bare_profile" = bare_profile,
                           "temp" = temp,
                           "precip" = precip,
                           "evap" = evap,
                           "soil_thick" = soil_thick,
                           "clay" = clay,
                           "pE" = pE,
                           "PS" = starting_soil_content,
                           "tilling_factor" = tilling_factor)
  
}

write(toJSON(case_inputs), "inputs.json")

