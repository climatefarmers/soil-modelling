# prepare_farm_data

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve, readr, pracma)

working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))
source(file.path(working_dir, "estimate_carbon_input.R"))
source(file.path(working_dir, "plotting_functions.R"))


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

farm_location <- unique(field_parameters$location)
if(length(farm_location) > 1)(stop("Currently only one location is supported per run. "))

weather_data <- read.csv(paste0("data/weather_average/",tolower(farm_location),"_average.csv"), 
                         row.names = c("evap", "precip", "temp")) %>% 
  select(-X) 
evap <- unlist(weather_data["evap",])
precip <- unlist(weather_data["precip",])
temp <- unlist(weather_data["temp",])


################################################################################
### Calculate carbon inputs based on parameters
################################################################################

# TODO What about portions of residues and roots being left in the ground?

carbon_inputs <- summarise_carbon_inputs(carbon_input_data,
                                         crop_data)

# Start Calculating field results

fields <- unique(field_parameters$field_id)

for (i in fields){
  
  hectares <- field_parameters$hectares[i]
  soil_thick <- field_parameters$soil_thick[i]
  SOC <- field_parameters$SOC[i]
  clay <- field_parameters$clay[i]
  pE <- field_parameters$pE[i]    # Evaporation coefficient - 0.75 open pan evaporation or 1.0 potential evaporation
  new_tilling_practice <- field_parameters$new_tilling_practice[i]
  prev_tilling_practice <- field_parameters$prev_tilling_practice[i]
  climate_zone <- field_parameters$climate_zone[i]
  bare_profile <- get_bare_profile(field_parameters)
  
  starting_soil_content_0 <- estimate_starting_soil_content(SOC, clay)
  
  for (case_select in c("base", "regen")){
    
    starting_soil_content <- starting_soil_content_0
    
    # process carbon inputs data
    field_carbon_inputs <- carbon_inputs %>% 
      filter(field_id == i, case == case_select)
    
    field_carbon_in <- field_carbon_inputs$carbon_inputs
    dr_ratios <- field_carbon_inputs$dr_ratio
    
    if(case_select == "base"){practice = prev_tilling_practice}else{practice = new_tilling_practice}
    
    tilling_factor <- calc_tilling_factor(climate_zone = climate_zone,
                                          practice = practice,
                                          tilling_factors)
 
    time_horizon = length(field_carbon_in)
    
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
    
    years <- get_monthly_dataframe(time_horizon, add_month = F)
    
    case_name = paste0(i,"_",case_select)
    
    # Generates and saves output plot
    plot_c_stocks(years, all_c, case_name, project_name)
    plot_total_c(years, all_c, case_name, project_name)
    
    # Calculate final values 
    c_final <- convert_to_tonnes(get_total_C(all_c))
    c_init <- convert_to_tonnes(get_initial_C(all_c))
    stored_carbon <- c_final - c_init
    annual_stored_carbon <- stored_carbon/time_horizon
    
    print(tibble(case_name, c_final, stored_carbon, annual_stored_carbon))
    
    if (i == 1 & case_select == "base"){
      all_results <- tibble(i, 
                            case_select, 
                            c_init, 
                            c_final, 
                            stored_carbon, 
                            annual_stored_carbon)
      
    }else{
      all_results <- all_results %>% 
        add_row(i, 
                case_select, 
                c_init, 
                c_final, 
                stored_carbon, 
                annual_stored_carbon)
    }
    
    write.csv(all_c, file.path("results", project_name, paste0(case_name,".csv")), row.names = F)
  }
}

all_results_summary <- all_results %>% 
  left_join(field_parameters %>% select(field_id, hectares), by = c("i" = "field_id")) %>% 
  mutate(c_init = c_init * hectares,
         c_final = c_final * hectares,
         stored_carbon = annual_stored_carbon * hectares,
         annual_stored_carbon = annual_stored_carbon * hectares) %>% 
  group_by(case_select) %>% 
  summarise(c_init = sum(c_init, na.rm = T),
            c_final = sum(c_final, na.rm = T),
            stored_carbon = sum(stored_carbon, na.rm = T),
            annual_stored_carbon = sum(annual_stored_carbon, na.rm = T), .groups = "drop") 

all_results <- all_results %>% add_row(all_results_summary) %>% 
  mutate(i = ifelse(is.na(i), "total", i))

write.csv(all_results, file.path("results", project_name, "results_summary.csv"), row.names = F)
