# prepare_farm_data


# Read in data, calculate base and new carbon inputs per field, run analysis 

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve, readr, pracma)

working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))
source(file.path(working_dir, "estimate_carbon_input.R"))

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

# TODO What about portions of residues and roots being left in the ground?

field_crop_data <- carbon_input_data %>% 
  left_join(crop_data, by = "crop") %>% 
  mutate(yield_bm = yield * 0.45,
         above_ground_bm = yield_bm *(1-harvest_index)/harvest_index,
         total_bm = yield_bm + above_ground_bm,
         roots_bm = total_bm * root_shoot_ratio,
         extra_roots_bm = roots_bm * rhizodeposition,
         total_c_input_tc = (above_ground_bm*residue + roots_bm + extra_roots_bm)/1000) %>% 
  mutate(across(contains("year"),
                ~ case_when(is_crop == "manure" ~ annual_quantity,
                            is_crop == "crop" ~ .x * total_c_input_tc)))

carbon_input_summary <- field_crop_data %>% 
  pivot_longer(cols = contains("year"), 
               names_to = "year",
               values_to = "carbon_input") %>% 
  group_by(field_id, case, is_crop, year) %>%
  summarise(carbon_input = sum(carbon_input, na.rm = TRUE), .groups = "drop") %>% 
  ungroup() %>% 
  mutate(year = as.numeric(gsub("year_", "", year))) %>% 
  arrange(field_id, case, is_crop, year)

carbon_inputs <- combine_crops_fym(carbon_input_summary)

# Start Calculating field results

farm_location <- unique(field_parameters$location)
if(length(farm_location) > 1)(stop("Currently only one location is supported per run. "))

weather_data <- read.csv(paste0("data/weather_average/",tolower(farm_location),"_average.csv"), 
                         row.names = c("evap", "precip", "temp")) %>% 
  select(-X) 
evap <- unlist(weather_data["evap",])
precip <- unlist(weather_data["precip",])
temp <- unlist(weather_data["temp",])

fields <- unique(field_parameters$field_id)

for (i in fields){
  
  desc <- field_parameters$desc[i]
  hectares <- field_parameters$hectares[i]
  soil_thick <- field_parameters$soil_thick[i]
  SOC <- field_parameters$SOC[i]
  clay <- field_parameters$clay[i]
  pE <- field_parameters$pE[i]    # Evaporation coefficient - 0.75 open pan evaporation or 1.0 potential evaporation
  new_tilling_practice <- field_parameters$new_tilling_practice[i]
  climate_zone <- field_parameters$climate_zone[i]
  bare_profile <- get_bare_profile(field_parameters)
  
  starting_soil_content_0 <- solve_for_initial_carbon_stocks(
    SOC_target = SOC,
    time_horizon = 100,
    bare = bare_profile,   
    temp = temp,
    precip = precip,
    evap = evap,
    soil_thick = soil_thick,
    clay = clay
  )
  
  for (case_select in c("base", "regen")){
    
    starting_soil_content <- starting_soil_content_0
    
    # process carbon inputs data
    field_carbon_inputs <- carbon_inputs %>% 
      filter(field_id == i, case == case_select)
    
    time_horizon = max(field_carbon_inputs$year)
    
    for (t in 1: time_horizon){
      
      # Runs the model for a single year, taking the inputs from the previous year as the SOC
      c_df <- calc_soil_carbon(
        time_horizon = 1,
        bare = bare_profile, 
        temp = temp,
        precip = precip,
        evap = evap,
        soil_thick = soil_thick,
        clay = clay,
        c_inputs = field_carbon_inputs$carbon_inputs[field_carbon_inputs$year == t],
        dr_ratio = field_carbon_inputs$dr_ratio[field_carbon_inputs$year == t],
        pE = pE,
        PS = starting_soil_content,
        description = desc,
        project_name = project_name
      )
      
      # Sets new starting_soil_content
      starting_soil_content <- as.numeric(tail(c_df, 1))
      
      
      
      if(t == 1){
        all_c = c_df
      }else{
        all_c <- rbind(all_c, c_df)
      }
    }
    
    # apply tilling losses
    tilling_factor <- calc_tilling_factor(starting_soil_content,
                                          climate_zone = climate_zone,
                                          new_practice = new_tilling_practice,
                                          tilling_factors)
    
    all_c <- calc_tilling_impact(tilling_factor, all_c)
    
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
