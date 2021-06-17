# Running the model

# This file allows you to test multiple soil configurations based on the inputs you give  
# in the parameter file defined by variable: input_file_name. The results and plots will be created
# in folders. The loop recreates the starting soil carbon content for each run. 

# To run: 
# 1. Ensure your working directory is the same as the repository. 
# 2. Create a parameter file for your project. The name of this file will be taken as the project name
# This can include as many tests as you would like. Just ensure a unique description is 
# used for each file. 
# 3. Run the code and review the results. Results are saved within the results and plots folders 


# TODO: 
# - Ensure that the crop inputs vary based on input data
# - Validate results! 
# - Sensitivity analysis of each variable


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve, readr)

working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))


input_file_name <- "test_dobimar.csv"
project_name <- gsub(".csv", "",input_file_name)

input_parameters <- read_csv(file.path(working_dir, "parameter_files", input_file_name),
                             col_types = "cccdddddddddllllllllllll")

# Test contents
no_tests <- nrow(input_parameters)
if(length(unique(input_parameters$desc)) != no_tests){stop("Descriptions are not unique. Results would be overwritten")}

C0_df <- NA
all_results <- NA

location <- unique(input_parameters$location)
if(length(location) > 1)(stop("Currently only one location is supported per run. "))

weather_data <- read.csv(paste0("data/weather_average/",tolower(location),"_average.csv"), 
                         row.names = c("evap", "precip", "temp")) %>% 
  select(-X) 
evap <- unlist(weather_data["evap",])
precip <- unlist(weather_data["precip",])
temp0 <- unlist(weather_data["temp",])

for (i in 1:no_tests){
  desc <- input_parameters$desc[i]
  site_location <- input_parameters$location[i]
  crop <- input_parameters$crop[i]
  soil_thick <- input_parameters$soil_thick[i]
  SOC <- input_parameters$SOC[i]
  clay <- input_parameters$clay[i]
  c_inputs_base <- input_parameters$c_in_base[i]
  c_inputs_reg <- input_parameters$c_in_reg[i]
  fym <- input_parameters$FYM[i]
  pE <- input_parameters$pE[i]
  time_horizon <- input_parameters$time_horizon[i]
  temp_adjustment <- input_parameters$temp_adjustment[i]
  bare_profile <- get_bare_profile(input_parameters[i])
  
  # add the fym to the c_inputs
  c_inputs <- c_inputs + fym
  
  dr_ratio_crops = 1.44
  dr_ratio_fym = 1
  
  # normalise the ratio by mass of carbon inputs from crops and fym
  dr_ratio = (dr_ratio_crops*c_inputs + dr_ratio_fym*fym) / (c_inputs + fym)
  
  # Set bare in the calc_soil_carbon to either a logical, bare or a 12 long string, bare_profile 
  
  FallIOM <- 0.049 * SOC^(1.139) # IOM using Falloon method
  
  temp <- temp0 + temp_adjustment 
  
  FallIOM <- 0.049 * SOC^(1.139)
  
  C0_df <- calc_soil_carbon(
    time_horizon = 500,
    bare = bare_profile, 
    temp = temp,
    precip = precip,
    evap = evap,
    soil_thick = soil_thick,
    clay = clay,
    c_inputs = c_inputs_base,
    dr_ratio = dr_ratio,
    pE = pE,
    PS = c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=FallIOM),
    description = "Base Case",
    project_name = project_name
  )
  
  C0 <- get_total_C(C0_df)
  
  starting_soil_content <- as.numeric(tail(C0_df, 1))
  
  C_df <- calc_soil_carbon(
    time_horizon = time_horizon,
    bare = bare_profile, 
    temp = temp,
    precip = precip,
    evap = evap,
    soil_thick = soil_thick,
    clay = clay,
    c_inputs = c_inputs_reg,
    dr_ratio = dr_ratio,
    pE = pE,
    PS = starting_soil_content,
    description = desc,
    project_name = project_name
  )
  
  
  years <- get_monthly_dataframe(time_horizon)
  
  # Generates and saves output plot
  plot_c_stocks(years, 
                C_df, 
                desc,
                project_name)
  
  plot_total_c(years, C_df, desc, project_name)
  
  
  plot_monthly_c(month = 3, 
                 time_horizon, 
                 C_df, 
                 desc, 
                 project_name)
  
  plot_monthly_c(month = 6, 
                 time_horizon, 
                 C_df, 
                 desc, 
                 project_name)
  
  plot_monthly_c(month = 9, 
                 time_horizon, 
                 C_df, 
                 desc, 
                 project_name)
  
  plot_monthly_histogram(time_horizon, C_df, desc, project_name)
  
  C_final <- get_total_C(C_df)
  
  C_init <- get_initial_C(C_df)
  
  print(tibble(desc, C_final, C_final - C_init))
  
  if (i == 1){
    all_results <- data.frame(desc, C_final, C_final - C_init)
  }else{
    all_results <- rbind(all_results, c(desc, C_final, C_final - C_init))
  }
  
}

# Create Results Path
results_path <- file.path(working_dir, "results", project_name)
results_file_name <- paste0(project_name, "_results.csv")
if(!dir.exists(results_path)){dir.create(results_path)}
write_csv(all_results, file.path(results_path, results_file_name))

