# parametrisation of model

# This file allows you to test multiple soil configurations based on the inputs you give  
# in the parameter file defined by input_file_name. The results and plots will be created
# in folders. The loop recreates the starting soil carbon content if parameters are changed
# between the runs. 

# To run: 
# 1. Ensure your working directory is the same as the repository. 
# 2. change the input file name to reflect the parameter file
# This can include as many tests as you would like. Just ensure a unique description is 
# used for each file. 
# 3. Run the code and review the results. 


# TODO: 
# - Average the starting_soil_content
# - Ensure that the crop and location inputs vary based on input data
# - Validate results! 


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve, readr)


working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))


input_file_name <- "test_bare_profile.csv"

input_parameters <- read_csv(file.path(working_dir, "parameter_files", input_file_name),
                             col_types = "cccddddddddllllllllllll")

# Test contents
no_tests <- nrow(input_parameters)
if(length(unique(input_parameters$desc)) != no_tests){stop("Descriptions are not unique. Results would be overwritten")}

C0_df <- NA
all_results <- NA

# Temperature (Wetterstation Lindenberg)
# 40 Year Mean Temperature/Month
temp0 <- c(2.6, 4.2, 8.5, 14.5, 19.5, 22.3, 24.5, 24.3, 
           19.4, 13.7, 7.2, 3.5)

# Precipitation - monthly mean values from 1981 to 2020
precip <- c(42, 35, 41, 34, 52, 62, 70, 60, 43, 39, 42, 47)

# Evaporation - Mean values from 1991 - 2020
evap <- c( 10, 15, 33, 53, 52, 61, 69, 51, 32, 19, 9, 7)

for (i in 1:no_tests){
  desc <- input_parameters$desc[i]
  site_location <- input_parameters$location[i]
  crop <- input_parameters$crop[i]
  soil_thick <- input_parameters$soil_thick[i]
  SOC <- input_parameters$SOC[i]
  clay <- input_parameters$clay[i]
  c_inputs <- input_parameters$c_inputs[i]
  fym <- input_parameters$FYM[i]
  pE <- input_parameters$pE[i]
  # bare <- input_parameters$bare[i]
  time_horizon <- input_parameters$time_horizon[i]
  temp_adjustment <- input_parameters$temp_adjustment[i]
  bare_profile <- get_bare_profile(input_parameters[i])
  
  # add the fym to the c_inputs
  c_inputs <- c_inputs + fym
  
  # Set bare in the calc_soil_carbon to either a logical, bare or a 12 long string, bare_profile 
  
  FallIOM <- 0.049 * SOC^(1.139) # IOM using Falloon method
  
  temp <- temp0 + temp_adjustment 
  
  # if(i == 1){
  #   # Sensitivity check
  #   C0_df <- calc_soil_carbon(
  #     time_horizon = 500,
  #     bare = bare_profile, 
  #     temp = temp,
  #     precip = precip,
  #     evap = evap,
  #     soil_thick = soil_thick,
  #     clay = clay,
  #     c_inputs = c_inputs,
  #     pE = pE,
  #     PS = c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=FallIOM),
  #     description = "Base Case"
  #   )
  #   C0 <- get_total_C(C0_df)
  #   
  #   starting_soil_content <- as.numeric(tail(C0_df, 1))
  #   
  #   
  #   soil_thick0 <- soil_thick
  #   clay0 <- clay
  #   c_inputs0 <- c_inputs
  #   pE0 <- pE
  #   temp_adjustment0 <- temp_adjustment
  #   SOC0 <- SOC
  #   
  # } else if (
  #   # if the soil_thickness, clay content, c_inputs or pE changes, need to rerun the starting_soil_content_calculation
  #   soil_thick != soil_thick0 | clay != clay0 | c_inputs != c_inputs0 | pE != pE0 | temp_adjustment != temp_adjustment0 | SOC != SOC0
  # ){
    # print("New starting soil content calculated")
    # print(c(
    #   soil_thick, soil_thick0, clay, clay0, c_inputs, c_inputs0, pE, pE0, temp_adjustment, temp_adjustment0, SOC, SOC0
    # ))
    
    FallIOM <- 0.049 * SOC^(1.139)
    
    C0_df <- calc_soil_carbon(
      time_horizon = 500,
      bare = FALSE, 
      temp = temp,
      precip = precip,
      evap = evap,
      soil_thick = soil_thick,
      clay = clay,
      c_inputs = c_inputs,
      pE = pE,
      PS = c(DPM=0, RPM=0, BIO=0, HUM=0, IOM=FallIOM),
      description = "Base Case"
    )
    
    C0 <- get_total_C(C0_df)
    
    starting_soil_content <- as.numeric(tail(C0_df, 1))
    
    soil_thick0 <- soil_thick
    clay0 <- clay
    c_inputs0 <- c_inputs
    pE0 <- pE
    temp_adjustment0 <- temp_adjustment
    SOC0 <- SOC
    
  # }
  
  C_df <- calc_soil_carbon(
    time_horizon = time_horizon,
    bare = bare_profile, 
    temp = temp,
    precip = precip,
    evap = evap,
    soil_thick = soil_thick,
    clay = clay,
    c_inputs = c_inputs,
    pE = pE,
    PS = starting_soil_content,
    description = desc
  )
  C <- get_total_C(C_df)
  
  print(tibble(desc, C))
  
  if (i == 1){
    all_results <- data.frame(desc, C)
  }else{
    all_results <- rbind(all_results, c(desc, C))
  }
  
}

# Create Results Path
results_path <- file.path(working_dir, "results")
results_file_name <- paste0(gsub(".csv", "",input_file_name), "_results.csv")
if(!dir.exists(results_path)){dir.create(results_path)}
write_csv(all_results, file.path(results_path, results_file_name))

