# prepare_farm_data


# Read in data, calculate base and new carbon inputs per field, run analysis 

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, soilassessment, deSolve, readr)

working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))
source(file.path(working_dir, "estimate_carbon_input.R"))

project_name <- "dobimar"

field_parameters <- read_csv(file.path(working_dir, "parameter_files", paste0(project_name,"_field_parameters.csv")),
                             col_types = "dcccdddddddddddllllllllllll")

carbon_input_data <- read_csv(file.path(working_dir, "parameter_files", paste0(project_name,"_carbon_inputs.csv")),
                              col_types = "dccddddddddddddddddddddddddd") 

# Check whether data exists for all fields in both files
check_field_differences(field_parameters, carbon_input_data)

crop_data <- clean_crop_variable_data(crop_data = read_csv("data/crop_values.csv", col_types =  "cdddd"))

c_inputs <- clear_carbon_input_data(carbon_input_data, crop_data) 

# Calculate the carbon input for the fields


                              