#######################################################################
# Model of SOC dynamics from Fransisco's farm.
# We assume that grazing and agroforestry practices are well constrained by RothC
# when associated C input are well calculated.
# Then we neglect potential effects of trees and grazing on C pools' dynamics.
#######################################################################


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(SoilR, ggplot2, dplyr, tidyr, tidyverse, soilassessment, deSolve, readr, pracma, jsonlite, kableExtra)

setwd("~/Stages/ClimateFarmers/soil-modelling/soil-modelling")

working_dir <- getwd()

parameters <- fromJSON(file.path(working_dir, "parameter_files", "init.json"))

soil_loc <-parameters$soil_loc
project_loc <- parameters$project_loc
project_name <- parameters$project_name
modelling_data_loc <- parameters$modelling_data_loc
weatherDB_loc <- parameters$weatherDB_loc


################# TO GET AUTOMATED FROM FARMERS INPUT TEMPLATE / farm_details :
parcels_list = c(1,2,3)

run_soil_model(soil_loc,project_loc,project_name,modelling_data_loc,weatherDB_loc, parcels_list)