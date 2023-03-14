# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

test_var = "ahah"

source("carbonplus_trigger_Rscript.R")

sensitive_data_loc <- "../sensitive-data"

farmIds <- read_csv(file.path(sensitive_data_loc,"farmIds.csv"), show_col_types = FALSE)
farmId <- farmIds$farmId[farmIds$farmer_name == "Manuel Die"]

init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))

out <- launching_Rscripts(init_file, farmId)
