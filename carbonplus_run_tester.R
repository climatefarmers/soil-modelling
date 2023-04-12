# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

debug_mode = TRUE  # Skip some steps. For now just skip fetching and       use dummy climate data.

source("carbonplus_trigger_Rscript.R")

sensitive_data_loc <- "../sensitive-data"

farmIds <- read_csv(file.path(sensitive_data_loc,"farmIds.csv"), show_col_types = FALSE)

farmId <- farmIds$farmId[2]
# farmId <- "a9f9b719-b301-4a7d-a89c-824f73e2c966" # This is a test Id from user Suhas

init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))

out <- launching_Rscripts(init_file, farmId)
