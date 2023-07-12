# Test run

# Small script to run carbon plus estimations. 
# Calls carbonplus_trigger_Rscript.R

rm(list=ls())

library(tidyverse)
library(jsonlite)

debug_mode = TRUE  # Skip some steps. For now just skip fetching and use dummy climate data.

source("carbonplus_main.R")

sensitive_data_loc <- "../sensitive-data"

farmIds <- read_csv(file.path(sensitive_data_loc,"farmIds.csv"), show_col_types = FALSE)

init_file <- fromJSON(file.path(sensitive_data_loc,"init_file.json"))

# farmId <- farmIds$farmId[2]
# farmId <- "a9f9b719-b301-4a7d-a89c-824f73e2c966" # This is a test Id from user Suhas
# farmId <- 'edf5cce8-eee2-40a8-af32-520d2b93ab5c' # Troya
# farmId <- '7fe9ced2-73b8-45aa-b6a2-a9ede144ca1b' # Valente
# farmId <- '3f916c12-3a2c-4904-91cb-bb64e6fb0832' # Alves 1
# farmId <- 'f67333e8-34a9-4030-93af-766f49d01310' # Alves 2
# farmId <- '584b48dc-0e5d-4ecc-b7d4-9acf281faaba' # Alves 3
# farmId <- 'bb393d6d-f952-474e-a790-5486365d929b' # Alves 4

# out <- carbonplus_main(init_file=init_file, farmId=farmId)


farmIds <- c(
  'edf5cce8-eee2-40a8-af32-520d2b93ab5c',
  '7fe9ced2-73b8-45aa-b6a2-a9ede144ca1b',
  '3f916c12-3a2c-4904-91cb-bb64e6fb0832',
  'f67333e8-34a9-4030-93af-766f49d01310',
  '584b48dc-0e5d-4ecc-b7d4-9acf281faaba',
  'bb393d6d-f952-474e-a790-5486365d929b'
)

for (farmId in farmIds){
  carbonplus_main(init_file=init_file, farmId=farmId)
}
