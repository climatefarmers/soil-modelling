#!/usr/bin/env Rscript

library(SoilR);
library(ggplot2);
library(dplyr);
library(tidyr);
library(deSolve);
library(readr);
library(rjson);

working_dir <- getwd()

source(file.path(working_dir, "model_functions.R"))
source(file.path(working_dir, "modified_functions.R"))


args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]

params = fromJSON(file = file.path(dir, "params.json"))

###############################################################################
# Input Parameters
###############################################################################

evap <- c(params$evap)
precip <- c(params$precip)
temp0 <- c(params$temp)
soil_thick <- params$soil_thick
SOC <- params$SOC_pools
clay <- params$clay
pE <- params$pE    # Evaporation coefficient - 0.75 open pan evaporation or 1.0 potential evaporat

c_inputs <- params$c_in
fym_inputs <- params$fym_in
pool_ratios <- c(params$pool_ratios)

time_horizon <- params$time_horizon
temp_adjustment <- params$temp_adjustment
bare_profile <- c(params$bare_profile)

desc <- params$desc


###############################################################################
# Derived parameters
###############################################################################

# add the fym to the c_inputs
c_inputs_total <- c_inputs + fym_inputs

# normalise the ratio by mass of carbon inputs from crops and fym
dr_ratio <- normalise_c_inputs(c_in = c_inputs, 
                               fym_in = fym_inputs)


temp <- temp0 + temp_adjustment 

###############################################################################
# Modelling run
###############################################################################

c_df <- calc_soil_carbon(
  time_horizon = time_horizon,
  bare = bare_profile, 
  temp = temp,
  precip = precip,
  evap = evap,
  soil_thick = soil_thick,
  clay = clay,
  c_inputs = c_inputs_total,
  dr_ratio = dr_ratio,
  pE = pE,
  PS = c(DPM=SOC$DPM, 
         RPM=SOC$RPM, 
         BIO=SOC$BIO, 
         HUM=SOC$HUM, 
         IOM=SOC$IOM),
  description = desc,
  project_name = project_name
)

c_df$TOTAL = rowSums(c_df[,c(1,2,3,4,5)])

print(c_df)
write.csv(c_df, file.path(dir, "out.csv"), row.names=FALSE)